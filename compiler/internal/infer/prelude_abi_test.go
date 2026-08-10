package infer

import (
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// preludeRuntimePath is the real slice-2 prelude module under test. The
// package-symbol test (internal/symbol) proves the resolver produces the
// parsed Allocator/Context symbol shape from this exact file; these tests
// prove the field TYPES this file spells are ABI-compatible with the
// synthesized runtime types the backend's Allocator adapter bridge expects.
func preludeRuntimePath(t *testing.T) string {
	t.Helper()
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("cannot locate prelude test source")
	}
	root := filepath.Clean(filepath.Join(filepath.Dir(filename), "..", "..", ".."))
	return filepath.Join(root, "compiler", "prelude", "runtime.peb")
}

// TestPreludeRuntimeFieldTypeSpellings reads the real prelude module and
// asserts, from its own syntax tree, that every struct field is declared with
// exactly the ABI type expression the backend's runtime callbacks expect:
//
//	Allocator.ptr     *void                          (C void *)
//	Allocator.alloc   fn (*void, uint) *void         (C void *(PebbleContext *, size_t))
//	Allocator.realloc fn (*void, *void, uint) *void  (C void *(PebbleContext *, void *, size_t))
//	Allocator.free    fn (*void, *void) void         (C void (PebbleContext *, void *))
//	Context.default_allocator Allocator              (the Allocator value, not a pointer)
//
// These spellings are the source-level (user-facing) parameter lists of the
// Pebble-convention callback function types; the hidden PebbleContext *ctx
// first parameter is threaded by the backend at C-emit time, not part of the
// source type — exactly the shape installPrelude synthesizes today
// (infer/runtime_prelude.go FunctionKey(Pebble, [*void, uint], *void, false)
// and friends).
func TestPreludeRuntimeFieldTypeSpellings(t *testing.T) {
	preludePath := preludeRuntimePath(t)
	contents, err := os.ReadFile(preludePath)
	if err != nil {
		t.Fatalf("cannot read real prelude: %v", err)
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	provider := inferenceMemoryProvider{}
	provider[module.CanonicalPath(filepath.ToSlash(filepath.Clean(preludePath)))] = contents
	graph := module.Build(module.BuildConfig{EntryPath: preludePath, Package: "prelude"}, provider, sources, diagnostics)
	if diagnostics.HasErrors() {
		t.Fatalf("prelude parse diagnostics: %+v", diagnostics.Items())
	}
	item, _ := graph.Module(graph.Root)
	file, _ := sources.File(item.Source)

	fields := make(map[string]map[string]string)
	walkSyntaxTree(item.Tree, item.Tree.Root(), func(id syntax.NodeID, node syntax.Node) {
		if node.Kind() != syntax.TypeDecl {
			return
		}
		nameID, ok := typeDeclName(item.Tree, node)
		if !ok {
			return
		}
		nameNode, _ := item.Tree.Node(nameID)
		_, body, ok := declarationBody(item.Tree, node)
		if !ok || body.Kind() != syntax.StructType {
			return
		}
		fieldMap := map[string]string{}
		for _, childID := range body.Children() {
			child, ok := item.Tree.Node(childID)
			if !ok || child.Kind() != syntax.FieldDecl {
				continue
			}
			childIDs := semanticSyntaxIDs(item.Tree, child.Children())
			if len(childIDs) < 2 {
				continue
			}
			typeNode, _ := item.Tree.Node(childIDs[len(childIDs)-1])
			typeText := string(file.Slice(typeNode.Span()))
			for _, nameNodeID := range childIDs[:len(childIDs)-1] {
				nameNode, _ := item.Tree.Node(nameNodeID)
				if nameNode.Kind() == syntax.Name {
					fieldMap[string(file.Slice(nameNode.Span()))] = typeText
				}
			}
		}
		fields[string(file.Slice(nameNode.Span()))] = fieldMap
	})

	want := map[string]map[string]string{
		"Allocator": {
			"ptr":     "*void",
			"alloc":   "fn (*void, uint) *void",
			"realloc": "fn (*void, *void, uint) *void",
			"free":    "fn (*void, *void) void",
		},
		"Context": {
			"default_allocator": "Allocator",
		},
	}
	if !reflect.DeepEqual(fields, want) {
		t.Fatalf("prelude field type spellings:\ngot  %+v\nwant %+v", fields, want)
	}
}

// TestPreludeRuntimeFieldTypeABI proves the type expressions the prelude file
// spells resolve, through the real declaration-preparation machinery, to
// EXACTLY the ABI TypeIDs the synthesized runtime version uses
// (runtime_prelude.go). The real prelude module cannot be run through the
// checker yet: the resolver's reservedBuiltin guard rejects the "Allocator"
// name (N0007) — itself the runtime-type special-casing slice 3 removes. So
// this test drives the identical type expressions through Prepare under
// non-reserved mirror names and compares the materialized member types,
// byte-for-byte TypeID against the keys installPrelude's descriptors use:
//
//	*void                          == Pointer(Void)
//	fn (*void, uint) *void         == FunctionKey(Pebble, [*void, uint], *void, false)
//	fn (*void, *void, uint) *void  == FunctionKey(Pebble, [*void, *void, uint], *void, false)
//	fn (*void, *void) void         == FunctionKey(Pebble, [*void, *void], Void, false)
//	Allocator                      == NominalKey(mirrorAllocator, nil)
func TestPreludeRuntimeFieldTypeABI(t *testing.T) {
	// Byte-identical field type expressions to compiler/prelude/runtime.peb,
	// under non-reserved names (the N0007 guard blocks only "Allocator").
	program, diagnostics := prepareSource(t, []byte(`
type MirrorAllocator = struct {
    ptr     *void;
    alloc   fn (*void, uint) *void;
    realloc fn (*void, *void, uint) *void;
    free    fn (*void, *void) void;
};
type MirrorContext = struct {
    default_allocator MirrorAllocator;
};
`))
	if diagnostics.HasErrors() {
		t.Fatalf("preparation diagnostics: %+v", diagnostics.Items())
	}

	store := program.inputs.Types
	builtins := program.builtins()
	voidPointer, err := program.internType(types.PointerKey(builtins.Void))
	if err != nil {
		t.Fatal(err)
	}
	allocType, err := program.internType(types.FunctionKey(types.Pebble, []types.TypeID{voidPointer, builtins.Uint}, voidPointer, false))
	if err != nil {
		t.Fatal(err)
	}
	reallocType, err := program.internType(types.FunctionKey(types.Pebble, []types.TypeID{voidPointer, voidPointer, builtins.Uint}, voidPointer, false))
	if err != nil {
		t.Fatal(err)
	}
	freeType, err := program.internType(types.FunctionKey(types.Pebble, []types.TypeID{voidPointer, voidPointer}, builtins.Void, false))
	if err != nil {
		t.Fatal(err)
	}

	allocatorDecl := findTypeDeclarationByName(t, program, "MirrorAllocator")
	contextDecl := findTypeDeclarationByName(t, program, "MirrorContext")
	allocatorNominal, err := program.internType(types.NominalKey(allocatorDecl.Symbol, nil))
	if err != nil {
		t.Fatal(err)
	}

	got := memberMaterializedTypes(t, program, allocatorDecl.Symbol)
	want := []types.TypeID{voidPointer, allocType, reallocType, freeType}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("MirrorAllocator member types:\ngot  %v\nwant %v", got, want)
	}
	if len(contextDecl.Members) != 1 {
		t.Fatalf("MirrorContext members = %d, want 1", len(contextDecl.Members))
	}
	contextFieldType := materializeMemberType(t, program, contextDecl.Members[0])
	if contextFieldType != allocatorNominal {
		t.Fatalf("MirrorContext.default_allocator type = %d, want nominal %d", contextFieldType, allocatorNominal)
	}
	if _, ok := store.Key(allocatorNominal); !ok {
		t.Fatal("MirrorAllocator nominal type is not interned")
	}
}

func findTypeDeclarationByName(t *testing.T, program *Program, name string) TypeDeclaration {
	t.Helper()
	for _, declaration := range program.TypeDeclarations() {
		sym, _ := program.inputs.Resolution.Symbols.Symbol(declaration.Symbol)
		if sym.Name == name {
			return declaration
		}
	}
	t.Fatalf("no prepared declaration named %s", name)
	return TypeDeclaration{}
}

func memberMaterializedTypes(t *testing.T, program *Program, owner symbol.SymbolID) []types.TypeID {
	t.Helper()
	declaration, ok := program.TypeDeclaration(owner)
	if !ok {
		t.Fatalf("missing declaration %d", owner)
	}
	out := make([]types.TypeID, 0, len(declaration.Members))
	for _, member := range declaration.Members {
		out = append(out, materializeMemberType(t, program, member))
	}
	return out
}

func materializeMemberType(t *testing.T, program *Program, member MemberDescriptor) types.TypeID {
	t.Helper()
	template, ok := program.Template(member.Type)
	if !ok {
		t.Fatalf("member template %d missing", member.Type)
	}
	if template.Kind != TemplateKnown {
		concrete, ok := program.materializeTemplate(member.Type, nil, false)
		if !ok {
			t.Fatalf("member template %d did not materialize", member.Type)
		}
		return concrete
	}
	return template.Known
}

func walkSyntaxTree(tree *syntax.Tree, id syntax.NodeID, visit func(syntax.NodeID, syntax.Node)) {
	node, ok := tree.Node(id)
	if !ok {
		return
	}
	visit(id, node)
	for _, child := range node.Children() {
		walkSyntaxTree(tree, child, visit)
	}
}

func typeDeclName(tree *syntax.Tree, node syntax.Node) (syntax.NodeID, bool) {
	for _, childID := range node.Children() {
		child, ok := tree.Node(childID)
		if ok && child.Kind() == syntax.Name {
			return childID, true
		}
	}
	return 0, false
}

func declarationBody(tree *syntax.Tree, node syntax.Node) (syntax.NodeID, syntax.Node, bool) {
	children := semanticSyntaxIDs(tree, node.Children())
	if len(children) < 2 {
		return 0, syntax.Node{}, false
	}
	id := children[len(children)-1]
	n, ok := tree.Node(id)
	return id, n, ok
}

func semanticSyntaxIDs(tree *syntax.Tree, ids []syntax.NodeID) []syntax.NodeID {
	out := make([]syntax.NodeID, 0, len(ids))
	for _, id := range ids {
		if n, ok := tree.Node(id); ok && n.Kind() != syntax.Missing && n.Kind() != syntax.Error && n.Kind() != syntax.EndOfFile {
			out = append(out, id)
		}
	}
	return out
}
