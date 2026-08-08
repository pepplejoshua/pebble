package symbol

import (
	"bytes"
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type memoryProvider struct {
	files map[module.CanonicalPath][]byte
}

func (p memoryProvider) Canonicalize(raw string) (module.CanonicalPath, error) {
	canonical := module.CanonicalPath(path.Clean(filepath.ToSlash(raw)))
	if _, ok := p.files[canonical]; !ok {
		return "", &module.ProviderError{Kind: module.ProviderNotFound, Path: raw, Err: fs.ErrNotExist}
	}
	return canonical, nil
}
func (p memoryProvider) ReadFile(name module.CanonicalPath) ([]byte, error) {
	contents, ok := p.files[name]
	if !ok {
		return nil, fs.ErrNotExist
	}
	return append([]byte(nil), contents...), nil
}

func TestNameFixtureCorpus(t *testing.T) {
	root := filepath.Join(repoRoot(t), "tests", "names")
	validFiles, err := filepath.Glob(filepath.Join(root, "valid", "*.peb"))
	if err != nil {
		t.Fatal(err)
	}
	for _, filename := range validFiles {
		filename := filename
		t.Run("valid/"+filepath.Base(filename), func(t *testing.T) {
			result, diagnostics, _, _ := resolveFiles(t, map[string]string{"main.peb": readText(t, filename)}, Config{})
			if got := nameErrors(diagnostics.Items()); len(got) != 0 {
				t.Fatalf("unexpected name diagnostics: %+v\n%s", got, dump(result))
			}
			assertResultInvariants(t, result)
		})
	}
	validMulti := filepath.Join(root, "valid", "multimodule")
	entries, err := os.ReadDir(validMulti)
	if err != nil {
		t.Fatal(err)
	}
	for _, entry := range entries {
		if entry.IsDir() {
			t.Run("valid/multimodule/"+entry.Name(), func(t *testing.T) {
				result, diagnostics, _, _ := resolveFiles(t, fixtureFiles(t, filepath.Join(validMulti, entry.Name())), Config{})
				if got := nameErrors(diagnostics.Items()); len(got) != 0 {
					t.Fatalf("unexpected name diagnostics: %+v\n%s", got, dump(result))
				}
				assertResultInvariants(t, result)
			})
		}
	}

	invalidRoot := filepath.Join(root, "invalid")
	codes, err := os.ReadDir(invalidRoot)
	if err != nil {
		t.Fatal(err)
	}
	for _, codeEntry := range codes {
		if !codeEntry.IsDir() {
			continue
		}
		code := codeEntry.Name()
		codeRoot := filepath.Join(invalidRoot, code)
		files, _ := filepath.Glob(filepath.Join(codeRoot, "*.peb"))
		for _, filename := range files {
			filename := filename
			t.Run("invalid/"+code+"/"+filepath.Base(filename), func(t *testing.T) {
				_, diagnostics, _, _ := resolveFiles(t, map[string]string{"main.peb": readText(t, filename)}, Config{})
				requireOnlyNameCode(t, diagnostics, diagnostic.Code(code))
			})
		}
		multiRoot := filepath.Join(codeRoot, "multimodule")
		entries, _ := os.ReadDir(multiRoot)
		for _, entry := range entries {
			if entry.IsDir() {
				entry := entry
				t.Run("invalid/"+code+"/multimodule/"+entry.Name(), func(t *testing.T) {
					_, diagnostics, _, _ := resolveFiles(t, fixtureFiles(t, filepath.Join(multiRoot, entry.Name())), Config{})
					requireOnlyNameCode(t, diagnostics, diagnostic.Code(code))
				})
			}
		}
	}

	recoveryFiles, err := filepath.Glob(filepath.Join(root, "recovery", "*.peb"))
	if err != nil {
		t.Fatal(err)
	}
	for _, filename := range recoveryFiles {
		filename := filename
		t.Run("recovery/"+filepath.Base(filename), func(t *testing.T) {
			result, _, _, _ := resolveFiles(t, map[string]string{"main.peb": readText(t, filename)}, Config{})
			assertResultInvariants(t, result)
		})
	}
}

func TestSequentialLocalBindingAndForwardModuleResolution(t *testing.T) {
	text := "type Unit = struct {}; let outer = later; let later = 1; fn use() Unit { let outer = outer; return outer; }"
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	moduleValue, _ := graph.Module(graph.Root)
	file, _ := sources.File(moduleValue.Source)
	refs := namedReferences(t, result, moduleValue, file, "outer")
	if len(refs) != 2 {
		t.Fatalf("outer references = %+v", refs)
	}
	first, _ := result.Symbols.Symbol(refs[0].Symbol)
	second, _ := result.Symbols.Symbol(refs[1].Symbol)
	if first.Scope == second.Scope || first.Kind != SymbolBinding || second.Kind != SymbolBinding {
		t.Fatalf("sequential lookup did not select outer then local: %+v / %+v", first, second)
	}
	laterRefs := namedReferences(t, result, moduleValue, file, "later")
	if len(laterRefs) != 1 || laterRefs[0].State != ResolutionResolved {
		t.Fatalf("module forward reference = %+v", laterRefs)
	}
}

func TestAnonymousFunctionHasStableFunctionSymbol(t *testing.T) {
	result, diagnostics, graph, _ := resolveFiles(t, map[string]string{"main.peb": "let value = fn(argument i32) i32 => argument;"}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	item, _ := graph.Module(graph.Root)
	var literal Symbol
	for _, candidate := range result.Symbols.All() {
		if candidate.Kind == SymbolFunction && candidate.Name == "" && candidate.Declaration.Module == item.ID {
			literal = candidate
			break
		}
	}
	if literal.ID == 0 || literal.Error || literal.Kind != SymbolFunction {
		t.Fatalf("anonymous function symbol = %+v", literal)
	}
	if literal.Scope == 0 || literal.Declaration.Node == 0 {
		t.Fatalf("anonymous function identity = %+v", literal)
	}
	parameters := 0
	for _, candidate := range result.Symbols.All() {
		if candidate.Kind == SymbolParameter && candidate.Containing == 0 && candidate.Declaration.Module == item.ID {
			parameters++
		}
	}
	if parameters == 0 {
		t.Fatal("anonymous function parameter was not assigned a distinct symbol")
	}
}

func TestBuiltinTypesLiveInReservedPrelude(t *testing.T) {
	text := "fn use(a bool, b char, c str, d int, e uint, f i8, g i16, h i32, i i64, j u8, k u16, l u32, m u64, n f32, o f64) void {}"
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	prelude, ok := result.Scopes.Scope(result.Prelude())
	if !ok || prelude.Kind != ScopePrelude || prelude.Parent != 0 || prelude.Module != 0 {
		t.Fatalf("prelude = %+v, %t", prelude, ok)
	}
	moduleScope := result.Scopes.All()[1]
	if moduleScope.Kind != ScopeModule || moduleScope.Parent != prelude.ID {
		t.Fatalf("module scope = %+v", moduleScope)
	}
	moduleValue, _ := graph.Module(graph.Root)
	file, _ := sources.File(moduleValue.Source)
	for kind := BuiltinBool; kind <= BuiltinF64; kind++ {
		id, ok := result.Builtin(kind)
		if !ok {
			t.Fatalf("missing builtin %s", kind)
		}
		sym, _ := result.Symbols.Symbol(id)
		if sym.Kind != SymbolBuiltinType || sym.Builtin != kind || sym.Name != kind.String() || sym.Scope != prelude.ID || sym.Declaration != (SyntaxRef{}) {
			t.Fatalf("builtin %s = %+v", kind, sym)
		}
		if kind == BuiltinVoid {
			// The parser represents an explicit void result without a Name node.
			continue
		}
		refs := namedReferences(t, result, moduleValue, file, kind.String())
		if len(refs) != 1 || refs[0].State != ResolutionResolved || refs[0].Symbol != id {
			t.Fatalf("references for %s = %+v, want symbol %d", kind, refs, id)
		}
	}
}

func TestRuntimeTypesHaveStableCompilerOwnedIdentities(t *testing.T) {
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": "fn preserve(value Allocator) Allocator => value;"}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	allocator, ok := result.Runtime(RuntimeAllocator)
	if !ok {
		t.Fatal("missing Allocator runtime identity")
	}
	context, ok := result.Runtime(RuntimeContext)
	if !ok || context != allocator+1 {
		t.Fatalf("runtime identities allocator=%d context=%d", allocator, context)
	}
	allocatorSymbol, _ := result.Symbols.Symbol(allocator)
	contextSymbol, _ := result.Symbols.Symbol(context)
	if allocatorSymbol.Kind != SymbolRuntimeType || allocatorSymbol.Runtime != RuntimeAllocator || allocatorSymbol.Name != "Allocator" || allocatorSymbol.Declaration != (SyntaxRef{}) {
		t.Fatalf("Allocator symbol = %+v", allocatorSymbol)
	}
	if contextSymbol.Kind != SymbolRuntimeType || contextSymbol.Runtime != RuntimeContext || contextSymbol.Name != "" || contextSymbol.Declaration != (SyntaxRef{}) {
		t.Fatalf("Context symbol = %+v", contextSymbol)
	}
	moduleValue, _ := graph.Module(graph.Root)
	file, _ := sources.File(moduleValue.Source)
	refs := namedReferences(t, result, moduleValue, file, "Allocator")
	if len(refs) != 2 {
		t.Fatalf("Allocator references = %+v", refs)
	}
	for _, ref := range refs {
		if ref.State != ResolutionResolved || ref.Symbol != allocator {
			t.Fatalf("Allocator reference = %+v", ref)
		}
	}
	if got := result.Members(allocator); len(got) != 4 {
		t.Fatalf("Allocator members = %v", got)
	}
	if got := result.Members(context); len(got) != 1 {
		t.Fatalf("Context members = %v", got)
	}
}

func TestDirectAliasBodiesAndFunctionResultsResolveAsTypes(t *testing.T) {
	text := "type Base = struct {}; type Alias = Base; fn use() Base {}"
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	moduleValue, _ := graph.Module(graph.Root)
	file, _ := sources.File(moduleValue.Source)
	refs := namedReferences(t, result, moduleValue, file, "Base")
	if len(refs) != 2 {
		t.Fatalf("Base references = %+v, want alias body and function result", refs)
	}
	for _, ref := range refs {
		selected, ok := result.Symbols.Symbol(ref.Symbol)
		if !ok || ref.State != ResolutionResolved || selected.Kind != SymbolType || selected.Name != "Base" {
			t.Fatalf("Base reference = %+v, selected = %+v", ref, selected)
		}
	}
}

func TestBuiltinNamesCannotBeRedeclaredAnywhere(t *testing.T) {
	text := "type int=struct{ bool char; fn str(f32 uint) void {} }; fn f[i8](i16 i32) i64 { let u8=1; { let u16=2; } return 1; }"
	_, diagnostics, _, _ := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	items := nameErrors(diagnostics.Items())
	if len(items) != 8 {
		t.Fatalf("reserved builtin diagnostics = %+v", items)
	}
	for _, item := range items {
		if item.Code != CodeReservedBuiltin {
			t.Fatalf("unexpected name diagnostic %s: %+v", item.Code, items)
		}
	}
}

func TestQualifiedLookupRecordsMemberAndQualifier(t *testing.T) {
	files := map[string]string{"main.peb": "import \"./dep\"; fn use(value dep::Thing) dep::Thing => dep::make(value);", "dep.peb": "type Thing = struct {}; fn make(value Thing) Thing => value;"}
	result, diagnostics, graph, sources := resolveFiles(t, files, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	main, _ := graph.Module(graph.Root)
	file, _ := sources.File(main.Source)
	qualifierNames := nameNodes(t, main, file, "dep")
	qualified := 0
	for _, id := range qualifierNames {
		if target, ok := result.Qualifier(SyntaxRef{Module: main.ID, Node: id}); ok {
			qualified++
			if target == 0 || target == main.ID {
				t.Fatalf("qualifier target = %d", target)
			}
		}
	}
	if qualified != 3 {
		t.Fatalf("qualifier mappings = %d, want 3", qualified)
	}
	thing := namedReferences(t, result, main, file, "Thing")
	makeRefs := namedReferences(t, result, main, file, "make")
	if len(thing) != 2 || len(makeRefs) != 1 {
		t.Fatalf("qualified members Thing=%+v make=%+v", thing, makeRefs)
	}
}

func TestSyntaxRefDisambiguatesEqualNodeIDsAcrossModules(t *testing.T) {
	dependency := "type Unit=struct{}; fn echo(value Unit) Unit => value;"
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{
		"main.peb": "import \"./left\"; import \"./right\";",
		"left.peb": dependency, "right.peb": dependency,
	}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	modules := graph.Modules()
	if len(modules) != 3 {
		t.Fatalf("modules = %d", len(modules))
	}
	var refs []Resolution
	for _, item := range modules[1:] {
		file, _ := sources.File(item.Source)
		found := namedReferences(t, result, item, file, "value")
		if len(found) != 1 {
			t.Fatalf("module %d value references = %+v", item.ID, found)
		}
		refs = append(refs, found[0])
	}
	if refs[0].Syntax.Node != refs[1].Syntax.Node || refs[0].Syntax.Module == refs[1].Syntax.Module || refs[0].Symbol == refs[1].Symbol {
		t.Fatalf("cross-module identities were conflated: %+v", refs)
	}
}

func TestAnonymousCaptureOrderAndExclusions(t *testing.T) {
	text := "type Unit=struct{}; let global=1; fn outer(parameter Unit) Unit { let local=parameter; let closure=fn(value Unit) Unit => local + parameter + local + global; return local; }"
	result, diagnostics, _, _ := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	captures := result.CaptureList()
	if len(captures) != 2 {
		t.Fatalf("captures = %+v, want local and parameter", captures)
	}
	first, _ := result.Symbols.Symbol(captures[0].Symbol)
	second, _ := result.Symbols.Symbol(captures[1].Symbol)
	if first.Name != "local" || second.Name != "parameter" {
		t.Fatalf("capture order = %q, %q", first.Name, second.Name)
	}
}

func TestNeutralBracketModesFollowResolvedBase(t *testing.T) {
	text := "type Unit=struct{}; type Box[T]=struct{}; fn identity[T](value T) T=>value; fn plain(value Unit) Unit=>value; fn use(box Box[Unit], runtime Unit, value Unit) Unit { let a=identity[Unit](value); let b=runtime[value]; let c=plain[value]; let d=(identity)[Unit](value); return a; }"
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	m, _ := graph.Module(graph.Root)
	file, _ := sources.File(m.Source)
	var modes []BracketMode
	walkTree(m.Tree, m.Tree.Root(), func(id syntax.NodeID, n syntax.Node) {
		if n.Kind() == syntax.BracketApply {
			mode, ok := result.Bracket(SyntaxRef{Module: m.ID, Node: id})
			if !ok {
				t.Fatalf("missing bracket mapping at %q", file.Slice(n.Span()))
			}
			modes = append(modes, mode)
		}
	})
	if !reflect.DeepEqual(modes, []BracketMode{BracketTypeNames, BracketTypeNames, BracketValueNames, BracketValueNames, BracketTypeNames}) {
		t.Fatalf("bracket modes = %v", modes)
	}
}

func TestExpressionOnlyBracketBasesUseValueNames(t *testing.T) {
	text := `
type Unit=struct{};
fn identity[T](value T) T=>value;
fn use(value Unit, argument Unit) Unit {
    let literal="x"[0];
    let array=[value][argument];
    let tuple=(value, value)[argument];
    let call=identity(value)[argument];
    let operator=(value + value)[argument];
    let cast=(value as Unit)[argument];
    let function=(fn(item Unit) Unit => item)[argument];
    return value;
}`
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	main, _ := graph.Module(graph.Root)
	file, _ := sources.File(main.Source)
	count := 0
	walkTree(main.Tree, main.Tree.Root(), func(id syntax.NodeID, node syntax.Node) {
		if node.Kind() != syntax.BracketApply {
			return
		}
		count++
		if mode, ok := result.Bracket(SyntaxRef{Module: main.ID, Node: id}); !ok || mode != BracketValueNames {
			t.Fatalf("bracket %q mode = %d, %t", file.Slice(node.Span()), mode, ok)
		}
		children := semanticChildren(main.Tree, node)
		if len(children) != 2 {
			t.Fatalf("bracket %q children = %v", file.Slice(node.Span()), children)
		}
		argumentNode, _ := main.Tree.Node(children[1])
		if argumentNode.Kind() == syntax.Name {
			argument, ok := result.Reference(SyntaxRef{Module: main.ID, Node: children[1]})
			if !ok || argument.State != ResolutionResolved || argument.Symbol == 0 {
				t.Fatalf("value argument for %q = %+v, %t", file.Slice(node.Span()), argument, ok)
			}
		}
	})
	if count != 7 {
		t.Fatalf("bracket count = %d", count)
	}
}

func TestDamagedExpressionBracketKeepsValueModeWithoutIdentity(t *testing.T) {
	text := `fn use() str { let value="x"[]; return value; }`
	result, _, graph, _ := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	main, _ := graph.Module(graph.Root)
	count := 0
	walkTree(main.Tree, main.Tree.Root(), func(id syntax.NodeID, node syntax.Node) {
		if node.Kind() != syntax.BracketApply {
			return
		}
		count++
		if mode, ok := result.Bracket(SyntaxRef{Module: main.ID, Node: id}); !ok || mode != BracketValueNames {
			t.Fatalf("damaged bracket mode = %d, %t", mode, ok)
		}
		for _, childID := range node.Children()[1:] {
			if reference, ok := result.Reference(SyntaxRef{Module: main.ID, Node: childID}); ok && reference.State == ResolutionResolved {
				t.Fatalf("damaged argument invented identity: %+v", reference)
			}
		}
	})
	if count != 1 {
		t.Fatalf("bracket count = %d", count)
	}
}

func TestDeferredBracketStillResolvesLexicalAndQualifiedNames(t *testing.T) {
	files := map[string]string{
		"main.peb": "import \"./dep\"; type Unit=struct{}; fn use(container Unit, argument Unit) Unit { let first=container.member[argument]; let second=container.member[dep::Thing]; return argument; }",
		"dep.peb":  "type Thing=struct{};",
	}
	result, diagnostics, graph, sources := resolveFiles(t, files, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	main, _ := graph.Module(graph.Root)
	file, _ := sources.File(main.Source)
	var brackets []syntax.NodeID
	walkTree(main.Tree, main.Tree.Root(), func(id syntax.NodeID, node syntax.Node) {
		if node.Kind() == syntax.BracketApply {
			brackets = append(brackets, id)
		}
	})
	if len(brackets) != 2 {
		t.Fatalf("brackets = %v", brackets)
	}
	for _, id := range brackets {
		if mode, ok := result.Bracket(SyntaxRef{Module: main.ID, Node: id}); !ok || mode != BracketDeferred {
			t.Fatalf("bracket %d mode = %d, %t", id, mode, ok)
		}
	}
	first, _ := main.Tree.Node(brackets[0])
	argumentID := first.Children()[1]
	argument, ok := result.Reference(SyntaxRef{Module: main.ID, Node: argumentID})
	if !ok || argument.State != ResolutionResolved || argument.Symbol == 0 {
		t.Fatalf("deferred bracket argument identity = %+v, %t", argument, ok)
	}
	second, _ := main.Tree.Node(brackets[1])
	pathNode, _ := main.Tree.Node(second.Children()[1])
	pathChildren := pathNode.Children()
	if len(pathChildren) != 2 {
		t.Fatalf("qualified bracket argument = %q", file.Slice(pathNode.Span()))
	}
	if target, ok := result.Qualifier(SyntaxRef{Module: main.ID, Node: pathChildren[0]}); !ok || target == 0 {
		t.Fatalf("deferred bracket qualifier = %d, %t", target, ok)
	}
	member, ok := result.Reference(SyntaxRef{Module: main.ID, Node: pathChildren[1]})
	if !ok || member.State != ResolutionResolved || member.Symbol == 0 {
		t.Fatalf("deferred bracket qualified member = %+v, %t", member, ok)
	}
}

func TestAnonymousFunctionDoesNotCaptureOuterTypeParameter(t *testing.T) {
	text := "fn outer[T](parameter T) T { let local=parameter; let closure=fn(value T) T => local + parameter; return local; }"
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	m, _ := graph.Module(graph.Root)
	file, _ := sources.File(m.Source)
	for _, ref := range namedReferences(t, result, m, file, "T") {
		if ref.State != ResolutionResolved || ref.Symbol == 0 {
			t.Fatalf("type parameter identity = %+v", ref)
		}
	}
	captures := result.CaptureList()
	if len(captures) != 2 {
		t.Fatalf("captures = %+v, want local and parameter", captures)
	}
	got := make([]string, 0, len(captures))
	for _, capture := range captures {
		symbol, _ := result.Symbols.Symbol(capture.Symbol)
		if symbol.Kind == SymbolTypeParameter {
			t.Fatalf("type parameter captured: %+v", symbol)
		}
		got = append(got, symbol.Name)
	}
	if !reflect.DeepEqual(got, []string{"local", "parameter"}) {
		t.Fatalf("capture order = %v", got)
	}
}

func TestStaticRecordMemberUsesMemberIdentity(t *testing.T) {
	text := "type Unit=struct{}; type Box=struct{ value Unit; }; fn make(input Unit) Box => Box.{ value=input };"
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	m, _ := graph.Module(graph.Root)
	file, _ := sources.File(m.Source)
	refs := namedReferences(t, result, m, file, "value")
	if len(refs) != 1 {
		t.Fatalf("record member references = %+v", refs)
	}
	symbol, _ := result.Symbols.Symbol(refs[0].Symbol)
	if symbol.Kind != SymbolField || symbol.Containing == 0 {
		t.Fatalf("record member symbol = %+v", symbol)
	}
}

func TestRuntimeAllocatorRecordConstructionResolvesFields(t *testing.T) {
	text := `
fn my_alloc(ptr *void, size uint) *void { return nil; }
fn my_realloc(ptr *void, data *void, size uint) *void { return nil; }
fn my_free(ptr *void, data *void) void {}
fn main() int {
    var a = Allocator.{ ptr = nil, alloc = my_alloc, realloc = my_realloc, free = my_free };
    return 0;
}`
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	allocator, ok := result.Runtime(RuntimeAllocator)
	if !ok {
		t.Fatal("missing Allocator runtime identity")
	}
	m, _ := graph.Module(graph.Root)
	file, _ := sources.File(m.Source)
	recordFields := 0
	walkTree(m.Tree, m.Tree.Root(), func(id syntax.NodeID, node syntax.Node) {
		if node.Kind() != syntax.RecordField {
			return
		}
		children := node.Children()
		if len(children) == 0 {
			return
		}
		recordFields++
		ref := SyntaxRef{Module: m.ID, Node: children[0]}
		resolved, ok := result.Reference(ref)
		if !ok || resolved.State != ResolutionResolved {
			t.Fatalf("record field %q = %+v, %t", file.Slice(node.Span()), resolved, ok)
		}
		selected, found := result.Symbols.Symbol(resolved.Symbol)
		if !found || selected.Kind != SymbolField || selected.Containing != allocator {
			t.Fatalf("record field selected symbol = %+v, %t", selected, found)
		}
	})
	if recordFields != 4 {
		t.Fatalf("Allocator record fields = %d, want 4", recordFields)
	}
}

func TestRuntimeAllocatorUnknownRecordFieldStaysDeferred(t *testing.T) {
	text := `
fn my_alloc(ptr *void, size uint) *void { return nil; }
fn main() int {
    var a = Allocator.{ ptr = nil, alloc = my_alloc, nope = 1 };
    return 0;
}`
	result, diagnostics, graph, sources := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	if got := nameErrors(diagnostics.Items()); len(got) != 0 {
		t.Fatalf("diagnostics: %+v", got)
	}
	m, _ := graph.Module(graph.Root)
	file, _ := sources.File(m.Source)
	deferred := 0
	walkTree(m.Tree, m.Tree.Root(), func(id syntax.NodeID, node syntax.Node) {
		if node.Kind() != syntax.RecordField {
			return
		}
		children := node.Children()
		if len(children) == 0 {
			return
		}
		nameNode, _ := m.Tree.Node(children[0])
		if nameNode.Kind() != syntax.Name || string(file.Slice(nameNode.Span())) != "nope" {
			return
		}
		ref := SyntaxRef{Module: m.ID, Node: children[0]}
		resolved, ok := result.Reference(ref)
		if !ok || resolved.State != ResolutionDeferred || resolved.Symbol != 0 {
			t.Fatalf("unknown record field resolution = %+v, %t; want deferred with no symbol", resolved, ok)
		}
		deferred++
	})
	if deferred != 1 {
		t.Fatalf("unknown Allocator fields deferred = %d, want 1", deferred)
	}
}

func TestParsedStructRecordMisspelledFieldStillErrors(t *testing.T) {
	text := `
type Point = struct { x i32; };
fn main() int {
    var p = Point.{ nope = 1 };
    return 0;
}`
	_, diagnostics, _, _ := resolveFiles(t, map[string]string{"main.peb": text}, Config{})
	requireOnlyNameCode(t, diagnostics, CodeUndefinedName)
}

func TestNonTypeOwnerRecordFieldKeepsEarlyMemberError(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{"builtin", `fn main() int { var a = int.{ x = 1 }; return 0; }`},
		{"type parameter", `fn make[T](x T) T { var r = T.{ x = 1 }; return x; } fn main() int { return 0; }`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, diagnostics, _, _ := resolveFiles(t, map[string]string{"main.peb": tt.text}, Config{})
			requireOnlyNameCode(t, diagnostics, CodeUndefinedName)
		})
	}
}

func TestResultAndSyntaxAreImmutableAndDeterministic(t *testing.T) {
	files := map[string]string{"main.peb": "type Unit=struct{}; fn use(value Unit) Unit { { let local=value; } return value; }"}
	first, firstDiagnostics, firstGraph, _ := resolveFiles(t, files, Config{})
	before := treeDumps(firstGraph)
	second, secondDiagnostics, _, _ := resolveFiles(t, files, Config{})
	if dump(first) != dump(second) || !reflect.DeepEqual(firstDiagnostics.Items(), secondDiagnostics.Items()) {
		t.Fatal("resolution is not deterministic")
	}
	if !reflect.DeepEqual(before, treeDumps(firstGraph)) {
		t.Fatal("resolver mutated syntax trees")
	}
	scopes := first.Scopes.All()
	symbols := first.Symbols.All()
	scopes[0].Symbols = nil
	symbols[0].Name = "changed"
	again, _ := first.Scopes.Scope(1)
	symbol, _ := first.Symbols.Symbol(1)
	if again.Symbols == nil || symbol.Name == "changed" {
		t.Fatal("store accessor exposed mutable storage")
	}
}

func TestConfiguredLimitsAndInvalidInputsAreBounded(t *testing.T) {
	text := "type Unit=struct{}; let a=1; let b=2; fn use() Unit { { { let c=missing1+missing2+missing3; } } return missing4; }"
	cases := []struct {
		name   string
		config Config
	}{
		{"symbols", Config{MaxSymbols: 2}}, {"scopes", Config{MaxScopes: 1}}, {"depth", Config{MaxScopeDepth: 1}}, {"diagnostics", Config{MaxDiagnostics: 2}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, diagnostics, _, _ := resolveFiles(t, map[string]string{"main.peb": text}, tc.config)
			items := nameErrors(diagnostics.Items())
			requireCode(t, diagnostics, CodeResourceLimit)
			if tc.config.MaxDiagnostics != 0 && uint32(len(items)) > tc.config.MaxDiagnostics {
				t.Fatalf("diagnostics = %d, limit %d", len(items), tc.config.MaxDiagnostics)
			}
		})
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	result := Resolve(nil, source.NewFileSet(), diagnostics, Config{MaxDiagnostics: 1})
	if result == nil {
		t.Fatal("nil graph returned nil result")
	}
	requireCode(t, diagnostics, CodeResourceLimit)
	graphDiagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app"}, memoryProvider{files: map[module.CanonicalPath][]byte{"main.peb": []byte("type Unit=struct{};")}}, sources, graphDiagnostics)
	missing := diagnostic.NewDiagnosticSet()
	Resolve(graph, source.NewFileSet(), missing, Config{})
	requireCode(t, missing, CodeResourceLimit)
}

func TestRequiredFixtureShapes(t *testing.T) {
	required := []string{"valid/forward_and_scopes.peb", "valid/generics_members_brackets.peb", "valid/capture.peb", "valid/runtime_allocator.peb", "valid/multimodule/qualified/main.peb", "invalid/N0001/block_lifetime.peb", "invalid/N0001/loop_lifetime.peb", "invalid/N0001/local_forward.peb", "invalid/N0001/runtime_context_hidden.peb", "invalid/N0002/cross_kind.peb", "invalid/N0002/parameter_body.peb", "invalid/N0002/parameters.peb", "invalid/N0002/members.peb", "invalid/N0002/multimodule/qualifier_collision/main.peb", "invalid/N0003/not_a_qualifier.peb", "invalid/N0003/multimodule/qualifier_shadow/main.peb", "invalid/N0004/multimodule/missing_member/main.peb", "invalid/N0005/category.peb", "invalid/N0005/value_as_type.peb", "invalid/N0007/reserved_builtin.peb", "invalid/N0007/reserved_allocator.peb", "recovery/damaged.peb"}
	root := filepath.Join(repoRoot(t), "tests", "names")
	for _, relative := range required {
		if _, err := os.Stat(filepath.Join(root, filepath.FromSlash(relative))); err != nil {
			t.Errorf("missing required fixture %s", relative)
		}
	}
}

func resolveFiles(t *testing.T, files map[string]string, config Config) (*Result, *diagnostic.DiagnosticSet, *module.Graph, *source.FileSet) {
	t.Helper()
	providerFiles := make(map[module.CanonicalPath][]byte, len(files))
	for name, text := range files {
		providerFiles[module.CanonicalPath(filepath.ToSlash(name))] = []byte(text)
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app"}, memoryProvider{files: providerFiles}, sources, diagnostics)
	result := Resolve(graph, sources, diagnostics, config)
	return result, diagnostics, graph, sources
}
func fixtureFiles(t *testing.T, root string) map[string]string {
	t.Helper()
	files := map[string]string{}
	err := filepath.WalkDir(root, func(filename string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() || filepath.Ext(filename) != ".peb" {
			return nil
		}
		relative, err := filepath.Rel(root, filename)
		if err != nil {
			return err
		}
		files[filepath.ToSlash(relative)] = readText(t, filename)
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	return files
}
func readText(t *testing.T, filename string) string {
	t.Helper()
	contents, err := os.ReadFile(filename)
	if err != nil {
		t.Fatal(err)
	}
	return string(contents)
}
func requireCode(t *testing.T, diagnostics *diagnostic.DiagnosticSet, code diagnostic.Code) {
	t.Helper()
	for _, item := range diagnostics.Items() {
		if item.Code == code {
			return
		}
	}
	t.Fatalf("missing diagnostic %s in %+v", code, diagnostics.Items())
}
func requireOnlyNameCode(t *testing.T, diagnostics *diagnostic.DiagnosticSet, code diagnostic.Code) {
	t.Helper()
	items := nameErrors(diagnostics.Items())
	if len(items) == 0 {
		t.Fatalf("missing diagnostic %s in %+v", code, diagnostics.Items())
	}
	for _, item := range items {
		if item.Code != code {
			t.Fatalf("unexpected name diagnostic %s, want only %s: %+v", item.Code, code, items)
		}
	}
}
func nameErrors(items []diagnostic.Diagnostic) []diagnostic.Diagnostic {
	var out []diagnostic.Diagnostic
	for _, item := range items {
		if item.Severity == diagnostic.Error && strings.HasPrefix(string(item.Code), "N") {
			out = append(out, item)
		}
	}
	return out
}
func repoRoot(t *testing.T) string {
	t.Helper()
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("cannot locate test source")
	}
	return filepath.Clean(filepath.Join(filepath.Dir(filename), "..", "..", ".."))
}
func dump(result *Result) string {
	var out bytes.Buffer
	if err := result.Dump(&out); err != nil {
		panic(err)
	}
	return out.String()
}
func assertResultInvariants(t *testing.T, result *Result) {
	t.Helper()
	for index, s := range result.Symbols.All() {
		if s.ID != SymbolID(index+1) {
			t.Fatalf("symbol ID at %d = %d", index, s.ID)
		}
	}
	for index, s := range result.Scopes.All() {
		if s.ID != ScopeID(index+1) {
			t.Fatalf("scope ID at %d = %d", index, s.ID)
		}
		if s.Parent >= s.ID && s.Parent != 0 {
			t.Fatalf("scope %d parent = %d", s.ID, s.Parent)
		}
	}
}
func nameNodes(t *testing.T, m module.Module, file *source.File, name string) []syntax.NodeID {
	t.Helper()
	var ids []syntax.NodeID
	walkTree(m.Tree, m.Tree.Root(), func(id syntax.NodeID, n syntax.Node) {
		if n.Kind() == syntax.Name && string(file.Slice(n.Span())) == name {
			ids = append(ids, id)
		}
	})
	return ids
}
func namedReferences(t *testing.T, result *Result, m module.Module, file *source.File, name string) []Resolution {
	t.Helper()
	var refs []Resolution
	for _, id := range nameNodes(t, m, file, name) {
		if ref, ok := result.Reference(SyntaxRef{Module: m.ID, Node: id}); ok {
			refs = append(refs, ref)
		}
	}
	sort.Slice(refs, func(i, j int) bool {
		ni, _ := m.Tree.Node(refs[i].Syntax.Node)
		nj, _ := m.Tree.Node(refs[j].Syntax.Node)
		return ni.Span().Start < nj.Span().Start
	})
	return refs
}
func walkTree(tree *syntax.Tree, id syntax.NodeID, visit func(syntax.NodeID, syntax.Node)) {
	node, ok := tree.Node(id)
	if !ok {
		return
	}
	visit(id, node)
	for _, child := range node.Children() {
		walkTree(tree, child, visit)
	}
}
func treeDumps(graph *module.Graph) map[module.ModuleID]string {
	out := map[module.ModuleID]string{}
	for _, m := range graph.Modules() {
		out[m.ID] = m.Tree.DumpString()
	}
	return out
}
