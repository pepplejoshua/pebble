package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func factInputs(t *testing.T, files checkProvider) (Inputs, *diagnostic.DiagnosticSet) {
	t.Helper()
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "facts"}, files, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	return Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics
}

// entryFrozenModule returns the compilation's root (entry) module. The
// embedded runtime prelude is always the first graph module, so the entry
// module is no longer Modules[0]; tests that need the authored entry must
// select it by graph identity.
func entryFrozenModule(c frozenCompilation) frozenModule {
	for _, m := range c.Modules {
		if m.ID == c.Root {
			return m
		}
	}
	return frozenModule{}
}

func TestWalkClosedDispatchIncludesEveryNodeKind(t *testing.T) {
	for kind := syntax.Missing; kind <= syntax.VariantDecl; kind++ {
		if !dispatchedNodeKind(kind) {
			t.Errorf("NodeKind %s (%d) is not in closed dispatch", kind, kind)
		}
	}
	if dispatchedNodeKind(syntax.NodeKind(255)) {
		t.Fatal("unknown NodeKind accepted by closed dispatch")
	}
}

func TestWalkPreparationAndAuthoredOrder(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let first i32 = 1;\nlet second i32 = 2;\n")})
	facts := run06a3(inputs, diagnostics, Config{})
	if facts == nil || facts.Constants == nil || facts.Program == nil || facts.Session == nil || facts.Generation == nil || facts.Walk == nil {
		t.Fatal("06a.3 did not construct its single owned lifecycle")
	}
	if facts.Generation.state != generationMutable {
		t.Fatalf("generation state = %d, want mutable", facts.Generation.state)
	}
	if len(facts.Walk.order) == 0 {
		t.Fatal("walk produced no order")
	}
	root := facts.Walk.order[0]
	item, _ := inputs.Graph.Module(root.Module)
	node, _ := item.Tree.Node(root.Node)
	if node.Kind() != syntax.File {
		t.Fatalf("first visited node = %s, want File", node.Kind())
	}
	last := facts.Walk.order[len(facts.Walk.order)-1]
	lastItem, _ := inputs.Graph.Module(last.Module)
	lastNode, _ := lastItem.Tree.Node(last.Node)
	if lastNode.Kind() != syntax.EndOfFile {
		t.Fatalf("last visited node = %s, want EndOfFile", lastNode.Kind())
	}
	if diagnostics.HasErrors() {
		t.Fatalf("valid walk diagnostics: %+v", diagnostics.Items())
	}
}

func TestWalkConsumesRecordFieldStructurallyAndEndOfFile(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct { x i32; };
let point = Point.{ x = 1 };
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	seenField, seenEOF := false, false
	for _, ref := range facts.Walk.order {
		item, _ := inputs.Graph.Module(ref.Module)
		node, _ := item.Tree.Node(ref.Node)
		seenField = seenField || node.Kind() == syntax.RecordField
		seenEOF = seenEOF || node.Kind() == syntax.EndOfFile
	}
	if !seenField || !seenEOF {
		t.Fatalf("RecordField/EndOfFile visited = %v/%v", seenField, seenEOF)
	}
}

func TestWalkDependencyOrderAndDuplicateDetection(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{
		"main.peb": []byte("import \"./dep\";\nlet main_value i32 = 1;\n"),
		"dep.peb":  []byte("let dependency_value i32 = 2;\n"),
	})
	facts := run06a3(inputs, diagnostics, Config{})
	order := inputs.Graph.DependencyOrder()
	if len(order) != 3 || len(facts.Walk.order) == 0 || facts.Walk.order[0].Module != order[0] {
		t.Fatalf("walk module order starts %v, dependency order %v", facts.Walk.order, order)
	}
	root, _ := inputs.Graph.Module(inputs.Graph.Root)
	facts.Walk.walkTree(root)
	facts.Generation.reporter.flush()
	found := false
	for _, item := range diagnostics.Items() {
		if item.Code == CodeGeneration {
			found = true
		}
	}
	if !found {
		t.Fatal("duplicate traversal did not produce C0619")
	}
}

func TestWalkLoweredVisitDepthAndDiagnosticReplacement(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let first i32 = 1;\nlet second i32 = 2;\n")})
	diagnostics.Error("EARLY", "earlier phase", source.Span{})
	run06a3(inputs, diagnostics, Config{MaxSyntaxVisits: 2, MaxTraversalDepth: 1, MaxDiagnostics: 1})
	if items := diagnostics.Items(); len(items) == 0 || items[0].Code != "EARLY" {
		t.Fatalf("generation replacement changed earlier diagnostics: %+v", items)
	}
	var generationDiagnostics []diagnostic.Diagnostic
	for _, item := range diagnostics.Items() {
		if item.Code == CodeGeneration {
			generationDiagnostics = append(generationDiagnostics, item)
		}
	}
	if len(generationDiagnostics) != 1 || generationDiagnostics[0].Message != "generation diagnostic limit of 1 reached" {
		t.Fatalf("generation diagnostics = %+v", generationDiagnostics)
	}
}

func TestWalkLoweredControlDepthStillConsumesIndependentNodes(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn nested() void { if true { while true { break; } } return; }
`)})
	facts := run06a3(inputs, diagnostics, Config{MaxControlDepth: 1})
	seenReturn := false
	for _, ref := range facts.Walk.order {
		item, _ := inputs.Graph.Module(ref.Module)
		node, _ := item.Tree.Node(ref.Node)
		seenReturn = seenReturn || node.Kind() == syntax.ReturnStmt
	}
	if !seenReturn {
		t.Fatal("control-depth recovery prevented later independent traversal")
	}
	found := false
	for _, item := range diagnostics.Items() {
		found = found || item.Code == CodeGeneration
	}
	if !found {
		t.Fatal("lowered control depth did not produce C0619")
	}
}
