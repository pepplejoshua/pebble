package check

import (
	"fmt"
	"reflect"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type checkProvider map[module.CanonicalPath][]byte

func (p checkProvider) Canonicalize(path string) (module.CanonicalPath, error) {
	key := module.CanonicalPath(path)
	if _, ok := p[key]; !ok {
		return "", fmt.Errorf("missing %s", path)
	}
	return key, nil
}

func (p checkProvider) ReadFile(path module.CanonicalPath) ([]byte, error) {
	value, ok := p[path]
	if !ok {
		return nil, fmt.Errorf("missing %s", path)
	}
	return append([]byte(nil), value...), nil
}

func validGenerationInputs(t *testing.T) Inputs {
	t.Helper()
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := checkProvider{"main.peb": []byte("let answer i32 = 42;\n")}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "test"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatalf("types.New: %v", err)
	}
	if diagnostics.HasErrors() {
		t.Fatalf("fixture setup diagnostics: %+v", diagnostics.Items())
	}
	return Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}
}

func rootRef(t *testing.T, inputs Inputs) symbol.SyntaxRef {
	t.Helper()
	item, ok := inputs.Graph.Module(inputs.Graph.Root)
	if !ok {
		t.Fatal("missing root module")
	}
	return symbol.SyntaxRef{Module: item.ID, Node: item.Tree.Root()}
}

func rootHeader(t *testing.T, inputs Inputs) recordHeader {
	t.Helper()
	ref := rootRef(t, inputs)
	item, _ := inputs.Graph.Module(ref.Module)
	return recordHeader{Syntax: ref, Span: source.Span{Source: item.Source}}
}

func TestConfigDefaultsAndInferenceOwnership(t *testing.T) {
	inference := infer.Config{MaxConstraints: 7, MaxDiagnostics: 9}
	got := normalizeConfig(Config{Inference: inference})
	if got.Inference != inference {
		t.Fatalf("inference config changed: got %+v want %+v", got.Inference, inference)
	}
	want := Config{
		Inference:       inference,
		MaxSyntaxVisits: DefaultMaxSyntaxVisits, MaxTraversalDepth: DefaultMaxTraversalDepth,
		MaxSemanticRecords: DefaultMaxSemanticRecords, MaxRecordComponents: DefaultMaxRecordComponents,
		MaxControlDepth: DefaultMaxControlDepth, MaxTrackedPlaces: DefaultMaxTrackedPlaces,
		MaxGenericRequirements: DefaultMaxGenericRequirements, MaxConstantDepth: DefaultMaxConstantDepth,
		MaxConstantOperations: DefaultMaxConstantOperations, MaxConstantBits: DefaultMaxConstantBits,
		MaxDiagnostics:     DefaultMaxDiagnostics,
		MaxValidationSteps: DefaultMaxValidationSteps, MaxIRNodes: DefaultMaxIRNodes,
		MaxIRComponents: DefaultMaxIRComponents, MaxFlowStates: DefaultMaxFlowStates,
		MaxDeferEdges: DefaultMaxDeferEdges, MaxDumpBytes: DefaultMaxDumpBytes,
	}
	if got != want {
		t.Fatalf("normalized config:\n got %+v\nwant %+v", got, want)
	}

	lowered := Config{
		Inference:       inference,
		MaxSyntaxVisits: 1, MaxTraversalDepth: 2, MaxSemanticRecords: 3,
		MaxRecordComponents: 4, MaxControlDepth: 5, MaxTrackedPlaces: 6,
		MaxGenericRequirements: 7, MaxConstantDepth: 8, MaxConstantOperations: 9,
		MaxConstantBits: 10, MaxDiagnostics: 11,
		MaxValidationSteps: 12, MaxIRNodes: 13, MaxIRComponents: 14,
		MaxFlowStates: 15, MaxDeferEdges: 16, MaxDumpBytes: 17,
	}
	if normalized := normalizeConfig(lowered); normalized != lowered {
		t.Fatalf("lowered config changed: got %+v want %+v", normalized, lowered)
	}
}

func TestGenerationSnapshotValidationIsBoundedAndNonpanicking(t *testing.T) {
	valid := validGenerationInputs(t)
	tests := []struct {
		name   string
		inputs Inputs
	}{
		{name: "nil inputs"},
		{name: "nil graph", inputs: Inputs{Sources: valid.Sources, Resolution: valid.Resolution, Types: valid.Types, LiteralTarget: valid.LiteralTarget}},
		{name: "nil sources", inputs: Inputs{Graph: valid.Graph, Resolution: valid.Resolution, Types: valid.Types, LiteralTarget: valid.LiteralTarget}},
		{name: "nil resolution", inputs: Inputs{Graph: valid.Graph, Sources: valid.Sources, Types: valid.Types, LiteralTarget: valid.LiteralTarget}},
		{name: "nil types", inputs: Inputs{Graph: valid.Graph, Sources: valid.Sources, Resolution: valid.Resolution, LiteralTarget: valid.LiteralTarget}},
		{name: "invalid literal target", inputs: Inputs{Graph: valid.Graph, Sources: valid.Sources, Resolution: valid.Resolution, Types: valid.Types, LiteralTarget: infer.LiteralTarget{WordBits: 16}}},
		{name: "foreign source set", inputs: Inputs{Graph: valid.Graph, Sources: source.NewFileSet(), Resolution: valid.Resolution, Types: valid.Types, LiteralTarget: valid.LiteralTarget}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			diagnostics := diagnostic.NewDiagnosticSet()
			generation := newGeneration(test.inputs, diagnostics, Config{MaxDiagnostics: 1})
			if generation == nil {
				t.Fatal("newGeneration returned nil")
			}
			if _, ok := generation.freeze(); !ok {
				t.Fatal("invalid snapshot must still freeze bounded recovery state")
			}
			if diagnostics.Len() != 1 || diagnostics.Items()[0].Code != CodeGeneration {
				t.Fatalf("diagnostics = %+v", diagnostics.Items())
			}
		})
	}

	diagnostics := diagnostic.NewDiagnosticSet()
	generation := newGeneration(valid, diagnostics, Config{})
	if _, ok := generation.freeze(); !ok || diagnostics.Len() != 0 {
		t.Fatalf("valid snapshot: ok=%t diagnostics=%+v", ok, diagnostics.Items())
	}

	nilDiagnostics := newGeneration(Inputs{}, nil, Config{MaxDiagnostics: 1})
	if _, ok := nilDiagnostics.freeze(); !ok || nilDiagnostics.diagnostics.Len() != 1 {
		t.Fatalf("nil diagnostic recovery: ok=%t diagnostics=%+v", ok, nilDiagnostics.diagnostics.Items())
	}
}

func TestRootIdentitiesRejectZeroForeignDuplicateAndInvalidRoots(t *testing.T) {
	inputs := validGenerationInputs(t)
	generation := newGeneration(inputs, diagnostic.NewDiagnosticSet(), Config{MaxSyntaxVisits: 2})
	if id, ok := generation.addValue(generatedValue{ID: 7}); ok || id != 0 || len(generation.values) != 0 {
		t.Fatal("preassigned value identity was accepted")
	}
	first, ok := generation.addValue(generatedValue{})
	if !ok || first != 1 {
		t.Fatalf("first value = %d, %t", first, ok)
	}
	second, ok := generation.addValue(generatedValue{})
	if !ok || second != 2 {
		t.Fatalf("second value = %d, %t", second, ok)
	}
	if id, ok := generation.addValue(generatedValue{}); ok || id != 0 || len(generation.values) != 2 {
		t.Fatalf("over-limit value appended: id=%d ok=%t len=%d", id, ok, len(generation.values))
	}

	root := valueRoot{Kind: rootSyntax, Syntax: rootRef(t, inputs)}
	if !generation.addRoot(first, root) {
		t.Fatal("valid syntax root rejected")
	}
	for name, candidate := range map[string]struct {
		value valueID
		root  valueRoot
	}{
		"zero handle":     {root: root},
		"foreign handle":  {value: 99, root: root},
		"duplicate root":  {value: first, root: root},
		"zero root":       {value: second},
		"foreign syntax":  {value: second, root: valueRoot{Kind: rootSyntax, Syntax: symbol.SyntaxRef{Module: 99, Node: 99}}},
		"foreign symbol":  {value: second, root: valueRoot{Kind: rootSymbol, Symbol: 999}},
		"bad alternative": {value: second, root: valueRoot{Kind: rootSyntax, Syntax: root.Syntax, Alternative: alternativeTag{Guarded: true}}},
	} {
		t.Run(name, func(t *testing.T) {
			before := len(generation.roots.values)
			if generation.addRoot(candidate.value, candidate.root) || len(generation.roots.values) != before {
				t.Fatalf("invalid root mutated arena: %+v", candidate)
			}
		})
	}
	if got, ok := generation.roots.root(first); !ok || got != root {
		t.Fatalf("root lookup = %+v, %t", got, ok)
	}
	if _, ok := generation.roots.root(0); ok {
		t.Fatal("zero root lookup succeeded")
	}
}

func TestRecordAndControlArenasAreAtomicAndOwnSlices(t *testing.T) {
	inputs := validGenerationInputs(t)
	generation := newGeneration(inputs, diagnostic.NewDiagnosticSet(), Config{
		MaxSyntaxVisits: 4, MaxSemanticRecords: 2, MaxRecordComponents: 2, MaxControlDepth: 2,
	})
	value, _ := generation.addValue(generatedValue{})
	control, ok := generation.addControl(0)
	if !ok || control != 1 {
		t.Fatalf("control = %d, %t", control, ok)
	}
	values := []valueID{value}
	controls := []controlID{control}
	header := rootHeader(t, inputs)
	id, ok := generation.addRecord(retainedRecord{Header: header, Values: values, Controls: controls})
	if !ok || id != 1 {
		t.Fatalf("record = %d, %t", id, ok)
	}
	values[0] = 99
	controls[0] = 99
	stored, ok := generation.records.record(id)
	if !ok || stored.Values[0] != value || stored.Controls[0] != control {
		t.Fatalf("record retained caller slice: %+v", stored)
	}
	stored.Values[0] = 77
	stored, _ = generation.records.record(id)
	if stored.Values[0] != value {
		t.Fatalf("record accessor exposed backing slice: %+v", stored)
	}

	beforeRecords, beforeComponents := len(generation.records.values), generation.records.components
	foreignHeader := header
	foreignHeader.ID = 99
	if _, ok := generation.addRecord(retainedRecord{Header: foreignHeader}); ok || len(generation.records.values) != beforeRecords || generation.records.components != beforeComponents {
		t.Fatal("preassigned record identity partially appended")
	}
	foreignHeader = header
	foreignHeader.Syntax.Module = 99
	if _, ok := generation.addRecord(retainedRecord{Header: foreignHeader}); ok || len(generation.records.values) != beforeRecords || generation.records.components != beforeComponents {
		t.Fatal("foreign record header partially appended")
	}
	if _, ok := generation.addRecord(retainedRecord{Header: header, Values: []valueID{value}}); ok || len(generation.records.values) != beforeRecords || generation.records.components != beforeComponents {
		t.Fatal("component limit failure partially appended")
	}
	if _, ok := generation.addRecord(retainedRecord{Header: header, Values: []valueID{99}}); ok || len(generation.records.values) != beforeRecords || generation.records.components != beforeComponents {
		t.Fatal("foreign value failure partially appended")
	}
	if _, ok := generation.addRecord(retainedRecord{Header: header, Controls: []controlID{99}}); ok || len(generation.records.values) != beforeRecords || generation.records.components != beforeComponents {
		t.Fatal("foreign control failure partially appended")
	}

	if _, ok := generation.records.record(0); ok {
		t.Fatal("zero record lookup succeeded")
	}
	if _, ok := generation.controls.region(99); ok {
		t.Fatal("foreign control lookup succeeded")
	}
}

func TestGenerationEveryLimitIsAtomic(t *testing.T) {
	inputs := validGenerationInputs(t)
	generation := newGeneration(inputs, diagnostic.NewDiagnosticSet(), Config{
		MaxSyntaxVisits: 1, MaxTraversalDepth: 1, MaxSemanticRecords: 1,
		MaxRecordComponents: 1, MaxControlDepth: 1, MaxTrackedPlaces: 1,
		MaxGenericRequirements: 1, MaxConstantDepth: 1, MaxConstantOperations: 2,
		MaxConstantBits: 3,
	})

	if !generation.chargeSyntaxVisit() || generation.chargeSyntaxVisit() || generation.counters.syntaxVisits != 1 {
		t.Fatalf("syntax visits = %d", generation.counters.syntaxVisits)
	}
	if !generation.enterTraversal() || generation.enterTraversal() || generation.counters.traversalDepth != 1 {
		t.Fatalf("traversal depth = %d", generation.counters.traversalDepth)
	}
	generation.leaveTraversal()
	if generation.counters.traversalDepth != 0 {
		t.Fatalf("traversal leave = %d", generation.counters.traversalDepth)
	}
	if !generation.trackPlace() || generation.trackPlace() || generation.counters.trackedPlaces != 1 {
		t.Fatalf("places = %d", generation.counters.trackedPlaces)
	}
	if !generation.addGenericRequirement() || generation.addGenericRequirement() || generation.counters.genericRequirements != 1 {
		t.Fatalf("requirements = %d", generation.counters.genericRequirements)
	}
	if !generation.enterConstant() || generation.enterConstant() || generation.counters.constantDepth != 1 {
		t.Fatalf("constant depth = %d", generation.counters.constantDepth)
	}
	generation.leaveConstant()
	if !generation.chargeConstantOperations(2) || generation.chargeConstantOperations(1) || generation.counters.constantOperations != 2 {
		t.Fatalf("constant operations = %d", generation.counters.constantOperations)
	}
	if !generation.constantBitsAllowed(3) || generation.constantBitsAllowed(4) {
		t.Fatal("constant bit limit not enforced")
	}

	value, _ := generation.addValue(generatedValue{})
	header := rootHeader(t, inputs)
	if _, ok := generation.addRecord(retainedRecord{Header: header, Values: []valueID{value}}); !ok {
		t.Fatal("first bounded record rejected")
	}
	beforeRecords, beforeComponents := len(generation.records.values), generation.records.components
	if _, ok := generation.addRecord(retainedRecord{Header: header}); ok || len(generation.records.values) != beforeRecords || generation.records.components != beforeComponents {
		t.Fatal("record-count limit partially appended")
	}
	control, ok := generation.addControl(0)
	if !ok || control != 1 {
		t.Fatalf("first control = %d, %t", control, ok)
	}
	if _, ok := generation.addControl(control); ok || len(generation.controls.values) != 1 {
		t.Fatal("control-depth limit partially appended")
	}
}

func TestGenerationFreezeIsOneWayAndDefensivelyCopied(t *testing.T) {
	inputs := validGenerationInputs(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	generation := newGeneration(inputs, diagnostics, Config{})
	value, _ := generation.addValue(generatedValue{})
	if !generation.addRoot(value, valueRoot{Kind: rootSyntax, Syntax: rootRef(t, inputs)}) {
		t.Fatal("root rejected")
	}
	control, _ := generation.addControl(0)
	record, _ := generation.addRecord(retainedRecord{Header: rootHeader(t, inputs), Values: []valueID{value}, Controls: []controlID{control}})
	frozen, ok := generation.freeze()
	if !ok || generation.state != generationFrozen || frozen.inputs.Graph != inputs.Graph {
		t.Fatalf("freeze = %+v, %t", frozen, ok)
	}

	values := frozen.Values()
	values[0].ID = 99
	if frozen.Values()[0].ID != value {
		t.Fatal("frozen values exposed backing slice")
	}
	roots := frozen.roots.All()
	roots[0].Value = 99
	if _, ok := frozen.roots.Root(value); !ok {
		t.Fatal("frozen roots exposed backing slice")
	}
	records := frozen.records.Records()
	records[0].Values[0] = 99
	if frozen.records.Records()[0].Values[0] != value || frozen.records.Records()[0].Header.ID != record {
		t.Fatal("frozen records exposed backing slice")
	}
	controls := frozen.records.Controls()
	controls[0].ID = 99
	if frozen.records.Controls()[0].ID != control {
		t.Fatal("frozen controls exposed backing slice")
	}

	if _, ok := generation.freeze(); ok {
		t.Fatal("duplicate freeze succeeded")
	}
	beforeValues := len(generation.values)
	if _, ok := generation.addValue(generatedValue{}); ok || len(generation.values) != beforeValues {
		t.Fatal("mutation after freeze succeeded")
	}
	if generation.addRoot(value, valueRoot{Kind: rootSyntax, Syntax: rootRef(t, inputs)}) {
		t.Fatal("root mutation after freeze succeeded")
	}
	if _, ok := generation.addControl(0); ok {
		t.Fatal("control mutation after freeze succeeded")
	}
	if diagnostics.Len() != 4 {
		t.Fatalf("lifecycle diagnostics = %+v", diagnostics.Items())
	}
	if diagnostics.Items()[3].Code != CodeGeneration {
		t.Fatalf("post-freeze control diagnostic = %+v", diagnostics.Items()[3])
	}
}

func TestRecordControlHierarchyConstructionAndCopies(t *testing.T) {
	inputs := validGenerationInputs(t)
	generation := newGeneration(inputs, diagnostic.NewDiagnosticSet(), Config{MaxControlDepth: 3, MaxSemanticRecords: 5})
	root, ok := generation.addControl(0)
	if !ok || root != 1 {
		t.Fatalf("root = %d, %t", root, ok)
	}
	child, ok := generation.addControl(root)
	if !ok || child != 2 {
		t.Fatalf("child = %d, %t", child, ok)
	}
	grandchild, ok := generation.addControl(child)
	if !ok || grandchild != 3 {
		t.Fatalf("grandchild = %d, %t", grandchild, ok)
	}
	sibling, ok := generation.addControl(root)
	if !ok || sibling != 4 {
		t.Fatalf("sibling = %d, %t", sibling, ok)
	}

	rootRegion, _ := generation.controls.region(root)
	childRegion, _ := generation.controls.region(child)
	grandchildRegion, _ := generation.controls.region(grandchild)
	siblingRegion, _ := generation.controls.region(sibling)
	if rootRegion.Parent != 0 || rootRegion.Depth != 1 {
		t.Fatalf("root hierarchy = %+v", rootRegion)
	}
	if childRegion.Parent != root || childRegion.Depth != 2 {
		t.Fatalf("child hierarchy = %+v", childRegion)
	}
	if grandchildRegion.Parent != child || grandchildRegion.Depth != 3 {
		t.Fatalf("grandchild hierarchy = %+v", grandchildRegion)
	}
	if siblingRegion.Parent != root || siblingRegion.Depth != 2 {
		t.Fatalf("sibling hierarchy = %+v", siblingRegion)
	}
	wantMutable := []mutableControlRegion{
		{ID: root, Depth: 1},
		{ID: child, Parent: root, Depth: 2},
		{ID: grandchild, Parent: child, Depth: 3},
		{ID: sibling, Parent: root, Depth: 2},
	}
	if !reflect.DeepEqual(generation.controls.values, wantMutable) {
		t.Fatalf("mutable controls contain eager hierarchy state: got %+v want %+v", generation.controls.values, wantMutable)
	}

	frozen, ok := generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	if !reflect.DeepEqual(controls[root-1].Children, []controlID{child, sibling}) || !reflect.DeepEqual(controls[child-1].Children, []controlID{grandchild}) {
		t.Fatalf("frozen hierarchy = %+v", controls)
	}
	if controls[root-1].Parent != 0 || controls[root-1].Depth != 1 || controls[child-1].Parent != root || controls[child-1].Depth != 2 || controls[grandchild-1].Parent != child || controls[grandchild-1].Depth != 3 {
		t.Fatalf("frozen parent/depth values = %+v", controls)
	}
	edges := 0
	roots := 0
	for _, region := range controls {
		edges += len(region.Children)
		if region.Parent == 0 {
			roots++
		}
	}
	if edges != len(controls)-roots {
		t.Fatalf("hierarchy edges = %d, regions = %d, roots = %d", edges, len(controls), roots)
	}
	controls[root-1].Children[0] = 99
	controls[child-1].Children[0] = 99
	controls = frozen.records.Controls()
	if !reflect.DeepEqual(controls[root-1].Children, []controlID{child, sibling}) || !reflect.DeepEqual(controls[child-1].Children, []controlID{grandchild}) {
		t.Fatalf("frozen controls exposed children: %+v", controls)
	}
}

func TestRecordControlHierarchyFailuresAreAtomic(t *testing.T) {
	inputs := validGenerationInputs(t)

	t.Run("foreign parent", func(t *testing.T) {
		generation := newGeneration(inputs, diagnostic.NewDiagnosticSet(), Config{MaxControlDepth: 3, MaxSemanticRecords: 3})
		root, _ := generation.addControl(0)
		before, _ := generation.controls.region(root)
		if id, ok := generation.addControl(99); ok || id != 0 || len(generation.controls.values) != 1 {
			t.Fatalf("foreign parent appended: id=%d ok=%t", id, ok)
		}
		after, _ := generation.controls.region(root)
		if !reflect.DeepEqual(after, before) {
			t.Fatalf("foreign-parent failure changed root: before=%+v after=%+v", before, after)
		}
	})

	t.Run("depth limit", func(t *testing.T) {
		generation := newGeneration(inputs, diagnostic.NewDiagnosticSet(), Config{MaxControlDepth: 2, MaxSemanticRecords: 3})
		root, _ := generation.addControl(0)
		child, _ := generation.addControl(root)
		before, _ := generation.controls.region(child)
		if id, ok := generation.addControl(child); ok || id != 0 || len(generation.controls.values) != 2 {
			t.Fatalf("depth-limit child appended: id=%d ok=%t", id, ok)
		}
		after, _ := generation.controls.region(child)
		if !reflect.DeepEqual(after, before) {
			t.Fatalf("depth failure changed parent: before=%+v after=%+v", before, after)
		}
	})

	t.Run("arena limit", func(t *testing.T) {
		generation := newGeneration(inputs, diagnostic.NewDiagnosticSet(), Config{MaxControlDepth: 3, MaxSemanticRecords: 2})
		root, _ := generation.addControl(0)
		_, _ = generation.addControl(root)
		before, _ := generation.controls.region(root)
		if id, ok := generation.addControl(root); ok || id != 0 || len(generation.controls.values) != 2 {
			t.Fatalf("arena-limit sibling appended: id=%d ok=%t", id, ok)
		}
		after, _ := generation.controls.region(root)
		if !reflect.DeepEqual(after, before) {
			t.Fatalf("arena failure changed parent: before=%+v after=%+v", before, after)
		}
	})

	t.Run("depth overflow", func(t *testing.T) {
		arena := controlArena{values: []mutableControlRegion{{ID: 1, Depth: ^uint32(0)}}}
		before, _ := arena.region(1)
		if id, ok := arena.append(1, ^uint32(0), ^uint32(0)); ok || id != 0 || len(arena.values) != 1 {
			t.Fatalf("overflowed depth appended: id=%d ok=%t", id, ok)
		}
		after, _ := arena.region(1)
		if !reflect.DeepEqual(after, before) {
			t.Fatalf("overflow failure changed parent: before=%+v after=%+v", before, after)
		}
	})
}

func TestRecordWideControlHierarchyFreezesLinearly(t *testing.T) {
	inputs := validGenerationInputs(t)
	const siblings = 4096
	generation := newGeneration(inputs, diagnostic.NewDiagnosticSet(), Config{MaxControlDepth: 2, MaxSemanticRecords: siblings + 1})
	root, ok := generation.addControl(0)
	if !ok {
		t.Fatal("root rejected")
	}
	for index := 0; index < siblings; index++ {
		id, added := generation.addControl(root)
		if !added || id != controlID(index+2) {
			t.Fatalf("sibling %d = %d, %t", index, id, added)
		}
	}
	if len(generation.controls.values) != siblings+1 {
		t.Fatalf("mutable region count = %d", len(generation.controls.values))
	}
	for _, region := range generation.controls.values {
		if region.ID != root && (region.Parent != root || region.Depth != 2) {
			t.Fatalf("mutable sibling = %+v", region)
		}
	}

	frozen, ok := generation.freeze()
	if !ok {
		t.Fatal("wide hierarchy freeze failed")
	}
	controls := frozen.records.Controls()
	children := controls[root-1].Children
	if len(children) != siblings {
		t.Fatalf("frozen sibling count = %d", len(children))
	}
	for index, child := range children {
		if child != controlID(index+2) {
			t.Fatalf("child %d = %d", index, child)
		}
	}
	edges := 0
	roots := 0
	for _, region := range controls {
		edges += len(region.Children)
		if region.Parent == 0 {
			roots++
		}
	}
	if edges != len(controls)-roots || edges != siblings {
		t.Fatalf("wide hierarchy edges = %d, regions = %d, roots = %d", edges, len(controls), roots)
	}
}

func TestGenerationDiagnosticsAreBoundedAndDeterministic(t *testing.T) {
	run := func() []diagnostic.Diagnostic {
		diagnostics := diagnostic.NewDiagnosticSet()
		generation := newGeneration(validGenerationInputs(t), diagnostics, Config{MaxDiagnostics: 2, MaxSyntaxVisits: 1})
		generation.addRoot(0, valueRoot{})
		generation.addRoot(99, valueRoot{})
		generation.addRoot(100, valueRoot{})
		if _, ok := generation.freeze(); !ok {
			t.Fatal("freeze failed")
		}
		return diagnostics.Items()
	}
	first := run()
	second := run()
	if !reflect.DeepEqual(first, second) {
		t.Fatalf("diagnostics differ:\n%+v\n%+v", first, second)
	}
	if len(first) != 2 || first[0].Code != CodeGeneration || first[1].Code != CodeGeneration || first[1].Message != "generation diagnostic limit of 2 reached" {
		t.Fatalf("bounded diagnostics = %+v", first)
	}
}
