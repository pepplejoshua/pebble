package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func specializationBuildFixture(t *testing.T, source, name string) (*types.Store, *irBuildState, []infer.Instantiation) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	state := testIRBuildState(t, handoff, records, requirements)
	state.store = inputs.Types
	state.cache = newSpecializationCache()

	var generic symbol.SymbolID
	for _, candidate := range inputs.Resolution.Symbols.All() {
		if candidate.Name == name && candidate.Kind == symbol.SymbolFunction {
			generic = candidate.ID
			break
		}
	}
	var instantiations []infer.Instantiation
	for _, instantiation := range handoff.Solution.Instantiations() {
		if instantiation.Generic == generic {
			instantiations = append(instantiations, instantiation)
		}
	}
	if generic == 0 || len(instantiations) == 0 {
		t.Fatalf("missing generic %q or solved instantiations", name)
	}
	return inputs.Types, state, instantiations
}

func TestBuildSpecialization(t *testing.T) {
	store, state, instantiations := specializationBuildFixture(t, `
fn identity[T](value T) T => value;
let a i32 = identity(1);
`, "identity")
	decl, ok := state.buildSpecialization(instantiations[0])
	if !ok {
		t.Fatal("buildSpecialization failed")
	}
	if again, ok := state.buildSpecialization(instantiations[0]); !ok || again != decl {
		t.Fatalf("cache hit = (%d, %t), want (%d, true)", again, ok, decl)
	}
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("specialized unit verification: %v", err)
	}
	node := unit.Nodes()[decl-1]
	if len(node.Parameters) != 1 || node.Parameters[0].Type != store.Builtins().I32 || node.ResultType != store.Builtins().I32 {
		t.Fatalf("specialized declaration = %+v, want i32 parameter and result", node)
	}
}

func TestBuildSpecializationDistinctInstantiations(t *testing.T) {
	store, state, instantiations := specializationBuildFixture(t, `
fn identity[T](value T) T => value;
let a i32 = identity(1);
let b char = identity('x');
`, "identity")
	if len(instantiations) != 2 {
		t.Fatalf("instantiations = %d, want 2", len(instantiations))
	}
	first, ok := state.buildSpecialization(instantiations[0])
	if !ok {
		t.Fatal("first specialization failed")
	}
	second, ok := state.buildSpecialization(instantiations[1])
	if !ok {
		t.Fatal("second specialization failed")
	}
	if first == second {
		t.Fatal("different instantiations reused one declaration node")
	}
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("specialized unit verification: %v", err)
	}
	if unit.Nodes()[first-1].ResultType != store.Builtins().I32 || unit.Nodes()[second-1].ResultType != store.Builtins().Char {
		t.Fatalf("specialized results = %d, %d; want i32, char", unit.Nodes()[first-1].ResultType, unit.Nodes()[second-1].ResultType)
	}
}

func TestBuildSpecializationSubstitutesCompositeBody(t *testing.T) {
	// A block body (not an expression body: `=> items[0];` lowers to an
	// empty Block with no children -- a pre-existing gap in expression-body
	// lowering, confirmed identical for an ordinary non-generic function
	// through the normal, already-accepted pipeline, unrelated to
	// specialization) is required here so the body actually contains real
	// value nodes whose substituted types this test can inspect.
	store, state, instantiations := specializationBuildFixture(t, `
fn first[T](items []T) T { return items[0]; }
let values []i32 = [1, 2, 3];
let result i32 = first(values);
`, "first")
	decl, ok := state.buildSpecialization(instantiations[0])
	if !ok {
		t.Fatal("buildSpecialization failed")
	}
	want, err := store.Intern(types.SliceKey(store.Builtins().I32))
	if err != nil {
		t.Fatal(err)
	}
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("specialized unit verification: %v", err)
	}
	declNode := unit.Nodes()[decl-1]
	if len(declNode.Parameters) != 1 || declNode.Parameters[0].Type != want {
		t.Fatalf("specialized parameter = %+v, want []i32 %d", declNode.Parameters, want)
	}
	var body tir.FunctionDecl
	for _, f := range unit.FunctionDeclarations() {
		if f.FunctionID == declNode.Function {
			body = f
		}
	}
	block := unit.Nodes()[body.Node-1]
	if len(block.Children) != 1 {
		t.Fatalf("specialized body block children = %+v, want exactly one Return statement", block.Children)
	}
	returnNode := unit.Nodes()[block.Children[0]-1]
	if returnNode.Kind != tir.Return || len(returnNode.Children) != 1 {
		t.Fatalf("specialized body statement = %+v, want a single-value Return", returnNode)
	}
	loadNode := unit.Nodes()[returnNode.Children[0]-1]
	if loadNode.Type != store.Builtins().I32 {
		t.Fatalf("specialized body's returned value type = %v, want the substituted i32 %v (not the symbolic type parameter)", loadNode.Type, store.Builtins().I32)
	}
}
