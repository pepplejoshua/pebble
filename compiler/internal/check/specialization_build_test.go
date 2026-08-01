package check

import (
	"bytes"
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

func fullPipelineFunctionNodes(unit *tir.Unit, symbolID symbol.SymbolID) []tir.Node {
	var declarations []tir.Node
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration && node.Symbol == symbolID {
			declarations = append(declarations, node)
		}
	}
	return declarations
}

func TestBuildUnitBuildsSpecialization(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn identity[T](value T) T { return value; }
let result i32 = identity(1);
`)
	if !ok {
		t.Fatal("full build rejected generic instantiation")
	}
	var symbolID symbol.SymbolID
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration {
			symbolID = node.Symbol
			break
		}
	}
	declarations := fullPipelineFunctionNodes(unit, symbolID)
	if len(declarations) != 2 {
		t.Fatalf("function declarations = %d, want symbolic plus specialization", len(declarations))
	}
	var specialization tir.Node
	for _, declaration := range declarations {
		if len(declaration.TypeArgs) == 1 {
			specialization = declaration
		}
	}
	if specialization.Function == 0 || len(specialization.Parameters) != 1 || specialization.Parameters[0].Type != specialization.ResultType {
		t.Fatalf("specialization declaration = %+v", specialization)
	}
	var body tir.FunctionDecl
	for _, candidate := range unit.FunctionDeclarations() {
		if candidate.FunctionID == specialization.Function {
			body = candidate
		}
	}
	if body.Node == 0 {
		t.Fatal("specialization body missing")
	}
	block := unit.Nodes()[body.Node-1]
	if len(block.Children) != 1 {
		t.Fatalf("specialization body = %+v, want one return", block.Children)
	}
	returnNode := unit.Nodes()[block.Children[0]-1]
	value := unit.Nodes()[returnNode.Children[0]-1]
	if value.Type != specialization.Parameters[0].Type {
		t.Fatalf("specialized return type = %v, want %v", value.Type, specialization.Parameters[0].Type)
	}
}

func TestBuildUnitBuildsDistinctSpecializations(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn identity[T](value T) T { return value; }
let a i32 = identity(1);
let b char = identity('x');
`)
	if !ok {
		t.Fatal("full build rejected generic instantiations")
	}
	var symbolID symbol.SymbolID
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration {
			symbolID = node.Symbol
			break
		}
	}
	declarations := fullPipelineFunctionNodes(unit, symbolID)
	if len(declarations) != 3 {
		t.Fatalf("function declarations = %d, want symbolic plus two specializations", len(declarations))
	}
	seen := make(map[types.TypeID]bool)
	for _, declaration := range declarations {
		if len(declaration.TypeArgs) == 0 {
			continue
		}
		if len(declaration.TypeArgs) != 1 || len(declaration.Parameters) != 1 || declaration.Parameters[0].Type != declaration.TypeArgs[0] || declaration.ResultType != declaration.TypeArgs[0] {
			t.Fatalf("specialization declaration = %+v", declaration)
		}
		seen[declaration.TypeArgs[0]] = true
	}
	if len(seen) != 2 {
		t.Fatalf("specialization type arguments = %v, want two distinct types", seen)
	}
}

func TestBuildUnitDeduplicatesSpecialization(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn identity[T](value T) T { return value; }
let a i32 = identity(1);
let b i32 = identity(2);
`)
	if !ok {
		t.Fatal("full build rejected repeated generic instantiation")
	}
	var symbolID symbol.SymbolID
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration {
			symbolID = node.Symbol
			break
		}
	}
	declarations := fullPipelineFunctionNodes(unit, symbolID)
	if len(declarations) != 2 {
		t.Fatalf("function declarations = %d, want symbolic plus one specialization", len(declarations))
	}
}

// recursiveSpecializationFixture mirrors buildUnit's real setup: store and
// cache are live before any body is built (unlike specializationBuildFixture,
// which installs them after testIRBuildState has already built the normal
// bodies). It deliberately stops before buildBlocks/finishFunctionDeclarations
// so the explicit buildSpecialization call below is the first build of the
// recursive body and the same-key re-entry is exercised there directly.
func recursiveSpecializationFixture(t *testing.T, source, name string) (*types.Store, *irBuildState, []infer.Instantiation) {
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
	if _, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{})); !ok {
		t.Fatal(diagnostics.Items())
	}
	b := tir.NewBuilder(handoff.Semantics.Types(), tir.Config{
		MaxIRNodes: DefaultMaxIRNodes, MaxIRComponents: DefaultMaxIRComponents,
		MaxDumpBytes: DefaultMaxDumpBytes,
	})
	state := &irBuildState{handoff: handoff, records: records, builder: b, store: inputs.Types, cache: newSpecializationCache(), irBuildScope: newIRBuildScope()}
	if !state.buildModules() || !state.buildTypes() || !state.buildDeclarations() || !state.buildTypeUses() || !state.indexExpressions() || !state.indexControls() {
		t.Fatal("failed to build recursive test IR state")
	}
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

func TestBuildSpecializationRecursiveSameKeyTerminates(t *testing.T) {
	// A block body (expression bodies lower to an empty Block in this slice)
	// is required so the recursive generic function-value reference is a real
	// value node inside the specialized body. The in-body bracket selfRef[i32]
	// requests the very specialization whose body is still being built, so
	// buildSpecialization is re-entered under the same cache key mid-build.
	store, state, instantiations := recursiveSpecializationFixture(t, `
fn selfRef[T](value T) T {
    let f fn(i32) i32 = selfRef[i32];
    return value;
}
let result i32 = selfRef(1);
`, "selfRef")
	if len(instantiations) < 2 {
		t.Fatalf("instantiations = %d, want the call site plus the in-body bracket", len(instantiations))
	}
	decl, ok := state.buildSpecialization(instantiations[0])
	if !ok {
		t.Fatal("recursive same-key specialization build failed")
	}
	again, ok := state.buildSpecialization(instantiations[0])
	if !ok || again != decl {
		t.Fatalf("recursive cache hit = (%d, %t), want the same (%d, true)", again, ok, decl)
	}
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("recursive specialization unit verification: %v", err)
	}
	declNode := unit.Nodes()[decl-1]
	if declNode.Kind != tir.FunctionDeclaration || declNode.Symbol != instantiations[0].Generic {
		t.Fatalf("recursive specialized declaration = %+v", declNode)
	}
	if len(declNode.TypeArgs) != 1 || declNode.TypeArgs[0] != store.Builtins().I32 {
		t.Fatalf("recursive specialized TypeArgs = %v, want [i32 %d]", declNode.TypeArgs, store.Builtins().I32)
	}
	var body tir.FunctionDecl
	for _, candidate := range unit.FunctionDeclarations() {
		if candidate.FunctionID == declNode.Function {
			body = candidate
		}
	}
	if body.Node == 0 {
		t.Fatal("recursive specialization body missing")
	}
	block := unit.Nodes()[body.Node-1]
	if block.Kind != tir.Block {
		t.Fatalf("recursive specialization body = %+v, want Block", block)
	}
	var genericValue *tir.Node
	for _, candidate := range unit.Nodes() {
		if candidate.Kind == tir.GenericFunctionValue {
			genericValue = &candidate
			break
		}
	}
	if genericValue == nil {
		t.Fatal("recursive in-body GenericFunctionValue missing")
	}
	if genericValue.Symbol != instantiations[0].Generic || len(genericValue.TypeArgs) != 1 || genericValue.TypeArgs[0] != store.Builtins().I32 {
		t.Fatalf("recursive GenericFunctionValue = %+v, want selfRef[i32]", genericValue)
	}
	instantiationRefs := unit.Instantiations()
	if uint64(genericValue.GenericRef) >= uint64(len(instantiationRefs)) {
		t.Fatalf("recursive GenericFunctionValue GenericRef %d out of range", genericValue.GenericRef)
	}
	if referenced := instantiationRefs[genericValue.GenericRef]; referenced.Declaration != instantiations[0].Generic {
		t.Fatalf("recursive GenericFunctionValue instantiation declaration = %d, want %d", referenced.Declaration, instantiations[0].Generic)
	}
	count := 0
	for _, candidate := range unit.Nodes() {
		if candidate.Kind == tir.FunctionDeclaration && candidate.Symbol == instantiations[0].Generic && len(candidate.TypeArgs) == 1 {
			count++
		}
	}
	if count != 1 {
		t.Fatalf("selfRef[i32] specialization declarations = %d, want exactly one", count)
	}
}

func TestBuildUnitNoGenericsRemainsDeterministic(t *testing.T) {
	const source = `fn main() i32 { return 1; }`
	first, ok := buildUnitFixture(t, source)
	if !ok {
		t.Fatal("first non-generic build failed")
	}
	second, ok := buildUnitFixture(t, source)
	if !ok {
		t.Fatal("second non-generic build failed")
	}
	var firstDump, secondDump bytes.Buffer
	if err := first.Dump(&firstDump); err != nil {
		t.Fatal(err)
	}
	if err := second.Dump(&secondDump); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(firstDump.Bytes(), secondDump.Bytes()) {
		t.Fatal("non-generic build output is not byte-identical")
	}
}
