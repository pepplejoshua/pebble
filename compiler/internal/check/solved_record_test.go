package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// handoffWithChoice builds a minimal solveHandoff that carries a real solution
// with one OneOf choice already resolved to alternative 1. It returns the handoff
// plus the choice ID, the active guarded slot (alternative 1) and the inactive
// guarded slot (alternative 0). Callers can add their own frozenRoots/Constants.
func handoffWithChoice(t *testing.T) (*solveHandoff, infer.ConstraintID, infer.SlotID, infer.SlotID) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	facts := run06a3(inputs, diagnostics, Config{})
	if facts == nil || facts.Program == nil || facts.Session == nil {
		t.Fatal("failed to prepare inference facts")
	}

	session := facts.Session
	program := facts.Program

	selector := session.Variable(infer.Origin{Role: "selector"})
	session.Add(infer.Equal(selector, session.Known(inputs.Types.Builtins().Bool), infer.Origin{}))

	inactive := session.IntegerLiteral([]byte("42"), infer.Origin{Role: "inactive literal"})
	active := session.Variable(infer.Origin{Role: "active result"})

	choiceID, choiceRef := session.AddChoice(infer.OneOf([]infer.Alternative{
		{Label: "integer", Constraints: []infer.Constraint{
			infer.Equal(selector, session.Known(inputs.Types.Builtins().Int), infer.Origin{}),
		}},
		{Label: "boolean", Constraints: []infer.Constraint{
			infer.Equal(active, session.Known(inputs.Types.Builtins().Char), infer.Origin{}),
		}},
	}, infer.Origin{Role: "choice"}))

	inactiveSlot := session.PublishGuardedSlot(choiceRef, 0, inactive)
	activeSlot := session.PublishGuardedSlot(choiceRef, 1, active)

	solution := session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("expected successful solve, got solution=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
	}

	semantics, ok := infer.Snapshot(program, solution, diagnostics)
	if !ok {
		t.Fatal("failed to build semantic snapshot")
	}

	handoff := &solveHandoff{
		Semantics: semantics,
		Solution:  solution,
	}
	return handoff, choiceID, activeSlot, inactiveSlot
}

// TestResolveRecordsUnguardedRoot verifies that a real, unguarded root from a
// normal 06a handoff resolves and is present in the arena.
func TestResolveRecordsUnguardedRoot(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatal("run06a returned nil")
	}
	if handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatal("handoff missing semantics or solution")
	}

	var target rootedValue
	found := false
	for _, rv := range handoff.Roots.All() {
		if !rv.Root.Alternative.Guarded {
			target = rv
			found = true
			break
		}
	}
	if !found {
		t.Fatal("test setup: no unguarded root found")
	}

	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("resolveRecords failed: %+v", diagnostics.Items())
	}
	if _, ok := records.Root(target.Value); !ok {
		t.Fatalf("unguarded root %v not found in arena", target.Value)
	}
}

// TestResolveRecordsGuardedActive verifies that a guarded root whose selection
// matches its Index is resolved and present in the arena.
func TestResolveRecordsGuardedActive(t *testing.T) {
	handoff, choiceID, activeSlot, _ := handoffWithChoice(t)

	handoff.Roots = frozenRoots{values: []rootedValue{
		{Value: 1, Root: valueRoot{
			Kind:        rootSlot,
			Slot:        activeSlot,
			Alternative: alternativeTag{Choice: choiceID, Index: 1, Guarded: true},
		}},
	}}

	records, ok := resolveRecords(handoff, diagnostic.NewDiagnosticSet(), normalizeConfig(Config{}))
	if !ok {
		t.Fatal("resolveRecords failed for an active guarded root")
	}
	result, ok := records.Root(1)
	if !ok {
		t.Fatal("active guarded root was not present in the arena")
	}
	if result.State != infer.TypeFinal {
		t.Fatalf("expected TypeFinal, got %v", result.State)
	}
}

// TestResolveRecordsGuardedInactive verifies that a guarded root whose selection
// does not match its Index is silently excluded and emits no diagnostic.
func TestResolveRecordsGuardedInactive(t *testing.T) {
	handoff, choiceID, _, inactiveSlot := handoffWithChoice(t)

	diagnostics := diagnostic.NewDiagnosticSet()
	before := diagnostics.Len()

	handoff.Roots = frozenRoots{values: []rootedValue{
		{Value: 1, Root: valueRoot{
			Kind:        rootSlot,
			Slot:        inactiveSlot,
			Alternative: alternativeTag{Choice: choiceID, Index: 0, Guarded: true},
		}},
	}}

	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal("resolveRecords failed for an inactive guarded root")
	}
	if _, ok := records.Root(1); ok {
		t.Fatal("inactive guarded root should not be present in the arena")
	}
	if diagnostics.Len() != before {
		t.Fatalf("inactive guarded root produced diagnostics: %+v", diagnostics.Items())
	}
}

// TestResolveRecordsDuplicateRoot verifies that a duplicated valueID across
// Roots.All() emits C0619 and fails the arena.
func TestResolveRecordsDuplicateRoot(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	full := run06a(inputs, diagnostics, Config{})
	if full == nil || len(full.Compilation.Modules) == 0 || len(full.Compilation.Modules[0].Declarations) == 0 {
		t.Fatal("test setup: no valid handoff with module declarations")
	}
	symbolID := full.Compilation.Modules[0].Declarations[0]

	handoff := &solveHandoff{
		Semantics: full.Semantics,
		Solution:  full.Solution,
		Roots: frozenRoots{values: []rootedValue{
			{Value: 1, Root: valueRoot{Kind: rootSymbol, Symbol: symbolID}},
			{Value: 1, Root: valueRoot{Kind: rootSymbol, Symbol: symbolID}},
		}},
	}

	resolveDiags := diagnostic.NewDiagnosticSet()
	records, ok := resolveRecords(handoff, resolveDiags, normalizeConfig(Config{}))
	if ok || records != nil {
		t.Fatal("expected duplicate root to fail arena construction")
	}
	if !hasCodeGenerationMessage(resolveDiags, "duplicate root value") {
		t.Fatalf("expected C0619 for duplicate root, got %+v", resolveDiags.Items())
	}
}

// TestResolveRecordsOutOfRangeParameter verifies that an instantiation root with
// Parameter beyond the solved argument count emits C0619.
func TestResolveRecordsOutOfRangeParameter(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn identity[T](value T) T => value;\nfn main() void {}\n")})
	facts := run06a3(inputs, diagnostics, Config{})
	if facts == nil || facts.Session == nil || facts.Program == nil {
		t.Fatal("test setup: failed to prepare session for instantiation")
	}

	var genericSymbol symbol.SymbolID
	for _, sym := range inputs.Resolution.Symbols.All() {
		if sym.Generic && sym.Name == "identity" {
			genericSymbol = sym.ID
			break
		}
	}
	if genericSymbol == 0 {
		t.Fatal("test setup: generic identity symbol not found")
	}

	rootModule, ok := inputs.Graph.Module(inputs.Graph.Root)
	if !ok {
		t.Fatal("test setup: no root module")
	}
	instSite := symbol.SyntaxRef{Module: rootModule.ID, Node: rootModule.Tree.Root()}

	facts.Session.PublishInstantiation(instSite, genericSymbol, []infer.Term{facts.Session.Known(inputs.Types.Builtins().I32)})
	solution := facts.Session.Solve()
	if !solution.Successful() {
		t.Fatalf("re-solve failed: %+v", diagnostics.Items())
	}
	semantics, ok := infer.Snapshot(facts.Program, solution, diagnostic.NewDiagnosticSet())
	if !ok {
		t.Fatalf("semantic snapshot after re-solve failed: %+v", diagnostics.Items())
	}

	handoff := &solveHandoff{
		Semantics: semantics,
		Solution:  solution,
		Roots: frozenRoots{values: []rootedValue{
			{Value: 1, Root: valueRoot{
				Kind:      rootInstantiation,
				Syntax:    instSite,
				Parameter: 5,
			}},
		}},
	}

	resolveDiags := diagnostic.NewDiagnosticSet()
	records, ok := resolveRecords(handoff, resolveDiags, normalizeConfig(Config{}))
	if ok || records != nil {
		t.Fatal("expected out-of-range parameter to fail arena construction")
	}
	if !hasCodeGenerationMessage(resolveDiags, "root instantiation parameter is out of range") {
		t.Fatalf("expected C0619 for out-of-range parameter, got %+v", resolveDiags.Items())
	}
}

// TestResolveRecordsConstant verifies that a frozen constant resolves into the
// arena and is queryable by its syntax site.
func TestResolveRecordsConstant(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let repeated [3]i32 = [1; 3];\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatal("run06a returned nil")
	}
	all := handoff.Constants.All()
	if len(all) == 0 {
		t.Fatal("test setup: no constants produced")
	}
	ref := all[0].Syntax

	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("resolveRecords failed: %+v", diagnostics.Items())
	}
	if _, ok := records.Constant(ref); !ok {
		t.Fatalf("constant %v not found in arena", ref)
	}
}

// TestResolveRecordsDuplicateConstant verifies that a duplicated syntax site in
// Constants.All() emits C0619 and fails the arena.
func TestResolveRecordsDuplicateConstant(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let repeated [3]i32 = [1; 3];\n")})
	full := run06a(inputs, diagnostics, Config{})
	if full == nil {
		t.Fatal("run06a returned nil")
	}
	all := full.Constants.All()
	if len(all) == 0 {
		t.Fatal("test setup: no constants produced")
	}
	ref := all[0].Syntax

	handoff := &solveHandoff{
		Semantics: full.Semantics,
		Solution:  full.Solution,
		Constants: frozenConstants{values: []frozenConstant{
			{Syntax: ref, Result: all[0].Result},
			{Syntax: ref, Result: all[0].Result},
		}},
	}

	resolveDiags := diagnostic.NewDiagnosticSet()
	records, ok := resolveRecords(handoff, resolveDiags, normalizeConfig(Config{}))
	if ok || records != nil {
		t.Fatal("expected duplicate constant to fail arena construction")
	}
	if !hasCodeGenerationMessage(resolveDiags, "duplicate constant syntax") {
		t.Fatalf("expected C0619 for duplicate constant, got %+v", resolveDiags.Items())
	}
}

// TestResolveRecordsTypeErrorStored verifies that a root whose solution result
// is in TypeError state is still stored, not treated as a resolution failure.
func TestResolveRecordsTypeErrorStored(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn conflict() void {
    let x u8 = 1;
    let y i32 = 2;
    let z i32 = x + y;
}
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Solution == nil {
		t.Fatal("run06a returned nil or no solution")
	}

	var targetSymbol symbol.SymbolID
	for _, st := range handoff.Solution.SymbolTypes() {
		if st.Result.State == infer.TypeError {
			targetSymbol = st.Symbol
			break
		}
	}
	if targetSymbol == 0 {
		t.Fatal("test setup: no TypeError symbol result found")
	}

	const customValue = 999
	handoff.Roots = frozenRoots{values: []rootedValue{
		{Value: customValue, Root: valueRoot{Kind: rootSymbol, Symbol: targetSymbol}},
	}}

	records, ok := resolveRecords(handoff, diagnostic.NewDiagnosticSet(), normalizeConfig(Config{}))
	if !ok {
		t.Fatal("resolveRecords should not fail on a TypeError root")
	}
	result, ok := records.Root(customValue)
	if !ok {
		t.Fatal("TypeError root was not stored in the arena")
	}
	if result.State != infer.TypeError {
		t.Fatalf("expected TypeError state, got %v", result.State)
	}
}

func hasCodeGenerationMessage(set *diagnostic.DiagnosticSet, message string) bool {
	for _, d := range set.Items() {
		if d.Code == CodeGeneration && d.Message == message {
			return true
		}
	}
	return false
}
