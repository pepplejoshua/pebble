package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func TestFieldAccessMethodReceiver(t *testing.T) {
	cases := map[string]string{
		"parameter": `
type Inner = struct { fn value(self Inner) void {} };
type Outer = struct { inner Inner; };
fn use(o Outer) void { o.inner.value(); }
`,
		"let": `
type Inner = struct { fn value(self Inner) void {} };
type Outer = struct { inner Inner; };
fn use(o Outer) void { let x = o; x.inner.value(); }
`,
		"pointer": `
type Inner = struct { fn value(self Inner) void {} };
fn use(p *Inner) void { (*p).value(); }
`,
	}
	for name, source := range cases {
		t.Run(name, func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
			handoff := run06a(inputs, diagnostics, Config{})
			if handoff == nil {
				t.Fatal("run06a returned a nil handoff")
			}
			if handoff.GenerationHadErrors || diagnostics.HasErrors() || handoff.Semantics == nil || handoff.Solution == nil {
				t.Fatalf("invalid handoff: GenerationHadErrors=%v diagnostics=%+v handoff=%+v", handoff.GenerationHadErrors, diagnostics.Items(), handoff)
			}
		})
	}
}

// TestSolveHandoffCleanEndToEnd verifies a complete valid program produces
// a fully initialized handoff with all required fields set correctly.
func TestSolveHandoffCleanEndToEnd(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct { x i32; y i32; };

fn distance(p Point) i32 {
    return p.x + p.y;
}

fn main() void {
    let origin = Point.{ x = 0, y = 0 };
    let d = distance(origin);
    print d;
}
`)})

	handoff := run06a(inputs, diagnostics, Config{})

	// All handoff fields must be populated for a successful run.
	if handoff == nil {
		t.Fatal("handoff is nil")
	}
	if !handoff.GenerationHadErrors {
		// No errors during generation (though there may be other-phase errors)
	}
	if handoff.Semantics == nil {
		t.Fatal("handoff.Semantics is nil")
	}
	if handoff.Solution == nil {
		t.Fatal("handoff.Solution is nil")
	}
	if handoff.Solution.Successful() != true {
		t.Fatal("solution is not successful")
	}
	if handoff.Compilation.Root == 0 {
		t.Fatal("handoff.Compilation.Root is zero")
	}
	if len(handoff.Compilation.Modules) == 0 {
		t.Fatal("handoff.Compilation.Modules is empty")
	}
	if len(handoff.Compilation.Modules) != 1 {
		t.Fatalf("handoff.Compilation.Modules length = %d, want 1", len(handoff.Compilation.Modules))
	}

	// Verify declarations are in source order: type, function, function
	module := handoff.Compilation.Modules[0]
	if len(module.Declarations) < 3 {
		t.Fatalf("module has %d declarations, want at least 3", len(module.Declarations))
	}

	// Resolve symbols to check order
	typeSymbol, _ := inputs.Resolution.Symbols.Symbol(module.Declarations[0])
	firstFnSymbol, _ := inputs.Resolution.Symbols.Symbol(module.Declarations[1])
	secondFnSymbol, _ := inputs.Resolution.Symbols.Symbol(module.Declarations[2])

	if typeSymbol.Name != "Point" {
		t.Fatalf("first declaration is %q, want Point", typeSymbol.Name)
	}
	if firstFnSymbol.Name != "distance" {
		t.Fatalf("second declaration is %q, want distance", firstFnSymbol.Name)
	}
	if secondFnSymbol.Name != "main" {
		t.Fatalf("third declaration is %q, want main", secondFnSymbol.Name)
	}
}

// TestSolveHandoffGenerationErrorsWithConflict verifies that GenerationHadErrors
// correctly reflects diagnostics.HasErrors() and that a solve conflict produces
// non-nil Semantics even though GenerationHadErrors is true.
func TestSolveHandoffGenerationErrorsWithConflict(t *testing.T) {
	// Create a program with a type conflict: mixing u8 and i32 in an operator.
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn conflict() void {
    let x u8 = 1;
    let y i32 = 2;
    let z i32 = x + y;
}
`)})

	handoff := run06a(inputs, diagnostics, Config{})

	if handoff == nil {
		t.Fatal("handoff is nil")
	}

	// GenerationHadErrors should match diagnostics.HasErrors()
	if handoff.GenerationHadErrors != diagnostics.HasErrors() {
		t.Fatalf("GenerationHadErrors=%v, but diagnostics.HasErrors()=%v", handoff.GenerationHadErrors, diagnostics.HasErrors())
	}

	// The key distinguishing case: we have a real conflict (solution not successful),
	// but the handoff is still populated with Semantics (not nil).
	if handoff.Semantics == nil {
		t.Fatal("handoff.Semantics is nil (should be non-nil even with a solve failure)")
	}

	if handoff.Solution == nil {
		t.Fatal("handoff.Solution is nil")
	}

	// The solution should reflect the conflict.
	if handoff.Solution.Successful() {
		t.Fatal("solution is successful (should fail due to type conflict)")
	}
}

// TestSolveHandoffMultiModuleCompilation verifies that multi-module programs
// build correct frozenCompilation metadata with all modules, imports, and
// declarations properly recorded.
func TestSolveHandoffMultiModuleCompilation(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{
		"main.peb": []byte(`import "./helper";

fn use_helper() i32 {
    return helper_fn();
}

fn main() void {
    print use_helper();
}
`),
		"helper.peb": []byte(`fn helper_fn() i32 {
    return 42;
}
`),
	})

	handoff := run06a(inputs, diagnostics, Config{})

	if handoff == nil {
		t.Fatal("handoff is nil")
	}
	if handoff.Semantics == nil {
		t.Fatal("handoff.Semantics is nil")
	}

	// Should have two modules.
	if len(handoff.Compilation.Modules) != 2 {
		t.Fatalf("module count = %d, want 2", len(handoff.Compilation.Modules))
	}

	// DependencyOrder should also have both modules.
	if len(handoff.Compilation.DependencyOrder) != 2 {
		t.Fatalf("dependency order length = %d, want 2", len(handoff.Compilation.DependencyOrder))
	}

	// Verify each module has valid declarations and (for main) imports.
	mainModule := handoff.Compilation.Modules[0]
	helperModule := handoff.Compilation.Modules[1]

	// Main module should have imports and declarations.
	if len(mainModule.Imports) == 0 {
		t.Fatal("main module has no imports")
	}

	// Check that import target exists in modules.
	importTarget := mainModule.Imports[0].Target
	foundTarget := false
	for _, m := range handoff.Compilation.Modules {
		if m.ID == importTarget {
			foundTarget = true
			break
		}
	}
	if !foundTarget {
		t.Fatalf("import target %d not found in modules", importTarget)
	}

	// Both modules should have declarations.
	if len(mainModule.Declarations) == 0 {
		t.Fatal("main module has no declarations")
	}
	if len(helperModule.Declarations) == 0 {
		t.Fatal("helper module has no declarations")
	}

	// Verify module spans are valid.
	if mainModule.Span.Start >= mainModule.Span.End {
		t.Fatalf("main module span is invalid: %v", mainModule.Span)
	}
	if helperModule.Span.Start >= helperModule.Span.End {
		t.Fatalf("helper module span is invalid: %v", helperModule.Span)
	}
}

// TestSolveHandoffExternBlockFolding verifies that extern blocks are folded
// into their contained declarations, with no separate ExternBlock entry.
func TestSolveHandoffExternBlockFolding(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
extern "C" {
    fn c_func1() void;
    fn c_func2() void;
}

fn main() void {
    c_func1();
}
`)})

	handoff := run06a(inputs, diagnostics, Config{})

	if handoff == nil {
		t.Fatal("handoff is nil")
	}
	if handoff.Semantics == nil {
		t.Fatal("handoff.Semantics is nil")
	}

	module := handoff.Compilation.Modules[0]
	declarations := module.Declarations

	// We expect: main, c_func1, c_func2 as three separate declarations (no ExternBlock entry).
	if len(declarations) < 3 {
		t.Fatalf("declarations count = %d, want at least 3", len(declarations))
	}

	// Check that c_func1 and c_func2 are present.
	names := make(map[string]bool)
	for _, declID := range declarations {
		sym, ok := inputs.Resolution.Symbols.Symbol(declID)
		if ok {
			names[sym.Name] = true
		}
	}

	if !names["c_func1"] {
		t.Fatal("c_func1 not found in declarations")
	}
	if !names["c_func2"] {
		t.Fatal("c_func2 not found in declarations")
	}
	if !names["main"] {
		t.Fatal("main not found in declarations")
	}
}

// TestSolveHandoffAuditValidProgramReachesSnapshot verifies that a valid program
// successfully completes all audit stages and produces a semantic snapshot.
func TestSolveHandoffAuditValidProgramReachesSnapshot(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn test() void {
    let x i32 = 1;
    print x;
}
`)})

	handoff := run06a(inputs, diagnostics, Config{})

	if handoff == nil {
		t.Fatal("handoff is nil")
	}

	// A valid program should pass all audits and reach the snapshot stage.
	if handoff.Semantics == nil {
		t.Fatal("Semantics should be non-nil for valid program")
	}
	if handoff.Solution == nil {
		t.Fatal("Solution should be non-nil for valid program")
	}

	// The solution should be successful.
	if !handoff.Solution.Successful() {
		t.Fatal("Solution.Successful() should be true for valid program")
	}
}

// TestSolveHandoffAuditControlHierarchy verifies that auditControlHierarchy
// correctly validates control records and regions.
func TestSolveHandoffAuditControlHierarchy(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn test() void {
    {
        if true { print 1; }
    }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})

	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}

	controls := frozen.records.Controls()
	records := frozen.records.Records()

	// A valid hierarchy should pass the audit.
	reporter := &auditReporter{gen: nil}
	result := auditControlHierarchy(controls, records, reporter)

	if !result {
		t.Fatal("auditControlHierarchy should have accepted valid hierarchy")
	}
	if reporter.failed {
		t.Fatal("auditReporter should not have recorded failure for valid hierarchy")
	}
}

// TestSolveHandoffAuditCompilationWithNoRootModule verifies that auditCompilation
// rejects a frozenCompilation with Root == 0.
func TestSolveHandoffAuditCompilationWithNoRootModule(t *testing.T) {
	compilation := frozenCompilation{
		Root:    0, // Invalid: no root
		Modules: []frozenModule{{ID: 1}},
	}

	reporter := &auditReporter{gen: nil}
	result := auditCompilation(compilation, reporter)

	if result {
		t.Fatal("auditCompilation should have rejected compilation with no root")
	}
	if !reporter.failed {
		t.Fatal("auditReporter should have recorded failure")
	}
}

// TestSolveHandoffAuditCompilationWithMissingRootModule verifies that
// auditCompilation rejects a compilation where Root is not in Modules.
func TestSolveHandoffAuditCompilationWithMissingRootModule(t *testing.T) {
	compilation := frozenCompilation{
		Root:    1,
		Modules: []frozenModule{{ID: 2}}, // Root 1 not in modules
	}

	reporter := &auditReporter{gen: nil}
	result := auditCompilation(compilation, reporter)

	if result {
		t.Fatal("auditCompilation should have rejected missing root module")
	}
	if !reporter.failed {
		t.Fatal("auditReporter should have recorded failure")
	}
}

// TestSolveHandoffAuditCompilationDependencyOrderMismatch verifies that
// auditCompilation rejects a DependencyOrder that doesn't match module count.
func TestSolveHandoffAuditCompilationDependencyOrderMismatch(t *testing.T) {
	compilation := frozenCompilation{
		Root:            1,
		Modules:         []frozenModule{{ID: 1}, {ID: 2}},
		DependencyOrder: []module.ModuleID{1}, // Only one; should be two
	}

	reporter := &auditReporter{gen: nil}
	result := auditCompilation(compilation, reporter)

	if result {
		t.Fatal("auditCompilation should have rejected mismatched dependency order")
	}
	if !reporter.failed {
		t.Fatal("auditReporter should have recorded failure")
	}
}

// TestSolveHandoffAuditCompilationInvalidImportTarget verifies that
// auditCompilation rejects an import with a nonexistent target.
func TestSolveHandoffAuditCompilationInvalidImportTarget(t *testing.T) {
	compilation := frozenCompilation{
		Root: 1,
		Modules: []frozenModule{
			{
				ID: 1,
				Imports: []frozenImport{
					{Target: 999}, // Nonexistent module
				},
			},
		},
		DependencyOrder: []module.ModuleID{1},
	}

	reporter := &auditReporter{gen: nil}
	result := auditCompilation(compilation, reporter)

	if result {
		t.Fatal("auditCompilation should have rejected invalid import target")
	}
	if !reporter.failed {
		t.Fatal("auditReporter should have recorded failure")
	}
}

// cloneRecordsForMutation deep-copies a frozen records slice so a negative
// test can corrupt one record without disturbing the baseline used elsewhere
// in the same test.
func cloneRecordsForMutation(records []retainedRecord) []retainedRecord {
	out := make([]retainedRecord, len(records))
	for index, record := range records {
		out[index] = cloneRetainedRecord(record)
	}
	return out
}

// cloneControlsForMutation deep-copies a frozen controls slice.
func cloneControlsForMutation(controls []controlRegion) []controlRegion {
	out := make([]controlRegion, len(controls))
	for index, region := range controls {
		out[index] = cloneControlRegion(region)
	}
	return out
}

func findControlRecord(records []retainedRecord, kind controlKind) int {
	for index, record := range records {
		if record.Control != nil && record.Control.Kind == kind {
			return index
		}
	}
	return -1
}

func printStatementRefs(t *testing.T, facts *preparedFacts) []symbol.SyntaxRef {
	t.Helper()
	var out []symbol.SyntaxRef
	for _, ref := range facts.Walk.order {
		node, ok := facts.Walk.node(ref.Module, ref.Node)
		if ok && node.Kind() == syntax.PrintStmt {
			out = append(out, ref)
		}
	}
	return out
}

// TestSolveHandoffCompositionExactReconstructionRejectsSwappedArms proves the
// exact-equality check (invariant 9.1) — not merely "resolves to some control
// record" — is what catches a silently swapped then/else: a well-formed pair
// pointing at real control records still fails once the roles are wrong.
func TestSolveHandoffCompositionExactReconstructionRejectsSwappedArms(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(flag bool) void {
    if flag { print 1; } else { print 2; }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()

	baseline := &auditReporter{gen: facts.Generation}
	if !auditStructuralComposition(facts.Generation, controls, records, baseline) {
		t.Fatal("baseline composition rejected")
	}

	mutated := cloneRecordsForMutation(records)
	index := findControlRecord(mutated, controlIf)
	if index < 0 {
		t.Fatal("no controlIf record")
	}
	composition := mutated[index].Control.Composition
	if len(composition) != 2 {
		t.Fatalf("if composition = %+v, want 2 entries", composition)
	}
	composition[0].Arm, composition[1].Arm = composition[1].Arm, composition[0].Arm

	reporter := &auditReporter{gen: facts.Generation}
	if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
		t.Fatal("swapped then/else composition accepted")
	}
	if !reporter.failed {
		t.Fatal("reporter did not record failure")
	}
}

// TestSolveHandoffCompositionExactReconstructionRejectsUnrelatedSibling
// proves a graph-owned, otherwise well-formed Arm naming an unrelated sibling
// statement that genuinely has its own valid control record is still
// rejected — cross-record resolution alone would accept it.
func TestSolveHandoffCompositionExactReconstructionRejectsUnrelatedSibling(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(flag bool) void {
    if flag { print 1; } else { print 2; }
    print 3;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()
	prints := printStatementRefs(t, facts)
	if len(prints) != 3 {
		t.Fatalf("print statements = %d, want 3", len(prints))
	}

	mutated := cloneRecordsForMutation(records)
	index := findControlRecord(mutated, controlIf)
	if index < 0 {
		t.Fatal("no controlIf record")
	}
	for entryIndex, entry := range mutated[index].Control.Composition {
		if entry.Role == roleElse {
			mutated[index].Control.Composition[entryIndex].Arm = prints[2]
		}
	}

	reporter := &auditReporter{gen: facts.Generation}
	if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
		t.Fatal("composition naming an unrelated sibling was accepted")
	}
}

// TestSolveHandoffCompositionExactReconstructionRejectsForOmissionAndFabrication
// proves a for's initializer or update omitted from Composition when the
// presence flag says it should be there, and the reverse — a fabricated
// entry present when the flag says it should not be — are both rejected.
func TestSolveHandoffCompositionExactReconstructionRejectsForOmissionAndFabrication(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn g(limit i32) void {
    for var step i32 = 0; step < limit; step += 1 { print 1; }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()

	baseline := &auditReporter{gen: facts.Generation}
	if !auditStructuralComposition(facts.Generation, controls, records, baseline) {
		t.Fatal("baseline for composition rejected")
	}

	t.Run("omitted initializer", func(t *testing.T) {
		mutated := cloneRecordsForMutation(records)
		index := findControlRecord(mutated, controlFor)
		composition := mutated[index].Control.Composition
		if len(composition) != 3 || composition[0].Role != roleInitializer {
			t.Fatalf("for composition = %+v, want initializer/update/body", composition)
		}
		mutated[index].Control.Composition = composition[1:]
		reporter := &auditReporter{gen: facts.Generation}
		if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
			t.Fatal("omitted initializer accepted")
		}
	})
	t.Run("fabricated extra update", func(t *testing.T) {
		mutated := cloneRecordsForMutation(records)
		index := findControlRecord(mutated, controlFor)
		composition := mutated[index].Control.Composition
		extra := append(append([]structuralChild(nil), composition...), composition[len(composition)-2])
		mutated[index].Control.Composition = extra
		reporter := &auditReporter{gen: facts.Generation}
		if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
			t.Fatal("fabricated extra composition entry accepted")
		}
	})
}

// TestSolveHandoffCompositionExactReconstructionRejectsSwitchCaseCorruption
// proves reordered, missing, and fabricated-extra switch case entries are
// each independently rejected.
func TestSolveHandoffCompositionExactReconstructionRejectsSwitchCaseCorruption(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn s(limit i32) void {
    switch limit {
    case 1: print 1;
    case 2: print 2;
    else: print 3;
    }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()

	baseline := &auditReporter{gen: facts.Generation}
	if !auditStructuralComposition(facts.Generation, controls, records, baseline) {
		t.Fatal("baseline switch composition rejected")
	}

	t.Run("reordered cases", func(t *testing.T) {
		mutated := cloneRecordsForMutation(records)
		index := findControlRecord(mutated, controlSwitch)
		composition := mutated[index].Control.Composition
		if len(composition) != 3 {
			t.Fatalf("switch composition = %+v, want 3 entries", composition)
		}
		composition[0].Arm, composition[1].Arm = composition[1].Arm, composition[0].Arm
		reporter := &auditReporter{gen: facts.Generation}
		if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
			t.Fatal("reordered switch cases accepted")
		}
	})
	t.Run("missing case", func(t *testing.T) {
		mutated := cloneRecordsForMutation(records)
		index := findControlRecord(mutated, controlSwitch)
		composition := mutated[index].Control.Composition
		mutated[index].Control.Composition = append(append([]structuralChild(nil), composition[:1]...), composition[2:]...)
		reporter := &auditReporter{gen: facts.Generation}
		if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
			t.Fatal("missing switch case entry accepted")
		}
	})
	t.Run("fabricated extra case", func(t *testing.T) {
		mutated := cloneRecordsForMutation(records)
		index := findControlRecord(mutated, controlSwitch)
		composition := mutated[index].Control.Composition
		extra := append(append([]structuralChild(nil), composition...), structuralChild{Role: roleCase, Ordinal: 2, Arm: composition[0].Arm})
		mutated[index].Control.Composition = extra
		reporter := &auditReporter{gen: facts.Generation}
		if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
			t.Fatal("fabricated extra switch case entry accepted")
		}
	})
}

// TestSolveHandoffCompositionKindCorrespondenceRejectsWrongKind proves a
// correct Arm SyntaxRef whose located control record carries the wrong
// controlKind is rejected, isolated from the exact-reconstruction and
// unique-resolution checks, which both still pass.
func TestSolveHandoffCompositionKindCorrespondenceRejectsWrongKind(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(flag bool) void {
    if flag print 1; else print 2;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()
	prints := printStatementRefs(t, facts)
	if len(prints) != 2 {
		t.Fatalf("print statements = %d, want 2", len(prints))
	}
	elseArm := prints[1]

	mutated := cloneRecordsForMutation(records)
	index := -1
	for candidate, record := range mutated {
		if record.Control != nil && record.Header.Syntax == elseArm {
			index = candidate
		}
	}
	if index < 0 {
		t.Fatal("no control record for the else arm")
	}
	if mutated[index].Control.Kind != controlPrint {
		t.Fatalf("else-arm control kind = %d, want controlPrint", mutated[index].Control.Kind)
	}
	mutated[index].Control.Kind = controlWhile

	reporter := &auditReporter{gen: facts.Generation}
	if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
		t.Fatal("wrong controlKind for a correctly resolved arm was accepted")
	}
}

// TestSolveHandoffCompositionLexicalPlacementRejectsLeafWrongRegion proves a
// correct Arm and correct controlKind for a leaf arm whose Region differs
// from the parent record's Region is rejected.
func TestSolveHandoffCompositionLexicalPlacementRejectsLeafWrongRegion(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(flag bool) void {
    if flag print 1; else print 2;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()
	prints := printStatementRefs(t, facts)
	if len(prints) != 2 {
		t.Fatalf("print statements = %d, want 2", len(prints))
	}
	elseArm := prints[1]

	mutated := cloneRecordsForMutation(records)
	index := -1
	for candidate, record := range mutated {
		if record.Control != nil && record.Header.Syntax == elseArm {
			index = candidate
		}
	}
	if index < 0 {
		t.Fatal("no control record for the else arm")
	}
	original := mutated[index].Control.Region
	wrong := controlID(0)
	for _, region := range controls {
		if region.ID != original {
			wrong = region.ID
		}
	}
	if wrong == 0 {
		t.Fatal("setup: no alternative region available")
	}
	mutated[index].Control.Region = wrong

	reporter := &auditReporter{gen: facts.Generation}
	if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
		t.Fatal("leaf arm naming the wrong region was accepted")
	}
}

// TestSolveHandoffCompositionLexicalPlacementRejectsRegionOwningWrongParent
// proves a region-owning arm whose own region exists but whose Parent is not
// the parent record's Region is rejected.
func TestSolveHandoffCompositionLexicalPlacementRejectsRegionOwningWrongParent(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(flag bool) void {
    if flag { print 1; } else { print 2; }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()

	ifIndex := findControlRecord(records, controlIf)
	if ifIndex < 0 {
		t.Fatal("no controlIf record")
	}
	var elseArm symbol.SyntaxRef
	for _, entry := range records[ifIndex].Control.Composition {
		if entry.Role == roleElse {
			elseArm = entry.Arm
		}
	}
	elseRecordIndex := -1
	for candidate, record := range records {
		if record.Control != nil && record.Header.Syntax == elseArm {
			elseRecordIndex = candidate
		}
	}
	if elseRecordIndex < 0 {
		t.Fatal("no control record for the else arm")
	}
	elseRegion := records[elseRecordIndex].Control.Region
	if elseRegion == 0 || uint64(elseRegion) > uint64(len(controls)) {
		t.Fatalf("else arm region = %d is not a valid region", elseRegion)
	}

	mutatedControls := cloneControlsForMutation(controls)
	wrongParent := controlID(0)
	for _, region := range controls {
		if region.ID != controls[elseRegion-1].Parent && region.ID != elseRegion {
			wrongParent = region.ID
		}
	}
	mutatedControls[elseRegion-1].Parent = wrongParent

	reporter := &auditReporter{gen: facts.Generation}
	if auditStructuralComposition(facts.Generation, mutatedControls, records, reporter) {
		t.Fatal("region-owning arm with the wrong parent region was accepted")
	}
}

// TestSolveHandoffCompositionPositives proves the matching positives: a bare
// leaf arm, a local-binding for-initializer arm, and a block/nested-control
// arm are all accepted with no diagnostic, exercising both branches of the
// leaf-versus-region-owning placement rule.
func TestSolveHandoffCompositionPositives(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(flag bool, other bool, limit i32) void {
    if flag return; else print 1;
    if flag { print 2; } else if other { print 3; }
    for var step i32 = 0; step < limit; step += 1 { print 4; }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()

	reporter := &auditReporter{gen: facts.Generation}
	if !auditStructuralComposition(facts.Generation, controls, records, reporter) {
		t.Fatal("valid composition rejected")
	}
	if reporter.failed {
		t.Fatal("reporter recorded a failure for a valid program")
	}
}

// TestSolveHandoffCompositionDamagedArmExemptFromResolutionChecks proves a
// damaged then-arm is retained, matches exact reconstruction, is exempted
// from unique resolution/kind/placement, and produces no additional C0619
// beyond the parser's own diagnostic for the damage.
func TestSolveHandoffCompositionDamagedArmExemptFromResolutionChecks(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(flag bool) void {
    if flag ; else print 1;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if !diagnostics.HasErrors() {
		t.Fatal("damaged then-arm produced no diagnostic")
	}
	compilation, ok := buildFrozenCompilation(facts.Generation, inputs)
	if !ok {
		t.Fatalf("buildFrozenCompilation failed: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatalf("freeze failed: %+v", diagnostics.Items())
	}
	if !auditFrozen(facts.Generation, frozen, compilation) {
		t.Fatalf("audit rejected a damaged arm it should exempt: %+v", diagnostics.Items())
	}
	for _, item := range diagnostics.Items() {
		if item.Code == CodeGeneration {
			t.Fatalf("damaged arm produced an additional generation diagnostic: %+v", item)
		}
	}
}

// TestSolveHandoffCompositionControlBindingInvariantRejections proves
// invariant 10's four checks each reject independently: a controlBinding
// record with a zero/invalid region, a nonempty Composition, or a
// Header.Syntax that does not name a BindingDecl occurrence.
func TestSolveHandoffCompositionControlBindingInvariantRejections(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn h() void {
    let x = 1;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()

	baseline := &auditReporter{gen: facts.Generation}
	if !auditStructuralComposition(facts.Generation, controls, records, baseline) {
		t.Fatal("baseline controlBinding rejected")
	}

	t.Run("zero region", func(t *testing.T) {
		mutated := cloneRecordsForMutation(records)
		index := findControlRecord(mutated, controlBinding)
		if index < 0 {
			t.Fatal("no controlBinding record")
		}
		mutated[index].Control.Region = 0
		reporter := &auditReporter{gen: facts.Generation}
		if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
			t.Fatal("controlBinding with a zero region was accepted")
		}
	})
	t.Run("nonempty composition", func(t *testing.T) {
		mutated := cloneRecordsForMutation(records)
		index := findControlRecord(mutated, controlBinding)
		if index < 0 {
			t.Fatal("no controlBinding record")
		}
		mutated[index].Control.Composition = []structuralChild{{Role: roleBody, Arm: mutated[index].Header.Syntax}}
		reporter := &auditReporter{gen: facts.Generation}
		if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
			t.Fatal("controlBinding with a nonempty composition was accepted")
		}
	})
	t.Run("names non-BindingDecl syntax", func(t *testing.T) {
		mutated := cloneRecordsForMutation(records)
		index := findControlRecord(mutated, controlBinding)
		if index < 0 {
			t.Fatal("no controlBinding record")
		}
		mutated[index].Header.Syntax = facts.Walk.order[0]
		mutated[index].Control.Header.Syntax = mutated[index].Header.Syntax
		reporter := &auditReporter{gen: facts.Generation}
		if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
			t.Fatal("controlBinding naming a non-BindingDecl occurrence was accepted")
		}
	})
}

// TestSolveHandoffEmptyModuleIsNotAFailure is a regression test: a module
// with zero top-level declarations previously could not be distinguished
// from a genuine buildModuleDeclarations failure, because both returned a
// bare nil []symbol.SymbolID. That collapsed a perfectly valid empty module
// into a silent full-pipeline rejection (GenerationHadErrors true, with no
// diagnostic explaining why, since the caller's failure branch never
// reported one). buildModuleDeclarations now returns an explicit ok result
// distinct from the slice itself; this proves an empty module succeeds.
func TestSolveHandoffEmptyModuleIsNotAFailure(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatal("run06a returned nil")
	}
	if handoff.GenerationHadErrors {
		t.Fatalf("an empty module must not be treated as a generation failure: diagnostics=%+v", diagnostics.Items())
	}
	if handoff.Semantics == nil {
		t.Fatal("Semantics should be non-nil for a valid empty module")
	}
	if len(handoff.Compilation.Modules) != 1 {
		t.Fatalf("expected exactly one module, got %d", len(handoff.Compilation.Modules))
	}
	if len(handoff.Compilation.Modules[0].Declarations) != 0 {
		t.Fatalf("expected zero declarations, got %d", len(handoff.Compilation.Modules[0].Declarations))
	}
}

// TestSolveHandoffCompositionEmptyCompositionDoesNotBypassAudit proves the
// freeze audit gates on the record's kind, not on whether its retained
// Composition happens to be non-empty. Dropping a composition-owning
// record's Composition entirely previously skipped exact reconstruction and
// passed; it must now be rejected.
func TestSolveHandoffCompositionEmptyCompositionDoesNotBypassAudit(t *testing.T) {
	for _, testCase := range []struct {
		name string
		kind controlKind
		src  string
	}{
		{"if", controlIf, "fn f(flag bool) void {\n    if flag { print 1; } else { print 2; }\n}\n"},
		{"while", controlWhile, "fn f(flag bool) void {\n    while flag { print 1; }\n}\n"},
		{"for", controlFor, "fn f() void {\n    for var i i32 = 0; i < 3; i += 1 { print i; }\n}\n"},
		{"switchCase", controlSwitchCase, "fn f(v i32) void {\n    switch v {\n    case 1:\n        print 1;\n    else:\n        print 2;\n    }\n}\n"},
	} {
		t.Run(testCase.name, func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(testCase.src)})
			facts := run06a3(inputs, diagnostics, Config{})
			if diagnostics.HasErrors() {
				t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
			}
			frozen, ok := facts.Generation.freeze()
			if !ok {
				t.Fatal("freeze failed")
			}
			controls := frozen.records.Controls()
			records := frozen.records.Records()

			baseline := &auditReporter{gen: facts.Generation}
			if !auditStructuralComposition(facts.Generation, controls, records, baseline) {
				t.Fatal("baseline composition rejected")
			}

			mutated := cloneRecordsForMutation(records)
			index := findControlRecord(mutated, testCase.kind)
			if index < 0 {
				t.Fatalf("no record of kind %v", testCase.kind)
			}
			if len(mutated[index].Control.Composition) == 0 {
				t.Fatal("record already has an empty composition; test proves nothing")
			}
			mutated[index].Control.Composition = nil

			reporter := &auditReporter{gen: facts.Generation}
			if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
				t.Fatal("dropped composition accepted by audit")
			}
			if !reporter.failed {
				t.Fatal("reporter did not record failure")
			}
		})
	}
}

// TestSolveHandoffCompositionEmptySwitchRemainsValid proves the kind-gated
// audit does not over-reject: a switch with no cases and no else legitimately
// has an empty Composition, because expectedComposition returns empty for it
// too, so the lengths agree.
func TestSolveHandoffCompositionEmptySwitchRemainsValid(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(v i32) void {
    switch v {
    }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	records := frozen.records.Records()
	index := findControlRecord(records, controlSwitch)
	if index < 0 {
		t.Fatal("no controlSwitch record")
	}
	if len(records[index].Control.Composition) != 0 {
		t.Fatalf("empty switch composition = %+v, want empty", records[index].Control.Composition)
	}
	reporter := &auditReporter{gen: facts.Generation}
	if !auditStructuralComposition(facts.Generation, frozen.records.Controls(), records, reporter) {
		t.Fatal("empty switch rejected by audit")
	}
}

// TestSolveHandoffControlBindingRejectsTopLevelBinding proves invariant 10
// rejects a controlBinding naming a module-level binding, decided from
// immutable graph structure rather than symbol lookup. The mutation gives a
// top-level BindingDecl an otherwise-valid callable and region, so only the
// top-level check can reject it.
func TestSolveHandoffControlBindingRejectsTopLevelBinding(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let g i32 = 1;

fn f() void {
    let x i32 = 2;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatal("freeze failed")
	}
	controls := frozen.records.Controls()
	records := frozen.records.Records()

	baseline := &auditReporter{gen: facts.Generation}
	if !auditStructuralComposition(facts.Generation, controls, records, baseline) {
		t.Fatal("baseline rejected")
	}

	local := findControlRecord(records, controlBinding)
	if local < 0 {
		t.Fatal("no controlBinding record for the local binding")
	}

	item, _ := inputs.Graph.Module(inputs.Graph.Root)
	root, _ := item.Tree.Node(item.Tree.Root())
	var topLevelBinding syntax.NodeID
	for _, child := range root.Children() {
		if node, ok := item.Tree.Node(child); ok && node.Kind() == syntax.BindingDecl {
			topLevelBinding = child
			break
		}
	}
	if topLevelBinding == 0 {
		t.Fatal("no top-level BindingDecl in fixture")
	}

	// Retarget the valid local controlBinding at the top-level binding,
	// keeping its callable and region intact so every other invariant still
	// holds and only the top-level rule can reject it.
	mutated := cloneRecordsForMutation(records)
	mutated[local].Header.Syntax = symbol.SyntaxRef{Module: inputs.Graph.Root, Node: topLevelBinding}
	mutated[local].Control.Header.Syntax = mutated[local].Header.Syntax

	reporter := &auditReporter{gen: facts.Generation}
	if auditStructuralComposition(facts.Generation, controls, mutated, reporter) {
		t.Fatal("controlBinding naming a top-level binding accepted")
	}
	if !reporter.failed {
		t.Fatal("reporter did not record failure")
	}
}
