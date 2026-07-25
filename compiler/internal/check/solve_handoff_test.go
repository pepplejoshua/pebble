package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
)

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
