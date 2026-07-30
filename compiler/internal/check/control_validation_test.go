package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

func hasControlDiagnostic(diagnostics *diagnostic.DiagnosticSet, code diagnostic.Code) bool {
	for _, d := range diagnostics.Items() {
		if d.Code == code {
			return true
		}
	}
	return false
}

// TestAuditControlArenaValidDeeplyNestedReal verifies that a real, deeply
// nested function with if/while/for/range-loop/switch passes the arena audit
// with no C0619.
func TestAuditControlArenaValidDeeplyNestedReal(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn deep(flag bool, limit i32) void {
    if flag {
        while flag {
            for var i i32 = 0; i < limit; i += 1 {
                if flag {
                    break;
                }
                loop 0..limit {
                    continue;
                }
                switch limit {
                case 1: print 1;
                case 2: print 2;
                else: print 0;
                }
            }
        }
    }
}
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff.GenerationHadErrors {
		t.Fatalf("generation had errors: %+v", diagnostics.Items())
	}
	diagnostics = diagnostic.NewDiagnosticSet()
	if !auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatalf("valid deeply nested function rejected: %+v", diagnostics.Items())
	}
	if hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatalf("unexpected C0619 on valid arena: %+v", diagnostics.Items())
	}
}

// TestAuditControlArenaRejectsNonContiguousIDs verifies that a
// directly-constructed arena with non-contiguous region IDs is rejected.
func TestAuditControlArenaRejectsNonContiguousIDs(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{3}},
		{ID: 3, Parent: 1, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("non-contiguous IDs accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for non-contiguous IDs")
	}
}

// TestAuditControlArenaRejectsRootDepthNotOne verifies that a root region
// with Depth != 1 is rejected.
func TestAuditControlArenaRejectsRootDepthNotOne(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("root depth != 1 accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for root depth != 1")
	}
}

// TestAuditControlArenaRejectsDepthMismatch verifies that a non-root region
// whose Depth doesn't equal parent.Depth + 1 is rejected.
func TestAuditControlArenaRejectsDepthMismatch(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2}},
		{ID: 2, Parent: 1, Depth: 5},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("depth mismatch accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for depth mismatch")
	}
}

// TestAuditControlArenaRejectsMalformedIfComposition verifies that a
// controlIf with roleElse before roleThen or two roleThen entries is rejected.
func TestAuditControlArenaRejectsMalformedIfComposition(t *testing.T) {
	cases := []struct {
		name        string
		composition []structuralChild
		elsePresent bool
	}{
		{
			name:        "roleElse before roleThen",
			composition: []structuralChild{{Role: roleElse, Arm: symbol.SyntaxRef{Module: 1, Node: 10}}, {Role: roleThen, Arm: symbol.SyntaxRef{Module: 1, Node: 11}}},
			elsePresent: true,
		},
		{
			name:        "two roleThen",
			composition: []structuralChild{{Role: roleThen, Arm: symbol.SyntaxRef{Module: 1, Node: 10}}, {Role: roleThen, Arm: symbol.SyntaxRef{Module: 1, Node: 11}}},
			elsePresent: false,
		},
		{
			name:        "roleThen then roleElse but ElsePresent false",
			composition: []structuralChild{{Role: roleThen, Arm: symbol.SyntaxRef{Module: 1, Node: 10}}, {Role: roleElse, Arm: symbol.SyntaxRef{Module: 1, Node: 11}}},
			elsePresent: false,
		},
		{
			name:        "single roleThen but ElsePresent true",
			composition: []structuralChild{{Role: roleThen, Arm: symbol.SyntaxRef{Module: 1, Node: 10}}},
			elsePresent: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			controls := []controlRegion{
				{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2, 3}},
				{ID: 2, Parent: 1, Depth: 2},
				{ID: 3, Parent: 1, Depth: 2},
			}
			records := []retainedRecord{
				{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
				{Control: &controlRecord{Kind: controlIf, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: tc.composition, ElsePresent: tc.elsePresent}},
			}
			handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
			diagnostics := diagnostic.NewDiagnosticSet()
			if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
				t.Fatalf("malformed if composition accepted: %s", tc.name)
			}
			if !hasControlDiagnostic(diagnostics, CodeGeneration) {
				t.Fatalf("expected C0619 for malformed if composition: %s", tc.name)
			}
		})
	}
}

// TestAuditControlArenaRejectsNonContiguousSwitchOrdinals verifies that a
// controlSwitch with non-contiguous roleCase ordinals is rejected.
func TestAuditControlArenaRejectsNonContiguousSwitchOrdinals(t *testing.T) {
	cases := []struct {
		name        string
		composition []structuralChild
		elsePresent bool
	}{
		{
			name: "gap in ordinals",
			composition: []structuralChild{
				{Role: roleCase, Ordinal: 0, Arm: symbol.SyntaxRef{Module: 1, Node: 10}},
				{Role: roleCase, Ordinal: 2, Arm: symbol.SyntaxRef{Module: 1, Node: 11}},
			},
		},
		{
			name: "out of order ordinals",
			composition: []structuralChild{
				{Role: roleCase, Ordinal: 1, Arm: symbol.SyntaxRef{Module: 1, Node: 10}},
				{Role: roleCase, Ordinal: 0, Arm: symbol.SyntaxRef{Module: 1, Node: 11}},
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			controls := []controlRegion{
				{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2, 3}},
				{ID: 2, Parent: 1, Depth: 2},
				{ID: 3, Parent: 1, Depth: 2},
			}
			records := []retainedRecord{
				{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
				{Control: &controlRecord{Kind: controlSwitch, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: tc.composition, ElsePresent: tc.elsePresent}},
			}
			handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
			diagnostics := diagnostic.NewDiagnosticSet()
			if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
				t.Fatalf("non-contiguous switch ordinals accepted: %s", tc.name)
			}
			if !hasControlDiagnostic(diagnostics, CodeGeneration) {
				t.Fatalf("expected C0619 for non-contiguous switch ordinals: %s", tc.name)
			}
		})
	}
}

// TestAuditControlArenaRejectsOutOfRangeRegion verifies that a record whose
// Region names an out-of-range controlID is rejected.
func TestAuditControlArenaRejectsOutOfRangeRegion(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlBlock, Region: 99, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("out-of-range region accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for out-of-range region")
	}
}

// TestAuditControlArenaRejectsOutOfRangeTarget verifies that a break/continue
// record whose Target names an out-of-range controlID is rejected.
func TestAuditControlArenaRejectsOutOfRangeTarget(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlBreak, Region: 1, Target: 99, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("out-of-range target accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for out-of-range target")
	}
}

// TestAuditControlArenaRejectsInconsistentCallable verifies that two
// controlRecords within the same function tree carrying different Callable
// values are rejected.
func TestAuditControlArenaRejectsInconsistentCallable(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2}},
		{ID: 2, Parent: 1, Depth: 2},
	}
	callableA := callableRef{Symbol: 10, Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}
	callableB := callableRef{Symbol: 20, Syntax: symbol.SyntaxRef{Module: 1, Node: 2}}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableA}},
		{Control: &controlRecord{Kind: controlBlock, Region: 2, Callable: callableB}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("inconsistent callable accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for inconsistent callable")
	}
}

// TestAuditControlArenaSuppressesOnGenerationErrors verifies that a handoff
// with GenerationHadErrors == true and a damaged arena does NOT additionally
// report C0619.
func TestAuditControlArenaSuppressesOnGenerationErrors(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 99},
	}
	handoff := &solveHandoff{
		GenerationHadErrors: true,
		Records:             frozenRecords{controls: controls},
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	if !auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("GenerationHadErrors handoff should return true")
	}
	if hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatalf("unexpected C0619 on damaged handoff: %+v", diagnostics.Items())
	}
}

// TestAuditControlArenaRejectsEmptyCompositionForRegionOwner verifies that a
// region-owning control record (e.g. controlWhile) with an empty Composition
// is rejected.
func TestAuditControlArenaRejectsEmptyCompositionForRegionOwner(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2}},
		{ID: 2, Parent: 1, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlWhile, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("empty composition for controlWhile accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for empty composition")
	}
}

// TestAuditControlArenaRejectsNonEmptyCompositionForLeaf verifies that a
// leaf control record (e.g. controlPrint) with a nonempty Composition is
// rejected.
func TestAuditControlArenaRejectsNonEmptyCompositionForLeaf(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlPrint, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: []structuralChild{{Role: roleBody, Arm: symbol.SyntaxRef{Module: 1, Node: 10}}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("nonempty composition for controlPrint accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for nonempty composition for leaf")
	}
}

// TestAuditControlArenaRejectsArmResolutionFailure verifies that a
// structuralChild.Arm that doesn't match exactly one other control record's
// Header.Syntax is rejected.
func TestAuditControlArenaRejectsArmResolutionFailure(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2}},
		{ID: 2, Parent: 1, Depth: 2},
	}
	arm := symbol.SyntaxRef{Module: 1, Node: 999}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlWhile, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: []structuralChild{{Role: roleBody, Arm: arm}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("unresolved arm accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for unresolved arm")
	}
}

// TestAuditControlArenaRejectsLeafArmWrongRegion verifies that a leaf arm
// whose Region differs from its parent's Region is rejected.
func TestAuditControlArenaRejectsLeafArmWrongRegion(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2, 3}},
		{ID: 2, Parent: 1, Depth: 2},
		{ID: 3, Parent: 1, Depth: 2},
	}
	armSyntax := symbol.SyntaxRef{Module: 1, Node: 10}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlWhile, Region: 2, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: []structuralChild{{Role: roleBody, Arm: armSyntax}}}},
		{Control: &controlRecord{Kind: controlPrint, Header: recordHeader{Syntax: armSyntax}, Region: 3, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("leaf arm with wrong region accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for leaf arm with wrong region")
	}
}

// TestAuditControlArenaRejectsRegionOwnerArmWrongParent verifies that a
// region-owning arm whose Region's Parent doesn't match the parent record's
// Region is rejected.
func TestAuditControlArenaRejectsRegionOwnerArmWrongParent(t *testing.T) {
	armSyntax := symbol.SyntaxRef{Module: 1, Node: 10}
	controls2 := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2}},
		{ID: 2, Parent: 1, Depth: 2, Children: []controlID{3}},
		{ID: 3, Parent: 1, Depth: 2},
	}
	records2 := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlIf, Region: 2, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: []structuralChild{{Role: roleThen, Arm: armSyntax}}}},
		// controlBlock arm is in region 3, whose parent is 1, but the parent
		// record's Region is 2. So this should fail.
		{Control: &controlRecord{Kind: controlBlock, Header: recordHeader{Syntax: armSyntax}, Region: 3, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls2, values: records2}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("region-owning arm with wrong parent accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for region-owning arm with wrong parent")
	}
}

// TestAuditControlArenaRejectsMissingFunctionRecord verifies that a root
// region without a controlFunction record is rejected.
func TestAuditControlArenaRejectsMissingFunctionRecord(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlBlock, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("missing function record accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for missing function record")
	}
}

// TestAuditControlArenaRejectsMultipleFunctionRecords verifies that two
// controlFunction records for one root region are rejected.
func TestAuditControlArenaRejectsMultipleFunctionRecords(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 2}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("multiple function records accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for multiple function records")
	}
}

// TestAuditControlArenaRejectsDuplicateCaseOrdinals verifies that a
// controlSwitch with duplicate roleCase ordinals is rejected.
func TestAuditControlArenaRejectsDuplicateCaseOrdinals(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2, 3}},
		{ID: 2, Parent: 1, Depth: 2},
		{ID: 3, Parent: 1, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlSwitch, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: []structuralChild{
			{Role: roleCase, Ordinal: 0, Arm: symbol.SyntaxRef{Module: 1, Node: 10}},
			{Role: roleCase, Ordinal: 0, Arm: symbol.SyntaxRef{Module: 1, Node: 11}},
		}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("duplicate case ordinals accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for duplicate case ordinals")
	}
}

// TestAuditControlArenaRejectsForBodyBeforeInitializer verifies that a
// controlFor with roleBody before roleInitializer is rejected.
func TestAuditControlArenaRejectsForBodyBeforeInitializer(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2}},
		{ID: 2, Parent: 1, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlFor, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: []structuralChild{
			{Role: roleBody, Arm: symbol.SyntaxRef{Module: 1, Node: 10}},
			{Role: roleInitializer, Arm: symbol.SyntaxRef{Module: 1, Node: 11}},
		}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("for body before initializer accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for for body before initializer")
	}
}

// TestAuditControlArenaRejectsEdgeCountMismatch verifies that an arena
// whose edge count doesn't equal regions minus roots is rejected.
func TestAuditControlArenaRejectsEdgeCountMismatch(t *testing.T) {
	// Root has 0 Children but there are 2 non-root regions with Parent=1.
	// The first check (childCount vs Children length) only checks non-roots,
	// so it passes. But the edge count check counts total Children lengths
	// (0) which doesn't equal regions(3) - roots(1) = 2.
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{}},
		{ID: 2, Parent: 1, Depth: 2},
		{ID: 3, Parent: 1, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("edge count mismatch accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for edge count mismatch")
	}
}

// TestAuditControlArenaRejectsOutOfRangeDeferRegion verifies that a
// deferRecord whose Region names an out-of-range controlID is rejected.
func TestAuditControlArenaRejectsOutOfRangeDeferRegion(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Defer: &deferRecord{Region: 99, Statement: symbol.SyntaxRef{Module: 1, Node: 10}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("out-of-range defer region accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for out-of-range defer region")
	}
}

// TestAuditControlArenaValidForWithAllClauses verifies a controlFor with
// initializer, update, and body passes composition validation.
func TestAuditControlArenaValidForWithAllClauses(t *testing.T) {
	initArm := symbol.SyntaxRef{Module: 1, Node: 10}
	updateArm := symbol.SyntaxRef{Module: 1, Node: 11}
	bodyArm := symbol.SyntaxRef{Module: 1, Node: 12}
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2}},
		{ID: 2, Parent: 1, Depth: 2, Children: []controlID{3}},
		{ID: 3, Parent: 2, Depth: 3},
	}
	callable := callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}
	fnHeader := recordHeader{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}
	records := []retainedRecord{
		{Header: fnHeader, Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callable}},
		{Header: fnHeader, Control: &controlRecord{Kind: controlFor, Region: 2, Callable: callable, Composition: []structuralChild{
			{Role: roleInitializer, Arm: initArm},
			{Role: roleUpdate, Arm: updateArm},
			{Role: roleBody, Arm: bodyArm},
		}}},
		// Leaf arms (initializer, update) are in the for's own region (2).
		{Header: recordHeader{Syntax: initArm}, Control: &controlRecord{Kind: controlBinding, Region: 2, Callable: callable}},
		{Header: recordHeader{Syntax: updateArm}, Control: &controlRecord{Kind: controlExpression, Region: 2, Callable: callable}},
		// Region-owning arm (body block) is in a child of the for's region (3, parent 2).
		{Header: recordHeader{Syntax: bodyArm}, Control: &controlRecord{Kind: controlBlock, Region: 3, Callable: callable}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if !auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatalf("valid for with all clauses rejected: %+v", diagnostics.Items())
	}
}

// TestAuditControlArenaValidSwitchWithElse verifies a controlSwitch with
// cases and else passes composition validation.
func TestAuditControlArenaValidSwitchWithElse(t *testing.T) {
	case0Arm := symbol.SyntaxRef{Module: 1, Node: 10}
	case1Arm := symbol.SyntaxRef{Module: 1, Node: 11}
	elseArm := symbol.SyntaxRef{Module: 1, Node: 12}
	case0Body := symbol.SyntaxRef{Module: 1, Node: 20}
	case1Body := symbol.SyntaxRef{Module: 1, Node: 21}
	elseBody := symbol.SyntaxRef{Module: 1, Node: 22}
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2, 3, 4}},
		{ID: 2, Parent: 1, Depth: 2},
		{ID: 3, Parent: 1, Depth: 2},
		{ID: 4, Parent: 1, Depth: 2},
	}
	callable := callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}
	fnHeader := recordHeader{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}
	records := []retainedRecord{
		{Header: fnHeader, Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callable}},
		{Header: fnHeader, Control: &controlRecord{Kind: controlSwitch, Region: 1, Callable: callable, ElsePresent: true, Composition: []structuralChild{
			{Role: roleCase, Ordinal: 0, Arm: case0Arm},
			{Role: roleCase, Ordinal: 1, Arm: case1Arm},
			{Role: roleElse, Arm: elseArm},
		}}},
		// Switch case arms are region-owning. Their Region's Parent must be
		// the switch's Region (1).
		{Header: recordHeader{Syntax: case0Arm}, Control: &controlRecord{Kind: controlSwitchCase, Region: 2, Callable: callable, Composition: []structuralChild{
			{Role: roleBody, Arm: case0Body},
		}}},
		{Header: recordHeader{Syntax: case1Arm}, Control: &controlRecord{Kind: controlSwitchCase, Region: 3, Callable: callable, Composition: []structuralChild{
			{Role: roleBody, Arm: case1Body},
		}}},
		{Header: recordHeader{Syntax: elseArm}, Control: &controlRecord{Kind: controlSwitchCase, Region: 4, Callable: callable, Composition: []structuralChild{
			{Role: roleBody, Arm: elseBody},
		}}},
		// Leaf arms (print statements) are in their parent case's region.
		{Header: recordHeader{Syntax: case0Body}, Control: &controlRecord{Kind: controlPrint, Region: 2, Callable: callable}},
		{Header: recordHeader{Syntax: case1Body}, Control: &controlRecord{Kind: controlPrint, Region: 3, Callable: callable}},
		{Header: recordHeader{Syntax: elseBody}, Control: &controlRecord{Kind: controlPrint, Region: 4, Callable: callable}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if !auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatalf("valid switch with else rejected: %+v", diagnostics.Items())
	}
}

// TestAuditControlArenaRejectsOrphanRegion verifies that a region not in
// any parent's Children list is detected via edge count mismatch.
func TestAuditControlArenaRejectsOrphanRegion(t *testing.T) {
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{}},
		{ID: 2, Parent: 1, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("orphan region accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for orphan region")
	}
}

// TestAuditControlArenaRejectsDuplicateArmSyntax verifies that two arms
// sharing the same SyntaxRef are rejected (arm resolution requires exactly
// one match).
func TestAuditControlArenaRejectsDuplicateArmSyntax(t *testing.T) {
	arm := symbol.SyntaxRef{Module: 1, Node: 10}
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2, 3}},
		{ID: 2, Parent: 1, Depth: 2},
		{ID: 3, Parent: 1, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlIf, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: []structuralChild{
			{Role: roleThen, Arm: arm},
			{Role: roleElse, Arm: arm},
		}}},
		{Control: &controlRecord{Kind: controlBlock, Header: recordHeader{Syntax: arm}, Region: 2, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("duplicate arm syntax accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for duplicate arm syntax")
	}
}

// TestAuditControlArenaRejectsDuplicateChildID verifies that a corrupted arena
// where a parent's Children list contains a duplicate entry (and consequently
// misses a real sibling) is rejected, even though the Children length matches
// the derived child count.
func TestAuditControlArenaRejectsDuplicateChildID(t *testing.T) {
	// Region 1 (root) has Children: [2, 2] — region 2 listed twice, region 3
	// missing. Regions 2 and 3 both correctly declare Parent: 1, Depth: 2.
	// The old count-only check would pass (2 expected, 2 listed).
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2, 2}},
		{ID: 2, Parent: 1, Depth: 2},
		{ID: 3, Parent: 1, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("duplicate child ID accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for duplicate child ID")
	}
}

// TestAuditControlArenaRejectsChildNotPointingToParent verifies that a
// Children entry whose declared Parent doesn't match the owning region is
// rejected.
func TestAuditControlArenaRejectsChildNotPointingToParent(t *testing.T) {
	// Region 1 lists child 2, but region 2 declares Parent: 3 (not 1).
	controls := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2}},
		{ID: 2, Parent: 3, Depth: 2},
		{ID: 3, Parent: 1, Depth: 2},
	}
	records := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls, values: records}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("child not pointing to parent accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for child not pointing to parent")
	}
}

// TestAuditControlArenaRejectsRegionOwnerInChildRegion verifies that a
// region-owning arm whose Region is not a child of the parent record's
// Region is rejected.
func TestAuditControlArenaRejectsRegionOwnerInChildRegion(t *testing.T) {
	armSyntax := symbol.SyntaxRef{Module: 1, Node: 10}
	controls2 := []controlRegion{
		{ID: 1, Parent: 0, Depth: 1, Children: []controlID{2, 3}},
		{ID: 2, Parent: 1, Depth: 2},
		{ID: 3, Parent: 1, Depth: 2},
	}
	records2 := []retainedRecord{
		{Control: &controlRecord{Kind: controlFunction, Region: 1, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
		{Control: &controlRecord{Kind: controlIf, Region: 2, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}, Composition: []structuralChild{
			{Role: roleThen, Arm: armSyntax},
		}}},
		// controlBlock arm is in region 3, whose parent is 1, not 2 (the parent
		// record's Region).
		{Control: &controlRecord{Kind: controlBlock, Header: recordHeader{Syntax: armSyntax}, Region: 3, Callable: callableRef{Syntax: symbol.SyntaxRef{Module: 1, Node: 1}}}},
	}
	handoff := &solveHandoff{Records: frozenRecords{controls: controls2, values: records2}}
	diagnostics := diagnostic.NewDiagnosticSet()
	if auditControlArena(handoff, diagnostics, normalizeConfig(Config{})) {
		t.Fatal("region-owning arm in wrong child region accepted")
	}
	if !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatal("expected C0619 for region-owning arm in wrong child region")
	}
}

func validateControlFixture(t *testing.T, source string) (*diagnostic.DiagnosticSet, bool) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	if handoff.GenerationHadErrors {
		t.Fatalf("06a reported errors: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, validateControlFlow(handoff, records, diagnostics, Config{})
}

func TestValidateControlFlowMissingReturnAndImplicitVoidReturn(t *testing.T) {
	diagnostics, valid := validateControlFixture(t, `
fn missing(flag bool) i32 {
    if flag { return 1; }
}
`)
	if valid || !hasControlDiagnostic(diagnostics, CodeMissingReturn) {
		t.Fatalf("missing non-void return was accepted: %+v", diagnostics.Items())
	}

	diagnostics, valid = validateControlFixture(t, `
fn fallsOff() void {
    print 1;
}
`)
	if !valid || hasControlDiagnostic(diagnostics, CodeMissingReturn) {
		t.Fatalf("void fallthrough was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateControlFlowUnreachableWarningIsNotDuplicated(t *testing.T) {
	diagnostics, valid := validateControlFixture(t, `
fn unreachable() void {
    return;
    print 1;
    print 2;
}
`)
	if !valid {
		t.Fatalf("warning-only flow validation failed: %+v", diagnostics.Items())
	}
	count := 0
	for _, item := range diagnostics.Items() {
		if item.Code == CodeUnreachable {
			count++
			if item.Severity != diagnostic.Warning {
				t.Fatalf("unreachable diagnostic is not a warning: %+v", item)
			}
		}
	}
	if count != 1 {
		t.Fatalf("got %d unreachable warnings, want one: %+v", count, diagnostics.Items())
	}
}

// TestValidateControlFlowExhaustiveEnumSwitchNoElse verifies that an
// exhaustive enum switch without else, where every case returns, is accepted
// (no false-positive C0607).
func TestValidateControlFlowExhaustiveEnumSwitchNoElse(t *testing.T) {
	diagnostics, valid := validateControlFixture(t, `
type Color = enum { red, blue };
fn classify(color Color) i32 {
    switch color {
    case Color.red: return 1;
    case Color.blue: return 2;
    }
}
`)
	if !valid || hasControlDiagnostic(diagnostics, CodeMissingReturn) {
		t.Fatalf("exhaustive enum switch without else was rejected: %+v", diagnostics.Items())
	}
}

// TestValidateControlFlowNonExhaustiveEnumSwitchNoElse verifies that a
// non-exhaustive enum switch without else (missing a variant) is still
// correctly rejected with C0607.
func TestValidateControlFlowNonExhaustiveEnumSwitchNoElse(t *testing.T) {
	diagnostics, valid := validateControlFixture(t, `
type Color = enum { red, blue, green };
fn classify(color Color) i32 {
    switch color {
    case Color.red: return 1;
    case Color.blue: return 2;
    }
}
`)
	if valid || !hasControlDiagnostic(diagnostics, CodeMissingReturn) {
		t.Fatalf("non-exhaustive enum switch without else was accepted: %+v", diagnostics.Items())
	}
}

// TestValidateControlFlowExhaustiveBoolSwitchNoElse verifies that a bool
// switch covering both true and false without else, where every case
// returns, is accepted (no false-positive C0607).
func TestValidateControlFlowExhaustiveBoolSwitchNoElse(t *testing.T) {
	diagnostics, valid := validateControlFixture(t, `
fn classify(flag bool) i32 {
    switch flag {
    case true: return 1;
    case false: return 0;
    }
}
`)
	if !valid || hasControlDiagnostic(diagnostics, CodeMissingReturn) {
		t.Fatalf("exhaustive bool switch without else was rejected: %+v", diagnostics.Items())
	}
}
