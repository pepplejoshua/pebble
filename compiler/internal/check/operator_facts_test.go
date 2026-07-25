package check

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func TestOperatorFactsRepositoryFixtures(t *testing.T) {
	patterns := []struct {
		pattern    string
		wantErrors bool
	}{
		{"../../../tests/check/facts/valid/operator_*.peb", false},
		{"../../../tests/check/facts/valid/place_*.peb", false},
		{"../../../tests/check/facts/invalid/T0507/operator_*.peb", true},
		{"../../../tests/check/facts/recovery/place_*.peb", true},
	}
	for _, group := range patterns {
		paths, err := filepath.Glob(group.pattern)
		if err != nil || len(paths) == 0 {
			t.Fatalf("glob %s: %v", group.pattern, err)
		}
		for _, path := range paths {
			t.Run(filepath.Base(path), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
				facts := run06a3(inputs, diagnostics, Config{})
				solution := facts.Session.Solve()
				if group.wantErrors == !diagnostics.HasErrors() {
					t.Fatalf("errors=%v successful=%v diagnostics=%+v", diagnostics.HasErrors(), solution.Successful(), diagnostics.Items())
				}
			})
		}
	}
}

func TestOperatorFactsFatalLimitStopsCheckerPublication(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let value i32 = 1 + 2; let later i32 = 3;`)})
	facts := run06a3(inputs, diagnostics, Config{Inference: infer.Config{MaxConstraints: 1, MaxDiagnostics: 4}})
	if !facts.Session.Fatal() {
		t.Fatalf("session not fatal: %+v", diagnostics.Items())
	}
	for _, retained := range facts.Generation.records.values {
		if retained.Operator != nil {
			t.Fatal("operator record committed after fatal limit")
		}
	}
	for _, item := range diagnostics.Items() {
		if item.Code == CodeGeneration {
			t.Fatalf("cascading checker diagnostic: %+v", diagnostics.Items())
		}
	}
}

func TestOperatorFactsLoweredLimitsRemainBounded(t *testing.T) {
	contents := []byte(`
fn update(values [4]i32, pointer *i32) void {
 var local i32 = values[0] + *pointer;
 local += values[1];
 values[2] = local;
 let tail []i32 = values[1:3];
 local = local + values[3];
}
fn invoke[T](callee T) void { callee(); }
let delayed_result i32 = delayed();
let delayed_result_two i32 = delayed_two();
let delayed fn() i32 = fn() i32 => 1;
let delayed_two fn() i32 = fn() i32 => 2;
`)
	configs := []Config{
		{MaxSemanticRecords: 1, MaxDiagnostics: 1},
		{MaxRecordComponents: 2, MaxDiagnostics: 1},
		{MaxTrackedPlaces: 1, MaxDiagnostics: 1},
		{Inference: infer.Config{MaxConstraints: 1, MaxDiagnostics: 1}, MaxDiagnostics: 1},
		{Inference: infer.Config{MaxShapeComponents: 1, MaxDiagnostics: 1}, MaxDiagnostics: 1},
		{Inference: infer.Config{MaxConstraintRequeues: 1, MaxTotalRequeues: 1, MaxDiagnostics: 1}, MaxDiagnostics: 1},
		{Inference: infer.Config{MaxDecompositionSteps: 1, MaxDiagnostics: 1}, MaxDiagnostics: 1},
		{Inference: infer.Config{MaxSolvedSlots: 1, MaxDiagnostics: 1}, MaxDiagnostics: 1},
	}
	for i, config := range configs {
		t.Run(string(rune('a'+i)), func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
			facts := run06a3(inputs, diagnostics, config)
			facts.Session.Solve()
			if !diagnostics.HasErrors() {
				t.Fatal("lowered limit produced no diagnostic")
			}
			if uint32(len(facts.Generation.records.values)) > facts.Generation.config.MaxSemanticRecords ||
				facts.Generation.records.components > uint64(facts.Generation.config.MaxRecordComponents) ||
				facts.Generation.counters.trackedPlaces > facts.Generation.config.MaxTrackedPlaces {
				t.Fatalf("limit exceeded records/components/places=%d/%d/%d", len(facts.Generation.records.values), facts.Generation.records.components, facts.Generation.counters.trackedPlaces)
			}
			seenEOF := false
			for _, ref := range facts.Walk.order {
				node, _ := facts.Walk.node(ref.Module, ref.Node)
				seenEOF = seenEOF || node.Kind() == syntax.EndOfFile
			}
			if !seenEOF {
				t.Fatal("limit recovery stopped authored traversal")
			}
		})
	}
}

func TestOperatorFactsExactFamiliesAndSolve(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let a i32 = 3;
let b i32 = 2;
let neg i32 = -1;
let min i8 = -128;
let wide i64 = -9223372036854775808;
let repeated i8 = - - -128;
let bit i32 = ~a;
let logical bool = !false;
let add i32 = 1 + 2;
let sub i32 = a - b;
let mul i32 = a * b;
let div i32 = a / b;
let rem i32 = a % b;
let shift i32 = a << 1;
let bits i32 = a & b | a ^ b;
let both bool = true && false || true;
let order bool = a < b;
let equal bool = a == b;
let text str = "a" + "b";
let ptr *i32 = &a;
let deref i32 = *ptr;
let optional ?i32 = some 1;
let forced i32 = optional!;
let cast i64 = a as i64;
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	families := map[operatorFamily]int{}
	forms := map[operatorForm]int{}
	casts, joined := 0, 0
	for _, retained := range facts.Generation.records.values {
		if retained.Operator != nil {
			families[retained.Operator.Family]++
			forms[retained.Operator.Form]++
			if len(retained.Operator.Operands) == 2 && retained.Operator.Token == syntax.Plus && retained.Operator.Operands[0] == retained.Operator.Operands[1] {
				t.Fatal("binary operand order collapsed")
			}
		}
		if retained.Cast != nil {
			casts++
		}
		if retained.Expression != nil && retained.Expression.Specialized != 0 {
			joined++
		}
	}
	for _, family := range []operatorFamily{operatorLiteralNegate, operatorNumericSame, operatorAdd, operatorIntegralSame, operatorShift, operatorBoolean, operatorOrdering, operatorEquality, operatorAddress, operatorDereference, operatorOptionalForce} {
		if families[family] == 0 {
			t.Fatalf("missing family %d: %+v diagnostics=%+v", family, families, diagnostics.Items())
		}
	}
	if casts != 1 || forms[operatorPrefix] == 0 || forms[operatorPostfix] == 0 || forms[operatorBinary] == 0 || joined == 0 {
		t.Fatalf("casts/forms/joins=%d/%+v/%d", casts, forms, joined)
	}
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestOperatorFactsStringAddAndMixedNumeric(t *testing.T) {
	valid, validDiagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let value str = "a" + "b";`)})
	validFacts := run06a3(valid, validDiagnostics, Config{})
	if solution := validFacts.Session.Solve(); !solution.Successful() || validDiagnostics.HasErrors() {
		t.Fatalf("string add: %+v", validDiagnostics.Items())
	}

	invalid, invalidDiagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let left i32 = 1; let right i64 = 2; let value = left + right;`)})
	invalidFacts := run06a3(invalid, invalidDiagnostics, Config{})
	if solution := invalidFacts.Session.Solve(); solution.Successful() || !invalidDiagnostics.HasErrors() {
		t.Fatalf("mixed numeric accepted: %+v", invalidDiagnostics.Items())
	}
}

func TestOperatorFactsRepeatedNegationPreservesExactValue(t *testing.T) {
	valid, validDiagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let value i8 = - - -128;`)})
	validFacts := run06a3(valid, validDiagnostics, Config{})
	if solution := validFacts.Session.Solve(); !solution.Successful() || validDiagnostics.HasErrors() {
		t.Fatalf("triple negation: %+v", validDiagnostics.Items())
	}
	invalid, invalidDiagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let value i8 = - -128;`)})
	invalidFacts := run06a3(invalid, invalidDiagnostics, Config{})
	if solution := invalidFacts.Session.Solve(); solution.Successful() || !invalidDiagnostics.HasErrors() {
		t.Fatalf("double negation accepted: %+v", invalidDiagnostics.Items())
	}
}

func TestOperatorFactsRigidRequirements(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn use[T, U](left T, right U, a i32, b i32) bool { let sum T = left + left; let nested T = (left + left) + (left + left); let converted i32 = (left + left) as i32; let shifted T = left << right; var update T = left; update += left; let concrete = a == b; return left == left; }`)})
	facts := run06a3(inputs, diagnostics, Config{})
	kinds := map[requirementKind]int{}
	for _, retained := range facts.Generation.records.values {
		if retained.Requirement != nil {
			kinds[retained.Requirement.Kind]++
		}
	}
	if kinds[requirementNumeric] < 10 || kinds[requirementIntegral] != 2 || kinds[requirementEquatable] != 2 || kinds[requirementUnsupportedConversion] != 1 {
		t.Fatalf("requirements=%+v diagnostics=%+v", kinds, diagnostics.Items())
	}
}

func TestAssignmentFactsProjectsKnownPlaceDestination(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn update() void { var value i64 = 0; value = 1 + 2; }`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	found := false
	for _, retained := range facts.Generation.records.values {
		if retained.Operator == nil || retained.Operator.Token != syntax.Plus {
			continue
		}
		resolved, ok := solution.SyntaxType(retained.Header.Syntax)
		if !ok || resolved.State != infer.TypeFinal || resolved.Type != inputs.Types.Builtins().I64 {
			t.Fatalf("assignment arithmetic type=%+v present=%v", resolved, ok)
		}
		found = true
	}
	if !found || !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("found=%v diagnostics=%+v", found, diagnostics.Items())
	}
}

func TestOperatorFactsGenericCastRetainsUnsupportedRequirements(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn convert[T, U](value T) U => value as U;`)})
	facts := run06a3(inputs, diagnostics, Config{})
	count := 0
	for _, retained := range facts.Generation.records.values {
		if retained.Requirement != nil && retained.Requirement.Kind == requirementUnsupportedConversion {
			count++
		}
	}
	if count != 2 || diagnostics.HasErrors() {
		t.Fatalf("unsupported conversions=%d diagnostics=%+v", count, diagnostics.Items())
	}
}

func TestOperatorFactsClosedRecordsRejectMalformedAtomically(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn update(value i32) void { var local i32 = value + 1; local += 1; }`)})
	facts := run06a3(inputs, diagnostics, Config{})
	reject := func(name string, source retainedRecord, mutate func(*retainedRecord)) {
		t.Helper()
		candidate := cloneRetainedRecord(source)
		header := candidate.Header
		header.ID = 0
		candidate.assignHeader(header)
		mutate(&candidate)
		before, components := len(facts.Generation.records.values), facts.Generation.records.components
		if id, ok := facts.Generation.addRecord(candidate); ok || id != 0 || len(facts.Generation.records.values) != before || facts.Generation.records.components != components {
			t.Fatalf("%s accepted or mutated arena", name)
		}
	}
	foundOperator, foundAssignment, foundPlace := false, false, false
	for _, record := range append([]retainedRecord(nil), facts.Generation.records.values...) {
		if record.Operator != nil && !foundOperator {
			foundOperator = true
			reject("operator form", record, func(v *retainedRecord) { v.Operator.Form = operatorPrefix })
		}
		if record.Assignment != nil && !foundAssignment {
			foundAssignment = true
			reject("assignment token", record, func(v *retainedRecord) { v.Assignment.Operator = syntax.LogicalAnd })
		}
		if record.Place != nil && !foundPlace {
			foundPlace = true
			reject("place projection", record, func(v *retainedRecord) { v.Place.Projections[0].Index = v.Place.Projections[0].Base + 1 })
			reject("place mutability", record, func(v *retainedRecord) { v.Place.RootMutable = !v.Place.RootMutable })
		}
	}
	if !foundOperator || !foundAssignment || !foundPlace {
		t.Fatalf("coverage=%v/%v/%v", foundOperator, foundAssignment, foundPlace)
	}
}
