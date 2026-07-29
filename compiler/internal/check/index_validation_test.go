package check

import (
	"math/big"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

const validIndexValidationSource = `
fn check(array [3]i32, values []i32, text str, index i32) void {
    let valid i32 = array[1];
    let validSlice []i32 = array[1:3];
    let runtimeArray i32 = array[index];
    let runtimeSlice []i32 = values[index:];
    let runtimeString char = text[index];
}
`

const invalidIndexValidationSource = `
fn check(array [3]i32, values []i32, text str, index i32) void {
    let negative i32 = array[-1];
    let outOfRange i32 = array[3];
    let reversed []i32 = array[2:1];
}
`

func TestValidateIndexRecords(t *testing.T) {
	diagnostics, handoff, solved := runIndexValidation(t, `fn check(array [3]i32) void {
    let valid i32 = array[1];
    let validSlice []i32 = array[1:3];
}`)
	if !validateIndexRecords(handoff, solved, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeIndexBound) {
		t.Fatalf("valid index records were rejected: %+v", diagnostics.Items())
	}

	diagnostics, handoff, solved = runIndexValidation(t, invalidIndexValidationSource)
	if hasValidationDiagnostic(diagnostics, CodeIndexBound) {
		t.Fatalf("generation unexpectedly reported C0609: %+v", diagnostics.Items())
	}
	if validateIndexRecords(handoff, solved, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeIndexBound) {
		t.Fatalf("invalid index records were not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateIndexRecordsSkipsUnavailableRuntimeBounds(t *testing.T) {
	diagnostics, handoff, solved := runIndexValidation(t, validIndexValidationSource)
	solved.constants = map[symbol.SyntaxRef]constantResult{}
	if !validateIndexRecords(handoff, solved, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeIndexBound) {
		t.Fatalf("unavailable runtime bounds were rejected: %+v", diagnostics.Items())
	}
}

func TestValidateIndexRecordsAcceptsRuntimeParameterIndex(t *testing.T) {
	diagnostics, handoff, solved := runIndexValidation(t, `fn get(array [3]i32, at i32) i32 {
    return array[at];
}`)
	if !validateIndexRecords(handoff, solved, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeIndexBound) {
		t.Fatalf("runtime parameter index was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateIndexRecordsSkipsConstantEvaluationError(t *testing.T) {
	_, handoff, solved := runIndexValidation(t, `fn check(array [3]i32) void { let value i32 = array[1]; }`)
	var index *indexRecord
	for _, retained := range handoff.Records.Records() {
		if retained.Index != nil {
			copy := *retained.Index
			index = &copy
			break
		}
	}
	if index == nil {
		t.Fatal("expected an index record")
	}
	solved.constants = map[symbol.SyntaxRef]constantResult{
		index.StartSyntax: {State: constantError},
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	if !validateIndexRecords(handoff, solved, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeIndexBound) {
		t.Fatalf("constant evaluation error was treated as an invalid bound: %+v", diagnostics.Items())
	}
}

func TestValidateIndexRecordsSuppressesInactiveAndUnresolved(t *testing.T) {
	diagnostics, handoff, solved := runIndexValidation(t, `fn check(array [3]i32) void { let value i32 = array[1]; }`)
	var retained retainedRecord
	for _, candidate := range handoff.Records.Records() {
		if candidate.Index != nil {
			retained = candidate
			break
		}
	}
	if retained.Index == nil {
		t.Fatal("expected an index record")
	}
	retained.Header.Alternative = alternativeTag{Guarded: true, Choice: 1, Index: 1}
	handoff.Records = frozenRecords{values: []retainedRecord{retained}}
	diagnostics = diagnostic.NewDiagnosticSet()
	if !validateIndexRecords(handoff, solved, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeIndexBound) {
		t.Fatalf("inactive index record was not suppressed: %+v", diagnostics.Items())
	}

	retained.Header.Alternative = alternativeTag{}
	handoff.Records = frozenRecords{values: []retainedRecord{retained}}
	solved.roots[retained.Index.Result] = infer.TypeResult{State: infer.TypeError}
	diagnostics = diagnostic.NewDiagnosticSet()
	if !validateIndexRecords(handoff, solved, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeIndexBound) {
		t.Fatalf("unresolved index record was not suppressed: %+v", diagnostics.Items())
	}
}

func TestValidateIndexRecordsRangeRules(t *testing.T) {
	_, handoff, solved := runIndexValidation(t, `fn check(array [3]i32) void { let value i32 = array[1]; }`)
	var retained retainedRecord
	for _, candidate := range handoff.Records.Records() {
		if candidate.Index != nil {
			retained = candidate
			break
		}
	}
	if retained.Index == nil {
		t.Fatal("expected an index record")
	}
	solved.constants = map[symbol.SyntaxRef]constantResult{
		retained.Index.StartSyntax: {State: constantKnown, Value: constantValue{Kind: constantInteger, Integer: big.NewInt(3)}},
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	if validateIndexRecords(handoff, solved, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeIndexBound) {
		t.Fatalf("exclusive index upper bound was accepted: %+v", diagnostics.Items())
	}
}

func runIndexValidation(t *testing.T, source string) (*diagnostic.DiagnosticSet, *solveHandoff, *solvedRecords) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	solved, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, handoff, solved
}
