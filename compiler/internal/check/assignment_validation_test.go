package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

func TestValidateAssignmentRecords(t *testing.T) {
	tests := []struct {
		name   string
		source string
		valid  bool
	}{
		{
			name: "simple writable assignment",
			source: `fn check() void {
    var value i32 = 1;
    value = 2;
}`,
			valid: true,
		},
		{
			name: "compound writable assignment",
			source: `fn check() void {
    var value i32 = 1;
    value += 2;
}`,
			valid: true,
		},
		{
			name: "standalone postfix update",
			source: `fn check() void {
    var value i32 = 1;
    value++;
}`,
			valid: true,
		},
		{
			name: "for update postfix",
			source: `fn check() void {
    for var value i32 = 0; value < 3; value++ {}
}`,
			valid: true,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			diagnostics, handoff, records := runPlaceValidation(t, test.source)
			valid := validateAssignmentRecords(handoff, records, diagnostics, Config{})
			if valid != test.valid || hasValidationDiagnostic(diagnostics, CodePlace) == test.valid {
				t.Fatalf("valid=%v C0606=%v want valid=%v: %+v", valid, hasValidationDiagnostic(diagnostics, CodePlace), test.valid, diagnostics.Items())
			}
		})
	}
}

func TestValidateAssignmentRecordsAllowsParameterMutationAcrossRegions(t *testing.T) {
	for _, source := range []string{
		`fn nested(flag bool, value i32) void {
    if flag { while value > 0 { value = value - 1; } }
}`,
		`fn deferred(flag bool, value i32) void {
    defer value = 3;
    if flag { defer value = 4; }
}`,
	} {
		diagnostics, handoff, records := runPlaceValidation(t, source)
		if !validateAssignmentRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodePlace) {
			t.Fatalf("parameter mutation in nested/deferred region was rejected: %+v", diagnostics.Items())
		}
	}
}

func TestValidateAssignmentRecordsRejectsImmutableTarget(t *testing.T) {
	for _, source := range []string{
		`fn check() void {
    var value i32 = 1;
    value = 2;
}`,
		`fn check() void {
    var value i32 = 1;
    value += 2;
}`,
	} {
		diagnostics, handoff, records := runPlaceValidation(t, source)
		retained := handoff.Records.Records()
		assignmentKeys := make(map[assignmentRecordKey]bool)
		for _, candidate := range retained {
			if candidate.Assignment != nil {
				assignmentKeys[assignmentRecordKey{syntaxRef: candidate.Header.Syntax, owner: candidate.Header.Owner}] = true
			}
		}
		found := false
		for index := range retained {
			key := assignmentRecordKey{syntaxRef: retained[index].Header.Syntax, owner: retained[index].Header.Owner}
			if retained[index].Place != nil && assignmentKeys[key] {
				retained[index].Place.RootMutable = false
				found = true
			}
		}
		if !found {
			t.Fatal("test setup did not produce an assignment place")
		}
		handoff.Records = frozenRecords{values: retained}
		*diagnostics = diagnostic.DiagnosticSet{}
		if validateAssignmentRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodePlace) {
			t.Fatalf("immutable assignment target was accepted: %+v", diagnostics.Items())
		}
	}
}

func TestValidateAssignmentRecordsRejectsMisplacedMutation(t *testing.T) {
	diagnostics, handoff, records := runPlaceValidation(t, `fn check() void {
    var value i32 = 1;
    value++;
}`)
	retained := handoff.Records.Records()
	for index := range retained {
		if retained[index].Control != nil && retained[index].Control.Kind == controlExpression && retained[index].Control.StatementForm == statementPostfixUpdate {
			retained[index].Control.StatementForm = statementDiscard
		}
	}
	handoff.Records = frozenRecords{values: retained}
	*diagnostics = diagnostic.DiagnosticSet{}
	if validateAssignmentRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodePlace) {
		t.Fatalf("misplaced mutation was accepted: %+v", diagnostics.Items())
	}
}

func TestValidateAssignmentRecordsRejectsNestedMutation(t *testing.T) {
	diagnostics, handoff, records := runPlaceValidation(t, `fn check() void {
    var value i32 = 1;
    var result i32 = value++ + 1;
}`)
	if validateAssignmentRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodePlace) {
		t.Fatalf("nested mutation was accepted: %+v", diagnostics.Items())
	}
}

func TestValidateAssignmentRecordsSkipsInactive(t *testing.T) {
	diagnostics, handoff, records := runPlaceValidation(t, `fn check() void {
    var value i32 = 1;
    value = 2;
}`)
	retained := handoff.Records.Records()
	for index := range retained {
		if retained[index].Assignment != nil {
			retained[index].Header.Alternative = alternativeTag{Guarded: true, Choice: 1, Index: 1}
		}
	}
	handoff.Records = frozenRecords{values: retained}
	*diagnostics = diagnostic.DiagnosticSet{}
	if !validateAssignmentRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodePlace) {
		t.Fatalf("inactive assignment was diagnosed: %+v", diagnostics.Items())
	}
}
