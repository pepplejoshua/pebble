package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

const memberValidationSource = `
type Box = struct {
    value i32;
    fn get(self Box) i32 => self.value;
};
type Color = enum { red, blue };
fn check() void {
    let box Box = Box.{ value = 1 };
    let field i32 = box.value;
    let pair (i32, str) = (1, "a");
    let first i32 = pair.0;
    let method i32 = box.get();
    let color Color = Color.red;
}
`

func runMemberValidation(t *testing.T, source string) (*diagnostic.DiagnosticSet, *solveHandoff, *solvedRecords) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, handoff, records
}

func TestValidateMemberRecordsAcceptsFieldsTuplesVariantsAndMethods(t *testing.T) {
	diagnostics, handoff, records := runMemberValidation(t, memberValidationSource)
	if !validateMemberRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatalf("valid members were rejected: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsRejectsUnknownField(t *testing.T) {
	diagnostics, handoff, records := runMemberValidation(t, memberValidationSource)
	for _, retained := range handoff.Records.values {
		if retained.Member != nil && retained.Member.Kind == memberField {
			retained.Member.Name = "missing"
			break
		}
	}
	if validateMemberRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatal("unknown field was not rejected")
	}
}

func TestValidateMemberRecordsRejectsWrongFieldCategoryAndTupleOrdinal(t *testing.T) {
	diagnostics, handoff, records := runMemberValidation(t, memberValidationSource)
	for _, retained := range handoff.Records.values {
		if retained.Member == nil {
			continue
		}
		if retained.Member.Kind == memberField {
			retained.Member.Name = "get"
		}
		if retained.Member.Kind == memberTuple {
			retained.Member.TupleOrdinal = 2
		}
	}
	if validateMemberRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatal("wrong field category or tuple ordinal was not rejected")
	}
}
