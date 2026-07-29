package check

import (
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

func validateCompatibilityFixture(t *testing.T, source string) (*diagnostic.DiagnosticSet, bool) {
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
	return diagnostics, validateCompatibilityRecords(handoff, records, diagnostics, Config{})
}

func hasConversionDiagnostic(set *diagnostic.DiagnosticSet) bool {
	for _, item := range set.Items() {
		if item.Code == CodeConversion {
			return true
		}
	}
	return false
}

func TestValidateCompatibilityRecordsAcceptsIdentityAssignment(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, `fn identity(value i32) i32 { let copy i32 = value; return copy; }`)
	if !ok || hasConversionDiagnostic(diagnostics) {
		t.Fatalf("identity assignment was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCompatibilityRecordsAcceptsImplicitOptionalInjection(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, `fn optional(value i32) ?i32 { let result ?i32 = value; return result; }`)
	if !ok || hasConversionDiagnostic(diagnostics) {
		t.Fatalf("implicit optional injection was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCompatibilityRecordsAcceptsExplicitCast(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, `fn cast(value i32) i64 { return value as i64; }`)
	if !ok || hasConversionDiagnostic(diagnostics) {
		t.Fatalf("explicit cast was rejected: %+v", diagnostics.Items())
	}
}

func compatibilityValidationHandoff(t *testing.T, record retainedRecord) (*solveHandoff, *diagnostic.DiagnosticSet) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}
	handoff.Records = frozenRecords{values: []retainedRecord{record}}
	return handoff, diagnostics
}

func TestValidateCompatibilityRecordsRejectsForbiddenPair(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}
	builtins := inputs.Types.Builtins()
	handoff.Records = frozenRecords{values: []retainedRecord{{
		Header:        recordHeader{ID: 1, Owner: 1},
		Compatibility: &compatibilityRecord{Header: recordHeader{ID: 1, Owner: 1}, Source: 1, Destination: 2, Role: compatibilityAssignment},
	}}}
	records := &solvedRecords{roots: map[valueID]infer.TypeResult{
		1: {State: infer.TypeFinal, Type: builtins.Bool},
		2: {State: infer.TypeFinal, Type: builtins.I32},
	}}
	fresh := diagnostic.NewDiagnosticSet()
	if validateCompatibilityRecords(handoff, records, fresh, Config{}) || !hasConversionDiagnostic(fresh) {
		t.Fatalf("forbidden compatibility was not rejected: %+v", fresh.Items())
	}
}

func TestValidateCompatibilityRecordsSkipsUnresolvedRoots(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}
	builtins := inputs.Types.Builtins()
	handoff.Records = frozenRecords{values: []retainedRecord{{
		Header:        recordHeader{ID: 1, Owner: 1},
		Compatibility: &compatibilityRecord{Header: recordHeader{ID: 1, Owner: 1}, Source: 1, Destination: 2, Role: compatibilityAssignment},
	}}}
	for _, roots := range []map[valueID]infer.TypeResult{
		{1: {State: infer.TypeError}, 2: {State: infer.TypeFinal, Type: builtins.I32}},
		{1: {State: infer.TypeFinal, Type: builtins.Bool}},
	} {
		fresh := diagnostic.NewDiagnosticSet()
		if !validateCompatibilityRecords(handoff, &solvedRecords{roots: roots}, fresh, Config{}) || hasConversionDiagnostic(fresh) {
			t.Fatalf("unresolved compatibility was not skipped: %+v", fresh.Items())
		}
	}
}

func TestValidateCompatibilityRecordsSkipsInactiveRecord(t *testing.T) {
	guard := alternativeTag{Choice: 999999, Index: 7, Guarded: true}
	handoff, _ := compatibilityValidationHandoff(t, retainedRecord{
		Header:        recordHeader{ID: 1, Owner: 1, Alternative: guard},
		Compatibility: &compatibilityRecord{Header: recordHeader{ID: 1, Owner: 1, Alternative: guard}, Source: 1, Destination: 2, Role: compatibilityAssignment},
	})
	builtins := handoff.Semantics.Types().Builtins()
	fresh := diagnostic.NewDiagnosticSet()
	if !validateCompatibilityRecords(handoff, &solvedRecords{roots: map[valueID]infer.TypeResult{
		1: {State: infer.TypeFinal, Type: builtins.Bool}, 2: {State: infer.TypeFinal, Type: builtins.I32},
	}}, fresh, Config{}) || hasConversionDiagnostic(fresh) {
		t.Fatalf("inactive compatibility record was not skipped: %+v", fresh.Items())
	}
}

func TestValidateCompatibilityRecordsUsesRoleContext(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}
	builtins := inputs.Types.Builtins()
	makeRecord := func(role compatibilityRole) retainedRecord {
		header := recordHeader{ID: 1, Owner: 1}
		return retainedRecord{Header: header, Compatibility: &compatibilityRecord{Header: header, Source: 1, Destination: 2, Role: role, Ordinal: 1}}
	}
	messages := make([]string, 0, 2)
	for _, role := range []compatibilityRole{compatibilityAssignment, compatibilityArgument} {
		handoff.Records = frozenRecords{values: []retainedRecord{makeRecord(role)}}
		fresh := diagnostic.NewDiagnosticSet()
		validateCompatibilityRecords(handoff, &solvedRecords{roots: map[valueID]infer.TypeResult{
			1: {State: infer.TypeFinal, Type: builtins.Bool}, 2: {State: infer.TypeFinal, Type: builtins.I32},
		}}, fresh, Config{})
		if len(fresh.Items()) != 1 {
			t.Fatalf("expected one conversion diagnostic: %+v", fresh.Items())
		}
		messages = append(messages, fresh.Items()[0].Message)
	}
	if messages[0] == messages[1] || !strings.Contains(messages[0], "assignment") || !strings.Contains(messages[1], "argument 2") {
		t.Fatalf("role context was not used: %q, %q", messages[0], messages[1])
	}
}
