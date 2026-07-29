package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

func TestValidatePlaceRecordsAddressWritability(t *testing.T) {
	tests := []struct {
		name    string
		source  string
		valid   bool
		message string
	}{
		{
			name: "writable local",
			source: `fn check() *i32 {
    var value i32 = 1;
    return &value;
}`,
			valid: true,
		},
		{
			name: "immutable local",
			source: `fn check() *i32 {
    let value i32 = 1;
    return &value;
}`,
			message: "let storage is rejected",
		},
		{
			name:   "dereference",
			source: `fn check(pointer *i32) *i32 { return &(*pointer); }`,
			valid:  true,
		},
		{
			name: "field inheritance",
			source: `type Box = struct { value i32; };
fn check() *i32 {
    var box Box;
    return &box.value;
}`,
			valid: true,
		},
		{
			name: "immutable field inheritance",
			source: `type Box = struct { value i32; };
fn check() *i32 {
    let box Box;
    return &box.value;
}`,
			message: "let field base is rejected",
		},
		{
			name: "array index inheritance",
			source: `fn check() *i32 {
    let values [2]i32;
    return &values[0];
}`,
			message: "array index inherits let root",
		},
		{
			name: "slice index",
			source: `fn check() *i32 {
    let values []i32;
    return &values[0];
}`,
			valid: true,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			diagnostics, handoff, records := runPlaceValidation(t, test.source)
			valid := validatePlaceRecords(handoff, records, diagnostics, Config{})
			hasPlaceError := hasValidationDiagnostic(diagnostics, CodePlace)
			if valid != test.valid || hasPlaceError == test.valid {
				t.Fatalf("valid=%v C0606=%v want valid=%v (%s): %+v", valid, hasPlaceError, test.valid, test.message, diagnostics.Items())
			}
		})
	}
}

func TestValidatePlaceRecordsRejectsLateStringIndexPlace(t *testing.T) {
	diagnostics, handoff, records := runPlaceValidation(t, `fn check(text str, index i32) *char {
    return &text[index];
}`)
	for _, retained := range handoff.Records.Records() {
		if retained.Place != nil {
			t.Fatalf("generation retained a string index as a place: %+v", retained.Place)
		}
	}
	if !validatePlaceRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodePlace) {
		t.Fatalf("non-place string indexing was diagnosed: %+v", diagnostics.Items())
	}

	diagnostics, handoff, records = runPlaceValidation(t, `fn check(text str, index i32) *i32 {
    var value i32;
    let ignored = text[index];
    return &value;
}`)
	var place retainedRecord
	var indexBase valueID
	for _, retained := range handoff.Records.Records() {
		if retained.Place != nil && place.Place == nil {
			place = retained
		}
		if retained.Index != nil && retained.Index.Mode == indexValue {
			indexBase = retained.Index.Base
		}
	}
	if place.Place == nil || indexBase == 0 {
		t.Fatal("test setup did not produce an address place and string index")
	}
	place.Place.Projections = append(place.Place.Projections, placeProjection{Kind: placeIndex, Base: indexBase, Index: 1})
	handoff.Records = frozenRecords{values: []retainedRecord{place}}
	*diagnostics = diagnostic.DiagnosticSet{}
	if validatePlaceRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodePlace) {
		t.Fatalf("fabricated string index place was accepted: %+v", diagnostics.Items())
	}
}

func TestValidatePlaceRecordsSuppressesUnresolvedIndexPlace(t *testing.T) {
	diagnostics, handoff, records := runPlaceValidation(t, `fn check() *i32 {
    var value i32;
    return &value;
}`)
	var retained retainedRecord
	for _, candidate := range handoff.Records.Records() {
		if candidate.Place != nil {
			retained = candidate
			break
		}
	}
	if retained.Place == nil {
		t.Fatal("expected an address place")
	}
	base := valueID(999999)
	retained.Place.Projections = append(retained.Place.Projections, placeProjection{Kind: placeIndex, Base: base, Index: 1})
	records.roots[base] = infer.TypeResult{State: infer.TypeError}
	handoff.Records = frozenRecords{values: []retainedRecord{retained}}
	*diagnostics = diagnostic.DiagnosticSet{}
	if !validatePlaceRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodePlace) {
		t.Fatalf("unresolved index place was diagnosed: %+v", diagnostics.Items())
	}
}

func TestValidatePlaceRecordsSkipsInactivePlace(t *testing.T) {
	diagnostics, handoff, records := runPlaceValidation(t, `fn check() *i32 {
    let value i32 = 1;
    return &value;
}`)
	var retained retainedRecord
	for _, candidate := range handoff.Records.Records() {
		if candidate.Place != nil {
			retained = candidate
			break
		}
	}
	if retained.Place == nil {
		t.Fatal("expected a place record")
	}
	retained.Header.Alternative = alternativeTag{Guarded: true, Choice: 1, Index: 1}
	handoff.Records = frozenRecords{values: []retainedRecord{retained}}
	*diagnostics = diagnostic.DiagnosticSet{}
	if !validatePlaceRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodePlace) {
		t.Fatalf("inactive place was diagnosed: %+v", diagnostics.Items())
	}
}

func runPlaceValidation(t *testing.T, source string) (*diagnostic.DiagnosticSet, *solveHandoff, *solvedRecords) {
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
