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

func TestValidateMemberRecordsAcceptsPointerFieldAccessAndWrites(t *testing.T) {
	source := `
type P = struct {
    cap i32;
    fn get(self *P) i32 => self.cap;
    fn other(self *P) i32 => self.cap;
};
fn read(p *P) i32 { return p.cap; }
fn check() i32 {
    var p P = P.{ cap = 1 };
    let pointer *P = &p;
    pointer.cap = 2;
    return pointer.get() + pointer.other() + p.cap;
}
`

	diagnostics, _, _ := runMemberValidation(t, source)
	if diagnostics.HasErrors() {
		t.Fatalf("pointer field access was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsAcceptsUnionVariantPayloadWrite(t *testing.T) {
	source := `
type Choice = union enum { Ok i32; Err str; };
fn set_err(self *Choice, e str) void {
    self.Err = e;
}
fn check() void {
    var c Choice = Choice.Ok(1);
    set_err(&c, "oops");
}
`
	diagnostics, _, _ := runMemberValidation(t, source)
	if diagnostics.HasErrors() {
		t.Fatalf("union variant payload write was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsRejectsUnknownUnionVariantPayloadWrite(t *testing.T) {
	source := `
type Choice = union enum { Ok i32; Err str; };
fn set_err(self *Choice, e str) void {
    self.Err = e;
}
`
	diagnostics, handoff, records := runMemberValidation(t, source)
	for _, retained := range handoff.Records.values {
		if retained.Member != nil && retained.Member.Kind == memberField && retained.Member.Name == "Err" {
			retained.Member.Name = "Typo"
			break
		}
	}
	if validateMemberRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatalf("unknown union variant payload write was accepted: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsAcceptsPointerSliceStructuralFields(t *testing.T) {
	source := `
fn read[T](s *[]T) uint { return s.len; }
`
	diagnostics, _, _ := runMemberValidation(t, source)
	if diagnostics.HasErrors() {
		t.Fatalf("pointer slice structural field was rejected: %+v", diagnostics.Items())
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

func TestValidateMemberRecordsAcceptsNarrowedUnionVariantAccess(t *testing.T) {
	source := `
type Data = union enum { Ok i32; Err str; };
fn get(self Data) int {
    switch self {
    case .Ok: return self.Ok;
    case .Err: return 0;
    }
    return 0;
}
`
	diagnostics, handoff, records := runMemberValidation(t, source)
	if !validateMemberRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatalf("union variant read in its narrowed case arm was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsRejectsUnionVariantAccessInWrongCase(t *testing.T) {
	source := `
type Data = union enum { Ok i32; Err str; };
fn get(self Data) int {
    switch self {
    case .Err: return self.Ok;
    case .Ok: return 0;
    }
    return 0;
}
`
	diagnostics, handoff, records := runMemberValidation(t, source)
	if validateMemberRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatalf("union variant read in a different case arm was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsRejectsUnionVariantAccessOutsideSwitch(t *testing.T) {
	source := `
type Data = union enum { Ok i32; Err str; };
fn get(self Data) int {
    return self.Ok;
}
`
	diagnostics, handoff, records := runMemberValidation(t, source)
	if validateMemberRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatalf("union variant read outside any switch was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsAcceptsGenericSelfNarrowedVariantRead(t *testing.T) {
	// The dispatch's read-side fix: a method whose receiver refers to its own
	// generic union type (`fn unwrap_or(self Result[T, E], ...)`) reads a
	// variant's payload (`self.Ok`) inside the switch arm narrowed to that
	// variant. Before the fix, the case-label aggregate lost its declaration
	// (Declaration=0) so the narrowed read was rejected with C0605.
	source := `
type Result[T, E] = union enum {
    Ok T;
    Err E;
    fn unwrap_or(self Result[T, E], def T) T {
        switch self {
        case .Ok: return self.Ok;
        case .Err: return def;
        }
    }
}
`
	diagnostics, handoff, records := runMemberValidation(t, source)
	if !validateMemberRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatalf("generic-self union variant read in its narrowed case arm was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsAcceptsGenericSelfNarrowedVariantReadInConstruction(t *testing.T) {
	// The map-shaped read: a generic-self variant read inside a record
	// construction in a narrowed arm (result.peb's `map`), the second place the
	// original C0605 fired.
	source := `
type Result[T, E] = union enum {
    Ok T;
    Err E;
    fn map[U](self Result[T, E], f fn(T) U) Result[U, E] {
        switch self {
        case .Ok: return Result[U, E].{ Ok = f(self.Ok) };
        case .Err: return Result[U, E].{ Err = self.Err };
        }
    }
}
`
	diagnostics, handoff, records := runMemberValidation(t, source)
	if !validateMemberRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatalf("generic-self union variant read in a narrowed construction was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsRejectsGenericSelfVariantReadInWrongCase(t *testing.T) {
	// The fix must not make narrowing overly permissive: reading one variant's
	// payload inside the arm narrowed to a different variant stays rejected.
	source := `
type Result[T, E] = union enum {
    Ok T;
    Err E;
    fn get(self Result[T, E]) T {
        switch self {
        case .Err: return self.Ok;
        case .Ok: return self.Ok;
        }
    }
}
`
	diagnostics, handoff, records := runMemberValidation(t, source)
	if validateMemberRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatalf("generic-self union variant read in a different case arm was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateMemberRecordsRejectsGenericSelfVariantReadOutsideSwitch(t *testing.T) {
	// And a generic-self variant read outside any narrowing arm stays rejected.
	source := `
type Result[T, E] = union enum {
    Ok T;
    Err E;
    fn get(self Result[T, E]) T {
        return self.Ok;
    }
}
`
	diagnostics, handoff, records := runMemberValidation(t, source)
	if validateMemberRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeMember) {
		t.Fatalf("generic-self union variant read outside any switch was not rejected: %+v", diagnostics.Items())
	}
}
