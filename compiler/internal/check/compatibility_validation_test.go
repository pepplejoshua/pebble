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

func TestValidateCompatibilityRecordsAcceptsArrayLiteralSliceBinding(t *testing.T) {
	// The primitive-element reproduction: an array literal directly
	// initializing a slice-typed binding is valid (equivalent to constructing
	// the array then taking a full slice of it) and must survive both
	// validation and typed-IR construction.
	diagnostics, result := runVariadicCheck(t, `fn main() int { var s []int = [1, 2, 3]; return s[1]; }`)
	if !result.Successful() || diagnostics.HasErrors() {
		t.Fatalf("array-literal slice binding was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCompatibilityRecordsAcceptsArrayLiteralStructSliceBinding(t *testing.T) {
	// The struct-element twin: the literal's record values must ground to the
	// slice's element type, so the whole one-step binding passes the full
	// pipeline (validation and typed-IR construction).
	diagnostics, result := runVariadicCheck(t, `
type Point = struct { x int; };
fn main() int {
    var s []Point = [Point.{ x = 1 }, Point.{ x = 2 }];
    return s[1].x;
}
`)
	if !result.Successful() || diagnostics.HasErrors() {
		t.Fatalf("struct-element array-literal slice binding was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCompatibilityRecordsRejectsArrayLiteralSliceReassignment(t *testing.T) {
	// The gap stays narrow: REINITIALIZING an existing slice local from a bare
	// array literal is not a binding initializer, so it must keep the existing
	// C0601 rather than leaking into the binding-only acceptance.
	diagnostics, ok := validateCompatibilityFixture(t, `fn main() int { var s []int = [1, 2, 3]; s = [4, 5, 6]; return s[1]; }`)
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("array-literal slice reassignment was not rejected with C0601: %+v", diagnostics.Items())
	}
}

func TestValidateCompatibilityRecordsRejectsArrayLiteralSliceElementMismatch(t *testing.T) {
	// The slice binding's element type drives the literal's elements: a
	// numeric literal where a struct element is required must still fail.
	diagnostics, result := runVariadicCheck(t, `
type Point = struct { x int; };
fn main() int {
    var s []Point = [1, 2];
    return s[1].x;
}
`)
	if result.Successful() || !diagnostics.HasErrors() {
		t.Fatalf("element-mismatched array-literal slice binding was accepted: %+v", diagnostics.Items())
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

func runVariadicCheck(t *testing.T, source string) (*diagnostic.DiagnosticSet, *Result) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	return diagnostics, Check(inputs, diagnostics, Config{})
}

// The exact reported repro: the checker must collect the scalar call-site
// arguments into the variadic slice parameter, checking each against the
// slice's element type instead of the slice type itself.
func TestVariadicCallChecksElementTypedArguments(t *testing.T) {
	diagnostics, result := runVariadicCheck(t, `
fn sum(...values []i32) i32 { return values.len as i32; }
fn main() i32 { return sum(1, 2, 3); }
`)
	if !result.Successful() || diagnostics.HasErrors() {
		t.Fatalf("variadic call was rejected: %+v", diagnostics.Items())
	}
}

func TestVariadicCallSeparatesFixedPrefix(t *testing.T) {
	diagnostics, result := runVariadicCheck(t, `
fn tagged(prefix i32, ...values []i32) i32 { return prefix; }
fn main() i32 { return tagged(0, 1, 2, 3); }
`)
	if !result.Successful() || diagnostics.HasErrors() {
		t.Fatalf("fixed+varying call was rejected: %+v", diagnostics.Items())
	}
}

func TestVariadicCallAllowsZeroVariadicArguments(t *testing.T) {
	diagnostics, result := runVariadicCheck(t, `
fn sum(...values []i32) i32 { return values.len as i32; }
fn main() i32 { return sum(); }
`)
	if !result.Successful() || diagnostics.HasErrors() {
		t.Fatalf("empty variadic call was rejected: %+v", diagnostics.Items())
	}
}

func TestVariadicCallRejectsLiteralOutsideElementType(t *testing.T) {
	diagnostics, result := runVariadicCheck(t, `
fn sum(...values []i32) i32 { return values.len as i32; }
fn main() i32 { return sum(1, 3000000000); }
`)
	if result.Successful() || !hasValidationDiagnostic(diagnostics, diagnostic.Code("T0508")) {
		t.Fatalf("out-of-range variadic literal was not rejected with T0508: %+v", diagnostics.Items())
	}
}

func TestVariadicDeclarationAloneIsAccepted(t *testing.T) {
	diagnostics, result := runVariadicCheck(t, `
fn sum(...values []i32) i32 { return values.len as i32; }
`)
	if !result.Successful() || hasValidationDiagnostic(diagnostics, diagnostic.Code("C0604")) {
		t.Fatalf("variadic declaration alone was rejected: %+v", diagnostics.Items())
	}
}

func TestVariadicChangePreservesOrdinaryCalls(t *testing.T) {
	diagnostics, result := runVariadicCheck(t, `
fn add(left i32, right i32) i32 => left + right;
fn main() i32 { return add(1, 2); }
`)
	if !result.Successful() || diagnostics.HasErrors() {
		t.Fatalf("ordinary nonvariadic call was rejected: %+v", diagnostics.Items())
	}
}

// Calling a C-convention variadic remains C0604; this task only enables
// Pebble-convention variadic calls.
// A variadic parameter that isn't the sole trailing group must be rejected
// with a real diagnostic naming the problem (T0501, infer/declaration.go's
// prepareSignatures), not merely happen to fail later with an incidental
// type-mismatch error for the wrong reason.
func TestVariadicParameterMustBeLast(t *testing.T) {
	diagnostics, result := runVariadicCheck(t, `
fn weird(...values []i32, extra i32) i32 { return extra; }
`)
	if result.Successful() || !hasValidationDiagnostic(diagnostics, diagnostic.Code("T0501")) {
		t.Fatalf("non-trailing variadic parameter was not rejected with T0501: %+v", diagnostics.Items())
	}
}

func TestVariadicChangeKeepsCVariadicCallRejected(t *testing.T) {
	diagnostics, result := runVariadicCheck(t, `
extern "C" { fn printf(fmt str, ...args []u8) i32; }
fn main() i32 { return printf("hi", 1, 2); }
`)
	if result.Successful() || !hasValidationDiagnostic(diagnostics, diagnostic.Code("C0604")) {
		t.Fatalf("C variadic call was not rejected with C0604: %+v", diagnostics.Items())
	}
}

// TestIntegerLiteralRangeRejectedPerWidth proves the "V2 preserves text,
// constrains range" claim: the checker must cleanly reject (T0508) a literal
// one past the width's max — and one below the min for signed widths, any
// negative for unsigned widths — in a local-declaration initializer, instead
// of silently wrapping it. Every fixed-width integer builtin the backend
// emits is covered. This is the direct per-width generalization of the
// existing variadic-argument T0508 test (TestVariadicCallRejectsLiteralOutsideElementType).
//
// `int` is deliberately excluded: the checker constrains an `int` literal
// against LiteralTarget.WordBits (64 in the production pipeline), so
// `let x int = 2147483648;` is checker-accepted even though the backend emits
// `int` as int32_t (proposal 10 §10.45), and the emitted C then fails cc
// under -Werror. That checker/backend `int`-literal-range mismatch is a NEW
// FINDING (see the worklog and the task report), not something this rejection
// test may assert yet.
func TestIntegerLiteralRangeRejectedPerWidth(t *testing.T) {
	tests := []struct {
		name    string
		width   string
		tooHigh string
		tooLow  string
	}{
		{"i8", "i8", "128", "-129"},
		{"i16", "i16", "32768", "-32769"},
		{"i32", "i32", "2147483648", "-2147483649"},
		{"i64", "i64", "9223372036854775808", "-9223372036854775809"},
		{"u8", "u8", "256", "-1"},
		{"u16", "u16", "65536", "-1"},
		{"u32", "u32", "4294967296", "-1"},
		{"u64", "u64", "18446744073709551616", "-1"},
		{"uint", "uint", "18446744073709551616", "-1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			for _, value := range []string{tc.tooHigh, tc.tooLow} {
				src := "fn main() int { let x " + tc.width + " = " + value + "; print x; return 0; }"
				inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(src)})
				result := Check(inputs, diagnostics, Config{})
				if result.Successful() {
					t.Fatalf("out-of-range literal %s for %s was accepted", value, tc.width)
				}
				if !hasValidationDiagnostic(diagnostics, diagnostic.Code("T0508")) {
					t.Fatalf("out-of-range literal %s for %s was not rejected with T0508: %+v", value, tc.width, diagnostics.Items())
				}
			}
		})
	}
}

// TestFloatLiteralRangePerWidth proves the checker half of the float-literal
// claim: a float literal is range-constrained against the DESTINATION float
// width (overflow to infinity is rejected with T0508, per the literal-fitting
// rule in 05b), a literal at the finite maximum is accepted, and a
// subnormal/tiny value is accepted (underflow to zero is allowed). This is
// the check that guarantees the backend never emits a C floating constant
// that overflows its destination float/double type.
func TestFloatLiteralRangePerWidth(t *testing.T) {
	tests := []struct {
		name   string
		width  string
		value  string
		accept bool
	}{
		{"f64 maximum accepted", "f64", "1.7976931348623157e308", true},
		{"f64 over maximum rejected", "f64", "1.8e308", false},
		{"f32 maximum accepted", "f32", "3.40282346e38", true},
		{"f32 over maximum rejected", "f32", "3.4028235e38", false},
		{"f64-sized value into f32 rejected", "f32", "1e40", false},
		{"f32 subnormal accepted", "f32", "1.4e-45", true},
		{"f64 subnormal accepted", "f64", "5e-324", true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			src := "fn main() int { let x " + tc.width + " = " + tc.value + "; print x; return 0; }"
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(src)})
			result := Check(inputs, diagnostics, Config{})
			if got := result.Successful(); got != tc.accept {
				t.Fatalf("accepted = %v, want %v; diagnostics=%+v", got, tc.accept, diagnostics.Items())
			}
			if !tc.accept && !hasValidationDiagnostic(diagnostics, diagnostic.Code("T0508")) {
				t.Fatalf("rejected literal was not rejected with T0508: %+v", diagnostics.Items())
			}
		})
	}
}
