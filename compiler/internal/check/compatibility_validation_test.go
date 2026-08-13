package check

import (
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
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

// TestNarrowWidthReturnCoercionRejectsDifferentConcreteWidths proves that
// returning a local of a distinct concrete integer width from an int-returning
// function is cleanly rejected at the checker level with C0601 — not silently
// passed through to the backend where it would produce a confusing internal-
// sounding Emit error. Every fixed-width integer builtin whose C type differs
// from int's (int32_t) is covered.
func TestNarrowWidthReturnCoercionRejectsDifferentConcreteWidths(t *testing.T) {
	tests := []struct {
		name string
		src  string
	}{
		{"u8_to_int", "fn main() int { var x u8 = 200; return x; }"},
		{"i16_to_int", "fn main() int { var x i16 = -100; return x; }"},
		{"u16_to_int", "fn main() int { var x u16 = 100; return x; }"},
		{"i64_to_int", "fn main() int { var x i64 = 9007199254740992; return x; }"},
		{"u32_to_int", "fn main() int { var x u32 = 4294967295; return x; }"},
		{"u64_to_int", "fn main() int { var x u64 = 18446744073709551615; return x; }"},
		{"uint_to_int", "fn main() int { var x uint = 18446744073709551615; return x; }"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics, ok := validateCompatibilityFixture(t, tc.src)
			if ok || !hasConversionDiagnostic(diagnostics) {
				t.Fatalf("%s: expected C0601 rejection but got success: %+v", tc.name, diagnostics.Items())
			}
			items := diagnostics.Items()
			if len(items) == 0 {
				t.Fatalf("%s: expected at least one diagnostic, got none", tc.name)
			}
			found := false
			for _, d := range items {
				if d.Code == CodeConversion && strings.Contains(d.Message, "explicit cast") {
					found = true
					break
				}
			}
			if !found {
				t.Fatalf("%s: expected C0601 with 'explicit cast' message, got: %+v", tc.name, items)
			}
		})
	}
}

// TestSameConcreteWidthIntegerPairIsAccepted proves that integer pairs sharing
// the same concrete C width (e.g. i32 ↔ int, both int32_t) pass validation
// without requiring an explicit cast. This covers the coincidentally-working
// case that was previously masked by the backend's own width-equivalence gate.
func TestSameConcreteWidthIntegerPairIsAccepted(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, "fn main() int { var x i32 = 5; return x; }")
	if !ok || hasConversionDiagnostic(diagnostics) {
		t.Fatalf("i32→int (same concrete width) was rejected: %+v", diagnostics.Items())
	}
}

// TestExplicitCastStillWorks proves that the explicit-cast form continues to
// check successfully for all the distinct-width pairs that are now rejected
// when returned bare.
func TestExplicitCastStillWorks(t *testing.T) {
	tests := []string{
		"fn main() int { var x u8 = 200; return x as int; }",
		"fn main() int { var x i16 = -100; return x as int; }",
		"fn main() int { var x u16 = 100; return x as int; }",
		"fn main() int { var x i64 = 9007199254740992; return x as int; }",
		"fn main() int { var x u32 = 4294967295; return x as int; }",
		"fn main() int { var x u64 = 18446744073709551615; return x as int; }",
		"fn main() int { var x uint = 18446744073709551615; return x as int; }",
		"fn main() int { var x i32 = 5; return x as int; }",
	}
	for _, src := range tests {
		diagnostics, ok := validateCompatibilityFixture(t, src)
		if !ok || hasConversionDiagnostic(diagnostics) {
			t.Fatalf("explicit cast form was rejected: %s; %+v", src, diagnostics.Items())
		}
	}
}

// TestNarrowWidthDeclarationInitializerRejected proves that var/let
// declaration initializers with distinct-concrete-width types are also
// rejected (not just returns), since they route through the same
// compatibility-record mechanism.
func TestNarrowWidthDeclarationInitializerRejected(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, "fn get_u8() u8 { return 200; } fn main() int { let x int = get_u8(); return 0; }")
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("narrow-width declaration initializer was not rejected: %+v", diagnostics.Items())
	}
}

// TestNarrowWidthAssignmentRejected proves that plain assignment targets with
// distinct-concrete-width types are also rejected.
func TestNarrowWidthAssignmentRejected(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, "fn main() int { var target int = 0; var source u8 = 200; target = source; return 0; }")
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("narrow-to-int assignment was not rejected: %+v", diagnostics.Items())
	}
}

// TestNarrowWidthArgumentRejected proves that call arguments with distinct-
// concrete-width types are also rejected.
func TestNarrowWidthArgumentRejected(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, "fn takes_int(x int) void {} fn main() int { var v u8 = 200; takes_int(v); return 0; }")
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("narrow-to-int argument was not rejected: %+v", diagnostics.Items())
	}
}

// TestPointerToPointerStillRejected proves that pointer-to-pointer conversions
// (which were already rejected before this change) remain rejected.
func TestPointerToPointerStillRejected(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}
	builtins := inputs.Types.Builtins()
	pointerI32, _ := inputs.Types.Intern(types.PointerKey(builtins.I32))
	pointerI64, _ := inputs.Types.Intern(types.PointerKey(builtins.I64))
	handoff.Records = frozenRecords{values: []retainedRecord{{
		Header:        recordHeader{ID: 1, Owner: 1},
		Compatibility: &compatibilityRecord{Header: recordHeader{ID: 1, Owner: 1}, Source: 1, Destination: 2, Role: compatibilityAssignment},
	}}}
	records := &solvedRecords{roots: map[valueID]infer.TypeResult{
		1: {State: infer.TypeFinal, Type: pointerI32},
		2: {State: infer.TypeFinal, Type: pointerI64},
	}}
	fresh := diagnostic.NewDiagnosticSet()
	if validateCompatibilityRecords(handoff, records, fresh, Config{}) || !hasConversionDiagnostic(fresh) {
		t.Fatalf("pointer-to-pointer was not rejected: %+v", fresh.Items())
	}
}

// TestFloatToIntExplicitlyRequired proves that float-to-integer conversions
// (classified as compatibleExplicit) require an explicit cast.
func TestFloatToIntExplicitlyRequired(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, "fn main() int { var x f32 = 3.14; return x; }")
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("f32→int was not rejected: %+v", diagnostics.Items())
	}
}

// TestExplicitCastFloatToIntStillWorks proves that explicit float-to-int cast
// continues to work.
func TestExplicitCastFloatToIntStillWorks(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, "fn main() int { var x f32 = 3.14; return x as int; }")
	if !ok || hasConversionDiagnostic(diagnostics) {
		t.Fatalf("f32→int explicit cast was rejected: %+v", diagnostics.Items())
	}
}

// TestIntToU8ExplicitlyRequired proves that int-to-narrower-unsigned conversions
// also require an explicit cast (reverse direction).
func TestIntToU8ExplicitlyRequired(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, "fn main() u8 { var x int = 200; return x; }")
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("int→u8 was not rejected: %+v", diagnostics.Items())
	}
}

// TestEnumToIntegerExplicitlyRequired proves that enum-to-integer conversions
// (classified as compatibleExplicit) require an explicit cast.
func TestEnumToIntegerExplicitlyRequired(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, "type Color = enum { red; green; blue; }; fn main() int { var c Color = Color.red; return c; }")
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("enum→int was not rejected: %+v", diagnostics.Items())
	}
}

// TestTupleComponentCoercionExplicitlyRequired proves that tuple components
// whose element-wise classification yields compatibleExplicit are rejected
// when returned from a function (the return path uses compatibilityReturn,
// which remains strict). The tuple-initialization component coercion itself
// (compatibilityTupleComponent) is permissive — this test exercises the
// intersection where the return rejects but the initialization does not.
func TestTupleComponentCoercionExplicitlyRequired(t *testing.T) {
	diagnostics, ok := validateCompatibilityFixture(t, "fn main() (int, int) { var t (u8, u8) = (1, 2); return t; }")
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("tuple(u8,u8)→(int,int) was not rejected: %+v", diagnostics.Items())
	}
}

// TestOptionalInjectionIsAccepted proves that optional-injection positions
// (some <value> into a wider optional payload) remain implicitly coercible
// after the explicit-cast narrowing — classify may yield compatibleExplicit
// for the payload pair, but the compatibilityOptionalInjection role skips
// the new rejection gate entirely.
func TestOptionalInjectionIsAccepted(t *testing.T) {
	cases := []string{
		"fn main() void { var o ?u32 = some 5; }",
		"fn main() void { var x u8 = 5; var o ?u32 = some x; }",
		"fn main() void { var f f32 = 2.5; var o ?f64 = some f; }",
		"fn main() void { var c char = 'a'; var o ?i32 = some c; }",
	}
	for _, src := range cases {
		diagnostics, ok := validateCompatibilityFixture(t, src)
		if !ok || hasConversionDiagnostic(diagnostics) {
			t.Fatalf("optional injection was rejected: %s; %+v", src, diagnostics.Items())
		}
	}
}

// TestTupleComponentCoercionIsAccepted proves that tuple-component coercion
// positions (element-wise width mismatch in a tuple literal initializing a
// typed tuple local) remain implicitly coercible — the compatibilityRole
// compatibilityTupleComponent skips the explicit-cast gate.
func TestTupleComponentCoercionIsAccepted(t *testing.T) {
	cases := []string{
		"fn main() void { let t (i64, f64) = (1, 2.0); }",
		"fn main() void { let a i32 = 1; let b i32 = 2; let t (i64, f64) = (a, b); }",
		"fn main() void { let a i32 = 1; let b i32 = 2; let c i32 = 3; let t (i32, i64, i32) = (a, b, c); }",
	}
	for _, src := range cases {
		diagnostics, ok := validateCompatibilityFixture(t, src)
		if !ok || hasConversionDiagnostic(diagnostics) {
			t.Fatalf("tuple component coercion was rejected: %s; %+v", src, diagnostics.Items())
		}
	}
}

// TestStructFieldConstructionIsAccepted proves that struct-field construction
// positions (field initializer with width mismatch) remain implicitly coercible
// — the compatibilityRole compatibilityRecordField skips the explicit-cast gate.
func TestStructFieldConstructionIsAccepted(t *testing.T) {
	cases := []string{
		"type P = struct { x i64; }; fn main() void { let s P = P.{ x = 1 }; }",
		"type P = struct { x i64; y f64; }; fn main() void { let a i32 = 1; let b f32 = 1.5; let s P = P.{ x = a, y = b }; }",
	}
	for _, src := range cases {
		diagnostics, ok := validateCompatibilityFixture(t, src)
		if !ok || hasConversionDiagnostic(diagnostics) {
			t.Fatalf("struct field construction was rejected: %s; %+v", src, diagnostics.Items())
		}
	}
}
