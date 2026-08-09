package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

// TestCheckWrappingU64BuiltinCallsTypeCheck runs the full checker pipeline over
// a program that calls both wrapping u64 builtins with u64 arguments (typed
// u64 locals and u64-width literals, including the UINT64_MAX boundary), and
// requires the whole check to succeed: prelude resolution of the builtin
// symbols, signature registration (prepareBuiltinSignatures in
// internal/infer/builtin.go), call fact collection against the builtin's
// (u64, u64) -> u64 shape, solve, validation, and typed-IR construction. This
// is the positive registration path for the prelude-registered builtin
// functions; a failure at any phase rejects the fixture.
func TestCheckWrappingU64BuiltinCallsTypeCheck(t *testing.T) {
	source := `
fn main() int {
    var product u64 = wrapping_mul_u64(6, 7);
    if product != 42 { return 1; }
    var wrapped u64 = wrapping_mul_u64(18446744073709551615, 2);
    if wrapped != 18446744073709551614 { return 2; }
    var sum u64 = wrapping_add_u64(2, 3);
    if sum != 5 { return 3; }
    var zero u64 = wrapping_add_u64(18446744073709551615, 1);
    if zero != 0 { return 4; }
    return 0;
}
`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() {
		t.Fatalf("wrapping u64 builtin calls were rejected: %+v", diagnostics.Items())
	}
	if result.IR() == nil {
		t.Fatal("successful check produced no typed-IR unit")
	}
}

// TestCheckRejectsWrappingU64BuiltinNonU64Argument confirms a non-u64 argument
// (a str literal where the builtin signature demands u64) is a clean
// checker-level type error, not a crash: the call fails unification (T0505)
// and argument conversion (C0601), and no C0619 internal-generation error
// leaks. The builtin's signature is registered exactly like an authored
// function's, so a wrong argument type must be reported the same way.
func TestCheckRejectsWrappingU64BuiltinNonU64Argument(t *testing.T) {
	source := `
fn main() int {
    var r u64 = wrapping_mul_u64("not a u64", 7);
    return r as int;
}
`
	diagnostics, result := run06bFixture(t, source)
	if result.Successful() {
		t.Fatal("wrapping_mul_u64 accepted a str first argument")
	}
	if !hasCode(diagnostics, infer.CodeUnification) {
		t.Fatalf("wrapping_mul_u64 str argument produced no unification error: %+v", diagnostics.Items())
	}
	if !hasCode(diagnostics, CodeConversion) {
		t.Fatalf("wrapping_mul_u64 str argument produced no conversion error: %+v", diagnostics.Items())
	}
	if hasCode(diagnostics, CodeGeneration) {
		t.Fatalf("wrapping_mul_u64 str argument leaked the C0619 internal-error path: %+v", diagnostics.Items())
	}
}
