package check

import (
	"testing"
)

// TestNegatedLiteralIndexPlaceAssignment verifies that negated integer
// literals correctly infer their width from the destination type when
// assigned into index-place expressions (array elements, slice elements,
// and struct fields). This is the regression guard for C0601 regressions
// where the placeholder's type hadn't been resolved yet at the point
// expectationFor was called, causing the negated literal to default to
// plain int instead of inheriting the destination's concrete width.
func TestNegatedLiteralIndexPlaceAssignment(t *testing.T) {
	tests := []struct {
		name   string
		source string
	}{
		// Index-place into fixed-size array — i32
		{
			name: "fixed-array-element-i32",
			source: `fn main() int {
    var arr [3]i32 = [1, 2, 3];
    arr[0] = -1;
    return arr[0] as int;
}`,
		},
		// Index-place into fixed-size array — i16
		{
			name: "fixed-array-element-i16",
			source: `fn main() int {
    var arr [3]i16 = [1, 2, 3];
    arr[1] = -42;
    return arr[1] as int;
}`,
		},
		// Index-place into fixed-size array — i8
		{
			name: "fixed-array-element-i8",
			source: `fn main() int {
    var arr [5]i8 = [1, 2, 3, 4, 5];
    arr[2] = -7;
    return arr[2] as int;
}`,
		},
		// Index-place into slice — i32
		{
			name: "slice-element-i32",
			source: `fn main(s []i32) int {
    s[0] = -100;
    return s[0] as int;
}`,
		},
		// Index-place into slice — i16
		{
			name: "slice-element-i16",
			source: `fn main(s []i16) int {
    s[1] = -999;
    return s[1] as int;
}`,
		},
		// Index-place into struct field — i32
		{
			name: "struct-field-i32",
			source: `type Box = struct { value i32; };
fn main() int {
    var b = Box.{ value = 0 };
    b.value = -50;
    return b.value as int;
}`,
		},
		// Index-place into struct field — i16
		{
			name: "struct-field-i16",
			source: `type Box = struct { value i16; };
fn main() int {
    var b = Box.{ value = 0 };
    b.value = -128;
    return b.value as int;
}`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics, result := runConventionCheck(t, tc.source)
			if !result.Successful() || diagnostics.HasErrors() {
				t.Fatalf("%s: expected success but got errors: %+v", tc.name, diagnostics.Items())
			}
		})
	}
}

// TestNegatedLiteralRegressionGuards confirms that the fix does not break
// already-working cases: plain variable declarations with negated literals,
// and positive-literal index-place assignments.
func TestNegatedLiteralRegressionGuards(t *testing.T) {
	tests := []struct {
		name   string
		source string
	}{
		// Plain declaration with negated literal — already working
		{
			name: "plain-declaration-negated",
			source: `fn main() int {
    var x i32 = -1;
    return x as int;
}`,
		},
		// Positive literal index-place — already working
		{
			name: "positive-literal-index-place",
			source: `fn main() int {
    var arr [3]i32 = [1, 2, 3];
    arr[0] = 1;
    return arr[0] as int;
}`,
		},
		// Variable declaration without explicit type — defaults to int
		{
			name: "untyped-negated-literal",
			source: `fn main() int {
    let y = -1;
    return y;
}`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics, result := runConventionCheck(t, tc.source)
			if !result.Successful() || diagnostics.HasErrors() {
				t.Fatalf("%s: expected success but got errors: %+v", tc.name, diagnostics.Items())
			}
		})
	}
}
