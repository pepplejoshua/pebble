package backend

import "testing"

// TestOptionalArrayPayloadShapes proves the array-typed optional payload
// `?[N]T` end to end (Phase 3 #7): construction from an array literal
// (`some [1, 2, 3]`), from a reference to an array-typed local, and from
// `none`, in every position — a local declaration, an optional-returning
// helper, an optional parameter, and a re-assignment — plus the presence
// member read and a force-unwrap (`o!`) used both as an array local's
// declaration initializer and as the base of an index read (`o![0]`).
// Every case runs through the full compile-link-run harness. Exit code 0
// means the round-trip was correct; any other code is returned by a
// deliberate wrong-branch or wrong-value check.
func TestOptionalArrayPayloadShapes(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"some-literal-unwrap-index", "fn main() int { var o ?[3]i32 = some [1, 2, 3]; if o.has_value { return o![0] as int; } return 0; }", 1},
		{"some-literal-unwrap-element2", "fn main() int { var o ?[3]i32 = some [5, 6, 7]; if o.has_value { return o![2] as int; } return 0; }", 7},
		{"some-local-unwrap-local", "fn main() int { var a [3]i32 = [1, 2, 3]; var o ?[3]i32 = some a; if o.has_value { var t [3]i32 = o!; return t[0] as int; } return 0; }", 1},
		{"none-has-value-false", "fn main() int { var o ?[3]i32 = none; if o.has_value { return 1; } return 0; }", 0},
		{"return-some", "fn mk() ?[3]i32 { return some [1, 2, 3]; } fn main() int { var o ?[3]i32 = mk(); if o.has_value { return o![0] as int; } return 0; }", 1},
		{"return-none", "fn mk() ?[3]i32 { return none; } fn main() int { var o ?[3]i32 = mk(); if o.has_value { return 1; } return 0; }", 0},
		{"argument", "fn g(o ?[3]i32) int { if o.has_value { return o![0] as int; } return 0; } fn main() int { return g(some [1, 2, 3]); }", 1},
		{"argument-none", "fn g(o ?[3]i32) int { if o.has_value { return 1; } return 0; } fn main() int { return g(none); }", 0},
		{"reassign", "fn main() int { var o ?[3]i32 = none; o = some [1, 2, 3]; if o.has_value { return o![0] as int; } return 0; }", 1},
		{"narrow-element-width", "fn main() int { var o ?[3]i8 = some [7, 8, 9]; if o.has_value { return o![1] as int; } return 0; }", 8},
		{"uint-element-width", "fn main() int { var o ?[3]u16 = some [3, 4, 5]; if o.has_value { return o![2] as int; } return 0; }", 5},
		{"bool-elements", "fn main() int { var o ?[2]bool = some [true, false]; if o.has_value { if o![0] && !o![1] { return 42; } } return 0; }", 42},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestOptionalArrayPayloadUnwrapNoneAborts proves that force-unwrapping an
// absent array-payload optional panics via the presence-only runtime check
// (pebble_rt_checked_unwrap_present), aborting the process exactly like every
// scalar-payload absent unwrap.
func TestOptionalArrayPayloadUnwrapNoneAborts(t *testing.T) {
	emitAndRun(t, "fn main() int { var o ?[3]i32 = none; return o![0] as int; }", false, 0, true)
}

// TestOptionalArrayPayloadConstructedFromLocal proves an array-typed optional
// constructed from a reference to an array-typed local whose unwrapped value
// round-trips through a helper that reads a specific element.
func TestOptionalArrayPayloadConstructedFromLocal(t *testing.T) {
	emitAndRun(t, `fn first(o ?[3]int) int { if o.has_value { return o![1]; } return 0; }
fn main() int { var a [3]int = [5, 6, 7]; return first(some a); }`, false, 6, false)
}

// TestOptionalSlicePayloadShapes proves the slice-typed optional payload `?[]T`
// end to end (Phase 3 #7): construction from a reference to a slice-typed
// local (`some s`), from a fresh checked slice (`some a[:]`), and from `none`,
// in every position — a local declaration, an optional-returning helper, an
// optional parameter, and a re-assignment — plus the presence member read and
// a force-unwrap (`o!`) into a slice-typed local. Every case runs through the
// full compile-link-run harness.
func TestOptionalSlicePayloadShapes(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"some-local-unwrap-local", "fn main() int { var a [3]int = [1, 2, 3]; var s []int = a[:]; var o ?[]int = some s; if o.has_value { var t []int = o!; return t[0]; } return 0; }", 1},
		{"some-construct-unwrap-local", "fn main() int { var a [3]int = [1, 2, 3]; var o ?[]int = some a[:]; if o.has_value { var t []int = o!; return t[2]; } return 0; }", 3},
		{"none-has-value-false", "fn main() int { var o ?[]int = none; if o.has_value { return 1; } return 0; }", 0},
		{"return-some", "fn mk() ?[]int { var a [3]int = [1, 2, 3]; return some a[:]; } fn main() int { var o ?[]int = mk(); if o.has_value { var t []int = o!; return t[1]; } return 0; }", 2},
		{"return-none", "fn mk() ?[]int { return none; } fn main() int { var o ?[]int = mk(); if o.has_value { return 1; } return 0; }", 0},
		{"argument", "fn g(o ?[]int) int { if o.has_value { var t []int = o!; return t[0]; } return 0; } fn main() int { var a [3]int = [1, 2, 3]; return g(some a[:]); }", 1},
		{"argument-none", "fn g(o ?[]int) int { if o.has_value { return 1; } return 0; } fn main() int { return g(none); }", 0},
		{"reassign", "fn main() int { var a [3]int = [1, 2, 3]; var o ?[]int = none; o = some a[:]; if o.has_value { var t []int = o!; return t[2]; } return 0; }", 3},
		{"unwrapped-index-base", "fn main() int { var a [3]int = [1, 2, 3]; var s []int = a[:]; var o ?[]int = some s; if o.has_value { return o![1]; } return 0; }", 2},
		{"narrow-element-width", "fn main() int { var a [3]i16 = [4, 5, 6]; var s []i16 = a[:]; var o ?[]i16 = some s; if o.has_value { var t []i16 = o!; return t[2] as int; } return 0; }", 6},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestOptionalSlicePayloadUnwrapNoneAborts proves that force-unwrapping an
// absent slice-payload optional panics via the presence-only runtime check,
// aborting the process.
func TestOptionalSlicePayloadUnwrapNoneAborts(t *testing.T) {
	emitAndRun(t, "fn main() int { var o ?[]int = none; var t []int = o!; return 0; }", false, 0, true)
}
