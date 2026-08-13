package backend

import "testing"

// Nested fixed arrays [N][M]T (Phase 3 #31). The backend's array machinery
// originally assumed a scalar/aggregate element and rejected an array-of-arrays
// in every position (arrayElementCType had no isArray case). A nested array's
// element is lowered as the inner array's own pebble_array_<innerID>_t wrapper
// typedef (exactly how struct/tuple/optional elements use their own typedef
// names): a standalone nested local is a raw C array of those wrappers
// (`pebble_array_<innerID>_t pebble_local_<s>[<N>]`), and an indexed element
// whose type is itself an array projects `.data` for the next index. These
// tests prove the supported positions end-to-end.

// TestNestedArrayLocalDeclaration proves the foundational shape: a local
// declaration initialized from an array-of-arrays literal, read back element by
// element, at the default width and at a non-default element width.
func TestNestedArrayLocalDeclaration(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"local-read", "fn main() int { var a [2][3]int = [[1,2,3],[4,5,6]]; return a[1][2]; }", 6},
		{"local-read-i32", "fn main() int { var a [2][3]i32 = [[1,2,3],[4,5,6]]; return a[1][2] as int; }", 6},
		{"local-read-corner", "fn main() int { var a [2][3]int = [[1,2,3],[4,5,6]]; return a[0][0] * 100 + a[1][1]; }", 105},
		{"three-level", "fn main() int { var a [2][2][2]int = [[[1,2],[3,4]],[[5,6],[7,8]]]; return a[1][0][1] * 10 + a[0][1][0]; }", 63},
		{"whole-local-copy", "fn main() int { var b [2][3]int = [[1,2,3],[4,5,6]]; var c [2][3]int = b; return c[0][2]; }", 3},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestNestedArrayIndexingReadWrite proves element reads and writes through two
// (and more) index levels: writes route through buildPlaceLValue's `.data`
// projection for an array-typed element, reads through the same lvalue shape,
// both bounds-checked.
func TestNestedArrayIndexingReadWrite(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"write-then-read", "fn main() int { var a [2][3]int = [[1,2,3],[4,5,6]]; a[0][1] = 9; a[1][2] = 8; return a[0][1] * 10 + a[1][2]; }", 98},
		{"write-i32", "fn main() int { var a [2][3]i32 = [[1,2,3],[4,5,6]]; a[0][1] = 9; return a[0][1] as int; }", 9},
		{"write-same-element", "fn main() int { var a [2][3]int = [[1,2,3],[4,5,6]]; a[1][1] = a[1][1] + 40; return a[1][1]; }", 45},
		{"loop-sum", "fn main() int { var a [2][3]int = [[1,2,3],[4,5,6]]; var total int = 0; var i int = 0; while i < 2 { var j int = 0; while j < 3 { total = total + a[i][j]; j = j + 1; } i = i + 1; } return total; }", 21},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestNestedArrayLocalElementReference proves a nested array literal whose
// elements reference in-scope array-typed locals (`[row, row]`): a raw array
// local's bytes are copied element-by-element into the inner wrapper compound
// literal.
func TestNestedArrayLocalElementReference(t *testing.T) {
	emitAndRun(t, "fn main() int { var row [3]int = [1,2,3]; var a [2][3]int = [row, row]; return a[1][2]; }", false, 3, false)
}

// TestNestedArrayParameters proves an array-of-arrays function parameter, both
// passing an existing nested local and an inline literal, with the body reading
// nested elements.
func TestNestedArrayParameters(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"local-arg", "fn f(a [2][3]int) int { return a[1][2]; } fn main() int { var x [2][3]int = [[1,2,3],[4,5,6]]; return f(x); }", 6},
		{"inline-arg", "fn f(a [2][3]int) int { return a[0][0] + a[1][2]; } fn main() int { return f([[1,2,3],[4,5,6]]); }", 7},
		{"param-i32", "fn f(a [2][3]i32) int { return a[1][2] as int; } fn main() int { var x [2][3]i32 = [[1,2,3],[4,5,6]]; return f(x); }", 6},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestNestedArrayReturns proves an array-of-arrays return type: a direct
// literal return and a call-result-initialized local, read back element by
// element.
func TestNestedArrayReturns(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"literal-return", "fn mk() [2][3]int { return [[1,2,3],[4,5,6]]; } fn main() int { var a [2][3]int = mk(); return a[1][2]; }", 6},
		{"call-result-then-param", "fn mk() [2][3]int { return [[1,2,3],[4,5,6]]; } fn f(a [2][3]int) int { return a[0][2]; } fn main() int { var x [2][3]int = mk(); return f(x); }", 3},
		{"forwarded-return", "fn mk() [2][3]int { return [[1,2,3],[4,5,6]]; } fn fwd() [2][3]int { return mk(); } fn main() int { var a [2][3]int = fwd(); return a[0][1]; }", 2},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestNestedArrayLen proves `.len` on both the outer and the inner dimension.
// The checker folds an array `.len` to a compile-time constant (a uint), so the
// read needs the usual `as int` cast.
func TestNestedArrayLen(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"outer", "fn main() int { var a [2][3]int = [[1,2,3],[4,5,6]]; return a.len as int; }", 2},
		{"inner", "fn main() int { var a [2][3]int = [[1,2,3],[4,5,6]]; return a[1].len as int; }", 3},
		{"both-on-param", "fn f(a [2][3]int) int { return (a.len as int) * 10 + (a[1].len as int); } fn main() int { return f([[1,2,3],[4,5,6]]); }", 23},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestNestedArrayElementWidths proves nested arrays at element types beyond a
// default-width integer: bool, float, and str elements (arrayElementCType's
// existing element cases, now reachable through the nested array's own element
// chain).
func TestNestedArrayElementWidths(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"bool", "fn main() int { var a [2][2]bool = [[true,false],[false,true]]; if a[0][0] { return 5; } return 6; }", 5},
		{"bool-write", "fn main() int { var a [2][2]bool = [[true,false],[false,true]]; a[0][1] = true; if a[0][1] { return 7; } return 8; }", 7},
		{"float", "fn main() int { var a [2][2]f32 = [[1.5,2.5],[3.5,4.5]]; return a[1][0] as int; }", 3},
		{"str", "fn main() int { var a [2][2]str = [[\"a\",\"b\"],[\"c\",\"d\"]]; if a[1][1] == \"d\" { return 1; } return 2; }", 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestNestedArrayWholeStore proves whole-array reassignment of a nested array
// local from a fresh array-of-arrays literal.
func TestNestedArrayWholeStore(t *testing.T) {
	emitAndRun(t, "fn main() int { var a [2][3]int = [[1,2,3],[4,5,6]]; a = [[7,8,9],[10,11,12]]; return a[1][0] * 10 + a[0][2]; }", false, 109, false)
}

// TestNestedArrayDeferredShapesRejected pins the deliberately-out-of-scope
// shapes to their current clean rejections, so a future follow-up can loosen
// them deliberately rather than by accident.
func TestNestedArrayDeferredShapesRejected(t *testing.T) {
	cases := []struct {
		name          string
		source        string
		wantSubstring string
	}{
		// A repeat whose repeated value is itself an array literal
		// (`[[7,8,9]; 2]`) is checker-legal but not yet supported by the
		// backend's repeat-value builder.
		{"repeat-of-array", "fn main() int { var a [2][3]int = [[7,8,9]; 2]; return a[1][2]; }", "ArrayValue"},
		// Binding a whole inner array read (`let q [3]int = a[0];`) is not yet
		// supported: the local-initializer Load path only accepts a
		// DereferencePlace.
		{"whole-inner-read", "fn main() int { var a [2][3]int = [[1,2,3],[4,5,6]]; let q [3]int = a[0]; return q[2]; }", "want a DereferencePlace"},
		// An array-of-arrays struct field is rejected by the aggregate
		// nesting-depth check before the array machinery is reached.
		{"struct-field", "type Box = struct { data [2][3]int; }; fn main() int { var b Box = Box.{ data = [[1,2,3],[4,5,6]] }; return b.data[1][2]; }", "more than one level of nesting"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRunRejects(t, tc.source, tc.wantSubstring)
		})
	}
}
