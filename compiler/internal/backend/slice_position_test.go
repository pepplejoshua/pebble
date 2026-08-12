package backend

import "testing"

// TestSliceReturningCallAsArgument proves a slice-returning call's result
// used directly as another call's argument (Phase 3 #5, buildSliceArgument's
// new DirectCall/MethodCall case).
func TestSliceReturningCallAsArgument(t *testing.T) {
	emitAndRun(t, `fn identity(s []int) []int { return s; }
fn first(s []int) int { return s[0]; }
fn main() int {
    var a [3]int = [1, 2, 3];
    var s []int = a[:];
    return first(identity(s));
}`, false, 1, false)
}

// TestSliceFieldAsReturnValue proves a slice-typed struct field read used
// directly as a return value (Phase 3 #5, buildSliceReturnValue's new
// Load(FieldPlace) case).
func TestSliceFieldAsReturnValue(t *testing.T) {
	emitAndRun(t, `type Bag = struct { items []int; };
fn view(b Bag) []int { return b.items; }
fn main() int {
    var arr [3]int = [1, 2, 3];
    var b Bag = Bag.{ items = arr[:] };
    var s []int = view(b);
    return s[1];
}`, false, 2, false)
}

// TestSliceReturningCallAsReturn proves a slice-returning call forwarded
// directly as another function's return value (Phase 3 #5,
// buildSliceReturnValue's new DirectCall case).
func TestSliceReturningCallAsReturn(t *testing.T) {
	emitAndRun(t, `fn identity(s []int) []int { return s; }
fn passthrough(s []int) []int { return identity(s); }
fn main() int {
    var a [3]int = [1, 2, 3];
    var s []int = a[:];
    var t []int = passthrough(s);
    return t[2];
}`, false, 3, false)
}

// TestStructLiteralSliceFieldInlineAsReturn proves a struct literal whose
// slice-typed field is constructed inline used directly as a return value
// (Phase 3 #5, buildStructValueExpr's statement-expression folding).
func TestStructLiteralSliceFieldInlineAsReturn(t *testing.T) {
	emitAndRun(t, `type Bag = struct { items []int; };
fn mk() Bag {
    var arr [3]int = [1, 2, 3];
    return Bag.{ items = arr[:] };
}
fn main() int {
    var b Bag = mk();
    return b.items[1];
}`, false, 2, false)
}

// TestReSliceLocalSlice proves re-slicing an existing slice-typed local, both
// with explicit bounds and with no bounds (Phase 3 #5,
// buildSliceConstruction's new slice-symbol base case).
func TestReSliceLocalSlice(t *testing.T) {
	emitAndRun(t, `fn main() int {
    var a [5]int = [1, 2, 3, 4, 5];
    var s []int = a[:];
    var t []int = s[1:3];
    return t[0];
}`, false, 2, false)
	emitAndRun(t, `fn main() int {
    var a [3]int = [1, 2, 3];
    var s []int = a[:];
    var t []int = s[:];
    return t[2];
}`, false, 3, false)
}

// TestSliceElementTypeSweep proves slice construction and index-read for
// every element type a slice-typed local can carry.
func TestSliceElementTypeSweep(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"i8", "fn main() int { var a [3]i8 = [1, 2, 3]; var s []i8 = a[:]; return s[1] as int; }", 2},
		{"i16", "fn main() int { var a [3]i16 = [1, 2, 3]; var s []i16 = a[:]; return s[1] as int; }", 2},
		{"i32", "fn main() int { var a [3]i32 = [1, 2, 3]; var s []i32 = a[:]; return s[1] as int; }", 2},
		{"i64", "fn main() int { var a [3]i64 = [1, 2, 3]; var s []i64 = a[:]; return s[1] as int; }", 2},
		{"u8", "fn main() int { var a [3]u8 = [1, 2, 3]; var s []u8 = a[:]; return s[1] as int; }", 2},
		{"u16", "fn main() int { var a [3]u16 = [1, 2, 3]; var s []u16 = a[:]; return s[1] as int; }", 2},
		{"u32", "fn main() int { var a [3]u32 = [1, 2, 3]; var s []u32 = a[:]; return s[1] as int; }", 2},
		{"u64", "fn main() int { var a [3]u64 = [1, 2, 3]; var s []u64 = a[:]; return s[1] as int; }", 2},
		{"bool", "fn main() int { var a [3]bool = [true, false, true]; var s []bool = a[:]; if s[1] { return 1; } return 0; }", 0},
		{"char", "fn main() int { var a [3]char = ['a', 'b', 'c']; var s []char = a[:]; return s[1] as int; }", 98},
		{"f32", "fn main() int { var a [3]f32 = [1.5, 2.5, 3.5]; var s []f32 = a[:]; if s[1] > 2.5 { return 1; } return 0; }", 0},
		{"f64", "fn main() int { var a [3]f64 = [1.5, 2.5, 3.5]; var s []f64 = a[:]; if s[1] > 2.5 { return 1; } return 0; }", 0},
		{"enum", "type Color = enum { red, green, blue }; fn main() int { let colors []Color = [Color.red, Color.green, Color.blue]; if colors[1] == Color.green { return 7; } return 0; }", 7},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestSliceStructAndTupleElements proves slice construction and index-read
// for struct-typed and tuple-typed elements.
func TestSliceStructAndTupleElements(t *testing.T) {
	emitAndRun(t, `type Point = struct { x int; y int; };
fn main() int {
    var a [2]Point = [Point.{ x = 1, y = 2 }, Point.{ x = 3, y = 4 }];
    var s []Point = a[:];
    return s[1].x;
}`, false, 3, false)
	emitAndRun(t, `fn main() int {
    var a [2](int, int) = [(1, 2), (3, 4)];
    var s [](int, int) = a[:];
    return s[1].0;
}`, false, 3, false)
}

// TestSliceLenDataAcrossConstructionShapes proves `.len`/`.data` access on a
// partial-range slice, a slice-typed struct field, and a slice-returning
// call's result.
func TestSliceLenDataAcrossConstructionShapes(t *testing.T) {
	emitAndRun(t, `fn main() int {
    var a [5]int = [10, 20, 30, 40, 50];
    var s []int = a[1:4];
    if s.len != 3 { return 1; }
    var p *int = s.data;
    return *p;
}`, false, 20, false)
	emitAndRun(t, `type Bag = struct { items []int; };
fn main() int {
    var a [5]int = [10, 20, 30, 40, 50];
    var b Bag = Bag.{ items = a[1:4] };
    if b.items.len != 3 { return 1; }
    if b.items.data == nil { return 2; }
    return 0;
}`, false, 0, false)
	// mk forwards its (caller-owned) parameter slice, avoiding the dangling
	// pointer a fixture returning a slice over the CALLEE's own local array
	// would produce (a separate, already-tracked escape-analysis gap).
	emitAndRun(t, `fn mk(s []int) []int {
    return s;
}
fn main() int {
    var a [3]int = [1, 2, 3];
    var base []int = a[:];
    var s []int = mk(base);
    if s.len != 3 { return 1; }
    if s.data == nil { return 2; }
    return 0;
}`, false, 0, false)
}
