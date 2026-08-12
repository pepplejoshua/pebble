package backend

import "testing"

// TestCompileRunFixedArrayStructFieldWidths proves that a bare integer-literal
// array used in a struct construction is checked and emitted at the field's
// declared element width. The non-default-width cases cover the original
// [3]i32 gap and the other fixed-width integer builtins.
func TestCompileRunFixedArrayStructFieldWidths(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"int-control", "type Box = struct { data [3]int; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; return b.data[0]; }", 1},
		{"i8", "type Box = struct { data [3]i8; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
		{"i16", "type Box = struct { data [3]i16; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
		{"i32", "type Box = struct { data [3]i32; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
		{"i64", "type Box = struct { data [3]i64; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
		{"u8", "type Box = struct { data [3]u8; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
		{"u16", "type Box = struct { data [3]u16; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
		{"u32", "type Box = struct { data [3]u32; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
		{"u64", "type Box = struct { data [3]u64; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
		{"out-of-order-fields", "type Box = struct { data [3]i32; tag int; }; fn main() int { var b Box = Box.{ tag = 7, data = [1, 2, 3] }; return b.data[2] as int + b.tag; }", 10},
		{"generic-i32", "type Box[T] = struct { data [3]T; }; fn main() int { var b Box[i32] = Box[i32].{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
		{"generic-u8", "type Box[T] = struct { data [3]T; }; fn main() int { var b Box[u8] = Box[u8].{ data = [1, 2, 3] }; return b.data[0] as int; }", 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestCompileRunFixedArraySupportedPositions proves the already-supported
// array positions exercised during the gap investigation: local literals and
// repeats, array parameters (including inline literals), array returns, and
// arrays whose elements are structs or tuples.
func TestCompileRunFixedArraySupportedPositions(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"local-i8", "fn main() int { var a [3]i8 = [1, 2, 3]; return a[0] as int; }", 1},
		{"local-u16", "fn main() int { var a [3]u16 = [1, 2, 3]; return a[0] as int; }", 1},
		{"repeat-i32", "fn main() int { var a [3]i32 = [5; 3]; return a[0] as int; }", 5},
		{"repeat-i16", "fn main() int { var a [3]i16 = [9; 3]; return a[2] as int; }", 9},
		{"repeat-u8", "fn main() int { var a [3]u8 = [7; 3]; return a[2] as int; }", 7},
		{"repeat-u64", "fn main() int { var a [3]u64 = [4; 3]; return a[2] as int; }", 4},
		{"parameter-local", "fn sum(a [3]i32) int { return a[0] as int; } fn main() int { var x [3]i32 = [1, 2, 3]; return sum(x); }", 1},
		{"parameter-inline-i8", "fn sum(a [3]i8) int { return a[0] as int; } fn main() int { return sum([1, 2, 3]); }", 1},
		{"parameter-inline-u64", "fn sum(a [3]u64) int { return a[0] as int; } fn main() int { return sum([1, 2, 3]); }", 1},
		{"return-i16", "fn mk() [3]i16 { return [1, 2, 3]; } fn main() int { var a [3]i16 = mk(); return a[0] as int; }", 1},
		{"return-i32", "fn mk() [3]i32 { return [1, 2, 3]; } fn main() int { var a [3]i32 = mk(); return a[0] as int; }", 1},
		{"return-int", "fn mk() [3]int { return [1, 2, 3]; } fn main() int { var a [3]int = mk(); return a[0]; }", 1},
		{"array-of-struct", "type Point = struct { x int; y int; }; fn main() int { var a [2]Point = [Point.{ x = 1, y = 2 }, Point.{ x = 3, y = 4 }]; return a[1].x; }", 3},
		{"array-of-tuple", "fn main() int { var a [2](i32, i32) = [(1, 2), (3, 4)]; return a[1].0 as int; }", 3},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}
