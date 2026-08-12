package backend

import "testing"

// TestProbeCompileRunAggregatePointerShapes compiles-and-runs every aggregate
// shape through a pointer that the probe matrix found reachable, to confirm
// the emitted C is correct (not just accepted by Emit).
func TestProbeCompileRunAggregatePointerShapes(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"read-array-local-init", "fn main() i32 { var a [3]i32 = [1, 2, 3]; let p *[3]i32 = &a; let q [3]i32 = *p; return q[0]; }", 1},
		{"read-tuple-local-init", "fn main() i32 { var t (i32, i32) = (20, 22); let p *(i32, i32) = &t; let q (i32, i32) = *p; return q.0; }", 20},
		{"read-enum-local-init", "type Color = enum { red, green, blue }; fn main() i32 { var c Color = Color.green; let p *Color = &c; let q Color = *p; if q == Color.green { return 5; } return 0; }", 5},
		{"write-array-whole", "fn reset(p *[3]i32, v [3]i32) void { *p = v; } fn main() i32 { var a [3]i32 = [1, 2, 3]; let q [3]i32 = [7, 8, 9]; reset(&a, q); return a[0]; }", 7},
		{"write-tuple-whole", "fn reset(p *(i32, i32), v (i32, i32)) void { *p = v; } fn main() i32 { var t (i32, i32) = (1, 2); let q (i32, i32) = (7, 8); reset(&t, q); return t.0; }", 7},
		{"write-enum-whole", "type Color = enum { red, green, blue }; fn reset(p *Color, v Color) void { *p = v; } fn main() i32 { var c Color = Color.red; var g Color = Color.green; reset(&c, g); if c == Color.green { return 5; } return 0; }", 5},
		{"addr-array-local-init", "fn main() i32 { var a [3]i32 = [1, 2, 3]; let p *[3]i32 = &a; return (*p)[0]; }", 1},
		{"addr-array-arg", "fn read(p *[3]i32) i32 { return (*p)[1]; } fn main() i32 { var a [3]i32 = [1, 2, 3]; return read(&a); }", 2},
		{"addr-array-field", "type Box = struct { data [3]int; }; fn main() int { var b Box = Box.{ data = [1, 2, 3] }; let p *[3]int = &b.data; return (*p)[2]; }", 3},
		{"nil-cmp-array", "fn mk() *[3]i32 { let p *[3]i32 = nil; return p; } fn main() i32 { if mk() == nil { return 5; } return 0; }", 5},
		{"eq-array", "fn main() i32 { var a [3]i32 = [1, 2, 3]; var b [3]i32 = [1, 2, 3]; let p *[3]i32 = &a; let q *[3]i32 = &b; if p == q { return 1; } return 5; }", 5},
		{"deref-return-array", "fn read(p *[3]i32) [3]i32 { return *p; } fn main() i32 { var a [3]i32 = [1, 2, 3]; let q [3]i32 = read(&a); return q[0]; }", 1},
		{"deref-return-tuple", "fn read(p *(i32, i32)) (i32, i32) { return *p; } fn main() i32 { var t (i32, i32) = (20, 22); let q (i32, i32) = read(&t); return q.0; }", 20},
		{"deref-return-enum", "type Color = enum { red, green, blue }; fn read(p *Color) Color { return *p; } fn main() i32 { var c Color = Color.green; let q Color = read(&c); if q == Color.green { return 5; } return 0; }", 5},
		{"deref-return-struct", "type Point = struct { x int; y int; }; fn read(p *Point) Point { return *p; } fn main() int { var p = Point.{ x = 5, y = 6 }; let q Point = read(&p); return q.x; }", 5},
		{"deref-arg-optional", "fn id(v ?i32) ?i32 { return v; } fn main() i32 { var o ?i32 = some 5; let p *?i32 = &o; let q ?i32 = id(*p); return q!; }", 5},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			emitAndRun(t, c.source, false, c.want, false)
		})
	}
}
