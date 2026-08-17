package backend

import "testing"

// TestCompileRunAddressOfDereferenceWholeAggregate compiles-and-runs the
// address-of (`&x`) and dereference (`*p`) shapes applied to a WHOLE aggregate
// (a struct, tuple, array, or slice value — not a field/element reached
// through one), across every value position real source uses: a local
// declaration initializer, a call argument, a return value, a whole-value
// write, and a struct-field construction value. Most of these were closed by
// Phase 3 #3's pointer/aggregate work (pointer_probe_test.go); this file locks
// in the two genuinely distinct gaps Phase 3 #24 found and fixed — a whole
// ARRAY dereference read (`*p` where p is *[N]T) as a call argument and as an
// array-typed struct field's construction value — plus the surrounding shapes
// that must not regress.
func TestCompileRunAddressOfDereferenceWholeAggregate(t *testing.T) {
	t.Parallel()
	cases := []struct {
		name   string
		source string
		want   int
	}{
		// --- Whole array deref as a call argument (Phase 3 #24 fix) ---
		{"deref-arg-array", "fn useIt(q [2]i32) i32 { return q[0]; } fn main() i32 { var a [2]i32 = [7, 8]; let p *[2]i32 = &a; return useIt(*p); }", 7},
		{"deref-arg-array-double-indirect", "fn useIt(q [1]i32) i32 { return q[0]; } fn main() i32 { var a [1]i32 = [4]; let p *[1]i32 = &a; let pp *[1]i32 = p; return useIt(*pp); }", 4},
		{"deref-arg-array-forwarded", "fn read(p *[2]i32) [2]i32 { return *p; } fn useIt(q [2]i32) i32 { return q[1]; } fn main() i32 { var a [2]i32 = [5, 6]; let p *[2]i32 = &a; return useIt(read(p)); }", 6},
		// --- Whole array deref as an array-typed struct field's construction value (Phase 3 #24 fix) ---
		{"deref-array-field-construct", "type Box = struct { data [2]i32; }; fn main() i32 { var a [2]i32 = [1, 2]; let p *[2]i32 = &a; let b Box = Box.{ data = *p }; return b.data[1]; }", 2},
		// --- Address-of whole aggregates in value positions ---
		{"addr-struct-local-init", "type Point = struct { x i32; y i32; }; fn main() i32 { var s Point = Point.{ x = 5, y = 6 }; let p *Point = &s; return (*p).x; }", 5},
		{"addr-struct-arg", "type Point = struct { x i32; y i32; }; fn read(p *Point) i32 { return (*p).y; } fn main() i32 { var s Point = Point.{ x = 5, y = 6 }; return read(&s); }", 6},
		{"addr-tuple-local-init", "fn main() i32 { var t (i32, i32) = (5, 6); let p *(i32, i32) = &t; return (*p).0; }", 5},
		{"addr-tuple-arg", "fn read(p *(i32, i32)) i32 { return (*p).1; } fn main() i32 { var t (i32, i32) = (5, 6); return read(&t); }", 6},
		{"addr-slice-local-init", "fn main() i32 { var s []i32 = [1, 2, 3]; let p *[]i32 = &s; return (*p)[0]; }", 1},
		{"addr-slice-arg", "fn read(p *[]i32) i32 { return (*p)[1]; } fn main() i32 { var s []i32 = [1, 2, 3]; return read(&s); }", 2},
		{"addr-struct-field-construct", "type Point = struct { x i32; }; type Holder = struct { p *Point; }; fn main() i32 { var s Point = Point.{ x = 9 }; let h Holder = Holder.{ p = &s }; return (*h.p).x; }", 9},
		{"addr-array-field-construct", "type Box = struct { data [3]i32; }; type Holder = struct { p *[3]i32; }; fn main() i32 { var b Box = Box.{ data = [1, 2, 3] }; let h Holder = Holder.{ p = &b.data }; return (*h.p)[2]; }", 3},
		// --- Whole deref as whole-value operand (argument, return, local-init, write) ---
		{"deref-arg-struct", "type Point = struct { x i32; y i32; }; fn useIt(s Point) i32 { return s.y; } fn main() i32 { var s Point = Point.{ x = 1, y = 2 }; let p *Point = &s; return useIt(*p); }", 2},
		{"deref-arg-tuple", "fn useIt(q (i32, i32)) i32 { return q.1; } fn main() i32 { var t (i32, i32) = (7, 8); let p *(i32, i32) = &t; return useIt(*p); }", 8},
		{"deref-arg-slice", "fn useIt(q []i32) i32 { return q[0]; } fn main() i32 { var s []i32 = [5, 6]; let p *[]i32 = &s; return useIt(*p); }", 5},
		{"deref-arg-optional", "fn id(v ?i32) ?i32 { return v; } fn main() i32 { var o ?i32 = some 5; let p *?i32 = &o; let q ?i32 = id(*p); return q!; }", 5},
		{"deref-return-struct", "type Point = struct { x i32; y i32; }; fn read(p *Point) Point { return *p; } fn main() i32 { var s Point = Point.{ x = 5, y = 6 }; let q Point = read(&s); return q.x; }", 5},
		{"deref-return-array", "fn read(p *[2]i32) [2]i32 { return *p; } fn main() i32 { var a [2]i32 = [1, 2]; let q [2]i32 = read(&a); return q[1]; }", 2},
		{"deref-return-tuple", "fn read(p *(i32, i32)) (i32, i32) { return *p; } fn main() i32 { var t (i32, i32) = (20, 22); let q (i32, i32) = read(&t); return q.0; }", 20},
		{"deref-return-slice", "fn read(p *[]i32) []i32 { return *p; } fn main() i32 { var s []i32 = [5, 6]; let q []i32 = read(&s); return q[0]; }", 5},
		{"deref-local-init-struct", "type Point = struct { x i32; }; fn main() i32 { var s Point = Point.{ x = 3 }; let p *Point = &s; let q Point = *p; return q.x; }", 3},
		{"write-struct-whole", "type Point = struct { x i32; }; fn reset(p *Point, v Point) void { *p = v; } fn main() i32 { var s Point = Point.{ x = 1 }; let q Point = Point.{ x = 8 }; reset(&s, q); return s.x; }", 8},
		{"paren-deref-arg", "type Point = struct { x i32; }; fn useIt(s Point) i32 { return s.x; } fn main() i32 { var s Point = Point.{ x = 6 }; let p *Point = &s; return useIt((*p)); }", 6},
		{"deref-chain-arg", "type Point = struct { x i32; }; fn middle(p *Point) Point { return *p; } fn sink(s Point) i32 { return s.x; } fn main() i32 { var s Point = Point.{ x = 2 }; let p *Point = &s; return sink(middle(p)); }", 2},
		{"deref-method-receiver", "type Point = struct { x i32; fn get(self Point) i32 => self.x; }; fn main() i32 { var s Point = Point.{ x = 8 }; let p *Point = &s; return (*p).get(); }", 8},
		{"deref-index-struct", "type Point = struct { x i32; }; fn main() i32 { var a [2]Point = [Point.{ x = 1 }, Point.{ x = 9 }]; let p *[2]Point = &a; return (*p)[1].x; }", 9},
		{"deref-optional-injection", "fn useIt(o ?i32) i32 { return o!; } fn main() i32 { var o ?i32 = some 5; let p *?i32 = &o; return useIt(*p); }", 5},
		{"some-addr-of", "fn useIt(p *i32) i32 { return *p; } fn main() i32 { var x i32 = 9; let o ?*i32 = some &x; return useIt(o!); }", 9},
		// --- Address-of field/index paths off an addressable place ---
		{"addr-struct-field", "type Point = struct { x i32; y i32; }; fn main() i32 { var s Point = Point.{ x = 5, y = 6 }; let p *i32 = &s.x; return *p; }", 5},
		{"addr-array-element", "fn main() i32 { var a [3]i32 = [1, 2, 3]; let p *i32 = &a[1]; return *p; }", 2},
		{"addr-tuple-element", "fn main() i32 { var t (i32, i32) = (1, 2); let p *i32 = &t.0; return *p; }", 1},
		{"addr-slice-element", "fn main() i32 { var s []i32 = [1, 2, 3]; let p *i32 = &s[1]; return *p; }", 2},
		{"addr-field-arg", "type Point = struct { x i32; }; fn read(p *i32) i32 { return *p; } fn main() i32 { var s Point = Point.{ x = 7 }; return read(&s.x); }", 7},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, c.source, false, c.want, false)
		})
	}
}
