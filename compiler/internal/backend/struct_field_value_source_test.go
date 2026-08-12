package backend

import "testing"

// Phase 3 #11 — struct field read and instance method selection off a
// NON-ADDRESSABLE struct value: a call result, a nested field read, an
// array/slice/tuple element, or a force-unwrap. These lower to a TIR
// FieldValue (or route a FieldValue as a method receiver) rather than
// Load(FieldPlace), which only covers a struct LOCAL's field — a shape
// buildStructValueNode/buildStructFieldValueRead (places.go) now build
// uniformly across field reads, method-call receivers, call arguments,
// return values, and local initializers.

// TestFieldReadOffCallResult proves a struct field read off a direct call
// result (`mk().x`).
func TestFieldReadOffCallResult(t *testing.T) {
	emitAndRun(t, "type Point = struct { x i32; y i32; }; fn mk() Point { return Point.{ x = 20, y = 22 }; } fn main() i32 { return mk().x; }", false, 20, false)
}

// TestFieldReadNestedNonAddressable proves a two-level field read where the
// outer receiver is a call result (`mk().inner.x`).
func TestFieldReadNestedNonAddressable(t *testing.T) {
	emitAndRun(t, "type Inner = struct { x i32; }; type Outer = struct { inner Inner; y i32; }; fn mk() Outer { return Outer.{ inner = Inner.{ x = 7 }, y = 8 }; } fn main() i32 { return mk().inner.x; }", false, 7, false)
}

// TestFieldReadOffAggregateElement proves a struct field read off an
// array/slice element and a tuple ordinal.
func TestFieldReadOffAggregateElement(t *testing.T) {
	emitAndRun(t, "type Point = struct { x i32; y i32; }; fn main() i32 { var a [2]Point = [Point.{ x = 1, y = 2 }, Point.{ x = 3, y = 4 }]; return a[1].x; }", false, 3, false)
	emitAndRun(t, "type Point = struct { x i32; y i32; }; fn main() i32 { var s []Point = [Point.{ x = 1, y = 10 }, Point.{ x = 2, y = 20 }]; return s[1].x; }", false, 2, false)
	emitAndRun(t, "type Point = struct { x i32; y i32; }; fn main() i32 { let t (Point, Point) = (Point.{ x = 5, y = 6 }, Point.{ x = 7, y = 8 }); return t.0.x; }", false, 5, false)
}

// TestFieldReadOffForceUnwrap proves a struct field read off a force-unwrapped
// struct-payload optional (`sp!.x`).
func TestFieldReadOffForceUnwrap(t *testing.T) {
	emitAndRun(t, "type Point = struct { x i32; y i32; }; fn main() i32 { var sp ?Point = some Point.{ x = 3, y = 4 }; return sp!.x; }", false, 3, false)
}

// TestMethodCallOffNonLocalReceiver proves a method call whose receiver is a
// call result, an array/slice element, a tuple ordinal, a force-unwrap, and a
// parenthesized local — every non-addressable-or-non-simple receiver shape.
func TestMethodCallOffNonLocalReceiver(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"call-result-field", "type Point = struct { x i32; y i32; fn get(self Point) Point { return Point.{ x = 20, y = 22 }; } }; fn main() i32 { let p Point = Point.{ x = 1, y = 2 }; return p.get().x; }", 20},
		{"array-element", "type Point = struct { x i32; y i32; fn get(self Point) i32 => self.x; }; fn main() i32 { var a [2]Point = [Point.{ x = 1, y = 2 }, Point.{ x = 3, y = 4 }]; return a[1].get(); }", 3},
		{"slice-element", "type Point = struct { x i32; y i32; fn get(self Point) i32 => self.x; }; fn main() i32 { var s []Point = [Point.{ x = 1, y = 10 }, Point.{ x = 2, y = 20 }]; return s[1].get(); }", 2},
		{"force-unwrap", "type Point = struct { x i32; y i32; fn get(self Point) i32 => self.x; }; fn main() i32 { var sp ?Point = some Point.{ x = 3, y = 4 }; return sp!.get(); }", 3},
		{"tuple-ordinal", "type Point = struct { x i32; y i32; fn get(self Point) i32 => self.x; }; fn main() i32 { let t (Point, Point) = (Point.{ x = 5, y = 6 }, Point.{ x = 7, y = 8 }); return t.0.get(); }", 5},
		{"parens", "type Point = struct { x i32; y i32; fn get(self Point) i32 => self.x; }; fn main() i32 { let p Point = Point.{ x = 40, y = 2 }; return (p).get(); }", 40},
		{"call-result-field-chain", "type Inner = struct { x i32; fn get(self Inner) i32 => self.x; }; type Outer = struct { inner Inner; }; fn mk() Outer { return Outer.{ inner = Inner.{ x = 42 } }; } fn main() i32 { return mk().inner.get(); }", 42},
		{"force-unwrap-method-chain", "type Inner = struct { x i32; fn mk(self Inner) Inner { return Inner.{ x = 7 }; } }; type Outer = struct { inner Inner; }; fn main() i32 { let o Outer = Outer.{ inner = Inner.{ x = 1 } }; return o.inner.mk().x; }", 7},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestNonBoolFieldTypeReadOffCallResult proves every non-integer field type's
// read grammar handles a non-addressable struct-value receiver: bool, str,
// enum, float, pointer, and uint.
func TestNonBoolFieldTypeReadOffCallResult(t *testing.T) {
	emitAndRun(t, "type S = struct { flag bool; }; fn mk() S { return S.{ flag = true }; } fn main() i32 { if mk().flag { return 42; } return 0; }", false, 42, false)
	emitAndRun(t, "type S = struct { s str; }; fn mk() S { return S.{ s = \"hi\" }; } fn main() i32 { let s str = mk().s; if s == \"hi\" { return 42; } return 0; }", false, 42, false)
	emitAndRun(t, "type E = enum { A, B }; type S = struct { e E; }; fn mk() S { return S.{ e = .B }; } fn main() i32 { if mk().e == .B { return 42; } return 0; }", false, 42, false)
	emitAndRun(t, "type S = struct { f f32; }; fn mk() S { return S.{ f = 1.5 }; } fn main() i32 { let g f32 = mk().f; if g > 1.0 { return 42; } return 0; }", false, 42, false)
	emitAndRun(t, "type S = struct { p *i32; }; fn mk(x *i32) S { return S.{ p = x }; } fn main() i32 { var x i32 = 7; return *(mk(&x).p); }", false, 7, false)
	emitAndRun(t, "type S = struct { n uint; }; fn mk() S { return S.{ n = 7 }; } fn main() i32 { let u uint = mk().n; if u == 7 { return 42; } return 0; }", false, 42, false)
}

// TestStructuralMemberOffCallResult proves the structural members (.len,
// .data via .len here, .has_value) read correctly off a non-addressable
// slice/str/optional call result.
func TestStructuralMemberOffCallResult(t *testing.T) {
	emitAndRun(t, "fn mk() []i32 { var a [2]i32 = [1, 2]; return a[:]; } fn main() i32 { return mk().len as i32; }", false, 2, false)
	emitAndRun(t, "fn mk() str { return \"hi\"; } fn main() i32 { return mk().len as i32; }", false, 2, false)
	emitAndRun(t, "fn mk() ?i32 { return some 7; } fn main() i32 { if mk().has_value { return 42; } return 0; }", false, 42, false)
}

// TestFieldReadOffSliceCallResultElement proves a struct field read off an
// index into a slice-typed CALL RESULT (`mk()[1].x`) — the CheckedIndex-over-
// a-call-result shape buildStructValueNode folds into a statement expression.
func TestFieldReadOffSliceCallResultElement(t *testing.T) {
	emitAndRun(t, "type Point = struct { x i32; y i32; }; fn mk() []Point { var a [2]Point = [Point.{ x = 1, y = 2 }, Point.{ x = 3, y = 4 }]; return a[:]; } fn main() i32 { return mk()[1].x; }", false, 3, false)
}

// TestWholeStructFieldOffCallResult proves a whole-struct field read off a
// non-addressable receiver used as a local initializer and as a return value
// (not just a scalar field projection).
func TestWholeStructFieldOffCallResult(t *testing.T) {
	emitAndRun(t, "type Inner = struct { x i32; }; type Outer = struct { inner Inner; }; fn mk() Outer { return Outer.{ inner = Inner.{ x = 7 } }; } fn main() i32 { let i Inner = mk().inner; return i.x; }", false, 7, false)
	emitAndRun(t, "type Inner = struct { x i32; }; type Outer = struct { inner Inner; }; fn mk() Outer { return Outer.{ inner = Inner.{ x = 7 } }; } fn get() Inner { return mk().inner; } fn main() i32 { return get().x; }", false, 7, false)
}

// TestFieldWriteThroughLocalHoldingCallResult proves the (already-working,
// unaffected) write path still works once a call result is bound to a local
// first — the addressable-local write path, a regression guard alongside the
// new non-addressable read path.
func TestFieldWriteThroughLocalHoldingCallResult(t *testing.T) {
	emitAndRun(t, "type S = struct { n i32; }; fn mk() S { return S.{ n = 1 }; } fn main() i32 { var s S = mk(); s.n = 42; return s.n; }", false, 42, false)
}

// TestEmitStructParenWrappedArgumentCompilesAndRuns proves a side effect of
// buildStructValueNode's SourceAlias case: a struct constructed inline but
// wrapped in an extra set of parens as a call argument — f((Point.{ x = 1, y
// = 2 })) — now compiles and runs, because buildStructValueNode transparently
// unwraps a SourceAlias for every shape it handles (the shared builder every
// struct-typed call argument now routes through). The tuple analog
// (f(((1, 2)))) is UNCHANGED and still a clean rejection — see
// TestEmitRejectsParenWrappedAggregateArgument — since this task's fix only
// touched struct-value handling.
func TestEmitStructParenWrappedArgumentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x; } fn main() i32 { return f((Point.{ x = 1, y = 2 })); }", false, 1, false)
}
