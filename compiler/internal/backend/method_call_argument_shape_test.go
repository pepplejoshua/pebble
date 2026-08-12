package backend

import "testing"

// Regression coverage for Phase 3 #17 (method call argument-shape gaps).
// callMember (internal/infer/instantiate.go) now grounds a method-call
// argument that is still an unresolved aggregate literal (array, tuple, or
// `some`) to the method's concrete parameter type, mirroring the walk-time
// KNOWN-parameter anchor a direct call already gets. Without this, these
// three literal shapes failed at the checker (C0601) or at emit, while
// working fine as a plain function call argument.

func TestMethodCallInlineTupleLiteralArgCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type S = struct { n i32; fn f(self S, t (i32, i32)) i32 => t.0 + t.1; }; fn main() i32 { let s S = S.{ n = 0 }; return s.f((40, 2)); }", false, 42, false)
}

func TestMethodCallInlineArrayLiteralArgCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type S = struct { n i32; fn f(self S, a [3]i32) i32 => a[0] + a[1] + a[2]; }; fn main() i32 { let s S = S.{ n = 0 }; return s.f([40, 1, 1]); }", false, 42, false)
}

func TestMethodCallSomeLiteralOptionalArgCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type S = struct { n i32; fn f(self S, o ?i32) i32 => o! + 1; }; fn main() i32 { let s S = S.{ n = 0 }; return s.f(some 41); }", false, 42, false)
}

func TestMethodCallStructArgFromCallResultCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Point = struct { x i32; }; type S = struct { n i32; fn f(self S, p Point) i32 => p.x; }; fn mk() Point { return Point.{ x = 42 }; } fn main() i32 { let s S = S.{ n = 0 }; return s.f(mk()); }", false, 42, false)
}

func TestMethodCallStructArgFromFieldReadCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Point = struct { x i32; }; type S = struct { n i32; fn f(self S, p Point) i32 => p.x; }; type Holder = struct { p Point; }; fn main() i32 { let s S = S.{ n = 0 }; let h Holder = Holder.{ p = Point.{ x = 42 } }; return s.f(h.p); }", false, 42, false)
}

func TestMethodCallTupleArgFromCallResultCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type S = struct { n i32; fn f(self S, t (i32, i32)) i32 => t.0 + t.1; }; fn mk() (i32, i32) { return (40, 2); } fn main() i32 { let s S = S.{ n = 0 }; return s.f(mk()); }", false, 42, false)
}

func TestMethodCallGenericWithStructArgCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Point = struct { x i32; }; type Box[K] = struct { value K; fn get[K](self Box[K], p Point) i32 { return p.x; } }; fn main() i32 { var b Box[int] = Box[int].{ value = 5 }; return b.get(Point.{ x = 7 }); }", false, 7, false)
}

func TestMethodCallGenericWithTupleArgCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Box[K] = struct { value K; fn get[K](self Box[K], t (i32, i32)) i32 { return t.0 + t.1; } }; fn main() i32 { var b Box[int] = Box[int].{ value = 5 }; return b.get((40, 2)); }", false, 42, false)
}

func TestMethodCallReceiverAndArgBothNonTrivialCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Point = struct { x i32; y i32; fn add(self Point, other Point) i32 => self.x + other.y; }; fn mk() Point { return Point.{ x = 20, y = 1 }; } fn mk2() Point { return Point.{ x = 1, y = 22 }; } fn main() i32 { return mk().add(mk2()); }", false, 42, false)
}

func TestMethodCallSliceArgFromLocalCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type S = struct { n i32; fn f(self S, sl []i32) i32 => sl.len as i32; }; fn main() i32 { var a [3]i32 = [1, 2, 3]; var sl []i32 = a[0:3]; let s S = S.{ n = 0 }; return s.f(sl); }", false, 3, false)
}
