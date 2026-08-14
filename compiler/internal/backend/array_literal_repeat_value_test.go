package backend

import "testing"

// Regression coverage for Phase 3 #18 (array literal/repeat value-shape
// gaps). Three real, distinct gaps were fixed:
//
//  1. `buildArrayArgument`'s ArrayRepeat case (calls.go) was missing
//     entirely — `[v; N]` as a call argument was rejected.
//  2. `buildStructArrayFieldValue`'s ArrayRepeat case (aggregates.go) was
//     missing — `S.{ data = [v; N] }` was rejected, plus both call sites
//     gained an aggregate-repeated-value branch (`[Point.{...}; N]`) via
//     `buildNestedAggregateValue`.
//  3. The checker's `finishArray`/`finishArrayRepeat` (aggregate_facts.go)
//     forced a hard `Equal` between an array/repeat element and the KNOWN
//     destination element type, wrongly rejecting a same-width-but-
//     distinct-kind value (an `int` call result into `[N]i32`/`[N]i64`)
//     that a plain scalar local initializer already accepts through the
//     ordinary compatibility record.

func TestArrayRepeatAsCallArgumentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn sum(a [3]i32) int { return a[2] as int; } fn main() int { return sum([7; 3]); }", false, 7, false)
}

func TestArrayRepeatFloatAsCallArgumentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn sum(a [3]f32) int { return a[2] as int; } fn main() int { return sum([2.0; 3]); }", false, 2, false)
}

func TestArrayRepeatCallValueAsCallArgumentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn f() int { return 11; } fn sum(a [3]i32) int { return a[2] as int; } fn main() int { return sum([f(); 3]); }", false, 11, false)
}

func TestArrayRepeatAsStructFieldValueCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Box = struct { data [3]i32; }; fn main() int { var b Box = Box.{ data = [7; 3] }; return b.data[2] as int; }", false, 7, false)
}

func TestArrayRepeatGenericStructFieldValueCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Box[T] = struct { data [3]T; }; fn main() int { var b Box[i32] = Box[i32].{ data = [7; 3] }; return b.data[2] as int; }", false, 7, false)
}

func TestArrayRepeatOfStructLiteralCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Point = struct { x int; y int; }; fn main() int { var a [2]Point = [Point.{ x = 1, y = 2 }; 2]; return a[1].y; }", false, 2, false)
}

func TestArrayRepeatOfTupleLiteralCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() int { var a [2](i32, i32) = [(1, 2); 2]; return a[1].0 as int; }", false, 1, false)
}

func TestArrayLiteralCallResultElementsAtNarrowWidthCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn f() int { return 11; } fn main() int { var a [3]u8 = [f(), f(), f()]; return a[2] as int; }", false, 11, false)
}

func TestArrayLiteralCallResultElementsAtWideWidthCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn f() int { return 11; } fn main() int { var a [3]i64 = [f(), f(), f()]; return a[2] as int; }", false, 11, false)
}

func TestArrayLiteralCallResultElementsAsCallArgumentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn f() int { return 11; } fn sum(a [3]i32) int { return a[1] as int; } fn main() int { return sum([f(), f(), f()]); }", false, 11, false)
}

// F5-10: struct ArrayRepeat as call argument — `[Point.{ x = 1, y = 2 }; 3]`
// into a `[3]Point` parameter. The temp declaration is pebble_repeat_arg_<argID>
// of the struct's typedef type, and the compound literal repeats that temp name.
func TestStructArrayRepeatAsCallArgumentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Point = struct { x int; y int; }; fn takes(pts [3]Point) int { return pts[0].x; } fn main() int { return takes([Point.{ x = 1, y = 2 }; 3]); }", false, 1, false)
}

// F5-10: struct ArrayRepeat as call argument — verify all elements get the
// correct value (not just element 0). Each repeated struct has its own field
// values propagated through the array.
func TestStructArrayRepeatAsCallArgumentAllElements(t *testing.T) {
	emitAndRun(t, "type Point = struct { x int; y int; }; fn takes(pts [3]Point) int { return pts[0].x + pts[1].y + pts[2].x; } fn main() int { return takes([Point.{ x = 10, y = 20 }; 3]); }", false, 40, false)
}

// F5-10: full struct ArrayValue literal as call argument — `[Point.{...}, Point.{...}]`
// into a `[2]Point` parameter. Each struct element goes through buildNestedAggregateValue.
func TestStructArrayValueAsCallArgumentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Point = struct { x int; y int; }; fn takes(pts [2]Point) int { return pts[0].x + pts[1].y; } fn main() int { return takes([Point.{ x = 3, y = 4 }, Point.{ x = 5, y = 6 }]); }", false, 9, false)
}

// F5-10: struct with 2+ fields confirming field values propagate correctly
// through the array when passed as a call argument via ArrayRepeat.
func TestStructArrayRepeatMultiFieldAsCallArgument(t *testing.T) {
	emitAndRun(t, "type RGB = struct { r i32; g i32; b i32; }; fn colorSum(c [2]RGB) i32 { return c[0].r + c[0].g + c[0].b + c[1].r + c[1].g + c[1].b; } fn main() i32 { return colorSum([RGB.{ r = 1, g = 2, b = 3 }, RGB.{ r = 4, g = 5, b = 6 }]); }", false, 21, false)
}

// F5-10: evaluate-once property for struct ArrayRepeat — the repeated value
// expression must be evaluated exactly once, not N times. A helper that
// constructs a Point increments a global counter each time it runs; the exit
// code proves it was called exactly once.
func TestStructArrayRepeatEvaluateOnce(t *testing.T) {
	emitAndRun(t, "type Point = struct { x int; y int; }; var count int = 0; fn mkPoint() Point { count = count + 1; return Point.{ x = 1, y = 2 }; } fn takes(pts [3]Point) int { return pts[0].x; } fn main() int { var _ = takes([mkPoint(); 3]); return count; }", false, 1, false)
}

// F5-11: struct ArrayRepeat as return value — `return [Point.{ x = 1, y = 2 }; 3];`
// in a `[3]Point`-returning function. The temp declaration is pebble_repeat_ret_<nodeID>
// of the struct's typedef type, and the compound literal repeats that temp name.
func TestStructArrayRepeatAsReturnValueCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type Point = struct { x int; y int; }; fn makeAll() [3]Point { return [Point.{ x = 1, y = 2 }; 3]; } fn main() int { var pts [3]Point = makeAll(); return pts[0].x; }", false, 1, false)
}

// F5-11: struct ArrayRepeat as return value — verify all elements get the
// correct value (not just element 0). Each repeated struct has its own field
// values propagated through the array.
func TestStructArrayRepeatAsReturnValueAllElements(t *testing.T) {
	emitAndRun(t, "type Point = struct { x int; y int; }; fn makeAll() [3]Point { return [Point.{ x = 10, y = 20 }; 3]; } fn main() int { var pts [3]Point = makeAll(); return pts[0].x + pts[1].y + pts[2].x; }", false, 40, false)
}

// F5-11: struct with 2+ fields confirming field values propagate correctly
// through the array when returned via ArrayRepeat.
func TestStructArrayRepeatMultiFieldAsReturnValue(t *testing.T) {
	emitAndRun(t, "type RGB = struct { r i32; g i32; b i32; }; fn makeAll() [2]RGB { return [RGB.{ r = 1, g = 2, b = 3 }; 2]; } fn main() i32 { var c [2]RGB = makeAll(); return c[0].r + c[0].g + c[0].b + c[1].r + c[1].g + c[1].b; }", false, 12, false)
}

// F5-11: evaluate-once property for struct ArrayRepeat as return value — the
// repeated value expression must be evaluated exactly once, not N times. A
// helper that constructs a Point increments a global counter each time it
// runs; the exit code proves it was called exactly once.
func TestStructArrayRepeatAsReturnValueEvaluateOnce(t *testing.T) {
	emitAndRun(t, "type Point = struct { x int; y int; }; var count int = 0; fn mkPoint() Point { count = count + 1; return Point.{ x = 1, y = 2 }; } fn makeAll() [3]Point { return [mkPoint(); 3]; } fn main() int { var _ [3]Point = makeAll(); return count; }", false, 1, false)
}
