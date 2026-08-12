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
