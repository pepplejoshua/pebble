package backend

import (
	"bytes"
	"fmt"
	"regexp"
	"strings"
	"testing"
)

func TestEmitIntegerReturnEntryWritesC(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{"pebble_rt.h", "pebble_rt_default_context", "return 0;", "static int pebble_user_main(PebbleContext *ctx)"} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "static void pebble_user_main") {
		t.Errorf("emitted C still declares pebble_user_main returning void, want int:\n%s", out)
	}
}

func TestEmitIntegerReturnEntryCompilesAndRunsExitCode42(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 42; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitIfElseEntryWritesC(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { if 1 < 2 { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_rt.h",
		"static int pebble_user_main(PebbleContext *ctx)",
		"if (1 < 2) {",
		"        return 10;",
		"    } else {",
		"        return 20;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitIfElseComparisonOperators(t *testing.T) {
	// All six comparison operators, each taking the branch matching its value;
	// both true and false outcomes are covered across the set so the emitter is
	// not silently only ever emitting one branch. The required shapes `1 < 2`
	// (true, exit 10) and `5 == 6` (false, exit 20) are in this table.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"less true", "fn main() i32 { if 1 < 2 { return 10; } else { return 20; } }", 10},
		{"equal false", "fn main() i32 { if 5 == 6 { return 10; } else { return 20; } }", 20},
		{"lessEqual true", "fn main() i32 { if 2 <= 2 { return 30; } else { return 40; } }", 30},
		{"lessEqual false", "fn main() i32 { if 3 <= 2 { return 30; } else { return 40; } }", 40},
		{"greater true", "fn main() i32 { if 3 > 2 { return 50; } else { return 60; } }", 50},
		{"greater false", "fn main() i32 { if 1 > 2 { return 50; } else { return 60; } }", 60},
		{"notEqual true", "fn main() i32 { if 1 != 2 { return 70; } else { return 80; } }", 70},
		{"notEqual false", "fn main() i32 { if 1 != 1 { return 70; } else { return 80; } }", 80},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitLogicalIfElseCompilesAndRuns(t *testing.T) {
	// && and || as an if condition are now supported: both lower to a
	// tir.ShortCircuitValue node (a different kind than the BinaryValue
	// comparison 10.7 handled), whose operands are themselves built through
	// buildBoolExpr. This table covers both operators with a true and a false
	// outcome each, so both directions are exercised. The `and true` row is
	// exactly the fixture 10.7/10.11 rejected — now the new positive case.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"and true", "fn main() i32 { if 1 < 2 && 3 < 4 { return 1; } else { return 2; } }", 1},
		{"and false", "fn main() i32 { if 1 < 2 && 5 < 4 { return 1; } else { return 2; } }", 2},
		{"or true", "fn main() i32 { if 1 < 2 || 3 < 4 { return 1; } else { return 2; } }", 1},
		{"or false", "fn main() i32 { if 5 < 4 || 1 > 2 { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitNestedIfDiamondCompilesAndRuns(t *testing.T) {
	// A nested if inside an arm (a "diamond"): the then-arm's block is itself
	// a two-armed if/else under the same recursive grammar, so buildBlock
	// recurses a second level. Three variants cover the inner-true (exit 1),
	// inner-false (exit 2), and outer-false (exit 3) paths. This is exactly
	// the shape 10.7 rejected as "nested if in an arm", now accepted.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"inner true", "fn main() i32 { if 1 < 2 { if 3 < 4 { return 1; } else { return 2; } } else { return 3; } }", 1},
		{"inner false", "fn main() i32 { if 1 < 2 { if 5 < 4 { return 1; } else { return 2; } } else { return 3; } }", 2},
		{"outer false", "fn main() i32 { if 2 < 1 { if 3 < 4 { return 1; } else { return 2; } } else { return 3; } }", 3},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitNestedIfEntryWritesC(t *testing.T) {
	// The emitted C for a nested if must indent each level correctly so the
	// output is well-formed, readable C: the outer if is at the top level
	// (4 spaces), the nested if inside the then-arm one level deeper (8
	// spaces), and its returns two levels deep (12 spaces). Asserting the
	// literal indentation is what stops the recursive build from quietly
	// collapsing all levels onto one.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { if 1 < 2 { if 3 < 4 { return 1; } else { return 2; } } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    if (1 < 2) {\n",
		"        if (3 < 4) {\n",
		"            return 1;",
		"        } else {",
		"            return 2;",
		"        }",
		"    } else {",
		"        return 3;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitWhileAccumulationCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: a real accumulation loop. i counts 0..4 and
	// sum accumulates i each pass, so sum = 0+1+2+3+4 = 10, returned as the
	// process exit code. This is the first program in the rewrite where a
	// loop actually iterates and accumulates across iterations. Execution is
	// bounded (compileAndRunBounded) so a miscompiled non-terminating loop
	// fails the test loudly instead of hanging it.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { sum = sum + i; i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitWhileNeverRunsCompilesAndRuns(t *testing.T) {
	// A loop whose condition is false before the first iteration: i = 10 is
	// not < 5, so the body never runs and x keeps its initial value 1. This
	// proves the emitted while does not run its body even once when the
	// condition is false at entry. Bounded execution in case of a
	// miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 10; var x i32 = 1; while i < 5 { x = 2; } return x; }", false, 1, false)
}

func TestEmitWhileCounterCompilesAndRuns(t *testing.T) {
	// A simple counter with no accumulator, to isolate the loop mechanism
	// from the accumulation pattern: i goes 0 -> 1 -> 2 -> 3, then i < 3 is
	// false and the loop exits, returning 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; } return i; }", false, 3, false)
}

func TestEmitWhileOverflowInBodyAborts(t *testing.T) {
	// Overflow must still be checked inside a loop body, not just in straight-
	// line code: x starts at 2147483640 and is incremented each of the ten
	// iterations, overflowing on the eighth (2147483647 + 1). The emitted
	// pebble_rt_checked_add_i32 call inside the loop must panic through
	// pebble_rt_panic partway through the loop, so the process terminates
	// abnormally — not exit 0, not return a silently wrapped value, and not a
	// hang. Because the program runs in bounded execution, reaching this
	// assert proves the abnormal termination is the genuine overflow abort and
	// not the bounded harness confusing it with a timeout.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 2147483640; var i i32 = 0; while i < 10 { x = x + 1; i = i + 1; } return x; }", false, 0, true)
}

func TestEmitTerminalWhileTrueEntryCompilesAndRuns(t *testing.T) {
	// A non-void entry whose body ends directly in an exhaustive `while true`
	// is now a supported tail shape: the checker accepts a constant-true loop
	// with no break (control can never fall through past it), and the IR
	// builder omits the ImplicitReturn tail for a non-void callable, so the
	// entry body's final child is the raw While. The backend must lower that
	// loop via the ordinary buildWhile path with no synthetic return — every
	// exit from the loop is a return, so the C function never falls off the
	// end. The loop's first (and only) pass returns 42 as the exit code.
	emitAndRunBounded(t, "fn main() i32 { while true { return 42; } }", false, 42, false)
}

func TestEmitTerminalWhileTrueConditionalReturnCompilesAndRuns(t *testing.T) {
	// The exact std/hmap get_by_ref / remove shape: a `while true` whose body
	// is a conditional if whose arm returns, with no break anywhere. The loop
	// never falls through (every iteration either returns from the if arm or
	// loops again), so the non-void helper needs no trailing return after the
	// loop. Calling f(5) returns on the first pass with x = 5.
	emitAndRunBounded(t, "fn f(x i32) i32 { while true { if x == 5 { return x; } } } fn main() i32 { return f(5); }", false, 5, false)
}

func TestEmitIfElseInsideLoopBodyCompilesAndRuns(t *testing.T) {
	// The fixture 10.10 used to prove an if inside a while body was rejected
	// is now a supported shape (10.11 widens the loop-body grammar to include
	// if/else, following the same 10.8 precedent of a now-supported shape
	// becoming the new positive case). It is also the "break out early" pattern
	// 10.11 exists for: i reaches 2, the if short-circuits the loop by jumping
	// i to 10, and the loop exits — return 10 as the exit code. Bounded
	// execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 5 { if i == 2 { i = 10; } else { i = i + 1; } } return i; }", false, 10, false)
}

func TestEmitPrintInsideWhileBodyCompilesAndRuns(t *testing.T) {
	// A print statement inside a loop body (legal source) is now a supported
	// statement kind: the loop body's statement switch routes it through the
	// shared buildPrint, so the body prints "hi" once per iteration (three
	// iterations) and returns the final i = 3 as the exit code. Bounded
	// execution in case of a miscompiled loop.
	out := emitAndRunCaptureBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { print(\"hi\"); i = i + 1; } return i; }", false, 3, false)
	if want := "hi\nhi\nhi\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitNoElseIfInLoopBodyCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: an if with no else inside a loop body. i counts
	// 0..9 but sum accumulates only while i < 5, so sum = 0+1+2+3+4 = 10,
	// returned as the process exit code. The no-else If is exactly the
	// two-child shape confirmed against a real fixture dump (condition,
	// then-arm, no third child), and the emitter must produce an if block with
	// no else. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i < 5 { sum = sum + i; } i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitNoElseIfInLoopBodyWritesC(t *testing.T) {
	// The emitted C for a no-else if inside a loop body must mirror buildIf's
	// indentation style: the while at the top level (4 spaces), the if one
	// level deeper (8 spaces), its store two levels deep (12 spaces), and the
	// if closed with no `else`. Asserting the literal indentation is what stops
	// the recursive build from quietly collapsing all levels onto one.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i < 5 { sum = sum + i; } i = i + 1; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    while (pebble_local_27 < 10) {\n",
		"        if (pebble_local_27 < 5) {\n",
		"            pebble_local_28 = pebble_rt_checked_add_i32(pebble_local_28, pebble_local_27, (PebbleSourceLoc){\"main.peb\", 1, 81});",
		"        }",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "        } else {") {
		t.Errorf("emitted C contains an else for a no-else if:\n%s", out)
	}
}

func TestEmitIfElseInLoopBodyEvenOddCompilesAndRuns(t *testing.T) {
	// A loop-body if with an else, both arms accumulating into distinct
	// enclosing locals: i counts 0..5, the then-arm counts evens (0, 2, 4) and
	// the else-arm counts odds (1, 3, 5), so even = 3 returned as the exit
	// code. The condition uses a checked modulo (`i % 2 == 0`), confirming
	// overflow-checked arithmetic is valid inside a loop-body if condition.
	// Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var even i32 = 0; var odd i32 = 0; while i < 6 { if i % 2 == 0 { even = even + 1; } else { odd = odd + 1; } i = i + 1; } return even; }", false, 3, false)
}

func TestEmitNestedWhileInLoopBodyCompilesAndRuns(t *testing.T) {
	// The nested double-loop confirmation fixture: i and j each count 0..2, so
	// the inner body runs 3 x 3 = 9 times and total = 9, returned as the exit
	// code. The inner While is a plain statement inside the outer loop's body
	// Block (the shape confirmed against a real fixture dump), and buildWhile
	// recurses into buildLoopBody for its own body unchanged. Bounded
	// execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { var j i32 = 0; while j < 3 { total = total + 1; j = j + 1; } i = i + 1; } return total; }", false, 9, false)
}

func TestEmitOverflowInsideNestedLoopIfAborts(t *testing.T) {
	// Overflow must still be checked deep inside nested control flow, not just
	// in straight-line loop bodies: x starts at 2147483640 and is incremented
	// inside a loop-body if nested inside the inner of two loops. Each of the
	// 3x3 = 9 iterations increments x (the if's condition 1 < 2 is always
	// true), overflowing on the eighth (2147483647 + 1). The emitted
	// pebble_rt_checked_add_i32 call must panic through pebble_rt_panic
	// partway through the nested loop, so the process terminates abnormally —
	// not exit 0, not return a silently wrapped value, and not a hang. Because
	// the program runs in bounded execution, reaching this assert proves the
	// abnormal termination is the genuine overflow abort and not the bounded
	// harness confusing it with a timeout.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 2147483640; var i i32 = 0; while i < 3 { var j i32 = 0; while j < 3 { if 1 < 2 { x = x + 1; } j = j + 1; } i = i + 1; } return x; }", false, 0, true)
}

func TestEmitLogicalAndWhileCompilesAndRuns(t *testing.T) {
	// && as a while condition, the shape 10.11 rejected: i counts 1..4 while
	// both sides hold (i < 5 && i > 0), then at i = 5 the left side fails and
	// the loop exits with i = 5 as the exit code. Bounded execution in case of
	// a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 1; while i < 5 && i > 0 { i = i + 1; } return i; }", false, 5, false)
}

func TestEmitLogicalOrWhileCompilesAndRuns(t *testing.T) {
	// || as a while condition: i counts 0..4 through the left side (i < 5),
	// then at i = 5 both sides are false (i < 5 || i == 10) and the loop
	// exits with i = 5. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 5 || i == 10 { i = i + 1; } return i; }", false, 5, false)
}

func TestEmitBoolWhileNegationLoopCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: a bool accumulator flag driving a while loop.
	// done starts false, so while !done runs; each pass sums i and increments
	// it, and when i == 5 the if sets done = true, exiting the loop with sum =
	// 0+1+2+3+4 = 10 as the exit code. This exercises a bare !-negated bool
	// local as a while condition (a tir.PrefixValue with the Bang operator)
	// and a Store reassigning a bool local inside a loop-body if. Bounded
	// execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var done bool = false; var i i32 = 0; var sum i32 = 0; while !done { sum = sum + i; i = i + 1; if i == 5 { done = true; } } return sum; }", false, 10, false)
}

func TestEmitBoolWhileNegationLoopWritesC(t *testing.T) {
	// The emitted C for the !done loop must declare the bool flag, negate it
	// with plain C ! in the while condition, and reassign it with a plain bool
	// literal inside the loop-body if. Symbols 25 (done), 26 (i), and 27 (sum)
	// come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var done bool = false; var i i32 = 0; var sum i32 = 0; while !done { sum = sum + i; i = i + 1; if i == 5 { done = true; } } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"bool pebble_local_27 = false;",
		"    while (!(pebble_local_27)) {\n",
		"        if (pebble_local_28 == 5) {\n",
		"            pebble_local_27 = true;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_rt_checked_neg_i32") {
		t.Errorf("emitted C used the integer checked-negate helper for a bool !:\n%s", out)
	}
}

func TestEmitNegatedComparisonWhileCompilesAndRuns(t *testing.T) {
	// The negation-of-a-comparison fixture 10.14 rejected: its PrefixValue(Bang)
	// wraps a SourceAlias around the comparison, and buildBoolExpr now unwraps
	// SourceAlias and lowers the comparison through buildComparison, so
	// !(i >= 5) compiles to !((pebble_local_<i> >= 5)) and drives the loop:
	// i counts 0..4 while !(i >= 5) holds, then i = 5 makes the negation false
	// and the loop exits with i = 5. Bounded execution in case of a
	// miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while !(i >= 5) { i = i + 1; } return i; }", false, 5, false)
}

func TestEmitNegatedComparisonWhileWritesC(t *testing.T) {
	// The emitted C for the negated-comparison loop must carry the negation
	// and the comparison in the while condition, with the SourceAlias unwrapped
	// into a plain C comparison: while (!(pebble_local_25 >= 5)). Symbol 25 is
	// the i local, confirmed against the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 0; while !(i >= 5) { i = i + 1; } return i; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    while (!(pebble_local_27 >= 5)) {\n",
		"        pebble_local_27 = pebble_rt_checked_add_i32(pebble_local_27, 1, (PebbleSourceLoc){\"main.peb\", 1, 54});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitReturnsGlobalLetConstantCompilesAndRuns(t *testing.T) {
	// A reference to a top-level `let` global constant is no longer a bare
	// SymbolValue naming storage the backend has no mechanism to lower (see
	// globalLetInitializers in ir_builder.go) — it's inlined to a fresh copy
	// of its initializer at each reference site, so `return x;` for a global
	// `let x i32 = 1;` now lowers to the same IntegerLiteral shape `return
	// 1;` would, which was always a supported return value. This supersedes
	// the old TestEmitRejectsVariableReturn, which asserted the PRE-fix
	// rejection for this exact shape.
	emitAndRun(t, "let x i32 = 1; fn main() i32 { return x; }", false, 1, false)
}

func TestEmitPrintBeforeReturnCompilesAndRuns(t *testing.T) {
	// A print statement before the final Return is a supported leading
	// statement in the entry body block: the print emits its single printf
	// line ("hi" plus the statement's trailing newline), then the block's
	// tail returns 1. Asserting the captured stdout confirms the printed text
	// is exactly one line — not just that the program compiled and exited.
	out := emitAndRunCapture(t, "fn main() i32 { print(\"hi\"); return 1; }", false, 1, false)
	if want := "hi\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitBreakInsideLoopIfCompilesAndRuns(t *testing.T) {
	// The break-inside-a-loop-body-if fixture: i counts 0..9 but the loop
	// breaks when i == 5, so sum accumulates 0+1+2+3+4 = 10, returned as the
	// process exit code. The Break is a leaf node (no children, confirmed
	// against a fixture dump) in the then-arm of a no-else loop-body if and
	// must emit exactly `break;` at the arm's indentation. Bounded execution in
	// case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i == 5 { break; } sum = sum + i; i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitContinueInsideLoopIfCompilesAndRuns(t *testing.T) {
	// The continue-inside-a-loop-body-if fixture: i counts 1..5, skipping the
	// accumulation when i == 3, so sum = 1+2+4+5 = 12, returned as the process
	// exit code. The Continue is a leaf node in the then-arm of a no-else
	// loop-body if and must emit exactly `continue;`. Bounded execution in case
	// of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { i = i + 1; if i == 3 { continue; } sum = sum + i; } return sum; }", false, 12, false)
}

func TestEmitNestedLoopBreakTargetsInnerLoopCompilesAndRuns(t *testing.T) {
	// The nested-loop break fixture: the inner loop breaks when j == 1, so each
	// of the 3 outer iterations runs the inner body once (for j == 0) and
	// total = 3, returned as the exit code. Confirmed against a real fixture
	// dump that this Break's Target names the inner loop's region (the inner
	// While), not the outer one — so plain C break (innermost-loop semantics)
	// is a correct translation and the backend never needs to consult or
	// compare Target's value. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { var j i32 = 0; while j < 3 { if j == 1 { break; } total = total + 1; j = j + 1; } i = i + 1; } return total; }", false, 3, false)
}

func TestEmitBreakDirectInLoopBodyCompilesAndRuns(t *testing.T) {
	// A bare break directly in the loop body (not inside an if), the simplest
	// loop-body jump: i advances to 1 then the break exits the loop, so the
	// return reads 1. Shares the same leaf-node dispatch as the inside-if
	// fixtures. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; break; } return i; }", false, 1, false)
}

func TestEmitContinueDirectInLoopBodyCompilesAndRuns(t *testing.T) {
	// A bare continue directly in the loop body (not inside an if): i advances
	// each iteration before the continue, which skips the accumulation that
	// follows, so total stays 0 and the loop still terminates. This proves the
	// continue actually jumps to the loop's next iteration rather than falling
	// through the rest of the body. Bounded execution in case of a miscompiled
	// loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { i = i + 1; continue; total = total + 1; } return total; }", false, 0, false)
}

func TestEmitBreakInsideLoopIfWritesC(t *testing.T) {
	// The emitted C for the break-inside-if fixture must carry literal
	// `break;` statements at the arm's indentation: the while at the top level
	// (4 spaces), the if one level deeper (8), the break inside the arm two
	// levels deep (12), and no else. Asserting the literal indentation is what
	// stops the recursive build from quietly collapsing levels or emitting the
	// jump at the wrong depth.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i == 5 { break; } sum = sum + i; i = i + 1; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    while (pebble_local_27 < 10) {\n",
		"        if (pebble_local_27 == 5) {\n",
		"            break;",
		"        }",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitContinueInsideLoopIfWritesC(t *testing.T) {
	// The emitted C for the continue-inside-if fixture must carry a literal
	// `continue;` at the arm's indentation (12 spaces), mirroring the break
	// fixture's indentation.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { i = i + 1; if i == 3 { continue; } sum = sum + i; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"        if (pebble_local_27 == 3) {\n",
		"            continue;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitDeferredPrintBeforeBreakCompilesAndRuns(t *testing.T) {
	// A real-source break inside a loop body that also contains a `defer
	// print 5;` produces a Break whose DeferChain references a Print node. The
	// backend emits the deferred print through the shared buildPrint before
	// the break, so the loop's first (and only) iteration prints "5" and then
	// breaks, exiting with i = 0. Bounded execution in case of a miscompiled
	// loop.
	out := emitAndRunCaptureBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { break; defer print 5; } return 0; }", false, 0, false)
	if want := "5\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitDeferredPrintBeforeContinueCompilesAndRuns(t *testing.T) {
	// Same deferred-print path for Continue: each continue fires the deferred
	// `print 5` at the loop's next pass. The body increments i and continues
	// every pass (i becomes 1, 2, then 3), so the deferred print fires three
	// times before the loop condition fails at i = 3 and the program returns
	// 0. Bounded execution in case of a miscompiled loop.
	out := emitAndRunCaptureBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; continue; defer print 5; } return 0; }", false, 0, false)
	if want := "5\n5\n5\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitPrintIntegerLiteralCompilesAndRuns(t *testing.T) {
	// The simplest print: a bare integer literal operand. A bare literal in a
	// print operand resolves to the unanchored int builtin (int32_t), so the
	// format specifier comes from PRId32; the captured output is exactly the
	// value plus the statement's single trailing newline.
	out := emitAndRunCapture(t, "fn main() i32 { print 42; return 0; }", false, 0, false)
	if want := "42\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitPrintEachIntegerWidthCompilesAndRuns(t *testing.T) {
	// Every integer builtin width, each through a local of that declared
	// width: the print dispatches on the operand's own resolved kind, so each
	// width gets its own PRId*/PRIu* specifier matching its fixed-width C
	// type — and each must compile clean under -Wall -Wextra -Werror, which is
	// exactly where a hand-picked (wrong) specifier would surface. All print
	// the same value 7.
	for _, tc := range []struct {
		width string
	}{
		{"i8"}, {"i16"}, {"i32"}, {"i64"}, {"u8"}, {"u16"}, {"u32"}, {"u64"},
	} {
		t.Run(tc.width, func(t *testing.T) {
			src := "fn main() i32 { let x " + tc.width + " = 7; print x; return 0; }"
			out := emitAndRunCapture(t, src, false, 0, false)
			if want := "7\n"; out != want {
				t.Fatalf("compiled program output = %q, want %q", out, want)
			}
		})
	}
}

func TestEmitPrintBoolCompilesAndRuns(t *testing.T) {
	// A bool operand prints as the word true or false (v1's approach: the
	// bool expression wrapped in a C ternary selecting the const char *
	// literal), not as 1/0. Covers both literal operands and a bool-typed
	// local operand.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"true literal", "fn main() i32 { print true; return 0; }", "true\n"},
		{"false literal", "fn main() i32 { print false; return 0; }", "false\n"},
		{"true local", "fn main() i32 { let b bool = true; print b; return 0; }", "true\n"},
		{"false local", "fn main() i32 { let b bool = false; print b; return 0; }", "false\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintCharCompilesAndRuns(t *testing.T) {
	// A char operand prints as the UTF-8 encoding of the single Unicode
	// scalar its int32_t value encodes: the scalar is routed through the
	// runtime helper pebble_rt_char_to_utf8 into a per-operand uint8_t[5]
	// buffer the combined printf prints under %s. ASCII and the full range of
	// multi-byte scalars (2-, 3-, and 4-byte encodings) must all print their
	// exact UTF-8 sequences. The two-operand case proves each char operand
	// gets its own distinctly named buffer (a name collision would fail the
	// C compile) while preserving operand order, and the mixed case proves a
	// char operand still composes with another printable type in the one
	// combined printf.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"ascii literal", "fn main() i32 { print 'x'; return 0; }", "x\n"},
		{"ascii local", "fn main() i32 { let c char = 'x'; print c; return 0; }", "x\n"},
		{"e-acute", "fn main() i32 { print 'é'; return 0; }", "é\n"},
		{"euro sign", "fn main() i32 { print '€'; return 0; }", "€\n"},
		{"grinning face", "fn main() i32 { print '😀'; return 0; }", "😀\n"},
		{"two char operands", "fn main() i32 { print 'é', '€'; return 0; }", "é€\n"},
		{"mixed with str", "fn main() i32 { print \"pre\", 'é', \"post\"; return 0; }", "preépost\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintStrCompilesAndRuns(t *testing.T) {
	// A str operand prints its bytes (specifier %s on the PebbleStr's .data
	// field), covering a string literal, a str-typed local, and the
	// parenthesized literal form `print ("hi")`, whose operand arrives wrapped
	// in a tir.SourceAlias that buildPrint unwraps before dispatching.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"literal", "fn main() i32 { print \"hello\"; return 0; }", "hello\n"},
		{"local", "fn main() i32 { let s str = \"hello\"; print s; return 0; }", "hello\n"},
		{"parenthesized literal", "fn main() i32 { print (\"hi\"); return 0; }", "hi\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintInterpolatedBoolCompilesAndRuns(t *testing.T) {
	// An interpolated-string print operand — `print \`ok? {true}\`;` — prints
	// its literal text runs and its interpolated bool values as the word
	// true/false, all folded into the print's one combined printf. The text is
	// escaped by the same escapeCString a str literal uses, and the value is a
	// bool under the same `(<expr> ? "true" : "false")` ternary the plain bool
	// print operand uses. A bool local and a mix with an ordinary operand
	// prove the value parts build through the bool grammar and the parts still
	// compose with later print operands in the one combined printf.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"literal true", "fn main() i32 { print `ok? {true}`; return 0; }", "ok? true\n"},
		{"literal false", "fn main() i32 { print `ok? {false}`; return 0; }", "ok? false\n"},
		{"bool local", "fn main() i32 { let b bool = true; print `ok? {b}`; return 0; }", "ok? true\n"},
		{"bool expression", "fn main() i32 { let b bool = false; print `ok? {!b}`; return 0; }", "ok? true\n"},
		{"mixed with plain operand", "fn main() i32 { print `pre {true}`, 1; return 0; }", "pre true1\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintFloatCompilesAndRuns(t *testing.T) {
	// A float operand prints with %f (f32/f64 promote to double in a variadic
	// call either way, so %f covers both, matching v1). A bare literal resolves
	// to f64; an f32-typed local prints through the float grammar at its own
	// kind. %f's default precision is 6, so 3.5 prints as 3.500000.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"f64 literal", "fn main() i32 { print 3.5; return 0; }", "3.500000\n"},
		{"f64 local", "fn main() i32 { let x f64 = 3.5; print x; return 0; }", "3.500000\n"},
		{"f32 local", "fn main() i32 { let x f32 = 3.5; print x; return 0; }", "3.500000\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintMultipleOperandsOneLineCompilesAndRuns(t *testing.T) {
	// `print a, b, c;` — multiple comma-separated operands print on ONE line
	// with no separator between them (matching v1): all operands share one
	// printf call, one format string, one argument list, one trailing newline.
	// Mixed operand types exercise that the format specifiers and arguments are
	// assembled in operand order.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"three ints", "fn main() i32 { print 1, 2, 3; return 0; }", "123\n"},
		{"three strings", "fn main() i32 { print \"a\", \"b\", \"c\"; return 0; }", "abc\n"},
		{"mixed", "fn main() i32 { print 1, \" \", 2, \" \", 3.25; return 0; }", "1 2 3.250000\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintWritesSingleCombinedPrintf(t *testing.T) {
	// The emitted C for a mixed-type print must be exactly ONE printf call per
	// print statement whose format string concatenates one specifier per
	// operand in order (integer via the out-of-quotes "%"PRId32 macro
	// spelling, bool/char/str/float as %s literals — a char operand's %s is
	// backed by its own UTF-8 buffer, see below) and ends in the literal \n,
	// with the same number of comma-separated arguments in operand order.
	// Asserting the literal C text is what proves the one-call combined
	// shape, not a per-operand call. The operand texts are confirmed against
	// the fixture dump: the char literal 'x' emits (int32_t)120, encoded to
	// UTF-8 by pebble_rt_char_to_utf8 into a per-operand uint8_t[5] buffer
	// named from the operand's node ID, the string literal "hi" emits its
	// PebbleStr compound literal's .data field cast to const char *, and the
	// bool emits the "true"/"false" ternary. The buffer name is captured
	// from the declaration so the assertion is robust to the fixture's normal
	// symbol/numbering, and the absence of any %c proves the old char path is
	// gone.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { print 1, true, 'x', \"hi\", 3.5; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if count := strings.Count(out, "printf("); count != 1 {
		t.Errorf("emitted C has %d printf( calls, want exactly one:\n%s", count, out)
	}
	bufferRE := regexp.MustCompile(`uint8_t (pebble_char_utf8_\d+)\[5\];`)
	match := bufferRE.FindStringSubmatch(out)
	if match == nil {
		t.Fatalf("emitted C missing the char operand's uint8_t[5] UTF-8 buffer:\n%s", out)
	}
	bufferName := match[1]
	for _, want := range []string{
		fmt.Sprintf("uint8_t %s[5];", bufferName),
		fmt.Sprintf("pebble_rt_char_to_utf8((int32_t)120, %s);", bufferName),
		fmt.Sprintf("printf(\"%%\"PRId32\"%%s\"\"%%s\"\"%%s\"\"%%f\"\"\\n\", 1, (true ? \"true\" : \"false\"), (const char *)%s, (const char *)(PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 }.data, 3.5);", bufferName),
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "%c") {
		t.Errorf("emitted C still contains the old char %%c path:\n%s", out)
	}
}

func TestEmitDeferredPrintCharCompilesAndRuns(t *testing.T) {
	// A deferred char print routes through the same shared buildPrint, so a
	// multi-byte char operand's UTF-8 buffer and pebble_rt_char_to_utf8
	// pre-statements must land in the deferred statement sequence at the
	// ImplicitReturn exit, ahead of the combined printf. Calling the helper
	// prints the exact UTF-8 encoding of 'é' and the entry returns 0.
	out := emitAndRunCapture(t, "fn helper() void { defer print 'é'; }\nfn main() i32 { helper(); return 0; }", false, 0, false)
	if want := "é\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitPrintInsideLoopIfCompilesAndRuns(t *testing.T) {
	// A print inside a loop-body if's arm routes through buildLoopBody -> the
	// Print case -> buildPrint, so it composes with the if-in-loop-body shape.
	// The arm fires once (when i == 1), printing "mark", and the loop returns
	// 2. Bounded execution in case of a miscompiled loop.
	out := emitAndRunCaptureBounded(t, "fn main() i32 { var i i32 = 0; while i < 2 { i = i + 1; if i == 1 { print \"mark\"; } } return i; }", false, 2, false)
	if want := "mark\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitRangeLoopAccumulationCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: a range loop lowers to a C for loop over the
	// bound iterator, and the iterator is an ordinary C loop counter the body
	// can read. i counts 0..3 (exclusive), sum accumulates i each pass, so
	// sum = 0+1+2 = 3, returned as the process exit code. The iterator's C
	// type is the entry's width (int32_t) and the body references it through
	// the seeded local scope like any other local. Bounded execution in case
	// of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", false, 3, false)
}

func TestEmitRangeLoopExclusiveVsInclusiveCompilesAndRuns(t *testing.T) {
	// The inclusive form (`..=`) differs from the exclusive form (`..`) by one
	// iteration: 0..3 sums 0+1+2 = 3, while 0..=3 sums 0+1+2+3 = 6 — the
	// emitted C condition is `<` for the exclusive form and `<=` for the
	// inclusive form (RangeLoop.RangeInclusive). Each row's expected exit code
	// therefore proves the right operator was chosen. Bounded execution.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"exclusive", "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", 3},
		{"inclusive", "fn main() i32 { var sum i32 = 0; loop 0..=3 : i { sum = sum + i; } return sum; }", 6},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRangeLoopBreakAndContinueCompilesAndRuns(t *testing.T) {
	// break/continue inside a range loop body work through the same
	// buildLoopJump machinery a while body uses: a Break's/Continue's Target
	// names the RangeLoop's own Region (confirmed against a real fixture dump),
	// and plain C break/continue already target the nearest enclosing loop,
	// which the emitted for loop is. i counts 0..9, breaking when i == 5 and
	// skipping the accumulation when i == 3, so sum = 0+1+2+4 = 7, returned as
	// the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var sum i32 = 0; loop 0..10 : i { if i == 5 { break; } if i == 3 { continue; } sum = sum + i; } return sum; }", false, 7, false)
}

func TestEmitRangeLoopNestedInRangeLoopCompilesAndRuns(t *testing.T) {
	// A range loop nested inside another range loop: the inner RangeLoop is a
	// plain statement in the outer loop's body Block, dispatched by
	// buildLoopBody exactly like a nested While. i and j each count 0..2, so
	// the inner body runs 3 x 3 = 9 times and total = 9, returned as the exit
	// code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; loop 0..3 : i { loop 0..3 : j { total = total + 1; } } return total; }", false, 9, false)
}

func TestEmitRangeLoopInsideWhileCompilesAndRuns(t *testing.T) {
	// A range loop nested inside a while loop's body: the RangeLoop is a
	// statement in the while's body Block, dispatched by buildLoopBody. Each
	// of the 2 while iterations runs the range loop's 3 inner iterations, so
	// total = 6, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 2 { loop 0..3 : j { total = total + 1; } i = i + 1; } return total; }", false, 6, false)
}

func TestEmitRangeLoopNonLiteralBoundsCompilesAndRuns(t *testing.T) {
	// The start/end are ordinary integer expressions built by buildExpr at
	// the entry's width — a local reference for the end, and checked
	// arithmetic for the start. With the iterator anchored by the i32
	// accumulation, n = 3 makes 1..=n sum 1+2+3 = 6, and the arithmetic start
	// 1+2 = 3 makes 3..=5 sum 3+4+5 = 12. Bounded execution.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"local reference bound", "fn main() i32 { var n i32 = 3; var sum i32 = 0; loop 1..=n : i { sum = sum + i; } return sum; }", 6},
		{"arithmetic bound", "fn main() i32 { var sum i32 = 0; loop 1+2..=5 : i { sum = sum + i; } return sum; }", 12},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRangeLoopIteratorComparisonOnlyCompilesAndRuns(t *testing.T) {
	// The iterator referenced only in a comparison (`if i == 2`) — never in a
	// width-anchoring position — stays the checker's unanchored int builtin
	// (confirmed against a real fixture dump), so the comparison operand
	// lowers it directly as its pebble_local_<symbol> name via
	// buildComparisonOperand's int-typed-SymbolValue case, and both bounds are
	// int-typed literals lowered as their decimal text. i counts 0..2, and the
	// accumulation happens once (when i == 2), so sum = 1. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { if i == 2 { sum = sum + 1; } } return sum; }", false, 1, false)
}

func TestEmitRangeLoopUnusedIteratorCompilesAndRuns(t *testing.T) {
	// A bound range loop whose iterator is never read in the body: the loop
	// still iterates over its C counter (the condition and increment read it),
	// so the body runs 3 times and sum = 3. The bounds are int-typed literals
	// (nothing anchors them), lowered as their decimal text. Bounded
	// execution.
	emitAndRunBounded(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + 1; } return sum; }", false, 3, false)
}

func TestEmitRangeLoopNestedIteratorAsInnerBoundCompilesAndRuns(t *testing.T) {
	// A nested range loop whose bound reads the outer loop's iterator
	// (`loop 0..i : j`): the outer iterator is an int-typed SymbolValue in the
	// inner loop's end position, lowered as its pebble_local_<symbol> name by
	// buildRangeBound's int-typed-SymbolValue case. The inner loop runs 0, 1,
	// and 2 iterations for i = 0, 1, 2, so total = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; loop 0..3 : i { loop 0..i : j { total = total + 1; } } return total; }", false, 3, false)
}

func TestEmitRangeLoopI64EntryCompilesAndRuns(t *testing.T) {
	// A range loop inside an i64 entry: the iterator's C type follows the
	// entry's width (int64_t), and the bounds/iterator are anchored to i64 by
	// the i64 accumulation. sum = 0+1+2 = 3, returned as the exit code.
	// Bounded execution.
	emitAndRunBounded(t, "fn main() i64 { var sum i64 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", false, 3, false)
}

func TestEmitRangeLoopWritesC(t *testing.T) {
	// The emitted C for the flagship fixture must be a C for loop whose
	// init/condition/increment all use the iterator's own
	// pebble_local_<symbol> name as an ordinary C loop counter at the entry's
	// width: `for (int32_t pebble_local_26 = 0; pebble_local_26 < 3;
	// pebble_local_26++)`. Symbols 25 (sum) and 26 (the iterator) come from
	// the real fixture dump. The inclusive form's condition must instead be
	// `<=` (RangeLoop.RangeInclusive), so the two operators are distinguishable
	// in the emitted text.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    for (int32_t pebble_local_28 = 0; pebble_local_28 < 3; pebble_local_28++) {\n",
		"        pebble_local_27 = pebble_rt_checked_add_i32(pebble_local_27, pebble_local_28, (PebbleSourceLoc){\"main.peb\", 1, 56});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	unit, snapshot, entryID, sources = buildFixture(t, "fn main() i32 { var sum i32 = 0; loop 0..=3 : i { sum = sum + i; } return sum; }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out = buf.String()
	if !strings.Contains(out, "    for (int32_t pebble_local_28 = 0; pebble_local_28 <= 3; pebble_local_28++) {\n") {
		t.Errorf("emitted C missing the inclusive for-loop header:\n%s", out)
	}
}

func TestEmitForLoopAccumulationCompilesAndRuns(t *testing.T) {
	// The flagship fixture: a classic for loop lowers to a C for loop with
	// the same three clauses. step counts 0..3 (the initializer declares it as
	// an ordinary local of the entry's width, seeded into the loop's scope),
	// and total accumulates step each pass, so total = 0+1+2 = 3, returned as
	// the process exit code. The body references step through the seeded scope
	// like any other local. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; step = step + 1 { total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopAllClausesOmittedCompilesAndRuns(t *testing.T) {
	// for ;; { ... } — all three clauses absent: an infinite loop from the
	// header's perspective, so termination comes only from the explicit break
	// in the body. The body advances its own counter (declared outside) and
	// breaks at 3, so the program terminates with i = 3 as the exit code.
	// Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; for ;; { if i >= 3 { break; } i = i + 1; } return i; }", false, 3, false)
}

func TestEmitForLoopNoConditionNeedsBreakCompilesAndRuns(t *testing.T) {
	// for var i i32 = 0;; i = i + 1 { ... } — no condition clause (the
	// initializer and update are both present), so termination comes only from
	// the explicit break in the body — the omitted-condition combination the
	// brief calls out explicitly. total accumulates 0+1+2 = 3 before the break
	// at i == 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0;; i = i + 1 { if i >= 3 { break; } total = total + i; } return total; }", false, 3, false)
}

func TestEmitForLoopNoUpdateCompilesAndRuns(t *testing.T) {
	// for var step i32 = 0; step < 3; { ... } — no update clause; the body
	// advances step itself. total accumulates 0+1+2 = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; { total = total + step; step = step + 1; } return total; }", false, 3, false)
}

func TestEmitForLoopInitializerOnlyCompilesAndRuns(t *testing.T) {
	// for var i i32 = 0;; { ... } — initializer only (no condition, no
	// update), so the body advances i and breaks when it reaches 3. total
	// accumulates 0+1+2 = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0;; { if i >= 3 { break; } total = total + i; i = i + 1; } return total; }", false, 3, false)
}

func TestEmitForLoopUpdateOnlyCompilesAndRuns(t *testing.T) {
	// for ; ; i = i + 1 { ... } — update only (no initializer, no condition):
	// i is declared outside, the header's update advances it, and the body
	// breaks when it reaches 3, so i = 3 is returned. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; for ; ; i = i + 1 { if i >= 3 { break; } } return i; }", false, 3, false)
}

func TestEmitForLoopConditionOnlyCompilesAndRuns(t *testing.T) {
	// for ; i < 3; { ... } — condition only (no initializer, no update): i is
	// declared outside, the header's condition alone controls the loop, and
	// the body both accumulates and advances i. This shape was blocked by a
	// real tir/verify.go bug (the verifier wrongly required For's first
	// child to always be CategoryNonvalue, but with no initializer the first
	// child is the condition itself, CategoryValue) until that bug was fixed
	// separately (commit "tir: fix verifier rejecting well-formed classic
	// for-loops missing an initializer"). sum accumulates 0+1+2 = 3.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; for ; i < 3; { sum = sum + i; i = i + 1; } return sum; }", false, 3, false)
}

func TestEmitForLoopConditionAndUpdateCompilesAndRuns(t *testing.T) {
	// for ; i < 3; i = i + 1 { ... } — condition and update, no initializer:
	// i is declared outside, the header's condition controls the loop and
	// its update advances i, so the body only needs to accumulate. Also
	// blocked by the same tir bug as the condition-only case above (the
	// verifier's fixed-position check rejected any For whose first child
	// was the condition, which includes this shape too). sum accumulates
	// 0+1+2 = 3.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; for ; i < 3; i = i + 1 { sum = sum + i; } return sum; }", false, 3, false)
}

func TestEmitForLoopBreakAndContinueCompilesAndRuns(t *testing.T) {
	// break/continue inside a for-loop body work through the same
	// buildLoopJump machinery a while/range body uses: a Break's/Continue's
	// Target names the For's own Region, and plain C break/continue already
	// target the nearest enclosing loop, which the emitted for loop is. step
	// counts 0..9, breaking when step == 5 and skipping the accumulation when
	// step == 3, so total = 0+1+2+4 = 7, returned as the exit code. Bounded
	// execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 10; step = step + 1 { if step == 5 { break; } if step == 3 { continue; } total = total + step; } return total; }", false, 7, false)
}

func TestEmitForLoopNestedInWhileCompilesAndRuns(t *testing.T) {
	// A classic for loop nested inside a while loop's body: the For is a
	// statement in the while's body Block, dispatched by buildLoopBody. Each
	// of the 2 while iterations runs the for loop's 3 inner iterations, so
	// total = 6, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 2 { for var j i32 = 0; j < 3; j = j + 1 { total = total + 1; } i = i + 1; } return total; }", false, 6, false)
}

func TestEmitForLoopNestedInRangeLoopCompilesAndRuns(t *testing.T) {
	// A classic for loop nested inside a range loop's body: the For is a
	// statement in the range loop's body Block, dispatched by buildLoopBody.
	// Each of the 3 range iterations runs the for loop's 2 inner iterations,
	// so total = 6, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; loop 0..3 : i { for var j i32 = 0; j < 2; j = j + 1 { total = total + 1; } } return total; }", false, 6, false)
}

func TestEmitForLoopNestedInForLoopCompilesAndRuns(t *testing.T) {
	// A classic for loop nested inside another classic for loop: the inner For
	// is a statement in the outer for's body Block, dispatched by
	// buildLoopBody exactly like a nested While or RangeLoop. i and j each
	// count 0..3, so the inner body runs 3 x 3 = 9 times and total = 9,
	// returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0; i < 3; i = i + 1 { for var j i32 = 0; j < 3; j = j + 1 { total = total + 1; } } return total; }", false, 9, false)
}

func TestEmitForLoopI64EntryCompilesAndRuns(t *testing.T) {
	// A classic for loop inside an i64 entry: the initializer's C type follows
	// the entry's width (int64_t). total accumulates 0+1+2 = 3, returned as
	// the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i64 { var total i64 = 0; for var step i64 = 0; step < 3; step = step + 1 { total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopBoolInitializerAndConditionCompilesAndRuns(t *testing.T) {
	// The bool grammar works in every for-loop clause: the initializer
	// declares a bool local (built by buildScalarInitializeCore's bool case),
	// the condition is a bare bool value (buildCondition -> buildBoolExpr), and
	// the update reassigns the bool (buildStoreCore's bool case). The loop
	// runs once (first is true), accumulates 1, flips first to false, and the
	// next condition check stops it, so total = 1. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var first bool = true; first; first = false { total = total + 1; } return total; }", false, 1, false)
}

func TestEmitForLoopLogicalConditionCompilesAndRuns(t *testing.T) {
	// A && condition in a for header goes through buildCondition ->
	// buildBoolExpr -> buildComparison exactly as an if/while condition does.
	// i counts 0..3 under the && condition, so total = 0+1+2 = 3. Bounded
	// execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0; i < 3 && i >= 0; i = i + 1 { total = total + i; } return total; }", false, 3, false)
}

func TestEmitForLoopWritesC(t *testing.T) {
	// The emitted C for the flagship fixture must be a C for loop whose
	// init/condition/update clauses match the source exactly: the initializer
	// declares the loop local as `<entry ctype> pebble_local_<symbol>` in the
	// header (no statement newline/indent, the for-header's first `;`
	// terminating the init clause), the condition is the plain comparison, and
	// the update is a bare assignment expression with NO trailing `;` (the for
	// statement supplies it). The initializer local gets its
	// -Wunused-variable (void) cast as the body's first statement, since its C
	// declaration lives in the header where a cast cannot go. Symbols 25
	// (total) and 26 (step) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; step = step + 1 { total = total + step; } return total; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    for (int32_t pebble_local_28 = 0; pebble_local_28 < 3; pebble_local_28 = pebble_rt_checked_add_i32(pebble_local_28, 1, (PebbleSourceLoc){\"main.peb\", 1, 75})) {\n",
		"        (void)pebble_local_28;",
		"        pebble_local_27 = pebble_rt_checked_add_i32(pebble_local_27, pebble_local_28, (PebbleSourceLoc){\"main.peb\", 1, 94});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitI64ReturnEntryWritesC(t *testing.T) {
	// Mirror of TestEmitIntegerReturnEntryWritesC at the wider width: the
	// pebble_user_main adapter must be declared with the 64-bit return type so
	// a wide return value is not truncated, not the i32 entry's "int".
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { return 42; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{"pebble_rt.h", "pebble_rt_default_context", "return 42;", "static int64_t pebble_user_main(PebbleContext *ctx)"} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "static int pebble_user_main") {
		t.Errorf("emitted C declares pebble_user_main returning plain int, want int64_t for an i64 entry:\n%s", out)
	}
}

func TestEmitI64ReturnEntryCompilesAndRunsExitCode42(t *testing.T) {
	emitAndRun(t, "fn main() i64 { return 42; }", false, 42, false)
}

func TestEmitI64WhileAccumulationCompilesAndRuns(t *testing.T) {
	// The full control-flow story at i64: locals, mutation, a while loop, and
	// checked arithmetic all at the wider width. i counts 0..4 and sum
	// accumulates i each pass, so sum = 0+1+2+3+4 = 10, returned as the
	// process exit code. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i64 { var i i64 = 0; var sum i64 = 0; while i < 5 { sum = sum + i; i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitI64WhileWritesC(t *testing.T) {
	// The emitted C for the i64 accumulation loop must declare its locals at
	// int64_t and use the i64 checked helpers, proving the width threads
	// through declarations, loop conditions, and arithmetic together. The
	// symbol IDs 27 (i) and 28 (sum) are the same ones the i32 fixture dump
	// established, so the assertions are exact.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { var i i64 = 0; var sum i64 = 0; while i < 5 { sum = sum + i; i = i + 1; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int64_t pebble_local_27 = 0;",
		"int64_t pebble_local_28 = 0;",
		"    while (pebble_local_27 < 5) {\n",
		"        pebble_local_28 = pebble_rt_checked_add_i64(pebble_local_28, pebble_local_27, (PebbleSourceLoc){\"main.peb\", 1, 69});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t") {
		t.Errorf("emitted C declares an i32 local in an i64 entry:\n%s", out)
	}
}

func TestEmitFloatArithmeticInFloatReturnPosition(t *testing.T) {
	// Regression: Stage A rejected this BinaryValue because float arithmetic
	// was not yet in buildFloatExpr. Stage B now lowers it as plain C float
	// arithmetic, with no checked runtime helper needed.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { return 1.0 + 2.0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	if !strings.Contains(buf.String(), "1.0 + 2.0") {
		t.Fatalf("emitted C did not contain plain float addition:\n%s", buf.String())
	}
	compileAndRun(t, buf.Bytes(), 3, false)
}

func TestEmitFloatToIntegerReleaseReturnsSentinel(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let x f64 = 2147483648.0; return (x as i32) + 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 1, false, false)
}

func TestEmitStrEqualityInWhileCompilesAndRuns(t *testing.T) {
	// A str == comparison as a bare while loop condition: the loop runs while
	// the sentinel string is "go" (it never changes — str locals are not
	// reassignable this slice), accumulating a counter until an in-loop
	// integer comparison breaks it. This exercises pebble_rt_str_eq through
	// buildCondition on a while (whose condition grammar routes BinaryValue to
	// buildComparison), not just an if, and runs under the bounded harness so
	// a miscompiled non-terminating loop fails loudly instead of hanging.
	emitAndRunBounded(t, "fn main() i32 { let s str = \"go\"; var n i32 = 0; while s == \"go\" { n = n + 1; if n == 2 { break; } } return n; }", false, 2, false)
}

func TestEmitStrOrderingPrefixTieBreakCompilesAndRuns(t *testing.T) {
	// Two strings that share a prefix but differ in length: "hi" vs "hi!".
	// The shorter string must sort first (matching strcmp's convention for a
	// prefix — the shorter one is "less"), so "hi" < "hi!" is true and the
	// then-arm runs, exiting 10. This proves the shorter-string-sorts-first
	// tie-break behaves correctly at runtime.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi!\"; if s < t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrIndexRangeLoopIteratorCompilesAndRuns(t *testing.T) {
	// A range loop's iterator used directly as a str index arrives as an
	// int-typed SymbolValue (the unanchored-int case, the same shortcut
	// buildArrayPlaceRead handles), confirmed against a real fixture dump.
	// Iterating 0..2 over "hi" and counting each match of 'h' (only index 0)
	// proves the iterator's C name is the correct index lvalue.
	emitAndRunBounded(t, "fn main() i32 { let s str = \"hi\"; var n i32 = 0; loop 0..2 : i { if s[i] == 'h' { n = n + 1; } } return n; }", false, 1, false)
}

func TestEmitSwitchMultiValueCaseCompilesAndRuns(t *testing.T) {
	// The flagship fixture: a switch with a multi-value case (1, 2 share the
	// same body returning 10), a single-value case (3 returning 30), and an
	// else (default returning 0). Subject value 1 hits the multi-value case
	// and returns 10.
	emitAndRun(t, "fn main() i32 { switch 1 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 10, false)
}

func TestEmitSwitchMultiValueCaseSecondValueCompilesAndRuns(t *testing.T) {
	// Same switch as above but subject value 2 — still hits the multi-value
	// case and returns 10, confirming both SwitchCase nodes sharing the same
	// body produce the same result.
	emitAndRun(t, "fn main() i32 { switch 2 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 10, false)
}

func TestEmitSwitchSingleValueCaseCompilesAndRuns(t *testing.T) {
	// Subject value 3 hits the single-value case and returns 30.
	emitAndRun(t, "fn main() i32 { switch 3 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 30, false)
}

func TestEmitSwitchElseCompilesAndRuns(t *testing.T) {
	// Subject value 99 hits the else/default arm and returns 0.
	emitAndRun(t, "fn main() i32 { switch 99 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchBlockCaseBodyCompilesAndRuns(t *testing.T) {
	// A block-wrapped (braced, multi-statement) case body: the case declares
	// a local and returns an expression using it. This exercises the
	// Block-bodied path in buildSwitchCaseBody.
	emitAndRun(t, "fn main() i32 { switch 1 { case 1: { let x i32 = 42; return x; } else: return 0; } }", false, 42, false)
}

func TestEmitSwitchBareReturnCaseBodyCompilesAndRuns(t *testing.T) {
	// A bare single-statement case body (no braces): `case 1: return 10;`.
	// This exercises the bare-statement path in buildSwitchCaseBody.
	emitAndRun(t, "fn main() i32 { switch 1 { case 1: return 10; else: return 0; } }", false, 10, false)
}

func TestEmitSwitchBoolSubjectCompilesAndRuns(t *testing.T) {
	// A bool subject with bool case values: `switch true { case true: return
	// 1; else: return 0; }`. Bool case values are emitted as `case 1:` (true)
	// and `case 0:` (false) in C, since C switch requires integral constants.
	emitAndRun(t, "fn main() i32 { switch true { case true: return 1; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchBoolSubjectFalseCompilesAndRuns(t *testing.T) {
	// Bool subject `false` hits the else/default arm.
	emitAndRun(t, "fn main() i32 { switch false { case true: return 1; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchWritesC(t *testing.T) {
	// Confirm the emitted C for a switch fixture contains the expected
	// stacked case labels and body structure.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { switch 1 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"switch (1)",
		"case 1:",
		"case 2:",
		"case 3:",
		"default:",
		"return 10;",
		"return 30;",
		"return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitSwitchCompilesCleanUnderStrictFlags(t *testing.T) {
	// The emitted C for a switch must compile under -Wall -Wextra -Werror
	// with no warnings. This exercises the full cc compilation path.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { switch 1 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 10, false)
}

func TestEmitTopLevelGuardIfCompilesAndRuns(t *testing.T) {
	// The minimal non-tail-if repro: a guard-clause if with no else, as a
	// leading statement in a top-level function body, followed by more code.
	// x = 5 takes the guard (x + 1 = 6); x = 0 falls through the guard to
	// the code after it (x + 10 = 10). Both calls run, so both the guard arm
	// and the fall-through code after the if are exercised.
	emitAndRun(t, "fn helper(x i32) i32 { if x > 0 { return x + 1; } return x + 10; } fn main() i32 { return helper(5) + helper(0); }", false, 16, false)
}

func TestEmitTopLevelGuardIfWritesC(t *testing.T) {
	// The emitted C for the minimal repro must be a plain no-else if whose
	// arm ends in return, followed by the enclosing return — the shape
	// buildLeadingIf produces, byte-identical in style to buildLoopIf's.
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper(x i32) i32 { if x > 0 { return 1; } return 0; } fn main() i32 { return helper(1); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    if (pebble_local_25 > 0) {",
		"        return 1;",
		"    }",
		"    return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "} else {") {
		t.Errorf("emitted C contains an else for a no-else if:\n%s", out)
	}
}

func TestEmitTopLevelIfElseLeadingCompilesAndRuns(t *testing.T) {
	// A top-level if/else (both arms present) as a leading statement,
	// followed by more code that runs after either arm: pick(1) takes the
	// then-arm (result = 10, then 11) and pick(0) takes the else-arm
	// (result = 20, then 20), so both arms plus the fall-through code after
	// the if are exercised.
	emitAndRun(t, "fn pick(x i32) i32 { var result i32 = 0; if x > 0 { result = 10; } else { result = 20; } result = result + x; return result; } fn main() i32 { return pick(1) + pick(0); }", false, 31, false)
}

func TestEmitTopLevelSwitchLeadingCompilesAndRuns(t *testing.T) {
	// A switch as an ordinary top-level leading statement, followed by more
	// code: classify(1) hits the single-value case (11), classify(2) hits the
	// multi-value case (21), and classify(9) hits the else arm (31). Every
	// case body falls through to the statements after the switch, and none
	// ends in a return — the shape buildSwitch was previously unable to emit
	// in a non-tail position.
	emitAndRun(t, "fn classify(x i32) i32 { var result i32 = 0; switch x { case 1: result = result + 10; case 2, 3: result = result + 20; else: result = result + 30; } result = result + 1; return result; } fn main() i32 { return classify(1) + classify(2) + classify(9); }", false, 63, false)
}

func TestEmitTopLevelSwitchLeadingWithReturningCaseCompilesAndRuns(t *testing.T) {
	// A top-level switch whose case bodies may return OR fall through:
	// f(1) returns 99 from inside a case body, f(2) falls through case 2 to
	// the code after the switch (1 + 10 = 11), and f(9) falls through the
	// else arm (2 + 10 = 12). This confirms a case body's "may fall through"
	// grammar includes the return case, matching what buildSwitchCaseBody's
	// bare-Return path did for the tail position.
	emitAndRun(t, "fn f(x i32) i32 { var total i32 = 0; switch x { case 1: return 99; case 2: total = total + 1; else: total = total + 2; } return total + 10; } fn main() i32 { return f(1) + f(2) + f(9); }", false, 122, false)
}

func TestEmitSwitchInsideLoopBodyCompilesAndRuns(t *testing.T) {
	// The switch-in-loop repro: a switch as a statement inside a while loop
	// body, followed by more loop-body code. i counts 0, 1, 2; each switch
	// adds 1/2/3 by case, then the code after the switch adds 1, so total =
	// (1+1) + (2+1) + (3+1) = 9, returned as the exit code. This is the
	// position where If already worked but Switch did not. Bounded execution
	// in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { switch i { case 0: total = total + 1; case 1: total = total + 2; else: total = total + 3; } total = total + 1; i = i + 1; } return total; }", false, 9, false)
}

func TestEmitIfInsideSwitchCaseBodyCompilesAndRuns(t *testing.T) {
	// Nested control flow inside a fall-through switch case body: the case 2
	// body is itself an if/else whose arms reassign the enclosing local. x=2
	// hits the case 2 body's then-arm (total = 5), then the code after the
	// switch adds 1, so total = 6.
	emitAndRun(t, "fn main() i32 { var x i32 = 2; var total i32 = 0; switch x { case 1: total = total + 1; case 2: if x == 2 { total = total + 5; } else { total = total + 6; } else: total = total + 9; } total = total + 1; return total; }", false, 6, false)
}

func TestEmitSwitchInsideIfArmCompilesAndRuns(t *testing.T) {
	// Nested control flow inside a top-level if arm: the then-arm is itself
	// a switch whose case bodies fall through, and the code after the if runs
	// after either arm. x=1 takes the then-arm's case 1 (total = 10), then
	// the fall-through code adds 1, so total = 11.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; var total i32 = 0; if x > 0 { switch x { case 1: total = total + 10; else: total = total + 20; } } else { total = total + 30; } total = total + 1; return total; }", false, 11, false)
}

func TestEmitIfAndSwitchInsideTopLevelIfArmCompilesAndRuns(t *testing.T) {
	// Both a nested if and a nested switch inside a single top-level if arm,
	// plus more code after the outer if: x=1 takes the then-arm, whose own if
	// adds 1 and whose switch adds 10, then the outer fall-through code adds
	// 1, so total = 12.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; var y i32 = 2; var total i32 = 0; if x > 0 { if y > 0 { total = total + 1; } else { total = total + 2; } switch y { case 2: total = total + 10; else: total = total + 20; } } total = total + 1; return total; }", false, 12, false)
}

func TestEmitBreakAndContinueInsideLoopIfArmCompilesAndRuns(t *testing.T) {
	// Regression: break and continue inside an if arm inside a while loop
	// body still work after the loop-body dispatch was reorganized into the
	// shared fall-through builder. i counts 1..9; even i continues past the
	// total accumulation, and i=5 breaks the loop. Only odd i < 5 accumulate
	// (1 and 3), so total = 4.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 10 { i = i + 1; if i % 2 == 0 { continue; } if i == 5 { break; } total = total + i; } return total; }", false, 4, false)
}

func TestEmitBreakInsideSwitchCaseBodyTargetsSwitchCompilesAndRuns(t *testing.T) {
	// A break inside a switch case body targets the switch — Pebble's break
	// resolves to the nearest enclosing loop-or-switch, so the emitted C
	// break (which C also resolves to the nearest switch/loop) is the direct,
	// correct translation. The switch sits inside a loop, so if the break
	// wrongly leaked to the loop the accumulation after the switch would stop
	// early. i=0 falls through the else (+1, then after-switch +1 = 2), i=1
	// hits case 1's break (switch only, then after-switch +1 = 3), i=2 hits
	// case 2 (+10, then after-switch +1 = 14), so total = 14.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { switch i { case 1: break; case 2: total = total + 10; else: total = total + 1; } total = total + 1; i = i + 1; } return total; }", false, 14, false)
}

func TestEmitReturnInsideLoopIfArmCompilesAndRuns(t *testing.T) {
	// A return inside an if arm inside a loop body exits the function early —
	// reachable now that the enclosing function's result grammar threads
	// through the fall-through builder into loop bodies. f(3) returns 3 from
	// inside the loop; f(20) never matches and returns 99 after the loop, so
	// the exit code is 3 + 99 = 102.
	emitAndRunBounded(t, "fn f(x i32) i32 { var i i32 = 0; while i < 10 { if i == x { return i; } i = i + 1; } return 99; } fn main() i32 { return f(3) + f(20); }", false, 102, false)
}

func TestEmitSwitchI64EntryCompilesAndRuns(t *testing.T) {
	// A switch with an i64 entry: the subject and case values are i64.
	emitAndRun(t, "fn main() i64 { switch 2 { case 1: return 100; case 2: return 200; else: return 0; } }", false, 200, false)
}

func TestEmitDeferBeforeReturnCompilesAndRuns(t *testing.T) {
	// A single defer running before a return, observably changing the returned
	// value. var x i32 = 0; defer x = x + 1; return x; should return 1, not 0,
	// proving the deferred Store executes before the return value is read.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; defer x = x + 1; return x; }", false, 1, false)
}

func TestEmitTwoDefersLIFOOrderCompilesAndRuns(t *testing.T) {
	// Two defers in the same scope, proving LIFO (last-registered-first)
	// order. The second-registered defer (x = x + 10) must run before the
	// first (x = x * 2). Starting from x=1: first defer registers x*2, then
	// x+10. LIFO means x+10 runs first (1+10=11), then x*2 (11*2=22).
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 1; defer x = x * 2; defer x = x + 10; return x; }", false, 22, false)
}

func TestEmitDeferInsideIfArmFiresCompilesAndRuns(t *testing.T) {
	// A defer inside an if-arm whose exit (return) is inside that same arm.
	// The defer fires because the return's DeferChain includes it. Both arms
	// return, so the if is the block's tail. Condition true: defer x=x+1
	// runs, return 0+1=1. Condition false: no defer, return 2.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; if x == 0 { defer x = x + 1; return x; } else { return 2; } }", false, 1, false)
}

func TestEmitDeferOutsideIfDoesNotFireCompilesAndRuns(t *testing.T) {
	// A defer registered inside a while-loop body, where the return is AFTER
	// the loop (outside the loop's region). The return's DeferChain does not
	// include the loop's defer because the return is outside the region the
	// defer was registered in. This proves static/lexical scoping: the defer
	// only fires at exits inside the same region it was registered in.
	// Loop: i counts 0..2, when i==0 defer x=x+100 is registered and x=1.
	// After loop, return x=1 (defer did NOT fire).
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; var i i32 = 0; while i < 2 { if i == 0 { defer x = x + 100; x = 1; } i = i + 1; } return x; }", false, 1, false)
}

func TestEmitDeferBeforeBreakCompilesAndRuns(t *testing.T) {
	// A defer before a break inside a loop. The break's DeferChain includes
	// the deferred Store, so it fires before the break. Loop: i counts 0..5,
	// defer x=x+1 fires on break when i==3, break exits. x starts at 10,
	// after defer x=11. Exit code 11.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 10; var i i32 = 0; while i < 5 { if i == 3 { defer x = x + 1; break; } i = i + 1; } return x; }", false, 11, false)
}

func TestEmitDeferBeforeContinueCompilesAndRuns(t *testing.T) {
	// A defer before a continue inside a loop. The continue's DeferChain
	// includes the deferred Store. Loop: i counts 0..5, when i==2, defer
	// x=x+1 fires on continue (skipping i=i+1), then i is incremented by
	// the loop body's i=i+1 for non-continue passes. Actually, continue
	// skips the rest of the body, so i=i+1 is skipped when i==2. Let me
	// restructure: use a for loop where the update is separate.
	// Simpler: while loop, i starts at 0, defer x=x+1 then continue when
	// i==1, so i stays at 1 forever — no, that would loop forever.
	// Better approach: defer fires on continue, then manually increment.
	// i: 0->1->2 (continue fires defer x=x+1) ->3->4->5 exits. x=20+1=21.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 20; var i i32 = 0; while i < 5 { if i == 2 { defer x = x + 1; i = i + 1; continue; } i = i + 1; } return x; }", false, 21, false)
}

func TestEmitCharInLoopBodyCompilesAndRuns(t *testing.T) {
	// A char-typed local declared and compared inside a while-loop body: the
	// loop runs three passes, each declaring c='a' and summing 1 when c == 'a',
	// exiting with 3 — proving char locals work through the loop-body
	// leading-statement and condition paths.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 3 { let c char = 'a'; if c == 'a' { sum = sum + 1; } i = i + 1; } return sum; }", false, 3, false)
}

func TestEmitDescendingRangeLoopCompilesAndRuns(t *testing.T) {
	// A descending range loop (start > end): loop 5..0 counts down, running
	// 5 iterations (i=5,4,3,2,1) and accumulating count = 5. This is the
	// reproduction of the zero-iteration bug: the old emitter unconditionally
	// emitted `<` and `++`, producing `for (i = 5; i < 0; i++)` which is
	// false on the first check and never executes the body.
	emitAndRunBounded(t, "fn main() i32 { var count i32 = 0; loop 5..0 : i { count = count + 1; } return count; }", false, 5, false)
}

func TestEmitDescendingRangeLoopInclusiveCompilesAndRuns(t *testing.T) {
	// A descending inclusive range loop (start >= end): loop 5..=0 counts
	// down including the end bound, running 6 iterations (i=5,4,3,2,1,0)
	// and accumulating count = 6. The inclusive form must emit `>=` and `--`.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"exclusive descending", "fn main() i32 { var count i32 = 0; loop 5..0 : i { count = count + 1; } return count; }", 5},
		{"inclusive descending", "fn main() i32 { var count i32 = 0; loop 5..=0 : i { count = count + 1; } return count; }", 6},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRangeLoopNonLiteralEndBoundEvaluatedOnceCompilesAndRuns(t *testing.T) {
	// A non-literal end bound (a side-effecting helper call) must be evaluated
	// exactly once, not once per loop condition check. This is the tracker's
	// reproduction: bound() prints on every call, so the number of printed
	// lines counts the actual number of calls. Before the fix the emitted C
	// spliced pebble_fn_<callee>(ctx) directly into the for-condition, and C
	// re-evaluated it before every iteration, printing 4 times for a
	// 3-iteration loop; now the end value is cached in a pebble_temp_<id> C
	// local before the loop and the condition compares against that local, so
	// bound() runs once. The loop still iterates correctly (count = 3 is the
	// exit code) — the assertion here is about call count, not iteration
	// count. Bounded execution.
	src := "fn bound() int { print \"bound called\\n\"; return 3; } fn main() int { var count = 0; loop 0..bound() : i { count = count + 1; } return count; }"
	out := emitAndRunCaptureBounded(t, src, false, 3, false)
	if got := strings.Count(out, "bound called"); got != 1 {
		t.Fatalf("bound() called %d time(s), want exactly 1 (once for the whole loop); output:\n%s", got, out)
	}
}

func TestEmitRangeLoopNonLiteralEndBoundEvaluatedOnceWritesC(t *testing.T) {
	// The emitted C must cache a non-literal end bound in a pebble_temp_<id>
	// local declared before the loop, and the for-loop condition must compare
	// against that local rather than re-splicing the raw end expression — the
	// shape of the once-only evaluation fix. The temp line initializes from
	// the helper call (pebble_fn_...), and the for-header references the temp
	// name instead of that call.
	unit, snapshot, entryID, sources := buildFixture(t, "fn bound() int { print \"bound called\\n\"; return 3; } fn main() int { var count = 0; loop 0..bound() : i { count = count + 1; } return count; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	var tempLine, forLine string
	foundTemp, foundFor := false, false
	for _, line := range strings.Split(out, "\n") {
		if strings.Contains(line, "pebble_temp_") && strings.Contains(line, " = ") && strings.HasSuffix(line, ";") {
			tempLine = line
			foundTemp = true
		}
		if strings.Contains(line, "for (int32_t pebble_local_") {
			forLine = line
			foundFor = true
		}
	}
	if !foundTemp {
		t.Fatalf("emitted C missing a pebble_temp_<id> declaration for the end bound:\n%s", out)
	}
	if !strings.Contains(tempLine, "pebble_fn_") {
		t.Errorf("pebble_temp_ line does not initialize from the bound() call:\n%s", tempLine)
	}
	if !foundFor {
		t.Fatalf("emitted C missing the range-loop for-header:\n%s", out)
	}
	if !strings.Contains(forLine, "pebble_temp_") {
		t.Errorf("for-header compares against the raw end expression instead of the pebble_temp_<id> local:\n%s", forLine)
	}
	if strings.Contains(forLine, "pebble_fn_") {
		t.Errorf("for-header still re-splices the end-bound call expression:\n%s", forLine)
	}
}

func TestEmitRangeLoopNonLiteralDescendingBoundStaysAscendingCompilesAndRuns(t *testing.T) {
	// A non-literal descending scenario (start > runtime end value): the
	// checker allows it (it cannot know the end value at compile time), and
	// the descending-range fix is deliberately scoped to literal bounds only,
	// so a non-literal end bound keeps the ascending `<` + `++` lowering.
	// bound() returns 3, so 5..3 is an ascending range that is false on the
	// first check: zero iterations (count = 0, exit code 0), and the
	// once-only evaluation fix still holds (bound() called exactly once, by
	// the single condition check). Bounded execution.
	src := "fn bound() int { print \"bound called\\n\"; return 3; } fn main() int { var count = 0; loop 5..bound() : i { count = count + 1; } return count; }"
	out := emitAndRunCaptureBounded(t, src, false, 0, false)
	if got := strings.Count(out, "bound called"); got != 1 {
		t.Fatalf("bound() called %d time(s), want exactly 1; output:\n%s", got, out)
	}
}

func TestEmitZeroLengthRangeLoopCompilesAndRuns(t *testing.T) {
	// A zero-length range (start == end) must still correctly run zero times
	// — this is not a regression from the descending fix. Both exclusive and
	// inclusive forms of start == end must produce zero iterations (the
	// inclusive form would also produce zero when start > end, but start ==
	// end inclusive still runs once — check both).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"zero-length exclusive", "fn main() i32 { var count i32 = 0; loop 3..3 : i { count = count + 1; } return count; }", 0},
		{"zero-length inclusive", "fn main() i32 { var count i32 = 0; loop 3..=3 : i { count = count + 1; } return count; }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitDescendingRangeLoopWritesC(t *testing.T) {
	// The emitted C for a descending range loop must use `>` (or `>=` for
	// inclusive) as the condition and `--` as the step, confirming the
	// direction fix is reflected in the generated code.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var count i32 = 0; loop 5..0 : i { count = count + 1; } return count; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "for (int32_t pebble_local_") {
		t.Errorf("emitted C missing for-loop header:\n%s", out)
	}
	if !strings.Contains(out, "pebble_local_") || !strings.Contains(out, "--") {
		t.Errorf("emitted C missing decrement step for descending range:\n%s", out)
	}
	// Inclusive descending: must use `>=` and `--`.
	unit, snapshot, entryID, sources = buildFixture(t, "fn main() i32 { var count i32 = 0; loop 5..=0 : i { count = count + 1; } return count; }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out = buf.String()
	if !strings.Contains(out, ">=") {
		t.Errorf("emitted C missing `>=` for inclusive descending range:\n%s", out)
	}
	if !strings.Contains(out, "--") {
		t.Errorf("emitted C missing decrement step for inclusive descending range:\n%s", out)
	}
}
