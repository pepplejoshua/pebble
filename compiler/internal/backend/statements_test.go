package backend

import (
	"bytes"
	"fmt"
	"regexp"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestEmitIntegerReturnEntryWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 42; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitIfElseEntryWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitLogicalIfElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitNestedIfDiamondCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitNestedIfEntryWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// The confirmation fixture: a real accumulation loop. i counts 0..4 and
	// sum accumulates i each pass, so sum = 0+1+2+3+4 = 10, returned as the
	// process exit code. This is the first program in the rewrite where a
	// loop actually iterates and accumulates across iterations. Execution is
	// bounded (compileAndRunBounded) so a miscompiled non-terminating loop
	// fails the test loudly instead of hanging it.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { sum = sum + i; i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitWhileNeverRunsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A loop whose condition is false before the first iteration: i = 10 is
	// not < 5, so the body never runs and x keeps its initial value 1. This
	// proves the emitted while does not run its body even once when the
	// condition is false at entry. Bounded execution in case of a
	// miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 10; var x i32 = 1; while i < 5 { x = 2; } return x; }", false, 1, false)
}

func TestEmitWhileCounterCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A simple counter with no accumulator, to isolate the loop mechanism
	// from the accumulation pattern: i goes 0 -> 1 -> 2 -> 3, then i < 3 is
	// false and the loop exits, returning 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; } return i; }", false, 3, false)
}

func TestEmitWhileOverflowInBodyAborts(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	// The exact std/hmap get_by_ref / remove shape: a `while true` whose body
	// is a conditional if whose arm returns, with no break anywhere. The loop
	// never falls through (every iteration either returns from the if arm or
	// loops again), so the non-void helper needs no trailing return after the
	// loop. Calling f(5) returns on the first pass with x = 5.
	emitAndRunBounded(t, "fn f(x i32) i32 { while true { if x == 5 { return x; } } } fn main() i32 { return f(5); }", false, 5, false)
}

func TestEmitIfElseInsideLoopBodyCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	// The confirmation fixture: an if with no else inside a loop body. i counts
	// 0..9 but sum accumulates only while i < 5, so sum = 0+1+2+3+4 = 10,
	// returned as the process exit code. The no-else If is exactly the
	// two-child shape confirmed against a real fixture dump (condition,
	// then-arm, no third child), and the emitter must produce an if block with
	// no else. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i < 5 { sum = sum + i; } i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitNoElseIfInLoopBodyWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A loop-body if with an else, both arms accumulating into distinct
	// enclosing locals: i counts 0..5, the then-arm counts evens (0, 2, 4) and
	// the else-arm counts odds (1, 3, 5), so even = 3 returned as the exit
	// code. The condition uses a checked modulo (`i % 2 == 0`), confirming
	// overflow-checked arithmetic is valid inside a loop-body if condition.
	// Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var even i32 = 0; var odd i32 = 0; while i < 6 { if i % 2 == 0 { even = even + 1; } else { odd = odd + 1; } i = i + 1; } return even; }", false, 3, false)
}

func TestEmitNestedWhileInLoopBodyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The nested double-loop confirmation fixture: i and j each count 0..2, so
	// the inner body runs 3 x 3 = 9 times and total = 9, returned as the exit
	// code. The inner While is a plain statement inside the outer loop's body
	// Block (the shape confirmed against a real fixture dump), and buildWhile
	// recurses into buildLoopBody for its own body unchanged. Bounded
	// execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { var j i32 = 0; while j < 3 { total = total + 1; j = j + 1; } i = i + 1; } return total; }", false, 9, false)
}

func TestEmitOverflowInsideNestedLoopIfAborts(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// && as a while condition, the shape 10.11 rejected: i counts 1..4 while
	// both sides hold (i < 5 && i > 0), then at i = 5 the left side fails and
	// the loop exits with i = 5 as the exit code. Bounded execution in case of
	// a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 1; while i < 5 && i > 0 { i = i + 1; } return i; }", false, 5, false)
}

func TestEmitLogicalOrWhileCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// || as a while condition: i counts 0..4 through the left side (i < 5),
	// then at i = 5 both sides are false (i < 5 || i == 10) and the loop
	// exits with i = 5. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 5 || i == 10 { i = i + 1; } return i; }", false, 5, false)
}

func TestEmitBoolWhileNegationLoopCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	// The break-inside-a-loop-body-if fixture: i counts 0..9 but the loop
	// breaks when i == 5, so sum accumulates 0+1+2+3+4 = 10, returned as the
	// process exit code. The Break is a leaf node (no children, confirmed
	// against a fixture dump) in the then-arm of a no-else loop-body if and
	// must emit exactly `break;` at the arm's indentation. Bounded execution in
	// case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i == 5 { break; } sum = sum + i; i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitContinueInsideLoopIfCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The continue-inside-a-loop-body-if fixture: i counts 1..5, skipping the
	// accumulation when i == 3, so sum = 1+2+4+5 = 12, returned as the process
	// exit code. The Continue is a leaf node in the then-arm of a no-else
	// loop-body if and must emit exactly `continue;`. Bounded execution in case
	// of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { i = i + 1; if i == 3 { continue; } sum = sum + i; } return sum; }", false, 12, false)
}

func TestEmitNestedLoopBreakTargetsInnerLoopCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A bare break directly in the loop body (not inside an if), the simplest
	// loop-body jump: i advances to 1 then the break exits the loop, so the
	// return reads 1. Shares the same leaf-node dispatch as the inside-if
	// fixtures. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; break; } return i; }", false, 1, false)
}

func TestEmitContinueDirectInLoopBodyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A bare continue directly in the loop body (not inside an if): i advances
	// each iteration before the continue, which skips the accumulation that
	// follows, so total stays 0 and the loop still terminates. This proves the
	// continue actually jumps to the loop's next iteration rather than falling
	// through the rest of the body. Bounded execution in case of a miscompiled
	// loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { i = i + 1; continue; total = total + 1; } return total; }", false, 0, false)
}

func TestEmitBreakInsideLoopIfWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
			t.Parallel()
			src := "fn main() i32 { let x " + tc.width + " = 7; print x; return 0; }"
			out := emitAndRunCapture(t, src, false, 0, false)
			if want := "7\n"; out != want {
				t.Fatalf("compiled program output = %q, want %q", out, want)
			}
		})
	}
}

func TestEmitPrintBoolCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintCharCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintStrCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintInterpolatedBoolCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintFloatCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintMultipleOperandsOneLineCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintWritesSingleCombinedPrintf(t *testing.T) {
	t.Parallel()
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

func TestEmitPrintStructOfScalarsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Composite print slice 1: a struct whose fields are all scalar types
	// prints as `<TypeName>{ <field>: <value>, ... }` in the struct's DECLARED
	// field order, followed by the statement's single trailing newline. Each
	// field value uses the exact scalar formatting a bare print operand uses
	// (the exact-width PRI* integer macros, "true"/"false", the str .data
	// projection), and the labels are the struct's own declared type name and
	// SOURCE field names — never the generated pebble_field_<member> C names.
	// Mixed operands prove a struct composes with scalar operands on one line,
	// and the helper-call operand proves a struct-returning call is
	// materialized once (evaluated a single time).
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"two ints", "type Point = struct { x int; y int; };\nfn main() i32 { let p = Point.{ x = 1, y = 2 }; print p; return 0; }", "Point{ x: 1, y: 2 }\n"},
		{"int bool str", "type Person = struct { name str; active bool; age i32; };\nfn main() i32 { let p = Person.{ name = \"ann\", active = true, age = 30 }; print p; return 0; }", "Person{ name: ann, active: true, age: 30 }\n"},
		{"signed and unsigned widths", "type Counts = struct { id i64; n u64; big uint; };\nfn main() i32 { let c = Counts.{ id = -5, n = 7, big = 9 }; print c; return 0; }", "Counts{ id: -5, n: 7, big: 9 }\n"},
		{"inline literal", "type Point = struct { x int; y int; };\nfn main() i32 { print Point.{ x = 3, y = 4 }; return 0; }", "Point{ x: 3, y: 4 }\n"},
		{"mixed with scalar operands", "type Point = struct { x int; y int; };\nfn main() i32 { let p = Point.{ x = 1, y = 2 }; print p, \" and \", 42; return 0; }", "Point{ x: 1, y: 2 } and 42\n"},
		{"helper call operand", "type Point = struct { x int; y int; };\nfn make_point(x int) Point { return Point.{ x = x, y = x + 1 }; }\nfn main() i32 { print make_point(10); return 0; }", "Point{ x: 10, y: 11 }\n"},
		{"two struct operands", "type Point = struct { x int; y int; };\nfn main() i32 { let a = Point.{ x = 1, y = 2 }; let b = Point.{ x = 3, y = 4 }; print a, b; return 0; }", "Point{ x: 1, y: 2 }Point{ x: 3, y: 4 }\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintStructWritesSequentialFprintfs(t *testing.T) {
	t.Parallel()
	// A struct operand's print is emitted as DIRECT SEQUENTIAL fprintf(stdout,
	// ...) calls — proposal 17's storage policy (no intermediate dynamic
	// string, so no dependency on the unfinished Allocator/Context redesign) —
	// one label call and one value call per field, with the operand's value
	// materialized once into a per-operand temp and every field read off that
	// temp. Asserting the literal C text proves the direct sequential shape
	// and that a struct print never folds into the single combined printf the
	// scalar-only path uses. The field projections are matched by regex (their
	// member symbol IDs depend on the fixture's symbol numbering); the labels
	// and punctuation are asserted verbatim.
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x int; y int; };\nfn main() i32 { let p = Point.{ x = 1, y = 2 }; print p; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	tempRE := regexp.MustCompile(`pebble_struct_\d+_t (pebble_print_struct_\d+) = pebble_local_\d+;`)
	match := tempRE.FindStringSubmatch(out)
	if match == nil {
		t.Fatalf("emitted C missing the struct operand's materialized temp:\n%s", out)
	}
	temp := match[1]
	combinedRE := regexp.MustCompile(`(?m)^\s*printf\(`)
	if combinedRE.MatchString(out) {
		t.Errorf("emitted C still contains a combined printf call for the struct print:\n%s", out)
	}
	valueRE := regexp.MustCompile(`fprintf\(stdout, "%"PRId32, ` + regexp.QuoteMeta(temp) + `\.pebble_field_\d+\);`)
	if values := valueRE.FindAllString(out, -1); len(values) != 2 {
		t.Errorf("emitted C has %d struct field value fprintf calls, want 2:\n%s", len(values), out)
	}
	for _, want := range []string{
		`fprintf(stdout, "Point{ x: ");`,
		`fprintf(stdout, ", y: ");`,
		`fprintf(stdout, " }""\n");`,
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitPrintStructFieldNamesAreSourceNames(t *testing.T) {
	t.Parallel()
	// The printed field labels are the struct's own SOURCE field names in
	// declared order — the fields are declared `y` before `x` here, so the
	// label order must be y, x regardless of the construction-site order
	// (`Point.{ x = 1, y = 2 }` lists x first). The generated C names
	// (pebble_field_<member>) must never appear in the output.
	out := emitAndRunCapture(t, "type Point = struct { y int; x int; };\nfn main() i32 { let p = Point.{ x = 1, y = 2 }; print p; return 0; }", false, 0, false)
	if want := "Point{ y: 2, x: 1 }\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitPrintSliceOfScalarsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Composite print slice 4: a slice operand prints as `[<e0>, <e1>, <e2>]`
	// with `, ` separators, and an EMPTY slice prints `[]`. A slice's element
	// COUNT is a runtime value — unlike a fixed array's compile-time length —
	// so the element formatter is generated once at Go-compile-time and
	// executed N times at C runtime inside a real for-loop. Every scalar
	// element is formatted by the same buildScalarPrintParts a bare scalar
	// print operand uses. The inline-construction operand (`print arr[:]`)
	// proves a CheckedSlice print operand's checked-start temp statement is
	// hosted as a leading pre-statement; the partial-range operand proves the
	// loop iterates the slice's runtime .len, not the backing array's length.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"multi-element", "fn main() i32 { var arr [3]int = [1, 2, 3]; var s []int = arr[:]; print s; return 0; }", "[1, 2, 3]\n"},
		{"empty", "fn main() i32 { var arr [3]int = [1, 2, 3]; var s []int = arr[0:0]; print s; return 0; }", "[]\n"},
		{"partial range", "fn main() i32 { var arr [3]int = [1, 2, 3]; var s []int = arr[1:3]; print s; return 0; }", "[2, 3]\n"},
		{"inline construction", "fn main() i32 { var arr [3]int = [1, 2, 3]; print arr[:]; return 0; }", "[1, 2, 3]\n"},
		{"bool elements", "fn main() i32 { var arr [2]bool = [true, false]; var s []bool = arr[:]; print s; return 0; }", "[true, false]\n"},
		{"char elements", "fn main() i32 { var arr [2]char = ['x', 'é']; var s []char = arr[:]; print s; return 0; }", "[x, é]\n"},
		{"mixed with scalar operands", "fn main() i32 { var arr [3]int = [1, 2, 3]; var s []int = arr[:]; print s, \" and \", 42; return 0; }", "[1, 2, 3] and 42\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintSliceOfCompositeElementsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The runtime loop's element formatter routes through the SAME
	// buildPrintValueCalls recursion the fixed-aggregate slice-3 work uses, so
	// a slice of structs or tuples prints each element with its full nested
	// `TypeName{ ... }` / `(...)` sequence inline inside the loop body — the
	// element formatter is generated against the element TYPE, only the
	// iteration count is dynamic (proposal 17 slice 4's second half).
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"slice of struct", "type Point = struct { x int; y int; };\nfn main() i32 { var arr [2]Point = [Point.{ x = 1, y = 2 }, Point.{ x = 3, y = 4 }]; var s []Point = arr[:]; print s; return 0; }", "[Point{ x: 1, y: 2 }, Point{ x: 3, y: 4 }]\n"},
		{"slice of tuple", "fn main() i32 { var arr [2](int, str) = [(1, \"a\"), (2, \"b\")]; var s [](int, str) = arr[:]; print s; return 0; }", "[(1, a), (2, b)]\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintSliceWritesRuntimeLoop(t *testing.T) {
	t.Parallel()
	// A slice operand's print is a REAL C runtime for-loop over the slice's
	// .len — the runtime-determined element count means the element formatter
	// can never be unrolled the way a fixed array's is. Asserting the literal
	// C text proves the dynamic-loop shape: the materialized temp declaration,
	// `[` punctuation, one `for (size_t pebble_print_i_<id> = 0; ... <
	// pebble_print_slice_<id>.len; ...++)` loop whose body is the separator
	// guard plus the single element fprintf reading <temp>.data[<i>], and the
	// `]` closing punctuation with the statement's trailing newline. A
	// slice-returning CALL operand must be materialized exactly once into the
	// temp — the loop reads the temp, never re-calls the helper, so the
	// helper call text appears exactly once in the emitted C.
	unit, snapshot, entryID, sources := buildFixture(t, "fn make() []int { var a [2]int = [10, 20]; return a[:]; }\nfn main() i32 { print make(); return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	tempRE := regexp.MustCompile(`pebble_slice_\d+_t (pebble_print_slice_\d+) = pebble_fn_\d+\(ctx\);`)
	match := tempRE.FindStringSubmatch(out)
	if match == nil {
		t.Fatalf("emitted C missing the slice operand's materialized temp from the call:\n%s", out)
	}
	temp := match[1]
	i := "pebble_print_i_" + regexp.QuoteMeta(strings.TrimPrefix(temp, "pebble_print_slice_"))
	loopRE := regexp.MustCompile(`(?m)^\s*for \(size_t ` + i + ` = 0; ` + i + ` < ` + regexp.QuoteMeta(temp) + `\.len; ` + i + `\+\+\) \{`)
	if !loopRE.MatchString(out) {
		t.Errorf("emitted C missing the runtime for-loop over the slice's .len:\n%s", out)
	}
	guard := regexp.MustCompile(`(?m)^\s*if \(` + i + ` != 0\) fprintf\(stdout, ", "\);`)
	if !guard.MatchString(out) {
		t.Errorf("emitted C missing the loop's element-separator guard:\n%s", out)
	}
	valueRE := regexp.MustCompile(`fprintf\(stdout, "%"PRId32, ` + regexp.QuoteMeta(temp) + `\.data\[` + i + `\]\);`)
	if count := len(valueRE.FindAllString(out, -1)); count != 1 {
		t.Errorf("emitted C has %d slice element value fprintf calls inside the loop, want exactly 1:\n%s", count, out)
	}
	for _, want := range []string{
		`fprintf(stdout, "[");`,
		`fprintf(stdout, "]""\n");`,
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	combinedRE := regexp.MustCompile(`(?m)^\s*printf\(`)
	if combinedRE.MatchString(out) {
		t.Errorf("emitted C still contains a combined printf call for the slice print:\n%s", out)
	}
	if callCount := len(regexp.MustCompile(`pebble_fn_\d+\(ctx\)`).FindAllString(out, -1)); callCount != 1 {
		t.Errorf("emitted C calls the slice-returning helper %d time(s), want exactly once (the temp initializer):\n%s", callCount, out)
	}
}

func TestEmitPrintEnumOfVariantsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Composite print slice 5: a plain enum operand prints as
	// `<TypeName>.<variant>` — the DECLARED enum type name, a literal dot, and
	// the matching variant's declared SOURCE name — selected by a runtime tag
	// comparison. Each variant asserts its exact output, so the tag-to-name
	// mapping is proven correct across the whole enum, not just coincidentally
	// for the first variant. The variant literal operand (`print Color.green`)
	// proves the EnumVariantValue shape, the helper-call operand proves an
	// enum-returning call is materialized once, and mixed scalar operands
	// prove an enum composes with the one-line print rule.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"green", "type Color = enum { red, green, blue };\nfn main() i32 { let c = Color.green; print c; return 0; }", "Color.green\n"},
		{"red", "type Color = enum { red, green, blue };\nfn main() i32 { let c = Color.red; print c; return 0; }", "Color.red\n"},
		{"blue", "type Color = enum { red, green, blue };\nfn main() i32 { let c = Color.blue; print c; return 0; }", "Color.blue\n"},
		{"inline variant literal", "type Color = enum { red, green, blue };\nfn main() i32 { print Color.green; return 0; }", "Color.green\n"},
		{"helper call operand", "type Color = enum { red, green, blue };\nfn pick() Color { return Color.blue; }\nfn main() i32 { print pick(); return 0; }", "Color.blue\n"},
		{"mixed with scalar operands", "type Color = enum { red, green, blue };\nfn main() i32 { let a = Color.red; let b = Color.green; print a, \" then \", b; return 0; }", "Color.red then Color.green\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintEnumWritesRuntimeSwitch(t *testing.T) {
	t.Parallel()
	// An enum operand's print is emitted as DIRECT SEQUENTIAL fprintf(stdout,
	// ...) calls with the operand's value materialized once into a per-operand
	// temp, followed by ONE runtime C switch over the temp's stored
	// discriminant — one case per declared variant emitting the variant's
	// full `Color.<name>` string, plus a defensive default case emitting
	// `Color<invalid: %d>` for an out-of-range discriminant. The trailing
	// newline rides on an empty-string label call (`fprintf(stdout, """"\n");`
	// = `fprintf(stdout, "\n");`) that follows the raw switch, because a raw
	// block cannot receive buildSequentialPrint's newline append. Asserting
	// the literal C text proves the runtime-switch shape, and that an enum
	// print never folds into the single combined printf the scalar-only path
	// uses. An enum-returning CALL operand must be materialized exactly once
	// into the temp — the switch reads the temp, never re-calls the helper,
	// so the helper call text appears exactly once in the emitted C.
	unit, snapshot, entryID, sources := buildFixture(t, "type Color = enum { red, green, blue };\nfn pick() Color { return Color.blue; }\nfn main() i32 { print pick(); return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	tempRE := regexp.MustCompile(`pebble_enum_\d+_t (pebble_print_enum_\d+) = pebble_fn_\d+\(ctx\);`)
	match := tempRE.FindStringSubmatch(out)
	if match == nil {
		t.Fatalf("emitted C missing the enum operand's materialized temp from the call:\n%s", out)
	}
	temp := match[1]
	switchRE := regexp.MustCompile(`(?m)^\s*switch \(` + regexp.QuoteMeta(temp) + `\) \{`)
	if !switchRE.MatchString(out) {
		t.Errorf("emitted C missing the runtime switch over the enum's discriminant:\n%s", out)
	}
	if count := len(regexp.MustCompile(`case pebble_variant_\d+:`).FindAllString(out, -1)); count != 3 {
		t.Errorf("emitted C has %d enum variant case labels, want 3 (red, green, blue):\n%s", count, out)
	}
	for _, want := range []string{
		`fprintf(stdout, "Color.red");`,
		`fprintf(stdout, "Color.green");`,
		`fprintf(stdout, "Color.blue");`,
		`fprintf(stdout, "Color<invalid: %d>", ` + temp + `);`,
		`fprintf(stdout, """\n");`,
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	combinedRE := regexp.MustCompile(`(?m)^\s*printf\(`)
	if combinedRE.MatchString(out) {
		t.Errorf("emitted C still contains a combined printf call for the enum print:\n%s", out)
	}
	if callCount := len(regexp.MustCompile(`pebble_fn_\d+\(ctx\)`).FindAllString(out, -1)); callCount != 1 {
		t.Errorf("emitted C calls the enum-returning helper %d time(s), want exactly once (the temp initializer):\n%s", callCount, out)
	}
}

func TestEmitPrintTaggedUnionOfVariantsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Composite print slice 6: a tagged union operand prints as
	// `<TypeName>.<variant>(<payload>)` for a payload-carrying variant — the
	// declared type name, a dot, the matching variant's declared source name,
	// then the payload's own value recursively formatted in parens — or as
	// bare `<TypeName>.<variant>` (no parens) for a void-payload variant.
	// Each case asserts its exact output, proving the tag-to-variant mapping
	// and the payload-vs-no-payload formatting are both correct, not just
	// coincidentally right for the first variant.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"payload variant", "type Result = union enum { ok i32; error str; };\nfn main() i32 { let r = Result.ok(42); print r; return 0; }", "Result.ok(42)\n"},
		{"second payload variant", "type Result = union enum { ok i32; error str; };\nfn main() i32 { let r = Result.error(\"failed\"); print r; return 0; }", "Result.error(failed)\n"},
		{"payload-less variant", "type Status = union enum { done void; error str; };\nfn main() i32 { let s = Status.done; print s; return 0; }", "Status.done\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintTaggedUnionWritesRuntimeSwitch(t *testing.T) {
	t.Parallel()
	// A tagged union operand's print is emitted as direct sequential
	// fprintf(stdout, ...) calls with the operand materialized once into a
	// per-operand temp, followed by ONE runtime C switch over the temp's
	// `.tag` discriminant — one case per declared variant, recursing into
	// the payload projection for a payload-carrying variant, plus a
	// defensive default case for an out-of-range discriminant, mirroring
	// the plain-enum switch shape from slice 5. Both payload-carrying
	// variants must actually be CONSTRUCTED somewhere in the unit: the C
	// union typedef only allocates a payload member for a constructed
	// variant (the same existing convention narrowed union-variant payload
	// access relies on, see unionVariantPayloadMember), so a variant never
	// constructed anywhere would fall back to the bare no-parens form here
	// for an unrelated reason and make this assertion meaningless.
	unit, snapshot, entryID, sources := buildFixture(t, "type Result = union enum { ok i32; error str; };\nfn main() i32 { let r = Result.ok(42); let e = Result.error(\"x\"); print r; print e; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if count := len(regexp.MustCompile(`(?m)^\s*switch \(.*\.tag\) \{`).FindAllString(out, -1)); count != 2 {
		t.Errorf("emitted C has %d switch(...tag) statements, want exactly 2 (one per print statement):\n%s", count, out)
	}
	for _, want := range []string{
		`fprintf(stdout, "Result.ok(");`,
		`fprintf(stdout, "Result.error(");`,
		`fprintf(stdout, ")");`,
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if !regexp.MustCompile(`Result<invalid-tag: %d>|Result<invalid: %d>`).MatchString(out) {
		t.Errorf("emitted C missing the defensive invalid-tag default case:\n%s", out)
	}
	combinedRE := regexp.MustCompile(`(?m)^\s*printf\(`)
	if combinedRE.MatchString(out) {
		t.Errorf("emitted C still contains a combined printf call for the union print:\n%s", out)
	}
}

func TestEmitPrintTaggedUnionRejectsNonPrintablePayload(t *testing.T) {
	t.Parallel()
	// A union with a not-yet-printable payload type in ANY declared variant
	// (a pointer, here) must be rejected by the checker, mirroring slice 1's
	// conservative struct-field rule: the checker cannot know at compile
	// time which variant will be active at runtime, so it must reject the
	// whole type if any variant's payload isn't printable.
	_, _, _, _, err := buildFixtureMaybeFailing(t, "type Box = union enum { p *i32; n i32; };\nfn main() i32 { let b = Box.n(1); print b; return 0; }", "main", false)
	if err == nil {
		t.Fatalf("expected a checker error rejecting a union with a pointer-payload variant, got none")
	}
	if !strings.Contains(err.Error(), "C0612") {
		t.Fatalf("expected a C0612 (not printable) error, got: %v", err)
	}
}

func TestEmitDeferredPrintCharCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
			t.Parallel()
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRangeLoopBreakAndContinueCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A range loop nested inside another range loop: the inner RangeLoop is a
	// plain statement in the outer loop's body Block, dispatched by
	// buildLoopBody exactly like a nested While. i and j each count 0..2, so
	// the inner body runs 3 x 3 = 9 times and total = 9, returned as the exit
	// code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; loop 0..3 : i { loop 0..3 : j { total = total + 1; } } return total; }", false, 9, false)
}

func TestEmitRangeLoopInsideWhileCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A range loop nested inside a while loop's body: the RangeLoop is a
	// statement in the while's body Block, dispatched by buildLoopBody. Each
	// of the 2 while iterations runs the range loop's 3 inner iterations, so
	// total = 6, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 2 { loop 0..3 : j { total = total + 1; } i = i + 1; } return total; }", false, 6, false)
}

func TestEmitRangeLoopNonLiteralBoundsCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRangeLoopIteratorComparisonOnlyCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A bound range loop whose iterator is never read in the body: the loop
	// still iterates over its C counter (the condition and increment read it),
	// so the body runs 3 times and sum = 3. The bounds are int-typed literals
	// (nothing anchors them), lowered as their decimal text. Bounded
	// execution.
	emitAndRunBounded(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + 1; } return sum; }", false, 3, false)
}

func TestEmitRangeLoopNestedIteratorAsInnerBoundCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A nested range loop whose bound reads the outer loop's iterator
	// (`loop 0..i : j`): the outer iterator is an int-typed SymbolValue in the
	// inner loop's end position, lowered as its pebble_local_<symbol> name by
	// buildRangeBound's int-typed-SymbolValue case. The inner loop runs 0, 1,
	// and 2 iterations for i = 0, 1, 2, so total = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; loop 0..3 : i { loop 0..i : j { total = total + 1; } } return total; }", false, 3, false)
}

func TestEmitRangeLoopRuntimeBoundsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The two reproductions of the silent-zero-iteration defect. A descending
	// range whose bounds are only known at runtime must still descend: a
	// descending range where start is 3 and end is 0 runs 3 iterations (i =
	// 3, 2, 1), not zero. Both repros return the iteration count as the exit
	// code, proving the body ran the right number of times. Bounded execution.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"runtime function bounds", "fn start_val() int { return 3; } fn end_val() int { return 0; } fn main() int { var total int = 0; loop start_val()..end_val() : i { total = total + 1; } return total; }", 3},
		{"negative literal end bound", "fn main() int { var total int = 0; loop 0..-5 : i { total = total + 1; } return total; }", 5},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRangeLoopNegativeLiteralEndBoundValuesCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The negative-literal descending reproduction, checking the iterator's
	// actual values as well as the count by printing each one: `loop 0..-5`
	// visits 0, -1, -2, -3, -4 (exclusive of -5) in that order and `loop
	// 0..=-5` adds -5, so the captured output is "0-1-2-3-4" and
	// "0-1-2-3-4-5" and the exit code is the iteration count (5 and 6).
	// Bounded execution.
	for _, tc := range []struct {
		name string
		src  string
		want int
		out  string
	}{
		{"exclusive", "fn main() int { var total int = 0; loop 0..-5 : i { print i; total = total + 1; } return total; }", 5, "0-1-2-3-4"},
		{"inclusive", "fn main() int { var total int = 0; loop 0..=-5 : i { print i; total = total + 1; } return total; }", 6, "0-1-2-3-4-5"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCaptureBounded(t, tc.src, false, tc.want, false)
			if got := strings.ReplaceAll(out, "\n", ""); got != tc.out {
				t.Errorf("iterator values printed %q, want %q", got, tc.out)
			}
		})
	}
}

func TestEmitRangeLoopBoundsEvaluatedStartBeforeEndCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The evaluation-order bug Sol's audit flagged separately: the old lowering
	// materialized the END bound's call before the loop's own for-header, so a
	// side-effecting end bound ran before a side-effecting start bound. The
	// runtime-direction lowering evaluates the START bound into its local
	// first, then the END bound second, as two sequential C statements, so
	// mark_start() must print before mark_end(). The captured output is
	// asserted to be exactly "se", and the loop still counts 0..3 = 3
	// iterations (exit code 3). Bounded execution.
	src := "fn mark_start() int { print \"s\"; return 0; } fn mark_end() int { print \"e\"; return 3; } fn main() int { var count int = 0; loop mark_start()..mark_end() : i { count = count + 1; } return count; }"
	out := emitAndRunCaptureBounded(t, src, false, 3, false)
	if got := strings.Count(out, "s"); got != 1 {
		t.Errorf("mark_start() printed %d time(s), want exactly once; output:\n%s", got, out)
	}
	if got := strings.Count(out, "e"); got != 1 {
		t.Errorf("mark_end() printed %d time(s), want exactly once; output:\n%s", got, out)
	}
	if si := strings.Index(out, "s"); si < 0 || strings.Index(out, "e") < si {
		t.Errorf("start bound did not run before the end bound; output:\n%s", out)
	}
}

func TestEmitRangeLoopI64EntryCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A range loop inside an i64 entry: the iterator's C type follows the
	// entry's width (int64_t), and the bounds/iterator are anchored to i64 by
	// the i64 accumulation. sum = 0+1+2 = 3, returned as the exit code.
	// Bounded execution.
	emitAndRunBounded(t, "fn main() i64 { var sum i64 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", false, 3, false)
}

func TestEmitRangeLoopWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for the flagship fixture must be the always-runtime-direction
	// lowering (V1's src/codegen.c AST_STMT_LOOP shape): both bounds materialized
	// into C locals first (start, then end, at the loop's bound type), the step
	// local computed once by comparing them, and the for loop's init/condition/
	// increment all referencing those locals with a ternary-on-the-step
	// condition: `for (int32_t pebble_local_28 = pebble_temp_22; (pebble_step_28
	// > 0) ? (pebble_local_28 < pebble_temp_23) : (pebble_local_28 >
	// pebble_temp_23); pebble_local_28 += pebble_step_28)`. Symbols 22/23 (the
	// bound literal nodes), 27 (sum), and 28 (the iterator) come from the real
	// fixture dump. The inclusive form is instead gated by a done local whose
	// test flips the direction operators to `>=`/`<=` (RangeLoop.RangeInclusive)
	// — the wrap-safe shape for unsigned iterators — so the two end rules are
	// distinguishable in the emitted text.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    int32_t pebble_temp_22 = 0;\n",
		"    int32_t pebble_temp_23 = 3;\n",
		"    int32_t pebble_step_28 = (pebble_temp_22 <= pebble_temp_23) ? 1 : -1;\n",
		"    for (int32_t pebble_local_28 = pebble_temp_22; (pebble_step_28 > 0) ? (pebble_local_28 < pebble_temp_23) : (pebble_local_28 > pebble_temp_23); pebble_local_28 += pebble_step_28) {\n",
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
	for _, want := range []string{
		"    int32_t pebble_temp_22 = 0;\n",
		"    int32_t pebble_temp_23 = 3;\n",
		"    int32_t pebble_done_28 = 0;\n",
		"    for (int32_t pebble_local_28 = pebble_temp_22; !pebble_done_28; pebble_done_28 |= (pebble_step_28 > 0) ? (pebble_local_28 >= pebble_temp_23) : (pebble_local_28 <= pebble_temp_23), pebble_local_28 += pebble_step_28) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitForLoopAccumulationCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The flagship fixture: a classic for loop lowers to a C for loop with
	// the same three clauses. step counts 0..3 (the initializer declares it as
	// an ordinary local of the entry's width, seeded into the loop's scope),
	// and total accumulates step each pass, so total = 0+1+2 = 3, returned as
	// the process exit code. The body references step through the seeded scope
	// like any other local. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; step = step + 1 { total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopAllClausesOmittedCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// for ;; { ... } — all three clauses absent: an infinite loop from the
	// header's perspective, so termination comes only from the explicit break
	// in the body. The body advances its own counter (declared outside) and
	// breaks at 3, so the program terminates with i = 3 as the exit code.
	// Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; for ;; { if i >= 3 { break; } i = i + 1; } return i; }", false, 3, false)
}

func TestEmitForLoopNoConditionNeedsBreakCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// for var i i32 = 0;; i = i + 1 { ... } — no condition clause (the
	// initializer and update are both present), so termination comes only from
	// the explicit break in the body — the omitted-condition combination the
	// brief calls out explicitly. total accumulates 0+1+2 = 3 before the break
	// at i == 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0;; i = i + 1 { if i >= 3 { break; } total = total + i; } return total; }", false, 3, false)
}

func TestEmitForLoopNoUpdateCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// for var step i32 = 0; step < 3; { ... } — no update clause; the body
	// advances step itself. total accumulates 0+1+2 = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; { total = total + step; step = step + 1; } return total; }", false, 3, false)
}

func TestEmitForLoopInitializerOnlyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// for var i i32 = 0;; { ... } — initializer only (no condition, no
	// update), so the body advances i and breaks when it reaches 3. total
	// accumulates 0+1+2 = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0;; { if i >= 3 { break; } total = total + i; i = i + 1; } return total; }", false, 3, false)
}

func TestEmitForLoopUpdateOnlyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// for ; ; i = i + 1 { ... } — update only (no initializer, no condition):
	// i is declared outside, the header's update advances it, and the body
	// breaks when it reaches 3, so i = 3 is returned. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; for ; ; i = i + 1 { if i >= 3 { break; } } return i; }", false, 3, false)
}

func TestEmitForLoopConditionOnlyCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	// A classic for loop nested inside a while loop's body: the For is a
	// statement in the while's body Block, dispatched by buildLoopBody. Each
	// of the 2 while iterations runs the for loop's 3 inner iterations, so
	// total = 6, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 2 { for var j i32 = 0; j < 3; j = j + 1 { total = total + 1; } i = i + 1; } return total; }", false, 6, false)
}

func TestEmitForLoopNestedInRangeLoopCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A classic for loop nested inside a range loop's body: the For is a
	// statement in the range loop's body Block, dispatched by buildLoopBody.
	// Each of the 3 range iterations runs the for loop's 2 inner iterations,
	// so total = 6, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; loop 0..3 : i { for var j i32 = 0; j < 2; j = j + 1 { total = total + 1; } } return total; }", false, 6, false)
}

func TestEmitForLoopNestedInForLoopCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A classic for loop nested inside another classic for loop: the inner For
	// is a statement in the outer for's body Block, dispatched by
	// buildLoopBody exactly like a nested While or RangeLoop. i and j each
	// count 0..3, so the inner body runs 3 x 3 = 9 times and total = 9,
	// returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0; i < 3; i = i + 1 { for var j i32 = 0; j < 3; j = j + 1 { total = total + 1; } } return total; }", false, 9, false)
}

func TestEmitForLoopI64EntryCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A classic for loop inside an i64 entry: the initializer's C type follows
	// the entry's width (int64_t). total accumulates 0+1+2 = 3, returned as
	// the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i64 { var total i64 = 0; for var step i64 = 0; step < 3; step = step + 1 { total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopBoolInitializerAndConditionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The bool grammar works in every for-loop clause: the initializer
	// declares a bool local (built by buildScalarInitializeCore's bool case),
	// the condition is a bare bool value (buildCondition -> buildBoolExpr), and
	// the update reassigns the bool (buildStoreCore's bool case). The loop
	// runs once (first is true), accumulates 1, flips first to false, and the
	// next condition check stops it, so total = 1. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var first bool = true; first; first = false { total = total + 1; } return total; }", false, 1, false)
}

func TestEmitForLoopLogicalConditionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A && condition in a for header goes through buildCondition ->
	// buildBoolExpr -> buildComparison exactly as an if/while condition does.
	// i counts 0..3 under the && condition, so total = 0+1+2 = 3. Bounded
	// execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0; i < 3 && i >= 0; i = i + 1 { total = total + i; } return total; }", false, 3, false)
}

func TestEmitForLoopWritesC(t *testing.T) {
	t.Parallel()
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

func TestEmitForLoopAssignmentInitializerCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An assignment-form initializer (`for step = 0; ...`) reassigns a local
	// already declared in the enclosing block instead of declaring a new one —
	// the ordinary pattern of reusing an existing variable as a loop counter.
	// step is seeded to 0 in the header, counts 0..3, and total accumulates
	// 0+1+2 = 3, returned as the process exit code. Bounded execution in case
	// of a miscompiled loop.
	emitAndRunBounded(t, "fn main() int { var step int = 0; var total int = 0; for step = 0; step < 3; step = step + 1 { total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopAssignmentBoolInitializerCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The assignment form also works for a bool-typed loop counter: the
	// initializer reassigns the already-declared bool first (buildStoreCore's
	// bool case), the condition is the bare bool value, and the update flips it
	// to false. The loop runs once (first is true), accumulates 1, and the
	// next condition check stops it, so total = 1. Bounded execution.
	emitAndRunBounded(t, "fn main() int { var total int = 0; var first bool = true; for first = true; first; first = false { total = total + 1; } return total; }", false, 1, false)
}

func TestEmitForLoopAssignmentInitializerNoConditionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The assignment-form initializer works without a condition clause too:
	// `for step = 0;; step = step + 1 { ... }` reassigns the already-declared
	// step in the header's initializer slot (positionally distinguished from
	// the update), with termination from the explicit break in the body. total
	// accumulates 0+1+2 = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() int { var step int = 0; var total int = 0; for step = 0;; step = step + 1 { if step >= 3 { break; } total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopAssignmentInitializerWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for an assignment-form initializer must have NO
	// declaration in the header's init clause: the clause is the bare
	// assignment `pebble_local_<symbol> = <expr>` (the reassigned variable
	// was already declared at block level before the for), and because nothing
	// new is declared there is no -Wunused-variable (void) cast emitted as the
	// body's first statement (contrast the declaration form, whose cast is
	// asserted in TestEmitForLoopWritesC). The body's first statement is
	// directly the accumulation. Symbols 27 (step) and 28 (total) come from
	// the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var step int = 0; var total int = 0; for step = 0; step < 3; step = step + 1 { total = total + step; } return total; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    for (pebble_local_27 = 0; pebble_local_27 < 3; pebble_local_27 = pebble_rt_checked_add_i32(pebble_local_27, 1, (PebbleSourceLoc){\"main.peb\", 1, 85})) {\n",
		"        pebble_local_28 = pebble_rt_checked_add_i32(pebble_local_28, pebble_local_27, (PebbleSourceLoc){\"main.peb\", 1, 104});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "        (void)pebble_local_27;") {
		t.Errorf("emitted C has a (void) cast for the assignment-form initializer's variable, but nothing new was declared:\n%s", out)
	}
}

func TestEmitI64ReturnEntryWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	emitAndRun(t, "fn main() i64 { return 42; }", false, 42, false)
}

func TestEmitI64WhileAccumulationCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The full control-flow story at i64: locals, mutation, a while loop, and
	// checked arithmetic all at the wider width. i counts 0..4 and sum
	// accumulates i each pass, so sum = 0+1+2+3+4 = 10, returned as the
	// process exit code. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i64 { var i i64 = 0; var sum i64 = 0; while i < 5 { sum = sum + i; i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitI64WhileWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let x f64 = 2147483648.0; return (x as i32) + 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 1, false, false)
}

func TestEmitStrEqualityInWhileCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// Two strings that share a prefix but differ in length: "hi" vs "hi!".
	// The shorter string must sort first (matching strcmp's convention for a
	// prefix — the shorter one is "less"), so "hi" < "hi!" is true and the
	// then-arm runs, exiting 10. This proves the shorter-string-sorts-first
	// tie-break behaves correctly at runtime.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi!\"; if s < t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrIndexRangeLoopIteratorCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A range loop's iterator used directly as a str index arrives as an
	// int-typed SymbolValue (the unanchored-int case, the same shortcut
	// buildArrayPlaceRead handles), confirmed against a real fixture dump.
	// Iterating 0..2 over "hi" and counting each match of 'h' (only index 0)
	// proves the iterator's C name is the correct index lvalue.
	emitAndRunBounded(t, "fn main() i32 { let s str = \"hi\"; var n i32 = 0; loop 0..2 : i { if s[i] == 'h' { n = n + 1; } } return n; }", false, 1, false)
}

func TestEmitSwitchMultiValueCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The flagship fixture: a switch with a multi-value case (1, 2 share the
	// same body returning 10), a single-value case (3 returning 30), and an
	// else (default returning 0). Subject value 1 hits the multi-value case
	// and returns 10.
	emitAndRun(t, "fn main() i32 { switch 1 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 10, false)
}

func TestEmitSwitchMultiValueCaseSecondValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same switch as above but subject value 2 — still hits the multi-value
	// case and returns 10, confirming both SwitchCase nodes sharing the same
	// body produce the same result.
	emitAndRun(t, "fn main() i32 { switch 2 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 10, false)
}

func TestEmitSwitchSingleValueCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Subject value 3 hits the single-value case and returns 30.
	emitAndRun(t, "fn main() i32 { switch 3 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 30, false)
}

func TestEmitSwitchElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Subject value 99 hits the else/default arm and returns 0.
	emitAndRun(t, "fn main() i32 { switch 99 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchBlockCaseBodyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A block-wrapped (braced, multi-statement) case body: the case declares
	// a local and returns an expression using it. This exercises the
	// Block-bodied path in buildSwitchCaseBody.
	emitAndRun(t, "fn main() i32 { switch 1 { case 1: { let x i32 = 42; return x; } else: return 0; } }", false, 42, false)
}

func TestEmitSwitchBareReturnCaseBodyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A bare single-statement case body (no braces): `case 1: return 10;`.
	// This exercises the bare-statement path in buildSwitchCaseBody.
	emitAndRun(t, "fn main() i32 { switch 1 { case 1: return 10; else: return 0; } }", false, 10, false)
}

func TestEmitSwitchBoolSubjectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A bool subject with bool case values: `switch true { case true: return
	// 1; else: return 0; }`. Bool case values are emitted as `case 1:` (true)
	// and `case 0:` (false) in C, since C switch requires integral constants.
	emitAndRun(t, "fn main() i32 { switch true { case true: return 1; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchBoolSubjectFalseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Bool subject `false` hits the else/default arm.
	emitAndRun(t, "fn main() i32 { switch false { case true: return 1; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchBoolParamSubjectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The proposal-13 defect, reproduced verbatim: a bool PARAMETER as the
	// switch subject used to emit a raw C `switch (bool)` and fail the
	// mandated -Wswitch-bool -Werror at the cc step. The backend now casts the
	// subject to int32_t for the C switch header only, so this exact program
	// must compile and run, returning 1.
	emitAndRun(t, "fn choose(flag bool) int { switch flag { case true: return 1; else: return 0; } } fn main() int { return choose(true); }", false, 1, false)
}

func TestEmitSwitchBoolSubjectWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C for a bool switch: the subject is the bool
	// parameter's C bool local, cast to int32_t in the C switch (...) header —
	// `switch ((int32_t)pebble_local_25)` — so the controlling expression is
	// an integer rather than a C bool, the -Wswitch-bool fix. The bool case
	// labels stay `case 1:` (true) / `case 0:` (false), which an int32_t
	// switch compares correctly against; the else arm is the default label.
	unit, snapshot, entryID, sources := buildFixture(t, "fn choose(flag bool) int { switch flag { case true: return 1; else: return 0; } } fn main() int { return choose(true); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"switch ((int32_t)pebble_local_25)",
		"case 1:",
		"default:",
		"return 1;",
		"return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 1, false)
}

func TestEmitSwitchBoolExhaustiveNoElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A bool switch with both case true and case false and no else arm: the
	// checker proves exhaustiveness for a bool subject, and the backend emits
	// `case 1:`/`case 0:` labels under an int32_t-cast subject. C cannot prove
	// a bool switch is exhaustive the way it can a C enum's, so the trailing
	// `return 999;` — which the checker flags as C0618, tolerated by
	// buildFixture — is the same fallback the exhaustive u8 switch test uses
	// to keep the emitted C warning-free under -Werror=return-type (a
	// pre-existing, documented backend limitation for exhaustive integer
	// switches). Running both arms through choose proves the matching is
	// correct across true and false.
	emitAndRun(t, "fn choose(flag bool) int { switch flag { case true: return 1; case false: return 0; } return 999; } fn main() int { var a int = 0; if choose(false) == 0 { a = a + 1; } if choose(true) == 1 { a = a + 2; } return a; }", false, 3, false)
}

func TestEmitSwitchCharSubjectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char-typed switch subject with char case labels and an else/default
	// arm, the checker/backend parity gap from proposal 13: the checker
	// accepts a char switch (char subject, char case values) but the backend
	// previously rejected it at emission because the switch-subject builder
	// had no char branch and buildCaseLabel only handled integer/bool labels.
	// classify('b') must hit the second case and return 2.
	emitAndRun(t, "fn classify(c char) int { switch c { case 'a': return 1; case 'b': return 2; else: return 0; } } fn main() int { return classify('b'); }", false, 2, false)
}

func TestEmitSwitchCharSubjectFirstCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same switch, subject 'a' hits the first case and returns 1.
	emitAndRun(t, "fn classify(c char) int { switch c { case 'a': return 1; case 'b': return 2; else: return 0; } } fn main() int { return classify('a'); }", false, 1, false)
}

func TestEmitSwitchCharSubjectElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same switch, subject 'z' (not among the case labels) falls to the
	// else/default arm and returns 0.
	emitAndRun(t, "fn classify(c char) int { switch c { case 'a': return 1; case 'b': return 2; else: return 0; } } fn main() int { return classify('z'); }", false, 0, false)
}

func TestEmitSwitchCharSubjectNonAsciiCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char switch over a non-ASCII Unicode scalar value: 'é' (U+00E9, 233)
	// is matched as the full int32_t scalar, proving the char case label
	// compares the same full scalar the subject carries, not a truncated byte.
	emitAndRun(t, "fn classify(c char) int { switch c { case 'a': return 1; case 'é': return 2; else: return 0; } } fn main() int { return classify('é'); }", false, 2, false)
}

func TestEmitSwitchCharSubjectLiteralCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char literal used directly as the switch subject — the CharLiteral
	// path in the subject builder, distinct from the char-parameter SymbolValue
	// path the fixtures above exercise. Subject 'a' (97) hits the 'a' case.
	emitAndRun(t, "fn main() int { switch 'a' { case 'a': return 1; case 'b': return 2; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchU8SubjectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact reproduction from proposal 13's active defect: a u8-typed
	// switch subject with an integer case label and an else/default arm. The
	// checker accepts a u8 switch and proves exhaustiveness correctly, but the
	// backend previously rejected it at emission because the switch-subject
	// type gate only accepted the entry's width. Subject x = 5 hits the case
	// and returns 1.
	emitAndRun(t, "fn main() int { let x u8 = 5; switch x { case 5: return 1; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchU8SubjectElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same u8 switch, subject x = 9 (not among the case labels) falls to the
	// else/default arm and returns 0, confirming the else arm is reachable for
	// a non-entry-width integer subject.
	emitAndRun(t, "fn main() int { let x u8 = 9; switch x { case 5: return 1; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchI16SubjectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A second non-entry-width integer subject: an i16-typed local. Signed, so
	// its case labels are emitted without the unsigned suffix; subject x = 5
	// hits the case and returns 1.
	emitAndRun(t, "fn main() int { let x i16 = 5; switch x { case 5: return 1; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchU32SubjectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A wider unsigned non-entry-width subject: u32. Subject x = 5 hits the
	// case and returns 1.
	emitAndRun(t, "fn main() int { let x u32 = 5; switch x { case 5: return 1; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchUintSubjectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact reproduction from proposal 13's active defect: a uint-typed
	// switch subject with an integer case label and an else/default arm. The
	// checker accepts a uint switch and proves exhaustiveness correctly, but
	// the backend previously rejected it at emission because the switch-
	// subject dispatch had no uint branch (2b3d684's fixed-width widening
	// deliberately excluded uint, the word-sized unsigned builtin distinct
	// from u64). Subject x = 5 hits the case and returns 1.
	emitAndRun(t, "fn main() int { let x uint = 5; switch x { case 5: return 1; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchUintSubjectElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same uint switch, subject x = 9 (not among the case labels) falls to
	// the else/default arm and returns 0, confirming the else arm is
	// reachable for a uint subject.
	emitAndRun(t, "fn main() int { let x uint = 9; switch x { case 5: return 1; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchUintSubjectWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C for a uint switch: the subject is the uint local
	// declared at its own uint64_t width (uint's C type, cType(types.Uint)),
	// and the integer case label is emitted at uint's OWN width — `case 5u:`,
	// the same unsigned suffix integerLiteralText gives a uint value
	// everywhere — so the label's C type matches the uint64_t subject. The
	// else arm is the default label.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { let x uint = 5; switch x { case 5: return 1; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"uint64_t pebble_local_27 = 5u;",
		"switch (pebble_local_27)",
		"case 5u:",
		"default:",
		"return 1;",
		"return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 1, false)
}

func TestEmitSwitchU8ExhaustiveNoElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An exhaustive u8 switch covering all 256 values (0..255) with no else
	// arm: the checker proves exhaustiveness for a u8 subject (the 4817dae
	// fix), and the backend must now accept the u8 subject and emit every case
	// label at the subject's own uint8_t width. Subject x = 200 hits case 200
	// and returns 200, proving the case matching is correct across the whole
	// domain. The trailing `return 999;` is an unreachable fallback the checker
	// flags as C0618 (tolerated by buildFixture, which only requires the check
	// to succeed): C cannot prove a uint8_t switch is exhaustive the way it can
	// a C enum's (clang reasons about enum domains), so without it the emitted
	// C would fail -Werror=return-type — a separate, pre-existing backend
	// limitation for exhaustive INTEGER switches (a no-else enum switch emits
	// identically and compiles), out of scope for this item.
	var b strings.Builder
	b.WriteString("fn main() int {\n    let x u8 = 200;\n    switch x {\n")
	for i := 0; i <= 255; i++ {
		fmt.Fprintf(&b, "        case %d: return %d;\n", i, i)
	}
	b.WriteString("    }\n    return 999;\n}\n")
	emitAndRun(t, b.String(), false, 200, false)
}

func TestEmitSwitchU8SubjectWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C for a u8 switch: the subject is the u8 local
	// declared at its own uint8_t width, and the integer case label is emitted
	// at the subject's OWN width — `case 5u:`, the same unsigned suffix
	// integerLiteralText gives a u8 value everywhere — so the label's C type
	// matches the uint8_t subject rather than a silently narrower/signed
	// constant. The else arm is the default label.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { let x u8 = 5; switch x { case 5: return 1; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"uint8_t pebble_local_27 = 5u;",
		"switch (pebble_local_27)",
		"case 5u:",
		"default:",
		"return 1;",
		"return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 1, false)
}

func TestEmitSwitchCharSubjectWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C for a char switch: the subject is the char
	// parameter's int32_t local, and each char case label is emitted as
	// `case (int32_t)<scalar>:` — the same int32_t spelling buildCharOperand
	// gives a char literal, so the labels match the subject's C type.
	unit, snapshot, entryID, sources := buildFixture(t, "fn classify(c char) int { switch c { case 'a': return 1; case 'b': return 2; else: return 0; } } fn main() int { return classify('b'); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"switch (pebble_local_25)",
		"case (int32_t)97:",
		"case (int32_t)98:",
		"default:",
		"return 1;",
		"return 2;",
		"return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitSwitchWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// The emitted C for a switch must compile under -Wall -Wextra -Werror
	// with no warnings. This exercises the full cc compilation path.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { switch 1 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 10, false)
}

func TestEmitSwitchNegativeCaseLabelI16CompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact reproduction from proposal 13's active defect: a negative
	// integer literal case label on a signed non-entry-width subject. The
	// checker stores the label as the canonical big.Int text "-5", which
	// buildCaseLabel now emits as `case -5:` at the i16 subject's own width;
	// the subject's `-5` initializer folds to the same negative constant. x =
	// -5 hits the case and returns 1.
	emitAndRun(t, "fn main() int { let x i16 = -5; switch x { case -5: return 1; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchNegativeCaseLabelI16NonMatchCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same i16 negative-label switch, subject x = -7 (not among the case
	// labels): the negative case label still compiles and runs, and the
	// non-matching subject falls to the else/default arm and returns 0.
	emitAndRun(t, "fn main() int { let x i16 = -7; switch x { case -5: return 1; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchNegativeCaseLabelEntryWidthIntCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A negative case label on an entry-width int subject: the int-typed
	// parameter `v` is built directly as its C local and the label is emitted
	// at the unanchored int width (`case -5:`). classify(-5) hits the case and
	// returns 1.
	emitAndRun(t, "fn classify(v int) int { switch v { case -5: return 1; else: return 0; } } fn main() int { return classify(-5); }", false, 1, false)
}

func TestEmitSwitchNegativeCaseLabelEntryWidthIntNonMatchCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same entry-width int negative-label switch, subject 7 (not among the
	// case labels): the non-matching subject falls to the else/default arm and
	// returns 0.
	emitAndRun(t, "fn classify(v int) int { switch v { case -5: return 1; else: return 0; } } fn main() int { return classify(7); }", false, 0, false)
}

func TestEmitSwitchNegativeCaseLabelWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C for a negative i16 case label: the case label is
	// spelled `case -5:` — the same negative decimal text a negative literal
	// emits in any other position — so it matches the int16_t subject rather
	// than a silently unsigned reinterpretation. The subject's `-5`
	// initializer itself now calls pebble_rt_checked_neg_i16 (Phase 3 #23
	// gave i16 its own checked-negation helper, alongside i8; before that fix
	// i16 had no helper at all, so an ordinary literal negation folded to a
	// compile-time constant instead — literal folding is now reserved for
	// the width's unspellable-minimum edge case only, see
	// TestEmitCheckedNegateLiteralMinimumWritesC).
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { let x i16 = -5; switch x { case -5: return 1; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_local_27 = pebble_rt_checked_neg_i16(5,",
		"switch (pebble_local_27)",
		"case -5:",
		"default:",
		"return 1;",
		"return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 1, false)
}

func TestEmitCheckedNegateLiteralNarrowWidthCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A negative literal at i8: since Phase 3 #23 gave i8 its own
	// pebble_rt_checked_neg_i8 helper (alongside i16), an ordinary literal
	// negation like this one now compiles through that helper rather than
	// folding to a compile-time constant — literal folding is reserved for
	// the width's unspellable-minimum edge case only (see
	// TestEmitCheckedNegateLiteralMinimumWidthCompilesAndRuns). Either way
	// the result must be correct end to end: x = -5 cast to the entry int
	// returns -5, whose OS-visible low byte is 251.
	emitAndRun(t, "fn main() int { let x i8 = -5; return x as int; }", false, 251, false)
}

func TestEmitCheckedNegateLiteralMinimumWidthCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A negative literal at the exact signed minimum of a width WITH a
	// pebble_rt_checked_neg_* runtime helper (i32/int/i64) folds at emission
	// to the width's own minimum C constant instead of calling the helper with
	// the unspellable positive magnitude (2147483648/9223372036854775808 is
	// not a valid int32_t/int64_t constant, so cc fails under -Werror). Each
	// row reads the value back and compares it against its own minimum to
	// prove the full magnitude survives the compile-link-run round trip — the
	// OS process exit code cannot carry it, so the program returns 1 only if
	// the comparison holds.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"i32", "fn main() int { let x i32 = -2147483648; if x == -2147483648 { return 1; } return 0; }"},
		{"i64", "fn main() int { let x i64 = -9223372036854775808; if x == -9223372036854775808 { return 1; } return 0; }"},
		{"int", "fn main() int { let x int = -2147483648; if x == -2147483648 { return 1; } return 0; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, 1, false)
		})
	}
}

func TestEmitCheckedNegateLiteralMinimumWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for a literal negation at the width's exact signed minimum
	// must be the minimum's own C constant — `-2147483648` at i32/int,
	// `INT64_MIN` at i64 — never a pebble_rt_checked_neg_*(2147483648) call
	// whose positive magnitude is unspellable at that width. The no-regression
	// rows assert a NON-minimum negative literal at the same widths still
	// routes through the runtime helper exactly as before (only the minimum
	// magnitude is unspellable as a positive literal, so only it folds), and a
	// non-constant negation (`-y`) is unchanged too (still a helper call, not
	// folded).
	for _, tc := range []struct {
		name    string
		src     string
		want    string
		mustNot string
	}{
		{
			name:    "i32 minimum folds to its decimal constant",
			src:     "fn main() int { let x i32 = -2147483648; return 0; }",
			want:    "= -2147483648;",
			mustNot: "pebble_rt_checked_neg_i32",
		},
		{
			name:    "i64 minimum folds to INT64_MIN",
			src:     "fn main() int { let x i64 = -9223372036854775808; return 0; }",
			want:    "= INT64_MIN;",
			mustNot: "pebble_rt_checked_neg_i64",
		},
		{
			name:    "int minimum folds to its decimal constant",
			src:     "fn main() int { let x int = -2147483648; return 0; }",
			want:    "= -2147483648;",
			mustNot: "pebble_rt_checked_neg_i32",
		},
		{
			name:    "i32 non-minimum literal stays on the runtime helper",
			src:     "fn main() int { let x i32 = -100; return 0; }",
			want:    "pebble_rt_checked_neg_i32(100",
			mustNot: "= -100;",
		},
		{
			name:    "i64 non-minimum literal stays on the runtime helper",
			src:     "fn main() int { let x i64 = -100; return 0; }",
			want:    "pebble_rt_checked_neg_i64(100",
			mustNot: "= -100;",
		},
		{
			name:    "non-constant negation stays on the runtime helper",
			src:     "fn main() int { var y i32 = 5; var z i32 = -y; return 0; }",
			want:    "pebble_rt_checked_neg_i32(pebble_local_",
			mustNot: "",
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			unit, snapshot, entryID, sources := buildFixture(t, tc.src, "main", false)
			var buf bytes.Buffer
			if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
				t.Fatalf("Emit failed: %v", err)
			}
			out := buf.String()
			if !strings.Contains(out, tc.want) {
				t.Errorf("emitted C missing %q:\n%s", tc.want, out)
			}
			if tc.mustNot != "" && strings.Contains(out, tc.mustNot) {
				t.Errorf("emitted C unexpectedly contains %q:\n%s", tc.mustNot, out)
			}
		})
	}
}

func TestBuildCaseLabelNegativeIntegerLiteral(t *testing.T) {
	t.Parallel()
	// buildCaseLabel is the single place a negative integer case-label text
	// (the checker's canonical big.Int spelling, a leading `-` followed by
	// digits) is turned into a C case label. On every SIGNED subject width the
	// label is the literal's own negative decimal text at that width; on every
	// UNSIGNED width it is a clean rejection naming the negative literal,
	// never a silent reinterpretation as a huge unsigned constant.
	_, snapshot, _, _ := buildFixture(t, "fn main() int { return 0; }", "main", false)
	caseNode := tir.Node{
		Kind:    tir.SwitchCase,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "-5"},
	}
	for _, tc := range []struct {
		name  string
		width types.BuiltinKind
	}{
		{"i8", types.I8},
		{"i16", types.I16},
		{"i32", types.I32},
		{"i64", types.I64},
		{"int", types.Int},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := buildCaseLabel(snapshot, caseNode, tc.width)
			if err != nil {
				t.Fatalf("buildCaseLabel rejected a negative label on signed %s: %v", tc.name, err)
			}
			if want := "case -5:"; got != want {
				t.Errorf("buildCaseLabel = %q, want %q", got, want)
			}
		})
	}
	for _, tc := range []struct {
		name  string
		width types.BuiltinKind
	}{
		{"u8", types.U8},
		{"u16", types.U16},
		{"u32", types.U32},
		{"u64", types.U64},
		{"uint", types.Uint},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			_, err := buildCaseLabel(snapshot, caseNode, tc.width)
			if err == nil {
				t.Fatalf("buildCaseLabel accepted a negative label on unsigned %s, want rejection", tc.name)
			}
			if !strings.Contains(err.Error(), "negative integer literal") {
				t.Errorf("buildCaseLabel unsigned rejection %q does not name the negative literal", err)
			}
		})
	}
	// Malformed literal text is still rejected on a signed subject, and a
	// non-negative label is emitted unchanged.
	for _, text := range []string{"", "-", "--5", "-5x", "1-"} {
		bad := caseNode
		bad.Literal.IntegerNum = text
		if _, err := buildCaseLabel(snapshot, bad, types.I16); err == nil {
			t.Errorf("buildCaseLabel accepted malformed literal text %q on a signed subject", text)
		}
	}
	good := caseNode
	good.Literal.IntegerNum = "5"
	got, err := buildCaseLabel(snapshot, good, types.I16)
	if err != nil {
		t.Fatalf("buildCaseLabel rejected a non-negative label: %v", err)
	}
	if want := "case 5:"; got != want {
		t.Errorf("buildCaseLabel = %q, want %q", got, want)
	}
}

func TestEmitTopLevelGuardIfCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The minimal non-tail-if repro: a guard-clause if with no else, as a
	// leading statement in a top-level function body, followed by more code.
	// x = 5 takes the guard (x + 1 = 6); x = 0 falls through the guard to
	// the code after it (x + 10 = 10). Both calls run, so both the guard arm
	// and the fall-through code after the if are exercised.
	emitAndRun(t, "fn helper(x i32) i32 { if x > 0 { return x + 1; } return x + 10; } fn main() i32 { return helper(5) + helper(0); }", false, 16, false)
}

func TestEmitTopLevelGuardIfWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A top-level if/else (both arms present) as a leading statement,
	// followed by more code that runs after either arm: pick(1) takes the
	// then-arm (result = 10, then 11) and pick(0) takes the else-arm
	// (result = 20, then 20), so both arms plus the fall-through code after
	// the if are exercised.
	emitAndRun(t, "fn pick(x i32) i32 { var result i32 = 0; if x > 0 { result = 10; } else { result = 20; } result = result + x; return result; } fn main() i32 { return pick(1) + pick(0); }", false, 31, false)
}

func TestEmitTopLevelSwitchLeadingCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A switch as an ordinary top-level leading statement, followed by more
	// code: classify(1) hits the single-value case (11), classify(2) hits the
	// multi-value case (21), and classify(9) hits the else arm (31). Every
	// case body falls through to the statements after the switch, and none
	// ends in a return — the shape buildSwitch was previously unable to emit
	// in a non-tail position.
	emitAndRun(t, "fn classify(x i32) i32 { var result i32 = 0; switch x { case 1: result = result + 10; case 2, 3: result = result + 20; else: result = result + 30; } result = result + 1; return result; } fn main() i32 { return classify(1) + classify(2) + classify(9); }", false, 63, false)
}

func TestEmitTopLevelSwitchLeadingWithReturningCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A top-level switch whose case bodies may return OR fall through:
	// f(1) returns 99 from inside a case body, f(2) falls through case 2 to
	// the code after the switch (1 + 10 = 11), and f(9) falls through the
	// else arm (2 + 10 = 12). This confirms a case body's "may fall through"
	// grammar includes the return case, matching what buildSwitchCaseBody's
	// bare-Return path did for the tail position.
	emitAndRun(t, "fn f(x i32) i32 { var total i32 = 0; switch x { case 1: return 99; case 2: total = total + 1; else: total = total + 2; } return total + 10; } fn main() i32 { return f(1) + f(2) + f(9); }", false, 122, false)
}

func TestEmitSwitchInsideLoopBodyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The switch-in-loop repro: a switch as a statement inside a while loop
	// body, followed by more loop-body code. i counts 0, 1, 2; each switch
	// adds 1/2/3 by case, then the code after the switch adds 1, so total =
	// (1+1) + (2+1) + (3+1) = 9, returned as the exit code. This is the
	// position where If already worked but Switch did not. Bounded execution
	// in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { switch i { case 0: total = total + 1; case 1: total = total + 2; else: total = total + 3; } total = total + 1; i = i + 1; } return total; }", false, 9, false)
}

func TestEmitIfInsideSwitchCaseBodyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Nested control flow inside a fall-through switch case body: the case 2
	// body is itself an if/else whose arms reassign the enclosing local. x=2
	// hits the case 2 body's then-arm (total = 5), then the code after the
	// switch adds 1, so total = 6.
	emitAndRun(t, "fn main() i32 { var x i32 = 2; var total i32 = 0; switch x { case 1: total = total + 1; case 2: if x == 2 { total = total + 5; } else { total = total + 6; } else: total = total + 9; } total = total + 1; return total; }", false, 6, false)
}

func TestEmitSwitchInsideIfArmCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Nested control flow inside a top-level if arm: the then-arm is itself
	// a switch whose case bodies fall through, and the code after the if runs
	// after either arm. x=1 takes the then-arm's case 1 (total = 10), then
	// the fall-through code adds 1, so total = 11.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; var total i32 = 0; if x > 0 { switch x { case 1: total = total + 10; else: total = total + 20; } } else { total = total + 30; } total = total + 1; return total; }", false, 11, false)
}

func TestEmitIfAndSwitchInsideTopLevelIfArmCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Both a nested if and a nested switch inside a single top-level if arm,
	// plus more code after the outer if: x=1 takes the then-arm, whose own if
	// adds 1 and whose switch adds 10, then the outer fall-through code adds
	// 1, so total = 12.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; var y i32 = 2; var total i32 = 0; if x > 0 { if y > 0 { total = total + 1; } else { total = total + 2; } switch y { case 2: total = total + 10; else: total = total + 20; } } total = total + 1; return total; }", false, 12, false)
}

func TestEmitBreakAndContinueInsideLoopIfArmCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Regression: break and continue inside an if arm inside a while loop
	// body still work after the loop-body dispatch was reorganized into the
	// shared fall-through builder. i counts 1..9; even i continues past the
	// total accumulation, and i=5 breaks the loop. Only odd i < 5 accumulate
	// (1 and 3), so total = 4.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 10 { i = i + 1; if i % 2 == 0 { continue; } if i == 5 { break; } total = total + i; } return total; }", false, 4, false)
}

func TestEmitBreakInsideSwitchCaseBodyTargetsSwitchCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A return inside an if arm inside a loop body exits the function early —
	// reachable now that the enclosing function's result grammar threads
	// through the fall-through builder into loop bodies. f(3) returns 3 from
	// inside the loop; f(20) never matches and returns 99 after the loop, so
	// the exit code is 3 + 99 = 102.
	emitAndRunBounded(t, "fn f(x i32) i32 { var i i32 = 0; while i < 10 { if i == x { return i; } i = i + 1; } return 99; } fn main() i32 { return f(3) + f(20); }", false, 102, false)
}

func TestEmitSwitchI64EntryCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A switch with an i64 entry: the subject and case values are i64.
	emitAndRun(t, "fn main() i64 { switch 2 { case 1: return 100; case 2: return 200; else: return 0; } }", false, 200, false)
}

func TestEmitSwitchU64SubjectMultiValueCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A wide-width u64 subject (the 2^32..2^64 range, beyond every narrower
	// width's representable domain) with a multi-value case and an else arm:
	// `case 5000000000, 9000000000: return 200;` stacks two C case labels on
	// one body. Subject x = 9000000000 hits the multi-value case's second
	// label and returns 200, proving the subject is switched on at its OWN
	// u64 width — a subject or label silently truncated to a narrower width
	// could never match a 64-bit-only value.
	emitAndRun(t, "fn main() int { let x u64 = 9000000000; switch x { case 5000000000, 9000000000: return 200; case 3000000000: return 100; else: return 0; } }", false, 200, false)
}

func TestEmitSwitchU64SubjectMultiValueCaseFirstLabelCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same u64 switch, subject x = 5000000000: the FIRST label of the
	// multi-value case fires and returns 200, proving both stacked case
	// labels route to the shared body rather than only the second one.
	emitAndRun(t, "fn main() int { let x u64 = 5000000000; switch x { case 5000000000, 9000000000: return 200; case 3000000000: return 100; else: return 0; } }", false, 200, false)
}

func TestEmitSwitchU64SubjectSingleCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same u64 switch, subject x = 3000000000: the single-value case fires
	// and returns 100, distinct from the multi-value case's 200.
	emitAndRun(t, "fn main() int { let x u64 = 3000000000; switch x { case 5000000000, 9000000000: return 200; case 3000000000: return 100; else: return 0; } }", false, 100, false)
}

func TestEmitSwitchU64SubjectElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same u64 switch, subject x = 7000000000 (covered by no case label): the
	// else/default arm fires and returns 0, proving the else arm is reachable
	// and correctly selected on a wide-width unsigned subject.
	emitAndRun(t, "fn main() int { let x u64 = 7000000000; switch x { case 5000000000, 9000000000: return 200; case 3000000000: return 100; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchU64SubjectWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C for a u64 switch: the subject is the u64 local
	// declared at its own uint64_t width, and each integer case label is
	// emitted at the subject's OWN width with the "u" suffix every unsigned
	// value carries — `case 5000000000u:` — so a 64-bit-only label constant
	// matches the uint64_t subject instead of silently truncating to a
	// narrower constant. The else arm is the default label.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { let x u64 = 9000000000; switch x { case 5000000000, 9000000000: return 200; case 3000000000: return 100; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"uint64_t pebble_local_27 = 9000000000u;",
		"switch (pebble_local_27)",
		"case 5000000000u:",
		"case 9000000000u:",
		"case 3000000000u:",
		"default:",
		"return 200;",
		"return 100;",
		"return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 200, false)
}

func TestEmitSwitchI8SubjectNegativeMultiValueCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A narrow signed i8 subject with a multi-value case spanning a NEGATIVE
	// and a positive label: `case -5, 7: return 1;` — the negative label is
	// emitted as `case -5:` at the subject's own int8_t width, never a "u"-
	// suffixed unsigned reinterpretation. Subject x = -5 hits the multi-value
	// case and returns 1.
	emitAndRun(t, "fn main() int { let x i8 = -5; switch x { case -5, 7: return 1; case 2: return 2; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchI8SubjectNegativeMultiValueCasePositiveLabelCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same i8 switch, subject x = 7: the positive member of the negative/positive
	// multi-value label fires and returns 1, proving both labels route to the
	// shared body.
	emitAndRun(t, "fn main() int { let x i8 = 7; switch x { case -5, 7: return 1; case 2: return 2; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchI8SubjectSingleCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same i8 switch, subject x = 2: the single-value case fires and returns
	// 2, distinct from the multi-value case's 1.
	emitAndRun(t, "fn main() int { let x i8 = 2; switch x { case -5, 7: return 1; case 2: return 2; else: return 0; } }", false, 2, false)
}

func TestEmitSwitchI8SubjectElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same i8 switch, subject x = 3 (covered by no case label): the else/
	// default arm fires and returns 0, proving the else arm is reachable and
	// correctly selected on a narrow signed subject.
	emitAndRun(t, "fn main() int { let x i8 = 3; switch x { case -5, 7: return 1; case 2: return 2; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchI8SubjectWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C for an i8 switch: the case labels are spelled
	// `case -5:` / `case 7:` / `case 2:` — the same texts a signed literal
	// emits anywhere — so the labels match the int8_t subject rather than a
	// silent unsigned reinterpretation. The else arm is the default label.
	// The subject's `-5` initializer itself now calls
	// pebble_rt_checked_neg_i8 (Phase 3 #23 gave i8 its own checked-negation
	// helper, alongside i16; before that fix i8 had no helper at all, so an
	// ordinary literal negation folded to a compile-time constant instead —
	// literal folding is now reserved for the width's unspellable-minimum
	// edge case only, see TestEmitCheckedNegateLiteralMinimumWritesC).
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { let x i8 = -5; switch x { case -5, 7: return 1; case 2: return 2; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_local_27 = pebble_rt_checked_neg_i8(5,",
		"switch (pebble_local_27)",
		"case -5:",
		"case 7:",
		"case 2:",
		"default:",
		"return 1;",
		"return 2;",
		"return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 1, false)
}

func TestEmitSwitchU8SubjectMultiValueCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A narrow unsigned u8 subject with a multi-value case: `case 5, 200:
	// return 1;` stacks two C case labels on one body, emitted at the
	// subject's own uint8_t width. Subject x = 200 hits the multi-value case
	// and returns 1; the distinct single case (100 -> 2) proves the shared
	// body is genuinely the one selected.
	emitAndRun(t, "fn main() int { let x u8 = 200; switch x { case 5, 200: return 1; case 100: return 2; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchU8SubjectMultiValueCaseFirstLabelCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same u8 switch, subject x = 5: the FIRST label of the multi-value case
	// fires and returns 1, proving both stacked labels route to the shared
	// body rather than only the second one.
	emitAndRun(t, "fn main() int { let x u8 = 5; switch x { case 5, 200: return 1; case 100: return 2; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchU8SubjectMultiValueCaseElseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same u8 switch, subject x = 50 (covered by no case label): the else/
	// default arm fires and returns 0, proving the else arm coexists with a
	// multi-value case on a narrow unsigned subject.
	emitAndRun(t, "fn main() int { let x u8 = 50; switch x { case 5, 200: return 1; case 100: return 2; else: return 0; } }", false, 0, false)
}

func TestEmitStrSwitchCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-typed switch subject: the exact reproduction from proposal 13. A
	// str subject cannot use a native C switch (C switch labels must be
	// integer constants), so it is lowered as an if/else chain using
	// pebble_rt_str_eq. classify("b") must return 2, proving the branch
	// selection is correct.
	for _, tc := range []struct {
		name string
		arg  string
		want int
	}{
		{"case a", `"a"`, 1},
		{"case b", `"b"`, 2},
		{"else", `"c"`, 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			src := `fn classify(s str) int { switch s { case "a": return 1; case "b": return 2; else: return 0; } } fn main() int { return classify(` + tc.arg + `); }`
			emitAndRun(t, src, false, tc.want, false)
		})
	}
}

func TestEmitStrSwitchMultiLabelPerCaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str switch with multiple case labels sharing one arm: `case "a",
	// "c": return 1;` — the two equality checks are ORed into a single if
	// condition. classify("a") and classify("c") both return 1; classify("b")
	// returns 2; anything else returns 0.
	for _, tc := range []struct {
		name string
		arg  string
		want int
	}{
		{"a matches multi", `"a"`, 1},
		{"b single", `"b"`, 2},
		{"c matches multi", `"c"`, 1},
		{"d else", `"d"`, 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			src := `fn classify(s str) int { switch s { case "a", "c": return 1; case "b": return 2; else: return 0; } } fn main() int { return classify(` + tc.arg + `); }`
			emitAndRun(t, src, false, tc.want, false)
		})
	}
}

func TestEmitStrSwitchWriteC(t *testing.T) {
	t.Parallel()
	// The emitted C for a str switch must contain the pebble_rt_str_eq calls
	// and the if/else chain structure, not a native C switch.
	src := `fn classify(s str) int { switch s { case "a": return 1; case "b": return 2; else: return 0; } } fn main() int { return classify("b"); }`
	unit, snapshot, entryID, sources := buildFixture(t, src, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_rt_str_eq",
		"if (pebble_rt_str_eq(",
		"} else if (pebble_rt_str_eq(",
		"} else {",
		".data = (const uint8_t *)\"a\"",
		".data = (const uint8_t *)\"b\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "switch (") {
		t.Errorf("emitted C contains native switch, want if/else chain:\n%s", out)
	}
}

func TestEmitStrSwitchCallSubjectEvaluatedOnce(t *testing.T) {
	t.Parallel()
	// A call-valued str switch subject with a side effect must be evaluated
	// exactly once per switch, not once per case comparison. The if/else-chain
	// lowering splices the subject's C text into every pebble_rt_str_eq call,
	// so without materializing the subject into a temp the helper would print
	// "called" once per case label (twice here: against "a" then "b"). This is
	// the exact reproduction from proposal 13. The subject is materialized
	// into a PebbleStr temp before the chain, so the helper runs once total
	// and the "b" case still returns 0.
	out := emitAndRunCapture(t, "fn choose() str {\n    print \"called\";\n    return \"b\";\n}\nfn main() int {\n    switch choose() {\n        case \"a\": return 1;\n        case \"b\": return 0;\n        else: return 2;\n    }\n}\n", false, 0, false)
	if out != "called\n" {
		t.Fatalf("compiled program output = %q, want %q (subject helper evaluated exactly once)", out, "called\n")
	}
}

func TestEmitStrSwitchCallSubjectMaterializedOnce(t *testing.T) {
	t.Parallel()
	// The emitted C must materialize a call-valued str switch subject into a
	// PebbleStr temp exactly once, then reference that temp in every
	// pebble_rt_str_eq call — the same evaluate-once-into-a-per-operand-temp
	// pattern the composite print operands use. The helper call text appears
	// exactly once (the temp's initializer), never spliced into a case
	// comparison.
	unit, snapshot, entryID, sources := buildFixture(t, "fn choose() str { return \"b\"; }\nfn main() int { switch choose() { case \"a\": return 1; case \"b\": return 0; else: return 2; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	tempRE := regexp.MustCompile(`(?m)^\s*PebbleStr (pebble_switch_str_\d+) = pebble_fn_\d+\(ctx\);`)
	match := tempRE.FindStringSubmatch(out)
	if match == nil {
		t.Fatalf("emitted C missing the subject's materialized PebbleStr temp from the call:\n%s", out)
	}
	temp := match[1]
	if count := len(regexp.MustCompile(`pebble_rt_str_eq\(`+regexp.QuoteMeta(temp)+`, `).FindAllString(out, -1)); count != 2 {
		t.Errorf("emitted C has %d pebble_rt_str_eq calls against the subject temp, want 2 (cases \"a\" and \"b\"):\n%s", count, out)
	}
	if callCount := len(regexp.MustCompile(`pebble_fn_\d+\(ctx\)`).FindAllString(out, -1)); callCount != 1 {
		t.Errorf("emitted C calls the subject helper %d time(s), want exactly once (the temp initializer):\n%s", callCount, out)
	}
}

func TestEmitStrSwitchBreakCaseBodyUsesTempCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A break inside a str switch case body wraps the whole if/else chain in
	// do { ... } while (0) so the emitted break has a valid C target. The
	// materialized subject temp must live inside that wrapper, one temp per
	// switch evaluation, and the break must exit only the switch, not the
	// enclosing loop. choose() is called as the subject once per loop
	// iteration ("called" prints once per iteration = 3 total, never 6 for
	// the two case comparisons), the never-hit break on case "a" proves the
	// wrapper gives break a valid target without intercepting the loop, and
	// the after-switch accumulation still runs each iteration (total = 3 *
	// 11 = 33), proving the break does not leak to the loop.
	out := emitAndRunCaptureBounded(t, "fn choose() str { print \"called\"; return \"b\"; }\nfn main() i32 { var total i32 = 0; var i i32 = 0; while i < 3 { switch choose() { case \"a\": break; case \"b\": total = total + 10; else: total = total + 1; } total = total + 1; i = i + 1; } return total; }", false, 33, false)
	if out != "called\ncalled\ncalled\n" {
		t.Fatalf("compiled program output = %q, want %q (one subject evaluation per switch)", out, "called\ncalled\ncalled\n")
	}
}

func TestEmitDeferBeforeReturnCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A single defer running before a return, observably changing the returned
	// value. var x i32 = 0; defer x = x + 1; return x; should return 1, not 0,
	// proving the deferred Store executes before the return value is read.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; defer x = x + 1; return x; }", false, 1, false)
}

func TestEmitTwoDefersLIFOOrderCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Two defers in the same scope, proving LIFO (last-registered-first)
	// order. The second-registered defer (x = x + 10) must run before the
	// first (x = x * 2). Starting from x=1: first defer registers x*2, then
	// x+10. LIFO means x+10 runs first (1+10=11), then x*2 (11*2=22).
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 1; defer x = x * 2; defer x = x + 10; return x; }", false, 22, false)
}

func TestEmitDeferInsideIfArmFiresCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A defer inside an if-arm whose exit (return) is inside that same arm.
	// The defer fires because the return's DeferChain includes it. Both arms
	// return, so the if is the block's tail. Condition true: defer x=x+1
	// runs, return 0+1=1. Condition false: no defer, return 2.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; if x == 0 { defer x = x + 1; return x; } else { return 2; } }", false, 1, false)
}

func TestEmitDeferOutsideIfDoesNotFireCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A defer before a break inside a loop. The break's DeferChain includes
	// the deferred Store, so it fires before the break. Loop: i counts 0..5,
	// defer x=x+1 fires on break when i==3, break exits. x starts at 10,
	// after defer x=11. Exit code 11.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 10; var i i32 = 0; while i < 5 { if i == 3 { defer x = x + 1; break; } i = i + 1; } return x; }", false, 11, false)
}

func TestEmitDeferBeforeContinueCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A char-typed local declared and compared inside a while-loop body: the
	// loop runs three passes, each declaring c='a' and summing 1 when c == 'a',
	// exiting with 3 — proving char locals work through the loop-body
	// leading-statement and condition paths.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 3 { let c char = 'a'; if c == 'a' { sum = sum + 1; } i = i + 1; } return sum; }", false, 3, false)
}

func TestEmitDescendingRangeLoopCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A descending range loop (start > end): loop 5..0 counts down, running
	// 5 iterations (i=5,4,3,2,1) and accumulating count = 5. This is the
	// reproduction of the zero-iteration bug: the old emitter unconditionally
	// emitted `<` and `++`, producing `for (i = 5; i < 0; i++)` which is
	// false on the first check and never executes the body.
	emitAndRunBounded(t, "fn main() i32 { var count i32 = 0; loop 5..0 : i { count = count + 1; } return count; }", false, 5, false)
}

func TestEmitDescendingRangeLoopInclusiveCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRangeLoopNonLiteralEndBoundEvaluatedOnceCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// The always-runtime-direction lowering materializes BOTH bounds into
	// pebble_temp_<id> locals declared before the loop, so the end bound's call
	// (pebble_fn_...) appears exactly once, in the end-temp declaration line,
	// and the for-loop condition compares against those locals rather than
	// re-splicing the raw end expression — the shape of the once-only, in-order
	// (start then end) evaluation guarantee.
	unit, snapshot, entryID, sources := buildFixture(t, "fn bound() int { print \"bound called\\n\"; return 3; } fn main() int { var count = 0; loop 0..bound() : i { count = count + 1; } return count; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	var startTempLine, endTempLine, forLine string
	foundStartTemp, foundEndTemp, foundFor := false, false, false
	for _, line := range strings.Split(out, "\n") {
		if strings.Contains(line, "pebble_temp_") && strings.Contains(line, " = ") && !strings.Contains(line, " ? ") && strings.HasSuffix(line, ";") {
			if strings.Contains(line, "pebble_fn_") {
				endTempLine = line
				foundEndTemp = true
			} else {
				startTempLine = line
				foundStartTemp = true
			}
		}
		if strings.Contains(line, "for (int32_t pebble_local_") {
			forLine = line
			foundFor = true
		}
	}
	if !foundStartTemp {
		t.Fatalf("emitted C missing the start-bound pebble_temp_<id> declaration:\n%s", out)
	}
	if !strings.Contains(startTempLine, "= 0") {
		t.Errorf("start-temp line does not initialize from the literal start bound:\n%s", startTempLine)
	}
	if !foundEndTemp {
		t.Fatalf("emitted C missing an end-bound pebble_temp_<id> declaration from the bound() call:\n%s", out)
	}
	if !strings.Contains(endTempLine, "pebble_fn_") {
		t.Errorf("end-temp line does not initialize from the bound() call:\n%s", endTempLine)
	}
	if !foundFor {
		t.Fatalf("emitted C missing the range-loop for-header:\n%s", out)
	}
	if !strings.Contains(forLine, "pebble_temp_") {
		t.Errorf("for-header compares against the raw bound expressions instead of the pebble_temp_<id> locals:\n%s", forLine)
	}
	if strings.Contains(forLine, "pebble_fn_") {
		t.Errorf("for-header still re-splices the end-bound call expression:\n%s", forLine)
	}
}

func TestEmitRangeLoopRuntimeDirectionWritesC(t *testing.T) {
	t.Parallel()
	// The runtime-direction lowering with call-valued bounds: both bound calls
	// land in pebble_temp_<id> locals in source order (start line first, then
	// end line), the step local is computed at runtime from comparing them,
	// and the for-loop header is a ternary on the step so the same lowering
	// serves ascending and descending runtime bounds alike. The start-temp line
	// must precede the end-temp line (the evaluation-order guarantee), and the
	// for-header must reference the temps, not the raw pebble_fn_ calls.
	src := "fn start_val() int { return 3; } fn end_val() int { return 0; } fn main() int { var total int = 0; loop start_val()..end_val() : i { total = total + 1; } return total; }"
	unit, snapshot, entryID, sources := buildFixture(t, src, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// The two bound-call temps are the two lines that reference pebble_fn_ and
	// end in `;`; they appear in source order (start first, end second). Helpers
	// lower to pebble_fn_<symbolID>, so the lines are distinguished by order,
	// not by source name.
	var boundTempLines []string
	stepIdx, forIdx := -1, -1
	var forLine string
	for _, line := range strings.Split(out, "\n") {
		switch {
		case strings.Contains(line, "pebble_temp_") && strings.Contains(line, "pebble_fn_") && strings.HasSuffix(line, ";"):
			boundTempLines = append(boundTempLines, line)
		case strings.Contains(line, " ? 1 : -1;"):
			stepIdx = strings.Index(out, line)
		case strings.Contains(line, "for (int32_t pebble_local_") && strings.Contains(line, "pebble_step_"):
			forIdx = strings.Index(out, line)
			forLine = line
		}
	}
	if len(boundTempLines) != 2 {
		t.Fatalf("emitted C has %d bound-call temp line(s), want exactly 2 (start then end):\n%s", len(boundTempLines), out)
	}
	startIdx := strings.Index(out, boundTempLines[0])
	endIdx := strings.Index(out, boundTempLines[1])
	if startIdx < 0 || endIdx < 0 || startIdx > endIdx {
		t.Errorf("start bound not evaluated before end bound (start at %d, end at %d):\n%s", startIdx, endIdx, out)
	}
	if stepIdx < 0 {
		t.Errorf("emitted C missing the runtime step local ((start <= end) ? 1 : -1):\n%s", out)
	}
	if forIdx < 0 {
		t.Errorf("emitted C missing the ternary-on-step for-loop header:\n%s", out)
	}
	if forIdx >= 0 {
		if !strings.Contains(forLine, "> 0) ? (") || !strings.Contains(forLine, ") : (") {
			t.Errorf("for-header is not the ternary-on-step runtime-direction shape:\n%s", forLine)
		}
		if strings.Contains(forLine, "pebble_fn_") {
			t.Errorf("for-header re-splices the bound calls instead of the temps:\n%s", forLine)
		}
	}
}

func TestEmitRangeLoopNonLiteralDescendingBoundCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A non-literal descending scenario (start > runtime end value): the
	// runtime-direction lowering decides the direction from the bounds'
	// runtime values, so `loop 5..bound()` with bound() returning 3 descends
	// 5, 4 — 2 iterations (count = 2, exit code 2), instead of the old
	// hardcoded ascending `<` + `++` lowering that ran zero iterations. This
	// is the exact silent-zero-iteration defect the always-runtime-direction
	// lowering fixes: a descending bound only known at runtime no longer
	// falls through to the ascending default. bound() is still evaluated
	// exactly once (once-only evaluation of both bounds holds). Bounded
	// execution.
	src := "fn bound() int { print \"bound called\\n\"; return 3; } fn main() int { var count = 0; loop 5..bound() : i { count = count + 1; } return count; }"
	out := emitAndRunCaptureBounded(t, src, false, 2, false)
	if got := strings.Count(out, "bound called"); got != 1 {
		t.Fatalf("bound() called %d time(s), want exactly 1; output:\n%s", got, out)
	}
}

func TestEmitZeroLengthRangeLoopCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitDescendingRangeLoopWritesC(t *testing.T) {
	t.Parallel()
	// The runtime-direction lowering expresses a descending range through the
	// runtime step local and the ternary condition — never a hardcoded `--` or
	// `>`: both bounds land in pebble_temp_<id> locals, the step local is
	// computed from comparing them, and the for-loop condition's descending
	// branch reads `pebble_local_28 > pebble_temp_23` (exclusive) or `>=`
	// (inclusive), with the increment `pebble_local_28 += pebble_step_28`.
	// Symbols 22/23 (the bound literal nodes), 27 (count), and 28 (the
	// iterator) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var count i32 = 0; loop 5..0 : i { count = count + 1; } return count; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    int32_t pebble_temp_22 = 5;\n",
		"    int32_t pebble_temp_23 = 0;\n",
		"    int32_t pebble_step_28 = (pebble_temp_22 <= pebble_temp_23) ? 1 : -1;\n",
		"    for (int32_t pebble_local_28 = pebble_temp_22; (pebble_step_28 > 0) ? (pebble_local_28 < pebble_temp_23) : (pebble_local_28 > pebble_temp_23); pebble_local_28 += pebble_step_28) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Inclusive descending: the loop is gated by the done local, which is set
	// (from the still-unincremented current value) inside the increment's comma
	// expression once the iterator reaches the end bound — the shape that keeps
	// an unsigned iterator from wrapping one past end (0 - 1) into an infinite
	// loop on `loop 5..=0`.
	unit, snapshot, entryID, sources = buildFixture(t, "fn main() i32 { var count i32 = 0; loop 5..=0 : i { count = count + 1; } return count; }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out = buf.String()
	for _, want := range []string{
		"    int32_t pebble_step_28 = (pebble_temp_22 <= pebble_temp_23) ? 1 : -1;\n",
		"    int32_t pebble_done_28 = 0;\n",
		"    for (int32_t pebble_local_28 = pebble_temp_22; !pebble_done_28; pebble_done_28 |= (pebble_step_28 > 0) ? (pebble_local_28 >= pebble_temp_23) : (pebble_local_28 <= pebble_temp_23), pebble_local_28 += pebble_step_28) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitBlockBodyExplicitReturnResultShapesCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The row's block-body proof by result/value shape: a helper with a plain
	// block body and an explicit `return` for each of int, bool, str, a
	// struct, and void. Every helper is called from one entry and each value
	// shape is read back and asserted together, so one compile-link-run proves
	// all five result shapes route through the plain-block + explicit-return
	// lowering. The void helper (which falls off the end of its block, the
	// ImplicitReturn tail) is also invoked as an expression statement.
	emitAndRun(t, `type Point = struct { x int; y int; };
fn int_id() int { return 7; }
fn bool_id() bool { return true; }
fn str_id() str { return "hi"; }
fn point_id() Point { return Point.{ x = 3, y = 4 }; }
fn void_helper() void { }
fn main() int {
  var a int = int_id();
  var b bool = bool_id();
  var c str = str_id();
  var p Point = point_id();
  void_helper();
  if a == 7 && b && c == "hi" && p.x == 3 && p.y == 4 { return 42; }
  return 1;
}`, false, 42, false)
}

func TestEmitExpressionBodyReturnResultShapesCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The row's expression-body ("tail expression is the return value") proof:
	// `=> expr` bodies for int, bool, char, f64, str, and a struct result.
	// The expression body synthesizes a real Return node (spec 06a:
	// "an expression body is visited once with the declared result expectation
	// and retains a return record"), so it is the language's closest analogue
	// of a last-expression implicit return. The str helper forwards a
	// parameter and the struct helper constructs a literal, because a BARE
	// string-literal `=> "literal"` body is a separate NEW FINDING (recorded
	// in the worklog), not something this proof can rely on.
	emitAndRun(t, `type Point = struct { x int; y int; };
fn pick_int(flag bool) int => 42;
fn pick_bool(flag bool) bool => true;
fn pick_char(flag bool) char => 'a';
fn pick_f64(flag bool) f64 => 1.5;
fn pick_str(s str) str => s;
fn pick_point() Point => Point.{ x = 3, y = 4 };
fn main() int {
  var a int = pick_int(true);
  var b bool = pick_bool(true);
  var c char = pick_char(true);
  var d f64 = pick_f64(true);
  var s str = pick_str("hi");
  var p Point = pick_point();
  if a == 42 && b && c == 'a' && d == 1.5 && s == "hi" && p.x == 3 && p.y == 4 { return 42; }
  return 1;
}`, false, 42, false)
}

func TestEmitIfElseArmReturnResultShapesCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The if/else tail shape: a non-void helper whose body is a two-armed
	// if/else where EVERY arm's tail is the return value (an explicit return
	// in each arm). Proven across int, bool, str, and a struct result, with
	// the arm selection flipped by a bool flag so both arms lower and run.
	emitAndRun(t, `type Point = struct { x int; };
fn pick_int(flag bool) int { if flag { return 1; } else { return 2; } }
fn pick_bool(flag bool) bool { if flag { return true; } else { return false; } }
fn pick_str(flag bool) str { if flag { return "yes"; } else { return "no"; } }
fn pick_point(flag bool) Point { if flag { return Point.{ x = 3 }; } else { return Point.{ x = 9 }; } }
fn main() int {
  var a int = pick_int(true);
  var b bool = pick_bool(false);
  var s str = pick_str(true);
  var p Point = pick_point(false);
  if a == 1 && !b && s == "yes" && p.x == 9 { return 42; }
  return 1;
}`, false, 42, false)
}

func TestEmitDiscardedCallExpressionStatementCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The expression-statement shape: a call whose result is discarded as a
	// standalone statement. The discarded call is non-void (a permitted
	// discard, per the C0612 rule that allows calls), and a subsequent call
	// proves statement sequencing still runs after the discarded one — the
	// exit code proves both the discard and the later return execute.
	emitAndRun(t, `fn side(x int) int { return x * 10; }
fn main() int {
  side(42);
  side(1);
  return 42;
}`, false, 42, false)
}
