package backend

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestEmitIntEntryExpressionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "fn main() int => 0;", true, 0, false)
}

func TestEmitLogicalBoolLiteralAndCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The bool-literal combination shape: && of two plain bool literals, no
	// comparison involved. This is exactly the fixture 10.14 rejected as a
	// ShortCircuitValue (if true && false), now accepted — true && false is
	// false, so the else arm runs and the process exits 0.
	emitAndRun(t, "fn main() i32 { if true && false { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitLogicalBoolLiteralOrCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Same bool-literal combination for ||: true || false is true, so the
	// then-arm runs and the process exits 1.
	emitAndRun(t, "fn main() i32 { if true || false { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitLogicalNestedPrecedenceCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A three-way nested combination. Precedence is already resolved in the
	// typed IR tree (confirmed against a real fixture dump: the && node is a
	// child of the || node for `false && false || true`), and Pebble's grammar
	// gives || precedence 1 and && precedence 2, so && binds tighter — the same
	// as C. Each row would evaluate to the *opposite* result under the wrong
	// grouping, so the expected exit code proves the tree is walked, not
	// re-derived: `false && false || true` is (false && false) || true = true
	// (a wrong `false && (false || true)` grouping would be false), and
	// `true || false && false` is true || (false && false) = true (a wrong
	// `(true || false) && false` grouping would be false).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"and then or", "fn main() i32 { if false && false || true { return 1; } else { return 0; } }", 1},
		{"or then and", "fn main() i32 { if true || false && false { return 1; } else { return 0; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitLogicalAndWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for a && condition combining a local-typed comparison and
	// an unanchored literal comparison: x < 10 && 1 < 2 must lower to the
	// parenthesized (pebble_local_25 < 10 && 1 < 2), the local reference
	// resolved to its pebble_local name. Symbol 25 is the x local, confirmed
	// against the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var x i32 = 7; if x < 10 && 1 < 2 { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_27 = 7;",
		"    if ((pebble_local_27 < 10 && 1 < 2)) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitLogicalAndParenthesizedComparisonWritesC(t *testing.T) {
	t.Parallel()
	// A parenthesized comparison operand (flag && (1 < 2)) arrives wrapped in a
	// SourceAlias (confirmed against a real fixture dump), which buildBoolExpr
	// must unwrap before lowering the comparison. The emitted C must therefore
	// carry the comparison directly inside the parenthesized &&, with the bool
	// local referenced by name: (pebble_local_27 && 1 < 2). Symbol 27 is the
	// flag local.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var flag bool = true; if flag && (1 < 2) { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "if ((pebble_local_27 && 1 < 2)) {") {
		t.Errorf("emitted C missing the unwrapped parenthesized comparison &&:\n%s", out)
	}
}

func TestEmitBoolEqualityComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// ==/!= between two bool values: (1 < 2) == (3 < 4) is the genuine gap
	// 10.15 left as a confirmed remaining rejection — the outer BinaryValue's
	// two operands are bool-typed SourceAlias-wrapped comparisons (confirmed
	// against the real fixture dump), which used to fail buildExpr's width
	// gate. Both bool operands are now built under the bool grammar, so the
	// equality composes. Each row's expected exit code hand-verifies the
	// comparison: (1 < 2) is true and (3 < 4) is true, so true == true is true
	// (exit 1) and true != true is false (the != twin, exit 2); and true ==
	// false / true != false take the other arms.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal true", "fn main() i32 { if (1 < 2) == (3 < 4) { return 1; } else { return 2; } }", 1},
		{"equal false", "fn main() i32 { if (1 < 2) == (2 < 1) { return 1; } else { return 2; } }", 2},
		{"notEqual false twin", "fn main() i32 { if (1 < 2) != (3 < 4) { return 1; } else { return 2; } }", 2},
		{"notEqual true", "fn main() i32 { if (1 < 2) != (2 < 1) { return 1; } else { return 2; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitBoolEqualityWithShortCircuitCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Bool equality composes with 10.15's && / ||: (a == b) && (1 < 2) is a
	// ShortCircuitValue whose left operand is the bool-equality BinaryValue,
	// built through buildBoolExpr's BinaryValue case into buildComparison's
	// bool branch. a = true, b = true makes the equality true and the && with
	// the true comparison true (exit 1); a = true, b = false makes the equality
	// false, so the && short-circuits to false and the else arm runs (exit 2).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"and true", "fn main() i32 { var a bool = true; var b bool = true; if (a == b) && (1 < 2) { return 1; } else { return 2; } }", 1},
		{"and false", "fn main() i32 { var a bool = true; var b bool = false; if (a == b) && (1 < 2) { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitBoolEqualityWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for the flagship fixture: (1 < 2) == (3 < 4) must lower to
	// the parenthesized bool equality ((1 < 2) == (3 < 4)) in the if condition
	// — each bool operand parenthesized so a comparison operand cannot chain
	// associatively with the outer operator — with the arms' returns indented
	// one level. Symbol-level operands emit parenthesized too, as
	// (pebble_local_<id>), matching the same rule.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { if (1 < 2) == (3 < 4) { return 1; } else { return 2; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    if ((1 < 2) == (3 < 4)) {\n",
		"        return 1;",
		"        return 2;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	unit, snapshot, entryID, sources = buildFixture(t, "fn main() i32 { var a bool = true; var b bool = false; if a == b { return 1; } else { return 2; } }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out = buf.String()
	if !strings.Contains(out, "    if ((pebble_local_27) == (pebble_local_28)) {\n") {
		t.Errorf("emitted C missing the parenthesized bool-local equality:\n%s", out)
	}
}

func TestEmitUnsuffixedU64MaxLiteralCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact reported bug: a decimal literal that does not fit in any
	// signed C integer type (UINT64_MAX's decimal form) assigned to a u64
	// local. Before the fix the emitted C was `uint64_t pebble_local_N =
	// 18446744073709551615;` — the unsuffixed literal made cc fail under the
	// mandated -Wall -Wextra -Werror via -Wimplicitly-unsigned-literal. The
	// emitted C must carry a "u" suffix and the program must compile, run,
	// and print the full value, not a truncated or misinterpreted one.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var y u64 = 18446744073709551615; print y; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "18446744073709551615u") {
		t.Fatalf("emitted C does not spell the u64 literal with an unsigned suffix:\n%s", out)
	}
	if got := compileAndRunCapture(t, buf.Bytes(), 0, false); got != "18446744073709551615\n" {
		t.Fatalf("compiled program output = %q, want %q", got, "18446744073709551615\n")
	}
}

func TestEmitUnsuffixedU32MaxLiteralCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The same large-literal-at-unsigned-width shape at u32: UINT32_MAX's
	// decimal form in a u32 local. Unsuffixed it exceeds a signed 32-bit
	// literal's range and cc would warn under -Wall -Wextra -Werror; with the
	// "u" suffix it is a plain unsigned int constant. The emitted C must use
	// the suffix and the program must print the full value.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var y u32 = 4294967295; print y; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "4294967295u") {
		t.Fatalf("emitted C does not spell the u32 literal with an unsigned suffix:\n%s", out)
	}
	if strings.Contains(out, " = 4294967295;") {
		t.Fatalf("emitted C contains an unsuffixed UINT32_MAX literal:\n%s", out)
	}
	if got := compileAndRunCapture(t, buf.Bytes(), 0, false); got != "4294967295\n" {
		t.Fatalf("compiled program output = %q, want %q", got, "4294967295\n")
	}
}

func TestEmitSmallUnsignedLiteralRegressionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The already-working small-literal case must keep working: a value well
	// inside signed range at u64 width still compiles clean (now with a
	// harmless "u" suffix) and prints the correct value.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var y u64 = 123456789; print y; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	if got := compileAndRunCapture(t, buf.Bytes(), 0, false); got != "123456789\n" {
		t.Fatalf("compiled program output = %q, want %q", got, "123456789\n")
	}
}

func TestEmitLargeSignedLiteralNoUnsignedSuffix(t *testing.T) {
	t.Parallel()
	// A large SIGNED literal (INT64_MAX) must not gain an unsigned suffix:
	// the emitted C must keep the plain decimal text so the value is the
	// signed maximum, not an unsigned constant of the same digits. The
	// program must compile under -Wall -Wextra -Werror (a bare INT64_MAX
	// decimal is exactly representable as signed long long, so it is
	// warning-free unsuffixed) and print the correct value.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let y i64 = 9223372036854775807; print y; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if strings.Contains(out, "9223372036854775807u") {
		t.Fatalf("emitted C gives the signed i64 literal an unsigned suffix:\n%s", out)
	}
	if !strings.Contains(out, "9223372036854775807") {
		t.Fatalf("emitted C is missing the signed i64 literal:\n%s", out)
	}
	if got := compileAndRunCapture(t, buf.Bytes(), 0, false); got != "9223372036854775807\n" {
		t.Fatalf("compiled program output = %q, want %q", got, "9223372036854775807\n")
	}
}

func TestEmitI64OverflowAborts(t *testing.T) {
	t.Parallel()
	// 9223372036854775807 + 1 overflows i64. Compiled in PEBBLE_RT_MODE_SAFE
	// (the same mode every end-to-end test here uses), the emitted
	// pebble_rt_checked_add_i64 call must panic through pebble_rt_panic, so the
	// process must terminate abnormally — proving the i64 overflow story is
	// real end to end, not merely that an i64 entry compiles.
	emitAndRun(t, "fn main() i64 { return 9223372036854775807 + 1; }", false, 0, true)
}

func TestEmitI64DivideByZeroAborts(t *testing.T) {
	t.Parallel()
	// 1 / 0 at i64 width: the emitted pebble_rt_checked_div_i64 call must
	// panic through pebble_rt_panic (divide-by-zero is a fault in every
	// configuration), so the process terminates abnormally.
	emitAndRun(t, "fn main() i64 { return 1 / 0; }", false, 0, true)
}

func TestEmitI64ForInitClauseInsideI32FunctionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The for-header initializer reuses buildScalarInitializeCore, so the
	// mismatched-width local fix covers it automatically: a classic for loop
	// whose init clause declares an i64 local inside an i32 function. The i64
	// local is only ever referenced through an explicit `as i32` cast (the
	// bare reference would fail the entry-width gate, as the test above
	// proves), and the loop accumulates 21 three times into an i32 counter.
	// Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; var i i32 = 0; for var limit i64 = 21; i < 3; i = i + 1 { total = total + (limit as i32); } return total; }", false, 63, false)
}

func TestEmitLeibnizPiApproximationCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The leibniz_pi_approx.peb shape end-to-end, with the file's staleness
	// fixed (float -> f64, `n as f64` for the mixed-int/float term): an f64
	// helper taking an int and an f64 parameter accumulates the Leibniz
	// series in a while loop (breaking when the term drops below the
	// precision), returns pi as an f64, and the entry stores the result in an
	// f64 local, prints it alongside the actual pi and the error. This is the
	// exact program from the task; the checked-in example file itself must
	// stay untouched (it still uses the undefined `float` type), so this test
	// carries the fixed form. 100000 terms of the slowly-converging Leibniz
	// series give 3.1415826535897198 (about 1e-5 off), so the printed values
	// are pinned below.
	out := emitAndRunCaptureBounded(t, `
fn approximate_pi(max_terms int, precision f64) f64 {
    var sum f64 = 0.0;
    var n = 0;
    var sign = 1.0;
    while n < max_terms {
        var term f64 = sign / (2.0 * (n as f64) + 1.0);
        sum = sum + term;
        sign = -sign;
        n = n + 1;
        if term < 0.0 {
            term = -term;
        }
        if term < precision {
            break;
        }
    }
    return sum * 4.0;
}

fn main() int {
    let max_terms = 100000;
    let precision = 0.000001;
    let pi_approx = approximate_pi(max_terms, precision);
    let actual_pi = 3.1415926535;
    var error = actual_pi - pi_approx;
    if error < 0.0 {
        error = -error;
    }

    print "Approximated pi:"; print pi_approx;
    print "Actual pi:"; print actual_pi;
    print "Error:"; print error;

    return 0;
}`, false, 0, false)
	want := "Approximated pi:\n3.141583\nActual pi:\n3.141593\nError:\n0.000010\n"
	if out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitParenthesizedFloatExpressions(t *testing.T) {
	t.Parallel()
	// SourceAlias unwrapping, the parenthesized-expression distinction: a
	// grouped float literal `(3.5)` as a local's initializer and a grouped
	// float reference `(x)` as the main's return value both arrive as
	// SourceAlias nodes, which buildFloatExpr transparently recurses through
	// (exactly as buildExpr and buildBoolExpr already do). (3.5 narrows to
	// exit code 3.)
	emitAndRun(t, "fn main() f64 { var x f64 = (3.5); return (x); }", false, 3, false)
}

func TestEmitF64LiteralTruncatesToExitCode(t *testing.T) {
	t.Parallel()
	// The float value round-trips through the real emitted C and the harness's
	// process-exit observation, not just "it compiles": a fractional f64
	// whose C float-to-int truncation would land on a different code than a
	// rounding (or an integer-truncated literal) lowering would. 3.99 must
	// truncate to 3 (not round to 4), and the compile-and-run of the actual
	// emitted C asserts 3.
	emitAndRun(t, "fn main() f64 { var x f64 = 3.99; return x; }", false, 3, false)
}

func TestEmitIntegerToFloatCastCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An integer local cast to a float and used in float arithmetic: 3 as f64
	// plus 0.5 must yield the real float 3.5, which the hosted main truncates
	// to exit code 3 — had the cast produced an integer (or an implicit
	// truncating coupling), the result would be a different value. The
	// emitted-C assertion pins the lowering: the integer child is built via
	// buildExpr at its own i32 width and wrapped in a plain C (double) cast,
	// exactly the FloatCast-free shape a well-defined int->float conversion
	// needs (no checked runtime primitive).
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x i32 = 3; return (x as f64) + 0.5; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(double)(pebble_local_") {
		t.Fatalf("emitted C missing IntegerToFloat (double) cast:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 3, false)
}

func TestEmitFloatCastNarrowingCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An f64 local narrowed to f32. 16777217.5 (2^24 + 1.5) is not
	// representable in f32: round-to-nearest-even gives 16777218.0f, which
	// truncates to exit code 2 (verified empirically against cc). Had the cast
	// been missing (the f64 kept), truncation of 16777217.5 would exit 1 — so
	// the exit code discriminates the narrowing from a no-op. The emitted-C
	// assertion pins the (float) cast on the f64 local.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f32 { var x f64 = 16777217.5; return (x as f32); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(float)(pebble_local_") {
		t.Fatalf("emitted C missing FloatCast narrowing (float) cast:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitFloatCastWideningCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An f32 local widened to f64. f32->f64 widening is exact, so no exit-code
	// truncation can distinguish the widened value from the unwidened one;
	// the emitted-C assertion pins the (double) cast on the f32 local, and the
	// arithmetic result (2.5 -> exit 2) verifies the widened value flows into
	// f64 arithmetic.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x f32 = 1.5; return (x as f64) + 1.0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(double)(pebble_local_") {
		t.Fatalf("emitted C missing FloatCast widening (double) cast:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitChainedIntegerToFloatAndFloatCastCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Both new casts in one expression: an integer local cast to f32 (an
	// IntegerToFloat), used in f32 arithmetic, then the result widened to f64
	// by a FloatCast for the final f64 return. 33 as f32 / 1.5 as f32 = 22.0f,
	// widened to 22.0, plus 1.0 -> 23.0, exit 23. The emitted C must contain
	// both the (float) IntegerToFloat cast and the (double) FloatCast widening
	// cast.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x i32 = 33; var f f32 = 1.5; var g f32 = (x as f32) / f; return (g as f64) + 1.0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(float)(pebble_local_") {
		t.Fatalf("emitted C missing chained IntegerToFloat (float) cast:\n%s", out)
	}
	if !strings.Contains(out, "(double)(pebble_local_") {
		t.Fatalf("emitted C missing chained FloatCast (double) cast:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 23, false)
}

func TestEmitFloatToIntegerCompilesAndRuns(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"f32 to i32", "fn main() i32 { let x f32 = 42.75; return x as i32; }", 42},
		{"f64 to i32", "fn main() i32 { let x f64 = 42.75; return x as i32; }", 42},
		{"f32 to i64", "fn main() i64 { let x f32 = 42.75; return x as i64; }", 42},
		{"f64 to i64", "fn main() i64 { let x f64 = 42.75; return x as i64; }", 42},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			unit, snapshot, entryID, sources := buildFixture(t, tc.src, "main", false)
			var buf bytes.Buffer
			if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
				t.Fatalf("Emit failed: %v", err)
			}
			out := buf.String()
			if !strings.Contains(out, "pebble_rt_checked_f") || !strings.Contains(out, "_to_i") {
				t.Fatalf("emitted C missing checked float-to-integer helper:\n%s", out)
			}
			compileAndRun(t, buf.Bytes(), tc.want, false)
		})
	}
}

func TestEmitFloatToIntegerBoundaryPanics(t *testing.T) {
	t.Parallel()
	// f32 cannot represent INT32_MAX; 2147483647.0f rounds to 2^31 and must
	// be rejected rather than reaching C's undefined float-to-int conversion.
	emitAndRun(t, "fn main() i32 { let x f32 = 2147483647.0; return x as i32; }", false, 0, true)
	emitAndRun(t, "fn main() i64 { let x f64 = 9223372036854775808.0; return x as i64; }", false, 0, true)
}

func TestEmitIndexedStrElementAssignmentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitRuntimeAndRun(t, `type Entry = struct { key str; }; fn main() i32 { var first Entry = Entry.{ key = "old" }; let values []Entry = slice &first, 1; var replacement str = "new"; values[0].key = replacement; if values[0].key == "new" { return 0; } return 1; }`, 0)
}

func TestEmitStrEqualLiteralsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Two identical string literals compared equal, driving an if: the
	// comparison is between two StringLiteral operands (no local involved),
	// each embedded as a PebbleStr compound literal, so the then-arm runs and
	// the process exits 10.
	emitAndRun(t, "fn main() i32 { if \"hi\" == \"hi\" { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrDifferentLiteralsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Two different string literals compared equal (false): the lengths are
	// equal but the bytes differ, so pebble_rt_str_eq returns false and the
	// else-arm runs, exiting 20.
	emitAndRun(t, "fn main() i32 { if \"hi\" == \"ho\" { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrNotEqualCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// != between two str locals, both directions: different strings are not
	// equal (then-arm, exit 10) and identical strings are not-not-equal
	// (else-arm, exit 20), so the negation of pebble_rt_str_eq is exercised
	// for both a true and a false outcome.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"different not equal", "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s != t { return 10; } else { return 20; } }", 10},
		{"identical not equal false", "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; if s != t { return 10; } else { return 20; } }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStrEqualityAsBoolValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str comparison used as a plain bool value (not just as an if/while
	// condition): the equality result is stored in a bool local and that local
	// drives the if. The comparison lowers to pebble_rt_str_eq, whose bool
	// result is the bool local's initializer, so the then-arm runs, exiting 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; let b bool = s == t; if b { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrEqualityAsLogicalOperandCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str comparison combined with && — the equality as a logical operand of
	// a larger bool expression. Both comparisons hold, so the conjunction is
	// true and the then-arm runs, exiting 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s == \"hi\" && t == \"ho\" { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrEscapeRoundTripCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The escaping-correctness fixture: a decoded literal containing a control
	// byte immediately followed by a digit character, plus an escaped quote
	// and backslash, compared against a differently-spelled literal that
	// decodes to the same bytes. The Pebble source spells the first with \\n,
	// \\t, \\", and \\\\ (the escapes that decode to newline, tab, quote, and
	// backslash) and the second with \\xHH byte escapes for the same four
	// bytes. If the emitter escaped either decoded string naively — e.g. a C
	// \\xHH hex escape, where C's maximal-munch rule would absorb the
	// following hex digit ('1' after newline, 'b' after tab — 0x0a1/0x09b are
	// a single wrong byte, not two) — the two C strings would not round-trip
	// to the same bytes and the equality would fail, exiting 3. The fixed-
	// width octal escapes (\\012 then '1', \\011 then 'b', \\042 for the
	// quote, \\134 for the backslash) make each escape self-delimiting, so
	// both sides reconstruct exactly the same 9 bytes and the then-arm runs,
	// exiting 7. Two sub-cases: both operands are literals directly, and one
	// operand is a local holding the same decoded content.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"two literals", "fn main() i32 { if \"a\\n1\\tb\\\"c\\\\d\" == \"a\\x0a1\\x09b\\x22c\\x5cd\" { return 7; } else { return 3; } }"},
		{"local vs literal", "fn main() i32 { let s str = \"a\\n1\\tb\\\"c\\\\d\"; if s == \"a\\x0a1\\x09b\\x22c\\x5cd\" { return 7; } else { return 3; } }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, 7, false)
		})
	}
}

func TestEmitStrEscapeRoundTripWritesC(t *testing.T) {
	t.Parallel()
	// The literal escaped C text the round-trip fixture produces: the tab and
	// newline bytes must be emitted as fixed-width octal escapes, never as C
	// \\x hex escapes (which would absorb the following '1'/'b'), the quote as
	// \\", and the backslash as \\\\, so the emitted C string-literal body is
	// exactly a\\0121\\011b\\"c\\\\d. The .len field must carry the decoded
	// byte length 9. Symbol 25 is the s local, confirmed against the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"a\\n1\\tb\\\"c\\\\d\"; if s == \"a\\x0a1\\x09b\\x22c\\x5cd\" { return 7; } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_27 = { .data = (const uint8_t *)\"a\\0121\\011b\\\"c\\\\d\", .len = 9 };",
		"    (void)pebble_local_27;",
		"if (pebble_rt_str_eq(pebble_local_27, (PebbleStr){ .data = (const uint8_t *)\"a\\0121\\011b\\\"c\\\\d\", .len = 9 })) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for a str local compared against a string literal: the
	// local is declared directly as the runtime's PebbleStr (no typedef) with
	// the escaped bytes and compile-time length, and the equality lowers to
	// pebble_rt_str_eq(<local>, <literal-as-compound-literal>) — the literal
	// operand needs no declared local, so it is built inline as a PebbleStr
	// compound literal. Symbol 25 is the s local, confirmed against the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; if s == \"hi\" { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_27 = { .data = (const uint8_t *)\"hi\", .len = 2 };",
		"    (void)pebble_local_27;",
		"if (pebble_rt_str_eq(pebble_local_27, (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 })) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrNotEqualWritesC(t *testing.T) {
	t.Parallel()
	// The != lowering must negate the runtime helper: s != t emits
	// !pebble_rt_str_eq(pebble_local_25, pebble_local_26), not a comparison of
	// the two strings some other way. Symbols 25/26 are the s and t locals,
	// confirmed against the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s != t { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_27 = { .data = (const uint8_t *)\"hi\", .len = 2 };",
		"PebbleStr pebble_local_28 = { .data = (const uint8_t *)\"ho\", .len = 2 };",
		"if (!pebble_rt_str_eq(pebble_local_27, pebble_local_28)) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrOrderingLessCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// s < t where "hi" < "ho" is true (lexicographic byte comparison: 'i' <
	// 'o'), so the then-arm runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s < t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingLessFalseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// s < t where "ho" < "hi" is false ('o' > 'i'), so the else-arm runs
	// and the process exits 20.
	emitAndRun(t, "fn main() i32 { let s str = \"ho\"; let t str = \"hi\"; if s < t { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrOrderingLessEqualCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// s <= t where "hi" <= "hi" is true (equal counts), so the then-arm
	// runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; if s <= t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingLessEqualFalseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// s <= t where "hi" <= "ha" is false ('i' > 'a'), so the else-arm runs
	// and the process exits 20.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ha\"; if s <= t { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrOrderingGreaterCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// s > t where "ho" > "hi" is true ('o' > 'i'), so the then-arm runs
	// and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"ho\"; let t str = \"hi\"; if s > t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingGreaterFalseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// s > t where "hi" > "ho" is false ('i' < 'o'), so the else-arm runs
	// and the process exits 20.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s > t { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrOrderingGreaterEqualCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// s >= t where "hi" >= "hi" is true (equal counts), so the then-arm
	// runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; if s >= t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingGreaterEqualFalseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// s >= t where "ha" >= "hi" is false ('a' < 'i'), so the else-arm runs
	// and the process exits 20.
	emitAndRun(t, "fn main() i32 { let s str = \"ha\"; let t str = \"hi\"; if s >= t { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrEmptyStringComparisonsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Empty-string comparisons across the operators: an empty string equals
	// itself (== true), differs from a non-empty string (!= true), sorts
	// before every non-empty string (< and <= true), and is >= itself. A
	// zero-length PebbleStr must flow through the length-aware runtime
	// helpers without relying on a NUL terminator. Both an empty literal
	// local and a direct `"" == ""` literal comparison are covered.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"empty equals empty", "fn main() i32 { let s str = \"\"; let t str = \"\"; if s == t { return 10; } else { return 20; } }", 10},
		{"empty literals equal", "fn main() i32 { if \"\" == \"\" { return 10; } else { return 20; } }", 10},
		{"empty not equal nonempty", "fn main() i32 { let s str = \"\"; let t str = \"a\"; if s != t { return 10; } else { return 20; } }", 10},
		{"empty equals nonempty false", "fn main() i32 { let s str = \"\"; let t str = \"a\"; if s == t { return 10; } else { return 20; } }", 20},
		{"empty less than nonempty", "fn main() i32 { let s str = \"\"; let t str = \"a\"; if s < t { return 10; } else { return 20; } }", 10},
		{"empty lessEqual nonempty", "fn main() i32 { let s str = \"\"; let t str = \"a\"; if s <= t { return 10; } else { return 20; } }", 10},
		{"empty lessEqual empty", "fn main() i32 { let s str = \"\"; let t str = \"\"; if s <= t { return 10; } else { return 20; } }", 10},
		{"nonempty greater than empty", "fn main() i32 { let s str = \"a\"; let t str = \"\"; if s > t { return 10; } else { return 20; } }", 10},
		{"empty greaterEqual empty", "fn main() i32 { let s str = \"\"; let t str = \"\"; if s >= t { return 10; } else { return 20; } }", 10},
		{"empty greaterEqual nonempty false", "fn main() i32 { let s str = \"\"; let t str = \"a\"; if s >= t { return 10; } else { return 20; } }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStrOrderingNonPrefixDifferentLengthCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A different-length ordering where neither string is a prefix of the
	// other: "ab" vs "b". The first bytes 'a' (97) and 'b' (98) decide the
	// ordering BEFORE the length tie-break comes into play, so "ab" < "b" is
	// true even though "ab" is the LONGER string — the byte-value ordering a
	// length-only or prefix-only comparison would get wrong. "b" > "ab" is
	// the same comparison in the other direction.
	emitAndRun(t, "fn main() i32 { let s str = \"ab\"; let t str = \"b\"; if s < t { return 10; } else { return 20; } }", false, 10, false)
	emitAndRun(t, "fn main() i32 { let s str = \"b\"; let t str = \"ab\"; if s < t { return 10; } else { return 20; } }", false, 20, false)
	emitAndRun(t, "fn main() i32 { let s str = \"b\"; let t str = \"ab\"; if s > t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrByteValueOrderingCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Single-byte byte-value ordering, both directions and the >=/<= edge: "a"
	// < "b" is true ('a' is 97, 'b' is 98), "b" < "a" is false, and "b" >=
	// "a" is true. These prove the comparison orders by byte VALUE, not by
	// length or by string identity.
	emitAndRun(t, "fn main() i32 { let s str = \"a\"; let t str = \"b\"; if s < t { return 10; } else { return 20; } }", false, 10, false)
	emitAndRun(t, "fn main() i32 { let s str = \"b\"; let t str = \"a\"; if s < t { return 10; } else { return 20; } }", false, 20, false)
	emitAndRun(t, "fn main() i32 { let s str = \"b\"; let t str = \"a\"; if s >= t { return 10; } else { return 20; } }", false, 10, false)
	emitAndRun(t, "fn main() i32 { let s str = \"a\"; let t str = \"b\"; if s >= t { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrLengthAwareEqualityCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Length-aware equality: a string that is a prefix of another but SHORTER
	// must not compare equal — "hi" vs "hi!" differ in length, so == is false
	// and != is true even though every byte of the shorter one matches the
	// longer one's first two bytes. This is the property that distinguishes
	// V2's length-prefixed PebbleStr equality (pebble_rt_str_eq checks .len
	// before bytes) from a NUL-terminated C strcmp-style prefix comparison.
	// The two longer-strings-equal case also holds (equal bytes and equal
	// length), proving the helper did not fall back to a blind prefix test.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi!\"; if s == t { return 10; } else { return 20; } }", false, 20, false)
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi!\"; if s != t { return 10; } else { return 20; } }", false, 10, false)
	emitAndRun(t, "fn main() i32 { let s str = \"hi!\"; let t str = \"hi!\"; if s == t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingLiteralOperandCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An ordering comparison where one operand is a string literal directly
	// (not a local), confirming buildStrOperand's existing literal path works
	// unchanged in this new position. "hi" < "ho" is true, so the
	// then-arm runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; if s < \"ho\" { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for an ordering comparison between two str locals: the
	// comparison must use pebble_rt_str_cmp with the source operator
	// translated to its C spelling, compared against 0. The ==/!= path is
	// still pebble_rt_str_eq-based (verified below). Symbols 25/26 are the
	// s and t locals, confirmed against the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s < t { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_27 = { .data = (const uint8_t *)\"hi\", .len = 2 };",
		"PebbleStr pebble_local_28 = { .data = (const uint8_t *)\"ho\", .len = 2 };",
		"if (pebble_rt_str_cmp(pebble_local_27, pebble_local_28) < 0) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrEqualityStillUsesStrEqWritesC(t *testing.T) {
	t.Parallel()
	// Regression check: the ==/!= path must still use pebble_rt_str_eq,
	// not pebble_rt_str_cmp. This confirms this slice didn't disturb the
	// existing equality lowering.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; if s == t { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "if (pebble_rt_str_eq(pebble_local_27, pebble_local_28)) {") {
		t.Errorf("expected pebble_rt_str_eq for ==, got:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_str_cmp") {
		t.Errorf("== path must not use pebble_rt_str_cmp:\n%s", out)
	}
}

func TestEmitStrIndexCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// String indexing (s[0]) is reachable from real source and lowers to a
	// bare tir.CheckedIndex whose result type is char (confirmed against a
	// real fixture dump: Children = [SymbolValue s, IntegerLiteral 0]); this
	// was 10.41's rejection fixture and is now the positive case —
	// buildCharOperand builds the read as the runtime's UTF-8 decoder
	// pebble_rt_str_char_at_i32. s = "hi", so s[0] decodes to 'h'; comparing
	// against the char literal proves the decoded value is correct
	// end-to-end, not just that it compiles.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let c char = s[0]; if c == 'h' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexWritesC(t *testing.T) {
	t.Parallel()
	// The exact emitted call for a str-typed local base: s[0] emits
	// pebble_rt_str_char_at_i32(pebble_local_<s>, 0) — the base is the
	// PebbleStr local's own C name (built by buildStrOperand's SymbolValue
	// case) and the literal index is emitted as its decimal text. Symbols 25
	// (s) and 26 (c) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let c char = s[0]; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_28 = pebble_rt_str_char_at_i32(pebble_local_27, 0, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrIndexOutOfBoundsEmitsRealSourceLoc(t *testing.T) {
	t.Parallel()
	// Out-of-range str indexing (s = "hi" has 2 codepoints; s[5] is out of
	// bounds) still aborts via pebble_rt_str_char_at_i32, and since 10.44 the
	// call carries a real, resolved Pebble source location instead of the
	// zero-valued placeholder.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let c char = s[5]; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_str_char_at_i32(") {
		t.Errorf("emitted C missing the str char-at call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
}

func TestEmitStrIndexLiteralBaseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A bare string literal base ("hi"[0]) is checker-reachable (confirmed
	// against a real fixture dump — the CheckedIndex base is a StringLiteral
	// node) and buildStrOperand already builds it unchanged as a PebbleStr
	// compound literal, so the decoder call takes the inline literal as its
	// base argument. "hi"[0] = 'h', exit 1.
	emitAndRun(t, "fn main() i32 { let c char = \"hi\"[0]; if c == 'h' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexLiteralBaseWritesC(t *testing.T) {
	t.Parallel()
	// The emitted call for the literal-base shape: the base argument is the
	// inline PebbleStr compound literal, not a local reference.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let c char = \"hi\"[0]; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		`pebble_rt_str_char_at_i32((PebbleStr){ .data = (const uint8_t *)"hi", .len = 2 }, 0, (PebbleSourceLoc){"main.peb"`,
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrIndexMultiByteCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// s[i] is a Unicode-scalar-value index, not a byte offset: "aéb" is a (1
	// byte) + é (U+00E9, 2 bytes) + b (1 byte), so codepoint 1 is é and
	// codepoint 2 is b — byte offset 2 would land in the middle of é's
	// UTF-8 sequence. Both reads round-trip through equality against the char
	// literals, proving the decoder walks codepoints, not bytes.
	emitAndRun(t, "fn main() i32 { let s str = \"a\u00e9b\"; let c char = s[1]; if c == '\u00e9' { let d char = s[2]; if d == 'b' { return 1; } else { return 0; } } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexEmojiCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The strongest multi-byte proof: "a😀b" is a (1 byte) + 😀 (U+1F600, 4
	// bytes) + b (1 byte), so codepoint 1 is the full 21-bit scalar value
	// 128512 — a 4-byte sequence — compared against the emoji char literal,
	// proving the index lands on the second codepoint and not partway through
	// the first one's bytes.
	emitAndRun(t, "fn main() i32 { let s str = \"a\U0001F600b\"; let c char = s[1]; if c == '\U0001F600' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexOutOfRangePanics(t *testing.T) {
	t.Parallel()
	// s = "hi" has 2 codepoints; s[2] is past the last codepoint, so the
	// runtime's UTF-8 decoder panics (abort) instead of reading past the end.
	// The process must terminate abnormally, not exit cleanly.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let c char = s[2]; return 0; }", false, 0, true)
}

func TestEmitStrIndexNegativePanics(t *testing.T) {
	t.Parallel()
	// A negative index — i = 0 - 1 = -1 computed by checked arithmetic (which
	// itself does not overflow) — panics the decoder. The process must
	// terminate abnormally.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let i i32 = 0; let j i32 = i - 1; let c char = s[j]; return 0; }", false, 0, true)
}

func TestEmitStrIndexI64EntryCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The width-generic path: an i64 entry's str index emits
	// pebble_rt_str_char_at_i64 — only the index parameter's width varies by
	// the entry's; the result type is still the fixed int32_t char either
	// way. The index here is an i64-typed local reference, so the whole
	// i64 index path is exercised. s = "hi", i = 1, s[1] = 'i', exit 1.
	emitAndRun(t, "fn main() i64 { let s str = \"hi\"; let i i64 = 1; let c char = s[i]; if c == 'i' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexI64EntryWritesC(t *testing.T) {
	t.Parallel()
	// The emitted call for an i64 entry: the helper is the _i64 variant (the
	// index parameter is int64_t), and the base local is still the PebbleStr
	// local's own C name. Symbols 25 (s) and 26 (c) come from the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { let s str = \"hi\"; let c char = s[0]; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_28 = pebble_rt_str_char_at_i64(pebble_local_27, 0, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitCharToIntegerCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact minimal repro from the brief: `let c char = 'A'; let n u32 =
	// c as u32; return n as i32;` — a char read out as its codepoint. The
	// checker lowers the cast to a tir.CharToInteger whose single child is the
	// char-typed local c, and the backend lowers it to a plain C cast
	// (uint32_t)(pebble_local_<c>) — 'A' is codepoint 65, so the exit code is
	// 65. The u32 destination is not the entry's i32 width, so the cast appears
	// as the u32 local's initializer, then the local is read back out as i32.
	emitAndRun(t, "fn main() i32 { let c char = 'A'; let n u32 = c as u32; return n as i32; }", false, 65, false)
}

func TestEmitCharToIntegerFromLiteralCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The CharToInteger child can be a bare char literal (not just a local
	// reference): 'A' as u32 is built by buildCharOperand's CharLiteral path
	// and cast to uint32_t, then read back out as i32. Codepoint 65.
	emitAndRun(t, "fn main() i32 { let n u32 = 'A' as u32; return n as i32; }", false, 65, false)
}

func TestEmitCharToIntegerHashCharCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The real motivating case from std/hash.peb:85-87 — `fn hash_char(val
	// char) u64 { return hash_u64(val as u64); }` — as a standalone fixture
	// (std/hash.peb itself is untouched). The `val as u64` is a CharToInteger
	// whose destination (u64) is the argument width of hash_u64, so the width
	// gate passes exactly as the hash module needs. hash_char('A') reads out
	// codepoint 65 and returns it as a u64, which main narrows to i32.
	emitAndRun(t, "fn hash_u64(val u64) u64 { return val; }\nfn hash_char(val char) u64 { return hash_u64(val as u64); }\nfn main() i32 { return hash_char('A') as i32; }", false, 65, false)
}

func TestEmitCharToIntegerU64CompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The same char-to-integer cast at the other motivating width (u64),
	// matching hash_char's destination directly in a u64 entry.
	emitAndRun(t, "fn main() i64 { let c char = 'A'; let n u64 = c as u64; return n as i64; }", false, 65, false)
}

func TestEmitCharToIntegerWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C directly: the CharToInteger lowers to a plain C
	// cast of the char value's expression to the destination C type,
	// (uint32_t)(...), with no runtime helper.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let c char = 'A'; let n u32 = c as u32; return n as i32; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(uint32_t)(pebble_local_") {
		t.Errorf("emitted C missing the char-to-integer cast (uint32_t)(pebble_local_...):\n%s", out)
	}
}

func TestEmitPointerToIntegerU64CompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact minimal repro of the pointer-to-integer shape: `let n u64 = p
	// as u64;` reads out a pointer's address as a u64. The address itself is
	// non-deterministic (it depends on the stack), so the deterministic
	// assertion is that the cast compiles and runs AND the pointer still
	// dereferences to 42 afterwards — the cast node is emitted as the u64
	// local's initializer, and if that emission were broken the whole program
	// would fail to compile or misbehave.
	emitAndRun(t, "fn main() i32 { var x i32 = 42; let p *i32 = &x; let n u64 = p as u64; return *p; }", false, 42, false)
}

func TestEmitPointerToIntegerUintCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The uint twin: `let n uint = p as uint;`. uint is the platform-native
	// pointer-width builtin the backend routes through buildUintExpr, so this
	// exercises the buildUintExpr PointerToInteger lowering (a plain C cast to
	// uint64_t) as opposed to buildExpr's.
	emitAndRun(t, "fn main() i32 { var x i32 = 42; let p *i32 = &x; let n uint = p as uint; return *p; }", false, 42, false)
}

func TestEmitPointerToIntegerHashPtrCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The real motivating case from std/hash.peb:80-82 — `fn hash_ptr[T](ptr
	// *T) u64 { return hash_u64(ptr as u64); }` — as a standalone fixture
	// (std/hash.peb itself is untouched). The `ptr as u64` is a
	// PointerToInteger whose destination (u64) is the argument width of
	// hash_u64, exactly the shape the std module needs. The cast's address
	// result is discarded; the deterministic assertion is the deref through
	// the original pointer still returns 42.
	emitAndRun(t, "fn hash_u64(val u64) u64 { return val; }\nfn hash_ptr[T](ptr *T) u64 { return hash_u64(ptr as u64); }\nfn main() i32 { var x i32 = 42; let p *i32 = &x; let h u64 = hash_ptr(p); return *p; }", false, 42, false)
}

func TestEmitPointerToIntegerWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C directly: the PointerToInteger lowers to a plain C
	// cast of the pointer expression to the destination C type,
	// (uint64_t)(...), with no runtime helper.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var x i32 = 42; let p *i32 = &x; let n u64 = p as u64; return *p; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(uint64_t)(pebble_local_") {
		t.Errorf("emitted C missing the pointer-to-integer cast (uint64_t)(pebble_local_...):\n%s", out)
	}
}

func TestEmitCharToIntegerAllWidthsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The full destination-width matrix for char-to-integer casts ('A' = 65).
	// u32 and u64 already have dedicated tests above; this table closes the
	// rest of the set the backend's CharToInteger case accepts (any width with
	// cType() != "" — see cType/resolvedBuiltin in types.go: int/i32 share
	// int32_t, i64 is int64_t, u8/u16/u32/u64 are the fixed unsigned widths,
	// i8/i16 are the narrow signed widths). Every row reads the cast result
	// back out as the entry's int and asserts the real codepoint value. The
	// 10th width, uint, is deliberately NOT here: `c as uint` is broken
	// (buildUintExpr has no CharToInteger case — see the NEW FINDING in the
	// task report), so a compile-link-run row for it cannot pass today.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"int", "fn main() int { let c char = 'A'; let n int = c as int; return n; }"},
		{"i8", "fn main() int { let c char = 'A'; let n i8 = c as i8; return n as int; }"},
		{"i16", "fn main() int { let c char = 'A'; let n i16 = c as i16; return n as int; }"},
		{"i32", "fn main() int { let c char = 'A'; let n i32 = c as i32; return n as int; }"},
		{"i64", "fn main() int { let c char = 'A'; let n i64 = c as i64; return n as int; }"},
		{"u8", "fn main() int { let c char = 'A'; let n u8 = c as u8; return n as int; }"},
		{"u16", "fn main() int { let c char = 'A'; let n u16 = c as u16; return n as int; }"},
		{"u32", "fn main() int { let c char = 'A'; let n u32 = c as u32; return n as int; }"},
		{"u64", "fn main() int { let c char = 'A'; let n u64 = c as u64; return n as int; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, 65, false)
		})
	}
}

func TestEmitCharToIntegerHighByteU8BoundaryCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char with a high byte value cast to the narrow destination that can
	// hold it. The checker's char-literal range is a full Unicode scalar (max
	// 0x10FFFF, excluding the surrogate range — see lexer.go's escape decode),
	// so '\u{FF}' (255, LATIN SMALL LETTER Y WITH DIAERESIS) is a valid char.
	// 255 is exactly u8's maximum, so `'\u{FF}' as u8` must produce 255 — a
	// real return value asserting the cast was not sign-extended, truncated,
	// or range-checked into a panic.
	emitAndRun(t, "fn main() int { let c char = '\\u{FF}'; let n u8 = c as u8; return n as int; }", false, 255, false)
}

func TestEmitCharToIntegerU16MaximumBoundaryCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// '\u{FFFF}' (65535) is a valid Unicode scalar and fits u16 exactly (its
	// maximum). The exit code cannot carry 65535 (the OS masks exit status to
	// 8 bits), so the value is asserted via a comparison: returning 7 proves
	// the cast produced exactly 65535, not a truncated or wrapped value.
	emitAndRun(t, "fn main() int { let c char = '\\u{FFFF}'; let n u16 = c as u16; if n == 65535 { return 7; } return 1; }", false, 7, false)
}

func TestEmitCharToIntegerNarrowOverflowCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// What actually happens when a char overflows a destination that cannot
	// hold it? The cast is a plain, unchecked C cast (CharToInteger emits
	// `(<dest C type>)(<char expr>)`), so the value wraps/truncates per C's
	// integer-conversion rules — this is documented, deterministic behavior
	// on every C implementation the test suite's cc accepts. Two cases are
	// pinned through their wrapped numeric value:
	//   - '\u{100}' (256) as u8 truncates to 0 (256 mod 256);
	//   - '\u{FF}' (255) as i8 wraps to -1 (bit pattern 0xFF), round-tripped
	//     back to u8 as 255 so no negative literal is needed in the fixture.
	emitAndRun(t, "fn main() int { let c char = '\\u{100}'; let n u8 = c as u8; if n == 0 { return 7; } return 1; }", false, 7, false)
	emitAndRun(t, "fn main() int { let c char = '\\u{FF}'; let n i8 = c as i8; let back u8 = n as u8; if back == 255 { return 7; } return 1; }", false, 7, false)
}

func TestEmitCharToIntegerSliceElementSourceCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char-element slice read as the cast source: s[1] over a []char slice
	// ('b' = 98). The CharToInteger child is Load(CheckedIndexPlace), built by
	// buildCharOperand's slice-element path, then cast to u8. Proves the cast
	// works for a source shape that is not a literal or a plain local.
	emitAndRun(t, "fn main() int { var arr [3]char = ['a', 'b', 'c']; var s []char = arr[:]; let n u8 = s[1] as u8; return n as int; }", false, 98, false)
}

func TestEmitCharToIntegerStrIndexSourceCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-index read as the cast source: "hi"[0] decodes to 'h' (104)
	// through the runtime's UTF-8 decoder, then cast to u16. Exercises the
	// CharToInteger child shape CheckedIndex (the non-addressable str base).
	emitAndRun(t, "fn main() int { let c char = \"hi\"[0]; let n u16 = c as u16; return n as int; }", false, 104, false)
}

func TestEmitCharToIntegerCallSourceCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char-returning helper call as the cast source: pick() ('z' = 122).
	// The CharToInteger child is a DirectCall, built by buildCharOperand's
	// DirectCall path, then cast to u8.
	emitAndRun(t, "fn pick() char { return 'z'; } fn main() int { let n u8 = pick() as u8; return n as int; }", false, 122, false)
}

func TestEmitPointerToIntegerI64DestinationCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The third pointer-width-or-wider destination beyond the existing u64 and
	// uint coverage: i64's int64_t is exactly as wide as a pointer, so the
	// cast emits a same-width C cast and compiles cleanly under -Werror (the
	// narrower-than-pointer destinations do NOT — see the NEW FINDING in the
	// task report). The address value is non-deterministic, so the assertion
	// is that the cast emits and the pointer still dereferences to 42.
	emitAndRun(t, "fn main() i32 { var x i32 = 42; let p *i32 = &x; let n i64 = p as i64; return *p; }", false, 42, false)
}

func TestEmitPointerToIntegerStructPointeeCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A struct pointee at both pointer-width destinations (uint and u64). The
	// address itself is non-deterministic, so the proof is: (1) two casts of
	// the same pointer produce the same numeric address (a wrong or
	// non-deterministic cast would diverge), (2) the address is non-zero (a
	// real stack address, not a collapsed NULL), and (3) the pointer still
	// reads the struct fields correctly afterwards (p.x + p.y = 15). An
	// emitted-C-only cast bug would fail the cc compile; a wrong-value cast
	// would fail the comparison or nonzero assertion. Struct fields are typed
	// int (not i32) to sidestep a pre-existing, unrelated checker quirk in
	// which i32-typed aggregate members do not unify in an int-entry function.
	emitAndRun(t, "type Point = struct { x int; y int; };\nfn main() int { var p Point = Point.{ x = 7, y = 8 }; let q *Point = &p; let a uint = q as uint; let b uint = q as uint; if a != 0 && a == b { return p.x + p.y; } return 1; }", false, 15, false)
	emitAndRun(t, "type Point = struct { x int; y int; };\nfn main() int { var p Point = Point.{ x = 7, y = 8 }; let q *Point = &p; let a u64 = q as u64; let b u64 = q as u64; if a != 0 && a == b { return p.x + p.y; } return 1; }", false, 15, false)
}

func TestEmitPointerToIntegerOpaquePointeeCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An opaque extern pointee (*FILE, a real libc type) cast to uint, proven
	// against a real file write-then-read-back round trip: fopen returns a
	// genuine FILE*, the cast must compile and produce a non-zero, stable
	// address (two casts of the same pointer are equal), and the FILE* must
	// still work for fputs/fgetc afterwards. Exit 0 = every step succeeded;
	// each other code names the exact step that failed, so a wrong cast (or a
	// wrong FILE* type name) pinpoints the break.
	path := filepath.Join(t.TempDir(), "pebble_opaque_pointee_cast_test.txt")
	source := fmt.Sprintf(`extern {
    type FILE;
    fn fopen(path str, mode str) *FILE;
    fn fclose(file *FILE) i32;
    fn fputs(s str, file *FILE) i32;
    fn fgetc(file *FILE) i32;
    fn remove(path str) i32;
}
fn main() int {
    var f = fopen(%q, "w");
    if f == nil { return 1; }
    let a uint = f as uint;
    let b uint = f as uint;
    if a == 0 { return 2; }
    if a != b { return 3; }
    var w = fputs("h", f);
    if w < 0 { return 4; }
    var closed = fclose(f);
    if closed != 0 { return 5; }
    var g = fopen(%q, "r");
    if g == nil { return 6; }
    var c = fgetc(g);
    var closed2 = fclose(g);
    if closed2 != 0 { return 7; }
    var removed = remove(%q);
    if removed != 0 { return 8; }
    if c != 104 { return 9; }
    return 0;
}`, path, path, path)
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, source)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

func TestEmitU64EqualityComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact minimal repro for the non-entry-width comparison bug: a u64
	// local (a non-entry width in an int-entry main) compared with == against
	// a literal, the result stored in a bool local and used in an if.
	// Previously failed with "entry function body expression contains a
	// SymbolValue of type u64, want int" because buildComparisonOperand built
	// both operands at the AMBIENT entry width rather than each operand's own
	// resolved width. The == is false (5 != 0), so the else branch runs and
	// exit code 1 proves the comparison emitted and evaluated correctly.
	emitAndRun(t, "fn main() int { let h u64 = 5; let eq bool = h == 0; if eq { return 0; } else { return 1; } }", false, 1, false)
}

func TestEmitU64OrderingComparisonsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// All four ordering operators on a non-entry-width integer (u64 in an
	// int-entry main), each taking the branch matching its value so both true
	// and false outcomes are exercised — proving the fix is not specific to
	// equality. Each table row is also self-anchoring: the literal operand is
	// unified to u64 by the checker, so both operands of every comparison
	// resolve to the same width.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"less", "fn main() int { let h u64 = 5; if h < 10 { return 1; } else { return 2; } }", 1},
		{"less false", "fn main() int { let h u64 = 5; if h < 5 { return 1; } else { return 2; } }", 2},
		{"lessEqual", "fn main() int { let h u64 = 5; if h <= 5 { return 1; } else { return 2; } }", 1},
		{"lessEqual false", "fn main() int { let h u64 = 5; if h <= 4 { return 1; } else { return 2; } }", 2},
		{"greater", "fn main() int { let h u64 = 5; if h > 4 { return 1; } else { return 2; } }", 1},
		{"greater false", "fn main() int { let h u64 = 5; if h > 5 { return 1; } else { return 2; } }", 2},
		{"greaterEqual", "fn main() int { let h u64 = 5; if h >= 5 { return 1; } else { return 2; } }", 1},
		{"greaterEqual false", "fn main() int { let h u64 = 5; if h >= 6 { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitU64HashBytesFnv1aCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The motivating case: a helper mirroring std/hash.peb's hash_bytes
	// FNV-1a body verbatim (`hash = hash ^ (x as u64); hash = hash *
	// fnv_prime;` inside a loop over a real []u8 slice), called from an
	// int-entry main. This combines the u64 checked-arithmetic fix with the
	// already-landed comparison (30fca68) and slice-element-type (f85b4a0)
	// fixes: the body's u64 multiply and slice index must now emit the real
	// pebble_rt_checked_mul_u64 / pebble_rt_checked_index_u64 helpers rather
	// than the empty-suffix names that previously failed at cc compile time.
	// The fixed loop bound stands in for `loop 0..data.len : i`, whose
	// uint-typed range bound is a separate pre-existing blocker unrelated to
	// this slice (std/hash.peb itself still can't be imported for other
	// separately-tracked cast reasons; this fixture is standalone).
	//
	// SAFE mode: the FNV-1a multiply genuinely wraps mod 2^64 (the offset
	// basis times fnv_prime exceeds 2^64 on the very first iteration), so
	// the checked-mul helper must panic — the run terminates abnormally,
	// proving the u64 overflow path is reached by a real hash workload.
	// RELEASE mode: the same multiply wraps (the unsigned type's own defined
	// semantics), so the whole FNV-1a body runs to completion and the
	// resulting hash is nonzero, returned as exit code 0.
	src := "fn hash_bytes(data []u8) u64 { var hash u64 = 14695981039346656037; let fnv_prime u64 = 1099511628211; loop 0..3 : i { hash = hash ^ (data[i] as u64); hash = hash * fnv_prime; } return hash; } fn main() int { var data [3]u8 = [1, 2, 3]; var s []u8 = data[:]; var h u64 = hash_bytes(s); if h == 0 { return 1; } return 0; }"
	unit, snapshot, entryID, sources := buildFixture(t, src, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_rt_checked_mul_u64(",
		"pebble_rt_checked_index_u64(",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 0, true)
	releaseBinary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, releaseBinary, 0, false, false)
}

func TestEmitU8ComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A different non-entry-width integer (u8 in an int-entry main) confirms
	// the fix generalizes rather than being u64-specific: both == and the
	// ordering operators build the u8 operands at their own resolved width.
	emitAndRun(t, "fn main() int { let h u8 = 3; if h == 3 { return 0; } else { return 1; } }", false, 0, false)
	emitAndRun(t, "fn main() int { let h u8 = 3; if h < 4 { return 0; } else { return 1; } }", false, 0, false)
}

func TestEmitU8AllComparisonOperatorsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// All six comparison operators on a u8 (a non-entry width in an int-entry
	// main), each with a true and a false outcome so both branches are proven
	// to evaluate to the arithmetically correct result at the narrow width —
	// completing the u8 coverage beyond the == and < the original slice added.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal", "fn main() int { let h u8 = 3; if h == 3 { return 1; } else { return 2; } }", 1},
		{"equal false", "fn main() int { let h u8 = 3; if h == 4 { return 1; } else { return 2; } }", 2},
		{"notEqual", "fn main() int { let h u8 = 3; if h != 4 { return 1; } else { return 2; } }", 1},
		{"notEqual false", "fn main() int { let h u8 = 3; if h != 3 { return 1; } else { return 2; } }", 2},
		{"less", "fn main() int { let h u8 = 3; if h < 4 { return 1; } else { return 2; } }", 1},
		{"less false", "fn main() int { let h u8 = 3; if h < 3 { return 1; } else { return 2; } }", 2},
		{"lessEqual", "fn main() int { let h u8 = 3; if h <= 3 { return 1; } else { return 2; } }", 1},
		{"lessEqual false", "fn main() int { let h u8 = 3; if h <= 2 { return 1; } else { return 2; } }", 2},
		{"greater", "fn main() int { let h u8 = 3; if h > 2 { return 1; } else { return 2; } }", 1},
		{"greater false", "fn main() int { let h u8 = 3; if h > 3 { return 1; } else { return 2; } }", 2},
		{"greaterEqual", "fn main() int { let h u8 = 3; if h >= 3 { return 1; } else { return 2; } }", 1},
		{"greaterEqual false", "fn main() int { let h u8 = 3; if h >= 4 { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitI8AllComparisonOperatorsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// All six comparison operators on a signed narrow width (i8 in an
	// int-entry main) with a NEGATIVE value — the "-5 < -4" direction is the
	// signedness-sensitive case that a width-mismatched or unsigned-treated
	// comparison would get wrong, so each row proves the signed comparison
	// evaluates correctly at the i8 width.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal", "fn main() int { let h i8 = -5; if h == -5 { return 1; } else { return 2; } }", 1},
		{"equal false", "fn main() int { let h i8 = -5; if h == -4 { return 1; } else { return 2; } }", 2},
		{"notEqual", "fn main() int { let h i8 = -5; if h != -4 { return 1; } else { return 2; } }", 1},
		{"notEqual false", "fn main() int { let h i8 = -5; if h != -5 { return 1; } else { return 2; } }", 2},
		{"less", "fn main() int { let h i8 = -5; if h < -4 { return 1; } else { return 2; } }", 1},
		{"less false", "fn main() int { let h i8 = -5; if h < -5 { return 1; } else { return 2; } }", 2},
		{"lessEqual", "fn main() int { let h i8 = -5; if h <= -5 { return 1; } else { return 2; } }", 1},
		{"lessEqual false", "fn main() int { let h i8 = -5; if h <= -6 { return 1; } else { return 2; } }", 2},
		{"greater", "fn main() int { let h i8 = -5; if h > -6 { return 1; } else { return 2; } }", 1},
		{"greater false", "fn main() int { let h i8 = -5; if h > -5 { return 1; } else { return 2; } }", 2},
		{"greaterEqual", "fn main() int { let h i8 = -5; if h >= -5 { return 1; } else { return 2; } }", 1},
		{"greaterEqual false", "fn main() int { let h i8 = -5; if h >= -4 { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitU64NotEqualComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The one gap in the u64 matrix (== covered by
	// TestEmitU64EqualityComparisonCompilesAndRuns and the four ordering
	// operators by TestEmitU64OrderingComparisonsCompilesAndRuns): != on a
	// non-entry-width u64, both a true and a false outcome.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"different not equal", "fn main() int { let h u64 = 5; if h != 4 { return 1; } else { return 2; } }", 1},
		{"identical not equal false", "fn main() int { let h u64 = 5; if h != 5 { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitUintAllComparisonOperatorsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// All six comparison operators on `uint` (the word-sized unsigned builtin
	// whose C type is uint64_t), in an int-entry main. uint comparison
	// operands flow through buildUintExpr rather than the general integer
	// operand path, so this is the uint matrix's dedicated proof: each row
	// exercises a real boolean outcome, both true and false.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal", "fn main() int { let h uint = 3; if h == 3 { return 1; } else { return 2; } }", 1},
		{"equal false", "fn main() int { let h uint = 3; if h == 4 { return 1; } else { return 2; } }", 2},
		{"notEqual", "fn main() int { let h uint = 3; if h != 4 { return 1; } else { return 2; } }", 1},
		{"notEqual false", "fn main() int { let h uint = 3; if h != 3 { return 1; } else { return 2; } }", 2},
		{"less", "fn main() int { let h uint = 3; if h < 4 { return 1; } else { return 2; } }", 1},
		{"less false", "fn main() int { let h uint = 3; if h < 3 { return 1; } else { return 2; } }", 2},
		{"lessEqual", "fn main() int { let h uint = 3; if h <= 3 { return 1; } else { return 2; } }", 1},
		{"lessEqual false", "fn main() int { let h uint = 3; if h <= 2 { return 1; } else { return 2; } }", 2},
		{"greater", "fn main() int { let h uint = 3; if h > 2 { return 1; } else { return 2; } }", 1},
		{"greater false", "fn main() int { let h uint = 3; if h > 3 { return 1; } else { return 2; } }", 2},
		{"greaterEqual", "fn main() int { let h uint = 3; if h >= 3 { return 1; } else { return 2; } }", 1},
		{"greaterEqual false", "fn main() int { let h uint = 3; if h >= 4 { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitF32AllComparisonOperatorsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// All six comparison operators on f32 values in an int-entry main. The
	// operands are two f32 locals so both sides resolve to the identical f32
	// type (the checker rejects a genuinely mixed f32/f64 comparison), and
	// every row asserts a real boolean outcome — completing the float matrix
	// beyond the single f64 `<` the existing float-comparison test covers.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal", "fn main() int { var a f32 = 1.5; var b f32 = 1.5; if a == b { return 1; } else { return 2; } }", 1},
		{"equal false", "fn main() int { var a f32 = 1.5; var b f32 = 2.5; if a == b { return 1; } else { return 2; } }", 2},
		{"notEqual", "fn main() int { var a f32 = 1.5; var b f32 = 2.5; if a != b { return 1; } else { return 2; } }", 1},
		{"notEqual false", "fn main() int { var a f32 = 1.5; var b f32 = 1.5; if a != b { return 1; } else { return 2; } }", 2},
		{"less", "fn main() int { var a f32 = 1.5; var b f32 = 2.5; if a < b { return 1; } else { return 2; } }", 1},
		{"less false", "fn main() int { var a f32 = 2.5; var b f32 = 1.5; if a < b { return 1; } else { return 2; } }", 2},
		{"lessEqual", "fn main() int { var a f32 = 1.5; var b f32 = 1.5; if a <= b { return 1; } else { return 2; } }", 1},
		{"lessEqual false", "fn main() int { var a f32 = 2.5; var b f32 = 1.5; if a <= b { return 1; } else { return 2; } }", 2},
		{"greater", "fn main() int { var a f32 = 2.5; var b f32 = 1.5; if a > b { return 1; } else { return 2; } }", 1},
		{"greater false", "fn main() int { var a f32 = 1.5; var b f32 = 2.5; if a > b { return 1; } else { return 2; } }", 2},
		{"greaterEqual", "fn main() int { var a f32 = 1.5; var b f32 = 1.5; if a >= b { return 1; } else { return 2; } }", 1},
		{"greaterEqual false", "fn main() int { var a f32 = 1.5; var b f32 = 2.5; if a >= b { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitF64AllComparisonOperatorsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The f64 twin of the f32 matrix above: all six operators on two f64
	// locals in an int-entry main. This is the full proof behind the audit
	// row's float claim — the existing TestEmitFloatComparisonBetweenLocals
	// only exercised `<`.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal", "fn main() int { var a f64 = 1.5; var b f64 = 1.5; if a == b { return 1; } else { return 2; } }", 1},
		{"equal false", "fn main() int { var a f64 = 1.5; var b f64 = 2.5; if a == b { return 1; } else { return 2; } }", 2},
		{"notEqual", "fn main() int { var a f64 = 1.5; var b f64 = 2.5; if a != b { return 1; } else { return 2; } }", 1},
		{"notEqual false", "fn main() int { var a f64 = 1.5; var b f64 = 1.5; if a != b { return 1; } else { return 2; } }", 2},
		{"less", "fn main() int { var a f64 = 1.5; var b f64 = 2.5; if a < b { return 1; } else { return 2; } }", 1},
		{"less false", "fn main() int { var a f64 = 2.5; var b f64 = 1.5; if a < b { return 1; } else { return 2; } }", 2},
		{"lessEqual", "fn main() int { var a f64 = 1.5; var b f64 = 1.5; if a <= b { return 1; } else { return 2; } }", 1},
		{"lessEqual false", "fn main() int { var a f64 = 2.5; var b f64 = 1.5; if a <= b { return 1; } else { return 2; } }", 2},
		{"greater", "fn main() int { var a f64 = 2.5; var b f64 = 1.5; if a > b { return 1; } else { return 2; } }", 1},
		{"greater false", "fn main() int { var a f64 = 1.5; var b f64 = 2.5; if a > b { return 1; } else { return 2; } }", 2},
		{"greaterEqual", "fn main() int { var a f64 = 1.5; var b f64 = 1.5; if a >= b { return 1; } else { return 2; } }", 1},
		{"greaterEqual false", "fn main() int { var a f64 = 1.5; var b f64 = 2.5; if a >= b { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitF64FloatLiteralComparisonOperandsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Ordering and equality where the float literal itself is an operand (not
	// two pre-declared locals): the checker anchors the literal to the local's
	// own float type, so the comparison resolves both sides at the same f64
	// width. 1.5 < 2.5 is true (exit 1) and 2.5 < 1.5 is false (exit 2),
	// proving the literal operand builds at the f64 grammar, not as a
	// mistyped integer.
	emitAndRun(t, "fn main() int { var a f64 = 1.5; if a < 2.5 { return 1; } else { return 2; } }", false, 1, false)
	emitAndRun(t, "fn main() int { var a f64 = 2.5; if a < 1.5 { return 1; } else { return 2; } }", false, 2, false)
}

func TestEmitGlobalLetConstantAsFixedWidthArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A top-level `let` global constant with an untyped literal initializer
	// stays at the abstract `int` builtin (not i32) until a use site pins it
	// to a concrete width — the std/io.peb `let SeekEnd = 2;` shape, passed
	// directly as an argument to a fixed-width `i32` parameter (mirroring
	// `fseek`'s `whence i32`). Before the fix, buildExpr's width gate
	// rejected the abstract-int-typed SymbolValue outright ("of type int,
	// want i32"); the checker side also needed a way to lower a reference to
	// a `let` global at all (globals aren't in locals scope), which the
	// buildDeclarations/buildValueRecord change provides by inlining a fresh
	// copy of the constant's initializer at each reference site. echo(Seed)
	// returning 7 proves the constant's value round-trips correctly through
	// a real i32-typed call argument, not just that it emits.
	unit, snapshot, entryID, sources := buildStdFixture(t, `let Seed = 7;
fn echo(x i32) i32 { return x; }
fn main() i32 {
    return echo(Seed);
}`, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 7, false)
}

func TestEmitCharNotEqualCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The != comparison between two char values: c='a' and d='b' differ, so
	// c != d is true and the process exits 1.
	emitAndRun(t, "fn main() i32 { let c char = 'a'; let d char = 'b'; if c != d { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharNonAsciiEqualityCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char-typed local declared from a non-ASCII literal — the accented
	// letter 'é' (U+00E9, 233) — compared for equality against the same
	// literal, proving the full Unicode scalar value round-trips through the
	// int32_t emission and back, not just an ASCII slice of it.
	emitAndRun(t, "fn main() i32 { let c char = 'é'; if c == 'é' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharEmojiEqualityCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char-typed local declared from an emoji — '😀' (U+1F600, 128512), a
	// value that needs more than a byte to represent — compared for equality.
	// This proves the full 21-bit Unicode scalar value round-trips, not just a
	// truncated low byte (which would collide with a different code point).
	emitAndRun(t, "fn main() i32 { let c char = '😀'; if c == '😀' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharOrderingCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An ordering comparison between two char values: c holds 'a' (97) and d
	// holds 'b' (98), so c < d is true and the process exits 1. Comparing
	// Unicode scalar values numerically is well-defined, and the checker
	// accepts ordering comparisons between chars (confirmed against a real
	// fixture), so the plain C operator is the correct lowering.
	emitAndRun(t, "fn main() i32 { let c char = 'a'; let d char = 'b'; if c < d { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharOrderingFalseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The false outcome of the char ordering fixture: 'b' < 'a' is false, so
	// the process exits 0 — proving the ordering distinguishes the two scalar
	// values in the correct direction.
	emitAndRun(t, "fn main() i32 { let c char = 'b'; let d char = 'a'; if c < d { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitCharI64EntryCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char-typed local inside an i64 entry: the entry's integer width picks
	// i64 arithmetic, but a char is still the fixed int32_t (the two are
	// unrelated concepts), so this confirms the char grammar is independent of
	// the entry's width.
	emitAndRun(t, "fn main() i64 { let c char = 'a'; if c == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for the char fixture: the char local is declared with the
	// fixed int32_t type and its literal emitted as an int32_t constant
	// ((int32_t)97 for 'a', (int32_t)98 for 'b'), the helper's parameter and
	// return type are both int32_t, the reassignment stores the call result,
	// and the comparison emits the plain C == operator between the two
	// int32_t operands. Symbols 24 (f), 25 (f's c parameter), 26 (main), 27
	// (main's c local) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn f(c char) char { return c; } fn main() i32 { var c char = 'a'; c = f('b'); if c == 'b' { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx, int32_t pebble_local_25) {",
		"int32_t pebble_local_29 = (int32_t)97;",
		"pebble_local_29 = pebble_fn_24(ctx, (int32_t)98);",
		"if (pebble_local_29 == (int32_t)98) {",
		"return pebble_local_25;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 1, false)
}

func TestEmitExplicitPointerCastRoundTripCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// var y i32 = 42; let p *i32 = &y; let q *void = p as *void; let r *i32 = q as *i32; return *r;
	// An explicit pointer-to-pointer cast (*i32 -> *void -> *i32) round-trips
	// correctly. Also exercises *void's own C representation (void *), which
	// pointerTypeName previously produced as a malformed empty type name
	// since it routed through cType (meant only for the fixed-width integer
	// kinds) rather than handling void/bool/char explicitly.
	emitAndRun(t, "fn main() i32 { var y i32 = 42; let p *i32 = &y; let q *void = p as *void; let r *i32 = q as *i32; return *r; }", false, 42, false)
}

func TestEmitIntegerCastRoundTripCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The intermediate i64 cast has a different width from the i32 entry, but
	// the outer cast returns to the entry width.
	// The test runner observes the process exit code, so reduce the result to
	// its low byte after the cast (300 exits as 44 on Unix either way).
	emitAndRunBounded(t, "fn main() i32 { var n i32 = 0; var done i32 = 0; while done == 0 { var x i32 = 300; n = (x as i64) as i32; done = 1; } return n % 256; }", false, 44, false)
}

func TestEmitIntegerCastTruncatesCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// 4294967297 narrowed to i32 wraps to 1, matching the fixed-width C cast.
	emitAndRunBounded(t, "fn main() i32 { var n i32 = 0; var done i32 = 0; while done == 0 { n = (4294967297 as i64) as i32; done = 1; } return n; }", false, 1, false)
}

func TestEmitIntegerCastUnsignedRoundTripCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Exercise a differently-signed intermediate type rather than only i32/i64.
	emitAndRunBounded(t, "fn main() i32 { var n i32 = 0; var done i32 = 0; while done == 0 { var x i32 = 300; n = (x as u32) as i32; done = 1; } return n % 256; }", false, 44, false)
}

func TestCheckStdHmapU64HashFnTypes(t *testing.T) {
	t.Parallel()
	// The real motivating module: std/hmap.peb declares `hash_fn fn (K) u64`
	// and `fn new[K, V](hash_fn fn (K) u64, eq_fn fn (K, K) bool) HashMap[K, V]`
	// — u64 in function-type parameter and RESULT positions (line 19 and 191 in
	// the module source). The checker must type-accept the module with no
	// diagnostics: this confirms the u64 function-type result/parameter shapes
	// the hardware accepts and that the feature's motivating real-code type has
	// always been well-typed. Reading the real module files (not a fixture
	// string) mirrors TestCheckStdVecHasNoGenericPointerReceiverShapeErrors.
	//
	// (Emit of a full hmap consumer — hmap::new[int, int], insert, get — is
	// blocked not by u64 function types but by pre-existing, out-of-scope
	// generic-struct gaps: HashMap's `key K` / `value V` type-parameter fields,
	// its uint/slice/runtime-Allocator fields, and the generic method calls
	// insert/get, all rejected by the backend's generic-struct/method
	// supporters separately. The u64 `hash_fn fn (K) u64` and
	// `fn new[K, V](..., hash_fn fn (K) u64, ...)` types themselves check clean
	// and their shapes emit-and-run in every function-type position the
	// TestEmitU64FunctionType* tests above exercise concretely.)
	hmap, err := os.ReadFile("../../std/hmap.peb")
	if err != nil {
		t.Fatal(err)
	}
	mem, err := os.ReadFile("../../std/mem.peb")
	if err != nil {
		t.Fatal(err)
	}
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb":     []byte(`import "std:hmap"; fn userHash(x int) u64 => x as u64; fn userEq(a int, b int) bool => a == b; fn main() int { var m = hmap::new[int, int](userHash, userEq); return 0; }`),
		"std/hmap.peb": hmap,
		"std/mem.peb":  mem,
	}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed on std:hmap with u64 hash_fn: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("check succeeded without an IR unit")
	}
}

// TestEmitClosureLiteralArrowShorthand verifies that an anonymous closure literal
// using => expr arrow shorthand compiles and runs correctly when passed as a call
// argument. This is the end-to-end regression for the C0607 false-positive fix
// where the checker previously rejected fn (a, b str) bool => a == b as a
// "non-void function can fall through without returning".
func TestEmitClosureLiteralArrowShorthand(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `
fn call_it(f fn (str, str) bool) bool {
    return f("a", "a");
}
fn main() int {
    var r = call_it(fn (a, b str) bool => a == b);
    if r { return 1; }
    return 0;
}
`, true, 1, false)
}

// TestEmitClosureLiteralArrowShorthandFalse compares two strings and returns
// false, verifying the closure's expression body evaluates correctly.
func TestEmitClosureLiteralArrowShorthandFalse(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `
fn call_it(f fn (str, str) bool) bool {
    return f("a", "b");
}
fn main() int {
    var r = call_it(fn (a, b str) bool => a == b);
    if r { return 0; }
    return 1;
}
`, true, 1, false)
}

// --- 13: enum/union-returning DirectCall in a general value position ---

// TestEmitEnumReturningCallAsSwitchSubjectCompilesAndRuns is the exact
// reproduction from tracker proposal 13: an enum-returning helper's result used
// directly as a switch subject (`switch pick() { ... }`) instead of being bound
// to a local first. buildEnumValue previously rejected the DirectCall node
// cleanly; the call is now built by buildDirectCallWithPre into
// `switch (pebble_fn_<callee>(ctx)) { ... }`. pick() returns Color.green, so
// the green case fires and the process exits 1.
func TestEmitEnumReturningCallAsSwitchSubjectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Color = enum { red, green, blue };

fn pick() Color {
    return Color.green;
}

fn main() int {
    switch pick() {
        case Color.red: return 0;
        case Color.green: return 1;
        case Color.blue: return 2;
    }
}`, false, 1, false)
}

// TestEmitEnumReturningCallAsSwitchSubjectWritesC checks the emitted C for the
// switch-subject shape: the entry's switch subject must be the helper call
// pebble_fn_<callee>(ctx) directly, never a temp local or a struct typedef.
func TestEmitEnumReturningCallAsSwitchSubjectWritesC(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, enumType, variants, sources := enumFixture(t, `type Color = enum { red, green, blue };

fn pick() Color {
    return Color.green;
}

fn main() int {
    switch pick() {
        case Color.red: return 0;
        case Color.green: return 1;
        case Color.blue: return 2;
    }
}`)
	if len(variants) != 3 {
		t.Fatalf("fixture has %d variants, want 3 (red, green, blue)", len(variants))
	}
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"switch (pebble_fn_",
		"case pebble_variant_" + strconv.Itoa(int(variants[1])) + ":",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, enumTypeName(enumType)+" pebble_local_") {
		t.Errorf("emitted C bound the switch subject into a temp local, want the helper call directly:\n%s", out)
	}
}

// TestEmitEnumReturningCallAsComparisonOperandCompilesAndRuns exercises the
// same buildEnumValue DirectCall case in a second general value position: an
// enum-returning call used directly as a == comparison operand
// (`pick() == Color.green`). pick() returns Color.green, so the comparison is
// true and the process exits 1.
func TestEmitEnumReturningCallAsComparisonOperandCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Color = enum { red, green, blue };

fn pick() Color {
    return Color.green;
}

fn main() int {
    if pick() == Color.green { return 1; }
    return 0;
}`, false, 1, false)
}

// TestEmitUnionReturningCallAsCallArgumentCompilesAndRuns is the tagged-union
// half of the gap, exercised in a general value position that routes through
// buildUnionValueExpr: a union-returning helper's result passed directly as a
// union-typed call argument (`takes(pick())`) rather than bound to a local
// first. The callee takes() switches on its union-typed parameter, so the
// runtime behavior is asserted end to end: pick() constructs Choice.value(5),
// so the value case fires and the process exits 1.
func TestEmitUnionReturningCallAsCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Choice = union enum { empty void; value int; };

fn pick() Choice {
    return Choice.value(5);
}

fn takes(c Choice) int {
    switch c {
        case Choice.empty: return 0;
        case Choice.value: return 1;
    }
}

fn main() int {
    return takes(pick());
}`, false, 1, false)
}

// TestEmitIntegerLiteralBoundariesAtEachWidthCompilesAndRuns proves the
// "V2 preserves text, constrains range, and lowers by width" claim for the
// positive-maximum and unsigned-minimum boundary of every fixed-width integer
// builtin the backend emits: a literal AT the exact max (or 0, the unsigned
// min) for that width is lowered at the local's own width and the printed
// value must equal the authored boundary exactly — never a wrapped or
// truncated neighbour. Each row is a compile-link-run through the real C
// toolchain with the mandated -Wall -Wextra -Werror, so a literal that fails
// to fit the width's own C type would fail here.
//
// The SIGNED MINIMUM literals are deliberately absent for i32/int/i64 (and
// only those three): `-2147483648` (i32/int) and `-9223372036854775808`
// (i64) pass the checker (correctly — each IS that width's minimum) but the
// backend lowers a negative-literal initializer to
// pebble_rt_checked_neg_i32(2147483648) / _i64(9223372036854775808), and the
// positive magnitude 2^(n-1) cannot be spelled as a signed C constant of that
// width, so cc fails under -Werror with -Wconstant-conversion /
// -Wimplicitly-unsigned-literal. i8 (-128) and i16 (-32768) min literals work
// because they take the constant-fold path instead. This is recorded as a NEW
// FINDING (checker/backend literal-lowering defect), not fixed here. The
// int max row also pins the backend's int32_t width for int (2147483647 works;
// 2147483648 is checker-accepted and then fails at cc — a separate NEW FINDING
// about the int literal range, recorded with the checker tests).
func TestEmitIntegerLiteralBoundariesAtEachWidthCompilesAndRuns(t *testing.T) {
	tests := []struct {
		name  string
		width string
		value string
		want  string
	}{
		{"i8 max", "i8", "127", "127\n"},
		{"i8 min", "i8", "-128", "-128\n"},
		{"i16 max", "i16", "32767", "32767\n"},
		{"i16 min", "i16", "-32768", "-32768\n"},
		{"i32 max", "i32", "2147483647", "2147483647\n"},
		{"i64 max", "i64", "9223372036854775807", "9223372036854775807\n"},
		{"int max", "int", "2147483647", "2147483647\n"},
		{"u8 max", "u8", "255", "255\n"},
		{"u8 min", "u8", "0", "0\n"},
		{"u16 max", "u16", "65535", "65535\n"},
		{"u16 min", "u16", "0", "0\n"},
		{"u32 max", "u32", "4294967295", "4294967295\n"},
		{"u32 min", "u32", "0", "0\n"},
		{"u64 max", "u64", "18446744073709551615", "18446744073709551615\n"},
		{"u64 min", "u64", "0", "0\n"},
		{"uint max", "uint", "18446744073709551615", "18446744073709551615\n"},
		{"uint min", "uint", "0", "0\n"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			src := "fn main() i32 { let x " + tc.width + " = " + tc.value + "; print x; return 0; }"
			if got := emitAndRunCapture(t, src, false, 0, false); got != tc.want {
				t.Fatalf("compiled program output = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestEmitFloatLiteralFormsAndWidthsCompileAndRun proves the "V2 supports
// f32/f64 lowering" claim for the literal forms Pebble's lexer can actually
// produce. There is NO f32/f64 literal suffix in this language: the lexer
// turns a numeric token with a fractional part and/or exponent into a bare
// FloatLiteral (digits [ . digits ] [ e/E [+-] digits ], no trailing suffix),
// and the float kind is inferred from context (the declared local/parameter
// width). The backend splices the validated literal text verbatim into the
// emitted C and the destination width's C type (float/double) applies the
// rounding. The print capture observes the exact resulting value, so the f32
// rows prove real 24-bit-significand narrowing (1.5e10 prints 15000000512,
// the nearest f32, not the exact 15000000000 the f64 row prints) rather than
// a silent double-wide interpretation. Negative and e/E-exponent forms are
// included, plus the f32/f64 finite-maximum magnitude (values at the largest
// representable magnitude round-trip without overflow to infinity).
func TestEmitFloatLiteralFormsAndWidthsCompileAndRun(t *testing.T) {
	tests := []struct {
		name  string
		width string
		value string
		want  string
	}{
		{"f64 fraction", "f64", "3.25", "3.250000\n"},
		{"f32 fraction", "f32", "3.25", "3.250000\n"},
		{"f64 exponent", "f64", "1.5e10", "15000000000.000000\n"},
		{"f32 exponent narrows", "f32", "1.5e10", "15000000512.000000\n"},
		{"f64 exponent upper E", "f64", "2.5E3", "2500.000000\n"},
		{"f64 negative exponent", "f64", "2.5e-3", "0.002500\n"},
		{"f32 negative fraction", "f32", "-2.5", "-2.500000\n"},
		{"f32 integer-valued exponent", "f32", "5e0", "5.000000\n"},
		{"f64 maximum magnitude", "f64", "1.7976931348623157e308", "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.000000\n"},
		{"f32 maximum magnitude", "f32", "3.40282346e38", "340282346638528859811704183484516925440.000000\n"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			src := "fn main() i32 { let x " + tc.width + " = " + tc.value + "; print x; return 0; }"
			if got := emitAndRunCapture(t, src, false, 0, false); got != tc.want {
				t.Fatalf("compiled program output = %q, want %q", got, tc.want)
			}
		})
	}
}
