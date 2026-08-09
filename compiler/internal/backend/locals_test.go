package backend

import (
	"bytes"
	"strings"
	"testing"
)

func TestEmitUintHelperCallAsLocalInitializerCompilesAndRuns(t *testing.T) {
	// A call to a uint-returning helper used as a uint local's declaration
	// initializer (`var n = get_count();`) — the buildUintExpr DirectCall
	// gap: the checker routes a uint-typed initializer through buildUintExpr
	// (mirroring how buildExpr handles every other type), which had no
	// DirectCall case and rejected the shape with "unsupported uint
	// expression node DirectCall". The helper's C return type is uint64_t —
	// the exact C type a uint value uses — so the call emits directly with
	// no cast, and the program's exit code asserts the actual uint value
	// came back (get_count's 5 must reach the comparison through the uint
	// local), not just that Emit succeeded.
	emitAndRun(t, "fn get_count() uint { return 5; }\nfn main() i32 {\nvar n = get_count();\nif n != 5 { return 1; }\nreturn 0;\n}", false, 0, false)
}

func TestEmitUintHelperCallWithArgsAsLocalInitializerCompilesAndRuns(t *testing.T) {
	// The read_line-shaped variant of the buildUintExpr DirectCall gap: a
	// uint-returning helper taking a pointer and a char address-of cast to
	// *void, called as a uint local's initializer — the std/io.peb shape
	// `var bytes = read(file, &ch as *void, 1);` (read's declared result
	// type is uint, so bytes is inferred uint). The exit code asserts the
	// returned uint value (1, the max_bytes argument) reached the comparison
	// through the uint local, and that the pointer/char address-of
	// arguments — ordinary scalar/pointer shapes whose call produces no
	// pre-statement — passed through the call correctly.
	emitAndRun(t, "fn read(file *void, buffer *void, max_bytes uint) uint { return max_bytes; }\nfn main() i32 {\nvar file *void = nil;\nvar ch char = 'a';\nvar bytes = read(file, &ch as *void, 1);\nif bytes != 1 { return 1; }\nreturn 0;\n}", false, 0, false)
}

func TestEmitLocalDeclarationsCompilesAndRuns(t *testing.T) {
	// Two locals feeding the return: x = 1, y = 2, return x + y = 3. Each
	// local emits one `const int32_t pebble_local_<id> = ...;` declaration in
	// declaration order, and the return expression references them by name.
	emitAndRun(t, "fn main() i32 { let x i32 = 1; let y i32 = 2; return x + y; }", false, 3, false)
}

func TestEmitLocalReferencingEarlierLocalCompilesAndRuns(t *testing.T) {
	// A local's initializer references an earlier local (y = x + x = 20) and
	// the final return references a later one (y - x = 10), confirming the
	// locals-so-far set is threaded through both the initializer and return
	// expression builds.
	emitAndRun(t, "fn main() i32 { let x i32 = 10; let y i32 = x + x; return y - x; }", false, 10, false)
}

func TestEmitLocalOverflowStillAborts(t *testing.T) {
	// 2147483647 + 1 overflows i32. The overflow must survive through a local
	// reference, not just literal operands: x holds the max literal and the
	// return's x + 1 lowers to pebble_rt_checked_add_i32(pebble_local_<x>, 1),
	// which must panic through pebble_rt_panic in PEBBLE_RT_MODE_SAFE — the
	// process must terminate abnormally, not exit 0 and not return any
	// specific arithmetic value.
	emitAndRun(t, "fn main() i32 { let x i32 = 2147483647; return x + 1; }", false, 0, true)
}

func TestEmitIfElseLocalConditionAndArm(t *testing.T) {
	// A local declared before the if is visible in both the condition (x >= 10
	// is false for x = 7, so the else arm runs) and the else arm's return value
	// (x itself), proving the same locals set threads through the condition and
	// both arms.
	emitAndRun(t, "fn main() i32 { let x i32 = 7; if x >= 10 { return 1; } else { return x; } }", false, 7, false)
}

func TestEmitLogicalBoolLocalCombinationCompilesAndRuns(t *testing.T) {
	// The bool-local combination fixture: a && !b combines a bare bool local, a
	// negation of another bool local, and the && operator — three different
	// operand shapes in one ShortCircuitValue. a = true and !b = !false = true,
	// so the then-arm runs and the process exits 1.
	emitAndRun(t, "fn main() i32 { var a bool = true; var b bool = false; if a && !b { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitLocalInArmCompilesAndRuns(t *testing.T) {
	// A local declared inside an arm is now a supported block under the
	// recursive grammar: the then-arm's block is one Initialize followed by
	// its Return, and the local is visible to that same arm's return. This is
	// exactly the shape 10.7 rejected as "local declared in an arm", now
	// accepted end to end (exit code 5).
	emitAndRun(t, "fn main() i32 { if 1 < 2 { let y i32 = 5; return y; } else { return 0; } }", false, 5, false)
}

func TestEmitScopeIsolationCompilesAndRuns(t *testing.T) {
	// Both arms declare a source-level local named `a` with a different value,
	// and each arm references only its own `a`. The checker assigns the two
	// declarations distinct symbol IDs (confirmed against a real fixture
	// dump — 25 and 26), so each arm's `a` emits a distinct pebble_local_<id>
	// declared in its own C block; the true branch's 100 must win. This
	// exercises that the two arms' declarations don't collide with each
	// other's C names or scopes.
	emitAndRun(t, "fn main() i32 { if 1 < 2 { let a i32 = 100; return a; } else { let a i32 = 200; return a; } }", false, 100, false)
}

func TestEmitLocalsBeforeIfAndInArmCompilesAndRuns(t *testing.T) {
	// A local declared before the if is visible inside an arm, and the arm's
	// own local builds on top of it: x = 1, then-arm declares y = x + 1 and
	// returns it (2), while the else-arm returns the outer x (1). This proves
	// the outer local's declaration survives into the arm's scope while the
	// arm's own local is scoped to the arm.
	emitAndRun(t, "fn main() i32 { let x i32 = 1; if x < 10 { let y i32 = x + 1; return y; } else { return x; } }", false, 2, false)
}

func TestEmitReassignLocalCompilesAndRuns(t *testing.T) {
	// Reassignment of a `var` local is now a supported statement: x is
	// declared once and reassigned, and the final return reads the
	// reassigned value. This is exactly the shape 10.6 rejected (an
	// Initialize, a Store, then the Return), now accepted end to end —
	// the process must exit with the reassigned value 2.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; x = 2; return x; }", false, 2, false)
}

func TestEmitCompoundI64LocalCombinesViaI64Helper(t *testing.T) {
	// A compound assignment combines at the local's own declared width, not the
	// entry's: an i64 local inside an i64 entry combines through the _i64
	// checked helper (the checkedSuffix selection mirrors buildStoreCore's
	// targetInfo.kind dispatch). The runtime exit code cannot carry a full i64
	// (the low byte is all the OS sees), so the i64 story is proven by the
	// emitted helper name here and by the i64-boundary overflow abort in
	// TestEmitCompoundI64OverflowStillAborts.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { var x i64 = 9223372036854775800; x += 5; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_add_i64(pebble_local_27, 5") {
		t.Fatalf("emitted C does not combine at i64 through the checked helper:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_checked_add_i32") {
		t.Fatalf("emitted C combines at i32 instead of the local's own i64 width:\n%s", out)
	}
}

func TestEmitCompoundI64LocalInsideI32Function(t *testing.T) {
	// An i64 local inside an i32 entry combines at i64 (buildExpr at the
	// local's own width, the _i64 checked helper) even though the entry's
	// resolved width is i32 — the same width independence a plain i64
	// reassignment already has. 21 += 21 makes 42, returned as i32 after the
	// cast.
	emitAndRun(t, "fn main() i32 { var y i64 = 21; y += 21; return y as i32; }", false, 42, false)
}

func TestEmitCompoundFloatLocalCompilesAndRuns(t *testing.T) {
	// A float compound assignment (checker-reachable: the -=, *=, /= families
	// are NumericSame and += is Add, both admitting floats) combines with the
	// same plain C operator buildFloatExpr's BinaryValue case uses — floats
	// have no checked arithmetic anywhere in this backend, so x += 1.0 emits
	// x = (x + 1.0). 2.5 += 1.0 = 3.5, truncated to exit code 3 by the C
	// float-to-int conversion of the process exit.
	emitAndRun(t, "fn main() f32 { var x f32 = 2.5; x += 1.0; return x; }", false, 3, false)
}

func TestEmitBoolLocalDeclarationCompilesAndRuns(t *testing.T) {
	// 10.14 makes a bool local with a bool literal initializer a supported
	// shape: the local emits `bool pebble_local_<id> = true;` and the i32
	// entry body continues to the return. This is exactly the fixture 10.13
	// rejected as a non-i32 local (the initializer is a bool literal, not an
	// i32 expression), now the new positive case. The unused bool local does
	// not disturb the i32 return value, exit code 1.
	emitAndRun(t, "fn main() i32 { let flag bool = true; return 1; }", false, 1, false)
}

func TestEmitBoolLocalIfCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: a bare bool local as an if condition. flag is
	// declared true and used directly (no comparison), so the then-arm runs
	// and the process exits 1 — proving a condition can be a bare reference to
	// an in-scope bool local, skipping the BinaryValue comparison path
	// entirely.
	emitAndRun(t, "fn main() i32 { var flag bool = true; if flag { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitBoolLocalIfWritesC(t *testing.T) {
	// The emitted C for the bare-bool if: the bool local must be declared with
	// the C bool keyword (backed by #include <stdbool.h>) and referenced
	// directly in the if condition, with the arms' returns indented one level.
	// Symbol 25 is the flag local, confirmed against the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var flag bool = true; if flag { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"#include <stdbool.h>",
		"bool pebble_local_27 = true;",
		"    if (pebble_local_27) {\n",
		"        return 1;",
		"        return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t pebble_local_25") {
		t.Errorf("emitted C declared the bool local as an integer:\n%s", out)
	}
}

func TestEmitBoolLocalReassignCompilesAndRuns(t *testing.T) {
	// A bool local reassigned: flag is declared false, then a Store reassigns
	// it to true before the bare-bool if, so the then-arm runs and the process
	// exits 1. This proves a Store into a bool local is emitted and validated
	// against the bool grammar, mirroring how integer reassignment works.
	emitAndRun(t, "fn main() i32 { var flag bool = false; flag = true; if flag { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitBoolEqualityLocalCompilesAndRuns(t *testing.T) {
	// The bare bool-local version: a == b compares two declared bool locals
	// directly, the same BinaryValue(Equal) shape with SymbolValue operands
	// instead of wrapped comparisons (confirmed against the real fixture dump).
	// a = true, b = false, so a == b is false and the else arm runs (exit 2);
	// with b = true the equality holds and the then arm runs (exit 1).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal false", "fn main() i32 { var a bool = true; var b bool = false; if a == b { return 1; } else { return 2; } }", 2},
		{"equal true", "fn main() i32 { var a bool = true; var b bool = true; if a == b { return 1; } else { return 2; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitI64NowDeclaresI32Local(t *testing.T) {
	// Stage 2's fix inverted this test's original assertion: an i32 local
	// inside an i64 entry was previously a clean width-mismatch rejection and
	// is now a legal, supported declaration (the local's own declared width,
	// not the entry's, governs its C type). The fixture declares the i32
	// local and returns the i64 constant, never leaking the i32 value without
	// a cast.
	emitAndRun(t, "fn main() i64 { let x i32 = 1; return 2; }", false, 2, false)
}

func TestEmitI32NowDeclaresI64Local(t *testing.T) {
	// Stage 2's fix inverted this test's original assertion: an i64 local
	// inside an i32 entry was previously a clean width-mismatch rejection and
	// is now a legal, supported declaration (the local's own declared width,
	// not the entry's, governs its C type and its initializer's ambient build
	// width). The fixture declares the i64 local and returns the i32 constant,
	// never leaking the i64 value without a cast.
	emitAndRun(t, "fn main() i32 { let x i64 = 1; return 2; }", false, 2, false)
}

func TestEmitDeclaresI64LocalInsideI32Function(t *testing.T) {
	// Stage 2's minimal repro: a local declared at a different integer width
	// than its own function. var y i64 = 100; inside an i32-returning
	// function is a plain i64 local used only internally (never returned or
	// leaked without a cast): buildScalarInitializeCore emits it as a C
	// int64_t (the local's own declared width, not the entry's i32), and the
	// function returns x unchanged as the exit code.
	emitAndRun(t, "fn main() i32 { var x i32 = 5; var y i64 = 100; return x; }", false, 5, false)
}

func TestEmitI64LocalsArithmeticInsideI32Function(t *testing.T) {
	// A local of the other width actually used in arithmetic with other
	// locals of that same other width, not just declared and ignored: two
	// i64 locals inside an i32 function are added together at i64 (buildExpr
	// builds both operands at the locals' own i64 width, lowering through the
	// i64 checked-arithmetic helper) and the explicitly-cast result is
	// returned as the exit code.
	emitAndRun(t, "fn main() i32 { var a i64 = 21; var b i64 = 21; return (a + b) as i32; }", false, 42, false)
}

func TestEmitReassignsI64LocalInsideI32Function(t *testing.T) {
	// A reassignment (a Store), not just the initial Initialize: an i64
	// local declared inside an i32 function is reassigned later in the same
	// body. buildStoreCore must build the new value at the local's own
	// recorded i64 width (not the entry's i32) and emit
	// `pebble_local_<sym> = <value>`; the reassigned value is then cast back
	// to i32 for the exit code.
	emitAndRun(t, "fn main() i32 { var y i64 = 100; y = 7; return y as i32; }", false, 7, false)
}

func TestEmitDeclaresU32LocalInsideI32Function(t *testing.T) {
	// A uint-family local (u32, not the i32/i64 pair) to confirm the fix is
	// generic across integer widths rather than hardcoded: a u32 local is
	// declared and then reassigned inside an i32 function, its value cast
	// back to i32 for the exit code. (Arithmetic on u32 is deliberately
	// avoided: the backend's checked-arithmetic helpers only cover i32/i64.)
	emitAndRun(t, "fn main() i32 { var a u32 = 40; a = 2; return a as i32; }", false, 2, false)
}

func TestEmitF64LocalDeclaresAndReturns(t *testing.T) {
	// Float Stage A's minimal repro (the required test 1): an f64 local is
	// declared, read back by a bare reference (the Return's SymbolValue), and
	// returned from an f64-returning main. buildScalarInitializeCore must emit
	// the local at its own float C type double, buildFloatExpr must both
	// accept the 3.14 FloatLiteral initializer and the SymbolValue reading the
	// local back, and buildBlock's tail-return float dispatch must emit
	// `return pebble_local_<sym>;`. The compile-and-run check asserts the
	// harness's observable contract: the hosted int main narrows the returned
	// double to the process exit code by C's float-to-integer truncation, so
	// 3.14 exits 3. That single code cannot distinguish 3.14 from (say) 3.0,
	// so the emitted-C assertions below pin the real value: the local is a C
	// double initialized from the untouched literal text 3.14, and the return
	// value is the local by name rather than a re-emitted (possibly
	// re-rounded) literal.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x f64 = 3.14; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"double pebble_local_",
		"= 3.14;",
		"return pebble_local_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 3, false)
}

func TestEmitF32LocalDeclaresAndReturns(t *testing.T) {
	// Confirms the float case is not hardcoded to one width (required test
	// 2): an f32 local (not f64) is declared and returned from an f32-
	// returning main. buildScalarInitializeCore must pick floatCType(F32) =
	// float for the C declaration, and buildFloatExpr must build the 3.9
	// initializer and the read-back SymbolValue at the f32 kind. f32 round-
	// trips as a C float (3.9f), and the hosted main truncates it to exit
	// code 3.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f32 { var x f32 = 3.9; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"float pebble_local_",
		"= 3.9;",
		"return pebble_local_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 3, false)
}

func TestEmitReassignsF64LocalAndReturns(t *testing.T) {
	// Required test 4: a Store reassigns an already-declared f64 local
	// (x = 2.5;), so buildStoreCore's float case must build the new value via
	// buildFloatExpr at the local's own recorded f64 kind and emit
	// `pebble_local_<sym> = 2.5;`. The exit code (2) is the truncation of the
	// reassigned 2.5, and the emitted-C assertion pins the literal 2.5 in the
	// Store (the declaration carried 1.25, so the 2.5 substring can only come
	// from the reassignment).
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x f64 = 1.25; x = 2.5; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"double pebble_local_",
		"= 1.25;",
		"pebble_local_",
		"= 2.5;",
		"return pebble_local_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitFloatComparisonBetweenLocalsCompilesAndRuns(t *testing.T) {
	// Float comparisons use the same BinaryValue condition path as integer
	// comparisons, but operands are built by buildFloatExpr at f64 width.
	emitAndRun(t, "fn main() f64 { var a f64 = 1.5; var b f64 = 2.5; if a < b { return 7.0; } else { return 3.0; } }", false, 7, false)
}

func TestEmitIntegerToFloatOfI64LocalCompilesAndRuns(t *testing.T) {
	// The IntegerToFloat child is resolved at its OWN integer width, not the
	// entry's: an i64 local (different from the f64 main's own width grammar)
	// cast to a float must still build the child via buildExpr at i64 width.
	// 16777217 (2^24+1) is not representable exactly in f32, so the narrowing
	// cast to f32 rounds it to 16777216.0f; adding 1.0 to the widened result
	// and truncating exits 16777217 % 256 == 1.
	emitAndRun(t, "fn main() f64 { var x i64 = 16777217; return ((x as f32) as f64) + 1.0; }", false, 1, false)
}

func TestEmitHelperCallInLocalInitializerCompilesAndRuns(t *testing.T) {
	// A helper call in a local's initializer: x is declared as the helper's
	// result, and the return reads it — the locals scope threads through the
	// call expression like any other expression of the entry's width.
	emitAndRun(t, "fn helper() i32 { return 7; } fn main() i32 { let x i32 = helper(); return x + 1; }", false, 8, false)
}

func TestEmitStrLocalUnusedCompilesClean(t *testing.T) {
	// A str-typed local declared and never referenced beyond its own
	// declaration must still compile clean under -Wall -Wextra -Werror: the
	// emitted PebbleStr declaration carries the escaped bytes and compile-time
	// length, and the (void) cast immediately after suppresses the
	// -Wunused-variable warning exactly as every other local's does.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; return 1; }", false, 1, false)
}

func TestEmitStrLocalEscapedUnusedCompilesClean(t *testing.T) {
	// Same unused-local shape but with a decoded content that forces C escapes
	// (newline, tab, quote, backslash, and a control byte), so the escaped
	// C literal itself is exercised under -Wall -Wextra -Werror (a malformed
	// escape would not compile, and a silently-wrong one would still compile
	// here — the byte-correctness is asserted by the round-trip test below).
	emitAndRun(t, "fn main() i32 { let s str = \"a\\n1\\t\\\"\\\\\\0\"; return 1; }", false, 1, false)
}

func TestEmitStrLocalAndLiteralEqualCompilesAndRuns(t *testing.T) {
	// A str local compared against a string literal — the mixed-operand shape:
	// one SymbolValue (a str local) and one StringLiteral. The local was
	// declared from the same decoded bytes as the literal, so equality holds
	// and the then-arm runs, exiting 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; if s == \"hi\" { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrParameterLocalCompilesAndRuns(t *testing.T) {
	// A str-typed parameter passed a str-typed local (not a literal directly)
	// at the call site: the local's decoded content flows into the callee
	// through the parameter, proving the value is passed rather than
	// re-created at the call site. With the local holding "hi" the comparison
	// is true (exit 1); with "ho" it is false (exit 0).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"local equals literal", "fn f(s str) i32 { if s == \"hi\" { return 1; } else { return 0; } } fn main() i32 { let x str = \"hi\"; return f(x); }", 1},
		{"local differs from literal", "fn f(s str) i32 { if s == \"hi\" { return 1; } else { return 0; } } fn main() i32 { let x str = \"ho\"; return f(x); }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStrReturningHelperLocalDeclarationCompilesAndRuns(t *testing.T) {
	// A str-returning helper whose result is used in a str-typed local's
	// declaration, then compared: g returns "hi", the entry declares s from the
	// call (the one supported call position for declaring a str local — the
	// direct initializer of a matching str local), and the comparison against
	// the literal proves the returned value landed in the local. 7 on the
	// then-arm, 3 on the else.
	emitAndRun(t, "fn g() str { return \"hi\"; } fn main() i32 { let s str = g(); if s == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// A str-returning helper forwarding an already-declared str local: g
	// declares its own str local and `return s;` forwards it (a plain
	// SymbolValue return, the str analog of the tuple/struct forward), so the
	// return statement emits `return pebble_local_<s>;`. The returned value
	// lands in the entry's local and compares equal. 7 on the then-arm.
	emitAndRun(t, "fn g() str { var s str = \"hi\"; return s; } fn main() i32 { let s str = g(); if s == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrIndexRuntimeLocalCompilesAndRuns(t *testing.T) {
	// The index is a runtime-computed width-typed local, not a literal: i is
	// declared i32 and computed by checked arithmetic (1 + 1 = 2), so the
	// CheckedIndex's index child is a CheckedArithmetic node built by
	// buildExpr. s = "abc", s[i] = s[2] = 'c', exit 1.
	emitAndRun(t, "fn main() i32 { let s str = \"abc\"; let i i32 = 1 + 1; let c char = s[i]; if c == 'c' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexLocalReferenceCompilesAndRuns(t *testing.T) {
	// The index is a plain width-typed local reference (a SymbolValue built by
	// buildExpr), reaching s[i] at a runtime-computed position: i = 1, so
	// s[1] = 'i'. Proves the width-typed SymbolValue index path, distinct from
	// the literal and arithmetic shapes.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let i i32 = 1; let c char = s[i]; if c == 'i' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitSwitchMultipleCasesWithLocalsCompilesAndRuns(t *testing.T) {
	// A switch where each case body declares its own local — confirming
	// scope isolation between arms. Case 1 declares x=10 and returns x; case
	// 2 declares x=20 and returns x; else returns 0. Subject 2 returns 20.
	emitAndRun(t, "fn main() i32 { switch 2 { case 1: { let x i32 = 10; return x; } case 2: { let x i32 = 20; return x; } else: return 0; } }", false, 20, false)
}

func TestEmitDeferNestedScopesCompilesAndRuns(t *testing.T) {
	// Defers in multiple nested scopes crossing an exit: a defer at the
	// function's top level, a while loop with its own defer, and a break
	// inside the loop. The break's DeferChain should include the loop's
	// defer but NOT the function-level defer. After break, the function's
	// defer fires on the return.
	// x=0, outer defer x=x+100 registered.
	// Loop: i=0, inner defer x=x+1 registered. i==0 -> break.
	// Break's DeferChain: inner defer only. x = 0+1 = 1.
	// Return's DeferChain: outer defer. x = 1+100 = 101.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; defer x = x + 100; var i i32 = 0; while i < 5 { defer x = x + 1; if i == 0 { break; } i = i + 1; } return x; }", false, 101, false)
}

func TestEmitCharLocalEqualityTrueCompilesAndRuns(t *testing.T) {
	// A char-typed local declared from a literal, compared for equality,
	// driving a return value — the true outcome: c and d both hold 'a', so
	// c == d and the process exits 1. Exercises the full char path: a
	// CharLiteral local declaration, two char local references as == operands,
	// and the equality feeding a tail return.
	emitAndRun(t, "fn main() i32 { let c char = 'a'; let d char = 'a'; if c == d { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharLocalEqualityFalseCompilesAndRuns(t *testing.T) {
	// The false outcome of the equality fixture: d holds 'b' instead of 'a',
	// so c == d is false and the process exits 0 — proving the comparison
	// actually distinguishes the two scalar values rather than always being
	// true.
	emitAndRun(t, "fn main() i32 { let c char = 'a'; let d char = 'b'; if c == d { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitCharReassignmentFromLocalCompilesAndRuns(t *testing.T) {
	// A char-typed reassignment from another char-typed local: a holds 'a', b
	// holds 'b', a = b copies b's scalar value into a, and comparing a against
	// 'b' afterwards proves the copy landed — the char-typed local reference
	// is a valid reassignment right-hand side.
	emitAndRun(t, "fn main() i32 { var a char = 'a'; let b char = 'b'; a = b; if a == 'b' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharLocalFromLocalCompilesAndRuns(t *testing.T) {
	// A char-typed local declared from a char-typed local reference: b is
	// declared from a (confirmed checker-reachable against a real fixture), so
	// b holds 'a' and the comparison is true. This exercises the SymbolValue
	// initializer shape a char local's declaration accepts.
	emitAndRun(t, "fn main() i32 { let a char = 'a'; let b char = a; if b == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// A char-returning helper whose result is forwarded through its own local:
	// f declares x from its char parameter and returns x, proving a char local
	// inside a helper and a char return value both build correctly, and main
	// compares the surviving value.
	emitAndRun(t, "fn f(c char) char { var x char = c; return x; } fn main() i32 { let c char = f('a'); if c == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharCallArgumentLocalCompilesAndRuns(t *testing.T) {
	// A char-typed local passed as a call argument: main declares c from a
	// literal and passes it to f, which compares it against 'a' — the
	// char-typed local reference is a valid call-site argument for a char
	// parameter.
	emitAndRun(t, "fn f(x char) i32 { if x == 'a' { return 1; } else { return 0; } } fn main() i32 { let c char = 'a'; return f(c); }", false, 1, false)
}

func TestEmitCharLocalFromCallCompilesAndRuns(t *testing.T) {
	// A char-typed local declared from a call to a char-returning helper
	// (confirmed checker-reachable against a real fixture): c is declared from
	// f('a') and compared, proving the DirectCall initializer shape works for
	// a char local.
	emitAndRun(t, "fn f(x char) char { return x; } fn main() i32 { let c char = f('a'); if c == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitNilPointerLocalCompilesAndRuns(t *testing.T) {
	// let p *i32 = nil; return 0;
	// Declaring a nil pointer local is valid; we just don't dereference it.
	emitAndRun(t, "fn main() i32 { let p *i32 = nil; return 0; }", false, 0, false)
}

func TestEmitFunctionTypedLocalCompilesAndRuns(t *testing.T) {
	// The exact minimal repro: a function-typed local initialized from a bare
	// top-level function reference (a HoistedFunctionValue), called through
	// an indirect call. add's own C name (pebble_fn_<symbol>) decays to a
	// function pointer matching the local's pebble_fnptr_<typeID>_t typedef,
	// no cast needed.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn main() int { var f fn(int, int) int = add; return f(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedLocalReassignmentCompilesAndRuns(t *testing.T) {
	// Reassigning a function-typed local to a different function of the same
	// signature (f = sub;) — the buildStoreCore functionType branch.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn sub(a int, b int) int { return a - b; } fn main() int { var f fn(int, int) int = add; f = sub; return f(5, 2); }", false, 3, false)
}

func TestEmitFunctionTypedLocalBoolSignatureCompilesAndRuns(t *testing.T) {
	// A bool-parameter/bool-result function type — confirms the
	// parameter/result C-type dispatch isn't hardcoded to int. This also
	// exercises the bool-returning-helper support added as a genuine
	// prerequisite (validateHelperSignature previously rejected any
	// bool-result helper outright, which would have made `id` itself
	// unemittable).
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { var f fn(bool) bool = id; if f(true) { return 1; } else { return 2; } }", false, 1, false)
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { var f fn(bool) bool = id; if f(false) { return 1; } else { return 2; } }", false, 2, false)
}

func TestEmitFunctionTypedLocalWritesC(t *testing.T) {
	// Confirm the emitted C directly: the typedef shape
	// (typedef <ret> (*pebble_fnptr_<id>_t)(PebbleContext *ctx, ...);), the
	// function value assigned bare (no cast) at the declaration site, and the
	// indirect call threading ctx as the first argument.
	unit, snapshot, entryID, sources := buildFixture(t, "fn add(a int, b int) int { return a + b; } fn main() int { var f fn(int, int) int = add; return f(1, 2); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"(*pebble_fnptr_",
		")(PebbleContext *ctx, int32_t, int32_t);",
		"pebble_fnptr_",
		"= pebble_fn_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "(pebble_fnptr_") && strings.Contains(out, ")pebble_fn_") {
		t.Errorf("function value declaration should not be cast, found a cast form:\n%s", out)
	}
}

func TestEmitPointerFunctionTypeLocalParameterCompilesAndRuns(t *testing.T) {
	// The exact minimal repro for the pointer PARAMETER gap: a function-typed
	// local whose signature takes a `*int` parameter (`fn(*int) int`), called
	// through an indirect call with the address of an entry local. The fnptr
	// typedef's parameter slot is spelled `int32_t *` (the same pointerTypeName
	// spelling helperSignature gives an ordinary helper's pointer parameter),
	// and buildCallArgument builds the &x argument through buildExpr's pointer
	// path — no cast at the call site.
	emitAndRun(t, "fn readPtr(p *int) int { return *p; } fn main() int { var x int = 42; var f fn(*int) int = readPtr; return f(&x); }", false, 42, false)
}
