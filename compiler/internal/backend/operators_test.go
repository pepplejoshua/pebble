package backend

import (
	"bytes"
	"strings"
	"testing"
)

func TestEmitIntEntryCheckedAddCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() int { let x int = 40; let y int = 2; return x + y; }", true, 42, false)
}

func TestEmitCheckedAddReturnCompilesAndRuns(t *testing.T) {
	// `return 1 + 2;` was rejected by 10.3; the checked arithmetic expression
	// tree is now accepted and lowered to pebble_rt_checked_add_i32(1, 2),
	// which must produce exit code 3 end to end.
	emitAndRun(t, "fn main() i32 { return 1 + 2; }", false, 3, false)
}

func TestEmitCheckedArithmeticPrecedenceCompilesAndRuns(t *testing.T) {
	// 1 + 2 * 3 must compute as 1 + (2 * 3) = 7. Precedence is already
	// resolved in the typed IR the checker built (the * node is a child of
	// the + node); the emitter only walks the tree, it does not re-implement
	// precedence.
	emitAndRun(t, "fn main() i32 { return 1 + 2 * 3; }", false, 7, false)
}

func TestEmitCheckedNegateFeedsArithmeticCompilesAndRuns(t *testing.T) {
	// A CheckedNegate feeding into a CheckedArithmetic: -5 + 10 = 5. This
	// exercises pebble_rt_checked_neg_i32(5) inside the add's left operand.
	emitAndRun(t, "fn main() i32 { return -5 + 10; }", false, 5, false)
}

func TestEmitBitwiseOperatorsCompilesAndRuns(t *testing.T) {
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"and", "fn main() i32 { let a i32 = 12; let b i32 = 10; return a & b; }", 8},
		{"or", "fn main() i32 { let a i32 = 12; let b i32 = 10; return a | b; }", 14},
		{"xor", "fn main() i32 { let a i32 = 12; let b i32 = 10; return a ^ b; }", 6},
		{"not", "fn main() i32 { let a i32 = 10; return (~a) & 15; }", 5},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitBitwiseCombinedExpressionCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a i32 = 12; let b i32 = 10; let c i32 = 3; return (a & b) | c; }", false, 11, false)
}

func TestEmitBitwiseI64CompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i64 { let a i64 = 12; let b i64 = 10; return (a ^ b) | 8; }", false, 14, false)
}

func TestEmitCheckedDivisionCompilesAndRuns(t *testing.T) {
	// 7 / 2 = 3 (plain C division truncates toward zero, which is also the
	// language's semantics): the CheckedArithmetic node with operator Slash is
	// now lowered to pebble_rt_checked_div_i32(7, 2), exit code 3.
	emitAndRun(t, "fn main() i32 { return 7 / 2; }", false, 3, false)
}

func TestEmitCheckedModuloCompilesAndRuns(t *testing.T) {
	// 7 % 2 = 1, lowered to pebble_rt_checked_mod_i32(7, 2), exit code 1.
	emitAndRun(t, "fn main() i32 { return 7 % 2; }", false, 1, false)
}

func TestEmitCheckedArithmeticOverflowAborts(t *testing.T) {
	// 2147483647 + 1 overflows i32. Compiled in PEBBLE_RT_MODE_SAFE (the
	// same mode the other end-to-end tests use), the emitted
	// pebble_rt_checked_add_i32 call must panic through pebble_rt_panic, so
	// the process must terminate abnormally — not exit 0 and not return any
	// specific arithmetic value.
	emitAndRun(t, "fn main() i32 { return 2147483647 + 1; }", false, 0, true)
}

func TestEmitCheckedArithmeticOverflowEmitsRealSourceLoc(t *testing.T) {
	// The overflow behavior is unchanged (the process still aborts via
	// pebble_rt_panic), and — new for this slice — the emitted
	// pebble_rt_checked_add_i32 call now carries the checked expression's own
	// resolved Pebble source location as its final argument, not the
	// zero-valued placeholder: (PebbleSourceLoc){"main.peb", 1, 24} for the
	// `2147483647 + 1` expression on the fixture's single line. Both halves
	// are proved here: the emitted C text is inspected directly for the
	// non-placeholder compound literal, and the compiled binary is run to
	// confirm the overflow still panics.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 2147483647 + 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, `pebble_rt_checked_add_i32(2147483647, 1, (PebbleSourceLoc){"main.peb", 1, 24})`) {
		t.Errorf("emitted C lacks a real source location on the checked-add call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
}

func TestEmitCheckedDivideByZeroAborts(t *testing.T) {
	// 1 / 0 divides by zero. The emitted pebble_rt_checked_div_i32 call must
	// panic through pebble_rt_panic (divide-by-zero is a fault in every
	// configuration, not just SAFE), so the process must terminate abnormally
	// — not exit 0 and not return any specific numeric value.
	emitAndRun(t, "fn main() i32 { return 1 / 0; }", false, 0, true)
}

func TestEmitCheckedShiftsCompileAndRun(t *testing.T) {
	emitAndRun(t, "fn main() i32 { return (3 << 4) >> 2; }", false, 12, false)
	emitAndRun(t, "fn main() i64 { return (3 << 4) >> 2; }", false, 12, false)
}

func TestEmitCheckedShiftAcceptsNarrowerAmount(t *testing.T) {
	emitAndRun(t, "fn main() i32 { var amount u8 = 2; return 1 << amount; }", false, 4, false)
}

func TestEmitCheckedShiftOutOfRangeAbortsInSafeMode(t *testing.T) {
	emitAndRun(t, "fn main() i32 { return 1 << 32; }", false, 0, true)
}

func TestEmitCheckedShiftMasksCountInReleaseMode(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 1 << 35; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 8, false, false)
}

func TestEmitCheckedShiftNegativeCountMasksInReleaseMode(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var one i32 = 1; var amount i32 = -1; return (one << amount) >> 31; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 255, false, false)
}

func TestEmitCheckedShiftsNarrowWidthsCompileAndRun(t *testing.T) {
	// u8/u16/i8/i16/u32 shifts, both directions, compile AND run: each width
	// now has its own runtime shift-helper pair (pebble_rt_checked_shl/shr_<w>)
	// instead of the pre-widening misleading rejection ("CheckedShift with
	// operator <<, want << or >>" — the op switch matched fine; the missing
	// width-to-helper mapping was what returned false). The exit code is the
	// OS-visible low byte, so each fixture's result stays below 256 (or is the
	// exact two's-complement low byte for a negative result).
	emitAndRun(t, "fn main() int { var x u8 = 5; var z u8 = x << 2; return z as int; }", false, 20, false)
	emitAndRun(t, "fn main() int { var x u8 = 200; var z u8 = x >> 3; return z as int; }", false, 25, false)
	emitAndRun(t, "fn main() int { var x u16 = 5; var z u16 = x << 4; return z as int; }", false, 80, false)
	emitAndRun(t, "fn main() int { var x u16 = 40000; var z u16 = x >> 8; return z as int; }", false, 156, false)
	emitAndRun(t, "fn main() int { var x u32 = 5; var z u32 = x << 4; return z as int; }", false, 80, false)
	emitAndRun(t, "fn main() int { var x i8 = 5; var z i8 = x << 2; return z as int; }", false, 20, false)
	emitAndRun(t, "fn main() int { var x i16 = 5; var z i16 = x << 4; return z as int; }", false, 80, false)
	// A negative signed value shifts through the width's own helper: -5 << 2
	// = -20 (exit code 236, the low byte) and -8 >> 2 = -2 (exit code 254) —
	// the same arithmetic-shift semantics the i32/i64 helpers already apply.
	emitAndRun(t, "fn main() int { var x i8 = -5 as i8; var z i8 = x << 2; return z as int; }", false, 236, false)
	emitAndRun(t, "fn main() int { var x i16 = -8 as i16; var z i16 = x >> 2; return z as int; }", false, 254, false)
}

func TestEmitCheckedShiftNarrowWidthOutOfRangeAbortsInSafeMode(t *testing.T) {
	// The narrower widths' bounds checks are width-correct, not promoted-to-i32:
	// a shift amount >= the operand's own bit width (8, 16, or 32) aborts
	// through pebble_rt_checked_shl_<w> in PEBBLE_RT_MODE_SAFE, exactly as
	// 1 << 32 already aborts through the i32 helper.
	emitAndRun(t, "fn main() int { var x u8 = 5; var z u8 = x << 8; return z as int; }", false, 0, true)
	emitAndRun(t, "fn main() int { var x u16 = 5; var z u16 = x << 16; return z as int; }", false, 0, true)
	emitAndRun(t, "fn main() int { var x u32 = 5; var z u32 = x << 32; return z as int; }", false, 0, true)
	emitAndRun(t, "fn main() int { var x i8 = 5; var z i8 = x << 8; return z as int; }", false, 0, true)
}

func TestEmitCheckedShiftNarrowWidthMasksCountInReleaseMode(t *testing.T) {
	// RELEASE masks the count to the operand's own width and always shifts,
	// matching the i32/i64 helpers: 5 << 8 masks to 5 << 0 = 5, and
	// 5 << 10 masks to 5 << 2 = 20.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var x u8 = 5; var z u8 = x << 8; return z as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 5, false, false)

	unit, snapshot, entryID, sources = buildFixture(t, "fn main() int { var x u8 = 5; var z u8 = x << 10; return z as int; }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary = compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 20, false, false)
}

func TestEmitCheckedShiftNarrowWidthCallsItsOwnHelper(t *testing.T) {
	// The emitted C names the width-specific helper, not a promoted-to-i32
	// call: a u8 shift calls pebble_rt_checked_shl_u8, a u16 shift calls
	// pebble_rt_checked_shr_u16, and so on — and the value stays at its own
	// width (uint8_t / uint16_t), so no i32-width helper is ever used.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var x u8 = 5; var z u8 = x << 2; return z as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_shl_u8(pebble_local_27, (uint8_t)(2)") {
		t.Fatalf("emitted C does not call the u8 shift helper at its own width:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_checked_shl_i32") {
		t.Fatalf("emitted C promotes a u8 shift to the i32 helper:\n%s", out)
	}
}

func TestEmitCheckedShiftU64StillRejected(t *testing.T) {
	// The u64 shift twin (pebble_rt_checked_shl_u64/shr_u64) is still not
	// implemented, so a u64 shift stays a clean rejection rather than a call
	// to a nonexistent helper — this slice adds the narrower-width pairs only.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() int { var x u64 = 5; var z u64 = x << 2; return z as int; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want << or >>")
}

func TestEmitI64CheckedAddCompilesAndRuns(t *testing.T) {
	// The checked helpers must be the i64 family at the wider width, producing
	// the right result end to end.
	emitAndRun(t, "fn main() i64 { return 1 + 2; }", false, 3, false)
}

func TestEmitI64CheckedAddWritesC(t *testing.T) {
	// Assert the exact helper name: an i64 entry's CheckedArithmetic must lower
	// to pebble_rt_checked_add_i64, proving the resolved width really reaches
	// the runtime function-name selection rather than staying hardcoded _i32.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { return 1 + 2; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_rt_checked_add_i64(1, 2, (PebbleSourceLoc){\"main.peb\", 1, 24})",
		"static int64_t pebble_user_main(PebbleContext *ctx)",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_rt_checked_add_i32") {
		t.Errorf("emitted C uses the i32 checked helper for an i64 entry:\n%s", out)
	}
}

func TestEmitFloatArithmeticOperatorsCompileAndRun(t *testing.T) {
	for _, test := range []struct {
		name, source, emitted string
		expected              int
	}{
		{"addition", "fn main() f64 { var a f64 = 1.5; var b f64 = 2.5; return a + b; }", " + ", 4},
		{"subtraction", "fn main() f64 { var a f64 = 5.5; var b f64 = 2.5; return a - b; }", " - ", 3},
		{"multiplication", "fn main() f64 { var a f64 = 1.5; var b f64 = 2.0; return a * b; }", " * ", 3},
		{"division", "fn main() f64 { var a f64 = 8.0; var b f64 = 2.0; return a / b; }", " / ", 4},
	} {
		t.Run(test.name, func(t *testing.T) {
			unit, snapshot, entryID, sources := buildFixture(t, test.source, "main", false)
			var buf bytes.Buffer
			if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
				t.Fatalf("Emit failed: %v", err)
			}
			if !strings.Contains(buf.String(), test.emitted) {
				t.Fatalf("emitted C missing plain float operator %q:\n%s", test.emitted, buf.String())
			}
			compileAndRun(t, buf.Bytes(), test.expected, false)
		})
	}
}

func TestEmitU64CheckedArithmeticCompilesAndRuns(t *testing.T) {
	// u64 add/sub/mul as a non-ambient width (u64 operands in u64-returning
	// helpers called from an int-entry main, mirroring the i64
	// non-entry-width slice test's avoidance of the entry width coinciding
	// with the type under test). Each helper lowers its CheckedArithmetic to
	// pebble_rt_checked_add/sub/mul_u64, which must produce the arithmetically
	// correct result for ordinary non-wrapping operands, returned via `as
	// int` and asserted as the process exit code.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"add", "fn addU(x u64, y u64) u64 { return x + y; } fn main() int { let r u64 = addU(40, 2); return r as int; }", 42},
		{"sub", "fn subU(x u64, y u64) u64 { return x - y; } fn main() int { let r u64 = subU(50, 8); return r as int; }", 42},
		{"mul", "fn mulU(x u64, y u64) u64 { return x * y; } fn main() int { let r u64 = mulU(6, 7); return r as int; }", 42},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitU64CheckedArithmeticWritesU64Helper(t *testing.T) {
	// Assert the exact helper names in the emitted C: a u64-returning
	// helper's CheckedArithmetic must lower to pebble_rt_checked_add_u64 (not
	// the empty-suffix pebble_rt_checked_add_ the pre-fix code emitted, which
	// only failed later at cc compile time), proving the resolved u64 width
	// really reaches the runtime function-name selection.
	unit, snapshot, entryID, sources := buildFixture(t, "fn addU(x u64, y u64) u64 { return x + y; } fn main() int { let r u64 = addU(40, 2); return r as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_add_u64(") {
		t.Errorf("emitted C missing the u64 checked-add helper:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_checked_add_(") || strings.Contains(out, "pebble_rt_checked_add_i64(") {
		t.Errorf("emitted C uses a wrong-width checked-add helper for u64 operands:\n%s", out)
	}
}

func TestEmitU64CheckedArithmeticOverflowAborts(t *testing.T) {
	// u64 overflow detection is real, not just "it compiles": each of add
	// (UINT64_MAX + 1), sub (0 - 1), and mul (UINT64_MAX * 2) wraps the
	// unsigned width, and the emitted pebble_rt_checked_*_u64 helper must
	// detect it with __builtin_*_overflow and panic through pebble_rt_panic
	// in PEBBLE_RT_MODE_SAFE — the process must terminate abnormally, not
	// exit 0 and not return any specific wrapped value.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"add", "fn addU(x u64, y u64) u64 { return x + y; } fn main() int { let r u64 = addU(18446744073709551615, 1); return r as int; }"},
		{"sub", "fn subU(x u64, y u64) u64 { return x - y; } fn main() int { let r u64 = subU(0, 1); return r as int; }"},
		{"mul", "fn mulU(x u64, y u64) u64 { return x * y; } fn main() int { let r u64 = mulU(18446744073709551615, 2); return r as int; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, 0, true)
		})
	}
}

func TestEmitWrappingU64BuiltinsCompileAndRun(t *testing.T) {
	// The explicit wrapping u64 builtins (wrapping_mul_u64 / wrapping_add_u64)
	// lower to the runtime's pebble_rt_wrapping_<op>_u64 helpers. The lowering
	// resolves the call's symbol back to its BuiltinFunction identity through
	// the symbol table (emitSymbols, exactly like externCName), so these run
	// through the buildFixtureWithSymbols path, not the symbol-free
	// emitAndRun. Normal-case results surface through the entry exit code
	// (wrapping_mul_u64(6, 7) == 42, wrapping_add_u64(2, 3) == 5); then the
	// full boundary set mirrors runtime/test/smoke_test.c's
	// test_wrapping_arithmetic_normal, each assertion contributing a distinct
	// failure code so a regression in any one boundary is distinguishable.
	// Wrapping must not panic in SAFE mode, so every case runs in the standard
	// compileAndRun harness.
	emitAndRunWithSymbols(t, "fn main() int { return wrapping_mul_u64(6, 7) as int; }", 42)
	emitAndRunWithSymbols(t, "fn main() int { return wrapping_add_u64(2, 3) as int; }", 5)
	emitAndRunWithSymbols(t, `fn main() int {
    var r u64 = wrapping_mul_u64(6, 7);
    if r != 42 { return 1; }
    var w u64 = wrapping_mul_u64(1, 18446744073709551615);
    if w != 18446744073709551615 { return 2; }
    var z u64 = wrapping_mul_u64(0, 18446744073709551615);
    if z != 0 { return 3; }
    var m u64 = wrapping_mul_u64(18446744073709551615, 2);
    if m != 18446744073709551614 { return 4; }
    var a u64 = wrapping_add_u64(2, 3);
    if a != 5 { return 5; }
    var b u64 = wrapping_add_u64(0, 0);
    if b != 0 { return 6; }
    var c u64 = wrapping_add_u64(18446744073709551615, 0);
    if c != 18446744073709551615 { return 7; }
    var d u64 = wrapping_add_u64(18446744073709551615, 1);
    if d != 0 { return 8; }
    var e u64 = wrapping_add_u64(18446744073709551615, 18446744073709551615);
    if e != 18446744073709551614 { return 9; }
    return 0;
}`, 0)
}
