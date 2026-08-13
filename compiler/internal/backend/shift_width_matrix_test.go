package backend

import (
	"bytes"
	"fmt"
	"strings"
	"testing"
)

// Shift width-matrix tests (Phase 3 #22). Shifts lower to checked runtime
// helpers (pebble_rt_checked_shl/shr_*) rather than plain C operators: the
// original i32/i64 pair plus a helper pair per narrower fixed-width integer
// (u8, u16, i8, i16, u32), and the u64 pair added when shifts were widened,
// which also serves uint (both carry the C type uint64_t). Before that fix the
// u64 shift was a clean Emit rejection and the uint shift failed with
// "unsupported uint expression node CheckedShift". This file pins the full
// << / >> matrix: compile+run for every (operator, width) pair across all 10
// integer widths, and folds in the edge cases around the u64/uint helpers
// (out-of-range amounts, bit-63 round-trip, mixed-width amounts, RELEASE-mode
// masking, emitted-helper-name assertions, negative amounts).

func TestShiftWidthMatrixCompileAndRun(t *testing.T) {
	t.Parallel()
	// (operator, width) pairs that must COMPILE AND RUN with the shift-correct
	// result. Each source is the probe shape `var r <W> = a <op> amount;` with
	// two same-width locals in an int-entry main, so the CheckedShift node
	// really emits at <W>. With a = 5 and amount = 2 the << result is 20 and
	// with amount = 1 the >> result is 2 — each operator pins a distinct
	// expected value that fits every width (all positive, small), so a
	// wrong-width or wrong-operator lowering changes the run's exit code and
	// fails the test. u64 and uint are the pairs that previously failed before
	// the Phase 3 #22 fix.
	for _, tc := range []struct {
		name   string
		width  string
		op     string
		amount int
		want   int
	}{
		{"shl i8", "i8", "<<", 2, 20},
		{"shr i8", "i8", ">>", 1, 2},
		{"shl i16", "i16", "<<", 2, 20},
		{"shr i16", "i16", ">>", 1, 2},
		{"shl i32", "i32", "<<", 2, 20},
		{"shr i32", "i32", ">>", 1, 2},
		{"shl i64", "i64", "<<", 2, 20},
		{"shr i64", "i64", ">>", 1, 2},
		{"shl int", "int", "<<", 2, 20},
		{"shr int", "int", ">>", 1, 2},
		{"shl u8", "u8", "<<", 2, 20},
		{"shr u8", "u8", ">>", 1, 2},
		{"shl u16", "u16", "<<", 2, 20},
		{"shr u16", "u16", ">>", 1, 2},
		{"shl u32", "u32", "<<", 2, 20},
		{"shr u32", "u32", ">>", 1, 2},
		{"shl u64", "u64", "<<", 2, 20},
		{"shr u64", "u64", ">>", 1, 2},
		{"shl uint", "uint", "<<", 2, 20},
		{"shr uint", "uint", ">>", 1, 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			src := fmt.Sprintf("fn main() int { var a %s = 5; var amount %s = %d; var r %s = a %s amount; return r as int; }", tc.width, tc.width, tc.amount, tc.width, tc.op)
			emitAndRun(t, src, false, tc.want, false)
		})
	}
}

func TestShiftU64OutOfRangeAbortsInSafeMode(t *testing.T) {
	// 64-bit widths: shift amount >= 64 must abort in SAFE mode, not wrap.
	emitAndRun(t, "fn main() int { var x u64 = 5; var z u64 = x << 64; return z as int; }", false, 0, true)
	emitAndRun(t, "fn main() int { var x uint = 5; var z uint = x << 64; return z as int; }", false, 0, true)
	emitAndRun(t, "fn main() int { var x u64 = 5; var z u64 = x >> 66; return z as int; }", false, 0, true)
	emitAndRun(t, "fn main() int { var x uint = 5; var z uint = x >> 66; return z as int; }", false, 0, true)
	// amount exactly 32 is in range (only >= 64 is out) — must NOT abort.
	emitAndRun(t, "fn main() int { var x u64 = 5; var z u64 = (x << 32) >> 32; return z as int; }", false, 5, false)
	emitAndRun(t, "fn main() int { var x uint = 5; var z uint = (x << 32) >> 32; return z as int; }", false, 5, false)
}

func TestShiftBit63RoundTrip(t *testing.T) {
	// Prove the value really shifts at 64 bits, not a narrower width: bit 63
	// set then cleared must round-trip to 1 at both u64 and uint.
	emitAndRun(t, "fn main() int { var x u64 = 1; var z u64 = (x << 63) >> 63; return z as int; }", false, 1, false)
	emitAndRun(t, "fn main() int { var x uint = 1; var z uint = (x << 63) >> 63; return z as int; }", false, 1, false)
}

func TestShiftMixedWidthAmount(t *testing.T) {
	// Shift amount of a narrower width than the value: the checker accepts any
	// integral amount, the emit path casts it to the value's C type.
	emitAndRun(t, "fn main() int { var x u64 = 5; var amount u8 = 2; var z u64 = x << amount; return z as int; }", false, 20, false)
	emitAndRun(t, "fn main() int { var x uint = 5; var amount i32 = 3; var z uint = x << amount; return z as int; }", false, 40, false)
	emitAndRun(t, "fn main() int { var x u64 = 5; var amount u64 = 2; var z u64 = x << amount; return z as int; }", false, 20, false)
	emitAndRun(t, "fn main() int { var x uint = 5; var amount uint = 2; var z uint = x << amount; return z as int; }", false, 20, false)
}

func TestShiftReleaseModeMasksAmount(t *testing.T) {
	// RELEASE mode masks the count to 64 and always shifts: 5 << 64 -> 5,
	// 5 << 66 -> 5 << 2 = 20.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var x u64 = 5; var z u64 = x << 64; return z as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 5, false, false)

	unit, snapshot, entryID, sources = buildFixture(t, "fn main() int { var x uint = 5; var z uint = x << 66; return z as int; }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary = compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 20, false, false)
}

func TestShiftEmittedHelperName(t *testing.T) {
	// Emitted C must call the u64 helper at both widths, never plain C shift.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var x u64 = 5; var z u64 = x << 2; return z as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_shl_u64(") {
		t.Fatalf("u64 shift did not call pebble_rt_checked_shl_u64:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_checked_shl_i32") || strings.Contains(out, "pebble_rt_checked_shl_i64") {
		t.Fatalf("u64 shift promoted to a narrower helper:\n%s", out)
	}

	unit, snapshot, entryID, sources = buildFixture(t, "fn main() int { var x uint = 5; var z uint = x << 2; return z as int; }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out = buf.String()
	if !strings.Contains(out, "pebble_rt_checked_shl_u64(") {
		t.Fatalf("uint shift did not call pebble_rt_checked_shl_u64:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_checked_shl_i32") || strings.Contains(out, "pebble_rt_checked_shl_i64") {
		t.Fatalf("uint shift promoted to a narrower helper:\n%s", out)
	}
}

func TestShiftNegativeAmountAbortsInSafeMode(t *testing.T) {
	// A negative shift amount is UB in C; the u64 helper's unsigned count
	// wraps it to a huge value the >= 64 check catches in SAFE mode.
	emitAndRun(t, "fn main() int { var x u64 = 5; var amount i64 = -1; var z u64 = x << amount; return z as int; }", false, 0, true)
}
