package backend

import "testing"

// Unary negation width-matrix tests (Phase 3 #23). Unary minus is a checked
// operation on signed integers (negating a type's minimum value overflows),
// lowered to pebble_rt_checked_neg_<width> at every signed width the checker
// admits. The runtime previously implemented only i32/i64 (the checkedSuffix
// family shared with index/slice-start/char-at/unwrap, which deliberately
// stays narrow); i8 and i16 negation was checker-accepted but Emit-rejected
// for any non-constant operand ("no checked-neg runtime helper"). Fixed by
// adding pebble_rt_checked_neg_i8/i16 to the runtime and a dedicated
// checkedNegSuffix selector (mirroring checkedShiftSuffix) so the negation
// family widens independently of the other checkedSuffix consumers. Unsigned
// widths (u8..u64, uint) are intentionally checker-rejected for unary minus —
// not a bug, confirmed by empirical probe — and floats (f32/f64) already
// worked via buildFloatExpr's plain-C negation, unrelated to this fix.

func TestUnaryNegationWidthMatrixCompileAndRun(t *testing.T) {
	t.Parallel()
	// Every signed integer width must compile and run: a = -5, r = -a = 5.
	// i8/i16 are the pair that previously rejected before the Phase 3 #23 fix.
	for _, width := range []string{"i8", "i16", "i32", "i64", "int"} {
		t.Run(width, func(t *testing.T) {
			t.Parallel()
			src := "fn main() int { var a " + width + " = -5; var r " + width + " = -a; return r as int; }"
			emitAndRun(t, src, false, 5, false)
		})
	}
}

func TestUnaryNegationMinimumValueAbortsInSafeMode(t *testing.T) {
	t.Parallel()
	// Negating a signed type's own minimum value is the one negation
	// overflow case — must abort in SAFE mode at every signed width,
	// including the two narrow widths this fix adds a helper for.
	for _, tc := range []struct {
		width string
		min   string
	}{
		{"i8", "-128"},
		{"i16", "-32768"},
		{"i32", "-2147483648"},
		{"i64", "-9223372036854775808"},
		{"int", "-9223372036854775808"},
	} {
		t.Run(tc.width, func(t *testing.T) {
			t.Parallel()
			src := "fn main() int { var a " + tc.width + " = " + tc.min + "; var r " + tc.width + " = -a; return r as int; }"
			emitAndRun(t, src, false, 0, true)
		})
	}
}

func TestUnaryNegationUnsignedRejectedByChecker(t *testing.T) {
	t.Parallel()
	// Unsigned widths have no unary minus in this language at all — the
	// checker rejects it before the backend ever sees it. Confirmed
	// intentional, not a gap: there is deliberately no
	// pebble_rt_checked_neg_* helper at any unsigned width.
	for _, width := range []string{"u8", "u16", "u32", "u64", "uint"} {
		t.Run(width, func(t *testing.T) {
			t.Parallel()
			src := "fn main() int { var a " + width + " = 5; var r " + width + " = -a; return r as int; }"
			_, _, _, _, err := buildFixtureMaybeFailing(t, src, "main", false)
			if err == nil {
				t.Fatalf("expected checker rejection of unary minus on %s, got none", width)
			}
		})
	}
}

func TestUnaryNegationFloatCompileAndRun(t *testing.T) {
	t.Parallel()
	// Float negation already worked (buildFloatExpr's plain-C '-'), unrelated
	// to this fix — locked in as a regression guard alongside the integer
	// matrix above.
	for _, width := range []string{"f32", "f64"} {
		t.Run(width, func(t *testing.T) {
			t.Parallel()
			src := "fn main() int { var a " + width + " = 5.0; var r " + width + " = -a; var r2 " + width + " = -r; return r2 as int; }"
			emitAndRun(t, src, false, 5, false)
		})
	}
}
