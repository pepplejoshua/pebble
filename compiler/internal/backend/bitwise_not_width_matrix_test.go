package backend

import "testing"

// Bitwise not (~) width-matrix tests (Phase 3 #25). Bitwise NOT is NOT a
// checked-overflow operation: it lowers to the plain C ~ operator at every
// width (no runtime helper at any width, same as & | ^, Phase 3 #21). The
// tracker row's "Partial by backend width" pointed at the exact same bug
// class #21 found for uint's & | ^: buildUintExpr (the dedicated uint
// grammar builder) had no PrefixValue case at all, so any uint `~x`
// expression died at Emit with "unsupported uint expression node
// PrefixValue" even though every other integer width already lowered ~
// correctly through buildExpr's own PrefixValue case. Fixed by adding the
// case, mirroring the existing BinaryValue/CheckedArithmetic pattern in the
// same builder exactly. bool is not a valid ~ operand (the checker's
// integral-capability check rejects it) — confirmed intentional, not a gap.

func TestBitwiseNotWidthMatrixCompileAndRun(t *testing.T) {
	t.Parallel()
	// Every integer width must compile and run: a starts at all-ones-minus-5
	// in its own width's bit pattern, ~a recovers 5. uint is the pair that
	// previously rejected at Emit ("unsupported uint expression node
	// PrefixValue") before the Phase 3 #25 fix.
	for _, tc := range []struct {
		width string
		value string
	}{
		{"i8", "-6"},
		{"i16", "-6"},
		{"i32", "-6"},
		{"i64", "-6"},
		{"int", "-6"},
		{"u8", "250"},
		{"u16", "65530"},
		{"u32", "4294967290"},
		{"u64", "18446744073709551610"},
		{"uint", "18446744073709551610"},
	} {
		t.Run(tc.width, func(t *testing.T) {
			t.Parallel()
			src := "fn main() int { var a " + tc.width + " = " + tc.value + "; var r " + tc.width + " = ~a; return r as int; }"
			emitAndRun(t, src, false, 5, false)
		})
	}
}

func TestBitwiseNotUintPositionsCompileAndRun(t *testing.T) {
	t.Parallel()
	// uint's ~ must work in every value position: return, call argument,
	// whole-value store, nested inside another binary/arithmetic expression,
	// and as the operand of an explicit cast.
	allOnesMinusFive := "18446744073709551610" // ~x = 5
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"return", "fn f() uint { var a uint = " + allOnesMinusFive + "; return ~a; } fn main() int { return f() as int; }", 5},
		{"call-argument", "fn f(x uint) int { return x as int; } fn main() int { var a uint = " + allOnesMinusFive + "; return f(~a); }", 5},
		{"whole-value-store", "fn main() int { var a uint = " + allOnesMinusFive + "; a = ~a; return a as int; }", 5},
		{"nested-in-bitwise", "fn main() int { var a uint = " + allOnesMinusFive + "; var r uint = (~a) ^ 3; return r as int; }", 6},
		{"nested-in-arithmetic", "fn main() int { var a uint = " + allOnesMinusFive + "; var r uint = (~a) + 3; return r as int; }", 8},
		{"cast-operand", "fn main() int { var a uint = " + allOnesMinusFive + "; var r i64 = (~a) as i64; return r as int; }", 5},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestBitwiseNotBoolRejectedByChecker(t *testing.T) {
	t.Parallel()
	// bool is not a valid ~ operand in this language — confirmed intentional
	// (the checker's integral-capability check rejects it), not a gap.
	src := "fn main() int { var a bool = true; var r bool = ~a; if r { return 1; } return 0; }"
	_, _, _, _, err := buildFixtureMaybeFailing(t, src, "main", false)
	if err == nil {
		t.Fatal("expected checker rejection of bitwise not on bool, got none")
	}
}
