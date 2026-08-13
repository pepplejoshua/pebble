package backend

import (
	"strings"
	"testing"
)

// Range-loop width matrix (Phase 3 #28). The inclusive end rule (`..=`) is
// lowered with a done-gated loop so an unsigned iterator can never wrap one
// past the end bound; these tests anchor the iterator to every integer width
// the checker accepts as a range-loop bound type — i8, i16, i32, i64, int, u8,
// u16, u32, u64, uint — and assert the exact visited values, so a width or
// direction regression shows up as a wrong value string, not just a wrong
// count. Widths are anchored through typed bound locals rather than
// `sum = sum + i` accumulation, because checked arithmetic at i8/i16/u8/u16/u32
// has no runtime helper (a separate, general backend limitation unrelated to
// range loops).

func TestEmitRangeLoopAscendingExclusiveWidthMatrixCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// loop lo..hi with lo=0, hi=3 visits 0,1,2.
	for _, tc := range []struct {
		w    string
		vals string
	}{
		{"i8", "012"}, {"i16", "012"}, {"i32", "012"}, {"i64", "012"}, {"int", "012"},
		{"u8", "012"}, {"u16", "012"}, {"u32", "012"}, {"u64", "012"}, {"uint", "012"},
	} {
		tc := tc
		t.Run(tc.w, func(t *testing.T) {
			t.Parallel()
			src := "fn main() i32 { var lo " + tc.w + " = 0; var hi " + tc.w + " = 3; var count i32 = 0; loop lo..hi : i { print i; count = count + 1; } if count == 3 { return 42; } return 1; }"
			out := emitAndRunCaptureBounded(t, src, false, 42, false)
			if got := strings.ReplaceAll(out, "\n", ""); got != tc.vals {
				t.Errorf("values printed %q, want %q", got, tc.vals)
			}
		})
	}
}

func TestEmitRangeLoopAscendingInclusiveWidthMatrixCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// loop lo..=hi with lo=0, hi=3 visits 0,1,2,3.
	for _, tc := range []struct {
		w    string
		vals string
	}{
		{"i8", "0123"}, {"i16", "0123"}, {"i32", "0123"}, {"i64", "0123"}, {"int", "0123"},
		{"u8", "0123"}, {"u16", "0123"}, {"u32", "0123"}, {"u64", "0123"}, {"uint", "0123"},
	} {
		tc := tc
		t.Run(tc.w, func(t *testing.T) {
			t.Parallel()
			src := "fn main() i32 { var lo " + tc.w + " = 0; var hi " + tc.w + " = 3; var count i32 = 0; loop lo..=hi : i { print i; count = count + 1; } if count == 4 { return 42; } return 1; }"
			out := emitAndRunCaptureBounded(t, src, false, 42, false)
			if got := strings.ReplaceAll(out, "\n", ""); got != tc.vals {
				t.Errorf("values printed %q, want %q", got, tc.vals)
			}
		})
	}
}

func TestEmitRangeLoopDescendingExclusiveWidthMatrixCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// loop hi..lo with lo=0, hi=5 visits 5,4,3,2,1.
	for _, tc := range []struct {
		w    string
		vals string
	}{
		{"i8", "54321"}, {"i16", "54321"}, {"i32", "54321"}, {"i64", "54321"}, {"int", "54321"},
		{"u8", "54321"}, {"u16", "54321"}, {"u32", "54321"}, {"u64", "54321"}, {"uint", "54321"},
	} {
		tc := tc
		t.Run(tc.w, func(t *testing.T) {
			t.Parallel()
			src := "fn main() i32 { var lo " + tc.w + " = 0; var hi " + tc.w + " = 5; var count i32 = 0; loop hi..lo : i { print i; count = count + 1; if count > 30 { return 1; } } if count == 5 { return 42; } return 1; }"
			out := emitAndRunCaptureBounded(t, src, false, 42, false)
			if got := strings.ReplaceAll(out, "\n", ""); got != tc.vals {
				t.Errorf("values printed %q, want %q", got, tc.vals)
			}
		})
	}
}

func TestEmitRangeLoopDescendingInclusiveWidthMatrixCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// loop hi..=lo with lo=0, hi=5 visits 5,4,3,2,1,0. The final iteration's
	// value IS the end bound, so an unsigned iterator must not be decremented
	// one past 0 — this is the done-gate regression matrix: every unsigned
	// width used to wrap 0 -> UINT_MAX and loop forever.
	for _, tc := range []struct {
		w    string
		vals string
	}{
		{"i8", "543210"}, {"i16", "543210"}, {"i32", "543210"}, {"i64", "543210"}, {"int", "543210"},
		{"u8", "543210"}, {"u16", "543210"}, {"u32", "543210"}, {"u64", "543210"}, {"uint", "543210"},
	} {
		tc := tc
		t.Run(tc.w, func(t *testing.T) {
			t.Parallel()
			src := "fn main() i32 { var lo " + tc.w + " = 0; var hi " + tc.w + " = 5; var count i32 = 0; loop hi..=lo : i { print i; count = count + 1; if count > 30 { return 1; } } if count == 6 { return 42; } return 1; }"
			out := emitAndRunCaptureBounded(t, src, false, 42, false)
			if got := strings.ReplaceAll(out, "\n", ""); got != tc.vals {
				t.Errorf("values printed %q, want %q", got, tc.vals)
			}
		})
	}
}

// TestEmitRangeLoopAscendingInclusiveWrapBoundaryCompilesAndRuns is the other
// wrap direction: an ascending inclusive range ending at the type's max. The
// last iteration is the max value, and an unsigned/signed iterator one past it
// wraps to 0 / the min and re-enters the range under the old lowering (u8
// 250..=255 wrapped 255 -> 0 and looped; i8 125..=127 wrapped 127 -> -128).
func TestEmitRangeLoopAscendingInclusiveWrapBoundaryCompilesAndRuns(t *testing.T) {
	for _, tc := range []struct {
		name string
		src  string
		vals string
	}{
		{"u8 past max", "fn main() i32 { var lo u8 = 250; var hi u8 = 255; var count i32 = 0; loop lo..=hi : i { print i; count = count + 1; if count > 30 { return 1; } } if count == 6 { return 42; } return 1; }", "250251252253254255"},
		{"i8 past max", "fn main() i32 { var lo i8 = 125; var hi i8 = 127; var count i32 = 0; loop lo..=hi : i { print i; count = count + 1; if count > 30 { return 1; } } if count == 3 { return 42; } return 1; }", "125126127"},
		{"u8 past max exclusive stays correct", "fn main() i32 { var lo u8 = 250; var hi u8 = 255; var count i32 = 0; loop lo..hi : i { print i; count = count + 1; } if count == 5 { return 42; } return 1; }", "250251252253254"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCaptureBounded(t, tc.src, false, 42, false)
			if got := strings.ReplaceAll(out, "\n", ""); got != tc.vals {
				t.Errorf("values printed %q, want %q", got, tc.vals)
			}
		})
	}
}

// TestEmitRangeLoopContinueOnFinalIterationCompilesAndRuns pins the continue
// contract of the done-gate: a body continue jumps to the C increment clause,
// where the done test lives, so a continue firing on the final inclusive
// iteration must still terminate instead of skipping a trailing break and
// wrapping the unsigned iterator into an infinite loop.
func TestEmitRangeLoopContinueOnFinalIterationCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// loop 5..=0 u8, continue on the first iteration (i == 5): body still runs
	// for 0,1,2,3,4 and terminates cleanly at 0.
	emitAndRunBounded(t, "fn main() i32 { var lo u8 = 0; var hi u8 = 5; var count i32 = 0; loop hi..=lo : i { if i == 5 { continue; } count = count + 1; } if count == 5 { return 42; } return 1; }", false, 42, false)
	// continue on the FINAL iteration itself (i == 0): the done test still runs
	// after the continue via the increment clause, so the loop terminates.
	emitAndRunBounded(t, "fn main() i32 { var lo u8 = 0; var hi u8 = 5; var count i32 = 0; loop hi..=lo : i { if i == 0 { continue; } count = count + 1; } if count == 5 { return 42; } return 1; }", false, 42, false)
}

// TestEmitRangeLoopBreakOnFinalIterationCompilesAndRuns pins break on the final
// inclusive iteration: the user break exits before the done test, and the loop
// must still have visited every value up to the break point.
func TestEmitRangeLoopBreakOnFinalIterationCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRunBounded(t, "fn main() i32 { var lo u8 = 0; var hi u8 = 5; var count i32 = 0; loop hi..=lo : i { count = count + 1; if i == 1 { break; } } if count == 5 { return 42; } return 1; }", false, 42, false)
}

// TestEmitRangeLoopBoundSourceShapesCompilesAndRuns probes that range bounds
// accept every ordinary integer expression shape the checker types — struct
// field reads, array-element reads, and helper call results — not just
// literals and locals, and that the iterator is usable as a call argument and
// an array index.
func TestEmitRangeLoopBoundSourceShapesCompilesAndRuns(t *testing.T) {
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"struct field read bounds", "type P = struct { a int; b int; };\nfn mk() P => P.{ a = 1, b = 3 };\nfn main() int { var p P = mk(); var sum int = 0; loop p.a..=p.b : i { sum = sum + i; } if sum == 6 { return 42; } return 1; }"},
		{"array element read bounds", "fn main() int { var a [3]int = [1, 3, 5]; var sum int = 0; loop a[0]..=a[1] : i { sum = sum + i; } if sum == 6 { return 42; } return 1; }"},
		{"call result bounds", "fn lo() int { return 1; } fn hi() int { return 3; } fn main() int { var sum int = 0; loop lo()..=hi() : i { sum = sum + i; } if sum == 6 { return 42; } return 1; }"},
		{"iterator as call argument", "fn triple(x int) int { return x * 3; } fn main() int { var sum int = 0; loop 1..3 : i { sum = sum + triple(i); } if sum == 9 { return 42; } return 1; }"},
		{"iterator as array index", "fn main() int { var a [4]int = [10, 20, 30, 40]; var sum int = 0; loop 0..4 : i { sum = sum + a[i]; } if sum == 100 { return 42; } return 1; }"},
		{"tail position", "fn count() int { var sum int = 0; loop 0..3 : i { sum = sum + i; } return sum; } fn main() int { if count() == 3 { return 42; } return 1; }"},
		{"single iteration exclusive", "fn main() i32 { var sum i32 = 0; loop 2..3 : i { sum = sum + i; } if sum == 2 { return 42; } return 1; }"},
		{"single iteration inclusive", "fn main() i32 { var sum i32 = 0; loop 2..=2 : i { sum = sum + i; } if sum == 2 { return 42; } return 1; }"},
		{"nested narrow width", "fn main() i32 { var lo u8 = 0; var hi u8 = 3; var count i32 = 0; loop lo..hi : i { loop lo..i : j { count = count + 1; } } if count == 3 { return 42; } return 1; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRunBounded(t, tc.src, false, 42, false)
		})
	}
}
