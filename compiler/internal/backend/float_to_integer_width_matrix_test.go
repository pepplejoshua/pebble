package backend

import (
	"bytes"
	"fmt"
	"strings"
	"testing"
)

// Float-to-integer width-matrix tests (Phase 3 #26). An explicit float-to-
// integer cast (`f as i32`) is a CHECKED runtime conversion: NaN and
// out-of-range values abort in SAFE mode rather than invoking C's undefined
// out-of-range float-to-int conversion. The runtime originally implemented
// only the i32/i64 destination pair (shared with checkedSuffix's other
// deliberately-narrow consumers); every other destination was checker-
// accepted but Emit-rejected — i8/i16/u8/u16/u32/u64 with the misleading
// "non-integer destination type" (floatToIntSuffix returns "" for them) and
// uint with "unsupported uint expression node FloatToInteger" (buildUintExpr
// had no case at all). Fixed by adding pebble_rt_checked_{f32,f64}_to_{i8,i16,
// u8,u16,u32,u64} to the runtime (uint shares the u64 helpers — both carry the
// C type uint64_t) and a dedicated floatToIntSuffix selector (mirroring
// checkedShiftSuffix/checkedNegSuffix) so the float-conversion family widens
// independently of checkedSuffix's other consumers. This file pins the full
// (source-width, destination-width) matrix and the SAFE-mode rejection cases.

func TestFloatToIntegerWidthMatrixCompileAndRun(t *testing.T) {
	t.Parallel()
	// Every (source-width, destination-width) pair must COMPILE AND RUN with
	// the arithmetically-correct truncated result. Each source is the probe
	// shape `let f <SW> = 3.5; let r <DW> = f as <DW>; return r as int;` in
	// an int-entry main, so the FloatToInteger node really emits at <DW> from
	// a float source at <SW>. 3.5 truncates to 3 at every integer width, so a
	// wrong-width or wrong-source lowering changes the run's exit code and
	// fails the test. i32/i64/int already worked before the Phase 3 #26 fix;
	// i8/i16/u8/u16/u32/u64 and uint are the pairs that previously rejected.
	for _, sw := range []string{"f32", "f64"} {
		for _, dw := range []string{"i8", "i16", "i32", "i64", "int", "u8", "u16", "u32", "u64", "uint"} {
			t.Run(sw+"-to-"+dw, func(t *testing.T) {
				t.Parallel()
				src := fmt.Sprintf("fn main() int { let f %s = 3.5; let r %s = f as %s; return r as int; }", sw, dw, dw)
				emitAndRun(t, src, false, 3, false)
			})
		}
	}
}

func TestFloatToIntegerOutOfRangeAbortsInSafeMode(t *testing.T) {
	t.Parallel()
	// NaN, +Infinity, -Infinity, and an out-of-range magnitude must abort in
	// SAFE mode at every newly-added destination width, never silently
	// produce garbage via C's undefined conversion. NaN/+Inf/-Inf are
	// computed (0.0/0.0, 1.0/0.0, -1.0/0.0) since the language has no float
	// NaN/Infinity literal.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"f64 nan to i8", "fn main() int { let x f64 = 0.0 / 0.0; return x as i8 as int; }"},
		{"f64 +inf to i16", "fn main() int { let x f64 = 1.0 / 0.0; return x as i16 as int; }"},
		{"f64 -inf to u8", "fn main() int { let x f64 = -1.0 / 0.0; return x as u8 as int; }"},
		{"f64 oob to i8", "fn main() int { let x f64 = 128.0; return x as i8 as int; }"},
		{"f64 oob neg to i8", "fn main() int { let x f64 = -129.0; return x as i8 as int; }"},
		{"f64 oob to i16", "fn main() int { let x f64 = 32768.0; return x as i16 as int; }"},
		{"f64 oob neg to i16", "fn main() int { let x f64 = -32769.0; return x as i16 as int; }"},
		{"f64 oob to u8", "fn main() int { let x f64 = 256.0; return x as u8 as int; }"},
		{"f64 oob neg to u8", "fn main() int { let x f64 = -1.0; return x as u8 as int; }"},
		{"f64 oob to u16", "fn main() int { let x f64 = 65536.0; return x as u16 as int; }"},
		{"f64 oob neg to u16", "fn main() int { let x f64 = -1.0; return x as u16 as int; }"},
		{"f64 oob to u32", "fn main() int { let x f64 = 4294967296.0; return x as u32 as int; }"},
		{"f64 oob neg to u32", "fn main() int { let x f64 = -1.0; return x as u32 as int; }"},
		{"f64 oob to u64", "fn main() int { let x f64 = 18446744073709551616.0; return x as u64 as int; }"},
		{"f64 oob neg to u64", "fn main() int { let x f64 = -1.0; return x as u64 as int; }"},
		{"f64 oob neg to uint", "fn main() int { let x f64 = -1.0; return x as uint as int; }"},
		{"f32 oob to i16", "fn main() int { let x f32 = 32768.0; return x as i16 as int; }"},
		{"f32 oob to u32", "fn main() int { let x f32 = 4294967296.0; return x as u32 as int; }"},
		{"f32 oob neg to uint", "fn main() int { let x f32 = -1.0; return x as uint as int; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, 0, true)
		})
	}
}

func TestFloatToIntegerBoundaryInRange(t *testing.T) {
	t.Parallel()
	// Values exactly at each width's inclusive edge must NOT abort and must
	// convert to the exact value — the check's lower bound is inclusive and
	// its upper bound is the width's own exclusive power of two.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"f64 127 to i8", "fn main() int { let x f64 = 127.0; if x as i8 == 127 { return 42; } return 1; }"},
		{"f64 -128 to i8", "fn main() int { let x f64 = -128.0; if x as i8 == -128 { return 42; } return 1; }"},
		{"f64 255 to u8", "fn main() int { let x f64 = 255.0; if x as u8 == 255 { return 42; } return 1; }"},
		{"f64 65535 to u16", "fn main() int { let x f64 = 65535.0; if x as u16 == 65535 { return 42; } return 1; }"},
		{"f64 2147483647 to u32", "fn main() int { let x f64 = 2147483647.0; if x as u32 == 2147483647 { return 42; } return 1; }"},
		{"f64 u64 max double", "fn main() int { let x f64 = 18446744073709549568.0; if x as u64 == 18446744073709549568 { return 42; } return 1; }"},
		{"f32 127 to i8", "fn main() int { let x f32 = 127.0; if x as i8 == 127 { return 42; } return 1; }"},
		{"f32 255 to u8", "fn main() int { let x f32 = 255.0; if x as u8 == 255 { return 42; } return 1; }"},
		{"f32 u64 max float", "fn main() int { let x f32 = 18446742974197923840.0; if x as u64 == 18446742974197923840 { return 42; } return 1; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, 42, false)
		})
	}
}

func TestFloatToIntegerEmittedHelperName(t *testing.T) {
	t.Parallel()
	// Emitted C must call the width's own checked helper, never promote to the
	// i32/i64 pair (and uint must reach the u64 helper, not fail the uint
	// grammar). 3.5 as 3 for each pair.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"f64 to i8", "fn main() int { let f f64 = 3.5; let r i8 = f as i8; return r as int; }", "pebble_rt_checked_f64_to_i8("},
		{"f32 to u16", "fn main() int { let f f32 = 3.5; let r u16 = f as u16; return r as int; }", "pebble_rt_checked_f32_to_u16("},
		{"f64 to u32", "fn main() int { let f f64 = 3.5; let r u32 = f as u32; return r as int; }", "pebble_rt_checked_f64_to_u32("},
		{"f32 to u64", "fn main() int { let f f32 = 3.5; let r u64 = f as u64; return r as int; }", "pebble_rt_checked_f32_to_u64("},
		{"f64 to uint", "fn main() int { let f f64 = 3.5; let r uint = f as uint; return r as int; }", "pebble_rt_checked_f64_to_u64("},
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
				t.Fatalf("emitted C missing %s:\n%s", tc.want, out)
			}
			for _, narrower := range []string{"_to_i32(", "_to_i64("} {
				if strings.Contains(out, "pebble_rt_checked_f"+narrower) {
					t.Fatalf("emitted C promoted the cast to a %s helper:\n%s", narrower, out)
				}
			}
			compileAndRun(t, buf.Bytes(), 3, false)
		})
	}
}

func TestFloatToIntegerUintPositions(t *testing.T) {
	t.Parallel()
	// uint's FloatToInteger routes through buildUintExpr (the dedicated uint
	// grammar), the Phase 3 #26 buildUintExpr-gap fix. Lock in the other uint
	// value positions beyond a local declaration initializer: a comparison
	// operand (uint goes through buildComparisonOperand's buildUintExpr
	// branch) and a helper-function return value.
	emitAndRun(t, "fn main() int { let v f64 = 3.5; if (v as uint) == 3 { return 42; } return 1; }", false, 42, false)
	emitAndRun(t, "fn f() uint { let x f64 = 3.5; return x as uint; } fn main() int { if f() == 3 { return 42; } return 1; }", false, 42, false)
	emitAndRun(t, "fn main() int { let v f64 = 3.5; let o ?uint = v as uint; if o! == 3 { return 42; } return 1; }", false, 42, false)
}

func TestFloatToIntegerCallArgumentAndField(t *testing.T) {
	t.Parallel()
	// The newly-added widths must work in every FloatToInteger position, not
	// just a local declaration: a call argument, a struct field's
	// construction value, and a store (assignment).
	emitAndRun(t, "fn f(x i8) int { if x == 3 { return 42; } return 1; } fn main() int { let v f64 = 3.5; return f(v as i8); }", false, 42, false)
	emitAndRun(t, "type S = struct { n u16; }; fn main() int { let v f64 = 3.5; let s S = S.{ n = v as u16 }; if s.n == 3 { return 42; } return 1; }", false, 42, false)
	emitAndRun(t, "fn main() int { var r u8 = 0; let v f32 = 3.5; r = v as u8; if r == 3 { return 42; } return 1; }", false, 42, false)
}
