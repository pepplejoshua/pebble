package backend

import (
	"bytes"
	"fmt"
	"strings"
	"testing"
)

// Arithmetic width-matrix tests (Phase 3 #20). These pin the checked-arithmetic
// + - * / % helper-width matrix documented in proposal 14's "Integer runtime
// coverage matrix": every checker-accepted integer width must either emit a
// real, correct checked-arithmetic lowering or reject CLEANLY at Emit — never
// emit a call to a nonexistent helper that only fails later at cc. The plain
// binary-expression matrix covers checked helpers for all supported widths and
// operators. The compound-assignment form has the same coverage, including the
// %= on uint shape that previously rejected even though the plain `a % b` form
// lowered fine.

func TestArithmeticWidthMatrixCompileAndRun(t *testing.T) {
	t.Parallel()
	// (operator, width) pairs that must COMPILE AND RUN with the arithmetically
	// correct result. Each source is the probe shape `var r <W> = a <op> b;`
	// with two same-width locals in an int-entry main, so the CheckedArithmetic
	// node really emits at <W>. 5 op 2 yields 7/3/10/2/1 for + - * / % at every
	// width.
	for _, tc := range []struct {
		name  string
		width string
		op    string
		want  int
	}{
		{"add int", "int", "+", 7},
		{"sub int", "int", "-", 3},
		{"mul int", "int", "*", 10},
		{"div int", "int", "/", 2},
		{"mod int", "int", "%", 1},
		{"add i32", "i32", "+", 7},
		{"sub i32", "i32", "-", 3},
		{"mul i32", "i32", "*", 10},
		{"div i32", "i32", "/", 2},
		{"mod i32", "i32", "%", 1},
		{"add i64", "i64", "+", 7},
		{"sub i64", "i64", "-", 3},
		{"mul i64", "i64", "*", 10},
		{"div i64", "i64", "/", 2},
		{"mod i64", "i64", "%", 1},
		{"add u64", "u64", "+", 7},
		{"sub u64", "u64", "-", 3},
		{"mul u64", "u64", "*", 10},
		{"div u64", "u64", "/", 2},
		{"mod u64", "u64", "%", 1},
		{"add uint", "uint", "+", 7},
		{"sub uint", "uint", "-", 3},
		{"mul uint", "uint", "*", 10},
		{"div uint", "uint", "/", 2},
		{"mod uint", "uint", "%", 1},
		{"add i8", "i8", "+", 7},
		{"sub i8", "i8", "-", 3},
		{"mul i8", "i8", "*", 10},
		{"div i8", "i8", "/", 2},
		{"mod i8", "i8", "%", 1},
		{"add i16", "i16", "+", 7},
		{"sub i16", "i16", "-", 3},
		{"mul i16", "i16", "*", 10},
		{"div i16", "i16", "/", 2},
		{"mod i16", "i16", "%", 1},
		{"add u8", "u8", "+", 7},
		{"sub u8", "u8", "-", 3},
		{"mul u8", "u8", "*", 10},
		{"div u8", "u8", "/", 2},
		{"mod u8", "u8", "%", 1},
		{"add u16", "u16", "+", 7},
		{"sub u16", "u16", "-", 3},
		{"mul u16", "u16", "*", 10},
		{"div u16", "u16", "/", 2},
		{"mod u16", "u16", "%", 1},
		{"add u32", "u32", "+", 7},
		{"sub u32", "u32", "-", 3},
		{"mul u32", "u32", "*", 10},
		{"div u32", "u32", "/", 2},
		{"mod u32", "u32", "%", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			src := fmt.Sprintf("fn main() int { var a %s = 5; var b %s = 2; var r %s = a %s b; return r as int; }", tc.width, tc.width, tc.width, tc.op)
			emitAndRun(t, src, false, tc.want, false)
		})
	}
}

func TestArithmeticWidthMatrixOverflowAborts(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"i8 add", "fn main() int { var a i8 = 127; var b i8 = 1; var r i8 = a + b; return r as int; }"},
		{"i8 sub", "fn main() int { var a i8 = -128; var b i8 = 1; var r i8 = a - b; return r as int; }"},
		{"i8 mul", "fn main() int { var a i8 = 64; var b i8 = 2; var r i8 = a * b; return r as int; }"},
		{"i16 add", "fn main() int { var a i16 = 32767; var b i16 = 1; var r i16 = a + b; return r as int; }"},
		{"i16 sub", "fn main() int { var a i16 = -32768; var b i16 = 1; var r i16 = a - b; return r as int; }"},
		{"i16 mul", "fn main() int { var a i16 = 256; var b i16 = 128; var r i16 = a * b; return r as int; }"},
		{"u8 add", "fn main() int { var a u8 = 255; var b u8 = 1; var r u8 = a + b; return r as int; }"},
		{"u8 sub", "fn main() int { var a u8 = 0; var b u8 = 1; var r u8 = a - b; return r as int; }"},
		{"u8 mul", "fn main() int { var a u8 = 16; var b u8 = 16; var r u8 = a * b; return r as int; }"},
		{"u16 add", "fn main() int { var a u16 = 65535; var b u16 = 1; var r u16 = a + b; return r as int; }"},
		{"u16 sub", "fn main() int { var a u16 = 0; var b u16 = 1; var r u16 = a - b; return r as int; }"},
		{"u16 mul", "fn main() int { var a u16 = 256; var b u16 = 256; var r u16 = a * b; return r as int; }"},
		{"u32 add", "fn main() int { var a u32 = 4294967295; var b u32 = 1; var r u32 = a + b; return r as int; }"},
		{"u32 sub", "fn main() int { var a u32 = 0; var b u32 = 1; var r u32 = a - b; return r as int; }"},
		{"u32 mul", "fn main() int { var a u32 = 65536; var b u32 = 65536; var r u32 = a * b; return r as int; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, 0, true)
		})
	}
}

func TestUintCompoundAssignmentWidthMatrixCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// uint compound assignment lowers to plain C arithmetic at uint's own
	// uint64_t width, exactly like a plain uint `a <op> b` expression. All five
	// operators must therefore work on a uint local; %= on uint is the shape
	// that previously rejected ("% is integral-only") even though the plain
	// form and every other compound operator already lowered fine. 5 op 2
	// yields 7/3/10/2/1 for += -= *= /= %=.
	for _, tc := range []struct {
		name string
		op   string
		want int
	}{
		{"add-assign", "+=", 7},
		{"sub-assign", "-=", 3},
		{"mul-assign", "*=", 10},
		{"div-assign", "/=", 2},
		{"mod-assign", "%=", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			src := fmt.Sprintf("fn main() int { var a uint = 5; a %s 2; return a as int; }", tc.op)
			emitAndRun(t, src, false, tc.want, false)
		})
	}
}

func TestUintCompoundModuloFieldPlaceCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The uint %= fix's struct-field shape: the std/hmap.peb self.len-style
	// compound assignment on a uint-typed field of a struct reached through a
	// pointer helper must also lower %= to plain C `%` — the same
	// buildCompoundUintCore path a uint local's %= uses (the field's resolved
	// element type is uint). 5 %= 2 = 1.
	emitAndRun(t, "type S = struct { n uint; }; fn f(s *S) void { s.n %= 2; } fn main() int { var s S = S.{ n = 5 }; f(&s); return s.n as int; }", false, 1, false)
}

func TestUintCompoundModuloWritesPlainCOperator(t *testing.T) {
	t.Parallel()
	// Assert the emitted C for uint %=: the combined value must be the plain C
	// modulo expression on the uint local, never a checked runtime helper call —
	// uint has no checked helper (checkedSuffix maps no width to uint), and the
	// plain operator is the whole lowering, exactly like uint += etc.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var a uint = 5; a %= 2; return a as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "% 2u)") {
		t.Errorf("emitted C missing the plain uint %%-operator lowering:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_checked_mod_") {
		t.Errorf("emitted C calls a checked-modulo helper for uint, want plain C %%:\n%s", out)
	}
}
