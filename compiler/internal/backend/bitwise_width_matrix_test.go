package backend

import (
	"bytes"
	"fmt"
	"strings"
	"testing"
)

// Bitwise & | ^ width-matrix tests (Phase 3 #21). Bitwise AND/OR/XOR are NOT
// checked-overflow operations: they lower to the plain C operator at every
// width (no runtime helper at any width, including the narrow fixed-width
// integers the checked-arithmetic family rejects). The tracker row's
// "backend coverage is width-specific" pointed at a possible leftover width
// gate; a full empirical sweep of the plain-binary-expression matrix (3
// operators x i8/i16/i32/i64/int/u8/u16/u32/u64/uint) found exactly one real
// gap: uint's `& | ^` rejected at Emit ("unsupported uint expression node
// BinaryValue") because buildUintExpr — the dedicated uint grammar — had a
// CheckedArithmetic case (the checker builds uint + - * / % as
// CheckedArithmetic) but no BinaryValue case, even though the checker builds
// & | ^ on integral operands as a BinaryValue and uint has no checked helper
// for ANY operator, so the plain C operator is the whole lowering — the same
// reason buildExpr's BinaryValue case already handles every other width.
// Fixed by routing uint's BinaryValue through the same plain-C-operator
// lowering. The `&= |= ^=` compound form does not exist in this language
// (the lexer/parser have no such tokens, in either the v2 or legacy C
// compiler) and bool is not a valid & | ^ operand (the checker's integral
// capability rejects it), so neither is in scope. This file pins the full
// plain-binary matrix: compile+run for every (op, width) pair.

func TestBitwiseWidthMatrixCompileAndRun(t *testing.T) {
	t.Parallel()
	// (operator, width) pairs that must COMPILE AND RUN with the bitwise-
	// correct result. Each source is the probe shape `var r <W> = a <op> b;`
	// with two same-width locals in an int-entry main, so the BinaryValue
	// node really emits at <W>. With a = 5 (0b101) and b = 6 (0b110):
	// 5 & 6 = 4, 5 | 6 = 7, 5 ^ 6 = 3 — three distinct results that pin
	// each operator to its own lowering. The operands and results fit every
	// width (all positive, all below 8), so a wrong-width or wrong-operator
	// lowering changes the run's exit code and fails the test. uint is the
	// pair that previously rejected at Emit ("unsupported uint expression
	// node BinaryValue") before the Phase 3 #21 fix.
	for _, tc := range []struct {
		name  string
		width string
		op    string
		want  int
	}{
		{"and i8", "i8", "&", 4},
		{"or i8", "i8", "|", 7},
		{"xor i8", "i8", "^", 3},
		{"and i16", "i16", "&", 4},
		{"or i16", "i16", "|", 7},
		{"xor i16", "i16", "^", 3},
		{"and i32", "i32", "&", 4},
		{"or i32", "i32", "|", 7},
		{"xor i32", "i32", "^", 3},
		{"and i64", "i64", "&", 4},
		{"or i64", "i64", "|", 7},
		{"xor i64", "i64", "^", 3},
		{"and int", "int", "&", 4},
		{"or int", "int", "|", 7},
		{"xor int", "int", "^", 3},
		{"and u8", "u8", "&", 4},
		{"or u8", "u8", "|", 7},
		{"xor u8", "u8", "^", 3},
		{"and u16", "u16", "&", 4},
		{"or u16", "u16", "|", 7},
		{"xor u16", "u16", "^", 3},
		{"and u32", "u32", "&", 4},
		{"or u32", "u32", "|", 7},
		{"xor u32", "u32", "^", 3},
		{"and u64", "u64", "&", 4},
		{"or u64", "u64", "|", 7},
		{"xor u64", "u64", "^", 3},
		{"and uint", "uint", "&", 4},
		{"or uint", "uint", "|", 7},
		{"xor uint", "uint", "^", 3},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			src := fmt.Sprintf("fn main() int { var a %s = 5; var b %s = 6; var r %s = a %s b; return r as int; }", tc.width, tc.width, tc.width, tc.op)
			emitAndRun(t, src, false, tc.want, false)
		})
	}
}

func TestUintBitwiseWritesPlainCOperator(t *testing.T) {
	t.Parallel()
	// Assert the emitted C for a uint bitwise expression: the combined value
	// must be the plain C operator, never a checked runtime helper call —
	// uint has no checked helper (checkedSuffix maps no width to uint), and
	// the plain operator is the whole lowering, exactly as a uint `a + b`
	// uses the plain C + in buildUintExpr's CheckedArithmetic case. The
	// bitwise shape is what previously rejected ("unsupported uint
	// expression node BinaryValue") before the Phase 3 #21 fix. The right
	// operand is a literal (`a & 6`), so the emitted text deterministically
	// splices the literal's uint spelling next to the operator — the same
	// assertion shape TestUintCompoundModuloWritesPlainCOperator uses.
	for _, tc := range []struct {
		op   string
		want string
	}{
		{"&", "& 6u)"},
		{"|", "| 6u)"},
		{"^", "^ 6u)"},
	} {
		t.Run(tc.op, func(t *testing.T) {
			t.Parallel()
			src := fmt.Sprintf("fn main() int { var a uint = 5; var r uint = a %s 6; return r as int; }", tc.op)
			unit, snapshot, entryID, sources := buildFixture(t, src, "main", false)
			var buf bytes.Buffer
			if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
				t.Fatalf("Emit failed: %v", err)
			}
			out := buf.String()
			if !strings.Contains(out, tc.want) {
				t.Errorf("emitted C missing the plain uint %s operator lowering:\n%s", tc.op, out)
			}
			if strings.Contains(out, "pebble_rt_checked_") {
				t.Errorf("emitted C calls a checked helper for uint %s, want plain C %s:\n%s", tc.op, tc.op, out)
			}
		})
	}
}
