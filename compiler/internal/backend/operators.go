package backend

import (
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func arithmeticOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.Plus:
		return "+", true
	case syntax.Minus:
		return "-", true
	case syntax.Star:
		return "*", true
	case syntax.Slash:
		return "/", true
	case syntax.Percent:
		return "%", true
	default:
		return "", false
	}
}

// comparisonOperator maps the six comparison token kinds this backend lowers
// to their plain C spellings. These map 1:1 to C syntax — no runtime helper is
// involved, since comparing two integer values (the entry's width, or the
// int-typed literal case) cannot overflow. Any other
// operator is deliberately not mapped and rejected by the caller.
func comparisonOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.Less:
		return "<", true
	case syntax.LessEqual:
		return "<=", true
	case syntax.Greater:
		return ">", true
	case syntax.GreaterEqual:
		return ">=", true
	case syntax.Equal:
		return "==", true
	case syntax.NotEqual:
		return "!=", true
	default:
		return "", false
	}
}

// shortCircuitOperator maps the two logical-combination token kinds a
// ShortCircuitValue may carry to their plain C spellings. Both C and Pebble
// && and || short-circuit their right operand, and both sides of the operator
// are side-effect-free in this backend's grammar, so the plain C operator is a
// direct, correct lowering. Any other operator is deliberately not mapped and
// rejected by the caller.
func shortCircuitOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.LogicalAnd:
		return "&&", true
	case syntax.LogicalOr:
		return "||", true
	default:
		return "", false
	}
}

// bitwiseOperator maps the unchecked integral operators whose C semantics are
// defined for every bit pattern. Shifts are deliberately excluded: their
// CheckedShift nodes require checked runtime semantics and are not plain C
// operators in this backend.
func bitwiseOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.Ampersand:
		return "&", true
	case syntax.Pipe:
		return "|", true
	case syntax.Caret:
		return "^", true
	default:
		return "", false
	}
}

// checkedArithmeticHelper maps the +, -, *, /, % operators a CheckedArithmetic
// node may carry to the runtime helper that implements their checked semantics,
// at the entry's resolved width (width's checkedSuffix picks the _i32 or _i64
// function-name suffix).
// Division and modulo map to pebble_rt_checked_div_i32 / pebble_rt_checked_mod_i32
// (or their _i64 twins), which handle both the divide-by-zero fault (in every
// mode) and the one
// division overflow input, INT32_MIN / -1 (INT64_MIN / -1 at the wider
// width). At u64 the +, -, * family exists (pebble_rt_checked_add/sub/mul_u64,
// this slice), but the / and % family has no u64 twin yet, so a u64 division
// or modulo is cleanly rejected rather than emitted as a call to a nonexistent
// pebble_rt_checked_div_u64/mod_u64. Any other operator (bitwise, etc.)
// is deliberately not mapped and rejected by the caller.
func checkedArithmeticHelper(op syntax.TokenKind, width types.BuiltinKind) (string, bool) {
	var base string
	switch op {
	case syntax.Plus:
		base = "pebble_rt_checked_add"
	case syntax.Minus:
		base = "pebble_rt_checked_sub"
	case syntax.Star:
		base = "pebble_rt_checked_mul"
	case syntax.Slash:
		base = "pebble_rt_checked_div"
	case syntax.Percent:
		base = "pebble_rt_checked_mod"
	default:
		return "", false
	}
	if width == types.U64 && (op == syntax.Slash || op == syntax.Percent) {
		return "", false
	}
	return base + "_" + checkedSuffix(width), true
}

func checkedShiftHelper(op syntax.TokenKind, width types.BuiltinKind) (string, bool) {
	var base string
	switch op {
	case syntax.ShiftLeft:
		base = "pebble_rt_checked_shl"
	case syntax.ShiftRight:
		base = "pebble_rt_checked_shr"
	default:
		return "", false
	}
	// Every width with a runtime shift-helper pair is admitted: the original
	// i32/i64 pair plus every narrower fixed-width integer (u8, u16, i8, i16,
	// u32) added when shifts were widened. A u64 (or uint) shift stays a clean
	// rejection here rather than being emitted as a call to a nonexistent
	// pebble_rt_checked_shl_u64/shr_u64 (checkedSuffix admits u64 for the +,
	// -, * arithmetic family this slice adds, but the shift family has no u64
	// twin yet).
	suffix := checkedShiftSuffix(width)
	if suffix == "" {
		return "", false
	}
	return base + "_" + suffix, true
}

// checkedShiftSuffix returns the pebble_rt_checked_shl/shr_* function-name
// suffix for the given width: "i32" for an int or i32 entry, "i64" for an
// i64 entry, and the width's own name for every narrower fixed-width integer
// (u8, u16, i8, i16, u32 — each of which has its own runtime shift-helper
// pair). It is the shift-specific twin of checkedSuffix, which deliberately
// stays narrow: the OTHER checked helper families (arithmetic, index, slice
// start, float-to-integer) admit only the i32/i64/u64 widths, so widening
// checkedSuffix globally would emit calls to nonexistent helpers for them
// (e.g. pebble_rt_checked_add_u8 does not exist). Any width without a
// runtime shift helper — u64 and uint, whose C representation is uint64_t —
// yields "", a clean rejection for the caller.
func checkedShiftSuffix(width types.BuiltinKind) string {
	switch width {
	case types.Int, types.I32:
		return "i32"
	case types.I64:
		return "i64"
	case types.U8:
		return "u8"
	case types.U16:
		return "u16"
	case types.U32:
		return "u32"
	case types.I8:
		return "i8"
	case types.I16:
		return "i16"
	}
	return ""
}

// isCompatibleIntegerWidth reports whether id resolves to an integer builtin
// (ANY fixed-width integer, not just the abstract `int` one) whose C
// representation shares width's own: cType(builtin) == cType(width). It is the
// symmetric twin of the isAbstractInt leniency buildExpr's width gate applies
// for the reverse direction: isAbstractInt lets an abstract-`int`-typed VALUE
// be emitted where a concrete width is requested, and this predicate lets a
// CONCRETE-width-typed value/parameter (typically the substitution a generic
// specialization performed — `clamp[T]` instantiated at `i32` has i32-typed
// parameters) be accepted where the surrounding context's own width — an
// `int`-declared entry's resolved `int` builtin — is expected, since `int` and
// `i32` share the int32_t representation. The gate deliberately does NOT accept
// a genuinely mismatched fixed width (an i64 value in an int/i32 context), which
// is the mismatch it exists to catch. isWidth itself stays exact-match so every
// other call site keeps its existing meaning.
func isCompatibleIntegerWidth(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) bool {
	if snapshot == nil || cType(width) == "" {
		return false
	}
	builtin, ok := resolvedBuiltin(snapshot, id)
	return ok && cType(builtin) != "" && cType(builtin) == cType(width)
}

// optionalUnwrapSuffix returns the pebble_rt_checked_unwrap_* helper suffix
// for an optional payload of the given type: "i32" for an int/i32 payload,
// "i64" for an i64 payload, "u64" for a uint or u64 payload (both carry the
// C type uint64_t, so one runtime helper reads both back at their true
// width), and "bool" for a bool payload. Any other payload type (a narrower
// fixed-width integer without a runtime unwrap helper yet, a char, str,
// tuple, or struct) yields "", a clean rejection for the caller. The helper
// must be selected from the PAYLOAD's own type rather than the ambient entry
// width: a uint payload's .value field is uint64_t, which only
// pebble_rt_checked_unwrap_u64 reads back at its true width, not the
// entry-width helper.
func optionalUnwrapSuffix(snapshot *types.Snapshot, id types.TypeID) string {
	if isBool(snapshot, id) {
		return "bool"
	}
	if isPointer(snapshot, id) {
		// A pointer payload's .value field is `<pointee> *` and the unwrap is
		// only the has_value check (a null payload value is a perfectly valid
		// unwrap result, so there is no dereference at this point) — one
		// void *-based helper serves every pointee.
		return "ptr"
	}
	payloadWidth, ok := resolvedBuiltin(snapshot, id)
	if !ok {
		return ""
	}
	switch payloadWidth {
	case types.Int, types.I32:
		return "i32"
	case types.I64:
		return "i64"
	case types.Uint, types.U64:
		return "u64"
	}
	return ""
}

// checkedSuffix returns the pebble_rt_checked_* function-name suffix for the
// given width: "i32" for an int or i32 entry, "i64" for an i64 entry, "u64"
// for a u64 entry. It is exactly the type's name for the fixed-width entries,
// but named for what it selects — the width-specific runtime helper family.
func checkedSuffix(width types.BuiltinKind) string {
	switch width {
	case types.Int:
		return "i32"
	case types.I32:
		return "i32"
	case types.I64:
		return "i64"
	case types.U64:
		return "u64"
	}
	return ""
}

func childFloatSuffix(width types.BuiltinKind) string {
	switch width {
	case types.F32:
		return "f32"
	case types.F64:
		return "f64"
	}
	return ""
}
