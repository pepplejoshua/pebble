#include "pebble_rt.h"

#include <stdint.h>

/* Checked i32 and i64 arithmetic, the runtime home of the typed IR's
 * CheckedArithmetic/CheckedNegate nodes (spec 06b leaves "release-mode
 * response to phase 10"; this file is that decision). Each public function
 * exists twice, at i32 and i64 width (matching the header's declarations);
 * the i64 variants are the exact same contract at the wider width. Every
 * function also takes a PebbleSourceLoc (see pebble_rt.h), the Pebble
 * source location of the checked expression, threaded into a panic report
 * so it names where in the user's own program the fault happened.
 *
 * Two modes, selected by the same macro the header guards on:
 *
 *   PEBBLE_RT_MODE_SAFE   — overflow calls pebble_rt_panic with
 *                           PEBBLE_PANIC_ARITHMETIC_OVERFLOW.
 *   PEBBLE_RT_MODE_RELEASE — overflow wraps using the operation's
 *                           two's-complement bit pattern, computed via
 *                           unsigned arithmetic. Signed overflow is
 *                           undefined behavior in C, so the release path
 *                           never uses plain signed + - * ; it casts to
 *                           uint32_t/uint64_t, does the operation unsigned
 *                           (defined wraparound), and casts back. loc is
 *                           accepted but unused on this path (no panic is
 *                           ever raised), silenced with (void)loc.
 *
 * The SAFE path detects overflow with the compiler's own overflow-checking
 * builtins (__builtin_*_overflow), which report overflow without the code
 * itself invoking UB while computing it. They work generically across
 * integer widths and are available in both GCC and Clang, which this
 * runtime already assumes (PEBBLE_RT_NORETURN's __GNUC__ || __clang__
 * guard in the header is the precedent).
 *
 * Division and modulo are a separate fault category (see pebble_rt.h):
 * b == 0 panics in every configuration, not just SAFE, because there is no
 * defined quotient for zero in either mode. They are defined once, below,
 * outside the mode gating — only the one overflow input they share,
 * INT32_MIN / -1, is mode-dependent.
 */

#if defined(PEBBLE_RT_MODE_SAFE)

static void pebble_rt_overflow_panic(const char *message, PebbleSourceLoc loc) {
    PebblePanicInfo info;
    info.kind = PEBBLE_PANIC_ARITHMETIC_OVERFLOW;
    info.message = message;
    info.file = loc.file;
    info.line = loc.line;
    info.column = loc.column;
    pebble_rt_panic(&info);
}

int32_t pebble_rt_checked_add_i32(int32_t a, int32_t b, PebbleSourceLoc loc) {
    int32_t result;
    if (__builtin_add_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("i32 addition overflow", loc);
    }
    return result;
}

int32_t pebble_rt_checked_sub_i32(int32_t a, int32_t b, PebbleSourceLoc loc) {
    int32_t result;
    if (__builtin_sub_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("i32 subtraction overflow", loc);
    }
    return result;
}

int32_t pebble_rt_checked_mul_i32(int32_t a, int32_t b, PebbleSourceLoc loc) {
    int32_t result;
    if (__builtin_mul_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("i32 multiplication overflow", loc);
    }
    return result;
}

/* Negation overflows only at the one boundary value: -INT32_MIN is not
 * representable in i32. __builtin_sub_overflow(0, a, ...) reports exactly
 * that case.
 */
int32_t pebble_rt_checked_neg_i32(int32_t a, PebbleSourceLoc loc) {
    int32_t result;
    if (__builtin_sub_overflow(0, a, &result)) {
        pebble_rt_overflow_panic("i32 negation overflow", loc);
    }
    return result;
}

/* The i64 twins of the four helpers above: the exact same contract at the
 * wider width. __builtin_*_overflow works generically across integer widths
 * in both GCC and Clang, so the SAFE path is the identical pattern with
 * int64_t/INT64_MIN substituting for int32_t/INT32_MIN, and the RELEASE
 * path wraps via uint64_t arithmetic the same way the i32 path wraps via
 * uint32_t.
 */
int64_t pebble_rt_checked_add_i64(int64_t a, int64_t b, PebbleSourceLoc loc) {
    int64_t result;
    if (__builtin_add_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("i64 addition overflow", loc);
    }
    return result;
}

int64_t pebble_rt_checked_sub_i64(int64_t a, int64_t b, PebbleSourceLoc loc) {
    int64_t result;
    if (__builtin_sub_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("i64 subtraction overflow", loc);
    }
    return result;
}

int64_t pebble_rt_checked_mul_i64(int64_t a, int64_t b, PebbleSourceLoc loc) {
    int64_t result;
    if (__builtin_mul_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("i64 multiplication overflow", loc);
    }
    return result;
}

/* Negation overflows only at the one boundary value: -INT64_MIN is not
 * representable in i64. __builtin_sub_overflow(0, a, ...) reports exactly
 * that case.
 */
int64_t pebble_rt_checked_neg_i64(int64_t a, PebbleSourceLoc loc) {
    int64_t result;
    if (__builtin_sub_overflow(0, a, &result)) {
        pebble_rt_overflow_panic("i64 negation overflow", loc);
    }
    return result;
}

#else /* PEBBLE_RT_MODE_RELEASE */

int32_t pebble_rt_checked_add_i32(int32_t a, int32_t b, PebbleSourceLoc loc) {
    (void)loc;
    return (int32_t)((uint32_t)a + (uint32_t)b);
}

int32_t pebble_rt_checked_sub_i32(int32_t a, int32_t b, PebbleSourceLoc loc) {
    (void)loc;
    return (int32_t)((uint32_t)a - (uint32_t)b);
}

int32_t pebble_rt_checked_mul_i32(int32_t a, int32_t b, PebbleSourceLoc loc) {
    (void)loc;
    return (int32_t)((uint32_t)a * (uint32_t)b);
}

int32_t pebble_rt_checked_neg_i32(int32_t a, PebbleSourceLoc loc) {
    (void)loc;
    return (int32_t)(0u - (uint32_t)a);
}

int64_t pebble_rt_checked_add_i64(int64_t a, int64_t b, PebbleSourceLoc loc) {
    (void)loc;
    return (int64_t)((uint64_t)a + (uint64_t)b);
}

int64_t pebble_rt_checked_sub_i64(int64_t a, int64_t b, PebbleSourceLoc loc) {
    (void)loc;
    return (int64_t)((uint64_t)a - (uint64_t)b);
}

int64_t pebble_rt_checked_mul_i64(int64_t a, int64_t b, PebbleSourceLoc loc) {
    (void)loc;
    return (int64_t)((uint64_t)a * (uint64_t)b);
}

int64_t pebble_rt_checked_neg_i64(int64_t a, PebbleSourceLoc loc) {
    (void)loc;
    return (int64_t)(0u - (uint64_t)a);
}

#endif /* PEBBLE_RT_MODE_SAFE / PEBBLE_RT_MODE_RELEASE */

/* ---- checked division and modulo -------------------------------------------
 * Defined once for both modes, per the header contract:
 *
 *   b == 0 — panics with PEBBLE_PANIC_DIVIDE_BY_ZERO in every configuration.
 *     There is no defined quotient for division by zero in either mode (unlike
 *     overflow, there is no bit pattern a release build could return), so this
 *     check is not gated behind the mode macro at all.
 *
 *   a == INT32_MIN && b == -1 for division — the one division input whose
 *     mathematical quotient (2147483648) does not fit i32. Follows the +, -, *
 *     convention: SAFE panics with PEBBLE_PANIC_ARITHMETIC_OVERFLOW, RELEASE
 *     returns the wrapped value (INT32_MIN).
 *
 *   a == INT32_MIN && b == -1 for modulo — mathematically 0, representable, so
 *     not a fault in either mode: return 0 directly.
 *
 * Critical implementation detail: INT32_MIN / -1 and INT32_MIN % -1 are
 * undefined behavior in C itself when actually evaluated — the language spec
 * says evaluating the expression is UB for this input, not just that the
 * result does not fit. Both functions detect that pair before ever writing
 * `a / b` or `a % b`, the same discipline the +, -, * helpers use
 * (__builtin_sub_overflow(0, a, ...) never evaluates -a for INT32_MIN). For
 * every other (a, b) with b != 0, plain C / and % are well-defined — C's
 * truncate-toward-zero semantics match the language — and are used directly;
 * no invented division algorithm.
 */

static void pebble_rt_div_by_zero_panic(const char *message, PebbleSourceLoc loc) {
    PebblePanicInfo info;
    info.kind = PEBBLE_PANIC_DIVIDE_BY_ZERO;
    info.message = message;
    info.file = loc.file;
    info.line = loc.line;
    info.column = loc.column;
    pebble_rt_panic(&info);
}

/* The INT32_MIN / -1 and INT64_MIN / -1 division boundaries, in which mode's
 * response applies. SAFE: overflow panic, same convention as the +, -, *
 * helpers. RELEASE: the wrapped result, the width's MIN (this is the
 * two's-complement bit pattern of the un-representable quotient, matching how
 * release handles +, -, *). Shared by both widths, which differ only in the
 * boundary value and the panic message.
 */
static int64_t pebble_rt_min_div_minus_one(int64_t min, const char *message, PebbleSourceLoc loc) {
#if defined(PEBBLE_RT_MODE_SAFE)
    pebble_rt_overflow_panic(message, loc);
#else
    (void)message;
    (void)loc;
#endif
    return min;
}

int32_t pebble_rt_checked_div_i32(int32_t a, int32_t b, PebbleSourceLoc loc) {
    if (b == 0) {
        pebble_rt_div_by_zero_panic("i32 division by zero", loc);
    }
    if (a == INT32_MIN && b == -1) {
        return (int32_t)pebble_rt_min_div_minus_one(INT32_MIN, "i32 division overflow", loc);
    }
    return a / b;
}

int32_t pebble_rt_checked_mod_i32(int32_t a, int32_t b, PebbleSourceLoc loc) {
    if (b == 0) {
        pebble_rt_div_by_zero_panic("i32 division by zero", loc);
    }
    if (a == INT32_MIN && b == -1) {
        return 0;
    }
    return a % b;
}

int64_t pebble_rt_checked_div_i64(int64_t a, int64_t b, PebbleSourceLoc loc) {
    if (b == 0) {
        pebble_rt_div_by_zero_panic("i64 division by zero", loc);
    }
    if (a == INT64_MIN && b == -1) {
        return pebble_rt_min_div_minus_one(INT64_MIN, "i64 division overflow", loc);
    }
    return a / b;
}

int64_t pebble_rt_checked_mod_i64(int64_t a, int64_t b, PebbleSourceLoc loc) {
    if (b == 0) {
        pebble_rt_div_by_zero_panic("i64 division by zero", loc);
    }
    if (a == INT64_MIN && b == -1) {
        return 0;
    }
    return a % b;
}
