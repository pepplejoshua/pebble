#include "pebble_rt.h"

#include <math.h>
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

/* The u64 trio: the same contract at the unsigned width. __builtin_*_overflow
 * accepts unsigned operand types directly (GCC and Clang both define them for
 * every integer type, signed or unsigned) and reports unsigned wraparound as
 * overflow exactly the way it reports signed overflow, so the SAFE path is the
 * identical shape with uint64_t substituting for int64_t. The RELEASE path
 * needs no cast gymnastics at all: plain uint64_t + - * is already the
 * defined modular-arithmetic wraparound semantics of unsigned C arithmetic
 * (C11 6.3.1.3/6.2.5), so the wrapped result is simply the direct operation.
 * There is deliberately no checked_neg_u64 here — the language rejects unary
 * minus on an unsigned operand at type-check time, so no runtime support is
 * needed.
 */
uint64_t pebble_rt_checked_add_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc) {
    uint64_t result;
    if (__builtin_add_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("u64 addition overflow", loc);
    }
    return result;
}

uint64_t pebble_rt_checked_sub_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc) {
    uint64_t result;
    if (__builtin_sub_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("u64 subtraction overflow", loc);
    }
    return result;
}

uint64_t pebble_rt_checked_mul_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc) {
    uint64_t result;
    if (__builtin_mul_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("u64 multiplication overflow", loc);
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

int32_t pebble_rt_checked_shl_i32(int32_t value, int32_t amount, PebbleSourceLoc loc) {
    if (amount < 0 || amount >= 32) {
        pebble_rt_overflow_panic("i32 shift amount out of range", loc);
    }
    /* Left-shifting a negative signed value is undefined behavior in C
     * regardless of whether amount is in range (C11 6.5.7p4) -- shift as
     * unsigned (defined for every bit pattern) and reinterpret the result,
     * matching the RELEASE-mode path below exactly. */
    return (int32_t)((uint32_t)value << (uint32_t)amount);
}

int32_t pebble_rt_checked_shr_i32(int32_t value, int32_t amount, PebbleSourceLoc loc) {
    if (amount < 0 || amount >= 32) {
        pebble_rt_overflow_panic("i32 shift amount out of range", loc);
    }
    return value >> amount;
}

int64_t pebble_rt_checked_shl_i64(int64_t value, int64_t amount, PebbleSourceLoc loc) {
    if (amount < 0 || amount >= 64) {
        pebble_rt_overflow_panic("i64 shift amount out of range", loc);
    }
    /* See pebble_rt_checked_shl_i32: left-shifting a negative signed value
     * is UB in C regardless of amount being in range; shift unsigned. */
    return (int64_t)((uint64_t)value << (uint64_t)amount);
}

int64_t pebble_rt_checked_shr_i64(int64_t value, int64_t amount, PebbleSourceLoc loc) {
    if (amount < 0 || amount >= 64) {
        pebble_rt_overflow_panic("i64 shift amount out of range", loc);
    }
    return value >> amount;
}

/* Narrower-width shift pairs: the same contract at the value's own width.
 * The value is shifted after being cast to its unsigned twin (defined for
 * every bit pattern, avoiding the C UB of left-shifting a negative signed
 * value) and the result cast back; the count is validated against the
 * operand's own bit width first. The unsigned pairs' count is unsigned too,
 * so a negative count reaches here wrapped to a value the >= width check
 * below always catches — no separate < 0 test is needed (and one would
 * trigger -Wtype-limits under -Wall -Wextra -Werror). */
uint8_t pebble_rt_checked_shl_u8(uint8_t value, uint8_t amount, PebbleSourceLoc loc) {
    if (amount >= 8) {
        pebble_rt_overflow_panic("u8 shift amount out of range", loc);
    }
    return (uint8_t)((uint8_t)value << (uint32_t)amount);
}

uint8_t pebble_rt_checked_shr_u8(uint8_t value, uint8_t amount, PebbleSourceLoc loc) {
    if (amount >= 8) {
        pebble_rt_overflow_panic("u8 shift amount out of range", loc);
    }
    return (uint8_t)((uint8_t)value >> (uint32_t)amount);
}

int8_t pebble_rt_checked_shl_i8(int8_t value, int8_t amount, PebbleSourceLoc loc) {
    if (amount < 0 || amount >= 8) {
        pebble_rt_overflow_panic("i8 shift amount out of range", loc);
    }
    return (int8_t)((uint8_t)value << (uint8_t)amount);
}

int8_t pebble_rt_checked_shr_i8(int8_t value, int8_t amount, PebbleSourceLoc loc) {
    if (amount < 0 || amount >= 8) {
        pebble_rt_overflow_panic("i8 shift amount out of range", loc);
    }
    return (int8_t)(value >> (uint8_t)amount);
}

uint16_t pebble_rt_checked_shl_u16(uint16_t value, uint16_t amount, PebbleSourceLoc loc) {
    if (amount >= 16) {
        pebble_rt_overflow_panic("u16 shift amount out of range", loc);
    }
    return (uint16_t)((uint16_t)value << (uint32_t)amount);
}

uint16_t pebble_rt_checked_shr_u16(uint16_t value, uint16_t amount, PebbleSourceLoc loc) {
    if (amount >= 16) {
        pebble_rt_overflow_panic("u16 shift amount out of range", loc);
    }
    return (uint16_t)((uint16_t)value >> (uint32_t)amount);
}

int16_t pebble_rt_checked_shl_i16(int16_t value, int16_t amount, PebbleSourceLoc loc) {
    if (amount < 0 || amount >= 16) {
        pebble_rt_overflow_panic("i16 shift amount out of range", loc);
    }
    return (int16_t)((uint16_t)value << (uint16_t)amount);
}

int16_t pebble_rt_checked_shr_i16(int16_t value, int16_t amount, PebbleSourceLoc loc) {
    if (amount < 0 || amount >= 16) {
        pebble_rt_overflow_panic("i16 shift amount out of range", loc);
    }
    return (int16_t)(value >> (uint16_t)amount);
}

uint32_t pebble_rt_checked_shl_u32(uint32_t value, uint32_t amount, PebbleSourceLoc loc) {
    if (amount >= 32) {
        pebble_rt_overflow_panic("u32 shift amount out of range", loc);
    }
    return (uint32_t)value << amount;
}

uint32_t pebble_rt_checked_shr_u32(uint32_t value, uint32_t amount, PebbleSourceLoc loc) {
    if (amount >= 32) {
        pebble_rt_overflow_panic("u32 shift amount out of range", loc);
    }
    return value >> amount;
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

uint64_t pebble_rt_checked_add_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc) {
    (void)loc;
    return a + b;
}

uint64_t pebble_rt_checked_sub_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc) {
    (void)loc;
    return a - b;
}

uint64_t pebble_rt_checked_mul_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc) {
    (void)loc;
    return a * b;
}

int64_t pebble_rt_checked_neg_i64(int64_t a, PebbleSourceLoc loc) {
    (void)loc;
    return (int64_t)(0u - (uint64_t)a);
}

int32_t pebble_rt_checked_shl_i32(int32_t value, int32_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return (int32_t)((uint32_t)value << ((uint32_t)amount & 31u));
}

int32_t pebble_rt_checked_shr_i32(int32_t value, int32_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return value >> ((uint32_t)amount & 31u);
}

int64_t pebble_rt_checked_shl_i64(int64_t value, int64_t amount, PebbleSourceLoc loc) {
    (void)loc;
    amount = (int64_t)((uint64_t)amount & 63u);
    return (int64_t)((uint64_t)value << (uint64_t)amount);
}

int64_t pebble_rt_checked_shr_i64(int64_t value, int64_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return value >> ((uint64_t)amount & 63u);
}

/* Narrower-width pairs, RELEASE: mask the count to the operand's own bit
 * width and shift. Left shifts go through the value's unsigned twin so a
 * negative signed value is never left-shifted (C UB); right shifts rely on
 * the de-facto arithmetic shift of the sign-extended int8_t/int16_t, the
 * same reliance pebble_rt_checked_shr_i32/i64 already make. */
uint8_t pebble_rt_checked_shl_u8(uint8_t value, uint8_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return (uint8_t)((uint8_t)value << ((uint32_t)amount & 7u));
}

uint8_t pebble_rt_checked_shr_u8(uint8_t value, uint8_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return (uint8_t)((uint8_t)value >> ((uint32_t)amount & 7u));
}

int8_t pebble_rt_checked_shl_i8(int8_t value, int8_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return (int8_t)((uint8_t)value << ((uint32_t)amount & 7u));
}

int8_t pebble_rt_checked_shr_i8(int8_t value, int8_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return (int8_t)(value >> ((uint32_t)amount & 7u));
}

uint16_t pebble_rt_checked_shl_u16(uint16_t value, uint16_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return (uint16_t)((uint16_t)value << ((uint32_t)amount & 15u));
}

uint16_t pebble_rt_checked_shr_u16(uint16_t value, uint16_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return (uint16_t)((uint16_t)value >> ((uint32_t)amount & 15u));
}

int16_t pebble_rt_checked_shl_i16(int16_t value, int16_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return (int16_t)((uint16_t)value << ((uint32_t)amount & 15u));
}

int16_t pebble_rt_checked_shr_i16(int16_t value, int16_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return (int16_t)(value >> ((uint32_t)amount & 15u));
}

uint32_t pebble_rt_checked_shl_u32(uint32_t value, uint32_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return value << ((uint32_t)amount & 31u);
}

uint32_t pebble_rt_checked_shr_u32(uint32_t value, uint32_t amount, PebbleSourceLoc loc) {
    (void)loc;
    return value >> ((uint32_t)amount & 31u);
}

#endif /* PEBBLE_RT_MODE_SAFE / PEBBLE_RT_MODE_RELEASE */

/* The explicit wrapping u64 arithmetic builtins, defined once for BOTH modes
 * (outside the mode gating): plain unsigned C arithmetic wraps modulo 2^64 by
 * definition (C11 6.3.1.3 / 6.2.5), so the wrapped result IS the direct
 * operation and there is nothing mode-specific to decide — no panic in SAFE,
 * no different answer in RELEASE. They take no PebbleSourceLoc for the same
 * reason: a wrapping operation is never a fault. Normal checked u64 arithmetic
 * (pebble_rt_checked_add/sub/mul_u64 above) is unaffected.
 */
uint64_t pebble_rt_wrapping_mul_u64(uint64_t a, uint64_t b) {
    return a * b;
}

uint64_t pebble_rt_wrapping_add_u64(uint64_t a, uint64_t b) {
    return a + b;
}

/* Float-to-integer conversion must check before the C cast: an out-of-range
 * or NaN conversion is undefined. The upper bounds are exclusive powers of
 * two, which remain correct when INT32_MAX/INT64_MAX are rounded by conversion
 * to the source float type. */
int32_t pebble_rt_checked_f32_to_i32(float value, PebbleSourceLoc loc) {
    if (isnan(value) || value < -2147483648.0f || value >= 2147483648.0f) {
#if defined(PEBBLE_RT_MODE_SAFE)
        pebble_rt_overflow_panic("f32 to i32 conversion overflow", loc);
#else
        (void)loc;
        return INT32_MIN;
#endif
    }
    return (int32_t)value;
}

int32_t pebble_rt_checked_f64_to_i32(double value, PebbleSourceLoc loc) {
    if (isnan(value) || value < -2147483648.0 || value >= 2147483648.0) {
#if defined(PEBBLE_RT_MODE_SAFE)
        pebble_rt_overflow_panic("f64 to i32 conversion overflow", loc);
#else
        (void)loc;
        return INT32_MIN;
#endif
    }
    return (int32_t)value;
}

int64_t pebble_rt_checked_f32_to_i64(float value, PebbleSourceLoc loc) {
    if (isnan(value) || value < -9223372036854775808.0f || value >= 9223372036854775808.0f) {
#if defined(PEBBLE_RT_MODE_SAFE)
        pebble_rt_overflow_panic("f32 to i64 conversion overflow", loc);
#else
        (void)loc;
        return INT64_MIN;
#endif
    }
    return (int64_t)value;
}

int64_t pebble_rt_checked_f64_to_i64(double value, PebbleSourceLoc loc) {
    if (isnan(value) || value < -9223372036854775808.0 || value >= 9223372036854775808.0) {
#if defined(PEBBLE_RT_MODE_SAFE)
        pebble_rt_overflow_panic("f64 to i64 conversion overflow", loc);
#else
        (void)loc;
        return INT64_MIN;
#endif
    }
    return (int64_t)value;
}

/* Integer-to-enum conversion (the compiler's CheckedIntegerToEnum node,
 * `5 as Color`): validates that the integer names a real variant of the
 * destination enum. Pebble enums are ordinal — variant Members[i] gets the C
 * enum value i — so an integer names a variant exactly when
 * 0 <= value < variant_count, and the validation is just a bounds check. A
 * single int64_t-based primitive serves every source integer width and
 * signedness: the compiler emits the source cast to int64_t before calling,
 * which sign-extends a genuinely negative signed source, zero-extends an
 * unsigned source below 2^63, and bit-reinterprets a u64 source at or above
 * 2^63 as a negative int64_t. The one comparison
 * `(uint64_t)value < (uint64_t)variant_count` then handles all of them:
 * reinterpreting a negative int64_t as uint64_t recovers the correct large
 * magnitude via two's-complement bit patterns, so a genuinely negative signed
 * source AND a genuinely huge unsigned source both correctly fail the bounds
 * check through the same single unsigned comparison, with no UB (unsigned
 * reinterpretation is well-defined in C). variant_count is always a small
 * nonnegative compile-time constant, the destination enum's variant count.
 * SAFE: an out-of-range value panics with PEBBLE_PANIC_ARITHMETIC_OVERFLOW,
 * the same panic every other checked integer primitive raises. RELEASE:
 * returns value unchanged, no check — trusting the input, matching the
 * release-mode convention for checked primitives above.
 */
int64_t pebble_rt_checked_int_to_enum(int64_t value, int64_t variant_count, PebbleSourceLoc loc) {
    if ((uint64_t)value >= (uint64_t)variant_count) {
#if defined(PEBBLE_RT_MODE_SAFE)
        pebble_rt_overflow_panic("integer-to-enum cast out of range", loc);
#else
        (void)loc;
#endif
    }
    return value;
}

/* Integer-to-optional-enum validity query (the compiler's
 * OptionalIntegerToEnum node, `5 as ?Color`): a pure, mode-independent bounds
 * check reporting whether the integer names a real variant of the destination
 * enum. The bounds logic is identical to pebble_rt_checked_int_to_enum above —
 * the same ordinal-enum reasoning (variant Members[i] gets the C enum value i,
 * so an integer names a variant exactly when 0 <= value < variant_count), the
 * same int64_t single-width input contract (the compiler emits the source cast
 * to int64_t before calling, so a genuinely negative signed source sign-extends
 * and a u64 source at or above 2^63 bit-reinterprets as negative, both of which
 * the single unsigned comparison (uint64_t)value < (uint64_t)variant_count
 * recovers correctly with no UB) — but as a pure query: it returns a bool, has
 * no panic branch, and takes no PebbleSourceLoc at all. It therefore behaves
 * IDENTICALLY in SAFE and RELEASE builds. That mode-independence is a
 * requirement, not a convenience: the compiler emits this query to compute an
 * optional's has_value field, and a wrong has_value would be silently
 * incorrect rather than merely unchecked, so the check must not be gated
 * behind the mode macro the way the checked cast's check is.
 */
bool pebble_rt_int_to_enum_is_valid(int64_t value, int64_t variant_count) {
    return (uint64_t)value < (uint64_t)variant_count;
}

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
