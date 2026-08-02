#include "pebble_rt.h"

#include <stdint.h>

/* Checked i32 arithmetic, the runtime home of the typed IR's
 * CheckedArithmetic/CheckedNegate nodes (spec 06b leaves "release-mode
 * response to phase 10"; this file is that decision).
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
 *                           uint32_t, does the operation unsigned (defined
 *                           wraparound), and casts back.
 *
 * The SAFE path detects overflow with the compiler's own overflow-checking
 * builtins (__builtin_*_overflow), which report overflow without the code
 * itself invoking UB while computing it. They are available in both GCC and
 * Clang, which this runtime already assumes (PEBBLE_RT_NORETURN's
 * __GNUC__ || __clang__ guard in the header is the precedent).
 */

#if defined(PEBBLE_RT_MODE_SAFE)

static void pebble_rt_overflow_panic(const char *message) {
    PebblePanicInfo info;
    info.kind = PEBBLE_PANIC_ARITHMETIC_OVERFLOW;
    info.message = message;
    info.file = NULL;
    info.line = 0;
    pebble_rt_panic(&info);
}

int32_t pebble_rt_checked_add_i32(int32_t a, int32_t b) {
    int32_t result;
    if (__builtin_add_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("i32 addition overflow");
    }
    return result;
}

int32_t pebble_rt_checked_sub_i32(int32_t a, int32_t b) {
    int32_t result;
    if (__builtin_sub_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("i32 subtraction overflow");
    }
    return result;
}

int32_t pebble_rt_checked_mul_i32(int32_t a, int32_t b) {
    int32_t result;
    if (__builtin_mul_overflow(a, b, &result)) {
        pebble_rt_overflow_panic("i32 multiplication overflow");
    }
    return result;
}

/* Negation overflows only at the one boundary value: -INT32_MIN is not
 * representable in i32. __builtin_sub_overflow(0, a, ...) reports exactly
 * that case.
 */
int32_t pebble_rt_checked_neg_i32(int32_t a) {
    int32_t result;
    if (__builtin_sub_overflow(0, a, &result)) {
        pebble_rt_overflow_panic("i32 negation overflow");
    }
    return result;
}

#else /* PEBBLE_RT_MODE_RELEASE */

int32_t pebble_rt_checked_add_i32(int32_t a, int32_t b) {
    return (int32_t)((uint32_t)a + (uint32_t)b);
}

int32_t pebble_rt_checked_sub_i32(int32_t a, int32_t b) {
    return (int32_t)((uint32_t)a - (uint32_t)b);
}

int32_t pebble_rt_checked_mul_i32(int32_t a, int32_t b) {
    return (int32_t)((uint32_t)a * (uint32_t)b);
}

int32_t pebble_rt_checked_neg_i32(int32_t a) {
    return (int32_t)(0u - (uint32_t)a);
}

#endif
