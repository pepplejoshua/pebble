/* Pebble runtime ABI — versioned, shared between the compiler's generated C
 * and the linked runtime implementation. See spec/compiler/10-c-backend-and-runtime.md
 * and spec/compiler/proposals/10-c-backend-implementation-plan.md for the
 * design rationale (inventory of the old backend, section 4 for this
 * slice's scope).
 *
 * This header declares the ABI surface only. Every symbol here must have
 * exactly one definition linked into the final program: the hosted
 * implementations live under runtime/src/; a freestanding platform must
 * supply its own.
 */
#ifndef PEBBLE_RT_H
#define PEBBLE_RT_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/* ---- ABI version -------------------------------------------------------
 * Bumped whenever a symbol's signature, layout, or documented contract
 * changes in a way that requires the compiler and runtime to agree. The
 * compiler stamps the version it was built against; a mismatch against the
 * linked runtime is a build-time error, not a silent one.
 */
#define PEBBLE_RT_ABI_VERSION 1u

/* ---- configuration -------------------------------------------------------
 * The compiler driver defines exactly one of PEBBLE_RT_MODE_SAFE or
 * PEBBLE_RT_MODE_RELEASE on the command line when compiling generated C
 * together with this runtime. PEBBLE_RT_FREESTANDING may additionally be
 * defined to build without the hosted implementations (context/panic/args
 * helpers below that depend on libc).
 */
#if !defined(PEBBLE_RT_MODE_SAFE) && !defined(PEBBLE_RT_MODE_RELEASE)
#error "pebble_rt.h requires PEBBLE_RT_MODE_SAFE or PEBBLE_RT_MODE_RELEASE to be defined"
#endif
#if defined(PEBBLE_RT_MODE_SAFE) && defined(PEBBLE_RT_MODE_RELEASE)
#error "pebble_rt.h: define exactly one of PEBBLE_RT_MODE_SAFE / PEBBLE_RT_MODE_RELEASE"
#endif

#if defined(PEBBLE_RT_MODE_SAFE)
#define PEBBLE_RT_CHECKS_ENABLED 1
#else
#define PEBBLE_RT_CHECKS_ENABLED 0
#endif

/* ---- context and allocator ------------------------------------------------
 * PebbleContext is always passed by pointer, never by value — the old
 * backend passed it by value into its own allocator function pointers,
 * which is a recursive-by-value shape that forces an awkward forward-typedef
 * dance for no benefit. A pointer is simpler and is what every real
 * implementation actually needs.
 *
 * The default allocator zero-initializes every allocation it returns. This
 * is a documented part of the ABI contract, not an incidental detail of the
 * default implementation: generated code and other runtime services may
 * rely on it.
 */
typedef struct PebbleContext PebbleContext;

typedef void *(*PebbleAllocFn)(PebbleContext *ctx, size_t size);
typedef void *(*PebbleReallocFn)(PebbleContext *ctx, void *ptr, size_t new_size);
typedef void (*PebbleFreeFn)(PebbleContext *ctx, void *ptr);

typedef struct PebbleAllocator {
    void *state;
    PebbleAllocFn alloc;
    PebbleReallocFn realloc;
    PebbleFreeFn free;
} PebbleAllocator;

struct PebbleContext {
    PebbleAllocator allocator;
};

/* The default hosted allocator: malloc/realloc/free-backed, zero-initializing
 * on alloc. `state` is unused (NULL) by this implementation.
 */
void *pebble_rt_default_alloc(PebbleContext *ctx, size_t size);
void *pebble_rt_default_realloc(PebbleContext *ctx, void *ptr, size_t new_size);
void pebble_rt_default_free(PebbleContext *ctx, void *ptr);

#ifndef PEBBLE_RT_FREESTANDING
/* Builds a context using the default hosted allocator above. Not available
 * when PEBBLE_RT_FREESTANDING is defined — a freestanding platform
 * constructs its own PebbleContext by whatever means it has.
 */
PebbleContext pebble_rt_default_context(void);
#endif

/* ---- panic ----------------------------------------------------------------
 * Every safety check the compiler emits (index bounds, force-unwrap, tagged-
 * union tag mismatch, assertion) funnels through this single entry point,
 * replacing the old backend's ad hoc, inconsistently-gated inline asserts
 * that bottomed out directly in libc. Exactly one definition of
 * pebble_rt_panic must be linked into any program: runtime/src/panic.c
 * supplies the hosted one; a freestanding platform must supply its own.
 */
typedef enum PebblePanicKind {
    PEBBLE_PANIC_ASSERT = 0,
    PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS,
    PEBBLE_PANIC_UNWRAP_FAILED,
    PEBBLE_PANIC_TAG_MISMATCH,
    PEBBLE_PANIC_ARITHMETIC_OVERFLOW,
    PEBBLE_PANIC_DIVIDE_BY_ZERO,
    PEBBLE_PANIC_NULL_DEREFERENCE,
    PEBBLE_PANIC_GENERIC
} PebblePanicKind;

typedef struct PebblePanicInfo {
    PebblePanicKind kind;
    const char *message; /* non-owning, must outlive the call */
    const char *file;    /* non-owning; NULL if unavailable */
    size_t line;         /* 0 if unavailable */
    size_t column;       /* 0 if unavailable (1-based Unicode-scalar column,
                           * meaningless without a nonzero line) */
} PebblePanicInfo;

#if defined(__GNUC__) || defined(__clang__)
#define PEBBLE_RT_NORETURN __attribute__((noreturn))
#else
#define PEBBLE_RT_NORETURN
#endif

PEBBLE_RT_NORETURN void pebble_rt_panic(const PebblePanicInfo *info);

/* ---- checked-operation source location -------------------------------------
 * Every pebble_rt_checked_* function below takes one of these as its final
 * argument: the Pebble *source* location (not the generated C's) of the
 * expression the check guards, so a panic report names where the fault is in
 * the program the user actually wrote, not a line in this backend's
 * generated C. The compiler resolves this at each call site from the typed
 * IR node's own Span (compiler/internal/source.Span, via
 * File.Position(offset)) and emits it as a compound-literal argument,
 * e.g. pebble_rt_checked_add_i32(x, y, (PebbleSourceLoc){"main.peb", 12, 5}).
 * file is non-owning and must be a string literal or otherwise outlive the
 * call (the compiler always emits a C string literal here); line and column
 * are 0 when genuinely unavailable (e.g. a hand-built IR node with no
 * authored span), in which case the resulting panic report omits location
 * entirely, the same as before this field existed.
 */
typedef struct PebbleSourceLoc {
    const char *file;
    size_t line;
    size_t column;
} PebbleSourceLoc;

/* ---- checked arithmetic ----------------------------------------------------
 * The compiler's typed IR retains CheckedArithmetic/CheckedNegate nodes with
 * "release-mode response left to phase 10" (spec 06b) — the language defines
 * checked-overflow semantics; this runtime is where that gets decided.
 *
 * PEBBLE_RT_MODE_SAFE: overflow calls pebble_rt_panic with
 * PEBBLE_PANIC_ARITHMETIC_OVERFLOW. PEBBLE_RT_MODE_RELEASE: overflow wraps
 * using the operation's two's-complement bit pattern (computed via unsigned
 * arithmetic, so this is defined behavior, never a signed-overflow UB trap) —
 * release mode trades the panic for speed, not for undefined behavior.
 *
 * i8, i16, u8, u16, u32, i32, i64, and u64. Every listed width supports
 * checked addition, subtraction, multiplication, division, and modulo. Other
 * integer widths arrive with the lowering
 * slices that need them. The i64 and u64 variants are the exact same contract
 * at the wider width — same overflow-panic-in-SAFE / wrap-in-RELEASE split,
 * same two's-complement wraparound (for u64 the wrap is simply the C-defined
 * modular semantics of the unsigned type itself). There is no checked_neg
 * at u64: the language rejects unary minus on an unsigned operand at
 * type-check time, so no runtime support is needed for it.
 */
int32_t pebble_rt_checked_add_i32(int32_t a, int32_t b, PebbleSourceLoc loc);
int32_t pebble_rt_checked_sub_i32(int32_t a, int32_t b, PebbleSourceLoc loc);
int32_t pebble_rt_checked_mul_i32(int32_t a, int32_t b, PebbleSourceLoc loc);
int32_t pebble_rt_checked_neg_i32(int32_t a, PebbleSourceLoc loc);

int8_t pebble_rt_checked_add_i8(int8_t a, int8_t b, PebbleSourceLoc loc);
int8_t pebble_rt_checked_sub_i8(int8_t a, int8_t b, PebbleSourceLoc loc);
int8_t pebble_rt_checked_mul_i8(int8_t a, int8_t b, PebbleSourceLoc loc);

int16_t pebble_rt_checked_add_i16(int16_t a, int16_t b, PebbleSourceLoc loc);
int16_t pebble_rt_checked_sub_i16(int16_t a, int16_t b, PebbleSourceLoc loc);
int16_t pebble_rt_checked_mul_i16(int16_t a, int16_t b, PebbleSourceLoc loc);

int64_t pebble_rt_checked_add_i64(int64_t a, int64_t b, PebbleSourceLoc loc);
int64_t pebble_rt_checked_sub_i64(int64_t a, int64_t b, PebbleSourceLoc loc);
int64_t pebble_rt_checked_mul_i64(int64_t a, int64_t b, PebbleSourceLoc loc);
int64_t pebble_rt_checked_neg_i64(int64_t a, PebbleSourceLoc loc);

/* Narrower-width checked negation: the same contract at the operand's own
 * width, for the fixed-width signed integers the language actually exposes
 * (`-a` on an i8/i16 value is checker-accepted and must negate in range or
 * panic on the minimum). The i32/i64 family above is the model: SAFE mode
 * reports the single overflow boundary (-<MIN> is unrepresentable) via
 * __builtin_sub_overflow, RELEASE mode wraps through the unsigned twin.
 * There is deliberately no checked_neg at any unsigned width, exactly as for
 * u64 — the language rejects unary minus on an unsigned operand. */
int8_t pebble_rt_checked_neg_i8(int8_t a, PebbleSourceLoc loc);
int16_t pebble_rt_checked_neg_i16(int16_t a, PebbleSourceLoc loc);

uint64_t pebble_rt_checked_add_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_sub_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_mul_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc);

uint8_t pebble_rt_checked_add_u8(uint8_t a, uint8_t b, PebbleSourceLoc loc);
uint8_t pebble_rt_checked_sub_u8(uint8_t a, uint8_t b, PebbleSourceLoc loc);
uint8_t pebble_rt_checked_mul_u8(uint8_t a, uint8_t b, PebbleSourceLoc loc);

uint16_t pebble_rt_checked_add_u16(uint16_t a, uint16_t b, PebbleSourceLoc loc);
uint16_t pebble_rt_checked_sub_u16(uint16_t a, uint16_t b, PebbleSourceLoc loc);
uint16_t pebble_rt_checked_mul_u16(uint16_t a, uint16_t b, PebbleSourceLoc loc);

uint32_t pebble_rt_checked_add_u32(uint32_t a, uint32_t b, PebbleSourceLoc loc);
uint32_t pebble_rt_checked_sub_u32(uint32_t a, uint32_t b, PebbleSourceLoc loc);
uint32_t pebble_rt_checked_mul_u32(uint32_t a, uint32_t b, PebbleSourceLoc loc);

/* ---- wrapping u64 arithmetic ------------------------------------------------
 * The explicit wrapping builtins wrapping_mul_u64 / wrapping_add_u64 lower to
 * these helpers. They implement the operation's modular-arithmetic wraparound
 * via plain unsigned C arithmetic, which is defined to wrap modulo 2^64 (C11
 * 6.3.1.3 / 6.2.5), so the implementation is identical in SAFE and RELEASE
 * modes: the helpers are defined once, outside the mode gating, and take no
 * PebbleSourceLoc because they never panic in either mode. Normal checked u64
 * arithmetic (pebble_rt_checked_add/sub/mul_u64) is unaffected.
 */
uint64_t pebble_rt_wrapping_mul_u64(uint64_t a, uint64_t b);
uint64_t pebble_rt_wrapping_add_u64(uint64_t a, uint64_t b);

/* ---- checked bit shifts -----------------------------------------------------
 * Shift counts outside [0, 32) or [0, 64) are invalid. SAFE mode panics with
 * PEBBLE_PANIC_ARITHMETIC_OVERFLOW; RELEASE mode masks the count to the
 * operand width before shifting, matching native hardware shift behavior.
 *
 * The narrower-width pairs (u8/u16/i8/i16/u32) enforce the count against
 * their own operand width ([0, 8), [0, 16), or [0, 32)) and take value,
 * count, and result all at the operand's own fixed-width C type — the same
 * contract at the narrower width. For the unsigned pairs the count is
 * unsigned too, so a negative count (cast at the call site to the operand's
 * width) wraps to a value the >= width check always catches in SAFE mode,
 * and the RELEASE mask (& 7u / & 15u / & 31u) reduces it to the correct
 * residue regardless.
 *
 * The u64 pair serves both u64 and uint (both carry the C type uint64_t, so
 * one helper pair reads both back at their true width), the same dual-width
 * mapping optionalUnwrapSuffix uses for a uint/u64 optional payload.
 */
int32_t pebble_rt_checked_shl_i32(int32_t value, int32_t amount, PebbleSourceLoc loc);
int32_t pebble_rt_checked_shr_i32(int32_t value, int32_t amount, PebbleSourceLoc loc);
int64_t pebble_rt_checked_shl_i64(int64_t value, int64_t amount, PebbleSourceLoc loc);
int64_t pebble_rt_checked_shr_i64(int64_t value, int64_t amount, PebbleSourceLoc loc);

uint8_t pebble_rt_checked_shl_u8(uint8_t value, uint8_t amount, PebbleSourceLoc loc);
uint8_t pebble_rt_checked_shr_u8(uint8_t value, uint8_t amount, PebbleSourceLoc loc);
int8_t pebble_rt_checked_shl_i8(int8_t value, int8_t amount, PebbleSourceLoc loc);
int8_t pebble_rt_checked_shr_i8(int8_t value, int8_t amount, PebbleSourceLoc loc);
uint16_t pebble_rt_checked_shl_u16(uint16_t value, uint16_t amount, PebbleSourceLoc loc);
uint16_t pebble_rt_checked_shr_u16(uint16_t value, uint16_t amount, PebbleSourceLoc loc);
int16_t pebble_rt_checked_shl_i16(int16_t value, int16_t amount, PebbleSourceLoc loc);
int16_t pebble_rt_checked_shr_i16(int16_t value, int16_t amount, PebbleSourceLoc loc);
uint32_t pebble_rt_checked_shl_u32(uint32_t value, uint32_t amount, PebbleSourceLoc loc);
uint32_t pebble_rt_checked_shr_u32(uint32_t value, uint32_t amount, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_shl_u64(uint64_t value, uint64_t amount, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_shr_u64(uint64_t value, uint64_t amount, PebbleSourceLoc loc);

/* ---- checked float-to-integer conversion -----------------------------------
 * Converts f32/f64 values to any fixed-width integer destination after
 * checking for NaN and values outside the destination's representable range.
 * SAFE mode panics with PEBBLE_PANIC_ARITHMETIC_OVERFLOW; RELEASE mode
 * returns the destination's integer-indefinite sentinel — the signed widths
 * return their minimum (INT8_MIN/INT16_MIN/INT32_MIN/INT64_MIN), the unsigned
 * widths the sign-bit-set bit pattern (0x80/0x8000/0x80000000/
 * 0x8000000000000000), the same bit pattern the signed sentinels use. The
 * upper bound is exclusive: for floating sources it is expressed as a power of
 * two (2^7/2^8/2^15/2^16/2^31/2^32/2^63/2^64) because the destination's
 * own maximum rounds to that value at the relevant precision.
 */
int32_t pebble_rt_checked_f32_to_i32(float value, PebbleSourceLoc loc);
int32_t pebble_rt_checked_f64_to_i32(double value, PebbleSourceLoc loc);
int64_t pebble_rt_checked_f32_to_i64(float value, PebbleSourceLoc loc);
int64_t pebble_rt_checked_f64_to_i64(double value, PebbleSourceLoc loc);
int8_t pebble_rt_checked_f32_to_i8(float value, PebbleSourceLoc loc);
int8_t pebble_rt_checked_f64_to_i8(double value, PebbleSourceLoc loc);
int16_t pebble_rt_checked_f32_to_i16(float value, PebbleSourceLoc loc);
int16_t pebble_rt_checked_f64_to_i16(double value, PebbleSourceLoc loc);
uint8_t pebble_rt_checked_f32_to_u8(float value, PebbleSourceLoc loc);
uint8_t pebble_rt_checked_f64_to_u8(double value, PebbleSourceLoc loc);
uint16_t pebble_rt_checked_f32_to_u16(float value, PebbleSourceLoc loc);
uint16_t pebble_rt_checked_f64_to_u16(double value, PebbleSourceLoc loc);
uint32_t pebble_rt_checked_f32_to_u32(float value, PebbleSourceLoc loc);
uint32_t pebble_rt_checked_f64_to_u32(double value, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_f32_to_u64(float value, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_f64_to_u64(double value, PebbleSourceLoc loc);

/* ---- checked integer-to-enum conversion -------------------------------------
 * Validates that an integer names a real variant of a destination enum (the
 * compiler's CheckedIntegerToEnum node, `5 as Color`). Pebble enums are
 * ordinal — variant Members[i] gets the C enum value i — so the validation is
 * just a bounds check: an integer names a variant exactly when
 * 0 <= value < variant_count. One int64_t-based primitive serves every source
 * integer width and signedness: the compiler emits the source cast to int64_t
 * before calling (sign-extending a genuinely negative signed source,
 * zero-extending an unsigned source below 2^63, and bit-reinterpreting a u64
 * source at or above 2^63 as negative), and the primitive's single unsigned
 * comparison (uint64_t)value < (uint64_t)variant_count recovers both a
 * genuinely negative signed source and a genuinely huge unsigned source as
 * out-of-range — unsigned reinterpreting is well-defined in C, so no path
 * invokes UB. variant_count is always a small nonnegative compile-time
 * constant (the destination enum's variant count). SAFE: an out-of-range value
 * panics with PEBBLE_PANIC_ARITHMETIC_OVERFLOW, the same panic the other
 * checked integer primitives raise. RELEASE: returns value unchanged, no
 * check — trusting the input, matching this runtime's release-mode convention
 * for checked primitives.
 */
int64_t pebble_rt_checked_int_to_enum(int64_t value, int64_t variant_count, PebbleSourceLoc loc);

/* ---- integer-to-optional-enum validity query -------------------------------
 * A pure, mode-independent bounds check (the compiler's OptionalIntegerToEnum
 * node, `5 as ?Color`): reports whether the integer names a real variant of the
 * destination enum. The bounds logic is identical to
 * pebble_rt_checked_int_to_enum above — the same ordinal-enum reasoning, the
 * same int64_t single-width input contract, the same single unsigned
 * comparison (uint64_t)value < (uint64_t)variant_count — but as a pure query:
 * it returns a bool, has no panic branch, and takes no PebbleSourceLoc. It
 * therefore behaves IDENTICALLY in SAFE and RELEASE builds; the check must not
 * be gated behind the mode macro, because the compiler emits this query to
 * compute an optional's has_value field and a wrong has_value would be
 * silently incorrect rather than merely unchecked.
 */
bool pebble_rt_int_to_enum_is_valid(int64_t value, int64_t variant_count);

/* ---- checked division and modulo -------------------------------------------
 * Division and modulo have a fault case wraparound cannot fix: b == 0 has no
 * defined quotient at all, in either mode — unlike +, -, *, there is no
 * "release mode returns some defined bit pattern" answer, so divide-by-zero
 * panics with PEBBLE_PANIC_DIVIDE_BY_ZERO in every configuration, not just
 * PEBBLE_RT_MODE_SAFE.
 *
 * The one arithmetic-overflow case division has (INT32_MIN / -1, not
 * representable in i32) follows the same convention as +, -, * above:
 * PEBBLE_RT_MODE_SAFE panics with PEBBLE_PANIC_ARITHMETIC_OVERFLOW,
 * PEBBLE_RT_MODE_RELEASE returns the wrapped result (INT32_MIN). Both
 * implementations must special-case this input rather than ever evaluating
 * C's `a / b` for it — INT32_MIN / -1 is undefined behavior in C itself, not
 * just a value to detect after the fact.
 *
 * INT32_MIN % -1 is mathematically 0 and IS representable — C still treats
 * evaluating `%` for this input as undefined behavior because it is defined
 * in terms of division, so this case must also be special-cased (returning 0
 * directly) rather than evaluated, but it is not a fault: no panic, in either
 * mode.
 *
 * The i64 variants are the exact same contract at the wider width: b == 0
 * panics in every mode, INT64_MIN / -1 follows the SAFE-panics /
 * RELEASE-wraps-to-INT64_MIN convention, and INT64_MIN % -1 is 0 in both
 * modes, never evaluated directly.
 */
int32_t pebble_rt_checked_div_i32(int32_t a, int32_t b, PebbleSourceLoc loc);
int32_t pebble_rt_checked_mod_i32(int32_t a, int32_t b, PebbleSourceLoc loc);

int8_t pebble_rt_checked_div_i8(int8_t a, int8_t b, PebbleSourceLoc loc);
int8_t pebble_rt_checked_mod_i8(int8_t a, int8_t b, PebbleSourceLoc loc);
int16_t pebble_rt_checked_div_i16(int16_t a, int16_t b, PebbleSourceLoc loc);
int16_t pebble_rt_checked_mod_i16(int16_t a, int16_t b, PebbleSourceLoc loc);

int64_t pebble_rt_checked_div_i64(int64_t a, int64_t b, PebbleSourceLoc loc);
int64_t pebble_rt_checked_mod_i64(int64_t a, int64_t b, PebbleSourceLoc loc);

uint8_t pebble_rt_checked_div_u8(uint8_t a, uint8_t b, PebbleSourceLoc loc);
uint8_t pebble_rt_checked_mod_u8(uint8_t a, uint8_t b, PebbleSourceLoc loc);
uint16_t pebble_rt_checked_div_u16(uint16_t a, uint16_t b, PebbleSourceLoc loc);
uint16_t pebble_rt_checked_mod_u16(uint16_t a, uint16_t b, PebbleSourceLoc loc);
uint32_t pebble_rt_checked_div_u32(uint32_t a, uint32_t b, PebbleSourceLoc loc);
uint32_t pebble_rt_checked_mod_u32(uint32_t a, uint32_t b, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_div_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_mod_u64(uint64_t a, uint64_t b, PebbleSourceLoc loc);

/* ---- checked array indexing -------------------------------------------------
 * A fixed-length array's element access is bounds-checked: an index outside
 * [0, length) panics with PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS. Unlike checked
 * arithmetic overflow, this is not gated by PEBBLE_RT_MODE_SAFE/RELEASE — an
 * out-of-bounds C array access is undefined behavior with no defined "wrapped"
 * result to fall back to (unlike arithmetic overflow's well-defined
 * two's-complement wraparound), so the check runs in every configuration, the
 * same reasoning already applied to division by zero above. length is the
 * array type's own compile-time element count, passed through at each call
 * site rather than baked into the check itself, so one pair of functions
 * serves every array length and width. Returns index unchanged when in
 * bounds, so a call site can be used directly as the emitted array subscript:
 * arr[pebble_rt_checked_index_i32(idx, N)]. The u64 variant is the same
 * contract at the unsigned width — its one difference is that an index can
 * never be negative, so only the index >= length bound needs checking.
 */
int32_t pebble_rt_checked_index_i32(int32_t index, int32_t length, PebbleSourceLoc loc);
int64_t pebble_rt_checked_index_i64(int64_t index, int64_t length, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_index_u64(uint64_t index, uint64_t length, PebbleSourceLoc loc);

/* ---- checked slice range ---------------------------------------------------
 * A slice expression (arr[start:end]) validates 0 <= start <= end <= length
 * before forming the view, panicking with PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS
 * otherwise — like checked indexing above, this is not gated by
 * PEBBLE_RT_MODE_SAFE/RELEASE, since there is no defined "wrapped" slice for
 * an invalid range to fall back to. Returns start unchanged when the range is
 * valid, so a call site can be used directly as the emitted slice's base
 * pointer offset: arr + pebble_rt_checked_slice_start_i32(start, end, N)
 * (the slice's own length is then simply end - start, itself already proven
 * non-negative by this check, so no separate helper computes it).
 */
int32_t pebble_rt_checked_slice_start_i32(int32_t start, int32_t end, int32_t length, PebbleSourceLoc loc);
int64_t pebble_rt_checked_slice_start_i64(int64_t start, int64_t end, int64_t length, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_slice_start_u64(uint64_t start, uint64_t end, uint64_t length, PebbleSourceLoc loc);

/* ---- checked pointer dereference --------------------------------------------
 * A dereference of a raw pointer is null-checked: a NULL pointer panics with
 * PEBBLE_PANIC_NULL_DEREFERENCE. Like array bounds and division by zero, this
 * is not gated by PEBBLE_RT_MODE_SAFE/RELEASE — dereferencing NULL is
 * undefined behavior in C with no defined "wrapped" fallback, so the check
 * runs in every configuration. Returns ptr unchanged when non-NULL, so a call
 * site can be used directly as the dereferenced value:
 * *pebble_rt_checked_deref_ptr(ptr, loc).
 */
void *pebble_rt_checked_deref_ptr(void *ptr, PebbleSourceLoc loc);

/* ---- checked optional unwrap -----------------------------------------------
 * An optional's force-unwrap (`value!`) panics with PEBBLE_PANIC_UNWRAP_FAILED
 * when the optional holds no value. Like array bounds and division by zero,
 * this is not gated by PEBBLE_RT_MODE_SAFE/RELEASE — there is no defined
 * payload to return for an absent optional in either mode. has_value is the
 * optional's own tag; value is its payload, read unconditionally by the
 * caller's emitted C (unwrapping the C struct) whether or not it is
 * meaningful — this function only turns "was the tag true" into "panic or
 * return the payload unchanged", one function per payload width/type this
 * backend supports.
 */
int32_t pebble_rt_checked_unwrap_i32(bool has_value, int32_t value, PebbleSourceLoc loc);
int64_t pebble_rt_checked_unwrap_i64(bool has_value, int64_t value, PebbleSourceLoc loc);
bool pebble_rt_checked_unwrap_bool(bool has_value, bool value, PebbleSourceLoc loc);
uint8_t pebble_rt_checked_unwrap_u8(bool has_value, uint8_t value, PebbleSourceLoc loc);
uint16_t pebble_rt_checked_unwrap_u16(bool has_value, uint16_t value, PebbleSourceLoc loc);
int8_t pebble_rt_checked_unwrap_i8(bool has_value, int8_t value, PebbleSourceLoc loc);
int16_t pebble_rt_checked_unwrap_i16(bool has_value, int16_t value, PebbleSourceLoc loc);
uint32_t pebble_rt_checked_unwrap_u32(bool has_value, uint32_t value, PebbleSourceLoc loc);
uint64_t pebble_rt_checked_unwrap_u64(bool has_value, uint64_t value, PebbleSourceLoc loc);
void *pebble_rt_checked_unwrap_ptr(bool has_value, void *value, PebbleSourceLoc loc);
float pebble_rt_checked_unwrap_f32(bool has_value, float value, PebbleSourceLoc loc);
double pebble_rt_checked_unwrap_f64(bool has_value, double value, PebbleSourceLoc loc);
/* An aggregate payload (a fixed array or slice) has no by-value scalar to
 * return through the helpers above, so its force-unwrap is lowered by the
 * compiler to a call to this presence-only check followed by a read of the
 * optional's own .value field: the call panics with
 * PEBBLE_PANIC_UNWRAP_FAILED when the optional holds no value and returns
 * nothing otherwise, leaving the compiler's emitted C to read the payload
 * out of the optional struct. Same not-mode-gated contract as the scalar
 * family (there is no defined payload for an absent optional in either
 * mode). */
void pebble_rt_checked_unwrap_present(bool has_value, PebbleSourceLoc loc);

/* ---- string representation -------------------------------------------------
 * Length-prefixed, not NUL-terminated-dependent — the old backend
 * represented `str` as a bare `const char *` and bounds-checked with
 * strlen() at every use. PebbleStr carries its own length.
 */
typedef struct PebbleStr {
    const uint8_t *data; /* not necessarily NUL-terminated */
    size_t len;
} PebbleStr;

/* ---- interpolated string materialization ----------------------------------
 * Builds a PebbleStr from a sequence of parts: literal text, bool values
 * formatted as "true"/"false", integer values formatted as their decimal
 * representation (a signed value with a leading '-' when negative, an
 * unsigned value with no sign — the runtime formats by value, so any integer
 * width promotes to the fixed int64_t/uint64_t fields below), float values
 * formatted with %f (the default precision, 6 decimal digits — the same
 * convention buildPrint's own scalar float print path uses, so an
 * interpolated float and a directly-printed float render identically; both
 * f32 and f64 promote to double), str values whose existing .data/.len
 * bytes are appended directly into the result without formatting (the same
 * byte-append logic PEBBLE_STR_PART_TEXT uses, just sourcing from a PebbleStr
 * instead of a raw C string), and char values whose Unicode scalar is encoded
 * to its UTF-8 byte sequence via pebble_rt_char_to_utf8 (a char is a Unicode
 * scalar value, int32_t, not a byte — ASCII encodes to 1 byte, é to 2, an
 * astral char to 4). Used by the compiler to materialize an interpolated
 * string expression as an ordinary str value (not just in print statements).
 */
typedef enum PebbleStrPartKind {
    PEBBLE_STR_PART_TEXT,
    PEBBLE_STR_PART_BOOL,
    PEBBLE_STR_PART_INT,   /* signed integer, int_value */
    PEBBLE_STR_PART_UINT,  /* unsigned integer, uint_value */
    PEBBLE_STR_PART_FLOAT, /* float, float_value */
    PEBBLE_STR_PART_STR,   /* str, str_value */
    PEBBLE_STR_PART_CHAR,  /* char, char_value */
} PebbleStrPartKind;

typedef struct PebbleStrPart {
    PebbleStrPartKind kind;
    const char *text;       /* for PEBBLE_STR_PART_TEXT */
    int bool_value;         /* for PEBBLE_STR_PART_BOOL: 0 or 1 */
    int64_t int_value;      /* for PEBBLE_STR_PART_INT */
    uint64_t uint_value;    /* for PEBBLE_STR_PART_UINT */
    double float_value;     /* for PEBBLE_STR_PART_FLOAT */
    PebbleStr str_value;    /* for PEBBLE_STR_PART_STR */
    int32_t char_value;     /* for PEBBLE_STR_PART_CHAR: Unicode scalar */
} PebbleStrPart;

PebbleStr pebble_rt_str_from_parts(PebbleContext *ctx, const PebbleStrPart *parts, size_t count);

/* Byte-for-byte equality: false immediately on a length mismatch (so a and b
 * are never memcmp'd past the shorter length), otherwise a memcmp over the
 * shared length. Not NUL-terminated-dependent, per this type's own contract
 * above.
 */
bool pebble_rt_str_eq(PebbleStr a, PebbleStr b);

/* Lexicographic byte comparison, the same contract as C's memcmp/strcmp:
 * negative if a sorts before b, zero if equal, positive if a sorts after b.
 * Compares byte-for-byte over the shared length first; if that prefix is
 * equal, the shorter string sorts first (matching strcmp's own convention
 * for one string being a prefix of the other). Not NUL-terminated-dependent,
 * per this type's own contract above.
 */
int pebble_rt_str_cmp(PebbleStr a, PebbleStr b);

/* Indexed access into a str, `s[i]`, is a Unicode-scalar-value index, not a
 * byte offset: index 0 names the first decoded codepoint, index 1 the
 * second, and so on, regardless of how many UTF-8 bytes each one occupies.
 * This walks the UTF-8 byte sequence from the start, decoding one codepoint
 * at a time, until the index'th one is reached — O(index) work, not O(1),
 * since UTF-8 is a variable-width encoding with no random-access byte
 * offset for "the i'th codepoint". Panics with
 * PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS in every configuration (SAFE and
 * RELEASE) — like checked array indexing and the checked slice range
 * above, there is no defined fallback result for an index past the last
 * codepoint — and, since PebbleStr's own bytes are not guaranteed to be
 * valid UTF-8 (a slice or a hand-constructed str could contain anything),
 * also panics on a malformed encoding it encounters along the way: a lead
 * byte that doesn't start a valid 1/2/3/4-byte sequence, a continuation
 * byte that isn't in [0x80, 0xBF], or a sequence truncated by the end of
 * the string's own length. Returns the decoded scalar value as an int32_t
 * (a Unicode scalar value fits in 21 bits; the language's own `char` type
 * is a full Unicode scalar value, not a single byte, matching tir.Literal's
 * own `Char rune` field, Go's rune being an int32 alias).
 */
int32_t pebble_rt_str_char_at_i32(PebbleStr s, int32_t index, PebbleSourceLoc loc);
int32_t pebble_rt_str_char_at_i64(PebbleStr s, int64_t index, PebbleSourceLoc loc);
int32_t pebble_rt_str_char_at_u64(PebbleStr s, uint64_t index, PebbleSourceLoc loc);

/* ---- char-to-UTF-8 encoding -------------------------------------------------
 * Encodes one Pebble `char` — a full Unicode scalar value, carried in an
 * int32_t, the same representation pebble_rt_str_char_at_* decodes to — as
 * UTF-8 into a caller-owned buffer. Normal UTF-8 encoding: 1 byte through
 * U+007F, 2 through U+07FF, 3 through U+FFFF, 4 through U+10FFFF. A trailing
 * NUL byte is ALWAYS written, so the buffer is directly usable as a C string
 * (the backend passes it to C `%s`). Returns the number of encoded bytes
 * (1-4).
 *
 * out must be caller-owned with capacity for four encoded bytes plus the
 * trailing NUL byte. PRECONDITION: scalar is a valid Unicode scalar value —
 * the language guarantees a `char` always is — so this helper performs no
 * validation and never panics. Mode-independent: SAFE and RELEASE builds
 * behave identically.
 */
size_t pebble_rt_char_to_utf8(int32_t scalar, uint8_t out[5]);

#ifndef PEBBLE_RT_FREESTANDING
/* ---- hosted argument adaptation --------------------------------------------
 * Adapts host argc/argv into a slice of PebbleStr. The returned slice's
 * backing array is allocated via ctx's allocator and owned by the caller.
 * This is a building block for the entry adapter, not the entry adapter
 * itself — wiring a generated program's user entry into a real C `main` is
 * later slice 10.2's job, once the compiler defines the entry signature
 * contract it needs to satisfy.
 */
typedef struct PebbleStrSlice {
    const PebbleStr *data;
    size_t len;
} PebbleStrSlice;

PebbleStrSlice pebble_rt_args_from_argv(PebbleContext *ctx, int argc, const char **argv);
#endif

#endif /* PEBBLE_RT_H */
