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
 * i32 and i64 for now; other integer widths arrive with the lowering slices
 * that need them. The i64 variants are the exact same contract at the wider
 * width — same overflow-panic-in-SAFE / wrap-in-RELEASE split, same
 * two's-complement wraparound computed via unsigned arithmetic.
 */
int32_t pebble_rt_checked_add_i32(int32_t a, int32_t b, PebbleSourceLoc loc);
int32_t pebble_rt_checked_sub_i32(int32_t a, int32_t b, PebbleSourceLoc loc);
int32_t pebble_rt_checked_mul_i32(int32_t a, int32_t b, PebbleSourceLoc loc);
int32_t pebble_rt_checked_neg_i32(int32_t a, PebbleSourceLoc loc);

int64_t pebble_rt_checked_add_i64(int64_t a, int64_t b, PebbleSourceLoc loc);
int64_t pebble_rt_checked_sub_i64(int64_t a, int64_t b, PebbleSourceLoc loc);
int64_t pebble_rt_checked_mul_i64(int64_t a, int64_t b, PebbleSourceLoc loc);
int64_t pebble_rt_checked_neg_i64(int64_t a, PebbleSourceLoc loc);

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

int64_t pebble_rt_checked_div_i64(int64_t a, int64_t b, PebbleSourceLoc loc);
int64_t pebble_rt_checked_mod_i64(int64_t a, int64_t b, PebbleSourceLoc loc);

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
 * arr[pebble_rt_checked_index_i32(idx, N)].
 */
int32_t pebble_rt_checked_index_i32(int32_t index, int32_t length, PebbleSourceLoc loc);
int64_t pebble_rt_checked_index_i64(int64_t index, int64_t length, PebbleSourceLoc loc);

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

/* ---- string representation -------------------------------------------------
 * Length-prefixed, not NUL-terminated-dependent — the old backend
 * represented `str` as a bare `const char *` and bounds-checked with
 * strlen() at every use. PebbleStr carries its own length.
 */
typedef struct PebbleStr {
    const uint8_t *data; /* not necessarily NUL-terminated */
    size_t len;
} PebbleStr;

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
