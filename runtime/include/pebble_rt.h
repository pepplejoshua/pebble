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
    PEBBLE_PANIC_GENERIC
} PebblePanicKind;

typedef struct PebblePanicInfo {
    PebblePanicKind kind;
    const char *message; /* non-owning, must outlive the call */
    const char *file;    /* non-owning; NULL if unavailable */
    size_t line;         /* 0 if unavailable */
} PebblePanicInfo;

#if defined(__GNUC__) || defined(__clang__)
#define PEBBLE_RT_NORETURN __attribute__((noreturn))
#else
#define PEBBLE_RT_NORETURN
#endif

PEBBLE_RT_NORETURN void pebble_rt_panic(const PebblePanicInfo *info);

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
 * i32 only for now; other integer widths arrive with the lowering slices that
 * need them.
 */
int32_t pebble_rt_checked_add_i32(int32_t a, int32_t b);
int32_t pebble_rt_checked_sub_i32(int32_t a, int32_t b);
int32_t pebble_rt_checked_mul_i32(int32_t a, int32_t b);
int32_t pebble_rt_checked_neg_i32(int32_t a);

/* ---- string representation -------------------------------------------------
 * Length-prefixed, not NUL-terminated-dependent — the old backend
 * represented `str` as a bare `const char *` and bounds-checked with
 * strlen() at every use. PebbleStr carries its own length.
 */
typedef struct PebbleStr {
    const uint8_t *data; /* not necessarily NUL-terminated */
    size_t len;
} PebbleStr;

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
