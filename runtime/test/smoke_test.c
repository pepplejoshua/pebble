/* Hand-written smoke test for the Pebble runtime ABI skeleton.
 *
 * Exercises, in order:
 *   1. pebble_rt_default_context() returns a context whose allocator
 *      function pointers are non-NULL.
 *   2. ctx.allocator.alloc() zero-initializes its allocation and the
 *      memory round-trips a write.
 *   3. pebble_rt_args_from_argv() adapts an argv into a PebbleStrSlice
 *      with matching data/len per entry.
 *   4. ctx.allocator.free() releases the allocation.
 *   5. pebble_rt_panic() aborts and prints — verified in a forked
 *      child, because a direct call would abort this very process
 *      before it could report success on the checks above.
 *   6. The checked i32 and i64 helpers produce arithmetically correct
 *      results for non-overflowing operands, including division/modulo with
 *      negative operands (plain C / and % truncate toward zero).
 *   7. Overflow behavior is mode-dependent and both modes are asserted:
 *      SAFE — pebble_rt_checked_add_i32(INT32_MAX, 1),
 *      pebble_rt_checked_neg_i32(INT32_MIN), and their i64 twins
 *      (add(INT64_MAX, 1), neg(INT64_MIN), div(INT64_MIN, -1)) panic
 *      through pebble_rt_panic, verified in forked children like check 5;
 *      RELEASE — the same operations wrap to the width's MIN instead of
 *      panicking.
 *   8. Division and modulo's distinct fault cases: division by zero panics
 *      in EVERY configuration (RELEASE included) at both widths — there is
 *      no defined quotient, so this fork check is not mode-gated; the MIN
 *      % -1 cases return 0 in both modes (mathematically 0, representable,
 *      not a fault); and MIN / -1 follows the overflow convention (SAFE
 *      panics with overflow, RELEASE wraps to the width's MIN).
 *   9. Checked array indexing: in-bounds indices return unchanged at both
 *      widths; an index too high or negative panics in EVERY configuration
 *      (RELEASE included) — like division by zero, there is no defined
 *      fallback for an out-of-bounds access.
 *  10. Checked optional unwrap: a present optional's payload returns
 *      unchanged at every payload type (i32, i64, bool); unwrapping an
 *      absent optional panics in EVERY configuration (RELEASE included).
 *  11. Str equality: identical bytes compare equal, differing bytes compare
 *      unequal, a length mismatch compares unequal without reading past the
 *      shorter operand, and two empty strs compare equal.
 *
 * Any failing check exits non-zero; on success it prints PASS and exits
 * zero.
 */
#include "pebble_rt.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

static void test_context_and_allocator(void) {
    PebbleContext ctx = pebble_rt_default_context();
    assert(ctx.allocator.alloc != NULL);
    assert(ctx.allocator.realloc != NULL);
    assert(ctx.allocator.free != NULL);

    const size_t n = 64;
    unsigned char *p = (unsigned char *)ctx.allocator.alloc(&ctx, n);
    assert(p != NULL);

    /* Zero-initialization is a documented ABI contract: before our own
     * write, every byte must read back as zero.
     */
    for (size_t i = 0; i < n; i++) {
        assert(p[i] == 0);
    }

    /* A write must stick. */
    for (size_t i = 0; i < n; i++) {
        p[i] = (unsigned char)(i * 7 + 3);
    }
    for (size_t i = 0; i < n; i++) {
        assert(p[i] == (unsigned char)(i * 7 + 3));
    }

    /* Zero-length allocation must yield a valid non-NULL pointer (the
     * documented size-0 convention) that free() accepts.
     */
    unsigned char *z = (unsigned char *)ctx.allocator.alloc(&ctx, 0);
    assert(z != NULL);

    ctx.allocator.free(&ctx, z);
    ctx.allocator.free(&ctx, p);
}

static void test_args_from_argv(void) {
    const char *prog = "./smoke_test";
    const char *a1 = "alpha";
    const char *a2 = "";
    const char *a3 = "a much longer third argument with spaces";
    const char *argv[] = {prog, a1, a2, a3};
    const int argc = 4;

    PebbleContext ctx = pebble_rt_default_context();
    PebbleStrSlice slice = pebble_rt_args_from_argv(&ctx, argc, argv);
    assert(slice.len == (size_t)argc);
    assert(slice.data != NULL);

    const char *expected[] = {prog, a1, a2, a3};
    for (int i = 0; i < argc; i++) {
        const char *want = expected[i];
        size_t want_len = strlen(want);
        assert(slice.data[i].len == want_len);
        if (want_len > 0) {
            assert(memcmp(slice.data[i].data, want, want_len) == 0);
        }
    }

    /* argc <= 0 yields a zero-length slice with NULL data. */
    PebbleStrSlice empty = pebble_rt_args_from_argv(&ctx, 0, argv);
    assert(empty.len == 0);
    assert(empty.data == NULL);

    /* The caller owns the returned backing array: free it. */
    ctx.allocator.free(&ctx, (void *)slice.data);
}

/* The checked i32 helpers with values that must not overflow: assert the
 * arithmetically correct result. The negation boundary cases are covered
 * too — INT32_MIN is the one value whose negation overflows, so the
 * adjacent value INT32_MIN + 1 must negate cleanly to INT32_MAX, and
 * INT32_MIN itself is exercised per-mode below (SAFE: panics, RELEASE:
 * wraps). Every check has an i64 twin asserting the same result at the
 * wider width (with INT64_MIN's adjacency boundary).
 */
static void test_checked_arithmetic_normal(void) {
    assert(pebble_rt_checked_add_i32(2, 3) == 5);
    assert(pebble_rt_checked_sub_i32(10, 4) == 6);
    assert(pebble_rt_checked_mul_i32(6, 7) == 42);
    assert(pebble_rt_checked_neg_i32(5) == -5);
    assert(pebble_rt_checked_neg_i32(INT32_MIN + 1) == INT32_MAX);

    assert(pebble_rt_checked_add_i64(2, 3) == 5);
    assert(pebble_rt_checked_sub_i64(10, 4) == 6);
    assert(pebble_rt_checked_mul_i64(6, 7) == 42);
    assert(pebble_rt_checked_neg_i64(5) == -5);
    assert(pebble_rt_checked_neg_i64(INT64_MIN + 1) == INT64_MAX);

    /* Division and modulo use plain C / and %, which truncate toward zero
     * on this platform — (-7) / 2 == -3 and (-7) % 2 == -1 (the sign of %
     * follows the dividend). The asserted values are the observed plain-C
     * results, not assumptions about a different rounding rule.
     */
    assert(pebble_rt_checked_div_i32(7, 2) == 3);
    assert(pebble_rt_checked_mod_i32(7, 2) == 1);
    assert(pebble_rt_checked_div_i32(-7, 2) == -3);
    assert(pebble_rt_checked_mod_i32(-7, 2) == -1);
    assert(pebble_rt_checked_div_i32(7, -2) == -3);
    assert(pebble_rt_checked_mod_i32(7, -2) == 1);
    assert(pebble_rt_checked_div_i32(-7, -2) == 3);
    assert(pebble_rt_checked_mod_i32(-7, -2) == -1);

    assert(pebble_rt_checked_div_i64(7, 2) == 3);
    assert(pebble_rt_checked_mod_i64(7, 2) == 1);
    assert(pebble_rt_checked_div_i64(-7, 2) == -3);
    assert(pebble_rt_checked_mod_i64(-7, 2) == -1);
    assert(pebble_rt_checked_div_i64(7, -2) == -3);
    assert(pebble_rt_checked_mod_i64(7, -2) == 1);
    assert(pebble_rt_checked_div_i64(-7, -2) == 3);
    assert(pebble_rt_checked_mod_i64(-7, -2) == -1);

    /* INT32_MIN % -1 is mathematically 0 and representable — not a fault in
     * either mode, so this must hold unconditionally. INT32_MIN / -1 is
     * exercised per-mode below (SAFE: panics with overflow, RELEASE: wraps).
     * The i64 twins follow the same contract (INT64_MIN % -1 == 0).
     */
    assert(pebble_rt_checked_mod_i32(INT32_MIN, -1) == 0);
    assert(pebble_rt_checked_mod_i64(INT64_MIN, -1) == 0);
}

/* Division by zero is one case that must abort in EVERY configuration — there
 * is no release-mode answer for it (see pebble_rt.h), so its fork check runs
 * in both modes and its trigger lives outside the SAFE gate below. Both
 * widths are exercised (i32 and i64).
 */
static void trigger_div_by_zero(void) {
    (void)pebble_rt_checked_div_i32(1, 0);
}

static void trigger_div_by_zero_i64(void) {
    (void)pebble_rt_checked_div_i64(1, 0);
}

/* Checked array indexing: in-bounds returns the index unchanged at both
 * widths, and negative or >= length indices panic in EVERY configuration —
 * like division by zero, there is no defined "wrapped" result for an
 * out-of-bounds access, so this is not mode-gated either.
 */
static void test_str_eq(void) {
    PebbleStr a = {(const uint8_t *)"hi", 2};
    PebbleStr b = {(const uint8_t *)"hi", 2};
    PebbleStr c = {(const uint8_t *)"ho", 2};
    PebbleStr shorter = {(const uint8_t *)"h", 1};
    PebbleStr empty1 = {(const uint8_t *)"", 0};
    PebbleStr empty2 = {(const uint8_t *)"", 0};
    assert(pebble_rt_str_eq(a, b) == true);
    assert(pebble_rt_str_eq(a, c) == false);
    assert(pebble_rt_str_eq(a, shorter) == false);
    assert(pebble_rt_str_eq(empty1, empty2) == true);
}

static void test_checked_index_normal(void) {
    assert(pebble_rt_checked_index_i32(0, 3) == 0);
    assert(pebble_rt_checked_index_i32(2, 3) == 2);
    assert(pebble_rt_checked_index_i64(0, 3) == 0);
    assert(pebble_rt_checked_index_i64(2, 3) == 2);
}

static void trigger_index_too_high(void) {
    (void)pebble_rt_checked_index_i32(3, 3);
}

static void trigger_index_negative(void) {
    (void)pebble_rt_checked_index_i32(-1, 3);
}

static void trigger_index_too_high_i64(void) {
    (void)pebble_rt_checked_index_i64(3, 3);
}

static void trigger_index_negative_i64(void) {
    (void)pebble_rt_checked_index_i64(-1, 3);
}

/* Checked optional unwrap: a present optional returns its payload unchanged
 * at every payload type; an absent optional panics in EVERY configuration —
 * same reasoning as division by zero and array bounds above.
 */
static void test_checked_unwrap_normal(void) {
    assert(pebble_rt_checked_unwrap_i32(true, 42) == 42);
    assert(pebble_rt_checked_unwrap_i64(true, 42) == 42);
    assert(pebble_rt_checked_unwrap_bool(true, true) == true);
    assert(pebble_rt_checked_unwrap_bool(true, false) == false);
}

static void trigger_unwrap_absent_i32(void) {
    (void)pebble_rt_checked_unwrap_i32(false, 0);
}

static void trigger_unwrap_absent_i64(void) {
    (void)pebble_rt_checked_unwrap_i64(false, 0);
}

static void trigger_unwrap_absent_bool(void) {
    (void)pebble_rt_checked_unwrap_bool(false, false);
}

/* The overflow-panic fork checks are SAFE-mode-only: in RELEASE mode the
 * same operations wrap instead of panicking, so a forked child would not
 * terminate abnormally and the check would (correctly, per mode) fail.
 */
#if defined(PEBBLE_RT_MODE_SAFE)

/* Triggers for the forked overflow checks: each performs one operation that
 * provably overflows i32 (or i64, the *_i64 triggers). In SAFE mode the
 * helper must panic before the value can be observed.
 */
static void trigger_add_overflow(void) {
    (void)pebble_rt_checked_add_i32(INT32_MAX, 1);
}

static void trigger_neg_overflow(void) {
    (void)pebble_rt_checked_neg_i32(INT32_MIN);
}

static void trigger_div_overflow(void) {
    (void)pebble_rt_checked_div_i32(INT32_MIN, -1);
}

static void trigger_add_overflow_i64(void) {
    (void)pebble_rt_checked_add_i64(INT64_MAX, 1);
}

static void trigger_neg_overflow_i64(void) {
    (void)pebble_rt_checked_neg_i64(INT64_MIN);
}

static void trigger_div_overflow_i64(void) {
    (void)pebble_rt_checked_div_i64(INT64_MIN, -1);
}

#endif /* PEBBLE_RT_MODE_SAFE */

/* Same shape as verify_panic_aborts: fork a child, capture its stderr
 * through a pipe, and confirm it terminated abnormally (abort() raises
 * SIGABRT; a non-zero exit is also accepted). The child runs the given
 * trigger; the operation it performs must call pebble_rt_panic, which never
 * returns, so the child must never reach its own _exit(2).
 */
static int verify_checked_overflow_panics(const char *what, void (*trigger)(void)) {
    int fds[2];
    if (pipe(fds) != 0) {
        fprintf(stderr, "smoke_test: pipe() failed\n");
        return 1;
    }

    /* Flush so the forked child does not inherit this process's buffered
     * stdout (fully buffered when piped) and duplicate it on abort().
     */
    fflush(stdout);

    pid_t pid = fork();
    if (pid < 0) {
        fprintf(stderr, "smoke_test: fork() failed\n");
        return 1;
    }

    if (pid == 0) {
        /* Child: send stderr to the pipe, then trigger the fault. Never
         * reaches the _exit(2) below: the operation must have panicked.
         */
        close(fds[0]);
        dup2(fds[1], STDERR_FILENO);
        close(fds[1]);

        (void)trigger();
        /* Unreachable: the fault must have panicked. */
        _exit(2);
    }

    /* Parent. */
    close(fds[1]);

    char buf[4096];
    size_t got = 0;
    for (;;) {
        ssize_t r = read(fds[0], buf + got, sizeof(buf) - got);
        if (r <= 0) {
            break;
        }
        got += (size_t)r;
        if (got >= sizeof(buf)) {
            break;
        }
    }
    close(fds[0]);

    int status = 0;
    if (waitpid(pid, &status, 0) < 0) {
        fprintf(stderr, "smoke_test: waitpid() failed\n");
        return 1;
    }

    if (got == 0) {
        fprintf(stderr, "smoke_test: %s child printed nothing to stderr\n", what);
        return 1;
    }

    /* abort() raises SIGABRT; accept either that or a non-zero exit as
     * proof the process terminated abnormally.
     */
    int aborted = WIFSIGNALED(status) && WTERMSIG(status) == SIGABRT;
    int nonzero_exit = WIFEXITED(status) && WEXITSTATUS(status) != 0;
    if (!aborted && !nonzero_exit) {
        fprintf(stderr, "smoke_test: %s child did not terminate abnormally\n", what);
        return 1;
    }

    return 0;
}

/* Returns 0 if the forked child provably panicked (aborted), non-zero on
 * failure. The child's stderr is captured through a pipe so we can also
 * verify it printed something.
 */
static int verify_panic_aborts(void) {
    int fds[2];
    if (pipe(fds) != 0) {
        fprintf(stderr, "smoke_test: pipe() failed\n");
        return 1;
    }

    /* Flush so the forked child does not inherit this process's buffered
     * stdout (fully buffered when piped) and duplicate it on abort().
     */
    fflush(stdout);

    pid_t pid = fork();
    if (pid < 0) {
        fprintf(stderr, "smoke_test: fork() failed\n");
        return 1;
    }

    if (pid == 0) {
        /* Child: send stderr to the pipe, then panic. Never returns. */
        close(fds[0]);
        dup2(fds[1], STDERR_FILENO);
        close(fds[1]);

        PebblePanicInfo info;
        info.kind = PEBBLE_PANIC_UNWRAP_FAILED;
        info.message = "forced unwrap of a None optional";
        info.file = __FILE__;
        info.line = __LINE__;
        pebble_rt_panic(&info);
        /* Unreachable, but be explicit in case a broken build returns. */
        _exit(2);
    }

    /* Parent. */
    close(fds[1]);

    char buf[4096];
    size_t got = 0;
    for (;;) {
        ssize_t r = read(fds[0], buf + got, sizeof(buf) - got);
        if (r <= 0) {
            break;
        }
        got += (size_t)r;
        if (got >= sizeof(buf)) {
            break;
        }
    }
    close(fds[0]);

    int status = 0;
    if (waitpid(pid, &status, 0) < 0) {
        fprintf(stderr, "smoke_test: waitpid() failed\n");
        return 1;
    }

    if (got == 0) {
        fprintf(stderr, "smoke_test: panic child printed nothing to stderr\n");
        return 1;
    }

    /* abort() raises SIGABRT; accept either that or a non-zero exit as
     * proof the process terminated abnormally.
     */
    int aborted = WIFSIGNALED(status) && WTERMSIG(status) == SIGABRT;
    int nonzero_exit = WIFEXITED(status) && WEXITSTATUS(status) != 0;
    if (!aborted && !nonzero_exit) {
        fprintf(stderr, "smoke_test: panic child did not terminate abnormally\n");
        return 1;
    }

    return 0;
}

int main(void) {
    test_context_and_allocator();
    printf("ok: context + zeroing allocator\n");

    test_args_from_argv();
    printf("ok: args_from_argv\n");

    test_str_eq();
    printf("ok: str equality\n");

    if (verify_panic_aborts() != 0) {
        fprintf(stderr, "smoke_test: panic subprocess check FAILED\n");
        return 1;
    }
    printf("ok: panic aborts in subprocess\n");

    test_checked_arithmetic_normal();
    printf("ok: checked arithmetic normal results\n");

    /* Division by zero panics in EVERY configuration — including RELEASE —
     * because there is no defined quotient to return. This check is outside
     * the mode gate so both builds exercise it, at both widths.
     */
    if (verify_checked_overflow_panics("i32 division by zero", trigger_div_by_zero) != 0) {
        fprintf(stderr, "smoke_test: checked div by zero subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i64 division by zero", trigger_div_by_zero_i64) != 0) {
        fprintf(stderr, "smoke_test: checked i64 div by zero subprocess check FAILED\n");
        return 1;
    }
    printf("ok: division by zero panics in subprocess\n");

    test_checked_index_normal();
    printf("ok: checked index normal results\n");

    /* Out-of-bounds indexing panics in EVERY configuration, both widths, both
     * directions (too high and negative) — same reasoning as division by
     * zero above.
     */
    if (verify_checked_overflow_panics("i32 index too high", trigger_index_too_high) != 0) {
        fprintf(stderr, "smoke_test: checked index too-high subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i32 index negative", trigger_index_negative) != 0) {
        fprintf(stderr, "smoke_test: checked index negative subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i64 index too high", trigger_index_too_high_i64) != 0) {
        fprintf(stderr, "smoke_test: checked i64 index too-high subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i64 index negative", trigger_index_negative_i64) != 0) {
        fprintf(stderr, "smoke_test: checked i64 index negative subprocess check FAILED\n");
        return 1;
    }
    printf("ok: out-of-bounds indexing panics in subprocess\n");

    test_checked_unwrap_normal();
    printf("ok: checked unwrap normal results\n");

    /* Unwrapping an absent optional panics in EVERY configuration, all three
     * payload types — same reasoning as division by zero and array bounds.
     */
    if (verify_checked_overflow_panics("i32 unwrap of absent optional", trigger_unwrap_absent_i32) != 0) {
        fprintf(stderr, "smoke_test: checked i32 unwrap subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i64 unwrap of absent optional", trigger_unwrap_absent_i64) != 0) {
        fprintf(stderr, "smoke_test: checked i64 unwrap subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("bool unwrap of absent optional", trigger_unwrap_absent_bool) != 0) {
        fprintf(stderr, "smoke_test: checked bool unwrap subprocess check FAILED\n");
        return 1;
    }
    printf("ok: unwrap of absent optional panics in subprocess\n");

#if defined(PEBBLE_RT_MODE_SAFE)
    /* Overflow must panic through pebble_rt_panic, verified in a forked
     * child the same way the direct-panic check above is. Each i32 check
     * has an i64 twin proving the wider-width overflow story is real.
     */
    if (verify_checked_overflow_panics("i32 addition overflow", trigger_add_overflow) != 0) {
        fprintf(stderr, "smoke_test: checked add overflow subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i32 negation overflow", trigger_neg_overflow) != 0) {
        fprintf(stderr, "smoke_test: checked neg overflow subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i32 division overflow", trigger_div_overflow) != 0) {
        fprintf(stderr, "smoke_test: checked div overflow subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i64 addition overflow", trigger_add_overflow_i64) != 0) {
        fprintf(stderr, "smoke_test: checked i64 add overflow subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i64 negation overflow", trigger_neg_overflow_i64) != 0) {
        fprintf(stderr, "smoke_test: checked i64 neg overflow subprocess check FAILED\n");
        return 1;
    }
    if (verify_checked_overflow_panics("i64 division overflow", trigger_div_overflow_i64) != 0) {
        fprintf(stderr, "smoke_test: checked i64 div overflow subprocess check FAILED\n");
        return 1;
    }
    printf("ok: checked arithmetic overflow panics in subprocess\n");
#else
    /* RELEASE: overflow wraps to the operation's two's-complement bit
     * pattern instead of panicking. Assert the exact wrapped values, not
     * merely "did not crash", at both widths.
     */
    if (pebble_rt_checked_add_i32(INT32_MAX, 1) != INT32_MIN) {
        fprintf(stderr, "smoke_test: checked add did not wrap to INT32_MIN in RELEASE\n");
        return 1;
    }
    if (pebble_rt_checked_neg_i32(INT32_MIN) != INT32_MIN) {
        fprintf(stderr, "smoke_test: checked neg did not wrap to INT32_MIN in RELEASE\n");
        return 1;
    }
    if (pebble_rt_checked_div_i32(INT32_MIN, -1) != INT32_MIN) {
        fprintf(stderr, "smoke_test: checked div did not wrap to INT32_MIN in RELEASE\n");
        return 1;
    }
    if (pebble_rt_checked_add_i64(INT64_MAX, 1) != INT64_MIN) {
        fprintf(stderr, "smoke_test: checked i64 add did not wrap to INT64_MIN in RELEASE\n");
        return 1;
    }
    if (pebble_rt_checked_neg_i64(INT64_MIN) != INT64_MIN) {
        fprintf(stderr, "smoke_test: checked i64 neg did not wrap to INT64_MIN in RELEASE\n");
        return 1;
    }
    if (pebble_rt_checked_div_i64(INT64_MIN, -1) != INT64_MIN) {
        fprintf(stderr, "smoke_test: checked i64 div did not wrap to INT64_MIN in RELEASE\n");
        return 1;
    }
    printf("ok: checked arithmetic wraps in RELEASE\n");
#endif

    printf("smoke_test: all checks passed\n");
    return 0;
}
