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

    if (verify_panic_aborts() != 0) {
        fprintf(stderr, "smoke_test: panic subprocess check FAILED\n");
        return 1;
    }
    printf("ok: panic aborts in subprocess\n");

    printf("smoke_test: all checks passed\n");
    return 0;
}
