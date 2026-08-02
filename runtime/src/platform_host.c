#include "pebble_rt.h"

#include <string.h>

#ifndef PEBBLE_RT_FREESTANDING

/* Adapts the host's argc/argv into a slice of PebbleStr. The returned
 * slice's backing array is allocated via ctx->allocator.alloc (never
 * malloc directly — allocation goes through the context, per the ABI)
 * and is owned by the caller; this function neither frees it nor retains
 * it.
 *
 * strlen() is the one legitimate use of strlen in the runtime: C's own
 * argv entries are NUL-terminated by the OS/libc contract, so measuring
 * them with strlen is exact. PebbleStr itself is length-prefixed and the
 * ABI never relies on NUL termination for user-facing Pebble strings.
 */
PebbleStrSlice pebble_rt_args_from_argv(PebbleContext *ctx, int argc,
                                        const char **argv) {
    if (argc <= 0) {
        /* No arguments: return a valid zero-length slice with a NULL data
         * pointer, nothing allocated, nothing to leak.
         */
        PebbleStrSlice slice;
        slice.data = NULL;
        slice.len = 0;
        return slice;
    }

    PebbleStr *strs = (PebbleStr *)ctx->allocator.alloc(
        ctx, (size_t)argc * sizeof(PebbleStr));
    if (strs == NULL) {
        PebbleStrSlice empty;
        empty.data = NULL;
        empty.len = 0;
        return empty;
    }

    /* Note: alloc zero-initializes, so every PebbleStr starts as
     * { NULL, 0 } and we only overwrite the entries we fill.
     */
    for (int i = 0; i < argc; i++) {
        strs[i].data = (const uint8_t *)argv[i];
        strs[i].len = strlen(argv[i]);
    }

    PebbleStrSlice slice;
    slice.data = strs;
    slice.len = (size_t)argc;
    return slice;
}

#endif /* !PEBBLE_RT_FREESTANDING */
