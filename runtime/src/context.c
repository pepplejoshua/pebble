#include "pebble_rt.h"

#include <stdlib.h>
#include <string.h>

/* The ABI contract (documented in pebble_rt.h): the default allocator
 * zero-initializes every allocation it returns. malloc alone does not
 * zero, so alloc() is malloc + memset.
 *
 * Size-0 convention: return a valid non-NULL pointer to a zero-length
 * allocation, i.e. treat size 0 as size 1. This mirrors what most real
 * allocators do with malloc(0) (return a unique non-NULL pointer that is
 * safe to free but holds no bytes), which keeps free() uniform — a
 * zero-sized alloc is always a pointer that free() accepts — and keeps
 * generated code that allocates a 0-byte buffer on a valid non-NULL data
 * pointer rather than a NULL one. The pointer is never dereferenced for a
 * zero-length allocation, so the extra byte is harmless.
 */
void *pebble_rt_default_alloc(PebbleContext *ctx, size_t size) {
    (void)ctx;
    size_t alloc_size = size;
    if (alloc_size == 0) {
        alloc_size = 1;
    }
    void *data = malloc(alloc_size);
    if (data == NULL) {
        return NULL;
    }
    /* Zeroing is a documented ABI contract, not an optimization. */
    memset(data, 0, alloc_size);
    return data;
}

/* NOTE on the zeroing asymmetry (deliberate, not an oversight):
 * pebble_rt_default_realloc does NOT zero the newly-grown tail. The ABI
 * zeroing contract applies only to pebble_rt_default_alloc's fresh
 * allocations — realloc's own contract is that existing bytes are
 * preserved and the grown region is unspecified, and restoring that
 * "unspecified" region to a defined-but-uninitialized state (rather than
 * silently zeroing it) is what lets callers rely on realloc semantics
 * (e.g. grow a buffer that already holds initialized data without paying
 * for an extra pass over it). If a caller needs zeroed memory after a
 * grow, it reallocs and zeroes the delta itself.
 */
void *pebble_rt_default_realloc(PebbleContext *ctx, void *ptr, size_t new_size) {
    (void)ctx;
    return realloc(ptr, new_size);
}

void pebble_rt_default_free(PebbleContext *ctx, void *ptr) {
    (void)ctx;
    free(ptr);
}

#ifndef PEBBLE_RT_FREESTANDING
PebbleContext pebble_rt_default_context(void) {
    PebbleContext ctx;
    ctx.allocator.state = NULL;
    ctx.allocator.alloc = pebble_rt_default_alloc;
    ctx.allocator.realloc = pebble_rt_default_realloc;
    ctx.allocator.free = pebble_rt_default_free;
    return ctx;
}
#endif
