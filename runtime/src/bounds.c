#include "pebble_rt.h"

#include <stdint.h>

/* Checked fixed-length array indexing (see pebble_rt.h). Bounds checking runs
 * in every configuration, not just PEBBLE_RT_MODE_SAFE: an out-of-bounds C
 * array access is undefined behavior with no defined fallback result, unlike
 * arithmetic overflow's well-defined wraparound in RELEASE mode. loc is the
 * Pebble source location of the checked expression (see pebble_rt.h),
 * threaded into the panic report.
 */

static void pebble_rt_index_panic(PebbleSourceLoc loc) {
    PebblePanicInfo info;
    info.kind = PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS;
    info.message = NULL;
    info.file = loc.file;
    info.line = loc.line;
    info.column = loc.column;
    pebble_rt_panic(&info);
}

int32_t pebble_rt_checked_index_i32(int32_t index, int32_t length, PebbleSourceLoc loc) {
    if (index < 0 || index >= length) {
        pebble_rt_index_panic(loc);
    }
    return index;
}

int64_t pebble_rt_checked_index_i64(int64_t index, int64_t length, PebbleSourceLoc loc) {
    if (index < 0 || index >= length) {
        pebble_rt_index_panic(loc);
    }
    return index;
}

int32_t pebble_rt_checked_slice_start_i32(int32_t start, int32_t end, int32_t length, PebbleSourceLoc loc) {
    if (start < 0 || start > end || end > length) {
        pebble_rt_index_panic(loc);
    }
    return start;
}

int64_t pebble_rt_checked_slice_start_i64(int64_t start, int64_t end, int64_t length, PebbleSourceLoc loc) {
    if (start < 0 || start > end || end > length) {
        pebble_rt_index_panic(loc);
    }
    return start;
}
