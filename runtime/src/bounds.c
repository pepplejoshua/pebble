#include "pebble_rt.h"

#include <stdint.h>

/* Checked fixed-length array indexing (see pebble_rt.h). Bounds checking runs
 * in every configuration, not just PEBBLE_RT_MODE_SAFE: an out-of-bounds C
 * array access is undefined behavior with no defined fallback result, unlike
 * arithmetic overflow's well-defined wraparound in RELEASE mode.
 */

static void pebble_rt_index_panic(void) {
    PebblePanicInfo info;
    info.kind = PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS;
    info.message = NULL;
    info.file = NULL;
    info.line = 0;
    pebble_rt_panic(&info);
}

int32_t pebble_rt_checked_index_i32(int32_t index, int32_t length) {
    if (index < 0 || index >= length) {
        pebble_rt_index_panic();
    }
    return index;
}

int64_t pebble_rt_checked_index_i64(int64_t index, int64_t length) {
    if (index < 0 || index >= length) {
        pebble_rt_index_panic();
    }
    return index;
}
