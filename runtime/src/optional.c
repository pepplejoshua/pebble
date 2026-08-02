#include "pebble_rt.h"

#include <stdint.h>

/* Checked optional force-unwrap (see pebble_rt.h). Panics in every
 * configuration, not just PEBBLE_RT_MODE_SAFE: there is no defined payload
 * to return for an absent optional in either mode.
 */

static void pebble_rt_unwrap_panic(void) {
    PebblePanicInfo info;
    info.kind = PEBBLE_PANIC_UNWRAP_FAILED;
    info.message = NULL;
    info.file = NULL;
    info.line = 0;
    pebble_rt_panic(&info);
}

int32_t pebble_rt_checked_unwrap_i32(bool has_value, int32_t value) {
    if (!has_value) {
        pebble_rt_unwrap_panic();
    }
    return value;
}

int64_t pebble_rt_checked_unwrap_i64(bool has_value, int64_t value) {
    if (!has_value) {
        pebble_rt_unwrap_panic();
    }
    return value;
}

bool pebble_rt_checked_unwrap_bool(bool has_value, bool value) {
    if (!has_value) {
        pebble_rt_unwrap_panic();
    }
    return value;
}
