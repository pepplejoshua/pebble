#include "pebble_rt.h"

#include <stdint.h>

/* Checked optional force-unwrap (see pebble_rt.h). Panics in every
 * configuration, not just PEBBLE_RT_MODE_SAFE: there is no defined payload
 * to return for an absent optional in either mode. loc is the Pebble source
 * location of the checked expression (see pebble_rt.h), threaded into the
 * panic report.
 */

static void pebble_rt_unwrap_panic(PebbleSourceLoc loc) {
    PebblePanicInfo info;
    info.kind = PEBBLE_PANIC_UNWRAP_FAILED;
    info.message = NULL;
    info.file = loc.file;
    info.line = loc.line;
    info.column = loc.column;
    pebble_rt_panic(&info);
}

int32_t pebble_rt_checked_unwrap_i32(bool has_value, int32_t value, PebbleSourceLoc loc) {
    if (!has_value) {
        pebble_rt_unwrap_panic(loc);
    }
    return value;
}

int64_t pebble_rt_checked_unwrap_i64(bool has_value, int64_t value, PebbleSourceLoc loc) {
    if (!has_value) {
        pebble_rt_unwrap_panic(loc);
    }
    return value;
}

bool pebble_rt_checked_unwrap_bool(bool has_value, bool value, PebbleSourceLoc loc) {
    if (!has_value) {
        pebble_rt_unwrap_panic(loc);
    }
    return value;
}

uint64_t pebble_rt_checked_unwrap_u64(bool has_value, uint64_t value, PebbleSourceLoc loc) {
    if (!has_value) {
        pebble_rt_unwrap_panic(loc);
    }
    return value;
}

void *pebble_rt_checked_unwrap_ptr(bool has_value, void *value, PebbleSourceLoc loc) {
    if (!has_value) {
        pebble_rt_unwrap_panic(loc);
    }
    return value;
}
