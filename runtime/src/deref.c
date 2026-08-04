#include "pebble_rt.h"

#include <stddef.h>

/* Checked pointer dereference (see pebble_rt.h). Null checking runs in every
 * configuration, not just PEBBLE_RT_MODE_SAFE: dereferencing NULL is undefined
 * behavior in C with no defined fallback result. loc is the Pebble source
 * location of the dereference expression, threaded into the panic report.
 */
void *pebble_rt_checked_deref_ptr(void *ptr, PebbleSourceLoc loc) {
    if (ptr == NULL) {
        PebblePanicInfo info;
        info.kind = PEBBLE_PANIC_NULL_DEREFERENCE;
        info.message = NULL;
        info.file = loc.file;
        info.line = loc.line;
        info.column = loc.column;
        pebble_rt_panic(&info);
    }
    return ptr;
}
