#include "pebble_rt.h"

#include <stdio.h>
#include <stdlib.h>

/* Maps a PebblePanicKind to a short human-readable label for the panic
 * report. Must match the enum order in pebble_rt.h.
 */
static const char *panic_kind_label(PebblePanicKind kind) {
    switch (kind) {
    case PEBBLE_PANIC_ASSERT:
        return "assertion failed";
    case PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS:
        return "index out of bounds";
    case PEBBLE_PANIC_UNWRAP_FAILED:
        return "unwrap of empty optional";
    case PEBBLE_PANIC_TAG_MISMATCH:
        return "tagged union tag mismatch";
    case PEBBLE_PANIC_ARITHMETIC_OVERFLOW:
        return "arithmetic overflow";
    case PEBBLE_PANIC_DIVIDE_BY_ZERO:
        return "division by zero";
    case PEBBLE_PANIC_NULL_DEREFERENCE:
        return "null pointer dereference";
    case PEBBLE_PANIC_GENERIC:
        return "panic";
    }
    /* Every enum member is handled above; keep the default defensive so a
     * future member added to the header without a case here still gets a
     * useful label instead of garbage.
     */
    return "panic";
}

/* The single hosted panic entry point. Every safety check the compiler
 * emits funnels through here (index bounds, force-unwrap, tagged-union
 * tag mismatch, assertion). This function never returns: the definition
 * is additionally annotated PEBBLE_RT_NORETURN (matching the header
 * declaration) and the final statement is abort(), so the compiler knows
 * every path is terminated and emits no warnings under -Wall -Wextra.
 */
PEBBLE_RT_NORETURN void pebble_rt_panic(const PebblePanicInfo *info) {
    if (info == NULL) {
        /* Defensive: report what we can and abort anyway. Do not
         * dereference a NULL info before anything useful is printed.
         */
        fprintf(stderr, "pebble: panic (no panic info provided)\n");
        abort();
    }

    fprintf(stderr, "pebble: %s", panic_kind_label(info->kind));
    if (info->message != NULL) {
        fprintf(stderr, ": %s", info->message);
    }
    if (info->file != NULL) {
        if (info->line != 0 && info->column != 0) {
            fprintf(stderr, " at %s:%zu:%zu", info->file, info->line, info->column);
        } else if (info->line != 0) {
            fprintf(stderr, " at %s:%zu", info->file, info->line);
        } else {
            fprintf(stderr, " at %s", info->file);
        }
    }
    fprintf(stderr, "\n");
    fflush(stderr);
    abort();
}
