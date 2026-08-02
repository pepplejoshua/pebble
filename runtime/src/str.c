#include "pebble_rt.h"

#include <string.h>

/* Byte-for-byte str equality (see pebble_rt.h). */
bool pebble_rt_str_eq(PebbleStr a, PebbleStr b) {
    if (a.len != b.len) {
        return false;
    }
    if (a.len == 0) {
        return true;
    }
    return memcmp(a.data, b.data, a.len) == 0;
}
