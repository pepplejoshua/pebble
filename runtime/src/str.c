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

/* Lexicographic byte comparison (see pebble_rt.h). */
int pebble_rt_str_cmp(PebbleStr a, PebbleStr b) {
    size_t shared = a.len < b.len ? a.len : b.len;
    int cmp = shared == 0 ? 0 : memcmp(a.data, b.data, shared);
    if (cmp != 0) {
        return cmp;
    }
    if (a.len < b.len) {
        return -1;
    }
    if (a.len > b.len) {
        return 1;
    }
    return 0;
}
