#include "pebble_rt.h"

#include <stdint.h>
#include <string.h>

static void pebble_rt_str_index_panic(void) {
    PebblePanicInfo info;
    info.kind = PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS;
    info.message = NULL;
    info.file = NULL;
    info.line = 0;
    pebble_rt_panic(&info);
}

/* Decodes the codepoint at UTF-8 byte offset *byte_pos, advancing *byte_pos
 * past it. Panics on any malformed encoding (an invalid lead byte, an
 * invalid continuation byte, or a sequence truncated by the end of s) —
 * PebbleStr's bytes are not guaranteed to be valid UTF-8, so this is a real
 * fault, not defense against an input the checker already ruled out.
 */
static int32_t pebble_rt_utf8_decode_one(PebbleStr s, size_t *byte_pos) {
    uint8_t lead = s.data[*byte_pos];
    size_t seq_len;
    int32_t cp;
    if ((lead & 0x80u) == 0x00u) {
        seq_len = 1;
        cp = (int32_t)lead;
    } else if ((lead & 0xE0u) == 0xC0u) {
        seq_len = 2;
        cp = (int32_t)(lead & 0x1Fu);
    } else if ((lead & 0xF0u) == 0xE0u) {
        seq_len = 3;
        cp = (int32_t)(lead & 0x0Fu);
    } else if ((lead & 0xF8u) == 0xF0u) {
        seq_len = 4;
        cp = (int32_t)(lead & 0x07u);
    } else {
        pebble_rt_str_index_panic();
        return 0; /* unreachable: pebble_rt_panic never returns */
    }
    if (*byte_pos + seq_len > s.len) {
        pebble_rt_str_index_panic();
        return 0;
    }
    for (size_t i = 1; i < seq_len; i++) {
        uint8_t cont = s.data[*byte_pos + i];
        if ((cont & 0xC0u) != 0x80u) {
            pebble_rt_str_index_panic();
            return 0;
        }
        cp = (cp << 6) | (int32_t)(cont & 0x3Fu);
    }
    *byte_pos += seq_len;
    return cp;
}

/* Shared implementation for both index widths (see pebble_rt.h). Walks
 * decoded codepoints from the start of s until reaching char_index,
 * panicking if char_index is negative or the string has fewer than
 * char_index + 1 codepoints (or is malformed along the way).
 */
static int32_t pebble_rt_str_char_at(PebbleStr s, int64_t char_index) {
    if (char_index < 0) {
        pebble_rt_str_index_panic();
    }
    size_t byte_pos = 0;
    int64_t count = 0;
    while (byte_pos < s.len) {
        int32_t cp = pebble_rt_utf8_decode_one(s, &byte_pos);
        if (count == char_index) {
            return cp;
        }
        count++;
    }
    pebble_rt_str_index_panic();
    return 0; /* unreachable */
}

int32_t pebble_rt_str_char_at_i32(PebbleStr s, int32_t index) {
    return pebble_rt_str_char_at(s, (int64_t)index);
}

int32_t pebble_rt_str_char_at_i64(PebbleStr s, int64_t index) {
    return pebble_rt_str_char_at(s, index);
}

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
