#include "pebble_rt.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h>

static void pebble_rt_str_index_panic(PebbleSourceLoc loc) {
    PebblePanicInfo info;
    info.kind = PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS;
    info.message = NULL;
    info.file = loc.file;
    info.line = loc.line;
    info.column = loc.column;
    pebble_rt_panic(&info);
}

/* Decodes the codepoint at UTF-8 byte offset *byte_pos, advancing *byte_pos
 * past it. Panics on any malformed encoding (an invalid lead byte, an
 * invalid continuation byte, or a sequence truncated by the end of s) —
 * PebbleStr's bytes are not guaranteed to be valid UTF-8, so this is a real
 * fault, not defense against an input the checker already ruled out.
 */
static int32_t pebble_rt_utf8_decode_one(PebbleStr s, size_t *byte_pos, PebbleSourceLoc loc) {
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
        pebble_rt_str_index_panic(loc);
        return 0; /* unreachable: pebble_rt_panic never returns */
    }
    if (*byte_pos + seq_len > s.len) {
        pebble_rt_str_index_panic(loc);
        return 0;
    }
    for (size_t i = 1; i < seq_len; i++) {
        uint8_t cont = s.data[*byte_pos + i];
        if ((cont & 0xC0u) != 0x80u) {
            pebble_rt_str_index_panic(loc);
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
static int32_t pebble_rt_str_char_at(PebbleStr s, int64_t char_index, PebbleSourceLoc loc) {
    if (char_index < 0) {
        pebble_rt_str_index_panic(loc);
    }
    size_t byte_pos = 0;
    int64_t count = 0;
    while (byte_pos < s.len) {
        int32_t cp = pebble_rt_utf8_decode_one(s, &byte_pos, loc);
        if (count == char_index) {
            return cp;
        }
        count++;
    }
    pebble_rt_str_index_panic(loc);
    return 0; /* unreachable */
}

int32_t pebble_rt_str_char_at_i32(PebbleStr s, int32_t index, PebbleSourceLoc loc) {
    return pebble_rt_str_char_at(s, (int64_t)index, loc);
}

int32_t pebble_rt_str_char_at_i64(PebbleStr s, int64_t index, PebbleSourceLoc loc) {
    return pebble_rt_str_char_at(s, index, loc);
}

/* The u64 variant of the str char-at index: the same contract at the
 * unsigned width. Its one structural difference from the shared signed
 * implementation is that an unsigned index can never be negative, so the
 * char_index < 0 branch needs no equivalent — only an index past the last
 * codepoint (or a malformed encoding) can fail, and the count is tracked in
 * uint64_t so a huge index compares correctly.
 */
int32_t pebble_rt_str_char_at_u64(PebbleStr s, uint64_t index, PebbleSourceLoc loc) {
    size_t byte_pos = 0;
    uint64_t count = 0;
    while (byte_pos < s.len) {
        int32_t cp = pebble_rt_utf8_decode_one(s, &byte_pos, loc);
        if (count == index) {
            return cp;
        }
        count++;
    }
    pebble_rt_str_index_panic(loc);
    return 0; /* unreachable */
}

/* Constructs a PebbleStr from a NUL-terminated C string (see pebble_rt.h).
 * Computes the length via strlen. A NULL input produces an empty PebbleStr
 * ({ .data = NULL, .len = 0 }) rather than dereferencing NULL via strlen.
 */
PebbleStr pebble_rt_str_from_cstr(const char *s) {
    if (s == NULL) {
        PebbleStr empty;
        empty.data = NULL;
        empty.len = 0;
        return empty;
    }
    PebbleStr result;
    result.data = (const uint8_t *)s;
    result.len = strlen(s);
    return result;
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

/* Read one raw byte from a str at a byte offset (see pebble_rt.h). Unlike
 * pebble_rt_str_char_at which walks UTF-8 codepoints, this reads the exact
 * byte at byte_index — useful for inspecting the raw UTF-8 encoding of a
 * string. In SAFE mode it checks byte_index < s.len and panics with
 * PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS on violation, using the injected
 * PebbleSourceLoc so the panic report names the exact call site. In RELEASE
 * mode the check is omitted and data[byte_index] is read directly.
 */
uint8_t pebble_rt_str_byte_at(PebbleStr s, uint64_t byte_index, PebbleSourceLoc loc) {
    (void)loc;
#if PEBBLE_RT_CHECKS_ENABLED
    if (byte_index >= s.len) {
        pebble_rt_str_index_panic(loc);
    }
#endif
    return s.data[byte_index];
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

/* Encodes one Unicode scalar as UTF-8 (see pebble_rt.h). Precondition: the
 * scalar is a valid Unicode scalar value, which the language guarantees for
 * every `char`, so no validation and no panic path is needed. Writes the
 * trailing NUL and returns the encoded byte count (1-4). Mode-independent.
 */
size_t pebble_rt_char_to_utf8(int32_t scalar, uint8_t out[5]) {
    if (scalar < 0x80) {
        out[0] = (uint8_t)scalar;
        out[1] = 0x00;
        return 1;
    }
    if (scalar < 0x800) {
        out[0] = (uint8_t)(0xC0 | (scalar >> 6));
        out[1] = (uint8_t)(0x80 | (scalar & 0x3F));
        out[2] = 0x00;
        return 2;
    }
    if (scalar < 0x10000) {
        out[0] = (uint8_t)(0xE0 | (scalar >> 12));
        out[1] = (uint8_t)(0x80 | ((scalar >> 6) & 0x3F));
        out[2] = (uint8_t)(0x80 | (scalar & 0x3F));
        out[3] = 0x00;
        return 3;
    }
    out[0] = (uint8_t)(0xF0 | (scalar >> 18));
    out[1] = (uint8_t)(0x80 | ((scalar >> 12) & 0x3F));
    out[2] = (uint8_t)(0x80 | ((scalar >> 6) & 0x3F));
    out[3] = (uint8_t)(0x80 | (scalar & 0x3F));
    out[4] = 0x00;
    return 4;
}

/* Materialize an interpolated string from parts (see pebble_rt.h). A single
 * measure+format pass computes each part's contribution to the total byte
 * length — text parts contribute strlen, bool parts 4 for "true" or 5 for
 * "false", integer parts the length of their decimal representation, float
 * parts the length of their %f rendering, str parts the str's own .len, and
 * char parts the byte length of their UTF-8 encoding — and formats each
 * non-text/non-str part ONCE into its own fixed scratch buffer (20 decimal
 * digits fit the widest uint64_t, a leading '-' fits a negative signed value,
 * plus the NUL; and a full-range double's %f output needs at most 317
 * characters — a leading '-' plus the 309 integer digits of DBL_MAX plus '.'
 * plus the 6 default-precision fractional digits — plus the NUL, so 320 covers
 * every value any field can hold, a char's 1-4 encoded bytes far inside it;
 * the int, float, and char widths share one block, each part's 320-byte slot
 * far exceeding what its own kind writes). The scratch buffers are allocated
 * through the context allocator (a `count`-wide `char[count][320]` block, the
 * ABI's only allocation path — a stack VLA would be C11-optional and undefined
 * at count 0). The second pass allocates exactly total_len bytes via the
 * context allocator and copies each part's bytes in sequence, integer, float,
 * and char parts from their already-encoded scratch buffer, str parts directly
 * from the str's own data — so no value is ever formatted twice, and the length
 * each part contributes in the measure pass and the bytes copied in the write
 * pass come from the same source and cannot disagree (snprintf never truncates
 * into a 320-byte slot, so its return value equals the bytes actually written;
 * pebble_rt_char_to_utf8 returns the same byte count its write pass relies on).
 */
#define PEBBLE_STR_PART_SCRATCH_SIZE 320

PebbleStr pebble_rt_str_from_parts(PebbleContext *ctx, const PebbleStrPart *parts, size_t count) {
    char (*scratch_bufs)[PEBBLE_STR_PART_SCRATCH_SIZE] = NULL;
    if (count > 0) {
        scratch_bufs = (char (*)[PEBBLE_STR_PART_SCRATCH_SIZE])ctx->allocator.alloc(ctx, count * sizeof(char[PEBBLE_STR_PART_SCRATCH_SIZE]));
        if (scratch_bufs == NULL) {
            return (PebbleStr){ NULL, 0 };
        }
    }
    size_t total_len = 0;
    for (size_t i = 0; i < count; i++) {
        switch (parts[i].kind) {
        case PEBBLE_STR_PART_TEXT:
            total_len += strlen(parts[i].text);
            break;
        case PEBBLE_STR_PART_BOOL:
            /* "true" = 4 chars, "false" = 5 chars — must match the write paths below */
            total_len += parts[i].bool_value ? 4 : 5;
            break;
        case PEBBLE_STR_PART_INT:
            total_len += (size_t)snprintf(scratch_bufs[i], PEBBLE_STR_PART_SCRATCH_SIZE, "%lld", (long long)parts[i].int_value);
            break;
        case PEBBLE_STR_PART_UINT:
            total_len += (size_t)snprintf(scratch_bufs[i], PEBBLE_STR_PART_SCRATCH_SIZE, "%llu", (unsigned long long)parts[i].uint_value);
            break;
        case PEBBLE_STR_PART_FLOAT:
            /* %f — default precision, 6 decimal digits — the exact convention
             * buildPrint's own scalar float print path uses, so an
             * interpolated float renders byte-for-byte identically to the same
             * float passed straight to print. f32/f64 both promote to double
             * here, so the one specifier covers both. */
            total_len += (size_t)snprintf(scratch_bufs[i], PEBBLE_STR_PART_SCRATCH_SIZE, "%f", parts[i].float_value);
            break;
        case PEBBLE_STR_PART_STR:
            /* A str part contributes its own .len bytes — no formatting,
             * no snprintf, just a direct copy of the str's existing data. */
            total_len += parts[i].str_value.len;
            break;
        case PEBBLE_STR_PART_CHAR:
            /* A char part encodes its Unicode scalar to UTF-8 ONCE, into its
             * scratch slot (pebble_rt_char_to_utf8 writes 1-4 bytes plus a
             * trailing NUL), and the returned byte count is its contribution
             * to the total length — the same length the write pass copies
             * back out of the same slot, so the two passes cannot disagree. */
            total_len += pebble_rt_char_to_utf8(parts[i].char_value, (uint8_t *)scratch_bufs[i]);
            break;
        }
    }
    uint8_t *buf = (uint8_t *)ctx->allocator.alloc(ctx, total_len);
    if (buf == NULL) {
        if (scratch_bufs != NULL) {
            ctx->allocator.free(ctx, scratch_bufs);
        }
        return (PebbleStr){ NULL, 0 };
    }
    size_t offset = 0;
    for (size_t i = 0; i < count; i++) {
        switch (parts[i].kind) {
        case PEBBLE_STR_PART_TEXT: {
            size_t len = strlen(parts[i].text);
            memcpy(buf + offset, parts[i].text, len);
            offset += len;
            break;
        }
        case PEBBLE_STR_PART_BOOL:
            if (parts[i].bool_value) {
                buf[offset++] = 't';
                buf[offset++] = 'r';
                buf[offset++] = 'u';
                buf[offset++] = 'e';
            } else {
                buf[offset++] = 'f';
                buf[offset++] = 'a';
                buf[offset++] = 'l';
                buf[offset++] = 's';
                buf[offset++] = 'e';
            }
            break;
        case PEBBLE_STR_PART_CHAR: {
            /* A char part encodes to 1-4 bytes of UTF-8 (or a single 0x00
             * for Unicode scalar value 0). Re-encoding directly into the
             * destination avoids relying on strlen, which would return 0
             * when the first encoded byte is 0x00 and silently drop the
             * character. pebble_rt_char_to_utf8 writes up to 5 bytes total
             * (4 encoded + trailing NUL), but we only copy the exact number
             * returned — the NUL terminator stays un-copied since buf is
             * not a NUL-terminated string. */
            size_t len = pebble_rt_char_to_utf8(parts[i].char_value, buf + offset);
            offset += len;
            break;
        }
        case PEBBLE_STR_PART_INT:
        case PEBBLE_STR_PART_UINT:
        case PEBBLE_STR_PART_FLOAT: {
            /* The int/float parts were snprintf'd into their scratch slots
             * during the measure pass, each NUL-terminated; copy those
             * bytes verbatim, so the written length always equals the
             * measured length. Numeric/float output can never contain an
             * embedded NUL before the terminating NUL, so strlen is safe. */
            size_t len = strlen(scratch_bufs[i]);
            memcpy(buf + offset, scratch_bufs[i], len);
            offset += len;
            break;
        }
        case PEBBLE_STR_PART_STR: {
            memcpy(buf + offset, parts[i].str_value.data, parts[i].str_value.len);
            offset += parts[i].str_value.len;
            break;
        }
        }
    }
    if (scratch_bufs != NULL) {
        ctx->allocator.free(ctx, scratch_bufs);
    }
    PebbleStr result;
    result.data = buf;
    result.len = total_len;
    return result;
}
