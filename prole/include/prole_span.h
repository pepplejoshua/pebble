#ifndef PROLE_SPAN_H
#define PROLE_SPAN_H

#include <stddef.h>

typedef struct {
  const char *file;
  size_t start_line;
  size_t start_col;
  size_t end_line;
  size_t end_col;
} ProleSpan;

static inline ProleSpan prole_span_new(const char *file, size_t start_line,
                                       size_t start_col, size_t end_line,
                                       size_t end_col) {
  ProleSpan span;
  span.file = file;
  span.start_line = start_line;
  span.start_col = start_col;
  span.end_line = end_line;
  span.end_col = end_col;
  return span;
}

static inline ProleSpan prole_span_combine(ProleSpan a, ProleSpan b) {
  size_t start_line;
  size_t start_col;
  size_t end_line;
  size_t end_col;

  if (a.start_line < b.start_line) {
    start_line = a.start_line;
    start_col = a.start_col;
  } else if (a.start_line > b.start_line) {
    start_line = b.start_line;
    start_col = b.start_col;
  } else {
    start_line = a.start_line;
    start_col = a.start_col <= b.start_col ? a.start_col : b.start_col;
  }

  if (a.end_line > b.end_line) {
    end_line = a.end_line;
    end_col = a.end_col;
  } else if (a.end_line < b.end_line) {
    end_line = b.end_line;
    end_col = b.end_col;
  } else {
    end_line = a.end_line;
    end_col = a.end_col >= b.end_col ? a.end_col : b.end_col;
  }

  return prole_span_new(a.file, start_line, start_col, end_line, end_col);
}

#endif
