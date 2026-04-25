#ifndef SOURCE_SPAN_H
#define SOURCE_SPAN_H

#include <stddef.h>

typedef struct {
  const char *file;   // file path
  size_t start_line;  // 1-based
  size_t start_col;   // 1-based
  size_t end_line;    // 1-based
  size_t end_col;     // 1-based
} SourceSpan;

static inline SourceSpan span_new(const char *file, size_t start_line,
                                  size_t start_col, size_t end_line,
                                  size_t end_col) {
  SourceSpan s;
  s.file = file;
  s.start_line = start_line;
  s.start_col = start_col;
  s.end_line = end_line;
  s.end_col = end_col;
  return s;
}

static inline SourceSpan span_combine(SourceSpan a, SourceSpan b) {
  size_t start_line, start_col, end_line, end_col;

  if (a.start_line < b.start_line) {
    start_line = a.start_line;
    start_col = a.start_col;
  } else if (a.start_line > b.start_line) {
    start_line = b.start_line;
    start_col = b.start_col;
  } else {
    start_line = a.start_line;
    start_col = (a.start_col <= b.start_col) ? a.start_col : b.start_col;
  }

  if (a.end_line > b.end_line) {
    end_line = a.end_line;
    end_col = a.end_col;
  } else if (a.end_line < b.end_line) {
    end_line = b.end_line;
    end_col = b.end_col;
  } else {
    end_line = a.end_line;
    end_col = (a.end_col >= b.end_col) ? a.end_col : b.end_col;
  }

  return span_new(a.file, start_line, start_col, end_line, end_col);
}

#endif