#ifndef PROLE_DIAG_H
#define PROLE_DIAG_H

#include "prole_span.h"
#include <stdbool.h>
#include <stddef.h>

typedef enum {
  PROLE_DIAG_ERROR,
  PROLE_DIAG_WARNING,
  PROLE_DIAG_NOTE,
} ProleDiagLevel;

typedef struct ProleDiagnostic {
  ProleDiagLevel level;
  char *message;
  bool has_span;
  ProleSpan span;
  const char *source_line;
  size_t error_start;
  size_t error_length;
  struct ProleDiagnostic *next;
} ProleDiagnostic;

typedef struct {
  const char *filename;
  char *source;
  const char **source_lines;
  size_t line_count;
  size_t error_count;
  size_t warning_count;
} ProleDiagnosticContext;

void prole_diagnostics_init(ProleDiagnosticContext *ctx, const char *filename,
                            const char *source);
void prole_diagnostics_free(ProleDiagnosticContext *ctx);

ProleDiagnostic *prole_diag_error(ProleDiagnosticContext *ctx, ProleSpan span,
                                  const char *fmt, ...);
ProleDiagnostic *prole_diag_warning(ProleDiagnosticContext *ctx,
                                    ProleSpan span, const char *fmt, ...);
ProleDiagnostic *prole_diag_error_no_span(ProleDiagnosticContext *ctx,
                                          const char *fmt, ...);
ProleDiagnostic *prole_diag_add_note(ProleDiagnostic *parent,
                                     const char *fmt, ...);
void prole_diag_emit(ProleDiagnostic *diag);

#endif
