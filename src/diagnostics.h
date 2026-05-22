#ifndef DIAGNOSTICS_H
#define DIAGNOSTICS_H

#include "source_span.h"
#include <stdbool.h>
#include <stddef.h>

typedef enum {
    DIAG_ERROR,     // Red - must fix to compile
    DIAG_WARNING,   // Yellow - should fix but compiles
    DIAG_TIP,       // Blue - helpful suggestion
} DiagnosticLevel;

typedef struct Diagnostic {
    DiagnosticLevel level;
    char *message;

    // Span info (optional)
    bool has_span;
    SourceSpan span;
    const char *source_line; // The problematic line
    size_t error_start;      // 0-based column where highlight starts
    size_t error_length;     // Length to highlight

    // Chained diagnostics
    struct Diagnostic *next; // For additional context
} Diagnostic;

typedef struct {
    const char *filename;
    const char *source;
    const char **source_lines;
    size_t line_count;
    size_t error_count;
    size_t warning_count;
} DiagnosticContext;

// Core API
void diagnostics_init(DiagnosticContext *ctx, const char *filename,
                      const char *source);
void diagnostics_free(DiagnosticContext *ctx);

// Diagnostic builders
Diagnostic *diagnostic_error(DiagnosticContext *ctx, SourceSpan span,
                             const char *fmt, ...);
Diagnostic *diagnostic_warning(DiagnosticContext *ctx, SourceSpan span,
                               const char *fmt, ...);
Diagnostic *diagnostic_error_no_loc(DiagnosticContext *ctx, const char *fmt,
                                    ...);
Diagnostic *diagnostic_warning_no_loc(DiagnosticContext *ctx, const char *fmt,
                                      ...);

// Chaining methods
Diagnostic *diagnostic_add_tip(Diagnostic *parent, const char *fmt, ...);

// Output
void diagnostic_emit(Diagnostic *diag);

#endif
