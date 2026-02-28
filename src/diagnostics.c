#include "diagnostics.h"
// #include "../pastel/pastel.h"
#include "alloc.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void split_source_into_lines(DiagnosticContext *ctx) {
  if (!ctx->source) {
    ctx->source_lines = NULL;
    ctx->line_count = 0;
    return;
  }

  // Alloc 1: Copy source (we'll modify this copy)
  size_t source_len = strlen(ctx->source);
  char *source_copy = arena_alloc(&long_lived, source_len + 1);
  memcpy(source_copy, ctx->source, source_len + 1);

  // Count lines first
  size_t line_count = 1;
  for (const char *p = source_copy; *p; p++) {
    if (*p == '\n')
      line_count++;
  }

  // Alloc 2: Array of line pointers only
  ctx->source_lines = arena_alloc(&long_lived, line_count * sizeof(char *));
  ctx->line_count = 0;

  // Split in-place: replace \n with \0 and track line starts
  char *line_start = source_copy;
  ctx->source_lines[ctx->line_count++] = line_start;

  for (char *p = source_copy; *p; p++) {
    if (*p == '\n') {
      *p = '\0'; // Terminate current line

      // Next line starts after the \n (if not at end)
      if (*(p + 1) != '\0') {
        ctx->source_lines[ctx->line_count++] = p + 1;
      }
    }
  }
}

void diagnostics_init(DiagnosticContext *ctx, const char *filename,
                      const char *source) {
  ctx->filename = str_dup(filename); // Use your arena str_dup
  ctx->source = str_dup(source);
  ctx->error_count = 0;
  ctx->warning_count = 0;

  // Split source into lines for display
  split_source_into_lines(ctx);
}

void diagnostics_free(DiagnosticContext *ctx) {
  // Arena handles cleanup, but reset counters
  ctx->filename = NULL;
  ctx->source = NULL;
  ctx->source_lines = NULL;
  ctx->line_count = 0;
  ctx->error_count = 0;
  ctx->warning_count = 0;
}

Diagnostic *diagnostic_error(DiagnosticContext *ctx, Location loc,
                             const char *fmt, ...) {
  Diagnostic *diag = arena_alloc(&long_lived, sizeof(Diagnostic));

  diag->level = DIAG_ERROR;
  diag->has_location = true;
  diag->location = loc;
  diag->next = NULL;

  // Format message
  va_list args;
  va_start(args, fmt);

  // Calculate needed size
  va_list args_copy;
  va_copy(args_copy, args);
  int needed = vsnprintf(NULL, 0, fmt, args_copy);
  va_end(args_copy);

  // Allocate and format
  diag->message = arena_alloc(&long_lived, needed + 1);
  vsnprintf(diag->message, needed + 1, fmt, args);
  va_end(args);

  // Extract source line if location is valid
  if (loc.line > 0 && loc.line <= ctx->line_count) {
    diag->source_line = ctx->source_lines[loc.line - 1]; // 1-based to 0-based
    diag->error_start = loc.column;
    diag->error_length = 1; // Default highlight length
  } else {
    diag->source_line = NULL;
    diag->error_start = 0;
    diag->error_length = 0;
  }

  ctx->error_count++;
  return diag;
}
