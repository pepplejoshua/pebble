#include "diagnostics.h"
#include "../pastel/pastel.h"
#include "alloc.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define PATH_MAX 4096

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
  ctx->filename = str_dup(filename);
  ctx->source = str_dup(source);
  ctx->error_count = 0;
  ctx->warning_count = 0;
  split_source_into_lines(ctx);
}

void diagnostics_free(DiagnosticContext *ctx) {
  ctx->filename = NULL;
  ctx->source = NULL;
  ctx->source_lines = NULL;
  ctx->line_count = 0;
  ctx->error_count = 0;
  ctx->warning_count = 0;
}

static Diagnostic *diagnostic_build_with_span(DiagnosticContext *ctx,
                                              DiagnosticLevel level,
                                              SourceSpan span, const char *fmt,
                                              va_list args) {
  Diagnostic *diag = arena_alloc(&long_lived, sizeof(Diagnostic));
  memset(diag, 0, sizeof(Diagnostic));

  diag->level = level;
  diag->has_span = true;
  diag->span = span;
  diag->next = NULL;

  va_list args_copy;
  va_copy(args_copy, args);
  int needed = vsnprintf(NULL, 0, fmt, args_copy);
  va_end(args_copy);

  diag->message = arena_alloc(&long_lived, (size_t)needed + 1);
  vsnprintf(diag->message, (size_t)needed + 1, fmt, args);

  size_t line = span.start_line;
  if (line > 0 && line <= ctx->line_count) {
    diag->source_line = ctx->source_lines[line - 1];

    size_t start_col = span.start_col > 0 ? span.start_col : 1;
    size_t end_col = span.end_col > 0 ? span.end_col : start_col + 1;

    if (end_col < start_col) {
      end_col = start_col;
    }

    diag->error_start = start_col - 1;
    diag->error_length = end_col > start_col ? (end_col - start_col) : 1;
  } else {
    diag->source_line = NULL;
    diag->error_start = 0;
    diag->error_length = 0;
  }

  if (level == DIAG_ERROR) {
    ctx->error_count++;
  } else if (level == DIAG_WARNING) {
    ctx->warning_count++;
  }

  return diag;
}

static Diagnostic *diagnostic_build_no_span(DiagnosticContext *ctx,
                                            DiagnosticLevel level,
                                            const char *fmt, va_list args) {
  (void)ctx;

  Diagnostic *diag = arena_alloc(&long_lived, sizeof(Diagnostic));
  memset(diag, 0, sizeof(Diagnostic));

  diag->level = level;
  diag->has_span = false;
  diag->next = NULL;

  va_list args_copy;
  va_copy(args_copy, args);
  int needed = vsnprintf(NULL, 0, fmt, args_copy);
  va_end(args_copy);

  diag->message = arena_alloc(&long_lived, (size_t)needed + 1);
  vsnprintf(diag->message, (size_t)needed + 1, fmt, args);

  diag->source_line = NULL;
  diag->error_start = 0;
  diag->error_length = 0;

  return diag;
}

Diagnostic *diagnostic_error(DiagnosticContext *ctx, SourceSpan span,
                             const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  Diagnostic *diag =
      diagnostic_build_with_span(ctx, DIAG_ERROR, span, fmt, args);
  va_end(args);
  return diag;
}

Diagnostic *diagnostic_warning(DiagnosticContext *ctx, SourceSpan span,
                               const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  Diagnostic *diag =
      diagnostic_build_with_span(ctx, DIAG_WARNING, span, fmt, args);
  va_end(args);
  return diag;
}

Diagnostic *diagnostic_error_no_loc(DiagnosticContext *ctx, const char *fmt,
                                    ...) {
  va_list args;
  va_start(args, fmt);
  Diagnostic *diag = diagnostic_build_no_span(ctx, DIAG_ERROR, fmt, args);
  va_end(args);
  ctx->error_count++;
  return diag;
}

Diagnostic *diagnostic_warning_no_loc(DiagnosticContext *ctx, const char *fmt,
                                      ...) {
  va_list args;
  va_start(args, fmt);
  Diagnostic *diag = diagnostic_build_no_span(ctx, DIAG_WARNING, fmt, args);
  va_end(args);
  ctx->warning_count++;
  return diag;
}

Diagnostic *diagnostic_add_tip(Diagnostic *parent, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  Diagnostic *tip = diagnostic_build_no_span(NULL, DIAG_TIP, fmt, args);
  va_end(args);

  Diagnostic *current = parent;
  while (current->next != NULL) {
    current = current->next;
  }
  current->next = tip;

  return parent;
}

static const char *get_last_segments(const char *path, int n) {
  if (!path || n <= 0)
    return path;

  const char *segments[10];
  int count = 0;

  const char *current = path + strlen(path);
  const char *segment_end = current;

  while (current > path && count < n && count < 10) {
    current--;
    if (*current == '/' || current == path) {
      const char *segment_start = (*current == '/') ? current + 1 : current;
      if (segment_end - segment_start > 0) {
        segments[count++] = segment_start;
      }
      segment_end = current;
    }
  }

  if (count == 0)
    return path;

  static char result[PATH_MAX];
  result[0] = '\0';

  for (int i = count - 1; i >= 0; i--) {
    if (strlen(result) > 0)
      strcat(result, "/");

    const char *start = segments[i];
    const char *end = strchr(start, '/');
    if (!end)
      end = start + strlen(start);

    size_t seg_len = (size_t)(end - start);
    strncat(result, start, seg_len);
  }

  return result;
}

static const char *get_display_path(const char *abs_path) {
  if (!abs_path)
    return "";

  char cwd[PATH_MAX];
  if (getcwd(cwd, sizeof(cwd)) != NULL) {
    size_t cwd_len = strlen(cwd);
    if (strncmp(abs_path, cwd, cwd_len) == 0) {
      const char *relative = abs_path + cwd_len;
      if (*relative == '/')
        relative++;
      if (*relative != '\0' && strlen(relative) < strlen(abs_path)) {
        return relative;
      }
    }
  }

  return get_last_segments(abs_path, 3);
}

static void emit_single_diagnostic(Diagnostic *diag) {
  const char *level_format;
  switch (diag->level) {
  case DIAG_ERROR:
    level_format = "*[*, red]error[/]";
    break;
  case DIAG_WARNING:
    level_format = "*[*, yellow]warning[/]";
    break;
  case DIAG_TIP:
    level_format = "*[*, blue]tip[/]";
    break;
  }

  char buffer[4096];

  if (diag->has_span) {
    size_t line = diag->span.start_line;
    size_t col = diag->span.start_col;
    const char *file = diag->span.file;

    int line_num_width = snprintf(NULL, 0, "%zu", line);
    const char *display_path = get_display_path(file);

    char caret_buf[256];
    size_t caret_len = diag->error_length > 0 ? diag->error_length : 1;
    if (caret_len >= sizeof(caret_buf)) {
      caret_len = sizeof(caret_buf) - 1;
    }
    for (size_t i = 0; i < caret_len; i++) {
      caret_buf[i] = '^';
    }
    caret_buf[caret_len] = '\0';

    snprintf(buffer, sizeof(buffer),
             "*[cyan]%s:%zu:%zu[/]\n"
             "%s: %s\n"
             "*[d]%zu[/] | %s\n"
             "%*s | %*s*[*, red]%s[/]\n\n",
             display_path, line, col, level_format, diag->message, line,
             diag->source_line ? diag->source_line : "", line_num_width, "",
             (int)diag->error_start, "", caret_buf);
  } else {
    snprintf(buffer, sizeof(buffer), "%s: %s\n", level_format, diag->message);
  }

  char formatted[8192];
  pastel_format(buffer, formatted, sizeof(formatted));
  fprintf(stderr, "%s", formatted);
}

void diagnostic_emit(Diagnostic *diag) {
  Diagnostic *current = diag;
  while (current != NULL) {
    emit_single_diagnostic(current);
    current = current->next;
  }
}
