#include "../include/prole_diag.h"
#include "../../pastel/pastel.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *prole_diag_strdup(const char *value) {
  if (!value) {
    return NULL;
  }

  size_t len = strlen(value);
  char *copy = malloc(len + 1);
  if (!copy) {
    return NULL;
  }

  memcpy(copy, value, len + 1);
  return copy;
}

static void split_source_into_lines(ProleDiagnosticContext *ctx) {
  if (!ctx->source) {
    return;
  }

  size_t line_count = 1;
  for (const char *p = ctx->source; *p; p++) {
    if (*p == '\n') {
      line_count++;
    }
  }

  ctx->source_lines = calloc(line_count, sizeof(char *));
  if (!ctx->source_lines) {
    ctx->line_count = 0;
    return;
  }

  char *line_start = ctx->source;
  ctx->source_lines[ctx->line_count++] = line_start;

  for (char *p = ctx->source; *p; p++) {
    if (*p == '\n') {
      *p = '\0';
      if (*(p + 1) != '\0') {
        ctx->source_lines[ctx->line_count++] = p + 1;
      }
    }
  }
}

void prole_diagnostics_init(ProleDiagnosticContext *ctx, const char *filename,
                            const char *source) {
  memset(ctx, 0, sizeof(ProleDiagnosticContext));
  ctx->filename = filename;
  ctx->source = prole_diag_strdup(source);
  split_source_into_lines(ctx);
}

void prole_diagnostics_free(ProleDiagnosticContext *ctx) {
  free(ctx->source);
  free(ctx->source_lines);
  memset(ctx, 0, sizeof(ProleDiagnosticContext));
}

static ProleDiagnostic *build_diag(ProleDiagnosticContext *ctx,
                                   ProleDiagLevel level, bool has_span,
                                   ProleSpan span, const char *fmt,
                                   va_list args) {
  ProleDiagnostic *diag = calloc(1, sizeof(ProleDiagnostic));
  if (!diag) {
    return NULL;
  }

  diag->level = level;
  diag->has_span = has_span;
  diag->span = span;

  va_list args_copy;
  va_copy(args_copy, args);
  int needed = vsnprintf(NULL, 0, fmt, args_copy);
  va_end(args_copy);

  if (needed < 0) {
    free(diag);
    return NULL;
  }

  diag->message = malloc((size_t)needed + 1);
  if (!diag->message) {
    free(diag);
    return NULL;
  }

  vsnprintf(diag->message, (size_t)needed + 1, fmt, args);

  if (has_span && ctx && span.start_line > 0 && span.start_line <= ctx->line_count) {
    diag->source_line = ctx->source_lines[span.start_line - 1];
    size_t start_col = span.start_col > 0 ? span.start_col : 1;
    size_t end_col = span.end_col > 0 ? span.end_col : start_col + 1;
    if (end_col < start_col) {
      end_col = start_col;
    }
    diag->error_start = start_col - 1;
    diag->error_length = end_col > start_col ? end_col - start_col : 1;
  }

  if (ctx && level == PROLE_DIAG_ERROR) {
    ctx->error_count++;
  } else if (ctx && level == PROLE_DIAG_WARNING) {
    ctx->warning_count++;
  }

  if (ctx) {
    if (ctx->last) {
      ctx->last->next = diag;
    } else {
      ctx->first = diag;
    }
    ctx->last = diag;
  }

  return diag;
}

ProleDiagnostic *prole_diag_error(ProleDiagnosticContext *ctx, ProleSpan span,
                                  const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  ProleDiagnostic *diag = build_diag(ctx, PROLE_DIAG_ERROR, true, span, fmt, args);
  va_end(args);
  return diag;
}

ProleDiagnostic *prole_diag_warning(ProleDiagnosticContext *ctx,
                                    ProleSpan span, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  ProleDiagnostic *diag =
      build_diag(ctx, PROLE_DIAG_WARNING, true, span, fmt, args);
  va_end(args);
  return diag;
}

ProleDiagnostic *prole_diag_error_no_span(ProleDiagnosticContext *ctx,
                                          const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  ProleDiagnostic *diag =
      build_diag(ctx, PROLE_DIAG_ERROR, false, prole_span_new(NULL, 0, 0, 0, 0),
                 fmt, args);
  va_end(args);
  return diag;
}

ProleDiagnostic *prole_diag_add_note(ProleDiagnostic *parent,
                                     const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  ProleDiagnostic *note =
      build_diag(NULL, PROLE_DIAG_NOTE, false, prole_span_new(NULL, 0, 0, 0, 0),
                 fmt, args);
  va_end(args);

  if (!parent) {
    return note;
  }

  ProleDiagnostic *current = parent;
  while (current->next) {
    current = current->next;
  }
  current->next = note;
  return parent;
}

static const char *level_label(ProleDiagLevel level) {
  switch (level) {
  case PROLE_DIAG_ERROR:
    return "*[bold,l_red]error[/]";
  case PROLE_DIAG_WARNING:
    return "*[bold,l_yellow]warning[/]";
  case PROLE_DIAG_NOTE:
    return "*[bold,l_blue]note[/]";
  }

  return "*[bold]diagnostic[/]";
}

void prole_diag_emit(ProleDiagnostic *diag) {
  for (ProleDiagnostic *current = diag; current; current = current->next) {
    char buffer[4096];
    if (current->has_span) {
      snprintf(buffer, sizeof(buffer), "*[l_cyan]%s:%zu:%zu[/]\n%s: %s\n",
               current->span.file ? current->span.file : "<unknown>",
               current->span.start_line, current->span.start_col,
               level_label(current->level), current->message);
    } else {
      snprintf(buffer, sizeof(buffer), "%s: %s\n", level_label(current->level),
               current->message);
    }

    char formatted[4096];
    pastel_format(buffer, formatted, sizeof(formatted));
    fputs(formatted, stderr);

    if (current->source_line) {
      fprintf(stderr, "%zu | %s\n", current->span.start_line,
              current->source_line);
      fprintf(stderr, "  | ");
      for (size_t i = 0; i < current->error_start; i++) {
        fputc(' ', stderr);
      }
      fputc('^', stderr);
      fputc('\n', stderr);
    }
  }
}

void prole_diagnostics_emit_all(ProleDiagnosticContext *ctx) {
  if (!ctx || !ctx->first) {
    return;
  }

  prole_diag_emit(ctx->first);
}
