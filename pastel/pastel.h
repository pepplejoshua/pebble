#ifndef PASTEL_H
#define PASTEL_H

#include <stddef.h>   // for size_t
#include <stdbool.h>  // for bool
#include <stdio.h>
#include <stdlib.h>   // for malloc
#include <string.h>   // for memcpy

// Configuration
#define MAX_NESTING_DEPTH 32
#define MAX_COLOR_NAME_LEN 16

// Token types for lexer
typedef enum {
    TOKEN_TEXT,
    TOKEN_FORMAT_START,  // *[...]
    TOKEN_FORMAT_END,    // [/]
    TOKEN_EOF,
    TOKEN_ERROR
} TokenType;

typedef struct {
    TokenType type;
    const char* start;   // Pointer into source string
    size_t length;       // Length of this token's text
    size_t line;
    size_t column;
} Token;

// AST node types
typedef enum {
    NODE_TEXT,
    NODE_FORMATTED
} NodeType;

// Style options parsed from *[...]
typedef struct {
    bool bold;
    bool underline;
    bool italic;
    bool strikethrough;
    bool dim;
    bool reverse;

    const char* fg_color;      // e.g., "l_red", "d_blue", or NULL
    size_t fg_color_len;
    const char* bg_color;      // e.g., "l_white", ":d_cyan", or NULL
    size_t bg_color_len;
} StyleOptions;

// ============================================
// PASTEL LEXER
// ============================================

typedef struct {
    const char* source;
    size_t length;
    size_t current;     // Current position in source
    size_t start;       // Start of current token
    size_t line;        // Current line (for error reporting)
    size_t column;      // Current column (for error reporting)
} PastelLexer;

// Initialize the lexer
static inline void pastel_lexer_init(PastelLexer* lexer, const char* source) {
    lexer->source = source;
    lexer->length = strlen(source);
    lexer->current = 0;
    lexer->start = 0;
    lexer->line = 1;
    lexer->column = 1;
}

// Character navigation (like your lexer)
static inline bool pastel_is_at_end(PastelLexer* lexer) {
  return lexer->current >= lexer->length;
}
static inline char pastel_advance(PastelLexer* lexer) {
  if (pastel_is_at_end(lexer))
         return '\0';

  char c = lexer->source[lexer->current++];
  if (c == '\n') {
      lexer->line++;
      lexer->column = 1;
  } else {
      lexer->column++;
  }
  return c;
}
static inline char pastel_peek(PastelLexer* lexer) {
  if (pastel_is_at_end(lexer))
    return '\0';
  return lexer->source[lexer->current];
}
static inline char pastel_peek_by(PastelLexer* lexer, int peek_by) {
  if (lexer->current + peek_by >= lexer->length)
    return '\0';
  return lexer->source[lexer->current + peek_by];
}
static inline bool pastel_match(PastelLexer* lexer, char expected) {
  if (pastel_is_at_end(lexer))
    return false;
  if (lexer->source[lexer->current] != expected)
    return false;
  lexer->current++;
  lexer->column++;
  return true;
}

// Pattern matching
// Check for "*["
static inline bool pastel_is_format_start(PastelLexer* lexer) {
  return pastel_peek(lexer) == '*' && pastel_peek_by(lexer, 1) == '[';
}
// Check for "[/]"
static inline bool pastel_is_format_end(PastelLexer* lexer) {
  return pastel_peek(lexer) == '[' &&
          pastel_peek_by(lexer, 1) == '/' &&
          pastel_peek_by(lexer, 2) == ']';
}

// Token creation
static inline Token pastel_make_token(PastelLexer* lexer, TokenType type) {
  Token token;
  token.type = type;
  token.start = &lexer->source[lexer->start];
  token.length = lexer->current - lexer->start;
  token.line = lexer->line;
  token.column = lexer->column - token.length;

  return token;
}
static inline Token pastel_error_token(PastelLexer* lexer, const char* message, size_t msg_len) {
  Token token;
  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = msg_len;
  token.line = lexer->line;
  token.column = lexer->column;
  return token;
}

// Token scanning functions
// Scan *[options]
static inline Token pastel_scan_format_start(PastelLexer* lexer) {
    lexer->start = lexer->current;

    // Consume "*["
    pastel_advance(lexer);  // '*'
    pastel_advance(lexer);  // '['

    size_t options_start = lexer->current;

    // Scan until we find the closing ']'
    while (!pastel_is_at_end(lexer) && pastel_peek(lexer) != ']') {
        pastel_advance(lexer);
    }

    if (pastel_is_at_end(lexer)) {
        const char *err = "Unterminated format directive";
        return pastel_error_token(lexer, err, strlen(err));
    }

    // Create token pointing to just the options part
    Token token;
    token.type = TOKEN_FORMAT_START;
    token.start = &lexer->source[options_start];
    token.length = lexer->current - options_start;
    token.line = lexer->line;
    token.column = lexer->column;

    pastel_advance(lexer);  // Consume ']'

    return token;
}
// Scan [/]
static inline Token pastel_scan_format_end(PastelLexer* lexer) {
    lexer->start = lexer->current;
    // Consume [/]
    pastel_advance(lexer);  // '['
    pastel_advance(lexer);  // '/'
    pastel_advance(lexer);  // ']'
    return pastel_make_token(lexer, TOKEN_FORMAT_END);
}
//Scan plain text
static inline Token pastel_scan_text(PastelLexer* lexer) {
    lexer->start = lexer->current;

    // Scan until we hit *[ or [/] or EOF
    while (!pastel_is_at_end(lexer) &&
           !pastel_is_format_start(lexer) &&
           !pastel_is_format_end(lexer)) {
        pastel_advance(lexer);
    }

    // Extract the text
    return pastel_make_token(lexer, TOKEN_TEXT);
}

// Main scanning function - get next token
static inline Token pastel_lexer_next_token(PastelLexer* lexer) {
    lexer->start = lexer->current;

    if (pastel_is_at_end(lexer)) {
        return pastel_make_token(lexer, TOKEN_EOF);
    }

    // Check for format directives first
    if (pastel_is_format_start(lexer)) {
        return pastel_scan_format_start(lexer);
    }

    if (pastel_is_format_end(lexer)) {
        return pastel_scan_format_end(lexer);
    }

    // Otherwise it's plain text
    return pastel_scan_text(lexer);
}

// Public API - tokenize entire input into array
static inline Token* pastel_tokenize(const char* input, size_t* token_count) {
    PastelLexer lexer;
    pastel_lexer_init(&lexer, input);

    // First pass: count tokens
    size_t capacity = 16;
    Token* tokens = (Token*)malloc(sizeof(Token) * capacity);
    if (!tokens) return NULL;

    size_t count = 0;
    Token token;

    do {
        token = pastel_lexer_next_token(&lexer);

        // Resize if needed
        if (count >= capacity) {
            capacity *= 2;
            Token* new_tokens = (Token*)realloc(tokens, sizeof(Token) * capacity);
            if (!new_tokens) {
                free(tokens);
                return NULL;
            }
            tokens = new_tokens;
        }

        tokens[count++] = token;
    } while (token.type != TOKEN_EOF);

    *token_count = count;
    return tokens;
}
static inline void pastel_free_tokens(Token* tokens) {
    free(tokens);
}

// ============================================
// PASTEL FORMATTER
// ============================================

// Style stack for tracking nested formatting
typedef struct {
    StyleOptions** stack;
    size_t capacity;
    size_t count;
} StyleStack;

static inline void free_style_options(StyleOptions* opts) {
    if (!opts) return;
    free(opts);
}
static inline void style_stack_init(StyleStack* stack) {
    stack->capacity = 8;
    stack->count = 0;
    stack->stack = (StyleOptions**)malloc(sizeof(StyleOptions*) * stack->capacity);
}

static inline void style_stack_push(StyleStack* stack, StyleOptions* style) {
    // Resize if needed
    if (stack->count >= stack->capacity) {
        stack->capacity *= 2;
        StyleOptions** new_stack = (StyleOptions**)realloc(stack->stack,
                                                           sizeof(StyleOptions*) * stack->capacity);
        if (!new_stack) return;  // TODO: error handling
        stack->stack = new_stack;
    }

    stack->stack[stack->count++] = style;
}

static inline StyleOptions* style_stack_pop(StyleStack* stack) {
    if (stack->count == 0) return NULL;
    return stack->stack[--stack->count];
}

static inline StyleOptions* style_stack_peek(StyleStack* stack) {
    if (stack->count == 0) return NULL;
    return stack->stack[stack->count - 1];
}

static inline void style_stack_free(StyleStack* stack) {
    // Free all StyleOptions in the stack
    for (size_t i = 0; i < stack->count; i++) {
        free_style_options(stack->stack[i]);
    }
    free(stack->stack);
    stack->count = 0;
    stack->capacity = 0;
}

// Helper: compare string with length
static inline bool str_eq(const char* str, size_t len, const char* target) {
    size_t target_len = strlen(target);
    return len == target_len && memcmp(str, target, len) == 0;
}

// Helper: check if substring starts with prefix
static inline bool str_starts_with(const char* str, size_t len, const char* prefix) {
    size_t prefix_len = strlen(prefix);
    return len >= prefix_len && memcmp(str, prefix, prefix_len) == 0;
}

// Parse options string into StyleOptions
// Input: "*, u, l_red:d_blue" or "b, red" etc.
// Parse options string into StyleOptions (no allocation!)
static inline void parse_style_options(const char* options_str, size_t options_len, StyleOptions* opts) {
    // Initialize
    opts->bold = false;
    opts->underline = false;
    opts->italic = false;
    opts->strikethrough = false;
    opts->dim = false;
    opts->reverse = false;
    opts->fg_color = NULL;
    opts->fg_color_len = 0;
    opts->bg_color = NULL;
    opts->bg_color_len = 0;

    if (!options_str || options_len == 0) return;

    // Parse comma-separated options
    size_t start = 0;
    for (size_t i = 0; i <= options_len; i++) {
        if (i == options_len || options_str[i] == ',') {
            // Extract token
            size_t token_start = start;
            size_t token_end = i;

            // Trim whitespace
            while (token_start < token_end && options_str[token_start] == ' ') token_start++;
            while (token_end > token_start && options_str[token_end - 1] == ' ') token_end--;

            size_t token_len = token_end - token_start;
            const char* token = &options_str[token_start];

            if (token_len == 0) {
                start = i + 1;
                continue;
            }

            // Check style flags
            if (str_eq(token, token_len, "*") || str_eq(token, token_len, "b")) {
                opts->bold = true;
            }
            else if (str_eq(token, token_len, "_") || str_eq(token, token_len, "u")) {
                opts->underline = true;
            }
            else if (str_eq(token, token_len, "/") || str_eq(token, token_len, "i")) {
                opts->italic = true;
            }
            else if (str_eq(token, token_len, "~") || str_eq(token, token_len, "s")) {
                opts->strikethrough = true;
            }
            else if (str_eq(token, token_len, "d")) {
                opts->dim = true;
            }
            else if (str_eq(token, token_len, "r")) {
                opts->reverse = true;
            }
            // Check for colors
            else if (str_starts_with(token, token_len, "l_") || str_starts_with(token, token_len, "d_")) {
                // Look for colon (fg:bg)
                size_t colon_pos = 0;
                bool has_colon = false;
                for (size_t j = 0; j < token_len; j++) {
                    if (token[j] == ':') {
                        colon_pos = j;
                        has_colon = true;
                        break;
                    }
                }

                if (has_colon) {
                    // Has both fg and bg
                    opts->fg_color = token;
                    opts->fg_color_len = colon_pos;
                    opts->bg_color = token + colon_pos + 1;
                    opts->bg_color_len = token_len - colon_pos - 1;
                } else {
                    // Just foreground
                    opts->fg_color = token;
                    opts->fg_color_len = token_len;
                }
            }
            // Background only (:color)
            else if (token[0] == ':' && token_len > 1) {
                opts->bg_color = token + 1;
                opts->bg_color_len = token_len - 1;
            } else {
                // Try plain color name (will be handled as l_color by get_ansi_color_code)
                if (str_eq(token, token_len, "black") ||
                    str_eq(token, token_len, "red") ||
                    str_eq(token, token_len, "green") ||
                    str_eq(token, token_len, "yellow") ||
                    str_eq(token, token_len, "blue") ||
                    str_eq(token, token_len, "magenta") ||
                    str_eq(token, token_len, "cyan") ||
                    str_eq(token, token_len, "white")) {
                    opts->fg_color = token;
                    opts->fg_color_len = token_len;
                }
            }


            start = i + 1;
        }
    }
}

// Helper to append string to dynamically growing buffer
static inline bool append_str(char** out, size_t* out_len, size_t* out_capacity, const char* str) {
    size_t str_len = strlen(str);
    if (*out_len + str_len >= *out_capacity) {
        return false;  // Buffer too small
    }
    memcpy(*out + *out_len, str, str_len);
    *out_len += str_len;
    (*out)[*out_len] = '\0';
    return true;
}

// Helper: append buffer to output
static inline bool append_buf(char** out, size_t* out_len, size_t* out_capacity, const char* buf, size_t buf_len) {
    if (*out_len + buf_len >= *out_capacity) {
        return false;  // Buffer too small
    }
    memcpy(*out + *out_len, buf, buf_len);
    *out_len += buf_len;
    (*out)[*out_len] = '\0';
    return true;
}

// ANSI escape code constants
#define ANSI_RESET        "\x1b[0m"
#define ANSI_BOLD         "\x1b[1m"
#define ANSI_DIM          "\x1b[2m"
#define ANSI_ITALIC       "\x1b[3m"
#define ANSI_UNDERLINE    "\x1b[4m"
#define ANSI_REVERSE      "\x1b[7m"
#define ANSI_STRIKETHROUGH "\x1b[9m"

// Emit reset code
static inline void emit_ansi_reset(char** output, size_t* len, size_t* capacity) {
    append_str(output, len, capacity, ANSI_RESET);
}

// Get ANSI color code from color string like "l_red" or "d_blue"
static inline const char* get_ansi_color_code(const char* color, size_t len, bool is_bg) {
    if (!color || len == 0) return NULL;

    bool is_light = true;  // Default to light
    const char* name = color;
    size_t name_len = len;

    // Check if it has l_ or d_ prefix
    if (len >= 3 && (color[0] == 'l' || color[0] == 'd') && color[1] == '_') {
        is_light = (color[0] == 'l');
        name = color + 2;
        name_len = len - 2;
    }
    // Otherwise it's a plain color name (like "red"), treat as light

    // Map color names to ANSI codes
    // Foreground: 30-37 (dark), 90-97 (light)
    // Background: 40-47 (dark), 100-107 (light)

    int base = 0;
    if (str_eq(name, name_len, "black"))   base = is_bg ? 40 : 30;
    else if (str_eq(name, name_len, "red"))     base = is_bg ? 41 : 31;
    else if (str_eq(name, name_len, "green"))   base = is_bg ? 42 : 32;
    else if (str_eq(name, name_len, "yellow"))  base = is_bg ? 43 : 33;
    else if (str_eq(name, name_len, "blue"))    base = is_bg ? 44 : 34;
    else if (str_eq(name, name_len, "magenta")) base = is_bg ? 45 : 35;
    else if (str_eq(name, name_len, "cyan"))    base = is_bg ? 46 : 36;
    else if (str_eq(name, name_len, "white"))   base = is_bg ? 47 : 37;
    else return NULL;

    if (is_light) base += 60;

    static char buf[16];
    snprintf(buf, sizeof(buf), "\x1b[%dm", base);
    return buf;
}


// Emit ANSI codes for a StyleOptions
static inline bool emit_ansi_codes(StyleOptions* style, char** output, size_t* len, size_t* capacity) {
  if (style->bold && !append_str(output, len, capacity, ANSI_BOLD)) return false;
      if (style->dim && !append_str(output, len, capacity, ANSI_DIM)) return false;
      if (style->italic && !append_str(output, len, capacity, ANSI_ITALIC)) return false;
      if (style->underline && !append_str(output, len, capacity, ANSI_UNDERLINE)) return false;
      if (style->strikethrough && !append_str(output, len, capacity, ANSI_STRIKETHROUGH)) return false;
      if (style->reverse && !append_str(output, len, capacity, ANSI_REVERSE)) return false;

      if (style->fg_color) {
          const char* code = get_ansi_color_code(style->fg_color, style->fg_color_len, false);
          if (code && !append_str(output, len, capacity, code)) return false;
      }

      if (style->bg_color) {
          const char* code = get_ansi_color_code(style->bg_color, style->bg_color_len, true);
          if (code && !append_str(output, len, capacity, code)) return false;
      }

      return true;
}

// Main formatter - converts tokens to ANSI-formatted string
static inline size_t pastel_format(const char* input, char* output, size_t output_size) {
    if (!input || !output || output_size == 0) return 0;

    PastelLexer lexer;
    pastel_lexer_init(&lexer, input);

    // Style stack (fixed size, no allocation!)
    StyleOptions stack[MAX_NESTING_DEPTH];
    size_t stack_count = 0;

    char* out = output;
    size_t out_len = 0;
    size_t out_cap = output_size;
    out[0] = '\0';

    Token token;
    while (true) {
        token = pastel_lexer_next_token(&lexer);

        if (token.type == TOKEN_EOF || token.type == TOKEN_ERROR) {
            break;
        }

        switch (token.type) {
            case TOKEN_FORMAT_START: {
                // Parse options and push onto stack
                if (stack_count >= MAX_NESTING_DEPTH) {
                    return 0;  // Too much nesting
                }

                StyleOptions* style = &stack[stack_count++];
                parse_style_options(token.start, token.length, style);

                // Emit ANSI codes
                if (!emit_ansi_codes(style, &out, &out_len, &out_cap)) {
                    return 0;  // Buffer too small
                }
                break;
            }

            case TOKEN_TEXT: {
                // Append text directly
                if (!append_buf(&out, &out_len, &out_cap, token.start, token.length)) {
                    return 0;  // Buffer too small
                }
                break;
            }

            case TOKEN_FORMAT_END: {
                // Pop from stack
                if (stack_count == 0) {
                    return 0;  // Unmatched closing tag
                }
                stack_count--;

                // Reset and re-apply parent style
                if (!append_str(&out, &out_len, &out_cap, ANSI_RESET)) {
                    return 0;
                }

                if (stack_count > 0) {
                    if (!emit_ansi_codes(&stack[stack_count - 1], &out, &out_len, &out_cap)) {
                        return 0;
                    }
                }
                break;
            }

            default:
                break;
        }
    }

    // Final reset
    append_str(&out, &out_len, &out_cap, ANSI_RESET);

    return out_len;
}

#endif
