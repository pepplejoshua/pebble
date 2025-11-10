#ifndef PASTEL_H
#define PASTEL_H

#include <stddef.h>   // for size_t
#include <stdbool.h>  // for bool
#include <stdio.h>
#include <stdlib.h>   // for malloc
#include <string.h>   // for memcpy

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
    char* value;         // For TEXT and FORMAT_START (options string)
    size_t line;        // Current line (for error reporting)
    size_t column;      // Current column (for error reporting)
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
    char* fg_color;      // e.g., "l_red", "d_blue", or NULL
    char* bg_color;      // e.g., "l_white", ":d_cyan", or NULL
} StyleOptions;

typedef struct ASTNode {
    NodeType type;
    union {
        char* text;                    // For NODE_TEXT
        struct {
            StyleOptions* style;
            struct ASTNode** children; // Array of child nodes
            size_t child_count;
        } formatted;
    } data;
} ASTNode;

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

// Helper: String duplication (you can use arena allocator or malloc)
static inline char* pastel_str_dup(const char* str, size_t len) {
    char* copy = (char*)malloc(len + 1);
    if (!copy) return NULL;
    memcpy(copy, str, len);
    copy[len] = '\0';
    return copy;
}

// Token creation
static inline Token pastel_make_token(PastelLexer* lexer, TokenType type, char* value) {
  Token token;
  token.type = type;
  token.line = lexer->line;
  token.column = lexer->column - (lexer->current - lexer->start);
  token.value = value;

  return token;
}
static inline Token pastel_error_token(PastelLexer* lexer, char* message) {
  Token token;
  token.type = TOKEN_ERROR;
  token.line = lexer->line;
  token.column = lexer->column;
  token.value = message;
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
        return pastel_error_token(lexer, pastel_str_dup("Unterminated format directive", 30));
    }

    // Extract options string
    size_t options_len = lexer->current - options_start;
    char* options = pastel_str_dup(&lexer->source[options_start], options_len);

    pastel_advance(lexer);  // Consume ']'

    return pastel_make_token(lexer, TOKEN_FORMAT_START, options);
}
// Scan [/]
static inline Token pastel_scan_format_end(PastelLexer* lexer) {
    lexer->start = lexer->current;

    // Consume [/]
    pastel_advance(lexer);  // '['
    pastel_advance(lexer);  // '/'
    pastel_advance(lexer);  // ']'

    return pastel_make_token(lexer, TOKEN_FORMAT_END, NULL);
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
    size_t len = lexer->current - lexer->start;
    char* text = pastel_str_dup(&lexer->source[lexer->start], len);
    return pastel_make_token(lexer, TOKEN_TEXT, text);
}

// Main scanning function - get next token
static inline Token pastel_lexer_next_token(PastelLexer* lexer) {
    // Skip any leading whitespace? (Optional - depends if you want to preserve it)
    // For now, let's preserve all text including whitespace

    lexer->start = lexer->current;

    if (pastel_is_at_end(lexer)) {
        return pastel_make_token(lexer, TOKEN_EOF, NULL);
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
static inline void pastel_free_tokens(Token* tokens, size_t count) {
    for (size_t i = 0; i < count; i++) {
        if (tokens[i].value) {
            free(tokens[i].value);
        }
    }
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
    if (opts->fg_color) free(opts->fg_color);
    if (opts->bg_color) free(opts->bg_color);
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

// Parse options string into StyleOptions
// Input: "*, u, l_red:d_blue" or "b, red" etc.
static inline StyleOptions* parse_style_options(const char* options_str) {
    StyleOptions* opts = (StyleOptions*)malloc(sizeof(StyleOptions));
    if (!opts) return NULL;

    // Initialize all to false/NULL
    opts->bold = false;
    opts->underline = false;
    opts->italic = false;
    opts->strikethrough = false;
    opts->dim = false;
    opts->reverse = false;
    opts->fg_color = NULL;
    opts->bg_color = NULL;

    if (!options_str || strlen(options_str) == 0) {
        return opts;
    }

    // Make a copy we can modify
    char* opts_copy = strdup(options_str);
    char* token = strtok(opts_copy, ",");

    while (token) {
        // Trim whitespace
        while (*token == ' ') token++;
        char* end = token + strlen(token) - 1;
        while (end > token && *end == ' ') end--;
        *(end + 1) = '\0';

        // Check for style flags
        if (strcmp(token, "*") == 0 || strcmp(token, "b") == 0) {
            opts->bold = true;
        }
        else if (strcmp(token, "_") == 0 || strcmp(token, "u") == 0) {
            opts->underline = true;
        }
        else if (strcmp(token, "/") == 0 || strcmp(token, "i") == 0) {
            opts->italic = true;
        }
        else if (strcmp(token, "~") == 0 || strcmp(token, "s") == 0) {
            opts->strikethrough = true;
        }
        else if (strcmp(token, "d") == 0) {
            opts->dim = true;
        }
        else if (strcmp(token, "r") == 0) {
            opts->reverse = true;
        }
        // Check for colors (l_COLOR or d_COLOR or l_COLOR:d_COLOR)
        else if (strncmp(token, "l_", 2) == 0 || strncmp(token, "d_", 2) == 0) {
            // Check if it contains ':' for fg:bg
            char* colon = strchr(token, ':');
            if (colon) {
                // Has both fg and bg
                size_t fg_len = colon - token;
                opts->fg_color = pastel_str_dup(token, fg_len);
                opts->bg_color = strdup(colon + 1);
            } else {
                // Just foreground
                opts->fg_color = strdup(token);
            }
        }
        // Check for background-only color (:l_COLOR or :d_COLOR)
        else if (token[0] == ':' && (strncmp(token + 1, "l_", 2) == 0 || strncmp(token + 1, "d_", 2) == 0)) {
            opts->bg_color = strdup(token + 1);  // Skip the ':'
        }
        // Try without prefix (shorthand like "red" assumes "l_red")
        else {
            // Check if it's a valid color name
            const char* colors[] = {"black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"};
            bool is_color = false;
            for (int i = 0; i < 8; i++) {
                if (strcmp(token, colors[i]) == 0) {
                    is_color = true;
                    break;
                }
            }
            if (is_color) {
              // Assume light foreground
              size_t len = strlen(token) + 3;  // "l_" + color + '\0'
              opts->fg_color = (char*)malloc(len);
              snprintf(opts->fg_color, len, "l_%s", token);
            }
            // Otherwise ignore unknown option
        }

        token = strtok(NULL, ",");
    }

    free(opts_copy);
    return opts;
}

// Helper to append string to dynamically growing buffer
static inline void append_string(char** output, size_t* len, size_t* capacity, const char* str) {
    if (!str) return;

    size_t str_len = strlen(str);

    // Ensure capacity
    while (*len + str_len + 1 > *capacity) {
        *capacity *= 2;
        char* new_output = (char*)realloc(*output, *capacity);
        if (!new_output) return;  // TODO: better error handling
        *output = new_output;
    }

    // Append
    memcpy(*output + *len, str, str_len);
    *len += str_len;
    (*output)[*len] = '\0';
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
    append_string(output, len, capacity, ANSI_RESET);
}

// Get ANSI color code from color string like "l_red" or "d_blue"
static inline const char* get_ansi_color_code(const char* color_str, bool is_background) {
    if (!color_str) return NULL;

    bool is_light = (color_str[0] == 'l');
    const char* color_name = color_str + 2;  // Skip "l_" or "d_"

    // Map color names to ANSI codes
    // Foreground: 30-37 (dark), 90-97 (light)
    // Background: 40-47 (dark), 100-107 (light)

    int base_code = 0;
    if (strcmp(color_name, "black") == 0)   base_code = is_background ? 40 : 30;
    else if (strcmp(color_name, "red") == 0)     base_code = is_background ? 41 : 31;
    else if (strcmp(color_name, "green") == 0)   base_code = is_background ? 42 : 32;
    else if (strcmp(color_name, "yellow") == 0)  base_code = is_background ? 43 : 33;
    else if (strcmp(color_name, "blue") == 0)    base_code = is_background ? 44 : 34;
    else if (strcmp(color_name, "magenta") == 0) base_code = is_background ? 45 : 35;
    else if (strcmp(color_name, "cyan") == 0)    base_code = is_background ? 46 : 36;
    else if (strcmp(color_name, "white") == 0)   base_code = is_background ? 47 : 37;
    else return NULL;

    if (is_light) base_code += 60;  // Light variants are +60

    static char code_buf[16];
    snprintf(code_buf, sizeof(code_buf), "\x1b[%dm", base_code);
    return code_buf;
}

// Emit ANSI codes for a StyleOptions
static inline void emit_ansi_codes(StyleOptions* style, char** output, size_t* len, size_t* capacity) {
    if (!style) return;

    if (style->bold) {
        append_string(output, len, capacity, ANSI_BOLD);
    }
    if (style->dim) {
        append_string(output, len, capacity, ANSI_DIM);
    }
    if (style->italic) {
        append_string(output, len, capacity, ANSI_ITALIC);
    }
    if (style->underline) {
        append_string(output, len, capacity, ANSI_UNDERLINE);
    }
    if (style->strikethrough) {
        append_string(output, len, capacity, ANSI_STRIKETHROUGH);
    }
    if (style->reverse) {
        append_string(output, len, capacity, ANSI_REVERSE);
    }

    // Foreground color
    if (style->fg_color) {
        const char* fg_code = get_ansi_color_code(style->fg_color, false);
        if (fg_code) {
            append_string(output, len, capacity, fg_code);
        }
    }

    // Background color
    if (style->bg_color) {
        const char* bg_code = get_ansi_color_code(style->bg_color, true);
        if (bg_code) {
            append_string(output, len, capacity, bg_code);
        }
    }
}

// Main formatter - converts tokens to ANSI-formatted string
static inline char* pastel_format(Token* tokens, size_t token_count) {
    // Initialize output buffer
    size_t capacity = 256;
    size_t len = 0;
    char* output = (char*)malloc(capacity);
    if (!output) return NULL;
    output[0] = '\0';

    // Initialize style stack
    StyleStack stack;
    style_stack_init(&stack);

    // Process each token
    for (size_t i = 0; i < token_count; i++) {
        Token* token = &tokens[i];

        switch (token->type) {
            case TOKEN_FORMAT_START: {
                // Parse options and push onto stack
                StyleOptions* style = parse_style_options(token->value);
                style_stack_push(&stack, style);

                // Emit ANSI codes for this style
                emit_ansi_codes(style, &output, &len, &capacity);
                break;
            }

            case TOKEN_TEXT: {
                // Just append the text (codes already emitted)
                append_string(&output, &len, &capacity, token->value);
                break;
            }

            case TOKEN_FORMAT_END: {
                // Pop from stack
                StyleOptions* popped = style_stack_pop(&stack);
                free_style_options(popped);

                // Reset and re-apply parent style (if any)
                emit_ansi_reset(&output, &len, &capacity);

                StyleOptions* parent = style_stack_peek(&stack);
                if (parent) {
                    emit_ansi_codes(parent, &output, &len, &capacity);
                }
                break;
            }

            case TOKEN_EOF:
                // Done
                break;

            case TOKEN_ERROR:
                // Handle error?
                break;
        }
    }

    // Final reset to clean up any remaining formatting
    emit_ansi_reset(&output, &len, &capacity);

    style_stack_free(&stack);
    return output;
}

// Public API - one-shot formatting
static inline char* pastel_format_text(const char* input) {
    size_t token_count;
    Token* tokens = pastel_tokenize(input, &token_count);

    if (!tokens) return NULL;

    char* result = pastel_format(tokens, token_count);

    pastel_free_tokens(tokens, token_count);

    return result;
}

#endif
