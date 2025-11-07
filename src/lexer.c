#include "lexer.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

// External allocator
extern Arena long_lived;

// Helper to duplicate strings into arena
static char *str_dup_lex(const char *str, size_t len) {
  char *copy = arena_alloc(&long_lived, len + 1);
  memcpy(copy, str, len);
  copy[len] = '\0';
  return copy;
}

// Character helpers
static bool is_digit(char c) { return c >= '0' && c <= '9'; }

static bool is_alpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool is_alnum(char c) { return is_alpha(c) || is_digit(c); }

// Lexer character scanning
static bool lexer_is_at_end(Lexer *lexer) {
  return lexer->current >= lexer->length;
}

static char lexer_advance(Lexer *lexer) {
  if (lexer_is_at_end(lexer))
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

static char lexer_peek(Lexer *lexer) {
  if (lexer_is_at_end(lexer))
    return '\0';
  return lexer->source[lexer->current];
}

static char lexer_peek_next(Lexer *lexer) {
  if (lexer->current + 1 >= lexer->length)
    return '\0';
  return lexer->source[lexer->current + 1];
}

static bool lexer_match(Lexer *lexer, char expected) {
  if (lexer_is_at_end(lexer))
    return false;
  if (lexer->source[lexer->current] != expected)
    return false;
  lexer->current++;
  lexer->column++;
  return true;
}

// Skip whitespace and comments
static void lexer_skip_whitespace(Lexer *lexer) {
  while (!lexer_is_at_end(lexer)) {
    char c = lexer_peek(lexer);
    switch (c) {
    case ' ':
    case '\r':
    case '\t':
    case '\n':
      lexer_advance(lexer);
      break;
    case '/':
      if (lexer_peek_next(lexer) == '/') {
        // Line comment
        while (lexer_peek(lexer) != '\n' && !lexer_is_at_end(lexer)) {
          lexer_advance(lexer);
        }
      } else {
        return;
      }
      break;
    default:
      return;
    }
  }
}

// Token creation
static Token lexer_make_token(Lexer *lexer, TokenType type) {
  Token token;
  token.type = type;
  token.location.file = lexer->filename;
  token.location.line = lexer->line;
  token.location.column = lexer->column - (lexer->current - lexer->start);

  size_t length = lexer->current - lexer->start;
  token.lexeme = str_dup_lex(&lexer->source[lexer->start], length);

  return token;
}

static Token lexer_error_token(Lexer *lexer, const char *message) {
  Token token;
  token.type = TOKEN_ERROR;
  token.location.file = lexer->filename;
  token.location.line = lexer->line;
  token.location.column = lexer->column;
  token.lexeme = str_dup_lex(message, strlen(message));
  return token;
}

// Keyword lookup table
static TokenType lexer_check_keyword(Lexer *lexer, size_t length,
                                     const char *keyword, TokenType type) {
  return (lexer->current - lexer->start == length &&
          memcmp(&lexer->source[lexer->start], keyword, length) == 0)
             ? type
             : TOKEN_IDENTIFIER;
}

static TokenType lexer_identifier_type(Lexer *lexer) {
  size_t length = lexer->current - lexer->start;
  const char *start = &lexer->source[lexer->start];

  switch (start[0]) {
  case 'a':
    if (length == 2)
      return lexer_check_keyword(lexer, 2, "as", TOKEN_AS);
    break;
  case 'b':
    if (length == 4)
      return lexer_check_keyword(lexer, 4, "bool", TOKEN_BOOL_TYPE);
    if (length == 5)
      return lexer_check_keyword(lexer, 5, "break", TOKEN_BREAK);
    break;
  case 'c':
    if (length == 7) {
      return lexer_check_keyword(lexer, 7, "context", TOKEN_CONTEXT);
    }
    if (length == 8)
      return lexer_check_keyword(lexer, 8, "continue", TOKEN_CONTINUE);
    if (length == 4) {
      if (start[1] == 'a')
        return lexer_check_keyword(lexer, 4, "case", TOKEN_CASE);

      if (start[1] == 'h')
        return lexer_check_keyword(lexer, 4, "char", TOKEN_CHAR_TYPE);
    }
    break;
  case 'd':
    if (length == 5)
      return lexer_check_keyword(lexer, 5, "defer", TOKEN_DEFER);
    break;
  case 'e':
    if (length == 4) {
      if (start[1] == 'l')
        return lexer_check_keyword(lexer, 4, "else", TOKEN_ELSE);

      if (start[1] == 'n')
        return lexer_check_keyword(lexer, 4, "enum", TOKEN_ENUM);
    }
    if (length == 6)
      return lexer_check_keyword(lexer, 6, "extern", TOKEN_EXTERN);
    break;
  case 'n':
    if (length == 3)
      return lexer_check_keyword(lexer, 3, "nil", TOKEN_NIL);
    if (length == 4)
      return lexer_check_keyword(lexer, 4, "none", TOKEN_NONE);
    break;
  case 'f':
    if (length > 1) {
      switch (start[1]) {
      case 'a':
        return lexer_check_keyword(lexer, 5, "false", TOKEN_FALSE);
      case 'n':
        return lexer_check_keyword(lexer, 2, "fn", TOKEN_FN);
      case 'o':
        return lexer_check_keyword(lexer, 3, "for", TOKEN_FOR);
      }
    }
    break;
  case 'i':
    if (length == 2) {
      if (start[1] == 'f')
        return lexer_check_keyword(lexer, 2, "if", TOKEN_IF);
      if (start[1] == '8')
        return lexer_check_keyword(lexer, 2, "i8", TOKEN_I8_TYPE);
    }
    if (length == 3) {
      if (start[1] == 'n' && start[2] == 't')
        return lexer_check_keyword(lexer, 3, "int", TOKEN_INT_TYPE);
      if (start[1] == '1' && start[2] == '6')
        return lexer_check_keyword(lexer, 3, "i16", TOKEN_I16_TYPE);
      if (start[1] == '3' && start[2] == '2')
        return lexer_check_keyword(lexer, 3, "i32", TOKEN_I32_TYPE);
      if (start[1] == '6' && start[2] == '4')
        return lexer_check_keyword(lexer, 3, "i64", TOKEN_I64_TYPE);
    }
    if (length == 5)
      return lexer_check_keyword(lexer, 5, "isize", TOKEN_ISIZE_TYPE);
    if (length == 6) {
      if (start[1] == 'm')
        return lexer_check_keyword(lexer, 6, "import", TOKEN_IMPORT);
      if (start[1] == 'n')
        return lexer_check_keyword(lexer, 6, "inline", TOKEN_INLINE);
    }
    break;
  case 'l':
    if (length == 3)
      return lexer_check_keyword(lexer, 3, "let", TOKEN_LET);
    if (length == 4)
      return lexer_check_keyword(lexer, 4, "loop", TOKEN_LOOP);
    break;
  case 'r':
    if (length == 6)
      return lexer_check_keyword(lexer, 6, "return", TOKEN_RETURN);
    break;
  case 's':
    if (length == 3)
      return lexer_check_keyword(lexer, 3, "str", TOKEN_STR_TYPE);
    if (length == 4)
      return lexer_check_keyword(lexer, 4, "some", TOKEN_SOME);
    if (length == 6) {
      if (start[1] == 't' && start[2] == 'r') {
        return lexer_check_keyword(lexer, 6, "struct", TOKEN_STRUCT);
      }
      if (start[1] == 'i' && start[2] == 'z') {
        return lexer_check_keyword(lexer, 6, "sizeof", TOKEN_SIZEOF);
      }
      if (start[1] == 'w' && start[2] == 'i') {
        return lexer_check_keyword(lexer, 6, "switch", TOKEN_SWITCH);
      }
    }
    break;
  case 't':
    if (length > 1) {
      switch (start[1]) {
      case 'r':
        return lexer_check_keyword(lexer, 4, "true", TOKEN_TRUE);
      case 'y':
        return lexer_check_keyword(lexer, 4, "type", TOKEN_TYPE);
      }
    }
    break;
  case 'u':
    if (length == 2)
      return lexer_check_keyword(lexer, 2, "u8", TOKEN_U8_TYPE);
    if (length == 3) {
      if (start[1] == '1' && start[2] == '6')
        return lexer_check_keyword(lexer, 3, "u16", TOKEN_U16_TYPE);
      if (start[1] == '3' && start[2] == '2')
        return lexer_check_keyword(lexer, 3, "u32", TOKEN_U32_TYPE);
      if (start[1] == '6' && start[2] == '4')
        return lexer_check_keyword(lexer, 3, "u64", TOKEN_U64_TYPE);
    }
    if (length == 5) {
      if (start[1] == 's') {
        return lexer_check_keyword(lexer, 5, "usize", TOKEN_USIZE_TYPE);
      }
      if (start[1] == 'n') {
        return lexer_check_keyword(lexer, 5, "union", TOKEN_UNION);
      }
    }
    break;
  case 'v':
    if (length == 3)
      return lexer_check_keyword(lexer, 3, "var", TOKEN_VAR);
    if (length == 4)
      return lexer_check_keyword(lexer, 4, "void", TOKEN_VOID_TYPE);
    break;
  case 'w':
    if (length == 5)
      return lexer_check_keyword(lexer, 5, "while", TOKEN_WHILE);
    break;
  case 'p':
    if (length == 5)
      return lexer_check_keyword(lexer, 5, "print", TOKEN_PRINT);
    break;
  }

  return TOKEN_IDENTIFIER;
}

// Process escape sequences in a string
// For interpolated strings, also handles \{ and \`
static char *process_string_escapes(Lexer *lexer, size_t start, size_t end,
                                    bool is_interpolated) {
  // Allocate max possible size (worst case: no escapes)
  size_t max_length = end - start;
  char *processed = arena_alloc(&long_lived, max_length + 1);
  size_t write_pos = 0;

  for (size_t i = start; i < end;) {
    if (lexer->source[i] == '\\' && i + 1 < end) {
      // Escape sequence
      char next = lexer->source[i + 1];
      switch (next) {
      case 'n':
        processed[write_pos++] = '\n';
        i += 2;
        break;
      case 't':
        processed[write_pos++] = '\t';
        i += 2;
        break;
      case 'r':
        processed[write_pos++] = '\r';
        i += 2;
        break;
      case '\\':
        processed[write_pos++] = '\\';
        i += 2;
        break;
      case '"':
        processed[write_pos++] = '"';
        i += 2;
        break;
      case '0':
        processed[write_pos++] = '\0';
        i += 2;
        break;
      case '`':
        if (is_interpolated) {
          processed[write_pos++] = '`';
          i += 2;
        } else {
          // Not valid in regular strings, keep as-is
          processed[write_pos++] = '\\';
          processed[write_pos++] = '`';
          i += 2;
        }
        break;
      case '{':
        if (is_interpolated) {
          processed[write_pos++] = '{';
          i += 2;
        } else {
          // Not valid in regular strings, keep as-is
          processed[write_pos++] = '\\';
          processed[write_pos++] = '{';
          i += 2;
        }
        break;
      default:
        // Unknown escape - keep both characters
        processed[write_pos++] = '\\';
        processed[write_pos++] = next;
        i += 2;
        break;
      }
    } else {
      // Regular character
      processed[write_pos++] = lexer->source[i];
      i++;
    }
  }

  processed[write_pos] = '\0';
  return processed;
}

static void lexer_queue_token(Lexer *lexer, Token token) {
  if (lexer->queue_count >= lexer->queue_capacity) {
    size_t new_capacity =
        lexer->queue_capacity == 0 ? 12 : lexer->queue_capacity * 2;

    Token *new_queue = arena_alloc(&long_lived, sizeof(Token) * new_capacity);

    if (lexer->token_queue != NULL && lexer->queue_count > 0) {
      memcpy(new_queue, lexer->token_queue, sizeof(Token) * lexer->queue_count);
    }

    lexer->token_queue = new_queue;
    lexer->queue_capacity = new_capacity;
  }

  lexer->token_queue[lexer->queue_count++] = token;
}

static Token lexer_make_token_at(Lexer *lexer, TokenType type, size_t start,
                                 size_t end, int line, int column) {
  Token token;
  token.type = type;
  token.location.file = lexer->filename;
  token.location.line = line;
  token.location.column = column;

  size_t length = end - start;
  token.lexeme = str_dup_lex(&lexer->source[start], length);

  return token;
}

static Token lexer_make_interpolated_fragment(Lexer *lexer, size_t start,
                                              size_t end, int line,
                                              int column) {
  Token token;
  token.type = TOKEN_STRING;
  token.location.file = lexer->filename;
  token.location.line = line;
  token.location.column = column;

  // Process both {{ -> { and escape sequences like \n, \t, \`, etc.
  char *processed = process_string_escapes(lexer, start, end, true);

  token.lexeme = processed;
  token.value.str_val = processed;

  return token;
}

static Token lexer_scan_interpolated_string(Lexer *lexer) {
  // lexer->start is at the backtick
  // Consume initial backtick
  lexer_advance(lexer);

  // Create opening backtick token
  Token bt =
      lexer_make_token_at(lexer, TOKEN_BACKTICK, lexer->start, lexer->current,
                          lexer->line, lexer->column - 1);

  // Track where current string fragment starts
  size_t fragment_start = lexer->current;
  int fragment_line = lexer->line;
  int fragment_column = lexer->column;

  while (!lexer_is_at_end(lexer)) {
    char c = lexer_peek(lexer);

    // Handle escape sequences first (including \`, \{, \n, etc.)
    if (c == '\\') {
      lexer_advance(lexer); // Skip backslash
      if (!lexer_is_at_end(lexer)) {
        lexer_advance(lexer); // Skip escaped character
      }
      continue;
    }

    if (c == '`') {
      // Found closing backtick (unescaped)
      // Queue any buffered fragment if it has content
      if (lexer->current > fragment_start) {
        Token fragment = lexer_make_interpolated_fragment(
            lexer, fragment_start, lexer->current, fragment_line,
            fragment_column);
        lexer_queue_token(lexer, fragment);
      }

      // Consume and queue closing backtick
      size_t backtick_start = lexer->current;
      int backtick_line = lexer->line;
      int backtick_column = lexer->column;
      lexer_advance(lexer);

      Token closing_backtick =
          lexer_make_token_at(lexer, TOKEN_BACKTICK, backtick_start,
                              lexer->current, backtick_line, backtick_column);
      lexer_queue_token(lexer, closing_backtick);
      break;
    }

    else if (c == '{') {
      // Unescaped { means code section starts
      // Queue buffered fragment if it has content
      if (lexer->current > fragment_start) {
        Token fragment = lexer_make_interpolated_fragment(
            lexer, fragment_start, lexer->current, fragment_line,
            fragment_column);
        lexer_queue_token(lexer, fragment);
      }

      // Consume and queue the {
      size_t lcurly_start = lexer->current;
      int lcurly_line = lexer->line;
      int lcurly_column = lexer->column;
      lexer_advance(lexer);

      Token lcurly =
          lexer_make_token_at(lexer, TOKEN_LBRACE, lcurly_start, lexer->current,
                              lcurly_line, lcurly_column);
      lexer_queue_token(lexer, lcurly);

      // Recursively lex tokens until we find matching }
      bool saw_rcurly = false;
      int num_of_lcurly = 1;

      while (!lexer_is_at_end(lexer)) {
        Token tok = lexer_next_token(lexer);

        if (tok.type == TOKEN_LBRACE) {
          // Nested braces (could be nested interpolated strings)
          num_of_lcurly++;
        }

        bool is_rcurly = (tok.type == TOKEN_RBRACE);
        lexer_queue_token(lexer, tok);

        if (is_rcurly) {
          if (num_of_lcurly == 1) {
            // This closes our original {
            saw_rcurly = true;
            break;
          }
          num_of_lcurly--;
        }
      }

      if (!saw_rcurly) {
        return lexer_error_token(
            lexer, "Unterminated code section in interpolated string");
      }

      // Reset fragment buffer for next section
      fragment_start = lexer->current;
      fragment_line = lexer->line;
      fragment_column = lexer->column;
    }

    else {
      // Regular character - advance (adds to fragment buffer)
      lexer_advance(lexer);
    }
  }

  if (lexer_is_at_end(lexer)) {
    return lexer_error_token(lexer, "Unterminated interpolated string");
  }

  // Return opening backtick (parts are queued)
  return bt;
}

// Scan specific token types
static Token lexer_scan_string(Lexer *lexer) {
  // Skip opening quote
  while (lexer_peek(lexer) != '"' && !lexer_is_at_end(lexer)) {
    if (lexer_peek(lexer) == '\\') {
      // Skip escape sequence (don't stop at escaped quotes)
      lexer_advance(lexer);
      if (!lexer_is_at_end(lexer)) {
        lexer_advance(lexer);
      }
    } else {
      if (lexer_peek(lexer) == '\n')
        lexer->line++;
      lexer_advance(lexer);
    }
  }

  if (lexer_is_at_end(lexer)) {
    return lexer_error_token(lexer, "Unterminated string");
  }

  // Skip closing quote
  lexer_advance(lexer);

  Token token = lexer_make_token(lexer, TOKEN_STRING);

  // Process escape sequences (without quotes)
  size_t content_start = lexer->start + 1;
  size_t content_end = lexer->current - 1;
  token.value.str_val =
      process_string_escapes(lexer, content_start, content_end, false);

  return token;
}

static Token lexer_scan_char(Lexer *lexer) {
  if (lexer_is_at_end(lexer)) {
    return lexer_error_token(lexer, "Unterminated character literal");
  }

  char c = lexer_advance(lexer);

  // Handle escape sequences
  if (c == '\\') {
    if (lexer_is_at_end(lexer)) {
      return lexer_error_token(lexer, "Unterminated escape sequence");
    }
    c = lexer_advance(lexer);
    switch (c) {
    case 'n':
      c = '\n';
      break;
    case 'r':
      c = '\r';
      break;
    case 't':
      c = '\t';
      break;
    case '\\':
      c = '\\';
      break;
    case '\'':
      c = '\'';
      break;
    case '0':
      c = '\0';
      break;
    default:
      return lexer_error_token(lexer, "Invalid escape sequence");
    }
  }

  // Expect closing quote
  if (lexer_peek(lexer) != '\'') {
    return lexer_error_token(lexer,
                             "Expected closing quote for character literal");
  }
  lexer_advance(lexer); // Skip closing quote

  Token token = lexer_make_token(lexer, TOKEN_CHAR);
  token.value.char_val = c;
  return token;
}

static Token lexer_scan_number(Lexer *lexer) {
  size_t start = lexer->start;
  while (is_digit(lexer_peek(lexer))) {
    lexer_advance(lexer);
  }

  // Check if single number 0 and next is 'x', 'X', 'b', or 'B' for hex/binary
  if (lexer->current - start == 1 && lexer->source[start] == '0') {
    char prefix = lexer_peek(lexer);

    // Hexadecimal: 0x or 0X
    if (prefix == 'x' || prefix == 'X') {
      lexer_advance(lexer); // consume 'x'
      // Scan hex digits
      while (true) {
        char c = lexer_peek(lexer);
        if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
            (c >= 'A' && c <= 'F')) {
          lexer_advance(lexer);
        } else {
          break;
        }
      }
      Token token = lexer_make_token(lexer, TOKEN_INT);
      // Parse hex value
      token.value.int_val = strtoll(&lexer->source[start], NULL, 16);
      return token;
    }

    // Binary: 0b or 0B
    if (prefix == 'b' || prefix == 'B') {
      lexer_advance(lexer); // consume 'b'
      // Scan binary digits (0 and 1 only)
      while (true) {
        char c = lexer_peek(lexer);
        if (c == '0' || c == '1') {
          lexer_advance(lexer);
        } else {
          break;
        }
      }
      Token token = lexer_make_token(lexer, TOKEN_INT);
      // Parse binary value
      token.value.int_val = strtoll(&lexer->source[start + 2], NULL, 2);
      return token;
    }
  }

  bool is_float = false;
  if (lexer_peek(lexer) == '.' && is_digit(lexer_peek_next(lexer))) {
    is_float = true;
    lexer_advance(lexer); // consume '.'
    while (is_digit(lexer_peek(lexer))) {
      lexer_advance(lexer);
    }
  }

  Token token = lexer_make_token(lexer, is_float ? TOKEN_FLOAT : TOKEN_INT);

  // Parse the value
  if (is_float) {
    token.value.float_val = strtod(&lexer->source[lexer->start], NULL);
  } else {
    token.value.int_val = strtoll(&lexer->source[lexer->start], NULL, 10);
  }

  return token;
}

static Token lexer_scan_identifier(Lexer *lexer) {
  while (is_alnum(lexer_peek(lexer))) {
    lexer_advance(lexer);
  }

  TokenType type = lexer_identifier_type(lexer);
  Token token = lexer_make_token(lexer, type);

  // Set bool values for true/false
  if (type == TOKEN_TRUE) {
    token.value.bool_val = true;
  } else if (type == TOKEN_FALSE) {
    token.value.bool_val = false;
  }

  return token;
}

// Main lexer functions
void lexer_init(Lexer *lexer, const char *source, const char *filename) {
  lexer->source = source;
  lexer->length = strlen(source);
  lexer->current = 0;
  lexer->start = 0;
  lexer->line = 1;
  lexer->column = 1;
  lexer->filename = filename;
  // Pre-allocate for 12 tokens
  lexer->token_queue = arena_alloc(&long_lived, sizeof(Token) * 12);
  lexer->queue_capacity = 12;
  lexer->queue_count = 0;
  lexer->queue_index = 0;
}

Token lexer_next_token(Lexer *lexer) {
  lexer_skip_whitespace(lexer);

  lexer->start = lexer->current;

  if (lexer_is_at_end(lexer)) {
    return lexer_make_token(lexer, TOKEN_EOF);
  }

  char c = lexer_advance(lexer);

  if (is_alpha(c))
    return lexer_scan_identifier(lexer);
  if (is_digit(c))
    return lexer_scan_number(lexer);

  switch (c) {
  case '`':
    return lexer_scan_interpolated_string(lexer);
  case '(':
    return lexer_make_token(lexer, TOKEN_LPAREN);
  case ')':
    return lexer_make_token(lexer, TOKEN_RPAREN);
  case '{':
    return lexer_make_token(lexer, TOKEN_LBRACE);
  case '}':
    return lexer_make_token(lexer, TOKEN_RBRACE);
  case '[':
    return lexer_make_token(lexer, TOKEN_LBRACKET);
  case ']':
    return lexer_make_token(lexer, TOKEN_RBRACKET);
  case ';':
    return lexer_make_token(lexer, TOKEN_SEMICOLON);
  case ',':
    return lexer_make_token(lexer, TOKEN_COMMA);
  case '.':
    if (lexer_match(lexer, '.')) {
      if (lexer_match(lexer, '.')) {
        return lexer_make_token(lexer, TOKEN_ELLIPSIS);
      }
      if (lexer_match(lexer, '=')) {
        return lexer_make_token(lexer, TOKEN_DOTDOTEQ);
      }
      return lexer_make_token(lexer, TOKEN_DOTDOT);
    }
    return lexer_make_token(lexer, TOKEN_DOT);
  case ':':
    if (lexer_match(lexer, ':')) {
      return lexer_make_token(lexer, TOKEN_MOD_SCOPE);
    }
    return lexer_make_token(lexer, TOKEN_COLON);
  case '+':
    if (lexer_match(lexer, '=')) {
      return lexer_make_token(lexer, TOKEN_PLUS_EQUAL);
    }
    if (lexer_match(lexer, '+')) {
      return lexer_make_token(lexer, TOKEN_PLUS_PLUS);
    }
    return lexer_make_token(lexer, TOKEN_PLUS);
  case '-':
    if (lexer_match(lexer, '=')) {
      return lexer_make_token(lexer, TOKEN_MINUS_EQUAL);
    }
    if (lexer_match(lexer, '-')) {
      return lexer_make_token(lexer, TOKEN_MINUS_MINUS);
    }
    return lexer_make_token(lexer, TOKEN_MINUS);
  case '*':
    return lexer_make_token(lexer, lexer_match(lexer, '=') ? TOKEN_STAR_EQUAL
                                                           : TOKEN_STAR);
  case '/':
    return lexer_make_token(lexer, lexer_match(lexer, '=') ? TOKEN_SLASH_EQUAL
                                                           : TOKEN_SLASH);
  case '"':
    return lexer_scan_string(lexer);
  case '\'':
    return lexer_scan_char(lexer);
  case '?':
    return lexer_make_token(lexer, TOKEN_QUESTION);

  case '!':
    return lexer_make_token(lexer,
                            lexer_match(lexer, '=') ? TOKEN_NE : TOKEN_NOT);
  case '=':
    if (lexer_match(lexer, '=')) {
      return lexer_make_token(lexer, TOKEN_EQ);
    } else if (lexer_match(lexer, '>')) {
      return lexer_make_token(lexer, TOKEN_FAT_ARROW);
    } else {
      return lexer_make_token(lexer, TOKEN_EQUAL);
    }
  case '<':
    if (lexer_match(lexer, '<')) {
      return lexer_make_token(lexer, TOKEN_LSHIFT);
    }
    return lexer_make_token(lexer,
                            lexer_match(lexer, '=') ? TOKEN_LE : TOKEN_LT);
  case '>':
    if (lexer_match(lexer, '>')) {
      return lexer_make_token(lexer, TOKEN_RSHIFT);
    }
    return lexer_make_token(lexer,
                            lexer_match(lexer, '=') ? TOKEN_GE : TOKEN_GT);
  case '&':
    if (lexer_match(lexer, '&')) {
      return lexer_make_token(lexer, TOKEN_AND);
    } else {
      return lexer_make_token(lexer, TOKEN_AMPERSAND);
    }
  case '|':
    if (lexer_match(lexer, '|')) {
      return lexer_make_token(lexer, TOKEN_OR);
    }
    return lexer_make_token(lexer, TOKEN_PIPE);
  case '^':
    return lexer_make_token(lexer, TOKEN_CARET);
  case '~':
    return lexer_make_token(lexer, TOKEN_TILDE);
  }

  return lexer_error_token(lexer, "Unexpected character");
}

const char *token_type_name(TokenType type) {
  switch (type) {
  case TOKEN_INT:
    return "INT";
  case TOKEN_FLOAT:
    return "FLOAT";
  case TOKEN_STRING:
    return "STRING";
  case TOKEN_CHAR:
    return "CHAR";
  case TOKEN_BOOL:
    return "BOOL";
  case TOKEN_IDENTIFIER:
    return "IDENTIFIER";
  case TOKEN_FN:
    return "FN";
  case TOKEN_RETURN:
    return "RETURN";
  case TOKEN_IF:
    return "IF";
  case TOKEN_ELSE:
    return "ELSE";
  case TOKEN_WHILE:
    return "WHILE";
  case TOKEN_LOOP:
    return "LOOP";
  case TOKEN_FOR:
    return "FOR";
  case TOKEN_TYPE:
    return "TYPE";
  case TOKEN_STRUCT:
    return "STRUCT";
  case TOKEN_LET:
    return "LET";
  case TOKEN_VAR:
    return "VAR";
  case TOKEN_TRUE:
    return "TRUE";
  case TOKEN_FALSE:
    return "FALSE";
  case TOKEN_PRINT:
    return "PRINT";
  case TOKEN_BREAK:
    return "BREAK";
  case TOKEN_CONTINUE:
    return "CONTINUE";
  case TOKEN_SIZEOF:
    return "SIZEOF";
  case TOKEN_AS:
    return "AS";
  case TOKEN_EXTERN:
    return "EXTERN";
  case TOKEN_SWITCH:
    return "SWITCH";
  case TOKEN_CASE:
    return "CASE";
  case TOKEN_DEFER:
    return "DEFER";
  case TOKEN_ENUM:
    return "ENUM";
  case TOKEN_CONTEXT:
    return "context";
  case TOKEN_INT_TYPE:
    return "INT_TYPE";
  case TOKEN_BOOL_TYPE:
    return "BOOL_TYPE";
  case TOKEN_STR_TYPE:
    return "STR_TYPE";
  case TOKEN_VOID_TYPE:
    return "VOID_TYPE";
  case TOKEN_U8_TYPE:
    return "U8_TYPE";
  case TOKEN_U16_TYPE:
    return "U16_TYPE";
  case TOKEN_U32_TYPE:
    return "U32_TYPE";
  case TOKEN_U64_TYPE:
    return "U64_TYPE";
  case TOKEN_USIZE_TYPE:
    return "USIZE_TYPE";
  case TOKEN_I8_TYPE:
    return "I8_TYPE";
  case TOKEN_I16_TYPE:
    return "I16_TYPE";
  case TOKEN_I32_TYPE:
    return "I32_TYPE";
  case TOKEN_I64_TYPE:
    return "I64_TYPE";
  case TOKEN_ISIZE_TYPE:
    return "ISIZE_TYPE";
  case TOKEN_CHAR_TYPE:
    return "CHAR_TYPE";
  case TOKEN_PLUS:
    return "PLUS";
  case TOKEN_MINUS:
    return "MINUS";
  case TOKEN_STAR:
    return "STAR";
  case TOKEN_SLASH:
    return "SLASH";
  case TOKEN_EQUAL:
    return "EQUAL";
  case TOKEN_EQ:
    return "EQ";
  case TOKEN_NE:
    return "NE";
  case TOKEN_LT:
    return "LT";
  case TOKEN_LE:
    return "LE";
  case TOKEN_GT:
    return "GT";
  case TOKEN_GE:
    return "GE";
  case TOKEN_AND:
    return "AND";
  case TOKEN_OR:
    return "OR";
  case TOKEN_NOT:
    return "NOT";
  case TOKEN_AMPERSAND:
    return "AMPERSAND";
  case TOKEN_PIPE:
    return "PIPE";
  case TOKEN_CARET:
    return "CARET";
  case TOKEN_TILDE:
    return "TILDE";
  case TOKEN_LSHIFT:
    return "LSHIFT";
  case TOKEN_RSHIFT:
    return "RSHIFT";
  case TOKEN_SEMICOLON:
    return "SEMICOLON";
  case TOKEN_COMMA:
    return "COMMA";
  case TOKEN_DOT:
    return "DOT";
  case TOKEN_DOTDOT:
    return "DOTDOT";
  case TOKEN_DOTDOTEQ:
    return "DOTDOTEQ";
  case TOKEN_LPAREN:
    return "LPAREN";
  case TOKEN_RPAREN:
    return "RPAREN";
  case TOKEN_LBRACE:
    return "LBRACE";
  case TOKEN_RBRACE:
    return "RBRACE";
  case TOKEN_LBRACKET:
    return "LBRACKET";
  case TOKEN_RBRACKET:
    return "RBRACKET";
  case TOKEN_FAT_ARROW:
    return "FAT_ARROW";
  case TOKEN_COLON:
    return "COLON";
  case TOKEN_EOF:
    return "EOF";
  case TOKEN_ERROR:
    return "ERROR";
  default:
    return "UNKNOWN";
  }
}
