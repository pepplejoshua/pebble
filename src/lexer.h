#ifndef LEXER_H
#define LEXER_H

#include "alloc.h"
#include "ast.h" // For Location
#include <stddef.h>

// Token types
typedef enum {
  // Literals
  TOKEN_INT,
  TOKEN_FLOAT,
  TOKEN_STRING,
  TOKEN_CHAR,
  TOKEN_BOOL,
  TOKEN_IDENTIFIER,

  // Keywords
  TOKEN_FN,
  TOKEN_RETURN,
  TOKEN_IF,
  TOKEN_ELSE,
  TOKEN_WHILE,
  TOKEN_LOOP,
  TOKEN_FOR,
  TOKEN_TYPE,
  TOKEN_STRUCT,
  TOKEN_LET, // let a = 5; a is constant
  TOKEN_VAR, // var b = 5; a is a mutable variable
  TOKEN_TRUE,
  TOKEN_FALSE,
  TOKEN_PRINT,
  TOKEN_BREAK,
  TOKEN_CONTINUE,
  TOKEN_SIZEOF,
  TOKEN_AS,
  TOKEN_NIL,
  TOKEN_EXTERN,
  TOKEN_SWITCH,
  TOKEN_CASE,
  TOKEN_DEFER,
  TOKEN_ENUM,
  TOKEN_SOME,
  TOKEN_NONE,

  // Types (built-in)
  TOKEN_INT_TYPE,    // "int"
  TOKEN_BOOL_TYPE,   // "bool"
  TOKEN_STR_TYPE,    // "str"
  TOKEN_VOID_TYPE,   // "void"
  TOKEN_U8_TYPE,     // "u8"
  TOKEN_U16_TYPE,    // "u16"
  TOKEN_U32_TYPE,    // "u32"
  TOKEN_U64_TYPE,    // "u64"
  TOKEN_USIZE_TYPE,  // "usize"
  TOKEN_I8_TYPE,     // "i8"
  TOKEN_I16_TYPE,    // "i16"
  TOKEN_I32_TYPE,    // "i32"
  TOKEN_I64_TYPE,    // "i64"
  TOKEN_ISIZE_TYPE,  // "isize"
  TOKEN_CHAR_TYPE,   // "char"

  // Operators
  TOKEN_PLUS,             // +
  TOKEN_MINUS,            // -
  TOKEN_STAR,             // *
  TOKEN_SLASH,            // /
  TOKEN_PLUS_PLUS,        // ++
  TOKEN_PLUS_EQUAL,       // +=
  TOKEN_MINUS_EQUAL,      // -=
  TOKEN_STAR_EQUAL,       // *=
  TOKEN_SLASH_EQUAL,      // /=
  TOKEN_EQUAL,            // =
  TOKEN_EQ,               // ==
  TOKEN_NE,               // !=
  TOKEN_LT,               // <
  TOKEN_LE,               // <=
  TOKEN_GT,               // >
  TOKEN_GE,               // >=
  TOKEN_AND,              // &&
  TOKEN_OR,               // ||
  TOKEN_NOT,              // !
  TOKEN_QUESTION,         // ?
  TOKEN_AMPERSAND,        // &
  TOKEN_PIPE,             // |
  TOKEN_CARET,            // ^
  TOKEN_TILDE,            // ~
  TOKEN_LSHIFT,           // <<
  TOKEN_RSHIFT,           // >>

  // Punctuation
  TOKEN_SEMICOLON, // ;
  TOKEN_COMMA,     // ,
  TOKEN_DOT,       // .
  TOKEN_DOTDOT,    // ..
  TOKEN_DOTDOTEQ,  // ..=
  TOKEN_ELLIPSIS,  // ...
  TOKEN_LPAREN,    // (
  TOKEN_RPAREN,    // )
  TOKEN_LBRACE,    // {
  TOKEN_RBRACE,    // }
  TOKEN_LBRACKET,  // [
  TOKEN_RBRACKET,  // ]
  TOKEN_FAT_ARROW, // =>
  TOKEN_COLON,     // :

  // Special
  TOKEN_EOF,
  TOKEN_ERROR
} TokenType;

// Token structure
typedef struct {
  TokenType type;
  char *lexeme;      // String representation (allocated)
  Location location; // Source location

  // Parsed values for literals
  union {
    long long int_val;
    double float_val;
    char char_val;
    bool bool_val;
    char *str_val; // For string literals (processed, allocated)
  } value;
} Token;

// Lexer state
typedef struct {
  const char *source;   // Source code
  size_t length;        // Source length
  size_t current;       // Current position
  size_t start;         // Start of current token
  int line;             // Current line
  int column;           // Current column
  const char *filename; // Source filename
} Lexer;

// Lexer functions
void lexer_init(Lexer *lexer, const char *source, const char *filename);
Token lexer_next_token(Lexer *lexer);
const char *token_type_name(TokenType type); // For debugging

#endif
