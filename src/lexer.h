#ifndef LEXER_H
#define LEXER_H

#include "ast.h"  // For Location
#include "alloc.h"
#include <stddef.h>

// Token types
typedef enum {
    // Literals
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_STRING,
    TOKEN_BOOL,
    TOKEN_IDENTIFIER,

    // Keywords
    TOKEN_FN,
    TOKEN_RETURN,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_WHILE,
    TOKEN_TYPE,
    TOKEN_STRUCT,
    TOKEN_LET, // let a = 5; a is constant
    TOKEN_VAR, // var b = 5; a is a mutable variable
    TOKEN_TRUE,
    TOKEN_FALSE,

    // Types (built-in)
    TOKEN_INT_TYPE,    // "int"
    TOKEN_FLOAT_TYPE,  // "float"
    TOKEN_BOOL_TYPE,   // "bool"
    TOKEN_STR_TYPE,    // "str"
    TOKEN_VOID_TYPE,   // "void"

    // Operators
    TOKEN_PLUS,        // +
    TOKEN_MINUS,       // -
    TOKEN_STAR,        // *
    TOKEN_SLASH,       // /
    TOKEN_EQUAL,       // =
    TOKEN_EQ,          // ==
    TOKEN_NE,          // !=
    TOKEN_LT,          // <
    TOKEN_LE,          // <=
    TOKEN_GT,          // >
    TOKEN_GE,          // >=
    TOKEN_AND,         // &&
    TOKEN_OR,          // ||
    TOKEN_NOT,         // !
    TOKEN_AMPERSAND,   // &

    // Punctuation
    TOKEN_SEMICOLON,   // ;
    TOKEN_COMMA,       // ,
    TOKEN_DOT,         // .
    TOKEN_LPAREN,      // (
    TOKEN_RPAREN,      // )
    TOKEN_LBRACE,      // {
    TOKEN_RBRACE,      // }
    TOKEN_LBRACKET,    // [
    TOKEN_RBRACKET,    // ]
    TOKEN_FAT_ARROW,       // =>
    TOKEN_COLON,       // :

    // Special
    TOKEN_EOF,
    TOKEN_ERROR
} TokenType;

// Token structure
typedef struct {
    TokenType type;
    char *lexeme;           // String representation (allocated)
    Location location;      // Source location

    // Parsed values for literals
    union {
        long long int_val;
        double float_val;
        bool bool_val;
        char *str_val;      // For string literals (processed, allocated)
    } value;
} Token;

// Lexer state
typedef struct {
    const char *source;     // Source code
    size_t length;          // Source length
    size_t current;         // Current position
    size_t start;           // Start of current token
    int line;               // Current line
    int column;             // Current column
    const char *filename;   // Source filename
} Lexer;

// Lexer functions
void lexer_init(Lexer *lexer, const char *source, const char *filename);
Token lexer_next_token(Lexer *lexer);
const char *token_type_name(TokenType type);  // For debugging

#endif
