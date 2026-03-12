#ifndef PARSER2_H
#define PARSER2_H

#include "ast.h"
#include "lexer.h"
#include "diagnostics.h"

typedef struct {
    Lexer lexer;
    Token current;
    Token previous;

    // Error handling
    DiagnosticContext *diagnostics;
    bool had_error;
    size_t max_errors;      // Prevent error spam (e.g., 50)

    // Stack overflow protection
    size_t nesting_depth;
    size_t max_depth;       // e.g., 200

    // For error reporting
    const char *abs_file_path;
} Parser2;

// External interface (same names as parser.h)
void parser_init(Parser2 *parser, const char *source, const char *filename, const char *abs_file_path);
AstNode *parse_program(Parser2 *parser);
BinaryOp token_to_binary_op(TokenType type);
UnaryOp token_to_unary_op(TokenType type);

// Internal interface (prefixed)
void parser2_advance(Parser2 *parser);
bool parser2_check(Parser2 *parser, TokenType type);
bool parser2_match(Parser2 *parser, TokenType type);
Token parser2_consume(Parser2 *parser, TokenType type, const char *message);

// Error handling
bool parser2_handle_error(Parser2 *parser, const char *expected);
AstNode *parser2_create_error_node(Parser2 *parser, const char *message);
void parser2_synchronize(Parser2 *parser);

// Core parsing functions (internal)
AstNode *parser2_declaration(Parser2 *parser);
AstNode *parser2_statement(Parser2 *parser);
AstNode *parser2_expression(Parser2 *parser);

#endif
