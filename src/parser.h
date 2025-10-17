#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lexer.h"

// Parser state
typedef struct {
  Lexer lexer;
  Token current;
  Token previous;
  bool had_error;
  bool panic_mode; // For error recovery
} Parser;

// Main parser functions
void parser_init(Parser *parser, const char *source, const char *filename);
AstNode *
parse_program(Parser *parser); // Returns list of top-level declarations

// Top-level declarations
AstNode *parse_declaration(Parser *parser);
AstNode *parse_function_decl(Parser *parser);
AstNode *parse_variable_decl(Parser *parser); // Handles both let/var
AstNode *parse_type_decl(Parser *parser);
AstNode *parse_print_stmt(Parser *parser);

// Statements
AstNode *parse_statement(Parser *parser);
AstNode *parse_return_stmt(Parser *parser);
AstNode *parse_if_stmt(Parser *parser);
AstNode *parse_while_stmt(Parser *parser);
AstNode *parse_loop_stmt(Parser *parser);
AstNode *parse_for_stmt(Parser *parser);
AstNode *parse_block_stmt(Parser *parser);
AstNode *parse_assignment_stmt(Parser *parser);

// Expressions (recursive descent by precedence level)
AstNode *parse_expression(Parser *parser); // Entry point
AstNode *parse_or_expr(Parser *parser);    // ||
AstNode *parse_and_expr(Parser *parser);   // &&
AstNode *parse_equality(Parser *parser);   // == !=
AstNode *parse_comparison(Parser *parser); // < <= > >=
AstNode *parse_term(Parser *parser);       // + -
AstNode *parse_factor(Parser *parser);     // * /
AstNode *parse_unary(Parser *parser);      // ! -
AstNode *parse_postfix(Parser *parser);    // calls, indexing, member access
AstNode *parse_call(Parser *parser, AstNode *func);     // func(arg1, arg2)
AstNode *parse_index(Parser *parser, AstNode *array);   // array[index]
AstNode *parse_member(Parser *parser, AstNode *object); // object.member
AstNode *parse_primary(Parser *parser); // literals, identifiers, grouping

// Type expressions
AstNode *parse_type_expression(Parser *parser);

// Utility functions
void parser_advance(Parser *parser);
bool parser_check(Parser *parser, TokenType type);
bool parser_match(Parser *parser, TokenType type);
Token parser_consume(Parser *parser, TokenType type, const char *message);

// Error handling
void parser_error(Parser *parser, const char *message);
void parser_error_at_current(Parser *parser, const char *message);
void parser_error_at_previous(Parser *parser, const char *message);
void parser_synchronize(Parser *parser); // Skip to next statement boundary

// Helper functions
BinaryOp token_to_binary_op(TokenType type);
UnaryOp token_to_unary_op(TokenType type);

#endif
