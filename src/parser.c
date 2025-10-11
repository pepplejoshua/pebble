#include "parser.h"
#include "alloc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External allocator
extern Arena long_lived;

// ============================================================================
// BASIC PARSER UTILITIES
// ============================================================================

void parser_init(Parser *parser, const char *source, const char *filename) {
    lexer_init(&parser->lexer, source, filename);
    parser->had_error = false;
    parser->panic_mode = false;

    // Prime the parser with the first token
    parser_advance(parser);
}

void parser_advance(Parser *parser) {
    parser->previous = parser->current;

    // Skip any error tokens
    while (true) {
        parser->current = lexer_next_token(&parser->lexer);
        if (parser->current.type != TOKEN_ERROR) break;

        parser_error_at_current(parser, parser->current.lexeme);
    }
}

bool parser_check(Parser *parser, TokenType type) {
    return parser->current.type == type;
}

bool parser_match(Parser *parser, TokenType type) {
    if (!parser_check(parser, type)) return false;
    parser_advance(parser);
    return true;
}

Token parser_consume(Parser *parser, TokenType type, const char *message) {
    if (parser->current.type == type) {
        parser_advance(parser);
        return parser->previous;
    }

    parser_error_at_current(parser, message);
    return parser->current;  // Return current token for error recovery
}

// ============================================================================
// ERROR HANDLING
// ============================================================================
static void parser_error_at(Parser *parser, Token *token, const char *message) {
    // Don't report more errors if we're already in panic mode
    if (parser->panic_mode) return;
    parser->panic_mode = true;

    fprintf(stderr, "[line %d] Error", token->location.line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Error token - message already printed
    } else {
        fprintf(stderr, " at '%s'", token->lexeme);
    }

    fprintf(stderr, ": %s\n", message);
    parser->had_error = true;
}

void parser_error_at_current(Parser *parser, const char *message) {
    parser_error_at(parser, &parser->current, message);
}

void parser_error_at_previous(Parser *parser, const char *message) {
    parser_error_at(parser, &parser->previous, message);
}

void parser_synchronize(Parser *parser) {
    parser->panic_mode = false;

    while (parser->current.type != TOKEN_EOF) {
        if (parser->previous.type == TOKEN_SEMICOLON) return;

        switch (parser->current.type) {
            case TOKEN_FN:
            case TOKEN_VAR:
            case TOKEN_LET:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_RETURN:
                return;
            default:
                break;
        }

        parser_advance(parser);
    }
}

void parser_error(Parser *parser, const char *message) {
    parser_error_at_current(parser, message);
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

BinaryOp token_to_binary_op(TokenType type) {
    switch (type) {
        case TOKEN_PLUS:    return BINOP_ADD;
        case TOKEN_MINUS:   return BINOP_SUB;
        case TOKEN_STAR:    return BINOP_MUL;
        case TOKEN_SLASH:   return BINOP_DIV;
        case TOKEN_EQ:      return BINOP_EQ;
        case TOKEN_NE:      return BINOP_NE;
        case TOKEN_LT:      return BINOP_LT;
        case TOKEN_LE:      return BINOP_LE;
        case TOKEN_GT:      return BINOP_GT;
        case TOKEN_GE:      return BINOP_GE;
        case TOKEN_AND:     return BINOP_AND;
        case TOKEN_OR:      return BINOP_OR;
        default:
            // This should never happen if called correctly
            fprintf(stderr, "Invalid binary operator token: %d\n", type);
            exit(1);
    }
}

UnaryOp token_to_unary_op(TokenType type) {
    switch (type) {
        case TOKEN_MINUS:   return UNOP_NEG;
        case TOKEN_NOT:     return UNOP_NOT;
        default:
            // This should never happen if called correctly
            fprintf(stderr, "Invalid unary operator token: %d\n", type);
            exit(1);
    }
}

// ============================================================================
// AST NODE CREATION HELPERS
// ============================================================================

// Helper to duplicate strings into arena
static char *str_dup(const char *str) {
    if (!str) return NULL;
    size_t len = strlen(str) + 1;
    char *copy = arena_alloc(&long_lived, len);
    memcpy(copy, str, len);
    return copy;
}

// Helper to create basic AST node
static AstNode *alloc_node(AstKind kind, Location loc) {
    AstNode *node = arena_alloc(&long_lived, sizeof(AstNode));
    node->kind = kind;
    node->loc = loc;
    return node;
}

// ============================================================================
// TOP-LEVEL PARSING
// ============================================================================

AstNode *parse_program(Parser *parser) {
    if (parser->current.type == TOKEN_EOF) {
        return NULL;
    }

    return parse_declaration(parser);
}

AstNode *parse_declaration(Parser *parser) {
    if (parser_match(parser, TOKEN_FN)) {
        return parse_function_decl(parser);
    }
    if (parser_match(parser, TOKEN_LET) || parser_match(parser, TOKEN_VAR)) {
        return parse_variable_decl(parser);
    }
    if (parser_match(parser, TOKEN_TYPE)) {
        return parse_type_decl(parser);
    }

    parser_error(parser, "Expected declaration");
    parser_synchronize(parser);
    return NULL;
}

AstNode *parse_function_decl(Parser *parser) {
    // fn name(params) return_type { body }
    // fn name(params) return_type => expr

    Token name = parser_consume(parser, TOKEN_IDENTIFIER, "Expected function name");

    parser_consume(parser, TOKEN_LPAREN, "Expected '(' after function name");

    // Parse parameters
    FuncParam *params = NULL;
    size_t param_count = 0;

    if (!parser_check(parser, TOKEN_RPAREN)) {
        // We have parameters
        // For now, allocate space for up to 16 parameters (we'll improve this later)
        params = arena_alloc(&long_lived, 16 * sizeof(FuncParam));

        do {
            if (param_count >= 16) {
                parser_error(parser, "Too many parameters (max 16)");
                break;
            }

            Token param_name = parser_consume(parser, TOKEN_IDENTIFIER, "Expected parameter name");
            AstNode *param_type = parse_type_expression(parser);

            params[param_count].name = param_name.lexeme;  // Already allocated by lexer
            params[param_count].type = param_type;
            param_count++;

        } while (parser_match(parser, TOKEN_COMMA));
    }

    parser_consume(parser, TOKEN_RPAREN, "Expected ')' after parameters");

    // Return type
    AstNode *return_type = parse_type_expression(parser);

    // Body: either { ... } or => expr
    AstNode *body = NULL;
    if (parser_match(parser, TOKEN_FAT_ARROW)) {
        // Expression function: fn name(...) type => expr
        AstNode *expr = parse_expression(parser);
        // Wrap the expression in a return statement, then in a block
        AstNode *ret_stmt = alloc_node(AST_STMT_RETURN, expr->loc);
        ret_stmt->data.return_stmt.expr = expr;
        AstNode **stmts = arena_alloc(&long_lived, sizeof(AstNode*));
        stmts[0] = ret_stmt;
        body = alloc_node(AST_STMT_BLOCK, expr->loc);
        body->data.block_stmt.stmts = stmts;
        body->data.block_stmt.stmt_count = 1;
    } else {
        // Regular function: fn name(...) type { ... }
        body = parse_block_stmt(parser);
    }

    AstNode *func = alloc_node(AST_DECL_FUNCTION, name.location);
    func->data.func_decl.name = str_dup(name.lexeme);
    func->data.func_decl.params = params;
    func->data.func_decl.param_count = param_count;
    func->data.func_decl.return_type = return_type;
    func->data.func_decl.body = body;
    return func;
}

AstNode *parse_variable_decl(Parser *parser) {
    // let name = expr; or let name type = expr;
    // var name = expr; or var name type = expr;

    bool is_mutable = (parser->previous.type == TOKEN_VAR);

    Token name = parser_consume(parser, TOKEN_IDENTIFIER, "Expected variable name");

    // Optional type annotation
    AstNode *type_expr = NULL;
    if (!parser_check(parser, TOKEN_EQUAL) && !parser_check(parser, TOKEN_SEMICOLON)) {
        type_expr = parse_type_expression(parser);
    }


    // Initializer
    AstNode *init = NULL;
    if (parser_match(parser, TOKEN_EQUAL)) {
        init = parse_expression(parser);
    }

    parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after variable declaration");

    // Create appropriate AST node
    if (is_mutable) {
        AstNode *var = alloc_node(AST_DECL_VARIABLE, name.location);
        var->data.var_decl.name = str_dup(name.lexeme);
        var->data.var_decl.type_expr = type_expr;
        var->data.var_decl.init = init;
        return var;
    } else {
        AstNode *const_var = alloc_node(AST_DECL_CONSTANT, name.location);
        const_var->data.const_decl.name = str_dup(name.lexeme);
        const_var->data.const_decl.type_expr = type_expr;
        const_var->data.const_decl.value = init;
        return const_var;
    }
}

AstNode *parse_type_decl(Parser *parser) {
    // type Name = TypeExpr;

    Token name = parser_consume(parser, TOKEN_IDENTIFIER, "Expected type name");
    parser_consume(parser, TOKEN_EQUAL, "Expected '=' after type name");
    AstNode *type_expr = parse_type_expression(parser);
    parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after type declaration");

    // Create AST node (we'll define ast_type_decl if it doesn't exist)
    AstNode *node = arena_alloc(&long_lived, sizeof(AstNode));
    node->kind = AST_DECL_TYPE;
    node->loc = name.location;
    node->data.type_decl.name = str_dup(name.lexeme);
    node->data.type_decl.type_expr = type_expr;

    return node;
}
