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
    // Parse all top-level declarations
    AstNode **decls = arena_alloc(&long_lived, 64 * sizeof(AstNode*));
    size_t decl_count = 0;

    while (!parser_check(parser, TOKEN_EOF)) {
        if (decl_count >= 64) {
            parser_error(parser, "Too many top-level declarations (max 64)");
            break;
        }

        AstNode *decl = parse_declaration(parser);
        if (decl != NULL) {
            decls[decl_count++] = decl;
        }

        // If we had an error and didn't make progress, break to avoid infinite loop
        if (parser->had_error && parser->current.type == parser->previous.type) {
            break;
        }
    }

    // Create a program node to hold all declarations
    Location loc = {
        .file = parser->lexer.filename,
        .line = 1,
        .column = 1
    };

    AstNode *program = alloc_node(AST_STMT_BLOCK, loc);
    program->data.block_stmt.stmts = decls;
    program->data.block_stmt.stmt_count = decl_count;

    return program;
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
        parser_consume(parser, TOKEN_LBRACE, "Expected '{' before function body");
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

// ============================================================================
// STATEMENT PARSING
// ============================================================================

AstNode *parse_statement(Parser *parser) {
    if (parser_match(parser, TOKEN_RETURN)) {
        return parse_return_stmt(parser);
    }
    if (parser_match(parser, TOKEN_IF)) {
        return parse_if_stmt(parser);
    }
    if (parser_match(parser, TOKEN_WHILE)) {
        return parse_while_stmt(parser);
    }
    if (parser_match(parser, TOKEN_LBRACE)) {
        return parse_block_stmt(parser);
    }
    if (parser_match(parser, TOKEN_LET) || parser_match(parser, TOKEN_VAR)) {
        return parse_variable_decl(parser);  // Local variables
    }

    // Check for assignment: identifier = expr
    if (parser_check(parser, TOKEN_IDENTIFIER)) {
        // Look ahead for assignment
        // We need to peek ahead to see if there's an = after the identifier
        // For now, let's parse as expression and convert if needed
        return parse_assignment_stmt(parser);
    }

    // Otherwise, it's an expression statement
    return parse_expression_stmt(parser);
}

AstNode *parse_return_stmt(Parser *parser) {
    Location loc = parser->previous.location;

    AstNode *expr = NULL;
    if (!parser_check(parser, TOKEN_SEMICOLON)) {
        expr = parse_expression(parser);
    }

    parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after return statement");

    AstNode *stmt = alloc_node(AST_STMT_RETURN, loc);
    stmt->data.return_stmt.expr = expr;
    return stmt;
}

AstNode *parse_if_stmt(Parser *parser) {
    Location loc = parser->previous.location;

    AstNode *cond = parse_expression(parser);
    AstNode *then_branch = parse_statement(parser);

    AstNode *else_branch = NULL;
    if (parser_match(parser, TOKEN_ELSE)) {
        else_branch = parse_statement(parser);
    }

    AstNode *stmt = alloc_node(AST_STMT_IF, loc);
    stmt->data.if_stmt.cond = cond;
    stmt->data.if_stmt.then_branch = then_branch;
    stmt->data.if_stmt.else_branch = else_branch;
    return stmt;
}

AstNode *parse_while_stmt(Parser *parser) {
    Location loc = parser->previous.location;

    AstNode *cond = parse_expression(parser);
    AstNode *body = parse_statement(parser);

    AstNode *stmt = alloc_node(AST_STMT_WHILE, loc);
    stmt->data.while_stmt.cond = cond;
    stmt->data.while_stmt.body = body;
    return stmt;
}

AstNode *parse_block_stmt(Parser *parser) {
    Location loc = parser->previous.location;

    // Parse statements until we hit }
    AstNode **stmts = arena_alloc(&long_lived, 64 * sizeof(AstNode*));  // Max 64 statements for now
    size_t stmt_count = 0;

    while (!parser_check(parser, TOKEN_RBRACE) && !parser_check(parser, TOKEN_EOF)) {
        if (stmt_count >= 64) {
            parser_error(parser, "Too many statements in block (max 64)");
            break;
        }

        AstNode *stmt = parse_statement(parser);
        if (stmt != NULL) {  // Add NULL check
            stmts[stmt_count++] = stmt;
        }
    }

    parser_consume(parser, TOKEN_RBRACE, "Expected '}' after block");

    AstNode *block = alloc_node(AST_STMT_BLOCK, loc);
    block->data.block_stmt.stmts = stmts;
    block->data.block_stmt.stmt_count = stmt_count;
    return block;
}

AstNode *parse_assignment_stmt(Parser *parser) {
    // We know current token is identifier
    AstNode *lhs = parse_expression(parser);  // This will parse the identifier

    if (parser_match(parser, TOKEN_EQUAL)) {
        Location loc = parser->previous.location;
        AstNode *rhs = parse_expression(parser);
        parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after assignment");

        AstNode *assign = alloc_node(AST_STMT_ASSIGN, loc);
        assign->data.assign_stmt.lhs = lhs;
        assign->data.assign_stmt.rhs = rhs;
        return assign;
    } else {
        // Not an assignment, it's an expression statement
        parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after expression");

        AstNode *expr_stmt = alloc_node(AST_STMT_EXPR, lhs->loc);
        expr_stmt->data.expr_stmt.expr = lhs;
        return expr_stmt;
    }
}

AstNode *parse_expression_stmt(Parser *parser) {
    AstNode *expr = parse_expression(parser);
    parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after expression");

    AstNode *stmt = alloc_node(AST_STMT_EXPR, expr->loc);
    stmt->data.expr_stmt.expr = expr;
    return stmt;
}

// ============================================================================
// EXPRESSION PARSING
// ============================================================================

AstNode *parse_expression(Parser *parser) {
    return parse_or_expr(parser);
}

AstNode *parse_or_expr(Parser *parser) {
    AstNode *left = parse_and_expr(parser);

    while (parser_match(parser, TOKEN_OR)) {
        Location loc = parser->previous.location;
        AstNode *right = parse_and_expr(parser);

        AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, loc);
        binop->data.binop.op = BINOP_OR;
        binop->data.binop.left = left;
        binop->data.binop.right = right;
        left = binop;
    }

    return left;
}

AstNode *parse_and_expr(Parser *parser) {
    AstNode *left = parse_equality(parser);

    while (parser_match(parser, TOKEN_AND)) {
        Location loc = parser->previous.location;
        AstNode *right = parse_equality(parser);

        AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, loc);
        binop->data.binop.op = BINOP_AND;
        binop->data.binop.left = left;
        binop->data.binop.right = right;
        left = binop;
    }

    return left;
}

AstNode *parse_equality(Parser *parser) {
    AstNode *left = parse_comparison(parser);

    while (parser_match(parser, TOKEN_EQ) || parser_match(parser, TOKEN_NE)) {
        Token op = parser->previous;
        AstNode *right = parse_comparison(parser);

        AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
        binop->data.binop.op = token_to_binary_op(op.type);
        binop->data.binop.left = left;
        binop->data.binop.right = right;
        left = binop;
    }

    return left;
}

AstNode *parse_comparison(Parser *parser) {
    AstNode *left = parse_term(parser);

    while (parser_match(parser, TOKEN_LT) || parser_match(parser, TOKEN_LE) ||
           parser_match(parser, TOKEN_GT) || parser_match(parser, TOKEN_GE)) {
        Token op = parser->previous;
        AstNode *right = parse_term(parser);

        AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
        binop->data.binop.op = token_to_binary_op(op.type);
        binop->data.binop.left = left;
        binop->data.binop.right = right;
        left = binop;
    }

    return left;
}

AstNode *parse_term(Parser *parser) {
    AstNode *left = parse_factor(parser);

    while (parser_match(parser, TOKEN_PLUS) || parser_match(parser, TOKEN_MINUS)) {
        Token op = parser->previous;
        AstNode *right = parse_factor(parser);

        AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
        binop->data.binop.op = token_to_binary_op(op.type);
        binop->data.binop.left = left;
        binop->data.binop.right = right;
        left = binop;
    }

    return left;
}

AstNode *parse_factor(Parser *parser) {
    AstNode *left = parse_unary(parser);

    while (parser_match(parser, TOKEN_STAR) || parser_match(parser, TOKEN_SLASH)) {
        Token op = parser->previous;
        AstNode *right = parse_unary(parser);

        AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
        binop->data.binop.op = token_to_binary_op(op.type);
        binop->data.binop.left = left;
        binop->data.binop.right = right;
        left = binop;
    }

    return left;
}

AstNode *parse_unary(Parser *parser) {
    if (parser_match(parser, TOKEN_MINUS) || parser_match(parser, TOKEN_NOT)) {
        Token op = parser->previous;
        AstNode *operand = parse_unary(parser);  // Right-associative (allow chaining)

        AstNode *unop = alloc_node(AST_EXPR_UNARY_OP, op.location);
        unop->data.unop.op = token_to_unary_op(op.type);
        unop->data.unop.operand = operand;
        return unop;
    }

    return parse_postfix(parser);
}

AstNode *parse_postfix(Parser *parser) {
    AstNode *expr = parse_primary(parser);

    while (true) {
        if (parser_match(parser, TOKEN_LPAREN)) {
            // Function call: expr(args)
            expr = parse_call(parser, expr);
        } else if (parser_match(parser, TOKEN_LBRACKET)) {
            // Array indexing: expr[index]
            expr = parse_index(parser, expr);
        } else if (parser_match(parser, TOKEN_DOT)) {
            // Member access: expr.member
            expr = parse_member(parser, expr);
        } else {
            break;
        }
    }

    return expr;
}

AstNode *parse_call(Parser *parser, AstNode *func) {
    Location loc = parser->previous.location;

    // Parse arguments
    AstNode **args = NULL;
    size_t arg_count = 0;

    if (!parser_check(parser, TOKEN_RPAREN)) {
        args = arena_alloc(&long_lived, 16 * sizeof(AstNode*));  // Max 16 args

        do {
            if (arg_count >= 16) {
                parser_error(parser, "Too many arguments (max 16)");
                break;
            }
            args[arg_count++] = parse_expression(parser);
        } while (parser_match(parser, TOKEN_COMMA));
    }

    parser_consume(parser, TOKEN_RPAREN, "Expected ')' after arguments");

    AstNode *call = alloc_node(AST_EXPR_CALL, loc);
    call->data.call.func = func;
    call->data.call.args = args;
    call->data.call.arg_count = arg_count;
    return call;
}

AstNode *parse_index(Parser *parser, AstNode *array) {
    Location loc = parser->previous.location;

    AstNode *index = parse_expression(parser);
    parser_consume(parser, TOKEN_RBRACKET, "Expected ']' after index");

    AstNode *idx = alloc_node(AST_EXPR_INDEX, loc);
    idx->data.index_expr.array = array;
    idx->data.index_expr.index = index;
    return idx;
}

AstNode *parse_member(Parser *parser, AstNode *object) {
    Location loc = parser->previous.location;

    Token member = parser_consume(parser, TOKEN_IDENTIFIER, "Expected member name after '.'");

    AstNode *mem = alloc_node(AST_EXPR_MEMBER, loc);
    mem->data.member_expr.object = object;
    mem->data.member_expr.member = str_dup(member.lexeme);
    return mem;
}

AstNode *parse_primary(Parser *parser) {
    // Literals
    if (parser_match(parser, TOKEN_INT)) {
        Token num = parser->previous;
        AstNode *lit = alloc_node(AST_EXPR_LITERAL_INT, num.location);
        lit->data.int_lit.value = num.value.int_val;
        return lit;
    }

    if (parser_match(parser, TOKEN_FLOAT)) {
        Token num = parser->previous;
        AstNode *lit = alloc_node(AST_EXPR_LITERAL_FLOAT, num.location);
        lit->data.float_lit.value = num.value.float_val;
        return lit;
    }

    if (parser_match(parser, TOKEN_STRING)) {
        Token str = parser->previous;
        AstNode *lit = alloc_node(AST_EXPR_LITERAL_STRING, str.location);
        lit->data.str_lit.value = str.value.str_val;  // Already duplicated by lexer
        return lit;
    }

    if (parser_match(parser, TOKEN_TRUE) || parser_match(parser, TOKEN_FALSE)) {
        Token bool_tok = parser->previous;
        AstNode *lit = alloc_node(AST_EXPR_LITERAL_BOOL, bool_tok.location);
        lit->data.bool_lit.value = bool_tok.value.bool_val;
        return lit;
    }

    // Identifier
    if (parser_match(parser, TOKEN_IDENTIFIER)) {
        Token ident = parser->previous;
        AstNode *id = alloc_node(AST_EXPR_IDENTIFIER, ident.location);
        id->data.ident.name = str_dup(ident.lexeme);
        return id;
    }

    // Parenthesized expression
    if (parser_match(parser, TOKEN_LPAREN)) {
        AstNode *expr = parse_expression(parser);
        parser_consume(parser, TOKEN_RPAREN, "Expected ')' after expression");
        return expr;
    }

    parser_error(parser, "Expected expression");
    return NULL;
}

// ============================================================================
// TYPE EXPRESSION PARSING
// ============================================================================

AstNode *parse_type_expression(Parser *parser) {
    AstNode *type = NULL;

    // Pointer type: *T
    if (parser_match(parser, TOKEN_STAR)) {
        Location loc = parser->previous.location;
        AstNode *base = parse_type_expression(parser);  // Recursive for **T, etc.

        type = alloc_node(AST_TYPE_POINTER, loc);
        type->data.type_pointer.base = base;
        return type;
    }

    // Array or slice: [N]T or []T
    if (parser_match(parser, TOKEN_LBRACKET)) {
        Location loc = parser->previous.location;

        // Check for slice []T vs array [N]T
        if (parser_match(parser, TOKEN_RBRACKET)) {
            // Slice: []T
            AstNode *element = parse_type_expression(parser);
            type = alloc_node(AST_TYPE_SLICE, loc);
            type->data.type_slice.element = element;
            return type;
        } else {
            // Array: [N]T
            if (!parser_check(parser, TOKEN_INT)) {
                parser_error(parser, "Expected array size");
                return NULL;
            }
            Token size_tok = parser->previous;
            parser_advance(parser);
            size_t size = (size_t)size_tok.value.int_val;

            parser_consume(parser, TOKEN_RBRACKET, "Expected ']' after array size");
            AstNode *element = parse_type_expression(parser);

            type = alloc_node(AST_TYPE_ARRAY, loc);
            type->data.type_array.element = element;
            type->data.type_array.size = size;
            return type;
        }
    }

    // Built-in types
    if (parser_match(parser, TOKEN_INT_TYPE)) {
        type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
        type->data.type_named.name = str_dup("int");
        return type;
    }

    if (parser_match(parser, TOKEN_FLOAT_TYPE)) {
        type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
        type->data.type_named.name = str_dup("float");
        return type;
    }

    if (parser_match(parser, TOKEN_BOOL_TYPE)) {
        type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
        type->data.type_named.name = str_dup("bool");
        return type;
    }

    if (parser_match(parser, TOKEN_STR_TYPE)) {
        type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
        type->data.type_named.name = str_dup("str");
        return type;
    }

    if (parser_match(parser, TOKEN_VOID_TYPE)) {
        type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
        type->data.type_named.name = str_dup("void");
        return type;
    }

    // Custom/named types
    if (parser_match(parser, TOKEN_IDENTIFIER)) {
        type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
        type->data.type_named.name = str_dup(parser->previous.lexeme);
        return type;
    }

    parser_error(parser, "Expected type");
    return NULL;
}
