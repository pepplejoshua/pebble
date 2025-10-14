#include "ast.h"
#include "alloc.h"
#include <string.h>
#include <assert.h>

// External allocators (will be defined in main.c)
extern Arena long_lived;

// Create AST node with basic setup
static AstNode *ast_node_create(AstKind kind, Location loc) {
    AstNode *node = arena_alloc(&long_lived, sizeof(AstNode));
    node->kind = kind;
    node->loc = loc;
    return node;
}

// Function declaration
AstNode *ast_func_decl(Location loc, const char *name, FuncParam *params, size_t param_count,
                       AstNode *return_type, AstNode *body) {
    AstNode *node = ast_node_create(AST_DECL_FUNCTION, loc);
    node->data.func_decl.name = str_dup(name);
    node->data.func_decl.params = params;  // Already allocated
    node->data.func_decl.param_count = param_count;
    node->data.func_decl.return_type = return_type;
    node->data.func_decl.body = body;
    return node;
}

// Variable declaration
AstNode *ast_var_decl(Location loc, const char *name, AstNode *type_expr, AstNode *init) {
    AstNode *node = ast_node_create(AST_DECL_VARIABLE, loc);
    node->data.var_decl.name = str_dup(name);
    node->data.var_decl.type_expr = type_expr;
    node->data.var_decl.init = init;
    return node;
}

// Constant declaration
AstNode *ast_const_decl(Location loc, const char *name, AstNode *type_expr, AstNode *value) {
    AstNode *node = ast_node_create(AST_DECL_CONSTANT, loc);
    node->data.const_decl.name = str_dup(name);
    node->data.const_decl.type_expr = type_expr;
    node->data.const_decl.value = value;
    return node;
}

// Return statement
AstNode *ast_return_stmt(Location loc, AstNode *expr) {
    AstNode *node = ast_node_create(AST_STMT_RETURN, loc);
    node->data.return_stmt.expr = expr;
    return node;
}

// Block statement
AstNode *ast_block_stmt(Location loc, AstNode **stmts, size_t stmt_count) {
    AstNode *node = ast_node_create(AST_STMT_BLOCK, loc);
    node->data.block_stmt.stmts = stmts;  // Already allocated
    node->data.block_stmt.stmt_count = stmt_count;
    return node;
}

// Integer literal
AstNode *ast_int_literal(Location loc, long long value) {
    AstNode *node = ast_node_create(AST_EXPR_LITERAL_INT, loc);
    node->data.int_lit.value = value;
    return node;
}

// Identifier
AstNode *ast_identifier(Location loc, const char *name) {
    AstNode *node = ast_node_create(AST_EXPR_IDENTIFIER, loc);
    node->data.ident.name = str_dup(name);
    return node;
}

// Binary operation
AstNode *ast_binary_op(Location loc, BinaryOp op, AstNode *left, AstNode *right) {
    AstNode *node = ast_node_create(AST_EXPR_BINARY_OP, loc);
    node->data.binop.op = op;
    node->data.binop.left = left;
    node->data.binop.right = right;
    return node;
}

// Function call
AstNode *ast_func_call(Location loc, AstNode *func, AstNode **args, size_t arg_count) {
    AstNode *node = ast_node_create(AST_EXPR_CALL, loc);
    node->data.call.func = func;
    node->data.call.args = args;  // Already allocated
    node->data.call.arg_count = arg_count;
    return node;
}
