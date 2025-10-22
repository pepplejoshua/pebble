#ifndef CHECKER_H
#define CHECKER_H

#include "ast.h"
#include "symbol.h"
#include <stdbool.h>

// Initialize the checker and symbol table system
void checker_init(void);

// Pass 2: Collect all global declarations into the symbol table
// Returns true if successful, false if there were errors
bool collect_globals(AstNode **decls, size_t decl_count);

// Pass 3: Type-check all global declarations (4 sub-passes)
// Returns true if successful, false if there were errors
bool check_globals(void);

// Pass 4: Type-check function bodies
// Returns true if successful, false if there were errors
bool check_function_bodies(void);

// Helper: Resolve a type expression AST node to a Type object
Type *resolve_type_expression(AstNode *type_expr);

// Helper: Check an expression and return its type
Type *check_expression(AstNode *expr);

// Check if checker has encountered errors
bool checker_has_errors(void);

// Error reporting
void checker_error(Location loc, const char *fmt, ...);

void checker_warning(Location loc, const char *fmt, ...);

#endif
