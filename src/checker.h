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

// Check if checker has encountered errors
bool checker_has_errors(void);

// Error reporting
void checker_error(Location loc, const char *fmt, ...);

#endif
