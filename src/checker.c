#include "checker.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

// Checker state (private to this file)
typedef struct {
    bool has_errors;
    int error_count;
} CheckerState;

static CheckerState checker_state;

void checker_init(void) {
    checker_state.has_errors = false;
    checker_state.error_count = 0;
    symbol_table_init();  // Initialize global scope
}

bool checker_has_errors(void) {
    return checker_state.has_errors;
}

void checker_error(Location loc, const char *fmt, ...) {
    fprintf(stderr, "%s:%d:%d: error: ", loc.file, loc.line, loc.column);

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    fprintf(stderr, "\n");

    checker_state.has_errors = true;
    checker_state.error_count++;
}

// Collect a single declaration into the global scope
static void collect_declaration(AstNode *decl) {
    char *name = NULL;
    SymbolKind kind;

    // Extract name and kind based on declaration type
    switch (decl->kind) {
        case AST_DECL_FUNCTION:
            name = decl->data.func_decl.name;
            kind = SYMBOL_FUNCTION;
            break;
        case AST_DECL_VARIABLE:
            name = decl->data.var_decl.name;
            kind = SYMBOL_VARIABLE;
            break;
        case AST_DECL_CONSTANT:
            name = decl->data.const_decl.name;
            kind = SYMBOL_CONSTANT;
            break;
        case AST_DECL_TYPE:
            name = decl->data.type_decl.name;
            kind = SYMBOL_TYPE;
            break;
        default:
            return; // Not a declaration
    }

    // Check for duplicates
    Symbol *existing = scope_lookup_local(global_scope, name);
    if (existing) {
        checker_error(decl->loc, "duplicate declaration of '%s'", name);
        checker_error(existing->decl->loc, "previous declaration was here");
        return;
    }

    // Create and add symbol
    Symbol *symbol = symbol_create(name, kind, decl);
    if (kind == SYMBOL_VARIABLE) {
        symbol->data.var.is_global = true;
    }
    scope_add_symbol(global_scope, symbol);
}

bool collect_globals(AstNode **decls, size_t decl_count) {
    for (size_t i = 0; i < decl_count; i++) {
        collect_declaration(decls[i]);
    }
    return !checker_state.has_errors;
}
