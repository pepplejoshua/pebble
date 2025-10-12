#include "checker.h"
#include "alloc.h"
#include "ast.h"
#include "symbol.h"
#include "type.h"
#include "uthash.h"
#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

// External allocator
extern Arena long_lived;

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

//=============================================================================
// PASS 2: COLLECT GLOBALS
//============================================================================

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

//=============================================================================
// PASS 3: CHECK GLOBALS
//=============================================================================

// Sub-pass 3a: Resolve type declarations
static void check_type_declarations(void) {
    Symbol *sym, *tmp;

    // Iterate over all symbols in global scope
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        // Only process type declarations
        if (sym->kind != SYMBOL_TYPE) {
            continue;
        }

        // Get the type expression from the declaration
        AstNode *type_expr = sym->decl->data.type_decl.type_expr;

        // Resolve it to a type object
        Type *resolved = resolve_type_expression(type_expr);
        if (!resolved) {
            continue; // Error already reported
        }

        // Store in symbol
        sym->type = resolved;

        // Register in global type table under given name
        type_register(sym->name, resolved);
    }
}

// Sub-pass 3b: Check constant declarations
static void check_constants(void) {
    // Walk global_scope->symbols
    // For each SYMBOL_CONSTANT, resolve type and check initializer
    // Store result in symbol->type
}

// Sub-pass 3c: Check global variable declarations
static void check_global_variables(void) {
    // Walk global_scope->symbols
    // For each SYMBOL_VARIABLE with is_global=true
    // Resolve type, optionally check initializer
}

// Sub-pass 3d: Check function signatures
static void check_function_signatures(void) {
    // Walk global_scope->symbols
    // For each SYMBOL_FUNCTION
    // Resolve param types, return type, create function scope
}

// Main entry point for Pass 3
bool check_globals(void) {
    check_type_declarations();
    check_constants();
    check_global_variables();
    check_function_signatures();
    return !checker_has_errors();
}

//=============================================================================
// TYPE RESOLUTION HELPERS
//=============================================================================

Type *resolve_type_expression(AstNode *type_expr) {
    if (!type_expr) {
        return NULL;
    }

    switch (type_expr->kind) {
      case AST_TYPE_NAMED: {

          // Look up named type in type table
          const char *name = type_expr->data.type_named.name;
          Type *type = type_lookup(name);
          if (!type) {
              checker_error(type_expr->loc, "undefined type '%s'", name);
              return NULL;
          }
          return type;
      }

      case AST_TYPE_POINTER: {
          // Resolve base type and create pointer type
          Type *base = resolve_type_expression(type_expr->data.type_pointer.base);
          if (!base) {
              return NULL;
          }
          return type_create_pointer(base);
      }

      case AST_TYPE_ARRAY: {
          // Resolve element type and create array type
          Type *element = resolve_type_expression(type_expr->data.type_array.element);
          if (!element) {
              return NULL;
          }
          size_t size = type_expr->data.type_array.size;
          return type_create_array(element, size);
      }

      case AST_TYPE_SLICE: {
          // Resolve element type and create slice (array with size 0)
          // We might need to change this later
          Type *element = resolve_type_expression(type_expr->data.type_slice.element);
          if (!element) {
              return NULL;
          }
          return type_create_array(element, 0);
      }

      case AST_TYPE_STRUCT: {
          // Resolve all field types and create struct type
          size_t field_count = type_expr->data.type_struct.field_count;
          char **field_names = type_expr->data.type_struct.field_names;
          AstNode **field_type_exprs = type_expr->data.type_struct.field_types;

          Type **field_types = arena_alloc(&long_lived, sizeof(Type*) * field_count);

          for (size_t i = 0; i < field_count; i++) {
              field_types[i] = resolve_type_expression(field_type_exprs[i]);
              if (!field_types[i]) {
                  return NULL;
              }
          }

          return type_create_struct(field_names, field_types, field_count);
      }

      case AST_TYPE_FUNCTION: {
          // Resolve parameter types and return type
          size_t param_count = type_expr->data.type_function.param_count;
          AstNode **param_type_exprs = type_expr->data.type_function.param_types;
          AstNode *return_type_expr = type_expr->data.type_function.return_type;

          Type **param_types = arena_alloc(&long_lived, sizeof(Type*) * param_count);

          for (size_t i = 0; i < param_count; i++) {
              param_types[i] = resolve_type_expression(param_type_exprs[i]);
              if (!param_types[i]) {
                  return NULL;
              }
          }

          Type *return_type = resolve_type_expression(return_type_expr);
          if (!return_type) {
              return NULL;
          }

          return type_create_function(param_types, param_count, return_type);
      }

      default:
          checker_error(type_expr->loc, "invalid type expression");
          return NULL;
    }
}

//=============================================================================
// EXPRESSION CHECKING HELPERS (for Pass 3b, 3c and later Pass 4)
//=============================================================================

// Helper: Check if a type supports equality comparison
static bool type_is_comparable(Type *type) {
    switch (type->kind) {
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_BOOL:
        case TYPE_STRING:
            return true;

        // Pointers, arrays, structs, functions not supported yet
        case TYPE_POINTER:
        case TYPE_ARRAY:
        case TYPE_SLICE:
        case TYPE_STRUCT:
        case TYPE_FUNCTION:
        case TYPE_VOID:
            return false;

        default:
            return false;
    }
}

Type *check_expression(AstNode *expr) {
    if (!expr) {
        return NULL;
    }

    switch (expr->kind) {
        case AST_EXPR_LITERAL_INT:
            return type_int;

        case AST_EXPR_LITERAL_FLOAT:
            return type_float;

        case AST_EXPR_LITERAL_STRING:
            return type_string;

        case AST_EXPR_LITERAL_BOOL:
            return type_bool;

        case AST_EXPR_IDENTIFIER: {
            const char *name = expr->data.ident.name;
            Symbol *sym = scope_lookup(current_scope, name);
            if (!sym) {
                checker_error(expr->loc, "undefined name '%s'", name);
                return NULL;
            }

            // Types are not values
            if (sym->kind == SYMBOL_TYPE) {
                checker_error(expr->loc, "'%s' is a type, not a value", name);
                return NULL;
            }

            // Only variables, constants and functions can be used as values
            if (sym->kind != SYMBOL_VARIABLE && sym->kind != SYMBOL_CONSTANT &&
                sym->kind != SYMBOL_FUNCTION) {
                    checker_error(expr->loc, "'%s' cannot be used as a value", name);
                    return NULL;
            }

            if (!sym->type) {
                checker_error(expr->loc, "name '%s' used before type is resolved", name);
                return NULL;
            }
            return sym->type;
        }

        case AST_EXPR_BINARY_OP: {
            // Check operands
            Type *left = check_expression(expr->data.binop.left);
            Type *right = check_expression(expr->data.binop.right);

            if (!left || !right) {
                return NULL;  // Error already reported
            }

            BinaryOp op = expr->data.binop.op;

            // Arithmetic: +, -, *, /
            if (op == BINOP_ADD || op == BINOP_SUB || op == BINOP_MUL || op == BINOP_DIV) {
                if (!type_is_numeric(left) || !type_is_numeric(right)) {
                    checker_error(expr->loc, "arithmetic operation requires numeric operands");
                    return NULL;
                }
                if (!type_equals(left, right)) {
                    checker_error(expr->loc, "type mismatch in binary operation");
                    return NULL;
                }
                return left;  // Result type is same as operands
            }

            // Comparison: <, >, <=, >=
            if (op == BINOP_LT || op == BINOP_GT || op == BINOP_LE || op == BINOP_GE) {
                if (!type_is_numeric(left) || !type_is_numeric(right)) {
                    checker_error(expr->loc, "comparison requires numeric operands");
                    return NULL;
                }
                if (!type_equals(left, right)) {
                    checker_error(expr->loc, "type mismatch in comparison");
                    return NULL;
                }
                return type_bool;  // Comparisons return bool
            }

            // Equality: ==, !=
            if (op == BINOP_EQ || op == BINOP_NE) {
                if (!type_is_comparable(left) || !type_is_comparable(right)) {
                    checker_error(expr->loc, "equality comparison not supported for this type");
                    return NULL;
                }
                if (!type_equals(left, right)) {
                    checker_error(expr->loc, "type mismatch in equality check");
                    return NULL;
                }
                return type_bool;
            }


            // Logical: &&, ||
            if (op == BINOP_AND || op == BINOP_OR) {
                if (left->kind != TYPE_BOOL || right->kind != TYPE_BOOL) {
                    checker_error(expr->loc, "logical operation requires boolean operands");
                    return NULL;
                }
                return type_bool;
            }

            checker_error(expr->loc, "unknown binary operator");
            return NULL;
        }

        case AST_EXPR_UNARY_OP: {
            Type *operand = check_expression(expr->data.unop.operand);
            if (!operand) {
                return NULL;
            }

            UnaryOp op = expr->data.unop.op;

            if (op == UNOP_NEG) {
                if (!type_is_numeric(operand)) {
                    checker_error(expr->loc, "negation requires numeric operand");
                    return NULL;
                }
                return operand;
            }

            if (op == UNOP_NOT) {
                if (operand->kind != TYPE_BOOL) {
                    checker_error(expr->loc, "logical not requires boolean operand");
                    return NULL;
                }
                return type_bool;
            }

            checker_error(expr->loc, "unknown unary operator");
            return NULL;
        }

        // TODO: Add more expression types in Pass 4

        default:
            checker_error(expr->loc, "unsupported expression type in expression type checking");
            return NULL;
    }
}
