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
    symbol_table_init();  // Create fresh global scope
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
static void check_global_constants(void) {
    Symbol *sym, *tmp;

    // Iterate over all symbols in global scope
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        // Only process constant declarations
        if (sym->kind != SYMBOL_CONSTANT) {
            continue;
        }

        AstNode *decl = sym->decl;
        AstNode *type_expr = decl->data.const_decl.type_expr;
        AstNode *value = decl->data.const_decl.value;

        // Rule: Constants must have an initializer
        if (!value) {
            checker_error(decl->loc, "global constant '%s' must be initialized", sym->name);
            continue;
        }

        // Rule: For now, only allow literal initializers
        if (value->kind != AST_EXPR_LITERAL_INT &&
            value->kind != AST_EXPR_LITERAL_FLOAT &&
            value->kind != AST_EXPR_LITERAL_STRING &&
            value->kind != AST_EXPR_LITERAL_BOOL) {
            checker_error(value->loc, "global constant initializer must be a literal (complex expressions not yet supported)");
            continue;
        }

        // Check the initializer expression
        Type *inferred_type = check_expression(value);
        if (!inferred_type) {
            continue;  // Error already reported
        }

        // If explicit type is given, resolve and verify it matches
        if (type_expr) {
            Type *explicit_type = resolve_type_expression(type_expr);
            if (!explicit_type) {
                continue;  // Error already reported
            }

            if (!type_equals(explicit_type, inferred_type)) {
                checker_error(value->loc, "constant initializer type mismatch");
                continue;
            }

            sym->type = explicit_type;
        } else {
            // No explicit type, use inferred type
            sym->type = inferred_type;
        }
    }
}

// Sub-pass 3c: Check global variable declarations
static void check_global_variables(void) {
    Symbol *sym, *tmp;

    // Iterate over all symbols in global scope
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        // Only process variable declarations
        if (sym->kind != SYMBOL_VARIABLE) {
            continue;
        }

        AstNode *decl = sym->decl;
        AstNode *type_expr = decl->data.var_decl.type_expr;
        AstNode *init = decl->data.var_decl.init;

        // Rule: Must have type or initializer (or both)
        if (!type_expr && !init) {
            checker_error(decl->loc, "variable '%s' must have either a type annotation or an initializer", sym->name);
            continue;
        }

        Type *explicit_type = NULL;
        Type *inferred_type = NULL;

        // Resolve explicit type if provided
        if (type_expr) {
            explicit_type = resolve_type_expression(type_expr);
            if (!explicit_type) {
                continue;  // Error already reported
            }
        }

        // Check initializer if provided
        if (init) {
            // Rule: For now, only allow literal initializers
            if (init->kind != AST_EXPR_LITERAL_INT &&
                init->kind != AST_EXPR_LITERAL_FLOAT &&
                init->kind != AST_EXPR_LITERAL_STRING &&
                init->kind != AST_EXPR_LITERAL_BOOL) {
                checker_error(init->loc, "global variable initializer must be a literal (complex expressions not yet supported)");
                continue;
            }

            inferred_type = check_expression(init);
            if (!inferred_type) {
                continue;  // Error already reported
            }
        }

        // Verify types match if both are present
        if (explicit_type && inferred_type) {
            if (!type_equals(explicit_type, inferred_type)) {
                checker_error(init->loc, "variable initializer type mismatch");
                continue;
            }
            sym->type = explicit_type;
        } else if (explicit_type) {
            // Only explicit type, no initializer
            sym->type = explicit_type;
        } else {
            // Only initializer, infer type
            sym->type = inferred_type;
        }
    }
}

// Sub-pass 3d: Check function signatures
static void check_function_signatures(void) {
    Symbol *sym, *tmp;

    // Iterate over all symbols in global scope
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        // Only process function declarations
        if (sym->kind != SYMBOL_FUNCTION) {
            continue;
        }

        AstNode *decl = sym->decl;
        FuncParam *params = decl->data.func_decl.params;
        size_t param_count = decl->data.func_decl.param_count;
        AstNode *return_type_expr = decl->data.func_decl.return_type;

        // Resolve parameter types
        Type **param_types = NULL;
        if (param_count > 0) {
            param_types = arena_alloc(&long_lived, sizeof(Type*) * param_count);
            for (size_t i = 0; i < param_count; i++) {
                param_types[i] = resolve_type_expression(params[i].type);
                if (!param_types[i]) {
                    // Error already reported, but keep going to check other params
                    param_types[i] = type_int;  // Use placeholder to continue
                }
            }
        }

        // Resolve return type
        Type *return_type = resolve_type_expression(return_type_expr);
        if (!return_type) {
            continue;  // Error already reported
        }

        // Create function type
        sym->type = type_create_function(param_types, param_count, return_type);

        // Create function's local scope with global as parent
        Scope *func_scope = scope_create(global_scope);
        sym->data.func.local_scope = func_scope;

        // Add parameters as symbols in the function scope
        for (size_t i = 0; i < param_count; i++) {
            if (!param_types[i]) {
                continue;  // Skip if type resolution failed
            }

            // Create parameter symbol
            Symbol *param_sym = symbol_create(params[i].name, SYMBOL_VARIABLE, decl);
            param_sym->type = param_types[i];
            param_sym->data.var.is_global = false;  // Parameters are local

            // Check for duplicate parameter names
            Symbol *existing = scope_lookup_local(func_scope, params[i].name);
            if (existing) {
                checker_error(decl->loc, "duplicate parameter name '%s'", params[i].name);
                continue;
            }

            scope_add_symbol(func_scope, param_sym);
        }
    }
}


// Main entry point for Pass 3
bool check_globals(void) {
    type_system_init();
    check_type_declarations();
    check_global_constants();
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
          return type_create_slice(element);
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

        case AST_EXPR_CALL: {
            AstNode *func_expr = expr->data.call.func;
            AstNode **args = expr->data.call.args;
            size_t arg_count = expr->data.call.arg_count;

            // Function expression must be an identifier (for now)
            if (func_expr->kind != AST_EXPR_IDENTIFIER) {
                checker_error(func_expr->loc, "function calls must use identifiers");
                return NULL;
            }

            // Look up the function symbol
            const char *func_name = func_expr->data.ident.name;
            Symbol *func_sym = scope_lookup(current_scope, func_name);
            if (!func_sym) {
                checker_error(func_expr->loc, "undefined function '%s'", func_name);
                return NULL;
            }

            // Verify it's actually a function
            if (func_sym->kind != SYMBOL_FUNCTION) {
                checker_error(func_expr->loc, "'%s' is not a function", func_name);
                return NULL;
            }

            Type *func_type = func_sym->type;
            size_t param_count = func_type->data.func.param_count;
            Type **param_types = func_type->data.func.param_types;
            Type *return_type = func_type->data.func.return_type;

            // Check argument count
            if (arg_count != param_count) {
                checker_error(expr->loc, "function '%s' expects %zu arguments, got %zu",
                             func_name, param_count, arg_count);
                return NULL;
            }

            // Check each argument type
            for (size_t i = 0; i < arg_count; i++) {
                Type *arg_type = check_expression(args[i]);
                if (!arg_type) {
                    continue;  // Error already reported
                }

                if (!type_equals(arg_type, param_types[i])) {
                    checker_error(args[i]->loc, "argument %zu type mismatch in call to '%s'",
                                 i + 1, func_name);
                }
            }

            return return_type;
        }

        case AST_EXPR_INDEX: {
            AstNode *array_expr = expr->data.index_expr.array;
            AstNode *index_expr = expr->data.index_expr.index;

            // Check the array expression
            Type *array_type = check_expression(array_expr);
            if (!array_type) {
                return NULL;
            }

            // Verify it's actually an array or slice
            if (array_type->kind != TYPE_ARRAY && array_type->kind != TYPE_SLICE) {
                checker_error(array_expr->loc, "cannot index into non-array type");
                return NULL;
            }

            // Check the index expression
            Type *index_type = check_expression(index_expr);
            if (!index_type) {
                return NULL;
            }

            // Verify index is an integer
            if (index_type->kind != TYPE_INT) {
                checker_error(index_expr->loc, "array index must be an integer");
                return NULL;
            }

            // Return the element type
            return array_type->data.array.element;
        }

        case AST_EXPR_MEMBER: {
            AstNode *object_expr = expr->data.member_expr.object;
            const char *field_name = expr->data.member_expr.member;

            // Check the object expression
            Type *object_type = check_expression(object_expr);
            if (!object_type) {
                return NULL;
            }

            // Verify it's a struct
            if (object_type->kind != TYPE_STRUCT) {
                checker_error(object_expr->loc, "member access requires struct type");
                return NULL;
            }

            // Look up the field in the struct
            size_t field_count = object_type->data.struct_data.field_count;
            char **field_names = object_type->data.struct_data.field_names;
            Type **field_types = object_type->data.struct_data.field_types;

            for (size_t i = 0; i < field_count; i++) {
                if (strcmp(field_names[i], field_name) == 0) {
                    // Found the field!
                    return field_types[i];
                }
            }

            // Field not found
            checker_error(expr->loc, "struct has no field named '%s'", field_name);
            return NULL;
        }

        default:
            checker_error(expr->loc, "unsupported expression type in expression type checking");
            return NULL;
    }
}

//=============================================================================
// PASS 4: CHECK FUNCTION BODIES
//=============================================================================

// Statement checking
// Returns false if statement does not return and true if it does
static bool check_statement(AstNode *stmt, Type *expected_return_type) {
    if (!stmt) {
        return false;
    }

    switch (stmt->kind) {
        case AST_STMT_RETURN: {
            AstNode *expr = stmt->data.return_stmt.expr;

            // Check return expression
            Type *expr_type = check_expression(expr);
            if (!expr_type) {
                return true;  // Error already reported
            }

            // Verify it matches expected return type
            if (!type_equals(expr_type, expected_return_type)) {
                checker_error(stmt->loc, "return type mismatch");
            }

            return true;
        }

        case AST_STMT_IF: {
            AstNode *cond = stmt->data.if_stmt.cond;
            AstNode *then_branch = stmt->data.if_stmt.then_branch;
            AstNode *else_branch = stmt->data.if_stmt.else_branch;

            // Check condition is boolean
            Type *cond_type = check_expression(cond);
            if (cond_type && cond_type->kind != TYPE_BOOL) {
                checker_error(cond->loc, "if condition must be boolean");
            }

            // Check both branches
            bool then_returns = check_statement(then_branch, expected_return_type);
            bool else_returns = else_branch ? check_statement(else_branch, expected_return_type) : false;

            // Only returns if BOTH branches return
            return then_returns && else_returns;
        }

        case AST_STMT_WHILE: {
            AstNode *cond = stmt->data.while_stmt.cond;
            AstNode *body = stmt->data.while_stmt.body;

            // Check condition is boolean
            Type *cond_type = check_expression(cond);
            if (cond_type && cond_type->kind != TYPE_BOOL) {
                checker_error(cond->loc, "while condition must be boolean");
            }

            // Check body
            check_statement(body, expected_return_type);

            // Can't prove while always executes
            return false;
        }

        case AST_STMT_BLOCK: {
            // Create child scope for this block
            Scope *block_scope = scope_create(current_scope);
            scope_push(block_scope);

            // Check all statements in the block
            AstNode **stmts = stmt->data.block_stmt.stmts;
            size_t count = stmt->data.block_stmt.stmt_count;
            bool returns = false;

            for (size_t i = 0; i < count; i++) {
                if (check_statement(stmts[i], expected_return_type)) {
                    returns = true;

                    // Warn about unreachable code (only once)
                    if (i < count - 1) {
                        checker_error(stmts[i + 1]->loc, "unreachable code after return");
                        break;  // Stop checking, don't spam errors
                    }
                }
            }

            // Pop back to parent scope
            scope_pop();
            return returns;
        }

        case AST_STMT_ASSIGN: {
            AstNode *lhs = stmt->data.assign_stmt.lhs;
            AstNode *rhs = stmt->data.assign_stmt.rhs;

            // LHS can be identifier, member access, or array index
            if (lhs->kind != AST_EXPR_IDENTIFIER &&
                lhs->kind != AST_EXPR_MEMBER &&
                lhs->kind != AST_EXPR_INDEX) {
                checker_error(lhs->loc, "invalid assignment target");
                return false;
            }

            // Check both sides
            Type *lhs_type = check_expression(lhs);
            Type *rhs_type = check_expression(rhs);

            if (lhs_type && rhs_type && !type_equals(lhs_type, rhs_type)) {
                checker_error(stmt->loc, "assignment type mismatch");
            }
            return false;
        }

        case AST_STMT_EXPR: {
            // Just check the expression, ignore result
            check_expression(stmt->data.expr_stmt.expr);
            return false;
        }

        case AST_DECL_VARIABLE: {
            // Local variable: var x int; or var y = 42;
            char *name = stmt->data.var_decl.name;
            AstNode *type_expr = stmt->data.var_decl.type_expr;
            AstNode *init = stmt->data.var_decl.init;

            // Must have type or initializer
            if (!type_expr && !init) {
                checker_error(stmt->loc, "variable '%s' must have type or initializer", name);
                return false;
            }

            Type *var_type = NULL;

            // Resolve type if provided
            if (type_expr) {
                var_type = resolve_type_expression(type_expr);
                if (!var_type) return false;
            }

            // Check initializer if provided
            if (init) {
                Type *init_type = check_expression(init);
                if (!init_type) return false;

                if (var_type && !type_equals(var_type, init_type)) {
                    checker_error(init->loc, "initializer type mismatch");
                    return false;
                }

                if (!var_type) {
                    var_type = init_type;  // Infer type
                }
            }

            // Check for duplicate in current scope
            Symbol *existing = scope_lookup_local(current_scope, name);
            if (existing) {
                checker_error(stmt->loc, "variable '%s' already declared in this scope", name);
                return false;
            }

            // Add to current scope
            Symbol *var_sym = symbol_create(name, SYMBOL_VARIABLE, stmt);
            var_sym->type = var_type;
            var_sym->data.var.is_global = false;
            scope_add_symbol(current_scope, var_sym);
            return false;
        }

        case AST_DECL_CONSTANT: {
            // Local constant: let x = 42;
            char *name = stmt->data.const_decl.name;
            AstNode *type_expr = stmt->data.const_decl.type_expr;
            AstNode *value = stmt->data.const_decl.value;

            // Constants must have initializer
            if (!value) {
                checker_error(stmt->loc, "constant '%s' must be initialized", name);
                return false;
            }

            Type *const_type = NULL;

            // Resolve explicit type if provided
            if (type_expr) {
                const_type = resolve_type_expression(type_expr);
                if (!const_type) return false;
            }

            // Check value
            Type *value_type = check_expression(value);
            if (!value_type) return false;

            if (const_type && !type_equals(const_type, value_type)) {
                checker_error(value->loc, "constant initializer type mismatch");
                return false;
            }

            if (!const_type) {
                const_type = value_type;  // Infer type
            }

            // Check for duplicate
            Symbol *existing = scope_lookup_local(current_scope, name);
            if (existing) {
                checker_error(stmt->loc, "constant '%s' already declared in this scope", name);
                return false;
            }

            // Add to current scope
            Symbol *const_sym = symbol_create(name, SYMBOL_CONSTANT, stmt);
            const_sym->type = const_type;
            scope_add_symbol(current_scope, const_sym);
            return false;
        }


        default:
            checker_error(stmt->loc, "unsupported statement type");
            return false;
    }
}

// Main entry point for Pass 4
bool check_function_bodies(void) {
    Symbol *sym, *tmp;

    // Iterate over all function symbols in global scope
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        // Only process functions
        if (sym->kind != SYMBOL_FUNCTION) {
            continue;
        }

        // Get function details
        AstNode *decl = sym->decl;
        AstNode *body = decl->data.func_decl.body;
        Type *func_type = sym->type;
        Type *return_type = func_type->data.func.return_type;

        // Push function's local scope (contains parameters)
        scope_push(sym->data.func.local_scope);

        // Check the function body
        bool definitely_returns = check_statement(body, return_type);

        // If non-void, ensure all paths return
        if (return_type->kind != TYPE_VOID && !definitely_returns) {
            checker_error(decl->loc, "function '%s' may not return a value on all paths", sym->name);
        }

        // Pop back to global scope
        scope_pop();
    }

    return !checker_has_errors();
}
