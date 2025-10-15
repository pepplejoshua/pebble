#include "checker.h"
#include "alloc.h"
#include "ast.h"
#include "symbol.h"
#include "type.h"
#include "uthash.h"
#include <assert.h>
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
    bool in_type_resolution;
} CheckerState;

static CheckerState checker_state;

void checker_init(void) {
    checker_state.has_errors = false;
    checker_state.error_count = 0;
    checker_state.in_type_resolution = false;
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

typedef struct Visited {
    Type *key;
    UT_hash_handle hh;
} Visited;

static bool canonicalize_type_internal(Type **type_ref, Visited **visited) {
    Type *type = *type_ref;
    if (!type) return false;

    // If already canonicalized, check for deduplication
    if (type->canonical_name != NULL) {
        Type *existing = canonical_lookup(type->canonical_name);
        if (existing && existing != type) {
            *type_ref = existing;
        }
        return false;
    }

    // Check for cycle (back-edge)
    Visited *found;
    HASH_FIND_PTR(*visited, &type, found);
    if (found) {
        return true;  // Cycle detected
    }

    // Add to visited (currently processing)
    Visited *new_v = arena_alloc(&long_lived, sizeof(Visited));
    new_v->key = type;
    HASH_ADD_PTR(*visited, key, new_v);

    // Recursively canonicalize component types
    bool cycle_detected = false;

    switch (type->kind) {
        case TYPE_POINTER:
            if (canonicalize_type_internal(&type->data.ptr.base, visited)) {
                cycle_detected = true;
            }
            break;

        case TYPE_SLICE:
            if (canonicalize_type_internal(&type->data.slice.element, visited)) {
                cycle_detected = true;
            }
            break;

        case TYPE_ARRAY:
            if (canonicalize_type_internal(&type->data.array.element, visited)) {
                cycle_detected = true;
            }
            break;

        case TYPE_TUPLE:
            for (size_t i = 0; i < type->data.tuple.element_count; i++) {
                if (canonicalize_type_internal(&type->data.tuple.element_types[i], visited)) {
                    cycle_detected = true;
                }
            }
            break;

        case TYPE_STRUCT:
            for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
                if (canonicalize_type_internal(&type->data.struct_data.field_types[i], visited)) {
                    cycle_detected = true;
                }
            }
            break;

        case TYPE_FUNCTION:
            for (size_t i = 0; i < type->data.func.param_count; i++) {
                if (canonicalize_type_internal(&type->data.func.param_types[i], visited)) {
                    cycle_detected = true;
                }
            }
            if (canonicalize_type_internal(&type->data.func.return_type, visited)) {
                cycle_detected = true;
            }
            break;

        default:
            // Primitives don't need recursion
            break;
    }

    // Now build canonical name based on type kind
    char *canonical_name = NULL;

    // Helper to get a component's name (handles cycles)
    #define GET_COMPONENT_NAME(component) \
        ({ \
            Type *_c = (component); \
            char *_name; \
            if (_c->canonical_name) { \
                /* Already canonicalized */ \
                _name = _c->canonical_name; \
            } else { \
                /* Not canonicalized yet - check if in cycle */ \
                Visited *_found; \
                HASH_FIND_PTR(*visited, &_c, _found); \
                if (_found) { \
                    /* In cycle - use declared name if available */ \
                    if (_c->declared_name) { \
                        _name = _c->declared_name; \
                    } else { \
                        _name = "UNRESOLVED"; \
                    } \
                } else { \
                    /* Not in visited and no canonical name - shouldn't happen */ \
                    _name = "UNRESOLVED"; \
                } \
            } \
            _name; \
        })

    switch (type->kind) {
        case TYPE_INT:
            canonical_name = str_dup("int");
            break;

        case TYPE_FLOAT:
            canonical_name = str_dup("float");
            break;

        case TYPE_BOOL:
            canonical_name = str_dup("bool");
            break;

        case TYPE_STRING:
            canonical_name = str_dup("str");
            break;

        case TYPE_VOID:
            canonical_name = str_dup("void");
            break;

        case TYPE_POINTER: {
            char *base_name = GET_COMPONENT_NAME(type->data.ptr.base);
            size_t len = strlen("ptr_") + strlen(base_name) + 1;
            canonical_name = arena_alloc(&long_lived, len);
            snprintf(canonical_name, len, "ptr_%s", base_name);
            break;
        }

        case TYPE_SLICE: {
            char *elem_name = GET_COMPONENT_NAME(type->data.slice.element);
            size_t len = strlen("slice_") + strlen(elem_name) + 1;
            canonical_name = arena_alloc(&long_lived, len);
            snprintf(canonical_name, len, "slice_%s", elem_name);
            break;
        }

        case TYPE_ARRAY: {
            char *elem_name = GET_COMPONENT_NAME(type->data.array.element);
            size_t len = strlen("array_") + 20 + strlen(elem_name) + 1;
            canonical_name = arena_alloc(&long_lived, len);
            snprintf(canonical_name, len, "array_%zu_%s", type->data.array.size, elem_name);
            break;
        }

        case TYPE_TUPLE: {
            // If cyclic, must use nominal name
            if (cycle_detected) {
                canonical_name = str_dup(type->declared_name);
            } else {
                // Non-cyclic tuple - structural name
                size_t capacity = 256;
                canonical_name = arena_alloc(&long_lived, capacity);
                size_t offset = 0;

                offset += snprintf(canonical_name + offset, capacity - offset, "tuple");

                for (size_t i = 0; i < type->data.tuple.element_count; i++) {
                    char *elem_name = GET_COMPONENT_NAME(type->data.tuple.element_types[i]);

                    size_t needed = offset + 1 + strlen(elem_name) + 1;
                    if (needed > capacity) {
                        capacity = needed * 2;
                        char *new_buf = arena_alloc(&long_lived, capacity);
                        memcpy(new_buf, canonical_name, offset);
                        canonical_name = new_buf;
                    }

                    offset += snprintf(canonical_name + offset, capacity - offset, "_%s", elem_name);
                }
            }
            break;
        }

        case TYPE_STRUCT: {
            // Structs are always nominal (use declared name)
            if (type->declared_name) {
                canonical_name = str_dup(type->declared_name);
            } else {
                // Anonymous struct - build structural name
                size_t capacity = 256;
                canonical_name = arena_alloc(&long_lived, capacity);
                size_t offset = 0;

                offset += snprintf(canonical_name + offset, capacity - offset, "struct");

                for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
                    char *field_name = type->data.struct_data.field_names[i];
                    char *type_name = GET_COMPONENT_NAME(type->data.struct_data.field_types[i]);

                    size_t needed = offset + 1 + strlen(field_name) + 1 + strlen(type_name) + 1;
                    if (needed > capacity) {
                        capacity = needed * 2;
                        char *new_buf = arena_alloc(&long_lived, capacity);
                        memcpy(new_buf, canonical_name, offset);
                        canonical_name = new_buf;
                    }

                    offset += snprintf(canonical_name + offset, capacity - offset,
                                      "_%s_%s", field_name, type_name);
                }
            }
            break;
        }

        case TYPE_FUNCTION: {
            size_t total_len = strlen("func");
            for (size_t i = 0; i < type->data.func.param_count; i++) {
                char *param_name = GET_COMPONENT_NAME(type->data.func.param_types[i]);
                total_len += 1 + strlen(param_name);
            }
            char *ret_name = GET_COMPONENT_NAME(type->data.func.return_type);
            total_len += strlen("_ret_") + strlen(ret_name);

            canonical_name = arena_alloc(&long_lived, total_len + 1);
            strcpy(canonical_name, "func");
            for (size_t i = 0; i < type->data.func.param_count; i++) {
                strcat(canonical_name, "_");
                char *param_name = GET_COMPONENT_NAME(type->data.func.param_types[i]);
                strcat(canonical_name, param_name);
            }
            strcat(canonical_name, "_ret_");
            strcat(canonical_name, ret_name);
            break;
        }

        case TYPE_UNRESOLVED:
            canonical_name = str_dup("UNRESOLVED");
            break;
    }

    #undef GET_COMPONENT_NAME

    // Check for existing type with same canonical name (deduplication)
    Type *existing = canonical_lookup(canonical_name);
    if (existing) {
        // Found duplicate - replace with existing
        *type_ref = existing;
    } else {
        // First occurrence - set canonical name and register
        type->canonical_name = canonical_name;
        canonical_register(canonical_name, type);
    }

    // Remove from visited
    HASH_DEL(*visited, new_v);
    return cycle_detected;
}

// Canonicalize a type (compute name, deduplicate)
static void canonicalize_type(Type **type_ref) {
    if (!type_ref || !*type_ref) return;

    Visited *visited = NULL;
    canonicalize_type_internal(type_ref, &visited);

    // Clean up visited
    Visited *curr, *tmp;
    HASH_ITER(hh, visited, curr, tmp) {
        HASH_DEL(visited, curr);
    }
}

// Sub-pass 3a-prime: Compute canonical names and deduplicate types
static void canonicalize_types(void) {
    // Walk all global symbols and canonicalize their types
    Symbol *sym, *tmp;
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        if (sym->type) {
            // Type hasn't been canonicalized yet
            canonicalize_type(&(sym->type));

            // Keep type_table in sync (important for deduplication)
            if (sym->kind == SYMBOL_TYPE) {
                type_register(sym->name, sym->type);
            }
        }
    }
}

// Sub-pass 3a: Resolve type declarations
static void check_type_declarations(void) {
    Symbol *sym, *tmp;

    // Build worklist of type declarations
    Symbol **worklist = NULL;
    size_t worklist_size = 0;
    size_t worklist_capacity = 0;

    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        if (sym->kind == SYMBOL_TYPE) {
            sym->type = NULL;
            if (worklist_size >= worklist_capacity) {
                worklist_capacity = worklist_capacity == 0 ? 8 : worklist_capacity * 2;
                worklist = realloc(worklist, sizeof(Symbol*) * worklist_capacity);
            }
            worklist[worklist_size++] = sym;
        }
    }

    checker_state.in_type_resolution = true;

    // PHASE 1: Pre-register all type names as placeholders
    for (size_t i = 0; i < worklist_size; i++) {
        sym = worklist[i];
        Type *placeholder = type_create(TYPE_UNRESOLVED);
        type_register(sym->name, placeholder);
    }

    // PHASE 2: Resolve types (handles out-of-order dependencies)
    bool made_progress = true;
    while (worklist_size > 0 && made_progress) {
        made_progress = false;
        size_t new_size = 0;

        for (size_t i = 0; i < worklist_size; i++) {
            sym = worklist[i];
            AstNode *type_expr = sym->decl->data.type_decl.type_expr;
            Type *resolved = resolve_type_expression(type_expr);

            if (resolved) {
                Type *placeholder = type_lookup(sym->name);

                // Always mutate placeholder in place
                if (placeholder && placeholder->kind == TYPE_UNRESOLVED) {
                    *placeholder = *resolved;
                }

                if (placeholder->kind == TYPE_STRUCT || placeholder->kind == TYPE_TUPLE) {
                    placeholder->declared_name = str_dup(sym->name);
                }

                sym->type = placeholder;

                // Check if fully resolved (not TYPE_UNRESOLVED anymore)
                if (placeholder->kind != TYPE_UNRESOLVED) {
                    made_progress = true;
                    // Remove from worklist
                } else {
                    // Still TYPE_UNRESOLVED, keep trying
                    worklist[new_size++] = sym;
                }
            } else {
                // Resolution failed, keep trying
                worklist[new_size++] = sym;
            }
        }

        worklist_size = new_size;
    }

    // PHASE 3: Report circular dependencies
    for (size_t i = 0; i < worklist_size; i++) {
        sym = worklist[i];
        checker_error(sym->decl->data.type_decl.type_expr->loc,
            "cannot resolve type '%s' (circular dependency)",
            sym->name);
    }

    checker_state.in_type_resolution = false;
    free(worklist);

    // Sub-pass 3a' - canonicalize and deduplicate
    canonicalize_types();
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
            sym->decl->resolved_type = explicit_type;
        } else {
            // No explicit type, use inferred type
            sym->type = inferred_type;
            sym->decl->resolved_type = inferred_type;
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
            sym->decl->resolved_type = explicit_type;
        } else if (explicit_type) {
            // Only explicit type, no initializer
            sym->type = explicit_type;
            sym->decl->resolved_type = explicit_type;
        } else {
            // Only initializer, infer type
            sym->type = inferred_type;
            sym->decl->resolved_type = inferred_type;
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
        sym->type = type_create_function(param_types, param_count, return_type, !checker_state.in_type_resolution);

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
                // During type resolution, check if it's an unresolved type decl
                if (checker_state.in_type_resolution) {  // ADD THIS CHECK
                    Symbol *sym = scope_lookup_local(global_scope, name);
                    if (sym && sym->kind == SYMBOL_TYPE && sym->type == NULL) {
                        // It's a forward reference - return NULL to retry later
                        return NULL;
                    }
                }

                // Otherwise, it's truly undefined
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
          return type_create_pointer(base, !checker_state.in_type_resolution);
      }

      case AST_TYPE_ARRAY: {
          // Resolve element type and create array type
          Type *element = resolve_type_expression(type_expr->data.type_array.element);
          if (!element) {
              return NULL;
          }
          size_t size = type_expr->data.type_array.size;
          return type_create_array(element, size, !checker_state.in_type_resolution);
      }

      case AST_TYPE_SLICE: {
          // Resolve element type and create slice (array with size 0)
          // We might need to change this later
          Type *element = resolve_type_expression(type_expr->data.type_slice.element);
          if (!element) {
              return NULL;
          }
          return type_create_slice(element, !checker_state.in_type_resolution);
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

          return type_create_function(param_types, param_count, return_type, !checker_state.in_type_resolution);
      }

      case AST_TYPE_TUPLE: {
            // Resolve all element types
            size_t element_count = type_expr->data.type_tuple.element_count;
            AstNode **element_type_exprs = type_expr->data.type_tuple.element_types;

            Type **element_types = arena_alloc(&long_lived, sizeof(Type*) * element_count);

            for (size_t i = 0; i < element_count; i++) {
                element_types[i] = resolve_type_expression(element_type_exprs[i]);
                if (!element_types[i]) {
                    return NULL;
                }
            }

            return type_create_tuple(element_types, element_count, !checker_state.in_type_resolution);
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

static bool is_lvalue(AstNode *expr) {
    switch (expr->kind) {
        case AST_EXPR_IDENTIFIER:
        case AST_EXPR_INDEX:
        case AST_EXPR_MEMBER:
            return true;
        case AST_EXPR_UNARY_OP:
            return expr->data.unop.op == UNOP_DEREF;
        default:
            return false;
    }
}

// Insert implicit cast if needed, returns the (possibly wrapped) expression
// Returns NULL if types are incompatible
static AstNode *maybe_insert_cast(AstNode *expr, Type *expr_type, Type *target_type) {
    if (type_equals(expr_type, target_type)) {
        return expr;  // No cast needed
    }

    // Check if implicit conversion is allowed
    if (expr_type->kind == TYPE_ARRAY && target_type->kind == TYPE_SLICE) {
        // Array [N]T can convert to slice []T
        if (type_equals(expr_type->data.array.element, target_type->data.slice.element)) {
            // Create implicit cast node
            AstNode *cast = arena_alloc(&long_lived, sizeof(AstNode));
            cast->kind = AST_EXPR_IMPLICIT_CAST;
            cast->loc = expr->loc;
            cast->data.implicit_cast.expr = expr;
            cast->data.implicit_cast.target_type = target_type;
            return cast;
        }
    }

    // No valid conversion
    return NULL;
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
            // expr->resolved_type = sym->type;
            expr->resolved_type = sym->type;
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
                expr->resolved_type = left;
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
                expr->resolved_type = type_bool;
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
                expr->resolved_type = type_bool;
                return type_bool;
            }


            // Logical: &&, ||
            if (op == BINOP_AND || op == BINOP_OR) {
                if (left->kind != TYPE_BOOL || right->kind != TYPE_BOOL) {
                    checker_error(expr->loc, "logical operation requires boolean operands");
                    return NULL;
                }
                expr->resolved_type = type_bool;
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
                expr->resolved_type = operand;
                return operand;
            }

            if (op == UNOP_NOT) {
                if (operand->kind != TYPE_BOOL) {
                    checker_error(expr->loc, "logical not requires boolean operand");
                    return NULL;
                }
                expr->resolved_type = type_bool;
                return type_bool;
            }

            if (op == UNOP_ADDR) {
                if (!is_lvalue(expr->data.unop.operand)) {
                    checker_error(expr->loc, "cannot take address of expression without memory location");
                    return NULL;
                }
                Type *ptr = type_create_pointer(operand, !checker_state.in_type_resolution);
                expr->resolved_type = ptr;
                return ptr;
            }

            if (op == UNOP_DEREF) {
                // Dereference: *ptr returns T where ptr has type *T
                if (operand->kind != TYPE_POINTER) {
                    checker_error(expr->loc, "dereference requires a pointer operand");
                    return NULL;
                }
                expr->resolved_type = operand->data.ptr.base;
                return operand->data.ptr.base;
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

                AstNode *converted = maybe_insert_cast(args[i], arg_type, param_types[i]);
                if (!converted) {
                    checker_error(args[i]->loc, "argument %zu type mismatch in call to '%s'",
                                 i + 1, func_name);
                } else {
                    args[i] = converted;
                }
            }

            expr->resolved_type = return_type;
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
            expr->resolved_type = array_type->data.array.element;
            return array_type->data.array.element;
        }

        case AST_EXPR_SLICE: {
            AstNode *array_expr = expr->data.slice_expr.array;
            AstNode *start_expr = expr->data.slice_expr.start;
            AstNode *end_expr = expr->data.slice_expr.end;

            // Check the array expression
            Type *array_type = check_expression(array_expr);
            if (!array_type) {
                return NULL;
            }

            // Can slice arrays or slices
            if (array_type->kind != TYPE_ARRAY && array_type->kind != TYPE_SLICE) {
                checker_error(array_expr->loc, "cannot slice non-array/slice type");
                return NULL;
            }

            // Get element type
            Type *element_type = NULL;
            if (array_type->kind == TYPE_ARRAY) {
                element_type = array_type->data.array.element;
            } else {
                element_type = array_type->data.slice.element;
            }

            // Check start index if present
            if (start_expr) {
                Type *start_type = check_expression(start_expr);
                if (!start_type) {
                    return NULL;
                }
                if (start_type->kind != TYPE_INT) {
                    checker_error(start_expr->loc, "slice start index must be an integer");
                    return NULL;
                }
            }

            // Check end index if present
            if (end_expr) {
                Type *end_type = check_expression(end_expr);
                if (!end_type) {
                    return NULL;
                }
                if (end_type->kind != TYPE_INT) {
                    checker_error(end_expr->loc, "slice end index must be an integer");
                    return NULL;
                }
            }

            // Return slice type
            Type *slice = type_create_slice(element_type, !checker_state.in_type_resolution);
            expr->resolved_type = slice;
            return slice;
        }

        case AST_EXPR_MEMBER: {
            AstNode *object_expr = expr->data.member_expr.object;
            const char *field_name = expr->data.member_expr.member;

            // Check the object expression
            Type *object_type = check_expression(object_expr);
            if (!object_type) {
                return NULL;
            }

            // Handle tuple member access (numeric fields: .0, .1, .2)
            if (object_type->kind == TYPE_TUPLE) {
                // Parse field name as number
                char *endptr;
                long index = strtol(field_name, &endptr, 10);

                // Verify it's a valid number
                if (*endptr != '\0' || index < 0) {
                    checker_error(expr->loc, "tuple field must be a non-negative integer");
                    return NULL;
                }

                // Check bounds
                if ((size_t)index >= object_type->data.tuple.element_count) {
                    checker_error(expr->loc, "tuple index %ld is out of bounds (tuple has %zu elements)",
                                    index, object_type->data.tuple.element_count);
                    return NULL;
                }

                // Return the element type at this index
                expr->resolved_type = object_type->data.tuple.element_types[index];
                return object_type->data.tuple.element_types[index];
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
                    expr->resolved_type = field_types[i];
                    return field_types[i];
                }
            }

            // Field not found
            checker_error(expr->loc, "struct has no field named '%s'", field_name);
            return NULL;
        }

        case AST_EXPR_TUPLE: {
            // Type-check all tuple elements
            size_t element_count = expr->data.tuple_expr.element_count;
            AstNode **elements = expr->data.tuple_expr.elements;

            Type **element_types = arena_alloc(&long_lived, sizeof(Type*) * element_count);

            for (size_t i = 0; i < element_count; i++) {
                element_types[i] = check_expression(elements[i]);
                if (!element_types[i]) {
                    return NULL;  // Error already reported
                }
            }

            // Create and return tuple type
            Type *tuple =  type_create_tuple(element_types, element_count, !checker_state.in_type_resolution);
            expr->data.tuple_expr.resolved_type = tuple;
            return tuple;
        }

        case AST_EXPR_STRUCT_LITERAL: {
            const char *type_name = expr->data.struct_literal.type_name;
            char **field_names = expr->data.struct_literal.field_names;
            AstNode **field_values = expr->data.struct_literal.field_values;
            size_t field_count = expr->data.struct_literal.field_count;

            // Look up the type
            Symbol *type_sym = scope_lookup(current_scope, type_name);
            if (!type_sym) {
                checker_error(expr->loc, "undefined type '%s'", type_name);
                return NULL;
            }

            if (type_sym->kind != SYMBOL_TYPE) {
                checker_error(expr->loc, "'%s' is not a type", type_name);
                return NULL;
            }

            Type *struct_type = type_sym->type;
            if (struct_type->kind != TYPE_STRUCT) {
                checker_error(expr->loc, "'%s' is not a struct type", type_name);
                return NULL;
            }

            // Verify field count matches
            size_t expected_count = struct_type->data.struct_data.field_count;
            if (field_count != expected_count) {
                checker_error(expr->loc, "struct literal has %zu fields, but type '%s' has %zu fields",
                             field_count, type_name, expected_count);
                return NULL;
            }

            // Check each field
            char **expected_names = struct_type->data.struct_data.field_names;
            Type **expected_types = struct_type->data.struct_data.field_types;

            for (size_t i = 0; i < field_count; i++) {
                // Find this field in the struct type
                bool found = false;
                for (size_t j = 0; j < expected_count; j++) {
                    if (strcmp(field_names[i], expected_names[j]) == 0) {
                        found = true;

                        // Type-check the value
                        Type *value_type = check_expression(field_values[i]);
                        if (!value_type) {
                            return NULL;  // Error already reported
                        }

                        if (!type_equals(value_type, expected_types[j])) {
                            checker_error(field_values[i]->loc,
                                         "field '%s' has type mismatch in struct literal", field_names[i]);
                            return NULL;
                        }
                        break;
                    }
                }

                if (!found) {
                    checker_error(expr->loc, "struct '%s' has no field named '%s'",
                                 type_name, field_names[i]);
                    return NULL;
                }
            }

            expr->resolved_type = struct_type;
            return struct_type;
        }

        case AST_EXPR_ARRAY_LITERAL: {
            size_t element_count = expr->data.array_literal.element_count;
            AstNode **elements = expr->data.array_literal.elements;

            // Empty array literal - we can't infer the type
            if (element_count == 0) {
                checker_error(expr->loc, "cannot infer type of empty array literal");
                return NULL;
            }

            // Type-check first element to infer array element type
            Type *element_type = check_expression(elements[0]);
            if (!element_type) {
                return NULL;  // Error already reported
            }

            // Check all other elements have the same type
            for (size_t i = 1; i < element_count; i++) {
                Type *elem_type = check_expression(elements[i]);
                if (!elem_type) {
                    return NULL;  // Error already reported
                }

                if (!type_equals(elem_type, element_type)) {
                    checker_error(elements[i]->loc,
                                 "array literal elements must all have the same type");
                    return NULL;
                }
            }

            // Create and return array type with inferred element type and size
            Type* array = type_create_array(element_type, element_count, !checker_state.in_type_resolution);
            expr->resolved_type = array;
            return array;
        }

        case AST_EXPR_IMPLICIT_CAST: {
            // The cast was already validated when inserted
            // Just return the target type
            return expr->data.implicit_cast.target_type;
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

            // Verify it matches expected return type (with implicit cast if needed)
            AstNode *converted = maybe_insert_cast(expr, expr_type, expected_return_type);
            if (!converted) {
                checker_error(stmt->loc, "return type mismatch");
            } else {
                stmt->data.return_stmt.expr = converted;  // Replace with cast node if needed
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

            if (lhs_type && rhs_type) {
                AstNode *converted = maybe_insert_cast(rhs, rhs_type, lhs_type);
                if (!converted) {
                    checker_error(stmt->loc, "assignment type mismatch");
                } else {
                    stmt->data.assign_stmt.rhs = converted;  // Replace with cast node if needed
                }
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

                if (var_type) {
                    // Type is specified, check compatibility and insert cast if needed
                    AstNode *converted = maybe_insert_cast(init, init_type, var_type);
                    if (!converted) {
                        checker_error(init->loc, "initializer type mismatch");
                        return false;
                    }
                    stmt->data.var_decl.init = converted;  // Replace with cast node if needed
                } else {
                    // No type specified, infer from initializer
                    var_type = init_type;
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
            stmt->resolved_type = var_type;
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

            if (const_type) {
                // Type specified, check compatibility and insert cast if needed
                AstNode *converted = maybe_insert_cast(value, value_type, const_type);
                if (!converted) {
                    checker_error(value->loc, "constant initializer type mismatch");
                    return false;
                }
                stmt->data.const_decl.value = converted;  // Replace with cast node if needed
            } else {
                // No type specified, infer from value
                const_type = value_type;
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
            stmt->resolved_type = const_type;
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
