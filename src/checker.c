#include "checker.h"
#include "alloc.h"
#include "ast.h"
#include "module.h"
#include "options.h"
#include "symbol.h"
#include "type.h"
#include "uthash.h"
#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

// External allocator
extern Arena long_lived;

// Checker state (private to this file)
typedef struct {
  bool has_errors;
  int error_count;
  bool in_type_resolution;
  bool in_loop;
  bool in_defer;
  size_t anonymous_functions;
  CallingConvention current_convention;
} CheckerState;

static CheckerState checker_state;

void checker_init(void) {
  checker_state.has_errors = false;
  checker_state.error_count = 0;
  checker_state.in_type_resolution = false;
  checker_state.in_loop = false;
  checker_state.anonymous_functions = 0;
  symbol_table_init(); // Create fresh global scope
}

static char *next_anonymous_function_name() {
  char buffer[32];
  memset(buffer, 0, 32);

  sprintf(buffer, "__anonymous_function_%ld",
          checker_state.anonymous_functions++);
  return str_dup(buffer);
}

bool checker_has_errors(void) { return checker_state.has_errors; }

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

void checker_warning(Location loc, const char *fmt, ...) {
  fprintf(stderr, "%s:%d:%d: warning: ", loc.file, loc.line, loc.column);

  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);

  fprintf(stderr, "\n");
}

static AstNode *maybe_insert_cast(AstNode *expr, Type *expr_type,
                                  Type *target_type);

static bool is_valid_cast(Type *from, Type *to);

static bool check_statement(AstNode *stmt, Type *expected_return_type);

static bool check_convention(AstNode *conv) {
  if (!conv) {
    // Assumes "pebble"
    return true;
  }

  // NOTE: string includes quotes

  assert(conv->kind == AST_EXPR_LITERAL_STRING);
  if (strcmp("\"c\"", conv->data.str_lit.value) == 0 ||
      strcmp("\"C\"", conv->data.str_lit.value) == 0) {
    return true;
  }
  if (strcmp("\"pebble\"", conv->data.str_lit.value) == 0) {
    return true;
  }

  checker_error(
      conv->loc,
      "unknown calling convention %s, expect \"c\" or \"pebble\" (default)",
      conv->data.str_lit.value);
  return false;
}

static CallingConvention convention_from_node(AstNode *conv) {
  if (!conv) {
    return CALL_CONV_PEBBLE;
  }

  // NOTE: string includes quotes

  assert(conv->kind == AST_EXPR_LITERAL_STRING);
  if (strcmp("\"c\"", conv->data.str_lit.value) == 0 ||
      strcmp("\"C\"", conv->data.str_lit.value) == 0) {
    return CALL_CONV_C;
  }
  if (strcmp("\"pebble\"", conv->data.str_lit.value) == 0) {
    return CALL_CONV_PEBBLE;
  }

  // Error would've been reported, just assume pebble
  return CALL_CONV_PEBBLE;
}

//=============================================================================
// PASS 2: COLLECT GLOBALS
//============================================================================

// Collect a single declaration into the global scope
static void collect_declaration(AstNode *decl) {
  char *name = NULL;
  char *qualified_name = NULL;
  SymbolKind kind;
  Location loc;
  bool is_opaque_type = false;

  // Extract name and kind based on declaration type
  switch (decl->kind) {
  case AST_DECL_FUNCTION:
    name = decl->data.func_decl.name;
    qualified_name = decl->data.func_decl.qualified_name;
    kind = SYMBOL_FUNCTION;
    loc = decl->loc;
    break;
  case AST_DECL_EXTERN_FUNC:
    name = decl->data.extern_func.name;
    qualified_name = decl->data.extern_func.name;
    kind = SYMBOL_EXTERN_FUNCTION;
    loc = decl->loc;
    break;
  case AST_DECL_EXTERN_TYPE:
    name = decl->data.extern_type.name;
    qualified_name = decl->data.extern_type.name;
    kind = SYMBOL_TYPE;
    is_opaque_type = true;
    loc = decl->loc;
    break;
  case AST_DECL_EXTERN_VARIABLE:
    name = decl->data.extern_var_decl.name;
    qualified_name = decl->data.extern_var_decl.name;
    kind = SYMBOL_EXTERN_VARIABLE;
    loc = decl->loc;
    break;
  case AST_DECL_EXTERN_CONSTANT:
    name = decl->data.extern_const_decl.name;
    qualified_name = decl->data.extern_const_decl.name;
    kind = SYMBOL_EXTERN_CONSTANT;
    loc = decl->loc;
    break;
  case AST_DECL_EXTERN_BLOCK: {
    size_t count = decl->data.extern_block.decls_count;
    if (count == 0) {
      return;
    }

    for (size_t i = 0; i < count; i++) {
      collect_declaration(decl->data.extern_block.decls[i]);
    }

    return;
  }
  case AST_DECL_VARIABLE:
    name = decl->data.var_decl.name;
    qualified_name = decl->data.var_decl.qualified_name;
    kind = SYMBOL_VARIABLE;
    loc = decl->loc;
    break;
  case AST_DECL_CONSTANT:
    name = decl->data.const_decl.name;
    qualified_name = decl->data.const_decl.qualified_name;
    kind = SYMBOL_CONSTANT;
    loc = decl->loc;
    break;
  case AST_DECL_TYPE:
    name = decl->data.type_decl.name;
    qualified_name = decl->data.type_decl.qualified_name;
    kind = SYMBOL_TYPE;
    loc = decl->loc;
    break;
  default:
    return; // Not a declaration
  }

  // Check for duplicates
  Symbol *existing = scope_lookup_local(global_scope, qualified_name);
  if (existing) {
    checker_error(decl->loc, "duplicate declaration of '%s'", name);
    checker_error(existing->decl->loc, "previous declaration was here");
    return;
  }

  // Create and add symbol
  Symbol *symbol = symbol_create(qualified_name, kind, decl);
  if (kind == SYMBOL_VARIABLE) {
    symbol->data.var.is_global = true;
  }
  if (kind == SYMBOL_EXTERN_FUNCTION) {
    AstNode *lib_name = decl->data.extern_func.lib_name;
    if (lib_name) {
      symbol->data.external.lib_name = lib_name->data.str_lit.value;

      // Add as library
      // FIXME: Might need to strip the quotes
      append_library_string(lib_name->data.str_lit.value);
    }
  }
  if (is_opaque_type) {
    symbol->type = type_create(TYPE_OPAQUE, loc);
    symbol->type->declared_name = symbol->name;
    symbol->type->canonical_name = symbol->name;
  }

  symbol->reg_name = name;

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
  if (!type)
    return false;

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
    return true; // Cycle detected
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
      cycle_detected = false;
    }
    break;

  case TYPE_OPTIONAL:
    if (canonicalize_type_internal(&type->data.optional.base, visited)) {
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
      if (canonicalize_type_internal(&type->data.tuple.element_types[i],
                                     visited)) {
        cycle_detected = true;
      }
    }
    break;

  case TYPE_STRUCT:
    for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
      if (canonicalize_type_internal(&type->data.struct_data.field_types[i],
                                     visited)) {
        cycle_detected = true;
      }
    }
    break;

  case TYPE_FUNCTION:
    for (size_t i = 0; i < type->data.func.param_count; i++) {
      if (canonicalize_type_internal(&type->data.func.param_types[i],
                                     visited)) {
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
#define GET_COMPONENT_NAME(component)                                          \
  ({                                                                           \
    Type *_c = (component);                                                    \
    char *_name;                                                               \
    if (_c->canonical_name) {                                                  \
      /* Already canonicalized */                                              \
      _name = _c->canonical_name;                                              \
    } else {                                                                   \
      /* Not canonicalized yet - check if in cycle */                          \
      Visited *_found;                                                         \
      HASH_FIND_PTR(*visited, &_c, _found);                                    \
      if (_found) {                                                            \
        /* In cycle - use qualified name if available */                       \
        if (_c->qualified_name) {                                              \
          _name = _c->qualified_name;                                          \
        } else {                                                               \
          _name = "UNRESOLVED";                                                \
        }                                                                      \
      } else {                                                                 \
        /* Not in visited and no canonical name - shouldn't happen */          \
        _name = "UNRESOLVED";                                                  \
      }                                                                        \
    }                                                                          \
    _name;                                                                     \
  })

  switch (type->kind) {
  case TYPE_INT:
    canonical_name = type_int->canonical_name;
    break;

  case TYPE_BOOL:
    canonical_name = type_bool->canonical_name;
    break;

  case TYPE_STRING:
    canonical_name = type_string->canonical_name;
    break;

  case TYPE_VOID:
    canonical_name = type_void->canonical_name;
    break;

  case TYPE_F32:
    canonical_name = type_f32->canonical_name;
    break;

  case TYPE_F64:
    canonical_name = type_f64->canonical_name;
    break;

  case TYPE_U8:
    canonical_name = type_u8->canonical_name;
    break;

  case TYPE_U16:
    canonical_name = type_u16->canonical_name;
    break;

  case TYPE_U32:
    canonical_name = type_u32->canonical_name;
    break;

  case TYPE_U64:
    canonical_name = type_u64->canonical_name;
    break;

  case TYPE_USIZE:
    canonical_name = type_usize->canonical_name;
    break;

  case TYPE_I8:
    canonical_name = type_i8->canonical_name;
    break;

  case TYPE_I16:
    canonical_name = type_i16->canonical_name;
    break;

  case TYPE_I32:
    canonical_name = type_i32->canonical_name;
    break;

  case TYPE_I64:
    canonical_name = type_i64->canonical_name;
    break;

  case TYPE_ISIZE:
    canonical_name = type_isize->canonical_name;
    break;

  case TYPE_CHAR:
    canonical_name = type_char->canonical_name;
    break;

  case TYPE_NONE:
    canonical_name = type_none->canonical_name;
    break;

  case TYPE_POINTER: {
    char *base_name = GET_COMPONENT_NAME(type->data.ptr.base);
    size_t len = strlen("ptr_") + strlen(base_name) + 1;
    canonical_name = arena_alloc(&long_lived, len);
    snprintf(canonical_name, len, "ptr_%s", base_name);
    break;
  }

  case TYPE_OPTIONAL: {
    char *base_name = GET_COMPONENT_NAME(type->data.ptr.base);
    size_t len = strlen("optional_") + strlen(base_name) + 1;
    canonical_name = arena_alloc(&long_lived, len);
    snprintf(canonical_name, len, "optional_%s", base_name);
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
    snprintf(canonical_name, len, "array_%zu_%s", type->data.array.size,
             elem_name);
    break;
  }

  case TYPE_TUPLE: {
    // If cyclic, must use nominal name
    if (cycle_detected) {
      checker_error(
          type->loc,
          "recursive type '%s' has infinite size (use pointer for indirection)",
          type->declared_name);
      canonical_name = str_dup(type->declared_name);
    } else if (type->qualified_name) {
      canonical_name = str_dup(type->qualified_name);
    } else {
      if (type->declared_name)
        return type->declared_name;
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

        offset += snprintf(canonical_name + offset, capacity - offset, "_%s",
                           elem_name);
      }
    }
    break;
  }

  case TYPE_STRUCT: {
    // Structs are always nominal (use declared name)
    if (cycle_detected) {
      checker_error(
          type->loc,
          "recursive type '%s' has infinite size (use pointer for indirection)",
          type->declared_name);
      canonical_name = str_dup(type->declared_name);
    } else if (type->qualified_name) {
      canonical_name = str_dup(type->qualified_name);
    } else {
      // Anonymous struct - build structural name
      size_t capacity = 256;
      canonical_name = arena_alloc(&long_lived, capacity);
      size_t offset = 0;

      offset += snprintf(canonical_name + offset, capacity - offset, "struct");

      for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
        char *field_name = type->data.struct_data.field_names[i];
        char *type_name =
            GET_COMPONENT_NAME(type->data.struct_data.field_types[i]);

        size_t needed =
            offset + 1 + strlen(field_name) + 1 + strlen(type_name) + 1;
        if (needed > capacity) {
          capacity = needed * 2;
          char *new_buf = arena_alloc(&long_lived, capacity);
          memcpy(new_buf, canonical_name, offset);
          canonical_name = new_buf;
        }

        offset += snprintf(canonical_name + offset, capacity - offset, "_%s_%s",
                           field_name, type_name);
      }
    }
    break;
  }

  case TYPE_ENUM: {
    if (type->qualified_name) {
      canonical_name = str_dup(type->qualified_name);
    } else {
      // Anonymous enum - build structural name
      size_t capacity = 256;
      canonical_name = arena_alloc(&long_lived, capacity);
      size_t offset = 0;

      offset += snprintf(canonical_name + offset, capacity - offset, "enum");

      for (size_t i = 0; i < type->data.enum_data.variant_count; i++) {
        char *variant = type->data.enum_data.variant_names[i];

        size_t needed = offset + 1 + strlen(variant) + 1;
        if (needed > capacity) {
          capacity = needed * 2;
          char *new_buf = arena_alloc(&long_lived, capacity);
          memcpy(new_buf, canonical_name, offset);
          canonical_name = new_buf;
        }

        offset += snprintf(canonical_name + offset, capacity - offset, "_%s",
                           variant);
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

  case TYPE_OPAQUE:
    canonical_name = type->canonical_name;
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
  if (!type_ref || !*type_ref)
    return;

  Visited *visited = NULL;
  canonicalize_type_internal(type_ref, &visited);

  // Clean up visited
  Visited *curr, *tmp;
  HASH_ITER(hh, visited, curr, tmp) { HASH_DEL(visited, curr); }
}

// Sub-pass 3a-prime: Compute canonical names and deduplicate types
static void canonicalize_types(void) {
  // Walk all global symbols and canonicalize their types
  Symbol *sym, *tmp;
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    if (sym->type) {
      // Type hasn't been canonicalized yet
      // Don't attempt to canonicalize opaque types
      if (sym->type->kind != TYPE_OPAQUE)
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
    // Opaque types have their sym->type already set
    if (sym->kind == SYMBOL_TYPE && sym->type == NULL) {
      if (worklist_size >= worklist_capacity) {
        worklist_capacity = worklist_capacity == 0 ? 8 : worklist_capacity * 2;
        worklist = realloc(worklist, sizeof(Symbol *) * worklist_capacity);
      }
      worklist[worklist_size++] = sym;
    }
  }

  checker_state.in_type_resolution = true;

  // PHASE 1: Pre-register all type names as placeholders
  for (size_t i = 0; i < worklist_size; i++) {
    sym = worklist[i];
    Type *placeholder = type_create(TYPE_UNRESOLVED, sym->decl->loc);
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
        Type *placeholder = type_lookup(sym->name, sym->decl->loc.file);

        // Always mutate placeholder in place
        if (placeholder && placeholder->kind == TYPE_UNRESOLVED) {
          *placeholder = *resolved;
        }

        if (placeholder->kind == TYPE_STRUCT ||
            placeholder->kind == TYPE_TUPLE || placeholder->kind == TYPE_ENUM) {
          placeholder->qualified_name = str_dup(sym->name);
          placeholder->declared_name = str_dup(sym->reg_name);
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
                  "cannot resolve type '%s' (circular dependency)", sym->name);
  }

  checker_state.in_type_resolution = false;
  free(worklist);

  // Sub-pass 3a' - canonicalize and deduplicate
  canonicalize_types();
}

static bool type_is_int(Type *type) {
  return type->kind == TYPE_INT || type->kind == TYPE_I32;
}

static bool type_is_integral(Type *type) {
  return type->kind == TYPE_U8 || type->kind == TYPE_U16 ||
         type->kind == TYPE_U32 || type->kind == TYPE_U64 ||
         type->kind == TYPE_USIZE || type->kind == TYPE_I8 ||
         type->kind == TYPE_I16 || type->kind == TYPE_I32 ||
         type->kind == TYPE_I64 || type->kind == TYPE_ISIZE ||
         type->kind == TYPE_INT;
}

static bool type_is_floating(Type *type) {
  return type->kind == TYPE_F32 || type->kind == TYPE_F64;
}

// Sub-pass 3b: Check constant declarations
static void check_global_constants(void) {
  Symbol *sym, *tmp;

  // Iterate over all symbols in global scope
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    // Only process constant declarations
    if (sym->kind != SYMBOL_CONSTANT && sym->kind != SYMBOL_EXTERN_CONSTANT) {
      continue;
    }

    if (sym->kind == SYMBOL_CONSTANT) {
      AstNode *decl = sym->decl;
      AstNode *type_expr = decl->data.const_decl.type_expr;
      AstNode *value = decl->data.const_decl.value;

      // Rule: Constants must have an initializer
      if (!value) {
        checker_error(decl->loc, "global constant '%s' must be initialized",
                      sym->name);
        continue;
      }

      // Rule: For now, only allow literal initializers
      if (value->kind != AST_EXPR_LITERAL_INT &&
          value->kind != AST_EXPR_LITERAL_FLOAT &&
          value->kind != AST_EXPR_LITERAL_STRING &&
          value->kind != AST_EXPR_LITERAL_BOOL) {
        checker_error(value->loc,
                      "global constant initializer must be a literal "
                      "(complex expressions not yet supported)");
        continue;
      }

      // Check the initializer expression
      Type *inferred_type = check_expression(value);
      if (!inferred_type) {
        continue; // Error already reported
      }

      // If explicit type is given, resolve and verify it matches
      if (type_expr) {
        Type *explicit_type = resolve_type_expression(type_expr);
        if (!explicit_type) {
          continue; // Error already reported
        }

        // Allow casting ints to pointers for global constants
        if (explicit_type->kind == TYPE_POINTER && type_is_int(inferred_type)) {
          sym->type = explicit_type;
          sym->decl->resolved_type = explicit_type;

          sym->decl->data.var_decl.init =
              maybe_insert_cast(value, inferred_type, explicit_type);

          continue;
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
    } else {
      AstNode *decl = sym->decl;
      AstNode *type_expr = decl->data.extern_const_decl.type_expr;

      Type *explicit_type = resolve_type_expression(type_expr);
      if (!explicit_type) {
        continue;
      }

      sym->type = explicit_type;
      sym->decl->resolved_type = explicit_type;
    }
  }
}

// Sub-pass 3c: Check global variable declarations
static void check_global_variables(void) {
  Symbol *sym, *tmp;

  // Iterate over all symbols in global scope
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    // Only process variable declarations
    if (sym->kind != SYMBOL_VARIABLE && sym->kind != SYMBOL_EXTERN_VARIABLE) {
      continue;
    }

    if (sym->kind == SYMBOL_VARIABLE) {
      AstNode *decl = sym->decl;
      AstNode *type_expr = decl->data.var_decl.type_expr;
      AstNode *init = decl->data.var_decl.init;

      // Rule: Must have type or initializer (or both)
      if (!type_expr && !init) {
        checker_error(decl->loc,
                      "variable '%s' must have either a type annotation or an "
                      "initializer",
                      sym->name);
        continue;
      }

      Type *explicit_type = NULL;
      Type *inferred_type = NULL;

      // Resolve explicit type if provided
      if (type_expr) {
        explicit_type = resolve_type_expression(type_expr);
        if (!explicit_type) {
          continue; // Error already reported
        }
      }

      // Check initializer if provided
      if (init) {
        // Rule: For now, only allow literal initializers
        if (init->kind != AST_EXPR_LITERAL_INT &&
            init->kind != AST_EXPR_LITERAL_FLOAT &&
            init->kind != AST_EXPR_LITERAL_STRING &&
            init->kind != AST_EXPR_LITERAL_BOOL) {
          checker_error(
              init->loc,
              "global variable initializer must be a literal (complex "
              "expressions not yet supported)");
          continue;
        }

        inferred_type = check_expression(init);
        if (!inferred_type) {
          continue; // Error already reported
        }
      }

      // Verify types match if both are present
      if (explicit_type && inferred_type) {
        // Allow casting ints to pointers for global vars
        if (explicit_type->kind == TYPE_POINTER && type_is_int(inferred_type)) {
          sym->type = explicit_type;
          sym->decl->resolved_type = explicit_type;

          sym->decl->data.var_decl.init =
              maybe_insert_cast(init, inferred_type, explicit_type);

          continue;
        }

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
    } else {
      AstNode *decl = sym->decl;
      AstNode *type_expr = decl->data.extern_var_decl.type_expr;

      Type *explicit_type = resolve_type_expression(type_expr);
      if (!explicit_type) {
        continue;
      }

      sym->type = explicit_type;
      sym->decl->resolved_type = explicit_type;
    }
  }
}

// Sub-pass 3d: Check function signatures
static void check_function_signatures(void) {
  Symbol *sym, *tmp;

  // Iterate over all symbols in global scope
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    // Only process function declarations
    if (sym->kind != SYMBOL_FUNCTION && sym->kind != SYMBOL_EXTERN_FUNCTION) {
      continue;
    }

    AstNode *decl = sym->decl;
    FuncParam *params;
    size_t param_count = 0;
    AstNode *return_type_expr;
    CallingConvention convention = CALL_CONV_C;

    if (decl->kind == AST_DECL_FUNCTION) {
      params = decl->data.func_decl.params;
      param_count = decl->data.func_decl.param_count;
      return_type_expr = decl->data.func_decl.return_type;

      check_convention(decl->data.func_decl.convention);

      convention = convention_from_node(decl->data.func_decl.convention);
    } else if (decl->kind == AST_DECL_EXTERN_FUNC) {
      params = decl->data.extern_func.params;
      param_count = decl->data.extern_func.param_count;
      return_type_expr = decl->data.extern_func.return_type;
      // NOTE: extern functions cannot define a convention
    }

    int is_variadic = -1;

    // Resolve parameter types
    Type **param_types = NULL;
    if (param_count > 0) {
      param_types = arena_alloc(&long_lived, sizeof(Type *) * param_count);
      for (size_t i = 0; i < param_count; i++) {
        if (is_variadic != -1) {
          checker_error(decl->loc,
                        "Parameter '%s' is marked as variadic but parameter "
                        "'%s' is already variadic",
                        params[i].name, params[is_variadic].name);
        }

        if (params[i].is_variadic) {
          is_variadic = (int)i;
        }

        param_types[i] = resolve_type_expression(params[i].type);
        if (!param_types[i]) {
          // Error already reported, but keep going to check other params
          param_types[i] = type_int; // Use placeholder to continue
        }
      }
    }

    // Resolve return type
    Type *return_type = resolve_type_expression(return_type_expr);
    if (!return_type) {
      continue; // Error already reported
    }

    // Create function type
    sym->type = type_create_function(
        param_types, param_count, return_type, is_variadic != -1,
        !checker_state.in_type_resolution, convention, sym->decl->loc);

    // Add parameters as symbols in the function scope
    if (sym->kind == SYMBOL_FUNCTION) {
      // Create function's local scope with global as parent
      Scope *func_scope = scope_create(global_scope);
      sym->data.func.local_scope = func_scope;
      for (size_t i = 0; i < param_count; i++) {
        if (!param_types[i]) {
          continue; // Skip if type resolution failed
        }

        // Create parameter symbol
        Symbol *param_sym =
            symbol_create(params[i].name, SYMBOL_VARIABLE, decl);
        param_sym->type = param_types[i];
        param_sym->data.var.is_global = false; // Parameters are local

        // Check for duplicate parameter names
        Symbol *existing = scope_lookup_local(func_scope, params[i].name);
        if (existing) {
          checker_error(decl->loc, "duplicate parameter name '%s'",
                        params[i].name);
          continue;
        }

        scope_add_symbol(func_scope, param_sym);
      }
    }
  }
}

bool check_anonymous_functions(void) {
  Symbol *sym, *tmp;

  // Check signatures of anonymous functions
  HASH_ITER(hh, anonymous_funcs->symbols, sym, tmp) {
    assert(sym->kind == SYMBOL_ANON_FUNCTION);

    AstNode *decl = sym->decl;
    FuncParam *params = decl->data.func_expr.params;
    size_t param_count = decl->data.func_expr.param_count;
    AstNode *return_type_expr = decl->data.func_expr.return_type;

    check_convention(decl->data.func_expr.convention);

    // Resolve parameter types
    Type **param_types = NULL;
    if (param_count > 0) {
      param_types = arena_alloc(&long_lived, sizeof(Type *) * param_count);
      for (size_t i = 0; i < param_count; i++) {
        param_types[i] = resolve_type_expression(params[i].type);
        if (!param_types[i]) {
          // Error already reported, but keep going to check other params
          param_types[i] = type_int; // Use placeholder to continue
        }
      }
    }

    // Resolve return type
    Type *return_type = resolve_type_expression(return_type_expr);
    if (!return_type) {
      continue; // Error already reported
    }

    // Add parameters as symbols in the function scope
    // Create function's local scope with global as parent
    Scope *func_scope = scope_create(global_scope);
    sym->data.func.local_scope = func_scope;
    for (size_t i = 0; i < param_count; i++) {
      if (!param_types[i]) {
        continue; // Skip if type resolution failed
      }

      // Create parameter symbol
      Symbol *param_sym = symbol_create(params[i].name, SYMBOL_VARIABLE, decl);
      param_sym->type = param_types[i];
      param_sym->data.var.is_global = false; // Parameters are local

      // Check for duplicate parameter names
      Symbol *existing = scope_lookup_local(func_scope, params[i].name);
      if (existing) {
        checker_error(decl->loc, "duplicate parameter name '%s'",
                      params[i].name);
        continue;
      }

      scope_add_symbol(func_scope, param_sym);
    }
  }

  // Check anonymous functions
  HASH_ITER(hh, anonymous_funcs->symbols, sym, tmp) {
    assert(sym->kind == SYMBOL_ANON_FUNCTION);

    // Get function details
    AstNode *decl = sym->decl;
    AstNode *body = decl->data.func_expr.body;
    Type *func_type = sym->type;
    Type *return_type = func_type->data.func.return_type;

    // Push function's local scope (contains parameters)
    scope_push(sym->data.func.local_scope);

    checker_state.current_convention = func_type->data.func.convention;

    // Check the function body
    bool definitely_returns = check_statement(body, return_type);

    // If non-void, ensure all paths return
    if (return_type->kind != TYPE_VOID && !definitely_returns) {
      checker_error(decl->loc,
                    "anonymous function may not return a value on all paths");
    }

    // Pop back to global scope
    scope_pop();
  }

  return !checker_has_errors();
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

  Location loc = type_expr->loc;

  switch (type_expr->kind) {
  case AST_TYPE_NAMED: {
    // Look up named type in type table
    char *name = type_expr->data.type_named.name;
    Type *type = type_lookup(name, loc.file);

    if (!type) {
      // During type resolution, check if it's an unresolved type decl
      if (checker_state.in_type_resolution) { // ADD THIS CHECK
        Symbol *sym = scope_lookup(global_scope, name, type_expr->loc.file);
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

  case AST_TYPE_QUALIFIED_NAMED: {
    // Look up qualified named type in type table
    char *mod = type_expr->data.type_qualified_named.mod_name;
    char *mem = type_expr->data.type_qualified_named.mem_name;

    char *prefix = prepend(mod, "__");
    char *qualified_name = prepend(prefix, mem);

    Type *type = type_lookup(qualified_name, loc.file);

    if (!type) {
      // During type resolution, check if it's an unresolved type decl
      if (checker_state.in_type_resolution) { // ADD THIS CHECK
        Symbol *sym = scope_lookup_local(global_scope, qualified_name);
        if (sym && sym->kind == SYMBOL_TYPE && sym->type == NULL) {
          // It's a forward reference - return NULL to retry later
          return NULL;
        }
      }

      // Otherwise, it's truly undefined
      checker_error(type_expr->loc, "undefined type '%s::%s'", mod, mem);
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
    return type_create_pointer(base, !checker_state.in_type_resolution, loc);
  }

  case AST_TYPE_OPTIONAL: {
    // Resolve base type and create pointer type
    Type *base = resolve_type_expression(type_expr->data.type_optional.base);
    if (!base) {
      return NULL;
    }
    return type_create_optional(base, !checker_state.in_type_resolution, loc);
  }

  case AST_TYPE_ARRAY: {
    // Resolve element type and create array type
    Type *element = resolve_type_expression(type_expr->data.type_array.element);
    if (!element) {
      return NULL;
    }
    if (element->kind == TYPE_OPAQUE) {
      checker_error(type_expr->loc,
                    "Cannot have array of opaque type '%s'"
                    "(use pointer instead)",
                    element->canonical_name);
      return NULL;
    }
    size_t size = type_expr->data.type_array.size;
    return type_create_array(element, size, !checker_state.in_type_resolution,
                             loc);
  }

  case AST_TYPE_SLICE: {
    // Resolve element type and create slice (array with size 0)
    // We might need to change this later
    Type *element = resolve_type_expression(type_expr->data.type_slice.element);
    if (!element) {
      return NULL;
    }
    if (element->kind == TYPE_OPAQUE) {
      checker_error(type_expr->loc,
                    "Cannot have slice of opaque type '%s'"
                    "(use pointer instead)",
                    element->canonical_name);
      return NULL;
    }
    return type_create_slice(element, !checker_state.in_type_resolution, loc);
  }

  case AST_TYPE_STRUCT: {
    size_t field_count = type_expr->data.type_struct.field_count;
    if (field_count == 0) {
      checker_warning(type_expr->loc,
                      "Struct was declared without any members");
      return type_create_struct(NULL, NULL, field_count, false, loc);
    }

    // Resolve all field types and create struct type
    char **field_names = type_expr->data.type_struct.field_names;
    AstNode **field_type_exprs = type_expr->data.type_struct.field_types;

    Type **field_types = arena_alloc(&long_lived, sizeof(Type *) * field_count);

    // Check duplicate field names
    typedef struct {
      char *name;
      UT_hash_handle hh;
    } variant_entry;
    variant_entry *seen = NULL;

    Arena temp_arena;
    arena_init(&temp_arena, 1024);

    for (size_t i = 0; i < field_count; i++) {
      // Check field name collisions
      variant_entry *entry;
      HASH_FIND_STR(seen, field_names[i], entry);
      if (entry) {
        checker_error(type_expr->loc, "Duplicate struct member '%s'",
                      field_names[i]);
      } else {
        entry = arena_alloc(&temp_arena, sizeof(variant_entry));
        entry->name = field_names[i];
        HASH_ADD_KEYPTR(hh, seen, entry->name, strlen(entry->name), entry);
      }

      // Check types
      field_types[i] = resolve_type_expression(field_type_exprs[i]);
      if (!field_types[i]) {
        HASH_CLEAR(hh, seen);
        arena_free(&temp_arena);
        return NULL;
      }

      if (field_types[i]->kind == TYPE_OPAQUE) {
        checker_error(type_expr->loc,
                      "Cannot have field of opaque type '%s' in struct (use "
                      "pointer instead)",
                      field_types[i]->canonical_name);

        HASH_CLEAR(hh, seen);
        arena_free(&temp_arena);
        return NULL;
      }
    }

    HASH_CLEAR(hh, seen);
    arena_free(&temp_arena);

    return type_create_struct(field_names, field_types, field_count, false,
                              loc);
  }

  case AST_TYPE_ENUM: {
    size_t variant_count = type_expr->data.type_enum.variant_count;
    if (variant_count == 0) {
      checker_warning(type_expr->loc, "Enum was declared without any members");
      return type_create_enum(NULL, variant_count, loc);
    }

    // Resolve all field types and create struct type
    char **variant_names = type_expr->data.type_enum.variant_names;

    // Check duplicate variants
    typedef struct {
      char *name;
      UT_hash_handle hh;
    } variant_entry;
    variant_entry *seen = NULL;

    Arena temp_arena;
    arena_init(&temp_arena, 1024);

    for (size_t i = 0; i < variant_count; i++) {
      variant_entry *entry;
      HASH_FIND_STR(seen, variant_names[i], entry);
      if (entry) {
        checker_error(type_expr->loc, "Duplicate enum variant '%s'",
                      variant_names[i]);
      } else {
        entry = arena_alloc(&temp_arena, sizeof(variant_entry));
        entry->name = variant_names[i];
        HASH_ADD_KEYPTR(hh, seen, entry->name, strlen(entry->name), entry);
      }
    }

    HASH_CLEAR(hh, seen);
    arena_free(&temp_arena);

    return type_create_enum(variant_names, variant_count, loc);
  }

  case AST_TYPE_FUNCTION: {
    // Resolve parameter types and return type
    size_t param_count = type_expr->data.type_function.param_count;
    AstNode **param_type_exprs = type_expr->data.type_function.param_types;
    AstNode *return_type_expr = type_expr->data.type_function.return_type;

    Type **param_types = arena_alloc(&long_lived, sizeof(Type *) * param_count);

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

    check_convention(type_expr->data.type_function.convention);

    CallingConvention convention =
        convention_from_node(type_expr->data.type_function.convention);

    return type_create_function(param_types, param_count, return_type, false,
                                !checker_state.in_type_resolution, convention,
                                loc);
  }

  case AST_TYPE_TUPLE: {
    // Resolve all element types
    size_t element_count = type_expr->data.type_tuple.element_count;
    AstNode **element_type_exprs = type_expr->data.type_tuple.element_types;

    Type **element_types =
        arena_alloc(&long_lived, sizeof(Type *) * element_count);

    for (size_t i = 0; i < element_count; i++) {
      element_types[i] = resolve_type_expression(element_type_exprs[i]);
      if (!element_types[i]) {
        return NULL;
      }
      if (element_types[i]->kind == TYPE_OPAQUE) {
        checker_error(type_expr->loc,
                      "Cannot have tuple field of opaque type '%s'"
                      "(use pointer instead)",
                      element_types[i]->canonical_name);
        return NULL;
      }
    }

    return type_create_tuple(element_types, element_count,
                             !checker_state.in_type_resolution, loc);
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
  if (type_is_numeric(type))
    return true;

  switch (type->kind) {
  case TYPE_BOOL:
  case TYPE_STRING:
  case TYPE_CHAR:
  case TYPE_POINTER:
  case TYPE_ENUM:
    return true;
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
AstNode *maybe_insert_cast(AstNode *expr, Type *expr_type, Type *target_type) {
  if (type_equals(expr_type, target_type)) {
    return expr; // No cast needed
  }

  // Cast integer literals
  if (expr->kind == AST_EXPR_LITERAL_INT && type_is_integral(expr_type) &&
      type_is_integral(target_type)) {
    AstNode *cast = arena_alloc(&long_lived, sizeof(AstNode));
    cast->kind = AST_EXPR_IMPLICIT_CAST;
    cast->loc = expr->loc;
    cast->data.implicit_cast.expr = expr;
    cast->data.implicit_cast.target_type = target_type;
    cast->resolved_type = target_type;
    return cast;
  }

  // Check if implicit conversion is allowed
  if (expr_type->kind == TYPE_POINTER && target_type->kind == TYPE_POINTER) {
    // *void to *T || *T to *void
    if (expr_type->data.ptr.base == type_void ||
        target_type->data.ptr.base == type_void) {
      AstNode *cast = arena_alloc(&long_lived, sizeof(AstNode));
      cast->kind = AST_EXPR_IMPLICIT_CAST;
      cast->loc = expr->loc;
      cast->data.implicit_cast.expr = expr;
      cast->data.implicit_cast.target_type = target_type;
      return cast;
    }
  } else if (expr_type->kind == TYPE_ARRAY && target_type->kind == TYPE_SLICE) {
    // Array [N]T can convert to slice []T
    if (type_equals(expr_type->data.array.element,
                    target_type->data.slice.element)) {
      // Create implicit cast node
      AstNode *cast = arena_alloc(&long_lived, sizeof(AstNode));
      cast->kind = AST_EXPR_IMPLICIT_CAST;
      cast->loc = expr->loc;
      cast->data.implicit_cast.expr = expr;
      cast->data.implicit_cast.target_type = target_type;
      return cast;
    }
  } else if (type_is_integral(expr_type) &&
             (target_type->kind == TYPE_F32 || target_type->kind == TYPE_F64)) {
    // Promote int to float
    AstNode *cast = arena_alloc(&long_lived, sizeof(AstNode));
    cast->kind = AST_EXPR_IMPLICIT_CAST;
    cast->loc = expr->loc;
    cast->data.implicit_cast.expr = expr;
    cast->data.implicit_cast.target_type = target_type;
    return cast;
  } else if (type_is_integral(expr_type) && type_is_integral(target_type)) {
    // Allow int literals to convert to sized integer types
    AstNode *cast = arena_alloc(&long_lived, sizeof(AstNode));
    cast->kind = AST_EXPR_IMPLICIT_CAST;
    cast->loc = expr->loc;
    cast->data.implicit_cast.expr = expr;
    cast->data.implicit_cast.target_type = target_type;
    return cast;
  } else if (expr_type->kind == TYPE_TUPLE && target_type->kind == TYPE_TUPLE) {
    // Check if tuples have the same number of elements
    if (expr_type->data.tuple.element_count !=
        target_type->data.tuple.element_count) {
      return NULL; // Different tuple sizes, no conversion possible
    }

    // Check if all elements match or can be converted
    bool needs_conversion = false;
    for (size_t i = 0; i < expr_type->data.tuple.element_count; i++) {
      if (!type_equals(expr_type->data.tuple.element_types[i],
                       target_type->data.tuple.element_types[i])) {
        needs_conversion = true;
        break;
      }
    }

    // If all elements match exactly, no cast needed
    if (!needs_conversion) {
      return expr;
    }

    // Determine if we are dealing a literal, so we can try to perform
    // implicit casts on its elements
    if (expr->kind != AST_EXPR_TUPLE) {
      return NULL;
    }

    // Create a new tuple expression with recursively casted elements
    AstNode *new_tuple = arena_alloc(&long_lived, sizeof(AstNode));
    new_tuple->kind = AST_EXPR_TUPLE;
    new_tuple->loc = expr->loc;
    new_tuple->data.tuple_expr.element_count =
        target_type->data.tuple.element_count;
    new_tuple->data.tuple_expr.elements = arena_alloc(
        &long_lived, sizeof(AstNode *) * target_type->data.tuple.element_count);

    // Recursively cast each element
    for (size_t i = 0; i < expr_type->data.tuple.element_count; i++) {
      AstNode *casted_elem =
          maybe_insert_cast(expr->data.tuple_expr.elements[i],
                            expr_type->data.tuple.element_types[i],
                            target_type->data.tuple.element_types[i]);

      if (!casted_elem) {
        return NULL; // Element conversion failed
      }

      new_tuple->data.tuple_expr.elements[i] = casted_elem;
    }

    new_tuple->resolved_type = target_type;

    return new_tuple;
  } else if ((expr_type->kind == TYPE_F32 && target_type->kind == TYPE_F64) ||
             (expr_type->kind == TYPE_F64 && target_type->kind == TYPE_F32)) {
    // float -> double
    // double -> float
    return expr;
  } else if (expr_type->kind == TYPE_ARRAY && target_type->kind == TYPE_ARRAY) {
    // Check if arrays have the same number of elements
    if (expr_type->data.array.size != target_type->data.array.size) {
      return NULL; // Different array sizes, no conversion possible
    }

    // If all elements match exactly, no cast needed
    if (type_equals(expr_type->data.array.element,
                    target_type->data.array.element)) {
      return expr;
    }

    // Determine if we are dealing a literal, so we can try to perform
    // implicit casts on its elements
    if (expr->kind != AST_EXPR_ARRAY_LITERAL) {
      return NULL;
    }

    // Create a new array expression with recursively casted elements
    AstNode *new_array = arena_alloc(&long_lived, sizeof(AstNode));
    new_array->kind = AST_EXPR_ARRAY_LITERAL;
    new_array->loc = expr->loc;
    new_array->data.array_literal.element_count = target_type->data.array.size;
    new_array->data.array_literal.elements = arena_alloc(
        &long_lived, sizeof(AstNode *) * target_type->data.array.size);

    // Recursively cast each element
    for (size_t i = 0; i < expr_type->data.array.size; i++) {
      AstNode *casted_elem = maybe_insert_cast(
          expr->data.array_literal.elements[i], expr_type->data.array.element,
          target_type->data.array.element);

      if (!casted_elem) {
        return NULL; // Element conversion failed
      }

      new_array->data.array_literal.elements[i] = casted_elem;
    }

    new_array->resolved_type = target_type;

    return new_array;
  } else if (expr_type->kind == TYPE_NONE &&
             target_type->kind == TYPE_OPTIONAL) {
    // Allow implicit case of none to any type ?T
    expr->resolved_type = target_type;
    return expr;
  } else if (expr_type->kind == TYPE_OPTIONAL &&
             target_type->kind == TYPE_OPTIONAL) {

    // If all elements match exactly, no cast needed
    if (type_equals(expr_type->data.optional.base,
                    target_type->data.optional.base)) {
      return expr;
    }

    // Determine if we are dealing a literal, so we can try to perform
    // an implicit cast on its element
    if (expr->kind != AST_EXPR_SOME) {
      return NULL;
    }

    AstNode *new_some = arena_alloc(&long_lived, sizeof(AstNode));
    new_some->kind = AST_EXPR_SOME;
    new_some->loc = expr->loc;

    // Attempt to cast the inner expression
    AstNode *casted_value = maybe_insert_cast(expr->data.some_expr.value,
                                              expr_type->data.optional.base,
                                              target_type->data.optional.base);

    if (!casted_value) {
      return NULL; // Cast failed
    }

    new_some->data.some_expr.value = casted_value;
    new_some->resolved_type = target_type;
    return new_some;
  }

  // No valid conversion
  return NULL;
}

bool is_valid_cast(Type *from, Type *to) {
  // Numeric types can convert to each other
  if (type_is_numeric(from) && type_is_numeric(to)) {
    return true;
  }

  // Pointer to pointer
  if (from->kind == TYPE_POINTER && to->kind == TYPE_POINTER) {
    return true;
  }

  // Int <-> Pointer (for FFI)
  if (type_is_int(from) && to->kind == TYPE_POINTER) {
    return true;
  }
  if (from->kind == TYPE_POINTER && type_is_int(to)) {
    return true;
  }

  // char <-> integral
  if (type_is_integral(from) && to->kind == TYPE_CHAR) {
    return true;
  }
  if (from->kind == TYPE_CHAR && type_is_integral(to)) {
    return true;
  }

  // str (which is const char *) -> *void
  if (from == type_string && to->kind == TYPE_POINTER &&
      to->data.ptr.base == type_void) {
    return true;
  }

  // enum -> integral
  if (from->kind == TYPE_ENUM && type_is_integral(to)) {
    return true;
  }

  // *char to str (which is const char *)
  if (from->kind == TYPE_POINTER && from->data.ptr.base == type_char &&
      to == type_string) {
    return true;
  }

  return false;
}

static bool is_nil_type(Type *type) {
  return type->kind == TYPE_POINTER && type->data.ptr.base->kind == TYPE_VOID;
}

static bool is_constant_known(AstNode *node) {
  // Enum values are always constant
  if (node->resolved_type->kind == TYPE_ENUM) {
    return true;
  }

  switch (node->kind) {
  // All simple literals are constant
  case AST_EXPR_LITERAL_BOOL:
  case AST_EXPR_LITERAL_CHAR:
  case AST_EXPR_LITERAL_FLOAT:
  case AST_EXPR_LITERAL_INT:
  case AST_EXPR_LITERAL_NIL:
  case AST_EXPR_LITERAL_STRING:
    return true;

  default:
    break;
  }

  return false;
}

static void check_switch_is_exhaustive(AstNode *node, Type *switch_type) {
  assert(node->kind == AST_STMT_SWITCH);
  // Default case will make it pass exhaustive check
  if (node->data.switch_stmt.default_case) {
    return;
  }

  // A lot of types are not feasible to check exhaustiveness for one reason or
  // another.
  // - strings
  // - ints (larger than 8 bit)
  // - floats

  if (switch_type->kind == TYPE_U8 || switch_type->kind == TYPE_I8) {
    const int size = 256;

    Arena temp_arena;
    arena_init(&temp_arena, size);

    bool *covered = arena_alloc(&temp_arena, size);

    for (size_t i = 0; i < node->data.switch_stmt.case_count; i++) {
      AstNode *_case =
          node->data.switch_stmt.cases[i]->data.case_stmt.condition;

      if (_case->kind == AST_EXPR_LITERAL_INT) {
        long long value = _case->data.int_lit.value;

        if (switch_type->kind == TYPE_U8) {
          if (value < 0 || value >= size) {
            continue;
          }

          // Check u8
          unsigned char b = value;
          if (covered[b]) {
            checker_error(
                node->loc,
                "Switch case %d has already covered its condition in a "
                "previous case.",
                i + 1);
          }

          covered[b] = true;
        } else {
          // Check i8
          if (value < -128 || value >= 128) {
            continue;
          }

          char b = value;
          if (covered[b + 128]) {
            checker_error(
                node->loc,
                "Switch case %d has already covered its condition in a "
                "previous case.",
                i + 1);
          }

          // NOTE: Reusing array which is 0-255 so if b was below zero, like
          // -128, we need to offset it
          covered[b + 128] = true;
        }
      }
    }

    size_t missing_items = 0;

    // Check for uncovered values
    for (int i = 0; i < size; i++) {
      // This would not be ideal to log all cases so we just count it
      if (!covered[i]) {
        missing_items++;
      }
    }

    if (missing_items > 0) {
      checker_error(node->loc,
                    "Switch is non-exhaustive and is missing %d values(s) of "
                    "type %s. Use \"else\" "
                    "if you need a default branch.",
                    missing_items, switch_type->canonical_name);
    }

    arena_free(&temp_arena);
  }

  // Check exhaustiveness of enums
  if (switch_type->kind == TYPE_ENUM) {
    size_t variant_count = switch_type->data.enum_data.variant_count;

    Arena temp_arena;
    arena_init(&temp_arena, variant_count);

    bool *covered = arena_alloc(&temp_arena, variant_count);

    // Mark covered values
    for (size_t i = 0; i < node->data.switch_stmt.case_count; i++) {
      AstNode *_case =
          node->data.switch_stmt.cases[i]->data.case_stmt.condition;

      if (_case->kind == AST_EXPR_MEMBER) {
        const char *variant_name = _case->data.member_expr.member;

        // Find which enum value this is
        for (size_t j = 0; j < variant_count; j++) {
          if (strcmp(variant_name,
                     switch_type->data.enum_data.variant_names[j]) == 0) {
            if (covered[j]) {
              if (covered[j]) {
                checker_error(node->loc,
                              "Switch case %d has already covered the variant "
                              "\"%s\" in a "
                              "previous case.",
                              i + 1, variant_name);
              }
            }

            covered[j] = true;
            break;
          }
        }
      }
    }

    size_t missing_items = 0;

    // Check for uncovered values
    for (size_t i = 0; i < variant_count; i++) {
      if (!covered[i]) {
        missing_items++;
        checker_error(node->loc,
                      "Switch not exhaustive: missing case for '%s.%s'",
                      switch_type->canonical_name,
                      switch_type->data.enum_data.variant_names[i]);
      }
    }

    if (missing_items > 0) {
      checker_error(node->loc,
                    "Switch is non-exhaustive and is missing %d variant(s) of "
                    "type %s. Use \"else\" "
                    "if you need a default branch.",
                    missing_items, switch_type->canonical_name);
    }

    arena_free(&temp_arena);
  }

  checker_error(node->loc,
                "Switch is non-exhaustive. If you need a default branch.");
}

Type *check_expression(AstNode *expr) {
  if (!expr) {
    return NULL;
  }

  Location loc = expr->loc;

  switch (expr->kind) {
  case AST_EXPR_LITERAL_NIL: {
    Type *void_ptr = type_create_pointer(type_void, true, loc);
    expr->resolved_type = void_ptr;
    return void_ptr;
  }

  case AST_EXPR_LITERAL_INT:
    expr->resolved_type = type_int;
    return type_int;

  case AST_EXPR_LITERAL_FLOAT:
    expr->resolved_type = type_f32;
    return type_f32;

  case AST_EXPR_LITERAL_STRING:
    expr->resolved_type = type_string;
    return type_string;

  case AST_EXPR_LITERAL_CHAR:
    expr->resolved_type = type_char;
    return type_char;

  case AST_EXPR_LITERAL_BOOL:
    expr->resolved_type = type_bool;
    return type_bool;

  case AST_EXPR_GROUPED_EXPR: {
    Type *resolved_type = check_expression(expr->data.grouped_expr.inner_expr);

    if (!resolved_type) {
      return NULL;
    }

    expr->resolved_type = resolved_type;
    return resolved_type;
  }

  case AST_EXPR_SIZEOF: {
    AstNode *type_expr = expr->data.sizeof_expr.type_expr;

    // Resolve the type expression to get the actual Type*
    Type *type = resolve_type_expression(type_expr);
    if (!type) {
      checker_error(expr->loc, "Invalid type in sizeof");
      return NULL;
    }

    if (type->kind == TYPE_OPAQUE) {
      checker_error(expr->loc, "Cannot take sizeof opaque type '%s'",
                    type->canonical_name);
      return NULL;
    }

    // Store the resolved Type for codegen
    expr->data.sizeof_expr.type_expr->resolved_type = type;

    // sizeof always returns int
    expr->resolved_type = type_int;
    return type_usize;
  }

  case AST_EXPR_LITERAL_NONE:
    expr->resolved_type = type_none;
    return type_none;

  case AST_EXPR_SOME: {
    AstNode *value = expr->data.some_expr.value;

    // Typecheck value
    Type *val_ty = check_expression(value);

    if (!val_ty) {
      return NULL;
    }

    Type *optional =
        type_create_optional(val_ty, !checker_state.in_type_resolution, loc);
    expr->resolved_type = optional;
    return optional;
  }

  case AST_EXPR_FORCE_UNWRAP: {
    AstNode *value = expr->data.force_unwrap.operand;

    // Typecheck value
    Type *val_ty = check_expression(value);

    if (!val_ty) {
      return NULL;
    }

    if (val_ty->kind != TYPE_OPTIONAL) {
      checker_error(
          expr->loc,
          "'%s' is not an optional type and cannot be force unwrapped.",
          type_name(val_ty));
      return NULL;
    }

    expr->resolved_type = val_ty->data.optional.base;
    return val_ty->data.optional.base;
  }

  case AST_EXPR_POSTFIX_INC: {
    AstNode *operand = expr->data.postfix_inc.operand;
    Type *operand_ty = check_expression(operand);

    if (!operand_ty) {
      return NULL;
    }

    // Check if operand is an lvalue
    if (!is_lvalue(operand)) {
      checker_error(expr->loc, "operand of '++' must be an lvalue");
      return NULL;
    }

    // Only works on integer types
    if (!type_is_integral(operand_ty)) {
      checker_error(expr->loc, "'++' requires integral type, got '%s'",
                    type_name(operand_ty));
      return NULL;
    }

    expr->resolved_type = operand_ty;
    return operand_ty;
  }

  case AST_EXPR_IDENTIFIER: {
    char *name = expr->data.ident.name;
    Symbol *sym = scope_lookup(current_scope, name, expr->loc.file);
    if (!sym) {
      checker_error(expr->loc, "undefined name '%s'", name);
      return NULL;
    }

    if (strcmp(name, sym->name)) {
      // The name got qualified, so we should update the reference to it
      expr->data.ident.qualified_name = sym->name;
    }

    // Types are not values
    if (sym->kind == SYMBOL_TYPE && sym->type->kind != TYPE_ENUM) {
      checker_error(expr->loc, "'%s' is a type, not a value", name);
      return NULL;
    }

    // Only variables, constants and functions can be used as values
    if (sym->kind != SYMBOL_VARIABLE && sym->kind != SYMBOL_CONSTANT &&
        sym->kind != SYMBOL_FUNCTION &&
        (sym->kind == SYMBOL_TYPE && sym->type->kind != TYPE_ENUM)) {
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
      return NULL; // Error already reported
    }

    BinaryOp op = expr->data.binop.op;

    // Arithmetic: +, -, *, /
    if (op == BINOP_ADD || op == BINOP_SUB || op == BINOP_MUL ||
        op == BINOP_DIV) {
      // Handle Pointer arithmetic
      if (op == BINOP_ADD) {
        if (left->kind == TYPE_POINTER && type_is_numeric(right)) {
          expr->resolved_type = left;
          return left;
        } else if (type_is_numeric(left) && right->kind == TYPE_POINTER) {
          expr->resolved_type = right;
          return right;
        }
      }
      if (op == BINOP_SUB) {
        if (left->kind == TYPE_POINTER && type_is_numeric(right)) {
          expr->resolved_type = left;
          return left;
        }
      }

      // Otherwise, perform normal checks
      if (!type_is_numeric(left) || !type_is_numeric(right)) {
        checker_error(expr->loc,
                      "arithmetic operation requires numeric operands");
        return NULL;
      }
      // Handle type promotion: int + float -> float,
      // int + sized_int -> sized_int
      if (!type_equals(left, right)) {
        if (type_is_int(left) && type_is_floating(right)) {
          expr->data.binop.left =
              maybe_insert_cast(expr->data.binop.left, left, right);
          left = right; // Now both float
        } else if (type_is_floating(left) && type_is_int(right)) {
          expr->data.binop.right =
              maybe_insert_cast(expr->data.binop.right, right, left);
          right = left;
        } else if (left->kind == TYPE_INT &&
                   (right->kind == TYPE_U8 || right->kind == TYPE_U16 ||
                    right->kind == TYPE_U32 || right->kind == TYPE_U64 ||
                    right->kind == TYPE_USIZE || right->kind == TYPE_I8 ||
                    right->kind == TYPE_I16 || right->kind == TYPE_I32 ||
                    right->kind == TYPE_I64 || right->kind == TYPE_ISIZE)) {
          // Promote int to sized integer type
          expr->data.binop.left =
              maybe_insert_cast(expr->data.binop.left, left, right);
          left = right;
        } else if (right->kind == TYPE_INT &&
                   (left->kind == TYPE_U8 || left->kind == TYPE_U16 ||
                    left->kind == TYPE_U32 || left->kind == TYPE_U64 ||
                    left->kind == TYPE_USIZE || left->kind == TYPE_I8 ||
                    left->kind == TYPE_I16 || left->kind == TYPE_I32 ||
                    left->kind == TYPE_I64 || left->kind == TYPE_ISIZE)) {
          // Promote int to sized integer type
          expr->data.binop.right =
              maybe_insert_cast(expr->data.binop.right, right, left);
          right = left;
        } else {
          checker_error(expr->loc, "incompatible types in binary operation");
          return NULL;
        }
      }
      expr->resolved_type = left;
      return left; // Result type is unified
    }

    // Comparison: <, >, <=, >=
    if (op == BINOP_LT || op == BINOP_GT || op == BINOP_LE || op == BINOP_GE) {
      if (!type_is_ord(left) || !type_is_ord(right)) {
        checker_error(expr->loc,
                      "comparison requires numeric or enum operands");
        return NULL;
      }
      // Handle type promotion for consistency
      if (!type_equals(left, right)) {
        if (type_is_int(left) && type_is_floating(right)) {
          expr->data.binop.left =
              maybe_insert_cast(expr->data.binop.left, left, right);
          left = right;
        } else if (type_is_floating(left) && type_is_int(right)) {
          expr->data.binop.right =
              maybe_insert_cast(expr->data.binop.right, right, left);
          right = left;
        } else if (left->kind == TYPE_INT &&
                   (right->kind == TYPE_U8 || right->kind == TYPE_U16 ||
                    right->kind == TYPE_U32 || right->kind == TYPE_U64 ||
                    right->kind == TYPE_USIZE || right->kind == TYPE_I8 ||
                    right->kind == TYPE_I16 || right->kind == TYPE_I32 ||
                    right->kind == TYPE_I64 || right->kind == TYPE_ISIZE)) {
          // Promote int to sized integer type
          expr->data.binop.left =
              maybe_insert_cast(expr->data.binop.left, left, right);
          left = right;
        } else if (right->kind == TYPE_INT &&
                   (left->kind == TYPE_U8 || left->kind == TYPE_U16 ||
                    left->kind == TYPE_U32 || left->kind == TYPE_U64 ||
                    left->kind == TYPE_USIZE || left->kind == TYPE_I8 ||
                    left->kind == TYPE_I16 || left->kind == TYPE_I32 ||
                    left->kind == TYPE_I64 || left->kind == TYPE_ISIZE)) {
          // Promote int to sized integer type
          expr->data.binop.right =
              maybe_insert_cast(expr->data.binop.right, right, left);
          right = left;
        } else {
          checker_error(expr->loc, "incompatible types in comparison");
          return NULL;
        }
      }
      expr->resolved_type = type_bool;
      return type_bool;
    }

    // Equality: ==, !=
    if (op == BINOP_EQ || op == BINOP_NE) {
      if (!type_is_comparable(left) || !type_is_comparable(right)) {
        checker_error(expr->loc,
                      "equality comparison not supported for this type");
        return NULL;
      }

      // Allow comparison if one operand is nil (void*) and the other is a
      // pointer
      if ((is_nil_type(left) && right->kind == TYPE_POINTER) ||
          (is_nil_type(right) && left->kind == TYPE_POINTER)) {
        expr->resolved_type = type_bool;
        return type_bool;
      } else if (left->kind == TYPE_INT &&
                 (right->kind == TYPE_U8 || right->kind == TYPE_U16 ||
                  right->kind == TYPE_U32 || right->kind == TYPE_U64 ||
                  right->kind == TYPE_USIZE || right->kind == TYPE_I8 ||
                  right->kind == TYPE_I16 || right->kind == TYPE_I32 ||
                  right->kind == TYPE_I64 || right->kind == TYPE_ISIZE)) {
        // Promote int to sized integer type
        expr->data.binop.left =
            maybe_insert_cast(expr->data.binop.left, left, right);
        left = right;
      } else if (right->kind == TYPE_INT &&
                 (left->kind == TYPE_U8 || left->kind == TYPE_U16 ||
                  left->kind == TYPE_U32 || left->kind == TYPE_U64 ||
                  left->kind == TYPE_USIZE || left->kind == TYPE_I8 ||
                  left->kind == TYPE_I16 || left->kind == TYPE_I32 ||
                  left->kind == TYPE_I64 || left->kind == TYPE_ISIZE)) {
        // Promote int to sized integer type
        expr->data.binop.right =
            maybe_insert_cast(expr->data.binop.right, right, left);
        right = left;
      }

      if (!type_equals(left, right)) {
        checker_error(expr->loc, "type mismatch in equality check");
        return NULL;
      }
      expr->resolved_type = type_bool;
      return type_bool;
    }

    // Bitwise: &, |, ^, <<, >>
    if (op == BINOP_BIT_AND || op == BINOP_BIT_OR || op == BINOP_BIT_XOR ||
        op == BINOP_BIT_SHL || op == BINOP_BIT_SHR) {
      if (!type_is_integral(left) || !type_is_integral(right)) {
        checker_error(expr->loc, "bitwise operation requires integer operands");
        return NULL;
      }
      // Handle type promotion for sized integers
      if (!type_equals(left, right)) {
        if (type_is_integral(left) && type_is_integral(right)) {
          expr->data.binop.left =
              maybe_insert_cast(expr->data.binop.left, left, right);
          left = right;
        } else if (type_is_integral(right) && type_is_integral(left)) {
          expr->data.binop.right =
              maybe_insert_cast(expr->data.binop.right, right, left);
          right = left;
        } else {
          checker_error(expr->loc, "incompatible types in bitwise operation");
          return NULL;
        }
      }
      expr->resolved_type = left;
      return left;
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

    if (op == UNOP_BIT_NOT) {
      if (!type_is_int(operand)) {
        checker_error(expr->loc, "bitwise not requires integer operand");
        return NULL;
      }
      expr->resolved_type = operand;
      return operand;
    }

    if (op == UNOP_ADDR) {
      if (!is_lvalue(expr->data.unop.operand)) {
        checker_error(
            expr->loc,
            "cannot take address of expression without memory location");
        return NULL;
      }
      Type *ptr =
          type_create_pointer(operand, !checker_state.in_type_resolution, loc);
      expr->resolved_type = ptr;
      return ptr;
    }

    if (op == UNOP_DEREF) {
      // Dereference: *ptr returns T where ptr has type *T
      if (operand->kind != TYPE_POINTER) {
        checker_error(expr->loc, "dereference requires a pointer operand");
        return NULL;
      }

      if (operand->data.ptr.base->kind == TYPE_OPAQUE) {
        checker_error(expr->loc,
                      "Cannot dereference pointer to opaque type '%s'",
                      operand->data.ptr.base->canonical_name);
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

    Type *call_type = check_expression(func_expr);
    if (call_type == NULL) {
      return NULL;
    }

    // Verify it's actually a function
    if (call_type->kind != TYPE_FUNCTION) {
      checker_error(func_expr->loc, "'%s' is not a function",
                    type_name(call_type));
      return NULL;
    }

    Type *func_type = call_type;
    CallingConvention callee_conv = func_type->data.func.convention;
    size_t param_count = func_type->data.func.param_count;
    Type **param_types = func_type->data.func.param_types;
    Type *return_type = func_type->data.func.return_type;
    bool is_variadic = func_type->data.func.is_variadic;

    // TODO: Allow passing in a context manually constructed
    // C convention cannot call Pebble convention directly
    if (callee_conv == CALL_CONV_PEBBLE &&
        checker_state.current_convention == CALL_CONV_C) {
      checker_error(
          expr->loc,
          "cannot call Pebble convention function from C convention function");
    }

    expr->resolved_type = func_type;

    // Handle variadic functions
    if (is_variadic) {
      size_t required_params = param_count - 1;

      // Must have at least required_params arguments
      if (arg_count < required_params) {
        checker_error(
            expr->loc,
            "variadic function expects at least %zu arguments, got %zu",
            required_params, arg_count);
        return NULL;
      }

      // Check fixed parameters
      for (size_t i = 0; i < required_params; i++) {
        Type *arg_type = check_expression(args[i]);
        if (!arg_type)
          continue;

        AstNode *converted =
            maybe_insert_cast(args[i], arg_type, param_types[i]);
        if (!converted) {
          checker_error(args[i]->loc,
                        "argument %zu type mismatch: expected %s, got %s",
                        i + 1, type_name(param_types[i]), type_name(arg_type));
        } else {
          args[i] = converted;
        }
      }

      // Check variadic arguments match slice element type
      Type *variadic_slice_type = param_types[required_params];
      Type *elem_type = variadic_slice_type->data.slice.element;

      if (arg_count == required_params + 1) {
        // Single argument - could be slice or single element
        Type *arg_type = check_expression(args[required_params]);
        if (!arg_type)
          return NULL;

        if (arg_type->kind == TYPE_SLICE) {
          // Passing slice directly
          if (!type_equals(arg_type->data.slice.element, elem_type)) {
            checker_error(args[required_params]->loc,
                          "slice element type mismatch: expected %s, got %s",
                          type_name(elem_type),
                          type_name(arg_type->data.slice.element));
            return NULL;
          }
        } else {
          // Single element
          AstNode *converted =
              maybe_insert_cast(args[required_params], arg_type, elem_type);
          if (!converted) {
            checker_error(
                args[required_params]->loc,
                "variadic argument type mismatch: expected %s, got %s",
                type_name(elem_type), type_name(arg_type));
          } else {
            args[required_params] = converted;
          }
        }
      } else {
        // Many elements
        for (size_t i = required_params; i < arg_count; i++) {
          Type *arg_type = check_expression(args[i]);
          if (!arg_type)
            continue;

          AstNode *converted = maybe_insert_cast(args[i], arg_type, elem_type);
          if (!converted) {
            checker_error(
                args[i]->loc,
                "variadic argument %zu type mismatch: expected %s, got %s",
                i - required_params + 1, type_name(elem_type),
                type_name(arg_type));
          } else {
            args[i] = converted;
          }
        }
      }
    } else {
      // Non-variadic function

      // Check argument count
      if (arg_count != param_count) {
        checker_error(expr->loc, "function '%s' expects %zu arguments, got %zu",
                      type_name(expr->resolved_type), param_count, arg_count);
        return NULL;
      }

      // Check each argument type
      for (size_t i = 0; i < arg_count; i++) {
        Type *arg_type = check_expression(args[i]);
        if (!arg_type) {
          continue; // Error already reported
        }

        AstNode *converted =
            maybe_insert_cast(args[i], arg_type, param_types[i]);
        if (!converted) {
          checker_error(args[i]->loc,
                        "argument %zu type mismatch: expected %s, got %s",
                        i + 1, type_name(param_types[i]), type_name(arg_type));
        } else {
          args[i] = converted;
        }
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

    // Verify it's actually an array, slice, or string
    if (array_type->kind != TYPE_ARRAY && array_type->kind != TYPE_SLICE &&
        array_type->kind != TYPE_STRING) {
      checker_error(array_expr->loc,
                    "cannot index into non-array/slice/string type");
      return NULL;
    }

    // Check the index expression
    Type *index_type = check_expression(index_expr);
    if (!index_type) {
      return NULL;
    }

    // Verify index is an integer
    if (!type_is_int(index_type) && index_type->kind != TYPE_USIZE) {
      checker_error(index_expr->loc, "array index must be an integer or usize");
      return NULL;
    }

    // Return the element type
    if (array_type->kind == TYPE_ARRAY) {
      expr->resolved_type = array_type->data.array.element;
      return array_type->data.array.element;
    } else if (array_type->kind == TYPE_SLICE) {
      expr->resolved_type = array_type->data.slice.element;
      return array_type->data.slice.element;
    } else { // TYPE_STRING
      expr->resolved_type = type_char;
      return type_char;
    }
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

    // Can slice arrays, slices, or pointers
    if (array_type->kind != TYPE_ARRAY && array_type->kind != TYPE_SLICE &&
        array_type->kind != TYPE_POINTER) {
      checker_error(array_expr->loc,
                    "cannot slice non-array/slice/pointer type");
      return NULL;
    }

    // Get element type
    Type *element_type = NULL;
    if (array_type->kind == TYPE_ARRAY) {
      element_type = array_type->data.array.element;
    } else if (array_type->kind == TYPE_SLICE) {
      element_type = array_type->data.slice.element;
    } else if (array_type->kind == TYPE_POINTER) {
      element_type = array_type->data.ptr.base;
    }

    // Pointer slicing requires an end index
    if (array_type->kind == TYPE_POINTER && !end_expr) {
      checker_error(
          expr->loc,
          "pointer slicing requires an end index to determine slice lengthq");
      return NULL;
    }

    // Cannot slice to create opaque slice
    if (element_type->kind == TYPE_OPAQUE) {
      checker_error(
          expr->loc,
          "Cannot have slice of opaque type '%s' (use pointer instead)",
          element_type->canonical_name);
      return NULL;
    }

    // Check start index if present
    if (start_expr) {
      Type *start_type = check_expression(start_expr);
      if (!start_type) {
        return NULL;
      }
      if (!type_is_int(start_type)) {
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
      if (!type_is_int(end_type)) {
        checker_error(end_expr->loc, "slice end index must be an integer");
        return NULL;
      }
    }

    // Return slice type
    Type *slice =
        type_create_slice(element_type, !checker_state.in_type_resolution, loc);
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

    // Flag to track if we're dealing with a pointer
    Type *base_type = object_type;

    // Handle pointer dereferencing
    if (object_type->kind == TYPE_POINTER) {
      base_type = object_type->data.ptr.base;
      if (!base_type) {
        checker_error(expr->loc, "invalid pointer type with no base type");
        return NULL;
      }
    }

    // Handle tuple member access
    if (base_type->kind == TYPE_TUPLE) {
      char *endptr;
      long index = strtol(field_name, &endptr, 10);

      // Verify it's a valid number
      if (*endptr != '\0' || index < 0) {
        checker_error(expr->loc, "tuple field must be a non-negative integer");
        return NULL;
      }

      if ((size_t)index >= base_type->data.tuple.element_count) {
        checker_error(
            expr->loc,
            "tuple index %ld is out of bounds (tuple has %zu elements)", index,
            base_type->data.tuple.element_count);
        return NULL;
      }

      expr->resolved_type = base_type->data.tuple.element_types[index];
      return expr->resolved_type;
    }
    // Handle array or slice member access
    else if (base_type->kind == TYPE_ARRAY || base_type->kind == TYPE_SLICE) {
      if (strcmp(field_name, "len") == 0) {
        expr->resolved_type = type_u32;
        return type_u32;
      } else {
        const char *type_name =
            base_type->kind == TYPE_ARRAY ? "array" : "slice";
        checker_error(expr->loc, "%s has only 'len' field", type_name);
        return NULL;
      }
    } else if (base_type->kind == TYPE_OPTIONAL) {
      if (strcmp(field_name, "is_some") == 0) {
        expr->resolved_type = type_bool;
        return type_bool;
      }
      checker_error(expr->loc, "%s has only 'is_some' field",
                    type_name(base_type));
      return NULL;
    }
    // Handle struct member access
    else if (base_type->kind == TYPE_STRUCT) {
      size_t field_count = base_type->data.struct_data.field_count;
      char **field_names = base_type->data.struct_data.field_names;
      Type **field_types = base_type->data.struct_data.field_types;

      for (size_t i = 0; i < field_count; i++) {
        if (strcmp(field_names[i], field_name) == 0) {
          expr->resolved_type = field_types[i];
          return field_types[i];
        }
      }

      checker_error(expr->loc, "struct has no field named '%s'", field_name);
      return NULL;
    } else if (base_type->kind == TYPE_ENUM) {
      char **variant_names = base_type->data.enum_data.variant_names;
      size_t variant_count = base_type->data.enum_data.variant_count;

      for (size_t i = 0; i < variant_count; i++) {
        if (strcmp(variant_names[i], field_name) == 0) {
          expr->resolved_type = base_type;
          return base_type;
        }
      }

      checker_error(expr->loc, "enum has no variant named '%s'", field_name);
      return NULL;
    } else {
      checker_error(object_expr->loc,
                    "member access requires struct, enum, array, "
                    "slice, or pointer to one of these");
      return NULL;
    }
  }

  case AST_EXPR_MODULE_MEMBER: {
    AstNode *module_expr = expr->data.mod_member_expr.module;
    const char *member_name = expr->data.mod_member_expr.member;

    // We need to convert these 2 into a qualifed string to perform a
    // name lookup
    char *prefix = prepend(module_expr->data.ident.name, "__");
    char *qualified_name = prepend(prefix, member_name);

    Symbol *sym = scope_lookup_local(global_scope, qualified_name);
    if (!sym) {
      checker_error(expr->loc, "undefined name '%s::%s'",
                    module_expr->data.ident.name, member_name);
      return NULL;
    }

    // Types are not values
    if (sym->kind == SYMBOL_TYPE && sym->type->kind != TYPE_ENUM) {
      checker_error(expr->loc, "'%s::%s' is a type, not a value",
                    module_expr->data.ident.name, member_name);
      return NULL;
    }

    // Only variables, constants and functions can be used as values
    if (sym->kind != SYMBOL_VARIABLE && sym->kind != SYMBOL_CONSTANT &&
        sym->kind != SYMBOL_FUNCTION &&
        (sym->kind == SYMBOL_TYPE && sym->type->kind != TYPE_ENUM)) {
      checker_error(expr->loc, "'%s::%s' cannot be used as a value",
                    module_expr->data.ident.name, member_name);
      return NULL;
    }

    expr->resolved_type = sym->type;
    return sym->type;
  }

  case AST_EXPR_TUPLE: {
    // Type-check all tuple elements
    size_t element_count = expr->data.tuple_expr.element_count;
    AstNode **elements = expr->data.tuple_expr.elements;

    Type **element_types =
        arena_alloc(&long_lived, sizeof(Type *) * element_count);

    for (size_t i = 0; i < element_count; i++) {
      element_types[i] = check_expression(elements[i]);
      if (!element_types[i]) {
        return NULL; // Error already reported
      }
    }

    // Create and return tuple type
    Type *tuple = type_create_tuple(element_types, element_count,
                                    !checker_state.in_type_resolution, loc);
    expr->resolved_type = tuple;
    return tuple;
  }

  case AST_EXPR_STRUCT_LITERAL: {
    char *type_name = expr->data.struct_literal.type_name;
    const char *type_mod_name = expr->loc.file;
    char **field_names = expr->data.struct_literal.field_names;
    AstNode **field_values = expr->data.struct_literal.field_values;
    size_t field_count = expr->data.struct_literal.field_count;

    // Look up the type
    Symbol *type_sym = scope_lookup(current_scope, type_name, type_mod_name);
    if (!type_sym) {
      checker_error(expr->loc, "undefined type '%s'", type_name);
      return NULL;
    }

    if (strcmp(type_name, type_sym->name)) {
      // The name got qualified, so we should update the reference to it
      expr->data.struct_literal.qualified_type_name = type_sym->name;
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
      checker_error(
          expr->loc,
          "struct literal has %zu fields, but type '%s' has %zu fields",
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
            return NULL; // Error already reported
          }

          AstNode *converted_init =
              maybe_insert_cast(field_values[i], value_type, expected_types[j]);

          if (!converted_init) {
            checker_error(expr->loc, "field '%s' has initializer type mismatch",
                          field_names[i]);
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
      return NULL; // Error already reported
    }

    // Check all other elements have the same type
    for (size_t i = 1; i < element_count; i++) {
      Type *elem_type = check_expression(elements[i]);
      if (!elem_type) {
        return NULL; // Error already reported
      }

      AstNode *element =
          maybe_insert_cast(elements[i], elem_type, element_type);

      if (!element) {
        checker_error(
            elements[i]->loc,
            "array literal elements must all have the same type '%s' != '%s'",
            type_name(elem_type), type_name(element_type));
        return NULL;
      }

      elements[i] = element;
    }

    // Create and return array type with inferred element type and size
    Type *array = type_create_array(element_type, element_count,
                                    !checker_state.in_type_resolution, loc);
    expr->resolved_type = array;
    return array;
  }

  case AST_EXPR_ARRAY_REPEAT: {
    AstNode *value_expr = expr->data.array_repeat.value;
    size_t count = expr->data.array_repeat.count;

    // Type-check the value expression
    Type *value_type = check_expression(value_expr);
    if (!value_type) {
      return NULL; // Error already reported
    }

    // Create array type with the element type and count
    Type *array_type = type_create_array(
        value_type, count, !checker_state.in_type_resolution, loc);
    expr->resolved_type = array_type;
    return array_type;
  }

  case AST_EXPR_FUNCTION: {
    // Resolve parameter types and return type
    size_t param_count = expr->data.func_expr.param_count;
    FuncParam *params = expr->data.func_expr.params;
    AstNode *return_type_expr = expr->data.func_expr.return_type;

    Type **param_types = arena_alloc(&long_lived, sizeof(Type *) * param_count);
    int is_variadic = -1;

    for (size_t i = 0; i < param_count; i++) {
      if (is_variadic != -1) {
        checker_error(expr->loc,
                      "Parameter '%s' is marked as variadic but parameter '%s' "
                      "is already variadic",
                      params[i].name, params[is_variadic].name);
      }

      if (params[i].is_variadic) {
        is_variadic = (int)i;
      }
      param_types[i] = resolve_type_expression(params[i].type);
      if (!param_types[i]) {
        return NULL;
      }
    }

    Type *return_type = resolve_type_expression(return_type_expr);
    if (!return_type) {
      return NULL;
    }

    CallingConvention convention =
        convention_from_node(expr->data.func_expr.convention);

    // Add function as symbol
    Type *fn_type = type_create_function(
        param_types, param_count, return_type, is_variadic != -1,
        !checker_state.in_type_resolution, convention, loc);

    char *fn_symbol_name = next_anonymous_function_name();
    Symbol *symbol = symbol_create(fn_symbol_name, SYMBOL_ANON_FUNCTION, expr);
    symbol->type = fn_type;
    scope_add_symbol(anonymous_funcs, symbol);

    expr->data.func_expr.symbol = fn_symbol_name;

    // Will check function body with other functions
    return fn_type;
  }

  case AST_EXPR_IMPLICIT_CAST: {
    // The cast was already validated when inserted
    // Just return the target type
    expr->resolved_type = expr->data.implicit_cast.target_type;
    return expr->data.implicit_cast.target_type;
  }

  case AST_EXPR_EXPLICIT_CAST: {
    // Type-check the value being cast
    Type *value_type = check_expression(expr->data.explicit_cast.expr);
    if (!value_type)
      return NULL;

    // Resolve the target type
    Type *target_type =
        resolve_type_expression(expr->data.explicit_cast.target_type);
    if (!target_type) {
      checker_error(expr->loc, "Invalid cast target type");
      return NULL;
    }

    // Validate the cast is legal
    if (!is_valid_cast(value_type, target_type)) {
      checker_error(expr->loc, "Invalid cast from %s to %s",
                    type_name(value_type), type_name(target_type));
      return NULL;
    }

    // Result type is the target type
    expr->resolved_type = target_type;
    return target_type;
  }
  case AST_EXPR_CONTEXT: {
    if (checker_state.current_convention == CALL_CONV_C) {
      checker_error(
          expr->loc,
          "cannot use \"context\" in functions with C calling convention");
    }

    expr->resolved_type = type_context;
    return type_context;
  }

  default:
    checker_error(expr->loc,
                  "unsupported expression type in expression type checking %d",
                  expr->kind);
    return NULL;
  }
}

//=============================================================================
// PASS 4: CHECK FUNCTION BODIES
//=============================================================================

// Statement checking
// Returns false if statement does not return and true if it does
bool check_statement(AstNode *stmt, Type *expected_return_type) {
  if (!stmt) {
    return false;
  }

  switch (stmt->kind) {
  case AST_STMT_BREAK:
  case AST_STMT_CONTINUE: {
    if (!checker_state.in_loop) {
      checker_error(
          stmt->loc,
          "control flow jump statement can be used only inside a loop");
    }
    return false;
  }

  case AST_STMT_PRINT: {
    // FIXME: we can maybe setup a user function that can takeover in
    // freestanding cases
    if (compiler_opts.freestanding) {
      checker_error(stmt->loc, "cannot use print in freestanding mode");
    }

    for (size_t i = 0; i < stmt->data.print_stmt.expr_count; i++) {
      AstNode *expr = stmt->data.print_stmt.exprs[i];
      Type *expr_type = check_expression(expr);
      if (!expr_type) {
        return false; // Error already reported
      }
    }

    return false;
  }

  case AST_STMT_RETURN: {
    AstNode *expr = stmt->data.return_stmt.expr;

    // Check return expression
    Type *expr_type = check_expression(expr);
    if (!expr_type) {
      return true; // Error already reported
    }

    // Verify it matches expected return type (with implicit cast if needed)
    AstNode *converted =
        maybe_insert_cast(expr, expr_type, expected_return_type);
    if (!converted) {
      checker_error(stmt->loc, "return type mismatch");
    } else {
      stmt->data.return_stmt.expr =
          converted; // Replace with cast node if needed
    }

    // Cannot return with defer statements
    if (checker_state.in_defer) {
      checker_error(stmt->loc, "cannot return within defer statements");
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
    bool else_returns = else_branch
                            ? check_statement(else_branch, expected_return_type)
                            : false;

    // Only returns if BOTH branches return
    return then_returns && else_returns;
  }

  case AST_STMT_SWITCH: {
    AstNode *cond = stmt->data.switch_stmt.condition;
    AstNode **cases = stmt->data.switch_stmt.cases;
    AstNode *default_case = stmt->data.switch_stmt.default_case;

    bool had_error = false;

    // Check condition is numeric or string
    Type *cond_type = check_expression(cond);
    if (cond_type && cond_type->kind == TYPE_BOOL) {
      checker_error(cond->loc, "switch cases cannot be used with boolean "
                               "types. please use if statements instead.");
      had_error = true;
    }

    if (cond_type && !type_is_numeric(cond_type) &&
        cond_type->kind != TYPE_STRING && cond_type->kind != TYPE_CHAR &&
        cond_type->kind != TYPE_ENUM) {
      checker_error(cond->loc, "switch condition must be numeric (int or "
                               "float), char, enum or string");
      had_error = true;
    }

    for (size_t i = 0; i < stmt->data.switch_stmt.case_count; i++) {
      check_statement(cases[i], expected_return_type);
    }

    if (!had_error) {
      check_switch_is_exhaustive(stmt, cond->resolved_type);
    }

    bool else_returns =
        default_case ? check_statement(default_case, expected_return_type)
                     : false;

    return else_returns;
  }

  case AST_STMT_CASE: {
    AstNode *cond = stmt->data.case_stmt.condition;
    AstNode *switch_cond =
        stmt->data.case_stmt.switch_stmt->data.switch_stmt.condition;

    Type *cond_type = check_expression(cond);
    AstNode *casted_cond =
        maybe_insert_cast(cond, cond_type, switch_cond->resolved_type);

    if (casted_cond && switch_cond->resolved_type &&
        !type_equals(switch_cond->resolved_type, casted_cond->resolved_type)) {
      checker_error(cond->loc,
                    "switch case condition '%s' doesn't match switch "
                    "condition type '%s'",
                    cond->resolved_type->canonical_name,
                    switch_cond->resolved_type->canonical_name);
    }

    // Check that condition is constant
    if (!is_constant_known(cond)) {
      checker_error(cond->loc, "switch case condition must be a constant");
    }

    return check_statement(stmt->data.case_stmt.body, expected_return_type);
  }

  case AST_STMT_DEFER: {
    bool last_in_defer = checker_state.in_defer;
    checker_state.in_defer = true;

    check_statement(stmt->data.defer_stmt.stmt, expected_return_type);

    checker_state.in_defer = last_in_defer;
    return false;
  }

  case AST_STMT_WHILE: {
    AstNode *cond = stmt->data.while_stmt.cond;
    AstNode *body = stmt->data.while_stmt.body;

    // Check condition is boolean
    Type *cond_type = check_expression(cond);
    if (cond_type && cond_type->kind != TYPE_BOOL) {
      checker_error(cond->loc, "while condition must be boolean");
    }

    bool old_in_loop = checker_state.in_loop;
    checker_state.in_loop = true;
    // Check body
    check_statement(body, expected_return_type);
    checker_state.in_loop = old_in_loop;

    // Can't prove while always executes
    return false;
  }

  case AST_STMT_LOOP: {
    AstNode *start = stmt->data.loop_stmt.start;
    AstNode *end = stmt->data.loop_stmt.end;
    AstNode *body = stmt->data.loop_stmt.body;
    AstNode *iterator_name = stmt->data.loop_stmt.iterator_name;

    // Check start is an integer (can be variable or expression)
    Type *start_type = check_expression(start);
    if (start_type && !type_is_integral(start_type)) {
      checker_error(start->loc, "loop range start must be an integer");
    }

    // Check end is an integer (can be variable or expression)
    Type *end_type = check_expression(end);
    if (end_type && !type_is_integral(end_type)) {
      checker_error(end->loc, "loop range end must be an integer");
    }

    AstNode *new_end = maybe_insert_cast(end, end_type, start_type);
    if (!new_end) {
      checker_error(end->loc,
                    "loop range end doesn't match start type '%s' != '%s'",
                    type_name(end_type), type_name(start_type));
    }

    stmt->data.loop_stmt.end = new_end;

    // Create a scope for the loop body and register 'iter' as a constant
    Scope *loop_scope = scope_create(current_scope);
    scope_push(loop_scope);

    // Register loop iterator name or 'iter' as a const variable of type int
    const char *iter_name =
        iterator_name ? iterator_name->data.ident.name : "iter";
    Symbol *new_sym = symbol_create(iter_name, SYMBOL_CONSTANT, NULL);
    new_sym->type = type_int;
    scope_add_symbol(current_scope, new_sym);

    bool old_in_loop = checker_state.in_loop;
    checker_state.in_loop = true;
    // Check body
    check_statement(body, expected_return_type);
    checker_state.in_loop = old_in_loop;

    scope_pop();

    // Can't prove loop always executes (range could be empty)
    return false;
  }

  case AST_STMT_FOR: {
    AstNode *init = stmt->data.for_stmt.init;
    AstNode *cond = stmt->data.for_stmt.cond;
    AstNode *update = stmt->data.for_stmt.update;
    AstNode *body = stmt->data.for_stmt.body;

    // Create scope for the for loop
    Scope *for_scope = scope_create(current_scope);
    scope_push(for_scope);

    // Check init (assignment statement)
    check_statement(init, expected_return_type);

    // Check condition is boolean
    Type *cond_type = check_expression(cond);
    if (cond_type && cond_type->kind != TYPE_BOOL) {
      checker_error(cond->loc, "for loop condition must be boolean");
    }

    // Check update (assignment statement)
    check_statement(update, expected_return_type);

    bool old_in_loop = checker_state.in_loop;
    checker_state.in_loop = true;
    // Check body
    check_statement(body, expected_return_type);
    checker_state.in_loop = old_in_loop;

    scope_pop();

    // Can't prove for loop always executes
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
          break; // Stop checking, don't spam errors
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
    if (lhs->kind != AST_EXPR_IDENTIFIER && lhs->kind != AST_EXPR_MEMBER &&
        lhs->kind != AST_EXPR_INDEX &&
        !(lhs->kind == AST_EXPR_UNARY_OP && lhs->data.unop.op == UNOP_DEREF)) {
      checker_error(lhs->loc, "invalid assignment target");
      return false;
    }

    // If LHS is an identifier, check if it's a constant
    if (lhs->kind == AST_EXPR_IDENTIFIER) {
      Symbol *sym =
          scope_lookup(current_scope, lhs->data.ident.name, lhs->loc.file);
      if (sym && sym->kind == SYMBOL_CONSTANT) {
        checker_error(lhs->loc, "cannot assign to constant");
        return false;
      }

      if (strcmp(lhs->data.ident.name, sym->name)) {
        // The name got qualified, so we should update the reference to it
        lhs->data.ident.qualified_name = sym->name;
      }
    }

    // Check both sides
    Type *lhs_type = check_expression(lhs);
    Type *rhs_type = check_expression(rhs);

    // Check if assigning to an index of str (immutable)
    if (lhs->kind == AST_EXPR_INDEX) {
      AstNode *array_expr = lhs->data.index_expr.array;
      Type *array_type = array_expr->resolved_type;
      if (array_type && array_type == type_string) {
        checker_error(lhs->loc, "cannot assign to index of immutable string");
        return false;
      }
    }

    if (lhs_type && rhs_type) {
      AstNode *converted = maybe_insert_cast(rhs, rhs_type, lhs_type);
      if (!converted) {
        checker_error(stmt->loc, "assignment type mismatch");
      } else {
        stmt->data.assign_stmt.rhs =
            converted; // Replace with cast node if needed
      }
    }
    return false;
  }

  case AST_EXPR_POSTFIX_INC: {
    AstNode *lhs = stmt->data.postfix_inc.operand;
    return check_expression(lhs);
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
      checker_error(stmt->loc, "variable '%s' must have type or initializer",
                    name);
      return false;
    }

    Type *var_type = NULL;

    // Resolve type if provided
    if (type_expr) {
      var_type = resolve_type_expression(type_expr);
      if (!var_type)
        return false;
    }

    // Check initializer if provided
    if (init) {
      Type *init_type = check_expression(init);
      if (!init_type)
        return false;

      if (var_type) {
        // Type is specified, check compatibility and insert cast if needed
        AstNode *converted = maybe_insert_cast(init, init_type, var_type);
        if (!converted) {
          checker_error(init->loc, "initializer type mismatch '%s' != '%s'",
                        type_name(var_type), type_name(init_type));
          return false;
        }
        stmt->data.var_decl.init =
            converted; // Replace with cast node if needed
      } else {
        // Prevent using `none` without a specified type
        if (init_type->kind == TYPE_NONE) {
          checker_error(
              init->loc,
              "cannot infer type from none. use explicit type annotation");
          return false;
        }

        // No type specified, infer from initializer
        var_type = init_type;
      }
    }

    if (var_type->kind == TYPE_OPAQUE) {
      checker_error(
          stmt->loc,
          "Cannot declare variable of opaque type '%s' (use pointer instead)",
          var_type->canonical_name);
      return false;
    }

    // Check for duplicate in current scope
    Symbol *existing = scope_lookup_local(current_scope, name);
    if (existing) {
      checker_error(stmt->loc, "variable '%s' already declared in this scope",
                    name);
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
      if (!const_type)
        return false;
    }

    // Check value
    Type *value_type = check_expression(value);
    if (!value_type)
      return false;

    if (const_type) {
      // Type specified, check compatibility and insert cast if needed
      AstNode *converted = maybe_insert_cast(value, value_type, const_type);
      if (!converted) {
        checker_error(value->loc, "constant initializer type mismatch");
        return false;
      }
      stmt->data.const_decl.value =
          converted; // Replace with cast node if needed
    } else {
      // Prevent using `none` without a specified type
      if (value_type->kind == TYPE_NONE) {
        checker_error(
            value_type->loc,
            "cannot infer type from none. use explicit type annotation");
        return false;
      }

      // No type specified, infer from value
      const_type = value_type;
    }

    if (const_type->kind == TYPE_OPAQUE) {
      checker_error(
          stmt->loc,
          "Cannot declare constant of opaque type '%s' (use pointer instead)",
          const_type->canonical_name);
      return false;
    }

    // Check for duplicate
    Symbol *existing = scope_lookup_local(current_scope, name);
    if (existing) {
      checker_error(stmt->loc, "constant '%s' already declared in this scope",
                    name);
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

    checker_state.current_convention = func_type->data.func.convention;

    // Check the function body
    bool definitely_returns = check_statement(body, return_type);

    // If non-void, ensure all paths return
    if (return_type->kind != TYPE_VOID && !definitely_returns) {
      checker_error(decl->loc,
                    "function '%s' may not return a value on all paths",
                    sym->name);
    }

    // Pop back to global scope
    scope_pop();
  }

  return !checker_has_errors();
}

// Verify that the entry point exists and has the correct signature
// - If entry_point is "main": must exist with signature (void) -> int OR
// (int,
// []str) -> int
// - If entry_point is NOT "main": must exist with signature () -> void
// - If --no-main is set: skip verification entirely
bool verify_entry_point(void) {
  const char *entry_name = compiler_opts.entry_point;
  bool is_main = strcmp(entry_name, "main") == 0;

  // Look up the entry point function in global scope
  Symbol *entry_sym = scope_lookup_local(global_scope, entry_name);

  if (!entry_sym) {
    // This is fine
    if (!compiler_opts.has_main) {
      return true;
    }

    fprintf(stderr, "error: entry point '%s' not found\n", entry_name);
    return false;
  }

  if (entry_sym->kind != SYMBOL_FUNCTION) {
    fprintf(stderr, "error: entry point '%s' is not a function\n", entry_name);
    return false;
  }

  // Get the function's type
  Type *func_type = entry_sym->type;
  if (!func_type || func_type->kind != TYPE_FUNCTION) {
    fprintf(stderr, "error: entry point '%s' has invalid type\n", entry_name);
    return false;
  }

  Type *return_type = func_type->data.func.return_type;
  size_t param_count = func_type->data.func.param_count;
  Type **param_types = func_type->data.func.param_types;

  if (is_main) {
    // main must return int
    if (!type_is_int(return_type)) {
      fprintf(stderr, "error: main function must return int, not '%s'\n",
              return_type->canonical_name);
      return false;
    }

    if (func_type->data.func.convention != CALL_CONV_PEBBLE) {
      fprintf(stderr, "error: main must use pebble calling convention\n");
      return false;
    }

    // main can have 0, 1 or 2 parameters
    switch (param_count) {
    // 0 params: fn main() -> int
    case 0: {
      // void
      break;
    }

    // 1 param: fn main(argv []str) -> int
    case 1: {
      // Array of char*
      Type *argv_type = param_types[0];
      if (argv_type->kind != TYPE_SLICE) {
        fprintf(stderr,
                "error: main's second parameter must be []str (got '%s')\n",
                argv_type->canonical_name);
        return false;
      }

      Type *inner_type = argv_type->data.slice.element;
      if (inner_type->kind != TYPE_STRING) {
        fprintf(stderr,
                "error: main's second parameter must be []str (got %s)\n",
                argv_type->canonical_name);
        return false;
      }
      break;
    }

    // 2 params: fn main(argc int, argv []str) -> int
    case 2: {
      // Check first param is int (argc)
      if (!type_is_int(param_types[0])) {
        fprintf(stderr,
                "error: main's first parameter must be int (got '%s')\n",
                param_types[0]->canonical_name);
        return false;
      }

      // Check second param is *str (argv)
      // Array of char*
      Type *argv_type = param_types[1];
      if (argv_type->kind != TYPE_POINTER) {
        fprintf(stderr,
                "error: main's second parameter must be *str, not '%s'\n",
                argv_type->canonical_name);
        return false;
      }

      Type *inner_type = argv_type->data.ptr.base;
      if (inner_type->kind != TYPE_STRING) {
        fprintf(stderr,
                "error: main's second parameter must be *str (got %s)\n",
                argv_type->canonical_name);
        return false;
      }

      return true;
    }

    default: {
      fprintf(stderr,
              "error: main function must have 0 or 2 parameters, has %zu\n",
              param_count);
      return false;
    }
    }

  } else {
    // Non-main entry points must return void and take no parameters
    if (return_type->kind != TYPE_VOID) {
      fprintf(stderr, "error: entry point '%s' must return void, not '%s'\n",
              entry_name, return_type->canonical_name);
      return false;
    }

    if (param_count != 0) {
      fprintf(stderr,
              "error: entry point '%s' must take no parameters, has %zu\n",
              entry_name, param_count);
      return false;
    }

    if (func_type->data.func.convention != CALL_CONV_C) {
      fprintf(stderr, "error: main must use c calling convention\n");
      return false;
    }
  }

  // Check for no-main (libraries will do this)
  if (!compiler_opts.has_main) {
    fprintf(stderr,
            "error: cannot have entry point '%s' when compiling for a library "
            "or without an entry point\n",
            entry_name);
    return false;
  }

  return true;
}
