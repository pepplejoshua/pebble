#include "checker.h"
#include "alloc.h"
#include "ast.h"
#include "module.h"
#include "options.h"
#include "symbol.h"
#include "type.h"
// #include "wrapped_uthash.h"
#include "uthash.h"
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
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
  Module *current_module;
} CheckerState;

static CheckerState checker_state;

void checker_init(Module *main_mod) {
  checker_state.has_errors = false;
  checker_state.error_count = 0;
  checker_state.in_type_resolution = false;
  checker_state.in_loop = false;
  checker_state.anonymous_functions = 0;
  checker_state.current_module = main_mod;
  symbol_table_init(); // Create fresh global scope
}

void checker_set_current_module(Module *mod) {
  checker_state.current_module = mod;
  current_scope = mod->scope;
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

  Module *module = checker_state.current_module;
  Scope *target_scope = module ? module->scope : global_scope;

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
    qualified_name = decl->data.extern_func.qualified_name;
    kind = SYMBOL_EXTERN_FUNCTION;
    loc = decl->loc;
    break;
  case AST_DECL_EXTERN_TYPE:
    name = decl->data.extern_type.name;
    qualified_name = decl->data.extern_type.qualified_name;
    kind = SYMBOL_TYPE;
    is_opaque_type = true;
    loc = decl->loc;
    break;
  case AST_DECL_EXTERN_VARIABLE:
    name = decl->data.extern_var_decl.name;
    qualified_name = decl->data.extern_var_decl.qualified_name;
    kind = SYMBOL_EXTERN_VARIABLE;
    loc = decl->loc;
    break;
  case AST_DECL_EXTERN_CONSTANT:
    name = decl->data.extern_const_decl.name;
    qualified_name = decl->data.extern_const_decl.qualified_name;
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
  Symbol *existing = scope_lookup_local(target_scope, qualified_name);
  if (existing) {
    checker_error(decl->loc, "duplicate declaration of '%s'", name);
    checker_error(existing->decl->loc, "previous declaration was here");
    return;
  }

  // Create and add symbol
  Symbol *symbol = symbol_create(qualified_name, kind, decl);
  symbol->reg_name = name;
  if (kind == SYMBOL_VARIABLE || kind == SYMBOL_CONSTANT) {
    symbol->data.var.is_global = true;
  }

  AstNode *lib_name = NULL;
  switch (kind) {
  case SYMBOL_EXTERN_FUNCTION:
    lib_name = decl->data.extern_func.lib_name;
    break;

  case SYMBOL_EXTERN_VARIABLE:
    lib_name = decl->data.extern_var_decl.lib_name;
    break;

  case SYMBOL_EXTERN_CONSTANT:
    lib_name = decl->data.extern_const_decl.lib_name;
    break;

  default:
    break;
  }

  if (lib_name) {
    symbol->data.external.lib_name = lib_name->data.str_lit.value;
    // Add as library
    append_library_string(lib_name->data.str_lit.value);
  }

  if (is_opaque_type) {
    symbol->type = type_create(TYPE_OPAQUE, loc);
    symbol->type->declared_name = name;
    symbol->type->canonical_name = symbol->name;
  }

  symbol->reg_name = name;

  // NEW: Handle generic types
  if (kind == SYMBOL_TYPE && decl->kind == AST_DECL_TYPE &&
      decl->data.type_decl.type_params_count > 0) {
    // Create a marker type that says "this is generic"
    Type *generic_marker = type_create(TYPE_GENERIC_TYPE_DECL, loc);
    generic_marker->data.generic_decl.decl = decl;
    generic_marker->declared_name = name;
    symbol->type = generic_marker;
  }

  scope_add_symbol(target_scope, symbol);
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

  if (type->kind == TYPE_UNRESOLVED) {
    // Should not happen — but if it does, skip
    return false;
  }

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

  case TYPE_UNION:
  case TYPE_TAGGED_UNION:
    for (size_t i = 0; i < type->data.union_data.variant_count; i++) {
      if (canonicalize_type_internal(&type->data.union_data.variant_types[i],
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
  case TYPE_GENERIC_TYPE_DECL:
    // This should not occur, ever
    assert(false && "Attempt to canonicalize generic type.");
  case TYPE_GENERIC_FUNCTION:
    // This should not occur, ever
    assert(false && "Attempt to canonicalize generic function.");
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

  case TYPE_UNION:
  case TYPE_TAGGED_UNION: {
    // Unions are always nominal (use declared name)
    if (cycle_detected) {
      checker_error(
          type->loc,
          "recursive type '%s' has infinite size (use pointer for indirection)",
          type->declared_name);
      canonical_name = str_dup(type->declared_name);
    } else if (type->qualified_name) {
      canonical_name = str_dup(type->qualified_name);
    } else {
      // Anonymous union - build structural name
      size_t capacity = 256;
      canonical_name = arena_alloc(&long_lived, capacity);
      size_t offset = 0;

      offset += snprintf(canonical_name + offset, capacity - offset, "union");

      for (size_t i = 0; i < type->data.union_data.variant_count; i++) {
        char *field_name = type->data.union_data.variant_names[i];
        char *type_name =
            GET_COMPONENT_NAME(type->data.union_data.variant_types[i]);

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
static void canonicalize_types(Module *module) {
  // Walk all global symbols and canonicalize their types
  Symbol *sym, *tmp;
  HASH_ITER(hh, module->scope->symbols, sym, tmp) {
    if (sym->type) {
      // Type hasn't been canonicalized yet
      // Don't attempt to canonicalize opaque types
      if (sym->type->kind != TYPE_OPAQUE &&
          sym->type->kind != TYPE_GENERIC_TYPE_DECL)
        canonicalize_type(&(sym->type));

      // Keep type_table in sync (important for deduplication)
      if (sym->kind == SYMBOL_TYPE) {
        type_register(sym->name, sym->type);
      }
    }
  }
}

// Sub-pass 3a: Resolve type declarations
static void check_type_declarations(Module *module) {
  Symbol *sym, *tmp;

  // Build worklist of type declarations
  Symbol **worklist = NULL;
  size_t worklist_size = 0;
  size_t worklist_capacity = 0;

  HASH_ITER(hh, checker_state.current_module->scope->symbols, sym, tmp) {
    // Opaque types have their sym->type already set
    if (sym->kind == SYMBOL_TYPE && sym->type == NULL) {
      if (worklist_size >= worklist_capacity) {
        worklist_capacity = worklist_capacity == 0 ? 8 : worklist_capacity * 2;
        worklist = realloc(worklist, sizeof(Symbol *) * worklist_capacity);
      }
      worklist[worklist_size++] = sym;
    } else if (sym->kind == SYMBOL_TYPE && sym->type &&
               sym->type->kind == TYPE_GENERIC_TYPE_DECL) {
      // Skip generic types - they're resolved on-demand
      // Just ensure they're registered by name so we can look them up
      type_register(sym->name, sym->type);
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
            placeholder->kind == TYPE_TUPLE || placeholder->kind == TYPE_ENUM ||
            placeholder->kind == TYPE_UNION ||
            placeholder->kind == TYPE_TAGGED_UNION) {
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
  canonicalize_types(module);
}

static bool type_is_int(Type *type) {
  return type->kind == TYPE_INT || type->kind == TYPE_I32;
}

static bool type_is_integral(Type *type) {
  if (!type)
    return false;

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
  HASH_ITER(hh, checker_state.current_module->scope->symbols, sym, tmp) {
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

        AstNode *converted =
            maybe_insert_cast(value, inferred_type, explicit_type);
        if (!converted) {
          checker_error(value->loc,
                        "constant initializer type mismatch '%s' != '%s'",
                        type_name(inferred_type), type_name(explicit_type));
          continue;
        }

        decl->data.const_decl.value = converted;

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
  HASH_ITER(hh, checker_state.current_module->scope->symbols, sym, tmp) {
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
// Extract the signature checking logic for a single function
static bool check_function_signature(Symbol *sym) {
  AstNode *decl = sym->decl;

  // Handle generic functions - give them a special type
  if (decl->kind == AST_DECL_FUNCTION &&
      decl->data.func_decl.type_param_count > 0) {
    Type *generic_type = type_create(TYPE_GENERIC_FUNCTION, decl->loc);
    generic_type->data.generic_decl.decl = decl;
    sym->type = generic_type;
    return true;
  }

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
  } else {
    return true; // Not a function we handle
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
    return false; // Error already reported
  }

  // Create function type
  sym->type = type_create_function(param_types, param_count, return_type,
                                   is_variadic != -1, false, convention,
                                   sym->decl->loc);

  Type *receiver_struct_type = NULL;
  if (sym->is_method && decl->kind == AST_DECL_FUNCTION) {
    // First parameter must be named "self"
    if (param_count > 0 && strcmp(params[0].name, "self") == 0) {
      // Extract receiver type from self parameter
      Type *self_type = param_types[0];

      // Handle both value and pointer receivers
      if (self_type->kind == TYPE_POINTER) {
        receiver_struct_type = self_type->data.ptr.base;
      } else {
        receiver_struct_type = self_type;
      }

      if (receiver_struct_type->kind != TYPE_STRUCT) {
        checker_error(decl->loc,
                      "Method '%s' self parameter must be a struct type or "
                      "pointer to struct type",
                      decl->data.func_decl.name);
        receiver_struct_type = NULL;
      } else {
        // Validate that self type matches the containing struct
        if (!type_equals(receiver_struct_type, sym->containing_struct_type)) {
          checker_error(decl->loc,
                        "Method '%s' self parameter type '%s' doesn't match "
                        "containing struct '%s'",
                        decl->data.func_decl.name,
                        type_name(receiver_struct_type),
                        type_name(sym->containing_struct_type));
          receiver_struct_type = NULL;
        }
      }
    } else {
      // Associated function - first param is NOT "self" (or no params)
      // Store its type on the containing struct
      if (sym->containing_struct_type) {
        char *qualified_name = decl->data.func_decl.qualified_name;
        Type *struct_type = sym->containing_struct_type;

        for (size_t i = 0; i < struct_type->data.struct_data.method_count;
             i++) {
          if (strcmp(struct_type->data.struct_data.method_qualified_names[i],
                     qualified_name) == 0) {
            struct_type->data.struct_data.method_types[i] = sym->type;
            break;
          }
        }
      }
    }
  }

  if (receiver_struct_type) {
    char *qualified_name = decl->data.func_decl.qualified_name;
    for (size_t i = 0; i < receiver_struct_type->data.struct_data.method_count;
         i++) {
      if (strcmp(
              receiver_struct_type->data.struct_data.method_qualified_names[i],
              qualified_name) == 0) {
        receiver_struct_type->data.struct_data.method_types[i] = sym->type;
        break;
      }
    }
  }

  // Add parameters as symbols in the function scope
  bool no_error = true;
  if (sym->kind == SYMBOL_FUNCTION) {
    // Create function's local scope with global as parent
    Scope *func_scope = scope_create(checker_state.current_module->scope);
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
        no_error = false;
        continue;
      }
      scope_add_symbol(func_scope, param_sym);
    }
  }
  return no_error;
}

// Now refactor the original function to use it
static void check_function_signatures(void) {
  Symbol *sym, *tmp;

  // Iterate over all symbols in global scope
  HASH_ITER(hh, checker_state.current_module->scope->symbols, sym, tmp) {
    // Only process function declarations
    if (sym->kind != SYMBOL_FUNCTION && sym->kind != SYMBOL_EXTERN_FUNCTION) {
      continue;
    }

    check_function_signature(sym);
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
    Scope *func_scope = scope_create(checker_state.current_module->scope);
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

static void collect_methods(void) {
  Symbol *sym, *tmp;

  // Iterate over all type symbols
  HASH_ITER(hh, checker_state.current_module->scope->symbols, sym, tmp) {
    if (sym->kind != SYMBOL_TYPE) {
      continue;
    }

    // Only struct types have methods
    if (!sym->type || sym->type->kind != TYPE_STRUCT) {
      continue;
    }

    // Get the type declaration AST node
    AstNode *type_decl = sym->decl;
    if (type_decl->kind != AST_DECL_TYPE) {
      continue;
    }

    // Get the struct type expression
    AstNode *type_expr = type_decl->data.type_decl.type_expr;
    if (!type_expr || type_expr->kind != AST_TYPE_STRUCT) {
      continue;
    }

    // Extract methods from the struct
    size_t method_count = type_expr->data.type_struct.method_count;
    AstNode **methods = type_expr->data.type_struct.methods;

    if (method_count == 0) {
      continue;
    }

    // Separate generic and regular methods
    size_t regular_method_count = 0;
    size_t generic_method_count = 0;
    for (size_t i = 0; i < method_count; i++) {
      AstNode *method = methods[i];
      if (method->kind == AST_DECL_FUNCTION) {
        if (method->data.func_decl.type_param_count > 0) {
          generic_method_count++;
        } else {
          regular_method_count++;
        }
      }
    }

    // Allocate arrays for regular methods
    if (regular_method_count > 0) {
      sym->type->data.struct_data.method_qualified_names =
          arena_alloc(&long_lived, regular_method_count * sizeof(char *));
      sym->type->data.struct_data.method_reg_names =
          arena_alloc(&long_lived, regular_method_count * sizeof(char *));
      sym->type->data.struct_data.method_types =
          arena_alloc(&long_lived, regular_method_count * sizeof(Type *));
      sym->type->data.struct_data.method_count = regular_method_count;
    }

    // Allocate arrays for generic methods
    if (generic_method_count > 0) {
      sym->type->data.struct_data.generic_method_symbols =
          arena_alloc(&long_lived, generic_method_count * sizeof(Symbol *));
      sym->type->data.struct_data.generic_method_reg_names =
          arena_alloc(&long_lived, generic_method_count * sizeof(char *));
      sym->type->data.struct_data.generic_method_count = generic_method_count;
    }

    // Register each method as a function symbol
    size_t regular_idx = 0;
    size_t generic_idx = 0;
    for (size_t i = 0; i < method_count; i++) {
      AstNode *method = methods[i];

      if (method->kind != AST_DECL_FUNCTION) {
        continue;
      }

      // Methods should already have qualified names from module.c
      char *qualified_name = method->data.func_decl.full_qualified_name;
      char *reg_name = method->data.func_decl.name;
      if (!qualified_name) {
        checker_error(method->loc, "Method missing qualified name");
        continue;
      }

      // Check for duplicates
      Symbol *existing = scope_lookup_local(checker_state.current_module->scope,
                                            qualified_name);
      if (existing) {
        checker_error(method->loc, "Duplicate declaration '%s'",
                      method->data.func_decl.name);
        continue;
      }

      // Create symbol for the method
      Symbol *method_sym =
          symbol_create(qualified_name, SYMBOL_FUNCTION, method);
      method_sym->reg_name = method->data.func_decl.name;
      method_sym->is_method = true;
      method_sym->containing_struct_type = sym->type;

      scope_add_symbol(checker_state.current_module->scope, method_sym);

      // Store in appropriate array (generic vs regular)
      if (method->data.func_decl.type_param_count > 0) {
        // Generic method
        sym->type->data.struct_data.generic_method_symbols[generic_idx] =
            method_sym;
        sym->type->data.struct_data.generic_method_reg_names[generic_idx] =
            reg_name;
        generic_idx++;
      } else {
        // Regular method
        sym->type->data.struct_data.method_qualified_names[regular_idx] =
            qualified_name;
        sym->type->data.struct_data.method_reg_names[regular_idx] = reg_name;
        regular_idx++;
      }
    }
  }
}

// Main entry point for Pass 3
bool check_globals(Module *module) {
  check_type_declarations(module);
  collect_methods();
  check_global_constants();
  check_global_variables();
  check_function_signatures();
  return !checker_has_errors();
}

//=============================================================================
// TYPE RESOLUTION HELPERS
//=============================================================================
static Type *monomorphize_struct_type(AstNode *generic_struct_decl,
                                      AstNode **type_args,
                                      size_t type_arg_count, Location call_loc);

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
    AstNode **type_args = type_expr->data.type_named.type_args;
    size_t type_arg_count = type_expr->data.type_named.type_arg_count;

    if (!type) {
      // During type resolution, check if it's an unresolved type decl
      if (checker_state.in_type_resolution) { // ADD THIS CHECK
        Scope *mod_scope = checker_state.current_module->scope;
        Symbol *sym =
            scope_lookup(mod_scope, mod_scope, name, type_expr->loc.file);
        if (sym && sym->kind == SYMBOL_TYPE && sym->type == NULL) {
          // It's a forward reference - return NULL to retry later
          return NULL;
        }
      }

      // Otherwise, it's truly undefined
      checker_error(type_expr->loc, "undefined type '%s'", name);
      return NULL;
    }

    // Handle generic types
    if (type->kind == TYPE_GENERIC_TYPE_DECL) {
      if (type_arg_count == 0) {
        checker_error(type_expr->loc,
                      "generic type '%s' requires type arguments", name);
        return NULL;
      }

      // Monomorphize the generic struct
      type = monomorphize_struct_type(type->data.generic_decl.decl, type_args,
                                      type_arg_count, type_expr->loc);
    } else if (type_arg_count > 0) {
      // Non-generic type with type arguments
      checker_error(type_expr->loc,
                    "type '%s' is not generic but has %zu type arguments", name,
                    type_arg_count);
      return NULL;
    }
    return type;
  }

  case AST_TYPE_QUALIFIED_NAMED: {
    // Look up qualified named type in type table
    char *mod = type_expr->data.type_qualified_named.mod_name;
    char *mem = type_expr->data.type_qualified_named.mem_name;
    AstNode **type_args = type_expr->data.type_qualified_named.type_args;
    size_t type_arg_count = type_expr->data.type_qualified_named.type_arg_count;

    char *prefix = prepend(mod, "__");
    char *qualified_name = prepend(prefix, mem);

    Type *type = type_lookup(qualified_name, loc.file);

    if (!type) {
      // During type resolution, check if it's an unresolved type decl
      if (checker_state.in_type_resolution) {
        Symbol *sym = scope_lookup_local(checker_state.current_module->scope,
                                         qualified_name);
        if (sym && sym->kind == SYMBOL_TYPE && sym->type == NULL) {
          // It's a forward reference - return NULL to retry later
          return NULL;
        }
      }

      // Otherwise, it's truly undefined
      checker_error(type_expr->loc, "undefined type '%s::%s'", mod, mem);
      return NULL;
    }

    // Handle generic types in qualified names
    if (type->kind == TYPE_GENERIC_TYPE_DECL) {
      if (type_arg_count == 0) {
        checker_error(type_expr->loc,
                      "generic type '%s::%s' requires type arguments", mod,
                      mem);
        return NULL;
      }

      type = monomorphize_struct_type(type->data.generic_decl.decl, type_args,
                                      type_arg_count, type_expr->loc);
    } else if (type_arg_count > 0) {
      checker_error(type_expr->loc,
                    "type '%s::%s' is not generic but has %zu type arguments",
                    mod, mem, type_arg_count);
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
      checker_error(type_expr->loc, "Struct was declared without any members");
      return type_create_struct(NULL, NULL, field_count, false,
                                !checker_state.in_type_resolution, loc);
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
                              !checker_state.in_type_resolution, loc);
  }

  case AST_TYPE_UNION: {
    bool tagged = type_expr->data.type_union.is_tagged;
    size_t variant_count = type_expr->data.type_union.variant_count;
    if (variant_count == 0) {
      checker_error(type_expr->loc, "Union was declared without any members");
      return type_create_union(tagged, NULL, NULL, variant_count,
                               !checker_state.in_type_resolution, loc);
    }

    // Resolve all field types and create struct type
    char **variant_names = type_expr->data.type_union.variant_names;
    AstNode **variant_type_exprs = type_expr->data.type_union.variant_types;

    Type **variant_types =
        arena_alloc(&long_lived, sizeof(Type *) * variant_count);

    // Check duplicate field names
    typedef struct {
      char *name;
      UT_hash_handle hh;
    } variant_entry;
    variant_entry *seen = NULL;

    Arena temp_arena;
    arena_init(&temp_arena, 1024);

    for (size_t i = 0; i < variant_count; i++) {
      // Check field name collisions
      variant_entry *entry;
      HASH_FIND_STR(seen, variant_names[i], entry);
      if (entry) {
        checker_error(type_expr->loc, "Duplicate union member '%s'",
                      variant_names[i]);
      } else {
        entry = arena_alloc(&temp_arena, sizeof(variant_entry));
        entry->name = variant_names[i];
        HASH_ADD_KEYPTR(hh, seen, entry->name, strlen(entry->name), entry);
      }

      // Check types
      variant_types[i] = resolve_type_expression(variant_type_exprs[i]);
      if (!variant_types[i]) {
        HASH_CLEAR(hh, seen);
        arena_free(&temp_arena);
        return NULL;
      }

      if (variant_types[i]->kind == TYPE_OPAQUE) {
        checker_error(type_expr->loc,
                      "Cannot have field of opaque type '%s' in union (use "
                      "pointer instead)",
                      variant_types[i]->canonical_name);

        HASH_CLEAR(hh, seen);
        arena_free(&temp_arena);
        return NULL;
      }
    }

    HASH_CLEAR(hh, seen);
    arena_free(&temp_arena);

    return type_create_union(tagged, variant_names, variant_types,
                             variant_count, !checker_state.in_type_resolution,
                             loc);
  }

  case AST_TYPE_ENUM: {
    size_t variant_count = type_expr->data.type_enum.variant_count;
    if (variant_count == 0) {
      checker_error(type_expr->loc, "Enum was declared without any members");
      return type_create_enum(NULL, variant_count,
                              !checker_state.in_type_resolution, loc);
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

    return type_create_enum(variant_names, variant_count,
                            !checker_state.in_type_resolution, loc);
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
  case AST_EXPR_GROUPED_EXPR:
    return is_lvalue(expr->data.grouped_expr.inner_expr);
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
  if (expr->kind == AST_EXPR_LITERAL_INT && type_is_integral(target_type)) {
    AstNode *cast = arena_alloc(&long_lived, sizeof(AstNode));
    cast->kind = AST_EXPR_IMPLICIT_CAST;
    cast->loc = expr->loc;
    cast->data.implicit_cast.expr = expr;
    cast->data.implicit_cast.target_type = target_type;
    cast->resolved_type = target_type;
    return cast;
  }

  // void* <-> string
  if ((expr_type->kind == TYPE_POINTER && target_type->kind == TYPE_STRING) ||
      (expr_type->kind == TYPE_STRING && target_type->kind == TYPE_POINTER)) {
    if (expr_type->data.ptr.base == type_void ||
        target_type->data.ptr.base == type_void) {
      AstNode *cast = arena_alloc(&long_lived, sizeof(AstNode));
      cast->kind = AST_EXPR_IMPLICIT_CAST;
      cast->loc = expr->loc;
      cast->data.implicit_cast.expr = expr;
      cast->data.implicit_cast.target_type = target_type;
      return cast;
    }
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
    if (!needs_conversion &&
        strcmp(type_name(expr_type), type_name(target_type)) == 0) {
      return expr;
    }

    // Determine if we are dealing with a literal, so we can try to perform
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
    expr->resolved_type = target_type;
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
  } else if (expr_type->kind == TYPE_ENUM && target_type->kind == TYPE_ENUM) {
    // Infer partial member as enum
    if (expr->kind != AST_EXPR_PARTIAL_MEMBER) {
      return NULL;
    }

    int index = member_index_of_type(target_type,
                                     expr->data.partial_member_expr.member);
    if (index == -1) {
      return NULL;
    }

    expr->resolved_type = target_type;
    return expr;
  } else if (expr_type->kind == TYPE_STRUCT &&
             target_type->kind == TYPE_STRUCT) {
    // Check if structs have the same number of fields
    if (expr_type->data.struct_data.field_count !=
        target_type->data.struct_data.field_count) {
      return NULL;
    }

    if (expr->kind != AST_EXPR_STRUCT_LITERAL) {
      return NULL;
    }

    bool needs_conversion = false;
    for (size_t i = 0; i < expr_type->data.struct_data.field_count; i++) {
      // Names must match
      if (strcmp(expr->data.struct_literal.field_names[i],
                 target_type->data.struct_data.field_names[i]) != 0) {
        return NULL;
      }

      if (!type_equals(expr_type->data.struct_data.field_types[i],
                       target_type->data.struct_data.field_types[i])) {
        needs_conversion = true;
        break;
      }
    }

    // If all elements match exactly, no cast needed
    if (!needs_conversion) {
      expr->resolved_type = target_type;
      return expr;
    }

    // Create a new struct expression with recursively casted elements
    AstNode *new_struct = arena_alloc(&long_lived, sizeof(AstNode));
    new_struct->kind = AST_EXPR_STRUCT_LITERAL;
    new_struct->loc = expr->loc;
    new_struct->data.struct_literal.field_count =
        target_type->data.struct_data.field_count;
    new_struct->data.struct_literal.field_names =
        arena_alloc(&long_lived,
                    sizeof(char *) * target_type->data.struct_data.field_count);
    memcpy(new_struct->data.struct_literal.field_names,
           expr->data.struct_literal.field_names,
           sizeof(char *) * target_type->data.struct_data.field_count);

    new_struct->data.struct_literal.field_values =
        arena_alloc(&long_lived, sizeof(AstNode *) *
                                     target_type->data.struct_data.field_count);

    // Recursively cast each element
    for (size_t i = 0; i < expr_type->data.struct_data.field_count; i++) {
      AstNode *casted_elem =
          maybe_insert_cast(expr->data.struct_literal.field_values[i],
                            expr_type->data.struct_data.field_types[i],
                            target_type->data.struct_data.field_types[i]);

      if (!casted_elem) {
        return NULL; // Element conversion failed
      }

      new_struct->data.struct_literal.field_values[i] = casted_elem;
    }

    expr->resolved_type = target_type;
    new_struct->resolved_type = target_type;
    return new_struct;
  } else if (expr_type->kind == TYPE_STRUCT &&
             (target_type->kind == TYPE_UNION ||
              target_type->kind == TYPE_TAGGED_UNION)) {
    // Must have a single field for unions
    if (expr_type->data.struct_data.field_count != 1) {
      return NULL;
    }

    // Can only use literals
    if (expr->kind != AST_EXPR_STRUCT_LITERAL) {
      return NULL;
    }

    bool needs_conversion = false;
    char *member = expr_type->data.struct_data.field_names[0];

    int index = member_index_of_type(target_type, member);
    if (index == -1) {
      return NULL;
    }

    if (!type_equals(expr_type->data.struct_data.field_types[0],
                     target_type->data.struct_data.field_types[index])) {
      needs_conversion = true;
    }

    // If all elements match exactly, no cast needed
    if (!needs_conversion) {
      expr->resolved_type = target_type;
      return expr;
    }

    // Create a new struct expression with recursively casted elements
    AstNode *new_union = arena_alloc(&long_lived, sizeof(AstNode));
    new_union->kind = AST_EXPR_STRUCT_LITERAL;
    new_union->loc = expr->loc;
    new_union->data.struct_literal.field_count = 1;
    new_union->data.struct_literal.field_names =
        arena_alloc(&long_lived, sizeof(char *));
    memcpy(new_union->data.struct_literal.field_names,
           expr->data.struct_literal.field_names, sizeof(char *));

    new_union->data.struct_literal.field_values =
        arena_alloc(&long_lived, sizeof(AstNode *));

    AstNode *casted_elem =
        maybe_insert_cast(expr->data.struct_literal.field_values[0],
                          expr_type->data.struct_data.field_types[0],
                          target_type->data.union_data.variant_types[index]);

    if (!casted_elem) {
      return NULL; // Element conversion failed
    }

    new_union->data.struct_literal.field_values[0] = casted_elem;

    expr->resolved_type = target_type;
    new_union->resolved_type = target_type;
    return new_union;
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
  if (type_is_integral(from) && to->kind == TYPE_POINTER) {
    return true;
  }
  if (from->kind == TYPE_POINTER && type_is_integral(to)) {
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

  // *u8 <-> str
  if ((from->kind == TYPE_POINTER && from->data.ptr.base->kind == TYPE_U8 &&
       to->kind == TYPE_STRING) ||
      (from->kind == TYPE_STRING && to->kind == TYPE_POINTER &&
       to->data.ptr.base->kind == TYPE_U8)) {
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

  // struct <-> struct
  if (from->kind == TYPE_STRUCT && to->kind == TYPE_STRUCT) {
    // Can only coerce to structs >= in field count
    if (from->data.struct_data.field_count < to->data.struct_data.field_count) {
      return false;
    }

    // Check fields align
    for (size_t i = 0; i < to->data.struct_data.field_count; i++) {
      if (strcmp(from->data.struct_data.field_names[i],
                 to->data.struct_data.field_names[i]) != 0) {
        return false;
      }

      if (!type_equals(from->data.struct_data.field_types[i],
                       to->data.struct_data.field_types[i])) {
        return false;
      }
    }

    return true;
  }

  // tuple <-> tuple
  if (from->kind == TYPE_TUPLE && to->kind == TYPE_TUPLE) {
    // Can only coerce to tuples >= in field count
    if (from->data.tuple.element_count < to->data.tuple.element_count) {
      return false;
    }

    // Check elements align
    for (size_t i = 0; i < to->data.tuple.element_count; i++) {
      if (!type_equals(from->data.tuple.element_types[i],
                       to->data.tuple.element_types[i])) {
        return false;
      }
    }

    return true;
  }

  return false;
}

static bool is_nil_type(Type *type) {
  return type->kind == TYPE_POINTER && type->data.ptr.base->kind == TYPE_VOID;
}

static bool is_constant_known(AstNode *node) {
  // Enum values and union tags are always constant
  if (node->resolved_type->kind == TYPE_ENUM ||
      node->resolved_type->kind == TYPE_TAGGED_UNION ||
      (node->resolved_type->kind == TYPE_POINTER &&
       node->resolved_type->data.ptr.base->kind == TYPE_TAGGED_UNION)) {
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

  // A lot of types are not feasible to check exhaustiveness for one reason or
  // another.
  // - strings
  // - ints (larger than 8 bit)
  // - floats

  if (switch_type->kind == TYPE_U8 || switch_type->kind == TYPE_I8) {
    const int size = 256;
    bool covered[size];
    memset(covered, 0, size);

    for (size_t i = 0; i < node->data.switch_stmt.case_count; i++) {
      AstNode *switch_case = node->data.switch_stmt.cases[i];

      AstNode *cases[1 + switch_case->data.case_stmt.alt_condition_count];
      cases[0] = switch_case;
      for (size_t i = 0; i < switch_case->data.case_stmt.alt_condition_count;
           i++) {
        cases[1 + i] = switch_case->data.case_stmt.alt_conditions[i];
      }

      for (size_t j = 0;
           j < 1 + switch_case->data.case_stmt.alt_condition_count; j++) {
        AstNode *_case = cases[j]->data.case_stmt.condition;

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
                  _case->loc,
                  "Switch case %d has already covered its condition in a "
                  "previous case.",
                  i + 1);
            }

            covered[b] = true;
          } else {
            // Check i8
            if (value < -127 || value >= 128) {
              continue;
            }

            char b = value;
            if (covered[b + 128]) {
              checker_error(
                  _case->loc,
                  "Switch case %d has already covered its condition in a "
                  "previous case.",
                  i + 1);
            }

            // NOTE: Reusing array which is 0-255 so if b was below zero, like
            // -127, we need to offset it
            covered[b + 128] = true;
          }
        }
      }
    }

    // Default case will make it pass exhaustive check
    if (node->data.switch_stmt.default_case) {
      return;
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
  }

  // Check exhaustiveness of enums
  else if (switch_type->kind == TYPE_ENUM) {
    size_t variant_count = switch_type->data.enum_data.variant_count;

    bool covered[variant_count];
    memset(covered, 0, variant_count);

    // Mark covered values
    for (size_t i = 0; i < node->data.switch_stmt.case_count; i++) {
      AstNode *switch_case = node->data.switch_stmt.cases[i];

      AstNode *cases[1 + switch_case->data.case_stmt.alt_condition_count];
      cases[0] = switch_case;
      for (size_t i = 0; i < switch_case->data.case_stmt.alt_condition_count;
           i++) {
        cases[1 + i] = switch_case->data.case_stmt.alt_conditions[i];
      }

      for (size_t j = 0;
           j < 1 + switch_case->data.case_stmt.alt_condition_count; j++) {
        AstNode *_case = cases[j]->data.case_stmt.condition;

        char *variant_name = NULL;
        if (_case->kind == AST_EXPR_MEMBER) {
          variant_name = _case->data.member_expr.member;
        } else if (_case->kind == AST_EXPR_PARTIAL_MEMBER) {
          variant_name = _case->data.partial_member_expr.member;
        }

        if (variant_name) {
          // Find which enum value this is
          for (size_t j = 0; j < variant_count; j++) {
            if (strcmp(variant_name,
                       switch_type->data.enum_data.variant_names[j]) == 0) {
              if (covered[j]) {
                if (covered[j]) {
                  checker_error(
                      _case->loc,
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
    }

    // Default case will make it pass exhaustive check
    if (node->data.switch_stmt.default_case) {
      return;
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

    return;
  }
  // Tagged enum variants
  else if (switch_type->kind == TYPE_TAGGED_UNION ||
           (switch_type->kind == TYPE_POINTER &&
            switch_type->data.ptr.base->kind == TYPE_TAGGED_UNION)) {

    size_t variant_count =
        switch_type->kind == TYPE_TAGGED_UNION
            ? switch_type->data.union_data.variant_count
            : switch_type->data.ptr.base->data.union_data.variant_count;

    char **variant_names =
        switch_type->kind == TYPE_TAGGED_UNION
            ? switch_type->data.union_data.variant_names
            : switch_type->data.ptr.base->data.union_data.variant_names;

    bool covered[variant_count];
    memset(covered, 0, variant_count);

    // Mark covered values
    for (size_t i = 0; i < node->data.switch_stmt.case_count; i++) {
      AstNode *switch_case = node->data.switch_stmt.cases[i];

      AstNode *cases[1 + switch_case->data.case_stmt.alt_condition_count];
      cases[0] = switch_case;
      for (size_t i = 0; i < switch_case->data.case_stmt.alt_condition_count;
           i++) {
        cases[1 + i] = switch_case->data.case_stmt.alt_conditions[i];
      }

      for (size_t j = 0;
           j < 1 + switch_case->data.case_stmt.alt_condition_count; j++) {
        AstNode *_case = cases[j]->data.case_stmt.condition;

        if (_case->kind == AST_EXPR_PARTIAL_MEMBER) {
          const char *variant_name = _case->data.partial_member_expr.member;

          // Find which enum value this is
          for (size_t j = 0; j < variant_count; j++) {
            if (strcmp(variant_name, variant_names[j]) == 0) {
              if (covered[j]) {
                if (covered[j]) {
                  checker_error(
                      _case->loc,
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
    }

    // Default case will make it pass exhaustive check
    if (node->data.switch_stmt.default_case) {
      return;
    }

    size_t missing_items = 0;

    // Check for uncovered values
    for (size_t i = 0; i < variant_count; i++) {
      if (!covered[i]) {
        missing_items++;
        checker_error(node->loc, "Switch not exhaustive: missing case for '%s'",
                      variant_names[i]);
      }
    }

    if (missing_items > 0) {
      checker_error(node->loc,
                    "Switch is non-exhaustive and is missing %d variant(s) of "
                    "type %s. Use \"else\" "
                    "if you need a default branch.",
                    missing_items, switch_type->canonical_name);
    }

    return;
  }

  // Default case will make it pass exhaustive check
  if (node->data.switch_stmt.default_case) {
    return;
  }

  checker_error(
      node->loc,
      "Switch is non-exhaustive. Use \"else\" if you need a default branch.");
}

// Type parameter bindings for monomorphization
typedef struct {
  char *param_name;    // "T", "U", etc.
  Type *concrete_type; // int, bool, etc.
} TypeBinding;

typedef struct {
  TypeBinding *bindings;
  size_t count;
} TypeBindings;

// Check if a name is a type parameter
static bool is_type_parameter(const char *name, TypeBindings *bindings) {
  for (size_t i = 0; i < bindings->count; i++) {
    if (strcmp(name, bindings->bindings[i].param_name) == 0) {
      return true;
    }
  }
  return false;
}

static bool extract_generic_specialization(Type *concrete_type,
                                           Type ***out_type_args,
                                           size_t *out_count) {
  if (!concrete_type || concrete_type->kind != TYPE_STRUCT) {
    return false;
  }

  // Check if this struct stores generic type args
  if (concrete_type->generic_type_arg_count > 0 &&
      concrete_type->generic_type_args) {
    *out_type_args = concrete_type->generic_type_args;
    *out_count = concrete_type->generic_type_arg_count;
    return true;
  }

  return false;
}

// Match a type pattern (AST node) against a concrete type and extract bindings
// Returns true if match succeeds, false otherwise
static bool match_and_bind_type(AstNode *pattern, Type *concrete,
                                TypeBindings *bindings, Location loc) {
  if (!pattern || !concrete) {
    return false;
  }

  switch (pattern->kind) {
  case AST_TYPE_NAMED: {
    char *name = pattern->data.type_named.name;

    // Check if this is a type parameter
    if (is_type_parameter(name, bindings)) {
      // Find the binding
      for (size_t i = 0; i < bindings->count; i++) {
        if (strcmp(name, bindings->bindings[i].param_name) == 0) {
          if (bindings->bindings[i].concrete_type == NULL) {
            // First binding for this type parameter
            bindings->bindings[i].concrete_type = concrete;
            return true;
          } else {
            // Already bound - check consistency
            if (type_equals(bindings->bindings[i].concrete_type, concrete)) {
              return true;
            } else {
              checker_error(loc,
                            "type variable '%s' was previously inferred as "
                            "'%s', but got '%s'",
                            name,
                            type_name(bindings->bindings[i].concrete_type),
                            type_name(concrete));
              return false;
            }
          }
        }
      }
    }

    // Handle generic struct references with type arguments
    if (pattern->data.type_named.type_arg_count > 0) {
      if (concrete->kind != TYPE_STRUCT) {
        checker_error(loc, "type mismatch: expected struct type, got '%s'",
                      type_name(concrete));
        return false;
      }

      if (!concrete->declared_name ||
          strcmp(concrete->declared_name, name) != 0) {
        checker_error(loc, "type mismatch: expected '%s', got '%s'", name,
                      type_name(concrete));
        return false;
      }

      // Extract concrete type arguments from the monomorphized struct
      Type **concrete_type_args = NULL;
      size_t concrete_type_arg_count = 0;

      if (!extract_generic_specialization(concrete, &concrete_type_args,
                                          &concrete_type_arg_count)) {
        // Either not monomorphized or not a generic struct
        // This shouldn't happen if type checking is working
        checker_error(
            loc,
            "internal error: struct '%s' is not a valid generic specialization",
            name);
        return false;
      }

      size_t pattern_arg_count = pattern->data.type_named.type_arg_count;
      if (concrete_type_arg_count != pattern_arg_count) {
        checker_error(loc,
                      "generic type '%s' expects %zu type arguments, got %zu",
                      name, pattern_arg_count, concrete_type_arg_count);
        return false;
      }

      // Recursively match pattern args against concrete args
      for (size_t i = 0; i < pattern_arg_count; i++) {
        if (!match_and_bind_type(pattern->data.type_named.type_args[i],
                                 concrete_type_args[i], bindings, loc)) {
          return false;
        }
      }

      return true;
    }

    // Not a type parameter - just a regular named type
    // We'd need to resolve it and check equality, but for now skip
    return true;
  }

  case AST_TYPE_SLICE: {
    // Pattern: []T
    // Concrete can be: []SomeType OR [N]SomeType (implicit cast)

    if (concrete->kind == TYPE_SLICE) {
      // Direct match: []T with []int
      return match_and_bind_type(pattern->data.type_slice.element,
                                 concrete->data.slice.element, bindings, loc);
    } else if (concrete->kind == TYPE_ARRAY) {
      // Array to slice: []T with [N]int
      // Extract element type from array
      return match_and_bind_type(pattern->data.type_slice.element,
                                 concrete->data.array.element, bindings, loc);
    } else {
      checker_error(loc, "type mismatch: expected slice type, got '%s'",
                    type_name(concrete));
      return false;
    }
  }

  case AST_TYPE_POINTER: {
    // Pattern: *T, Concrete must be: *SomeType
    if (concrete->kind != TYPE_POINTER) {
      checker_error(loc, "type mismatch: expected pointer type, got '%s'",
                    type_name(concrete));
      return false;
    }

    // Recursively match base types
    return match_and_bind_type(pattern->data.type_pointer.base,
                               concrete->data.ptr.base, bindings, loc);
  }

  case AST_TYPE_OPTIONAL: {
    // Pattern: ?T, Concrete must be: ?SomeType
    if (concrete->kind != TYPE_OPTIONAL) {
      checker_error(loc, "type mismatch: expected optional type, got '%s'",
                    type_name(concrete));
      return false;
    }

    // Recursively match base types
    return match_and_bind_type(pattern->data.type_optional.base,
                               concrete->data.optional.base, bindings, loc);
  }

  case AST_TYPE_ARRAY: {
    // Pattern: [N]T, Concrete must be: [N]SomeType with same size
    if (concrete->kind != TYPE_ARRAY) {
      checker_error(loc, "type mismatch: expected array type, got '%s'",
                    type_name(concrete));
      return false;
    }

    if (pattern->data.type_array.size != concrete->data.array.size) {
      checker_error(loc, "array size mismatch: expected [%zu], got [%zu]",
                    pattern->data.type_array.size, concrete->data.array.size);
      return false;
    }

    // Recursively match element types
    return match_and_bind_type(pattern->data.type_array.element,
                               concrete->data.array.element, bindings, loc);
  }

  case AST_TYPE_FUNCTION: {
    // Pattern: fn(T1, T2) R, Concrete must be: fn(Type1, Type2) RetType
    if (concrete->kind != TYPE_FUNCTION) {
      checker_error(loc, "type mismatch: expected function type, got '%s'",
                    type_name(concrete));
      return false;
    }

    if (pattern->data.type_function.param_count !=
        concrete->data.func.param_count) {
      checker_error(loc,
                    "function parameter count mismatch: expected %zu, got %zu",
                    pattern->data.type_function.param_count,
                    concrete->data.func.param_count);
      return false;
    }

    // Match each parameter type
    for (size_t i = 0; i < pattern->data.type_function.param_count; i++) {
      if (!match_and_bind_type(pattern->data.type_function.param_types[i],
                               concrete->data.func.param_types[i], bindings,
                               loc)) {
        return false;
      }
    }

    // Match return type
    return match_and_bind_type(pattern->data.type_function.return_type,
                               concrete->data.func.return_type, bindings, loc);
  }

  case AST_TYPE_STRUCT: {
    // Pattern: struct { T1, T2 }, Concrete must be: struct { Type1, Type2 }
    if (concrete->kind != TYPE_STRUCT) {
      checker_error(loc, "type mismatch: expected struct type, got '%s'",
                    type_name(concrete));
      return false;
    }

    if (pattern->data.type_struct.field_count !=
        concrete->data.struct_data.field_count) {
      checker_error(loc, "struct element count mismatch: expected %zu, got %zu",
                    pattern->data.type_struct.field_count,
                    concrete->data.struct_data.field_count);
      return false;
    }

    for (size_t i = 0; i < pattern->data.type_struct.field_count; i++) {
      char *pattern_field_name = pattern->data.type_struct.field_names[i];
      char *concrete_field_name = concrete->data.struct_data.field_names[i];

      if (strcmp(pattern_field_name, concrete_field_name) != 0) {
        checker_error(
            loc,
            "struct field mismatch: expected field %zu to be %s, got field %s",
            i + 1, pattern_field_name, concrete_field_name);
        return false;
      }

      if (!match_and_bind_type(pattern->data.type_struct.field_types[i],
                               concrete->data.struct_data.field_types[i],
                               bindings, loc)) {
        return false;
      }
    }

    return true;
  }

  case AST_TYPE_TUPLE: {
    // Pattern: (T1, T2, T3), Concrete must be: (Type1, Type2, Type3)
    if (concrete->kind != TYPE_TUPLE) {
      checker_error(loc, "type mismatch: expected tuple type, got '%s'",
                    type_name(concrete));
      return false;
    }

    if (pattern->data.type_tuple.element_count !=
        concrete->data.tuple.element_count) {
      checker_error(loc, "tuple element count mismatch: expected %zu, got %zu",
                    pattern->data.type_tuple.element_count,
                    concrete->data.tuple.element_count);
      return false;
    }

    // Match each element type
    for (size_t i = 0; i < pattern->data.type_tuple.element_count; i++) {
      if (!match_and_bind_type(pattern->data.type_tuple.element_types[i],
                               concrete->data.tuple.element_types[i], bindings,
                               loc)) {
        return false;
      }
    }

    return true;
  }

  default:
    return false;
  }
}

static TypeBindings infer_type_arguments(AstNode *generic_func, AstNode **args,
                                         size_t arg_count, Location call_loc) {
  TypeBindings result = {0};

  size_t type_param_count = generic_func->data.func_decl.type_param_count;
  Token *type_params = generic_func->data.func_decl.type_params;
  FuncParam *params = generic_func->data.func_decl.params;
  size_t param_count = generic_func->data.func_decl.param_count;

  result.bindings =
      arena_alloc(&long_lived, sizeof(TypeBinding) * type_param_count);
  result.count = type_param_count;

  // Initialize all bindings to NULL
  for (size_t i = 0; i < type_param_count; i++) {
    result.bindings[i].param_name = type_params[i].lexeme;
    result.bindings[i].concrete_type = NULL;
  }

  // Check if function is variadic
  bool is_variadic = (param_count > 0 && params[param_count - 1].is_variadic);
  size_t required_param_count = is_variadic ? param_count - 1 : param_count;

  // Check argument count
  if (is_variadic) {
    if (arg_count < required_param_count) {
      checker_error(call_loc,
                    "variadic function expects at least %zu arguments, got %zu",
                    required_param_count, arg_count);
      return result;
    }
  } else {
    if (arg_count != param_count) {
      checker_error(call_loc, "function expects %zu arguments, got %zu",
                    param_count, arg_count);
      return result;
    }
  }

  // Match non-variadic parameters
  for (size_t j = 0; j < required_param_count; j++) {
    AstNode *param_type = params[j].type;

    // Get the concrete type of the argument
    Type *arg_type = check_expression(args[j]);
    if (!arg_type)
      continue;

    // Match the parameter type pattern with the argument's concrete type
    match_and_bind_type(param_type, arg_type, &result, call_loc);
  }

  // Match variadic parameters if present
  if (is_variadic && arg_count > required_param_count) {
    AstNode *variadic_type = params[required_param_count].type;

    // Extract element type from []T
    AstNode *element_type = NULL;
    if (variadic_type->kind == AST_TYPE_SLICE) {
      element_type = variadic_type->data.type_slice.element;
    } else {
      checker_error(call_loc, "variadic parameter must have slice type");
      return result;
    }

    if (arg_count == required_param_count + 1) {
      // Single variadic argument - could be slice or element
      Type *arg_type = check_expression(args[required_param_count]);
      if (arg_type) {
        if (arg_type->kind == TYPE_SLICE) {
          // Passing a slice directly - infer from its element type
          match_and_bind_type(element_type, arg_type->data.slice.element,
                              &result, call_loc);
        } else {
          // Single element - infer from the element itself
          match_and_bind_type(element_type, arg_type, &result, call_loc);
        }
      }
    } else {
      // Multiple variadic arguments - infer from each element
      for (size_t j = required_param_count; j < arg_count; j++) {
        Type *arg_type = check_expression(args[j]);
        if (!arg_type)
          continue;

        match_and_bind_type(element_type, arg_type, &result, call_loc);
      }
    }
  }

  return result;
}

// Generate mangled name for a monomorphized function
// Example: mangle_generic_name("add", [int, bool]) -> "add__int__bool"
static char *mangle_generic_name(const char *func_name, Type **concrete_types,
                                 size_t type_count) {
  // Start with function name
  size_t len = strlen(func_name) + 1;

  // Calculate total length needed
  for (size_t i = 0; i < type_count; i++) {
    Type *t = concrete_types[i];
    char *type_str =
        t->canonical_name ? t->canonical_name : compute_canonical_name(t);
    len += 2 + strlen(type_str); // "__" + type name
  }

  // Allocate and build the mangled name
  char *result = arena_alloc(&long_lived, len);
  strcpy(result, func_name);

  for (size_t i = 0; i < type_count; i++) {
    strcat(result, "__");
    Type *t = concrete_types[i];
    char *type_str =
        t->canonical_name ? t->canonical_name : compute_canonical_name(t);
    strcat(result, type_str);
  }

  return result;
}

// Substitute type parameters in type expressions (recursive)
static void substitute_type_params_in_type(AstNode *type_node,
                                           TypeBindings *bindings) {
  if (!type_node)
    return;

  switch (type_node->kind) {
  // Base case: Named type that might be a type parameter
  case AST_TYPE_NAMED: {
    char *name = type_node->data.type_named.name;

    // Check if this name matches a type parameter
    for (size_t i = 0; i < bindings->count; i++) {
      if (strcmp(name, bindings->bindings[i].param_name) == 0) {
        // Found a match! Replace with the concrete type's AST representation
        Type *concrete = bindings->bindings[i].concrete_type;
        AstNode *replacement = type_to_ast_node(concrete);

        // Replace this node's content with the replacement
        type_node->kind = replacement->kind;
        type_node->data = replacement->data;
        type_node->resolved_type = concrete;
        type_node->loc = replacement->loc;
        return;
      }
    }

    if (type_node->data.type_named.type_arg_count > 0) {
      for (size_t i = 0; i < type_node->data.type_named.type_arg_count; i++) {
        substitute_type_params_in_type(type_node->data.type_named.type_args[i],
                                       bindings);
      }
    }
    break;
  }

  // Recursive cases: Complex types
  case AST_TYPE_POINTER:
    substitute_type_params_in_type(type_node->data.type_pointer.base, bindings);
    break;

  case AST_TYPE_OPTIONAL:
    substitute_type_params_in_type(type_node->data.type_optional.base,
                                   bindings);
    break;

  case AST_TYPE_ARRAY:
    substitute_type_params_in_type(type_node->data.type_array.element,
                                   bindings);
    break;

  case AST_TYPE_SLICE:
    substitute_type_params_in_type(type_node->data.type_slice.element,
                                   bindings);
    break;

  case AST_TYPE_FUNCTION:
    substitute_type_params_in_type(type_node->data.type_function.return_type,
                                   bindings);
    for (size_t i = 0; i < type_node->data.type_function.param_count; i++) {
      substitute_type_params_in_type(
          type_node->data.type_function.param_types[i], bindings);
    }
    break;

  case AST_TYPE_TUPLE:
    for (size_t i = 0; i < type_node->data.type_tuple.element_count; i++) {
      substitute_type_params_in_type(
          type_node->data.type_tuple.element_types[i], bindings);
    }
    break;

  case AST_TYPE_STRUCT:
    for (size_t i = 0; i < type_node->data.type_struct.field_count; i++) {
      substitute_type_params_in_type(type_node->data.type_struct.field_types[i],
                                     bindings);
    }
    break;

  case AST_TYPE_UNION:
    for (size_t i = 0; i < type_node->data.type_union.variant_count; i++) {
      substitute_type_params_in_type(
          type_node->data.type_union.variant_types[i], bindings);
    }
    break;

  case AST_TYPE_QUALIFIED_NAMED:
    if (type_node->data.type_qualified_named.type_arg_count > 0) {
      for (size_t i = 0;
           i < type_node->data.type_qualified_named.type_arg_count; i++) {
        substitute_type_params_in_type(
            type_node->data.type_qualified_named.type_args[i], bindings);
      }
    }
    break;

  case AST_TYPE_ENUM:
    // These don't contain nested type parameters
    break;

  default:
    break;
  }
}

// Forward declaration for mutual recursion
static void substitute_type_params_in_expr(AstNode *expr,
                                           TypeBindings *bindings);

// Substitute type parameters in statements (recursive)
static void substitute_type_params_in_stmt(AstNode *stmt,
                                           TypeBindings *bindings) {
  if (!stmt)
    return;

  switch (stmt->kind) {
  case AST_DECL_VARIABLE:
    // var x T = ...;
    substitute_type_params_in_type(stmt->data.var_decl.type_expr, bindings);
    substitute_type_params_in_expr(stmt->data.var_decl.init, bindings);
    break;

  case AST_DECL_CONSTANT:
    // const x T = ...;
    substitute_type_params_in_type(stmt->data.const_decl.type_expr, bindings);
    substitute_type_params_in_expr(stmt->data.const_decl.value, bindings);
    break;

  case AST_STMT_RETURN:
    substitute_type_params_in_expr(stmt->data.return_stmt.expr, bindings);
    break;

  case AST_STMT_IF:
    substitute_type_params_in_expr(stmt->data.if_stmt.cond, bindings);
    substitute_type_params_in_stmt(stmt->data.if_stmt.then_branch, bindings);
    substitute_type_params_in_stmt(stmt->data.if_stmt.else_branch, bindings);
    break;

  case AST_STMT_WHILE:
    substitute_type_params_in_expr(stmt->data.while_stmt.cond, bindings);
    substitute_type_params_in_stmt(stmt->data.while_stmt.body, bindings);
    break;

  case AST_STMT_LOOP:
    substitute_type_params_in_expr(stmt->data.loop_stmt.start, bindings);
    substitute_type_params_in_expr(stmt->data.loop_stmt.end, bindings);
    substitute_type_params_in_expr(stmt->data.loop_stmt.iterator_name,
                                   bindings);
    substitute_type_params_in_stmt(stmt->data.loop_stmt.body, bindings);
    break;

  case AST_STMT_FOR:
    substitute_type_params_in_stmt(stmt->data.for_stmt.init, bindings);
    substitute_type_params_in_expr(stmt->data.for_stmt.cond, bindings);
    substitute_type_params_in_expr(stmt->data.for_stmt.update, bindings);
    substitute_type_params_in_stmt(stmt->data.for_stmt.body, bindings);
    break;

  case AST_STMT_BLOCK:
    for (size_t i = 0; i < stmt->data.block_stmt.stmt_count; i++) {
      substitute_type_params_in_stmt(stmt->data.block_stmt.stmts[i], bindings);
    }
    break;

  case AST_STMT_EXPR:
    substitute_type_params_in_expr(stmt->data.expr_stmt.expr, bindings);
    break;

  case AST_STMT_ASSIGN:
    substitute_type_params_in_expr(stmt->data.assign_stmt.lhs, bindings);
    substitute_type_params_in_expr(stmt->data.assign_stmt.rhs, bindings);
    break;

  case AST_STMT_PRINT:
    for (size_t i = 0; i < stmt->data.print_stmt.expr_count; i++) {
      substitute_type_params_in_expr(stmt->data.print_stmt.exprs[i], bindings);
    }
    break;

  case AST_STMT_CASE:
    substitute_type_params_in_expr(stmt->data.case_stmt.condition, bindings);
    substitute_type_params_in_stmt(stmt->data.case_stmt.body, bindings);
    break;

  case AST_STMT_SWITCH:
    substitute_type_params_in_expr(stmt->data.switch_stmt.condition, bindings);
    for (size_t i = 0; i < stmt->data.switch_stmt.case_count; i++) {
      substitute_type_params_in_stmt(stmt->data.switch_stmt.cases[i], bindings);
    }
    substitute_type_params_in_stmt(stmt->data.switch_stmt.default_case,
                                   bindings);
    break;

  case AST_STMT_DEFER:
    substitute_type_params_in_stmt(stmt->data.defer_stmt.stmt, bindings);
    break;

  case AST_STMT_BREAK:
  case AST_STMT_CONTINUE:
    // No nested nodes
    break;

  default:
    break;
  }
}

// Substitute type parameters in expressions (recursive)
static void substitute_type_params_in_expr(AstNode *expr,
                                           TypeBindings *bindings) {
  if (!expr)
    return;

  switch (expr->kind) {
  case AST_EXPR_EXPLICIT_CAST:
    // x as T
    substitute_type_params_in_expr(expr->data.explicit_cast.expr, bindings);
    substitute_type_params_in_type(expr->data.explicit_cast.target_type,
                                   bindings);
    break;

  case AST_EXPR_SIZEOF:
    // sizeof T
    substitute_type_params_in_type(expr->data.sizeof_expr.type_expr, bindings);
    break;

  case AST_EXPR_FUNCTION:
    // Anonymous function: fn(x T) U { ... }
    for (size_t i = 0; i < expr->data.func_expr.param_count; i++) {
      substitute_type_params_in_type(expr->data.func_expr.params[i].type,
                                     bindings);
    }
    substitute_type_params_in_type(expr->data.func_expr.return_type, bindings);
    substitute_type_params_in_stmt(expr->data.func_expr.body, bindings);
    break;

  // Recurse into nested expressions
  case AST_EXPR_BINARY_OP:
    substitute_type_params_in_expr(expr->data.binop.left, bindings);
    substitute_type_params_in_expr(expr->data.binop.right, bindings);
    break;

  case AST_EXPR_UNARY_OP:
    substitute_type_params_in_expr(expr->data.unop.operand, bindings);
    break;

  case AST_EXPR_CALL:
    substitute_type_params_in_expr(expr->data.call.func, bindings);
    for (size_t i = 0; i < expr->data.call.arg_count; i++) {
      substitute_type_params_in_expr(expr->data.call.args[i], bindings);
    }

    for (size_t i = 0; i < expr->data.call.type_arg_count; i++) {
      substitute_type_params_in_type(expr->data.call.type_args[i], bindings);
    }
    break;

  case AST_EXPR_INDEX:
    substitute_type_params_in_expr(expr->data.index_expr.array, bindings);
    substitute_type_params_in_expr(expr->data.index_expr.index, bindings);
    break;

  case AST_EXPR_SLICE:
    substitute_type_params_in_expr(expr->data.slice_expr.array, bindings);
    substitute_type_params_in_expr(expr->data.slice_expr.start, bindings);
    substitute_type_params_in_expr(expr->data.slice_expr.end, bindings);
    break;

  case AST_EXPR_MEMBER:
    substitute_type_params_in_expr(expr->data.member_expr.object, bindings);
    break;

  case AST_EXPR_MODULE_MEMBER:
    substitute_type_params_in_expr(expr->data.mod_member_expr.module, bindings);
    break;

  case AST_EXPR_TUPLE:
    for (size_t i = 0; i < expr->data.tuple_expr.element_count; i++) {
      substitute_type_params_in_expr(expr->data.tuple_expr.elements[i],
                                     bindings);
    }
    break;

  case AST_EXPR_STRUCT_LITERAL:
    for (size_t i = 0; i < expr->data.struct_literal.field_count; i++) {
      substitute_type_params_in_expr(expr->data.struct_literal.field_values[i],
                                     bindings);
    }

    for (size_t i = 0; i < expr->data.struct_literal.type_arg_count; i++) {
      substitute_type_params_in_type(expr->data.struct_literal.type_args[i],
                                     bindings);
    }
    break;

  case AST_EXPR_ARRAY_LITERAL:
    for (size_t i = 0; i < expr->data.array_literal.element_count; i++) {
      substitute_type_params_in_expr(expr->data.array_literal.elements[i],
                                     bindings);
    }
    break;

  case AST_EXPR_ARRAY_REPEAT:
    substitute_type_params_in_expr(expr->data.array_repeat.value, bindings);
    break;

  case AST_EXPR_INTERPOLATED_STRING:
    for (size_t i = 0; i < expr->data.interpolated_string.num_parts; i++) {
      substitute_type_params_in_expr(expr->data.interpolated_string.parts[i],
                                     bindings);
    }
    break;

  case AST_EXPR_GROUPED_EXPR:
    substitute_type_params_in_expr(expr->data.grouped_expr.inner_expr,
                                   bindings);
    break;

  case AST_EXPR_SOME:
    substitute_type_params_in_expr(expr->data.some_expr.value, bindings);
    break;

  case AST_EXPR_FORCE_UNWRAP:
    substitute_type_params_in_expr(expr->data.force_unwrap.operand, bindings);
    break;

  case AST_EXPR_POSTFIX_INC:
    substitute_type_params_in_expr(expr->data.postfix_inc.operand, bindings);
    break;

  case AST_EXPR_POSTFIX_DEC:
    substitute_type_params_in_expr(expr->data.postfix_dec.operand, bindings);
    break;

  case AST_EXPR_IMPLICIT_CAST:
    substitute_type_params_in_expr(expr->data.implicit_cast.expr, bindings);
    break;

  // Literals and identifiers - no substitution needed
  case AST_EXPR_LITERAL_INT:
  case AST_EXPR_LITERAL_FLOAT:
  case AST_EXPR_LITERAL_STRING:
  case AST_EXPR_LITERAL_CHAR:
  case AST_EXPR_LITERAL_BOOL:
  case AST_EXPR_LITERAL_NIL:
  case AST_EXPR_LITERAL_NONE:
  case AST_EXPR_IDENTIFIER:
  case AST_EXPR_CONTEXT:
  case AST_EXPR_PARTIAL_MEMBER:
    // No type parameters here
    break;

  default:
    break;
  }
}

// Main entry point: substitute in function signature and body
static void substitute_function_types(AstNode *func_decl,
                                      TypeBindings *bindings) {
  // Substitute in parameter types
  for (size_t i = 0; i < func_decl->data.func_decl.param_count; i++) {
    substitute_type_params_in_type(func_decl->data.func_decl.params[i].type,
                                   bindings);
  }

  // Substitute in return type
  substitute_type_params_in_type(func_decl->data.func_decl.return_type,
                                 bindings);

  // Substitute in body
  substitute_type_params_in_stmt(func_decl->data.func_decl.body, bindings);
}

// Helper: Check if a type has cycles (for monomorphized generics)
static bool check_type_has_cycles(Type *type, Visited **visited) {
  if (!type)
    return false;

  // Already has a canonical name - assume it's been checked
  if (type->canonical_name != NULL) {
    return false;
  }

  // Pointers break cycles
  if (type->kind == TYPE_POINTER) {
    return false;
  }

  // Check for cycle (back-edge)
  Visited *found;
  HASH_FIND_PTR(*visited, &type, found);
  if (found) {
    return true; // Cycle detected
  }

  // Add to visited
  Visited *new_v = arena_alloc(&long_lived, sizeof(Visited));
  new_v->key = type;
  HASH_ADD_PTR(*visited, key, new_v);

  // Check components recursively
  bool cycle_detected = false;

  if (type->kind == TYPE_STRUCT) {
    for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
      if (check_type_has_cycles(type->data.struct_data.field_types[i],
                                visited)) {
        cycle_detected = true;
        break;
      }
    }
  } else if (type->kind == TYPE_ARRAY) {
    cycle_detected = check_type_has_cycles(type->data.array.element, visited);
  } else if (type->kind == TYPE_POINTER) {
    // Pointers don't create cycles (they break them)
  } else if (type->kind == TYPE_SLICE) {
    cycle_detected = check_type_has_cycles(type->data.slice.element, visited);
  }

  HASH_DEL(*visited, new_v);
  return cycle_detected;
}

static bool check_function_body(Symbol *sym);

// Main monomorphization function
static Type *monomorphize_struct_type(AstNode *generic_struct_decl,
                                      AstNode **type_args,
                                      size_t type_arg_count,
                                      Location call_loc) {
  size_t type_param_count =
      generic_struct_decl->data.type_decl.type_params_count;

  if (type_arg_count != type_param_count) {
    checker_error(call_loc, "type '%s' expects %zu type arguments, got %zu",
                  generic_struct_decl->data.type_decl.name, type_param_count,
                  type_arg_count);
    return NULL;
  }

  // Step 1: Resolve type args
  TypeBindings bindings = {0};
  bindings.count = type_param_count;
  bindings.bindings =
      arena_alloc(&long_lived, sizeof(TypeBinding) * type_param_count);
  Type **concrete_types =
      arena_alloc(&long_lived, sizeof(Type *) * type_param_count);

  for (size_t i = 0; i < type_param_count; i++) {
    bindings.bindings[i].param_name =
        generic_struct_decl->data.type_decl.type_params[i].lexeme;
    Type *concrete = resolve_type_expression(type_args[i]);
    if (!concrete)
      return NULL;
    bindings.bindings[i].concrete_type = concrete;
    concrete_types[i] = concrete;
  }

  // Step 2: Mangle name
  char *mangled_name = mangle_generic_name(
      generic_struct_decl->data.type_decl.full_qualified_name, concrete_types,
      type_param_count);

  // Step 3: Check cache
  Type *existing = type_lookup(mangled_name, generic_struct_decl->loc.file);
  if (existing) {
    return existing;
  }

  // Step 4: Create placeholder EARLY
  Type *placeholder = type_create(TYPE_UNRESOLVED, call_loc);
  placeholder->declared_name = generic_struct_decl->data.type_decl.name;
  type_register(mangled_name, placeholder);

  // Step 5: Clone + substitute
  AstNode *type_expr = generic_struct_decl->data.type_decl.type_expr;
  AstNode *specialized_expr = clone_ast_node(type_expr);
  substitute_type_params_in_type(specialized_expr, &bindings);

  // Step 6: Resolve body — may hit placeholder (safe)
  checker_state.in_type_resolution = true;
  Type *resolved_body = resolve_type_expression(specialized_expr);
  checker_state.in_type_resolution = false;

  // Step 7: NOW update placeholder
  char *saved_declared_name = placeholder->declared_name;
  if (resolved_body) {
    *placeholder = *resolved_body;
    placeholder->canonical_name = NULL;
    placeholder->qualified_name = mangled_name;
    placeholder->declared_name = saved_declared_name;
  }

  // Step 8: Track generic args
  placeholder->generic_type_args = concrete_types;
  placeholder->generic_type_arg_count = type_arg_count;
  placeholder->declared_name = generic_struct_decl->data.type_decl.name;

  // Step 9: Check for cycles BEFORE canonicalization
  Visited *visited = NULL;
  bool has_cycle = check_type_has_cycles(placeholder, &visited);

  // Clean up visited set
  Visited *curr, *tmp;
  HASH_ITER(hh, visited, curr, tmp) { HASH_DEL(visited, curr); }

  if (has_cycle) {
    checker_error(
        call_loc,
        "recursive type '%s' has infinite size (use pointer for indirection)",
        placeholder->declared_name);
    return NULL;
  }

  // Step 10: Final canonicalization (dedup + name)
  canonicalize_type(&placeholder);

  // Step 11 (NEW): Process methods with partial substitution
  if (specialized_expr->kind == AST_TYPE_STRUCT &&
      specialized_expr->data.type_struct.method_count > 0) {

    size_t method_count = specialized_expr->data.type_struct.method_count;
    AstNode **methods = specialized_expr->data.type_struct.methods;

    // Temporary arrays on the stack/malloc to collect results
    char **temp_method_qualified_names = malloc(method_count * sizeof(char *));
    char **temp_method_reg_names = malloc(method_count * sizeof(char *));
    Type **temp_method_types = malloc(method_count * sizeof(Type *));
    Symbol **temp_generic_method_symbols =
        malloc(method_count * sizeof(Symbol *));
    char **temp_generic_method_reg_names =
        malloc(method_count * sizeof(char *));

    size_t regular_idx = 0;
    size_t generic_idx = 0;

    Symbol **concrete_methods_to_check =
        malloc(method_count * sizeof(Symbol *));
    size_t concrete_methods_count = 0;

    // Process each method
    for (size_t i = 0; i < method_count; i++) {
      AstNode *method = methods[i];

      // Clone the method AST node
      AstNode *cloned_method = clone_ast_node(method);

      char *qualified_name = cloned_method->data.func_decl.full_qualified_name;
      char *reg_name = cloned_method->data.func_decl.name;

      // Inherit parent type parameters (avoiding duplicates)
      size_t parent_type_param_count =
          generic_struct_decl->data.type_decl.type_params_count;
      Token *parent_type_params =
          generic_struct_decl->data.type_decl.type_params;

      // Create new combined type param list: [method_params...,
      // parent_params...] but skip parent params that are already in method
      // params
      size_t total_type_param_count =
          cloned_method->data.func_decl.type_param_count;
      Token *combined_type_params = arena_alloc(
          &long_lived, (cloned_method->data.func_decl.type_param_count +
                        parent_type_param_count) *
                           sizeof(Token));

      // Copy method's own type params first
      memcpy(combined_type_params, cloned_method->data.func_decl.type_params,
             cloned_method->data.func_decl.type_param_count * sizeof(Token));

      // Append parent params that aren't already in method params
      for (size_t j = 0; j < parent_type_param_count; j++) {
        bool already_exists = false;
        for (size_t k = 0; k < cloned_method->data.func_decl.type_param_count;
             k++) {
          if (strcmp(parent_type_params[j].lexeme,
                     cloned_method->data.func_decl.type_params[k].lexeme) ==
              0) {
            already_exists = true;
            break;
          }
        }
        if (!already_exists) {
          combined_type_params[total_type_param_count++] =
              parent_type_params[j];
        }
      }

      // Update the cloned method with combined params
      cloned_method->data.func_decl.type_params = combined_type_params;
      cloned_method->data.func_decl.type_param_count = total_type_param_count;

      // Apply partial substitution using substitute_function_types
      substitute_function_types(cloned_method, &bindings);

      // Update type param list - remove substituted params, keep unbound ones
      size_t new_type_param_count = 0;
      Token *new_type_params = arena_alloc(
          &long_lived,
          cloned_method->data.func_decl.type_param_count * sizeof(Token));

      for (size_t j = 0; j < cloned_method->data.func_decl.type_param_count;
           j++) {
        const char *param_name =
            cloned_method->data.func_decl.type_params[j].lexeme;

        // Check if this param was substituted in bindings
        bool was_substituted = false;
        for (size_t k = 0; k < bindings.count; k++) {
          if (strcmp(param_name, bindings.bindings[k].param_name) == 0) {
            was_substituted = true;
            break;
          }
        }

        // Keep params that weren't substituted
        if (!was_substituted) {
          new_type_params[new_type_param_count++] =
              cloned_method->data.func_decl.type_params[j];
        }
      }

      cloned_method->data.func_decl.type_params = new_type_params;
      cloned_method->data.func_decl.type_param_count = new_type_param_count;

      if (cloned_method->data.func_decl.type_param_count > 0) {
        // Still generic - create symbol and store in temp generic arrays
        Symbol *method_sym =
            symbol_create(qualified_name, SYMBOL_FUNCTION, cloned_method);
        method_sym->reg_name = reg_name;
        method_sym->is_method = true;
        method_sym->containing_struct_type = placeholder;
        scope_add_symbol(checker_state.current_module->scope, method_sym);

        temp_generic_method_symbols[generic_idx] = method_sym;
        temp_generic_method_reg_names[generic_idx] = reg_name;
        generic_idx++;
      } else {
        // Now concrete - create symbol and store in temp regular arrays
        // Mangle the concrete method name
        char *mangled_method_name = mangle_generic_name(
            qualified_name, concrete_types, type_param_count);

        // Update cloned method AST with mangled name
        cloned_method->data.func_decl.name = mangled_method_name;
        cloned_method->data.func_decl.qualified_name = mangled_method_name;
        cloned_method->data.func_decl.full_qualified_name = mangled_method_name;

        // Create human-readable display name with type args
        // Example: "push[int]"
        size_t display_name_len = strlen(reg_name) + 1; // base name + '['
        for (size_t j = 0; j < type_param_count; j++) {
          display_name_len += strlen(type_name(concrete_types[j]));
          if (j < type_param_count - 1) {
            display_name_len += 2; // ", "
          }
        }
        display_name_len += 2; // ']' + null terminator

        char *display_name = arena_alloc(&long_lived, display_name_len);
        strcpy(display_name, reg_name);
        strcat(display_name, "[");
        for (size_t j = 0; j < type_param_count; j++) {
          strcat(display_name, type_name(concrete_types[j]));
          if (j < type_param_count - 1) {
            strcat(display_name, ", ");
          }
        }
        strcat(display_name, "]");

        // Update cloned method with display name
        cloned_method->data.func_decl.name = display_name;

        Symbol *method_sym =
            symbol_create(mangled_method_name, SYMBOL_FUNCTION, cloned_method);
        method_sym->reg_name = reg_name;
        method_sym->is_method = true;
        method_sym->is_mono = true;
        method_sym->containing_struct_type = placeholder;
        scope_add_symbol(checker_state.current_module->scope, method_sym);

        // Store using the mangled name
        temp_method_qualified_names[regular_idx] = mangled_method_name;
        temp_method_reg_names[regular_idx] = reg_name;
        temp_method_types[regular_idx] = NULL;
        regular_idx++;

        concrete_methods_to_check[concrete_methods_count++] = method_sym;
      }
    }

    // Now arena_alloc the final arrays with correct sizes and copy
    if (regular_idx > 0) {
      placeholder->data.struct_data.method_qualified_names =
          arena_alloc(&long_lived, regular_idx * sizeof(char *));
      placeholder->data.struct_data.method_reg_names =
          arena_alloc(&long_lived, regular_idx * sizeof(char *));
      placeholder->data.struct_data.method_types =
          arena_alloc(&long_lived, regular_idx * sizeof(Type *));
      memcpy(placeholder->data.struct_data.method_qualified_names,
             temp_method_qualified_names, regular_idx * sizeof(char *));
      memcpy(placeholder->data.struct_data.method_reg_names,
             temp_method_reg_names, regular_idx * sizeof(char *));
      memcpy(placeholder->data.struct_data.method_types, temp_method_types,
             regular_idx * sizeof(Type *));
      placeholder->data.struct_data.method_count = regular_idx;
    }

    if (generic_idx > 0) {
      placeholder->data.struct_data.generic_method_symbols =
          arena_alloc(&long_lived, generic_idx * sizeof(Symbol *));
      placeholder->data.struct_data.generic_method_reg_names =
          arena_alloc(&long_lived, generic_idx * sizeof(char *));
      memcpy(placeholder->data.struct_data.generic_method_symbols,
             temp_generic_method_symbols, generic_idx * sizeof(Symbol *));
      memcpy(placeholder->data.struct_data.generic_method_reg_names,
             temp_generic_method_reg_names, generic_idx * sizeof(char *));
      placeholder->data.struct_data.generic_method_count = generic_idx;
    }

    // Free temporary arrays
    free(temp_method_qualified_names);
    free(temp_method_reg_names);
    free(temp_method_types);
    free(temp_generic_method_symbols);
    free(temp_generic_method_reg_names);

    // Check all concrete methods
    Scope *saved_scope = current_scope;
    for (size_t i = 0; i < concrete_methods_count; i++) {
      Symbol *method_sym = concrete_methods_to_check[i];

      // Check signature
      current_scope = checker_state.current_module->scope;
      if (check_function_signature(method_sym)) {
        for (size_t j = 0; j < regular_idx; j++) {
          if (strcmp(placeholder->data.struct_data.method_qualified_names[j],
                     method_sym->name) == 0) {
            placeholder->data.struct_data.method_types[j] = method_sym->type;
            break;
          }
        }

        // Check body
        check_function_body(method_sym);
      }
    }
    current_scope = saved_scope;

    free(concrete_methods_to_check);
  }

  return placeholder;
}

typedef struct {
  AstNode *func;
  Symbol *symbol;
} MonoResult;

// Monomorphize a generic function with concrete types
static MonoResult monomorphize_function(AstNode *generic_func,
                                        TypeBindings *bindings,
                                        Location call_loc,
                                        Module *context_module) {
  // Step 1: Generate mangled name
  Type **concrete_types =
      arena_alloc(&long_lived, sizeof(Type *) * bindings->count);
  for (size_t i = 0; i < bindings->count; i++) {
    concrete_types[i] = bindings->bindings[i].concrete_type;
  }

  char *mangled_name =
      mangle_generic_name(generic_func->data.func_decl.full_qualified_name,
                          concrete_types, bindings->count);

  // Step 2: Check cache - return if already monomorphized
  MonoFuncInstance *existing = NULL;
  HASH_FIND_STR(mono_instances, mangled_name, existing);
  if (existing) {
    return (MonoResult){
        .func = existing->monomorphized_func,
        .symbol = existing->symbol // ← Return cached symbol
    };
  }

  // Step 3: Deep clone the generic function
  AstNode *mono_func = clone_ast_node(generic_func);

  // Step 4: Substitute type parameters throughout
  substitute_function_types(mono_func, bindings);

  // Step 5: Update function metadata
  mono_func->data.func_decl.name = mangled_name;
  mono_func->data.func_decl.qualified_name = mangled_name;
  mono_func->data.func_decl.full_qualified_name = mangled_name;

  // Create a human-readable name with type arguments
  // e.g., "get[str, int]"
  size_t name_len =
      strlen(generic_func->data.func_decl.name) + 1; // base name + '['
  for (size_t i = 0; i < bindings->count; i++) {
    name_len += strlen(type_name(bindings->bindings[i].concrete_type));
    if (i < bindings->count - 1) {
      name_len += 2; // ", "
    }
  }
  name_len += 2; // ']' + null terminator

  char *display_name = arena_alloc(&long_lived, name_len);
  strcpy(display_name, generic_func->data.func_decl.name);
  strcat(display_name, "[");
  for (size_t i = 0; i < bindings->count; i++) {
    strcat(display_name, type_name(bindings->bindings[i].concrete_type));
    if (i < bindings->count - 1) {
      strcat(display_name, ", ");
    }
  }
  strcat(display_name, "]");

  mono_func->data.func_decl.name = display_name;

  // Clear type parameters (it's now concrete)
  mono_func->data.func_decl.type_param_count = 0;
  mono_func->data.func_decl.type_params = NULL;

  // Step 6: Add to symbol table
  Symbol *mono_sym = symbol_create(mangled_name, SYMBOL_FUNCTION, mono_func);
  scope_add_symbol(checker_state.current_module->scope, mono_sym);
  mono_sym->is_mono = true;

  // Cache BEFORE type-checking to break recursion cycles
  MonoFuncInstance *instance =
      arena_alloc(&long_lived, sizeof(MonoFuncInstance));
  instance->key = mangled_name;
  instance->generic_func = generic_func;
  instance->concrete_types = concrete_types;
  instance->type_count = bindings->count;
  instance->monomorphized_func = mono_func;
  instance->symbol = mono_sym;
  HASH_ADD_KEYPTR(hh, mono_instances, instance->key, strlen(instance->key),
                  instance);

  // Step 7: Temporarily switch to defining module context for checking
  Module *saved_module = checker_state.current_module;
  Scope *saved_scope = current_scope;
  checker_set_current_module(context_module);

  // Type-check signature in defining context
  if (!check_function_signature(mono_sym)) {
    checker_state.current_module = saved_module;
    // Remove from cache if signature check fails
    HASH_DEL(mono_instances, instance);
    checker_error(call_loc, "failed to monomorphize function '%s' called here",
                  generic_func->data.func_decl.name);
    return (MonoResult){.func = NULL, .symbol = NULL};
  }

  // Step 8: Type-check the body
  bool succeeded = check_function_body(mono_sym);

  // Restore scope state and current module
  checker_set_current_module(saved_module);
  current_scope = saved_scope;

  if (!succeeded) {
    // Remove from cache if body check fails
    HASH_DEL(mono_instances, instance);
    checker_error(
        call_loc,
        "failed to type-check monomorphized function '%s' called here",
        generic_func->data.func_decl.name);
    return (MonoResult){.func = NULL, .symbol = NULL};
  }

  // Step 9: Return the monomorphized function
  return (MonoResult){.func = mono_func, .symbol = mono_sym};
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

  case AST_EXPR_INTERPOLATED_STRING: {
    // Check all parts of the interpolated string
    for (size_t i = 0; i < expr->data.interpolated_string.num_parts; i++) {
      AstNode *part = expr->data.interpolated_string.parts[i];
      Type *part_type = check_expression(part);

      if (!part_type) {
        return NULL; // Error already reported
      }
    }

    // The whole interpolated string evaluates to a string
    expr->resolved_type = type_string;
    return type_string;
  }

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
    expr->resolved_type = type_usize;
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

  case AST_EXPR_POSTFIX_DEC: {
    AstNode *operand = expr->data.postfix_dec.operand;
    Type *operand_ty = check_expression(operand);

    if (!operand_ty) {
      return NULL;
    }

    // Check if operand is an lvalue
    if (!is_lvalue(operand)) {
      checker_error(expr->loc, "operand of '--' must be an lvalue");
      return NULL;
    }

    // Only works on integer types
    if (!type_is_integral(operand_ty)) {
      checker_error(expr->loc, "'--' requires integral type, got '%s'",
                    type_name(operand_ty));
      return NULL;
    }

    expr->resolved_type = operand_ty;
    return operand_ty;
  }

  case AST_EXPR_IDENTIFIER: {
    char *name = expr->data.ident.name;
    Scope *mod_scope = checker_state.current_module->scope;
    Symbol *sym = scope_lookup(mod_scope, current_scope, name,
                               checker_state.current_module->name);
    if (!sym) {
      checker_error(expr->loc, "undefined name '%s'", name);
      return NULL;
    }

    if (sym->kind == SYMBOL_EXTERN_FUNCTION ||
        sym->kind == SYMBOL_EXTERN_CONSTANT ||
        sym->kind == SYMBOL_EXTERN_VARIABLE) {
      expr->data.ident.is_extern = true;
    }

    if (sym->kind == SYMBOL_VARIABLE || sym->kind == SYMBOL_CONSTANT) {
      // The name got qualified, so we should update the reference to it
      if (sym->data.var.is_global) {
        char *prefix =
            prepend(checker_state.current_module->qualified_name, "__");
        char *qualified = prepend(prefix, sym->reg_name);
        expr->data.ident.full_qualified_name = qualified;
      }

      expr->data.ident.qualified_name = sym->name;
    } else {
      char *prefix =
          prepend(checker_state.current_module->qualified_name, "__");
      char *qualified = prepend(prefix, sym->reg_name);
      expr->data.ident.full_qualified_name = qualified;
    }

    // Types are not values
    if (sym->kind == SYMBOL_TYPE && sym->type->kind != TYPE_ENUM &&
        sym->type->kind != TYPE_TAGGED_UNION) {
      checker_error(expr->loc, "'%s' is a type, not a value", name);
      return NULL;
    }

    // Only variables, constants and functions can be used as values
    if (sym->kind != SYMBOL_VARIABLE && sym->kind != SYMBOL_CONSTANT &&
        sym->kind != SYMBOL_FUNCTION &&
        (sym->kind == SYMBOL_TYPE && sym->type->kind != TYPE_ENUM &&
         sym->type->kind != TYPE_TAGGED_UNION)) {
      checker_error(expr->loc, "'%s' cannot be used as a value", name);
      return NULL;
    }

    if (!sym->type) {
      checker_error(expr->loc, "name '%s' used before type is resolved", name);
      return NULL;
    }

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
        op == BINOP_DIV || op == BINOP_MOD) {
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
      if (op == BINOP_MOD) {
        if (type_is_floating(left) || type_is_floating(right)) {
          checker_error(
              expr->loc,
              "Modulo operator cannot be used with floating-point types");
          return NULL;
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
          checker_error(expr->loc,
                        "incompatible types in binary operation: %s and %s",
                        type_name(left), type_name(right));
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

      AstNode *right_converted =
          maybe_insert_cast(expr->data.binop.right, right, left);
      if (!right_converted) {
        checker_error(expr->loc, "type mismatch in equality check '%s' != '%s'",
                      type_name(left), type_name(right));
        return NULL;
      }
      expr->resolved_type = type_bool;
      return type_bool;
    }

    // Bitwise: &, |, ^, <<, >>
    if (op == BINOP_BIT_AND || op == BINOP_BIT_OR || op == BINOP_BIT_XOR ||
        op == BINOP_BIT_SHL || op == BINOP_BIT_SHR) {
      if (!type_is_integral(left) || !type_is_integral(right)) {
        checker_error(expr->loc,
                      "bitwise operation requires integer operands, got %s",
                      type_name(expr->resolved_type));
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
      if (!type_is_integral(operand)) {
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

    if (func_expr->kind == AST_EXPR_MEMBER &&
        func_expr->data.member_expr.is_method_ref) {
      char *qualified_method_name =
          func_expr->data.member_expr.method_qualified_name;

      if (func_expr->data.member_expr.is_associated_function) {
        // Associated function call - just rewrite to qualified name, no
        // receiver
        AstNode *qualified_func = arena_alloc(&long_lived, sizeof(AstNode));
        memset(qualified_func, 0, sizeof(AstNode));
        qualified_func->kind = AST_EXPR_IDENTIFIER;
        qualified_func->loc = func_expr->loc;
        qualified_func->data.ident.name = qualified_method_name;
        qualified_func->data.ident.qualified_name = qualified_method_name;
        qualified_func->data.ident.full_qualified_name = qualified_method_name;
        qualified_func->resolved_type = call_type;

        // No argument prepending for associated functions
        expr->data.call.func = qualified_func;
        func_expr = qualified_func;
        // args and arg_count stay the same
      } else {
        // Instance method call - rewrite with receiver conversion
        AstNode *object = func_expr->data.member_expr.object;

        // Get the method's function type to check the first parameter (self)
        // type
        Type *method_func_type = call_type;
        Type *expected_self_type = NULL;

        if (method_func_type->kind == TYPE_FUNCTION) {
          if (method_func_type->data.func.param_count == 0) {
            checker_error(func_expr->loc, "Invalid method type");
            return NULL;
          }
          expected_self_type = method_func_type->data.func.param_types[0];
        } else if (method_func_type->kind == TYPE_GENERIC_FUNCTION) {
          AstNode *generic_decl = method_func_type->data.generic_decl.decl;
          if (generic_decl->data.func_decl.param_count == 0) {
            checker_error(func_expr->loc, "Invalid method type");
            return NULL;
          }
          // Get the first parameter's type from the AST and resolve it
          FuncParam *first_param = &generic_decl->data.func_decl.params[0];
          expected_self_type = resolve_type_expression(first_param->type);
          if (!expected_self_type) {
            checker_error(func_expr->loc,
                          "Could not resolve self parameter type");
            return NULL;
          }
        } else {
          checker_error(func_expr->loc, "Invalid method type");
          return NULL;
        }
        Type *actual_object_type = object->resolved_type;

        // Handle automatic pointer conversion for self parameter
        AstNode *converted_object = object;
        if (!type_equals(actual_object_type, expected_self_type)) {
          // Try automatic conversions
          if (expected_self_type->kind == TYPE_POINTER &&
              type_equals(actual_object_type,
                          expected_self_type->data.ptr.base)) {
            // Method expects pointer, object is value → take address
            AstNode *addr_expr = arena_alloc(&long_lived, sizeof(AstNode));
            memset(addr_expr, 0, sizeof(AstNode));
            addr_expr->kind = AST_EXPR_UNARY_OP;
            addr_expr->loc = object->loc;
            addr_expr->data.unop.op = UNOP_ADDR;
            addr_expr->data.unop.operand = object;
            addr_expr->resolved_type = expected_self_type;
            converted_object = addr_expr;
          } else if (actual_object_type->kind == TYPE_POINTER &&
                     type_equals(actual_object_type->data.ptr.base,
                                 expected_self_type)) {
            // Method expects value, object is pointer → dereference
            AstNode *deref_expr = arena_alloc(&long_lived, sizeof(AstNode));
            memset(deref_expr, 0, sizeof(AstNode));
            deref_expr->kind = AST_EXPR_UNARY_OP;
            deref_expr->loc = object->loc;
            deref_expr->data.unop.op = UNOP_DEREF;
            deref_expr->data.unop.operand = object;
            deref_expr->resolved_type = expected_self_type;
            converted_object = deref_expr;
          } else {
            checker_error(
                object->loc,
                "Method call type mismatch: method expects %s, got %s",
                type_name(expected_self_type), type_name(actual_object_type));
            return NULL;
          }
        }

        // Create new identifier for the qualified method name
        AstNode *qualified_func = arena_alloc(&long_lived, sizeof(AstNode));
        memset(qualified_func, 0, sizeof(AstNode));
        qualified_func->kind = AST_EXPR_IDENTIFIER;
        qualified_func->loc = func_expr->loc;
        qualified_func->data.ident.name = qualified_method_name;
        qualified_func->data.ident.qualified_name = qualified_method_name;
        qualified_func->data.ident.full_qualified_name = qualified_method_name;
        qualified_func->resolved_type = call_type;

        // Prepend object to the arguments
        size_t new_arg_count = arg_count + 1;
        AstNode **new_args =
            arena_alloc(&long_lived, new_arg_count * sizeof(AstNode *));

        new_args[0] = converted_object;
        if (arg_count > 0) {
          memcpy(&new_args[1], args, arg_count * sizeof(AstNode *));
        }

        // Update the call expression
        expr->data.call.func = qualified_func;
        expr->data.call.args = new_args;
        expr->data.call.arg_count = new_arg_count;

        func_expr = qualified_func;
        args = new_args;
        arg_count = new_arg_count;
      }
    }

    // Check if this is a generic function
    if (call_type->kind == TYPE_GENERIC_FUNCTION) {
      AstNode *generic_decl = call_type->data.generic_decl.decl;
      TypeBindings bindings;

      bool inferred_arguments = false;

      if (expr->data.call.type_args != NULL) {
        // Use explicit type arguments instead of inference
        size_t type_arg_count = expr->data.call.type_arg_count;
        size_t type_param_count = generic_decl->data.func_decl.type_param_count;

        // Validate count
        if (type_arg_count != type_param_count) {
          checker_error(expr->loc,
                        "function '%s' expects %zu type arguments, got %zu",
                        generic_decl->data.func_decl.name, type_param_count,
                        type_arg_count);
          return NULL;
        }

        // Build TypeBindings from explicit type arguments
        bindings.count = type_param_count;
        bindings.bindings =
            arena_alloc(&long_lived, sizeof(TypeBinding) * type_param_count);

        for (size_t i = 0; i < type_param_count; i++) {
          bindings.bindings[i].param_name =
              generic_decl->data.func_decl.type_params[i].lexeme;

          // Resolve the type expression to get a concrete Type*
          Type *concrete_type =
              resolve_type_expression(expr->data.call.type_args[i]);

          if (!concrete_type) {
            checker_error(expr->data.call.type_args[i]->loc,
                          "invalid type argument %zu", i + 1);
            return NULL;
          }

          bindings.bindings[i].concrete_type = concrete_type;
        }
      } else {
        // Infer type arguments from call arguments
        bindings =
            infer_type_arguments(generic_decl, args, arg_count, expr->loc);
        // Check if inference succeeded
        for (size_t i = 0; i < bindings.count; i++) {
          if (!bindings.bindings[i].concrete_type) {
            checker_error(expr->loc, "could not infer type parameter '%s'",
                          bindings.bindings[i].param_name);
            return NULL;
          }
        }

        inferred_arguments = true;
      }

      // Determine defining module for monomorphization context
      Module *context_module;
      AstNode *func_expr = expr->data.call.func;

      if (func_expr->kind == AST_EXPR_MODULE_MEMBER) {
        // Qualified call: e.g., mem::new -> lookup "mem"
        const char *mod_name =
            func_expr->data.mod_member_expr.module->data.ident.name;
        context_module =
            lookup_imported_module(checker_state.current_module, mod_name);
        if (!context_module) {
          // Shouldn't happen (prior resolution would error), but safe
          checker_error(expr->loc,
                        "internal error: could not find defining module '%s'",
                        mod_name);
          return NULL;
        }
      } else {
        // Local/unqualified call: e.g., new.[int]() in same module
        context_module = checker_state.current_module;
      }

      // Monomorphize!
      MonoResult mono_result = monomorphize_function(generic_decl, &bindings,
                                                     expr->loc, context_module);
      if (!mono_result.func) {
        return NULL; // Error already reported
      }

      AstNode *mono_func = mono_result.func;
      Symbol *mono_sym = mono_result.symbol;

      if (!mono_sym) {
        checker_error(expr->loc,
                      "internal error: monomorphized function has no type");
        return NULL;
      }

      // Update the call expression to reference the monomorphized function
      if (func_expr->kind == AST_EXPR_IDENTIFIER) {
        func_expr->data.ident.name = mono_func->data.func_decl.name;
        func_expr->data.ident.qualified_name =
            mono_func->data.func_decl.qualified_name;
        func_expr->data.ident.full_qualified_name =
            mono_func->data.func_decl.full_qualified_name;
        func_expr->resolved_type = mono_sym->type;
      } else if (func_expr->kind == AST_EXPR_MODULE_MEMBER) {
        // Transform module member access into a regular identifier
        // A::foo[int]() becomes a call to A__foo__int() in current scope
        func_expr->kind = AST_EXPR_IDENTIFIER;
        func_expr->data.ident.name = mono_func->data.func_decl.name;
        func_expr->data.ident.qualified_name =
            mono_func->data.func_decl.qualified_name;
        func_expr->data.ident.full_qualified_name =
            mono_func->data.func_decl.full_qualified_name;
        func_expr->data.ident.is_extern = false;
        func_expr->resolved_type = mono_sym->type;
      }

      // Now treat it like a normal function call
      call_type = mono_sym->type;

      // NOTE: Inferred will already check arguments and cast them
      if (inferred_arguments) {
        // Check arity and convention before exit
        Type *func_type = call_type;
        CallingConvention callee_conv = func_type->data.func.convention;
        size_t param_count = func_type->data.func.param_count;
        Type *return_type = func_type->data.func.return_type;
        bool is_variadic = func_type->data.func.is_variadic;

        // C convention cannot call Pebble convention directly
        if (callee_conv == CALL_CONV_PEBBLE &&
            checker_state.current_convention == CALL_CONV_C) {
          checker_error(expr->loc, "cannot call Pebble convention function "
                                   "from C convention function");
        }

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
        } else {
          // Check argument count
          if (arg_count != param_count) {
            checker_error(expr->loc,
                          "function '%s' expects %zu arguments, got %zu",
                          type_name(func_type), param_count, arg_count);
            return NULL;
          }
        }

        expr->resolved_type = return_type;
        return return_type;
      }
    }

    // Only reached for non-generic functions
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

    // Handle variadic functions
    if (is_variadic) {
      size_t required_params = param_count - 1;

      // Must have at least required_params arguments
      if (arg_count < required_params) {
        checker_error(
            expr->loc,
            "variadic function '%s' expects at least %zu arguments, got %zu",
            type_name(func_type), required_params, arg_count);
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
      // Check argument count
      if (arg_count != param_count) {
        checker_error(expr->loc, "function '%s' expects %zu arguments, got %zu",
                      type_name(func_type), param_count, arg_count);
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
      AstNode *cast = maybe_insert_cast(start_expr, start_type, type_usize);
      if (!cast) {
        checker_error(start_expr->loc,
                      "slice start index must be numerically typed");
        return NULL;
      }
      start_expr = cast;
    }

    // Check end index if present
    if (end_expr) {
      Type *end_type = check_expression(end_expr);
      if (!end_type) {
        return NULL;
      }
      AstNode *cast = maybe_insert_cast(end_expr, end_type, type_usize);
      if (!cast) {
        checker_error(end_expr->loc,
                      "slice end index must be numerically typed");
        return NULL;
      }
      end_expr = cast;
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

    // Check if the object is a type. This will allow associated function access
    if (object_expr->kind == AST_EXPR_IDENTIFIER) {
      // TODO: Handle case where there are type arguments.
      Symbol *sym = scope_lookup(checker_state.current_module->scope,
                                 current_scope, object_expr->data.ident.name,
                                 checker_state.current_module->name);

      if (sym && sym->kind == SYMBOL_TYPE) {
        // Check for associated function
        Type *type = sym->type;
        if (type->kind != TYPE_STRUCT) {
          checker_error(expr->loc,
                        "Only struct types can have associated functions");
          return NULL;
        }

        // Look through the struct's methods for associated functions
        size_t method_count = type->data.struct_data.method_count;
        char **method_qualified_names =
            type->data.struct_data.method_qualified_names;
        char **method_reg_names = type->data.struct_data.method_reg_names;
        Type **method_types = type->data.struct_data.method_types;

        for (size_t i = 0; i < method_count; i++) {
          if (!method_qualified_names[i] || !method_types[i] ||
              !method_reg_names[i]) {
            continue;
          }

          char *reg_name = method_reg_names[i];
          if (strcmp(reg_name, field_name) == 0) {
            // Check if it's an associated function by checking the actual
            // function
            char *qualified_name = method_qualified_names[i];
            Symbol *method_sym = scope_lookup_local(
                checker_state.current_module->scope, qualified_name);

            if (method_sym && method_sym->decl &&
                method_sym->decl->kind == AST_DECL_FUNCTION) {
              AstNode *method_decl = method_sym->decl;
              bool is_associated = true;

              if (method_decl->data.func_decl.param_count > 0) {
                FuncParam *first_param = &method_decl->data.func_decl.params[0];
                if (strcmp(first_param->name, "self") == 0) {
                  is_associated = false;
                }
              }

              if (is_associated) {
                // Found associated function!
                expr->data.member_expr.is_method_ref = true;
                expr->data.member_expr.is_associated_function = true;
                expr->data.member_expr.method_qualified_name = qualified_name;
                expr->resolved_type = method_types[i];
                return method_types[i];
              } else {
                checker_error(expr->loc,
                              "Cannot call instance method '%s' on type",
                              reg_name);
                return NULL;
              }
            }
          }
        }

        // Search generic method templates if regular methods didn't match
        size_t generic_method_count =
            type->data.struct_data.generic_method_count;
        Symbol **generic_method_symbols =
            type->data.struct_data.generic_method_symbols;
        char **generic_method_reg_names =
            type->data.struct_data.generic_method_reg_names;

        for (size_t i = 0; i < generic_method_count; i++) {
          if (!generic_method_symbols[i] || !generic_method_reg_names[i]) {
            continue;
          }

          char *reg_name = generic_method_reg_names[i];
          if (strcmp(reg_name, field_name) == 0) {
            // Check if it's an associated function (no self parameter)
            AstNode *method_decl = generic_method_symbols[i]->decl;
            bool is_associated = true;

            if (method_decl->data.func_decl.param_count > 0) {
              FuncParam *first_param = &method_decl->data.func_decl.params[0];
              if (strcmp(first_param->name, "self") == 0) {
                is_associated = false;
              }
            }

            if (is_associated) {
              // Found generic associated function!
              expr->data.member_expr.is_method_ref = true;
              expr->data.member_expr.is_associated_function = true;
              expr->data.member_expr.method_qualified_name =
                  generic_method_symbols[i]->name;
              expr->resolved_type = generic_method_symbols[i]->type;
              return generic_method_symbols[i]->type;
            } else {
              checker_error(expr->loc,
                            "Cannot call instance method '%s' on type",
                            reg_name);
              return NULL;
            }
          }
        }

        checker_error(expr->loc, "Type '%s' has no associated function '%s'",
                      type_name(type), field_name);
        return NULL;
      }
    } else if (object_expr->kind == AST_EXPR_MODULE_MEMBER) {
      AstNode *module_expr = object_expr->data.mod_member_expr.module;
      const char *member_name = object_expr->data.mod_member_expr.member;

      Module *module = lookup_imported_module(checker_state.current_module,
                                              module_expr->data.ident.name);
      if (!module) {
        checker_error(expr->loc, "cannot find module '%s'",
                      module_expr->data.ident.name);
        return NULL;
      }

      char *prefix = prepend(module->name, "__");
      char *qualified_name = prepend(prefix, member_name);
      Symbol *sym = scope_lookup_local(module->scope, qualified_name);

      if (sym && sym->kind == SYMBOL_TYPE && sym->type->kind == TYPE_STRUCT) {
        // Copy the same method lookup logic from the local type case
        Type *type = sym->type;
        size_t method_count = type->data.struct_data.method_count;
        char **method_qualified_names =
            type->data.struct_data.method_qualified_names;
        char **method_reg_names = type->data.struct_data.method_reg_names;
        Type **method_types = type->data.struct_data.method_types;

        for (size_t i = 0; i < method_count; i++) {
          if (!method_qualified_names[i] || !method_types[i] ||
              !method_reg_names[i]) {
            continue;
          }

          char *reg_name = method_reg_names[i];
          if (strcmp(reg_name, field_name) == 0) {
            char *qualified_method_name = method_qualified_names[i];
            Symbol *method_sym =
                scope_lookup_local(module->scope, qualified_method_name);

            if (method_sym && method_sym->decl &&
                method_sym->decl->kind == AST_DECL_FUNCTION) {
              AstNode *method_decl = method_sym->decl;
              bool is_associated = true;

              if (method_decl->data.func_decl.param_count > 0) {
                FuncParam *first_param = &method_decl->data.func_decl.params[0];
                if (strcmp(first_param->name, "self") == 0) {
                  is_associated = false;
                }
              }

              if (is_associated) {
                expr->data.member_expr.is_method_ref = true;
                expr->data.member_expr.is_associated_function = true;
                expr->data.member_expr.method_qualified_name =
                    qualified_method_name;
                expr->resolved_type = method_types[i];
                return method_types[i];
              } else {
                checker_error(expr->loc,
                              "Cannot call instance method '%s' on type",
                              reg_name);
                return NULL;
              }
            }
          }
        }

        // Search generic method templates for associated functions
        size_t generic_method_count =
            type->data.struct_data.generic_method_count;
        Symbol **generic_method_symbols =
            type->data.struct_data.generic_method_symbols;
        char **generic_method_reg_names =
            type->data.struct_data.generic_method_reg_names;

        for (size_t i = 0; i < generic_method_count; i++) {
          if (!generic_method_symbols[i] || !generic_method_reg_names[i]) {
            continue;
          }

          char *reg_name = generic_method_reg_names[i];
          if (strcmp(reg_name, field_name) == 0) {
            // Check if it's an associated function (no self parameter)
            AstNode *method_decl = generic_method_symbols[i]->decl;
            bool is_associated = true;

            if (method_decl->data.func_decl.param_count > 0) {
              FuncParam *first_param = &method_decl->data.func_decl.params[0];
              if (strcmp(first_param->name, "self") == 0) {
                is_associated = false;
              }
            }

            if (is_associated) {
              // Found generic associated function!
              expr->data.member_expr.is_method_ref = true;
              expr->data.member_expr.is_associated_function = true;
              expr->data.member_expr.method_qualified_name =
                  generic_method_symbols[i]->name;
              expr->resolved_type = generic_method_symbols[i]->type;
              return generic_method_symbols[i]->type;
            } else {
              checker_error(expr->loc,
                            "Cannot call instance method '%s' on type",
                            reg_name);
              return NULL;
            }
          }
        }

        checker_error(expr->loc, "Type '%s' has no associated function '%s'",
                      type_name(type), field_name);
        return NULL;
      } else if (sym) {
        checker_error(expr->loc, "'%s::%s' is not a struct type",
                      module_expr->data.ident.name, member_name);
        return NULL;
      } else {
        checker_error(expr->loc, "undefined name '%s::%s'",
                      module_expr->data.ident.name, member_name);
        return NULL;
      }
    }

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
    } else if (base_type->kind == TYPE_SLICE) {
      if (strcmp(field_name, "len") == 0) {
        expr->resolved_type = type_usize;
        return type_usize;
      } else if (strcmp(field_name, "data") == 0) {
        expr->resolved_type =
            type_create_pointer(base_type->data.slice.element, true, expr->loc);
        return expr->resolved_type;
      } else {
        checker_error(expr->loc, "slice has only 'data' and 'len' fields");
        return NULL;
      }
    } else if (base_type->kind == TYPE_ARRAY) {
      if (strcmp(field_name, "len") == 0) {
        expr->resolved_type = type_usize;
        return type_usize;
      } else {
        checker_error(expr->loc, "array has only 'len' field");
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

      size_t method_count = base_type->data.struct_data.method_count;
      char **method_qualified_names =
          base_type->data.struct_data.method_qualified_names;
      char **method_reg_names = base_type->data.struct_data.method_reg_names;
      Type **method_types = base_type->data.struct_data.method_types;

      for (size_t i = 0; i < method_count; i++) {
        if (!method_qualified_names[i] || !method_types[i] ||
            !method_reg_names[i]) {
          continue; // Method not fully resolved yet
        }

        // Extract method name from qualified name
        // qualified name is like "module__Struct__methodName"
        char *qualified_name = method_qualified_names[i];
        char *reg_name = method_reg_names[i];
        if (strcmp(reg_name, field_name) == 0) {
          // Check if this is an associated function by examining the
          // function type
          Type *method_func_type = method_types[i];
          if (method_func_type->kind == TYPE_FUNCTION) {
            bool is_associated = true;

            if (method_func_type->data.func.param_count > 0) {
              Type *first_param_type =
                  method_func_type->data.func.param_types[0];
              // If first param is the struct type (or pointer to it), it's an
              // instance method
              if (first_param_type == base_type ||
                  (first_param_type->kind == TYPE_POINTER &&
                   first_param_type->data.ptr.base == base_type)) {
                is_associated = false;
              }
            }

            if (is_associated) {
              checker_error(expr->loc,
                            "Cannot call associated function '%s' on instance, "
                            "use '%s.%s' instead",
                            reg_name, type_name(base_type), reg_name);
              return NULL;
            }
          }

          expr->data.member_expr.is_method_ref = true;
          expr->data.member_expr.method_qualified_name = qualified_name;
          expr->resolved_type = method_types[i];
          return method_types[i];
        }
      }

      // Search generic method templates if regular methods didn't match
      size_t generic_method_count =
          base_type->data.struct_data.generic_method_count;
      Symbol **generic_method_symbols =
          base_type->data.struct_data.generic_method_symbols;
      char **generic_method_reg_names =
          base_type->data.struct_data.generic_method_reg_names;

      for (size_t i = 0; i < generic_method_count; i++) {
        if (!generic_method_symbols[i] || !generic_method_reg_names[i]) {
          continue;
        }

        char *reg_name = generic_method_reg_names[i];
        if (strcmp(reg_name, field_name) == 0) {
          // Found generic method - check if it's an instance method
          AstNode *method_decl = generic_method_symbols[i]->decl;
          bool is_instance_method = false;

          if (method_decl->data.func_decl.param_count > 0) {
            FuncParam *first_param = &method_decl->data.func_decl.params[0];
            if (strcmp(first_param->name, "self") == 0) {
              is_instance_method = true;
            }
          }

          if (is_instance_method) {
            // Found generic instance method!
            expr->data.member_expr.is_method_ref = true;
            expr->data.member_expr.method_qualified_name =
                generic_method_symbols[i]->name;
            expr->resolved_type = generic_method_symbols[i]->type;
            return generic_method_symbols[i]->type;
          } else {
            checker_error(expr->loc,
                          "Cannot call associated function '%s' on instance, "
                          "use '%s.%s' instead",
                          reg_name, type_name(base_type), reg_name);
            return NULL;
          }
        }
      }

      checker_error(expr->loc, "struct %s has no field or method named '%s'",
                    type_name(base_type), field_name);
      return NULL;
    } else if (base_type->kind == TYPE_UNION ||
               base_type->kind == TYPE_TAGGED_UNION) {
      size_t variant_count = base_type->data.union_data.variant_count;
      char **variant_names = base_type->data.union_data.variant_names;
      Type **variant_types = base_type->data.union_data.variant_types;

      for (size_t i = 0; i < variant_count; i++) {
        if (strcmp(variant_names[i], field_name) == 0) {
          expr->resolved_type = variant_types[i];
          return variant_types[i];
        }
      }

      checker_error(expr->loc, "union has no field named '%s'", field_name);
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

  case AST_EXPR_PARTIAL_MEMBER: {
    char **variant_names = arena_alloc(&long_lived, 1);
    variant_names[0] = expr->data.partial_member_expr.member;

    Type *type = type_create_enum(variant_names, 1,
                                  !checker_state.in_type_resolution, expr->loc);
    expr->resolved_type = type;

    return type;
  }

  case AST_EXPR_MODULE_MEMBER: {
    AstNode *module_expr = expr->data.mod_member_expr.module;
    const char *member_name = expr->data.mod_member_expr.member;

    Symbol *sym = NULL;
    Module *module = lookup_imported_module(checker_state.current_module,
                                            module_expr->data.ident.name);
    if (!module) {
      checker_error(expr->loc, "cannot find module '%s'",
                    module_expr->data.ident.name);
    } else {
      expr->data.mod_member_expr.qualified_path = module->qualified_name;

      // We need to convert these 2 into a qualifed string to perform a
      // name lookup
      char *prefix = prepend(module->name, "__");
      char *qualified_name = prepend(prefix, member_name);

      sym = scope_lookup_local(module->scope, qualified_name);
    }

    if (!sym) {
      checker_error(expr->loc, "undefined name '%s::%s'",
                    module_expr->data.ident.name, member_name);
      return NULL;
    } else if (sym->kind == SYMBOL_EXTERN_FUNCTION ||
               sym->kind == SYMBOL_EXTERN_VARIABLE ||
               sym->kind == SYMBOL_EXTERN_CONSTANT) {
      expr->data.mod_member_expr.is_extern = true;
    }

    // Types are not values
    if (sym->kind == SYMBOL_TYPE && sym->type->kind != TYPE_ENUM &&
        sym->type->kind != TYPE_ENUM) {
      checker_error(expr->loc, "'%s::%s' is a type, not a value",
                    module_expr->data.ident.name, member_name);
      return NULL;
    }

    // Only variables, constants and functions can be used as values
    if (sym->kind != SYMBOL_VARIABLE && sym->kind != SYMBOL_CONSTANT &&
        sym->kind != SYMBOL_FUNCTION &&
        (sym->kind == SYMBOL_TYPE && sym->type->kind != TYPE_ENUM &&
         sym->type->kind != TYPE_TAGGED_UNION)) {
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
    char *struct_type_name = expr->data.struct_literal.type_name;
    const char *type_mod_name = checker_state.current_module->name;
    char **field_names = expr->data.struct_literal.field_names;
    AstNode **field_values = expr->data.struct_literal.field_values;
    size_t field_count = expr->data.struct_literal.field_count;

    typedef struct {
      char *name;
      UT_hash_handle hh;
    } variant_entry;
    variant_entry *seen = NULL;

    Arena temp_arena;
    arena_init(&temp_arena, 1024);

    for (size_t i = 0; i < field_count; i++) {
      variant_entry *entry;
      HASH_FIND_STR(seen, field_names[i], entry);
      if (entry) {
        checker_error(expr->loc, "Duplicate struct field '%s'", field_names[i]);
      } else {
        entry = arena_alloc(&temp_arena, sizeof(variant_entry));
        entry->name = field_names[i];
        HASH_ADD_KEYPTR(hh, seen, entry->name, strlen(entry->name), entry);
      }
    }

    HASH_CLEAR(hh, seen);
    arena_free(&temp_arena);

    if (!struct_type_name) {
      Type **field_types =
          arena_alloc(&long_lived, field_count * sizeof(Type *));
      for (size_t i = 0; i < field_count; i++) {
        field_types[i] = check_expression(field_values[i]);
        if (!field_types[i]) {
          return NULL;
        }
      }

      Type *struct_type =
          type_create_struct(field_names, field_types, field_count, false,
                             !checker_state.in_type_resolution, expr->loc);
      expr->resolved_type = struct_type;

      return struct_type;
    }

    // Look up the type
    Type *type_of = type_lookup(struct_type_name, type_mod_name);
    if (!type_of) {
      checker_error(expr->loc, "undefined type '%s'", struct_type_name);
      return NULL;
    }

    if (type_of->kind == TYPE_GENERIC_TYPE_DECL) {
      AstNode **type_args = expr->data.struct_literal.type_args;
      size_t type_arg_count = expr->data.struct_literal.type_arg_count;
      if (type_args) {
        // Explicit generic instantiation 'GenericTypeName.[...]{ ... }'
        AstNode *generic_decl = type_of->data.generic_decl.decl;

        // Monomorphize the struct type
        type_of = monomorphize_struct_type(generic_decl, type_args,
                                           type_arg_count, expr->loc);
        if (!type_of)
          return NULL;
      } else {
        // Error: GenericStruct.{ ... }
        checker_error(expr->loc,
                      "generic type '%s' cannot be instantiated without "
                      "explicit type arguments. use an anonymous struct "
                      "'.{...}' if the type is known",
                      struct_type_name);
        return NULL;
      }
    }

    if (type_of->kind != TYPE_STRUCT && type_of->kind != TYPE_UNION &&
        type_of->kind != TYPE_TAGGED_UNION) {
      checker_error(expr->loc, "'%s' is not a struct or union type",
                    struct_type_name);
      return NULL;
    }

    if (type_of->qualified_name) {
      // The name got qualified, so we should update the reference to it
      expr->data.struct_literal.qualified_type_name = type_of->qualified_name;
    }

    switch (type_of->kind) {
    case TYPE_STRUCT: {
      Type *struct_type = type_of;

      // Verify field count matches
      size_t expected_count = struct_type->data.struct_data.field_count;
      if (field_count != expected_count) {
        checker_error(
            expr->loc,
            "struct literal has %zu field(s), but type '%s' has %zu field(s)",
            field_count, type_name(type_of), expected_count);
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

            AstNode *converted_init = maybe_insert_cast(
                field_values[i], value_type, expected_types[j]);

            if (!converted_init) {
              checker_error(
                  expr->loc,
                  "field '%s' has initializer type mismatch '%s' != '%s'",
                  field_names[i], type_name(expected_types[j]),
                  type_name(value_type));
              return NULL;
            }

            break;
          }
        }

        if (!found) {
          checker_error(expr->loc, "struct '%s' has no field named '%s'",
                        type_name(struct_type), field_names[i]);
          return NULL;
        }
      }

      break;
    }

    case TYPE_UNION:
    case TYPE_TAGGED_UNION: {
      Type *union_type = type_of;

      // Union can only have 1 active field (need to check empty union too)
      if (field_count > 1) {
        checker_error(expr->loc,
                      "union literal has %zu field(s), but type '%s' can only "
                      "have 0 or 1 active variant",
                      field_count, type_name(type_of));
        return NULL;
      }

      size_t expected_count = union_type->data.union_data.variant_count;
      // Variants and no field, or no variants and 1 field
      if (expected_count == 0 && field_count == 1) {
        checker_error(expr->loc,
                      "union literal has %zu field(s), but type '%s' can only "
                      "have 0 or 1 active variant",
                      field_count, type_name(type_of));
        return NULL;
      }

      // Check each field
      size_t variant_count = union_type->data.union_data.variant_count;
      char **variant_names = union_type->data.union_data.variant_names;
      Type **variant_types = union_type->data.union_data.variant_types;

      int index_of_active_member = -1;

      for (size_t i = 0; i < field_count; i++) {
        if (index_of_active_member >= 0) {
          // Member found
          break;
        }

        // Find variant to set
        for (size_t j = 0; j < variant_count; j++) {
          if (strcmp(field_names[i], variant_names[j]) == 0) {
            index_of_active_member = j;

            // Type-check the value
            Type *value_type = check_expression(field_values[i]);
            if (!value_type) {
              return NULL; // Error already reported
            }

            AstNode *converted_init = maybe_insert_cast(
                field_values[i], value_type, variant_types[j]);

            if (!converted_init) {
              checker_error(
                  expr->loc,
                  "field '%s' has initializer type mismatch '%s' != '%s'",
                  field_names[i], type_name(variant_types[j]),
                  type_name(value_type));
              return NULL;
            }

            break;
          }
        }
      }

      if (index_of_active_member == -1 && field_count > 0) {
        // No member found or is invalid
        checker_error(expr->loc, "struct '%s' has no field named '%s'",
                      type_name, variant_names[0]);
        return NULL;
      }

      break;
    }

    default:
      break;
    }

    expr->resolved_type = type_of;
    return type_of;
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
    // If this anonymous function already has a symbol, it's already been
    // checked (Happens when checking monomorphized functions that were cloned)
    if (expr->data.func_expr.symbol != NULL) {
      // Already checked - just return its type
      if (expr->resolved_type) {
        return expr->resolved_type;
      }
      // Look up the existing symbol
      Symbol *existing =
          scope_lookup_local(anonymous_funcs, expr->data.func_expr.symbol);
      if (existing) {
        expr->resolved_type = existing->type;
        return existing->type;
      }
    }

    // No generic anonymous functions yet
    if (expr->data.func_expr.type_param_count > 0) {
      checker_error(expr->loc,
                    "generic anonymous functions are not yet supported");
      return NULL;
    }

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

    if ((value_type->kind == TYPE_STRUCT && target_type->kind == TYPE_STRUCT) ||
        (value_type->kind == TYPE_TUPLE && target_type->kind == TYPE_TUPLE)) {
      expr->data.explicit_cast.pointer_cast = true;
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
      // Return value is expected
      if (expected_return_type->kind != TYPE_VOID) {
        checker_error(stmt->loc, "return value is expected with type '%s'",
                      type_name(expected_return_type));
      }

      return true; // Error already reported
    }

    // Verify it matches expected return type (with implicit cast if needed)
    AstNode *converted =
        maybe_insert_cast(expr, expr_type, expected_return_type);
    if (!converted) {
      checker_error(stmt->loc, "return type mismatch '%s' != '%s'",
                    type_name(expr_type), type_name(expected_return_type));
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
    if (cond_type) {
      if (cond_type->kind == TYPE_BOOL) {
        checker_error(cond->loc, "switch cases cannot be used with boolean "
                                 "types. please use if statements instead.");
        had_error = true;
      }

      if (!type_is_integral(cond_type) && cond_type->kind != TYPE_STRING &&
          cond_type->kind != TYPE_CHAR && cond_type->kind != TYPE_ENUM &&
          cond_type->kind != TYPE_TAGGED_UNION &&
          // Allow pointers to tagged unions
          !(cond_type->kind == TYPE_POINTER &&
            cond_type->data.ptr.base->kind != TYPE_TAGGED_UNION)) {
        checker_error(cond->loc,
                      "switch condition must be integral, "
                      "char, enum, string or tagged union. Got '%s'.",
                      type_name(cond_type));
        had_error = true;
      }

      bool cases_return = true;

      for (size_t i = 0; i < stmt->data.switch_stmt.case_count; i++) {
        if (!check_statement(cases[i], expected_return_type)) {
          cases_return = false;
        }
      }

      if (!had_error) {
        check_switch_is_exhaustive(stmt, cond->resolved_type);
      }

      bool else_returns =
          default_case ? check_statement(default_case, expected_return_type)
                       : false;

      return cases_return && else_returns;
    }

    return false;
  }

  case AST_STMT_CASE: {
    AstNode *cond = stmt->data.case_stmt.condition;
    AstNode *switch_cond =
        stmt->data.case_stmt.switch_stmt->data.switch_stmt.condition;

    bool is_tagged_union =
        switch_cond->resolved_type->kind == TYPE_TAGGED_UNION ||
        (switch_cond->resolved_type->kind == TYPE_POINTER &&
         switch_cond->resolved_type->data.ptr.base->kind == TYPE_TAGGED_UNION);

    Type *cond_type = NULL;
    // FIXME: Allow TypeName.variant like enums
    if (!is_tagged_union && cond->kind != AST_EXPR_PARTIAL_MEMBER) {
      cond_type = check_expression(cond);
      AstNode *casted_cond =
          maybe_insert_cast(cond, cond_type, switch_cond->resolved_type);

      Type *resolved = casted_cond && casted_cond->resolved_type
                           ? casted_cond->resolved_type
                           : cond_type;

      if (!type_equals(switch_cond->resolved_type, resolved)) {
        checker_error(cond->loc,
                      "switch case condition '%s' doesn't match switch "
                      "condition type '%s'",
                      type_name(switch_cond->resolved_type),
                      type_name(resolved));
      }

      cond->resolved_type = resolved;
    } else {
      // Tagged union/partial member will assume type
      cond->resolved_type = switch_cond->resolved_type;
    }

    // Check that condition is constant
    if (!is_constant_known(cond)) {
      checker_error(cond->loc, "switch case condition must be a constant");
    }

    if (stmt->data.case_stmt.alt_condition_count > 0) {
      for (size_t i = 0; i < stmt->data.case_stmt.alt_condition_count; i++) {
        check_statement(stmt->data.case_stmt.alt_conditions[i],
                        expected_return_type);
      }
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
    if (!end_type) {
      return NULL;
    }

    if (!type_is_integral(end_type)) {
      checker_error(end->loc, "loop range end must be an integer");
      return NULL;
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
      Scope *mod_scope = checker_state.current_module->scope;
      Symbol *sym = scope_lookup(mod_scope, current_scope, lhs->data.ident.name,
                                 lhs->loc.file);

      if (!sym) {
        checker_error(lhs->loc, "'%s' is not defined", lhs->data.ident.name);
        return false;
      }

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
      // printf("%s vs %s\n", lhs_type->canonical_name,
      // rhs_type->canonical_name);
      AstNode *converted = maybe_insert_cast(rhs, rhs_type, lhs_type);
      if (!converted) {
        checker_error(stmt->loc, "assignment type mismatch, '%s' != '%s'",
                      type_name(lhs_type), type_name(rhs_type));
      } else {
        stmt->data.assign_stmt.rhs =
            converted; // Replace with cast node if needed
      }
    }
    return false;
  }

  case AST_EXPR_POSTFIX_INC: {
    // We allow checking postfix ++ as a stmt because of
    // of for loop update statement
    AstNode *lhs = stmt->data.postfix_inc.operand;
    Type *lhs_ty = check_expression(lhs);
    lhs->resolved_type = lhs_ty;
    return false;
  }

  case AST_EXPR_POSTFIX_DEC: {
    // We allow checking postfix -- as a stmt because
    // of for loop update statement
    AstNode *lhs = stmt->data.postfix_dec.operand;
    Type *lhs_ty = check_expression(lhs);
    lhs->resolved_type = lhs_ty;
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
        checker_error(value->loc,
                      "constant initializer type mismatch '%s' != '%s'",
                      type_name(const_type), type_name(value_type));
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
// Extract the body checking logic for a single function
static bool check_function_body(Symbol *sym) {
  if (!sym->type) {
    return false;
  }

  // Skip generic functions - they'll be checked when monomorphized
  if (sym->type && sym->type->kind == TYPE_GENERIC_FUNCTION) {
    return true;
  }

  AstNode *decl = sym->decl;
  AstNode *body = decl->data.func_decl.body;
  Type *func_type = sym->type;
  Type *return_type = func_type->data.func.return_type;

  // Push function's local scope (contains parameters)
  scope_push(sym->data.func.local_scope);

  checker_state.current_convention = func_type->data.func.convention;

  // Check the function body
  bool had_error = checker_state.has_errors;
  checker_state.has_errors = false;
  bool definitely_returns = check_statement(body, return_type);
  bool has_error = checker_state.has_errors;
  checker_state.has_errors = had_error;

  // Pop back to global scope
  scope_pop();

  // If non-void, ensure all paths return
  if (return_type->kind != TYPE_VOID && !definitely_returns) {
    checker_error(decl->loc,
                  "function '%s' may not return a value on all paths",
                  sym->decl->data.func_decl.name);
    return false;
  }

  return !has_error;
}

// Now refactor the original function to use it
bool check_function_bodies(void) {
  Symbol *sym, *tmp;

  bool no_error = true;
  // Iterate over all function symbols in global scope
  HASH_ITER(hh, checker_state.current_module->scope->symbols, sym, tmp) {
    // Only process functions
    if (sym->kind != SYMBOL_FUNCTION || sym->is_mono) {
      continue;
    }

    if (no_error) {
      no_error = check_function_body(sym);
    }
  }

  return no_error;
}

// Verify that the entry point exists and has the correct signature
// - If entry_point is "main": must exist with signature (void) -> int OR
// (int,
// []str) -> int
// - If entry_point is NOT "main": must exist with signature () -> void
// - If --no-main is set: skip verification entirely
bool verify_entry_point(Module *main_mod) {
  const char *entry_name = compiler_opts.entry_point;
  bool is_main = strcmp(entry_name, "main") == 0;

  // Look up the entry point function in global scope
  Symbol *entry_sym = scope_lookup_local(main_mod->scope, entry_name);

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
