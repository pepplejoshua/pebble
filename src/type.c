#include "type.h"
#include "alloc.h"
#include "ast.h"
#include "module.h"
// #include "wrapped_uthash.h"
#include "uthash.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External allocators
extern Arena long_lived;

// Built-in type globals
Type *type_int = NULL;
Type *type_bool = NULL;
Type *type_string = NULL;
Type *type_void = NULL;
Type *type_f32 = NULL;
Type *type_f64 = NULL;
Type *type_u8 = NULL;
Type *type_u16 = NULL;
Type *type_u32 = NULL;
Type *type_u64 = NULL;
Type *type_usize = NULL;
Type *type_i8 = NULL;
Type *type_i16 = NULL;
Type *type_i32 = NULL;
Type *type_i64 = NULL;
Type *type_isize = NULL;
Type *type_char = NULL;
Type *type_none = NULL;
Type *type_context = NULL;

// Type table (hash map of name => type entries)
TypeEntry *type_table = NULL;

// Canonical type table (hash map of canonical_name => type entries)
TypeEntry *canonical_type_table = NULL;

// Monomorphization table (hash map of mangled_name => MonoFuncInstance)
MonoFuncInstance *mono_instances = NULL;

// Create a basic type
Type *type_create(TypeKind kind, Location loc) {
  Type *type = arena_alloc(&long_lived, sizeof(Type));
  memset(type, 0, sizeof(Type));
  type->kind = kind;
  type->loc = loc;
  type->used = false;
  return type;
}

// Create pointer type (conditional canonicalization)
Type *type_create_pointer(Type *base, bool canonicalize, Location loc) {
  assert(base);

  if (canonicalize) {
    // Create temp pointer type to compute canonical name
    Type temp = {
        .kind = TYPE_POINTER, .data.ptr.base = base, .canonical_name = NULL};
    char *canonical_name = compute_canonical_name(&temp);
    Type *existing = canonical_lookup(canonical_name);
    if (existing)
      return existing;

    Type *type = arena_alloc(&long_lived, sizeof(Type));
    type->kind = TYPE_POINTER;
    type->data.ptr.base = base;
    type->canonical_name = canonical_name;
    canonical_register(canonical_name, type);
    return type;
  } else {
    // Just create type without canonicalization
    Type *type = type_create(TYPE_POINTER, loc);
    type->data.ptr.base = base;
    return type;
  }
}

// Create pointer type (conditional canonicalization)
Type *type_create_optional(Type *base, bool canonicalize, Location loc) {
  assert(base);

  if (canonicalize) {
    // Create temp pointer type to compute canonical name
    Type temp = {.kind = TYPE_OPTIONAL,
                 .data.optional.base = base,
                 .canonical_name = NULL};
    char *canonical_name = compute_canonical_name(&temp);
    Type *existing = canonical_lookup(canonical_name);
    if (existing)
      return existing;

    Type *type = arena_alloc(&long_lived, sizeof(Type));
    type->kind = TYPE_OPTIONAL;
    type->data.optional.base = base;
    type->canonical_name = canonical_name;
    canonical_register(canonical_name, type);
    return type;
  } else {
    // Just create type without canonicalization
    Type *type = type_create(TYPE_OPTIONAL, loc);
    type->data.optional.base = base;
    return type;
  }
}

// Create slice type (with deduplication)
Type *type_create_slice(Type *element, bool canonicalize, Location loc) {
  assert(element);

  if (canonicalize) {
    Type temp = {.kind = TYPE_SLICE,
                 .data.slice.element = element,
                 .canonical_name = NULL};

    char *canonical_name = compute_canonical_name(&temp);

    Type *existing = canonical_lookup(canonical_name);
    if (existing)
      return existing;

    Type *type = arena_alloc(&long_lived, sizeof(Type));
    *type = temp;
    type->canonical_name = canonical_name;
    canonical_register(canonical_name, type);
    return type;
  } else {
    Type *type = type_create(TYPE_SLICE, loc);
    type->data.slice.element = element;
    return type;
  }
}

// Create array type (with deduplication)
Type *type_create_array(Type *element, size_t size, bool canonicalize,
                        Location loc) {
  assert(element);

  if (canonicalize) {
    Type temp = {.kind = TYPE_ARRAY,
                 .data.array.element = element,
                 .data.array.size = size,
                 .canonical_name = NULL};

    char *canonical_name = compute_canonical_name(&temp);

    Type *existing = canonical_lookup(canonical_name);
    if (existing)
      return existing;

    Type *type = arena_alloc(&long_lived, sizeof(Type));
    *type = temp;
    type->canonical_name = canonical_name;
    canonical_register(canonical_name, type);
    return type;
  } else {
    Type *type = type_create(TYPE_ARRAY, loc);
    type->data.array.element = element;
    type->data.array.size = size;
    return type;
  }
}

// Create struct type
Type *type_create_struct(char **field_names, Type **field_types,
                         size_t field_count, bool builtin, bool canonicalize,
                         Location loc) {
  if (canonicalize) {
    // First create a temporary type to compute canonical name
    Type *temp_type = arena_alloc(&long_lived, sizeof(Type));
    temp_type->kind = TYPE_STRUCT;
    temp_type->data.struct_data.builtin = builtin;

    char **names = NULL;
    Type **types = NULL;

    if (field_count > 0) {
      // Duplicate field names into arena
      names = arena_alloc(&long_lived, field_count * sizeof(char *));
      for (size_t i = 0; i < field_count; i++) {
        names[i] = str_dup(field_names[i]);
      }

      // Copy field types array
      types = arena_alloc(&long_lived, field_count * sizeof(Type *));
      memcpy(types, field_types, field_count * sizeof(Type *));
    }

    temp_type->data.struct_data.field_names = names;
    temp_type->data.struct_data.field_types = types;
    temp_type->data.struct_data.field_count = field_count;

    // Compute canonical name (this requires element types to be canonicalized
    // first) For now, assume element types are already canonicalized
    char *canonical_name = compute_canonical_name(temp_type);

    // Check if this type already exists
    Type *existing = canonical_lookup(canonical_name);
    if (existing) {
      // Return existing type, discard temp
      return existing;
    }

    // First time - finalize the type
    Type *type = temp_type;
    type->canonical_name = canonical_name;
    canonical_register(canonical_name, type);
    return type;
  } else {
    Type *type = type_create(TYPE_STRUCT, loc);
    type->data.struct_data.builtin = builtin;

    if (field_count == 0) {
      type->data.struct_data.field_count = field_count;
      return type;
    }

    // Duplicate field names into arena
    char **names = arena_alloc(&long_lived, field_count * sizeof(char *));
    for (size_t i = 0; i < field_count; i++) {
      names[i] = str_dup(field_names[i]);
    }

    // Copy field types array
    Type **types = arena_alloc(&long_lived, field_count * sizeof(Type *));
    memcpy(types, field_types, field_count * sizeof(Type *));

    type->data.struct_data.field_names = names;
    type->data.struct_data.field_types = types;
    type->data.struct_data.field_count = field_count;
    return type;
  }
}

// Create union type
Type *type_create_union(bool tagged, char **variant_names, Type **variant_types,
                        size_t variant_count, bool canonicalize, Location loc) {
  if (canonicalize) {
    // First create a temporary type to compute canonical name
    Type *temp_type = arena_alloc(&long_lived, sizeof(Type));
    temp_type->kind = tagged ? TYPE_TAGGED_UNION : TYPE_UNION;

    char **names = NULL;
    Type **types = NULL;

    if (variant_count > 0) {
      // Duplicate field names into arena
      names = arena_alloc(&long_lived, variant_count * sizeof(char *));
      for (size_t i = 0; i < variant_count; i++) {
        names[i] = str_dup(variant_names[i]);
      }

      // Copy field types array
      types = arena_alloc(&long_lived, variant_count * sizeof(Type *));
      memcpy(types, variant_types, variant_count * sizeof(Type *));
    }

    temp_type->data.union_data.variant_names = names;
    temp_type->data.union_data.variant_types = types;
    temp_type->data.union_data.variant_count = variant_count;

    // Compute canonical name (this requires element types to be canonicalized
    // first) For now, assume element types are already canonicalized
    char *canonical_name = compute_canonical_name(temp_type);

    // Check if this type already exists
    Type *existing = canonical_lookup(canonical_name);
    if (existing) {
      // Return existing type, discard temp
      return existing;
    }

    // First time - finalize the type
    Type *type = temp_type;
    type->canonical_name = canonical_name;
    canonical_register(canonical_name, type);
    return type;
  } else {
    Type *type = type_create(tagged ? TYPE_TAGGED_UNION : TYPE_UNION, loc);
    if (variant_count == 0) {
      type->data.union_data.variant_count = variant_count;
      return type;
    }

    // Duplicate field names into arena
    char **names = arena_alloc(&long_lived, variant_count * sizeof(char *));
    for (size_t i = 0; i < variant_count; i++) {
      names[i] = str_dup(variant_names[i]);
    }

    // Copy field types array
    Type **types = arena_alloc(&long_lived, variant_count * sizeof(Type *));
    memcpy(types, variant_types, variant_count * sizeof(Type *));

    type->data.union_data.variant_names = names;
    type->data.union_data.variant_types = types;
    type->data.union_data.variant_count = variant_count;
    return type;
  }
}

// Create enum type
Type *type_create_enum(char **variant_names, size_t variant_count,
                       bool canonicalize, Location loc) {
  if (canonicalize) {
    Type *temp_type = type_create(TYPE_ENUM, loc);

    // Duplicate field names into arena
    char **names = arena_alloc(&long_lived, variant_count * sizeof(char *));
    for (size_t i = 0; i < variant_count; i++) {
      names[i] = str_dup(variant_names[i]);
    }

    temp_type->data.enum_data.variant_names = names;
    temp_type->data.enum_data.variant_count = variant_count;

    // Compute canonical name (this requires element types to be canonicalized
    // first) For now, assume element types are already canonicalized
    char *canonical_name = compute_canonical_name(temp_type);

    // Check if this type already exists
    Type *existing = canonical_lookup(canonical_name);
    if (existing) {
      // Return existing type, discard temp
      return existing;
    }

    // First time - finalize the type
    Type *type = temp_type;
    type->canonical_name = canonical_name;
    canonical_register(canonical_name, type);
    return type;
  } else {
    Type *type = type_create(TYPE_ENUM, loc);

    if (variant_count == 0) {
      type->data.enum_data.variant_count = variant_count;
      return type;
    }

    // Duplicate field names into arena
    char **names = arena_alloc(&long_lived, variant_count * sizeof(char *));
    for (size_t i = 0; i < variant_count; i++) {
      names[i] = str_dup(variant_names[i]);
    }

    type->data.enum_data.variant_names = names;
    type->data.enum_data.variant_count = variant_count;
    return type;
  }
}

// Create tuple type (no caching)
// Create tuple type (with deduplication)
Type *type_create_tuple(Type **element_types, size_t element_count,
                        bool canonicalize, Location loc) {
  assert(element_types && element_count > 0);

  if (canonicalize) {
    // First create a temporary type to compute canonical name
    Type *temp_type = arena_alloc(&long_lived, sizeof(Type));
    temp_type->kind = TYPE_TUPLE;
    temp_type->data.tuple.element_types = element_types;
    temp_type->data.tuple.element_count = element_count;

    // Compute canonical name (this requires element types to be canonicalized
    // first) For now, assume element types are already canonicalized
    char *canonical_name = compute_canonical_name(temp_type);

    // Check if this type already exists
    Type *existing = canonical_lookup(canonical_name);
    if (existing) {
      // Return existing type, discard temp
      return existing;
    }

    // First time - finalize the type
    Type *type = temp_type;
    type->canonical_name = canonical_name;
    canonical_register(canonical_name, type);
    return type;
  } else {
    Type *type = type_create(TYPE_TUPLE, loc);
    Type **types = arena_alloc(&long_lived, element_count * sizeof(Type *));
    memcpy(types, element_types, element_count * sizeof(Type *));
    type->data.tuple.element_types = types;
    type->data.tuple.element_count = element_count;
    return type;
  }
}

// Create function type (no caching)
// Create function type (with deduplication)
Type *type_create_function(Type **param_types, size_t param_count,
                           Type *return_type, bool is_variadic,
                           bool canonicalize, CallingConvention convention,
                           Location loc) {
  assert(return_type);

  if (canonicalize) {
    Type temp = {.kind = TYPE_FUNCTION,
                 .data.func.convention = convention,
                 .data.func.param_types = param_types,
                 .data.func.param_count = param_count,
                 .data.func.return_type = return_type,
                 .data.func.is_variadic = is_variadic,
                 .canonical_name = NULL};

    char *canonical_name = compute_canonical_name(&temp);

    Type *existing = canonical_lookup(canonical_name);
    if (existing)
      return existing;

    Type *type = arena_alloc(&long_lived, sizeof(Type));
    *type = temp;
    type->canonical_name = canonical_name;
    canonical_register(canonical_name, type);
    return type;
  } else {
    Type *type = type_create(TYPE_FUNCTION, loc);
    if (param_count > 0) {
      assert(param_types);
      Type **types = arena_alloc(&long_lived, param_count * sizeof(Type *));
      memcpy(types, param_types, param_count * sizeof(Type *));
      type->data.func.param_types = types;
    }
    type->data.func.convention = convention;
    type->data.func.is_variadic = is_variadic;
    type->data.func.param_count = param_count;
    type->data.func.return_type = return_type;
    return type;
  }
}

// Look up named type in type table
Type *type_lookup(const char *name, const char *mod_name) {
  if (!name)
    return NULL;

  TypeEntry *entry;
  HASH_FIND_STR(type_table, name, entry);
  if (!entry) {
    // For qualified type names
    char *prefix = prepend(get_basename(mod_name, true), "__");
    char *qualified_name = prepend(prefix, name);
    HASH_FIND_STR(type_table, qualified_name, entry);
  }
  return entry ? entry->type : NULL;
}

// Register a named type in type table
void type_register(const char *name, Type *type) {
  assert(name && type);

  TypeEntry *entry = arena_alloc(&long_lived, sizeof(TypeEntry));
  entry->name = str_dup(name);
  entry->type = type;
  HASH_ADD_STR(type_table, name, entry);
}

// Register a named type in canonical type table
void canonical_register(const char *canonical_name, Type *type) {
  assert(canonical_name && type);

  TypeEntry *entry = arena_alloc(&long_lived, sizeof(TypeEntry));
  entry->name = str_dup(canonical_name); // Duplicate canonical name
  entry->type = type;
  HASH_ADD_STR(canonical_type_table, name, entry);
}

// Look up canonical type
Type *canonical_lookup(const char *canonical_name) {
  if (!canonical_name)
    return NULL;

  TypeEntry *entry;
  HASH_FIND_STR(canonical_type_table, canonical_name, entry);
  return entry ? entry->type : NULL;
}

// Check if two types are equal (using canonical names)
bool type_equals(Type *a, Type *b) {
  if (a == b)
    return true; // Same pointer
  if (!a || !b)
    return false;

  // Function comparisons
  if (a->kind == TYPE_FUNCTION && b->kind == TYPE_FUNCTION) {
    // Incorrect calling convention
    if (a->data.func.convention != b->data.func.convention) {
      return false;
    }

    // Incorrect arity
    if (a->data.func.param_count != b->data.func.param_count) {
      return false;
    }

    // Compare parameter types
    for (size_t i = 0; i < a->data.func.param_count; i++) {
      if (!type_equals(a->data.func.param_types[i],
                       b->data.func.param_types[i])) {
        return false;
      }
    }

    // Compare return types
    return type_equals(a->data.func.return_type, b->data.func.return_type);
  }

  if (!a->canonical_name || !b->canonical_name) {
    // Fallback to pointer equality if canonical names not set yet
    // (shouldn't happen after canonicalization, but safety check)
    return false;
  }

  // Compare canonical names
  return strcmp(a->canonical_name, b->canonical_name) == 0;
}

// Check if type is numeric (int or float)
bool type_is_numeric(Type *type) {
  return type && (type->kind == TYPE_INT || type->kind == TYPE_U8 ||
                  type->kind == TYPE_U16 || type->kind == TYPE_U32 ||
                  type->kind == TYPE_U64 || type->kind == TYPE_USIZE ||
                  type->kind == TYPE_I8 || type->kind == TYPE_I16 ||
                  type->kind == TYPE_I32 || type->kind == TYPE_I64 ||
                  type->kind == TYPE_ISIZE || type->kind == TYPE_F32 ||
                  type->kind == TYPE_F64);
}

bool type_is_ord(Type *type) {
  return type_is_numeric(type) || type->kind == TYPE_ENUM;
}

// Initialize the type system
void type_system_init(void) {
  Location loc = {
      "builtin",
      0,
      0,
  };
  // Create built-in types
  type_int = type_create(TYPE_INT, loc);
  type_bool = type_create(TYPE_BOOL, loc);
  type_string = type_create(TYPE_STRING, loc);
  type_void = type_create(TYPE_VOID, loc);
  type_f32 = type_create(TYPE_F32, loc);
  type_f64 = type_create(TYPE_F64, loc);
  type_u8 = type_create(TYPE_U8, loc);
  type_u16 = type_create(TYPE_U16, loc);
  type_u32 = type_create(TYPE_U32, loc);
  type_u64 = type_create(TYPE_U64, loc);
  type_usize = type_create(TYPE_USIZE, loc);
  type_i8 = type_create(TYPE_I8, loc);
  type_i16 = type_create(TYPE_I16, loc);
  type_i32 = type_create(TYPE_I32, loc);
  type_i64 = type_create(TYPE_I64, loc);
  type_isize = type_create(TYPE_ISIZE, loc);
  type_char = type_create(TYPE_CHAR, loc);
  type_none = type_create(TYPE_NONE, loc);

  // Set canonical names for built-in types
  type_int->canonical_name = "int";
  type_bool->canonical_name = "bool";
  type_string->canonical_name = "str";
  type_void->canonical_name = "void";
  type_f32->canonical_name = "f32";
  type_f64->canonical_name = "f64";
  type_u8->canonical_name = "u8";
  type_u16->canonical_name = "u16";
  type_u32->canonical_name = "u32";
  type_u64->canonical_name = "u64";
  type_usize->canonical_name = "usize";
  type_i8->canonical_name = "i8";
  type_i16->canonical_name = "i16";
  type_i32->canonical_name = "i32";
  type_i64->canonical_name = "i64";
  type_isize->canonical_name = "isize";
  type_char->canonical_name = "char";
  type_none->canonical_name = "none";

  // Register built-in types in type table
  type_register("int", type_int);
  type_register("bool", type_bool);
  type_register("str", type_string);
  type_register("void", type_void);
  type_register("f32", type_f32);
  type_register("f64", type_f64);
  type_register("u8", type_u8);
  type_register("u16", type_u16);
  type_register("u32", type_u32);
  type_register("u64", type_u64);
  type_register("usize", type_usize);
  type_register("i8", type_i8);
  type_register("i16", type_i16);
  type_register("i32", type_i32);
  type_register("i64", type_i64);
  type_register("isize", type_isize);
  type_register("char", type_char);

  // Create types for context and allocator
  Type *void_ptr = type_create_pointer(type_void, true, loc);

  // NOTE: Context needs to be a dummy type, then patched
  //       after allocator as it requires it.
  type_context = type_create_struct(NULL, NULL, 0, true, false, loc);
  type_context->canonical_name = "__pebble_context";

  Type **alloc_param_types = arena_alloc(&long_lived, 2 * sizeof(Type *));
  alloc_param_types[0] = void_ptr;
  alloc_param_types[1] = type_usize;
  Type *alloc_fn_t = type_create_function(alloc_param_types, 2, void_ptr, false,
                                          false, CALL_CONV_PEBBLE, loc);

  Type **realloc_param_types = arena_alloc(&long_lived, 3 * sizeof(Type *));
  realloc_param_types[0] = void_ptr;
  realloc_param_types[1] = void_ptr;
  realloc_param_types[2] = type_usize;
  Type *realloc_fn_t = type_create_function(
      realloc_param_types, 3, void_ptr, false, false, CALL_CONV_PEBBLE, loc);

  Type **free_param_types = arena_alloc(&long_lived, 2 * sizeof(Type *));
  free_param_types[0] = void_ptr;
  free_param_types[1] = void_ptr;
  Type *free_fn_t = type_create_function(free_param_types, 2, type_void, false,
                                         false, CALL_CONV_PEBBLE, loc);

  char **allocator_field_names = arena_alloc(&long_lived, 4 * sizeof(char *));
  allocator_field_names[0] = "ptr";
  allocator_field_names[1] = "alloc";
  allocator_field_names[2] = "realloc";
  allocator_field_names[3] = "free";

  Type **allocator_types = arena_alloc(&long_lived, 4 * sizeof(Type *));
  allocator_types[0] = void_ptr;
  allocator_types[1] = alloc_fn_t;
  allocator_types[2] = realloc_fn_t;
  allocator_types[3] = free_fn_t;
  Type *allocator_t = type_create_struct(allocator_field_names, allocator_types,
                                         4, true, false, loc);
  allocator_t->canonical_name = "Allocator";

  // Patch context
  char **context_field_names = arena_alloc(&long_lived, 1 * sizeof(char *));
  context_field_names[0] = "default_allocator";

  Type **context_types = arena_alloc(&long_lived, 1 * sizeof(Type *));
  context_types[0] = allocator_t;

  type_context->data.struct_data.field_names = context_field_names;
  type_context->data.struct_data.field_types = context_types;
  type_context->data.struct_data.field_count = 1;

  // Register all needed types
  type_register("Allocator", allocator_t);
  type_register("__pebble_context", type_context);

  // Register built-ins in canonical type table
  canonical_register("int", type_int);
  canonical_register("bool", type_bool);
  canonical_register("str", type_string);
  canonical_register("void", type_void);
  canonical_register("f32", type_f32);
  canonical_register("f64", type_f64);
  canonical_register("u8", type_u8);
  canonical_register("u16", type_u16);
  canonical_register("u32", type_u32);
  canonical_register("u64", type_u64);
  canonical_register("usize", type_usize);
  canonical_register("i8", type_i8);
  canonical_register("i16", type_i16);
  canonical_register("i32", type_i32);
  canonical_register("i64", type_i64);
  canonical_register("isize", type_isize);
  canonical_register("char", type_char);
  canonical_register("Allocator", allocator_t);
  canonical_register("__pebble_context", type_context);
}

// Compute canonical name for a type (with cycle detection)
char *compute_canonical_name(Type *type) {
  assert(type);

  if (type->kind == TYPE_UNRESOLVED) {
    // This is a placeholder — return a temporary name so recursion doesn't
    // crash
    return "__UNRESOLVED__";
  }

  char *result = NULL;

  switch (type->kind) {
  case TYPE_INT:
  case TYPE_BOOL:
  case TYPE_STRING:
  case TYPE_VOID:
  case TYPE_F32:
  case TYPE_F64:
  case TYPE_U8:
  case TYPE_U16:
  case TYPE_U32:
  case TYPE_U64:
  case TYPE_USIZE:
  case TYPE_I8:
  case TYPE_I16:
  case TYPE_I32:
  case TYPE_I64:
  case TYPE_ISIZE:
  case TYPE_CHAR:
  case TYPE_OPAQUE:
  case TYPE_NONE:
    result = type->canonical_name;
    break;

  case TYPE_GENERIC_FUNCTION:
    assert(false && "Attempt to compute canonical name on a generic function.");
    break;

  case TYPE_GENERIC_TYPE_DECL:
    assert(false && "Attempt to compute canonical name on a generic type.");
    break;

  case TYPE_POINTER: {
    char *base_name = compute_canonical_name(type->data.ptr.base);
    size_t len = strlen("ptr_") + strlen(base_name) + 1;
    result = arena_alloc(&long_lived, len);
    snprintf(result, len, "ptr_%s", base_name);
    break;
  }

  case TYPE_OPTIONAL: {
    char *base_name = compute_canonical_name(type->data.ptr.base);
    size_t len = strlen("optional_") + strlen(base_name) + 1;
    result = arena_alloc(&long_lived, len);
    snprintf(result, len, "optional_%s", base_name);
    break;
  }

  case TYPE_ARRAY: {
    char *elem_name = compute_canonical_name(type->data.array.element);
    size_t len = strlen("array_") + 10 + 1 + strlen(elem_name) + 1;
    result = arena_alloc(&long_lived, len);
    snprintf(result, len, "array_%zu_%s", type->data.array.size, elem_name);
    break;
  }

  case TYPE_SLICE: {
    char *elem_name = compute_canonical_name(type->data.slice.element);
    size_t len = strlen("slice_") + strlen(elem_name) + 1;
    result = arena_alloc(&long_lived, len);
    snprintf(result, len, "slice_%s", elem_name);
    break;
  }

  case TYPE_TUPLE: {
    // Compute all element names
    char **elem_names = arena_alloc(
        &long_lived, type->data.tuple.element_count * sizeof(char *));
    size_t total_len = strlen("tuple_");

    for (size_t i = 0; i < type->data.tuple.element_count; i++) {
      elem_names[i] = compute_canonical_name(type->data.tuple.element_types[i]);
      total_len += strlen(elem_names[i]) + 1;
    }
    total_len--; // Remove last "_"

    result = arena_alloc(&long_lived, total_len + 1);
    strcpy(result, "tuple_");
    for (size_t i = 0; i < type->data.tuple.element_count; i++) {
      if (i > 0)
        strcat(result, "_");
      strcat(result, elem_names[i]);
    }
    break;
  }

  case TYPE_ENUM: {
    if (!type->canonical_name) {
      size_t capacity = 256;
      char *canonical_name = arena_alloc(&long_lived, capacity);
      size_t offset = 0;

      offset += snprintf(canonical_name + offset, capacity - offset, "enum");

      for (size_t i = 0; i < type->data.enum_data.variant_count; i++) {
        char *field_name = type->data.enum_data.variant_names[i];

        size_t needed = offset + 1 + strlen(field_name) + 1;
        if (needed > capacity) {
          capacity = needed * 2;
          char *new_buf = arena_alloc(&long_lived, capacity);
          memcpy(new_buf, canonical_name, offset);
          canonical_name = new_buf;
        }

        offset += snprintf(canonical_name + offset, capacity - offset, "_%s",
                           field_name);

        canonical_register(canonical_name, type);
      }

      type->canonical_name = canonical_name;
    }
    result = type->canonical_name;
    break;
  }

  case TYPE_UNION:
  case TYPE_TAGGED_UNION: {
    // Named unions already have canonical_name set (nominal)
    if (!type->canonical_name) {
      size_t capacity = 256;
      char *canonical_name = arena_alloc(&long_lived, capacity);
      size_t offset = 0;

      offset += snprintf(canonical_name + offset, capacity - offset, "union");

      for (size_t i = 0; i < type->data.union_data.variant_count; i++) {
        char *field_name = type->data.union_data.variant_names[i];
        char *type_name =
            type->data.union_data.variant_types[i]->canonical_name;

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

        canonical_register(canonical_name, type);
      }

      type->canonical_name = canonical_name;
    }
    result = type->canonical_name;
    break;
  }

  case TYPE_STRUCT: {
    // Named structs already have canonical_name set (nominal)
    if (!type->canonical_name) {
      // Anonymous struct - build structural name
      size_t capacity = 256;
      char *canonical_name = arena_alloc(&long_lived, capacity);
      size_t offset = 0;

      offset += snprintf(canonical_name + offset, capacity - offset, "struct");

      for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
        char *field_name = type->data.struct_data.field_names[i];
        Type *field_type = type->data.struct_data.field_types[i];
        char *type_name = field_type->canonical_name;
        if (!type_name) {
          type_name = compute_canonical_name(field_type);
        }
        assert(type_name && "Failed to canonicalize type");

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

        canonical_register(canonical_name, type);
      }

      type->canonical_name = canonical_name;
    }

    result = type->canonical_name;
    break;
  }

  case TYPE_FUNCTION: {
    // Compute param names
    size_t total_len = strlen("func");

    switch (type->data.func.convention) {
    case CALL_CONV_C:
      total_len += 3;
      break;

    case CALL_CONV_PEBBLE:
      total_len += 8;
      break;
    }

    char **param_names = NULL;
    if (type->data.func.param_count > 0) {
      param_names = arena_alloc(&long_lived,
                                type->data.func.param_count * sizeof(char *));
      for (size_t i = 0; i < type->data.func.param_count; i++) {
        param_names[i] = compute_canonical_name(type->data.func.param_types[i]);
        total_len += strlen(param_names[i]) + 1;
      }
    }

    // Return type
    char *ret_name = compute_canonical_name(type->data.func.return_type);
    total_len += strlen("_ret_") + strlen(ret_name);

    result = arena_alloc(&long_lived, total_len + 1);
    strcpy(result, "func");

    switch (type->data.func.convention) {
    case CALL_CONV_C:
      strcat(result, "_c_");
      break;

    case CALL_CONV_PEBBLE:
      strcat(result, "_pebble_");
      break;
    }

    if (type->data.func.param_count > 0) {
      for (size_t i = 0; i < type->data.func.param_count; i++) {
        strcat(result, "_");
        strcat(result, param_names[i]);
      }
    }

    strcat(result, "_ret_");
    strcat(result, ret_name);
    break;
  }

  default:
    assert(false && "Cannot compute canonical name for unknown type");
    return NULL;
  }
  return result;
}

static size_t num_digits(uint64_t n) {
  if (n == 0)
    return 1;
  size_t digits = 0;
  while (n > 0) {
    digits++;
    n /= 10;
  }
  return digits;
}

char *type_name(Type *type) {
  assert(type && "Type is null");

  switch (type->kind) {
  case TYPE_INT:
  case TYPE_BOOL:
  case TYPE_STRING:
  case TYPE_VOID:
  case TYPE_F32:
  case TYPE_F64:
  case TYPE_U8:
  case TYPE_U16:
  case TYPE_U32:
  case TYPE_U64:
  case TYPE_USIZE:
  case TYPE_I8:
  case TYPE_I16:
  case TYPE_I32:
  case TYPE_I64:
  case TYPE_ISIZE:
  case TYPE_CHAR:
  case TYPE_NONE:
    return type->canonical_name;
  case TYPE_STRUCT:
  case TYPE_ENUM:
  case TYPE_OPAQUE:
  case TYPE_UNION:
  case TYPE_TAGGED_UNION:
    return type->declared_name ? type->declared_name : type->canonical_name;

  case TYPE_POINTER: {
    char *base_ty_name = type_name(type->data.ptr.base);
    size_t len = strlen(base_ty_name) + 2;
    char *ptr_str = arena_alloc(&long_lived, len);
    ptr_str[0] = '*';
    memcpy(ptr_str + 1, base_ty_name, strlen(base_ty_name) + 1);
    return ptr_str;
  }
  case TYPE_OPTIONAL: {
    char *base_ty_name = type_name(type->data.ptr.base);
    size_t len = strlen(base_ty_name) + 2;
    char *ptr_str = arena_alloc(&long_lived, len);
    ptr_str[0] = '?';
    memcpy(ptr_str + 1, base_ty_name, strlen(base_ty_name) + 1);
    return ptr_str;
  }
  case TYPE_ARRAY: {
    char *element_ty_name = type_name(type->data.array.element);
    size_t num_len =
        num_digits(type->data.array.size); // Length of size as string
    size_t len =
        strlen(element_ty_name) + num_len + 3; // [N]T + null terminator
    char *arr_str = arena_alloc(&long_lived, len);
    snprintf(arr_str, len, "[%zu]%s", type->data.array.size, element_ty_name);
    return arr_str;
  }
  case TYPE_SLICE: {
    char *element_ty_name = type_name(type->data.slice.element);
    size_t len = strlen(element_ty_name) + 3; // []T + null terminator
    char *slice_str = arena_alloc(&long_lived, len);
    slice_str[0] = '[';
    slice_str[1] = ']';
    memcpy(slice_str + 2, element_ty_name, strlen(element_ty_name) + 1);
    return slice_str;
  }
  case TYPE_FUNCTION: {
    // Step 1: Cache parameter and return type names
    size_t num_params = type->data.func.param_count;
    char **param_ty_names =
        arena_alloc(&long_lived, num_params * sizeof(char *));
    size_t len = 4; // "fn ("

    CallingConvention conv = type->data.func.convention;
    if (conv == CALL_CONV_C) {
      len += 4; // "c" + space
    }

    for (size_t i = 0; i < num_params; i++) {
      param_ty_names[i] = type_name(type->data.func.param_types[i]);
      len += strlen(param_ty_names[i]);
      if (i < num_params - 1)
        len += 2; // ", "
    }
    len += 2; // ") "
    char *ret_ty_name = type_name(type->data.func.return_type);
    len += strlen(ret_ty_name) + 1; // Return type + null terminator

    // Step 2: Build the string
    char *fn_str = arena_alloc(&long_lived, len);
    size_t offset = 4;
    if (conv == CALL_CONV_C) {
      strcpy(fn_str, "fn \"c\" (");
      offset = 8;
    } else {
      strcpy(fn_str, "fn (");
    }
    for (size_t i = 0; i < num_params; i++) {
      size_t param_len = strlen(param_ty_names[i]);
      memcpy(fn_str + offset, param_ty_names[i], param_len);
      offset += param_len;
      if (i < num_params - 1) {
        fn_str[offset++] = ',';
        fn_str[offset++] = ' ';
      }
    }
    fn_str[offset++] = ')';
    fn_str[offset++] = ' ';
    strcpy(fn_str + offset, ret_ty_name);
    return fn_str;
  }
  case TYPE_TUPLE: {
    if (type->declared_name) {
      return type->declared_name;
    }
    // Step 1: Cache element type names
    size_t num_elements = type->data.tuple.element_count;
    char **elem_ty_names =
        arena_alloc(&long_lived, num_elements * sizeof(char *));
    size_t len = 1; // "("
    for (size_t i = 0; i < num_elements; i++) {
      elem_ty_names[i] = type_name(type->data.tuple.element_types[i]);
      len += strlen(elem_ty_names[i]);
      if (i < num_elements - 1)
        len += 2; // ", "
    }
    len += 2; // ")" + null terminator

    // Step 2: Build the string
    char *tuple_str = arena_alloc(&long_lived, len);
    tuple_str[0] = '(';
    size_t offset = 1;
    for (size_t i = 0; i < num_elements; i++) {
      size_t elem_len = strlen(elem_ty_names[i]);
      memcpy(tuple_str + offset, elem_ty_names[i], elem_len);
      offset += elem_len;
      if (i < num_elements - 1) {
        tuple_str[offset++] = ',';
        tuple_str[offset++] = ' ';
      }
    }
    tuple_str[offset++] = ')';
    tuple_str[offset] = '\0';
    return tuple_str;
  }
  case TYPE_UNRESOLVED:
    return "UNRESOLVED";
  default:
    return "UNKNOWN_TYPE";
  }
}

int member_index_of_type(Type *type, const char *member) {
  switch (type->kind) {
  case TYPE_STRUCT: {
    for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
      if (strcmp(member, type->data.struct_data.field_names[i]) == 0) {
        return i;
      }
    }

    return -1;
  }

  case TYPE_ENUM: {
    for (size_t i = 0; i < type->data.enum_data.variant_count; i++) {
      if (strcmp(member, type->data.enum_data.variant_names[i]) == 0) {
        return i;
      }
    }

    return -1;
  }

  case TYPE_UNION:
  case TYPE_TAGGED_UNION: {
    for (size_t i = 0; i < type->data.union_data.variant_count; i++) {
      if (strcmp(member, type->data.union_data.variant_names[i]) == 0) {
        return i;
      }
    }

    return -1;
  }

  default:
    return -1;
  }
}

// Convert a Type* back to an AstNode* representing that type expression
// This is the inverse of resolve_type_expression()
AstNode *type_to_ast_node(Type *type) {
  if (!type)
    return NULL;

  AstNode *node = arena_alloc(&long_lived, sizeof(AstNode));
  memset(node, 0, sizeof(AstNode));
  node->loc = type->loc;

  switch (type->kind) {
  case TYPE_INT:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "int";
    break;

  case TYPE_BOOL:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "bool";
    break;

  case TYPE_STRING:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "str";
    break;

  case TYPE_VOID:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "void";
    break;

  case TYPE_CHAR:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "char";
    break;

  case TYPE_F32:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "f32";
    break;

  case TYPE_F64:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "f64";
    break;

  case TYPE_U8:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "u8";
    break;

  case TYPE_U16:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "u16";
    break;

  case TYPE_U32:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "u32";
    break;

  case TYPE_U64:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "u64";
    break;

  case TYPE_USIZE:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "usize";
    break;

  case TYPE_I8:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "i8";
    break;

  case TYPE_I16:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "i16";
    break;

  case TYPE_I32:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "i32";
    break;

  case TYPE_I64:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "i64";
    break;

  case TYPE_ISIZE:
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name = "isize";
    break;

  case TYPE_NONE:
    node->kind = AST_EXPR_LITERAL_NONE;
    break;

  case TYPE_POINTER:
    node->kind = AST_TYPE_POINTER;
    node->data.type_pointer.base = type_to_ast_node(type->data.ptr.base);
    break;

  case TYPE_OPTIONAL:
    node->kind = AST_TYPE_OPTIONAL;
    node->data.type_optional.base = type_to_ast_node(type->data.optional.base);
    break;

  case TYPE_ARRAY:
    node->kind = AST_TYPE_ARRAY;
    node->data.type_array.size = type->data.array.size;
    node->data.type_array.element = type_to_ast_node(type->data.array.element);
    break;

  case TYPE_SLICE:
    node->kind = AST_TYPE_SLICE;
    node->data.type_slice.element = type_to_ast_node(type->data.slice.element);
    break;

  case TYPE_TUPLE:
    node->kind = AST_TYPE_TUPLE;
    node->data.type_tuple.element_count = type->data.tuple.element_count;
    node->data.type_tuple.element_types = arena_alloc(
        &long_lived, sizeof(AstNode *) * type->data.tuple.element_count);
    for (size_t i = 0; i < type->data.tuple.element_count; i++) {
      node->data.type_tuple.element_types[i] =
          type_to_ast_node(type->data.tuple.element_types[i]);
    }
    break;

  case TYPE_FUNCTION:
    node->kind = AST_TYPE_FUNCTION;
    node->data.type_function.param_count = type->data.func.param_count;
    node->data.type_function.param_types = arena_alloc(
        &long_lived, sizeof(AstNode *) * type->data.func.param_count);
    for (size_t i = 0; i < type->data.func.param_count; i++) {
      node->data.type_function.param_types[i] =
          type_to_ast_node(type->data.func.param_types[i]);
    }
    node->data.type_function.return_type =
        type_to_ast_node(type->data.func.return_type);
    // TODO: Handle convention if needed
    node->data.type_function.convention = NULL;
    break;

  case TYPE_STRUCT:
  case TYPE_ENUM:
  case TYPE_UNION:
  case TYPE_TAGGED_UNION:
  case TYPE_OPAQUE:
    // For named types (struct, enum, union, opaque), use the declared name
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name =
        type->declared_name ? type->declared_name : type->canonical_name;
    break;

  default:
    // Fallback for any unhandled types
    node->kind = AST_TYPE_NAMED;
    node->data.type_named.name =
        type->declared_name
            ? type->declared_name
            : (type->canonical_name ? type->canonical_name : "<unknown>");
    break;
  }

  return node;
}
