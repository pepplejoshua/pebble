#include "type.h"
#include "alloc.h"
#include "ast.h"
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

// Type table (hash map of name => type entries)
TypeEntry *type_table = NULL;

// Canonical type table (hash map of canonical_name => type entries)
TypeEntry *canonical_type_table = NULL;

// Create a basic type
Type *type_create(TypeKind kind, Location loc) {
  Type *type = arena_alloc(&long_lived, sizeof(Type));
  type->kind = kind;
  type->loc = loc;
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
                         size_t field_count, Location loc) {
  Type *type = type_create(TYPE_STRUCT, loc);

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

// Create enum type
Type *type_create_enum(char **variant_names, size_t variant_count,
                        Location loc) {
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
                           Type *return_type, bool canonicalize, Location loc) {
  assert(return_type);

  if (canonicalize) {
    Type temp = {.kind = TYPE_FUNCTION,
                 .data.func.param_types = param_types,
                 .data.func.param_count = param_count,
                 .data.func.return_type = return_type,
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
    type->data.func.param_count = param_count;
    type->data.func.return_type = return_type;
    return type;
  }
}

// Look up named type in type table
Type *type_lookup(const char *name) {
  if (!name)
    return NULL;

  TypeEntry *entry;
  HASH_FIND_STR(type_table, name, entry);
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
  return type && (type->kind == TYPE_INT ||
                  type->kind == TYPE_U8 || type->kind == TYPE_U16 ||
                  type->kind == TYPE_U32 || type->kind == TYPE_U64 ||
                  type->kind == TYPE_USIZE || type->kind == TYPE_I8 ||
                  type->kind == TYPE_I16 || type->kind == TYPE_I32 ||
                  type->kind == TYPE_I64 || type->kind == TYPE_ISIZE ||
                  type->kind == TYPE_F32 || type->kind == TYPE_F64);
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
}

// Compute canonical name for a type (with cycle detection)
char *compute_canonical_name(Type *type) {
  assert(type);

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
    result = type->canonical_name;
    break;

  case TYPE_POINTER: {
    char *base_name = compute_canonical_name(type->data.ptr.base);
    size_t len = strlen("ptr_") + strlen(base_name) + 1;
    result = arena_alloc(&long_lived, len);
    snprintf(result, len, "ptr_%s", base_name);
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

  case TYPE_STRUCT: {
    // Named structs already have canonical_name set (nominal)
    result = type->canonical_name;
    break;
  }

  case TYPE_FUNCTION: {
    // Compute param names
    size_t total_len = strlen("func");
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

  case TYPE_UNRESOLVED:
    assert(false && "Cannot compute canonical name for unresolved type");
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
    return type->canonical_name;
  case TYPE_STRUCT:
  case TYPE_ENUM:
    return type->declared_name;
  case TYPE_POINTER: {
    char *base_ty_name = type_name(type->data.ptr.base);
    size_t len = strlen(base_ty_name) + 2;
    char *ptr_str = arena_alloc(&long_lived, len);
    ptr_str[0] = '*';
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
    strcpy(fn_str, "fn (");
    size_t offset = 4;
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
