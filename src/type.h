#ifndef TYPE_H
#define TYPE_H

#include "ast.h"
#include "uthash.h"
#include <stdbool.h>
#include <stddef.h>

// Forward declarations
typedef struct Type Type;
typedef struct TypeEntry TypeEntry;

// Type kinds
typedef enum {
  TYPE_INT,
  TYPE_BOOL,
  TYPE_STRING,
  TYPE_VOID,
  TYPE_F32,
  TYPE_F64,
  TYPE_U8,
  TYPE_U16,
  TYPE_U32,
  TYPE_U64,
  TYPE_USIZE,
  TYPE_I8,
  TYPE_I16,
  TYPE_I32,
  TYPE_I64,
  TYPE_ISIZE,
  TYPE_CHAR,
  TYPE_POINTER,
  TYPE_ARRAY,
  TYPE_SLICE,
  TYPE_STRUCT,
  TYPE_FUNCTION,
  TYPE_TUPLE,
  TYPE_UNRESOLVED,
  TYPE_OPAQUE,
} TypeKind;

// Type structure
struct Type {
  TypeKind kind;
  char *canonical_name;
  char *declared_name;
  Location loc;

  union {
    struct {
      Type *base; // Base type for pointer
    } ptr;

    struct {
      Type *element; // Element type
      size_t size;   // Array size
    } array;

    struct {
      Type *element; // Element type
    } slice;

    struct {
      char **field_names; // Array of field names
      Type **field_types; // Array of field types
      size_t field_count; // Number of fields
    } struct_data;

    struct {
      Type **param_types; // Parameter types
      size_t param_count; // Number of parameters
      Type *return_type;  // Return type
    } func;

    struct {
      Type **element_types; // Tuple element types
      size_t element_count; // Number of elements
    } tuple;
  } data;
};

// Type table entry (name -> type mapping)
struct TypeEntry {
  char *name;        // Key for hash table
  Type *type;        // Pointer to the type
  UT_hash_handle hh; // Hash handle
};

// Built-in types
extern Type *type_int;
extern Type *type_bool;
extern Type *type_string;
extern Type *type_void;
extern Type *type_f32;
extern Type *type_f64;
extern Type *type_u8;
extern Type *type_u16;
extern Type *type_u32;
extern Type *type_u64;
extern Type *type_usize;
extern Type *type_i8;
extern Type *type_i16;
extern Type *type_i32;
extern Type *type_i64;
extern Type *type_isize;
extern Type *type_char;

// Type table (global hash map of named types)
extern TypeEntry *type_table;

// Canonical type table (global hash map of canonical name -> type)
extern TypeEntry *canonical_type_table;

// Type system functions
void type_system_init(void);
Type *type_create(TypeKind kind, Location loc);
Type *type_create_pointer(Type *base, bool canonicalize, Location loc);
Type *type_create_slice(Type *element, bool canonicalize, Location loc);
Type *type_create_array(Type *element, size_t size, bool canonicalize, Location loc);
Type *type_create_struct(char **field_names, Type **field_types,
                         size_t field_count, Location loc);
Type *type_create_tuple(Type **element_types, size_t element_count,
                        bool canonicalize, Location loc);
Type *type_create_function(Type **param_types, size_t param_count,
                           Type *return_type, bool canonicalize, Location loc);

// Type lookup and registration
Type *type_lookup(const char *name);
void type_register(const char *name, Type *type);
Type *canonical_lookup(const char *canonical_name);
void canonical_register(const char *canonical_name, Type *type);

// Compute canonical name for a fully resolved type
char *compute_canonical_name(Type *type);

// Generate regular name for a fully resolved type
char *type_name(Type *type);

// Type checking utilities
bool type_equals(Type *a, Type *b);
bool type_is_numeric(Type *type);

#endif
