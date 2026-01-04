#ifndef TYPE_H
#define TYPE_H

#include "ast.h"
#include "symbol.h"
#include "wrapped_uthash.h"
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
  TYPE_UNION,
  TYPE_TAGGED_UNION,
  TYPE_ENUM,
  TYPE_FUNCTION,
  TYPE_TUPLE,
  TYPE_UNRESOLVED,
  TYPE_OPAQUE,
  TYPE_OPTIONAL,
  TYPE_NONE,
  TYPE_GENERIC_FUNCTION,
  TYPE_GENERIC_TYPE_DECL,
} TypeKind;

typedef enum {
  CALL_CONV_C,
  // has "context" as first arg
  CALL_CONV_PEBBLE,
} CallingConvention;

// Type structure
struct Type {
  TypeKind kind;
  char *canonical_name;
  char *declared_name;
  char *qualified_name; // Used for named types
  Location loc;
  bool used;

  // For monomorphized generics, store what it was specialized with
  Type **generic_type_args;      // [int, string] for GenericType[int, string]
  size_t generic_type_arg_count; // 2

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
      bool builtin;

      char **field_names; // Array of field names
      Type **field_types; // Array of field types
      size_t field_count; // Number of fields

      char **method_qualified_names;
      char **method_reg_names;
      Type **method_types;
      size_t method_count;

      Symbol **generic_method_symbols;
      char **generic_method_reg_names;
      size_t generic_method_count;
    } struct_data;

    struct {
      bool tagged;
      char **variant_names; // Array of variant names
      Type **variant_types; // Array of variant types
      size_t variant_count; // Number of variants

      char **method_qualified_names;
       char **method_reg_names;
       Type **method_types;
       size_t method_count;

       Symbol **generic_method_symbols;
       char **generic_method_reg_names;
       size_t generic_method_count;
    } union_data;

    struct {
      char **variant_names; // Array of variant names
      size_t variant_count; // Number of variants
    } enum_data;

    struct {
      CallingConvention convention;
      Type **param_types; // Parameter types
      size_t param_count; // Number of parameters
      Type *return_type;  // Return type
      bool is_variadic;
    } func;

    struct {
      Type **element_types; // Tuple element types
      size_t element_count; // Number of elements
    } tuple;

    struct {
      Type *base;
    } optional;

    struct {
      AstNode *decl;  // Points to the original AST_DECL_FUNCTION
    } generic_decl;

  } data;
};

// Type table entry (name -> type mapping)
struct TypeEntry {
  char *name;        // Key for hash table
  Type *type;        // Pointer to the type
  UT_hash_handle hh; // Hash handle
};

// Monomorphization instance (for generic function)
typedef struct MonoFuncInstance {
  char *key;                   // Mangled name: "add__int"
  AstNode *generic_func;   // Original generic function declaration
  Type **concrete_types;       // [int] or [int, bool]
  size_t type_count;
  AstNode *monomorphized_func; // The specialized function AST
  Symbol *symbol;
  UT_hash_handle hh;
} MonoFuncInstance;

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
extern Type *type_none;
extern Type *type_context;

// Type table (global hash map of named types)
extern TypeEntry *type_table;

// Canonical type table (global hash map of canonical name -> type)
extern TypeEntry *canonical_type_table;

// Monomorphization table (global hash map of mangled name -> function instance)
extern MonoFuncInstance *mono_instances;

// Type system functions
void type_system_init(void);
Type *type_create(TypeKind kind, Location loc);
Type *type_create_pointer(Type *base, bool canonicalize, Location loc);
Type *type_create_optional(Type *base, bool canonicalize, Location loc);
Type *type_create_slice(Type *element, bool canonicalize, Location loc);
Type *type_create_array(Type *element, size_t size, bool canonicalize, Location loc);
Type *type_create_struct(char **field_names, Type **field_types,
                         size_t field_count, bool builtin, bool canonicalize,
                         Location loc);
Type *type_create_union(bool tagged, char **variant_names, Type **variant_types,
                         size_t variant_count, bool canonicalize, Location loc);
Type *type_create_enum(char **variant_names, size_t variant_count, bool canonicalize, Location loc);
Type *type_create_tuple(Type **element_types, size_t element_count,
                        bool canonicalize, Location loc);
Type *type_create_function(Type **param_types, size_t param_count,
                           Type *return_type, bool is_variadic,
                           bool canonicalize, CallingConvention convention,
                           Location loc);

// Type lookup and registration
Type *type_lookup(const char *name, const char *mod_name);
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
bool type_is_ord(Type *type);

// Type conversion
AstNode *type_to_ast_node(Type *type);

int member_index_of_type(Type *type, const char *member);

#endif
