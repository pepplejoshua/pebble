#include "type.h"
#include "alloc.h"
#include "ast.h"
#include "uthash.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// External allocators
extern Arena long_lived;

// Built-in type globals
Type *type_int = NULL;
Type *type_float = NULL;
Type *type_bool = NULL;
Type *type_string = NULL;
Type *type_void = NULL;

// Type table (hash map of name => type entries)
TypeEntry *type_table = NULL;

// Canonical type table (hash map of canonical_name => type entries)
TypeEntry *canonical_type_table = NULL;

// Create a basic type
Type *type_create(TypeKind kind) {
    Type *type = arena_alloc(&long_lived, sizeof(Type));
    type->kind = kind;
    return type;
}

// Create pointer type (with deduplication)
// Create pointer type (conditional canonicalization)
Type *type_create_pointer(Type *base, bool canonicalize) {
    assert(base);

    if (canonicalize) {
        // Compute canonical name and deduplicate
        char *canonical_name = compute_canonical_name(base);
        Type *existing = canonical_lookup(canonical_name);
        if (existing) return existing;

        Type *type = arena_alloc(&long_lived, sizeof(Type));
        type->kind = TYPE_POINTER;
        type->data.ptr.base = base;
        type->canonical_name = canonical_name;
        canonical_register(canonical_name, type);
        return type;
    } else {
        // Just create type without canonicalization
        Type *type = type_create(TYPE_POINTER);
        type->data.ptr.base = base;
        return type;
    }
}

// Create slice type (with deduplication)
Type *type_create_slice(Type *element, bool canonicalize) {
    assert(element);

    if (canonicalize) {
        Type temp = {
            .kind = TYPE_SLICE,
            .data.slice.element = element,
            .canonical_name = NULL
        };

        char *canonical_name = compute_canonical_name(&temp);

        Type *existing = canonical_lookup(canonical_name);
        if (existing) return existing;

        Type *type = arena_alloc(&long_lived, sizeof(Type));
        *type = temp;
        type->canonical_name = canonical_name;
        canonical_register(canonical_name, type);
        return type;
    } else {
        Type *type = type_create(TYPE_SLICE);
        type->data.slice.element = element;
        return type;
    }
}

// Create array type (with deduplication)
Type *type_create_array(Type *element, size_t size, bool canonicalize) {
    assert(element);

    if (canonicalize) {
        Type temp = {
            .kind = TYPE_ARRAY,
            .data.array.element = element,
            .data.array.size = size,
            .canonical_name = NULL
        };

        char *canonical_name = compute_canonical_name(&temp);

        Type *existing = canonical_lookup(canonical_name);
        if (existing) return existing;

        Type *type = arena_alloc(&long_lived, sizeof(Type));
        *type = temp;
        type->canonical_name = canonical_name;
        canonical_register(canonical_name, type);
        return type;
    } else {
        Type *type = type_create(TYPE_ARRAY);
        type->data.array.element = element;
        type->data.array.size = size;
        return type;
    }
}

// Create struct type
Type *type_create_struct(char **field_names, Type **field_types, size_t field_count) {
    assert(field_names && field_types && field_count > 0);
    Type *type = type_create(TYPE_STRUCT);

    // Duplicate field names into arena
    char **names = arena_alloc(&long_lived, field_count * sizeof(char*));
    for (size_t i = 0; i < field_count; i++) {
        names[i] = str_dup(field_names[i]);
    }

    // Copy field types array
    Type **types = arena_alloc(&long_lived, field_count * sizeof(Type*));
    memcpy(types, field_types, field_count * sizeof(Type*));

    type->data.struct_data.field_names = names;
    type->data.struct_data.field_types = types;
    type->data.struct_data.field_count = field_count;
    return type;
}

// Create tuple type (no caching)
// Create tuple type (with deduplication)
Type *type_create_tuple(Type **element_types, size_t element_count, bool canonicalize) {
    assert(element_types && element_count > 0);

    if (canonicalize) {
        // First create a temporary type to compute canonical name
        Type *temp_type = arena_alloc(&long_lived, sizeof(Type));
        temp_type->kind = TYPE_TUPLE;
        temp_type->data.tuple.element_types = element_types;
        temp_type->data.tuple.element_count = element_count;

        // Compute canonical name (this requires element types to be canonicalized first)
        // For now, assume element types are already canonicalized
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
        Type *type = type_create(TYPE_TUPLE);
        Type **types = arena_alloc(&long_lived, element_count * sizeof(Type*));
        memcpy(types, element_types, element_count * sizeof(Type*));
        type->data.tuple.element_types = types;
        type->data.tuple.element_count = element_count;
        return type;
    }
}

// Create function type (no caching)
// Create function type (with deduplication)
Type *type_create_function(Type **param_types, size_t param_count, Type *return_type, bool canonicalize) {
    assert(return_type);

    if (canonicalize) {
        Type temp = {
            .kind = TYPE_FUNCTION,
            .data.func.param_types = param_types,
            .data.func.param_count = param_count,
            .data.func.return_type = return_type,
            .canonical_name = NULL
        };

        char *canonical_name = compute_canonical_name(&temp);

        Type *existing = canonical_lookup(canonical_name);
        if (existing) return existing;

        Type *type = arena_alloc(&long_lived, sizeof(Type));
        *type = temp;
        type->canonical_name = canonical_name;
        canonical_register(canonical_name, type);
        return type;
    } else {
        Type *type = type_create(TYPE_FUNCTION);
        if (param_count > 0) {
            assert(param_types);
            Type **types = arena_alloc(&long_lived, param_count * sizeof(Type*));
            memcpy(types, param_types, param_count * sizeof(Type*));
            type->data.func.param_types = types;
        }
        type->data.func.param_count = param_count;
        type->data.func.return_type = return_type;
        return type;
    }
}

// Look up named type in type table
Type *type_lookup(const char *name) {
    if (!name) return NULL;

    TypeEntry *entry;
    HASH_FIND_STR(type_table, name, entry);
    return entry ? entry->type : NULL;
}

// Register a named type in type table
void type_register(const char *name, Type *type) {
    assert(name && type);

    TypeEntry *entry = arena_alloc(&long_lived, sizeof(TypeEntry));
    entry->name = strdup(name);
    entry->type = type;
    HASH_ADD_STR(type_table, name, entry);
}

// Register a named type in canonical type table
void canonical_register(const char *canonical_name, Type *type) {
    assert(canonical_name && type);

    TypeEntry *entry = arena_alloc(&long_lived, sizeof(TypeEntry));
    entry->name = str_dup(canonical_name);  // Duplicate canonical name
    entry->type = type;
    HASH_ADD_STR(canonical_type_table, name, entry);
}

// Look up canonical type
Type *canonical_lookup(const char *canonical_name) {
    if (!canonical_name) return NULL;

    TypeEntry *entry;
    HASH_FIND_STR(canonical_type_table, canonical_name, entry);
    return entry ? entry->type : NULL;
}


// // Helper with cycle detection for type equality
// static bool type_equals_impl(Type *a, Type *b, Type **visited_a, Type **visited_b, size_t *visited_count) {
//     if (a == b) return true;  // Same pointer
//     if (!a || !b) return false;
//     if (a->kind != b->kind) return false;

//     // Check if we've already started comparing these types (cycle detection)
//     for (size_t i = 0; i < *visited_count; i++) {
//         if (visited_a[i] == a && visited_b[i] == b) {
//             return true;  // Assume equal to break cycle
//         }
//     }

//     // Mark as visiting
//     if (*visited_count < 64) {
//         visited_a[*visited_count] = a;
//         visited_b[*visited_count] = b;
//         (*visited_count)++;
//     }

//     switch (a->kind) {
//         case TYPE_INT:
//         case TYPE_FLOAT:
//         case TYPE_BOOL:
//         case TYPE_STRING:
//         case TYPE_VOID:
//             return true;

//         case TYPE_POINTER:
//             return type_equals_impl(a->data.ptr.base, b->data.ptr.base,
//                                    visited_a, visited_b, visited_count);

//         case TYPE_ARRAY:
//             return a->data.array.size == b->data.array.size &&
//                    type_equals_impl(a->data.array.element, b->data.array.element,
//                                    visited_a, visited_b, visited_count);

//         case TYPE_SLICE:
//             return type_equals_impl(a->data.slice.element, b->data.slice.element,
//                                    visited_a, visited_b, visited_count);

//         case TYPE_STRUCT:
//             if (a->data.struct_data.field_count != b->data.struct_data.field_count) {
//                 return false;
//             }
//             for (size_t i = 0; i < a->data.struct_data.field_count; i++) {
//                 if (strcmp(a->data.struct_data.field_names[i],
//                           b->data.struct_data.field_names[i]) != 0) {
//                     return false;
//                 }
//                 if (!type_equals_impl(a->data.struct_data.field_types[i],
//                                      b->data.struct_data.field_types[i],
//                                      visited_a, visited_b, visited_count)) {
//                     return false;
//                 }
//             }
//             return true;

//         case TYPE_FUNCTION:
//             if (a->data.func.param_count != b->data.func.param_count) {
//                 return false;
//             }
//             if (!type_equals_impl(a->data.func.return_type, b->data.func.return_type,
//                                  visited_a, visited_b, visited_count)) {
//                 return false;
//             }
//             for (size_t i = 0; i < a->data.func.param_count; i++) {
//                 if (!type_equals_impl(a->data.func.param_types[i],
//                                      b->data.func.param_types[i],
//                                      visited_a, visited_b, visited_count)) {
//                     return false;
//                 }
//             }
//             return true;

//         case TYPE_TUPLE:
//             if (a->data.tuple.element_count != b->data.tuple.element_count) {
//                 return false;
//             }
//             for (size_t i = 0; i < a->data.tuple.element_count; i++) {
//                 if (!type_equals_impl(a->data.tuple.element_types[i],
//                                      b->data.tuple.element_types[i],
//                                      visited_a, visited_b, visited_count)) {
//                     return false;
//                 }
//             }
//             return true;

//         case TYPE_UNRESOLVED:
//             return false;
//     }

//     return false;
// }

// // Check if two types are equal (with cycle detection)
// bool type_equals(Type *a, Type *b) {
//     Type *visited_a[64] = {NULL};
//     Type *visited_b[64] = {NULL};
//     size_t visited_count = 0;
//     return type_equals_impl(a, b, visited_a, visited_b, &visited_count);
// }

// Check if two types are equal (using canonical names)
bool type_equals(Type *a, Type *b) {
    if (a == b) return true;  // Same pointer
    if (!a || !b) return false;
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
    return type && (type->kind == TYPE_INT || type->kind == TYPE_FLOAT);
}

// Initialize the type system
void type_system_init(void) {
    // Create built-in types
    type_int = type_create(TYPE_INT);
    type_float = type_create(TYPE_FLOAT);
    type_bool = type_create(TYPE_BOOL);
    type_string = type_create(TYPE_STRING);
    type_void = type_create(TYPE_VOID);

    // Set canonical names for built-in types
    type_int->canonical_name = "int";
    type_float->canonical_name = "float";
    type_bool->canonical_name = "bool";
    type_string->canonical_name = "str";
    type_void->canonical_name = "void";

    // Register built-in types in type table
    type_register("int", type_int);
    type_register("float", type_float);
    type_register("bool", type_bool);
    type_register("str", type_string);
    type_register("void", type_void);

    // Register built-ins in canonical type table
    canonical_register("int", type_int);
    canonical_register("float", type_float);
    canonical_register("bool", type_bool);
    canonical_register("str", type_string);
    canonical_register("void", type_void);
}

// Compute canonical name for a type (with cycle detection)
char *compute_canonical_name(Type *type) {
    assert(type);

    char *result = NULL;

    switch (type->kind) {
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_BOOL:
        case TYPE_STRING:
        case TYPE_VOID:
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
            char **elem_names = arena_alloc(&long_lived,
                type->data.tuple.element_count * sizeof(char*));
            size_t total_len = strlen("tuple_");

            for (size_t i = 0; i < type->data.tuple.element_count; i++) {
                elem_names[i] = compute_canonical_name(type->data.tuple.element_types[i]);
                total_len += strlen(elem_names[i]) + 1;
            }
            total_len--;  // Remove last "_"

            result = arena_alloc(&long_lived, total_len + 1);
            strcpy(result, "tuple_");
            for (size_t i = 0; i < type->data.tuple.element_count; i++) {
                if (i > 0) strcat(result, "_");
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
                param_names = arena_alloc(&long_lived, type->data.func.param_count * sizeof(char*));
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
