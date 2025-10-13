#include "type.h"
#include "alloc.h"
#include "ast.h"
#include "uthash.h"
#include <stdio.h>
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

// Helper to duplicate strings
static char *str_dup(const char *str) {
    if (!str) return NULL;
    size_t len = strlen(str) + 1;
    char *copy = arena_alloc(&long_lived, len);
    memcpy(copy, str, len);
    return copy;
}

// Create a basic type
Type *type_create(TypeKind kind) {
    Type *type = arena_alloc(&long_lived, sizeof(Type));
    type->kind = kind;
    return type;
}

// Create pointer type (no caching)
Type *type_create_pointer(Type *base) {
    assert(base);
    Type *type = type_create(TYPE_POINTER);
    type->data.ptr.base = base;
    return type;
}

// Create slice type (no caching)
Type *type_create_slice(Type *element) {
    assert(element);
    Type *type = type_create(TYPE_SLICE);
    type->data.slice.element = element;
    return type;
}

// Create array type (no caching)
Type *type_create_array(Type *element, size_t size) {
    assert(element);
    Type *type = type_create(TYPE_ARRAY);
    type->data.array.element = element;
    type->data.array.size = size;
    return type;
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
Type *type_create_tuple(Type **element_types, size_t element_count) {
    assert(element_types && element_count > 0);
    Type *type = type_create(TYPE_TUPLE);

    // Copy element types array
    Type **types = arena_alloc(&long_lived, element_count * sizeof(Type*));
    memcpy(types, element_types, element_count * sizeof(Type*));

    type->data.tuple.element_types = types;
    type->data.tuple.element_count = element_count;
    return type;
}

// Create function type (no caching)
Type *type_create_function(Type **param_types, size_t param_count, Type *return_type) {
    assert(return_type);
    Type *type = type_create(TYPE_FUNCTION);

    if (param_count > 0) {
        assert(param_types);
        Type **types = arena_alloc(&long_lived, param_count * sizeof(Type*));
        memcpy(types, param_types, param_count * sizeof(Type*));
        type->data.func.param_types = types;
    } else {
        type->data.func.param_types = NULL;
    }

    type->data.func.param_count = param_count;
    type->data.func.return_type = return_type;
    return type;
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

// Helper with cycle detection for type equality
static bool type_equals_impl(Type *a, Type *b, Type **visited_a, Type **visited_b, size_t *visited_count) {
    if (a == b) return true;  // Same pointer
    if (!a || !b) return false;
    if (a->kind != b->kind) return false;

    // Check if we've already started comparing these types (cycle detection)
    for (size_t i = 0; i < *visited_count; i++) {
        if (visited_a[i] == a && visited_b[i] == b) {
            return true;  // Assume equal to break cycle
        }
    }

    // Mark as visiting
    if (*visited_count < 64) {
        visited_a[*visited_count] = a;
        visited_b[*visited_count] = b;
        (*visited_count)++;
    }

    switch (a->kind) {
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_BOOL:
        case TYPE_STRING:
        case TYPE_VOID:
            return true;

        case TYPE_POINTER:
            return type_equals_impl(a->data.ptr.base, b->data.ptr.base,
                                   visited_a, visited_b, visited_count);

        case TYPE_ARRAY:
            return a->data.array.size == b->data.array.size &&
                   type_equals_impl(a->data.array.element, b->data.array.element,
                                   visited_a, visited_b, visited_count);

        case TYPE_SLICE:
            return type_equals_impl(a->data.slice.element, b->data.slice.element,
                                   visited_a, visited_b, visited_count);

        case TYPE_STRUCT:
            if (a->data.struct_data.field_count != b->data.struct_data.field_count) {
                return false;
            }
            for (size_t i = 0; i < a->data.struct_data.field_count; i++) {
                if (strcmp(a->data.struct_data.field_names[i],
                          b->data.struct_data.field_names[i]) != 0) {
                    return false;
                }
                if (!type_equals_impl(a->data.struct_data.field_types[i],
                                     b->data.struct_data.field_types[i],
                                     visited_a, visited_b, visited_count)) {
                    return false;
                }
            }
            return true;

        case TYPE_FUNCTION:
            if (a->data.func.param_count != b->data.func.param_count) {
                return false;
            }
            if (!type_equals_impl(a->data.func.return_type, b->data.func.return_type,
                                 visited_a, visited_b, visited_count)) {
                return false;
            }
            for (size_t i = 0; i < a->data.func.param_count; i++) {
                if (!type_equals_impl(a->data.func.param_types[i],
                                     b->data.func.param_types[i],
                                     visited_a, visited_b, visited_count)) {
                    return false;
                }
            }
            return true;

        case TYPE_TUPLE:
            if (a->data.tuple.element_count != b->data.tuple.element_count) {
                return false;
            }
            for (size_t i = 0; i < a->data.tuple.element_count; i++) {
                if (!type_equals_impl(a->data.tuple.element_types[i],
                                     b->data.tuple.element_types[i],
                                     visited_a, visited_b, visited_count)) {
                    return false;
                }
            }
            return true;

        case TYPE_UNRESOLVED:
            return false;
    }

    return false;
}

// Check if two types are equal (with cycle detection)
bool type_equals(Type *a, Type *b) {
    Type *visited_a[64] = {NULL};
    Type *visited_b[64] = {NULL};
    size_t visited_count = 0;
    return type_equals_impl(a, b, visited_a, visited_b, &visited_count);
}


// Check if type is numeric (int or float)
bool type_is_numeric(Type *type) {
    return type && (type->kind == TYPE_INT || type->kind == TYPE_FLOAT);
}

// Initialize the type system
void type_system_init(void) {
    // Only initialize if not already done
    // if (type_int != NULL) {
    //     return;  // Already initialized
    // }

    // Create built-in types
    type_int = type_create(TYPE_INT);
    type_float = type_create(TYPE_FLOAT);
    type_bool = type_create(TYPE_BOOL);
    type_string = type_create(TYPE_STRING);
    type_void = type_create(TYPE_VOID);

    // Register built-in types in type table
    type_register("int", type_int);
    type_register("float", type_float);
    type_register("bool", type_bool);
    type_register("str", type_string);
    type_register("void", type_void);
}
