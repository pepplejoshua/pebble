#include "type.h"
#include "alloc.h"
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

// Type table (hash map of named types)
Type *type_table = NULL;

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
    type->name = NULL;
    return type;
}

// Create pointer type
Type *type_create_pointer(Type *base) {
    assert(base);
    Type *type = type_create(TYPE_POINTER);
    type->data.ptr.base = base;
    return type;
}

// Create array type
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

// Create function type
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

    Type *type;
    HASH_FIND_STR(type_table, name, type);
    return type;
}

// Register a named type in type table
void type_register(const char *name, Type *type) {
    assert(name && type);

    // Create a wrapper/alias type entry
    Type *entry = arena_alloc(&long_lived, sizeof(Type));
    memcpy(entry, type, sizeof(Type));
    entry->name = str_dup(name);

    HASH_ADD_STR(type_table, name, entry);
}

// Check if two types are equal
bool type_equals(Type *a, Type *b) {
    if (a == b) return true;  // Same pointer
    if (!a || !b) return false;
    if (a->kind != b->kind) return false;

    switch (a->kind) {
        case TYPE_INT:
        case TYPE_FLOAT:
        case TYPE_BOOL:
        case TYPE_STRING:
        case TYPE_VOID:
            return true;  // Built-in types are equal by kind

        case TYPE_POINTER:
            return type_equals(a->data.ptr.base, b->data.ptr.base);

        case TYPE_ARRAY:
            return a->data.array.size == b->data.array.size &&
                   type_equals(a->data.array.element, b->data.array.element);

        case TYPE_SLICE:
            return type_equals(a->data.array.element, b->data.array.element);

        case TYPE_STRUCT:
            if (a->data.struct_data.field_count != b->data.struct_data.field_count) {
                return false;
            }
            for (size_t i = 0; i < a->data.struct_data.field_count; i++) {
                if (strcmp(a->data.struct_data.field_names[i],
                          b->data.struct_data.field_names[i]) != 0) {
                    return false;
                }
                if (!type_equals(a->data.struct_data.field_types[i],
                               b->data.struct_data.field_types[i])) {
                    return false;
                }
            }
            return true;

        case TYPE_FUNCTION:
            if (a->data.func.param_count != b->data.func.param_count) {
                return false;
            }
            if (!type_equals(a->data.func.return_type, b->data.func.return_type)) {
                return false;
            }
            for (size_t i = 0; i < a->data.func.param_count; i++) {
                if (!type_equals(a->data.func.param_types[i],
                               b->data.func.param_types[i])) {
                    return false;
                }
            }
            return true;
    }

    return false;
}

// Check if type is numeric (int or float)
bool type_is_numeric(Type *type) {
    return type && (type->kind == TYPE_INT || type->kind == TYPE_FLOAT);
}

// Initialize the type system
void type_system_init(void) {
    // Only initialize if not already done
    if (type_int != NULL) {
        return;  // Already initialized
    }

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
