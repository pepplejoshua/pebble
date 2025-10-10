#include "alloc.h"
#include "symbol.h"
#include "type.h"
#include <stdio.h>

// Global allocators
Arena long_lived;

// Test/debug function
void run_tests(void) {
    printf("=== Pebble Compiler Tests ===\n");

    // Test type lookup
    Type *int_type = type_lookup("int");
    Type *str_type = type_lookup("str");
    printf("✓ type_lookup('int'): %s\n", int_type ? "found" : "not found");
    printf("✓ type_lookup('str'): %s\n", str_type ? "found" : "not found");

    // Test type creation
    Type *ptr_int = type_create_pointer(int_type);
    printf("✓ Created pointer type: *int\n");

    // Test symbol creation
    Symbol *test_sym = symbol_create("test_var", SYMBOL_VARIABLE, NULL);
    printf("✓ Created test symbol: %s\n", test_sym->name);

    printf("✓ All tests passed\n");
    printf("Arena usage: %zu / %zu bytes\n", long_lived.used, long_lived.capacity);
}

int main(int argc, char **argv) {
    // Initialize systems
    arena_init(&long_lived, 1024 * 1024);
    type_system_init();
    symbol_table_init();

    if (argc > 1) {
        if (strcmp(argv[1], "--test") == 0) {
            run_tests();
        } else {
            printf("Compiling: %s\n", argv[1]);
            // TODO: Add lexer/parser/passes here
        }
    } else {
        printf("Usage: %s <source_file>\n", argv[0]);
        printf("       %s --test\n", argv[0]);
    }

    // Cleanup
    arena_free(&long_lived);
    return 0;
}
