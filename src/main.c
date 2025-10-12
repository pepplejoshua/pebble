#include "alloc.h"
#include "tests.h"
#include <stdio.h>
#include <string.h>

// Global allocators
Arena long_lived;

int main(int argc, char **argv) {
    // Initialize compiler systems
    arena_init(&long_lived, 1024 * 1024);

    // Handle command-line arguments
    if (argc > 1) {
        if (strcmp(argv[1], "--test") == 0) {
            test_setup();
        } else if (strcmp(argv[1], "--test-lexer") == 0) {
            test_lexer();
        } else if (strcmp(argv[1], "--test-parser") == 0) {
            test_parser();
        } else if (strcmp(argv[1], "--test-checker") == 0) {
            test_checker();
        } else if (strcmp(argv[1], "--test-all") == 0) {
            test_all();
        } else {
            // Compile a source file
            printf("Compiling: %s\n", argv[1]);
            // TODO: Implement compilation pipeline
        }
    } else {
        printf("Pebble Compiler\n");
        printf("Usage: %s <source_file>\n", argv[0]);
        printf("       %s --test\n", argv[0]);
        printf("       %s --test-lexer\n", argv[0]);
        printf("       %s --test-parser\n", argv[0]);
        printf("       %s --test-checker\n", argv[0]);
        printf("       %s --test-all\n", argv[0]);
    }

    // Cleanup
    arena_free(&long_lived);
    return 0;
}
