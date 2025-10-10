#include "alloc.h"
#include "symbol.h"
#include "type.h"
#include "lexer.h"
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

void test_lexer(void) {
    printf("=== Lexer Tests ===\n");

    // Test cases
    const char *test_sources[] = {
        // Simple function
        "fn add(a int, b int) int {\n    return a + b;\n}",

        // Expression function
        "fn square(x int) int => x * x",

        // Variables and literals
        "let name = \"hello\";\nvar count = 42;\nlet pi = 3.14;",

        // Keywords and operators
        "if true && false || !x >= 10 { return; }",

        // Comments
        "// This is a comment\nlet x = 5; // Another comment"
    };

    for (int i = 0; i < 5; i++) {
        printf("\n--- Test %d ---\n", i + 1);
        printf("Source: %s\n\n", test_sources[i]);

        Lexer lexer;
        lexer_init(&lexer, test_sources[i], "<test>");

        Token token;
        do {
            token = lexer_next_token(&lexer);

            printf("%-15s", token_type_name(token.type));
            printf("'%s'", token.lexeme);
            printf(" at %d:%d", token.location.line, token.location.column);

            // Print parsed values for literals
            switch (token.type) {
                case TOKEN_INT:
                    printf(" (value: %lld)", token.value.int_val);
                    break;
                case TOKEN_FLOAT:
                    printf(" (value: %g)", token.value.float_val);
                    break;
                case TOKEN_STRING:
                    printf(" (value: \"%s\")", token.value.str_val);
                    break;
                case TOKEN_TRUE:
                case TOKEN_FALSE:
                    printf(" (value: %s)", token.value.bool_val ? "true" : "false");
                    break;
                default:
                    break;
            }

            printf("\n");

            if (token.type == TOKEN_ERROR) {
                printf("ERROR: %s\n", token.lexeme);
                break;
            }

        } while (token.type != TOKEN_EOF);
    }

    printf("\n✓ Lexer tests completed\n");
}


int main(int argc, char **argv) {
    // Initialize systems
    arena_init(&long_lived, 1024 * 1024);
    type_system_init();
    symbol_table_init();

    if (argc > 1) {
        if (strcmp(argv[1], "--test") == 0) {
            run_tests();
        } else if (strcmp(argv[1], "--test-lexer") == 0) {
            test_lexer();
        } else {
            printf("Compiling: %s\n", argv[1]);
            // TODO: Add lexer/parser/passes here
        }
    } else {
        printf("Usage: %s <source_file>\n", argv[0]);
        printf("       %s --test\n", argv[0]);
        printf("       %s --test-lexer\n", argv[0]);  // Add this line
    }

    // Cleanup
    arena_free(&long_lived);
    return 0;
}
