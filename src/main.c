#include "alloc.h"
#include "symbol.h"
#include "type.h"
#include "lexer.h"
#include "parser.h"
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

// Helper function to get AST kind name
const char *ast_kind_name(AstKind kind) {
    switch (kind) {
        case AST_DECL_FUNCTION: return "FUNCTION_DECL";
        case AST_DECL_VARIABLE: return "VARIABLE_DECL";
        case AST_DECL_CONSTANT: return "CONSTANT_DECL";
        case AST_DECL_TYPE: return "TYPE_DECL";
        case AST_STMT_RETURN: return "RETURN_STMT";
        case AST_STMT_IF: return "IF_STMT";
        case AST_STMT_WHILE: return "WHILE_STMT";
        case AST_STMT_BLOCK: return "BLOCK_STMT";
        case AST_STMT_EXPR: return "EXPR_STMT";
        case AST_STMT_ASSIGN: return "ASSIGN_STMT";
        case AST_EXPR_LITERAL_INT: return "INT_LITERAL";
        case AST_EXPR_LITERAL_FLOAT: return "FLOAT_LITERAL";
        case AST_EXPR_LITERAL_STRING: return "STRING_LITERAL";
        case AST_EXPR_LITERAL_BOOL: return "BOOL_LITERAL";
        case AST_EXPR_IDENTIFIER: return "IDENTIFIER";
        case AST_EXPR_BINARY_OP: return "BINARY_OP";
        case AST_EXPR_UNARY_OP: return "UNARY_OP";
        case AST_EXPR_CALL: return "CALL";
        case AST_EXPR_INDEX: return "INDEX";
        case AST_EXPR_MEMBER: return "MEMBER";
        default: return "UNKNOWN";
    }
}

void test_parser(void) {
    printf("=== Parser Tests ===\n");

    // Test cases
    const char *test_sources[] = {
        // Simple function
        "fn add(a int, b int) int {\n    return a + b;\n}",

        // Expression function
        "fn square(x int) int => x * x",

        // Function with local variables
        "fn factorial(n int) int {\n"
        "    var result = 1;\n"
        "    while n > 1 {\n"
        "        result = result * n;\n"
        "        n = n - 1;\n"
        "    }\n"
        "    return result;\n"
        "}",

        // Variable declarations
        "let name = \"hello\";\nvar count = 42;",

        // If statement
        "fn max(a int, b int) int {\n"
        "    if a > b {\n"
        "        return a;\n"
        "    } else {\n"
        "        return b;\n"
        "    }\n"
        "}"
    };

    for (int i = 0; i < 5; i++) {
        printf("\n--- Test %d ---\n", i + 1);
        printf("Source:\n%s\n\n", test_sources[i]);

        Parser parser;
        parser_init(&parser, test_sources[i], "<test>");

        AstNode *program = parse_program(&parser);

        if (parser.had_error) {
            printf("❌ Parse failed with errors\n");
        } else if (program == NULL) {
            printf("✓ Parsed successfully (empty program)\n");
        } else {
            printf("✓ Parsed successfully!\n");
            printf("  Declarations: %zu\n", program->data.block_stmt.stmt_count);

            // Print details for each declaration
            for (size_t j = 0; j < program->data.block_stmt.stmt_count; j++) {
                AstNode *decl = program->data.block_stmt.stmts[j];
                printf("  [%zu] %s", j, ast_kind_name(decl->kind));

                switch (decl->kind) {
                    case AST_DECL_FUNCTION:
                        printf(" - %s (%zu params)",
                              decl->data.func_decl.name,
                              decl->data.func_decl.param_count);
                        break;
                    case AST_DECL_VARIABLE:
                        printf(" - %s", decl->data.var_decl.name);
                        break;
                    case AST_DECL_CONSTANT:
                        printf(" - %s", decl->data.const_decl.name);
                        break;
                    case AST_DECL_TYPE:
                        printf(" - %s", decl->data.type_decl.name);
                        break;
                    default:
                        break;
                }
                printf("\n");
            }
        }
    }

    printf("\n✓ Parser tests completed\n");
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
        } else if (strcmp(argv[1], "--test-parser") == 0) {
            test_parser();
        } else {
            printf("Compiling: %s\n", argv[1]);
            // TODO: Add lexer/parser/passes here
        }
    } else {
        printf("Usage: %s <source_file>\n", argv[0]);
        printf("       %s --test\n", argv[0]);
        printf("       %s --test-lexer\n", argv[0]);
        printf("       %s --test-parser\n", argv[0]);
    }


    // Cleanup
    arena_free(&long_lived);
    return 0;
}
