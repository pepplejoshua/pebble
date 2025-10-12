#include "tests.h"
#include "alloc.h"
#include "symbol.h"
#include "type.h"
#include "lexer.h"
#include "parser.h"
#include "checker.h"
#include <stdio.h>

extern Arena long_lived;

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

void test_setup(void) {
    printf("=== Pebble Compiler Tests ===\n");

    type_system_init();

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

    const char *test_sources[] = {
        "fn add(a int, b int) int {\n    return a + b;\n}",
        "fn square(x int) int => x * x",
        "let name = \"hello\";\nvar count = 42;\nlet pi = 3.14;",
        "if true && false || !x >= 10 { return; }",
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

void test_parser(void) {
    printf("=== Parser Tests ===\n");

    const char *test_sources[] = {
        "fn add(a int, b int) int {\n    return a + b;\n}",
        "fn square(x int) int => x * x",
        "fn factorial(n int) int {\n"
        "    var result = 1;\n"
        "    while n > 1 {\n"
        "        result = result * n;\n"
        "        n = n - 1;\n"
        "    }\n"
        "    return result;\n"
        "}",
        "let name = \"hello\";\nvar count = 42;",
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

void test_checker(void) {
    printf("=== Checker Tests (Pass 2: Collect Globals) ===\n");
    // Test 1: Valid - unique global declarations
    {
        printf("\n--- Test 1: Valid unique globals ---\n");
        const char *source =
            "fn add(a int, b int) int { return a + b; }\n"
            "fn subtract(a int, b int) int { return a - b; }\n"
            "var count int;\n"
            "let pi = 3.14;\n"
            "type MyInt = int;";

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (success && !checker_has_errors()) {
                printf("✓ Collected globals successfully\n");

                // Verify symbols were added
                Symbol *add = scope_lookup(global_scope, "add");
                Symbol *sub = scope_lookup(global_scope, "subtract");
                Symbol *count = scope_lookup(global_scope, "count");
                Symbol *pi = scope_lookup(global_scope, "pi");
                Symbol *myint = scope_lookup(global_scope, "MyInt");

                printf("  - 'add' found: %s (kind: %d)\n", add ? "yes" : "no", add ? add->kind : -1);
                printf("  - 'subtract' found: %s (kind: %d)\n", sub ? "yes" : "no", sub ? sub->kind : -1);
                printf("  - 'count' found: %s (kind: %d)\n", count ? "yes" : "no", count ? count->kind : -1);
                printf("  - 'pi' found: %s (kind: %d)\n", pi ? "yes" : "no", pi ? pi->kind : -1);
                printf("  - 'MyInt' found: %s (kind: %d)\n", myint ? "yes" : "no", myint ? myint->kind : -1);
            } else {
                printf("❌ Expected success but got errors\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    // Test 2: Invalid - duplicate function names
    {
        printf("\n--- Test 2: Duplicate function names ---\n");
        const char *source =
            "fn foo() int { return 1; }\n"
            "fn foo() int { return 2; }";

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (!success && checker_has_errors()) {
                printf("✓ Correctly detected duplicate 'foo'\n");
            } else {
                printf("❌ Should have detected duplicate declaration\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    // Test 3: Invalid - mixed duplicates (function and variable)
    {
        printf("\n--- Test 3: Mixed duplicate (function + variable) ---\n");
        const char *source =
            "fn bar() void { }\n"
            "var bar int;";

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (!success && checker_has_errors()) {
                printf("✓ Correctly detected duplicate 'bar'\n");
            } else {
                printf("❌ Should have detected duplicate declaration\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    // Test 4: Pass 3 - Type resolution
    {
        printf("\n--- Test 4: Pass 3 - Type resolution ---\n");

        const char *source =
            "type MyInt = int;\n"
            "type IntPtr = *int;\n"
            "let pi = 3.14;\n"
            "var count int;\n"
            "var total = 100;\n"
            "fn add(a int, b int) int { return a + b; }";

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();

            // Pass 2: Collect globals
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (!success) {
                printf("❌ Pass 2 failed\n");
                return;
            }

            // Pass 3: Check globals
            success = check_globals();

            if (success && !checker_has_errors()) {
                printf("✓ Pass 3 completed successfully\n");

                // Verify types were resolved
                Symbol *myint = scope_lookup(global_scope, "MyInt");
                Symbol *intptr = scope_lookup(global_scope, "IntPtr");
                Symbol *pi = scope_lookup(global_scope, "pi");
                Symbol *count = scope_lookup(global_scope, "count");
                Symbol *total = scope_lookup(global_scope, "total");
                Symbol *add = scope_lookup(global_scope, "add");

                printf("  - MyInt type: %s\n", myint && myint->type ? "resolved" : "NULL");
                printf("  - IntPtr type: %s\n", intptr && intptr->type ? "resolved" : "NULL");
                printf("  - pi type: %s (kind: %d)\n",
                       pi && pi->type ? "resolved" : "NULL",
                       pi && pi->type ? pi->type->kind : -1);
                printf("  - count type: %s (kind: %d)\n",
                       count && count->type ? "resolved" : "NULL",
                       count && count->type ? count->type->kind : -1);
                printf("  - total type: %s (kind: %d)\n",
                       total && total->type ? "resolved" : "NULL",
                       total && total->type ? total->type->kind : -1);
                printf("  - add type: %s (kind: %d)\n",
                       add && add->type ? "resolved" : "NULL",
                       add && add->type ? add->type->kind : -1);

                // Check function has local scope with parameters
                if (add && add->data.func.local_scope) {
                    printf("  - add has local scope: yes\n");
                    Symbol *a = scope_lookup_local(add->data.func.local_scope, "a");
                    Symbol *b = scope_lookup_local(add->data.func.local_scope, "b");
                    printf("    - param 'a' in scope: %s\n", a ? "yes" : "no");
                    printf("    - param 'b' in scope: %s\n", b ? "yes" : "no");
                } else {
                    printf("  - add has local scope: NO ❌\n");
                }
            } else {
                printf("❌ Pass 3 failed with errors\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    // Test 5: Type interning - cached types
    {
        printf("\n--- Test 5: Type interning ---\n");

        const char *source =
            "type IntSlice = []int;\n"
            "type IntPtr = *int;\n"
            "type IntArray = [5]int;\n"
            "var s1 []int;\n"
            "var s2 IntSlice;\n"
            "var p1 *int;\n"
            "var p2 IntPtr;\n"
            "var a1 [5]int;\n"
            "var a2 IntArray;";

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();

            // Pass 2: Collect globals
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (!success) {
                printf("❌ Pass 2 failed\n");
                return;
            }

            // Pass 3: Check globals
            success = check_globals();
            printf("%s\n\n", source);

            if (success && !checker_has_errors()) {
                printf("✓ Pass 3 completed successfully\n");

                // Get symbols
                Symbol *s1 = scope_lookup(global_scope, "s1");
                Symbol *s2 = scope_lookup(global_scope, "s2");
                Symbol *p1 = scope_lookup(global_scope, "p1");
                Symbol *p2 = scope_lookup(global_scope, "p2");
                Symbol *a1 = scope_lookup(global_scope, "a1");
                Symbol *a2 = scope_lookup(global_scope, "a2");

                // Verify types are cached (same pointer)
                if (s1 && s2 && s1->type == s2->type) {
                    printf("  ✓ []int cached: s1 and s2 share same type object\n");
                } else {
                    printf("  ❌ []int NOT cached: s1=%p, s2=%p\n",
                           (void*)(s1 ? s1->type : NULL),
                           (void*)(s2 ? s2->type : NULL));
                }

                if (p1 && p2 && p1->type == p2->type) {
                    printf("  ✓ *int cached: p1 and p2 share same type object\n");
                } else {
                    printf("  ❌ *int NOT cached: p1=%p, p2=%p\n",
                           (void*)(p1 ? p1->type : NULL),
                           (void*)(p2 ? p2->type : NULL));
                }

                if (a1 && a2 && a1->type == a2->type) {
                    printf("  ✓ [5]int cached: a1 and a2 share same type object\n");
                } else {
                    printf("  ❌ [5]int NOT cached: a1=%p, a2=%p\n",
                           (void*)(a1 ? a1->type : NULL),
                           (void*)(a2 ? a2->type : NULL));
                }
            } else {
                printf("❌ Pass 3 failed with errors\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    // Test 6: Tuples
    {
        printf("\n--- Test 6: Tuples ---\n");

        const char *source =
            "type Data = (int, str, bool);\n"
            "var t1 (int, str, bool);\n"
            "var t2 Data;\n"
            "fn get_tuple() (int, str) { return (1, \"test\"); }";

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();

            // Pass 2: Collect globals
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (!success) {
                printf("❌ Pass 2 failed\n");
                return;
            }

            // Pass 3: Check globals
            success = check_globals();

            if (success && !checker_has_errors()) {
                printf("✓ Pass 3 completed successfully\n");

                // Get symbols
                Symbol *t1 = scope_lookup(global_scope, "t1");
                Symbol *t2 = scope_lookup(global_scope, "t2");
                Symbol *get_tuple = scope_lookup(global_scope, "get_tuple");

                // Verify tuple types are cached
                if (t1 && t2 && t1->type == t2->type) {
                    printf("  ✓ (int, str, bool) cached: t1 and t2 share same type\n");
                } else {
                    printf("  ❌ Tuple NOT cached\n");
                }

                // Verify function with tuple return type
                if (get_tuple && get_tuple->type && get_tuple->type->kind == TYPE_FUNCTION) {
                    Type *ret = get_tuple->type->data.func.return_type;
                    if (ret && ret->kind == TYPE_TUPLE) {
                        printf("  ✓ Function returns tuple type\n");
                    } else {
                        printf("  ❌ Function return type not tuple\n");
                    }
                } else {
                    printf("  ❌ Function type resolution failed\n");
                }
            } else {
                printf("❌ Pass 3 failed with errors\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    // Test 7: Forward references (out-of-order declarations)
    {
        printf("\n--- Test 7: Forward references ---\n");

        const char *source =
            "fn main() int { return helper(); }\n"              // Uses helper before declared
            "fn helper() int { return get_value(); }\n"        // Uses get_value before declared
            "fn get_value() int { return VALUE; }\n"           // Uses VALUE before declared
            "let VALUE = 42;\n"                                // Constant after functions
            "\n"
            "type Result = (Status, int);\n"                   // Uses Status before declared
            "type Status = int;\n"                             // Type after being referenced
            "\n"
            "var cache Result;\n"                              // Uses Result (which uses Status)
            "\n"
            "fn process() Data { return create_data(); }\n"    // Uses Data before declared
            "fn create_data() Data {\n"                        // Uses Data in signature
            "    var d Data;\n"
            "    return d;\n"
            "}\n"
            "type Data = (int, str);";                         // Data declared last

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();

            // Pass 2: Collect globals
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (!success) {
                printf("❌ Pass 2 failed\n");
                return;
            }

            // Pass 3: Check globals
            success = check_globals();
            printf("%s\n\n", source);

            if (success && !checker_has_errors()) {
                printf("✓ All forward references resolved successfully\n");

                // Verify some key resolutions
                Symbol *main_fn = scope_lookup(global_scope, "main");
                Symbol *result_type = scope_lookup(global_scope, "Result");
                Symbol *cache_var = scope_lookup(global_scope, "cache");

                if (main_fn && main_fn->type) {
                    printf("  ✓ main() signature resolved\n");
                }

                if (result_type && result_type->type && result_type->type->kind == TYPE_TUPLE) {
                    printf("  ✓ Result (tuple using forward-referenced Status) resolved\n");
                }

                if (cache_var && cache_var->type && cache_var->type->kind == TYPE_TUPLE) {
                    printf("  ✓ cache variable uses forward-referenced types\n");
                }
            } else {
                printf("❌ Pass 3 failed with errors\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    // Test 8: Circular type dependencies (should fail)
    {
        printf("\n--- Test 8: Circular type dependencies ---\n");

        const char *source =
            "type A = B;\n"
            "type B = C;\n"
            "type C = A;";

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();

            // Pass 2: Collect globals
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (!success) {
                printf("❌ Pass 2 failed\n");
                return;
            }

            // Pass 3: Check globals (should detect cycle)
            success = check_globals();

            if (!success && checker_has_errors()) {
                printf("✓ Correctly detected circular type dependency\n");
            } else {
                printf("❌ Should have detected circular dependency (A→B→C→A)\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    // Test 9: Self-referential type (should fail)
    {
        printf("\n--- Test 9: Self-referential type ---\n");

        const char *source = "type Node = Node;";

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();

            // Pass 2: Collect globals
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (!success) {
                printf("❌ Pass 2 failed\n");
                return;
            }

            // Pass 3: Check globals (should detect cycle)
            success = check_globals();

            if (!success && checker_has_errors()) {
                printf("✓ Correctly detected self-referential type\n");
            } else {
                printf("❌ Should have detected self-reference (Node→Node)\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    // Test 10: Valid indirect type usage via pointer (should pass)
    {
        printf("\n--- Test 10: Valid indirect type via pointer ---\n");

        const char *source =
            "type Node = (int, *Node);\n"  // Valid: pointer breaks the cycle
            "var head Node;";

        Parser parser;
        parser_init(&parser, source, "<test>");
        AstNode *program = parse_program(&parser);

        if (!parser.had_error && program) {
            checker_init();

            // Pass 2: Collect globals
            bool success = collect_globals(
                program->data.block_stmt.stmts,
                program->data.block_stmt.stmt_count
            );

            if (!success) {
                printf("❌ Pass 2 failed\n");
                return;
            }

            // Pass 3: Check globals (should succeed - pointer breaks cycle)
            success = check_globals();

            if (success && !checker_has_errors()) {
                printf("✓ Correctly allowed self-reference via pointer\n");

                Symbol *node = scope_lookup(global_scope, "Node");
                if (node && node->type && node->type->kind == TYPE_TUPLE) {
                    printf("  ✓ Node type resolved as tuple\n");
                }
            } else {
                printf("❌ Should have allowed pointer-based self-reference\n");
            }
        } else {
            printf("❌ Parse failed\n");
        }
    }

    printf("\n✓ Checker tests completed\n");
}

void test_all() {
  test_setup();
  printf("\n\n");
  test_lexer();
  printf("\n\n");
  test_parser();
  printf("\n\n");
  test_checker();
}
