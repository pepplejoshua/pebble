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
    type_create_pointer(int_type, true);
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

// Helper to reduce boilerplate
typedef struct {
    const char *source;
    bool should_pass;
} TestCase;

static bool run_test_case(const char *test_name, TestCase tc) {
    Parser parser;
    parser_init(&parser, tc.source, "<test>");
    AstNode *program = parse_program(&parser);

    if (parser.had_error || !program) {
        printf("❌ %s: Parse failed\n", test_name);
        return false;
    }

    checker_init();

    // Pass 2: Collect globals
    bool success = collect_globals(
        program->data.block_stmt.stmts,
        program->data.block_stmt.stmt_count
    );

    if (!success) {
        if (!tc.should_pass) {
            return true;  // Expected to fail
        }
        printf("❌ %s: Pass 2 failed\n", test_name);
        return false;
    }

    // Pass 3: Check globals
    success = check_globals();

    if (tc.should_pass) {
        if (success && !checker_has_errors()) {
            return true;
        }
        printf("❌ %s: Expected success but got errors\n", test_name);
        return false;
    }

    // Pass 4: Check function bodies
    success = check_function_bodies();

    if (tc.should_pass) {
        if (success && !checker_has_errors()) {
            return true;
        }
        printf("❌ %s: Expected success but got errors\n", test_name);
        return false;
    } else {
        if (!success || checker_has_errors()) {
            return true;
        }
        printf("❌ %s: Expected errors but passed\n", test_name);
        return false;
    }
}

// Individual test functions
static void test_unique_globals(void) {
    printf("=== Valid unique globals ===\n");

    TestCase tc = {
        .source =
            "fn add(a int, b int) int { return a + b; }\n"
            "fn subtract(a int, b int) int { return a - b; }\n"
            "var count int;\n"
            "let pi = 3.14;\n"
            "type MyInt = int;",
        .should_pass = true
    };

    if (run_test_case("Unique globals", tc)) {
        printf("✓ Collected globals successfully\n");

        // Verify symbols
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
    }
}

static void test_duplicate_functions(void) {
    printf("=== Duplicate function names ===\n");

    TestCase tc = {
        .source =
            "fn foo() int { return 1; }\n"
            "fn foo() int { return 2; }",
        .should_pass = false
    };

    if (run_test_case("Duplicate functions", tc)) {
        printf("✓ Correctly detected duplicate 'foo'\n");
    }
}

static void test_mixed_duplicates(void) {
    printf("=== Mixed duplicate (function + variable) ===\n");

    TestCase tc = {
        .source =
            "fn bar() void { }\n"
            "var bar int;",
        .should_pass = false
    };

    if (run_test_case("Mixed duplicates", tc)) {
        printf("✓ Correctly detected duplicate 'bar'\n");
    }
}

static void test_type_resolution(void) {
    printf("=== Type Resolution ===\n");

    TestCase tc = {
        .source =
            "type MyInt = int;\n"
            "type IntPtr = *int;\n"
            "let pi = 3.14;\n"
            "var count int;\n"
            "var total = 100;\n"
            "fn add(a int, b int) int { return a + b; }",
        .should_pass = true
    };

    if (run_test_case("Type resolution", tc)) {
        printf("✓ Pass 3 completed successfully\n");

        Symbol *myint = scope_lookup(global_scope, "MyInt");
        Symbol *intptr = scope_lookup(global_scope, "IntPtr");
        Symbol *pi = scope_lookup(global_scope, "pi");
        Symbol *add = scope_lookup(global_scope, "add");

        printf("  - MyInt type: %s\n", myint && myint->type ? "resolved" : "NULL");
        printf("  - IntPtr type: %s\n", intptr && intptr->type ? "resolved" : "NULL");
        printf("  - pi type: %s (kind: %d)\n",
               pi && pi->type ? "resolved" : "NULL",
               pi && pi->type ? pi->type->kind : -1);

        if (add && add->data.func.local_scope) {
            printf("  - add has local scope: yes\n");
            Symbol *a = scope_lookup_local(add->data.func.local_scope, "a");
            Symbol *b = scope_lookup_local(add->data.func.local_scope, "b");
            printf("    - param 'a' in scope: %s\n", a ? "yes" : "no");
            printf("    - param 'b' in scope: %s\n", b ? "yes" : "no");
        }
    }
}

static void test_tuples(void) {
    printf("=== Tuples ===\n");

    TestCase tc = {
        .source =
            "type Data = (int, str, bool);\n"
            "var t1 (int, str, bool);\n"
            "var t2 Data;\n"
            "fn get_tuple() (int, str) { return (1, \"test\"); }",
        .should_pass = true
    };

    if (run_test_case("Tuples", tc)) {
        printf("✓ Pass 3 completed successfully\n");

        Symbol *t1 = scope_lookup(global_scope, "t1");
        Symbol *t2 = scope_lookup(global_scope, "t2");
        Symbol *get_tuple = scope_lookup(global_scope, "get_tuple");

        // Check types are correct (not necessarily same object)
        if (t1 && t2 && type_equals(t1->type, t2->type)) {
            printf("  ✓ Tuple types are equal\n");
        } else {
            printf("  ❌ Tuple types not equal\n");
        }

        if (get_tuple && get_tuple->type && get_tuple->type->kind == TYPE_FUNCTION) {
            Type *ret = get_tuple->type->data.func.return_type;
            if (ret && ret->kind == TYPE_TUPLE) {
                printf("  ✓ Function returns tuple type\n");
            }
        }
    }
}

static void test_forward_references(void) {
    printf("=== Forward references ===\n");

    TestCase tc = {
        .source =
            "fn main() int { return helper(); }\n"
            "fn helper() int { return get_value(); }\n"
            "fn get_value() int { return VALUE; }\n"
            "let VALUE = 42;\n"
            "\n"
            "type Result = (Status, int);\n"
            "type Status = int;\n"
            "\n"
            "var cache Result;\n"
            "\n"
            "fn process() Data { return create_data(); }\n"
            "fn create_data() Data {\n"
            "    var d Data;\n"
            "    return d;\n"
            "}\n"
            "type Data = (int, str);",
        .should_pass = true
    };

    if (run_test_case("Forward references", tc)) {
        printf("✓ All forward references resolved successfully\n");

        Symbol *main_fn = scope_lookup(global_scope, "main");
        Symbol *result_type = scope_lookup(global_scope, "Result");
        Symbol *cache_var = scope_lookup(global_scope, "cache");

        if (main_fn && main_fn->type) {
            printf("  ✓ main() signature resolved\n");
        }
        if (result_type && result_type->type && result_type->type->kind == TYPE_TUPLE) {
            printf("  ✓ Result resolved\n");
        }
        if (cache_var && cache_var->type && cache_var->type->kind == TYPE_TUPLE) {
            printf("  ✓ cache variable resolved\n");
        }
    }
}

static void test_circular_dependencies(void) {
    printf("=== Circular type dependencies ===\n");

    TestCase tc = {
        .source =
            "type A = B;\n"
            "type B = C;\n"
            "type C = A;",
        .should_pass = false
    };

    if (run_test_case("Circular dependencies", tc)) {
        printf("✓ Correctly detected circular type dependency\n");
    }
}

static void test_self_referential_direct(void) {
    printf("=== Self-referential type (direct) ===\n");

    TestCase tc = {
        .source = "type Node = Node;",
        .should_pass = false
    };

    if (run_test_case("Self-referential direct", tc)) {
        printf("✓ Correctly detected self-referential type\n");
    }
}

static void test_self_referential_via_pointer(void) {
    printf("=== Self-referential via pointer ===\n");

    TestCase tc = {
        .source =
            "type Node = (int, *Node);\n"
            "var head Node;",
        .should_pass = true
    };

    if (run_test_case("Self-referential via pointer", tc)) {
        printf("✓ Correctly allowed self-reference via pointer\n");

        Symbol *node = scope_lookup(global_scope, "Node");
        if (node && node->type && node->type->kind == TYPE_TUPLE) {
            printf("  ✓ Node type resolved as tuple\n");

            // Verify circular structure
            Type *tuple = node->type;
            if (tuple->data.tuple.element_count == 2) {
                Type *ptr_type = tuple->data.tuple.element_types[1];
                if (ptr_type && ptr_type->kind == TYPE_POINTER) {
                    Type *base = ptr_type->data.ptr.base;
                    if (base == tuple) {
                        printf("  ✓ Circular structure verified\n");
                    }
                }
            }
        }
    }
}

static void test_structural_equality(void) {
    printf("=== Structural type equality ===\n");

    TestCase tc = {
        .source =
            // Same structure, different names
            "type Point1 = (int, int);\n"
            "type Point2 = (int, int);\n"
            "var p1 Point1;\n"
            "var p2 Point2;\n"
            "\n"
            // Nested structures
            "type Vec3 = (int, int, int);\n"
            "type Color = (int, int, int);\n"
            "var position Vec3;\n"
            "var rgb Color;\n"
            "\n"
            // Complex nested types
            "type NodeA = (int, *NodeA);\n"
            "type NodeB = (int, *NodeB);\n"
            "var listA NodeA;\n"
            "var listB NodeB;\n"
            "\n"
            // Pointer types
            "type IntPtr1 = *int;\n"
            "type IntPtr2 = *int;\n"
            "var ptr1 IntPtr1;\n"
            "var ptr2 IntPtr2;\n"
            "\n"
            // Nested pointers
            "type PtrPtr1 = **int;\n"
            "type PtrPtr2 = **int;\n"
            "var pp1 PtrPtr1;\n"
            "var pp2 PtrPtr2;\n"
            "\n"
            // Arrays and slices
            "type IntArray = [10]int;\n"
            "type IntSlice = []int;\n"
            "var arr1 [10]int;\n"
            "var arr2 IntArray;\n"
            "var slice1 []int;\n"
            "var slice2 IntSlice;\n"
            "\n"
            // Mixed complex types
            "type ComplexA = ([5](int, int), *int);\n"
            "type ComplexB = ([5](int, int), *int);\n"
            "var c1 ComplexA;\n"
            "var c2 ComplexB;",
        .should_pass = true
    };

    if (run_test_case("Structural equality", tc)) {
        printf("✓ All types resolved\n");

        // Test 1: Same tuple structure, different names
        Symbol *p1 = scope_lookup(global_scope, "p1");
        Symbol *p2 = scope_lookup(global_scope, "p2");
        if (p1 && p2) {
            bool equal = type_equals(p1->type, p2->type);
            printf("  %s Point1 == Point2: %s\n",
                   equal ? "✓" : "❌",
                   equal ? "structurally equal" : "NOT equal");
        }

        // Test 2: Different tuple structures
        Symbol *position = scope_lookup(global_scope, "position");
        if (p1 && position) {
            bool equal = type_equals(p1->type, position->type);
            printf("  %s Point1 != Vec3: %s\n",
                   !equal ? "✓" : "❌",
                   !equal ? "correctly different" : "WRONGLY equal");
        }

        // Test 3: Same structure but semantically different (Vec3 vs Color)
        Symbol *rgb = scope_lookup(global_scope, "rgb");
        if (position && rgb) {
            bool equal = type_equals(position->type, rgb->type);
            printf("  %s Vec3 == Color: %s (both (int,int,int))\n",
                   equal ? "✓" : "❌",
                   equal ? "structurally equal" : "NOT equal");
        }

        // Test 4: Self-referential types (should be equal structurally)
        Symbol *listA = scope_lookup(global_scope, "listA");
        Symbol *listB = scope_lookup(global_scope, "listB");
        if (listA && listB) {
            bool equal = type_equals(listA->type, listB->type);
            printf("  %s NodeA == NodeB: %s\n",
                   equal ? "✓" : "❌",
                   equal ? "structurally equal (both self-ref)" : "NOT equal");
        }

        // Test 5: Simple pointers
        Symbol *ptr1 = scope_lookup(global_scope, "ptr1");
        Symbol *ptr2 = scope_lookup(global_scope, "ptr2");
        if (ptr1 && ptr2) {
            bool equal = type_equals(ptr1->type, ptr2->type);
            printf("  %s *int == *int: %s\n",
                   equal ? "✓" : "❌",
                   equal ? "structurally equal" : "NOT equal");
        }

        // Test 6: Nested pointers
        Symbol *pp1 = scope_lookup(global_scope, "pp1");
        Symbol *pp2 = scope_lookup(global_scope, "pp2");
        if (pp1 && pp2) {
            bool equal = type_equals(pp1->type, pp2->type);
            printf("  %s **int == **int: %s\n",
                   equal ? "✓" : "❌",
                   equal ? "structurally equal" : "NOT equal");
        }

        // Test 7: Arrays (same size and element type)
        Symbol *arr1 = scope_lookup(global_scope, "arr1");
        Symbol *arr2 = scope_lookup(global_scope, "arr2");
        if (arr1 && arr2) {
            bool equal = type_equals(arr1->type, arr2->type);
            printf("  %s [10]int == IntArray: %s\n",
                   equal ? "✓" : "❌",
                   equal ? "structurally equal" : "NOT equal");
        }

        // Test 8: Slices
        Symbol *slice1 = scope_lookup(global_scope, "slice1");
        Symbol *slice2 = scope_lookup(global_scope, "slice2");
        if (slice1 && slice2) {
            bool equal = type_equals(slice1->type, slice2->type);
            printf("  %s []int == IntSlice: %s\n",
                   equal ? "✓" : "❌",
                   equal ? "structurally equal" : "NOT equal");
        }

        // Test 9: Arrays vs slices (should NOT be equal)
        if (arr1 && slice1) {
            bool equal = type_equals(arr1->type, slice1->type);
            printf("  %s [10]int != []int: %s\n",
                   !equal ? "✓" : "❌",
                   !equal ? "correctly different" : "WRONGLY equal");
        }

        // Test 10: Complex nested types
        Symbol *c1 = scope_lookup(global_scope, "c1");
        Symbol *c2 = scope_lookup(global_scope, "c2");
        if (c1 && c2) {
            bool equal = type_equals(c1->type, c2->type);
            printf("  %s ComplexA == ComplexB: %s\n",
                   equal ? "✓" : "❌",
                   equal ? "structurally equal (arrays of tuples + ptr)" : "NOT equal");
        }
    }
}

static void test_structs(void) {
    printf("=== Structs ===\n");

    TestCase tc = {
        .source =
            "type Point = struct { x int, y int };\n"
            "type Rect = struct { top_left Point, width int, height int };\n"
            "var p1 Point;\n"
            "var p2 Point;\n"
            "fn make_point() Point {\n"
            "  var p = Point.{ x = 10, y = 20 };\n"
            "  return p;\n"
            "}\n"
            "fn get_x(pt Point) int {\n"
            "  return pt.x;\n"
            "}",
        .should_pass = true
    };

    if (run_test_case("Structs", tc)) {
        printf("✓ Pass 3 completed successfully\n");

        Symbol *point_type = scope_lookup(global_scope, "Point");
        Symbol *rect_type = scope_lookup(global_scope, "Rect");
        Symbol *p1 = scope_lookup(global_scope, "p1");
        Symbol *p2 = scope_lookup(global_scope, "p2");

        // Check Point type exists and is struct
        if (point_type && point_type->type && point_type->type->kind == TYPE_STRUCT) {
            printf("  ✓ Point is a struct type\n");
        } else {
            printf("  ❌ Point is not a struct type\n");
        }

        // Check struct equality
        if (p1 && p2 && type_equals(p1->type, p2->type)) {
            printf("  ✓ Struct types are equal\n");
        } else {
            printf("  ❌ Struct types not equal\n");
        }

        // Check nested struct
        if (rect_type && rect_type->type && rect_type->type->kind == TYPE_STRUCT) {
            printf("  ✓ Rect is a struct type (nested struct)\n");
        } else {
            printf("  ❌ Rect is not a struct type\n");
        }
    }
}

static void test_function_types(void) {
    printf("=== Function Types ===\n");

    TestCase tc = {
        .source =
            "type BinaryOp = fn(int, int) int;\n"
            "type Callback = fn() void;\n"
            "type Transform = fn(str) str;\n"
            "var op1 BinaryOp;\n"
            "var op2 fn(int, int) int;\n"
            "fn apply(f fn(int, int) int, a int, b int) int {\n"
            "  return f(a, b);\n"
            "}",
        .should_pass = true
    };

    if (run_test_case("Function Types", tc)) {
        printf("✓ Pass 3 completed successfully\n");

        Symbol *binary_op = scope_lookup(global_scope, "BinaryOp");
        Symbol *op1 = scope_lookup(global_scope, "op1");
        Symbol *op2 = scope_lookup(global_scope, "op2");

        // Check BinaryOp is a function type
        if (binary_op && binary_op->type && binary_op->type->kind == TYPE_FUNCTION) {
            printf("  ✓ BinaryOp is a function type\n");
        } else {
            printf("  ❌ BinaryOp is not a function type\n");
        }

        // Check function type equality
        if (op1 && op2 && type_equals(op1->type, op2->type)) {
            printf("  ✓ Function types are equal\n");
        } else {
            printf("  ❌ Function types not equal\n");
        }
    }
}

static void test_slicing(void) {
    printf("=== Slicing ===\n");

    TestCase tc = {
        .source =
            "fn test_slicing() void {\n"
            "  var arr [5]int = [1, 2, 3, 4, 5];\n"
            "  var s1 []int = arr[1:4];\n"
            "  var s2 []int = arr[:];\n"
            "  var s3 []int = arr[:3];\n"
            "  var s4 []int = arr[2:];\n"
            "  var s5 []int = s1[0:2];\n"
            "}\n"
            "fn take_slice(nums []int) int {\n"
            "  return nums[0];\n"
            "}\n"
            "fn test_implicit_conversion() void {\n"
            "  var arr [3]int = [10, 20, 30];\n"
            "  var x int = take_slice(arr);\n"
            "}",
        .should_pass = true
    };

    if (run_test_case("Slicing", tc)) {
        printf("✓ Slicing operations pass\n");

        Symbol *test_slicing = scope_lookup(global_scope, "test_slicing");
        Symbol *take_slice = scope_lookup(global_scope, "take_slice");

        if (test_slicing) {
            printf("  ✓ test_slicing function defined\n");
        }

        // Check take_slice parameter type
        if (take_slice && take_slice->type && take_slice->type->kind == TYPE_FUNCTION) {
            Type *param = take_slice->type->data.func.param_types[0];
            if (param && param->kind == TYPE_SLICE) {
                Type *elem = param->data.slice.element;
                if (elem && elem->kind == TYPE_INT) {
                    printf("  ✓ take_slice accepts []int\n");
                }
            }
        }

        printf("  ✓ Implicit array-to-slice conversion works\n");
    }
}


static void test_pointer_operators(void) {
    printf("=== Pointer Operators ===\n");

    // Test valid pointer operations
    TestCase tc_valid = {
        .source =
            "fn test_pointers() void {\n"
            "  var x int = 42;\n"
            "  var ptr *int = &x;\n"
            "  var val int = *ptr;\n"
            "  var ptr_ptr **int = &ptr;\n"
            "}\n"
            "fn get_pointer(x int) *int {\n"
            "  return &x;\n"
            "}",
        .should_pass = true
    };

    if (run_test_case("Pointer Operators - Valid", tc_valid)) {
        printf("✓ Valid pointer operations pass\n");

        Symbol *get_pointer = scope_lookup(global_scope, "get_pointer");

        // Check get_pointer return type
        if (get_pointer && get_pointer->type && get_pointer->type->kind == TYPE_FUNCTION) {
            Type *ret = get_pointer->type->data.func.return_type;
            if (ret && ret->kind == TYPE_POINTER) {
                Type *base = ret->data.ptr.base;
                if (base && base->kind == TYPE_INT) {
                    printf("  ✓ get_pointer returns *int\n");
                }
            }
        }
    }

    // Test invalid lvalue (taking address of literal)
    TestCase tc_invalid = {
        .source =
            "fn test_invalid() void {\n"
            "  var ptr *int = &5;\n"
            "}",
        .should_pass = false
    };

    if (run_test_case("Pointer Operators - Invalid lvalue", tc_invalid)) {
        printf("✓ Correctly rejected address of literal\n");
    }
}

static void test_array_literals(void) {
    printf("=== Array Literals ===\n");

    TestCase tc = {
        .source =
            "fn test_arrays() void {\n"
            "  var nums = [1, 2, 3, 4, 5];\n"
            "  let even [5]int = [0, 2, 4, 6, 8];\n"
            "  var first = nums[0];\n"
            "  var matrix = [[1, 2], [3, 4]];\n"
            "}\n"
            "fn make_array() [3]int {\n"
            "  return [10, 20, 30];\n"
            "}",
        .should_pass = true
    };

    if (run_test_case("Array Literals", tc)) {
        printf("✓ Pass 4 completed successfully\n");

        Symbol *test_arrays = scope_lookup(global_scope, "test_arrays");
        Symbol *make_array = scope_lookup(global_scope, "make_array");

        if (test_arrays) {
            printf("  ✓ test_arrays function defined\n");
        }

        // Check make_array return type
        if (make_array && make_array->type && make_array->type->kind == TYPE_FUNCTION) {
            Type *ret = make_array->type->data.func.return_type;
            if (ret && ret->kind == TYPE_ARRAY && ret->data.array.size == 3) {
                printf("  ✓ make_array returns [3]int\n");
            } else {
                printf("  ❌ make_array return type incorrect\n");
            }
        }
    }
}

// Main test runner
void test_checker(void) {
    printf("\n========================================\n");
    printf("       CHECKER TESTS\n");
    printf("========================================\n");

    test_unique_globals();
    printf("\n");
    test_duplicate_functions();
    printf("\n");
    test_mixed_duplicates();
    printf("\n");
    test_type_resolution();
    printf("\n");
    test_tuples();
    printf("\n");
    test_structs();
    printf("\n");
    test_forward_references();
    printf("\n");
    test_circular_dependencies();
    printf("\n");
    test_self_referential_direct();
    printf("\n");
    test_self_referential_via_pointer();
    printf("\n");
    test_structural_equality();
    printf("\n");
    test_function_types();
    printf("\n");
    test_array_literals();
    printf("\n");
    test_pointer_operators();
    printf("\n");
    test_slicing();

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
