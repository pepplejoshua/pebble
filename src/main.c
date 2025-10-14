#include "alloc.h"
#include "tests.h"
#include "parser.h"
#include "checker.h"
#include "type.h"
// #include "codegen.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Global allocators
Arena long_lived;

static char *read_file(const char *path) {
    FILE *file = fopen(path, "rb");
    if (!file) {
        fprintf(stderr, "Could not open file '%s'\n", path);
        return NULL;
    }

    fseek(file, 0L, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    char *buffer = malloc(file_size + 1);
    if (!buffer) {
        fprintf(stderr, "Not enough memory to read file '%s'\n", path);
        fclose(file);
        return NULL;
    }

    size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
    if (bytes_read < file_size) {
        fprintf(stderr, "Could not read file '%s'\n", path);
        fclose(file);
        free(buffer);
        return NULL;
    }

    buffer[bytes_read] = '\0';
    fclose(file);
    return buffer;
}

// Debug function to print type table
static void debug_print_type_table(void) {
    printf("\n=== USER-DEFINED TYPES ===\n");

    // Print built-ins
    printf("'int' -> canonical: 'int'\n");
    printf("'float' -> canonical: 'float'\n");
    printf("'bool' -> canonical: 'bool'\n");
    printf("'str' -> canonical: 'str'\n");
    printf("'void' -> canonical: 'void'\n");

    // Print user-defined types
    Symbol *sym, *tmp;
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        if (sym->kind == SYMBOL_TYPE) {
            Type *t = type_lookup(sym->name);
            printf("'%s' -> ", sym->name);
            if (t && t->canonical_name) {
                printf("canonical: '%s'\n", t->canonical_name);
            } else {
                printf("no canonical name yet\n");
            }
        }
    }

    printf("\n=== CANONICAL TYPES ===\n");

    // Print canonical type table (deduplicated types)
    TypeEntry *entry, *tmp2;
    HASH_ITER(hh, canonical_type_table, entry, tmp2) {
        printf("'%s'\n", entry->name);
    }

    printf("==========================\n\n");
}

// Function to compile a source file
// Function to compile a source file
static bool compile_file(const char *filename) {
    // Read the source file
    char *source = read_file(filename);
    if (!source) {
        return false;
    }

    printf("Compiling: %s\n", filename);

    // Phase 1 and 2: Lexical analysis and Parsing
    Parser parser;
    parser_init(&parser, source, filename);
    AstNode *program = parse_program(&parser);

    if (parser.had_error) {
        printf("Compilation failed due to parse errors\n");
        free(source);
        return false;
    }

    // Phase 3: Type checking
    checker_init();

    // Pass 2: Collect globals
    if (!collect_globals(program->data.block_stmt.stmts, program->data.block_stmt.stmt_count)) {
        printf("Compilation failed during symbol collection\n");
        free(source);
        return false;
    }

    // Pass 3: Type check globals
    if (!check_globals()) {
        printf("Compilation failed during type checking\n");
        free(source);
        return false;
    }

    // Pass 4: Type check function bodies
    if (!check_function_bodies()) {
        printf("Compilation failed during function body checking\n");
        free(source);
        debug_print_type_table();
        return false;
    }

    // Debug: Print type information
    debug_print_type_table();

    // Pass 5: Code generation
    // FILE *output = fopen("output.c", "w");
    // if (!output) {
        // printf("Failed to open output.c\n");
        // return false;
    // }

    // Codegen cg;
    // codegen_init(&cg, output);
    // emit_program(&cg, program);

    // fclose(output);
    // printf("Generated output.c\n");

    printf("Compilation successful!\n");

    free(source);
    return true;
}



int main(int argc, char **argv) {
    // Initialize compiler systems
    arena_init(&long_lived, 256 * 1024);

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
            // printf("Compiling: %s\n", argv[1]);
            if (!compile_file(argv[1])) {
                return 1;
            }
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


    // Report memory usage
    printf("Memory used: %zu bytes (%.2f KB) out of %zu bytes (%.2f KB)\n",
            long_lived.used, long_lived.used / 1024.0,
            long_lived.capacity, long_lived.capacity / 1024.0);

    // Cleanup
    arena_free(&long_lived);
    return 0;
}
