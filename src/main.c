#include "alloc.h"
#include "checker.h"
#include "codegen.h"
#include "parser.h"
#include "options.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

  char *buffer = arena_alloc(&long_lived, file_size + 1);
  if (!buffer) {
    fprintf(stderr, "Not enough memory to read file '%s'\n", path);
    fclose(file);
    return NULL;
  }

  size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
  if (bytes_read < file_size) {
    fprintf(stderr, "Could not read file '%s'\n", path);
    fclose(file);
    return NULL;
  }

  buffer[bytes_read] = '\0';
  fclose(file);
  return buffer;
}

// Debug function to print type table
// static void debug_print_type_table(void) {
//   printf("\n=== USER-DEFINED TYPES ===\n");

//   // Print built-ins
//   printf("'int' -> canonical: 'int'\n");
//   printf("'float' -> canonical: 'float'\n");
//   printf("'bool' -> canonical: 'bool'\n");
//   printf("'str' -> canonical: 'str'\n");
//   printf("'void' -> canonical: 'void'\n");

//   // Print user-defined types
//   Symbol *sym, *tmp;
//   HASH_ITER(hh, global_scope->symbols, sym, tmp) {
//     if (sym->kind == SYMBOL_TYPE) {
//       Type *t = type_lookup(sym->name);
//       printf("'%s' -> ", sym->name);
//       if (t && t->canonical_name) {
//         printf("canonical: '%s'\n", t->canonical_name);
//       } else {
//         printf("no canonical name yet\n");
//       }
//     }
//   }

//   printf("\n=== CANONICAL TYPES ===\n");

//   // Print canonical type table (deduplicated types)
//   TypeEntry *entry, *tmp2;
//   HASH_ITER(hh, canonical_type_table, entry, tmp2) {
//     printf("'%s'\n", entry->name);
//   }

//   printf("==========================\n\n");
// }

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
    return false;
  }

  // Phase 3: Type checking
  checker_init();

  // Pass 2: Collect globals
  if (!collect_globals(program->data.block_stmt.stmts,
                       program->data.block_stmt.stmt_count)) {
    printf("Compilation failed during symbol collection\n");
    return false;
  }

  // Pass 3: Type check globals
  if (!check_globals()) {
    printf("Compilation failed during type checking\n");
    return false;
  }

  // Pass 4: Type check function bodies
  if (!check_function_bodies()) {
    printf("Compilation failed during function body checking\n");
    // debug_print_type_table();
    return false;
  }

  // Debug: Print type information
  // debug_print_type_table();

  printf("Compilation successful!\n");

  Codegen cg;
  FILE *output = fopen("output.c", "w");
  codegen_init(&cg, output);
  // codegen_init(&cg, stdout);
  emit_program(&cg);
  // printf("\n");
  fclose(output);
  printf("Generated output.c\n");

  int gcc_result = 0;
  const char* default_compiler_args = "gcc output.c -Wall -Wextra -Wno-discarded-qualifiers";

  char compiler_args[1024];
  snprintf(compiler_args, sizeof(compiler_args), "%s -o %s %s", default_compiler_args, compiler_opts.output_name, release_mode_string());

  if (compiler_opts.freestanding) {
    char buffer[2048];
    snprintf(buffer, sizeof(buffer), "%s -ffreestanding", compiler_args);
    gcc_result = system(buffer);
  } else {
    // Compile with GCC
    gcc_result = system(compiler_args);
  }
  if (gcc_result != 0) {
    printf("GCC compilation failed\n");
    return false;
  }

  if (!compiler_opts.keep_c_file) {
    system("rm output.c");
  }

  printf("Compiled to output executable\n");

  return true;
}

int main(int argc, char **argv) {
  // Initialize compiler systems
  arena_init(&long_lived, 256 * 1024);

  if (argc == 1) {
    print_usage(argv[0]);
    return 0;
  }

  initialise_args();

  // Try to parse arguments
  if (!parse_args(argc, argv)) {
    return 1;
  }

  // Check if we have an input file
  if (compiler_opts.input_file == NULL) {
    fprintf(stderr, "Error: No input file specified\n");
    print_usage(argv[0]);
    return 1;
  }

  // Compile the source file
  if (!compile_file(compiler_opts.input_file)) {
    return 1;
  }

  // Report memory usage
  if (compiler_opts.verbose) {
    printf("Memory used: %zu bytes (%.2f KB) out of %zu bytes (%.2f KB)\n",
           long_lived.used, long_lived.used / 1024.0, long_lived.capacity,
           long_lived.capacity / 1024.0);
  }

  // Cleanup
  arena_free(&long_lived);
  return 0;
}
