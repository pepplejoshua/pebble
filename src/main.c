#include "alloc.h"
#include "checker.h"
#include "codegen.h"
#include "module.h"
#include "options.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Global allocators
Arena long_lived;

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
  printf("Compiling: %s\n", filename);

  Module *main_mod = new_module(filename);
  main_mod->is_main = true;
  bool module_had_error = parse_module(main_mod);

  // Path of the main module
  // Need this for relative qualified module names
  root_directory = main_mod->abs_dir_path;

  if (module_had_error) {
    printf("Compilation failed due to parse errors in %s.\n",
           main_mod->filename);
    return false;
  }

  // track the main module
  track_module(main_mod);

  if (!collect_all_modules(main_mod)) {
    module_table_cleanup();
    return false;
  }

  qualify_globals_in_module(main_mod);

  // Phase 3: Type checking
  checker_init(main_mod);

  size_t total_modules = 0;
  ModuleEntry *cur, *tmp;

  HASH_ITER(hh, module_table, cur, tmp) {
    Module *mod = cur->module;
    checker_set_current_module(mod);

    total_modules++;

    if (!collect_globals(mod->ast->data.block_stmt.stmts,
                         mod->global_node_count)) {
      printf("Compilation failed during symbol collection in %s\n", mod->filename);
      module_table_cleanup();
      return false;
    }
  }

  Module **all_modules = arena_alloc(&long_lived, total_modules * sizeof(Module *));
  {
    size_t i = 0;
    HASH_ITER(hh, module_table, cur, tmp) {
      all_modules[i++] = cur->module;
    }
  }

  // Sort by import_score N -> 0
  sort_modules(all_modules, total_modules);

  type_system_init();

  for (size_t i = 0; i < total_modules; i++) {
    Module *mod = all_modules[i];
    checker_set_current_module(mod);

    // Pass 3: Type check globals
    if (!check_globals(mod)) {
      printf("Compilation failed during type checking\n");
      module_table_cleanup();
      return false;
    }

    // Pass 4: Type check function bodies
    if (!check_function_bodies()) {
      printf("Compilation failed during function body checking\n");
      module_table_cleanup();
      return false;
    }
  }

  if (!check_anonymous_functions()) {
    printf("Compilation failed during anonymous function body checking\n");
    return false;
  }

  // Pass 5: Verify entry point exists and has correct signature
  if (!verify_entry_point(main_mod)) {
    printf("Compilation failed during entry point verification\n");
    module_table_cleanup();
    return false;
  }

  // Debug: Print type information
  // debug_print_type_table();

  printf("Compilation successful!\n");

  if (compiler_opts.check_only) {
    return true;
  }

  Codegen cg;
  const char *c_filename = compiler_opts.output_c_name;
  FILE *output = fopen(c_filename, "w");
  codegen_init(&cg, output);
  // codegen_init(&cg, stdout);
  emit_program(&cg, main_mod);
  // printf("\n");
  fclose(output);
  printf("Generated %s\n", c_filename);

  module_table_cleanup();

  if (compiler_opts.generate_only) {
    return true;
  }

  int gcc_result = 0;
  // Conditional defaults
  char default_compiler_args[256];
  bool is_gcc = (strstr(compiler_opts.compiler, "gcc") != NULL);
  if (compiler_opts.warnings) {
    if (is_gcc) {
      snprintf(default_compiler_args, sizeof(default_compiler_args),
               "%s -Wall -Wextra -Wno-discarded-qualifiers", c_filename);
    } else {
      snprintf(default_compiler_args, sizeof(default_compiler_args),
               "%s -Wall -Wextra", c_filename);
    }
  } else {
    snprintf(default_compiler_args, sizeof(default_compiler_args), "%s -w",
             c_filename);
  }

  char compiler_args[1024 * 2] = {0};

  char *libraries = flatten_strings(compiler_opts.linked_libraries,
                                    compiler_opts.linked_libraries_count, 'l');

  char *lib_paths = flatten_strings(compiler_opts.lib_paths,
                                    compiler_opts.lib_paths_count, 'L');

  char *include_paths = flatten_strings(compiler_opts.include_paths,
                                        compiler_opts.include_paths_count, 'I');

  if (!compiler_opts.has_main) {
    // Compile to object file (.o) instead of executable
    char obj_filename[256];
    snprintf(obj_filename, sizeof(obj_filename), "%s.o",
             compiler_opts.output_exe_name);

    switch (compiler_opts.library) {
    case LIBRARY_NONE: {
      snprintf(compiler_args, sizeof(compiler_args), "%s -c %s -o %s %s",
               compiler_opts.compiler, default_compiler_args, obj_filename,
               release_mode_string());
      break;
    }

    case LIBRARY_SHARED: {
      // Use -c flag to compile to object file
      snprintf(compiler_args, sizeof(compiler_args),
               "%s -shared -fPIC -c %s -o %s %s", compiler_opts.compiler,
               default_compiler_args, obj_filename, release_mode_string());
      break;
    }

    case LIBRARY_STATIC: {
      // TODO
      break;
    }
    }

    if (compiler_opts.verbose) {
      printf("Compiling to object file: %s\n", compiler_args);
    }

    if (include_paths) {
      strcat(compiler_args, " ");
      strcat(compiler_args, include_paths);
    }

    if (lib_paths) {
      strcat(compiler_args, " ");
      strcat(compiler_args, lib_paths);
    }

    if (libraries) {
      strcat(compiler_args, " ");
      strcat(compiler_args, libraries);
    }

    int result = system(compiler_args);
    if (result != 0) {
      printf("Compilation to object file failed\n");
      return false;
    }

    if (!compiler_opts.keep_c_file) {
      char rm_cmd[256];
      snprintf(rm_cmd, sizeof(rm_cmd), "rm %s", c_filename);
      system(rm_cmd);
    }

    printf("Compiled to object file: %s\n", obj_filename);
    return true;
  }

  // Free standing should not be compiled at all
  if (!compiler_opts.freestanding) {
    // Compile as executable
    snprintf(compiler_args, sizeof(compiler_args), "%s %s -o %s %s",
             compiler_opts.compiler, default_compiler_args,
             compiler_opts.output_exe_name, release_mode_string());

    if (include_paths) {
      strcat(compiler_args, " ");
      strcat(compiler_args, include_paths);
    }

    if (lib_paths) {
      strcat(compiler_args, " ");
      strcat(compiler_args, lib_paths);
    }

    if (libraries) {
      strcat(compiler_args, " ");
      strcat(compiler_args, libraries);
    }

    if (compiler_opts.verbose) {
      printf("Compiling C: %s\n", compiler_args);
    }

    // Compile
    gcc_result = system(compiler_args);
  }

  if (gcc_result != 0) {
    printf("GCC compilation failed\n");
    return false;
  }

  if (!compiler_opts.keep_c_file) {
    char rm_cmd[512];
    snprintf(rm_cmd, sizeof(rm_cmd), "rm %s", c_filename);
    system(rm_cmd);
  }

  if (!compiler_opts.freestanding) {
    printf("Compiled to %s\n", compiler_opts.output_exe_name);
  }

  return true;
}

int main(int argc, char **argv) {
  // Initialize compiler systems
  arena_init(&long_lived, 256 * 1024);

  if (argc == 1) {
    print_usage(argv[0]);
    arena_free(&long_lived);
    return 0;
  }

  initialise_args();

  // Try to parse arguments
  if (!parse_args(argc, argv)) {
    cleanup_args();
    arena_free(&long_lived);
    return 1;
  }

  // Check if we have an input file
  if (compiler_opts.input_file == NULL) {
    fprintf(stderr, "Error: No input file specified\n");
    print_usage(argv[0]);
    cleanup_args();
    arena_free(&long_lived);
    return 1;
  }

  // Compile the source file
  if (!compile_file(compiler_opts.input_file)) {
    cleanup_args();
    arena_free(&long_lived);
    return 1;
  }

  // Report memory usage
  if (compiler_opts.verbose) {
    size_t used, capacity;
    arena_get_stats(&long_lived, &used, &capacity);
    printf("Memory used: %zu bytes (%.2f KB) out of %zu bytes (%.2f KB)\n",
           used, used / 1024.0, capacity, capacity / 1024.0);
  }

  // Cleanup
  cleanup_args();
  arena_free(&long_lived);
  return 0;
}
