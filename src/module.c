#include "module.h"
#include "alloc.h"
#include "ast.h"
#include "parser.h"
#include "uthash.h"
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External allocator
extern Arena long_lived;

// Module table for tracking parsed modules
ModuleEntry *module_table = NULL;

// Helpers
char *get_basename(const char *full_path, bool without_ext) {
  // Make a copy to avoid modifying the original
  char *path_copy = str_dup(full_path);
  if (!path_copy) {
    return NULL; // Arena memory allocation failed
  }

  // Find the last '/' to get the filename part
  char *filename = strrchr(path_copy, '/');
  if (filename) {
    filename++; // skip the '/'
  } else {
    filename = path_copy; // No directory, so use whole string
  }

  if (without_ext) {
    // Find the '.' in the filename to remove extension
    char *dot = strrchr(filename, '.');
    if (dot &&
        dot != filename) { // Avoid removing the leading dot (e.g., .hidden)
      *dot = '\0';
    }
  }

  // Allocate and return a copy of the result
  char *result = str_dup(filename);
  return result;
}

char *get_absolute_path(const char *partial_path) {
  char resolved_path[PATH_MAX];

  if (realpath(partial_path, resolved_path) == NULL) {
    printf("failed to get absolute path of '%s'", partial_path);
    return NULL;
  }

  return str_dup(resolved_path);
}

Module *new_module(const char *partial_path) {
  char *module_abs_path = get_absolute_path(partial_path);
  char *module_filename = get_basename(module_abs_path, false);
  char *module_name = get_basename(module_abs_path, true);

  Module *new_module = arena_alloc(&long_lived, sizeof(Module));
  new_module->abs_file_path = module_abs_path;
  new_module->filename = module_filename;
  new_module->name = module_name;
  new_module->ast = NULL;
  new_module->global_node_count = 0;

  return new_module;
}

char *read_file(const char *filepath) {
  FILE *file = fopen(filepath, "rb");
  if (!file) {
    fprintf(stderr, "Could not open file '%s'\n", filepath);
    return NULL;
  }

  fseek(file, 0L, SEEK_END);
  size_t file_size = ftell(file);
  rewind(file);

  char *buffer = arena_alloc(&long_lived, file_size + 1);
  if (!buffer) {
    fprintf(stderr, "Not enough memory to read file '%s'\n", filepath);
    fclose(file);
    return NULL;
  }

  size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
  if (bytes_read < file_size) {
    fprintf(stderr, "Could not read file '%s'\n", filepath);
    fclose(file);
    return NULL;
  }

  buffer[bytes_read] = '\0';
  fclose(file);
  return buffer;
}

bool parse_module(Module *mod) {
  char *source = read_file(mod->abs_file_path);
  if (!source) {
    return false;
  }

  Parser parser;
  parser_init(&parser, source, mod->filename);
  AstNode *program = parse_program(&parser);
  mod->ast = program;
  mod->global_node_count = program->data.block_stmt.stmt_count;

  return parser.had_error;
}

void module_error(Location loc, const char *msg) {
  fprintf(stderr, "%s:%d:%d: %s\n", loc.file, loc.line, loc.column, msg);
}

Module *get_module_from_table(const char *full_path) {
  ModuleEntry *exists;
  HASH_FIND_STR(module_table, full_path, exists);

  return exists ? exists->module : NULL;
}

void track_module(Module *new_mod) {
  if (!get_module_from_table(new_mod->abs_file_path)) {
    ModuleEntry *entry = arena_alloc(&long_lived, sizeof(ModuleEntry));
    entry->module_abs_path = new_mod->abs_file_path;
    entry->module = new_mod;
    HASH_ADD_STR(module_table, module_abs_path, entry);
  }
}

bool collect_all_modules(Module *cur) {
  // Go through global ast nodes and collect all import statements
  AstNode *program = cur->ast;
  size_t globals_count = program->data.block_stmt.stmt_count;
  AstNode **globals = program->data.block_stmt.stmts;

  // For each import, we make sure:
  for (size_t i = 0; i < globals_count; i++) {
    AstNode *node = globals[i];
    if (node->kind != AST_DECL_IMPORT)
      continue;

    char *path = node->data.import_stmt.path_str->data.str_lit.value;
    char *code_file_path = prepend(path, ".peb");

    // 1. We expand it to absolute path
    char *mod_path = get_absolute_path(code_file_path);

    // 2. We are not self-importing
    // (report error and return false)
    if (!mod_path) {
      return false;
    }
    if (strcmp(mod_path, cur->abs_file_path) == 0) {
      module_error(node->loc, "A module cannot import itself.");
      return false;
    }

    // 3. We are not importing main
    // (report error and return false)
    Module *exists = get_module_from_table(mod_path);
    if (exists) {
      if (exists->is_main) {
        module_error(node->loc,
                     "A module cannot import the entry / main module.");
        return false;
      }
      continue; // We already know of this file
    }

    // 4. We make a new module for the new path and we parse it
    Module *new_mod = new_module(mod_path);
    bool module_had_error = parse_module(new_mod);
    if (module_had_error) {
      printf("Compilation failed due to parse errors in %s.\n",
             new_mod->filename);
      return false;
    }

    // 5. We track the new module
    track_module(new_mod);

    // 6. We call collect_all_modules on the new module
    if (!collect_all_modules(new_mod))
      return false;

    // 7. Qualify all global names in module
    qualify_globals_in_module(new_mod);
  }

  return true;
}

char *prepend(const char *prefix, const char *base) {
  if (!prefix || !base) {
    return NULL; // Handle NULL inputs gracefully
  }

  size_t prefix_len = strlen(prefix);
  size_t base_len = strlen(base);
  size_t total_len = prefix_len + base_len + 1; // +1 for null terminator

  char *result = arena_alloc(&long_lived, total_len);
  if (!result) {
    return NULL; // Allocation failed
  }

  // Copy prefix first
  strcpy(result, prefix);
  // Append base
  strcat(result, base);
  return result;
}

void qualify_globals_in_module(Module *mod) {
  // Go through all applicable global nodes and prepend
  // `mod->name::` to each of them
  AstNode *program = mod->ast;
  size_t globals_count = program->data.block_stmt.stmt_count;
  AstNode **globals = program->data.block_stmt.stmts;

  for (size_t i = 0; i < globals_count; i++) {
    AstNode *node = globals[i];

    if (node->kind == AST_DECL_IMPORT) {
      continue;
    }

    switch (node->kind) {
    case AST_DECL_FUNCTION: {
      char *cur_name = node->data.func_decl.name;
      // Do not qualify the main function
      if (mod->is_main && strcmp("main", cur_name) == 0) {
        continue;
      }
      char *prefix = prepend(mod->name, "__");
      node->data.func_decl.qualified_name = prepend(prefix, cur_name);
      break;
    }
    case AST_DECL_EXTERN_FUNC:
    case AST_DECL_EXTERN_TYPE:
    case AST_DECL_EXTERN_BLOCK:
      break;
    case AST_DECL_VARIABLE: {
      char *cur_name = node->data.var_decl.name;
      char *prefix = prepend(mod->name, "__");
      node->data.var_decl.qualified_name = prepend(prefix, cur_name);
      break;
    }
    case AST_DECL_CONSTANT: {
      char *cur_name = node->data.const_decl.name;
      char *prefix = prepend(mod->name, "__");
      node->data.const_decl.qualified_name = prepend(prefix, cur_name);
      break;
    }
    case AST_DECL_TYPE: {
      char *cur_name = node->data.type_decl.name;
      char *prefix = prepend(mod->name, "__");
      node->data.type_decl.qualified_name = prepend(prefix, cur_name);
      break;
    }
    default:
      continue;
    }
  }
}

AstNode *combine_modules() {
  ModuleEntry *cur, *tmp;

  size_t total_num_nodes = 0;

  HASH_ITER(hh, module_table, cur, tmp) {
    total_num_nodes += cur->module->global_node_count;
  }

  AstNode **complete_program =
      arena_alloc(&long_lived, total_num_nodes * sizeof(AstNode *));
  size_t prog_i = 0;

  HASH_ITER(hh, module_table, cur, tmp) {
    AstNode **cur_mod_ast = cur->module->ast->data.block_stmt.stmts;
    size_t cur_node_count = cur->module->global_node_count;

    for (size_t i = 0; i < cur_node_count; i++) {
      complete_program[prog_i] = cur_mod_ast[i];
      prog_i++;
    }
  }

  AstNode *prog = arena_alloc(&long_lived, sizeof(AstNode));
  prog->kind = AST_STMT_BLOCK;
  prog->data.block_stmt.stmt_count = total_num_nodes;
  prog->data.block_stmt.stmts = complete_program;
  return prog;
}

void module_table_cleanup() { HASH_CLEAR(hh, module_table); }
