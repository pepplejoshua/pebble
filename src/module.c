#include "module.h"
#include "alloc.h"
#include "ast.h"
#include "options.h"
#include "parser.h"
#include "symbol.h"
// #include "wrapped_uthash.h"
#include "uthash.h"
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#if defined(__APPLE__)
#include <mach-o/dyld.h>
#endif

// External allocator
extern Arena long_lived;

char *root_directory = NULL;

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

static char *get_executable_dir(void) {
  char path[PATH_MAX] = {0};

#if defined(__APPLE__)
  uint32_t size = sizeof(path);
  char *exe_path = path;

  if (_NSGetExecutablePath(path, &size) != 0) {
    // Buffer too small; allocate dynamically
    exe_path = arena_alloc(&long_lived, size);
    if (!exe_path)
      return NULL;
    if (_NSGetExecutablePath(exe_path, &size) != 0) {
      return NULL;
    }
  }

  // Resolve any symlinks to get the real absolute path (always)
  char resolved[PATH_MAX];
  if (realpath(exe_path, resolved) == NULL) {
    return NULL;
  }
  strncpy(path, resolved, sizeof(path));

#else
  ssize_t len = 0;
  len = readlink("/proc/self/exe", path, sizeof(path) - 1);
  if (len == -1) {
    return NULL;
  }
  path[len] = '\0';
#endif

  // Strip filename → keep directory only
  char *slash = strrchr(path, '/');
  if (slash) {
    *slash = '\0';
  }

  return str_dup(path);
}

static char *get_absolute_path(const char *path_literal,
                               const char *partial_path) {
  char resolved_path[PATH_MAX] = {0};

  size_t len = strlen(partial_path);
  // at least longer than "std:"
  if (len > 4 && strncmp(path_literal, "std:", 4) == 0) {
    if (!compiler_opts.std_path) {
      char *executable_dir = get_executable_dir();
      char *std_path = prepend(executable_dir, "/std/");
      char *module_path = prepend(std_path, path_literal + 4);
      char *code_path = prepend(module_path, ".peb");
      return code_path;
    } else {
      char *path = str_dup(compiler_opts.std_path);
      char *module_path = prepend(path, path_literal + 4);
      char *code_path = prepend(module_path, ".peb");

      printf("code_path = \"%s\"\n", code_path);
      return code_path;
    }
  }

  if (realpath(partial_path, resolved_path) == NULL) {
    fprintf(stderr, "Failed to get absolute path of '%s'\n", partial_path);
    return NULL;
  }

  return str_dup(resolved_path);
}

static char *get_absolute_directory(const char *filepath) {
  if (!filepath)
    return str_dup(".");

  char abs_path[4096] = {0};
  char *resolved = realpath(filepath, abs_path);
  if (!resolved) {
    // If file doesn't exist yet, try resolving its directory
    char tmp[4096];
    strncpy(tmp, filepath, sizeof(tmp) - 1);
    tmp[sizeof(tmp) - 1] = '\0';

    // Trim trailing slashes
    for (int i = strlen(tmp) - 1; i >= 0 && (tmp[i] == '/' || tmp[i] == '\\');
         --i) {
      tmp[i] = '\0';
    }

    // Remove filename part
    char *slash1 = strrchr(tmp, '/');
    char *slash2 = strrchr(tmp, '\\');
    char *slash = slash1 > slash2 ? slash1 : slash2;
    if (slash) {
      *slash = '\0';
    } else {
      strcpy(tmp, ".");
    }

    if (realpath(tmp, abs_path) == NULL) {
      // Even directory can't be resolved → just return "."
      return str_dup(".");
    }
  }

  // Strip off filename from absolute path
  char *slash1 = strrchr(abs_path, '/');
  char *slash2 = strrchr(abs_path, '\\');
  char *slash = slash1 > slash2 ? slash1 : slash2;

  if (slash) {
    *(slash + 1) = '\0';
  } else {
    strcpy(abs_path, ".");
  }

  return str_dup(abs_path);
}

Module *new_module(const char *partial_path) {
  char *module_abs_path = get_absolute_path(partial_path, partial_path);
  if (!module_abs_path) {
    return NULL;
  }
  char *module_abs_dir = get_absolute_directory(partial_path);
  char *module_filename = get_basename(module_abs_path, false);
  char *module_name = get_basename(module_abs_path, true);

  // Compute qualified name from root
  char *qualified_name = module_name; // Default to basename
  if (root_directory && module_abs_path) {
    size_t root_len = strlen(root_directory);
    size_t path_len = strlen(module_abs_path);

    // Check if module_abs_path starts with root_directory
    if (path_len > root_len &&
        strncmp(module_abs_path, root_directory, root_len) == 0) {
      // Skip root and leading slash
      char *relative = module_abs_path + root_len;
      if (relative[0] == '/')
        relative++;

      // Remove .peb extension and replace / with _
      size_t rel_len = strlen(relative);
      if (rel_len > 4 && strcmp(relative + rel_len - 4, ".peb") == 0) {
        rel_len -= 4;
      }

      qualified_name = arena_alloc(&long_lived, rel_len + 1);
      for (size_t i = 0; i < rel_len; i++) {
        qualified_name[i] = (relative[i] == '/') ? '_' : relative[i];
      }
      qualified_name[rel_len] = '\0';
    }
  }

  Module *new_module = arena_alloc(&long_lived, sizeof(Module));
  new_module->abs_file_path = module_abs_path;
  new_module->abs_dir_path = module_abs_dir;
  new_module->filename = module_filename;
  new_module->name = module_name;
  new_module->qualified_name = qualified_name;
  new_module->ast = NULL;
  new_module->global_node_count = 0;
  new_module->scope = scope_create(global_scope);

  new_module->imported_modules = arena_alloc(&long_lived, 2 * sizeof(Module *));
  new_module->import_count = 0;
  new_module->import_capacity = 2;
  new_module->import_score = 0;

  return new_module;
}

char *read_file(const char *filepath) {
  FILE *file = fopen(filepath, "rb");
  if (!file) {
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
    return true;
  }

  Parser parser;
  parser_init(&parser, source, mod->filename, mod->abs_file_path);
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
    char *resolved_path = prepend(cur->abs_dir_path, path);
    char *code_file_path = prepend(resolved_path, ".peb");

    // 1. We expand it to absolute path
    char *mod_path = get_absolute_path(path, code_file_path);

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

      append_module(cur, exists);

      module_increment_count(exists);

      continue; // We already know of this file
    }

    // 4. We make a new module for the new path and we parse it
    Module *new_mod = new_module(mod_path);
    bool module_had_error = parse_module(new_mod);
    if (module_had_error) {
      printf("Compilation failed due to parse errors in %s.\n", mod_path);
      return false;
    }

    // 5. We track the new module
    track_module(new_mod);

    append_module(cur, new_mod);

    // 6. We call collect_all_modules on the new module
    if (!collect_all_modules(new_mod))
      return false;

    module_increment_count(new_mod);

    // 7. Qualify all global names in module
    qualify_globals_in_module(new_mod);
  }

  return true;
}

void append_module(Module *module, Module *imported) {
  if (module->import_count >= module->import_capacity) {
    module->import_capacity *= 2;
    Module **new_modules =
        arena_alloc(&long_lived, module->import_capacity * sizeof(Module *));
    memcpy(new_modules, module->imported_modules,
           module->import_count * sizeof(Module *));

    module->imported_modules = new_modules;
  }

  module->imported_modules[module->import_count++] = imported;
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
      char *full_prefix = prepend(mod->qualified_name, "__");
      node->data.func_decl.qualified_name = prepend(prefix, cur_name);
      node->data.func_decl.full_qualified_name = prepend(full_prefix, cur_name);
      break;
    }
    case AST_DECL_EXTERN_VARIABLE: {
      char *cur_name = node->data.extern_var_decl.name;
      char *prefix = prepend(mod->name, "__");
      char *full_prefix = prepend(mod->qualified_name, "__");
      node->data.extern_var_decl.qualified_name = prepend(prefix, cur_name);
      node->data.extern_var_decl.full_qualified_name =
          prepend(full_prefix, cur_name);
      break;
    }
    case AST_DECL_EXTERN_CONSTANT: {
      char *cur_name = node->data.extern_const_decl.name;
      char *prefix = prepend(mod->name, "__");
      char *full_prefix = prepend(mod->qualified_name, "__");
      node->data.extern_const_decl.qualified_name = prepend(prefix, cur_name);
      node->data.extern_const_decl.full_qualified_name =
          prepend(full_prefix, cur_name);
      break;
    }
    case AST_DECL_EXTERN_FUNC: {
      char *cur_name = node->data.extern_func.name;
      if (mod->is_main && strcmp("main", cur_name) == 0) {
        continue;
      }
      char *prefix = prepend(mod->name, "__");
      char *full_prefix = prepend(mod->qualified_name, "__");
      node->data.extern_func.qualified_name = prepend(prefix, cur_name);
      node->data.extern_func.full_qualified_name =
          prepend(full_prefix, cur_name);
      break;
    }
    case AST_DECL_EXTERN_TYPE: {
      char *cur_name = node->data.extern_type.name;
      if (mod->is_main && strcmp("main", cur_name) == 0) {
        continue;
      }
      char *prefix = prepend(mod->name, "__");
      char *full_prefix = prepend(mod->qualified_name, "__");
      node->data.extern_type.qualified_name = prepend(prefix, cur_name);
      node->data.extern_type.full_qualified_name =
          prepend(full_prefix, cur_name);
      break;
    }
    case AST_DECL_EXTERN_BLOCK: {
      for (size_t i = 0; i < node->data.extern_block.decls_count; i++) {
        AstNode *decl = node->data.extern_block.decls[i];
        switch (decl->kind) {
        case AST_DECL_EXTERN_VARIABLE: {
          char *cur_name = decl->data.extern_var_decl.name;
          char *prefix = prepend(mod->name, "__");
          char *full_prefix = prepend(mod->qualified_name, "__");
          decl->data.extern_var_decl.qualified_name = prepend(prefix, cur_name);
          decl->data.extern_var_decl.full_qualified_name =
              prepend(full_prefix, cur_name);
          break;
        }
        case AST_DECL_EXTERN_CONSTANT: {
          char *cur_name = decl->data.extern_const_decl.name;
          char *prefix = prepend(mod->name, "__");
          char *full_prefix = prepend(mod->qualified_name, "__");
          decl->data.extern_const_decl.qualified_name =
              prepend(prefix, cur_name);
          decl->data.extern_const_decl.full_qualified_name =
              prepend(full_prefix, cur_name);
          break;
        }
        case AST_DECL_EXTERN_FUNC: {
          char *cur_name = decl->data.extern_func.name;
          if (mod->is_main && strcmp("main", cur_name) == 0) {
            continue;
          }
          char *prefix = prepend(mod->name, "__");
          char *full_prefix = prepend(mod->qualified_name, "__");
          decl->data.extern_func.qualified_name = prepend(prefix, cur_name);
          decl->data.extern_func.full_qualified_name =
              prepend(full_prefix, cur_name);
          break;
        }
        case AST_DECL_EXTERN_TYPE: {
          char *cur_name = decl->data.extern_type.name;
          if (mod->is_main && strcmp("main", cur_name) == 0) {
            continue;
          }
          char *prefix = prepend(mod->name, "__");
          char *full_prefix = prepend(mod->qualified_name, "__");
          decl->data.extern_type.qualified_name = prepend(prefix, cur_name);
          decl->data.extern_type.full_qualified_name =
              prepend(full_prefix, cur_name);
          break;
        }

        default:
          break;
        }
      }

      break;
    }
    case AST_DECL_VARIABLE: {
      char *cur_name = node->data.var_decl.name;
      char *prefix = prepend(mod->name, "__");
      char *full_prefix = prepend(mod->qualified_name, "__");
      node->data.var_decl.qualified_name = prepend(prefix, cur_name);
      node->data.var_decl.full_qualified_name = prepend(full_prefix, cur_name);
      break;
    }
    case AST_DECL_CONSTANT: {
      char *cur_name = node->data.const_decl.name;
      char *prefix = prepend(mod->name, "__");
      char *full_prefix = prepend(mod->qualified_name, "__");
      node->data.const_decl.qualified_name = prepend(prefix, cur_name);
      node->data.const_decl.full_qualified_name =
          prepend(full_prefix, cur_name);
      break;
    }
    case AST_DECL_TYPE: {
      char *cur_name = node->data.type_decl.name;
      char *prefix = prepend(mod->name, "__");
      char *full_prefix = prepend(mod->qualified_name, "__");
      node->data.type_decl.qualified_name = prepend(prefix, cur_name);
      node->data.type_decl.full_qualified_name = prepend(full_prefix, cur_name);
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

Module *lookup_imported_module(Module *from_module, const char *name) {
  if (!from_module) {
    return NULL;
  }

  for (size_t i = 0; i < from_module->import_count; i++) {
    if (strcmp(from_module->imported_modules[i]->name, name) == 0) {
      return from_module->imported_modules[i];
    }
  }

  return NULL;
}

void module_increment_count(Module *module) {
  // Increase score
  module->import_score++;

  // Increase its dependencies scores
  for (size_t i = 0; i < module->import_count; i++) {
    module_increment_count(module->imported_modules[i]);
  }
}

int compare_modules_desc(const void *a, const void *b) {
  const Module *ma = *(const Module **)a;
  const Module *mb = *(const Module **)b;

  // Sort from most to least (descending)
  return (mb->import_score - ma->import_score);
}

// Example usage:
void sort_modules(Module **modules, size_t count) {
  qsort(modules, count, sizeof(Module *), compare_modules_desc);
}

void module_table_cleanup() { HASH_CLEAR(hh, module_table); }
