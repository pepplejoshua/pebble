#ifndef MODULE_H
#define MODULE_H

#include "ast.h"
#include "alloc.h"
#include "uthash.h"

typedef struct Module {
  char *name;                    // "main", "string"
  char *filename;               // "main.peb", "string.peb"
  char *abs_file_path;          // "~/Desktop/Code/pebble/main.peb"
  AstNode *ast;                 // Parsed program
  size_t global_node_count;
  bool is_main;                 // Entry point of compilation
} Module;

typedef struct ModuleEntry {
  char *module_abs_path;
  Module *module;
  UT_hash_handle hh; // Hash handle
} ModuleEntry;

extern ModuleEntry *module_table;

// Helpers for file/path/string operations
char *get_basename(const char* full_path, bool without_ext);
char *get_absolute_path(const char* partial_path);
char *read_file(const char* filepath);
char *prepend(const char *prefix, const char *base);

// Module functions
Module* new_module(const char *partial_path);
bool parse_module(Module *mod);
void module_error(Location loc, const char *msg);

// Module table functions
Module* get_module_from_table(const char *full_path);
void track_module(Module *new_mod);
bool collect_all_modules(Module *mod);
void qualify_globals_in_module(Module *mod);
AstNode* combine_modules();

// Clean up module table
void module_table_cleanup();
#endif
