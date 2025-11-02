#ifndef MODULE_H
#define MODULE_H

#include "ast.h"
#include "alloc.h"
#include "uthash.h"
#include "symbol.h"

extern char *root_directory;

typedef struct Module {
  char *name;                   // "main", "string"
  char *qualified_name;         // foo__bar__string
  char *filename;               // "main.peb", "string.peb"
  char *abs_file_path;          // "~/Desktop/Code/pebble/main.peb"
  char *abs_dir_path;           // "~/Desktop/Code/pebble"
  AstNode *ast;                 // Parsed program
  size_t global_node_count;
  bool is_main;                 // Entry point of compilation

  struct Module **imported_modules;  // Array of imported module qualified names
  size_t import_count;
  size_t import_capacity;

  size_t import_score; // +1 for each time imported
  Scope *scope;
} Module;

typedef struct ModuleEntry {
  char *module_abs_path;
  Module *module;
  UT_hash_handle hh; // Hash handle
} ModuleEntry;

extern ModuleEntry *module_table;

// Helpers for file/path/string operations
char *get_basename(const char* full_path, bool without_ext);
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

void append_module(Module *module, Module *imported);

Module *lookup_imported_module(Module *from_module, const char *name);

void module_increment_count(Module *module);

int compare_modules_desc(const void *a, const void *b);

// Example usage:
void sort_modules(Module **modules, size_t count);

// Clean up module table
void module_table_cleanup();
#endif
