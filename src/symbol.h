#ifndef SYMBOL_H
#define SYMBOL_H

#include "ast.h"
#include "uthash.h"
#include <stdbool.h>

// Forward declarations
typedef struct Symbol Symbol;
typedef struct Scope Scope;
typedef struct Type Type;

// Symbol kinds
typedef enum {
  SYMBOL_FUNCTION,
  SYMBOL_EXTERN_FUNCTION,
  SYMBOL_ANON_FUNCTION,
  SYMBOL_VARIABLE,
  SYMBOL_CONSTANT,
  SYMBOL_TYPE
} SymbolKind;

// Symbol structure
struct Symbol {
  char *name;      // Symbol name (allocated in arena)
  char *reg_name;
  SymbolKind kind; // What kind of symbol this is
  AstNode *decl;   // Pointer to declaration AST node
  Type *type;      // Type (NULL initially, filled in pass 3)

  // Kind-specific data
  union {
    struct {
      Scope *local_scope; // Function's local scope
    } func;

    struct {
      bool is_global; // Whether this is a global variable
                      // Storage info can be added later
    } var;

    struct {
      char *lib_name;
    } external;

    struct {
      bool is_evaluated; // Whether constant value is computed
      union {
        long long int_val;
        double float_val;
        bool bool_val;
        char *str_val;
      } value;
    } const_data;

    struct {
      // Type-specific data can be added later
    } type_data;
  } data;

  UT_hash_handle hh; // For uthash
};

// Scope structure
struct Scope {
  Scope *parent;         // Parent scope (NULL for global)
  Symbol *symbols;       // Hash table of symbols in this scope
  Scope **children;      // Array of child scopes
  size_t child_count;    // Number of child scopes
  size_t child_capacity; // Capacity of children array
};

// Scope management functions
Scope *scope_create(Scope *parent);
void scope_add_child(Scope *parent, Scope *child);
Symbol *scope_lookup(Scope *scope, char *name, const char *sym_mod_name);
Symbol *scope_lookup_local(Scope *scope,
                           const char *name); // Only in this scope
void scope_add_symbol(Scope *scope, Symbol *symbol);

// Symbol creation functions
Symbol *symbol_create(const char *name, SymbolKind kind, AstNode *decl);

// Global scope management
extern Scope *global_scope;
extern Scope *anonymous_funcs;
extern Scope *current_scope;

void symbol_table_init(void);
void scope_push(Scope *scope);
void scope_pop(void);

#endif
