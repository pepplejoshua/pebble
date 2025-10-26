#include "symbol.h"
#include "alloc.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

// External allocators
extern Arena long_lived;

// Global variables
Scope *global_scope = NULL;
Scope *anonymous_funcs = NULL;
Scope *current_scope = NULL;

// Create a new scope
Scope *scope_create(Scope *parent) {
  Scope *scope = arena_alloc(&long_lived, sizeof(Scope));
  scope->parent = parent;
  scope->symbols = NULL; // uthash starts as NULL
  scope->children = NULL;
  scope->child_count = 0;
  scope->child_capacity = 0;

  // Add to parent's children if parent exists
  if (parent) {
    scope_add_child(parent, scope);
  }

  return scope;
}

// Add child scope to parent
void scope_add_child(Scope *parent, Scope *child) {
  assert(parent && child);

  // Grow children array if needed
  if (parent->child_count >= parent->child_capacity) {
    size_t new_capacity =
        parent->child_capacity == 0 ? 4 : parent->child_capacity * 2;
    Scope **new_children =
        arena_alloc(&long_lived, new_capacity * sizeof(Scope *));

    if (parent->children) {
      memcpy(new_children, parent->children,
             parent->child_count * sizeof(Scope *));
    }

    parent->children = new_children;
    parent->child_capacity = new_capacity;
  }

  parent->children[parent->child_count++] = child;
}

// Look up symbol in this scope and parent scopes
Symbol *scope_lookup(Scope *scope, const char *name) {
  while (scope) {
    Symbol *symbol = scope_lookup_local(scope, name);
    if (symbol)
      return symbol;
    scope = scope->parent;
  }
  return NULL;
}

// Look up symbol only in this scope (not parents)
Symbol *scope_lookup_local(Scope *scope, const char *name) {
  if (!scope || !name)
    return NULL;

  Symbol *symbol;
  HASH_FIND_STR(scope->symbols, name, symbol);
  return symbol;
}

// Add symbol to scope
void scope_add_symbol(Scope *scope, Symbol *symbol) {
  assert(scope && symbol);
  HASH_ADD_STR(scope->symbols, name, symbol);
}

// Create a new symbol
Symbol *symbol_create(const char *name, SymbolKind kind, AstNode *decl) {
  Symbol *symbol = arena_alloc(&long_lived, sizeof(Symbol));
  symbol->name = str_dup(name);
  symbol->kind = kind;
  symbol->decl = decl;
  symbol->type = NULL; // Filled in pass 3

  // Initialize kind-specific data
  switch (kind) {
  case SYMBOL_FUNCTION:
  case SYMBOL_EXTERN_FUNCTION:
  case SYMBOL_ANON_FUNCTION:
    symbol->data.func.local_scope = NULL; // Created later
    break;
  case SYMBOL_VARIABLE:
    symbol->data.var.is_global = false; // Set by caller
    break;
  case SYMBOL_CONSTANT:
    symbol->data.const_data.is_evaluated = false;
    break;
  case SYMBOL_TYPE:
    // Nothing to initialize
    break;
  }

  return symbol;
}

// Initialize the symbol table system
void symbol_table_init(void) {
  global_scope = scope_create(NULL); // No parent
  anonymous_funcs = scope_create(NULL);
  current_scope = global_scope;
}

// Push a new current scope
void scope_push(Scope *scope) {
  assert(scope);
  current_scope = scope;
}

// Pop back to parent scope
void scope_pop(void) {
  assert(current_scope && current_scope->parent);
  HASH_CLEAR(hh, current_scope->symbols);
  current_scope = current_scope->parent;
}
