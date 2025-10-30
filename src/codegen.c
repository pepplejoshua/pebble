#include "codegen.h"
#include "alloc.h"
#include "ast.h"
#include "options.h"
#include "symbol.h"
#include "type.h"
#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

static void append_to_section(Codegen *cg, const char *str, size_t str_len) {
  char **buf = NULL;
  size_t *len, *cap;
  if (strcmp(cg->current_section, "forward_types") == 0) {
    buf = &cg->forward_types;
    len = &cg->forward_types_len;
    cap = &cg->forward_types_cap;
  } else if (strcmp(cg->current_section, "type_defs") == 0) {
    buf = &cg->type_defs;
    len = &cg->type_defs_len;
    cap = &cg->type_defs_cap;
  } else if (strcmp(cg->current_section, "forward_vars_funcs") == 0) {
    buf = &cg->forward_vars_funcs;
    len = &cg->forward_vars_funcs_len;
    cap = &cg->forward_vars_funcs_cap;
  } else if (strcmp(cg->current_section, "defs") == 0) {
    buf = &cg->defs;
    len = &cg->defs_len;
    cap = &cg->defs_cap;
  } else
    return; // Invalid section

  if (*len + str_len >= *cap) {
    int old_cap = *cap;
    *cap = (*cap == 0) ? 1024 : *cap * 2; // Start at 1K, double as needed
    *buf = realloc(*buf, *cap);
    if (!*buf) { /* error */
    }

    // NOTE: zero out buffer, this fixes strange bytes appearing in source
    memset(*buf + old_cap, 0, *cap - old_cap);
  }
  memcpy(*buf + *len, str, str_len);
  *len += str_len;
}

static DeferStack *defer_stack_create(DeferStack *parent,
                                      DeferScopeType scope_type) {
  DeferStack *stack = arena_alloc(&long_lived, sizeof(DeferStack));
  stack->defers = NULL;
  stack->count = 0;
  stack->capacity = 0;
  stack->scope_type = scope_type;
  stack->parent = parent;

  return stack;
}

static void defer_stack_push(Codegen *cg, AstNode *defer_stmt) {
  DeferStack *stack = cg->defer_stack;
  assert(stack);

  if (stack->count >= stack->capacity) {
    bool is_empty = stack->capacity == 0;
    AstNode **old_buffer = stack->defers;

    stack->capacity = is_empty ? 2 : stack->capacity * 2;
    stack->defers =
        arena_alloc(&long_lived, stack->capacity * sizeof(AstNode *));
    if (!is_empty) {
      memcpy(stack->defers, old_buffer, stack->count * sizeof(AstNode *));
    }
  }

  stack->defers[stack->count++] = defer_stmt;
}

// Emit all defers in the current scope only (LIFO)
static void defer_stack_emit_current_scope(Codegen *cg, bool lock) {
  DeferStack *stack = cg->defer_stack;
  if (!stack || stack->locked)
    return;

  stack->locked = lock;

  cg->in_defer = true;

  // Emit in reverse order (most recent first)
  for (int i = stack->count - 1; i >= 0; i--) {
    emit_string(cg, "{ /* defer */\n");
    emit_stmt(cg, stack->defers[i]);
    emit_string(cg, "}\n");
  }

  cg->in_defer = false;
}

// Emit defers from current scope up to (but not including) a target scope type
// This is used for break/continue to emit defers up to the loop boundary
static void defer_stack_emit_until(Codegen *cg, DeferScopeType target_scope,
                                   bool lock) {
  DeferStack *stack = cg->defer_stack;
  if (!stack || stack->locked)
    return;

  stack->locked = lock;

  cg->in_defer = true;

  while (stack && stack->scope_type != target_scope) {
    // Emit all defers in this scope (reverse order)
    for (int i = stack->count - 1; i >= 0; i--) {
      emit_string(cg, "{ /* defer */\n");
      emit_stmt(cg, stack->defers[i]);
      emit_string(cg, "}\n");
    }
    stack = stack->parent;
  }

  cg->in_defer = false;
}

// Emit all defers from current scope up through and including all parent scopes
// This is used for return statements
static void defer_stack_emit_all(Codegen *cg, bool lock) {
  DeferStack *stack = cg->defer_stack;
  if (!stack || stack->locked)
    return;

  stack->locked = lock;

  cg->in_defer = true;

  while (stack) {
    // Emit all defers in this scope (reverse order)
    for (int i = stack->count - 1; i >= 0; i--) {
      emit_string(cg, "{ /* defer */\n");
      emit_stmt(cg, stack->defers[i]);
      emit_string(cg, "}\n");
    }
    stack = stack->parent;
  }

  cg->in_defer = false;
}

// Enter a new defer scope
static void defer_scope_enter(Codegen *cg, DeferScopeType scope_type) {
  cg->defer_stack = defer_stack_create(cg->defer_stack, scope_type);
}

static void defer_scope_exit(Codegen *cg) {
  defer_stack_emit_current_scope(cg, false);
  DeferStack *old = cg->defer_stack;
  cg->defer_stack = old ? old->parent : NULL;
}

void codegen_init(Codegen *cg, FILE *output) {
  cg->output = output;
  cg->indent_level = 0;
  cg->current_section = NULL;

  // Init section buffers (start empty)
  cg->forward_types = NULL;
  cg->forward_types_len = 0;
  cg->forward_types_cap = 0;
  cg->type_defs = NULL;
  cg->type_defs_len = 0;
  cg->type_defs_cap = 0;
  cg->forward_vars_funcs = NULL;
  cg->forward_vars_funcs_len = 0;
  cg->forward_vars_funcs_cap = 0;
  cg->defs = NULL;
  cg->defs_len = 0;
  cg->defs_cap = 0;

  if (!compiler_opts.freestanding) {
    // Set preamble (use alloc.c's str_dup for long-lived strings if needed)
    cg->preamble = "#include <stdlib.h>\n#include <stdbool.h>\n#include "
                   "<stdio.h>\n#include <string.h>\n#include <stddef.h>\n#include <assert.h>\n\n";
  } else {
    // Freestanding has basic default includes
    cg->preamble = "#include <stddef.h>\n#include <stdbool.h>\n\n";
  }

  // Init uthash sets to empty
  cg->declared_types = NULL;
  cg->defined_types = NULL;
  cg->declared_vars = NULL;

  cg->defer_stack = false;
  cg->defer_stack = NULL;

  // Init temporary
  cg->temporary_count = 0;
}

char *get_temporary_name(Codegen *cg, char *buffer, size_t size) {
  assert(size >= 32);
  memset(buffer, 0, size);
  sprintf(buffer, "__temporary_var_%ld", cg->temporary_count);

  cg->temporary_count++;
  return buffer;
}

void emit_string(Codegen *cg, const char *str) {
  if (!cg->current_section)
    return; // Not collecting into sections yet
  size_t len = strlen(str);
  append_to_section(cg, str, len);
}

static void emit_indent_spaces(Codegen *cg) {
  for (int i = 0; i < cg->indent_level; i++) {
    emit_string(cg, "  ");
  }
}

// Check if an expression is a zero/false literal (for optimization)
static bool is_zero_value(AstNode *expr) {
  if (!expr)
    return false;

  switch (expr->kind) {
  case AST_EXPR_LITERAL_INT:
    return expr->data.int_lit.value == 0;

  case AST_EXPR_LITERAL_FLOAT:
    return expr->data.float_lit.value == 0.0;

  case AST_EXPR_LITERAL_BOOL:
    return expr->data.bool_lit.value == false;

  case AST_EXPR_LITERAL_CHAR:
    return expr->data.char_lit.value == 0;

  case AST_EXPR_TUPLE: {
    // All elements must be zero
    for (size_t i = 0; i < expr->data.tuple_expr.element_count; i++) {
      if (!is_zero_value(expr->data.tuple_expr.elements[i])) {
        return false;
      }
    }
    return true;
  }

  case AST_EXPR_STRUCT_LITERAL: {
    // All field values must be zero
    for (size_t i = 0; i < expr->data.struct_literal.field_count; i++) {
      if (!is_zero_value(expr->data.struct_literal.field_values[i])) {
        return false;
      }
    }
    return true;
  }

  default:
    return false;
  }
}

void emit_indent(Codegen *cg) { cg->indent_level++; }

void emit_dedent(Codegen *cg) { cg->indent_level--; }

// Dependency graph node
typedef struct TypeDepNode {
  char *name;
  char **depends_on; // Array of type names this depends on
  size_t dep_count;
  bool visited;
  bool in_stack;
  UT_hash_handle hh;
} TypeDepNode;

// Check if a canonical name matches any declared type
bool is_declared_type(const char *canonical_name, TypeDepNode *all_types) {
  TypeDepNode *node = NULL;
  HASH_FIND_STR(all_types, canonical_name, node);
  return node != NULL;
}

// Collect dependencies by checking canonical names against declared types
void collect_dependencies(Type *t, const char *self_name,
                          TypeDepNode *all_types, char ***deps,
                          size_t *dep_count, size_t *dep_capacity) {
  if (!t)
    return;

  if (t->canonical_name && strcmp(t->canonical_name, self_name) == 0)
    return;

  // Check if this type's canonical name matches a declared type
  if (t->canonical_name && strcmp(t->canonical_name, self_name) != 0) {
    if (is_declared_type(t->canonical_name, all_types)) {
      // Check if we already added this dependency
      for (size_t i = 0; i < *dep_count; i++) {
        if (strcmp((*deps)[i], t->canonical_name) == 0) {
          return; // Already have it
        }
      }
      // Add dependency
      if (*dep_count >= *dep_capacity) {
        *dep_capacity = (*dep_capacity == 0) ? 4 : *dep_capacity * 2;
        *deps = realloc(*deps, *dep_capacity * sizeof(char *));
      }
      (*deps)[(*dep_count)++] = strdup(t->canonical_name);
    }
  }

  // Recurse into compound types
  switch (t->kind) {
  case TYPE_STRUCT:
    for (size_t i = 0; i < t->data.struct_data.field_count; i++) {
      collect_dependencies(t->data.struct_data.field_types[i], self_name,
                           all_types, deps, dep_count, dep_capacity);
    }
    break;
  case TYPE_TUPLE:
    for (size_t i = 0; i < t->data.tuple.element_count; i++) {
      collect_dependencies(t->data.tuple.element_types[i], self_name, all_types,
                           deps, dep_count, dep_capacity);
    }
    break;
  case TYPE_POINTER:
    collect_dependencies(t->data.ptr.base, self_name, all_types, deps,
                         dep_count, dep_capacity);
    break;
  case TYPE_ARRAY:
    collect_dependencies(t->data.array.element, self_name, all_types, deps,
                         dep_count, dep_capacity);
    break;
  case TYPE_SLICE:
    collect_dependencies(t->data.slice.element, self_name, all_types, deps,
                         dep_count, dep_capacity);
    break;
  default:
    break;
  }
}

// Topological sort DFS visit
bool topo_visit(TypeDepNode *all_types, TypeDepNode *node, char ***result,
                size_t *result_count) {
  if (node->in_stack) {
    fprintf(stderr, "Error: Circular type dependency involving '%s'\n",
            node->name);
    return false;
  }

  if (node->visited) {
    return true;
  }

  node->visited = true;
  node->in_stack = true;

  // Visit dependencies first
  for (size_t i = 0; i < node->dep_count; i++) {
    TypeDepNode *dep = NULL;
    HASH_FIND_STR(all_types, node->depends_on[i], dep);
    if (dep) {
      if (!topo_visit(all_types, dep, result, result_count)) {
        return false;
      }
    }
  }

  node->in_stack = false;

  // Add to result
  (*result)[(*result_count)++] = node->name;
  return true;
}

void emit_program(Codegen *cg) {
  TypeDepNode *dep_graph = NULL;
  Symbol *sym, *tmp;

  TypeEntry *entry, *type_tmp;

  // Pass 1: Create nodes for all declared types
  HASH_ITER(hh, canonical_type_table, entry, type_tmp) {
    Type *type = entry->type;
    if (type->kind == TYPE_ARRAY || type->kind == TYPE_TUPLE ||
        type->kind == TYPE_SLICE || type->kind == TYPE_STRUCT ||
        type->kind == TYPE_ENUM || type->kind == TYPE_FUNCTION ||
        type->kind == TYPE_OPTIONAL) {
      TypeDepNode *node = calloc(1, sizeof(TypeDepNode));
      node->name = type->canonical_name;
      node->depends_on = NULL;
      node->dep_count = 0;

      HASH_ADD_KEYPTR(hh, dep_graph, node->name, strlen(node->name), node);
    }
  }

  // Pass 2: Build dependency lists
  HASH_ITER(hh, canonical_type_table, entry, type_tmp) {
    Type *type = entry->type;
    TypeDepNode *node = NULL;
    HASH_FIND_STR(dep_graph, type->canonical_name, node);
    size_t dep_capacity = 0;

    switch (type->kind) {
    case TYPE_ARRAY: {
      collect_dependencies(type->data.array.element, type->canonical_name,
                           dep_graph, &node->depends_on, &node->dep_count,
                           &dep_capacity);
      break;
    }

    case TYPE_TUPLE: {
      for (size_t i = 0; i < type->data.tuple.element_count; i++) {
        collect_dependencies(type->data.tuple.element_types[i],
                             type->canonical_name, dep_graph, &node->depends_on,
                             &node->dep_count, &dep_capacity);
      }
      break;
    }

    case TYPE_OPTIONAL: {
      collect_dependencies(type->data.optional.base, type->canonical_name,
                           dep_graph, &node->depends_on, &node->dep_count,
                           &dep_capacity);
      break;
    }

    case TYPE_SLICE: {
      collect_dependencies(type->data.slice.element, type->canonical_name,
                           dep_graph, &node->depends_on, &node->dep_count,
                           &dep_capacity);
      break;
    }

    case TYPE_STRUCT: {
      for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
        collect_dependencies(type->data.struct_data.field_types[i],
                             type->canonical_name, dep_graph, &node->depends_on,
                             &node->dep_count, &dep_capacity);
      }
      break;
    }
    case TYPE_FUNCTION: {
      for (size_t i = 0; i < type->data.func.param_count; i++) {
        collect_dependencies(type->data.func.param_types[i],
                             type->canonical_name, dep_graph, &node->depends_on,
                             &node->dep_count, &dep_capacity);
      }
      collect_dependencies(type->data.func.return_type, type->canonical_name,
                           dep_graph, &node->depends_on, &node->dep_count,
                           &dep_capacity);
      break;
    }
    default:
      break;
    }
  }

  // Pass 3: Topological sort
  size_t type_count = HASH_COUNT(dep_graph);
  char **sorted_types = malloc(type_count * sizeof(char *));
  size_t sorted_count = 0;

  TypeDepNode *node, *node_tmp;
  HASH_ITER(hh, dep_graph, node, node_tmp) {
    if (!node->visited) {
      if (!topo_visit(dep_graph, node, &sorted_types, &sorted_count)) {
        fprintf(stderr, "Failed to sort global types\n");
        exit(1);
      }
    }
  }

  // Emit types in sorted order
  cg->current_section = "forward_types";
  for (size_t i = 0; i < sorted_count; i++) {
    // Check for array/tuple type is in canonical type table
    TypeEntry *entry = NULL;
    HASH_FIND_STR(canonical_type_table, sorted_types[i], entry);
    if (entry &&
        (entry->type->kind == TYPE_ARRAY || entry->type->kind == TYPE_TUPLE ||
         entry->type->kind == TYPE_SLICE || entry->type->kind == TYPE_STRUCT ||
         entry->type->kind == TYPE_ENUM || entry->type->kind == TYPE_FUNCTION ||
         entry->type->kind == TYPE_OPTIONAL)) {
      emit_type_if_needed(cg, entry->type);
    }
  }

  // Cleanup dependency graph
  HASH_ITER(hh, dep_graph, node, node_tmp) {
    HASH_DEL(dep_graph, node);
    for (size_t i = 0; i < node->dep_count; i++) {
      free(node->depends_on[i]);
    }
    free(node->depends_on);
    free(node);
  }
  free(sorted_types);

  // Emit forwards/defs for vars/constants
  cg->current_section = "forward_vars_funcs";
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    if (sym->kind == SYMBOL_VARIABLE || sym->kind == SYMBOL_CONSTANT) {
      // Emit extern decl
      emit_string(cg, "extern ");
      emit_type_name(cg, sym->type);
      emit_string(cg, " ");
      emit_string(cg, sym->name);

      emit_string(cg, ";\n");
    } else if (sym->kind == SYMBOL_EXTERN_FUNCTION) {
      Type *func_type = sym->type;
      if (!sym->data.external.lib_name) {
        continue;
      }
      
      emit_string(cg, "/* ");
      emit_string(cg, sym->data.external.lib_name);
      emit_string(cg, " */\n");

      emit_string(cg, "extern ");
      emit_type_name(cg, func_type->data.func.return_type);
      emit_string(cg, " ");
      emit_string(cg, sym->name);

      emit_string(cg, "(");

      for (size_t i = 0; i < func_type->data.func.param_count; i++) {
        if (i > 0) {
          emit_string(cg, ", ");
        }
        emit_type_name(cg, func_type->data.func.param_types[i]);
      }

      emit_string(cg, ");\n");
    }
  }

  cg->current_section = "defs";
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    if (sym->kind == SYMBOL_VARIABLE || sym->kind == SYMBOL_CONSTANT) {
      emit_type_name(cg, sym->type);
      emit_string(cg, " ");
      emit_string(cg, sym->name);
      if (sym->kind == SYMBOL_VARIABLE && sym->decl) {
        emit_string(cg, " = ");

        if (sym->decl->data.var_decl.init) {
          emit_expr(cg, sym->decl->data.var_decl.init);
        } else {
          Type *init_t = sym->decl->resolved_type;

          if (init_t->kind == TYPE_ARRAY) {
            emit_string(cg, " {{0}, ");
            char len_buffer[32] = {0};
            sprintf(len_buffer, "%zu", init_t->data.array.size);
            emit_string(cg, len_buffer);
            emit_string(cg, "}");
          } else {
            emit_string(cg, "{0}");
          }
        }
      } else if (sym->kind == SYMBOL_CONSTANT && sym->decl &&
                 sym->decl->data.const_decl.value) {
        emit_string(cg, " = ");
        emit_expr(cg, sym->decl->data.const_decl.value);
      }
      emit_string(cg, ";\n");
    }
  }

  // Emit func prototypes
  cg->current_section = "forward_vars_funcs";
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    if (sym->kind == SYMBOL_FUNCTION) {
      // if (sym->kind == SYMBOL_FUNCTION || sym->kind ==
      // SYMBOL_EXTERN_FUNCTION) {
      Type *func = sym->type; // Assume sym->ast points to func node
      // Emit prototype
      emit_type_name(cg, func->data.func.return_type);
      emit_string(cg, " ");
      if (strcmp("main", sym->name) == 0) {
        emit_string(cg, "__user_main");
      } else {
        emit_string(cg, sym->name);
      }
      emit_string(cg, "(");

      size_t param_count = func->data.func.param_count;

      CallingConvention conv = func->data.func.convention;
      if (conv == CALL_CONV_PEBBLE) {
        emit_string(cg, "__pebble_context context");
      }

      for (size_t i = 0; i < param_count; i++) {
        if (conv == CALL_CONV_PEBBLE || i > 0)
          emit_string(cg, ", ");
        emit_type_name(cg, func->data.func.param_types[i]);
        emit_string(cg, " ");
        emit_string(cg, sym->decl->data.func_decl.params[i].name);
      }

      emit_string(cg, ")");
      emit_string(cg, ";\n");
    }
  }

  // Emit anonymous functions declarations
  HASH_ITER(hh, anonymous_funcs->symbols, sym, tmp) {
    Type *func = sym->type;
    // Emit prototype
    emit_type_name(cg, func->data.func.return_type);
    emit_string(cg, " ");
    emit_string(cg, sym->name);
    emit_string(cg, "(");

    CallingConvention conv = func->data.func.convention;
    if (conv == CALL_CONV_PEBBLE) {
      emit_string(cg, "__pebble_context");
    } else if (func->data.func.param_count == 0) {
      emit_string(cg, "void");
    }

    for (size_t i = 0; i < func->data.func.param_count; i++) {
      if (conv == CALL_CONV_PEBBLE || i > 0)
        emit_string(cg, ", ");
      emit_type_name(cg, func->data.func.param_types[i]);
      emit_string(cg, " ");
      emit_string(cg, sym->decl->data.func_expr.params[i].name);
    }

    emit_string(cg, ")");
    emit_string(cg, ";\n");
  }

  // Emit anonymous functions
  cg->current_section = "defs";
  HASH_ITER(hh, anonymous_funcs->symbols, sym, tmp) {
    AstNode *func = sym->decl; // Func decl AST node
    emit_type_name(cg, sym->type->data.func.return_type);
    emit_string(cg, " ");
    emit_string(cg, func->data.func_expr.symbol);
    emit_string(cg, "(");

    size_t param_count = sym->type->data.func.param_count;

    CallingConvention conv = sym->type->data.func.convention;
    if (conv == CALL_CONV_PEBBLE) {
      emit_string(cg, "__pebble_context context");
    } else if (param_count == 0) {
      emit_string(cg, "void");
    }

    for (size_t i = 0; i < param_count; i++) {
      if (conv == CALL_CONV_PEBBLE || i > 0)
        emit_string(cg, ", ");
      emit_type_name(cg, sym->type->data.func.param_types[i]);
      emit_string(cg, " ");
      emit_string(cg, func->data.func_expr.params[i].name);
    }

    emit_string(cg, ") ");
    emit_string(cg, "{\n");
    emit_indent(cg);

    // Enter function scope
    defer_scope_enter(cg, DEFER_SCOPE_FUNCTION);

    // Emit body (minimal traversal)
    emit_stmt(cg, func->data.func_expr.body);

    // Exit function scope (though returns should have handled this already)
    defer_scope_exit(cg);

    emit_dedent(cg);
    emit_string(cg, "}\n");
  }

  // Emit func definitions
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    if (sym->kind == SYMBOL_FUNCTION) {
      AstNode *func = sym->decl; // Func decl AST node
      emit_type_name(cg, sym->type->data.func.return_type);
      emit_string(cg, " ");
      if (strcmp("main", sym->name) == 0) {
        emit_string(cg, "__user_main");
      } else {
        emit_string(cg, func->data.func_decl.name);
      }
      emit_string(cg, "(");

      size_t param_count = sym->type->data.func.param_count;

      CallingConvention conv = sym->type->data.func.convention;
      if (conv == CALL_CONV_PEBBLE) {
        emit_string(cg, "__pebble_context context");
      }

      for (size_t i = 0; i < param_count; i++) {
        if (conv == CALL_CONV_PEBBLE || i > 0)
          emit_string(cg, ", ");
        emit_type_name(cg, sym->type->data.func.param_types[i]);
        emit_string(cg, " ");
        emit_string(cg, func->data.func_decl.params[i].name);
      }

      emit_string(cg, ") ");
      emit_string(cg, "{\n");
      emit_indent(cg);

      // Enter function scope
      defer_scope_enter(cg, DEFER_SCOPE_FUNCTION);

      // Emit body (minimal traversal)
      emit_stmt(cg, func->data.func_decl.body);

      // Exit function scope (though returns should have handled this already)
      defer_scope_exit(cg);

      emit_dedent(cg);
      emit_string(cg, "}\n");
    }
  }

  // Emit sections
  emit_sections(cg);

  // Free global symbol and type tables
  HASH_CLEAR(hh, global_scope->symbols);
  HASH_CLEAR(hh, type_table);
  HASH_CLEAR(hh, canonical_type_table);
  HASH_CLEAR(hh, anonymous_funcs->symbols);
}

void emit_type_name(Codegen *cg, Type *type) {
  assert(type);

  switch (type->kind) {
  case TYPE_INT:
    emit_string(cg, "int");
    break;
  case TYPE_BOOL:
    emit_string(cg, "bool");
    break;
  case TYPE_STRING:
    emit_string(cg, "const char*");
    break;
  case TYPE_VOID:
    emit_string(cg, "void");
    break;
  case TYPE_F32:
    emit_string(cg, "float");
    break;
  case TYPE_F64:
    emit_string(cg, "double");
    break;
  case TYPE_U8:
    emit_string(cg, "unsigned char");
    break;
  case TYPE_U16:
    emit_string(cg, "unsigned short");
    break;
  case TYPE_U32:
    emit_string(cg, "unsigned int");
    break;
  case TYPE_U64:
    emit_string(cg, "unsigned long long");
    break;
  case TYPE_USIZE:
    emit_string(cg, "size_t");
    break;
  case TYPE_I8:
    emit_string(cg, "signed char");
    break;
  case TYPE_I16:
    emit_string(cg, "short");
    break;
  case TYPE_I32:
    emit_string(cg, "int");
    break;
  case TYPE_I64:
    emit_string(cg, "long long");
    break;
  case TYPE_ISIZE:
    emit_string(cg, "ptrdiff_t");
    break;
  case TYPE_CHAR:
    emit_string(cg, "char");
    break;
  case TYPE_POINTER:
    emit_type_name(cg, type->data.ptr.base);
    emit_string(cg, "*");
    break;
  case TYPE_OPAQUE:
    emit_string(cg, type->canonical_name);
    break;
  case TYPE_STRUCT:
  case TYPE_TUPLE:
  case TYPE_OPTIONAL:
  case TYPE_ENUM:
    emit_string(cg, type->canonical_name); // Just "Node" or "tuple_int_int"
    break;
  case TYPE_ARRAY: {
    int old_indent = cg->indent_level;
    cg->indent_level = 0;
    emit_type_if_needed(cg, type);
    emit_string(cg, type->canonical_name);
    cg->indent_level = old_indent;
    break;
  }
  case TYPE_SLICE: {
    int old_indent = cg->indent_level;
    cg->indent_level = 0;
    emit_type_if_needed(cg, type);
    emit_string(cg, type->canonical_name);
    cg->indent_level = old_indent;
    break;
  }

  default:
    emit_string(cg, type->canonical_name);
    break;
  }
}

void emit_sections(Codegen *cg) {
  // Emit preamble directly
  if (cg->preamble) {
    fputs(cg->preamble, cg->output);
  }

  if (!compiler_opts.freestanding) {
    fputs(
      "typedef struct __pebble_context __pebble_context;\n\n"
      "typedef struct Allocator {\n"
      "  void *ptr;\n"
      "  void *(*alloc)(__pebble_context, size_t);\n"
      "  void (*free)(__pebble_context, void *);\n"
      "} Allocator;\n\n"
      "struct __pebble_context {\n"
      "  Allocator default_allocator;\n"
      "};\n\n"
      "void *__pebble_c_alloc(__pebble_context, size_t size) {\n"
      "  return malloc(size);\n"
      "}\n\n"
      "void __pebble_c_free(__pebble_context, void *ptr) {\n"
      "  return free(ptr);\n"
      "}\n\n"
      "\n"
      ,
      cg->output
    );
  }

  // Emit sections (if they have content), then free buffers
  if (cg->forward_types) {
    fputs(cg->forward_types, cg->output);
    fputc('\n', cg->output);
    free(cg->forward_types);
    cg->forward_types = NULL;
  }
  if (cg->type_defs) {
    fputs(cg->type_defs, cg->output);
    fputc('\n', cg->output);
    free(cg->type_defs);
    cg->type_defs = NULL;
  }
  if (cg->forward_vars_funcs) {
    fputs(cg->forward_vars_funcs, cg->output);
    fputc('\n', cg->output);
    free(cg->forward_vars_funcs);
    cg->forward_vars_funcs = NULL;
  }
  if (cg->defs) {
    fputs(cg->defs, cg->output);
    fputc('\n', cg->output);
    free(cg->defs);
    cg->defs = NULL;
  }

  // Emit main function
  const char *entry_name = compiler_opts.entry_point;
  bool is_main = strcmp(entry_name, "main") == 0;

  if (is_main) {
    // Look up the entry point function in global scope
    Symbol *entry_sym = scope_lookup(global_scope, entry_name);
    
    fputs(
      "int main(int argc, const char **argv) {\n"
      "  __pebble_context context = {\n"
      "    .default_allocator = (Allocator){\n"
      "      .ptr = NULL,\n"
      "      .alloc = __pebble_c_alloc,\n"
      "      .free = __pebble_c_free,\n"
      "    },\n"
      "  };\n"
      "  return __user_main(context"
      ,
      cg->output
    );

    if (entry_sym->type->data.func.param_count == 2) {
      fputs(", argc, argv", cg->output);
    }

    fputs(");\n}\n", cg->output);
  }

  // Clear uthash sets
  HASH_CLEAR(hh, cg->declared_types);
  HASH_CLEAR(hh, cg->defined_types);
  HASH_CLEAR(hh, cg->declared_vars);
  // Reset lengths/caps to 0 (optional, since freed)
}

void emit_type_if_needed(Codegen *cg, Type *type) {
  if (!type)
    return;

  // Get canonical name (use if available, else compute)
  const char *canonical = type->canonical_name;

  // Check if already declared (forward)
  CodegenTypeEntry *decl_entry;
  HASH_FIND_STR(cg->declared_types, canonical, decl_entry);
  if (!decl_entry) {
    // Emit forward decl (to forward_types, preserving section)
    char *old_section = cg->current_section;
    cg->current_section = "forward_types";

    if (type->kind == TYPE_FUNCTION) {
      emit_string(cg, "typedef ");

      emit_type_name(cg, type->data.func.return_type);

      emit_string(cg, " (*");
      emit_string(cg, canonical);
      emit_string(cg, ")(");

      CallingConvention conv = type->data.func.convention;
      if (conv == CALL_CONV_PEBBLE) {
        emit_string(cg, "__pebble_context");
      }

      for (size_t i = 0; i < type->data.func.param_count; i++) {
        if (conv == CALL_CONV_PEBBLE || i > 0) {
          emit_string(cg, ", ");
        }
        emit_type_name(cg, type->data.func.param_types[i]);
      }

      emit_string(cg, ")");

      emit_string(cg, ";\n");
    } else {
      if (type->kind == TYPE_ENUM) {
        emit_string(cg, "typedef enum ");
      } else {
        emit_string(cg, "typedef struct ");
      }

      emit_string(cg, canonical);
      emit_string(cg, " ");
      emit_string(cg, canonical);
      emit_string(cg, ";\n");
    }

    cg->current_section = old_section;

    // Insert into declared_types hash
    decl_entry = arena_alloc(&long_lived, sizeof(CodegenTypeEntry));
    decl_entry->key = str_dup(canonical); // Persist key
    HASH_ADD_STR(cg->declared_types, key, decl_entry);
  }

  if (type->kind == TYPE_ENUM) {
    CodegenTypeEntry *def_entry;
    HASH_FIND_STR(cg->defined_types, canonical, def_entry);

    if (!def_entry) {
      // Emit full def
      char *old_section = cg->current_section;
      cg->current_section = "type_defs";

      emit_string(cg, "enum ");
      emit_string(cg, canonical);
      emit_string(cg, " {");
      emit_string(cg, "\n");
      emit_indent(cg);

      for (size_t i = 0; i < type->data.enum_data.variant_count; i++) {
        emit_indent_spaces(cg);
        emit_string(cg, canonical);
        emit_string(cg, "_");
        emit_string(cg, type->data.enum_data.variant_names[i]);
        emit_string(cg, ",\n");
      }

      emit_dedent(cg);
      emit_indent_spaces(cg);
      emit_string(cg, "};\n");
      cg->current_section = old_section;

      // Insert into defined_types hash to prevent duplicates
      def_entry = arena_alloc(&long_lived, sizeof(CodegenTypeEntry));
      def_entry->key = str_dup(canonical); // Persist key
      HASH_ADD_STR(cg->defined_types, key, def_entry);
    }
  }

  // Check if already defined (full struct/tuple)
  if (type->kind == TYPE_STRUCT || type->kind == TYPE_TUPLE ||
      type->kind == TYPE_ARRAY || type->kind == TYPE_SLICE ||
      type->kind == TYPE_OPTIONAL) {
    CodegenTypeEntry *def_entry;
    HASH_FIND_STR(cg->defined_types, canonical, def_entry);
    if (!def_entry) {
      // Emit full def
      char *old_section = cg->current_section;
      cg->current_section = "type_defs";

      emit_string(cg, "struct ");
      emit_string(cg, canonical);
      emit_string(cg, " {");
      emit_string(cg, "\n");
      emit_indent(cg);
      if (type->kind == TYPE_STRUCT) {
        for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
          emit_indent_spaces(cg);
          emit_type_name(cg, type->data.struct_data.field_types[i]);
          emit_string(cg, " ");
          emit_string(cg, type->data.struct_data.field_names[i]);
          emit_string(cg, ";");
          emit_string(cg, "\n");
        }
      } else if (type->kind == TYPE_TUPLE) {
        for (size_t i = 0; i < type->data.tuple.element_count; i++) {
          emit_indent_spaces(cg);
          emit_type_name(cg, type->data.tuple.element_types[i]);
          emit_string(cg, " _");
          // NOTE: This will break past 10 elements 0-9
          char idx_str[2] = {'0' + (char)i, '\0'};
          emit_string(cg, idx_str);
          emit_string(cg, ";");
          emit_string(cg, "\n");
        }
      } else if (type->kind == TYPE_ARRAY) {
        emit_indent_spaces(cg);
        emit_type_name(cg, type->data.array.element);
        char buf[32];
        sprintf(buf, " data[%zu];", type->data.array.size);
        emit_string(cg, buf);
        emit_string(cg, "\n");
        emit_indent_spaces(cg);
        emit_string(cg, "size_t len;\n");
      } else if (type->kind == TYPE_SLICE) {
        emit_indent_spaces(cg);
        emit_type_name(cg, type->data.slice.element);
        emit_string(cg, "* data;");
        emit_string(cg, "\n");
        emit_indent_spaces(cg);
        emit_string(cg, "size_t len;");
        emit_string(cg, "\n");
      } else if (type->kind == TYPE_OPTIONAL) {
        emit_indent_spaces(cg);
        emit_type_name(cg, type->data.optional.base);
        emit_string(cg, " value;");
        emit_string(cg, "\n");
        emit_indent_spaces(cg);
        emit_string(cg, "bool has_value;");
        emit_string(cg, "\n");
      }
      emit_dedent(cg);
      emit_indent_spaces(cg);
      emit_string(cg, "};");
      emit_string(cg, "\n");
      cg->current_section = old_section;

      // Insert into defined_types hash to prevent duplicates
      def_entry = arena_alloc(&long_lived, sizeof(CodegenTypeEntry));
      def_entry->key = str_dup(canonical); // Persist key
      HASH_ADD_STR(cg->defined_types, key, def_entry);
    }
  }
}

// Emit statement (minimal)
void emit_stmt(Codegen *cg, AstNode *stmt) {
  if (!stmt)
    return;
  switch (stmt->kind) {
  case AST_STMT_BREAK:
    // Emit all defers up to (but not including) the loop scope
    defer_stack_emit_until(cg, DEFER_SCOPE_LOOP, true);

    emit_indent_spaces(cg);
    emit_string(cg, "break;\n");
    break;
  case AST_STMT_CONTINUE:
    // Emit all defers up to (but not including) the loop scope
    defer_stack_emit_until(cg, DEFER_SCOPE_LOOP, true);

    emit_indent_spaces(cg);
    emit_string(cg, "continue;\n");
    break;
  case AST_STMT_PRINT: {
    emit_indent_spaces(cg);
    emit_string(cg, "printf(\"");

    // Emit type qualifiers
    for (size_t i = 0; i < stmt->data.print_stmt.expr_count; i++) {
      Type *type = stmt->data.print_stmt.exprs[i]->resolved_type;

      if (type->kind == TYPE_INT || type->kind == TYPE_I32 ||
          type->kind == TYPE_ENUM) {
        emit_string(cg, "%d");
      } else if (type->kind == TYPE_I8) {
        emit_string(cg, "%hhd");
      } else if (type->kind == TYPE_I16) {
        emit_string(cg, "%hd");
      } else if (type->kind == TYPE_I64) {
        emit_string(cg, "%lld");
      } else if (type->kind == TYPE_ISIZE) {
        emit_string(cg, "%td");
      } else if (type->kind == TYPE_U8) {
        emit_string(cg, "%hhu");
      } else if (type->kind == TYPE_U16) {
        emit_string(cg, "%hu");
      } else if (type->kind == TYPE_U32) {
        emit_string(cg, "%lu");
      } else if (type->kind == TYPE_U64) {
        emit_string(cg, "%llu");
      } else if (type->kind == TYPE_USIZE) {
        emit_string(cg, "%zu");
      } else if (type->kind == TYPE_CHAR) {
        emit_string(cg, "%c");
      } else if (type->kind == TYPE_F32 || type->kind == TYPE_F64) {
        emit_string(cg, "%f");
      } else if (type->kind == TYPE_STRING) {
        emit_string(cg, "%s");
      } else {
        emit_string(cg, "%s");
      }
    }

    emit_string(cg, "\\n\", ");

    // Emit expressions
    for (size_t i = 0; i < stmt->data.print_stmt.expr_count; i++) {
      Type *type = stmt->data.print_stmt.exprs[i]->resolved_type;

      if (i > 0) {
        emit_string(cg, ", ");
      }

      if (type->kind == TYPE_INT || type->kind == TYPE_I32 ||
          type->kind == TYPE_ENUM || type->kind == TYPE_I8 ||
          type->kind == TYPE_I16 || type->kind == TYPE_I64 ||
          type->kind == TYPE_ISIZE || type->kind == TYPE_U8 ||
          type->kind == TYPE_U16 || type->kind == TYPE_U32 ||
          type->kind == TYPE_U64 || type->kind == TYPE_USIZE ||
          type->kind == TYPE_CHAR || type->kind == TYPE_F32 ||
          type->kind == TYPE_F64) {
        emit_expr(cg, stmt->data.print_stmt.exprs[i]);
      } else if (type->kind == TYPE_BOOL) {
        emit_expr(cg, stmt->data.print_stmt.exprs[i]);
        emit_string(cg, " ? \"true\" : \"false\"");
      } else if (type->kind == TYPE_STRING) {
        AstNode *expr = stmt->data.print_stmt.exprs[i];
        if (stmt->data.print_stmt.exprs[i]->kind != AST_EXPR_LITERAL_STRING) {
          emit_string(cg, "({ ");

          char buffer[64];
          char *temporary_name = get_temporary_name(cg, buffer, 64);

          emit_type_name(cg, expr->resolved_type);
          emit_string(cg, " ");
          emit_string(cg, temporary_name);

          emit_string(cg, " = ");
          emit_expr(cg, expr);
          emit_string(cg, ";\n");

          emit_string(cg, "(");
          emit_string(cg, temporary_name);
          emit_string(cg, ") ? ");
          emit_string(cg, temporary_name);
          emit_string(cg, " : \"\";\n");

          emit_string(cg, " })");
        } else {
          emit_expr(cg, expr);
        }
      } else {
        emit_string(cg, "\"");
        emit_string(cg,
                    type_name(stmt->data.print_stmt.exprs[i]->resolved_type));
        emit_string(cg, "\"");
      }
    }

    emit_string(cg, ");\n");
    break;
  }
  case AST_STMT_RETURN:
    // Emit all current defers
    defer_stack_emit_all(cg, true);

    emit_indent_spaces(cg);
    emit_string(cg, "return");
    if (stmt->data.return_stmt.expr) {
      emit_string(cg, " ");
      emit_expr(cg, stmt->data.return_stmt.expr);
    }
    emit_string(cg, ";\n");
    break;
  case AST_STMT_DEFER: {
    // Emit held stmt to defer stack
    defer_stack_push(cg, stmt->data.defer_stmt.stmt);
    break;
  }
  case AST_STMT_BLOCK:
    if (!cg->in_defer) {
      // Enter new regular block scope
      defer_scope_enter(cg, DEFER_SCOPE_BLOCK);
    }

    for (size_t i = 0; i < stmt->data.block_stmt.stmt_count; i++) {
      emit_stmt(cg, stmt->data.block_stmt.stmts[i]);
    }

    if (!cg->in_defer) {
      defer_scope_exit(cg);
    }
    break;
  case AST_STMT_IF: {
    emit_indent_spaces(cg);
    emit_string(cg, "if (");
    emit_expr(cg, stmt->data.if_stmt.cond);
    emit_string(cg, ") ");
    emit_string(cg, "{\n");
    emit_indent(cg);
    emit_stmt(cg, stmt->data.if_stmt.then_branch);
    emit_dedent(cg);
    emit_indent_spaces(cg);
    emit_string(cg, "}");
    if (stmt->data.if_stmt.else_branch) {
      emit_indent_spaces(cg);
      emit_string(cg, " else {\n");
      emit_indent(cg);
      emit_stmt(cg, stmt->data.if_stmt.else_branch);
      emit_dedent(cg);
      emit_indent_spaces(cg);
      emit_string(cg, "}\n");
    } else {
      emit_string(cg, "\n");
    }
    break;
  }
  case AST_STMT_SWITCH: {
    AstNode *cond = stmt->data.switch_stmt.condition;

    Type *cond_type = cond->resolved_type;
    AstNode **cases = stmt->data.switch_stmt.cases;

    emit_indent_spaces(cg);

    switch (cond_type->kind) {
    case TYPE_CHAR:
    case TYPE_INT:
    case TYPE_U8:
    case TYPE_U32:
    case TYPE_USIZE:
    case TYPE_I16:
    case TYPE_I64:
    case TYPE_U16:
    case TYPE_U64:
    case TYPE_I8:
    case TYPE_I32:
    case TYPE_ISIZE:
    case TYPE_ENUM: {
      emit_string(cg, "switch (");
      emit_expr(cg, cond);
      emit_string(cg, ") {\n");

      for (size_t i = 0; i < stmt->data.switch_stmt.case_count; i++) {
        emit_string(cg, "case ");
        emit_expr(cg, cases[i]->data.case_stmt.condition);
        emit_string(cg, ":\n");

        emit_indent(cg);
        emit_stmt(cg, cases[i]->data.case_stmt.body);
        emit_dedent(cg);

        emit_string(cg, "break;\n");
      }

      emit_string(cg, "default: {\n");

      if (stmt->data.switch_stmt.default_case) {
        emit_indent(cg);
        emit_stmt(cg, stmt->data.switch_stmt.default_case);
        emit_dedent(cg);
      }

      emit_string(cg, "}\n");
      emit_string(cg, "break;\n");

      emit_indent_spaces(cg);
      emit_string(cg, "}\n");
      break;
    }

    case TYPE_F32:
    case TYPE_F64: {
      char buffer[64];
      char *temporary_name = get_temporary_name(cg, buffer, 64);

      size_t case_count = stmt->data.switch_stmt.case_count;

      if (case_count > 0) {
        emit_type_name(cg, stmt->data.switch_stmt.condition->resolved_type);
        emit_string(cg, " ");
        emit_string(cg, temporary_name);

        emit_string(cg, " = ");
        emit_expr(cg, cond);
        emit_string(cg, ";\n");
      }

      for (size_t i = 0; i < case_count; i++) {
        if (i == 0) {
          emit_string(cg, "if (");
        } else {
          emit_string(cg, "else if (");
        }

        emit_string(cg, temporary_name);
        emit_string(cg, " == ");

        emit_expr(cg, cases[i]->data.case_stmt.condition);
        emit_string(cg, ") {\n");

        emit_indent(cg);
        emit_stmt(cg, cases[i]->data.case_stmt.body);
        emit_dedent(cg);

        emit_string(cg, "}");
      }

      if (stmt->data.switch_stmt.default_case) {
        if (case_count > 0) {
          emit_string(cg, " else ");
        }

        emit_string(cg, "{\n");

        emit_indent(cg);
        emit_stmt(cg, stmt->data.switch_stmt.default_case);
        emit_dedent(cg);

        emit_string(cg, "}");
      }

      emit_string(cg, "\n");
      break;
    }

    case TYPE_STRING: {
      char buffer[64];
      char *temporary_name = get_temporary_name(cg, buffer, 64);

      size_t case_count = stmt->data.switch_stmt.case_count;

      if (case_count > 0) {
        emit_type_name(cg, stmt->data.switch_stmt.condition->resolved_type);
        emit_string(cg, " ");
        emit_string(cg, temporary_name);

        emit_string(cg, " = ");
        emit_expr(cg, cond);
        emit_string(cg, ";\n");
      }

      for (size_t i = 0; i < case_count; i++) {
        if (i == 0) {
          emit_string(cg, "if (");
        } else {
          emit_string(cg, " else if (");
        }

        emit_string(cg, "strcmp(");

        emit_string(cg, temporary_name);
        emit_string(cg, ", ");

        emit_expr(cg, cases[i]->data.case_stmt.condition);
        emit_string(cg, ") == 0) {\n");

        emit_indent(cg);
        emit_stmt(cg, cases[i]->data.case_stmt.body);
        emit_dedent(cg);

        emit_string(cg, "}");
      }

      if (stmt->data.switch_stmt.default_case) {
        if (case_count > 0) {
          emit_string(cg, " else ");
        }

        emit_string(cg, "{\n");

        emit_indent(cg);
        emit_stmt(cg, stmt->data.switch_stmt.default_case);
        emit_dedent(cg);

        emit_string(cg, "}");
      }

      emit_string(cg, "\n");
      break;
    }

    default:
      break;
    }

    break;
  }
  case AST_STMT_WHILE: {
    emit_indent_spaces(cg);
    emit_string(cg, "while (");
    emit_expr(cg, stmt->data.while_stmt.cond);
    emit_string(cg, ") ");
    emit_string(cg, "{\n");
    emit_indent(cg);

    // Enter LOOP scope - break/continue target this
    defer_scope_enter(cg, DEFER_SCOPE_LOOP);

    emit_stmt(cg, stmt->data.while_stmt.body);

    // Exit loop scope (emits defers at end of each iteration)
    defer_scope_exit(cg);

    emit_dedent(cg);
    emit_indent_spaces(cg);
    emit_string(cg, "}\n");
    break;
  }
  case AST_STMT_LOOP: {
    // Generate: for (long long _loop_i = start; _loop_i < end; _loop_i++) {
    //     const long long iter = _loop_i;
    //     body
    // }
    static int loop_counter = 0;
    char loop_var[64];
    snprintf(loop_var, sizeof(loop_var), "_loop_i%d", loop_counter++);

    emit_indent_spaces(cg);
    emit_string(cg, "for (int ");
    emit_string(cg, loop_var);
    emit_string(cg, " = ");
    emit_expr(cg, stmt->data.loop_stmt.start);
    emit_string(cg, "; ");
    emit_string(cg, loop_var);
    if (stmt->data.loop_stmt.inclusive) {
      emit_string(cg, " <= ");
    } else {
      emit_string(cg, " < ");
    }
    emit_expr(cg, stmt->data.loop_stmt.end);
    emit_string(cg, "; ");
    emit_string(cg, loop_var);
    emit_string(cg, "++) {\n");
    emit_indent(cg);

    // Emit const iter = _loop_i
    emit_indent_spaces(cg);
    AstNode *iter_name = stmt->data.loop_stmt.iterator_name;
    char *iterator_name = iter_name ? iter_name->data.ident.name : "iter";
    char c_iter_access[64];
    snprintf(c_iter_access, sizeof(c_iter_access),
             "const int %s = ", iterator_name);
    emit_string(cg, c_iter_access);
    emit_string(cg, loop_var);
    emit_string(cg, ";\n");

    // Enter LOOP scope
    defer_scope_enter(cg, DEFER_SCOPE_LOOP);

    emit_stmt(cg, stmt->data.loop_stmt.body);

    // Exit loop scope
    defer_scope_exit(cg);

    emit_dedent(cg);
    emit_indent_spaces(cg);
    emit_string(cg, "}\n");
    break;
  }
  case AST_STMT_FOR: {
    AstNode *init = stmt->data.for_stmt.init;

    // Check if init is a variable declaration
    if (init->kind == AST_DECL_VARIABLE) {
      // Generate: for (int i = 0; cond; update) { body }
      emit_indent_spaces(cg);
      emit_string(cg, "for (");
      emit_type_name(cg, init->resolved_type);
      emit_string(cg, " ");
      emit_string(cg, init->data.var_decl.name);
      emit_string(cg, " = ");
      emit_expr(cg, init->data.var_decl.init);
      emit_string(cg, "; ");
      emit_expr(cg, stmt->data.for_stmt.cond);
      emit_string(cg, "; ");
      
      AstNode *update = stmt->data.for_stmt.update;
      if (update->kind == AST_STMT_ASSIGN) {
        emit_expr(cg, update->data.assign_stmt.lhs);
        emit_string(cg, " = ");
        emit_expr(cg, update->data.assign_stmt.rhs);
      } else {
        emit_expr(cg, update);
      }
      emit_string(cg, ") {\n");
      emit_indent(cg);

      // Enter LOOP scope for the body
      defer_scope_enter(cg, DEFER_SCOPE_LOOP);

      emit_stmt(cg, stmt->data.for_stmt.body);

      // Exit loop scope
      defer_scope_exit(cg);

      emit_dedent(cg);
      emit_indent_spaces(cg);
      emit_string(cg, "}\n");
    } else {
      // Init is assignment, need scope for existing variable
      // Generate: { init; for (; cond; update) { body } }
      emit_indent_spaces(cg);
      emit_string(cg, "{\n");
      emit_indent(cg);

      // Emit init
      emit_stmt(cg, init);

      // Emit for loop
      emit_indent_spaces(cg);
      emit_string(cg, "for (; ");
      emit_expr(cg, stmt->data.for_stmt.cond);
      emit_string(cg, "; ");
      emit_expr(cg, stmt->data.for_stmt.update->data.assign_stmt.lhs);
      emit_string(cg, " = ");
      emit_expr(cg, stmt->data.for_stmt.update->data.assign_stmt.rhs);
      emit_string(cg, ") {\n");
      emit_indent(cg);

      // Enter LOOP scope
      defer_scope_enter(cg, DEFER_SCOPE_LOOP);

      emit_stmt(cg, stmt->data.for_stmt.body);

      // Exit loop scope
      defer_scope_exit(cg);

      emit_dedent(cg);
      emit_indent_spaces(cg);
      emit_string(cg, "}\n");

      emit_dedent(cg);
      emit_indent_spaces(cg);
      emit_string(cg, "}\n");
    }
    break;
  }
  case AST_STMT_EXPR: {
    emit_indent_spaces(cg);
    emit_expr(cg, stmt->data.expr_stmt.expr);
    emit_string(cg, ";\n");
    break;
  }
  case AST_STMT_ASSIGN: {
    emit_indent_spaces(cg);
    emit_expr(cg, stmt->data.assign_stmt.lhs);

    switch (stmt->data.assign_stmt.op) {
      case BINOP_ADD: {
        emit_string(cg, " += ");
        break;
      }

      case BINOP_SUB: {
        emit_string(cg, " -= ");
        break;
      }

      case BINOP_MUL: {
        emit_string(cg, " *= ");
        break;
      }

      case BINOP_DIV: {
        emit_string(cg, " /= ");
        break;
      }

      default: {
        emit_string(cg, " = ");
        break;
      }
    }

    emit_expr(cg, stmt->data.assign_stmt.rhs);
    emit_string(cg, ";\n");
    break;
  }
  case AST_DECL_VARIABLE: {
    emit_indent_spaces(cg);
    emit_type_name(cg, stmt->resolved_type); // Resolved type?
    emit_string(cg, " ");
    emit_string(cg, stmt->data.var_decl.name);
    if (stmt->data.var_decl.init) {
      emit_string(cg, " = ");
      emit_expr(cg, stmt->data.var_decl.init);
    } else {
      if (stmt->resolved_type->kind == TYPE_ARRAY) {
        emit_string(cg, " = {{0}, ");
        char len_buffer[32] = {0};
        sprintf(len_buffer, "%zu", stmt->resolved_type->data.array.size);
        emit_string(cg, len_buffer);
        emit_string(cg, "}");
      } else {
        emit_string(cg, " = {0}");
      }
    }
    emit_string(cg, ";\n");
    break;
  }
  case AST_DECL_CONSTANT: {
    emit_indent_spaces(cg);
    emit_string(cg, "const ");
    emit_type_name(cg, stmt->resolved_type);
    emit_string(cg, " ");
    emit_string(cg, stmt->data.const_decl.name);
    emit_string(cg, " = ");
    emit_expr(cg, stmt->data.const_decl.value);
    emit_string(cg, ";\n");
    break;
  }

  default:
    emit_indent_spaces(cg);
    emit_string(cg, "/* TODO stmt */");
  }
}

// Emit expression (minimal for PBL)
void emit_expr(Codegen *cg, AstNode *expr) {
  switch (expr->kind) {
  case AST_EXPR_LITERAL_NONE:
    emit_string(cg, "(");
    emit_type_name(cg, expr->resolved_type);
    emit_string(cg, ")");
    emit_string(cg, "{0}");
    break;

  case AST_EXPR_LITERAL_NIL:
    emit_string(cg, "NULL");
    break;

  case AST_EXPR_IDENTIFIER:
    emit_string(cg, expr->data.ident.name);
    break;

  case AST_EXPR_LITERAL_INT: {
    // Format long long
    char buf[32];
    sprintf(buf, "%lld", expr->data.int_lit.value);
    emit_string(cg, buf);
    break;
  }

  case AST_EXPR_LITERAL_FLOAT: {
    char buf[64];
    sprintf(buf, "%f", expr->data.float_lit.value);
    emit_string(cg, buf);
    break;
  }

  case AST_EXPR_LITERAL_STRING:
    emit_string(cg, "\"");
    emit_string(cg, expr->data.str_lit.value);
    emit_string(cg, "\"");
    break;

  case AST_EXPR_SIZEOF: {
    Type *type = expr->data.sizeof_expr.type_expr->resolved_type;

    emit_string(cg, "sizeof(");
    emit_type_name(cg, type);
    emit_string(cg, ")");
    break;
  }

  case AST_EXPR_LITERAL_CHAR: {
    char c = expr->data.char_lit.value;
    emit_string(cg, "'");
    if (c == '\n') {
      emit_string(cg, "\\n");
    } else if (c == '\r') {
      emit_string(cg, "\\r");
    } else if (c == '\t') {
      emit_string(cg, "\\t");
    } else if (c == '\\') {
      emit_string(cg, "\\\\");
    } else if (c == '\'') {
      emit_string(cg, "\\'");
    } else if (c == '\0') {
      emit_string(cg, "\\0");
    } else if (c >= 32 && c <= 126) {
      // Printable ASCII (including ", numbers, letters, punctuation, etc.) —
      // emit as-is
      char buf[2] = {c, '\0'};
      emit_string(cg, buf);
    } else {
      // Non-printable or non-ASCII (e.g., control characters, extended chars)
      // — use fallback
      emit_string(cg, "?");
    }
    emit_string(cg, "'");
    break;
  }

  case AST_EXPR_LITERAL_BOOL:
    emit_string(cg, expr->data.bool_lit.value ? "true" : "false");
    break;

  case AST_EXPR_GROUPED_EXPR:
    emit_string(cg, "(");
    emit_expr(cg, expr->data.grouped_expr.inner_expr);
    emit_string(cg, ")");
    break;

  case AST_EXPR_BINARY_OP: {
    emit_expr(cg, expr->data.binop.left);
    emit_string(cg, " ");
    // Map BinaryOp to C op string
    switch (expr->data.binop.op) {
    case BINOP_ADD:
      emit_string(cg, "+");
      break;
    case BINOP_SUB:
      emit_string(cg, "-");
      break;
    case BINOP_MUL:
      emit_string(cg, "*");
      break;
    case BINOP_DIV:
      emit_string(cg, "/");
      break;
    case BINOP_EQ:
      emit_string(cg, "==");
      break;
    case BINOP_NE:
      emit_string(cg, "!=");
      break;
    case BINOP_LT:
      emit_string(cg, "<");
      break;
    case BINOP_LE:
      emit_string(cg, "<=");
      break;
    case BINOP_GT:
      emit_string(cg, ">");
      break;
    case BINOP_GE:
      emit_string(cg, ">=");
      break;
    case BINOP_AND:
      emit_string(cg, "&&");
      break;
    case BINOP_OR:
      emit_string(cg, "||");
      break;
    case BINOP_BIT_AND:
      emit_string(cg, "&");
      break;
    case BINOP_BIT_OR:
      emit_string(cg, "|");
      break;
    case BINOP_BIT_XOR:
      emit_string(cg, "^");
      break;
    case BINOP_BIT_SHL:
      emit_string(cg, "<<");
      break;
    case BINOP_BIT_SHR:
      emit_string(cg, ">>");
      break;
    default:
      emit_string(cg, "/* ? */");
    }
    emit_string(cg, " ");
    emit_expr(cg, expr->data.binop.right);
    break;
  }

  case AST_EXPR_UNARY_OP: {
    // Map UnaryOp to C op string
    switch (expr->data.unop.op) {
    case UNOP_NEG:
      emit_string(cg, "-");
      break;
    case UNOP_NOT:
      emit_string(cg, "!");
      break;
    case UNOP_ADDR:
      emit_string(cg, "&");
      break;
    case UNOP_DEREF:
      emit_string(cg, "*");
      break;
    case UNOP_BIT_NOT:
      emit_string(cg, "~");
      break;
    default:
      emit_string(cg, "/* ? */");
    }
    emit_expr(cg, expr->data.unop.operand);
    break;
  }

  case AST_EXPR_SOME: {
    // (optional_T){<emit wrapped expression>, true}
    emit_string(cg, "(");
    emit_type_name(cg, expr->resolved_type);
    emit_string(cg, "){");
    emit_expr(cg, expr->data.some_expr.value);
    emit_string(cg, ", true}");
    break;
  }

  case AST_EXPR_FORCE_UNWRAP: {
    // emit assert on has_value
    // <emit operand>.value
    emit_string(cg, "({ ");
    emit_type_name(cg, expr->data.force_unwrap.operand->resolved_type);
    emit_string(cg, " __opt = ");
    emit_expr(cg, expr->data.force_unwrap.operand);
    emit_string(cg, "; assert(__opt.has_value); __opt.value; })");
    break;
  }

  case AST_EXPR_POSTFIX_INC: {
    emit_expr(cg, expr->data.postfix_inc.operand);
    emit_string(cg, "++");
    break;
  }

  case AST_EXPR_IMPLICIT_CAST: {
    Type *target = expr->data.implicit_cast.target_type;
    AstNode *src_expr = expr->data.implicit_cast.expr;
    Type *src_type = src_expr->resolved_type;

    if (src_type->kind == TYPE_ARRAY && target->kind == TYPE_SLICE) {
      // Special: construct slice from array
      emit_string(cg, "(");
      emit_type_name(cg, target); // e.g., slice_int
      emit_string(cg, "){ &");
      emit_expr(cg, src_expr);
      emit_string(cg, ".data[0], ");
      emit_expr(cg, src_expr);
      emit_string(cg, ".len }");
    } else {
      // General cast, e.g., (float)int
      emit_string(cg, "(");
      emit_type_name(cg, target);
      emit_string(cg, ")");
      emit_expr(cg, src_expr);
    }
    break;
  }

  case AST_EXPR_EXPLICIT_CAST: {
    Type *target_type = expr->resolved_type;

    emit_string(cg, "(");
    emit_type_name(cg, target_type); // Uses canonical name
    emit_string(cg, ")");
    emit_expr(cg, expr->data.explicit_cast.expr);
    break;
  }

  case AST_EXPR_ARRAY_LITERAL: {
    emit_string(cg, "(");
    emit_type_name(cg, expr->resolved_type);
    emit_string(cg, "){ {");
    for (size_t i = 0; i < expr->data.array_literal.element_count; i++) {
      if (i > 0)
        emit_string(cg, ", ");
      emit_expr(cg, expr->data.array_literal.elements[i]);
    }
    emit_string(cg, "}, ");
    char count_buf[16];
    sprintf(count_buf, "%zu", expr->data.array_literal.element_count);
    emit_string(cg, count_buf);
    emit_string(cg, " }");
    break;
  }

  case AST_EXPR_ARRAY_REPEAT: {
    AstNode *value_expr = expr->data.array_repeat.value;
    size_t count = expr->data.array_repeat.count;
    Type *array_type = expr->resolved_type;
    Type *element_type = array_type->data.array.element;

    // Optimization: if value is zero, use {0} initializer
    if (is_zero_value(value_expr)) {
      emit_string(cg, "(");
      emit_type_name(cg, array_type);
      emit_string(cg, "){0}");
      break;
    }

    // Generate statement expression with loop
    emit_string(cg, "({\n");
    emit_indent(cg);

    // Declare the array variable
    emit_indent_spaces(cg);
    emit_type_name(cg, array_type);
    emit_string(cg, " _arr;\n");

    // Evaluate value expression once
    emit_indent_spaces(cg);
    emit_type_name(cg, element_type);
    emit_string(cg, " _val = ");
    emit_expr(cg, value_expr);
    emit_string(cg, ";\n");

    // Generate the fill loop
    emit_indent_spaces(cg);
    emit_string(cg, "for (size_t _i = 0; _i < ");
    char count_buf[32];
    sprintf(count_buf, "%zu", count);
    emit_string(cg, count_buf);
    emit_string(cg, "; _i++) {\n");
    emit_indent(cg);

    emit_indent_spaces(cg);
    emit_string(cg, "_arr.data[_i] = _val;\n");

    emit_dedent(cg);
    emit_indent_spaces(cg);
    emit_string(cg, "}\n");

    // Set the array length
    emit_indent_spaces(cg);
    emit_string(cg, "_arr.len = ");
    emit_string(cg, count_buf);
    emit_string(cg, ";\n");

    // Return the array value
    emit_indent_spaces(cg);
    emit_string(cg, "_arr;\n");

    emit_dedent(cg);
    emit_indent_spaces(cg);
    emit_string(cg, "})");
    break;
  }

  case AST_EXPR_SLICE: {
    emit_string(cg, "(");
    emit_type_name(cg, expr->resolved_type);
    emit_string(cg, "){ ");
    if (expr->data.slice_expr.array->resolved_type->kind == TYPE_POINTER) {
      emit_expr(cg, expr->data.slice_expr.array);
      emit_string(cg, " + ");
      if (expr->data.slice_expr.start) {
        emit_expr(cg, expr->data.slice_expr.start);
      } else {
        emit_string(cg, "0");
      }
    } else {
      emit_string(cg, "&");
      emit_expr(cg, expr->data.slice_expr.array);
      emit_string(cg, ".data[");
      if (expr->data.slice_expr.start) {
        emit_expr(cg, expr->data.slice_expr.start);
      } else {
        emit_string(cg, "0");
      }
      emit_string(cg, "]");
    }
    emit_string(cg, ", ");
    if (expr->data.slice_expr.end) {
      emit_expr(cg, expr->data.slice_expr.end);
      emit_string(cg, " - ");
      if (expr->data.slice_expr.start) {
        emit_expr(cg, expr->data.slice_expr.start);
      } else {
        emit_string(cg, "0");
      }
    } else {
      emit_expr(cg, expr->data.slice_expr.array);
      emit_string(cg, ".len - ");
      if (expr->data.slice_expr.start) {
        emit_expr(cg, expr->data.slice_expr.start);
      } else {
        emit_string(cg, "0");
      }
    }
    emit_string(cg, " }");
    break;
  }

  case AST_EXPR_INDEX: {
    AstNode *array_expr = expr->data.index_expr.array;
    Type *array_type = array_expr->resolved_type;

    if (array_type->kind == TYPE_STRING) {
      // For str, index directly
      if (compiler_opts.release_mode == RELEASE_DEBUG) {
        emit_string(cg, "({ int __index = ");
        emit_expr(cg, expr->data.index_expr.index);
        emit_string(cg, "; char *__item = ");
        emit_expr(cg, array_expr);
        emit_string(cg, "; assert(__index >= 0 && __index < (int)strlen(__item)); __item[__index]; })");
      } else {
        emit_expr(cg, array_expr);
        emit_string(cg, "[");
        emit_expr(cg, expr->data.index_expr.index);
        emit_string(cg, "]");
      }
    } else {
      // For arrays/slices, use .data
      if (compiler_opts.release_mode == RELEASE_DEBUG) {
        // Bounds checking
        emit_string(cg, "({ int __index = ");
        emit_expr(cg, expr->data.index_expr.index);
        emit_string(cg, "; ");

        emit_type_name(cg, array_expr->resolved_type);
        emit_string(cg, " __item = ");
        emit_expr(cg, array_expr);
        emit_string(cg, "; assert(__index >= 0 && __index < (int)__item.len); __item.data[__index]; })");
      } else {
        emit_expr(cg, array_expr);
        emit_string(cg, ".data[");
        emit_expr(cg, expr->data.index_expr.index);
        emit_string(cg, "]");
      }
    }
    break;
  }

  case AST_EXPR_CALL: {
    Type *func_type = expr->data.call.func->resolved_type;

    emit_expr(cg, expr->data.call.func);
    emit_string(cg, "(");

    if (func_type->data.func.is_variadic) {
      size_t arg_count = expr->data.call.arg_count;
      size_t param_count = func_type->data.func.param_count;
      size_t fixed_params = param_count - 1;
      size_t variadic_count = arg_count - fixed_params;

      // Emit fixed arguments
      for (size_t i = 0; i < fixed_params; i++) {
        if (i > 0)
          emit_string(cg, ", ");
        emit_expr(cg, expr->data.call.args[i]);
      }

      // Emit variadic count
      if (fixed_params > 0) {
        emit_string(cg, ", ");
      }

      Type **param_types = func_type->data.func.param_types;
      Type *variadic_type = param_types[fixed_params];

      if (arg_count == fixed_params + 1 && variadic_type->kind == TYPE_SLICE) {
        emit_expr(cg, expr->data.call.args[fixed_params]);
      } else {
        Type *inner_type = variadic_type->data.slice.element;

        if (variadic_count > 0) {
          emit_string(cg, "({\n");
          emit_indent_spaces(cg);
          emit_indent_spaces(cg);

          char count_buf[24] = {0};
          snprintf(count_buf, sizeof(count_buf), "%zu", variadic_count);

          emit_string(cg, "size_t __argc = ");
          emit_string(cg, count_buf);
          emit_string(cg, ";\n");

          emit_indent_spaces(cg);
          emit_indent_spaces(cg);

          emit_type_name(cg, inner_type);
          emit_string(cg, " *__args = alloca(sizeof(");
          emit_type_name(cg, inner_type);
          emit_string(cg, ") * __argc);\n");

          // Emit variadic arguments
          for (size_t i = fixed_params; i < expr->data.call.arg_count; i++) {
            char index_buff[24] = {0};
            snprintf(index_buff, sizeof(index_buff), "%zu", i);

            emit_indent_spaces(cg);
            emit_indent_spaces(cg);

            emit_string(cg, "__args[");
            emit_string(cg, index_buff);
            emit_string(cg, "] = ");

            emit_expr(cg, expr->data.call.args[i]);

            emit_string(cg, ";\n");
          }

          emit_indent_spaces(cg);
          emit_indent_spaces(cg);

          emit_string(cg, "(");
          emit_type_name(cg, variadic_type);
          emit_string(cg, "){ __args, __argc };\n");

          emit_indent_spaces(cg);
          emit_string(cg, "})");
        } else {
          // No elements
          emit_string(cg, "(");
          emit_type_name(cg, variadic_type);
          emit_string(cg, "){ NULL, 0 }");
        }
      }
    } else {
      for (size_t i = 0; i < expr->data.call.arg_count; i++) {
        if (i > 0)
          emit_string(cg, ", ");
        emit_expr(cg, expr->data.call.args[i]);
      }
    }

    emit_string(cg, ")");
    break;
  }

  case AST_EXPR_TUPLE: {
    emit_string(cg, "(");
    emit_type_name(cg, expr->resolved_type);
    emit_string(cg, ") {");
    for (size_t i = 0; i < expr->data.tuple_expr.element_count; i++) {
      if (i > 0)
        emit_string(cg, ", ");
      emit_expr(cg, expr->data.tuple_expr.elements[i]);
    }
    emit_string(cg, "}");
    break;
  }

  case AST_EXPR_MEMBER: {
    AstNode *object_expr = expr->data.member_expr.object;
    const char *member = expr->data.member_expr.member;

    // Get the type of the object expression
    Type *object_type = object_expr->resolved_type;

    if (object_type->kind == TYPE_OPTIONAL) {
      emit_expr(cg, object_expr);
      emit_string(cg, ".");
      emit_string(cg, "has_value");
      break;
    }

    // Enums require their name prefixed to variant name
    if (object_type->kind == TYPE_ENUM) {
      emit_string(cg, object_type->canonical_name);
      emit_string(cg, "_");
      emit_string(cg, member);
      break;
    }

    // Check if the object is a pointer and prepare the operator
    if (object_type && object_type->kind == TYPE_POINTER) {
      Type *base_type = object_type->data.ptr.base;
      if (base_type && base_type->kind == TYPE_STRUCT) {
        // Pointer to struct: emit object directly, then -> (e.g., ptr->field)
        emit_expr(cg, object_expr);
        emit_string(cg, "->");
      } else {
        // Pointer to tuple, array, or slice: emit (*object). (e.g.,
        // (*ptr).len or (*ptr)._0)
        emit_string(cg, "(*");
        emit_expr(cg, object_expr);
        emit_string(cg, ").");
      }
    } else {
      // Non-pointer: emit object, then . (e.g., obj.field or t._0)
      emit_expr(cg, object_expr);
      emit_string(cg, ".");
    }

    // For tuple access, prepend underscore to numeric fields
    if (member[0] >= '0' && member[0] <= '9') {
      emit_string(cg, "_");
    }

    // Emit the member name
    emit_string(cg, member);
    break;
  }

  case AST_EXPR_STRUCT_LITERAL: {
    emit_string(cg, "(");
    emit_string(cg, expr->data.struct_literal.type_name);
    emit_string(cg, "){");
    for (size_t i = 0; i < expr->data.struct_literal.field_count; i++) {
      if (i > 0)
        emit_string(cg, ", ");
      emit_string(cg, ".");
      emit_string(cg, expr->data.struct_literal.field_names[i]);
      emit_string(cg, " = ");
      emit_expr(cg, expr->data.struct_literal.field_values[i]);
    }
    emit_string(cg, "}");
    break;
  }

  case AST_EXPR_FUNCTION: {
    emit_string(cg, expr->data.func_expr.symbol);
    break;
  }

  default:
    emit_string(cg, "/* TODO */");
  }
}
