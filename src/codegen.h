#ifndef CODEGEN_H
#define CODEGEN_H

#include "alloc.h"
#include "ast.h"
#include "symbol.h"
#include "type.h"
#include "uthash.h"
#include <stdio.h>

// Uthash struct for set-based deduplication (key: canonical type/variable name)
typedef struct {
  char *key;
  UT_hash_handle hh;
} CodegenTypeEntry;

typedef struct {
  FILE *output;
  int indent_level;

  // Section buffers (grow with realloc)
  char *preamble;
  char *forward_types;
  size_t forward_types_len, forward_types_cap;
  char *type_defs;
  size_t type_defs_len, type_defs_cap;
  char *forward_vars_funcs;
  size_t forward_vars_funcs_len, forward_vars_funcs_cap;
  char *defs;
  size_t defs_len, defs_cap;
  char *current_section;

  // Uthash sets for deduplication
  CodegenTypeEntry *declared_types;
  CodegenTypeEntry *defined_types;
  CodegenTypeEntry *declared_vars;

  // Temporaries
  size_t temporary_count;
} Codegen;

// Core functions
void codegen_init(Codegen *cg, FILE *output);
void emit_program(Codegen *cg);
void emit_sections(Codegen *cg);

// Emission helpers
void emit_string(Codegen *cg, const char *str);
void emit_indent(Codegen *cg);
void emit_dedent(Codegen *cg);
void emit_type_name(Codegen *cg, Type *type);
void emit_to_section(Codegen *cg, const char *section,
                     const char *str); // New: Append to section

// Helpers for types and deduplication
void emit_type_if_needed(
    Codegen *cg, Type *type); // Check/gen forward/def if not declared/defined

// Ast code generators
void emit_stmt(Codegen *cg, AstNode *stmt);
void emit_expr(Codegen *cg, AstNode *expr);

#endif
