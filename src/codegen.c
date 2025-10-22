#include "codegen.h"
#include "alloc.h"
#include "ast.h"
#include "symbol.h"
#include "type.h"
#include "options.h"
#include <stddef.h>
#include <stdio.h>

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
    *cap = (*cap == 0) ? 1024 : *cap * 2; // Start at 1K, double as needed
    *buf = realloc(*buf, *cap);
    if (!*buf) { /* error */
    }
  }
  memcpy(*buf + *len, str, str_len);
  *len += str_len;
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
    cg->preamble =
    "#include <stdlib.h>\n#include <stdbool.h>\n#include <stdio.h>\n\n";
  } else {
    // Freestanding has basic default includes
    cg->preamble = "#include <stddef.h>\n#include <stdbool.h>\n\n";
  }

  // Init uthash sets to empty
  cg->declared_types = NULL;
  cg->defined_types = NULL;
  cg->declared_vars = NULL;
}

void emit_string(Codegen *cg, const char *str) {
  if (!cg->current_section)
    return; // Not collecting into sections yet
  size_t len = strlen(str);
  append_to_section(cg, str, len);
}

static void emit_indent_spaces(Codegen *cg) {
  for (int i = 0; i < cg->indent_level; i++) {
    emit_string(cg, "    ");
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

void emit_program(Codegen *cg) {
  // Emit forwards and defs for types
  cg->current_section = "forward_types"; // For typedefs
  Symbol *sym, *tmp;
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    if (sym->kind == SYMBOL_TYPE && sym->type->kind != TYPE_OPAQUE) {
      Type *t = type_lookup(sym->name);
      if (t)
        emit_type_if_needed(cg, t); // Emits typedef and def if struct/tuple
    }
  }

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
          emit_string(cg, "{0}");
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
      emit_string(cg, sym->name);
      emit_string(cg, "(");
      for (size_t i = 0; i < func->data.func.param_count; i++) {
        if (i > 0)
          emit_string(cg, ", ");
        emit_type_name(cg, func->data.func.param_types[i]);
        emit_string(cg, " ");
        emit_string(cg, sym->decl->data.func_decl.params[i].name);
      }
      emit_string(cg, ")");
      emit_string(cg, ";\n");
    }
  }

  // Emit func definitions
  cg->current_section = "defs";
  HASH_ITER(hh, global_scope->symbols, sym, tmp) {
    if (sym->kind == SYMBOL_FUNCTION) {
      AstNode *func = sym->decl; // Func decl AST node
      emit_type_name(cg, sym->type->data.func.return_type);
      emit_string(cg, " ");
      emit_string(cg, func->data.func_decl.name);
      emit_string(cg, "(");
      for (size_t i = 0; i < sym->type->data.func.param_count; i++) {
        if (i > 0)
          emit_string(cg, ", ");
        emit_type_name(cg, sym->type->data.func.param_types[i]);
        emit_string(cg, " ");
        emit_string(cg, func->data.func_decl.params[i].name);
      }
      emit_string(cg, ") ");
      emit_string(cg, "{\n");
      emit_indent(cg);
      // Emit body (minimal traversal)
      emit_stmt(cg, func->data.func_decl.body);
      emit_dedent(cg);
      emit_string(cg, "}\n");
    }
  }

  // Emit sections
  emit_sections(cg);
}

void emit_type_name(Codegen *cg, Type *type) {
  if (!type)
    return;
  switch (type->kind) {
  case TYPE_INT:
    emit_string(cg, "int");
    break;
  case TYPE_FLOAT:
    emit_string(cg, "float");
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
  case TYPE_DOUBLE:
    emit_string(cg, "double");
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
    emit_string(cg, type->canonical_name); // Just "Node" or "tuple_int_int",
                                           // not "struct ..."
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

  // Clear uthash sets (if uthash.h defines HASH_CLEAR)
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
    emit_string(cg, "typedef struct ");
    emit_string(cg, canonical);
    emit_string(cg, " ");
    emit_string(cg, canonical);
    emit_string(cg, ";\n");
    cg->current_section = old_section;

    // Insert into declared_types hash
    decl_entry = arena_alloc(&long_lived, sizeof(CodegenTypeEntry));
    decl_entry->key = str_dup(canonical); // Persist key
    HASH_ADD_STR(cg->declared_types, key, decl_entry);
  }

  // Check if already defined (full struct/tuple)
  if (type->kind == TYPE_STRUCT || type->kind == TYPE_TUPLE ||
      type->kind == TYPE_ARRAY || type->kind == TYPE_SLICE) {
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
      } else { // TYPE_TUPLE
        for (size_t i = 0; i < type->data.tuple.element_count; i++) {
          emit_indent_spaces(cg);
          emit_type_name(cg, type->data.tuple.element_types[i]);
          emit_string(cg, " _");
          char idx_str[2] = {'0' + (char)i, '\0'};
          emit_string(cg, idx_str);
          emit_string(cg, ";");
          emit_string(cg, "\n");
        }
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
    emit_indent_spaces(cg);
    emit_string(cg, "break;\n");
    break;
  case AST_STMT_CONTINUE:
    emit_indent_spaces(cg);
    emit_string(cg, "continue;\n");
    break;
  case AST_STMT_PRINT: {
    Type *type = stmt->data.print_stmt.expr->resolved_type;

    emit_indent_spaces(cg);
    if (type->kind == TYPE_INT || type->kind == TYPE_I32) {
      emit_string(cg, "printf(\"%d\\n\", ");
    } else if (type->kind == TYPE_I8) {
      emit_string(cg, "printf(\"%hhd\\n\", ");
    } else if (type->kind == TYPE_I16) {
      emit_string(cg, "printf(\"%hd\\n\", ");
    } else if (type->kind == TYPE_I64) {
      emit_string(cg, "printf(\"%lld\\n\", ");
    } else if (type->kind == TYPE_ISIZE) {
      emit_string(cg, "printf(\"%td\\n\", ");
    } else if (type->kind == TYPE_U8) {
      emit_string(cg, "printf(\"%hhu\\n\", ");
    } else if (type->kind == TYPE_U16) {
      emit_string(cg, "printf(\"%hu\\n\", ");
    } else if (type->kind == TYPE_U32) {
      emit_string(cg, "printf(\"%lu\\n\", ");
    } else if (type->kind == TYPE_U64) {
      emit_string(cg, "printf(\"%llu\\n\", ");
    } else if (type->kind == TYPE_USIZE) {
      emit_string(cg, "printf(\"%zu\\n\", ");
    } else if (type->kind == TYPE_CHAR) {
      emit_string(cg, "printf(\"%c\\n\", ");
    } else if (type->kind == TYPE_FLOAT || type->kind == TYPE_DOUBLE) {
      emit_string(cg, "printf(\"%f\\n\", ");
    } else if (type->kind == TYPE_STRING) {
      emit_string(cg, "printf(\"%s\\n\", ");
    } else if (type->kind == TYPE_BOOL) {
      emit_string(cg, "printf(\"%s\\n\", ");
      emit_expr(cg, stmt->data.print_stmt.expr);
      emit_string(cg, " ? \"true\" : \"false\"");
      emit_string(cg, ");\n");
      return;
    } else {
      emit_string(cg, "printf(\"");
      emit_string(cg, type_name(stmt->data.print_stmt.expr->resolved_type));
      emit_string(cg, "\\n\");\n");
      return;
    }

    emit_expr(cg, stmt->data.print_stmt.expr);
    emit_string(cg, ");\n");
    break;
  }
  case AST_STMT_RETURN:
    emit_indent_spaces(cg);
    emit_string(cg, "return");
    if (stmt->data.return_stmt.expr) {
      emit_string(cg, " ");
      emit_expr(cg, stmt->data.return_stmt.expr);
    }
    emit_string(cg, ";\n");
    break;
  case AST_STMT_BLOCK:
    for (size_t i = 0; i < stmt->data.block_stmt.stmt_count; i++) {
      emit_stmt(cg, stmt->data.block_stmt.stmts[i]);
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
    emit_stmt(cg, stmt->data.while_stmt.body);
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
    emit_string(cg, "for (long long ");
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
    emit_string(cg, "const long long iter = ");
    emit_string(cg, loop_var);
    emit_string(cg, ";\n");

    emit_stmt(cg, stmt->data.loop_stmt.body);
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
      emit_expr(cg, stmt->data.for_stmt.update->data.assign_stmt.lhs);
      emit_string(cg, " = ");
      emit_expr(cg, stmt->data.for_stmt.update->data.assign_stmt.rhs);
      emit_string(cg, ") {\n");
      emit_indent(cg);
      emit_stmt(cg, stmt->data.for_stmt.body);
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
      emit_stmt(cg, stmt->data.for_stmt.body);
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
    emit_string(cg, " = ");
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
      emit_string(cg, " = {0}");
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
    default:
      emit_string(cg, "/* ? */");
    }
    emit_expr(cg, expr->data.unop.operand);
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
      emit_expr(cg, array_expr);
      emit_string(cg, "[");
      emit_expr(cg, expr->data.index_expr.index);
      emit_string(cg, "]");
    } else {
      // For arrays/slices, use .data
      emit_expr(cg, array_expr);
      emit_string(cg, ".data[");
      emit_expr(cg, expr->data.index_expr.index);
      emit_string(cg, "]");
    }
    break;
  }

  case AST_EXPR_CALL: {
    emit_expr(cg, expr->data.call.func);
    emit_string(cg, "(");
    for (size_t i = 0; i < expr->data.call.arg_count; i++) {
      if (i > 0)
        emit_string(cg, ", ");
      emit_expr(cg, expr->data.call.args[i]);
    }
    emit_string(cg, ")");
    break;
  }

  case AST_EXPR_TUPLE: {
    emit_string(cg, "(");
    emit_type_name(cg, expr->data.tuple_expr.resolved_type);
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

  default:
    emit_string(cg, "/* TODO */");
  }
}
