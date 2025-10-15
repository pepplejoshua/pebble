#include "codegen.h"
#include "symbol.h"
#include "type.h"
#include <stddef.h>
#include <stdio.h>

static void append_to_section(Codegen *cg, const char *str, size_t str_len) {
    char **buf = NULL;
    size_t *len, *cap;
    if (strcmp(cg->current_section, "forward_types") == 0) {
        buf = &cg->forward_types; len = &cg->forward_types_len; cap = &cg->forward_types_cap;
    } else if (strcmp(cg->current_section, "type_defs") == 0) {
        buf = &cg->type_defs; len = &cg->type_defs_len; cap = &cg->type_defs_cap;
    } else if (strcmp(cg->current_section, "forward_vars_funcs") == 0) {
        buf = &cg->forward_vars_funcs; len = &cg->forward_vars_funcs_len; cap = &cg->forward_vars_funcs_cap;
    } else if (strcmp(cg->current_section, "defs") == 0) {
        buf = &cg->defs; len = &cg->defs_len; cap = &cg->defs_cap;
    } else return;  // Invalid section

    if (*len + str_len >= *cap) {
        *cap = (*cap == 0) ? 1024 : *cap * 2;  // Start at 1K, double as needed
        *buf = realloc(*buf, *cap);
        if (!*buf) { /* error */ }
    }
    memcpy(*buf + *len, str, str_len);
    *len += str_len;
}

void codegen_init(Codegen *cg, FILE *output) {
    cg->output = output;
    cg->indent_level = 0;
    cg->current_section = NULL;

    // Init section buffers (start empty)
    cg->forward_types = NULL; cg->forward_types_len = 0; cg->forward_types_cap = 0;
    cg->type_defs = NULL; cg->type_defs_len = 0; cg->type_defs_cap = 0;
    cg->forward_vars_funcs = NULL; cg->forward_vars_funcs_len = 0; cg->forward_vars_funcs_cap = 0;
    cg->defs = NULL; cg->defs_len = 0; cg->defs_cap = 0;

    // Set preamble (use alloc.c's str_dup for long-lived strings if needed)
    cg->preamble = "#include <stdlib.h>\n#include <stdbool.h>\n\n";

    // Init uthash sets to empty
    cg->declared_types = NULL;
    cg->defined_types = NULL;
    cg->declared_vars = NULL;
}

void emit_string(Codegen *cg, const char *str) {
    if (!cg->current_section) return;  // Not collecting into sections yet
    size_t len = strlen(str);
    append_to_section(cg, str, len);
}

static void emit_indent_spaces(Codegen *cg) {
    for (int i = 0; i < cg->indent_level; i++) {
        emit_string(cg, "    ");
    }
}

void emit_line(Codegen *cg, const char *line) {
        if (!cg->current_section) return;
        // Build indented line in a temp buffer (use a stack buf; 1K should suffice for PBL)
        char buf[1024];
        int indent_len = 0;
        for (int i = 0; i < cg->indent_level; i++) indent_len += 4;
        if (indent_len + strlen(line) + 1 >= sizeof(buf)) { /* error or dyn alloc */ }
        memset(buf, ' ', indent_len);
        strcpy(buf + indent_len, line);
        size_t len = strlen(buf);
        buf[len] = '\n'; len++;  // Add newline
        append_to_section(cg, buf, len);
}

void emit_indent(Codegen *cg) {
    cg->indent_level++;
}

void emit_dedent(Codegen *cg) {
    cg->indent_level--;
}

void emit_program(Codegen *cg) {
    // Emit forwards and defs for types
    cg->current_section = "forward_types";  // For typedefs
    Symbol *sym, *tmp;
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        if (sym->kind == SYMBOL_TYPE) {
            Type *t = type_lookup(sym->name);
            if (t) emit_type_if_needed(cg, t);  // Emits typedef and def if struct/tuple
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
            emit_line(cg, ";");
        }
    }
    cg->current_section = "defs";
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        if (sym->kind == SYMBOL_VARIABLE || sym->kind == SYMBOL_CONSTANT) {
            emit_type_name(cg, sym->type);
            emit_string(cg, " ");
            emit_string(cg, sym->name);
            if (sym->kind == SYMBOL_VARIABLE && sym->decl && sym->decl->data.var_decl.init) {
                emit_string(cg, " = ");
                emit_expr(cg, sym->decl->data.var_decl.init);
            } else if (sym->kind == SYMBOL_CONSTANT && sym->decl && sym->decl->data.const_decl.value) {
                emit_string(cg, " = ");
                emit_expr(cg, sym->decl->data.const_decl.value);
            }
            emit_line(cg, ";");

        }
    }

    // Emit func prototypes
    cg->current_section = "forward_vars_funcs";
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        if (sym->kind == SYMBOL_FUNCTION) {
            Type *func = sym->type;  // Assume sym->ast points to func node
            // Emit prototype
            emit_type_name(cg, func->data.func.return_type);
            emit_string(cg, " ");
            emit_string(cg, sym->name);
            emit_string(cg, "(");
            for (size_t i = 0; i < func->data.func.param_count; i++) {
                if (i > 0) emit_string(cg, ", ");
                emit_type_name(cg, func->data.func.param_types[i]);
                emit_string(cg, " ");
                emit_string(cg, sym->decl->data.func_decl.params[i].name);
            }
            emit_string(cg, ")");
            emit_line(cg, ";");
        }
    }

    // Emit func definitions
    cg->current_section = "defs";
    HASH_ITER(hh, global_scope->symbols, sym, tmp) {
        if (sym->kind == SYMBOL_FUNCTION) {
            AstNode *func = sym->decl;  // Func decl AST node
            emit_type_name(cg, sym->type->data.func.return_type);
            emit_string(cg, " ");
            emit_string(cg, func->data.func_decl.name);
            emit_string(cg, "(");
            for (size_t i = 0; i < sym->type->data.func.param_count; i++) {
                if (i > 0) emit_string(cg, ", ");
                emit_type_name(cg, sym->type->data.func.param_types[i]);
                emit_string(cg, " ");
                emit_string(cg, func->data.func_decl.params[i].name);
            }
            emit_string(cg, ") ");
            emit_line(cg, "{");
            emit_indent(cg);
            // Emit body (minimal traversal)
            emit_stmt(cg, func->data.func_decl.body);
            emit_dedent(cg);
            emit_line(cg, "}");
        }
    }


    // Emit sections
    emit_sections(cg);
}

void emit_type_name(Codegen *cg, Type *type) {
    if (!type) return;
    switch (type->kind) {
        case TYPE_INT:
            emit_string(cg, "int"); break;
        case TYPE_FLOAT:
            emit_string(cg, "float"); break;
        case TYPE_BOOL:
            emit_string(cg, "bool"); break;
        case TYPE_STRING:
            emit_string(cg, "char*"); break;
        case TYPE_VOID:
            emit_string(cg, "void"); break;
        case TYPE_POINTER:
            emit_type_name(cg, type->data.ptr.base);
            emit_string(cg, "*");
            break;
        case TYPE_STRUCT:
        case TYPE_TUPLE:
            emit_string(cg, type->canonical_name);  // Just "Node" or "tuple_int_int", not "struct ..."
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
            emit_string(cg, type->canonical_name); break;
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
    if (!type) return;

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
        decl_entry = malloc(sizeof(CodegenTypeEntry));
        decl_entry->key = str_dup(canonical);  // Persist key
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
            emit_line(cg, "");
            emit_indent(cg);
            if (type->kind == TYPE_STRUCT) {
                for (size_t i = 0; i < type->data.struct_data.field_count; i++) {
                    emit_indent_spaces(cg);
                    emit_type_name(cg, type->data.struct_data.field_types[i]);
                    emit_string(cg, " ");
                    emit_string(cg, type->data.struct_data.field_names[i]);
                    emit_string(cg, ";");
                    emit_line(cg, "");
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
                emit_line(cg, "");
                emit_indent_spaces(cg);
                emit_string(cg, "size_t len;");
                emit_line(cg, "");
            } else {  // TYPE_TUPLE
                for (size_t i = 0; i < type->data.tuple.element_count; i++) {
                    emit_indent_spaces(cg);
                    emit_type_name(cg, type->data.tuple.element_types[i]);
                    emit_string(cg, " _");
                    char idx_str[2] = {'0' + (char)i, '\0'};
                    emit_string(cg, idx_str);
                    emit_string(cg, ";");
                    emit_line(cg, "");
                }
            }
            emit_dedent(cg);
            emit_indent_spaces(cg);
            emit_string(cg, "};");
            emit_line(cg, "");
            cg->current_section = old_section;

            // Insert into defined_types hash to prevent duplicates
            def_entry = malloc(sizeof(CodegenTypeEntry));
            def_entry->key = str_dup(canonical);  // Persist key
            HASH_ADD_STR(cg->defined_types, key, def_entry);
        }
    }
}

// Emit statement (minimal)
void emit_stmt(Codegen *cg, AstNode *stmt) {
    if (!stmt) return;
    switch (stmt->kind) {
        case AST_STMT_RETURN:
            emit_indent_spaces(cg);
            emit_string(cg, "return ");
            emit_expr(cg, stmt->data.return_stmt.expr);
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
            emit_line(cg, "{");
            emit_indent(cg);
            emit_stmt(cg, stmt->data.if_stmt.then_branch);
            emit_dedent(cg);
            emit_indent_spaces(cg);
            emit_line(cg, "}");
            if (stmt->data.if_stmt.else_branch) {
                emit_indent_spaces(cg);
                emit_line(cg, " else {");
                emit_indent(cg);
                emit_stmt(cg, stmt->data.if_stmt.else_branch);
                emit_dedent(cg);
                emit_indent_spaces(cg);
                emit_line(cg, "}");
            }
            break;
        }
        case AST_STMT_WHILE: {
            emit_indent_spaces(cg);
            emit_string(cg, "while (");
            emit_expr(cg, stmt->data.while_stmt.cond);
            emit_string(cg, ") ");
            emit_line(cg, "{");
            emit_indent(cg);
            emit_stmt(cg, stmt->data.while_stmt.body);
            emit_dedent(cg);
            emit_indent_spaces(cg);
            emit_line(cg, "}");
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
            emit_type_name(cg, stmt->data.var_decl.resolved_type);  // Resolved type?
            emit_string(cg, " ");
            emit_string(cg, stmt->data.var_decl.name);
            if (stmt->data.var_decl.init) {
                emit_string(cg, " = ");
                emit_expr(cg, stmt->data.var_decl.init);
            }
            emit_string(cg, ";\n");
            break;
        }
        case AST_DECL_CONSTANT: {
            emit_indent_spaces(cg);
            emit_string(cg, "const ");
            emit_type_name(cg, stmt->data.const_decl.resolved_type);
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
        case AST_EXPR_LITERAL_BOOL:
            emit_string(cg, expr->data.bool_lit.value ? "true" : "false");
            break;
        case AST_EXPR_BINARY_OP: {
            emit_string(cg, "(");
            emit_expr(cg, expr->data.binop.left);
            emit_string(cg, " ");
            // Map BinaryOp to C op string
            switch (expr->data.binop.op) {
                case BINOP_ADD: emit_string(cg, "+"); break;
                case BINOP_SUB: emit_string(cg, "-"); break;
                case BINOP_MUL: emit_string(cg, "*"); break;
                case BINOP_DIV: emit_string(cg, "/"); break;
                case BINOP_EQ: emit_string(cg, "=="); break;
                case BINOP_NE: emit_string(cg, "!="); break;
                case BINOP_LT: emit_string(cg, "<"); break;
                case BINOP_LE: emit_string(cg, "<="); break;
                case BINOP_GT: emit_string(cg, ">"); break;
                case BINOP_GE: emit_string(cg, ">="); break;
                case BINOP_AND: emit_string(cg, "&&"); break;
                case BINOP_OR: emit_string(cg, "||"); break;
                default: emit_string(cg, "/* ? */");
            }
            emit_string(cg, " ");
            emit_expr(cg, expr->data.binop.right);
            emit_string(cg, ")");
            break;
        }

        case AST_EXPR_UNARY_OP: {
            // Map UnaryOp to C op string
            switch (expr->data.unop.op) {
                case UNOP_NEG: emit_string(cg, "-"); break;
                case UNOP_NOT: emit_string(cg, "!"); break;
                case UNOP_ADDR: emit_string(cg, "&"); break;
                case UNOP_DEREF: emit_string(cg, "*"); break;
                default: emit_string(cg, "/* ? */");
            }
            emit_expr(cg, expr->data.unop.operand);
            break;
        }

        case AST_EXPR_ARRAY_LITERAL: {
            emit_string(cg, "(");
            emit_type_name(cg, expr->data.array_literal.resolved_type);
            emit_string(cg, "){ {");
            for (size_t i = 0; i < expr->data.array_literal.element_count; i++) {
                if (i > 0) emit_string(cg, ", ");
                emit_expr(cg, expr->data.array_literal.elements[i]);
            }
            emit_string(cg, "}, ");
            char count_buf[16];
            sprintf(count_buf, "%zu", expr->data.array_literal.element_count);
            emit_string(cg, count_buf);
            emit_string(cg, " }");
            break;
        }

        case AST_EXPR_SLICE: {
            emit_string(cg, "(");
            emit_type_name(cg, expr->data.slice_expr.resolved_type);
            emit_string(cg, "){ &");
            emit_expr(cg, expr->data.slice_expr.array);
            emit_string(cg, ".data[");
            if (expr->data.slice_expr.start) {
                emit_expr(cg, expr->data.slice_expr.start);
            } else {
                emit_string(cg, "0");
            }
            emit_string(cg, "], ");
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
            emit_expr(cg, expr->data.index_expr.array);
            emit_string(cg, ".data[");
            emit_expr(cg, expr->data.index_expr.index);
            emit_string(cg, "]");
            break;
        }

        case AST_EXPR_CALL: {
            emit_expr(cg, expr->data.call.func);
            emit_string(cg, "(");
            for (size_t i = 0; i < expr->data.call.arg_count; i++) {
                if (i > 0) emit_string(cg, ", ");
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
                if (i > 0) emit_string(cg, ", ");
                emit_expr(cg, expr->data.tuple_expr.elements[i]);
            }
            emit_string(cg, "}");
            break;
        }

        case AST_EXPR_MEMBER: {
            emit_expr(cg, expr->data.member_expr.object);
            emit_string(cg, ".");
            if (expr->data.member_expr.member[0] >= '0' && expr->data.member_expr.member[0] <= '9') {
                emit_string(cg, "_");
            }
            emit_string(cg, expr->data.member_expr.member);
            break;
        }

        default:
            emit_string(cg, "/* TODO */");
    }
}
