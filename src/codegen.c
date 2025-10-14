#include "codegen.h"
#include <stdio.h>

void codegen_init(Codegen *cg, FILE *output) {
    cg->output = output;
    cg->indent_level = 0;
}

void emit_line(Codegen *cg, const char *line) {
    for (int i = 0; i < cg->indent_level; i++) {
        fprintf(cg->output, "    ");
    }
    fprintf(cg->output, "%s\n", line);
}

void emit_string(Codegen *cg, const char *str) {
    fprintf(cg->output, "%s", str);
}

void emit_indent(Codegen *cg) {
    cg->indent_level++;
}

void emit_dedent(Codegen *cg) {
    cg->indent_level--;
}

void emit_program(Codegen *cg, AstNode *program) {}

void emit_type_name(Codegen *cg, Type *type) {}
