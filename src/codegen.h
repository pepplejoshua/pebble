#ifndef CODEGEN_H
#define CODEGEN_H

#include "ast.h"
#include "type.h"
#include <stdio.h>

typedef struct {
    FILE *output;
    int indent_level;
} Codegen;

// Core functions
void codegen_init(Codegen *cg, FILE *output);
void emit_program(Codegen *cg, AstNode *program);

// Emission helpers
void emit_line(Codegen *cg, const char *line);
void emit_string(Codegen *cg, const char *str);
void emit_indent(Codegen *cg);
void emit_type_name(Codegen *cg, Type *type);

#endif
