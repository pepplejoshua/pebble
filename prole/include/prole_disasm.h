#ifndef PROLE_DISASM_H
#define PROLE_DISASM_H

#include "prole_bytecode.h"
#include <stdbool.h>
#include <stdio.h>

typedef struct {
  bool color;
  bool show_offsets;
} ProleDisasmOptions;

void prole_disasm_options_default(ProleDisasmOptions *options);
void prole_disassemble(const ProleModule *module, FILE *out,
                       const ProleDisasmOptions *options);

#endif
