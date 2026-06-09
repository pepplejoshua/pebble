#include "../include/prole.h"

#include <stdio.h>

int main(void) {
  ProleModule *module = prole_module_new("smoke", NULL);
  if (!module) {
    return 1;
  }

  uint32_t main_fn =
      prole_module_add_function(module, "main", PROLE_TYPE_VOID, NULL, 0);
  prole_module_set_entry(module, main_fn);

  ProleFunction *function = &module->functions[main_fn];
  prole_function_emit(function, prole_inst(PROLE_OP_CONST_I64, 0, 0, 0, 42));
  prole_function_emit(function, prole_inst(PROLE_OP_PRINT, 0, 0, 0, 0));
  prole_function_emit(function, prole_inst(PROLE_OP_RET_VOID, 0, 0, 0, 0));

  ProleDisasmOptions options;
  prole_disasm_options_default(&options);
  prole_disassemble(module, stdout, &options);

  prole_module_free(module);
  return 0;
}
