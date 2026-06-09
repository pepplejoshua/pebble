#include "../include/prole.h"

#include <stdio.h>

int main(void) {
  ProleTrackingAllocator tracker;
  prole_tracking_allocator_init(&tracker, NULL);
  ProleAllocator allocator = prole_tracking_allocator(&tracker);

  ProleModule *module = prole_module_new("smoke", &allocator);
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
  if (prole_tracking_allocator_has_leaks(&tracker)) {
    prole_tracking_allocator_dump_leaks(&tracker, stderr);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }

  return 0;
}
