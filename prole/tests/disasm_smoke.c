#include "../include/prole.h"

#include <stdio.h>

static int check_tracking_allocator(ProleTrackingAllocator *tracker) {
  if (prole_tracking_allocator_has_leaks(tracker)) {
    prole_tracking_allocator_dump_leaks(tracker, stderr);
    prole_tracking_allocator_discard_records(tracker);
    return 1;
  }

  return 0;
}

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

  ProleType add_params[] = {PROLE_TYPE_I64, PROLE_TYPE_I64};
  uint32_t add_fn =
      prole_module_add_function(module, "add", PROLE_TYPE_I64, add_params, 2);

  ProleFunction *function = &module->functions[main_fn];
  prole_function_emit(function, prole_inst(PROLE_OP_CONST_I64, 8, 0, 0, 20));
  prole_function_emit(function, prole_inst(PROLE_OP_CONST_I64, 9, 0, 0, 22));
  prole_function_emit(function, prole_inst(PROLE_OP_CALL, 10, add_fn, 8, 2));
  prole_function_emit(function, prole_inst(PROLE_OP_PRINT, 10, 0, 0, 0));
  prole_function_emit(function, prole_inst(PROLE_OP_RET_VOID, 0, 0, 0, 0));
  if (function->register_count != 11) {
    fprintf(stderr, "expected main to require 11 registers, got %u\n",
            function->register_count);
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }

  ProleFunction *add = &module->functions[add_fn];
  prole_function_emit(add, prole_inst(PROLE_OP_ADD_I64, 2, 0, 1, 0));
  prole_function_emit(add, prole_inst(PROLE_OP_RET, 2, 0, 0, 0));
  if (add->register_count != 3) {
    fprintf(stderr, "expected add to require 3 registers, got %u\n",
            add->register_count);
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }

  ProleDiagnosticContext diagnostics;
  prole_diagnostics_init(&diagnostics, "<smoke>", NULL);
  if (!prole_validate_runnable_module(module, &diagnostics)) {
    prole_diagnostics_emit_all(&diagnostics);
    prole_diagnostics_free(&diagnostics);
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }
  prole_diagnostics_free(&diagnostics);

  ProleDiagnosticContext invalid_diagnostics;
  ProleInst saved_call = function->code[2];
  function->code[2].imm = 1;
  prole_diagnostics_init(&invalid_diagnostics, "<invalid-smoke>", NULL);
  if (prole_validate_module(module, &invalid_diagnostics)) {
    fprintf(stderr, "expected validation to reject wrong call arity\n");
    prole_diagnostics_free(&invalid_diagnostics);
    function->code[2] = saved_call;
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }
  prole_diagnostics_free(&invalid_diagnostics);
  function->code[2] = saved_call;

  ProleDisasmOptions options;
  prole_disasm_options_default(&options);
  prole_disassemble(module, stdout, &options);

  ProleVm *call_vm = prole_vm_new(&allocator);
  if (!call_vm || !prole_vm_load_module(call_vm, module)) {
    fprintf(stderr, "failed to initialize call VM smoke module\n");
    prole_vm_free(call_vm);
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }

  printf("\n");
  ProleStepResult call_result = prole_vm_run(call_vm);
  if (call_result != PROLE_STEP_RETURNED) {
    fprintf(stderr, "call VM smoke failed: %s\n",
            prole_vm_trap_message(call_vm));
    prole_vm_free(call_vm);
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }
  prole_vm_free(call_vm);

  ProleModule *vm_module = prole_module_new("vm_smoke", &allocator);
  if (!vm_module) {
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }

  uint32_t vm_main =
      prole_module_add_function(vm_module, "main", PROLE_TYPE_VOID, NULL, 0);
  prole_module_set_entry(vm_module, vm_main);
  ProleFunction *vm_function = &vm_module->functions[vm_main];
  prole_function_emit(vm_function, prole_inst(PROLE_OP_CONST_I64, 0, 0, 0, 42));
  prole_function_emit(vm_function, prole_inst(PROLE_OP_CONST_I64, 1, 0, 0, 8));
  prole_function_emit(vm_function, prole_inst(PROLE_OP_SUB_I64, 2, 0, 1, 0));
  uint32_t local0 = prole_function_add_local(vm_function, PROLE_TYPE_I64);
  prole_function_emit(vm_function,
                      prole_inst(PROLE_OP_STORE_LOCAL, local0, 2, 0, 0));
  prole_function_emit(vm_function,
                      prole_inst(PROLE_OP_LOAD_LOCAL, 2, local0, 0, 0));
  prole_function_emit(vm_function, prole_inst(PROLE_OP_CONST_I64, 3, 0, 0, 12));
  prole_function_emit(vm_function, prole_inst(PROLE_OP_GT_I64, 4, 2, 3, 0));
  prole_function_emit(vm_function, prole_inst(PROLE_OP_PRINT, 2, 0, 0, 0));
  prole_function_emit(vm_function, prole_inst(PROLE_OP_PRINT, 4, 0, 0, 0));
  prole_function_emit(vm_function, prole_inst(PROLE_OP_RET_VOID, 0, 0, 0, 0));

  prole_diagnostics_init(&diagnostics, "<vm-smoke>", NULL);
  if (!prole_validate_runnable_module(vm_module, &diagnostics)) {
    prole_diagnostics_emit_all(&diagnostics);
    prole_diagnostics_free(&diagnostics);
    prole_module_free(vm_module);
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }
  prole_diagnostics_free(&diagnostics);

  ProleVm *vm = prole_vm_new(&allocator);
  if (!vm || !prole_vm_load_module(vm, vm_module)) {
    fprintf(stderr, "failed to initialize VM smoke module\n");
    prole_vm_free(vm);
    prole_module_free(vm_module);
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }

  printf("\n");
  ProleStepResult result = prole_vm_run(vm);
  if (result != PROLE_STEP_RETURNED) {
    fprintf(stderr, "VM smoke failed: %s\n", prole_vm_trap_message(vm));
    prole_vm_free(vm);
    prole_module_free(vm_module);
    prole_module_free(module);
    prole_tracking_allocator_discard_records(&tracker);
    return 1;
  }

  prole_vm_free(vm);
  prole_module_free(vm_module);
  prole_module_free(module);
  return check_tracking_allocator(&tracker);
}
