#include "../include/prole_vm.h"

#include <stdio.h>
#include <string.h>

ProleValue prole_value_void(void) {
  ProleValue value;
  value.type = PROLE_TYPE_VOID;
  value.as.i64 = 0;
  return value;
}

ProleValue prole_value_i64(int64_t value) {
  ProleValue prole_value;
  prole_value.type = PROLE_TYPE_I64;
  prole_value.as.i64 = value;
  return prole_value;
}

ProleValue prole_value_bool(bool value) {
  ProleValue prole_value;
  prole_value.type = PROLE_TYPE_BOOL;
  prole_value.as.boolean = value;
  return prole_value;
}

static ProleStepResult trap(ProleVm *vm, const char *message) {
  vm->trap_message = message;
  return PROLE_STEP_TRAP;
}

static ProleFrame *current_frame(ProleVm *vm);
static const ProleFrame *current_frame_const(const ProleVm *vm);

static bool is_i64(ProleValue value) { return value.type == PROLE_TYPE_I64; }

static ProleStepResult exec_i64_binary(ProleVm *vm, const ProleInst *inst,
                                       int64_t (*op)(int64_t, int64_t)) {
  ProleFrame *frame = current_frame(vm);
  ProleValue lhs = frame->registers[inst->b];
  ProleValue rhs = frame->registers[inst->c];
  if (!is_i64(lhs) || !is_i64(rhs)) {
    return trap(vm, "expected i64 register value");
  }

  frame->registers[inst->a] = prole_value_i64(op(lhs.as.i64, rhs.as.i64));
  return PROLE_STEP_OK;
}

static ProleStepResult exec_i64_compare(ProleVm *vm, const ProleInst *inst,
                                        bool (*op)(int64_t, int64_t)) {
  ProleFrame *frame = current_frame(vm);
  ProleValue lhs = frame->registers[inst->b];
  ProleValue rhs = frame->registers[inst->c];
  if (!is_i64(lhs) || !is_i64(rhs)) {
    return trap(vm, "expected i64 register value");
  }

  frame->registers[inst->a] = prole_value_bool(op(lhs.as.i64, rhs.as.i64));
  return PROLE_STEP_OK;
}

static int64_t add_i64(int64_t lhs, int64_t rhs) { return lhs + rhs; }
static int64_t sub_i64(int64_t lhs, int64_t rhs) { return lhs - rhs; }
static int64_t mul_i64(int64_t lhs, int64_t rhs) { return lhs * rhs; }
static int64_t div_i64(int64_t lhs, int64_t rhs) { return lhs / rhs; }
static int64_t mod_i64(int64_t lhs, int64_t rhs) { return lhs % rhs; }
static bool eq_i64(int64_t lhs, int64_t rhs) { return lhs == rhs; }
static bool ne_i64(int64_t lhs, int64_t rhs) { return lhs != rhs; }
static bool lt_i64(int64_t lhs, int64_t rhs) { return lhs < rhs; }
static bool le_i64(int64_t lhs, int64_t rhs) { return lhs <= rhs; }
static bool gt_i64(int64_t lhs, int64_t rhs) { return lhs > rhs; }
static bool ge_i64(int64_t lhs, int64_t rhs) { return lhs >= rhs; }

static ProleFrame *current_frame(ProleVm *vm) {
  if (!vm || vm->frame_count == 0) {
    return NULL;
  }

  return &vm->frames[vm->frame_count - 1];
}

static const ProleFrame *current_frame_const(const ProleVm *vm) {
  if (!vm || vm->frame_count == 0) {
    return NULL;
  }

  return &vm->frames[vm->frame_count - 1];
}

static void free_frame_registers(ProleVm *vm, ProleFrame *frame) {
  prole_free(&vm->allocator, frame->registers,
             frame->register_count * sizeof(ProleValue));
  prole_free(&vm->allocator, frame->locals,
             frame->local_count * sizeof(ProleValue));
  frame->registers = NULL;
  frame->register_count = 0;
  frame->locals = NULL;
  frame->local_count = 0;
}

static void clear_frames(ProleVm *vm) {
  for (size_t i = 0; i < vm->frame_count; i++) {
    free_frame_registers(vm, &vm->frames[i]);
  }
  vm->frame_count = 0;
}

static bool grow_frames(ProleVm *vm) {
  if (vm->frame_count < vm->frame_capacity) {
    return true;
  }

  size_t new_capacity = vm->frame_capacity == 0 ? 8 : vm->frame_capacity * 2;
  ProleFrame *frames =
      prole_realloc(&vm->allocator, vm->frames,
                    vm->frame_capacity * sizeof(ProleFrame),
                    new_capacity * sizeof(ProleFrame));
  if (!frames) {
    return false;
  }

  vm->frames = frames;
  vm->frame_capacity = new_capacity;
  return true;
}

static ProleValue *alloc_registers(ProleVm *vm, uint32_t register_count) {
  if (register_count == 0) {
    return NULL;
  }

  ProleValue *registers =
      prole_alloc(&vm->allocator, register_count * sizeof(ProleValue));
  if (!registers) {
    return NULL;
  }

  for (uint32_t i = 0; i < register_count; i++) {
    registers[i] = prole_value_void();
  }

  return registers;
}

static ProleValue *alloc_values(ProleVm *vm, size_t count) {
  if (count == 0) {
    return NULL;
  }

  ProleValue *values = prole_alloc(&vm->allocator, count * sizeof(ProleValue));
  if (!values) {
    return NULL;
  }

  for (size_t i = 0; i < count; i++) {
    values[i] = prole_value_void();
  }

  return values;
}

static bool push_frame(ProleVm *vm, const ProleFunction *function,
                       uint32_t return_register) {
  if (!grow_frames(vm)) {
    return false;
  }

  ProleValue *registers = alloc_registers(vm, function->register_count);
  if (function->register_count > 0 && !registers) {
    return false;
  }

  ProleValue *locals = alloc_values(vm, function->local_count);
  if (function->local_count > 0 && !locals) {
    prole_free(&vm->allocator, registers,
               function->register_count * sizeof(ProleValue));
    return false;
  }

  ProleFrame *frame = &vm->frames[vm->frame_count++];
  frame->function = function;
  frame->ip = 0;
  frame->registers = registers;
  frame->register_count = function->register_count;
  frame->locals = locals;
  frame->local_count = function->local_count;
  frame->return_register = return_register;
  return true;
}

static ProleStepResult finish_frame(ProleVm *vm, ProleValue value) {
  ProleFrame *frame = current_frame(vm);
  if (!frame) {
    return trap(vm, "cannot return without a current frame");
  }

  uint32_t return_register = frame->return_register;
  free_frame_registers(vm, frame);
  vm->frame_count--;

  ProleFrame *caller = current_frame(vm);
  if (!caller) {
    vm->result = value;
    vm->returned = true;
    return PROLE_STEP_RETURNED;
  }

  caller->registers[return_register] = value;
  return PROLE_STEP_OK;
}

ProleVm *prole_vm_new(ProleAllocator *allocator) {
  ProleAllocator vm_allocator = allocator ? *allocator : prole_malloc_allocator();
  ProleVm *vm = prole_alloc(&vm_allocator, sizeof(ProleVm));
  if (!vm) {
    return NULL;
  }

  memset(vm, 0, sizeof(ProleVm));
  vm->allocator = vm_allocator;
  vm->result = prole_value_void();
  return vm;
}

void prole_vm_free(ProleVm *vm) {
  if (!vm) {
    return;
  }

  ProleAllocator allocator = vm->allocator;
  clear_frames(vm);
  prole_free(&allocator, vm->frames,
             vm->frame_capacity * sizeof(ProleFrame));
  prole_free(&allocator, vm, sizeof(ProleVm));
}

bool prole_vm_load_module(ProleVm *vm, const ProleModule *module) {
  if (!vm || !module || !module->has_entry ||
      module->entry_function >= module->function_count) {
    return false;
  }

  clear_frames(vm);

  vm->module = module;
  vm->loaded = true;
  vm->returned = false;
  vm->result = prole_value_void();
  vm->trap_message = NULL;

  const ProleFunction *entry = &module->functions[module->entry_function];
  return push_frame(vm, entry, 0);
}

ProleStepResult prole_vm_step(ProleVm *vm) {
  if (!vm || !vm->loaded) {
    return trap(vm, "VM has no loaded module");
  }

  if (vm->returned) {
    return PROLE_STEP_RETURNED;
  }

  ProleFrame *frame = current_frame(vm);
  if (!frame) {
    return trap(vm, "VM has no active frame");
  }

  const ProleFunction *function = frame->function;
  if (frame->ip >= function->code_count) {
    return trap(vm, "instruction pointer is outside function body");
  }

  const ProleInst *inst = &function->code[frame->ip++];
  switch (inst->op) {
  case PROLE_OP_NOP:
    return PROLE_STEP_OK;
  case PROLE_OP_CONST_I64:
    frame->registers[inst->a] = prole_value_i64(inst->imm);
    return PROLE_STEP_OK;
  case PROLE_OP_CONST_BOOL:
    frame->registers[inst->a] = prole_value_bool(inst->imm != 0);
    return PROLE_STEP_OK;
  case PROLE_OP_PRINT: {
    ProleValue value = frame->registers[inst->a];
    switch (value.type) {
    case PROLE_TYPE_VOID:
      printf("void\n");
      break;
    case PROLE_TYPE_I64:
      printf("%lld\n", (long long)value.as.i64);
      break;
    case PROLE_TYPE_BOOL:
      printf("%s\n", value.as.boolean ? "true" : "false");
      break;
    }
    return PROLE_STEP_OK;
  }
  case PROLE_OP_RET:
    return finish_frame(vm, frame->registers[inst->a]);
  case PROLE_OP_RET_VOID:
    return finish_frame(vm, prole_value_void());
  case PROLE_OP_LOAD_LOCAL:
    frame->registers[inst->a] = frame->locals[inst->b];
    return PROLE_STEP_OK;
  case PROLE_OP_STORE_LOCAL:
    frame->locals[inst->a] = frame->registers[inst->b];
    return PROLE_STEP_OK;
  case PROLE_OP_ADD_I64:
    return exec_i64_binary(vm, inst, add_i64);
  case PROLE_OP_SUB_I64:
    return exec_i64_binary(vm, inst, sub_i64);
  case PROLE_OP_MUL_I64:
    return exec_i64_binary(vm, inst, mul_i64);
  case PROLE_OP_DIV_I64:
    if (is_i64(frame->registers[inst->c]) &&
        frame->registers[inst->c].as.i64 == 0) {
      return trap(vm, "division by zero");
    }
    return exec_i64_binary(vm, inst, div_i64);
  case PROLE_OP_MOD_I64:
    if (is_i64(frame->registers[inst->c]) &&
        frame->registers[inst->c].as.i64 == 0) {
      return trap(vm, "modulo by zero");
    }
    return exec_i64_binary(vm, inst, mod_i64);
  case PROLE_OP_EQ_I64:
    return exec_i64_compare(vm, inst, eq_i64);
  case PROLE_OP_NE_I64:
    return exec_i64_compare(vm, inst, ne_i64);
  case PROLE_OP_LT_I64:
    return exec_i64_compare(vm, inst, lt_i64);
  case PROLE_OP_LE_I64:
    return exec_i64_compare(vm, inst, le_i64);
  case PROLE_OP_GT_I64:
    return exec_i64_compare(vm, inst, gt_i64);
  case PROLE_OP_GE_I64:
    return exec_i64_compare(vm, inst, ge_i64);
  case PROLE_OP_JUMP:
  case PROLE_OP_JUMP_IF_FALSE:
  case PROLE_OP_CALL_NATIVE:
    return trap(vm, "opcode is not implemented in VM yet");
  case PROLE_OP_CALL: {
    const ProleFunction *callee = &vm->module->functions[inst->b];
    size_t caller_index = vm->frame_count - 1;
    if (!push_frame(vm, callee, inst->a)) {
      return trap(vm, "failed to allocate call frame");
    }

    ProleFrame *caller = &vm->frames[caller_index];
    ProleFrame *callee_frame = current_frame(vm);
    for (int64_t i = 0; i < inst->imm; i++) {
      callee_frame->registers[i] = caller->registers[inst->c + (uint32_t)i];
    }
    return PROLE_STEP_OK;
  }
  }

  return trap(vm, "unknown opcode");
}

ProleStepResult prole_vm_run(ProleVm *vm) {
  for (;;) {
    ProleStepResult result = prole_vm_step(vm);
    if (result != PROLE_STEP_OK) {
      return result;
    }
  }
}

const ProleFunction *prole_vm_current_function(const ProleVm *vm) {
  const ProleFrame *frame = current_frame_const(vm);
  return frame ? frame->function : NULL;
}

size_t prole_vm_ip(const ProleVm *vm) {
  const ProleFrame *frame = current_frame_const(vm);
  return frame ? frame->ip : 0;
}

ProleValue prole_vm_read_register(const ProleVm *vm, uint32_t reg) {
  const ProleFrame *frame = current_frame_const(vm);
  if (!frame || reg >= frame->register_count) {
    return prole_value_void();
  }

  return frame->registers[reg];
}

const char *prole_vm_trap_message(const ProleVm *vm) {
  return vm ? vm->trap_message : NULL;
}
