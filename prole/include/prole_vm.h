#ifndef PROLE_VM_H
#define PROLE_VM_H

#include "prole_allocator.h"
#include "prole_bytecode.h"
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

typedef struct {
  ProleType type;
  union {
    int64_t i64;
    bool boolean;
  } as;
} ProleValue;

typedef enum {
  PROLE_STEP_OK,
  PROLE_STEP_RETURNED,
  PROLE_STEP_TRAP,
} ProleStepResult;

typedef struct {
  const ProleFunction *function;
  size_t ip;
  ProleValue *registers;
  size_t register_count;
  ProleValue *locals;
  size_t local_count;
  uint32_t return_register;
} ProleFrame;

typedef struct {
  ProleAllocator allocator;
  const ProleModule *module;
  ProleFrame *frames;
  size_t frame_count;
  size_t frame_capacity;
  bool loaded;
  bool returned;
  ProleValue result;
  const char *trap_message;
} ProleVm;

ProleValue prole_value_void(void);
ProleValue prole_value_i64(int64_t value);
ProleValue prole_value_bool(bool value);

ProleVm *prole_vm_new(ProleAllocator *allocator);
void prole_vm_free(ProleVm *vm);

bool prole_vm_load_module(ProleVm *vm, const ProleModule *module);
ProleStepResult prole_vm_step(ProleVm *vm);
ProleStepResult prole_vm_run(ProleVm *vm);

const ProleFunction *prole_vm_current_function(const ProleVm *vm);
size_t prole_vm_ip(const ProleVm *vm);
ProleValue prole_vm_read_register(const ProleVm *vm, uint32_t reg);
const char *prole_vm_trap_message(const ProleVm *vm);

#endif
