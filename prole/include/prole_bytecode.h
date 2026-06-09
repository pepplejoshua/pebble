#ifndef PROLE_BYTECODE_H
#define PROLE_BYTECODE_H

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

typedef enum {
  PROLE_TYPE_VOID,
  PROLE_TYPE_I64,
  PROLE_TYPE_BOOL,
} ProleType;

typedef enum {
  PROLE_OP_NOP,
  PROLE_OP_CONST_I64,
  PROLE_OP_CONST_BOOL,
  PROLE_OP_LOAD_LOCAL,
  PROLE_OP_STORE_LOCAL,
  PROLE_OP_ADD_I64,
  PROLE_OP_SUB_I64,
  PROLE_OP_MUL_I64,
  PROLE_OP_DIV_I64,
  PROLE_OP_MOD_I64,
  PROLE_OP_EQ_I64,
  PROLE_OP_NE_I64,
  PROLE_OP_LT_I64,
  PROLE_OP_LE_I64,
  PROLE_OP_GT_I64,
  PROLE_OP_GE_I64,
  PROLE_OP_JUMP,
  PROLE_OP_JUMP_IF_FALSE,
  PROLE_OP_CALL,
  PROLE_OP_CALL_NATIVE,
  PROLE_OP_PRINT,
  PROLE_OP_RET,
  PROLE_OP_RET_VOID,
} ProleOp;

typedef struct {
  ProleOp op;
  uint32_t a;
  uint32_t b;
  uint32_t c;
  int64_t imm;
} ProleInst;

typedef struct {
  char *name;
} ProleNative;

typedef struct {
  char *name;
  ProleType return_type;
  ProleType *param_types;
  size_t param_count;
  ProleType *local_types;
  size_t local_count;
  ProleInst *code;
  size_t code_count;
  size_t code_capacity;
} ProleFunction;

typedef struct {
  char *name;
  ProleFunction *functions;
  size_t function_count;
  size_t function_capacity;
  ProleNative *natives;
  size_t native_count;
  size_t native_capacity;
  uint32_t entry_function;
  bool has_entry;
} ProleModule;

const char *prole_type_name(ProleType type);
const char *prole_op_name(ProleOp op);

ProleModule *prole_module_new(const char *name);
void prole_module_free(ProleModule *module);

uint32_t prole_module_add_function(ProleModule *module, const char *name,
                                   ProleType return_type,
                                   const ProleType *param_types,
                                   size_t param_count);
uint32_t prole_module_add_native(ProleModule *module, const char *name);
void prole_module_set_entry(ProleModule *module, uint32_t function_index);

uint32_t prole_function_add_local(ProleFunction *function, ProleType type);
void prole_function_emit(ProleFunction *function, ProleInst inst);

ProleInst prole_inst(ProleOp op, uint32_t a, uint32_t b, uint32_t c,
                     int64_t imm);

#endif
