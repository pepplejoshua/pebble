#include "../include/prole_bytecode.h"

#include <stdlib.h>
#include <string.h>

static char *prole_strdup(const char *value) {
  if (!value) {
    return NULL;
  }

  size_t len = strlen(value);
  char *copy = malloc(len + 1);
  if (!copy) {
    return NULL;
  }

  memcpy(copy, value, len + 1);
  return copy;
}

const char *prole_type_name(ProleType type) {
  switch (type) {
  case PROLE_TYPE_VOID:
    return "void";
  case PROLE_TYPE_I64:
    return "i64";
  case PROLE_TYPE_BOOL:
    return "bool";
  }

  return "unknown";
}

const char *prole_op_name(ProleOp op) {
  switch (op) {
  case PROLE_OP_NOP:
    return "nop";
  case PROLE_OP_CONST_I64:
    return "const.i64";
  case PROLE_OP_CONST_BOOL:
    return "const.bool";
  case PROLE_OP_LOAD_LOCAL:
    return "load.local";
  case PROLE_OP_STORE_LOCAL:
    return "store.local";
  case PROLE_OP_ADD_I64:
    return "add.i64";
  case PROLE_OP_SUB_I64:
    return "sub.i64";
  case PROLE_OP_MUL_I64:
    return "mul.i64";
  case PROLE_OP_DIV_I64:
    return "div.i64";
  case PROLE_OP_MOD_I64:
    return "mod.i64";
  case PROLE_OP_EQ_I64:
    return "eq.i64";
  case PROLE_OP_NE_I64:
    return "ne.i64";
  case PROLE_OP_LT_I64:
    return "lt.i64";
  case PROLE_OP_LE_I64:
    return "le.i64";
  case PROLE_OP_GT_I64:
    return "gt.i64";
  case PROLE_OP_GE_I64:
    return "ge.i64";
  case PROLE_OP_JUMP:
    return "jump";
  case PROLE_OP_JUMP_IF_FALSE:
    return "jif.false";
  case PROLE_OP_CALL:
    return "call";
  case PROLE_OP_CALL_NATIVE:
    return "call.native";
  case PROLE_OP_PRINT:
    return "print";
  case PROLE_OP_RET:
    return "ret";
  case PROLE_OP_RET_VOID:
    return "ret.void";
  }

  return "unknown";
}

ProleModule *prole_module_new(const char *name) {
  ProleModule *module = calloc(1, sizeof(ProleModule));
  if (!module) {
    return NULL;
  }

  module->name = prole_strdup(name ? name : "main");
  return module;
}

void prole_module_free(ProleModule *module) {
  if (!module) {
    return;
  }

  free(module->name);

  for (size_t i = 0; i < module->function_count; i++) {
    ProleFunction *function = &module->functions[i];
    free(function->name);
    free(function->param_types);
    free(function->local_types);
    free(function->code);
  }

  for (size_t i = 0; i < module->native_count; i++) {
    free(module->natives[i].name);
  }

  free(module->functions);
  free(module->natives);
  free(module);
}

static bool grow_functions(ProleModule *module) {
  if (module->function_count < module->function_capacity) {
    return true;
  }

  size_t new_capacity = module->function_capacity == 0
                            ? 8
                            : module->function_capacity * 2;
  ProleFunction *functions =
      realloc(module->functions, new_capacity * sizeof(ProleFunction));
  if (!functions) {
    return false;
  }

  module->functions = functions;
  module->function_capacity = new_capacity;
  return true;
}

uint32_t prole_module_add_function(ProleModule *module, const char *name,
                                   ProleType return_type,
                                   const ProleType *param_types,
                                   size_t param_count) {
  if (!grow_functions(module)) {
    return UINT32_MAX;
  }

  uint32_t index = (uint32_t)module->function_count++;
  ProleFunction *function = &module->functions[index];
  memset(function, 0, sizeof(ProleFunction));
  function->name = prole_strdup(name);
  function->return_type = return_type;
  function->param_count = param_count;

  if (param_count > 0) {
    function->param_types = malloc(param_count * sizeof(ProleType));
    if (!function->param_types) {
      return UINT32_MAX;
    }
    memcpy(function->param_types, param_types, param_count * sizeof(ProleType));
  }

  return index;
}

uint32_t prole_module_add_native(ProleModule *module, const char *name) {
  if (module->native_count >= module->native_capacity) {
    size_t new_capacity =
        module->native_capacity == 0 ? 8 : module->native_capacity * 2;
    ProleNative *natives =
        realloc(module->natives, new_capacity * sizeof(ProleNative));
    if (!natives) {
      return UINT32_MAX;
    }

    module->natives = natives;
    module->native_capacity = new_capacity;
  }

  uint32_t index = (uint32_t)module->native_count++;
  module->natives[index].name = prole_strdup(name);
  return index;
}

void prole_module_set_entry(ProleModule *module, uint32_t function_index) {
  module->entry_function = function_index;
  module->has_entry = true;
}

uint32_t prole_function_add_local(ProleFunction *function, ProleType type) {
  ProleType *locals =
      realloc(function->local_types, (function->local_count + 1) * sizeof(ProleType));
  if (!locals) {
    return UINT32_MAX;
  }

  uint32_t index = (uint32_t)(function->param_count + function->local_count);
  function->local_types = locals;
  function->local_types[function->local_count++] = type;
  return index;
}

void prole_function_emit(ProleFunction *function, ProleInst inst) {
  if (function->code_count >= function->code_capacity) {
    size_t new_capacity =
        function->code_capacity == 0 ? 16 : function->code_capacity * 2;
    ProleInst *code = realloc(function->code, new_capacity * sizeof(ProleInst));
    if (!code) {
      return;
    }

    function->code = code;
    function->code_capacity = new_capacity;
  }

  function->code[function->code_count++] = inst;
}

ProleInst prole_inst(ProleOp op, uint32_t a, uint32_t b, uint32_t c,
                     int64_t imm) {
  ProleInst inst;
  inst.op = op;
  inst.a = a;
  inst.b = b;
  inst.c = c;
  inst.imm = imm;
  return inst;
}
