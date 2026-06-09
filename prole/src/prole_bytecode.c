#include "../include/prole_bytecode.h"

#include <string.h>

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
    return "jif";
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

ProleModule *prole_module_new(const char *name, ProleAllocator *allocator) {
  ProleAllocator module_allocator =
      allocator ? *allocator : prole_malloc_allocator();
  ProleModule *module = prole_alloc(&module_allocator, sizeof(ProleModule));
  if (!module) {
    return NULL;
  }
  memset(module, 0, sizeof(ProleModule));

  module->allocator = module_allocator;
  module->name = prole_strdup(&module->allocator, name ? name : "main");
  return module;
}

void prole_module_free(ProleModule *module) {
  if (!module) {
    return;
  }

  ProleAllocator allocator = module->allocator;
  prole_free(&allocator, module->name,
             module->name ? strlen(module->name) + 1 : 0);

  for (size_t i = 0; i < module->function_count; i++) {
    ProleFunction *function = &module->functions[i];
    prole_free(&allocator, function->name,
               function->name ? strlen(function->name) + 1 : 0);
    prole_free(&allocator, function->param_types,
               function->param_count * sizeof(ProleType));
    prole_free(&allocator, function->local_types,
               function->local_count * sizeof(ProleType));
    prole_free(&allocator, function->code,
               function->code_capacity * sizeof(ProleInst));
  }

  for (size_t i = 0; i < module->native_count; i++) {
    prole_free(&allocator, module->natives[i].name,
               module->natives[i].name ? strlen(module->natives[i].name) + 1
                                       : 0);
  }

  prole_free(&allocator, module->functions,
             module->function_capacity * sizeof(ProleFunction));
  prole_free(&allocator, module->natives,
             module->native_capacity * sizeof(ProleNative));
  prole_free(&allocator, module, sizeof(ProleModule));
}

static bool grow_functions(ProleModule *module) {
  if (module->function_count < module->function_capacity) {
    return true;
  }

  size_t new_capacity = module->function_capacity == 0
                            ? 8
                            : module->function_capacity * 2;
  ProleFunction *functions =
      prole_realloc(&module->allocator, module->functions,
                    module->function_capacity * sizeof(ProleFunction),
                    new_capacity * sizeof(ProleFunction));
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
  function->allocator = module->allocator;
  function->name = prole_strdup(&function->allocator, name);
  function->return_type = return_type;
  function->param_count = param_count;
  function->register_count = (uint32_t)param_count;

  if (param_count > 0) {
    function->param_types =
        prole_alloc(&function->allocator, param_count * sizeof(ProleType));
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
        prole_realloc(&module->allocator, module->natives,
                      module->native_capacity * sizeof(ProleNative),
                      new_capacity * sizeof(ProleNative));
    if (!natives) {
      return UINT32_MAX;
    }

    module->natives = natives;
    module->native_capacity = new_capacity;
  }

  uint32_t index = (uint32_t)module->native_count++;
  module->natives[index].name = prole_strdup(&module->allocator, name);
  return index;
}

void prole_module_set_entry(ProleModule *module, uint32_t function_index) {
  module->entry_function = function_index;
  module->has_entry = true;
}

uint32_t prole_function_add_local(ProleFunction *function, ProleType type) {
  ProleType *locals =
      prole_realloc(&function->allocator, function->local_types,
                    function->local_count * sizeof(ProleType),
                    (function->local_count + 1) * sizeof(ProleType));
  if (!locals) {
    return UINT32_MAX;
  }

  uint32_t index = (uint32_t)function->local_count;
  function->local_types = locals;
  function->local_types[function->local_count++] = type;
  return index;
}

void prole_function_require_registers(ProleFunction *function, uint32_t count) {
  if (count > function->register_count) {
    function->register_count = count;
  }
}

static void note_register(ProleFunction *function, uint32_t reg) {
  prole_function_require_registers(function, reg + 1);
}

static void note_call_arg_registers(ProleFunction *function, ProleInst inst) {
  if (inst.imm <= 0) {
    return;
  }

  uint64_t last_arg = (uint64_t)inst.c + (uint64_t)inst.imm - 1;
  if (last_arg > UINT32_MAX) {
    prole_function_require_registers(function, UINT32_MAX);
    return;
  }

  note_register(function, (uint32_t)last_arg);
}

static void update_register_count(ProleFunction *function, ProleInst inst) {
  switch (inst.op) {
  case PROLE_OP_NOP:
  case PROLE_OP_JUMP:
  case PROLE_OP_RET_VOID:
    break;
  case PROLE_OP_CONST_I64:
  case PROLE_OP_CONST_BOOL:
  case PROLE_OP_LOAD_LOCAL:
  case PROLE_OP_PRINT:
  case PROLE_OP_RET:
    note_register(function, inst.a);
    break;
  case PROLE_OP_STORE_LOCAL:
    note_register(function, inst.b);
    break;
  case PROLE_OP_ADD_I64:
  case PROLE_OP_SUB_I64:
  case PROLE_OP_MUL_I64:
  case PROLE_OP_DIV_I64:
  case PROLE_OP_MOD_I64:
  case PROLE_OP_EQ_I64:
  case PROLE_OP_NE_I64:
  case PROLE_OP_LT_I64:
  case PROLE_OP_LE_I64:
  case PROLE_OP_GT_I64:
  case PROLE_OP_GE_I64:
    note_register(function, inst.a);
    note_register(function, inst.b);
    note_register(function, inst.c);
    break;
  case PROLE_OP_JUMP_IF_FALSE:
    note_register(function, inst.a);
    break;
  case PROLE_OP_CALL:
  case PROLE_OP_CALL_NATIVE:
    note_register(function, inst.a);
    note_call_arg_registers(function, inst);
    break;
  }
}

void prole_function_emit(ProleFunction *function, ProleInst inst) {
  if (function->code_count >= function->code_capacity) {
    size_t new_capacity =
        function->code_capacity == 0 ? 16 : function->code_capacity * 2;
    ProleInst *code =
        prole_realloc(&function->allocator, function->code,
                      function->code_capacity * sizeof(ProleInst),
                      new_capacity * sizeof(ProleInst));
    if (!code) {
      return;
    }

    function->code = code;
    function->code_capacity = new_capacity;
  }

  function->code[function->code_count++] = inst;
  update_register_count(function, inst);
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
