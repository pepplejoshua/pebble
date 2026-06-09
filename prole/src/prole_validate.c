#include "../include/prole_validate.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>

static void validate_error(ProleDiagnosticContext *diagnostics,
                           const char *fmt, ...) {
  if (!diagnostics) {
    return;
  }

  va_list args;
  va_start(args, fmt);
  char buffer[1024];
  vsnprintf(buffer, sizeof(buffer), fmt, args);
  va_end(args);

  prole_diag_error_no_span(diagnostics, "%s", buffer);
}

static bool reg_in_range(const ProleFunction *function, uint32_t reg) {
  return reg < function->register_count;
}

static bool local_in_range(const ProleFunction *function, uint32_t local) {
  return local < function->local_count;
}

static bool target_in_range(const ProleFunction *function, uint32_t target) {
  return target < function->code_count;
}

static bool call_args_in_range(const ProleFunction *function,
                               const ProleInst *inst) {
  if (inst->imm < 0) {
    return false;
  }

  if (inst->imm == 0) {
    return true;
  }

  uint64_t last_arg = (uint64_t)inst->c + (uint64_t)inst->imm - 1;
  return last_arg < function->register_count;
}

static bool validate_function_inst(const ProleModule *module,
                                   const ProleFunction *function,
                                   size_t function_index, size_t inst_index,
                                   const ProleInst *inst,
                                   ProleDiagnosticContext *diagnostics) {
  bool ok = true;

#define CHECK_REG(reg, role)                                                   \
  do {                                                                         \
    if (!reg_in_range(function, (reg))) {                                       \
      validate_error(diagnostics,                                              \
                     "function '%s' instruction %zu: %s register r%u is "      \
                     "outside register_count %u",                             \
                     function->name ? function->name : "<anon>", inst_index,   \
                     (role), (reg), function->register_count);                 \
      ok = false;                                                              \
    }                                                                          \
  } while (0)

  switch (inst->op) {
  case PROLE_OP_NOP:
  case PROLE_OP_RET_VOID:
    break;
  case PROLE_OP_CONST_I64:
  case PROLE_OP_CONST_BOOL:
  case PROLE_OP_PRINT:
  case PROLE_OP_RET:
    CHECK_REG(inst->a, "operand");
    break;
  case PROLE_OP_LOAD_LOCAL:
    CHECK_REG(inst->a, "destination");
    if (!local_in_range(function, inst->b)) {
      validate_error(diagnostics,
                     "function '%s' instruction %zu: local%u is outside "
                     "local_count %zu",
                     function->name ? function->name : "<anon>", inst_index,
                     inst->b, function->local_count);
      ok = false;
    }
    break;
  case PROLE_OP_STORE_LOCAL:
    CHECK_REG(inst->b, "source");
    if (!local_in_range(function, inst->a)) {
      validate_error(diagnostics,
                     "function '%s' instruction %zu: local%u is outside "
                     "local_count %zu",
                     function->name ? function->name : "<anon>", inst_index,
                     inst->a, function->local_count);
      ok = false;
    }
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
    CHECK_REG(inst->a, "destination");
    CHECK_REG(inst->b, "lhs");
    CHECK_REG(inst->c, "rhs");
    break;
  case PROLE_OP_JUMP:
    if (!target_in_range(function, inst->a)) {
      validate_error(diagnostics,
                     "function '%s' instruction %zu: jump target %u is "
                     "outside code_count %zu",
                     function->name ? function->name : "<anon>", inst_index,
                     inst->a, function->code_count);
      ok = false;
    }
    break;
  case PROLE_OP_JUMP_IF_FALSE:
    CHECK_REG(inst->a, "condition");
    if (!target_in_range(function, inst->b)) {
      validate_error(diagnostics,
                     "function '%s' instruction %zu: jump target %u is "
                     "outside code_count %zu",
                     function->name ? function->name : "<anon>", inst_index,
                     inst->b, function->code_count);
      ok = false;
    }
    break;
  case PROLE_OP_CALL:
    CHECK_REG(inst->a, "destination");
    if (inst->b >= module->function_count) {
      validate_error(diagnostics,
                     "function '%s' instruction %zu: call target fn%u does "
                     "not exist",
                     function->name ? function->name : "<anon>", inst_index,
                     inst->b);
      ok = false;
    } else {
      const ProleFunction *callee = &module->functions[inst->b];
      if ((size_t)inst->imm != callee->param_count) {
        validate_error(diagnostics,
                       "function '%s' instruction %zu: call to '%s' expects "
                       "%zu arg(s), got %lld",
                       function->name ? function->name : "<anon>", inst_index,
                       callee->name ? callee->name : "<anon>",
                       callee->param_count, (long long)inst->imm);
        ok = false;
      }
    }
    if (!call_args_in_range(function, inst)) {
      validate_error(diagnostics,
                     "function '%s' instruction %zu: call arg range starting "
                     "at r%u with count %lld is outside register_count %u",
                     function->name ? function->name : "<anon>", inst_index,
                     inst->c, (long long)inst->imm, function->register_count);
      ok = false;
    }
    break;
  case PROLE_OP_CALL_NATIVE:
    CHECK_REG(inst->a, "destination");
    if (inst->b >= module->native_count) {
      validate_error(diagnostics,
                     "function '%s' instruction %zu: native target native%u "
                     "does not exist",
                     function->name ? function->name : "<anon>", inst_index,
                     inst->b);
      ok = false;
    }
    if (!call_args_in_range(function, inst)) {
      validate_error(diagnostics,
                     "function '%s' instruction %zu: native call arg range "
                     "starting at r%u with count %lld is outside "
                     "register_count %u",
                     function->name ? function->name : "<anon>", inst_index,
                     inst->c, (long long)inst->imm, function->register_count);
      ok = false;
    }
    break;
  }

  if (inst->op == PROLE_OP_RET && function->return_type == PROLE_TYPE_VOID) {
    validate_error(diagnostics,
                   "function '%s' instruction %zu: ret with value in void "
                   "function",
                   function->name ? function->name : "<anon>", inst_index);
    ok = false;
  }

  if (inst->op == PROLE_OP_RET_VOID &&
      function->return_type != PROLE_TYPE_VOID) {
    validate_error(diagnostics,
                   "function '%s' instruction %zu: ret.void in non-void "
                   "function",
                   function->name ? function->name : "<anon>", inst_index);
    ok = false;
  }

#undef CHECK_REG
  (void)function_index;
  return ok;
}

bool prole_validate_module(const ProleModule *module,
                           ProleDiagnosticContext *diagnostics) {
  bool ok = true;

  if (!module) {
    validate_error(diagnostics, "module is null");
    return false;
  }

  if (module->has_entry && module->entry_function >= module->function_count) {
    validate_error(diagnostics, "entry function index %u does not exist",
                   module->entry_function);
    ok = false;
  }

  for (size_t i = 0; i < module->function_count; i++) {
    const ProleFunction *function = &module->functions[i];
    if (function->register_count < function->param_count) {
      validate_error(diagnostics,
                     "function '%s': register_count %u is smaller than arity "
                     "%zu",
                     function->name ? function->name : "<anon>",
                     function->register_count, function->param_count);
      ok = false;
    }

    for (size_t j = 0; j < function->code_count; j++) {
      if (!validate_function_inst(module, function, i, j, &function->code[j],
                                  diagnostics)) {
        ok = false;
      }
    }
  }

  return ok;
}

bool prole_validate_runnable_module(const ProleModule *module,
                                    ProleDiagnosticContext *diagnostics) {
  bool ok = prole_validate_module(module, diagnostics);

  if (!module) {
    return false;
  }

  if (!module->has_entry) {
    validate_error(diagnostics, "runnable module must define an entry function");
    ok = false;
  } else if (module->entry_function >= module->function_count) {
    ok = false;
  }

  return ok;
}
