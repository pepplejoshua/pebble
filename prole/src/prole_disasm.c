#include "../include/prole_disasm.h"
#include "../../pastel/pastel.h"

#include <stdio.h>
#include <string.h>

void prole_disasm_options_default(ProleDisasmOptions *options) {
  options->color = false;
  options->show_offsets = false;
}

static void write_colored_line(FILE *out, const char *line) {
  char formatted[4096];
  pastel_format(line, formatted, sizeof(formatted));
  fputs(formatted, out);
}

static void print_reg(FILE *out, uint32_t reg) { fprintf(out, "r%u", reg); }

static void disassemble_inst(const ProleInst *inst, FILE *out) {
  fputs("  ", out);
  fputs(prole_op_name(inst->op), out);

  switch (inst->op) {
  case PROLE_OP_NOP:
  case PROLE_OP_RET_VOID:
    break;
  case PROLE_OP_CONST_I64:
    fputc(' ', out);
    print_reg(out, inst->a);
    fprintf(out, ", %lld", (long long)inst->imm);
    break;
  case PROLE_OP_CONST_BOOL:
    fputc(' ', out);
    print_reg(out, inst->a);
    fprintf(out, ", %s", inst->imm ? "true" : "false");
    break;
  case PROLE_OP_LOAD_LOCAL:
  case PROLE_OP_STORE_LOCAL:
    fputc(' ', out);
    print_reg(out, inst->a);
    fprintf(out, ", local%u", inst->b);
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
    fputc(' ', out);
    print_reg(out, inst->a);
    fputs(", ", out);
    print_reg(out, inst->b);
    fputs(", ", out);
    print_reg(out, inst->c);
    break;
  case PROLE_OP_JUMP:
    fprintf(out, " .L%u", inst->a);
    break;
  case PROLE_OP_JUMP_IF_FALSE:
    fputc(' ', out);
    print_reg(out, inst->a);
    fprintf(out, ", .L%u", inst->b);
    break;
  case PROLE_OP_CALL:
  case PROLE_OP_CALL_NATIVE:
    fputc(' ', out);
    print_reg(out, inst->a);
    fprintf(out, ", fn%u, argc%u", inst->b, inst->c);
    break;
  case PROLE_OP_PRINT:
  case PROLE_OP_RET:
    fputc(' ', out);
    print_reg(out, inst->a);
    break;
  }

  fputc('\n', out);
}

static void disassemble_function(const ProleFunction *function, size_t index,
                                 FILE *out, const ProleDisasmOptions *options) {
  fprintf(out, "\nfn %s/%zu -> %s\n", function->name ? function->name : "<anon>",
          function->param_count, prole_type_name(function->return_type));

  for (size_t i = 0; i < function->code_count; i++) {
    if (options->show_offsets) {
      fprintf(out, "  ; %04zu\n", i);
    }
    disassemble_inst(&function->code[i], out);
  }

  (void)index;
}

void prole_disassemble(const ProleModule *module, FILE *out,
                       const ProleDisasmOptions *options) {
  ProleDisasmOptions fallback;
  if (!options) {
    prole_disasm_options_default(&fallback);
    options = &fallback;
  }

  if (options->color) {
    char line[1024];
    snprintf(line, sizeof(line), "*[bold,l_cyan]module[/] %s\n",
             module->name ? module->name : "main");
    write_colored_line(out, line);
  } else {
    fprintf(out, "module %s\n", module->name ? module->name : "main");
  }

  if (module->has_entry && module->entry_function < module->function_count) {
    const ProleFunction *entry = &module->functions[module->entry_function];
    if (options->color) {
      char line[1024];
      snprintf(line, sizeof(line), "*[bold,l_cyan]entry[/] %s\n",
               entry->name ? entry->name : "<anon>");
      write_colored_line(out, line);
    } else {
      fprintf(out, "entry %s\n", entry->name ? entry->name : "<anon>");
    }
  }

  for (size_t i = 0; i < module->native_count; i++) {
    fprintf(out, "native %s\n",
            module->natives[i].name ? module->natives[i].name : "<anon>");
  }

  for (size_t i = 0; i < module->function_count; i++) {
    disassemble_function(&module->functions[i], i, out, options);
  }
}
