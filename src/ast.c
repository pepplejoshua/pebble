#include "ast.h"
#include "alloc.h"

AstNode *clone_ast_node(AstNode *node) {
  if (!node)
    return NULL;

  AstNode *clone = arena_alloc(&long_lived, sizeof(AstNode));
  clone->kind = node->kind;
  clone->loc = node->loc;
  clone->resolved_type = node->resolved_type;

  switch (node->kind) {
  // Declarations
  case AST_DECL_TYPE: {
    clone->data.type_decl.name = node->data.type_decl.name;
    clone->data.type_decl.qualified_name = node->data.type_decl.qualified_name;
    clone->data.type_decl.full_qualified_name =
        node->data.type_decl.full_qualified_name;
    clone->data.type_decl.type_expr =
        clone_ast_node(node->data.type_decl.type_expr);
    // Shallow copy
    clone->data.type_decl.type_params = node->data.type_decl.type_params;
    clone->data.type_decl.type_params_count =
        node->data.type_decl.type_params_count;
    break;
  }

  case AST_DECL_VARIABLE: {
    clone->data.var_decl.name = node->data.var_decl.name;
    clone->data.var_decl.qualified_name = node->data.var_decl.qualified_name;
    clone->data.var_decl.full_qualified_name =
        node->data.var_decl.full_qualified_name;
    clone->data.var_decl.type_expr =
        clone_ast_node(node->data.var_decl.type_expr);
    clone->data.var_decl.init = clone_ast_node(node->data.var_decl.init);
    break;
  }

  case AST_DECL_CONSTANT: {
    clone->data.const_decl.name = node->data.const_decl.name;
    clone->data.const_decl.qualified_name =
        node->data.const_decl.qualified_name;
    clone->data.const_decl.full_qualified_name =
        node->data.const_decl.full_qualified_name;
    clone->data.const_decl.type_expr =
        clone_ast_node(node->data.const_decl.type_expr);
    clone->data.const_decl.value = clone_ast_node(node->data.const_decl.value);
    break;
  }

  case AST_DECL_FUNCTION: {
    clone->data.func_decl.inlined = node->data.func_decl.inlined;
    clone->data.func_decl.convention =
        clone_ast_node(node->data.func_decl.convention);
    clone->data.func_decl.name = node->data.func_decl.name;
    clone->data.func_decl.qualified_name = node->data.func_decl.qualified_name;
    clone->data.func_decl.full_qualified_name =
        node->data.func_decl.full_qualified_name;
    clone->data.func_decl.param_count = node->data.func_decl.param_count;
    clone->data.func_decl.return_type =
        clone_ast_node(node->data.func_decl.return_type);
    clone->data.func_decl.body = clone_ast_node(node->data.func_decl.body);
    clone->data.func_decl.type_param_count =
        node->data.func_decl.type_param_count;
    clone->data.func_decl.type_params =
        node->data.func_decl.type_params; // Shallow copy

    // Clone params array (FuncParam structs)
    if (node->data.func_decl.param_count > 0) {
      clone->data.func_decl.params = arena_alloc(
          &long_lived, sizeof(FuncParam) * node->data.func_decl.param_count);
      for (size_t i = 0; i < node->data.func_decl.param_count; i++) {
        clone->data.func_decl.params[i].name =
            node->data.func_decl.params[i].name;
        clone->data.func_decl.params[i].type =
            clone_ast_node(node->data.func_decl.params[i].type);
        clone->data.func_decl.params[i].is_variadic =
            node->data.func_decl.params[i].is_variadic;
      }
    } else {
      clone->data.func_decl.params = NULL;
    }
    break;
  }

  // Statements
  case AST_STMT_RETURN: {
    clone->data.return_stmt.expr = clone_ast_node(node->data.return_stmt.expr);
    break;
  }
  case AST_STMT_IF: {
    clone->data.if_stmt.cond = clone_ast_node(node->data.if_stmt.cond);
    clone->data.if_stmt.then_branch =
        clone_ast_node(node->data.if_stmt.then_branch);
    clone->data.if_stmt.else_branch =
        clone_ast_node(node->data.if_stmt.else_branch);
    break;
  }
  case AST_STMT_WHILE: {
    clone->data.while_stmt.cond = clone_ast_node(node->data.while_stmt.cond);
    clone->data.while_stmt.body = clone_ast_node(node->data.while_stmt.body);
    break;
  }
  case AST_STMT_LOOP: {
    clone->data.loop_stmt.start = clone_ast_node(node->data.loop_stmt.start);
    clone->data.loop_stmt.end = clone_ast_node(node->data.loop_stmt.end);
    clone->data.loop_stmt.inclusive = node->data.loop_stmt.inclusive;
    clone->data.loop_stmt.iterator_name =
        clone_ast_node(node->data.loop_stmt.iterator_name);
    clone->data.loop_stmt.body = clone_ast_node(node->data.loop_stmt.body);
    break;
  }
  case AST_STMT_FOR: {
    clone->data.for_stmt.init = clone_ast_node(node->data.for_stmt.init);
    clone->data.for_stmt.cond = clone_ast_node(node->data.for_stmt.cond);
    clone->data.for_stmt.update = clone_ast_node(node->data.for_stmt.update);
    clone->data.for_stmt.body = clone_ast_node(node->data.for_stmt.body);
    break;
  }
  case AST_STMT_BLOCK: {
    clone->data.block_stmt.stmt_count = node->data.block_stmt.stmt_count;

    if (node->data.block_stmt.stmt_count > 0) {
      clone->data.block_stmt.stmts = arena_alloc(
          &long_lived, sizeof(AstNode *) * node->data.block_stmt.stmt_count);
      for (size_t i = 0; i < node->data.block_stmt.stmt_count; i++) {
        clone->data.block_stmt.stmts[i] =
            clone_ast_node(node->data.block_stmt.stmts[i]);
      }
    }
    break;
  }
  case AST_STMT_EXPR: {
    clone->data.expr_stmt.expr = clone_ast_node(node->data.expr_stmt.expr);
    break;
  }
  case AST_STMT_ASSIGN: {
    clone->data.assign_stmt.op = node->data.assign_stmt.op;
    clone->data.assign_stmt.lhs = clone_ast_node(node->data.assign_stmt.lhs);
    clone->data.assign_stmt.rhs = clone_ast_node(node->data.assign_stmt.rhs);
    break;
  }
  case AST_STMT_PRINT: {
    clone->data.print_stmt.expr_count = node->data.print_stmt.expr_count;

    if (node->data.print_stmt.expr_count > 0) {
      clone->data.print_stmt.exprs = arena_alloc(
          &long_lived, sizeof(AstNode *) * node->data.print_stmt.expr_count);
      for (size_t i = 0; i < node->data.print_stmt.expr_count; i++) {
        clone->data.print_stmt.exprs[i] =
            clone_ast_node(node->data.print_stmt.exprs[i]);
      }
    }
    break;
  }
  case AST_STMT_BREAK:
  case AST_STMT_CONTINUE:
    // No additional data
    break;
  case AST_STMT_CASE: {
    clone->data.case_stmt.switch_stmt =
        node->data.case_stmt.switch_stmt; // Don't clone, just reference
    clone->data.case_stmt.condition =
        clone_ast_node(node->data.case_stmt.condition);
    clone->data.case_stmt.body = clone_ast_node(node->data.case_stmt.body);
    break;
  }
  case AST_STMT_SWITCH: {
    clone->data.switch_stmt.condition =
        clone_ast_node(node->data.switch_stmt.condition);
    clone->data.switch_stmt.case_count = node->data.switch_stmt.case_count;
    clone->data.switch_stmt.default_case =
        clone_ast_node(node->data.switch_stmt.default_case);

    if (node->data.switch_stmt.case_count > 0) {
      clone->data.switch_stmt.cases = arena_alloc(
          &long_lived, sizeof(AstNode *) * node->data.switch_stmt.case_count);
      for (size_t i = 0; i < node->data.switch_stmt.case_count; i++) {
        clone->data.switch_stmt.cases[i] =
            clone_ast_node(node->data.switch_stmt.cases[i]);
      }
    }
    break;
  }
  case AST_STMT_DEFER: {
    clone->data.defer_stmt.stmt = clone_ast_node(node->data.defer_stmt.stmt);
    break;
  }

  // Expressions
  case AST_EXPR_LITERAL_NIL:
  case AST_EXPR_LITERAL_NONE:
  case AST_EXPR_CONTEXT:
    // No additional data
    break;
  case AST_EXPR_LITERAL_INT: {
    clone->data.int_lit.value = node->data.int_lit.value;
    break;
  }
  case AST_EXPR_LITERAL_FLOAT: {
    clone->data.float_lit.value = node->data.float_lit.value;
    break;
  }
  case AST_EXPR_LITERAL_STRING: {
    clone->data.str_lit.value = node->data.str_lit.value;
    break;
  }
  case AST_EXPR_INTERPOLATED_STRING: {
    clone->data.interpolated_string.num_parts =
        node->data.interpolated_string.num_parts;

    if (node->data.interpolated_string.num_parts > 0) {
      clone->data.interpolated_string.parts = arena_alloc(
          &long_lived,
          sizeof(AstNode *) * node->data.interpolated_string.num_parts);
      for (size_t i = 0; i < node->data.interpolated_string.num_parts; i++) {
        clone->data.interpolated_string.parts[i] =
            clone_ast_node(node->data.interpolated_string.parts[i]);
      }
    }
    break;
  }
  case AST_EXPR_LITERAL_CHAR: {
    clone->data.char_lit.value = node->data.char_lit.value;
    break;
  }
  case AST_EXPR_LITERAL_BOOL: {
    clone->data.bool_lit.value = node->data.bool_lit.value;
    break;
  }
  case AST_EXPR_IDENTIFIER: {
    clone->data.ident.name = node->data.ident.name;
    clone->data.ident.qualified_name = node->data.ident.qualified_name;
    clone->data.ident.full_qualified_name =
        node->data.ident.full_qualified_name;
    clone->data.ident.is_extern = node->data.ident.is_extern;
    break;
  }
  case AST_EXPR_BINARY_OP: {
    clone->data.binop.op = node->data.binop.op;
    clone->data.binop.left = clone_ast_node(node->data.binop.left);
    clone->data.binop.right = clone_ast_node(node->data.binop.right);
    break;
  }
  case AST_EXPR_UNARY_OP: {
    clone->data.unop.op = node->data.unop.op;
    clone->data.unop.operand = clone_ast_node(node->data.unop.operand);
    break;
  }
  case AST_EXPR_CALL: {
    clone->data.call.func = clone_ast_node(node->data.call.func);
    clone->data.call.arg_count = node->data.call.arg_count;

    if (node->data.call.arg_count > 0) {
      clone->data.call.args = arena_alloc(
          &long_lived, sizeof(AstNode *) * node->data.call.arg_count);
      for (size_t i = 0; i < node->data.call.arg_count; i++) {
        clone->data.call.args[i] = clone_ast_node(node->data.call.args[i]);
      }
    }
    if (node->data.call.type_args) {
      size_t count = node->data.call.type_arg_count;
      clone->data.call.type_arg_count = count;
      clone->data.call.type_args =
          arena_alloc(&long_lived, sizeof(AstNode *) * count);
      for (size_t i = 0; i < count; i++) {
        clone->data.call.type_args[i] =
            clone_ast_node(node->data.call.type_args[i]);
      }
    }
    break;
  }
  case AST_EXPR_INDEX: {
    clone->data.index_expr.array = clone_ast_node(node->data.index_expr.array);
    clone->data.index_expr.index = clone_ast_node(node->data.index_expr.index);
    break;
  }
  case AST_EXPR_SLICE: {
    clone->data.slice_expr.array = clone_ast_node(node->data.slice_expr.array);
    clone->data.slice_expr.start = clone_ast_node(node->data.slice_expr.start);
    clone->data.slice_expr.end = clone_ast_node(node->data.slice_expr.end);
    break;
  }
  case AST_EXPR_MEMBER: {
    clone->data.member_expr.object =
        clone_ast_node(node->data.member_expr.object);
    clone->data.member_expr.member = node->data.member_expr.member;
    break;
  }
  case AST_EXPR_MODULE_MEMBER: {
    clone->data.mod_member_expr.module =
        clone_ast_node(node->data.mod_member_expr.module);
    clone->data.mod_member_expr.qualified_path =
        node->data.mod_member_expr.qualified_path;
    clone->data.mod_member_expr.member = node->data.mod_member_expr.member;
    clone->data.mod_member_expr.is_extern =
        node->data.mod_member_expr.is_extern;
    break;
  }
  case AST_EXPR_PARTIAL_MEMBER: {
    clone->data.partial_member_expr.member =
        node->data.partial_member_expr.member;
    break;
  }
  case AST_EXPR_TUPLE: {
    clone->data.tuple_expr.element_count = node->data.tuple_expr.element_count;

    if (node->data.tuple_expr.element_count > 0) {
      clone->data.tuple_expr.elements = arena_alloc(
          &long_lived, sizeof(AstNode *) * node->data.tuple_expr.element_count);
      for (size_t i = 0; i < node->data.tuple_expr.element_count; i++) {
        clone->data.tuple_expr.elements[i] =
            clone_ast_node(node->data.tuple_expr.elements[i]);
      }
    }
    break;
  }
  case AST_EXPR_STRUCT_LITERAL: {
    clone->data.struct_literal.type_name = node->data.struct_literal.type_name;
    clone->data.struct_literal.qualified_type_name =
        node->data.struct_literal.qualified_type_name;
    clone->data.struct_literal.field_count =
        node->data.struct_literal.field_count;

    if (node->data.struct_literal.field_count > 0) {
      clone->data.struct_literal.field_names = arena_alloc(
          &long_lived, sizeof(char *) * node->data.struct_literal.field_count);
      clone->data.struct_literal.field_values =
          arena_alloc(&long_lived, sizeof(AstNode *) *
                                       node->data.struct_literal.field_count);

      for (size_t i = 0; i < node->data.struct_literal.field_count; i++) {
        clone->data.struct_literal.field_names[i] =
            node->data.struct_literal.field_names[i];
        clone->data.struct_literal.field_values[i] =
            clone_ast_node(node->data.struct_literal.field_values[i]);
      }
    }
    if (node->data.struct_literal.type_args) {
      size_t count = node->data.struct_literal.type_arg_count;
      clone->data.struct_literal.type_arg_count = count;
      clone->data.struct_literal.type_args =
          arena_alloc(&long_lived, sizeof(AstNode *) * count);
      for (size_t i = 0; i < count; i++) {
        clone->data.struct_literal.type_args[i] =
            clone_ast_node(node->data.struct_literal.type_args[i]);
      }
    }
    break;
  }
  case AST_EXPR_ARRAY_LITERAL: {
    clone->data.array_literal.element_count =
        node->data.array_literal.element_count;

    if (node->data.array_literal.element_count > 0) {
      clone->data.array_literal.elements =
          arena_alloc(&long_lived, sizeof(AstNode *) *
                                       node->data.array_literal.element_count);
      for (size_t i = 0; i < node->data.array_literal.element_count; i++) {
        clone->data.array_literal.elements[i] =
            clone_ast_node(node->data.array_literal.elements[i]);
      }
    }
    break;
  }
  case AST_EXPR_ARRAY_REPEAT: {
    clone->data.array_repeat.value =
        clone_ast_node(node->data.array_repeat.value);
    clone->data.array_repeat.count = node->data.array_repeat.count;
    break;
  }

  case AST_EXPR_FUNCTION: {
    clone->data.func_expr.inlined = node->data.func_expr.inlined;
    clone->data.func_expr.convention =
        clone_ast_node(node->data.func_expr.convention);
    clone->data.func_expr.param_count = node->data.func_expr.param_count;
    clone->data.func_expr.return_type =
        clone_ast_node(node->data.func_expr.return_type);
    clone->data.func_expr.body = clone_ast_node(node->data.func_expr.body);
    clone->data.func_expr.symbol = node->data.func_expr.symbol;
    clone->data.func_expr.type_param_count =
        node->data.func_expr.type_param_count;
    clone->data.func_expr.type_params =
        node->data.func_expr.type_params; // Shallow copy

    // Clone params array
    if (node->data.func_expr.param_count > 0) {
      clone->data.func_expr.params = arena_alloc(
          &long_lived, sizeof(FuncParam) * node->data.func_expr.param_count);
      for (size_t i = 0; i < node->data.func_expr.param_count; i++) {
        clone->data.func_expr.params[i].name =
            node->data.func_expr.params[i].name;
        clone->data.func_expr.params[i].type =
            clone_ast_node(node->data.func_expr.params[i].type);
        clone->data.func_expr.params[i].is_variadic =
            node->data.func_expr.params[i].is_variadic;
      }
    }
    break;
  }
  case AST_EXPR_IMPLICIT_CAST: {
    clone->data.implicit_cast.expr =
        clone_ast_node(node->data.implicit_cast.expr);
    clone->data.implicit_cast.target_type =
        node->data.implicit_cast.target_type;
    break;
  }
  case AST_EXPR_SIZEOF: {
    clone->data.sizeof_expr.type_expr =
        clone_ast_node(node->data.sizeof_expr.type_expr);
    break;
  }
  case AST_EXPR_EXPLICIT_CAST: {
    clone->data.explicit_cast.expr =
        clone_ast_node(node->data.explicit_cast.expr);
    clone->data.explicit_cast.target_type =
        clone_ast_node(node->data.explicit_cast.target_type);
    clone->data.explicit_cast.pointer_cast =
        node->data.explicit_cast.pointer_cast;
    break;
  }
  case AST_EXPR_GROUPED_EXPR: {
    clone->data.grouped_expr.inner_expr =
        clone_ast_node(node->data.grouped_expr.inner_expr);
    break;
  }
  case AST_EXPR_SOME: {
    clone->data.some_expr.value = clone_ast_node(node->data.some_expr.value);
    break;
  }
  case AST_EXPR_FORCE_UNWRAP: {
    clone->data.force_unwrap.operand =
        clone_ast_node(node->data.force_unwrap.operand);
    break;
  }
  case AST_EXPR_POSTFIX_INC: {
    clone->data.postfix_inc.operand =
        clone_ast_node(node->data.postfix_inc.operand);
    break;
  }
  case AST_EXPR_POSTFIX_DEC: {
    clone->data.postfix_dec.operand =
        clone_ast_node(node->data.postfix_dec.operand);
    break;
  }

  // Type expressionss
  case AST_TYPE_NAMED: {
    clone->data.type_named.name = node->data.type_named.name;
    if (node->data.type_named.type_args) {
      size_t count = node->data.type_named.type_arg_count;
      clone->data.type_named.type_arg_count = count;
      clone->data.type_named.type_args =
          arena_alloc(&long_lived, sizeof(AstNode *) * count);
      for (size_t i = 0; i < count; i++) {
        clone->data.type_named.type_args[i] =
            clone_ast_node(node->data.type_named.type_args[i]);
      }
    }
    break;
  }
  case AST_TYPE_QUALIFIED_NAMED: {
    clone->data.type_qualified_named.mod_name =
        node->data.type_qualified_named.mod_name;
    clone->data.type_qualified_named.mem_name =
        node->data.type_qualified_named.mem_name;
    if (node->data.type_qualified_named.type_args) {
      size_t count = node->data.type_qualified_named.type_arg_count;
      clone->data.type_qualified_named.type_arg_count = count;
      clone->data.type_qualified_named.type_args =
          arena_alloc(&long_lived, sizeof(AstNode *) * count);
      for (size_t i = 0; i < count; i++) {
        clone->data.type_qualified_named.type_args[i] =
            clone_ast_node(node->data.type_qualified_named.type_args[i]);
      }
    }
    break;
  }
  case AST_TYPE_POINTER: {
    clone->data.type_pointer.base =
        clone_ast_node(node->data.type_pointer.base);
    break;
  }
  case AST_TYPE_OPTIONAL: {
    clone->data.type_optional.base =
        clone_ast_node(node->data.type_optional.base);
    break;
  }
  case AST_TYPE_ARRAY: {
    clone->data.type_array.element =
        clone_ast_node(node->data.type_array.element);
    clone->data.type_array.size = node->data.type_array.size;
    break;
  }
  case AST_TYPE_SLICE: {
    clone->data.type_slice.element =
        clone_ast_node(node->data.type_slice.element);
    break;
  }
  case AST_TYPE_STRUCT: {
    clone->data.type_struct.field_count = node->data.type_struct.field_count;
    // Clone field names array
    if (node->data.type_struct.field_count > 0) {
      clone->data.type_struct.field_names = arena_alloc(
          &long_lived, sizeof(char *) * node->data.type_struct.field_count);
      for (size_t i = 0; i < node->data.type_struct.field_count; i++) {
        clone->data.type_struct.field_names[i] =
            node->data.type_struct.field_names[i];
      }

      // Clone field types array
      clone->data.type_struct.field_types = arena_alloc(
          &long_lived, sizeof(AstNode *) * node->data.type_struct.field_count);
      for (size_t i = 0; i < node->data.type_struct.field_count; i++) {
        clone->data.type_struct.field_types[i] =
            clone_ast_node(node->data.type_struct.field_types[i]);
      }
    }
    break;
  }
  case AST_TYPE_FUNCTION: {
    clone->data.type_function.convention =
        clone_ast_node(node->data.type_function.convention);
    clone->data.type_function.param_count =
        node->data.type_function.param_count;
    clone->data.type_function.return_type =
        clone_ast_node(node->data.type_function.return_type);

    // Clone param types array
    if (node->data.type_function.param_count > 0) {
      clone->data.type_function.param_types =
          arena_alloc(&long_lived,
                      sizeof(AstNode *) * node->data.type_function.param_count);
      for (size_t i = 0; i < node->data.type_function.param_count; i++) {
        clone->data.type_function.param_types[i] =
            clone_ast_node(node->data.type_function.param_types[i]);
      }
    }
    break;
  }
  case AST_TYPE_TUPLE: {
    clone->data.type_tuple.element_count = node->data.type_tuple.element_count;

    // Clone element types array
    if (node->data.type_tuple.element_count > 0) {
      clone->data.type_tuple.element_types = arena_alloc(
          &long_lived, sizeof(AstNode *) * node->data.type_tuple.element_count);
      for (size_t i = 0; i < node->data.type_tuple.element_count; i++) {
        clone->data.type_tuple.element_types[i] =
            clone_ast_node(node->data.type_tuple.element_types[i]);
      }
    }
    break;
  }
  case AST_TYPE_ENUM: {
    clone->data.type_enum.variant_count = node->data.type_enum.variant_count;

    // Clone variant names array
    if (node->data.type_enum.variant_count > 0) {
      clone->data.type_enum.variant_names = arena_alloc(
          &long_lived, sizeof(char *) * node->data.type_enum.variant_count);
      for (size_t i = 0; i < node->data.type_enum.variant_count; i++) {
        clone->data.type_enum.variant_names[i] =
            node->data.type_enum.variant_names[i];
      }
    }
    break;
  }
  case AST_TYPE_UNION: {
    clone->data.type_union.is_tagged = node->data.type_union.is_tagged;
    clone->data.type_union.variant_count = node->data.type_union.variant_count;

    // Clone variant names array
    if (node->data.type_union.variant_count > 0) {
      clone->data.type_union.variant_names = arena_alloc(
          &long_lived, sizeof(char *) * node->data.type_union.variant_count);
      for (size_t i = 0; i < node->data.type_union.variant_count; i++) {
        clone->data.type_union.variant_names[i] =
            node->data.type_union.variant_names[i];
      }

      // Clone variant types array
      clone->data.type_union.variant_types = arena_alloc(
          &long_lived, sizeof(AstNode *) * node->data.type_union.variant_count);
      for (size_t i = 0; i < node->data.type_union.variant_count; i++) {
        clone->data.type_union.variant_types[i] =
            clone_ast_node(node->data.type_union.variant_types[i]);
      }
    }
    break;
  }
  default:
    break;
  }

  return clone;
}
