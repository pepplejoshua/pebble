#include "parser.h"
#include "alloc.h"
#include "ast.h"
#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External allocator
extern Arena long_lived;

// ============================================================================
// BASIC PARSER UTILITIES
// ============================================================================

void parser_init(Parser *parser, const char *source, const char *filename) {
  lexer_init(&parser->lexer, source, filename);
  parser->had_error = false;
  parser->panic_mode = false;

  // Prime the parser with the first token
  parser_advance(parser);
}

void parser_advance(Parser *parser) {
  parser->previous = parser->current;

  // Skip any error tokens
  while (true) {
    parser->current = lexer_next_token(&parser->lexer);
    if (parser->current.type != TOKEN_ERROR)
      break;

    parser_error_at_current(parser, parser->current.lexeme);
  }
}

bool parser_check(Parser *parser, TokenType type) {
  return parser->current.type == type;
}

bool parser_match(Parser *parser, TokenType type) {
  if (!parser_check(parser, type))
    return false;
  parser_advance(parser);
  return true;
}

Token parser_consume(Parser *parser, TokenType type, const char *message) {
  if (parser->current.type == type) {
    parser_advance(parser);
    return parser->previous;
  }

  parser_error_at_current(parser, message);
  return parser->current; // Return current token for error recovery
}

// ============================================================================
// ERROR HANDLING
// ============================================================================
static void parser_error_at(Parser *parser, Token *token, const char *message) {
  // Don't report more errors if we're already in panic mode
  if (parser->panic_mode)
    return;
  parser->panic_mode = true;

  fprintf(stderr, "[line %d] Error", token->location.line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Error token - message already printed
  } else {
    fprintf(stderr, " at '%s'", token->lexeme);
  }

  fprintf(stderr, ": %s\n", message);
  parser->had_error = true;
}

void parser_error_at_current(Parser *parser, const char *message) {
  parser_error_at(parser, &parser->current, message);
}

void parser_error_at_previous(Parser *parser, const char *message) {
  parser_error_at(parser, &parser->previous, message);
}

void parser_synchronize(Parser *parser) {
  parser->panic_mode = false;

  while (parser->current.type != TOKEN_EOF) {
    if (parser->previous.type == TOKEN_SEMICOLON)
      return;

    switch (parser->current.type) {
    case TOKEN_FN:
    case TOKEN_VAR:
    case TOKEN_LET:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_LOOP:
    case TOKEN_FOR:
    case TOKEN_RETURN:
      return;
    default:
      break;
    }

    parser_advance(parser);
  }
}

void parser_error(Parser *parser, const char *message) {
  parser_error_at_current(parser, message);
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

BinaryOp token_to_binary_op(TokenType type) {
  switch (type) {
  case TOKEN_PLUS:
    return BINOP_ADD;
  case TOKEN_MINUS:
    return BINOP_SUB;
  case TOKEN_STAR:
    return BINOP_MUL;
  case TOKEN_SLASH:
    return BINOP_DIV;
  case TOKEN_EQ:
    return BINOP_EQ;
  case TOKEN_NE:
    return BINOP_NE;
  case TOKEN_LT:
    return BINOP_LT;
  case TOKEN_LE:
    return BINOP_LE;
  case TOKEN_GT:
    return BINOP_GT;
  case TOKEN_GE:
    return BINOP_GE;
  case TOKEN_AND:
    return BINOP_AND;
  case TOKEN_OR:
    return BINOP_OR;
  default:
    // This should never happen if called correctly
    fprintf(stderr, "Invalid binary operator token: %d\n", type);
    exit(1);
  }
}

UnaryOp token_to_unary_op(TokenType type) {
  switch (type) {
  case TOKEN_MINUS:
    return UNOP_NEG;
  case TOKEN_NOT:
    return UNOP_NOT;
  case TOKEN_AMPERSAND:
    return UNOP_ADDR;
  case TOKEN_STAR:
    return UNOP_DEREF;
  default:
    // This should never happen if called correctly
    fprintf(stderr, "Invalid unary operator token: %d\n", type);
    exit(1);
  }
}

// ============================================================================
// AST NODE CREATION HELPERS
// ============================================================================

// Helper to create basic AST node
static AstNode *alloc_node(AstKind kind, Location loc) {
  AstNode *node = arena_alloc(&long_lived, sizeof(AstNode));
  memset(node, 0, sizeof(AstNode));
  node->kind = kind;
  node->loc = loc;
  return node;
}

// ============================================================================
// TOP-LEVEL PARSING
// ============================================================================

AstNode *parse_program(Parser *parser) {
  // Parse all top-level declarations
  AstNode **decls = arena_alloc(&long_lived, 64 * sizeof(AstNode *));
  size_t decl_count = 0;

  while (!parser_check(parser, TOKEN_EOF)) {
    if (decl_count >= 64) {
      parser_error(parser, "Too many top-level declarations (max 64)");
      break;
    }

    AstNode *decl = parse_declaration(parser);
    if (decl != NULL) {
      decls[decl_count++] = decl;
    }

    // If we had an error and didn't make progress, break to avoid infinite loop
    if (parser->had_error && parser->current.type == parser->previous.type) {
      break;
    }
  }

  // Create a program node to hold all declarations
  Location loc = {.file = parser->lexer.filename, .line = 1, .column = 1};

  AstNode *program = alloc_node(AST_STMT_BLOCK, loc);
  program->data.block_stmt.stmts = decls;
  program->data.block_stmt.stmt_count = decl_count;

  return program;
}

AstNode *parse_declaration(Parser *parser) {
  if (parser_match(parser, TOKEN_FN)) {
    return parse_function_decl(parser);
  }

  if (parser_match(parser, TOKEN_LET) || parser_match(parser, TOKEN_VAR)) {
    return parse_variable_decl(parser);
  }
  if (parser_match(parser, TOKEN_TYPE)) {
    return parse_type_decl(parser);
  }

  parser_error(parser, "Expected declaration");
  parser_synchronize(parser);
  return NULL;
}

AstNode *parse_function_decl(Parser *parser) {
  // fn name(params) return_type { body }
  // fn name(params) return_type => expr

  Token name =
      parser_consume(parser, TOKEN_IDENTIFIER, "Expected function name");

  parser_consume(parser, TOKEN_LPAREN, "Expected '(' after function name");

  // Parse parameters
  FuncParam *params = NULL;
  size_t param_count = 0;

  if (!parser_check(parser, TOKEN_RPAREN)) {
    // We have parameters
    // For now, allocate space for up to 16 parameters (we'll improve this
    // later)
    params = arena_alloc(&long_lived, 16 * sizeof(FuncParam));

    do {
      if (param_count >= 16) {
        parser_error(parser, "Too many parameters (max 16)");
        break;
      }

      Token param_name =
          parser_consume(parser, TOKEN_IDENTIFIER, "Expected parameter name");
      AstNode *param_type = parse_type_expression(parser);

      params[param_count].name =
          param_name.lexeme; // Already allocated by lexer
      params[param_count].type = param_type;
      param_count++;

    } while (parser_match(parser, TOKEN_COMMA));
  }

  parser_consume(parser, TOKEN_RPAREN, "Expected ')' after parameters");

  // Return type
  AstNode *return_type = parse_type_expression(parser);

  // Body: either { ... } or => expr
  AstNode *body = NULL;
  if (parser_match(parser, TOKEN_FAT_ARROW)) {
    // Expression function: fn name(...) type => expr
    AstNode *expr = parse_expression(parser);
    // Wrap the expression in a return statement, then in a block
    AstNode *ret_stmt = alloc_node(AST_STMT_RETURN, expr->loc);
    ret_stmt->data.return_stmt.expr = expr;
    AstNode **stmts = arena_alloc(&long_lived, sizeof(AstNode *));
    stmts[0] = ret_stmt;
    body = alloc_node(AST_STMT_BLOCK, expr->loc);
    body->data.block_stmt.stmts = stmts;
    body->data.block_stmt.stmt_count = 1;
  } else {
    // Regular function: fn name(...) type { ... }
    parser_consume(parser, TOKEN_LBRACE, "Expected '{' before function body");
    body = parse_block_stmt(parser);
  }

  AstNode *func = alloc_node(AST_DECL_FUNCTION, name.location);
  func->data.func_decl.name = str_dup(name.lexeme);
  func->data.func_decl.params = params;
  func->data.func_decl.param_count = param_count;
  func->data.func_decl.return_type = return_type;
  func->data.func_decl.body = body;
  return func;
}

AstNode *parse_variable_decl(Parser *parser) {
  // let name = expr; or let name type = expr;
  // var name = expr; or var name type = expr;

  bool is_mutable = (parser->previous.type == TOKEN_VAR);

  Token name =
      parser_consume(parser, TOKEN_IDENTIFIER, "Expected variable name");

  // Optional type annotation
  AstNode *type_expr = NULL;
  if (!parser_check(parser, TOKEN_EQUAL) &&
      !parser_check(parser, TOKEN_SEMICOLON)) {
    type_expr = parse_type_expression(parser);
  }

  // Initializer
  AstNode *init = NULL;
  if (parser_match(parser, TOKEN_EQUAL)) {
    init = parse_expression(parser);
  }

  parser_consume(parser, TOKEN_SEMICOLON,
                 "Expected ';' after variable declaration");

  // Create appropriate AST node
  if (is_mutable) {
    AstNode *var = alloc_node(AST_DECL_VARIABLE, name.location);
    var->data.var_decl.name = str_dup(name.lexeme);
    var->data.var_decl.type_expr = type_expr;
    var->data.var_decl.init = init;
    return var;
  } else {
    AstNode *const_var = alloc_node(AST_DECL_CONSTANT, name.location);
    const_var->data.const_decl.name = str_dup(name.lexeme);
    const_var->data.const_decl.type_expr = type_expr;
    const_var->data.const_decl.value = init;
    return const_var;
  }
}

AstNode *parse_type_decl(Parser *parser) {
  // type Name = TypeExpr;

  Token name = parser_consume(parser, TOKEN_IDENTIFIER, "Expected type name");
  parser_consume(parser, TOKEN_EQUAL, "Expected '=' after type name");
  AstNode *type_expr = parse_type_expression(parser);
  parser_consume(parser, TOKEN_SEMICOLON,
                 "Expected ';' after type declaration");

  // Create AST node (we'll define ast_type_decl if it doesn't exist)
  AstNode *node = arena_alloc(&long_lived, sizeof(AstNode));
  node->kind = AST_DECL_TYPE;
  node->loc = name.location;
  node->data.type_decl.name = str_dup(name.lexeme);
  node->data.type_decl.type_expr = type_expr;

  return node;
}

AstNode *parse_print_stmt(Parser *parser) {
  // print expr;

  Location loc = parser->previous.location;
  AstNode *expr = parse_expression(parser);
  parser_consume(parser, TOKEN_SEMICOLON,
                 "Expected ';' after print statement.");

  AstNode *node = arena_alloc(&long_lived, sizeof(AstNode));
  node->kind = AST_STMT_PRINT;
  node->loc = loc;
  node->data.print_stmt.expr = expr;

  return node;
}

AstNode *parse_break_continue_stmt(Parser *parser) {
  // break;
  // continue;

  Location loc = parser->previous.location;
  bool is_break = parser->previous.type == TOKEN_BREAK;
  parser_consume(parser, TOKEN_SEMICOLON,
                 "Expected ';' after control flow jump statement.");

  AstNode *node = arena_alloc(&long_lived, sizeof(AstNode));

  node->kind = is_break ? AST_STMT_BREAK : AST_STMT_CONTINUE;
  node->loc = loc;

  return node;
}

// ============================================================================
// STATEMENT PARSING
// ============================================================================

AstNode *parse_statement(Parser *parser) {
  if (parser_match(parser, TOKEN_RETURN)) {
    return parse_return_stmt(parser);
  }
  if (parser_match(parser, TOKEN_IF)) {
    return parse_if_stmt(parser);
  }
  if (parser_match(parser, TOKEN_WHILE)) {
    return parse_while_stmt(parser);
  }
  if (parser_match(parser, TOKEN_LOOP)) {
    return parse_loop_stmt(parser);
  }
  if (parser_match(parser, TOKEN_FOR)) {
    return parse_for_stmt(parser);
  }
  if (parser_match(parser, TOKEN_LBRACE)) {
    return parse_block_stmt(parser);
  }
  if (parser_match(parser, TOKEN_LET) || parser_match(parser, TOKEN_VAR)) {
    return parse_variable_decl(parser); // Local variables
  }
  if (parser_match(parser, TOKEN_PRINT)) {
    return parse_print_stmt(parser);
  }
  if (parser_match(parser, TOKEN_BREAK) ||
      parser_match(parser, TOKEN_CONTINUE)) {
    return parse_break_continue_stmt(parser);
  }

  return parse_assignment_stmt(parser);
}

AstNode *parse_return_stmt(Parser *parser) {
  Location loc = parser->previous.location;

  AstNode *expr = NULL;
  if (!parser_check(parser, TOKEN_SEMICOLON)) {
    expr = parse_expression(parser);
  }

  parser_consume(parser, TOKEN_SEMICOLON,
                 "Expected ';' after return statement");

  AstNode *stmt = alloc_node(AST_STMT_RETURN, loc);
  stmt->data.return_stmt.expr = expr;
  return stmt;
}

AstNode *parse_if_stmt(Parser *parser) {
  Location loc = parser->previous.location;

  AstNode *cond = parse_expression(parser);
  AstNode *then_branch = parse_statement(parser);

  AstNode *else_branch = NULL;
  if (parser_match(parser, TOKEN_ELSE)) {
    else_branch = parse_statement(parser);
  }

  AstNode *stmt = alloc_node(AST_STMT_IF, loc);
  stmt->data.if_stmt.cond = cond;
  stmt->data.if_stmt.then_branch = then_branch;
  stmt->data.if_stmt.else_branch = else_branch;
  return stmt;
}

AstNode *parse_while_stmt(Parser *parser) {
  Location loc = parser->previous.location;

  AstNode *cond = parse_expression(parser);
  AstNode *body = parse_statement(parser);

  AstNode *stmt = alloc_node(AST_STMT_WHILE, loc);
  stmt->data.while_stmt.cond = cond;
  stmt->data.while_stmt.body = body;
  return stmt;
}

AstNode *parse_loop_stmt(Parser *parser) {
  Location loc = parser->previous.location;

  // Parse start expression
  AstNode *start = parse_expression(parser);

  // Expect .. or ..=
  bool inclusive = false;
  if (parser_match(parser, TOKEN_DOTDOTEQ)) {
    inclusive = true;
  } else if (parser_match(parser, TOKEN_DOTDOT)) {
    inclusive = false;
  } else {
    parser_error(parser, "Expected '..' or '..=' in loop range");
    return NULL;
  }

  // Parse end expression
  AstNode *end = parse_expression(parser);

  // Parse loop body
  AstNode *body = parse_statement(parser);

  AstNode *stmt = alloc_node(AST_STMT_LOOP, loc);
  stmt->data.loop_stmt.start = start;
  stmt->data.loop_stmt.end = end;
  stmt->data.loop_stmt.inclusive = inclusive;
  stmt->data.loop_stmt.body = body;
  return stmt;
}

AstNode *parse_for_stmt(Parser *parser) {
  Location loc = parser->previous.location;

  // Parse init (can be variable declaration or assignment)
  AstNode *init;
  if (parser_match(parser, TOKEN_VAR)) {
    init = parse_variable_decl(parser);
  } else {
    // Parse assignment without semicolon
    AstNode *init_lhs = parse_expression(parser);
    parser_consume(parser, TOKEN_EQUAL, "Expected '=' in for loop init");
    AstNode *init_rhs = parse_expression(parser);
    parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after for loop init");

    init = alloc_node(AST_STMT_ASSIGN, init_lhs->loc);
    init->data.assign_stmt.lhs = init_lhs;
    init->data.assign_stmt.rhs = init_rhs;
  }

  // Parse condition
  AstNode *cond = parse_expression(parser);
  parser_consume(parser, TOKEN_SEMICOLON,
                 "Expected ';' after for loop condition");

  // Parse update (assignment without semicolon)
  AstNode *lhs = parse_expression(parser);
  parser_consume(parser, TOKEN_EQUAL, "Expected '=' in for loop update");
  AstNode *rhs = parse_expression(parser);

  AstNode *update = alloc_node(AST_STMT_ASSIGN, lhs->loc);
  update->data.assign_stmt.lhs = lhs;
  update->data.assign_stmt.rhs = rhs;

  // Parse body
  AstNode *body = parse_statement(parser);

  AstNode *stmt = alloc_node(AST_STMT_FOR, loc);
  stmt->data.for_stmt.init = init;
  stmt->data.for_stmt.cond = cond;
  stmt->data.for_stmt.update = update;
  stmt->data.for_stmt.body = body;
  return stmt;
}

AstNode *parse_block_stmt(Parser *parser) {
  Location loc = parser->previous.location;

  // Parse statements until we hit }
  AstNode **stmts = arena_alloc(
      &long_lived, 64 * sizeof(AstNode *)); // Max 64 statements for now
  size_t stmt_count = 0;

  while (!parser_check(parser, TOKEN_RBRACE) &&
         !parser_check(parser, TOKEN_EOF)) {
    if (stmt_count >= 64) {
      parser_error(parser, "Too many statements in block (max 64)");
      break;
    }

    AstNode *stmt = parse_statement(parser);
    if (stmt != NULL) { // Add NULL check
      stmts[stmt_count++] = stmt;
    }
  }

  parser_consume(parser, TOKEN_RBRACE, "Expected '}' after block");

  AstNode *block = alloc_node(AST_STMT_BLOCK, loc);
  block->data.block_stmt.stmts = stmts;
  block->data.block_stmt.stmt_count = stmt_count;
  return block;
}

AstNode *parse_assignment_stmt(Parser *parser) {
  // We know current token is identifier
  AstNode *lhs = parse_expression(parser); // This will parse the identifier

  if (parser_match(parser, TOKEN_EQUAL)) {
    Location loc = parser->previous.location;
    AstNode *rhs = parse_expression(parser);
    parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after assignment");

    AstNode *assign = alloc_node(AST_STMT_ASSIGN, loc);
    assign->data.assign_stmt.lhs = lhs;
    assign->data.assign_stmt.rhs = rhs;
    return assign;
  } else {
    // Not an assignment, it's an expression statement
    parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after expression");

    AstNode *expr_stmt = alloc_node(AST_STMT_EXPR, lhs->loc);
    expr_stmt->data.expr_stmt.expr = lhs;
    return expr_stmt;
  }
}

// ============================================================================
// EXPRESSION PARSING
// ============================================================================

AstNode *parse_expression(Parser *parser) { return parse_or_expr(parser); }

AstNode *parse_or_expr(Parser *parser) {
  AstNode *left = parse_and_expr(parser);

  while (parser_match(parser, TOKEN_OR)) {
    Location loc = parser->previous.location;
    AstNode *right = parse_and_expr(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, loc);
    binop->data.binop.op = BINOP_OR;
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_and_expr(Parser *parser) {
  AstNode *left = parse_equality(parser);

  while (parser_match(parser, TOKEN_AND)) {
    Location loc = parser->previous.location;
    AstNode *right = parse_equality(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, loc);
    binop->data.binop.op = BINOP_AND;
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_equality(Parser *parser) {
  AstNode *left = parse_comparison(parser);

  while (parser_match(parser, TOKEN_EQ) || parser_match(parser, TOKEN_NE)) {
    Token op = parser->previous;
    AstNode *right = parse_comparison(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = token_to_binary_op(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_comparison(Parser *parser) {
  AstNode *left = parse_term(parser);

  while (parser_match(parser, TOKEN_LT) || parser_match(parser, TOKEN_LE) ||
         parser_match(parser, TOKEN_GT) || parser_match(parser, TOKEN_GE)) {
    Token op = parser->previous;
    AstNode *right = parse_term(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = token_to_binary_op(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_term(Parser *parser) {
  AstNode *left = parse_factor(parser);

  while (parser_match(parser, TOKEN_PLUS) ||
         parser_match(parser, TOKEN_MINUS)) {
    Token op = parser->previous;
    AstNode *right = parse_factor(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = token_to_binary_op(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_factor(Parser *parser) {
  AstNode *left = parse_unary(parser);

  while (parser_match(parser, TOKEN_STAR) ||
         parser_match(parser, TOKEN_SLASH)) {
    Token op = parser->previous;
    AstNode *right = parse_unary(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = token_to_binary_op(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_unary(Parser *parser) {
  if (parser_match(parser, TOKEN_MINUS) || parser_match(parser, TOKEN_NOT) ||
      parser_match(parser, TOKEN_AMPERSAND) ||
      parser_match(parser, TOKEN_STAR)) {
    Token op = parser->previous;
    AstNode *operand =
        parse_unary(parser); // Right-associative (allow chaining)

    AstNode *unop = alloc_node(AST_EXPR_UNARY_OP, op.location);
    unop->data.unop.op = token_to_unary_op(op.type);
    unop->data.unop.operand = operand;
    return unop;
  }

  return parse_postfix(parser);
}

AstNode *parse_postfix(Parser *parser) {
  AstNode *expr = parse_primary(parser);

  while (true) {
    if (parser_match(parser, TOKEN_LPAREN)) {
      expr = parse_call(parser, expr);
    } else if (parser_match(parser, TOKEN_LBRACKET)) {
      expr = parse_index(parser, expr);
    } else if (parser_match(parser, TOKEN_DOT)) {
      // Check for struct literal: IDENTIFIER.{ ... }
      if (parser_check(parser, TOKEN_LBRACE) &&
          expr->kind == AST_EXPR_IDENTIFIER) {
        parser_advance(parser); // consume '{'
        Location loc = expr->loc;
        char *type_name = expr->data.ident.name;

        // Parse struct literal fields
        char **field_names = NULL;
        AstNode **field_values = NULL;
        size_t count = 0;
        size_t capacity = 4;

        field_names = arena_alloc(&long_lived, capacity * sizeof(char *));
        field_values = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

        // Allow empty struct literal
        if (!parser_check(parser, TOKEN_RBRACE)) {
          do {
            // Grow arrays if needed
            if (count >= capacity) {
              capacity *= 2;
              char **new_names =
                  arena_alloc(&long_lived, capacity * sizeof(char *));
              AstNode **new_values =
                  arena_alloc(&long_lived, capacity * sizeof(AstNode *));
              memcpy(new_names, field_names, count * sizeof(char *));
              memcpy(new_values, field_values, count * sizeof(AstNode *));
              field_names = new_names;
              field_values = new_values;
            }

            // Parse field: IDENTIFIER = EXPR
            if (!parser_check(parser, TOKEN_IDENTIFIER)) {
              parser_error(parser, "Expected field name in struct literal");
              return NULL;
            }
            parser_advance(parser);
            Token field_name = parser->previous;

            parser_consume(parser, TOKEN_EQUAL,
                           "Expected '=' after field name");

            field_names[count] = str_dup(field_name.lexeme);
            field_values[count] = parse_expression(parser);
            if (!field_values[count]) {
              return NULL;
            }
            count++;

          } while (parser_match(parser, TOKEN_COMMA));
        }

        parser_consume(parser, TOKEN_RBRACE,
                       "Expected '}' after struct literal fields");

        expr = alloc_node(AST_EXPR_STRUCT_LITERAL, loc);
        expr->data.struct_literal.type_name = type_name;
        expr->data.struct_literal.field_names = field_names;
        expr->data.struct_literal.field_values = field_values;
        expr->data.struct_literal.field_count = count;
        return expr;
      } else {
        // Regular member access
        expr = parse_member(parser, expr);
      }
    } else {
      break;
    }
  }

  return expr;
}

AstNode *parse_call(Parser *parser, AstNode *func) {
  Location loc = parser->previous.location;

  // Parse arguments
  AstNode **args = NULL;
  size_t arg_count = 0;

  if (!parser_check(parser, TOKEN_RPAREN)) {
    args = arena_alloc(&long_lived, 16 * sizeof(AstNode *)); // Max 16 args

    do {
      if (arg_count >= 16) {
        parser_error(parser, "Too many arguments (max 16)");
        break;
      }
      args[arg_count++] = parse_expression(parser);
    } while (parser_match(parser, TOKEN_COMMA));
  }

  parser_consume(parser, TOKEN_RPAREN, "Expected ')' after arguments");

  AstNode *call = alloc_node(AST_EXPR_CALL, loc);
  call->data.call.func = func;
  call->data.call.args = args;
  call->data.call.arg_count = arg_count;
  return call;
}

AstNode *parse_index(Parser *parser, AstNode *array) {
  Location loc = parser->previous.location;

  // Check for slice syntax: arr[start:end]
  AstNode *start = NULL;
  AstNode *end = NULL;

  // Parse start (or empty for [:end])
  if (!parser_check(parser, TOKEN_COLON)) {
    start = parse_expression(parser);
  }

  // Check for colon (slice) vs close bracket (index)
  if (parser_match(parser, TOKEN_COLON)) {
    // It's a slice: arr[start:end]

    // Parse end (or empty for [start:])
    if (!parser_check(parser, TOKEN_RBRACKET)) {
      end = parse_expression(parser);
    }

    parser_consume(parser, TOKEN_RBRACKET, "Expected ']' after slice");

    AstNode *slice = alloc_node(AST_EXPR_SLICE, loc);
    slice->data.slice_expr.array = array;
    slice->data.slice_expr.start = start;
    slice->data.slice_expr.end = end;
    return slice;
  } else {
    // It's an index: arr[expr]
    parser_consume(parser, TOKEN_RBRACKET, "Expected ']' after index");

    AstNode *index = alloc_node(AST_EXPR_INDEX, loc);
    index->data.index_expr.array = array;
    index->data.index_expr.index = start; // start is the index expression
    return index;
  }
}

AstNode *parse_member(Parser *parser, AstNode *object) {
  Location loc = parser->previous.location;

  AstNode *mem = alloc_node(AST_EXPR_MEMBER, loc);
  mem->data.member_expr.object = object;

  // Accept either identifier (named member) or int (tuple index)
  if (parser_check(parser, TOKEN_INT)) {
    Token idx = parser_consume(parser, TOKEN_INT,
                               "Expected member name or index after '.'");
    // Convert int to string
    char buf[32];
    snprintf(buf, sizeof(buf), "%d", (int)idx.value.int_val);
    mem->data.member_expr.member = str_dup(buf);
  } else {
    Token member = parser_consume(parser, TOKEN_IDENTIFIER,
                                  "Expected member name or index after '.'");
    mem->data.member_expr.member = str_dup(member.lexeme);
  }

  return mem;
}

AstNode *parse_primary(Parser *parser) {
  // Literals
  if (parser_match(parser, TOKEN_INT)) {
    Token num = parser->previous;
    AstNode *lit = alloc_node(AST_EXPR_LITERAL_INT, num.location);
    lit->data.int_lit.value = num.value.int_val;
    return lit;
  }

  if (parser_match(parser, TOKEN_FLOAT)) {
    Token num = parser->previous;
    AstNode *lit = alloc_node(AST_EXPR_LITERAL_FLOAT, num.location);
    lit->data.float_lit.value = num.value.float_val;
    return lit;
  }

  if (parser_match(parser, TOKEN_STRING)) {
    Token str = parser->previous;
    AstNode *lit = alloc_node(AST_EXPR_LITERAL_STRING, str.location);
    lit->data.str_lit.value = str.value.str_val; // Already duplicated by lexer
    return lit;
  }

  if (parser_match(parser, TOKEN_CHAR)) {
    Token char_tok = parser->previous;
    AstNode *lit = alloc_node(AST_EXPR_LITERAL_CHAR, char_tok.location);
    lit->data.char_lit.value = char_tok.value.char_val;
    return lit;
  }

  if (parser_match(parser, TOKEN_TRUE) || parser_match(parser, TOKEN_FALSE)) {
    Token bool_tok = parser->previous;
    AstNode *lit = alloc_node(AST_EXPR_LITERAL_BOOL, bool_tok.location);
    lit->data.bool_lit.value = bool_tok.value.bool_val;
    return lit;
  }

  // Identifier
  if (parser_match(parser, TOKEN_IDENTIFIER)) {
    Token ident = parser->previous;
    AstNode *id = alloc_node(AST_EXPR_IDENTIFIER, ident.location);
    id->data.ident.name = str_dup(ident.lexeme);
    return id;
  }

  // Grouping / Tuple literal
  if (parser_match(parser, TOKEN_LPAREN)) {
    Location loc = parser->previous.location;

    // Empty tuple/parens not allowed
    if (parser_match(parser, TOKEN_RPAREN)) {
      parser_error(parser, "empty parentheses not allowed");
      return NULL;
    }

    // Parse first expression
    AstNode **elements = NULL;
    size_t count = 0;
    size_t capacity = 4;
    elements = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

    elements[count++] = parse_expression(parser);

    // Check for comma (distinguishes tuple from grouped expression)
    if (parser_match(parser, TOKEN_COMMA)) {
      // It's a tuple literal! Parse remaining elements
      do {
        // Grow array if needed
        if (count >= capacity) {
          capacity *= 2;
          AstNode **new_array =
              arena_alloc(&long_lived, capacity * sizeof(AstNode *));
          memcpy(new_array, elements, count * sizeof(AstNode *));
          elements = new_array;
        }

        elements[count++] = parse_expression(parser);
      } while (parser_match(parser, TOKEN_COMMA));

      parser_consume(parser, TOKEN_RPAREN, "Expected ')' after tuple literal");

      AstNode *tuple = alloc_node(AST_EXPR_TUPLE, loc);
      tuple->data.tuple_expr.elements = elements;
      tuple->data.tuple_expr.element_count = count;
      return tuple;
    } else {
      // No comma - just a grouped expression, return the inner expression
      parser_consume(parser, TOKEN_RPAREN, "Expected ')' after expression");
      return elements[0];
    }
  }

  // Array literal [1, 2, 3] or array repeat [value; count]
  if (parser_match(parser, TOKEN_LBRACKET)) {
    Location loc = parser->previous.location;

    // Check for empty array literal
    if (parser_check(parser, TOKEN_RBRACKET)) {
      parser_advance(parser);
      AstNode *array_lit = alloc_node(AST_EXPR_ARRAY_LITERAL, loc);
      array_lit->data.array_literal.elements = NULL;
      array_lit->data.array_literal.element_count = 0;
      return array_lit;
    }

    // Parse first expression
    AstNode *first_expr = parse_expression(parser);

    // Check if it's array repeat syntax: [value; count]
    if (parser_match(parser, TOKEN_SEMICOLON)) {
      // Expect integer literal for count
      if (!parser_check(parser, TOKEN_INT)) {
        parser_error(parser, "Expected integer literal for array repeat count");
        return NULL;
      }

      long long repeat_count = parser->current.value.int_val;
      if (repeat_count <= 0) {
        parser_error(parser, "Array repeat count must be positive");
        return NULL;
      }

      parser_advance(parser);
      parser_consume(parser, TOKEN_RBRACKET,
                     "Expected ']' after array repeat count");

      AstNode *array_repeat = alloc_node(AST_EXPR_ARRAY_REPEAT, loc);
      array_repeat->data.array_repeat.value = first_expr;
      array_repeat->data.array_repeat.count = (size_t)repeat_count;
      return array_repeat;
    }

    // Otherwise it's an array literal
    AstNode **elements = NULL;
    size_t count = 0;
    size_t capacity = 4;
    elements = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

    elements[count++] = first_expr;

    // Parse remaining elements
    while (parser_match(parser, TOKEN_COMMA)) {
      // Grow array if needed
      if (count >= capacity) {
        capacity *= 2;
        AstNode **new_array =
            arena_alloc(&long_lived, capacity * sizeof(AstNode *));
        memcpy(new_array, elements, count * sizeof(AstNode *));
        elements = new_array;
      }

      elements[count++] = parse_expression(parser);
    }

    parser_consume(parser, TOKEN_RBRACKET, "Expected ']' after array elements");

    AstNode *array_lit = alloc_node(AST_EXPR_ARRAY_LITERAL, loc);
    array_lit->data.array_literal.elements = elements;
    array_lit->data.array_literal.element_count = count;
    return array_lit;
  }

  // Sizeof expression (sizeof type)
  if (parser_match(parser, TOKEN_SIZEOF)) {
    Location loc = parser->previous.location;
    AstNode *type_ast = parse_type_expression(parser);

    AstNode *sizeof_expr = alloc_node(AST_EXPR_SIZEOF, loc);
    sizeof_expr->data.sizeof_expr.type_expr = type_ast;
    return sizeof_expr;
  }

  parser_error(parser, "Expected expression");
  return NULL;
}

// ============================================================================
// TYPE EXPRESSION PARSING
// ============================================================================

AstNode *parse_type_expression(Parser *parser) {
  AstNode *type = NULL;

  // Function type: fn(T1, T2, ...) ReturnType
  if (parser_match(parser, TOKEN_FN)) {
    Location loc = parser->previous.location;

    parser_consume(parser, TOKEN_LPAREN, "Expected '(' after 'fn'");

    // Parse parameter types
    AstNode **param_types = NULL;
    size_t count = 0;
    size_t capacity = 4;
    param_types = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

    // Allow empty parameter list
    if (!parser_check(parser, TOKEN_RPAREN)) {
      do {
        // Grow array if needed
        if (count >= capacity) {
          capacity *= 2;
          AstNode **new_array =
              arena_alloc(&long_lived, capacity * sizeof(AstNode *));
          memcpy(new_array, param_types, count * sizeof(AstNode *));
          param_types = new_array;
        }

        param_types[count++] = parse_type_expression(parser);
      } while (parser_match(parser, TOKEN_COMMA));
    }

    parser_consume(parser, TOKEN_RPAREN, "Expected ')' after parameter types");

    // Parse return type
    AstNode *return_type = parse_type_expression(parser);
    if (!return_type) {
      parser_error(parser, "Expected return type after function parameters");
      return NULL;
    }

    type = alloc_node(AST_TYPE_FUNCTION, loc);
    type->data.type_function.param_types = param_types;
    type->data.type_function.param_count = count;
    type->data.type_function.return_type = return_type;
    return type;
  }

  // Tuple type: (T1, T2, ...)
  if (parser_match(parser, TOKEN_LPAREN)) {
    Location loc = parser->previous.location;

    // Empty tuple not allowed
    if (parser_match(parser, TOKEN_RPAREN)) {
      parser_error(parser, "empty tuple type not allowed");
      return NULL;
    }

    // Parse first element type
    AstNode **element_types = NULL;
    size_t count = 0;
    size_t capacity = 4;
    element_types = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

    element_types[count++] = parse_type_expression(parser);

    // Check for comma (distinguishes tuple from parenthesized type)
    if (parser_match(parser, TOKEN_COMMA)) {
      // It's a tuple. Parse remaining elements
      do {
        // Grow array if needed
        if (count >= capacity) {
          capacity *= 2;
          AstNode **new_array =
              arena_alloc(&long_lived, capacity * sizeof(AstNode *));
          memcpy(new_array, element_types, count * sizeof(AstNode *));
          element_types = new_array;
        }

        element_types[count++] = parse_type_expression(parser);
      } while (parser_match(parser, TOKEN_COMMA));

      parser_consume(parser, TOKEN_RPAREN, "Expected ')' after tuple type");

      type = alloc_node(AST_TYPE_TUPLE, loc);
      type->data.type_tuple.element_types = element_types;
      type->data.type_tuple.element_count = count;
      return type;
    } else {
      // No comma - just a parenthesized type, return the inner type
      parser_consume(parser, TOKEN_RPAREN, "Expected ')'");
      return element_types[0];
    }
  }

  // Pointer type: *T
  if (parser_match(parser, TOKEN_STAR)) {
    Location loc = parser->previous.location;
    AstNode *base = parse_type_expression(parser); // Recursive for **T, etc.

    type = alloc_node(AST_TYPE_POINTER, loc);
    type->data.type_pointer.base = base;
    return type;
  }

  // Array or slice: [N]T or []T
  if (parser_match(parser, TOKEN_LBRACKET)) {
    Location loc = parser->previous.location;

    // Check for slice []T vs array [N]T
    if (parser_match(parser, TOKEN_RBRACKET)) {
      // Slice: []T
      AstNode *element = parse_type_expression(parser);
      type = alloc_node(AST_TYPE_SLICE, loc);
      type->data.type_slice.element = element;
      return type;
    } else {
      // Array: [N]T
      if (!parser_check(parser, TOKEN_INT)) {
        parser_error(parser, "Expected array size");
        return NULL;
      }
      parser_advance(parser);
      Token size_tok = parser->previous;
      size_t size = (size_t)size_tok.value.int_val;

      parser_consume(parser, TOKEN_RBRACKET, "Expected ']' after array size");
      AstNode *element = parse_type_expression(parser);

      type = alloc_node(AST_TYPE_ARRAY, loc);
      type->data.type_array.element = element;
      type->data.type_array.size = size;
      return type;
    }
  }

  // Struct type: struct { field1 type1, field2 type2, ... }
  if (parser_match(parser, TOKEN_STRUCT)) {
    Location loc = parser->previous.location;

    parser_consume(parser, TOKEN_LBRACE, "Expected '{' after 'struct'");

    // Parse fields
    char **field_names = NULL;
    AstNode **field_types = NULL;
    size_t count = 0;
    size_t capacity = 4;

    field_names = arena_alloc(&long_lived, capacity * sizeof(char *));
    field_types = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

    // Allow empty struct
    if (!parser_check(parser, TOKEN_RBRACE)) {
      do {
        // Grow arrays if needed
        if (count >= capacity) {
          capacity *= 2;
          char **new_names =
              arena_alloc(&long_lived, capacity * sizeof(char *));
          AstNode **new_types =
              arena_alloc(&long_lived, capacity * sizeof(AstNode *));
          memcpy(new_names, field_names, count * sizeof(char *));
          memcpy(new_types, field_types, count * sizeof(AstNode *));
          field_names = new_names;
          field_types = new_types;
        }

        // Parse field: IDENTIFIER TYPE
        if (!parser_check(parser, TOKEN_IDENTIFIER)) {
          parser_error(parser, "Expected field name");
          return NULL;
        }
        parser_advance(parser);
        Token field_name = parser->previous;

        field_names[count] = str_dup(field_name.lexeme);
        field_types[count] = parse_type_expression(parser);
        if (!field_types[count]) {
          return NULL;
        }
        count++;

      } while (parser_match(parser, TOKEN_COMMA));
    }

    parser_consume(parser, TOKEN_RBRACE, "Expected '}' after struct fields");

    type = alloc_node(AST_TYPE_STRUCT, loc);
    type->data.type_struct.field_names = field_names;
    type->data.type_struct.field_types = field_types;
    type->data.type_struct.field_count = count;
    return type;
  }

  // Built-in types
  if (parser_match(parser, TOKEN_INT_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("int");
    return type;
  }

  if (parser_match(parser, TOKEN_FLOAT_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("float");
    return type;
  }

  if (parser_match(parser, TOKEN_BOOL_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("bool");
    return type;
  }

  if (parser_match(parser, TOKEN_STR_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("str");
    return type;
  }

  if (parser_match(parser, TOKEN_VOID_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("void");
    return type;
  }

  if (parser_match(parser, TOKEN_U8_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("u8");
    return type;
  }

  if (parser_match(parser, TOKEN_U16_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("u16");
    return type;
  }

  if (parser_match(parser, TOKEN_U32_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("u32");
    return type;
  }

  if (parser_match(parser, TOKEN_U64_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("u64");
    return type;
  }

  if (parser_match(parser, TOKEN_USIZE_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("usize");
    return type;
  }

  if (parser_match(parser, TOKEN_I8_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("i8");
    return type;
  }

  if (parser_match(parser, TOKEN_I16_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("i16");
    return type;
  }

  if (parser_match(parser, TOKEN_I32_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("i32");
    return type;
  }

  if (parser_match(parser, TOKEN_I64_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("i64");
    return type;
  }

  if (parser_match(parser, TOKEN_ISIZE_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("isize");
    return type;
  }

  if (parser_match(parser, TOKEN_CHAR_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("char");
    return type;
  }

  if (parser_match(parser, TOKEN_DOUBLE_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("double");
    return type;
  }

  // Custom/named types
  if (parser_match(parser, TOKEN_IDENTIFIER)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup(parser->previous.lexeme);
    return type;
  }

  parser_error(parser, "Expected type");
  return NULL;
}
