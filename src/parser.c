#include "parser.h"
#include "alloc.h"
#include "ast.h"
#include "lexer.h"
#include "module.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External allocator
extern Arena long_lived;

// ============================================================================
// BASIC PARSER UTILITIES
// ============================================================================

void parser_init(Parser *parser, const char *source, const char *filename,
                 const char *abs_file_path) {
  lexer_init(&parser->lexer, source, filename);
  parser->had_error = false;
  parser->panic_mode = false;
  parser->abs_file_path = abs_file_path;

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

  fprintf(stderr, "%s:%d:%d:\nError", parser->abs_file_path,
          token->location.line, token->location.column);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Error token - message already printed
  } else {
    fprintf(stderr, " at '%s'", token->lexeme);
  }

  fprintf(stderr, ": %s\n", message);
  parser->had_error = true;

  parser_synchronize(parser);
}

void parser_error_at_current(Parser *parser, const char *message) {
  parser_error_at(parser, &parser->current, message);
}

void parser_error_at_previous(Parser *parser, const char *message) {
  parser_error_at(parser, &parser->previous, message);
}

void parser_synchronize(Parser *parser) {
  int skips = 0, allowed_skips = 3;

  while (parser->current.type != TOKEN_EOF && skips++ < allowed_skips) {
    if (parser->previous.type == TOKEN_SEMICOLON) {
      parser_advance(parser);
      parser->panic_mode = false;
      return;
    }

    switch (parser->current.type) {
    case TOKEN_FN:
    case TOKEN_VAR:
    case TOKEN_LET:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_LOOP:
    case TOKEN_FOR:
    case TOKEN_RETURN:
    case TOKEN_PRINT:
    case TOKEN_BREAK:
    case TOKEN_CONTINUE:
    case TOKEN_TYPE:
    case TOKEN_EXTERN:
    case TOKEN_COMMA:
    case TOKEN_SEMICOLON:
    case TOKEN_LPAREN:
    case TOKEN_RPAREN:
    case TOKEN_LBRACKET:
    case TOKEN_RBRACKET:
    case TOKEN_LBRACE:
    case TOKEN_RBRACE:
    // Common binops
    case TOKEN_PLUS:
    case TOKEN_MINUS:
    case TOKEN_STAR:
    case TOKEN_SLASH:
    case TOKEN_GT:
    case TOKEN_LT:
    case TOKEN_GE:
    case TOKEN_LE:
    case TOKEN_AND:
    case TOKEN_OR:
    case TOKEN_EQUAL:
      parser_advance(parser);
      return;

    default:
      break;
    }

    parser_advance(parser);
  }

  if (skips >= allowed_skips) {
    parser_error(parser, "Too many tokens skipped; input may be malformed");
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
  case TOKEN_AMPERSAND:
    return BINOP_BIT_AND;
  case TOKEN_PIPE:
    return BINOP_BIT_OR;
  case TOKEN_CARET:
    return BINOP_BIT_XOR;
  case TOKEN_LSHIFT:
    return BINOP_BIT_SHL;
  case TOKEN_RSHIFT:
    return BINOP_BIT_SHR;
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
  case TOKEN_TILDE:
    return UNOP_BIT_NOT;
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
    } else if (parser->had_error) {
      parser_synchronize(parser);
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

  if (parser_match(parser, TOKEN_EXTERN)) {
    return parse_extern(parser);
  }

  if (parser_match(parser, TOKEN_LET) || parser_match(parser, TOKEN_VAR)) {
    return parse_variable_decl(parser);
  }

  if (parser_match(parser, TOKEN_TYPE)) {
    return parse_type_decl(parser);
  }

  if (parser_match(parser, TOKEN_IMPORT)) {
    return parse_import_stmt(parser);
  }

  parser_error(parser, "Expected declaration");
  return NULL;
}

AstNode *parse_import_stmt(Parser *parser) {
  Location loc = parser->previous.location;

  Token path_str = parser_consume(parser, TOKEN_STRING,
                                  "Expected a string for import path.");

  AstNode *import_path = NULL;

  if (path_str.type == TOKEN_STRING) {
    import_path = alloc_node(AST_EXPR_LITERAL_STRING, path_str.location);
    import_path->data.str_lit.value = str_dup(path_str.value.str_val);
  }

  parser_consume(parser, TOKEN_SEMICOLON,
                 "Expected ';' after import declaration");

  AstNode *import_stmt = alloc_node(AST_DECL_IMPORT, loc);
  import_stmt->data.import_stmt.path_str = import_path;
  return import_stmt;
}

static AstNode *parse_function(Parser *parser, Location location, char *name,
                               bool inlined, AstNode *convention) {
  // (params) return_type { body }
  // (params) return_type => expr

  parser_consume(parser, TOKEN_LPAREN, "Expected '(' after function name");

  // Parse parameters
  FuncParam *params = NULL;
  size_t param_count = 0;

  if (!parser_check(parser, TOKEN_RPAREN)) {
    size_t param_capacity = 4;
    params = arena_alloc(&long_lived, param_capacity * sizeof(FuncParam));

    do {
      if (param_count >= 256) {
        parser_error(parser, "Too many parameters (max 256)");
        break;
      }

      if (param_count >= param_capacity) {
        param_capacity *= 2;
        FuncParam *new_params =
            arena_alloc(&long_lived, param_capacity * sizeof(FuncParam));
        memcpy(new_params, params, param_count * sizeof(FuncParam));
        params = new_params;
      }

      bool is_variadic = parser_match(parser, TOKEN_ELLIPSIS);

      Token param_name =
          parser_consume(parser, TOKEN_IDENTIFIER, "Expected parameter name");

      if (parser_check(parser, TOKEN_COMMA)) {
        size_t current_param_count = param_count;

        params[param_count++].name = param_name.lexeme;

        while (parser_match(parser, TOKEN_COMMA)) {
          if (param_count >= param_capacity) {
            param_capacity *= 2;
            FuncParam *new_params =
                arena_alloc(&long_lived, param_capacity * sizeof(FuncParam));
            memcpy(new_params, params, param_count * sizeof(FuncParam));
            params = new_params;
          }

          Token param_name = parser_consume(parser, TOKEN_IDENTIFIER,
                                            "Expected parameter name");
          params[param_count++].name = param_name.lexeme;
        }

        AstNode *param_type = parse_type_expression(parser);

        for (size_t i = current_param_count; i < param_count; i++) {
          params[i].type = param_type;
        }

        continue;
      }

      AstNode *param_type = parse_type_expression(parser);

      params[param_count].name =
          param_name.lexeme; // Already allocated by lexer
      params[param_count].type = param_type;
      params[param_count].is_variadic = is_variadic;
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
    if (!expr) {
      parser_synchronize(parser);
      return NULL;
    }
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

  AstNode *func = NULL;

  if (name) {
    // Named function
    func = alloc_node(AST_DECL_FUNCTION, location);
    func->data.func_decl.name = str_dup(name);
    func->data.func_decl.qualified_name = str_dup(name);
    func->data.func_decl.params = params;
    func->data.func_decl.param_count = param_count;
    func->data.func_decl.return_type = return_type;
    func->data.func_decl.body = body;
    func->data.func_decl.inlined = inlined;
    func->data.func_decl.convention = convention;
  } else {
    // Anonymous function
    func = alloc_node(AST_EXPR_FUNCTION, location);
    func->data.func_expr.params = params;
    func->data.func_expr.param_count = param_count;
    func->data.func_expr.return_type = return_type;
    func->data.func_expr.body = body;
    func->data.func_expr.inlined = inlined;
    func->data.func_expr.convention = convention;
  }

  return func;
}

AstNode *parse_function_decl(Parser *parser) {
  // fn name(params) return_type { body }
  // fn name(params) return_type => expr

  bool inlined = parser_match(parser, TOKEN_INLINE);

  AstNode *convention = NULL;
  if (parser_match(parser, TOKEN_STRING)) {
    convention = alloc_node(AST_EXPR_LITERAL_STRING, parser->previous.location);
    convention->data.str_lit.value = str_dup(parser->previous.lexeme);
  }

  Token name =
      parser_consume(parser, TOKEN_IDENTIFIER, "Expected function name");

  return parse_function(parser, name.location, name.lexeme, inlined,
                        convention);
}

AstNode *parse_extern(Parser *parser) {
  Location extern_loc = parser->previous.location;

  // extern fn name(params) return_type;
  AstNode *lib_name = NULL;
  if (parser_match(parser, TOKEN_STRING)) {
    lib_name = alloc_node(AST_EXPR_LITERAL_STRING, parser->previous.location);
    lib_name->data.str_lit.value = str_dup(parser->previous.lexeme);
  }

  if (parser_match(parser, TOKEN_LBRACE)) {
    // Extern block
    size_t count = 0, capacity = 2;
    AstNode **externs = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

    // fn ..., type IDENT
    while (parser_check(parser, TOKEN_FN) || parser_check(parser, TOKEN_TYPE) ||
           parser_check(parser, TOKEN_LET) || parser_check(parser, TOKEN_VAR)) {
      if (count >= capacity) {
        capacity *= 2;
        AstNode **new_externs =
            arena_alloc(&long_lived, capacity * sizeof(AstNode *));
        memcpy(new_externs, externs, count * sizeof(AstNode *));
        externs = new_externs;
      }

      if (parser_match(parser, TOKEN_FN)) {
        Token name = parser_consume(parser, TOKEN_IDENTIFIER,
                                    "Expected extern function name");

        parser_consume(parser, TOKEN_LPAREN,
                       "Expected '(' after function name");

        // Parse parameters
        FuncParam *params = NULL;
        size_t param_count = 0;

        if (!parser_check(parser, TOKEN_RPAREN)) {
          // We have parameters
          // For now, allocate space for up to 16 parameters (we'll improve
          // this later)
          params = arena_alloc(&long_lived, 16 * sizeof(FuncParam));

          do {
            if (param_count >= 16) {
              parser_error(parser, "Too many parameters (max 16)");
              break;
            }

            Token param_name = parser_consume(parser, TOKEN_IDENTIFIER,
                                              "Expected parameter name");
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
        parser_consume(parser, TOKEN_SEMICOLON,
                       "Expected ';' after extern function declaration");

        AstNode *func = alloc_node(AST_DECL_EXTERN_FUNC, name.location);
        func->data.extern_func.name = str_dup(name.lexeme);
        func->data.extern_func.qualified_name = str_dup(name.lexeme);
        func->data.extern_func.params = params;
        func->data.extern_func.param_count = param_count;
        func->data.extern_func.return_type = return_type;
        func->data.extern_func.lib_name = lib_name;

        externs[count++] = func;
      } else if (parser_match(parser, TOKEN_TYPE)) {
        Token name = parser_consume(parser, TOKEN_IDENTIFIER,
                                    "Expected extern function name");
        parser_consume(parser, TOKEN_SEMICOLON,
                       "Expected ';' after extern type declaration");

        AstNode *opaque_type = alloc_node(AST_DECL_EXTERN_TYPE, name.location);
        opaque_type->data.extern_type.name = str_dup(name.lexeme);
        opaque_type->data.extern_type.qualified_name = str_dup(name.lexeme);

        externs[count++] = opaque_type;
      } else if (parser_match(parser, TOKEN_LET)) {
        Token name =
            parser_consume(parser, TOKEN_IDENTIFIER, "Expected constant name");
        AstNode *type_expr = parse_type_expression(parser);
        parser_consume(parser, TOKEN_SEMICOLON,
                       "Expected ';' after constant declaration");

        AstNode *let = alloc_node(AST_DECL_EXTERN_CONSTANT, name.location);
        let->data.extern_const_decl.name = str_dup(name.lexeme);
        let->data.extern_const_decl.qualified_name = str_dup(name.lexeme);
        let->data.extern_const_decl.type_expr = type_expr;
        let->data.extern_const_decl.lib_name = lib_name;
        externs[count++] = let;
      } else if (parser_match(parser, TOKEN_VAR)) {
        Token name =
            parser_consume(parser, TOKEN_IDENTIFIER, "Expected variable name");
        AstNode *type_expr = parse_type_expression(parser);
        parser_consume(parser, TOKEN_SEMICOLON,
                       "Expected ';' after variable declaration");

        AstNode *var = alloc_node(AST_DECL_EXTERN_VARIABLE, name.location);
        var->data.extern_var_decl.name = str_dup(name.lexeme);
        var->data.extern_var_decl.qualified_name = str_dup(name.lexeme);
        var->data.extern_var_decl.type_expr = type_expr;
        var->data.extern_var_decl.lib_name = lib_name;
        externs[count++] = var;
      }
    }

    parser_consume(parser, TOKEN_RBRACE, "Expect '}' after extern block");

    AstNode *extern_block = alloc_node(AST_DECL_EXTERN_BLOCK, extern_loc);
    extern_block->data.extern_block.lib_name = lib_name;
    extern_block->data.extern_block.decls = externs;
    extern_block->data.extern_block.decls_count = count;

    return extern_block;
  } else if (parser_match(parser, TOKEN_FN)) {
    Token name = parser_consume(parser, TOKEN_IDENTIFIER,
                                "Expected extern function name");

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
    parser_consume(parser, TOKEN_SEMICOLON,
                   "Expected ';' after extern function declaration");

    AstNode *func = alloc_node(AST_DECL_EXTERN_FUNC, name.location);
    func->data.extern_func.name = str_dup(name.lexeme);
    func->data.extern_func.qualified_name = str_dup(name.lexeme);
    func->data.extern_func.params = params;
    func->data.extern_func.param_count = param_count;
    func->data.extern_func.return_type = return_type;
    func->data.extern_func.lib_name = lib_name;
    return func;
  } else if (parser_match(parser, TOKEN_TYPE)) {
    Token name = parser_consume(parser, TOKEN_IDENTIFIER,
                                "Expected extern function name");
    parser_consume(parser, TOKEN_SEMICOLON,
                   "Expected ';' after extern type declaration");

    AstNode *opaque_type = alloc_node(AST_DECL_EXTERN_TYPE, name.location);
    opaque_type->data.extern_type.name = str_dup(name.lexeme);
    opaque_type->data.extern_type.qualified_name = str_dup(name.lexeme);
    return opaque_type;
  } else if (parser_match(parser, TOKEN_LET)) {
    Token name =
        parser_consume(parser, TOKEN_IDENTIFIER, "Expected constant name");
    AstNode *type_expr = parse_type_expression(parser);
    parser_consume(parser, TOKEN_SEMICOLON,
                   "Expected ';' after constant declaration");

    AstNode *let = alloc_node(AST_DECL_EXTERN_CONSTANT, name.location);
    let->data.extern_const_decl.name = str_dup(name.lexeme);
    let->data.extern_const_decl.qualified_name = str_dup(name.lexeme);
    let->data.extern_const_decl.type_expr = type_expr;
    let->data.extern_const_decl.lib_name = lib_name;
    return let;
  } else if (parser_match(parser, TOKEN_VAR)) {
    Token name =
        parser_consume(parser, TOKEN_IDENTIFIER, "Expected variable name");
    AstNode *type_expr = parse_type_expression(parser);
    parser_consume(parser, TOKEN_SEMICOLON,
                   "Expected ';' after variable declaration");

    AstNode *var = alloc_node(AST_DECL_EXTERN_VARIABLE, name.location);
    var->data.extern_var_decl.name = str_dup(name.lexeme);
    var->data.extern_var_decl.qualified_name = str_dup(name.lexeme);
    var->data.extern_var_decl.type_expr = type_expr;
    var->data.extern_var_decl.lib_name = lib_name;
    return var;
  }
  parser_error(parser, "extern is only allowed on function prototypes, opaque "
                       "types, variables or constants.");
  return NULL;
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
    var->data.var_decl.qualified_name = str_dup(name.lexeme);
    var->data.var_decl.type_expr = type_expr;
    var->data.var_decl.init = init;
    return var;
  } else {
    AstNode *const_var = alloc_node(AST_DECL_CONSTANT, name.location);
    const_var->data.const_decl.name = str_dup(name.lexeme);
    const_var->data.const_decl.qualified_name = str_dup(name.lexeme);
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
  node->data.type_decl.qualified_name = str_dup(name.lexeme);
  node->data.type_decl.type_expr = type_expr;

  return node;
}

AstNode *parse_print_stmt(Parser *parser) {
  // print expr;

  Location loc = parser->previous.location;
  size_t count = 0, capacity = 2;

  AstNode **exprs = arena_alloc(&long_lived, capacity * sizeof(AstNode *));
  exprs[count++] = parse_expression(parser);

  while (parser_match(parser, TOKEN_COMMA)) {
    if (count >= capacity) {
      capacity *= 2;
      AstNode **new_exprs =
          arena_alloc(&long_lived, capacity * sizeof(AstNode *));
      memcpy(new_exprs, exprs, count * sizeof(AstNode *));
      exprs = new_exprs;
    }

    exprs[count++] = parse_expression(parser);
  }

  parser_consume(parser, TOKEN_SEMICOLON,
                 "Expected ';' after print statement.");

  AstNode *node = arena_alloc(&long_lived, sizeof(AstNode));
  node->kind = AST_STMT_PRINT;
  node->loc = loc;
  node->data.print_stmt.exprs = exprs;
  node->data.print_stmt.expr_count = count;

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

AstNode *parse_switch_stmt(Parser *parser) {
  // switch <expr> { case COND1: BODY1; ... (else: BODY) }

  Location loc = parser->previous.location;

  AstNode *stmt = alloc_node(AST_STMT_SWITCH, loc);
  stmt->data.switch_stmt.condition = parse_expression(parser);

  parser_consume(parser, TOKEN_LBRACE, "Expect '{' after switch condition");

  size_t count = 0, capacity = 2;
  AstNode **cases = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

  // Parse all cases
  while (parser_match(parser, TOKEN_CASE)) {
    if (count >= capacity) {
      capacity *= 2;
      AstNode **new_cases =
          arena_alloc(&long_lived, capacity * sizeof(AstNode *));
      memcpy(new_cases, cases, count * sizeof(AstNode *));
      cases = new_cases;
    }

    AstNode *_case = alloc_node(AST_STMT_CASE, parser->previous.location);
    _case->data.case_stmt.switch_stmt = stmt;
    _case->data.case_stmt.condition = parse_expression(parser);

    parser_consume(parser, TOKEN_COLON,
                   "Expect ':' after switch case condition");
    _case->data.case_stmt.body = parse_statement(parser);

    cases[count++] = _case;
  }

  stmt->data.switch_stmt.cases = cases;
  stmt->data.switch_stmt.case_count = count;

  // Parse default
  if (parser_match(parser, TOKEN_ELSE)) {
    parser_consume(parser, TOKEN_COLON, "Expect ':' after switch case else");
    stmt->data.switch_stmt.default_case = parse_statement(parser);
  }

  parser_consume(parser, TOKEN_RBRACE, "Expect '}' after switch cases");

  return stmt;
}

AstNode *parse_defer_stmt(Parser *parser) {
  Location loc = parser->previous.location;

  AstNode *stmt = alloc_node(AST_STMT_DEFER, loc);
  stmt->data.defer_stmt.stmt = parse_statement(parser);

  return stmt;
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
  if (parser_match(parser, TOKEN_SWITCH)) {
    return parse_switch_stmt(parser);
  }
  if (parser_match(parser, TOKEN_DEFER)) {
    return parse_defer_stmt(parser);
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

  AstNode *iterator_name = NULL;
  if (parser_match(parser, TOKEN_COLON)) {
    iterator_name = parse_primary(parser);
    if (iterator_name->kind != AST_EXPR_IDENTIFIER) {
      parser_error(parser,
                   "Expected an identifier to name the iterator of the loop.");
    }
  }

  // Parse loop body
  AstNode *body = parse_statement(parser);

  AstNode *stmt = alloc_node(AST_STMT_LOOP, loc);
  stmt->data.loop_stmt.start = start;
  stmt->data.loop_stmt.end = end;
  stmt->data.loop_stmt.inclusive = inclusive;
  stmt->data.loop_stmt.iterator_name = iterator_name;
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
    if (!init_lhs || !init_rhs) {
      parser_synchronize(parser);
      return NULL;
    }

    init = alloc_node(AST_STMT_ASSIGN, init_lhs->loc);
    init->data.assign_stmt.op = -1;
    init->data.assign_stmt.lhs = init_lhs;
    init->data.assign_stmt.rhs = init_rhs;
  }

  // Parse condition
  AstNode *cond = parse_expression(parser);
  parser_consume(parser, TOKEN_SEMICOLON,
                 "Expected ';' after for loop condition");

  // Parse update (assignment without semicolon)
  AstNode *lhs = parse_expression(parser);
  AstNode *update = lhs;

  if (lhs->kind != AST_EXPR_POSTFIX_INC && lhs->kind != AST_EXPR_POSTFIX_DEC) {
    parser_consume(parser, TOKEN_EQUAL, "Expected '=' in for loop update");
    AstNode *rhs = parse_expression(parser);

    if (!lhs || !rhs) {
      parser_synchronize(parser);
      return NULL;
    }

    update = alloc_node(AST_STMT_ASSIGN, lhs->loc);
    update->data.assign_stmt.op = -1;
    update->data.assign_stmt.lhs = lhs;
    update->data.assign_stmt.rhs = rhs;
  }

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

  // Handle compound assignments
  TokenType compound_op = TOKEN_EOF;
  BinaryOp binop;

  if (parser_match(parser, TOKEN_PLUS_EQUAL)) {
    compound_op = TOKEN_PLUS_EQUAL;
    binop = BINOP_ADD;
  } else if (parser_match(parser, TOKEN_MINUS_EQUAL)) {
    compound_op = TOKEN_MINUS_EQUAL;
    binop = BINOP_SUB;
  } else if (parser_match(parser, TOKEN_STAR_EQUAL)) {
    compound_op = TOKEN_STAR_EQUAL;
    binop = BINOP_MUL;
  } else if (parser_match(parser, TOKEN_SLASH_EQUAL)) {
    compound_op = TOKEN_SLASH_EQUAL;
    binop = BINOP_DIV;
  }

  if (compound_op != TOKEN_EOF) {
    Location loc = parser->previous.location;
    AstNode *rhs = parse_expression(parser);
    parser_consume(parser, TOKEN_SEMICOLON,
                   "Expected ';' after compound assignment");

    AstNode *assign = alloc_node(AST_STMT_ASSIGN, loc);
    assign->data.assign_stmt.op = binop;
    assign->data.assign_stmt.lhs = lhs;
    assign->data.assign_stmt.rhs = rhs;
    return assign;
  }

  if (parser_match(parser, TOKEN_EQUAL)) {
    Location loc = parser->previous.location;
    AstNode *rhs = parse_expression(parser);
    parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after assignment");

    AstNode *assign = alloc_node(AST_STMT_ASSIGN, loc);
    assign->data.assign_stmt.op = -1;
    assign->data.assign_stmt.lhs = lhs;
    assign->data.assign_stmt.rhs = rhs;
    return assign;
  } else {
    // Not an assignment, it's an expression statement
    parser_consume(parser, TOKEN_SEMICOLON, "Expected ';' after expression");
    if (!lhs) {
      parser_synchronize(parser);
      return NULL;
    }

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
  AstNode *left = parse_bit_or_expr(parser);

  while (parser_match(parser, TOKEN_AND)) {
    Location loc = parser->previous.location;
    AstNode *right = parse_bit_or_expr(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, loc);
    binop->data.binop.op = BINOP_AND;
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_bit_or_expr(Parser *parser) {
  AstNode *left = parse_bit_xor_expr(parser);

  while (parser_match(parser, TOKEN_PIPE)) {
    Token op = parser->previous;
    AstNode *right = parse_bit_xor_expr(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = BINOP_BIT_OR;
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_bit_xor_expr(Parser *parser) {
  AstNode *left = parse_bit_and_expr(parser);

  while (parser_match(parser, TOKEN_CARET)) {
    Token op = parser->previous;
    AstNode *right = parse_bit_and_expr(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = BINOP_BIT_XOR;
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_bit_and_expr(Parser *parser) {
  AstNode *left = parse_equality(parser);

  while (parser_match(parser, TOKEN_AMPERSAND)) {
    Token op = parser->previous;
    AstNode *right = parse_equality(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = BINOP_BIT_AND;
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
  AstNode *left = parse_shift(parser);

  while (parser_match(parser, TOKEN_LT) || parser_match(parser, TOKEN_LE) ||
         parser_match(parser, TOKEN_GT) || parser_match(parser, TOKEN_GE)) {
    Token op = parser->previous;
    AstNode *right = parse_shift(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = token_to_binary_op(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_shift(Parser *parser) {
  AstNode *left = parse_cast(parser);

  while (parser_match(parser, TOKEN_LSHIFT) ||
         parser_match(parser, TOKEN_RSHIFT)) {
    Token op = parser->previous;
    AstNode *right = parse_cast(parser);

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = token_to_binary_op(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

AstNode *parse_cast(Parser *parser) {
  AstNode *left = parse_term(parser);

  while (parser_match(parser, TOKEN_AS)) {
    Token op = parser->previous;
    AstNode *right = parse_type_expression(parser);

    AstNode *expl_cast = alloc_node(AST_EXPR_EXPLICIT_CAST, op.location);
    expl_cast->data.explicit_cast.expr = left;
    expl_cast->data.explicit_cast.target_type = right;
    expl_cast->data.explicit_cast.pointer_cast = false;
    left = expl_cast;
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
      parser_match(parser, TOKEN_STAR) || parser_match(parser, TOKEN_TILDE)) {
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

  if (!expr) {
    parser_synchronize(parser);
    return NULL;
  }

  while (true) {
    if (parser_match(parser, TOKEN_PLUS_PLUS)) {
      // Postfix increment: expr++
      Location loc = parser->previous.location;
      AstNode *postfix = alloc_node(AST_EXPR_POSTFIX_INC, loc);
      postfix->data.postfix_inc.operand = expr;
      expr = postfix;
    } else if (parser_match(parser, TOKEN_MINUS_MINUS)) {
      // expr--
      Location loc = parser->previous.location;
      AstNode *postfix = alloc_node(AST_EXPR_POSTFIX_DEC, loc);
      postfix->data.postfix_dec.operand = expr;
      expr = postfix;
    } else if (parser_match(parser, TOKEN_NOT)) {
      Location loc = parser->previous.location;
      AstNode *force_unwrap = alloc_node(AST_EXPR_FORCE_UNWRAP, loc);
      force_unwrap->data.force_unwrap.operand = expr;
      expr = force_unwrap;
    } else if (parser_match(parser, TOKEN_LPAREN)) {
      expr = parse_call(parser, expr);
    } else if (parser_match(parser, TOKEN_LBRACKET)) {
      expr = parse_index(parser, expr);
    } else if (parser_match(parser, TOKEN_DOT)) {
      // Check for struct literal: IDENTIFIER.{ ... }
      if (parser_check(parser, TOKEN_LBRACE) &&
          (expr->kind == AST_EXPR_IDENTIFIER ||
           expr->kind == AST_EXPR_MODULE_MEMBER)) {
        parser_advance(parser); // consume '{'
        Location loc = expr->loc;
        char *type_name = NULL;

        // FIXME: Will be better to defer the type name to checker
        if (expr->kind == AST_EXPR_IDENTIFIER) {
          type_name = expr->data.ident.name;
        } else {
          char *prefix =
              prepend(expr->data.mod_member_expr.module->data.ident.name, "__");
          type_name = prepend(prefix, expr->data.mod_member_expr.member);
        }

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
            if (parser_check(parser, TOKEN_RBRACE)) {
              break;
            }
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
              parser_synchronize(parser);
              continue;
            }
            parser_advance(parser);
            Token field_name = parser->previous;

            parser_consume(parser, TOKEN_EQUAL,
                           "Expected '=' after field name");

            field_names[count] = str_dup(field_name.lexeme);
            field_values[count] = parse_expression(parser);
            if (!field_values[count]) {
              parser_synchronize(parser);
              continue;
            }
            count++;

          } while (parser_match(parser, TOKEN_COMMA));
        }

        parser_consume(parser, TOKEN_RBRACE,
                       "Expected '}' after struct literal fields");

        expr = alloc_node(AST_EXPR_STRUCT_LITERAL, loc);
        expr->data.struct_literal.type_name = type_name;
        expr->data.struct_literal.qualified_type_name = type_name;
        expr->data.struct_literal.field_names = field_names;
        expr->data.struct_literal.field_values = field_values;
        expr->data.struct_literal.field_count = count;
        return expr;
      } else {
        // Regular member access
        expr = parse_member(parser, expr);
      }
    } else if (parser_match(parser, TOKEN_MOD_SCOPE)) {
      // Module member access
      expr = parse_module_member(parser, expr);
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
  size_t arg_capacity = 4;

  AstNode *call = alloc_node(AST_EXPR_CALL, loc);

  if (!parser_check(parser, TOKEN_RPAREN)) {
    // FIXME
    args = arena_alloc(&long_lived,
                       arg_capacity * sizeof(AstNode *)); // Max 16 args

    do {
      if (arg_count >= 64) {
        parser_error(parser, "Too many arguments (max 64)");
        break;
      }

      if (arg_count >= arg_capacity) {
        arg_capacity *= 2;
        AstNode **new_args =
            arena_alloc(&long_lived, arg_capacity * sizeof(AstNode *));
        memcpy(new_args, args, arg_count * sizeof(AstNode *));
        args = new_args;
      }

      args[arg_count++] = parse_expression(parser);
    } while (parser_match(parser, TOKEN_COMMA));
  }

  parser_consume(parser, TOKEN_RPAREN, "Expected ')' after arguments");

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

AstNode *parse_module_member(Parser *parser, AstNode *object) {
  if (object->kind != AST_EXPR_IDENTIFIER) {
    parser_error_at_previous(parser, "Module name must be an identifier.");
  }

  Location loc = parser->previous.location;
  AstNode *mod_mem = alloc_node(AST_EXPR_MODULE_MEMBER, loc);
  mod_mem->data.mod_member_expr.module = object;

  Token member = parser_consume(parser, TOKEN_IDENTIFIER,
                                "Expected module member name after '::'");
  mod_mem->data.mod_member_expr.member = str_dup(member.lexeme);

  mod_mem->data.mod_member_expr.is_extern = false;
  return mod_mem;
}

static AstNode *parse_interpolated_string(Parser *parser) {
  // parser->previous is the opening TOKEN_BACKTICK
  Location loc = parser->previous.location;

  AstNode **parts = NULL;
  size_t count = 0;
  size_t capacity = 4;
  parts = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

  // Parse until we hit the closing backtick
  while (!parser_check(parser, TOKEN_BACKTICK) &&
         !parser_check(parser, TOKEN_EOF)) {
    // Grow array if needed
    if (count >= capacity) {
      capacity *= 2;
      AstNode **new_array =
          arena_alloc(&long_lived, capacity * sizeof(AstNode *));
      memcpy(new_array, parts, count * sizeof(AstNode *));
      parts = new_array;
    }

    if (parser_match(parser, TOKEN_STRING)) {
      // String fragment
      Token str = parser->previous;
      AstNode *lit = alloc_node(AST_EXPR_LITERAL_STRING, str.location);
      lit->data.str_lit.value = str.value.str_val;
      parts[count++] = lit;
    } else if (parser_match(parser, TOKEN_LBRACE)) {
      // Interpolated expression: { expr }
      AstNode *expr = parse_expression(parser);
      parser_consume(parser, TOKEN_RBRACE,
                     "Expected '}' after interpolated expression");
      parts[count++] = expr;
    } else {
      parser_error_at_current(parser,
                              "Unexpected token in interpolated string");
      parser_advance(parser); // Skip problematic token
    }
  }

  parser_consume(parser, TOKEN_BACKTICK,
                 "Expected closing '`' for interpolated string");

  AstNode *interp = alloc_node(AST_EXPR_INTERPOLATED_STRING, loc);
  interp->data.interpolated_string.parts = parts;
  interp->data.interpolated_string.num_parts = count;
  return interp;
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

  if (parser_match(parser, TOKEN_BACKTICK)) {
    return parse_interpolated_string(parser);
  }

  if (parser_match(parser, TOKEN_TRUE) || parser_match(parser, TOKEN_FALSE)) {
    Token bool_tok = parser->previous;
    AstNode *lit = alloc_node(AST_EXPR_LITERAL_BOOL, bool_tok.location);
    lit->data.bool_lit.value = bool_tok.value.bool_val;
    return lit;
  }

  if (parser_match(parser, TOKEN_NIL)) {
    Token nil_tok = parser->previous;
    AstNode *lit = alloc_node(AST_EXPR_LITERAL_NIL, nil_tok.location);
    return lit;
  }

  // Context
  if (parser_match(parser, TOKEN_CONTEXT)) {
    return alloc_node(AST_EXPR_CONTEXT, parser->previous.location);
  }

  // Identifier
  if (parser_match(parser, TOKEN_IDENTIFIER)) {
    Token ident = parser->previous;
    AstNode *id = alloc_node(AST_EXPR_IDENTIFIER, ident.location);
    id->data.ident.name = str_dup(ident.lexeme);
    id->data.ident.qualified_name = str_dup(ident.lexeme);
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
      AstNode *grouped_expr = alloc_node(AST_EXPR_GROUPED_EXPR, loc);
      grouped_expr->data.grouped_expr.inner_expr = elements[0];
      return grouped_expr;
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

  // Optional expression (some expression)
  if (parser_match(parser, TOKEN_SOME)) {
    Location loc = parser->previous.location;
    AstNode *inner_expr = parse_expression(parser);

    AstNode *some_expr = alloc_node(AST_EXPR_SOME, loc);
    some_expr->data.some_expr.value = inner_expr;
    return some_expr;
  }

  // Optional none expression
  if (parser_match(parser, TOKEN_NONE)) {
    Location loc = parser->previous.location;

    AstNode *none_expr = alloc_node(AST_EXPR_LITERAL_NONE, loc);
    return none_expr;
  }

  // Function literal
  if (parser_match(parser, TOKEN_FN)) {
    bool inlined = parser_match(parser, TOKEN_INLINE);

    AstNode *convention = NULL;
    if (parser_match(parser, TOKEN_STRING)) {
      convention =
          alloc_node(AST_EXPR_LITERAL_STRING, parser->previous.location);
      convention->data.str_lit.value = str_dup(parser->previous.lexeme);
    }

    return parse_function(parser, parser->previous.location, NULL, inlined,
                          convention);
  }

  // Anonymous struct literal
  if (parser_match(parser, TOKEN_DOT)) {
    if (parser_match(parser, TOKEN_IDENTIFIER)) {
      // Partial member access .member
      AstNode *expr = alloc_node(AST_EXPR_PARTIAL_MEMBER, parser->previous.location);
      expr->data.partial_member_expr.member = str_dup(parser->previous.lexeme);
      return expr;
    } else if (parser_match(parser, TOKEN_LBRACE)) {
      // Check for struct literal: .{ ... }
      Location loc = parser->previous.location;

      // Parse struct literal fields
      char **field_names = NULL;
      AstNode **field_values = NULL;
      size_t count = 0;
      size_t capacity = 4;

      field_names = arena_alloc(&long_lived, capacity * sizeof(char *));
      field_values = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

      if (!parser_check(parser, TOKEN_RBRACE)) {
        do {
          if (parser_check(parser, TOKEN_RBRACE)) {
            break;
          }
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
            parser_synchronize(parser);
            continue;
          }
          parser_advance(parser);
          Token field_name = parser->previous;

          parser_consume(parser, TOKEN_EQUAL, "Expected '=' after field name");

          field_names[count] = str_dup(field_name.lexeme);
          field_values[count] = parse_expression(parser);
          if (!field_values[count]) {
            parser_synchronize(parser);
            continue;
          }
          count++;

        } while (parser_match(parser, TOKEN_COMMA));
      }

      parser_consume(parser, TOKEN_RBRACE,
                     "Expected '}' after struct literal fields");

      AstNode *expr = alloc_node(AST_EXPR_STRUCT_LITERAL, loc);
      expr->data.struct_literal.type_name = NULL;
      expr->data.struct_literal.qualified_type_name = NULL;
      expr->data.struct_literal.field_names = field_names;
      expr->data.struct_literal.field_values = field_values;
      expr->data.struct_literal.field_count = count;
      return expr;
    }
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

    AstNode *convention = NULL;
    if (parser_match(parser, TOKEN_STRING)) {
      convention =
          alloc_node(AST_EXPR_LITERAL_STRING, parser->previous.location);
      convention->data.str_lit.value = str_dup(parser->previous.lexeme);
    }

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
    type->data.type_function.convention = convention;
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

  // Optional type: ?T
  if (parser_match(parser, TOKEN_QUESTION)) {
    Location loc = parser->previous.location;
    AstNode *base = parse_type_expression(parser);

    type = alloc_node(AST_TYPE_OPTIONAL, loc);
    type->data.type_optional.base = base;
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
        if (parser_check(parser, TOKEN_RBRACE)) {
          break;
        }

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

        // Parse field: IDENTIFIER (, IDENTIFIER ...) TYPE
        if (!parser_check(parser, TOKEN_IDENTIFIER)) {
          parser_error(parser, "Expected field name");
          return NULL;
        }
        parser_advance(parser);
        Token field_name = parser->previous;

        if (parser_check(parser, TOKEN_COMMA)) {
          size_t current_count = count;

          field_names[count++] = str_dup(field_name.lexeme);

          while (parser_match(parser, TOKEN_COMMA)) {
            if (!parser_check(parser, TOKEN_IDENTIFIER)) {
              parser_error(parser, "Expected field name");
              return NULL;
            }
            parser_advance(parser);
            Token field_name = parser->previous;

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

            field_names[count++] = str_dup(field_name.lexeme);
          }

          field_types[current_count] = parse_type_expression(parser);
          if (!field_types[current_count]) {
            return NULL;
          }

          // Copy type to all fields
          for (size_t i = current_count + 1; i < count; i++) {
            field_types[i] = field_types[current_count];
          }

          continue;
        }

        field_names[count] = str_dup(field_name.lexeme);
        field_types[count] = parse_type_expression(parser);
        if (!field_types[count]) {
          return NULL;
        }
        count++;

      } while (parser_match(parser, TOKEN_SEMICOLON));
    }

    parser_consume(parser, TOKEN_RBRACE, "Expected '}' after struct fields");

    type = alloc_node(AST_TYPE_STRUCT, loc);
    type->data.type_struct.field_names = field_names;
    type->data.type_struct.field_types = field_types;
    type->data.type_struct.field_count = count;
    return type;
  }

  // Union type: union (enum+) { field1 type1, field2 type2, ... }
  if (parser_match(parser, TOKEN_UNION)) {
    Location loc = parser->previous.location;

    bool is_tagged = parser_match(parser, TOKEN_ENUM);

    parser_consume(parser, TOKEN_LBRACE, "Expected '{' after 'union'");

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
        if (parser_check(parser, TOKEN_RBRACE)) {
          break;
        }

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

        // Parse field: IDENTIFIER (, IDENTIFIER ...) TYPE
        if (!parser_check(parser, TOKEN_IDENTIFIER)) {
          parser_error(parser, "Expected field name");
          return NULL;
        }
        parser_advance(parser);
        Token field_name = parser->previous;

        if (parser_check(parser, TOKEN_COMMA)) {
          size_t current_count = count;

          field_names[count++] = str_dup(field_name.lexeme);

          while (parser_match(parser, TOKEN_COMMA)) {
            if (!parser_check(parser, TOKEN_IDENTIFIER)) {
              parser_error(parser, "Expected field name");
              return NULL;
            }
            parser_advance(parser);
            Token field_name = parser->previous;

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

            field_names[count++] = str_dup(field_name.lexeme);
          }

          field_types[current_count] = parse_type_expression(parser);
          if (!field_types[current_count]) {
            return NULL;
          }

          // Copy type to all fields
          for (size_t i = current_count + 1; i < count; i++) {
            field_types[i] = field_types[current_count];
          }

          continue;
        }

        field_names[count] = str_dup(field_name.lexeme);
        field_types[count] = parse_type_expression(parser);
        if (!field_types[count]) {
          return NULL;
        }
        count++;

      } while (parser_match(parser, TOKEN_SEMICOLON));
    }

    parser_consume(parser, TOKEN_RBRACE, "Expected '}' after union fields");

    type = alloc_node(AST_TYPE_UNION, loc);
    type->data.type_union.is_tagged = is_tagged;
    type->data.type_union.variant_names = field_names;
    type->data.type_union.variant_types = field_types;
    type->data.type_union.variant_count = count;
    return type;
  }

  // Enum type: enum { variant, ... }
  if (parser_match(parser, TOKEN_ENUM)) {
    Location loc = parser->previous.location;

    parser_consume(parser, TOKEN_LBRACE, "Expected '{' after 'enum'");

    // Parse fields
    char **variant_names = NULL;
    size_t count = 0;
    size_t capacity = 2;

    variant_names = arena_alloc(&long_lived, capacity * sizeof(char *));

    // Allow empty enum
    if (!parser_check(parser, TOKEN_RBRACE)) {
      do {
        if (parser_check(parser, TOKEN_RBRACE)) {
          break;
        }

        // Grow arrays if needed
        if (count >= capacity) {
          capacity *= 2;
          char **new_variants =
              arena_alloc(&long_lived, capacity * sizeof(char *));
          memcpy(new_variants, variant_names, count * sizeof(char *));
          variant_names = new_variants;
        }

        // Parse variant: IDENTIFIER (, IDENTIFIER ...)
        if (!parser_check(parser, TOKEN_IDENTIFIER)) {
          parser_error(parser, "Expected enum variant name");
          return NULL;
        }
        parser_advance(parser);

        Token variant_name = parser->previous;
        variant_names[count++] = str_dup(variant_name.lexeme);
      } while (parser_match(parser, TOKEN_COMMA));
    }

    parser_consume(parser, TOKEN_RBRACE, "Expected '}' after enum variants");

    type = alloc_node(AST_TYPE_ENUM, loc);
    type->data.type_enum.variant_names = variant_names;
    type->data.type_enum.variant_count = count;
    return type;
  }

  // Built-in types
  if (parser_match(parser, TOKEN_INT_TYPE)) {
    type = alloc_node(AST_TYPE_NAMED, parser->previous.location);
    type->data.type_named.name = str_dup("int");
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

  // Custom/named types
  if (parser_match(parser, TOKEN_IDENTIFIER)) {
    Token name = parser->previous;

    if (parser_match(parser, TOKEN_MOD_SCOPE)) {
      Token mod_type_name = parser_consume(parser, TOKEN_IDENTIFIER,
                                           "Expected an identifier after '::'");
      type = alloc_node(AST_TYPE_QUALIFIED_NAMED, mod_type_name.location);
      type->data.type_qualified_named.mod_name = name.lexeme;
      type->data.type_qualified_named.mem_name = mod_type_name.lexeme;
      return type;
    }

    type = alloc_node(AST_TYPE_NAMED, name.location);
    type->data.type_named.name = str_dup(name.lexeme);
    return type;
  }

  parser_error(parser, "Expected type");
  return NULL;
}
