#include "parser2.h"
#include "alloc.h"
#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External allocator
extern Arena long_lived;

static AstNode *parse_import_stmt2(Parser2 *p);
static AstNode *parse_variable_decl2(Parser2 *p, bool is_mutable);
static AstNode *parse_type_decl2(Parser2 *p);
static AstNode *parse_extern_decl2(Parser2 *p);

static AstNode *alloc_node(AstKind kind, Location loc) {
  AstNode *n = arena_alloc(&long_lived, sizeof(AstNode));
  n->kind = kind;
  n->loc = loc;
  n->resolved_type = NULL;
  return n;
}

static Location cur_loc(Parser2 *p) {
  Location loc = {
      .file = p->abs_file_path,
      .line = p->current.location.line,
      .column = p->current.location.column,
  };
  return loc;
}

static Location prev_loc(Parser2 *p) {
  Location loc = {
      .file = p->abs_file_path,
      .line = p->previous.location.line,
      .column = p->previous.location.column,
  };
  return loc;
}

static size_t parser2_error_count(Parser2 *p) {
  if (!p || !p->diagnostics) {
    return 0;
  }
  return p->diagnostics->error_count;
}

static bool parser2_too_many_errors(Parser2 *p) {
  return parser2_error_count(p) >= p->max_errors;
}

static bool parser2_is_stmt_starter(TokenType t) {
  switch (t) {
  case TOKEN_FN:
  case TOKEN_VAR:
  case TOKEN_LET:
  case TOKEN_TYPE:
  case TOKEN_IMPORT:
  case TOKEN_EXTERN:
  case TOKEN_IF:
  case TOKEN_WHILE:
  case TOKEN_FOR:
  case TOKEN_LOOP:
  case TOKEN_RETURN:
  case TOKEN_PRINT:
  case TOKEN_SWITCH:
  case TOKEN_DEFER:
  case TOKEN_BREAK:
  case TOKEN_CONTINUE:
  case TOKEN_LBRACE:
  case TOKEN_RBRACE:
  case TOKEN_EOF:
    return true;
  default:
    return false;
  }
}

static bool parser2_expect_semicolon(Parser2 *p, const char *message) {
  if (parser2_match(p, TOKEN_SEMICOLON)) {
    return true;
  }

  parser2_handle_error(p, message);

  // Treat as an inserted ';' when the next token clearly begins a new
  // statement/declaration or closes the current scope.
  if (parser2_is_stmt_starter(p->current.type)) {
    return false;
  }

  return false;
}

static AstNode *parse_function_decl2(Parser2 *p);
static AstNode *parse_block_stmt2(Parser2 *p);
static AstNode *parse_return_stmt2(Parser2 *p);
static AstNode *parse_expression2(Parser2 *p);
static AstNode *parse_primary2(Parser2 *p);
static AstNode *parse_type_expression2(Parser2 *p);

void parser_init(Parser2 *parser, const char *source, const char *filename,
                 const char *abs_file_path) {
  lexer_init(&parser->lexer, source, filename);
  parser->abs_file_path = str_dup(abs_file_path);

  parser->diagnostics = arena_alloc(&long_lived, sizeof(DiagnosticContext));
  diagnostics_init(parser->diagnostics, abs_file_path, source);

  parser->max_errors = 50;
  parser->nesting_depth = 0;
  parser->max_depth = 200;

  // Prime tokens
  parser->current = (Token){0};
  parser->previous = (Token){0};
  parser2_advance(parser);
}

// ---------- token utilities ----------
void parser2_advance(Parser2 *parser) {
  parser->previous = parser->current;

  size_t lexer_errors = 0;
  const size_t max_lexer_errors = 8;

  Location last_err_loc = (Location){0};

  while (true) {
    parser->current = lexer_next_token(&parser->lexer);

    if (parser->current.type != TOKEN_ERROR) {
      return;
    }

    Location err_loc = cur_loc(parser);

    // Detect a "stuck" lexer (repeating the same error location).
    if (lexer_errors > 0 && err_loc.line == last_err_loc.line &&
        err_loc.column == last_err_loc.column) {
      return;
    }
    last_err_loc = err_loc;

    if (parser->diagnostics) {
      Diagnostic *error =
          diagnostic_error(parser->diagnostics, err_loc, "Lexical error: %s",
                           parser->current.lexeme);
      diagnostic_emit(error);
    }

    lexer_errors++;
    if (lexer_errors >= max_lexer_errors) {
      return;
    }

    if (parser2_too_many_errors(parser)) {
      return;
    }
  }
}

bool parser2_check(Parser2 *parser, TokenType type) {
  return parser->current.type == type;
}

bool parser2_match(Parser2 *parser, TokenType type) {
  if (parser2_check(parser, type)) {
    parser2_advance(parser);
    return true;
  }
  return false;
}

Token parser2_consume(Parser2 *parser, TokenType type, const char *message) {
  if (parser->current.type == type) {
    Token consumed = parser->current;
    parser2_advance(parser);
    return consumed;
  }

  parser2_handle_error(parser, message);
  return parser->current;
}

bool parser2_expect(Parser2 *parser, TokenType type, const char *message) {
  if (parser->current.type == type) {
    parser2_advance(parser);
    return true;
  }

  parser2_handle_error(parser, message);
  return false;
}

// ---------- error handling ----------
bool parser2_handle_error(Parser2 *parser, const char *expected) {
  if (parser2_too_many_errors(parser)) {
    return false;
  }

  Location loc = cur_loc(parser);
  Diagnostic *error =
      diagnostic_error(parser->diagnostics, loc, "%s", expected);
  diagnostic_emit(error);

  return true;
}

void parser2_synchronize(Parser2 *parser) {
  // Minimal version: advance until likely statement/declaration boundary.
  // Important: ensure progress even if we're already at EOF.
  if (!parser2_check(parser, TOKEN_EOF)) {
    parser2_advance(parser);
  }

  size_t skips = 0;
  const size_t max_skips = 50;

  while (!parser2_check(parser, TOKEN_EOF) && skips++ < max_skips) {
    if (parser->previous.type == TOKEN_SEMICOLON)
      return;

    switch (parser->current.type) {
    case TOKEN_FN:
    case TOKEN_VAR:
    case TOKEN_LET:
    case TOKEN_TYPE:
    case TOKEN_IMPORT:
    case TOKEN_EXTERN:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_FOR:
    case TOKEN_LOOP:
    case TOKEN_RETURN:
    case TOKEN_LBRACE:
    case TOKEN_RBRACE:
      return;
    default:
      break;
    }

    parser2_advance(parser);
  }
}

// ---------- top level ----------
AstNode *parse_program(Parser2 *parser) {
  AstNode **decls = arena_alloc(&long_lived, 64 * sizeof(AstNode *));
  size_t decl_count = 0;

  while (!parser2_check(parser, TOKEN_EOF)) {
    if (decl_count >= 64) {
      parser2_handle_error(parser, "Too many top-level declarations (max 64)");
      break;
    }

    AstNode *decl = parser2_declaration(parser);
    if (decl) {
      decls[decl_count++] = decl;
    } else {
      parser2_synchronize(parser);
    }

    if (parser->diagnostics->error_count >= parser->max_errors)
      break;
  }

  Location loc = {.file = parser->abs_file_path, .line = 1, .column = 1};
  AstNode *program = alloc_node(AST_STMT_BLOCK, loc);
  program->data.block_stmt.stmts = decls;
  program->data.block_stmt.stmt_count = decl_count;
  return program;
}

AstNode *parser2_declaration(Parser2 *parser) {
  if (parser2_match(parser, TOKEN_FN)) {
    return parse_function_decl2(parser);
  }

  if (parser2_match(parser, TOKEN_EXTERN)) {
    return parse_extern_decl2(parser);
  }

  if (parser2_match(parser, TOKEN_LET)) {
    return parse_variable_decl2(parser, false);
  }

  if (parser2_match(parser, TOKEN_VAR)) {
    return parse_variable_decl2(parser, true);
  }

  if (parser2_match(parser, TOKEN_TYPE)) {
    return parse_type_decl2(parser);
  }

  if (parser2_match(parser, TOKEN_IMPORT)) {
    return parse_import_stmt2(parser);
  }

  parser2_handle_error(parser, "Expected declaration");
  return NULL;
}

static AstNode *parse_import_stmt2(Parser2 *p) {
  Location loc = prev_loc(p); // 'import'

  Token path_tok =
      parser2_consume(p, TOKEN_STRING, "Expected a string for import path.");

  AstNode *import_path = NULL;
  if (path_tok.type == TOKEN_STRING) {
    import_path = alloc_node(AST_EXPR_LITERAL_STRING, path_tok.location);
    // Match parser.c: use processed string value.
    import_path->data.str_lit.value = str_dup(path_tok.value.str_val);
  }

  parser2_expect_semicolon(p, "Expected ';' after import declaration");

  AstNode *import_stmt = alloc_node(AST_DECL_IMPORT, loc);
  import_stmt->data.import_stmt.path_str = import_path;
  return import_stmt;
}

static AstNode *parse_variable_decl2(Parser2 *p, bool is_mutable) {
  // let name = expr; or let name type = expr;
  // var name = expr; or var name type = expr;

  Token name = parser2_consume(p, TOKEN_IDENTIFIER, "Expected variable name");
  if (name.type != TOKEN_IDENTIFIER)
    return NULL;

  // Optional type annotation (present when next token isn't '=' or ';')
  AstNode *type_expr = NULL;
  if (!parser2_check(p, TOKEN_EQUAL) && !parser2_check(p, TOKEN_SEMICOLON)) {
    type_expr = parse_type_expression2(p);
  }

  // Initializer (optional)
  AstNode *init = NULL;
  if (parser2_match(p, TOKEN_EQUAL)) {
    init = parse_expression2(p);
  }

  parser2_expect_semicolon(p, "Expected ';' after variable declaration");

  if (is_mutable) {
    AstNode *var = alloc_node(AST_DECL_VARIABLE, name.location);
    var->data.var_decl.name = str_dup(name.lexeme);
    var->data.var_decl.qualified_name = str_dup(name.lexeme);
    var->data.var_decl.full_qualified_name = NULL;
    var->data.var_decl.type_expr = type_expr;
    var->data.var_decl.init = init;
    return var;
  } else {
    AstNode *c = alloc_node(AST_DECL_CONSTANT, name.location);
    c->data.const_decl.name = str_dup(name.lexeme);
    c->data.const_decl.qualified_name = str_dup(name.lexeme);
    c->data.const_decl.full_qualified_name = NULL;
    c->data.const_decl.type_expr = type_expr;
    c->data.const_decl.value = init;
    return c;
  }
}

static AstNode *parse_type_decl2(Parser2 *p) {
  // type Name = TypeExpr;
  // type Name[T, U, V] = TypeExpr;

  Token name = parser2_consume(p, TOKEN_IDENTIFIER, "Expected type name");
  if (name.type != TOKEN_IDENTIFIER)
    return NULL;

  // Parse comma-separated type parameters (no arbitrary cap). We build a linked
  // list of Token nodes, count them, then build a contiguous Token array.
  Token *type_params = NULL;
  size_t type_param_count = 0;
  if (parser2_match(p, TOKEN_LBRACKET)) {
    typedef struct TypeParamNode {
      Token tok;
      struct TypeParamNode *next;
    } TypeParamNode;

    TypeParamNode *head = NULL;
    TypeParamNode *tail = NULL;

    if (!parser2_check(p, TOKEN_RBRACKET)) {
      while (true) {
        Token type_param_name =
            parser2_consume(p, TOKEN_IDENTIFIER, "Expected parameter name");

        TypeParamNode *node = arena_alloc(&long_lived, sizeof(TypeParamNode));
        node->tok = type_param_name;
        node->next = NULL;

        if (!head) {
          head = node;
          tail = node;
        } else {
          tail->next = node;
          tail = node;
        }

        type_param_count++;

        if (!parser2_match(p, TOKEN_COMMA)) {
          break;
        }

        if (parser2_check(p, TOKEN_RBRACKET)) {
          break;
        }
      }
    }

    parser2_consume(p, TOKEN_RBRACKET, "Expected '[' after generic parameters");

    if (type_param_count > 0) {
      type_params = arena_alloc(&long_lived, type_param_count * sizeof(Token));
      size_t i = 0;
      for (TypeParamNode *n = head; n; n = n->next) {
        type_params[i++] = n->tok;
      }
    }
  }

  parser2_consume(p, TOKEN_EQUAL, "Expected '=' after type name");
  AstNode *type_expr = parse_type_expression2(p);

  parser2_expect_semicolon(p, "Expected ';' after type declaration");

  AstNode *node = alloc_node(AST_DECL_TYPE, name.location);
  node->data.type_decl.name = str_dup(name.lexeme);
  node->data.type_decl.qualified_name = str_dup(name.lexeme);
  node->data.type_decl.full_qualified_name = NULL;
  node->data.type_decl.type_expr = type_expr;
  node->data.type_decl.type_params = type_params;
  node->data.type_decl.type_params_count = type_param_count;
  return node;
}

static AstNode *parse_extern_decl2(Parser2 *p) {
  Location extern_loc = prev_loc(p); // 'extern'

  // extern "lib"
  AstNode *lib_name = NULL;
  if (parser2_match(p, TOKEN_STRING)) {
    lib_name = alloc_node(AST_EXPR_LITERAL_STRING, prev_loc(p));
    lib_name->data.str_lit.value = str_dup(p->previous.lexeme);
  }

  if (parser2_match(p, TOKEN_LBRACE)) {
    // Extern block: { fn..., type..., let..., var... }
    size_t count = 0, capacity = 2;
    AstNode **externs = arena_alloc(&long_lived, capacity * sizeof(AstNode *));

    while (parser2_check(p, TOKEN_FN) || parser2_check(p, TOKEN_TYPE) ||
           parser2_check(p, TOKEN_LET) || parser2_check(p, TOKEN_VAR)) {
      if (count >= capacity) {
        capacity *= 2;
        AstNode **new_externs =
            arena_alloc(&long_lived, capacity * sizeof(AstNode *));
        memcpy(new_externs, externs, count * sizeof(AstNode *));
        externs = new_externs;
      }

      if (parser2_match(p, TOKEN_FN)) {
        Token name = parser2_consume(p, TOKEN_IDENTIFIER,
                                     "Expected extern function name");

        parser2_consume(p, TOKEN_LPAREN, "Expected '(' after function name");

        // Parse parameters into a linked list of FuncParam nodes, then
        // materialize a contiguous FuncParam array at the end.
        typedef struct FuncParamNode {
          FuncParam param;
          struct FuncParamNode *next;
        } FuncParamNode;

        FuncParamNode *param_head = NULL;
        FuncParamNode *param_tail = NULL;
        size_t param_count = 0;

        if (!parser2_check(p, TOKEN_RPAREN)) {
          while (true) {
            Token param_name =
                parser2_consume(p, TOKEN_IDENTIFIER, "Expected parameter name");
            AstNode *param_type = parse_type_expression2(p);

            FuncParamNode *node =
                arena_alloc(&long_lived, sizeof(FuncParamNode));
            node->param.name = param_name.lexeme;
            node->param.type = param_type;
            node->param.is_variadic = false;
            node->next = NULL;

            if (!param_head) {
              param_head = node;
              param_tail = node;
            } else {
              param_tail->next = node;
              param_tail = node;
            }
            param_count++;

            if (!parser2_match(p, TOKEN_COMMA)) {
              break;
            }

            if (parser2_check(p, TOKEN_RPAREN)) {
              break;
            }
          }
        }

        parser2_consume(p, TOKEN_RPAREN, "Expected ')' after parameters");

        FuncParam *params = NULL;
        if (param_count > 0) {
          params = arena_alloc(&long_lived, param_count * sizeof(FuncParam));
          size_t i = 0;
          for (FuncParamNode *n = param_head; n; n = n->next) {
            params[i++] = n->param;
          }
        }

        AstNode *return_type = parse_type_expression2(p);
        parser2_expect_semicolon(
            p, "Expected ';' after extern function declaration");

        AstNode *func = alloc_node(AST_DECL_EXTERN_FUNC, name.location);
        func->data.extern_func.name = str_dup(name.lexeme);
        func->data.extern_func.qualified_name = str_dup(name.lexeme);
        func->data.extern_func.full_qualified_name = NULL;
        func->data.extern_func.params = params;
        func->data.extern_func.param_count = param_count;
        func->data.extern_func.return_type = return_type;
        func->data.extern_func.lib_name = lib_name;

        externs[count++] = func;
      } else if (parser2_match(p, TOKEN_TYPE)) {
        Token name = parser2_consume(p, TOKEN_IDENTIFIER,
                                     "Expected extern function name");
        parser2_expect_semicolon(p,
                                 "Expected ';' after extern type declaration");

        AstNode *opaque_type = alloc_node(AST_DECL_EXTERN_TYPE, name.location);
        opaque_type->data.extern_type.name = str_dup(name.lexeme);
        opaque_type->data.extern_type.qualified_name = str_dup(name.lexeme);
        opaque_type->data.extern_type.full_qualified_name = NULL;

        externs[count++] = opaque_type;
      } else if (parser2_match(p, TOKEN_LET)) {
        Token name =
            parser2_consume(p, TOKEN_IDENTIFIER, "Expected constant name");
        AstNode *type_expr = parse_type_expression2(p);
        parser2_expect_semicolon(p, "Expected ';' after constant declaration");

        AstNode *let = alloc_node(AST_DECL_EXTERN_CONSTANT, name.location);
        let->data.extern_const_decl.name = str_dup(name.lexeme);
        let->data.extern_const_decl.qualified_name = str_dup(name.lexeme);
        let->data.extern_const_decl.full_qualified_name = NULL;
        let->data.extern_const_decl.type_expr = type_expr;
        let->data.extern_const_decl.lib_name = lib_name;

        externs[count++] = let;
      } else if (parser2_match(p, TOKEN_VAR)) {
        Token name =
            parser2_consume(p, TOKEN_IDENTIFIER, "Expected variable name");
        AstNode *type_expr = parse_type_expression2(p);
        parser2_expect_semicolon(p, "Expected ';' after variable declaration");

        AstNode *var = alloc_node(AST_DECL_EXTERN_VARIABLE, name.location);
        var->data.extern_var_decl.name = str_dup(name.lexeme);
        var->data.extern_var_decl.qualified_name = str_dup(name.lexeme);
        var->data.extern_var_decl.full_qualified_name = NULL;
        var->data.extern_var_decl.type_expr = type_expr;
        var->data.extern_var_decl.lib_name = lib_name;

        externs[count++] = var;
      }
    }

    parser2_consume(p, TOKEN_RBRACE, "Expect '}' after extern block");

    AstNode *extern_block = alloc_node(AST_DECL_EXTERN_BLOCK, extern_loc);
    extern_block->data.extern_block.lib_name = lib_name;
    extern_block->data.extern_block.decls = externs;
    extern_block->data.extern_block.decls_count = count;

    return extern_block;
  }

  if (parser2_match(p, TOKEN_FN)) {
    Token name =
        parser2_consume(p, TOKEN_IDENTIFIER, "Expected extern function name");

    parser2_consume(p, TOKEN_LPAREN, "Expected '(' after function name");

    // Parse parameters into a linked list of FuncParam nodes, then materialize
    // a contiguous FuncParam array at the end.
    typedef struct FuncParamNode {
      FuncParam param;
      struct FuncParamNode *next;
    } FuncParamNode;

    FuncParamNode *param_head = NULL;
    FuncParamNode *param_tail = NULL;
    size_t param_count = 0;

    if (!parser2_check(p, TOKEN_RPAREN)) {
      while (true) {
        Token param_name =
            parser2_consume(p, TOKEN_IDENTIFIER, "Expected parameter name");
        AstNode *param_type = parse_type_expression2(p);

        FuncParamNode *node = arena_alloc(&long_lived, sizeof(FuncParamNode));
        node->param.name = param_name.lexeme;
        node->param.type = param_type;
        node->param.is_variadic = false;
        node->next = NULL;

        if (!param_head) {
          param_head = node;
          param_tail = node;
        } else {
          param_tail->next = node;
          param_tail = node;
        }
        param_count++;

        if (!parser2_match(p, TOKEN_COMMA)) {
          break;
        }

        if (parser2_check(p, TOKEN_RPAREN)) {
          break;
        }
      }
    }

    parser2_consume(p, TOKEN_RPAREN, "Expected ')' after parameters");

    FuncParam *params = NULL;
    if (param_count > 0) {
      params = arena_alloc(&long_lived, param_count * sizeof(FuncParam));
      size_t i = 0;
      for (FuncParamNode *n = param_head; n; n = n->next) {
        params[i++] = n->param;
      }
    }

    AstNode *return_type = parse_type_expression2(p);
    parser2_expect_semicolon(p,
                             "Expected ';' after extern function declaration");

    AstNode *func = alloc_node(AST_DECL_EXTERN_FUNC, name.location);
    func->data.extern_func.name = str_dup(name.lexeme);
    func->data.extern_func.qualified_name = str_dup(name.lexeme);
    func->data.extern_func.full_qualified_name = NULL;
    func->data.extern_func.params = params;
    func->data.extern_func.param_count = param_count;
    func->data.extern_func.return_type = return_type;
    func->data.extern_func.lib_name = lib_name;
    return func;
  }

  if (parser2_match(p, TOKEN_TYPE)) {
    Token name =
        parser2_consume(p, TOKEN_IDENTIFIER, "Expected extern function name");
    parser2_expect_semicolon(p, "Expected ';' after extern type declaration");

    AstNode *opaque_type = alloc_node(AST_DECL_EXTERN_TYPE, name.location);
    opaque_type->data.extern_type.name = str_dup(name.lexeme);
    opaque_type->data.extern_type.qualified_name = str_dup(name.lexeme);
    opaque_type->data.extern_type.full_qualified_name = NULL;
    return opaque_type;
  }

  if (parser2_match(p, TOKEN_LET)) {
    Token name = parser2_consume(p, TOKEN_IDENTIFIER, "Expected constant name");
    AstNode *type_expr = parse_type_expression2(p);
    parser2_expect_semicolon(p, "Expected ';' after constant declaration");

    AstNode *let = alloc_node(AST_DECL_EXTERN_CONSTANT, name.location);
    let->data.extern_const_decl.name = str_dup(name.lexeme);
    let->data.extern_const_decl.qualified_name = str_dup(name.lexeme);
    let->data.extern_const_decl.full_qualified_name = NULL;
    let->data.extern_const_decl.type_expr = type_expr;
    let->data.extern_const_decl.lib_name = lib_name;
    return let;
  }

  if (parser2_match(p, TOKEN_VAR)) {
    Token name = parser2_consume(p, TOKEN_IDENTIFIER, "Expected variable name");
    AstNode *type_expr = parse_type_expression2(p);
    parser2_expect_semicolon(p, "Expected ';' after variable declaration");

    AstNode *var = alloc_node(AST_DECL_EXTERN_VARIABLE, name.location);
    var->data.extern_var_decl.name = str_dup(name.lexeme);
    var->data.extern_var_decl.qualified_name = str_dup(name.lexeme);
    var->data.extern_var_decl.full_qualified_name = NULL;
    var->data.extern_var_decl.type_expr = type_expr;
    var->data.extern_var_decl.lib_name = lib_name;
    return var;
  }

  parser2_handle_error(p, "extern is only allowed on function prototypes, "
                          "opaque types, variables or constants.");
  return NULL;
}

static AstNode *parse_function_decl2(Parser2 *p) {
  // fn name(params) return_type { body }
  // fn name(params) return_type => expr
  //
  // NOTE: This is still a partial port: params, generics, inline, and
  // convention are not fully implemented yet, but return/statement behavior is
  // aligned.

  Token name_tok =
      parser2_consume(p, TOKEN_IDENTIFIER, "Expected function name");
  if (name_tok.type != TOKEN_IDENTIFIER)
    return NULL;

  Location loc = prev_loc(p);
  AstNode *fn = alloc_node(AST_DECL_FUNCTION, loc);

  fn->data.func_decl.inlined = false;
  fn->data.func_decl.convention = NULL;
  fn->data.func_decl.name = name_tok.lexeme;
  fn->data.func_decl.qualified_name = NULL;
  fn->data.func_decl.full_qualified_name = NULL;

  // Generics (parsed but minimal: keep empty for now)
  fn->data.func_decl.type_params = NULL;
  fn->data.func_decl.type_param_count = 0;

  parser2_consume(p, TOKEN_LPAREN, "Expected '(' after function name");

  // Minimal: no params (support empty list)
  fn->data.func_decl.params = NULL;
  fn->data.func_decl.param_count = 0;
  if (!parser2_check(p, TOKEN_RPAREN)) {
    // Not implementing params yet; recover by syncing to ')'
    parser2_handle_error(p, "Parameters not implemented in parser2 yet");
    while (!parser2_check(p, TOKEN_RPAREN) && !parser2_check(p, TOKEN_EOF)) {
      parser2_advance(p);
    }
  }
  parser2_consume(p, TOKEN_RPAREN, "Expected ')' after parameters");

  // Return type (required in parser.c path)
  fn->data.func_decl.return_type = parse_type_expression2(p);
  if (!fn->data.func_decl.return_type)
    return NULL;

  // Body: block or fat-arrow expression
  if (parser2_match(p, TOKEN_FAT_ARROW)) {
    // Expression-bodied function: => expr
    AstNode *expr = parse_expression2(p);
    if (!expr)
      return NULL;

    AstNode *ret = alloc_node(AST_STMT_RETURN, expr->loc);
    ret->data.return_stmt.expr = expr;

    AstNode *block = alloc_node(AST_STMT_BLOCK, expr->loc);
    AstNode **stmts = arena_alloc(&long_lived, 1 * sizeof(AstNode *));
    stmts[0] = ret;
    block->data.block_stmt.stmts = stmts;
    block->data.block_stmt.stmt_count = 1;

    fn->data.func_decl.body = block;
    return fn;
  }

  parser2_consume(p, TOKEN_LBRACE, "Expected '{' before function body");
  fn->data.func_decl.body = parse_block_stmt2(p);
  if (!fn->data.func_decl.body)
    return NULL;

  return fn;
}

static AstNode *parse_block_stmt2(Parser2 *p) {
  // Assumes '{' already consumed.
  Location loc = prev_loc(p);
  AstNode *block = alloc_node(AST_STMT_BLOCK, loc);

  AstNode **stmts = arena_alloc(&long_lived, 256 * sizeof(AstNode *));
  size_t stmt_count = 0;

  while (!parser2_check(p, TOKEN_RBRACE) && !parser2_check(p, TOKEN_EOF)) {
    if (stmt_count >= 256) {
      parser2_handle_error(p, "Too many statements in block (max 256)");
      break;
    }

    AstNode *s = parser2_statement(p);
    if (s) {
      stmts[stmt_count++] = s;
    } else {
      parser2_synchronize(p);
    }

    if (p->diagnostics->error_count >= p->max_errors)
      break;
  }

  // Missing '}' at EOF is recoverable: keep the block we parsed so far.
  if (parser2_check(p, TOKEN_EOF)) {
    parser2_handle_error(p, "Expected '}' after block");
  } else if (!parser2_expect(p, TOKEN_RBRACE, "Expected '}' after block")) {
    return NULL;
  }

  block->data.block_stmt.stmts = stmts;
  block->data.block_stmt.stmt_count = stmt_count;
  return block;
}

AstNode *parser2_statement(Parser2 *p) {
  if (parser2_match(p, TOKEN_RETURN)) {
    return parse_return_stmt2(p);
  }

  // Minimal: expression statement
  AstNode *expr = parse_expression2(p);
  if (!expr)
    return NULL;

  // Missing ';' is recoverable when the next token clearly starts a statement.
  parser2_expect_semicolon(p, "Expected ';' after expression");
  AstNode *stmt = alloc_node(AST_STMT_EXPR, expr->loc);
  stmt->data.expr_stmt.expr = expr;
  return stmt;
}

static AstNode *parse_return_stmt2(Parser2 *p) {
  Location loc = prev_loc(p);
  AstNode *ret = alloc_node(AST_STMT_RETURN, loc);

  // Match parser.c: allow bare `return;`
  AstNode *expr = NULL;
  if (!parser2_check(p, TOKEN_SEMICOLON)) {
    expr = parse_expression2(p);
    if (!expr)
      return NULL;
  }

  // Missing ';' is recoverable when the next token clearly starts a statement.
  parser2_expect_semicolon(p, "Expected ';' after return statement");
  ret->data.return_stmt.expr = expr;
  return ret;
}

// ---------- minimal expressions ----------
static AstNode *parse_expression2(Parser2 *p) {
  // Minimal: just primary for now
  return parse_primary2(p);
}

static AstNode *parse_primary2(Parser2 *p) {
  if (parser2_match(p, TOKEN_INT)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_LITERAL_INT, loc);
    n->data.int_lit.value = atoll(p->previous.lexeme);
    return n;
  }

  parser2_handle_error(p, "Expected expression");
  return NULL;
}

// ---------- minimal type expr ----------
static AstNode *parse_type_expression2(Parser2 *p) {
  // Minimal: named types only, including built-in types lexed as distinct
  // tokens
  switch (p->current.type) {
  case TOKEN_IDENTIFIER:
  case TOKEN_INT_TYPE:
  case TOKEN_BOOL_TYPE:
  case TOKEN_STR_TYPE:
  case TOKEN_VOID_TYPE:
  case TOKEN_U8_TYPE:
  case TOKEN_U16_TYPE:
  case TOKEN_U32_TYPE:
  case TOKEN_U64_TYPE:
  case TOKEN_USIZE_TYPE:
  case TOKEN_I8_TYPE:
  case TOKEN_I16_TYPE:
  case TOKEN_I32_TYPE:
  case TOKEN_I64_TYPE:
  case TOKEN_ISIZE_TYPE:
  case TOKEN_CHAR_TYPE: {
    parser2_advance(p);
    Location loc = prev_loc(p);
    AstNode *t = alloc_node(AST_TYPE_NAMED, loc);
    t->data.type_named.name = p->previous.lexeme;
    t->data.type_named.type_args = NULL;
    t->data.type_named.type_arg_count = 0;
    return t;
  }
  default:
    break;
  }

  parser2_handle_error(p, "Expected type");
  return NULL;
}
