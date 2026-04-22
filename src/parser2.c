#include "parser2.h"
#include "alloc.h"
#include "lexer.h"
#include "module.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External allocator
extern Arena long_lived;

typedef enum {
  PARSE_CTX_TOP_LEVEL,
  PARSE_CTX_BLOCK,
} ParseContext;

static AstNode *parse_import_stmt2(Parser2 *p);
static AstNode *parse_variable_decl2(Parser2 *p, bool is_mutable);
static AstNode *parse_type_decl2(Parser2 *p);
static AstNode *parse_extern_decl2(Parser2 *p);

static AstNode *parse_print_stmt2(Parser2 *p);
static AstNode *parse_break_continue_stmt2(Parser2 *p);
static AstNode *parse_defer_stmt2(Parser2 *p);

typedef struct AstNodePtrNode {
  AstNode *node;
  struct AstNodePtrNode *next;
} AstNodePtrNode;

static AstNode *alloc_node(AstKind kind, Location loc) {
  AstNode *n = arena_alloc(&long_lived, sizeof(AstNode));
  memset(n, 0, sizeof(AstNode));
  n->kind = kind;
  n->loc = loc;
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

static bool parser2_follow_stmt_boundary(TokenType t, ParseContext ctx) {
  // FOLLOW-like sets for semicolon insertion.
  // If we're missing a ';' and the next token is in this set, it's safe to
  // "pretend" the semicolon was there and continue.
  switch (ctx) {
  case PARSE_CTX_TOP_LEVEL:
    switch (t) {
    case TOKEN_EOF:
    case TOKEN_FN:
    case TOKEN_VAR:
    case TOKEN_LET:
    case TOKEN_TYPE:
    case TOKEN_IMPORT:
    case TOKEN_EXTERN:
      return true;
    default:
      return false;
    }
  case PARSE_CTX_BLOCK:
    switch (t) {
    case TOKEN_EOF:
    case TOKEN_RBRACE:
    case TOKEN_RETURN:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_FOR:
    case TOKEN_LOOP:
    case TOKEN_LET:
    case TOKEN_VAR:
    case TOKEN_PRINT:
    case TOKEN_SWITCH:
    case TOKEN_DEFER:
    case TOKEN_BREAK:
    case TOKEN_CONTINUE:
    case TOKEN_LBRACE:
      return true;
    default:
      return false;
    }
  default:
    return false;
  }
}

static bool parser2_expect_semicolon(Parser2 *p, ParseContext ctx,
                                     const char *message) {
  if (parser2_match(p, TOKEN_SEMICOLON)) {
    return true;
  }

  parser2_handle_error(p, message);

  // Insert ';' only when the next token is in the FOLLOW-like boundary set.
  // Otherwise, force higher-level recovery (sync) instead of guessing.
  if (parser2_follow_stmt_boundary(p->current.type, ctx)) {
    return true;
  }

  return false;
}

static AstNode *parse_function_decl2(Parser2 *p);
static AstNode *parse_function2(Parser2 *p, Location location, char *name,
                                bool inlined, AstNode *convention,
                                bool require_semicolon_after_arrow);
static AstNode *parse_block_stmt2(Parser2 *p);
static AstNode *parse_return_stmt2(Parser2 *p);
static AstNode *parse_assignment_stmt2(Parser2 *p);

// Expressions (precedence ladder)
static AstNode *parse_expression2(Parser2 *p);
static AstNode *parse_or_expr2(Parser2 *p);
static AstNode *parse_and_expr2(Parser2 *p);
static AstNode *parse_bit_or_expr2(Parser2 *p);
static AstNode *parse_bit_xor_expr2(Parser2 *p);
static AstNode *parse_bit_and_expr2(Parser2 *p);
static AstNode *parse_equality2(Parser2 *p);
static AstNode *parse_comparison2(Parser2 *p);
static AstNode *parse_shift2(Parser2 *p);
static AstNode *parse_cast2(Parser2 *p);
static AstNode *parse_term2(Parser2 *p);
static AstNode *parse_factor2(Parser2 *p);
static AstNode *parse_unary2(Parser2 *p);
static AstNode *parse_postfix2(Parser2 *p);
static AstNode *parse_call2(Parser2 *p, AstNode *func);
static AstNode *parse_index2(Parser2 *p, AstNode *array);
static AstNode *parse_member2(Parser2 *p, AstNode *object);
static AstNode *parse_module_member2(Parser2 *p, AstNode *object);
static AstNode *parse_interpolated_string2(Parser2 *p);
static AstNode *parse_primary2(Parser2 *p);
static bool parser2_is_expr_terminator(TokenType t);

// Type expressions
static AstNode *parse_type_expression2(Parser2 *p);

static void parser2_synchronize_ctx(Parser2 *parser, ParseContext ctx);

void parser_init(Parser2 *parser, const char *source, const char *filename,
                 const char *abs_file_path) {
  lexer_init(&parser->lexer, source, filename);
  parser->abs_file_path = abs_file_path;

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
  // Backwards-compatible entrypoint: default to block context.
  parser2_synchronize_ctx(parser, PARSE_CTX_BLOCK);
}

void parser2_synchronize_ctx(Parser2 *parser, ParseContext ctx) {
  // Advance until a likely boundary for the current parse context.
  // Important: ensure progress even if we're already at EOF.
  if (!parser2_check(parser, TOKEN_EOF)) {
    parser2_advance(parser);
  }

  size_t skips = 0;
  const size_t max_skips = 50;

  while (!parser2_check(parser, TOKEN_EOF) && skips++ < max_skips) {
    if (parser->previous.type == TOKEN_SEMICOLON)
      return;

    // Context-sensitive sync points.
    if (ctx == PARSE_CTX_TOP_LEVEL) {
      switch (parser->current.type) {
      case TOKEN_FN:
      case TOKEN_VAR:
      case TOKEN_LET:
      case TOKEN_TYPE:
      case TOKEN_IMPORT:
      case TOKEN_EXTERN:
      case TOKEN_EOF:
        return;
      default:
        break;
      }
    } else {
      switch (parser->current.type) {
      case TOKEN_RBRACE:
      case TOKEN_RETURN:
      case TOKEN_IF:
      case TOKEN_WHILE:
      case TOKEN_FOR:
      case TOKEN_LOOP:
      case TOKEN_LET:
      case TOKEN_VAR:
      case TOKEN_PRINT:
      case TOKEN_SWITCH:
      case TOKEN_DEFER:
      case TOKEN_BREAK:
      case TOKEN_CONTINUE:
      case TOKEN_LBRACE:
      case TOKEN_EOF:
        return;
      default:
        break;
      }
    }

    parser2_advance(parser);
  }
}

// ---------- top level ----------
AstNode *parse_program(Parser2 *parser) {
  AstNodePtrNode *head = NULL;
  AstNodePtrNode *tail = NULL;
  size_t decl_count = 0;

  while (!parser2_check(parser, TOKEN_EOF)) {
    AstNode *decl = parser2_declaration(parser);
    if (decl) {
      AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
      n->node = decl;
      n->next = NULL;
      if (!head) {
        head = n;
        tail = n;
      } else {
        tail->next = n;
        tail = n;
      }
      decl_count++;
    } else {
      parser2_synchronize_ctx(parser, PARSE_CTX_TOP_LEVEL);
    }

    if (parser->diagnostics->error_count >= parser->max_errors)
      break;
  }

  AstNode **decls = NULL;
  if (decl_count > 0) {
    decls = arena_alloc(&long_lived, decl_count * sizeof(AstNode *));
    size_t i = 0;
    for (AstNodePtrNode *n = head; n; n = n->next) {
      decls[i++] = n->node;
    }
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
    // Lexer owns processed string storage in long_lived arena.
    import_path->data.str_lit.value = path_tok.value.str_val;
  }

  parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                           "Expected ';' after import declaration");

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

  parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                           "Expected ';' after variable declaration");

  if (is_mutable) {
    AstNode *var = alloc_node(AST_DECL_VARIABLE, name.location);
    var->data.var_decl.name = name.lexeme;
    var->data.var_decl.qualified_name = name.lexeme;
    var->data.var_decl.full_qualified_name = NULL;
    var->data.var_decl.type_expr = type_expr;
    var->data.var_decl.init = init;
    return var;
  } else {
    AstNode *c = alloc_node(AST_DECL_CONSTANT, name.location);
    c->data.const_decl.name = name.lexeme;
    c->data.const_decl.qualified_name = name.lexeme;
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

  parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                           "Expected ';' after type declaration");

  AstNode *node = alloc_node(AST_DECL_TYPE, name.location);
  node->data.type_decl.name = name.lexeme;
  node->data.type_decl.qualified_name = name.lexeme;
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
    // TOKEN_STRING lexeme/value storage is lexer-owned (long_lived arena).
    lib_name->data.str_lit.value = p->previous.value.str_val;
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
            p, PARSE_CTX_TOP_LEVEL,
            "Expected ';' after extern function declaration");

        AstNode *func = alloc_node(AST_DECL_EXTERN_FUNC, name.location);
        func->data.extern_func.name = name.lexeme;
        func->data.extern_func.qualified_name = name.lexeme;
        func->data.extern_func.full_qualified_name = NULL;
        func->data.extern_func.params = params;
        func->data.extern_func.param_count = param_count;
        func->data.extern_func.return_type = return_type;
        func->data.extern_func.lib_name = lib_name;

        externs[count++] = func;
      } else if (parser2_match(p, TOKEN_TYPE)) {
        Token name = parser2_consume(p, TOKEN_IDENTIFIER,
                                     "Expected extern function name");
        parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                                 "Expected ';' after extern type declaration");

        AstNode *opaque_type = alloc_node(AST_DECL_EXTERN_TYPE, name.location);
        opaque_type->data.extern_type.name = name.lexeme;
        opaque_type->data.extern_type.qualified_name = name.lexeme;
        opaque_type->data.extern_type.full_qualified_name = NULL;

        externs[count++] = opaque_type;
      } else if (parser2_match(p, TOKEN_LET)) {
        Token name =
            parser2_consume(p, TOKEN_IDENTIFIER, "Expected constant name");
        AstNode *type_expr = parse_type_expression2(p);
        parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                                 "Expected ';' after constant declaration");

        AstNode *let = alloc_node(AST_DECL_EXTERN_CONSTANT, name.location);
        let->data.extern_const_decl.name = name.lexeme;
        let->data.extern_const_decl.qualified_name = name.lexeme;
        let->data.extern_const_decl.full_qualified_name = NULL;
        let->data.extern_const_decl.type_expr = type_expr;
        let->data.extern_const_decl.lib_name = lib_name;

        externs[count++] = let;
      } else if (parser2_match(p, TOKEN_VAR)) {
        Token name =
            parser2_consume(p, TOKEN_IDENTIFIER, "Expected variable name");
        AstNode *type_expr = parse_type_expression2(p);
        parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                                 "Expected ';' after variable declaration");

        AstNode *var = alloc_node(AST_DECL_EXTERN_VARIABLE, name.location);
        var->data.extern_var_decl.name = name.lexeme;
        var->data.extern_var_decl.qualified_name = name.lexeme;
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
    parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                             "Expected ';' after extern function declaration");

    AstNode *func = alloc_node(AST_DECL_EXTERN_FUNC, name.location);
    func->data.extern_func.name = name.lexeme;
    func->data.extern_func.qualified_name = name.lexeme;
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
    parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                             "Expected ';' after extern type declaration");

    AstNode *opaque_type = alloc_node(AST_DECL_EXTERN_TYPE, name.location);
    opaque_type->data.extern_type.name = name.lexeme;
    opaque_type->data.extern_type.qualified_name = name.lexeme;
    opaque_type->data.extern_type.full_qualified_name = NULL;
    return opaque_type;
  }

  if (parser2_match(p, TOKEN_LET)) {
    Token name = parser2_consume(p, TOKEN_IDENTIFIER, "Expected constant name");
    AstNode *type_expr = parse_type_expression2(p);
    parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                             "Expected ';' after constant declaration");

    AstNode *let = alloc_node(AST_DECL_EXTERN_CONSTANT, name.location);
    let->data.extern_const_decl.name = name.lexeme;
    let->data.extern_const_decl.qualified_name = name.lexeme;
    let->data.extern_const_decl.full_qualified_name = NULL;
    let->data.extern_const_decl.type_expr = type_expr;
    let->data.extern_const_decl.lib_name = lib_name;
    return let;
  }

  if (parser2_match(p, TOKEN_VAR)) {
    Token name = parser2_consume(p, TOKEN_IDENTIFIER, "Expected variable name");
    AstNode *type_expr = parse_type_expression2(p);
    parser2_expect_semicolon(p, PARSE_CTX_TOP_LEVEL,
                             "Expected ';' after variable declaration");

    AstNode *var = alloc_node(AST_DECL_EXTERN_VARIABLE, name.location);
    var->data.extern_var_decl.name = name.lexeme;
    var->data.extern_var_decl.qualified_name = name.lexeme;
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

  bool inlined = parser2_match(p, TOKEN_INLINE);

  AstNode *convention = NULL;
  if (parser2_match(p, TOKEN_STRING)) {
    convention = alloc_node(AST_EXPR_LITERAL_STRING, prev_loc(p));
    // Use raw lexeme to match parser.c behavior.
    convention->data.str_lit.value = p->previous.lexeme;
  }

  Token name_tok =
      parser2_consume(p, TOKEN_IDENTIFIER, "Expected function name");
  if (name_tok.type != TOKEN_IDENTIFIER)
    return NULL;

  return parse_function2(p, name_tok.location, name_tok.lexeme, inlined,
                         convention, true);
}

static AstNode *parse_function2(Parser2 *p, Location location, char *name,
                                bool inlined, AstNode *convention,
                                bool require_semicolon_after_arrow) {
  // Handle generic parameters: [T, U]
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

  // (params) return_type { body } or => expr
  parser2_consume(p, TOKEN_LPAREN, "Expected '(' after function name");

  typedef struct FuncParamNode {
    FuncParam param;
    struct FuncParamNode *next;
  } FuncParamNode;

  FuncParamNode *param_head = NULL;
  FuncParamNode *param_tail = NULL;
  size_t param_count = 0;

  if (!parser2_check(p, TOKEN_RPAREN)) {
    while (true) {
      bool is_variadic = parser2_match(p, TOKEN_ELLIPSIS);

      Token param_name =
          parser2_consume(p, TOKEN_IDENTIFIER, "Expected parameter name");
      if (param_name.type != TOKEN_IDENTIFIER)
        return NULL;

      if (parser2_check(p, TOKEN_COMMA)) {
        FuncParamNode *group_start = NULL;
        FuncParamNode *group_end = NULL;

        FuncParamNode *node = arena_alloc(&long_lived, sizeof(FuncParamNode));
        node->param.name = param_name.lexeme;
        node->param.type = NULL;
        node->param.is_variadic = false;
        node->next = NULL;

        if (!param_head) {
          param_head = node;
          param_tail = node;
        } else {
          param_tail->next = node;
          param_tail = node;
        }
        group_start = node;
        group_end = node;
        param_count++;

        while (parser2_match(p, TOKEN_COMMA)) {
          if (!parser2_check(p, TOKEN_IDENTIFIER)) {
            parser2_handle_error(p, "Expected parameter name");
            break;
          }

          Token next_name =
              parser2_consume(p, TOKEN_IDENTIFIER, "Expected parameter name");

          FuncParamNode *n = arena_alloc(&long_lived, sizeof(FuncParamNode));
          n->param.name = next_name.lexeme;
          n->param.type = NULL;
          n->param.is_variadic = false;
          n->next = NULL;

          param_tail->next = n;
          param_tail = n;
          group_end = n;
          param_count++;
        }

        AstNode *param_type = parse_type_expression2(p);
        if (!param_type)
          return NULL;

        for (FuncParamNode *n = group_start;; n = n->next) {
          n->param.type = param_type;
          n->param.is_variadic = false;
          if (n == group_end)
            break;
        }
      } else {
        AstNode *param_type = parse_type_expression2(p);
        if (!param_type)
          return NULL;

        FuncParamNode *node = arena_alloc(&long_lived, sizeof(FuncParamNode));
        node->param.name = param_name.lexeme;
        node->param.type = param_type;
        node->param.is_variadic = is_variadic;
        node->next = NULL;

        if (!param_head) {
          param_head = node;
          param_tail = node;
        } else {
          param_tail->next = node;
          param_tail = node;
        }
        param_count++;
      }

      if (!parser2_match(p, TOKEN_COMMA)) {
        break;
      }

      if (parser2_check(p, TOKEN_RPAREN)) {
        break;
      }
    }
  }

  parser2_consume(p, TOKEN_RPAREN, "Expected ')' after parameters");

  AstNode *return_type = parse_type_expression2(p);
  if (!return_type)
    return NULL;

  AstNode *body = NULL;
  if (parser2_match(p, TOKEN_FAT_ARROW)) {
    AstNode *expr = parse_expression2(p);
    if (!expr) {
      // Local recovery: avoid dropping the whole function on a bad arrow body.
      if (require_semicolon_after_arrow) {
        while (!parser2_check(p, TOKEN_SEMICOLON) &&
               !parser2_check(p, TOKEN_EOF) &&
               !parser2_follow_stmt_boundary(p->current.type,
                                             PARSE_CTX_TOP_LEVEL)) {
          parser2_advance(p);
        }
        if (!parser2_match(p, TOKEN_SEMICOLON)) {
          parser2_handle_error(p,
                               "Expected ';' after expression-bodied function");
        }
      } else {
        while (!parser2_is_expr_terminator(p->current.type) &&
               !parser2_check(p, TOKEN_EOF)) {
          parser2_advance(p);
        }
      }

      body = alloc_node(AST_STMT_BLOCK, location);
      body->data.block_stmt.stmts = NULL;
      body->data.block_stmt.stmt_count = 0;
    } else {
      if (require_semicolon_after_arrow) {
        if (!parser2_match(p, TOKEN_SEMICOLON)) {
          parser2_handle_error(p,
                               "Expected ';' after expression-bodied function");
        }
      }

      AstNode *ret_stmt = alloc_node(AST_STMT_RETURN, expr->loc);
      ret_stmt->data.return_stmt.expr = expr;

      AstNode **stmts = arena_alloc(&long_lived, sizeof(AstNode *));
      stmts[0] = ret_stmt;
      body = alloc_node(AST_STMT_BLOCK, expr->loc);
      body->data.block_stmt.stmts = stmts;
      body->data.block_stmt.stmt_count = 1;
    }
  } else {
    parser2_consume(p, TOKEN_LBRACE, "Expected '{' before function body");
    body = parse_block_stmt2(p);
    if (!body)
      return NULL;
  }

  FuncParam *params = NULL;
  if (param_count > 0) {
    params = arena_alloc(&long_lived, param_count * sizeof(FuncParam));
    size_t i = 0;
    for (FuncParamNode *n = param_head; n; n = n->next) {
      params[i++] = n->param;
    }
  }

  AstNode *func = NULL;

  if (name) {
    func = alloc_node(AST_DECL_FUNCTION, location);
    func->data.func_decl.name = name;
    func->data.func_decl.qualified_name = name;
    func->data.func_decl.full_qualified_name = NULL;
    func->data.func_decl.params = params;
    func->data.func_decl.param_count = param_count;
    func->data.func_decl.return_type = return_type;
    func->data.func_decl.body = body;
    func->data.func_decl.inlined = inlined;
    func->data.func_decl.convention = convention;
    func->data.func_decl.type_params = type_params;
    func->data.func_decl.type_param_count = type_param_count;
  } else {
    func = alloc_node(AST_EXPR_FUNCTION, location);
    func->data.func_expr.params = params;
    func->data.func_expr.param_count = param_count;
    func->data.func_expr.return_type = return_type;
    func->data.func_expr.body = body;
    func->data.func_expr.inlined = inlined;
    func->data.func_expr.convention = convention;
    func->data.func_expr.type_params = type_params;
    func->data.func_expr.type_param_count = type_param_count;
    func->data.func_expr.symbol = NULL;
  }

  return func;
}

static AstNode *parse_block_stmt2(Parser2 *p) {
  // Assumes '{' already consumed.
  Location loc = prev_loc(p);
  AstNode *block = alloc_node(AST_STMT_BLOCK, loc);

  AstNodePtrNode *head = NULL;
  AstNodePtrNode *tail = NULL;
  size_t stmt_count = 0;

  while (!parser2_check(p, TOKEN_RBRACE) && !parser2_check(p, TOKEN_EOF)) {
    AstNode *s = parser2_statement(p);
    if (s) {
      AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
      n->node = s;
      n->next = NULL;
      if (!head) {
        head = n;
        tail = n;
      } else {
        tail->next = n;
        tail = n;
      }
      stmt_count++;
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

  AstNode **stmts = NULL;
  if (stmt_count > 0) {
    stmts = arena_alloc(&long_lived, stmt_count * sizeof(AstNode *));
    size_t i = 0;
    for (AstNodePtrNode *n = head; n; n = n->next) {
      stmts[i++] = n->node;
    }
  }

  block->data.block_stmt.stmts = stmts;
  block->data.block_stmt.stmt_count = stmt_count;
  return block;
}

static AstNode *parse_if_stmt2(Parser2 *p);
static AstNode *parse_while_stmt2(Parser2 *p);
static AstNode *parse_loop_stmt2(Parser2 *p);
static AstNode *parse_for_stmt2(Parser2 *p);
static AstNode *parse_switch_stmt2(Parser2 *p);

AstNode *parser2_statement(Parser2 *p) {
  if (parser2_match(p, TOKEN_RETURN)) {
    return parse_return_stmt2(p);
  }

  if (parser2_match(p, TOKEN_IF)) {
    return parse_if_stmt2(p);
  }

  if (parser2_match(p, TOKEN_WHILE)) {
    return parse_while_stmt2(p);
  }

  if (parser2_match(p, TOKEN_LOOP)) {
    return parse_loop_stmt2(p);
  }

  if (parser2_match(p, TOKEN_FOR)) {
    return parse_for_stmt2(p);
  }

  if (parser2_match(p, TOKEN_LBRACE)) {
    return parse_block_stmt2(p);
  }

  if (parser2_match(p, TOKEN_LET)) {
    return parse_variable_decl2(p, false);
  }

  if (parser2_match(p, TOKEN_VAR)) {
    return parse_variable_decl2(p, true);
  }

  if (parser2_match(p, TOKEN_PRINT)) {
    return parse_print_stmt2(p);
  }

  if (parser2_match(p, TOKEN_BREAK) || parser2_match(p, TOKEN_CONTINUE)) {
    return parse_break_continue_stmt2(p);
  }

  if (parser2_match(p, TOKEN_SWITCH)) {
    return parse_switch_stmt2(p);
  }

  if (parser2_match(p, TOKEN_DEFER)) {
    return parse_defer_stmt2(p);
  }

  // Fallback: assignment or expression statement
  return parse_assignment_stmt2(p);
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
  parser2_expect_semicolon(p, PARSE_CTX_BLOCK,
                           "Expected ';' after return statement");
  ret->data.return_stmt.expr = expr;
  return ret;
}

static AstNode *parse_if_stmt2(Parser2 *p) {
  Location loc = prev_loc(p);

  AstNode *cond = parse_expression2(p);
  AstNode *then_branch = parser2_statement(p);

  AstNode *else_branch = NULL;
  if (parser2_match(p, TOKEN_ELSE)) {
    else_branch = parser2_statement(p);
  }

  AstNode *stmt = alloc_node(AST_STMT_IF, loc);
  stmt->data.if_stmt.cond = cond;
  stmt->data.if_stmt.then_branch = then_branch;
  stmt->data.if_stmt.else_branch = else_branch;
  return stmt;
}

static AstNode *parse_while_stmt2(Parser2 *p) {
  Location loc = prev_loc(p);

  AstNode *cond = parse_expression2(p);
  AstNode *body = parser2_statement(p);

  AstNode *stmt = alloc_node(AST_STMT_WHILE, loc);
  stmt->data.while_stmt.cond = cond;
  stmt->data.while_stmt.body = body;
  return stmt;
}

static AstNode *parse_loop_stmt2(Parser2 *p) {
  Location loc = prev_loc(p);

  AstNode *start = parse_expression2(p);

  bool inclusive = false;
  if (parser2_match(p, TOKEN_DOTDOTEQ)) {
    inclusive = true;
  } else if (parser2_match(p, TOKEN_DOTDOT)) {
    inclusive = false;
  } else {
    parser2_handle_error(p, "Expected '..' or '..=' in loop range");
    return NULL;
  }

  AstNode *end = parse_expression2(p);

  AstNode *iterator_name = NULL;
  if (parser2_match(p, TOKEN_COLON)) {
    iterator_name = parse_primary2(p);
    if (iterator_name && iterator_name->kind != AST_EXPR_IDENTIFIER) {
      parser2_handle_error(
          p, "Expected an identifier to name the iterator of the loop.");
    }
  }

  AstNode *body = parser2_statement(p);

  AstNode *stmt = alloc_node(AST_STMT_LOOP, loc);
  stmt->data.loop_stmt.start = start;
  stmt->data.loop_stmt.end = end;
  stmt->data.loop_stmt.inclusive = inclusive;
  stmt->data.loop_stmt.iterator_name = iterator_name;
  stmt->data.loop_stmt.body = body;
  return stmt;
}

static AstNode *parse_for_stmt2(Parser2 *p) {
  Location loc = prev_loc(p);

  AstNode *init = NULL;
  if (parser2_match(p, TOKEN_VAR)) {
    init = parse_variable_decl2(p, true);
  } else {
    AstNode *init_lhs = parse_expression2(p);
    parser2_consume(p, TOKEN_EQUAL, "Expected '=' in for loop init");
    AstNode *init_rhs = parse_expression2(p);
    parser2_consume(p, TOKEN_SEMICOLON, "Expected ';' after for loop init");
    if (!init_lhs || !init_rhs) {
      return NULL;
    }

    init = alloc_node(AST_STMT_ASSIGN, init_lhs->loc);
    init->data.assign_stmt.op = -1;
    init->data.assign_stmt.lhs = init_lhs;
    init->data.assign_stmt.rhs = init_rhs;
  }

  AstNode *cond = parse_expression2(p);
  parser2_consume(p, TOKEN_SEMICOLON, "Expected ';' after for loop condition");

  AstNode *lhs = parse_expression2(p);
  AstNode *update = lhs;

  if (lhs && lhs->kind != AST_EXPR_POSTFIX_INC &&
      lhs->kind != AST_EXPR_POSTFIX_DEC) {
    parser2_consume(p, TOKEN_EQUAL, "Expected '=' in for loop update");
    AstNode *rhs = parse_expression2(p);

    if (!lhs || !rhs) {
      return NULL;
    }

    update = alloc_node(AST_STMT_ASSIGN, lhs->loc);
    update->data.assign_stmt.op = -1;
    update->data.assign_stmt.lhs = lhs;
    update->data.assign_stmt.rhs = rhs;
  }

  AstNode *body = parser2_statement(p);

  AstNode *stmt = alloc_node(AST_STMT_FOR, loc);
  stmt->data.for_stmt.init = init;
  stmt->data.for_stmt.cond = cond;
  stmt->data.for_stmt.update = update;
  stmt->data.for_stmt.body = body;
  return stmt;
}

static AstNode *parse_switch_stmt2(Parser2 *p) {
  Location loc = prev_loc(p);

  AstNode *stmt = alloc_node(AST_STMT_SWITCH, loc);
  stmt->data.switch_stmt.condition = parse_expression2(p);

  parser2_consume(p, TOKEN_LBRACE, "Expect '{' after switch condition");

  AstNodePtrNode *case_head = NULL;
  AstNodePtrNode *case_tail = NULL;
  size_t case_count = 0;

  while (parser2_match(p, TOKEN_CASE)) {
    AstNode *_case = alloc_node(AST_STMT_CASE, prev_loc(p));
    _case->data.case_stmt.switch_stmt = stmt;
    _case->data.case_stmt.condition = parse_expression2(p);

    AstNodePtrNode *alt_head = NULL;
    AstNodePtrNode *alt_tail = NULL;
    size_t alt_count = 0;

    if (parser2_match(p, TOKEN_COMMA)) {
      while (true) {
        AstNode *alt_case = alloc_node(AST_STMT_CASE, prev_loc(p));
        alt_case->data.case_stmt.switch_stmt = stmt;
        alt_case->data.case_stmt.condition = parse_expression2(p);
        alt_case->data.case_stmt.body = NULL;
        alt_case->data.case_stmt.alt_conditions = NULL;
        alt_case->data.case_stmt.alt_condition_count = 0;

        AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
        n->node = alt_case;
        n->next = NULL;
        if (!alt_head) {
          alt_head = n;
          alt_tail = n;
        } else {
          alt_tail->next = n;
          alt_tail = n;
        }
        alt_count++;

        if (!parser2_match(p, TOKEN_COMMA)) {
          break;
        }
      }
    }

    if (alt_count > 0) {
      AstNode **alt_cases =
          arena_alloc(&long_lived, alt_count * sizeof(AstNode *));
      size_t i = 0;
      for (AstNodePtrNode *n = alt_head; n; n = n->next) {
        alt_cases[i++] = n->node;
      }
      _case->data.case_stmt.alt_conditions = alt_cases;
      _case->data.case_stmt.alt_condition_count = alt_count;
    } else {
      _case->data.case_stmt.alt_conditions = NULL;
      _case->data.case_stmt.alt_condition_count = 0;
    }

    parser2_consume(p, TOKEN_COLON, "Expect ':' after switch case condition");
    _case->data.case_stmt.body = parser2_statement(p);

    AstNodePtrNode *cn = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
    cn->node = _case;
    cn->next = NULL;
    if (!case_head) {
      case_head = cn;
      case_tail = cn;
    } else {
      case_tail->next = cn;
      case_tail = cn;
    }
    case_count++;
  }

  AstNode **cases = NULL;
  if (case_count > 0) {
    cases = arena_alloc(&long_lived, case_count * sizeof(AstNode *));
    size_t i = 0;
    for (AstNodePtrNode *n = case_head; n; n = n->next) {
      cases[i++] = n->node;
    }
  }

  stmt->data.switch_stmt.cases = cases;
  stmt->data.switch_stmt.case_count = case_count;

  if (parser2_match(p, TOKEN_ELSE)) {
    parser2_consume(p, TOKEN_COLON, "Expect ':' after switch case else");
    stmt->data.switch_stmt.default_case = parser2_statement(p);
  }

  parser2_consume(p, TOKEN_RBRACE, "Expect '}' after switch cases");

  return stmt;
}

static AstNode *parse_assignment_stmt2(Parser2 *p) {
  AstNode *lhs = parse_expression2(p);
  if (!lhs)
    return NULL;

  TokenType compound_op = TOKEN_EOF;
  BinaryOp binop = BINOP_ADD;

  if (parser2_match(p, TOKEN_PLUS_EQUAL)) {
    compound_op = TOKEN_PLUS_EQUAL;
    binop = BINOP_ADD;
  } else if (parser2_match(p, TOKEN_MINUS_EQUAL)) {
    compound_op = TOKEN_MINUS_EQUAL;
    binop = BINOP_SUB;
  } else if (parser2_match(p, TOKEN_STAR_EQUAL)) {
    compound_op = TOKEN_STAR_EQUAL;
    binop = BINOP_MUL;
  } else if (parser2_match(p, TOKEN_SLASH_EQUAL)) {
    compound_op = TOKEN_SLASH_EQUAL;
    binop = BINOP_DIV;
  } else if (parser2_match(p, TOKEN_PERCENT_EQUAL)) {
    compound_op = TOKEN_PERCENT_EQUAL;
    binop = BINOP_MOD;
  }

  if (compound_op != TOKEN_EOF) {
    Location loc = prev_loc(p);
    AstNode *rhs = parse_expression2(p);
    if (!rhs)
      return NULL;

    parser2_expect_semicolon(p, PARSE_CTX_BLOCK,
                             "Expected ';' after compound assignment");

    AstNode *assign = alloc_node(AST_STMT_ASSIGN, loc);
    assign->data.assign_stmt.op = binop;
    assign->data.assign_stmt.lhs = lhs;
    assign->data.assign_stmt.rhs = rhs;
    return assign;
  }

  if (parser2_match(p, TOKEN_EQUAL)) {
    Location loc = prev_loc(p);
    AstNode *rhs = parse_expression2(p);
    if (!rhs)
      return NULL;

    parser2_expect_semicolon(p, PARSE_CTX_BLOCK,
                             "Expected ';' after assignment");

    AstNode *assign = alloc_node(AST_STMT_ASSIGN, loc);
    assign->data.assign_stmt.op = -1;
    assign->data.assign_stmt.lhs = lhs;
    assign->data.assign_stmt.rhs = rhs;
    return assign;
  }

  parser2_expect_semicolon(p, PARSE_CTX_BLOCK, "Expected ';' after expression");
  AstNode *expr_stmt = alloc_node(AST_STMT_EXPR, lhs->loc);
  expr_stmt->data.expr_stmt.expr = lhs;
  return expr_stmt;
}

// ---------- expressions (precedence ladder) ----------
static AstNode *parse_print_stmt2(Parser2 *p) {
  // print expr (, expr)* ;
  Location loc = prev_loc(p); // 'print'
  AstNodePtrNode *head = NULL;
  AstNodePtrNode *tail = NULL;
  size_t count = 0;

  AstNode *first = parse_expression2(p);
  if (!first)
    return NULL;

  AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
  n->node = first;
  n->next = NULL;
  head = n;
  tail = n;
  count = 1;

  while (parser2_match(p, TOKEN_COMMA)) {
    AstNode *e = parse_expression2(p);
    if (!e) {
      // Recover: stop consuming more expressions; caller/container will sync.
      break;
    }

    AstNodePtrNode *nn = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
    nn->node = e;
    nn->next = NULL;
    tail->next = nn;
    tail = nn;
    count++;
  }

  parser2_expect_semicolon(p, PARSE_CTX_BLOCK,
                           "Expected ';' after print statement.");

  AstNode **exprs = NULL;
  if (count > 0) {
    exprs = arena_alloc(&long_lived, count * sizeof(AstNode *));
    size_t i = 0;
    for (AstNodePtrNode *cur = head; cur; cur = cur->next) {
      exprs[i++] = cur->node;
    }
  }

  AstNode *node = alloc_node(AST_STMT_PRINT, loc);
  node->data.print_stmt.exprs = exprs;
  node->data.print_stmt.expr_count = count;
  return node;
}

static AstNode *parse_break_continue_stmt2(Parser2 *p) {
  // break;
  // continue;
  Location loc = prev_loc(p); // break/continue token
  bool is_break = (p->previous.type == TOKEN_BREAK);

  parser2_expect_semicolon(p, PARSE_CTX_BLOCK,
                           "Expected ';' after control flow jump statement.");

  AstNode *node =
      alloc_node(is_break ? AST_STMT_BREAK : AST_STMT_CONTINUE, loc);
  return node;
}

static AstNode *parse_defer_stmt2(Parser2 *p) {
  Location loc = prev_loc(p); // 'defer'

  AstNode *stmt = alloc_node(AST_STMT_DEFER, loc);
  stmt->data.defer_stmt.stmt = parser2_statement(p);
  if (!stmt->data.defer_stmt.stmt) {
    return NULL;
  }
  return stmt;
}

// ---------- expressions (precedence ladder) ----------

// TODO(parser2): Re-evaluate whether this helper is pulling its weight once
// postfix parsing and expression-level recovery are fully ported (100% parser2
// parity). Right now it's primarily a conservative stop-set to avoid consuming
// tokens we don't yet handle in the postfix chain.
static bool parser2_is_expr_terminator(TokenType t) {
  switch (t) {
  case TOKEN_SEMICOLON:
  case TOKEN_COMMA:
  case TOKEN_RPAREN:
  case TOKEN_RBRACKET:
  case TOKEN_RBRACE:
  case TOKEN_COLON:
  case TOKEN_EOF:
    return true;
  default:
    return false;
  }
}

static AstNode *parse_expression2(Parser2 *p) { return parse_or_expr2(p); }

static AstNode *parse_or_expr2(Parser2 *p) {
  AstNode *left = parse_and_expr2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_OR)) {
    Token op = p->previous;
    AstNode *right = parse_and_expr2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after '||'");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_and_expr2(Parser2 *p) {
  AstNode *left = parse_bit_or_expr2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_AND)) {
    Token op = p->previous;
    AstNode *right = parse_bit_or_expr2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after '&&'");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_bit_or_expr2(Parser2 *p) {
  AstNode *left = parse_bit_xor_expr2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_PIPE)) {
    Token op = p->previous;
    AstNode *right = parse_bit_xor_expr2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after '|'");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_bit_xor_expr2(Parser2 *p) {
  AstNode *left = parse_bit_and_expr2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_CARET)) {
    Token op = p->previous;
    AstNode *right = parse_bit_and_expr2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after '^'");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_bit_and_expr2(Parser2 *p) {
  AstNode *left = parse_equality2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_AMPERSAND)) {
    Token op = p->previous;
    AstNode *right = parse_equality2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after '&'");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_equality2(Parser2 *p) {
  AstNode *left = parse_comparison2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_EQ) || parser2_match(p, TOKEN_NE)) {
    Token op = p->previous;
    AstNode *right = parse_comparison2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after equality operator");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_comparison2(Parser2 *p) {
  AstNode *left = parse_shift2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_LT) || parser2_match(p, TOKEN_LE) ||
         parser2_match(p, TOKEN_GT) || parser2_match(p, TOKEN_GE)) {
    Token op = p->previous;
    AstNode *right = parse_shift2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after comparison operator");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_shift2(Parser2 *p) {
  AstNode *left = parse_cast2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_LSHIFT) || parser2_match(p, TOKEN_RSHIFT)) {
    Token op = p->previous;
    AstNode *right = parse_cast2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after shift operator");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_cast2(Parser2 *p) {
  AstNode *left = parse_term2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_AS)) {
    Token as_tok = p->previous;
    AstNode *target_type = parse_type_expression2(p);
    if (!target_type) {
      parser2_handle_error(p, "Expected type after 'as'");
      return left;
    }

    AstNode *cast = alloc_node(AST_EXPR_EXPLICIT_CAST, as_tok.location);
    cast->data.explicit_cast.expr = left;
    cast->data.explicit_cast.target_type = target_type;
    cast->data.explicit_cast.pointer_cast = false;
    left = cast;
  }

  return left;
}

static AstNode *parse_term2(Parser2 *p) {
  AstNode *left = parse_factor2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_PLUS) || parser2_match(p, TOKEN_MINUS)) {
    Token op = p->previous;
    AstNode *right = parse_factor2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after '+' or '-'");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_factor2(Parser2 *p) {
  AstNode *left = parse_unary2(p);
  if (!left)
    return NULL;

  while (parser2_match(p, TOKEN_STAR) || parser2_match(p, TOKEN_SLASH) ||
         parser2_match(p, TOKEN_PERCENT)) {
    Token op = p->previous;
    AstNode *right = parse_unary2(p);
    if (!right) {
      parser2_handle_error(p, "Expected expression after '*', '/' or '%'");
      return left;
    }

    AstNode *binop = alloc_node(AST_EXPR_BINARY_OP, op.location);
    binop->data.binop.op = ast_binop_from_token(op.type);
    binop->data.binop.left = left;
    binop->data.binop.right = right;
    left = binop;
  }

  return left;
}

static AstNode *parse_unary2(Parser2 *p) {
  if (parser2_match(p, TOKEN_MINUS) || parser2_match(p, TOKEN_NOT) ||
      parser2_match(p, TOKEN_AMPERSAND) || parser2_match(p, TOKEN_STAR) ||
      parser2_match(p, TOKEN_TILDE)) {
    Token op = p->previous;
    AstNode *operand = parse_unary2(p);
    if (!operand) {
      parser2_handle_error(p, "Expected expression after unary operator");
      return NULL;
    }

    AstNode *unop = alloc_node(AST_EXPR_UNARY_OP, op.location);
    unop->data.unop.op = ast_unop_from_token(op.type);
    unop->data.unop.operand = operand;
    return unop;
  }

  return parse_postfix2(p);
}

static AstNode *parse_postfix2(Parser2 *p) {
  AstNode *expr = parse_primary2(p);
  if (!expr)
    return NULL;

  while (true) {
    // Force unwrap: expr!
    if (parser2_match(p, TOKEN_NOT)) {
      Token bang = p->previous;
      AstNode *n = alloc_node(AST_EXPR_FORCE_UNWRAP, bang.location);
      n->data.force_unwrap.operand = expr;
      expr = n;
      continue;
    }

    // Postfix inc/dec: expr++ / expr--
    if (parser2_match(p, TOKEN_PLUS_PLUS)) {
      Token tok = p->previous;
      AstNode *n = alloc_node(AST_EXPR_POSTFIX_INC, tok.location);
      n->data.postfix_inc.operand = expr;
      expr = n;
      continue;
    }

    if (parser2_match(p, TOKEN_MINUS_MINUS)) {
      Token tok = p->previous;
      AstNode *n = alloc_node(AST_EXPR_POSTFIX_DEC, tok.location);
      n->data.postfix_dec.operand = expr;
      expr = n;
      continue;
    }

    // Call: expr(...)
    if (parser2_match(p, TOKEN_LPAREN)) {
      expr = parse_call2(p, expr);
      if (!expr)
        return NULL;
      continue;
    }

    // Index or slice: expr[...]
    if (parser2_match(p, TOKEN_LBRACKET)) {
      expr = parse_index2(p, expr);
      if (!expr)
        return NULL;
      continue;
    }

    // Stop if next token clearly can't continue an expression.
    if (parser2_is_expr_terminator(p->current.type)) {
      break;
    }

    // Member access: expr.member or expr.0
    if (parser2_match(p, TOKEN_DOT)) {
      // Generic postfix: expr.[T, U](...), expr.[T]{...}, expr.[T].member
      if (parser2_match(p, TOKEN_LBRACKET)) {
        AstNode *base = expr;

        typedef struct TypeArgNode {
          AstNode *arg;
          struct TypeArgNode *next;
        } TypeArgNode;

        TypeArgNode *arg_head = NULL;
        TypeArgNode *arg_tail = NULL;
        size_t type_arg_count = 0;

        if (!parser2_check(p, TOKEN_RBRACKET)) {
          while (true) {
            AstNode *arg = parse_type_expression2(p);
            if (!arg) {
              parser2_handle_error(p, "Expected type argument");
              while (!parser2_check(p, TOKEN_COMMA) &&
                     !parser2_check(p, TOKEN_RBRACKET) &&
                     !parser2_check(p, TOKEN_EOF)) {
                parser2_advance(p);
              }
              if (parser2_match(p, TOKEN_COMMA)) {
                if (parser2_check(p, TOKEN_RBRACKET)) {
                  break;
                }
                continue;
              }
              break;
            }

            TypeArgNode *n = arena_alloc(&long_lived, sizeof(TypeArgNode));
            n->arg = arg;
            n->next = NULL;
            if (!arg_head) {
              arg_head = n;
              arg_tail = n;
            } else {
              arg_tail->next = n;
              arg_tail = n;
            }
            type_arg_count++;

            if (!parser2_match(p, TOKEN_COMMA)) {
              break;
            }

            if (parser2_check(p, TOKEN_RBRACKET)) {
              break;
            }
          }
        }

        parser2_consume(p, TOKEN_RBRACKET, "Expected ']' after type arguments");

        AstNode **type_args = NULL;
        if (type_arg_count > 0) {
          type_args =
              arena_alloc(&long_lived, type_arg_count * sizeof(AstNode *));
          size_t i = 0;
          for (TypeArgNode *n = arg_head; n; n = n->next) {
            type_args[i++] = n->arg;
          }
        }

        if (parser2_match(p, TOKEN_LPAREN)) {
          expr = parse_call2(p, expr);
          if (!expr)
            return NULL;
          expr->data.call.type_args = type_args;
          expr->data.call.type_arg_count = type_arg_count;
          continue;
        }

        if (parser2_match(p, TOKEN_LBRACE)) {
          Location loc = expr->loc;
          char *type_name = NULL;

          if (expr->kind == AST_EXPR_IDENTIFIER) {
            type_name = expr->data.ident.name;
          } else {
            char *prefix = prepend(
                expr->data.mod_member_expr.module->data.ident.name, "__");
            type_name = prepend(prefix, expr->data.mod_member_expr.member);
          }

          typedef struct FieldNode {
            char *name;
            AstNode *value;
            struct FieldNode *next;
          } FieldNode;

          FieldNode *field_head = NULL;
          FieldNode *field_tail = NULL;
          size_t field_count = 0;

          if (!parser2_check(p, TOKEN_RBRACE)) {
            while (true) {
              if (parser2_check(p, TOKEN_RBRACE)) {
                break;
              }

              if (!parser2_check(p, TOKEN_IDENTIFIER)) {
                parser2_handle_error(p,
                                     "Expected field name in struct literal");
                while (!parser2_check(p, TOKEN_COMMA) &&
                       !parser2_check(p, TOKEN_RBRACE) &&
                       !parser2_check(p, TOKEN_EOF)) {
                  parser2_advance(p);
                }
                if (parser2_match(p, TOKEN_COMMA)) {
                  continue;
                }
                break;
              }

              Token field_name = parser2_consume(
                  p, TOKEN_IDENTIFIER, "Expected field name in struct literal");

              parser2_consume(p, TOKEN_EQUAL, "Expected '=' after field name");

              AstNode *value = parse_expression2(p);
              if (!value) {
                while (!parser2_check(p, TOKEN_COMMA) &&
                       !parser2_check(p, TOKEN_RBRACE) &&
                       !parser2_check(p, TOKEN_EOF)) {
                  parser2_advance(p);
                }
                if (parser2_match(p, TOKEN_COMMA)) {
                  continue;
                }
                break;
              }

              FieldNode *fn = arena_alloc(&long_lived, sizeof(FieldNode));
              fn->name = field_name.lexeme;
              fn->value = value;
              fn->next = NULL;
              if (!field_head) {
                field_head = fn;
                field_tail = fn;
              } else {
                field_tail->next = fn;
                field_tail = fn;
              }
              field_count++;

              if (!parser2_match(p, TOKEN_COMMA)) {
                break;
              }

              if (parser2_check(p, TOKEN_RBRACE)) {
                break;
              }
            }
          }

          parser2_consume(p, TOKEN_RBRACE,
                          "Expected '}' after struct literal fields");

          char **field_names = NULL;
          AstNode **field_values = NULL;
          if (field_count > 0) {
            field_names =
                arena_alloc(&long_lived, field_count * sizeof(char *));
            field_values =
                arena_alloc(&long_lived, field_count * sizeof(AstNode *));
            size_t i = 0;
            for (FieldNode *n = field_head; n; n = n->next) {
              field_names[i] = n->name;
              field_values[i] = n->value;
              i++;
            }
          }

          AstNode *lit = alloc_node(AST_EXPR_STRUCT_LITERAL, loc);
          lit->data.struct_literal.type_name = type_name;
          lit->data.struct_literal.qualified_type_name = type_name;
          lit->data.struct_literal.field_names = field_names;
          lit->data.struct_literal.field_values = field_values;
          lit->data.struct_literal.field_count = field_count;
          lit->data.struct_literal.type_args = type_args;
          lit->data.struct_literal.type_arg_count = type_arg_count;
          expr = lit;
          return expr;
        }

        if (parser2_match(p, TOKEN_DOT)) {
          Token member = parser2_consume(
              p, TOKEN_IDENTIFIER, "Expected member name after generic type");

          AstNode *member_expr = alloc_node(AST_EXPR_MEMBER, expr->loc);
          member_expr->data.member_expr.object = expr;
          member_expr->data.member_expr.member = member.lexeme;
          member_expr->data.member_expr.is_method_ref = false;
          member_expr->data.member_expr.method_qualified_name = NULL;
          member_expr->data.member_expr.is_associated_function = false;
          member_expr->data.member_expr.type_args = type_args;
          member_expr->data.member_expr.type_arg_count = type_arg_count;

          expr = member_expr;
          continue;
        }

        parser2_handle_error(p,
                             "Expected '(' or '{' or '.' after type arguments");
        return base;
      }

      // Non-generic struct literal: IDENTIFIER.{ ... }
      if (parser2_check(p, TOKEN_LBRACE) &&
          (expr->kind == AST_EXPR_IDENTIFIER ||
           expr->kind == AST_EXPR_MODULE_MEMBER)) {
        parser2_advance(p); // consume '{'
        Location loc = expr->loc;
        char *type_name = NULL;

        if (expr->kind == AST_EXPR_IDENTIFIER) {
          type_name = expr->data.ident.name;
        } else {
          char *prefix =
              prepend(expr->data.mod_member_expr.module->data.ident.name, "__");
          type_name = prepend(prefix, expr->data.mod_member_expr.member);
        }

        typedef struct FieldNode {
          char *name;
          AstNode *value;
          struct FieldNode *next;
        } FieldNode;

        FieldNode *field_head = NULL;
        FieldNode *field_tail = NULL;
        size_t field_count = 0;

        if (!parser2_check(p, TOKEN_RBRACE)) {
          while (true) {
            if (parser2_check(p, TOKEN_RBRACE)) {
              break;
            }

            if (!parser2_check(p, TOKEN_IDENTIFIER)) {
              parser2_handle_error(p, "Expected field name in struct literal");
              while (!parser2_check(p, TOKEN_COMMA) &&
                     !parser2_check(p, TOKEN_RBRACE) &&
                     !parser2_check(p, TOKEN_EOF)) {
                parser2_advance(p);
              }
              if (parser2_match(p, TOKEN_COMMA)) {
                continue;
              }
              break;
            }

            Token field_name = parser2_consume(
                p, TOKEN_IDENTIFIER, "Expected field name in struct literal");

            parser2_consume(p, TOKEN_EQUAL, "Expected '=' after field name");

            AstNode *value = parse_expression2(p);
            if (!value) {
              while (!parser2_check(p, TOKEN_COMMA) &&
                     !parser2_check(p, TOKEN_RBRACE) &&
                     !parser2_check(p, TOKEN_EOF)) {
                parser2_advance(p);
              }
              if (parser2_match(p, TOKEN_COMMA)) {
                continue;
              }
              break;
            }

            FieldNode *fn = arena_alloc(&long_lived, sizeof(FieldNode));
            fn->name = field_name.lexeme;
            fn->value = value;
            fn->next = NULL;
            if (!field_head) {
              field_head = fn;
              field_tail = fn;
            } else {
              field_tail->next = fn;
              field_tail = fn;
            }
            field_count++;

            if (!parser2_match(p, TOKEN_COMMA)) {
              break;
            }

            if (parser2_check(p, TOKEN_RBRACE)) {
              break;
            }
          }
        }

        parser2_consume(p, TOKEN_RBRACE,
                        "Expected '}' after struct literal fields");

        char **field_names = NULL;
        AstNode **field_values = NULL;
        if (field_count > 0) {
          field_names = arena_alloc(&long_lived, field_count * sizeof(char *));
          field_values =
              arena_alloc(&long_lived, field_count * sizeof(AstNode *));
          size_t i = 0;
          for (FieldNode *n = field_head; n; n = n->next) {
            field_names[i] = n->name;
            field_values[i] = n->value;
            i++;
          }
        }

        AstNode *lit = alloc_node(AST_EXPR_STRUCT_LITERAL, loc);
        lit->data.struct_literal.type_name = type_name;
        lit->data.struct_literal.qualified_type_name = type_name;
        lit->data.struct_literal.field_names = field_names;
        lit->data.struct_literal.field_values = field_values;
        lit->data.struct_literal.field_count = field_count;
        expr = lit;
        return expr;
      }

      expr = parse_member2(p, expr);
      if (!expr)
        return NULL;
      continue;
    }

    // Module member access: module::member
    if (parser2_match(p, TOKEN_MOD_SCOPE)) {
      expr = parse_module_member2(p, expr);
      if (!expr)
        return NULL;
      continue;
    }

    // Other postfix forms (struct literal/etc)
    // are not ported yet; stop here to avoid consuming tokens incorrectly.
    break;
  }

  return expr;
}

static AstNode *parse_call2(Parser2 *p, AstNode *func) {
  // We enter here after consuming '('.
  Location loc = prev_loc(p);
  AstNode *call = alloc_node(AST_EXPR_CALL, loc);
  call->data.call.func = func;
  call->data.call.args = NULL;
  call->data.call.arg_count = 0;
  call->data.call.type_args = NULL;
  call->data.call.type_arg_count = 0;

  // Parse arguments using linked list -> final contiguous array.
  AstNodePtrNode *head = NULL;
  AstNodePtrNode *tail = NULL;
  size_t arg_count = 0;

  if (!parser2_check(p, TOKEN_RPAREN)) {
    while (true) {
      AstNode *arg = parse_expression2(p);
      if (!arg) {
        // Local recovery: skip tokens until ',' or ')' or terminator.
        while (!parser2_check(p, TOKEN_COMMA) &&
               !parser2_check(p, TOKEN_RPAREN) &&
               !parser2_check(p, TOKEN_EOF)) {
          if (parser2_is_expr_terminator(p->current.type)) {
            break;
          }
          parser2_advance(p);
        }
        break;
      }

      AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
      n->node = arg;
      n->next = NULL;
      if (!head) {
        head = n;
        tail = n;
      } else {
        tail->next = n;
        tail = n;
      }
      arg_count++;

      if (!parser2_match(p, TOKEN_COMMA)) {
        break;
      }

      if (parser2_check(p, TOKEN_RPAREN)) {
        break;
      }
    }
  }

  parser2_consume(p, TOKEN_RPAREN, "Expected ')' after arguments");

  AstNode **args = NULL;
  if (arg_count > 0) {
    args = arena_alloc(&long_lived, arg_count * sizeof(AstNode *));
    size_t i = 0;
    for (AstNodePtrNode *n = head; n; n = n->next) {
      args[i++] = n->node;
    }
  }

  call->data.call.args = args;
  call->data.call.arg_count = arg_count;
  return call;
}

static AstNode *parse_index2(Parser2 *p, AstNode *array) {
  // We enter here after consuming '['.
  Location loc = prev_loc(p);

  // Slice syntax: arr[start:end]
  AstNode *start = NULL;
  AstNode *end = NULL;

  if (!parser2_check(p, TOKEN_COLON)) {
    start = parse_expression2(p);
  }

  if (parser2_match(p, TOKEN_COLON)) {
    // Slice: arr[start:end]
    if (!parser2_check(p, TOKEN_RBRACKET)) {
      end = parse_expression2(p);
    }

    parser2_consume(p, TOKEN_RBRACKET, "Expected ']' after slice");

    AstNode *slice = alloc_node(AST_EXPR_SLICE, loc);
    slice->data.slice_expr.array = array;
    slice->data.slice_expr.start = start;
    slice->data.slice_expr.end = end;
    return slice;
  }

  // Index: arr[expr]
  parser2_consume(p, TOKEN_RBRACKET, "Expected ']' after index");

  AstNode *index = alloc_node(AST_EXPR_INDEX, loc);
  index->data.index_expr.array = array;
  index->data.index_expr.index = start;
  return index;
}

static AstNode *parse_member2(Parser2 *p, AstNode *object) {
  // We enter here after consuming '.'.
  Location loc = prev_loc(p);

  AstNode *mem = alloc_node(AST_EXPR_MEMBER, loc);
  mem->data.member_expr.object = object;

  // Accept either identifier (named member) or int (tuple index)
  if (parser2_check(p, TOKEN_INT)) {
    Token idx = parser2_consume(p, TOKEN_INT,
                                "Expected member name or index after '.'");
    char buf[32];
    snprintf(buf, sizeof(buf), "%d", (int)idx.value.int_val);
    mem->data.member_expr.member = str_dup(buf);
  } else {
    Token member = parser2_consume(p, TOKEN_IDENTIFIER,
                                   "Expected member name or index after '.'");
    // Lexer owns identifier lexeme storage in long_lived arena.
    mem->data.member_expr.member = member.lexeme;
  }

  mem->data.member_expr.is_method_ref = false;
  mem->data.member_expr.method_qualified_name = NULL;
  mem->data.member_expr.is_associated_function = false;
  mem->data.member_expr.type_args = NULL;
  mem->data.member_expr.type_arg_count = 0;

  return mem;
}

static AstNode *parse_module_member2(Parser2 *p, AstNode *object) {
  // We enter here after consuming '::'.
  if (object->kind != AST_EXPR_IDENTIFIER) {
    parser2_handle_error(p, "Module name must be an identifier.");
  }

  Location loc = prev_loc(p);
  AstNode *mod_mem = alloc_node(AST_EXPR_MODULE_MEMBER, loc);
  mod_mem->data.mod_member_expr.module = object;

  Token member = parser2_consume(p, TOKEN_IDENTIFIER,
                                 "Expected module member name after '::'");
  // Lexer owns identifier lexeme storage in long_lived arena.
  mod_mem->data.mod_member_expr.member = member.lexeme;

  mod_mem->data.mod_member_expr.is_extern = false;
  mod_mem->data.mod_member_expr.qualified_path = NULL;
  return mod_mem;
}

static AstNode *parse_interpolated_string2(Parser2 *p) {
  // parser2_previous is the opening TOKEN_BACKTICK
  Location loc = prev_loc(p);

  AstNodePtrNode *head = NULL;
  AstNodePtrNode *tail = NULL;
  size_t count = 0;

  while (!parser2_check(p, TOKEN_BACKTICK) && !parser2_check(p, TOKEN_EOF)) {
    if (parser2_match(p, TOKEN_STRING)) {
      Token str = p->previous;
      AstNode *lit = alloc_node(AST_EXPR_LITERAL_STRING, str.location);
      // Lexer owns processed string storage in long_lived arena.
      lit->data.str_lit.value = str.value.str_val;

      AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
      n->node = lit;
      n->next = NULL;
      if (!head) {
        head = n;
        tail = n;
      } else {
        tail->next = n;
        tail = n;
      }
      count++;
    } else if (parser2_match(p, TOKEN_LBRACE)) {
      AstNode *expr = parse_expression2(p);
      parser2_consume(p, TOKEN_RBRACE,
                      "Expected '}' after interpolated expression");

      AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
      n->node = expr;
      n->next = NULL;
      if (!head) {
        head = n;
        tail = n;
      } else {
        tail->next = n;
        tail = n;
      }
      count++;
    } else {
      parser2_handle_error(p, "Unexpected token in interpolated string");
      parser2_advance(p);
    }
  }

  parser2_consume(p, TOKEN_BACKTICK,
                  "Expected closing '`' for interpolated string");

  AstNode **parts = NULL;
  if (count > 0) {
    parts = arena_alloc(&long_lived, count * sizeof(AstNode *));
    size_t i = 0;
    for (AstNodePtrNode *n = head; n; n = n->next) {
      parts[i++] = n->node;
    }
  }

  AstNode *interp = alloc_node(AST_EXPR_INTERPOLATED_STRING, loc);
  interp->data.interpolated_string.parts = parts;
  interp->data.interpolated_string.num_parts = count;
  return interp;
}

static AstNode *parse_primary2(Parser2 *p) {
  // Literals
  if (parser2_match(p, TOKEN_INT)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_LITERAL_INT, loc);
    n->data.int_lit.value = atoll(p->previous.lexeme);
    return n;
  }

  if (parser2_match(p, TOKEN_FLOAT)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_LITERAL_FLOAT, loc);
    n->data.float_lit.value = p->previous.value.float_val;
    return n;
  }

  if (parser2_match(p, TOKEN_STRING)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_LITERAL_STRING, loc);
    // Lexer owns processed string storage in long_lived arena.
    n->data.str_lit.value = p->previous.value.str_val;
    return n;
  }

  if (parser2_match(p, TOKEN_BACKTICK)) {
    return parse_interpolated_string2(p);
  }

  if (parser2_match(p, TOKEN_CHAR)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_LITERAL_CHAR, loc);
    n->data.char_lit.value = p->previous.value.char_val;
    return n;
  }

  if (parser2_match(p, TOKEN_TRUE) || parser2_match(p, TOKEN_FALSE)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_LITERAL_BOOL, loc);
    n->data.bool_lit.value = (p->previous.type == TOKEN_TRUE);
    return n;
  }

  if (parser2_match(p, TOKEN_NIL)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_LITERAL_NIL, loc);
    return n;
  }

  if (parser2_match(p, TOKEN_NONE)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_LITERAL_NONE, loc);
    return n;
  }

  // Context
  if (parser2_match(p, TOKEN_CONTEXT)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_CONTEXT, loc);
    return n;
  }

  // Identifiers
  if (parser2_match(p, TOKEN_IDENTIFIER)) {
    Location loc = prev_loc(p);
    AstNode *n = alloc_node(AST_EXPR_IDENTIFIER, loc);
    // Lexer owns identifier lexeme storage in long_lived arena.
    n->data.ident.name = p->previous.lexeme;
    n->data.ident.qualified_name = p->previous.lexeme;
    n->data.ident.full_qualified_name = NULL;
    n->data.ident.is_extern = false;
    return n;
  }

  // Grouping / Tuple literal
  if (parser2_match(p, TOKEN_LPAREN)) {
    Location loc = prev_loc(p);

    if (parser2_match(p, TOKEN_RPAREN)) {
      parser2_handle_error(p, "empty parentheses not allowed");
      return NULL;
    }

    AstNode *first = parse_expression2(p);
    if (!first) {
      parser2_handle_error(p, "Expected expression");
      return NULL;
    }

    if (parser2_match(p, TOKEN_COMMA)) {
      AstNodePtrNode *head = NULL;
      AstNodePtrNode *tail = NULL;
      size_t count = 0;

      AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
      n->node = first;
      n->next = NULL;
      head = n;
      tail = n;
      count = 1;

      while (true) {
        if (parser2_check(p, TOKEN_RPAREN)) {
          break;
        }

        AstNode *elem = parse_expression2(p);
        if (!elem) {
          while (!parser2_check(p, TOKEN_COMMA) &&
                 !parser2_check(p, TOKEN_RPAREN) &&
                 !parser2_check(p, TOKEN_EOF)) {
            parser2_advance(p);
          }
          if (parser2_match(p, TOKEN_COMMA)) {
            continue;
          }
          break;
        }

        AstNodePtrNode *nn = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
        nn->node = elem;
        nn->next = NULL;
        tail->next = nn;
        tail = nn;
        count++;

        if (!parser2_match(p, TOKEN_COMMA)) {
          break;
        }
      }

      parser2_consume(p, TOKEN_RPAREN, "Expected ')' after tuple literal");

      AstNode **elements = NULL;
      if (count > 0) {
        elements = arena_alloc(&long_lived, count * sizeof(AstNode *));
        size_t i = 0;
        for (AstNodePtrNode *cur = head; cur; cur = cur->next) {
          elements[i++] = cur->node;
        }
      }

      AstNode *tuple = alloc_node(AST_EXPR_TUPLE, loc);
      tuple->data.tuple_expr.elements = elements;
      tuple->data.tuple_expr.element_count = count;
      return tuple;
    }

    parser2_consume(p, TOKEN_RPAREN, "Expected ')' after expression");
    AstNode *grouped = alloc_node(AST_EXPR_GROUPED_EXPR, loc);
    grouped->data.grouped_expr.inner_expr = first;
    return grouped;
  }

  // Array literal [1,2,3] or array repeat [value; count]
  if (parser2_match(p, TOKEN_LBRACKET)) {
    Location loc = prev_loc(p);

    if (parser2_check(p, TOKEN_RBRACKET)) {
      parser2_advance(p);
      AstNode *array_lit = alloc_node(AST_EXPR_ARRAY_LITERAL, loc);
      array_lit->data.array_literal.elements = NULL;
      array_lit->data.array_literal.element_count = 0;
      return array_lit;
    }

    AstNode *first = parse_expression2(p);
    if (!first) {
      return NULL;
    }

    if (parser2_match(p, TOKEN_SEMICOLON)) {
      if (!parser2_check(p, TOKEN_INT)) {
        parser2_handle_error(p,
                             "Expected integer literal for array repeat count");
        return NULL;
      }

      long long repeat_count = p->current.value.int_val;
      if (repeat_count <= 0) {
        parser2_handle_error(p, "Array repeat count must be positive");
        return NULL;
      }

      parser2_advance(p);
      parser2_consume(p, TOKEN_RBRACKET,
                      "Expected ']' after array repeat count");

      AstNode *array_repeat = alloc_node(AST_EXPR_ARRAY_REPEAT, loc);
      array_repeat->data.array_repeat.value = first;
      array_repeat->data.array_repeat.count = (size_t)repeat_count;
      return array_repeat;
    }

    AstNodePtrNode *head = NULL;
    AstNodePtrNode *tail = NULL;
    size_t count = 0;

    AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
    n->node = first;
    n->next = NULL;
    head = n;
    tail = n;
    count = 1;

    while (parser2_match(p, TOKEN_COMMA)) {
      AstNode *elem = parse_expression2(p);
      if (!elem) {
        while (!parser2_check(p, TOKEN_COMMA) &&
               !parser2_check(p, TOKEN_RBRACKET) &&
               !parser2_check(p, TOKEN_EOF)) {
          parser2_advance(p);
        }
        if (parser2_check(p, TOKEN_RBRACKET)) {
          break;
        }
        continue;
      }

      AstNodePtrNode *nn = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
      nn->node = elem;
      nn->next = NULL;
      tail->next = nn;
      tail = nn;
      count++;
    }

    parser2_consume(p, TOKEN_RBRACKET, "Expected ']' after array elements");

    AstNode **elements = NULL;
    if (count > 0) {
      elements = arena_alloc(&long_lived, count * sizeof(AstNode *));
      size_t i = 0;
      for (AstNodePtrNode *cur = head; cur; cur = cur->next) {
        elements[i++] = cur->node;
      }
    }

    AstNode *array_lit = alloc_node(AST_EXPR_ARRAY_LITERAL, loc);
    array_lit->data.array_literal.elements = elements;
    array_lit->data.array_literal.element_count = count;
    return array_lit;
  }

  // Sizeof expression
  if (parser2_match(p, TOKEN_SIZEOF)) {
    Location loc = prev_loc(p);
    AstNode *type_ast = parse_type_expression2(p);

    AstNode *sizeof_expr = alloc_node(AST_EXPR_SIZEOF, loc);
    sizeof_expr->data.sizeof_expr.type_expr = type_ast;
    return sizeof_expr;
  }

  // Optional expression (some expression)
  if (parser2_match(p, TOKEN_SOME)) {
    Location loc = prev_loc(p);
    AstNode *inner_expr = parse_expression2(p);
    if (!inner_expr) {
      return NULL;
    }

    AstNode *some_expr = alloc_node(AST_EXPR_SOME, loc);
    some_expr->data.some_expr.value = inner_expr;
    return some_expr;
  }

  // Function literal
  if (parser2_match(p, TOKEN_FN)) {
    Location loc = prev_loc(p);
    bool inlined = parser2_match(p, TOKEN_INLINE);

    AstNode *convention = NULL;
    if (parser2_match(p, TOKEN_STRING)) {
      convention = alloc_node(AST_EXPR_LITERAL_STRING, prev_loc(p));
      // Use raw lexeme to match parser.c behavior.
      convention->data.str_lit.value = p->previous.lexeme;
    }

    return parse_function2(p, loc, NULL, inlined, convention, true);
  }

  // Anonymous struct literal / partial member
  if (parser2_match(p, TOKEN_DOT)) {
    if (parser2_match(p, TOKEN_IDENTIFIER)) {
      AstNode *expr = alloc_node(AST_EXPR_PARTIAL_MEMBER, prev_loc(p));
      expr->data.partial_member_expr.member = p->previous.lexeme;
      return expr;
    } else if (parser2_match(p, TOKEN_LBRACE)) {
      Location loc = prev_loc(p);

      typedef struct FieldNode {
        char *name;
        AstNode *value;
        struct FieldNode *next;
      } FieldNode;

      FieldNode *field_head = NULL;
      FieldNode *field_tail = NULL;
      size_t field_count = 0;

      if (!parser2_check(p, TOKEN_RBRACE)) {
        while (true) {
          if (parser2_check(p, TOKEN_RBRACE)) {
            break;
          }

          if (!parser2_check(p, TOKEN_IDENTIFIER)) {
            parser2_handle_error(p, "Expected field name in struct literal");
            while (!parser2_check(p, TOKEN_COMMA) &&
                   !parser2_check(p, TOKEN_RBRACE) &&
                   !parser2_check(p, TOKEN_EOF)) {
              parser2_advance(p);
            }
            if (parser2_match(p, TOKEN_COMMA)) {
              continue;
            }
            break;
          }

          Token field_name = parser2_consume(
              p, TOKEN_IDENTIFIER, "Expected field name in struct literal");

          parser2_consume(p, TOKEN_EQUAL, "Expected '=' after field name");

          AstNode *value = parse_expression2(p);
          if (!value) {
            while (!parser2_check(p, TOKEN_COMMA) &&
                   !parser2_check(p, TOKEN_RBRACE) &&
                   !parser2_check(p, TOKEN_EOF)) {
              parser2_advance(p);
            }
            if (parser2_match(p, TOKEN_COMMA)) {
              continue;
            }
            break;
          }

          FieldNode *fn = arena_alloc(&long_lived, sizeof(FieldNode));
          fn->name = field_name.lexeme;
          fn->value = value;
          fn->next = NULL;
          if (!field_head) {
            field_head = fn;
            field_tail = fn;
          } else {
            field_tail->next = fn;
            field_tail = fn;
          }
          field_count++;

          if (!parser2_match(p, TOKEN_COMMA)) {
            break;
          }
        }
      }

      parser2_consume(p, TOKEN_RBRACE,
                      "Expected '}' after struct literal fields");

      char **field_names = NULL;
      AstNode **field_values = NULL;
      if (field_count > 0) {
        field_names = arena_alloc(&long_lived, field_count * sizeof(char *));
        field_values =
            arena_alloc(&long_lived, field_count * sizeof(AstNode *));
        size_t i = 0;
        for (FieldNode *n = field_head; n; n = n->next) {
          field_names[i] = n->name;
          field_values[i] = n->value;
          i++;
        }
      }

      AstNode *expr = alloc_node(AST_EXPR_STRUCT_LITERAL, loc);
      expr->data.struct_literal.type_name = NULL;
      expr->data.struct_literal.qualified_type_name = NULL;
      expr->data.struct_literal.field_names = field_names;
      expr->data.struct_literal.field_values = field_values;
      expr->data.struct_literal.field_count = field_count;
      return expr;
    }
  }

  parser2_handle_error(p, "Expected expression");
  return NULL;
}

// ---------- type expr ----------
static AstNode *parse_type_expression2(Parser2 *p) {
  AstNode *type = NULL;

  // Function type: fn(T1, T2, ...) ReturnType
  if (parser2_match(p, TOKEN_FN)) {
    Location loc = prev_loc(p);

    AstNode *convention = NULL;
    if (parser2_match(p, TOKEN_STRING)) {
      convention = alloc_node(AST_EXPR_LITERAL_STRING, prev_loc(p));
      convention->data.str_lit.value = p->previous.lexeme;
    }

    parser2_consume(p, TOKEN_LPAREN, "Expected '(' after 'fn'");

    AstNodePtrNode *head = NULL;
    AstNodePtrNode *tail = NULL;
    size_t count = 0;

    if (!parser2_check(p, TOKEN_RPAREN)) {
      while (true) {
        AstNode *param_type = parse_type_expression2(p);
        if (!param_type)
          return NULL;

        AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
        n->node = param_type;
        n->next = NULL;
        if (!head) {
          head = n;
          tail = n;
        } else {
          tail->next = n;
          tail = n;
        }
        count++;

        if (!parser2_match(p, TOKEN_COMMA)) {
          break;
        }
      }
    }

    parser2_consume(p, TOKEN_RPAREN, "Expected ')' after parameter types");

    AstNode *return_type = parse_type_expression2(p);
    if (!return_type) {
      parser2_handle_error(p, "Expected return type after function parameters");
      return NULL;
    }

    AstNode **param_types = NULL;
    if (count > 0) {
      param_types = arena_alloc(&long_lived, count * sizeof(AstNode *));
      size_t i = 0;
      for (AstNodePtrNode *n = head; n; n = n->next) {
        param_types[i++] = n->node;
      }
    }

    type = alloc_node(AST_TYPE_FUNCTION, loc);
    type->data.type_function.convention = convention;
    type->data.type_function.param_types = param_types;
    type->data.type_function.param_count = count;
    type->data.type_function.return_type = return_type;
    return type;
  }

  // Tuple type: (T1, T2, ...)
  if (parser2_match(p, TOKEN_LPAREN)) {
    Location loc = prev_loc(p);

    if (parser2_match(p, TOKEN_RPAREN)) {
      parser2_handle_error(p, "empty tuple type not allowed");
      return NULL;
    }

    AstNode *first = parse_type_expression2(p);
    if (!first)
      return NULL;

    if (parser2_match(p, TOKEN_COMMA)) {
      AstNodePtrNode *head = NULL;
      AstNodePtrNode *tail = NULL;
      size_t count = 0;

      AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
      n->node = first;
      n->next = NULL;
      head = n;
      tail = n;
      count = 1;

      while (true) {
        AstNode *elem = parse_type_expression2(p);
        if (!elem)
          return NULL;

        AstNodePtrNode *nn = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
        nn->node = elem;
        nn->next = NULL;
        tail->next = nn;
        tail = nn;
        count++;

        if (!parser2_match(p, TOKEN_COMMA)) {
          break;
        }
      }

      parser2_consume(p, TOKEN_RPAREN, "Expected ')' after tuple type");

      AstNode **elements = arena_alloc(&long_lived, count * sizeof(AstNode *));
      size_t i = 0;
      for (AstNodePtrNode *cur = head; cur; cur = cur->next) {
        elements[i++] = cur->node;
      }

      type = alloc_node(AST_TYPE_TUPLE, loc);
      type->data.type_tuple.element_types = elements;
      type->data.type_tuple.element_count = count;
      return type;
    }

    parser2_consume(p, TOKEN_RPAREN, "Expected ')'");
    return first;
  }

  // Pointer type: *T
  if (parser2_match(p, TOKEN_STAR)) {
    Location loc = prev_loc(p);
    AstNode *base = parse_type_expression2(p);

    type = alloc_node(AST_TYPE_POINTER, loc);
    type->data.type_pointer.base = base;
    return type;
  }

  // Optional type: ?T
  if (parser2_match(p, TOKEN_QUESTION)) {
    Location loc = prev_loc(p);
    AstNode *base = parse_type_expression2(p);

    type = alloc_node(AST_TYPE_OPTIONAL, loc);
    type->data.type_optional.base = base;
    return type;
  }

  // Array or slice: [N]T or []T
  if (parser2_match(p, TOKEN_LBRACKET)) {
    Location loc = prev_loc(p);

    if (parser2_match(p, TOKEN_RBRACKET)) {
      AstNode *element = parse_type_expression2(p);
      type = alloc_node(AST_TYPE_SLICE, loc);
      type->data.type_slice.element = element;
      return type;
    }

    if (!parser2_check(p, TOKEN_INT)) {
      parser2_handle_error(p, "Expected array size");
      return NULL;
    }
    parser2_advance(p);
    Token size_tok = p->previous;
    size_t size = (size_t)size_tok.value.int_val;

    parser2_consume(p, TOKEN_RBRACKET, "Expected ']' after array size");
    AstNode *element = parse_type_expression2(p);

    type = alloc_node(AST_TYPE_ARRAY, loc);
    type->data.type_array.element = element;
    type->data.type_array.size = size;
    return type;
  }

  // Struct type: struct { ... }
  if (parser2_match(p, TOKEN_STRUCT)) {
    Location loc = prev_loc(p);

    parser2_consume(p, TOKEN_LBRACE, "Expected '{' after 'struct'");

    typedef struct FieldNode {
      char *name;
      AstNode *type_expr;
      struct FieldNode *next;
    } FieldNode;

    typedef struct MethodNode {
      AstNode *method;
      struct MethodNode *next;
    } MethodNode;

    FieldNode *field_head = NULL;
    FieldNode *field_tail = NULL;
    size_t field_count = 0;

    MethodNode *method_head = NULL;
    MethodNode *method_tail = NULL;
    size_t method_count = 0;

    while (!parser2_check(p, TOKEN_RBRACE)) {
      if (parser2_check(p, TOKEN_FN)) {
        parser2_advance(p);
        AstNode *method = parse_function_decl2(p);
        if (!method) {
          return NULL;
        }

        MethodNode *mn = arena_alloc(&long_lived, sizeof(MethodNode));
        mn->method = method;
        mn->next = NULL;
        if (!method_head) {
          method_head = mn;
          method_tail = mn;
        } else {
          method_tail->next = mn;
          method_tail = mn;
        }
        method_count++;
        continue;
      }

      if (!parser2_check(p, TOKEN_IDENTIFIER)) {
        parser2_handle_error(p, "Expected field name");
        return NULL;
      }

      // Parse field name(s)
      typedef struct NameNode {
        char *name;
        struct NameNode *next;
      } NameNode;

      NameNode *name_head = NULL;
      NameNode *name_tail = NULL;

      Token field_name =
          parser2_consume(p, TOKEN_IDENTIFIER, "Expected field name");
      NameNode *nn = arena_alloc(&long_lived, sizeof(NameNode));
      nn->name = field_name.lexeme;
      nn->next = NULL;
      name_head = nn;
      name_tail = nn;

      while (parser2_match(p, TOKEN_COMMA)) {
        Token next_name =
            parser2_consume(p, TOKEN_IDENTIFIER, "Expected field name");
        NameNode *n2 = arena_alloc(&long_lived, sizeof(NameNode));
        n2->name = next_name.lexeme;
        n2->next = NULL;
        name_tail->next = n2;
        name_tail = n2;
      }

      AstNode *field_type = parse_type_expression2(p);
      if (!field_type)
        return NULL;

      for (NameNode *n = name_head; n; n = n->next) {
        FieldNode *fn = arena_alloc(&long_lived, sizeof(FieldNode));
        fn->name = n->name;
        fn->type_expr = field_type;
        fn->next = NULL;
        if (!field_head) {
          field_head = fn;
          field_tail = fn;
        } else {
          field_tail->next = fn;
          field_tail = fn;
        }
        field_count++;
      }

      parser2_consume(p, TOKEN_SEMICOLON, "Expected ';' after field");
    }

    parser2_consume(p, TOKEN_RBRACE, "Expected '}' after struct fields");

    char **field_names = NULL;
    AstNode **field_types = NULL;
    if (field_count > 0) {
      field_names = arena_alloc(&long_lived, field_count * sizeof(char *));
      field_types = arena_alloc(&long_lived, field_count * sizeof(AstNode *));
      size_t i = 0;
      for (FieldNode *n = field_head; n; n = n->next) {
        field_names[i] = n->name;
        field_types[i] = n->type_expr;
        i++;
      }
    }

    AstNode **methods = NULL;
    if (method_count > 0) {
      methods = arena_alloc(&long_lived, method_count * sizeof(AstNode *));
      size_t i = 0;
      for (MethodNode *m = method_head; m; m = m->next) {
        methods[i++] = m->method;
      }
    }

    type = alloc_node(AST_TYPE_STRUCT, loc);
    type->data.type_struct.field_names = field_names;
    type->data.type_struct.field_types = field_types;
    type->data.type_struct.field_count = field_count;
    type->data.type_struct.methods = methods;
    type->data.type_struct.method_count = method_count;
    return type;
  }

  // Union type: union (enum) { ... }
  if (parser2_match(p, TOKEN_UNION)) {
    Location loc = prev_loc(p);

    bool is_tagged = parser2_match(p, TOKEN_ENUM);

    parser2_consume(p, TOKEN_LBRACE, "Expected '{' after 'union'");

    typedef struct FieldNode {
      char *name;
      AstNode *type_expr;
      struct FieldNode *next;
    } FieldNode;

    typedef struct MethodNode {
      AstNode *method;
      struct MethodNode *next;
    } MethodNode;

    FieldNode *field_head = NULL;
    FieldNode *field_tail = NULL;
    size_t field_count = 0;

    MethodNode *method_head = NULL;
    MethodNode *method_tail = NULL;
    size_t method_count = 0;

    while (!parser2_check(p, TOKEN_RBRACE)) {
      if (parser2_check(p, TOKEN_FN)) {
        parser2_advance(p);
        AstNode *method = parse_function_decl2(p);
        if (!method) {
          return NULL;
        }

        MethodNode *mn = arena_alloc(&long_lived, sizeof(MethodNode));
        mn->method = method;
        mn->next = NULL;
        if (!method_head) {
          method_head = mn;
          method_tail = mn;
        } else {
          method_tail->next = mn;
          method_tail = mn;
        }
        method_count++;
        continue;
      }

      if (!parser2_check(p, TOKEN_IDENTIFIER)) {
        parser2_handle_error(p, "Expected field name");
        return NULL;
      }

      typedef struct NameNode {
        char *name;
        struct NameNode *next;
      } NameNode;

      NameNode *name_head = NULL;
      NameNode *name_tail = NULL;

      Token field_name =
          parser2_consume(p, TOKEN_IDENTIFIER, "Expected field name");
      NameNode *nn = arena_alloc(&long_lived, sizeof(NameNode));
      nn->name = field_name.lexeme;
      nn->next = NULL;
      name_head = nn;
      name_tail = nn;

      while (parser2_match(p, TOKEN_COMMA)) {
        Token next_name =
            parser2_consume(p, TOKEN_IDENTIFIER, "Expected field name");
        NameNode *n2 = arena_alloc(&long_lived, sizeof(NameNode));
        n2->name = next_name.lexeme;
        n2->next = NULL;
        name_tail->next = n2;
        name_tail = n2;
      }

      AstNode *field_type = parse_type_expression2(p);
      if (!field_type)
        return NULL;

      for (NameNode *n = name_head; n; n = n->next) {
        FieldNode *fn = arena_alloc(&long_lived, sizeof(FieldNode));
        fn->name = n->name;
        fn->type_expr = field_type;
        fn->next = NULL;
        if (!field_head) {
          field_head = fn;
          field_tail = fn;
        } else {
          field_tail->next = fn;
          field_tail = fn;
        }
        field_count++;
      }

      parser2_consume(p, TOKEN_SEMICOLON, "Expected ';' after variant");
    }

    parser2_consume(p, TOKEN_RBRACE, "Expected '}' after union fields");

    char **variant_names = NULL;
    AstNode **variant_types = NULL;
    if (field_count > 0) {
      variant_names = arena_alloc(&long_lived, field_count * sizeof(char *));
      variant_types = arena_alloc(&long_lived, field_count * sizeof(AstNode *));
      size_t i = 0;
      for (FieldNode *n = field_head; n; n = n->next) {
        variant_names[i] = n->name;
        variant_types[i] = n->type_expr;
        i++;
      }
    }

    AstNode **methods = NULL;
    if (method_count > 0) {
      methods = arena_alloc(&long_lived, method_count * sizeof(AstNode *));
      size_t i = 0;
      for (MethodNode *m = method_head; m; m = m->next) {
        methods[i++] = m->method;
      }
    }

    type = alloc_node(AST_TYPE_UNION, loc);
    type->data.type_union.is_tagged = is_tagged;
    type->data.type_union.variant_names = variant_names;
    type->data.type_union.variant_types = variant_types;
    type->data.type_union.variant_count = field_count;
    type->data.type_union.methods = methods;
    type->data.type_union.method_count = method_count;
    return type;
  }

  // Enum type: enum { variant, ... }
  if (parser2_match(p, TOKEN_ENUM)) {
    Location loc = prev_loc(p);

    parser2_consume(p, TOKEN_LBRACE, "Expected '{' after 'enum'");

    typedef struct NameNode {
      char *name;
      struct NameNode *next;
    } NameNode;

    NameNode *head = NULL;
    NameNode *tail = NULL;
    size_t count = 0;

    if (!parser2_check(p, TOKEN_RBRACE)) {
      do {
        if (parser2_check(p, TOKEN_RBRACE)) {
          break;
        }

        if (!parser2_check(p, TOKEN_IDENTIFIER)) {
          parser2_handle_error(p, "Expected enum variant name");
          return NULL;
        }
        parser2_advance(p);
        Token variant_name = p->previous;

        NameNode *n = arena_alloc(&long_lived, sizeof(NameNode));
        n->name = variant_name.lexeme;
        n->next = NULL;
        if (!head) {
          head = n;
          tail = n;
        } else {
          tail->next = n;
          tail = n;
        }
        count++;
      } while (parser2_match(p, TOKEN_COMMA));
    }

    parser2_consume(p, TOKEN_RBRACE, "Expected '}' after enum variants");

    char **variant_names = NULL;
    if (count > 0) {
      variant_names = arena_alloc(&long_lived, count * sizeof(char *));
      size_t i = 0;
      for (NameNode *n = head; n; n = n->next) {
        variant_names[i++] = n->name;
      }
    }

    type = alloc_node(AST_TYPE_ENUM, loc);
    type->data.type_enum.variant_names = variant_names;
    type->data.type_enum.variant_count = count;
    return type;
  }

  // Built-in types
  switch (p->current.type) {
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

  // Custom/named types (with optional qualified name and type args)
  if (parser2_match(p, TOKEN_IDENTIFIER)) {
    Token name = p->previous;

    bool is_qualified = false;
    if (parser2_match(p, TOKEN_MOD_SCOPE)) {
      Token mem = parser2_consume(p, TOKEN_IDENTIFIER,
                                  "Expected an identifier after '::'");
      type = alloc_node(AST_TYPE_QUALIFIED_NAMED, mem.location);
      type->data.type_qualified_named.mod_name = name.lexeme;
      type->data.type_qualified_named.mem_name = mem.lexeme;
      is_qualified = true;
    } else {
      type = alloc_node(AST_TYPE_NAMED, name.location);
      type->data.type_named.name = name.lexeme;
    }

    AstNodePtrNode *head = NULL;
    AstNodePtrNode *tail = NULL;
    size_t count = 0;

    if (parser2_match(p, TOKEN_LBRACKET)) {
      if (!parser2_check(p, TOKEN_RBRACKET)) {
        while (true) {
          AstNode *arg = parse_type_expression2(p);
          if (!arg)
            return NULL;

          AstNodePtrNode *n = arena_alloc(&long_lived, sizeof(AstNodePtrNode));
          n->node = arg;
          n->next = NULL;
          if (!head) {
            head = n;
            tail = n;
          } else {
            tail->next = n;
            tail = n;
          }
          count++;

          if (!parser2_match(p, TOKEN_COMMA)) {
            break;
          }
        }
      }

      parser2_consume(p, TOKEN_RBRACKET, "Expected ']' after type arguments");
    }

    AstNode **type_args = NULL;
    if (count > 0) {
      type_args = arena_alloc(&long_lived, count * sizeof(AstNode *));
      size_t i = 0;
      for (AstNodePtrNode *n = head; n; n = n->next) {
        type_args[i++] = n->node;
      }
    }

    if (is_qualified) {
      type->data.type_qualified_named.type_args = type_args;
      type->data.type_qualified_named.type_arg_count = count;
    } else {
      type->data.type_named.type_args = type_args;
      type->data.type_named.type_arg_count = count;
    }

    return type;
  }

  parser2_handle_error(p, "Expected type");
  return NULL;
}
