#include "parser2.h"
#include "alloc.h"
#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// External allocator
extern Arena long_lived;

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

  parser2_handle_error(parser, "Expected declaration");
  return NULL;
}

// ---------- minimal function parsing ----------
static AstNode *parse_function_decl2(Parser2 *p) {
  // fn <name> ( ... ) <return_type> { ... }
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

  // Return type
  fn->data.func_decl.return_type = parse_type_expression2(p);
  if (!fn->data.func_decl.return_type)
    return NULL;

  // Body
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

  // Minimal: require expression + ';' (matches your existing common style)
  AstNode *expr = parse_expression2(p);
  if (!expr)
    return NULL;

  // Missing ';' is recoverable when the next token clearly starts a statement.
  parser2_expect_semicolon(p, "Expected ';' after return value");
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
  // Minimal: named types only (int, usize, etc.)
  if (parser2_match(p, TOKEN_IDENTIFIER)) {
    Location loc = prev_loc(p);
    AstNode *t = alloc_node(AST_TYPE_NAMED, loc);
    t->data.type_named.name = p->previous.lexeme;
    t->data.type_named.type_args = NULL;
    t->data.type_named.type_arg_count = 0;
    return t;
  }

  parser2_handle_error(p, "Expected type");
  return NULL;
}
