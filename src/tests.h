#ifndef TESTS_H
#define TESTS_H

#include "ast.h"

// Test functions
void test_setup(void);
void test_lexer(void);
void test_parser(void);
void test_all(void);

// Helper functions
const char *ast_kind_name(AstKind kind);

#endif
