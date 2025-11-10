#include "pastel.h"
#include <stdio.h>

void print_token(Token *token) {
  const char *type_names[] = {"TEXT", "FORMAT_START", "FORMAT_END", "EOF",
                              "ERROR"};

  printf("Token { type: %-13s, line: %zu, col: %zu, value: \"%s\" }\n",
         type_names[token->type], token->line, token->column,
         token->value ? token->value : "(null)");
}

void test_tokenize(const char *input) {
  printf("\n========================================\n");
  printf("Input: %s\n", input);
  printf("========================================\n");

  size_t token_count;
  Token *tokens = pastel_tokenize(input, &token_count);

  if (!tokens) {
    printf("ERROR: Failed to tokenize\n");
    return;
  }

  for (size_t i = 0; i < token_count; i++) {
    print_token(&tokens[i]);
  }

  pastel_free_tokens(tokens, token_count);
}

int main() {
  // Test 1: Simple text
  test_tokenize("Hello world");

  // Test 2: Single directive
  test_tokenize("*[*, red]Bold red text[/]");

  // Test 3: Multiple directives
  test_tokenize("Normal *[b]bold[/] normal *[u, blue]underlined blue[/] end");

  // Test 4: Nested (should work now!)
  test_tokenize("*[*]Bold with *[red]nested red[/] back to bold[/]");

  return 0;
}
