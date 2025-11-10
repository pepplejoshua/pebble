#include "pastel.h"
#include <stdio.h>

void test_format(const char *input) {
  printf("\n========================================\n");
  printf("Input: %s\n", input);
  printf("========================================\n");

  char *output = pastel_format_text(input);

  if (!output) {
    printf("ERROR: Failed to format\n");
    return;
  }

  printf("Output: %s\n", output);
  printf("========================================\n");

  free(output);
}

int main() {
  // Test 1: Plain text
  test_format("Hello world");

  // Test 2: Bold text
  test_format("*[*]This is bold[/]");

  // Test 3: Bold with color
  test_format("*[*, red]Bold red text[/]");

  // Test 4: Multiple styles
  test_format("Normal *[b]bold[/] *[u]underline[/] *[i]italic[/] normal");

  // Test 5: Colors
  test_format("*[l_red]Light red[/] *[d_blue]Dark blue[/]");

  // Test 6: Foreground and background
  test_format("*[l_white:d_red]White on red background[/]");

  // Test 7: Complex combination
  test_format("*[*, u, l_cyan:d_magenta]Bold underlined cyan on magenta[/]");

  // Test 8: Nesting!
  test_format("*[*]Bold with *[red]nested red[/] back to bold[/]");

  // Test 9: Multiple nested levels
  test_format(
      "*[b]Bold *[u]and underlined *[green]and green[/] back[/] just bold[/]");

  // Test 10: Background only
  test_format("*[:d_yellow]Yellow background only[/]");

  return 0;
}
