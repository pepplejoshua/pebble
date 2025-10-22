#ifndef PEBBLE_OPTIONS_H
#define PEBBLE_OPTIONS_H

#include <stdbool.h>

typedef enum ReleaseMode
{
  RELEASE_DEBUG,
  RELEASE_SMALL,
  RELEASE_DEFAULT,
} ReleaseMode;

// Compiler options
typedef struct CompilerOptions
{
  bool freestanding;
  bool verbose;
  bool keep_c_file;
  bool release_mode;
  const char *compiler;
  const char *output_name;
  const char *input_file;
} CompilerOptions;

// Global compiler options
extern CompilerOptions compiler_opts;

void initialise_args();
char* release_mode_string();
void print_usage(const char *program_name);
bool parse_args(int argc, char **argv);

#endif // PEBBLE_OPTIONS_H