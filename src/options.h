#ifndef PEBBLE_OPTIONS_H
#define PEBBLE_OPTIONS_H

#include <stdbool.h>
#include <stddef.h>

typedef enum ReleaseMode
{
  RELEASE_DEBUG,
  RELEASE_SMALL,
  RELEASE_DEFAULT,
} ReleaseMode;

typedef enum LibraryType
{
  LIBRARY_NONE,
  LIBRARY_SHARED,
  LIBRARY_STATIC, // TODO
} LibraryType;

// Compiler options
typedef struct CompilerOptions
{
  bool freestanding;
  bool verbose;
  bool warnings;
  bool keep_c_file;
  bool generate_only;
  ReleaseMode release_mode;
  const char *compiler;
  const char *output_exe_name;
  const char *output_c_name;
  const char *input_file;
  bool has_main;
  LibraryType library;
  const char *entry_point;

  char **linked_libraries;
  size_t linked_libraries_count;
  size_t linked_libraries_capacity;
} CompilerOptions;

// Global compiler options
extern CompilerOptions compiler_opts;

void auto_detect_compiler(void);
void initialise_args(void);
void cleanup_args(void);
void append_library_string(char *library);
char *flatten_library_strings(void);
char* release_mode_string(void);
void print_usage(const char *program_name);
bool parse_args(int argc, char **argv);

#endif // PEBBLE_OPTIONS_H
