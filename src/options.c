#include "options.h"

#include "alloc.h"
#include "uthash.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

CompilerOptions compiler_opts = {0};

// Auto-detect available C compiler
void auto_detect_compiler(void) {
  if (compiler_opts.compiler != NULL &&
      strcmp(compiler_opts.compiler, "") != 0) {
    return; // User specified; don't override
  }

  int clang_exists = system("command -v clang >/dev/null 2>&1");
  int gcc_exists = system("command -v gcc >/dev/null 2>&1");
  int cc_exists = system("command -v cc >/dev/null 2>&1");

  const char *detected = NULL;

#ifdef __APPLE__
  // macOS: Prefer clang (gcc is usually clang alias)
  if (clang_exists == 0) {
    detected = "clang";
  } else if (cc_exists == 0) {
    detected = "cc"; // Often points to clang
  } else if (gcc_exists == 0) {
    detected = "gcc";
  }
#elif defined(__linux__)
  // Linux: Prefer gcc (clang optional)
  if (gcc_exists == 0) {
    detected = "gcc";
  } else if (clang_exists == 0) {
    detected = "clang";
  } else if (cc_exists == 0) {
    detected = "cc";
  }
#else
  // Generic: Prefer clang if available, else gcc
  if (clang_exists == 0) {
    detected = "clang";
  } else if (gcc_exists == 0) {
    detected = "gcc";
  } else if (cc_exists == 0) {
    detected = "cc";
  }
#endif

  if (detected == NULL) {
    fprintf(stderr, "Error: No suitable C compiler found (clang, gcc, or cc). "
                    "Install one and ensure it's in PATH.\n");
    exit(1); // Or set a flag to error later
  }

  compiler_opts.compiler =
      str_dup(detected); // Use your str_dup; assumes it copies to arena/global
}

typedef struct {
  char *name;
  UT_hash_handle hh;
} StringEntry;

static StringEntry *library_set = NULL;
static StringEntry *library_path_set = NULL;
static StringEntry *include_path_set = NULL;
static StringEntry *header_set = NULL;
static StringEntry *system_header_set = NULL;

void initialise_args(void) {
  // Initialize defaults
  compiler_opts.freestanding = false;
  compiler_opts.release_mode = RELEASE_DEBUG;
  compiler_opts.compiler = NULL;
  compiler_opts.verbose = false;
  compiler_opts.warnings = false;
  compiler_opts.keep_c_file = true;
  compiler_opts.generate_only = false;
  compiler_opts.output_exe_name = "output";
  compiler_opts.output_c_name = "output.c";
  compiler_opts.has_main = true;
  compiler_opts.library = LIBRARY_NONE;
  compiler_opts.entry_point = "main";
  compiler_opts.input_file = NULL;
  compiler_opts.std_path = NULL; // Default to alongside compiler

  // linked libraries
  compiler_opts.linked_libraries = NULL;
  compiler_opts.linked_libraries_count = 0;
  compiler_opts.linked_libraries_capacity = 0;

  compiler_opts.lib_paths = NULL;
  compiler_opts.lib_paths_count = 0;
  compiler_opts.lib_paths_capacity = 0;

  compiler_opts.local_headers = NULL;
  compiler_opts.local_headers_count = 0;
  compiler_opts.local_headers_capacity = 0;

  compiler_opts.system_headers = NULL;
  compiler_opts.system_headers_count = 0;
  compiler_opts.system_headers_capacity = 0;

  compiler_opts.include_paths = NULL;
  compiler_opts.include_paths_count = 0;
  compiler_opts.include_paths_capacity = 0;

  auto_detect_compiler();
}

void cleanup_args(void) {
  HASH_CLEAR(hh, library_set);
  HASH_CLEAR(hh, library_path_set);
  HASH_CLEAR(hh, include_path_set);
  HASH_CLEAR(hh, header_set);
  HASH_CLEAR(hh, system_header_set);
}

void append_library_string(char *str) {
  char *library = str;

  char str_buffer[512] = {0};

  size_t len = strlen(library);
  if (len >= 2 && library[0] != '"' && library[len - 1] != '"') {
    sprintf(str_buffer, "\"%s\"", library);
    library = str_buffer;
  }

  StringEntry *entry;
  HASH_FIND_STR(library_set, library, entry);

  if (entry) {
    return;
  }

  if (compiler_opts.linked_libraries_count >=
      compiler_opts.linked_libraries_capacity) {
    size_t new_cap = compiler_opts.linked_libraries_capacity;
    new_cap = new_cap == 0 ? 4 : new_cap * 2;

    char **new_libraries = arena_alloc(&long_lived, new_cap * sizeof(char *));
    memcpy(new_libraries, compiler_opts.linked_libraries,
           compiler_opts.linked_libraries_count * sizeof(char *));

    compiler_opts.linked_libraries = new_libraries;
  }

  compiler_opts.linked_libraries[compiler_opts.linked_libraries_count++] =
      str_dup(library);

  entry = arena_alloc(&long_lived, sizeof(StringEntry));
  entry->name = str_dup(library);
  HASH_ADD_KEYPTR(hh, library_set, entry->name, strlen(entry->name), entry);
}

void append_library_path_string(char *str) {
  char *library_path = str;

  char str_buffer[512] = {0};

  size_t len = strlen(library_path);
  if (len >= 2 && library_path[0] != '"' && library_path[len - 1] != '"') {
    sprintf(str_buffer, "\"%s\"", library_path);
    library_path = str_buffer;
  }

  StringEntry *entry;
  HASH_FIND_STR(library_path_set, library_path, entry);

  if (entry) {
    return;
  }

  if (compiler_opts.lib_paths_count >=
      compiler_opts.lib_paths_capacity) {
    size_t new_cap = compiler_opts.lib_paths_capacity;
    new_cap = new_cap == 0 ? 4 : new_cap * 2;

    char **new_library_paths = arena_alloc(&long_lived, new_cap * sizeof(char *));
    memcpy(new_library_paths, compiler_opts.lib_paths,
           compiler_opts.lib_paths_count * sizeof(char *));

    compiler_opts.lib_paths = new_library_paths;
  }

  compiler_opts.lib_paths[compiler_opts.lib_paths_count++] =
      str_dup(library_path);

  entry = arena_alloc(&long_lived, sizeof(StringEntry));
  entry->name = str_dup(library_path);
  HASH_ADD_KEYPTR(hh, library_path_set, entry->name, strlen(entry->name), entry);
}

void append_include_path_string(char *str) {
  char *include = str;

  char str_buffer[512] = {0};

  size_t len = strlen(include);
  if (len >= 2 && include[0] != '"' && include[len - 1] != '"') {
    sprintf(str_buffer, "\"%s\"", include);
    include = str_buffer;
  }

  StringEntry *entry;
  HASH_FIND_STR(include_path_set, include, entry);

  if (entry) {
    return;
  }

  if (compiler_opts.include_paths_count >=
      compiler_opts.include_paths_capacity) {
    size_t new_cap = compiler_opts.include_paths_capacity;
    new_cap = new_cap == 0 ? 4 : new_cap * 2;

    char **new_include_paths = arena_alloc(&long_lived, new_cap * sizeof(char *));
    memcpy(new_include_paths, compiler_opts.include_paths,
           compiler_opts.include_paths_count * sizeof(char *));

    compiler_opts.include_paths = new_include_paths;
  }

  compiler_opts.include_paths[compiler_opts.include_paths_count++] =
      str_dup(include);

  entry = arena_alloc(&long_lived, sizeof(StringEntry));
  entry->name = str_dup(include);
  HASH_ADD_KEYPTR(hh, library_path_set, entry->name, strlen(entry->name), entry);
}

void append_header_string(char *str) {
  char *header = str;

  char str_buffer[512] = {0};

  size_t len = strlen(header);
  if (len >= 2 && header[0] != '"' && header[len - 1] != '"') {
    sprintf(str_buffer, "\"%s\"", header);
    header = str_buffer;
  }

  StringEntry *entry;
  HASH_FIND_STR(header_set, header, entry);

  if (entry) {
    return;
  }

  if (compiler_opts.local_headers_count >=
      compiler_opts.local_headers_capacity) {
    size_t new_cap = compiler_opts.local_headers_capacity;
    new_cap = new_cap == 0 ? 4 : new_cap * 2;

    char **new_headers = arena_alloc(&long_lived, new_cap * sizeof(char *));
    memcpy(new_headers, compiler_opts.local_headers,
           compiler_opts.local_headers_count * sizeof(char *));

    compiler_opts.local_headers = new_headers;
  }

  compiler_opts.local_headers[compiler_opts.local_headers_count++] =
      str_dup(header);

  entry = arena_alloc(&long_lived, sizeof(StringEntry));
  entry->name = str_dup(header);
  HASH_ADD_KEYPTR(hh, header_set, entry->name, strlen(entry->name), entry);
}

void append_system_header_string(char *str) {
  char *header = str;

  char str_buffer[512] = {0};

  size_t len = strlen(header);
  if (len >= 2 && header[0] == '"' && header[len - 1] == '"') {
    sprintf(str_buffer, "%s", header);
    header = str_buffer;
  }

  StringEntry *entry;
  HASH_FIND_STR(system_header_set, header, entry);

  if (entry) {
    return;
  }

  if (compiler_opts.system_headers_count >=
      compiler_opts.system_headers_capacity) {
    size_t new_cap = compiler_opts.system_headers_capacity;
    new_cap = new_cap == 0 ? 4 : new_cap * 2;

    char **new_headers = arena_alloc(&long_lived, new_cap * sizeof(char *));
    memcpy(new_headers, compiler_opts.system_headers,
           compiler_opts.system_headers_count * sizeof(char *));

    compiler_opts.system_headers = new_headers;
  }

  compiler_opts.system_headers[compiler_opts.system_headers_count++] =
      str_dup(header);

  entry = arena_alloc(&long_lived, sizeof(StringEntry));
  entry->name = str_dup(header);
  HASH_ADD_KEYPTR(hh, system_header_set, entry->name, strlen(entry->name), entry);
}

char *flatten_strings(char **strings, size_t count, char compiler_arg) {
  size_t total_length = count * 3; // account for "-<compiler arg>" and " "
  for (size_t i = 0; i < count; i++) {
    total_length += strlen(strings[i]);
  }

  char *result = arena_alloc(&long_lived, total_length + 1);
  if (result == NULL) {
    return NULL;
  }

  memset(result, 0, total_length + 1);

  char *ptr = result;

  // Copy strings into result
  for (size_t i = 0; i < count; i++) {
    *ptr++ = '-';
    *ptr++ = compiler_arg;

    size_t len = strlen(strings[i]);
    memcpy(ptr, strings[i], len);
    ptr += len;

    *ptr++ = ' ';
  }

  return result;
}

char *release_mode_string(void) {
  bool is_clang_like = (strstr(compiler_opts.compiler, "clang") != NULL ||
                        strcmp(compiler_opts.compiler, "cc") == 0);

  switch (compiler_opts.release_mode) {
  case RELEASE_DEBUG: {
    // Debug flags same for both
    if (is_clang_like) {
      return "-g3 -O0 -Wall -Wextra -Wpedantic "
             "-fsanitize=address,undefined";
    } else {
      return "-g3 -O0 -Wall -Wextra -Wpedantic "
             "-fsanitize=address,leak,undefined";
    }
  }

  case RELEASE_SMALL:
    if (is_clang_like) {
      return "-Os -fdata-sections -Wl,-dead_strip";
    } else {
      return "-Os -ffunction-sections -fdata-sections -Wl,--gc-sections";
    }

  case RELEASE_DEFAULT:
    if (is_clang_like) {
      return "-O2 -march=native -flto";
    } else {
      return "-O2 -march=native -flto";
    }

  default:
    return "-O0";
  }
}

void print_usage(const char *program_name) {
  printf("Pebble Compiler\n");
  printf("Usage: %s [options] <source_file>\n\n", program_name);
  printf("Options:\n");
  printf("  -v, --verbose        Enable verbose output\n");
  printf("  -w, --warnings       Enable C compiler warnings\n");
  printf("  --keep-c             Keep generated C file (default)\n");
  printf("  --no-keep-c          Remove generated C file after compilation\n");
  printf(
      "  --generate-only      Only generate the C source without compiling\n");
  printf("  --compiler           Specify the compiler used when compiling C "
         "(autodetects gcc/clang/cc depending on your computer)\n");
  printf("  --no-main            No entry point to the program. Compiles to an "
         "object only.\n");
  printf("  -o <name>            Specify output executable name (default: "
         "output)\n");
  printf("  -c <name>            Specify output c file name (default: "
         "output.c)\n");
  printf("  -l <library>         Specify library to include\n");
  printf("  -L <path>            Specify library path to add to path\n");
  printf("  -I <path>            Specify include path to add to path\n");
  printf("  --header <name>      Specify header to include in source\n");
  printf("  --sys-header <name>  Specify system header to include in source\n");
  printf("  --test               Run all tests\n");
  printf("  --test-lexer         Run lexer tests\n");
  printf("  --test-parser        Run parser tests\n");
  printf("  --test-checker       Run checker tests\n");
  printf("  --test-all           Run all tests\n");
  printf("\nModule Paths:\n");
  printf("  --std-path           Location of std lib (default: alongside compiler)\n");
  printf("\nFreestanding Options:\n");
  printf("  --freestanding       Generate freestanding code (no standard "
         "library)\n");
  printf("  --entry-point        Entry point of your code (default main)\n");
  printf("  --shared             Compile as a shared library\n");
  printf("\nRelease Options:\n");
  printf("  --debug              Compile in debug mode\n");
  printf("  --release-small      Compile for a smaller binary\n");
  printf(
      "  --release            Compile with standard release compiler flags\n");
  printf("\nOther Options:\n");
  printf("  -h, --help           Show this help message\n");
}

bool parse_args(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--freestanding") == 0) {
      compiler_opts.freestanding = true;
    } else if (strcmp(argv[i], "-v") == 0 ||
               strcmp(argv[i], "--verbose") == 0) {
      compiler_opts.verbose = true;
    } else if (strcmp(argv[i], "-w") == 0 ||
               strcmp(argv[i], "--warnings") == 0) {
      compiler_opts.warnings = true;
    } else if (strcmp(argv[i], "--keep-c") == 0) {
      compiler_opts.keep_c_file = true;
    } else if (strcmp(argv[i], "--no-keep-c") == 0) {
      compiler_opts.keep_c_file = false;
    } else if (strcmp(argv[i], "--generate-only") == 0) {
      compiler_opts.keep_c_file = true;
      compiler_opts.generate_only = true;
    } else if (strcmp(argv[i], "-o") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: -o requires an argument\n");
        return false;
      }
      compiler_opts.output_exe_name = argv[++i];
    } else if (strcmp(argv[i], "-l") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: -l requires an argument\n");
        return false;
      }

      append_library_string(argv[++i]);
    } else if (strncmp(argv[i], "-l", 2) == 0) {
      // allow -l<lib> variant where lib immediately
      size_t len = strlen(argv[i]) - 2;
      if (len == 0) {
        fprintf(stderr, "Error: -l requires an argument\n");
        return false;
      }

      // skip "-l"
      append_library_string(argv[i] + 2);
    } else if (strcmp(argv[i], "-L") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: -L requires an argument\n");
        return false;
      }

      append_library_path_string(argv[++i]);
    } else if (strncmp(argv[i], "-L", 2) == 0) {
      // allow -L<lib> variant where lib immediately
      size_t len = strlen(argv[i]) - 2;
      if (len == 0) {
        fprintf(stderr, "Error: -L requires an argument\n");
        return false;
      }

      // skip "-L"
      append_library_path_string(argv[i] + 2);
    } else if (strcmp(argv[i], "-I") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: -I requires an argument\n");
        return false;
      }

      append_include_path_string(argv[++i]);
    } else if (strncmp(argv[i], "-I", 2) == 0) {
      // allow -I<path> variant
      size_t len = strlen(argv[i]) - 2;
      if (len == 0) {
        fprintf(stderr, "Error: -I requires an argument\n");
        return false;
      }

      // skip "-I"
      append_include_path_string(argv[i] + 2);
    } else if (strcmp(argv[i], "--header") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --header requires an argument\n");
        return false;
      }

      append_header_string(argv[++i]);
    } else if (strcmp(argv[i], "--sys-header") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --sys-header requires an argument\n");
        return false;
      }

      append_system_header_string(argv[++i]);
    } else if (strcmp(argv[i], "-c") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: -c requires an argument\n");
        return false;
      }
      compiler_opts.output_c_name = argv[++i];
    } else if (strcmp(argv[i], "--compiler") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --compiler requires an argument\n");
        return false;
      }
      compiler_opts.compiler = argv[++i];
    } else if (strcmp(argv[i], "--entry-point") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --entry-point requires an argument\n");
        return false;
      }
      compiler_opts.entry_point = argv[++i];
    } else if (strcmp(argv[i], "--std-path") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: --std-path requires an argument\n");
        return false;
      }
      compiler_opts.std_path = argv[++i];
    } else if (strcmp(argv[i], "--no-main") == 0) {
      compiler_opts.has_main = false;
    } else if (strcmp(argv[i], "--shared") == 0) {
      compiler_opts.library = LIBRARY_SHARED;
      compiler_opts.has_main = false;
    } else if (strcmp(argv[i], "--debug") == 0) {
      compiler_opts.release_mode = RELEASE_DEBUG;
    } else if (strcmp(argv[i], "--release-small") == 0) {
      compiler_opts.release_mode = RELEASE_SMALL;
    } else if (strcmp(argv[i], "--release") == 0) {
      compiler_opts.release_mode = RELEASE_DEFAULT;
    } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      print_usage(argv[0]);
      exit(0);
    } else if (strcmp(argv[i], "--test") == 0 ||
               strcmp(argv[i], "--test-lexer") == 0 ||
               strcmp(argv[i], "--test-parser") == 0 ||
               strcmp(argv[i], "--test-checker") == 0 ||
               strcmp(argv[i], "--test-all") == 0) {
      fprintf(stderr, "Test mode not yet implemented\n");
      return false;
    } else if (argv[i][0] == '-') {
      fprintf(stderr, "Error: Unknown option '%s'\n", argv[i]);
      print_usage(argv[0]);
      return false;
    } else {
      // First non-flag argument is the input file
      if (compiler_opts.input_file != NULL) {
        fprintf(stderr, "Error: Multiple input files specified\n");
        return false;
      }
      compiler_opts.input_file = argv[i];
    }
  }

  // Validate compiler after args
  if (compiler_opts.compiler == NULL ||
      strcmp(compiler_opts.compiler, "") == 0) {
    auto_detect_compiler(); // Fallback if user set invalid
  }

  if (compiler_opts.compiler != NULL) { // Null-check
    char cmd[256];                      // Buffer for command; adjust if needed
    snprintf(cmd, sizeof(cmd), "command -v %s >/dev/null 2>&1",
             compiler_opts.compiler);

    int compiler_check = system(cmd);
    if (compiler_check != 0) {
      fprintf(stderr, "Error: Specified compiler '%s' not found in PATH.\n",
              compiler_opts.compiler);
      return false;
    }
  } else {
    fprintf(stderr, "Error: No compiler specified or detected.\n");
    return false;
  }

  return true;
}
