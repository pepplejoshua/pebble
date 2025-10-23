#include "options.h"

#include "alloc.h"
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

void initialise_args() {
  // Initialize defaults
  compiler_opts.freestanding = false;
  compiler_opts.release_mode = RELEASE_DEBUG;
  compiler_opts.compiler = NULL;
  compiler_opts.verbose = false;
  compiler_opts.keep_c_file = true;
  compiler_opts.output_exe_name = "output";
  compiler_opts.output_c_name = "output.c";
  compiler_opts.has_main = true;
  compiler_opts.library = LIBRARY_NONE;
  compiler_opts.entry_point = "main";
  compiler_opts.input_file = NULL;

  auto_detect_compiler();
}

char *release_mode_string() {
  bool is_clang_like = (strstr(compiler_opts.compiler, "clang") != NULL ||
                        strcmp(compiler_opts.compiler, "cc") == 0);

  switch (compiler_opts.release_mode) {
  case RELEASE_DEBUG:
    // Debug flags same for both
    return "-g3 -O0 -Wall -Wextra -Wpedantic -fsanitize=address "
           "-fsanitize=undefined";

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
  printf("  --keep-c             Keep generated C file (default)\n");
  printf("  --no-keep-c          Remove generated C file after compilation\n");
  printf("  --compiler           Specify the compiler used when compiling C "
         "(autodetects gcc/clang/cc depending on your computer)\n");
  printf("  --no-main            No entry point to the program. Compiles to an object only.\n");
  printf("  -o <name>            Specify output executable name (default: "
         "output)\n");
  printf("  -c <name>            Specify output c file name (default: "
         "output.c)\n");
  printf("  --test               Run all tests\n");
  printf("  --test-lexer         Run lexer tests\n");
  printf("  --test-parser        Run parser tests\n");
  printf("  --test-checker       Run checker tests\n");
  printf("  --test-all           Run all tests\n");
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
    } else if (strcmp(argv[i], "--keep-c") == 0) {
      compiler_opts.keep_c_file = true;
    } else if (strcmp(argv[i], "--no-keep-c") == 0) {
      compiler_opts.keep_c_file = false;
    } else if (strcmp(argv[i], "-o") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Error: -o requires an argument\n");
        return false;
      }
      compiler_opts.output_exe_name = argv[++i];
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
