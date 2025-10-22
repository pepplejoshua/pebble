#include "options.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

CompilerOptions compiler_opts = {0};

void initialise_args()
{
    // Initialize defaults
    compiler_opts.freestanding = false;
    compiler_opts.release_mode = RELEASE_DEBUG;
    compiler_opts.verbose = false;
    compiler_opts.keep_c_file = true;
    compiler_opts.output_name = "output";
    compiler_opts.input_file = NULL;
}

char* release_mode_string() {
    // TODO: Add/configure more flags to release modes later
    switch (compiler_opts.release_mode) {
    case RELEASE_DEBUG:
        return "-g3 -O0 -Wall -Wextra -Wpedantic -fsanitize=address -fsanitize=undefined";
        
    case RELEASE_SMALL:
        return "-Os -ffunction-sections -fdata-sections -Wl,--gc-sections -s";

    case RELEASE_DEFAULT:
        return "-O2 -march=native -flto -s";

    default:
        return "-O0";
    }

    __builtin_unreachable();
}

void print_usage(const char *program_name)
{
    printf("Pebble Compiler\n");
    printf("Usage: %s [options] <source_file>\n\n", program_name);
    printf("Options:\n");
    printf("  --freestanding       Generate freestanding code (no standard library)\n");
    printf("  -v, --verbose        Enable verbose output\n");
    printf("  --keep-c             Keep generated C file (default)\n");
    printf("  --no-keep-c          Remove generated C file after compilation\n");
    printf("  -o <name>            Specify output executable name (default: output)\n");
    printf("  --test               Run all tests\n");
    printf("  --test-lexer         Run lexer tests\n");
    printf("  --test-parser        Run parser tests\n");
    printf("  --test-checker       Run checker tests\n");
    printf("  --test-all           Run all tests\n");
    printf("\nRelease Options:\n");
    printf("  --debug              Compile in debug mode\n");
    printf("  --release-small      Compile for a smaller binary\n");
    printf("  --release            Compile with standard release compiler flags\n");
    printf("\nOther Options:\n");
    printf("  -h, --help           Show this help message\n");
}

bool parse_args(int argc, char **argv)
{
    for (int i = 1; i < argc; i++)
    {
        if (strcmp(argv[i], "--freestanding") == 0)
        {
            compiler_opts.freestanding = true;
        }
        else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0)
        {
            compiler_opts.verbose = true;
        }
        else if (strcmp(argv[i], "--keep-c") == 0)
        {
            compiler_opts.keep_c_file = true;
        }
        else if (strcmp(argv[i], "--no-keep-c") == 0)
        {
            compiler_opts.keep_c_file = false;
        }
        else if (strcmp(argv[i], "-o") == 0)
        {
            if (i + 1 >= argc)
            {
                fprintf(stderr, "Error: -o requires an argument\n");
                return false;
            }
            compiler_opts.output_name = argv[++i];
        }
        else if (strcmp(argv[i], "--debug") == 0)
        {
            compiler_opts.release_mode = RELEASE_DEBUG;
        }
        else if (strcmp(argv[i], "--release-small") == 0)
        {
            compiler_opts.release_mode = RELEASE_SMALL;
        }
        else if (strcmp(argv[i], "--release") == 0)
        {
            compiler_opts.release_mode = RELEASE_DEFAULT;
        }
        else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0)
        {
            print_usage(argv[0]);
            exit(0);
        }
        else if (strcmp(argv[i], "--test") == 0 ||
                 strcmp(argv[i], "--test-lexer") == 0 ||
                 strcmp(argv[i], "--test-parser") == 0 ||
                 strcmp(argv[i], "--test-checker") == 0 ||
                 strcmp(argv[i], "--test-all") == 0)
        {
            fprintf(stderr, "Test mode not yet implemented\n");
            return false;
        }
        else if (argv[i][0] == '-')
        {
            fprintf(stderr, "Error: Unknown option '%s'\n", argv[i]);
            print_usage(argv[0]);
            return false;
        }
        else
        {
            // First non-flag argument is the input file
            if (compiler_opts.input_file != NULL)
            {
                fprintf(stderr, "Error: Multiple input files specified\n");
                return false;
            }
            compiler_opts.input_file = argv[i];
        }
    }

    return true;
}