# Driver and CLI

## Driver responsibilities

The driver:

- parses options;
- creates a compilation;
- selects the stopping phase;
- asks phases to run;
- renders diagnostics;
- writes requested outputs atomically;
- invokes the C toolchain with an argument vector;
- removes only temporary files it owns;
- maps the result to an exit status.

Semantic and backend packages do not inspect global CLI options.

## Modes to preserve or decide

The current prototype exposes parse-only, check-only, generate-C-only,
freestanding, no-main, custom entry point, release mode, warning mode, include
paths, library paths, libraries, local/system headers, custom C compiler, C
flags, and generated-C retention.

Each option must state which phase consumes it. Options that influence semantic
or ABI results become part of relevant cache keys.

## Toolchain invocation

The Go driver uses `exec.Command` with separate arguments. It must not build a
shell command string or use the shell for deletion. This provides correct
quoting, avoids fixed command buffers, and makes the invoked command printable
without making the printed form executable input.

Toolchain diagnostics are preserved. Verbose mode prints normalized phase and
command information useful for reproducing failures.

## Output ownership

Generated files are written to temporary siblings and renamed on success.
Failure must not destroy a pre-existing requested output. Clean-up follows one
driver-level path rather than being duplicated at each early return.
