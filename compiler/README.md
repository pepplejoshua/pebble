# Pebble Compiler

This directory contains the permanent Go implementation of the Pebble
compiler.

The C prototype remains in `../src` while behavior is specified and replaced.
It is a behavioral reference, not the architecture for this implementation.
The compiler specification lives in `../spec/compiler` and the source-driven
language tests live in `../tests`.

## Planned shape

```text
cmd/pebc/            command-line entry point
internal/driver/     compilation orchestration
internal/source/     immutable source files and spans
internal/diagnostic/ structured diagnostics and renderers
internal/syntax/     lexer, parser, and immutable surface tree
internal/module/     import loading and module graph
internal/symbol/     declarations, scopes, and name resolution
internal/types/      concrete TypeID store and interning
internal/sema/       constraints, inference, and checking
internal/hir/        resolved typed IR
internal/mono/       generic specialization
internal/lower/      backend-independent lowering
internal/backend/c/  C emission
internal/toolchain/  C compiler invocation
```

Directories are added when their first real implementation slice begins. We do
not create empty packages merely to mirror this diagram.

## Current implementation

The first vertical slice is complete:

- `internal/source` stores immutable UTF-8 files, byte spans, line indexes, and
  display positions;
- `internal/diagnostic` collects structured diagnostics and renders stable
  color-free text;
- `internal/syntax` defines tokens and implements the lexer, including strict
  literals and nested interpolation modes;
- `internal/syntax` also owns the immutable surface tree and complete-file
  parser: declarations, statements, expression precedence, shared bracket
  terms, type syntax, and bounded local recovery;
- the syntax tests discover the valid and invalid `.peb` files under
  `../tests/lexer` and the expression/type fragments under `../tests/parser`.

The parser corpus also runs the standard library and examples through the Go
implementation so syntax migrations and parser regressions fail ordinary tests.

Run the compiler tests from this directory:

```sh
go test ./... -parallel 16
```

The backend package's tests are dominated by end-to-end cases that shell
out to `cc` and run the resulting binary, so most of the wall time is
spent waiting on subprocesses rather than burning CPU — `-parallel 16`
(double the default 8-core `GOMAXPROCS`) cuts the backend suite from
~227s to ~169s on an 8-core machine. Going higher (e.g. 24) compiles
faster in aggregate but causes enough CPU contention that the
bounded-loop tests' 5s execution timeout (`loopExecutionTimeout` in
`internal/backend/emit_test.go`) starts false-failing — 16 is the
highest value confirmed stable.

## Implementation rules

- One compilation owns its mutable state; packages do not expose ambient
  current-module or current-scope globals.
- Source text and the surface tree are immutable after construction.
- Inference variables never enter the concrete type interner.
- Diagnostics are values and are rendered only at the driver boundary.
- Language behavior is exercised primarily through `.peb` files in `../tests`.
- A clean build must be deterministic before persistent caching is attempted.

## Migration

The Go compiler replaces the prototype phase by phase:

1. source database, lexer, and lexer test runner;
2. parser and recovery;
3. modules and name resolution;
4. type storage, inference, and semantic checking;
5. typed IR, generic specialization, and lowering;
6. C backend, runtime ABI, and toolchain driver.

The root build switches to this compiler only after the standard library,
examples, and source test corpus pass through the Go implementation.
