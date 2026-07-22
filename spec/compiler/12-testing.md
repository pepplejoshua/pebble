# Testing

## Principle

Language behavior is tested primarily with Pebble source files. Tests must not
construct syntax trees by hand merely to exercise ordinary language rules.

## Source test layout

**Proposed:**

```text
tests/
  lexer/valid/*.peb
  lexer/invalid/<CODE>/*.peb
  parser/expression/valid/*.peb
  parser/expression/invalid/<CODE>/*.peb
  parser/type/valid/*.peb
  parser/type/invalid/<CODE>/*.peb
  parser/file/valid/*.peb
  parser/file/invalid/<CODE>/*.peb
  parser/recovery/*.peb
  checker/valid/*.peb
  checker/invalid/*.peb
  runtime/valid/*.peb
  modules/<case>/main.peb
  modules/<case>/...
```

Directory placement supplies the minimum expectation:

- `valid`: the selected phase must succeed;
- `invalid/<CODE>`: the selected phase must fail and every emitted error must
  have the code named by the directory;
- `runtime/valid`: compile and execute successfully.

That means a new basic test can be only a `.peb` file.

## Optional golden files

A sidecar is added only when exact observable output matters:

```text
invalid/bad_add.peb
invalid/bad_add.stderr.golden

valid/hello.peb
valid/hello.stdout.golden
```

The runner compares actual output to the checked-in golden file and prints a
unified diff on mismatch. A deliberate update command rewrites goldens for
review; normal test runs never update them.

Golden diagnostics use normalized relative paths, no color, stable ordering,
and no platform-specific temporary directories.

## Verbose failure report

For every unexpected result, the runner prints:

- case path and selected compiler phase;
- expected and actual exit status;
- rendered Pebble diagnostics;
- golden diff when present;
- generated-C/toolchain diagnostics for backend cases;
- executable stdout/stderr for runtime cases.

## In-source diagnostic markers

**Open:** later support compact markers for local diagnostic assertions, for
example a comment pointing at an expected error. This can be useful but is not
required for the first runner; sidecars keep the source language uncontaminated
and allow testing the full rendered diagnostic.

## Unit tests

Small Go unit tests remain appropriate for algorithms whose behavior is not a
Pebble program by itself: union-find, source line indexing, graph algorithms,
type interning, layout calculation, and argument escaping. They complement,
not replace, source tests.

## Differential tests

During the rewrite, run old and new compilers over the same source corpus.
Differences are classified as:

- intentional spec correction;
- new compiler bug;
- old compiler bug;
- unspecified behavior requiring a decision.

Generated C need not be text-identical. Executable behavior, diagnostics, and
defined ABI properties are the useful comparisons.

## Fuzzing

Fuzz in stages:

1. arbitrary bytes into the lexer/parser: never hang or panic;
2. token-aware malformed programs: bounded recovery and diagnostics;
3. grammar-generated valid programs: parse/check deterministically;
4. typed program generation: compare interpreter or generated executable
   behavior when an oracle exists;
5. mutation of real standard-library and example programs.

Every discovered crash or hang is reduced and committed as an ordinary `.peb`
regression test.
