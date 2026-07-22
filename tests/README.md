# Pebble Source Tests

Language behavior is specified primarily with `.peb` source files.

```text
<phase>/valid/    the selected phase must succeed
<phase>/invalid/<CODE>/  the selected phase must emit that error code
```

For example:

```text
lexer/invalid/L0002/malformed_numeric_separators.peb
```

Every error emitted for that source file must have the expected `L0002` code.
Keeping the expectation in the directory makes each test an ordinary Pebble
source file without a Go filename map or sidecar metadata.

The files under `tests/lexer` describe the new lexer contract in
`spec/compiler/02*.md`. The fragment files under `tests/parser/expression` and
`tests/parser/type` describe parser Slice 2A. Parser fragment files contain
exactly one expression or type and are required to reach EOF after it. Some
cases deliberately differ from the C prototype and are not expected to pass
it. The Go syntax test runner discovers and executes them automatically.

An ordinary test needs only its `.peb` source file. Optional
`.stderr.golden`, `.stdout.golden`, or `.tokens.golden` sidecars are reserved
for cases where exact output matters. A normal test run never rewrites a
golden file.

Every crash or fuzzing discovery should be reduced to a small `.peb` case and
kept here permanently.
