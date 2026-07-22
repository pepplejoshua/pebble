# Parser Implementation Slices

The parser phase is delivered as two bounded slices. It must not become one
phase-mixed replacement for `parser2.c`.

## Slice 2A: parser foundation

Implementation:

1. parser diagnostic constants and cursor/token buffering;
2. immutable `Tree`, opaque `NodeID`, node storage, and deterministic dump;
3. missing/error nodes and local delimiter/list recovery;
4. type grammar excluding aggregate methods that require full declarations;
5. precedence parser for literals, prefix/binary/cast operations, calls,
   neutral bracket application, slices, paths, members, tuples, and arrays;
6. parser fragment harness used only by conformance tests.

Slice 2A deliberately postpones complete files, function/record literals with
statement bodies, declarations, and control-flow statements. It proves the
hard expression/type foundation before more syntax depends on it.

Conformance layout:

```text
tests/parser/
  expression/valid/*.peb
  expression/invalid/<CODE>/*.peb
  type/valid/*.peb
  type/invalid/<CODE>/*.peb
  recovery/*.peb
```

Each fragment file contains only the Pebble expression or type selected by its
directory. Tests invoke unexported fragment entry points and require EOF after
the fragment. Optional `.ast.golden` files assert precedence or a recovery
shape; most cases need only a `.peb` file and expected success/code.

Slice 2A completion criteria:

- every precedence level has boundary and associativity cases;
- `identity[int](x)` and `functions[i](x)` produce the same neutral bracket
  node shape before resolution;
- slice syntax remains structurally distinct;
- all valid type forms parse, including nested generics and function types;
- invalid input cannot panic, hang, or exceed configured limits;
- `go test ./...` and `go vet ./...` pass.

## Slice 2B: complete files

Implementation:

1. imports, bindings, type declarations, functions, extern items, and blocks;
2. all statements and assignment forms;
3. function literals, record literals, aggregate types, fields, variants, and
   methods;
4. statement/declaration/switch/aggregate recovery;
5. complete-file `Parse` API and source-driven runner;
6. migration corpus for the standard library and examples.

Conformance layout:

```text
tests/parser/
  file/valid/*.peb
  file/invalid/<CODE>/*.peb
  recovery/*.peb
```

Existing parser cases move into this structure. Invalid directories assert the
diagnostic code. Recovery cases normally use `.stderr.golden` and
`.ast.golden` sidecars because multiple errors and surviving declarations are
observable behavior.

The standard library and examples become complete-file parser tests after their
prototype `.[...]` generic syntax is migrated. Removed names such as `float`,
`isize`, and `usize` remain syntactically valid identifiers, so they are parser
valid; they must still be migrated before the later semantic corpus can pass. A
corpus report classifies every failure as:

- source requires mechanical migration to an accepted syntax;
- intentional language correction;
- new parser bug;
- old parser bug;
- unresolved specification decision.

Slice 2B completion criteria:

- the target-syntax stdlib and examples parse without parser diagnostics;
- every supported declaration, statement, expression, and type has a plain
  source conformance case;
- recovery preserves later top-level declarations and later block statements;
- the parser emits no semantic diagnostics;
- complete-file parsing is deterministic;
- `go test ./...` and `go vet ./...` pass.

## Focused Go tests

Go unit tests remain appropriate for internal invariants that are not Pebble
program behavior:

- cursor lookahead and EOF stability;
- node/list arena bounds;
- source-span joining;
- nesting and diagnostic limits;
- failing internal writers used by deterministic tree dumps.

They do not replace source files for ordinary syntax behavior and must not
construct large trees by hand.

## Fuzzing boundary

After Slice 2B, add Go fuzz targets for:

1. arbitrary bytes through `FileSet.Add`, lexer, and parser;
2. valid UTF-8 token noise through complete-file parsing;
3. mutation of parser corpus files.

Properties: no panic, no hang, bounded diagnostics/nesting, valid spans, and
deterministic results. Every minimized failure becomes an ordinary `.peb`
regression case.
