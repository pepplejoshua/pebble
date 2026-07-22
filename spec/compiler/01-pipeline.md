# Compiler Pipeline

## Required pipeline

```text
arguments
  -> source loading
  -> lexing
  -> parsing
  -> module graph construction
  -> declaration collection
  -> name resolution
  -> type declaration resolution
  -> signature checking
  -> body constraint generation and solving
  -> typed IR construction
  -> generic specialization
  -> lowering
  -> C emission
  -> C toolchain invocation
  -> artifact
```

Each arrow is an API boundary with explicit inputs, outputs, and diagnostics.
Phases must not obtain ambient state from globals.

## Whole-program phase order

Declaration collection happens for every reachable module before any function
body is checked. Type declarations and signatures are then resolved for the
whole module graph. Function bodies may only be checked once every referenced
module has a usable declaration surface.

The module graph, not hash-table iteration or an import-count score, determines
ordering. Strongly connected components must be handled explicitly according
to the module-cycle policy.

## Compilation state

**Proposed:**

```go
type Compilation struct {
    Sources     *source.FileSet
    Diagnostics *diagnostic.DiagnosticSet
    Modules     *module.Graph
    Symbols     *symbol.Store
    Types       *types.Store
    Options     Options
}
```

Each phase receives `*Compilation` plus stable IDs for the items it operates
on. APIs return values or IDs rather than changing a global current item.

## Error boundary

- Lexer and parser errors prevent semantic checking of irrecoverable syntax,
  but a recovered AST may still be produced for diagnostics and tooling.
- Semantic errors prevent lowering and code generation.
- Backend errors prevent toolchain invocation.
- Toolchain failures are reported separately from Pebble diagnostics.
- The driver owns exit status. Individual phases never terminate the process.

## Determinism

Given identical source files, options, target, compiler version, and runtime
ABI version, Pebble must produce diagnostics in the same order and equivalent
generated C. Maps may be used internally, but observable output must use a
stable ordering.

## Prototype findings

**Current:** `src/main.c` owns phase orchestration, cleanup, generated-file
management, C command construction, and artifact selection in one function.
The checker mixes resolution, inference, AST rewriting, generic
specialization, and validation. The rewrite must split these responsibilities
rather than translate the existing file structure.
