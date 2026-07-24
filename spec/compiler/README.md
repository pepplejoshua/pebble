# Pebble Compiler Specification

This directory is the source of truth for the Pebble compiler rewrite.

The specification describes the language and the contracts between compiler
phases. It does not preserve behavior merely because the prototype implements
it. Every statement uses one of these labels:

- **Required**: intended language or compiler behavior.
- **Current**: behavior observed in the C prototype, not yet accepted.
- **Proposed**: concrete design awaiting a decision.
- **Open**: a decision that must be made before the affected phase is stable.

## Documents

1. [Pipeline](01-pipeline.md)
2. [Source and lexing](02-source-and-lexing.md)
   - [Token inventory](02a-token-inventory.md)
   - [Literals and interpolation](02b-literals-and-interpolation.md)
3. [Syntax and surface tree](03-syntax-and-ast.md)
   - [Parser grammar](03a-grammar.md)
   - [Surface-tree inventory](03b-surface-tree.md)
   - [Parser recovery](03c-parser-recovery.md)
   - [Parser implementation slices](03d-parser-slices.md)
4. [Modules, names, and scopes](04-modules-and-names.md)
   - [04a Module graph](04a-module-graph.md)
   - [04b Name resolution](04b-name-resolution.md)
5. [Types and inference](05-types-and-inference.md)
   - [Semantic type store](05a-semantic-type-store.md)
   - [Algebraic inference](05b-algebraic-inference.md)
6. [Checking, validation, and typed IR](06-checking-and-conversions.md)
   - [06a Semantic fact generation](06a-semantic-fact-generation.md)
   - [06b Semantic validation and typed IR](06b-validation-and-typed-ir.md)
7. [Generics](07-generics.md)
8. [Diagnostics](08-diagnostics.md)
9. [Typed IR and caching](09-typed-ir-and-caching.md)
10. [C backend and runtime ABI](10-c-backend-and-runtime.md)
11. [Driver and CLI](11-driver-and-cli.md)
12. [Testing](12-testing.md)
13. [Pastel formatting](13-pastel.md)
14. [Open decisions](OPEN-DECISIONS.md)

## Rewrite rule

The old C compiler is a behavioral reference, not the design. When the
prototype, README, standard library, and examples disagree, record the
disagreement and decide it deliberately. Do not make the Go implementation
guess.

## Pipeline-wide invariants

- A `Compilation` owns all mutable state for one invocation.
- No semantic phase depends on package-global current-module or current-scope
  state.
- Source text and the surface tree remain immutable after parsing.
- Diagnostics are collected as data and rendered by the driver.
- Failed programs may contain error types, but later phases must not panic.
- Code generation only accepts a successfully checked and lowered program.
- Iteration order must not change semantics or diagnostic order.
