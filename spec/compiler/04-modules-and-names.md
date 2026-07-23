# Modules, Names, and Scopes

This phase turns independently parsed source files into one explicit module
graph, then assigns stable semantic identities to authored declarations and
references. It never rewrites source names or mutates the surface tree.

The phase is specified by two independently named tasks:

1. [04a Module Graph](04a-module-graph.md) resolves imports, loads source
   files, assigns `ModuleID`s, and validates the dependency graph.
2. [04b Name Resolution](04b-name-resolution.md) collects declarations,
   builds scopes, assigns `ScopeID` and `SymbolID`, and resolves authored name
   references.

`04b` consumes the immutable graph produced by `04a`. Its contract may be
reviewed and tested in parallel with `04a`, but its implementation integrates
only against the committed `04a` API.

## Shared invariants

- A `Compilation` owns module, scope, and symbol stores for one invocation.
- Source text and surface trees remain immutable.
- Compiler identity uses stable IDs, never generated display strings.
- Absolute host paths are not semantic identities or user-facing names.
- Imports do not inject their members into the importing module.
- Diagnostics are values with stable codes and authored source spans.
- Observable module, scope, symbol, and diagnostic order is deterministic.
- Failed resolution may produce error IDs for continued diagnostics, but later
  phases must not infer meaning from them.

## Identity versus spelling

These are separate values:

```text
authored spelling     "new"
semantic identity     SymbolID(42)
backend C spelling    "peb_std_mem_new"
```

The resolver records `SymbolID(42)` for the syntax reference. It does not
replace `new` with `mem__new`. Backend symbol spelling is assigned only after
successful checking and lowering.

## First-contract language decisions

- Every module cycle is rejected.
- Types, values, functions, and module qualifiers share one lexical namespace.
- A declaration may not duplicate another declaration in the same scope.
- A declaration in an inner lexical scope may shadow an outer declaration.
- Parameters share the function body's outer scope, so that body cannot
  redeclare a parameter.
- Module declarations are collected before body resolution and may be
  referenced before their textual declaration.
- Local bindings enter scope only after their declaration syntax; local
  references cannot look forward.
- With no visibility syntax in the language, every module-level declaration is
  available through an explicit imported-module qualifier.

## Prototype findings

The C prototype tracks modules by absolute path, looks imported modules up by
basename, recursively propagates an `import_score`, mutates AST declarations
into names such as `mem__new`, and relies on global `current_scope` plus a
mutable current module. These mechanisms are behavioral evidence only. They
are not part of the rewrite contract.

## Phase boundary

After `04a` and `04b`, every resolvable source name has a `SymbolID`, every
scope has a `ScopeID`, and every module qualifier has a `ModuleID`. Types,
member access through runtime values, conversions, overload-like semantic
choices, and C names remain unresolved.
