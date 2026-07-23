# 04b Name Resolution

`04b Name Resolution` consumes the immutable `04a` graph and assigns stable
semantic identities to declarations, scopes, module qualifiers, and authored
name references. It never rewrites syntax spelling.

## Dependency

Implementation begins against the committed public API from
[04a Module Graph](04a-module-graph.md). Specification review, prototype
inventory, and source-case design may proceed while `04a` is being
implemented. Do not create a competing module graph or temporary path-based
symbol identity inside `04b`.

## Inputs and outputs

The concrete phase entry point is:

```go
type Config struct {
    MaxSymbols     uint32
    MaxScopes      uint32
    MaxScopeDepth  uint32
    MaxDiagnostics uint32
}

func Resolve(
    graph *module.Graph,
    sources *source.FileSet,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) *Result
```

`sources` is the same compilation-owned file set passed to `module.Build`.
The graph's `source.ID` values index that file set; a resolver must not reopen
files through the host filesystem. Nil or inconsistent inputs produce bounded
diagnostics rather than a panic.

The logical input relationship is:

```text
module.Graph
  -> ModuleID
  -> source.ID
  -> immutable syntax.Tree
  -> import qualifier edges
```

Output is a separate immutable resolution result:

```go
type SyntaxRef struct {
    Module ModuleID
    Node   syntax.NodeID
}

type Result struct {
    Scopes     *ScopeStore
    Symbols    *SymbolStore
    References // SyntaxRef -> SymbolID or error identity
    Qualifiers // SyntaxRef -> ModuleID
    Captures   // anonymous function -> referenced outer bindings
}
```

Maps above are conceptual. Public accessors return stable IDs and values in
deterministic order. The result does not store generated C names or semantic
`TypeID`s. The stores and result are owned by one `Result`; no package-global
current scope or current module participates in resolution.

Zero-valued limits select documented package defaults. Tests may lower every
limit. At minimum the implementation bounds symbols, scopes, scope depth, and
name-resolution diagnostics.

## Stable identities

```go
type ScopeID uint32
type SymbolID uint32
```

Zero is invalid. IDs live for one compilation snapshot and are assigned in
deterministic module/source order.

A symbol records at least:

- authored name and declaration span;
- `SymbolKind`;
- defining `ModuleID` and `ScopeID`;
- declaration `SyntaxRef`;
- containing type or function identity when applicable;
- an error state when collection recovered from damaged syntax.

Ordinary declarations use their declaration node as `SyntaxRef`. An imported
module qualifier uses the corresponding `ImportDecl` as its declaration
origin. `04b` pairs an `ImportEdge` with that node by the edge's retained span
and spelling; it does not ask `04a` to add semantic nodes or mutate the tree.

`SymbolID`, not a pointer, qualified string, or hash-table address, is semantic
declaration identity.

## Identity layers and checker handoff

The compiler keeps three different questions separate:

| Identity | Answers | Example use |
| --- | --- | --- |
| `SyntaxRef` | Which authored occurrence in which module? | the second use of `count` |
| `SymbolID` | Which declaration does that occurrence denote? | the local binding declared earlier |
| `TypeID` | What semantic type does it have? | `int` |

A `syntax.NodeID` is unique only inside one syntax tree. `SyntaxRef` combines
`module.ModuleID` with `syntax.NodeID`, making one unambiguous coordinate for
side tables across the complete compilation. Different `SyntaxRef` values may
therefore map to the same `SymbolID` when several authored uses denote one
declaration.

`04b` produces the first semantic link:

```text
SyntaxRef(name occurrence) -> SymbolID
```

The checking and inference phases consume the immutable syntax trees together
with this `Result`. They attach further facts in their own stores, for example:

```text
SymbolID(binding)       -> TypeID
SyntaxRef(expression)   -> TypeID
```

Those later stores do not decorate syntax nodes, replace authored names, or
use a node pointer as cross-phase identity. `04b` does not assign `TypeID`s;
it gives the checker the stable declaration identity needed to do so.

## Namespaces

The first contract uses one lexical namespace for:

- module qualifiers;
- types;
- functions;
- `let` and `var` bindings;
- parameters and loop bindings;
- type parameters.

Therefore a module cannot declare both `type Item` and `fn Item`, and an import
qualifier cannot share a module scope with another declaration of the same
name. Context still validates category: resolving `Item` to a type does not
make it legal in a value-only position.

Fields, variants, and methods occupy one member namespace owned by their
containing nominal type. Different nominal types may reuse member names. A
field and method with the same name in one type are duplicates.

## Scope tree

Scopes are explicit stored values, not a mutable global current scope.

- Every module has one module scope.
- Every function has a signature/body scope whose parent is its module scope
  or containing type environment.
- Parameters share the function body's outer scope.
- Every authored block creates a child lexical scope, except that the function
  body block reuses the function scope.
- A range-loop iterator belongs to a scope surrounding only the loop body.
- A C-style `for` owns a scope containing its initializer, condition, update,
  and body; the authored body block remains a child scope.
- Type parameters belong to the declaration environment visible to its
  signature and body.
- Methods retain their containing nominal type and defining module.
- Anonymous functions own a function scope and retain links to their defining
  lexical environment.

Scope creation order follows module and source order and is deterministic.

## Declaration collection

Collection runs for every reachable module before reference resolution.

Module collection includes imports, types, functions, extern items, and global
bindings. All valid module declarations may be referenced before their textual
position. Aggregate collection assigns identities to fields, variants, and
methods before method bodies are resolved.

Local bindings are not hoisted. A local becomes visible only after its name,
optional type, and initializer have been resolved. Its initializer therefore
sees an outer symbol of the same name, if one exists, rather than the binding
being declared.

Damaged declarations may receive error symbols to prevent cascades. Error
symbols never satisfy a successful semantic query.

## Duplicate and shadowing rules

- Two declarations with the same name in one scope are an error.
- Parameters share one scope and cannot duplicate one another.
- A function body's outer block cannot redeclare a parameter.
- An inner lexical scope may shadow a declaration from an ancestor lexical or
  module scope.
- Sibling scopes may reuse names independently.
- Shadowing is ordinary lookup behavior and does not emit a warning in the
  first contract.
- A declaration cannot shadow another declaration in the same scope by using
  a different symbol kind; the namespace is shared.

Lookup always starts from the reference's actual scope. The compiler never
prefers a module global merely because its generated backend name would be
unique.

## Lookup

Unqualified lookup:

```text
innermost lexical scope
-> ancestor lexical/function scopes
-> module scope
```

Imported declarations are never searched by unqualified lookup.

Qualified lookup for `module::member`:

1. Resolve `module` through ordinary unqualified lookup.
2. Require the selected symbol to be an imported-module qualifier.
3. Use its `ModuleID` to inspect the target module scope.
4. Resolve `member` in that module scope.

For a successful qualified reference, `References` maps the authored member
name to its `SymbolID`, while `Qualifiers` maps the authored qualifier/base
name to the selected `ModuleID`. The import declaration itself is represented
by its module-qualifier `SymbolID`; it is not a name reference.

There is no fallback from a failed qualified lookup to the current module.
With no visibility syntax yet, every successfully collected module-level
declaration is available through an imported qualifier.

Authored names are ASCII and case-sensitive under the current lexer contract.

## Members and bracket application

Name resolution handles lexical names, module paths, declarations, and
statically named aggregate members. Runtime member selection such as
`value.field` is recorded for later type-directed member resolution unless the
base already identifies a nominal declaration without type inference.

Neutral `BracketApply` is never classified from capitalization or spelling.
When ordinary resolution selects a generic type or generic callable directly,
its bracket arguments enter type-name resolution. When it selects a runtime
binding, arguments enter value-name resolution. Bases whose category depends
on expression typing remain explicitly unresolved for the checker; `04b` does
not guess.

The checker later validates arity, indexability, concrete types, and generic
requirements.

## Anonymous functions and captures

References from an anonymous function to bindings in an enclosing function are
recorded as captures in deterministic first-reference order. `04b` identifies
the captured `SymbolID`; it does not choose closure layout or claim the backend
supports that capture. The checker may diagnose an unsupported capture until
closure semantics are implemented.

Module globals and imported module members are not closure captures.

## Diagnostics

Initial stable codes:

| Code | Meaning |
| --- | --- |
| `N0001` | undefined name |
| `N0002` | duplicate declaration in one scope or member environment |
| `N0003` | qualifier does not identify an imported module |
| `N0004` | imported module has no requested member |
| `N0005` | name resolves to an invalid category for the syntax position |
| `N0006` | symbol-count, scope-count, scope-depth, or diagnostic limit |

Duplicate diagnostics label both the new and original declaration. Undefined
and qualified-member diagnostics point at authored reference spans. Resolution
continues with error identities where doing so prevents cascades.

Parser diagnostics do not forbid resolution of independent valid subtrees or
modules. Missing and error nodes are skipped or represented with error
identities as appropriate; malformed trees must not panic the resolver.

## Source-driven tests

```text
tests/names/
  valid/*.peb
  valid/multimodule/<case>/main.peb
  invalid/<CODE>/*.peb
  invalid/<CODE>/multimodule/<case>/main.peb
  recovery/*.peb
```

Required cases include module and local forward references, sequential local
bindings, same-scope duplicates across symbol kinds, nested shadowing,
parameter/body collision, sibling reuse, block lifetime, loop lifetime,
function type parameters, aggregate members and methods, qualified module
lookup, qualifier shadowing, missing module member, anonymous-function capture,
and neutral brackets whose category follows the resolved base.

Most behavior uses plain `.peb` files and expected diagnostic-code
directories. Optional `.symbols.golden`, `.scopes.golden`, or
`.resolution.golden` files are reserved for exact graph shape and recovery.

## Non-goals

`04b` does not:

- resolve semantic types or layouts;
- infer expression types;
- validate operator, call, conversion, or assignment compatibility;
- resolve runtime member access that requires a receiver type;
- validate generic requirements or specialize generics;
- mutate syntax or insert coercions;
- assign backend symbol names;
- implement closure layout.

## Completion criteria

- Every collected declaration has one deterministic `SymbolID` or explicit
  error identity.
- The resolver reads authored names from the compilation `source.FileSet` and
  performs no filesystem I/O.
- Every created scope has a deterministic `ScopeID` and documented parent.
- Every resolvable lexical and qualified reference maps to the intended ID.
- Module declarations support forward references; locals remain sequential.
- Duplicate, shadowing, parameter, loop, member, and qualifier rules match the
  contract.
- Anonymous-function captures are recorded without global mutable context.
- Neutral brackets use resolved identity where possible and remain explicit
  where type information is required.
- Syntax trees and authored names remain unchanged.
- Source cases cover every rule above.
- Resolution dumps and diagnostics are deterministic.
- Configured symbol, scope, depth, and diagnostic limits terminate with
  bounded output.
- `go test ./...`, `go test -race ./...`, and `go vet ./...` pass from
  `compiler/`.

## Handoff

The implementing chat owns `compiler/internal/symbol`, name diagnostic codes,
and `tests/names`. It consumes `compiler/internal/module` and must not alter
`04a` identity or import semantics without reporting a contract discrepancy.
Its final handoff reports public stores and result APIs, diagnostic codes,
source coverage, verification commands, commit, and any checker-facing
question that remains.
