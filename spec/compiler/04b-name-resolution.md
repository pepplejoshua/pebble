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
    Scopes  *ScopeStore
    Symbols *SymbolStore
    // private immutable reference, qualifier, bracket, capture, member,
    // builtin, runtime, and prelude tables
}
```

Maps above are conceptual. Public accessors return stable IDs and values in
deterministic order. The result does not store generated C names or semantic
`TypeID`s. The stores and result are owned by one `Result`; no package-global
current scope or current module participates in resolution.

Zero-valued limits select documented package defaults. Tests may lower every
limit. At minimum the implementation bounds symbols, scopes, scope depth, and
name-resolution diagnostics.

### Immutable result queries

The checker-facing API is the concrete `compiler/internal/symbol` contract:

```go
type ResolutionState uint8
const (
    ResolutionResolved ResolutionState = iota + 1
    ResolutionError
    ResolutionDeferred
)

type Resolution struct {
    Syntax SyntaxRef
    Symbol SymbolID
    State  ResolutionState
}

type BracketMode uint8
const (
    BracketDeferred BracketMode = iota + 1
    BracketTypeNames
    BracketValueNames
)

type Capture struct {
    Function SyntaxRef
    Symbol   SymbolID
}

func (r *Result) Prelude() ScopeID
func (r *Result) Builtin(BuiltinType) (SymbolID, bool)
func (r *Result) Runtime(RuntimeType) (SymbolID, bool)
func (r *Result) Reference(SyntaxRef) (Resolution, bool)
func (r *Result) References() []Resolution
func (r *Result) Qualifier(SyntaxRef) (ModuleID, bool)
func (r *Result) Bracket(SyntaxRef) (BracketMode, bool)
func (r *Result) Captures(function SyntaxRef) []SymbolID
func (r *Result) CaptureList() []Capture
func (r *Result) Members(owner SymbolID) []SymbolID
```

`BracketDeferred` means resolution could not decide type-name versus
value-name interpretation without expression typing. `BracketTypeNames` and
`BracketValueNames` record the exact traversal already performed for the
arguments; later phases must not reinterpret the other category except through
phase 6's bounded alternative rule.

All returned identities are local to the owning resolution snapshot.
`Prelude`, `Builtin`, and `Runtime` are nil-safe as implemented; the remaining
queries require the non-nil `Result` returned by `Resolve`. An invalid
discriminator or absent point-query key returns the zero value and `false`;
an absent capture/member key returns an empty/nil copied slice. Absence never
requests textual lookup. `Reference` returns its stored immutable value.
`References` returns a fresh slice sorted by
`(Syntax.Module, Syntax.Node)`. `Captures` returns a fresh slice in first-
reference order for that anonymous function. `CaptureList` returns a fresh
slice ordered by anonymous-function discovery and then first-reference order.
`Members` returns a fresh slice in the owner's declaration/member order.
Store accessors and every slice-bearing result copy their slice storage;
callers own no resolver backing array. `Qualifier`, `Bracket`, `Builtin`, and
`Runtime` are immutable point queries. No query performs lookup, allocates a
new semantic identity, or accepts an ID from another snapshot as equivalent
merely because its integer value matches.

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

Predeclared builtin and runtime types are the only symbols without authored
declarations. Primitive builtins record `SymbolBuiltinType` and an exact
`BuiltinType` discriminator. Compiler-owned runtime types record
`SymbolRuntimeType` and an exact `RuntimeType` discriminator. Both use stable
compiler-owned `SymbolID`s and a zero `SyntaxRef`; consumers never infer either
identity from spelling.

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

- One synthetic prelude scope is created first. It has no parent, module,
  owner, or authored origin.
- Every module has one module scope.
- Every module scope has the prelude as its parent.
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
- Anonymous functions own a function scope. Resolution may follow the defining
  lexical chain only to identify prohibited references to enclosing
  function-local storage; it does not define a runtime environment.

Scope creation order follows module and source order and is deterministic.

## Reserved primitive and runtime types

The prelude contains exactly these `SymbolBuiltinType` symbols in this fixed
order:

```text
bool char str void int uint i8 i16 i32 i64 u8 u16 u32 u64 f32 f64
```

They receive the first `SymbolID`s in a compilation snapshot. `Result.Prelude`
returns the prelude `ScopeID`; `Result.Builtin(BuiltinType)` returns the exact
symbol identity. Every authored type-position occurrence resolves through
ordinary scope lookup to one of these symbols. The parser's dedicated `void`
result syntax denotes `BuiltinVoid` without requiring an authored name
reference.

The prelude and its symbols count toward `MaxScopes` and `MaxSymbols`. A limit
too small to construct the complete prelude produces bounded `N0006` recovery;
the resolver never silently omits a builtin in an otherwise successful result.

Immediately after the primitive builtins, `04b` installs two compiler-owned
runtime identities in this order:

```go
type RuntimeType uint8
const (
    RuntimeAllocator RuntimeType = iota + 1
    RuntimeContext
)
```

`RuntimeAllocator` has source spelling `Allocator`, is bound in the prelude,
and participates in ordinary type-name resolution. `RuntimeContext` has no
source spelling and is not installed in any lexical binding table; phase 6
obtains it only through the runtime API when generating `ContextExpr`.
Compiler-owned field symbols follow in fixed declaration/member order and are
reachable only through `Result.Members(owner)`: `Allocator.ptr`,
`Allocator.alloc`, `Allocator.realloc`, `Allocator.free`, then
`Context.default_allocator`. They have zero `SyntaxRef`s and do not create a
synthetic syntax tree, generated source declaration, scope, or qualified-name
string.

Primitive builtin names and `Allocator` are reserved throughout the language.
No import qualifier, module declaration, local binding, parameter, loop binding, type parameter,
field, variant, or method may declare one of these names. This prohibition also
applies in member namespaces: a builtin name cannot be hidden, shadowed, or
repurposed anywhere. A rejected declaration receives an error symbol and
`N0007`; it is not installed into its lexical or member namespace. Subsequent
lookup therefore continues to select the builtin where that category is
legal.

The internal Context identity is compiler-reserved rather than text-reserved:
it has no spelling that source lookup can select. In particular, neither
`Context` nor `__pebble_context` is a magic type name.

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
  module scope, except that builtin names are reserved and cannot be shadowed.
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
-> prelude scope
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
Classification is closed over the authored base shape and resolved identity:

- a directly resolved generic type or generic callable, and a valid
  type-context application, uses `BracketTypeNames`;
- a directly resolved runtime value or nongeneric callable uses
  `BracketValueNames`, as does every base shape that syntax proves is an
  expression value: literals, aggregate/value expressions, calls, operators,
  casts, function values, and completed bracket applications;
- `BracketDeferred` is reserved for bases whose category genuinely requires
  expression typing, initially type-directed member and method forms, plus
  deterministic damaged-input recovery where no successful identity exists.

Transparent grouping preserves the enclosed base classification. The selected
mode fixes argument traversal exactly: type names for `BracketTypeNames`, value
expressions for `BracketValueNames`, and neutral traversal for
`BracketDeferred`. Resolution does not use inferred types, following-call
shape, traversal order, capitalization, or spelling to change that mode.

The checker later validates arity, indexability, concrete types, and generic
requirements.

## Anonymous functions and captures

A nongeneric anonymous function has a noncapturing, globally hoisted semantic
model. It may reference module globals, imported module members, and other
module-level declarations. It may not capture parameters, locals, loop
bindings, or any other storage owned by an enclosing function.

When lexical resolution finds such a prohibited reference, `04b` records the
enclosing anonymous-function `SyntaxRef` and captured `SymbolID` in
deterministic first-reference order. These capture records are evidence for
phase 6 diagnostic `C0617`; they do not authorize capture lifting or an
environment. The implicit Pebble `Context` is calling-convention state and is
not a lexical reference or capture. Module/global references are not capture
records.

Valid anonymous functions are later represented as hoisted global function
identities. Closure objects, environment allocation, capture lifting, heap
allocation, and closure lowering are outside the current language. Whether
closures are ever added is an explicit future language decision, not an
assumption of this resolver contract.

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
| `N0007` | declaration attempts to use a reserved primitive or runtime type name |

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
The corpus also covers every builtin and runtime identity, deterministic
prelude ordering, module-to-prelude parentage, successful `Allocator` lookup,
ordinary lookup failure for the internal Context identity, and rejected
primitive/`Allocator` redeclarations in lexical and member namespaces.

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
- define closure semantics or runtime environments for anonymous functions.

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
- Primitive and source-spellable runtime references resolve to fixed prelude
  symbols, and no authored declaration can reuse a reserved name. The internal
  Context symbol remains unavailable to ordinary source lookup.
- Anonymous-function references to enclosing function-local storage are
  recorded as prohibited-capture evidence in stable first-reference order;
  module/global references are not captures.
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
