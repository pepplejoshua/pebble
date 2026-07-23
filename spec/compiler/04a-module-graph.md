# 04a Module Graph

`04a Module Graph` is the source-loading and dependency-graph task. Starting
from one entry file and explicit resolver configuration, it discovers every
reachable module exactly once and returns an immutable, validated graph.

## Inputs

The driver supplies all resolution state. Module code does not inspect the
compiler executable path, process-global options, or an ambient current
module.

```go
type BuildConfig struct {
    EntryPath     string
    Package       PackageID
    StandardRoot  string
    SearchRoots   []SearchRoot
    MaxModules    uint32
    MaxImportDepth uint32
}
```

The implementation uses a narrow source-provider abstraction for path
canonicalization and reads. Production uses the host filesystem; tests use an
in-memory provider. The provider must distinguish not-found, unreadable, and
invalid-path failures.

## Module identity

```go
type ModuleID uint32

type ModuleKey struct {
    Package PackageID
    Path    CanonicalPath
}
```

`ModuleID(0)` is invalid. IDs are stable only for one compilation snapshot.
Equal `ModuleKey` values identify one module even when multiple imports reach
it. A basename is display information and a possible local qualifier; it is
never unique identity.

Canonical paths use normalized separators and remove `.` components. Relative
`..` components are resolved before identity is assigned. Symlink policy is
owned by the source provider and must produce one canonical key for one host
file. Diagnostics display stable project- or package-relative paths rather
than embedding a user's absolute checkout path.

## Import spellings

The first contract accepts:

```text
./math             relative to the importing module
../shared/math     relative to the importing module
std:mem            relative to the configured standard-library root
std:mem/arena      nested standard-library module
collections/map    searched in configured package roots
```

Rules:

- Absolute imports are rejected.
- Backslashes are rejected in authored import strings.
- Empty components and trailing separators are rejected.
- Authored `.peb` extensions are rejected; the resolver appends the one
  canonical source extension.
- `std:` imports consult only `StandardRoot`.
- Explicit relative imports consult only the importing module's directory.
- Bare package imports consult `SearchRoots` in configured order.
- The first matching search root wins only when exactly one configured package
  owns that spelling. Ambiguous package ownership is an error.

The local qualifier is the final path component: `std:mem/arena` binds
`arena`. The grammar has no import-alias syntax in this contract.

## Graph representation

```go
type ImportEdge struct {
    Span      source.Span
    Spelling  string
    Qualifier string
    Target    ModuleID
}

type Module struct {
    ID      ModuleID
    Key     ModuleKey
    Source  source.ID
    Tree    *syntax.Tree
    Imports []ImportEdge
}

type Graph struct {
    Root    ModuleID
    Modules // private deterministic storage
}
```

Accessors return values or copies and never expose mutable backing storage.
Every edge retains the importing source span. The graph owns module
relationships; syntax trees continue to own authored import nodes.

## Loading algorithm

1. Resolve and load the entry module.
2. Parse it through `syntax.Parse`.
3. Visit import declarations in source order.
4. Validate and resolve each import spelling.
5. Reuse an existing `ModuleID` for an existing `ModuleKey`; otherwise load
   and parse the target.
6. Record the edge and continue until every reachable module is known.
7. Validate duplicate bindings, limits, and cycles.
8. Freeze and return the graph.

Discovery order and IDs are deterministic for identical inputs. Hash-map
iteration never determines IDs, diagnostics, or traversal order.

## Duplicate and collision rules

- Importing the same `ModuleKey` twice from one module is an error, even if the
  authored spellings differ.
- Two imports that derive the same qualifier but resolve to different modules
  are an error.
- A later `04b` module declaration that collides with an import qualifier is a
  same-scope duplicate.
- A module reached through multiple different importing modules is loaded and
  parsed once; that is ordinary graph sharing, not a duplicate import.

## Cycles and ordering

Every directed module cycle is rejected in the first contract. Cycle
diagnostics show the ordered authored edge chain and label every import span.
Traversal uses explicit visit states and never recursively propagates scores.

For an acyclic graph, dependency order comes from the graph. Ties are broken
by deterministic module discovery order. Import popularity and hash-table
iteration have no role.

## Diagnostics

Initial stable codes:

| Code | Meaning |
| --- | --- |
| `M0001` | invalid import spelling |
| `M0002` | module not found or unreadable |
| `M0003` | duplicate import of one module |
| `M0004` | imported qualifier collision |
| `M0005` | module cycle |
| `M0006` | ambiguous package-root resolution |
| `M0007` | module-count or import-depth limit |

Diagnostics are emitted in deterministic discovery and source order. A module
that cannot be loaded receives no invented syntax tree. Other independent
imports may still be inspected until the configured diagnostic limit.

## Resource limits

Defaults are explicit constants owned by a resolver instance, not package
globals. Tests can lower them. At minimum, bound reachable modules, import
depth, and module diagnostics. No graph input may hang, recurse without bound,
or repeatedly parse one canonical module.

## Source-driven tests

Fixtures are directories because one case may contain multiple files:

```text
tests/module/
  valid/<case>/main.peb
  invalid/<CODE>/<case>/main.peb
  recovery/<case>/main.peb
```

Fixture configuration declares standard and package roots without relying on
the repository working directory. Optional `.graph.golden` files are used only
when exact edge order or a cycle chain is the behavior under test.

Required cases include relative, parent-relative, standard, package-root,
shared dependency, missing file, unreadable provider result, invalid spelling,
duplicate target, qualifier collision, self-cycle, multi-module cycle,
diamond graph, depth limit, module-count limit, and deterministic reruns.

## Non-goals

`04a` does not:

- collect declarations or build lexical scopes;
- resolve `module::member`;
- determine visibility or types;
- classify neutral bracket application;
- combine module syntax trees;
- assign backend names;
- implement persistent caching or parallel loading.

The data model must permit later parallelism and caching, but this task proves
deterministic single-process behavior first.

## Completion criteria

- Every accepted import spelling follows the documented resolution route.
- Every reachable canonical module is parsed exactly once.
- The graph retains authored edge spans and deterministic order.
- Duplicate targets and qualifier collisions are diagnosed.
- Every cycle terminates and reports its edge chain.
- Invalid providers and configured limits return bounded diagnostics.
- Surface trees remain unchanged.
- Source fixtures cover every rule above.
- `go test ./...`, `go test -race ./...`, and `go vet ./...` pass from
  `compiler/`.

## Handoff

The implementing chat owns `compiler/internal/module`, module diagnostic codes,
and `tests/module`. It must not add symbol tables or edit `04b` semantics. Its
final handoff reports the public graph API, diagnostic codes, fixture coverage,
verification commands, commit, and any contract discrepancy discovered.
