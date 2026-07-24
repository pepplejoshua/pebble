# 06 Checking, Validation, and Typed IR

Phase 6 is the language-policy boundary between algebraic inference and typed
IR. It is implemented as two dependency-ordered tasks inside
`compiler/internal/check`:

- [06a Semantic Fact Generation](06a-semantic-fact-generation.md) owns the
  constant evaluator, `05b` preparation, the one semantic traversal, every
  inference fact/publication, frozen semantic records, and the single
  `Solve`.
- [06b Semantic Validation and Typed IR](06b-validation-and-typed-ir.md) owns
  post-solve validation, conversions, places, calls, generic requirements,
  structural control flow, entry validation, and closed typed-IR construction.

Implementation of 06b begins only after 06a has been reviewed and accepted.
Both tasks are internal parts of one checker invocation; their handoff is not
a public compiler phase or serialized API.

## Lifecycle

```text
Prepare Program
-> create Session
-> generate all 06a facts
-> Solve once
-> create immutable SemanticSnapshot and types.Snapshot
-> discard Session and AST-bearing generation inputs
-> construct solveHandoff
-> run06b using only frozen compilation metadata, SemanticSnapshot,
   Solution, and frozen 06a facts
-> build and verify closed typed IR
-> phase 9 input only on complete success
```

`run06a` initializes its bounded constant evaluator before `Prepare` so the
program's array-length callback is available; this is setup for the sequence,
not a second traversal or solve. Fact generation includes the one deterministic
surface traversal and freezing all publications and retained records.

The boundary is strict. Every fact that can affect type choice exists before
`Solve`. Every language-policy relationship that depends on final types is
retained by 06a and validated by 06b. Validation never repairs a missing fact,
reopens inference, or reconstructs meaning from syntax.

Failed preparation, generation, solving, validation, resource checks, or IR
verification publishes no typed IR, phase-9 input, lowering input, or backend
input. A failed result may retain immutable solved facts and independent
diagnostics for tooling and recovery.

## Shared invariants

Phase 6:

- consumes phase 03-05 inputs during preparation/generation, then gives 06b
  only copied immutable tree-free snapshots and frozen facts;
- uses `SyntaxRef`, `SymbolID`, and `TypeID` as their owning documents define;
- performs no lexical/textual lookup and never infers from spelling;
- never mutates or decorates the surface tree;
- never rebuilds scopes, reopens source files, or creates source declarations;
- never clones generic bodies, specializes generics, computes layout, or
  assigns backend names;
- preserves authored left-to-right evaluation and deterministic diagnostic/
  publication order;
- bounds visits, records, constants, validation, control flow, defer edges,
  IR storage, diagnostics, and dumps;
- has no package-global current module, scope, callable, context, region,
  formatter, or cache.

`05a` remains authoritative for semantic type identity. `05b` remains
authoritative for type syntax, terms, constraints, exact literals, solving,
and immutable solutions. Phase 7 owns specialization and concrete proof of
published generic requirements. Phase 9 owns downstream query/caching and
integration policy. Phase 10 owns layout, representation, and lowering of
already explicit semantic operations.

## Internal handoff

06a passes one immutable in-package `solveHandoff` containing:

- a tree-free compilation snapshot with copied module keys/source spans,
  authored imports and top-level declaration order, dependency order,
  root-module identity, and copied source metadata;
- one immutable tree-free `infer.SemanticSnapshot`, including its owned
  `types.Snapshot`, exact immutable resolution, and the exact matching
  immutable `infer.Solution`;
- frozen copied records, the sole control-region hierarchy, roots, and
  constant results;
- generation success state.

No `module.Graph`, `module.Module`, `syntax.Tree`, AST node, source bytes,
syntax-reconstruction lookup, `infer.Program`, `types.Store`, `infer.Session`,
`InferID`, term, mutable slice, layout, conversion decision/result, or typed-IR
node crosses this boundary. Runtime identities are read from the semantic
snapshot; the handoff has no duplicate runtime field.
The exact structure, copied compilation metadata, control hierarchy, root API/audit,
solved-slot/choice rules, and frozen record inventory are authoritative in
06a. The solved-root queries, handoff audit, validation order, and result
publication gate are authoritative in 06b.

If 06b needs information absent from this handoff, implementation stops and
reports a specification defect. It does not add a public fact API merely to
bridge two files in one package.

## Policy ownership

The detailed documents are the only normative locations for their rules:

| Concern | Authority |
| --- | --- |
| constant language and array-length callback | 06a |
| traversal and exhaustive surface dispatch | 06a |
| expected identity/literal/shape evidence | 06a |
| equations, capabilities, shapes, choices, and publications | 06a plus 05b |
| retained assignment/call/operator/place/member/control/context facts | 06a |
| control parent/depth/ordered-child hierarchy | sole frozen `controlRegion` arena in 06a; flow analysis in 06b |
| primitive/composite compatibility matrices | 06b |
| exact operator and conversion legality | 06b |
| place projection, mutability, assignment, and single evaluation | 06b |
| call/member/record/variant/bracket/index/slice legality | 06b |
| hidden Pebble Context propagation | generation facts in 06a; validation and IR in 06b |
| generic requirement normalization/publication | 06b; specialization proof in phase 7 |
| reachability, targets, switches, defers, and definite return | 06b |
| constant-global and configured entry validation | 06b |
| closed typed-IR inventory, verifier, spans, and dumps | 06b |
| specialization, layout, lowering, backend ABI, and caching | phases 7, 9, and 10 |

This overview intentionally does not repeat conversion cells, operator tables,
control equations, diagnostic suppression, typed-node inventories, resource
defaults, fixtures, or slice ownership.

## Runtime context

The compiler-owned runtime identities are prepared upstream. Generation uses
`Program.RuntimeTypes().Context` before solve; validation uses
`SemanticSnapshot.RuntimeTypes().Context` after handoff. Phase 6 performs no name lookup and creates
no authored parameter or synthetic declaration. Source-level `FunctionKey`
parameters remain authored-only. 06a retains context-use and call-flow facts;
06b permits `context` only in Pebble-convention callables and records hidden
context forwarding explicitly in typed IR. C calls carry no hidden context.

The runtime entry adapter is a later driver/lowering concern. It does not
change source function identity or permit phase 6 to search for an entry name.

## Diagnostics and recovery

06a owns `C0614` constant diagnostics, generation-side `C0619`, and the
`T0501`-`T0512` diagnostics produced by 05b. 06b owns `C0601`-`C0613`,
`C0615`-`C0620`, plus its validation/IR-side `C0619`. The exact code meanings,
suppression rules, limits, and fixtures are defined in the detailed owner.

Recovery never creates an error `TypeID`. `Session.Error` is silent recovery;
an empty binding uses it only to suppress inference cascades, produces no
`T0510`, and receives exactly one 06b-owned `C0602`. A failed inference root
otherwise suppresses only dependent policy diagnostics. Independent semantic
and control errors may still report, but no recovery path can publish backend
input.

## Implementation and verification

The required cross-document implementation chain is:

```text
05a type-snapshot extension
-> 05b.8 semantic-snapshot continuation
-> 06a.8 handoff
-> 06b.1 validation
```

Within phase 6, preserve each document's eight-slice order. Slices 06a.2
through 06a.7 may proceed before the snapshot chain completes wherever their
already stated 03b/04b/05b dependencies permit; only 06a.8 must wait for
05b.8, and 06b.1 must wait for 06a.8. A downstream slice must consume the
upstream snapshot API and must not implement a missing 05a/05b API locally.
A slice owns only its listed files and reports an upstream discrepancy rather
than editing another phase to make the implementation pass.

Every slice runs its targeted direct/source tests and the common compiler
suite with build caches outside the repository:

```sh
GOCACHE=/tmp/pebble-go-cache go test ./...
GOCACHE=/tmp/pebble-go-cache go test -race ./...
GOCACHE=/tmp/pebble-go-cache go vet ./...
```

Run `git diff --check` from the repository root. Source `.peb` fixtures are
normative for language behavior; compact goldens are reserved for stable fact
or typed-IR shape where direct assertions are less readable. Normal test runs
never update goldens.

## Resolved inputs and future decisions

There are no remaining unresolved language-contract blockers. The snapshot
implementation prerequisites above remain mandatory. 03b defines
`EndOfFile` and `RecordField`, and exhaustive 06a dispatch retains them. 04b
provides every immutable checker query. 05b provides ordinary/guarded solved
slots, conditional publication, `Callable`, `Indexable`, and `Sliceable`.
Generic anonymous functions have the explicit `C0608` rejection path; valid
nongeneric anonymous functions are noncapturing and globally hoisted. The 06b
handoff audit identifies one exact frozen 06a source for every validation and
typed-IR input.

Slice lifetime/escape/runtime representation and phase-10 release-mode runtime
fault behavior remain future and nonblocking. They do not permit closures,
generic anonymous functions, unsafe pointers, C adapters, public structural
traits, textual lookup, AST rereading/mutation, another solve, implicit numeric
conversion, or guessed semantics.
