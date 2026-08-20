# 22 — incremental resolve/check: the real refactor

**Status:** planning. No implementation started. This document exists
because `21-daemon-incremental-lsp.md`'s slice 21.2b, on its own required
design investigation, found that a correct module-scoped recheck is **not**
achievable as a thin wrapper around `check.Check` — it needs real changes
to how the checker and inference engine own state. Per an explicit
decision (2026-08-19), that work is scoped here as its own initiative
rather than being squeezed into `21`'s slice sequence. `21`'s daemon work
(21.1a/21.1b/21.2a, all landed) stands on its own and is not blocked by
this document — the daemon already gets a real, measured win from process
reuse alone (see `21`'s "Completed slices"); this document is about the
next tier, skipping *unchanged* work on repeated builds.

**This is genuinely complex, not a bolt-on.** The same category of problem
`rustc`'s incremental compilation solves — narrower in scope here (one
implementation, no cross-crate ecosystem compatibility to preserve), but
the core difficulty is the same kind: today's checker has no concept of
"this module's result is a stable, reusable fact," and introducing one
without silently producing wrong answers is the hard part, not the
plumbing.

## Provenance

Everything below is drawn from the 21.2b design investigation (`orc`
session `ses_6a864385d74fb0faae4048c7`, `openai/gpt-5.6-luna`, resumed
once after an initial partial run), independently spot-verified before
being trusted — see "Verification" below for what was directly confirmed
by re-reading the source, not taken on the investigation's word alone.

## The verdict, and why

A correct, minimal module-scoped recheck is not achievable as a thin
wrapper around `check.Check`, for reasons rooted in real architectural
choices, not missing glue code:

1. **No warm state exists to reuse in the first place.** `daemon.
   serveBuild` calls `compileOnce` for every request
   (`compiler/cmd/pebc/daemon.go:502-535`), and `compileOnce` constructs a
   completely fresh `FileSet`, `module.Graph`, `symbol.Result`,
   `types.Store`, `infer.Program`, and `infer.Session` on every single
   call (`compiler/cmd/pebc/compiler.go:73-127`). Nothing survives between
   two `daemon build` requests today except the file-watcher's tracked
   hashes. A module-scoped recheck API is necessary but not sufficient —
   the daemon itself must also hold onto compiler state across requests
   for there to be anything to reuse.
2. **`infer.Session`'s inference cells are permanently owned by one
   session and refuse further mutation once solved.** Confirmed directly:
   `Session.mutable()` reports `CodeResourceLimit` and hard-stops once
   `s.solved` is true (`compiler/internal/infer/session.go:190-203`).
   Terms are session-token-owned (`session.go:172-177`); a term from one
   session cannot be handed to another. There is no "reopen and add a
   little more" path — a session is a single, sealed unit of work by
   design.
3. **Symbol/type/inference IDs have no per-module boundary.** `SymbolID`
   and `ScopeID` are allocated sequentially across the ENTIRE resolution
   result (`compiler/internal/symbol/resolve.go:436-464`, `517-550`);
   `TypeID` is `len(store.entries)+1` (established in `21`'s own
   provenance section, `internal/types/store.go:120`). A partial
   resolution that only processes some modules would allocate different
   IDs than a full one — even within a single process, since these are
   simple counters over whatever gets processed, not stable per-module
   namespaces.
4. **There's no existing concept of "treat this module's checked result as
   a fixed external fact."** Tracing a real cross-module case: module A's
   reference to something in module B resolves through B's global
   `SymbolID` during collection (`compiler/internal/symbol/resolve.go:
   67-113`, `visit.go:725-776`), then through B's prepared templates
   during inference (`internal/infer/type_resolver.go:11-28,157-214`,
   `declaration.go:138-215`). Cross-module generic instantiation confirms
   the same pattern for generics specifically
   (`internal/check/cross_module_generic_test.go:21-37,274-298`). Every
   one of these paths assumes B was just resolved/checked in the same
   pass — there's no "B's interface, frozen, fed in as input" boundary
   anywhere in the current design.
5. **`21.2a`'s `TransitiveDependents` (already landed, real and useful)
   only tracks explicit import edges — it does not know about the
   prelude**, which is implicitly visible to every module
   (`internal/symbol/resolve.go:77-104`). A prelude change must
   conservatively invalidate everything; import-based dependency tracking
   alone would silently miss this.

None of these are bugs — they're reasonable choices for a batch,
whole-program compiler. They just don't leave room for a safe module-scoped
recheck without deliberately adding the missing seams.

## What correctness actually requires

The hard invariant, non-negotiable: after a scoped recheck, the result
(diagnostics, TIR, resolved types) for every rechecked module must be
**identical** to what a full rebuild would have produced for the same
source. A silently wrong incremental result is worse than no incremental
compilation — it produces code that looks right and isn't. Concrete ways a
naive implementation could get this wrong, all identified in the
investigation and all needing an explicit answer in the design below, not
just awareness:

- Stale symbol references after an ID shift.
- Stale dependency signatures/type templates (B changed in a way that's
  compatible with its OLD callers but not obviously so).
- Missing constraints from a generic declaration or one of its consumers.
- A different global generic-choice/specialization outcome than a full
  build would have picked.
- Different literal-defaulting behavior depending on what's in scope
  during a partial vs. full solve.
- Stale specialization/TIR reuse.
- A missed prelude dependency (see point 5 above).

## Proposed shape (starting point, not final)

From the investigation, a real API needs an explicit, persistent,
versioned checked-state object — not a bare wrapper function:

```go
type CheckedState struct {
    Graph      *module.Graph
    Sources    *source.FileSet
    Resolution *symbol.Result
    Types      *types.Store
    Program    *infer.Program
    Modules    map[module.ModuleID]ModuleState
}

func CheckIncremental(
    state *CheckedState,
    recheck []module.ModuleID,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) (*CheckedState, *Result)
```

`ModuleState` needs to capture a module's STABLE, reusable surface: its
exported symbols, signatures, type templates, a compatibility fingerprint
(so a downstream module can tell "did B's externally-visible interface
actually change, or just its body"), retained facts, and diagnostics. The
investigation confirmed the existing `infer.SemanticSnapshot` is
output-only and cannot be fed back in as input to a fresh
`Prepare`/`NewSession` call (`internal/infer/semantic_snapshot.go:40-89`)
— this needs new machinery, not reuse of something that already exists.

This is a starting sketch, not a spec — the actual design pass (22.0
below) may reshape it.

## Phased plan (rough — sharpen during 22.0)

This needs its own concreteness/design pass before slicing, mirroring how
`21` itself got a phase-concreteness investigation before its slices were
written. Rough shape, subject to revision:

- **22.0 — design pass.** Work out the real shape of `ModuleState` (what
  exactly counts as a module's "stable interface" vs. its "body," and how
  a fingerprint over the former is computed), the daemon-side warm-state
  lifecycle (what `daemon.go`/`compiler.go` need to hold across requests,
  and how a detected file change maps to "rebuild `CheckedState` for
  these modules, keep the rest"), and the prelude-invalidation rule
  (treat any prelude change as "invalidate everything," at least
  initially — don't try to be clever about partial prelude invalidation
  in v1). Should produce the same kind of concrete, evidence-backed
  design doc `21`'s own provenance investigations did, before any code.
- **22.1 — stable module interface + fingerprinting.** Introduce
  `ModuleState`/the interface-vs-body distinction for real, with a
  fingerprint that's stable across two consecutive full builds of
  unchanged source (a strong, checkable invariant: build twice, same
  fingerprint, before touching anything else).
- **22.2 — daemon-held `CheckedState`.** The daemon keeps one
  `CheckedState` alive across requests instead of rebuilding from scratch
  every time; a request with no detected changes is a true no-op reusing
  the existing state entirely.
- **22.3 — scoped `CheckIncremental`.** The actual incremental entry
  point, built on 22.1/22.2, wired to `Graph.TransitiveDependents` (21.2a)
  plus conservative prelude invalidation.
- **22.4 — correctness harness.** Before this is trusted for real use: a
  test harness that, for a corpus of real multi-module programs, compares
  incremental-recheck output against full-rebuild output across many
  randomized edit sequences, and fails loudly on any divergence. This is
  not optional polish — given the "silently wrong is worse than no
  incrementality" invariant above, this harness is what actually earns
  trust in the feature before it ships as the default path.

Once 22 lands, `21`'s 21.2c ("daemon wiring for scoped rechecking") and
the deferred half of 21.2b become straightforward — the hard part will
already be done.

## Verification discipline for this doc

Every claim in "The verdict, and why" was checked directly against the
source before being written here, not taken on the investigating
session's word alone — the `Session.mutable()`/`s.solved` check
specifically was re-read directly (`internal/infer/session.go:190-203`)
and confirmed to match exactly what the investigation reported, and is
independently consistent with this session's own earlier direct work on
`infer.Session`'s taint-propagation mechanism (`Term{owner: s.token, kind:
termError}` — the same session-token-ownership model, confirmed firsthand
while fixing unrelated diagnostic-cascade bugs the same day).

## Standing constraint

Every commit toward this initiative, same as `21`'s, must leave the
existing one-shot `pebc <entry.peb>` CLI path fully working — build clean,
tests clean, verified before commit, every time. This compiler is in
active daily use for stdlib work; nothing here may leave it broken.
