# 21 — persistent daemon, incremental compilation, and an LSP core

**Status:** in progress. 21.1a is implemented, committed, and pushed
(`ca653de`). This document is being updated in place as each slice lands,
the same way `07`/`19` are — treat "Completed slices" below as authoritative
fact and "Slice record and remaining work" as current plan, sharpened as
each piece is actually built.

## Completed slices (implemented, verified, committed)

- **21.1a — daemon process lifecycle skeleton** (`ca653de`):
  `pebc daemon {start,build,ping,stop}`. Single instance per project root
  (Unix socket at `<root>/.pebble/daemon.sock`, probe-then-attach), idle-
  timeout self-shutdown, stale-binary self-restart via re-exec with the
  listener fd handed to the child. No incrementality yet — every build
  request still runs the full pipeline via a new `compileOnce` (extracted
  from the pre-existing one-shot path, used unchanged by both). Real,
  measured win at this slice: ~5-6x faster per request purely from process
  reuse (~0.39s cold one-shot vs. ~0.06-0.07s daemon-served). `fsnotify`
  added as the first third-party dependency (unwired — real watching is
  21.1b). Three real bugs found and fixed during independent verification,
  all in the re-exec/socket-ownership path (a `net.UnixListener.Close()`
  unlink-on-close default, a throwaway listener bind-then-close race, and
  a `handedOff`-vs-`inherited` ownership conflation that deleted the
  socket out from under a freshly spawned child) — see the commit message
  for the full account; every one of the three was caught by an actual
  multi-process reproduction, not by code review, and the first two fixes
  that looked complete on inspection alone were each proven insufficient
  by rerunning the real repro.

**Motivation.** `pebc` today is a one-shot batch pipeline: every invocation
parses, resolves, type-checks, and emits C for the entire program from a
cold start — including any imported stdlib modules unchanged since the last
invocation — then discards all of that state on exit. Two goals motivate
fixing this: (1) "the same files that have barely changed" should compile
faster on repeated builds, not just once; (2) a genuinely fast edit-rebuild
loop (honestly scoped as fast-rebuild-and-restart, not in-process hot code
swapping — see "What 'hot reload' means here" below) and a real LSP core for
editor integration, both riding on the same underlying warm-state machinery.

## Provenance

Two investigation passes (both `openai/gpt-5.6-luna` via `orc`, both
independently spot-verified against the actual source before being trusted
— see "Verification discipline used for this doc" below) preceded this
plan:

1. **Feasibility investigation** (session `ses_6a8602560b341a44147ab731`,
   "Incremental Compilation Feasibility for pebc"): established that
   `TypeID`/`SymbolID`/`InferID`/TIR `NodeID` are all bare allocation-order
   indices (`types.Store.Intern`: `id := TypeID(len(s.entries) + 1)`,
   confirmed directly at `internal/types/store.go:120`), not portable
   semantic identities. This rules out naively serializing checked state to
   disk and reloading it in a fresh process — a cached type ID from run A
   means nothing to run B's fresh store. A persistent daemon (state never
   crosses a process boundary) sidesteps this problem entirely rather than
   solving it, which is why it's the recommended foundation rather than an
   on-disk semantic cache (the `rustc` incremental-compilation route: real
   prior art, but took years to get right even with content-fingerprinting
   specifically built to solve this exact problem).

2. **Phase-concreteness verification** (session `ses_6a8609839f81961ea4355d28`,
   "Verifying Daemon, Incremental, and LSP Roadmap Phases"): checked each
   phase's key assumption against the actual code. Findings below are
   drawn from this pass, each independently spot-verified.

## What "hot reload" means here — scoped explicitly

Web-dev hot reload (Vite/webpack HMR) patches running JS in the same
process, no restart. That's a different, harder problem for a compiled
native language — it needs per-module dynamic libraries and
`dlopen`/`dlsym` swapping, or a VM with native live code-swapping (Erlang/
BEAM). **Out of scope for this plan.** What's in scope, and what every fast
native dev loop actually does (Go's `air`, Rust's `cargo-watch`): rebuild
and restart the process fast enough it feels instant. With a warm daemon
and incremental checking, "instant" means low hundreds of ms for a typical
edit, not literal in-process patching.

## The Phase 3 fork — resolved

The original sketch included a "Phase 3": split `backend.Emit` to emit one
`.c` translation unit per Pebble module instead of one file for the whole
program, enabling `ccache`-style per-module object caching (mirroring
`compiler/cmd/pebc/runtime_cache.go`'s existing content-hash pattern, just
applied one level up). The concreteness pass found this **not concrete**:

- `Emit` computes **whole-program reachability** from a single entry symbol
  for dead-code elimination (`internal/backend/emit.go`, the `Emit`
  function) — not naturally scoped to one module.
- `Emit` performs a **globally-ordered typedef emission pass**: function
  typedefs before enum typedefs before aggregate typedefs before slice
  typedefs, driven by real cross-type reference dependencies (confirmed by
  reading `Emit`'s body directly — the ordering comments describe exactly
  this dependency chain). This is a whole-program topological sort, not a
  per-module concern.
- TIR `Node.Syntax` (which would carry module identity via
  `symbol.SyntaxRef.Module`) is **not universally populated** — several
  synthetic/structural node kinds (`ImplicitReturn`, `Block`, `Initialize`,
  and others) pass `symbol.SyntaxRef{}` explicitly at their `addNode` call
  sites in `internal/check/ir_builder_control.go`. "Group nodes by
  originating module via `Node.Syntax`" breaks for these node kinds as-is.

**Decision (owner call, 2026-08-19): Phase 3 is deferred entirely from this
plan's scope.** Real wall-clock measurement (see the perf-hunt work
preceding this doc) showed `-check` alone dominates a full build's cost —
check-only ~470ms vs. full build ~550–600ms for a representative
multi-module program. Eliminating redundant rechecking of *unchanged*
modules (Phases 1–2 below) captures the large majority of the realistic
win. This plan always does one whole-program `Emit` → `cc` → link when an
actual rebuild is requested, using the daemon's warm *checked* state as
input, but never attempts per-module object caching. Revisit only if
profiling after this plan's v1 lands shows `Emit`+`cc`+link has become the
new dominant cost relative to checking.

## New dependencies (owner call, 2026-08-19)

The compiler module has zero third-party dependencies today (no `go.sum`
exists). This plan adds real, well-established libraries rather than
hand-rolling a file watcher or an LSP JSON-RPC transport:

- A file-watching library (`fsnotify` or equivalent) for slice 21.1.
- A Go LSP protocol library (e.g. `go.lsp.dev/protocol` or equivalent —
  pick during 21.4a, evaluate what's actively maintained at that time) for
  slice 21.4.

## Scope: what's in this plan vs. deferred

| | In scope | Deferred |
|---|---|---|
| Daemon core, warm process, file watching | ✅ 21.1 | |
| Incremental re-check (skip unchanged modules) | ✅ 21.2 | |
| `pebc dev`: watch + fast rebuild-restart | ✅ 21.3 | |
| LSP core: diagnostics, hover | ✅ 21.4 | |
| Per-module `.c` emission + object-file caching | | Phase 3, deferred (see above) |
| In-process hot code swap (`dlopen`-style) | | Explicit stretch, not this plan |
| Full LSP surface (completion, goto-def, references, rename) | | Beyond 21.4's "solid, extensible core" bar — future slices once the core is proven, same approach as `syght` shipping grammar-only first |

## Slice record and remaining work

None of the slices below are started. Each slice must land with its own
before/after evidence (timing numbers where relevant, a real repro/test)
and independent verification before being marked complete, per this
project's standing discipline (see "Verification discipline used for this
doc" below) — the same bar every slice in `07`/`19`/`20` was held to.

### 21.1 — Daemon core

- **21.1a — process lifecycle skeleton.** Done — see "Completed slices"
  above.
- **21.1b — file watching + content-hash change detection.** Add the file
  watcher dependency. On a filesystem event, hash the changed file's
  content and compare against the daemon's last-known hash for that path;
  skip re-parsing (not yet re-checking — that's 21.2) if unchanged. This is
  the primitive 21.2's invalidation will build on.
  **Test:** touch file A with identical content (same hash) — confirm no
  reparse occurs (via daemon-side instrumentation/logging count); touch
  file A with real new content — confirm it does reparse; confirm touching
  file B never triggers a reparse of unrelated file A.

### 21.2 — Real incremental invalidation

- **21.2a — reverse-dependency index.** `module.Graph.Imports
  []ImportEdge` (confirmed present at `internal/module/module.go:172`)
  makes a reverse "who imports module X" index a direct inversion. Build
  it as a new type/function with unit tests against a small synthetic
  multi-module graph (A imports B imports C — changing C must report
  exactly `[B, A]` as affected, in dependency order).
- **21.2b — module-scoped resolve/check entry points (highest-risk
  slice).** `symbol.Resolve` and `check.Check` are whole-graph-only today
  — `check.Check` builds one global `infer.Program`/`infer.Session` and
  walks the full dependency order (confirmed by reading
  `internal/check/check.go` and `internal/infer/program.go`/`session.go`).
  The concreteness pass found no *algorithmic* reason a subset walk can't
  work (the global session/program shape isn't inherently whole-graph-only
  by necessity, just by current API surface) — but this needs its own
  focused design-and-verify pass before implementation starts, not a blind
  attempt. **Do not skip straight to implementing this slice** — dispatch
  a scoped design investigation first: given a module's own content hash
  is unchanged AND every module it transitively imports is either
  unchanged or itself already-rechecked-and-still-type-compatible, what
  exactly is safe to reuse from the previous `infer.Session`/`Program`
  state, and what's the minimal new API surface (`check.CheckIncremental`?
  a `Program` method that accepts a subset with an
  already-resolved-context?) needed to express that. Bring back concrete
  findings before writing the real implementation.
  **Test (once implemented):** real multi-module program, edit one leaf
  module, confirm (via instrumentation) only it and its direct importers
  are rechecked — not the whole graph — and confirm output diagnostics are
  identical to a full rebuild's for the same edit (correctness first,
  speed second).
- **21.2c — daemon wiring.** Connect 21.2a+21.2b into the daemon's request
  loop: a file change triggers narrowly-scoped rechecking instead of full
  re-check.
  **Test:** real before/after wall-clock on a representative multi-module
  program (reuse the 10-stdlib-module synthetic program from the earlier
  perf-hunt work) — edit one leaf module, measure daemon-warm incremental
  rebuild time vs. cold one-shot `pebc -check` time. This is the number
  that actually validates the whole initiative's premise.

### 21.3 — `pebc dev`: the user-facing fast-rebuild-restart front end

- **21.3a — process supervision.** Launch the target built executable as a
  child process; on a successful incremental rebuild, kill and relaunch
  it, forwarding stdio.
- **21.3b — CLI wiring and clean shutdown.** `pebc dev main.peb` command;
  Ctrl-C cleanly tears down both the daemon and the child process (or
  leaves the daemon running for the next `pebc dev` invocation, per
  whatever 21.1a's lifecycle design settled on — decide during this
  slice, not before, once 21.1a's actual shutdown semantics are known).
  **Test:** real end-to-end — start `pebc dev`, edit a file, confirm the
  child process restarts and the edit is reflected in its output; measure
  and report real edit-to-running-again latency, not a synthetic number.

### 21.4 — LSP core

- **21.4a — transport.** Add the chosen LSP protocol library and `go.sum`.
  Minimal JSON-RPC-over-stdio wiring responding to `initialize`/`shutdown`
  only — prove an editor can connect without crashing before adding real
  features.
- **21.4b — diagnostics on save.** Wire `textDocument/didSave` (or
  `didChange` with debounce) to trigger an incremental recheck (reusing
  21.2's machinery directly — this is why 21.4 depends on 21.2, not on the
  deferred Phase 3) and publish `textDocument/publishDiagnostics` from the
  real `DiagnosticSet`, converted to LSP diagnostic ranges.
- **21.4c — hover.** Type-at-position lookup against the daemon's warm
  checked state — a read-only query, no new invalidation machinery needed
  beyond what 21.2 already built.
  **Test:** connect a real LSP client (a minimal test harness or an actual
  editor) and confirm diagnostics appear and clear correctly as the file is
  edited; confirm hover reports real inferred types at real cursor
  positions, not placeholder text.

## Verification discipline used for this doc

Every claim above that could be checked directly against the source was
checked directly, not taken on the investigating session's word alone —
matching this project's standing "never trust self-report" discipline. Spot
checks performed while reviewing the concreteness pass's findings, before
writing this document:

- `types.Store.Intern`'s allocation-order `TypeID` assignment — read
  directly at `internal/types/store.go:110-124`.
- `infer.Program`'s single-owner store mutex (`storeMu`) — read directly at
  `internal/infer/program.go:146-186`.
- `symbol.SyntaxRef{}` passed as a literal zero value at several `addNode`
  call sites in `internal/check/ir_builder_control.go` (`ImplicitReturn`,
  `Block`, `Initialize` node kinds) — grepped and read directly.
- `module.Graph`'s `Imports []ImportEdge` field — grepped and read
  directly at `internal/module/module.go:172`.
- Absence of `go.sum` in the compiler module — checked directly (`ls
  compiler/go.sum` → not found).
- `Emit`'s whole-program reachability computation and globally-ordered
  typedef emission — read directly in `internal/backend/emit.go`'s `Emit`
  function body (the ordering comments describe the real cross-type
  dependency chain driving typedef order).

## How to continue this work

Dispatch each slice via `orc`, one at a time, following this project's
established discipline (see `07-generics-implementation-plan.md`'s "Using
orc to dispatch implementation work" section for the general pattern this
project already uses): tightly scoped briefs, no `git stash`, scratch files
cleaned up before finishing, independent verification (real repro, real
before/after numbers, targeted test suite — not the full suite per slice)
before any commit, resume stalled sessions rather than redispatch fresh
when real progress exists in the worklog, never trust a "completed" status
without checking the actual diff and worklog content. 21.2b explicitly
needs a design-investigation dispatch before its implementation dispatch —
do not combine those into one task.
