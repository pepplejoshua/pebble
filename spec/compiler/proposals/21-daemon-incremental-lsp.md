# 21 — persistent daemon, incremental compilation, and an LSP core

**Status:** all slices except 21.2b/21.2c are complete. 21.1a, 21.1b, 21.2a,
21.3, 21.4a, 21.4b, and 21.4c are implemented, committed, and pushed
(`ca653de`, `c7ddf92`, `d4cdbb2`, `23dce8b`, `16e39fa`, `c42c4df`,
`9708b19`) — a working daemon, `pebc dev`, and an LSP core (handshake,
diagnostics-on-save, hover) all exist and are verified working today.
**21.2b (module-scoped resolve/check) is BLOCKED**:
its own required design investigation (session `ses_6a864385d74fb0faae4048c7`,
independently spot-verified) returned a decisive verdict — a correct
module-scoped recheck is not achievable as a minimal wrapper around
`check.Check`; it needs a real refactor (persistent cross-request identity,
an explicit module-interface boundary, scoped solving, conservative
prelude invalidation). Per an explicit decision (2026-08-19), that refactor
is scoped as its own initiative — see
`spec/compiler/proposals/22-incremental-check-refactor.md` — rather than
being squeezed into this slice. 21.2b/21.2c in this document are
superseded by that plan; do not implement them here. This document is
updated in place as each slice lands, the same way `07`/`19` are.

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
- **21.1b — file watching + content-hash change detection** (`c7ddf92`):
  fsnotify wired into the daemon's event loop; content-hash tracking
  scoped to the module graph of the last successful build (threaded out of
  `compileOnce` via a new `compileResult.files`), not a whole-project glob
  — a file outside the current program's import graph is never tracked or
  reported. Detection only at this slice — nothing skips real work yet,
  that's 21.2. New `daemon watch-status` RPC for observability. One real
  bug found and fixed: a reentrant-mutex deadlock (`serveBuild` holds
  `d.mu` for a build's duration and called `trackFiles`, which tried to
  re-lock the same non-reentrant mutex) — caught via a goroutine dump
  (SIGQUIT) after a live reproduction hung on the very first build
  request, fixed by splitting into a locked wrapper and an internal
  `trackFilesLocked` used by callers that already hold the lock.
- **21.2a — reverse-dependency (importer) index** (`d4cdbb2`):
  `Graph.ReverseDependents()` (direct importers, cached at build time
  alongside the existing dependency order) and
  `Graph.TransitiveDependents(id)` (full importer closure via BFS,
  computed fresh per call, ordered so a module always precedes its
  importers). Pure data structure, not wired into anything yet — the
  building block 21.2b needs.
- **21.3 — `pebc dev`, the fast rebuild-restart front end** (`23dce8b`):
  `pebc dev <entry.peb>` auto-starts a daemon if none is running, does an
  initial build, launches the executable as a supervised child with
  stdio forwarded live, then polls `watch-status` (300ms default) and
  rebuilds+restarts on a detected change. A failed build leaves the
  last-known-good child running and prints diagnostics rather than
  killing it. Ctrl-C kills the child but deliberately leaves the daemon
  running (it self-manages via its own idle timeout). Does not depend on
  21.2b/22 — uses the existing full-recheck daemon build path and will
  get faster automatically once `22` lands. Verified with a genuinely
  long-running child (a busy-loop program) across a real compile error,
  confirming it survives untouched and recovers once fixed.
- **21.4a — LSP transport skeleton** (`16e39fa`): `pebc lsp` starts an
  LSP server over stdio using `go.lsp.dev/protocol` (go directive
  bumped to 1.26.0, required by the library). Implements only the
  initialize/shutdown/exit handshake — no real features yet. Found and
  fixed a real bug: the stdio adapter's `Close()` was a no-op, so
  `jsonrpc2.Conn.Close()` (called from the `exit` handler) could never
  unblock the read goroutine blocked in `os.Stdin.Read()` — the process
  hung forever on `exit` instead of terminating; fixed by having
  `Close()` actually close `os.Stdin`. Verified independently with both
  the dispatch's own subprocess test harness and a from-scratch manual
  Content-Length-framed JSON-RPC handshake. Fifth dispatch attempt on
  this slice — the first four (two models) died 15-20s in mid-tool-call
  with a false "completed" status, an apparent transient infra issue
  (confirmed via near-zero token spend at time of death); `opencode-go/
  hy3` completed it correctly on the retry after a smoke test confirmed
  it and two other candidate models' health.
- **21.4b — diagnostics on save** (`c42c4df`): `textDocument/didSave`
  triggers a full build via the existing daemon path and publishes real,
  positioned `protocol.Diagnostic`s (a new `structuredDiagnostic` list
  threaded through the daemon RPC via `Diagnostic.Primary.Span` +
  `source.File.Position`, kept alongside the existing plain-text
  diagnostics unchanged). Captures the real workspace root from the
  editor's `initialize` params rather than assuming process cwd. Always
  publishes, including an empty array on a clean build, to clear stale
  markers. Two real findings surfaced and documented rather than
  silently worked around: a `go.lsp.dev/jsonrpc2` stream-reading race
  where two zero-gap notifications can silently drop the second one's
  dispatch (no real editor fires didOpen/didSave with zero gap, so
  documented as a known limitation, not fixed); and a latent daemon
  robustness gap (21.1a, not new here) — the Unix socket path can
  exceed macOS's 104-byte `sun_path` limit for deep project roots,
  confirmed with a real 107-byte path that silently failed to bind
  (found via Go's own `t.TempDir()` nesting; flagged as a future
  daemon-hardening item, not fixed in this slice).
- **21.4c — hover** (`9708b19`): `textDocument/hover` via a new daemon
  `hover` RPC — finds the smallest syntax node at the requested byte
  offset, maps it through `tir.Unit.SourceMap` to its checked TIR node,
  and renders the type with `types.DescribeKey`. No warm state reused
  (same as 21.4b), so every hover pays a full recheck — correct, not
  instant, explicitly acceptable pending `22`. A "gap" found during
  manual review (hovering a variable reference, not just a literal,
  seemed to return nothing) turned out to be an unrelated type error in
  the reproduction program, not a real bug — confirmed by a dispatched
  follow-up and locked in as a passing test
  (`TestLSPHoverVariableReference`) with no code changes needed. This
  closes out the full 21.4 slice sequence (transport, diagnostics,
  hover) and the daemon-side half of this proposal — only the blocked
  21.2b/21.2c remain, tracked in `22-incremental-check-refactor.md`.

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
- **21.1b — file watching + content-hash change detection.** Done — see
  "Completed slices" above.

### 21.2 — Real incremental invalidation

- **21.2a — reverse-dependency index.** Done — see "Completed slices"
  above.
- **21.2b — module-scoped resolve/check entry points.** BLOCKED — see
  "Status" at the top of this document. Superseded by
  `22-incremental-check-refactor.md`. Do not implement here.
- **21.2c — daemon wiring for scoped rechecking.** BLOCKED on 21.2b, same
  as above. Note: this does NOT block 21.3/21.4 below — both can proceed
  today using the existing full-recheck-per-request daemon build (21.1a/
  21.1b/21.2a already provide a working, measurably-faster-than-cold
  rebuild trigger; they just don't skip unchanged work yet). 21.3/21.4
  will automatically get faster once `22` lands, without needing their
  own call sites to change — `pebc dev`/the LSP core call the same daemon
  build path either way.

### 21.3 — `pebc dev`: the user-facing fast-rebuild-restart front end

Done — see "Completed slices" above.

### 21.4 — LSP core

- **21.4a — transport.** Done — see "Completed slices" above.
- **21.4b — diagnostics on save.** Done — see "Completed slices" above.
- **21.4c — hover.** Done — see "Completed slices" above.

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
