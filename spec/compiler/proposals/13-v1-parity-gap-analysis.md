# 13 — v1 parity gaps

**Purpose.** This file is the working area for exactly ONE gap at a time,
sourced from `14-v2-v1-checker-backend-parity-audit.md`'s master findings
list. It is not a backlog. Proposal 14 is the backlog and the completion
record; this file is the scratch pad for whichever single item is currently
being reproduced, worked, and closed.

## Workflow

1. Pick the next unaddressed item from proposal 14 (its "Confirmed open
   tracker items" list or its "New findings" table).
2. Reproduce it for real. Document the item here, in "Active defect," with
   its exact reproduction, current failure, and known cause — this file must
   never hold an item that hasn't been reproduced.
3. Work it. Any sub-issue discovered while working it (a stale test, a
   related but distinct bug, a scope question) gets recorded here too, under
   the same active item, not spun off silently.
4. When satisfactorily complete (verified, tested, committed): wipe this
   file's "Active defect" section back to empty, mark the corresponding item
   complete in proposal 14, and pick the next item. Never carry more than one
   active item here.
5. When proposal 14's whole list is exhausted, ask Sol/Codex for another
   audit pass to find what's still missing.

## Dispatch rules (apply to the current active item only)

- Dispatch compiler and runtime logic through Orc.
- Use one small, decisive Orc slice at a time. Review and verify each slice
  before the next dispatch.
- Use `opencode-go/deepseek-v4-flash` by default. Do not give it a long,
  multi-layer task. If flash stalls or fails, escalate to
  `opencode-go/mimo-v2.5`, then `openai/gpt-5.6-luna` if mimo also fails —
  and check `orc list` for the user's own concurrent Luna usage before ever
  escalating to Luna. When escalating a stuck session, prefer resuming the
  same session with the new `--model` over deleting and dispatching fresh.
- Before each dispatch, require a clean worktree and no active Orc or
  OpenCode worker for this repository.
- After each dispatch, inspect the diff and check for scratch files, debug
  output, scope growth, and stale tests. Run the full required verification
  and a causation check before commit and push.
- Delete failed/stalled/killed Orc sessions with `orc delete` immediately,
  not just their scratch files.

## Active defect

### Extern variables/constants have no backend declaration/read/write path

**Source:** proposal 14, "New findings" table, row "Extern variables and
constants have no backend declaration/use path."

**Area:** backend generator; root cause in
`compiler/internal/tir/node.go:35` (`ExternDeclaration`, the single TIR
node kind used for both extern functions and extern data)

**Priority:** high — batch item 2 of 7. Real, ordinary feature (accessing
extern C globals like `errno`), flatly absent, matching the same shape as
the mutable-globals gap just fixed (`14739f3`), reusable pattern.

**Reproduction:**

```pebble
extern {
    var errno int;
}

fn main() int {
    return errno;
}
```

`go run ./cmd/pebc -run` fails: `pebc: emission failed: entry function body
expression references symbol N, which is not a local declared earlier in
the entry body` — the checker accepts the extern variable declaration
(the failure is purely at emission), but the backend has no path for it.

**Root cause:** only one `ExternDeclaration` TIR node kind exists (single
entry in `node.go`'s kind table, `CategoryNonvalue`), used uniformly for
both `extern fn` declarations and extern data declarations. The backend
only knows how to treat this shape as "a callable extern function" (see
`externCName`, `emit.go:810`, used to resolve an extern function's real C
name for a call) — there is no extern-data emission or reference-resolution
path at all.

**Scope for this slice, directly reusable from the just-landed
mutable-globals fix (`14739f3`):**
1. Emit a forward `extern <ctype> <realCName>;` declaration for each
   referenced extern variable — NOT a `static ... = ...;` definition with
   synthesized storage like the mutable-globals fix does, since an extern
   variable's real storage is defined elsewhere (e.g. inside libc itself);
   this backend only needs to declare it exists and its type, then
   reference it by its REAL C name (reuse `externCName` exactly as extern
   functions already do — do not synthesize a `pebble_global_<id>`-style
   name for extern data).
2. Make read references resolve to that real C name, reusing the same set
   of "not a local in scope" touch points the mutable-globals fix just
   updated (`values.go`, `places.go`, `statements.go` — check each site
   the prior fix touched, since extern-variable reads need the identical
   treatment, just resolving to a different name-construction rule).
3. Make write references (if extern variables can be mutable — confirm
   whether `extern { var ... }` vs `extern { let ... }` distinguishes
   mutability, and whether writing to an extern variable is even something
   this reproduction needs, or whether read-only is the realistic first
   scope) resolve via `buildStoreCore`/`buildCompoundStore`, mirroring the
   mutable-globals fix's write-path changes.

**Not yet done:** the actual implementation. Next step: dispatch through
Orc per the tracker's dispatch rules, pointing directly at commit `14739f3`
as the pattern to mirror.
