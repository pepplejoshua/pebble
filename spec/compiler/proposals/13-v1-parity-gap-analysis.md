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

### Generic `Result[T, E]` methods cannot narrow `self` — `self.Ok`/`self.Err` rejected

**Source:** proposal 14's "Confirmed open tracker items" list, item 3.
Blocks `std/result.peb` and `examples/count_lines.peb`.

**Area:** checker (generic tagged-union self-narrowing)

**Priority:** batch item 4 of 7 — a real, commonly-needed language feature,
not an edge case. Genuinely one of this batch's harder items.

**Reproduction:** `compiler/std/result.peb` itself is the reproduction —
its own methods fail to compile:

```pebble
type Result[T, E] = union enum {
    Ok T;
    Err E;

    fn is_ok(self Result[T, E]) bool {
        switch self {
            case .Ok: return true;
            case .Err: return false;
        }
    }
    ...
```

Compiling any program that imports `std:result` and constructs a
`Result[T, E]` fails with, among others:

```
std:result.peb:14:30: error[C0605]: member operation is invalid
      case .Ok: return self.Ok;
```

(line 14, inside a switch arm that already narrowed `self` to the `.Ok`
case). The same failure recurs at lines 21, 22 (`map`'s narrowed reads) and
line 27 (`set_error`'s narrowed WRITE, `self.Err = error;`) — so both read
and write narrowing are broken for a generic union receiver.

Ordinary (non-generic-self) tagged-union narrowing already works
correctly — this is specific to a method whose receiver refers to its own
generic union type (`self Result[T, E]`).

**Root cause, established from earlier investigation this session (verify
it still holds before implementing — the codebase has changed since):** the
case-label's `aggregateEnumVariant` record loses its declaration identity
and reaches `Declaration=0`, because the generic receiver's template is
never materialized into `knownValues`, so `knownDestination` cannot recover
the nominal declaration for `self` inside the narrowed arm. Search the
checker for `aggregateEnumVariant`, `knownValues`, and `knownDestination`
to confirm this is still the exact mechanism before making changes.

**Scope for this slice (matches the item's own historical plan, not yet
executed):**
1. Fix read-side narrowing first: recover the declaration from the solved
   receiver type when the case-label aggregate has `Declaration=0`. Add
   positive (matching-arm read succeeds) and negative (wrong-arm/outside-arm
   read still correctly rejected) checker tests.
2. Fix narrowed writes (`self.Err = error;` inside `set_error`) as a
   separate, explicit step — the existing narrowing widening may only
   apply to read-side member validation; do not assume the read fix covers
   writes.
3. If reachable after 1-2, confirm whether a `str`-payload variant read
   through a narrowed generic self needs its own backend
   `Load(FieldPlace)` support, or whether that's already handled (an
   earlier, now-stale prediction claimed a gap here that later turned out
   not to exist for the non-generic case — verify freshly for the
   generic-self case specifically, don't assume either way).
4. Once the checker changes land, compile and run `compiler/std/result.peb`
   itself as the real consumer proof (the reproduction above), plus
   `examples/count_lines.peb` if it also exercises this path — check
   whether it does first.

**Not yet done:** the actual implementation. Next step: dispatch through
Orc per the tracker's dispatch rules. Given the checker-level, generic-type
nature of this fix, investigate first if the full scope looks too large for
one dispatch, and split if so — do not force read+write+backend into one
slice if that risks a rushed result.
