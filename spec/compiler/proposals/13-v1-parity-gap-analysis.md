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

**Item: composite print slice 2 — tuples and fixed arrays of scalars.**

Sourced from proposal 17 (composite print design), slice 2 of 9. Slice 1
(struct-of-scalars) landed in `c182e73`.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let t = (1, true, "hi");
    let a = [10, 20, 30];
    print t;
    print a;
    return 0;
}
```

Both fail with `error[C0612]: print operand is not printable`.

**Scope for this slice:** per proposal 17's design —
- Tuple: `(1, true, hi)`; a one-element tuple uses a trailing comma,
  `(1,)`, to avoid ambiguity with a parenthesized expression.
- Fixed array: `[10, 20, 30]`, compile-time unrolled (length is part of
  the type).
- Elements are scalar only at this slice (bool, char, str, integer,
  float) — a tuple/array containing a struct, another tuple, another
  array, etc. is a LATER slice (slice 3, nested aggregates); stays
  rejected for now.
- Reuse `buildScalarPrintParts` (introduced in slice 1) for every element
  value — do not reimplement scalar formatting.
- Reuse the direct-sequential-fprintf emission shape slice 1 established
  (`printFprintfCall`, `buildSequentialPrint`) — a tuple/array operand
  should plug into the same dispatch a struct operand uses, not a
  separate mechanism.
- Verify both reproductions print exactly `(1, true, hi)` and
  `[10, 20, 30]`.
- Confirm slice 1 (struct print) and existing scalar prints are
  unaffected.
- Write tests: checker acceptance for tuple-of-scalars and array-of-
  scalars, checker rejection still holds for a tuple/array containing a
  non-scalar element, and backend compile-run tests asserting exact
  printed output for both shapes plus the one-element-tuple trailing-
  comma case.

