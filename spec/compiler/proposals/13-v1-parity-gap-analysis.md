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

**Item: fixed-width integers other than the entry width are not accepted as a backend switch subject (checker proves exhaustiveness fine).**

Batch item 2 of a new batch of 5.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let x u8 = 5;
    switch x {
        case 5: return 1;
        else: return 0;
    }
}
```

Current failure:

```
pebc: emission failed: switch subject has type u8, want int, bool, or
char, or an enum/tagged-union type
```

**Known cause:** the backend's switch-subject type gate only accepts
`int` (the entry width), `bool`, `char`, or an enum/tagged-union — no
other fixed-width integer (`u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `i64`,
`u64` when not the entry width) is accepted, even though the checker
already proves exhaustiveness correctly for these widths (confirmed
working for `u8`/`i8` specifically by the `4817dae` fix earlier
tonight). Same class of gap as the already-fixed char-switch item — just
never extended past `char`/`int`.

**Scope for this item:**
1. Widen the backend's switch-subject type gate to accept any
   fixed-width integer type, not just the entry width — mirroring the
   convention already used elsewhere in this codebase for widening a
   type-acceptance gate to "any fixed-width integer" (e.g. struct field
   types, task #16 from earlier sessions).
2. Verify the reproduction above compiles and runs, returning 1.
3. Confirm a switch on the entry-width `int` type is unaffected (still
   works exactly as before).
4. Write compile-run tests covering at least `u8` and one other
   non-entry width (e.g. `i16` or `u32`).

