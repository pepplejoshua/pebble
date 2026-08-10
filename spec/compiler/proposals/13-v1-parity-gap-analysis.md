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

**Item: a negative integer literal in a switch case label (`case -5:`) is rejected outright for a signed subject type.**

Batch item 1 of a new batch of 7.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let x i16 = -5;
    switch x {
        case -5: return 1;
        else: return 0;
    }
}
```

Current failure:

```
pebc: emission failed: switch case contains an integer literal with
malformed text "-5"
```

**Known cause:** `buildCaseLabel`'s `isNonNegativeDecimal` guard has no
negative-literal path at all — it assumes every integer case-label
literal's text is a plain non-negative decimal string, but a negative
case label's literal text apparently includes the leading `-`
(or the CaseValue node is built differently for a negative literal —
needs isolating whether the checker even produces a well-formed negative
`CaseValue` before the backend sees it).

**Scope for this item:**
1. First isolate whether this is purely a backend text-parsing gap
   (`isNonNegativeDecimal` rejecting a leading `-`) or whether the
   checker/TIR builder does something different for a negative case-label
   literal that also needs fixing.
2. Fix `buildCaseLabel` (and `isNonNegativeDecimal` or its caller) to
   accept a negative integer literal for a SIGNED subject type, emitting
   the correct negative C case label matching the subject's width.
3. Confirm a negative case label on an UNSIGNED subject type still
   cleanly rejects (that should remain a real error, not silently
   accepted).
4. Verify the reproduction above compiles and runs, returning 1.
5. Write tests covering: a negative case label matching, a negative case
   label on the entry-width `int` type, and continued rejection for an
   unsigned subject.

