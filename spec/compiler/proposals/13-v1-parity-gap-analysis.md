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

**Item: `str + str` is checker-accepted but has no backend lowering; policy decided — reject it at the checker instead of implementing it.**

Batch item 7 of the agreed 7 (final item).

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let a = "hello";
    let b = "world";
    let c = a + b;
    return c.len as int;
}
```

Current failure:

```
pebc: emission failed: entry function body block declares a str-typed
local initialized from a BinaryValue, want a StringLiteral (a string
literal) or a call to a str-returning helper; initializing a str local
from another value is not supported yet
```

**Decision (direct instruction, 2026-08-09):** primitive `str + str`
should be a checker-level type error, not implemented. Plain `str` is
meant to be an immutable view; implementing runtime concatenation would
require an implicit allocator, entangling with the Allocator/Context
redesign that was explicitly deferred earlier in this session (see
proposal 15). `std/string.peb`'s `String` type with `push_str` already
covers real concatenation with an explicit, visible allocator. This is a
**policy decision, not a bug fix** — the fix is to move the rejection
earlier (to the checker) with a clear message, not to implement
concatenation.

**Scope for this item:**
1. Find where the checker currently accepts `+` between two `str`
   operands (likely wherever binary-operator operand types are validated)
   and add an explicit rejection for `str + str` (and, if it also reaches
   this path, `str + <anything>`), with a clear diagnostic message
   pointing at `String`/`push_str` as the alternative, matching this
   project's diagnostic style/codes.
2. Confirm other `+`-eligible types (int, uint, float, etc.) are
   unaffected — this must be scoped exactly to `str` operands.
3. Verify the reproduction above now fails at the checker stage with a
   clear message instead of the backend's generic "not supported yet"
   error.
4. Write a test in whichever checker test file is idiomatic for binary
   operator validation.

