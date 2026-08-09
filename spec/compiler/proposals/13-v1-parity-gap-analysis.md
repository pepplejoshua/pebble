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

### The checker accepts a `char`-typed switch subject, but the backend rejects it

**Source:** proposal 14, "New findings" table, row "V2 checker accepts
character switch, but backend rejects it."

**Area:** backend generator, `compiler/internal/backend/statements.go`,
the switch-subject builder (rejection at line 621) and `buildCaseLabel`

**Priority:** low — clean rejection, not a correctness hazard.

**Reproduction:**

```pebble
fn classify(c char) int {
    switch c {
        case 'a': return 1;
        case 'b': return 2;
        else: return 0;
    }
}

fn main() int {
    return classify('b');
}
```

The checker accepts this program; it fails cleanly at emission:

```
pebc: emission failed: switch subject has type char, want int or bool, or
an enum/tagged-union type
```

**Root cause:** the switch-subject builder (`statements.go:621`, inside the
function that builds a switch statement's subject expression) has explicit
branches for the entry integer width, bool, and enum/tagged-union subjects,
with no `isChar` branch — falls through to the generic rejection. This is
likely a two-part fix: the subject itself needs a `char`-aware branch
(there's already a `buildCharOperand` helper used elsewhere in the backend
for char-typed expressions — check `values.go`), and per proposal 14's own
audit note, case labels currently "accept only integer, bool, or enum" —
`buildCaseLabel` likely also needs a char-literal branch, not just the
subject.

**Not yet done:** add `char` support to both the switch-subject builder and
`buildCaseLabel`, reusing `buildCharOperand`/char-literal handling already
established elsewhere in the backend rather than inventing new char-lowering
logic. Next step: dispatch through Orc per the tracker's dispatch rules.
