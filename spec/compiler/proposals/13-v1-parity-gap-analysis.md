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
  multi-layer task. Escalate only after a real capability failure — and
  check `orc list` for the user's own concurrent Luna usage before ever
  escalating to Luna.
- Before each dispatch, require a clean worktree and no active Orc or
  OpenCode worker for this repository.
- After each dispatch, inspect the diff and check for scratch files, debug
  output, scope growth, and stale tests. Run the full required verification
  and a causation check before commit and push.
- Delete failed/stalled/killed Orc sessions with `orc delete` immediately,
  not just their scratch files.

## Active defect

### Descending range loops execute zero iterations

**Source:** proposal 14, "New findings" table, row "Descending range loops
execute zero iterations."

**Area:** backend generator, `compiler/internal/backend/statements.go`,
`buildRangeLoop` (line 996)

**Priority:** dangerous — silent wrong behavior, no compile error, no
runtime panic. The program simply skips the loop body entirely. This is
worse than a clean rejection because nothing signals that anything is
wrong; the same class of severity as the tagged-union C-type bug just
fixed, but with a much larger blast radius since a descending range needs
no special nesting or type to trigger — any ordinary `loop start..end` with
`start > end` hits it unconditionally.

**Reproduction:**

```pebble
fn main() int {
    var count = 0;
    loop 5..0 : i {
        count = count + 1;
    }
    return count;
}
```

`go run ./cmd/pebc -run` returns exit 0 (`count` stayed 0 — the loop body
never ran). An ascending range with the same span,
`loop 0..5 : i { count = count + 1; }`, correctly returns exit 5, confirming
the range machinery itself works and this is specifically a
direction/step-sign bug.

**Root cause, confirmed from the emitted C** (`go run ./cmd/pebc -o` and
inspecting the output directly):

```c
for (int32_t pebble_local_28 = 5; pebble_local_28 < 0; pebble_local_28++) {
```

The loop starts at `5`, but the condition is unconditionally `<` (never
`>`), and the step is unconditionally `++` (never `--`). Since `5 < 0` is
false on the very first check, the loop body never executes — it's not a
narrow off-by-one, the loop condition is inverted relative to the actual
iteration direction. Matches proposal 14's own analysis exactly: "V2 always
writes `<`/`<=` and `iterator++` ... a descending range runs zero times."
V1 (`codegen.c:2568`, per the audit) evaluates both bounds once, chooses
step `+1` or `-1` based on their relative order, and emits the matching
comparison direction.

**Not yet done:** find the exact comparison-operator and step-sign
selection logic inside `buildRangeLoop` and fix it to choose `>`/`--` when
the start bound is greater than the end bound (a compile-time-constant
check when both bounds are literals; likely needs a runtime `>` vs `<`
branch, matching V1's approach, when a bound is not a literal — check
whether the inclusive/exclusive range distinction and this direction choice
interact before implementing). Next step: dispatch through Orc per the
tracker's dispatch rules.
