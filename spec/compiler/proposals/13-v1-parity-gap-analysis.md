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

### The checker accepts a `str`-typed switch subject, but the backend has no lowering

**Source:** proposal 14, "New findings" table, row "V2 checker accepts
string switch, but backend has no lowering."

**Area:** backend generator, `compiler/internal/backend/statements.go`,
switch-subject builder

**Priority:** low — clean rejection, not a correctness hazard. Architecturally
bigger than the char-switch fix just landed (`72f0207`), so scoping it
correctly matters.

**Reproduction:**

```pebble
fn classify(s str) int {
    switch s {
        case "a": return 1;
        case "b": return 2;
        else: return 0;
    }
}

fn main() int {
    return classify("b");
}
```

Fails cleanly at emission (error message already updated by the char-switch
fix to list char, confirming that fix landed correctly):

```
pebc: emission failed: switch subject has type str, want int, bool, or
char, or an enum/tagged-union type
```

**Root cause and why this is NOT a same-shape fix as char:** every other
supported switch subject (int, bool, char, enum, tagged-union) maps
directly to a native C `switch` statement, because C `switch` only accepts
integer-constant case labels — char lowers to an `int32_t` scalar compare,
enum lowers to its underlying integer tag. A `str` subject cannot use a
native C `switch` at all; V1 (per the audit) emits it as an `strcmp`
if/else chain instead. This backend already has exactly the runtime helper
needed for that: `pebble_rt_str_eq` (used for `==`/`!=` on two `str`
values, see `buildComparison` in `values.go`). The fix is not "add a
branch to the existing switch builder" like char was — it's "detect a
`str`-typed subject early and emit a completely different C shape" (a
chain of `if (pebble_rt_str_eq(subject, "case1")) { ... } else if
(pebble_rt_str_eq(subject, "case2")) { ... } else { <default/else arm> }`),
sharing the case-body emission logic with the normal switch path but not
its native-`switch`-statement shape.

**Not yet done:** implement the if/else-chain lowering for a `str`-typed
switch subject, reusing `pebble_rt_str_eq` and whatever case-body-emission
helper the normal switch path already uses (so the arm bodies themselves —
including defer/break/fallthrough semantics if switch supports those —
stay consistent between the native-switch and if/else-chain lowering
paths). Confirm how `switch`-targeted `break` (a documented V2 extension
over V1, per proposal 14) should behave inside an if/else-chain lowering —
it likely needs to become a labeled break or an equivalent since there's no
enclosing native `switch`/loop construct to break out of. Next step:
dispatch through Orc per the tracker's dispatch rules.
