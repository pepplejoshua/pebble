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

### `sizeof` on a fixed array passes the checker but the backend rejects it

**Source:** proposal 14, "New findings" table, row "`sizeof` a fixed array
passes validation but the backend rejects it."

**Area:** backend generator, `compiler/internal/backend/types.go`,
`sizeofCTypeName` (line 976)

**Priority:** low — clean rejection, not a silent-wrong-behavior bug like
the last three items. Missing capability, not a correctness hazard.

**Reproduction:**

```pebble
fn main() int {
    let s = sizeof [4]int;
    print "{}\n", s;
    return 0;
}
```

The checker accepts this (`let s = ...` type-checks fine); it fails at
emission with a clean Go-level diagnostic, not a raw `cc` crash:

```
pebc: emission failed: sizeof of type [4]int is not supported, want a
fixed-width integer, bool, char, str, tuple, optional, slice, enum,
struct, or pointer
```

**Root cause:** `sizeofCTypeName` has one branch per supported kind
(integer, bool, char, str, runtime type, tuple, optional, slice, enum,
struct, pointer) and simply has no `isArray` branch — arrays were never
added to this function's coverage. `arrayTypeName(id)` already exists
(used elsewhere for array typedef naming) and follows the exact same
`pebble_array_<typeID>_t` convention as every other type-name helper this
function already calls.

**Not yet done:** add an `isArray` branch to `sizeofCTypeName`, returning
`arrayTypeName(id)`, following the existing pattern. Also check — matching
the exact compounding issue found while fixing the tagged-union
`sizeof` bug (`f2e8c62`) — whether a bare `sizeof [N]T` with no other
reference to that array type anywhere in the program correctly forces the
array's typedef to be collected/emitted; if not, fix that too, the same way
`collectUnionTypesWalk` was extended for `SizeofType` nodes referencing a
union enum. Next step: dispatch through Orc per the tracker's dispatch
rules.
