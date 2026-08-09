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

**Item: whole struct-local reassignment is rejected by the backend (checker accepts it).**

Batch item 6 of the agreed 7. One narrow slice of proposal 14's broader
"Whole tuple, array, struct, enum, and string copy/reassignment paths are
incomplete" finding, scoped specifically to struct — mirroring the
already-supported enum/slice reassignment cases in `buildStoreCore`.

**Reproduction 1 — reassignment through a pointer deref** (confirmed
against current HEAD):

```
type Point = struct {
    x int;
    y int;
};

fn reset(self *Point, other Point) void {
    *self = other;
}

fn main() int {
    var p = Point.{ x = 1, y = 2 };
    let q = Point.{ x = 9, y = 9 };
    reset(&p, q);
    return p.x;
}
```

Current failure:

```
pebc: emission failed: entry function body block reassigns an element of
type nominal(symbol 24), want a fixed-width integer, char, bool, pointer,
enum, str, or slice
```

**Reproduction 2 — plain local reassignment** (confirmed against current
HEAD):

```
type Point = struct {
    x int;
    y int;
};

fn main() int {
    var p = Point.{ x = 1, y = 2 };
    let q = Point.{ x = 9, y = 9 };
    p = q;
    return p.x;
}
```

Current failure:

```
pebc: emission failed: entry function body block reassigns symbol 30, a
struct-typed local of type nominal(symbol 24); reassigning a whole struct
is not supported yet
```

**Known cause:** the checker fully accepts both forms. `buildStoreCore`
(`compiler/internal/backend/stores.go`) already has explicit branches for
bool, integer, char, pointer, str, enum, and slice element types on the
deref/field-write path — struct is simply missing, falling through to the
generic rejection error. The plain-local path
(`compiler/internal/backend/locals.go` or wherever "reassigning symbol N"
is raised) has its own explicit "not supported yet" rejection for struct
specifically, separate from the deref path's generic fallback.

**Scope for this item:**
1. Add a struct branch to `buildStoreCore`'s element-type switch, mirroring
   the existing enum/slice branches — a whole-struct value assigned through
   a pointer deref or field write should emit a real member-wise (or
   memcpy-equivalent) C assignment.
2. Fix the plain-local reassignment path to allow `p = q;` where both are
   the same struct type, using the same underlying mechanism as (1) if
   practical.
3. Verify both reproductions above compile and run correctly end-to-end
   (returned values must reflect the reassigned struct's fields, not the
   original).
4. Decide and test the array/tuple-of-struct-element edge case only if it
   falls out naturally from the fix — do not go looking for it separately;
   that is explicitly a different, broader finding in proposal 14.
