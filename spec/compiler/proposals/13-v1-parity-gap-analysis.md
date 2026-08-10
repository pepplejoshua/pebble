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

**Item: whole struct-local reassignment from a struct-returning call expression is rejected (both plain local and pointer-deref).**

Batch item 4 of the current batch of 5. Narrow follow-on to `9df0351`
(whole struct-local reassignment), which deliberately scoped out this
exact case.

**Reproduction 1 — plain local** (confirmed against current HEAD):

```
type Point = struct {
    x int;
    y int;
};

fn make_point() Point {
    return Point.{ x = 9, y = 9 };
}

fn main() int {
    var p = Point.{ x = 1, y = 2 };
    p = make_point();
    return p.x;
}
```

**Reproduction 2 — pointer deref** (also confirmed against current HEAD):

```
type Point = struct {
    x int;
    y int;
};

fn make_point() Point {
    return Point.{ x = 9, y = 9 };
}

fn reset(self *Point) void {
    *self = make_point();
}

fn main() int {
    var p = Point.{ x = 1, y = 2 };
    reset(&p);
    return p.x;
}
```

Both currently fail identically:

```
pebc: emission failed: entry function body block reassigns a
struct-typed place of type pebble_struct_23_t from a DirectCall, want a
reference to a struct-typed local in scope or a struct literal (a
RecordConstruct)
```

**Known cause:** `buildStructStoreValue` (introduced in `9df0351`) only
accepts a `RecordConstruct` (struct literal) or a `SymbolValue`
(reference to an in-scope struct-typed local) as the new value's node
kind — a `DirectCall` (a call to a struct-returning helper) is a clean
rejection, matching `buildAggregateArgument`'s existing discipline at the
time, but explicitly logged as a narrower remaining gap.

**Scope for this item:**
1. Extend `buildStructStoreValue` to also accept a `DirectCall` node
   whose result type matches the target struct type — check how a
   struct-returning call is already built as a VALUE elsewhere in this
   backend (e.g. a struct-returning call used to initialize a `let`
   local, or as a call argument) to find the existing builder/pattern for
   materializing a struct-returning call's result as a C value
   expression, and reuse it rather than inventing new lowering.
2. Verify both reproductions above compile and run correctly end-to-end
   (both should return 9, the field of the value returned by
   `make_point()`).
3. Confirm the existing supported shapes (struct literal, struct-typed
   local reference) are unaffected.
4. Write compile-run tests for both reproduction shapes.

