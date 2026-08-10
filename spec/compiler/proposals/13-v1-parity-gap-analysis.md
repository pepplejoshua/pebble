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

**Item: an array literal cannot directly initialize a slice-typed local (confirmed general, not specific to non-primitive elements as proposal 14's title implies).**

Batch item 5 of the current batch of 7.

**Reproduction 1 — primitive element** (confirmed against current HEAD):

```
fn main() int {
    var s []int = [1, 2, 3];
    return s[1];
}
```

**Reproduction 2 — non-primitive (struct) element** (also confirmed):

```
type Point = struct {
    x int;
};

fn main() int {
    var s []Point = [Point.{ x = 1 }, Point.{ x = 2 }];
    return s[1].x;
}
```

Both fail identically:

```
error[C0601]: cannot convert value for assignment
      var s []int = [1, 2, 3];
      ^
```

**Confirmed working (no gap here):** an array literal DOES already
correctly initialize an ARRAY-typed local, even with struct elements
(`var arr [2]Point = [Point.{x=1}, Point.{x=2}];` compiles and runs
correctly). The existing two-step workaround for a slice also already
works: `var arr [3]int = [1, 2, 3]; var s []int = arr[:];`. So the gap
is narrowly: a slice-typed binding's initializer specifically cannot be
a bare array literal in one step — it must go through an intermediate
array local first.

**Scope for this item:**
1. Investigate where `C0601` "cannot convert value for assignment" is
   raised for this shape, and how the checker currently validates a
   slice-typed binding's initializer (compare against how it already
   accepts an array-typed binding's array-literal initializer, and how
   it already accepts `arr[:]` as a slice-typed binding's initializer —
   this new case sits directly between those two working patterns).
2. Implement checker-side acceptance: a slice-typed binding's
   initializer may be a bare array literal whose element type and
   length are compatible with the slice's element type, treated
   equivalently to the array-then-slice two-step (implicitly: construct
   the array, then take a full slice of it).
3. Implement backend lowering: reuse the EXISTING array-literal
   construction machinery (already used for an array-typed local) plus
   the existing full-slice construction machinery — likely synthesizing
   a hidden array temp and slicing it, mirroring what the two-step
   workaround already does under the hood, rather than inventing new
   lowering.
4. Verify both reproductions above compile and run, returning 2 for each.
5. Confirm the existing array-typed-local array-literal case and the
   existing two-step array-then-slice case are both unaffected.
6. Write tests for both reproduction shapes.

