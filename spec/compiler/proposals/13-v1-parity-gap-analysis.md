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

**Item: tagged-union variant write (`self.Err = e;`) is rejected everywhere, and the backend's existing lvalue-write path for a union variant payload never sets the tag.**

Continuation of batch item 4 (Result[T,E] narrowing, read-side landed in
`7b7eee0`). This finishes the write-side that was deliberately deferred
there. Picked ahead of the originally-planned "whole struct-local
reassignment" slice because it's fresher, better-scoped, and unblocks
`std/result.peb` compiling end-to-end.

**Reproduction** (`/tmp/.../union_write.peb`, confirmed against current
HEAD just now):

```
type Choice = union enum {
    Ok int;
    Err str;
};

fn set_err(self *Choice, e str) void {
    self.Err = e;
}

fn main() int {
    var c = Choice.Ok(5);
    set_err(&c, "oops");
    switch c {
        case .Ok: return 1;
        case .Err: return 0;
    }
}
```

Current failure:

```
union_write.peb:7:5: error[C0605]: member operation is invalid
      self.Err = e;
      ^
```

**Known cause** (established during item 4's investigation): the checker
unconditionally rejects any direct write to a union variant payload member
— fails identically for a plain non-generic union, so it is not
generics-specific. Separately, the backend's existing lvalue-write path
(`places.go`'s `buildPlaceLValue`, `unionVariantPayloadMember` case) writes
the payload value but does NOT update the union's `.tag`. So the checker
gap and the backend gap are two parts of one fix: lifting the checker
restriction naively, without also fixing the backend to set the tag on
write, would silently corrupt any later `switch`/narrowing read on that
value.

**Scope for this item:**
1. Decide and implement checker-side semantics: allow `self.Variant = value;`
   (and non-pointer-receiver / plain local equivalents) when writing to a
   union variant's own payload member, matching read-side narrowing rules
   sensibly (likely: only inside a context where the write is to the
   union's own declared variant name — same member as today's read checks
   use).
2. Fix the backend's union variant payload write path to also set `.tag`
   to the correct variant discriminant when writing the payload.
3. Verify: the repro above compiles and runs correctly end-to-end (tag
   ends up correct after the write, subsequent switch narrows correctly).
4. Verify `std/result.peb`'s `set_error` now compiles, and if practical,
   a real end-to-end program using `result::Result[T,E]` with a write
   then a narrowed read.
