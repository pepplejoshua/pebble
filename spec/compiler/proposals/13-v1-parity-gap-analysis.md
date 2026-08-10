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

**Item: qualified static method calls on nominal types are unsupported (`Point.origin()` for a self-less method inside a struct/enum/union body).**

Batch item 3 of the current batch of 7. Larger than the mechanical
widenings — this is a real, previously-unimplemented feature (matches
proposal 14 confirmed-open item 4), not a one-line fix.

**Reproduction** (confirmed against current HEAD):

```
type Point = struct {
    x int;
    y int;

    fn origin() Point {
        return Point.{ x = 0, y = 0 };
    }
};

fn main() int {
    let p = Point.origin();
    return p.x;
}
```

Current failure:

```
static_method.peb:11:13: error[C0619]: invalid, foreign, or over-limit
semantic record
      let p = Point.origin();
              ^
static_method.peb:11:13: error[T0510]: inference variable has no
unique semantic type
static_method.peb:12:12: error[T0510]: inference variable has no
unique semantic type
```

Also confirmed `Point::origin()` (double-colon) is NOT the intended
syntax — that's reserved for module-qualified calls and correctly
rejects with `error[N0003]: "Point" does not identify an imported
module`. The dot form (`Point.origin()`) is the natural syntax, matching
this codebase's existing conventions for struct literals (`Point.{ x =
0 }`) and union variant constructors (`Choice.value(42)`).

**Known cause (not yet fully root-caused):** the opaque `C0619` suggests
the checker's member/call resolution doesn't currently have a path for
"a bare nominal TYPE NAME (not an instance value) followed by `.method(
...)` where `method` is declared with no `self` parameter." It likely
falls through whatever generic member-record classification exists into
an "invalid/foreign" catch-all rather than being recognized as a static
call at all. This needs real investigation before implementation — do
NOT guess at a fix without first tracing exactly which code path
produces `C0619` for this case and confirming there's no existing but
disconnected static-method machinery already in the checker (grep for
any existing handling of a self-less method inside a struct/enum/union
declaration body — those methods must already parse and typecheck as
declarations even if never callable this way, so check what currently
happens to them).

**Scope for this item:**
1. Investigate: find where `C0619` is raised, trace back why `Point` (a
   type name, not a value) reaches that path when followed by
   `.origin()`. Compare against how an INSTANCE method call (`p.some_method()`)
   is currently resolved, and how a variant constructor (`Choice.value(42)`)
   is currently resolved — the static-method case sits between these two
   existing patterns and should reuse as much of their machinery as
   possible.
2. Implement checker-side recognition: a bare nominal type name as a
   member-access base, where the accessed member is a self-less method
   declared in that type's body, resolves as a static call (not an
   instance method call, not a struct-literal construction, not a
   variant constructor).
3. Implement backend lowering: the static call should compile to a
   direct call to the method's underlying function symbol, exactly like
   any other direct function call — check how an instance method call
   already lowers (self is just the first argument) and adapt for the
   self-less case.
4. Verify the reproduction above compiles and runs, returning 0.
5. Confirm existing instance-method calls, struct literals, and variant
   constructors are completely unaffected.
6. Write tests for the checker acceptance and a compile-run test for the
   full call.

