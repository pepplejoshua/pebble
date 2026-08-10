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

**Item: a generic struct method's parameter/return type cannot be the owner's type parameter directly (unwrapped).**

Batch item 6 of the current batch of 7. Real generic-specialization
substitution gap, architecturally related to the earlier "apply
generic-struct-field substitution" fix — that fix covered FIELD types;
this one is METHOD parameter/return types.

**Reproduction 1 — return type** (confirmed against current HEAD):

```
type Box[T] = struct {
    value T;

    fn get(self Box[T]) T {
        return self.value;
    }
};

fn main() int {
    let b = Box[int].{ value = 42 };
    return b.get();
}
```

Failure:

```
pebc: emission failed: called function symbol 27 has result type
type-parameter(symbol 25), want its own integer width, bool, char,
str, f32, f64, a tuple/struct result type, a slice result type, a
pointer result type, an optional result type, a function result type,
or void
```

**Reproduction 2 — parameter type** (also confirmed):

```
type Box[T] = struct {
    value T;

    fn set(self *Box[T], v T) void {
        self.value = v;
    }
};

fn main() int {
    var b = Box[int].{ value = 1 };
    b.set(42);
    return b.value;
}
```

Failure:

```
pebc: emission failed: called function symbol 27 parameter 1 (symbol
29) has type type-parameter(symbol 25), want a fixed-width integer
(int, uint, or u64), bool, char, str, f32, f64, a tuple/struct type, a
slice type, a pointer type, an optional type, a function type, or an
enum/union type
```

**Known cause (not yet fully root-caused):** for a concrete
instantiation like `Box[int]`, the backend already correctly
substitutes `T` for `int` in the struct's FIELD types (`value T` becomes
`value int`), and generic struct method CALLS already work in general
(per earlier session work). But when a method's own declared parameter
or return type is the owner's type parameter DIRECTLY (not wrapped
inside e.g. a slice or another generic), that substitution apparently
isn't applied to the method's signature — the type parameter symbol
reaches the backend completely unresolved. Needs investigation: find
where field-type substitution for a concrete instantiation happens and
why the equivalent substitution isn't also applied to a method's own
parameter/return types.

**Scope for this item:**
1. Investigate: find where generic struct field types get substituted
   for a concrete instantiation (the earlier field-substitution fix is
   the closest working precedent), and trace why a method's own
   parameter/return type — when it's directly the owner's type
   parameter — isn't substituted the same way.
2. Implement the substitution for method parameter/return types,
   reusing the existing field-substitution mechanism if at all possible
   rather than inventing a parallel one.
3. Verify both reproductions above compile and run correctly (repro 1
   returns 42, repro 2 returns 42).
4. Confirm existing generic struct method calls (where the
   parameter/return type is NOT directly the type parameter — e.g. a
   fixed concrete type, or the type parameter wrapped in something else)
   are unaffected.
5. Write tests for both reproduction shapes.

