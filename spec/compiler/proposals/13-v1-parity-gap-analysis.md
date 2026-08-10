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

**Item: composite print slice 3 — nested aggregates (struct-of-struct, struct-of-tuple, struct-of-array, tuple-of-struct, array-of-struct).**

Sourced from proposal 17, slice 3 of 9. Slices 1 (struct-of-scalars) and 2
(tuple/array-of-scalars) landed in `c182e73`/`5e6e786`.

**Reproduction** (confirmed against current HEAD):

```
type Inner = struct { v int; };
type Outer = struct { inner Inner; };

fn main() int {
    let o = Outer.{ inner = Inner.{ v = 1 } };
    print o;
    return 0;
}
```

Fails with `error[C0612]: print operand is not printable`.

**Scope for this slice:** every field/element type printable at this
point (scalar, struct, tuple, array — any combination, arbitrarily
nested) is now accepted. A field/element of a type NOT yet printable
(enum, union, optional, slice, pointer) still rejects — those are later
slices.
- Checker: generalize `valuePrintable`'s struct/tuple/array field-type
  check to recurse — a field/element is acceptable if it's a scalar OR
  itself a printable struct/tuple/array (recursive definition), not just
  a flat scalar check.
- Backend: the existing `buildStructPrintOperand`/`buildTuplePrintOperand`/
  `buildArrayPrintOperand` (slices 1-2) need their per-field/per-element
  dispatch widened to recurse into a nested struct/tuple/array field
  (currently they likely only call `buildScalarPrintParts`, which will
  reject a non-scalar field/element type) — recursing means emitting that
  field/element's own nested punctuation+label+value sequence inline,
  not a separate print statement.
- The print expression must STILL be materialized exactly once at the
  operand level (as slices 1-2 already do) — a side-effecting nested
  aggregate read must not be re-evaluated per nesting level.
- Verify the reproduction prints exactly `Outer{ inner: Inner{ v: 1 } }`.
- Also verify at least one struct-of-tuple, one struct-of-array, one
  array-of-struct, and one tuple-of-struct case (proposal 17's own
  example, `Line{ a: Point{ x: 1, y: 2 }, b: Point{ x: 3, y: 4 } }`, is
  a good struct-of-struct-of-scalars fixture to include).
- Confirm slices 1-2 and all scalar prints are unaffected.
- Write tests: checker acceptance for at least struct-of-struct and
  array-of-struct, checker rejection still holds for a field/element of
  a not-yet-printable type (e.g. an enum field), and backend compile-run
  tests asserting exact printed output for the reproduction plus at
  least one other nested shape.

