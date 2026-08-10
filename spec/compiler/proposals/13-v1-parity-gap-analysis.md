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

**Item: fixed-width integers other than the entry width are not accepted as a helper function parameter type.**

Batch item 3 of the current batch of 5.

**Reproduction** (confirmed against current HEAD):

```
fn take(x u8) int {
    return x as int;
}

fn main() int {
    let x u8 = 5;
    return take(x);
}
```

Current failure:

```
pebc: emission failed: called function symbol 24 parameter 0 (symbol 25)
has type u8, want int, bool, char, str, f32, f64, a tuple/struct type, a
slice type, a pointer type, an optional type, a function type, or an
enum/union type (a parameter may be the entry's integer width, uint,
u64, bool, char, str, f32, f64, a tuple/struct type, a slice type, a
pointer type, an optional type, a function type, or an enum/union type)
```

**Known cause:** `helperSignature`'s parameter-type acceptance gate
allows the entry width, `uint`, `u64`, and various non-integer types, but
not the narrower fixed-width integers (`u8`, `i8`, `u16`, `i16`, and,
unless already covered, `i32`/`u32` when not the entry width). Same class
of gap as the switch-subject widening just fixed in `2b3d684` — the
narrower widths were simply never added to this particular acceptance
gate.

**Scope for this item:**
1. Widen `helperSignature`'s parameter-type gate (and the matching return
   path / call-argument path if the same gate is shared) to accept any
   fixed-width integer builtin, mirroring the pattern just used for the
   switch-subject fix and the existing struct-field-type widening
   convention.
2. Verify the reproduction above compiles and runs, returning `take(x)`'s
   result (5).
3. Confirm existing entry-width/`uint`/`u64` parameters are unaffected.
4. Write compile-run tests covering at least `u8` and one other
   non-entry width parameter, plus a call passing a matching-width
   argument.
5. If `uint` is separately still broken elsewhere (per the note left in
   proposal 14 from the switch-subject fix), do NOT try to fix that here
   unless it falls out naturally — it's a distinct, already-logged item.

