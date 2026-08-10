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

**Item: composite print slice 4 — slices (dynamic runtime loop).**

Sourced from proposal 17, slice 4 of 9. Slices 1-3 (struct/tuple/array of
scalars, then nested aggregates) landed in `c182e73`/`5e6e786`/`b80fbc4`.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    var arr [3]int = [1, 2, 3];
    var s []int = arr[:];
    print s;
    return 0;
}
```

Fails with `error[C0612]: print operand is not printable`.

**Scope for this slice:** per proposal 17 —
- Format: `[1, 2, 3]`, empty slice prints `[]`.
- Unlike the fixed array (slice 3, compile-time unrolled since length is
  part of the type), a slice's length is RUNTIME, so this needs a real C
  `for` loop over `.len`/`.data` — see proposal 17's own sketch:
  ```c
  fprintf(stdout, "[");
  for (size_t i = 0; i < value.len; i++) {
      if (i != 0) fprintf(stdout, ", ");
      /* recursively emitted element formatter */
  }
  fprintf(stdout, "]");
  ```
  The element formatter itself is still statically generated (reuse
  `buildPrintValueCalls`'s recursion from slice 3) — only the iteration
  COUNT is dynamic.
- Start with a slice of scalars, then (if it composes naturally through
  the existing recursion) a slice of structs — do not force it if slice
  elements interact awkwardly with the existing per-element fprintf-call
  list shape (a runtime loop can only contain a FIXED sequence of C
  statements per iteration, so this may need the element's calls
  collapsed into one loop body rather than emitted N times — think this
  through before implementing, and note any real design tension found).
- Verify: `[1, 2, 3]` for a 3-element slice, `[]` for an empty slice.
- Confirm slices 1-3 and scalar prints are unaffected.
- Write tests: checker acceptance for a printable slice, checker
  rejection for a slice of a not-yet-printable element type (e.g. enum),
  and backend compile-run tests for a multi-element slice and an empty
  slice, asserting exact printed output.

