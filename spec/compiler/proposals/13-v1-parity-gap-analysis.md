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

### A non-trivial range end expression is evaluated on every loop iteration instead of once

**Source:** proposal 14, "New findings" table, row "A nontrivial range end
is evaluated on every loop test instead of once."

**Area:** backend generator, `compiler/internal/backend/statements.go`,
`buildRangeLoop` (start ~996, the specific splice at line 1074)

**Priority:** dangerous — silent wrong behavior, same class as the
descending-range-loop bug just fixed. No compile error, no panic; a
side-effecting or expensive range-end expression just silently runs far
more times than the source implies.

**Reproduction:**

```pebble
fn bound() int {
    print "bound called\n";
    return 3;
}

fn main() int {
    var count = 0;
    loop 0..bound() : i {
        count = count + 1;
    }
    return count;
}
```

`go run ./cmd/pebc -run` prints `bound called` **4 times** for a 3-iteration
loop (once per condition check, i.e. once per iteration plus the final
failing check), not once. The loop's own iteration count is otherwise
correct (`count` comes back as 3) — this is purely about how many times the
end-bound expression itself gets evaluated, not about wrong iteration
counts.

**Root cause, confirmed from the emitted C:**

```c
for (int32_t pebble_local_29 = 0; pebble_local_29 < pebble_fn_24(ctx); pebble_local_29++) {
```

The call to `bound()` (`pebble_fn_24`) is spliced directly into the C `for`
loop's condition via `endText` (`statements.go:1074`,
`buildRangeLoop`'s final `fmt.Sprintf`). `endText` comes from
`buildRangeBound` (line 1013), which returns the end bound's raw expression
text — for a non-literal bound like a function call, that's the call
expression itself, not a value bound to a C local first. Ordinary C `for`
loop semantics re-evaluate the condition before every iteration, so the
call runs once per iteration check. The loop's *start* bound is correctly
evaluated once (assigned into the C loop-variable initializer), so only the
*end* bound has this problem.

**Also confirmed unaffected:** the descending-range fix just landed
(`8baeb8e`) is scoped to literal-bound ranges only, so it doesn't touch
this bug's non-literal-bound code path — the two are independent, not
overlapping.

**Not yet done:** fix `buildRangeLoop` so a non-literal end bound is
evaluated exactly once, into its own C local, before the loop starts (the
same treatment the start bound already gets), and the loop condition
compares against that local instead of re-splicing the raw expression text.
Preserve the existing literal-bound fast path (a plain literal end bound
doesn't need a local — re-splicing a decimal number has no evaluation-order
consequence). Next step: dispatch through Orc per the tracker's dispatch
rules.
