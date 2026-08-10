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

**Item: composite print slice 6 — tagged unions.**

Sourced from proposal 17, slice 6 of 9. Slices 1-5 landed
(`c182e73`/`5e6e786`/`b80fbc4`/`21e54ec`/`c1bf23b`).

**Reproduction** (confirmed against current HEAD):

```
type Result = union enum { ok int; error str; };
fn main() int {
    let r = Result.ok(42);
    print r;
    return 0;
}
```

Fails with `error[C0612]: print operand is not printable`.

**Scope for this slice:** per proposal 17 —
- Format: `Result.ok(42)`, `Result.error("failed")` — declared type name,
  `.`, declared variant name, then `(`, the payload's own printed value
  (recursively formatted via `buildPrintValueCalls`, same as any nested
  field — the payload could itself be a struct/tuple/array/enum, anything
  currently printable), `)`.
- A payload-less variant prints without parens: `Result.done` (no `()`).
- Payload printability: a union whose ACTIVE variant's payload type is
  not printable should still be rejected by the checker for values of
  that declared type overall (same conservative approach slice 1 used
  for struct fields — a union is printable only if EVERY variant's
  payload type is printable, since the checker cannot know at
  compile-time which variant will be active at runtime).
- Invalid tag defensive case: `Result<invalid-tag: N>`, mirroring the
  plain-enum invalid-discriminant case from slice 5.
- Backend: this is a runtime switch on the union's `.tag`, similar in
  shape to slice 5's enum switch but each case ALSO recurses into the
  payload (reading `.payload.pebble_field_<variant>`) when the variant
  has one, and closes the parens. Reuse the same `raw` printFprintfCall
  mechanism slices 4-5 established.
- Verify the reproduction prints exactly `Result.ok(42)`.
- Also verify a payload-less variant (e.g. `union enum { a void; b int;
  }`, printing `.a`) and a second payload variant (e.g. `Result.error("failed")`)
  to prove the tag-to-variant mapping and payload-vs-no-payload
  formatting are both correct.
- Confirm slices 1-5 and scalar prints are unaffected.
- Write tests: checker acceptance for a printable union value, checker
  rejection for a union with a non-printable payload type in any
  variant, and backend compile-run tests for a payload variant, a
  payload-less variant, and a second payload variant proving the
  tag-to-name mapping, asserting exact printed output.

