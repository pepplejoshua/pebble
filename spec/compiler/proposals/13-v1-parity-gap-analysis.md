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

**Item: composite print slice 5 — plain enums.**

Sourced from proposal 17, slice 5 of 9. Slices 1-4 landed
(`c182e73`/`5e6e786`/`b80fbc4`/`21e54ec`).

**Reproduction** (confirmed against current HEAD):

```
type Color = enum { red, green, blue };
fn main() int {
    let c = Color.green;
    print c;
    return 0;
}
```

Fails with `error[C0612]: print operand is not printable`.

**Scope for this slice:** per proposal 17 — format `Color.red` (declared
type name, `.`, declared variant name). For an invalid discriminant
value (defensive, not normally reachable but proposal 17 calls for it),
print `Color<invalid: N>` rather than reading an arbitrary/garbage name
table entry.
- Checker: `printableType` gains a plain-enum case (recursive not
  needed — an enum has no nested fields — but must still route through
  the shared function).
- Backend: a new enum print builder, generating a switch (or equivalent)
  over the enum's `.tag`/discriminant that emits the matching variant's
  declared source name as a static string per case, with a defensive
  default/else case producing `Color<invalid: N>`. Reuse whatever
  existing enum-tag/variant-name machinery this backend already has
  (e.g. how a plain enum is already compared/switched on elsewhere) —
  do not invent new enum representation knowledge.
- This operand also needs to route through the composite dispatch
  (`buildSequentialPrint`'s composite recognition) even though an enum
  has no nested fields to recurse into — it's still not a `printf`-
  foldable scalar, since it needs a runtime tag comparison, not a
  static format specifier.
- Verify the reproduction prints exactly `Color.green`.
- Confirm slices 1-4 and scalar prints are unaffected.
- Write tests: checker acceptance for a printable enum value, and a
  backend compile-run test for at least two different variants (proving
  the tag-to-name mapping is correct, not just the first variant),
  asserting exact printed output.

