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

**Item: `uint` (word-sized unsigned) is still not accepted as a switch subject at the backend.**

Batch item 2 of the current batch of 7. Narrower follow-on to `2b3d684`
(fixed-width integer switch subjects), which deliberately excluded
`uint`.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let x uint = 5;
    switch x {
        case 5: return 1;
        else: return 0;
    }
}
```

Current failure:

```
pebc: emission failed: switch subject has type uint, want int, bool,
or char, or an enum/tagged-union type
```

**Known cause / prior investigation:** `2b3d684`'s new switch-subject
branch used `resolvedBuiltin`/`cType` and deliberately excluded `uint`
via `!isUint(...)`, since `uint` is the abstract word-sized builtin (like
`int`), not one of the fixed 8/16/32/64-bit widths. However, `uint`
VALUES are already readable elsewhere in this backend via the existing
`buildUintExpr` builder (used for globals, locals, etc.) — the same
builder `isUint(snapshot, param.Type)` already routes to in
`helperSignature`. This suggests the fix is a dedicated `isUint(...)`
branch in `buildSwitchStatement` calling `buildUintExpr`, mirroring how
`isBool`/`isChar` each get their own dedicated branch — not a deep
width-resolution rework.

**Scope for this item:**
1. Add a dedicated `uint` branch to `buildSwitchStatement`'s
   subject-type dispatch chain (before or after the existing fixed-width
   branch, matching the isBool/isChar pattern), calling `buildUintExpr`
   for the subject expression.
2. Case labels for a `uint` subject must be emitted at the `uint` width
   (`u64`/whatever `uint`'s cType convention is — check `cType(types.Uint)`)
   so they carry the correct C suffix/type.
3. Verify the reproduction above compiles and runs, returning 1.
4. Confirm the fixed-width-integer and entry-width `int` switch subjects
   fixed in `2b3d684` are unaffected.
5. Write a compile-run test for a `uint` switch subject.

