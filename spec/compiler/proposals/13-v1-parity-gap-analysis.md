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

### `main(argv []str)` cannot receive C process arguments

**Source:** proposal 14's "Confirmed open tracker items" list, item 5.

**Area:** backend generator, `compiler/internal/backend/validate.go`
(`validateEntrySignature`, line 24) and the entry-point C bridge (search
`emit.go` for `emitEntryC` and the literal `int main(int argc, const char
**argv)` text)

**Priority:** low — no current example depends on it, but it's a real,
self-contained feature, not a small fix.

**Reproduction:**

```pebble
fn main(argv []str) int {
    return argv.len;
}
```

`go run ./cmd/pebc -o` fails cleanly:

```
pebc: emission failed: entry function has 1 parameter(s), want 0
(main([]str) and main(i32, []str) are not supported yet)
```

**Root cause:** `validateEntrySignature` unconditionally rejects any
nonzero parameter count — the message even names the exact unsupported
shape. Separately, the emitted C entry bridge already generates a real
`int main(int argc, const char **argv)` (confirmed by inspecting other
emitted programs tonight), but immediately discards both with `(void)argc;
(void)argv;` before calling `pebble_user_main(&ctx)` with no arguments.

**Scope for a first slice** (matches this item's own historical slice
plan, not yet executed): accept exactly the one-parameter
`main(argv []str) int` form (not the two-parameter `main(argc, argv)`
form — that stays intentionally unsupported, a documented V1-parity
decision, do not implement it). At the C entry point, build a `[]str`
value (this backend's `PebbleStr`-slice shape) from the real `argc`/`argv`
and pass it as `pebble_user_main`'s argument. Open question to resolve
during implementation, not assumed here: does the `[]str` include
`argv[0]` (the program name, that being the raw C convention) or start
from `argv[1]` (excluding the program name, matching most modern
language runtimes' convention)? Neither is asserted correct here — decide
based on whatever's more consistent with this project's other conventions,
or default to excluding the program name (`argv[1..argc]`) if nothing else
dictates otherwise, and document the choice clearly.

**Not yet done:** the actual implementation — checker/typed-IR-side
carrying of the parameter is claimed already accepted by the checker
(confirm this still holds), and the backend-side changes above. Next step:
dispatch through Orc per the tracker's dispatch rules.
