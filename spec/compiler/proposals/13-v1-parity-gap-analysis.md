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

### The checker never proves a `u8`/`i8` switch exhaustive, even covering all 256 values

**Source:** proposal 14, "New findings" table, row "V2 does not prove a
complete `u8` or `i8` switch exhaustive."

**Area:** checker, `compiler/internal/check/control_validation.go`,
`switchIsExhaustive` (starts line 29)

**Priority:** batch item 3 of 7 — checker-only, moderate scope (small
finite domain, 256 values), not a backend fix.

**Reproduction:** a `u8` switch covering ALL 256 possible values (not a
partial one — a switch missing values correctly requires a fallback, that
part already works right) still requires an `else`/fallback arm:

```pebble
fn classify(b u8) int {
    switch b {
        case 0: return 0;
        case 1: return 1;
        // ... all 256 cases, 0 through 255 ...
    }
}

fn main() int {
    return classify(1);
}
```

Fails: `error[C0607]: non-void function can fall through without
returning` — even with literally every value covered.

**Root cause, confirmed by reading the code directly:** `switchIsExhaustive`
tracks covered `bool` values (`coveredBools[true]`/`coveredBools[false]`)
and covered enum/tagged-union variants, then proves exhaustiveness for
those two type families by checking full coverage. For integer subjects,
the exact same coverage-collection loop has:

```go
case constantInteger:
    // Integer subjects are not exhaustively enumerable here.
```

— a no-op. No integer value is ever tracked, and there is no follow-up
check for any integer width at all, so an integer-subject switch can never
be proven exhaustive, regardless of how many values it actually covers.

**Scope for this slice:** only `u8` and `i8` (256-value domains, small
enough to enumerate practically) per the tracker's own naming — do NOT
attempt this for wider integer widths (`u16`/`i16` and up have domains too
large to reasonably enumerate case-by-case; that's a different, much
harder problem, out of scope here). Add integer-value tracking to the
coverage loop (a set of covered `int64` values, mirroring `coveredBools`'
shape), then after the loop, if the switch subject's builtin type is `u8`
or `i8`, check whether every value in that width's exact range (`0..255`
for `u8`, `-128..127` for `i8`) is present in the covered set.

**Not yet done:** the actual implementation. Next step: dispatch through
Orc per the tracker's dispatch rules.
