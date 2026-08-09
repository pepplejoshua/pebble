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

### An enum/tagged-union-returning call cannot be used directly in a general value position

**Source:** proposal 14, "New findings" table, row logged while fixing the
enum/union helper-return bug (`4475579`): "an enum/tagged-union-returning
`DirectCall` used directly in a general value position ... still cleanly
rejects."

**Area:** backend generator, `compiler/internal/backend/values.go`,
`buildEnumValue` (line 418) and `buildUnionValueExpr` (line 588)

**Priority:** low — clean rejection, not a correctness hazard. Direct
follow-on to `4475579`, which fixed enum/union helper returns for the tail
`return` and local-declaration-initializer positions but explicitly left
`values.go` untouched.

**Reproduction:**

```pebble
type Color = enum { red, green, blue };

fn pick() Color {
    return Color.green;
}

fn main() int {
    switch pick() {
        case Color.red: return 0;
        case Color.green: return 1;
        case Color.blue: return 2;
    }
}
```

Fails cleanly:

```
pebc: emission failed: entry function body expression contains a
DirectCall, want an enum variant literal (an EnumVariantValue) or a
reference to an enum-typed local
```

The checker accepts calling an enum-returning function directly as a switch
subject (or presumably any other general enum/union-value position — a
comparison operand, a field-construction value, etc.); the backend's value
builders for enum/union only accept a literal or a local reference, not a
call.

**Root cause:** `buildEnumValue` and `buildUnionValueExpr` (both in
`values.go`) switch on the value-producing node kind and have no
`tir.DirectCall` case. This is the same "value-source position matrix"
shape the audit already documents broadly (many builders only accept a
narrow set of source-node shapes per destination position) — this is one
specific instance of it, now unblocked because the return/initializer sides
of the same feature are fixed.

**Not yet done:** add a `DirectCall` case to both `buildEnumValue` and
`buildUnionValueExpr`, routing through the same call-building machinery
other value positions already use for forwarding a call result (look at how
existing positions handle a call that might need a leading statement —
`buildDirectCall`/`buildDirectCallWithPre` — since a helper call used
inline in an expression may need its own pre-statement, matching the
pattern tracker item 6, "inline slice construction in pure expression
positions," already describes for a different value shape). Next step:
dispatch through Orc per the tracker's dispatch rules.
