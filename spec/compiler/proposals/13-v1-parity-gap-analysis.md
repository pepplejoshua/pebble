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

### Mutable module-level globals have no backend storage — read or write

**Source:** proposal 14, "New findings" table, row "Mutable globals have no
backend storage."

**Area:** backend generator (whole-program storage scheme is simply
missing); root visible in
`compiler/internal/check/ir_builder.go:475-489`

**Priority:** high — first batch item beyond the two already agreed. This
is an ordinary, common language feature that's flatly absent, not a narrow
edge case; likely to be hit by real programs quickly.

**Reproduction (both read and write independently confirmed broken):**

Read:
```pebble
var counter int = 5;

fn main() int {
    return counter;
}
```
Fails: `pebc: emission failed: entry function body expression references
symbol 24, which is not a local declared earlier in the entry body`

Write:
```pebble
var counter int = 0;

fn bump() int {
    counter = counter + 1;
    return counter;
}

fn main() int {
    bump();
    bump();
    return bump();
}
```
Fails: `pebc: emission failed: entry function body block reassigns symbol
24, which is not a local in scope`

**Root cause:** `let` (immutable) module-level globals already work today,
but via a completely different mechanism that doesn't generalize to `var`:
`ir_builder.go`'s comment explains it directly — a `let` global's VALUE is
recorded (`s.globalLetInitializers[b.Symbol] = b.Initializer`) and inlined
at every reference site as a compile-time constant, so the backend never
needs real storage for it (the value can never change, so every reference
can just rebuild the same literal). A `var` global gets a
`tir.GlobalDeclaration` node and an `AddGlobalDecl` call, but — per the
comment's own words — this deliberately does NOT record an initializer for
`var`, "whose value could change," and nothing downstream ever emits real
C file-scope storage or lets a read/write reference resolve to it. The
backend's symbol resolution only knows about function-local scope maps;
there is no concept of "global storage" anywhere in it yet.

**Scope for this slice:** implement real global storage for `var`
module-level bindings:
1. Emit an actual C file-scope variable declaration for each mutable
   global (e.g. `static int32_t pebble_global_<symbolID> = <initializer>;`
   — confirm the right C type/initializer-value construction using the
   same helpers already used for local variable declarations of the same
   type).
2. Make reads of a global symbol resolve to that C variable wherever a
   `SymbolValue`/reference currently only checks the local-scope map (this
   likely touches several read-path builders, not just one — the checker
   distinguishes local vs. global bindings already, so the backend needs
   the same distinction wherever it currently assumes "not local == error").
3. Make writes (`Store`/`CompoundStore`, whatever this backend calls
   reassignment) resolve to the same C variable for a global place.
4. Confirm whether module-level initialization ordering matters (does a
   global's initializer need to run before `main`, and does this backend
   already have an init-ordering concept for anything else it could
   reuse, or does a simple C static initializer expression suffice for
   every currently-legal global initializer shape?).

**Not yet done:** the actual implementation. Next step: dispatch through
Orc per the tracker's dispatch rules. Given the scope (storage emission +
multiple read/write resolution points), this may need more than one Orc
slice — investigate first if the full scope looks too large for one
dispatch, and split if so.
