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

**Item: a global's constant initializer that isn't a literal leaf (e.g. `var x int = 1 + 2;`) is rejected — needs compile-time constant folding.**

Batch item 5 (final item) of the current batch of 5.

**Reproduction** (confirmed against current HEAD):

```
var x int = 1 + 2;

fn main() int {
    return x;
}
```

Current failure:

```
pebc: emission failed: global symbol 24 initializer: contains a
CheckedArithmetic, which is not a literal constant; only literal
constant initializers are supported for mutable globals yet (an
arithmetic or cast constant expression is not a C static-initializable
expression)
```

**Known cause:** `CheckedArithmetic` normally lowers to a runtime-checked
overflow-detecting function call, which is not a valid C static
initializer (C requires a compile-time constant expression for file-scope
storage). The backend currently only accepts a bare literal leaf as a
global initializer.

**Decided scope (to keep this narrow and mechanical, matching the rest
of this batch):** implement BACKEND-SIDE constant folding, restricted to
a `CheckedArithmetic` tree whose operands are, recursively, ALL integer
literals (no locals, no calls, no non-literal subexpressions of any
kind). Fold the value in Go at compile time, verify the folded result
fits within the target type's declared range (reproducing the same
overflow check the runtime helper would have performed, just done
statically in the Go compiler code instead), and emit the folded result
as a plain literal C constant. Do NOT attempt general constant-expression
evaluation (no locals, no function calls, no non-integer types) — if the
tree contains anything other than integer literals and integer
arithmetic operators, it stays a clean rejection exactly as before.

**Scope for this item:**
1. Find where the global initializer's TIR node is walked/validated for
   the "not a literal constant" rejection (likely near the
   `emitGlobals`/`buildGlobalStorage` code in
   `compiler/internal/backend/globals.go`, added earlier tonight in
   `14739f3`).
2. Add a recursive Go-side folder: given a `CheckedArithmetic` node,
   recursively evaluate it if every leaf is an `IntegerLiteral` and every
   internal node is a supported arithmetic operator (+, -, *, /, % at
   minimum — matching whatever operators `CheckedArithmetic` already
   supports). Compute using Go's own arbitrary-precision or checked
   integer arithmetic, verify the result fits the target width/signedness
   (overflow → clean rejection with a clear message, not a silent wrap),
   and emit the folded literal.
3. Verify the reproduction above compiles and runs, returning 3.
4. Verify an overflow case is still cleanly rejected (e.g. `var x u8 =
   250 + 10;` — folds to 260, out of u8 range) with a clear message, not
   silently wrapped or a Go panic.
5. Verify a non-foldable initializer (e.g. involving a function call or a
   non-literal operand) still gets the exact original rejection message,
   unaffected.
6. Write tests for: successful folding (a couple of operators), overflow
   rejection, and non-foldable rejection unaffected.

