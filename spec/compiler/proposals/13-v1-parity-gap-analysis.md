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

**Item: inline slice construction fails in pure nested expression positions — decided to fix via GNU statement-expressions.**

Batch item 4 of the current batch of 7.

**Reproduction** (confirmed against current HEAD):

```
fn sum(s []int) int {
    var total = 0;
    loop 0..s.len : i {
        total = total + s[i];
    }
    return total;
}

fn wrap(s []int) int {
    return sum(s);
}

fn main() int {
    var arr [3]int = [1, 2, 3];
    return wrap(arr[:]);
}
```

Current failure:

```
pebc: emission failed: entry function body expression contains a call
to symbol 26 whose argument is an inline slice construction (a
CheckedSlice), which is not supported in this expression position: a C
function argument is a pure expression position with nowhere to place
the temp-declaration statement the slice construction needs; bind the
slice into a local first
```

**Known cause:** `buildSliceArgument`'s doc comment (`calls.go`, near
line 1520-1534) explicitly documents this as a deliberate prior
decision: an inline `CheckedSlice` construction needs a two-statement
lowering (a temp declaration, then a compound literal referencing it),
which has nowhere to go when the enclosing call itself is nested inside
another pure expression (e.g. `wrap(arr[:])` where `wrap`'s call is
itself the argument to `sum`, or more directly: `arr[:]` used as an
argument to a call that is ITSELF used in another expression position,
not a bare statement or a local's initializer). The doc comment states
this backend "does not reach for a GNU statement-expression or any
other workaround."

**Decision (direct instruction, 2026-08-09):** REVERSE that prior
decision. Use GNU statement-expressions (`({ ...; value; })`), a
widely-supported GCC/Clang extension this project's toolchain already
targets via `cc`, to let a slice-construction temp declaration live
inside an arbitrary nested expression position. This is a real language
extension available at compile time, not a hack — pick it over
`.docstyle` alternatives.

**Scope for this item:**
1. Find every place where an inline `CheckedSlice` argument is currently
   rejected specifically because of "nowhere to place the temp-
   declaration statement" (start from `buildSliceArgument`'s doc comment
   and the two-statement lowering it references), and replace that
   rejection with a GNU statement-expression: `({ <temp decl>; <compound
   literal value>; })` used directly as the expression text in the
   nested position.
2. Keep the existing non-nested paths (a bare call statement, a local's
   declaration initializer) working exactly as before — only the
   previously-rejected NESTED case needs the new statement-expression
   lowering; don't rewrite the already-working simple case unless truly
   necessary for consistency.
3. Verify the reproduction above compiles and runs correctly (`wrap(arr[:])`
   nested inside `main`'s return, itself calling `sum` with the slice —
   returns 6).
4. Confirm the existing non-nested slice-construction-as-argument case
   (a direct `f(a[1:3])` call statement or initializer) is unaffected.
5. Write tests for both the nested case and confirmation the simple case
   still works.

