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
- Never let a worker use `git stash` for its own causation checks — it
  shares one global stack with anything else that's ever stashed in this
  repo and can silently pop/lose unrelated WIP. Instruct file-copy
  revert/restore instead (`cp file backup`, `git show HEAD:file > file` to
  revert, `cp backup file` to restore).

## Active defect

### A helper returning a plain enum or tagged union is misclassified as a struct result

**Source:** proposal 14, "New findings" table, row "A helper that returns a
plain enum or tagged union is classified as a struct result."

**Area:** backend generator, `compiler/internal/backend/calls.go`,
`helperSignature`'s return-type switch (~line 371), and
`compiler/internal/backend/emit.go`'s `resultInfo` struct (line 1152) and
whatever builds a tail-position `Return` from it

**Priority:** low — clean rejection, not a correctness hazard. Bigger scope
than the earlier `isStruct`/`isEnumType`-ordering bugs fixed tonight — this
one is a genuinely missing case, not a misordering, and needs a new
`resultInfo` field plus new Return-building logic, not a one-line switch
addition.

**Reproduction:**

```pebble
type Color = enum { red, green, blue };

fn pick() Color {
    return Color.green;
}

fn main() int {
    let c = pick();
    switch c {
        case Color.red: return 0;
        case Color.green: return 1;
        case Color.blue: return 2;
    }
}
```

Fails cleanly at emission, with an error message that literally talks about
struct returns for an enum-returning function:

```
pebc: emission failed: entry function body return statement returns a
EnumVariantValue, want a reference to a struct-typed local in scope, a
struct literal (a RecordConstruct), or a call to a struct-returning helper
(a DirectCall); only returning an already-declared struct-typed local,
constructing a fresh struct literal inline, or forwarding a struct-returning
helper call is supported
```

**Root cause, confirmed by reading the code directly:** `helperSignature`'s
PARAMETER-type switch (line ~184) already has a correct, working
`case isEnumType(unit, snapshot, param.Type):` branch before its
`case isStruct(...)` branch (line ~215) — enum parameters already work.
The RETURN-type switch (starting ~line 371) has NO enum case at all — it
goes straight from `isTuple` to `isStruct` (line 374, which uses
`runtimeTypeName` and sets `result = resultInfo{structType: ...}`) with
nothing for `isEnumType` in between, so an enum-typed return falls through
and gets treated as an unrecognized struct-shaped return, hence the
struct-flavored error message on a return statement that's actually
returning an enum variant.

`resultInfo` (`emit.go:1152`) has fields for every OTHER supported return
shape (`tuple`, `structType`, `sliceType`, `pointerType`, `optionalType`,
`functionType`, `arrayType`, plus `isStr`/`isChar`/scalar `kind`) but no
enum-shaped field at all — this needs to be added, then whatever builds the
tail-position `Return` statement (likely in `buildBlock`, look for how it
switches on `resultInfo`'s populated field to decide how to build the
return value) needs a new branch for it too, mirroring how each other
`resultInfo` shape already has its own Return-building logic.

**Not yet done:**
1. Add an enum-shaped field to `resultInfo` (and a tagged-union-shaped one
   too, if the audit's "or tagged union" half of this finding is also being
   fixed in the same pass — confirm both are in scope, or split into two
   dispatches if that's cleaner).
2. Add the missing `isEnumType`/`isTaggedUnionType` case(s) to
   `helperSignature`'s return-type switch, mirroring the parameter-type
   switch's already-working structure, using `enumTypeName`/`unionTypeName`
   as appropriate (matching tonight's earlier `isTaggedUnionType`-before-
   `isEnumType` ordering fixes).
3. Add the corresponding Return-value-building logic for an enum/union
   result, reusing whatever helper already builds enum/union values
   elsewhere (`buildEnumValue`, `buildUnionValueExpr`) rather than
   inventing new lowering.

Next step: dispatch through Orc per the tracker's dispatch rules.
