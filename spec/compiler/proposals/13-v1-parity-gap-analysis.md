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

### `sizeof` on a tagged union selects the wrong C type and doesn't compile

**Source:** proposal 14, "New findings" table, row "`sizeof` a tagged union
selects the discriminant enum C type instead of the tagged-union C type."

**Area:** backend generator, `compiler/internal/backend/types.go`,
`sizeofCTypeName` (line 976)

**Priority:** dangerous — same root-cause family as the struct-field/
optional-payload C-type bug already fixed (`4d1ef51`). If a `sizeof`
result on a tagged union ever drove an allocation size, the size would be
wrong (too small — the bare tag enum instead of the full tag+payload
union). In practice it's worse than that: it doesn't even compile.

**Reproduction:**

```pebble
type Choice = union enum {
    empty void;
    value int;
};

fn main() int {
    let s = sizeof Choice;
    print "{}\n", s;
    return 0;
}
```

`go run ./cmd/pebc -run` fails:

```
program.c:8:39: error: use of undeclared identifier 'pebble_enum_23_t'
    8 |     uint64_t pebble_local_30 = sizeof(pebble_enum_23_t);
```

Not just the wrong type — the referenced type isn't even declared anywhere
in the emitted C, because nothing else in this minimal program forces the
union's typedef pair to be collected (the earlier struct-field fix's own
root-cause note applies here too: a tagged union's discriminant enum
typedef is only emitted as part of the union typedef pair, and a bare
`sizeof` expression doesn't trigger that collection path the way a struct
field or optional payload does).

**Root cause, confirmed by reading `sizeofCTypeName` directly:**

```go
if isEnumType(unit, snapshot, id) {
    return enumTypeName(id), nil
}
if isStruct(snapshot, id) {
    return structTypeName(id), nil
}
```

Exactly the same shape as the already-fixed bug: `isEnumType` returns true
for both plain enums AND tagged unions, and this check runs before any
tagged-union-specific branch (there isn't one), so a tagged union always
gets misclassified as a plain enum here too.

**Not yet done:** add an `isTaggedUnionType` check before the `isEnumType`
check in `sizeofCTypeName`, returning `unionTypeName(id)` for that case —
directly mirroring the fix already applied to `structFieldCType`/
`optionalPayloadCType`. Also confirm (may already be handled, verify)
whether `sizeof` on a tagged union needs to force the union typedef pair to
be collected/emitted even when nothing else in the program references it,
the same way a struct field or optional payload already does — otherwise
the fixed `sizeofCTypeName` will correctly choose `unionTypeName` but still
fail to compile if the typedef itself never gets emitted into a bare-sizeof
program. Next step: dispatch through Orc per the tracker's dispatch rules.
