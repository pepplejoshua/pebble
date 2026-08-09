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
  multi-layer task. Escalate only after a real capability failure — and
  check `orc list` for the user's own concurrent Luna usage before ever
  escalating to Luna.
- Before each dispatch, require a clean worktree and no active Orc or
  OpenCode worker for this repository.
- After each dispatch, inspect the diff and check for scratch files, debug
  output, scope growth, and stale tests. Run the full required verification
  and a causation check before commit and push.
- Delete failed/stalled/killed Orc sessions with `orc delete` immediately,
  not just their scratch files.

## Active defect

### A tagged union nested in a struct field or optional payload gets the wrong C type

**Source:** proposal 14, "New findings" table, row "A tagged union used as a
struct field or optional payload receives the plain-enum C type name," and
the "Backend C-shape capability matrix" section.

**Area:** backend generator (`compiler/internal/backend/emit.go`)

**Reproduced tonight, two distinct failure shapes, one shared root cause.**

**Case 1 — tagged union as a struct field.** Reproduction:

```pebble
type Choice = union enum {
    empty void;
    value int;
};

type Holder = struct {
    tag Choice;
};

fn main() int {
    let c = Choice.value(42);
    var h = Holder.{ tag = c };
    return 0;
}
```

`go run ./cmd/pebc -run` fails with a raw `cc` compile error, not a clean
Pebble diagnostic:

```
program.c:7:5: error: unknown type name 'pebble_enum_23_t'
    7 |     pebble_enum_23_t pebble_field_28;
```

Two bugs stacked here:

1. **Typedef ordering bug (the one that's actually hit first):** the emitted
   C declares `pebble_struct_24_t` (which references `pebble_enum_23_t` as a
   field type) BEFORE `pebble_enum_23_t`'s own `typedef enum {...}` appears
   later in the same file. This alone is a hard `cc` failure regardless of
   the type-choice bug below.
2. **Wrong field type (the audit's actual prediction), confirmed underneath
   bug 1** by manually reordering the emitted C so it compiles far enough to
   reach it:
   ```
   error: initializing 'pebble_enum_23_t' with an expression of
   incompatible type 'pebble_union_23_t'
   ```
   The `Holder.tag` field is declared as `pebble_enum_23_t` (the bare 4-byte
   tag-only C enum, no payload storage) but the value actually assigned to
   it is `pebble_union_23_t` (the real struct-with-payload-union type,
   larger). **With the project's own strict compile flags
   (`-std=c11 -Wall -Wextra -Werror`, confirmed these are what `pebc -run`
   always uses), `cc` rejects this as a hard error — it is NOT silent
   runtime corruption in practice, contrary to my initial worry when this
   finding was first reported. It fails loud, just very late (a raw `cc`
   error instead of a clean Pebble diagnostic), and only after bug 1's
   ordering issue is somehow worked around.**

**Case 2 — tagged union as an optional payload.** Reproduction:

```pebble
type Choice = union enum {
    empty void;
    value int;
};

fn main() int {
    let c = Choice.value(42);
    var o ?Choice = some c;
    return 0;
}
```

Fails with a clean Go-level diagnostic (not a raw `cc` failure, unlike case
1):

```
pebc: emission failed: entry function body block declares an optional-typed
local of type pebble_optional_24_t initialized from some with an enum
payload pebble_enum_23_t; the only supported enum-payload optional
initializer is an integer-to-optional-enum cast (e.g. 5 as ?Color)
```

Same root cause as case 1, different symptom shape: the optional-payload
path also misclassifies the tagged union's payload as `pebble_enum_23_t`
(the bare tag enum) instead of the real union type, but this path happens to
hit its own clean rejection before ever generating broken C.

**Root cause (per proposal 14's own analysis, confirmed by the above):**
`isStruct` treats every nominal type as a struct; `isEnumType` treats both
plain enums AND tagged unions as enum-shaped. `structFieldCType` and
`optionalPayloadCType` then both select `enumTypeName` for anything
enum-shaped, without distinguishing "plain enum" from "tagged union, which
actually needs `unionTypeName`."

**Also found, distinct, unrelated bug — noted, not chased in this item:**
tagged-union switch narrowing works when the switch subject is a function
*parameter* (`fn pick(c Choice) int { switch c { ... } }` — confirmed
working) but fails with `error[C0605]: member operation is invalid` when
the switch subject is an ordinary `let`-bound local in the same function
(`let c = Choice.value(42); switch c { case .value: return c.value; }`).
This is NOT the struct-field/optional bug above — it's a narrowing gap for
the "own-scope local" position specifically. Flagging for a future item,
not working it now.

**Not yet done:** identify the exact ordering bug in the struct-typedef
emission pass (why `pebble_enum_23_t`'s typedef is emitted after
`pebble_struct_24_t` instead of before), and the exact fix for
`structFieldCType`/`optionalPayloadCType` to select `unionTypeName` for a
tagged union instead of `enumTypeName`. Next step: dispatch investigation
through Orc per the tracker's dispatch rules.
