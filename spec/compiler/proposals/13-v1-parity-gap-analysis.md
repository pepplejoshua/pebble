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

### A bare `sizeof` of a plain struct or enum leaves its typedef uncollected

**Source:** proposal 14, "New findings" table, the row logged while fixing
the tagged-union `sizeof` bug: "a bare `sizeof` of ANY plain struct or enum
also leaves its typedef uncollected."

**Area:** backend generator, `compiler/internal/backend/collect.go`

**Priority:** low — clean, if unhelpful, `cc` failure; not a correctness
hazard. Last of the three-part `sizeof`-typedef-collection family
(`sizeof` on a tagged union fixed in `f2e8c62`, `sizeof` on a fixed array
fixed in `cacaa28` — both hit this exact same collection-gap shape, just
for their own kind).

**Reproduction:**

```pebble
type Pair = struct {
    x int;
    y int;
};

fn main() int {
    let s = sizeof Pair;
    print "{}\n", s;
    return 0;
}
```

and

```pebble
type Color = enum { red, green, blue };

fn main() int {
    let s = sizeof Color;
    print "{}\n", s;
    return 0;
}
```

Both fail identically — the type SELECTION is already correct
(`pebble_struct_23_t`/`pebble_enum_23_t`, the real types, not a
misclassification like the union bug), but the typedef itself is never
emitted when `sizeof` is the type's only reference in the program:

```
program.c:8:39: error: use of undeclared identifier 'pebble_struct_23_t'
```

```
program.c:8:39: error: use of undeclared identifier 'pebble_enum_23_t'
```

**Root cause:** whatever collection pass registers a struct/enum's typedef
for emission currently only gets triggered by references other than a bare
`sizeof` (field types, parameter/return types, local declarations, etc.).
`collectArrayTypes` (from `cacaa28`) already establishes the exact fix
pattern: it walks the entry body and every reachable helper body collecting
`SizeofType` node type arguments. The struct/enum equivalent needs the same
treatment — find `collectStructTypes`/`collectEnumTypes` (or their `*Walk`
variants) in `collect.go` and add the same `SizeofType`-argument collection
to each, mirroring `collectArrayTypes`/`collectUnionTypesWalk` exactly.

**Not yet done:** implement the collection fix for both struct and enum
`SizeofType` references. Next step: dispatch through Orc per the tracker's
dispatch rules.
