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

**Item: a whole dereferenced struct (`*ptr`) cannot become a value (local initializer or call argument).**

Batch item 7, final item of the current batch of 7. Read-side twin of
the struct-reassignment write-side work done earlier tonight (`*self =
other;` already works as an lvalue write; `*ptr` as an rvalue READ does
not).

**Reproduction 1 — local initializer** (confirmed against current HEAD):

```
type Point = struct {
    x int;
    y int;
};

fn use_point(p Point) int {
    return p.x;
}

fn main() int {
    var p = Point.{ x = 5, y = 6 };
    let ptr = &p;
    let q = *ptr;
    return use_point(q);
}
```

Failure:

```
pebc: emission failed: entry function body block declares a
struct-typed local of type pebble_struct_23_t initialized from a Load
whose place is a DereferencePlace, want a CheckedIndexPlace (a by-value
struct-element read)
```

**Reproduction 2 — direct call argument** (also confirmed):

```
fn main() int {
    var p = Point.{ x = 5, y = 6 };
    let ptr = &p;
    return use_point(*ptr);
}
```

Failure:

```
pebc: emission failed: entry function body expression contains a call
to symbol 27 whose argument 0 is a Load, want a reference to a
struct-typed local in scope or a struct literal (a RecordConstruct);
only passing an already-declared struct-typed local or constructing a
fresh struct literal inline is supported
```

**Known cause:** the struct-typed-local-initializer builder and the
struct-typed call-argument builder each only accept a `SymbolValue`
(reference to an already-declared struct local) or a `RecordConstruct`
(struct literal) as the struct value's source. A `Load(DereferencePlace)`
— reading the whole struct through a pointer deref — isn't among the
accepted shapes at either position, even though the underlying C
operation (`*(pebble_struct_..._t *)ptr`, a plain struct-by-value copy)
is exactly as trivially valid as the already-supported shapes.

**Scope for this item:**
1. Extend the struct-typed local-initializer builder to accept a
   `Load(DereferencePlace)` source, emitting the dereferenced pointer
   directly as the whole-struct C value (reusing whatever the existing
   struct-field-read-through-a-pointer machinery already uses for the
   pointer's C dereference expression).
2. Extend the struct-typed call-argument builder (`buildAggregateArgument`
   or wherever the rejection in reproduction 2 originates) the same way.
3. Verify both reproductions above compile and run, returning 5 for each.
4. Confirm the existing supported shapes (a struct-typed local reference,
   a struct literal) are unaffected.
5. Write tests for both reproduction shapes.

