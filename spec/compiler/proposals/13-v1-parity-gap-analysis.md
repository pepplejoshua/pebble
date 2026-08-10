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

**Item: composite print slice 1 — struct-of-scalars printing.**

Sourced from proposal 17's design and slice plan (composite printing),
not proposal 14 — proposal 14 listed "composite print from V1 is absent"
with "policy undecided"; the policy is now decided (implement it, quality
bar is real). Proposal 17 has the full design (formatting policy, cycle
safety, storage strategy, generated-C shape). This is slice 1 of 9:
struct-of-scalars only.

**Reproduction** (confirmed against current HEAD):

```
type Point = struct {
    x int;
    y int;
};

fn main() int {
    let p = Point.{ x = 1, y = 2 };
    print p;
    return 0;
}
```

Current failure:

```
print_struct.peb:8:5: error[C0612]: print operand is not printable
      print p;
      ^
```

**Scope for this slice (do NOT exceed):**
1. Checker: widen `valuePrintable`
   (`compiler/internal/check/control_flow_validation.go:111-122`) to
   accept a struct-typed value WHOSE FIELDS ARE ALL currently-printable
   scalar types (bool, char, str, integer, float) — not yet nested
   structs, tuples, arrays, slices, enums, unions, optionals, or
   pointers; those are later slices. A struct with a non-scalar field
   stays rejected for now (later slices will lift this incrementally).
2. Backend: extend `buildPrint`
   (`compiler/internal/backend/statements.go:2511-2770`) with a struct
   branch that emits `TypeName{ field: value, field: value }` using
   direct sequential `fprintf(stdout, ...)` calls (NOT a runtime string
   allocator — proposal 17 explicitly requires no dependency on the
   unfinished Allocator/Context redesign). Field names come from the
   struct's own declared field names in declaration order. Each scalar
   field value reuses the EXISTING scalar print builders exactly (the
   same code paths `buildPrint` already uses for a bare integer/bool/
   char/str/float print) — do not reimplement scalar formatting.
3. The print statement still ends with exactly one trailing newline,
   matching current behavior for scalars.
4. Verify the reproduction above compiles and runs, printing
   `Point{ x: 1, y: 2 }` followed by a newline.
5. Confirm existing scalar `print` statements are completely unaffected.
6. Write tests: checker acceptance for struct-of-scalars, checker
   rejection still holds for a struct with a non-scalar field (e.g. a
   nested struct field — confirm this is EXPECTED to still reject at
   this slice, not a regression), and a backend compile-run test
   asserting the exact printed output.

