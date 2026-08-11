# 13 — v1 parity gaps

**Purpose.** This file is the working area for exactly ONE gap at a time,
sourced from `14-v2-v1-checker-backend-parity-audit.md`'s master findings
list. It is not a backlog. Proposal 14 is the backlog and the completion
record; this file is the scratch pad for whichever single item is currently
being reproduced, worked, and closed.

## Workflow

1. Pick the next unaddressed item from proposal 14 (its fourth-pass current
   gap table and implementation slicing notes).
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
  multi-layer task. `opencode-go/mimo-v2.5` is banned. If flash genuinely
  stalls or fails, check `orc list` for the user's own concurrent Luna usage,
  then escalate to `openai/gpt-5.6-luna`. When escalating a stuck session,
  prefer resuming the same session with the new `--model` over deleting and
  dispatching fresh.
- Before each dispatch, require a clean worktree and no active Orc or
  OpenCode worker for this repository.
- After each dispatch, inspect the diff and check for scratch files, debug
  output, scope growth, and stale tests. Run the full required verification
  and a causation check before commit and push.
- Delete failed/stalled/killed Orc sessions with `orc delete` immediately,
  not just their scratch files.

## Active defect

**Item: `sizeof [N]Struct` emits the array typedef before its struct
element typedef exists — actually a missing-collection gap, not an
ordering gap.**

Sourced from proposal 14's backend gap matrix (`sizeof [N]Struct`,
line 75), P1. Independently reproduced and root-caused before
dispatch.

**Reproduction** (confirmed against current HEAD):

```
type Point = struct { x int; y int; };
fn main() int {
    return (sizeof [2]Point) as int;
}
```

`go run ./cmd/pebc -run <file.peb>` fails at the C COMPILER stage (not
Pebble's own emission): `cc compilation failed: ... error: unknown
type name 'pebble_struct_19_t'` — the emitted C for the array typedef
(`pebble_array_24_t { pebble_struct_19_t data[2]; }`) references
`pebble_struct_19_t`, but that struct typedef never appears anywhere
in the output. Point is otherwise unreferenced (no construction, no
field access, no local) — its ONLY reference in the whole program is
as the array's element type inside `sizeof [2]Point`.

**Root cause — this is NOT an ordering bug, it's a missing-collection
bug.** `collectArrayTypesWalk` (`compiler/internal/backend/collect.go:180-192`)
already has a `case tir.SizeofType && isArray(snapshot, node.TypeArg)`
that correctly collects the ARRAY type itself (`pebble_array_24_t`) —
this is the fix from the already-resolved item at tracker 14 line 642
(`cacaa28`). But `collectStructTypesWalk`
(`compiler/internal/backend/collect.go:956-974`) only checks
`isStruct(snapshot, node.TypeArg)` — i.e. whether the `SizeofType`
node's OWN TypeArg is directly a struct. For `sizeof [2]Point`,
`node.TypeArg` is the ARRAY type `[2]Point`, not `Point` itself, so
this check never fires and the struct element is never collected. The
struct typedef is simply never emitted at all — not emitted-late,
never-emitted — so `orderAggregateTypes`/the typedef-ordering pass
never even sees it as something to place before the array typedef.

**Scope:** add a case to `collectStructTypesWalk` (or a small
dedicated helper called from it) that recognizes a `SizeofType` node
whose `TypeArg` is an ARRAY type, resolves the array's element type
via `snapshot.Key(node.TypeArg).Array()` (the standard `(length,
elementType, ok)` pattern used throughout this file — e.g.
`locals.go:180`, `calls.go:1878`), and — if that element type is a
struct (guarded the same way the existing bare-struct `SizeofType`
case is: `runtimeType(...) == 0 && !isEnumType(...)`) — collects the
ELEMENT type, not just the array type. Verify the reproduction above
compiles and runs, returning 16 (2 × 8-byte `Point`, confirm the exact
value against the actual emitted struct layout, don't assume). Verify
the existing bare `sizeof Struct` and bare `sizeof [N]T` (a
primitive-element array, already working) cases are unaffected.
**Also check, but only fix if trivial and doesn't expand scope
significantly:** whether the same gap exists for an array whose
element is a TUPLE, OPTIONAL, or ENUM type under `sizeof [N]T` (e.g.
`sizeof [2](int,int)`) — if it's the identical shape of gap, note it
in the report; if fixing it requires meaningfully more work than the
struct case, leave it out and document it as a separate follow-up
rather than silently expanding this task.

<!-- Previous item, resolved 2026-08-11:

**Item: a direct cast of a `sizeof` expression is rejected.**

Sourced from proposal 14's backend gap matrix (`Direct cast of
sizeof`, line 74/330), P1. Independently reproduced and root-caused
before dispatch. Small, narrow fix — one missing switch case.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    return (sizeof int) as int;
}
```

`go run ./cmd/pebc -run <file.peb>` fails with: `entry function body
integer cast child: entry function body expression contains a
SizeofType, want an integer literal, a reference to a local declared
earlier in the body, checked +, -, *, /, % arithmetic, bitwise &, |,
^, ~, or a call to another function`.

**Root cause.** `buildExpr` (`compiler/internal/backend/values.go:1547-2220`,
the general expression dispatcher `IntegerCast`'s child builder calls)
has NO `case tir.SizeofType:` in its switch — every other integer
cast child shape is handled, but a bare `sizeof T` used as the cast's
operand falls through to the default rejection. This exact shape is
ALREADY supported in the sibling `buildUintExpr`
(`compiler/internal/backend/values.go:123-190`, its own
`case tir.SizeofType:` at line 137), since `sizeof T` is itself a
`uint`-typed expression — `buildUintExpr` resolves the C type name via
`sizeofCTypeName` and emits `sizeof(<type>)`, and that helper's logic
does not depend on the `width` parameter at all (only `node.TypeArg`).

**Scope:** add a `case tir.SizeofType:` to `buildExpr`'s switch that
delegates directly to `buildUintExpr(st, unit, snapshot, fileSet, id,
locals, width)` (the same node, same width — `buildUintExpr`'s
`SizeofType` branch ignores `width` entirely, so no width mismatch is
possible). Verify: the reproduction above compiles and runs (`sizeof
int` is 4, so the program returns 4 on this platform — confirm the
actual value the runtime harness expects, don't assume 4 blindly, it
may differ from other prior sizeof-related fixture expectations).
Verify a `sizeof` of a wider type (e.g. `sizeof i64 as int`) and a
`sizeof` of a struct type cast to an integer also work, mirroring what
`buildUintExpr`'s existing `SizeofType` case already supports for a
plain (uncast) `sizeof` expression. Verify the existing plain
`sizeof T` (uncast, used directly as a `uint`-typed value) is
unaffected.

**Resolution (`634db99`, 2026-08-11).** Added a `case tir.SizeofType:`
to `buildExpr`'s switch, delegating to `buildUintExpr` exactly as
scoped. Verified: the reproduction returns 4; `(sizeof i64) as int`
returns 8; `(sizeof Pair) as int` (a two-`int`-field struct) returns
8; the existing plain, uncast `sizeof` path is unaffected. Full suite
(`go test ./... -count=1 -timeout 600s -parallel 16`, 11 packages)
clean, `gofmt`/`go vet` clean, causation check confirmed reverting
reproduces the original rejection exactly.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: an existing slice cannot be passed directly as the sole tail
argument to a variadic slice parameter.**

Sourced from proposal 14's backend gap matrix (`One existing slice as
the sole variadic tail`, line 296; `Existing slice as one variadic
tail`, line 73), P1. Independently reproduced and root-caused before
dispatch. Broader/riskier than the recent backend-only fixes (#40-49)
— touches the CHECKER's constraint-based inference, not just the
backend — so scope this one carefully.

**Reproduction** (confirmed against current HEAD):

```
fn sum(...values []int) int {
    var total int = 0;
    var i uint = 0;
    while i < values.len {
        total = total + values[i];
        i = i + 1;
    }
    return total;
}
fn main() int {
    var arr [3]int = [1, 2, 3];
    var s []int = arr[0:3];
    return sum(s);
}
```

`go run ./cmd/pebc -run <file.peb>` fails at the CHECKER level (not
emission): `error[C0601]: cannot convert value for argument 1`, since
`s`'s type (`[]int`) doesn't unify against the per-element expectation
(`int`) the checker builds for every variadic-tail argument.

**Root cause.** `prepareDirect`'s variadic handling
(`compiler/internal/check/call_facts.go`, the `if signature.Variadic`
block starting around line 285) unconditionally treats EVERY argument
from `p.target.FixedCount` onward as one ELEMENT of the variadic
slice: it calls `w.variadicElement` to resolve the slice parameter's
element type, then builds one destination typed as that element for
each tail argument, with no special case for "the caller passed
exactly one argument whose OWN type is already the parameter's whole
slice type." V1's reference semantics (`src/codegen.c:4020-4022`:
`if (arg_count == fixed_params + 1 && variadic_type->kind ==
TYPE_SLICE) { write_expression(exprs[fixed_params]); }`) confirm this
is a real, intentional V1 behavior — not just a convenience the V1
codegen invented — the emitted C forwards the single slice argument
directly instead of collecting it into a synthesized array-backed
slice. **This means V1's own semantic/type-checking layer (find and
read the equivalent pre-codegen file in `src/`, e.g. wherever call
argument type-checking happens, before assuming the exact rule) must
already special-case arg-count == 1 against the parameter's own slice
type — confirm the precise V1 rule by reading that code, don't just
infer it from the codegen snippet.**

Even if the checker is fixed to accept this shape, the BACKEND also
needs a matching change: `buildVariadicSliceArgument`
(`compiler/internal/backend/calls.go:1448-1489`) unconditionally
builds a `(sliceType){ .data = (elemType[]){ ... }, .len = N }`
compound literal from the collected per-element argument expressions
— it has no path for "the single tail argument is already a
slice value of the right type, forward it directly" (V1's own
`arg_count == fixed_params + 1` shortcut). Both layers need the fix
for the reproduction to actually work end-to-end.

**Scope — investigate before implementing, this is not a
guaranteed-safe backend-only change:**

1. Read `prepareDirect`'s variadic block and `variadicElement` in full
   to understand exactly how per-tail-argument destinations are
   created and how the constraint solver (`w.addConstraint`,
   `infer.Equal`) resolves an argument's type against a destination.
2. Determine whether there's an existing, cheap way to check "this
   specific call has exactly one tail argument, and that argument's
   syntax node already carries a statically-known type equal to the
   variadic parameter's slice type" BEFORE constraints are added —
   look for how other parts of the checker peek an already-resolved
   type for a simple reference (a local variable's declared type is
   often known immediately, unlike a general inferred expression).
   If no such cheap peek exists and the only way to decide is to run
   full inference twice (once against the slice type, once against
   the element type, picking whichever unifies) — STOP and report
   this precisely rather than forcing something architecturally
   awkward into the constraint solver; this may need a design
   decision rather than a quick fix.
3. If a workable approach exists: scope it EXACTLY to "arg count in
   the variadic tail is exactly 1." Do NOT change behavior for zero
   tail arguments, multiple tail arguments, or a single tail argument
   that is genuinely meant as one element (e.g. `sum(5)` — a bare
   `int` literal in a single-element variadic call — MUST continue to
   work exactly as today; verify this explicitly as a regression
   test).
4. Once the checker accepts the reproduction, add the matching
   backend case to `buildVariadicSliceArgument`: when there is exactly
   one variadic argument AND its resolved type is exactly the slice
   parameter's type (not the element type), forward it directly as
   the argument expression instead of building the collected-array
   compound literal (mirroring what `buildSliceArgument`
   `calls.go:1657+` already does for a plain slice-typed argument in
   a non-variadic position — likely reusable logic, not a fresh
   pattern).
5. Verify the reproduction compiles and runs, returning 6. Verify
   `sum(5)` (a single literal element, not a slice) is unaffected.
   Verify a multi-element variadic call (`sum(1, 2, 3)`) is unaffected.
   Verify a zero-argument variadic call is unaffected. Verify a fixed
   parameter alongside a variadic slice tail (both the multi-element
   and sole-slice-tail shapes) still works.

**Resolution (`94e74f0`, 2026-08-11).** Confirmed the V1 rule exactly
by reading `src/checker.c:3204-3237`: V1 synthesizes a sole tail
argument's type bottom-up first, then branches (forward the slice if
the synthesized type is itself a slice; otherwise bind as one
element). Implemented as a `knownReferenceType` peek in `prepareDirect`
(`compiler/internal/check/call_facts.go`): when the tail has exactly
one argument that is a simple reference (a Name/Path resolving to an
annotated binding or parameter) whose statically-known declared type
equals the variadic parameter's whole slice type, the destination is
the slice type instead of the element type — bottom-up and
expectation-free, matching V1's synthesize-then-branch order, not a
"try both and see which unifies" hack. `buildVariadicSliceArgument`
(`compiler/internal/backend/calls.go`) got the matching case,
delegating to the existing `buildSliceArgument` to forward the value
directly. A slice-typed struct field as the sole tail argument
(`sum(h.values)`) stays rejected — its type isn't statically known to
the walker at `prepareDirect` time (a solver-resolved constraint, not
an annotated binding/parameter) — a deliberate, narrower scope
boundary. Verified: the reproduction, a bare literal (`sum(5)`), a
multi-element call, a zero-argument call, a fixed-parameter-plus-tail
call, and the GENERIC variadic path (which already forwarded at the
checker level via a different mechanism) all behave correctly; full
suite (`go test ./... -count=1 -timeout 600s -parallel 16`, 11
packages) clean, `gofmt`/`go vet` clean, causation check independently
bisected checker-only and backend-only reverts to confirm both layers
are individually necessary.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: a slice-typed struct field cannot be passed directly as a
call argument.**

Sourced from proposal 14's backend gap matrix (`Slice field as call
argument`, line 72), P1. Independently reproduced and root-caused
before dispatch.

**Reproduction** (confirmed against current HEAD):

```
type Holder = struct { values []int; };
fn sum(v []int) int {
    var total int = 0;
    var i uint = 0;
    while i < v.len {
        total = total + v[i];
        i = i + 1;
    }
    return total;
}
fn main() int {
    var arr [3]int = [1, 2, 3];
    var h Holder = Holder.{ values = arr[0:3] };
    return sum(h.values);
}
```

`go run ./cmd/pebc -run <file.peb>` fails with: `entry function body
expression contains a call to symbol 26 whose argument 0 is a Load,
want a reference to a slice-typed local in scope; only passing an
already-declared slice-typed local is supported`.

**Root cause.** `buildSliceArgument`
(`compiler/internal/backend/calls.go:1657-1687`) only handles
`tir.CheckedSlice` (an inline slice construction) and `tir.SymbolValue`
(an in-scope slice local) as a call argument's node kind; a
`tir.Load` reading a slice-typed struct field (`h.values`, lowered by
the checker to `Load(FieldPlace)`) is rejected outright. This exact
shape is ALREADY supported for a slice-typed LOCAL's declaration
initializer: `buildSliceLocalDeclaration`
(`compiler/internal/backend/locals.go:465-489`) has a `tir.Load` case
that calls `buildPlaceLValue` to build the field-projection lvalue
expression directly (`pebble_local_<sym>.pebble_field_<member>`),
double-checks the resolved element type, and emits a straight
whole-struct-copy declaration. This is the same shape of gap as the
local-copy-initialization family (tasks #40-45) and the array-literal
return gap (task #48): a value shape the backend already knows how to
build in one context (a local declaration) isn't yet wired into a
sibling context (a call argument).

**Scope:** add a `tir.Load` case to `buildSliceArgument`, reusing
`buildPlaceLValue` exactly as `buildSliceLocalDeclaration` already
does, to build the field-projection C expression and pass it directly
as the argument (no local temp needed — `buildPlaceLValue`'s output is
itself a valid C lvalue expression usable inline as a call argument).
Verify: the reproduction above compiles and runs, returning 6; the
existing `CheckedSlice` and `SymbolValue` argument paths are
unaffected; if practical, verify a nested field read (a slice field
one level deeper than the reproduction, e.g. `outer.inner.values`) and
a slice field passed to a variadic call's sole tail argument (if that
shape reaches `buildSliceArgument` rather than
`buildVariadicSliceArgument` — check which function actually handles
it and note the answer in the report either way).

**Resolution (`d33060e`, 2026-08-11).** `buildSliceArgument` gained a
`tir.Load` case reusing `buildPlaceLValue`, exactly as scoped. Verified:
the reproduction compiles and runs (6); a nested field read
(`o.inner.values`) also works; the emitted C for the reproduction was
inspected directly and confirmed the field projection
(`pebble_local_<h>.pebble_field_<values>`) is passed straight to the
callee with no temp declaration and no GNU statement-expression. A
slice field as a variadic call's sole tail argument turned out to be
rejected by the CHECKER (`C0601`) before reaching either
slice-argument builder — a separate, checker-level shape question, not
a backend gap, correctly left out of scope. Full suite
(`go test ./... -count=1 -timeout 600s -parallel 16`, 11 packages)
clean, `gofmt`/`go vet` clean, causation check confirmed reverting
reproduces the original rejection exactly.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: a fixed-array-returning function cannot return an array
literal or repeat expression directly.**

Sourced from proposal 14's backend gap matrix (`Fixed-array literal
returned directly`, line 329), P1. Independently reproduced and
root-caused before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn make() [3]int {
    return [1, 2, 3];
}
fn main() int {
    let a = make();
    return a[0] + a[1] + a[2];
}
```

`go run ./cmd/pebc -run <file.peb>` fails with: `array return is a
ArrayValue, want an array local or array-returning call`.

**Root cause.** `buildArrayReturnValue`
(`compiler/internal/backend/calls.go:1808-1842`) only handles
`tir.DirectCall` (delegating to `buildDirectCall`) and
`tir.SymbolValue` (an existing array local) as the tail-position return
node; anything else — including a direct `ArrayValue` (`[1, 2, 3]`)
or `ArrayRepeat` (`[v; N]`) literal — is rejected outright. The element
brace-building logic already exists and is shared elsewhere:
`buildArrayBraceElements` (`compiler/internal/backend/locals.go:270`)
builds the per-element C expressions for an `ArrayValue`, and a
similar `ArrayRepeat` builder exists for
`buildArrayRepeatLocalDeclaration` (`locals.go:317+` — read it to find
the exact repeat-element expression it builds). This is the same shape
of gap as the local-copy-initialization family (tasks #40-45): a value
shape the backend already knows how to build in one context (a local
declaration) isn't yet wired into a sibling context (a return
statement).

**Scope:** add `tir.ArrayValue` and `tir.ArrayRepeat` cases to
`buildArrayReturnValue`, reusing `buildArrayBraceElements` (and the
equivalent single-value-repeated builder for `ArrayRepeat`) to build
the element expressions, then emit the same
`(%s){ .data = { %s } }` brace-list shape the existing tail return
already produces for a `SymbolValue` array local. Verify: the
reproduction above compiles and runs, returning 6; an `ArrayRepeat`
direct return (`return [7; 3];`) compiles and runs, returning 21; the
existing `DirectCall` and `SymbolValue` return paths are unaffected;
an array of a non-trivial element type (struct or tuple element, if
practical) returned directly also works, mirroring what
`buildArrayBraceElements` already supports for locals.

**Resolution (`7c625ab`, 2026-08-11).** `buildArrayReturnValue`
gained `tir.ArrayValue` and `tir.ArrayRepeat` cases, reusing
`buildArrayBraceElements` for the literal case. The first delivered
diff for `ArrayRepeat` had a real bug an independent review caught
before commit: it repeated the built value-expression STRING `length`
times in the C brace list rather than evaluating it once, so any
side-effecting `[v; N]` value would have run `v` `N` times instead of
once — a silent divergence from the single-evaluation semantics
`buildArrayRepeatLocalDeclaration` already guarantees for the local
case. Fixed in the same commit by threading a `preReturn` temp
declaration (mirroring the existing `buildSliceReturnValue` pattern)
so the value is built once into a C temp and the temp name is what's
repeated in the compound literal. Verified: both reproductions compile
and run correctly (6 and 21); the emitted C for the `ArrayRepeat` case
was inspected directly and confirmed a single `pebble_repeat_ret_N`
temp declaration with the value assigned exactly once, referenced
three times in the brace list; full suite
(`go test ./... -count=1 -timeout 600s -parallel 16`, 11 packages)
clean, `gofmt`/`go vet` clean, causation check confirmed reverting
reproduces the original rejection exactly.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: a plain (non-generic, non-recursive) three-level aggregate
dependency chain is rejected as "more than one level of nesting."**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1. Independently reproduced and root-caused before dispatch.

**Reproduction** (confirmed against current HEAD):

```
type Inner = struct { value int; };
type Middle = struct { inner Inner; };
type Outer = struct { middle Middle; };
fn main() int {
    let o = Outer.{ middle = Middle.{ inner = Inner.{ value = 42 } } };
    return o.middle.inner.value;
}
```

`go run ./cmd/pebc <file.peb>` fails with: `aggregate type
nominal(symbol 28) has more than one level of nesting, which is
unsupported`. Nothing generic or recursive here — three ordinary
non-generic structs, `Outer -> Middle -> Inner`, one level each.

**Known cause — looks like a stale, overly-conservative guard, not a
real limitation. Confirm this precisely, don't assume it.**
`orderAggregateTypes` (`compiler/internal/backend/typedefs.go:33-96`)
computes a `depth` for every tuple/optional/array/struct type via
recursive dependency walking (lines 38-85), then HARD-REJECTS anything
with `depth(id, ...) > 1` (lines 92-96) — before any typedef ordering
even happens. But the ACTUAL typedef-ordering logic immediately below
(lines 97+) is a proper dependency-first postorder DFS (`dfs`,
recursing into each type's own dependencies, emitting dependencies
before dependents) that appears to handle ARBITRARY nesting depth
correctly by construction — nothing in the postorder DFS itself
imposes a depth-1 limit. This strongly suggests the depth check was a
deliberate but now-obsolete safety guard from before the postorder DFS
existed/was trusted, not a genuine constraint of the C typedef
emission this backend produces.

**Scope:** investigate carefully whether the postorder DFS actually
produces CORRECT C typedef ordering at depth 2+ (the emitted C for
`Outer`/`Middle`/`Inner` must declare `Inner`'s typedef before
`Middle`'s, and `Middle`'s before `Outer`'s) — don't just delete the
check and hope; construct the reproduction above, temporarily
remove/raise the depth cap, and inspect the ACTUAL emitted C to verify
the ordering is correct, then run it end-to-end. If correct, remove or
substantially raise the depth cap (prefer removing it entirely unless
you find a genuine remaining constraint — if you find one, document it
precisely rather than picking an arbitrary higher number). Verify the
reproduction above compiles and runs, returning 42. Also verify: a
4-level chain (one level deeper than the reproduction) if practical,
a struct containing a nested struct field where the nested struct
ALSO has a nested struct field but nothing deeper (the existing
depth-1 case, already working) is unaffected, tuples/optionals nested
inside structs at depth 2+ (since the depth check covers those types
too), and that a GENUINELY problematic shape (if one exists — check
whether recursive/self-referential nesting is a separate, intentional
rejection elsewhere, which should stay rejected) is not accidentally
now accepted.

**Resolution (`e649476`, 2026-08-11).** The depth-1 cap in
`orderAggregateTypes` (`compiler/internal/backend/typedefs.go`) was
indeed a stale, overly-conservative guard for the plain struct/tuple/
optional case, but not removable wholesale: a struct field whose type
is an array of an aggregate (`struct { arr [2]Inner }`) also hits the
same cap, and `emit.go`'s field-referenced-array typedef ordering
relies on that specific rejection staying in place (it unconditionally
emits array typedefs before the aggregate block, which is only correct
because no array element could be an aggregate). Fix: `depth()` now
returns both the max nesting depth and whether any dependency edge in
the chain passed through an array type; the depth>1 rejection fires
only when an array is present in the chain. Verified: the three-level
and four-level struct-only reproductions compile and run correctly
(exit 42), with `Inner`'s typedef confirmed emitted before `Middle`'s
and `Middle`'s before `Outer`'s in the actual emitted C; a tuple+
optional-in-struct chain at depth 2 compiles and runs; the
array-of-aggregate shape (`struct { arr [2]Inner }`, constructed so
the checker actually reaches it) still rejects with the same message;
a recursive struct via a pointer field is unaffected (emitted C
identical before/after, confirmed via causation check). Full suite
(`go test ./... -count=1 -timeout 600s -parallel 16`, 11 packages)
clean, `gofmt`/`go vet` clean, independent causation check confirmed
reverting the fix reproduces the original rejection exactly.
-->

<!-- Previous item, resolved 2026-08-10:

**Item: a `TupleCoerce` node (per-element implicit tuple coercion)
reaches the backend and fails.**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1. Independently reproduced and root-caused before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let a i32 = 1;
    let b i32 = 2;
    let value (i64, f64) = (a, b);
    return value.0 as i32;
}
```

`go run ./cmd/pebc <file.peb>` fails with: `entry function body block
declares a tuple-typed local of type pebble_tuple_26_t initialized
from a TupleCoerce, want a TupleValue (a tuple literal), a call to a
tuple-returning helper, or a reference to a tuple-typed local in scope
of that type`.

**Known cause and exact node shape — already traced, don't
re-investigate from scratch.** The checker
(`compiler/internal/check/ir_builder_value.go:185-211`) builds a
`TupleCoerce` node whenever a tuple LITERAL's per-element source types
don't already match the destination tuple type's element types
(`(a, b)` here: `i32, i32` source elements coerced to `i64, f64`
destination elements). Confirmed via `internal/tir/verify.go:764-770`:
`TupleCoerce.Children` has AT LEAST 2 entries — `Children[0]` is a
synthesized `TupleValue` node holding the ORIGINAL, pre-coercion
element expressions (kept for tooling/verification, not needed for
codegen), and `Children[1:]` are the ALREADY-COERCED per-element
expression nodes (each individually cast/wrapped to its correct
destination type) that should actually be emitted — `TypeArgs` holds
the destination element types, one per coerced child, same order.

**Scope:** add a `TupleCoerce` case to `buildTupleLocalDeclaration`
(`compiler/internal/backend/locals.go`) that builds a tuple brace list
using `initValue.Children[1:]` as the element value nodes — reuse
`buildTupleBraceList`'s existing element-building logic (or the
underlying per-element expression builder it calls), sourcing from the
coerced children (index 1 onward), NOT `Children[0]` (the uncoerced
tuple) and NOT the raw node's own `Children` (index 0 onward, which
would be wrong for this node kind). Confirm element count consistency
(`len(initValue.Children) - 1` should equal the destination tuple
type's element count). Verify the reproduction above compiles and
runs, returning 1 (`(i64)1 as i32`). Also verify: a 3-element tuple
needing coercion on only SOME elements (not all), and that ordinary
tuple literal initialization (no coercion needed) and tuple local copy
initialization (`834927e`) are unaffected.

**Resolution (`d905ab6`, 2026-08-10, Luna).** Added a `TupleCoerce`
case to `buildTupleLocalDeclaration` building a brace list from
`Children[1:]` against the destination type recovered from `TypeArgs`
(`TupleCoerce.Type` is the SOURCE type, not the destination — no more
direct field carries it). Also fixed a compounding typedef-collection
gap for the destination tuple's typedef. Verified the repro and a
partial-coercion case; ordinary tuple literal/copy-init unaffected.
Causation-checked.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: a slice-typed local cannot be initialized from another slice
value (`let second []int = first;`) — the LAST of the 6 "local copy
initialization" slices.**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1. Independently reproduced before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let first []int = [1, 2, 3];
    let second []int = first;
    return second[0];
}
```

`go run ./cmd/pebc <file.peb>` fails with: `entry function body block
slice construction is a SymbolValue, want a CheckedSlice`.

**Known cause — this one is shaped differently from the other 5.**
`buildSliceLocalDeclaration`
(`compiler/internal/backend/locals.go:398`) has NO fallback rejection
of its own for an unrecognized initializer kind — it handles
`DirectCall`/`MethodCall`, `SliceFromRaw`, and `Load` (a whole-struct
copy of a slice-typed struct FIELD, `var old_entries = self.entries;`
— note this ALREADY does almost exactly what we need, just for a
field read instead of a plain local reference) explicitly, then falls
through to `buildSliceConstruction`
(`compiler/internal/backend/aggregates.go:456`) for anything else,
which only accepts a `CheckedSlice` node — hence the confusing "want a
CheckedSlice" message for a plain `SymbolValue`.

**Scope:** add a `SymbolValue` case to `buildSliceLocalDeclaration`,
mirroring its OWN existing `Load` case almost exactly (same
declaration-with-initializer shape: `pebble_slice_<typeID>_t
pebble_local_<new> = <value>;` plus `(void)` cast) — a slice is a
struct (`.data`/`.len`), so copying the struct header directly (not
the underlying array) is correct and matches how DirectCall/
SliceFromRaw/Load already just assign the compound value. Scope lookup
+ type-match guard, same pattern as the other 5 already-fixed
siblings (check this function's `localInfo` field name for slice type
— likely `sliceType`, confirm by reading the existing `Load` case's
own scope-registration line). Verify the reproduction above compiles
and runs, returning 1. Also verify: a slice copied through a helper
call vs. through a plain local reference behave identically, and that
slice reassignment (if it exists), slice indexing, and the existing
`Load`/`DirectCall`/`SliceFromRaw` initializer paths are unaffected.

**Resolution (`22ceab8`, 2026-08-10).** Added a `SymbolValue` case to
`buildSliceLocalDeclaration` before the `buildSliceConstruction`
fallthrough, mirroring the function's own `Load` case — a slice is a
struct, no `memcpy` needed. Confirmed the shared-backing-array
semantics are correct (matches how a slice parameter already behaves).
Verified the repro, a chained copy, a copy of a resliced array, and a
write-through-copy visible in the original; existing initializer
paths, indexing, and `.len` unaffected. Causation-checked.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: a `str`-typed local cannot be initialized from another `str`
value (`let second str = first;`).**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1, fifth of 6 "local copy initialization" slices. Independently
reproduced before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let first str = "hello";
    let second str = first;
    if second == "hello" { return 0; }
    return 1;
}
```

`go run ./cmd/pebc <file.peb>` fails with: `entry function body block
declares a str-typed local initialized from a SymbolValue, want a
StringLiteral (a string literal) or a call to a str-returning helper;
initializing a str local from another value is not supported yet`.

**Known cause and required approach.** `buildStrLocalDeclaration`
(`compiler/internal/backend/locals.go:1010`) rejects a `SymbolValue`
initializer. A `str`-typed local is `PebbleStr` — a plain C struct
`{data, len}` — same representation family as the already-fixed
tuple (`834927e`), struct (`2179ebf`), and enum (`7f1db25`) siblings.
Trivially declaration-initializable from another `str` value in plain
C, no `memcpy` needed.

**Scope:** widen `buildStrLocalDeclaration` to accept a `SymbolValue`
naming an in-scope `str`-typed local, mirroring the tuple/struct/enum
local-copy fixes' acceptance-logic shape exactly (scope lookup, a
`str`-specific type check — check how this codebase's `localInfo`
marks a local as str-typed, likely a boolean flag rather than a
type-ID field like the aggregate cases, since every `str` shares one
C type `PebbleStr` — don't assume the same field shape as
tuple/struct/enum), single-statement declaration-with-initializer plus
`(void)` cast. Verify the reproduction above compiles and runs,
returning 0. Also verify: a `str` local copied from a helper-call
result stored in an intermediate local (chained copy), and that `str`
reassignment, `str` equality, and `str`-typed struct fields are
unaffected.

**Resolution (`7747aaa`, 2026-08-10).** Added a `SymbolValue` branch to
`buildStrLocalDeclaration` guarded on `localInfo.isStr` (str has no
per-declaration type ID, unlike the aggregate cases) — `PebbleStr` is
a genuine C struct, no `memcpy` needed. Verified the repro and a
chained copy (`a → b → c`); reassignment, equality, and str-typed
struct fields unaffected. Causation-checked.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: an enum-typed local cannot be initialized from another enum
value (`let second Color = first;`).**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1, fourth of 6 "local copy initialization" slices. Independently
reproduced before dispatch.

**Reproduction** (confirmed against current HEAD):

```
type Color = enum { red, green, blue };
fn main() int {
    let first = Color.green;
    let second Color = first;
    if second == Color.green { return 0; }
    return 1;
}
```

`go run ./cmd/pebc <file.peb>` fails with: `entry function body block
declares an enum-typed local of type pebble_enum_19_t initialized from
a SymbolValue, want a variant literal (e.g. Color.green) or an integer
cast to the enum type (e.g. 5 as Color); initializing an enum local
from another value is not supported yet`.

**Known cause and required approach.** `buildEnumLocalDeclaration`
(`compiler/internal/backend/locals.go:781`) rejects a `SymbolValue`
initializer. An enum-typed local is a plain C `enum`
(`pebble_enum_<id>_t`), trivially declaration-initializable from
another enum value in plain C — same shape as the already-fixed tuple
(`834927e`) and struct (`2179ebf`) siblings, no `memcpy` needed. NOTE:
this same function already gained a `Load(CheckedIndexPlace)` case
earlier TODAY (commit `94a2a39`, the enum-typed-array/slice-element
fix) — read that diff too, so the new `SymbolValue` case doesn't
duplicate or conflict with the existing cases in this function.

**Scope:** widen `buildEnumLocalDeclaration` to accept a `SymbolValue`
naming an in-scope enum-typed local of the exact matching type,
mirroring the tuple/struct local-copy fixes' acceptance-logic shape
(scope lookup, type-match guard, single-statement declaration-with-
initializer plus `(void)` cast — check this function's own `localInfo`
field name for enum type, likely something like `enumType`, don't
assume it matches tuple/struct's field names). Verify the reproduction
above compiles and runs, returning 0. Also verify: a different variant
(not just the first one declared), and that enum-typed struct fields,
enum-typed array/slice elements (from `94a2a39`), and enum equality
comparisons are unaffected.

**Resolution (`7f1db25`, 2026-08-10).** Added a `SymbolValue` branch
to `buildEnumLocalDeclaration` mirroring the tuple/struct fixes
exactly — an enum is a plain C enum, no `memcpy` needed. Sits
correctly alongside (not conflicting with) the `Load(CheckedIndexPlace)`
case from `94a2a39`. Verified the repro and a second-variant copy
(proving tag round-tripping); reassignment, struct fields, array/slice
elements, and equality unaffected. Causation-checked.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: a struct-typed local cannot be initialized from another
struct value (`let second Point = first;`).**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1, third of 6 "local copy initialization" slices. Independently
reproduced before dispatch.

**Reproduction** (confirmed against current HEAD):

```
type Point = struct { x int; y int; };
fn main() int {
    let first = Point.{ x = 1, y = 2 };
    let second Point = first;
    return second.x;
}
```

`go run ./cmd/pebc <file.peb>` fails with: `entry function body block
declares a struct-typed local of type pebble_struct_19_t initialized
from a SymbolValue, want a RecordConstruct (a struct literal) or a
call to a struct-returning helper; initializing a struct local from
another value is not supported yet`.

**Known cause and required approach — the SIMPLEST of the 6 local-
copy-init slices.** `buildStructLocalDeclaration`
(`compiler/internal/backend/locals.go:656`) rejects a `SymbolValue`
initializer. Unlike array, a struct-typed local IS a genuine C struct
(`pebble_struct_<id>_t`), freely declaration-initializable from
another struct value in plain C
(`pebble_struct_19_t pebble_local_28 = pebble_local_27;` is valid,
ordinary C — no `memcpy` needed, exactly like tuple). This is the
closest possible sibling of the ALREADY-FIXED tuple local copy
initialization (`834927e`,
`buildTupleLocalDeclaration`) — same representation, same fix shape,
different type.

**Scope:** widen `buildStructLocalDeclaration` to accept a
`SymbolValue` naming an in-scope struct-typed local of the exact
matching type, mirroring `buildTupleLocalDeclaration`'s `SymbolValue`
branch (`834927e`) almost exactly — same type-match guard shape, same
single-statement `<structType> pebble_local_<new> =
pebble_local_<other>;` plus `(void)` cast. Verify the reproduction
above compiles and runs, returning 1. Also verify: a struct with more
fields, a struct containing a nested struct field, and that whole-
struct REASSIGNMENT (already working, from an earlier session's
`9df0351`/`5ef060a`) and struct field reads are unaffected.

**Resolution (`2179ebf`, 2026-08-10).** Added a `SymbolValue` branch to
`buildStructLocalDeclaration` mirroring the tuple fix (`834927e`)
exactly — a struct is a plain C struct, so a declaration-with-
initializer needs no `memcpy`. Verified the repro, a 3-field struct,
and a struct containing a nested struct; reassignment and field reads
unaffected. Causation-checked.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: an array-typed local cannot be initialized from another array
value (`let second [3]int = first;`).**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1, second of 6 "local copy initialization" slices. Independently
reproduced before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let first [3]int = [1, 2, 3];
    let second [3]int = first;
    return second[0];
}
```

`go run ./cmd/pebc <file.peb>` fails with: `entry function body block
declares an array-typed local of type [3]int initialized from a
SymbolValue, want an ArrayValue (an array literal) or an ArrayRepeat
(a [v; N] repeat initializer); initializing an array local from
another value is not supported yet`.

**Known cause and required approach — IMPORTANT, different shape from
the tuple sibling fix.** `buildArrayLocalDeclaration`
(`compiler/internal/backend/locals.go:145`) rejects a `SymbolValue`
initializer. Unlike tuple (a C struct, freely `=`-assignable), a
standalone array local is a RAW C array
(`int32_t pebble_local_27[3] = { 1, 2, 3 };`, confirmed earlier
today). A C declaration CANNOT be initialized from another array
VARIABLE via `=` either — `int32_t b[3] = a;` is just as invalid as
`b = a;` (only a brace-enclosed initializer list or another array's
address is legal). This is the exact representation problem today's
array REASSIGNMENT fix (`aef808e`,
`buildArrayStoreValue`/`buildStoreCore`) already solved via `memcpy` —
this is the sibling case for array local DECLARATION instead of
reassignment.

**Scope:** widen `buildArrayLocalDeclaration` to accept a `SymbolValue`
naming an in-scope array-typed local of the exact matching type
(length AND element type). The emitted C must declare the new local's
storage first (uninitialized, or however this codebase's convention
handles a declaration whose initializer isn't a simple expression —
check how the declaration-then-separate-statement pattern is
structured elsewhere, e.g. `buildRuntimeLocalDeclaration` or similar
multi-statement local builders), then `memcpy` the source array's
bytes into it — mirroring `buildArrayStoreValue`'s exact memcpy
pattern from `aef808e`. Read `git show aef808e` in full as the
template. Verify the reproduction above compiles and runs, returning
1. Also verify: a longer array (5+ elements), a bool-element array,
and that whole-array REASSIGNMENT (`aef808e`, already working) and
array element reads/writes are unaffected.

**Resolution (`8c72f36`, 2026-08-10).** Added a `SymbolValue` branch
to `buildArrayLocalDeclaration`, emitting a bare declaration followed
by a `memcpy` (the same shape `aef808e`'s reassignment fix uses,
adapted for a declaration since C can't initialize a raw array
variable from another array variable either). `hasArrayStore` set so
`<string.h>` is included. Verified the repro, a 5-element array, and a
bool-element array; reassignment and element reads/writes unaffected.
Causation-checked.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: a tuple-typed local cannot be initialized from another tuple
value (`let second (int, int) = first;`).**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1, first of 6 "local copy initialization" slices (tuple, array,
struct, enum, string, slice — one type per task, matching this
tracker's established discipline). Independently reproduced before
dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    let first (int, int) = (1, 2);
    let second (int, int) = first;
    return second.0;
}
```

`go run ./cmd/pebc <file.peb>` fails with: `entry function body block
declares a tuple-typed local of type pebble_tuple_23_t initialized
from a SymbolValue, want a TupleValue (a tuple literal) or a call to a
tuple-returning helper; initializing a tuple local from another value
is not supported yet`.

**Known cause:** `buildTupleLocalDeclaration`
(`compiler/internal/backend/locals.go:92`) only accepts a `TupleValue`
(a tuple literal) or a `DirectCall` to a tuple-returning helper as the
initializer — a `SymbolValue` naming an already-declared tuple-typed
local is rejected. This is the exact same shape of gap already fixed
TODAY for tuple REASSIGNMENT (`d1b05be`,
`buildTupleStoreValue`/`buildStoreCore`) — this is the sibling case
for tuple local INITIALIZATION (`let x = y;`, a fresh declaration,
not reassigning an existing local).

**Scope:** widen `buildTupleLocalDeclaration` to also accept a
`SymbolValue` naming an in-scope tuple-typed local of the exact
matching type, emitting the same plain C struct assignment/
initialization the literal case already produces (a tuple lowers to a
C struct; C already allows initializing one struct FROM another
struct value directly in a declaration, `pebble_tuple_23_t
pebble_local_28 = pebble_local_27;`, which is simpler than the
reassignment case's separate-statement `=` — confirm this exact C
shape compiles). Mirror `buildTupleStoreValue`'s exact acceptance
logic (type-match guard, scope lookup) rather than inventing new
logic. A tuple-returning-call right-hand side already works (existing
behavior, confirmed accepted) — don't touch that path. Verify the
reproduction above compiles and runs, returning 1. Also verify: a
3-element tuple, a mixed-type tuple, and that whole-tuple REASSIGNMENT
(`d1b05be`, already working) and tuple ELEMENT reads are unaffected.

**Resolution (`834927e`, 2026-08-10).** Added a `SymbolValue` branch
to `buildTupleLocalDeclaration` mirroring `buildTupleStoreValue`'s
acceptance logic, emitted as a plain C declaration-with-initializer.
Verified the repro, a 3-element tuple, and a mixed-type `(int, str)`
tuple; reassignment and element reads unaffected. Causation-checked.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: an unbound range loop (`loop start..end { ... }`, no `: name`
iterator) is accepted by the parser, checker, and TIR, then rejected
only at the backend with an internal-sounding error.**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1. Independently reproduced before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    var total int = 0;
    loop 0..3 {
        total = total + 1;
    }
    return total;
}
```

`go run ./cmd/pebc <file.peb>` fails at EMISSION (not the checker):
`entry function body block contains an unbound range loop (loop
start..end { ... } with no ': name' iterator); only the bound `loop
start..end : name { ... }` form is supported`.

**Known cause:** `prepareRangeLoop`
(`compiler/internal/check/control_facts.go:257`) only publishes the
iterator symbol and binding INSIDE the `if iterator != (symbol.SyntaxRef{})`
block (line 288). When no `: name` is authored, this whole block is
skipped — nothing rejects the omission, `emission.iteratorSymbol`
simply stays zero, and the program proceeds through the checker and
into TIR silently missing that data. Only later, deep in
`buildRangeLoop` (`compiler/internal/backend/statements.go:1319-1330`,
checking `rangeNode.Symbol == 0`), does anything notice — producing a
low-level "emission failed" message a user would have no way to
connect to "you forgot `: name`" without reading the compiler's
internals.

**Decision** (matches Sol's framing): V2's policy of requiring an
explicit iterator name (`: name`) is being KEPT (an intentional
difference from V1, which synthesizes an implicit `iter` name) — this
is not a "V1 parity" gap to close by implementing an implicit name.
The gap is a CHECKER/BACKEND CONTRACT DEFECT: whatever V2's actual
policy is, it should be enforced at the checker, with a real
diagnostic and source span, not discovered by the backend three
compiler phases later.

**Scope:** add a checker diagnostic (next free code: `C0622`, since
`C0601`-`C0621` are all already in use — confirm this is still true
before picking the number) rejecting a range loop with no `: name`
iterator, emitted at the loop's own source span, with a clear message
("a range loop requires an explicit iterator name, e.g. `loop
0..3 : i { ... }`" or similar). Wire it into the same validation pass
other control-flow diagnostics use (see `control_validation.go`'s
existing `C06xx` codes for the established pattern/convention). Verify
the reproduction above now fails at the CHECKER (not emission) with
the new clean diagnostic; verify the bound form (`loop 0..3 : i {
... }`) is completely unaffected; verify existing range-loop tests
pass unchanged. Once the checker rejects it cleanly, the backend's own
`rangeNode.Symbol == 0` guard becomes defense-in-depth for hand-built
TIR (matching this codebase's established pattern elsewhere) — leave
it in place, don't remove it.

**Resolution (`87e8c43`, 2026-08-10).** Added `CodeUnboundRangeIterator`
(`C0622`), reported in `validateControlFlow`'s `controlRangeLoop` case
when `ctrl.IteratorSymbol == 0`, at the loop's own span. The backend's
`Symbol == 0` guard stays as defense-in-depth. A real-source test that
used to exercise that backend guard could no longer reach it (the
checker now rejects first) — rewritten to hand-build a Symbol-0
`RangeLoop` through the IR builder. Verified the repro fails at the
checker with the correct span; the bound form and existing tests
unaffected. Causation-checked.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: a non-literal bool switch subject emits an invalid native C
`switch(bool)`, failing under the mandated `-Wswitch-bool -Werror`.**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P1. Independently reproduced and isolated before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn choose(flag bool) int {
    switch flag {
        case true: return 1;
        else: return 0;
    }
}
fn main() int {
    return choose(true);
}
```

`go run ./cmd/pebc -run <file.peb>` fails at the `cc` step:
`error: switch condition has boolean value [-Werror,-Wswitch-bool]`.
The checker accepts this program fine (bool switch exhaustiveness is
already proven, per proposal 14's switch matrix) — this is purely a
backend C-shape bug. (A separate-looking `-Wreturn-type` error only
appeared in my FIRST reproduction attempt, which omitted the `else`
clause — that's an artifact of my own incomplete test, not a real
second bug; confirmed by re-testing with an explicit `else`, which
isolates cleanly to just `-Wswitch-bool`.)

**Known cause:** `buildSwitchStatement`
(`compiler/internal/backend/statements.go`, around line 711) routes a
bool-typed subject through `buildBoolExpr` into the SAME native-C-
`switch(...)` lowering as int/uint/char subjects. C forbids/warns on
switching directly on a `bool` expression under `-Wswitch-bool`. A
str-typed subject already has its own dedicated lowering (an if/else
chain via `buildStrSwitchStatement`, just fixed today for double-
evaluation) precisely because C switch labels must be integer
constants — bool needs similar special treatment, but the fix here is
much smaller: bool's underlying value IS already an integer (0 or 1),
so casting the subject expression to `int` before the native C switch
(`switch ((int)pebble_local_25)`) is the standard, minimal C idiom
that silences `-Wswitch-bool` without restructuring the lowering into
an if/else chain at all.

**Scope:** when the subject is bool-typed, wrap the built subject
expression in an `(int)` cast (or `int32_t`, matching whatever this
backend's existing int-cast convention is — check `cType`/similar) for
JUST the C `switch (...)` header — the case labels themselves (`case
1:`, `case 0:` or however `true`/`false` case constants are currently
spelled) don't need to change, since a `switch(int)` still compares
correctly against integer case labels. Verify the reproduction above
now compiles and runs (returns 1). Also verify: a LITERAL bool subject
(`switch true { ... }`) is unaffected (still works, was already
covered by existing tests per the ledger's "Verified V2 extension"
claim); a bool switch with both `case true`/`case false` (no `else`,
exhaustive by construction) compiles and runs correctly; existing bool
switch tests are unaffected.

**Resolution (`9b86144`, 2026-08-10).** The bool subject is now cast
to `int32_t` for just the C switch header
(`switch ((int32_t)pebble_local_25)`), matching this backend's
existing cast convention elsewhere. Case labels unchanged — they
already compare correctly against an `int32_t` switch. Verified the
non-literal repro, literal bool subjects, and an exhaustive true/false
switch (no else). Causation-checked.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: a range loop with a runtime-computed or negative-literal
descending bound silently runs zero iterations.**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P0. Independently reproduced before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn start_val() int { return 3; }
fn end_val() int { return 0; }
fn main() int {
    var total int = 0;
    loop start_val()..end_val() : i {
        total = total + 1;
    }
    return total;
}
```

```
fn main() int {
    var total int = 0;
    loop 0..-5 : i {
        total = total + 1;
    }
    return total;
}
```

Both `go run ./cmd/pebc -run <file.peb>` runs return `0` — `total`
never incremented, the loop body never ran, no error, no warning. A
genuinely silent semantic bug.

**Known cause:** `buildRangeLoop`
(`compiler/internal/backend/statements.go:1319`) only chooses the
descending direction (`>`/`--`) when BOTH bounds' emitted C text
parses via `strconv.Atoi` as plain decimal literals (lines 1375-1394)
— i.e. only when both bounds are compile-time-known, non-negative
integer LITERALS. A runtime bound (a call, a variable) never produces
`strconv.Atoi`-parseable text, so it silently falls through to the
"ascending" default. A negative literal ALSO fails this: `-5` in
Pebble source doesn't lower as a single `IntegerLiteral` node with
negative text — `buildRangeBound`'s literal branch
(`isNonNegativeDecimal`, line ~1449) only accepts non-negative decimal
text, so a negative bound is built via a different node shape entirely
(routed through `buildExpr`, likely emitting a checked-negation
runtime call, not plain decimal text) — which also fails the
`strconv.Atoi` literal check and falls through to ascending.

**V1's actual fix, already in production — mirror it exactly, don't
invent a new pattern.** `src/codegen.c:2568-2670` (`AST_STMT_LOOP`)
NEVER special-cases compile-time-literal bounds. It ALWAYS:
1. Evaluates both bounds once into C locals (`int loop_start0 = ...;
   int loop_end0 = ...;`).
2. Computes the step direction AT RUNTIME: `int loop_step0 =
   (loop_start0 <= loop_end0) ? 1 : -1;`.
3. Uses a ternary-conditioned loop test that works for either
   direction: `for (int loop_i0 = loop_start0; (loop_step0 > 0) ?
   (loop_i0 < loop_end0) : (loop_i0 > loop_end0); loop_i0 +=
   loop_step0) { ... }` (with `<=`/`>=` swapped in for an inclusive
   range).

This ONE uniform lowering handles compile-time-ascending,
compile-time-descending, negative-literal, and runtime-computed bounds
identically, with zero special-casing — it's strictly simpler than
V2's current two-path (literal vs. non-literal) logic, not more
complex, and it's V1 parity by construction since it's V1's own
emitted C, verbatim.

**Scope:** replace `buildRangeLoop`'s compile-time-literal direction
detection entirely with this always-runtime-direction lowering.
Verify both reproductions above now return `3` (three iterations).
Also verify: the existing compile-time-ascending case (`loop 0..3 :
i`) and compile-time-descending case (`loop 3..0 : i`, from the
earlier `8baeb8e` fix) both still produce the correct iteration count
and correct iterator values inside the body; inclusive ranges (`loop
0..=3`) in both directions; a zero-length range (`loop 3..3`) still
runs zero iterations correctly (not an off-by-one). Full suite clean,
causation-checked.

**Note — likely fixes a second tracked P0 as a byproduct.** Sol's
audit separately flagged that `buildRangeLoop` evaluates the END bound
before the START bound (a side-effecting bound records the wrong
order vs. V1). Since V1's pattern (above) evaluates start into a local
FIRST, then end into a local SECOND, as two separate sequential C
statements, adopting it here should also fix that ordering bug for
free. Confirm this explicitly with a side-effecting-bounds
reproduction (each bound a call that appends to a shared counter/log)
proving start runs before end, and report whether a separate dispatch
for that item is still needed.

**Resolution (`003141d`, 2026-08-10).** Replaced the compile-time-
literal direction detection entirely with V1's actual production
lowering (`src/codegen.c`'s `AST_STMT_LOOP`), mirrored verbatim: both
bounds evaluated once into C locals in source order, step computed at
runtime from comparing them, ternary-conditioned loop test. Verified a
runtime-bound descending range, a negative-literal range (`0..-5`
exclusive correctly visits 5 values: 0,-1,-2,-3,-4), all existing
ascending/descending/inclusive/zero-length/uint-bounded cases
unaffected. Confirmed the evaluation-order P0 (below) was also fixed
as a byproduct — no separate dispatch was needed. Causation-checked by
reverting and reproducing both original bugs.

-->

<!-- Previous item, resolved 2026-08-10:

**Item: a call-valued `str` switch subject is evaluated once per case
comparison instead of once.**

Sourced from Sol's fourth-pass audit (2026-08-10, commit `3047352`),
P0. Independently spot-verified before dispatch.

**Reproduction** (confirmed against current HEAD):

```
fn choose() str {
    print "called";
    return "b";
}
fn main() int {
    switch choose() {
        case "a": return 1;
        case "b": return 0;
        else: return 2;
    }
}
```

`go run ./cmd/pebc -run <file.peb>` prints `called` TWICE (once per
case comparison against `"a"` and `"b"`), instead of once. A genuine
silent semantic bug: any switch subject with a side effect (a call
that logs, mutates, or has any observable effect beyond its return
value) runs that side effect once per case label instead of once
total.

**Known cause:** `buildStrSwitchStatement`
(`compiler/internal/backend/statements.go:912`) builds the subject
expression ONCE via `buildStrOperand` (line 919), but a `str` switch
doesn't lower to a native C `switch` (C can't switch on a struct) — it
lowers to an if/else chain of `pebble_rt_str_eq(subjectExpr, lit)`
calls (line 972), and the same `subjectExpr` TEXT is spliced into
every one of those calls. A native C `switch(expr)` evaluates `expr`
exactly once by C's own semantics; this if/else-chain lowering gets no
such guarantee for free, and nothing here materializes the subject
into a temp first.

**Scope:** materialize the subject into a `PebbleStr` local temp once,
before the if/else chain, and reference that temp (not the raw
expression) in every `pebble_rt_str_eq` call — mirroring the
"materialize once into a per-operand temp" convention already used
elsewhere in this backend (e.g. composite print operands, the
tagged-union switch subject). Verify the reproduction above prints
`called` exactly once; verify a str-LITERAL subject (no side effect to
observe, but confirm it still compiles/runs correctly and the temp
doesn't break the no-side-effect case); verify existing str-switch
tests (from `49d0f23`) are unaffected; verify a `do { ... } while (0)`
break-wrapped chain still uses the temp correctly.

**Resolution (`b1a53e7`, 2026-08-10).** The subject is now materialized
into a `PebbleStr` temp once, before the if/else chain; every equality
check reads the temp. Verified the repro now prints once, not twice; a
str-literal/str-local subject and the break-wrapped shape are
unaffected; causation-checked by reverting and reproducing the exact
double-print.

-->



<!-- Previous item, resolved 2026-08-10:

**Item: `context`-as-value — bare `context` expression fails as a function
argument and as a `let` local's initializer.**

Sourced from proposal 15 slice 4's verification (2026-08-10) — the
`Allocator`/`Context` ordinary-struct redesign (`b54d79d`/`dee9b0f`/
`a404f14`) fully fixed `Allocator` crossing a function boundary as an
argument, a return value, and a struct-field assignment, but `Context`
specifically was never independently verified. The user asked "so we
can use context expr and allocator type as we like?", which prompted
testing `Context` on its own — it's not the same, and it's broken in
three of four value positions.

**Reproduction** (confirmed against current HEAD, `go run ./cmd/pebc -run <file.peb>`):

```
fn use_context(c: Context) void {}

fn make_local() void {
    let c = context;
}

fn returns_context() Context {
    return context;
}

struct Holder { c: Context }
fn field_works() Holder {
    return Holder.{ c = context };  // this ONE already works
}
```

- Argument: `use_context(context)` fails —
  `entry function body expression contains a call to symbol 24 whose
  argument 0 is a ContextValue, want a reference to a struct-typed
  local in scope or a struct literal (a RecordConstruct); only passing
  an already-declared struct-typed local or constructing a fresh struct
  literal inline is supported`
- Local initializer: `let c = context;` fails —
  `entry function body block declares a runtime-typed local initialized
  from a ContextValue`
- Return: `return context;` fails —
  `entry function body return statement returns a ContextValue, want a
  reference to a struct-typed local in scope, a struct literal (a
  RecordConstruct), or a call to a struct-returning helper (a
  DirectCall); only returning an already-declared struct-typed local,
  constructing a fresh struct literal inline, or forwarding a
  struct-returning helper call is supported`
- Struct-field construction value (`Holder.{ c = context }`) already
  works — confirmed via direct repro, exit 1 (no error).

**Known cause:** the bare `context` keyword expression lowers to a
distinct TIR node kind, `ContextValue` — not `SymbolValue` (the existing
runtime-identity reference handling) and not `RecordConstruct` (the
shape slice 3's Allocator-in-value-position fix added support for, see
`compiler/internal/backend/aggregates.go`'s `buildRuntimeAllocatorBraceList`
and `compiler/internal/backend/values.go`'s `buildRuntimeValue`/
`buildStructValueExpr`). `buildRuntimeValue` already has a
`node.Symbol == unit.Runtime().Context` check that returns `"(*ctx)"`
for one call path, but the argument-building, local-declaration-
building, and return-building code paths never consult it for a bare
`ContextValue` node — they only recognize `SymbolValue`/`RecordConstruct`
shapes. Needs a `ContextValue` case added to whichever functions build
call arguments, local initializers, and return values, mirroring the
existing single-site handling.

**Scope:** fix all three broken positions (argument, local initializer,
return) using the reproductions above as acceptance tests; reconfirm
the already-working struct-field-value case is unaffected; full suite
clean; causation-check against the exact errors quoted above.

-->

**Resolution (`64d2e2b`, 2026-08-10).** `buildAggregateArgument`,
`buildRuntimeLocalDeclaration`, and `buildAggregateReturnValue` each
gained a `ContextValue` case emitting `(*ctx)`, mirroring the existing
single-site handling. Verified end-to-end for all three positions via
a real alloc→write→read→free roundtrip through `default_allocator`;
the struct-field-value case and Allocator's own slice-3 paths
reconfirmed unaffected; causation-checked against the exact pre-fix
rejection messages.
