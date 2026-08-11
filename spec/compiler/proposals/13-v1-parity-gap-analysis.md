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
