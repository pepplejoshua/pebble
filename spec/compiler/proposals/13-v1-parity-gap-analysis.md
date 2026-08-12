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

*(empty — item #87 (char-to-uint explicit cast) closed in `60a3346`.
Second fix in the consolidated gap-filling pass over the proof-batch
findings: #88 (narrow pointer-to-int, needs a design call), #89
(negative-MIN literal), #90 (void anonymous function C0607), #92
(struct call-result/field-read as argument/receiver), and #93 (bare
string-literal arrow-body T0501) remain queued.)*

<!-- Previous item, resolved 2026-08-11:

**Item: char-to-uint explicit cast (`c as uint`) failed at Emit.**

`buildUintExpr` (`internal/backend/values.go`) is a separate builder
function from `buildExpr`; it had no `CharToInteger` case, so every
other integer destination width worked (`buildExpr`'s own
`CharToInteger` case) but `uint` failed with "unsupported uint
expression node CharToInteger".

**Resolution (`60a3346`, 2026-08-11).** Added a `CharToInteger` case
to `buildUintExpr` mirroring its existing `IntegerCast`/
`PointerToInteger` cases: build the char child via `buildCharOperand`,
cast to `cType(types.Uint)`. Four new tests prove the local-reference,
literal-source, high-codepoint (non-truncating), and emitted-C shapes;
the pre-existing all-widths matrix test's uint row (previously
deliberately absent) is now filled in.

-->

<!-- Previous item, resolved 2026-08-11:

**Item: nil pointer whose pointee is a plain enum or tagged union,
never otherwise referenced as a value, failed to compile.**

Reproduction:

```
type Color = enum { red, green, blue }
fn main() int {
    var pe *Color = nil;
    if pe != nil { return 8; }
    return 0;
}
```

`Emit` succeeded but the emitted C declared
`pebble_enum_19_t * pebble_local_31 = NULL;` with no
`typedef ... pebble_enum_19_t;` anywhere, so `cc` failed "unknown type
name" under `-Wall -Wextra -Werror`. Same break for a tagged-union
pointee (`var pc *Choice = nil`) and for a helper parameter
(`fn is_nil(p *Color) int { ... }` called as `is_nil(nil)`). Adding any
unrelated value use of the enum/union elsewhere in the same program
made it compile — the pre-existing value-shape collection rules were
the only path to a typedef.

Cause: `collectStructTypesWalk` in `internal/backend/collect.go` had a
pointer-pointee collection rule (a pointer-typed `Initialize` child
whose pointee is a struct); `collectEnumTypesWalk` and
`collectUnionTypesWalk` lacked the mirror. Neither `collectEnumTypes`
nor `collectUnionTypes` scanned a reachable helper's parameter list for
a pointer-to-enum/union pointee either — a nil pointer argument
constructs no value node, so only a signature-level scan finds it
(`collectStructTypes` already had this scan; the enum/union callers
didn't).

**Resolution (`6c0af95`, 2026-08-11).** Added the mirrored
pointer-pointee rule to both walks' `Initialize` handling and the
mirrored Parameters scan to both callers. The enum pointee check uses
`isDefinitelyEnumType` (requires real variant-declaration evidence)
rather than the looser `isEnumType`, whose no-evidence fallback reports
true for a declared-but-empty `TypeDeclaration` — including an opaque
extern type like `FILE`; using the looser check regressed the
pre-existing `TestEmitNilPointerAcrossPointeeTypesCompilesAndRuns`
during development and was caught before landing. Two new tests prove
the local and helper-parameter shapes for both enum and union pointees
in isolation; a third proves the pre-existing passing shape (same type
used as both a nil-pointer pointee and a real value elsewhere) still
emits exactly one typedef per type, no duplicates from the two
collection sites now firing together. Causation-checked: reverting
`collect.go` alone reproduces both original cc failures exactly;
restoring the fix passes again.

-->

<!-- Previous item, resolved 2026-08-11:

**Item: tuple ordinal reads of `char` and `str` elements are cleanly
rejected — a genuine narrow accessor gap, RESCOPED after
investigation to exclude `f64` (see below).**

Discovered during proof-batch verification (task #65-67), 2026-08-11.
Independently investigated and precisely rescoped before dispatch.

**Investigation finding — the original "f64/char/str" grouping was
WRONG, do not dispatch it as one task.** All three were re-investigated
individually:

- **`char`**: a genuine narrow gap, confirmed identical in shape to
  the char/str Load-case gaps already fixed elsewhere this session.
  `(char, int)` tuple CONSTRUCTION already works; only the read-back
  (`t.0` into a `char` local) fails: `entry function body expression
  contains a char Load whose place is a TuplePlace, want a
  CheckedIndexPlace (a char-element slice read)` —
  `buildCharOperand`'s `Load` gate
  (`compiler/internal/backend/values.go:1387`) only recognizes a
  `CheckedIndexPlace` (a slice element), not a `TuplePlace`.
- **`str`**: also a genuine narrow gap, and ALSO reachable through a
  struct field (not just a tuple) — `str` tuple/struct-field STORAGE
  already works; only reading a `str` value back out via a `Load` into
  a local fails, with the identical message for both the tuple and the
  struct-field case: `... declares a str-typed local initialized from
  a Load, want a StringLiteral ..., a call to a str-returning helper,
  or a reference to a str-typed local in scope` —
  `compiler/internal/backend/locals.go:1135`'s str-local-declaration
  switch has NO `Load` case at all.
- **`f64` — NOT a narrow gap, explicitly EXCLUDED from this task.**
  Investigation revealed `f64` is not accepted as a STRUCT FIELD type
  at all (`struct type ...: field type f64 is not supported, want a
  fixed-width integer, bool, str, tuple, struct, enum, pointer, slice,
  function type, or runtime type` — confirmed via a direct
  reproduction). This means float support as an aggregate MEMBER type
  (tuple element or struct field) is a pre-existing, deliberate, much
  larger scope boundary than a missing accessor case — task #22
  ("Support f32/f64 helper parameters and return values") scoped floats
  to PARAMETERS and RESULTS only, never aggregate members. Fixing this
  properly needs float support threaded through typedef field-type
  acceptance, `orderAggregateTypes`, and every aggregate value builder
  — a substantially bigger, separate future task, not a quick
  accessor fix. Do NOT attempt it as part of this item.

**Reproductions** (confirmed against current HEAD):

```
fn main() int {
    let t (char, int) = ('a', 1);
    let x char = t.0;
    if x == 'a' { return 1; }
    return 0;
}
```

```
fn main() int {
    let t (str, int) = ("hi", 1);
    let x str = t.0;
    return 0;
}
```

**Scope:**
1. Add a `Load(TuplePlace)` case to `buildCharOperand`
   (`compiler/internal/backend/values.go`), reusing `buildPlaceLValue`
   the way other tuple-ordinal reads for other types already do —
   read the existing int/bool tuple-ordinal `Load` case in `buildExpr`/
   `buildBoolExpr` as the pattern to mirror, and confirm
   `buildPlaceLValue` already resolves a `TuplePlace` correctly (it
   should, since int/bool tuple ordinals already work) before writing
   the char case.
2. Add a `Load` case to the str-local-declaration switch in
   `locals.go` (search for the exact rejection text above to find the
   switch), reusing `buildPlaceLValue` the same way — this single case
   should serve BOTH the tuple-ordinal shape (`t.0`) AND the
   struct-field shape (`b.v`), since both hit the identical rejection
   today; verify both with a reproduction, don't assume fixing one
   fixes the other.
3. Verify both reproductions above compile and run. Verify a str
   struct-field read-back (`let x str = b.v;` for a `Box.{ v = "hi" }`
   struct) also works from the same fix. Verify the existing
   int/bool/other tuple-ordinal reads and existing char/str usage
   (literals, comparisons, slice-element char reads) are completely
   unaffected.

**Resolution (`bd84ee9`, 2026-08-11).** Fixed exactly as rescoped —
`buildCharOperand` gained a `TuplePlace` case, and
`buildStrLocalDeclaration` gained ONE `Load` case correctly serving
both the tuple-ordinal AND struct-field shapes, confirmed
independently for both. Verified: both reproductions compile and run;
a str struct-field read-back also works from the same fix; existing
int/bool tuple-ordinal reads, char slice-element reads, and str
literals/comparisons are unaffected. `f64` remained untouched as
scoped, spun off as task #86 (deferred, separate, larger). A further
adjacent gap was found and correctly left alone: `str` tuple-ordinal
reads in a str VALUE position (e.g. a call argument) still reject via
`buildStrOperand`'s `FieldPlace`-only `Load` case — noted, not fixed,
not yet a formal item. Full suite (`go test ./... -count=1 -timeout
600s -parallel 16`, 11 packages) clean, `gofmt`/`go vet` clean,
causation check confirmed reverting reproduces both exact original
rejections.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: a tagged-union-typed struct field's construction fails —
`buildStructBraceList`'s enum-field case routes a payload-carrying
variant to the wrong builder.**

The THIRD sibling gap flagged during task #57's verification
(2026-08-11), now root-caused and formally opened (task #84).

**Reproduction** (confirmed against current HEAD):

```
type Choice = union enum { empty void; value i32; };
type Holder = struct { u Choice; };
fn main() int {
    let h Holder = Holder.{ u = Choice.value(5) };
    return 0;
}
```

`go run ./cmd/pebc -run <file.peb>` fails with a CLEAN Pebble-level
rejection (not a `cc` failure, unlike #57's bug): `entry function body
expression constructs enum variant symbol 26 with 1 payload(s); a
tagged-union (union enum) construction routes through
buildUnionConstruction, never a plain enum value`.

**Root cause.** `buildStructBraceList`
(`compiler/internal/backend/aggregates.go`, its per-field-type switch)
has a `case isEnumType(unit, snapshot, fieldType):` that unconditionally
routes the field's construction value to `buildEnumValue` — but
`isEnumType` is true for BOTH a plain enum AND a tagged union (this
codebase's own established convention, confirmed repeatedly this
session — a tagged union is enum-shaped, checked separately from
struct-shaped). `buildEnumValue` explicitly rejects any payload-carrying
variant by design (it's ONLY for plain enums). There is NO
`isTaggedUnionType` case checked BEFORE the `isEnumType` case in this
switch — unlike the ALREADY-FIXED sibling pattern in
`buildOptionalValueExpr`/`buildOptionalLocalDeclaration` (task #55),
where the tagged-union case correctly precedes the enum case and
routes to `buildUnionValueExpr`.

**Scope:** add a `case isTaggedUnionType(unit, snapshot, fieldType):`
to `buildStructBraceList`'s switch, positioned BEFORE the existing
`case isEnumType(...)` (mirroring the exact ordering
`buildOptionalValueExpr` already uses), routing to
`buildUnionValueExpr(st, unit, snapshot, fileSet, field.Value, scope,
context, fieldType, width)` — the same function #55 used for an
optional's tagged-union payload. Verify the reproduction above compiles
and runs. Verify a payload-LESS tagged-union variant as a struct field
(`Holder.{ u = Choice.empty }`) also works. Verify the existing
plain-enum struct field paths (task #18, task #57) are completely
unaffected. Verify a tagged-union LOCAL declaration and a tagged-union
call argument/return (if those already work — confirm before assuming)
are unaffected, since this fix only touches the struct-field
construction switch.

**Resolution (`e3478af`, 2026-08-11).** Verification revealed a
SECOND, compounding gap needed alongside the scoped builder fix:
`collectUnionTypesWalk` had no `RecordConstruct` case (the identical
"Fields isn't in Children" gap `collectEnumTypesWalk` got fixed for in
`d19717c`) — without it, the union's typedef pair was never collected
when only reachable through a struct field's construction, and the
field declaration named an undeclared C type even after the builder
fix. Both fixes landed together, independently causation-checked as
each individually necessary (reverting either one alone reproduces a
distinct failure — the original clean rejection, or a `cc` "unknown
type name" failure). **Also confirmed: the tracker's own literal
reproduction (`value i32`) hits a SEPARATE, pre-existing, orthogonal
limitation** — this backend's tagged-union payloads must be exactly
int/bool/str (documented gates in `collect.go`/`types.go`), confirmed
identical for a union LOCAL of the same shape on unmodified `HEAD`, so
NOT part of this defect. Verified instead with the repo's own
established `value int` convention (matching every existing union
test): the payload-carrying case round-trips through construction,
storage, and a narrowing switch; the payload-less sibling
(`Holder.{ u = Choice.empty }`) correctly routes through
`buildUnionValueExpr`'s `EnumVariantValue` case; plain-enum struct
fields and tagged-union locals/arguments/returns are confirmed
unaffected. Full suite (`go test ./... -count=1 -timeout 600s
-parallel 16`, 11 packages) clean, `gofmt`/`go vet` clean.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: a bare `sizeof (T,U)` or `sizeof ?T` (a tuple/optional type
with NO array wrapper) is rejected — the array-element case was
fixed (task #52), but the direct case never was.**

Sourced from task #52's own verification notes (2026-08-11), not from
Sol's audit. Independently reproduced and root-caused before dispatch
(this session, prior to dispatching this task).

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    return (sizeof (int,int)) as int;
}
```

and

```
fn main() int {
    return (sizeof ?int) as int;
}
```

`go run ./cmd/pebc -run <file.peb>` fails at the C COMPILER stage for
both: `use of undeclared identifier 'pebble_tuple_23_t'` (tuple case)
and `use of undeclared identifier 'pebble_optional_23_t'` (optional
case) — Pebble's own emission succeeds; `pebc` even lowers the
expression to `(int32_t)(sizeof(pebble_tuple_23_t))`, but the tuple's
own typedef is never collected/emitted.

**Root cause.** `collectTupleTypesWalk` and `collectOptionalTypesWalk`
(`compiler/internal/backend/collect.go`) each have EXACTLY ONE
`SizeofType` case — added in task #52 — that fires only when
`node.TypeArg` is an ARRAY type whose ELEMENT is a tuple/optional
(`sizeof [N](int,int)`). Neither has a case for `node.TypeArg` being
the tuple/optional type DIRECTLY (a bare `sizeof (int,int)` with no
array wrapper at all, no other reference to that tuple/optional type
anywhere in the program). This mirrors exactly the pattern
`collectStructTypesWalk`'s bare-struct `SizeofType` case already
handles (`isStruct(snapshot, node.TypeArg)`, checked BEFORE the
array-element case) — that sibling case is the model to follow.

**Scope:** add a direct-match `SizeofType` case to BOTH
`collectTupleTypesWalk` (`isTuple(snapshot, node.TypeArg)`) and
`collectOptionalTypesWalk` (`isOptional(snapshot, node.TypeArg)`),
each appending `node.TypeArg` itself — positioned alongside (not
replacing) the existing array-element case each function already has
from task #52. Verify both reproductions above compile and run.
Verify the existing array-element `sizeof [N](int,int)`/`sizeof
[N]?int` shapes (task #52) are completely unaffected. Verify a bare
`sizeof Struct` (the sibling case this mirrors) is unaffected. Verify
a bare `sizeof T` for a primitive type (already working) is
unaffected.

**Resolution (`392ae16`, 2026-08-11).** Added the direct-match case
to both walks exactly as scoped, positioned alongside (not replacing)
the existing array-element case — the two guards are disjoint by
construction, so both coexist correctly. Verified: both reproductions
compile and run (exit 8 each); the array-element shapes from task #52
and a bare `sizeof Struct`/`sizeof int` are unaffected; a spot-check
of a nested `sizeof (?int, int)` also works correctly (exit 12),
confirming the fix generalizes across combined aggregate shapes. Full
suite (`go test ./... -count=1 -timeout 600s -parallel 16`, 11
packages) clean, `gofmt`/`go vet` clean, causation check confirmed
reverting reproduces the exact original `cc` failure.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: an enum-typed struct field's construction fails — the
enum's typedef and variant constant are never collected when the
field's construction value is only reachable through
`RecordConstruct.Fields`.**

Discovered during task #55's verification (2026-08-11), not from
Sol's audit. P1-equivalent (a clean, real backend gap with a precise
mechanical fix, mirroring three already-fixed sibling collectors).

**Reproduction** (confirmed against current HEAD):

```
type Color = enum { red, green, blue };
type Holder = struct { c Color; };
fn main() int {
    let h Holder = Holder.{ c = Color.blue };
    return 0;
}
```

`go run ./cmd/pebc -run <file.peb>` fails at the C COMPILER stage:
`unknown type name 'pebble_enum_19_t'` and (in the same output) `use
of undeclared identifier 'pebble_variant_27'` — both the enum's
typedef AND its variant constant are missing from the emitted C.

**Root cause.** `RecordConstruct`'s field values live in a SEPARATE
`node.Fields []FieldInit` slice, not the generic `node.Children` every
collection walk's default recursion follows (confirmed pattern from
this session's earlier fixes, e.g. task #52). Three sibling type
collectors already have a dedicated `case tir.RecordConstruct:` in
`compiler/internal/backend/collect.go` that explicitly recurses into
`field.Value` for exactly this reason:
`collectStructTypesWalk` (own struct field, per its own comment
citing this same "Fields isn't in Children" pattern),
`collectOptionalTypesWalk` (commit `8c339d3`), and
`collectFunctionTypesWalk` (commit `0b6ed32`). `collectEnumTypesWalk`
has NO `RecordConstruct` case at all — a plain enum value used ONLY
as a struct field's construction value (`Holder.{ c = Color.blue }`,
no other reference to `Color` anywhere in the program) is never
visited by the walk, so neither the enum's typedef nor its
`EnumVariantValue`'s variant constant get collected.

**Independently spot-checked and confirmed narrow:** the identical
gap does NOT reproduce for a TUPLE-typed struct field constructed the
same way (`Holder.{ t = (1, 2) }` compiles and runs fine today) — so
this is genuinely enum-specific, not a broader pattern needing a
matching fix across every collector. Do not expand scope to other
collectors without first reproducing a real failure for that shape.

**Scope:** add a `case tir.RecordConstruct:` to `collectEnumTypesWalk`
that iterates `node.Fields` and recursively walks
`collectEnumTypesWalk(unit, snapshot, field.Value, out)` for each
field — mirroring `collectStructTypesWalk`'s own `RecordConstruct`
case exactly (same iteration pattern, same recursive self-call).
Verify the reproduction above compiles and runs. Verify a plain enum
value used ONLY as a NESTED struct field's construction value (a
struct-within-a-struct) also works, if practical. Verify existing
enum-typed struct field paths (already-working shapes — an enum field
read, assignment, comparison from task #18, and an enum-typed local
declaration) are completely unaffected. Verify a tagged-union-typed
struct field constructed via `VariantConstruct` (not `RecordConstruct`
— a different node kind) is unaffected, since this fix only touches
the `RecordConstruct` case.

**Resolution (`d19717c`, 2026-08-11).** Added the `RecordConstruct`
case to `collectEnumTypesWalk` exactly as scoped, mirroring the three
sibling collectors precisely. Verified: the reproduction compiles and
runs; a nested struct-within-a-struct construction round-trips the
enum value correctly (exit 42); the pre-existing enum field read/
assign/compare paths and enum-typed locals are unaffected. A
tagged-union struct field via `VariantConstruct` was independently
checked and causation-checked to fail IDENTICALLY before and after
this fix — a separate, still-open, pre-existing gap (noted above),
correctly left untouched rather than folded in. Full suite (`go test
./... -count=1 -timeout 600s -parallel 16`, 11 packages) clean,
`gofmt`/`go vet` clean, causation check confirmed reverting reproduces
the exact original `cc` failure (both the missing typedef and the
missing variant constant).
-->

<!-- Previous item, resolved 2026-08-11:

**Item: a first-class function type's narrow-width
(u8/u16/i8/i16/u32/i64) PARAMETER passes signature validation, then
its C typedef builder rejects it — a genuine validator/builder
inconsistency, not a deliberate narrower grammar.**

Sourced from proposal 14's backend gap matrix (`First-class narrow
integer function signature`, line 79), P1. Independently reproduced
and root-caused before dispatch. Scoped to the PARAMETER side only —
the RESULT side (`fn() u8`) was independently checked and confirmed to
reject CONSISTENTLY at validation, matching its own documented,
deliberately narrower design (the indirect-call result-consumption
positions this backend can lower into are genuinely limited) — that
is NOT a bug and is explicitly out of scope here.

**Reproduction** (confirmed against current HEAD):

```
fn add_one(x u8) int {
    return x as int + 1;
}
fn main() int {
    var f fn(u8) int = add_one;
    return f(5);
}
```

`go run ./cmd/pebc -run <file.peb>` fails with: `function type
parameter type u8 is not supported, want int, uint, bool, char, str,
or a pointer type`.

**Root cause.** `validateFunctionTypeSignature`
(`compiler/internal/backend/validate.go:306-343`) admits ANY fixed-
width integer parameter, resolved at ITS OWN width (line 334:
`paramWidth, integerParam := resolvedBuiltin(snapshot, parameter);
if !(integerParam && cType(paramWidth) != "") && ...` — this is
deliberately width-independent per the function's own doc comment:
"independent of the ambient width of the context the function type is
being validated from"). But `functionTypeParamCType`
(`compiler/internal/backend/types.go:1528-1557`, the function that
resolves each parameter's actual C type for the typedef) only has a
`case isWidth(snapshot, width, param):` — matching ONLY when the
parameter's width equals the AMBIENT `width` argument — with no
general "any other fixed-width integer, resolved at its own width"
case. A u8 parameter validated successfully (since u8 is a fixed-width
integer with a valid `cType`) then falls through
`functionTypeParamCType`'s switch to its final rejection at line 1556,
since `isWidth(snapshot, width, param)` is false whenever the entry's
ambient width isn't u8 specifically.

**Scope:** widen `functionTypeParamCType`'s width-matching case to
resolve ANY fixed-width integer parameter at its OWN width, not just
one matching the ambient width — replace (or add alongside)
`case isWidth(snapshot, width, param): return cType(width), nil` with
a general case using `resolvedBuiltin(snapshot, param)` +
`cType(paramWidth)` (mirroring the exact pattern
`validateFunctionTypeSignature` itself already uses to VALIDATE this
shape, and the pattern this codebase uses pervasively elsewhere — e.g.
`buildArrayBraceElements`'s "each element is built at the element's
own resolved width," `buildCallArgument`'s per-argument-own-width
resolution). Verify the reproduction above compiles and runs
(returning 6: `5 as int + 1`). Verify each of the other affected
widths (u16, i8, i16, u32, i64 — everything `validateFunctionTypeSignature`
already accepts but `functionTypeParamCType` currently can't build)
also works with its own small reproduction. Verify the existing
ambient-width, uint/u64, bool, char, str, and pointer parameter shapes
are completely unaffected. Verify the RESULT side (`fn() u8`) is
UNCHANGED — still cleanly rejects, do not touch
`functionTypeResultCType` or `validateFunctionTypeSignature`'s result
check.

**Resolution (`6fd44d2`, 2026-08-11).** Fixed exactly as scoped:
`functionTypeParamCType`'s ambient-width-only case replaced with a
general fallback resolving any fixed-width integer parameter at its
own width. Verified: the reproduction and each of the other five
narrow widths (u16, i8, i16, u32, i64) compile and run correctly, each
with a distinct expected value; the emitted C was inspected directly
and confirmed the fnptr typedef, the hoisted helper, and the indirect
call site all agree on the parameter's own C type (`uint8_t` for the
u8 case); the existing uint/char/str/ambient-width parameter shapes
are unaffected; the RESULT side (`fn() u8`) is confirmed still
rejected with its original, unchanged message via a dedicated
regression test. Full suite (`go test ./... -count=1 -timeout 600s
-parallel 16`, 11 packages) clean, `gofmt`/`go vet` clean, causation
check confirmed reverting reproduces the exact original rejection.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: `some Color.blue` (an ordinary enum-variant literal as an
optional's payload) is rejected — only an integer-to-optional-enum
cast works.**

Sourced from proposal 14's backend gap matrix (`Ordinary optional
enum construction`, line 78/322), P1. Independently reproduced and
root-caused before dispatch.

**Reproduction** (confirmed against current HEAD):

```
type Color = enum { red, green, blue };
fn main() int {
    let value ?Color = some Color.blue;
    return 0;
}
```

`go run ./cmd/pebc -run <file.peb>` fails with: `entry function body
block declares an optional-typed local of type pebble_optional_24_t
initialized from some with an enum payload pebble_enum_19_t; the only
supported enum-payload optional initializer is an integer-to-optional-enum
cast (e.g. 5 as ?Color)`.

**Root cause.** Two separate builders each handle a `SomeOptional`/
`OptionalInject` payload by switching on the payload's type, and BOTH
are missing a plain-enum case:

1. `buildOptionalLocalDeclaration`
   (`compiler/internal/backend/locals.go:544-`, the `some`-payload
   `switch` starting ~line 567) has a `case isTaggedUnionType(...)`
   (line 596, correctly calling `buildUnionValueExpr`) immediately
   followed by `case isEnumType(unit, snapshot, payloadType):` (line
   613) — but THIS case doesn't build a value at all, it just
   `return`s the clean-rejection error (line 619). This is a
   DELIBERATE, explicit rejection (there's an explanatory comment
   right above it), not a missing-case fallthrough — someone
   intentionally left this unimplemented, presumably because the
   integer-to-optional-enum cast path shipped first and covered the
   immediate need.
2. `buildOptionalValueExpr` (`compiler/internal/backend/values.go:312-365`,
   used for OTHER optional-value positions — struct-field
   construction, call arguments, wherever a fresh optional value is
   built inline, not just a local declaration) has the SAME shape of
   switch (line 330-360) with a `case isTaggedUnionType(...)` (line
   339) but NO enum case at all — a plain-enum payload falls through
   to the generic `default: return "", fmt.Errorf("%s optional
   payload %s is unsupported", ...)` at line 358-359.

Both switches already have the exact machinery to fix this: plain enum
values are built elsewhere by `buildEnumValue`
(`compiler/internal/backend/values.go:457+`), the same function an
enum-typed local's declaration, a struct field, etc. already use. The
signature is `buildEnumValue(st, unit, snapshot, fileSet, id, locals,
width)` — an exact match for what each switch case needs to call with
`node.Children[0]`/`initValue.Children[0]` as `id`.

**Scope:**
1. In `buildOptionalLocalDeclaration`
   (`compiler/internal/backend/locals.go`), replace the
   `case isEnumType(unit, snapshot, payloadType):` body (currently just
   the rejection) with a call to `buildEnumValue(st, unit, snapshot,
   fileSet, initValue.Children[0], scope, width)`, assigning the
   result to `valueExpr` exactly like the `isTaggedUnionType` case
   immediately above it does.
2. In `buildOptionalValueExpr` (`compiler/internal/backend/values.go`),
   add a `case isEnumType(unit, snapshot, payload):` (positioned after
   the existing `isTaggedUnionType` case, before the `default`) that
   assigns `value, err = buildEnumValue(st, unit, snapshot, fileSet,
   node.Children[0], scope, width)`.
3. Verify the reproduction above compiles and runs. Verify a plain
   enum payload works in EVERY position `buildOptionalValueExpr`
   serves — check which real source shapes actually reach it (a
   struct field of optional-enum type constructed with `some`, an
   optional-enum call argument, an optional-enum return, if
   reachable) and test at least one beyond the local-declaration
   reproduction. Verify the existing integer-to-optional-enum cast
   path (`5 as ?Color`) is completely unaffected. Verify a `none`
   optional-enum initializer is unaffected (a different code path,
   the `NoneOptional` branch, not touched by this fix). Verify a
   tagged-union payload (the case immediately preceding the one
   you're adding in both functions) is completely unaffected.

**Resolution (`1bf785d`, 2026-08-11).** Fixed exactly as scoped in
both `buildOptionalLocalDeclaration` and `buildOptionalValueExpr`.
Verified: local declaration, call argument, and helper return position
all compile and run correctly for `some Color.blue`; the
integer-to-optional-enum cast path is unaffected; the tagged-union
case is unaffected. The struct-field position was independently
checked and found to fail — but with a DIFFERENT, pre-existing error
(`unknown type name pebble_struct_..._t`/`pebble_enum_..._t`) that
reproduces identically for a bare, non-optional enum struct field
(`Holder.{ c = Color.blue }`, no `some`/optional involved at all), so
it's unrelated to this fix and was correctly left alone rather than
folded in — noted above as a fresh discovery for a future item. Full
suite (`go test ./... -count=1 -timeout 600s -parallel 16`, 11
packages) clean, `gofmt`/`go vet` clean, causation check confirmed
reverting reproduces the exact original rejection.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: force-unwrapping a narrow-width (u8/u16/i8/i16/u32) optional
has no runtime helper — a genuine runtime-coverage gap, not a
backend-only bug like the last several items.**

Sourced from proposal 14's integer runtime coverage matrix (`Narrow
optional unwrap`, line 77/400), P1. Independently reproduced and
root-caused before dispatch. Unlike #47-53 (all backend Go-only), this
one needs REAL RUNTIME C ADDITIONS in `runtime/src/optional.c` and
`runtime/include/pebble_rt.h` (repo root `runtime/`, NOT under
`compiler/`), plus a small backend Go change to route to them.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    var x ?u8 = some 42;
    return x! as int;
}
```

`go run ./cmd/pebc -run <file.peb>` already rejects CLEANLY at
`pebc`'s own emission stage (not a `cc` failure, unlike #53's bug):
`entry function body integer cast child: entry function body
expression contains a CheckedOptionalUnwrap of a u8 payload, which has
no runtime unwrap helper`. `?u8` storage and `some 42` construction
both already work fine — only the force-unwrap (`!`) operation is
missing.

**Root cause.** `optionalUnwrapSuffix`
(`compiler/internal/backend/operators.go:210-234`) maps a payload's
resolved width to a `pebble_rt_checked_unwrap_<suffix>` helper suffix,
but its `switch` only covers `types.Int/I32 → "i32"`,
`types.I64 → "i64"`, and `types.Uint/U64 → "u64"` (plus separate
`isBool`/`isPointer` branches above the switch) — every other integer
width (u8, u16, i8, i16, u32) falls through to `return ""`, a clean
rejection. This is NOT a bug in the suffix-resolution logic itself
(unlike #53's missing-guard bug) — it's simply that the runtime C
helpers for these five widths were never written.
`runtime/src/optional.c` currently has exactly five checked-unwrap
helpers: `pebble_rt_checked_unwrap_i32/i64/bool/u64/ptr`, each an
identical three-line pattern (`if (!has_value) { panic(loc); } return
value;`) differing only in the C parameter/return type. The optional
struct's own `.value` field ALREADY correctly declares the payload at
its narrow C type (`optionalPayloadCType`,
`compiler/internal/backend/types.go:1448-1491`, confirmed via
`cType(payloadWidth)` — e.g. `uint8_t` for a `u8` payload) — the
struct layout is not the problem, only the missing unwrap helper.

**Scope:**
1. In `runtime/src/optional.c`, add five new helpers following the
   EXACT existing pattern (same panic call, same shape), one each for
   u8/u16/i8/i16/u32, using the confirmed C types
   (`compiler/internal/backend/types.go`'s `cType` function pins
   these): `uint8_t pebble_rt_checked_unwrap_u8(bool has_value,
   uint8_t value, PebbleSourceLoc loc)`, and the u16/i8/i16/u32 twins
   with `uint16_t`/`int8_t`/`int16_t`/`uint32_t` respectively.
2. Declare all five in `runtime/include/pebble_rt.h` alongside the
   existing five declarations (same header, same style).
3. In `compiler/internal/backend/operators.go`'s `optionalUnwrapSuffix`,
   add the five missing width cases to the switch (`types.U8 → "u8"`,
   `types.U16 → "u16"`, `types.I8 → "i8"`, `types.I16 → "i16"`,
   `types.U32 → "u32"`), mirroring the existing `types.I64 → "i64"`
   case exactly.
4. Verify the reproduction above compiles and runs, returning 42.
   Verify each of the other four widths (u16, i8, i16, u32) also
   works with its own small reproduction. Verify the PANIC path: a
   `none`-valued optional force-unwrapped at each of these widths
   panics (does not silently return garbage) — follow whatever
   existing test convention proves this for i32/i64/bool/u64 today
   (search `compiler/internal/backend/*_test.go` and
   `runtime/test/smoke_test.c` for the existing unwrap-panic tests as
   the structural model).
5. Verify the existing i32/i64/bool/u64/ptr unwrap paths are
   completely unaffected.
6. Check whether `runtime/test/smoke_test.c` needs matching entries
   for the five new helpers (it currently exercises the existing five
   directly — `test_checked_unwrap_normal`/panic tests around line
   698-726) and add them if that's this repo's established
   convention for new runtime helpers.

**Resolution (`9426382`, 2026-08-11).** Added the five runtime
helpers, header declarations, backend switch cases, and matching
`smoke_test.c` entries exactly as scoped. Verified: all five widths
compile and run for `some`-initialized optionals; all five panic
(process abort via `pebble_rt_panic`) for a `none`-initialized force
unwrap; runtime smoke test passes in both SAFE and RELEASE mode; the
existing i32/i64/bool/u64/ptr paths are unaffected. **Independent
full-suite verification caught a real regression the dispatched
worker's own narrower `-run` test filter (`TestEmitOptional*`) missed:
a pre-existing test, `TestEmitRejectsOptionalUnwrapOfU8Payload`
(`validate_test.go`), pinned the OLD "has no runtime unwrap helper"
rejection specifically for u8 and started failing once u8 became
supported.** Converted it (renamed
`TestEmitOptionalUnwrapOfU8PayloadCompilesAndRuns`) to assert the new
correct compile-and-run behavior instead of deleting it, following
this project's established convention for a stale-rejection test
(mirrors #47's `TestEmitRejectsTupleNestedMoreThanOneLevel`
conversion). Full suite (`go test ./... -count=1 -timeout 600s
-parallel 16`, 11 packages) clean after the correction, `gofmt`/`go
vet` clean, causation check (reverting all three fix files together)
confirmed reverting reproduces the exact original rejection.
-->

<!-- Previous item, resolved 2026-08-11:

**Item: a narrow-width (u8/u16/i8/i16/u32/uint) checked arithmetic
binary expression emits a call to a nonexistent, empty-suffix runtime
helper instead of a clean rejection.**

Sourced from proposal 14's integer runtime coverage matrix (`Narrow
checked arithmetic`, line 76), P1. Independently reproduced and
root-caused before dispatch. Small, narrow fix — one missing guard,
matching a pattern the sibling shift-helper function already has.

**Reproduction** (confirmed against current HEAD):

```
fn main() int {
    var a u8 = 200;
    var b u8 = 100;
    var c u8 = a + b;
    return c as int;
}
```

`go run ./cmd/pebc -run <file.peb>` fails at the C COMPILER stage (not
Pebble's own emission): `error: call to undeclared function
'pebble_rt_checked_add_'` — note the TRAILING UNDERSCORE with no
suffix at all.

**Root cause.** `checkedArithmeticHelper`
(`compiler/internal/backend/operators.go:96-116`) builds the helper
name as `base + "_" + checkedSuffix(width)` UNCONDITIONALLY and always
returns `ok=true` (except for an unmapped operator). `checkedSuffix`
(`operators.go:236-248`) returns `""` for any width it doesn't
recognize (u8, u16, i8, i16, u32, uint — everything except
int/i32/i64/u64), so a narrow-width add/sub/mul/div/mod silently
produces `pebble_rt_checked_add_` (empty suffix) instead of being
rejected. The caller in `values.go:1967-1970` (`buildExpr`'s
`CheckedArithmetic` case) DOES correctly check the returned `ok` and
reject cleanly — but since `checkedArithmeticHelper` always reports
`ok=true`, that check never fires for this shape. The sibling
`checkedShiftHelper` (`operators.go:118-140`) already has the correct
pattern: it computes its own suffix via `checkedShiftSuffix`, checks
`if suffix == "" { return "", false }`, and only then builds the
helper name — `checkedArithmeticHelper` is simply missing this same
guard.

**Note: the compound-assignment path (`a += b`) is NOT affected** —
`buildCompoundIntegerCore` (`compiler/internal/backend/stores.go:996-1009`)
has its own EARLIER explicit `checkedSuffix(placeWidth) == ""` guard
before it ever calls `checkedArithmeticHelper` (whose own `ok` it then
discards with `_` — safe, since the guard already filtered the empty
case), so `a += b` at u8 already rejects cleanly today with a
Pebble-level error, not a `cc` failure. Confirm this yourself before
assuming it needs fixing too — only the PLAIN BINARY EXPRESSION path
(`a + b`, not `a += b`) has the bug.

**Scope:** add the same empty-suffix guard to
`checkedArithmeticHelper` that `checkedShiftHelper` already has:
compute `checkedSuffix(width)` into a local, check if it's empty, and
return `("", false)` before building the helper name. Verify: the
reproduction above now rejects CLEANLY (a Pebble-level emission error,
not a `cc` failure) — check the exact error message text produced and
confirm it's informative, adjusting only if the existing generic
`values.go:1969` error message needs it (it currently reads
reasonably already: "want +, -, *, /, or %% (at u64, only +, -, and *
have a checked runtime helper)" — this may need a small wording
addition since narrow widths are now ALSO excluded, not just some u64
operators; use your judgment on whether the message needs updating).
Verify the existing i32/i64/u64 (add/sub/mul) and i32/i64 (div/mod)
checked arithmetic paths are completely unaffected. Verify the
compound-assignment path (`a += b` at u8) is unaffected (still
rejects with its own existing, different, already-correct message).
Verify checked shift, checked negation, and float-to-integer
conversion (separate helper functions, not touched by this fix) are
unaffected.

**Resolution (`73bfbb1`, 2026-08-11).** Added the same empty-suffix
guard `checkedShiftHelper` already had. Also widened the
`CheckedArithmetic` rejection message (`values.go:1969`) to name the
actual offending width instead of unconditionally mentioning u64.
**Correction to this item's own listed affected widths:** `uint`
turned out NOT to be affected — `buildUintExpr` lowers uint-typed
`CheckedArithmetic` to plain C arithmetic and never calls
`checkedArithmeticHelper` at all, discovered and confirmed during
independent verification. The genuinely affected widths are
u8/u16/i8/i16/u32. Verified: the reproduction now rejects cleanly at
`pebc`'s own emission stage (`entry function body expression contains
a CheckedArithmetic with operator + at u8, want an operator with a
checked runtime helper...`), not a `cc` failure; the compound-assignment
path (`a += b` at u8) is unaffected, still rejecting with its own
distinct, pre-existing message; existing i32/i64/u64 checked
arithmetic, checked shift, checked negation, and float-to-integer
conversion are all unaffected. Full suite (`go test ./... -count=1
-timeout 600s -parallel 16`, 11 packages) clean, `gofmt`/`go vet`
clean, causation check confirmed reverting reproduces the exact
original `cc` failure.
-->

<!-- Previous item, resolved 2026-08-11:

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

**Resolution (`cf97cd3`, 2026-08-11).** Confirmed as a
missing-collection bug, not an ordering bug, exactly as hypothesized.
Added a `SizeofType`-with-array-`TypeArg` case to
`collectStructTypesWalk`, and — since the identical shape was
confirmed to affect tuple and optional array elements too, and was
trivially fixable alongside — the same case to
`collectTupleTypesWalk` and `collectOptionalTypesWalk`. An
enum-element array under `sizeof` hits a separate, pre-existing,
apparently intentional rejection ("enum-typed array elements are not
supported yet") and was correctly left alone. A bare `sizeof (T,U)`/
`sizeof ?T` with no array wrapper is a broader, separate, still-open
gap (those two walks have no `SizeofType` case at all for a
non-array `TypeArg`) — independently spot-checked and confirmed real,
noted above as a follow-up, not folded into this fix. Verified: the
reproduction returns 16; `sizeof [2](int,int)` and `sizeof [2]?int`
both now work, also returning 16; the emitted C was inspected
directly and confirmed the struct typedef is placed before the array
typedef that references it; `sizeof Point` (bare, no array) and
`sizeof [3]int` (array of a primitive) are unaffected. Full suite
(`go test ./... -count=1 -timeout 600s -parallel 16`, 11 packages)
clean, `gofmt`/`go vet` clean, causation check confirmed reverting
reproduces the exact original `cc` failure.
-->

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
