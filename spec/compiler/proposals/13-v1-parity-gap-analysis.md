# 13 — v1 parity gaps

**Purpose.** This file is the working area for exactly one active defect.
Proposal 14 is the full parity ledger and the open backlog. Closed work does
not stay in this file.

## Workflow

1. Pick one open item from proposal 14's fifth-pass table.
2. Reproduce it with the current compiler.
3. Record its minimal source, exact failure, root cause, and scope below.
4. Dispatch one small implementation slice through Orc.
5. Review the diff, remove scratch work, run `gofmt`/`go vet` plus the
   package(s) actually touched, and do a causation check. **Do not run the
   full backend/repo test suite per item** — it's a periodic checkpoint
   (roughly every 5 items), not a per-item gate; use `-parallel 12` when a
   checkpoint run does happen, to reduce contention-driven flaky loop/break
   failures.
6. Commit and push the verified fix. Update proposal 14 with the closing
   commit, then clear the active defect from this file.
7. Do not start another implementation worker until the current worker has
   finished and the worktree is clean.

## Dispatch rules

- Use `orc run --claude` when dispatching as Claude (this assistant); use the
  attribution flag matching whichever assistant is actually dispatching.
- Alternate `opencode-go/deepseek-v4-flash` and `vercel/alibaba/qwen3.7-flash`
  per dispatch for routine/mechanical tasks.
- `opencode-go/mimo-v2.5` and `opencode-go/kimi-k2.7-code` are banned.
- Use `openai/gpt-5.6-luna` only after a real failure from the primary
  rotation, unless the user explicitly selects Luna.
- Never use `openai/gpt-5.6-sol`.
- Do not use `openai/gpt-5.6-terra` without user approval.
- Resume a stalled session (`orc run --session <id>`) rather than
  re-dispatching fresh. **`orc delete`/`--clean`/`--delete` is permanently
  banned for any session, any status** — never run it. If a "failed" session's
  independently-verified diff is actually correct, run `orc complete <id>`
  instead; this only corrects the status field and destroys no data.
- Keep implementation tasks sequential. Parallel workers are permitted only
  for read-only work when the user explicitly requests them.
- Before an implementation dispatch, require a clean worktree and no active
  Orc or OpenCode worker for this repository.
- After a dispatch, inspect the real diff. Check for scratch files, debug
  output, scope growth, and stale tests. Do not trust the worker summary.

## Active defect

*(empty — F5-06 (interpolation of a `char` value part) closed in
`170ff96`. The scalar is UTF-8-encoded via the existing
`pebble_rt_char_to_utf8`, reused from the char-print path. A real
NUL-char (Unicode scalar 0) regression surfaced during verification:
the write pass's first draft reused the int/float cases' `strlen`-
based copy, which returns 0 for a buffer whose first byte is `0x00`,
silently dropping the character and desyncing every later part's
offset — fixed by encoding directly into the destination and using
the encoder's own return value as the byte count, not `strlen`. Two
follow-up dispatches were needed: one to fix the NUL bug itself (a
session that stalled mid-edit left a genuinely broken, uncompilable
`#if 0` with no `#endif` in `str.c` — caught by grep, not trusted from
the "completed" report, fixed with a second tiny dispatch), and one to
rescope 3 Go test cases that asserted on `print` OUTPUT of a
NUL-containing string — `print`'s C emission
(`fprintf(..., "%s", ...)`) truncates at any embedded NUL for ANY
`str` value, confirmed independent of interpolation or char work
entirely (`print "x\0y";` already truncates on plain `HEAD`); rescoped
those 3 cases to assert on `.len` materialization instead (which IS
correct), and logged the print-NUL-truncation limitation as its own
new backlog row (F5-06b) rather than trying to fix it here. Full
`internal/backend` checkpoint hit the same known rotating loop/while
flakiness at `-parallel 12` (exit -1, unrelated tests); confirmed
flaky by isolation rerun, clean at `-parallel 4`. Causation-checked
via file-copy swap against HEAD.

*(empty — F5-07 (interpolation of a plain-enum value part) closed in
`cd5e3c6`. A plain enum's formatted representation depends on a
runtime tag comparison across N static strings, so it can't become a
single inline `PebbleStrPart` entry; `buildEnumInterpolationSwitch`
emits a pre-statement C switch assigning a temp `PebbleStr` per
variant (reusing F5-05's `PEBBLE_STR_PART_STR` machinery — no new
runtime code), and reuses the existing enum-print naming helpers
(`enumSourceName`/`variantSourceName`/`enumVariantName`) so an
interpolated enum renders identically to the same enum passed straight
to `print`. `buildInterpolatedStringParts`'s signature grew a `[]string`
pre-statements return, threaded through all 3 call sites (local decl,
general expression, and both of `buildPrint`'s combined/sequential
paths). A tagged union (payload-carrying enum) is explicitly rejected
with a clear error, confirmed by test — not silently mishandled.
Also fixed a real collection gap found during this work:
`collectEnumTypesWalk` only followed `node.Children`, missing an enum
referenced only via an interpolation's `node.Parts` (e.g.
`` `pick={Color.green}` ``), leaving its typedef/variant constants
uncollected — the same Parts-not-Children shape `collectDirectCalls`
already closes for helper calls used as interpolated values.

*(empty — F5-08 (interpolation of a non-nested struct value part)
closed in `7696630`. One struct value part expands into MULTIPLE
`PebbleStrPart` entries directly in `buildInterpolatedStringParts`'s
own `parts` array (a text label per field boundary, then whichever
entry kind matches each field's own type, reusing the exact same
per-type dispatch this function already had for bool/int/float/str/
char/plain-enum) — no new runtime code, no new `PebbleStrPartKind`.
`structSourceName`/`fieldSourceName` (the same helpers the bare-print
path uses) keep an interpolated struct identical to the same struct
passed straight to `print`. A struct/tuple/array/untagged-union field
is cleanly rejected — nested-aggregate recursion out of scope. THIS
ITEM NEEDED TWO VERIFICATION-CAUGHT FOLLOW-UP FIXES, both real bugs
neither session's own "completed"/"failed" report caught:
1. A `field.member`/`fieldSourceName` naming mismatch — the new code
   used `field.name`/`field.symbol`, but `structFieldInfo` only has
   `.member` (a `symbol.SymbolID`); the source name needed a call to
   the existing `fieldSourceName(unit, fileSet, field.member)` helper.
   This failed to COMPILE, caught by `go build`, not by the session's
   own report (which claimed success).
2. A `len(node.Parts)` vs `len(parts)` count mismatch — the function's
   final `return` used `len(node.Parts)` (the ORIGINAL interpolation
   part count) as the array-length argument to
   `pebble_rt_str_from_parts`, which was correct for every PRIOR part
   kind (each expands 1:1 into exactly one `parts` entry) but wrong
   for struct (one part expands into N entries), silently truncating
   the output mid-struct. Caught by piping the repro's actual output
   through `od -c` and finding a stray control byte, not by the
   session's own report (which claimed success). Fixed by using
   `len(parts)` (the actual built slice) instead.
Lesson for future multi-entry-expansion part kinds (if any): always
re-verify byte-exact output via `od -c`, not just exit code — a
truncation bug can still exit 0.

*(empty — F5-09 (interpolation of a non-nested tuple value part)
closed in `8b7d057`, the final item in the interpolation-value-part
matrix (F5-01 through F5-09 all done). Mirrored F5-08's struct
pattern almost exactly: one tuple value part expands into multiple
`PebbleStrPart` entries (`"("`, each element's own entry, `", "`
between, `")"`, a trailing `,` for a single element) directly in
`buildInterpolatedStringParts`'s array, reusing the same per-type
dispatch and `buildTuplePrintValueCalls`/`Operand`/`ValueExpr`'s
naming so an interpolated tuple matches bare print exactly. A
struct-typed tuple element does NOT fall out naturally (the struct
case resolves fields off a source node, incompatible with a tuple's
ordinal `.0`/`.1` access), so it's cleanly rejected along with tuple/array
elements — verified with extra rigor per F5-08's lesson (`go build`
confirmed clean, byte-exact `od -c` output confirmed, the `len(parts)`
count line confirmed unchanged) and landed clean on the first dispatch,
no follow-ups needed this time.

*(empty — F5-10 (aggregate `ArrayRepeat` call argument) closed in
`cea6231`. Both of `buildArrayArgument`'s element-building switches
(the full `ArrayValue` literal case, which already had an `isArray`
nested-array precedent, and the `ArrayRepeat` `[v; N]` case, which
only handled bool/int/float) gained an `isStruct` branch using
`buildNestedAggregateValue`, preserving the `ArrayRepeat` case's
evaluate-once/copy-N-times pattern exactly. `buildStructValueExpr`
also gained a `DirectCall`/`MethodCall` case so a struct-returning
helper call can be the repeated value directly, proven by a dedicated
evaluate-once test (a global counter, confirmed called exactly once).
Landed clean on the first dispatch, no follow-ups needed.

*(empty — F5-11 (aggregate `ArrayRepeat` return) closed in `298dc80`.
`buildArrayReturnValue`'s `ArrayRepeat` branch gained an `isStruct`
case using `buildNestedAggregateValue` before its generic `buildExpr`
fallback, mirroring F5-10's call-argument fix exactly and preserving
the evaluate-once/copy-N-times pattern (`pebble_repeat_ret_<nodeID>`
temp). The sibling `ArrayValue` case (a full struct-literal array
return, not a repeat) was already working and untouched. Landed
clean on the first dispatch.

*(empty — F5-12 (whole tuple reassignment from a call) closed in
`1d85f45`. Mirrored `buildStructStoreValue`'s already-resolved
`DirectCall` case exactly: result-type check, `findCallDeclaration`/
`ResultType` double-check, `buildDirectCallNested`, call expression
returned directly. A now-obsolete negative test asserting the old
rejection was caught and removed at the periodic full-suite checkpoint
(NOTE: this checkpoint was run per-item this window, denser than the
standing ~every-5-items cadence — corrected going forward per the
user's direct feedback; do not run a full-suite checkpoint again until
several more items are closed).

*(empty — F5-13 (whole array reassignment from a call) closed in
`ea5a52b`. The suggested GNU statement-expression address trick did
NOT work — Apple's Clang rejects taking the address of a statement
expression's result, even though it accepts the basic `({ ... })`
form. The implementer found a more portable alternative: an anonymous
struct-wrapper compound literal with a designated initializer,
`&(struct { T val; }){ .val = make_items(ctx) }.val` — standard C99,
valid on both GCC and Clang, no GNU-extension edge case. Verified via
REAL `cc -Wall -Wextra` compilation (not just Go-string assertions,
given how non-obvious this C shape is) across plain-local, 5-element,
pointer-deref, and struct-field reassignment shapes, plus a single-
evaluation proof. LESSON for any future similar "need an address of a
call result" shape: prefer the struct-wrapper compound-literal idiom
over `&({ ... })` — it's portable, this one already isn't.

*(empty — F5-14 (tuple-return forwarding) closed in `dc8de85`. The
struct branch of `buildAggregateReturnValue` already had a working
`DirectCall` case for this exact shape; added the exact tuple analogue
(same `findCallDeclaration`/`ResultType` check, same
`buildDirectCallWithPre` — no signature change needed, the function
already supports pre-statements). A now-obsolete negative test
(two-hop forwarding chain) was converted into a positive
compile-and-run test in place, matching the F5-12 precedent for
handling stale coverage. Landed clean on the first dispatch.

*(empty — F5-15 (`str` tuple element as a call argument) closed in
`9e6bcdc`. The existing `buildTuplePlaceRead` helper turned out to
only accept bool/entry-width elements (str is neither), so the fix
was NOT the one-line reuse originally planned — it added a
`TuplePlace` branch inline (base-expression/element-lookup/projection
logic mirroring `buildTuplePlaceRead`'s own body shape, but with an
`isStr` check instead of the bool/width gate), emitting the same
`pebble_local_<sym>._<ordinal>` C shape. Verified across call-
argument (multiple ordinal positions), local-init, comparison, and
return-value consumer positions — all share this one `buildStrOperand`
fix. Landed clean on the first dispatch despite the plan needing a
small course-correction mid-implementation.

*(empty — F5-16 (optional field read as a call argument) closed in
`8dff13c`. `buildOptionalValue`'s `Load` case gained a `FieldPlace`
branch mirroring `buildStructFieldRead`'s field-access convention
(`buildPlaceLValue` for the base, `.`/`->` depending on pointer-vs-
value receiver, `pebble_field_<member>` projection). Since the
builder is shared between call-argument and return-value positions,
the one fix covered both, confirmed by test. Landed clean, no
follow-up needed for the implementation itself (though the first
dispatch stalled before writing tests, needing one small dispatch
just to add the missing test coverage — the implementation itself was
already correct).

*(empty — F5-17 (struct payload in a tagged union) closed in
`ac60dc1`. `isPlainStructPayload` admits a struct only if every field
is self-contained at the union block's position (never another
struct/tuple/optional/array/slice/pointer/union field); Emit hoists
each qualifying struct's typedef into a new block emitted between the
enum block and the union block, filtered out of the main aggregate
block to avoid double emission. Construction and the narrowed read
both gained struct-payload cases. Verified against the full existing
union-payload test suite (scalar/bool/char/str/float/enum/nested-
union payloads) with zero regressions. Landed clean on the FIRST
dispatch despite the structural nature — no follow-up rounds needed,
contrary to the multi-round budget flagged when this item was picked
up. A struct with a nested aggregate field stays cleanly rejected;
general dependency-graph ordering remains deliberately out of scope.

*(empty — F5-18 (fixed array of a plain struct as a struct field)
closed in `e05f71f`. Took three dispatch rounds (the first stalled
with zero diff; the resumed second produced a genuine stuck-loop
"completed" report that left the tree with a real `go build` failure —
an incomplete `isArray`/`result.arrays` DFS addition referencing a
struct field never added to `aggregateTypeOrder`). Round three,
precisely scoped to investigate rather than guess, empirically
confirmed (by temporarily disabling the broken line and reading actual
emitted C) that the minimal `throughArray`-exclusion-only fix was
insufficient — the array typedef genuinely does emit before the
struct typedef it references — so it finished the `arrays`-field
wiring properly instead of reverting it: `aggregateTypeOrder` gained
an `arrays` field, `orderAggregateTypes`'s DFS now recurses through a
plain-struct array element and categorizes the array itself via
DFS-postorder, and `Emit` excludes these "interleaved" arrays from the
leading field-array block so `buildAggregateTypedefs` emits each one
at its correct postorder position (after its element's struct
typedef, before any aggregate that references the array). New
`isPlainStructField`/`isPlainStructArrayElement` predicates in
`types.go` (deliberately separate from F5-17's `isPlainStructPayload`,
matching that item's own precedent of per-call-site duplication over
forced unification). The session also caught and fixed an unrelated
pre-existing bug in its own already-written
`TestEmitArrayOfPlainStructMultiFieldCompilesAndRuns` test fixture
(mixing `i32`/`int` fields hit a real T0505 checker-level unification
error, not a backend issue) by making all fields `i32` and adjusting
the expected sum. Verified: `go build` clean, `gofmt`/`go vet` clean,
all 3 new tests pass plus the existing
`TestEmitArrayOfAggregateStructFieldStillRejected` (adjusted to nest
through a genuinely non-plain struct so it still exercises the
rejection path) and `TestEmitArrayOfNestedStructStillRejected` (struct-
in-struct through an array still correctly rejects with "more than one
level of nesting"), full `internal/backend` suite clean (390s, no
flaky-signature failures), causation-checked via file-copy swap
against `HEAD` using `-run` (pre-fix: rejects with "more than one
level of nesting"; post-fix: compiles and exits 7 for the confirmed
live repro).

*(empty — F5-19 (plain-struct parameter in a first-class function
type) closed in `4839a31`. `validateFunctionTypeSignature` gained a
plain-struct parameter case reusing F5-18's `isPlainStructField`
predicate unchanged; `functionTypeParamCType` gained a matching struct
case spelling `structTypeName(param)`; `unit *tir.Unit` had to be
threaded through `validateFunctionTypeSignature` and its four call
sites (all already had `unit` in scope). The real work was typedef
ordering: function typedefs are emitted FIRST in the output (before
even the enum block), which is correct today because a struct can
already carry a function-typed FIELD (needing the function typedef
defined first) — but a function type with a plain-struct PARAMETER
needs the reverse. Resolved by hoisting only the specific struct
typedefs used as function-type parameters into a new
`preFunctionStructTypedefs` block emitted before the function block
(`collectFunctionParamStructs`, new in `typedefs.go`), while
explicitly excluding any struct that itself carries a function-typed
field from that hoist (`hasFunctionTypedFields`, new) — hoisting one
of those would create a genuine circular C dependency, so those stay
in the ordinary aggregate block below the function block, unchanged.
Landed clean on the first real dispatch round (an initial "failed"/
stalled report actually had produced a complete, working diff, caught
only by verifying directly rather than trusting the status — the
build succeeded and the repro ran correctly despite the report).
Verification surfaced one genuine, expected regression in the full
suite: `TestEmitGenericMethodTypeParamOnlyInStructFnParamRejects`
asserted rejection of a generic struct-typed function-value parameter
that, after substitution (`Inner[int]` → `struct { val int; }`), is
now a legitimately-admitted plain struct — confirmed correct by hand
(`pebc -run` now exits 6, the right answer) before dispatching a
small, precisely-scoped follow-up that converted the obsolete negative
test into a positive compile-and-run test in place, matching the
established F5-12/F5-14 precedent. Full `internal/backend` suite
clean after the follow-up (425s), causation-checked via file-copy swap
against `HEAD` using `-run` (pre-fix: rejects with the original "want
int, uint, u64, ... or a pointer type" message; post-fix: compiles and
exits 7 for the confirmed live repro).

*(empty — F5-20 (plain-struct result in a first-class function type)
closed in `a059058`. `validateFunctionTypeSignature` and
`functionTypeResultCType` admit a plain struct result, mirroring
F5-19's parameter case exactly (reusing `isPlainStructField`
unchanged). The typedef-hoisting collector F5-19 added
(`collectFunctionParamStructs`) was widened rather than duplicated —
renamed `collectFunctionParamAndResultStructs`, now walking each
function type's `result` in addition to its `params`, with the same
`hasFunctionTypedFields` circular-dependency exclusion applying
identically to a result-position struct. The genuinely NEW piece (no
F5-19 analogue): `buildStructLocalDeclaration`
(internal/backend/locals.go) had no `tir.IndirectCall` case at all —
only `DirectCall`/`MethodCall` were handled for a struct-typed local's
initializer — so `var p Point = f();` was unreachable independent of
the validation gate; added an `IndirectCall` case delegating to
`buildFunctionIndirectCall` (which already builds a correct call
expression for any result type) and consuming it as the local's
whole-value initializer, mirroring the DirectCall case's shape.
Verification surfaced one trivial, expected fallout: a narrow-integer-
result regression test's expected-rejection substring went stale (the
admitted-types list in the error message grew to include "a plain
struct type"), fixed by a tiny one-line follow-up dispatch — the
underlying rejection behavior itself was never wrong, only the string
assertion. Full `internal/backend` suite clean after both rounds
(403s then 415s), causation-checked via file-copy swap against `HEAD`
using `-run` (pre-fix: rejects with the original "want int, u64, bool,
char, f32, f64, void, or a pointer type" message; post-fix: compiles
and exits 7 for the confirmed live repro). New tests also proved a
struct chained through BOTH a result and a parameter position across
two indirect calls in sequence.

Picking up F5-21 next (`str` result in a function value — e.g. `fn()
str`, rejected by the same `validateFunctionTypeSignature`
result-admission check, per the master ledger: "Add only `str` result
C naming and indirect-call result handling"). This is a MUCH smaller
slice than F5-19/F5-20 — `str` (`PebbleStr`) has no aggregate-typedef-
ordering hazard at all (it is a plain, always-available runtime
struct, not a program-defined typedef — see `functionTypeParamCType`'s
existing `isStr` case, which already admits `str` as a PARAMETER with
zero typedef-ordering complexity), so this should require no
typedef-hoisting work, just: (1) `validateFunctionTypeSignature`'s
result-admission check gains `isStr(snapshot, result)`; (2)
`functionTypeResultCType` gains an `isStr` case returning
`"PebbleStr"` (mirroring `functionTypeParamCType`'s own `isStr` case
exactly); (3) confirm whether `buildStructLocalDeclaration`'s sibling,
whatever function handles a `str`-typed local's initializer (likely
`buildStrLocalDeclaration` or similar — check `locals.go`), already
has or needs an `IndirectCall` case the way F5-20 needed one for
struct locals. Investigate/reproduce directly first with a minimal
`.peb` snippet (`fn() str` returning a literal, called through a
function-typed local, bound to a `str` local, compared or interpolated
to prove the actual bytes survived) before writing a dispatch brief;
next dispatch should use `vercel/alibaba/qwen3.7-flash` (the last two
real dispatches, F5-20 main + follow-up, used qwen then
deepseek-v4-flash, so this one continues the alternation back to
qwen).)*
