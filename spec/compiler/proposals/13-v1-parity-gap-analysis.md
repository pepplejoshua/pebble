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

*(empty — F5-21 (`str` result in a first-class function type) closed
in `1f7939e`. Exactly as sized: `validateFunctionTypeSignature` and
`functionTypeResultCType` each gained an `isStr` case mirroring the
existing parameter-side `isStr` case; NO typedef-ordering work was
needed (confirmed correct — `PebbleStr` is the runtime's fixed C
struct, never a program-defined typedef, so `collectFunctionParamAndResultStructs`
needed no changes). The one genuinely new piece:
`buildStrLocalDeclaration` (`locals.go`) had no `tir.IndirectCall`
case — only `DirectCall`/`MethodCall` were handled for a str-typed
local's initializer — so `var s str = f();` was unreachable
independent of the validation gate; added an `IndirectCall` case
mirroring F5-20's struct-local `IndirectCall` case exactly (delegate
to `buildFunctionIndirectCall`, consume as the whole-value
initializer), but using `buildStrLocalDeclaration`'s own
`PebbleStr`/`localInfo{isStr: true}` conventions rather than the
struct case's. Verification surfaced the SAME trivial fallout pattern
as F5-20 (a narrow-integer-result regression test's expected-rejection
substring going stale as the admitted-types message list grew again),
fixed by an equally tiny one-line follow-up dispatch. Full
`internal/backend` suite clean after both rounds (440s then 426s),
causation-checked via file-copy swap against `HEAD` using `-run`
(pre-fix: rejects with the original message; post-fix: compiles and
runs, correctly comparing the round-tripped string).

*(empty — F5-22 (print an optional value, proposal 17 slice 7) closed
in `1987102`. Landed clean on the FIRST dispatch, despite touching both
the checker AND the backend — a genuinely different category than every
prior F5 item this window, all of which were pure backend gaps.
`printableType` (checker) gained a `types.Optional` case recursing into
the payload via `key.Child()`, mirroring the existing slice/tuple
recursion. Backend emits a raw C `if (<expr>.has_value) { "some(" +
<recursive payload> + ")" } else { "none" }`. The dispatched session did
real, visible incremental debugging beyond the initial plan — discovering
and fixing that `buildOptionalValueExpr` (previously only handling
`SomeOptional`/`NoneOptional`/`OptionalInject` construction nodes) needed
`SymbolValue`/`DirectCall`/`SourceAlias` cases to handle a print operand
that REFERENCES an already-declared optional local rather than
constructing one fresh, and that `buildStructValueExpr`/
`buildTupleValueExpr` needed to transparently unwrap a `SourceAlias`
wrapper around a nested struct/tuple payload literal
(`some(Point.{...})`, `some((1, 2))`) — genuine, correctly-scoped fixes
discovered through real test-driven debugging, not scope creep. Reported
"failed"/stalled despite a complete, correct diff — caught, as always,
by verifying directly rather than trusting the status. Verified with
byte-exact captured stdout (not just exit code) across scalar/struct/
tuple payloads and their `none` counterparts, plus a mixed-operand
print statement. A pointer-payload optional stays cleanly rejected
(pointers are proposal 17's slice 8, not yet printable). No stale
`C0612` negative fixtures existed for optionals (checked, none found).
Causation-checked against `HEAD`; full `internal/backend` AND
`internal/check` suites both clean (this item's checker-touching nature
made the `internal/check` suite a new checkpoint addition, not run for
prior F5 items).

*(empty — F5-23 (print a pointer value, proposal 17 slice 8) closed in
`a8c48b8`. `printableType` gained a `types.Pointer` LEAF case
(unconditionally printable, no `key.Child()` recursion — pointers are
never dereferenced for printing, which is exactly what makes a
self-referential cycle trivially safe). Backend emits
`"&" + %p` for a non-nil pointer and the bare `"nil"` literal for null.
`buildPointerPrintValueExpr` covers every real pointer-value print-
operand shape (`SymbolValue`, `Load`, `FieldValue`, `AddressOf`,
`NilPointer`, `DirectCall`, `PointerCast`, `SourceAlias`). This item
took FOUR total dispatch rounds — by far the most of any F5 item this
window — because printing a self-referential struct
(`type Node = struct { next *Node; };`) surfaced a genuine, necessary
structural requirement: C requires a struct's own typedef to carry a
tag name so a pointer field can reference the enclosing type before the
typedef completes. Round 1 (main implementation) worked but stopped
without tests. Round 2 (test-coverage follow-up) discovered the missing
piece, wrote the required self-referential-cycle test the proposal
explicitly calls for, and fixed it by making `buildStructTypedef`/
`pointerTypeNameForUnit` ALWAYS emit a C tag — too broad: the next
full-suite checkpoint found 22 regressed tests, all asserting the plain
untagged spelling for the overwhelming non-cyclic common case. Round 3
narrowed the fix correctly: `structIsCyclic`/`structCycleSet` (a
directed graph over pointer-typed fields, DFS cycle detection) tags
ONLY structs that actually participate in a pointer-reachable cycle,
restoring the untagged form everywhere else — full suite dropped from
22 failures to 1. Round 4 fixed that last one, another instance of the
same "now legitimately printable" pattern seen throughout this window:
a tagged-union variant with a pointer payload is now printable too
(the union's printability check recurses into payload types), so the
old negative test asserting rejection was obsolete; discovered
mid-fix that pointer-payload union VARIANT CONSTRUCTION itself remains
a separate, unrelated, still-unimplemented backend restriction
(`unionPayloadCTypeAdmissible`), correctly identified as out of scope
and left alone rather than chased. Two now-obsolete negative tests
(optional-of-pointer, union-with-pointer-payload) converted to positive
tests; a dedicated self-referential-cycle test added, proving
termination not just correct output. Causation-checked against `HEAD`;
full `internal/backend` (393s–447s across rounds) AND `internal/check`
suites both 100% clean on the final round.

*(empty — F5-24 (print a function value, proposal 17 slice 9 — the
FINAL slice) closed in `460a769`. Proposal 17's entire composite-print
matrix (slices 1–9) is now fully implemented and resolved.
`printableType` gained a `types.Function` unconditional LEAF case.
Backend dispatch is by the print operand's TIR NODE SHAPE, not a
runtime branch: a bare reference to a known top-level function
(`HoistedFunctionValue`/`GenericFunctionValue`) prints its declared
source name (`<fn f>`, new `functionSourceName` helper mirroring the
existing `structSourceName`/`enumSourceName`/`unionSourceName` family);
every other shape falls back to F5-23's address format (`<fn @0x...>`).
Round 1 (main dispatch) implemented both cases correctly at the
checker/backend-logic level but left a genuine COMPILE bug: the named
case emitted only a static string literal, never generating any C text
referencing the underlying `pebble_fn_<symbol>` function, so `cc
-Wunused-function -Werror` correctly failed the build — caught by hand
(not by the dispatch's own report) before dispatching a small,
precisely-scoped round-2 fix (a `(void)` cast referencing the function
value, mirroring the pervasive `(void)pebble_local_<symbol>;`
unused-variable-suppression idiom already used throughout this
backend). Verified with byte-exact captured stdout for both cases
(including a generic function reference and a function-typed struct
field), causation-checked against `HEAD`; full `internal/backend` and
`internal/check` suites clean on the final round. This closes the
entire proposal-17 F5-22–F5-24 print sub-arc.

*(empty — F5-25 (platform-sized `int`/`uint`) closed in `3bae4e7`. This
closes the ENTIRE F5-05–F5-25 sweep, the last item in the active F5
queue. `cType` now maps `types.Int` to `int64_t` (the only target this
compiler builds for, per `WordBits: 64` hardcoded in `cmd/pebc`/
`cmd/tirdump`, matching spec `05-types-and-inference.md`'s target-
native-word definition — `uint`'s `int64_t`/`uint64_t` mapping was
already correct by coincidence). Threaded through `printfSpecifier`,
`integerKindRange`, and all five checked-arithmetic/shift/negation/
unwrap/float-cast runtime-helper-suffix functions that previously
routed `int` through the `i32` family purely because they happened to
share a C representation, plus a matching checker-side fix
(`isPointerWidthInteger`, `cTypeWidth`) so pointer-width and
structural-width-comparison logic stayed consistent. Deliberately kept
the width as a hardcoded constant rather than genuinely threading
`WordBits` as a real parameter (asked the user directly; they agreed
this was right-sized, since nothing else in the compiler treats
`WordBits` as configurable and no 32-bit target path exists anywhere —
revisit only if a real 32-bit target is ever added). Also fixed a
related, previously-latent gap found during verification: 64-bit signed
literals need an explicit `LL` C suffix (mirroring the existing
unsigned `u` suffix), applying to both `int` and `i64`.

Took FOUR dispatch rounds given real volume: the core width fix landed
clean on round 1, but surfaced 91 pre-existing test failures (a mix of
stale `int32_t`/width-compatibility assertions needing updates, plus
one genuine backend validation gap that silently relied on int==i32
rather than admitting `types.Int` explicitly) — round 2 fixed 5 checker
failures (2 more than anticipated, correctly diagnosed and fixed),
round 3 fixed 69 of 88 backend failures (down to 19, all one shared
pattern — missing `LL` literal suffix — discovered as a byproduct of
round 3's own correctness fix), round 4 closed out the remaining 19.
Full `internal/backend` (593s) and `internal/check` suites both 100%
clean on the final round; causation-checked against `HEAD` using the
master ledger's own repro (`let x int = 2147483648;` — pre-fix: fails
`cc` with `-Wconstant-conversion`; post-fix: compiles and runs
correctly, exit 0).

*(empty — F5-01b (duplicate C enumerators across two live
instantiations of one generic tagged union) closed in `4df19bc`. Root
cause confirmed exactly as suspected: `enumVariantName`
(`internal/backend/types.go`) named a variant's C enum constant using
ONLY the variant's own `symbol.SymbolID`
(`pebble_variant_<memberID>`) — correct for a non-generic enum/union,
but every concrete specialization of a generic tagged union shares the
SAME underlying template's variant symbols (only payload types get
substituted, never the member symbols), so two live specializations
both tried to declare the same C enum constant name in their own
correctly-distinct enum typedefs — a hard `cc` redefinition error.
Widened to `pebble_variant_<ownerTypeID>_<memberID>`, mirroring the
existing `pebble_enum_<typeID>_t`/`pebble_union_<typeID>_t` convention
of embedding a stable type ID, threaded through all 13 call sites
across 8 files. Landed clean on the first real dispatch's core logic
(build succeeded, repro compiled and ran, on the FIRST round — a rare
one-shot clean landing for a 13-call-site refactor), but needed one
follow-up round: the dispatch's own required test coverage (a
compile-and-run test for the two-specialization repro, plus a
structural test proving the two typedefs now carry genuinely distinct
constant names) was never added, and the full-suite checkpoint caught
9 pre-existing structural `*WritesC` tests still asserting the old
single-number constant format — both fixed in the follow-up round. A
false-alarm 10-minute test-BINARY timeout on one run (unrelated tests
were still executing when the default `go test` timeout hit — a
system-load artifact, not a real hang) was cleared by retrying with an
explicit longer timeout, confirming a genuine clean pass. Full
`internal/backend` suite clean at `-parallel 12` (the standing
full-suite flag — this window's actual checkpoint runs had NOT been
consistently passing `-parallel 12` before this item; corrected going
forward per direct user question), causation-checked against `HEAD`.

*(empty — F5-06b (print truncates a `str` value at an embedded NUL
byte) closed in `3d5c93a`. Root cause confirmed exactly as suspected:
`buildScalarPrintParts`'s `case kind == types.Str` (the single
scalar-formatting site every print path routes through) formatted a
str operand as `fprintf(..., "%s", (const char *)expr.data)` — a
C-string operation that stops at the first `0x00`, even though
PebbleStr carries its own `.len` and its bytes are not
NUL-terminated by contract. Fixed by making str force the
sequential-print path (mirroring how a composite/enum/optional/
pointer/function operand already does, via `unwrapPrintOperands`'s
`hasComposite` flag) and emitting a length-bounded
`fwrite((const void *)(expr.data), 1, expr.len, stdout)` raw
statement instead of a printf specifier — new `buildStrPrintOperand`/
`buildStrPrintValueCalls`, mirroring the existing
`buildPointerPrintOperand`/`buildPointerPrintValueCalls` shape
exactly, plus the same fix applied to the materialized-interpolation
print branch in `buildSequentialPrint`. The old `case kind ==
types.Str` in `buildScalarPrintParts` now hard-errors instead of
silently reinstating `%s`, since every real str print path is
intercepted before reaching it. Covers a bare str operand AND a
str-typed struct field/tuple element/array element/slice element (the
deeper fix — nested str print slots route through the same
`buildPrintValueCalls` dispatch). One dispatch stalled twice
(`provider_stalled: no output timeout`) but each resume picked up
correctly via `orc run --session`, per the standing "resume, don't
redispatch" rule — the second resume completed the full implementation
and required test coverage in one pass. Verified independently:
`go build`/`gofmt`/`vet` clean, the exact repro
(`var s str = "x\0y"; print s;`) confirmed byte-exact via `od -c`
(`x\0y\n`, not the old truncated `x\n`), a struct-field embedded-NUL
case confirmed byte-exact too, full `internal/backend` Emit suite and
`internal/check` suite both 100% clean at `-parallel 12`,
causation-checked against `HEAD` (pre-fix: `x\n`, 2 bytes; post-fix:
`x\0y\n`, 4 bytes). This closes the F5 backlog's last remaining item —
both the F5-05–F5-25 active queue and the F5-01b/F5-06b deferred
items are now fully resolved.)*

*(empty — two direct-instruction items closed after the F5 sweep, off
the numbered backlog:

1. Brace-less `else if` (and other bare/unbraced single-statement
   if/loop arms) crashing `Emit` with "... is a If, want a Block" —
   closed in `bc7f506`. Found while verifying the arena rewrite below
   (its `realloc` used `else if`), but independent of it — reproduces
   with a bare `if a > 0 { return 1; } else if b > 0 { return 2; }`
   with no arena involved. Root cause: the grammar documents an
   if-statement's arms as "a statement" (`spec/compiler/
   03b-surface-tree.md`), not "a block", but `buildIf`'s terminal-arm
   path and `buildFallthroughBody`'s ordinary/loop-body path both
   required `tir.Block` unconditionally. Fixed with a new `buildIfArm`
   (terminal if, accepting Block/Return/If) and a non-Block fallback in
   `buildFallthroughBody` delegating to the existing per-statement
   dispatcher (covers the general single-statement case: fallthrough
   if, loop bodies, loop-if arms). Verified via real compile-and-run of
   an else-if chain, an else-if-else chain, a bare-return arm, and a
   bare-if loop body; targeted `TestEmitIf`/`TestEmitWhile` suites
   clean; causation-checked against `HEAD`.

2. `std/mem/arena.peb` rebuilt to remove raw pointer arithmetic,
   replaced with slab-relative `uint` offsets and `slice`-derived ABI
   pointers — closed in `448e386`. See the commit message for detail;
   verified via `examples/arena_alloc.peb` and a new
   `examples/arena_alloc_stress.peb` (slab growth, free-list
   exact-fit/split reuse, in-place end-of-slab realloc growth/shrink,
   byte-level field checks). Surfaced one genuine, NOT-yet-fixed
   backend defect during verification, worked around rather than
   fixed (out of scope for this item): an explicit `as <T>` cast on a
   direct function-call result's initializer is silently dropped by
   the backend, so `let x *MemHeader = some_call(...) as *MemHeader;`
   emits C with the callee's own return type, not the cast's
   destination type, when the callee returns a different (but
   related, e.g. `*u8` vs `*MemHeader`) pointer type — worked around by
   giving the callee itself a correctly-typed return type instead of
   relying on a call-site cast. Logged in proposal 14's backlog as a
   new row for a future item; not reproduced or root-caused beyond the
   worker's own report — needs independent reproduction before
   dispatch.

Both items requested directly by the user mid-session, alongside
removing the `slice` keyword's std-only restriction (queued next) and
resuming the F6 backend-gap sweep (narrow-width checked arithmetic
etc., queued after).)*
