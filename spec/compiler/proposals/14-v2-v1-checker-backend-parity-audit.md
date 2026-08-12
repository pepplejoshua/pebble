# V1 checker and backend parity audit for compiler v2

**Status:** in progress; fourth serious source-and-runtime pass complete on
2026-08-10

This document is the complete V1 feature ledger. It is not the issue tracker.
The only issue tracker is
`spec/compiler/proposals/13-v1-parity-gap-analysis.md`.

The audit compares the V1 source in `src/checker.c`, `src/codegen.c`, and the
V1 type and AST definitions with the V2 Go checker, typed IR, backend, and C
runtime. A matching parser node or a matching Go type is not proof of parity.
The V2 checker and backend must agree, and an end-to-end test must compile,
link, and run the emitted C before a row can be **Verified**.

Status values:

- **Verified**: source support and focused end-to-end tests exist.
- **Implemented, proof needed**: source support exists, but focused proof is
  not yet recorded here.
- **Partial**: only some V1 forms or value shapes work.
- **Absent**: V1 supports the behavior and V2 does not.
- **Intentional difference**: V2 has an accepted different rule.
- **Decision needed**: V2 has no accepted rule for the feature.

## Audit method and closed inventories

The V1 AST inventory comes from `src/ast.h:16-85`. The checker inventory is
the set of all `AST_DECL_*`, `AST_STMT_*`, `AST_EXPR_*`, and `AST_TYPE_*`
branches in `src/checker.c`. The emission inventory is the matching set in
`src/codegen.c`, plus the global, extern, type-layout, prototype, and entry
emission passes. The V2 typed-IR inventory is the 85 tags in
`compiler/internal/tir/node.go`.

This pass checked the following V1 feature sets as separate items instead of
using one broad row:

- 10 declaration forms;
- 14 statement forms;
- 31 expression forms;
- 11 syntax-level type forms and all resolved type families;
- 17 binary and 5 unary operators;
- implicit and explicit conversion matrices;
- structural members;
- switch subject and exhaustiveness matrices;
- range and classic-for lowering rules;
- global and extern data emission;
- scalar, aggregate, optional, enum, union, slice, string, and function-value
  C shapes.

## Fourth serious pass: current confirmed gaps

The fourth pass started from commit `8fa6d97`, re-read the V1 and V2 control,
type, value, and ABI paths, and ran focused programs through the current
`pebc -run` path. A separate read-only Orc review rebuilt and ran both V1 and
V2 for the control-flow cases. No compiler implementation changed during the
pass.

The following failures are current and independently reproduced:

| Current gap | Exact observed behavior | Class |
|---|---|---|
| Call-valued `str` switch subject | `switch choose()` with two case comparisons calls `choose` twice. V1 stores the subject once before its `strcmp` chain. V2 splices the raw call into every `pebble_rt_str_eq` in `buildStrSwitchStatement`. | **Silent semantic defect** |
| ~~Runtime or negative descending range~~ | **RESOLVED (`003141d`, 2026-08-10).** `buildRangeLoop` now mirrors V1's actual runtime-direction lowering exactly instead of detecting compile-time literals. | — |
| ~~Range-bound evaluation order~~ | **RESOLVED (`003141d`, 2026-08-10),** as a byproduct of the descending-range fix — both bounds are now materialized into locals in source order (start then end). | — |
| Non-literal bool switch | A helper that switches on a bool parameter reaches C, then fails under the required `-Wswitch-bool -Werror`. Existing proof covered only a literal bool subject. | **Checker/backend/C contract defect** |
| Unbound range loop | `loop 0..3 { ... }` is accepted by parser, checker, and TIR, then rejected because `RangeLoop.Symbol == 0`. V1 exposes the implicit iterator as `iter`. If V2 keeps the explicit-name rule, the checker must reject the source form. | **Checker/backend contract defect** |
| Local copy initialization | **RESOLVED for all 6 types (2026-08-10).** `let second T = first` now works for tuple (`834927e`), array (`8c72f36`), struct (`2179ebf`), enum (`7f1db25`), `str` (`7747aaa`), and slice (`22ceab8`) — each independently verified and causation-checked, one type per task. | — |
| Tuple coercion | **RESOLVED (`d905ab6`, 2026-08-10)** — `let value (i64, f64) = (a, b);` now works for the tuple-local declaration path (call-argument and reassignment `TupleCoerce` remain deliberately out of scope), verified for a full and a partial-coercion case, causation-checked | — |
| More than one aggregate dependency level | ~~A plain `Outer -> Middle -> Inner` struct chain is rejected as “more than one level of nesting”.~~ **Resolved (`e649476`).** `orderAggregateTypes`'s depth cap is now selective: struct/tuple/optional-only chains nest arbitrarily deep; the cap stays only for chains routed through an array (preserving `emit.go`'s field-array-before-aggregate-block ordering invariant). | **Closed** |
| Direct array return | `return [20, 22]` from a function with result `[2]int` is rejected. Therefore an array-returning call cannot yet provide the deferred call-value copy paths. | **V1 aggregate result gap** |
| Slice field as call argument | ~~`sum(holder.values)` is rejected because the slice argument builder accepts only a slice local or parameter, not `Load(FieldPlace)`.~~ **Resolved (`d33060e`).** `buildSliceArgument` now handles `Load(FieldPlace)`, reusing `buildPlaceLValue`. | **Closed** |
| Existing slice as one variadic tail | ~~`sum(values)` for `fn sum(...values []int)` is rejected as an element conversion.~~ **Resolved (`94e74f0`).** Checker peeks the sole tail argument's known type bottom-up; backend forwards it directly. | **Closed** |
| Direct cast of `sizeof` | ~~`return (sizeof int) as int;` is rejected because the integer-cast child builder has no `SizeofType` case.~~ **Resolved (`634db99`).** `buildExpr` now delegates `SizeofType` to `buildUintExpr`. | **Closed** |
| `sizeof [N]Struct` | ~~The type resolver now accepts the fixed array, but emitted C can reference the array typedef before the struct element typedef exists.~~ **Resolved (`cf97cd3`).** Was actually a missing-collection bug (the element struct/tuple/optional typedef was never collected at all, not merely ordered late); fixed in all three walks. A bare `sizeof (T,U)`/`sizeof ?T` with no array wrapper is also now resolved (`392ae16`). | **Closed** |
| Narrow checked arithmetic | ~~`u8 + u8` and `u8 / u8` emit calls named `pebble_rt_checked_add_` / `pebble_rt_checked_div_`, then fail in `cc`.~~ **Resolved (`73bfbb1`)** for the plain-binary-expression shape — `checkedArithmeticHelper` now rejects cleanly at u8/u16/i8/i16/u32 instead of emitting an empty-suffix call; `uint` was never actually affected (routes through `buildUintExpr`'s plain-C path). Narrow signed negation and `u64` divide/shift still reject earlier, unaffected by this fix. | **Closed** for this specific shape |
| Narrow optional unwrap | ~~`?u8` storage and `some` construction work, but `value!` rejects because no narrow unwrap helper exists.~~ **Resolved (`9426382`).** Added `pebble_rt_checked_unwrap_u8/u16/i8/i16/u32` to the runtime plus matching backend dispatch. | **Closed** |
| Ordinary optional enum construction | ~~`let value ?Color = some Color.blue;` rejects.~~ **Resolved (`1bf785d`)** for local declaration, call argument, and return position — both `buildOptionalLocalDeclaration` and `buildOptionalValueExpr` now delegate a plain-enum payload to `buildEnumValue`. A related, separate gap remains: an enum-typed struct FIELD's construction fails even without an optional wrapper (`RecordConstruct.Fields` traversal gap in `collectEnumTypesWalk`), not yet a formal tracker item. | **Closed** for the optional-construction shape |
| First-class narrow integer function signature | ~~`fn(u8) int` passes signature validation, then its C typedef builder rejects the `u8` parameter.~~ **Resolved (`6fd44d2`)** — `functionTypeParamCType` now resolves any fixed-width integer parameter at its own width. `fn() u8`'s result-side rejection is a separate, deliberate, unchanged narrower grammar (confirmed consistent, not a bug). | **Closed** for the parameter shape |
| V1-recursive C shapes | Confirmed clean rejections remain for a `char` struct field, `char` optional payload, `char` tagged-union payload, `f32` slice element, and enum tuple element. | **C-shape matrix gaps** |
| General interpolated-string value | A local `str` initialized from an interpolated string reaches `InterpolatedString`, then the str-local builder rejects it. | **V1 expression gap** |
| Deferred statement families | Deferred local declaration, block, if, while, range, classic-for, and switch all pass the checker and fail in `buildDeferredStatements`. The bare deferred-local scope leak is also still present. | **Checker/backend contract defects** |

The import-only standard-library sweep now passes for `std:func`, `hash`,
`hmap`, `io`, `libc`, `math`, `mem`, `result`, `set`, `string`, and `vec`.
`std:mem/arena` still fails only on its old pointer-arithmetic expressions;
the Allocator/Context and generic-Result failures no longer appear.

## Declaration and module ledger

| V1 behavior | V1 source | V2 behavior | Status |
|---|---|---|---|
| Pebble function declaration with parameters, result, body, and hidden context | `checker.c` function passes; `codegen.c` prototypes and bodies | `FunctionDeclaration`, reachability walk, helper prototypes, definitions, and context actions | **Verified** (`0f15fd0`) for a representative parameter/result matrix, including the hidden-context threading confirmed via an emitted-C shape check |
| Direct and mutual helper recursion | V1 emits prototypes before bodies | V2 emits helper prototypes and has direct and mutual recursion run tests near `emit_test.go:6155` | **Verified** |
| A recursion cycle through `main` | V1 gives the source entry an internal callable symbol and emits prototypes | V2 rejects any call to `main`; the entry is not an ordinary helper | **Intentional difference**, decided 2026-08-09 |
| Extern C function | V1 emits `extern` declarations and calls the registered C name | V2 validates C signatures and emits extern calls | **Verified** (`5956816`) — real-libc (putchar, memcpy) and shim-linked signatures across bool/char/u8/i16/u64/f32/mixed-multi-param/void-result families |
| Library-named extern block | V1 stores and emits the library name as a C comment/declaration group | V2 accepts a named block and emits/calls its declarations | **Verified** with `extern "C" { fn abs(...) }` compile-link-run |
| Opaque extern type | V1 resolves and emits an incomplete C type | V2 emits an incomplete C type and permits supported pointer use | **Verified** with real `tmpfile`/`fclose` compile-link-run |
| Extern variable and extern constant | `codegen.c:509-620` emits C data declarations | V2 emits real-name `extern` data declarations and supports reads/writes | **Resolved (`1372734`)** |
| Module-level mutable variable storage | V1 emits declarations, definitions, reads, and writes | V2 emits storage and supports scalar reads and writes across functions | **Resolved (`14739f3`)**; aggregate global shapes remain limited |
| Module-level immutable constant | V1 emits global C storage; initializer is limited to a simple literal | V2 inlines immutable global values at each use and has a richer constant evaluator | **Partial**, with a V2 extension; prove every supported constant value shape |
| Uninitialized local or global variable with an explicit type | V1 accepts it and C zero-initializes the object | V2 requires an initializer for each non-extern `let` or `var` | **Intentional difference** |
| Constant declaration | V1 requires an initializer and limits global initialization to literals | V2 constant evaluation supports references, unary and binary expressions, enums, cycle checks, and budgets | **Verified V2 extension** for the evaluator; backend value-shape proof is still needed |
| Import and qualified module value/function/type paths | V1 module member lookup and registered names | V2 module builder, `Path`, and symbol resolution | **Verified** (`ed48868`) — a two-file fixture proves mutable value (read+write-through to real shared storage), immutable value, non-generic function, generic function with a nested qualified type argument, plain type, and two-level generic type, all resolved through a `lib::` qualified path |
| Type declaration | V1 registers struct, union, tagged union, enum, alias, and generic declarations | V2 type declaration facts and specialized nominal types | **Partial**; see the type ledger |
| Instance method | V1 resolves a receiver and emits it as the first call argument | V2 `MethodCall` and receiver validation | **Verified** (`ed48868`) for a representative owner-shape (value/pointer, plain/generic) x argument-shape (literal/local/call-result) spread. The struct call-result/field-read-as-receiver gap found during this proof is now **Resolved** (`008b6fd`) — see the "struct call-result/field-read as argument" row below |
| Associated function selected as `Type.method(...)` | V1 distinguishes methods and associated functions | V2 recognizes a self-less nominal method as a qualified direct call | **Resolved (`e22f185`)** |
| Calling-convention annotation | V1 supports Pebble and C conventions, including a C-convention function body | V2 accepts bodies only for the Pebble convention. C convention is an extern boundary | **Intentional difference** |
| Bare string-literal arrow body | n/a | checker bug | **Resolved** (`dd56d9e`) — `convention()` now breaks the scan at the first non-`Literal` child instead of scanning every child unconditionally, matching the parser grammar (a genuine convention annotation is always a leading modifier before the `Name` node; the trailing arrow-body expression can never be reached). Genuine convention-annotation detection is unaffected |
| `inline` function annotation | V1 writes the C `inline` keyword | V2 keeps `Inline` in TIR as an optimization request, but the backend does not write a C keyword | **Intentional difference**; no language semantic is missing |
| Anonymous non-capturing function | V1 hoists it to module scope and cannot capture a local | V2 `HoistedFunctionValue` does the same | **Verified** (`0f15fd0`) for the block-body form, module-scope hoisting (emitted-C shape check), and checker-level capture rejection (C0617). **New finding**: a void-result anonymous function whose body can fall through is wrongly rejected — see the gap table below. |
| Capturing closure | V1 anonymous functions use module scope as parent and cannot capture a local | V2 reports C0617 for a capture | **Intentional difference from closure languages; parity with V1** |
| Generic anonymous function | V1 rejects it | V2 reports C0608 | **Intentional difference from generic named functions; parity with V1** |
| Entry with no parameters | V1 and V2 emit a C bridge | V2 has focused backend tests | **Verified** |
| Entry with one `[]str` parameter | V1 builds an argument slice | V2 builds the slice through `pebble_rt_args_from_argv`, including `argv[0]` | **Resolved (`fb94640`)** and independently compile-run verified |
| Entry with `argc` and `argv` parameters | V1 accepts the old two-parameter form | V2 rejects it | **Intentional difference** |

## Type and member ledger

| V1 type or member | V2 behavior | Status |
|---|---|---|
| `int`/V1 `isize`, `uint`/V1 `usize`, `i8/i16/i32/i64`, `u8/u16/u32/u64`, `f32/f64`, `bool`, `char`, `void` | All exist in V2; V2 uses `int` and `uint` as the pointer-width signed and unsigned names | **Verified** (`264ad45`) — uint arithmetic and 2^64 wraparound, i64 division/modulo (incl. negative truncation), int (- * / %), f32 arithmetic, i16/u16/u32/i64 six-op comparison matrices (incl. an i64 wide-precision case an f64 comparison would round away), and the missing char <=/>/>= operators |
| Pointer `*T`, address-of, dereference, `nil` | V2 has `AddressOf`, `DereferencePlace`, `Load`, and pointer conversions | **Partial**; whole dereferenced structs are resolved (`a242181`), but other aggregate shapes remain position-specific |
| Fixed array `[N]T` | V2 has `ArrayValue`, `ArrayRepeat`, array locals, indexing, and `.len` | **Partial.** Struct fields (`9dfa4e1`), local/literal reassignment (`aef808e`), enum elements (`94a2a39`), ordinary `sizeof`, direct array-literal/repeat return (`7c625ab`), and `sizeof [N]Struct`/tuple/optional element collection (`cf97cd3`) landed. |
| Slice `[]T` | V2 has slice types, `.len`, `.data`, checked index/slice, and `SliceFromRaw` | **Partial** by source position and element type |
| V1 pointer slice `ptr[start:end]` | V2 rejects pointer slicing and provides std-only `slice ptr, count` | **Intentional difference** under the pointer-safety design |
| Struct | V2 record construction, fields, methods, parameters, results, runtime nominals, and C typedefs exist | **Partial**; local copy initialization and deep aggregate dependencies still reject |
| Tuple | V2 tuple construction, elements, parameters, results, and `TupleCoerce` exist | **Mostly resolved.** Whole-value reassignment (`d1b05be`), local copy initialization (`834927e`), and `TupleCoerce` in a local declaration (`d905ab6`) all landed 2026-08-10. `TupleCoerce` in a call argument or reassignment remains deliberately out of scope. |
| Optional `?T`, `some`, `none`, force unwrap | V2 has optional construction, injection, and checked unwrap | **Partial** by payload type |
| Enum | V2 construction, switch labels, and integer conversions exist | **Verified** for enum-to-integer and integer-to-enum. Enum-element arrays and slices resolved `94a2a39`; local copy initialization resolved (task #43); ordinary `some Color.red` optional initialization resolved `1bf785d`. Remaining: enum tuple elements, and an enum-typed struct field's construction (a separate `RecordConstruct.Fields` typedef-collection gap, discovered but not yet a formal item). |
| Tagged union | V2 construction, ordinary switch narrowing, generic-self read/write narrowing, and helper results exist | **Partial** by payload and container C shape; generic-self narrowing resolved in `7b7eee0`/`7e7163e`; struct-field construction resolved `e3478af` |
| Untagged union | V1 emits a C union and permits construction and member access | V2 rejects construction, read, and write because no safety rule is accepted | **Decision needed** |
| Function type and function value | V2 supports Pebble-convention, non-variadic function values for a limited C-representable signature set | **Partial**; V1 supports a wider convention and signature surface |
| Opaque extern type | V2 represents it, emits incomplete C declarations, permits pointer use, and rejects invalid `sizeof` use | **Verified** |
| Generic type and specialization | V2 supports generic nominal types, specialization, and owner type-parameter inheritance | **Partial** by deep aggregate and value-source shape; owner inheritance resolved in `ddbe454` |
| Recursive nominal type | V2 collection has dependency ordering and recursion paths | **Resolved (`e649476`)**; a plain three-level (and deeper) non-recursive struct chain now compiles and runs; array-of-aggregate chains remain rejected by design |
| Tuple member `.0`, `.1`, and so on | V1 and V2 resolve tuple ordinals | **Verified** for integer/bool elements (`f1841e1`) across 2-5-element tuples, locals, parameters, and struct fields, and for char/str elements (`bd84ee9`). `f64` tuple-ordinal reads remain rejected — not an accessor gap, `f64` is rejected as an aggregate member type entirely (task #86, deferred). |
| Array `.len` | V1 and V2 support it | **Verified** (`f1841e1`) — previously ZERO backend compile-link-run coverage; now proven on a local, parameter, loop bound, struct field, i64 entry, `ArrayRepeat` source, and an array-returning helper's result. |
| Slice `.len` and `.data` | V1 and V2 support both | **Verified** (`f1841e1`) — `.len` proven on a struct field and in parameter arithmetic; `.data` proven as a real pointer value (pointee read, pointer equality against shared/distinct backings, nil comparison, and pointer-argument use), not just as an index/slice base. |
| String `.len` | V1 string code uses `strlen` but has no structural member | V2 exposes byte length as `.len` | **Verified V2 extension** in real string consumers |
| Optional presence member | V1 spells it `.is_some` | V2 spells it `.has_value` | **Intentional rename** |
| Struct field and instance method selection | Both compilers support it | **Partial** by value-source shape; generic and runtime-owner gaps listed here are resolved |
| Enum and union variant selection | Both compilers support it | **Partial** for untagged unions and unsupported payload/container shapes; generic-self selection is resolved |
| Static member call through a type | V1 supports associated functions | V2 supports a self-less method called through its nominal type | **Resolved (`e22f185`)** |

## Literal, expression, and operator ledger

| V1 expression or operator | V2 behavior | Status |
|---|---|---|
| Integer literal | V2 preserves text, constrains range, and lowers by width | **Verified** (`a427181`) for i8/i16/i32/i64/u8/u16/u32/u64/uint boundaries, both compile-run and checker-rejection. **`int`'s boundary is NOT verified — see two new findings below**, one of which is an architectural question (32-bit vs 64-bit `int`), not a routine gap. |
| Float literal | V2 supports f32/f64 lowering | **Verified** (`a427181`) — no literal suffix exists in this language (float kind is always inferred from context); fraction/exponent/negative/maximum-magnitude forms proven at both f32 and f64, both compile-run and checker-range-rejection |
| Boolean literal | V2 supports it | **Verified** in conditions, print, calls, and aggregates |
| Character literal | V2 stores a Unicode scalar and uses integer C storage | **Verified** |
| String literal | V1 is a NUL-terminated C pointer; V2 is `PebbleStr {data,len}` | **Intentional ABI and semantic difference** |
| `nil` pointer | V2 `NilPointer` | **Verified** (`5956816`) across *i32/*u8/*u64/*bool/*char/*void/*Point/*FILE pointees, both comparison directions, a nil-check branch, a helper parameter, and a nil-to-nil copy. Enum/union pointee shape resolved separately (`6c0af95`) — see the "enum/union pointer-pointee typedef collection" row below |
| `none` and `some value` | V2 optional nodes | **Partial** by payload shape |
| Enum/union pointer-pointee typedef collection | n/a | `*EnumType`/`*UnionType` nil pointer whose pointee type is never otherwise used as a value | **Resolved** (`6c0af95`) — mirrored `collectStructTypesWalk`'s pointer-pointee rule into both `collectEnumTypesWalk`/`collectUnionTypesWalk`, plus a Parameters scan on both callers for the helper-parameter shape a value-node walk can't see. Uses `isDefinitelyEnumType` rather than `isEnumType` to avoid wrongly collecting an opaque extern pointee (e.g. `*FILE`) as an enum |
| Context expression | V1 and V2 expose the hidden allocator/context value | **Resolved.** The `Allocator`/`Context` ordinary-struct redesign (proposal 15) is fully complete — all 4 slices, `context` working in every value position (argument, return, local initializer, struct-field construction/assignment). |
| Identifier, module member, partial member | V2 symbol and member value paths | **Partial** by declaration category |
| Grouped expression | Parser-only grouping in both compilers | **Verified** by construction; no backend behavior |
| Interpolated string value | V1 materializes a string expression and formats string, bool, signed/unsigned integer, float, char, enum, struct, and tuple parts | V2 builds `InterpolatedString` TIR, but the backend can consume it only as a direct `print` operand and only when every value part is bool | **Absent except for one narrow print form** |
| Direct call | V2 supports helper and extern direct calls | **Verified** (`0f15fd0`) — the helper half via the function-declaration matrix, the extern half via four real libc signature shapes (int->int, f64->f64, two-f64->f64, str->uint), each confirmed to lower to the real C name with no hidden context |
| Indirect call | V2 supports non-capturing function values | **Partial** by function signature |
| Method call | V2 supports instance calls | **Partial** by owner and argument shape |
| Struct call-result/field-read as argument or receiver | n/a | checker-accepted, was Emit-rejected | **Resolved** (`008b6fd`) — `buildAggregateArgument`'s struct branch gained a `DirectCall`/`MethodCall` case (delegating to `buildDirectCallNested`) and its `Load` case now also accepts `FieldPlace` (via `buildPlaceLValue`), mirroring the already-correct local-declaration precedent. NEW FINDING (untracked): the identical gap exists on the TUPLE-argument branch (`f(makeT())` still rejects) |
| Generic call | V2 specializes named generic functions | **Verified** (`ed48868`) — the same generic function specialized at a generic-struct type argument, a three-level type nest, and a nested type argument in a non-first parameter position, with an emitted-C check confirming no residual unsubstituted type parameter |
| Index | V1 checks array, slice, string, and pointer indexing; V2 checks array, slice, and string and uses Unicode decode for string reads | **Intentional string change** and **partial** aggregate proof |
| String index result | V1 returns one byte; V2 stores bytes but walks UTF-8 from the start and returns the scalar at the requested code-point index | **Verified V2 semantic change** |
| Slice expression | V2 checked slices work in ordinary and nested expression positions, including GNU statement-expression lowering where a temporary is required | **Partial**; struct-literal slice fields and other value-source positions remain separate gaps (`836fbea`) |
| Tuple literal | V2 `TupleValue` | **Implemented**, but whole-value copy paths are partial |
| Array literal and repeat | V2 `ArrayValue` and `ArrayRepeat` | **Partial** by element and destination shape |
| Struct literal | V2 `RecordConstruct` | **Resolved (`e649476`)** for plain deep struct/tuple/optional nesting; runtime Allocator/Context construction is resolved; array-of-aggregate struct fields remain a separate, out-of-scope backend gap |
| Tagged-union variant literal | V2 `VariantConstruct` | **Partial** by payload C shape; generic narrowing is resolved |
| `sizeof(T)` | V1 rejects opaque types but otherwise delegates to C | V2 supports scalar, struct, enum, union, tuple, optional, slice, pointer, runtime, and fixed-array types, plus a direct cast of `sizeof` (`634db99`), `sizeof [N]Struct`/tuple/optional (`cf97cd3`), and a bare `sizeof (T,U)`/`sizeof ?T` with no array wrapper (`392ae16`). | **Verified** |
| Force unwrap | V2 checked optional unwrap | **Resolved** for every scalar payload type (i32/i64/u64/bool/pointer plus u8/u16/i8/i16/u32, `9426382`) |
| Postfix `++` and `--` as a value expression | V1 uses C postfix semantics and returns the old value | V2 defines them as void updates that are legal only as statements or for updates | **Intentional difference** |
| Arithmetic `+ - * / %` | V1 emits raw C arithmetic for all numeric types | V2 uses checked helpers for integers and direct C for floats | **Partial**; helper-width matrix is incomplete |
| Numeric comparisons `== != < <= > >=` | Both compilers support numeric comparisons | **Verified** (`b086dfd`) — all six operators x true/false on u8, i8 (signed, negative values), uint, f32, f64, and u64; no gaps found |
| String equality and ordering | V1 uses C string comparison paths | V2 has length-aware string runtime paths | **Verified** (`b086dfd`) — all six operators proven for empty strings, non-prefix different-length ordering, byte-value ordering, and length-aware equality (a byte-for-byte prefix does not compare equal); no gaps found |
| Boolean logical `&&` and `||` | V2 uses `ShortCircuitValue` | **Verified** for short-circuit sequencing |
| Bitwise `& | ^` | V2 checker accepts integral types; backend coverage is width-specific | **Partial** |
| Shifts `<< >>` | V1 emits C shifts; V2 uses checked helpers | **Partial**; no `uint` or `u64` helpers |
| Unary numeric negation | V2 uses checked negation for integers | **Partial** by integer width |
| Logical not | V2 supports bool | **Verified** |
| Address-of and dereference | V2 place model supports scalar and field/index paths | **Partial** for whole aggregate values |
| Bitwise not | V2 accepts integral types | **Partial** by backend width |
| Pointer arithmetic | V1 supports pointer plus/minus integer | V2 forbids it | **Intentional difference**; decision record in pointer-arithmetic proposal |

## Conversion ledger

V1 implicit conversions are in `checker.c:2082-2434`. V1 explicit casts are
in `checker.c:2437-2529`. V2 classification is in
`compiler/internal/check/compatibility.go`.

| Conversion | V1 | V2 | Status |
|---|---|---|---|
| Integer literal to another integer width | implicit | contextual literal fit or explicit cast | **Implemented with different checking** |
| Non-literal integer to another integer width | implicit | explicit only | **Intentional stricter rule** |
| Integer to float | implicit | explicit only | **Intentional stricter rule** |
| Float width change | implicit | explicit only | **Intentional stricter rule** |
| Float to integer | explicit | explicit, checked runtime conversion | **Partial** by destination width |
| Integer to integer | explicit and implicit | explicit, plus contextual literal fit | **Implemented**, backend proof needed by pair |
| Character to integer | explicit | explicit | **Verified** (`20efd9a`) for int/i8/i16/i32/i64/u8/u16/u32/u64, boundary values, overflow semantics, and non-literal sources. `char as uint` was broken (`buildUintExpr` had no `CharToInteger` case) — **Resolved** (`60a3346`) |
| Integer to character | explicit | forbidden | **Intentional difference** until Unicode scalar validation is specified |
| Enum to integer | explicit | explicit | **Verified** |
| Integer to enum | absent as a general V1 cast; V1 has partial enum inference | explicit checked cast, plus optional checked form | **Verified V2 extension** |
| Pointer to pointer | explicit; `*void` conversions are also implicit | explicit only | **Intentional stricter rule** |
| Pointer to integer | explicit | explicit | **Verified** (`20efd9a`) for pointer-width-or-wider destinations (u64, uint, i64) across int/struct/opaque-extern pointees. **New finding**: any NARROWER destination (u8/u16/u32/i8/i16/i32/int) is checker-accepted but fails at `cc` under `-Werror -Wpointer-to-int-cast` — see the gap table below. |
| Integer to pointer | explicit | forbidden | **Intentional difference** |
| V1 `str` to/from `*void`, `*u8`, or `*char` | explicit or implicit, because V1 `str` is a C pointer | absent for V2 `PebbleStr` | **Intentional ABI difference**; use explicit library adapters if accepted later |
| Fixed array to slice | implicit | dedicated checked slice shape, including direct array-literal initialization of a slice binding | **Partial by source position**; binding form resolved in `f4c3970` |
| Tuple literal element conversion | implicit, equal tuple length | **RESOLVED for local declarations (`d905ab6`, 2026-08-10)**; checker builds `TupleCoerce`, backend now accepts it in a local declaration initializer | — |
| Explicit tuple prefix cast | source can have more elements than destination | V2 requires equal length | **Absent** unless the narrower V2 rule is accepted |
| Array literal element conversion | implicit for equal length | no general structural conversion class | **Partial/absent**; isolate by destination shape |
| Struct literal field conversion | implicit for equal field count and matching names | no structural struct conversion class | **Absent** |
| Explicit structural struct prefix cast | source prefix can cast to a smaller destination struct | forbidden | **Absent** unless nominal-only conversion is accepted |
| `none` to any optional | implicit | contextual optional construction | **Verified** (`5956816`) — none-initialized ?i32/?bool/?*int/?Point all read `has_value == false`; some-constructed counterparts read `has_value == true` with scalar unwrap round-trip |
| `some S` to optional `T` with payload conversion | implicit for a literal `some` | optional injection exists, but payload and backend shapes are limited | **Partial** |
| Single-field struct literal to matching union variant | implicit | V2 uses explicit `VariantConstruct` syntax/facts | **Intentional representation difference** for tagged unions; untagged union is undecided |

## Print and interpolation matrix

V1 `get_format_specifier`, `build_composite_format_string`, and
`build_composite_args` are at `codegen.c:1754-1867`. V1 recursively prints
structs, tuples, arrays, nested composites, and enum variant names. V2
now has recursive checker and backend print paths for structs, tuples, fixed
arrays, slices, plain enums, and tagged unions. Proposal 17 records the exact
slices. Optionals, pointers, and function values remain open.

| Printed value | V1 | V2 | Status |
|---|---|---|---|
| Integer, float, bool, string | supported | supported | **Verified** for common widths; complete width proof needed |
| ASCII character | supported | supported | **Verified** |
| Multi-byte Unicode character | V1 C `%c` is byte-limited | V2 uses `pebble_rt_char_to_utf8` and has `é`, emoji, mixed, and deferred-print run tests | **Verified V2 fix/extension** |
| Enum name | supported | prints `Type.variant` with invalid-tag fallback | **Resolved (`c1bf23b`)** |
| Struct, tuple, and fixed array | recursively formatted | recursively formatted with declared names/order | **Resolved (`c182e73`, `5e6e786`)** |
| Nested composite and slice | recursively formatted | recursive compile-time formatting plus a runtime slice loop | **Resolved (`b80fbc4`, `21e54ec`)** |
| Tagged union | V1 formats tagged union values | prints variant name and recursively prints payload | **Resolved (`9a0f27d`)** |
| Optional, pointer, function value | V1 has format paths for its supported value forms | V2 checker still rejects them | **Absent; proposal 17 slices 7-9** |
| Interpolation | V1 formats string, bool, integer, float, char, enum, struct, and tuple parts and produces a string value | V2 supports only bool value parts and only when the interpolation is a direct `print` operand | **Absent except for the verified bool-print form** |

The earlier audit claim that multi-byte `%c` remained open was stale. The
runtime helper is in `runtime/src/str.c:141`, its ABI is in
`runtime/include/pebble_rt.h:433`, and backend tests are near
`emit_test.go:4600` and `emit_test.go:4850`.

## Control-flow ledger

| V1 control behavior | V2 behavior | Status |
|---|---|---|
| Block, expression statement, return, implicit return | V2 has direct TIR nodes and backend builders | **Verified** (`264ad45`) for block-body explicit return, `=>` expression-body, and if/else-tail-return shapes across int/bool/str/struct/void, plus a discarded-call expression statement. NEW FINDING (tracked separately): a bare string-literal `=>` body is wrongly rejected — see the "bare string-literal arrow body" row below |
| Discard an arbitrary non-void expression as a statement | V1 checks the expression and discards its result | V2 permits only calls, postfix updates, and other expressions whose solved result is void; C0612 rejects other non-void results | **Intentional stricter rule** |
| If/else and terminal-path analysis | V2 validates and emits nested arms | **Verified** for ordinary scalar and aggregate paths; value-shape limits remain |
| While and infinite `loop` | V2 emits while loops and accepts exhaustive terminal loops | **Verified** for ordinary paths |
| Range loop, exclusive and inclusive | Both compilers support both end rules | **Partial** |
| Range-bound evaluation count and order | V1 stores start, then end, and evaluates each once | **Resolved (`003141d`, 2026-08-10).** Both bounds evaluated once, start then end, mirroring V1's actual lowering exactly — fixed as a byproduct of the descending-range fix below, no separate work needed. | — |
| Descending range | V1 evaluates both bounds once and chooses step `1` or `-1` at runtime | **Resolved (`003141d`, 2026-08-10).** `buildRangeLoop`'s compile-time-literal direction detection replaced entirely with V1's actual runtime-direction lowering (start/end/step locals, ternary-conditioned loop test) — verified for runtime-computed and negative-literal descending bounds, and every existing ascending/descending/inclusive/zero-length/uint-bounded case. | — |
| Implicit range iterator named `iter` | V1 creates it when no name is present | **Resolved (`87e8c43`, 2026-08-10).** V2's explicit-iterator-name policy (intentional, kept) is now enforced at the checker (`C0622`) instead of leaking through to a backend-only rejection. | — |
| Range iterator type | V1 registers and emits the iterator as `int` | V2 gives the iterator the exact shared bound type | **V2 extension/correction** |
| Classic `for` with declaration initializer | Both compilers support it | **Verified** for current V2 scalar forms |
| Classic `for` with assignment initializer | V1 accepts and emits it | **RESOLVED (`e3ec6bc`, 2026-08-10).** Decided: implement, don't reject earlier — V1 parity, checker already accepted it, an ordinary for-loop pattern. `buildForInitClause` now accepts a Store (reassigning an already-declared local) alongside the existing Initialize (declaring a fresh one); the no-condition two-clause shape widened the same way. | — |
| Classic `for` with expression update | V1 accepts any checked expression | V2 accepts only assignment, compound assignment, or postfix update | **Intentional narrower rule** |
| Optional classic-for clauses | V2 accepts omitted clauses and lowers them | **Verified V2 surface** |
| Break and continue in loops | Both compilers support them | **Verified**, including defer cleanup |
| Break that targets a switch | V1 permits break only in a loop | V2 control regions permit switch break | **V2 extension** |
| Defer LIFO and cleanup on return, break, and continue | V1 emits deferred statements at exits | V2 has focused compile-run tests for LIFO, nested scopes, helper calls, return, break, continue, compound store, and Unicode print at `emit_test.go:9602-9763` | **Verified for the supported deferred statement kinds** |
| Deferred reassignment, compound assignment/postfix update, print, or void call | V1 accepts and emits each as an ordinary deferred statement | V2 `buildDeferredStatements` supports `Store`, `CompoundStore`, `Print`, and a void-call `ExpressionStatement` | **Verified** |
| Deferred local declaration | V1 accepts it and emits it in a defer-local C block | V2 checker permits deferred bindings, but `buildDeferredStatements` rejects `Initialize`; bare deferred locals also leak into the enclosing checker scope | **Absent; checker/backend contract defect** |
| Deferred block, conditional, loop, or switch | V1 checker recursively accepts any statement except return, and codegen calls the normal statement emitter at the exit | V2 validation specification permits these statements, but `buildDeferredStatements` has no builder case for their TIR nodes | **Absent; checker/backend contract defect** |
| Return inside defer | V1 rejects it | V2 rejects it | **Parity** |
| Break, continue, or nested defer inside defer | V1 does not apply all V2 restrictions | V2 rejects these forms to keep cleanup control explicit | **Intentional stricter rule** |

## Switch matrix

V1 switch validation is at `checker.c:2537-2665` and
`checker.c:7018-7100`. V1 emission is at `codegen.c:2319`. V2 validation is
in `check/switch_validation.go` and `check/control_validation.go`; V2 emission
is at `backend/emit.go:4140-4405`.

| Switch subject | V1 checker/backend | V2 checker/backend | Status |
|---|---|---|---|
| Integer | accepted and emitted as C switch | accepted and emitted | **Verified** (`d8dfbe9`) for u64 (>2^32 labels, genuine 64-bit dispatch, confirmed via emitted-C shape check) and i8 (negative/positive labels) |
| `u8` or `i8` with all 256 values | V1 treats it as exhaustive | V2 enumerates the full domain | **Resolved (`4817dae`)** |
| Character | accepted and emitted as C switch | accepted and emitted with Unicode-scalar labels | **Resolved (`72f0207`)** |
| String | accepted and emitted as `strcmp` if/else chain with a subject temporary | V2 emits a `pebble_rt_str_eq` if/else chain, subject materialized once into a temp | **Resolved (`b1a53e7`, 2026-08-10)** |
| Boolean | V1 rejects it | **Resolved (`9b86144`, 2026-08-10).** V2 accepts it and proves `true` plus `false` exhaustive; a bool subject is now cast to `int32_t` for the C switch header, fixing the non-literal case's `-Wswitch-bool` failure. | — |
| Enum | accepted, duplicate-checked, exhaustive, emitted | V2 supports it | **Verified** for ordinary enums |
| Tagged union | accepted, narrowed, exhaustive, emitted by tag | V2 supports local, variant, and call-valued subjects; generic-self narrowing is resolved | **Verified for current subject forms**, payload C-shape limits remain |
| Default `else` | supported | supported | **Verified** for int, enum, str, and tagged-union subjects (`d8dfbe9` plus pre-existing coverage) |
| Multiple labels on one case | supported | supported | **Verified** (`d8dfbe9`) for int, enum, str, and tagged union — closed a real gap where the existing tagged-union multi-label test only ever proved one of its two listed variants actually routed to the shared body |
| Duplicate constant labels | rejected | rejected | **Verified** for scalar and nominal cases |

## Calls and variadic arguments

| V1 behavior | V2 behavior | Status |
|---|---|---|
| Fixed Pebble parameters | supported | supported | **Verified** (`56cb9ff`) — single-program matrix proves literal, local reference, nested-call result, struct-field read, and inline struct/tuple literal argument shapes all land correctly |
| Trailing Pebble slice parameter marked variadic | V1 collects zero or more tail elements | V2 collects zero or more tail elements into a temporary slice | **Verified** for int, bool, zero tail, and fixed-prefix tests near `emit_test.go:12762` |
| One existing slice as the sole variadic tail | V1 detects the matching slice and passes it directly at `codegen.c:4000-4068` | ~~V2 validates the slice as one element and reports C0601~~ **Resolved (`94e74f0`)** | **Closed** |
| C variadic extern call | V1 permits primitive C variadic use | V2 reports C0604 | **Decision needed**; do not infer a target from V1 alone |
| Aggregate argument, result, and receiver | V1 C value passing handles ordinary C-representable aggregate values | V2 has many implemented paths, but each type and source expression has a separate builder | **Partial**; see the backend shape table |

## Backend whole-value and aggregate-shape ledger

These are live explicit rejection branches in the V2 backend package. V1 usually
gets these operations from ordinary C value copy. A source-level rejection is
strong evidence of missing backend capability, but each row still needs one
small source reproduction before it moves to the issue tracker.

| Value shape | V2 source evidence | Status |
|---|---|---|
| Reassign a whole tuple local | `buildTupleStoreValue`, `stores.go` | **RESOLVED** (`d1b05be`, local/literal; a call value stays deferred) |
| Reassign a whole fixed-array local | `buildArrayStoreValue`, `stores.go` | **RESOLVED** (`aef808e`, local/literal via `memcpy`; a call value stays deferred) |
| Reassign a whole struct local | `buildStructStoreValue`, `stores.go` | **RESOLVED** (`9df0351`/`5ef060a`, local/literal/call value all supported) |
| Reassign a `str` local from another string value | `buildStrStoreValue` accepts only a string literal | **Confirmed partial** |
| Initialize a tuple local from another tuple value | **RESOLVED for `SymbolValue` (`834927e`) and `TupleCoerce` (`d905ab6`), both 2026-08-10** — `let second (int, int) = first;` and `let value (i64, f64) = (a, b);` both now work, causation-checked | — |
| Initialize an array local from another array value | **RESOLVED (`8c72f36`, 2026-08-10)** — `let second [3]int = first;` now works via a bare declaration + `memcpy`, verified for 5-element and bool-element arrays, causation-checked | — |
| Initialize a struct local from another struct value | **RESOLVED (`2179ebf`, 2026-08-10)** — `let second Point = first;` now works, verified for a 3-field struct and a nested-struct field, causation-checked | — |
| Initialize an enum local from another enum value | **RESOLVED (`7f1db25`, 2026-08-10)** — `let second Color = first;` now works, verified for a second variant proving tag round-tripping, causation-checked | — |
| Initialize a `str` local from another `str` value | **RESOLVED (`7747aaa`, 2026-08-10)** — `let second str = first;` now works, verified for a chained copy, causation-checked | — |
| Initialize a slice local from another slice value | **RESOLVED (`22ceab8`, 2026-08-10)** — `let second []int = first;` now works (shared-backing-array semantics, matching slice-parameter behavior), verified for a chained copy and a write-through-copy, causation-checked | — |
| Materialize an interpolated string as a local, argument, result, or ordinary value | `InterpolatedString` is handled only inside `buildPrint`; general string builders reject it | **Absent** |
| Enum-typed fixed-array element | `arrayElementCType`, `types.go` | **RESOLVED** (`94a2a39`, 2026-08-10) |
| Enum-typed slice element | `sliceElementCType`, `types.go` | **RESOLVED** (`94a2a39`, 2026-08-10) |
| Ordinary `some Color.red` optional enum payload | ~~accepts only the integer-to-optional-enum cast path~~ **Resolved (`1bf785d`)** | **Closed** |
| Enum-typed struct field construction (`Holder.{ c = Color.blue }`) | ~~`collectEnumTypesWalk` had no `RecordConstruct` case, so the enum's typedef and variant constant were never collected when only reachable via a field's construction value~~ **Resolved (`d19717c`)** | **Closed** |
| Tagged-union-typed struct field construction (`Holder.{ u = Choice.value(5) }`) | ~~fails identically to the enum case above~~ **Resolved (`e3478af`)**, requiring both a builder-routing fix and a matching `collectUnionTypesWalk` collection fix | **Closed** |
| Tuple ordinal read of a `char`/`str` element | ~~checker accepts, Emit cleanly rejects each: `char` hits `buildCharOperand`'s `TuplePlace` gate, `str` hits the str-local initializer's `Load` gate~~ **Resolved (`bd84ee9`)**. `str` in a VALUE position (not a local declaration, e.g. a call argument) has an adjacent, still-open gap in `buildStrOperand`'s `FieldPlace`-only `Load` case — not yet formal. | **Closed** for the local-declaration shape |
| Tuple/struct-field `f64` member | rejected entirely — `f64` is not accepted as a struct field type at all, and by extension a tuple element read-back; `f32`/`f64` were only ever wired up for helper parameters/results (task #22), never aggregate members | **Confirmed absent**; deferred as task #86 (larger, separate scope: typedef field-type acceptance, `orderAggregateTypes`, every aggregate value builder) |
| `char as uint` | `buildUintExpr` has `IntegerCast` and `PointerToInteger` cases but no `CharToInteger` case | **Confirmed absent**, discovered during cast proof-batch verification (2026-08-11), queued as task #87 |
| Pointer cast to a destination narrower than the pointer (`ptr as int`/`u8`/`u16`/`u32`/`i8`/`i16`/`i32`) | checker-accepted, backend emits a plain `(destType)(ptr)` cast, but `cc` failed under this project's required `-Wall -Wextra -Werror` with `-Wpointer-to-int-cast`; only pointer-width-or-wider destinations (`u64`/`uint`/`i64`) actually compiled | **Resolved** (`297e162`) — the project owner decided against truncation; the checker now rejects a narrow destination cleanly (`isPointerWidthInteger` gates both `classifyComposite` and `coercionFor`), with a single C0601 diagnostic and no C0619 leak. Wide destinations unaffected |
| **⚠ ARCHITECTURAL: `int` literal range mismatch (32-bit vs 64-bit `int`)** | the checker constrains an `int` literal against a 64-bit word (`LiteralTarget.WordBits = 64`), but the backend emits `int`'s C type as `int32_t` (proposal 10 §10.45) — `let x int = 2147483648;` is checker-accepted, then fails at `cc`. This is not a quick accessor fix: it's a live contradiction between two parts of the spec about what `int` actually IS on this target (a native 64-bit word, per spec §05:64, or a 32-bit type, per the backend's own `cType`/proposal 10). Fixing the SYMPTOM (narrowing the checker's range check to 32 bits) could silently change what programs are valid; fixing it the other way (widening `int`'s C representation to `int64_t`) is a much larger backend change touching every `int`-typed position in the compiler. | **Confirmed absent**, discovered during literal proof-batch verification (2026-08-11). Needs a human design decision on which of spec §05:64 or proposal 10 §10.45 is authoritative before ANY implementation — not queued as a routine task. |
| Negative-MIN literal for `i32`/`i64`/`int` (`-2147483648`, `-9223372036854775808`) | checker-accepted (correctly — these are the valid signed minimums), but the checked-negation lowering emitted the POSITIVE magnitude (`2147483648`, `9223372036854775808`) as the runtime helper's constant argument, itself unspellable as a signed C literal at the destination width | **Resolved** (`2bfbfc4`) — a literal that folds to exactly the width's minimum now emits the minimum's own C constant directly (plain decimal for i32/int, the stdint.h `INT64_MIN` macro for i64), bypassing the runtime-helper call for that one case; every other literal and non-constant negation unchanged |
| Void-result anonymous function whose body can fall through (`fn () void {}`) | V1 accepts this (`checker.c:1392` gates the fall-through error on `return_type->kind != TYPE_VOID`); V2 wrongly rejected it with C0607 ("non-void function can fall through without returning"), even though a NAMED void helper (`fn helper() void {}`) was accepted fine. | **Resolved** (`9430881`) — `prepareSignatures()` no longer skips anonymous-function symbols; `signatureNodes()` and `resolveAnonymousFunction`'s parameter `Containing` linkage updated to match |
| Aggregate nesting deeper than one dependency level | ~~aggregate ordering rejects a plain `Outer -> Middle -> Inner` chain~~ **Resolved (`e649476`)** | **Closed** |
| Whole dereferenced struct as a value | local-initializer and argument paths | **Resolved (`a242181`)** |
| Runtime `Allocator`/`Context` argument, result, field assignment, and local initializer | ordinary-struct redesign, proposal 15 | **RESOLVED** — all 4 slices complete (`b54d79d`/`dee9b0f`/`a404f14`/`64d2e2b`), both types verified in every value position |
| Array literal directly assigned to a slice local | checker and backend lower it through a hidden backing array | **Resolved (`f4c3970`)** |
| Slice-typed struct field passed as an argument | ~~backend accepts slice locals but rejects this field source shape~~ **Resolved (`d33060e`)** | **Closed** |
| Inline checked slice inside a nested pure expression | GNU statement-expression carries its required temporary | **Resolved (`836fbea`)** except a slice-typed struct-literal field |
| Fixed-array literal returned directly | ~~fixed-array return builder accepts a local or call, not `ArrayValue`/`ArrayRepeat`~~ **Resolved (`7c625ab`).** Both cases now supported; `ArrayRepeat` single-evaluates its value via a threaded pre-return temp. | **Closed** |
| Direct cast of `sizeof` | ~~integer cast child builder rejects `SizeofType`~~ **Resolved (`634db99`)** | **Closed** |
| Function value with C convention, variadic signature, or unsupported aggregate result | `validateFunctionTypeSignature` near `emit.go:3069` restricts the signature | **Partial** |

## Backend C-shape capability matrix

V1 emits C type names recursively for arrays, slices, tuples, optionals,
structs, unions, tagged unions, enums, and function types. Its dependency
walk includes the child types of each of these shapes. V2 has separate C-type
gates for each container. These gates are not one shared language rule, and
they do not accept the same child types.

| V2 C position | Accepted by the backend | Rejected or defective V1 shapes |
|---|---|---|
| Fixed-array element | any C-spellable integer, `bool`, `char`, `str`, float, tuple, optional, plain enum, or nominal struct | tagged union, pointer, array, slice, and function value |
| Slice element | any C-spellable integer, `bool`, `char`, tuple, optional, plain enum, or nominal struct | `str`, tagged union, float, pointer, array, nested slice, and function value |
| Tuple element | any C-spellable integer, `bool`, `char`, `str`, float, tuple, optional, or nominal struct | enum, tagged union, pointer, array, slice, and function value |
| Optional payload storage | any C-spellable integer, `bool`, tuple, nominal struct, plain enum (including ordinary `some Enum.variant` initialization, `1bf785d`), tagged union, or C-spellable pointer | `char`, `str`, float, array, slice, optional, and function value |
| Struct field | any C-spellable integer, `bool`, `str`, tuple, optional, runtime nominal, nominal struct, plain enum, tagged union, C-spellable pointer, slice, fixed array, or admitted function value | `char`, float, and nested forms rejected by their own gates |
| Tagged-union variant payload | only the enclosing entry width, `bool`, or `str` | all other integer widths, `char`, float, tuple, struct, enum, union, pointer, array, slice, optional, and function value |
| First-class function parameter | any fixed-width integer (each resolved at its own width, `6fd44d2`), `uint`, `u64`, `bool`, `char`, `str`, or C-spellable pointer | Float and every aggregate, enum, union, optional, slice, array, or function value reject |
| First-class function result | enclosing entry width, `u64`, `bool`, `char`, `void`, or C-spellable pointer | `uint` when it is not the entry width, other integer widths, `str`, float, and every aggregate, enum, union, optional, slice, array, or function value |
| C extern parameter/result | C-spellable integer, `bool`, `char`, `str` as `const char *`, float, C-spellable pointer, and `void` result | aggregate, enum, union, optional, slice, array, function value, or opaque value by copy |
| `sizeof` type | C-spellable integer, `bool`, `char`, `str`, runtime nominal, tuple, optional, slice, fixed array, plain enum, tagged union, struct, or C-spellable pointer | function type, `void`, and opaque extern nominal. An array whose element is a struct has a separate typedef-order defect |

Tagged-union fields and optional payloads now select the full tagged-union C
type (`4d1ef51`). Plain-enum and tagged-union helper results also select their
correct C value types (`4475579`), and direct calls in ordinary enum and union
value positions are covered by `2978280`.

## Backend value-source position matrix

V1 normally emits any already-typed C value expression in these positions.
V2 selects a separate builder by destination type and then accepts a short
list of source-node shapes. These are source-reachable limits, not only checks
for damaged hand-written TIR.

| Position | V2 accepted source shapes | Missing source shapes |
|---|---|---|
| Slice call argument | a matching slice local or parameter; a fresh checked slice, including a nested pure expression that carries its leading temporary | slice field, slice-returning call, and raw slice construction |
| Enum or tagged-union call argument | a matching local or parameter, and a supported direct helper result | inline enum variant, inline tagged-union variant, and field value |
| Tuple call argument | matching local or inline tuple literal | tuple-returning call and general tuple-valued expression |
| Struct call argument | matching local or inline record literal | struct field value and other general struct-valued expressions; call forwarding is position-specific |
| Fixed-array call argument | matching local, inline array literal, or array-returning call | repeat expression and other array-valued expressions |
| Fixed-array return | matching local or array-returning call | inline array literal and repeat expression |
| Slice return | matching local, checked slice, or raw slice construction | slice-returning call and slice field value |
| Tuple return | matching local or inline tuple literal | tuple-returning call and other tuple-valued expressions |
| Struct return | matching local, inline record literal, or struct-returning call | struct field value and other struct-valued expressions |
| Optional argument/return | matching local, `some`, `none`, optional injection, optional-returning call, or limited bare payload injection | other optional-valued expressions; bare implicit injection supports integers, `bool`, and pointer only |
| Array or slice index base | addressable array/slice place; for a non-addressable slice value, a slice local/place, slice-returning call, or slice field of a call result | array literal and array-returning call as non-addressable bases |
| Tuple projection base | matching tuple local/place | direct tuple literal and other non-addressable tuple value |
| Grouped value (`SourceAlias`) | transparent in the current scalar, place, and print builders | no current failure reproduced; keep it as a regression-check shape, not an open defect |
| Function-typed struct-field value | field of an addressable struct local or pointer-to-struct local | field of a temporary/call-result receiver and other non-addressable receivers |

Each row must be split by one destination position and one source shape when
it becomes an implementation task. A single “support aggregate values” task
is too large and cannot prove that the other position builders are correct.

## Integer runtime coverage matrix

V1 emits raw C arithmetic. V2 intentionally uses checked arithmetic by
default. The safety rule is accepted, but every checker-accepted integer
width needs a real helper or valid direct lowering.

| Operation | V2 helper coverage | Gap |
|---|---|---|
| Checked add, subtract, multiply | `i32`, `i64`, `u64`; source `int` resolves to the entry signed width | Narrow widths (u8/u16/i8/i16/u32) have no runtime helper and now reject cleanly (`73bfbb1`), rather than emitting an invalid empty-suffix helper call. `uint` is unaffected by this gap — it lowers to plain C arithmetic, not a checked helper call. |
| Checked divide and modulo | `i32`, `i64`; source `int` resolves to the entry signed width | no `u64`, `uint`, or narrow-width helper. A checked `u8` operation reaches invalid C; `u64` rejects earlier |
| Checked shift left and right | `i32`, `i64`, `i8/i16`, `u8/u16/u32` | no `uint` or `u64` helper |
| Checked integer negation | `i32` and `i64`; a negative narrow literal can be folded | a non-literal narrow signed value has no helper and rejects |
| Float-to-integer checked conversion | helper family is limited to `i32` and `i64` destinations | no full integer destination matrix |
| Optional unwrap payload | scalar helper family covers `i32`, `i64`, `u64`, `u8`, `u16`, `i8`, `i16`, `u32`, bool, and pointer paths | **Resolved** (`9426382`) |
| Explicit wrapping multiplication and addition | `wrapping_mul_u64` and `wrapping_add_u64` lower to runtime helpers | **Verified** in SAFE and RELEASE runtime tests and backend run tests |

The earlier audit claim that wrapping `u64` operations remained open was
stale. The helpers are in `runtime/src/arith.c:425-431`, their ABI is in
`runtime/include/pebble_rt.h:182-191`, and backend tests are near
`emit_test.go:11000`.

Proposal 13 currently has no active defect. Before an integer task starts,
one row from this matrix must move there with one exact reproduction and one
small operation-by-width slice.

## Historical proposal 13 snapshot

This section records the old ten-item proposal 13 backlog. It is not a live
task list. Proposal 13 is now empty and holds only one active defect at a
time.

1. ~~`Allocator`/`Context` values cannot cross function boundaries.~~
   **RESOLVED (proposal 15, all 4 slices,
   `b54d79d`/`dee9b0f`/`a404f14`/`64d2e2b`, 2026-08-10).**
   `Allocator`/`Context` are now ordinary parsed structs, not
   compiler-synthesized special types. Both types now move correctly
   through every value position (argument, return, local initializer,
   struct-field construction/assignment): a constructed `Allocator`
   crossing a function boundary as an argument, a return value, and a
   struct-field assignment, all together, compiles and runs correctly
   (verified end-to-end, causation-checked). The bare `context` keyword
   expression — a distinct `ContextValue` TIR node — was initially
   missed (only `Allocator` was verified when this was first marked
   resolved, prompting a correction after the user asked "so we can use
   context expr and allocator type as we like?"); it now also works as
   a function argument, a `let` local's initializer, and a return
   value, verified via a real alloc→write→read→free roundtrip through
   `default_allocator` in each position, causation-checked against the
   exact pre-fix rejection messages. See proposal 15's slice 4 section
   for full detail.
2. The arena rewrite exposes struct/slice typedef identity errors and missing
   checked-arithmetic suffixes. The remaining arena functions still need the
   slice-and-offset rewrite. **Update (2026-08-10):** re-attempted after
   item 1's resolution — `examples/arena_alloc.peb` still fails, but now
   with zero `Allocator`/`Context`-related errors; the remaining failures
   are pointer-arithmetic type-unification errors (`T0505`/`T0507`), a
   distinct, separately-tracked gap (proposal 16), unaffected by this
   item.
3. **RESOLVED (`7b7eee0` read-side, `7e7163e` write-side).** `self.Ok`/
   `self.Err` correctly readable inside a narrowed switch arm for a
   generic-self receiver (4 tests, checker-verified, causation-checked).
   Write-side (`self.Err = error;`) now accepted by the checker for any
   receiver form (pointer, value, plain local) and correctly sets the
   union's `.tag` in the backend on write (2 more tests, causation-
   checked). The tag-omission latent-corruption risk noted below is
   fixed as part of the same commit. `std/result.peb`'s `set_error`
   method itself now checker/backend-verified via a direct non-generic
   reproduction of the same pattern; full end-to-end compile of
   `std/result.peb` via module import syntax was not separately
   exercised (out of scope). Fixing this also fixed a distinct pre-
   existing bug found along the way: `pointerTypeName` had no case for
   a pointer-to-union pointee at all (only `isStruct` was handled), so
   any `*SomeUnion`-typed pointer receiver/parameter emitted a bogus C
   type name — now fixed as `pointerTypeNameForUnit`, used at every
   `pointerTypeName` call site.
4. ~~Qualified static methods are unsupported.~~ **RESOLVED (`e22f185`).**
   A self-less method inside a struct/enum/union body is now callable
   qualified on the bare type name (`Point.origin()`). Root cause:
   `staticTarget` (call_facts.go) already handled a bare type name
   resolving to a plain function (`callDirect`) or a union variant
   (`callVariant`), but had no `SymbolMethod` case, so a self-less
   method fell through to the opaque `C0619` semantic-record rejection.
   Added a case recognizing a method whose first parameter is not named
   `self` as a static call — the backend needed no changes, since
   `callDirect` already lowers correctly. Verified with and without
   arguments; instance methods, struct literals, and variant
   constructors unaffected; a negative test confirms a self-less method
   invoked with instance syntax (`p.origin()`) still cleanly rejects.
   Causation-checked.
5. ~~`main(argv []str)` cannot receive C process arguments.~~ RESOLVED (`fb94640`).
6. ~~Inline slice construction fails in pure nested expression positions.~~
   **RESOLVED (`836fbea`), decided via a policy reversal (2026-08-09,
   direct instruction).** A prior session had deliberately declined to
   use GNU statement-expressions for this; that decision was reversed.
   A `nested bool` parameter now threads through the call-building chain
   (buildDirectCall/buildCallArgument/buildSliceArgument); in a pure
   expression position an inline `CheckedSlice` argument's temp
   declaration and compound literal fold into a single
   `({ <temp decl>; <compound literal>; })` statement-expression instead
   of being rejected. Every prior rejection site for this shape
   (buildUintExpr, buildEnumValue, buildUnionValueExpr,
   buildStructStoreValue) now routes through the same mechanism. One
   harder shape is left explicitly out of scope: a struct literal's
   slice-typed FIELD value from an inline construction, since wrapping
   it would require folding the whole struct literal into the
   statement-expression. Verified end-to-end (the exact `wrap(arr[:])`
   nested-call reproduction returns 6), emitted C inspected directly and
   confirmed to compile/run via `cc`, existing simple case unaffected,
   causation-checked.
7. ~~A non-primitive array literal cannot directly initialize a slice local.~~
   **RESOLVED (`f4c3970`).** Confirmed general (not specific to non-
   primitive elements as originally logged) — both a primitive and a
   struct-element reproduction were broken identically. Checker's
   `implicitArrayToSlice` narrowly recognizes an authored array-literal
   expression directly initializing a slice-typed BINDING (never a
   plain reassignment or any other array→slice position, which keep
   their existing `C0601`), lowered to a `CheckedSlice` wrapping the
   array literal — mirroring exactly what the existing two-step
   workaround already compiles to. Backend's `buildSliceConstruction`
   gains a third accepted base shape (an `ArrayValue` literal),
   constructing a hidden backing array and slicing it, reusing the
   existing array-literal element-building machinery
   (`buildArrayBraceElements`, extracted from the pre-existing
   array-local path). Verified both reproductions end-to-end; confirmed
   array-typed-local literals, the two-step workaround, array literals
   as call arguments, and plain slice reassignment are all unaffected
   or still correctly rejected. Causation-checked.
8. ~~A generic struct method cannot inherit its owner type parameter.~~
   **RESOLVED (`ddbe454`).** A non-generic method on a generic struct
   whose own parameter/return type is directly the owner's type
   parameter (`fn get(self Box[T]) T`) reached the backend unresolved,
   even though field-type substitution and general generic-method calls
   already worked — confirmed by TIR dump that the checker already
   resolves the CALL SITE's result type correctly, but the shared
   symbolic `FunctionDeclaration` itself was never substituted.
   `genericStructMethodSubstitutions` recovers the concrete
   instantiation from the call's receiver and reuses the exact
   `structSubstitutions` map the field machinery already computes (no
   parallel mechanism); `substituteDeclarationSignature` builds the
   monomorphized signature; `discoverReachableHelpers`'s walk is rekeyed
   from plain `FunctionID` to a new `helperKey` (FunctionID +
   substitution identity) so two instantiations of the same shared
   declaration (`Box[int].get()` and `Box[bool].get()`) each get their
   own distinct emitted C function — a real correctness bug (one
   instantiation's signature silently winning for both) explicitly
   avoided and tested. Verified all three shapes (return-position,
   parameter-position, two-instantiation) end-to-end; existing
   already-concrete generic-struct methods unaffected; causation-
   checked.
9. ~~A whole dereferenced struct cannot become a value.~~ **RESOLVED
   (`a242181`).** Read-side twin of the earlier struct-reassignment
   write-side fix (`*self = other;`). `buildStructLocalDeclaration`'s
   struct-typed Load initializer and `buildAggregateArgument`'s struct
   branch both widened to accept a `Load(DereferencePlace)` — a whole
   struct read through a pointer deref (`let q = *ptr;`,
   `use_point(*ptr)`) — reusing the existing `buildPlaceLValue` and
   `buildDereferencePlaceRead` machinery with no new lowering. Verified
   both a local initializer and a direct call argument end-to-end;
   existing struct-value shapes unaffected; causation-checked.
10. ~~`emit.go` and `emit_test.go` need a behavior-preserving file split.~~
    **RESOLVED (`bf16ffe`).** Split into 12 production files + 13 test files
    along natural function-name seams (types, typedefs, collect, values,
    locals, calls, places, statements, stores, aggregates, operators,
    validate). Verified independently: function count preserved exactly
    (260 before/after), two spot-checked functions byte-identical to their
    pre-split versions, full test suite passes with zero failures.

**Note on this section:** proposal 13 was restructured after this audit was
written — it is no longer a 10-item backlog mirror, it now holds exactly one
reproduced item at a time, sourced from this document's own findings tables.
This numbered list is a historical snapshot of what proposal 13 held when
this audit was last read in full, not a live mirror going forward.

Proposal 13 item 3 contains one stale predicted backend gap. It says that
`buildStrOperand` has no `Load(FieldPlace)` path. The current source has that
path at `emit.go:9867-9935`, including the tagged-union payload projection in
`buildStructFieldRead`, and focused struct-string-field compile-run tests are
near `emit_test.go:7828-7868`. Generic-self narrowing can still block this
path before the backend receives TIR, but the stated missing backend case no
longer exists and must be removed or replaced after a real `Result` test.

## Verification queue result

The fourth pass completed the old read-only queue:

- library-named extern blocks compile, link, and run;
- opaque extern types compile, link, and run through real `FILE *` use;
- a plain three-level aggregate dependency was a confirmed backend defect,
  broader than nested generic types — resolved in `e649476`;
- `TupleCoerce` is reachable from ordinary source and is a confirmed backend
  defect;
- `TypeUse` is compile-time metadata and needs no runtime backend node;
- the open-language decision document still contains the drift listed below.

## Open-language decision document drift

The verification-queue read of
`spec/compiler/proposals/open-language-decisions.md` found these current
source conflicts. That document is a reading aid, not an authority, and its
opening phase-status snapshot is obsolete.

| Decision document claim | Current compiler state | Audit result |
|---|---|---|
| Tuple positional access such as `pair.0` does not work | V2 resolves tuple ordinals and emits tuple-place reads | **Stale; resolved** |
| Enum-to-integer and integer-to-enum conversion is forbidden or undecided | Both directions have checker, TIR, backend, runtime checks where needed, and focused compile-run tests | **Stale; resolved** |
| Release-mode overflow/fault behavior is undecided | Implemented checked-helper APIs panic in SAFE mode and return defined wrapped arithmetic results in RELEASE mode for the helper widths that exist; bounds and invalid unwrap remain checks | **Stale as a general claim; width coverage is still partial** |
| The `char` C ABI is undecided | V2 stores a Unicode scalar in `int32_t`, uses that type in helper and extern signatures, and UTF-8-encodes it for print | **Stale; resolved by implementation, but the ABI spec should state it** |
| Generic monomorphization and ownership are open | The same document later marks always-monomorphized shared specialization resolved | **Internally stale summary text** |
| Raw-pointer escape policy is open | The same document records the accepted std-only `slice ptr, count` rule and continued pointer-arithmetic ban | **Internally stale summary text** |

The following entries still describe real open decisions or future features:
untagged-union safety; C variadics and calling-convention adapters; generic
anonymous functions; `_` generic arguments; slice/string lifetime and
ownership; freestanding and driver modes; closures; distinct/newtypes;
pointer mutability and ownership distinctions; named constraints and public
structural traits; bound-method values; explicit enum values; and enum-name
reflection/printing.

## Implementation slicing notes

This is historical investigation detail plus the remaining slice boundaries.
Closed rows remain as audit evidence. A production task must first copy one
current defect to proposal 13 with its exact reproduction and root cause.

| Finding | Confidence | Next small slice |
|---|---|---|
| **RESOLVED (`9dfa4e1`, 2026-08-10).** A fixed-array-typed struct field was rejected entirely by the backend. `structFieldCType` now accepts an array field (declared with its own `pebble_array_<id>_t` typedef, mirroring a slice field). Two compounding gaps fixed alongside it: the typedef was never collected when an array only appeared as a struct field, and once collected needed to be emitted BEFORE the referencing aggregate block (an inline `elem data[length]` needs the complete typedef, unlike a slice's pointer); an array-typed field's lvalue needed a `.data` projection for element reads/writes. Verified end-to-end: construction from a literal/local/call, index reads/writes, a bool-element array, and a mixed-field struct alongside a standalone array. Causation-checked. | — |
| ~~V2 checker accepts character switch, but backend rejects it~~ | **RESOLVED (`72f0207`).** Added an `isChar` branch to the switch-subject builder (reusing `buildCharOperand`) and a char-literal branch to `buildCaseLabel`. 6 new tests including a non-ASCII scalar case. Independently verified, causation-checked. | — |
| ~~V2 checker accepts string switch, but backend has no lowering~~ | **RESOLVED (`49d0f23`).** String switch lowers to an if/else chain through `pebble_rt_str_eq`. A fourth-pass reproduction disproved the earlier `continue` concern. The current defect is different: a call-valued subject is evaluated once per tested case instead of once for the switch. | — |
| ~~V2 does not prove a complete `u8` or `i8` switch exhaustive~~ | **RESOLVED (`4817dae`).** Added covered-integer-value tracking plus a full-range check for `u8`/`i8` specifically (256-value domains, small enough to enumerate) — wider widths are explicitly unaffected, still always require a fallback. 5 new tests, independently verified at the checker level, causation-checked. | — |
| ~~`u8` (and presumably other non-entry-width integers) is not accepted as a switch subject type by the *backend* at all~~ | **RESOLVED (`2b3d684`).** `buildSwitchStatement` now accepts any concrete fixed-width integer builtin as a switch subject (u8/i8/u16/i16/u32/i32/u64/i64), lowering the subject at its own width and threading that width into `buildCaseLabel` so case constants match the subject's C type. Verified u8 and i16. **Deliberately excluded, still open:** the abstract `uint` builtin (word-sized unsigned) — still fails with the same rejection; this was never accepted even before this fix (not a regression), just not covered by this slice. 2+ new tests, causation-checked. | — |
| ~~a negative integer literal in a switch case label (`case -5:`) is rejected outright for a signed subject type~~ | **RESOLVED (`8f643cd`).** `buildCaseLabel` gained an `isNegativeDecimal` path: accepted on a signed subject at its own width, cleanly rejected on unsigned (confirmed the checker already independently enforces this via `T0508`, so the backend guard is defense-in-depth). Also found and fixed a related, more severe pre-existing bug surfaced while building the reproduction: `CheckedNegate` at a narrow width (e.g. i16) has no `pebble_rt_checked_neg_*` runtime helper, so a negative-literal initializer would have emitted a call to a nonexistent function — fixed via literal-only constant folding (`checkedNegateLiteral`). Causation-checked. | — |
| ~~the abstract `uint` builtin is still rejected as a switch subject at the backend~~ | **RESOLVED (`f44133d`).** `buildSwitchStatement` gained a dedicated `isUint` branch calling `buildUintExpr` — the same builder every other uint value position (parameters, locals, globals) already uses — since uint doesn't fit the fixed-width-integer predicate `2b3d684` added. Case labels spelled at uint's own width. Verified; u8/entry-width int subjects unaffected; causation-checked. | — |
| ~~`u8` (and presumably other non-entry-width integers) is not accepted as a function *parameter* type at all~~ | **RESOLVED (`c39416b`).** New `isFixedWidthInteger` predicate plus a matching case in `helperSignature` and `validateHelperSignature` — a u8/i16/u32/... parameter is now declared at its own C type and seeds the callee's scope at its own width, mirroring the switch-subject widening (`2b3d684`). Verified: a helper taking a `u8` parameter, called with a `u8` argument, compiles and runs (returns 5). Entry-width/uint/u64 parameters unaffected. Causation-checked. | — |
| ~~Literal descending range loops execute zero iterations~~ | **RESOLVED (`8baeb8e`)** for plain positive literal bounds. The fourth pass confirms that runtime descending bounds and negative-literal bounds still silently execute zero iterations. | — |
| ~~A nontrivial range end is evaluated on every loop test instead of once~~ | **RESOLVED (`e111c37`).** Each bound now evaluates once. The fourth pass found a separate order defect: V2 evaluates end before start, while V1 and source order evaluate start before end. | — |
| ~~Mutable globals have no backend storage~~ | **RESOLVED (`14739f3`).** Real backend storage for both read and write, supporting integer/uint/bool/char/float/str/plain-enum globals. Required a small necessary checker/TIR companion (`bindingGlobalVar`'s initializer is now recorded as a real TIR node, not just validated and discarded). 13 new tests; independently verified with real cross-function/in-loop mutation, causation-checked across all 10 touched files. | — |
| ~~a global's constant initializer that isn't a literal leaf (e.g. `var x int = 1 + 2;`) is cleanly rejected as not C-static-initializable~~ | **RESOLVED (`9e547fa`), scoped to integer-literal-only arithmetic.** Backend-side folding (`foldConstantIntegerTree`, `math/big`) for a `CheckedArithmetic` tree (+, -, *, /, %) whose operands are, recursively, all integer literals — folds to a plain C literal, range-checked against the global's declared type before narrowing (distinct overflow error, not a silent wrap or Go panic). A checker-accepted but non-foldable shape (a `CheckedNegate`, e.g. `var x int = -5;`) still gets the exact original rejection, confirmed by test — not general constant-expression evaluation (no locals, no calls, no floats). 4 new tests, causation-checked. | — |
| ~~Extern variables and constants have no backend declaration/use path~~ | **RESOLVED (`1372734`).** Reuses the mutable-globals fix's pattern (`14739f3`), but emits a forward `extern <ctype> <realCName>;` declaration instead of synthesized storage. Both read and write supported (checker already enforces extern-`let` immutability, so no backend-side mutability distinction needed). Real platform caveat found and documented: `errno` is a macOS header macro, not a linkable symbol, so runtime correctness is proven against a hand-written C shim instead. 9 new tests, independently verified against real hand-written shims (not just the test harness), causation-checked. | — |
| Composite print still lacks optional, pointer, and function-value shapes | confirmed from the current print matrix | decide and implement one value family per task |
| Existing-slice variadic pass-through is absent | confirmed by a compile-run reproduction | one checker/call-lowering slice |
| General interpolated-string values are absent; only a narrow direct-print form lowers | confirmed by a local-value reproduction | first materialize one scalar interpolated-string local; widen value-part types separately |
| ~~V2 checker accepts string `+`, but backend cannot materialize its `BinaryValue` result~~ | **DECIDED + RESOLVED (`f4f2412`, 2026-08-09, direct instruction).** `str + str` (and any `+` with a `str` operand) is now a checker-level type error (`C0603`), not implemented — plain `str` is an immutable view, and real concatenation already has a first-class path via `String.push_str` (`std/string.peb`) with an explicit allocator; implementing `+` would need an implicit one, entangling with the deferred Allocator/Context redesign (proposal 15). Other `+` operand types (int, uint, float, ...) confirmed unaffected. The backend's own defense-in-depth rejection test for this shape is preserved via hand-built TIR, since real source can no longer reach it past the checker. | — |
| Deferred declaration, block, conditional, loop, and switch forms pass V2 validation policy but have no backend lowering | **Confirmed and scoped (2026-08-10).** Deferred `Store`/`CompoundStore`/`Print`/void-call work. Seven families remain unsupported: local declaration, block, if, while, range-loop, classic-for, and switch. Each can reuse its non-deferred builder. A bare deferred local also leaks into the enclosing checker scope. | one family per task; fix deferred-local scope as its own checker slice before its backend slice |
| **RESOLVED (`9e04364`, 2026-08-10).** A companion crash was found during the same investigation, more serious than the missing-lowering gap above: `defer { return 1; }`, `defer { break; }` (targeting a loop outside the deferred block), and `defer if x { return 1; }` all crashed the compiler with a stack overflow — `C0613` only checked whether the deferred statement itself was directly `return`/`break`/`continue`/`defer`, not whether one was reachable nested inside a deferred block/if/loop; the IR builder's defer-chain walk then re-registered the same defer infinitely. Fixed at the checker level: `validateDefers` now walks the deferred statement's region subtree and rejects any exit whose target lies outside it as `C0613`, while correctly leaving alone a break/continue whose target loop is itself entirely inside the deferred block. Verified all three crash reproductions now reject cleanly; the contained-exit case still passes the checker; causation-checked by reverting and reproducing the exact stack overflow. This had to land before any of the 6 families above are implemented, since implementing deferred blocks/loops without this guard would only widen the crash surface. | — |
| ~~Assignment-form classic-for initializer reaches TIR but backend rejects it~~ | **RESOLVED (`e3ec6bc`, 2026-08-10).** Decision made: grammar acceptance is intentional (V1 parity, an ordinary for-loop pattern) — implemented rather than rejected earlier. Verified: local-to-local reassignment as the initializer, a bool-typed initializer, the no-condition variant, and a value-computed initializer; the declaration-form initializer and initializer-only/condition-only/update-only variants confirmed unaffected. An obsolete rejection test was found and removed. Causation-checked. | — |
| Whole tuple, array, struct, enum, string, and slice copy initialization is incomplete | confirmed by six focused local-initializer reproductions; many reassignment paths are now resolved | one type and declaration-initialization operation per task; do not combine them |
| — struct: **RESOLVED (`9df0351` local/literal, `5ef060a` call value).** Whole struct-local reassignment now works for a pointer-deref/field write and a plain local, with the new value an in-scope struct-typed local, a fresh struct literal, OR a call to a struct-returning helper (`p = make_point();` / `*self = make_point();`) — all reproductions independently verified end-to-end (exit 9, causation-checked). | — |
| — tuple: **RESOLVED (`d1b05be`, local/literal only, 2026-08-10).** Whole tuple-local reassignment now works for a pointer-deref write and a plain local, with the new value an in-scope tuple-typed local or a fresh tuple literal (`buildTupleStoreValue`, mirroring the struct fix). A call to a tuple-returning helper on the right-hand side stays a deliberate, clean rejection (out of scope, same staged approach the struct fix used). Verified: local-to-local, pointer-deref, fresh-literal, a 3-element tuple, and a mixed-type `(int, str)` tuple. A stale test asserting the old blanket rejection was found and replaced. Causation-checked. | — |
| — array: **RESOLVED (`aef808e`, local/literal only, 2026-08-10).** Whole-array reassignment now works for a plain local and a struct-field-through-pointer-deref write. A standalone array local is a raw C array (not a wrapper-struct typedef like struct/tuple), so C cannot assign it with `=`; the store lowers to a `memcpy` instead. A real compounding typedef-collection bug (the array-literal case's compound literal needs a `pebble_array_<id>_t` typedef nothing was collecting) was found during independent verification, after an initial dispatch's own self-report claimed success without actually testing the literal-reassignment path — and fixed in the same change. An array-returning call on the right-hand side stays a clean rejection, confirmed unreachable from real source today anyway (returning an array literal isn't supported at all yet — separate, pre-existing gap). Verified: local-to-local, literal (5-element, bool-element), and pointer-deref-through-struct-field. Causation-checked. Enum and string reassignment remain untouched. | — |
| — enum-typed array/slice elements: **RESOLVED (`94a2a39`, 2026-08-10).** Both array and slice elements of a plain enum type now work (the task's assumed premise that arrays already supported this was wrong — arrays needed the same widening as slices). Plain enum uses `isDefinitelyEnumType` (positive-evidence form, avoiding a false-positive exclusion of a real struct); a tagged union element stays correctly rejected. A tagged forward-declaration mechanism (mirroring `ffa50d1`'s struct/tuple/optional support) lets the slice typedef block, emitted before the enum block, safely name the element's incomplete enum typedef. Two unrelated latent bugs found and fixed alongside it: `pointerTypeNameForUnit` had no enum-pointee case, and `buildEnumLocalDeclaration` lacked a `Load(CheckedIndexPlace)` initializer case. Verified: construction, multi-variant round-tripping, a `SliceFromRaw` enum-element slice in std. Causation-checked. Ordinary optional enum payloads remain a separate, still-open shape. | — |
| ~~Entry-function recursion cycle is rejected~~ | **DECIDED (2026-08-09, direct instruction):** current V2 behavior (rejecting a call cycle through `main`) is correct and intentional — `main` is the entry point, it should not be callable from anywhere. Move to "Accepted V2 differences" below; not a defect. | — |
| Checked numeric helper coverage is incomplete | confirmed by the width-by-operation matrix and real `u8` invalid-C reproductions | fix one operation family and one width family per task |
| Each aggregate/container C position accepts a different child-type set | high from the dedicated array, slice, tuple, optional, struct, union, and function-type C-name gates | reproduce one container plus one rejected child type per slice; do not dispatch a general container task |
| ~~A tagged union used as a struct field or optional payload receives the plain-enum C type name~~ | **RESOLVED (`4d1ef51`).** Reproduced two stacked bugs: a typedef-ordering defect (union typedef emitted after first use, hard `cc` failure) and the predicted wrong-type-selection bug underneath it (confirmed real, but caught by `-Werror` as a hard error, not silent). Both fixed in `emit.go`; `structFieldCType`/`optionalPayloadCType` now use the existing `isTaggedUnionType` distinction. 5 new compile-run tests; construct-store-read-back round-trip and panic-on-none independently verified, not just clean compilation. | — |
| ~~`sizeof` a fixed array passes validation but the backend rejects it~~ | **RESOLVED (`cacaa28`).** Added the missing `isArray` branch to `sizeofCTypeName`, plus the same compounding typedef-collection gap as the union fix (`collectHelperArrayTypes` replaced with `collectArrayTypes`, now walking entry/helper bodies for `SizeofType` array references). A bare `sizeof [N]StructType` still fails due to the separate bare-sizeof-of-struct/enum gap logged above — intentionally not expanded here. Independently verified: exact repro compiles/runs (prints 16), causation-checked. | — |
| ~~`sizeof` a tagged union selects the discriminant enum C type instead of the tagged-union C type~~ | **RESOLVED (`f2e8c62`).** Also fixed a compounding bug: nothing forced the union's typedef pair to be collected when `sizeof` was its only reference, so it didn't compile even after the type-selection fix. `isTaggedUnionType` (construction-based) couldn't be reused for `sizeofCTypeName` — added declaration-level `isUnionEnumType` instead. Independently verified: exact tracker repro compiles/runs, causation-checked. | — |
| ~~A bare `sizeof` of ANY plain struct or enum leaves its typedef uncollected~~ | **RESOLVED (`8de0cdb`).** Last piece of the sizeof-collection family (union `f2e8c62`, array `cacaa28`). Extended `collectStructTypesWalk`/`collectEnumTypesWalk` to also collect `SizeofType` type arguments, mirroring the array/union fix exactly. Independently verified: both repros compile and print correct sizes (8, 4), causation-checked. | — |
| ~~A helper that returns a plain enum or tagged union is classified as a struct result~~ | **RESOLVED (`4475579`).** Both plain-enum and tagged-union returns landed together (same shape of change). Added `enumType`/`unionType` fields to `resultInfo`, the missing return-switch cases, `buildReturnStatement` branches, and a required companion fix (a local declared from the call, `let c = pick();`, was separately rejected). 4 new tests, independently verified, causation-checked via file copies. | — |
| ~~An enum/tagged-union-returning `DirectCall` used directly in a general value position still cleanly rejects~~ | **RESOLVED (`2978280`)** for the positions that route through `buildEnumValue`/`buildUnionValueExpr` (switch subject for enums, comparison operands, call arguments) — added the missing `DirectCall` case to both, mirroring the existing `buildUintExpr`/`buildStrOperand` pattern. Independently verified, causation-checked. | — |
| **Resolved investigation (2026-08-10), narrowed to a single small backend gap.** A tagged union used directly as a switch subject (`switch make() { case .value: ...; else: ... }`) was originally assumed to be a `statements.go` backend gap, then re-scoped to a checker gap (`C0611`). Re-investigation against current HEAD found the checker gap is ALREADY FIXED — same-scope-local and call-expression subjects (proposal's cases 2 and 3) are one single root cause (`caseVariantMember`, `switch_validation.go:104`, returning a zero `Declaration` whenever the subject's type isn't already `Known`), already resolved by `7b7eee0` (`resolvedRootType` recovery, `solved_record.go:41-76`). No checker gap remains for any switch-subject shape. What DOES still fail, unmasked now that the checker accepts the program: the BACKEND's tagged-union switch-subject builder (`statements.go:603-647`) only handles `tir.SymbolValue` and `tir.VariantConstruct`/`tir.EnumVariantValue`, not `tir.DirectCall` — `buildUnionValueExpr` already has the exact `DirectCall` pattern needed (`values.go:702-731`), the switch-subject builder just never got it. Enum and int `DirectCall` switch subjects are confirmed unaffected (a separate code path). | **RESOLVED (`3f9f7e2`, 2026-08-10).** Added a `tir.DirectCall` case to the tagged-union switch-subject builder mirroring `buildUnionValueExpr`'s existing pattern. Verified end-to-end for both variants, an else-arm form, and a loop-body position; confirmed the call is materialized exactly once in emitted C; `SymbolValue`/`VariantConstruct` union subjects and `DirectCall` subjects for plain enums/integers confirmed unaffected. Causation-checked. | — |
| Value-source support changes by argument, initializer, return, index, projection, and assignment position | high from the dedicated builders and their clean rejections | reproduce one destination position and one source node shape per slice |
| ~~writing to a tagged-union variant member (e.g. `self.Err = error;`) is rejected with `C0605`~~ | **RESOLVED (`7e7163e`).** Checker's `unionVariantPayloadWrite` accepts a write to a union's own declared variant payload member (pointer, value, or plain-local receiver), still hard-rejecting any non-variant name. Backend's `unionVariantPayloadStoreTarget` + a comma-expression store now sets `.tag` to the correct discriminant on the same write, closing the latent-corruption risk. Also fixed a distinct bug found while wiring this up: `pointerTypeName` had no case for a pointer-to-union pointee at all, so `*SomeUnion` params/receivers emitted a bogus C type name — replaced with `pointerTypeNameForUnit` at every call site. 3 new tests (checker accept/reject, backend compile-run), causation-checked by reverting all touched files to HEAD and confirming the original `C0605` rejection reproduces exactly. | — |
| ~~Tagged-union switch narrowing works when the subject is a function parameter but fails (`C0605`) when the subject is an ordinary same-scope `let`-bound local~~ | **STALE, appears already resolved (2026-08-09).** Re-reproduced the exact quoted repro against current HEAD (both `let` and `var`, both an exhaustive second case and an `else` arm) — all correctly narrow and return the expected value, no `C0605`. Most likely fixed as a side effect of `7b7eee0` (Result[T,E] generic self-narrowing), which touched the same `caseVariantMember`/same-scope narrowing path. No code change made; closed without a new commit. | — |

## Accepted V2 differences and extensions

These items must not return as parity defects without a new language decision:

- explicit loop iterator names; the checker must reject the currently
  accepted unbound form if this rule stays;
- no pointer arithmetic;
- no integer-to-pointer conversion;
- no integer-to-character conversion;
- byte-length `PebbleStr` with UTF-8 scalar decode on indexed reads;
- checked integer arithmetic by default, with explicit wrapping builtins;
- no untagged-union operations before a safety design;
- no old two-parameter entry form;
- postfix increment and decrement are void updates, not value expressions;
- a non-void expression cannot be discarded unless it is an accepted call or postfix-update statement;
- local and global non-extern bindings require initialization;
- C convention is an extern boundary, not a user-function body convention;
- boolean switch and switch-targeted `break` are V2 extensions;
- richer module-level constant evaluation is a V2 extension;
- `main` cannot be called recursively or from anywhere else in the program — it is the entry point only, decided 2026-08-09.

## Build configuration and ABI ledger

These modes are visible in V1 checker or code generation even though some of
their command-line selection belongs to the driver.

| V1 mode or ABI behavior | V2 behavior | Status |
|---|---|---|
| SAFE bounds checks | V1 emits checks only in safe mode for arrays, slices, and strings | V2 runtime keeps bounds, null-dereference, and invalid-optional checks in all modes because there is no valid wrapped result | **Intentional stronger safety rule** |
| SAFE integer overflow | V1 relies mainly on C arithmetic behavior | V2 checked helpers panic in `PEBBLE_RT_MODE_SAFE` | **Verified V2 safety extension** for implemented helper widths |
| RELEASE integer overflow | V1 emits raw C operations | V2 checked helper API returns defined wrapped results in `PEBBLE_RT_MODE_RELEASE` | **Verified** for implemented helper widths |
| Freestanding compilation | V1 suppresses hosted headers, allocator adapters, and `print`; entry gets an empty context | V2 runtime header describes `PEBBLE_RT_FREESTANDING`, but the current backend `Emit` API has no freestanding configuration and always writes the hosted entry/context template | **Decision needed**, also a driver decision |
| No-main/library output | V1 checker can skip entry validation and codegen can omit hosted `main` | V2 checker has `EntryNone`, but backend `Emit` requires one entry symbol and always writes hosted `main` | **Partial/absent backend mode**; driver decision remains open |
| Custom non-`main` entry | V1 accepts a configured zero-parameter, void, C-convention entry | V2 checker can validate a configured symbol, but backend always emits its fixed hosted `main` bridge around that entry | **Partial**; exact desired mode is undecided |
| Hidden Pebble context forwarding | V1 adds context to Pebble-convention calls | V2 records `ContextForward`, `ContextExpr`, and `ContextIndirect` and writes `PebbleContext *ctx` | **Verified** (`56cb9ff`) — nested three-level call chain and an indirect call through a function-typed parameter both proven with a real allocator alloc/write/read/free roundtrip at the deepest hop, confirming `ctx` threads through each hop rather than being dropped or re-fetched |
| Allocator callback ABI bridge | V1 runtime context stores alloc/realloc/free callbacks | V2 emits file-scope adapters for source functions stored in `Allocator` callback fields | **Verified** for construction, invocation, argument passing, return, local initialization, and field storage |
| C headers for extern functions | V1 emits configured headers/library data | V2 includes a fixed broad libc header set when any C extern exists | **Partial/intentional simplification**; custom header and library selection is driver work |

## Exact V1 AST coverage index

This index closes the mechanical `src/ast.h:16-85` inventory. The detailed
behavior and status are in the ledger named in the last column.

### Declaration nodes

| V1 AST node | V2 mapping | Detailed ledger |
|---|---|---|
| `AST_DECL_FUNCTION` | `FunctionDeclaration` | Declaration and module ledger |
| `AST_DECL_EXTERN_FUNC` | function-shaped `ExternDeclaration` | Declaration and module ledger |
| `AST_DECL_EXTERN_TYPE` | extern nominal/type symbol | Declaration and module ledger |
| `AST_DECL_EXTERN_BLOCK` | flattened extern declarations | Declaration and module ledger |
| `AST_DECL_EXTERN_VARIABLE` | data-shaped `ExternDeclaration` | Declaration and module ledger; **Resolved (`1372734`)** |
| `AST_DECL_EXTERN_CONSTANT` | data-shaped `ExternDeclaration` | Declaration and module ledger; **Resolved (`1372734`)** |
| `AST_DECL_VARIABLE` | local `Initialize` or `GlobalDeclaration` | Declaration and module ledger |
| `AST_DECL_CONSTANT` | local/global binding plus constant value | Declaration and module ledger |
| `AST_DECL_TYPE` | `TypeDeclaration` and member declarations | Type and member ledger |
| `AST_DECL_IMPORT` | TIR/module `Import` | Declaration and module ledger |

### Statement nodes

| V1 AST node | V2 mapping | Detailed ledger |
|---|---|---|
| `AST_STMT_RETURN` | `Return` or `ImplicitReturn` | Control-flow ledger |
| `AST_STMT_IF` | `If` | Control-flow ledger |
| `AST_STMT_WHILE` | `While` | Control-flow ledger |
| `AST_STMT_LOOP` | `While` or `RangeLoop` by syntax form | Control-flow ledger |
| `AST_STMT_FOR` | `For` | Control-flow ledger |
| `AST_STMT_BLOCK` | `Block` | Control-flow ledger |
| `AST_STMT_EXPR` | `ExpressionStatement` | Control-flow ledger |
| `AST_STMT_ASSIGN` | `Store` or `CompoundStore` | Backend whole-value ledger |
| `AST_STMT_PRINT` | `Print` | Print and interpolation matrix |
| `AST_STMT_BREAK` | `Break` | Control-flow ledger |
| `AST_STMT_CONTINUE` | `Continue` | Control-flow ledger |
| `AST_STMT_CASE` | `SwitchCase`; emitted only through its parent switch in both compilers | Switch matrix |
| `AST_STMT_SWITCH` | `Switch` | Switch matrix |
| `AST_STMT_DEFER` | `DeferRegister` | Control-flow ledger |

### Expression nodes

| V1 AST node | V2 mapping | Detailed ledger |
|---|---|---|
| `AST_EXPR_CONTEXT` | `ContextValue` | Literal/expression ledger and ABI ledger |
| `AST_EXPR_LITERAL_INT` | `IntegerLiteral` | Literal/expression ledger |
| `AST_EXPR_LITERAL_FLOAT` | `FloatLiteral` | Literal/expression ledger |
| `AST_EXPR_LITERAL_STRING` | `StringLiteral` | Literal/expression ledger |
| `AST_EXPR_INTERPOLATED_STRING` | `InterpolatedString` | Print and interpolation matrix |
| `AST_EXPR_LITERAL_CHAR` | `CharLiteral` | Literal/expression ledger |
| `AST_EXPR_LITERAL_BOOL` | `BoolLiteral` | Literal/expression ledger |
| `AST_EXPR_LITERAL_NIL` | `NilPointer` | Literal/expression ledger |
| `AST_EXPR_IDENTIFIER` | `SymbolValue`, `StoragePlace`, or hoisted value | Literal/expression ledger |
| `AST_EXPR_BINARY_OP` | `BinaryValue`, `ShortCircuitValue`, or checked operation | Literal/expression and integer matrices |
| `AST_EXPR_UNARY_OP` | `PrefixValue`, `AddressOf`, dereference place, or checked negate | Literal/expression ledger |
| `AST_EXPR_CALL` | `DirectCall`, `IndirectCall`, or `MethodCall` | Calls and variadic arguments |
| `AST_EXPR_INDEX` | `CheckedIndex` or `CheckedIndexPlace` plus `Load` | Literal/expression ledger |
| `AST_EXPR_SLICE` | `CheckedSlice` or `SliceFromRaw` | Literal/expression ledger |
| `AST_EXPR_MEMBER` | `FieldValue`, `FieldPlace`, tuple place, or method call facts | Type and member ledger |
| `AST_EXPR_MODULE_MEMBER` | resolved path plus symbol value | Declaration and module ledger |
| `AST_EXPR_PARTIAL_MEMBER` | contextual enum/variant value | Type and member ledger |
| `AST_EXPR_TUPLE` | `TupleValue` | Backend whole-value ledger |
| `AST_EXPR_STRUCT_LITERAL` | `RecordConstruct` or `VariantConstruct` | Backend whole-value ledger |
| `AST_EXPR_ARRAY_LITERAL` | `ArrayValue` | Backend whole-value ledger |
| `AST_EXPR_ARRAY_REPEAT` | `ArrayRepeat` | Backend whole-value ledger |
| `AST_EXPR_FUNCTION` | `HoistedFunctionValue` | Declaration and module ledger |
| `AST_EXPR_IMPLICIT_CAST` | dedicated coercion nodes | Conversion ledger |
| `AST_EXPR_SIZEOF` | `SizeofType` | Literal/expression ledger |
| `AST_EXPR_EXPLICIT_CAST` | dedicated conversion/check nodes | Conversion ledger |
| `AST_EXPR_GROUPED_EXPR` | source alias/grouping; no runtime operation | Literal/expression ledger |
| `AST_EXPR_SOME` | `SomeOptional` or `OptionalInject` | Conversion and type ledgers |
| `AST_EXPR_LITERAL_NONE` | `NoneOptional` | Conversion and type ledgers |
| `AST_EXPR_FORCE_UNWRAP` | `CheckedOptionalUnwrap` | Type and integer runtime ledgers |
| `AST_EXPR_POSTFIX_INC` | `CompoundStore` in statement/update position | Literal/expression ledger |
| `AST_EXPR_POSTFIX_DEC` | `CompoundStore` in statement/update position | Literal/expression ledger |

### Type-expression nodes

| V1 AST node | V2 mapping | Detailed ledger |
|---|---|---|
| `AST_TYPE_NAMED` | builtin, nominal, alias, or type parameter | Type and member ledger |
| `AST_TYPE_QUALIFIED_NAMED` | qualified path to type | Declaration and module ledger |
| `AST_TYPE_POINTER` | pointer type | Type and member ledger |
| `AST_TYPE_OPTIONAL` | optional type | Type and member ledger |
| `AST_TYPE_ARRAY` | array type | Type and member ledger |
| `AST_TYPE_SLICE` | slice type | Type and member ledger |
| `AST_TYPE_STRUCT` | nominal struct declaration | Type and member ledger |
| `AST_TYPE_FUNCTION` | function type | Type and member ledger |
| `AST_TYPE_TUPLE` | tuple type | Type and member ledger |
| `AST_TYPE_ENUM` | enum declaration | Type and member ledger |
| `AST_TYPE_UNION` | tagged or untagged union declaration | Type and member ledger |

### Internal resolved V1 type kinds

| V1 `TypeKind` | V2 mapping or rule |
|---|---|
| `TYPE_INT` | pointer-width signed `int` builtin |
| `TYPE_BOOL` | `bool` builtin |
| `TYPE_STRING` | `str` builtin with the new `PebbleStr` ABI |
| `TYPE_VOID` | `void` builtin |
| `TYPE_F32`, `TYPE_F64` | `f32`, `f64` builtins |
| `TYPE_U8`, `TYPE_U16`, `TYPE_U32`, `TYPE_U64` | same fixed-width unsigned builtins |
| `TYPE_USIZE` | renamed pointer-width unsigned `uint` builtin |
| `TYPE_I8`, `TYPE_I16`, `TYPE_I32`, `TYPE_I64` | same fixed-width signed builtins |
| `TYPE_ISIZE` | renamed pointer-width signed `int` builtin |
| `TYPE_CHAR` | Unicode-scalar `char` builtin |
| `TYPE_POINTER` | pointer type |
| `TYPE_ARRAY` | fixed-array type |
| `TYPE_SLICE` | slice type |
| `TYPE_STRUCT` | nominal struct |
| `TYPE_UNION` | untagged nominal union; operations need a V2 safety decision |
| `TYPE_TAGGED_UNION` | tagged nominal union |
| `TYPE_ENUM` | nominal enum |
| `TYPE_FUNCTION` | function type |
| `TYPE_TUPLE` | tuple type |
| `TYPE_UNRESOLVED` | V2 inference state; no C shape |
| `TYPE_OPAQUE` | extern opaque nominal |
| `TYPE_OPTIONAL` | optional type |
| `TYPE_NONE` | contextual none state and `NoneOptional`; no independent C type |
| `TYPE_GENERIC_FUNCTION` | generic function declaration/template and specialization |
| `TYPE_GENERIC_TYPE_DECL` | generic type declaration/template and specialization |

### Exact operator coverage index

| V1 operator constant | Source operator | V2 status |
|---|---|---|
| `BINOP_ADD` | `+` | supported; integer helper matrix is partial |
| `BINOP_SUB` | `-` | supported; integer helper matrix is partial |
| `BINOP_MUL` | `*` | supported; integer helper matrix is partial |
| `BINOP_DIV` | `/` | supported; integer helper matrix is partial |
| `BINOP_MOD` | `%` | supported for integers; helper matrix is partial |
| `BINOP_EQ` | `==` | supported for numeric, bool, char, pointer, enum, and string values; V1 does not admit aggregate equality |
| `BINOP_NE` | `!=` | supported for numeric, bool, char, pointer, enum, and string values; V1 does not admit aggregate inequality |
| `BINOP_LT` | `<` | supported by ordered numeric, character, and string rules |
| `BINOP_LE` | `<=` | supported by ordered numeric, character, and string rules |
| `BINOP_GT` | `>` | supported by ordered numeric, character, and string rules |
| `BINOP_GE` | `>=` | supported by ordered numeric, character, and string rules |
| `BINOP_AND` | `&&` | supported with short-circuit TIR |
| `BINOP_OR` | `||` | supported with short-circuit TIR |
| `BINOP_BIT_AND` | `&` | checker support exists; backend width matrix is partial |
| `BINOP_BIT_OR` | `|` | checker support exists; backend width matrix is partial |
| `BINOP_BIT_XOR` | `^` | checker support exists; backend width matrix is partial |
| `BINOP_BIT_SHL` | `<<` | checked-shift helper matrix is partial |
| `BINOP_BIT_SHR` | `>>` | checked-shift helper matrix is partial |
| `UNOP_NEG` | unary `-` | checked integer and direct float paths; width proof is partial |
| `UNOP_NOT` | `!` | supported for bool; optional force unwrap is a separate postfix node |
| `UNOP_ADDR` | `&` | supported through the place model; aggregate cases are partial |
| `UNOP_DEREF` | unary `*` | supported through dereference places; whole structs work, while other aggregate value positions remain partial |
| `UNOP_BIT_NOT` | `~` | checker support exists; backend width matrix is partial |

V1 compound assignment supports `+=`, `-=`, `*=`, `/=`, and `%=`. V2
represents the same five operations as `CompoundStore`, and it also represents
postfix `++` and `--` through `CompoundStore`. V2 has focused support for
locals, indexed places, fields, and pointer writes, but the same integer
helper-width gaps apply. Float `%=` is correctly rejected.

## Completion gate

This audit is not complete. The fourth serious source and runtime pass removed
stale claims and exposed new gaps, but a complete parity claim needs all of these
conditions:

1. Every V1 AST kind in `src/ast.h` has a row above.
2. Every V1 conversion branch has a row above.
3. Every V1 structural member and switch subject has a row above.
4. Every live V2 backend clean-rejection branch has either a matching V2
   language restriction or a focused reproduction.
5. Each **Implemented, proof needed** row has a compile-link-run test for its
   meaningful type and value-shape matrix.
6. The next selected failure is copied to proposal 13 with its exact source,
   error, cause, priority, and one small investigation or implementation
   slice. The remaining failures stay in this ledger.
7. The audit file receives a non-colliding proposal number. Proposal number
   14 is also used by `14-pointer-arithmetic.md`.

Until these conditions are true, this document must not state that V2 has
full V1 parity.
