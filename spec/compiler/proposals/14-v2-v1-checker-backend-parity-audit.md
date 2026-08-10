# V1 checker and backend parity audit for compiler v2

**Status:** in progress; third serious source pass complete on 2026-08-08

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

## Declaration and module ledger

| V1 behavior | V1 source | V2 behavior | Status |
|---|---|---|---|
| Pebble function declaration with parameters, result, body, and hidden context | `checker.c` function passes; `codegen.c` prototypes and bodies | `FunctionDeclaration`, reachability walk, helper prototypes, definitions, and context actions | **Implemented, proof needed** for the full parameter/result matrix |
| Direct and mutual helper recursion | V1 emits prototypes before bodies | V2 emits helper prototypes and has direct and mutual recursion run tests near `emit_test.go:6155` | **Verified** |
| A recursion cycle through `main` | V1 gives the source entry an internal callable symbol and emits prototypes | V2 rejects the cycle in `emit.go:976-1030`; focused rejection test at `emit_test.go:6329` | **Absent** |
| Extern C function | V1 emits `extern` declarations and calls the registered C name | V2 validates C signatures and emits extern calls | **Implemented, proof needed** for all accepted parameter/result types |
| Library-named extern block | V1 stores and emits the library name as a C comment/declaration group | V2 syntax and symbols exist | **Implemented, proof needed**; tracker verification queue item |
| Opaque extern type | V1 resolves and emits an incomplete C type | V2 extern type paths exist | **Implemented, proof needed**; tracker verification queue item |
| Extern variable and extern constant | `codegen.c:509-620` emits C data declarations | V2 has no extern-data TIR or emission path. `ExternDeclaration` is function-shaped and the backend treats it only as callable | **Absent** |
| Module-level mutable variable storage | V1 emits declarations, definitions, reads, and writes | V2 creates `GlobalDeclaration`, but the backend emits no global storage. The IR comment at `check/ir_builder.go:482-491` confirms that no storage scheme exists | **Absent** |
| Module-level immutable constant | V1 emits global C storage; initializer is limited to a simple literal | V2 inlines immutable global values at each use and has a richer constant evaluator | **Partial**, with a V2 extension; prove every supported constant value shape |
| Uninitialized local or global variable with an explicit type | V1 accepts it and C zero-initializes the object | V2 requires an initializer for each non-extern `let` or `var` | **Intentional difference** |
| Constant declaration | V1 requires an initializer and limits global initialization to literals | V2 constant evaluation supports references, unary and binary expressions, enums, cycle checks, and budgets | **Verified V2 extension** for the evaluator; backend value-shape proof is still needed |
| Import and qualified module value/function/type paths | V1 module member lookup and registered names | V2 module builder, `Path`, and symbol resolution | **Implemented, proof needed** by symbol category |
| Type declaration | V1 registers struct, union, tagged union, enum, alias, and generic declarations | V2 type declaration facts and specialized nominal types | **Partial**; see the type ledger |
| Instance method | V1 resolves a receiver and emits it as the first call argument | V2 `MethodCall` and receiver validation | **Implemented, proof needed** for all owner/value shapes |
| Associated function selected as `Type.method(...)` | V1 distinguishes methods and associated functions | V2 has no receiver value for a qualified type path and rejects the call | **Absent**; tracker item 4 |
| Calling-convention annotation | V1 supports Pebble and C conventions, including a C-convention function body | V2 accepts bodies only for the Pebble convention. C convention is an extern boundary | **Intentional difference** |
| `inline` function annotation | V1 writes the C `inline` keyword | V2 keeps `Inline` in TIR as an optimization request, but the backend does not write a C keyword | **Intentional difference**; no language semantic is missing |
| Anonymous non-capturing function | V1 hoists it to module scope and cannot capture a local | V2 `HoistedFunctionValue` does the same | **Implemented, proof needed** |
| Capturing closure | V1 anonymous functions use module scope as parent and cannot capture a local | V2 reports C0617 for a capture | **Intentional difference from closure languages; parity with V1** |
| Generic anonymous function | V1 rejects it | V2 reports C0608 | **Intentional difference from generic named functions; parity with V1** |
| Entry with no parameters | V1 and V2 emit a C bridge | V2 has focused backend tests | **Verified** |
| ~~Entry with one `[]str` parameter~~ | V1 builds an argument slice | **RESOLVED (`fb94640`).** The C bridge now builds the `[]str` via a pre-existing, previously-unused runtime helper (`pebble_rt_args_from_argv`) instead of discarding `argc`/`argv`. `argv[0]` (program name) included, matching both V1 and that helper's own convention. Independently verified with a real compiled binary run against actual OS argv, causation-checked. | ~~**Absent**; tracker item 5~~ |
| Entry with `argc` and `argv` parameters | V1 accepts the old two-parameter form | V2 rejects it | **Intentional difference** |

## Type and member ledger

| V1 type or member | V2 behavior | Status |
|---|---|---|
| `int`/V1 `isize`, `uint`/V1 `usize`, `i8/i16/i32/i64`, `u8/u16/u32/u64`, `f32/f64`, `bool`, `char`, `void` | All exist in V2; V2 uses `int` and `uint` as the pointer-width signed and unsigned names | **Implemented, proof needed** for each operation and ABI width |
| Pointer `*T`, address-of, dereference, `nil` | V2 has `AddressOf`, `DereferencePlace`, `Load`, and pointer conversions | **Partial**; whole dereferenced aggregates remain absent |
| Fixed array `[N]T` | V2 has `ArrayValue`, `ArrayRepeat`, array locals, indexing, and `.len` | **Partial** by element and whole-value shape |
| Slice `[]T` | V2 has slice types, `.len`, `.data`, checked index/slice, and `SliceFromRaw` | **Partial** by source position and element type |
| V1 pointer slice `ptr[start:end]` | V2 rejects pointer slicing and provides std-only `slice ptr, count` | **Intentional difference** under the pointer-safety design |
| Struct | V2 record construction, fields, methods, parameters, results, and C typedefs exist | **Partial**; runtime nominals and whole-value paths are incomplete |
| Tuple | V2 tuple construction, elements, parameters, results, and `TupleCoerce` exist | **Partial**; whole-value copies and focused coercion proof are missing |
| Optional `?T`, `some`, `none`, force unwrap | V2 has optional construction, injection, and checked unwrap | **Partial** by payload type |
| Enum | V2 construction, switch labels, and integer conversions exist | **Verified** for enum-to-integer and integer-to-enum; aggregate containers remain partial |
| Tagged union | V2 construction and ordinary switch narrowing exist | **Partial**; generic-self narrowing is tracker item 3 |
| Untagged union | V1 emits a C union and permits construction and member access | V2 rejects construction, read, and write because no safety rule is accepted | **Decision needed** |
| Function type and function value | V2 supports Pebble-convention, non-variadic function values for a limited C-representable signature set | **Partial**; V1 supports a wider convention and signature surface |
| Opaque extern type | V2 represents it and rejects invalid `sizeof` use | **Implemented, proof needed** |
| Generic type and specialization | V2 supports generic nominal types and specialization | **Partial**; owner type-parameter inheritance is tracker item 8 |
| Recursive nominal type | V2 collection has dependency ordering and recursion paths | **Partial**; a self-reference plus slice field currently breaks C typedef identities in tracker item 2 |
| Tuple member `.0`, `.1`, and so on | V1 and V2 resolve tuple ordinals | **Implemented, proof needed** |
| Array `.len` | V1 and V2 support it | **Implemented, proof needed** |
| Slice `.len` and `.data` | V1 and V2 support both | **Implemented, proof needed** |
| String `.len` | V1 string code uses `strlen` but has no structural member | V2 exposes byte length as `.len` | **Verified V2 extension** in real string consumers |
| Optional presence member | V1 spells it `.is_some` | V2 spells it `.has_value` | **Intentional rename** |
| Struct field and instance method selection | Both compilers support it | **Partial** by generic and runtime owner shape |
| Enum and union variant selection | Both compilers support it | **Partial** for generic receivers and untagged unions |
| Static member call through a type | V1 supports associated functions | V2 member and call records cannot represent the call without a receiver value | **Absent** |

## Literal, expression, and operator ledger

| V1 expression or operator | V2 behavior | Status |
|---|---|---|
| Integer literal | V2 preserves text, constrains range, and lowers by width | **Implemented, proof needed** for all boundaries |
| Float literal | V2 supports f32/f64 lowering | **Implemented, proof needed** for suffix and range behavior |
| Boolean literal | V2 supports it | **Verified** in conditions, print, calls, and aggregates |
| Character literal | V2 stores a Unicode scalar and uses integer C storage | **Verified** |
| String literal | V1 is a NUL-terminated C pointer; V2 is `PebbleStr {data,len}` | **Intentional ABI and semantic difference** |
| `nil` pointer | V2 `NilPointer` | **Implemented, proof needed** by pointer type |
| `none` and `some value` | V2 optional nodes | **Partial** by payload shape |
| Context expression | V1 and V2 expose the hidden allocator/context value | **Partial** while `Allocator` and `Context` ordinary-struct redesign is open |
| Identifier, module member, partial member | V2 symbol and member value paths | **Partial** by declaration category |
| Grouped expression | Parser-only grouping in both compilers | **Verified** by construction; no backend behavior |
| Interpolated string value | V1 materializes a string expression and formats string, bool, signed/unsigned integer, float, char, enum, struct, and tuple parts | V2 builds `InterpolatedString` TIR, but the backend can consume it only as a direct `print` operand and only when every value part is bool | **Absent except for one narrow print form** |
| Direct call | V2 supports helper and extern direct calls | **Implemented, proof needed** for the signature matrix |
| Indirect call | V2 supports non-capturing function values | **Partial** by function signature |
| Method call | V2 supports instance calls | **Partial** by owner and argument shape |
| Generic call | V2 specializes named generic functions | **Implemented, proof needed** for nested type arguments |
| Index | V1 checks array, slice, string, and pointer indexing; V2 checks array, slice, and string and uses Unicode decode for string reads | **Intentional string change** and **partial** aggregate proof |
| String index result | V1 returns one byte; V2 stores bytes but walks UTF-8 from the start and returns the scalar at the requested code-point index | **Verified V2 semantic change** |
| Slice expression | V2 checked slices work when the builder can carry required pre-statements | **Partial**; pure nested expression positions are tracker item 6 |
| Tuple literal | V2 `TupleValue` | **Implemented**, but whole-value copy paths are partial |
| Array literal and repeat | V2 `ArrayValue` and `ArrayRepeat` | **Partial** by element and destination shape |
| Struct literal | V2 `RecordConstruct` | **Partial** for runtime nominal and nested aggregate shapes |
| Tagged-union variant literal | V2 `VariantConstruct` | **Partial** for generic narrowing and payload shape |
| `sizeof(T)` | V1 rejects opaque types but otherwise delegates to C | V2 checker rejects function, void, and opaque extern types. The backend also rejects fixed arrays and all other C-unspelled types, although the checker accepts arrays | **Partial; checker/backend contract defect for fixed arrays** |
| Force unwrap | V2 checked optional unwrap | **Partial** by payload type |
| Postfix `++` and `--` as a value expression | V1 uses C postfix semantics and returns the old value | V2 defines them as void updates that are legal only as statements or for updates | **Intentional difference** |
| Arithmetic `+ - * / %` | V1 emits raw C arithmetic for all numeric types | V2 uses checked helpers for integers and direct C for floats | **Partial**; helper-width matrix is incomplete |
| Numeric comparisons `== != < <= > >=` | Both compilers support numeric comparisons | **Implemented, proof needed** for mixed widths and floats |
| String equality and ordering | V1 uses C string comparison paths | V2 has length-aware string runtime paths | **Implemented, proof needed** for all six operators |
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
| Character to integer | explicit | explicit | **Implemented, proof needed** |
| Integer to character | explicit | forbidden | **Intentional difference** until Unicode scalar validation is specified |
| Enum to integer | explicit | explicit | **Verified** |
| Integer to enum | absent as a general V1 cast; V1 has partial enum inference | explicit checked cast, plus optional checked form | **Verified V2 extension** |
| Pointer to pointer | explicit; `*void` conversions are also implicit | explicit only | **Intentional stricter rule** |
| Pointer to integer | explicit | explicit | **Implemented, proof needed** by width |
| Integer to pointer | explicit | forbidden | **Intentional difference** |
| V1 `str` to/from `*void`, `*u8`, or `*char` | explicit or implicit, because V1 `str` is a C pointer | absent for V2 `PebbleStr` | **Intentional ABI difference**; use explicit library adapters if accepted later |
| Fixed array to slice | implicit | dedicated checked slice shape, not general compatibility | **Partial**; direct non-primitive literal-to-slice is tracker item 7 |
| Tuple literal element conversion | implicit, equal tuple length | `TupleCoerce`, equal length and per-element compatibility | **Implemented, proof needed** |
| Explicit tuple prefix cast | source can have more elements than destination | V2 requires equal length | **Absent** unless the narrower V2 rule is accepted |
| Array literal element conversion | implicit for equal length | no general structural conversion class | **Partial/absent**; isolate by destination shape |
| Struct literal field conversion | implicit for equal field count and matching names | no structural struct conversion class | **Absent** |
| Explicit structural struct prefix cast | source prefix can cast to a smaller destination struct | forbidden | **Absent** unless nominal-only conversion is accepted |
| `none` to any optional | implicit | contextual optional construction | **Implemented, proof needed** |
| `some S` to optional `T` with payload conversion | implicit for a literal `some` | optional injection exists, but payload and backend shapes are limited | **Partial** |
| Single-field struct literal to matching union variant | implicit | V2 uses explicit `VariantConstruct` syntax/facts | **Intentional representation difference** for tagged unions; untagged union is undecided |

## Print and interpolation matrix

V1 `get_format_specifier`, `build_composite_format_string`, and
`build_composite_args` are at `codegen.c:1754-1867`. V1 recursively prints
structs, tuples, arrays, nested composites, and enum variant names. V2
`valuePrintable` at `check/control_flow_validation.go:111` and `buildPrint`
at `backend/emit.go:6503` accept only scalar values.

| Printed value | V1 | V2 | Status |
|---|---|---|---|
| Integer, float, bool, string | supported | supported | **Verified** for common widths; complete width proof needed |
| ASCII character | supported | supported | **Verified** |
| Multi-byte Unicode character | V1 C `%c` is byte-limited | V2 uses `pebble_rt_char_to_utf8` and has `é`, emoji, mixed, and deferred-print run tests | **Verified V2 fix/extension** |
| Enum name | supported | rejected by V2 checker | **Intentional difference** in tracker decisions |
| Struct, tuple, and fixed array | recursively formatted | rejected by V2 checker | **Decision needed** for V1 parity |
| Nested composite | recursively formatted | rejected | **Decision needed** |
| Interpolation | V1 formats string, bool, integer, float, char, enum, struct, and tuple parts and produces a string value | V2 supports only bool value parts and only when the interpolation is a direct `print` operand | **Absent except for the verified bool-print form** |

The earlier audit claim that multi-byte `%c` remained open was stale. The
runtime helper is in `runtime/src/str.c:141`, its ABI is in
`runtime/include/pebble_rt.h:433`, and backend tests are near
`emit_test.go:4600` and `emit_test.go:4850`.

## Control-flow ledger

| V1 control behavior | V2 behavior | Status |
|---|---|---|
| Block, expression statement, return, implicit return | V2 has direct TIR nodes and backend builders | **Implemented, proof needed** by result/value shape |
| Discard an arbitrary non-void expression as a statement | V1 checks the expression and discards its result | V2 permits only calls, postfix updates, and other expressions whose solved result is void; C0612 rejects other non-void results | **Intentional stricter rule** |
| If/else and terminal-path analysis | V2 validates and emits nested arms | **Verified** for ordinary scalar and aggregate paths; value-shape limits remain |
| While and infinite `loop` | V2 emits while loops and accepts exhaustive terminal loops | **Verified** for ordinary paths |
| Range loop, exclusive and inclusive | Both compilers support both end rules | **Partial** |
| Range-bound evaluation count | V1 stores both start and end before the C loop and evaluates each once | V2 evaluates the start once in the C initializer, but writes the end expression directly in the loop condition, so a call or mutable read can be evaluated on every iteration | **Absent V1 semantics; backend defect** |
| Descending range | V1 evaluates both bounds once, chooses step `1` or `-1`, and runs in either direction at `codegen.c:2568` | V2 always writes `<`/`<=` and `iterator++` at `emit.go:4605-4659`; a descending range runs zero times | **Absent** |
| Implicit range iterator named `iter` | V1 creates it when no name is present | V2 requires an explicit iterator name | **Intentional difference** |
| Range iterator type | V1 registers and emits the iterator as `int` | V2 gives the iterator the exact shared bound type | **V2 extension/correction** |
| Classic `for` with declaration initializer | Both compilers support it | **Verified** for current V2 scalar forms |
| Classic `for` with assignment initializer | V1 accepts and emits it | V2 checker can produce it, but backend rejects it; `emit.go:4773` records this reachable form | **Absent; checker/backend contract defect** |
| Classic `for` with expression update | V1 accepts any checked expression | V2 accepts only assignment, compound assignment, or postfix update | **Intentional narrower rule** |
| Optional classic-for clauses | V2 accepts omitted clauses and lowers them | **Verified V2 surface** |
| Break and continue in loops | Both compilers support them | **Verified**, including defer cleanup |
| Break that targets a switch | V1 permits break only in a loop | V2 control regions permit switch break | **V2 extension** |
| Defer LIFO and cleanup on return, break, and continue | V1 emits deferred statements at exits | V2 has focused compile-run tests for LIFO, nested scopes, helper calls, return, break, continue, compound store, and Unicode print at `emit_test.go:9602-9763` | **Verified for the supported deferred statement kinds** |
| Deferred reassignment, compound assignment/postfix update, print, or void call | V1 accepts and emits each as an ordinary deferred statement | V2 `buildDeferredStatements` supports `Store`, `CompoundStore`, `Print`, and a void-call `ExpressionStatement` | **Verified** |
| Deferred local declaration | V1 accepts it and emits it in a defer-local C block | V2 checker permits deferred bindings, but `emit.go:6104` explicitly rejects `Initialize` | **Absent; checker/backend contract defect** |
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
| Integer | accepted and emitted as C switch | accepted and emitted | **Implemented, proof needed** by width |
| `u8` or `i8` with all 256 values | V1 treats it as exhaustive | V2 does not enumerate integer domains | **Absent** |
| Character | accepted and emitted as C switch | checker accepts it; backend case labels accept only integer, bool, or enum | **Confirmed checker/backend defect** |
| String | accepted and emitted as `strcmp` if/else chain | checker accepts it; backend has no string-switch path | **Confirmed checker/backend defect** |
| Boolean | V1 rejects it | V2 accepts it and proves `true` plus `false` exhaustive | **Verified V2 extension** |
| Enum | accepted, duplicate-checked, exhaustive, emitted | V2 supports it | **Verified** for ordinary enums |
| Tagged union | accepted, narrowed, exhaustive, emitted by tag | V2 supports ordinary unions | **Partial** for generic-self narrowing |
| Default `else` | supported | supported | **Implemented, proof needed** for each subject kind |
| Multiple labels on one case | supported | supported | **Implemented, proof needed** by kind |
| Duplicate constant labels | rejected | rejected | **Verified** for scalar and nominal cases |

## Calls and variadic arguments

| V1 behavior | V2 behavior | Status |
|---|---|---|
| Fixed Pebble parameters | supported | supported | **Implemented, proof needed** by value shape |
| Trailing Pebble slice parameter marked variadic | V1 collects zero or more tail elements | V2 collects zero or more tail elements into a temporary slice | **Verified** for int, bool, zero tail, and fixed-prefix tests near `emit_test.go:12762` |
| One existing slice as the sole variadic tail | V1 detects the matching slice and passes it directly at `codegen.c:4000-4068` | V2 validates every tail argument as one element and always builds a new slice | **Absent**; focused reproduction needed before tracker entry |
| C variadic extern call | V1 permits primitive C variadic use | V2 reports C0604 | **Decision needed**; do not infer a target from V1 alone |
| Aggregate argument, result, and receiver | V1 C value passing handles ordinary C-representable aggregate values | V2 has many implemented paths, but each type and source expression has a separate builder | **Partial**; see the backend shape table |

## Backend whole-value and aggregate-shape ledger

These are live explicit rejection branches in `backend/emit.go`. V1 usually
gets these operations from ordinary C value copy. A source-level rejection is
strong evidence of missing backend capability, but each row still needs one
small source reproduction before it moves to the issue tracker.

| Value shape | V2 source evidence | Status |
|---|---|---|
| Reassign a whole tuple local | `emit.go:5520` rejects it | **Absent** |
| Reassign a whole fixed-array local | `emit.go:5523` rejects it | **Absent** |
| Reassign a whole struct local | `emit.go:5543` rejects it | **Absent** |
| Reassign a `str` local from another string value | `emit.go:5474` accepts only a string literal | **Partial** |
| Initialize a tuple local from another tuple value | `emit.go:7190` accepts only a tuple literal or helper result | **Partial** |
| Initialize an array local from another array value | `emit.go:7418` accepts only a literal or repeat | **Partial** |
| Initialize a struct local from another struct value | `emit.go:8474` accepts only record construction or helper result, with a few special indexed paths elsewhere | **Partial** |
| Initialize an enum local from another enum value | `emit.go:8826` accepts only a variant literal or integer cast | **Partial** |
| Initialize a `str` local from another `str` value | `emit.go:9290` accepts a limited literal/call grammar | **Partial** |
| Materialize an interpolated string as a local, argument, result, or ordinary value | `InterpolatedString` is handled only inside `buildPrint`; general string builders reject it | **Absent** |
| Enum-typed fixed-array element | `emit.go:7442` and `emit.go:8148` reject it | **Absent** |
| Enum-typed slice element | `emit.go:8186` rejects it | **Absent** |
| Ordinary `some Color.red` optional enum payload | `emit.go:8270` accepts only the integer-to-optional-enum cast path | **Partial** |
| Aggregate nesting deeper than one dependency level | `emit.go:2026` rejects it | **Absent**; the accepted C-layout graph needs investigation |
| Whole dereferenced struct as a value | explicit checker/backend boundary gap | **Absent**; tracker item 9 |
| Runtime `Allocator` argument, result, and field assignment | special runtime nominal paths do not form one ordinary value model | **Absent**; tracker item 1 and proposal 15 |
| Array literal of non-primitive values directly assigned to a slice local | checker reports C0601 | **Absent**; tracker item 7 |
| Slice-typed struct field passed as an argument | backend accepts slice locals but rejects this field source shape | **Absent**; recorded under tracker item 2 |
| Inline checked slice inside a nested pure expression | required pre-statement cannot travel through all expression builders | **Absent**; tracker item 6 |
| Function value with C convention, variadic signature, or unsupported aggregate result | `validateFunctionTypeSignature` near `emit.go:3069` restricts the signature | **Partial** |

## Backend C-shape capability matrix

V1 emits C type names recursively for arrays, slices, tuples, optionals,
structs, unions, tagged unions, enums, and function types. Its dependency
walk includes the child types of each of these shapes. V2 has separate C-type
gates for each container. These gates are not one shared language rule, and
they do not accept the same child types.

| V2 C position | Accepted by the backend | Rejected or defective V1 shapes |
|---|---|---|
| Fixed-array element | any C-spellable integer, `bool`, `char`, `str`, tuple, optional, or non-enum nominal struct | enum, tagged union, float, pointer, array, slice, and function value |
| Slice element | any C-spellable integer, `bool`, `char`, tuple, optional, or non-enum nominal struct | `str`, enum, tagged union, float, pointer, array, nested slice, and function value |
| Tuple element | only the enclosing entry width, `bool`, tuple, optional, or non-enum nominal struct | other integer widths, `char`, `str`, float, enum, tagged union, pointer, array, slice, and function value |
| Optional payload storage | any C-spellable integer, `bool`, tuple, nominal struct or enum, and a C-spellable pointer | `char`, `str`, float, array, slice, optional, and function value. A tagged-union payload is classified as an enum and receives the tag-only enum C name instead of the union C name |
| Struct field | any C-spellable integer, `bool`, `str`, tuple, optional, runtime nominal, nominal struct or enum, C-spellable pointer, slice, or admitted function value | `char`, float, array, and nested forms rejected by their own gates. A tagged-union field is classified as an enum and receives the tag-only enum C name instead of the union C name |
| Tagged-union variant payload | only the enclosing entry width, `bool`, or `str` | all other integer widths, `char`, float, tuple, struct, enum, union, pointer, array, slice, optional, and function value |
| First-class function parameter | any C-spellable integer, `bool`, `char`, `str`, or C-spellable pointer | float and every aggregate, enum, union, optional, slice, array, or function value |
| First-class function result | enclosing entry width, `u64`, `bool`, `char`, `void`, or C-spellable pointer | `uint` when it is not the entry width, other integer widths, `str`, float, and every aggregate, enum, union, optional, slice, array, or function value |
| C extern parameter/result | C-spellable integer, `bool`, `char`, `str` as `const char *`, float, C-spellable pointer, and `void` result | aggregate, enum, union, optional, slice, array, function value, or opaque value by copy |
| `sizeof` type | C-spellable integer, `bool`, `char`, `str`, runtime nominal, tuple, optional, slice, plain enum, struct, or C-spellable pointer | fixed array, function type, `void`, and opaque extern nominal. The fixed-array rejection is later than the checker. A tagged union is classified as an enum, so the backend selects the tag enum size instead of the full tagged-union storage size |

The tagged-union field and optional-payload rows are stronger than a missing
feature. `isStruct` treats every nominal as a struct, and `isEnumType` treats
plain enums and tagged unions as enum-shaped. `structFieldCType` and
`optionalPayloadCType` then select `enumTypeName`, while a payload-carrying
tagged union needs `unionTypeName`. A focused reproduction must prove whether
this produces a clean emitter error or invalid/truncated C storage.

The ordinary helper-result switch has a related defect. It has no enum or
tagged-union result branch. Its `isStruct` branch catches every nominal first,
uses `structTypeName`, and records struct return rules. Plain-enum and tagged-
union helper results therefore need separate reproductions. V1 passes both by
their real C value type.

## Backend value-source position matrix

V1 normally emits any already-typed C value expression in these positions.
V2 selects a separate builder by destination type and then accepts a short
list of source-node shapes. These are source-reachable limits, not only checks
for damaged hand-written TIR.

| Position | V2 accepted source shapes | Missing source shapes |
|---|---|---|
| Slice call argument | a matching slice local or parameter; a fresh checked slice when the enclosing call position can carry its leading temporary | slice field, slice-returning call, raw slice construction, and fresh checked slice in a pure nested expression position |
| Enum or tagged-union call argument | a matching local or parameter | inline enum variant, inline tagged-union variant, field value, and helper result |
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
| Grouped value (`SourceAlias`) | unwrapped by some type-specific builders | the general integer-expression builder still rejects it in uncovered positions, so parentheses are not uniformly transparent in the backend |
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
| Checked add, subtract, multiply | `i32`, `i64`, `u64` | no helper suffix for `int`, `uint`, `i8/i16`, or `u8/u16/u32`; exact treatment of `int` depends on its resolved width |
| Checked divide and modulo | `i32`, `i64` | no `u64`, `uint`, or narrow-width helper |
| Checked shift left and right | `i32`, `i64`, `i8/i16`, `u8/u16/u32` | no `uint` or `u64` helper |
| Checked integer negation | fixed signed widths supported through dedicated paths | prove `int` resolution and narrow signed widths |
| Float-to-integer checked conversion | helper family is limited to `i32` and `i64` destinations | no full integer destination matrix |
| Optional unwrap payload | scalar helper family covers `i32`, `i64`, `u64`, bool, and pointer paths | narrow integer payload matrix is incomplete |
| Explicit wrapping multiplication and addition | `wrapping_mul_u64` and `wrapping_add_u64` lower to runtime helpers | **Verified** in SAFE and RELEASE runtime tests and backend run tests |

The earlier audit claim that wrapping `u64` operations remained open was
stale. The helpers are in `runtime/src/arith.c:425-431`, their ABI is in
`runtime/include/pebble_rt.h:182-191`, and backend tests are near
`emit_test.go:11000`.

Tracker item 2 records the missing helper suffixes and missing `uint`/`u64`
shift helpers, but it does not yet state the complete width-by-operation
matrix above.

## Confirmed open tracker items

This section mirrors only the current open items in proposal 13. It does not
copy their full reproduction or plan.

1. `Allocator` values cannot cross function boundaries; proposal 15 plans an
   ordinary-struct redesign for `Allocator` and `Context`.
2. The arena rewrite exposes struct/slice typedef identity errors and missing
   checked-arithmetic suffixes. The remaining arena functions still need the
   slice-and-offset rewrite.
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
4. Qualified static methods are unsupported.
5. ~~`main(argv []str)` cannot receive C process arguments.~~ RESOLVED (`fb94640`).
6. Inline slice construction fails in pure nested expression positions.
7. A non-primitive array literal cannot directly initialize a slice local.
8. A generic struct method cannot inherit its owner type parameter.
9. A whole dereferenced struct cannot become a value.
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

## Tracker verification queue mirror

These are not confirmed failures. They remain small read-only or test-only
investigations in proposal 13:

1. Library-named extern blocks.
2. Opaque extern types.
3. Three-level nested generic types.
4. `TupleCoerce` backend reachability and emission.
5. Confirmation that `TypeUse` is compile-time-only and needs no backend
   node.
6. A fresh audit of `open-language-decisions.md`, because some old status
   text is known to be stale.

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

## New findings that proposal 13 must reconcile

No production work must start from this list. Each source-only finding needs
one small reproduction. Then proposal 13 must receive the reproduction and
root cause before an Orc implementation task starts.

| Finding | Confidence | Next small slice |
|---|---|---|
| ~~V2 checker accepts character switch, but backend rejects it~~ | **RESOLVED (`72f0207`).** Added an `isChar` branch to the switch-subject builder (reusing `buildCharOperand`) and a char-literal branch to `buildCaseLabel`. 6 new tests including a non-ASCII scalar case. Independently verified, causation-checked. | — |
| ~~V2 checker accepts string switch, but backend has no lowering~~ | **RESOLVED (`49d0f23`).** Different shape than the char fix — C switch can't take non-integer labels, so this lowers to an if/else-chain via the existing `pebble_rt_str_eq` helper instead of a native switch. `break` targeting the switch is handled via a `do{}while(0)` wrapper; documented accepted limitation where that wrapper would incorrectly intercept a `continue` to an enclosing loop in the rare case both are present together. 3 new tests, independently verified, causation-checked. | — |
| ~~V2 does not prove a complete `u8` or `i8` switch exhaustive~~ | **RESOLVED (`4817dae`).** Added covered-integer-value tracking plus a full-range check for `u8`/`i8` specifically (256-value domains, small enough to enumerate) — wider widths are explicitly unaffected, still always require a fallback. 5 new tests, independently verified at the checker level, causation-checked. | — |
| **New, found while verifying the row above:** `u8` (and presumably other non-entry-width integers) is not accepted as a switch subject type by the *backend* at all — `switch subject has type u8, want int, bool, or char, or an enum/tagged-union type` — even though the checker now correctly proves a full `u8` switch exhaustive. Same class of gap as the already-fixed char-switch item, just never extended past `char`. | high, directly reproduced | one compile-to-C reproduction: a full-coverage `u8` switch that clears the checker but fails at backend emission |
| **New, found while verifying the row above:** `u8` (and presumably other non-entry-width integers) is not accepted as a function *parameter* type at all — `called function symbol N parameter 0 has type u8, want int, bool, char, str, f32, f64, ...` (the accepted-types list doesn't include `u8`/`u16`/`i8`/`i16`, only the entry width, `uint`, `u64`, and a handful of others). | high, directly reproduced | one compile-to-C reproduction: a helper taking a `u8` parameter, called with a `u8`-typed local argument |
| ~~Descending range loops execute zero iterations~~ | **RESOLVED (`8baeb8e`).** Fixed for the literal-bound case (direction known statically); non-literal bounds are unchanged (always ascending in the current grammar — not yet independently confirmed whether the grammar actually forbids a descending non-literal-bound range or just doesn't need to distinguish it, worth a follow-up check if that syntax is ever used). 4 new tests; independently verified descending now runs the correct count, ascending/zero-length unregressed, causation-checked by reverting and reproducing. | — |
| ~~A nontrivial range end is evaluated on every loop test instead of once~~ | **RESOLVED (`e111c37`).** Non-literal end bounds now evaluated once into a C temp before the loop; literal bounds unchanged. Composes correctly with the descending-range fix in the same function. 3 new tests; independently verified a side-effecting bound now called exactly once (was 4x for a 3-iteration loop), causation-checked. | — |
| ~~Mutable globals have no backend storage~~ | **RESOLVED (`14739f3`).** Real backend storage for both read and write, supporting integer/uint/bool/char/float/str/plain-enum globals. Required a small necessary checker/TIR companion (`bindingGlobalVar`'s initializer is now recorded as a real TIR node, not just validated and discarded). 13 new tests; independently verified with real cross-function/in-loop mutation, causation-checked across all 10 touched files. | — |
| **New, found while fixing the row above:** a global's constant initializer that isn't a literal leaf (e.g. `var x int = 1 + 2;`, which lowers to `CheckedArithmetic`) is cleanly rejected as not C-static-initializable — only literal initializers are currently supported. | high, directly reproduced and documented as an intentional scope boundary while landing the row above | backend constant folding, or checker-side folded-constant serialization, so a compile-time-constant arithmetic expression can also initialize a global |
| ~~Extern variables and constants have no backend declaration/use path~~ | **RESOLVED (`1372734`).** Reuses the mutable-globals fix's pattern (`14739f3`), but emits a forward `extern <ctype> <realCName>;` declaration instead of synthesized storage. Both read and write supported (checker already enforces extern-`let` immutability, so no backend-side mutability distinction needed). Real platform caveat found and documented: `errno` is a macOS header macro, not a linkable symbol, so runtime correctness is proven against a hand-written C shim instead. 9 new tests, independently verified against real hand-written shims (not just the test harness), causation-checked. | — |
| Composite print from V1 is absent | certain, but policy is undecided | ask whether V1 debug formatting remains a language feature |
| Existing-slice variadic pass-through is absent | medium-high from checker and builder structure | one call reproduction |
| General interpolated-string values are absent; only direct bool interpolation in `print` lowers | high from V1 expression code and V2 `buildPrint` special case | one local-value and one multi-type interpolation reproduction |
| ~~V2 checker accepts string `+`, but backend cannot materialize its `BinaryValue` result~~ | **DECIDED + RESOLVED (`f4f2412`, 2026-08-09, direct instruction).** `str + str` (and any `+` with a `str` operand) is now a checker-level type error (`C0603`), not implemented — plain `str` is an immutable view, and real concatenation already has a first-class path via `String.push_str` (`std/string.peb`) with an explicit allocator; implementing `+` would need an implicit one, entangling with the deferred Allocator/Context redesign (proposal 15). Other `+` operand types (int, uint, float, ...) confirmed unaffected. The backend's own defense-in-depth rejection test for this shape is preserved via hand-built TIR, since real source can no longer reach it past the checker. | — |
| Deferred declaration, block, conditional, loop, and switch forms pass V2 validation policy but have no backend lowering | high from the accepted 06b rules and the closed `buildDeferredStatements` switch | one source reproduction per deferred statement family; implement one family per task |
| Assignment-form classic-for initializer reaches TIR but backend rejects it | high; backend documents and tests the rejection | decide whether grammar acceptance is intentional, then track or reject earlier |
| Whole tuple, array, struct, enum, and string copy/reassignment paths are incomplete | high from explicit backend errors | one type and one operation per investigation; do not combine them |
| — struct slice: **RESOLVED (`9df0351`).** Whole struct-local reassignment now works for both a pointer-deref/field write (`*self = other;`) and a plain local (`p = q;`), with the new value either an in-scope struct-typed local of the matching type or a fresh struct literal. Both reproductions independently verified end-to-end (exit 9, causation-checked). Remaining, deliberately out of scope for this slice: reassigning from a struct-returning call expression (`p = make_point();`) still cleanly rejects — a new, narrower open item if picked up later. Tuple, array, enum, and string reassignment remain untouched. | — |
| Enum array/slice elements and ordinary optional enum payloads are incomplete | high from explicit backend errors | one shape per investigation |
| ~~Entry-function recursion cycle is rejected~~ | **DECIDED (2026-08-09, direct instruction):** current V2 behavior (rejecting a call cycle through `main`) is correct and intentional — `main` is the entry point, it should not be callable from anywhere. Move to "Accepted V2 differences" below; not a defect. | — |
| Checked numeric helper coverage is incomplete beyond the two symptoms in tracker item 2 | high from helper tables and suffix functions | build a checker-accepted width/operation matrix, then fix one family per task |
| Each aggregate/container C position accepts a different child-type set | high from the dedicated array, slice, tuple, optional, struct, union, and function-type C-name gates | reproduce one container plus one rejected child type per slice; do not dispatch a general container task |
| ~~A tagged union used as a struct field or optional payload receives the plain-enum C type name~~ | **RESOLVED (`4d1ef51`).** Reproduced two stacked bugs: a typedef-ordering defect (union typedef emitted after first use, hard `cc` failure) and the predicted wrong-type-selection bug underneath it (confirmed real, but caught by `-Werror` as a hard error, not silent). Both fixed in `emit.go`; `structFieldCType`/`optionalPayloadCType` now use the existing `isTaggedUnionType` distinction. 5 new compile-run tests; construct-store-read-back round-trip and panic-on-none independently verified, not just clean compilation. | — |
| ~~`sizeof` a fixed array passes validation but the backend rejects it~~ | **RESOLVED (`cacaa28`).** Added the missing `isArray` branch to `sizeofCTypeName`, plus the same compounding typedef-collection gap as the union fix (`collectHelperArrayTypes` replaced with `collectArrayTypes`, now walking entry/helper bodies for `SizeofType` array references). A bare `sizeof [N]StructType` still fails due to the separate bare-sizeof-of-struct/enum gap logged above — intentionally not expanded here. Independently verified: exact repro compiles/runs (prints 16), causation-checked. | — |
| ~~`sizeof` a tagged union selects the discriminant enum C type instead of the tagged-union C type~~ | **RESOLVED (`f2e8c62`).** Also fixed a compounding bug: nothing forced the union's typedef pair to be collected when `sizeof` was its only reference, so it didn't compile even after the type-selection fix. `isTaggedUnionType` (construction-based) couldn't be reused for `sizeofCTypeName` — added declaration-level `isUnionEnumType` instead. Independently verified: exact tracker repro compiles/runs, causation-checked. | — |
| ~~A bare `sizeof` of ANY plain struct or enum leaves its typedef uncollected~~ | **RESOLVED (`8de0cdb`).** Last piece of the sizeof-collection family (union `f2e8c62`, array `cacaa28`). Extended `collectStructTypesWalk`/`collectEnumTypesWalk` to also collect `SizeofType` type arguments, mirroring the array/union fix exactly. Independently verified: both repros compile and print correct sizes (8, 4), causation-checked. | — |
| ~~A helper that returns a plain enum or tagged union is classified as a struct result~~ | **RESOLVED (`4475579`).** Both plain-enum and tagged-union returns landed together (same shape of change). Added `enumType`/`unionType` fields to `resultInfo`, the missing return-switch cases, `buildReturnStatement` branches, and a required companion fix (a local declared from the call, `let c = pick();`, was separately rejected). 4 new tests, independently verified, causation-checked via file copies. | — |
| ~~An enum/tagged-union-returning `DirectCall` used directly in a general value position still cleanly rejects~~ | **RESOLVED (`2978280`)** for the positions that route through `buildEnumValue`/`buildUnionValueExpr` (switch subject for enums, comparison operands, call arguments) — added the missing `DirectCall` case to both, mirroring the existing `buildUintExpr`/`buildStrOperand` pattern. Independently verified, causation-checked. | — |
| **Corrected/re-scoped:** a tagged union used directly as a **switch subject** specifically (`switch make() { case .value: ...; else: ... }` where `make()` returns a union) was assumed to be a `statements.go` backend gap, but direct reproduction shows it actually fails at the **checker**, not the backend: `error[C0611]: switch case value is not a variant of the subject type` — the checker cannot resolve `.value` as a variant of the subject's type when the subject is a call expression rather than a local/parameter. This may be a third variant of the same-scope-narrowing family already logged below (parameter subjects work, same-scope-`let`-bound-local subjects fail with `C0605`, and now call-expression subjects fail differently with `C0611`) — not yet confirmed whether they share one root cause or are three separate checker gaps. | high, directly reproduced, but root cause not yet fully isolated (checker vs. narrower backend-adjacent gap; relationship to the existing narrowing finding below is unconfirmed) | one checker-level investigation: trace why `.value` fails to resolve as a `Choice` variant specifically when the switch subject is a `DirectCall` node, and whether this is the same code path that already fails for a same-scope local subject |
| Value-source support changes by argument, initializer, return, index, projection, and assignment position | high from the dedicated builders and their clean rejections | reproduce one destination position and one source node shape per slice |
| ~~writing to a tagged-union variant member (e.g. `self.Err = error;`) is rejected with `C0605`~~ | **RESOLVED (`7e7163e`).** Checker's `unionVariantPayloadWrite` accepts a write to a union's own declared variant payload member (pointer, value, or plain-local receiver), still hard-rejecting any non-variant name. Backend's `unionVariantPayloadStoreTarget` + a comma-expression store now sets `.tag` to the correct discriminant on the same write, closing the latent-corruption risk. Also fixed a distinct bug found while wiring this up: `pointerTypeName` had no case for a pointer-to-union pointee at all, so `*SomeUnion` params/receivers emitted a bogus C type name — replaced with `pointerTypeNameForUnit` at every call site. 3 new tests (checker accept/reject, backend compile-run), causation-checked by reverting all touched files to HEAD and confirming the original `C0605` rejection reproduces exactly. | — |
| ~~Tagged-union switch narrowing works when the subject is a function parameter but fails (`C0605`) when the subject is an ordinary same-scope `let`-bound local~~ | **STALE, appears already resolved (2026-08-09).** Re-reproduced the exact quoted repro against current HEAD (both `let` and `var`, both an exhaustive second case and an `else` arm) — all correctly narrow and return the expected value, no `C0605`. Most likely fixed as a side effect of `7b7eee0` (Result[T,E] generic self-narrowing), which touched the same `caseVariantMember`/same-scope narrowing path. No code change made; closed without a new commit. | — |

## Accepted V2 differences and extensions

These items must not return as parity defects without a new language decision:

- explicit loop iterator names;
- no pointer arithmetic;
- no integer-to-pointer conversion;
- no integer-to-character conversion;
- byte-length `PebbleStr` with UTF-8 scalar decode on indexed reads;
- checked integer arithmetic by default, with explicit wrapping builtins;
- no enum name in plain `print`;
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
| Hidden Pebble context forwarding | V1 adds context to Pebble-convention calls | V2 records `ContextForward`, `ContextExpr`, and `ContextIndirect` and writes `PebbleContext *ctx` | **Implemented, proof needed** for indirect and nested call chains |
| Allocator callback ABI bridge | V1 runtime context stores alloc/realloc/free callbacks | V2 emits file-scope adapters for source functions stored in `Allocator` callback fields | **Verified** for construction and invocation, but ordinary `Allocator` value movement is tracker item 1 |
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
| `AST_DECL_EXTERN_VARIABLE` | no matching data-emission node | Declaration and module ledger; **Absent** |
| `AST_DECL_EXTERN_CONSTANT` | no matching data-emission node | Declaration and module ledger; **Absent** |
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
| `UNOP_DEREF` | unary `*` | supported through dereference places; whole aggregates are absent |
| `UNOP_BIT_NOT` | `~` | checker support exists; backend width matrix is partial |

V1 compound assignment supports `+=`, `-=`, `*=`, `/=`, and `%=`. V2
represents the same five operations as `CompoundStore`, and it also represents
postfix `++` and `--` through `CompoundStore`. V2 has focused support for
locals, indexed places, fields, and pointer writes, but the same integer
helper-width gaps apply. Float `%=` is correctly rejected.

## Completion gate

This audit is not complete. The third serious source pass removed stale
claims and exposed new gaps, but a complete parity claim needs all of these
conditions:

1. Every V1 AST kind in `src/ast.h` has a row above.
2. Every V1 conversion branch has a row above.
3. Every V1 structural member and switch subject has a row above.
4. Every live V2 backend clean-rejection branch has either a matching V2
   language restriction or a focused reproduction.
5. Each **Implemented, proof needed** row has a compile-link-run test for its
   meaningful type and value-shape matrix.
6. Each confirmed failure is copied to proposal 13 with its exact source,
   error, cause, priority, and one small investigation or implementation
   slice.
7. The audit file receives a non-colliding proposal number. Proposal number
   14 is also used by `14-pointer-arithmetic.md`.

Until these conditions are true, this document must not state that V2 has
full V1 parity.
