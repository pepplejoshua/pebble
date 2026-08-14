# V1 checker and backend parity audit for compiler v2

**Status:** in progress; fifth serious source-and-runtime pass complete on
2026-08-14

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

## Fifth serious pass: current confirmed gaps

The fifth pass started from commit `2683ff6`. Three independent read-only Orc
audits used `opencode-go/deepseek-v4-flash`,
`vercel/alibaba/qwen3.7-flash`, and `openai/gpt-5.6-luna`. The supervisor then
compared their claims with focused V2 probes, V1 source or compile-run proof,
current backend rejection branches, the Git history, and all ten examples.
No compiler or runtime implementation changed during this pass.

This is the open-only work list. Each row is one bounded implementation or
decision slice. Broad claims from a worker are excluded when current source,
tests, or later commits contradict them.

| ID | Open task | Current proof and cause | Smallest next slice | Class |
|---|---|---|---|---|
| F5-01 | Generic tagged-union method switch subject | A specialized method such as `Result[T, E].is_ok` keeps the template union TypeID on `self`. The backend union map is keyed by the concrete TypeID, so switch lowering misses the tagged-union path and emits the whole C union value instead of `.tag`. `std:result`'s `is_ok`, `unwrap_or`, and `map` fail end to end. | **Resolved** (`73245a0`) — root cause was checker-side, not backend: `buildMethodCall`'s specialization trigger required `methodSymbol.Generic` (true only when the method itself declares type parameters, e.g. `map[U]`), so a method that only INHERITS type parameters from its containing generic type (`is_ok`, `unwrap_or`, no type params of their own) never got a concrete `FunctionDeclaration` specialization built — `self` kept the unspecialized template's TypeID at every call site (confirmed via `tirdump`, contrasted against the free generic function `result_ok[T, E]`, which already carried correct `typeargs`). Fixed by triggering specialization whenever `signature.TypeParams` is non-empty, regardless of whether the method declares its own. The backend's `.tag`-projection logic needed no change — it was already correct once given a properly specialized receiver type. A genuinely separate bug was found and deliberately left unfixed — see the new row below |
| F5-01b | Duplicate C enumerators across two instantiations of one generic tagged union | Found during F5-01's investigation, 2026-08-14. A program with TWO DIFFERENT concrete instantiations of the same generic tagged union live at once (e.g. `Result[int, str]` and `Result[bool, str]` both constructed in one program) emits duplicate C enumerator names for the shared variant names (`pebble_variant_29` twice) — independent of the F5-01 fix, reproduces identically before and after it. Likely the per-instantiation enum-typedef collection doesn't dedupe or uniquely-suffix variant names across separate specializations of the same generic union declaration. | Root cause not yet chased — needs its own investigation pass (reproduce minimally without `std:result`, trace the enum-typedef collection/naming code for a generic tagged union's per-specialization variant enumerators). | **Backend typedef-naming defect, confirmed pre-existing and independent of F5-01** |
| F5-02 | Generic untagged-union field specialization | A generic untagged union instantiated with a scalar payload reaches Emit with the field type still recorded as a type parameter and is rejected as an unsupported C field shape. Non-generic scalar untagged unions work. | **Resolved** (`eee586e`) — `resolveStructInfo` recovers a generic instantiation's concrete type arguments and substitutes each field's type via `structSubstitutions`/`snapshot.Substitute` before it reaches C-type naming; `resolveUntaggedUnionInfo` (Phase 3 #51) never did this, reading each member's type directly with no substitution, so a generic union's field reached Emit still carrying the raw type-parameter symbol. Fixed by mirroring `resolveStructInfo`'s substitution logic exactly (same helper, no new machinery). Covers multiple concrete instantiations of one generic union live simultaneously, each getting its own correctly-substituted typedef; a non-scalar instantiation still cleanly rejects, now naming the correct substituted type instead of a raw type-parameter symbol. Root cause and fix independent of F5-01, as the audit anticipated |
| F5-03 | `str` reassignment from another local | `var a str = "a"; var b str = "b"; a = b;` reaches `buildStoreCore`, whose `str` branch accepts only `StringLiteral` and `InterpolatedString`. V1 emits a normal string-view assignment. | **Resolved** (`7d418bd`) — `buildStoreCore`'s narrow two-case manual switch deleted, delegated entirely to `buildStrOperand` (the general str-value builder already used for call arguments/comparisons/field construction), a strict superset already handling `SymbolValue` correctly. Confirmed byte-identical C for the pre-existing literal/interpolated shapes | — |
| F5-04 | `str` reassignment from a call | `s = get_str();` reaches the same branch and rejects `DirectCall`. | **Resolved** (`7d418bd`, same commit as F5-03 — shared root cause and fix location) — `buildStrOperand` already handled `DirectCall`/`MethodCall` with correct single-evaluation via the shared `buildDirectCall` machinery; confirmed via a dedicated single-evaluation test | — |
| F5-05 | Interpolation of a `str` value part | General interpolated-string materialization rejects the part with “want bool, an integer type, or a float type”. V1 formats it. | **Resolved** (`a785060`) — a `str` part needs no formatting, just a direct memcpy of its own data/len; added `PEBBLE_STR_PART_STR` to the runtime enum/struct, a matching case in `pebble_rt_str_from_parts`, and widened `buildInterpolatedStringParts`/`buildPrint` (both combined and sequential paths) to build the value via `buildStrOperand`. New runtime smoke coverage and Go end-to-end tests across local-init, call-argument, return, comparison, reassignment, and print positions. | — |
| F5-06 | Interpolation of a `char` value part | Same rejection. V1 formats a character value. | **Resolved** (`170ff96`) — the scalar is encoded to UTF-8 via the existing `pebble_rt_char_to_utf8`, reusing the same encoder a bare char print operand uses. Added `PEBBLE_STR_PART_CHAR`, a matching case in `pebble_rt_str_from_parts`, and widened `buildInterpolatedStringParts`/`buildPrint`. A NUL-char (scalar 0) regression was caught during verification: the write pass initially reused the int/float cases' `strlen`-based copy, which returns 0 for a buffer starting with `0x00`, silently dropping the character and desyncing later parts' offsets — fixed by encoding directly into the destination and using the encoder's own return value as the byte count. ASCII, 2-byte, and 4-byte (astral) UTF-8 proofs included. | **New:** print's C emission (`fprintf(..., "%s", ...)`) truncates at any embedded NUL for ANY `str` value, independent of interpolation — confirmed pre-existing (`print "x\0y";` already truncates on `HEAD`), logged below as its own row, not fixed here. |
| F5-06b | `print` truncates any `str` value at an embedded NUL byte | `fprintf(..., "%s", ...)` is a C-string operation and stops at the first `0x00`; confirmed independent of interpolation (`var s str = "x\0y"; print s;` already truncates on plain `HEAD`, no char/interpolation involved). | Every `print`/`fprintf` call site for a `str` value would need to switch to a length-bounded write (`fwrite(s.data, 1, s.len, stdout)`). Not started — low priority, embedded-NUL strings are a rare edge case, and materialization (`.len`/`.data`) is already correct; only the print path is affected. | **Low-priority backend gap, discovered during F5-06** |
| F5-07 | Interpolation of an enum value part | Same rejection. V1 formats the variant name. | **Resolved** (`cd5e3c6`), scoped to PLAIN enums — a runtime switch (`buildEnumInterpolationSwitch`) assigns a temp `PebbleStr` from a static `Type.variant` string per case, reusing F5-05's str-part machinery (no new `PebbleStrPartKind`) and the existing enum-print naming helpers. A tagged union (payload-carrying enum) is explicitly rejected, not silently mishandled — payload recursion is a separate follow-up. Also fixed a real collection gap: `collectEnumTypesWalk` only followed `node.Children`, missing an enum referenced only via an interpolation's `node.Parts` (e.g. `` `pick={Color.green}` ``), leaving its typedef/variant constants uncollected. | — |
| F5-08 | Interpolation of a struct value part | Same rejection. V1 recursively formats the value. | **Resolved** (`7696630`), scoped to non-nested structs (a struct/tuple/array/untagged-union field is cleanly rejected). One struct part expands into MULTIPLE `PebbleStrPart` entries in `buildInterpolatedStringParts`' own array (a text label per field boundary plus each field's own entry, reusing the existing per-type dispatch), matching `buildStructPrintValueCalls`'s bare-print rendering exactly. Needed two verification-caught follow-ups: a `field.member`/`fieldSourceName` naming mismatch that failed to compile, and a `len(node.Parts)` vs `len(parts)` count mismatch that silently truncated output (struct was the first part kind where one interpolation part expands to multiple array entries) — neither caught by the implementing sessions' own reports, both found by directly building/running the code. | — |
| F5-09 | Interpolation of a tuple value part | Same rejection. V1 recursively formats the value. | **Resolved** (`8b7d057`), scoped to non-nested tuples (a struct/tuple/array/untagged-union element is cleanly rejected). One tuple part expands into MULTIPLE `PebbleStrPart` entries the same way F5-08's struct case does, reusing `buildTuplePrintValueCalls`/`Operand`/`ValueExpr`'s naming conventions so an interpolated tuple matches bare print exactly, including a single-element tuple's trailing comma. Closes F5-01 through F5-09, the full interpolation-value-part matrix. | — |
| F5-10 | Aggregate `ArrayRepeat` call argument | Passing `[Point.{ ... }; N]` to `[N]Point` rejects the nominal element in `buildArrayArgument`. The scalar repeat path works, and V1 runs the aggregate case. | **Resolved** (`cea6231`) — both of `buildArrayArgument`'s element-building switches (the full `ArrayValue` literal case and the `ArrayRepeat` `[v; N]` case) gained an `isStruct` branch using `buildNestedAggregateValue` (already used for nested-array elements), preserving the `ArrayRepeat` case's evaluate-once/copy-N-times pattern. `buildStructValueExpr` also gained a `DirectCall`/`MethodCall` case so a struct-returning helper call can be the repeated value directly, proven by a dedicated evaluate-once test. | — |
| F5-11 | Aggregate `ArrayRepeat` return | Returning `[Point.{ ... }; N]` calls the scalar expression builder and rejects `RecordConstruct`. V1 runs it. | **Resolved** (`298dc80`) — `buildArrayReturnValue`'s `ArrayRepeat` branch gained an `isStruct` case using `buildNestedAggregateValue`, mirroring F5-10's call-argument fix exactly and preserving the evaluate-once/copy-N-times pattern (`pebble_repeat_ret_<nodeID>` temp). The sibling `ArrayValue` case (a full struct-literal array return, not a repeat) was already working and untouched. Landed clean on the first dispatch. | — |
| F5-12 | Whole tuple reassignment from a call | `pair = make_pair();` is explicitly rejected by `buildTupleStoreValue`; local and literal sources work. | **Resolved** (`1d85f45`) — `buildTupleStoreValue` gained the tuple analogue of `buildStructStoreValue`'s already-resolved `DirectCall` case: result-type check, `findCallDeclaration`/`ResultType` double-check, `buildDirectCallNested`. Covers plain-local, 3+ element, and pointer-deref reassignment shapes. A now-obsolete negative test asserting the old rejection was removed (caught by the periodic full-suite checkpoint, not the dispatch's own report). | — |
| F5-13 | Whole array reassignment from a call | `items = make_items();` is explicitly rejected by `buildArrayStoreValue`; local and literal sources work. | **Resolved** (`ea5a52b`) — since array stores are memcpy'd (not a plain assignment), `buildArrayStoreValue` must return an ADDRESS expression, but a call result is an rvalue. Uses an anonymous struct-wrapper compound literal with a designated initializer (`&(struct { T val; }){ .val = call() }.val`) rather than a GNU statement-expression address, which Apple Clang rejects taking the address of — portable, standard C99. Covers plain-local, 5-element, pointer-deref, and struct-field shapes; verified via real `cc -Wall -Wextra` compilation given the non-obvious C shape. | — |
| F5-14 | Tuple-return forwarding | `return make_pair();` is rejected; tuple local and tuple literal returns work. Struct call forwarding already provides the nearest pattern. | **Resolved** (`dc8de85`) — the struct branch of the same `buildAggregateReturnValue` function already had a working `DirectCall` case for this exact shape; added the exact tuple analogue (same `findCallDeclaration`/`ResultType` check, same `buildDirectCallWithPre`, no signature change). A now-obsolete negative test (two-hop forwarding chain) was converted into a positive compile-and-run test in place. | — |
| F5-15 | `str` tuple element as a call argument | A `str`-typed `Load(TuplePlace)` is rejected because `buildStrOperand` accepts only `CheckedIndexPlace` and `FieldPlace` loads. Local initialization from the same tuple element already works. | **Resolved** (`9e6bcdc`) — the existing `buildTuplePlaceRead` helper only accepts bool/entry-width elements, so a `TuplePlace` branch was added inline (base-expression/element-lookup/projection logic with an `isStr` check) rather than reused, emitting the same `pebble_local_<sym>._<ordinal>` C shape. Verified across call-argument, local-init, comparison, and return-value positions. | — |
| F5-16 | Optional field read as a call argument | Passing `holder.value` where the parameter type is optional reaches an optional-value builder that does not accept `Load(FieldPlace)`. | **Resolved** (`8dff13c`) — `buildOptionalValue`'s `Load` case gained a `FieldPlace` branch mirroring `buildStructFieldRead`'s field-access convention; since the builder is shared between call-argument and return-value positions, the one fix covers both (confirmed by test). Covers some/none states and both value/pointer receivers. | — |
| F5-17 | Struct payload in a tagged union | A variant with a struct payload gets a typedef-level or construction-level unsupported-payload rejection. Scalar, string, enum, and nested tagged-union payloads work. | **Resolved** (`ac60dc1`) — the union typedef block leads the aggregate block, but a struct payload's C member names the struct's own typedef, needing the reverse order. `isPlainStructPayload` admits a struct only if every field is self-contained at the union block's position (never another struct/tuple/optional/array/slice/pointer/union field); Emit hoists each qualifying struct's typedef between the enum and union blocks, filtered out of the main aggregate block. Construction and the narrowed read both gained struct-payload cases. Verified against the full existing union-payload test suite with zero regressions. A struct with a nested aggregate field stays cleanly rejected (general dependency-graph ordering deliberately out of scope). | — |
| F5-18 | Fixed array of structs as a struct field | A field such as `items [N]Point` is rejected by the remaining array-through-aggregate ordering guard as “more than one level of nesting”. Arrays of arrays work; plain deep aggregate chains work. | **Resolved** (`e05f71f`) — a *plain* struct array element (self-contained scalar fields only) is now admitted past the `throughArray` nesting guard; `aggregateTypeOrder` gained an `arrays` field and `orderAggregateTypes`'s DFS recurses through the element to categorize the array by postorder position, and `Emit` excludes these "interleaved" arrays from the leading field-array block so `buildAggregateTypedefs` emits each one after its element's struct typedef and before any aggregate referencing it — empirically confirmed necessary (not just the nesting-guard exclusion alone) by observing real emitted C. New `isPlainStructField`/`isPlainStructArrayElement` predicates in `types.go`, deliberately separate from F5-17's `isPlainStructPayload`. A struct-in-struct element through an array still cleanly rejects with "more than one level of nesting". Verified end-to-end via real `cc` compile-and-run plus a causation check against `HEAD`. | — |
| F5-19 | Aggregate parameter in a function value | A type such as `fn(Point) int` is rejected by `validateFunctionTypeSignature`. Ordinary helpers already pass structs by value. | **Resolved** (`4839a31`) — `validateFunctionTypeSignature` and `functionTypeParamCType` admit a *plain* struct parameter (reusing F5-18's `isPlainStructField`); the indirect-call argument lowering needed no changes (already shared with ordinary direct calls via `buildCallArgument`'s existing struct case). The real work was typedef ordering: function typedefs lead the whole output (correct today because a struct can carry a function-typed field, needing the reverse order), so the admitted struct's typedef is hoisted into a new pre-function block — but only for structs that do NOT themselves carry a function-typed field, since hoisting one of those would create a circular C dependency. One generic-inference negative test became obsolete (a generic struct parameter that substitutes to a plain struct is now legitimately admitted) and was converted to a positive test, matching the F5-12/F5-14 precedent. | — |
| F5-20 | Aggregate result in a function value | Aggregate results are rejected by the same first-class signature gate. | **Resolved** (`a059058`) — `validateFunctionTypeSignature`/`functionTypeResultCType` admit a plain struct result, mirroring F5-19's parameter case (same `isPlainStructField` reuse). F5-19's typedef-hoisting collector was widened (renamed `collectFunctionParamAndResultStructs`) rather than duplicated, now walking each function type's result as well as its parameters. `buildStructLocalDeclaration` gained an `IndirectCall` case (previously only `DirectCall`/`MethodCall` were handled for a struct-typed local's initializer), delegating to the already-generic `buildFunctionIndirectCall`. Verified end-to-end via real `cc` compile-and-run, including a chained result-then-parameter shape across two indirect calls, plus a causation check against `HEAD`. | — |
| F5-21 | `str` result in a function value | A type such as `fn() str` is rejected although ordinary helpers return `PebbleStr`. | **Resolved** (`1f7939e`) — `validateFunctionTypeSignature`/`functionTypeResultCType` admit `str` as a result, mirroring the existing parameter-side `isStr` case exactly; no typedef-ordering work needed since `PebbleStr` is the runtime's fixed C struct, never a program-defined typedef. `buildStrLocalDeclaration` gained an `IndirectCall` case (previously only `DirectCall`/`MethodCall` were handled), mirroring F5-20's struct-local case. Verified end-to-end via real `cc` compile-and-run plus a causation check against `HEAD`. | — |
| F5-22 | Print an optional value | V1 has an optional format path. V2 rejects the operand in the checker. | Implement proposal 17's optional-print slice only. | **Checker/backend print gap** |
| F5-23 | Print a pointer value | V1 has a pointer format path. V2 rejects the operand in the checker. | Implement proposal 17's pointer-print slice only. | **Checker/backend print gap** |
| F5-24 | Print a function value | V1 has a function-value format path. V2 rejects the operand in the checker. | Implement proposal 17's function-value-print slice only. | **Checker/backend print gap** |
| F5-25 | Platform-sized `int` and `uint` are not implemented end to end | The ABI spec already requires target-native word types. The checker has `LiteralTarget.WordBits`, but `pebc` hardcodes it to 64. The backend then maps `int` to `int32_t` and `uint` to `uint64_t`; compatibility checks and checked-runtime-helper selection repeat those fixed mappings. On the current 64-bit host, `uint` matches by accident and `int` is wrong. `let x int = 2147483648;` passes checking and fails in `cc`. | First add one authoritative target-word configuration and thread it through checking and Emit without changing behavior. Then change C spelling and runtime-helper selection together, with 32-bit and 64-bit emitted-C tests. | **Confirmed cross-layer implementation defect** |

The example sweep passes for every example except `arena_alloc.peb`.
`arena_alloc.peb` still uses pointer arithmetic and an uninitialized binding;
both are accepted V2 differences, not new parity defects. `read_file.peb`
reports its expected missing demo file and exits successfully.

The fourth-pass active rows for string switch single evaluation, bool switch,
unbound range loops, direct array returns, copy initialization, tuple
coercion, recursive aggregate chains, deferred statement families, and the
older C-shape examples were stale. Their completion evidence remains in the
detailed ledgers below; they are not repeated in this open-only table.

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
| Module-level immutable constant | V1 emits global C storage; initializer is limited to a simple literal | V2 inlines immutable global values at each use and has a richer constant evaluator | **Verified** (`1e526cf`) — 42-case matrix (`TestModuleConstantValueShapes`) proves every checker-supported constant value shape across every position that accepts a constant reference; see the "Constant declaration" row below for the two non-bug findings from this proof and the new follow-up row for a real, unrelated gap it surfaced |
| Uninitialized local or global variable with an explicit type | V1 accepts it and C zero-initializes the object | V2 requires an initializer for each non-extern `let` or `var` | **Intentional difference** |
| Constant declaration | V1 requires an initializer and limits global initialization to literals | V2 constant evaluation supports references, unary and binary expressions, enums, cycle checks, and budgets | **Verified** (`1e526cf`) for the evaluator and the backend value shape both. No product bug: narrow-width (i8/i16/u8/u32 checked arithmetic, u64 division) constant initializers are rejected, but `TestPlainNarrowWidthArithmeticLimitation` confirms plain non-constant operands at the same widths fail identically — the pre-existing general checked-arithmetic-width limitation, not constant-specific |
| Import and qualified module value/function/type paths | V1 module member lookup and registered names | V2 module builder, `Path`, and symbol resolution | **Verified** (`ed48868`) — a two-file fixture proves mutable value (read+write-through to real shared storage), immutable value, non-generic function, generic function with a nested qualified type argument, plain type, and two-level generic type, all resolved through a `lib::` qualified path |
| Type declaration | V1 registers struct, union, tagged union, enum, alias, and generic declarations | V2 type declaration facts and specialized nominal types | **Rollup row, not separately workable** (checked 2026-08-12) — every Partial entry in the "Type and member ledger" below is already tracked as its own row elsewhere in this table (Pointer, Fixed array, Slice, Struct, Optional, Tagged union, Function type, Generic type, Struct/enum selection). The one distinct entry, Untagged union (line 132), is **Decision needed**, not a bug |
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
| Pointer `*T`, address-of, dereference, `nil` | V2 has `AddressOf`, `DereferencePlace`, `Load`, and pointer conversions | **Verified** (`a80a707`) — whole dereferenced structs resolved (`a242181`); array, tuple, enum, and optional pointees now support whole-value read through a deref, address-of, whole-value write, nil comparison, and pointer equality (16-case compile-link-run matrix, `TestProbeCompileRunAggregatePointerShapes`). See the new "Escape analysis for a stack-local's address" row below for a general, pre-existing, unrelated gap this investigation surfaced |
| Fixed array `[N]T` | V2 has `ArrayValue`, `ArrayRepeat`, array locals, indexing, and `.len` | **Partial.** The foundational local, parameter, return, index, copy, nested-array, `sizeof`, struct-field, enum-element, tuple-element, and optional-payload shapes are resolved. Current confirmed limits are aggregate-element `ArrayRepeat` in call/return positions (F5-10/F5-11), whole-array reassignment from a call (F5-13), and an array-of-struct used as a struct field (F5-18). |
| Nested fixed array `[N][M]T` | n/a | backend gap, found 2026-08-12 during Phase 3 #4 | **Resolved** (`b903ddb`, Phase 3 #31) for the foundational positions — root cause was `arrayElementCType` having no case for an array-typed element, cascading to every position. Fixed: local declaration (incl. 3-level nesting and whole-local copy), indexed read/write through multiple levels (via a new `.data` lvalue projection mirroring the existing struct-field-array case), a nested-array literal referencing in-scope array locals, parameters, returns, `.len` on both dimensions, element widths beyond default (bool/float/str), whole-array reassignment. Three shapes deliberately deferred, each pinned to a clean rejection in `nested_array_test.go`'s `TestNestedArrayDeferredShapesRejected`: array-of-arrays struct field (blocked earlier by `orderAggregateTypes`' nesting-depth check), `ArrayRepeat` of an array-typed value, and binding a whole inner-array read to a local (local-initializer Load path only accepts a `DereferencePlace`) — see the three new follow-up rows below. Also surfaced, explicitly NOT fixed as out of scope: a general, pre-existing, non-nested-specific checker bug where arithmetic on a non-default-width array element read fails T0505 for a SINGLE-LEVEL array too |
| Array-of-arrays struct field | n/a | backend gap, found 2026-08-13 during Phase 3 #31, deliberately deferred | **Resolved** (`b1a5303`, Phase 3 #38) — `orderAggregateTypes`'s depth check blanket-rejected any struct field whose chain passed through an array, regardless of the array's own element kind; narrowed so only an array-of-STRUCT/TUPLE/OPTIONAL trips the rejection (the genuine ordering hazard), since Phase 3 #31 already made array-of-ARRAY self-contained within `buildArrayTypedefs`'s own DFS-postorder emission. Covers read, indexed write, passing to a helper, non-default width, and (falling out for free) 3-level nesting; array-of-aggregate independently confirmed still rejected |
| `ArrayRepeat` of an array-typed value | n/a | backend gap, found 2026-08-13 during Phase 3 #31, deliberately deferred | **Resolved** (`7f62398`, Phase 3 #39) for the local-declaration position — `buildArrayRepeatLocalDeclaration`'s element-type dispatch had no array case; widened the existing struct/tuple/optional branch to also route an array-typed element through `buildNestedAggregateValue` (whose `isArray` case, from Phase 3 #31, already builds it correctly). Investigated and confirmed a broader, separate, pre-existing gap: the array-typed CALL-ARGUMENT and RETURN-VALUE `ArrayRepeat` builders support no aggregate-typed repeat value at all (not just arrays) — left untouched, out of scope for this narrow fix |
| Binding a whole inner-array read to a local | n/a | backend gap, found 2026-08-13 during Phase 3 #31, deliberately deferred | **Resolved** (`480eab5`, Phase 3 #40) — `buildArrayLocalDeclaration`'s Load-initializer path only accepted a `DereferencePlace`; added a `CheckedIndexPlace` branch alongside it, reusing `buildPlaceLValue`'s existing `.data`-projected nested-array lvalue (from Phase 3 #31) as the memcpy source. Verified as a real independent copy, not an alias, by mutating the source array after binding and confirming the bound local is unaffected. This closes the last of the three shapes deferred from Phase 3 #31 |
| Arithmetic on a non-default-width array element read | n/a | checker gap, found 2026-08-13 during Phase 3 #31, general/pre-existing, not array-specific | **Resolved** (`81b08b6`, Phase 3 #41) — the original filing under-scoped it as array-specific; it is not. Root cause: `prepareOperator` pushed the enclosing destination's expected type onto EVERY operand of a same-result binary operator, including an already-concretely-typed sibling (not just the literal), so a mismatched outer destination (e.g. `int` vs. an `i32` local) pinned the literal to the wrong width before `finishOperator`'s hard unification compared it against the sibling's real type. Fixed by excluding a literal operand from the pushdown (mirroring the existing unary-minus exclusion) and having it adopt its concrete sibling's type via `LiteralFits` instead, with the operator's own result correctly propagating concreteness for nested expressions (`x + 1 + 2`) and outer-destination coercion via the compatibility record. Verified against the FULL `internal/check` and `internal/backend` suites (the highest-risk fix in this sweep — every arithmetic expression in the language routes through this code); one pre-existing, unrelated full-suite failure surfaced and independently confirmed via causation-check, logged separately below. A separate, pre-existing backend gap was also surfaced (not caused by this fix): returning a non-default-width value — even a bare local, no arithmetic — from a default-width-returning function fails at Emit; logged as its own row below |
| Non-default-width value returned from a default-width-returning function | n/a | backend gap, found 2026-08-13 during Phase 3 #41, general/pre-existing | **Resolved** (`e7375a8`, Phase 3 #44) — root cause was in `internal/check`, not the backend: the compatibility-record mechanism classified this pair as `compatibleExplicit` (Rust-style: no implicit conversion between distinct concrete types) but the IR builder silently fell through to a bare, uncoerced value with zero diagnostic for any non-`compatibleImplicit` class, deferring the confusingly-worded rejection to the backend's own separate width gate. Decision (Rust model, confirmed with the user): require an explicit `as` cast, scoped to `compatibilityAssignment`/`Argument`/`Return` roles only — optional injection, tuple component coercion, and struct field construction (all deliberately implicit) keep their exact prior behavior, with a composite-literal-source carve-out inside `compatibilityAssignment` so tuple/struct LITERAL coercion still routes through the existing per-element mechanism. Also fixed a genuine regression surfaced in `std/io.peb` (three bare `int`-defaulting `let` constants passed to a `u16` parameter) and updated 5 pre-existing backend tests whose own assertions/comments had gone stale (they existed to prove the BACKEND's width gate caught this; it's now caught earlier and more clearly by the checker) |
| `TestEmitRejectsSliceParameterUnsupportedElementType` hand-built-IR test failure | n/a | test/backend gap, found 2026-08-13 during Phase 3 #41's full-suite checkpoint, pre-existing | **Resolved** (`15ad508`, Phase 3 #48) — NOT a production ordering bug: the fixture's premise itself went stale. It hand-built a `[]str` helper parameter to exercise `validateHelperSignature`'s slice-element-type rejection, but str became a SUPPORTED slice element type as part of Phase 3 #32 (earlier in this same engagement), so `[]str` no longer reaches that gate at all — it fell through to an unrelated argument-count check against the fixture's deliberately-empty call instead. Confirmed the element-type gate is still live and reachable via a genuinely unsupported element (slice-of-fixed-array, `[][3]i32`, which the backend has no lowering/typedef for) and rebuilt the fixture around that. Full `internal/backend` suite is now FULLY GREEN — this was the last remaining known failure carried since Phase 3 #41 |
| `ArrayRepeat` as a struct-field construction value | n/a | backend gap, found 2026-08-12 during Phase 3 #4 | **Resolved** (`b3020aa`, Phase 3 #18) — `buildStructArrayFieldValue` gained an ArrayRepeat case mirroring the ArrayValue case `249995d` already fixed, including a narrow-width element and a generic struct field. The companion checker gap (a narrow/wide-width element rejected at C0601) was also fixed as part of the same commit (finishArray/finishArrayRepeat no longer force a hard Equal against a KNOWN destination element) |
| Array-typed tuple element | n/a | checker gap, found 2026-08-12 during Phase 3 #4 | **Resolved** (`5f7d40b`, Phase 3 #42) — the original T0501 checker-unification filing was stale by the time this was picked up (independently reconfirmed the checker accepts `([3]i32, i32)` cleanly in every position); the real, current gap was purely backend — `tupleElementCType` had no `isArray` case, the same bug class already fixed for arrays/slices/struct fields. Fixed with the element-type gate plus a pre-aggregate-block typedef-collection step for tuple-element arrays (mirroring the existing struct-field/optional-payload treatment), plus a tuple-literal array-element construction case and a whole-array-element-binding (`TuplePlace`) case as natural extensions. (The array-typed OPTIONAL half of this row, `?[3]i32`, is resolved separately — see "Optional `?T`" below: the T0505 checker gap was found already fixed by prior work, and the remaining backend gap closed in `08ce755`) |
| Whole-array `==`/`!=` | n/a | checker behavior, confirmed 2026-08-12 during Phase 3 #4 | Rejected by the checker (C0603), confirmed NOT a backend gap — whether this should be supported is a design question, not logged as a bug |
| Anonymous `.{ ... }` struct construction (no base type name) | n/a | checker gap, found 2026-08-12 during Phase 3 #4, unrelated to arrays | **Resolved** (`ef5fc24`, Phase 3 #33) — the resolver only resolves field names when the literal carries a base-type name; fixed by re-deriving the member symbol by name against the destination declaration, both at walk time (`prepareRecord`, needed so an array-typed field value's element type gets grounded early) and as a post-solve closing net (`buildRecordConstruct`, mirroring the existing `.{ Int = 42 }` tagged-union pattern). Covers local declaration, call argument, return, reassignment, and array-typed field values; diagnostic parity with the named form confirmed for wrong-type/unknown-field/missing-field. Two genuine, separate, pre-existing gaps surfaced, affecting the NAMED form identically (not anonymous-specific) — see the two new rows below |
| Nested anonymous struct literal as a field value (`Outer.{ inner = .{ a = 1 } }`) | n/a | checker gap, found 2026-08-13 during Phase 3 #33, general/pre-existing, affects the named outer form too | **Resolved** (`cf76b45`, Phase 3 #45) — `recordFieldDeclaredType` (added for an earlier, narrower array-only task) only grounded ARRAY-typed struct fields as a KNOWN destination at walk time; a struct-typed field's own nested anonymous literal has no base name to anchor its type, so it stayed an unbound inference variable without early grounding, for both the anonymous and named outer form. Widened to also ground a plain STRUCT-typed field (`Nominal` with `infer.NominalStruct`), via a new `recordFieldGroundable` helper; tagged unions/enums (also `Nominal`) are explicitly excluded — `.{ Int = 42 }` construction already works through a separate mechanism (Phase 3 #84) and is untouched. Covers arbitrary nesting depth, generic struct-typed fields (via the existing `TemplateArray`/`MaterializeTemplate` substitution path), and confirmed no regression to array fields, named nesting, or tagged-union construction |
| Optional field assigned `some <value>` in a `.{ ... }` literal (`.{ opt = some 5 }`) | n/a | checker gap, found 2026-08-13 during Phase 3 #33, general/pre-existing, affects the named form too | **Resolved** (`72c3109`, Phase 3 #46) — same bug class as Phase 3 #45, one type-kind short: `recordFieldGroundable` covered only Array and plain Struct, so an optional-typed field's destination was never grounded as KNOWN at walk time, meaning Phase 3 #27's existing SomeExpr-pinning (which types `some <payload>` from a known destination's optional type) never fired inside a record literal — the SomeExpr typed itself from the payload alone and failed the field-role compatibility check, even for a MATCHING literal payload. Widened `recordFieldGroundable` to also cover `types.Optional`, and widened the generic-substitution gate to admit `infer.TemplateOptional` so a generic struct's own `?T` field grounds correctly too. A genuine side effect — grounding the Optional TypeID earlier shifts downstream type-ID interning order — broke one unrelated pre-existing test's hardcoded exact typedef numbers (`TestEmitGenericStructOptionalTwoSpecializationsWriteConcreteCTypedefs`); confirmed a pure renumbering (same correct C shape) via the actual failure output and updated the hardcoded numbers |
| Scalar cast inside a generic function body | n/a | checker gap, found 2026-08-12 during Phase 3 #4, unrelated to arrays | **Resolved** (`2a063f0`, Phase 3 #34) — three independent layers all lacked a case for a TypeParameter-typed operand (cast validation, requirement validation, IR-build coercion selection), each rejecting/failing a cast touching a still-generic type during template checking. Fixed by deferring all three to instantiation time (a new `hasTypeParameter` helper), sound because this codebase's generic model rebuilds the entire body per concrete instantiation via `buildSpecialization` — the deferred template-time node is never what reaches the backend. Verified end-to-end (two concrete instantiations, i32 and u64, both produce the correct cast result) and confirmed a genuinely invalid cast inside a generic body still correctly fails |
| Slice `[]T` | V2 has slice types, `.len`, `.data`, checked index/slice, and `SliceFromRaw` | **Verified** (`36c47ff`) — a slice-returning call as a call argument or forwarded return, a slice-typed struct field read as a return, re-slicing an existing slice LOCAL (not just a slice field), and a struct literal with an inline slice-construction field in a pure expression position all now work; every integer width/bool/char/float/enum/struct/tuple element type and `.len`/`.data` across local/field/call-result construction confirmed. Two genuine remaining gaps split into their own rows below: `?[]T` optional payload, and `str` slice elements |
| Slice-typed optional payload `?[]T` | n/a | backend gap, found 2026-08-12 during Phase 3 #5 | **Resolved** (`08ce755`) |
| `str`-element slice | n/a | backend gap, found 2026-08-12 during Phase 3 #5 | **Resolved** (`c4a2a4c`, Phase 3 #32) — `isSupportedSliceElementType` and `sliceElementCType` both had no `str` case (every other element kind was admitted); fixed by mirroring `arrayElementCType`'s existing str-to-`PebbleStr` handling. Covers construction, local declaration, indexed read/write, parameter, return, `.len` across full/partial/re-slice construction. Surfaced a genuine, separate, pre-existing bug — see the new row below |
| `.len` read directly on a str-typed indexed lvalue expression | n/a | backend gap, found 2026-08-13 during Phase 3 #32, general/pre-existing, not slice-specific | **Resolved** (`91da966`, Phase 3 #43) — `buildExpr`'s integer `Load`/`FieldPlace` case passed the Load's OWN width (a structural `.len` is always `uint`) into `buildStructFieldRead`'s receiver construction instead of the ambient entry width, so `checkedSuffix` (i32/i64/u64-only) resolved empty for the receiver's checked-index call. Fixed with a one-line change (`entryWidth` instead of `width`), mirroring the sibling `CheckedIndexPlace` branch two lines above which already did this correctly. Covers slice and fixed-array str-indexed receivers, literal and local-variable indices, and the out-of-bounds panic path. A separate, pre-existing, unrelated bug was surfaced and left out of scope — see the new row below |
| `.len` used as a `print` operand fails a format-specifier mismatch | n/a | backend gap, found 2026-08-13 during Phase 3 #43, general/pre-existing, unrelated to indexing | **Resolved** (`1dd0d63`, Phase 3 #47) — `.len`'s C expression carries the runtime aggregate's real `size_t` type (`PebbleStr`/`PebbleStrSlice` both declare `.len` as `size_t`, a fixed array's `.len` folds to a uint-typed C literal), neither of which is `uint64_t`, so the `PRIu64` format specifier rejected it under `-Werror -Wformat`. Fixed by casting a `.len`-sourced (or any Uint-typed literal — the investigation found a plain `print(5u)` hits the identical mismatch) print operand's C expression to `uint64_t` at the print-call site, mirroring an existing precedent (a str's `.data` field cast to `const char *` for a `%s` libc argument). Covers str/slice/fixed-array `.len`, addressable and non-addressable (call-result) receivers, parenthesized and mixed-operand prints, and deferred prints; composite print operands (a struct field or array element built from `.len`) were already unaffected since their temp is already `uint64_t` |
| V1 pointer slice `ptr[start:end]` | V2 rejects pointer slicing and provides std-only `slice ptr, count` | **Intentional difference** under the pointer-safety design |
| Struct | V2 record construction, fields, methods, parameters, results, runtime nominals, and C typedefs exist | **Verified** (checked 2026-08-12, Phase 3 #6) — stale wording corrected: local copy initialization resolved (`2179ebf`), deep aggregate dependencies resolved (`e649476`) for plain deep struct/tuple/optional nesting (array-of-aggregate struct fields remain a separate, intentionally out-of-scope backend gap). Remaining struct-specific gaps are tracked individually elsewhere in this table: "Struct field and instance method selection" (line 151), "Struct literal field conversion" (line 227) |
| Tuple | V2 tuple construction, elements, parameters, results, and `TupleCoerce` exist | **Mostly resolved.** Whole-value reassignment (`d1b05be`), local copy initialization (`834927e`), and `TupleCoerce` in a local declaration (`d905ab6`) all landed 2026-08-10. `TupleCoerce` in a call argument or reassignment remains deliberately out of scope. |
| Optional `?T`, `some`, `none`, force unwrap | V2 has optional construction, injection, and checked unwrap | **Verified** (`08ce755`) — the two remaining payload gaps (array `?[N]T`, slice `?[]T`) closed, covering construction, every position, presence check, and force-unwrap for both, at every element width. The array case's checker-side T0505 unification gap (originally logged during Phase 3 #4) was found ALREADY resolved by prior work — an exhaustive probe found every some/none/return/argument/reassignment shape already passes the checker; only the backend `Emit` rejection needed fixing. New runtime helper `pebble_rt_checked_unwrap_present` added for aggregate-payload force-unwrap (no scalar to return through the existing checked-unwrap family) |
| Enum | V2 construction, switch labels, and integer conversions exist | **Verified** for enum-to-integer and integer-to-enum. Enum-element arrays and slices resolved `94a2a39`; local copy initialization resolved (task #43); ordinary `some Color.red` optional initialization resolved `1bf785d`. Remaining: enum tuple elements, and an enum-typed struct field's construction (a separate `RecordConstruct.Fields` typedef-collection gap, discovered but not yet a formal item). |
| Tagged union | V2 construction, ordinary switch narrowing, generic-self read/write narrowing, and helper results exist | **Partial.** Scalar, string, enum, and nested tagged-union payloads work. A struct payload remains open as F5-17. The fifth pass also found the specialized generic-method switch-subject defect F5-01. Other aggregate payload kinds need separate focused proof before they become tasks. |
| Nested tagged-union payload, inline construction | n/a | backend typedef-ordering gap, found 2026-08-12 during Phase 3 #8 | **Resolved** (`6fe558b`, Phase 3 #37) — the exact same bug class as the nested-array typedef-ordering fix (Phase 3 #31): `buildUnionTypedefs` emitted in first-encountered collection order rather than dependency order. Fixed by rewriting it to a memoized DFS-postorder builder mirroring `buildArrayTypedefs` exactly; collection itself needed no change (already reaches the inner union transitively). Covers local declaration, function argument, return value, and (falling out for free) 3-level nesting |
| Untagged union | V1 emits a C union and permits construction and member access | V2 supports explicitly unsafe scalar-payload construction, read, and write | **Partial.** Scalar support landed in `46df6e1`, the char read follow-up closed in `67f82b9`, and generic scalar member specialization closed in `eee586e` (F5-02). Aggregate and `str` fields need separate focused proof before they become tasks. |
| `char`-typed untagged-union field read | n/a | backend gap, found 2026-08-13 during Phase 3 #51's test-coverage pass, deliberately deferred | **Resolved** (`67f82b9`, Phase 3 #52) — `buildStructFieldRead`/`buildStructFieldValueRead` each had a case for every other scalar kind (bool, integer, pointer, enum, str — all resolving to the same plain `pebble_field_<member>` C projection, no coercion needed) but none for `isChar`; construction and write already worked via `buildCharOperand`. Fixed by adding the missing case, mirroring the existing pointer/enum/str cases exactly. Covers a non-ASCII (`'é'`) round trip, not just ASCII, proving the full Unicode scalar value survives |
| Escape analysis for a stack-local's address | n/a | backend gap, found 2026-08-12 during Phase 3 #3 (pointer/aggregate deref) investigation | **Decided 2026-08-13: leave as-is for now.** `fn mk() *T { var x T = ...; return &x; }` is accepted by the checker for any aggregate type and compiles to a real dangling pointer; the test harness's `-Werror` catches it as `-Wreturn-stack-address`, but nothing in the checker rejects the *source* pattern — V1 had no escape analysis either. Recognized as a real future upgrade (the prerequisite for stack-allocation optimization, safe pointer returns, and eventually move/borrow checking) rather than a narrowly scoped bug — logged in `18-future-safety-and-ergonomics-roadmap.md`, not scheduled |
| Function type and function value | V2 supports Pebble-convention, non-variadic function values for a limited C-representable signature set | **Partial**; float (f32/f64) parameter and result support added (`cd6e604`). Aggregate parameters/results remain unsupported (a later, larger follow-up — untouched indirect-call dispatch machinery); non-Pebble convention and variadic signatures are intentional/decision-pending scope limits tracked by separate rows (lines 108, 313), not gaps in this row |
| Opaque extern type | V2 represents it, emits incomplete C declarations, permits pointer use, and rejects invalid `sizeof` use | **Verified** |
| Generic type and specialization | V2 supports generic nominal types, specialization, and owner type-parameter inheritance | **Verified** (`4ae1ac6`) — a 20+-shape empirical sweep (deep/nested generic fields, multiple/repeated type params, coexisting specializations, generic-fn-returning-generic-struct, cross-boundary specialization, optional-of-generic payloads) found every shape already working; the one real generics-specific gap found (a generic method's own type param inferable only from a function-value argument) is fixed. Owner inheritance resolved in `ddbe454`. Every remaining failure encountered during the sweep reproduced identically for a plain non-generic control — general limitations tracked elsewhere (array-of-aggregate struct fields, non-addressable FieldValue/struct-unwrap), not generics-specific |
| Recursive nominal type | V2 collection has dependency ordering and recursion paths | **Resolved (`e649476`)**; a plain three-level (and deeper) non-recursive struct chain now compiles and runs; array-of-aggregate chains remain rejected by design |
| Tuple member `.0`, `.1`, and so on | V1 and V2 resolve tuple ordinals | **Verified** for integer/bool elements (`f1841e1`) across 2-5-element tuples, locals, parameters, and struct fields, and for char/str elements (`bd84ee9`), and for f64/f32 elements (`e5add48`, task #86 slice 86a). Floats as aggregate members generally: **Resolved**, task #86 (all four sub-tasks landed — see the dedicated row below). |
| Array `.len` | V1 and V2 support it | **Verified** (`f1841e1`) — previously ZERO backend compile-link-run coverage; now proven on a local, parameter, loop bound, struct field, i64 entry, `ArrayRepeat` source, and an array-returning helper's result. |
| Slice `.len` and `.data` | V1 and V2 support both | **Verified** (`f1841e1`) — `.len` proven on a struct field and in parameter arithmetic; `.data` proven as a real pointer value (pointee read, pointer equality against shared/distinct backings, nil comparison, and pointer-argument use), not just as an index/slice base. |
| String `.len` | V1 string code uses `strlen` but has no structural member | V2 exposes byte length as `.len` | **Verified V2 extension** in real string consumers |
| Optional presence member | V1 spells it `.is_some` | V2 spells it `.has_value` | **Intentional rename** |
| Struct field and instance method selection | Both compilers support it | **Verified** (`e09f9f8`) — a new shared value-builder (buildStructValueNode/buildStructFieldValueRead) covers every non-local receiver shape: a call result, a nested field read, an array/slice/tuple element, a force-unwrap, and a parenthesized expression, for both field reads and method-call receivers, across every field type. Generic and runtime-owner gaps resolved. Remaining out-of-scope shapes (a char-typed field; `(*pp).x` on a pointer local) are separate, already-tracked general limitations, not selection-specific |
| Enum and union variant selection | Both compilers support it | **Verified** (`1414a3a`) for value-source shape — a tagged-union switch subject or call-argument value is now accepted from every source enum-typed selection already supported (struct field, force-unwrap, parens, deref, non-addressable field, nested aggregate), not just a local/direct-call/fresh-construction. Untagged unions remain a separate "Decision needed" row; unsupported payload/container shapes are separately tracked (payload widening closed `9a5342e`; array/slice/tuple union elements remain documented container gaps). Generic-self selection resolved |
| Enum variant literal as a direct call argument | n/a | backend gap, found 2026-08-12 during Phase 3 #1 (module-level constant) investigation | **Resolved** (`0266bb2`, Phase 3 #13) — `check(Color.green)` and the zero-payload call form `check(Color.green())` now emit directly via `buildEnumValue`, no local-binding workaround needed |
| Enum call argument from a call result, struct field, or integer-to-enum cast | n/a | backend gap, found 2026-08-12 during Phase 3 #13 | **Resolved** (`2a76a27`, Phase 3 #35) — `buildEnumValue` already built correct C for all these node kinds internally (used elsewhere, e.g. a local declaration initializer); the gap was purely `buildCallArgument`'s own dispatch never recognizing them and delegating. Fixed by adding dispatch cases for `check(pick())` (DirectCall), `check(s.c)` (Load of a FieldPlace), `check(1 as Color)` (CheckedIntegerToEnum), plus a bonus fourth shape found during testing: `check(mk().c)` (a FieldValue — field read off a call-result struct VALUE rather than an addressable local). Confirmed IndirectCall-with-enum-result is genuinely unreachable from real source, so no case was needed there |
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
| `none` and `some value` | V2 optional nodes | **Verified** (`ffb365b`) — a real correctness bug fixed: `none`/`some` for an otherwise-unreferenced enum/union payload crashed cc ("unknown type name pebble_enum_N_t") because a NoneOptional carries no value the typedef collectors could discover the payload through; both collectors gained an optional-payload scan. `some` wrapping a non-trivial expression (call result, field read, tuple ordinal) confirmed already working across every position |
| Enum/union pointer-pointee typedef collection | n/a | `*EnumType`/`*UnionType` nil pointer whose pointee type is never otherwise used as a value | **Resolved** (`6c0af95`) — mirrored `collectStructTypesWalk`'s pointer-pointee rule into both `collectEnumTypesWalk`/`collectUnionTypesWalk`, plus a Parameters scan on both callers for the helper-parameter shape a value-node walk can't see. Uses `isDefinitelyEnumType` rather than `isEnumType` to avoid wrongly collecting an opaque extern pointee (e.g. `*FILE`) as an enum |
| Context expression | V1 and V2 expose the hidden allocator/context value | **Resolved.** The `Allocator`/`Context` ordinary-struct redesign (proposal 15) is fully complete — all 4 slices, `context` working in every value position (argument, return, local initializer, struct-field construction/assignment). |
| Identifier, module member, partial member | V2 symbol and member value paths | **Verified** (`7dec3fc`) — a real bug fixed: a module-member/global/extern-variable read used as a struct field's construction value (`P.{ x = lib::counter }`) was checker-accepted but Emit-rejected, because both typedef collectors walked only node.Children, missing a RecordConstruct's field values (stored in node.Fields). Plain identifiers and the base-less `.name` enum-variant shorthand confirmed already working across every value position |
| Grouped expression | Parser-only grouping in both compilers | **Verified** by construction; no backend behavior |
| Interpolated string value | V1 materializes a string expression and formats string, bool, signed/unsigned integer, float, char, enum, struct, and tuple parts | V2 builds `InterpolatedString` TIR; as of Phase 3 #36 (`395eb2c`) the backend also materializes it as an ordinary `str` value (local, argument, return, comparison operand, reassignment), not just a `print` operand — still only when every value part is `bool`, matching `print`'s own restriction | **Partial** — bool-only value-part scope remains a real, separately-tracked gap (see the Phase 3 gap table's "Interpolated string with a non-`bool` value part" row) |
| Direct call | V2 supports helper and extern direct calls | **Verified** (`0f15fd0`) — the helper half via the function-declaration matrix, the extern half via four real libc signature shapes (int->int, f64->f64, two-f64->f64, str->uint), each confirmed to lower to the real C name with no hidden context |
| Indirect call | V2 supports non-capturing function values | **Verified** (`e8672f7`) — confirmed distinct from the "Function type and function value" row's signature restriction; fixed the real call-site gap: a function-typed field of a non-addressable struct value (a call result) as the callee, local init, argument, or field-construction value, previously rejected. An 11-shape sweep found no other call-site gaps (array/slice-of-function-value elements remain rejected, already tracked separately) |
| Method call | V2 supports instance calls | **Verified** (`0f97ff8`, owner half also `e09f9f8`) — owner/receiver-shape half fully covered by Phase 3 #11's `buildStructValueNode` fix; the argument-shape half had a distinct gap: method-call argument destinations are fresh unconstrained solver slots (an instance method's symbol resolves only in the solver, unlike a direct call's walk-time KNOWN-parameter anchor), so an inline array/tuple/`some`-literal argument kept its own self-typed structure and failed classify() against the concrete parameter type. Fixed in `callMember` by unifying any argument whose source still carries an unresolved aggregate shape with its destination once the method's concrete parameters are known. An empirical sweep of struct/tuple/array/slice/enum/optional/fn-value arguments from literal/call-result/field-read sources, plus generic methods, found no other method-specific gap; a scalar-width-widening gap (u8->u32, bare int->i64 without a literal) reproduces identically for plain calls and is general, not method-specific — separately tracked |
| Struct call-result/field-read as argument or receiver | n/a | checker-accepted, was Emit-rejected | **Resolved** (`008b6fd`) — `buildAggregateArgument`'s struct branch gained a `DirectCall`/`MethodCall` case (delegating to `buildDirectCallNested`) and its `Load` case now also accepts `FieldPlace` (via `buildPlaceLValue`), mirroring the already-correct local-declaration precedent |
| Tuple call-result/field-read/deref-read as argument | n/a | checker-accepted, was Emit-rejected | **Resolved** (`181f7c6`) — the tuple-side mirror of the row above. `buildAggregateArgument`'s tuple branch gained the same `DirectCall`/`MethodCall` case plus a `Load` case handling both `FieldPlace` and `DereferencePlace` |
| Inline tuple-literal struct field construction | n/a | checker-accepted, was Emit-rejected | **Resolved** (`98ab9ae`) — `buildTupleValueExpr` named its compound-literal cast after the tuple literal's own (checker-interned, structurally-distinct) TypeID instead of the field's declared TypeID. Now takes an explicit target-type parameter, with a defensive shape-equality check (`tupleSameCShape`) rejecting a genuine mismatch cleanly. Verified `buildStructValueExpr` does NOT have the same bug (nominal structs canonicalize to one TypeID per declaration) |
| Generic call | V2 specializes named generic functions | **Verified** (`ed48868`) — the same generic function specialized at a generic-struct type argument, a three-level type nest, and a nested type argument in a non-first parameter position, with an emitted-C check confirming no residual unsubstituted type parameter |
| Index | V1 checks array, slice, string, and pointer indexing; V2 checks array, slice, and string and uses Unicode decode for string reads | **Intentional string change** and **partial** aggregate proof |
| String index result | V1 returns one byte; V2 stores bytes but walks UTF-8 from the start and returns the scalar at the requested code-point index | **Verified V2 semantic change** |
| Slice expression | V2 checked slices work in ordinary and nested expression positions, including GNU statement-expression lowering where a temporary is required | **Verified** (`36c47ff`) — the struct-literal slice field gap `836fbea` left open is closed: buildStructValueExpr now folds the whole struct literal into a statement-expression when a field's slice construction needs a pre-statement, so it works as both a call argument and a return value |
| Tuple literal | V2 `TupleValue` | **Implemented**, but whole-value copy paths are partial |
| Array literal and repeat | V2 `ArrayValue` and `ArrayRepeat` | **Verified** (`b3020aa`) — 5 real gaps closed: ArrayRepeat (`[v; N]`) as a call argument and as a struct-field construction value were both entirely unsupported (Emit-rejected); both call sites plus the existing local-declaration path gained an aggregate-typed repeated-value branch (`[Point.{...}; N]`, `[(1,2); N]`); and the checker's `finishArray`/`finishArrayRepeat` forced a hard Equal between an element and its KNOWN destination type, wrongly rejecting a same-width-but-distinct-kind value (an `int` call result into `[N]i32`/`[N]i64`) that a plain scalar local initializer already accepts through the ordinary compatibility record. A ~40-shape empirical sweep found no other array-literal/repeat-specific gap; bare scalar-width widening without a literal, bool-as-int cast, nested array-of-array, and array-typed tuple elements all reproduce identically for non-array code or are already separately tracked |
| Struct literal | V2 `RecordConstruct` | **Resolved (`e649476`)** for plain deep struct/tuple/optional nesting; runtime Allocator/Context construction is resolved; array-of-aggregate struct fields remain a separate, out-of-scope backend gap |
| Tagged-union variant literal | V2 `VariantConstruct` | **Verified** (`a7a8077`) — the stale "Partial by payload C shape" wording predates Phase 3 #8's closure of the scalar payload-width restriction. A ~40-shape sweep of construction across every payload type/position found one distinct, real gap: a variant constructed as the RETURN VALUE of a bare (brace-less) single-statement switch case body (`case .value: return C.value(5);`) was rejected — buildSwitchCaseBody's bare-return path re-implemented only a subset of return shapes and fell through to the plain-integer path for everything else, even though the block-body form and switch-expression fallback both already used the complete buildReturnStatement. Fixed by delegating the bare-return path to buildReturnStatement directly; also confirmed the real std/result.peb `Result.map` shape (a generic union constructed in a case-body return) was affected. Aggregate payloads, inline nested-union construction, and union-as-container-element remain out of scope per already-tracked rows |
| `sizeof(T)` | V1 rejects opaque types but otherwise delegates to C | V2 supports scalar, struct, enum, union, tuple, optional, slice, pointer, runtime, and fixed-array types, plus a direct cast of `sizeof` (`634db99`), `sizeof [N]Struct`/tuple/optional (`cf97cd3`), and a bare `sizeof (T,U)`/`sizeof ?T` with no array wrapper (`392ae16`). | **Verified** |
| Force unwrap | V2 checked optional unwrap | **Resolved** for every scalar payload type (i32/i64/u64/bool/pointer plus u8/u16/i8/i16/u32, `9426382`) |
| Postfix `++` and `--` as a value expression | V1 uses C postfix semantics and returns the old value | V2 defines them as void updates that are legal only as statements or for updates | **Intentional difference** |
| Arithmetic `+ - * / %` | V1 emits raw C arithmetic for all numeric types | V2 uses checked helpers for integers and direct C for floats | **Verified** (`f54e6ef`) — a full ~78-pair sweep (5 operators x every integer width) of the plain-binary-expression form found the "helper-width matrix" fully correct already: i32/i64/int lower through checked helpers for all 5 ops, u64 through checked helpers for + - *, u8/u16/i8/i16/u32 and u64 / % all reject cleanly at Emit naming the operator and width (no missing-helper panic), and uint lowers as plain C for all 5 ops. The one real, distinct gap was in the COMPOUND-assignment form: `%=` on a uint place was hard-rejected ("% is integral-only") even though plain `a % b` on uint, and every other uint compound operator, already worked — a leftover special case with no runtime-helper justification (uint has no checked helper for any operator). Fixed by routing `%=` through the same operator resolution the other four compound operators already use |
| Numeric comparisons `== != < <= > >=` | Both compilers support numeric comparisons | **Verified** (`b086dfd`) — all six operators x true/false on u8, i8 (signed, negative values), uint, f32, f64, and u64; no gaps found |
| String equality and ordering | V1 uses C string comparison paths | V2 has length-aware string runtime paths | **Verified** (`b086dfd`) — all six operators proven for empty strings, non-prefix different-length ordering, byte-value ordering, and length-aware equality (a byte-for-byte prefix does not compare equal); no gaps found |
| Boolean logical `&&` and `||` | V2 uses `ShortCircuitValue` | **Verified** for short-circuit sequencing |
| Bitwise `& | ^` | V2 checker accepts integral types; backend coverage is width-specific | **Verified** (`05a4d6e`) — a full ~30-pair sweep (3 operators x every integer width) found one real gap: buildUintExpr (the dedicated uint grammar) had no BinaryValue case at all, so any uint `& | ^` expression died at Emit ("unsupported uint expression node BinaryValue") even though every other width already lowered correctly through buildExpr's BinaryValue case. Fixed by adding the case, mirroring the existing pattern exactly (both operands recurse into buildUintExpr, combined via the plain C operator — uint has no checked helper for any operator). The `&= |= ^=` compound form does not exist in this language at all (no lexer token in either compiler) and bool is not a valid operand (checker rejects it) — both confirmed out of scope, not gaps |
| Shifts `<< >>` | V1 emits C shifts; V2 uses checked helpers | **Verified** (`8718b00`) — a full ~20-pair sweep (2 operators x every integer width) confirmed the row's claim exactly: `uint`/`u64` were the only widths without a checked shift-helper pair, `u64` a clean Emit rejection and `uint` an "unsupported ... node CheckedShift" backend gap (buildUintExpr had no CheckedShift case at all). Fixed by adding `pebble_rt_checked_shl_u64`/`shr_u64` to the C runtime (mirroring the existing u32 pair's SAFE-mode abort / RELEASE-mode mask structure exactly) and wiring both Go-side dispatch points (`checkedShiftSuffix` now maps `Uint`/`U64` to the shared helper, and `buildUintExpr` gained a `CheckedShift` case). Every other width already worked |
| Unary numeric negation | V2 uses checked negation for integers | **Verified** (`ea6525e`) — a full sweep confirmed i32/i64/int already worked, unsigned widths are intentionally checker-rejected for unary minus (not a bug), and floats already worked via plain-C negation; i8/i16 non-negation of a non-constant operand was the one real gap (Emit-rejected, "no checked-neg runtime helper"). Fixed by adding `pebble_rt_checked_neg_i8`/`neg_i16` to the C runtime (mirroring the i32/i64 pair's exact SAFE/RELEASE structure) and a dedicated `checkedNegSuffix` selector (mirroring `checkedShiftSuffix`) so negation widens independently of `checkedSuffix`'s other, deliberately-narrower consumers. Surfaced and fixed two pre-existing tests that pinned the old "narrow width folds any literal" emitted-C text, now correctly reserved for the width's unspellable-minimum edge case only |
| Logical not | V2 supports bool | **Verified** |
| Address-of and dereference | V2 place model supports scalar and field/index paths | **Verified** (`75fc89a`) — a ~45-shape sweep found two real, distinct gaps beyond Phase 3 #3's earlier work: a whole ARRAY read through a pointer dereference (`*p` where `p` is `*[N]T`) was unsupported as a call argument and as an array-typed struct field's construction value (both Emit-rejected); it already worked as a return value. Fixed by extracting the existing return-value lowering into a shared helper and reusing it at both new call sites — no new lowering strategy. Everything else already worked: struct/tuple/array/slice address-of and whole-value dereference across every position, method calls through a dereferenced receiver, address-of a field/index path, deref chaining, and pointer injection into an optional |
| Bitwise not | V2 accepts integral types | **Verified** (`55be9b9`) — the same bug class Phase 3 #21 found for uint's `& | ^`: buildUintExpr had no PrefixValue case at all, so uint `~x` died at Emit ("unsupported uint expression node PrefixValue") even though every other width already lowered `~` correctly through buildExpr's own PrefixValue case. Fixed by adding the case, mirroring the existing pattern exactly. bool is not a valid `~` operand (checker-rejected) — confirmed intentional, not a gap |
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
| Float to integer | explicit | explicit, checked runtime conversion | **Verified** (`f52bbaf`) — a 20-pair sweep (f32/f64 x every integer destination width) found both anticipated gaps real: the runtime had checked conversion helpers only for i32/i64 destinations (i8/i16/u8/u16/u32/u64 Emit-rejected), and uint failed differently ("unsupported uint expression node FloatToInteger", buildUintExpr had no case). Fixed by adding 12 checked conversion helpers (mirroring the existing i32/i64 pair's SAFE-mode NaN/range-check and RELEASE-mode sentinel structure exactly) and a dedicated `floatToIntSuffix` selector (mirroring `checkedShiftSuffix`/`checkedNegSuffix`) so the family widens independently of `checkedSuffix`'s other narrower consumers. i32/i64/int already worked correctly, including SAFE-mode abort on NaN/±Inf/out-of-range |
| Integer to integer | explicit and implicit | explicit, plus contextual literal fit | **Implemented**, backend proof needed by pair |
| Character to integer | explicit | explicit | **Verified** (`20efd9a`) for int/i8/i16/i32/i64/u8/u16/u32/u64, boundary values, overflow semantics, and non-literal sources. `char as uint` was broken (`buildUintExpr` had no `CharToInteger` case) — **Resolved** (`60a3346`) |
| Integer to character | explicit | forbidden | **Intentional difference** until Unicode scalar validation is specified |
| Enum to integer | explicit | explicit | **Verified** |
| Integer to enum | absent as a general V1 cast; V1 has partial enum inference | explicit checked cast, plus optional checked form | **Verified V2 extension** |
| Pointer to pointer | explicit; `*void` conversions are also implicit | explicit only | **Intentional stricter rule** |
| Pointer to integer | explicit | explicit | **Verified** (`20efd9a`) for pointer-width-or-wider destinations (u64, uint, i64) across int/struct/opaque-extern pointees. **New finding**: any NARROWER destination (u8/u16/u32/i8/i16/i32/int) is checker-accepted but fails at `cc` under `-Werror -Wpointer-to-int-cast` — see the gap table below. |
| Integer to pointer | explicit | forbidden | **Intentional difference** |
| V1 `str` to/from `*void`, `*u8`, or `*char` | explicit or implicit, because V1 `str` is a C pointer | absent for V2 `PebbleStr` | **Intentional ABI difference**; use explicit library adapters if accepted later |
| Fixed array to slice | implicit | dedicated checked slice shape, including direct array-literal initialization of a slice binding | **Verified** (`36c47ff`) by source position; binding form resolved in `f4c3970`. Implicit array-to-slice coercion outside a binding initializer (a field/arg/return) is confirmed a deliberate checker rejection (C0601), not a bug |
| Tuple literal element conversion | implicit, equal tuple length | **RESOLVED for local declarations (`d905ab6`, 2026-08-10)**; checker builds `TupleCoerce`, backend now accepts it in a local declaration initializer | — |
| Explicit tuple prefix cast | source can have more elements than destination | V2 requires equal length | **Absent** unless the narrower V2 rule is accepted |
| Array literal element conversion | implicit for equal length | no general structural conversion class | **Verified** (`b3020aa`, Phase 3 #18) — a scalar element sourced from a call result (a distinct term from the array's destination element type, `int` into `[N]i32`/`[N]i64`) now converts through the ordinary compatibility record, matching how a plain scalar local initializer already worked; `finishArray`'s hard Equal against a KNOWN destination element was the root cause, not a missing conversion class |
| Struct literal field conversion | implicit for equal field count and matching names | no structural struct conversion class | **Absent** |
| Explicit structural struct prefix cast | source prefix can cast to a smaller destination struct | forbidden | **Absent** unless nominal-only conversion is accepted |
| `none` to any optional | implicit | contextual optional construction | **Verified** (`5956816`) — none-initialized ?i32/?bool/?*int/?Point all read `has_value == false`; some-constructed counterparts read `has_value == true` with scalar unwrap round-trip |
| `some S` to optional `T` with payload conversion | implicit for a literal `some` | optional injection exists, but payload and backend shapes are limited | **Verified** (`17d0eb1`) — distinct from Phase 3 #7 (payload types) and #14 (value-source/typedef gaps): a `some <value>` whose payload's own type needed a width/type conversion to match the destination's declared payload type (a u8 local wrapped into `?u32`) was checker-rejected with C0601, because the SomeExpr typed itself as `?<payload's own type>` regardless of any known destination. Fixed by pinning the SomeExpr's optional type to a known destination's optional type at solve, then wrapping the payload in the ordinary coercion node at IR-build — mirroring the existing tuple-element coercion mechanism exactly. The literal half already worked. Confirmed out of scope: operator-payload conversion and plain (non-optional) width widening are general, already-tracked gaps; V1 has no bare-value optional injection to compare against |
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
| Interpolation | V1 formats string, bool, integer, float, char, enum, struct, and tuple parts and produces a string value | V2 materializes interpolated strings in general value positions and supports bool, integer, float, str, char, plain-enum, non-nested-struct, and non-nested-tuple parts | **Resolved for all non-nested scalar/aggregate kinds** (F5-01 through F5-09) — tagged-union payload recursion and nested-aggregate recursion remain unscheduled |

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
| Range loop, exclusive and inclusive | Both compilers support both end rules | **Verified** (`61eb1c5`) — a real infinite-loop bug: an inclusive range's (`..=`) unconditional post-body `i += step` advances the iterator one PAST the end bound, which wraps for an unsigned iterator descending through 0, or for either signedness ascending past the type's own max, and the old step-direction-ternary condition then reads the wrapped value as still in range — confirmed hanging forever (`loop 5..=0 : i` over u8, `loop 250..=255 : i` over u8, the i8 ascending-max equivalent). The exclusive form was never affected (its last step is always from one past the end toward it, never past a representable boundary), which is why this survived the four prior range-loop fixes — none touched the inclusive boundary. Fixed with a done-gate set from the iterator's pre-increment value inside the for-loop's increment clause, continue-robust since C's `continue` jumps to the increment clause where the done test lives |
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
| Deferred local declaration | V1 accepts it and emits it in a defer-local C block | V2 checker permits deferred bindings, but `buildDeferredStatements` rejects `Initialize`; bare deferred locals also leak into the enclosing checker scope | **Verified** (`2c1c867`) — both halves were real. Resolver: `resolveStatement`'s DeferStmt case resolved its child with the enclosing scope, so a bare `defer var x = 5;` leaked x into the surrounding function (a block-wrapped deferred local already got its own scope via resolveBlock). Fixed by resolving the DeferStmt's child inside a fresh ScopeBlock, mirroring resolveBlock's own pattern. Backend: `buildDeferredStatements` had no case for `tir.Initialize` (rejected outright) or `tir.Block` (also broken, though only Initialize was named in the audit) — fixed by adding both, each building over a cloned/discarded locals scope via the existing buildLeadingStatement/buildFallthroughBody machinery, wrapped in a fresh C block mirroring V1's defer-local block exactly |
| Deferred block, conditional, loop, or switch | V1 checker recursively accepts any statement except return, and codegen calls the normal statement emitter at the exit | V2 validation specification permits these statements, but `buildDeferredStatements` has no builder case for their TIR nodes | **Verified** (`7f5732c`) — mostly closed as a side effect of Phase 3 #29's `tir.Block` case, which already covers a conditional/loop/switch INSIDE a deferred block via its `buildFallthroughBody` delegation. The one distinct remaining gap was a BARE deferred control-flow statement with no enclosing block (`defer if ...`, `defer while ...`, `defer loop ...`, `defer for ...`, `defer switch ...`) — checker-accepted (C0613 permits it) but Emit-rejected for all five TIR node kinds. Fixed by adding a case for each, delegating to the same builder the non-deferred fall-through dispatch already uses, wrapped in a fresh C block mirroring V1's defer-local block — no new lowering strategy |
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
| C variadic extern call | V1 permits primitive C variadic use | V2 reports C0604 (imprecise filing — the actual gap is one layer earlier: the parser has no syntax for a bare, nameless `...` at all, confirmed live via `extern fn printf(fmt *u8, ...) i32;` failing at P0002/P0004, never reaching C0604) | **Decided 2026-08-13, deferred — not needed right now.** Support a real C-ABI bare `...` as the final parameter of an `extern fn` declaration only, with NO interop type checking on the variadic call-site tail — pure unsafe passthrough, the same treatment as the untagged-union decision. Pebble's own checked variadic (`...name []T`, already fully working, including on `extern` declarations) remains the safe alternative. Confirmed nothing in `std/` or the example programs currently needs this — no pressing dependency, so this stays a decided-but-unscheduled item (see `18-future-safety-and-ergonomics-roadmap.md`) rather than an active Phase 3 dispatch. A first implementation pass was dispatched and then abandoned mid-run once the "not needed right now" call was made — do not resume or trust any leftover working-tree state from that attempt without re-verifying from scratch |
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
| Materialize an interpolated string as a local, argument, result, or ordinary value | `InterpolatedString` is handled only inside `buildPrint`; general string builders reject it | **Resolved** (`395eb2c`, Phase 3 #36) for literal text and `bool`-typed value parts, matching `buildPrint`'s own existing scope exactly — new runtime primitive `pebble_rt_str_from_parts` (allocates and concatenates through the context allocator) plus backend wiring into `buildStrOperand`/`buildStrLocalDeclaration`/`buildStoreCore`'s str-reassignment case, reusing `escapeCString`/`buildBoolExpr`. A real heap-buffer-overflow bug (bool-part byte-length ternary backwards) was found and fixed during independent verification, not by the dispatch itself, which had not run its own tests. Interpolating non-`bool` value types (int, float, str, enum, etc.) remains a separate, explicitly out-of-scope follow-up — see the new row below |
| Interpolated string with a non-`bool` value part (int, float, str, enum, etc.) | n/a | backend/runtime gap, found 2026-08-13 during Phase 3 #36, deliberately deferred | **Partial** — integer value parts **Resolved** (`7e4bcb4`, Phase 3 #49) and float value parts **Resolved** (`8dfcf51`, Phase 3 #50): both share the same runtime mechanism — `pebble_rt_str_from_parts` refactored from a fixed per-kind length table (bool is always 4/5 chars) to a single measure+format pass, formatting each variable-length part once via `snprintf` into a per-part scratch buffer (widened from 24 to 320 bytes for float's `%f` worst case — a full-range `double` near `DBL_MAX` renders as 309 integer digits — verified against the actual empirical worst case, not guessed), freed before return on both paths. Float uses the same `%f` default-precision convention `print`'s own bare float path already uses, confirmed identical via a direct cross-check test. Str, char, enum, struct, and tuple value parts remain **Absent** — each is its own follow-up slice of the same general formatting-matrix scope |
| Enum-typed fixed-array element | `arrayElementCType`, `types.go` | **RESOLVED** (`94a2a39`, 2026-08-10) |
| Enum-typed slice element | `sliceElementCType`, `types.go` | **RESOLVED** (`94a2a39`, 2026-08-10) |
| Ordinary `some Color.red` optional enum payload | ~~accepts only the integer-to-optional-enum cast path~~ **Resolved (`1bf785d`)** | **Closed** |
| Enum-typed struct field construction (`Holder.{ c = Color.blue }`) | ~~`collectEnumTypesWalk` had no `RecordConstruct` case, so the enum's typedef and variant constant were never collected when only reachable via a field's construction value~~ **Resolved (`d19717c`)** | **Closed** |
| Tagged-union-typed struct field construction (`Holder.{ u = Choice.value(5) }`) | ~~fails identically to the enum case above~~ **Resolved (`e3478af`)**, requiring both a builder-routing fix and a matching `collectUnionTypesWalk` collection fix | **Closed** |
| Tuple ordinal read of a `char`/`str` element | ~~checker accepts, Emit cleanly rejects each: `char` hits `buildCharOperand`'s `TuplePlace` gate, `str` hits the str-local initializer's `Load` gate~~ **Resolved (`bd84ee9`)**. `str` in a VALUE position (not a local declaration, e.g. a call argument) has an adjacent, still-open gap in `buildStrOperand`'s `FieldPlace`-only `Load` case — not yet formal. | **Closed** for the local-declaration shape |
| Tuple/struct-field `f64` member | rejected entirely — `f64` is not accepted as a struct field type at all, and by extension a tuple element read-back; `f32`/`f64` were only ever wired up for helper parameters/results (task #22), never aggregate members | **Resolved**, task #86, sliced by aggregate kind — all four sub-tasks now landed. Tuple/array element read: `e5add48` (slice 86a). Struct field (typedef, construction, read, write): `8bd34f6` (slice 86b). Optional payload (typedef, construction, force-unwrap, plus new `pebble_rt_checked_unwrap_f32`/`f64` runtime helpers): `11e9b83` (slice 86c). Slice element (typedef, construction; read/write already worked for free): `c39b553` (slice 86d). Two pre-existing general gaps found during 86d and confirmed NOT float-specific are tracked separately — see the two rows below |
| Slice construction inside a float-returning entry | n/a | malformed C — empty `checkedSuffix`/`cType` | **Resolved** (`6f80778`) — `Emit`'s `result` (the entry's declared result kind, which can be a float) was threaded directly as the INTEGER `width` every checked slice/index helper's suffix and C type is selected from. Now a separate `width` variable falls back to `types.Int` whenever `result` has no integer `cType`. Also fixed a second, independent instance of the same bug class in `buildFloatExpr`'s `IntegerToFloat` case (a leftover from task #86 slice 86a's `entryWidth` threading) |
| Slicing a wrapped array parameter inside a helper | n/a | invalid C — array typedef used as slice base instead of `.data` | **Resolved** (`91a50f4`) — `buildSliceConstruction`'s array-symbol-base case now checks `localInfo.arrayWrapped` and appends `.data`, mirroring the identical check already used in 5 other places in this backend |
| `char as uint` | `buildUintExpr` formerly had no `CharToInteger` case | **Resolved (`60a3346`)** |
| Pointer cast to a destination narrower than the pointer (`ptr as int`/`u8`/`u16`/`u32`/`i8`/`i16`/`i32`) | checker-accepted, backend emits a plain `(destType)(ptr)` cast, but `cc` failed under this project's required `-Wall -Wextra -Werror` with `-Wpointer-to-int-cast`; only pointer-width-or-wider destinations (`u64`/`uint`/`i64`) actually compiled | **Resolved** (`297e162`) — the project owner decided against truncation; the checker now rejects a narrow destination cleanly (`isPointerWidthInteger` gates both `classifyComposite` and `coercionFor`), with a single C0601 diagnostic and no C0619 leak. Wide destinations unaffected |
| Platform-sized `int`/`uint` lowering | The ABI spec requires target-native signed and unsigned word representations such as `intptr_t`/`uintptr_t`. The checker has `LiteralTarget.WordBits`, but production `pebc` hardcodes 64; backend `cType`, checker compatibility width, and checked-helper selection instead hardcode `int` as i32 and `uint` as u64. | **Confirmed implementation defect, F5-25.** The language decision already exists. Fix target-width configuration, emitted C spelling, compatibility, and runtime-helper dispatch as ordered slices; do not narrow the checker to i32. |
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
| Function value with C convention, variadic signature, or unsupported aggregate result | `validateFunctionTypeSignature` (`internal/backend/validate.go:306`) restricts the signature | **Partial** — float parameter/result widened (`cd6e604`); C convention, variadic, and aggregate parameter/result remain restricted |

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
| Tagged-union variant payload | any fixed-width integer at its own width, `bool`, `char`, `str`, float, a plain enum, or a nested tagged union (`9a5342e`) | tuple, struct, pointer, array, slice, optional, and function value — each needs an aggregate typedef the current union-leads-aggregate typedef order can't yet satisfy without a larger refactor |
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
| Slice call argument | matching local/parameter, checked or raw construction, field read, or slice-returning call | no current failure in these audited source shapes |
| Enum or tagged-union call argument | matching local/parameter, direct helper result, inline variant, supported field value, or integer-to-enum cast | no current failure in these audited source shapes |
| Tuple call argument | matching local, inline tuple literal, tuple-returning call, field read, or whole dereference read | general tuple-valued expressions outside these shapes need focused proof |
| Struct call argument | matching local, inline record literal, struct-returning call, field read, or whole dereference read | general struct-valued expressions outside these shapes need focused proof |
| Fixed-array call argument | matching local, inline literal, scalar-element repeat, array-returning call, or whole dereference read | aggregate-element `ArrayRepeat` (F5-10) |
| Fixed-array return | matching local, inline literal, scalar-element repeat, array-returning call, or whole dereference read | aggregate-element `ArrayRepeat` (F5-11) |
| Slice return | matching local, checked slice, or raw slice construction | slice-returning call and slice field value |
| Tuple return | matching local or inline tuple literal | tuple-returning call and other tuple-valued expressions |
| Struct return | matching local, inline record literal, struct-returning call, field value, whole dereference read, or indexed read | general struct-valued expressions outside these shapes need focused proof |
| Optional argument/return | matching local, `some`, `none`, optional injection, optional-returning call, or supported bare payload injection | optional field read as a call argument (F5-16); other optional-valued expressions need focused proof |
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
| ~~V2 checker accepts string switch, but backend has no lowering~~ | **RESOLVED (`49d0f23`); call-valued subject single evaluation also resolved (`b1a53e7`).** String switch lowers to an if/else chain through `pebble_rt_str_eq`, with the subject materialized once. | — |
| ~~V2 does not prove a complete `u8` or `i8` switch exhaustive~~ | **RESOLVED (`4817dae`).** Added covered-integer-value tracking plus a full-range check for `u8`/`i8` specifically (256-value domains, small enough to enumerate) — wider widths are explicitly unaffected, still always require a fallback. 5 new tests, independently verified at the checker level, causation-checked. | — |
| ~~`u8` (and presumably other non-entry-width integers) is not accepted as a switch subject type by the *backend* at all~~ | **RESOLVED (`2b3d684`).** `buildSwitchStatement` now accepts every concrete fixed-width integer builtin. The abstract `uint` follow-up was also resolved later in `f44133d`. | — |
| ~~a negative integer literal in a switch case label (`case -5:`) is rejected outright for a signed subject type~~ | **RESOLVED (`8f643cd`).** `buildCaseLabel` gained an `isNegativeDecimal` path: accepted on a signed subject at its own width, cleanly rejected on unsigned (confirmed the checker already independently enforces this via `T0508`, so the backend guard is defense-in-depth). Also found and fixed a related, more severe pre-existing bug surfaced while building the reproduction: `CheckedNegate` at a narrow width (e.g. i16) has no `pebble_rt_checked_neg_*` runtime helper, so a negative-literal initializer would have emitted a call to a nonexistent function — fixed via literal-only constant folding (`checkedNegateLiteral`). Causation-checked. | — |
| ~~the abstract `uint` builtin is still rejected as a switch subject at the backend~~ | **RESOLVED (`f44133d`).** `buildSwitchStatement` gained a dedicated `isUint` branch calling `buildUintExpr` — the same builder every other uint value position (parameters, locals, globals) already uses — since uint doesn't fit the fixed-width-integer predicate `2b3d684` added. Case labels spelled at uint's own width. Verified; u8/entry-width int subjects unaffected; causation-checked. | — |
| ~~`u8` (and presumably other non-entry-width integers) is not accepted as a function *parameter* type at all~~ | **RESOLVED (`c39416b`).** New `isFixedWidthInteger` predicate plus a matching case in `helperSignature` and `validateHelperSignature` — a u8/i16/u32/... parameter is now declared at its own C type and seeds the callee's scope at its own width, mirroring the switch-subject widening (`2b3d684`). Verified: a helper taking a `u8` parameter, called with a `u8` argument, compiles and runs (returns 5). Entry-width/uint/u64 parameters unaffected. Causation-checked. | — |
| ~~Literal descending range loops execute zero iterations~~ | **RESOLVED (`8baeb8e`)** for plain positive literal bounds. The fourth pass confirms that runtime descending bounds and negative-literal bounds still silently execute zero iterations. | — |
| ~~A nontrivial range end is evaluated on every loop test instead of once~~ | **RESOLVED (`e111c37`).** Each bound now evaluates once. The fourth pass found a separate order defect: V2 evaluates end before start, while V1 and source order evaluate start before end. | — |
| ~~Mutable globals have no backend storage~~ | **RESOLVED (`14739f3`).** Real backend storage for both read and write, supporting integer/uint/bool/char/float/str/plain-enum globals. Required a small necessary checker/TIR companion (`bindingGlobalVar`'s initializer is now recorded as a real TIR node, not just validated and discarded). 13 new tests; independently verified with real cross-function/in-loop mutation, causation-checked across all 10 touched files. | — |
| ~~a global's constant initializer that isn't a literal leaf (e.g. `var x int = 1 + 2;`) is cleanly rejected as not C-static-initializable~~ | **RESOLVED (`9e547fa`), scoped to integer-literal-only arithmetic.** Backend-side folding (`foldConstantIntegerTree`, `math/big`) for a `CheckedArithmetic` tree (+, -, *, /, %) whose operands are, recursively, all integer literals — folds to a plain C literal, range-checked against the global's declared type before narrowing (distinct overflow error, not a silent wrap or Go panic). A checker-accepted but non-foldable shape (a `CheckedNegate`, e.g. `var x int = -5;`) still gets the exact original rejection, confirmed by test — not general constant-expression evaluation (no locals, no calls, no floats). 4 new tests, causation-checked. | — |
| ~~Extern variables and constants have no backend declaration/use path~~ | **RESOLVED (`1372734`).** Reuses the mutable-globals fix's pattern (`14739f3`), but emits a forward `extern <ctype> <realCName>;` declaration instead of synthesized storage. Both read and write supported (checker already enforces extern-`let` immutability, so no backend-side mutability distinction needed). Real platform caveat found and documented: `errno` is a macOS header macro, not a linkable symbol, so runtime correctness is proven against a hand-written C shim instead. 9 new tests, independently verified against real hand-written shims (not just the test harness), causation-checked. | — |
| Composite print still lacks optional, pointer, and function-value shapes | confirmed from the current print matrix | decide and implement one value family per task |
| ~~Existing-slice variadic pass-through is absent~~ | **RESOLVED (`94e74f0`).** | — |
| General interpolated-string values | **Resolved for all non-nested scalar/aggregate kinds.** bool (`395eb2c`), integer (`7e4bcb4`), float (`8dfcf51`), str (`a785060`), char (`170ff96`), plain-enum (`cd5e3c6`), non-nested-struct (`7696630`), non-nested-tuple (`8b7d057`). | Tagged-union payload recursion and nested-aggregate recursion remain unscheduled |
| ~~V2 checker accepts string `+`, but backend cannot materialize its `BinaryValue` result~~ | **DECIDED + RESOLVED (`f4f2412`, 2026-08-09, direct instruction).** `str + str` (and any `+` with a `str` operand) is now a checker-level type error (`C0603`), not implemented — plain `str` is an immutable view, and real concatenation already has a first-class path via `String.push_str` (`std/string.peb`) with an explicit allocator; implementing `+` would need an implicit one, entangling with the deferred Allocator/Context redesign (proposal 15). Other `+` operand types (int, uint, float, ...) confirmed unaffected. The backend's own defense-in-depth rejection test for this shape is preserved via hand-built TIR, since real source can no longer reach it past the checker. | — |
| ~~Deferred declaration, block, conditional, loop, and switch forms pass V2 validation policy but have no backend lowering~~ | **RESOLVED (`7f5732c`).** All seven families lower, and the deferred-local scope leak is closed. | — |
| **RESOLVED (`9e04364`, 2026-08-10).** A companion crash was found during the same investigation, more serious than the missing-lowering gap above: `defer { return 1; }`, `defer { break; }` (targeting a loop outside the deferred block), and `defer if x { return 1; }` all crashed the compiler with a stack overflow — `C0613` only checked whether the deferred statement itself was directly `return`/`break`/`continue`/`defer`, not whether one was reachable nested inside a deferred block/if/loop; the IR builder's defer-chain walk then re-registered the same defer infinitely. Fixed at the checker level: `validateDefers` now walks the deferred statement's region subtree and rejects any exit whose target lies outside it as `C0613`, while correctly leaving alone a break/continue whose target loop is itself entirely inside the deferred block. Verified all three crash reproductions now reject cleanly; the contained-exit case still passes the checker; causation-checked by reverting and reproducing the exact stack overflow. This had to land before any of the 6 families above are implemented, since implementing deferred blocks/loops without this guard would only widen the crash surface. | — |
| ~~Assignment-form classic-for initializer reaches TIR but backend rejects it~~ | **RESOLVED (`e3ec6bc`, 2026-08-10).** Decision made: grammar acceptance is intentional (V1 parity, an ordinary for-loop pattern) — implemented rather than rejected earlier. Verified: local-to-local reassignment as the initializer, a bool-typed initializer, the no-condition variant, and a value-computed initializer; the declaration-form initializer and initializer-only/condition-only/update-only variants confirmed unaffected. An obsolete rejection test was found and removed. Causation-checked. | — |
| ~~Whole tuple, array, struct, enum, string, and slice copy initialization is incomplete~~ | **RESOLVED** for all six initialization types. Tuple and array reassignment from a call remain separate F5-12/F5-13 source-position gaps. | — |
| — struct: **RESOLVED (`9df0351` local/literal, `5ef060a` call value).** Whole struct-local reassignment now works for a pointer-deref/field write and a plain local, with the new value an in-scope struct-typed local, a fresh struct literal, OR a call to a struct-returning helper (`p = make_point();` / `*self = make_point();`) — all reproductions independently verified end-to-end (exit 9, causation-checked). | — |
| — tuple: **RESOLVED (`d1b05be`, local/literal only, 2026-08-10).** Whole tuple-local reassignment now works for a pointer-deref write and a plain local, with the new value an in-scope tuple-typed local or a fresh tuple literal (`buildTupleStoreValue`, mirroring the struct fix). A call to a tuple-returning helper on the right-hand side stays a deliberate, clean rejection (out of scope, same staged approach the struct fix used). Verified: local-to-local, pointer-deref, fresh-literal, a 3-element tuple, and a mixed-type `(int, str)` tuple. A stale test asserting the old blanket rejection was found and replaced. Causation-checked. | — |
| — array: **RESOLVED (`aef808e`, local/literal only, 2026-08-10).** Whole-array reassignment now works for a plain local and a struct-field-through-pointer-deref write. A standalone array local is a raw C array (not a wrapper-struct typedef like struct/tuple), so C cannot assign it with `=`; the store lowers to a `memcpy` instead. A real compounding typedef-collection bug (the array-literal case's compound literal needs a `pebble_array_<id>_t` typedef nothing was collecting) was found during independent verification, after an initial dispatch's own self-report claimed success without actually testing the literal-reassignment path — and fixed in the same change. An array-returning call on the right-hand side stays a clean rejection, confirmed unreachable from real source today anyway (returning an array literal isn't supported at all yet — separate, pre-existing gap). Verified: local-to-local, literal (5-element, bool-element), and pointer-deref-through-struct-field. Causation-checked. Enum and string reassignment remain untouched. | — |
| — enum-typed array/slice elements: **RESOLVED (`94a2a39`, 2026-08-10).** Both array and slice elements of a plain enum type work. The ordinary optional-enum payload follow-up was also resolved later in `1bf785d`. | — |
| ~~Entry-function recursion cycle is rejected~~ | **DECIDED (2026-08-09, direct instruction):** current V2 behavior (rejecting a call cycle through `main`) is correct and intentional — `main` is the entry point, it should not be callable from anywhere. Move to "Accepted V2 differences" below; not a defect. | — |
| Checked numeric helper coverage is incomplete | confirmed by the width-by-operation matrix and real `u8` invalid-C reproductions | fix one operation family and one width family per task |
| Each aggregate/container C position accepts a different child-type set | high from the dedicated array, slice, tuple, optional, struct, union, and function-type C-name gates | reproduce one container plus one rejected child type per slice; do not dispatch a general container task |
| ~~A tagged union used as a struct field or optional payload receives the plain-enum C type name~~ | **RESOLVED (`4d1ef51`).** Reproduced two stacked bugs: a typedef-ordering defect (union typedef emitted after first use, hard `cc` failure) and the predicted wrong-type-selection bug underneath it (confirmed real, but caught by `-Werror` as a hard error, not silent). Both fixed in `emit.go`; `structFieldCType`/`optionalPayloadCType` now use the existing `isTaggedUnionType` distinction. 5 new compile-run tests; construct-store-read-back round-trip and panic-on-none independently verified, not just clean compilation. | — |
| ~~`sizeof` a fixed array passes validation but the backend rejects it~~ | **RESOLVED (`cacaa28`).** The bare `sizeof [N]Struct`/tuple/optional follow-up was also resolved later in `cf97cd3`. | — |
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
| Freestanding compilation | V1 suppresses hosted headers, allocator adapters, and `print`; entry gets an empty context | V2 runtime header describes `PEBBLE_RT_FREESTANDING`, but the current backend `Emit` API has no freestanding configuration and always writes the hosted entry/context template | **Decision needed, confirmed not pressing 2026-08-13** — no current dependency on this; still genuinely undecided (unlike the C-variadic-extern row, which has an accepted design just deferred on scheduling), so it stays a real "Decision needed" item, just not an active one. Revisit whenever it becomes necessary, also a driver decision, not just checker/backend |
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
