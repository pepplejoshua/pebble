# 11 — raw pointers, `slice`, and the unsafe-operations boundary

This resolves `open-language-decisions.md` §3.3 ("pointer mutability,
nullability, and safety distinctions" — partially, see below) and §3.8
("unsafe pointer policy"), and reaffirms §1.5 ("pointer arithmetic,
ordering, and nil policy") unchanged. It does not resolve §1.5's
calling-convention items or anything else in that document.

## 1. Motivation

Method-call lowering (10.47) unblocked everything in `std/*.peb` except
three prerequisite backend features: methods (done), raw pointers, and
multi-module imports (already confirmed working, see 10.46). Raw pointers
are what's left, and they're the one prerequisite `std/string.peb` actually
needs — every mutating method on `String` takes `self *String`.

## 2. Scope decision: stay conservative, defer the bigger memory story

Considered and explicitly rejected for this round: tracing GC, reference
counting (ARC), and Rust-style ownership/borrow checking. Each is a correct,
well-precedented answer to memory safety (Go/Java/C#, Swift/Objective-C, and
Rust respectively), but each is also a project on the scale of the backend
itself — a new runtime subsystem, or a second type system layered on top of
the current one. None is proportionate to where this compiler is today (raw
pointers don't exist yet at all), and ARC in particular is not a stepping
stone toward the eventual goal — Swift's own retrofit of ownership onto ARC
is evidence the two don't compose cleanly. The project's own precedent
(`String`'s manual `new()`/`delete()`, chosen deliberately over any
compiler-enforced scheme) already points at a plain-manual v1, with a
narrower safety net layered on later.

**Decision:** ship raw pointers now with the narrowest safety net that's
actually cheap (a null check, matching every other `pebble_rt_checked_*`
operation this backend already has), and explicitly punt everything else —
generational/UAF-safety tracking, `any`-style type erasure, and any
ownership/borrow-checking model — to a deliberate "v2: memory story"
follow-up. This is not a decision that any of those are wrong; it's a
decision about sequencing, made explicit so it isn't lost.

## 3. `*T` — what v1 actually is

- One raw pointer form `*T`, identified only by pointee (§3.3, unchanged —
  still no `const`/mutable distinction, no address-space tag).
- Pointer arithmetic and ordering: still forbidden (§1.5, unchanged).
  Pointer `==`/`!=`, including against `nil`: still accepted (§1.5,
  unchanged).
- **Dereference is checked for null only.** A new runtime primitive,
  `pebble_rt_checked_deref` (naming TBD at implementation time, following
  the existing `pebble_rt_checked_*` family's convention), panics with a
  real Pebble source location on a null dereference — the same SAFE/RELEASE
  gating every other checked operation already has. This is genuinely new
  backend + runtime work; nothing like it exists today.
- **Casts between differently-typed pointers are explicit-only.** No
  implicit `*i32` → `*i64` conversion. This is cheap — a checker-side
  restriction, not a runtime feature — and closes the type-confusion bug
  class for free.
- **Use-after-free, double-free, and dangling-pointer creation are not
  caught.** Real UB, same as C. This is the explicit v1/v2 line: a
  generational-pointer safety net (tracking validity via the existing
  swappable `PebbleAllocator`'s `state`, which already makes this a
  surgical addition rather than a new subsystem) is the natural v2 answer,
  but it is not part of this slice.
- **Uninitialized reads are already handled, not a gap.** The default
  allocator's zero-init-on-alloc is an existing, documented ABI contract
  (`runtime/include/pebble_rt.h`) — nothing new needed here.

## 4. Pointer-to-slice construction

Array-slice syntax (`a[1:3]`) only ever accepts a fixed-size array as its
base today. `std/mem.peb`, `std/func.peb`, `std/vec.peb`, and
`std/string.peb` all currently assume `ptr[:count]` — slicing a *raw
pointer* with a runtime length — also works. It doesn't, and it shouldn't:
letting arbitrary code apply array-slice syntax to a pointer is silent,
looks identical to safe array slicing, and gives the compiler no way to
verify the length claim.

**Decision: slicing a pointer with `[:]` syntax is illegal, everywhere,
including inside `std`.** The one legitimate need it served — building a
`[]T` from a fresh allocation — gets its own narrow, clearly-marked
primitive instead:

### 4.1 `mem::new_slice[T](count usize) []T` stays ordinary Pebble code

Already present in `std/mem.peb`, already correctly generic (`T` can't be
inferred from `count` alone, so the explicit `[T]` is required — unlike the
primitive below). Its *body* changes from `return ptr[:count];` (now
illegal) to end with a call to the new `slice` primitive:

```
fn new_slice[T](count usize) []T {
    let ptr *T = mem::new(count * sizeof T) as *T;
    return slice ptr, count;
}
```

Everything else in `new_slice` — the allocator call, the `sizeof`
computation, the pointer cast — is already-ordinary code needing no new
language surface.

### 4.2 `slice` — a new keyword, restricted to the std lib package

Modeled directly on `sizeof`'s own treatment (`KwSizeof` in the lexer, a
dedicated `SizeofExpr` syntax node with no call-parens): `slice` becomes a
reserved keyword, not a function. Its grammar is `slice <expr>, <expr>` — a
comma between the two operands (not parens, not a third punctuation form),
since unlike `sizeof`'s single bounded type-operand, `slice` needs two
general value expressions and a delimiter is required to parse that
unambiguously. Precedent for comma/paren use to resolve exactly this kind
of ambiguity already exists in `sizeof (T, U)`, which parenthesizes a tuple
*type* operand for the same reason.

`T` is inferred from the first operand's pointee type — no explicit `[T]`
needed, unlike `new_slice`.

Lowering: a dedicated TIR node (two children: the pointer value, the count
value; a `Type` field for the resulting slice type), mirroring how
array-slicing is already `tir.CheckedSlice` rather than an ordinary call.
Backend lowering reuses the array-slicing pipeline's existing slice-typedef
machinery (`buildSliceTypedefs`, `sliceTypeName`, `collectSliceTypes`)
almost entirely — the only difference from array-slicing is that there's no
array to bounds-check against, so it skips the
`pebble_rt_checked_slice_start_*` call and emits the compound literal
directly: `(pebble_slice_<id>_t){ .data = <ptr>, .len = <count> }`. No new
runtime primitive needed for `slice` itself — there's nothing to check.

**Restriction:** a `slice` expression is only valid when the enclosing
module's package is `module.StandardPackage` (`internal/module/module.go`,
already a first-class recognized package identity — no new privilege
system invented). User code cannot write `slice`; only modules under `std:`
can. This is enforced at check time, not by convention — unlike the
narrower "route everything through `new_slice`" discipline inside `std`
itself, which stays a convention (nothing stops another `std` module from
calling `slice` directly, the same way Rust doesn't stop other `unsafe`
code from calling `slice::from_raw_parts` directly instead of going through
a safe wrapper).

## 5. `*void` and `any` — no change needed for v1

`*void` stays exactly as `std/libc.peb`/`std/mem.peb` already use it: a
legitimate, C-interop-style opaque pointer (`malloc`, `memcpy`, the
allocator functions). The earlier idea of an `any` type with real type
erasure and a checked downcast — most cheaply built by reusing this
backend's existing tagged-union machinery (the erasure tag *is* a union
discriminant, the downcast *is* a checked variant-match) rather than
inventing a runtime type registry — is real, well-scoped future work, but
nothing in v1 requires it. Recorded here so it isn't lost, not scoped for
this slice.

## 6. Known follow-up work, not part of this slice

- **`std/string.peb` needs a rewrite.** It currently does raw pointer
  arithmetic throughout (`self.data + self.len`, `*(self.data + self.len) =
  c`, etc.), which is illegal under §1.5 (unchanged, reaffirmed above). Once
  `*T`/`slice`/`new_slice` land, `String` should store `data` as a slice
  (backed by `mem::new_slice`) instead of a raw pointer + separate
  capacity, and every read/write goes through ordinary checked slice
  indexing instead of pointer math.
- **`std/func.peb` needs a smaller fix.** `map`/`filter`/`zip` each inline
  `(mem::new(...) as *T)[:count]` directly instead of calling
  `mem::new_slice[T]`. Once pointer-slicing is illegal, this becomes a
  correctness fix, not just a tidiness one.
- **v2, deliberately deferred, not scoped here:** generational-pointer
  UAF/double-free tracking (natural home: the existing swappable
  `PebbleAllocator`'s `state`), `any` with real type erasure, and any
  ownership/borrow-checking model.

## 7. What's already built, confirmed by direct investigation, not assumed

A live probe against the real checker (`check.Check`) found more existing
plumbing than expected:

- Dereferencing an existing pointer as a value read (`*p`) already has real
  IR-builder support — `ir_builder_value.go` dispatches `operatorDereference`
  to a `tir.Load` over a `tir.DereferencePlace`, fully wired.
- Address-of (`&x`) does not yet have a TIR construction path — no
  dedicated node kind exists for it in `tir/node.go` (only
  `DereferencePlace`); a probe (`let p *i32 = &y;`) fails with `C0619`
  ("typed-IR construction failed during buildBlocks").
- `&x` on a `let`-bound (immutable) local is correctly rejected
  (`C0606`, "place is not writable") — `&x` on a `var`-bound local gets
  past that check.
- Backend (`compiler/internal/backend/emit.go`) has zero references to
  pointers at all today.

So this is not greenfield: dereference-as-read is real, working checker
machinery already. Address-of's TIR construction is the concrete, scoped
gap to close first — not a redesign, a missing case in a switch that
already handles its sibling operations. This should be confirmed with its
own investigation dispatch before committing to a implementation-slice
sequence.

## 8. Slice 1 — address-of (`&x`) TIR construction (done)

Confirmed the hypothesis in §7 precisely, then closed it. Root cause:
`ir_builder_value.go`'s `expressionPrefix`/`Postfix`/`Binary` dispatch only
special-cased `operatorDereference`; `operatorAddress` (already correctly
classified as its own operator family, separately from bitwise-AND, and
already constrained to `PointerShape(pointee)` by the solver — both
pre-existing, confirmed by reading `operator_facts.go` directly) fell
through to `buildOperatorValue`, which cannot construct a place-valued
operand. Fixed by adding a new `tir.AddressOf` value node kind (one child,
required to be a `CategoryPlace` node — distinct from `DereferencePlace`,
which is itself a place, not a value) and a new case in `ir_builder_value.go`
that calls `buildPlaceForValue` on the *operand* (not the address-of
expression itself) and wraps the resulting place in `tir.AddressOf`. No
changes needed to `operator_facts.go`, `place_validation.go`'s existing
`C0606` immutable-place rejection, or the type-resolution side — all three
were already correct.

Verified end-to-end through `check.Check()` (not just parsing/validation)
for both a `var`-bound scalar local and a `var`-bound struct local, with a
real round-trip test proving `&y` followed by `*p` produces the correct
`AddressOf` → `Load`/`DereferencePlace` shape, not just that `Check()`
returns success. The existing `C0606` rejection of `&` on a `let`-bound
local was confirmed still correct (regression guard). Full checker suite,
full `tir` suite, and full repo suite all pass — `compiler/internal/backend`
genuinely untouched (still zero pointer support there, as intended for this
slice).

**Still not done, unchanged from §3/§6 above**: backend lowering (`Emit`
still can't compile any of this to C), the checked-null-dereference runtime
primitive, the explicit-only pointer-cast rule, and `slice`/`mem::new_slice`.
Taking the address of a tuple/array/slice element (`&t.0`, `&a[0]`) was
deliberately left out of scope and not investigated.

## 9. Slice 2 — backend lowering: locals, address-of, checked dereference (done)

Closes the remaining backend gap from §8: pointer-typed locals, `&x`, and
`*p` (read and write) now all lower to real, correct C, plus the new
`pebble_rt_checked_deref_ptr` runtime primitive (`void
*pebble_rt_checked_deref_ptr(void *ptr, PebbleSourceLoc loc)`, returns the
pointer unchanged when non-null, panics with a new `PEBBLE_PANIC_NULL_DEREFERENCE`
kind otherwise). Investigation found the null check should run
unconditionally, not gated by `PEBBLE_RT_MODE_SAFE`/`RELEASE`: contrary to
this proposal's own earlier assumption, most of the runtime (`bounds.c`,
`str.c`, `optional.c`) already runs its checks unconditionally — only
`arith.c`'s overflow paths (not division-by-zero itself) are actually
mode-gated — so `deref.c` matches the dominant existing convention, not a
new one.

**A first implementation attempt stalled twice with zero output** (an
unrelated infra issue — the model was routed through a shared subscription
that was busy elsewhere at the time, not a problem with the task). A retry
on a different model produced a real, substantial, mostly-correct
implementation (5 files, ~350 lines) — but adversarial review against the
required tests found four real, confirmed bugs before landing, each fixed
directly rather than re-dispatched, since each was small and precisely
diagnosed once found:

1. **`buildExpr`'s pointer-type width-gate bypass was incomplete.** The
   first pass only exempted `AddressOf`/`NilPointer` (freshly-constructed
   pointer values) from the entry-width gate, not a plain reference to an
   existing pointer-typed local (`SymbolValue`) or a call to a
   pointer-returning helper (`DirectCall`) — meaning the single most common
   case, reading back a pointer local by name, failed outright. Fixed by
   restructuring the bypass to switch on node kind for any pointer-typed
   node, covering all four shapes.
2. **Pointee-vs-pointer type confusion, in four separate spots**
   (`buildDereferencePlaceRead`, `buildPlaceLValue`'s `DereferencePlace`
   case, `buildHelperFunctions`'s pointer-parameter and pointer-result
   cases, `buildExpr`'s `AddressOf`/`NilPointer` cases). `pointerTypeName`'s
   contract is "takes the pointee, appends ` *`" — several call sites
   passed the *pointer* type instead (or, for `DereferencePlace`, treated
   an already-pointee-typed `node.Type` as if it still needed unwrapping),
   producing malformed C (empty return types, `()(&...)`) rather than a Go
   error, since `pointerTypeName` silently returns `""` on an unresolvable
   kind. Each site fixed to resolve the pointee correctly.
3. **`buildCallArguments` had no case for a pointer-typed parameter at
   all** — `validateHelperSignature` and `buildHelperFunctions` were
   correctly extended to accept pointer parameters, but the call-site
   argument builder wasn't, so passing a pointer to any helper failed.
4. **Struct/tuple typedef collection doesn't see a type reached only
   through a pointer.** A struct type used exclusively as a pointer
   parameter, result, or local (never directly) never got its
   `pebble_struct_<id>_t` typedef collected, producing C that referenced
   an undefined type name. Fixed by mirroring the existing "parameter/
   result is a typedef source the body walk can't see" pattern (already
   used for direct struct parameters) to also unwrap a pointer parameter/
   result/local-initializer's pointee.

Two pre-existing tests broke as an intended, correct consequence of real
pointer support landing, not regressions: `TestEmitRejectsPointerReceiverMethodCall`
(a pointer-receiver method call, correctly rejected before pointers
existed, now correctly compiles and runs — rewritten into a positive proof)
and `TestEmitRejectsStoreToNonStoragePlace` (its hand-built fixture
specifically exercised `DereferencePlace` as an unsupported Store target,
which is now supported by design — repointed at a `TuplePlace` target,
which remains genuinely unsupported).

**Confirmed, scoped, NOT fixed this slice**: `(*p).x` — reading a struct
field through a dereferenced pointer — degrades to a `tir.FieldValue` node
(field-of-a-value) rather than the expected `FieldPlace`-over-`DereferencePlace`
place chain, because the checker's place-tracking doesn't extend through a
dereference used as a field-access base in this position. Confirmed the
same gap blocks even materializing the whole dereferenced struct into a
local (`let v Point = *p;` also fails — `buildStructLocalDeclaration` only
accepts a struct literal or struct-returning call as an initializer, not a
general `Load`). This needs real new struct-rvalue backend support, not a
quick fix; the one test exercising it is `t.Skip`'d with this explanation.

Verified: `gofmt`/`go vet`/`go build` clean, the full backend suite (every
new pointer test plus the two repaired pre-existing ones), full repo suite,
the runtime smoke test compiled and run clean in both SAFE and RELEASE
(a new item 16 added covering the null-deref panic), and independently
outside the harness — manually compiled and ran the emitted C for a
pointer returned from a helper call, dereferenced, producing the real
panic report `pebble: null pointer dereference at main.peb:1:80`.

## 10. Slice 3 — explicit-only pointer casts (`x as *T`) (done)

Closes §3's remaining cast rule: casting between two distinct pointer
types (`*i32 as *void`, `*void as *i32`, etc.) is now possible, and only
explicitly — never implicitly. `classifyComposite` gained one new case
(`source.Kind() == types.Pointer && destination.Kind() == types.Pointer`
→ `compatibleExplicit`, checked after the pre-existing identity check so a
same-type pointer pair still classifies as identity, not a cast) and a
matching `coercionPointerCast` → `tir.PointerCast` (a new node kind,
mirroring `IntegerCast`'s exact shape — one value child) wired into both
of `ir_builder_value.go`'s `coercionKind -> tir.NodeKind` maps. Backend
lowering is a plain C pointer assignment/cast wherever a pointer-typed
node already has a home (the `isPointer(snapshot, node.Type)` dispatch
`buildExpr` gained in slice 2).

Two real bugs found and fixed before landing, beyond what the dispatch
delivered as-is:

1. **A missing closing brace** in `compatibility_validation.go` — a
   genuine Go syntax error that made the whole package fail to compile.
   Not a design issue, just an incomplete edit; fixed directly.
2. **The new "reject an implicit `compatibleExplicit` conversion" check
   was scoped too broadly.** The first version made
   `validateCompatibilityRecords` reject *every* `compatibleExplicit`
   pair used implicitly — not just pointers. This broke four
   pre-existing tests whose fixtures relied on integer width-widening,
   tuple coercion, and similar `compatibleExplicit` conversions being
   silently permitted in implicit positions (an existing, separate
   leniency this task was never meant to touch or tighten). Fixed by
   adding `isPointerToPointerCompatibility` and gating the new rejection
   on it specifically — every other `compatibleExplicit` pair keeps its
   pre-existing (permissive) behavior unchanged; only pointer-to-pointer
   is newly explicit-only.
3. **`pointerTypeName` didn't handle `void`, `bool`, or `char` pointees**
   — it routed every builtin pointee through `cType`, which only maps
   the fixed-width integer kinds (it's meant for width-typed locals, not
   every possible pointee). `*void` is the single most common pointee in
   this codebase (`std/libc.peb`, `std/mem.peb`) and produced malformed C
   (an empty type name, ` * pebble_local_N`) with no compile error from
   `Emit()` — caught only by actually compiling the emitted C with `cc`,
   not by any Go-level check. Fixed by giving `void`/`bool`/`char`
   explicit cases (`"void *"`, `"bool *"`, and `"int32_t *"` for `char`,
   matching the existing char-as-`int32_t` convention).
4. **The dispatch never ran `go test ./internal/tir/...`** — the new
   `PointerCast` node kind broke four tests there (`TestNodeKindInventory`'s
   exact-84-tags count, and three tests whose `validNode`/`damageNode`
   helpers have no case for the new kind) purely because the exhaustive
   test tables weren't updated, the same category of gap the address-of
   slice's own dispatch got right. Fixed by mirroring the existing
   `IntegerCast`-family shared case exactly.
5. **The required end-to-end backend round-trip test was never added at
   all** — confirmed by grepping for it, not assumed. Added directly
   (`TestEmitExplicitPointerCastRoundTripCompilesAndRuns`), which is
   what caught bug 3 above in the first place.

Verified: `gofmt`/`go vet`/`go build` clean, full checker suite
(including the pointer-cast node-shape test, the implicit-rejection
test, and the same-type-identity regression guard), full `tir` suite,
full backend suite (every pre-existing test, including the four this
slice's first version broke and then fixed), full repo suite, and
independently outside the harness — manually compiled and ran the
emitted C for a `*i32 -> *void -> *i32` round trip, exit code 42.

## 11. Slice 4 — the `slice` keyword (pointer + count → slice) (done)

Closes §4's pointer-to-slice construction design: `slice ptr, count` is a
new unconditional reserved keyword (lexer/token/tree layers), parsing to
`SliceFromExpr` (pointer expression, count expression). The checker
constrains the pointer operand to `infer.PointerShape(infer.Leaf(pointee))`,
the count to `infer.Integral(...)`, and the result to
`infer.SliceShape(infer.Leaf(pointee))`, and — mirroring the existing
`sizeof`-type-use restriction pattern exactly — rejects use outside the
standard library package (`item.Key.Package != module.StandardPackage`,
diagnostic `"slice is restricted to the standard library package"`).
Lowers to a new `tir.SliceFromRaw` node (two value children: pointer,
count) and a backend helper (`buildRawSliceConstruction`) that emits
`(%s){ .data = %s, .len = (size_t)(%s) }` directly — no bounds check, no
new runtime primitive, matching §4.2. Wired into local-decl initializers,
general expression position, and function tail-return position (the last
of which is exactly what `std/mem.peb`'s `new_slice[T]` needs for
`return slice ptr, count;`).

This slice went through the first dispatch of this whole arc that bundled
every compiler layer (lexer/parser, checker/TIR, backend, std-lib call
site) into one task — a scoping mistake, not a model-capability one. It
took four dispatch attempts (two infra stalls, one investigation-only run
that correctly identified `infer.Integral` and `uint`-not-`usize` as the
compiler's real size type but implemented nothing, then a successful
retry seeded with those findings) before landing real code, and the
landed result itself needed a real design correction:

1. **The dispatch made `slice` a "soft"/contextual keyword** — using
   lookahead on the token following `slice` (`[`, `.`, `::`, `)`, `;`) to
   decide between parsing the `SliceFromExpr` keyword form or falling
   back to an ordinary identifier, specifically to avoid breaking
   pre-existing parameters literally named `slice` in `std/mem.peb` and
   `std/vec.peb`. Rejected outright: `slice` must be unconditional,
   exactly like `sizeof` — no contextual/lookahead disambiguation for a
   keyword, ever. Fixed by reverting the heuristic (both the `parsePrimary`
   `KwSlice` case and `parseName`'s special-case admitting `KwSlice` as a
   declarable name) and instead renaming every real identifier collision:
   `std/mem.peb`'s `delete_slice[T](slice *[]T)` → `delete_slice[T](s *[]T)`,
   `std/vec.peb`'s `from_slice[T](slice []T)` → `from_slice[T](s []T)`,
   `examples/slice_minmax.peb`'s `find_min`/`find_max` parameter `slice`
   → `items` (caught by the whole-repository parser corpus scan test),
   and two checker test fixtures — one `.peb` file
   (`tests/check/ir/valid/operations_and_calls.peb`'s
   `let slice []i32 = ...;` → `let sub`, which also required regenerating
   `tests/check/ir/operations_and_calls.tir.golden`, verified via diff
   that the only changes were consistent byte-offset shifts, not
   structural ones) and one inline Go fixture
   (`TestPlaceFactsAssignmentIndexSliceAndSingleEvaluation`) with the same
   pattern, plus `TestBuildValueCheckedSlice`'s four sub-tests, which used
   `fn slice(...)` as a function name.
2. **The required end-to-end `mem::new_slice[T]` backend test was never
   added** — confirmed by grepping for it, not assumed. Writing it
   directly surfaced a separate, genuine, pre-existing bug: `usize` is
   not a recognized type spelling anywhere in this compiler (confirmed —
   `uint` is the only valid size/index builtin), yet `std/mem.peb`'s
   `new`, `stack_new`, `realloc`, `copy`, `align_up`, and the `extern`
   block's `alloca`/`memcpy` all still spelled it `usize`. Fixed directly
   (mechanical `usize` → `uint`, matching what `new_slice`'s own
   signature already correctly used). This unblocked type-checking the
   file but exposed a second, larger pre-existing gap: `std/mem.peb`'s
   allocator functions all reference `context.default_allocator`, and
   `context`/`Allocator` are not implemented as compiler builtins at all
   (confirmed — no such symbol exists anywhere in `internal/symbol` or
   `internal/check`). This is well beyond a mechanical fix — it needs a
   real context/allocator subsystem — so the full `mem::new_slice`
   end-to-end test is **not achievable yet** and was not added; it
   remains a known follow-up. The narrower, already-passing
   `TestEmitSliceFromRawCompilesAndRuns` (a direct `slice ptr, count`
   under a standard-package fixture, not routed through `mem.peb`) is
   the test actually covering this slice's backend lowering.

Also confirmed, independently, a useful design fact while investigating
the std-library restriction: `import "std:..."` always resolves through
`module.StandardPackage` regardless of the importing module's own
package (`internal/module/build.go`'s `resolveImport`), so an ordinary
non-std program can legitimately call `mem::new_slice[T](n)` even though
it can never write the `slice` keyword directly itself.

Verified: `gofmt`/`go vet`/`go build` clean, full `syntax`/`check`/`tir`
suites (including the new `TestParserSliceFromExpression` and the three
`slice_from_test.go` checker tests — std-package acceptance, non-std
rejection, non-pointer-operand rejection), full backend suite (including
`TestEmitSliceFromRawCompilesAndRuns`), full repo suite, and
independently outside the harness — manually compiled and ran the
emitted C for `slice ptr, 1` under a standard-package fixture, exit
code 42.

**Update — most of the known follow-up above is resolved, and the note
it corrects itself needed a correction.** `context`/`Allocator` turned
out to already be partially registered (`symbol.SymbolRuntimeType`,
confirmed in `internal/symbol/resolve.go`'s `installPrelude` — the
original note above claiming "no such symbol exists anywhere" was
itself wrong, written from an incomplete investigation) — the real gap
was that the backend had zero emission support for these two
runtime-injected types (no `TypeDeclaration`, so struct emission failed
outright). Closed in a later session by mapping them to the runtime's
already-existing `PebbleAllocator`/`PebbleContext` C types instead of
synthesizing new ones (commit `67f6319`). A second, separate gap
surfaced right after — `obj.name(args)` method-call syntax only ever
resolved a real declared method, never a plain function-typed field
(`Allocator.alloc`/`.realloc`/`.free`), so `allocator.alloc(...)` calls
still failed even with the backend fix in place; fixed generically, not
`Allocator`-specific (commit `33a4880`).

**Final update — the genuine end-to-end test now exists and passes.**
Getting there required three more real, separate compiler fixes
downstream of this slice, none of them this slice's own scope: the
pointer-receiver `self.field` gap (the actual remaining piece of this
arc's own motivating goal), a duplicate-source-map bug for grouped
parameter declarations (`std/mem.peb`'s own `align_up`), and — the
biggest one — the backend never lowered any generic function call at
all, which also blocked every other generic std-library function
(`Vec[T]`, `HashMap[K,V]`, etc.), not just `mem::new_slice[T]`.
`TestEmitStdMemNewSliceCompilesAndRuns`
(`internal/backend/emit_test.go`) now compiles and runs a real `import
"std:mem"; var values []i32 = mem::new_slice[i32](3); ...` fixture, exit
42. Full history of this whole downstream chain is in
`12-outstanding-implementation-work.md`.
