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
