# 12 — outstanding implementation work tracker

This is a living checklist, not a design proposal. Its job is to make sure
nothing discovered mid-investigation gets lost by being chased and
forgotten in favor of the next thing. Each item links back to the proposal
doc that owns its detailed design/history, if one exists. Update this file
whenever an item's status changes; don't let it drift out of sync with
reality.

Status legend: `[x]` done and committed, `[~]` in progress / partially
done, `[ ]` not started.

## 11 — raw pointers, `slice`, and the unsafe-operations boundary

See `11-raw-pointers-and-unsafe-ops.md` for full design/history.

- [x] Slice 1 — address-of (`&x`) TIR construction
- [x] Slice 2 — backend lowering (locals, address-of, checked dereference)
- [x] Slice 3 — explicit-only pointer casts (`x as *T`)
- [x] Slice 4 — `slice` keyword (pointer + count → slice), implemented as an
      unconditional reserved keyword after the initial contextual-keyword
      design was rejected and corrected
- [ ] **§11 doc entry for the `slice` keyword slice is still owed** — the
      spec doc's own record stops at Slice 3; Slice 4's real history
      (the contextual-keyword flaw, every renamed identifier, the golden-file
      regen) needs writing up before it's considered closed on paper, even
      though the code itself is done and committed.
- [ ] Known follow-up (documented in 11 §6): `std/string.peb` needs a
      rewrite — it does raw pointer arithmetic throughout, illegal under
      §1.5. Overlaps with the std-library audit below.
- [ ] Known follow-up (documented in 11 §6): `std/func.peb`'s
      `map`/`filter`/`zip` inline `(mem::new(...) as *T)[:count]` instead of
      calling `mem::new_slice[T]` — illegal pointer-slicing. Overlaps with
      the std-library audit below.
- [ ] Known, deliberately deferred, not scoped: `(*p).x` on a struct
      pointer / materializing a whole dereferenced struct into a local
      (`let v Point = *p;`) — the checker's place-tracking doesn't extend a
      `DereferencePlace` through a field-access base in this position.
      Confirmed via a `t.Skip`'d test in `internal/backend/emit_test.go`
      with the exact root cause recorded inline. Needs new struct-rvalue
      backend support; not urgent, just tracked here so it isn't
      rediscovered from scratch later.
- [ ] Known, deliberately deferred (11 §6, "v2, deliberately deferred"):
      generational-pointer UAF/double-free tracking, `any` with real type
      erasure, ownership/borrow-checking. Not scoped, intentionally out of
      the v1 raw-pointers slice.

**Correction to an earlier note in 11's own body**: the doc's "Known
follow-up" at the very end claims `context`/`Allocator` are "not
implemented as compiler builtins at all." That was true when written but
is now stale — see the next section; they were already partially
registered (`symbol.SymbolRuntimeType`) and the real gap was backend
emission, not missing symbol registration. Fix this note when writing the
§11 Slice 4 entry above, so a future reader isn't misled by it.

## Runtime types / allocator arc (grew out of 11, not originally its own section)

- [x] Backend emission for `Allocator`/`Context` runtime types — maps them
      to the existing `PebbleAllocator`/`PebbleContext` C types instead of
      trying to synthesize a struct declaration. Committed `67f6319`.
- [x] Method-call resolution for function-typed fields (`obj.field(args)`
      where `field` is a plain function-typed field, not a declared
      method) — general fix, not `Allocator`-specific. Committed `33a4880`.
      This removed the need for the `(a.alloc)(...)` parenthesization
      workaround; `std/mem.peb` uses the plain unparenthesized form.
- [x] `sizeof T` inside a generic function body incorrectly rejected at the
      template level (`C0615`) even when every concrete instantiation is
      valid. `validateSizeof` now defers `TypeParameter` at the template
      pass; a new `validateSpecializedSizeof` re-checks each concrete
      instantiation's substituted target for real, preserving `C0615` for
      genuinely invalid concrete types. Also fixed a related IR-snapshot
      timing bug (specialization substitution was interning composite
      `sizeof` targets after the immutable type snapshot had already been
      taken). Committed `f7f92d7`, independently re-verified (gofmt/vet/
      build/full suite all clean, no `std/` files touched).

## Standard-library correctness audit (user request: "inspect all lib files
for now wrong/illegal behaviour and correct them")

Two real, checker-enforced illegal patterns are present throughout `std/`:
`usize` (not a real type anywhere — `uint` is), and raw pointer arithmetic
(`self.data + i`, forbidden under 11 §1.5, actively rejected by the
checker, confirmed via direct probe). A third, pointer-slicing (`ptr[:n]`),
is illegal per 11 §4's decision.

- [x] `std/mem.peb` — `usize` → `uint` swept across all functions
      (`new`, `stack_new`, `realloc`, `copy`, `align_up`, the `extern`
      block). Colliding `slice`-named identifiers already renamed as part
      of the Slice 4 keyword work.
- [ ] **Newly discovered, not yet scoped**: `std/mem.peb`'s
      `delete_slice[T](s *[]T) void { delete(s.data); s.data = nil; }` —
      `s.data` (a field access through a pointer-to-slice) fails
      type-checking with `T0507 field receiver is not a nominal type`.
      Confirmed via direct probe against the real file (not a synthetic
      repro) after the allocator and method-call fixes landed. Not yet
      root-caused; needs its own investigation before a fix brief can be
      written.
- [ ] **Newly discovered, not yet scoped**: `std/mem.peb`'s `new_typed[T]()
      *T { return new(sizeof T); }` is missing an explicit `*void` → `*T`
      cast — `new` returns `*void`, and this compiler has no implicit
      pointer conversions (11 §3: "casts between differently-typed
      pointers are explicit-only"). Surfaced by the `sizeof T` fix's own
      commit message while confirming `mem.peb` checks cleanly past the
      `sizeof`/allocator layers; mechanical fix (`as *T`), not yet applied.
- [ ] `std/libc.peb` — `usize` → `uint` sweep (mechanical; extern
      declarations only, no pointer arithmetic present).
- [ ] `std/hash.peb` — `usize` → `uint` sweep (mechanical).
- [ ] `std/io.peb` — `usize` → `uint` sweep (mechanical).
- [ ] `std/hmap.peb` — `usize` → `uint` sweep, plus check for pointer
      arithmetic beyond the hash-index computation already scanned (not
      fully audited yet).
- [ ] `std/set.peb` — same as `hmap.peb`.
- [ ] `std/vec.peb` — `usize` → `uint` sweep, plus a real redesign: `data`
      needs to be backed by a `mem::new_slice`d slice (capacity-sized) and
      indexed via `data[i]` instead of pointer arithmetic
      (`*(self.data + i)`). Not mechanical — a structural change.
- [ ] `std/string.peb` — same redesign as `vec.peb` for its buffer (this is
      also 11 §6's tracked follow-up above — one item, not two).
- [ ] `std/func.peb` — replace inlined pointer-slicing with
      `mem::new_slice[T]` calls (also 11 §6's tracked follow-up above).

## Backend generic function-call lowering

Not part of any existing proposal doc yet — probably belongs as a new
slice in `10-c-backend-implementation-plan.md` once scoped, since 07
(generics, checker/typed-IR side) is already fully closed per that doc's
own baseline note.

- [ ] **Not started, only investigated.** The backend today cleanly
      rejects any call carrying type arguments ("generics are not
      supported yet" — `internal/backend/emit.go`, two call sites). This
      blocks every generic std-library function (`Vec[T]`, `HashMap[K,V]`,
      `mem::new_slice[T]`, etc.) from ever compiling end-to-end, regardless
      of the std-library audit above landing.
  - Confirmed via direct probe: the checker's specialization machinery
    (phase 07) already builds a genuinely distinct, fully-monomorphized
    `FunctionDecl` per concrete instantiation — a generic `fn f[T]`
    called as `f[i32](...)` and `f[i64](...)` produced *three* separate
    `FunctionDecl` entries (one generic template plus two specializations)
    with three distinct `FunctionID`s, all sharing one `Symbol`.
  - Two concrete gaps identified, neither investigated deep enough yet to
    write a dispatch brief:
    1. No accessor found yet correlating a call site's `TypeArgs` to the
       right specialized `FunctionID` (`Unit.Instantiations()` returned
       empty in the probe — either it's not wired into this build path, or
       the real correlation mechanism is something else not yet found).
    2. Emitted C helper functions are named `pebble_fn_<symbol>` — since
       multiple specializations share one `Symbol`, this scheme collides
       across specializations and needs a per-specialization disambiguator
       (likely `FunctionID`-based).
  - This is confirmed to be on the project's actual roadmap, not
    speculative: `07-generics.md`'s "Specialization" section commits to
    full monomorphization as the permanent design, and
    `10-c-backend-implementation-plan.md`'s own baseline note treats phase
    07 as an already-closed prerequisite feeding into phase 10.
