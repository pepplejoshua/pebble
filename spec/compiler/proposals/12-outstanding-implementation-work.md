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

## CRITICAL — pointer-receiver `self.field` access has never worked

**Confirmed via direct testing, independent of every other fix in this
doc** (identical failure with and without the slice/array/str `.len` fix
below — not a regression from it, a pre-existing gap): a method with a
pointer receiver (`fn get(self *P) uint => self.cap;`, exactly the
declared shape every method in `std/vec.peb`/`std/string.peb`/
`std/hmap.peb`/`std/set.peb` uses) fails reading its own field with
`T0507 field receiver is not a nominal type` (or, after the structural-
field fix, `T0507 type has no structural field named cap`). Confirmed
this is **not** limited to methods — an ordinary plain function taking a
pointer parameter (`fn f(p *P) uint { return p.cap; }`) fails identically.
The checker's field-resolution constraint (`hasField`) never auto-derefs
a pointer receiver; something else in this codebase must have been doing
that for every previously-confirmed-working pointer-receiver test
(10.47's own summary explicitly lists "a pointer-receiver method cleanly
rejected" as one of its four passing tests — meaning pointer receivers
were deliberately unsupported as of that point), and it appears nothing
has closed that gap since, despite the entire raw-pointers-and-unsafe-ops
arc (11) being explicitly motivated by exactly this: 11's own §1
Motivation says outright, "raw pointers... are the one prerequisite
`std/string.peb` actually needs — every mutating method on `String`
takes `self *String`."

**This means the raw-pointers arc's own stated goal isn't actually
delivered yet, despite all 4 of its slices being done and committed.**
This is very likely the single highest-leverage remaining item on this
entire list — every pointer-receiver method in the standard library is
blocked by it, independent of `.len`, `usize`, pointer arithmetic, or
anything else already fixed or still open. Not yet scoped into a dispatch
brief; needs its own investigation into exactly where a fix belongs (the
member-facts/hasField/structuralField layer most likely, possibly needing
an explicit pointer-deref step on the receiver term before field
resolution runs, mirroring how `receiverNominal` — used for method
*selection* — already handles a pointer receiver by dereffing it, per
`internal/infer/instantiate.go`).

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
- [x] `std/mem.peb`'s `new_typed[T]() *T { return new(sizeof T); }` was
      missing an explicit `*void` → `*T` cast — fixed directly
      (`return new(sizeof T) as *T;`), confirmed the resulting error is
      gone from a real check of the file.
- [x] `.len`/`.data` field access on a slice, fixed-array, or `str` VALUE
      (not through a pointer — see the CRITICAL section above for that
      separate, bigger gap) from Pebble source. New
      `constraintStructuralField` resolves slice `.len`/`.data`, array
      `.len` as a genuine compile-time constant (an `IntegerLiteral`, not
      a runtime field — array length is already carried on the type
      shape), and `str.len`; falls back to the unchanged, existing
      `hasField` for real nominal structs (confirmed no regression via a
      dedicated test: a struct declaring its own fields literally named
      `len`/`data` still resolves via nominal lookup). Committed `b4f0eb9`,
      independently re-verified (gofmt/vet/build/full suite all clean).
  - `[N]T` fixed arrays' `.len` and `str`'s `.len` were also confirmed
    affected by the same root cause and are fixed by the same commit.
  - **Not yet confirmed either way** (my own test syntax may have been
    wrong, not a confirmed checker gap): plain `union` field access failed
    with a different, unrelated-looking error (`C0605 member operation is
    invalid`, not `T0507`) — a distinct code path, needs its own
    investigation with correct syntax before concluding anything. Tagged
    union (`union enum`) pattern-matching wasn't tested validly (parser
    errors from guessed-wrong match/switch syntax) — needs the real
    syntax looked up before testing.
  - **Confirmed unaffected**: tuple `.0`/`.1` component access already
    works (`memberTuple` has its own dedicated path in `member_facts.go`).
  - **Newly discovered while verifying this fix, unrelated, tracked
    separately**: primitive integer casts (`as i32`, `as uint`, etc.,
    distinct from pointer casts `as *T` which already work) are not
    supported ANYWHERE in the C backend — confirmed via direct probe, and
    via zero existing passing tests anywhere in `emit_test.go` exercising
    one. Fails even as a plain helper function's tail-return expression,
    not just inside the entry. This is why the dispatch for this fix
    couldn't add its required executable backend round-trip test (the
    natural fixture needed a `uint` → `i32` cast to produce an entry-
    compatible return type) — correctly left out rather than worked
    around. Not yet scoped into its own dispatch brief.
  - This fix unblocks `.len`/`.data` for VALUE receivers; the CRITICAL
    pointer-receiver gap above still blocks the same std-library files'
    actual methods (which all use pointer receivers), independent of this
    fix.
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
