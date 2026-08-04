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
- [x] Known follow-up (documented in 11 §6): `std/func.peb`'s
      `map`/`filter`/`zip` inline `(mem::new(...) as *T)[:count]` instead of
      calling `mem::new_slice[T]` — fixed, see the std-library audit below
      (`4d1a8e5`).
- [ ] Known, deliberately deferred, not scoped: `(*p).x` on a struct
      pointer / materializing a whole dereferenced struct into a local
      (`let v Point = *p;`) — the checker's place-tracking doesn't extend a
      `DereferencePlace` through a field-access base in this position.
      Confirmed via a `t.Skip`'d test in `internal/backend/emit_test.go`
      with the exact root cause recorded inline. Needs new struct-rvalue
      backend support; not urgent, just tracked here so it isn't
      rediscovered from scratch later.
- [x] **Fixed.** `&self.data[i]` — address-of a slice-index place, reached
      through a pointer receiver's field — previously failed with
      `C0619`. Fixed (`3d2bc9d`), three distinct root causes in sequence:
      (1) the deferred member-vs-generic bracket disambiguation path
      retained a speculative generic `TypeUse` for the index operand
      under an inactive branch, which `buildTypeUses` processed anyway
      instead of filtering like solved-root resolution already does; (2)
      the same deferred path retained an index record but no matching
      expression record, which place-building required; (3) a genuine,
      separate bug — place construction marked ANY field literally named
      `data` as the structural slice-accessor sentinel before ever trying
      real nominal member lookup, meaning a real user-declared `data []T`
      field (exactly `Vec[T]`'s own field name) would have silently
      resolved to the wrong thing. Backend also gained real support for
      slice-typed struct fields throughout emission. Verified with a real
      aliasing proof: mutate through a returned `&self.data[index]`,
      read the mutation back through the original struct — exit 9.
      Independently re-verified — full suite green. **This was the actual
      blocker for the `vec.peb`/`string.peb` redesign below**, which can
      now proceed.
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

## Pointer-receiver `self.field` access (was CRITICAL, now fixed)

- [x] **Fixed.** A method with a pointer receiver (`fn get(self *P) uint
      => self.cap;`, exactly the declared shape every method in
      `std/vec.peb`/`std/string.peb`/`std/hmap.peb`/`std/set.peb` uses),
      and even an ordinary plain function taking a pointer parameter,
      never worked reading (or writing) a field through the pointer —
      confirmed this was the actual reason the raw-pointers-and-unsafe-
      ops arc's (11) own stated motivation ("every mutating method on
      `String` takes `self *String`") wasn't really delivered despite all
      4 of that arc's slices being committed. Root cause: `hasField` and
      `structuralField` never dereferenced a pointer receiver before
      classifying it, unlike `receiverNominal` (used for method
      *selection*, which already worked). Fixed by adding the identical
      one-level pointer peel `receiverNominal` already had, to both
      functions' known-type and shape-based branches, plus the same peel
      in two downstream consumers (`member_validation.go`'s diagnostic
      pass, `ir_builder_place.go`'s `memberSymbol` lookup). The backend
      now emits `->` instead of `.` for a pointer-typed field base, and
      `buildStoreCore` now accepts `FieldPlace` as a write target (which
      also fixed plain non-pointer struct field assignment,
      `point.x = 5;`, previously rejected outright for the same reason).
      Committed `d9baea8`, independently re-verified — full suite green,
      and confirmed directly against a real `vec.peb`-shaped fixture (a
      struct with a pointer-receiver method conditionally reading AND
      writing its own fields) that the exact pattern used throughout
      `std/` now type-checks.

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
- [x] `.len`/`.data` field access on a slice, fixed-array, or `str` value
      from Pebble source (`b4f0eb9`), and through a pointer to one, e.g.
      `s *[]T` (`d9baea8`, same fix as the pointer-receiver item above).
      New `constraintStructuralField` resolves slice `.len`/`.data`,
      array `.len` as a genuine compile-time constant (an
      `IntegerLiteral`, not a runtime field — array length is already
      carried on the type shape), and `str.len`; falls back to the
      unchanged, existing `hasField` for real nominal structs (confirmed
      no regression via a dedicated test: a struct declaring its own
      fields literally named `len`/`data` still resolves via nominal
      lookup). Both commits independently re-verified (gofmt/vet/build/
      full suite all clean).
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
  - Combined with the pointer-receiver fix above, `.len`/`.data` now
    resolves through both value and pointer receivers.
- [x] `std/libc.peb` — `usize` → `uint` sweep (`8913cd4`). Confirmed via
      direct checking: this file now type-checks cleanly, end to end,
      standalone.
- [x] `std/hash.peb`, `std/io.peb`, `std/hmap.peb`, `std/set.peb`,
      `std/vec.peb`, `std/string.peb` — `usize`/`isize` → `uint`/`int`
      swept mechanically (`8913cd4`; the naive `sed -i '' 's/\busize\b/.../'`
      silently did nothing on macOS's BSD `sed`, which doesn't support
      `\b` — redone with a portable bracket-class pattern and verified
      function names like `hash_usize`/`hash_isize` were correctly left
      alone, only real type positions replaced). This does not mean these
      files fully check yet — pointer arithmetic (`std/vec.peb`,
      `std/string.peb`, `std/hmap.peb`, `std/set.peb`) and an invalid
      `.is_some` field accessor (`std/hmap.peb`, `std/set.peb`) remain,
      see below.
- [x] `std/func.peb` — replaced all three inlined pointer-slicing sites
      (`map`/`filter`/`zip`) with `mem::new_slice[T]`/`mem::new_slice[Ret]`/
      `mem::new_slice[(T, U)]` calls (`4d1a8e5`). Confirmed the specific
      `T0507 type is not structurally sliceable` errors are gone from a
      real check of the file; one shared, separate, newly-found gap
      remains (next item).
- [x] `std/mem.peb`'s `new_slice[T]` had a second missing explicit cast
      (`allocator.alloc` returns `*void`, assigned directly to a `*T`
      local) — fixed (`4d1a8e5`).
- [x] `p.field = nil;` (any field place — pointer, slice, or plain
      nominal struct alike) previously failed with `T0510`. Fixed
      (`ca1ee43`): `expectedType` gained a `ShapeLiteral` flag, set only
      for `nil`/`none`, and `applyExpected` now unifies exactly those
      shape-literal terms with the assignment destination so later
      constraint solving can ground them — narrow and additive, doesn't
      touch ordinary (non-literal) field assignment. Also closed a
      previously-unexercised backend gap surfaced while adding the
      required round-trip test: pointer-typed struct fields had no
      support anywhere in struct construction, field reads, field
      stores, or C-type declaration (no prior test in this repo ever
      constructed/read/wrote a struct field of pointer type through the
      backend). Independently re-verified — full suite green.
  - **Residual, smaller, separate finding, now with a clean minimal
    repro**: the file is down to a single different error (`C0619 typed-IR
    construction failed during buildDeclarations`, not the `T0510` this
    fix targeted). Confirmed NOT reproduced by `delete_slice` alone, nor
    several individual function-pair combinations tried standalone — but
    IS reproduced by the ENTIRE file's declarations coexisting, and
    critically, **reproduces from a real cross-module import alone, with
    zero usage**: `import "std:mem"; fn main() i32 { return 0; }` fails
    checking with this exact error the moment `std:mem` is imported at
    all (confirmed directly — not a call-site interaction, not specific
    to `mem::new_slice`, purely a property of building typed IR for
    `mem.peb`'s own full declaration set). This is the actual current
    blocker for a genuine `mem::new_slice[T]` end-to-end backend test
    (referenced as still-missing in `11-raw-pointers-and-unsafe-ops.md`'s
    now-corrected Slice 4 entry). Not yet bisected to a specific pair of
    declarations within the full set, not yet scoped into a dispatch
    brief.
- [ ] `std/vec.peb` — real redesign needed beyond the `usize` sweep:
      `data` needs to be backed by a `mem::new_slice`d slice
      (capacity-sized) and indexed via `data[i]` instead of pointer
      arithmetic (`*(self.data + i)`, confirmed still present and still
      illegal). Not mechanical — a structural change.
- [ ] `std/string.peb` — same redesign as `vec.peb` for its buffer (this is
      also 11 §6's tracked follow-up — one item, not two).
- [ ] `std/hmap.peb`, `std/set.peb` — confirmed via direct checking (not
      previously known) to need the same pointer-arithmetic-to-slice-
      indexing redesign as `vec.peb`/`string.peb` (e.g.
      `(new_entries + i).state = .Empty;`), **plus** a separate, genuine
      bug: both call `.is_some` as a field on an Optional value
      (`tombstone_index.is_some`) — confirmed via checking every other
      `?T`-consuming pattern in `std/` that `.is_some` is not a real
      accessor anywhere else in this language.

## OPEN DESIGN QUESTION — how is an Optional's "is it set" queried at all?

Not a bug — genuinely unclear, needs a real answer before `hmap.peb`/
`set.peb` can be fixed (see `.is_some` above). Confirmed, via direct
testing against the real checker, that NONE of the following work:

- `.is_some` as a field/method (`T0507`/`C0605`, not a real accessor)
- `o == none` / `o != none` (`C0603 operator operands or result have
  invalid types` — equality against `none` is not supported)

Also confirmed via grep: no existing test anywhere in this repo
(`internal/check/*_test.go`, `internal/backend/*_test.go`) exercises
`== none`/`!= none`/`.is_some`/any other Optional-query form. The only
two operations this language supports on a `?T` today, confirmed
working, are: constructing one (`some x` / `none`), and force-unwrapping
one (`!`, e.g. `stack_val!.0`, used throughout `std/vec.peb`). There is
no confirmed way to check "is this Optional set" without unwrapping it
unconditionally (which panics on `none`).

This needs a real design decision — likely one of: (a) support `==`/`!=`
against `none` as a genuine boolean query (cheapest, matches how pointer
`nil` comparison already works per `11-raw-pointers-and-unsafe-ops.md`
§3 / `OPEN-DECISIONS.md`'s resolved pointer-equality note — an Optional
being "none" is conceptually the same shape of question), (b) add an
`is_some`/`is_none` accessor as a real language feature, or (c) something
else entirely (pattern-binding `if`, etc.). Whichever is chosen, it's a
real, if small, checker feature — not a one-line bug fix — and
`hmap.peb`/`set.peb` can't be finished until it exists.

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
