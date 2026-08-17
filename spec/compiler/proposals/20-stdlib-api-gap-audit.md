# 20 — stdlib API gap audit

**Status:** findings only, not a proposal. Captured 2026-08-17 as a
reference to come back to before/during `pjson` work. Nothing here is
scheduled; this is an inventory, not a slice log.

## How this was produced

A read-through of every file under `compiler/std/` (not just a grep of
function names — struct fields, doc comments, and existing helper
patterns were checked too, so items below aren't duplicates of things
that already exist under a different name or as a public field).

## Per-module findings

### `vec.peb` — solid, a few real gaps
Has: `push`/`pop`/`insert`/`remove`/`swap_remove`/`get`/`get_by_ref`/
`resize`/`truncate`/`retain`/`contains`/`find`/`find_all`/`sort`/
`partition`/`reverse`/`as_slice`/`from_slice`/`push_slice`.

Missing:
- `join` — `Vec[str]` → `str` with a separator. Likely wanted for
  `pjson` output.
- `last()` / `first()` — trivial accessors, absent.
- `swap(i, j)` — swap two elements in place (distinct from
  `swap_remove`, which removes one).
- `dedup()` — drop consecutive duplicates (pairs naturally with
  `sort`).
- `binary_search()` — same pairing.
- `clone()` — deep-copy a `Vec`.

Note: `as_slice()` already bridges to `func::map/filter/reduce`, so
slice-level transforms aren't missing — just not `Vec`-native methods.

### `hmap.peb` — the real blocker
Has: `insert`/`get`/`get_by_ref`/`contains`/`remove`/`clear`, `.len`/
`.cap` as public fields.

Missing:
- **No iteration at all.** No `keys()`, `values()`, `entries()`,
  nothing. The `entries` field is technically public and walkable, but
  callers would have to manually skip `Tombstone`/`Empty` slots
  themselves — leaks an internal implementation detail. This is the one
  real blocker for `pjson`: serializing an object means walking its
  keys.
- `get_or_insert(key, default)` — insert-if-absent ergonomics.
- `clone()`.

### `set.peb` — same shape as hmap, same gap
Has: `insert`/`contains`/`remove`/`clear`, identical tombstone-entry
design to `hmap`.

Missing:
- **Iteration**, same situation as `hmap`.
- Set algebra: `union`, `intersection`, `difference`, `is_subset`.
- `clone()`.

### `string.peb` — well-documented, missing tokenization
Has: `push_str`/`push_char`/`push_byte`/`insert`/`remove`/`substr`/
`find`/`starts_with`/`ends_with`/`char_at` (with a deliberate,
well-explained byte-vs-decoded-char distinction — this is intentional
design, not a gap, don't add O(1) scalar indexing).

Missing:
- `split(sep)` — needed for basically any parser, including `pjson`'s
  tokenizer.
- `trim` / `trim_start` / `trim_end` — whitespace stripping.
- `replace(from, to)`.
- `contains(needle)` — trivial wrapper over `find`, doesn't exist as
  its own method.
- `to_upper` / `to_lower` — at least ASCII-only would help.
- `parse_int` / `parse_float` — string→number conversion (though
  `libc::atof`/`atol` exist as raw extern bindings that could be
  wrapped).
- `join(pieces, sep)` — the `Vec[str]` counterpart to `split`.

### `hash.peb` — already complete
Full width coverage (`i8`..`u64`, `bool`, `ptr`, `char`, `bytes`, `str`,
plus `hash_combine`). Nothing obviously missing.

### `result.peb` — thin, no longer has the compiler caveat this doc originally claimed
Has: `is_ok`, `unwrap_or`, `map`, `set_error`, plus `result_ok`/
`result_err` constructors.

Missing:
- `is_err`, `unwrap` (abort on `Err`), `unwrap_err`, `map_err`,
  `and_then` (chain fallible steps), `ok()` (→ `?T`, dropping the
  error).

**Correction (2026-08-17): the cross-module limitation described below
in the T0510 appendix has been directly re-verified and no longer
reproduces.** `Result[T, E]` instantiation, method calls (including the
generic `.map()`), and passing a named function value into `.map()`
all now type-check and compile/run correctly from an external module —
confirmed with a real, standalone repro compiled and executed
end-to-end, not just re-reading the old note. New combinators added
here are safely callable directly from `pjson` or any other importer,
no wrapper-function workaround required. (An earlier version of this
doc, and a comment in `result.peb` itself, both said otherwise — both
were accurate when the hardening slices were written, and have since
gone stale as later compiler work fixed the underlying issue without
either doc being updated. `result.peb`'s comment has been corrected to
match.)

### `func.peb` — solid generic toolkit, no real cross-module caveat either
Has: `map`/`filter`/`reduce`/`find`/`any`/`all`/`zip` over plain
slices.

Missing (nice-to-have, not blockers):
- `filter_map` (filter+map in one pass), `flat_map`, `take`/`skip`.

**Correction (2026-08-17): this section previously claimed a real,
reproducing cross-module limitation here too — that claim was wrong.**
An earlier version of this doc (and a comment in `func.peb` itself)
said passing a named function value into a generic higher-order
function from outside `func.peb` fails with `T0505`/`T0510`, "confirmed
directly." That confirmation was flawed: the repro used to demonstrate
it had an unrelated bug (a fixed-size array literal passed where a
slice parameter was expected), and THAT mismatch — not the module
boundary — produced the T0505/T0510 pair. A dispatched investigation
(escalated to a stronger model after two other attempts stalled trying
to "fix" a bug that turned out not to exist) caught this by refusing to
trust the stated repro and re-deriving it carefully instead, then
independently re-verified myself: with the array/slice mismatch
corrected, both a minimal two-module case and a real call into
`std:func::filter` from an external module — including against the
actual `std:func.peb` source — type-check, compile, and run correctly.
See `internal/backend/cross_module_function_value_test.go` for the
regression coverage this added. `func.peb`'s own comment has been
corrected to match.

### `math.peb` — more complete than it first looked
Has `PI`/`E` constants, full trig/log/exp set, `abs`/`min`/`max`/
`clamp` all generic already.

Missing:
- `sign`, `gcd`/`lcm` (integer), `is_nan`/`is_inf`.
- No RNG anywhere in the stdlib — would be a new module, not an
  addition here, and only worth it if something upcoming actually needs
  randomness.

Known separate bug (already logged in Slice 7 of proposal 19, not
re-logging here): `math::abs[f64]` was hitting a `LiteralFits`
constraint failure — this was fixed as a standalone follow-up after
Slice 7 closed (see `project_stdlib_hardening_complete` — commit
referenced there). Confirmed fixed, not an open item.

### `mem.peb` — looks solid
`alloca`/`memcpy`/`new`/`new_typed`/`new_slice`/`realloc`/`delete`/
`copy`/`align_up` — no obvious gap spotted without deeper
allocator-design context.

### `mem/arena.peb` — internal, not a convenience-API candidate
This is the arena allocator's actual implementation (slab/offset/header
arithmetic) — not a public surface to extend the way `vec`/`hmap`/
`set`/`string` are.

### `io.peb` — already comprehensive
Full libc-mirroring layer plus a higher-level `open_checked`/
`read_all_into`/`write_all` layer. Nothing obviously missing.

### `libc.peb` — thin binding layer, one small gap
Has `atof`/`atol` but no `atoi`. Trivial one-line addition if wanted
for int-from-string without going through `atol`.

## Priority read, if `pjson` is the next real driver

`hmap`/`set` iteration and `string::split`/`trim` are the two gaps that
will actually **block** `pjson` (serializing/deserializing an object
needs to walk keys; parsing needs tokenization). Everything else above
is a convenience gap, not a blocker.

---

## Appendix: the T0510 / cross-module generics limitation, precisely

This came up because `result.peb`'s and `func.peb`'s own comments used
to both flag it. Worth recording precisely, including the correction
history, since this doc itself got it wrong once (see item 2 below)
before being corrected by an independent, more careful re-investigation
— a useful cautionary example, not just a status log.

**What T0510 actually is**: a single, generic checker diagnostic —
`internal/infer/diagnostics.go:20` (`CodeUnresolved = "T0510"`), raised
from exactly one place, `internal/infer/solve.go:529`
(`"inference variable has no unique semantic type"`). It fires whenever
the type-inference solver finishes and some internal inference variable
never got pinned to a concrete type. It is **not specific to Result, or
even to generics** — other T0510 sites exist in the checker for
unrelated causes (`nil`/`none` coercion, negated-literal inference,
anonymous struct field covering). It's the solver's generic
"gave up" signal, not a named bug.

**Historical background** — from
`spec/compiler/proposals/19-stdlib-production-hardening.md:255-280`
(Slice 5's findings, written before the compiler work referenced
below), two distinct limitations were both confirmed at the time, both
about generic union enums crossing module boundaries: (1) a generic
union enum type couldn't be instantiated from outside its declaring
module, and (2) even where the type name resolved, its methods weren't
callable from outside the declaring module.

**Status as of 2026-08-17, directly re-verified, not just re-read**:

1. **Generic union enum instantiation + methods across modules: FIXED.**
   Built a standalone repro — a two-type-parameter union enum
   (`Pair[T, E]`, the same shape as `Result[T, E]`) declared in one
   module, instantiated from a second module via a constructor
   function, with a method (`is_ok`-equivalent) AND a generic method
   (`map[U]`) called from the external module, including passing a
   *named function value* into the generic method. Compiled and ran
   correctly end to end (real `cc` build, real execution, correct exit
   code). Repeated the same test directly against the real `std:result`
   module (`result_ok`, `.is_ok()`, `.map(named_fn)`, `.unwrap_or()`,
   all called from outside `result.peb`, zero wrapper functions) — same
   result: type-checks, compiles, runs, correct output. Whatever fixed
   this happened after Slice 5/7 were written (most plausibly the
   `F5-01`/`F5-02` generic-tagged-union slices and other Phase 3
   tagged-union work done later in this initiative) — neither the
   `19-stdlib-production-hardening.md` slice log nor `result.peb`'s own
   comment were updated when it was fixed. `result.peb`'s comment has
   now been corrected to reflect this.
2. **`func.peb`'s higher-order-generic case: also fine — this doc
   originally said otherwise, and that was wrong.** An earlier pass of
   this audit re-ran a repro (a named function value passed to
   `func::filter` from an external module) and concluded it still
   reproduced `T0505`/`T0510`, "confirmed directly." That confirmation
   itself was flawed: the repro passed a fixed-size array literal (which
   defaults to `[N]T`) where `filter[T](items []T, ...)` expects a
   slice — an unrelated array-vs-slice mismatch that happens to also
   produce `T0505`/`T0510`, and was misattributed to the module
   boundary instead of investigated further. This was caught by a
   dispatched orc investigation given the fix task — two primary-tier
   attempts stalled trying to reproduce and fix a bug that, it turned
   out, doesn't exist; escalating to a stronger model got a careful
   re-derivation instead of a speculative fix, and it correctly refused
   to change the checker for a repro it couldn't actually make fail
   once the unrelated mismatch was corrected. Independently re-verified
   directly afterward: with the array/slice mismatch fixed, a minimal
   two-module case AND a real call into `std:func::filter` — against
   the actual `std:func.peb` source, with a named function declared in
   a genuinely separate helper module — both type-check, compile, and
   run correctly. See
   `compiler/internal/backend/cross_module_function_value_test.go` for
   the regression coverage this added. `func.peb`'s own comment has
   been corrected to match.

**Is this Result-specific? No, and it never was** — Result was just the
type that most visibly surfaced the ONE real limitation here (the
generic union enum case), which has since been fixed. The `func.peb`
case never had a real limitation at all; it was a documentation error
compounded by an under-verified repro, not a second checker bug.

**Practical takeaway**: extend both `Result` and `func.peb` freely —
new combinators on either are directly callable from `pjson` or any
external importer, no wrapper-function workaround needed for either
module. The wrapper functions that remain in both files are kept
purely as regression-style compiled-and-run coverage, not because
external code needs to go through them.

**Lesson for future audits of this kind**: a diagnostic reproducing is
not the same as the diagnostic reproducing for the claimed reason —
always isolate variables in a repro (here: slice vs. array literal
typing) before attributing a failure to the mechanism under
investigation, and be willing to have that checked independently rather
than trusting a first pass at face value, including your own.
