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

### `result.peb` — thin, and has a real compiler-level caveat
Has: `is_ok`, `unwrap_or`, `map`, `set_error`, plus `result_ok`/
`result_err` constructors.

Missing:
- `is_err`, `unwrap` (abort on `Err`), `unwrap_err`, `map_err`,
  `and_then` (chain fallible steps), `ok()` (→ `?T`, dropping the
  error).

**Compiler-level caveat, not a stdlib gap** — see the dedicated section
below. New combinators added here are only directly *callable* the
same constrained way the existing ones are (wrapped in public,
non-generic functions inside `result.peb` itself), not callable
directly on a `Result` value from an external module.

### `func.peb` — solid generic toolkit
Has: `map`/`filter`/`reduce`/`find`/`any`/`all`/`zip` over plain
slices.

Missing (nice-to-have, not blockers):
- `filter_map` (filter+map in one pass), `flat_map`, `take`/`skip`.

Also subject to the same cross-module generic-instantiation limitation
as `result.peb` (see below) when a caller passes a *named* function
value from outside the module.

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

This came up because `result.peb`'s own comments flag it. Worth
recording precisely since it's easy to misread as "a Result bug."

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

**What specifically triggers it for `Result`** — from
`spec/compiler/proposals/19-stdlib-production-hardening.md:255-280`
(Slice 5's findings), two distinct, separately-confirmed limitations,
both about generic union enums crossing module boundaries:

1. **A generic union enum type cannot be instantiated from outside its
   declaring module.** `Result[i32, str]` type-checks fine written
   inside the same file `Result` is declared in; referencing that same
   instantiation from an importing module fails — the checker can't
   resolve the cross-module generic instantiation, leaves an inference
   variable unconstrained, reports T0510.
2. **Separate and narrower**: even where the type name itself resolves,
   a union enum's *methods* are not callable from outside the declaring
   module. Confirmed empirically: `.is_ok()` works called from inside
   `io.peb` itself, fails with `T0507`/`T0510` together when called
   from an external importer.

**Is this Result-specific? No.** It's a general gap in the checker's
cross-module generic-instantiation machinery, and Result is just the
stdlib's flagship generic tagged union, so it surfaces the limitation
most visibly. The same slice log notes `func.peb`'s higher-order
generics (passing a *named* function value into `map`/`filter`/etc.
from outside `func.peb`) hit "the exact class of limitation" too — so
it's broader than tagged unions specifically.

**The stdlib's current workaround**, used consistently everywhere this
has come up (`io.peb` Slice 5, `func.peb`/`result.peb` Slice 7): wrap
the generic logic in public, non-generic, primitive-in/primitive-out
functions declared *inside* the same module as the generic type or
generic function, so external callers go through those wrappers instead
of touching the generic type/function directly. This is why any new
`Result` combinators should follow the same pattern — extendable, but
only reachable from outside `result.peb` via a wrapper, not via direct
method calls on an externally-held `Result` value.

**Actually fixing the underlying limitation is compiler work** (checker
cross-module generic resolution), not something reachable from `.peb`
source — out of scope for stdlib changes, would need its own
initiative if ever tackled.
