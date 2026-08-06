# 13 — v1 parity gap analysis

**Purpose.** The rewrite's goal is parity with the original (v1, `src/*.c`)
compiler. This document is a systematic accounting of where v2 currently
falls short of that goal, compiled by (1) reading v1's own `README.md`
feature inventory end to end, (2) verifying each claim against v2 directly
— compiling and running real code, not reading source and guessing — and
(3) reviewing `open-language-decisions.md` for design questions that were
never closed. Every entry below states exactly how it was checked. Nothing
here is inferred from a doc comment alone unless explicitly marked as such.

This is a reading aid and a punch list, not a design proposal. Update it as
items close; don't let it drift out of sync the way
`open-language-decisions.md` did (see the note at the end of this doc).

**This document is the issue tracker, per direct instruction (2026-08-05).**
Rules: new findings go here as soon as they're discovered. No work starts
on anything not already tracked here. When an item is fixed and verified,
its entry is DELETED from this document, not marked `[x]` — the tracker
should only ever show what's currently outstanding, never grow into an
archive. The permanent record of what was done and why lives in commit
messages and git history, not here. `12-outstanding-implementation-work.md`
is now historical/archived — its old items are folded into this document
where still relevant (see "Deferred, not scoped" below); do not add new
entries to it.

Status legend: `[x]` confirmed working today, `[ ]` confirmed broken/missing
today, `[~]` partially working (works in some positions, not others).

---

## Part A — statement-level and control-flow gaps (DONE)

Every item closed and pushed: `print` (`b44691e`), mid-body `if`/`switch`
(`b5be90d`), compound assignment/`++`/`--` including the double-eval fix
(`d035ff5`, `de32223`), bitwise AND/OR/XOR/NOT (`6b3d818`), bit shifts
(`7df11b7`). A plain function body can now do everything an ordinary
imperative program needs.

---

## New findings (add here as discovered, remove once fixed)

- [ ] **A bool-returning DIRECT call used straight in a condition
      position fails — confirmed unrelated to function types, zero
      function types involved.** `fn id(b bool) bool { return b; }
      fn main() int { if id(true) { return 1; } else { return 2; } }`
      fails with "entry function body expression contains a DirectCall,
      want a bool literal, a reference to a bool local declared earlier
      in the body, a comparison, a && / || combination, or a !
      negation" — `buildBoolExpr` has no `DirectCall` case at all.
      Found while verifying a function-types slice-3 dispatch's test
      suite (one of 16 real scenarios it wrote); confirmed genuinely
      pre-existing and unrelated by reproducing with the function type
      entirely removed. Not yet scoped in detail.

---

## Part B — v1 feature-parity checklist

Verified today by writing and compiling real Pebble source against v2,
cross-referenced against v1's `README.md` (the only place v1's full surface
syntax is documented in one place). Items already covered in Part A are not
repeated here.

### Confirmed working

- [x] Expression-bodied functions: `fn square(x i32) i32 => x * x;`
- [x] Open-ended slice syntax: `a[:3]`, `a[2:]` (not just `a[1:3]`)
- [x] Inclusive range loops: `loop 0..=5 : i { ... }`
- [x] Plain type aliases: `type MyInt = i32;`
- [x] Integer `switch` statements (in tail position — not separately
      verified in a leading/mid-body position, which Part A's `If` finding
      suggests is also likely broken there)
- [x] Generic functions, generic structs, generic methods (extensively
      verified all session)
- [x] Relative-path imports — `import "./utils/math";` (the leading
      `./` is required; a bare `"utils/math"` needs a configured
      `SearchRoots` entry `pebc` doesn't set up, an unrelated,
      correct-as-designed rejection, not a bug)
- [x] `context`/`Allocator` system (extensively verified all session)
- [x] Explicit casts (`as`), `sizeof`

### Confirmed broken or missing

- **`iter`, the implicit loop-variable name — DECIDED, not a gap.** v1:
      `loop 0..10 { print iter; }` defaults the loop variable to `iter`
      when the `: name` clause is omitted. v2 requires `: name` always
      (`loop 0..5 { sum = sum + iter; }` fails at the checker with
      `N0001: undefined name "iter"`). Direct decision (2026-08-05): keep
      v2's current behavior — explicit naming stays required, no implicit
      `iter` default. Not tracked as a gap.
- [ ] **All three slices done (2ab27d6, 0b6ed32, 0643cca) — function
      types now work end to end for locals, values, indirect calls,
      struct fields, parameters, and results.** Independently
      re-verified `std/hmap.peb` via `backend.Emit` (not just
      `check.Check`) as promised — found one real, narrow, precise
      residual gap in the process: `validateFunctionTypeSignature`'s
      supported RESULT types are entry-width/bool/char/void only, not
      `uint`/`u64` — asymmetric with parameters, where `uint` IS
      already supported. `hmap.peb`'s `hash_fn fn (K) u64;` field fails
      with exactly this: "function type fn(int) u64 has result type
      u64, want int, bool, char, or void." `std/set.peb` almost
      certainly hits the same gap (uses the same `hash_fn` field
      shape via `hmap`) — not separately re-tested, expect the
      identical failure. Not yet fixed; `functionTypeResultCType`
      (`internal/backend/emit.go`) needs a `uint`/`u64` case mirroring
      the existing parameter-side `isUint` support exactly.
      CORRECTED, much bigger than originally scoped: function types have
      NO real backend representation anywhere, not just in locals.
      Original entry claimed struct fields already worked ("confirmed
      throughout `std/hmap.peb`/`std/set.peb`") — that claim was wrong,
      based only on the CHECKER accepting those files
      (`check.Check`), never on actually running them through
      `backend.Emit`. Directly verified today: `types.Function` has
      exactly ONE reference in the entire `internal/backend/emit.go`
      (the whole backend package — confirmed only one non-test `.go`
      file there), and it's purely for a human-readable error message in
      `describeType`, not real C emission. A function-typed struct FIELD
      fails identically to a function-typed local: `type Table = struct
      { op fn(i32, i32) i32; }; ... var t Table = Table.{ op = add };`
      fails with `struct type pebble_struct_N_t: field type fn(i32,
      i32) i32 is not supported, want i32 or bool`. There is no C
      function-pointer typedef mechanism, no function-value emission,
      nothing — this needs a real, standalone feature (comparable in
      size to the float-expression work, not a one-case fix), threading
      function-pointer C types through struct fields, locals, and likely
      parameters/results.

      **Sobering correction this forces**: `std/hmap.peb`/`std/set.peb`
      being marked "checks clean, 0 diagnostics" earlier this session
      meant exactly that — clean at the CHECKER level — and should NOT
      have been read as "compiles and runs." Both files have a
      function-typed field (`hash_fn fn (K) u64;`) and almost certainly
      do not compile to real C yet. This needs independent re-verification
      via `backend.Emit` (not just `check.Check`) once function types are
      actually supported, before trusting either file end-to-end.
- **Tagged-union field access on the matched variant (`case Ok: return
      self.Ok;`) is pattern matching — still deferred, confirmed by
      spec text, not merely a sequencing choice.** Previously framed as
      "mechanical, fix now, pattern matching is a separate later
      conversation" — that framing undersold how entangled the two
      actually are. `06b-validation-and-typed-ir.md` line 850 states
      switch narrowing explicitly: "A tagged-union case narrows only its
      dominated case region" — meaning `self.Ok` is only ever meant to
      be legal INSIDE a `case Ok:` body that has narrowed `self` to that
      variant, not as unconditional field access on any union value
      (confirmed the `member operation is invalid` / `C0605` root cause:
      `internal/check/member_validation.go`'s `memberField` case only
      matches members of `symbol.SymbolKind` `SymbolField`, but union
      members are registered as `SymbolVariant`
      (`internal/symbol/resolve.go:242`) — so naively making that check
      also accept `SymbolVariant` would make `d.Int` legal on ANY union
      value unconditionally, which is a different, weaker, almost
      certainly wrong design than the flow-sensitive narrowing the spec
      actually describes). Implementing this for real is flow-sensitive
      type narrowing inside a dominated case region — a genuine,
      nontrivial feature, not a validator tweak. Stays deferred per the
      already-decided sequencing (after enums/unions are otherwise
      working and everything else on this tracker is done).
- **Untagged `union` construction/read/write has no accepted design at
      all — separate from tagged unions, explicitly "Undecided" in
      `open-language-decisions.md` §1.3, not merely unimplemented.**
      Confirmed still accurate; not touched by the construction-syntax
      fix above (which only affects union ENUM / tagged-union
      destinations — untagged `union` has no discriminant at all, so
      the whole "which variant" question that the `.{ Name = value }`
      routing fix resolves for tagged unions doesn't even apply). Stays
      out of scope until the safety-model design question is settled.
- **Printing an enum is still rejected exactly as designed** (not a bug):
      `print Color.red;` fails at the checker with `C0612: print operand
      is not printable`, matching `open-language-decisions.md` §3.11
      exactly. Confirmed still accurate. **Decision (2026-08-05):
      deliberately last, not tracked as near-term work.** Debug-object
      printing is planned to become "amazing" as its own dedicated effort
      — not just enum variant names, a real facility — but that only makes
      sense once the compiler itself is fully correct first (this whole
      tracker). Do not scope or start this until everything else here is
      done. Not something to fix as part of Part A's plain `print`
      implementation, which only needs to handle the types the checker
      already allows today (bool/char/str/integers/floats).

### Not yet individually re-verified (flagged, not confirmed)

The following v1 features are not yet checked against v2 with a real
compile-and-run test. Each should be verified the same way as everything
above before being trusted either as "works" or "broken":

- Anonymous/untagged struct literal in a union context, and untagged union
  field mutation specifically (`other.int_val = 32;` — a write, not just a
  read)
- `extern "libm.so" { ... }` — library-named extern blocks (only bare
  `extern { ... }` and single `extern fn` have been used/verified this
  session)
- `extern { type FILE; }` — extern opaque types
- Nested generic instantiations at the type level (`Vec[HashMap[str,
  Result[T, E]]]`) — generics have been verified extensively but not
  specifically nested three levels deep with a union in the mix
- `Result[T,E]`-shaped generic tagged-union methods with their own type
  parameters (`fn map[U](self Result[T, E], f fn(T) U) Result[U, E]`) —
  blocked transitively by the union gap above, not separately testable yet

### Backend TIR node-kind audit — other zero-coverage node kinds

Cross-referencing all 81 `tir.NodeKind` values against `internal/backend/
emit.go` (the entire backend package — confirmed there is only this one
non-test `.go` file in `internal/backend/`) found these additional node
kinds with zero references, beyond everything already covered in Part A/B
above. None of these has been individually verified with a real compile
attempt yet — listed here as a starting point for the next audit pass, not
as confirmed gaps:

- `HoistedFunctionValue`, `GenericFunctionValue` — a function referenced as
  a first-class VALUE (not called immediately). Likely related to, or the
  same root cause as, the function-typed-local gap confirmed above.
- `TupleCoerce` — implicit tuple element-type coercion at an assignment
  boundary. Unclear whether this is genuinely unreachable given how the
  checker structures these coercions elsewhere, or a real gap; needs
  direct investigation, not a grep-based conclusion. (`OptionalInject`
  is done — implicit optional injection now works for local
  declarations and function results, this session.)
- `TypeUse` — almost certainly a compile-time-only bookkeeping node with no
  runtime representation; low suspicion of being a real gap, listed for
  completeness only.

---

## Part C — tooling and driver gaps (not language gaps, but block real use)

- **v2's CLI will deliberately stay simpler than v1's — not a gap to
      close by re-implementing everything.** v1 (`README.md` "Compiler
      Options"): `-o`, `-c`, `--check-only`, `--generate-only`,
      `--keep-c`/`--no-keep-c`, `--compiler`, `-l`/`-L`/`-I` (linking),
      `--header`/`--sys-header`/`--cc-flags`, `--std-path`, `--freestanding`,
      `--entry-point`, `--no-main`, `--shared`, `--debug`/`--release-small`/
      `--release-safe`/`--release`, `-v`/`--verbose`, `-w`/`--warnings`. v2's
      `pebc` (`cmd/pebc/main.go`): exactly one flag, `-o`. **Decision
      (2026-08-05): v2's CLI stays simpler by design — a lot of v1's
      surface gets cut, not ported.** This entry stays open only for the
      `go:embed`-stdlib work already scoped on the active list below
      (making `-std:...` imports resolve without a flag at all); do not
      treat the rest of v1's flag list as a to-do. If/when specific
      modes (freestanding, release variants, etc.) are wanted, that's a
      separate, deliberate decision to make later — not default parity
      work.

---

## Part D — open language decisions: currency check

`open-language-decisions.md` is a real, thorough, already-existing document
— but its own status line ("06a nearly complete... 06b has not started")
places it very early in this project's history, well before 06b, phase 7
(generics), phase 10 (backend), and `11-raw-pointers-and-unsafe-ops.md` were
even started, let alone finished. Spot-checking three of its "still open"
items today found it is now itself out of date in a way that matters:

- **§1.1 (tuple positional access, `pair.0`) — RESOLVED, contradicting the
  document.** The document calls this "not an edge case, the plain case"
  of a total failure. Confirmed today: `let pair (i32, i32) = (1, 42);
  return pair.1;` compiles and runs correctly, returning 42. This
  proposal's blocking status should be marked resolved.
- **§1.4 (enum-to-integer conversion) — partially resolved, see Part B
  above.** The checker now accepts it; only the backend lowering is
  missing (a normal, narrow implementation gap, not an open design
  question anymore).
- **§3.11 (printing an enum) — confirmed still accurate**, exactly as
  written.

**Recommendation:** before relying on ANY "still open" entry in
`open-language-decisions.md` for planning purposes, re-verify it directly
against the current compiler the way this document does — don't trust the
doc's own claim at face value. A full re-audit of that document (not done
here, given time — only 3 of its ~19 entries were spot-checked) is
itself a worthwhile follow-up task, separate from this one.

---

## Deferred, not scoped

Carried over from `12-outstanding-implementation-work.md` (now archived).
Not urgent, no timeline — tracked here only so they aren't rediscovered
from scratch later.

- `(*p).x` on a struct pointer / materializing a whole dereferenced struct
  into a local (`let v Point = *p;`) — the checker's place-tracking doesn't
  extend a `DereferencePlace` through a field-access base in this position.
  A `t.Skip`'d test in `internal/backend/emit_test.go` records the exact
  root cause inline. Needs new struct-rvalue backend support.
- Generational-pointer UAF/double-free tracking, `any` with real type
  erasure, ownership/borrow-checking. Not scoped, intentionally out of the
  v1 raw-pointers slice (`11-raw-pointers-and-unsafe-ops.md` §6, "v2,
  deliberately deferred").

---

## Priority recommendation

In order, before returning to `12-outstanding-implementation-work.md`'s
remaining items (which are already correctly ordered relative to EACH
OTHER, just not relative to what's in this document):

1. **Part A, all six items** — `print`, mid-body `if`, compound assignment,
   increment/decrement, bitwise `& | ^ ~`, bit shifts. These block writing
   any ordinary program and are each individually narrow.
2. **`go:embed` for the stdlib + wiring `pebc`'s CLI to use it** — makes
   the compiler actually usable end-to-end as a real tool, not just via
   internal test harnesses.
3. **Unions: construction/read/write**, since real std-library code
   (`std/result.peb`) already depends on it and it's the prerequisite for
   pattern matching, not a parallel, independent feature.
4. Pattern matching (`switch` on a tagged union with per-variant field
   access) — now correctly scoped as "on top of #3," not standalone.
5. Everything else in `12-outstanding-implementation-work.md` and this
   document's remaining checklist items, roughly in the order each was
   already being worked.
