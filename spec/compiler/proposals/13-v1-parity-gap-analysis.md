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

## Part A — statement-level and control-flow gaps (highest priority)

These are more foundational than anything in `12-outstanding-implementation-work.md`
and should be fixed before returning to that list's remaining items
(pattern matching, etc.) — you cannot write an ordinary imperative program
without them. Confirmed today via direct `emitAndRun`/`Emit` probes against
the real compiler, not assumed from v1's README.

- [ ] **`print` — the whole statement is unimplemented in the backend.**
      `tir.Print` exists as a node kind (built correctly by the checker,
      which validates the operand is `bool`/`char`/`str`/an integer/a float
      — `C0612` for anything else, e.g. a nominal/enum operand, confirmed
      still enforced) but has ZERO references anywhere in
      `internal/backend/emit.go`. `print "hello";` and `print x;` both fail
      with `entry function body block statement is a Print, want a local
      declaration...`. There is currently no way to produce output from a
      Pebble program at all. This is probably the single most damaging gap
      on this whole list — every other gap is about writing MORE
      sophisticated programs; this one blocks writing ANY observable
      program.
- [ ] **A plain `if` statement not in tail position and not inside a loop
      is rejected.** `internal/backend/emit.go`'s `buildLeadingStatement`
      (the function that builds every non-final statement in a function
      body) accepts exactly three statement kinds at its top level:
      `Initialize` (a local declaration), `Store` (a reassignment), and
      `ExpressionStatement` (a void call used as a statement). `If` is
      absent. `buildBlock` special-cases `While`/`For`/`RangeLoop` as
      leading statements (each routes through a separate, more permissive
      loop-body builder) and separately special-cases the block's FINAL
      statement to allow `Return` or a two-armed `if`/`else` there — but a
      guard-clause `if` in the middle of an ordinary function body (`if x <
      0 { return 0; } ... more code ...`), the single most common shape of
      real imperative code, is rejected outright unless wrapped in a loop.
      Confirmed: `fn helper(x i32) i32 { if x > 0 { return 1; } return 0;
      }` fails with `entry function body block statement is a If, want a
      local declaration (Initialize), a reassignment (Store), or a call to
      a void-returning function used as a statement (ExpressionStatement)`.
      A non-tail `Switch` is presumably affected identically (not
      separately verified, but it shares the same `buildLeadingStatement`
      gate).
- [ ] **Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`) is unimplemented.**
      `tir.CompoundStore` exists and is built by the checker (confirmed:
      `x += 1;` parses and type-checks) but has ZERO references in
      `emit.go`. Fails identically to `If`: `entry function body block
      statement is a CompoundStore, want a local declaration...`. You must
      write `x = x + 1;` instead.
- [ ] **Increment/decrement (`i++`, `i--`) is unimplemented** — same root
      cause as compound assignment: `i++` parses to a `CompoundStore` node,
      which hits the identical `buildLeadingStatement` gap. Confirmed via
      direct test.
- [ ] **Bitwise operators `&`, `|`, `^` are unimplemented in the backend.**
      Each parses and checks fine, producing a `tir.BinaryValue` node, but
      `buildExpr`'s accepted-node-kind list (documented explicitly in its
      own doc comment: "It accepts exactly four node kinds") has no case
      for a bitwise `BinaryValue` — only comparison-shaped `BinaryValue`s
      route through the separate `buildComparison`/`buildBoolExpr` path.
      Confirmed: `a & b`, `a | b`, `a ^ b` (each with `i32` operands) all
      fail with `entry function body expression contains a BinaryValue,
      want an integer literal, a reference to a local declared earlier in
      the body, checked +, -, *, /, % arithmetic, or a call to another
      function`.
- [ ] **Bitwise NOT (`~a`) is unimplemented.** Produces a `tir.PrefixValue`
      node; confirmed rejected with the same "want ... checked
      arithmetic..." message, naming `PrefixValue` instead of
      `BinaryValue`.
- [ ] **Bit shifts (`<<`, `>>`) are unimplemented.** These produce a
      distinct `tir.CheckedShift` node (not `CheckedArithmetic`, which only
      covers `+`/`-`/`*`/`/`/`%` — confirmed via `checkedArithmeticHelper`'s
      own switch, which has no shift cases), and `CheckedShift` has zero
      references in `emit.go`. Confirmed: `x << 2` fails with `entry
      function body expression contains a CheckedShift, want an integer
      literal, a reference to a local declared earlier in the body, checked
      +, -, *, /, % arithmetic, or a call to another function`. Already
      known and named in an existing doc comment ("the integral operators
      that build this node but are not yet lowered") — this document just
      confirms it's still true and gives a real repro.

**Why these six matter more than anything else on the outstanding-work
list:** a program that can't print, can't do an early-return guard clause,
can't increment a loop counter with `i++`, and can't do bitwise arithmetic
is not usable for ordinary code, regardless of how complete its generics or
type system are. All six share a common shape (each is a real, working TIR
node with zero backend emission) and are individually narrow, well-scoped
fixes — likely comparable in size to `IntegerCast` or Float Stage A/B from
today's earlier work, not a redesign.

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
- [x] `context`/`Allocator` system (extensively verified all session)
- [x] Explicit casts (`as`), `sizeof`

### Confirmed broken or missing

- [ ] **Variadic functions are broken at the checker, not the parser.**
      v1's syntax (`fn sum(...values []int) int`) is also v2's accepted
      grammar (`parameter_group = [ "..." ], identifier_list, type ;` in
      `03a-grammar.md`) — confirmed the parser accepts `fn sum(...values
      []i32) i32`. But calling it fails at the checker: `fn sum(...values
      []i32) i32 { return values.len as i32; } fn main() i32 { return
      sum(1, 2, 3); }` produces `T0508: exact numeric literal does not fit
      the required builtin type` on the first call-site argument, plus
      `C0604: callable declaration is invalid` for the function itself.
      Variadic call-site argument type inference is genuinely broken, not
      just unimplemented in the backend — this is upstream of `emit.go`
      entirely.
- [ ] **`iter`, the implicit loop-variable name, does not exist in v2.**
      v1: `loop 0..10 { print iter; }` — omitting the `: name` clause
      defaults the loop variable to `iter`. v2: confirmed `loop 0..5 { sum
      = sum + iter; }` fails at the checker with `N0001: undefined name
      "iter"` — v2 apparently requires the `: name` clause always. Either
      this is a deliberate design change (in which case it should be
      documented as one, not left silent) or a real gap; not distinguished
      by this audit.
- [ ] **Function-typed locals don't work in the entry-body backend
      grammar**, though function-typed STRUCT FIELDS do (confirmed
      throughout `std/hmap.peb`/`std/set.peb`, e.g. `hash_fn fn (K) u64;`).
      `type BinaryOp = fn(i32, i32) i32; fn main() i32 { var op BinaryOp =
      add; return op(1, 2); }` fails at `buildScalarInitializeCore`: `want
      an integer type, bool, char, or float`. A first-class function VALUE
      stored in a local (as opposed to invoked directly, or stored as a
      struct field) isn't supported by the restricted statement grammar —
      likely the same "narrow grammar built incrementally, never
      generalized" shape as the width-restriction and float-support gaps
      fixed earlier today.
- [~] **Unions are only partially implemented — declarable, not usable.**
      Both `union` (untagged) and `union enum` (tagged) parse as type
      declarations. A tagged union can apparently be constructed
      (`Data.{ Int = 42 }` doesn't error at construction) but field access
      fails: `var d Data = Data.{ Int = 42 }; return d.Int;` produces
      `C0605: member operation is invalid`. Untagged unions fail
      identically. This matches `open-language-decisions.md` §1.3's
      already-recorded finding for untagged unions specifically
      ("Untagged unions cannot be constructed, read, or written... `C0615`")
      but that document does not mention tagged unions having the same
      field-read problem — worth checking whether `C0605` here is the same
      underlying gap as `open-language-decisions.md`'s `C0615`, or a
      second, distinct one; not resolved by this audit.
      - **Concrete evidence this blocks real, already-written code:**
        `std/result.peb` is written entirely around tagged-union pattern
        matching — `type Result[T, E] = union enum { Ok T; Err E; ...
        fn is_ok(self Result[T, E]) bool { switch self { case Ok: return
        true; case Err: return false; } } ... }`. This file was NEVER part
        of the std-library audit's "checks clean" list this session (`vec`,
        `string`, `hmap`, `set`, `mem`, `hash`, `io`, `libc`, `func` all
        were; `result` was not) — it almost certainly does not compile
        today, blocked on exactly this gap.
- [ ] **`switch` on a tagged union, with the `case Ok: return self.Ok;`
      style destructuring-by-field-access, is what v1 calls pattern
      matching** — and what `12-outstanding-implementation-work.md`
      already tracks as "held for a design conversation." This audit's
      contribution: it is not merely a syntax gap. It requires (1) unions
      being readable/writable at all (the item directly above), which is
      itself broken today, and (2) `switch` accepting a union-typed subject
      with per-variant cases, which has not been verified independently of
      (1) — you cannot test the switch mechanism in isolation while field
      access on the switched-into variant is broken. **Recommendation:**
      pattern matching should not be scoped as "add switch destructuring
      syntax" alone; it should be scoped as "make unions readable/writable,
      then add the switch mechanism on top," since v1's own design (per
      `README.md`) inseparably combines the two (`switch self { case Ok:
      return self.Ok; ... }` reads a field of the matched variant inline).
- [~] **Enum-to-integer casts: checker allows it, backend doesn't emit it.**
      `open-language-decisions.md` §1.4 records this as an OPEN LANGUAGE
      QUESTION ("06a still calls this open... 06b's matrix says
      forbidden"). Verified today the checker actually ACCEPTS `Color.green
      as i32` and produces a `tir.EnumToInteger` node — so whichever side
      of that internal disagreement won, the answer is "allowed," at least
      partially. But `EnumToInteger` has zero references in `emit.go`,
      so it fails at the backend: `entry function body expression contains
      a EnumToInteger, want an integer literal, ...`. This is the same
      "checker allows it, backend was never wired up" shape as several
      Part A items, not a design question anymore — the design question in
      `open-language-decisions.md` §1.4 appears to have been silently
      resolved (in favor of "allowed") sometime since that document was
      written, without the document being updated.
- [ ] **Printing an enum is still rejected exactly as designed** (not a
      bug): `print Color.red;` fails at the checker with `C0612: print
      operand is not printable`, matching `open-language-decisions.md`
      §3.11 exactly. Confirmed still accurate. This is explicitly listed
      there as "blocks nothing today" and a real future-feature gap
      (needs a variant-name table emitted into the binary) — not
      something to fix as part of Part A's `print` work, which only needs
      to handle the types the checker already allows.

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
- Relative-path imports (`import "utils/math";`) — only `import
  "std:..."` has been exercised this session
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

- `OptionalIntegerToEnum`, `CheckedIntegerToEnum` — the reverse direction
  of the enum-to-integer gap above (integer→enum coercion); likely the
  same "checker allows it, backend never wired up" shape.
- `HoistedFunctionValue`, `GenericFunctionValue` — a function referenced as
  a first-class VALUE (not called immediately). Likely related to, or the
  same root cause as, the function-typed-local gap confirmed above.
- `OptionalInject`, `TupleCoerce` — implicit coercion nodes (wrapping a
  plain value into `some x`, or coercing a tuple's element types at an
  assignment boundary). Unclear whether these are genuinely unreachable
  given how the checker structures these coercions elsewhere, or a real
  gap; needs direct investigation, not a grep-based conclusion.
- `TypeUse` — almost certainly a compile-time-only bookkeeping node with no
  runtime representation; low suspicion of being a real gap, listed for
  completeness only.

---

## Part C — tooling and driver gaps (not language gaps, but block real use)

- [ ] **`pebc` (the actual CLI) cannot resolve `import "std:..."` at all.**
      `cmd/pebc/main.go` calls `module.Build` with no `StandardRoot` set —
      confirmed via direct reading of the file. Every std-importing test
      this entire session went through internal Go test harnesses that set
      `StandardRoot` manually; the real, user-facing CLI has never been
      exercised against a std-importing program and would fail to resolve
      the import. **Decided fix direction (this conversation):** use Go's
      `//go:embed` to bake `std/`'s `.peb` sources directly into the `pebc`
      binary at build time, so `module.Build` resolves `"std:..."` from an
      embedded filesystem — no install-time copy step, no path-resolution
      drift between the binary and whatever stdlib happens to be on disk.
      Not yet implemented.
- [ ] **v1's CLI surface is far larger than v2's.** v1 (`README.md`
      "Compiler Options"): `-o`, `-c`, `--check-only`, `--generate-only`,
      `--keep-c`/`--no-keep-c`, `--compiler`, `-l`/`-L`/`-I` (linking),
      `--header`/`--sys-header`/`--cc-flags`, `--std-path`, `--freestanding`,
      `--entry-point`, `--no-main`, `--shared`, `--debug`/`--release-small`/
      `--release-safe`/`--release`, `-v`/`--verbose`, `-w`/`--warnings`. v2's
      `pebc` (`cmd/pebc/main.go`, confirmed via direct reading): exactly one
      flag, `-o`. Every other v1 mode — check-only, freestanding, custom
      entry point, no-main/object-file-only, shared library, release
      variants, linking flags — has no v2 equivalent yet. This matches
      `open-language-decisions.md` §2.8 ("Driver/CLI modes... has not
      decided which of the C prototype's CLI modes it preserves") almost
      exactly, except that document is about which modes v2 SHOULD support
      (a design question); this entry is about the fact that NONE of them
      exist yet at all (an implementation gap) — both are true
      simultaneously.

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
