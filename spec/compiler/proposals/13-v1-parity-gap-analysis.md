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

- [x] Cross-module generic specialization type lookups against a
      frozen snapshot fixed (`f7c6451`) — the root cause of `std/
      hmap.peb`'s opaque `C0619` failure on every real method (found
      while trying a full real `hmap.peb` consumer end-to-end after
      the generic-method-calls fix). `rehash`/`get_by_ref`/`get`/
      `insert`/`maybe_grow`/`contains`/`remove`/`clear` all now build
      cleanly — confirmed via a real Go-harness `Emit` compile.
- [x] Recursive and mutually-recursive function calls fixed
      (`d9a8da3`) — every reachable helper now gets a C forward
      declaration before any definition, so `insert -> maybe_grow ->
      rehash -> insert` (and direct self-recursion) compile and run
      correctly. The one cycle shape still rejected is a cycle passing
      through the entry function itself (`main`'s fixed C name has no
      forward-declared prototype) — not a real-world blocker, `main`
      calling back into itself recursively isn't a normal shape.
- [x] Optional payload types widened to any fixed-width integer
      (`d737242`) — `std/hmap.peb`'s `var tombstone_index ?uint =
      none;` (`insert`) now works, including `some`/`none`
      construction, `.has_value`, and force-unwrap (a new runtime
      `pebble_rt_checked_unwrap_u64` was needed, since only i32/i64/
      bool existed). A full `new`+`insert`+`get` consumer progresses
      past this to a different gap instead (below).
- [x] Pointer-payload optionals (`?*T`) fixed (`64faff4`) —
      `get_by_ref`'s `?*V` (and `get`'s force-unwrap/re-wrap of it) now
      work: typedef, `some`/`none` construction, `.has_value`, and
      force-unwrap (a new runtime `pebble_rt_checked_unwrap_ptr`, plus
      two backend sites the earlier `uint`-payload fix never needed —
      a pointer-typed local declared directly from a force-unwrap, and
      the general-expression-position unwrap).
- [x] `Allocator`-typed struct fields fixed (`f90be50`) —
      `HashMap`'s `backing Allocator` field now typedefs, constructs,
      and reads correctly (`orderAggregateTypes` now skips compiler
      -builtin runtime types the same way `collectStructTypesWalk`
      already did; `structFieldCType`/`buildStructBraceList`/
      `buildStructFieldRead` gained runtime-type cases mirroring the
      existing runtime-typed-local pattern).
- [x] Ordinary struct fields widened to any fixed-width integer
      (`485f57f`) — `HashMap`'s own `len uint`/`cap uint` fields now
      typedef, construct, and read correctly.
- [x] Generic struct field substitution now selects the correct
      canonical declaration key (`88e0149`) — **corrected diagnosis
      from the prior entry**: the real bug wasn't in
      `orderAggregateTypes`'s own walk; `Entry[int,int]` was already
      being correctly discovered and resolved. The type snapshot holds
      MULTIPLE all-type-parameter Nominal keys for one generic struct
      when it's referenced from other generic contexts (every generic
      method naming `Entry[K, V]` with its own K/V interns a distinct
      such key) — `structTypeParameters`' first-match scan grabbed
      whichever key was interned earliest, often a method's own
      inherited parameters rather than `Entry`'s actual declaration
      parameters, so the substitution map used mismatched symbol IDs
      and silently left fields unsubstituted. Now selects the
      candidate whose argument symbols actually match the
      declaration's own field-referenced parameters.
- [x] Enum-typed struct fields implemented (`36dbf11`) —
      construction, plain reads, assignment (both directly and through
      a pointer), and comparisons of a plain-enum-typed struct field
      all work now (`structFieldCType`, `buildStructBraceList`,
      `buildStructFieldRead`, `buildStoreCore`, and `buildEnumValue`
      each gained an enum case, reusing the existing general
      `buildEnumValue` machinery rather than inventing new enum
      -value semantics).
- [x] **[generator]** Struct/tuple/optional-element slices fixed
      (`ffa50d1`) — mirrors `arrayElementCType`'s existing support
      (arrays already handled this); needed a genuine new mechanism
      beyond that precedent, though: a real C incomplete-type forward
      declaration for an aggregate used as a slice element, since a
      slice's own typedef is emitted before the aggregate typedef
      block it needs to point into.
- [x] **[generator]** `uint` expression grammar completed — the full
      `std/hmap.peb` consumer (`new` + `insert` + `get`, including a
      real `maybe_grow`-triggered `rehash`) now compiles under `-Wall
      -Wextra -Werror` and **runs correctly end-to-end**
      (`TestEmitStdHmapInsertGetFullConsumer`, asserted via a real
      `cc` compile + execution, not just `Emit` succeeding). This
      closes the entire `std/hmap.peb` arc: every position that can
      hold a `uint` value now routes through `buildUintExpr` — local
      declarations/reassignments, `return`, range-loop bounds,
      slice/array/str indices (read, lvalue, and bound), struct-field
      reads, optional payloads and their force-unwrap, and compound
      assignment (`self.len += 1;`) — and `buildUintExpr` itself
      gained `SizeofType` (now resolving ANY type's real C type via
      the new `sizeofCTypeName`, not just the three builtin widths —
      this fixed a genuine memory-corruption bug: `sizeof Entry[K, V]`
      previously fell through to `sizeof(uint64_t)`, under-allocating
      `rehash`'s new table), `IntegerCast`, `Load`, and
      `CheckedOptionalUnwrap` cases. Also needed along the way: a
      bare `return;` in a void helper, a slice-typed field's
      whole-value read/reassignment, an optional-typed local's
      whole-value reassignment (previously a clean rejection — see
      `TestEmitOptionalLocalStoreCompilesAndRuns`, which replaces the
      old `TestEmitRejectsOptionalLocalStore`), and a pointer-to-struct
      receiver for a function-typed field read (`self.hash_fn(key)`
      through a `*HashMap` method parameter).

---

**Note on labeling (2026-08-06):** entries above are now tagged
`[checker]` or `[generator]` where the distinction was confirmed.
`[generator]` (backend/`emit.go` C-emission code) has been the large
majority of this session's fixes — the checker already accepted these
programs; a specific C-emission code path just hadn't been taught the
shape yet. `[checker]` fixes (generic struct method call
specialization, the cross-module frozen-snapshot bug) were rarer but
more serious: actual logic bugs in code that's supposed to already be
authoritative, not missing coverage. Older, unlabeled entries above
this note predate the convention; assume `[generator]` unless the
entry's own text says otherwise (nearly all of them touch
`internal/backend/emit.go`).

---

## Part A.1 — `std/*.peb` and `examples/*.peb` sweep (parallel orc diagnostic fleet, 2026-08-07)

With `std/hmap.peb` closed, a fleet of parallel orc workers ran the
built `pebc` binary directly against the remaining 9 `examples/*.peb`
files (no synthetic wrapper needed — they already have their own
`main`). Findings triaged into: real compiler gaps (below), and stale
example files fixed directly (not compiler gaps — `usize`/`isize`/
`float` were deliberately removed per
`spec/compiler/05-types-and-inference.md:74`, and `README.md` had the
same staleness, also fixed).

## Part A.2 — remaining `std/*.peb` files sweep in isolation (2026-08-07)

With the examples sweep (Part A.1) mostly done, swept the 8 `std/*.peb`
files not yet individually check-verified this session (`func`,
`hash`, `libc`, `math`, `mem`, `set`, `string`, `vec` — `hmap`, `io`,
`result` were already deep-dived earlier). Each checked in isolation
via a minimal `import "std:<module>"; fn main() int { return 0; }`
fixture. Results: `func`, `hash`, `libc`, `mem`, `string`, `vec` all
**PASS** (check clean, emit, compile, and run). `set` fails only on
the already-tracked `C0618` unreachable-statement warning (see the
`std/hmap.peb`/`std/set.peb` note under the closure-literal entry
above — `pebc`'s CLI treats any diagnostic, even a warning, as fatal;
not a real check failure). `math` found a genuine new gap:

- [x] **CLOSED (`b4410cb`) — float literals in a top-level `let`
      constant binding.** `std/math.peb:18-19`: `let PI = 3.141593;` /
      `let E = 2.718282;` previously failed with `C0616: binding
      initializer is invalid`. Root cause confirmed exactly as
      suspected: `constant.go`'s `literal()` had no `FloatLiteral` case
      (floats weren't constant-evaluable). Fixed by adding a
      `constantFloat` kind + `Float float64` field to `constantValue`
      and a `case syntax.FloatLiteral:` parsing via `strconv.ParseFloat`
      (fails cleanly with `C0614` on parse error). `math.peb`'s two
      constants now bind. This fix also unmasked a separate, new
      `C0621` generic-requirement-propagation gap in `clamp[T]`'s
      `min`/`max` calls — logged separately below, not yet closed.

- [x] **PARTIALLY CLOSED (`69742fb`)** — inline slice construction as a
      call argument (`f(a[1:3])`) now works whenever the call itself is
      in a leading-statement position (a bare call statement, or any
      local's declaration initializer — scalar, slice, struct, tuple,
      array, optional, str, or pointer), verified compiled and run, not
      just emitted. `examples/prime_sieve.peb` itself is left using its
      existing `let`-bound workaround (untouched, still correct). The
      narrow boundary that's STILL a clean rejection, by design (would
      need the full `buildDirectCall`-signature-changing refactor to
      lift): a slice construction nested inside a pure expression
      position — `print f(a[1:3])`, `return f(a[1:3])`, `f(g(a[1:3]))`.
- [x] CLOSED — `examples/extern_mem_funcs.peb`'s "concrete specialization
      not found" was the extern-fn direct-call gap, fully resolved by
      the extern-call implementation (`6e75c3c`) and the example itself
      closed (`ae4b7c0`) — see that entry above. (Stale duplicate entry
      removed; this was never deleted when that work landed.)
- [x] `buildPlaceLValue`'s `FieldPlace` case missing structural-field
      (`.len`/`.data`) handling — fixed (`a31cf99`). `examples/
      slice_minmax.peb` now fully closed (`0fc2480`): compiles and runs
      correctly end-to-end, verified via the new `pebc -run` flag.
- [x] **CLOSED (`30a5d95`) — `f32`/`f64` helper parameters and return
      values.** `validateHelperSignature`/`helperSignature`/
      `buildCallArgument` widened for float, mirroring the `uint`
      widening pattern exactly; `buildFloatExpr` additionally gained
      `PrefixValue` (unary negation) and `DirectCall`/`MethodCall`
      (a float-returning helper call used as a float value) cases real
      programs need. `examples/leibniz_pi_approx.peb` fully closed
      (`f0950b7`) — compiles and runs, converging to pi (`3.141583`,
      error `0.000010`), exit 0.
- [x] **CLOSED (`6e75c3c`) — was likely the biggest gap found this
      sweep: direct calls to an `extern fn` declaration.** `emit.go` had
      zero references to `tir.ExternDeclaration` anywhere.
      `findFunctionDeclaration` now also matches it; new `externCName`
      resolves the real C name (`malloc`, not `pebble_fn_24`) via a
      symbol table now threaded into `Emit` (signature gained a
      `symbols *symbol.Result` param); new `validateExternSignature`/
      `externCType` validate C-convention param/result C spellings;
      `reachabilityWalk.visit` skips extern callees (no body, no
      helper emitted); `buildDirectCall` gained a C-convention branch
      (real name, no context threading); the preamble adds
      `<stdlib.h>/<string.h>/<math.h>` whenever a C extern exists.
      Design note: the symbol table lives in a package-level
      `emitSymbols` var scoped to one `Emit` call rather than threaded
      through `buildDirectCall`'s ~19 call sites and their transitive
      callers — a deliberate, documented tradeoff (this package assumes
      single-threaded, non-reentrant `Emit`, matching every current/
      planned caller; a reentrant call panics loudly instead of
      silently corrupting state; confirmed race-clean under
      `go test -race`). `examples/extern_mem_funcs.peb` fully closed
      (`ae4b7c0`) — compiles and runs, prints 42, exit 0.
- [x] **CLOSED (`1265121`)** — closure-literal `=>` arrow shorthand
      false `C0607`. Root cause: `syntax.FunctionTerm` (a closure
      literal) has its own dispatch branch entirely separate from
      `callableChildren` (named `FunctionDecl`/`ExternFunction`
      declarations), and that branch was simply missing the arrow-body
      implicit-return wiring block `callableChildren` already had —
      added it. `examples/std_hash.peb` and `examples/std_set.peb` both
      now check clean past this error (confirmed: the `C0607` is
      completely gone from both). Neither is fully "done" yet, though —
      both still surface only PRE-EXISTING, unrelated `C0618`
      "unreachable statement" WARNINGS inside `std/hmap.peb`/
      `std/set.peb` themselves (a trailing `return none;`/`return
      false;` after a `while true { ... }` loop the checker can't
      statically prove never falls through — needed to satisfy the
      must-return check, so likely NOT safely removable without
      triggering the opposite error; not investigated further, low
      priority). `pebc`'s CLI currently exits 1 on ANY diagnostic
      including warnings (`diagnostics.Len() > 0`, not diagnostic
      severity), so both examples still show a nonzero exit from the
      CLI despite genuinely passing check — this is a `pebc` CLI
      strictness quirk, not a compiler correctness gap. Not scoped
      further; flagged here for whoever picks it up next.
- [ ] **[checker] Qualified static-method calls on a nominal type
      (`TypeName.method(...)`) are entirely broken — not just for
      generics.** Found via a full `count_lines.peb` triage (which
      revealed `std/result.peb` itself doesn't check in isolation —
      import it alone into a bare `fn main() int { return 0; }` and its
      own errors surface). `std/result.peb`'s `Result[U, E].ok(...)`/
      `.err(...)` constructor pattern fails with `C0619` (generic case)
      or `N0001: type has no member` (through a type alias). Reproduced
      standalone, bounded precisely: `Box[int].mk(5)` on a fresh generic
      struct → `C0619`; **`Box.mk(5)` on a plain NON-generic struct also
      fails** → `C0619` — so this isn't specifically a generics gap, the
      qualified-call form itself doesn't resolve. Also confirmed a
      self-less method declaration (`fn mk(x int) Box { ... }`, no
      `self` parameter) is independently rejected with `C0604 method
      self parameter is invalid` — meaning the entire "static
      constructor method, called via `TypeName.method(...)`" pattern is
      unimplemented today, not merely a generic-specialization edge
      case. This is a substantial, foundational gap: it blocks
      `std/result.peb` entirely (its whole public API is `ok`/`err`
      constructors) and likely any other std/user code following the
      same "constructor static method" idiom. **Now fully scoped**
      (2026-08-07 investigation): root-caused exactly —
      `finishCall` (`check/call_facts.go:398`) looks up
      `w.valuesBySyntax[m.base]` for the receiver, but when the member
      base is a type `Path` (not a value expression), `suppressAll()`
      (`check/expression_facts.go:152`) already prevented any value
      from being produced for it, so the receiver lookup fails,
      `failExpression` fires, no call record is created, and the
      solver reports an unresolved variable → `C0619`. The self-less
      -method rejection is CATEGORICAL, not a narrow bug: three
      independent rejection points (`finishCall`'s receiver lookup,
      `validateCallRecords:93` unconditionally rejecting
      `Receiver == 0`, `ir_builder_calls.go:105`'s `buildValue(0)`
      failing) — no "static method" concept exists anywhere in the
      method model. Confirmed a genuine missing feature, not a
      deliberate design decision (`open-language-decisions.md` doesn't
      list it; nothing in spec forbids it). Real implementation plan:
      8 files across 3 layers (facts/`call_facts.go`+`member_facts.go`,
      validation/`call_validation.go`+`record.go`, IR/
      `ir_builder_calls.go`, plus an `infer` package constraint) —
      MEDIUM-LARGE, ~2-4 focused sub-tasks. **However: a much cheaper
      path exists and should be tried FIRST.** Two alternatives already
      work TODAY with zero compiler changes: (1) record-construction
      syntax, already functional via `aggregateTaggedVariant` —
      `Result[int, str].{ Ok = 42 }` / `Result[int, str].{ Err = "bad" }`
      — or (2) plain top-level generic helper functions (`fn
      result_ok[T, E](value T) Result[T, E] { return Result[T, E].{ Ok
      = value }; }`), which is the more idiomatic Pebble shape.
      **DONE (`b9d9738`): `std/result.peb`'s public API rewritten
      around option (2)** — `ok`/`err` replaced with top-level
      `result_ok[T, E]`/`result_err[T, E]`, `map` constructs records
      directly. `examples/count_lines.peb`'s call sites updated to
      match. Nothing in the tree depends on the qualified-static-call
      feature anymore — the real checker feature above stays open but
      is now a genuinely low-priority, purely architectural item, not
      blocking anything. **Note: `std/result.peb` itself still does
      NOT check clean in isolation** — `is_ok`/`unwrap_or`/`map`/
      `set_error` all read `self.Ok`/`self.Err`, which is the
      SEPARATE, already-tracked, deliberately-deferred union-variant
      -payload-access gap (flow-sensitive type narrowing — see that
      entry elsewhere in this doc). That gap, not this one, is what's
      left blocking `std/result.peb` and `count_lines.peb` now
      (`count_lines.peb` also still has its own unrelated `usize`
      staleness, not yet fixed).
- [x] **CLOSED (`608c645`) — `std/result.peb`'s bare `case Ok:`/
      `case Err:` switch-case syntax fixed to dot-shorthand `case .Ok:`/
      `case .Err:`** (8 case labels total across `is_ok`/`unwrap_or`/
      `map`), plus the same bug in `examples/count_lines.peb`'s own
      switch (2 instances). Verified: the `N0001 undefined name "Ok"`
      errors are gone from both files.
- [x] **CLOSED (`a6d83e7`) — `std/hash.peb`'s `hash_ptr` explicit
      pointer→u64 cast, same gap as the ptr-to-uint/u64 item below.**
      `*T as uint`/`*T as u64` now classify `compatibleExplicit`
      (`compatibility.go`, new `coercionPointerToInteger` kind) and
      lower to a plain unchecked C cast (`emit.go`, new
      `tir.PointerToInteger` node in both `buildExpr` and
      `buildUintExpr`). One direction only — `uint`/`u64 as *T` stays
      forbidden by design (would reopen the pointer-arithmetic backdoor
      `open-language-decisions.md` closed), covered by explicit
      regression tests. Confirmed via causation-check (reverted, saw
      the original `C0601` on real `std/hash.peb`, restored). Re
      -verified 2026-08-07: `examples/std_hash.peb`/`examples/
      std_set.peb` no longer show the `C0601` pointer-cast error —
      `std_hash.peb` now hits only the already-tracked pre-existing
      `hmap.peb` C0618 warnings.
- [x] **`std/io.peb` fixed (`524eee4`) — was stale relative to a
      `string.peb` refactor (`String.data` became `[]char`, a slice, no
      longer a raw `*char`) and the "no uninitialized locals" rule.**
      Six mechanical fixes (cast `ftell`'s `i64` before `uint`
      arithmetic/`fread`, route through `s.data.data as *void` for
      `fread` matching `string.peb`'s own pattern, replace raw pointer
      arithmetic with slice indexing, initialize the `var ch char`
      local, cast `&ch` to `*void` for `read()`, fix `s.len += -1` to
      `s.len -= 1`) plus a signature change (`fwrite`/`write_bytes` now
      take `str` directly instead of illegally casting `str as *void` —
      str↔pointer casts are forbidden by design; extern functions
      marshal `str` at the FFI boundary instead, matching `fopen`/`stat`'s
      existing pattern). Verified: `std/io.peb` now checks fully clean.
      `examples/read_file.peb` still fails, but ONLY on its own two
      separate, unrelated bugs (below) — no error traces to `io.peb`
      anymore.
- [x] **PARTIALLY CLOSED, Slice A only (`dcc935f`) — checker-side
      widening: `fn main(argv []str) int` is now accepted as a valid
      entry point.** The v2 checker previously only accepted a
      ZERO-parameter entry point (`entry_validation.go`,
      `len(signature.Parameters) == 0`). Confirmed a genuine v1-parity
      gap, not by-design: the old v1 C backend explicitly supported 0/1
      (`argv []str`)/2 (`argc int, argv []str`) parameter `main`
      signatures (`10-c-backend-implementation-plan.md:2212-2238`,
      itself flagged there as `REDESIGN` — v1's own 2-param path had a
      bug passing raw `argv` despite `[]str` verification, which is why
      **only the one-parameter form was implemented here** — the
      2-param form stays deliberately rejected). New
      `validArgvParameter` helper resolves the parameter's type and
      confirms it's `[]str` specifically (not "any one parameter").
      Verified end-to-end against a real fixture: `fn main(argv []str)
      int { return 0; }` now reaches emission (no more `C0620`) — this
      slice deliberately stops at the checker boundary; a program
      declaring `argv` but not reading it now compiles past check
      entirely, which is the complete, useful deliverable of this
      slice on its own. **Still open (Slice B, not yet dispatched):**
      backend/IR support for actually READING `argv`'s contents inside
      `main`'s body — `pebble_user_main` and both C entry templates
      (`voidEntryUserMain`/`integerEntryUserMain` in `emit.go`) still
      hard-wire zero parameters at the C level, and the real C `main`'s
      `argc`/`argv` are still discarded (`(void)argc; (void)argv;`) —
      needs an `argc`/`argv`→`[]str` adapter threaded through the IR
      builder and both entry templates. Larger, multi-layer, not yet
      scoped. **Note:** `examples/read_file.peb` was originally written
      against this shape and blocked by it, but has since been
      rewritten (`a1b69dc`) to a zero-parameter `main` with a hardcoded
      demo path, sidestepping the gap rather than closing it — this
      item is no longer blocking any file in the tree.
- [x] **STALE, file rewritten — `examples/read_file.peb`'s
      `contents.as_str()` call no longer exists.** Same rewrite
      (`a1b69dc`) replaced it with a char-by-char loop over
      `contents.as_slice()` (`String`'s real API). File now fully
      compiles and runs; its only remaining diagnostic was the
      (separately tracked, now also closed) `std/io.peb` C0618
      warning.
- [x] Stale example files fixed directly (not compiler gaps — verified
      each compiles+runs after the fix, or is blocked only by one of
      the real gaps above): `extern_mem_funcs.peb` (`usize`→`uint`,
      missing explicit casts on `malloc`/`free`, still blocked by the
      `concrete specialization not found` gap above), `prime_sieve.peb`
      (`primes[:]` via a named local — compiles and runs, verified,
      committed `8ea3936`), `README.md` (same `usize`/`isize`/`float`
      staleness throughout its type list and code examples — committed
      `52d9c59`), `slice_minmax.peb` (missing `: iter` binding, which
      surfaced the real `buildPlaceLValue` structural-field gap above —
      both fixed, file fully compiles and runs, committed `0fc2480`
      alongside the backend fix `a31cf99`), `extern_mem_funcs.peb`
      (`usize` + two missing explicit casts — blocked until the
      extern-call backend gap above landed; both fixed, file fully
      compiles and runs, committed `ae4b7c0` alongside the backend fix
      `6e75c3c`), `leibniz_pi_approx.peb` (`float`→`f64` plus a mixed
      -int/float-arithmetic fix — blocked until the f32/f64 helper
      -param backend gap above landed; both fixed, file fully compiles
      and runs (converges to pi, `3.141583`), committed `f0950b7`
      alongside the backend fix `30a5d95`), `std_hash.peb`
      (`usize`→`uint`, committed `325dac6` — now checks clean past both
      that and the arrow-closure-literal bug above, but still surfaces
      pre-existing unrelated `std/hmap.peb` warnings; see that entry's
      note). Not yet closed: `read_file.peb` (stale `String.as_str()`
      call — std/string.peb deliberately has no such method; also
      still has its own separate `argv`-in-`main` unsupported-entry
      -point issue). `count_lines.peb` — fully triaged (its own separate
      "Part A.1 sweep" note below has the full ~60-error root-cause
      breakdown: `usize`, the `case Ok:` syntax bug, the qualified
      -static-call gap, `std/io.peb`, union-payload-access). `usize`
      fixed (`1f6103e`) — genuinely confirmed further progress, not
      just a no-op: the file now surfaces real remaining issues past
      that fix, specifically the exact same stale
      pointer-arithmetic-on-`[]char`-slice pattern already fixed in
      `std/io.peb` (`contents.data + contents.len - 1` should be
      `contents.data[contents.len - 1]`, not yet applied to this
      example) plus errors tracing to the deliberately-deferred union
      -payload-access gap. Not chased further — fundamentally blocked
      on that deferred feature regardless of any other fixes here.
      `bubble_sort.peb` — a genuine `[generator]` gap, not example
      staleness: `validateHelperSignature` in `emit.go` rejected
      array-typed helper parameters/return values (`[5]int`); FIXED and
      verified (`f394a10`, escalated to Luna after two flash stalls —
      the real fix needed a C struct-wrapper typedef mechanism for
      arrays-by-value, genuinely harder than the other gate-widenings
      this session): compiles under `-Wall -Wextra -Werror` and runs,
      printing the correctly sorted values, exit 0. `count_lines.peb` —
      fully triaged (see the qualified-static-call and `case Ok:`
      findings above/below): stale-example fault is `usize` plus the
      `case Ok:`/`case Err:` syntax bug; but even after fixing both,
      the file can't check clean until the qualified-static-call gap
      and the (already-deferred) union-variant-payload-access gap are
      implemented — it also transitively imports `std:io`, so it was
      independently hitting the `std/io.peb` gap above too (same root
      cause, being fixed there).
- [x] **CLOSED (`ce86705`) — `std/io.peb`'s `read_line` C0618 unreachable
      -statement warning removed.** Same pattern as the `hmap.peb`/
      `set.peb` C0618 warnings noted above, now empirically resolved for
      this shape: the trailing `return string::new();` after
      `read_line`'s `while true { ... }` loop is genuinely dead (every
      internal path already returns), so deleting it is safe — confirmed
      no new C0607 (missing return) appears, and a full `go test ./...`
      run stays green. This also answers the open question left by the
      `hmap.peb`/`set.peb` note above: at least for the "trailing return
      after an exhaustively-returning `while true`" shape, deletion is
      the correct fix, not a workaround — worth revisiting those two
      files with the same treatment.
- [x] **CLOSED (`d7bbadc`) — `std/string.peb:18`'s `self.data = slice
      ptr, new_cap;` inside `grow()` now emits.** `buildStoreCore`'s
      `isSlice` branch only accepted a `SymbolValue` (reference to an
      already-declared slice-typed local) as the reassignment value;
      added a `SliceFromRaw` case reusing the existing
      `buildRawSliceConstruction` helper (already used for the
      slice-local-declaration case), type-checked against the field's
      declared slice type before use. New end-to-end test
      (`TestEmitSliceFieldReassignmentFromRawCompilesAndRuns`) compiles
      under `-Wall -Wextra -Werror`, runs, and asserts the reassigned
      value reads back correctly. Causation confirmed (revert
      reproduces the exact original error; restore passes again).
      Re-running `examples/read_file.peb` confirms the `SliceFromRaw`
      error is completely gone — replaced by a new, different,
      unrelated gap (below), not a regression.
- [x] **CLOSED (`4956133`) — a struct-typed `return` statement can now
      directly return a `DirectCall`.** `buildAggregateReturnValue`'s
      struct branch only accepted a `SymbolValue` or a `RecordConstruct`;
      widened to accept a `DirectCall` too, forwarding the callee's
      result (validated against the function's own result type), built
      via the existing `buildDirectCallWithPre`. Signature changed from
      `(string, error)` to `(string, string, error)` (a `(pre, expr)`
      pair, mirroring `buildSliceReturnValue`'s existing convention),
      since a return is a pure expression position with nowhere to
      inline a call's pre-statement. Both call sites
      (`buildReturnStatement`, `buildSwitchCaseBody`) updated. The tuple
      branch is untouched — `DirectCall` stays rejected there, by
      design, out of scope. New end-to-end test
      (`TestEmitStructReturningHelperForwardsCallCompilesAndRuns`)
      chains two struct-returning helpers with no intermediate local,
      compiles and runs, reads real fields back. A pre-existing test
      asserting the OLD restriction for this exact shape
      (`TestEmitRejectsStructReturningHelperInAnotherHelpersReturn`) was
      removed as obsolete — its coverage is superseded by the new
      positive test. Causation confirmed (revert reproduces the
      original error; restore passes again). Re-running `examples/
      read_file.peb` confirms the `DirectCall` struct-return error is
      completely gone — `std/io.peb`'s early-return paths (`read_all`,
      `read_line`, `get_file_error`, etc.) now emit correctly.
- [x] **CLOSED (`214fa3d`) — `int`-vs-`i32` width mismatch, root-caused
      as two stacked gaps, not the range loop originally suspected.**
      `std/io.peb`'s `let SeekEnd = 2;` (an untyped-literal global
      constant, staying at the abstract `int` builtin) is passed to
      `fseek`'s `whence i32` parameter. (1) The backend had NO
      mechanism at all for lowering a reference to ANY `let` global
      constant — globals aren't in locals scope, and the initializer
      was never represented in the TIR for a reference site to rebuild
      from. Fixed: `buildDeclarations` now records every `let` global's
      initializer; a reference is rebuilt as a fresh copy of the
      initializer (`buildValueRecord`, a new unmemoized core split out
      of `buildValueBase`) rather than an unresolvable `SymbolValue`,
      registered with an empty source-map ref so multiple inlined
      copies of one declaration don't collide (`MapSource` rejects a
      second, different node mapped to the same syntax ref — this was
      the exact cause of a regression hit mid-fix, described below).
      (2) Separately, the backend's width gate required an exact
      builtin match, rejecting an abstract-`int`-typed node even where
      `int` and `i32` share a C representation. Fixed: new
      `isAbstractInt` + a width-gate exception in `buildExpr`. This is
      now a GENERAL capability — a `let` global of any type is inlined
      at each reference site, not narrowly scoped to int constants
      (confirmed this was never actually usable end-to-end for enum/
      char/pointer/optional-typed globals either, just differently
      broken — a bare `SymbolValue` naming a global was never
      resolvable by the backend regardless of type). Causation
      confirmed (revert reproduces the exact original error; restore
      passes past it to a new, unrelated gap, below). Full `go test
      ./...` stays green (including 4 pre-existing `check`-package
      tests and 1 `backend`-package test whose assertions reflected the
      old, now-superseded `SymbolValue`-child/rejection shape —
      corrected, not silently weakened; see the commit message for the
      full list).
      **Process note**: this fix took three dispatch rounds. Attempt 1
      (flash) introduced a real regression (`MapSource` duplicate
      -registration — importing `std:io` at all broke `buildBlocks`,
      even with zero constant references) which I caught via direct
      verification before committing (never assume a dispatch's own
      "done" report without independently re-testing the exact
      repro). Attempt 2 (also flash, resumed same session) left debug
      `fmt.Fprintf`/`os.Stderr` instrumentation in place without
      imports, breaking the build. Escalated to Luna (same resumed
      session) for attempt 3, which correctly root-caused and fixed
      the regression but stopped before adding the required tests;
      verified the core fix was correct via causation-check myself,
      then wrote the tests directly per the established exception for
      a verified-but-untested stalled dispatch.
- [x] **CLOSED (`c9f99de`) — discarded non-void call used as a bare
      statement.** `buildExpressionStatement`'s `isVoid` gate rejected
      a bare discarded-call statement (`f();`) whenever the callee
      returned anything but void — `std/io.peb`'s `read_all` calls
      `fseek(file, 0, SeekEnd);` as a bare statement, deliberately
      discarding its `i32` return value (ordinary C idiom; the checker
      itself already permits a discarded call statement of any result
      type, only rejecting a discarded non-call expression). Fixed by
      removing the `isVoid` check entirely — in C, discarding any
      function's return value at a bare call statement is always legal
      and never warns, even under `-Wall -Wextra -Werror`, so the call
      is now built and emitted identically regardless of result type.
      New end-to-end test (`TestEmitNonVoidDiscardedCallStatementCompilesAndRuns`)
      calls a print-then-return helper as a bare statement, captures
      its output to prove the side effect ran while the discarded
      result didn't disturb subsequent logic. Causation confirmed
      (revert reproduces the exact original error; restore passes
      again). Re-verified against a real `std/io.peb` scratch fixture:
      the `fseek`-as-bare-statement error is completely gone, reaching
      the next, separate, already-tracked gap below.
- [x] **CLOSED (`f011286`) — masked `C0621` generic-requirement
      -propagation gap in `std/math.peb`'s `clamp[T]`, exposed by the
      float-constant fix (`b4410cb`).** Root cause: `clamp[T] { return
      max(lo, min(x, hi)); }` never uses `<`/`>` directly — only calls
      `min[T]`/`max[T]`, which do — so `clamp`'s own type parameter `T`
      never accumulated an `Ordered` requirement of its own, yet
      `validateGenericInstantiations` checked the INNER calls against
      `clamp`'s still-abstract `T` at check time (correctly failing,
      since an unresolved type parameter has no concrete builtin/enum
      identity) instead of deferring to `clamp`'s own external
      instantiation sites (which the same validator already
      independently checks). Two-part checker fix: (1) new
      `propagateGenericRequirements` (`requirement_validation.go`) —
      a fixpoint pass copying a callee's requirement onto a caller's
      own rigid type parameter wherever an instantiation's argument is
      that parameter, repeating until stable (so multi-hop chains like
      `outer→mid→min` propagate correctly); (2) new
      `deferredGenericRequirement` (`generic_validation.go`) — skips
      judging an inner call's argument against the enclosing
      declaration's own type parameter when that declaration already
      carries the (now-propagated) same requirement, deferring
      enforcement to its own external call sites. A necessary second
      layer in the typed-IR builder (`ir_builder.go`/
      `ir_builder_calls.go`/`ir_builder_value.go`): `buildSpecializations`
      now skips a symbolic top-level instantiation (new
      `concreteInstantiation`, reusing the existing
      `containsTypeParameter`); `buildDirectCall`/
      `buildGenericFunctionValue` resolve their own type arguments
      through the active specialization substitution when building
      inside an already-concrete specialization, building the callee's
      own matching concrete specialization once resolved — otherwise
      the backend would have nothing to match `min[i32]`/`max[i32]`
      against even after the checker accepted the shape. Six new tests
      cover the exact `clamp[T]` shape, a 2-hop transitive chain, a
      different requirement kind (`Numeric`), and two regression
      guards proving a bad concrete type (a struct) is still correctly
      rejected for both `Ordered` and `Numeric` — confirming the fix
      defers checking rather than disabling it. Causation confirmed
      (revert reproduces the exact original two `C0621` errors;
      restore passes all six new tests). Re-verified against the real
      `std/math.peb`: `math::clamp` called with concrete `i32` locals
      no longer produces `C0621` at all — reaches emission and hits a
      different, separate, pre-existing backend limitation instead
      (below), not a regression.
- [x] **CLOSED (`c0e0306`) — a generic helper's specialized parameter
      width vs. the entry's own declared width.** `math::clamp`
      (specialized at `i32`) called from an `fn main() int { ... }`
      entry, whose own declared width is the abstract `int` builtin,
      not `i32`, failed — the backend's helper-parameter validator
      (`validateHelperSignature`) only accepted the ENTRY's own
      resolved width for a scalar parameter (exact `BuiltinKind` match
      via `isWidth`), not an arbitrary concrete width a generic
      specialization produced (`clamp[i32]`'s parameters are genuinely
      `i32`, a distinct builtin from `int` sharing the same C
      representation `int32_t` — the same asymmetry the `int`-vs-`i32`
      fix above addressed for the reverse direction). New symmetric
      twin `isCompatibleIntegerWidth` (accepts ANY integer builtin
      whose `cType(...)` matches the requested width, not just the
      abstract `int` one) wired into `validateHelperSignature`'s
      parameter gate, `helperSignature`'s parameter-declaration switch
      (new case declaring the C parameter at its own concrete width),
      and `buildExpr`'s general width gate. `isWidth` itself untouched;
      a genuinely mismatched width (`i64` in an `int`/`i32` context) is
      still cleanly rejected, confirmed by a new regression test. Four
      new tests: the exact repro (run, returns correct value); the real
      `clamp[T]` shape defined inline (run, returns the correctly
      clamped value); an emitted-C shape assertion (genuinely
      `int32_t`, not a guessed width); the mismatched-width regression
      guard. Causation confirmed (revert reproduces the exact original
      error; restore passes again). Re-verified against the real
      `std/math.peb`: `math::clamp` called with concrete `i32` locals
      from a real `fn main() int` entry now compiles AND RUNS
      end-to-end, returning the correct clamped value.
- [x] **CLOSED (`317f3c0`) — `buildUintExpr` (the specialized builder
      for a `uint`-typed expression) had no case for `tir.DirectCall`**
      — found via a further end-to-end re-check of `examples/
      read_file.peb` after the two fixes above landed. `pebc: emission
      failed: unsupported uint expression node DirectCall`. Real shape:
      a call to a `uint`-returning helper used as a `uint` local's
      declaration initializer (`var bytes = read(file, &ch as *void,
      1);`, the `std/io.peb` `read_line` shape — `read` returns `uint`).
      Fixed by adding a `DirectCall` case: resolves and validates the
      callee's result type is genuinely `uint`, builds via the existing
      `buildDirectCallWithPre`. Deliberate scope boundary: `buildUintExpr`
      returns `(string, error)` with no pre-statement threading and is
      called from ~18 pure-expression positions throughout the file —
      a non-empty `pre` (only produced for an inline slice-construction
      argument) is a clean rejection naming the unsupported shape,
      never silently dropped; a signature-widening refactor across all
      18 call sites was judged out of scope for this gap. Two new
      end-to-end tests (a plain repro, and a read_line-shaped variant
      with pointer/char-address-of arguments) compile and run,
      asserting the actual `uint` value round-tripped correctly.
      Causation confirmed (revert reproduces the exact original error;
      restore passes again). Re-verified against `examples/
      read_file.peb`: the `DirectCall` error is completely gone,
      reaching a different, separate, new gap (below). **Note:**
      `std/io.peb`'s `read_line` is not actually reachable from
      `read_all` in the real file (confirmed by tracing the call
      graph) — this fix's real-world relevance is the general
      DirectCall-as-uint-initializer shape, not specifically
      `read_all`'s own path.
- [x] **CLOSED (`6eb5a63`) — re-slicing an existing slice-typed value
      (not just a fixed-size array).** `buildSliceConstruction`
      required its base to be a bare `SymbolValue` naming an
      ARRAY-typed local — `std/string.peb`'s `String::as_slice()` does
      `return self.data[:self.len];`, where `self.data` is `[]char` (a
      slice FIELD, not an array), lowering to a `Load(FieldPlace)`
      base, which failed with `entry function body return statement
      slice base is a Load, want a SymbolValue naming an array local`.
      Slicing an array and re-slicing a slice need genuinely different
      C (an array decays to a pointer with a compile-time length; a
      slice base has no compile-time length — the new slice's `.data`
      must offset the EXISTING slice's own runtime `.data`, and the
      bounds-check upper limit is the existing slice's runtime `.len`).
      Fixed by adding a second accepted base shape alongside the
      UNCHANGED array path: a `Load` of a slice-typed place builds its
      C lvalue via `buildPlaceLValue`, using `<lvalue>.len`/
      `<lvalue>.data` in place of the array path's compile-time
      length/decay. No runtime-helper changes needed — the bounds
      -check helper already accepts its length argument as a plain C
      expression. Three new tests: the exact `as_slice()` shape (run,
      correct value); the explicit-start-bound twin (proving the
      offset math against the base's own `.data`); an emitted-C shape
      assertion. Causation confirmed (revert reproduces the exact
      original error; restore passes again); the pre-existing array
      -slicing tests pass unchanged (no regression to the original
      path). Re-verified against the real `std/string.peb`:
      `String::as_slice()` now compiles and runs end-to-end.
- [x] **CLOSED (`6426177`) — indexing directly into a value with no
      addressable place (a slice/array-typed call result) is now
      supported, not just `str`.** `foo()[i]` is an ordinary operation
      in the language; the fix is a REAL capability, not routing around
      a deliberate restriction (see the corrected framing this doc
      carried briefly before the fix landed). Root design constraint:
      unlike `str` (a stateless decode function safely callable on the
      base repeatedly), a slice/array read needs the base's `.data`
      AND `.len`, so a freshly-computed base (a call result) evaluated
      twice would run the underlying call twice — wrong and wasteful.
      New `buildSliceIndexValue` handles four base shapes: a
      `SymbolValue` naming a slice-typed local or a `Load` of a
      slice-typed place (pure projections, safe to reference twice, no
      temp needed) build directly; a `DirectCall`/`MethodCall` or a
      `FieldValue` reading a slice field off a call result are built
      ONCE into a temp local, then bounds-checked and read off the
      temp (the same two-statement pattern `buildSliceConstruction`
      established). The temp-declaration statement is threaded as a
      leading pre-statement into `buildPrint` (now `(pre, expr,
      error)`, the established `preReturn` convention) and its
      callers; positions with no statement sequence to host a temp
      (`return`, comparisons, arguments) accept only the
      no-temp-needed base shapes and cleanly reject a call-result base
      with an actionable "bind the slice into a local first" message.
      Five new tests: the confirmed real-world shape (captured stdout,
      correct value); a single-evaluation proof (the base call's own
      side-effect marker prints exactly once, not twice — the
      correctness property that matters most here); the char-element
      twin; the pure-projection base shape (unaffected, still works);
      a bounds-check regression guard. One pre-existing test's
      assertion was updated, not weakened (the same array-literal
      -indexing scenario is still correctly rejected, just with
      updated wording for the now-wider accepted set). Causation
      confirmed; full suite green. Independently re-verified beyond
      what the dispatch itself checked: correct values, single
      -evaluation, and bounds-checking all confirmed via direct `pebc`
      runs before committing. `examples/read_file.peb`'s own instance
      of this gap was already sidestepped (`d24779f`, binding
      `as_slice()`'s result to a local) — that workaround stays in
      place (still a reasonable standalone improvement), but the real
      underlying capability is now genuinely implemented too.
- [ ] **[generator] New, foundational, general gap found verifying
      `read_file.peb` further: `extern { type FILE; }` (an opaque
      extern type with no body) emits as a synthesized
      `pebble_struct_<N>_t*` instead of mapping to its real C type
      (`FILE`, already available via the `<stdio.h>` preamble
      `hasCExterns` already includes).** Confirmed via a minimal
      standalone repro (`extern { type FILE; fn fopen(...) *FILE; fn
      fclose(file *FILE) i32; } fn main() int { var f = fopen(...);
      fclose(f); return 0; }`) — `cc` rejects the emitted C outright:
      `fopen`/`fclose`/etc. all expect a real `FILE *`, but the backend
      passes a `pebble_struct_23_t *` it invented for the opaque type
      instead. This blocks `std/io.peb`'s entire `FILE`/`DIR`-based API
      (`open`, `close`, `read`, `read_all`, `read_line`, `seek`, `tell`,
      `filesize`, `flush`, `is_eof`, `get_file_error`, `is_dir`, and
      more) from ever actually compiling to a working binary — not
      previously reachable in `examples/read_file.peb` because earlier
      gaps (now all closed) failed first. Confirmed pre-existing, not a
      regression — verified via `git stash` that the error surfaces
      identically without today's `read_file.peb` edit, just masked by
      the (now-fixed) earlier `MethodCall`-indexing rejection. Not yet
      root-caused precisely (exact function/line handling `extern type`
      declarations not pinned down) or scoped for dispatch — likely
      needs the extern-type declaration's OWN name threaded through as
      the emitted C type instead of synthesizing a struct, for any
      `extern type` with no body (an opaque type, as opposed to a
      transparent struct-mapped extern type, if any exists).
- [x] Stale `contents.data + contents.len - 1`/`contents.data + i`
      pointer-arithmetic-on-slice fixed directly in `examples/
      count_lines.peb` (`0c9997b`) — the exact same staleness pattern
      already fixed in `std/io.peb` (`String.data` became `[]char`, not
      `*char`). Replaced with slice indexing. Clears the `T0505`/
      `T0508` errors this staleness caused.
- [ ] **[checker] New, general bug found sweeping example files further:
      a function whose declared result type is a type alias to a
      generic instantiation is misclassified with an invalid calling
      convention (`C0604 callable declaration is invalid`).** Confirmed
      via a minimal standalone repro (do not modify `examples/
      count_lines.peb`'s own shape — this is the real, general bug it
      happens to hit, not example-specific staleness):
      ```
      import "std:result";
      type StatsResult = result::Result[int, str];
      fn count_file(filename str) StatsResult {
          return result::result_ok[int, str](5);
      }
      fn main() int { return 0; }
      ```
      Root-caused precisely: `declaration_facts.go`'s
      `handleNamedCallable` has an early-return error branch (`if
      !prepared || signature.State != infer.DeclarationReady { ...
      w.retainCallable(record); return; }`) that retains the
      `callableRecord` WITHOUT ever setting `record.Convention` (only
      set on the normal path, via `record.Convention,
      record.Variadic = signature.Convention, signature.Variadic`,
      which this branch skips entirely) — leaving `Convention` at the
      Go zero value, which is neither `types.Pebble` (`1`) nor `types.C`
      (`2`) (`internal/types/key.go`: `Pebble CallingConvention = iota
      + 1`). `call_validation.go`'s `validateCallableRecords` then sees
      `callable.BodyPresent && callable.Convention != types.Pebble` →
      true, reporting the misleading `C0604` "callable declaration is
      invalid" — the REAL problem is upstream: `count_file`'s signature
      never reaches `infer.DeclarationReady`, almost certainly because
      its result type (a local alias to a CROSS-MODULE generic
      instantiation, `result::Result[Stats, str]` imported from
      `std:result`) isn't resolving correctly during signature
      preparation. Blocks `examples/count_lines.peb` (whose
      `count_file` function is exactly this shape) even after its own
      unrelated staleness is fixed (above). Not yet root-caused past
      this point (why signature preparation specifically fails for
      this alias-to-cross-module-generic-instantiation shape isn't
      pinned down) or scoped for dispatch — needs real `infer`/`check`
      package investigation, likely in whatever code prepares a
      function's `Signature()` before `handleNamedCallable` consumes
      it. A secondary, smaller finding worth fixing alongside: the
      `C0604` error MESSAGE itself is misleading for this failure mode
      (it reports "invalid calling convention" for what's actually "the
      declaration's signature never became ready") — regardless of the
      real fix, the diagnostic should point at the true cause once
      found.

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
- [x] Function types: fully done, all four slices (2ab27d6, 0b6ed32,
      0643cca, b5c139c) — locals, values, indirect calls, struct
      fields, parameters, results, and `uint`/`u64` in every position.
      `std/hmap.peb`'s `hash_fn fn (K) u64;` field now compiles and
      runs through `backend.Emit`. `std/set.peb` uses the identical
      `hash_fn` shape via `hmap` and should be re-verified the same way
      before being trusted end-to-end (not separately re-tested).
- [x] Generic struct DATA fields, slices 1 (`254a00c`) and 2a
      (`8c339d3`) done — a field directly typed as the struct's own
      type parameter (`key K`), and a field whose type WRAPS a
      parameter via `?K` or `*K`, both now resolve correctly
      per-instantiation, including two specializations of the same
      generic struct sharing every field symbol in one program.
- [x] Struct field construction from an inline slice expression fixed
      (`9024f37`) — **corrected framing: this was never
      generic-specific.** `Table.{ items = arr[:] }` failed identically
      for a plain non-generic struct; `buildStructBraceList`'s slice
      case only ever accepted a reference to an already-declared
      slice-typed local, unlike every other aggregate field type. Also
      fixes the generic case this was originally investigated for.
- [x] Nested-generic struct fields fixed (`56fd83e`) — one generic
      struct field typed as ANOTHER generic struct instantiated with
      the outer's own parameter (`inner Inner[K]`) now works, including
      two specializations of the outer struct in one program.
      `collectStructTypesWalk` never recursed into a `RecordConstruct`'s
      `field.Value` (only `.Children`), so a nested construction's
      struct type never got collected for a typedef — the same
      "`Fields` isn't in `Children`" gap already closed twice elsewhere
      this session. Note: `HashMap[K,V]`'s actual `backing` field is
      `Allocator`-typed, a compiler builtin rather than a real generic
      struct declaration, so this fix's exact applicability to
      `std/hmap.peb` itself is not yet confirmed — re-verify against
      the real file before assuming it's fully unblocked.
- [x] Generic struct METHOD calls fixed (`6666d2d`) — a method that
      redeclares its own type parameters matching its containing
      struct's (the exact shape `std/hmap.peb`'s real methods use,
      e.g. `fn insert[K, V](self *HashMap[K, V], ...)`) now builds a
      concrete specialization and compiles/runs, including two
      specializations of the receiver, extra parameters beyond `self`,
      and a pointer receiver. Root cause was checker-side: the checker
      never built a concrete specialization for a method call the way
      it already did for a free generic function call, so the backend
      always found nothing to lower against. **Still open, narrower**:
      a method declaring NO type parameters of its own but still
      referencing its containing struct's parameter directly (`fn
      describe(self Box[K]) int => 42;`, no `[K]` on the method) hits
      a different, unrelated backend error when called — not fixed,
      not the shape `hmap.peb` actually uses, so not urgent.
- [x] `std/hash.peb`'s `hash_bytes` fixed (2a917d5) — **corrected
      framing: this was never a checker bug.** Pointer arithmetic
      (`ptr + integer`) is deliberately forbidden by the language
      (`open-language-decisions.md` §1.5); `hash_bytes` used it and
      was correctly rejected. Redesigned to `data []u8` + slice
      indexing, matching the established `vec.peb`/`set.peb` house
      style (`52c72b7`).
- [x] Slice element-type support widened (f85b4a0) — any fixed-width
      integer (`uint`, `u64`, `u8`/`u16`/`u32`, `i8`/`i16`/`i32`/`i64`)
      and `char` now work as a slice element, alongside the
      already-working entry width and `bool`. Still open, narrower:
      `[]str` and a slice of any struct/tuple/generic-struct type
      (`HashMap[K,V]`'s `entries` field is this shape) are unchanged —
      not confirmed to be the same fix as this one turned out to be,
      contrary to the earlier note here; this slice was scalars only.
- [x] Comparison operators on non-entry-width integers fixed
      (`30fca68`) — **narrower than first framed: this was never
      general arithmetic, only comparison operands.** `u64`/`i64`/`u8`
      etc. already worked fine in arithmetic, casts, calls, and
      returns; only `==`/`!=`/`<`/`<=`/`>`/`>=` on a non-entry-width
      integer rejected outright (`buildComparisonOperand` built every
      such operand at the ambient entry width instead of its own).
- [x] Checked arithmetic (`+`, `-`, `*`) for `u64` fixed (`d5e7fe9`) —
      new runtime primitives (`pebble_rt_checked_add/sub/mul_u64`,
      mirroring the `i64` family) plus `pebble_rt_checked_slice_start_u64`
      and `pebble_rt_checked_str_char_at_u64`, which turned out to be
      reachable too (a `u64`-returning function's ambient width reaches
      every `checkedSuffix` call site, not just arithmetic — found by
      compiling emitted C directly with `cc`, not just trusting the Go
      test suite). `u64` division/modulo, shifts, and float-to-integer
      conversion still have no runtime twin and are now explicit clean
      rejections. `std/hash.peb`'s real `hash_bytes` body (mirrored,
      not the actual file — see below) now compiles and runs
      end-to-end via `backend.Emit`.
- [x] `char as <integer>` explicit cast fixed (`5a53a4d`) — one
      direction only (`integer as char` deliberately stays forbidden,
      no validity check exists for it). Unblocks `hash_char` and half
      of `hash_str`'s cast (both `std/hash.peb`, not yet re-verified
      against the real file — fixed via a standalone mirrored fixture
      per this session's usual pattern).
- [x] **CLOSED (`a6d83e7`) — `*T as uint`/`*T as u64` explicit
      cast (one direction only), mirroring the `char as <integer>` fix
      above exactly.** Originally decided 2026-08-06 as "non-pressing"
      and deferred; the 2026-08-07 parallel diagnostic fleet sweep
      (Part A.1 above) confirmed this was the sole blocker on TWO
      real example programs (`examples/std_hash.peb`,
      `examples/std_set.peb`), un-deferred, then implemented and
      verified same-day — see the duplicate entry above (`std/
      hash.peb`'s `hash_ptr`) for the implementation detail. The reverse
      (`uint`/`u64 as *T`) stays forbidden — allowing it would let
      user code reconstruct a pointer from an arbitrary integer,
      reopening the pointer-arithmetic backdoor
      `open-language-decisions.md` §1.5/§3.8 deliberately closed.
- [x] **CLOSED (2026-08-07, three slices, `08122fc`/`65699e2`/`a82ed63`)
      — tagged-union field access on the matched variant
      (`self.Ok` inside `case .Ok:`).** Previously deferred as a
      genuine, nontrivial flow-sensitive-narrowing feature, not a
      validator tweak — the user explicitly greenlit implementing it
      this session. Broken into three independently-dispatched,
      independently-verified slices after the first attempt at one
      giant combined brief stalled three times in a row with zero code
      written despite correct investigation each time (a 185-line
      brief with several open design questions — decomposing into
      short, decisive, single-file briefs immediately unstuck it):
      - **Slice A (checker, `08122fc`):**
        `member_validation.go`'s `memberField` case now accepts a
        union-variant read when `switch_validation.go`'s new
        `switchCaseNarrowing` confirms the access sits inside its
        narrowest enclosing switch-case arm (smallest case-arm span
        containing the access, owned by the same callable so a nested
        closure can't inherit the narrowing — reuses the existing
        `variantBySyntax` case-arm-to-variant mapping, extracted into
        a shared `collectVariantBySyntax` rather than duplicated) AND
        that arm is narrowed to the matching variant. Outside any
        matching case, or in the wrong case, stays a clean `C0605`
        rejection — narrowing, not a blanket unlock.
      - **Slice B (IR, `65699e2`, no production code needed):**
        turned out to be unnecessary — `ir_builder_place.go`'s
        pre-existing `memberSymbol` fallback already matches ANY
        member by name with no `SymbolKind` filter, so it already
        resolved a validated variant read to a real `FieldPlace`
        (confirmed for both a plain union and a generic one). The
        dispatched "fix" was genuine dead code, caught during
        adversarial causation-checking and dropped before commit;
        only a pinning test was kept, confirming the existing
        behavior is correct.
      - **Slice C (backend, `a82ed63`):** `self.Ok` now emits
        `pebble_local_<sym>.payload.pebble_field_<member>` — the
        exact projection the union's own construction side
        (`buildUnionConstruction`) already fills, found by
        investigating the already-working `Result[T, E].{ Ok = value
        }` record-construction path rather than guessing a new C
        layout. Also needed along the way: the union payload gate
        widened from width/bool to width/bool/str (real `Result`
        usage has `str` payloads), enum/union-typed helper parameters
        supported end to end (there was previously no case for one at
        all), and a real regression caught and fixed during this
        slice's own verification — `isEnumType`'s no-evidence
        fallback wrongly matched method-only structs (no
        `FieldDeclaration` nodes) when used to exclude enum-shaped
        types from struct-type collection; a new, stricter
        `isDefinitelyEnumType` (positive `VariantDeclaration`
        evidence, no fallback) fixed it.

      **Verified end-to-end**: `unwrap_or(Result[int,str].{Ok=42}, 0)
      + unwrap_or(Result[int,str].{Err="bad"}, 100)` compiles and
      runs, returns 142. The narrowing boundary (wrong case / outside
      any switch) still correctly rejects. Every slice's causation
      independently confirmed by reverting to HEAD and reproducing the
      original error, then restoring.

      **Two new, real, separate gaps found as stretch-goal
      byproducts, NOT part of this closure, reported not chased:**
      1. `std/result.peb`'s own `is_ok`/`unwrap_or`/`map`/`set_error`
         methods are STILL checker-blocked — the narrowing
         implemented here doesn't yet cover a self-referential GENERIC
         receiver (`self Result[T, E]`, referencing the union through
         the method's own redeclared type parameters — confirmed the
         non-generic, non-self-referential case works fine). Narrowed
         WRITES (`self.Err = error`, needed by `set_error`) are
         SEPARATELY checker-blocked too — Slice A's `narrowedUnionVariantAccess`
         only widened the READ-side `memberField` validation; a write
         through a narrowed place needs its own equivalent check
         somewhere in assignment/place validation, not yet scoped.
      2. `[generator]` A pre-existing, unrelated, currently-UNREACHABLE
         backend gap: str-typed `FieldPlace` reads (e.g. `return
         r.Err;` where `E = str`) have no `buildStrOperand` case in
         whatever function builds a `Load(FieldPlace)` for a str
         field — only matters once gap 1 above closes (the only
         programs that would reach it, `std/result.peb`'s real
         methods, are currently blocked by gap 1 first).
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
