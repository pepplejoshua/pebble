# 07 generics — rough implementation plan

**Status:** planning draft, not an audit. Unlike
`06b-implementation-plan.md`, this is written *before* any 07 slice has
been implemented, so it is deliberately rough where 06b's plan (written
after 06a's real gaps had already surfaced once) could afford to be exact.
Owned files, exact dependency order, and diagnostic codes below are
starting proposals; expect them to sharpen slice by slice as real
implementation details surface, the same way 06b.7b's five parts each
informed the next.

**Baseline.** `main` at `4e62016`. 06a and 06b are both complete (all
06a.1–06a.8 and 06b.1–06b.8 slices accepted, plus the four
Sol-flagged 06b defect fixes). No 07-owned file exists yet.

**Resolved before this plan:** the two genuinely blocking language
decisions from `spec/compiler/proposals/open-language-decisions.md`
§2.1 — always monomorphize (no erased/runtime form), and cross-module
specialization is shared and owned by the declaring module. Both are
now recorded directly in `07-generics.md` §"Specialization". Doc
display of inferred requirements remains open but does not block
implementation.

## What already exists (evidence, not spec prose)

Generics work is further along than `07-generics.md` alone suggests,
because 06a/06b already had to build the symbolic half of the
mechanism to validate ordinary generic code:

- **Type-parameter scoping and substitution** — `internal/infer`'s
  solver (05b) already resolves generic type parameters symbolically
  within a body.
- **Requirement inference, checked once symbolically** —
  `compiler/internal/check/generic_facts.go`'s `retainRequirement`/
  `requirementRecord` collect obligations like `Ordered(T)` from a
  generic body exactly once (spec rule: "the body is checked
  symbolically once; it is not accepted merely because one observed
  instantiation happens to work" — already satisfied).
- **Requirement normalization and per-owner publication** —
  `compiler/internal/check/requirement_validation.go`'s
  `validateRequirements` (06b category 5) deduplicates and publishes
  obligations per generic owner, queryable today via
  `Result.Requirements(owner)`. This function operates **only** at the
  declaration level — it has no notion of a concrete call site's actual
  type arguments.
- **Call-site generic application recording** —
  `genericApplication` (`generic_facts.go`) already captures each call
  site's substitution mapping (`site`, `generic` symbol, `parameters`,
  `arguments`, `substitutions`).
- **TIR schema readiness** — `compiler/internal/tir/node.go`'s
  `Instantiation{Site, Declaration, TypeArgs, Requirements}` and the
  `GenericFunctionValue` node kind, plus
  `compiler/internal/tir/unit.go`'s `Builder.AddInstantiation` and
  full verifier/dumper support for both, already exist and are
  exercised by the `tir` package's own totality tests. This is exactly
  the spec's specialization key shape. **Nothing in
  `compiler/internal/check/ir_builder.go` references
  `tir.GenericFunctionValue` or calls `AddInstantiation` today** — the
  schema is ready and completely unused.
- **Explicitly out of scope, not phase 7's problem**: generic anonymous
  functions and the `_` type-argument placeholder are both deliberately
  rejected today (`C0608`) per `open-language-decisions.md` §2.2/§2.3.
  Nothing about this plan proposes reopening either.

## What's actually missing

1. Checking whether a **specific call site's concrete type
   argument(s)** satisfy the generic owner's already-published
   requirements (does `i32` support `Ordered`? does `MyStruct` support
   `Equatable`?). Nothing today cross-references a call site's solved
   type arguments against `Result.Requirements(owner)`.
2. The **specialization cache**: an in-process (not persistent —
   see `09-typed-ir-and-caching.md`) map keyed by
   `(GenericSymbolID, ordered TypeIDs, ABI options)`, populated with an
   in-progress marker before recursing so structurally recursive
   generics terminate.
3. **Building monomorphized typed IR** per unique specialization —
   almost certainly by reusing `ir_builder.go`'s existing
   `buildValue`/`buildRegionBlock`/`buildRegionBlock` traversal
   machinery, parameterized by a concrete type substitution instead of
   the symbolic type-parameter identity.
4. **Wiring `GenericFunctionValue`** at the value/call sites that
   reference a generic function, populating `Instantiation` via the
   already-built `AddInstantiation`.
5. **Diagnostics** that name both the unmet requirement and the failing
   call (spec goal: "explain both the generic requirement and the call
   that failed to satisfy it") — this needs a new diagnostic code (or
   codes) distinct from 06b's `C0601`–`C0620` range; the exact code
   number is a detail to settle when this slice is written, not now.

## Rough dependency-ordered slices

These names are provisional (`07.N`, matching the `06a.N`/`06b.N`
convention) and will very likely be renumbered/resplit once the first
one or two are actually implemented — do not treat the boundaries below
as fixed the way 06b's final slice list was by the time it shipped.

### 07.1 — Call-site requirement satisfaction

Cross-check each concrete instantiation's solved type arguments against
the generic owner's already-published `Requirements` (from
`requirement_validation.go`). Likely owned files: a new
`generic_validation.go` (or extends `requirement_validation.go` — TBD
once actually written), plus its test file. This is the first checker
gap and has no TIR dependency, so it can start immediately and
independently verify against real `.peb` fixtures without needing any
of the later slices.

### 07.2 — Specialization key and in-process cache

The `(GenericSymbolID, TypeArgs, ABI)` → cache-entry map, in-progress
markers for recursion termination, and the cache's own lifecycle
(reset per compilation, not persisted). No IR construction yet — this
slice can be built and unit-tested against synthetic keys before 07.3
needs it.

### 07.3 — Monomorphized IR construction

The actual specializer: given a cache miss, build a typed-IR function
body for the concrete instantiation, reusing `ir_builder.go`'s existing
traversal parameterized by a substitution map. This is almost certainly
the largest slice in the phase, likely needing its own sub-parts the
way 06b.7b did (06b.7b needed 8 parts once real implementation started
— expect 07.3 to split similarly once work begins).

### 07.4 — `GenericFunctionValue` wiring

Connect 07.1–07.3 to real call/value sites: populate `Instantiation`
via `Builder.AddInstantiation` and emit `GenericFunctionValue` nodes
where a generic function is referenced, whether called immediately or
taken as a standalone value (`let parse_int = parse[int];`).

### 07.5 — Diagnostics

The dual-context diagnostic(s) tying a failed requirement to both its
origin (the generic body) and the failing call site. Depends on 07.1
existing; does not depend on 07.3/07.4 being done first, so it could
plausibly be pulled earlier if 07.1 lands cleanly.

### 07.6 — Full test coverage

Determinism (repeated specialization of the same key produces
byte-identical IR — same idiom as `validation_determinism_test.go`),
recursive-generic termination, cross-module sharing (the resolved
decision from this plan), and fuzz/race coverage extending
`fuzz_test.go`/`race_test.go`'s existing shape. Mirrors 06b.8's own
final slice.

## What will need sharpening once 07.1 is actually written

- The exact new diagnostic code range/numbering for generic-specific
  failures (currently 06b owns `C06xx`; 07 likely gets its own prefix
  or continues the same range — undecided, deliberately not guessed
  here).
- Whether 07.1 is a wholly new validator or an extension of
  `requirement_validation.go` — real code shape will decide this, not
  spec prose.
- The precise `ABI options` component of the specialization key
  (calling-convention variance for generic functions with `extern`
  bodies, if that combination is even legal — needs checking against
  `types.CallingConvention`'s existing scope before 07.2 is written).
- Whether 07.3 needs its own sub-slice breakdown (near-certain given
  06b.7b's precedent, exact split unknown until real body-building
  starts).

## Verification (matching 06/06b's established bar)

Each slice: `GOCACHE=/tmp/pebble-orc-gocache go test ./...`,
`GOCACHE=/tmp/pebble-orc-gocache go test -race ./...`,
`GOCACHE=/tmp/pebble-orc-gocache go vet ./...`, then repository-root
`git diff --check`. No slice edits any 01–06 phase file or file to make
implementation easier, matching every prior phase's own rule.
