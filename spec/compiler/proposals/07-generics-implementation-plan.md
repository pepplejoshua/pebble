# 07 generics — rough implementation plan

**Status:** in progress. 07.1–07.6e are implemented, committed, and
pushed (see "Completed slices" below). This document is being updated
in place as each slice lands, rather than staying a pre-implementation
sketch — treat the "Completed slices" section as authoritative fact and
everything under "Slice record and remaining work" as current record or
still-rough planning, sharpened as
each piece is actually written, the same way 06b.7b's parts each
informed the next.

**Baseline.** `main` at `5ed43ff` (07.6e, the last landed slice). 06a
and 06b are both complete (all 06a.1–06a.8 and 06b.1–06b.8 slices
accepted, plus the four Sol-flagged 06b defect fixes).

**Resolved before this plan:** the two genuinely blocking language
decisions from `spec/compiler/proposals/open-language-decisions.md`
§2.1 — always monomorphize (no erased/runtime form), and cross-module
specialization is shared and owned by the declaring module. Both are
now recorded directly in `07-generics.md` §"Specialization". Doc
display of inferred requirements remains open but does not block
implementation.

## Completed slices (implemented, verified, committed)

- **07.1 — call-site requirement satisfaction** (`generic_validation.go`,
  new `C0621`). Cross-checks each concrete instantiation's solved type
  arguments against the generic owner's published `Requirements`.
  Added `(*infer.Solution) Instantiations()` (solver had no way to
  enumerate every instantiation before this — only a single-site
  lookup existed).
- **07.2 — specialization key and in-process cache**
  (`specialization.go`): `specializationKey{Generic, TypeArgs,
  Convention}` (TypeArgs encoded as fixed-width hex, collision-free),
  `specializationCache.reserve/finish/lookup` with an in-progress state
  for recursion termination.
- **07.3a — type substitution** (`internal/types/substitute.go`):
  `Store.Substitute(id, substitutions)` rewrites `TypeParameter`
  occurrences through composite type structure. No freeze/lock on the
  store, so interning novel composite shapes mid-compilation is safe.
- **07.3b — substituted signature computation**
  (`specialization_signature.go`): `buildSpecializedSignature` pairs a
  generic's own `TypeParams` with one instantiation's solved arguments,
  substitutes the callable's symbolic parameter/result types. Needs the
  live `*types.Store`, not `handoff.Semantics.Types()` (that's the
  read-only `*types.Snapshot`, no `Substitute` method — a real gap this
  slice found and worked around by threading the store in explicitly).
- **07.3c — substitution-aware type resolution** (`ir_builder.go`):
  `irBuildState.resolveType` wraps `typeOfValue`, applying `Substitute`
  when `activeSubstitution` is set. Every internal `typeOfValue(s.records,
  X)` call site now goes through it. `activeSubstitution` is `nil` for
  every normal build (today); this was verified to introduce zero
  behavior change by re-running the entire existing
  `ir_builder_test.go` suite unchanged.
- **07.3d — isolated build scope** (`ir_builder.go`, `irBuildScope`):
  extracted the seven output-memoization fields (`functions`,
  `functionNodes`, `regions`, `values`, `placeValues`, `blockNodes`,
  `deferNodes`) that are keyed by identities repeating across every
  specialization of the same generic symbol. `withFreshScope` swaps in
  an empty scope for a build closure and restores the previous one
  after.
- **07.3e — reserve-then-complete function declarations**
  (`internal/tir/unit.go`): `ReserveFunctionDecl`/`CompleteFunctionDecl`
  let a caller get a real `FunctionID` before a body exists (mirrors
  `AddRegion`'s identity-first allocation) — needed because a body's own
  `Return` nodes must reference their function's ID while still being
  built, and the existing `AddFunctionDecl` only allocates one after the
  body is done.
- **07.3f — build one specialization end-to-end**
  (`specialization_build.go`): `buildSpecialization` ties 07.2–07.3e
  together to build one real, verifier-clean, substituted function for
  one instantiation. **Found and fixed a real bug**: rebuilding a
  generic's body reuses the existing `buildRegionBlock`/`buildValue`
  traversal, which always maps built nodes back to their authored
  syntax ref via `MapSource` — but the same body was already built once
  symbolically by the normal build pass, which already claimed every
  one of those refs. Fix: `addNode` now suppresses both the node's own
  `Syntax` field and the `MapSource` call whenever a specialization is
  being built (confirmed via the typed-IR verifier's own symmetric
  invariant: a nonzero `node.Syntax` must always have a matching
  `SourceMap` entry pointing back to it — an asymmetric fix that only
  skipped `MapSource` was tried first and correctly rejected by the
  verifier). **Also found a separate, pre-existing, unrelated bug**:
  expression-bodied functions (`=> expr;`) lower to an empty `Block`
  with no children — confirmed identical for an ordinary non-generic
  function through the completely normal pipeline. Not fixed as part of
  this phase; noted here so it isn't rediscovered by surprise. Test
  fixtures in this phase use block bodies (`{ return expr; }`) to avoid
  it.
- **07.4a — wire specialization into the normal build**
  (`ir_builder.go`): `buildSpecializations` is the final step in
  `buildUnit`'s pipeline, walking every real instantiation
  (`handoff.Solution.Instantiations()`) and triggering a build for each.
  A specialization's own `FunctionDeclaration` node now carries its
  `TypeArgs` (added to `tir/verify.go`'s allowed-field list for this
  node kind), matching the `(Symbol, TypeArgs, Convention)` triple that
  `DirectCall` nodes at generic call sites already carry — so a
  consumer can find the one specialization matching a given call site
  among however many `FunctionDeclaration` entries share that symbol
  (the symbolic declaration's own `TypeArgs` stays empty). Verified
  against the entire existing generic test corpus (07.1's own tests,
  `generic_*.peb` fixtures, requirement-publication tests) to confirm
  no regressions from turning this on.
- **07.4b — named and generic function values** (`ir_builder.go`):
  confirmed empirically that a bare `identity[i32]` value reference
  publishes an instantiation at its bracket syntax reference and is retained
  as `expressionBracket`. Named non-generic function references now emit
  `HoistedFunctionValue` nodes. Bare generic function references now build the
  matching specialization, add a typed-IR `Instantiation`, and emit a
  verifier-clean `GenericFunctionValue` with concrete `TypeArgs`. Function
  declarations now reserve their final IDs before body construction so a
  specialization built during block traversal cannot shift normal function
  IDs. Added block-bodied IR tests for both paths. Full tests, race tests,
  vet, build, and diff checks pass.
- **07.5 — generic instantiation diagnostics** (`generic_validation.go`):
  kept `C0621` as one deterministic diagnostic per failed instantiation,
  retained the concrete call or bare generic-value site as the primary span,
  and added a related label for the generic-body requirement origin. Bare
  generic function values now receive a real bracket-site span. Added exact
  call-site and bare-value diagnostics tests. Full tests, race tests, vet,
  build, and diff checks pass.
- **07.6a — recursive specialization termination**
  (`specialization_build.go`): publishes the specialized
  `FunctionDeclaration` node and cache declaration reference before building
  the body. Same-key re-entry now returns that stable declaration instead of
  failing with `(0, false)`. Added a block-bodied recursive generic
  function-value test that inspects the verified body, instantiation, and
  single specialization declaration. Full tests, race tests, vet, build, and
  diff checks pass.
- **07.6b — generic typed-IR determinism** (`determinism_test.go`): repeated
  full-pipeline checks of a valid generic program now compare the complete
  canonical typed-IR dump, including specialization declarations,
  instantiation ordering, node IDs, concrete `TypeArgs`, and source maps.
  The fixture covers distinct and repeated generic instantiations, bare
  generic function values, and multiple specialization declarations. Full
  tests, race tests, vet, build, and diff checks pass.
- **07.6c — cross-module generic sharing** (`cross_module_generic_test.go`):
  a two-module fixture now proves that an imported generic keeps its declaring
  module ownership, all consumer sites target the imported symbol, and each
  distinct specialization key produces one shared declaration and one
  consistent function identity. Repeated same-key requests do not duplicate
  declarations or instantiation entries. Full tests, race tests, vet, build,
  and diff checks pass.
- **07.6d — generic fuzz and race seeds** (`generic_specialization.peb`,
  `race_test.go`): the existing `FuzzCheck` and `FuzzBuildUnit` corpora now
  include a small valid generic program, and race coverage now reads a
  published generic result and specialized TIR concurrently. No fuzz harness
  or production code changed. Full tests, fuzz runs, race tests, vet, build,
  and diff checks pass.
- **07.6e — generic method-call correlation** (`ir_builder.go`): generic
  method calls now copy their solved concrete type arguments into
  `tir.MethodCall`, matching the specialization key carried by direct calls
  and declarations. Added a focused inferred-generic-method IR test. Full
  tests, race tests, vet, build, and diff checks pass.

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
  the spec's specialization key shape. 07.4b now consumes this schema from
  `ir_builder.go` for bare generic function values.
- **Explicitly out of scope, not phase 7's problem**: generic anonymous
  functions and the `_` type-argument placeholder are both deliberately
  rejected today (`C0608`) per `open-language-decisions.md` §2.2/§2.3.
  Nothing about this plan proposes reopening either.

## What's actually missing

Items 1–4 (call-site requirement satisfaction, the specialization
cache, building monomorphized typed IR, and generic diagnostics) are done —
see "Completed slices" above. What remains is full generics coverage.

## Slice record and remaining work

### 07.4b — `GenericFunctionValue` wiring (completed; investigation record)

The investigation and implementation are complete. The details below record
the evidence that closed the handoff and the exact implementation boundary.

**Scope decision (already made — do not re-ask):** the user chose to
fix this together with the underlying "named function as bare value"
gap, rather than scoping around it. Extend `buildSymbolValue`
(`compiler/internal/check/ir_builder.go`) to handle
`symbol.SymbolFunction`-kind symbols:
- **Non-generic case**: build an ordinary function-value node (reusing
  `s.functions[symbol]`, the same identity `HoistedFunctionValue`
  already uses for anonymous literals) — this is the missing, more
  general fix; generics build on top of it.
- **Generic case with explicit type arguments**: trigger
  `buildSpecialization` for the resolved instantiation and emit a real
  `tir.GenericFunctionValue` node (`Symbol`, `GenericRef` via
  `Builder.AddInstantiation`, `TypeArgs` — the schema has existed,
  fully verified/dumped, since before 07 started; 07.4b now consumes it).

**Why this is bigger than originally scoped**: `buildSymbolValue`'s
switch (`internal/check/ir_builder.go`, case list starting
`symbol.SymbolBinding, symbol.SymbolParameter, ...`) had no case for
`symbol.SymbolFunction` — it fell through to `default: return false`. The
07.4b implementation now adds this path. The only older path that builds a
function-*value* node is
`case expressionFunction:` → `tir.HoistedFunctionValue`
(`ir_builder.go`), but `expressionFunction` is retained **only** for an
inline anonymous function literal term — confirmed directly:
`compiler/internal/check/expression_facts.go`'s `retainExistingExpression`
(~line 499) sets `kind = expressionFunction` only when `node.Kind() ==
syntax.FunctionTerm`. A reference to an already-declared *named*
function (generic or not) is a different syntax node entirely and never
took this path. This was the pre-existing 06b.7b-era gap closed by 07.4b.

**Investigation trail so far (real file/line evidence, to save
re-deriving it):**

1. For `identity[i32]` used bare (not called), `04b`/`05b`'s resolver
   classifies the bracket as `symbol.BracketTypeNames` mode
   (`compiler/internal/check/bracket_facts.go`'s `prepareBracket`,
   ~line 84). Confirmed this path is reached for a generic *function*
   base (not just generic types): `w.genericIdentity(p.base, tree)`
   resolves the base symbol regardless of whether it names a type or a
   function; the `case symbol.BracketTypeNames:` branch (~line 84–99)
   only calls `w.mirrorTypeInstantiation` when `w.program.TypeDeclaration(p.generic)`
   succeeds (i.e. `p.generic` really is a type) — otherwise it falls to
   `w.prepareGeneric(ref, p.generic, p.arguments, ctx)` directly, which
   is exactly the function case.
2. `prepareGeneric` (`generic_facts.go`, already used by 07.1's own
   investigation) calls `w.publishInstantiation(site, generic, terms)`
   regardless of whether the call site is an actual call or a bare
   value reference — meaning `handoff.Solution.Instantiation(ref)`
   (07.1's accessor) is queryable for a bare `identity[i32]` reference
   site, not just for a call. A throwaway block-bodied fixture confirmed
   this before implementation.
3. `finishBracket` (`bracket_facts.go`, ~line 186) for `p.mode ==
   symbol.BracketTypeNames` calls
   `signature, ok := w.program.Signature(p.generic)` then
   `w.instantiateSignature(signature, p.application, origin)` then
   `w.retainBracket(ref, ctx, result, 0, alternativeTag{}, nil)`.
   `retainBracket` retains `expressionBracket` with the bracket result and no
   children, symbol, or specialization. This is the exact record shape used
   by the new `buildValueBase` dispatch.
4. `bracket_facts.go`'s `prepareDeferredBracket`/`finishDeferredBracket`
   (~line 129–177, 244+) is a **different**, more specific case:
   generic *method* application with explicit brackets on a member
   expression (`value.map[str](convert)` from the spec's own example,
   `baseNode.Kind() == syntax.MemberExpr` is required, returns early
   otherwise). Confirmed this does **not** apply to a plain-name base
   like `identity[i32]` — don't conflate the two paths.

The implementation then added the named-function path and the generic
function-value path together, with block-bodied IR tests that inspect the
emitted nodes, instantiation table, and specialized declaration.

### 07.5 — Diagnostics (completed; implementation record)

`C0621` now ties a failed requirement to both contexts: the concrete call or
bare generic-value site is the primary label, and the normalized requirement's
retained generic-body origin is a related label. The implementation keeps
deterministic one-diagnostic-per-instantiation behavior and covers both call
and bare-value paths with exact source-span tests.

### 07.6 — Full test coverage

07.6 is split into small slices. 07.6a–07.6e are complete. The full
generics-coverage slice is now closed.

- **07.7 — `RequirementLiteralFits` checking** (`generic_validation.go`,
  `infer/literal.go`, `infer/capability.go`, `infer/semantic_snapshot.go`):
  a generic call whose literal argument doesn't fit the resolved
  concrete type now fails `C0621` instead of silently passing. Reuses
  the existing exact-literal bounds math (`integerFits`/`floatFits`)
  via a new session-free `infer.LiteralFitsBuiltin`, exposed through a
  new `LiteralTarget()` accessor on `SemanticSnapshot`. Covers both
  call sites and bare generic-value sites, with span/related-label
  tests matching 07.5's shape.

Deferred and outside 07.6: expression-bodied functions still have the
pre-existing empty-block lowering bug recorded under 07.3f.

## What needed sharpening, resolved during 07.1–07.5

These were open questions in the original pre-implementation draft;
kept here (rather than deleted) as a record of how they actually
resolved, since real code shape decided all of them, not spec prose:

- Diagnostic code: 07.1 added `CodeGenericInstantiation = "C0621"` —
  continues 06b's `C06xx` range, no new prefix needed.
- 07.1 is a wholly new file (`generic_validation.go`), not an
  extension of `requirement_validation.go` — it reuses predicates
  extracted out of `operator_validation.go` instead.
- The specialization key's third component is `Convention
  types.CallingConvention`, not a separate "ABI options" concept —
  `specializationKey` in `specialization.go` uses the type directly.
- 07.3 did need its own sub-slice breakdown, same as 06b.7b: it split
  into 07.3a–07.3f (six parts), one more than originally guessed.

Still genuinely open (not yet resolved by real code):

- Whether 07.3's `ABI`/`Convention` handling needs anything different
  once 07.4b makes generic functions referenceable as bare values
  (today only call sites are exercised) — not yet investigated, flag
  it if 07.4b's implementation surfaces anything unexpected here.

## How to continue this work

This section is for whoever (or whichever tool/session — Codex or
otherwise) picks this up next, without the benefit of this
conversation's history.

### Where things stand

Read "Completed slices" above for exactly what's built and verified.
07.4b, 07.5, and 07.6a–07.6e are complete. The full 07.6 generics-coverage
slice is closed. Future work should start from a new phase or a separately
approved follow-up slice.

### Using `orc` to dispatch implementation work

This phase's slices (07.1–07.6e) were each implemented by dispatching
a tightly-scoped brief to `orc`, a supervisor CLI that runs an
OpenCode worker model against this repository and blocks until it
finishes:

```bash
orc run --codex --model opencode-go/deepseek-v4-flash --prompt-file /tmp/orc_task_<name>.md "<short summary>"
```

Always pass `--codex` (attribution for this assistant's own
dispatches). Run it with a background-capable tool so you can keep
working while it completes — `orc run` blocks until the worker exits,
which can take several minutes for a substantial slice.

**Model policy, as actually used across this phase:**

- Default and only approved model: `opencode-go/deepseek-v4-flash`
  ("flash"). The user's current instruction is explicit: use Flash only
  for Orc dispatches in this phase. If a dispatch is silent or weak, retry
  the same Flash brief once; do not escalate to another model without a new
  explicit user instruction.
- **`opencode-go/kimi-k2.7-code` ("kimi") is permanently banned in this
  project** — it contributed to blowing OpenCode usage limits in an
  earlier phase. Never dispatch to it here, regardless of what the
  general `dispatch-orc-task` skill's own model-tiering guidance says.
- Do not use Luna, Kimi, Sol, or Terra for this phase.

**Checking a dispatch's outcome:**

```bash
orc result <session>          # JSON: status, response summary, metrics
```

then read the worklog file path it returns
(`~/.orc/sessions/<session>/worklog.md`) for the worker's own
running account of what it did and the verification output it ran.

### Brief-writing conventions that worked well this phase

Every successful brief in this phase (see `/tmp/orc_task_*.md`
filenames referenced in worklogs, e.g.
`07_4a_wire_specialization_pipeline.md`) followed this shape — keep it
when writing the next one:

- **State exactly what's already confirmed**, with real file/line
  citations from direct investigation (not guesses) — e.g. "already
  confirmed for you: the correlation gap," quoting the actual existing
  code. This prevents the worker from re-deriving (or mis-deriving)
  context you already have.
- **Give an explicit "Files you may modify" allow-list**, and an
  explicit "Do not modify" list naming specific other files/packages
  that are off-limits. Every brief this phase used this and it
  reliably kept dispatches from drifting into unrelated files.
- **Require exact verification commands**, always prefixed
  `GOCACHE=/tmp/pebble-orc-gocache` (this repo's Go module needs this
  to avoid cache contention across concurrent workers), and demand
  *literal pasted output*, not a summarized "tests pass" claim — e.g.
  "actually run this yourself and paste the complete output." This
  matters: worklog claims have twice in this phase alone been proven
  wrong or misleadingly weak once independently re-verified (see next
  section).
- **"Do not commit"**: every implementation-only brief this phase
  explicitly instructed the worker to leave changes uncommitted
  (`Do not run git commit, git add, git push...`) — the supervisor
  (you) reviews and commits after independent verification, never the
  worker.
- **Worklog-as-you-go**: instruct `orc worklog append <session> "..."`
  during the work, not just a final summary — this is what makes a
  stalled/confused dispatch diagnosable via `orc result`'s `last_event`
  field instead of a black box.

### Independent verification discipline — the single most important habit

**A worklog's "tests pass" claim is not sufficient evidence, even when
technically true.** This phase caught two real problems this way that
would otherwise have shipped:

1. Two dispatch attempts (07.1's first, 07.3f's first two) did
   essentially nothing despite returning a "completed" status —
   caught only by checking `orc result`'s `metrics.tool_calls`/
   `metrics.cpu_seconds` for near-zero values, not by reading the
   response text.
2. 07.3f's delivered tests technically passed, but two of three used
   an expression-bodied (`=>`) fixture that (confirmed by writing a
   throwaway scratch test) lowers to an empty typed-IR `Block` with
   zero children — meaning the tests exercised no real body content at
   all. Rewriting the fixture to use block-body syntax exposed a
   genuine bug (a `MapSource`/`SourceMap` symmetry violation) the weak
   tests never caught.

**Concretely, before accepting any dispatch's result:**

- Rebuild and rerun the full verification suite yourself
  (`gofmt -l .`, `go vet ./...`, `go build ./...`, `go test ./...
  -count=1`, `go test -race -count=1 ./...`) — don't just trust the
  worklog's pasted output, actually run it again.
- Read the actual diff (`git diff --stat` then the real files), not
  just the worklog's prose description of it.
- If something feels underexercised even after tests pass — a fixture
  that seems too simple, a code path you can't see directly tested —
  write a throwaway scratch probe yourself: either a small `_test.go`
  with a body-inspection assertion, or raw `fmt.Println` debug
  instrumentation added directly into the file under investigation
  (back it up first with `cp file.go /tmp/file.go.bak` so it's trivial
  to cleanly restore). Delete/restore the scratch instrumentation
  before finalizing, and confirm via `git diff` that the restored file
  exactly matches the pre-investigation state — no debug artifacts
  should leak into what gets committed.
- If a dispatch's own fix attempt gets rejected by an existing
  invariant (as happened with 07.3f's first fix attempt hitting
  `verifySourceMap`'s symmetry check) and it reports this honestly
  instead of forcing something through, that is a *good* sign about
  the worker's reliability — treat the honest failure report as
  useful signal, dispatch a corrected follow-up brief that names the
  specific invariant that was violated, rather than treating it as a
  wasted attempt.

## Verification (matching 06/06b's established bar)

Each slice: `GOCACHE=/tmp/pebble-orc-gocache go test ./...`,
`GOCACHE=/tmp/pebble-orc-gocache go test -race ./...`,
`GOCACHE=/tmp/pebble-orc-gocache go vet ./...`, then repository-root
`git diff --check`. No slice edits any 01–06 phase file or file to make
implementation easier, matching every prior phase's own rule.
