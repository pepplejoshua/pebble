# 06b implementation-readiness audit

**Status:** Audit report. Not a spec amendment. Nothing here changes an
accepted document; where a change is needed this report says what it is and
stops.

**Revision note.** This report was independently reviewed after its first
draft. The review confirmed F1, F5, and most of the hidden-integration-site
sweep, and made four corrections now folded into the sections below: F4 is
substantially worse than originally characterized (a missing *representation*
in `06a`'s handoff, not missing prose — see its section), the recommended
implementation order cannot compile as originally written (06b.1's `Result`
type references `tir.NodeID`/`tir.Unit` directly, so `06b.7a`'s identity types
must exist first, not merely "early for efficiency"), 06b.5c is not fully
independent of 06b.5a, and two items in the hidden-integration-site sweep had
a wrong count. The reviewer also settled the three questions the first draft
declined to answer. All four corrections and all three decisions are recorded
in place, not appended separately, so this document reflects the corrected
understanding throughout.

**Second revision.** F1 landed, verified, and is marked resolved throughout
(commit `0f6125f`) rather than deleted, since its analysis is the same
failure shape F4 turned out to share. F4's own proposal
(`06a-structural-composition.md`) was separately corrected on review: it did
not originally account for `BindingDecl`, which retains no control record at
all, breaking both `for`-initializer-as-binding resolution and ordinary
local-binding sequencing; and its validation contract did not distinguish
per-record checks (at `06a.7` record-append time) from the cross-record
check that a `Composition` arm actually resolves to a retained record (which
only `06a.8`'s freeze-time audit can prove, mirroring the existing
`controlFunction`-per-root check). Both are corrected in that file; this
document's F4 references are updated to match.

**Scope.** For each of the eight slices named in
`spec/compiler/06b-validation-and-typed-ir.md` §"Dependency-ordered
implementation slices", answer one question: *does this slice's specification
completely and unambiguously determine everything an implementer needs, with
no invented API and no reopened design?*

**Why this exists.** `06a` was implemented against a spec that had already
passed review, and two gaps still surfaced during implementation rather than
before it:

1. tuple component access (`pair.0`) had no `05b` constraint to solve it; the
   accepted spec simply never defined one. Fixed by
   `proposals/05b-tuple-component-constraint.md` and the `HasComponent`
   delayed structural constraint.
2. that amendment, though correct in prose, never named the two dispatch sites
   it had to be added to — `infer/session.go`'s constraint-validation switch
   and `infer/solve.go`'s literal-defaulting-block switch. They were found by
   reading the solver's dispatch code line by line.

Both were **silent**: nothing failed to compile, no existing test caught them,
and one sat in committed code across multiple slices before surfacing. This
audit therefore treats "the spec reads fine" as insufficient evidence, and
verifies every accessor and every dispatch site against the real code.

**Baseline.** `main` at `0f6125f`, clean tree (originally audited at
`a754b39`; `0f6125f` lands F1 — see "Second revision" note above — with no
change to any other finding in this document). `06a.1`–`06a.8` accepted,
plus the `HasComponent` amendment and F1. `06b` not started: `ls
compiler/internal/check/`
confirms none of `validation.go`, `solved_record.go`, `result.go`,
`check.go`, `compatibility.go`, `operator_validation.go`, `call_validation.go`,
`member_validation.go`, `aggregate_validation.go`, `index_validation.go`,
`context_validation.go`, `place_validation.go`, `assignment_validation.go`,
`requirement_validation.go`, `global_validation.go`, `entry_validation.go`,
`control_validation.go`, `switch_validation.go`, `defer_validation.go`, or
`ir_builder.go` exists, and `ls compiler/internal/` confirms there is no `tir`
package. No 06b-owned filename is currently occupied by unrelated content.

**Evidence rule.** Every claim below about what exists cites a file and line
that was actually read. Claims sourced only from spec prose are marked as
such.

---

## Headline findings

Four findings are material. The rest are noted in their slice sections.

| # | Finding | Class | Slice |
| --- | --- | --- | --- |
| **F1** | ~~Tuple component access on a **rigid type parameter** produces no diagnostic anywhere~~ — **RESOLVED**, commit `0f6125f` ("compiler: retain unsupported tuple component requirements"). `requirementUnsupportedComponent` added to `check.requirementKind`, retained in `member_facts.go` for `memberTuple`, `record.go`'s freeze range extended, `06a`/`06b` prose updated. Full suite, `-race`, `vet`, `git diff --check` all passed at landing. Kept here as historical evidence of the failure shape, since it is the same shape F4 turned out to be. | Was: silent gap, exact repeat of the `HasComponent` integration-site failure mode. Now: closed. | 06b.5 (fix landed in `06a` `member_facts.go`) |
| **F2** | `requirementLiteralFits` is declared in `check` but **never constructed anywhere**, while `05b` does produce `RequirementLiteralFits`. 06b's stated join rule has no left-hand side for literal-fit. **Decided** (see 06b.5 section): publish directly from `Solution.Requirements()`, no `06a`-side record. | Gap, now decided | 06b.5 |
| **F3** | 06b's requirement join key includes `origin`, but `05b`'s requirement table dedupes on `(Owner, Kind, Subject)` and keeps only the **earliest** origin. Per-use-site `06a` records that are not the earliest cannot join on origin. | Contradiction between accepted spec text and accepted code | 06b.5 |
| **F4** | `06a`'s handoff has **no representation at all** for which arm of an `if`/loop a leaf statement is, when that arm is not itself a block — the same gap also applies to `BindingDecl`, which retains no control record at all, breaking `for`-initializer resolution and ordinary local-binding sequencing. `if`, `while`, `for`, range-loop, and switch-case all stamp every non-block clause (then, else, init, update, body) into one shared region with no role tag and no child region to index into. This is not an unwritten mapping over an existing ordinal — the ordinal does not exist for that case. All of 06b.6's flow analysis rests on recovering this. | **Missing representation in the accepted `06a.7` data model**, not a documentation gap. Reopens already-accepted `control_facts.go` and `record.go` (06a.7), plus `solve_handoff.go` (06a.8, for the cross-record resolution check). Must be fixed before any 06b slice starts, not just before 06b.6. | 06b.6, but the fix lands in `06a` |

Plus one architectural gap (**F5**, 06b.3: "the instantiated descriptor" is not
constructible post-solve) and two mechanical drifts (**D1**, `frozenConstants`'
accessor name and return type; **D2**, `Requirement.Operator token.Kind` naming
a package that does not exist).

---

## Slice 06b.1 — handoff resolution, lifecycle, and result shell

**Owned files** (copied from the spec):

```text
compiler/internal/check/validation.go
compiler/internal/check/solved_record.go
compiler/internal/check/result.go
compiler/internal/check/config.go (06b field extension only)
compiler/internal/check/validation_test.go
compiler/internal/check/result_test.go
compiler/internal/check/validation_repository_test.go
tests/check/validation/invalid/C0619/*.peb
tests/check/validation/recovery/handoff_*.peb
```

### What it needs from upstream — confirmed against real code

| 06b spec assumes | Real code | Verdict |
| --- | --- | --- |
| `solveHandoff{Compilation, Semantics, Solution, Records, Roots, Constants, GenerationHadErrors}` | `check/solve_handoff.go:46-54` | exact match, field for field |
| `frozenCompilation{Root, Modules, DependencyOrder, Sources}` and its `frozenModule`/`frozenImport`/`frozenSource` members | `check/solve_handoff.go:15-43` | exact match |
| `frozenRoots.All() []rootedValue`, `frozenRoots.Root(valueID) (valueRoot, bool)` | `check/root.go:97-101` | exact match; `All` clones (`cloneRootedValues`, `root.go:89`) |
| `rootedValue` carries no `infer.Term` / `InferID` | `check/root.go:39-51` — `valueRoot` holds only `Kind`, `Syntax`, `Symbol`, `Slot`, `Parameter`, `Alternative` | confirmed |
| `frozenRecords.Records()`, `frozenRecords.Controls()` | `check/record.go:560-574` | exact match (both deep-clone); `Components()` also exists, unmentioned and harmless |
| `Solution.SyntaxType`, `.SymbolType`, `.Instantiation`, `.Method`, `.Slot`, `.Selection`, `.Successful`, `.Requirements` | `infer/solution.go:116-208` | **all present with the assumed signatures.** `Selection` returns `(uint32, bool)`; the spec's shorthand `Solution.Selection(Choice) == Index` (06b line 205) is prose, not a signature claim |
| `Instantiation.Arguments` / `MethodSelection.Arguments` indexable by a zero-based ordinal | `infer/solution.go:60-69`, both `[]TypeResult`, both defensively copied on access (`:191`, `:199`) | confirmed |
| `Semantics.Matches(Solution)`, `.Types()`, `.Resolution()`, `.TypeDeclaration(s)`, `.Signature(s)`, `.Template(s)`, `.RuntimeTypes()`, `.TypeParameter`, `.OwnerParameters` | `infer/semantic_snapshot.go:91-186` | all nine present |
| `Semantics.Types().Key` / `.Kind` / `.Builtins` / `.Contains` | `types/snapshot.go:102-139` | all four present, plus `Len`/`IDs` |
| failed-shell path: `Semantics == nil`, `GenerationHadErrors == true`, existing `T0512` | `check/solve_handoff.go:61,65,72,78,84,92,97` — every failure path returns `&solveHandoff{GenerationHadErrors: true}` with all other fields zero | confirmed |

The root-resolution table (06b lines 180-187) is satisfiable: `valueRoot.Kind`
covers `rootSyntax`/`rootSymbol`/`rootInstantiation`/`rootMethod`/`rootSlot`
(`check/root.go:31-37`), the instantiation/method *site* is `valueRoot.Syntax`,
and the ordinal is `valueRoot.Parameter`. `alternativeTag{Choice, Index,
Guarded}` with its `valid()` predicate (`check/root.go:16-27`) supports the
guarded-alternative rule exactly as written.

### Drift D1 — `frozenConstants` accessor does not exist as specified

06b line 172-173 declares:

```go
type frozenConstants struct { /* private ordered memoized values */ }
func (c frozenConstants) Value(symbol.SyntaxRef) (constantValue, bool)
```

The real type (`check/constant.go:994-1017`) is:

```go
type frozenConstants struct{ values []frozenConstant }
func (f frozenConstants) All() []frozenConstant
func (f frozenConstants) Constant(ref symbol.SyntaxRef) (constantResult, bool)
```

Three differences: the method is named `Constant`, not `Value`; it returns
`constantResult` (which wraps `constantValue` with `State` — one of
`constantKnown` / `constantError` / `constantUnavailable`, `constant.go:46-58`
— and an unexported `contextual` flag), not a bare `constantValue`; and `All()`
is not mentioned in 06b at all.

This is **mechanical**, not a design question. `constantResult.State` is
strictly more information than `constantValue` and 06b needs it: 06b's global
rule ("a nonconstant global initializer emits `C0616`", line 723) is precisely
the `constantUnavailable` case, which a bare `constantValue` cannot express.
The smallest amendment is to correct 06b's §"Frozen access required by
validation" block to the real signatures and to say that
`State == constantKnown` is the precondition for every constant-dependent 06b
rule (switch duplicate detection, `C0616`, and constant-source enum
conversion). No accepted behaviour changes.

### Note N1 — both keyed frozen accessors are linear scans

`frozenRoots.Root` falls back to a full scan when the arena's index map is
absent, which it always is after freeze (`check/root.go:80-86`: the comment
states "Frozen roots do not need to retain the mutable arena's index"), and
`frozenConstants.Constant` is an unconditional linear scan
(`check/constant.go:1010-1016`). 06b calls `Root(valueID)` once per value on
every record. That is O(records x values).

This is not a spec gap and not a correctness problem — it is a resourcing one.
06b's own §"Recovery, limits, fuzzing, and race safety" says "All record, root,
type-key, ... work is charged", and the default `MaxValidationSteps` is
16,777,216. A large compilation would exhaust that budget on scan steps alone
if each probe is charged honestly, or hide a quadratic if it is not.
06b.1 should build its `solvedRecords` arena with a single ordered pass over
`Roots.All()` (which the spec already permits: "`All` is used once for bounded
completeness/duplicate auditing") and index it locally, rather than calling
`Root` per lookup. Worth stating explicitly in the slice handoff so it is a
decision rather than an accident.

### Readiness verdict — **Ready for direct implementation**

Every type and accessor the slice consumes exists as named. D1 is a two-line
correction to a code block, not a design decision, and can be applied as part
of the slice's own handoff rather than blocking it. N1 is an implementation
note.

### Finer decomposition

**Existing granularity is right.** The three source files are one unit: you
cannot resolve roots (`validation.go`) without somewhere to put them
(`solved_record.go`), and the `Result` shell's failed-but-queryable contract
(06b lines 253-304) is what proves the resolution failed safely. Splitting
would produce a slice whose completion bar cannot be demonstrated.

---

## Slice 06b.2 — primitive/composite compatibility and operators

**Owned files:**

```text
compiler/internal/check/compatibility.go
compiler/internal/check/operator_validation.go
compiler/internal/check/compatibility_test.go
compiler/internal/check/operator_validation_test.go
tests/check/validation/valid/conversion_*.peb
tests/check/validation/valid/operator_*.peb
tests/check/validation/invalid/C0601/conversion_*.peb
tests/check/validation/invalid/C0603/*.peb
```

### What it needs from upstream — confirmed

- `compatibilityRecord` with `Source`, `Destination`, `Role` in
  `compatibilityAssignment..compatibilityBranch`, and `castRecord` with
  `Source`/`Destination`/`Result` — both validated in
  `check/record.go:290-297` and `:317-324`.
- `operatorRecord{Header, Form, Family, Token, Operands, Result, GenericOwner}`
  — `check/operator_facts.go:34-42`. `operatorForm` is
  prefix/postfix/binary (`:10-16`); `operatorFamily` is the closed 12-member
  set at `:18-32` (`operatorLiteralNegate`, `operatorNumericSame`,
  `operatorAdd`, `operatorIntegralSame`, `operatorShift`, `operatorBoolean`,
  `operatorOrdering`, `operatorEquality`, `operatorAddress`,
  `operatorDereference`, `operatorOptionalForce`, `operatorMutation`). Every
  row of 06b's operator table (lines 449-460) maps onto one of these families
  plus the exact `Token`.
- `types.TypeKey` decomposition: `Kind()`, `Builtin()`, `Child()`, `Array()`,
  `Elements()`, `Function()`, `Nominal()`, `TypeParameter()` —
  `types/key.go:103-164`. `types.Kind` is the closed 9-member set at
  `key.go:12-22`; `BuiltinKind` is the closed 16-member set at `:27-44`.
  Every cell of both matrices is decidable from these.
- Struct / union / tagged-union / enum / extern discrimination is **not** in
  `types.Kind` — all five are `types.Nominal` with a declaration `SymbolID`.
  The discriminator is `infer.NominalKind` (`infer/program.go:56-64`:
  `NominalStruct`, `NominalUnion`, `NominalTaggedUnion`, `NominalEnum`,
  `NominalExtern`) reached through `Semantics.TypeDeclaration(decl).Nominal`.
  06b's audit table (line 241) already routes declarations through
  `Semantics.TypeDeclaration`, so this is available, but 06b's matrix prose
  says "enum" and "union" as if they were type kinds. Implementers should know
  the two-step is required; it is not a gap, just non-obvious.

### Enum/integer rows — confirmed applied, no drift

`proposals/enum-integer-conversion.md` is marked **"Accepted and applied"**
(line 3), and 06b's composite matrix does carry the three rows:

- `enum -> concrete integer` — explicit, total, declaration ordinal (06b:404)
- `concrete integer -> ?enum` — explicit, checked, `some`/`none` (06b:405)
- `concrete integer -> enum` — explicit, asserted, runtime fault (06b:406)

plus the tagged-union exclusion and the constant-folding rule at 06b:425-440.
The IR nodes `EnumToInteger`, `OptionalIntegerToEnum`, `CheckedIntegerToEnum`
are in the coercion inventory (06b:966). **No drift.** Constant folding is
supported: `constantValue` carries `Kind == constantEnum` with `EnumType`,
`EnumVariant`, and `EnumOrdinal` (`check/constant.go:20-37`).

`proposals/open-language-decisions.md` §1.4 claims these two documents
disagree and that the matrix says "forbidden". That entry is now **stale** —
it describes the tree before commit `9fcc35f`. Same for §1.1 (tuple access),
which landed as `HasComponent` in `a754b39`. Both entries should be retired
from that inventory; neither blocks anything.

### Note N2 — `TypeDeclaration.Members` is a filtered list

`infer/declaration.go:111-130` builds `TypeDeclaration.Members` by skipping any
member where `!ok || member.Error` (`:118`) or where the member's template
resolves to `0` (`:124`). So `Members` is **not** guaranteed to be the
complete, gap-free declaration-order list of an enum's variants.

06b's enum-to-integer rule says "the variant's zero-based declaration ordinal".
If that ordinal is read as an index into a filtered `Members`, a single
erroring variant silently renumbers every variant after it. The complete list
is `Semantics.Resolution().Members(decl)` (`symbol/symbol.go:363-365`), which
is what `declaration.go:116` itself iterates before filtering.

In practice the blast radius is bounded — a declaration with an erroring member
produces an error compilation, and 06b publishes no IR when errors exist (06b
line 297, gate condition 6). But the same filtered list also drives **switch
exhaustiveness** in 06b.6 ("exhaustive when every variant or `else` exists",
06b:783-785), where accepting a non-exhaustive switch is a wrong *acceptance*,
not just a wrong ordinal. This deserves one explicit sentence in 06b naming
which list is authoritative for ordinals and for exhaustiveness. It is a
one-line clarification, not a design question.

### Readiness verdict — **Ready for direct implementation**

Both matrices are fully closed, every discriminator exists, and both landed
proposals are consistently reflected. N2 is a clarification the slice can raise
in its own handoff.

### Finer decomposition

**Existing granularity is right, marginally.** Compatibility and operators are
formally separable — no operator inserts a coercion (06b:462), which is exactly
the property that makes them independent. But the slice's stated completion bar
*is* that independence ("no operator inserts a coercion"), and it is only
demonstrable with both halves present. Keeping them in one slice keeps that
bar checkable. No split.

---

## Slice 06b.3 — calls, context, members, aggregates, brackets, and bounds

**Owned files:**

```text
compiler/internal/check/call_validation.go
compiler/internal/check/member_validation.go
compiler/internal/check/aggregate_validation.go
compiler/internal/check/index_validation.go
compiler/internal/check/context_validation.go
compiler/internal/check/call_validation_test.go
compiler/internal/check/member_validation_test.go
compiler/internal/check/aggregate_validation_test.go
compiler/internal/check/index_validation_test.go
compiler/internal/check/context_validation_test.go
tests/check/validation/valid/call_*.peb
tests/check/validation/valid/aggregate_*.peb
tests/check/validation/valid/index_*.peb
tests/check/validation/valid/callable_*.peb
tests/check/validation/invalid/C0601/call_*.peb
tests/check/validation/invalid/C0601/field_*.peb
tests/check/validation/invalid/C0604/*.peb
tests/check/validation/invalid/C0605/*.peb
tests/check/validation/invalid/C0608/*.peb
tests/check/validation/invalid/C0609/*.peb
tests/check/validation/invalid/C0617/*.peb
```

### What it needs from upstream — confirmed

- `callRecord` / `callTarget` / `callArgument`, with the direct/indirect/
  method/variant discrimination and the `ConventionKnown`, `Variadic`,
  `FixedCount`, `Site`, `Receiver` invariants — enforced at
  `check/record.go:298-316`, which is a usefully strict pre-image of 06b's own
  call rules.
- `memberRecord{Header, Kind, Base, Result, Member, Name, NameSpan,
  TupleOrdinal}` — `check/member_facts.go:133`, validated at
  `check/record.go:361-369`. `memberStatic`/`memberField`/`memberTuple`/
  `memberMethod`/`memberVariant` all present.
- `aggregateRecord` with ordered `Fields []fieldValue` carrying
  `Field`/`NameSyntax`/`Value`/`Destination`, plus `DeclarationFields` —
  `check/record.go:275-289`.
- `indexRecord` with `Mode` in `indexValue..indexSlice`, `Base`, `Start`,
  `End`, `Result`, `StartPresent`/`EndPresent`, `HasKnownArrayLength`,
  `KnownArrayLength` — `check/record.go:370-377`. Every row of 06b's
  index/slice table (lines 618-625) is decidable from this plus the solved
  base key.
- `contextFlowRecord` with `Kind` in `contextExpression..contextIndirect`,
  `Context`, `Callee` — `check/record.go:226-233`. Cross-check target
  `Semantics.RuntimeTypes()` returns `RuntimeTypes{Allocator, Context}`
  (`infer/program.go:119-124`, accessor `semantic_snapshot.go:164`).
- Bracket mode: `Semantics.Resolution().Bracket(ref) (BracketMode, bool)` —
  `symbol/symbol.go:350`. Present.
- `unsupportedCallableRecord` with non-empty `TypeParameters` —
  `check/record.go:234-240`. The `C0608` generic-anonymous path is fully
  determined.
- Captures: `Resolution().Captures(function SyntaxRef) []SymbolID`
  (`symbol/symbol.go:351`) plus `callableRecord.Captures` — the `C0617` rule is
  fully determined.

### Gap F5 — "the instantiated descriptor" is not constructible after solve

06b §"Members and records" (line 584-585) states:

> Field result type must match the instantiated descriptor.

That sentence is undetermined for any **generic** nominal, because there is no
post-solve way to instantiate a member descriptor.

*Why it is undetermined.* `TypeDeclaration.Members` is
`[]MemberDescriptor{Symbol symbol.SymbolID; Type TemplateID}`
(`infer/program.go:92-95`) — the member's type is a **template**, not a
`TypeID`. Turning a template into a `TypeID` requires substituting the
nominal's type arguments and then interning the result. The only code that
does this is `Session.templateShape` / `Program.materializeTemplate`
(`infer/instantiate.go:27`, `infer/type_resolver.go:301`), both methods on
`*Session` / `*Program`. Both objects are **destroyed before the handoff**:
`06a` line 1057-1059 requires discarding the session and program, and 06b line
156 states "This audit reads no `Program` or `types.Store` and never interns."
`types.Snapshot` (`types/snapshot.go`) exposes only `Builtins`, `Kind`, `Key`,
`Len`, `Contains`, `IDs` — read-only, no interning. So for `Box[i32].value`,
`06b` can obtain the member's `TemplateID` and the receiver's arguments, but it
cannot produce the concrete `TypeID` to compare against.

*A rejected shortcut, and why it is wrong.* An implementer under pressure would
add a small local template-materializer to `check` — walk the `TypeTemplate`
tree, substitute parameters, and look up or intern each composite. That is
wrong on three counts: it duplicates `05b`-owned semantics in `06b`
(`06b` line 26 forbids `06b` from performing generic substitution), any
composite not already interned during `06a` cannot be produced from a snapshot
at all, and a second materializer is exactly the kind of parallel
implementation that drifts from the solver's and produces disagreement the
verifier cannot see.

*What is actually true.* The solver already enforced this. `Session.hasField`
(`infer/instantiate.go:79-141`) locates the member by name over `decl.Members`,
builds the parameter→argument mapping (`:127-133`), materializes the member's
shape via `templateShape` (`:135`), and unifies it with the member result term
(`:139`). The field's solved type *is* the instantiated descriptor. A `06b`
recheck is therefore either redundant or impossible, depending on how the
sentence is read.

*Smallest amendment.* Replace the sentence with an explicit statement of what
`06b` does check for a member: that the selected member `SymbolID` belongs to
the solved receiver declaration's ordered `Members`; that its category matches
the record's `memberKind`; and that the solved result root is `TypeFinal` and
store-owned. Add one sentence stating that the concrete instantiated field type
is established by `05b`'s `HasField`/`HasComponent` unification and is not
recomputed after solve. This closes the gap without reopening any design — it
only writes down the division of labour that the code already implements.

I am confident enough in this one to name the amendment because the alternative
reading (build a materializer) is explicitly forbidden by 06b's own text, so
there is only one consistent option.

### Cross-reference to F1 (resolved)

The rigid-subject tuple-access hole (06b.5) originated in
`check/member_facts.go:147-149`, which is 06b.3 territory by subject matter
even though the missing artefact was a requirement record. Resolved in
`0f6125f` — noted here only so 06b.3's own implementer knows the fix already
landed and does not duplicate it.

### Readiness verdict — **Needs further spec work first**

One precise gap (F5), one small amendment named above, no reopened design.
Everything else in the slice is fully determined.

### Finer decomposition — **yes, split into three**

This is by a wide margin the largest slice: five source files, five test files,
eleven fixture directories, and six distinct diagnostic codes (`C0601`,
`C0604`, `C0605`, `C0608`, `C0609`, `C0617`). Its completion bar is a run-on of
five unrelated clauses ("no lookup or syntax reinterpretation remains, hidden
context action is exact, record fields are complete, variants are closed, and
every dynamic bound has a check plan"). That is a review burden, not a unit of
work. `06a` split its slices by node kind for exactly this reason.

Proposed split, with the dependency that justifies the order:

- **06b.3a — members, aggregates, and variants.** Owns
  `member_validation.go`, `aggregate_validation.go`, their tests,
  `tests/check/validation/valid/aggregate_*.peb`, `invalid/C0605/*.peb`,
  `invalid/C0601/field_*.peb`. Bar: member category/ownership exactness,
  struct field completeness and duplicate/missing diagnosis in the specified
  orders, variant arity and payload rules, `C0615` for untagged-union and
  opaque construction.
- **06b.3b — calls, callable declarations, and context flow.** Owns
  `call_validation.go`, `context_validation.go`, their tests, `call_*.peb`,
  `callable_*.peb`, `invalid/C0604/*.peb`, `invalid/C0608/*.peb`,
  `invalid/C0617/*.peb`, `invalid/C0601/call_*.peb`. Depends on 3a **only**
  for the variant-call row (`VariantConstruct` needs the variant member
  identity 3a validates); everything else is independent.
- **06b.3c — brackets, indexing, slicing, and bounds.** Owns
  `index_validation.go`, its test, `index_*.peb`, `invalid/C0609/*.peb`.
  Depends on neither 3a nor 3b. Could be implemented first or in parallel.

These three bars genuinely do not depend on each other. Note that untagged
unions being `C0615` today (06b:597) is a **settled, implementable rejection** —
the open untagged-union safety model listed in
`proposals/open-language-decisions.md` §1.3 blocks a future feature, **not**
06b.3a. Likewise the open calling-convention-adaptation question (§1.6) does
not block 06b.3b: "no default promotions are defined", C-variadic calls are
`C0604`, and conventions never adapt (06b:538, :417) are all implementable
rules today.

---

## Slice 06b.4 — places, mutation, assignments, and coercion plans

**Owned files:**

```text
compiler/internal/check/place_validation.go
compiler/internal/check/assignment_validation.go
compiler/internal/check/place_validation_test.go
compiler/internal/check/assignment_validation_test.go
tests/check/validation/valid/place_*.peb
tests/check/validation/invalid/C0601/assignment_*.peb
tests/check/validation/invalid/C0606/*.peb
```

### What it needs from upstream — confirmed

- `placeRecord{Header, Root symbol.SymbolID, RootKind symbol.SymbolKind,
  RootMutable bool, Projections []placeProjection}` —
  `check/place_facts.go:42-48`. The retained root **kind and mutability** that
  06b's writability rules (06b:491-497) depend on are both present as copied
  values; no lookup is needed, exactly as the spec claims.
- `placeKind` closed set `placeStorage`, `placeDereference`, `placeField`,
  `placeTuple`, `placeIndex` — `check/place_facts.go:11-15`, matching 06b's
  five place kinds (06b:484-488) one-for-one.
- Structural invariants already enforced at freeze
  (`check/record.go:347-360`): non-empty projections, `RootKind !=
  symbol.SymbolError`, `Root != 0` iff `RootKind != 0`, and `Root != 0` iff the
  first projection is `placeStorage`. 06b inherits a well-formed chain.
- `assignmentRecord` with `Kind` in `assignmentSimple..assignmentCompound`,
  `Place`, `Source`, `Operator`, and `Statement == Header.Syntax` —
  `check/record.go:339-346`. Compound assignment records are generated
  alongside their `operatorRecord` at `check/operator_facts.go:402`, so the
  "retained `operatorRecord` and result-to-place compatibility record" 06b
  requires (06b:509-511) demonstrably exist.

### Readiness verdict — **Ready for direct implementation**

Every writability input is a copied value on the record. The load / operator /
coerce / store expansion is fully specified (06b:507-517), including
`++`/`--` ("exact literal one, return `void`, legal only as an expression
statement or `for` update") and the single-evaluation rule. No accessor is
missing, no rule is open.

### Finer decomposition

**Existing granularity is right.** Assignment validation's completion bar
("compound/postfix left sides evaluate once") is a statement *about* places;
the two files are one idea split across two functions. Splitting them would
leave the assignment slice with no way to demonstrate its own bar.

---

## Slice 06b.5 — generic requirements, globals, and entry point

**Owned files:**

```text
compiler/internal/check/requirement_validation.go
compiler/internal/check/global_validation.go
compiler/internal/check/entry_validation.go
compiler/internal/check/requirement_validation_test.go
compiler/internal/check/global_validation_test.go
compiler/internal/check/entry_validation_test.go
tests/check/validation/valid/generic_*.peb
tests/check/validation/valid/global_*.peb
tests/check/validation/valid/sizeof_*.peb
tests/check/validation/valid/entry_*.peb
tests/check/validation/invalid/C0602/*.peb
tests/check/validation/invalid/C0610/*.peb
tests/check/validation/invalid/C0615/*.peb
tests/check/validation/invalid/C0616/*.peb
tests/check/validation/invalid/C0620/*.peb
```

### What it needs from upstream — confirmed, and where it breaks

Confirmed present:

- `Solution.Requirements(owner symbol.SymbolID) []Requirement`
  (`infer/solution.go:180-185`), returning copies.
- `infer.Requirement{Owner, Parameter, Kind, Subject, Origin, LiteralKind,
  Numerator, Denominator}` (`infer/solution.go:50-59`).
- `infer.RequirementKind` closed set: `RequirementNumeric`,
  `RequirementIntegral`, `RequirementOrdered`, `RequirementLiteralFits`
  (`infer/solution.go:37-41`). No `Equatable` — consistent with 06b's
  "`Equatable` is checker-owned" (06b:702).
- `check.requirementRecord{Header, Kind, Subject valueID, Operator
  syntax.TokenKind}` (`check/generic_facts.go:29-34`), populated with a token
  only through `retainOperatorRequirement` (`check/operator_facts.go:430-442`).
- Entry validation inputs: `Config` exists and is extensible
  (`check/config.go:22-35`), `handoff.Compilation.Root` exists
  (`check/solve_handoff.go:38-43`), `Semantics.Signature` exists
  (`infer/semantic_snapshot.go:124`), `Signature` carries `Parameters`,
  `TypeParams`, `Result`, `Convention`, `Variadic` (`infer/program.go:108-117`)
  — every property 06b's entry rule tests (06b:821-826) is available.
- `sizeof` inputs: `typeUseRecord` with `Kind` up to `typeUseExplicitArgument`
  and a `Type` value (`check/record.go:219-225`).
- Global inputs: `bindingRecord` with `Kind` in
  `bindingLocalLet..bindingRangeIterator`, `AnnotationPresent`,
  `InitializerPresent`, `Annotation`, `Initializer`
  (`check/record.go:198-204`) — the `C0602` empty-form rule is fully
  determined and type-independent, as 06b:648-654 claims.

Now the three problems.

### Gap F1 — RESOLVED (commit `0f6125f`)

**This gap is closed.** `requirementUnsupportedComponent` was added to
`check.requirementKind` (appended, no renumbering), retained for
`memberTuple` in `member_facts.go`, `record.go`'s freeze-time range
extended from `requirementUnsupportedConstruction` to
`requirementUnsupportedComponent`, and "tuple-component" added to both
`06a`'s unsupported-generic-requirement-kind prose and `06b`'s `C0610`
clause. A direct test (`TestTupleComponentRigidRetainsUnsupportedRequirement`)
confirms the requirement record joins the member record by `Header`/
`Subject`, carries zero `Operator`, and is counted exactly once. Full
suite, `-race`, `vet`, and `git diff --check` all passed at landing. The
analysis below is kept as historical evidence: it is the exact failure
shape F4 (§06b.6) turned out to share, and is worth reading for that
reason even though the finding itself is closed.

*What was undetermined.* What happens to `x.0` where `x` has a rigid type
parameter type, inside a generic body.

*Why.* The accepted `05b` constraint table
(`05b-algebraic-inference.md:1003`) says of `HasComponent`:

> A rigid type parameter makes the constraint checker-deferred complete,
> recovers `result` to `Error` without an inference diagnostic, and invents no
> trait; **phase 6 retains the unsupported checker requirement**.

The implementation matches that: `Session.hasComponent`
(`infer/structural.go:273-274`) returns `s.recoverTerms(result), true, false`
on `structuralRigid` — recovered, successful, no conflict, no diagnostic. By
design, `05b` is silent and phase 6 is meant to speak.

Phase 6 does not speak. `check/member_facts.go:138-143` retains an unsupported
requirement for exactly two member kinds:

```go
if p.kind == memberField {
    w.retainRequirement(header, requirementUnsupportedField, base.ID)
}
if p.kind == memberMethod {
    w.retainRequirement(header, requirementUnsupportedMethod, base.ID)
}
```

`memberTuple` is absent. And there is no kind to use even if the call were
added: `check.requirementKind` (`check/generic_facts.go:10-26`) enumerates
`requirementUnsupportedField`, `Method`, `Index`, `Slice`, `Call`,
`Conversion`, `Layout`, `Print`, `Construction` — **no component kind**.
`06b`'s own `C0610` clause (06b:713-714) lists "Field/method/index/call
requirements on an otherwise unconstrained parameter, conversion to/from a
parameter, layout, printing, and construction" — tuple components are not in
that list either.

Net effect: `fn f[T](x T) void { let y = x.0; }` yields an `Error` result term
with **no diagnostic from any phase**. Every other structural relation
(`HasField`, `SelectMethod`, `Callable`, `Indexable`, `Sliceable`) has its
`requirementUnsupported*` counterpart wired at `member_facts.go:139,142`,
`bracket_facts.go:226,255,262,266`, `index_facts.go:88`, and
`call_facts.go:368,457`. `HasComponent` — the newest constraint — is the sole
omission. This is the same failure shape as the `session.go`/`solve.go` miss:
an amendment that was correct in prose but did not name every site it had to
reach. The solver-side sites *were* named and *were* wired
(`infer/session.go:524`, `infer/solve.go:127`, `:481`, all verified present).
The `check`-side requirement site was not.

*A rejected shortcut, and why it is wrong.* Reusing
`requirementUnsupportedIndex` for tuple access. It is wrong because the two are
deliberately distinct relations: `05b`'s `Indexable`/`Sliceable` "deliberately
exclude tuples" and `06b` states "Pointers and tuples are not indexable"
(06b:629). Reporting a tuple-component failure as an indexing requirement would
produce a diagnostic that contradicts the accepted spec's own words, and it
would collide with a real `Indexable` requirement on the same parameter.

*Amendment landed.* One `requirementUnsupportedComponent` kind added to
`check`'s `requirementKind`, retained for `memberTuple` in
`member_facts.go`, the freeze validation range at `check/record.go`
extended from `requirementUnsupportedConstruction` to
`requirementUnsupportedComponent` (which is now the accepted terminal
requirement kind — any future addition appends after it, the same way this
one appended after `requirementUnsupportedConstruction`), and "tuple
component" added to `06b`'s `C0610` clause. The `record.go` range check was
correctly identified as a fourth integration site and was not missed.

**This touched `06a`-owned files**, correctly: `06b`'s slice contract says
"No slice edits phases 03, 04b, 05a, or 05b to make implementation easier"
(06b:1501-1502) — `06a` is not on that list, and the change was a genuine
`06a` defect, not an implementation convenience. It landed as its own small
amendment before any 06b slice started, exactly as recommended.

### Gap F2 — literal-fit requirements have no `06a` record to join

06b:700-701 states:

> Numeric, integral, ordered, and literal-fit records must join exactly with
> one `05b` requirement by owner, rigid parameter identity, kind, and origin.

There are no literal-fit records. `requirementLiteralFits` is declared at
`check/generic_facts.go:17` and, per a whole-tree grep, **appears nowhere else
in the repository** — not in production code, not in tests. The only
requirement-retaining call sites are `retainOperatorRequirement` (numeric,
integral, ordered, equatable — `operator_facts.go:412-418`),
`assignment_facts.go:119-121` (numeric/integral), and the nine
`requirementUnsupported*` sites.

Meanwhile `05b` **does** produce `RequirementLiteralFits`:
`Session.recordLiteralRequirement` (`infer/capability.go:169-190`) constructs
one with `LiteralKind`, `Numerator`, and `Denominator` populated, and
`semantic_snapshot.go:600-640` validates it.

So the join rule is unsatisfiable in one direction. The published
`Requirement` interface includes `RequirementLiteralFits` (06b:684) and its
`LiteralKind`/`Numerator`/`Denominator` fields (06b:694-696) — the *output*
side is fully specified, and the values exist on the `05b` side. What is
undetermined is only the *source*: does `06b` publish literal-fit requirements
from `Solution.Requirements(owner)` alone (no `06a` record involved), or is
`06a` missing a retention site the way it is for tuple components (F1)?

*Why it is not obviously one or the other.* Literal fitting genuinely has no
per-use-site checker policy — `06b` explicitly says literal fitting "was
inference evidence" and is "not a conversion" (06b:355-356), which argues for
"no record needed". But the same paragraph that names the join also says
"duplicate equivalent uses produce one interface requirement with all use sites
retained as related labels" — and without a `06a` record there are no use sites
to retain as labels for the literal-fit kind, so its diagnostics would be
strictly poorer than the other three kinds'.

*Rejected shortcut.* Silently dropping literal-fit from the published
requirement list because no record exists. That would contradict 06b:684, which
puts `RequirementLiteralFits` in the closed published enum, and would silently
weaken what phase 7 can prove at a generic call site.

I am **not** naming the amendment here. Choosing between "publish from the
solution alone, with the origin `05b` kept" and "add a `06a` retention site so
literal fits get use-site labels like every other kind" is a real decision about
what the requirement interface owes phase 7, and it is not mine to make. What
the amendment must do is state which, and correct 06b:700-701 so the join rule
does not name a record class that does not exist.

### Gap F3 — the join key cannot include `origin`

06b:700-701 makes `origin` part of the join key. `05b`'s requirement table
cannot support that.

`Session.recordRequirement` (`infer/capability.go:143-176`) deduplicates on
`(Owner, Kind, Subject)` only:

```go
for i, existing := range s.requirements {
    if existing.Owner == owner && existing.Kind == kind && existing.Subject == subject {
        if originBefore(origin, existing.Origin) {
            s.requirements[i].Origin = origin
        }
        return
    }
}
```

It keeps **one** requirement per `(Owner, Kind, Subject)` and narrows its
`Origin` to the earliest. `06a`, by contrast, retains **one record per use
site**: `retainOperatorRequirement` (`operator_facts.go:430-442`) is called per
operand per operator occurrence, each with its own `Header.Syntax`. So for

```pebble
fn f[T](a T, b T, c T) T { return a + b + c; }
```

there are multiple `requirementNumeric` records with distinct origins and
exactly one `05b` `RequirementNumeric`. Every record except the earliest fails
an origin-keyed join.

There is a second, smaller type mismatch on the same key: `06b`'s published
`Requirement.Origin` is `symbol.SyntaxRef` (06b:692) while `infer.Requirement.
Origin` is `infer.Origin` — a struct of `{Syntax, Span, Role, Symbol,
GenericOwner}` (`infer/constraint.go:8-14`). Comparison would be against
`.Syntax`, which the spec does not say.

*Rejected shortcut.* Loosening the implementation to "join if any field
matches" and moving on. That destroys the property the rule exists to
guarantee — that every checker-side requirement is backed by a solver-side one.

*Smallest amendment.* The two sentences at 06b:700-705 already describe the
correct behaviour in the second one ("Normalize by owner `SymbolID`, declared
parameter ordinal, kind, then first source origin; duplicate equivalent uses
produce one interface requirement with all use sites retained as related
labels"). Correct the first sentence to make the join key
`(Owner, Kind, Subject)` — matching `05b`'s dedup key exactly — with origin used
for *ordering and labelling*, not matching; and state that many `06a` records
may join one `05b` requirement. Add that `Origin` is compared/copied as
`infer.Origin.Syntax`. This is mechanical: it writes down what the second
sentence already intends and what `05b` already does.

### Drift D2 — `Requirement.Operator token.Kind` names a package that does not exist

06b:693 declares the published field as `Operator token.Kind`. There is no
`token` package: `ls compiler/internal/` gives `check`, `diagnostic`, `infer`,
`module`, `source`, `symbol`, `syntax`, `types`. The real type is
`syntax.TokenKind` (`syntax/token.go:6`), which is what
`requirementRecord.Operator` uses (`check/generic_facts.go:33`). One-word
correction; no behaviour change. Worth fixing before the type is published,
since `Requirement` is a public checker interface consumed by phase 7.

### Readiness verdict — **Ready for `06b.5a`, pending mechanical spec corrections**

F1 is resolved (commit `0f6125f`). F2 is decided (publish directly from
`Solution.Requirements()`, no `06a`-side record — see "Decisions made on
review" below). F3 and D2 are both mechanical corrections to `06b`'s own
text with only one reading consistent with already-shipped code — F3's join
key, D2's `Operator` type. None of the four blocks implementation once
those corrections are applied to the accepted `06b` spec text (Wave 2 work,
not done in this document).

The globals/`sizeof` half and the entry-point half of this slice are, by
contrast, **fully determined** — every input confirmed above, `C0602`/`C0616`/
`C0615`/`C0620` all fully specified, no open decision touching any of them.

### Finer decomposition — **yes, split into three**

This slice bundles three concerns that share no record type, no diagnostic
code, and no upstream accessor: generic requirements, constant globals plus
`sizeof`, and the configured entry point. The spec's own completion sentence
gives it away by being three independent clauses joined with "and": "symbolic
bodies publish the closed ordered interface, unsupported requirements fail,
globals are constant, and no spelling selects an entry." **Corrected on
review: this is not the same as saying their completion bars have no
dependency on each other at all** — `06b.5c` reads normalized requirements,
and the accepted validation order places requirement normalization first
(see `06b.5c`'s own entry below). The three are independent enough to
justify the split; they are not independent enough for all three to be
*accepted as complete* in any order.

- **06b.5a — generic requirements.** `requirement_validation.go` + test,
  `generic_*.peb`, `invalid/C0610/*.peb`. F1 resolved; **blocked** only on
  F3/D2's mechanical spec corrections (F2 is decided, not blocking).
- **06b.5b — globals, bindings, and `sizeof`.** `global_validation.go` + test,
  `global_*.peb`, `sizeof_*.peb`, `invalid/C0602/*.peb`,
  `invalid/C0615/*.peb`, `invalid/C0616/*.peb`. **Independently ready** —
  the one genuinely self-contained piece of this split, depending only on
  `06b.1`.
- **06b.5c — entry point.** `entry_validation.go` + test, `entry_*.peb`,
  `invalid/C0620/*.peb`. **May be drafted early; accepted only after
  `06b.5a`** — its own inputs (`Config.Entry`, `Semantics.Signature`, the
  root module) are all available from `06b.1` alone, but entry validation
  reads normalized requirements, which only exist once `06b.5a` has produced
  them. Writing `06b.5c` early is fine; calling it complete before `06b.5a`
  exists is not, and risks duplicating requirement logic to avoid the wait.

The practical payoff is still real: splitting lets `06b.5b` proceed
immediately and lets `06b.5c` be drafted immediately, instead of the whole
slice waiting behind the requirement-interface decision that used to gate
all three together. Only `06b.5a` itself, and `06b.5c`'s final acceptance,
wait on that decision now.

---

## Slice 06b.6 — structural flow, switches, returns, reachability, and defer

**Owned files:**

```text
compiler/internal/check/control_validation.go
compiler/internal/check/switch_validation.go
compiler/internal/check/defer_validation.go
compiler/internal/check/control_validation_test.go
compiler/internal/check/defer_validation_test.go
tests/check/validation/valid/control_*.peb
tests/check/validation/valid/defer_*.peb
tests/check/validation/valid/statement_*.peb
tests/check/validation/invalid/C0601/return_*.peb
tests/check/validation/invalid/C0607/*.peb
tests/check/validation/invalid/C0611/*.peb
tests/check/validation/invalid/C0612/*.peb
tests/check/validation/invalid/C0613/*.peb
tests/check/validation/invalid/C0618/*.peb
```

(Note: the spec lists no `switch_validation_test.go` even though it lists
`switch_validation.go` and its `TestSwitchValidation` run filter. Probably an
oversight in the owned-files list rather than an intent that switch tests live
in `control_validation_test.go`; worth confirming, trivial either way.)

### What it needs from upstream — confirmed

- `controlRegion{ID, Parent, Depth, Children}` — `check/record.go:452-457`,
  with `Children` **derived once at freeze** from parent links
  (`record.go:509-552`) in ascending `controlID` order, and `controlID`
  allocated in traversal order. So children are in authored order by
  construction, as 06b assumes.
- `controlRecord{Header, Kind, Region, Target, Callable, StatementForm,
  Values, ConditionPresent, ElsePresent, RangeInclusive}` —
  `check/control_facts.go:57-64`.
- `controlKind` closed 14-member set (`control_facts.go:9-25`), with
  `regionOwningControl` naming the 8 region-owning kinds explicitly
  (`control_facts.go:75-84`): `controlFunction`, `controlBlock`, `controlIf`,
  `controlWhile`, `controlRangeLoop`, `controlFor`, `controlSwitch`,
  `controlSwitchCase`.
- `controlValueRole` closed 9-member set (`control_facts.go:39-49`):
  `valueCondition`, `valueSubject`, `valueCase`, `valueReturn`,
  `valueRangeStart`, `valueRangeEnd`, `valueRangeIterator`,
  `valuePrintOperand`, `valueDiscarded`. Every role 06b's audit table (line
  229) names is present.
- `statementForm` closed set (`control_facts.go:29-36`): `statementPrint`,
  `statementDiscard`, `statementAssignment`, `statementCall`,
  `statementPostfixUpdate`, `statementOther` — enough to decide 06b's
  expression-statement legality rule (06b:655-658) without rescanning syntax,
  as claimed.
- `deferRecord{Header, Region, Ordinal, Statement}` —
  `check/control_facts.go:67-72`, with `Ordinal` giving registration order.
- The freeze invariants 06b says it rechecks (06b:743-747) are already enforced
  once at freeze: contiguous IDs, root depth one, earlier-valid parents, exact
  depth increments, edge count (`record.go:509-552`); valid record/defer regions
  and targets, one `controlFunction` per root, consistent `callableRef`
  throughout each function tree (`solve_handoff.go:403-521`). 06b's recheck is
  a genuine second line of defence over already-validated data, not a
  requirement for anything new.
- `diagnostic.Warning` severity exists and does **not** increment `errorCount`
  (`diagnostic/diagnostic.go:20, 64-69`), so `C0618` can be emitted as a
  warning without tripping the IR publication gate, exactly as 06b:300 requires.
  There is no `Warning` convenience constructor — only `Error(...)` and
  `Add(Diagnostic)` — so 06b.6 must use `Add` directly. Minor, not a gap.

### Gap F4 — `06a`'s handoff has no representation for a non-block arm's role

**This section was substantially revised after independent review. The
original draft characterized F4 as an unwritten but existing mapping — a
documentation gap. That is wrong. The information the mapping would describe
does not exist in the handoff for a common, legal case. This is a missing
representation in the accepted `06a.7` data model, not missing prose, and the
fix reopens `control_facts.go`, not just a spec paragraph.**

*What is undetermined.* Given a region-owning `controlRecord`, which of the
flat leaf records sharing its region is the "then" arm, which is "else", which
is the loop's init/update/body, and which case body is which — **in the case
where that arm or clause is not itself a block**.

*Why the original "add a table" fix cannot work.* A table mapping child
ordinal to role presupposes the arena's `Children []controlID` list actually
contains one entry per role. It does not, whenever an arm is a bare non-block
statement, because only region-owning statements allocate a `controlID` at
all. Confirmed directly against the parser and the walker:

`parser_statement.go:110-121` (`parseIfStatement`) calls the general
`p.parseStatement()` for both the then-branch and the else-branch — not a
block-only production. `if flag return; else print 1;` is legal, parses two
bare leaf statements as the arms, and is not an edge case; it is what the
grammar allows everywhere a compound body is expected.

`control_facts.go:243-262` (`prepareIf`) allocates **one** region for the
`if` itself and stamps that same region onto every child item —
`w.stampRegion(items, control)` — *before* dispatching to them. A `BlockStmt`
arm allocates its own nested region when the walker later reaches it (region-
owning kinds do this); a bare `ReturnStmt` or `PrintStmt` arm does not — leaf
kinds never call `enterRegion`. So for `if flag return; else print 1;`, the
`if`'s region has **zero child regions** (nothing nested allocated a region),
while its two arms exist only as flat `controlRecord`s — one `controlReturn`,
one `controlPrint` — both with `.Region` pointing at the same `if` region, and
neither record carrying anything that says "I am the then arm" versus "I am
the else arm." There is no ordinal to put in a table, because there is no
child in the arena sense to index.

The same shape recurs beyond `if`: `for` allocates one region and stamps
init, condition, update, *and* body into it identically
(`prepareFor`, mirroring `prepareIf`'s pattern); a `for`/`while`/range-loop
body, or a `switch-case` body, can equally be a bare non-block statement. Any
construct with more than one role-bearing clause, where at least one clause
can legally be a non-block statement, has this gap. That is not a short list
of exceptions — it is the general case for every region-owning kind except
`controlBlock` and `controlFunction`, whose children are an ordinary ordered
statement sequence with no named-role ambiguity to begin with.

*Why this is dangerous rather than merely undocumented.* Beyond the missing-
ordinal case above, the existing text has a second, independent hazard:
`enterRegion` returns `0` on a refused allocation and the statement "recovers
as a leaf of its enclosing region instead of fabricating a second root"
(comment at `control_facts.go:188-190`). Even in the all-blocks case, if a
*then*-arm block's region allocation is refused, the `if` region's only
*child region* is the *else* region, and any positional interpretation of
child-region order would silently treat the else arm as the then arm. That
path is preceded by a `C0619`, so no IR is ever published from it — but it
shows that even the child-region list, when it does exist, is not safe to
read positionally without an explicit role.

*Rejected shortcut.* Having 06b infer arm roles from the child regions' own
control records (e.g. "the child whose record is a `controlBlock` at index 0
is the then arm"). Wrong on two counts: it does nothing for the bare-leaf
case above, where no child region exists at all; and even restricted to the
all-blocks case it fails for `else if`, whose else child is a `controlIf`,
not a `controlBlock`, so "first child is then" is not even a safe rule for
blocks alone.

*Required correction.* Extend `06a`'s handoff with explicit structural-
composition evidence: ordered references, tagged with an explicit role
(`then`, `else`, `body`, `initializer`, `update`, `case` *i*), from a region-
owning control record to whichever single record actually names that arm —
its own leaf `controlRecord` if the arm is bare, or its region-owning
`controlRecord` if the arm is a block or nested control statement. A new
closed leaf kind, `controlBinding`, is required so that a local binding
statement — which today retains no control record at all — can be named as
an arm and can take its place in sequential flow.
`controlRegion.Parent`/`Children` remains the sole authority for lexical
parentage and defer scope, unchanged; this is a **separate relationship** —
structural composition, not lexical containment — living beside it, not
replacing it.

The freeze audit gains four checks, in strict order, each closing a gap the
previous one leaves open:

1. **Exact authored composition.** One authoritative
   `expectedComposition(ref, node, tree)` helper derives the exact ordered
   role/ordinal/`SyntaxRef` list from the parent's surface node. `06a.7`
   population uses it directly; `06a.8`'s audit calls it again
   independently and compares field-for-field — same length, order, `Role`,
   `Ordinal`, and `Arm`. Without this, a silent then/else swap passes every
   other check.
2. **Unique control-record resolution.** Each non-recovery `Arm` resolves
   to exactly one retained record with `Control != nil` — counting only
   control records, since `bindingRecord` and others legitimately share a
   `SyntaxRef` with the record being sought.
3. **Exact syntax-kind → control-kind correspondence.** A closed predicate
   maps each legal arm syntax kind to the one control kind its record must
   carry (`IfStmt`→`controlIf`, `BindingDecl`→`controlBinding`,
   `AssignmentStmt`/`ExpressionStmt`→`controlExpression`, and so on). No
   spelling, record order, or heuristic.
4. **Exact lexical placement.** A leaf arm must satisfy `child.Region ==
   parent.Region`; a region-owning arm must satisfy
   `controls[child.Region-1].Parent == parent.Region`. Containment
   validation only — it cross-validates the region arena against the
   structural role rather than duplicating parentage.

`Missing`/`Error` arms retain their exact `Composition` entry and are
exempt from (2), (3), and (4) — never from (1). Full design is in
`proposals/06a-structural-composition.md` (`06a` is reopened
`06a.7`/`06a.8`-accepted territory; this is not a documentation fix and
should not be scoped as one).

*Consequence for sequencing.* This fix changes what `06a` hands off, which
every other 06b slice reads. It must land — as an accepted `06a` amendment,
implemented and verified with the same rigor as `HasComponent` — before any
06b slice begins, not merely before 06b.6. See the corrected recommended
order below.

### Readiness verdict — **Blocked on an `06a` amendment, not "needs spec work"**

F4 is not closable by editing this document or `06b`'s spec text. It requires
a new `06a` proposal, review, and implementation, landed and verified before
`06b.1` starts. Once that lands, the rest of 06b.6 is fully determined: every
record, arena, role, and invariant it otherwise consumes exists and is
already validated at freeze. Note also N2 from 06b.2: enum switch
exhaustiveness must be computed against the declaration-complete variant
list, not the filtered `TypeDeclaration.Members`.

### Finer decomposition — **existing granularity is right**

`switch_validation.go` looks separable, but its exit-set contribution ("a
switch consumes its matching break", "exhaustive switch exits are the union of
reachable case exits", 06b:780, :790) *is* the flow analysis, and defer chains
are computed over the exit edges control analysis produces (06b:798-803). The
three files are one algorithm. The slice is large, but its parts are genuinely
coupled — unlike 06b.3 and 06b.5. No split.

---

## Slice 06b.7 — typed-IR store, construction, and verifier

**Owned files:**

```text
compiler/internal/tir/id.go
compiler/internal/tir/node.go
compiler/internal/tir/unit.go
compiler/internal/tir/verify.go
compiler/internal/tir/*_test.go
compiler/internal/check/ir_builder.go
compiler/internal/check/ir_builder_test.go
tests/check/ir/valid/*.peb
tests/check/ir/*.tir.golden
```

### What it needs from upstream

Everything, and only through slices 06b.1–06b.6: "consume only successful
semantic decisions from slices 06b.1-06b.6". The `tir` package does not exist
(`ls compiler/internal/` confirms), so this slice has no upstream code to
verify against — it *is* the upstream for phase 9.

What can be checked is that the spec closes the inventory, and it largely does:
`NodeID`/`FunctionID`/`RegionID`/`TempID` (06b:833-836), the surface-kind
disposition table (06b:864-882) covering every `03b` kind including the
explicit nonvalue paths for `EndOfFile`, `RecordField`, `StructType`/
`UnionType`/`EnumType`, `ExternDecl`/`ExternBlock`, and recovery nodes; the
five node-category lists (06b:886-994); twelve invariants (06b:998-1018); and
the verifier's checked properties (06b:1020-1024).

### Readiness verdict — **Uncertain**

Not because a gap is visible, but because of a specific pattern the two prior
`06a` gaps warn about, which cannot be confirmed without building the thing.

The concrete concern: this slice creates the compiler's **first new
kind-enumerating dispatch surface since `05b`** — a node-tag switch in
`tir/verify.go` covering roughly 60 node tags across five categories, plus a
normalized-dump printer that must cover the same set. Every prior instance of
this shape in the codebase has a closed enum guarded by a range check at the
arena boundary (`check/record.go:200-401` for record payloads,
`:381` for requirement kinds, `infer/semantic_snapshot.go:600` for requirement
kinds again). The `HasComponent` experience was that a *correct* spec plus a
*new* dispatch surface still lost a case, because the spec named the feature
but not the switch. Here the spec names ~60 node tags in five prose lists and
one 19-row table, with no single enumeration an implementer can mechanically
transcribe and no stated rule that verifier coverage must be total.

What would resolve the uncertainty: a totality mechanism decided *before* the
node set is written — an exhaustive-switch discipline (a `default:` that fails
`C0619` rather than falling through, on every `tir` switch), plus one test that
constructs at least one instance of every declared node tag and asserts the
verifier and the dump printer both handle it, driven from a single tag list.
If 06b.7 commits to that up front, this slice becomes Ready; the design content
is fully specified and I found nothing undetermined in it. Without it, the
probability of a silent missing tag is high, and the failure would be exactly
as invisible as the two prior ones.

Secondary, smaller uncertainty: 06b:930-935 says an identity `as` cast becomes
a "cast-tagged `SourceAlias`", but `SourceAlias` appears in the **Values** list
with no stated tag field. Whether "cast-tagged" is a distinct node tag or a
boolean on `SourceAlias` is not determined. It is small and the slice can
decide it, since either choice satisfies every stated invariant — but it should
be decided deliberately and recorded, not left to whoever types first.

### Finer decomposition — **yes, split into two**

- **06b.7a — the `tir` package.** `tir/id.go`, `tir/node.go`, `tir/unit.go`,
  `tir/verify.go`, `tir/*_test.go`. Bar: the closed store exists, IDs and
  ownership are enforced, and the verifier proves every invariant in
  06b:998-1024 **against synthetic units built by test-only constructors**.
- **06b.7b — the builder.** `check/ir_builder.go` + test,
  `tests/check/ir/valid/*.peb`, `tests/check/ir/*.tir.golden`. Bar: every
  successful semantic decision from 06b.1-06b.6 maps to exactly one node, and
  golden dumps are stable.

This split is worth more than the others for two reasons. First, 7a's
completion bar is genuinely independent — a verifier is testable against
hand-built damaged units without any checker input, which is exactly how you
prove it rejects things. Second, 7a has **no dependency on slices 06b.2–06b.6
at all**, so it can be built in parallel with them rather than at the end of a
seven-slice chain. That materially shortens the critical path. It also isolates
the totality discipline described above into a slice whose only job is to get
it right.

---

## Slice 06b.8 — publication gate, determinism, recovery, fuzz, and race

**Owned files:**

```text
compiler/internal/check/check.go
compiler/internal/check/publication_test.go
compiler/internal/check/validation_determinism_test.go
compiler/internal/check/validation_recovery_test.go
compiler/internal/check/fuzz_test.go
compiler/internal/check/race_test.go
tests/check/validation/valid/multimodule/*
tests/check/validation/recovery/integration_*.peb
tests/check/ir/recovery/*.peb
```

### What it needs from upstream — confirmed

- `run06a(inputs, diagnostics, config) *solveHandoff` exists
  (`check/solve_handoff.go:57`) and structurally enforces one traversal and one
  solve: `facts.Session.Solve()` is called at exactly one place (`:87`), and
  `infer.Snapshot` at exactly one place (`:90`).
- There is **no public checker entry today**. `grep -rn "run06a"` over
  `compiler/internal/` returns only test callers of `run06a3`; `run06a` itself
  has no non-test caller anywhere, and there is no `func Check(...)` in the
  `check` package. So `check.go` is a genuinely new file wiring a genuinely
  new public surface, not a modification of an existing one. Nothing else in
  the tree needs updating to call it — confirmed, no driver call site exists.
- `diagnostic.DiagnosticSet.HasErrors()` (`diagnostic/diagnostic.go:109`)
  supports gate condition 6 ("no error or limit diagnostic from any earlier
  compiler phase"), since the set is shared across phases. Note this makes
  condition 6 automatic rather than something 06b.8 must implement separately.

### Readiness verdict — **Uncertain**

The slice's own content is fully determined: the six gate conditions are
enumerated (06b:294-298), determinism is specified down to the tiebreak order
(06b:318-321), and the fuzz/race obligations are explicit (06b:1088-1101).
Nothing about *this* slice is open.

The uncertainty is entirely inherited. 06b.8's bar is "the complete checker
under deterministic, damaged, limited, fuzzed, and concurrent workloads", which
by construction cannot be assessed until 06b.1–06b.7 exist. It is Ready
conditional on its predecessors, and it will surface any gap the earlier slices
left — which is the right place for it in the order, but means its readiness
cannot be certified now.

One thing that *can* be settled now, and should be: `MaxDiagnostics` is shared
with `06a` (06b:100-102, `check/config.go:34` — default 100), and 06b's rule is
that hitting it "replaces the final retained 06b diagnostic with `C0619`"
(06b:327-328). `06a` has its own `generationDiagnosticBudget`
(`check/constant.go:89`). Whether `06b` gets a fresh budget or continues `06a`'s
running count across the same `DiagnosticSet` changes observable output on any
compilation near the limit, and neither document says. It is small, but it is a
determinism question and belongs in 06b.1's config extension, not discovered in
06b.8's determinism tests.

### Finer decomposition — **existing granularity is right**

The whole point of the slice is that everything is exercised together through
one public entry. Splitting determinism from fuzz from race would produce three
slices that each need the same complete pipeline standing up.

---

## Recommended implementation order

**This section was corrected after independent review.** The original order
placed `06b.7a` second "for efficiency." That understated it: `06b.1`'s own
`Result` type (06b:249-278) declares fields typed `tir.NodeID` and returns
`*tir.Unit` directly. `check/result.go` cannot be written — not merely
"implemented suboptimally," cannot *compile* — without the `tir` package's
identity types already existing. `06b.7a` must precede `06b.1`, as a hard
dependency, not an optimization. The review also found `06b.5c` (entry point)
is not fully independent of `06b.5a` (generic requirements): entry validation
reads normalized requirements, and the accepted validation order places
requirement normalization first. `06b.5c` can be *drafted* early but should
not be accepted as complete before `06b.5a`, unless it duplicates requirement
logic, which it should not.

**Corrected order:**

```text
0.  06a amendment: requirementUnsupportedComponent  (F1)     [DONE — 0f6125f]
1.  06a amendment: structural-composition handoff           (F4)
2.  Apply F2/F3/F5 and the mechanical 06b corrections to the accepted spec
3.  06b.7a  tir store, node inventory, verifier totality    [no dependency on 2-6 below]
4.  06b.1   handoff, solved records, result shell
5.  06b.2   compatibility + operators
6.  06b.3c  brackets, indexing, slicing                     ┐ where ownership permits,
    06b.3a  members, aggregates, variants  [needs F5]        ┘ these three may proceed
    06b.5b  globals, bindings, sizeof                          in any order after 4
7.  06b.3b  calls, callables, context                       [after 3a]
8.  06b.4   places, assignments                             [after 3]
9.  06b.5a  generic requirements                            [after F2/F3 applied in step 2]
10. 06b.5c  entry point                                     [after 5a, normalized requirements]
11. 06b.6   structural flow, switches, defer                [after 2/3, F4 fixed; after compatibility/member/place decisions]
12. 06b.7b  IR builder
13. 06b.8   publication gate, determinism, fuzz, race
```

**Step 0 (F1) is complete.** Step 1 (F4) is next and remains the only
`06a`-touching step still outstanding. Both were placed before any 06b slice,
not just before the slice that happens to surface them (06b.5 and 06b.6
respectively), because both touch `06a`-owned files — every hour of 06b work
that lands first is an hour of rebasing on top of a handoff schema that is
about to change. F1 turned out to be exactly as small as expected. F4 is not
— it reopens `control_facts.go`'s data model (and, per the corrected
proposal, `solve_handoff.go`'s cross-record audit as well) — which is exactly
why it must not be discovered mid-phase.

**`06b.7a` moves to third position as a hard requirement, not an
optimization** (see correction above). It also has no dependency on
`06b.2`–`06b.6` — a store plus a verifier over synthetic units — so building
it there additionally front-loads the node-tag totality discipline this audit
is least confident about (see 06b.7), off the critical path.

**`06b.5b`/`06b.3c`/`06b.3a` may proceed in any order once their prerequisites
land**, since none of the three depends on the others. `06b.5c` moves *after*
`06b.5a`, correcting the original draft, which placed it independently early;
it reads normalized requirements and should not be accepted as complete
before they exist.

**Not deviating further:** `06b.4` stays after `06b.3`, because place
validation consumes "the exact member/index decisions from 06b.3." `06b.6`
stays after `06b.2`/`06b.3` (return validation uses the compatibility matrix,
switch validation uses member decisions) and after the F4 amendment lands.
`06b.8` stays last, unavoidably.

---

## Summary table

| Slice | Verdict | One-line reason |
| --- | --- | --- |
| **06b.1** handoff, records, result | **Ready** | Every handoff type and `Solution`/`Semantics`/`types.Snapshot` accessor confirmed to exist as named; only `frozenConstants`' accessor name/return type is misdocumented (D1) |
| **06b.2** compatibility, operators | **Ready** | Both matrices are closed, every discriminator exists, enum/integer rows confirmed applied; needs one clarifying sentence about which member list defines enum ordinals (N2) |
| **06b.3** calls, members, aggregates, index, context | **Needs spec work** | "Field result type must match the instantiated descriptor" (06b:584) is not constructible post-solve — template instantiation is `Session`/`Program`-private and `types.Snapshot` cannot intern (F5) |
| **06b.4** places, assignments | **Ready** | Root kind, mutability, and the full projection chain are copied onto `placeRecord`; freeze already enforces chain well-formedness |
| **06b.5** requirements, globals, entry | **Ready, pending mechanical spec corrections** | F1 resolved (`0f6125f`); F2 decided (publish from `Solution.Requirements()` directly); F3 and D2 are mechanical corrections to `06b`'s own text, not open questions. Globals half is Ready; entry half is Ready but depends on 5a's requirements, not independent as first drafted |
| **06b.6** flow, switches, defer | **Blocked on an `06a` amendment** | `06a`'s handoff has no representation at all for a non-block arm's role (then/else/init/update/body), and no control record at all for local bindings — not an unwritten mapping, a missing one. The amendment audits four things in order: exact authored composition against one shared `expectedComposition` helper, unique control-record resolution, exact syntax/control-kind correspondence, and exact leaf/region-owning lexical placement. Reopens `control_facts.go`/`record.go` (06a.7) and `solve_handoff.go` (06a.8); must land before any 06b slice, not just before 06b.6 (F4) |
| **06b.7** typed IR, verifier | **Uncertain** | Content is fully specified, but the slice creates ~60 node tags across five prose lists with no single transcribable enumeration and no stated verifier-totality rule — the exact shape of both prior silent gaps |
| **06b.8** gate, determinism, fuzz, race | **Uncertain** | Its own content is determined and no public entry exists to conflict with, but its bar is inherited from all seven predecessors; one settleable question: whether `06b` shares or restarts `06a`'s `MaxDiagnostics` count |

---

## Hidden-integration-site sweep

The second `06a` gap was a spec amendment that was correct in prose but never
named the dispatch sites it had to be added to. Below is every place I
specifically looked for that shape — a switch, range check, or dispatch table
elsewhere in the codebase that enumerates constraint / record / requirement /
node / type kinds and that a `06b` feature would need to be extended into.
Clean results are listed too.

**Sites where something was found (F1: all three now resolved, `0f6125f`):**

1. **`compiler/internal/check/generic_facts.go:10-26`** — the `requirementKind`
   enum. Contained `requirementUnsupportedField`, `Method`, `Index`, `Slice`,
   `Call`, `Conversion`, `Layout`, `Print`, `Construction`, no component kind,
   at audit time. This was finding **F1** — the `HasComponent` amendment
   reached the solver's two switches but never reached this enum. **Resolved:**
   `requirementUnsupportedComponent` appended.
2. **`compiler/internal/check/member_facts.go:138-143`** — the retention site
   that paired with (1). Retained an unsupported requirement for
   `memberField` and `memberMethod` only; `memberTuple` was absent. Same
   finding. **Resolved:** `memberTuple` now retains
   `requirementUnsupportedComponent`.
3. **`compiler/internal/check/record.go`** (was `:381`, now the same check at
   a shifted line after the enum addition) — the append-time requirement
   validation range, `requirement.Kind < requirementNumeric ||
   requirement.Kind > requirementUnsupportedConstruction`. This was a
   *fourth* site the F1 fix had to touch, and the one most likely to have
   been missed, since adding an enum member after
   `requirementUnsupportedConstruction` would have silently passed while
   adding one before it would have silently shifted the range. **Resolved:**
   the range now ends at `requirementUnsupportedComponent`, the new accepted
   terminal requirement kind — not missed.
4. **`compiler/internal/check/generic_facts.go:17`** — `requirementLiteralFits`
   declared, constructed nowhere in the entire tree. Finding **F2**.

**Sites checked and found clean:**

5. **`compiler/internal/infer/session.go:512-590`** — the constraint-validation
   switch, 12 cases. `constraintHasComponent` present at `:524`. This is one of
   the two sites the prior gap missed; it is now correct.
6. **`compiler/internal/infer/solve.go:98-139`** — the main constraint dispatch,
   16 cases. `constraintHasComponent` present at `:127`.
7. **`compiler/internal/infer/solve.go:481`** — the delayed/literal-defaulting
   group: `case constraintHasField, constraintHasComponent,
   constraintSelectMethod, constraintCallable, constraintIndexable,
   constraintSliceable`. This is the second site the prior gap missed; it is
   now correct. A whole-package `grep -rn "case constraint"` returns hits in
   exactly these two files (12 in `session.go`, 16 in `solve.go`), so there is
   no third constraint-dispatch site anywhere in `infer`.
8. **`compiler/internal/infer/semantic_snapshot.go:600`** — requirement-kind
   range validation, bounded by `RequirementLiteralFits`. `06b` adds no
   `infer`-side requirement kind (`Equatable` is checker-owned and never
   crosses the boundary), so this site needs no change. Confirmed clean.
9. **`compiler/internal/check/record.go:186-406`** — `payloadResources()`,
   which enumerates all **18** (corrected; not 20 — recounted directly against
   `retainedRecord`'s field list, `record.go:20-42`) record payload pointers
   and validates each. `06b` introduces **no new record kind** (06b:26: "`06b`
   never adds an equation, alternative, capability, expected-evidence fact,
   inference variable, publication, instantiation, or method selection"), so
   nothing must be added here. Confirmed clean. F2 is now decided in favor of
   publishing literal-fit requirements directly from `Solution.Requirements()`
   with no new `06a` record, so this site stays clean under the settled
   decision, not merely pending one.
10. **`compiler/internal/types/key.go:103-164`** — the eight `TypeKey`
    decomposition accessors, each switching on `Kind`. `06b` introduces no new
    `types.Kind`, so no extension is needed. Confirmed clean.
11. **`compiler/internal/diagnostic/diagnostic.go`** — checked for a central
    diagnostic-code registry that new `C06xx` codes would need to be added to.
    **There is none**: `Code` is a bare `type Code string` (`:13`) with no
    validation, table, or enumeration. `C0619` and `C0620` can be introduced by
    a string literal alone. Confirmed clean — a genuinely absent failure mode,
    not an unchecked one.
12. **`compiler/internal/check/walk.go`** and the `06a` fact generators —
    checked for a syntax-kind dispatch that `06b` would need to extend. `06b`
    adds no surface kind and performs no traversal (06b:28: "If validation needs
    an inference fact or solved root that `06a` did not retain, the checker has
    a specification or implementation defect"). Confirmed clean.
13. **`compiler/internal/check/config.go:37-72`** — `normalizeConfig`, which
    must gain **six** new default assignments for 06b's limits (corrected;
    not seven — `06b-validation-and-typed-ir.md:78-90` lists exactly six
    numeric limit fields with a six-row default table), **plus** explicit
    `Entry` normalization for the seventh added field. `Entry EntryPoint` is
    not a limit and has no default-value row — it is a mode/symbol pair the
    driver must set explicitly (`EntryRequired` with a valid symbol, or
    `EntryNone` with a zero value), a categorically different kind of
    addition from the six numeric limits. The spec names this site
    explicitly ("`config.go` (06b field extension only)"), so it is a
    *named* integration site, not a hidden one. Listed for completeness.
14. **`compiler/internal/symbol/symbol.go:321-365`** — the immutable resolution
    accessor set `06b` assumes: `Prelude`, `Builtin`, `Runtime`, `Reference`,
    `References`, `Qualifier`, `Bracket`, `Captures`, `CaptureList`, `Members`.
    All ten present. Nothing to extend. Confirmed clean.
15. **`compiler/internal/tir`** — checked for a pre-existing verifier tag table
    or node enumeration that a new node kind would need adding to. **The package
    does not exist**; `06b.7` creates it from nothing. This is why 06b.7 is
    marked Uncertain rather than Ready: there is no existing site to audit, and
    the new one will be the largest kind-enumerating surface in the compiler.
16. **Public checker entry / driver call sites** — checked whether anything
    outside `check` calls into it and would need updating when `run06b` lands.
    `grep -rn "run06a" compiler/internal/` returns only `_test.go` callers of
    `run06a3`; `run06a` has no caller at all, and no `func Check` exists.
    Confirmed clean: `06b.8` wires a new surface with no existing consumers.

**Sites checked for stale documentation rather than code:**

17. **`spec/compiler/proposals/05b-tuple-component-constraint.md:3`** still says
    **"Status: Proposed"**, although `HasComponent` is present in the accepted
    `05b` spec text (`05b-algebraic-inference.md:889`, `:937`, `:1003`, `:1024`)
    and implemented in code. Bookkeeping only; the accepted spec and the code
    agree.
18. **`spec/compiler/proposals/open-language-decisions.md`** §1.1 (tuple access)
    and §1.4 (enum/integer conversion "the two phase-6 documents disagree") are
    both **stale** — both landed, in `a754b39` and `9fcc35f` respectively. Its
    §10 status line ("06a is nearly complete (06a.1–06a.7 accepted, 06a.8
    next)") is also stale. Against 06b's eight slices, its remaining "blocking
    now" entries resolve as: §1.2 constant language — **not blocking** (shipped
    in `06a.2`, consumed as-is by 06b.5b); §1.3 untagged unions — **not
    blocking** (`C0615` is a settled, implementable rejection; the open safety
    model gates a future feature); §1.5 pointer arithmetic — **not blocking**
    (06b:462-466 gives complete rules; only a future unsafe-pointer form is
    open); §1.6 calling conventions — **not blocking** (no adaptation, no
    promotions, C-variadic is `C0604` — all implementable today). Its §2.4
    (slice lifetime) and §2.5 (release-mode faults) match 06b's own two
    explicitly-future items and correctly block nothing. **Conclusion: no entry
    in that inventory blocks any 06b slice's completion bar.** Every finding in
    this report was found by reading code and spec text, not from that
    inventory.

---

## Decisions made on review

The first draft of this report deliberately declined to make three calls that
were genuinely open. Independent review made all three. Recorded here as
decided; applying them to the accepted `06b` spec text is Wave 2 work, not
done in this document.

1. **F2 — literal-fit requirements publish directly from
   `Solution.Requirements()`, with no new `06a` retention site.** Literal-fit
   is solver evidence with a canonical payload and an exact earliest origin
   already retained by `05b`; a parallel `06a` record would create a
   redundant join and would not naturally preserve every negated/composite
   literal payload. `06b`'s text should say: numeric/integral/ordered join
   many `06a` use-site records to one solver requirement; literal-fit
   publishes directly from the solver requirement and retains its canonical
   earliest `Origin.Syntax`, without promising additional related use-site
   labels the way the other three kinds do.
2. **`SourceAlias` cast tagging — a boolean flag** (e.g. `ExplicitCast`), not
   a distinct node tag. An identity `as` cast remains an alias, not a new
   operation or coercion; a distinct tag would unnecessarily expand the
   closed verifier surface for no added information.
3. **`MaxDiagnostics` — a fresh `06b` counter**, sharing the same
   configuration value and `DiagnosticSet` as `06a` but not its running
   count. The accepted spec already implies this: `06b` replaces its own
   final retained diagnostic with `C0619` on overflow, described as a
   property of `06b`'s own budget, not as consuming whatever count `06a`
   happened to leave behind.

Everything else flagged above except F4 (F1, F3, F5, D1, D2, N1, N2) is
mechanical: the amendment is stated because there is only one reading
consistent with already-accepted text and already-shipped code. F4 is
different in kind — its correction is a genuine design addition to `06a`'s
handoff schema (see its section), not a restatement of behavior that already
exists, and needs its own proposal before implementation, the same way
`HasComponent` did.
