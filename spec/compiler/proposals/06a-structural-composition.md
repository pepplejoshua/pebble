# Amendment: explicit structural-composition evidence for control arms

**Status:** Proposed. Owner slice `06a.7`, reopening already-accepted
`compiler/internal/check/control_facts.go` and `record.go`. A second,
smaller reopening of already-accepted **`06a.8`** (`solve_handoff.go`) is
also required — see "B. Cross-record resolution" below; this was missing
from the first draft. Not a documentation fix — new fields, one new record
kind, and one new authoritative helper function, populated from information
`06a` already has at the point it currently discards it.

**Revision history.** First draft: proposed `Composition` without accounting
for `BindingDecl`, which retains no control record at all, and without
distinguishing per-record from cross-record validation. Second draft:
corrected both, added `controlBinding`, split validation correctly across
`validControlRecord`/`generation.addRecord`/`payloadResources`, and moved
`controlBinding`'s retention to the top of `handleBinding` so it no longer
depends on symbol resolution at all. Third draft (this one): closed the
remaining gap — nothing previously proved retained `Composition` was
*correct*, only that it was well-formed and pointed at *some* real record. A
silent then/else swap would have passed every earlier check. Added
`expectedComposition` (§A½) as the one authoritative reconstruction, used
identically by population and by a new exact-equality check in `06a.8`'s
audit, checked before the existing cross-record resolution rather than
instead of it.

## Evidence

On `main` at `0f6125f` (F1 already landed), `06a.7`/`06a.8` accepted:

```pebble
fn f(flag bool) void {
    if flag return; else print 1;
}
```

is legal Pebble. `parser_statement.go:110-121` (`parseIfStatement`) parses
both arms with the general `p.parseStatement()`, not a block-only
production. `control_facts.go:243-262` (`prepareIf`) allocates exactly one
region for the `if` and stamps that same region onto both arms *before*
dispatching to either child; a bare leaf arm never allocates its own region.
For the program above, the `if`'s region has zero child regions, and its two
arms exist only as flat `controlRecord`s — one `controlReturn`, one
`controlPrint` — both naming the same region, with nothing distinguishing
which is which. The same shape recurs for `for`'s initializer/update/body,
and any loop or switch-case body that is a bare non-block statement.

`06a` (`06a-semantic-fact-generation.md`, "Statement and control facts") and
`06b` (`06b-validation-and-typed-ir.md`, "Structural control flow, targets,
and defers") both describe the child-order mapping only circularly, and
never state it, because for the bare-arm case there is no arena "child" for
either document to describe.

### The binding gap

`walk.go:339-341` dispatches every `BindingDecl` (and `ExternBinding`)
through:

```go
case syntax.BindingDecl, syntax.ExternBinding:
    w.handleBinding(ref, node, ctx)
    return w.structuralChildren(ref, node, ctx, tree)
```

`handleBinding` (`declaration_facts.go:190-242`) retains exactly one
`bindingRecord` via `w.retainBinding(...)` (`:236-241`) and nothing else.
`BindingDecl` never appears in `prepareStatement`'s dispatch (the function
that owns every other statement kind's control-facts retention) — confirmed
by reading its full switch, which lists `BlockStmt, IfStmt, WhileStmt,
RangeLoopStmt, ForStmt, SwitchStmt, SwitchCase, ReturnStmt, DeferStmt` for
preparation and `ReturnStmt, BreakStmt, ContinueStmt, PrintStmt,
ExpressionStmt, AssignmentStmt` for finishing. `BindingDecl` is in neither
list. A local binding therefore retains **no `controlRecord` at all**: no
`Region`, no `StatementForm`, nothing.

Two concrete consequences:

1. A `for`-loop initializer that is a binding declaration (`for var step i32
   = 0; ...`) cannot be named by this amendment's own `roleInitializer`
   entry, because `roleInitializer`'s `Arm` would point at a `SyntaxRef` with
   no control record to resolve to via `Result.Control(ref)` — the exact
   accessor this amendment otherwise relies on throughout.
2. Independent of `for`: an ordinary local `let x = 5;` statement inside any
   block has no retained lexical region, no statement form, and no place in
   sequential order at all, as far as `06b`'s control facts are concerned.
   `06b.6`'s reachability, unreachable-statement warnings, defer scope, and
   sequential-flow composition all depend on every statement having a
   control record; a binding silently has none.

## Rejected alternatives

**A documentation table mapping child ordinal to role.** Presupposes the
arena's `Children` list already contains one entry per role; for a bare
non-block arm it does not, because only region-owning statements allocate a
region.

**`06b` inferring arm roles from child regions' own control records.** Fails
for the bare-leaf case (no child region exists) and, even restricted to
all-block arms, fails for `else if`, whose else child is a `controlIf`, not
a `controlBlock`.

**Requiring every arm to be a block.** Would remove the gap by narrowing
what `if`/`while`/`for`/`switch-case` accept — a Pebble language change, not
a compiler fix. Out of scope; `03a-grammar.md`'s `statement` production is
untouched.

**Treating a binding's `for`-initializer specially, without a general
`controlBinding` kind.** Would close the `for`-initializer case but leave
every ordinary local `let`/`var` invisible to `06b.6`'s flow analysis, which
is the larger and more consequential half of the binding gap. Rejected for
leaving known-bad behavior in place solely because this amendment's original
motivating example did not exercise it.

## The design

### 1. Structural-composition evidence on `controlRecord`

```go
type structuralRole uint8

const (
	roleThen structuralRole = iota + 1
	roleElse
	roleInitializer
	roleUpdate
	roleBody
	roleCase
)

type structuralChild struct {
	Role    structuralRole
	Ordinal uint32          // zero-based; nonzero only for roleCase
	Arm     symbol.SyntaxRef
}
```

```go
type controlRecord struct {
	Header                                        recordHeader
	Kind                                          controlKind
	Region, Target                                controlID
	Callable                                      callableRef
	StatementForm                                 statementForm
	Values                                        []controlValue
	Composition                                   []structuralChild // new
	ConditionPresent, ElsePresent, RangeInclusive bool
}
```

`Composition` is the statement-role analogue of the already-accepted
`Values []controlValue`. Resolved by `Arm symbol.SyntaxRef`, not a
`recordID`, because the region-owning statement's own record retains at
*entry* — before its arms are visited — so no arm has a `recordID` yet at
that point; its `SyntaxRef` is already available from `node.Children()`
with no dependency on traversal order. This matches how `06b` already
consumes control facts: `Result.Control(symbol.SyntaxRef) (ControlResult,
bool)` is already `SyntaxRef`-keyed.

`controlRegion.Parent`/`Children`/`Depth` is unchanged and remains the sole
authority for lexical parentage and defer scope. `Composition` is a
separate relationship, carried beside it. The existing rule that record
allocation order supplies sequential statement order within one region is
unchanged and remains sufficient for same-role sequences (a block's
statements, a switch's case *values* via `controlValue{Role: valueCase}`);
`Composition` adds only what that rule cannot express — distinguishing
differently-named roles sharing one region.

### 2. A new closed leaf kind, `controlBinding`

```go
const (
	controlFunction controlKind = iota + 1
	controlBlock
	controlReturn
	controlIf
	controlWhile
	controlRangeLoop
	controlFor
	controlSwitch
	controlSwitchCase
	controlBreak
	controlContinue
	controlDefer
	controlPrint
	controlExpression
	controlBinding // new; appended, no renumbering
)
```

`controlBinding` is a leaf kind (never added to `regionOwningControl`'s
case list). Retained for every **local** `BindingDecl`/`ExternBinding`
while a lexical control region is available, independent of whether the
binding symbol itself publishes successfully — mirroring
`handleBinding`'s own existing behavior, which already retains its
`bindingRecord` regardless of `published` (`declaration_facts.go:227-241`).
Retention must not depend on binding-symbol success, because the statement
occupies sequential position and lexical scope whether or not its symbol
resolved; losing the control record on a damaged binding would make the
*rest* of the block's reachability wrong on top of whatever diagnostic the
binding itself already produced.

**Exact site and gate — corrected on review.** The first draft placed this
retention at the same point as the existing `retainBinding` call, near the
*end* of `handleBinding`, gated on `!global`. That is not literally
independent of binding-symbol success: `handleBinding` returns early,
before reaching that point, whenever `declarationSymbols(ref)` finds no
`SymbolBinding`/`SymbolExternBinding` symbol at all
(`declaration_facts.go:198-200`, `if binding.ID == 0 { return }`) — the one
case where "binding-symbol success" has failed most completely. The
original gate also read `global` from `w.bindingKind(binding, node)`, whose
`global` computation depends on `value.Scope`
(`declaration_facts.go:485-487`) — a field of the very symbol that may not
exist in this failure case, making the check fragile exactly where it needs
to be most robust.

The correct, symbol-independent gate is `ctx.control.region != 0` alone,
checked at the *top* of `handleBinding`, before any symbol lookup:

```go
func (w *walker) handleBinding(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	if ctx.control.region != 0 {
		w.retainControl(ref, ctx, controlEmission{
			kind: controlBinding, form: statementOther, region: ctx.control.region,
		})
	}
	var binding symbol.Symbol
	for _, value := range w.declarationSymbols(ref) {
		...
	}
	if binding.ID == 0 {
		return
	}
	... // unchanged
}
```

This is not merely a safer position for the same check — `ctx.control.region
!= 0` is *by itself* a complete, symbol-free proxy for "this is a local
binding," so `!global` is no longer needed at all. Region-stamping is
activated exclusively by `beginCallableRegion` (`walk.go:366,469`), which is
reachable only from function/callable-body traversal, never from
module-level declaration traversal — confirmed by reading its two call
sites. A module-level binding's context therefore has `ctx.control.region ==
0` unconditionally, regardless of anything about its symbol; a binding
inside a function body has it nonzero, equally regardless of its symbol.
Dropping `!global` removes the only place this design depended on symbol
resolution at all, making "independent of binding-symbol success" a
structural guarantee rather than a claim to verify case by case.

**Why this needs no extra exclusion for parameters or synthetic range
iterators.** Both are structurally unreachable from this code path already,
not by a new guard this amendment adds. `Parameter` is a distinct syntax
kind, dispatched through `structuralChildren`'s own case
(`walk.go:377-380`), and never reaches `handleBinding` — parameters are
never `BindingDecl` nodes. A range-loop iterator is declared as a bare
`Name` child of `RangeLoopStmt`, not a `BindingDecl`; its `bindingRecord`
(`Kind: bindingRangeIterator`) is retained directly inside
`prepareRangeLoop` (`control_facts.go`), never through `handleBinding`
either.

**Two records, one `SyntaxRef`, by design.** `controlBinding`'s own
`controlRecord` and the existing `bindingRecord` both retain with
`Header.Syntax == ref` — two separate records (each with exactly one
payload, satisfying `payloadResources()`'s existing `payloads > 1` rule),
sharing the same syntax location. This is an already-established pattern in
this package (e.g. a member access already retains both a `memberRecord`
and a related `expressionRecord` at overlapping syntax), not something new.
It is also exactly why the cross-record audit (below) must count only
`Control != nil` records: without that restriction, a `for`-initializer
binding's `roleInitializer` arm would find *two* same-`Syntax` records
(`bindingRecord` and `controlBinding`'s `controlRecord`) and reject itself
as ambiguous, even though exactly one of them is the actual statement
control record the audit is looking for.

**Content.** `controlBinding` carries no `Composition` (empty, as for
every leaf kind) and no `Values` — the binding's annotation/initializer
values already live on the separate, already-accepted `bindingRecord`; this
record's sole job is presence, region membership, and sequential position
for `06b.6`'s flow analysis.

**Audited invariants.** `auditFrozen` must require, of *every* retained
`controlBinding` record — not only those reached as a `Composition` arm,
since most local bindings are ordinary block statements named by no arm at
all:

- its `Header.Syntax` names a `syntax.BindingDecl` occurrence, resolved
  through `gen.inputs.Graph` exactly as §B½'s kind check does;
- it is a leaf: `regionOwningControl(controlBinding)` is `false`, so it must
  never appear in the region arena as a region owner;
- its `Composition` is empty;
- its `Region` is nonzero and names an existing region in the frozen
  `controls` slice — a binding statement always sits in a real lexical
  region, which is the entire reason this kind exists.

Any violation is `C0619`. Conversely, and equally audited: no
`controlBinding` record may exist for a global binding, a function
parameter, an extern module-level binding, or a synthetic range-loop
iterator. Globals and extern module bindings are excluded structurally by
the `ctx.control.region != 0` gate (module-level traversal never stamps a
region); parameters and range iterators are excluded because neither
reaches `handleBinding` at all. The audit's job here is to confirm that
structural exclusion actually held, not to re-derive it.

### 3. Per-kind `Composition` — exact role, cardinality, and validity

| Kind | Allowed roles and cardinality |
| --- | --- |
| `controlIf` | exactly one `roleThen`; `roleElse` present iff `ElsePresent` (0 or 1) |
| `controlWhile` | exactly one `roleBody` |
| `controlFor` | `roleInitializer` present iff `ForInitializerPresent` (0 or 1); `roleUpdate` present iff `ForUpdatePresent` (0 or 1); exactly one `roleBody`, always |
| `controlRangeLoop` | exactly one `roleBody` (start/end/iterator are expressions, already covered by `valueRangeStart`/`valueRangeEnd`/`valueRangeIterator`) |
| `controlSwitch` | zero or more `roleCase`, one per authored case, `Ordinal` contiguous ascending from 0 with no gaps or duplicates, in authored order; `roleElse` present iff `ElsePresent` (0 or 1), naming the else `SwitchCase` node itself, `Ordinal` 0 |
| `controlSwitchCase` | exactly one `roleBody` (the case's own values are covered by `controlValue{Role: valueCase}`) |
| `controlBlock`, `controlFunction`, every leaf kind (`controlReturn`, `controlBreak`, `controlContinue`, `controlDefer`, `controlPrint`, `controlExpression`, `controlBinding`) | `Composition` must be empty — no role applies |

Closed invariants, checked wherever the record is validated (see next
section for exactly where):

- `Ordinal == 0` for every role except `roleCase`.
- No two entries share the same `(Role, Ordinal)` — this alone forbids a
  second `roleThen`, a second `roleElse`, etc., since every singleton role
  always carries `Ordinal == 0`.
- A role not in that `Kind`'s allowed set (e.g. `roleUpdate` on
  `controlWhile`) is rejected.
- `Arm != (symbol.SyntaxRef{})` — a zero `Arm` is rejected, exactly like
  every other zero-handle rule in this package.
- `roleCase` ordinals for one `controlSwitch` record are exactly `{0, 1, ...,
  N-1}` with no gap, matching authored case order.

## Where each check actually lives — corrected

The first draft of this proposal said `structuralChild.Arm` "must be a
`SyntaxRef` `06a` actually visited (`w.generation.validSyntax`...)". That is
wrong in a way worth stating precisely: `validSyntax`
(`generation.go:524-534`) proves only that the `SyntaxRef`'s module and node
**exist in the graph** — it is a structural check against the immutable
tree, not a claim that `06a`'s walker visited that node or retained
anything for it. Two genuinely different checks are needed, at two
genuinely different points, and they must not be conflated:

### A. Per-record structural validity — three functions, corrected split

The second draft said `payloadResources()` gains `w.generation.validSyntax(
entry.Arm)`. That is architecturally impossible and a real error, not
imprecise wording: `payloadResources()`'s only receiver is `value
retainedRecord` — a plain value with no `w` and no `generation` in scope
(`record.go:186`, `func (value retainedRecord) payloadResources() ([]
valueID, uint64, bool)`). It cannot call `validSyntax`, which is a method on
`*generation`. Three functions genuinely divide this work, and each gets
exactly the part it has the access to perform:

- **`validControlRecord`** (`control_facts.go:89-`, a closed function over a
  `controlRecord` value alone, no external access needed or used) gains a
  loop over `Composition` checking everything derivable from the record's
  own fields: `Role` in `roleThen..roleCase`; `Ordinal == 0` unless `Role ==
  roleCase`; `Arm != zero`; the per-kind role/cardinality table above
  (mirroring how `ConditionPresent`/`ElsePresent` are already gated by
  `value.Kind`, `:102-106`); no duplicate `(Role, Ordinal)`; and, for
  `controlSwitch`, that `roleCase` ordinals are exactly `{0..N-1}` with no
  gaps. `Composition` must be empty for any `Kind` outside the per-kind
  table's region-owning set. This function needs no generation/graph access
  because none of these are graph-ownership questions — they are all closed
  facts about the record's own shape.
- **`generation.addRecord`** (`generation.go:151-`, receiver `g
  *generation`, which does have `g.validSyntax`) is where graph-ownership
  validation actually belongs, and the precedent is already in the same
  function: it already validates `value.Header.Syntax` via `g.validSyntax`
  (`:155`), and already loops a payload-specific slice of `SyntaxRef`s the
  same way this needs — `value.UnsupportedCallable.TypeParameters`
  (`:176-180`), each checked with `g.validSyntax`, atomic report-and-return
  on the first invalid one, *before* `g.records.append` is ever reached
  (`:264`). Add the identical pattern for `Composition`:
  ```go
  if value.Control != nil {
      for _, entry := range value.Control.Composition {
          if !g.validSyntax(entry.Arm) {
              g.report("control record composition names invalid syntax", value.Header.Span)
              return 0, false
          }
      }
  }
  ```
  placed among `addRecord`'s existing payload-specific checks, before the
  `g.records.append` call.
- **`payloadResources()`** gains only the component charge:
  `components += uint64(len(control.Composition))`, alongside the existing
  baseline `components := uint64(len(value.Values)) +
  uint64(len(value.Controls))`. No `Arm` validation here — that is
  `addRecord`'s job, per above.
- **`cloneRetainedRecord`** (`record.go:116-120`) gains `copy.Composition =
  append([]structuralChild(nil), value.Control.Composition...)`, mirroring
  the existing `Values` copy on the line directly above it. No separate
  change is needed to `frozenRecords.Records()` — it already calls
  `cloneRetainedRecord` for every record.

**Atomicity is unaffected by this split.** `addRecord` already runs every
one of its graph-ownership checks and returns `(0, false)` on the first
failure *before* calling `g.records.append`, which is itself where
`payloadResources()`/`validControlRecord` run. A `Composition` failure at
either stage — graph-ownership in `addRecord`, or closed-shape in
`validControlRecord` — refuses the whole record with no partial commit,
exactly the same as every other payload kind already does.

**None of this can prove an `Arm` resolves to a real, retained statement
record.** At the moment a region-owning statement's own record is
validated, its arms may not have been visited yet (`prepareIf` retains the
parent before dispatching to children) — and even once visited, neither
`addRecord` nor `payloadResources()` ever sees more than one record at a
time, never the whole arena. That is a different kind of check, described
next.

### A½. `expectedComposition` — the one authoritative reconstruction

**A gap the second draft still had.** Everything up to this point proves
`Composition` is internally well-formed (§A) and that each `Arm` resolves to
*some* control record (§B, below). Neither proves the retained content is
*correct*. A bug that silently swapped `roleThen`/`roleElse` — retaining the
else-arm's `SyntaxRef` under `roleThen` and vice versa — would satisfy every
check described so far: `validControlRecord` sees a well-formed pair with
correct cardinality, and the cross-record check finds a real control record
at each `Arm`, since both arms genuinely have one. This is exactly the
"positional interpretation silently treats the else arm as the then arm"
hazard this proposal exists to eliminate, and nothing above actually
eliminates it.

The fix is one authoritative, pure function that derives the exact, ordered
`Composition` a region-owning statement's own record must retain, directly
from its authored syntax — used identically by **both** consumers, so
neither can drift from the other:

```go
// expectedComposition is the sole derivation of a region-owning statement's
// Composition — distinct from controlRegion, which remains the sole
// authority for lexical parentage (see B½). 06a.7's population calls it
// once, at retention time, and uses its result directly. 06a.8's freeze
// audit calls it again,
// independently, from the frozen module graph, and compares the result
// field-for-field against what was actually retained — proving population
// used it correctly and that nothing corrupted the value between retention
// and freeze, not merely that this function's own logic is self-consistent.
func expectedComposition(ref symbol.SyntaxRef, node syntax.Node, tree *syntax.Tree) []structuralChild {
	children := node.Children()
	arm := func(id syntax.NodeID) symbol.SyntaxRef {
		return symbol.SyntaxRef{Module: ref.Module, Node: id}
	}
	switch node.Kind() {
	case syntax.IfStmt:
		var out []structuralChild
		if len(children) >= 2 {
			out = append(out, structuralChild{Role: roleThen, Arm: arm(children[1])})
		}
		if len(children) >= 3 {
			out = append(out, structuralChild{Role: roleElse, Arm: arm(children[2])})
		}
		return out
	case syntax.WhileStmt:
		if len(children) >= 2 {
			return []structuralChild{{Role: roleBody, Arm: arm(children[1])}}
		}
		return nil
	case syntax.ForStmt:
		var out []structuralChild
		if node.Data()&syntax.ForInitializerPresent != 0 && len(children) >= 1 {
			out = append(out, structuralChild{Role: roleInitializer, Arm: arm(children[0])})
		}
		if len(children) == 0 {
			return out
		}
		bodyIndex := len(children) - 1
		if node.Data()&syntax.ForUpdatePresent != 0 && bodyIndex >= 1 {
			out = append(out, structuralChild{Role: roleUpdate, Arm: arm(children[bodyIndex-1])})
		}
		return append(out, structuralChild{Role: roleBody, Arm: arm(children[bodyIndex])})
	case syntax.RangeLoopStmt:
		if len(children) == 0 {
			return nil
		}
		return []structuralChild{{Role: roleBody, Arm: arm(children[len(children)-1])}}
	case syntax.SwitchStmt:
		var out []structuralChild
		caseOrdinal := uint32(0)
		for _, id := range children {
			child, ok := tree.Node(id)
			if !ok || child.Kind() != syntax.SwitchCase {
				continue // a non-case recovery insertion names no role
			}
			if child.Token() == syntax.KwElse {
				out = append(out, structuralChild{Role: roleElse, Arm: arm(id)})
				continue
			}
			out = append(out, structuralChild{Role: roleCase, Ordinal: caseOrdinal, Arm: arm(id)})
			caseOrdinal++
		}
		return out
	case syntax.SwitchCase:
		if len(children) == 0 {
			return nil
		}
		return []structuralChild{{Role: roleBody, Arm: arm(children[len(children)-1])}}
	default:
		return nil
	}
}
```

**Why this cannot reuse the existing `forCondition`/`rangeLoopParts`-style
helpers directly, and why the `for` case is written the way it is.**
`semanticRefs` (`call_facts.go:61-70`), which `forCondition` is built on,
explicitly filters out `Missing`/`Error` children. `rangeLoopParts` filters
`start`/`end` the same way inline. Both are correct for their own,
different purpose — they feed live constraint generation, which has nothing
useful to do with a damaged operand — but wrong for `expectedComposition`,
which must retain a damaged arm's `SyntaxRef` (§C, resolved to retain, not
omit). Reusing them would silently drop exactly the entries this proposal
requires be present.

`ForStmt` is the genuinely hard case. `parser_statement.go:151-190` shows
the parser can insert an extra `Missing`-kind child for a missing semicolon
between clauses (`missingFirst`/`missingSecond`), and — critically — that
insertion is not recorded in `node.Data()`'s flags at all, so a
Missing-kind child at some middle position cannot be told apart, by kind
alone, from a genuinely damaged initializer/condition/update. Naive
forward index arithmetic from the flags (the way `forCondition` does it,
correctly, only because `semanticRefs` has already discarded every
Missing-kind child including the semicolon placeholders) cannot be ported
here without also discarding the damaged-clause case it must preserve.

The resolution does not require distinguishing the two: `body` is
*unconditionally* the parser's last-appended child in every path through
`parseForStatement`, including its early-return sugar form for a
brace-less, semicolon-terminated body — verified by reading every `return`
in that function. `update`, when `ForUpdatePresent`, is *unconditionally*
the child immediately before `body`, since nothing is ever inserted between
them (no semicolon is expected there). `initializer`, when
`ForInitializerPresent`, is *unconditionally* `children[0]`, since nothing
precedes it. Anchoring `roleBody`/`roleUpdate` from the **end** of the
children array, and `roleInitializer` from the **start**, is therefore
exact and requires no knowledge of how many semicolon placeholders — if
any — were inserted in between. `roleCondition` is never computed here at
all, because it is not a `Composition` concern: the condition is an
expression, already fully covered by the existing `controlValue{Role:
valueCondition}`.

`SwitchStmt`'s handling needs no equivalent care: `switchElsePresent`
(`control_facts.go:414-422`) already establishes the right pattern by
filtering on `child.Kind() == syntax.SwitchCase`, which — unlike a
Missing-kind check — naturally and correctly excludes a switch's own
non-case recovery insertion (from `parseSwitchStatement`'s `recoverTo`
fallback) without needing to reason about it specially: such a node simply
is not `SwitchCase`-kind, so it receives no role, exactly as intended.
`expectedComposition` reuses that exact filter.

**Population now calls this function directly, replacing ad-hoc per-kind
construction.** The "Per-kind population" table below is retired in favor
of one call site per `prepareX` function: `emission.composition =
expectedComposition(ref, node, tree)`, assigned exactly once, using the
result as-is.

### B. Cross-record resolution — a new check in `06a.8`'s `auditFrozen`

**This is the corrected scope of this proposal: it also reopens
`compiler/internal/check/solve_handoff.go`, accepted as part of `06a.8`.**
The precedent already exists in that exact file: `auditControlHierarchy`
(`solve_handoff.go:456-`, called from `auditFrozen` at `:435`) proves
"exactly one `controlFunction` record per function root, consistent
`callableRef` throughout" — a cross-record property that cannot be checked
per-record either, for exactly the same reason.

**The audit now runs two checks per region-owning record, strictly in
order, using the same `expectedComposition` population uses:**

1. **Exact-equality reconstruction, first.** Look up the record's own
   `Header.Syntax` in `gen.inputs.Graph` (the same access
   `buildFrozenCompilation` already uses; the tree is not yet discarded at
   this point in `run06a`), call `expectedComposition` on it independently,
   and compare the result against the record's retained `Composition`
   field-for-field: same length, same order, same `Role` at each index,
   same `Ordinal` at each index, same `Arm` at each index. Any difference —
   wrong role, swapped order, missing or extra entry, wrong ordinal, wrong
   `SyntaxRef` — is `C0619`. This is what actually proves correctness, not
   merely plausibility; it is checked *before* resolution, below, because a
   record that fails exact equality should never be trusted enough to have
   its individual arms resolved.
2. **Cross-record resolution, second, only after (1) passes.**
3. **Kind correspondence and lexical placement, third, only after (2)
   locates a unique record** — see §B½ below.

**Corrected on review: the resolution rule must count only control records,
not "any payload kind."** Since `bindingRecord`, `compatibilityRecord`, and
other payload kinds can and do share `Header.Syntax` with the record that
actually names an arm (see `controlBinding`'s "two records, one `SyntaxRef`"
note above), counting every payload kind at that `Syntax` would produce
false ambiguity — a `for`-initializer's `SyntaxRef` genuinely carries *two*
records once `controlBinding` exists (its own `bindingRecord` and its new
`controlBinding` control record), and only the second is the one this check
is looking for. The rule:

> For every retained `controlRecord` with a non-empty `Composition`, for
> every entry whose `Arm` does not name a `Missing`/`Error` syntax node (see
> below), exactly one retained record **with `Control != nil`** has
> `Header.Syntax == Arm`. Records without a `Control` payload —
> `bindingRecord`, `compatibilityRecord`, `expressionRecord`, or any other
> kind that may coincidentally share the same `SyntaxRef` — are not counted
> and must not cause a false ambiguity. Zero matching control records, or
> more than one, is `C0619`.

For a bare-leaf arm (`return`/`print`/etc.), the matching control record is
that leaf's own. For a block or nested-control arm, it is that construct's
own region-owning control record. For a `for`-initializer that is a binding
declaration, it is specifically the `controlBinding` record `controlBinding`
(§2) exists to provide — this is the exact reason that leaf kind is
required, not optional polish: without it, this check would find zero
`Control != nil` records at a binding-initializer's `SyntaxRef` and
correctly, but unhelpfully, reject every such program.

### B½. Kind correspondence and lexical placement

**The remaining gap.** §A½ proves `Composition` names the right *syntax*.
§B proves each `Arm` resolves to exactly one control record. Neither proves
that record is the *right kind* of control record, or that it sits in the
*right region*. A record whose `Header.Syntax` names an `IfStmt` but whose
`Kind` is `controlWhile`, or a then-arm whose control record was somehow
stamped into an unrelated sibling region, satisfies both prior checks. Two
further checks close this, both performed only after §B has located the
unique record, and both skipped entirely for a `Missing`/`Error` arm under
the existing §C exemption.

**1. Closed syntax-kind → control-kind correspondence.** One closed
predicate, shared by the audit, mapping each legal arm's authored syntax
kind to the single control kind its record must carry. Verified against the
real retention sites rather than assumed — every row below was confirmed by
reading the `retainControl`/`controlEmission` call that produces it:

| Arm syntax kind | Required `controlKind` |
| --- | --- |
| `syntax.BindingDecl` | `controlBinding` |
| `syntax.BlockStmt` | `controlBlock` |
| `syntax.ReturnStmt` | `controlReturn` |
| `syntax.IfStmt` | `controlIf` |
| `syntax.WhileStmt` | `controlWhile` |
| `syntax.RangeLoopStmt` | `controlRangeLoop` |
| `syntax.ForStmt` | `controlFor` |
| `syntax.SwitchStmt` | `controlSwitch` |
| `syntax.SwitchCase` | `controlSwitchCase` |
| `syntax.BreakStmt` | `controlBreak` |
| `syntax.ContinueStmt` | `controlContinue` |
| `syntax.DeferStmt` | `controlDefer` |
| `syntax.PrintStmt` | `controlPrint` |
| `syntax.AssignmentStmt` | `controlExpression` |
| `syntax.ExpressionStmt` | `controlExpression` |

Any other non-recovery arm syntax kind, and any mismatch between an arm's
syntax kind and its located record's `Kind`, is `C0619`. The mapping is
resolved exclusively through this closed predicate — never from spelling,
record allocation order, `StatementForm`, or any inferred heuristic.

Note that `AssignmentStmt` and `ExpressionStmt` deliberately share
`controlExpression`, matching what the code actually retains
(`statement_facts.go:68` and `:94` respectively, distinguished only by
`StatementForm`, not by `Kind`). The predicate is therefore a function from
syntax kind to control kind, not a bijection, and must not be implemented
as a reversible lookup.

**2. Exact lexical placement.** After the kind matches, the located record's
`Region` must sit in the correct structural relationship to the parent
record's own `Region`, keyed on whether the arm is itself region-owning —
reusing the existing `regionOwningControl` predicate
(`control_facts.go:79-87`), not a reimplementation:

- **Leaf arm** (`regionOwningControl(child.Kind) == false`): require
  `child.Region == parent.Region`. A bare `return`/`print`/binding arm lives
  directly in its parent's own region — it allocated none of its own.
- **Region-owning arm** (`regionOwningControl(child.Kind) == true`):
  require `child.Region` to exist (nonzero and within the frozen `controls`
  slice) and `controls[child.Region-1].Parent == parent.Region`. A block or
  nested-control arm allocated its own region, whose parent must be exactly
  the region the parent record owns. The `controls[id-1]` indexing matches
  the established convention in the same file (`solve_handoff.go:528`,
  `:533`).

Either failure is `C0619`.

**This is containment validation only, not a second parentage authority.**
`controlRegion.Parent`/`Children`/`Depth` remains the sole source of truth
for lexical parentage and defer scope, exactly as stated throughout this
proposal. `Composition` stores no parent link and no depth; this check
*reads* the existing region arena to confirm the structural relationship
already recorded there is consistent with the structural role
`Composition` claims. It duplicates no data — it cross-validates two
independently-derived views of the same tree, which is precisely what makes
it able to catch a corruption that either view alone would accept.

### C. `Missing`/`Error` arms — retained, not omitted; exempted at audit time

**Reversed on review.** The prior draft omitted a `Missing`/`Error` arm from
`Composition` entirely, to avoid the audit in (B) rejecting it. That
creates a real contradiction: §3's cardinality table requires, for example,
exactly one `roleThen` on every `controlIf` — but a damaged then-clause
under the omission rule would produce *zero* `roleThen` entries, violating
the very cardinality rule stated one section earlier. Omission would need
*conditional* cardinality ("exactly one, unless the arm is damaged, then
zero") and would require moving the "was a required role actually present"
question to whatever stage can tell a genuinely-absent optional clause
apart from a damaged required one by inspecting syntax — reopening exactly
the kind of complexity this proposal is trying to close, not simplify.

The corrected rule keeps cardinality unconditional and moves the leniency
to the one place that can already tell the difference:

- **`validControlRecord`/population retain the structural role
  unconditionally**, exactly matching the presence flags as already
  designed: `ElsePresent`/`ForInitializerPresent`/`ForUpdatePresent` are
  computed from authored structure (e.g. `elsePresent: len(node.Children())
  == 3`) and are already `true` whenever the clause exists syntactically,
  *including* when the parser had to substitute a `Missing`/`Error`
  placeholder for it. No change to how these flags are computed; `Arm` is
  populated the same way for a damaged clause as a clean one, and the
  per-kind cardinality table in §3 needs no modification.
- **`auditFrozen` detects a damaged arm by inspecting its syntax kind**,
  which it can do because the module graph and tree are not yet discarded
  at this point in `run06a` — `buildFrozenCompilation`, in the same file,
  already reads `gen.inputs.Graph`/tree structure directly (e.g. to find
  each module's root `File` node) before the final handoff discards them.
  The same access resolves `Arm`'s node kind: `gen.inputs.Graph.Module(
  Arm.Module).Tree.Node(Arm.Node).Kind()`, checked against `syntax.Missing`/
  `syntax.Error`.
- **When `Arm` names such a node, that specific entry is exempted from
  every resolution-stage check — (B)'s "exactly one matching control
  record", and both of (B½)'s kind-correspondence and lexical-placement
  checks.** Zero matching records is accepted for that one entry, and since
  no record is located, there is nothing to check a kind or region against;
  a damaged arm is skipped wholesale after §A½'s exact-equality check, not
  partially validated. Note this exemption applies *only* from §B onward:
  §A½'s exact-equality reconstruction still applies in full to a damaged
  arm, and `expectedComposition` still retains its exact `SyntaxRef`, which
  is precisely why recovery arms are retained rather than omitted. The
  exemption is per-entry, not per-record: `if flag return; else
  <damaged>;` still requires `roleThen` to resolve normally (the then-arm is
  fine); only `roleElse` is exempted.
- **No new diagnostic is added for this case.** Whatever produced the
  `Missing`/`Error` node in the first place already reported it — a parser
  diagnostic, or an independent `06a` generation error — and that existing
  diagnostic already sets `GenerationHadErrors`, which already prevents
  typed-IR publication downstream. The audit exemption exists precisely so
  this amendment does not *also* fail with a second, redundant `C0619` on
  top of a problem already reported once.

## Per-kind population — superseded by `expectedComposition`

**Retired in favor of §A½.** The first two drafts described population as
per-kind ad-hoc construction, duplicating the same logic
`expectedComposition` now owns exclusively — precisely the drift that would
have let population and the audit disagree without either side's own logic
being wrong in isolation. Population no longer computes `Composition`
itself in any form; each of `prepareIf`/`prepareWhile`/`prepareFor`/
`prepareRangeLoop`/`prepareSwitch`/`prepareSwitchCase` calls
`expectedComposition(ref, node, tree)` once and assigns the result directly
to `emission.composition`, unmodified. `controlBlock`, `controlFunction`,
`controlBinding`, and every other leaf kind never call it and retain no
`Composition` at all — `expectedComposition`'s own `default` case returns
`nil` for any kind it does not explicitly handle, so calling it on a kind
that should not have `Composition` is itself harmless, but population does
not call it for those kinds regardless, since there is nothing to assign.

## Non-goals

- No change to `controlRegion`'s lexical-parentage/defer-scope contract.
- No change to `03a`'s grammar. Bare-statement arms remain legal.
- No new `controlValueRole`. `Composition` is kept separate from `Values`,
  mirroring the existing statement/expression separation.
- No retroactive change to how `Values`/`ConditionPresent`/`ElsePresent`/
  `RangeInclusive` are computed.
- `controlBinding` retains no annotation/initializer content of its own —
  that remains exclusively `bindingRecord`'s job. It exists only to give a
  binding statement a region, a sequential position, and a resolvable
  `SyntaxRef` for `Composition` and for `06b.6`'s flow analysis.
- No generic-body or requirement-kind interaction. `controlBinding` and
  `Composition` are unrelated to `F1`.

## Acceptance

```pebble
fn f(flag bool) void {
    if flag return; else print 1;
}
```

must retain a `controlIf` record whose `Composition` names the `ReturnStmt`
as `roleThen` and the `PrintStmt` as `roleElse`.

```pebble
fn g() void {
    for var step i32 = 0; step < 3; step += 1 {
        if step == 1 { break; } else { continue; }
    }
}
```

must retain a `controlFor` record whose `Composition` names the binding
declaration's own `SyntaxRef` as `roleInitializer`, the postfix update as
`roleUpdate`, and the block as `roleBody`. The initializer's `SyntaxRef`
must resolve, via the cross-record audit, to exactly one `Control != nil`
record — a new `controlBinding` record retained alongside (not instead of)
the existing `bindingRecord` at that same location.

```pebble
fn h() void {
    let x = 1;
    let y = 2;
}
```

must retain two `controlBinding` records, one per statement, both in the
function's own block region, in authored order — proving ordinary local
bindings are no longer invisible to sequential flow.

Every existing `06a.7`/`06a.8` fixture and direct test must continue to
pass unchanged.

## Testing contract

- `Composition` for each of the eight region-owning kinds, covering both
  the bare-statement-arm and block-arm form of every applicable role.
- `roleElse`/no-`roleElse` and bare/block-arm combinations for `if`.
- A `for` with initializer and update both omitted, proving `Composition`
  contains only `roleBody`.
- A `for` whose initializer is a binding declaration, proving
  `roleInitializer`'s `Arm` resolves to a retained `controlBinding` record
  (not merely a `bindingRecord`) via the cross-record check.
- A `switch` with an else case and at least two ordinary cases, proving
  `roleCase` ordinals are zero-based, ascending, contiguous, and distinct
  from `roleElse`.
- A local binding whose symbol publication fails (e.g. a duplicate name),
  proving `controlBinding` is still retained and the block's sequential
  order/reachability around it is unaffected. Separately, and more
  precisely: a local binding for which `declarationSymbols(ref)` resolves no
  `SymbolBinding`/`SymbolExternBinding` at all (`binding.ID == 0`,
  `handleBinding`'s own early-return case) — proving `controlBinding` is
  retained even here, since this is the exact failure the corrected
  top-of-function gate exists to cover, distinct from and stronger than mere
  publication failure.
- A global binding and a function parameter, proving neither retains a
  `controlBinding` record.
- The per-record rejection: a hand-built malformed `controlRecord` for each
  invariant in "Per-kind `Composition`" above (wrong role for the kind,
  nonzero `Ordinal` outside `roleCase`, duplicate `(Role, Ordinal)`, gapped
  case ordinals, zero `Arm`, non-empty `Composition` on a leaf/block/
  function kind) — each rejected atomically, matching
  `TestControlFactsRejectsMalformedRecords`'s existing style.
- The cross-record rejection in `06a.8`: an `Arm` naming a `SyntaxRef` with
  zero retained **control** records, and one naming a `SyntaxRef` with two
  control records — both `C0619`, in a new test alongside
  `auditControlHierarchy`'s existing coverage in `solve_handoff_test.go`.
  Distinct from the `for`-initializer-as-binding case, which legitimately
  has two records (one `bindingRecord`, one `controlBinding` control
  record) at the same `SyntaxRef` and must **not** be rejected — assert this
  positive case alongside the two rejection cases so the "count only
  `Control != nil`" rule is proven, not just stated.
- A `Missing`/`Error` arm (a program with a deliberately damaged then-clause
  or case body): `Composition` **does** retain the corresponding entry
  (cardinality unconditional, per the corrected §C), §A½'s exact-equality
  check still applies to it in full, and the audit exempts that specific
  entry from all three resolution-stage checks — unique resolution (§B),
  kind correspondence, and lexical placement (§B½) — producing no `C0619`
  beyond whatever the parser already reported for the damage. Assert all
  three: the entry's presence in `Composition`, its exact match against
  `expectedComposition`, and the absence of a second diagnostic.
- `controlBinding` itself: for a program with a real local `let`, assert
  every one of §2's four audited invariants — its `Header.Syntax` resolves
  (via `gen.inputs.Graph`) to an actual `syntax.BindingDecl`-kind node, it
  is a leaf, its `Composition` is empty, and its `Region` is nonzero and
  names an existing region. Plus the negative half: a global binding, a
  function parameter, an extern module-level binding, and a range-loop
  iterator each retain **no** `controlBinding` record.
- **Kind-correspondence and lexical-placement regressions (§B½), each
  hand-built so that §A½'s exact-equality check and §B's unique-resolution
  check both pass, isolating the new checks as the only thing that can
  reject them:**
  - a correct `Arm` `SyntaxRef` whose located control record carries the
    wrong `controlKind` (e.g. an `IfStmt` arm resolving to a record whose
    `Kind` is `controlWhile`) — `C0619`;
  - a correct `Arm` and correct `controlKind` for a **leaf** arm, but whose
    `Region` differs from the parent record's `Region` — `C0619`;
  - a correct `Arm` and correct `controlKind` for a **region-owning** arm,
    whose own `Region` exists but whose `controls[Region-1].Parent` is not
    the parent record's `Region` — `C0619`;
  - the three valid positives that must be accepted with no diagnostic: a
    bare leaf arm (`if flag return;`), a local-binding arm (a `for` whose
    initializer is a `let`), and a block/nested-control arm (`if flag {
    ... } else if ...`), each exercising a different branch of the
    leaf-versus-region-owning placement rule;
  - a `Missing`/`Error` arm, confirming it is exempt from kind and
    placement checks as well as from resolution, and still emits no
    additional `C0619`;
  - every failure above is atomic — the arena is unmutated and `run06a`
    returns a failed handoff (`Semantics == nil`,
    `GenerationHadErrors == true`), never a partially-valid one.
- **Atomic `auditFrozen` regressions for the exact-equality check (§A½/§B),
  each a hand-built `controlRecord` whose retained `Composition` disagrees
  with what `expectedComposition` would independently reconstruct from the
  real syntax, proving the audit — not just the per-record shape check —
  catches it:**
  - a graph-owned, otherwise-valid `Arm` naming an unrelated sibling
    statement that genuinely has its own valid control record — proving
    "resolves to some control record" (§B's older check alone) is
    insufficient; only exact reconstruction catches a wrong-but-plausible
    reference;
  - `roleThen`/`roleElse` swapped on an `if` with distinct arms;
  - a `for`'s initializer or update omitted from `Composition` when the
    corresponding presence flag says it should be there, and the reverse —
    a fabricated entry present when the flag says it should not be;
  - two `switch` cases retained in reversed order relative to their
    authored order (same set of arms, wrong order — proving the check
    compares order, not just membership);
  - a `switch` missing one authored case's entry, and one with a fabricated
    extra case entry that does not correspond to any authored `SwitchCase`;
  - the positive control for all of the above: the exact same programs,
    with `Composition` constructed correctly, must be accepted with no
    `C0619` — proving the check does not merely reject everything.
- A full-suite regression run proving every existing `06a.7`/`06a.8`
  fixture and direct test passes unchanged.
