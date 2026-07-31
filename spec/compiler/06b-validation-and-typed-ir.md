# 06b Semantic Validation and Typed IR

`06b` is the post-solve half of phase 6. It begins only after
[06a Semantic Fact Generation](06a-semantic-fact-generation.md) has frozen its
records, called `05b` `Solve` exactly once, created immutable semantic/type
snapshots, discarded the mutable session and AST-bearing inputs, and produced
an immutable in-package `solveHandoff`. `06b` resolves retained roots through
the immutable `Solution`, validates every retained semantic
relationship, performs structural control-flow analysis, and constructs the
closed typed IR accepted by phase 9 and lowering.

The lifecycle is normative:

```text
06a frozen records + immutable 05b SemanticSnapshot + matching Solution
  -> resolve every retained root
  -> validate selected records in deterministic order
  -> validate generic requirements and structural control flow
  -> build and verify closed typed IR
  -> publish checker result and backend input only on complete success
```

`06b` never adds an equation, alternative, capability, expected-evidence fact,
inference variable, publication, instantiation, or method selection. It never
reopens `Solve`, mutates the `Solution`, performs lexical lookup, infers from a
name spelling, rewrites the surface tree, clones a generic body, computes
layout, specializes a generic, or assigns a backend name. If validation needs
an inference fact or solved root that `06a` did not retain, the checker has a
specification or implementation defect; `06b` emits no guessed result.

## Authority and ownership

- [03b Surface Tree](03b-surface-tree.md) owns syntax kinds, authored order,
  recovery nodes, and spans.
- [04b Name Resolution](04b-name-resolution.md) owns `SyntaxRef`, `SymbolID`,
  reference/category identity, members, captures, and bracket modes.
- [05a Semantic Type Store](05a-semantic-type-store.md) owns immutable
  `TypeID` identity and closed key decomposition.
- [05b Algebraic Inference](05b-algebraic-inference.md) owns preparation-time
  `Program`, the tree-free immutable `SemanticSnapshot`, matching `Solution`,
  solved publications, selections, and inference requirements.
- `06a` owns the one traversal, constant evaluation, inference facts,
  publications, and frozen semantic records.
- This document owns post-solve policy, structural control flow, coercion and
  runtime-check selection, and typed-IR construction.
- [07 Generics](07-generics.md) owns specialization and proving a published
  symbolic requirement against concrete type arguments.
- [09 Typed IR and Caching](09-typed-ir-and-caching.md) owns downstream query,
  caching, invalidation, and integration concerns. This document defines the
  IR it receives but does not duplicate phase-9 cache policy.

## Package-private entry and configuration

`06a` and `06b` are tasks inside `compiler/internal/check`, not separately
exported compiler phases. The package-private entry is:

```go
func run06b(
    handoff *solveHandoff,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) *Result
```

`Config` extends the shared checker configuration with:

```go
type EntryMode uint8
const (
    EntryNone EntryMode = iota + 1
    EntryRequired
)

type EntryPoint struct {
    Mode   EntryMode
    Symbol symbol.SymbolID // valid exactly for EntryRequired
}

// Fields added to the 06a Config:
MaxValidationSteps uint64
MaxIRNodes         uint32
MaxIRComponents    uint64
MaxFlowStates      uint32
MaxDeferEdges      uint64
MaxDumpBytes       uint64
Entry              EntryPoint
```

Zero values select:

| Limit | Default |
| --- | ---: |
| validation steps | 16,777,216 |
| typed-IR nodes | 4,194,304 |
| typed-IR components | 16,777,216 |
| structural flow states | 4,194,304 |
| defer references across exits | 8,388,608 |
| one normalized typed-IR dump | 268,435,456 bytes |

`MaxDiagnostics`, `MaxSemanticRecords`, `MaxRecordComponents`,
`MaxControlDepth`, `MaxTrackedPlaces`, and `MaxGenericRequirements` are shared
with `06a`; `06b` does not maintain competing configured values. `06b` starts
a fresh phase-local diagnostic counter at zero while appending to the same
`DiagnosticSet`; diagnostics retained by `06a` do not consume the `06b`
counter. Tests may lower every limit. Work is charged before allocation or
append. A limit failure emits one bounded `C0619`, prevents typed-IR
publication, and may continue only with independent diagnostics that require
no missing result.

Nil handoff, foreign or inconsistent snapshot identities, mutable/session
state in frozen storage, or a semantic snapshot and solution for different
program/solve identities produces `C0619` rather than a panic. `EntryRequired` must carry a
valid symbol from the same resolution snapshot; `EntryNone` must carry zero.

## Exact `06a` to `06b` boundary

The sole input is the package-private `solveHandoff` defined by `06a`:

```go
type solveHandoff struct {
    Compilation         frozenCompilation
    Semantics           *infer.SemanticSnapshot
    Solution            *infer.Solution
    Records             frozenRecords
    Roots               frozenRoots
    Constants           frozenConstants
    GenerationHadErrors bool
}
```

This is an internal ownership boundary, not a public API, serialized format,
or reusable checker protocol. `run06b` consumes it once. The eventual package
entry creates the handoff and immediately passes it to `run06b`; callers
cannot construct or mutate one.

`Compilation` is the 06a-owned tree-free snapshot: copied source metadata;
modules in `ModuleID` order with keys, source/file spans, authored imports, and
source-ordered declaration symbols; the complete dependencies-before-importers
order and root-module identity. `Semantics` contains the exact immutable
resolution used by the program, copied semantic descriptors, runtime
identities, and owned `types.Snapshot`. It contains no graph, module value,
tree, node, source bytes, or syntax-reconstruction callback. 06b may validate
retained spans against copied source lengths, but it cannot read or parse
source text.

Before resolving any record, `run06b` requires nonnil `Semantics` and
`Solution`, `Semantics.Matches(Solution)`, a finalized solution, a nonnil
owned type snapshot, and containment of every `TypeFinal` and retained known
`TypeID`. It also rechecks that all semantic descriptor/template/runtime IDs
are present, copied module/source identities are coherent with
`Semantics.Resolution()`, and no handoff field owns an AST-bearing or mutable
object. A mismatched program or
solve token, incomplete solution, missing type, failed snapshot construction,
or forbidden retained object is `C0619`; validation may retain only independent
diagnostics and cannot publish typed IR. The sole exception to a new `C0619`
is the 06a failed shell with `Semantics == nil`,
`GenerationHadErrors == true`, and the snapshot constructor's existing
`T0512`; 06b rejects it without duplicating the diagnostic. This audit reads
no `Program` or `types.Store` and never interns.

### Frozen access required by validation

The frozen views retain copied, immutable values in allocation/source order:

```go
type frozenRoots struct { /* private ordered storage */ }
func (r frozenRoots) All() []rootedValue
func (r frozenRoots) Root(valueID) (valueRoot, bool)

type frozenRecords struct { /* private per-kind ordered storage */ }
func (r frozenRecords) Records() []retainedRecord
func (r frozenRecords) Controls() []controlRegion
// later closed payload accessors preserve this copied, ordered ownership

type frozenConstants struct { /* private ordered memoized values */ }
func (c frozenConstants) All() []frozenConstant
func (c frozenConstants) Constant(symbol.SyntaxRef) (constantResult, bool)
```

`rootedValue` pairs one `valueID` with the `valueRoot` owned by `06a`; it
contains no
`infer.Term`, `InferID`, known-type shortcut, or derivation recipe. Resolution
is exact:

| `06a` root | `06b` query |
| --- | --- |
| `rootSyntax` | `Solution.SyntaxType(SyntaxRef)` |
| `rootSymbol` | `Solution.SymbolType(SymbolID)` |
| `rootInstantiation` | `Solution.Instantiation(site).Arguments[ordinal]` |
| `rootMethod` | `Solution.Method(site).Arguments[ordinal]` |
| `rootSlot` | `Solution.Slot(SlotID)` after guard selection |

`Roots.All` and `Constants.All` are each consumed once to perform bounded
completeness/duplicate auditing and build local immutable lookup indices.
Every semantic consumer then uses those indices; it does not repeatedly scan
the frozen slices through their linear point accessors. A constant-dependent
rule accepts only `constantKnown`; `constantError` and `constantUnavailable`
are recovery states, not values. There is no syntax or symbol reverse
accessor. For `rootInstantiation` and `rootMethod`, `Parameter` is a zero-based
ordinal and must be less than the corresponding solved `Arguments` length
before access. Every lookup checks exact kind, site, ordinal, selected
alternative, final state, and store ownership. A `TypeError` is recovery,
never a `TypeID`. A missing, duplicate, mismatched, zero, or out-of-range root
is `C0619` and invalidates the owning record. `06b` never reconstructs a type
from adjacent syntax or another record.

Before validation, `06b` resolves all active record handles into an immutable
`solvedRecords` arena. An unguarded record/root has `Alternative.Guarded ==
false`. A guarded record/root is active only when
`Solution.Selection(Choice) == Index`; an inactive guarded slot is expected to
be absent from `Solution.Slot`. Inactive records and roots cannot produce
diagnostics, constants, IR, defaulting, or publications. A missing choice
selection or selected guarded slot is inference failure, not checker policy.

### Frozen handoff audit

The closed 06a handoff is the sole source for every validation and IR input.
This table is the audited join, not a second record definition:

| 06b consumer | Exact frozen 06a source |
| --- | --- |
| expression category, runtime child order, literal/interpolation payload, resolved value symbol, specialized-record join | `expressionRecord` |
| module/import containers, root module, dependency order, module keys/source spans, authored imports, top-level declaration order | `handoff.Compilation` copied modules/sources/order/root; nested declaration order and metadata come from `Semantics.TypeDeclarations`, `Semantics.Signatures`, and immutable resolution symbols |
| binding form, initializer/annotation presence, global/local/extern category, mutability | `bindingRecord` |
| callable kind, identity, convention, parameters/result, body form, variadic/inline bits, captures | `callableRecord` |
| annotation, cast target, `sizeof`, explicit type argument | `typeUseRecord` |
| simple/compound assignment, authored operator, exact place/source, owning statement | `assignmentRecord` |
| directional boundary, ordinal, destination identity/span | `compatibilityRecord` |
| direct/indirect/method/variant target, known/solved convention, fixed/variadic shape, receiver, argument source/destinations, result | `callRecord`, `callTarget`, `callArgument` |
| cast source/destination/result and operation span | `castRecord` and its header |
| exact operator form/family/token, operands/result, generic owner | `operatorRecord` |
| index/slice mode, base/bounds/result, bound presence, known array length | `indexRecord` |
| root symbol kind/mutability and complete ordered dereference/field/tuple/index projection | `placeRecord`, `placeProjection` |
| static/field/tuple/method/variant category, base/result, selected member or copied authored name, name span/ordinal | `memberRecord` |
| aggregate category/declaration/receiver/result, authored field/name bytes/span/value/destination order, declaration field order | `aggregateRecord`, `fieldValue` |
| region parent, derived depth, and ordered children | `frozenRecords.Controls()` entries of the 06a-owned `controlRegion` arena |
| control kind/owning region/target, named-or-anonymous `callableRef`, statement form, condition/subject/case/return/range/print/discard roles, explicit condition/else/range mode, and exact authored structural roles/arms | `controlRecord`, `controlValue`, `controlRecord.Composition`, `structuralChild` |
| lexical region, registration order, checked statement identity | `deferRecord` |
| context expression/forward/none/indirect action, caller identity, callee root, exact Context `TypeID` | `contextFlowRecord`, cross-checked with `Semantics.RuntimeTypes()` |
| generic owner, solved rigid subject, exact supported/unsupported requirement kind and operator/use origin | `requirementRecord` plus `Solution.Requirements(owner)` |
| generic-anonymous rejection and diagnostic span | `unsupportedCallableRecord` |
| switch/global constant value and origin | `frozenConstants` |
| entry symbol, root module, symbol category/signature/type/captures/requirements | `Config.Entry`, `handoff.Compilation.Root`, `Semantics.Resolution()`, `Semantics.Signature`, solved roots, and frozen callable/requirement records |
| bracket mode | immutable `Semantics.Resolution().Bracket(expression Header.Syntax)` plus the expression's specialized record/guard tag |
| choice identity and alternative | `recordHeader.Alternative` and `valueRoot.Alternative`, checked against `Solution.Selection` |
| source location | every `recordHeader.Syntax`/`Span`, retained destination/name/part spans, and copied source ID/path/length metadata |
| semantic type key decomposition and builtins | `Semantics.Types().Key`, `.Kind`, `.Builtins`, `.Contains` |
| immutable reference, qualifier, bracket, capture, member, builtin, runtime, scope, and symbol queries | `Semantics.Resolution()` and its 04b-defined copied accessors |
| prepared templates, declarations/members, signatures, owner/type-parameter order, runtime identities | `Semantics.Template(s)`, `.TypeDeclaration(s)`, `.Signature(s)`, `.OwnerParameters`, `.TypeParameter`, `.RuntimeTypes` |
| deterministic order | monotonic record/value/control IDs, copied component order, `handoff.Compilation.DependencyOrder`, authored module/import/declaration order, `Semantics` ordered accessors, and `Solution` ordered accessors |

This audit is exhaustive: no 06b rule requires an AST/source read, lexical
lookup, reverse root index, new inference fact, or guessed payload. If
implementation later finds a missing item, it must amend 06a's frozen contract
before using it; 06b may not compensate after solve.

## Result and backend publication gate

The unified checker result is immutable:

```go
type Result struct { /* private ordered stores */ }

func (r *Result) Successful() bool
func (r *Result) Solution() *infer.Solution
func (r *Result) SymbolType(symbol.SymbolID) (infer.TypeResult, bool)
func (r *Result) Expression(symbol.SyntaxRef) (ExpressionResult, bool)
func (r *Result) Place(symbol.SyntaxRef) (PlaceResult, bool)
func (r *Result) Conversion(symbol.SyntaxRef) (ConversionResult, bool)
func (r *Result) Call(symbol.SyntaxRef) (CallResult, bool)
func (r *Result) Member(symbol.SyntaxRef) (MemberResult, bool)
func (r *Result) Control(symbol.SyntaxRef) (ControlResult, bool)
func (r *Result) Requirements(symbol.SymbolID) []Requirement
func (r *Result) Instantiation(symbol.SyntaxRef) (infer.Instantiation, bool)
func (r *Result) IR() *tir.Unit
```

The result values are closed snapshots:

```go
type ExpressionResult struct { Syntax symbol.SyntaxRef; Span source.Span; Type infer.TypeResult; Category valueCategory; Node tir.NodeID }
type PlaceResult struct { Syntax symbol.SyntaxRef; Span source.Span; Type infer.TypeResult; Kind placeKind; Root symbol.SymbolID; Writable bool; Projections []PlaceProjectionResult; Node tir.NodeID }
type ConversionResult struct { Syntax symbol.SyntaxRef; Span source.Span; Source, Destination types.TypeID; Class compatibilityClass; Coercion coercionKind; Role compatibilityRole; Ordinal uint32; Node tir.NodeID }
type CallResult struct { Syntax symbol.SyntaxRef; Span source.Span; Kind callKind; Symbol symbol.SymbolID; Convention types.CallingConvention; Variadic bool; Arguments []ConversionResult; Context contextAction; Node tir.NodeID }
type MemberResult struct { Syntax symbol.SyntaxRef; Span source.Span; Kind memberKind; Owner, Member symbol.SymbolID; TupleOrdinal uint32; Node tir.NodeID }
type ControlResult struct { Syntax symbol.SyntaxRef; Span source.Span; Reachable bool; Exits []ExitResult; Target controlID; Defers []tir.NodeID; Node tir.NodeID }
```

`valueCategory`, `coercionKind`, `contextAction`, and exit/projection result
variants are closed mirrors of the validated/IR inventories below. Zero
`NodeID` is valid only in a failed result where no IR was published. Slice
fields are copied on construction and access.

Accessors return values or copies ordered by `SymbolID` or
`(ModuleID, NodeID)`. A failed result remains queryable for final solved facts
and independent diagnostics. `IR()` returns non-nil exactly when all of these
are true:

1. `GenerationHadErrors` is false;
2. `Solution.Successful()` is true;
3. every active retained record resolved and validated;
4. structural control, global, context, generic-body, and entry validation
   succeeded;
5. typed-IR construction and its closed verification succeeded;
6. no error or limit diagnostic from any earlier compiler phase exists for
   the checked compilation.

Warnings, including `C0618`, do not by themselves prevent publication.
Failed checking or solving publishes no `tir.Unit`, phase-9 input, lowering
input, specialization seed, or backend input. Partial IR arenas are discarded
and never exposed.

## Validation order and suppression

`06b` validates active records in this stable order:

1. resolve roots and selected alternatives;
2. declarations, binding forms, globals, and callable declarations;
3. members, aggregates, calls, brackets, indices, slices, and context flow;
4. operators, casts, places, assignments, and compatibility;
5. generic requirements;
6. per-function structural control flow, defers, returns, and reachability;
7. configured entry point;
8. typed-IR construction and closed verification.

Ordering controls deterministic diagnostics and dumps, not semantics. Within
one category use module dependency order, declaration/source order, record ID,
then authored component ordinal. No map iteration, `TypeID` numeric order,
hash seed, diagnostic arrival order, or pointer identity is observable.

When a required root is `TypeError` or the exact occurrence is already
invalidated upstream, dependent policy diagnostics are suppressed. Independent
record and control errors still report. One operation owns one primary
diagnostic; child records mark the failure as explained. The fresh phase-local
`MaxDiagnostics` counter replaces the final retained 06b diagnostic with
`C0619` and suppresses later 06b diagnostics without altering, counting, or
replacing earlier-phase diagnostics.

## Compatibility and coercions

Compatibility is directional from solved source to solved destination.

Each classification consumes one active `compatibilityRecord` or
`castRecord`, its solved source/destination roots, retained role/span, and
`handoff.Semantics.Types().Key` decomposition. It never derives a destination
from context after solve.

The closed classes are:

```go
type compatibilityClass uint8
const (
    compatibleIdentity compatibilityClass = iota + 1
    compatibleImplicit
    compatibleExplicit
    compatibleForbidden
)
```

Identity emits no coercion. Implicit emits a typed-IR coercion only at a
retained assignment, fixed argument, return, record field, tuple component,
optional payload, or accepted branch boundary. Explicit is accepted only by a
retained `castRecord`. Forbidden emits `C0601`. Literal fitting and contextual
`nil`/`none` shaping were inference evidence and are not conversions between
distinct concrete types. Operator peers and switch cases use their stricter
rules rather than this matrix.

### Primitive matrix

| source -> destination | integer | float | `bool` | `char` | `str` | `void` |
| --- | --- | --- | --- | --- | --- | --- |
| integer | identity only for the same builtin; every other pair explicit | explicit | forbidden | forbidden | forbidden | forbidden |
| float | explicit | identity only for the same builtin; other width explicit | forbidden | forbidden | forbidden | forbidden |
| `bool` | forbidden | forbidden | identity | forbidden | forbidden | forbidden |
| `char` | forbidden | forbidden | forbidden | identity | forbidden | forbidden |
| `str` | forbidden | forbidden | forbidden | forbidden | identity | forbidden |
| `void` | forbidden | forbidden | forbidden | forbidden | forbidden | identity |

There is no implicit conversion between distinct concrete numeric types.
`int`, `uint`, every exact-width integer, `f32`, and `f64` are distinct. An
authored cast selects one of these IR operations:

- `IntegerCast`: modulo `2^N` narrowing and two's-complement signedness
  reinterpretation;
- `IntegerToFloat`: IEEE round-to-nearest, ties-to-even, checked finite
  overflow;
- `FloatToInteger`: truncation toward zero with checked range failure;
- `FloatCast`: IEEE round-to-nearest, ties-to-even, checked finite overflow.

The typed operation records source/destination `TypeID` and checked semantics.
Phase 10 decides target lowering and release-mode trap mechanics; it does not
reclassify legality. `char` is never a numeric compatibility family.

### Composite matrix

| Source | Destination | Class and typed operation |
| --- | --- | --- |
| `*A` | `*A` | identity |
| `*A` | `*B`, `A != B` | forbidden |
| pointer | integer, optional, or reverse | forbidden |
| `[N]A` | identical `[N]A` | identity |
| array | any other array or slice | forbidden; authored slicing is an operation, not a cast |
| `[]A` | identical `[]A` | identity |
| slice | other slice or array | forbidden; no variance or mapping |
| identical tuple | same tuple | identity |
| `(A...)` | `(B...)`, same arity | implicit iff every component is identity/implicit; explicit under `as` iff every component is not forbidden; otherwise forbidden; emit ordered `TupleCoerce` |
| tuple | nontuple or different arity | forbidden |
| `A` | `?A` | implicit `OptionalInject`, including `?T -> ??T` |
| `?A` | identical `?A` | identity |
| `?A` | `A` or differing optional payload | forbidden; `!` is a checked operation, not conversion |
| nominal | identical nominal `TypeID` | identity |
| nominal | any different nominal/structural type | forbidden |
| enum | any concrete integer | explicit; total; yields the variant's zero-based declaration ordinal |
| concrete integer | `?enum` | explicit; checked; yields `some` variant when the value names one and `none` otherwise |
| concrete integer | `enum` | explicit; asserted; traps at runtime when the value names no variant |
| union | payload or reverse | forbidden; variant operations are dedicated nodes |
| function | identical function `TypeID` | identity |
| function | any distinct function type | forbidden; no variance, variadic adaptation, or convention adapter |

Transparent aliases have already collapsed. Rigid type parameters are
compatible only by identity while validating a generic body. Every unlisted
primitive/composite pair is forbidden. Recursive tuple classification is
bounded and cycle-free because `05a` keys are immutable finite graphs.

`Pebble` and `C` function types are distinct and incompatible for assignment,
arguments, returns, equality, indirect adaptation, and `as`.

`enum -> integer` and `integer -> enum` apply to `enum` declarations only;
`union enum` tagged-union variants carry a payload, so an integer does not
determine a value, and tagged-union-to-integer conversion in either direction
remains forbidden. A variant's integer is its zero-based declaration-order
ordinal; Pebble enums have no explicit variant values.

`integer -> enum` has two authored destination spellings, and the spelling
selects the failure behavior. Both produce exactly their authored destination
type. `n as ?Color` is the handled form: the value in range yields `some`
variant, and an out-of-range value yields `none` with no trap. `n as Color`
is the asserted form: the same range test runs, but an out-of-range value is
a runtime fault rather than a value, joining the existing family of
runtime-checked operations alongside optional force-unwrap and bounds checks,
and it is bound by the same still-open release-mode fault-behavior decision
below. When the source is a compile-time constant accepted by the `06a`
constant language, the conversion is decided during checking instead of at
runtime: an in-range constant lowers to the variant with no runtime test, and
an out-of-range constant is a compile-time diagnostic in both destination
forms — neither form may compile a known-bad constant into a guaranteed
runtime outcome. `Color.red as ?Color` is optional injection of an
already-`Color` value, not a checked conversion; only a concrete integer
source selects the checked or asserted row.

## Exact operator validation

Each rule consumes one active `operatorRecord`, solved operand/result roots,
the exact token/family, and, for a rigid subject, matching
`Solution.Requirements(owner)` plus its `requirementRecord`.

| Operator | Accepted solved operands | Required result |
| --- | --- | --- |
| unary `-` | any signed integer or float; exact negative literals were handled by `05b`; unsigned nonliteral is forbidden | operand type |
| unary `!` | `bool` | `bool` |
| unary `~` | any integer | operand type |
| `+ - * /` | identical numeric types | that type |
| `+` | additionally `str,str` | `str` |
| `%` | identical integer types | that type |
| `<< >>` | any integer left and any integer right; types may differ | left type |
| `& | ^` | identical integer types | that type |
| `&& ||` | `bool,bool` | `bool`, with left-to-right short circuit |
| `< <= > >=` | identical numeric types, `char,char`, `str,str`, or identical enum nominal | `bool` |
| `== !=` | identical `bool`, `char`, `str`, numeric, pointer, or enum type | `bool` |

Same-type concrete numeric operands are mandatory; no conversion is inserted
inside an operator. Pointer equality requires the exact pointer `TypeID` and
may include a context-shaped `nil`; pointer ordering and arithmetic are
forbidden on an ordinary pointer in this contract, pending the future unsafe-
pointer feature that may deliberately reopen them on a distinct pointer form.
Arrays, slices, tuples, optionals, structs, unions, tagged unions,
opaque externs, functions, and `void` are not equatable in this contract.
Enum ordering is declaration order and requires exact nominal identity.
Division and invalid-shift runtime behavior is represented explicitly for
lowering; compile-time constants already failed in `06a` where applicable.

An accepted rigid parameter use publishes only `Numeric`, `Integral`,
`Ordered`, or checker-owned `Equatable` as specified below. Any concrete
failure emits `C0603`; no observed instantiation can rescue a symbolic body.

## Places, projection, mutation, and assignment

Place validation consumes `placeRecord`, its base/root solved types, retained
root kind/mutability, and selected member/index records. It performs no lookup.
The closed place kinds are:

```text
StoragePlace(SymbolID)
DereferencePlace(base value)
FieldPlace(base place, field SymbolID)
TuplePlace(base place, element ordinal)
IndexPlace(base place/value, index value, checked bounds metadata)
```

A storage place is writable for `var` and writable parameter/storage
categories; `let`, constants, range iterators, functions, types, modules,
variants, and enum values are not writable. Dereference is writable because
raw pointer identity carries no constness. Field and tuple projections inherit
their base writability. Array indexing inherits its place root; slice indexing
is writable through the slice. String indexing is never a place. Calls,
casts, literals, aggregates, force unwrap, `sizeof`, functions, operators,
methods, variants, partial members, and other temporaries are not places.

Prefix `&` requires a writable place because Pebble has no const pointer.
Prefix `*` requires exact `*T` and produces `DereferencePlace(T)`. There is no
automatic dereference. Tuple projection requires an authored base-10 ordinal
within the solved tuple arity. Field projection requires the exact selected
field of the solved nominal declaration. Any violation emits `C0606`.

Simple assignment consumes `assignmentRecord`, its final writable place, and
the paired `compatibilityRecord`; it validates source-to-destination once.
Compound assignment additionally consumes the retained `operatorRecord` and
result-to-place compatibility record. It loads the old value, evaluates the
right operand, applies the exact operator, coerces only at the final store if
the matrix permits it, and stores once. The complete left projection is
evaluated once into IR temporaries. `++`/`--` follow the same load/operator/
store rule with exact literal one, return `void`, and are legal only as an
expression statement or `for` update.

Assignment is never an expression and produces no value node. An invalid
place or compatibility suppresses only dependent store IR.

## Calls, context, members, aggregates, and variants

### Calls and calling conventions

Call validation consumes `callRecord`, solved callee/receiver/result/argument
roots, `Semantics.Signature` for a direct target, `Solution.Method` for a method
site, `Solution.Instantiation` for a generic site, and the matching
compatibility/context-flow records.

- A direct call target must match the exact resolved function `SymbolID` and
  prepared signature retained by `06a`.
- An indirect callee must have a solved `Function` key; its exact convention,
  fixed parameters, result, and variadic bit come from that key.
- A method call must use the exact `Solution.Method(site)` member, bind its
  authored explicit `self` parameter once, and validate remaining arguments
  in order. A method member outside immediate-call position is `C0608`.
- A variant call is accepted only for the exact enum/tagged-union member and
  becomes `VariantConstruct`, not an ordinary function call.
- Nonvariadic arity must match. Calling a C variadic function remains `C0604`;
  no default promotions are defined. Pebble-defined variadics are invalid.
- Every fixed argument uses the common compatibility matrix. No argument is
  revisited or reordered. The call result must equal the prepared/solved
  result exactly.

Every successful call record contains an explicit context action:

```text
Pebble direct/method/indirect call -> ForwardCurrentContext
C direct/indirect call             -> NoContext
```

`ContextExpr` consumes its `contextFlowRecord`,
`Semantics.RuntimeTypes().Context`, and enclosing
callable descriptor. It is legal only in a Pebble-convention body. A C body
has no context source. The hidden context never appears in authored parameter
lists or function `TypeID`; typed IR records it separately on calls. A mismatch
is `C0604` or `C0615` at the context expression as appropriate.

Function/method declaration validation consumes the prepared signature,
callable records including their frozen 04b capture identities, and solved
symbols:

- a body is allowed only for Pebble convention;
- an extern function is C convention and has no body;
- a variadic callable is extern C with the sole variadic group last;
- a method's first expanded parameter is named `self` and has exact containing
  nominal type or pointer-to-that-type;
- a nongeneric anonymous function is a globally hoisted, noncapturing function
  identified in source by its `FunctionTerm` `SyntaxRef`; module/global
  references are legal, any retained enclosing-local capture is `C0617`, and
  no closure/environment IR is fabricated;
- every `unsupportedCallableRecord` for an anonymous function with authored
  type parameters emits `C0608`; it has no successful value or typed IR;
- the retained `inline` bit is copied to callable IR as an optimization request
  and changes no legality or type identity.

### Members and records

Member validation consumes `memberRecord`, solved base/result, any exact
selected `SymbolID` from 04b or `Solution.Method`, the copied authored name for
a type-directed field/variant, and the owning
`Semantics.TypeDeclaration`. When
the member was not statically selected, 06b inspects only the solved nominal
declaration's ordered member descriptors, joins their `SymbolID`s to immutable
symbol records, and matches the copied name; it never searches another
declaration or a lexical scope. The selected member must belong to the solved
receiver declaration's ordered members and its symbol category must match the
retained `memberKind`; the result root must be `TypeFinal` and owned by the
semantic type snapshot. The concrete instantiated field/component result was
already established by 05b `HasField`/`HasComponent`; 06b does not recompute
template substitution or intern a replacement type. Tuple members use the
retained numeric ordinal.
Fields are callable only when their solved type is a function. Unknown,
wrong-category, cross-declaration, or unsupported opaque/union member use emits
`C0605` once.

Record validation consumes `aggregateRecord`, its solved nominal result,
ordered `fieldValue` entries, exact `Semantics` declaration metadata, member
records, and field compatibility records. A struct construction must name
every declared field exactly once. Duplicate/unknown fields are diagnosed in
authored order; missing fields in declaration order. A base-less record is
legal only when inference selected one exact nominal struct. Enums and tagged
unions use variant syntax; untagged-union construction/read/write and extern
opaque construction are `C0615`.

Enum variants are values of the containing enum. A tagged-union variant with
`void` payload takes zero arguments; every other variant takes exactly one
compatible payload. Its result is the containing nominal application.
Base-less `.name` requires expected enum/tagged-union identity and selects
only that declaration's member; it never searches other declarations.

## Brackets, indexing, slicing, and checks

Bracket validation consumes the immutable bracket mode from
`handoff.Semantics.Resolution()`, the frozen expression/specialized record, its
guard tag, and the active `Solution.Selection`. Type mode can produce only
explicit generic application; value mode can produce only one-argument
indexing. Deferred mode accepts only the unique 05b selection with generic
alternative `0` or index alternative `1`. Inactive branch records and guarded
slots have no semantic result.

Index and slice validation consumes `indexRecord`, solved base/index/bound/
result roots, constant values when available, and place metadata:

| Operation | Base | Bounds | Result |
| --- | --- | --- | --- |
| index | `[N]T` | one integer | `T`, checked against `N` |
| index | `[]T` | one integer | `T`, checked against runtime length |
| index | `str` | one integer | `char`, Unicode-scalar index |
| slice | `[N]T` | zero, one, or two integer bounds | `[]T` |
| slice | `[]T` | zero, one, or two integer bounds | `[]T` |
| slice | `str` | zero, one, or two integer bounds | `str`, Unicode-scalar indices |

Every index type may be any concrete integer; floats, `bool`, `char`, and all
composites are rejected. Pointers and tuples are not indexable. Bounds are
half-open; omitted start/end become zero/current length in IR without a fake
syntax node. A provably constant negative, out-of-range, reversed, or overflowed
bound emits `C0609`; otherwise IR contains `CheckedIndex` or `CheckedSlice`.
String checks operate on scalar indices and cannot split an encoded scalar.

Array-to-slice requires authored slice syntax. Slice lifetime, escape,
ownership, and runtime representation are explicitly future and nonblocking;
06b neither rejects an otherwise well-typed slice for escape nor invents a
lifetime proof. Typed IR preserves the array/slice/string source category and
checked bounds so a later accepted lifetime/runtime contract can lower it
without changing type inference.

## Remaining declaration and statement legality

These rules consume the corresponding binding/callable/control records,
solved value roots, exact symbol categories, and constant results. They do not
rescan statement syntax.

- A non-extern local or global `let`/`var` requires an initializer. An
  annotation-only or entirely empty form emits exactly one `C0602`, owned by
  its `bindingRecord`. For an entirely empty form, the solved symbol is
  expected to be `TypeError` from 06a's silent `Session.Error` recovery; no
  `T0510` exists and that recovery does not suppress this type-independent
  policy diagnostic. The annotation and initializer relationship uses its
  retained compatibility record.
- An expression statement is legal only when its retained expression is a
  call or postfix `++`/`--`. Discarding another non-`void` value emits
  `C0612`; a `void` expression is accepted.
- A `print` record accepts `bool`, `char`, `str`, every integer, and every
  float. Pointer, function, composite, nominal, optional, and `void` operands
  emit `C0612`. Interpolation consumes the same printability classification
  for every embedded solved value; it is not a conversion to `str`.
- A range-loop control record requires both solved bounds to have one exact
  integer `TypeID`; its optional iterator must have that identity and is an
  immutable place. Inclusive/exclusive mode is retained in IR. Direction and
  overflow are explicit lowering/runtime behavior, not type inference.
- A `for` update must be its retained assignment or postfix `++`/`--` record.
  An omitted condition is semantic true in the control record and has no
  fabricated expression node.
- `inline` is retained only as an optimization request and changes no
  validation rule or type identity.

## Generic-body requirements

Requirement validation consumes every active `requirementRecord`, solved
rigid subject, `Solution.Requirements(owner)`, exact operator/use, and the
owner's prepared type-parameter order. The published closed interface is:

```go
type RequirementKind uint8
const (
    RequirementNumeric RequirementKind = iota + 1
    RequirementIntegral
    RequirementOrdered
    RequirementEquatable
    RequirementLiteralFits
)

type Requirement struct {
    Owner       symbol.SymbolID
    Parameter   symbol.SymbolID
    Kind        RequirementKind
    Subject     types.TypeID // rigid TypeParameter ID
    Origin      symbol.SyntaxRef
    Operator    syntax.TokenKind
    LiteralKind infer.ExactLiteralKind
    Numerator   string
    Denominator string
}
```

Numeric, integral, and ordered 06a records join the one matching 05b
requirement by `(Owner, Kind, Subject)`; `Origin` is not a join key. The joined
record origins are retained in source order for diagnostics, and the published
origin is the earliest `infer.Origin.Syntax`. `Equatable` is checker-owned and
comes only from its 06a use-site records. `LiteralFits` comes directly from
`Solution.Requirements(owner)` with its canonical literal payload and earliest
`Origin.Syntax`; 06a retains no duplicate literal-fit record and 06b promises
no additional use-site label for it.

Normalize by owner `SymbolID`, declared parameter ordinal, requirement kind,
then first source origin. Equivalent duplicate uses publish one interface
requirement; numeric/integral/ordered/equatable use-site records remain
available as stable related labels. A missing or mismatched required join is
`C0619`, not a newly inferred requirement.

`Operator` is the earliest joined use site's authored token for
numeric/integral/ordered/equatable and zero for `LiteralFits`. `LiteralKind`,
`Numerator`, and `Denominator` are populated exactly for `LiteralFits` and are
zero/empty for every other kind.

Concrete operands are validated immediately by the operator/literal rules.
At a generic call, `06b` attaches the normalized declaration requirements to
the immutable instantiation reference. Phase 7 proves them against concrete
arguments and owns specialization-time diagnostics/caching. `06b` neither
specializes nor accepts a body based on a call site.

Field/tuple-component/method/index/call requirements on an otherwise
unconstrained parameter, conversion to/from a parameter, layout, printing, and
construction have no accepted requirement kind and emit `C0610` in the generic
body.

## Constants, globals, and `sizeof`

Global validation consumes the binding record, resolved symbol, initializer
root, and `frozenConstants` classification. Every non-extern global `let` or
`var` requires an initializer accepted by the 06a constant language.
Annotation-only and missing initializers use the binding record's already-owned
`C0602` and emit no second diagnostic during global classification; a
nonconstant global initializer emits `C0616`. Extern bindings require an exact
annotation and no initializer. Dynamic initialization, implicit zeroing, and
uninitialized locals are not accepted.

`sizeof T` consumes the type root retained by `06a`; it rejects `void`,
function, unresolved rigid/generic, and opaque extern types with `C0615`.
The typed node stores `TypeID(T)` and result `uint`. It contains no size and
performs no layout query; phase 9/10 computes the target-specific value.

Switch constant duplicate checks consume only `frozenConstants`, never host
evaluation or the solver.

## Structural control flow, targets, and defers

Control validation consumes the frozen `controlRegion` arena plus
`controlRecord`/`deferRecord`, solved conditions/subjects/cases, constant
values, compatibility/member records, and callable result types. It does not
walk syntax. The arena is the sole hierarchy: `Parent`, derived `Depth`, and
ordered `Children` are never taken from or duplicated in a control record.
06b rechecks the 06a freeze invariants before flow analysis: contiguous IDs;
root depth one; earlier valid parents; exact parent-depth increments; each
nonroot in exactly one derived child list; edge count equal to regions minus
roots; valid record/defer regions and targets; one function record per root;
and consistent `callableRef` throughout each function tree. Failure is
`C0619` and prevents IR.

Because 06a compared each composition with the surface tree, checked graph
ownership and syntax-kind/control-kind correspondence, then discarded the
tree, 06b trusts that audited portion of the accepted handoff and does not
pretend to repeat it. For a handoff with `GenerationHadErrors == false`, it can
and does recheck the remaining tree-free invariants: valid role/ordinal ranges
and exact cardinality for the parent kind; nonzero unique arms; exactly one
matching frozen control record per arm; exact leaf-versus-region-owner lexical
placement in the frozen arena; and component/record limits. A damaged handoff
already has `GenerationHadErrors == true`, so recovery arms require no
tree-free classification and can produce neither flow nor IR. Any disagreement
in a supposedly successful handoff is `C0619`.

The `controlRegion` arena remains the sole authority for lexical parentage,
depth, ordered child regions, and defer scope. `controlRecord.Composition` is a
separate exact structural-role relation; it does not duplicate or replace
parentage. Flow analysis follows this composition and never assigns meaning to
a child-region ordinal. The closed authored roles are:

```text
controlIf         -> roleThen, optional roleElse
controlWhile      -> roleBody
controlFor        -> optional roleInitializer, optional roleUpdate, roleBody
controlRangeLoop  -> roleBody
controlSwitch     -> authored roleCase/roleElse arms; roleCase ordinal 0..n-1
controlSwitchCase -> roleBody
```

Conditions, bounds, subjects, and case labels remain `controlValue` roles and
are not structural statement arms.

Each non-recovery arm resolves to exactly one control record, whose closed
control kind and lexical placement were audited by 06a. A local binding is the
leaf `controlBinding` and participates in its enclosing sequential statement
order like every other leaf. `Missing`/`Error` arms remain present in exact
composition but imply `GenerationHadErrors`; 06b does not assign them flow or
IR. Record allocation order supplies ordinary statement order within one
region. Flow analysis may compute reachability and exit sets, but it may not
manufacture, reparent, reorder, or reinterpret a region or arm.

Each statement maps incoming reachability to this exit set:

```text
Fallthrough
Return
Break(controlID)
Continue(controlID)
Diverge
```

Sequential composition passes only `Fallthrough`. A statement with no incoming
edge is still independently type/record validated but does not contribute an
exit. Emit warning `C0618` once at the first statement of each contiguous
unreachable region; nested unreachable regions do not duplicate the warning
until reachability resumes and ends again.

- `return` produces `Return` after its value compatibility and defer edge.
- `break` targets the nearest enclosing loop or switch candidate retained by
  `06a`; `continue` targets the nearest loop and ignores switches. Missing or
  inconsistent targets emit `C0611`.
- `if` unions both arms; missing `else` contributes `Fallthrough`.
- A loop consumes matching `Continue` as a back edge and matching `Break` as
  loop fallthrough. `for ;;` and a condition proven constant `true` have no
  fallthrough unless a reachable matching break exists. Other loops may fall
  through conservatively.
- A switch consumes its matching break. Cases do not fall through.
- Enum/tagged-union switches require exact-declaration variant cases, reject
  duplicate variants, and are exhaustive when every variant or `else` exists.
  A tagged-union case narrows only its dominated case region.
- Scalar switches accept `bool`, `char`, `str`, or integer subjects and
  same-kind constant cases. Duplicate values are rejected. Boolean is
  exhaustive with both values; other scalar switches require `else`.

Floating, pointer, optional, tuple, array, slice, struct, union, function, and
opaque subjects are invalid switch categories. Exhaustive switch exits are
the union of reachable case exits; nonexhaustive switches add fallthrough.
Variant declaration ordinals and exhaustiveness use the declaration-complete
ordered `Semantics.Resolution().Members(declaration)` list, not the filtered
`Semantics.TypeDeclaration(declaration).Members` list. Each member is then
joined to its immutable symbol descriptor to confirm variant category.

Every `deferRecord` registers a checked statement with its lexical region in
authored order. Deferred expressions execute at exit, not registration.
Deferred blocks, bindings, assignments, calls, prints, conditionals, loops,
and switches are allowed. Deferred `return`, `break`, `continue`, or nested
`defer` emits `C0613`.

For each fallthrough, return, break, and continue edge, compute the lexical
regions crossed between source and destination. Attach their defers in
innermost-region-first and reverse-registration order. Defers in regions not
crossed do not run. Each attached reference is charged to `MaxDeferEdges`.
Typed IR stores the exact ordered defer chain on the exit edge; lowering may
expand it but may not recompute lexical behavior.

## Returns and entry point

Return validation consumes return control/compatibility records and the solved
result root from the named or anonymous `callableRecord` (joined to the
prepared signature for named callables). Bare return requires exact `void`;
value return is forbidden for `void`; otherwise the common compatibility
matrix applies. Compatibility is validated before definite return.

Every reachable path of a non-`void` function must end in `Return` or
`Diverge`; remaining fallthrough emits `C0607` at the declared result and
smallest fallthrough edge. A `void` function receives an `ImplicitReturn`
typed statement exactly when its end is reachable; its defer chain is the
function-end fallthrough chain.

For `EntryRequired`, validation consumes only the configured `SymbolID`, its
immutable resolution descriptor, `Semantics.Signature`, solved symbol type,
callable/capture records, requirements, and root-module identity. It never
searches for `main`. The entry must be one nonmethod, nongeneric, nonvariadic,
nonextern Pebble function in the root module, with zero authored parameters,
no captures or requirements, and result exactly `void` or `int`. `int` is
process status; `void` means successful status. `EntryNone` performs no entry
validation. Failure is `C0620`.

## Closed typed IR

The implementation lives in `compiler/internal/tir`. IDs are nonzero,
snapshot-local, monotonically allocated, and not durable cache keys:

```go
type NodeID uint32
type FunctionID uint32
type RegionID uint32
type TempID uint32

type Unit struct { /* immutable ordered stores */ }
```

`NodeID` is the common closed-store identity. The expression-category accessor
exposes it as phase 9's conceptual `ExprID`; callers cannot reinterpret a
statement/place/declaration node as an expression. This keeps one source map
and one stable allocation order without creating parallel pointer-owned IR
stores.

All constructors are package-private to `check`/`tir` integration. Accessors
return copies or iterators. A unit retains its owning store/snapshot identity,
ordered modules/declarations/functions/globals, node store, source map,
requirements, and instantiation references. It contains no `InferID`, term,
constraint, choice, unresolved name/member, mutable AST pointer, layout,
backend name, or specialization body.

### Complete node inventory

Every accepted surface kind has one closed disposition below. Runtime-bearing
occurrences map by `SyntaxRef` to exactly one semantic node. Pure syntax
termination/recovery and structural labels are validated/folded as stated and
never become fake runtime values. `06b` follows `expressionRecord` and the
specialized frozen records; it does not reread AST children. Synthetic nodes
have their originating operation's span and an explicit synthetic role.

The leading node identifiers in the seven category blocks below, in their
written order, form the exact closed `tir.NodeKind` enumeration. Production
declares them as one contiguous nonzero `iota` range with a private exclusive
upper bound; it
adds no unnamed tag and omits none. Category metadata, normalized dumping, and
verification are indexed or exhaustively dispatched from that same range so
the inventory cannot drift between independent switches.

| 03b surface kinds | Closed validation and IR disposition |
| --- | --- |
| `File` | one ordered `Module` container |
| `EndOfFile` | dispatched and audited entirely by 06a; no 06b input or runtime node |
| `ImportDecl` | `Import` identity/container, no runtime node |
| `BindingDecl`, `ExternBinding` | global/local/extern declaration plus `Initialize` where authored |
| `TypeDecl`, `ExternType`, `FieldDecl`, `VariantDecl` | resolved declaration/type/member metadata nodes from `Semantics` plus immutable symbol records |
| `StructType`, `UnionType`, `EnumType` | folded into the owning `TypeDeclaration`; no standalone node or syntax query |
| `FunctionDecl`, `ExternFunction`, `FunctionTerm`, `Parameter`, `TypeParameter` | callable/declaration containers; nongeneric valid `FunctionTerm` becomes `HoistedFunctionValue`; generic `FunctionTerm` fails `C0608` |
| `ExternDecl`, `ExternBlock` | structural wrappers folded into their source-ordered contained declarations; no standalone node or syntax query |
| `BlockStmt`, `ReturnStmt`, `IfStmt`, `WhileStmt`, `RangeLoopStmt`, `ForStmt`, `SwitchStmt`, `SwitchCase`, `DeferStmt`, `PrintStmt`, `BreakStmt`, `ContinueStmt`, `AssignmentStmt`, `ExpressionStmt` | the corresponding closed statement/control node below |
| value-position `Name`, `Path` | `SymbolValue`; type/qualifier/label positions use their declaration/type/aggregate owner and produce no value |
| `Literal`, `InterpolatedString`, `ContextExpr`, `SomeExpr`, `SizeofExpr`, `GroupedTerm`, `TupleTerm` | corresponding value/check/type-use node; shared type-position forms become `TypeUse` |
| `ArrayExpr`, `ArrayRepeatExpr`, `RecordExpr`, `PartialMemberExpr` | aggregate/member value node selected by frozen expression plus aggregate/member records |
| `RecordField` | one ordered field entry inside `RecordConstruct`; its label/recovery children create no standalone value |
| `PrefixTerm`, `PostfixExpr`, `BinaryExpr`, `CastExpr` | resolved operator/check/coercion node; type-position shared forms become `TypeUse` |
| `CallExpr`, `BracketApply`, `SliceExpr`, `MemberExpr` | resolved call/generic/index/slice/member node; inactive bracket alternatives produce no node |
| `OptionalType`, `SliceType`, `ArrayType` | `TypeUse` |
| `Missing`, `Error` | recovery only; no successful mapping or IR node |

**Declarations and nonvalue structure**

```text
Module, Import, TypeDeclaration, FieldDeclaration, VariantDeclaration
FunctionDeclaration, ExternDeclaration, GlobalDeclaration, LocalDeclaration
ParameterDeclaration, TypeParameterDeclaration, TypeUse, Block
```

Imports and type/container nodes carry resolved identities/order only and
generate no runtime value. `Module` and `Import` nodes come only from the
tree-free compilation snapshot; top-level declaration order comes from each
frozen module. Declaration identity/span/category comes from its frozen symbol
and record, while nested member/parameter order comes from `Semantics` and
resolution descriptors. Structural type bodies and extern wrappers are folded
as stated above. Recovery nodes have no successful mapping.
Callable declarations carry exact convention, ordered authored parameters,
result, variadic/body form, retained `inline` request, and `FunctionID`.
`HoistedFunctionValue` refers to the nongeneric anonymous callable allocated in
source order from its `FunctionTerm` `SyntaxRef`; it contains no environment.

**Statements and control**

```text
Initialize, Store, CompoundStore, ExpressionStatement, Print
Return, ImplicitReturn, If, While, RangeLoop, For, Switch, SwitchCase
Break, Continue, DeferRegister
```

Each control node names explicit regions/targets and ordered children. Every
exit carries its ordered defer chain. A switch case carries its constant or
variant identity and optional narrowing fact.

**Values**

```text
BoolLiteral, CharLiteral, StringLiteral, IntegerLiteral, FloatLiteral
NilPointer, NoneOptional, SomeOptional, TupleValue, ArrayValue, ArrayRepeat
RecordConstruct, HoistedFunctionValue, SymbolValue, EnumVariantValue, ContextValue
InterpolatedString, SizeofType, PrefixValue, BinaryValue, ShortCircuitValue
FieldValue, TupleElementValue, GenericFunctionValue, SourceAlias
```

06b exact-decodes the frozen numeric bytes into canonical integer/rational IR
payloads, verifies them against the solved materialized `TypeID`, and discards
the source spelling. Boolean, character, string, and interpolation text use
the frozen decoded payloads. No phase after 06b parses literal syntax. A symbol
value contains `SymbolID`; a field/variant contains the selected member identity.
`SourceAlias` represents a successful grouping/shared surface occurrence whose
semantic value is its one already typed child; it preserves the occurrence's
own `SyntaxRef` and span without creating a coercion or second evaluation. Its
payload has `ExplicitCast bool`. Grouping and other shared aliases set it
false; an authored identity `as` cast sets it true. There is no second alias
tag and no identity coercion node.

**Places and loads**

```text
StoragePlace, DereferencePlace, FieldPlace, TuplePlace, CheckedIndexPlace
Load
```

Place nodes carry place type, root/projection identity, writability, and span.
Values read from places use `Load`; assignment destinations use the place
directly. String indexing is a value check, never a place.

**Calls and construction**

```text
DirectCall, IndirectCall, MethodCall, VariantConstruct
```

Every call carries exact result and function `TypeID`, convention, selected
symbol/member when present, ordered fixed arguments after coercion, generic
instantiation reference when present, and hidden context action. Method calls
carry the explicit receiver exactly once.

**Coercions and runtime checks**

```text
IntegerCast, IntegerToFloat, FloatToInteger, FloatCast
OptionalInject, TupleCoerce
CheckedOptionalUnwrap, CheckedIndex, CheckedSlice
CheckedArithmetic, CheckedNegate, CheckedShift
EnumToInteger, OptionalIntegerToEnum, CheckedIntegerToEnum
```

No identity coercion node exists. `TupleCoerce` owns ordered component
coercions and evaluates its source once. `CheckedArithmetic`, `CheckedNegate`,
and `CheckedShift` retain the exact operator and fault category while leaving
release-mode response to phase 10. Check nodes contain enough semantic metadata
for lowering to implement the chosen rule without inspecting syntax.
`OptionalIntegerToEnum` and `CheckedIntegerToEnum` share one range test
against the destination enum's variant count and differ only in outcome:
the former produces `?Enum` and never faults, the latter produces `Enum` and
joins `CheckedArithmetic`/`CheckedShift` under the same release-mode fault
policy. A constant source that names no variant is rejected in `06b` and
lowers to neither node.

**Evaluation sequencing**

```text
TempBind(TempID, value-or-place)
TempRead(TempID)
Sequence(ordered nodes, result)
```

Use temporaries whenever a semantic expansion would otherwise evaluate an
authored child more than once: compound stores, `++`/`--`, method receiver
binding, tuple coercion, checked index/slice bases and bounds, and any defer
edge expansion representation. Temp allocation follows containing node then
authored child order. A temp never changes user-visible lifetime or escapes
its function.

### Typed-IR invariants

For every published unit:

1. every value/place has one valid store-owned `TypeID`; statement/container
   nodes have an explicit `void`/nonvalue category;
2. every source name/member/operator/bracket/call has already been resolved to
   a `SymbolID`, member identity, operation kind, convention, and coercion;
3. every source child is evaluated once in authored left-to-right order;
4. every implicit/explicit conversion is represented by the exact node above;
5. every numeric-cast, arithmetic, shift, optional, index, or slice dynamic
   failure is an explicit operation/check node with a closed fault category;
6. every place is closed, projected, and has final writability;
7. every control exit has a target and exact defer chain;
8. every generic reference has a declaration `SymbolID`, ordered solved
   arguments, and normalized requirements; no specialization is performed;
9. every Pebble call has explicit hidden-context propagation and every C call
   has none;
10. every node has an exact authored span or an origin span plus synthetic
    role; no generated node claims a fabricated source location;
11. node/module/declaration/function/component order is deterministic;
12. the backend and phase 9 can lower/query the unit without reading the
    surface tree, resolution tables, `05b` solution, or checker records.

The IR verifier checks closed tags, valid IDs, ownership, child kinds, type
agreement, source-map completeness, temp dominance/single definition,
region/target membership, defer-edge order, context action versus convention,
and absence of recovery values. Verification failure is `C0619` and discards
the unit. Its dispatch is total over the contiguous `NodeKind` range: zero and
out-of-range tags fail; every in-range tag executes its exact category/payload
checks; no default branch accepts a node. One table-driven totality test walks
the complete range and proves that category metadata, verifier dispatch, and
normalized dumping cover the same tags. Per-tag tests construct at least one
valid node and one malformed payload/child case through test-only builders.

### Stable publication and dumps

Allocate modules in dependency order, declarations/functions in source order,
and nodes by deterministic validated-record construction order with authored
children first unless the node inventory requires an enclosing header ID.
Numeric ID values are debug data; normalized dumps sort by these specified
semantic orders and print symbolic labels, exact `TypeID` formatting, source
IDs with byte-span offsets, coercions/checks, context actions, targets, temps, and
requirements. They never print pointers, hashes, `InferID`s, map order, or
backend names. Dump generation is bounded by `MaxDumpBytes`, never updates a
golden automatically, and is not a cache serialization format.

## Diagnostics

The phase-6 codes are:

| Code | Meaning |
| --- | --- |
| `C0601` | incompatible assignment, argument, return, field, tuple component, or cast |
| `C0602` | invalid binding initialization form |
| `C0603` | invalid operator operands |
| `C0604` | invalid call, method, arity, convention, variadic use, or context propagation |
| `C0605` | invalid member, record, or variant operation |
| `C0606` | expression is not a valid writable place |
| `C0607` | invalid return form or reachable non-void fallthrough |
| `C0608` | invalid callable/method declaration, generic anonymous function, or bound-method value |
| `C0609` | invalid indexing, slicing, or statically invalid bounds |
| `C0610` | unsupported or inconsistent generic requirement |
| `C0611` | invalid switch case, `break`, `continue`, or target |
| `C0612` | invalid expression statement or printable value |
| `C0613` | invalid deferred statement |
| `C0614` | invalid or over-limit constant expression (`06a`) |
| `C0615` | operation requires unavailable layout/union/opaque/context policy |
| `C0616` | nonconstant global initialization |
| `C0617` | prohibited anonymous-function capture |
| `C0618` | deterministic unreachable-statement warning |
| `C0619` | checker limit, inconsistent handoff, or closed-IR failure |
| `C0620` | invalid configured entry point |

Diagnostics use retained spans and identities. Related labels identify the
destination, declaration, operand, requirement origin, target, earlier case,
or fallthrough edge in stable order. Type text uses the deterministic semantic
formatter outside `types.Store`. Diagnostics expose no solver internals.

## Recovery, limits, fuzzing, and race safety

`06b` accepts partially failed solutions only for type-independent retained
diagnostics (`C0602` empty form, `C0608`, and `C0617`) or independent policy
checks whose type-dependent roots are all `TypeFinal`. It does not validate a
type-dependent relationship with an error endpoint, manufacture a recovery
`TypeID`, or publish partial IR. Independent functions and control graphs
remain bounded and queryable.

All record, root, type-key, compatibility recursion, control state, defer
edge, IR node/component, dump, and diagnostic work is charged. Composite
validation uses explicit stacks or reducers bounded by the 05a key depth and
06b counters. Numeric IR decoding reuses the already enforced 05b literal
byte/bit/exponent limits and is also charged to validation work; disagreement
with the successful solved literal is `C0619`. No malformed frozen graph,
adversarial nesting, huge tuple,
defer fan-out, or repeated error can cause unbounded recursion, allocation,
retry, or output.

Fuzz targets construct valid and damaged immutable handoffs through test-only
builders; they cover every record tag, selected/inactive alternatives, type
key pair, control-region graph, defer exit, and IR verifier tag. Assertions are
no panic, bounded work/output, deterministic normalized result, and no IR on
failure. Fuzzing never forges production mutable solver state.

Frozen compilation/fact inputs, `SemanticSnapshot`, `types.Snapshot`,
`Solution`, successful `Result`, and `tir.Unit` are read-only after
publication. One `run06b` construction is single-owner. Tests
may concurrently read a frozen result/unit and may run independent 06b
validations over independent handoffs. There is no package-global checker,
formatter, current function, current region, dump buffer, or cache. The race
suite must cover concurrent read access and independent compilations.

## Tests and source fixtures

Source behavior lives under:

```text
tests/check/validation/
  valid/*.peb
  valid/multimodule/<case>/main.peb
  invalid/C0601/*.peb ... invalid/C0620/*.peb
  recovery/*.peb
tests/check/ir/
  valid/*.peb
  recovery/*.peb
  *.tir.golden
```

Ordinary `.peb` fixtures are primary. `.tir.golden` is reserved for compact
closed-IR shape/order cases and is never rewritten by normal tests. Direct Go
tests own matrix cells, solved-record/root joins, exact node tags, exit sets,
defer chains, limit counters, and verifier failures.

Required conversion coverage includes every primitive family pair, every
composite row, same/different numeric builtins, positive identity/implicit/
explicit cases, and every forbidden implicit/cast case. Required control
coverage includes nested loops/switches, target shadowing, constant-true loops,
exhaustive/nonexhaustive switches, narrowing, invalid targets, return
compatibility before definite return, unreachable regions, every lexical exit
through zero/one/many defers, and void implicit return.

Required recovery coverage combines failed solve, inactive alternatives,
upstream error symbols, conversion failure, control failure, requirement
failure, and a later independent valid declaration. Required resource coverage
lowers every shared/06b limit. Fuzz seeds and race tests are part of completion,
not deferred hardening.

Common verification from `compiler/` is:

```sh
GOCACHE=/tmp/pebble-codex-gocache go test ./...
GOCACHE=/tmp/pebble-codex-gocache go test -race ./...
GOCACHE=/tmp/pebble-codex-gocache go vet ./...
```

Use a fresh external cache directory when isolation is required. From the
repository root also run `git diff --check`. No verification command updates
fixtures or writes build artifacts into the repository.

## Dependency-ordered implementation slices

Implementation starts only after the accepted 06a structural-composition
amendment is implemented and its complete 06a.8 handoff is accepted. The
dependency order is normative:

```text
06b.7a
  -> 06b.1
  -> 06b.2
  -> 06b.3a, 06b.3c, and 06b.5b where file ownership permits
  -> 06b.3b after 06b.3a
  -> 06b.4 after the relevant 06b.3 member/index decisions
  -> 06b.5a
  -> 06b.5c after 06b.5a
  -> 06b.6 after compatibility/member/place decisions
  -> 06b.7b
  -> 06b.8
```

Independent nodes on one line may run concurrently only with exclusive files.
The concrete snapshot implementation prerequisites for 06b.1 remain
mandatory.

### Slice 06b.7a: closed TIR store and total verifier

**Owned files:**

```text
compiler/internal/tir/id.go
compiler/internal/tir/node.go
compiler/internal/tir/unit.go
compiler/internal/tir/verify.go
compiler/internal/tir/*_test.go
```

**Input/output:** implement the immutable ID/store/unit foundation, the exact
closed `NodeKind` range above, category metadata, normalized synthetic-unit
dumping, and the total verifier. Test-only builders may construct valid and
damaged synthetic units; this slice consumes no checker result.

**Complete when:** ID ownership and deterministic stores hold; every exact tag
has one valid and one malformed verifier case; zero/out-of-range tags fail;
the range/category/verifier/dumper totality test passes; `SourceAlias` has the
single `ExplicitCast` flag; and synthetic unit access is immutable and
race-safe. No 06b semantic builder exists yet.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/tir -run 'TestUnit|TestVerify|TestNodeKind|TestDump'`, common suite, then `git diff --check`.

### Slice 06b.1: handoff resolution, lifecycle, and result shell

**Prerequisite:** the chain `05a type snapshot -> 05b.8 semantic snapshot ->
accepted complete 06a.8 handoff -> 06b.7a TIR identities` is complete. This
slice consumes those APIs and must not implement a local substitute.

**Owned files:**

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

**Input/output:** consume `solveHandoff`; validate ownership, alternatives,
semantic/type snapshot integrity, and roots; produce immutable solved-record
arenas and a failed/success-capable result shell. No policy or IR yet.

**Complete when:** every record handle resolves through its documented
symbol/syntax/instantiation/method/ordinary-slot/selected-guarded-slot root,
using one `Roots.All` pass to audit and construct its bounded local lookup;
using one `Constants.All` pass for the analogous constant index;
zero-based instantiation/method ordinals are bounds checked, inactive
alternatives disappear, `Semantics.Matches(Solution)` and every captured
`TypeID` hold, the complete handoff is tree-free and store-free, arbitrary
retained terms are impossible, and failed solve/snapshot cannot expose IR.

**Direct/source tests:** mismatched program/solve tokens, stale type counts,
foreign/missing/duplicate roots, missing semantic/type snapshot entries,
selected branches, copied accessors, stable order,
`tests/check/validation/recovery/handoff_*.peb`.

**Matrix/control coverage:** table-driven smoke roots for every compatibility
record role and every control record kind; classification is deferred.

**Limits/failure/recovery/fuzz/race:** lower validation/record/diagnostic
limits; partial and failed solutions; fuzz frozen record tags; concurrent read
of failed results.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestValidationInput|TestSolvedRecord|TestResult|TestValidationFixtures/Handoff'`, common suite, then `git diff --check`.

### Slice 06b.2: primitive/composite compatibility and operators

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

**Input/output:** consume active solved compatibility/cast/operator/
requirement records and 05a keys; produce exact classes, coercion plans, and
operator decisions without IR allocation.

**Complete when:** every matrix cell and operator row is exhaustive, no
distinct numeric pair is implicit, calling conventions never adapt, and no
operator inserts a coercion.

**Direct/source tests:** every primitive pair, composite pair, nested tuple,
numeric width/signedness, string/enum/pointer operator, and rigid parameter.

**Matrix/control coverage:** complete conversion matrix; control regression
ensures failed condition/operator types do not create flow decisions.

**Limits/failure/recovery/fuzz/race:** lower validation/decomposition/
diagnostic limits; `TypeError` suppression; fuzz `TypeKey` pairs/operators;
parallel read-only classification.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestCompatibility|TestOperatorValidation|TestValidationFixtures/(Conversion|Operator)'`, common suite, then `git diff --check`.

### Slice 06b.3a: members, aggregates, and variants

**Owned files:**

```text
compiler/internal/check/member_validation.go
compiler/internal/check/aggregate_validation.go
compiler/internal/check/member_validation_test.go
compiler/internal/check/aggregate_validation_test.go
tests/check/validation/valid/aggregate_*.peb
tests/check/validation/invalid/C0601/field_*.peb
tests/check/validation/invalid/C0605/*.peb
```

**Input/output:** consume member/aggregate records, expression records,
`Semantics` declarations and resolution members, selected methods, and field
compatibility; produce exact member, construction, and variant decisions.

**Complete when:** member ownership/category checks use the solved declaration;
05b-established instantiated results are not recomputed; record fields are
complete; variants are closed; and no lexical lookup or syntax reinterpretation
remains.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestMemberValidation|TestAggregateValidation|TestValidationFixtures/Aggregate'`, common suite, then `git diff --check`.

### Slice 06b.3b: calls, callable declarations, and context

**Prerequisite:** 06b.3a, because variant construction is a closed call target.

**Owned files:**

```text
compiler/internal/check/call_validation.go
compiler/internal/check/context_validation.go
compiler/internal/check/call_validation_test.go
compiler/internal/check/context_validation_test.go
tests/check/validation/valid/call_*.peb
tests/check/validation/valid/callable_*.peb
tests/check/validation/invalid/C0601/call_*.peb
tests/check/validation/invalid/C0604/*.peb
tests/check/validation/invalid/C0608/*.peb
tests/check/validation/invalid/C0617/*.peb
```

**Input/output:** consume call/context/callable records, prepared signatures,
method/instantiation solutions, 3a variant decisions, and compatibility plans;
produce direct/indirect/method/variant call, context, and hoisting decisions.

**Complete when:** target, convention, arity, receiver, fixed arguments, hidden
context action, capture policy, and generic-anonymous rejection are exact.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestCallValidation|TestContextValidation|TestValidationFixtures/(Call|Callable)'`, common suite, then `git diff --check`.

### Slice 06b.3c: brackets, indexing, slicing, and bounds

**Owned files:**

```text
compiler/internal/check/index_validation.go
compiler/internal/check/index_validation_test.go
tests/check/validation/valid/index_*.peb
tests/check/validation/invalid/C0609/*.peb
```

**Input/output:** consume immutable bracket selections, active guarded records,
index records, constants, and place metadata; produce generic/index/slice
classification and exact static or dynamic check plans.

**Complete when:** all bracket modes and index/slice rows are closed, inactive
alternatives disappear, every dynamic bound has a check plan, and slice source
category is preserved without deciding lifetime representation.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestIndexValidation|TestValidationFixtures/Index'`, common suite, then `git diff --check`.

### Slice 06b.4: places, mutation, assignments, and coercion plans

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

**Input/output:** consume solved place/assignment/operator/compatibility
records and the exact member/index decisions from 06b.3; produce closed place
descriptors and ordered load/operator/coerce/store plans.

**Complete when:** every place projection and writability root is exact,
compound/postfix left sides evaluate once, and assignment never becomes an
expression.

**Direct/source tests:** storage categories, deep field/tuple/index/deref
projections, address-of `let`, string index, simple/compound/postfix cases.

**Matrix/control coverage:** every assignment-relevant identity/optional/
tuple/numeric explicit rejection; expression-statement/for-update placement.

**Limits/failure/recovery/fuzz/race:** lower place/component/validation
limits; erroneous bases and failed solve; fuzz projection chains; independent
parallel place validation.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestPlaceValidation|TestAssignmentValidation|TestValidationFixtures/Place'`, common suite, then `git diff --check`.

### Slice 06b.5a: generic requirement interfaces

**Owned files:**

```text
compiler/internal/check/requirement_validation.go
compiler/internal/check/requirement_validation_test.go
tests/check/validation/valid/generic_*.peb
tests/check/validation/invalid/C0610/*.peb
```

**Input/output:** join 06a requirement records with `Solution.Requirements`,
consume literal-fit directly from the solution, and publish the normalized
generic interface.

**Complete when:** symbolic bodies publish the closed ordered interface,
unsupported requirements fail, equivalent uses deduplicate, and every
record-owned join and solver-owned literal payload follows the exact rules
above.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestRequirementValidation|TestValidationFixtures/Generic'`, common suite, then `git diff --check`.

### Slice 06b.5b: globals, bindings, and `sizeof`

**Owned files:**

```text
compiler/internal/check/global_validation.go
compiler/internal/check/global_validation_test.go
tests/check/validation/valid/global_*.peb
tests/check/validation/valid/sizeof_*.peb
tests/check/validation/invalid/C0602/*.peb
tests/check/validation/invalid/C0615/*.peb
tests/check/validation/invalid/C0616/*.peb
```

**Input/output:** consume binding/type-use records and the local indexed
`constantKnown` results; publish binding/global/sizeof decisions.

**Complete when:** initializer policy is exact, globals are constant, extern
forms are closed, and `sizeof` retains only its semantic type with no layout.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestGlobalValidation|TestValidationFixtures/(Global|Sizeof)'`, common suite, then `git diff --check`.

### Slice 06b.5c: configured entry point

**Prerequisite:** 06b.5a is accepted. Entry legality includes the normalized
absence of generic requirements.

**Owned files:**

```text
compiler/internal/check/entry_validation.go
compiler/internal/check/entry_validation_test.go
tests/check/validation/valid/entry_*.peb
tests/check/validation/invalid/C0620/*.peb
```

**Input/output:** consume only `Config.Entry`, root-module identity, immutable
symbol/signature facts, solved type, callable/capture decisions, and normalized
requirements; publish entry metadata or `C0620`.

**Complete when:** every accepted/rejected entry rule above is identity-based,
`EntryNone` is inert, and no name search or spelling convention exists.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestEntryValidation|TestValidationFixtures/Entry'`, common suite, then `git diff --check`.

### Slice 06b.6: structural flow, switches, returns, reachability, and defer

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

**Input/output:** consume the sole frozen control-region arena, defer records,
solved control values,
constants, member/compatibility decisions, and callable results; produce exit
sets, targets, narrowing, reachability, definite return, and exact defer chains.

**Complete when:** all structural composition rules are deterministic,
the frozen control-region arena is the only parent/depth/children authority,
flow follows exact `Composition` roles rather than child-region ordinals,
`controlBinding` participates as a leaf, declaration-complete variants govern
switches, unreachable warnings occur once per region, and every lexical exit
has its correct reverse defer order.

**Direct/source tests:** exhaustive exit-set cross-product, nested loop/switch
targets, scalar/nominal switches, constant-true loops, invalid returns,
implicit void return, print/discard statement forms, and nested defers on every
exit.

**Matrix/control coverage:** return compatibility uses the full relevant
matrix before definite return; complete required control-flow coverage.

**Limits/failure/recovery/fuzz/race:** lower control/flow/defer/diagnostic
limits; failed condition/return roots with independent regions; fuzz valid and
damaged frozen region arenas; parallel function analysis.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestControlValidation|TestSwitchValidation|TestDeferValidation|TestValidationFixtures/(Control|Defer|Statement)'`, common suite, then `git diff --check`.

### Slice 06b.7b: typed-IR construction and publication candidate

**Owned files:**

```text
compiler/internal/check/ir_builder.go
compiler/internal/check/ir_builder_test.go
tests/check/ir/valid/*.peb
tests/check/ir/*.tir.golden
```

**Prerequisite:** accepted 06b.1-06b.6 decisions and the immutable 06b.7a
store/verifier.

**Input/output:** consume only successful semantic decisions from slices
06b.1-06b.6; construct through the 06b.7a package, verify, freeze, and return
the closed `tir.Unit` candidate.

**Complete when:** every successful surface node maps once, every decision is
explicit, evaluation is single/left-to-right, spans are complete, and the
verifier proves no backend reinterpretation is needed.

**Direct/source tests:** every node tag, source/synthetic span, temp dominance,
coercion/check/call/context node, target/defer chain, generic reference, stable
normalized dumps, and the explicit nonvalue/folded paths for `EndOfFile`,
`RecordField`, type positions, and recovery nodes. Numeric literal cases prove
frozen bytes become canonical IR payloads without a source/AST read; decoded
character/string/interpolation payloads preserve their exact order and value.

**Matrix/control coverage:** every accepted coercion becomes the exact node;
every control exit/implicit return/defer chain becomes exact IR.

**Limits/failure/recovery/fuzz/race:** lower node/component/dump limits; reject
failed semantic input; fuzz builder inputs; concurrently read immutable units.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./internal/check -run 'TestIRBuilder|TestIRFixtures'`, common suite, then `git diff --check`.

### Slice 06b.8: publication gate, determinism, recovery, fuzz, and race

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

**Input/output:** connect `run06a` to `run06b`, enforce the all-or-nothing IR/
backend gate, and exercise the complete checker under deterministic, damaged,
limited, fuzzed, and concurrent workloads.

**Complete when:** exactly one traversal/solve is structurally enforced,
failed generation/solve/validation/verification exposes no IR, repeated runs
and forced map seeds produce identical diagnostics/results/dumps, and all
public stores are immutable.

**Direct/source tests:** full corpus, multimodule identity, every diagnostic
family, normalized result/IR repetitions, no compiler-code fixture fallback.

**Matrix/control coverage:** full matrix and full control corpus rerun through
the public checker entry and backend-input gate.

**Limits/failure/recovery/fuzz/race:** lower every 05b/06a/06b limit; combine
failed solve, recovery, and later valid declarations; run all fuzz seeds and
`go test -race` concurrent reads/independent compilations.

**Verification:**
`GOCACHE=/tmp/pebble-codex-gocache go test ./...`,
`GOCACHE=/tmp/pebble-codex-gocache go test -race ./...`,
`GOCACHE=/tmp/pebble-codex-gocache go vet ./...`, then repository-root
`git diff --check`.

Each slice handoff reports exact files, input/output contracts, matrix and
control coverage, direct/source fixtures, limits, failed-solve/recovery/fuzz/
race evidence, commands/results, and upstream discrepancies. No slice edits
phases 03, 04b, 05a, or 05b to make implementation easier.

## Resolved upstream contracts and future decisions

There are no remaining unresolved language-contract decisions for 06b. The
accepted structural-composition contract must still be implemented and pass
the complete 06a.8 handoff before any 06b slice begins. The implementation
prerequisites are:

- 04b supplies the concrete immutable checker queries.
- 05b supplies ordinary and guarded solved slots, conditional guarded
  publication, exact choice selection, `Callable`, `Indexable`, and
  `Sliceable`.
- 03b includes `EndOfFile` and `RecordField`; 06a dispatches both and the
  surface-to-IR table above gives their closed nonvalue/folded paths.
- Generic anonymous functions are rejected through
  `unsupportedCallableRecord` and `C0608`; valid nongeneric anonymous
  functions are noncapturing and globally hoisted.
- The audited 06a root/record contract supplies every identity, solved value,
  structural role/arm, runtime child order, policy payload, context fact, span,
  and deterministic order used here.
- The handoff is tree-free; module/import/declaration IR and entry identity use
  only copied compilation metadata, `SemanticSnapshot`, frozen records, and
  immutable symbol descriptors.
- `frozenRoots.All`/`Root` are the only root APIs, the `controlRegion` arena is
  the sole hierarchy, and empty bindings recover silently before their one
  06b-owned `C0602`.

The upstream chain is `05a type-snapshot extension -> 05b.8 semantic-snapshot
continuation -> accepted complete 06a.8 handoff`. After that, implementation
starts with `06b.7a` so `06b.1` can refer to real TIR identities. `06b.1` must
consume the completed upstream APIs and must not implement a private type
snapshot, semantic snapshot, or alternate handoff locally.

Two decisions remain explicitly future and nonblocking:

- slice lifetime, escape, ownership, and runtime representation;
- phase-10 release-mode behavior for numeric-cast, arithmetic-fault,
  optional-unwrap, and bounds-check failures.

Typed IR already records the source category and exact semantic check kind, so
neither decision can affect inference or current validation legality. Closure
support, generic anonymous functions, unsafe pointer policy, C adapters/
promotions, public callable/indexable/sliceable traits, and other deferred
features are not invented by this task. No implementation may use textual
lookup, AST rereading, syntax mutation, an extra solve, implicit numeric
conversion, or any other guessed semantics as a substitute.
