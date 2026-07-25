# 05b Algebraic Inference

`05b` specifies the deterministic inference subsystem used by semantic
checking. It resolves authored type syntax into immutable declaration
descriptors, allocates solver-local terms, accepts equations from phase 6,
solves them by unification and ordered worklists, and publishes immutable type
solutions.

The central rule is:

> **Phase 6 generates language facts. `05b` solves them. Neither traversal nor
> insertion order may choose a type.**

[05a Semantic Type Store](05a-semantic-type-store.md) is authoritative.
`05b` neither creates a second concrete type representation nor changes
`TypeID` identity. It explicitly rejects mutable AST types, pointer identity,
global checker state, repeated lexical lookup, traversal-order inference,
heuristic numeric ranking, and monomorphization through AST cloning. It also
rejects the prototype's canonical-name identity, package-global type tables,
and first-observed generic binding. The C prototype is evidence for source
cases only.

`05b` is not a second AST pass and does not decide what source constructs mean.
It does not walk bodies, classify operators, select conversions, determine
places, or validate control flow. Phase 6 performs the one semantic traversal
and drives the APIs defined here. This makes `05b` the algebraic core of the
language's semantics without turning the solver into the owner of every
semantic rule.

## Phase boundary and lifecycle

### Inputs and entry point

The implementation lives in `compiler/internal/infer`. It has an immutable
declaration layer and one mutable solve session:

```go
type Config struct {
    MaxInferVariables        uint32
    MaxConstraints           uint32
    MaxShapeComponents       uint32
    MaxLiteralBytes          uint32
    MaxLiteralBits           uint32
    MaxLiteralExponent       uint32
    MaxAliasDepth            uint32
    MaxTypeSyntaxDepth       uint32
    MaxUnificationSteps      uint64
    MaxDecompositionSteps    uint64
    MaxConstraintRequeues    uint32
    MaxTotalRequeues         uint64
    MaxChoices               uint32
    MaxChoiceAlternatives    uint32
    MaxChoiceStates          uint64
    MaxSolvedSlots           uint32
    MaxDiagnostics           uint32
}

type ArrayLengthState uint8

const (
    ArrayLengthKnown ArrayLengthState = iota + 1
    ArrayLengthError
    ArrayLengthUnavailable
)

type ArrayLengthResult struct {
    State ArrayLengthState
    Value uint64
}

type ArrayLengthEvaluator interface {
    ArrayLength(symbol.SyntaxRef) ArrayLengthResult
}

type ProgramInputs struct {
    Graph        *module.Graph
    Sources      *source.FileSet
    Resolution   *symbol.Result
    Types        *types.Store
    ArrayLengths ArrayLengthEvaluator
    LiteralTarget LiteralTarget
}

type LiteralTarget struct {
    WordBits uint8 // exactly 32 or 64
}

func Prepare(
    inputs ProgramInputs,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) *Program

func NewSession(
    program *Program,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) *Session
```

`Graph` supplies reachable modules, dependency order, each module's
`source.ID`, and its immutable `syntax.Tree`. `Sources` is the same
compilation-owned file set used by phases `04a` and `04b`; `05b` never reopens
a file. `Resolution` supplies the `SyntaxRef -> SymbolID`, qualifier,
`BracketMode`, member, and capture mappings from `04b`. `Types` is the one
compilation-owned `05a` store. `Prepare` resolves type declarations,
signatures, and the type syntax owned by those declarations. `Session` never
performs name lookup or source traversal. During its one traversal, phase 6
asks the session to resolve local annotations, cast targets, explicit type
arguments, and other body-owned type syntax, then creates terms, constraints,
publication roots, and generic-instantiation roots before invoking `Solve`
exactly once.

The required orchestration is:

```go
program := infer.Prepare(inputs, diagnostics, inferConfig)
session := infer.NewSession(program, diagnostics, inferConfig)
checker.Generate(program, session, graph, resolution) // one AST traversal
solution := session.Solve()
result := checker.ValidateAndLower(program, solution)
```

`Prepare`, followed by the entire `Session` lifetime, are the only writers to
`Types`; the driver does not run either concurrently because the first `05a`
store contract is single-owner. `checker.Generate` may call only the documented
session builder operations; it cannot inspect or mutate union-find state.

`LiteralTarget.WordBits` supplies the selected target's already-known native
integer width and must be 32 or 64. It affects only fitting `int` and `uint`
literals; it does not expose layout or ABI queries to `05b`. Missing or invalid
target width produces `T0512` before literal solving.

Array lengths are the one explicit semantic callback. Phase 6 owns the
constant-expression language and supplies this evaluator before calling
`Prepare`. `05b` requests a
nonnegative `uint64` for the length expression's `SyntaxRef`; it does not
define the permitted constant-expression language. `ArrayLengthKnown` is
interned subject to the `05a` array limit, `ArrayLengthError` reuses the
provider's diagnostic and recovers with `Error`, and
`ArrayLengthUnavailable` produces `T0512`. Phase 6 may not claim production
completion without the provider implementing its accepted constant-expression
rules. The callback
keeps constant evaluation out of the unifier without leaving ownership
undefined.

Nil, inconsistent, cross-snapshot, or missing inputs produce one bounded
`T0512` diagnostic and an immutable partial result rather than a panic. The
subsystem owns no filesystem, target, layout, backend, or process-global
state.

Zero-valued limits select these defaults:

| Limit | Default |
| --- | ---: |
| inference variables | 1,048,576 |
| constraints | 4,194,304 |
| shape components | 4,194,304 |
| authored bytes in one numeric literal | 65,536 |
| numerator, denominator, or integer magnitude bits | 1,048,576 |
| absolute floating exponent | 1,000,000 |
| alias depth | 256 |
| type-syntax depth | 256 |
| unification steps | 16,777,216 |
| decomposition steps | 16,777,216 |
| requeues of one constraint | 256 |
| total requeues | 16,777,216 |
| choices | 65,536 |
| alternatives in one choice | 8 |
| speculative choice states | 1,048,576 |
| solved slots | 4,194,304 |
| inference diagnostics | 50 |

Tests may lower every limit.
`MaxAliasDepth` and `MaxTypeSyntaxDepth` saturate at the implementation hard
ceiling of 1,024 even when a larger value is requested. This bounds the few
depth-recursive reducers independently of the host stack; broad shapes and
unbounded searches still use explicit stacks and separate counters.

### Prepared declaration program

`Program` is the preparation- and solve-time result of type-syntax resolution.
It privately retains `ProgramInputs` and is therefore not a phase-6 validation
handoff. Its semantic records are closed data, not undefined interfaces:

```go
type DeclarationState uint8
const (
    DeclarationReady DeclarationState = iota + 1
    DeclarationError
)

type TypeDeclarationForm uint8
const (
    DeclarationNominal TypeDeclarationForm = iota + 1
    DeclarationAlias
)

type NominalKind uint8
const (
    NominalStruct NominalKind = iota + 1
    NominalUnion
    NominalTaggedUnion
    NominalEnum
    NominalExtern
)

type TemplateID uint32 // zero is invalid; owned by Program

type TemplateKind uint8
const (
    TemplateKnown TemplateKind = iota + 1
    TemplateParameter
    TemplatePointer
    TemplateArray
    TemplateSlice
    TemplateTuple
    TemplateOptional
    TemplateFunction
    TemplateNominal
)

type TypeTemplate struct {
    ID          TemplateID
    Kind        TemplateKind
    Known       types.TypeID
    Parameter   symbol.SymbolID
    Declaration symbol.SymbolID
    Length      uint64
    Convention  types.CallingConvention
    Variadic    bool
    Children    []TemplateID
}

type MemberDescriptor struct {
    Symbol symbol.SymbolID
    Type   TemplateID
}

type TypeDeclaration struct {
    Symbol     symbol.SymbolID
    State      DeclarationState
    Form       TypeDeclarationForm
    Nominal    NominalKind
    Parameters []symbol.SymbolID
    Concrete   types.TypeID
    Template   TemplateID
    Members    []MemberDescriptor
}

type Signature struct {
    Symbol     symbol.SymbolID
    State      DeclarationState
    Parameters []symbol.SymbolID
    TypeParams []symbol.SymbolID
    Inputs     []TemplateID
    Result     TemplateID
    Convention types.CallingConvention
    Variadic   bool
}

type RuntimeTypes struct {
    Allocator types.TypeID
    Context   types.TypeID
}

func (p *Program) TypeDeclaration(symbol.SymbolID) (TypeDeclaration, bool)
func (p *Program) TypeDeclarations() []TypeDeclaration
func (p *Program) Signature(symbol.SymbolID) (Signature, bool)
func (p *Program) Signatures() []Signature
func (p *Program) Template(TemplateID) (TypeTemplate, bool)
func (p *Program) RuntimeTypes() (RuntimeTypes, bool)
```

All slice accessors return copies in ascending declaration identity and source
order. A `TemplateKnown` contains one valid `05a` ID and no children. A
`TemplateParameter` contains one rigid type-parameter `SymbolID`. Composite
templates use the same child order and identity fields as the corresponding
`05a` key. `TemplateNominal` contains the nominal declaration `SymbolID` and
ordered argument children. Pointer, array, slice, and optional templates have
one child; tuples and nominals use authored order; a function stores its
ordered parameters followed by its result as the final child.

Every ready declaration has a valid `Template`. A nongeneric nominal or alias
also publishes `Concrete`, and its template is `TemplateKnown(Concrete)`. A
generic nominal or alias has zero `Concrete`; it is a constructor identified by
its declaration `SymbolID` and parameterized template, not a `TypeID` for an
unapplied constructor. `Nominal` is nonzero only for
`DeclarationNominal`; aliases use zero. A concrete application substitutes its
ordered arguments into the template and only then interns a semantic type.
This is the formal distinction between declaration identity and type identity.

Builtin type syntax must resolve in `04b` to `SymbolBuiltinType`. `Prepare`
maps its exact `symbol.BuiltinType` discriminator to `Types.Builtins()`. It
never recognizes a builtin from source text. Because `04b` reserves builtin
names globally, no authored declaration can replace that identity.

`RuntimeTypes` is returned by value and is valid only when both IDs are
store-owned nominals prepared successfully. Sessions never mutate it. The
post-solve snapshot below copies it; phase 6 validation does not retain the
`Program` that produced it.

### Immutable downstream semantic snapshot

After exactly one `Session.Solve`, the compiler creates the tree-free semantic
view consumed by phase 6 validation:

```go
func Snapshot(
    program *Program,
    solution *Solution,
    diagnostics *diagnostic.DiagnosticSet,
) (*SemanticSnapshot, bool)

func (s *SemanticSnapshot) Types() *types.Snapshot
func (s *SemanticSnapshot) Resolution() *symbol.Result
func (s *SemanticSnapshot) TypeDeclaration(symbol.SymbolID) (TypeDeclaration, bool)
func (s *SemanticSnapshot) TypeDeclarations() []TypeDeclaration
func (s *SemanticSnapshot) Signature(symbol.SymbolID) (Signature, bool)
func (s *SemanticSnapshot) Signatures() []Signature
func (s *SemanticSnapshot) Template(TemplateID) (TypeTemplate, bool)
func (s *SemanticSnapshot) Templates() []TypeTemplate
func (s *SemanticSnapshot) RuntimeTypes() (RuntimeTypes, bool)
func (s *SemanticSnapshot) TypeParameter(symbol.SymbolID) (types.TypeID, bool)
func (s *SemanticSnapshot) OwnerParameters(symbol.SymbolID) []symbol.SymbolID
func (s *SemanticSnapshot) Matches(solution *Solution) bool
```

`Snapshot` is the one post-solve construction API. It requires a nonnil valid
`Program` and the final `Solution` returned by `Solve` for one session created
from that exact program. A final failed solution is permitted so downstream
recovery can inspect independent facts; an unfinalized, fabricated, repeated-
solve recovery, foreign-program, or different-session solution is not. The
program carries an opaque program token, each session carries a fresh opaque
solve token, and the final solution copies both tokens plus the store length at
solve finalization. These tokens contain no backpointer or mutable state.
`Snapshot` checks both identities and requires the exclusively locked store
length to equal the solution's captured length. `Matches` later checks the
same program and solve tokens, finalization marker, and captured type-snapshot
length. Thus a semantic snapshot cannot be paired with an earlier or later
solution even when a program is reused.

Construction holds the program's existing store-ownership lock, calls
`ProgramInputs.Types.Snapshot`, captures the exact immutable
`ProgramInputs.Resolution`, and then copies into private storage, in this
deterministic order: templates by `TemplateID`; declarations, signatures, and
type-parameter bindings by `SymbolID`; each descriptor/list in its authored
order; owner records by owner `SymbolID`; and runtime identities by value.
It preserves every prepared `TypeTemplate`, `TypeDeclaration`,
`MemberDescriptor`, `Signature`, runtime type, type-parameter binding, and
owner parameter order needed downstream. Slice/map fields and returned
records are defensively copied. `Types` returns the owned immutable type
snapshot. `Resolution` returns the exact immutable tree-free `symbol.Result`
used to prepare the program; 04b's immutable copied-accessor contract applies,
and the semantic snapshot retains no resolver, graph, module, source, or syntax
input through it. `SemanticSnapshot` is the sole phase-6 handoff location and
authority for this resolution pointer; compilation metadata must not duplicate
it. No accessor returns mutable backing storage. Capturing this already bounded
immutable result does not copy or create resolution records.

Before publication, construction validates template IDs and child references,
descriptor/signature references, owner/parameter ordering, runtime identities,
and every copied declaration, member, callable, owner, type-parameter, runtime,
and solution-owned `SymbolID` against that exact resolution result. For
resolver-owned syntax roles it verifies the corresponding immutable reference,
qualifier, bracket, capture, or member query; value syntax that has no resolver
record is not required to fabricate one. It validates every nonzero `TypeID`
in the copied program data and final solution
against `types.Snapshot.Contains`. `TypeFinal` must contain a captured ID;
`TypeError` must contain none. It also validates all symbol, syntax, slot,
requirement, instantiation, method, and selected-alternative result tables as
complete according to the final solution contract. A final unsuccessful
solution may contain documented `TypeError` entries; incompleteness is not the
same as unsuccessful inference.

Every copied template node, child, declaration, member, signature component,
owner parameter, type-parameter binding, runtime identity, and solution entry
charges `MaxDecompositionSteps`; individual template/list widths remain
bounded by `MaxShapeComponents`, and the type copy uses the 05a bounds. Counts
and prospective allocations use checked arithmetic. Snapshot diagnostics use
the existing `MaxDiagnostics` budget. No snapshot-specific unbounded limit is
introduced.

The operation is atomic: any identity mismatch, incomplete/fabricated
solution, invalid reference or `TypeID`, store-length mismatch, type-snapshot
failure, or resource exhaustion emits bounded `T0512`, returns `(nil, false)`,
and publishes no partial snapshot. `Program`, `Solution`, and the original
store remain unchanged. The returned object owns all copied state and is safe
for concurrent reads. It contains no `ProgramInputs`, graph, module value,
source file set or bytes, syntax tree/node, resolver callback, mutable store,
reporter, alias-preparation state, interning ability, session, constraint, or
solver cell. Hiding any such value behind a read-only interface does not
satisfy this contract.

Programs remain reusable for independent sequential inference sessions. Each
session produces its own final solution and, when requested immediately after
that solve under the serialized lifecycle, its own independent semantic
snapshot. The original append-only store is neither frozen nor reachable from
the snapshot and remains available to separately authorized later sequential
compiler phases; this contract designs no phase-7 behavior.

### Session builder and immutable solution

Phase 6 generates facts through this conceptual API:

```go
func (s *Session) Variable(origin Origin) Term
func (s *Session) Known(types.TypeID) Term
func (s *Session) Error(origin Origin) Term
func (s *Session) IntegerLiteral(text []byte, origin Origin) Term
func (s *Session) FloatLiteral(text []byte, origin Origin) Term
func (s *Session) NegateLiteral(Term, Origin) Term
func (s *Session) ResolveType(symbol.SyntaxRef, symbol.SymbolID) TypeResult
func (s *Session) Add(constraint Constraint) ConstraintID
func (s *Session) AddChoice(constraint Constraint) (ConstraintID, ChoiceRef)
func (s *Session) PublishSymbol(symbol.SymbolID, Term)
func (s *Session) PublishSyntax(symbol.SyntaxRef, Term)
func (s *Session) PublishSlot(Term) SlotID
func (s *Session) PublishGuardedSlot(ChoiceRef, uint32, Term) SlotID
func (s *Session) PublishInstantiation(symbol.SyntaxRef, symbol.SymbolID, []Term)
func (s *Session) PublishGuardedInstantiation(ChoiceRef, uint32, symbol.SyntaxRef, symbol.SymbolID, []Term)
func (s *Session) Fatal() bool
func (s *Session) Solve() *Solution
```

Every mutator fails atomically with a bounded `T0512` diagnostic after its
limit is reached. Calling a mutator after `Solve`, calling `Solve` twice, or
mixing terms from different sessions is an inconsistent-input error, not a
panic.

`Fatal` is the authoritative read-only, allocation-free builder lifecycle
query. Nil and invalid sessions report true. It becomes true immediately when
any session-owned path produces or schedules `T0512`, including a diagnostic-
budget overflow before diagnostics are flushed, and remains true through and
after `Solve`. `T0501` through `T0511` recovery does not set it. Once true,
the current operation performs no subsequent forward cell, memo, publication,
constraint-progress, requirement, selection, counter, or table mutation.
Transactional choice evaluation is the exception: it must preserve the exact
fatal conflict, restore the complete pre-speculation snapshot, leave
speculative mode, clear speculative-only conflict state, and only then publish
that preserved `T0512` once outside speculation. This rollback cannot mark a
choice complete, retain a selection, or resume solving.
Later mutators publish nothing, leave all session tables and counters
unchanged, and return their ordinary recovery or zero result without emitting
cascading diagnostics. Mutation after a completed solve and repeated `Solve`
set the same state; all solve-stage and speculative barriers consult this one
query. It exposes no diagnostic collection, solver cell, counter, memo table,
publication table, or other private mutation state.

The second argument to `ResolveType` is the containing callable or type
declaration, or zero for a module-level nongeneric environment. It selects the
ordered rigid type-parameter environment already stored in `Program`; it is
never used for lexical lookup. Results are memoized by `(SyntaxRef, owner)` in
the session. A foreign owner or an occurrence outside that owner's environment
is an inconsistent-input error.

```go
type TypeState uint8

const (
    TypeFinal TypeState = iota + 1
    TypeError
)

type TypeResult struct {
    State TypeState
    Type  types.TypeID // valid exactly when State == TypeFinal
}

type SymbolType struct {
    Symbol symbol.SymbolID
    Result TypeResult
}

type SyntaxType struct {
    Syntax symbol.SyntaxRef
    Result TypeResult
}

type SlotID struct { /* opaque session identity plus nonzero ordinal */ }

type SlotType struct {
    Slot   SlotID
    Result TypeResult
}

type RequirementKind uint8
const (
    RequirementNumeric RequirementKind = iota + 1
    RequirementIntegral
    RequirementOrdered
    RequirementLiteralFits
)

type ExactLiteralKind uint8
const (
    ExactInteger ExactLiteralKind = iota + 1
    ExactFloat
)

type Requirement struct {
    Owner   symbol.SymbolID
    Parameter symbol.SymbolID
    Kind    RequirementKind
    Subject types.TypeID
    Origin  Origin
    LiteralKind ExactLiteralKind
    Numerator   string
    Denominator string
}

type Instantiation struct {
    Site      symbol.SyntaxRef
    Generic   symbol.SymbolID
    Arguments []TypeResult
}

type MethodSelection struct {
    Site      symbol.SyntaxRef
    Method    symbol.SymbolID
    Arguments []TypeResult
}

type Solution struct { /* unexported immutable storage */ }

func (r *Solution) Successful() bool
func (r *Solution) SymbolType(symbol.SymbolID) (TypeResult, bool)
func (r *Solution) SymbolTypes() []SymbolType
func (r *Solution) SyntaxType(symbol.SyntaxRef) (TypeResult, bool)
func (r *Solution) SyntaxTypes() []SyntaxType
func (r *Solution) Slot(SlotID) (TypeResult, bool)
func (r *Solution) Slots() []SlotType
func (r *Solution) Requirements(symbol.SymbolID) []Requirement
func (r *Solution) Instantiation(symbol.SyntaxRef) (Instantiation, bool)
func (r *Solution) Method(symbol.SyntaxRef) (MethodSelection, bool)
func (r *Solution) Selection(ConstraintID) (uint32, bool)
```

The slice accessors return copies ordered by ascending `SymbolID`, by
`(ModuleID, NodeID)`, or by the private ascending slot ordinal for their
respective tables.
Requirements and instantiations return copies in stable owner, source, and
parameter order. A `Requirement` is retained when a
phase-6-generated capability or exact literal-fitting constraint targets a
rigid type parameter while checking the generic declaration identified by
`Owner`. Integer requirements use a signed canonical decimal numerator and an
empty denominator; floating requirements use canonical decimal numerator and
positive denominator. Concrete policy obligations remain owned by phase 6 and
are not smuggled into this result.

`PublishInstantiation` remains the unconditional publication for an explicit
or inferred generic application. `PublishGuardedInstantiation` has the same
site, generic identity, ordered argument, ownership, copying, and deterministic
solution contracts, but attaches the publication to one exact session-owned
`OneOf` alternative. Ordinary and guarded instantiations share one duplicate
namespace keyed by site: a site may be published exactly once by either API.
`MaxShapeComponents` independently bounds both the total ordinary-plus-guarded
publication count and the cumulative number of retained arguments across those
publications. Site duplication and every caller-supplied term are validated
before either bound is preflighted and before the argument slice is copied.
Cumulative arguments are preflighted without overflow, and the counter
advances only immediately before successful insertion. Invalid sites or
generic identities, foreign terms or choices, invalid alternative indices,
duplicates, either limit's exhaustion, and post-solve mutation leave the
publication table, retained-argument counter, and caller storage unchanged and
produce bounded `T0512` recovery.

A guarded instantiation's arguments are roots only if its exact alternative is
the unique committed selection. Only then does it appear in
`Solution.Instantiation` and the ordered instantiation manifest. If another
alternative is selected, or if the choice is failed or ambiguous, the
publication is absent. Its arguments alone cannot become reachable, default,
emit `T0510` or another branch-local diagnostic, or enter the downstream
semantic-snapshot manifest. An argument independently reachable through an
ordinary or selected publication retains that independent liveness. No
callback or checker-owned generic policy participates in this filtering.

Every unconditional or selected root explicitly published by phase 6 has an
entry; an unselected guarded slot or guarded instantiation is intentionally
absent. `05b` does not decide
which syntax is value-producing and does not invent entries by traversing the
tree. Phase 6 must publish every binding, parameter, callable, field, variant
payload, and value-producing expression required by later checking or typed
IR. Names used only as module qualifiers and damaged `04b` error symbols are
not published as successful types.

An ordinary solved slot publishes one valid session term that has no honest
`SymbolID` or `SyntaxRef`. An alternative-guarded slot publishes one valid
session term under one exact `OneOf` constraint and zero-based alternative
index. `AddChoice` accepts only a top-level `OneOf`, performs the same atomic
validation/allocation as `Add`, and returns its `ConstraintID` plus an opaque
session-owned `ChoiceRef`; the reference cannot be reconstructed from the
numeric ID. Phase 6 uses the numeric ID for `Selection`/record tags and the
opaque reference only for pre-solve guarded publication. An ordinary slot,
or a guarded slot whose alternative is selected, materializes exactly like an
ordinary publication and therefore has `TypeFinal` or `TypeError`. A guarded
slot whose alternative is not selected has no `Solution.Slot` entry. Its
publication alone must not attach evidence, make its term reachable for
defaulting, diagnose it, constrain it, or materialize a shape. Only constraints
inside the selected alternative may do so.

Slots are allocated monotonically in publication order, bounded by
`MaxSolvedSlots`, immutable after `Solve`, and local to the session/solution
snapshot. The tuple `(guard or ordinary, term)` may be published only once;
duplicate publication is inconsistent input. Invalid or foreign terms,
non-`OneOf` or foreign choices, invalid alternative indices, duplicate
publication, slot-limit exhaustion, mutation after solve, and cross-session
use fail atomically with `T0512`. A failed publication returns zero and adds no
partial root. A `Solution.Slot` query with a foreign/zero ID returns absent;
immutable queries emit no diagnostic. Slot IDs must never be compared across
compilations.

Phase 6 uses slots only for honest checker-owned internal roots such as call
parameter/result leaves, record-field destination terms, compound-operation
temporaries, and the selected value interpretation of a deferred bracket. It
must not misuse a symbol or syntax publication to expose such a term, and it
must not retain `Term` or `InferID` after solving.

A successful solution has no inference diagnostic, no `TypeError` entry, no
unresolved `InferID`, no unmaterialized shape, and no pending inference-owned
constraint. Every published final ID belongs to `ProgramInputs.Types`. A
partially failed solution is still queryable: independent solved
entries remain `TypeFinal`, every affected or unresolved entry is `TypeError`,
and no zero or recovery ID is published. Backend-consumable typed IR requires
`Successful()` and later phase-6 success.

The solution owns copied immutable tables and exposes no union-find node,
literal buffer, constraint, mutable slice, AST pointer, syntax-node decoration,
or session reference. Its private program/solve ownership tokens contain no
backpointer. `Program` deliberately retains preparation inputs and is removed
from the downstream boundary by `SemanticSnapshot`.

## Solver identities, terms, and shapes

```go
type InferID uint32
type ConstraintID uint32
type OriginID uint32
```

Zero is invalid. IDs belong to one `Session`. `InferID`s and `ConstraintID`s
are allocated monotonically in deterministic builder-call order; their numeric
values are debugging data, not language semantics.

`ChoiceRef` is an opaque session-owned capability naming one exact `OneOf`
constraint. Unlike the numeric debugging IDs, it retains enough ownership to
reject cross-session guarded-slot publication. It never crosses the immutable
solution boundary.

The inference term is a closed tagged value:

```text
Known(TypeID)
Variable(InferID)
IntLiteral(exact integer)
FloatLiteral(exact rational)
Error
```

`Known` must refer to an ID already issued by the input store. `Variable` is a
solver-local cell that may be unioned or solved. Literal terms retain exact
values until fitting or defaulting. One solver-owned `Error` sentinel absorbs
dependent constraints. `Error` is never passed to `types.Intern`, published
as a `TypeID`, or confused with an invalid zero ID.

`SelectMethod` may allocate hidden cells for omitted method-local type
arguments only after its receiver declaration is known. Those cells are
charged to `MaxInferVariables`, allocated once in source `ConstraintID` and
parameter order, and otherwise obey the same union-find and publication rules.

Unresolved composite structure is not a sixth term variant. A constraint may
carry this closed algebraic shape whose leaves are terms:

```text
Leaf(Term)
Pointer(Shape)
Array(uint64, Shape)
Slice(Shape)
Tuple(nonempty ordered []Shape)
Optional(Shape)
Function(CallingConvention, ordered []Shape, result Shape, variadic bool)
Nominal(SymbolID, ordered []Shape)
```

Clients construct shapes through validated functions that copy ordered
children:

```go
func Leaf(Term) Shape
func PointerShape(Shape) Shape
func ArrayShape(uint64, Shape) Shape
func SliceShape(Shape) Shape
func TupleShape([]Shape) Shape
func OptionalShape(Shape) Shape
func FunctionShape(types.CallingConvention, []Shape, Shape, bool) Shape
func NominalShape(symbol.SymbolID, []Shape) Shape
```

`TupleShape` rejects an empty list. Zero terms, invalid conventions or nominal
symbols, and shapes containing terms from another session are rejected when
the containing constraint is added.

A `Shape(subject, pattern)` constraint says that `subject` has that structure.
Shapes are solver-owned constraint payloads, not semantic types, not generally
addressable identities, and not stored in `05a`. When every leaf becomes a
known ID, the solver interns the corresponding `TypeKey` bottom-up and binds
the subject to `Known(TypeID)`. Matching a known ID against a shape decomposes
the `05a` key and emits leaf equations. This is the only bridge from unresolved
composites to the immutable store.

An `Instantiate(template, substitutions, subject)` constraint is the compact
form used for generic signatures and aliases. `template` is a `Program-owned
TemplateID`; substitutions are ordered `(parameter SymbolID, Term)` pairs. It
expands deterministically into a shape by walking the immutable template,
replacing only the listed rigid parameters, and then constrains `subject`. It
is not a second semantic type store or a partially interned `TypeID`.

### Exact literal values

The literal parser copies authored bytes from `source.FileSet`; it never keeps
a mutable substring. Separators and radix syntax are removed before numeric
conversion. An integer is an arbitrary-precision signed mathematical integer
with a canonical sign and unsigned big-endian magnitude; zero has positive
sign and empty magnitude. Equality is mathematical equality, independent of
radix, separators, leading zeroes, or spelling.

A finite floating literal is an exact reduced rational `numerator/denominator`
with signed arbitrary-precision numerator, positive arbitrary-precision
denominator, and gcd equal to one. Decimal and hexadecimal significands and
exponents are converted exactly; no host `float32`, `float64`, locale, or
rounding mode participates. Equality compares canonical numerator and
denominator. Pebble has no source NaN or infinity literal in this contract.

Before allocation, the scanner enforces `MaxLiteralBytes` and parses exponent
digits with checked bounded arithmetic. Materialization stops if the absolute
exponent exceeds `MaxLiteralExponent` or any integer magnitude, numerator, or
denominator would exceed `MaxLiteralBits`. Failure emits `T0508`, creates
`Error`, and retains no oversized value. Stored big integers and rationals are
owned immutable copies; helpers never expose their backing words.

The lexer token for `52` denotes a positive `IntLiteral(52)`. Source `-52` is
unary negation over that term. When the operand is a literal, generation
consumes its pristine one-cell exact-literal term: `NegateLiteral` clones and
negates the owned payload in place, updates its origin, and returns the same
`Term` and `InferID`. It allocates no inference cell and leaves no positive or
intermediate literal available for defaulting. The operation rejects foreign,
malformed, or already-unified components atomically. Thus `-128` may fit `i8`
while `128` does not, and repeated unary negation remains exact. Negation of a
nonliteral remains an operator obligation.

## Type-syntax resolution

Type resolution is a memoized query keyed by `(SyntaxRef, type-environment)`.
The environment is an ordered mapping of declared type-parameter `SymbolID`s
to rigid `TypeParameter(SymbolID)` IDs for symbolic declaration checking or
known argument `TypeID`s while applying a generic alias. Call-site inference
variables are expanded only by solver-owned `Instantiate` constraints. Type
resolution never performs lexical lookup. A `Name`, `Path`, or type-position `BracketApply`
must consume `04b` reference, qualifier, and bracket mappings.

Declaration preparation proceeds in deterministic runtime/dependency/module/source
order:

1. Prepare the compiler-owned runtime prelude described below.
2. Predeclare one `Nominal(declaration, [])` ID for each nongeneric nominal
   aggregate or opaque external declaration and register generic nominal
   constructors by their declaration `SymbolID` and ordered parameters.
3. Intern one rigid `TypeParameter(parameter SymbolID)` for every declared type
   parameter.
4. Classify every type declaration as nominal definition or transparent alias.
5. Resolve signatures, alias targets, aggregate metadata, and their contained
   type occurrences through the memoized query.
6. Freeze the declaration program. Phase 6 then resolves body-owned type
   occurrences through `Session.ResolveType` while generating their facts.

The exact syntax rules are:

- A builtin reference must select a `04b` `SymbolBuiltinType`. Its
  `BuiltinType` discriminator maps to the corresponding `Types.Builtins()`
  field. Source spelling is never consulted. Removed spellings such as
  `isize`, `usize`, and `float` therefore resolve as ordinary undefined names.
- An `Allocator` reference must select the exact
  `SymbolRuntimeType(RuntimeAllocator)` installed by `04b`; spelling is not a
  fallback. The internal `RuntimeContext` symbol has no source reference and
  therefore cannot be resolved by authored type syntax.
- Named and module-qualified types use the final name node's `04b`
  `SymbolID`. The selected symbol must be a type, type parameter, or extern
  type. No text lookup, current-module fallback, or generated qualified name
  is permitted.
- `*T`, `[N]T`, `[]T`, nonempty tuples, `?T`, and function types resolve their
  children bottom-up and intern the exact `05a` key.
- Grouping returns its child's identity. A grouped singleton is not a tuple.
  An authored tuple must contain at least one element. The parser rejects `()`
  with `P0004`; a damaged or directly constructed zero-child `TupleTerm` that
  nevertheless crosses the phase boundary emits defensive `T0504`. Neither
  path calls `TupleKey([])`.
- Function identity includes the parsed calling convention, ordered parameter
  types, result, and variadic flag. Known spellings map to `Pebble` or `C`;
  an unknown convention emits `T0501`. Whether conventions are compatible is
  not decided here.
- A type-parameter name returns the rigid
  `TypeParameter(parameter SymbolID)` ID. It is never an `InferID` in its own
  generic body.
- A direct `TypeDecl` body that is `struct`, plain `union`, tagged `union
  enum`, `enum`, or an external opaque declaration uses
  `Nominal(declaration SymbolID, arguments)`. Ordered fields, variants, and
  methods are declaration metadata outside the key.
- Applying a generic nominal resolves exactly its declared arity of arguments,
  substitutes them into declaration metadata as a descriptor, and interns
  `Nominal(declaration SymbolID, ordered arguments)`. A nongeneric nominal with
  arguments, missing arguments, extra arguments, `_`, or a partly resolved
  argument emits `T0501`; partial generic applications are not `TypeID`s.
- `type Alias = Existing` is transparent. A concrete alias resolves directly
  to the target ID. An alias chain memoizes the same final ID. A generic alias
  stores an ordered parameterized target descriptor; applying it substitutes
  arguments and returns the target's identity. An alias of a generic nominal
  constructor preserves that nominal declaration identity and arity.
- Alias resolution uses an explicit white/gray/black state table and an
  ordered stack of alias `SymbolID`s. Encountering gray emits one `T0502` at
  the closing reference, labels the declarations in cycle order, marks every
  cycle member erroneous, and returns `Error`. References flowing through the
  damaged aliases do not repeat the diagnostic. Depth is bounded separately.
- An aggregate syntax node is legal only as the direct body of its named
  `TypeDecl`. A bare aggregate in a parameter, result, binding annotation,
  tuple element, generic argument, field type, alias target nested below
  another constructor, or any other position emits `T0503`. In particular a
  field whose type is another anonymous aggregate is rejected. No structural
  aggregate key exists.
- A direct nominal body's own fields may refer recursively to its predeclared
  nominal ID. A transparent alias never creates a predeclared nominal ID.
- An array length is obtained only through `ArrayLengths`, must be known and
  nonnegative in `uint64`, and is passed unchanged to `ArrayKey`. The constant
  evaluator owns expression legality and evaluation diagnostics; `05b` owns
  the missing/unavailable boundary and the `05a` limit error translation.
- A syntax node that resolves to a value, module qualifier, function in a type
  position, error symbol, missing node, or unsupported category emits `T0501`
  unless an earlier diagnostic already explains that occurrence.

`05b` owns source-driven fixtures for concrete aliases, generic aliases, alias
chains and cycles, and bare and nested anonymous aggregate use. Parser fixtures
own authored empty-tuple rejection. Type resolution never mutates the tree or
symbol records.

### Compiler-owned runtime prelude

`Prepare` requires the two exact `04b` runtime symbols and interns these
store-owned nominal identities before authored declarations:

```text
Allocator = Nominal(RuntimeAllocator SymbolID, [])
Context   = Nominal(RuntimeContext SymbolID, [])
```

It then creates ordinary immutable `TypeDeclaration` records with these exact
ordered member descriptors:

```text
Allocator.ptr     : *void
Allocator.alloc   : fn Pebble(*void, uint) *void
Allocator.realloc : fn Pebble(*void, *void, uint) *void
Allocator.free    : fn Pebble(*void, *void) void

Context.default_allocator : Allocator
```

The `FunctionKey` parameter arrays above contain only the authored callback
parameters. Pebble calling convention identity represents the hidden context
ABI parameter; `05b` does not prepend `Context`. `uint` replaces the
prototype's `usize` evidence.

Runtime declarations use the same nominal metadata and `HasField` path as
authored declarations. They do not use a special member switch. Preparation
constructs no synthetic tree, source declaration, qualified spelling, or
second store. If either runtime symbol/member set is missing or inconsistent,
`Prepare` emits bounded `T0512`, publishes no runtime IDs through
`RuntimeTypes`, and does not fabricate recovery `TypeID`s.

## Constraints and provenance

Constraints are one closed tagged struct with variant-specific payloads, not
an interface whose behavior is delegated to unspecified implementations:

```text
Equal(a, b)
Numeric(t)
Integral(t)
Ordered(t)
HasField(receiverType, name, fieldType)
SelectMethod(receiverType, name, callableType, explicitTypeArguments, site)
Callable(calleeType, orderedSourceDestinationArguments, resultType)
Indexable(receiverType, resultType)
Sliceable(receiverType, resultType)
LiteralFits(literal, candidateType)
Shape(subject, algebraicShape)
Instantiate(templateType, substitutions, subject)
TypeOccurrence(ref, rigidOwner, subject)
ValueOccurrence(ref)
OneOf(ordered alternatives)
```

The public builder surface is constructor-based so clients cannot forge an
invalid tag/payload combination:

```go
type Term struct { /* session identity plus closed internal payload */ }
type Constraint struct { /* closed validated payload */ }

type Origin struct {
    Syntax      symbol.SyntaxRef
    Span        source.Span
    Role        string
    Symbol      symbol.SymbolID
    GenericOwner symbol.SymbolID
}

type Substitution struct {
    Parameter symbol.SymbolID
    Argument  Term
}

type CallableArgument struct {
    Source      Term
    Destination Term
}

type Alternative struct {
    Label       string
    Constraints []Constraint
}

func Equal(a, b Term, origin Origin) Constraint
func Numeric(term Term, origin Origin) Constraint
func Integral(term Term, origin Origin) Constraint
func Ordered(term Term, origin Origin) Constraint
func HasField(receiver Term, name string, field Term, origin Origin) Constraint
func SelectMethod(receiver Term, name string, callable Term, explicit []Term, site symbol.SyntaxRef, origin Origin) Constraint
func Callable(callee Term, arguments []CallableArgument, result Term, origin Origin) Constraint
func Indexable(receiver, result Term, origin Origin) Constraint
func Sliceable(receiver, result Term, origin Origin) Constraint
func LiteralFits(literal, candidate Term, origin Origin) Constraint
func ConstrainShape(subject Term, shape Shape, origin Origin) Constraint
func Instantiate(template TemplateID, substitutions []Substitution, subject Term, origin Origin) Constraint
func TypeOccurrence(ref symbol.SyntaxRef, owner symbol.SymbolID, subject Term, origin Origin) Constraint
func ValueOccurrence(ref symbol.SyntaxRef, origin Origin) Constraint
func OneOf(alternatives []Alternative, origin Origin) Constraint
```

`Origin.GenericOwner` is zero outside symbolic generic-body checking. A
capability on a rigid parameter becomes a `Requirement` only when this owner
is present. Constructors copy strings and slices, reject invalid terms or
foreign template IDs when added to a session, and never retain caller-owned
mutable storage.

`OneOf` is the bounded algebraic disjunction needed when syntax has two
genuinely different inference interpretations, initially `BracketDeferred`.
Alternatives contain only `05b` constraints; phase-6 semantic records remain
owned by phase 6 and are tagged with the corresponding alternative index.
`Selection` reports the unique chosen zero-based alternative after solving.
`OneOf` is not an operator-overload registry and does not contain callbacks or
checker policy.

`TypeOccurrence` and `ValueOccurrence` are the closed category gates used by
those two alternatives. They retain only snapshot-local identities and copied
origins. Neither accepts a callback, spelling, lookup policy, mutable tree, or
checker-owned record.

The ordered `CallableArgument` values correspond one-to-one with authored call
arguments. For each fixed function parameter, `Destination` is equated with
that parameter while `Source` remains distinct for phase-6 compatibility.
For a variadic tail, which phase 6 rejects under the initial policy,
`Destination` is equated with `Source` solely to materialize the retained
argument root; it is not a parameter compatibility decision. This lets
function structure determine fixed parameter types without turning all legal
call compatibility into equality.

This is deliberately smaller than the set of semantic relationships.
Assignments, casts, operators, calls, and indexing remain phase-6 records even
when `Callable`, `Indexable`, or `Sliceable` also supplies their closed
structural inference relation. Phase 6 decomposes each accepted language rule
into the algebraic facts above and retains the original relationship for
post-solve validation and typed-IR coercion. `05b` never receives an opaque
request whose meaning depends on a later pass.

Every constraint stores its `ConstraintID`, primary `OriginID`, and ordered
related `OriginID`s. An origin contains `SyntaxRef`, exact source span, a
stable role such as `argument 2`, `parameter 2`, `initializer`, `annotation`,
or `first equation`, and the originating symbol when present. Origins are
allocated in source order and copied into diagnostics. Rewrites and recursive
decomposition preserve the parent origin and append the most specific child
role; they never replace provenance with an internal solver location.

### Exact constraint behavior

| Constraint | Generated by | Progress, delay, success, and failure | Policy owner |
| --- | --- | --- | --- |
| `Equal(a,b)` | annotations, shared operands, bindings, returns, composite decomposition, and generic substitution | Union variables, bind a variable, merge same-kind literal type-choice classes while retaining every value, or decompose shapes. A literal paired with a known candidate is processed exactly as `LiteralFits`; the solver never diagnoses raw literal-versus-known inequality. Delay only while both sides contain unresolved variables/shapes. Equal known IDs succeed. Distinct known IDs, integer-versus-float literal kinds, or incompatible shapes fail with `T0505`; different values of the same literal kind do not conflict because equality is between their types. `Error` succeeds silently. | `05b` identity only |
| `Numeric(t)` | numeric literal/operator rules and inferred generic obligations | A numeric literal records the capability and waits for fitting. A variable attaches the obligation to its root. Known `int`, `uint`, fixed integers, `f32`, or `f64` succeeds; another known builtin or composite fails `T0507`. A rigid type parameter records `Numeric(parameter)` as a generic obligation rather than failing. | `05b` builtin category; phase 6 still owns particular operators/conversions |
| `Integral(t)` | integral-only literal/operator/index rules | Same attachment/delay behavior as `Numeric`. Known target-word or fixed-width integer succeeds; floats and nonnumeric types fail `T0507`; rigid parameters retain an obligation. | `05b` builtin category |
| `Ordered(t)` | ordering syntax and symbolic generic bodies | Variables attach; literals delay until selected; rigid parameters retain `Ordered(parameter)`. A known type makes inference progress by fixing the obligation's subject but does not prove that a particular operator is supported. It is handed to phase 6. `Error` is suppressed-complete; this constraint does not otherwise fail in `05b`, and phase 6 owns any unsatisfied concrete ordering diagnostic. | phase 6 operator policy |
| `HasField(recv,name,field)` | type-directed member access and record construction | Strip no conversions. Delay until the receiver is a known nominal, optional/pointer behavior has been made explicit by another rule, or a nominal shape identifies its declaration and arguments. Query ordered declaration metadata by `SymbolID`, instantiate its field descriptor, equate it with `field`, and succeed. A known nonnominal or missing field fails `T0507`. `Error` suppresses it. | `05b` member shape; phase 6 owns accessibility, place, method, and category policy |
| `SelectMethod(recv,name,callable,explicit,site)` | an immediate instance-method call, including a generic method selected through a deferred member/bracket | Delay until `recv` is a known nominal, pointer-to-nominal, or corresponding shape. Query only that declaration's ordered `04b Members`; require exactly one same-spelled `SymbolMethod`; substitute containing-type arguments from the receiver, relate leading explicit method-type arguments, allocate the remaining method-local inference variables once in source constraint order, instantiate the prepared method signature, and equate its complete explicit-self function shape with `callable`. Record the selected method `SymbolID` and all method-local type arguments at `site`. A nonnominal receiver, missing/wrong-category member, excess explicit argument, or damaged signature fails deterministically. `Error` suppresses it. | `05b` delayed identity and substitution; phase 6 owns call legality and bound-method rejection |
| `Callable(callee,args,result)` | indirect calls whose complete function key is not independently known during generation | Delay while the callee lacks concrete structure. Once it is a concrete function type or matching function shape, decompose its authored fixed parameters in order into each argument `Destination`, equate its result with `result`, and preserve its exact calling convention and variadic bit in the callee structure; argument `Source` terms are not equated to fixed parameters. Nonvariadic arity must match; a variadic key requires at least its fixed count and uses source identity only for tail destinations as defined above. It never invents or guesses Pebble versus C. A concrete nonfunction or impossible arity fails with `T0507`; absent independent callable evidence ends in ordinary `T0510` unresolved recovery. A rigid type parameter makes the constraint checker-deferred complete, recovers argument destinations/result to `Error` without an inference diagnostic, and invents no requirement; phase 6 retains the unsupported checker requirement. `Error` suppresses it. | `05b` closed function structure; phase 6 owns arguments, convention legality, variadic policy, and calls |
| `Indexable(recv,result)` | value indexing, including the runtime branch of a deferred bracket | Delay until receiver structure is available. Array and slice structure equate `result` with the element; known `str` equates it with `char`. Any other concrete receiver fails with `T0507`; unresolved structure uses ordinary `T0510`. A rigid type parameter makes the constraint checker-deferred complete, recovers `result` to `Error` without an inference diagnostic, and invents no trait; phase 6 retains the unsupported checker requirement. `Error` suppresses it. | `05b` closed index-result structure; phase 6 owns index type, place, bounds, and legality |
| `Sliceable(recv,result)` | authored slicing | Delay until receiver structure is available. Array and slice structure constrain `result` to `Slice(element)`; known `str` equates it with `str`. Any other concrete receiver fails with `T0507`; unresolved structure uses ordinary `T0510`. A rigid type parameter makes the constraint checker-deferred complete, recovers `result` to `Error` without an inference diagnostic, and invents no trait; phase 6 retains the unsupported checker requirement. It decides no bounds, lifetime, escape, or runtime-check policy. `Error` suppresses it. | `05b` closed slice-result structure; phase 6 owns bounds, escape, and legality |
| `LiteralFits(lit,candidate)` | literal expected types, literal equality, and final selection | Delay until candidate is a known builtin. A rigid type parameter retains an exact `LiteralFits` generic obligation. Exact integer bounds use signedness, exact width, or `LiteralTarget.WordBits`. An exact rational fits `f32` or `f64` when IEEE-754 round-to-nearest, ties-to-even produces a finite value; ordinary rounding and underflow to signed zero are allowed, overflow to infinity is not. The check uses integer/rational comparisons rather than host floating arithmetic. Success binds the literal occurrence to the candidate. A known wrong category or out-of-range value fails `T0508`. `Error` suppresses it. | `05b` literal selection; this is not conversion between concrete types |
| `Shape(subject,shape)` | tuple/array/function/optional construction, nil/none recovery, and instantiation expansion | Bind or merge a root shape, recursively match a known key, or materialize a fully known shape. Delay on unresolved leaves. Constructor, arity, convention, variadic, nominal declaration, or array-length mismatch fails `T0505`. Occurs failure is `T0506`. | `05b` structural identity |
| `Instantiate(template,subst,subject)` | generic calls, generic member receivers/results, and generic aliases | Recursively expand the known symbolic template in stable child order, replace listed rigid parameters, and emit `Shape`. Delay only if the template descriptor is damaged; missing/duplicate substitutions or invalid template structure fail `T0501`. | `05b` substitution; phase 7 owns specialization |
| `TypeOccurrence(ref,owner,subject)` | type argument of a deferred bracket | Resolve the exact immutable syntax occurrence under the selected rigid owner from the prepared program. During speculation, represent composite results as algebraic shapes and known leaves rather than interning a `TypeID`; relate the shape to `subject`. A valid occurrence that is not a type rejects only its alternative with `T0501`. In-tree `Missing`/`Error` nodes and absent or damaged immutable resolution evidence retain deterministic `T0511`. Foreign identities, malformed builder inputs, inconsistent ownership, and resource limits are fatal `T0512`. | `03b` syntax, `04b` identity/category evidence, and `05b` prepared type templates |
| `ValueOccurrence(ref)` | runtime argument of a deferred bracket | Prove from the exact immutable syntax node and stored `04b` reference/bracket evidence that the occurrence can denote a runtime value. Type-only and wrong-category occurrences reject only their alternative with `T0501`. It performs no checker policy, lookup by spelling, callback, publication, or mutation. In-tree `Missing`/`Error` nodes and absent or damaged immutable resolution evidence retain deterministic `T0511`. Foreign identities, malformed builder inputs, inconsistent ownership, and resource limits are fatal `T0512`. | `03b` syntax and `04b` immutable resolution evidence |
| `OneOf(alternatives)` | phase-6 rules with genuinely disjoint inference interpretations | Propagate common facts first. Eliminate alternatives that conflict under a rollback snapshot. Commit the only surviving alternative; zero survivors fail with the smallest branch conflict, while multiple viable alternatives fail with `T0509`. Exploration is source ordered, bounded, and never uses first-success semantics. | phase 6 defines each alternative; `05b` proves uniqueness |

No failure is emitted when any required input term is `Error`. A constraint
whose only remaining work depends on `Error` becomes suppressed-complete. A
constraint may add a genuinely independent diagnostic—for example a malformed
explicit target type—but never repeats the mismatch that produced the error.

`Ordered` on a rigid parameter may become a generic requirement. A concrete
`Ordered` fact is emitted only after phase 6 selects a rule that requires it;
it is not proof that every ordering spelling is legal for that type.

`Callable`, `Indexable`, and `Sliceable` participate in the same single solve
and the same ascending-`ConstraintID` worklists as every other delayed
constraint. Each examination and structural decomposition is charged to the
existing requeue/decomposition limits; no private retry loop or unbounded
watcher exists. They retain exact origins, wake only from monotonic root/shape
progress, are equation-insertion-order independent, and become silently
complete through `Error`. They expose only the closed structural relations in
the table and no phase-6 legality, conversion, bounds, lifetime, or trait
policy.

## Phase-6 fact-generation contract

This section constrains the client of `05b`; none of this traversal lives in
`compiler/internal/infer`. Phase 6 walks modules in
`Graph.DependencyOrder()`, declarations in tree child order, and children in
authored order. It may allocate terms in that order, but it may not inspect a
current solution to select a rule or type. All relations implied by the checked
unit are emitted before its single `Solve` call.

Phase 6 retains its own closed semantic records for assignment, call, cast,
operator, indexing, place, and control-flow relationships. Before solving,
their value handles refer to session terms; freezing replaces every required
synthetic root with a `SlotID` or another documented solution root. The
records themselves are not passed to `Session.Add` and never appear in
`Solution` as supposedly completed checks.

### Declarations and statements

- An annotated binding allocates a symbol term `S`, resolves annotation `A`,
  emits `Equal(S,A)`, generates the initializer with expected evidence from
  `A`, and records the phase-6 assignment relationship. The expectation is
  evidence, not a write.
- An unannotated initialized binding allocates `S`, generates initializer `I`,
  and emits `Equal(S,I)`. For a binding with neither usable annotation nor
  initializer, phase 6 uses `Session.Error` only as silent recovery and
  publishes the affected symbol as `TypeError` to suppress dependent
  inference cascades. This case does not emit `T0510`; phase 6 retains the
  binding form and 06b emits the sole authoritative `C0602`.
- Global and local `let`/`var` use the same equations. Mutability, initialization
  order, and whether a left side is a place belong to phase 6.
- Assignment generates left `L` and right `R` independently, passes `L` as
  expected evidence while generating `R`, and records `(R,L)` for phase-6
  conversion validation. A compound assignment additionally records its
  operator use and `(temporary,L)` result assignment without desugaring or
  duplicating evaluation.
- Each function parameter symbol equals its resolved annotation. A declared
  result `R` is resolved once. The function symbol receives a
  `Shape(Function(convention, parameters, R, variadic))`; no parameter name is
  part of that shape.
- An extern function follows the same signature equations with its authored
  convention and no body. An extern binding requires and equals its annotation.
  An extern type is a nominal opaque declaration. Extern blocks contribute
  their contained declarations but no ambient mutable checker mode.
- A type declaration publishes its predeclared nominal ID or transparent alias
  result and separately generates every field, variant, and method signature.
  It does not equate a nominal with its aggregate body structure.
- `return e` generates `e` with the enclosing declared result as expectation
  and records `(e,result)` for conversion validation. Bare return emits
  `Equal(result,void)` as an inference fact; legality on individual paths
  belongs to phase 6.
- Expression statements generate their expression and discard no constraints.
  Blocks concatenate fact sets. `if`, `while`, and C-style `for` conditions
  generate `Equal(condition,bool)`; branch/body terms do not imperatively
  merge. Range bounds and switch subjects/cases generate terms plus
  phase-6 operator and assignment records.
- `print`, `defer`, `break`, `continue`, reachability, exhaustive switching,
  definite return, loop placement, and global initialization ordering produce
  no type choice beyond their contained expressions; phase 6 validates their
  semantic legality.
- Nominal field and variant payload symbols equal their resolved type syntax.
  A record field initializer is generated with that field descriptor as its
  expected evidence and records `(value,fieldType)` for conversion validation.

### Expressions

- Integer and float tokens produce exact literal terms. String, character,
  and Boolean literals produce their known builtins. `context` requires the
  exact `Program.RuntimeTypes().Context` identity. Phase 6 emits that known
  term without textual lookup; it separately owns legality and hidden-argument
  propagation and never pretends an authored parameter is the implicit context.
  Interpolated expressions are all generated;
  the overall expression is
  `str`, while printable conversion policy remains phase 6.
- A resolved name/path reads `Resolution.Reference(SyntaxRef)` and equates the
  occurrence term with the already allocated symbol term. It never searches a
  scope or spelling. An error/deferred resolution produces `Error` and no
  second undefined-name diagnostic.
- Grouping equates the group and child. A tuple allocates element terms and a
  result term, then emits one nonempty tuple `Shape`. An array literal allocates
  one element term `E`, equates every element with `E`, and emits
  `Shape(result,Array(count,E))`. An empty array has an unconstrained `E` and
  therefore requires expected evidence or ends with `T0510`.
- Array repetition generates value `V`, sends the count occurrence to the
  constant-expression provider, and emits `Shape(result,Array(count,V))`.
- `some e` emits `Shape(result,Optional(e))`. `none` emits
  `Shape(result,Optional(fresh payload))`; without expected optional evidence
  the payload is ambiguous. `nil` similarly emits
  `Shape(result,Pointer(fresh pointee))` until phase 6 defines any broader nil
  policy. Force unwrap emits an optional shape for its operand and equates its
  result with the payload; postfix mutation remains a phase-6 operator record.
- Prefix and binary nodes allocate all operand and result terms before adding
  the equations selected by the phase-6 operator rule. Address/dereference use
  pointer `Shape` relations. Unary minus over a literal produces the exact
  signed literal rule; it never ranks numeric types.
- An explicit cast resolves target `D`, generates source `S` without replacing
  it, records `(S,D)` for phase-6 cast validation, and equates the cast result
  with `D`. No implicit cast node is inserted into syntax.
- A conditional expression, where supported by syntax, constrains its
  condition to `bool`, generates both branches with the same expected result
  term, and records both branch-to-result relationships. Neither branch is the
  winner.
- A call generates its callee and every argument, obtains a direct signature
  from `Program` or emits `Callable` over fresh ordered argument-destination
  terms and its result, then
  records each argument-to-parameter relationship. Argument expectations add
  facts during that traversal; they never cause a second traversal.
- A slice generates receiver/start/end, gives present bounds `Integral`, and
  emits `Sliceable(receiver,result)`. An index or value-mode bracket emits
  `Indexable(base,result)`, records `(base,index,result)`, and requires exactly
  one value argument.
- A type-directed field emits `HasField(receiver,name,result)`. Tuple numeric
  members use deterministic fixed-index decomposition. A statically resolved
  module or type member consumes the `04b` `SymbolID` and equates with that
  symbol's term; it performs no receiver lookup. An immediate instance-method
  call emits `SelectMethod(receiver,name,callable,explicit,site)` and separately
  constrains `callable` with the explicit receiver, authored arguments, and
  result function shape. Bound method values remain phase-6 policy.
- A record with explicit base resolves or infers its nominal type, emits a
  result equality, then one `HasField` constraint and one phase-6 assignment
  record per authored field. A base-less record uses its expected result as
  receiver evidence; if
  no unique nominal receiver emerges it is ambiguous. Duplicate/unknown field
  policy and missing required fields belong to phase 6, while lookup facts and
  field type inference belong here.
- A nongeneric anonymous function allocates parameter terms, a
  declared/inferred result term, and one function `Shape`; its body is generated
  once with return expectations. It is noncapturing and represents a globally
  hoisted function; `04b` capture records are phase-6 rejection evidence and do
  not alter its function shape. A `FunctionTerm` with authored type parameters
  is syntactically retained but unsupported in the initial checker contract;
  phase 6 produces an error term and does not invent a generic owner or walk
  the body in an invalid environment.
- `sizeof T` resolves `T` for validity and gives the expression builtin `uint`;
  it does not request layout in `05b`. Phase 6 decides whether the operand has
  a valid layout and typed IR/lowering supplies the actual value.
- `PartialMemberExpr` is delayed with its expected nominal receiver. It emits a
  field/variant obligation rather than selecting the first same-spelled member.

### Neutral brackets

`04b`'s exact `BracketMode` controls generation:

- `BracketTypeNames` resolves each ordered argument as type syntax and applies
  the selected generic declaration or generic callable. It never treats an
  argument as an index.
- `BracketValueNames` requires exactly one expression argument and emits
  a phase-6 indexing record. It never attempts generic instantiation.
- `BracketDeferred` generates the base and retains both the base's resolved
  category facts and the bracket syntax. Phase 6 constructs a two-alternative
  `OneOf` through `AddChoice`: generic application with type-argument facts, or runtime indexing
  with value-argument and `Indexable` facts. The generic branch begins with
  `TypeOccurrence(argument, owner, typeArgument)`; the runtime branch begins
  with `ValueOccurrence(argument)`. It tags every branch constraint,
  checker record, generic instantiation publication, and runtime-value solved
  slot with the exact choice and alternative index. A generic application
  nested in either deferred-bracket alternative uses
  `PublishGuardedInstantiation`; it must not use unconditional
  `PublishInstantiation`. Syntax/results that exist
  only in the runtime branch use alternative-guarded slots rather than
  unconditional syntax publication.
  Unselected type/value branches publish no successful syntax type, cause no
  defaulting, and emit no branch-local diagnostic. The solver commits only the
  unique viable branch; if both or neither remain viable it emits `T0509` and
  no typed-IR-capable branch root exists. It never uses
  capitalization, a following call, traversal order, or first-success
  heuristics.

## Deterministic unification

One solver owns one union-find table indexed by `InferID`. Each cell stores a
parent, rank/size used only for complexity, the smallest member `InferID`, an
optional known or shape binding, an ordered list of exact literal evidence,
attached capability evidence, and the earliest binding origin.

Representative selection cannot alter semantics. Rank and stable tie-breaking
choose the private physical parent; every merged root separately retains its
smallest member `InferID` as the canonical debugging/diagnostic representative.
No representative escapes the solution API. Capability/origin evidence is
merged by stable IDs, never map order.

Unification is defined as follows:

1. `Error` with anything yields `Error` and no diagnostic.
2. Known-known structural unification first compares canonical IDs. Identical
   IDs succeed. Distinct IDs recursively compare their complete `05a` keys in
   deterministic child order to locate the smallest constructor, arity,
   scalar, nominal, convention, or child mismatch for provenance, then fail;
   canonical concrete IDs never become equal merely because a conversion may
   exist. The same decomposition machinery matches a known ID against an
   algebraic shape or generic template.
3. A variable-known pair binds the variable root after reconciling any root
   shape, literal, or capabilities. The `TypeID` is never mutated.
4. A variable-variable pair merges roots and then reconciles both bindings in
   deterministic origin order.
5. A variable-literal pair appends that occurrence's exact value and origin to
   the root's type-choice class. Two integer literals merge regardless of
   value, as do two float literals; every retained value must fit the eventual
   candidate. Integer and float literal classes do not merge implicitly.
6. A variable-shape pair performs an occurs check before attaching the shape.
   Two shapes require equal constructors and recursively unify children in
   source/key order.
7. Known-shape matching requires identical constructor, arity, array length,
   calling convention, variadic bit, and nominal declaration, then recursively
   matches children obtained through `Store.Key`. A fully known shape interns
   exactly the corresponding key bottom-up.
8. `TypeParameter(SymbolID)` is rigid. It unifies only with the same known
   `TypeID` during symbolic body checking. Call-site substitution replaces a
   declared parameter in `Instantiate`; ordinary unification never binds or
   rewrites the rigid ID.
9. Function parameters and results decompose in order. Pointer, array, slice,
   tuple, optional, function, and nominal application shapes recurse through
   all identity components. Nominal declarations must match exactly; their
   field structure is irrelevant to equality.
10. Every conflict records the constraint origin plus the earliest conflicting
    binding origin. `T0505` labels both and explains the smallest differing
    constructor/child, never a union-find address or representative number.

Occurs checking asks whether the candidate shape reaches the variable's
minimum representative through shape leaves or root bindings. It uses an
explicit stack and visited set keyed by `InferID`, charges every edge to
`MaxDecompositionSteps`, and emits `T0506` on a cycle. TypeID graphs need not
be traversed for occurs checking because canonical store entries cannot
contain `InferID`s. Structural validation and occurs checking use explicit
bounded stacks. Shape matching/materialization and template reduction are
depth-recursive only after validation against the hard 1,024 ceiling; host
stack growth is therefore not an input-controlled correctness mechanism.

## Ordered fixed point and termination

The solver processes pending constraints in ascending `ConstraintID` rounds.
Completed constraints leave the pending set permanently. Every examination of
a still-delayed constraint is charged to its per-constraint requeue limit and
the session-wide total-requeue limit, including an examination that discovers
no progress. This bounds deliberately simple full-round scheduling; a future
watcher optimization may skip clean constraints but may not change semantics,
ordering, diagnostics, or the configured work bounds. Hash maps may locate
data but never supply work order.

Solving has these stages:

1. **Equality closure:** process `Equal`, merge root bindings, reconcile
   shapes, and expand immediately available `Instantiate` constraints.
2. **Capability and shape:** process `Numeric`, `Integral`, `Ordered`,
   `Shape`, and known `HasField` facts.
3. **Delayed fixed point:** process remaining member, callable, indexable,
   sliceable, instantiation, shape, and literal-fit constraints in charged
   source-ordered rounds.
4. **Choice resolution:** eliminate contradictory `OneOf` alternatives using
   rollback snapshots. If propagation cannot select one, explore every viable
   alternative and nested choice in stable index order, charging each snapshot
   to `MaxChoiceStates`. Commit only a unique viable alternative; never commit
   the first successful branch.
5. **Ambiguity pass:** classify stalled constraints and connected unresolved
   components. Do not default a literal that is connected to a nonliteral
   unresolved choice, generic argument, member receiver, callable/indexable/
   sliceable relation, or competing expected type. Terms reachable only from
   an unselected guarded slot are not roots and do not participate.
6. **Literal fitting and defaulting:** evaluate all known candidate fits in
   ascending origin order. Only a genuinely unconstrained integer literal
   defaults to `int`; only a genuinely unconstrained float literal defaults to
   `f64`. Every value in the class is then checked against that default, so an
   oversized unconstrained integer diagnoses rather than selecting a wider
   type. Defaulting emits a binding fact and reruns stages 1-3. No ordered
   numeric preference list exists.
7. **Finalization:** emit one ambiguity or unresolved diagnostic per smallest
   connected component, mark affected published entries `TypeError`, and
   freeze ordered result tables, requirements, and instantiations.

A constraint remains pending only while its required root lacks a binding,
known ID, complete shape, or member declaration. Merely examining it is not
progress, but the examination is still charged. Each successful union,
binding, shape edge,
materialized type, discharged constraint, or newly attached capability is
monotonic. Fixed point is a round with no state change.

Each constraint tracks its requeue count. Exceeding a per-constraint or total
limit emits one `T0512`, converts its connected component to `Error`, and
removes it from the pending set. The finite variable, constraint, shape-edge, step,
requeue, and diagnostic limits guarantee termination.

Choice exploration uses rollbackable union-find changes or bounded state
copies; it never mutates `types.Store` speculatively. Shapes that would require
interning remain complete keys inside the branch and are interned only after
that branch is committed. Failed or ambiguous branches therefore cannot
consume `TypeID`s or perturb deterministic interning order.

The rollback boundary includes failure state, speculative diagnostics,
type/value occurrence memo tables, solved terms, requirements, delayed method
state, and constraint progress. Losing alternatives cannot retain a memoized
category decision, solved type, requirement, default, root, or diagnostic.
`T0512` is never an alternative conflict: it replaces any earlier nonfatal
speculative conflict with its exact code, message, primary origin, and related
origins, aborts choice exploration, and cannot make another alternative appear
uniquely viable. If all alternatives
instead fail semantically, the deterministic smallest underlying conflict is
published; two category failures therefore retain `T0501` rather than being
rewritten as `T0509`.

Hash-map iteration must never determine constraint IDs, work order,
representatives, defaulting, diagnostic selection, result ordering, ordered
`Intern` calls, or final `TypeID`s. Before materialization, ready shapes are
sorted by their owning constraint ID and recursively intern children in key
order. Repeated runs over identical inputs therefore issue identical ordered
store calls and produce identical results.

## Bidirectional evidence

Phase 6 may generate an expression with an optional expected term, but
"expected" is not itself a solver constraint. The expression rule converts it
into one of three explicit forms:

1. **Identity evidence.** Add `Equal(actual, expected)` only when the language
   rule requires the expression itself to have exactly that type.
2. **Literal evidence.** Add `LiteralFits(literal, expected)`; do not first
   default the literal or model fitting as a concrete conversion.
3. **Shape evidence.** Constrain contextual forms such as `none`, empty arrays,
   base-less records, and inferred function results with `Shape` and leaf
   equations against the expected term.

For an operator family whose language rule requires its result and one or more
operands to have one identical type, a known expected destination may provide
identity evidence to the result and exactly those same-type operands before
solving. Exact numeric literals among them then receive `LiteralFits` evidence
for that selected concrete type. If the destination is `Optional(D)` and phase
6 permits optional injection at that compatibility boundary, phase 6 projects
`D` into the operator result and same-type operands while retaining the outer
result-to-optional compatibility record. It does not equate the operator result
with the optional destination.

This projection is legal only through identity or that single optional-
injection boundary. It never crosses an explicit conversion, forbidden
conversion, or an implicit conversion whose concrete source and destination
may differ. Assignment, argument, return, field, tuple-component, and branch
compatibility are not thereby converted wholesale into `Equal`. All affected
operand/result terms and literal-fit facts are created independent of operand
or equation insertion order. Thus a known `i32` destination can select both
literals in `1 + 2` as `i32`, string addition remains possible, and two
distinct concrete numeric operands still conflict without an authored cast.

An ordinary synthesized expression is not unified with its destination merely
because phase 6 later permits a conversion. Phase 6 retains that assignment,
argument, return, branch, or field relationship and validates the two solved
types against its conversion matrix. If both are already distinct known IDs,
`05b` adds no equality and reports no mismatch on behalf of conversion policy.

Annotated declarations, arguments, returns, record fields, and conditional
branches pass their destination as expected evidence. Generic call results may
use identity or shape evidence from the expected result, including
zero-argument constructors, before solving. Every expression is still
generated exactly once; expected evidence never overwrites its synthesized
term or skips its children. Conflicting identity, literal, and shape evidence
is an ordinary provenance-rich solver conflict. No expectation wins because
it was visited first.

## Generic boundary

Declared type parameters are rigid `TypeParameter(SymbolID)` `TypeID`s.
Generic named function and method bodies are generated by phase 6 and solved
symbolically once. Generic anonymous functions are outside the initial checker
contract and never create a generic owner in `05b`. Future support may retain
the same noncapturing, globally hoisted model and does not depend on closure
support. Capabilities on rigid
parameters become ordered `Requirement` records in the
declaration's semantic interface; a body is not accepted because one observed
instantiation happens to work.

At each generic call or explicit instantiation, phase 6 uses `05b` to:

1. allocate one fresh `InferID` per declared generic parameter in declaration
   order;
2. equate explicit type arguments with their corresponding variables;
3. expand symbolic receiver, parameter, and result templates through
   `Instantiate`;
4. add receiver, ordered argument, and expected-result evidence selected by
   the applicable phase-6 rules;
5. solve all evidence together and diagnose ambiguity instead of choosing a
   first matching argument;
6. record the generic declaration `SymbolID`, ordered final argument
   `TypeID`s or error state, call `SyntaxRef`, and required symbolic
   requirements.

Omitted and explicit arguments use the same variables. `_` remains unsupported
until its phase-7/source contract is accepted; it is not interned as a type.
Generic nominal calls and methods also include receiver substitutions. Nested
templates recursively substitute through every `05a` key.

Phase 7 receives immutable symbolic body types, inferred requirements, and
`Instantiation{Site, Generic, Arguments}`.
It owns proving specialization-time obligations, cache entries, recursive
in-progress handling, IR substitution, and the unresolved specialization
policy. No monomorphized name, generated C spelling, type-name string, cloned
AST, or rewritten syntax is produced by `05b`.

## Boundary with phase 6

`05b` owns exact type-syntax identity, declaration templates, inference terms,
the closed algebraic constraints, structural unification, exact literal values,
deterministic solving/defaulting, generic substitution, and final type side
tables.

Phase 6 owns the semantic traversal. Before `Solve`, it must make explicit:

- which equation, capability, shape, or literal-fit facts each syntax rule
  contributes;
- which expected contexts provide identity, literal, or shape evidence;
- the phase-6 records for assignments, calls, casts, operators, indexing,
  places, and control flow that will require post-solve validation;

After `Solve` and successful immutable snapshot construction, phase 6 owns:

- the concrete numeric conversion matrix;
- which operators each concrete or symbolic type supports;
- assignment and argument conversion policy and explicit coercion insertion;
- compatibility or adaptation across calling conventions;
- pointer arithmetic, pointer comparison, and nil policy beyond the structural
  fact above;
- place/mutability rules, control-flow validity, prohibited-capture
  diagnostics, and entry-point validity;
- constant-expression legality and the `ArrayLengthEvaluator` implementation;
- layout, target ABI, runtime checks, and lowering behavior.

`05b` must not silently add a numeric widening, choose an operator overload,
erase a calling convention, allow pointer arithmetic, insert a cast, or ask
layout to choose a type. Phase 6 consumes the matching `SemanticSnapshot` and
`Solution`, validates its retained relationships, and may produce explicit
typed-IR coercions; it may not rewrite solver results. It does not retain or
query `Program` or `types.Store`. Any equation needed to select a type must be
emitted during generation before `Solve`, never discovered during post-solve validation. A
post-solve rule that would need another equation is a phase-boundary bug and
requires the phase-6 generation contract to be corrected.

Phase 6 now settles string operators, the accepted array constant-expression
language, and the conversion/calling-convention matrices. They remain checker
policy and are not hidden 05b defaults.

## Diagnostics and recovery

Initial stable inference codes are:

| Code | Meaning |
| --- | --- |
| `T0501` | unknown type syntax, invalid type category, arity, or generic application |
| `T0502` | transparent alias cycle |
| `T0503` | anonymous or nested anonymous aggregate type |
| `T0504` | defensive zero-child tuple type crossing the parser boundary |
| `T0505` | unification or structural-shape conflict |
| `T0506` | occurs-check failure |
| `T0507` | unsatisfied inference-owned capability or missing member |
| `T0508` | numeric literal too large, wrong category, overflow, or failed fitting |
| `T0509` | ambiguous inference or neutral bracket classification |
| `T0510` | unresolved inference variable or missing expected evidence |
| `T0511` | damaged upstream resolution/syntax prevented a type result |
| `T0512` | resource limit, inconsistent input, unavailable array length, or store limit |

Diagnostics are structured data in the shared `DiagnosticSet`. The primary
label is the syntax that generated the failing constraint. Related labels are
the smallest ordered set of conflicting annotation, argument/parameter,
earlier binding, alias declaration, or generic requirement origins needed to
explain it. Messages print semantic types through a deterministic formatter
outside `types.Store`; they never expose `InferID`, union-find roots, pointer
addresses, hashes, worklist stages, or generated backend names.

One root conflict produces one diagnostic. The connected root becomes
`Error`; constraints depending only on it are suppressed. Independent
mistakes still report. Alias cycles report once per cycle, oversized literals
once per token, and unresolved components once at their smallest source
origin. When the diagnostic limit is reached, the final retained diagnostic is
replaced by one `T0512` limit diagnostic and further inference diagnostics are
suppressed. The owning session becomes fatal as soon as that replacement is
scheduled, before flush. Earlier phase diagnostics are never removed or
counted as 05b's budget.

## Resource safety

All configurable counts are checked before allocation or append. Every shape
node and leaf is charged to `MaxShapeComponents`. Type syntax, aliases, shapes,
known type decomposition, instantiation, and occurs checking use explicit
stacks or hard-depth-bounded reducers charged to their corresponding limits. Literal parsing
checks digits, exponent, and prospective bit growth before shifts or powers.
Constraints use bounded charged reexaminations and monotonic state. On any limit,
the smallest affected component becomes `Error`, one bounded diagnostic is
stored, and independent components may continue.

Every `OneOf` alternative and speculative state is charged before allocation.
Rollback logs are bounded by the same unification, decomposition, and choice
state limits as committed work.

No adversarial source can cause unbounded recursion, arbitrary-precision
allocation, retry loops, diagnostic cascades, or output proportional to an
implicit infinite expansion. Store errors are translated without retry and
without changing `05a`.

`SemanticSnapshot` construction follows the same rule: it charges each copied
record/component, validates before publication, and translates a 05a snapshot
error or ownership inconsistency to the one bounded `T0512` recovery path.

## Testing contract

Direct Go tests in `compiler/internal/infer` are authoritative where terms,
IDs, equations, or solver structure are clearer than source text:

- the same equations inserted in several orders produce the same substitutions,
  final semantic keys, and normalized diagnostics;
- variable chains report the smallest representative independent of union
  rank and remain stable across repeated runs;
- pointer, array, slice, nonempty tuple, optional, function, and nominal shapes
  unify structurally and materialize canonical `05a` IDs;
- recursive variable shapes fail occurs checking in bounded work;
- exact integer/rational equality, minimum signed values, fitting failures,
  unconstrained `int`/`f64` defaulting, and constrained literals do not use a
  numeric preference rank;
- expected-result evidence infers a zero-argument generic result;
- generic argument, receiver, explicit argument, and result evidence agree or
  report both origins;
- field and instantiation constraints delay, wake exactly on relevant root
  change, and reach a fixed point;
- callable, indexable, and sliceable constraints delay until structure is
  independently available, decompose in stable order, suppress through Error,
  reject unsupported concrete/rigid subjects as specified, and produce the
  same result under reversed equation insertion;
- delayed method selection uses only the solved receiver declaration, infers
  method-local arguments from receiver/argument/result evidence, publishes one
  stable method identity, and never searches unrelated declarations;
- `OneOf` commits a unique viable alternative, reports zero or multiple viable
  alternatives deterministically, rolls back failed facts, and never consumes
  speculative `TypeID`s;
- type/value occurrence gates select local and qualified deferred-bracket
  arguments, isolate inactive diagnostics and memoized state, retain `T0501`
  when neither category survives, retain damaged immutable evidence as
  `T0511`, and treat lowered limits, foreign identities, or malformed inputs as
  fatal `T0512` under both cold and memoized evaluation;
- ordinary and guarded solved slots are bounded, copied, snapshot-local, and
  queryable only when active; unselected guarded slots neither default nor
  diagnose their otherwise-unconstrained terms;
- semantic snapshot copying preserves deterministic descriptor/template/
  owner order and exact type IDs; mismatched program/solve tokens, stale store
  lengths, incomplete solutions, invalid type IDs, and lowered copy limits
  fail atomically with `T0512`; copied snapshots remain tree-free and safe for
  concurrent reads while the original program can serve a later independent
  sequential session;
- contextual same-type operator evidence solves `i32` arithmetic literals,
  minimum signed unary negation, and optional injection without permitting
  mixed concrete numeric types or rejecting string addition; reversed operand,
  equation, and traversal construction orders normalize identically;
- `Error` never reaches `types.Intern` and suppresses dependent cascades;
- forced map seeds and repeated runs produce identical work, diagnostics,
  ordered intern calls, `SymbolType`, and `SyntaxType` results;
- every variable, constraint, shape-component, literal, alias, syntax-depth,
  decomposition, unification, requeue, diagnostic, and type-store limit
  terminates atomically.

Type-syntax behavior owned by `05b` uses ordinary Pebble fixtures:

```text
tests/types/
  valid/*.peb
  invalid/T0501/*.peb
  invalid/T0502/*.peb
  invalid/T0503/*.peb
  invalid/T0505/*.peb
  invalid/T0506/*.peb
  invalid/T0507/*.peb
  invalid/T0508/*.peb
  invalid/T0509/*.peb
  invalid/T0510/*.peb
  invalid/T0511/*.peb
  invalid/T0512/*.peb
  recovery/*.peb
```

Required `05b` fixtures cover tuples, arrays, optionals, functions, concrete
and generic aliases, alias chains/cycles, direct nominal recursion, anonymous
and nested anonymous aggregate rejection, builtin symbol
mapping, and independent recovery after errors. Phase 6 owns source fixtures
for expression traversal, expected contexts, calls, operators, indexing,
assignments, and conversion behavior. Direct `05b` tests still cover literal
boundaries, generic evidence, ambiguity, and equation-order independence.
Multi-module cases prove qualified type references use `04b` mappings.
Runtime-prelude coverage additionally proves exact nominal identities and
members, ordinary `Allocator` resolution, hidden Context lookup, callback
calling-convention identity, normal `HasField`, damaged-input recovery, and
concurrent independent sessions over one frozen program.

Use direct assertions rather than goldens for `InferID` normalization,
constraint sets, substitutions, `TypeID` equality, and work counts. Use a
diagnostic sidecar only when exact user-visible labels or rendering matter.
Normal test runs never update goldens.

## Dependency-ordered implementation slices

### Slice 05b.1: program, session, solution, and limits

Own `compiler/internal/infer/id.go`, `program.go`, `session.go`, `solution.go`,
`config.go`, and direct tests. Implement closed IDs, lifecycle states, ordered
copied accessors, input validation, and all counters without body traversal or
solving. Complete when foreign terms, invalid templates, repeated solve, and
zero/error states cannot publish a `TypeID`, and lowered limits fail
deterministically. Handoff: exact API, defaults, ownership, and ordering.

### Slice 05b.2: type syntax, declarations, and aliases

Own `type_resolver.go`, `declaration.go`, alias diagnostics, and `tests/types`
alias/aggregate fixtures. Consume only `module.Graph`, `source.FileSet`,
`symbol.Result`, `ArrayLengthEvaluator`, and `types.Store`. Implement nominal
predeclaration, rigid parameters, all accepted composite syntax, generic
descriptors, alias memoization/cycles, anonymous aggregate rejection, and
defensive zero-child tuple recovery. Complete when every type form has a stable
result and no lookup or AST mutation exists. Handoff any exact `05a` operation discrepancy before
continuing.

### Slice 05b.3: terms, exact literals, shapes, and provenance

Own `term.go`, `literal.go`, `shape.go`, `constraint.go`, and structural tests.
Implement the closed variants, immutable arbitrary precision values, bounded
parsing, algebraic shapes, instantiation payloads, origins, and deterministic
allocation. No union-find yet. Complete when equality, copying, limits, and
negative-literal behavior have direct assertions.

### Slice 05b.4: union-find and structural unification

Own `unify.go` and solver unit tests. Implement minimum-ID representative
semantics, variable bindings, rigid parameters, recursive shape decomposition,
occurs checking, conflict origins, and bottom-up `05a` materialization using
bounded explicit stacks. Complete when order permutations and forced map seeds
produce identical normalized results and no recovery term is interned.

### Slice 05b.5: ordered worklists and capabilities

Own `solve.go`, `capability.go`, capability tables, and direct fixed-point/limit
tests. Implement source-ordered rounds, monotonic progress, delayed member/
callable/indexable/sliceable constraints, bounded choice rollback, literal
fitting/defaulting, ambiguity, requeue bounds, and error suppression. Complete
when all stages terminate, only genuinely unconstrained literals default,
closed structural constraints are equation-order independent, and first-
success choice is impossible.

### Slice 05b.6: builder publication and bidirectional primitives

Own constraint constructors, session publication tables, and direct tests for
identity, literal, and shape evidence. Implement no AST traversal. Complete
when clients can publish symbol/syntax, ordinary-slot, guarded-slot, and
retained-instantiation roots, all inputs are copied and session-checked,
inactive guarded roots cannot default or diagnose, and reversed constraint
insertion produces the same normalized semantic keys and diagnostics. Numeric
`TypeID` values need only be deterministic for the same ordered compilation
input.

### Slice 05b.7: generic templates and symbolic requirements

Own `instantiate.go`, generic portions of `declaration.go`, and direct generic
tests. Implement fresh call-site variables, template expansion,
receiver/argument/explicit/result evidence, delayed method selection, rigid
parameter requirements, and immutable phase-7 instantiations. Complete when a
client can express zero-argument expected-result inference, ambiguous evidence
diagnoses, and no AST clone or monomorphized name exists.

### Slice 05b.8: determinism and recovery hardening

Own `semantic_snapshot.go`, its direct ownership/copy/race tests, the diagnostic
formatting adapter, recovery fixtures, fuzz seeds, and repeated-run tests.
Complete when partial solutions remain queryable, immutable semantic snapshots
are deterministic and tree-free, mismatched or incomplete solutions fail
atomically with `T0512`,
successful solutions contain no unresolved inference, all limits have
adversarial coverage, and phase 6 can consume solutions without access to
solver internals.

This continuation begins only after the 05a `types.Snapshot` extension exists.
It must use that API and must not reproduce type-snapshot storage or copying in
`infer`.

Each slice handoff reports owned files, public/internal contracts, direct and
source tests, commands and results, resource limits exercised, deterministic
ordering evidence, commit, and any upstream discrepancy. A slice must not add
a second type store, change `TypeID`, decorate AST nodes, rebuild scopes, clone
generic ASTs, or hide policy in a numeric rank.

## 05a discrepancy audit and unresolved decisions

No missing `05a` operation is required by this design. `Builtins`, `Intern`,
`Kind`, and `Key` plus the closed `TypeKey` accessors are sufficient.
Unresolved composites and substitutions remain solver-owned shapes until they
can form a complete key, exactly as `05a` requires. In particular, `05b` does
not require interning an `InferID`, literal, error, partial generic application,
or forward structural placeholder.

The following are deliberately unresolved outside this contract:

- the accepted language of array constant evaluation, owned by phase 6;
- phase-6 conversion, operator, assignment, indexing, nil, pointer arithmetic,
  calling-convention, control-flow, layout, and ABI policies;
- whether the language ever gains closures; current anonymous functions are
  noncapturing and globally hoisted, and `05b` defines no closure machinery;
- the phase-7 erased-versus-monomorphized policy, visibility, cross-module
  ownership, code-size controls, and specialization recursion limits.

These decisions may refine policy obligations and consumers. They must not
change the algebraic term boundary, immutable `TypeID` identity, deterministic
solver rule, or prohibition on AST cloning.
