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
`ArrayLengthUnavailable` produces `T0512`. Until the open constant-expression
decision is resolved, phase 6 may not claim production completion without a
provider implementing its accepted constant-expression rules. The callback
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
| inference diagnostics | 50 |

Tests may lower every limit.

### Immutable declaration program

`Program` is the phase-6-facing result of type-syntax resolution. Its records
are closed data, not undefined interfaces:

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

func (p *Program) TypeDeclaration(symbol.SymbolID) (TypeDeclaration, bool)
func (p *Program) TypeDeclarations() []TypeDeclaration
func (p *Program) Signature(symbol.SymbolID) (Signature, bool)
func (p *Program) Signatures() []Signature
func (p *Program) Template(TemplateID) (TypeTemplate, bool)
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

### Session builder and immutable solution

Phase 6 generates facts through this conceptual API:

```go
func (s *Session) Variable(origin Origin) Term
func (s *Session) Known(types.TypeID) Term
func (s *Session) IntegerLiteral(text []byte, origin Origin) Term
func (s *Session) FloatLiteral(text []byte, origin Origin) Term
func (s *Session) NegateLiteral(Term, Origin) Term
func (s *Session) ResolveType(symbol.SyntaxRef, symbol.SymbolID) TypeResult
func (s *Session) Add(constraint Constraint) ConstraintID
func (s *Session) PublishSymbol(symbol.SymbolID, Term)
func (s *Session) PublishSyntax(symbol.SyntaxRef, Term)
func (s *Session) PublishInstantiation(symbol.SyntaxRef, symbol.SymbolID, []Term)
func (s *Session) Solve() *Solution
```

Every mutator fails atomically with a bounded `T0512` diagnostic after its
limit is reached. Calling a mutator after `Solve`, calling `Solve` twice, or
mixing terms from different sessions is an inconsistent-input error, not a
panic.

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

type RequirementKind uint8
const (
    RequirementNumeric RequirementKind = iota + 1
    RequirementIntegral
    RequirementOrdered
)

type Requirement struct {
    Owner   symbol.SymbolID
    Kind    RequirementKind
    Subject types.TypeID
    Origin  Origin
}

type Instantiation struct {
    Site      symbol.SyntaxRef
    Generic   symbol.SymbolID
    Arguments []TypeResult
}

type Solution struct { /* unexported immutable storage */ }

func (r *Solution) Successful() bool
func (r *Solution) SymbolType(symbol.SymbolID) (TypeResult, bool)
func (r *Solution) SymbolTypes() []SymbolType
func (r *Solution) SyntaxType(symbol.SyntaxRef) (TypeResult, bool)
func (r *Solution) SyntaxTypes() []SyntaxType
func (r *Solution) Requirements(symbol.SymbolID) []Requirement
func (r *Solution) Instantiation(symbol.SyntaxRef) (Instantiation, bool)
func (r *Solution) Selection(ConstraintID) (uint32, bool)
```

The slice accessors return copies ordered by ascending `SymbolID` and then by
`(ModuleID, NodeID)`. Requirements and instantiations return copies in stable
owner, source, and parameter order. A `Requirement` is retained only when a
phase-6-generated capability constraint targets a rigid type parameter while
checking the generic declaration identified by `Owner`; concrete policy
obligations remain owned by phase 6 and are not smuggled into this result.

Every root explicitly published by phase 6 has an entry. `05b` does not decide
which syntax is value-producing and does not invent entries by traversing the
tree. Phase 6 must publish every binding, parameter, callable, field, variant
payload, and value-producing expression required by later checking or typed
IR. Names used only as module qualifiers and damaged `04b` error symbols are
not published as successful types.

A successful solution has no inference diagnostic, no `TypeError` entry, no
unresolved `InferID`, no unmaterialized shape, and no pending inference-owned
constraint. Every published final ID belongs to `ProgramInputs.Types`. A
partially failed solution is still queryable: independent solved
entries remain `TypeFinal`, every affected or unresolved entry is `TypeError`,
and no zero or recovery ID is published. Backend-consumable typed IR requires
`Successful()` and later phase-6 success.

The program and solution own copied immutable tables. They expose no union-find nodes,
literal buffers, constraints, mutable slices, AST pointers, or syntax-node
decorations.

## Solver identities, terms, and shapes

```go
type InferID uint32
type ConstraintID uint32
type OriginID uint32
```

Zero is invalid. IDs belong to one `Session`. `InferID`s and `ConstraintID`s
are allocated monotonically in deterministic builder-call order; their numeric
values are debugging data, not language semantics.

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
creates the exact signed literal `-52` without first requiring `52` to fit the
candidate type. Thus `-128` may fit `i8` while `128` does not. Repeated unary
negation is exact. Negation of a nonliteral remains an operator obligation.

## Type-syntax resolution

Type resolution is a memoized query keyed by `(SyntaxRef, type-environment)`.
The environment is an ordered mapping of declared type-parameter `SymbolID`s
to rigid `TypeParameter(SymbolID)` IDs for symbolic declaration checking or
known argument `TypeID`s while applying a generic alias. Call-site inference
variables are expanded only by solver-owned `Instantiate` constraints. Type
resolution never performs lexical lookup. A `Name`, `Path`, or type-position `BracketApply`
must consume `04b` reference, qualifier, and bracket mappings.

Declaration preparation proceeds in deterministic dependency/module/source
order:

1. Predeclare one `Nominal(declaration, [])` ID for each nongeneric nominal
   aggregate or opaque external declaration and register generic nominal
   constructors by their declaration `SymbolID` and ordered parameters.
2. Intern one rigid `TypeParameter(parameter SymbolID)` for every declared type
   parameter.
3. Classify every type declaration as nominal definition or transparent alias.
4. Resolve signatures, alias targets, aggregate metadata, and their contained
   type occurrences through the memoized query.
5. Freeze the declaration program. Phase 6 then resolves body-owned type
   occurrences through `Session.ResolveType` while generating their facts.

The exact syntax rules are:

- A builtin reference must select a `04b` `SymbolBuiltinType`. Its
  `BuiltinType` discriminator maps to the corresponding `Types.Builtins()`
  field. Source spelling is never consulted. Removed spellings such as
  `isize`, `usize`, and `float` therefore resolve as ordinary undefined names.
- Named and module-qualified types use the final name node's `04b`
  `SymbolID`. The selected symbol must be a type, type parameter, or extern
  type. No text lookup, current-module fallback, or generated qualified name
  is permitted.
- `*T`, `[N]T`, `[]T`, nonempty tuples, `?T`, and function types resolve their
  children bottom-up and intern the exact `05a` key.
- Grouping returns its child's identity. A grouped singleton is not a tuple.
  An authored tuple must contain at least one element; damaged or empty tuple
  syntax emits `T0504` and never calls `TupleKey([])`.
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
chains and cycles, bare and nested anonymous aggregate use, and empty tuple
recovery. Type resolution never mutates the tree or symbol records.

## Constraints and provenance

Constraints are one closed tagged struct with variant-specific payloads, not
an interface whose behavior is delegated to unspecified implementations:

```text
Equal(a, b)
Numeric(t)
Integral(t)
Ordered(t)
HasField(receiverType, name, fieldType)
LiteralFits(literal, candidateType)
Shape(subject, algebraicShape)
Instantiate(templateType, substitutions, subject)
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

type Alternative struct {
    Label       string
    Constraints []Constraint
}

func Equal(a, b Term, origin Origin) Constraint
func Numeric(term Term, origin Origin) Constraint
func Integral(term Term, origin Origin) Constraint
func Ordered(term Term, origin Origin) Constraint
func HasField(receiver Term, name string, field Term, origin Origin) Constraint
func LiteralFits(literal, candidate Term, origin Origin) Constraint
func ConstrainShape(subject Term, shape Shape, origin Origin) Constraint
func Instantiate(template TemplateID, substitutions []Substitution, subject Term, origin Origin) Constraint
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

This is deliberately smaller than the set of semantic relationships. Calls,
assignments, casts, operators, and indexing are phase-6 records, not solver
constraints. Phase 6 decomposes each accepted language rule into the algebraic
facts above and retains the original relationship for post-solve validation
and typed-IR coercion. `05b` therefore never receives an opaque request whose
meaning depends on a later pass.

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
| `LiteralFits(lit,candidate)` | literal expected types, literal equality, and final selection | Delay until candidate is a known builtin. A rigid type parameter retains an exact `LiteralFits` generic obligation. Exact integer bounds use signedness, exact width, or `LiteralTarget.WordBits`. An exact rational fits `f32` or `f64` when IEEE-754 round-to-nearest, ties-to-even produces a finite value; ordinary rounding and underflow to signed zero are allowed, overflow to infinity is not. The check uses integer/rational comparisons rather than host floating arithmetic. Success binds the literal occurrence to the candidate. A known wrong category or out-of-range value fails `T0508`. `Error` suppresses it. | `05b` literal selection; this is not conversion between concrete types |
| `Shape(subject,shape)` | tuple/array/function/optional construction, nil/none recovery, and instantiation expansion | Bind or merge a root shape, recursively match a known key, or materialize a fully known shape. Delay on unresolved leaves. Constructor, arity, convention, variadic, nominal declaration, or array-length mismatch fails `T0505`. Occurs failure is `T0506`. | `05b` structural identity |
| `Instantiate(template,subst,subject)` | generic calls, generic member receivers/results, and generic aliases | Recursively expand the known symbolic template in stable child order, replace listed rigid parameters, and emit `Shape`. Delay only if the template descriptor is damaged; missing/duplicate substitutions or invalid template structure fail `T0501`. | `05b` substitution; phase 7 owns specialization |
| `OneOf(alternatives)` | phase-6 rules with genuinely disjoint inference interpretations | Propagate common facts first. Eliminate alternatives that conflict under a rollback snapshot. Commit the only surviving alternative; zero survivors fail with the smallest branch conflict, while multiple viable alternatives fail with `T0509`. Exploration is source ordered, bounded, and never uses first-success semantics. | phase 6 defines each alternative; `05b` proves uniqueness |

No failure is emitted when any required input term is `Error`. A constraint
whose only remaining work depends on `Error` becomes suppressed-complete. A
constraint may add a genuinely independent diagnostic—for example a malformed
explicit target type—but never repeats the mismatch that produced the error.

`Ordered` on a rigid parameter may become a generic requirement. A concrete
`Ordered` fact is emitted only after phase 6 selects a rule that requires it;
it is not proof that every ordering spelling is legal for that type.

## Phase-6 fact-generation contract

This section constrains the client of `05b`; none of this traversal lives in
`compiler/internal/infer`. Phase 6 walks modules in
`Graph.DependencyOrder()`, declarations in tree child order, and children in
authored order. It may allocate terms in that order, but it may not inspect a
current solution to select a rule or type. All relations implied by the checked
unit are emitted before its single `Solve` call.

Phase 6 retains its own closed semantic records for assignment, call, cast,
operator, indexing, place, and control-flow relationships. Those records hold
session terms until solving, then phase 6 queries their final `TypeResult`s and
applies its conversion and legality matrices. They are not passed to
`Session.Add` and never appear in `Solution` as supposedly completed checks.

### Declarations and statements

- An annotated binding allocates a symbol term `S`, resolves annotation `A`,
  emits `Equal(S,A)`, generates the initializer with expected evidence from
  `A`, and records the phase-6 assignment relationship. The expectation is
  evidence, not a write.
- An unannotated initialized binding allocates `S`, generates initializer `I`,
  and emits `Equal(S,I)`. A binding with neither usable annotation nor
  initializer becomes `Error` with `T0510`.
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
  and Boolean literals produce their known builtins. `context` equates with an
  explicitly declared enclosing contextual parameter when one exists; until
  phase 6 specifies any implicit context value/type beyond the calling-
  convention bit, it remains a policy obligation and cannot cause `05b` to
  invent a builtin or nominal type. Interpolated expressions are all generated;
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
  from `Program` or constrains an indirect callee with a function `Shape`, then
  records each argument-to-parameter relationship. Argument expectations add
  facts during that traversal; they never cause a second traversal.
- A slice generates receiver/start/end. Start and end receive `Integral`; the
  result shape and permitted receiver come from the selected phase-6 indexing
  rule. An index or value-mode bracket records `(base,index,result)` and
  requires exactly one value argument.
- A type-directed member emits `HasField(receiver,name,result)`. Tuple numeric
  members use deterministic fixed-index decomposition. A statically resolved
  module or type member consumes the `04b` `SymbolID` and equates with that
  symbol's term; it performs no receiver lookup. Methods retain receiver and
  call relationships without synthesizing cloned declarations.
- A record with explicit base resolves or infers its nominal type, emits a
  result equality, then one `HasField` constraint and one phase-6 assignment
  record per authored field. A base-less record uses its expected result as
  receiver evidence; if
  no unique nominal receiver emerges it is ambiguous. Duplicate/unknown field
  policy and missing required fields belong to phase 6, while lookup facts and
  field type inference belong here.
- An anonymous function allocates rigid declared type parameters, parameter
  terms, a declared/inferred result term, and one function `Shape`. Its body is
  generated symbolically once with return expectations. Capture support and
  closure layout remain phase-6/backend concerns.
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
  `OneOf`: generic application with type-argument facts, or runtime indexing
  with value-argument facts. It tags its corresponding semantic records with
  those alternative indices. The solver commits only the unique viable branch;
  if both or neither remain viable it emits `T0509`. It never uses
  capitalization, a following call, traversal order, or first-success
  heuristics.

## Deterministic unification

One solver owns one union-find table indexed by `InferID`. Each cell stores a
parent, rank/size used only for complexity, the smallest member `InferID`, an
optional known or shape binding, an ordered list of exact literal evidence,
attached capability sets, watcher IDs, and the earliest binding origin.

Representative selection cannot alter semantics. On variable-variable merge,
the root with smaller minimum-member `InferID` is the externally reported
representative; rank may choose physical parent only if `Find` reports the
minimum member and all ordered output uses it. Capability/origin/watch lists
are merged by stable IDs, never map order.

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
contain `InferID`s. All structural matching and materialization use explicit
bounded stacks; host recursion depth is not a correctness mechanism.

## Ordered worklist and termination

The solver uses queues of `ConstraintID`, bitsets for queued state, and watcher
lists keyed by minimum `InferID`. Queue order is always ascending constraint
creation order within a stage. Hash maps may locate data but never supply work
order.

Solving has these stages:

1. **Equality closure:** process `Equal`, merge root bindings, reconcile
   shapes, and expand immediately available `Instantiate` constraints.
2. **Capability and shape:** process `Numeric`, `Integral`, `Ordered`,
   `Shape`, and known `HasField` facts.
3. **Delayed fixed point:** process remaining member, instantiation, shape,
   and literal-fit constraints whose watched roots changed.
4. **Choice resolution:** eliminate contradictory `OneOf` alternatives using
   rollback snapshots. If propagation cannot select one, explore every viable
   alternative and nested choice in stable index order, charging each snapshot
   to `MaxChoiceStates`. Commit only a unique viable alternative; never commit
   the first successful branch.
5. **Ambiguity pass:** classify stalled constraints and connected unresolved
   components. Do not default a literal that is connected to a nonliteral
   unresolved choice, generic argument, member receiver, callable shape, or
   competing expected type.
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

A constraint is requeued only when a watched root changes representative,
binding kind, known ID, shape completeness, or attached capability set. Merely
examining it is not progress. Each successful union, binding, shape edge,
materialized type, discharged constraint, or newly attached capability is
monotonic. Fixed point is an empty queue with no changed watcher generation.

Each constraint tracks its requeue count. Exceeding a per-constraint or total
limit emits one `T0512`, converts its connected component to `Error`, and
removes its watchers. The finite variable, constraint, shape-edge, step,
requeue, and diagnostic limits guarantee termination.

Choice exploration uses rollbackable union-find changes or bounded state
copies; it never mutates `types.Store` speculatively. Shapes that would require
interning remain complete keys inside the branch and are interned only after
that branch is committed. Failed or ambiguous branches therefore cannot
consume `TypeID`s or perturb deterministic interning order.

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
Generic function and method bodies are generated by phase 6 and solved
symbolically once. Capabilities on rigid parameters become ordered
`Requirement` records in the
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

After `Solve`, phase 6 owns:

- the concrete numeric conversion matrix;
- which operators each concrete or symbolic type supports;
- assignment and argument conversion policy and explicit coercion insertion;
- compatibility or adaptation across calling conventions;
- pointer arithmetic, pointer comparison, and nil policy beyond the structural
  fact above;
- place/mutability rules, control-flow validity, closure support, and entry
  point validity;
- constant-expression legality and the `ArrayLengthEvaluator` implementation;
- layout, target ABI, runtime checks, and lowering behavior.

`05b` must not silently add a numeric widening, choose an operator overload,
erase a calling convention, allow pointer arithmetic, insert a cast, or ask
layout to choose a type. Phase 6 consumes `Solution`, validates its retained
relationships, and may produce explicit typed-IR coercions; it may not rewrite
solver results. Any equation needed to select a type must be emitted during
generation before `Solve`, never discovered during post-solve validation. A
post-solve rule that would need another equation is a phase-boundary bug and
requires the phase-6 generation contract to be corrected.

The current documents do not yet settle string operators, the accepted array
constant-expression language, or the conversion/calling-convention matrices.
Phase 6 owns those decisions; they are not hidden 05b defaults.

## Diagnostics and recovery

Initial stable inference codes are:

| Code | Meaning |
| --- | --- |
| `T0501` | unknown type syntax, invalid type category, arity, or generic application |
| `T0502` | transparent alias cycle |
| `T0503` | anonymous or nested anonymous aggregate type |
| `T0504` | empty tuple type |
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
suppressed. Earlier phase diagnostics are never removed or counted as 05b's
budget.

## Resource safety

All configurable counts are checked before allocation or append. Every shape
node and leaf is charged to `MaxShapeComponents`. Type syntax, aliases, shapes,
known type decomposition, instantiation, and occurs checking
use explicit stacks charged to their corresponding limits. Literal parsing
checks digits, exponent, and prospective bit growth before shifts or powers.
Constraints use bounded watcher lists and monotonic generations. On any limit,
the smallest affected component becomes `Error`, one bounded diagnostic is
stored, and independent components may continue.

Every `OneOf` alternative and speculative state is charged before allocation.
Rollback logs are bounded by the same unification, decomposition, and choice
state limits as committed work.

No adversarial source can cause unbounded recursion, arbitrary-precision
allocation, retry loops, diagnostic cascades, or output proportional to an
implicit infinite expansion. Store errors are translated without retry and
without changing `05a`.

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
- `OneOf` commits a unique viable alternative, reports zero or multiple viable
  alternatives deterministically, rolls back failed facts, and never consumes
  speculative `TypeID`s;
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
  invalid/T0504/*.peb
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
and nested anonymous aggregate rejection, empty tuple recovery, builtin symbol
mapping, and independent recovery after errors. Phase 6 owns source fixtures
for expression traversal, expected contexts, calls, operators, indexing,
assignments, and conversion behavior. Direct `05b` tests still cover literal
boundaries, generic evidence, ambiguity, and equation-order independence.
Multi-module cases prove qualified type references use `04b` mappings.

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
descriptors, alias memoization/cycles, anonymous aggregate and empty tuple
rejection. Complete when every type form has a stable result and no lookup or
AST mutation exists. Handoff any exact `05a` operation discrepancy before
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

Own `solve.go`, `worklist.go`, capability tables, and direct fixed-point/limit
tests. Implement staged queues, watchers, progress generations, delayed
constraints, bounded choice rollback, literal fitting/defaulting, ambiguity,
requeue bounds, and error suppression. Complete when all stages terminate,
only genuinely unconstrained literals default, and first-success choice is
impossible.

### Slice 05b.6: builder publication and bidirectional primitives

Own constraint constructors, session publication tables, and direct tests for
identity, literal, and shape evidence. Implement no AST traversal. Complete
when clients can publish symbol/syntax roots and retained instantiations, all
inputs are copied and session-checked, and reversed constraint insertion
produces the same normalized semantic keys and diagnostics. Numeric `TypeID`
values need only be deterministic for the same ordered compilation input.

### Slice 05b.7: generic templates and symbolic requirements

Own `generic.go` and direct generic tests. Implement fresh call-site variables,
template expansion, receiver/argument/explicit/result evidence, rigid
parameter requirements, and immutable phase-7 instantiations. Complete when a
client can express zero-argument expected-result inference, ambiguous evidence
diagnoses, and no AST clone or monomorphized name exists.

### Slice 05b.8: determinism and recovery hardening

Own the diagnostic formatting adapter, recovery fixtures, fuzz seeds, and
repeated-run tests. Complete when partial solutions remain queryable,
successful solutions contain no unresolved inference, all limits have
adversarial coverage, and phase 6 can consume solutions without access to
solver internals.

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
  calling-convention, control-flow, closure, layout, and ABI policies;
- the phase-7 erased-versus-monomorphized policy, visibility, cross-module
  ownership, code-size controls, and specialization recursion limits.

These decisions may refine policy obligations and consumers. They must not
change the algebraic term boundary, immutable `TypeID` identity, deterministic
solver rule, or prohibition on AST cloning.
