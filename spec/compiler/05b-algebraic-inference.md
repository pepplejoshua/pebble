# 05b Algebraic Inference

`05b` specifies the deterministic constraint engine between name resolution and
semantic checking. It consumes resolved syntax and immutable semantic types,
allocates solver-local inference identities, generates facts, solves them by
unification and ordered worklists, and publishes immutable symbol and syntax
type tables.

The central rule is:

> **Source traversal generates facts. The solver chooses types. Traversal order
> must never choose types.**

[05a Semantic Type Store](05a-semantic-type-store.md) is authoritative.
`05b` neither creates a second concrete type representation nor changes
`TypeID` identity. It explicitly rejects mutable AST types, pointer identity,
global checker state, repeated lexical lookup, traversal-order inference,
heuristic numeric ranking, and monomorphization through AST cloning. It also
rejects the prototype's canonical-name identity, package-global type tables,
and first-observed generic binding. The C prototype is evidence for source
cases only.

## Phase boundary

### Inputs and entry point

The implementation lives in `compiler/internal/infer`. Its entry point is:

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

type Inputs struct {
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

func Analyze(
    inputs Inputs,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) *Result
```

`Graph` supplies reachable modules, dependency order, each module's
`source.ID`, and its immutable `syntax.Tree`. `Sources` is the same
compilation-owned file set used by phases `04a` and `04b`; `05b` never reopens
a file. `Resolution` supplies the `SyntaxRef -> SymbolID`, qualifier,
`BracketMode`, member, and capture mappings from `04b`. `Types` is the one
compilation-owned `05a` store. `Analyze` is the sole writer to that store while
it runs because the first `05a` store contract is single-owner.

`LiteralTarget.WordBits` supplies the selected target's already-known native
integer width and must be 32 or 64. It affects only fitting `int` and `uint`
literals; it does not expose layout or ABI queries to `05b`. Missing or invalid
target width produces `T0512` before literal solving.

Array lengths are the one explicit upstream semantic query. `05b` requests a
nonnegative `uint64` for the length expression's `SyntaxRef`; it does not
define the permitted constant-expression language. `ArrayLengthKnown` is
interned subject to the `05a` array limit, `ArrayLengthError` reuses the
provider's diagnostic and recovers with `Error`, and
`ArrayLengthUnavailable` produces `T0512`. Until the open constant-expression
decision is resolved, a production `05b` implementation is not complete
without this provider contract being assigned to a phase.

Nil, inconsistent, cross-snapshot, or missing inputs produce one bounded
`T0512` diagnostic and an immutable partial result rather than a panic. The
entry point owns no filesystem, target, layout, backend, or process-global
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
| inference diagnostics | 50 |

Tests may lower every limit.

### Immutable result

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

type Result struct { /* unexported immutable storage */ }

func (r *Result) Successful() bool
func (r *Result) SymbolType(symbol.SymbolID) (TypeResult, bool)
func (r *Result) SymbolTypes() []SymbolType
func (r *Result) SyntaxType(symbol.SyntaxRef) (TypeResult, bool)
func (r *Result) SyntaxTypes() []SyntaxType
func (r *Result) Declaration(symbol.SymbolID) (DeclarationType, bool)
func (r *Result) GenericObligations(symbol.SymbolID) []Obligation
func (r *Result) CallInstantiation(symbol.SyntaxRef) (Instantiation, bool)
```

The slice accessors return copies ordered by ascending `SymbolID` and then by
`(ModuleID, NodeID)`. Declaration records contain the nominal category,
ordered resolved fields or variants, symbolic signature, transparent-alias
target descriptor, and an error bit. They are separate from `types.Store`;
field spelling and declaration damage never become `TypeKey` identity.
Obligations and instantiations likewise return copies in stable source and
parameter order.

Every value-producing expression occurrence visited by `05b` has a syntax
entry. Every binding, parameter, function, method, field, variant payload,
type parameter, and type declaration whose type is owned by phases 5/6 has a
symbol entry. Names used only as module qualifiers and damaged `04b` error
symbols do not acquire successful type entries.

A successful result has no inference diagnostic, no `TypeError` entry, no
unresolved `InferID`, no unmaterialized shape, and no pending inference-owned
constraint. Every published final ID belongs to `inputs.Types`. Phase-6 policy
obligations may be present but cannot have been mistaken for successful policy
checks. A partially failed result is still queryable: independent solved
entries remain `TypeFinal`, every affected or unresolved entry is `TypeError`,
and no zero or recovery ID is published. Backend-consumable typed IR requires
`Successful()` and later phase-6 success.

The result owns copied immutable tables. It exposes no union-find nodes,
literal buffers, constraints, mutable slices, AST pointers, or syntax-node
decorations.

## Solver identities, terms, and shapes

```go
type InferID uint32
type ConstraintID uint32
type OriginID uint32
```

Zero is invalid. IDs belong to one `Analyze` invocation. `InferID`s and
`ConstraintID`s are allocated monotonically in deterministic fact-generation
order; their numeric values are debugging data, not language semantics.

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

A `Shape(subject, pattern)` constraint says that `subject` has that structure.
Shapes are solver-owned constraint payloads, not semantic types, not generally
addressable identities, and not stored in `05a`. When every leaf becomes a
known ID, the solver interns the corresponding `TypeKey` bottom-up and binds
the subject to `Known(TypeID)`. Matching a known ID against a shape decomposes
the `05a` key and emits leaf equations. This is the only bridge from unresolved
composites to the immutable store.

An `Instantiate(template, substitutions, subject)` constraint is the compact
form used for generic signatures and aliases. `template` is a known symbolic
`TypeID` that may contain rigid `TypeParameter` IDs; substitutions are ordered
`(parameter SymbolID, Variable(InferID))` pairs. It expands deterministically
into a shape by recursively decomposing the template, replacing only the
listed rigid parameters, and then constrains `subject`. It is not a second
store or a partially interned `TypeID`.

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

Resolution proceeds in deterministic dependency/module/source order:

1. Predeclare one `Nominal(declaration, [])` ID for each nongeneric nominal
   aggregate or opaque external declaration and register generic nominal
   constructors by their declaration `SymbolID` and ordered parameters.
2. Intern one rigid `TypeParameter(parameter SymbolID)` for every declared type
   parameter.
3. Classify every type declaration as nominal definition or transparent alias.
4. Resolve signatures, alias targets, aggregate metadata, and authored type
   occurrences through the memoized query.
5. Generate body constraints only after all declaration-level descriptors are
   available, without requiring declaration source order.

The exact syntax rules are:

- Builtin spellings map only through `Types.Builtins()` to `bool`, `char`,
  `str`, `void`, `int`, `uint`, `i8`...`i64`, `u8`...`u64`, `f32`, and `f64`.
  Removed spellings such as `isize`, `usize`, and `float` are unknown types.
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
Callable(fnType, ordered argumentTypes, resultType)
HasField(receiverType, name, fieldType)
Assignable(sourceType, destinationType)
LiteralFits(literal, candidateType)
Shape(subject, algebraicShape)
Instantiate(templateType, substitutions, subject)
Indexable(receiverType, indexType, elementType)
Castable(sourceType, destinationType)
OperatorUse(operator, ordered operands, resultType)
```

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
| `Equal(a,b)` | annotations, shared operands, bindings, returns, composite decomposition, and generic substitution | Union variables, bind a variable, merge same-kind literal type-choice classes while retaining every value, or decompose shapes. Delay only while both sides contain unresolved variables/shapes. Equal known IDs succeed. Distinct known IDs, integer-versus-float literal kinds, or incompatible shapes fail with `T0505`; different values of the same literal kind do not conflict because equality is between their types. `Error` succeeds silently. | `05b` identity only |
| `Numeric(t)` | numeric literal/operator rules and inferred generic obligations | A numeric literal records the capability and waits for fitting. A variable attaches the obligation to its root. Known `int`, `uint`, fixed integers, `f32`, or `f64` succeeds; another known builtin or composite fails `T0507`. A rigid type parameter records `Numeric(parameter)` as a generic obligation rather than failing. | `05b` builtin category; phase 6 still owns particular operators/conversions |
| `Integral(t)` | integral-only literal/operator/index rules | Same attachment/delay behavior as `Numeric`. Known target-word or fixed-width integer succeeds; floats and nonnumeric types fail `T0507`; rigid parameters retain an obligation. | `05b` builtin category |
| `Ordered(t)` | ordering syntax and symbolic generic bodies | Variables attach; literals delay until selected; rigid parameters retain `Ordered(parameter)`. A known type makes inference progress by fixing the obligation's subject but does not prove that a particular operator is supported. It is handed to phase 6. `Error` is suppressed-complete; this constraint does not otherwise fail in `05b`, and phase 6 owns any unsatisfied concrete ordering diagnostic. | phase 6 operator policy |
| `Callable(f,args,r)` | every call | If `f` is a known function, decompose ordered parameters/result, validate structural arity, emit argument `Assignable` constraints and result equality, and succeed. If `f` has a function shape, decompose it. If unresolved, index the constraint by its root and delay. A known nonfunction or impossible arity fails `T0507`. Variadic expansion is structural; element compatibility is `Assignable`. Direct invocation does not equate distinct calling conventions. | `05b` call shape and arity; phase 6 call legality and convention rules |
| `HasField(recv,name,field)` | type-directed member access and record construction | Strip no conversions. Delay until the receiver is a known nominal, optional/pointer behavior has been made explicit by another rule, or a nominal shape identifies its declaration and arguments. Query ordered declaration metadata by `SymbolID`, instantiate its field descriptor, equate it with `field`, and succeed. A known nonnominal or missing field fails `T0507`. `Error` suppresses it. | `05b` member shape; phase 6 owns accessibility, place, method, and category policy |
| `Assignable(src,dst)` | annotations, assignment, arguments, returns, and record fields | Equal types unify immediately. A literal emits `LiteralFits` against a known destination. Otherwise attach/delay until both sides are resolved, then record a phase-6 assignment obligation without choosing or inserting a conversion. It never turns one known concrete type into another. | phase 6 conversion matrix |
| `LiteralFits(lit,candidate)` | literal expected types, literal equality, and final selection | Delay until candidate is a known builtin. A rigid type parameter retains an exact `LiteralFits` generic obligation. Exact integer bounds use signedness, exact width, or `LiteralTarget.WordBits`. An exact rational fits `f32` or `f64` when IEEE-754 round-to-nearest, ties-to-even produces a finite value; ordinary rounding and underflow to signed zero are allowed, overflow to infinity is not. The check uses integer/rational comparisons rather than host floating arithmetic. Success binds the literal occurrence to the candidate. A known wrong category or out-of-range value fails `T0508`. `Error` suppresses it. | `05b` literal selection; this is not conversion between concrete types |
| `Shape(subject,shape)` | tuple/array/function/optional construction, nil/none recovery, and instantiation expansion | Bind or merge a root shape, recursively match a known key, or materialize a fully known shape. Delay on unresolved leaves. Constructor, arity, convention, variadic, nominal declaration, or array-length mismatch fails `T0505`. Occurs failure is `T0506`. | `05b` structural identity |
| `Instantiate(template,subst,subject)` | generic calls, generic member receivers/results, and generic aliases | Recursively expand the known symbolic template in stable child order, replace listed rigid parameters, and emit `Shape`. Delay only if the template descriptor is damaged; missing/duplicate substitutions or invalid template structure fail `T0501`. | `05b` substitution; phase 7 owns specialization |
| `Indexable(recv,index,elem)` | value-mode `BracketApply` and slice/index syntax | Attach and delay until receiver shape is known. Arrays and slices expose their element and require an integral index; tuple-index literals may resolve a fixed element. Other known shapes are retained for phase-6 rejection. It never guesses generic application from indexability. | `05b` built-in shapes; phase 6 owns permitted categories and bounds/runtime checks |
| `Castable(src,dst)` | explicit `as` | Resolve the destination type and publish it as the cast expression result; retain source and destination as a phase-6 obligation. It does not add equality or perform a conversion. | phase 6 conversion matrix and unsafe rules |
| `OperatorUse(op,operands,result)` | prefix, postfix, binary, compound assignment, and language-defined operator syntax | Retains exact token, arity, operand/result terms, and origins. Policy-free fixed facts are generated separately: `&&`/`||` operands and result equal `bool`; comparison/equality results equal `bool`; ordering also emits operand equality plus `Ordered`; ordinary numeric spellings emit same-type equations plus `Numeric` or `Integral` only after phase 6 classifies that spelling as the numeric form. Otherwise the obligation remains for phase 6. | phase 6 operator support, pointer arithmetic, string operations, and mutation rules |

No failure is emitted when any required input term is `Error`. A constraint
whose only remaining work depends on `Error` becomes suppressed-complete. A
constraint may add a genuinely independent diagnostic—for example a malformed
explicit target type—but never repeats the mismatch that produced the error.

`Ordered`, unresolved known-known `Assignable`, unresolved known-shape
`Indexable`, `Castable`, and `OperatorUse` can be solved for inference while
remaining explicit phase-6 obligations. They are not counted as unresolved
inference variables and are not silently accepted as language policy.

## Fact generation

Generation walks modules in `Graph.DependencyOrder()`, declarations in tree
child order, and children in authored order. It may allocate IDs in that order,
but it may not inspect a current solution to select a rule or type. All
relations implied by a node are emitted before solving. Solving may run after
each bounded declaration group for memory control only if the complete fact
set for that group is independent of interim choices and produces the same
result as one whole-module solve.

### Declarations and statements

- An annotated binding allocates a symbol term `S`, resolves annotation `A`,
  emits `Equal(S,A)`, generates the initializer with expected term `A`, and
  emits `Assignable(initializer,A)`. The expectation is evidence, not a write.
- An unannotated initialized binding allocates `S`, generates initializer `I`,
  and emits `Equal(S,I)`. A binding with neither usable annotation nor
  initializer becomes `Error` with `T0510`.
- Global and local `let`/`var` use the same equations. Mutability, initialization
  order, and whether a left side is a place belong to phase 6.
- Assignment generates left `L` and right `R` independently, passes `L` as
  expected evidence while generating `R`, and emits `Assignable(R,L)`. A
  compound assignment additionally emits `OperatorUse(op,L,R,temporary)` and
  `Assignable(temporary,L)` without desugaring or duplicating evaluation.
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
  and emits `Assignable(e,result)`. Bare return emits `Equal(result,void)` as
  an inference fact; legality on individual paths belongs to phase 6.
- Expression statements generate their expression and discard no constraints.
  Blocks concatenate fact sets. `if`, `while`, and C-style `for` conditions
  generate `Equal(condition,bool)`; branch/body terms do not imperatively
  merge. Range bounds and switch subjects/cases generate terms plus
  `OperatorUse`/`Assignable` relationships retained for phase 6.
- `print`, `defer`, `break`, `continue`, reachability, exhaustive switching,
  definite return, loop placement, and global initialization ordering produce
  no type choice beyond their contained expressions; phase 6 validates their
  semantic legality.
- Nominal field and variant payload symbols equal their resolved type syntax.
  A record field initializer is generated with that field descriptor as its
  expected term and emits `Assignable(value,fieldType)`.

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
  result with the payload; postfix mutation remains `OperatorUse`.
- Prefix and binary nodes allocate all operand and result terms before adding
  `OperatorUse` and the fixed equations described above. Address/dereference
  use pointer `Shape` relations. Unary minus over a literal produces the exact
  signed literal rule; it never ranks numeric types.
- An explicit cast resolves target `D`, generates source `S` without replacing
  it, emits `Castable(S,D)`, and equates the cast result with `D`. No implicit
  cast node is inserted into syntax.
- A conditional expression, where supported by syntax, constrains its
  condition to `bool`, generates both branches with the same expected result
  term, and emits `Assignable(then,result)` and
  `Assignable(else,result)`. Neither branch is the winner.
- A call generates its callee and every argument before emitting
  `Callable(callee,args,result)`. Argument expectations arise by callable
  decomposition and add constraints; they never cause a second traversal.
- A slice generates receiver/start/end. Start and end receive `Integral`; the
  result shape and permitted receiver are an `Indexable`/phase-6 rule. An index
  or value-mode bracket emits `Indexable(base,index,result)` and requires
  exactly one value argument.
- A type-directed member emits `HasField(receiver,name,result)`. Tuple numeric
  members use deterministic fixed-index decomposition. A statically resolved
  module or type member consumes the `04b` `SymbolID` and equates with that
  symbol's term; it performs no receiver lookup. Methods retain receiver and
  callable constraints without synthesizing cloned declarations.
- A record with explicit base resolves or infers its nominal type, emits a
  result equality, then one `HasField` and `Assignable` pair per authored
  field. A base-less record uses its expected result as receiver evidence; if
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
  `Indexable`. It never attempts generic instantiation.
- `BracketDeferred` generates the base and retains both the base's resolved
  category facts and the bracket syntax. Once the base becomes a known generic
  declaration or runtime value, it selects the corresponding already-defined
  branch. If both are viable, neither is viable, or the category stays
  unresolved at fixed point, it emits `T0509`; it never uses capitalization,
  a following call, traversal order, or indexability heuristics.

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
   `Shape`, known `Callable`, known `HasField`, and built-in `Indexable` facts.
3. **Delayed fixed point:** process remaining call, member, instantiation,
   assignability, literal-fit, index, and neutral-bracket constraints whose
   watched roots changed.
4. **Ambiguity pass:** classify stalled constraints and connected unresolved
   components. Do not default a literal that is connected to a nonliteral
   unresolved choice, generic argument, member receiver, callable shape, or
   competing expected type.
5. **Literal fitting and defaulting:** evaluate all known candidate fits in
   ascending origin order. Only a genuinely unconstrained integer literal
   defaults to `int`; only a genuinely unconstrained float literal defaults to
   `f64`. Every value in the class is then checked against that default, so an
   oversized unconstrained integer diagnoses rather than selecting a wider
   type. Defaulting emits a binding fact and reruns stages 1-3. No ordered
   numeric preference list exists.
6. **Finalization:** emit one ambiguity or unresolved diagnostic per smallest
   connected component, mark affected published entries `TypeError`, and
   freeze ordered result tables and phase-6 obligations.

A constraint is requeued only when a watched root changes representative,
binding kind, known ID, shape completeness, or attached capability set. Merely
examining it is not progress. Each successful union, binding, shape edge,
materialized type, discharged constraint, or newly attached capability is
monotonic. Fixed point is an empty queue with no changed watcher generation.

Each constraint tracks its requeue count. Exceeding a per-constraint or total
limit emits one `T0512`, converts its connected component to `Error`, and
removes its watchers. The finite variable, constraint, shape-edge, step,
requeue, and diagnostic limits guarantee termination.

Hash-map iteration must never determine constraint IDs, work order,
representatives, defaulting, diagnostic selection, result ordering, ordered
`Intern` calls, or final `TypeID`s. Before materialization, ready shapes are
sorted by their owning constraint ID and recursively intern children in key
order. Repeated runs over identical inputs therefore issue identical ordered
store calls and produce identical results.

## Bidirectional evidence

Generation accepts an optional expected term. Expectations always add
constraints; they never overwrite a term or skip generation of the expression:

- annotated declarations add annotation equality and initializer
  assignability;
- assignment sends the destination term to the right side and retains the
  explicit `Assignable` constraint;
- callable decomposition sends each parameter term to the matching argument;
- each return expression receives the declared function result;
- record construction sends each resolved field descriptor to its initializer;
- an expected call result is equated with the call's result term and therefore
  contributes to generic substitutions, including zero-argument constructors;
- conditional branches share one result expectation through equations rather
  than a then-first or else-first choice.

Conflicting synthesized and expected evidence is an ordinary provenance-rich
unification conflict. No expected type has priority merely because it was
visited first.

## Generic boundary

Declared type parameters are rigid `TypeParameter(SymbolID)` `TypeID`s.
Generic function and method bodies are generated and solved symbolically once.
Capabilities on rigid parameters become ordered `Obligation` records in the
declaration's semantic interface; a body is not accepted because one observed
instantiation happens to work.

At each generic call or explicit instantiation, `05b`:

1. allocates one fresh `InferID` per declared generic parameter in declaration
   order;
2. equates explicit type arguments with their corresponding variables;
3. expands symbolic receiver, parameter, and result templates through
   `Instantiate`;
4. adds receiver, ordered argument, and expected-result constraints;
5. solves all evidence together and diagnoses ambiguity instead of choosing a
   first matching argument;
6. records the generic declaration `SymbolID`, ordered final argument
   `TypeID`s or error state, call `SyntaxRef`, and required symbolic
   obligations.

Omitted and explicit arguments use the same variables. `_` remains unsupported
until its phase-7/source contract is accepted; it is not interned as a type.
Generic nominal calls and methods also include receiver substitutions. Nested
templates recursively substitute through every `05a` key.

Phase 7 receives immutable symbolic body types, inferred obligations, and
`Instantiation{GenericSymbolID, ordered concrete TypeIDs, call SyntaxRef}`.
It owns proving specialization-time obligations, cache entries, recursive
in-progress handling, IR substitution, and the unresolved specialization
policy. No monomorphized name, generated C spelling, type-name string, cloned
AST, or rewritten syntax is produced by `05b`.

## Boundary with phase 6

`05b` owns exact type-syntax identity, inference terms, equations, structural
unification, literal values, deterministic solving/defaulting, call/member
shape discovery, generic argument inference, and final type side tables.

Phase 6 owns and must make explicit:

- the concrete numeric conversion matrix;
- which operators each concrete or symbolic type supports;
- assignment and argument conversion policy and explicit coercion insertion;
- compatibility or adaptation across calling conventions;
- pointer arithmetic, pointer comparison, and nil policy beyond the structural
  fact above;
- place/mutability rules, control-flow validity, closure support, and entry
  point validity;
- constant-expression legality unless reassigned by its own specification;
- layout, target ABI, runtime checks, and lowering behavior.

Consequently `Assignable`, `Castable`, concrete `Ordered`, policy-dependent
`Indexable`, and `OperatorUse` can be inference-complete but remain phase-6
obligations. `05b` must not silently add a numeric widening, choose an operator
overload, erase a calling convention, allow pointer arithmetic, insert a cast,
or ask layout to choose a type. Phase 6 consumes the immutable obligation and
type tables; it may reject or add explicit typed-IR coercions, but it may not
imperatively rewrite `05b` solutions. If an accepted future phase-6 policy
needs new equations to select types, those equations must become an explicit
input to `Analyze` in a revised boundary and be solved in the same worklist,
not applied after publication.

The current documents do not settle string operators, array
constant-expression ownership, or the conversion/calling-convention matrices.
These remain explicit handoff decisions, not hidden 05b defaults.

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
| `T0507` | unsatisfied inference-owned capability, noncallable value, or missing member |
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
- callable and member constraints delay, wake exactly on relevant root change,
  and reach a fixed point;
- `Error` never reaches `types.Intern` and suppresses dependent cascades;
- forced map seeds and repeated runs produce identical work, diagnostics,
  ordered intern calls, `SymbolType`, and `SyntaxType` results;
- every variable, constraint, shape-component, literal, alias, syntax-depth,
  decomposition, unification, requeue, diagnostic, and type-store limit
  terminates atomically.

Language behavior uses ordinary Pebble fixtures:

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

Required fixtures cover annotated/inferred bindings, order-independent
expressions, expected arguments/returns/record fields, generic calls and
expected-result inference, neutral brackets in all three `BracketMode`s,
delayed calls and fields, tuples/arrays/optionals/functions, concrete and
generic aliases, alias chains/cycles, direct nominal recursion, anonymous and
nested anonymous aggregate rejection, empty tuple recovery, literal boundaries,
ambiguous empty arrays/`none`, deterministic diagnostics, and independent
recovery after errors. Multi-module cases prove qualified references use 04b
mappings.

Use direct assertions rather than goldens for `InferID` normalization,
constraint sets, substitutions, `TypeID` equality, and work counts. Use a
diagnostic sidecar only when exact user-visible labels or rendering matter.
Normal test runs never update goldens.

## Dependency-ordered implementation slices

### Slice 05b.1: identities, immutable result, and limits

Own `compiler/internal/infer/id.go`, `result.go`, `config.go`, and direct tests.
Implement closed IDs, states, ordered copied accessors, input validation, and
all counters without traversal or solving. Complete when zero/foreign/error
states cannot publish a `TypeID` and lowered limits fail deterministically.
Handoff: exact API, defaults, result ordering, and input ownership.

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
constraints, literal fitting/defaulting, ambiguity, requeue bounds, and error
suppression. Complete when all stages terminate and only genuinely
unconstrained literals default.

### Slice 05b.6: syntax fact generation and bidirectional evidence

Own `generate.go` plus source fixtures for declarations, statements,
expressions, calls, records, members, optionals, and brackets. Emit the exact
facts above from immutable trees and 04b mappings; never query interim solver
answers during traversal. Complete when reversed source/equation cases produce
identical semantic results and every relevant node has a side-table entry.

### Slice 05b.7: generic calls and symbolic obligations

Own `generic.go` and generic source/direct tests. Implement fresh call-site
variables, template expansion, receiver/argument/explicit/result evidence,
symbolic body obligations, and immutable phase-7 instantiations. Complete when
zero-argument expected-result inference works, ambiguous calls diagnose, and
no AST clone or monomorphized name exists.

### Slice 05b.8: phase-6 integration and recovery hardening

Own the explicit phase-6 obligation handoff, diagnostic formatting adapter,
recovery fixtures, fuzz seeds, and repeated-run tests. Do not decide the open
conversion/operator/ABI policies inside this slice. Complete when partial
results remain queryable, successful results contain no unresolved inference,
all limits have adversarial coverage, and phase 6 can consume obligations
without mutating syntax or solver results.

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

- ownership and accepted language of array constant evaluation;
- phase-6 conversion, operator, assignment, indexing, nil, pointer arithmetic,
  calling-convention, control-flow, closure, layout, and ABI policies;
- the phase-7 erased-versus-monomorphized policy, visibility, cross-module
  ownership, code-size controls, and specialization recursion limits.

These decisions may refine policy obligations and consumers. They must not
change the algebraic term boundary, immutable `TypeID` identity, deterministic
solver rule, or prohibition on AST cloning.
