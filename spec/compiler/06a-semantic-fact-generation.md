# 06a Semantic Fact Generation

`06a` is the pre-solve half of phase 6. It creates the phase-6 constant
evaluator, invokes `05b Prepare`, performs the single semantic traversal,
emits every inference fact, freezes checker-owned semantic records, invokes
`05b Solve` exactly once, and hands the immutable result to `06b`.

`06a` and `06b` are implementation tasks within `compiler/internal/check`,
not public compiler phases. The eventual package entry point owns both tasks;
there is no exported serialized fact format between them.

The lifecycle is normative:

```text
validate checker inputs
  -> construct 06a ConstantEvaluator
  -> infer.Prepare using that ArrayLengthEvaluator
  -> infer.NewSession
  -> one deterministic semantic traversal
  -> freeze publications and retained records
  -> session.Solve exactly once
  -> immutable in-package handoff to 06b
```

No checker or validator may add an equation, capability, shape, literal fit,
instantiation, method selection, choice, or publication after `Solve` begins.
A validation rule that needs a missing inference fact is an `06a` defect, not
permission for `06b` to repair the solution.

The surface AST remains immutable. `06a` never decorates nodes, rebuilds a
scope, performs lexical or textual lookup, reopens source files, clones a
generic AST, creates a source declaration, generates a backend name, computes
layout, specializes a generic, or calls `Solve` more than once.

## Authoritative boundaries

- [03b Surface Tree](03b-surface-tree.md) owns node kinds, authored child
  order, spans, and recovery nodes.
- [04b Name Resolution](04b-name-resolution.md) owns `SyntaxRef`, `SymbolID`,
  scopes, references, bracket classification, members, captures, and runtime
  symbol identities.
- [05a Semantic Type Store](05a-semantic-type-store.md) owns immutable
  `TypeID` identity and decomposition.
- [05b Algebraic Inference](05b-algebraic-inference.md) owns declaration
  preparation, terms, constraints, solving, and `Solution`.
- [07 Generics](07-generics.md) owns the symbolic-body and specialization
  boundary.
- [09 Typed IR and Caching](09-typed-ir-and-caching.md) requires resolved IDs
  and explicit post-solve policy; typed IR construction belongs to `06b`.

Where this document assigns a language rule, it repartitions the generation
portion of [06 Checking and Conversions](06-checking-and-conversions.md).
Post-solve legality, compatibility, coercions, control-flow validation, entry
validation, and typed-IR construction remain `06b` work.

## Package contract, configuration, and ownership

The shared checker package receives one compilation snapshot:

```go
type Inputs struct {
    Graph         *module.Graph
    Sources       *source.FileSet
    Resolution    *symbol.Result
    Types         *types.Store
    LiteralTarget infer.LiteralTarget
}

type Config struct {
    Inference              infer.Config
    MaxSyntaxVisits        uint32
    MaxTraversalDepth      uint32
    MaxSemanticRecords     uint32
    MaxRecordComponents    uint32
    MaxControlDepth        uint32
    MaxTrackedPlaces       uint32
    MaxGenericRequirements uint32
    MaxConstantDepth       uint32
    MaxConstantOperations  uint64
    MaxConstantBits        uint32
    MaxDiagnostics         uint32

    // 06b-owned policy fields are added by the 06b specification.
}
```

Zero-valued generation limits select:

| Limit | Default |
| --- | ---: |
| syntax visits | 4,194,304 |
| traversal depth | 1,024 |
| semantic records | 4,194,304 |
| components across retained records and handoff metadata | 8,388,608 |
| nested control regions | 1,024 |
| tracked places | 1,048,576 |
| generic requirement candidates | 1,048,576 |
| constant depth | 256 |
| constant operations | 1,048,576 |
| constant integer magnitude bits | 1,048,576 |
| generation diagnostics | 100 |

`Inference` is passed unchanged to `infer.Prepare` and `infer.NewSession`.
`LiteralTarget` must satisfy `05b`. Tests may lower every limit. Counts are
checked before allocation or append; failure is atomic.

The package-private entry for this task is:

```go
func run06a(
    inputs Inputs,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) *solveHandoff
```

The eventual exported checker entry calls `run06a` and then the `06b`
consumer in the same package. `run06a` owns its `ConstantEvaluator`, `Program`,
`Session`, traversal stacks, term table, record arenas, counters, and handoff.
It does not accept a caller-created session and therefore cannot accidentally
solve a foreign or previously solved session.

One compilation owns one `types.Store`. `infer.Prepare`, the session lifetime,
`Solve`, and post-solve snapshot construction are serialized because `05a`
permits no concurrent store access during interning or snapshot copying.
Frozen earlier-phase inputs may otherwise be shared for independent
compilations only under their own contracts.

### Snapshot validation

Before `infer.Prepare`, `run06a` validates non-nil inputs, valid literal target,
every graph module's source/tree relationship, and that resolution references
use reachable `ModuleID`/`NodeID` pairs. The evaluator and mutable generation
may retain the exact inputs only through traversal and solving. The final
handoff instead receives the tree-free copied compilation snapshot defined
below. `infer.Prepare` performs its own store, resolution, runtime-prelude, and
declaration coherence checks.

After `Prepare`, `06a` requires:

- every ready authored type/function declaration to have the corresponding
  `Program` descriptor;
- every descriptor `SymbolID` to exist in the supplied resolution snapshot;
- every concrete/template `TypeID` to belong to the supplied store;
- `Program.RuntimeTypes()` to contain the exact allocator/context nominal IDs
  prepared from `Resolution.Runtime`;
- every tree node visited to produce a `SyntaxRef{Module, Node}` whose span
  belongs to that module's source file.

Compact IDs cannot prove foreign-snapshot ownership when coincidentally in
range. As in `05a`, the driver must preserve snapshot ownership. Observable
inconsistency produces bounded recovery rather than lookup or reconstruction.

## Internal identities and provenance

`06a` keeps these identities distinct:

| Identity | Use in `06a` |
| --- | --- |
| `SyntaxRef` | one authored occurrence and the key for syntax publication |
| `SymbolID` | one resolved declaration/member and the key for symbol publication |
| `TypeID` | immutable known or solved semantic type |
| `InferID` | mutable session variable hidden inside `infer.Term` |
| `source.Span` | diagnostic label/evaluation origin, never identity |

Every generated fact uses an `infer.Origin` containing the occurrence's
`SyntaxRef`, exact tree span, stable role, relevant `SymbolID`, and current
generic owner. Related roles use authored ordinal text such as `argument 2`,
`field 3`, or `right operand`. Source spans never replace `SyntaxRef`, and
neither span nor spelling is used to recover a symbol.

The checker uses monotonic package-private handles:

```go
type valueID uint32
type recordID uint32
type controlID uint32

// controlID is nonzero, snapshot-local, monotonic allocation identity.
type controlRegion struct {
    ID       controlID
    Parent   controlID
    Depth    uint32
    Children []controlID
}

type alternativeTag struct {
    Choice infer.ConstraintID
    Index  uint32
    Guarded bool
}

type valueRootKind uint8
const (
    rootSyntax valueRootKind = iota + 1
    rootSymbol
    rootInstantiation
    rootMethod
    rootSlot
)

type valueRoot struct {
    Kind          valueRootKind
    Syntax        symbol.SyntaxRef
    Symbol        symbol.SymbolID
    Slot          infer.SlotID
    Parameter     uint32 // zero-based instantiation/method argument ordinal
    Alternative   alternativeTag
}

type generatedValue struct {
    ID     valueID
    Term   infer.Term       // mutable-session lifetime only
    Root   valueRoot        // required when 06b needs the solved value
    Origin infer.Origin
}
```

Zero handles are invalid. Arenas append in traversal order and never recycle.
`infer.Term` and `InferID` do not cross a successful handoff. Before solving,
the freezer proves that every value required by `06b` has a solution-readable
root. Checker-owned synthetic values use ordinary or alternative-guarded
`infer.SlotID` roots; they never borrow a `SymbolID` or `SyntaxRef`.

## Constant evaluator and `ArrayLengthEvaluator`

The evaluator must exist before `infer.Prepare`, but it remains an internal
part of the checker:

```go
type constantEvaluator struct { /* snapshot, memo tables, counters */ }

func newConstantEvaluator(
    inputs Inputs,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) *constantEvaluator

func (e *constantEvaluator) ArrayLength(
    ref symbol.SyntaxRef,
) infer.ArrayLengthResult
```

`run06a` passes the same pointer as `ProgramInputs.ArrayLengths`. The evaluator
reads only `Graph`, `Sources`, immutable trees, decoded literal data, and
`Resolution` identities. It performs no type query, solver query, layout,
filesystem access, or function-body semantic traversal.

The accepted constant language is:

```text
integer, Boolean, character, and string literals
grouping
integer unary - and ~; Boolean unary !
integer + - * / % << >> & | ^
Boolean && ||
same-kind == != and ordered scalar comparisons
references to module-level let declarations with constant initializers
enum variant values
```

Array lengths and array-repeat counts accept only the integer subset. The
final result must be nonnegative and fit `uint64`; `05a` applies its own array
limit. Calls, casts, `sizeof`, floats, optionals, pointers, aggregates,
indexing, interpolation, functions, parameters, locals, and `var` are not
constant expressions.

Evaluation uses arbitrary-precision integers. Division truncates toward zero;
remainder has the dividend's sign. `&&` and `||` short-circuit. References use
resolved `SymbolID`s and a white/gray/black memo table, so forward and imported
constants are deterministic and cycles report once. Depth, operations, and
prospective bit growth are charged before work. Division by zero, negative or
over-limit shifts, category errors, cycles, nonrepresentable lengths, and
limits produce one `C0614` at the smallest source origin.

`ArrayLength` returns:

- `ArrayLengthKnown` only with a valid `uint64`;
- `ArrayLengthError` after the evaluator has emitted the sole semantic
  diagnostic for that occurrence/component;
- `ArrayLengthUnavailable` only for inconsistent snapshot state or a resource
  boundary that prevents a semantic answer.

Results are memoized by `SyntaxRef`; repeated `05b` requests never duplicate a
diagnostic. Evaluation uses an explicit bounded stack. The same memoized value
engine supplies switch constants and global-constant classification records
for `06b`.

Constant evaluation is the required pre-`Prepare` query, not a second checker
fact traversal: it creates no term, publication, place, control region, or
semantic record. When the semantic visitor later reaches a length/count
occurrence, it attaches the memoized result and does not evaluate that constant
subtree again. Each syntax node is still dispatched once by the checker
visitor.

## Deterministic traversal

Traversal order is:

1. `Graph.DependencyOrder()`;
2. one module's `File` declarations in tree child order;
3. declaration, statement, expression, type, and recovery children in authored
   order;
4. parameter names within one authored parameter group in name order;
5. fields, variants, arguments, elements, and cases in authored order.

The visitor uses an explicit stack. A node is charged and marked before its
children are pushed. Reaching the same `SyntaxRef` a second time is `C0619`;
expected evidence is passed into the first visit and never causes a revisit.
Map iteration may locate state but never selects visit, allocation,
constraint, publication, record, diagnostic, or handoff order.

The visitor maintains explicit callable, generic-owner, nominal-owner,
control-region, defer-region, expected-evidence, and bracket-alternative
contexts. These are semantic stacks, not rebuilt lexical scopes. All name and
member identities come from `04b`.

### Closed `03b` dispatch

Every node kind defined by `03b` is handled exactly once:

| Surface nodes | Generation action |
| --- | --- |
| `File`, `EndOfFile`, `ImportDecl` | visit ordered declarations and the final EOF child; `EndOfFile` and imports produce no semantic value or inference fact |
| `BindingDecl`, `ExternBinding` | binding rules and symbol publication |
| `TypeDecl`, `ExternType`, `StructType`, `UnionType`, `EnumType`, `FieldDecl`, `VariantDecl` | consume prepared descriptors and visit authored members/types |
| `FunctionDecl`, `ExternFunction`, `FunctionTerm`, `Parameter`, `TypeParameter`, `ExternDecl`, `ExternBlock` | callable/signature/body rules |
| `BlockStmt`, `ReturnStmt`, `IfStmt`, `WhileStmt`, `RangeLoopStmt`, `ForStmt`, `SwitchStmt`, `SwitchCase`, `DeferStmt`, `PrintStmt`, `BreakStmt`, `ContinueStmt`, `AssignmentStmt`, `ExpressionStmt` | statement facts and control records |
| `Name`, `Path`, `Literal`, `InterpolatedString`, `ContextExpr`, `SomeExpr`, `SizeofExpr`, `GroupedTerm`, `TupleTerm` | primary/shared term rules |
| `ArrayExpr`, `ArrayRepeatExpr`, `RecordExpr`, `RecordField`, `PartialMemberExpr` | contextual/aggregate rules; `RecordField` is the structural container consumed by its enclosing record rule |
| `PrefixTerm`, `PostfixExpr`, `BinaryExpr`, `CastExpr` | operator/cast facts |
| `CallExpr`, `BracketApply`, `SliceExpr`, `MemberExpr` | call/member/bracket/index facts |
| `OptionalType`, `SliceType`, `ArrayType`, plus shared terms used in a known type position | prepared-type consumption or one `Session.ResolveType` query |
| `Missing`, `Error` | session error term or enclosing-node skip; no inferred meaning |

`EndOfFile` is explicitly dispatched and produces no semantic value or fact.
`RecordField` is explicitly dispatched in the enclosing `RecordExpr` context:
its own `SyntaxRef`, first-child `Name` `SyntaxRef`, authored name bytes/span,
and final value participate in the retained `fieldValue` and `HasField` fact.
The final value/recovery child is visited once with field evidence, and any
intervening `Missing` child is visited as recovery. A missing name has no
member identity and suppresses that field fact. Neither kind is replaced by an
invented payload.

Every authored type occurrence is visited. Declaration-owned type occurrences
consume the already prepared `Program` descriptor and are not re-resolved.
Body-owned annotations, cast targets, `sizeof` operands, explicit type
arguments, and function-literal signature types call `Session.ResolveType`
once with the containing callable/type owner. The sole exception is a generic
anonymous function's unsupported subtree: its type occurrences are consumed
without resolution because no valid owner exists. A type occurrence is never
treated as a value unless it is inside a selected value interpretation of a
neutral bracket.

## Expected-type evidence

Expected type is an input to a node's sole visit, not a hidden constraint:

```go
type expectedKind uint8
const (
    expectNone expectedKind = iota
    expectIdentity
    expectLiteral
    expectShape
)

type expectedType struct {
    Kind        expectedKind
    Destination valueID
    Role        compatibilityRole
}
```

The expression rule converts it into explicit facts:

| Evidence | Required generation |
| --- | --- |
| identity | `Equal(actual, destination)` |
| literal | `LiteralFits(exactLiteral, candidate)` |
| shape | one algebraic `Shape` plus ordered evidence for authored leaves |

Identity evidence is used only where the expression itself must have exact
identity: annotations on symbols, Boolean conditions, same-type operator
peers, explicit generic arguments, function shapes, and other identity rules
listed below. A legal later conversion is never modeled as `Equal`.

Literal evidence selects a concrete numeric type without defaulting first. If
the known destination is `?D` and phase-6 compatibility permits injection, a
direct literal receives `LiteralFits(literal,D)` and a retained optional-
injection compatibility record; the literal is not fitted to `?D`.

Shape evidence applies to `nil`, `none`, `some`, tuple/array literals, base-less
records, function literals, and generic results whose authored structure needs
context. It propagates only through matching authored components. Expected
tuple components flow to tuple elements; expected record identity flows to
field descriptors; expected function shape flows to declared parameters and
result. Explicit-only or forbidden conversions never project evidence.

When an operator row below requires the result and named operands to have one
identical type, a known destination may be projected into the operator result
and exactly those same-type operands during their sole visits. The generator
adds identity evidence for the result/operands and `LiteralFits` for exact
numeric literals against the selected concrete destination. This applies
uniformly at annotated initializers, arguments, returns, record fields, tuple
components, and every other known directional destination.

If that destination is `Optional(D)` and the retained compatibility boundary
permits optional injection, project only `D` into the operator result and
same-type operands and retain the outer result-to-`Optional(D)` compatibility
record. Do not project through a cast, a forbidden conversion, or an implicit
conversion that permits distinct concrete source/destination types. This is
not a rule that turns general assignment, argument, return, field, component,
or branch compatibility into `Equal`. Terms are allocated before projection
and facts are emitted in fixed source/role order, so reversing operand
traversal or equation insertion cannot choose a type.

Every assignment, fixed argument, return, record field, tuple component,
optional injection, and future branch-result boundary retains a directional
compatibility record even when expected evidence was emitted. `06b` decides
identity/implicit/explicit/forbidden after solving.

## Publications and retained records

`06a` publishes all roots required by `05b`, `06b`, typed IR, and phase 7:

- `PublishSymbol` for every valid concrete binding, parameter, callable,
  field, variant payload, enum variant, method, and range iterator;
- `PublishSyntax` for every unconditional value-producing expression
  occurrence, including `void` expressions;
- `PublishSlot` for each checker-owned synthetic destination/result needed by
  retained records, including indirect-call parameter/result leaves,
  record-field destination terms, and compound-operation temporaries;
- `PublishGuardedSlot` for every value or synthetic result that exists only in
  one deferred-bracket alternative;
- `PublishInstantiation` once for each explicit or inferred generic
  application site that exists unconditionally;
- `PublishGuardedInstantiation` once for each generic application that exists
  only inside one deferred-bracket alternative, tagged with that exact choice
  and alternative;
- the `SelectMethod` site required for `Solution.Method`.

Generic constructors without a complete application, type-only occurrences,
module qualifiers, imports, aggregate containers, and damaged error identities
are not successful value roots. The value-producing syntax inside a deferred
runtime branch is represented by guarded slot roots and becomes readable only
when that branch is selected. Duplicate publication is `C0619` after the
underlying `05b` operation reports inconsistent input.

The record arena is closed:

```go
type recordHeader struct {
    ID          recordID
    Syntax      symbol.SyntaxRef
    Span        source.Span
    Owner       symbol.SymbolID
    Alternative alternativeTag
    Suppressed  bool
}

type literalPayload struct { Kind literalKind; NumericBytes []byte; Bool bool; Rune rune; Text string }
type interpolationPart struct { Kind interpolationPartKind; Span source.Span; Text string; Value valueID }
type expressionRecord struct { Header recordHeader; Kind expressionKind; Result valueID; Children []valueID; Symbol symbol.SymbolID; Literal literalPayload; Parts []interpolationPart; Specialized recordID }
type bindingRecord struct { Header recordHeader; Symbol symbol.SymbolID; Kind bindingKind; Annotation, Initializer valueID; AnnotationPresent, InitializerPresent, Global, Mutable bool }
type callableRecord struct { Header recordHeader; Kind callableKind; Symbol symbol.SymbolID; Expression valueID; Convention types.CallingConvention; Parameters []valueID; Result valueID; Variadic, BodyPresent, ExpressionBody, Inline bool; Captures []symbol.SymbolID }
type typeUseRecord struct { Header recordHeader; Kind typeUseKind; Type valueID }
type assignmentRecord struct { Header recordHeader; Kind assignmentKind; Place, Source valueID; Operator token.Kind; Statement symbol.SyntaxRef }
type compatibilityRecord struct { Header recordHeader; Source, Destination valueID; Role compatibilityRole; Ordinal uint32; DestinationSymbol symbol.SymbolID; DestinationSpan source.Span }
type callTarget struct { Kind callKind; Symbol symbol.SymbolID; Site symbol.SyntaxRef; Convention types.CallingConvention; ConventionKnown bool; FixedCount uint32; Variadic bool }
type callArgument struct { Source, Destination valueID; Ordinal uint32; Variadic bool }
type callRecord struct { Header recordHeader; Callee, Receiver, Result valueID; Arguments []callArgument; Target callTarget }
type castRecord struct { Header recordHeader; Source, Destination, Result valueID }
type operatorRecord struct { Header recordHeader; Form operatorForm; Family operatorFamily; Token token.Kind; Operands []valueID; Result valueID; GenericOwner symbol.SymbolID }
type indexRecord struct { Header recordHeader; Mode indexMode; Base, Start, End, Result valueID; StartPresent, EndPresent bool; KnownArrayLength uint64; HasKnownArrayLength bool; EscapeDestination symbol.SymbolID }
type placeProjection struct { Kind placeKind; Base valueID; Member symbol.SymbolID; TupleOrdinal uint32; Index valueID }
type placeRecord struct { Header recordHeader; Root symbol.SymbolID; RootKind symbol.SymbolKind; RootMutable bool; Projections []placeProjection }
type memberRecord struct { Header recordHeader; Kind memberKind; Base, Result valueID; Member symbol.SymbolID; Name string; NameSpan source.Span; TupleOrdinal uint32 }
type fieldValue struct { Field, NameSyntax symbol.SyntaxRef; Name string; NameSpan source.Span; Member symbol.SymbolID; Value, Destination valueID; Ordinal uint32 }
type aggregateRecord struct { Header recordHeader; Kind aggregateKind; Result, Receiver valueID; Declaration symbol.SymbolID; Fields []fieldValue; DeclarationFields []symbol.SymbolID }
type controlValue struct { Role controlValueRole; Value valueID; Ordinal uint32 }
type controlRecord struct { Header recordHeader; Kind controlKind; Region, Target controlID; Callable callableRef; StatementForm statementForm; Values []controlValue; ConditionPresent, ElsePresent, RangeInclusive bool }
type deferRecord struct { Header recordHeader; Region controlID; Ordinal uint32; Statement symbol.SyntaxRef }
type callableRef struct { Symbol symbol.SymbolID; Syntax symbol.SyntaxRef }
type contextFlowRecord struct { Header recordHeader; Kind contextFlowKind; Caller callableRef; Callee valueID; Context types.TypeID }
type requirementRecord struct { Header recordHeader; Kind requirementKind; Subject valueID; Operator token.Kind }
type unsupportedCallableRecord struct { Header recordHeader; TypeParameters []symbol.SyntaxRef }
```

The discriminator declarations are closed and use this exact order:

```go
const ( literalInteger literalKind = iota + 1; literalFloat; literalBool; literalChar; literalString; literalNil; literalNone )
const ( interpolationText interpolationPartKind = iota + 1; interpolationValue )
const ( expressionName expressionKind = iota + 1; expressionPath; expressionLiteral; expressionInterpolated; expressionContext; expressionSome; expressionSizeof; expressionGrouped; expressionTuple; expressionArray; expressionArrayRepeat; expressionRecordValue; expressionFunction; expressionPartialMember; expressionPrefix; expressionPostfix; expressionBinary; expressionCast; expressionCall; expressionBracket; expressionSlice; expressionMember )
const ( bindingLocalLet bindingKind = iota + 1; bindingLocalVar; bindingGlobalLet; bindingGlobalVar; bindingExternLet; bindingExternVar; bindingParameter; bindingRangeIterator )
const ( callableNamed callableKind = iota + 1; callableMethod; callableExtern; callableLiteral )
const ( typeUseAnnotation typeUseKind = iota + 1; typeUseCastTarget; typeUseSizeof; typeUseExplicitArgument )
const ( assignmentSimple assignmentKind = iota + 1; assignmentCompound )
const ( callDirect callKind = iota + 1; callIndirect; callMethod; callVariant )
const ( operatorPrefix operatorForm = iota + 1; operatorPostfix; operatorBinary )
const ( operatorLiteralNegate operatorFamily = iota + 1; operatorNumericSame; operatorAdd; operatorIntegralSame; operatorShift; operatorBoolean; operatorOrdering; operatorEquality; operatorAddress; operatorDereference; operatorOptionalForce; operatorMutation )
const ( aggregateStruct aggregateKind = iota + 1; aggregateEnumVariant; aggregateTaggedVariant )
const ( indexValue indexMode = iota + 1; indexSlice )
const ( placeStorage placeKind = iota + 1; placeDereference; placeField; placeTuple; placeIndex )
const ( memberStatic memberKind = iota + 1; memberField; memberTuple; memberMethod; memberVariant )
const ( controlFunction controlKind = iota + 1; controlBlock; controlReturn; controlIf; controlWhile; controlRangeLoop; controlFor; controlSwitch; controlSwitchCase; controlBreak; controlContinue; controlDefer; controlPrint; controlExpression )
const ( statementPrint statementForm = iota + 1; statementDiscard; statementAssignment; statementCall; statementPostfixUpdate; statementOther )
const ( valueCondition controlValueRole = iota + 1; valueSubject; valueCase; valueReturn; valueRangeStart; valueRangeEnd; valueRangeIterator; valuePrintOperand; valueDiscarded )
const ( compatibilityAssignment compatibilityRole = iota + 1; compatibilityArgument; compatibilityReturn; compatibilityRecordField; compatibilityTupleComponent; compatibilityOptionalInjection; compatibilityBranch )
const ( contextExpression contextFlowKind = iota + 1; contextForward; contextNone; contextIndirect )
const ( requirementNumeric requirementKind = iota + 1; requirementIntegral; requirementOrdered; requirementEquatable; requirementLiteralFits; requirementUnsupportedField; requirementUnsupportedMethod; requirementUnsupportedIndex; requirementUnsupportedSlice; requirementUnsupportedCall; requirementUnsupportedConversion; requirementUnsupportedLayout; requirementUnsupportedPrint; requirementUnsupportedConstruction )
```

`operatorFamily` uses the exact rows in the operator table below and is paired
with the authoritative authored `token.Kind`; no unknown or extension tag is
valid in any discriminator.

Every successfully generated value expression receives one `expressionRecord`
in traversal order. `Children` contains its authored runtime-evaluation
children in left-to-right order; type-only, field-label, recovery, and
constant-only children remain represented by their owning specialized/type/
aggregate records. `Symbol` is nonzero only for a resolved name/path or static
value identity. `Specialized` names the same-syntax call, cast, operator,
member, aggregate, or index record when that policy record owns additional
semantics. Literal payloads copy storage. Integer/float payloads retain exact
authored token bytes; Boolean, character, and string payloads retain their
validated decoded values. `06b` canonicalizes frozen numeric bytes into the IR
with the accepted literal decoder; no later phase rereads source spelling.
Interpolation parts copy decoded text and value handles in source order.

The records above retain the complete frozen payload required by 06b:
expression structure and literal/interpolation data; direct/member identities;
authored convention evidence and variadic metadata; fixed argument
destinations; copied authored member names and declaration field order;
storage category/mutability and the full ordered projection chain; present
bounds and escape destination; references into the sole frozen control-region
hierarchy;
named-or-anonymous callable identity; explicit condition/else/range modes;
callable owner/signature/captures/inline request; binding form; type-use root;
candidate target; statement form; and every ordered value role. A zero
optional handle is allowed only when its corresponding presence flag or record
kind says the authored component is absent.

Constructors copy component slices, charge every component, and reject foreign
or zero handles atomically. Alternative-tagged records are consumed by `06b`
only when their `OneOf` selection matches. Records contain identities and
handles, never callbacks, AST pointers, generated names, layouts, conversions,
or typed-IR nodes.

## Declaration and type facts

### Imports, types, fields, and variants

Imports generate no semantic value. A ready nongeneric type declaration or
transparent alias publishes the `Program` concrete type. A generic type
declaration remains a constructor `SymbolID` and publishes no standalone
`TypeID`.

Nominal declarations consume their immutable ordered `Program` metadata.
Each field or variant payload symbol is related to its prepared template in
the declaration's rigid parameter environment and published. Enum variant
symbols are shaped as the containing enum nominal. The nominal is never
equated with its fields or variants. Methods use the callable rules below.
Wrong category, damaged descriptor, or missing prepared metadata becomes one
error term and a suppressed record; `06a` does not reconstruct metadata.

### Bindings and externs

For a binding symbol term `S`:

| Authored form | Facts |
| --- | --- |
| annotation `A`, initializer `I` | resolve `A`; `Equal(S,A)`; visit `I` with expectation from `A`; retain `(I,A)` compatibility |
| initializer `I` only | visit `I`; `Equal(S,I)` |
| annotation only | resolve `A`; `Equal(S,A)`; retain missing-initializer policy record for `06b` |
| neither usable | use silent `Session.Error`; publish the affected symbol as `TypeError` only to suppress inference cascades; retain the empty form for 06b `C0602` |

Global and local facts are identical. `let`/`var`, initialization policy, and
global-constant legality are retained metadata for `06b`; they do not change
type equations. An extern binding resolves and equals its required annotation.
Extern blocks are containers and do not create ambient checker state.

### Functions, methods, and function literals

For each expanded parameter symbol `P`, resolve its prepared annotation `A`,
emit `Equal(P,A)`, and publish `P`. Resolve the declared result once. Shape and
publish a nongeneric callable as:

```text
Function(authored convention, ordered authored parameter terms,
         authored result term, authored variadic bit)
```

Names, `inline`, bodies, and the hidden runtime context are not `FunctionShape`
components. The callable record nevertheless retains the authored `inline`
bit as an optimization request for typed IR; it has no inference effect. A
generic callable symbol remains declaration-level; its body is visited once in
the rigid environment and call sites instantiate its templates.
An expression body is visited once with the declared result expectation and
retains a return record; no surface `ReturnStmt` is synthesized.

A method body inherits containing nominal type parameters before its own type
parameters. Its explicit first `self` parameter remains in the authored
function shape. Instance-call binding is a call fact, not a signature rewrite.
Extern functions publish their prepared exact signatures and have no body.
`FunctionTerm` with no type parameters follows the same shape/body rule and
publishes its expression root. It may reference module/global declarations but
must not capture enclosing parameters, locals, loop bindings, or other
function-local storage. The function literal is identified by its
`FunctionTerm` `SyntaxRef` and callable record, not a fabricated `SymbolID`;
`06b` and lowering treat a valid literal as a globally hoisted function
identity. Backend naming remains outside `06a`.

`06a` copies `04b`'s ordered captured `SymbolID`s into the callable record. It
constructs no environment and publishes no capture value. This retained list,
the callable's `FunctionTerm` span, and the captured symbols' declaration spans
are the complete stable evidence for `06b` diagnostic `C0617`: the function
span is primary and declaration spans are related labels in capture order. The
implicit Pebble `Context` is calling-convention state, not a lexical capture.
Any callable with capture evidence is semantically invalid: its syntax root is
published as `TypeError`, not as a successful function value, while its body is
still visited once for independent facts and bounded recovery.

`FunctionTerm` with one or more authored type parameters is syntactically
valid but unsupported in the initial checker contract. `06a` visits the
function node and its signature/type-parameter recovery children to satisfy
closed dispatch, creates one error term and `unsupportedCallableRecord`, and
does not resolve a generic owner, fabricate a `SymbolID`, semantically walk the
body under an invalid generic environment, publish a successful function
value, or emit generic requirements/capture facts for it. The ordinary visitor
still consumes every signature/body/recovery descendant once in a closed
unsupported context that charges visits and validates child shape but emits no
facts or values. `06b` owns the stable `C0608` diagnostic. Generic anonymous-
function owner identity, inference, and lowering are an explicit future
feature; future support may retain the same noncapturing, globally hoisted
model and does not depend on closures. No anonymous-function symbol kind is
added.

## Runtime context and hidden propagation

`ContextExpr` generation is exact:

1. Read `Program.RuntimeTypes()`; do not resolve a name.
2. Require a valid store-owned `Context` `TypeID`; otherwise create
   `Session.Error` with the existing upstream explanation.
3. Create `Known(runtime.Context)`, publish the expression `SyntaxRef`, and
   retain a `contextFlowRecord` identifying the enclosing callable.

No `Context` symbol is installed into an authored scope, no parameter symbol
is generated, and no `Context` term is prepended to `FunctionShape` or
`FunctionKey` parameters.

Every call retains hidden-context propagation facts independent of type
compatibility:

- a direct Pebble-convention call records `forwardContext` from the enclosing
  callable to the call site;
- a direct C-convention call records `noContext`;
- an indirect call records its callee root and the enclosing context source so
  `06b` can classify the solved convention;
- a Pebble function/literal body has an implicit context source; a C body has
  none;
- the runtime entry adapter is not an authored parameter and remains a later
  driver/lowering responsibility.

These records are facts about the Pebble calling convention. They add no
equation and never affect function type identity. `06b` validates unavailable
context uses and lowers successful forwarding explicitly.

## Statement and control facts

Statements are visited in authored order and retain structure without deciding
post-solve legality:

- `BlockStmt`: allocate a lexical control/defer region and visit children.
- `ReturnStmt`: bare return emits `Equal(result,void)`; value return visits the
  expression with result expectation and retains return compatibility.
- `IfStmt`, `WhileStmt`, authored `ForStmt` condition: visit condition and emit
  `Equal(condition,bool)`; visit all authored arms/clauses once. The control
  record sets `ConditionPresent`; `IfStmt` also records `ElsePresent` exactly.
- omitted `for` condition: retain semantic-true control metadata without
  inventing a syntax node or publication and leave `ConditionPresent` false.
- `RangeLoopStmt`: visit both bounds, emit `Integral` for each and identity
  evidence between their types, relate/publish the optional iterator, and
  retain inclusive/exclusive mode in `RangeInclusive`.
- `SwitchStmt`: visit subject, every case value, and every body. Scalar cases
  receive subject identity/literal evidence and constant-evaluator records.
  Nominal cases retain the subject and member identity/name for post-solve
  variant selection and narrowing. The switch control record sets
  `ElsePresent` exactly.
- `BreakStmt`/`ContinueStmt`: retain the nearest structural candidate target
  from the control stack; missing/illegal target policy belongs to `06b`.
- `DeferStmt`: allocate a defer record for the current lexical region and visit
  the deferred statement once under a `deferred` context. It is not revisited
  on each exit.
- `PrintStmt`: visit every operand and retain a print record; printability is
  post-solve policy.
- `ExpressionStmt`: visit its expression and retain discard context.

The frozen `controlRegion` arena is the sole authority for parentage, derived
depth, and ordered region children. A `controlRecord` contains no parent or
child list. Region-owning kinds (`controlFunction`, `controlBlock`,
`controlIf`, loop, switch, and switch-case kinds) name their own region; leaf
statements name the lexical region that contains them. A region-owning
record's structural arms or bodies are that region's children in the
kind-specific authored order.
Control records retain loop/switch candidate targets, callable ownership,
authored statement order through record allocation, defer registration order,
and syntax spans. `06a` does not calculate reachability, exhaustiveness,
definite return, defer edge expansion, or entry-point validity.

## Expression facts

Every successful value-expression rule appends exactly one `expressionRecord`
at the occurrence's `SyntaxRef` while the node is visited. The record freezes
the semantic expression kind, result handle, authored runtime-evaluation
children, resolved symbol when applicable, literal/interpolation payload, and
the ID of any same-syntax specialized policy record. It is structural handoff
data for closed IR, not typed IR and not an alternate AST: it contains no token
tree, mutable child, inferred type, legality decision, coercion, or lowering
node. `Missing`, `Error`, type-only occurrences, `EndOfFile`, `RecordField`
containers, and rejected generic function terms receive no successful
expression record.

### Names, literals, grouping, and contextual forms

- `Name`/`Path`: consume `Resolution.Reference(SyntaxRef)`, relate the
  occurrence to the preallocated symbol term with `Equal`, and publish it. A
  qualifier, type, generic constructor, or error symbol in value position uses
  `Session.Error`; there is no lookup fallback.
- integer/float literal: copy the exact authored bytes into `literalPayload`,
  call the exact `05b` literal constructor with those bytes, and publish the
  term;
- Boolean/character/string literal: copy the lexer's validated decoded value
  into `literalPayload` and publish the corresponding known builtin;
- `nil`: allocate pointee `P`, result `R`, emit `Shape(R,Pointer(P))`;
- `none`: allocate payload `P`, result `R`, emit `Shape(R,Optional(P))`;
- `some E`: visit `E`, emit `Shape(R,Optional(E))`;
- grouping: visit the child, emit `Equal(group,child)`, preserve the child's
  place candidate, and publish the group;
- tuple: visit nonempty elements, emit one ordered tuple shape, apply matching
  expected components, and retain element compatibility records;
- interpolation: copy decoded text parts and embedded value handles in order
  into `interpolationPart`, then publish known `str`;
- `context`: use the runtime-context rule above;
- `sizeof T`: visit/resolve `T` once and publish known `uint`; retain the type
  result for `06b` layout-legality validation.

### Arrays, records, and aggregates

An array list allocates one element term `E`, visits each element once with
applicable expected element evidence, emits `Equal(element,E)` for each, then
`Shape(result,Array(authoredCount,E))`; its expression record retains the
ordered element handles. Empty arrays use a fresh `E` and depend on expected
shape or `05b` ambiguity recovery.

Array repetition visits the value once, obtains the count exclusively from the
constant evaluator, and emits `Shape(result,Array(count,value))`. The count
syntax is visited as a type/constant occurrence, not published as a runtime
value. Its expression record retains the value handle; the array `TypeID` and
frozen constant result carry the count without rereading count syntax.

An explicit record base resolves or infers one nominal result. A base-less
record receives nominal shape evidence from its expectation. Each authored
initializer is one `RecordField` child. Dispatch it as a structural container:
consume its first `Name` through the existing 04b identity where available,
visit any intervening recovery child, allocate and publish a slot for the
field-destination term, emit `HasField(receiver,name,fieldDestination)`, visit
its final value/recovery child with field expectation, and retain the exact
copied name, name span, authored ordinal, value, destination, and selected
member identity when 04b already supplies one.
Duplicate, missing, category, and construction legality are `06b` policy. The
record result is published once. No parallel field-pair payload is invented.

`PartialMemberExpr` retains the expected nominal receiver and member spelling
identity/span. It does not search declarations. The applicable field/variant
fact is emitted only through the expected declaration evidence available in
the same visit.

### Members, calls, and generic applications

A statically resolved module/type member consumes its `04b` `SymbolID` and
equals the member occurrence with that symbol term. A type-directed field
visits the receiver, allocates/publishes the member result, emits
`HasField(receiver,name,result)`, and retains the copied name, exact name span,
member result, and place projection.
A tuple numeric member visits the receiver, allocates and publishes the member
result, emits `HasComponent(receiver,ordinal,result)`, and retains the parsed
index, base, and place projection. Final legality belongs to `06b`.

An immediate instance-method call emits:

```text
SelectMethod(receiver, name, callable, explicitMethodArgs, callSite)
```

It separately relates the explicit `self`, authored arguments, expected
result, and complete callable shape. `Solution.Method(callSite)` supplies
selected method identity and method-local type arguments. A non-immediate
bound member is retained for `06b` rejection and does not synthesize a closure.

For a direct nongeneric call, consume the prepared `Signature`, instantiate
its fixed parameter/result templates in the current rigid environment, visit
arguments in order with parameter expectations, retain each compatibility,
and publish slots for synthetic parameter destinations that are not honest
symbol/syntax roots. Publish the call's authored expression result as syntax;
use a slot only for a distinct synthetic result leaf required by a retained
relationship. For an inferred or explicit generic call:

1. allocate one variable per declared type parameter in declaration order;
2. equate leading explicit type arguments with their variables;
3. emit `Instantiate` for receiver, parameter, and result templates;
4. add receiver, argument, and expected-result evidence;
5. call `PublishInstantiation(site,generic,orderedVariables)` once when the
   application is unconditional, or
   `PublishGuardedInstantiation(choice,alternative,site,generic,orderedVariables)`
   once when it is nested in a deferred-bracket alternative;
6. retain the generic declaration and site for phase 7.

Omitted arguments use the same variables. `_` is unsupported. An explicit
instantiation without a call publishes the instantiated function value. A bare
generic constructor is not a value.

An indirect call allocates one destination term and ordinary slot per authored
argument plus a result term, visits each source argument once with the
corresponding destination expectation, then emits
`Callable(callee,ordered{source,destination},result)` and retains directional
compatibility. It publishes the authored call result and adds a context-flow
record. `Callable` delays until independent evidence supplies the callee's
function structure and never guesses Pebble or C. `06b` reads the solved callee
key for exact convention/variadic policy.

### Neutral brackets, indexing, and slicing

Consume the resolved `04b` bracket mode:

- `BracketTypeNames`: visit each argument as a type occurrence and generate
  explicit generic application only;
- `BracketValueNames`: visit exactly one argument as a value and generate an
  index record only;
- `BracketDeferred`: in one node visit, create exactly two `OneOf`
  alternatives ordered generic then index, add it through `AddChoice`, and tag every branch constraint,
  record, slot, and branch-local generic instantiation with that exact choice
  and alternative `0`/`1`.

The generic branch gates each argument with
`infer.TypeOccurrence(argument, owner, typeArgument)` before its
type-application facts and publications. The runtime branch gates its one
argument with `infer.ValueOccurrence(argument)` before its value traversal
facts, `Integral(index)`,
`Indexable(base,result)`, and alternative-guarded slots for the bracket value,
runtime argument syntax, and branch-only synthetic destinations. It does not
use unconditional `PublishSyntax` for those values. `06b` consumes records and
slots only when `Solution.Selection(choice)` matches their tag. An unselected
branch has no successful syntax result, defaulting root, or branch-local
diagnostic; an ambiguous/failed choice has no typed-IR-capable result.
Generic applications nested in either branch use guarded instantiation
publication, so an inactive, failed, or ambiguous branch likewise contributes
no `Solution.Instantiation`, ordered manifest entry, or semantic-snapshot
manifest entry. Ordinary `PublishInstantiation` is forbidden for such a
branch-local application.

Both gates use only immutable syntax and `04b` evidence. A wrong semantic
category rejects its alternative without publishing a diagnostic; `T0512`
from a limit or inconsistent snapshot aborts the choice and is never evidence
for the other alternative. No spelling, capitalization, following call,
traversal order, or first success selects an interpretation. `OneOf` remains
confined to this genuine generic-application versus runtime-index ambiguity
and contains algebraic constraints only.

Indexing visits base/index, emits `Integral(index)`, allocates a result, and
emits `Indexable(base,result)`. That closed relation implements:

```text
Array(N,T) -> T
Slice(T)   -> T
str        -> char
```

The first two retain place projections; string indexing never does. The
constraint delays when the base is unresolved and never guesses from traversal
order. Pointer/tuple policy is retained for `06b`.

Slicing visits base and present bounds, emits `Integral` for each bound, and
emits `Sliceable(base,result)`: array/slice yields `Slice(T)`, `str` yields
`str`. Omitted bounds create no syntax root. Bounds, escape, lifetime, and
runtime-check policy belong to `06b` and later lowering. Indexing or slicing a
rigid type parameter retains an unsupported-requirement record; no trait is
invented.

### Operators, casts, places, and assignment

Operands and result terms are allocated before constraints are added.

| Family | Pre-solve facts |
| --- | --- |
| literal unary `-` | `NegateLiteral`; no intermediate positive fitting |
| nonliteral unary `-` | `Numeric(operand)`, `Equal(result,operand)` |
| prefix `!` | `Equal(operand,bool)`, result known `bool` |
| prefix `~` | `Integral(operand)`, `Equal(result,operand)` |
| `- * /` | `Numeric(left/right)`, `Equal(left,right)`, `Equal(result,left)` |
| `+` | `Equal(left,right)`, `Equal(result,left)`; add `Numeric(left)` when either operand is a numeric literal, a known numeric builtin, or a rigid type parameter under a generic owner; omit it for known `str` or otherwise unresolved nonliteral operands and retain string/numeric policy for `06b` |
| `%` | `Integral(left/right)`, `Equal(left,right)`, `Equal(result,left)` |
| `<< >>` | `Integral(left/right)`, `Equal(result,left)` |
| `& | ^` | `Integral(left/right)`, `Equal(left,right)`, `Equal(result,left)` |
| `&& ||` | both operands equal `bool`, result known `bool` |
| `< <= > >=` | `Equal(left,right)`, `Ordered(left)`, result known `bool` |
| `== !=` | `Equal(left,right)`, result known `bool`; retain `Equatable` requirement candidate for rigid parameters |

For rows whose result and operands are explicitly equal above, apply the
same-type expected projection rule before solve. A known `i32` destination for
`1 + 2` adds identity evidence from the destination to result/operands and
`LiteralFits` for both exact literals against `i32`; neither literal defaults
to `int`. Unary literal negation first forms the exact signed literal with
`NegateLiteral` and then applies destination fitting, so the minimum signed
value is representable. An expected `?i32` at an optional-injection boundary
projects `i32` into the arithmetic result/operands while retaining the outer
injection compatibility.

The projection does not apply to shift's right operand, Boolean/comparison
results, casts, or any operand not required by the row to share result
identity. Known `str` continues to satisfy `+`. Distinct known numeric operand
types fail the equality facts; no direction, visitation order, or expected
destination inserts an implicit numeric conversion.

The `+` rule must not add unconditional `Numeric` to an unresolved term that
may be `str`; doing so would reject a valid string fact before `06b`. It also
must not leave a rigid numeric body without a requirement. The generator uses
known rigid/builtin evidence and records the exact operator; it does not choose
a numeric winner or use `OneOf` as an overload registry.

Prefix `&` visits a place candidate `T` and emits
`Shape(result,Pointer(T))`. Prefix `*` visits the pointer and emits
`Shape(pointer,Pointer(result))`; it retains a dereference place. Postfix force
unwrap emits `Shape(operand,Optional(payload))` and equals result with payload.
Postfix `++`/`--` retains one place evaluation plus the numeric operator/store
records; no AST desugaring occurs.

A cast resolves destination `D`, visits source `S` without destination
equality, gives the cast expression exact `D`, and retains `(S,D)`. Conversion
legality is entirely `06b`.

A place candidate is retained for resolved bindings/parameters,
dereferences, field/tuple projections, and array/slice indexing. Root
mutability and final category are not inference constraints. Calls, casts,
literals, aggregate temporaries, unwraps, strings, operators, functions, and
variants retain non-place candidates.

Simple assignment visits left once as a place candidate and right once with
left expectation, then retains directional compatibility. Compound assignment
adds the corresponding operator facts, publishes an ordinary slot for the
compound-operation temporary, and retains result-to-place compatibility while
preserving single evaluation of the left syntax. Assignment is
not published as an expression.

## Generic requirements

Generic bodies are visited once with rigid `TypeParameter(SymbolID)` IDs and
`Origin.GenericOwner` set to their declaration. Containing nominal parameters
precede method-local parameters.

`Numeric`, `Integral`, `Ordered`, and exact `LiteralFits` on a rigid parameter
are published by `05b Solution.Requirements`. `06a` retains their operator/use
origins plus checker-owned `Equatable` candidates. Records are allocated in
owner, source, and parameter order; normalization and concrete policy belong
to `06b`/phase 7.

Field access, method lookup, indexing, calling an unconstrained parameter,
conversion, layout, printing, and construction have no accepted generic
requirement kind. `06a` still generates their operands and retains an
unsupported-requirement record; it does not invent a trait or accept the body
from one observed instantiation.

Call-site receiver, explicit arguments, ordinary arguments, and expected
result all contribute before the one solve. Phase 7 receives the final
`Instantiation`, symbolic body types, and normalized requirements through
`06b`; `06a` never specializes or clones syntax.

## Generation diagnostics and recovery

`06a` owns only diagnostics required to form a bounded fact graph:

- `C0614` for the accepted constant language and its limits;
- `C0619` for checker input inconsistency, traversal/record/resource limits,
  duplicate visits/publications, or an impossible internal handoff root;
- `T0501`-`T0512` emitted by `05b` for type resolution, inference facts,
  ambiguity, recovery, and its limits.

Operator legality, conversions, places, calls, records, control flow, defers,
entry points, and typed-IR diagnostics are `06b` even when their record is
obviously invalid before solve. Keeping those diagnostics with their policy
owner prevents duplicated messages and generation-order behavior.

For a binding with neither usable annotation nor initializer,
`Session.Error` is silent and exists only to publish `TypeError` and suppress
dependent inference cascades. Neither 06a nor 05b emits `T0510`; the retained
`bindingRecord` causes exactly one 06b `C0602`.

A nongeneric `FunctionTerm` with captured-symbol evidence produces no
generation diagnostic. Its callable record suppresses successful function
publication, and `06b` emits the sole `C0617` at the retained function span
with captured declaration labels in stable first-reference order.

A generic `FunctionTerm` produces no generation diagnostic: its error term and
`unsupportedCallableRecord` suppress inference cascades, and `06b` emits the
sole stable `C0608` diagnostic at the retained function span.

For a `Missing`/`Error` node, damaged `04b` identity, failed prepared
declaration, or already explained child, `06a` calls `Session.Error(origin)`
or skips the nonvalue container, marks dependent records suppressed, and emits
no duplicate undefined-name/category diagnostic. Constraints containing
`Error` rely on `05b` suppression. An independent subtree is still visited.

When the generation diagnostic limit is reached, the last retained 06a
diagnostic becomes one `C0619`; further generation diagnostics are suppressed.
Earlier-phase and inference diagnostic budgets remain independent.

## Freeze, single solve, and the `06b` handoff

After the last node visit, `06a` performs a read-only freeze audit and, where
stated, a post-solve ownership audit:

1. every required symbol/syntax/instantiation/method root is published once;
2. every record/component count matches its charged arena;
3. every alternative tag names one created `OneOf` constraint/index;
4. every `06b` value handle has exactly one symbol, syntax, instantiation,
   method-selection, or `SlotID` root; a known type is still published through
   its honest syntax/symbol root or an internal slot;
5. no type-only/error occurrence is represented as successful typed syntax;
6. every successful value-producing surface occurrence has exactly one
   expression record with copied literal/interpolation payload and all runtime
   evaluation children rooted;
7. control IDs are contiguous allocation order; every root has parent zero and
   depth one; every nonroot parent is an earlier ID, depth is exactly parent
   depth plus one, and it appears exactly once in that parent's derived
   ascending-ID child list; the edge count is regions minus roots;
8. every control/defer record references an existing region; every nonzero
   target exists in the same function-root tree; exactly one
   `controlFunction` record owns each function root, and every record below it
   carries the same `callableRef`;
9. the tree-free compilation snapshot has one copied module entry per reachable
   module, a complete dependency order, valid import targets/spans, one root
   module, valid source metadata, and complete source-ordered top-level
   declaration identities;
10. record order is deterministic source/allocation order.

If the audit fails, emit `C0619`, use bounded error recovery, and do not call
`Solve` with a fact graph known to have an unreadable required result. If it
succeeds, call `session.Solve()` once. Before discarding the program's
AST-bearing inputs, call the sole `infer.Snapshot(program, solution,
diagnostics)` constructor. That constructor atomically captures the immutable
`types.Snapshot` and tree-free semantic data and binds them to that exact final
solution. Snapshot failure emits bounded `T0512`; 06a returns a failed handoff
shell with `Semantics == nil` and `GenerationHadErrors == true`, and 06b
publishes no IR or duplicate diagnostic for that already explained failure.
On success, discard the session and AST-bearing
generation inputs before constructing the handoff; the checker never retains
a mutable session, store, or program behind it.

The post-solve ownership audit requires the semantic snapshot's type snapshot
to contain every final and retained known `TypeID` and requires
`Semantics.Matches(Solution)` to be true. It also requires
`Semantics.Resolution()` to be the exact immutable result supplied in
`Inputs.Resolution`; resolution is not copied into `frozenCompilation`.
Failure follows the snapshot-failure path above.

The package-private handoff uses a narrow tree-free compilation snapshot:

```go
type frozenSource struct {
    ID     source.ID
    Path   string
    Length uint32
}

type frozenImport struct {
    Span   source.Span
    Target module.ModuleID
}

type frozenModule struct {
    ID           module.ModuleID
    Key          module.ModuleKey
    Source       source.ID
    Span         source.Span
    Imports      []frozenImport
    Declarations []symbol.SymbolID
}

type frozenCompilation struct {
    Root            module.ModuleID
    Modules         []frozenModule
    DependencyOrder []module.ModuleID
    Sources         []frozenSource
}

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

`Modules` is copied in ascending `ModuleID`; `DependencyOrder` is the graph's
complete dependencies-before-importers order; imports and declarations retain
authored order. `Span` is the copied `File` span. `Declarations` lists every
top-level declaration symbol in source order; extern wrapper/block syntax is
folded into its contained declarations. Nested fields, variants, parameters,
type parameters, and methods come from `Semantics` declaration/signature
descriptors and immutable resolution symbols. These sources are sufficient for
module, import, declaration, and nonvalue-container typed IR without a tree
query.

`Sources` contains only copied source identity, display path, and byte length,
ordered by `source.ID`; it exposes no bytes or syntax. It validates retained
spans and stable `SourceID:start:end` dump locations. Resolution and
semantic-type queries are available only through `Semantics.Resolution()` and
`Semantics.Types()`. String and slice fields are copied; every module, source,
import, declaration, and dependency entry is charged to
`MaxRecordComponents` before allocation, in addition to its upstream module/
source bounds. `frozenCompilation` contains no
`module.Graph`, `module.Module`, `syntax.Tree`, syntax node, source text, or
callback/lookup recipe capable of reconstructing syntax.

All handoff fields point to or own immutable, tree-free data. `Semantics`
owns the exact immutable resolution, copied templates, declarations,
signatures, runtime identities, owner/type-parameter order, and a copied
`types.Snapshot`; it retains neither the originating `Program` nor
`types.Store`. `Solution` matches it through opaque
program/solve tokens with no backpointers.
`frozenRecords`, `frozenRoots`, and `frozenConstants` expose package-private
copied accessors; they contain no `infer.Term`, `InferID`, AST pointer, mutable
slice, map-order output, backend name, conversion decision/result, or typed-IR
node.
Runtime identities are read from `Semantics.RuntimeTypes()`; no duplicate
handoff field exists. The mutable 06a traversal inputs, graph, module values,
trees, evaluator, program, session, and mutable store access are discarded
before `run06b`.

The 06a-owned frozen-root API is exactly:

```go
type rootedValue struct { Value valueID; Root valueRoot }
type frozenRoots struct { /* private ordered []rootedValue */ }

func (f frozenRoots) All() []rootedValue
func (f frozenRoots) Root(valueID) (valueRoot, bool)
```

`All` returns a copy in root-allocation order. `Root` is the sole keyed query
and returns a copied `valueRoot`; no syntax/symbol reverse index exists or is
needed. Storage is bounded by `MaxSyntaxVisits`, every `valueID` occurs at most
once, and the freeze audit proves that each required value occurs exactly once.

The sole frozen control-hierarchy accessor is:

```go
func (f frozenRecords) Controls() []controlRegion
```

It returns copied regions in ascending `controlID`, including copied child
slices. Region count is bounded by `MaxSemanticRecords`, depth by
`MaxControlDepth`, and children are derived once at freeze from `Parent`; no
record constructor or 06b consumer may supply or mutate a second hierarchy.

`frozenRoots` stores only `rootSyntax`, `rootSymbol`, `rootInstantiation`,
`rootMethod`, or `rootSlot`. A slot root carries only its
`infer.SlotID` and alternative tag. For an ordinary or selected guarded root,
`06b` queries `Solution.Slot`; an inactive guarded root is filtered before
lookup. `valueRoot.Parameter` is used only by `rootInstantiation` and
`rootMethod`, is zero-based, and must be less than the length of the matching
`Solution.Instantiation(site).Arguments` or
`Solution.Method(site).Arguments` list before access. No successful handoff
contains `infer.Term`, `InferID`, `ChoiceRef`, or a derivation recipe. A
checker-owned known type that lacks an honest symbol or syntax publication
uses an ordinary slot; it does not add another root kind.
An instantiation root belonging to a deferred alternative carries the same
alternative tag as its record and is filtered before lookup. This agrees with
the guarded 05b solution manifest: inactive, failed, and ambiguous choices
cannot make an instantiation argument root readable.

If `Solution.Successful()` is false, the unified checker may ask `06b` only to
perform diagnostics explicitly independent of the failed type. These include
an empty binding's form-only `C0602`, retained `C0608` and `C0617` callable
records, and policy checks whose type-dependent inputs are `TypeFinal`; it
cannot produce typed IR or repair inference. On a successful solution, `06b`
resolves roots, applies policy, normalizes requirements, validates control flow
and entry, and builds typed IR without any session builder operation.

## Upstream status and deferred features

Most upstream contracts required to implement `06a` and supply `06b` are
resolved. The remaining implementation dependencies must be completed in
their owning slices rather than treated as checker-local policy:

- `03b` defines `EndOfFile` and `RecordField` with their real child/span/
  recovery contracts, and the dispatch above covers every `syntax.NodeKind`;
- `04b` defines the concrete immutable reference, qualifier, bracket, capture,
  member, builtin, and runtime query API;
- `04b` capture queries plus the callable record define stable `C0617`
  evidence; valid nongeneric literals use their `FunctionTerm` `SyntaxRef` as
  the source identity for global hoisting and require no anonymous symbol;
- `05b` supplies bounded ordinary and alternative-guarded `SlotID` roots,
  closed delayed `Callable`, `Indexable`, and `Sliceable` constraints, and the
  alternative-safe `TypeOccurrence`/`ValueOccurrence` gates required before
  slice 06a.5 can generate deferred brackets;
- deferred brackets tag facts, records, and guarded roots with one exact
  choice/alternative, and use guarded instantiation publication for nested
  generic applications, so inactive branches cannot default, diagnose, or
  publish specialization arguments;
- the record/root contracts above retain every identity, expression child,
  literal/interpolation payload, control mode, and policy input 06b needs
  without syntax reinterpretation;
- `Program.RuntimeTypes()` supplies `ContextExpr`, while hidden context remains
  checker-owned propagation metadata outside authored function keys.

The implementation prerequisite for slice 06a.8 is the completed 05a
type-snapshot extension followed by the 05b.8 semantic-snapshot continuation.
Slices 06a.2 through 06a.7 may proceed earlier wherever their existing
dependencies permit; they do not wait on snapshots they neither construct nor
consume.

Generic anonymous functions remain explicitly deferred. Their syntax is
preserved, but initial checking rejects them through the error-term/
`unsupportedCallableRecord` path and 06b diagnostic `C0608`; no owner symbol,
generic inference, or lowering is specified. Their future design may preserve
the current noncapturing, globally hoisted model and has no dependency on
closure support. Public generic callable/indexable/sliceable traits likewise
remain future language features;
the closed structural constraints do not create them.

## Deferred to `06b` or later phases

`06a` deliberately does not contain the conversion matrices, post-solve
operator/category validation, final place writability, call/record/variant
legality, calling-convention compatibility, switch exhaustiveness,
reachability, definite return, defer exit expansion, entry-point validation,
coercion insertion, runtime checks, typed-IR construction, layout,
specialization, lowering, or backend ABI.

Language decisions already marked future by phase 6 remain future: dynamic or
zero-initialized globals, uninitialized locals, full slice lifetime/escape,
unsafe pointers, untagged-union safety, closure support, bound-method values,
C variadic promotions/adapters, public traits, and runtime overflow/ABI
behavior. Enum/integer conversion is no longer on this list: it is decided,
in both directions, per `06b`'s composite conversion matrix. Closure support
is an explicit unresolved language decision, not an assumed roadmap item;
the current anonymous-function
contract is noncapturing and globally hoisted. `06a` records the authored
relationships needed by accepted rules but does not add speculative machinery
for them.

## Tests and fixtures

Source-driven tests live under:

```text
tests/check/facts/
  valid/*.peb
  valid/multimodule/<case>/main.peb
  invalid/C0608/*.peb
  invalid/C0614/*.peb
  invalid/C0617/*.peb
  invalid/C0619/*.peb
  invalid/T0501/*.peb ... invalid/T0512/*.peb
  recovery/*.peb
```

Fixtures assert fact-generation success/failure and stable diagnostic codes;
they do not assert conversion or typed-IR policy owned by `06b`. Optional
sidecars are used only for provenance labels. Direct tests assert terms,
constraints, publications, records, alternative tags, counts, traversal
order, constant values, runtime context facts, and frozen handoff shape.

Required fact fixtures/direct cases include contextually inferred `i32`
arithmetic literals, minimum signed literal through unary negation, optional
injection of a contextually inferred arithmetic result, string addition,
mixed concrete numeric rejection, and reversed operand/equation/traversal
construction. They also cover all three delayed structural constraints,
ordinary/guarded slots, inactive deferred-bracket branches, explicit generic-
anonymous-function rejection records, nongeneric module/global references,
prohibited local/parameter/loop captures in stable first-reference order, and
every `EndOfFile`/`RecordField` recovery shape. Expression-record coverage
proves every successful value occurrence has one ordered child/payload record,
including numeric/decoded literals, interpolation parts, empty aggregates,
specialized-record joins, copied member names, and copied backing storage.
`06b` fixtures own the rendered
`C0608`/`C0617` diagnostics and final operator/call/index legality.

Every implementation slice runs its targeted test plus the common verification
suite from `compiler/`:

```sh
go test ./...
go test -race ./...
go vet ./...
```

Every slice also runs `git diff --check` from the repository root. Tests and
build caches must remain outside the repository when the toolchain permits
configuration.

## Dependency-ordered implementation slices

### Slice 06a.1: lifecycle, arenas, roots, and limits

**Owned files:**

```text
compiler/internal/check/config.go
compiler/internal/check/generation.go
compiler/internal/check/record.go
compiler/internal/check/root.go
compiler/internal/check/generation_test.go
```

**Input/output:** define `Inputs`, the 06a-owned `Config` fields, monotonic
arenas, value/root/record/control IDs, snapshot checks that do not require a
tree walk, and the mutable-to-frozen lifecycle skeleton. Output is an empty
bounded fact arena suitable for later slices; no public 06a result.

**Complete when:** zero/foreign handles, over-limit appends, double freeze,
mutation after freeze, nil inputs, and copied expression/literal/interpolation/
component ownership are deterministic and nonpanicking.

**Tests/fixtures:** direct lifecycle, copying, and invalid-handle tests;
source fixtures begin with the traversal slice.

**Limits/recovery:** lower every arena/component/diagnostic limit and prove
atomic failure without a partial append.

**Verification:** `go test ./internal/check -run 'TestGeneration|TestRecord|TestRoot|TestConfig'`, then the common suite and `git diff --check`.

### Slice 06a.2: bounded constant evaluator

**Owned files:**

```text
compiler/internal/check/constant.go
compiler/internal/check/constant_test.go
compiler/internal/check/constant_repository_test.go
tests/check/facts/valid/constant_*.peb
tests/check/facts/invalid/C0614/constant_*.peb
tests/check/facts/recovery/constant_*.peb
```

**Input/output:** consume immutable graph/sources/resolution and emit memoized
constant values plus the `infer.ArrayLengthEvaluator` adapter. No `Program` or
session dependency.

**Complete when:** every accepted/rejected form, imported/forward reference,
cycle, arithmetic rule, repeated callback, and `uint64` boundary is specified
by source and direct tests.

**Tests/fixtures:** direct evaluator/memoization assertions and the owned
`constant_*` valid, invalid, and recovery sources.

**Limits/recovery:** lower depth, operations, bits, and diagnostics; prove one
diagnostic per cycle/occurrence and independent recovery.

**Verification:** `go test ./internal/check -run 'TestConstant|TestArrayLength'`, then the common suite and `git diff --check`.

### Slice 06a.3: preparation, traversal, declarations, and runtime context

**Owned files:**

```text
compiler/internal/check/walk.go
compiler/internal/check/declaration_facts.go
compiler/internal/check/type_occurrence.go
compiler/internal/check/context_facts.go
compiler/internal/check/declaration_facts_test.go
compiler/internal/check/walk_test.go
tests/check/facts/valid/declaration_*.peb
tests/check/facts/valid/context_*.peb
tests/check/facts/valid/anonymous_function_*.peb
tests/check/facts/invalid/C0608/generic_anonymous_*.peb
tests/check/facts/invalid/C0617/anonymous_capture_*.peb
tests/check/facts/invalid/C0619/declaration_*.peb
tests/check/facts/recovery/declaration_*.peb
```

**Input/output:** construct `Program`/session, walk every `03b` kind once,
consume prepared declaration types, resolve body-owned types, publish symbols,
and emit function/method/extern/runtime-context facts. Output is a mutable
ordered arena; no solve yet.

**Complete when:** closed-node dispatch includes `EndOfFile` and `RecordField`,
module/source order, grouped parameters, named rigid generic owners, exact
runtime `Context`, hidden-context records, and the generic-anonymous unsupported
path have direct assertions using only the formal 04b queries. Nongeneric
function literals accept module/global references, retain prohibited-capture
evidence without an environment, suppress successful values when captures are
present, and use their `SyntaxRef` as the source identity for global hoisting.
`Context` never appears as a capture, and no anonymous owner identity is added.

**Tests/fixtures:** direct dispatch/publication/context/capture assertions and
the owned `declaration_*`, `context_*`, and `anonymous_*` source cases,
including `C0608` record ownership and `C0617` label evidence.

**Limits/recovery:** lower visits, traversal/control depth, publications, and
diagnostics; combine damaged declarations with a later valid declaration.

**Verification:** `go test ./internal/check -run 'TestWalk|TestDeclarationFacts|TestContextFacts'`, then the common suite and `git diff --check`.

### Slice 06a.4: expected evidence and primary/aggregate expressions

**Owned files:**

```text
compiler/internal/check/expected.go
compiler/internal/check/expression_facts.go
compiler/internal/check/aggregate_facts.go
compiler/internal/check/expected_test.go
compiler/internal/check/expression_facts_test.go
tests/check/facts/valid/evidence_*.peb
tests/check/facts/valid/aggregate_*.peb
tests/check/facts/invalid/T0505/evidence_*.peb
tests/check/facts/invalid/T0510/aggregate_*.peb
tests/check/facts/recovery/expression_*.peb
```

**Input/output:** consume the visitor/session/program and emit primary,
literal, optional/pointer, tuple, array, record, interpolation, and `sizeof`
facts plus publications/compatibility records.

**Complete when:** identity/literal/shape evidence is exhaustive, each child is
visited once, record-field destinations have ordinary slot roots, empty/
contextual aggregates solve only with valid evidence, contextual same-type
operators receive pre-solve evidence, and ordinary conversions never become
equality. Every successful primary/aggregate expression also freezes its exact
kind, ordered runtime children, literal/interpolation payload, and specialized
record join for 06b without an AST reread.

**Tests/fixtures:** direct constraint/record assertions and the owned
`evidence_*`, `aggregate_*`, and `expression_*` sources.

**Limits/recovery:** lower records/components/places/diagnostics and combine an
error term with independent valid expressions.

**Verification:** `go test ./internal/check -run 'TestExpected|TestExpressionFacts|TestAggregateFacts'`, then the common suite and `git diff --check`.

### Slice 06a.5: calls, members, generics, and neutral brackets

**Owned files:**

```text
compiler/internal/check/call_facts.go
compiler/internal/check/member_facts.go
compiler/internal/check/generic_facts.go
compiler/internal/check/bracket_facts.go
compiler/internal/check/call_facts_test.go
compiler/internal/check/generic_facts_test.go
tests/check/facts/valid/call_*.peb
tests/check/facts/valid/generic_*.peb
tests/check/facts/valid/bracket_*.peb
tests/check/facts/invalid/T0509/bracket_*.peb
tests/check/facts/recovery/call_*.peb
```

**Input/output:** emit direct/indirect/method calls, `HasField`,
`SelectMethod`, template instantiations, explicit/inferred type arguments,
requirements, context flow, publications, and exactly two deferred-bracket
alternatives.

**Complete when:** receiver/argument/result evidence is order-independent,
generic declaration bodies are generated once, explicit instantiation is a
value, method identity comes only from `Solution.Method`, indirect calls use
`Callable`, synthetic destinations use slots, and deferred runtime values use
guarded slots whose inactive alternatives cannot default or diagnose.

**Tests/fixtures:** direct call/member/generic/choice assertions and the owned
`call_*`, `generic_*`, and `bracket_*` sources.

**Limits/recovery:** lower inference variables, constraints, choices, method
arguments, solved slots, records, requirements, and diagnostics; prove failed
or inactive alternatives do not leak records, slots, defaults, diagnostics, or
types.

**Verification:** `go test ./internal/check -run 'TestCallFacts|TestMemberFacts|TestGenericFacts|TestBracketFacts'`, then the common suite and `git diff --check`.

### Slice 06a.6: operators, places, assignment, indexing, and slicing

**Owned files:**

```text
compiler/internal/check/operator_facts.go
compiler/internal/check/place_facts.go
compiler/internal/check/assignment_facts.go
compiler/internal/check/index_facts.go
compiler/internal/check/operator_facts_test.go
compiler/internal/check/place_facts_test.go
tests/check/facts/valid/operator_*.peb
tests/check/facts/valid/place_*.peb
tests/check/facts/invalid/T0507/operator_*.peb
tests/check/facts/recovery/place_*.peb
```

**Input/output:** emit the exact operator table, pointer/optional shapes,
place projections, simple/compound assignment records, integral indices and
bounds, and result publications. Output remains policy-neutral records for
`06b`.

**Complete when:** operand order cannot choose a type, contextual `i32` and
optional-injected arithmetic solve before defaulting, string `+` is not
rejected by unconditional numeric capability, mixed concrete numeric types
fail, literal negation preserves the minimum signed edge, compound left sides
are visited once with slot-rooted temporaries, every place projection is
retained, and unknown receiver families use `Indexable`/`Sliceable`.

**Tests/fixtures:** direct operator/place/single-evaluation assertions and the
owned `operator_*`/`place_*` valid, invalid, and recovery sources.

**Limits/recovery:** lower places, records/components, constraints, shapes,
requeues/decomposition, solved slots, and diagnostics; exercise deep
projection, delayed callable/index/slice recovery, and erroneous-base
suppression.

**Verification:** `go test ./internal/check -run 'TestOperatorFacts|TestPlaceFacts|TestAssignmentFacts|TestIndexFacts'`, then the common suite and `git diff --check`.

### Slice 06a.7: statements, control regions, switches, and defer

**Owned files:**

```text
compiler/internal/check/statement_facts.go
compiler/internal/check/control_facts.go
compiler/internal/check/defer_facts.go
compiler/internal/check/control_facts_test.go
compiler/internal/check/defer_facts_test.go
tests/check/facts/valid/control_*.peb
tests/check/facts/valid/defer_*.peb
tests/check/facts/invalid/C0614/switch_*.peb
tests/check/facts/recovery/control_*.peb
```

**Input/output:** visit every statement once and emit condition, return, range,
switch constant/variant, candidate target, lexical region, and defer records.
No reachability or exit validation.

**Complete when:** nested region/target/defer identities are deterministic,
deferred statements are not revisited, omitted `for` conditions create no
syntax node, condition/else/range modes are explicit, and all contained
expressions retain their facts.

**Tests/fixtures:** direct region/target/defer assertions and the owned
`control_*`, `defer_*`, and `switch_*` sources.

**Limits/recovery:** lower control depth, visits, records/components,
constant work, and diagnostics; recover from an invalid switch constant and
continue into a later statement.

**Verification:** `go test ./internal/check -run 'TestStatementFacts|TestControlFacts|TestDeferFacts|TestSwitchFacts'`, then the common suite and `git diff --check`.

### Slice 06a.8: freeze audit, one solve, determinism, and handoff

**Owned files:**

```text
compiler/internal/check/solve_handoff.go
compiler/internal/check/solve_handoff_test.go
compiler/internal/check/determinism_test.go
compiler/internal/check/recovery_test.go
compiler/internal/check/facts_repository_test.go
tests/check/facts/valid/multimodule/*
tests/check/facts/recovery/handoff_*.peb
```

**Input/output:** consume the complete mutable fact arena, audit roots and
alternatives, freeze copied records/constants, call `Solve` once, discard the
session and AST-bearing traversal inputs, create the bounded immutable
`SemanticSnapshot`/`types.Snapshot`, copy the tree-free compilation metadata,
and return `solveHandoff` to the in-package `06b` consumer.

**Prerequisite:** consume the completed 05b.8 `infer.Snapshot` API, which in
turn depends on the 05a type-snapshot extension. This slice must not implement
either missing upstream snapshot locally.

**Complete when:** double solve/post-solve mutation is impossible by structure,
every 06b-required value is rooted by `SymbolID`, `SyntaxRef`, instantiation,
method selection, or `SlotID`; inactive guarded slots are absent; failed
inference or snapshot failure cannot enable typed IR; the semantic snapshot
matches exactly one final solution; repeated runs/forced map seeds produce
identical normalized facts and diagnostics; and no graph, module value, source
bytes/file set, tree, AST node, program, mutable store, term, `InferID`,
`ChoiceRef`, or session escapes. Every successful expression record
and its copied payload/children passes the freeze audit.

**Tests/fixtures:** direct freeze/solve/determinism assertions, the repository
fact runner, owned multimodule cases, and `handoff_*` recovery sources.

**Limits/recovery:** combine every checker and inference limit with independent
later roots; explicitly lower solved-slot, choice, requeue, and decomposition
limits and prove bounded partial solutions and deterministic suppression.

**Verification:** `go test ./internal/check -run 'TestSolveHandoff|TestFactDeterminism|TestFactRecovery|TestFactFixtures'`, then the common suite and `git diff --check`.

The dependency order remains `06a.1 -> 06a.2 -> 06a.3 -> 06a.4 -> 06a.5 ->
06a.6 -> 06a.7 -> 06a.8`. `05b.6` solved-slot publication must precede
`06a.4`; `05b.5` delayed `Callable`/`Indexable`/`Sliceable` support must
precede `06a.5` and `06a.6`. Accepted `06a` is the input to `06b`. Each slice
handoff reports exact files, facts/publications added, targeted and source
tests, lowered limits, command results, and any unresolved upstream contract.
No implementation slice may edit phases 03, 04b, 05a, or 05b to make its task
pass.

Across documents, the handoff-critical chain is `05a type snapshot -> 05b.8
semantic snapshot -> 06a.8 handoff -> 06b.1 validation`. This does not add a
dependency from the snapshot work to 06a.2-06a.7 beyond the dependencies
already listed for those slices.
