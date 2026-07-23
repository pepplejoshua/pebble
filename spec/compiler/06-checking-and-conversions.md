# 06 Checking and Conversions

Phase 6 is the language-policy layer between algebraic inference and typed IR.
It performs the only semantic traversal of function bodies and value syntax.
During that traversal it generates every fact that can affect inference and
retains every relationship that requires language-policy validation. It then
invokes `05b` `Solve` exactly once for the checked unit, validates the retained
relationships against the immutable solution, and emits typed IR with every
non-identity conversion explicit.

The lifecycle is closed:

```text
05b Prepare
  -> 06 Generate (one deterministic semantic AST traversal)
  -> 05b Solve (exactly once)
  -> 06 Validate and build typed IR (no new inference facts)
```

Phase 6 must not mutate the surface tree, decorate syntax nodes, rebuild a
scope, perform textual name lookup, reopen source files, clone a generic AST,
generate a backend name, compute layout, specialize a generic, add an equation
after solving, call `Solve` again, or modify a `05b` solution. If validation
discovers that a missing equation would be needed to choose a type, that is a
checker implementation defect; it is never repaired by a second solve.

[05a Semantic Type Store](05a-semantic-type-store.md),
[05b Algebraic Inference](05b-algebraic-inference.md), and
[04b Name Resolution](04b-name-resolution.md) are authoritative for identity,
inference, type syntax, declarations, and resolution. This document does not
restate their algorithms.

## Public contract and ownership

The implementation lives in `compiler/internal/check`. The compilation driver
constructs the `05b` program before invoking the checker:

```go
type EntryMode uint8

const (
    EntryNone EntryMode = iota + 1
    EntryRequired
)

type EntryPoint struct {
    Mode   EntryMode
    Symbol symbol.SymbolID // required exactly for EntryRequired
}

type Config struct {
    MaxSemanticRecords       uint32
    MaxControlDepth          uint32
    MaxConstantDepth         uint32
    MaxConstantOperations    uint64
    MaxConstantBits          uint32
    MaxTrackedPlaces         uint32
    MaxGenericRequirements   uint32
    MaxDiagnostics           uint32
    Entry                    EntryPoint
}

type Inputs struct {
    Graph      *module.Graph
    Sources    *source.FileSet
    Resolution *symbol.Result
    Types      *types.Store
    Program    *infer.Program
    Constants  *ConstantEvaluator
}

func Check(
    inputs Inputs,
    session *infer.Session,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) *Result
```

`Graph`, `Sources`, `Resolution`, `Types`, `Program`, `Constants`, and
`session` must belong to one compilation snapshot. `Program` must have been
prepared from the graph, sources, resolution, store, and the `Constants`
array-length adapter. `session` must be new, unsolved, and owned by that
program. Nil, inconsistent, foreign, or already solved input produces one
bounded `C0619` and an immutable failed result rather than a panic.

The driver supplies an entry `SymbolID`; phase 6 never searches for the text
`main`. `EntryNone` is used by libraries, check-only fragments, and explicitly
freestanding units. `EntryRequired` validates the supplied symbol after solve.

Zero-valued limits select these defaults:

| Limit | Default |
| --- | ---: |
| semantic records | 4,194,304 |
| nested control constructs | 1,024 |
| constant-expression depth | 256 |
| constant-expression operations | 1,048,576 |
| constant integer magnitude bits | 1,048,576 |
| tracked places | 1,048,576 |
| inferred generic requirements | 1,048,576 |
| phase-6 diagnostics | 100 |

Tests may lower every limit. A limit is checked before append or allocation;
failure is atomic, emits one `C0619` for the smallest affected construct, and
allows independent declarations to continue where safe.

### Immutable result

The logical result is:

```go
type Result struct { /* immutable compilation-owned tables */ }

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

`ExpressionResult` contains the final `TypeResult`, value category, and source
span. `PlaceResult` contains place kind, root storage symbol when one exists,
writability, and any retained projection path. `ConversionResult` contains
source type, destination type, conversion class, typed-IR coercion kind, and
origin role. `CallResult` contains direct/indirect/method/variant kind, selected
symbol when present, calling convention, ordered parameters, variadic state,
and ordered argument conversions. `MemberResult` contains field, method,
variant, or tuple-element identity. `ControlResult` contains entry reachability,
exit behavior, enclosing loop/switch targets, and active defers.

All slice/table accessors return copies ordered by `SymbolID` or
`(ModuleID, NodeID)` as appropriate. A failed result remains queryable, but
`IR()` is non-nil only when both `Solution.Successful()` and phase-6 validation
succeed. No recovery `TypeID`, `InferID`, session term, AST pointer, generated
name, or mutable backing storage escapes in a successful result.

Phase 6 owns all checker records and typed IR. `05b` owns its program, session,
and solution. The surface trees and resolution result remain immutable and
owned by earlier phases.

## Deterministic orchestration and publication

Generation visits modules in `Graph.DependencyOrder()`, declarations in file
child order, and every child/list in authored order. It allocates symbol terms
before the first body use that can reference them, following `05b` declaration
rules, and otherwise allocates terms and records in visitation order. Map
iteration, source spelling, host pointer order, and `TypeID` numeric order may
not affect generation or output.

For each checked unit, `Check` performs these steps exactly once:

1. Validate snapshot ownership and freeze the set of reachable syntax trees.
2. Preallocate terms for every `04b` symbol whose type is published by phase 6.
3. Traverse declarations, statements, and expressions once. Create all terms,
   equations, capabilities, shapes, exact literal evidence, instantiations,
   publication roots, and bounded `OneOf` alternatives during this traversal.
4. Freeze checker records. No generation API is used after this point.
5. Call `session.Solve()` once.
6. Read only `Solution` and `Program`; validate all records in deterministic
   source order and create explicit coercion nodes.
7. Validate control flow and the configured entry point, freeze result tables,
   and publish typed IR only on complete semantic success.

Phase 6 publishes through `PublishSymbol` every binding, parameter, function,
field, variant payload, method, and range iterator with a valid `SymbolID`. It
publishes through `PublishSyntax` every value-producing expression, including
`void` expressions. Qualifiers, type-only occurrences, `Missing`, `Error`, and
upstream error identities are not successful value publications. Each generic
application calls `PublishInstantiation` once at its bracket or inferred-call
site. Duplicate or missing publication is `C0619`.

Each syntax occurrence is generated at most once. Passing expected evidence
does not revisit an expression and does not replace its synthesized term.
After solve, validation may inspect a record only; it may not call `Variable`,
`Known`, literal constructors, `ResolveType`, `Add`, any `Publish` operation,
or `Solve`.

## Expected-type evidence

An expected type is a term available while an expression is generated. The
syntax rule must translate it into exactly the applicable evidence below and
retain the destination relationship for post-solve compatibility.

| Evidence | Use | Generated fact |
| --- | --- | --- |
| identity | language identity is required, such as a Boolean condition, operator peers, annotated symbol, or explicit generic argument | `Equal(actual, expected)` |
| literal | the expression is an integer or float literal selected by a destination | `LiteralFits(literal, expected)` |
| shape | the syntax is contextual: `nil`, `none`, `some`, empty/aggregate literal, tuple literal, record without an explicit base, or function literal | the corresponding `Shape`, with expected component leaves related in authored order |

Shape evidence propagates only through authored structure. For example, an
expected `?i32` applied to `some 1` shapes the result as optional and gives
`i32` as literal evidence to `1`; an expected `[0]T` supplies the element of
`[]`; and an expected nominal record supplies its field descriptors. A shape
constructor mismatch is an inference conflict, not a conversion.

Evidence follows an already known implicit-conversion destination without
turning that conversion into equality. In particular, a direct literal
assigned to known `?D` receives literal evidence `D`, and the retained
relationship later emits `OptionalInject`; it is never tested as a literal
that must fit the optional type itself. Expected tuple components similarly
flow to authored tuple elements, while the tuple-to-tuple conversion remains a
retained post-solve relationship. No evidence projection is invented through
an explicit-only or forbidden conversion.

An ordinary name, call, cast, operator result, field, index, or other
synthesized expression is never equated with its destination merely because a
conversion might later be legal. Assignment, argument, return, tuple element,
record field, and any future branch-result context retain `(source,
destination)` and use the compatibility rules after solve.

## Checker record model

Generation retains closed records, each with a stable kind, owning
`SyntaxRef`, exact span, ordered related origins, and session terms:

- `AssignmentRecord`: destination place, source, operator, and destination
  type term;
- `CallRecord`: callee, receiver when present, instantiated parameter/result
  terms, arguments, convention, and generic site;
- `CastRecord`: source and resolved target;
- `OperatorRecord`: spelling, family, operands, result, and generic owner;
- `IndexRecord`: base, index/bounds, result, and index/slice mode;
- `PlaceRecord`: identifier/dereference/field/index projection and writability;
- `MemberRecord`: static or type-directed member, category, owner, and result;
- `RecordRecord`: nominal receiver and ordered field initializers;
- `ControlRecord`: reachability, exits, loop/switch target, narrowing, and
  active defers;
- `CompatibilityRecord`: assignment, argument, return, field, tuple element,
  optional payload, or future branch-result role.

Records tagged with a `OneOf` alternative are ignored unless
`Solution.Selection` selects that alternative. No record contains a callback
into the checker or solver.

### Exhaustive surface dispatch

The generation dispatcher is closed over the authoritative `03b` kinds. Each
kind below is dispatched exactly once to the named rule group; adding a syntax
kind requires extending this table before implementation accepts it.

| Surface kinds | Phase-6 rule group |
| --- | --- |
| `File`, `EndOfFile`, `ImportDecl` | orchestration/import |
| `BindingDecl`, `ExternBinding` | bindings |
| `TypeDecl`, `ExternType`, `FieldDecl`, `VariantDecl`, `StructType`, `UnionType`, `EnumType` | type declarations and prepared members |
| `FunctionDecl`, `ExternFunction`, `FunctionTerm`, `Parameter`, `TypeParameter`, `ExternDecl`, `ExternBlock` | functions, methods, externs, and literals |
| `BlockStmt`, `ReturnStmt`, `IfStmt`, `WhileStmt`, `RangeLoopStmt`, `ForStmt`, `SwitchStmt`, `SwitchCase`, `DeferStmt`, `PrintStmt`, `BreakStmt`, `ContinueStmt`, `AssignmentStmt`, `ExpressionStmt` | statement and control-flow rules |
| `Name`, `Path`, `Literal`, `InterpolatedString`, `ContextExpr`, `SomeExpr`, `SizeofExpr`, `GroupedTerm`, `TupleTerm` | primary expressions |
| `ArrayExpr`, `ArrayRepeatExpr`, `RecordExpr`, `RecordField`, `PartialMemberExpr` | aggregate/contextual expressions |
| `PrefixTerm`, `PostfixExpr`, `BinaryExpr`, `CastExpr` | operator/cast rules |
| `CallExpr`, `BracketApply`, `SliceExpr`, `MemberExpr` | call/member/bracket/indexing rules |
| `OptionalType`, `SliceType`, `ArrayType` and body-owned type uses of shared `Name`, `Path`, `PrefixTerm`, `GroupedTerm`, `TupleTerm`, `FunctionTerm`, or `BracketApply` | one `Session.ResolveType` query in the current generic owner environment; identity is owned by `05b` |
| `Missing`, `Error` | upstream-error propagation only |

`ImportDecl`, `ExternDecl`, and `ExternBlock` are containers/nonvalues and do
not publish expression roots. Type-only dispatch does not duplicate `05b`
resolution: phase 6 identifies the known type position during its one walk and
submits its `SyntaxRef` and owner once.

## Declaration rules

### Imports and type declarations

Imports add no type facts. Their qualifiers are never values.

A ready concrete type declaration publishes the `Program` type result. A
generic nominal or alias remains a `SymbolID`-identified constructor and has
no standalone `TypeID` to publish. Phase 6 checks
each field, variant, and method exactly once but does not re-resolve its type
syntax, equate a nominal with its fields, or alter declaration metadata.

- A struct field symbol equals its prepared template/type. Duplicate names
  have already been rejected by `04b`.
- A tagged-union variant payload symbol equals its prepared type. `void`
  denotes a payload-free variant.
- An enum variant is a value of the containing enum nominal type and has no
  payload.
- An untagged-union member has its declared payload type, but value
  construction, read, and write are rejected with `C0615` until a safety model
  is specified. Declaring and passing an opaque value of the exact union type
  remains legal.
- A transparent alias publishes the resolved target identity and receives no
  nominal conversion behavior.
- An extern type is opaque nominal. It has identity, pointer, parameter, and
  return uses, but no field, construction, comparison, or layout-dependent
  operation in phase 6.

Struct construction requires each declared field exactly once and rejects
unknown or repeated fields. There are no default field values. Fields are
validated in authored initializer order; missing fields are diagnosed in
declaration order. Tagged-union and enum values use variant member syntax, not
record syntax. Anonymous record syntax is legal only with unique expected
nominal struct shape; Pebble still has no anonymous record type.

### Bindings

Every non-extern `let` or `var` requires an initializer in this phase-6
contract. An authored annotation is optional. An annotation-only or entirely
empty binding emits `C0602`; implicit zero initialization and uninitialized
locals are future features.

For an annotated binding, symbol term `S` equals annotation `A`; the initializer
is generated with `A` as expectation and `(initializer,A)` is retained. For an
unannotated binding, `S` equals initializer term `I`. `let` creates a
non-writable storage place after initialization; `var` creates a writable one.
The initializer is never allowed to name the local being declared because
`04b` already resolves that occurrence to an outer declaration or error.

Global initializers obey the same typing rule. They must also be accepted
phase-6 constant expressions; dynamic global initialization order is
unresolved and rejected with `C0616`. Extern `let`/`var` instead require an
annotation, forbid an initializer by grammar, and publish that exact type.

### Functions, methods, externs, and literals

Each parameter symbol equals its prepared annotation. The function symbol is
shaped from its prepared convention, ordered expanded parameters, result, and
variadic bit. Grouped parameters such as `a, b T` become two semantic
parameters in authored name order.

An ordinary function or method body is generated once. An expression body is
equivalent for checking to one return relationship; it is not rewritten into a
surface `ReturnStmt`. Extern functions have no body. `inline` affects no phase-6
type rule and is retained as an optimization request.

The optional function modifier string must map exactly to the already prepared
`Pebble` or `C` convention. A body is permitted only for `Pebble` convention.
An `extern fn` must use `C` convention; a variadic function must be extern,
must use `C`, must place its single variadic parameter group last, and the
variadic group contributes no fixed parameter after it. No Pebble-defined
variadic function is accepted.

A method is an authored function nested directly in a nominal declaration. It
must have a first parameter named `self`; that parameter type must be exactly
the containing nominal application or `*` that application. A method may add
its own type parameters after the containing type parameters. Instance-member
syntax binds this explicit first parameter. Extracting a bound method value is
rejected with `C0608`; `value.method(args)` is accepted as one method-call
form. Static access to a method declaration through a type/module path remains
an ordinary function value and requires the explicit `self` argument.

Anonymous functions use the same signature and body rules. If the result is
syntactically declared, it is exact. The current grammar always declares a
result, so inferred anonymous-function results are reserved. A function
literal may use shape evidence from an expected function type, but authored
parameter/result annotations must still match identity. An anonymous function
with any `04b` capture is rejected with `C0617`; closure representation and
capture lifetime remain future work.

## Statement rules

- A block visits statements in order and creates one structural control
  region. It does not create or query a scope.
- `return e` generates `e` with the enclosing result as expected evidence and
  retains return compatibility. Bare `return` is legal only for exact `void`.
  A value return is illegal for `void`; a bare return is illegal otherwise.
- `if` and `while` add identity evidence `condition == bool`. Each arm/body is
  checked in its own control region.
- `loop a..b : i` and `loop a..=b : i` require bounds of one identical
  integral type after literal fitting. The optional iterator equals that type,
  is an immutable place, and is scoped by `04b` to the body. Range overflow and
  iteration direction are lowering/runtime decisions; descending ranges are
  not implicitly invented.
- A C-style `for` checks its initializer, requires an authored condition to be
  `bool`, treats an omitted condition as semantic `true`, checks its update,
  and then its body. A non-assignment update must be a standalone postfix
  `++`/`--` expression.
- An assignment statement uses the assignment rules below. An expression
  statement must be a call or postfix `++`/`--`; silently discarding any other
  non-`void` value is `C0612`.
- `print` generates every operand. It accepts `bool`, `char`, `str`, every
  integer, and every float. Composite, pointer, function, nominal, optional,
  and `void` values require a future formatting protocol and are rejected.
- `break` targets the innermost enclosing loop or switch. `continue` targets
  the innermost enclosing loop, ignoring switches. Missing targets are
  `C0611`.
- `defer S` registers the authored statement `S` with the current lexical
  block. Deferred statements execute in reverse registration order whenever
  control exits that block, including fallthrough, `return`, and a
  `break`/`continue` crossing the block boundary. Expressions inside `S` are
  evaluated when `S` executes, not when it is registered. Blocks, bindings,
  assignments, calls, printing, conditionals, loops, and switches are legal
  deferred statements. A deferred `return`, `break`, `continue`, or nested
  `defer` is `C0613`; these would transfer control out of or ambiguously extend
  an already-exiting region. Typed IR records a checked deferred statement and
  its lexical region; phase 10 owns edge expansion.

### Switch

A switch is either nominal-tag switching or scalar equality switching.

For an enum or tagged-union subject, each case must denote one variant of that
exact declaration. Because base-less partial members are intentionally
deferred by `04b`, phase 6 retains the case name and, after the subject nominal
is final, selects only from that declaration's ordered `04b Members`. It never
searches unrelated declarations. A variant may appear at most once. Within
a tagged-union case, the subject is narrowed to that variant, and access to
its payload member is legal only in that case's dominated region. Payload-free
variants expose no value. The switch is exhaustive when every variant is
covered or an `else` exists.

For `bool`, `char`, `str`, or integer subjects, each case expression is
generated with subject identity/literal evidence and must be an accepted
constant value. Duplicate values are rejected. Floating, pointer, optional,
tuple, array, slice, struct, union, tagged-union payload, and function equality
switches are forbidden. A scalar switch is exhaustive only when it has `else`,
except a Boolean switch containing both values and an enum switch containing
all variants.

Cases do not fall through. `break` exits the switch. An exhaustive switch falls
through after the statement only if at least one reachable case body does; an
unreachable missing alternative contributes no path.

## Expression rules

Every expression rule publishes its result term and records its value/place
category. `Missing`, `Error`, or an upstream error identity yields the session
error term, publishes no successful type, and suppresses dependent phase-6
diagnostics.

### Names, literals, grouping, and interpolation

- A name or path consumes its `04b` `SymbolID` and equals the occurrence with
  the preallocated symbol term. Type, qualifier, and damaged symbols are not
  values. No spelling is searched.
- Integer and float tokens create exact `05b` literal terms. Boolean, character,
  and string tokens use exact builtins. Character decoding must contain one
  Unicode scalar value; its runtime representation remains a phase-10 ABI
  decision and creates no numeric conversion.
- `nil` shapes as `Pointer(fresh pointee)`. It is compatible only through
  identity/shape evidence with a pointer type; it is not an integer zero and
  does not inhabit optionals.
- `none` shapes as `Optional(fresh payload)`. `some e` shapes as
  `Optional(type(e))`. An unconstrained payload is `T0510`.
- Grouping has its child's term and place category. A tuple literal or
  temporary is not a place; a binding or dereference whose type is a tuple is
  a place and supports valid element projections. Tuple shape is nonempty and
  follows authored order.
- Interpolation has type `str`. Each embedded value obeys the same printable
  set as `print`. Interpolation introduces formatting operations, not implicit
  conversions to `str`.
- `context` denotes the implicit runtime context of an enclosing `Pebble`
  function. It is unavailable in `C` functions. It is not an authored
  parameter and does not change the source-level parameter list or function
  `TypeID`; the calling convention carries it out of band. Its semantic type
  must come from the authoritative runtime-context contract identified in the
  upstream discrepancies below.
- `sizeof T` resolves `T`, rejects `void`, function, unresolved generic, and
  opaque extern types with `C0615`, and has type `uint`. Actual layout and the
  value are phase 9/10 work; a successful typed node retains `TypeID(T)`.

### Arrays, tuples, records, and functions

An array list literal has exact type `[N]E`, where `N` is the authored element
count. Every element is generated once. Literal and shape expectations may
select `E`; otherwise element terms are equal because array identity has one
element type. Post-solve element conversions are not used to manufacture a
common type. `[]` requires expected array shape; an unconstrained empty array
is `T0510`.

`[value; count]` evaluates `count` through the constant evaluator and has type
`[N]type(value)`. Count syntax is not a runtime expression in typed IR and is
not published as a value expression merely because the surface grammar calls
it an expression.

Tuple literal elements receive matching expected tuple components when
available. An already typed tuple may convert component-wise only under the
conversion matrix. Record literals use the declaration rules above. Function
literals use the function rules above.

### Calls and generic instantiation

A direct call obtains its signature by resolved `SymbolID`. An indirect call
constrains the callee with a function shape containing fresh ordered parameter
terms and one result term. Arity is checked before compatibility:

- nonvariadic calls require exactly the fixed parameter count;
- calling a C variadic function is rejected with `C0604`; Pebble has no
  semantic C default-promotion or explicit C ABI scalar types yet, so accepting
  extra arguments would silently copy host/prototype behavior;
- every fixed argument is generated with its parameter as expected evidence
  and retains argument compatibility;
- the call result is the signature result, including `void`.

No implicit calling-convention adaptation exists. The callee's exact function
type supplies the convention recorded in typed IR.

At a generic call or explicit instantiation, phase 6 allocates one fresh term
per declared type parameter in declaration order, relates explicit arguments,
receiver evidence, ordinary arguments, and expected result, emits
`Instantiate` for every receiver/parameter/result template, and calls
`PublishInstantiation` at the site. Missing type arguments are inferred;
provided arguments occupy the leading declared positions and the remaining
positions are inferred. `_` is unsupported. Extra arguments are `T0501`.

Explicit instantiation may produce a function value without a call. A bare
generic declaration is not a value and cannot be published without a unique
complete instantiation. Phase 6 records, but does not perform, phase-7
specialization.

### Members, fields, methods, and variants

Static module/type members consume `04b` identities. Type-directed fields emit
`HasField(receiver,name,result)` and retain receiver/name/result for
declaration-local post-solve identity selection. Immediate instance-method
calls emit `SelectMethod(receiver,name,callable,explicit,site)`; its immutable
method selection supplies the `SymbolID` and inferred method-local arguments.
Phase 6 then validates category and receiver behavior:

- a struct field access yields a place when the receiver is a place; its
  writability is inherited from the receiver root;
- `(*p).field` is supported through explicit dereference; phase 6 performs no
  automatic pointer dereference;
- tuple `.N` requires a base-10 integer token in range and yields the selected
  element; it inherits place/writability from a tuple place projection even
  though a tuple value as a whole is not directly assignable;
- enum variants are values of the enum type;
- tagged-union variants are constructors when selected through a type and
  narrowed payload projections when selected through a value;
- instance methods are legal only as the immediate callee of a method call and
  bind the explicit `self` parameter;
- fields are not callable unless their solved field type is a function;
- unknown or wrong-category members diagnose once at the member occurrence.

A tagged-union variant with `void` payload takes no argument; every other
variant constructor takes exactly one compatible payload argument. Its result
is the containing nominal application.

A base-less partial member `.name` is legal only for an expected enum or tagged
union and must select a unique variant of that expected declaration. It does
not search all declarations for a matching spelling.

### Neutral brackets, indexing, and slicing

Phase 6 consumes `04b` bracket classification exactly:

- `BracketTypeNames`: resolve every argument as a type and perform explicit
  generic instantiation;
- `BracketValueNames`: require one value argument and generate indexing;
- `BracketDeferred`: construct exactly one two-alternative `OneOf`, ordered
  generic first and indexing second, and tag all branch-specific checker
  records with alternative indices `0` and `1`.

No other rule uses `OneOf`. An alternative contains only algebraic constraints.
No more than two alternatives are created for one deferred bracket, nested
brackets are independently bounded by `05b`, and phase 6 uses only the unique
post-solve `Selection`. It never tries one interpretation and falls back.

Indexing rules are:

| Base | Index | Result | Place |
| --- | --- | --- | --- |
| `[N]T` | any integral type | `T` | inherits array root writability |
| `[]T` | any integral type | `T` | writable through the raw slice |
| `str` | any integral type | `char` | never a place; index counts Unicode scalar values |

Pointer and tuple indexing are forbidden. Bounds checks are explicit typed-IR
operations for arrays, slices, and strings; a provably constant out-of-range
index is `C0609`. String slicing also uses Unicode-scalar indices and may not
split an encoded scalar value.

Slicing `[start:end]` accepts arrays and slices and returns `[]T`; slicing a
`str` returns `str`. Each present bound must be integral. Slicing an array is
an explicit source operation that creates a view; the general implicit
array-to-slice conversion remains forbidden. Bounds are half-open and checked
in typed IR. Omitted start/end mean zero/length. Slice lifetime and escape
rules remain future lifetime/runtime work; until specified, an array-derived slice may not
escape its declaring function through return, global storage, or capture.

### Prefix, postfix, cast, and binary expressions

Prefix `&` requires a writable place and yields `*T`. Because pointer identity
has no constness, taking the address of a `let` is forbidden. Prefix `*`
requires `*T`, yields a writable place of type `T`, and introduces no automatic
null check beyond the language's raw-pointer policy. Prefix/postfix operator
rules are otherwise in the operator table below.

Postfix force unwrap `e!` requires `?T`, returns `T`, is not a place, and emits
an explicit checked unwrap operation. `++`/`--` require a writable numeric
place, perform the corresponding `+`/`-` rule with literal one, store the
result, and have type `void`; the operand is evaluated once. They are legal
only as expression statements or `for` updates.

`source as Destination` resolves the destination before solve, retains a cast
record, and gives the expression exact destination type. Validation accepts
identity, implicit, or explicit conversion-matrix entries and rejects
forbidden entries. The syntax node is not mutated; typed IR contains
`ExplicitNumericCast`, `OptionalInject`, or the applicable
composite coercion.

## Operator families

Both operands of a binary family are generated before facts are added. Where
the table says “same,” generation adds identity facts between operands after
literal evidence; mixed already-concrete types require an authored cast.

| Operators | Operand rule | Result | Notes |
| --- | --- | --- | --- |
| unary `-` | numeric | operand type | exact literal negation uses `05b`; unsigned nonliteral negation is forbidden |
| unary `!` | exact `bool` | `bool` | prefix logical not |
| unary `~` | integral | operand type | bitwise complement |
| `+ - * /` | same numeric | same type | `+` also accepts `str,str -> str`; division policy is runtime/lowering |
| `%` | same integral | same type | no floating remainder |
| `<< >>` | integral left; integral right | left type | operand types need not match; result follows left; runtime masks are forbidden and invalid counts are checked |
| `& | ^` | same integral | same type | Boolean bitwise operations are forbidden |
| `&& ||` | exact `bool`, exact `bool` | `bool` | short-circuit left-to-right |
| `< <= > >=` | same numeric, or `char/char`, or `str/str` | `bool` | strings use scalar-value lexicographic order |
| `== !=` | same equatable type | `bool` | no conversion except direct literal fitting |

Pointer arithmetic is forbidden. Pointer equality is accepted only for the
same pointer `TypeID`, including a contextual `nil`. Pointer ordering is
forbidden. Array, slice, tuple, optional, struct, tagged/untagged union, opaque,
and function equality is forbidden in the first contract. Enum equality and
ordering are accepted only within the same enum identity; enum ordering uses
declaration order. `void` is never an operand.

For a rigid generic parameter, generation emits the `05b` Numeric, Integral,
or Ordered capability where applicable and phase 6 retains the exact operator
requirement. Equality emits a checker-owned `Equatable` requirement because
`05b` has no corresponding capability. At an instantiation, the ordered
requirements are attached to the specialization obligation and checked by
phase 7. No operator succeeds merely because one observed concrete call works.

## Places, mutation, and assignment

A place is one of:

- a value binding or parameter name;
- dereference of a pointer;
- a valid struct-field or tuple-element projection from a place;
- valid array/slice indexing.

Calls, casts, literals, record/array/tuple temporaries, force unwrap, string
indexing, methods, variants, functions, `sizeof`, operators, and partial
members are not places. A place is writable when rooted in `var`, a writable
parameter/storage symbol, pointer dereference, or writable slice storage and
all projections preserve writability. `let`, range iterators, constants,
function symbols, and enum/variant values are not writable. Raw pointer and
slice parameters are writable through because their type identity has no
constness.

Simple assignment requires a writable place and validates source-to-place
compatibility once. Compound assignment requires a writable place, applies the
corresponding binary operator to the old value and right operand, then requires
the operator result to be assignable back to the exact place type. It evaluates
the place expression once and emits explicit temporaries in typed IR. Assignment
is never an expression.

## Compatibility and conversion matrix

Compatibility is directional from source to destination. `identity` emits no
coercion, `implicit` emits an explicit typed-IR coercion at assignment,
argument, return, field, tuple-element, or accepted branch boundary,
`explicit` is accepted only under `as`, and `forbidden` is rejected. Literal
fitting and contextual `nil`/`none` shaping are inference evidence, not
conversions between concrete types.

Assignment, fixed arguments, returns, record fields, tuple components, and any
future expression branches use the same matrix. Operator peers use stricter
identity rules stated above. Variadic arguments and switch cases never receive
implicit conversions.

The authoritative grammar has no value-producing conditional or switch
expression, so phase 6 currently has no branch-result join. If such syntax is
added, every branch must be generated with one shared expected result and each
retained branch-to-result relationship must use this same matrix; neither
branch may select the other's type.

### Primitive matrix

Rows are source families and columns are destinations.

| source → destination | integer | float | bool | char | str | void |
| --- | --- | --- | --- | --- | --- | --- |
| integer | identity for the same builtin; otherwise explicit | explicit | forbidden | forbidden | forbidden | forbidden |
| float | explicit | identity for the same builtin; otherwise explicit | forbidden | forbidden | forbidden | forbidden |
| bool | forbidden | forbidden | identity | forbidden | forbidden | forbidden |
| char | forbidden | forbidden | forbidden | identity | forbidden | forbidden |
| str | forbidden | forbidden | forbidden | forbidden | identity | forbidden |
| void | forbidden | forbidden | forbidden | forbidden | forbidden | identity |

“Other integer” includes signed/unsigned, width, and `int`/`uint` changes.
There are no implicit conversions between already concrete numeric types.
Every integer/integer, integer/float, float/integer, and `f32`/`f64` change is
explicit and uses the target type's defined truncation/rounding behavior.
Integer narrowing discards high bits modulo `2^N` and reinterprets signedness
in two's-complement; float-to-integer truncates toward zero and traps in checked
mode when out of range; integer-to-float and float-width changes use IEEE
round-to-nearest ties-to-even and reject finite-to-infinity in checked mode.
Release-mode trap behavior remains a phase-10 decision, but the semantic cast
kind is fixed here. `char` remains semantically distinct until its ABI is
specified.

### Composite matrix

| Source | Destination | Class | Rule/coercion |
| --- | --- | --- | --- |
| `*A` | `*A` | identity | exact pointer identity |
| `*A` | `*B`, `A != B` | forbidden | requires a future unsafe/FFI contract |
| pointer | integer or reverse | forbidden | requires a future unsafe/FFI contract |
| pointer | optional or reverse | forbidden | pointers already admit contextual `nil` only |
| `[N]A` | `[N]A` | identity | exact length and element identity |
| `[N]A` | `[M]B` otherwise | forbidden | no array resizing or element-wise array cast |
| `[N]A` | `[]A` | forbidden | explicit slicing syntax creates the view; `as` does not |
| `[]A` | `[]A` | identity | exact element identity |
| `[]A` | `[]B` otherwise | forbidden | no variance or element mapping |
| `[]A` | array | forbidden | no implicit length proof or copy |
| `(A...)` | identical tuple | identity | exact arity/components |
| `(A...)` | `(B...)` same arity | implicit, explicit, or forbidden as derived | implicit iff every component is identity/implicit; otherwise explicit under `as` iff every component is not forbidden; otherwise forbidden |
| tuple | nontuple or different arity | forbidden | no packing/unpacking |
| `A` | `?A` | implicit | `OptionalInject`, including injection of `?T` into `??T` |
| `?A` | `?A` | identity | exact payload identity |
| `?A` | `A` | forbidden as conversion | postfix `!` is the only extraction form |
| `?A` | `?B`, payload differs and destination payload is not the complete source `?A` | forbidden | no optional mapping; nested injection is the preceding row |
| nominal `D[args]` | same `TypeID` | identity | includes structs, enums, unions, tagged unions, opaque types |
| nominal | any different nominal or structural type | forbidden | no structural or representation conversion |
| enum | integer or reverse | forbidden | declaration-order representation is not a conversion contract |
| tagged/untagged union | member payload or reverse | forbidden | variant construction/projection uses dedicated syntax |
| function | identical function `TypeID` | identity | parameters, result, convention, and variadic bit all exact |
| function | any distinct function type | forbidden | no variance, thunk, or ABI adaptation |

Transparent aliases have already collapsed and therefore follow their target
row. Type parameters are compatible only by rigid identity during generic-body
checking; their inferred requirements are not conversions. Record construction,
variant construction, array slicing, optional unwrap, and bounds checks are
typed operations, not hidden compatibility exceptions.

Every primitive-to-composite, composite-to-primitive, or composite pair not
listed above is forbidden. A conditional composite row always resolves to one
of `identity`, `implicit`, `explicit`, or `forbidden`; “derived” is not a fifth
conversion class.

### Calling conventions

`Pebble` and `C` function types are distinct and incompatible. Assignment,
argument passing, return, equality, indirect call adaptation, and `as` between
different conventions are forbidden. A call uses the convention of its exact
callee type. Phase 10 may later introduce an explicitly declared adapter, but
phase 6 never synthesizes one. Variadic and nonvariadic functions are likewise
incompatible.

## Constant expressions and array lengths

Phase 6 owns one memoized `ConstantEvaluator`; its `ArrayLength` adapter is
passed to `infer.Prepare` before `Check`. It consumes only immutable syntax,
`04b` reference mappings, and decoded literal data. It performs no type lookup,
layout query, body traversal, host evaluation, or solver query.

```go
type ConstantInputs struct {
    Graph      *module.Graph
    Sources    *source.FileSet
    Resolution *symbol.Result
}

func NewConstantEvaluator(
    inputs ConstantInputs,
    diagnostics *diagnostic.DiagnosticSet,
    config Config,
) *ConstantEvaluator

func (e *ConstantEvaluator) ArrayLength(symbol.SyntaxRef) infer.ArrayLengthResult
```

The evaluator is compilation-owned and immutable in identity, with internal
single-owner memoization. The driver creates it first, passes it as
`ProgramInputs.ArrayLengths` to `infer.Prepare`, and passes the same evaluator
to `Check`. Its graph, sources, and resolution must be the exact checker
inputs. Construction validates this snapshot relationship without traversing
function bodies.

The accepted constant language is deliberately small:

```text
integer, Boolean, character, and string literals
grouping
unary - and ~ on integer constants; ! on Boolean constants
integer + - * / % << >> & | ^
Boolean && ||
same-kind == != and ordered scalar comparisons
references to module-level let declarations whose initializer is constant
enum variant values
```

Array lengths and array-repeat counts accept only the integer-valued subset.
The final value must be nonnegative and fit `uint64`; `05a` separately enforces
`MaxArrayLength`. Calls, casts, `sizeof`, floats, `nil`, `none`, `some`, arrays,
tuples, records, members other than enum variants, indexing, interpolation,
functions, mutable/global `var`, parameters, locals, and runtime values are not
constant expressions.

Integer evaluation is arbitrary precision. Division truncates toward zero;
remainder has the dividend's sign. Division by zero, a negative shift, a shift
count exceeding the operation limit, invalid operator categories, a reference
cycle, excessive depth/operations/bits, or a nonrepresentable final length
emits one `C0614`. `&&` and `||` short-circuit. References use `04b` `SymbolID`
and a white/gray/black table; constants may refer forward and across imported
modules, and a cycle is diagnosed once in deterministic symbol order.

The adapter contract is exact:

- return `ArrayLengthKnown{Value}` only after successful integer evaluation;
- return `ArrayLengthError` when phase 6 has emitted the unique diagnostic for
  that occurrence/component;
- return `ArrayLengthUnavailable` only for inconsistent snapshot/input or an
  internal resource boundary for which no semantic result can be supplied;
- memoize by `SyntaxRef`, return the same state on repeated `05b` requests, and
  never emit a duplicate diagnostic;
- perform bounded explicit-stack evaluation; host recursion and machine
  integer overflow are not semantics.

The same evaluator supplies constant scalar values for switch duplicate checks
and proves whether a global initializer is constant. This does not make such
values `TypeID`s.

## Generic body checking and requirements

A generic function, method, or nominal method body is generated symbolically
once with rigid `TypeParameter(SymbolID)` types. It is never rechecked by
cloning or rewriting its surface AST. Containing nominal parameters precede
method-local parameters in the symbolic environment.

Phase 6 retains one normalized requirement per first source origin:

```go
type RequirementKind uint8
const (
    RequirementNumeric RequirementKind = iota + 1
    RequirementIntegral
    RequirementOrdered
    RequirementEquatable
)

type Requirement struct {
    Owner    symbol.SymbolID
    Kind     RequirementKind
    Subject types.TypeID // rigid TypeParameter ID
    Origin   symbol.SyntaxRef
    Operator string
}
```

Numeric, integral, and ordered requirements must agree with the corresponding
`05b` `Solution.Requirements`; phase 6 joins them by owner, rigid subject, and
origin. `Equatable` is checker-owned. Requirements are sorted by owner,
type-parameter declaration order, kind, and first origin. Repeated equivalent
uses do not duplicate the semantic interface, but all use sites remain related
diagnostic labels.

The first contract can infer only these closed requirements. Field access,
method lookup, indexing, calling an otherwise unconstrained type parameter,
conversion to/from a parameter, layout, printing, and construction cannot be
expressed as a generic requirement and are rejected in the generic body with
`C0610`. A future public trait/constraint system must extend this closed
contract explicitly.

Each call-site instantiation records the generic `SymbolID`, ordered solved
type arguments, and the declaration requirements. Phase 7 proves them against
the concrete arguments, owns recursion/specialization caching, and emits the
call-site/declaration diagnostic if they fail. Phase 6 does not accept a body
based on an observed instantiation and does not specialize it.

## Control flow, reachability, and exits

Control analysis is structural and function-local. It uses no file-global
Boolean state. Each statement computes an exit set from:

```text
Fallthrough  Return  Break(target)  Continue(target)  Diverge
```

Sequential composition sends only `Fallthrough` into the next statement.
Statements reached by no incoming edge are still type-checked for independent
errors and receive warning `C0618`; they do not contribute exits or inferred
returns. Diagnostics are emitted once at the first unreachable statement of a
contiguous region.

- `return` contributes `Return` after evaluating its expression and registered
  defers for every lexical region crossed by the exit.
- `break`/`continue` contribute their resolved target exit and execute defers
  in each lexical region they cross; loop-local lowering preserves target
  identity.
- `if` unions reachable exits from both arms; a missing `else` contributes
  `Fallthrough`.
- A loop consumes matching `Break` as loop fallthrough and matching `Continue`
  as a back edge. An unconditional `for ;;`/`while true` has no fallthrough
  unless a reachable matching `break` exists. Other loops conservatively may
  fall through.
- A switch consumes its matching `Break`. A nonexhaustive switch contributes
  `Fallthrough`; an exhaustive switch unions only its case/else exits.
- `defer` registration itself falls through; the checked deferred statement
  cannot contain an outward control transfer.

Every reachable path of a non-`void` function must end in `Return` or
`Diverge`; otherwise `C0607` labels the function result and the smallest
fallthrough edge. A `void` function receives an implicit typed-IR return only
when its end is reachable. Return compatibility is checked before definite
return analysis so one mismatch does not masquerade as a missing return.

### Entry point

For `EntryRequired`, the supplied symbol must identify exactly one nonmethod,
nongeneric, nonvariadic, nonextern Pebble function in the root compilation
module. It must take zero parameters and return exactly `void` or `int`. It may
not capture, require inferred generic obligations, or use the `C` convention.
`int` is the process status; `void` means successful status. Command-line
argument parameters, custom runtime context, multiple entries, and adapter
signatures remain phase-11/runtime decisions. `EntryNone` performs no entry
validation.

## Typed-IR production

Validation walks frozen records in source order and creates typed IR without
rewriting syntax. Every expression/place carries its final `TypeID` and
resolved `SymbolID`/member identity. At minimum the IR distinguishes:

```text
Identity value (no coercion node)
ExplicitNumericCast
OptionalInject
TupleCoerce
CheckedOptionalUnwrap
CheckedIndex / CheckedSlice
DirectCall / IndirectCall / MethodCall / VariantConstruct
FieldPlace / TuplePlace / IndexPlace / DereferencePlace
DeferredStatement
GenericInstantiation reference
```

Evaluation order is authored left-to-right. Compound assignments and method
calls contain explicit single-evaluation temporaries. Coercions are inserted
only at retained compatibility or cast records, never opportunistically inside
an operator. Failed or partial solving never produces backend-consumable IR.

## Diagnostics and cascade suppression

Initial stable phase-6 codes are:

| Code | Meaning |
| --- | --- |
| `C0601` | incompatible assignment, argument, return, field, tuple component, or conversion |
| `C0602` | invalid binding initialization form |
| `C0603` | invalid operator operands |
| `C0604` | invalid call, method, arity, convention, or variadic use |
| `C0605` | invalid member, record, or variant operation |
| `C0606` | expression is not a writable place |
| `C0607` | invalid return form or reachable non-void fallthrough |
| `C0608` | invalid function/method declaration or unsupported bound method value |
| `C0609` | invalid indexing, slicing, or statically invalid bounds |
| `C0610` | unsupported or unsatisfied inferred generic requirement |
| `C0611` | invalid `break`, `continue`, switch case, or control target |
| `C0612` | invalid expression statement or printable value |
| `C0613` | invalid `defer` |
| `C0614` | invalid or over-limit constant expression |
| `C0615` | operation requires unresolved layout, union safety, opaque, pointer, or enum policy |
| `C0616` | nonconstant global initialization |
| `C0617` | unsupported closure capture or escape |
| `C0618` | unreachable statement warning |
| `C0619` | checker limit or inconsistent phase input/boundary |
| `C0620` | invalid configured entry point |

Diagnostics are structured records in the shared `DiagnosticSet`. The primary
label is the authored operation; related labels identify the destination,
declaration, conflicting operand, generic requirement, control target, or
earlier constant in stable source order. Type text uses the deterministic
semantic formatter outside `types.Store`. Diagnostics never expose `InferID`,
pointer addresses, hash order, generated names, or speculative `OneOf` state.

If a required `Solution` result is `TypeError`, or an upstream diagnostic
already invalidates the exact symbol/occurrence, phase 6 suppresses dependent
policy diagnostics. One failed compatibility record does not suppress an
independent control-flow error. One root operation emits one primary
diagnostic; child records mark themselves explained. When `MaxDiagnostics` is
reached, the final retained phase-6 diagnostic is replaced by one `C0619` and
further phase-6 diagnostics are suppressed. Earlier-phase diagnostics do not
consume this budget.

## Source fixtures and direct tests

Language behavior is tested primarily with ordinary Pebble source:

```text
tests/check/
  valid/*.peb
  valid/multimodule/<case>/main.peb
  invalid/C0601/*.peb
  ...
  invalid/C0620/*.peb
  recovery/*.peb
```

Required fixtures cover every declaration, statement, expression, operator,
place, call, member, variant, record, bracket, index/slice, generic,
conversion-matrix, constant, control-flow, defer, and entry rule above. Matrix
fixtures include every primitive family pair and every composite row, with
positive identity/implicit/explicit cases and negative implicit/forbidden
cases. Multi-module fixtures prove all names and members come from `04b`
identity. Generic fixtures prove symbolic body checking, requirement order,
receiver/argument/result inference, explicit instantiation values, and no
observed-call acceptance. Recovery fixtures combine independent solver,
conversion, and control errors and assert bounded cascades.

Direct Go tests in `compiler/internal/check` are authoritative for:

- exactly one traversal, one `Solve`, and rejection of post-solve mutation;
- publication completeness and deterministic record/table order;
- identity, literal, and recursive shape evidence;
- `BracketDeferred` creating exactly two alternatives and honoring only the
  unique `Selection`;
- every conversion-matrix cell and exact coercion kind;
- place projection/writability and single evaluation of compound assignment;
- operator-family classification and rigid requirement normalization;
- explicit-stack constant evaluation, cycles, arithmetic semantics, and every
  limit;
- exit-set composition, switch exhaustiveness, defer order, and unreachable
  suppression;
- repeated runs with forced map seeds producing identical diagnostics, result
  tables, and normalized typed IR;
- every semantic-record, control-depth, constant, place, requirement, and
  diagnostic limit terminating atomically.

Use direct assertions for terms, selected alternatives, `TypeID` equality,
conversion kinds, requirements, exit sets, and IR node shape. Use diagnostic
sidecars only where label placement/rendering matters. Normal tests never
rewrite goldens.

Verification from `compiler/` is:

```sh
go test ./...
go test -race ./...
go vet ./...
```

Also run `git diff --check` at the repository root. Source fixtures, not the C
prototype, are normative. Prototype cases may motivate a fixture only after
the rule is accepted here.

## Dependency-ordered implementation slices

### Slice 06.1: checker shell, records, lifecycle, and publication

Own `compiler/internal/check/config.go`, `record.go`, `result.go`, and the
checker driver. Implement snapshot validation, limits, deterministic traversal
skeleton, publication accounting, freeze states, exactly-one solve, and empty
typed-IR/result plumbing. No policy shortcuts. Complete when a fake session
proves all lifecycle failures and table ordering.

### Slice 06.2: constant evaluator

Own `constant.go`, its `ArrayLengthEvaluator` adapter, and direct/source
constant fixtures. Implement the closed constant language, symbol-identity
references, cycles, checked arbitrary precision, memoization, and limits.
Complete before production `infer.Prepare` integration; do not place constant
evaluation in `05b`.

### Slice 06.3: declarations and expression fact generation

Implement all declaration/binding/function and primary/aggregate expression
rules, the three expected-evidence forms, publication roots, and checker
records. Complete when every surface declaration/expression kind is generated
once and solving needs no validation-time equation.

### Slice 06.4: calls, members, records, generics, and neutral brackets

Implement direct/indirect/method/variant calls, field and record facts,
instantiation evidence, symbolic requirements, explicit instantiation values,
index/slice records, and the exact two-way `OneOf`. Complete when phase 7 can
consume ordered obligations and no scope/name lookup or AST clone exists.

### Slice 06.5: operators, places, assignment, and conversions

Implement operator fact generation, place projections, mutability, retained
compatibility validation, the complete matrices, and explicit coercion IR.
Complete when every matrix cell and operator family has direct plus source
coverage and mixed concrete numeric behavior never depends on operand order.

### Slice 06.6: control flow, defer, switches, and entry point

Implement structural exit sets, narrowing, loop/switch targets, definite
return, unreachable warnings, checked lexical-defer records, constant global
validation, and `SymbolID`-based entry validation. Complete when nested
control cases require no global flags and every edge has a deterministic test.

### Slice 06.7: typed IR integration and recovery hardening

Build the closed typed nodes, explicit single-evaluation temporaries,
coercions/checks, source spans, deterministic dumps, recovery suppression,
limit/fuzz tests, race tests, and full fixtures. Complete when successful IR
contains no unresolved lookup, term, choice, or heuristic type decision and a
failed solve produces no backend input.

Each slice handoff reports owned files, public/internal contracts, fixtures,
direct tests, commands/results, resource limits exercised, and any upstream
contract discrepancy. A slice must not edit phases 03, 04b, 05a, or 05b to
make implementation easier.

## Upstream discrepancies and explicit future features

The current authoritative inputs have one phase-boundary discrepancy that must
be resolved in its owning specification before the affected phase-6 slice is
implemented:

1. `ContextExpr` requires a canonical semantic type and runtime identity for
   the implicit Pebble context. Neither `04b`, `05a`, nor `05b` currently owns
   such an identity. The runtime-context specification must define how the
   checker obtains that type without creating a generated source symbol or a
   textual lookup.

The concrete `05b` implementation supplies bounded `SelectMethod` and
immutable `MethodSelection` for delayed generic method calls; phase 6 must use
that operation rather than search declarations or perform a second generation
walk. The concrete `04b Result` already exposes `Reference`, `References`,
`Qualifier`, `Bracket`, `Captures`, and `Members`; no new bracket or static
member query is required. Default/custom entry selection belongs to the
driver: it passes a preselected `SymbolID`, and phase 6 validates identity
without textual lookup. Checker-owned `Equatable` requirements remain phase-6
records and do not require a new `05b` capability.

The following are explicit future features, not accidental prototype behavior:

- dynamic or zero-initialized globals and their ordering;
- uninitialized locals and definite assignment;
- implicit array-to-slice conversion and complete slice lifetime/escape rules;
- pointer arithmetic, pointer/integer conversion, const pointers, and an
  `unsafe` facility;
- enum/integer conversions, untagged-union read/write safety, and `char` ABI;
- structural equality, formatting protocols, user-defined operators, and
  public traits/constraints;
- closure captures and bound-method values;
- Pebble variadics, C default argument promotions, and cross-convention
  adapters;
- public constant declarations beyond module `let`, floating constants, and
  layout-dependent `sizeof` constants;
- command-line entry parameters, runtime context injection, and alternate
  entry adapters;
- conditional expressions, which are absent from the authoritative `03a`
  grammar;
- release-mode overflow/trap mechanics, runtime representations, layout,
  specialization policy, and backend ABI.

None of these may be enabled by copying behavior from the C prototype. Each
requires a specification change, source fixtures, and an explicit typed-IR or
runtime consequence.
