# 05a Semantic Type Store

`05a` defines the immutable semantic type foundation used by inference,
checking, typed IR, specialization, layout, and lowering. It replaces the C
prototype's pointer identity, duplicate mutable `Type` objects, canonical-name
keys, checker globals, and traversal-dependent type choices. The prototype is
behavioral evidence only.

## Identity boundary

The four identities must not be conflated:

| Identity | Meaning |
| --- | --- |
| `SyntaxRef` | one authored occurrence |
| `SymbolID` | one declaration |
| `TypeID` | one immutable canonical semantic type |
| `InferID` | one mutable `05b` inference variable |

`TypeID` is not a source spelling, host pointer, declaration address, layout,
generated C name, hash, or inference-variable address. A `TypeID` is never
rebound and its key is never mutated. `05b` may represent a solver term as
`Known(TypeID)` or `Variable(InferID)`; solving the latter produces a
`TypeID`, not a change to the former.

## Ownership, lifetime, and IDs

One `Compilation` owns one `types.Store`. The store and every `TypeID` it
issues live only for that compilation snapshot. IDs must not be compared
across stores or serialized as durable cache keys. There is no package-global
store, current type, or implicit target.

```go
type TypeID uint32

func (id TypeID) IsValid() bool
```

Zero is invalid. A store rejects zero, out-of-range, or not-yet-issued child
IDs. Passing an ID from another store violates the API contract; because IDs
are compact integers, a coincidentally in-range foreign ID is not detectable.
Callers must preserve store ownership.

The store owns copies of all variable-length key data. Public accessors return
scalars, iterators, or copies and never mutable backing slices or entry
pointers. Store mutation consists only of appending a new immutable entry and
its lookup index. The first implementation is single-owner and not safe for
concurrent calls, including reads concurrent with `Intern`. A later read-only
concurrency contract requires a separate specification; `05b` must serialize
access now.

## Public contract

The implementation lives in `compiler/internal/types`. Its public surface is:

```go
type Config struct {
    MaxTypes          uint32
    MaxKeyComponents  uint32
    MaxTupleElements  uint32
    MaxFunctionParams uint32
    MaxGenericArgs    uint32
    MaxArrayLength    uint64
}

func New(config Config) (*Store, error)
func (s *Store) Builtins() Builtins
func (s *Store) Intern(key TypeKey) (TypeID, error)
func (s *Store) Kind(id TypeID) (Kind, bool)
func (s *Store) Key(id TypeID) (TypeKey, bool)
func (s *Store) Len() uint32
func (s *Store) IDs() iter.Seq[TypeID]
```

Zero-valued limits select these defaults: `MaxTypes = 1,048,576`,
`MaxKeyComponents = 4,096`, `MaxTupleElements = 1,024`,
`MaxFunctionParams = 1,024`, `MaxGenericArgs = 256`, and
`MaxArrayLength = math.MaxUint64`. `MaxKeyComponents` bounds the child
`TypeID`s in one key; the constructor-specific lower bound also applies. Tests
may lower each limit. A nonzero `MaxTypes` below the 16 required builtins makes
`New` return `ErrLimitExceeded` without a usable store.

`IDs` yields valid IDs in ascending allocation order. Numeric ID values and
allocation order are not language behavior or persistent identity; only
equality is semantic. Given the same configuration and ordered `Intern` calls,
IDs and iteration repeat exactly. Hash-map bucket order is never observable.

`Kind` and `Key` are the only general decomposition operations. Named
consumers use `TypeKey` accessors for children, function components, nominal
declaration, and generic arguments; the package must not add parallel
`IsNumeric`, `Fields`, layout, display-name, or backend-name APIs.

## `TypeKey`

`TypeKey` is a closed tagged value with unexported storage. Package
constructors copy input slices; accessors return copies or iterators.
Logically its complete representation is:

```text
Builtin(BuiltinKind)
Pointer(pointee TypeID)
Array(length uint64, element TypeID)
Slice(element TypeID)
Tuple(ordered elements []TypeID)
Optional(element TypeID)
Function(
    convention CallingConvention,
    ordered parameters []TypeID,
    result TypeID,
    variadic bool,
)
Nominal(declaration SymbolID, ordered arguments []TypeID)
TypeParameter(declaration SymbolID)
```

The corresponding `Kind` values are `Builtin`, `Pointer`, `Array`, `Slice`,
`Tuple`, `Optional`, `Function`, `Nominal`, and `TypeParameter`. A zero
`TypeKey`, unknown tag, or unknown enum value is invalid. Grouped source type
syntax has no key because grouping has no semantic meaning.

All listed components participate in identity, including order, array length,
function convention, and `variadic`. Nothing else does. In particular, source
spans, authored names, field spellings, module-path strings, diagnostic state,
layout, use counts, and backend names are absent.

The public constructors are exactly:

```go
BuiltinKey(BuiltinKind) TypeKey
PointerKey(TypeID) TypeKey
ArrayKey(uint64, TypeID) TypeKey
SliceKey(TypeID) TypeKey
TupleKey([]TypeID) TypeKey
OptionalKey(TypeID) TypeKey
FunctionKey(CallingConvention, []TypeID, TypeID, bool) TypeKey
NominalKey(symbol.SymbolID, []TypeID) TypeKey
TypeParameterKey(symbol.SymbolID) TypeKey

func (k TypeKey) Kind() Kind
func (k TypeKey) Builtin() (BuiltinKind, bool)
func (k TypeKey) Child() (TypeID, bool) // pointer, slice, or optional
func (k TypeKey) Array() (length uint64, element TypeID, ok bool)
func (k TypeKey) Elements() ([]TypeID, bool) // tuple copy
func (k TypeKey) Function() (
    convention CallingConvention,
    parameters []TypeID,
    result TypeID,
    variadic bool,
    ok bool,
)
func (k TypeKey) Nominal() (
    declaration symbol.SymbolID,
    arguments []TypeID,
    ok bool,
)
func (k TypeKey) TypeParameter() (symbol.SymbolID, bool)
```

`Child` succeeds only for the three one-child kinds. Slice-returning accessors
return fresh copies. A mismatched accessor returns `ok == false` and zero
payloads. These accessors, plus `Store.Kind` and `Store.Key`, are the complete
public decomposition API.

The store validates representation, issued child IDs, nonzero declaration
IDs, configured counts, and total key components. It does not validate source
legality such as whether an optional payload is permitted, a variadic
parameter is a slice, or a nominal argument satisfies a requirement. The
nonempty tuple rule is a key representation invariant and is validated here.
Other rules are resolver, `05b`, phase 6, or phase 7 responsibilities.

## Interning

`Intern` performs these steps:

1. Validate the closed key and every direct child against this store and the
   configured limits.
2. Compute a hash over the complete tagged key.
3. Search only that hash bucket, comparing the complete key on every candidate.
4. Return the existing ID on full equality, otherwise copy the key, append one
   immutable entry, and index it.

A hash match is never equality. Collision handling must compare tags, scalars,
ordered lengths, and every element. Child IDs are already canonical because a
valid child must have been issued by this store; `Intern` never follows a
pointer, resolves an alias, rewrites a child, or accepts an unresolved child.
Bottom-up interning therefore canonicalizes every finite structural key.

Invalid and over-limit keys return errors matching package sentinels
`ErrInvalidKey` and `ErrLimitExceeded`, respectively, and do not append an
entry or partially update the index. The concrete error may retain the
offending kind or limit for diagnostics, but never a source span. Reaching
`MaxTypes`, a component limit, or `MaxArrayLength` fails in bounded work with
no retry or eviction. Allocation failure follows the compiler's process-level
out-of-memory policy; it is not a semantic diagnostic. Hash functions may be
randomized internally for denial of service resistance, but equality,
returned IDs for an ordered call stream, iteration, diagnostics, and dumps
cannot depend on the seed or map iteration.

## Builtins

`New` preinterns these builtins in a fixed package-defined order:

```text
bool char str void
int uint
i8 i16 i32 i64
u8 u16 u32 u64
f32 f64
```

`int` and `uint` mean target-native signed and unsigned word values. They are
not aliases for C `int`, are not equal to an exact-width integer even when the
target widths match, and require no target layout in the store. Layout later
maps them using the selected target.

```go
type Builtins struct {
    Bool, Char, Str, Void TypeID
    Int, Uint             TypeID
    I8, I16, I32, I64     TypeID
    U8, U16, U32, U64     TypeID
    F32, F64              TypeID
}
```

`Builtins` is returned by value. Callers do not intern primitive spellings or
assume numeric ID values. The accepted contract has no `any`, `never`, `none`,
`unknown`, or canonical error builtin. `none` is an expression involved in
optional inference, not a semantic type. Adding a distinguished type requires
a specification revision and a new `BuiltinKind`. Builtins have categories,
not a numeric preference rank; neither the key nor its accessors can implement
heuristic numeric winner selection.

## Composite identity

- `Pointer(T)` is identified only by pointee `T`. Pebble currently has one raw
  pointer form; no mutability, ownership, address-space, nullability, or safety
  bit is accepted. A future such distinction must be added to the key before
  it can affect semantics. Optionality is represented by `Optional`, not a
  pointer flag.
- `Array(N, T)` is identified by the exact nonnegative `uint64` length and
  element `T`. Constant-expression evaluation supplies `N`; `05a` does not
  evaluate syntax. Target layout may later reject an otherwise representable
  length.
- `Slice(T)` is identified by element `T`. Ownership, lifetime, and runtime
  representation are intentionally not identity components because those
  language rules remain open.
- `Tuple([T0, ...])` is identified by arity, order, and every element. Pebble
  has no empty tuple type; `void` represents no value. `TupleKey([])` is an
  invalid key, and `Intern` returns an error matching `ErrInvalidKey` without
  changing the store. One-element and larger tuples are valid subject to the
  configured limits. Empty function parameter lists and empty nominal generic
  argument lists remain independently valid where those constructors permit
  them.
- `Optional(T)` is identified by payload `T`.
- `Function(C, [P0, ...], R, V)` is identified by calling convention, ordered
  parameter types, result, and variadic flag. Supported conventions are
  `Pebble` and `C`. Parameter names, `inline`, declaration identity, body,
  extern library spelling, and generated adapter names do not participate.
  Whether distinct conventions can convert or compare compatibly remains the
  phase-6 open decision; they are nevertheless distinct semantic types under
  the already accepted key in `09`.

The type store has no structural aggregate `TypeKey` variants. Pebble has no
anonymous aggregate types. An aggregate body creates semantic identity only
when it is the direct defining body of a named `TypeDecl`:

```pebble
type Point = struct { x int; };
```

This creates `Nominal(PointSymbol, [])`. A bare aggregate is semantically
invalid as a parameter, result, local annotation, tuple element, generic
argument, field type, or nested aggregate field. For example, both of these
must be rejected:

```pebble
fn use(value struct { x int; }) void { }
type Outer = struct { inner struct { value int; }; };
```

The parser may continue producing aggregate syntax nodes in general type
positions. Syntax acceptance does not imply semantic validity. `05b`
type-syntax resolution diagnoses invalid anonymous aggregate use with
source-driven fixtures; `05a` never interns such syntax.

Compiler-owned runtime records are nominal rather than primitive. `Allocator`
and the implicit runtime `Context` each use
`Nominal(runtime SymbolID, [])`; neither extends `BuiltinKind` or `Builtins`.
Their fields remain prepared declaration metadata outside the key exactly like
authored nominal fields. The store neither knows their spellings nor inserts
the hidden context ABI parameter into `FunctionKey` parameter lists.

## Nominal and declared types

One declaration receives one nominal identity:

```text
Nominal(declaration SymbolID, arguments [])
Nominal(declaration SymbolID, arguments [T0, ...])
```

This represents declared structs, enums, tagged unions, untagged unions, and
external opaque types. The declaration's checked semantic record, outside the
type store, records which category it is and its ordered fields, variants,
methods, and definitions. A category discriminator is intentionally absent
from `NominalKey`: `SymbolID` already selects exactly one declaration, and a
duplicate category risks disagreement.

Field or variant spelling and shape are declaration metadata, not nominal
identity. Declaration pointers, source addresses, module path strings,
qualified spellings, and generated backend names are also excluded. Two
same-spelled declarations have different `SymbolID`s and therefore different
types. References and aliases to one declaration reuse its ID.

A nominal ID may be interned as soon as `04b` has assigned its declaration
`SymbolID`, before fields, variants, or generic bodies are checked. This makes
recursive declarations finite: `Node` is first
`Nominal(NodeSymbol, [])`; a field such as `*Node` refers back through that
ID. Completing or diagnosing the declaration updates a separate declaration
result, never the nominal key. A damaged declaration may keep its nominal ID
for bounded recovery, but it cannot enter successful typed IR.

A generic application is `Nominal(GenericSymbol, ordered arguments)`. Arity,
argument order, and every argument `TypeID` participate. The declaration's
unapplied generic constructor is a `SymbolID`, not a `TypeID`; therefore
`Nominal(GenericSymbol, [])` is valid only for a genuinely zero-parameter
nominal declaration. Arity is validated before calling `Intern`.

## Type parameters and specialization boundary

Each declared type parameter already has its own `SymbolID` from `04b`.
`TypeParameter(ParameterSymbol)` is its stable symbolic semantic type. Two
generic declarations that both spell a parameter `T` remain unequal, as do
two parameters in one declaration. Spelling and ordinal are not identity.

Composite types may contain type-parameter IDs while a generic body is checked;
they are stable semantic types, not inference variables. For example the
symbolic type `*T` is an ordinary interned `Pointer(TypeParameter(TSymbol))`.
Substitution and specialization belong to phase 7. They recursively replace
declared parameter IDs with supplied argument IDs and intern the resulting
keys. Specialization caches use `(GenericSymbolID, ordered concrete TypeIDs,
relevant ABI options)` as specified in `07`; monomorphized names, AST clones,
and backend spellings never enter this store.

Generic function declarations and unapplied generic type declarations remain
declaration-level entities. A partially applied generic, an omitted argument,
or `_` awaiting inference is a phase-7/solver term, not a `TypeID`.

## Aliases and distinct types

The current language contract makes `type Alias = Existing` transparent.
Type-syntax resolution follows the alias and returns the target `TypeID`; an
alias does not receive `Nominal(AliasSymbol, ...)`, and alias chains collapse
to the same ID. Generic declaration aliases preserve the target declaration
identity, and a concrete alias such as `type Bytes = Vec[u8]` returns the
existing application ID. Alias cycles are diagnosed by type-syntax resolution
without interning an unresolved placeholder.

This describes the identity delivered to the store, not work performed by
`05a`. `05b` owns type-syntax and alias resolution, including alias-chain,
alias-cycle, generic-alias, and concrete-alias source fixtures. The type store
must not contain a partial resolver or compatibility helper.

Pebble has no distinct/newtype feature. Adding one would require new syntax and
a nominal `SymbolID` identity; the C prototype's duplicate objects or declared
names are not evidence that such identity exists. No unresolved alias/newtype
decision changes the `05a` key.

## Inference-facing boundary

Only stable semantic meanings enter the store:

| Entity | Representation |
| --- | --- |
| known concrete or symbolic declared type | canonical `TypeID` |
| declared generic parameter | `TypeParameter(SymbolID)` `TypeID` |
| inference variable | `05b` `Variable(InferID)` |
| exact integer literal value | `05b` `IntLiteral` term |
| exact floating literal value | `05b` `FloatLiteral` term |
| unknown or unresolved type | `05b` state or resolver error |
| recovery error | one `05b` `Error` term, not a `TypeID` |
| overloaded/capability-constrained value | `05b` constraints over terms |
| partial generic application | phase-7/`05b` term or diagnostic |
| compiler-owned Allocator or Context | `Nominal(runtime SymbolID, [])` |

Literal values are not interned, so their arbitrary-precision representation,
memory limits, fitting, and defaulting belong to `05b`. After constraints are
solved, an integer literal materializes as a known integer `TypeID` (default
`int` if unconstrained) and a float literal as `f32`/`f64` (default `f64`).
`LiteralFits` checks the retained value against that candidate. Negative syntax
remains unary negation handled by `05b`.

The solver's `Error` term is a single recovery sentinel per solver. Equality
with it suppresses cascades but does not make it a semantic type. Failed
compilations do not produce backend-consumable typed IR, so no error `TypeID`
is required. This preserves the existing `05` and `09` boundary.

## Queries required by consumers

The closed `Kind`/`Key` contract supplies only these decompositions:

- `05b` uses builtin kind for `Numeric`, `Integral`, `Ordered`, and
  `LiteralFits`; function convention/parameters/result/variadic for `Callable`;
  and recursive key decomposition for `Equal` and occurs/substitution work.
- Phase 6 uses pointer/array/slice/tuple/optional/function components for
  `Assignable` and conversions. `HasField` obtains only nominal declaration
  and generic arguments here, then queries checked declaration metadata by
  `SymbolID`; the type store does not own fields.
- Phase 7 uses `TypeParameter` declaration identity, nominal generic
  arguments, and structural decomposition for substitution and specialization
  keys.
- Typed IR stores final `TypeID`s and may use `Key` for validation and stable
  debug formatting implemented outside this package.
- Layout and lowering decompose keys and join nominal IDs to checked
  declaration records. Target layout remains a separate `LayoutOf(TypeID,
  TargetID)` query.
- The C backend consumes lowered types and calling conventions; its name
  mangler maps semantic IDs to names without writing them back.

These operations are enough to support `Equal`, `Numeric`, `Integral`,
`Ordered`, `Callable`, `HasField`, `Assignable`, `LiteralFits`, and the
checker/IR/backend consumers. Constraint generation, capability policy, field
lookup, layout, display formatting, and compatibility are deliberately not
store queries.

## Phase boundaries

`05a` provides:

- to `05b`: canonical builtins, immutable structural/nominal keys, interning,
  and decomposition;
- to phase 6: final equality and components for checking policy;
- to phase 7: parameter/declaration identity and canonical substitution
  results;
- to typed IR: snapshot-local final `TypeID`s;
- to layout: semantic components and nominal declaration joins;
- to lowering/code generation: stable type equality and function convention.

`05a` does not generate constraints; allocate or solve `InferID`s; unify;
default literals; resolve type syntax or aliases; look up names; validate
operators, calls, assignments, conversions, generic requirements, or
source-level function compatibility; resolve fields through receiver
inference; specialize generics; compute layouts; mutate syntax; or generate
display/backend names.

## Testing contract

Direct Go structural tests in `compiler/internal/types` are authoritative for:

- every preinterned primitive being valid, canonical, distinct, and returned
  by `Builtins`;
- repeated interning of every structural key returning one ID;
- `TupleKey([])` being rejected with `ErrInvalidKey` without changing `Len`,
  while one-element and larger tuple keys remain valid within limits;
- keys differing by tag, child, length, order, arity, convention, variadic
  flag, declaration, or generic argument remaining unequal;
- forced hash collisions still using complete equality;
- nested pointer/array/slice/tuple/optional/function decomposition;
- `Pebble` versus `C` function identity and variadic identity;
- nominal equality by `SymbolID`, including same spelling represented by two
  symbol IDs;
- predeclared recursive nominal IDs remaining unchanged after declaration
  metadata is checked elsewhere;
- generic nominal applications and type parameters using ordered stable IDs;
- the error-type boundary: direct `05a` tests prove there is no error
  builtin/key constructor; eventual `05b` tests prove solver recovery never
  calls `Intern` for its singleton `Error` term;
- mutating constructor inputs or accessor results not changing stored keys;
- identical ordered call streams producing identical IDs and `IDs` order
  across stores, including with forced hash seeds;
- each resource limit failing atomically and leaving `Len` unchanged;
- zero keys, unknown tags/enums, zero/out-of-range children and symbols, and
  excessive components returning stable errors;
- single-owner concurrency expectations documented and exercised under
  `go test -race` by the ordinary serialized API tests, not by unsupported
  concurrent mutation tests.

`05b` owns source-driven `.peb` fixtures for bare and nested anonymous
aggregates, alias chains, alias cycles, generic aliases, and concrete aliases.
Later resolver/checker suites own source fixtures for same-spelled nominals in
different modules, recursive declarations, generic applications, accepted
composite spellings, and calling conventions. None of these fixtures belongs
to the type-store package. Do not create golden files where direct `TypeID`
equality or key assertions are clearer.

## Implementation task and handoff

The implementing task owns:

```text
compiler/internal/types/id.go
compiler/internal/types/key.go
compiler/internal/types/store.go
compiler/internal/types/builtin.go
compiler/internal/types/*_test.go
```

It must not add a type or alias resolver, resolver integration fixtures,
compatibility wrappers around prototype `Type *`, a second type
representation, or production solver/checker code.

Completion requires the exact public contract above; immutable full-key
interning; all builtin, collision, nominal, generic, invalid-input, limit, and
determinism tests; no globals or host-pointer identity; and no generated names
or layouts in stored entries. Verify from `compiler/` with:

```sh
go test ./...
go test -race ./...
go vet ./...
```

Also run `git diff --check` from the repository root. The handoff reports the
public API, default limits, test-only collision mechanism, files, commands and
results, commit, and any contract discrepancy found in `05b` or later specs.

The nominal-only aggregate rule in this document is accepted language
behavior, not an implementation choice or open decision. Transparent aliases
still reuse their resolved target identity, but resolving them belongs to
`05b`. The open compatibility rule between calling conventions belongs to
phase 6 and must not be inferred by `05a`.
