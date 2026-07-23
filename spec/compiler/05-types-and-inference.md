# Types and Inference

Phase 5 is split into two implementation-ready task contracts:

- [05a Semantic Type Store](05a-semantic-type-store.md) defines the immutable,
  compilation-owned `TypeID` store, complete semantic keys, and decomposition
  API.
- `05b Algebraic Inference` will define expression inference variables,
  equation and capability generation, deterministic unification, worklist
  solving, literal fitting/defaulting, and error recovery.

`05a` is the required foundation for `05b`. The overview below states the
language-level contract; the task documents own implementation detail.

## Semantic type universe

Pebble represents:

- target-word integers: `int` and `uint`
- exact-width signed integers: `i8`, `i16`, `i32`, `i64`
- exact-width unsigned integers: `u8`, `u16`, `u32`, `u64`
- exact-width floating point: `f32` and `f64`
- `bool`, `char`, `str`, and `void`
- pointers, arrays, slices, tuples, optionals, functions
- structs, untagged unions, tagged unions, enums, and opaque external types
- generic functions and generic type declarations

## Numeric type meanings

**Required:**

- `int` is the target's native signed word type: 32 bits on a 32-bit target and
  64 bits on a 64-bit target.
- `uint` is the corresponding target-native unsigned word type.
- `i8`, `i16`, `i32`, and `i64` have exactly the stated signed width.
- `u8`, `u16`, `u32`, and `u64` have exactly the stated unsigned width.
- `f32` is an IEEE 754 binary32 value.
- `f64` is an IEEE 754 binary64 value.
- `f64` is the default concrete type for an otherwise-unconstrained floating
  literal.

`isize`, `usize`, and `float` are removed. They would duplicate `int`, `uint`,
and `f64` without adding a semantic distinction. The backend must not define
`int` by mapping it to C `int`; it uses the target description and exact C
types such as `intptr_t`, `uintptr_t`, and the `<stdint.h>` integer types.

C ABI types are modeled explicitly when needed and are not inferred from
Pebble's convenient numeric names.

## Type identity

Every semantic type is interned and addressed by `TypeID`. Structural types
are equal when their canonical components are equal. Named nominal types are
equal according to an explicit alias/newtype rule; their printed names are not
their identity.

Aliases resolve to an existing type identity. A future distinct/newtype
feature would create a new nominal identity and must use separate syntax.

## Constraint-based inference

Pebble can implement its own solver without a dependency. The minimum design
is a union-find unifier plus a deterministic worklist of constraints.

For each expression, checking creates a type variable or known type. It then
emits constraints such as:

```text
Equal(a, b)
Numeric(t)
Integral(t)
Ordered(t)
Callable(fnType, argTypes, resultType)
HasField(receiverType, name, fieldType)
Assignable(sourceType, destinationType)
LiteralFits(literal, type)
```

Example:

```pebble
let x = choose() + 1;
```

Conceptually produces:

```text
type(choose()) = A
type(1) = B where B is an integer-literal type
Numeric(A)
Numeric(B)
A = B
type(x) = A
```

The solver processes these constraints until no progress remains. Source order
does not decide which operand dictates the other operand's type.

## Literal types

Integer and float literals initially have inference-level literal types, not
concrete machine types:

```text
IntLiteral(value)
FloatLiteral(value)
```

An expected type may select their concrete type if the value fits. If no
context constrains a literal, integer literals default to `int` and floating
literals default to `f64`.

Overflow is checked against the selected type. A negative literal is unary
negation applied to a positive literal, and its minimum-value edge case must be
handled deliberately.

## Bidirectional checking

Inference uses information in both directions:

- synthesize a type from an expression when no expectation exists;
- check an expression against an expected type for assignments, arguments,
  returns, and annotated declarations.

Constraint solving and bidirectional checking complement each other. Expected
types reduce ambiguity; the solver prevents incidental traversal order from
becoming semantics.

## Solver failure

An unsatisfied constraint reports the smallest useful explanation at its
origin. The solver uses an `Error` type to suppress cascades. It must not choose
an arbitrary type merely to make progress.

## Inference types and concrete interning

Inference state is not stored in the concrete type interner. During solving, a
type position is one of:

```text
Known(TypeID)
Variable(InferID)
IntLiteral(value)
FloatLiteral(value)
Error
```

Only after a position is fully resolved does its structural type enter the
`TypeStore`. Unresolved placeholders are never interned and an interned type is
never mutated into a different kind. The `Error` term is solver-owned recovery
state, not a canonical `TypeID`. See
[05a Semantic Type Store](05a-semantic-type-store.md) for the exact identity
and interning boundary.
