# Checking and Conversions

## Checker responsibilities

The semantic checker:

- resolves names;
- validates declarations and scopes;
- generates and solves type constraints;
- validates operators, calls, indexing, fields, control flow, and entry points;
- records generic obligations;
- produces typed IR containing explicit coercions.

It does not mutate the surface AST and does not generate C names.

## Expression rule format

Every expression kind will receive a rule with:

1. required operand constraints;
2. synthesized result type;
3. whether it is an assignable place;
4. permitted implicit coercions;
5. runtime checks introduced during lowering;
6. diagnostics on failure.

The same format applies to statement and declaration rules.

## Bracket application

The surface parser leaves expression-position brackets neutral. After the base
symbol is resolved, semantic checking elaborates the node as exactly one of:

- generic instantiation, whose arguments must resolve as types and satisfy the
  declaration's arity and constraints; or
- indexing, whose single argument must be a value accepted by the base's index
  rule.

Calling the result is an independent postfix operation. Consequently both
`identity[int](value)` and `functions[i](value)` use their natural spelling.
Explicit instantiation may also produce a function value without an immediate
call, as in `let f = identity[int];`.

If future language features make a resolved base support both operations, the
program is ambiguous and requires an explicit language-level disambiguator;
the checker must not select an interpretation by heuristic.

## Place expressions

**Proposed:** identifier variables, dereferences, valid field accesses, and
valid indexing expressions are places. Constants, calls, casts, literals, and
temporary aggregate values are not assignable. Mutability is checked separately
from whether an expression denotes a place.

## Conversion classes

Conversions are divided into three classes:

### Identity and representation-preserving coercions

Permitted implicitly where documented. Candidates include:

- integer or float literal selection when the value fits;
- mutable reference/pointer to a compatible read-only form if constness is
  added;
- fixed array to slice view;
- `T` to `?T` by optional injection.

### Widening or potentially surprising numeric conversions

**Proposed:** do not implicitly convert between already-concrete numeric types
merely because one is wider. Literal fitting is not the same operation as
converting an `i32` expression to `i64`. Concrete numeric conversion requires
`as` unless a small, explicitly listed safe set is accepted later.

This rule prevents operator behavior from depending on a heuristic numeric
ranking.

### Unsafe or representation-changing conversions

Require explicit syntax and validation:

- integer/pointer conversion;
- pointer reinterpretation;
- signed/unsigned conversion;
- narrowing numeric conversion;
- float/integer conversion;
- opaque FFI conversions.

Some conversions may additionally require an `unsafe` facility if Pebble adds
one.

## Conversion matrix

Before implementation, this document must contain a matrix for every pair of
primitive type families and separate rules for aggregates, pointers, slices,
optionals, enums, unions, function types, and calling conventions. Each entry
is one of:

```text
identity | implicit | explicit | forbidden
```

## Operators

Operators generate capabilities and equality constraints rather than choosing
a winner based on operand order. For example, ordinary numeric binary
operators require both operands to solve to one compatible numeric type. Mixed
concrete types require an explicit cast unless the conversion matrix says
otherwise.

Equality, ordering, bitwise operations, logical operations, pointer arithmetic,
and string operations each require independent rules; “comparable” and
“numeric” are not universal shortcuts.

## Control flow

The checker records reachability and exit behavior. A function with a non-void
result must return on every reachable path. Loop, switch, break, continue, and
defer behavior must be modeled structurally rather than with file-global
Boolean flags.
