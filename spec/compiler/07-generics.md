# Generics

## Goals

- Generic code should be easy to declare and normally require no call-site
  type arguments.
- Generic bodies should be checked meaningfully before arbitrary
  monomorphizations are encountered.
- Specialization must be deterministic, cached, and independent of AST
  mutation.
- Diagnostics should explain both the generic requirement and the call that
  failed to satisfy it.

## Type parameter scope

**Required:** type parameters declared by a type are automatically in scope for
its fields and methods.

```pebble
type Vec[T] = struct {
    data *T;

    fn push(self *Vec[T], value T) void { ... }
    fn map[U](self *Vec[T], f fn(T) U) Vec[U] { ... }
};
```

`push` does not redeclare `[T]`. `map` declares only its new parameter `[U]`.
This is simpler than the repeated `[T]` currently found throughout the
standard library and requires less generic-substitution machinery.

## Call-site inference

Argument types, receiver types, and the expected result type all contribute
constraints. These should compile without explicit specialization syntax when
the solution is unique:

```pebble
let x = identity(1);
var values Vec[int] = vec::new();
values.push(3);
```

Expected-result inference is important for zero-argument constructors.

## Explicit type arguments

Square brackets are used consistently for declarations, type use, construction,
and explicit call specialization. The prototype's separating dot is removed:

```pebble
Vec[int]
Vec[int]{ data = nil }
vec::new[int]()
value.map[str](convert)
```

Explicit arguments are not required when inference can solve the call. Square
brackets in expression position remain neutral in the surface tree until the
base is resolved. A generic base makes them type arguments; an indexable value
makes them an index operation:

```pebble
identity[int](52)
functions[i](52)
```

There is no special parenthesized spelling for calling an indexed function
value. The compiler absorbs the ambiguity through ordinary name and type
resolution rather than exposing a parser rule to the user.

Explicit instantiation is also a value and need not be called immediately:

```pebble
let parse_int = parse[int];
```

`_` may later stand for an inferred type argument:

```pebble
convert[_, str](value)
```

## Inferred generic requirements

Pebble can infer internal obligations from a generic body without first adding
a public trait system.

```pebble
fn max[T](a T, b T) T {
    if a > b { return a; }
    return b;
}
```

The body generates an obligation equivalent to `Ordered(T)`. The generic
definition stores this obligation. Each instantiation must prove that its
concrete type supports the operation.

This is implementable with the same constraint system used for ordinary type
inference. It provides useful auto-constraints, but it must obey two rules:

1. The body is checked symbolically once; it is not accepted merely because one
   observed instantiation happens to work.
2. Inferred obligations are part of the function's semantic interface and are
   available to diagnostics and caching.

A future named constraint/trait system can expose these obligations in source
syntax without replacing the core mechanism.

## Specialization

A specialization key is:

```text
(GenericSymbolID, ordered concrete TypeIDs, relevant ABI options)
```

The cache is populated with an in-progress entry before recursively checking a
body so recursive generic functions terminate. A specialization produces typed
IR; it never clones and rewrites the surface AST.

## Open generic decisions

- Whether every generic is monomorphized or some may use erased/runtime forms
- Whether inferred obligations are displayed in generated documentation
- Rules for specialization visibility and cross-module ownership
- Code-size controls and recursion limits
