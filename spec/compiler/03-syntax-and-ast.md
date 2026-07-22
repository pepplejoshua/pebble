# Syntax and Surface AST

## Surface syntax currently represented

Declarations:

- functions and anonymous functions
- external functions, types, variables, constants, and extern blocks
- mutable `var` and immutable `let` declarations
- type declarations and aliases
- imports
- methods nested in struct and union declarations
- generic functions and generic types

Statements:

- return, if/else, while, range loop, C-style for, and block
- expression and assignment statements
- print, break, continue, switch/case, and defer

Expressions:

- integer, float, string, interpolated string, character, Boolean, nil, and
  none literals
- identifiers and context
- binary and unary operations
- calls, indexing, slicing, member access, and module-qualified access
- tuples, struct/union literals, arrays, and repeated arrays
- anonymous functions
- `sizeof`, explicit casts, optional construction, force unwrap, and postfix
  increment/decrement

Types:

- named and module-qualified named types
- pointer, optional, array, slice, struct, union, tagged union, enum, function,
  and tuple types

## AST contract

The parser produces an immutable syntax tree containing only syntactic facts:

- node kind
- source span
- names as written
- child nodes
- literal spelling or decoded value
- explicit syntax choices

The surface AST must not contain semantic types, resolved symbols, mangled
names, synthesized address/dereference nodes, implicit casts, or generated
generic instances.

## Grammar source

The final syntax specification must include an EBNF grammar. Until that is
written, `parser2.c`, the standard library, examples, and parser tests form the
inventory. They are not automatically authoritative when they disagree.

## Parser recovery

Recovery is specified separately from the grammar. A missing delimiter may be
synthesized only at a documented recovery boundary. An error node preserves
the damaged span so later tooling can continue without interpreting the node as
valid syntax.

## Generic syntax decision

Generic type arguments use square brackets consistently, without the
prototype's separating dot:

```pebble
Vec[int]
Vec[int]{ data = nil }
vec::new[int]()
value.map[str](convert)
```

Type arguments should normally be inferred. Explicit brackets are the escape
hatch when arguments, receiver type, and expected result type do not determine
a unique specialization.

## Bracket application

**Required:** the parser does not classify expression-position square brackets
as either generic instantiation or indexing. It produces a neutral bracket
application containing the base, argument syntax, and complete source span.
The surface tree preserves syntax; it does not use symbol lookup or token-shape
heuristics to choose a meaning.

Name and type resolution classify the node from the resolved base:

```pebble
identity[int](52)       // generic function instantiation and call
functions[i](52)       // index operation followed by a call
let f = identity[int];  // explicitly specialized function value
```

If the base denotes a generic declaration, bracket arguments are resolved as
type arguments. If the base denotes an indexable value, the bracket contains
an index expression. A base that remains ambiguous after normal lookup is an
error; the compiler never guesses from whether an argument happens to look
like a type name.

Type-required contexts such as `Vec[int]` already require a type and therefore
do not have the value-indexing ambiguity.

## Open syntax decisions

- Whether type annotations keep the current `name Type` form
- Whether `print` remains a statement or becomes a library function/macro
- Whether both range loops and C-style `for` loops remain
- Whether struct literals retain `Type.{ ... }`
- Whether `some value` / `none` remain the optional construction syntax
