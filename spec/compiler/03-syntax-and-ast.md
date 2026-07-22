# Syntax and Surface Tree

This document defines the parser boundary. Detailed contracts are split by
concern:

- [grammar](03a-grammar.md);
- [surface-tree inventory and ownership](03b-surface-tree.md);
- [recovery and parser diagnostics](03c-parser-recovery.md);
- [implementation slices and conformance tests](03d-parser-slices.md).

## Parser responsibility

The parser consumes one immutable source file through the lexer and produces
one immutable surface tree plus structured diagnostics. It records syntax, not
meaning.

The parser does:

- recognize declarations, statements, expressions, types, and delimiters;
- preserve names and literal spellings through source spans;
- record source order and explicit grouping;
- construct missing and error nodes during documented recovery;
- preserve expression-position bracket application without classifying it as
  generic instantiation or indexing.

The parser does not:

- resolve names or modules;
- attach semantic types or symbols;
- infer generic arguments;
- validate places, conversions, control flow, or entry points;
- insert implicit casts, address operations, or dereferences;
- clone generic bodies or generate backend names.

## Source of truth

The EBNF grammar and parser contracts in this directory are normative. The C
`parser2.c`, standard library, examples, and old parser tests are inventory and
migration inputs. A disagreement does not silently change the grammar.

The first Go parser retains the currently supported statement and type forms so
the corpus can migrate phase by phase. Retention in the parser does not prevent
later language proposals from replacing `print`, either loop form, optional
construction, or record-literal spelling through an explicit spec change.

## Generic brackets

Generic declarations and type-required uses are syntactically known:

```pebble
fn identity[T](value T) T => value;
type Vec[T] = struct { data *T; };
var values Vec[int];
```

Square brackets after an expression remain neutral in the surface tree:

```pebble
identity[int](52)       // resolved later as generic instantiation
functions[i](52)       // resolved later as indexing, then a call
let f = identity[int];  // resolved later as a specialized function value
```

The parser produces the same bracket-application shape in all three cases. Name
and type resolution classify the base and interpret its arguments. The parser
never guesses from capitalization, argument spelling, a following `(`, or the
number of bracket arguments.

A bracket containing `:` is syntactically a slice and is not ambiguous:

```pebble
values[start:end]
values[:end]
values[start:]
```

## Surface decisions for the first parser

The first Go parser accepts these existing forms:

- type annotations remain `name Type`;
- semicolons terminate bindings, returns, assignments, expression statements,
  jumps, imports, extern items, fields, and type declarations;
- `print`, range `loop`, C-style `for`, `some`, and `none` remain supported;
- named record literals retain the unambiguous dot marker:
  `Point.{ x = 1, y = 2 }`;
- a generic record literal moves the dot after the new generic brackets:
  `Pair[int, str].{ first = 1, second = "one" }`;
- anonymous inferred record literals remain `.{ field = value }`;
- function result types remain explicit, including `void`.

The dot before a record body is not the removed generic `.[...]` separator. It
is retained because it distinguishes a record literal from the block following
an `if`, loop, or function header without semantic lookup or parser heuristics.

## Global invariants

- Parsing the same file and token stream produces the same tree and diagnostic
  order.
- Every loop in the parser consumes a token, inserts one documented missing
  construct, or returns.
- Every real node has one source file and a half-open byte span.
- Lists preserve source order and never rely on Go map iteration.
- A tree containing errors remains traversable without pretending its error
  nodes are valid syntax.
- Parser-local node IDs are not persistent cache IDs. Persistent identity is a
  later fingerprinting concern.
