# Literals and Interpolation

## Numeric separators

An underscore may appear only between two digits of the same digit sequence.
It cannot be leading, trailing, doubled, adjacent to a radix prefix, adjacent
to a decimal point, or adjacent to an exponent marker or sign.

Valid:

```pebble
1_000
0xff_ff
0b1010_0110
6.022_140_76e23
```

Invalid:

```pebble
_1       // identifier, not a number
1_
1__0
0x_ff
1_.0
1._0
1e_3
```

## Integer literals

```ebnf
decimal_digit = "0" … "9" ;
binary_digit = "0" | "1" ;
hex_digit = decimal_digit | "a" … "f" | "A" … "F" ;

decimal_digits = decimal_digit, { ["_"], decimal_digit } ;
binary_digits = binary_digit, { ["_"], binary_digit } ;
hex_digits = hex_digit, { ["_"], hex_digit } ;

decimal_integer = decimal_digits ;
binary_integer = ("0b" | "0B"), binary_digits ;
hex_integer = ("0x" | "0X"), hex_digits ;

integer_literal = decimal_integer | binary_integer | hex_integer ;
```

A radix prefix requires at least one digit. Once `0b` or `0x` begins a literal,
an invalid alphanumeric digit is part of one malformed numeric token and is
diagnosed as such; `0b102` is not silently split into `0b10` and `2`.

The lexer preserves spelling and does not force the value into a host integer.
The inference solver later parses an arbitrary-precision value, chooses a
concrete type from context, and reports range errors. `-` is always a separate
operator token.

There are no numeric suffixes. Use inference or an annotation:

```pebble
let inferred = 42;
let byte u8 = 42;
let exact i64 = 42;
```

## Floating-point literals

```ebnf
exponent = ("e" | "E"), ["+" | "-"], decimal_digits ;

float_literal =
    decimal_digits, ".", decimal_digits, [exponent]
  | decimal_digits, exponent ;
```

Both sides of a decimal point require digits. Write `0.5`, not `.5`, and
`1.0`, not `1.`. This keeps `.`, `..`, `..=`, and member syntax unambiguous.
Once `e` or `E` immediately follows decimal digits, it begins an exponent and
must be followed by the required exponent digits. `1e`, `1e+`, and `1.0e-` are
malformed floating literals rather than an integer followed by identifiers or
operators.

Valid:

```pebble
0.0
3.141_592
1e9
6.022e23
1.5e-8
```

Hexadecimal floats, `NaN`, and infinity have no literal spelling initially.
Unconstrained floating literals default to `f64`; an expected `f32` type can
select `f32` after finite-range checking and the specified IEEE rounding.

## Escape sequences

Ordinary strings, characters, and interpolation text share these escapes:

| Escape | Value |
|---|---|
| `\\` | Backslash |
| `\"` | Double quote |
| `\'` | Single quote |
| `\n` | Line feed |
| `\r` | Carriage return |
| `\t` | Horizontal tab |
| `\0` | Zero byte |
| `\xNN` | Byte with exactly two hexadecimal digits |
| `\u{H…}` | Unicode scalar with one through six hexadecimal digits |

Surrogate code points and values above `U+10FFFF` are invalid Unicode escapes.
Unknown and incomplete escapes are lexical errors. A backslash followed by a
physical line ending is not a continuation.

Interpolation text additionally accepts:

| Escape | Value |
|---|---|
| `\{` | Literal opening brace |
| `\`` | Literal backtick |

## Ordinary strings

An ordinary string begins and ends with `"`. It cannot contain an unescaped
physical line ending. Empty strings are valid. The token span includes both
quotes; decoding excludes them.

```pebble
""
"hello"
"line one\nline two"
"snowman: \u{2603}"
```

There is no raw or ordinary multiline string in the initial language.

## Character literals

A character literal begins and ends with `'` and decodes to exactly one Unicode
scalar value. Empty characters, multiple scalars, invalid escapes, and physical
line endings are lexical errors.

```pebble
'a'
'\n'
'\x7f'
'\u{2603}'
```

The semantic representation and C ABI of `char` are specified by the type and
runtime documents; the lexer only guarantees one decoded scalar.

## Interpolated strings

Interpolated strings are delimited by backticks. An unescaped `{` enters normal
expression lexing and the matching `}` returns to text mode.

```pebble
`hello`
`name: {name}`
`point: {{ x = 1, y = 2 }.x}`
`nested: {format(`value {x}`)}`
```

The conceptual token sequence for `` `a {x + 1} b` `` is:

```text
InterpolationStart
InterpolationText("a ")
InterpolationExprStart
Identifier("x")
Plus
IntegerLiteral("1")
InterpolationExprEnd
InterpolationText(" b")
InterpolationEnd
```

Expression mode tracks ordinary brace depth. A `}` ends interpolation only
when that depth returns to zero. Strings and nested interpolated strings inside
the expression manage their own delimiters through the lexer mode stack.

Interpolated strings may span physical lines. Text retains those line endings.
Use `\{` for a literal opening brace and `\`` for a literal backtick. An
unmatched interpolation brace or missing closing backtick is a lexical error.
