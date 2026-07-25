# Source and Lexing

This document defines source storage, lexer behavior, and the boundary between
lexing and parsing. The complete token list is in
[Token inventory](02a-token-inventory.md). Literal grammars and interpolation
are in [Literals and interpolation](02b-literals-and-interpolation.md).

## Source files

**Required:** Pebble source is UTF-8. A UTF-8 byte-order mark is accepted only
at byte offset zero and is ignored. Invalid UTF-8 produces a source diagnostic
before lexing proceeds.

Every loaded file receives a `SourceID`. Source contents are immutable for one
compilation snapshot.

```go
type Span struct {
    Source SourceID
    Start  uint32 // byte offset, inclusive
    End    uint32 // byte offset, exclusive
}
```

Line and display-column information is derived by the source database. Tokens
and syntax nodes store spans, not copied filenames, source lines, or line and
column pairs. Diagnostic rendering accounts for UTF-8 and tabs when computing
display columns.

Line endings `\n` and `\r\n` are accepted. A bare `\r` is also treated as a
line ending so diagnostics remain well formed for old files.

## Lexer interface

Conceptually:

```go
type Token struct {
    Kind TokenKind
    Span Span
}

type Lexer struct {
    source SourceID
    offset uint32
    modes  []Mode
}

func (l *Lexer) Next() Token
```

Token spelling is obtained from the source database through the span. The
lexer does not allocate a copy of every identifier, keyword, operator, or
literal. Decoded literal values are computed later or stored in a separate
literal table only when needed.

`Next` always does one of the following:

- returns a non-empty token;
- returns EOF;
- reports an error, consumes the offending input, and returns an invalid token.

It must never report the same error at the same offset indefinitely.

## Identifier policy

**Required for the first implementation:**

```ebnf
identifier_start = "A" … "Z" | "a" … "z" | "_" ;
identifier_continue = identifier_start | "0" … "9" ;
identifier = identifier_start, { identifier_continue } ;
```

Identifiers are ASCII initially. UTF-8 remains valid in strings and comments.
An unsupported non-ASCII character outside a string or comment is diagnosed,
not silently split into bytes. Unicode identifiers can be added later as one
deliberate language-version change with an explicit normalization policy.

Keywords are recognized after scanning the entire identifier. The
implementation uses one keyword table rather than a first-character decision
tree.

Built-in type names are ordinary identifiers resolved from the predeclared
type scope:

```text
int uint i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 bool char str void
```

The lexer has no special token kind for each built-in type. These
predeclared names are reserved and cannot be shadowed; see
[04b Name resolution](04b-name-resolution.md).

## Whitespace and comments

The lexer discards:

- space;
- horizontal tab;
- line endings;
- line comments beginning with `//` and continuing up to, but not including,
  the line ending or EOF.

Comments do not nest because block comments are not part of the initial
language. `/*` is tokenized as `/` followed by `*`. Documentation comments and
nested block comments may be added later without changing ordinary line-comment
behavior.

## Longest match

Operators and punctuation use maximal munch: at a given byte offset, the lexer
emits the longest valid token. Examples:

```text
... before .. before .
..= before ..
>> before >
>= before >
== and => before =
// comment before /
```

Generic square brackets require no lexer mode or token distinction. The parser
preserves bracket application without guessing whether it is generic
instantiation or indexing; name and type resolution decide from the base.

## Lexer modes

The lexer uses a small mode stack:

```text
Normal
InterpolatedText(delimiter offset)
InterpolationExpression(brace depth)
```

Modes replace the prototype's recursive public lexer calls and queued tokens.
The mode stack allows nested braces and nested interpolated strings while
preserving ordinary left-to-right token production.

## Error ownership

The lexer diagnoses only lexical failures:

- invalid UTF-8;
- unexpected source characters;
- malformed numeric spelling;
- unterminated or malformed string and character literals;
- invalid escapes;
- unterminated interpolation text or expression.

It does not diagnose unknown names, unsupported operators formed from multiple
otherwise-valid tokens, literal range overflow for a selected semantic type,
or grammar errors.

## Deliberate first-version exclusions

- Unicode identifiers
- numeric type suffixes
- raw strings
- ordinary multiline quoted strings
- block comments
- octal literals

Each exclusion can be added independently later. None is needed to make the
initial lexer or type inference coherent.
