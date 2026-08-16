# pjson Specification

Status: Draft for implementation

Project: `pjson`

Language: Pebble

Reference design: jsmn

JSON standard: RFC 8259

JSON Pointer standard: RFC 6901

## 1. Purpose

`pjson` is a small, strict, zero-copy JSON parser for Pebble.

The parser follows the main design of jsmn:

- It scans the input once.
- It does not build a tree of allocated objects.
- It stores a flat array of tokens.
- Each token refers to a byte range in the original input.
- The caller supplies all parser storage.
- Parsing can continue after the caller supplies more input or more storage.
- The core parser has no dependency on the heap.

`pjson` is not a direct translation of jsmn. It uses jsmn's small token model,
but it applies strict RFC 8259 grammar and provides safe Pebble navigation and
decoding helpers.

The first useful product is a library. A small command-line program is also
part of the project because it gives the parser a real interface and makes
conformance testing easy.

## 2. Design rules

The following rules are normative.

1. The core parser must not allocate memory.
2. The parser must not copy string or number contents.
3. All token offsets are byte offsets into the original JSON input.
4. All token ranges are half-open: `start` is included and `end` is excluded.
5. The parser must accept every JSON text that RFC 8259 requires it to accept.
6. The parser must reject invalid JSON syntax.
7. The parser must not depend on null-terminated input.
8. The parser must support a JSON value of any type at the root.
9. The parser must report incomplete input separately from invalid input.
10. Storage exhaustion must be separate from invalid input.
11. A failed parse must report the byte offset at which failure was detected.
12. Public navigation helpers must validate token kinds and bounds.
13. The web, command-line, and test tools must use the same library parser.

## 3. Goals

The initial implementation must provide:

- strict JSON syntax validation;
- a flat, caller-owned token array;
- incremental parsing;
- object and array navigation;
- raw string and number access;
- JSON string decoding;
- integer, unsigned integer, and floating-point number conversion;
- compact and pretty JSON output;
- RFC 6901 JSON Pointer lookup;
- a command-line program;
- unit, regression, and external conformance tests.

The implementation must be suitable for:

- configuration files;
- command-line tools;
- HTTP request and response bodies;
- protocol messages;
- JSON validation;
- source inspection tools;
- later use by the Pebble compiler observatory.

## 4. Non-goals

The first version will not provide:

- a heap-owned JSON document tree;
- automatic conversion from JSON to arbitrary Pebble records;
- JSON Schema;
- JSON5;
- comments;
- trailing commas;
- unquoted object keys;
- NaN or infinity;
- arbitrary-precision numbers;
- streaming callbacks;
- in-place mutation of the source document;
- canonical JSON signing;
- JSON Patch or JSON Merge Patch.

These features can be separate projects or later layers. They must not make the
core parser larger or less predictable.

## 5. Required JSON behavior

### 5.1 JSON text

A JSON text contains exactly one JSON value, with optional JSON whitespace
before and after it.

The root can be:

- an object;
- an array;
- a string;
- a number;
- `true`;
- `false`;
- `null`.

After the root value, only JSON whitespace is valid. A second value or any
other byte is an error.

### 5.2 Whitespace

Only these four bytes are JSON whitespace:

- space (`0x20`);
- horizontal tab (`0x09`);
- line feed (`0x0A`);
- carriage return (`0x0D`).

Other Unicode spaces are not JSON whitespace.

### 5.3 Objects

An object has this grammar:

```text
object = '{' whitespace [member *(whitespace ',' whitespace member)] whitespace '}'
member = string whitespace ':' whitespace value
```

Object keys must be strings. A trailing comma is invalid.

Duplicate object names are valid JSON. The parser must preserve every member
and its source order. Lookup helpers use the first matching member unless a
helper explicitly states a different policy.

### 5.4 Arrays

An array has this grammar:

```text
array = '[' whitespace [value *(whitespace ',' whitespace value)] whitespace ']'
```

A trailing comma is invalid. Empty elements are invalid.

### 5.5 Literals

The only literals are:

```text
true
false
null
```

They are lowercase and must match exactly.

### 5.6 Numbers

A number has this grammar:

```text
number = ['-'] integer ['.' 1*digit] [('e' / 'E') ['+' / '-'] 1*digit]
integer = '0' / (digit-one-to-nine *digit)
```

The parser must reject:

- a leading plus sign;
- a leading zero followed by another integer digit;
- a missing integer part;
- a decimal point without a following digit;
- an exponent marker without exponent digits;
- hexadecimal notation;
- underscores;
- NaN;
- infinity.

The parser validates number grammar but does not decide whether the number fits
a machine type. Conversion helpers report overflow or underflow.

### 5.7 Strings

A JSON string starts and ends with `"`.

The valid escapes are:

```text
\"  \\  \/  \b  \f  \n  \r  \t  \uXXXX
```

The parser must reject:

- an unescaped byte from `0x00` through `0x1F`;
- an unknown escape;
- fewer than four hexadecimal digits after `\u`;
- invalid UTF-8 in unescaped string content;
- an unpaired UTF-16 surrogate escape;
- a low surrogate without a preceding high surrogate;
- a high surrogate that is not followed by a low surrogate.

A valid surrogate pair must decode to one Unicode scalar value.

Escaped solidus (`\/`) is accepted. Escaping a solidus is optional.

The raw string token range excludes the surrounding quotes and retains escape
sequences exactly as written. Decoding is a separate operation.

## 6. Token model

### 6.1 Token kinds

The core token kinds follow jsmn:

```pebble
type TokenKind = enum {
    Undefined;
    Object;
    Array;
    String;
    Primitive;
};
```

`Primitive` covers numbers, `true`, `false`, and `null`. This keeps the parser
token model small. Helpers can classify a primitive without changing the
stored token.

### 6.2 Token structure

The public token contract is:

```pebble
type Token = struct {
    kind TokenKind;
    start uint;
    end uint;
    size uint;
    parent int;
};
```

The exact final Pebble declaration can change if the language requires a small
syntax adjustment. The field meanings cannot change.

Field meanings:

- `kind` is the token kind.
- `start` is the first byte in the token payload.
- `end` is the first byte after the token payload.
- `size` is the number of direct logical children.
- `parent` is the parent token index, or `-1` for the root.

Range rules:

- Object and array ranges include their opening and closing delimiters.
- String ranges exclude their quotes.
- Primitive ranges contain exactly the primitive bytes.
- An unfinished token uses `end = 0` until it is complete.

Size rules:

- An array's size is its number of elements.
- An object's size is its number of key/value members.
- A string or primitive value has size zero.
- An object key is a string token. Its next sibling token is its value.

### 6.3 Flat order

Tokens are stored in source order and in depth-first order.

For this input:

```json
{"name":"Ada","active":true,"scores":[7,9]}
```

the tokens are conceptually:

| Index | Kind | Parent | Size | Raw payload |
|---:|---|---:|---:|---|
| 0 | Object | -1 | 3 | complete object |
| 1 | String | 0 | 0 | `name` |
| 2 | String | 0 | 0 | `Ada` |
| 3 | String | 0 | 0 | `active` |
| 4 | Primitive | 0 | 0 | `true` |
| 5 | String | 0 | 0 | `scores` |
| 6 | Array | 0 | 2 | complete array |
| 7 | Primitive | 6 | 0 | `7` |
| 8 | Primitive | 6 | 0 | `9` |

An implementation may keep internal parser metadata outside `Token`. It must
not expose internal state through token fields.

## 7. Parser state

The caller owns a reusable parser value.

```pebble
type Parser = struct {
    pos uint;
    next_token uint;
    root int;
    current_parent int;
    depth uint;
    complete bool;
};
```

The final implementation can add private fields. Public behavior is normative:

- `init` creates an empty parser.
- `reset` returns it to the initial state.
- parser state remains valid after incomplete input;
- parser state remains valid after token or nesting storage exhaustion;
- a caller can retry after it supplies a larger buffer;
- a syntax failure is terminal until `reset` is called.

Strict object and array grammar needs nesting state. The caller supplies a
frame array so the core does not allocate.

Each frame records:

- the open container token;
- whether it is an object or array;
- the next item that grammar permits;
- the current member count or element count.

The implementation must check frame capacity before it opens a deeper
container.

## 8. Parse result and errors

### 8.1 Parse status

```pebble
type ParseStatus = enum {
    Complete;
    Incomplete;
    NeedTokens;
    NeedFrames;
    Invalid;
};
```

Meanings:

- `Complete`: one complete JSON text was parsed.
- `Incomplete`: the current bytes form a valid prefix of a JSON text.
- `NeedTokens`: syntax is valid so far, but the token array is full.
- `NeedFrames`: syntax is valid so far, but the frame array is full.
- `Invalid`: the input cannot become a valid JSON text.

### 8.2 Error kinds

```pebble
type ErrorKind = enum {
    None;
    UnexpectedByte;
    UnexpectedEnd;
    TrailingContent;
    ExpectedKey;
    ExpectedColon;
    ExpectedValue;
    ExpectedCommaOrEnd;
    InvalidLiteral;
    InvalidNumber;
    InvalidEscape;
    InvalidUnicodeEscape;
    InvalidUtf8;
};
```

### 8.3 Result structure

```pebble
type ParseResult = struct {
    status ParseStatus;
    error ErrorKind;
    offset uint;
    token_count uint;
};
```

`offset` is the byte offset associated with the result:

- for `Invalid`, it is the first byte at which invalidity is known;
- for `Incomplete`, it is the end of available input;
- for `NeedTokens` or `NeedFrames`, it is the byte that could not be processed;
- for `Complete`, it is the first byte after the JSON text and trailing JSON
  whitespace.

No failure API may use only a Boolean value. The caller must be able to tell
incomplete input, invalid syntax, and insufficient storage apart.

## 9. Core parser API

The intended public interface is conceptually:

```pebble
fn parser_init() Parser;
fn parser_reset(parser *Parser) void;

fn parse(
    parser *Parser,
    source str,
    tokens []Token,
    frames []Frame,
) ParseResult;
```

The parser consumes all bytes currently present in `source`. For incremental
use, the caller must pass the same logical input with additional bytes
appended. Existing bytes before `parser.pos` must remain unchanged.

The parser must not retain a pointer to `source`, `tokens`, or `frames` after a
call. Only indices and counters can remain in `Parser`.

### 9.1 One-shot helper

A convenience helper can initialize a parser and parse once:

```pebble
fn parse_once(source str, tokens []Token, frames []Frame) ParseResult;
```

This helper does not allocate and does not hide storage exhaustion.

### 9.2 Token counting

The project must provide a way to determine the required token count without
allocating tokens. This can be a separate `count_tokens` function or a parse
mode with an empty token slice.

The behavior must be explicit:

- valid complete input returns the required token count;
- incomplete input returns `Incomplete`;
- invalid input returns `Invalid`;
- frame storage limits still apply unless the counting operation uses a proven
  allocation-free alternative.

## 10. Primitive classification

The library must classify a `Primitive` token as one of:

```pebble
type PrimitiveKind = enum {
    Number;
    True;
    False;
    Null;
};
```

Classification operates on the token's raw source bytes. The parser has
already validated the grammar, so classification must not rescan unrelated
input.

Helpers must include the equivalent of:

```pebble
fn primitive_kind(source str, token Token) PrimitiveKind;
fn is_null(source str, token Token) bool;
fn as_bool(source str, token Token, out *bool) ValueError;
```

Calling a primitive helper on the wrong token kind must return an error. It
must not silently invent a default value.

## 11. String access and decoding

### 11.1 Raw access

`raw_string` returns the exact bytes between the source quotes. It does not
unescape them.

Example:

```json
"line\nvalue"
```

Raw result:

```text
line\nvalue
```

Decoded result contains an actual line-feed byte.

### 11.2 Decoded length

The library must support a sizing pass:

```pebble
fn decoded_string_len(source str, token Token, out *uint) ValueError;
```

This returns the number of UTF-8 bytes required by the decoded string. It does
not include a null terminator.

### 11.3 Decode into caller storage

```pebble
fn decode_string(
    source str,
    token Token,
    output []u8,
    written *uint,
) ValueError;
```

Rules:

- The caller owns `output`.
- `written` is the number of decoded bytes.
- The function must report insufficient output capacity.
- It must not write outside the supplied slice.
- It must produce valid UTF-8.
- It must not append a null terminator unless a separate API promises one.

### 11.4 String comparison

Object lookup needs comparison without mandatory allocation. The library must
compare a decoded JSON key with a Pebble `str` value while it scans escapes.

```pebble
fn string_equals(source str, token Token, expected str) bool;
```

The comparison is byte-exact after JSON escape decoding. It does not apply
Unicode normalization or case folding.

## 12. Number conversion

Conversion helpers must include:

```pebble
fn as_i64(source str, token Token, out *i64) ValueError;
fn as_u64(source str, token Token, out *u64) ValueError;
fn as_f64(source str, token Token, out *f64) ValueError;
```

Required errors include:

```pebble
type ValueError = enum {
    None;
    WrongKind;
    InvalidTokenRange;
    OutputTooSmall;
    Overflow;
    Underflow;
    NotInteger;
    NotFound;
    InvalidPointer;
};
```

Rules:

- Integer conversion rejects a fraction or exponent unless its exact value can
  be accepted by a separately named conversion API.
- Signed conversion reports values outside the `i64` range.
- Unsigned conversion rejects negative values and reports values above the
  `u64` range.
- Floating-point conversion reports overflow.
- The implementation must not depend on the source being null-terminated.
- Locale must not change parsing. The decimal separator is always `.`.

## 13. Navigation

Navigation must use token indices. It must not return unstable raw pointers to
tokens.

Required operations:

```pebble
fn root_index(result ParseResult, out *uint) ValueError;
fn first_child(tokens []Token, index uint, out *uint) ValueError;
fn next_sibling(tokens []Token, index uint, out *uint) ValueError;
fn array_get(tokens []Token, array_index uint, element uint, out *uint) ValueError;
fn object_get(source str, tokens []Token, object_index uint, key str, out *uint) ValueError;
```

### 13.1 Subtree skipping

The library must provide an O(1) or amortized simple operation to find the token
after a subtree. It can use parent links, stored subtree spans, or a small
linear walk. The chosen behavior must be documented and tested.

The initial implementation can use a linear walk over descendants. It must not
use recursion.

### 13.2 Array lookup

`array_get`:

- requires an array token;
- uses a zero-based element index;
- returns `NotFound` when the index is outside the array;
- returns the token index of the complete element value.

### 13.3 Object lookup

`object_get`:

- requires an object token;
- compares decoded keys;
- returns the value token, not the key token;
- returns the first match when names are duplicated;
- returns `NotFound` when no member matches.

A later iterator API can expose every duplicate member. The parser must retain
enough information for that API.

## 14. JSON Pointer

The library must implement RFC 6901 lookup over parsed tokens.

```pebble
fn pointer_get(
    source str,
    tokens []Token,
    root uint,
    pointer str,
    out *uint,
) ValueError;
```

Required behavior:

- An empty pointer selects the root.
- A non-empty pointer must start with `/`.
- `~0` decodes to `~`.
- `~1` decodes to `/`.
- Any other `~` escape is invalid.
- An object segment selects a decoded member name.
- An array segment is a base-10 index.
- Array index `0` is valid.
- Other array indexes must not start with `0`.
- `-` does not select an existing array element and returns `NotFound`.
- Missing object names and array elements return `NotFound`.
- Pointer evaluation must not allocate memory for decoded segments.

URI fragment pointer syntax is outside the first version.

## 15. Formatting

Formatting operates from valid parsed JSON. It must not alter string contents,
number spelling, object member order, or duplicate names.

Two modes are required:

- compact;
- pretty.

### 15.1 Compact mode

Compact mode removes insignificant JSON whitespace and keeps all token payload
bytes unchanged.

### 15.2 Pretty mode

Pretty mode uses:

- two spaces per indentation level by default;
- one member or element per line for non-empty containers;
- one space after `:`;
- no trailing whitespace;
- one final line feed in command-line output.

The library formatter must accept an output sink or caller-owned buffer. It
must not require a single heap allocation proportional to the full document.

The first implementation may write into `std:string::String` if the no-heap
core parser remains independent from it.

## 16. Command-line program

The executable is named `pjson`.

Required commands:

```text
pjson validate [file]
pjson tokens [file]
pjson fmt [file]
pjson get POINTER [file]
```

If `file` is absent, input comes from standard input.

### 16.1 `validate`

Success:

- exit status `0`;
- no required standard output.

Invalid JSON:

- exit status `1`;
- a concise diagnostic on standard error;
- byte offset and error kind;
- line and column when they can be computed safely.

System or usage failure uses an exit status greater than `1`.

### 16.2 `tokens`

Print one stable, machine-readable token per line or print a documented JSON
token representation. The output must include:

- token index;
- kind;
- start;
- end;
- size;
- parent;
- escaped preview.

This command is a debugging and teaching interface. Its format must have tests.

### 16.3 `fmt`

Read, validate, and pretty-print the document. Invalid input must not produce a
partial successful output.

An explicit compact flag can select compact output.

### 16.4 `get`

Evaluate one RFC 6901 pointer and write the selected value as valid JSON.

Missing values return exit status `1`. Invalid pointers and invalid JSON must
produce distinct diagnostics.

## 17. Package layout

The intended project layout is:

```text
pjson/
  SPEC.md
  README.md
  src/
    pjson.peb
    parse.peb
    string.peb
    number.peb
    navigate.peb
    pointer.peb
    format.peb
    main.peb
  tests/
    unit/
    regression/
    integration/
    conformance/
  testdata/
    valid/
    invalid/
```

The exact file split can change when implementation exposes natural module
boundaries. The public API should remain available through one main `pjson`
module.

External test suites must be pinned by version or commit and must retain their
license files.

## 18. Test strategy

The test harness is part of the product. A parser without strong negative tests
is not complete.

### 18.1 Unit tests

Unit tests must cover:

- empty objects and arrays;
- nested objects and arrays;
- every root value type;
- all four whitespace bytes;
- every valid escape;
- BMP Unicode escapes;
- surrogate pairs;
- raw UTF-8 strings;
- minimum and maximum integer conversions;
- positive and negative exponents;
- parent indices;
- token sizes;
- source ranges;
- duplicate object names;
- object and array navigation;
- JSON Pointer escapes and indexes;
- compact and pretty formatting.

### 18.2 Invalid-input tests

Each grammar boundary needs a focused invalid test:

- empty input;
- whitespace-only input;
- two root values;
- missing object key;
- non-string object key;
- missing colon;
- missing value;
- missing comma;
- trailing comma;
- mismatched closing delimiter;
- unclosed container;
- unclosed string;
- raw control byte in a string;
- bad escape;
- incomplete Unicode escape;
- invalid hexadecimal digit;
- unpaired surrogate;
- invalid UTF-8;
- misspelled literal;
- leading plus;
- leading zero;
- incomplete fraction;
- incomplete exponent;
- trailing non-whitespace content.

Each test must assert the error kind and byte offset when the offset is stable.

### 18.3 Incremental tests

Every representative document must be split at every byte boundary.

For each split:

1. Parse the prefix.
2. Require `Incomplete`, `NeedTokens`, or `NeedFrames` as appropriate.
3. Supply the complete input or larger storage.
4. Resume the same parser.
5. Require the same final tokens as one-shot parsing.

Special split points include:

- after a backslash;
- inside `\uXXXX`;
- between UTF-8 bytes;
- between surrogate escapes;
- inside each literal;
- after a decimal point;
- after an exponent sign;
- before every closing delimiter.

### 18.4 Storage exhaustion tests

For each document with `N` tokens:

- capacities from zero through `N - 1` must report `NeedTokens` safely;
- capacity `N` must succeed;
- retry with larger storage must succeed;
- no token outside the reported count may be read;
- guard bytes around token and frame buffers must remain unchanged.

Frame capacity tests must cover every nesting depth from zero through the
required depth.

### 18.5 Translated jsmn tests

The project must translate jsmn's parser tests into Pebble. These tests protect
the flat-token and incremental-parser behavior that motivated this design.

If strict RFC 8259 behavior differs from permissive jsmn behavior, the pjson
expectation takes precedence and the test must explain the difference.

### 18.6 JSONTestSuite

The harness must run the external JSONTestSuite corpus.

Its conventional groups are:

- `y_`: the parser must accept;
- `n_`: the parser must reject;
- `i_`: behavior can depend on implementation policy.

For `i_` cases, pjson must keep an explicit expectation list. No case can be
silently ignored. The expectation list must explain policies such as invalid
UTF-8, extreme number range, and unpaired surrogates.

The harness must distinguish:

- accepted;
- rejected;
- process crash;
- timeout;
- harness failure.

A crash or timeout is always a test failure.

### 18.7 End-to-end tests

End-to-end tests must compile and run the Pebble program. They must cover:

- validation from a file;
- validation from standard input;
- token output;
- pretty formatting;
- compact formatting;
- pointer lookup;
- invalid input diagnostics;
- large input;
- deeply nested input within configured capacity.

Compiler checking alone is not sufficient proof.

### 18.8 Differential tests

Where behavior is defined by RFC 8259, the harness should compare acceptance
with at least one mature JSON implementation.

Differential tests are supporting evidence. The RFC and this specification are
the authority when implementations disagree.

### 18.9 Fuzz and property tests

When the Pebble tooling can support them, add these properties:

- parsing formatted valid JSON succeeds;
- compact output parses to the same token structure;
- pretty output parses to the same token structure;
- no input can cause an out-of-bounds token or frame access;
- random byte input terminates;
- a successful parse has exactly one root token;
- every non-root token has a valid earlier ancestor;
- every token range is inside the input range;
- decoded strings are valid UTF-8.

## 19. Limits and safety

The caller controls token and frame capacity. This is the primary resource
limit.

The parser must also support an explicit maximum nesting depth. It must never
use C or Pebble call-stack recursion for document nesting.

All arithmetic on offsets, lengths, token indices, and output sizes must check
for overflow before addition or multiplication.

The parser must not read one byte past `source.len`, including while it checks:

- literals;
- UTF-8 continuation bytes;
- escape sequences;
- Unicode escapes;
- exponent digits;
- closing delimiters.

Error reporting must not read the invalid byte when the offset equals the input
length.

## 20. Performance contract

The parser is single-pass over the source, apart from local rescans done by
requested access helpers.

Core parsing should be:

- O(input bytes);
- O(token count + maximum nesting depth) caller-owned storage;
- zero heap allocations;
- no recursion;
- no source copies.

Object lookup is O(number of members) in the first version. Array lookup can be
O(number of preceding elements). These costs are acceptable for the flat-token
model and must be documented.

Benchmarks must keep parsing separate from file I/O, token allocation, string
decoding, and formatting.

Initial benchmark documents should include:

- a small configuration object;
- a flat numeric array;
- a deeply nested document;
- a string-heavy document with escapes;
- a representative API response;
- a large generated document.

Performance work must not weaken syntax validation.

## 21. Implementation stages

### Stage 1: Token core

Implement:

- token kinds and token structure;
- parser initialization and reset;
- objects, arrays, strings, and primitives;
- strict structural grammar;
- token and frame exhaustion;
- one-shot parsing;
- exact token range tests.

Acceptance:

- simple valid and invalid documents compile and run;
- token arrays match expected values;
- no core parser heap allocation exists.

### Stage 2: Strict scalar validation

Implement:

- exact literals;
- strict number grammar;
- string escapes;
- UTF-8 validation;
- surrogate-pair validation;
- focused error kinds and offsets.

Acceptance:

- all focused scalar tests pass;
- no invalid scalar is accepted because a delimiter happened to follow it.

### Stage 3: Incremental parsing

Implement and prove:

- incomplete input;
- resume after more bytes;
- resume after more token storage;
- resume after more frame storage;
- split-at-every-byte tests.

Acceptance:

- incremental and one-shot parses produce identical tokens.

### Stage 4: Access and navigation

Implement:

- primitive classification;
- raw token access;
- decoded string length;
- caller-buffer string decoding;
- string comparison;
- integer and floating-point conversion;
- object and array lookup;
- sibling and child traversal.

Acceptance:

- every helper rejects wrong token kinds and invalid ranges;
- duplicate-name behavior is tested.

### Stage 5: JSON Pointer

Implement RFC 6901 lookup and its complete focused test matrix.

Acceptance:

- all RFC examples pass;
- invalid escapes and array indexes are rejected correctly.

### Stage 6: Formatting

Implement compact and pretty output.

Acceptance:

- formatted output reparses;
- raw scalar spelling and object order remain unchanged;
- golden output tests pass.

### Stage 7: Command-line program

Implement `validate`, `tokens`, `fmt`, and `get`.

Acceptance:

- file and standard-input paths work;
- exit statuses and diagnostics are stable;
- end-to-end tests compile and run the Pebble executable.

### Stage 8: External conformance

Add translated jsmn tests and JSONTestSuite.

Acceptance:

- all `y_` cases pass;
- all `n_` cases are rejected;
- every `i_` case has an explicit recorded policy;
- no case crashes or times out.

## 22. Definition of done

Version 1.0 is complete only when all of the following are true:

- The core parser is allocation-free.
- All token ranges and parent links have direct tests.
- Incomplete input is separate from invalid input.
- Storage exhaustion is separate from invalid input.
- Parsing can resume after more input or storage is supplied.
- Strict RFC 8259 object, array, number, literal, string, and whitespace grammar
  is enforced.
- Strings validate UTF-8 and surrogate pairs.
- Navigation and conversion APIs report errors without unsafe defaults.
- RFC 6901 lookup passes its conformance tests.
- Compact and pretty output reparse successfully.
- The command-line program works with files and standard input.
- Translated jsmn tests pass.
- JSONTestSuite required-accept and required-reject groups pass.
- The test harness detects crashes and timeouts.
- End-to-end tests compile and execute real Pebble programs.
- Public behavior is documented in `pjson/README.md`.

## 23. Decisions that are fixed

These decisions must not be reopened during the first implementation unless a
real Pebble limitation makes them impossible:

- The project name is `pjson`.
- The token model is flat and jsmn-like.
- The parser is strict RFC 8259, not permissive jsmn mode.
- The caller owns parser storage.
- Core parsing does not allocate.
- Tokens use byte offsets into the original input.
- Strings remain zero-copy until decoding is requested.
- Parent relationships are available.
- Duplicate names are preserved.
- A root scalar is valid.
- Incomplete input and invalid input are different results.
- The first object lookup result wins when names are duplicated.
- JSON Pointer follows RFC 6901.

## 24. Open implementation questions

These questions can be resolved during the first implementation without
changing the product design:

1. Whether parser frames are public or hidden behind a public storage wrapper.
2. Whether token counting is a dedicated function or a mode of `parse`.
3. Whether the formatter uses `std:string::String` first or a small output-sink
   interface.
4. Whether floating-point conversion uses a runtime helper or a Pebble
   implementation.
5. Whether token subtree spans should be stored for faster navigation.
6. Which exact module names best fit Pebble's import rules.

Each answer must be recorded in this specification before it becomes a public
API dependency.
