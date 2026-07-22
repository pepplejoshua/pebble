# Parser Recovery and Diagnostics

Recovery exists to produce useful diagnostics and a traversable surface tree.
It must not reinterpret malformed code as a different valid program.

## Diagnostic codes

Parser codes are declared once as typed constants in `internal/syntax`, just as
lexer codes are. Initial categories are:

| Code | Constant | Meaning |
| --- | --- | --- |
| `P0001` | `codeExpectedDeclaration` | expected a top-level declaration |
| `P0002` | `codeExpectedName` | expected an identifier/name |
| `P0003` | `codeExpectedExpression` | expected an expression or operand |
| `P0004` | `codeExpectedType` | expected type syntax |
| `P0005` | `codeExpectedToken` | missing delimiter, separator, or terminator |
| `P0006` | `codeInvalidSyntax` | tokens form a forbidden syntax shape |
| `P0007` | `codeNestingLimit` | parser nesting limit exceeded |

The stable code identifies the category. The message names the concrete token
or construct, for example `expected ')' after function parameters`. Tests
assert codes and use goldens only when exact messages, labels, or recovery order
matter.

## Result contract

- A syntax error appends a diagnostic and produces a `Missing` or `Error` node
  where possible.
- Parsing continues until EOF, the diagnostic limit, or the nesting limit.
- A file with errors still returns a tree.
- Parser diagnostics never erase lexer diagnostics.
- An `Invalid` lexer token is already diagnosed. The parser consumes it without
  emitting a duplicate parser diagnostic for the same lexical failure, then
  recovers according to its surrounding construct.

## Progress invariant

Every recovery path must do at least one of:

1. consume a real token;
2. insert one missing construct and return to its caller;
3. unwind to a caller that owns the current synchronization token;
4. stop at EOF or a configured limit.

A loop records its cursor before parsing an element. If neither the cursor nor
the element count changes, the loop emits an internal-test failure; production
code forcibly consumes one token as an error node. No source input may hang the
parser.

## Missing-token insertion

A missing closing delimiter or semicolon may be inserted only when the current
token is already a valid follower of the construct. Examples:

- insert `)` before a function result type, `{`, `=>`, `;`, or EOF;
- insert `]` before a postfix continuation, separator, terminator, or EOF;
- insert `;` before `}`, EOF, or an unambiguous next statement/declaration;
- insert `}` at EOF or when an outer construct's closer is reached.

Insertion emits `P0005` and creates a zero-width `Missing` node/token. The
parser does not consume the follower. A missing opener is not silently
inserted; the parser reports it and synchronizes because ownership of following
tokens would otherwise be unclear.

## Local list recovery

Comma-separated lists recover to their comma or closing delimiter:

```text
parameters       -> ',' or ')'
arguments        -> ',' or ')'
bracket arguments-> ',' or ']'
array elements   -> ',' or ']'
record fields    -> ',' or '}'
enum variants    -> ',' or '}'
```

An invalid element becomes one `Error` child. Recovery must not discard valid
elements following the next separator.

## Expression recovery

When an operand is missing before a known expression follower such as `,`,
`;`, `)`, `]`, `}`, interpolation end, or `:`, the parser inserts a
`Missing` expression and returns without consuming the follower.

When unexpected tokens occur inside an expression, it consumes them into one
`Error` node until it reaches an operator or follower valid for the current
precedence level. It does not synchronize all the way to the next statement
unless the enclosing statement also fails.

Non-associative comparison/equality chains produce `P0006` at the second
operator and preserve the full damaged expression in an error node.

## Statement and declaration recovery

Block synchronization stops before:

- `}` or EOF;
- a statement starter (`return`, `if`, `while`, `loop`, `for`, `switch`,
  `defer`, `print`, `break`, `continue`, `{`);
- a local binding starter (`let`, `var`);
- an expression starter after a consumed semicolon.

Top-level synchronization stops before `fn`, `extern`, `let`, `var`, `type`,
`import`, or EOF. A synchronization routine may consume a stray semicolon but
never consumes a closing brace owned by its caller.

Function-header recovery uses `)`, result-type starters, `{`, and `=>` as local
anchors. Aggregate-body recovery uses `;`, `fn`, and `}`. Switch recovery uses
`case`, `else`, and `}`.

## Cascades and limits

- Do not emit the same code at the same byte offset more than once.
- Prefer the innermost concrete failure; callers do not add generic
  `expected statement` errors when a child already diagnosed the position.
- The default diagnostic limit is 50 parser errors per file. Reaching it emits
  one final note and stops parsing new constructs.
- The default syntactic nesting limit is 256. Exceeding it emits `P0007`,
  consumes to a safe enclosing boundary, and never recurses further.
- Limits are deterministic and test-configurable, not package globals.

## Recovery acceptance tests

Recovery tests assert more than non-crashing behavior:

- diagnostics appear in source order with the expected codes;
- declarations/statements after the damaged construct remain in the tree;
- missing and error nodes occupy the expected parent position;
- EOF and nested delimiters terminate in bounded time;
- rerunning the parser yields an identical tree dump and diagnostics.
