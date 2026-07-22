# Token Inventory

This is the complete token inventory for the first Go lexer. Names shown in
code are conceptual Go enum names; source spelling is normative.

## Structural tokens

| Kind | Meaning |
|---|---|
| `EOF` | End of source |
| `Invalid` | Input was consumed and a lexer diagnostic was emitted |
| `Identifier` | Non-keyword identifier |
| `IntegerLiteral` | Decimal, hexadecimal, or binary integer literal |
| `FloatLiteral` | Decimal floating-point literal |
| `StringLiteral` | Ordinary quoted string |
| `CharacterLiteral` | Quoted character |
| `InterpolationStart` | Opening backtick |
| `InterpolationText` | Non-empty text inside an interpolated string |
| `InterpolationExprStart` | Unescaped `{` in interpolation text |
| `InterpolationExprEnd` | Matching `}` for an interpolation expression |
| `InterpolationEnd` | Closing backtick |

Empty interpolation text does not produce an `InterpolationText` token.

## Keywords

| Source | Kind |
|---|---|
| `as` | `KwAs` |
| `break` | `KwBreak` |
| `case` | `KwCase` |
| `context` | `KwContext` |
| `continue` | `KwContinue` |
| `defer` | `KwDefer` |
| `else` | `KwElse` |
| `enum` | `KwEnum` |
| `extern` | `KwExtern` |
| `false` | `KwFalse` |
| `fn` | `KwFn` |
| `for` | `KwFor` |
| `if` | `KwIf` |
| `import` | `KwImport` |
| `inline` | `KwInline` |
| `let` | `KwLet` |
| `loop` | `KwLoop` |
| `nil` | `KwNil` |
| `none` | `KwNone` |
| `print` | `KwPrint` |
| `return` | `KwReturn` |
| `sizeof` | `KwSizeof` |
| `some` | `KwSome` |
| `struct` | `KwStruct` |
| `switch` | `KwSwitch` |
| `true` | `KwTrue` |
| `type` | `KwType` |
| `union` | `KwUnion` |
| `var` | `KwVar` |
| `while` | `KwWhile` |

Keyword matching is exact and case-sensitive. `Fn`, `TRUE`, and `integer` are
identifiers.

## Punctuation

| Source | Kind |
|---|---|
| `(` | `LeftParen` |
| `)` | `RightParen` |
| `{` | `LeftBrace` |
| `}` | `RightBrace` |
| `[` | `LeftBracket` |
| `]` | `RightBracket` |
| `;` | `Semicolon` |
| `,` | `Comma` |
| `:` | `Colon` |
| `::` | `PathSeparator` |
| `.` | `Dot` |
| `..` | `Range` |
| `..=` | `RangeInclusive` |
| `...` | `Ellipsis` |
| `=>` | `FatArrow` |

## Operators

| Source | Kind |
|---|---|
| `+` | `Plus` |
| `-` | `Minus` |
| `*` | `Star` |
| `/` | `Slash` |
| `%` | `Percent` |
| `++` | `PlusPlus` |
| `--` | `MinusMinus` |
| `+=` | `PlusAssign` |
| `-=` | `MinusAssign` |
| `*=` | `StarAssign` |
| `/=` | `SlashAssign` |
| `%=` | `PercentAssign` |
| `=` | `Assign` |
| `==` | `Equal` |
| `!=` | `NotEqual` |
| `<` | `Less` |
| `<=` | `LessEqual` |
| `>` | `Greater` |
| `>=` | `GreaterEqual` |
| `&&` | `LogicalAnd` |
| `||` | `LogicalOr` |
| `!` | `Bang` |
| `?` | `Question` |
| `&` | `Ampersand` |
| `|` | `Pipe` |
| `^` | `Caret` |
| `~` | `Tilde` |
| `<<` | `ShiftLeft` |
| `>>` | `ShiftRight` |

`&=`, `|=`, `^=`, `<<=`, `>>=`, `->`, and `**` are not tokens in the initial
language. If adjacent valid tokens can be formed, the lexer emits those tokens
and leaves rejection to the parser; it does not invent one combined token.

## Removed prototype token distinctions

The new lexer does not have separate token kinds for every primitive type.
`int`, `uint`, exact-width numerics, `bool`, `char`, `str`, and `void` are
ordinary identifiers. The old generic `TOKEN_BOOL` distinction is also
unnecessary; `true` and `false` are explicit keyword tokens.
