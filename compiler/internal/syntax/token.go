package syntax

import "github.com/pepplejoshua/pebble/compiler/internal/source"

// TokenKind identifies one lexical token.
type TokenKind uint16

const (
	EOF TokenKind = iota
	Invalid
	Identifier
	IntegerLiteral
	FloatLiteral
	StringLiteral
	CharacterLiteral
	InterpolationStart
	InterpolationText
	InterpolationExprStart
	InterpolationExprEnd
	InterpolationEnd

	KwAs
	KwBreak
	KwCase
	KwContext
	KwContinue
	KwDefer
	KwElse
	KwEnum
	KwExtern
	KwFalse
	KwFn
	KwFor
	KwIf
	KwImport
	KwInline
	KwLet
	KwLoop
	KwNil
	KwNone
	KwPrint
	KwReturn
	KwSizeof
	KwSome
	KwStruct
	KwSwitch
	KwTrue
	KwType
	KwUnion
	KwVar
	KwWhile

	LeftParen
	RightParen
	LeftBrace
	RightBrace
	LeftBracket
	RightBracket
	Semicolon
	Comma
	Colon
	PathSeparator
	Dot
	Range
	RangeInclusive
	Ellipsis
	FatArrow

	Plus
	Minus
	Star
	Slash
	Percent
	PlusPlus
	MinusMinus
	PlusAssign
	MinusAssign
	StarAssign
	SlashAssign
	PercentAssign
	Assign
	Equal
	NotEqual
	Less
	LessEqual
	Greater
	GreaterEqual
	LogicalAnd
	LogicalOr
	Bang
	Question
	Ampersand
	Pipe
	Caret
	Tilde
	ShiftLeft
	ShiftRight
)

var tokenKindNames = [...]string{
	EOF:                    "end of file",
	Invalid:                "invalid token",
	Identifier:             "identifier",
	IntegerLiteral:         "integer literal",
	FloatLiteral:           "floating literal",
	StringLiteral:          "string literal",
	CharacterLiteral:       "character literal",
	InterpolationStart:     "interpolation start",
	InterpolationText:      "interpolation text",
	InterpolationExprStart: "interpolation expression start",
	InterpolationExprEnd:   "interpolation expression end",
	InterpolationEnd:       "interpolation end",
	KwAs:                   "as",
	KwBreak:                "break",
	KwCase:                 "case",
	KwContext:              "context",
	KwContinue:             "continue",
	KwDefer:                "defer",
	KwElse:                 "else",
	KwEnum:                 "enum",
	KwExtern:               "extern",
	KwFalse:                "false",
	KwFn:                   "fn",
	KwFor:                  "for",
	KwIf:                   "if",
	KwImport:               "import",
	KwInline:               "inline",
	KwLet:                  "let",
	KwLoop:                 "loop",
	KwNil:                  "nil",
	KwNone:                 "none",
	KwPrint:                "print",
	KwReturn:               "return",
	KwSizeof:               "sizeof",
	KwSome:                 "some",
	KwStruct:               "struct",
	KwSwitch:               "switch",
	KwTrue:                 "true",
	KwType:                 "type",
	KwUnion:                "union",
	KwVar:                  "var",
	KwWhile:                "while",
	LeftParen:              "(",
	RightParen:             ")",
	LeftBrace:              "{",
	RightBrace:             "}",
	LeftBracket:            "[",
	RightBracket:           "]",
	Semicolon:              ";",
	Comma:                  ",",
	Colon:                  ":",
	PathSeparator:          "::",
	Dot:                    ".",
	Range:                  "..",
	RangeInclusive:         "..=",
	Ellipsis:               "...",
	FatArrow:               "=>",
	Plus:                   "+",
	Minus:                  "-",
	Star:                   "*",
	Slash:                  "/",
	Percent:                "%",
	PlusPlus:               "++",
	MinusMinus:             "--",
	PlusAssign:             "+=",
	MinusAssign:            "-=",
	StarAssign:             "*=",
	SlashAssign:            "/=",
	PercentAssign:          "%=",
	Assign:                 "=",
	Equal:                  "==",
	NotEqual:               "!=",
	Less:                   "<",
	LessEqual:              "<=",
	Greater:                ">",
	GreaterEqual:           ">=",
	LogicalAnd:             "&&",
	LogicalOr:              "||",
	Bang:                   "!",
	Question:               "?",
	Ampersand:              "&",
	Pipe:                   "|",
	Caret:                  "^",
	Tilde:                  "~",
	ShiftLeft:              "<<",
	ShiftRight:             ">>",
}

func (k TokenKind) String() string {
	if int(k) >= len(tokenKindNames) || tokenKindNames[k] == "" {
		return "unknown token"
	}
	return tokenKindNames[k]
}

// Token references its spelling through an immutable source span.
type Token struct {
	Kind TokenKind
	Span source.Span
}
