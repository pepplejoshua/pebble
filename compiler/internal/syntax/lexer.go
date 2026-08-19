package syntax

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

type lexerModeKind uint8

const (
	modeNormal lexerModeKind = iota
	modeInterpolatedText
	modeInterpolationExpression
)

type lexerMode struct {
	kind       lexerModeKind
	opening    uint32
	braceDepth int
}

// Lexer produces tokens from one immutable source file.
type Lexer struct {
	file        *source.File
	diagnostics *diagnostic.DiagnosticSet
	text        []byte
	offset      uint32
	modes       []lexerMode
}

// NewLexer creates a lexer positioned at the beginning of file.
func NewLexer(file *source.File, diagnostics *diagnostic.DiagnosticSet) *Lexer {
	return &Lexer{
		file:        file,
		diagnostics: diagnostics,
		text:        file.Text(),
		modes:       []lexerMode{{kind: modeNormal}},
	}
}

// Next returns the next token. It always advances unless it returns EOF.
func (l *Lexer) Next() Token {
	mode := l.topMode()
	if mode.kind == modeInterpolatedText {
		return l.lexInterpolatedText()
	}
	return l.lexCode(mode.kind == modeInterpolationExpression)
}

func (l *Lexer) lexCode(inInterpolation bool) Token {
	l.skipWhitespaceAndComments()
	start := l.offset

	if l.atEnd() {
		if inInterpolation {
			opening := l.topMode().opening
			l.resetModes()
			return l.invalid(opening, l.offset, codeUnterminatedInterpolationExpr, "unterminated interpolation expression")
		}
		return l.token(EOF, start, start)
	}

	b := l.peek(0)
	if isIdentifierStart(b) {
		return l.lexIdentifier()
	}
	if isDecimalDigit(b) {
		return l.lexNumber()
	}

	if b >= utf8.RuneSelf {
		_, size := utf8.DecodeRune(l.text[l.offset:])
		l.offset += uint32(size)
		return l.invalid(start, l.offset, codeUnsupportedCharacter, "non-ASCII characters are not allowed in identifiers")
	}

	if inInterpolation {
		switch b {
		case '{':
			l.offset++
			l.modes[len(l.modes)-1].braceDepth++
			return l.token(LeftBrace, start, l.offset)
		case '}':
			l.offset++
			modeIndex := len(l.modes) - 1
			if l.modes[modeIndex].braceDepth == 0 {
				l.modes = l.modes[:modeIndex]
				return l.token(InterpolationExprEnd, start, l.offset)
			}
			l.modes[modeIndex].braceDepth--
			return l.token(RightBrace, start, l.offset)
		}
	}

	l.offset++
	switch b {
	case '`':
		l.modes = append(l.modes, lexerMode{kind: modeInterpolatedText, opening: start})
		return l.token(InterpolationStart, start, l.offset)
	case '(':
		return l.token(LeftParen, start, l.offset)
	case ')':
		return l.token(RightParen, start, l.offset)
	case '{':
		return l.token(LeftBrace, start, l.offset)
	case '}':
		return l.token(RightBrace, start, l.offset)
	case '[':
		return l.token(LeftBracket, start, l.offset)
	case ']':
		return l.token(RightBracket, start, l.offset)
	case ';':
		return l.token(Semicolon, start, l.offset)
	case ',':
		return l.token(Comma, start, l.offset)
	case ':':
		if l.take(':') {
			return l.token(PathSeparator, start, l.offset)
		}
		return l.token(Colon, start, l.offset)
	case '.':
		if l.take('.') {
			if l.take('.') {
				return l.token(Ellipsis, start, l.offset)
			}
			if l.take('=') {
				return l.token(RangeInclusive, start, l.offset)
			}
			return l.token(Range, start, l.offset)
		}
		return l.token(Dot, start, l.offset)
	case '+':
		if l.take('+') {
			return l.token(PlusPlus, start, l.offset)
		}
		if l.take('=') {
			return l.token(PlusAssign, start, l.offset)
		}
		return l.token(Plus, start, l.offset)
	case '-':
		if l.take('-') {
			return l.token(MinusMinus, start, l.offset)
		}
		if l.take('=') {
			return l.token(MinusAssign, start, l.offset)
		}
		return l.token(Minus, start, l.offset)
	case '*':
		if l.take('=') {
			return l.token(StarAssign, start, l.offset)
		}
		return l.token(Star, start, l.offset)
	case '/':
		if l.take('=') {
			return l.token(SlashAssign, start, l.offset)
		}
		return l.token(Slash, start, l.offset)
	case '%':
		if l.take('=') {
			return l.token(PercentAssign, start, l.offset)
		}
		return l.token(Percent, start, l.offset)
	case '=':
		if l.take('=') {
			return l.token(Equal, start, l.offset)
		}
		if l.take('>') {
			return l.token(FatArrow, start, l.offset)
		}
		return l.token(Assign, start, l.offset)
	case '!':
		if l.take('=') {
			return l.token(NotEqual, start, l.offset)
		}
		return l.token(Bang, start, l.offset)
	case '<':
		if l.take('<') {
			return l.token(ShiftLeft, start, l.offset)
		}
		if l.take('=') {
			return l.token(LessEqual, start, l.offset)
		}
		return l.token(Less, start, l.offset)
	case '>':
		if l.take('>') {
			return l.token(ShiftRight, start, l.offset)
		}
		if l.take('=') {
			return l.token(GreaterEqual, start, l.offset)
		}
		return l.token(Greater, start, l.offset)
	case '&':
		if l.take('&') {
			return l.token(LogicalAnd, start, l.offset)
		}
		return l.token(Ampersand, start, l.offset)
	case '|':
		if l.take('|') {
			return l.token(LogicalOr, start, l.offset)
		}
		return l.token(Pipe, start, l.offset)
	case '^':
		return l.token(Caret, start, l.offset)
	case '~':
		return l.token(Tilde, start, l.offset)
	case '?':
		return l.token(Question, start, l.offset)
	case '"':
		return l.lexString(start)
	case '\'':
		return l.lexCharacter(start)
	default:
		return l.invalid(start, l.offset, codeUnsupportedCharacter, fmt.Sprintf("unexpected character %q", b))
	}
}

func (l *Lexer) lexIdentifier() Token {
	start := l.offset
	l.offset++
	for !l.atEnd() && isIdentifierContinue(l.peek(0)) {
		l.offset++
	}
	text := string(l.text[start:l.offset])
	if kind, ok := keywords[text]; ok {
		return l.token(kind, start, l.offset)
	}
	return l.token(Identifier, start, l.offset)
}

func (l *Lexer) lexNumber() Token {
	start := l.offset

	if l.peek(0) == '0' && (l.peek(1) == 'x' || l.peek(1) == 'X') {
		l.offset += 2
		return l.lexRadixNumber(start, isHexDigit, "hexadecimal")
	}
	if l.peek(0) == '0' && (l.peek(1) == 'b' || l.peek(1) == 'B') {
		l.offset += 2
		return l.lexRadixNumber(start, isBinaryDigit, "binary")
	}

	digitsStart := l.offset
	l.scanDecimalSequence()
	malformed := !validSeparatedDigits(l.text[digitsStart:l.offset], isDecimalDigit)
	kind := IntegerLiteral

	if l.peek(0) == '.' && l.peek(1) == '_' {
		l.offset += 2
		for isIdentifierContinue(l.peek(0)) {
			l.offset++
		}
		return l.invalid(start, l.offset, codeMalformedNumericLiteral, "malformed floating-point literal")
	}

	if l.peek(0) == '.' && isDecimalDigit(l.peek(1)) {
		kind = FloatLiteral
		l.offset++
		fractionStart := l.offset
		l.scanDecimalSequence()
		malformed = malformed || !validSeparatedDigits(l.text[fractionStart:l.offset], isDecimalDigit)
	}

	if l.peek(0) == 'e' || l.peek(0) == 'E' {
		kind = FloatLiteral
		l.offset++
		if l.peek(0) == '+' || l.peek(0) == '-' {
			l.offset++
		}
		exponentStart := l.offset
		l.scanDecimalSequence()
		malformed = malformed || !validSeparatedDigits(l.text[exponentStart:l.offset], isDecimalDigit)
	}

	if malformed {
		return l.invalid(start, l.offset, codeMalformedNumericLiteral, "malformed numeric literal")
	}
	return l.token(kind, start, l.offset)
}

func (l *Lexer) lexRadixNumber(start uint32, validDigit func(byte) bool, name string) Token {
	digitsStart := l.offset
	for isASCIILetter(l.peek(0)) || isDecimalDigit(l.peek(0)) || l.peek(0) == '_' {
		l.offset++
	}
	digits := l.text[digitsStart:l.offset]
	if !validSeparatedDigits(digits, validDigit) {
		return l.invalid(start, l.offset, codeMalformedNumericLiteral, fmt.Sprintf("malformed %s integer literal", name))
	}
	return l.token(IntegerLiteral, start, l.offset)
}

func (l *Lexer) scanDecimalSequence() {
	for isDecimalDigit(l.peek(0)) || l.peek(0) == '_' {
		l.offset++
	}
}

func (l *Lexer) lexString(start uint32) Token {
	var decoded strings.Builder
	for !l.atEnd() {
		switch l.peek(0) {
		case '"':
			l.offset++
			return l.decodedToken(StringLiteral, start, l.offset, tokenDecodedLiteral{
				kind: DecodedString,
				text: decoded.String(),
			})
		case '\n', '\r':
			return l.invalid(start, l.offset, codeMalformedStringLiteral, "ordinary strings cannot contain a physical line ending")
		case '\\':
			escape, ok, message := l.consumeEscape(false)
			if !ok {
				l.recoverQuoted('"')
				return l.invalid(start, l.offset, codeInvalidEscapeSequence, message)
			}
			if escape.byteValue {
				decoded.WriteByte(byte(escape.value))
			} else {
				decoded.WriteRune(escape.value)
			}
		default:
			previous := l.offset
			_, size := utf8.DecodeRune(l.text[l.offset:])
			l.offset += uint32(size)
			decoded.Write(l.text[previous:l.offset])
		}
	}
	return l.invalid(start, l.offset, codeMalformedStringLiteral, "unterminated string literal")
}

func (l *Lexer) lexCharacter(start uint32) Token {
	if l.atEnd() || isLineEnding(l.peek(0)) {
		return l.invalid(start, l.offset, codeInvalidCharacterLiteral, "unterminated character literal")
	}
	if l.peek(0) == '\'' {
		l.offset++
		return l.invalid(start, l.offset, codeInvalidCharacterLiteral, "character literal cannot be empty")
	}

	var decoded rune
	if l.peek(0) == '\\' {
		escape, ok, message := l.consumeEscape(false)
		if !ok {
			l.recoverQuoted('\'')
			return l.invalid(start, l.offset, codeInvalidEscapeSequence, message)
		}
		decoded = escape.value
	} else {
		value, size := utf8.DecodeRune(l.text[l.offset:])
		l.offset += uint32(size)
		decoded = value
	}

	if l.peek(0) != '\'' {
		l.recoverQuoted('\'')
		return l.invalid(start, l.offset, codeInvalidCharacterLiteral, "character literal must contain exactly one Unicode scalar")
	}
	l.offset++
	return l.decodedToken(CharacterLiteral, start, l.offset, tokenDecodedLiteral{
		kind: DecodedCharacter,
		rune: decoded,
	})
}

func (l *Lexer) lexInterpolatedText() Token {
	start := l.offset
	var decoded strings.Builder
	if l.atEnd() {
		opening := l.topMode().opening
		l.resetModes()
		return l.invalid(opening, l.offset, codeUnterminatedInterpolatedString, "unterminated interpolated string")
	}

	switch l.peek(0) {
	case '`':
		l.offset++
		l.modes = l.modes[:len(l.modes)-1]
		return l.token(InterpolationEnd, start, l.offset)
	case '{':
		l.offset++
		l.modes = append(l.modes, lexerMode{
			kind:    modeInterpolationExpression,
			opening: start,
		})
		return l.token(InterpolationExprStart, start, l.offset)
	}

	for !l.atEnd() {
		switch l.peek(0) {
		case '`', '{':
			return l.decodedToken(InterpolationText, start, l.offset, tokenDecodedLiteral{
				kind: DecodedInterpolationText,
				text: decoded.String(),
			})
		case '\\':
			escape, ok, message := l.consumeEscape(true)
			if !ok {
				return l.invalid(start, l.offset, codeInvalidEscapeSequence, message)
			}
			if escape.byteValue {
				decoded.WriteByte(byte(escape.value))
			} else {
				decoded.WriteRune(escape.value)
			}
		default:
			previous := l.offset
			_, size := utf8.DecodeRune(l.text[l.offset:])
			l.offset += uint32(size)
			decoded.Write(l.text[previous:l.offset])
		}
	}

	opening := l.topMode().opening
	l.resetModes()
	return l.invalid(opening, l.offset, codeUnterminatedInterpolatedString, "unterminated interpolated string")
}

type decodedEscape struct {
	value     rune
	byteValue bool
}

func (l *Lexer) consumeEscape(interpolation bool) (decodedEscape, bool, string) {
	l.offset++ // backslash
	if l.atEnd() || isLineEnding(l.peek(0)) {
		return decodedEscape{}, false, "unterminated escape sequence"
	}

	escape := l.peek(0)
	l.offset++
	switch escape {
	case '\\', '"', '\'':
		return decodedEscape{value: rune(escape)}, true, ""
	case 'n':
		return decodedEscape{value: '\n'}, true, ""
	case 'r':
		return decodedEscape{value: '\r'}, true, ""
	case 't':
		return decodedEscape{value: '\t'}, true, ""
	case '0':
		return decodedEscape{value: 0}, true, ""
	case '{', '`':
		if interpolation {
			return decodedEscape{value: rune(escape)}, true, ""
		}
		return decodedEscape{}, false, fmt.Sprintf("unknown escape sequence \\%c", escape)
	case 'x':
		value := rune(0)
		for range 2 {
			if !isHexDigit(l.peek(0)) {
				return decodedEscape{}, false, "byte escape requires exactly two hexadecimal digits"
			}
			value = value*16 + hexValue(l.peek(0))
			l.offset++
		}
		return decodedEscape{value: value, byteValue: true}, true, ""
	case 'u':
		return l.consumeUnicodeEscape()
	default:
		return decodedEscape{}, false, fmt.Sprintf("unknown escape sequence \\%c", escape)
	}
}

func (l *Lexer) consumeUnicodeEscape() (decodedEscape, bool, string) {
	if !l.take('{') {
		return decodedEscape{}, false, "Unicode escape requires '{' after \\u"
	}
	digitsStart := l.offset
	for isHexDigit(l.peek(0)) {
		l.offset++
	}
	digitCount := l.offset - digitsStart
	if !l.take('}') {
		for !l.atEnd() && !isLineEnding(l.peek(0)) && l.peek(0) != '}' {
			l.offset++
		}
		l.take('}')
		return decodedEscape{}, false, "Unicode escape must end with '}'"
	}
	if digitCount == 0 || digitCount > 6 {
		return decodedEscape{}, false, "Unicode escape requires one through six hexadecimal digits"
	}
	value, _ := strconv.ParseUint(string(l.text[digitsStart:digitsStart+digitCount]), 16, 32)
	if value > utf8.MaxRune || value >= 0xd800 && value <= 0xdfff {
		return decodedEscape{}, false, "Unicode escape is not a valid scalar value"
	}
	return decodedEscape{value: rune(value)}, true, ""
}

func (l *Lexer) recoverQuoted(delimiter byte) {
	for !l.atEnd() && !isLineEnding(l.peek(0)) {
		b := l.peek(0)
		l.offset++
		if b == delimiter {
			return
		}
	}
}

func (l *Lexer) skipWhitespaceAndComments() {
	for !l.atEnd() {
		switch l.peek(0) {
		case ' ', '\t', '\n', '\r':
			l.offset++
		case '/':
			if l.peek(1) != '/' {
				return
			}
			l.offset += 2
			for !l.atEnd() && !isLineEnding(l.peek(0)) {
				l.offset++
			}
		default:
			return
		}
	}
}

func (l *Lexer) token(kind TokenKind, start, end uint32) Token {
	return Token{Kind: kind, Span: source.NewSpan(l.file.ID(), start, end)}
}

func (l *Lexer) decodedToken(kind TokenKind, start, end uint32, decoded tokenDecodedLiteral) Token {
	return Token{Kind: kind, Span: source.NewSpan(l.file.ID(), start, end), decoded: decoded}
}

func (l *Lexer) invalid(start, end uint32, code diagnostic.Code, message string) Token {
	span := source.NewSpan(l.file.ID(), start, end)
	l.diagnostics.Error(code, message, span)
	return Token{Kind: Invalid, Span: span}
}

func (l *Lexer) atEnd() bool { return l.offset >= uint32(len(l.text)) }

func (l *Lexer) peek(ahead uint32) byte {
	index := l.offset + ahead
	if index >= uint32(len(l.text)) {
		return 0
	}
	return l.text[index]
}

func (l *Lexer) take(expected byte) bool {
	if l.peek(0) != expected {
		return false
	}
	l.offset++
	return true
}

func (l *Lexer) topMode() lexerMode { return l.modes[len(l.modes)-1] }

func (l *Lexer) resetModes() { l.modes = []lexerMode{{kind: modeNormal}} }

func validSeparatedDigits(text []byte, validDigit func(byte) bool) bool {
	if len(text) == 0 || !validDigit(text[0]) || !validDigit(text[len(text)-1]) {
		return false
	}
	previousUnderscore := false
	for _, b := range text {
		if b == '_' {
			if previousUnderscore {
				return false
			}
			previousUnderscore = true
			continue
		}
		if !validDigit(b) {
			return false
		}
		previousUnderscore = false
	}
	return true
}

func isIdentifierStart(b byte) bool { return isASCIILetter(b) || b == '_' }

func hexValue(b byte) rune {
	switch {
	case b >= '0' && b <= '9':
		return rune(b - '0')
	case b >= 'a' && b <= 'f':
		return rune(b-'a') + 10
	default:
		return rune(b-'A') + 10
	}
}

func isIdentifierContinue(b byte) bool { return isIdentifierStart(b) || isDecimalDigit(b) }

func isASCIILetter(b byte) bool { return b >= 'a' && b <= 'z' || b >= 'A' && b <= 'Z' }

func isDecimalDigit(b byte) bool { return b >= '0' && b <= '9' }

func isBinaryDigit(b byte) bool { return b == '0' || b == '1' }

func isHexDigit(b byte) bool {
	return isDecimalDigit(b) || b >= 'a' && b <= 'f' || b >= 'A' && b <= 'F'
}

func isLineEnding(b byte) bool { return b == '\n' || b == '\r' }

var keywords = map[string]TokenKind{
	"as":       KwAs,
	"break":    KwBreak,
	"case":     KwCase,
	"context":  KwContext,
	"continue": KwContinue,
	"defer":    KwDefer,
	"else":     KwElse,
	"enum":     KwEnum,
	"extern":   KwExtern,
	"false":    KwFalse,
	"fn":       KwFn,
	"for":      KwFor,
	"if":       KwIf,
	"import":   KwImport,
	"inline":   KwInline,
	"let":      KwLet,
	"loop":     KwLoop,
	"nil":      KwNil,
	"none":     KwNone,
	"print":    KwPrint,
	"println":  KwPrintln,
	"return":   KwReturn,
	"slice":    KwSlice,
	"sizeof":   KwSizeof,
	"some":     KwSome,
	"struct":   KwStruct,
	"switch":   KwSwitch,
	"true":     KwTrue,
	"type":     KwType,
	"union":    KwUnion,
	"var":      KwVar,
	"while":    KwWhile,
}
