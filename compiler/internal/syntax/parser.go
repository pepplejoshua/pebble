package syntax

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

const (
	defaultParserDiagnosticLimit = 50
	defaultParserNestingLimit    = 256
)

type tokenCursor struct {
	lexer  *Lexer
	tokens []Token
	index  int
}

func (c *tokenCursor) peek(ahead int) Token {
	wanted := c.index + ahead
	for len(c.tokens) <= wanted {
		last := len(c.tokens) - 1
		if last >= 0 && c.tokens[last].Kind == EOF {
			return c.tokens[last]
		}
		c.tokens = append(c.tokens, c.lexer.Next())
	}
	return c.tokens[wanted]
}

func (c *tokenCursor) advance() Token {
	token := c.peek(0)
	if token.Kind != EOF {
		c.index++
	}
	return token
}

type parser struct {
	file        *source.File
	diagnostics *diagnostic.DiagnosticSet
	cursor      tokenCursor
	tree        *Tree
	nesting     int
	nestingMax  int
	errorLimit  int
	errors      int
	emitted     map[string]struct{}
}

func newParser(file *source.File, diagnostics *diagnostic.DiagnosticSet) *parser {
	if diagnostics == nil {
		diagnostics = diagnostic.NewDiagnosticSet()
	}
	return &parser{
		file: file, diagnostics: diagnostics,
		cursor: tokenCursor{lexer: NewLexer(file, diagnostics)},
		tree:   newTree(file.ID()), nestingMax: defaultParserNestingLimit,
		errorLimit: defaultParserDiagnosticLimit, emitted: make(map[string]struct{}),
	}
}

// Parse parses one complete Pebble source file. It always returns a non-nil
// tree rooted at File, including for empty or damaged input.
func Parse(file *source.File, diagnostics *diagnostic.DiagnosticSet) *Tree {
	p := newParser(file, diagnostics)
	p.tree.root = p.parseFile()
	return p.tree
}

func parseExpressionFragment(file *source.File, diagnostics *diagnostic.DiagnosticSet) *Tree {
	p := newParser(file, diagnostics)
	p.tree.root = p.parseExpression()
	p.requireEOF("after expression")
	return p.tree
}

func parseTypeFragment(file *source.File, diagnostics *diagnostic.DiagnosticSet) *Tree {
	p := newParser(file, diagnostics)
	p.tree.root = p.parseType()
	p.requireEOF("after type")
	return p.tree
}

func (p *parser) current() Token         { return p.cursor.peek(0) }
func (p *parser) peek(ahead int) Token   { return p.cursor.peek(ahead) }
func (p *parser) at(kind TokenKind) bool { return p.current().Kind == kind }
func (p *parser) take(kind TokenKind) (Token, bool) {
	if !p.at(kind) {
		return Token{}, false
	}
	return p.cursor.advance(), true
}

func (p *parser) requireEOF(context string) {
	for p.at(Invalid) {
		p.cursor.advance()
	}
	if p.at(EOF) {
		return
	}
	p.report(codeInvalidSyntax, fmt.Sprintf("unexpected syntax %s", context), p.current().Span)
	for !p.at(EOF) {
		p.cursor.advance()
	}
}

func (p *parser) report(code diagnostic.Code, message string, span source.Span) {
	if p.errors >= p.errorLimit {
		return
	}
	key := fmt.Sprintf("%s:%d", code, span.Start)
	if _, exists := p.emitted[key]; exists {
		return
	}
	p.emitted[key] = struct{}{}
	p.diagnostics.Error(code, message, span)
	p.errors++
	if p.errors == p.errorLimit {
		p.diagnostics.Add(diagnostic.Diagnostic{
			Severity: diagnostic.Note,
			Code:     code,
			Message:  "parser diagnostic limit reached; remaining syntax was not parsed",
			Primary:  diagnostic.Label{Span: span},
		})
	}
}

func (p *parser) enter() bool {
	if p.nesting >= p.nestingMax {
		span := p.current().Span
		p.report(codeNestingLimit, "parser nesting limit exceeded", span)
		return false
	}
	p.nesting++
	return true
}

func (p *parser) leave() { p.nesting-- }

func (p *parser) missing(kind NodeKind, expected string, code diagnostic.Code, message string) NodeID {
	position := p.current().Span.Start
	span := source.NewSpan(p.file.ID(), position, position)
	p.report(code, message, span)
	return p.tree.add(kind, span, EOF, expected)
}

func (p *parser) expect(kind TokenKind, context string) (Token, NodeID) {
	if token, ok := p.take(kind); ok {
		return token, 0
	}
	missing := p.missing(Missing, kind.String(), codeExpectedToken, fmt.Sprintf("expected %q %s", kind.String(), context))
	return Token{Kind: kind, Span: source.NewSpan(p.file.ID(), p.current().Span.Start, p.current().Span.Start)}, missing
}

func (p *parser) errorNode(expected string, code diagnostic.Code, message string) NodeID {
	if p.at(EOF) || isExpressionFollower(p.current().Kind) {
		return p.missing(Missing, expected, code, message)
	}
	token := p.cursor.advance()
	if token.Kind != Invalid {
		p.report(code, message, token.Span)
	}
	return p.tree.add(Error, token.Span, token.Kind, expected)
}

func (p *parser) recoverTo(expected, message string, stops ...TokenKind) NodeID {
	if p.at(EOF) || tokenIn(p.current().Kind, stops...) {
		return 0
	}
	start := p.current().Span.Start
	reported := false
	end := start
	for !p.at(EOF) && !tokenIn(p.current().Kind, stops...) {
		token := p.cursor.advance()
		end = token.Span.End
		if !reported && token.Kind != Invalid {
			p.report(codeInvalidSyntax, message, token.Span)
			reported = true
		}
	}
	return p.tree.add(Error, source.NewSpan(p.file.ID(), start, end), EOF, expected)
}

func tokenIn(kind TokenKind, choices ...TokenKind) bool {
	for _, choice := range choices {
		if kind == choice {
			return true
		}
	}
	return false
}

func isExpressionFollower(kind TokenKind) bool {
	switch kind {
	case EOF, Comma, Semicolon, RightParen, RightBracket, RightBrace, LeftBrace,
		InterpolationExprEnd, Colon, FatArrow, Assign, Range, RangeInclusive,
		KwImport, KwLet, KwVar, KwType, KwExtern, KwReturn, KwIf, KwWhile,
		KwLoop, KwFor, KwSwitch, KwDefer, KwPrint, KwBreak, KwContinue:
		return true
	default:
		return false
	}
}
