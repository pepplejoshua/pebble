package syntax

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

func (p *parser) parseExpression() NodeID {
	if !p.enter() {
		return p.errorNode("expression", codeNestingLimit, "parser nesting limit exceeded")
	}
	defer p.leave()
	return p.parseBinary(1)
}

func (p *parser) parseBinary(minPrecedence int) NodeID {
	left := p.parseCast()
	seenNonAssociative := map[int]bool{}
	for {
		operator := p.current()
		precedence, nonAssociative := binaryPrecedence(operator.Kind)
		if precedence < minPrecedence {
			break
		}
		p.cursor.advance()
		right := p.parseBinary(precedence + 1)
		span := source.NewSpan(p.file.ID(), p.nodeSpan(left).Start, p.nodeSpan(right).End)
		if nonAssociative && seenNonAssociative[precedence] {
			p.report(codeInvalidSyntax, fmt.Sprintf("operator %q cannot be chained without grouping", operator.Kind), operator.Span)
			left = p.tree.add(Error, span, operator.Kind, "grouped non-associative expression", left, right)
			continue
		}
		left = p.tree.add(BinaryExpr, span, operator.Kind, "", left, right)
		seenNonAssociative[precedence] = nonAssociative
	}
	return left
}

func binaryPrecedence(kind TokenKind) (int, bool) {
	switch kind {
	case LogicalOr:
		return 1, false
	case LogicalAnd:
		return 2, false
	case Pipe:
		return 3, false
	case Caret:
		return 4, false
	case Ampersand:
		return 5, false
	case Equal, NotEqual:
		return 6, true
	case Less, LessEqual, Greater, GreaterEqual:
		return 7, true
	case ShiftLeft, ShiftRight:
		return 8, false
	case Plus, Minus:
		return 9, false
	case Star, Slash, Percent:
		return 10, false
	default:
		return 0, false
	}
}

func (p *parser) parseCast() NodeID {
	left := p.parsePrefix()
	for p.at(KwAs) {
		operator := p.cursor.advance()
		target := p.parseType()
		left = p.tree.add(CastExpr,
			source.NewSpan(p.file.ID(), p.nodeSpan(left).Start, p.nodeSpan(target).End),
			operator.Kind, "", left, target)
	}
	return left
}

func (p *parser) parsePrefix() NodeID {
	if isPrefixOperator(p.current().Kind) {
		operator := p.cursor.advance()
		operand := p.parsePrefix()
		return p.tree.add(PrefixTerm,
			source.NewSpan(p.file.ID(), operator.Span.Start, p.nodeSpan(operand).End),
			operator.Kind, "", operand)
	}
	return p.parsePostfix()
}

func isPrefixOperator(kind TokenKind) bool {
	switch kind {
	case Minus, Bang, Ampersand, Star, Tilde:
		return true
	default:
		return false
	}
}

func (p *parser) parsePostfix() NodeID {
	base := p.parsePrimary()
	for {
		switch p.current().Kind {
		case LeftParen:
			base = p.parseCallSuffix(base)
		case LeftBracket:
			base = p.parseBracketOrSliceSuffix(base)
		case Dot:
			if p.peek(1).Kind == LeftBrace {
				base = p.parseRecordExpression(base)
			} else {
				base = p.parseMemberSuffix(base)
			}
		case PathSeparator:
			base = p.parsePathSuffix(base)
		case Bang, PlusPlus, MinusMinus:
			operator := p.cursor.advance()
			base = p.tree.add(PostfixExpr,
				source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, operator.Span.End),
				operator.Kind, "", base)
		default:
			return base
		}
	}
}

func (p *parser) parsePrimary() NodeID {
	token := p.current()
	switch token.Kind {
	case Identifier:
		p.cursor.advance()
		return p.tree.add(Name, token.Span, token.Kind, "")
	case IntegerLiteral, FloatLiteral, StringLiteral, CharacterLiteral, KwTrue, KwFalse, KwNil, KwNone:
		p.cursor.advance()
		return p.tree.add(Literal, token.Span, token.Kind, "")
	case KwContext:
		p.cursor.advance()
		return p.tree.add(ContextExpr, token.Span, token.Kind, "")
	case KwSome:
		p.cursor.advance()
		value := p.parseExpression()
		return p.tree.add(SomeExpr, source.NewSpan(p.file.ID(), token.Span.Start, p.nodeSpan(value).End), token.Kind, "", value)
	case KwSizeof:
		p.cursor.advance()
		typeNode := p.parseType()
		return p.tree.add(SizeofExpr, source.NewSpan(p.file.ID(), token.Span.Start, p.nodeSpan(typeNode).End), token.Kind, "", typeNode)
	case LeftParen:
		return p.parseGroupedExpression()
	case LeftBracket:
		return p.parseArrayExpression()
	case InterpolationStart:
		return p.parseInterpolatedString()
	case KwFn:
		return p.parseFunctionLiteral()
	case Dot:
		if p.peek(1).Kind == LeftBrace {
			return p.parseRecordExpression(0)
		}
		return p.parsePartialMember()
	default:
		return p.errorNode("expression", codeExpectedExpression, "expected expression")
	}
}

func (p *parser) parseGroupedExpression() NodeID {
	opening := p.cursor.advance()
	if p.at(RightParen) {
		closing := p.cursor.advance()
		p.report(codeExpectedExpression, "empty parentheses are not an expression", source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End))
		return p.tree.add(Error, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), EOF, "expression")
	}
	first := p.parseExpression()
	recovered := p.recoverTo("grouped expression", "expected ',' or ')' after expression",
		Comma, RightParen, Semicolon, RightBracket, RightBrace, Colon, InterpolationExprEnd)
	if _, comma := p.take(Comma); !comma {
		closing, missing := p.expect(RightParen, "after grouped expression")
		children := []NodeID{first}
		if recovered != 0 {
			children = append(children, recovered)
		}
		if missing != 0 {
			children = append(children, missing)
		}
		return p.tree.add(GroupedTerm, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), EOF, "", children...)
	}

	children := []NodeID{first}
	if recovered != 0 {
		children = append(children, recovered)
	}
	for !p.at(RightParen) && !p.at(EOF) {
		before := p.cursor.index
		children = append(children, p.parseExpression())
		if recovered := p.recoverTo("tuple element", "expected ',' or ')' after tuple element",
			Comma, RightParen, Semicolon, RightBracket, RightBrace, Colon, InterpolationExprEnd); recovered != 0 {
			children = append(children, recovered)
		}
		if _, comma := p.take(Comma); !comma {
			break
		}
		if p.cursor.index == before {
			children = append(children, p.errorNode("tuple element", codeExpectedExpression, "expected tuple element"))
		}
	}
	closing, missing := p.expect(RightParen, "after tuple")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(TupleTerm, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), EOF, "", children...)
}

func (p *parser) parseArrayExpression() NodeID {
	opening := p.cursor.advance()
	if closing, ok := p.take(RightBracket); ok {
		return p.tree.add(ArrayExpr, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), EOF, "")
	}
	first := p.parseExpression()
	if recovered := p.recoverTo("array element", "expected ';', ',' or ']' after array element", Semicolon, Comma, RightBracket); recovered != 0 {
		firstSpan := p.nodeSpan(first)
		first = p.tree.add(Error, source.NewSpan(p.file.ID(), firstSpan.Start, p.nodeSpan(recovered).End), EOF, "array element", first, recovered)
	}
	if _, repeat := p.take(Semicolon); repeat {
		count := p.parseExpression()
		closing, missing := p.expect(RightBracket, "after array repetition")
		children := []NodeID{first, count}
		if missing != 0 {
			children = append(children, missing)
		}
		return p.tree.add(ArrayRepeatExpr, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), EOF, "", children...)
	}
	children := []NodeID{first}
	for {
		if _, comma := p.take(Comma); !comma {
			break
		}
		if p.at(RightBracket) {
			break
		}
		children = append(children, p.parseExpression())
		if recovered := p.recoverTo("array element", "expected ',' or ']' after array element",
			Comma, RightBracket, Semicolon, RightParen, RightBrace, Colon, InterpolationExprEnd); recovered != 0 {
			children = append(children, recovered)
		}
	}
	closing, missing := p.expect(RightBracket, "after array elements")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(ArrayExpr, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), EOF, "", children...)
}

func (p *parser) parseInterpolatedString() NodeID {
	opening := p.cursor.advance()
	var children []NodeID
	for !p.at(InterpolationEnd) && !p.at(EOF) {
		switch p.current().Kind {
		case InterpolationText:
			token := p.cursor.advance()
			children = append(children, p.tree.add(Literal, token.Span, token.Kind, ""))
		case InterpolationExprStart:
			p.cursor.advance()
			children = append(children, p.parseExpression())
			_, missing := p.expect(InterpolationExprEnd, "after interpolated expression")
			if missing != 0 {
				children = append(children, missing)
			}
		case Invalid:
			children = append(children, p.errorNode("interpolation content", codeInvalidSyntax, "invalid interpolation content"))
		default:
			children = append(children, p.errorNode("interpolation content", codeInvalidSyntax, "expected interpolation text or expression"))
		}
	}
	closing, missing := p.expect(InterpolationEnd, "after interpolated string")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(InterpolatedString, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), EOF, "", children...)
}

func (p *parser) parsePartialMember() NodeID {
	opening := p.cursor.advance()
	member := p.parseName("after '.'")
	return p.tree.add(PartialMemberExpr, source.NewSpan(p.file.ID(), opening.Span.Start, p.nodeSpan(member).End), EOF, "", member)
}

func (p *parser) parseCallSuffix(base NodeID) NodeID {
	p.cursor.advance()
	children := []NodeID{base}
	for !p.at(RightParen) && !p.at(EOF) {
		children = append(children, p.parseExpression())
		if recovered := p.recoverTo("call argument", "expected ',' or ')' after call argument",
			Comma, RightParen, Semicolon, RightBracket, RightBrace, Colon, InterpolationExprEnd); recovered != 0 {
			children = append(children, recovered)
		}
		if _, comma := p.take(Comma); !comma {
			break
		}
	}
	closing, missing := p.expect(RightParen, "after call arguments")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(CallExpr, source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, closing.Span.End), EOF, "", children...)
}

func (p *parser) parseBracketOrSliceSuffix(base NodeID) NodeID {
	p.cursor.advance()
	children := []NodeID{base}
	if _, colon := p.take(Colon); colon {
		var flags uint32
		if !p.at(RightBracket) {
			children = append(children, p.parseExpression())
			flags |= SliceEndPresent
		}
		closing, missing := p.expect(RightBracket, "after slice")
		if missing != 0 {
			children = append(children, missing)
		}
		return p.tree.addData(SliceExpr, source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, closing.Span.End), EOF, flags, "", children...)
	}
	if p.at(RightBracket) {
		closing := p.current()
		missingSpan := source.NewSpan(p.file.ID(), closing.Span.Start, closing.Span.Start)
		p.report(codeExpectedExpression, "bracket application requires an argument", missingSpan)
		missing := p.tree.add(Missing, missingSpan, EOF, "bracket argument")
		p.cursor.advance()
		children = append(children, missing)
		return p.tree.add(BracketApply, source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, closing.Span.End), EOF, "", children...)
	}

	first := p.parseSyntaxTerm()
	children = append(children, first)
	if recovered := p.recoverTo("bracket argument", "expected ':', ',' or ']' after bracket argument",
		Colon, Comma, RightBracket, Semicolon, RightParen, RightBrace, InterpolationExprEnd); recovered != 0 {
		children = append(children, recovered)
	}
	if _, colon := p.take(Colon); colon {
		flags := SliceStartPresent
		if !p.at(RightBracket) {
			children = append(children, p.parseExpression())
			flags |= SliceEndPresent
		}
		closing, missing := p.expect(RightBracket, "after slice")
		if missing != 0 {
			children = append(children, missing)
		}
		return p.tree.addData(SliceExpr, source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, closing.Span.End), EOF, flags, "", children...)
	}
	for {
		if _, comma := p.take(Comma); !comma {
			break
		}
		if p.at(RightBracket) {
			break
		}
		children = append(children, p.parseSyntaxTerm())
		if recovered := p.recoverTo("bracket argument", "expected ',' or ']' after bracket argument",
			Comma, RightBracket, Semicolon, RightParen, RightBrace, Colon, InterpolationExprEnd); recovered != 0 {
			children = append(children, recovered)
		}
	}
	closing, missing := p.expect(RightBracket, "after bracket arguments")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(BracketApply, source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, closing.Span.End), EOF, "", children...)
}

func (p *parser) parseSyntaxTerm() NodeID {
	switch p.current().Kind {
	case Question, KwFn, KwStruct, KwUnion, KwEnum:
		return p.parseType()
	case LeftBracket:
		if p.looksLikeArrayType() {
			return p.parseType()
		}
	}
	return p.parseExpression()
}

func (p *parser) looksLikeArrayType() bool {
	depth := 0
	for ahead := 0; ahead < 64; ahead++ {
		switch p.peek(ahead).Kind {
		case LeftBracket:
			depth++
		case RightBracket:
			depth--
			if depth == 0 {
				return startsType(p.peek(ahead + 1).Kind)
			}
		case EOF, Comma:
			if depth == 0 {
				return false
			}
		}
	}
	return false
}

func (p *parser) parseMemberSuffix(base NodeID) NodeID {
	p.cursor.advance()
	var member NodeID
	if p.at(IntegerLiteral) {
		token := p.cursor.advance()
		member = p.tree.add(Literal, token.Span, token.Kind, "")
	} else {
		member = p.parseName("after '.'")
	}
	return p.tree.add(MemberExpr, source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, p.nodeSpan(member).End), EOF, "", base, member)
}

func (p *parser) parseFunctionLiteral() NodeID {
	opening := p.cursor.advance()
	children := p.parseFunctionModifiers()
	children = append(children, p.parseTypeParameters()...)
	children = append(children, p.parseFunctionSignature()...)
	end := p.nodeSpan(children[len(children)-1]).End
	var flags uint32
	switch p.current().Kind {
	case LeftBrace:
		body := p.parseBlock()
		children = append(children, body)
		end = p.nodeSpan(body).End
		flags = FunctionBodyPresent
	case FatArrow:
		p.cursor.advance()
		body := p.parseExpression()
		children = append(children, body)
		end = p.nodeSpan(body).End
		flags = FunctionBodyPresent | FunctionExpressionBody
	default:
		missing := p.missing(Missing, "function literal body", codeExpectedToken, "expected function literal body or '=>' expression")
		children = append(children, missing)
		end = p.nodeSpan(missing).End
	}
	return p.tree.addData(FunctionTerm, source.NewSpan(p.file.ID(), opening.Span.Start, end), opening.Kind, flags, "", children...)
}

func (p *parser) parseRecordExpression(base NodeID) NodeID {
	dot := p.cursor.advance()
	opening, missingOpening := p.expect(LeftBrace, "before record fields")
	children := make([]NodeID, 0)
	start := dot.Span.Start
	if base != 0 {
		children = append(children, base)
		start = p.nodeSpan(base).Start
	}
	if missingOpening != 0 {
		children = append(children, missingOpening)
	}
	for !p.at(RightBrace) && !p.at(EOF) {
		fieldStart := p.current().Span.Start
		name := p.parseName("in record field")
		_, missingAssign := p.expect(Assign, "after record field name")
		value := p.parseExpression()
		fieldChildren := []NodeID{name}
		if missingAssign != 0 {
			fieldChildren = append(fieldChildren, missingAssign)
		}
		fieldChildren = append(fieldChildren, value)
		field := p.tree.add(RecordField, source.NewSpan(p.file.ID(), fieldStart, p.nodeSpan(value).End), EOF, "", fieldChildren...)
		children = append(children, field)
		if recovered := p.recoverTo("record field", "expected ',' or '}' after record field", Comma, RightBrace); recovered != 0 {
			children = append(children, recovered)
		}
		if _, comma := p.take(Comma); !comma {
			break
		}
	}
	closing, missingClosing := p.expect(RightBrace, "after record fields")
	if missingClosing != 0 {
		children = append(children, missingClosing)
	}
	return p.tree.add(RecordExpr, source.NewSpan(p.file.ID(), start, closing.Span.End), opening.Kind, "", children...)
}

func (p *parser) parsePathSuffix(base NodeID) NodeID {
	p.cursor.advance()
	member := p.parseName("after '::'")
	return p.tree.add(Path, source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, p.nodeSpan(member).End), EOF, "", base, member)
}

func (p *parser) parseName(context string) NodeID {
	if token, ok := p.take(Identifier); ok {
		return p.tree.add(Name, token.Span, token.Kind, "")
	}
	return p.missing(Missing, "name", codeExpectedName, "expected name "+context)
}

func (p *parser) nodeSpan(id NodeID) source.Span {
	if node, ok := p.tree.Node(id); ok {
		return node.Span()
	}
	position := p.current().Span.Start
	return source.NewSpan(p.file.ID(), position, position)
}
