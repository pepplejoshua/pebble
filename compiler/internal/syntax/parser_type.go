package syntax

import "github.com/pepplejoshua/pebble/compiler/internal/source"

func (p *parser) parseType() NodeID {
	if !p.enter() {
		return p.errorNode("type", codeNestingLimit, "parser nesting limit exceeded")
	}
	defer p.leave()

	token := p.current()
	switch token.Kind {
	case Identifier:
		return p.parseNamedType()
	case Star:
		p.cursor.advance()
		base := p.parseType()
		return p.tree.add(PrefixTerm, source.NewSpan(p.file.ID(), token.Span.Start, p.nodeSpan(base).End), token.Kind, "", base)
	case Question:
		p.cursor.advance()
		base := p.parseType()
		return p.tree.add(OptionalType, source.NewSpan(p.file.ID(), token.Span.Start, p.nodeSpan(base).End), token.Kind, "", base)
	case LeftBracket:
		return p.parseArrayOrSliceType()
	case KwFn:
		return p.parseFunctionType()
	case LeftParen:
		return p.parseGroupedType()
	case KwStruct:
		return p.parseAggregateType(StructType, FieldDecl, false)
	case KwUnion:
		return p.parseUnionType()
	case KwEnum:
		return p.parseEnumType()
	default:
		return p.errorNode("type", codeExpectedType, "expected type")
	}
}

func startsType(kind TokenKind) bool {
	switch kind {
	case Identifier, Star, Question, LeftBracket, KwFn, LeftParen, KwStruct, KwUnion, KwEnum:
		return true
	default:
		return false
	}
}

func (p *parser) parseNamedType() NodeID {
	first := p.cursor.advance()
	base := p.tree.add(Name, first.Span, first.Kind, "")
	for p.at(PathSeparator) {
		base = p.parsePathSuffix(base)
	}
	if !p.at(LeftBracket) {
		return base
	}
	return p.parseTypeArguments(base)
}

func (p *parser) parseTypeArguments(base NodeID) NodeID {
	p.cursor.advance()
	children := []NodeID{base}
	if p.at(RightBracket) {
		closing := p.current()
		missingSpan := source.NewSpan(p.file.ID(), closing.Span.Start, closing.Span.Start)
		p.report(codeExpectedType, "generic argument list cannot be empty", missingSpan)
		missing := p.tree.add(Missing, missingSpan, EOF, "type argument")
		p.cursor.advance()
		children = append(children, missing)
		return p.tree.add(BracketApply, source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, closing.Span.End), EOF, "", children...)
	}
	for {
		children = append(children, p.parseType())
		if recovered := p.recoverTo("type argument", "expected ',' or ']' after type argument", Comma, RightBracket); recovered != 0 {
			children = append(children, recovered)
		}
		if _, comma := p.take(Comma); !comma {
			break
		}
		if p.at(RightBracket) {
			break
		}
	}
	closing, missing := p.expect(RightBracket, "after type arguments")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(BracketApply, source.NewSpan(p.file.ID(), p.nodeSpan(base).Start, closing.Span.End), EOF, "", children...)
}

func (p *parser) parseArrayOrSliceType() NodeID {
	opening := p.cursor.advance()
	if _, empty := p.take(RightBracket); empty {
		element := p.parseType()
		return p.tree.add(SliceType, source.NewSpan(p.file.ID(), opening.Span.Start, p.nodeSpan(element).End), EOF, "", element)
	}
	length := p.parseExpression()
	_, missingCloser := p.expect(RightBracket, "after array length")
	element := p.parseType()
	children := []NodeID{length}
	if missingCloser != 0 {
		children = append(children, missingCloser)
	}
	children = append(children, element)
	return p.tree.add(ArrayType, source.NewSpan(p.file.ID(), opening.Span.Start, p.nodeSpan(element).End), EOF, "", children...)
}

func (p *parser) parseFunctionType() NodeID {
	opening := p.cursor.advance()
	var children []NodeID
	if p.at(StringLiteral) {
		token := p.cursor.advance()
		children = append(children, p.tree.add(Literal, token.Span, token.Kind, ""))
	}
	_, missingOpening := p.expect(LeftParen, "before function type parameters")
	if missingOpening != 0 {
		children = append(children, missingOpening)
	}
	for !p.at(RightParen) && !p.at(EOF) {
		children = append(children, p.parseType())
		if recovered := p.recoverTo("function parameter type", "expected ',' or ')' after function parameter type", Comma, RightParen); recovered != 0 {
			children = append(children, recovered)
		}
		if _, comma := p.take(Comma); !comma {
			break
		}
	}
	_, missingClosing := p.expect(RightParen, "after function type parameters")
	if missingClosing != 0 {
		children = append(children, missingClosing)
	}
	result := p.parseType()
	children = append(children, result)
	return p.tree.add(FunctionTerm, source.NewSpan(p.file.ID(), opening.Span.Start, p.nodeSpan(result).End), opening.Kind, "", children...)
}

func (p *parser) parseGroupedType() NodeID {
	opening := p.cursor.advance()
	if p.at(RightParen) {
		closing := p.cursor.advance()
		p.report(codeExpectedType, "empty parentheses are not a type", source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End))
		return p.tree.add(Error, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), EOF, "type")
	}
	first := p.parseType()
	recovered := p.recoverTo("grouped type", "expected ',' or ')' after type", Comma, RightParen)
	if _, comma := p.take(Comma); !comma {
		closing, missing := p.expect(RightParen, "after grouped type")
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
		children = append(children, p.parseType())
		if recovered := p.recoverTo("tuple type element", "expected ',' or ')' after tuple type element", Comma, RightParen); recovered != 0 {
			children = append(children, recovered)
		}
		if _, comma := p.take(Comma); !comma {
			break
		}
	}
	closing, missing := p.expect(RightParen, "after tuple type")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(TupleTerm, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), EOF, "", children...)
}

func (p *parser) parseAggregateType(kind NodeKind, memberKind NodeKind, tagged bool) NodeID {
	opening := p.cursor.advance()
	children := make([]NodeID, 0)
	if tagged {
		marker := p.cursor.advance()
		children = append(children, p.tree.add(Literal, marker.Span, marker.Kind, ""))
	}
	_, missingOpening := p.expect(LeftBrace, "before aggregate members")
	if missingOpening != 0 {
		children = append(children, missingOpening)
	}
	for !p.at(RightBrace) && !p.at(EOF) {
		before := p.cursor.index
		if p.at(KwFn) {
			children = append(children, p.recoverAggregateMethod())
		} else {
			children = append(children, p.parseAggregateMember(memberKind))
		}
		if p.cursor.index == before {
			children = append(children, p.errorNode("aggregate member", codeInvalidSyntax, "expected aggregate member"))
		}
	}
	closing, missingClosing := p.expect(RightBrace, "after aggregate members")
	if missingClosing != 0 {
		children = append(children, missingClosing)
	}
	return p.tree.add(kind, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), opening.Kind, "", children...)
}

func (p *parser) parseAggregateMember(kind NodeKind) NodeID {
	start := p.current().Span.Start
	children := []NodeID{p.parseName("in aggregate member")}
	for p.at(Comma) {
		p.cursor.advance()
		children = append(children, p.parseName("after ',' in aggregate member"))
	}
	typeNode := p.parseType()
	children = append(children, typeNode)
	if recovered := p.recoverTo("aggregate member", "expected ';' after aggregate member", Semicolon, RightBrace); recovered != 0 {
		children = append(children, recovered)
	}
	terminator, missing := p.expect(Semicolon, "after aggregate member")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(kind, source.NewSpan(p.file.ID(), start, terminator.Span.End), EOF, "", children...)
}

func (p *parser) recoverAggregateMethod() NodeID {
	start := p.current().Span.Start
	p.report(codeInvalidSyntax, "aggregate methods are implemented in parser Slice 2B", p.current().Span)
	for !p.at(Semicolon) && !p.at(RightBrace) && !p.at(EOF) {
		p.cursor.advance()
	}
	if p.at(Semicolon) {
		p.cursor.advance()
	}
	return p.tree.add(Error, source.NewSpan(p.file.ID(), start, p.current().Span.Start), EOF, "aggregate field")
}

func (p *parser) parseUnionType() NodeID {
	tagged := p.peek(1).Kind == KwEnum
	return p.parseAggregateType(UnionType, VariantDecl, tagged)
}

func (p *parser) parseEnumType() NodeID {
	opening := p.cursor.advance()
	children := make([]NodeID, 0)
	_, missingOpening := p.expect(LeftBrace, "before enum variants")
	if missingOpening != 0 {
		children = append(children, missingOpening)
	}
	for !p.at(RightBrace) && !p.at(EOF) {
		children = append(children, p.parseName("in enum type"))
		if recovered := p.recoverTo("enum variant", "expected ',' or '}' after enum variant", Comma, RightBrace); recovered != 0 {
			children = append(children, recovered)
		}
		if _, comma := p.take(Comma); !comma {
			break
		}
	}
	closing, missingClosing := p.expect(RightBrace, "after enum variants")
	if missingClosing != 0 {
		children = append(children, missingClosing)
	}
	return p.tree.add(EnumType, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), opening.Kind, "", children...)
}
