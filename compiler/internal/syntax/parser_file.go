package syntax

import "github.com/pepplejoshua/pebble/compiler/internal/source"

func (p *parser) parseFile() NodeID {
	children := make([]NodeID, 0)
	for !p.at(EOF) && p.errors < p.errorLimit {
		before := p.cursor.index
		if isDeclarationStart(p.current().Kind) {
			children = append(children, p.parseDeclaration())
		} else {
			children = append(children, p.recoverDeclaration())
		}
		if p.cursor.index == before {
			children = append(children, p.errorNode("declaration", codeExpectedDeclaration, "expected declaration"))
		}
	}
	for !p.at(EOF) {
		p.cursor.advance()
	}
	eof := p.current()
	children = append(children, p.tree.add(EndOfFile, eof.Span, eof.Kind, ""))
	return p.tree.add(File, source.NewSpan(p.file.ID(), 0, p.file.Len()), EOF, "", children...)
}

func isDeclarationStart(kind TokenKind) bool {
	switch kind {
	case KwImport, KwLet, KwVar, KwType, KwFn, KwExtern:
		return true
	default:
		return false
	}
}

func (p *parser) parseDeclaration() NodeID {
	switch p.current().Kind {
	case KwImport:
		return p.parseImportDeclaration()
	case KwLet, KwVar:
		return p.parseBindingDeclaration(true)
	case KwType:
		return p.parseTypeDeclaration()
	case KwFn:
		return p.parseFunctionDeclaration()
	case KwExtern:
		return p.parseExternDeclaration()
	default:
		return p.recoverDeclaration()
	}
}

func (p *parser) recoverDeclaration() NodeID {
	start := p.current().Span.Start
	end := start
	reported := false
	for !p.at(EOF) && !isDeclarationStart(p.current().Kind) {
		token := p.cursor.advance()
		end = token.Span.End
		if !reported && token.Kind != Invalid {
			p.report(codeExpectedDeclaration, "expected top-level declaration", token.Span)
			reported = true
		}
		if token.Kind == Semicolon {
			break
		}
	}
	return p.tree.add(Error, source.NewSpan(p.file.ID(), start, end), EOF, "declaration")
}

func (p *parser) parseImportDeclaration() NodeID {
	opening := p.cursor.advance()
	children := make([]NodeID, 0, 2)
	if p.at(StringLiteral) {
		token := p.cursor.advance()
		children = append(children, p.tree.add(Literal, token.Span, token.Kind, ""))
	} else if isDeclarationStart(p.current().Kind) || p.at(EOF) {
		children = append(children, p.missing(Missing, "import path", codeInvalidSyntax, "expected string literal after 'import'"))
	} else {
		children = append(children, p.errorNode("import path", codeInvalidSyntax, "expected string literal after 'import'"))
	}
	terminator, missing := p.expect(Semicolon, "after import declaration")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(ImportDecl, source.NewSpan(p.file.ID(), opening.Span.Start, terminator.Span.End), opening.Kind, "", children...)
}

func (p *parser) parseBindingDeclaration(withTerminator bool) NodeID {
	opening := p.cursor.advance()
	children := []NodeID{p.parseName("after binding keyword")}
	var flags uint32
	if startsType(p.current().Kind) {
		children = append(children, p.parseType())
		flags |= BindingTypePresent
	}
	if _, initialized := p.take(Assign); initialized {
		children = append(children, p.parseExpression())
		flags |= BindingInitializerPresent
	}
	end := p.nodeSpan(children[len(children)-1]).End
	if withTerminator {
		terminator, missing := p.expect(Semicolon, "after binding declaration")
		end = terminator.Span.End
		if missing != 0 {
			children = append(children, missing)
		}
	}
	return p.tree.addData(BindingDecl, source.NewSpan(p.file.ID(), opening.Span.Start, end), opening.Kind, flags, "", children...)
}

func (p *parser) parseTypeDeclaration() NodeID {
	opening := p.cursor.advance()
	children := []NodeID{p.parseName("after 'type'")}
	children = append(children, p.parseTypeParameters()...)
	_, missingAssign := p.expect(Assign, "before type definition")
	if missingAssign != 0 {
		children = append(children, missingAssign)
	}
	children = append(children, p.parseType())
	terminator, missingTerminator := p.expect(Semicolon, "after type declaration")
	if missingTerminator != 0 {
		children = append(children, missingTerminator)
	}
	return p.tree.add(TypeDecl, source.NewSpan(p.file.ID(), opening.Span.Start, terminator.Span.End), opening.Kind, "", children...)
}

func (p *parser) parseTypeParameters() []NodeID {
	if _, present := p.take(LeftBracket); !present {
		return nil
	}
	children := make([]NodeID, 0)
	if p.at(RightBracket) {
		span := source.NewSpan(p.file.ID(), p.current().Span.Start, p.current().Span.Start)
		p.report(codeExpectedName, "type parameter list cannot be empty", span)
		children = append(children, p.tree.add(Missing, span, EOF, "type parameter"))
	}
	for !p.at(RightBracket) && !p.at(EOF) {
		name := p.parseName("in type parameter list")
		children = append(children, p.tree.add(TypeParameter, p.nodeSpan(name), EOF, "", name))
		if recovered := p.recoverTo("type parameter", "expected ',' or ']' after type parameter", Comma, RightBracket); recovered != 0 {
			children = append(children, recovered)
		}
		if _, comma := p.take(Comma); !comma {
			break
		}
	}
	_, missing := p.expect(RightBracket, "after type parameters")
	if missing != 0 {
		children = append(children, missing)
	}
	return children
}

func (p *parser) parseFunctionDeclaration() NodeID {
	opening := p.cursor.advance()
	children := p.parseFunctionModifiers()
	children = append(children, p.parseName("after 'fn' and modifiers"))
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
		flags = FunctionBodyPresent | FunctionExpressionBody
		terminator, missing := p.expect(Semicolon, "after expression-bodied function")
		end = terminator.Span.End
		if missing != 0 {
			children = append(children, missing)
		}
	default:
		missing := p.missing(Missing, "function body", codeExpectedToken, "expected function body or '=>' expression")
		children = append(children, missing)
		end = p.nodeSpan(missing).End
	}
	return p.tree.addData(FunctionDecl, source.NewSpan(p.file.ID(), opening.Span.Start, end), opening.Kind, flags, "", children...)
}

func (p *parser) parseFunctionModifiers() []NodeID {
	children := make([]NodeID, 0, 2)
	if p.at(KwInline) {
		token := p.cursor.advance()
		children = append(children, p.tree.add(Literal, token.Span, token.Kind, ""))
	}
	if p.at(StringLiteral) {
		token := p.cursor.advance()
		children = append(children, p.tree.add(Literal, token.Span, token.Kind, ""))
	}
	return children
}

func (p *parser) parseFunctionSignature() []NodeID {
	parameters := p.parseParameterList()
	result := p.parseType()
	return append(parameters, result)
}

func (p *parser) parseParameterList() []NodeID {
	children := make([]NodeID, 0)
	_, missingOpening := p.expect(LeftParen, "before function parameters")
	if missingOpening != 0 {
		children = append(children, missingOpening)
	}
	for !p.at(RightParen) && !p.at(EOF) && !p.at(LeftBrace) && !p.at(FatArrow) {
		before := p.cursor.index
		children = append(children, p.parseParameterGroup())
		if recovered := p.recoverTo("parameter", "expected ',' or ')' after parameter", Comma, RightParen, LeftBrace, FatArrow); recovered != 0 {
			children = append(children, recovered)
		}
		if _, comma := p.take(Comma); !comma {
			break
		}
		if p.cursor.index == before {
			children = append(children, p.errorNode("parameter", codeInvalidSyntax, "expected parameter"))
		}
	}
	_, missingClosing := p.expect(RightParen, "after function parameters")
	if missingClosing != 0 {
		children = append(children, missingClosing)
	}
	return children
}

func (p *parser) parseParameterGroup() NodeID {
	start := p.current().Span.Start
	variadic := uint32(0)
	if _, ok := p.take(Ellipsis); ok {
		variadic = ParameterVariadic
	}
	children := []NodeID{p.parseName("in parameter")}
	for p.at(Comma) && p.peek(1).Kind == Identifier && (p.peek(2).Kind == Comma || startsType(p.peek(2).Kind)) {
		p.cursor.advance()
		children = append(children, p.parseName("in parameter group"))
	}
	children = append(children, p.parseType())
	return p.tree.addData(Parameter, source.NewSpan(p.file.ID(), start, p.nodeSpan(children[len(children)-1]).End), EOF, variadic, "", children...)
}

func (p *parser) parseExternDeclaration() NodeID {
	opening := p.cursor.advance()
	children := make([]NodeID, 0)
	if p.at(StringLiteral) {
		token := p.cursor.advance()
		children = append(children, p.tree.add(Literal, token.Span, token.Kind, ""))
	}
	if p.at(LeftBrace) {
		children = append(children, p.parseExternBlock())
	} else {
		children = append(children, p.parseExternItem())
	}
	end := p.nodeSpan(children[len(children)-1]).End
	return p.tree.add(ExternDecl, source.NewSpan(p.file.ID(), opening.Span.Start, end), opening.Kind, "", children...)
}

func (p *parser) parseExternBlock() NodeID {
	opening := p.cursor.advance()
	children := make([]NodeID, 0)
	for !p.at(RightBrace) && !p.at(EOF) {
		before := p.cursor.index
		children = append(children, p.parseExternItem())
		if p.cursor.index == before {
			children = append(children, p.errorNode("extern item", codeInvalidSyntax, "expected extern item"))
		}
	}
	closing, missing := p.expect(RightBrace, "after extern block")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(ExternBlock, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), opening.Kind, "", children...)
}

func (p *parser) parseExternItem() NodeID {
	opening := p.current()
	switch opening.Kind {
	case KwFn:
		p.cursor.advance()
		children := []NodeID{p.parseName("after extern 'fn'")}
		children = append(children, p.parseFunctionSignature()...)
		terminator, missing := p.expect(Semicolon, "after extern function")
		if missing != 0 {
			children = append(children, missing)
		}
		return p.tree.add(ExternFunction, source.NewSpan(p.file.ID(), opening.Span.Start, terminator.Span.End), opening.Kind, "", children...)
	case KwType:
		p.cursor.advance()
		children := []NodeID{p.parseName("after extern 'type'")}
		terminator, missing := p.expect(Semicolon, "after extern type")
		if missing != 0 {
			children = append(children, missing)
		}
		return p.tree.add(ExternType, source.NewSpan(p.file.ID(), opening.Span.Start, terminator.Span.End), opening.Kind, "", children...)
	case KwLet, KwVar:
		p.cursor.advance()
		children := []NodeID{p.parseName("after extern binding keyword"), p.parseType()}
		terminator, missing := p.expect(Semicolon, "after extern binding")
		if missing != 0 {
			children = append(children, missing)
		}
		return p.tree.add(ExternBinding, source.NewSpan(p.file.ID(), opening.Span.Start, terminator.Span.End), opening.Kind, "", children...)
	default:
		return p.recoverTo("extern item", "expected extern function, type, or binding",
			Semicolon, RightBrace, KwFn, KwType, KwLet, KwVar, KwImport, KwExtern)
	}
}
