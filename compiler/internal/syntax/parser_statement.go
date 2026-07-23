package syntax

import "github.com/pepplejoshua/pebble/compiler/internal/source"

func (p *parser) parseBlock() NodeID {
	opening := p.cursor.advance()
	children := make([]NodeID, 0)
	for !p.at(RightBrace) && !p.at(EOF) && p.errors < p.errorLimit {
		before := p.cursor.index
		children = append(children, p.parseStatement())
		if p.cursor.index == before {
			children = append(children, p.recoverStatement())
		}
	}
	closing, missing := p.expect(RightBrace, "after block")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(BlockStmt, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), opening.Kind, "", children...)
}

func (p *parser) parseStatement() NodeID {
	if !p.enter() {
		return p.errorNode("statement", codeNestingLimit, "parser nesting limit exceeded")
	}
	defer p.leave()
	switch p.current().Kind {
	case LeftBrace:
		return p.parseBlock()
	case KwLet, KwVar:
		return p.parseBindingDeclaration(true)
	case KwReturn:
		return p.parseReturnStatement()
	case KwIf:
		return p.parseIfStatement()
	case KwWhile:
		return p.parseWhileStatement()
	case KwLoop:
		return p.parseRangeLoopStatement()
	case KwFor:
		return p.parseForStatement()
	case KwSwitch:
		return p.parseSwitchStatement()
	case KwDefer:
		return p.parseDeferStatement()
	case KwPrint:
		return p.parsePrintStatement()
	case KwBreak, KwContinue:
		return p.parseJumpStatement()
	default:
		if startsExpression(p.current().Kind) {
			return p.parseAssignmentOrExpressionStatement(true)
		}
		return p.recoverStatement()
	}
}

func startsExpression(kind TokenKind) bool {
	switch kind {
	case Identifier, IntegerLiteral, FloatLiteral, StringLiteral, CharacterLiteral,
		InterpolationStart, KwTrue, KwFalse, KwContext, KwNil, KwNone, KwSome,
		KwSizeof, LeftParen, LeftBracket, KwFn, Dot, Minus, Bang, Ampersand, Star, Tilde:
		return true
	default:
		return false
	}
}

func isStatementStart(kind TokenKind) bool {
	return kind == LeftBrace || kind == KwLet || kind == KwVar || kind == KwReturn ||
		kind == KwIf || kind == KwWhile || kind == KwLoop || kind == KwFor ||
		kind == KwSwitch || kind == KwDefer || kind == KwPrint || kind == KwBreak ||
		kind == KwContinue || startsExpression(kind)
}

func (p *parser) recoverStatement() NodeID {
	start := p.current().Span.Start
	end := start
	reported := false
	for !p.at(EOF) && !p.at(RightBrace) {
		if reported && isStatementStart(p.current().Kind) {
			break
		}
		token := p.cursor.advance()
		end = token.Span.End
		if !reported && token.Kind != Invalid {
			p.report(codeInvalidSyntax, "expected statement", token.Span)
			reported = true
		}
		if token.Kind == Semicolon {
			break
		}
	}
	return p.tree.add(Error, source.NewSpan(p.file.ID(), start, end), EOF, "statement")
}

func (p *parser) parseReturnStatement() NodeID {
	opening := p.cursor.advance()
	children := make([]NodeID, 0, 2)
	if !p.at(Semicolon) && !p.at(RightBrace) && !p.at(EOF) {
		children = append(children, p.parseExpression())
	}
	terminator, missing := p.expect(Semicolon, "after return statement")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(ReturnStmt, source.NewSpan(p.file.ID(), opening.Span.Start, terminator.Span.End), opening.Kind, "", children...)
}

func (p *parser) parseIfStatement() NodeID {
	opening := p.cursor.advance()
	condition := p.parseExpression()
	thenBranch := p.parseStatement()
	children := []NodeID{condition, thenBranch}
	end := p.nodeSpan(thenBranch).End
	if _, present := p.take(KwElse); present {
		elseBranch := p.parseStatement()
		children = append(children, elseBranch)
		end = p.nodeSpan(elseBranch).End
	}
	return p.tree.add(IfStmt, source.NewSpan(p.file.ID(), opening.Span.Start, end), opening.Kind, "", children...)
}

func (p *parser) parseWhileStatement() NodeID {
	opening := p.cursor.advance()
	condition := p.parseExpression()
	body := p.parseStatement()
	return p.tree.add(WhileStmt, source.NewSpan(p.file.ID(), opening.Span.Start, p.nodeSpan(body).End), opening.Kind, "", condition, body)
}

func (p *parser) parseRangeLoopStatement() NodeID {
	opening := p.cursor.advance()
	start := p.parseExpression()
	operator := p.current()
	if operator.Kind == Range || operator.Kind == RangeInclusive {
		p.cursor.advance()
	} else {
		p.report(codeExpectedToken, "expected '..' or '..=' in range loop", operator.Span)
		operator = Token{Kind: Range, Span: source.NewSpan(p.file.ID(), operator.Span.Start, operator.Span.Start)}
	}
	endExpression := p.parseExpression()
	children := []NodeID{start, endExpression}
	if _, named := p.take(Colon); named {
		children = append(children, p.parseName("after ':' in range loop"))
	}
	body := p.parseStatement()
	children = append(children, body)
	return p.tree.add(RangeLoopStmt, source.NewSpan(p.file.ID(), opening.Span.Start, p.nodeSpan(body).End), operator.Kind, "", children...)
}

func (p *parser) parseForStatement() NodeID {
	opening := p.cursor.advance()
	children := make([]NodeID, 0, 4)
	var flags uint32
	if !p.at(Semicolon) {
		if p.at(KwLet) || p.at(KwVar) {
			children = append(children, p.parseBindingDeclaration(false))
		} else {
			children = append(children, p.parseAssignmentOrExpressionStatement(false))
		}
		flags |= ForInitializerPresent
	}
	_, missingFirst := p.expect(Semicolon, "after for initializer")
	if missingFirst != 0 {
		children = append(children, missingFirst)
	}
	if !p.at(Semicolon) {
		children = append(children, p.parseExpression())
		flags |= ForConditionPresent
	}
	_, missingSecond := p.expect(Semicolon, "after for condition")
	if missingSecond != 0 {
		children = append(children, missingSecond)
	}
	if !p.at(LeftBrace) && !isStatementKeyword(p.current().Kind) && !p.at(EOF) {
		candidate := p.parseAssignmentOrExpressionStatement(false)
		if terminator, expressionBody := p.take(Semicolon); expressionBody {
			p.tree.extendTo(candidate, terminator.Span.End)
			children = append(children, candidate)
			return p.tree.addData(ForStmt,
				source.NewSpan(p.file.ID(), opening.Span.Start, terminator.Span.End),
				opening.Kind, flags, "", children...)
		}
		children = append(children, candidate)
		flags |= ForUpdatePresent
	}
	body := p.parseStatement()
	children = append(children, body)
	return p.tree.addData(ForStmt, source.NewSpan(p.file.ID(), opening.Span.Start, p.nodeSpan(body).End), opening.Kind, flags, "", children...)
}

func isStatementKeyword(kind TokenKind) bool {
	switch kind {
	case KwReturn, KwIf, KwWhile, KwLoop, KwFor, KwSwitch, KwDefer, KwPrint, KwBreak, KwContinue, KwLet, KwVar:
		return true
	default:
		return false
	}
}

func (p *parser) parseSwitchStatement() NodeID {
	opening := p.cursor.advance()
	children := []NodeID{p.parseExpression()}
	_, missingOpening := p.expect(LeftBrace, "before switch cases")
	if missingOpening != 0 {
		children = append(children, missingOpening)
	}
	seenElse := false
	for !p.at(RightBrace) && !p.at(EOF) {
		switch p.current().Kind {
		case KwCase:
			if seenElse {
				children = append(children, p.recoverTo("switch case", "case cannot follow switch else", KwElse, RightBrace))
			} else {
				children = append(children, p.parseSwitchCase(false))
			}
		case KwElse:
			if seenElse {
				children = append(children, p.recoverTo("switch case", "switch can contain only one else case", RightBrace))
			} else {
				children = append(children, p.parseSwitchCase(true))
				seenElse = true
			}
		default:
			recovered := p.recoverTo("switch case", "expected case, else, or '}' in switch", KwCase, KwElse, RightBrace)
			if recovered != 0 {
				children = append(children, recovered)
			}
		}
	}
	closing, missingClosing := p.expect(RightBrace, "after switch cases")
	if missingClosing != 0 {
		children = append(children, missingClosing)
	}
	return p.tree.add(SwitchStmt, source.NewSpan(p.file.ID(), opening.Span.Start, closing.Span.End), opening.Kind, "", children...)
}

func (p *parser) parseSwitchCase(isElse bool) NodeID {
	opening := p.cursor.advance()
	children := make([]NodeID, 0)
	if !isElse {
		for {
			children = append(children, p.parseExpression())
			if _, comma := p.take(Comma); !comma {
				break
			}
		}
	}
	_, missingColon := p.expect(Colon, "after switch case")
	if missingColon != 0 {
		children = append(children, missingColon)
	}
	body := p.parseStatement()
	children = append(children, body)
	return p.tree.add(SwitchCase, source.NewSpan(p.file.ID(), opening.Span.Start, p.nodeSpan(body).End), opening.Kind, "", children...)
}

func (p *parser) parseDeferStatement() NodeID {
	opening := p.cursor.advance()
	statement := p.parseStatement()
	return p.tree.add(DeferStmt, source.NewSpan(p.file.ID(), opening.Span.Start, p.nodeSpan(statement).End), opening.Kind, "", statement)
}

func (p *parser) parsePrintStatement() NodeID {
	opening := p.cursor.advance()
	children := []NodeID{p.parseExpression()}
	for p.at(Comma) {
		p.cursor.advance()
		children = append(children, p.parseExpression())
	}
	terminator, missing := p.expect(Semicolon, "after print statement")
	if missing != 0 {
		children = append(children, missing)
	}
	return p.tree.add(PrintStmt, source.NewSpan(p.file.ID(), opening.Span.Start, terminator.Span.End), opening.Kind, "", children...)
}

func (p *parser) parseJumpStatement() NodeID {
	opening := p.cursor.advance()
	terminator, missing := p.expect(Semicolon, "after jump statement")
	children := make([]NodeID, 0, 1)
	if missing != 0 {
		children = append(children, missing)
	}
	kind := BreakStmt
	if opening.Kind == KwContinue {
		kind = ContinueStmt
	}
	return p.tree.add(kind, source.NewSpan(p.file.ID(), opening.Span.Start, terminator.Span.End), opening.Kind, "", children...)
}

func (p *parser) parseAssignmentOrExpressionStatement(withTerminator bool) NodeID {
	left := p.parseExpression()
	kind := ExpressionStmt
	token := EOF
	children := []NodeID{left}
	if isAssignmentOperator(p.current().Kind) {
		operator := p.cursor.advance()
		token = operator.Kind
		children = append(children, p.parseExpression())
		kind = AssignmentStmt
	}
	end := p.nodeSpan(children[len(children)-1]).End
	if withTerminator {
		terminator, missing := p.expect(Semicolon, "after statement")
		end = terminator.Span.End
		if missing != 0 {
			children = append(children, missing)
		}
	}
	return p.tree.add(kind, source.NewSpan(p.file.ID(), p.nodeSpan(left).Start, end), token, "", children...)
}

func isAssignmentOperator(kind TokenKind) bool {
	switch kind {
	case Assign, PlusAssign, MinusAssign, StarAssign, SlashAssign, PercentAssign:
		return true
	default:
		return false
	}
}
