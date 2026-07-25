package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type operatorForm uint8

const (
	operatorPrefix operatorForm = iota + 1
	operatorPostfix
	operatorBinary
)

type operatorFamily uint8

const (
	operatorLiteralNegate operatorFamily = iota + 1
	operatorNumericSame
	operatorAdd
	operatorIntegralSame
	operatorShift
	operatorBoolean
	operatorOrdering
	operatorEquality
	operatorAddress
	operatorDereference
	operatorOptionalForce
	operatorMutation
)

type operatorRecord struct {
	Header       recordHeader
	Form         operatorForm
	Family       operatorFamily
	Token        syntax.TokenKind
	Operands     []valueID
	Result       valueID
	GenericOwner symbol.SymbolID
}

func validOperatorRecord(v operatorRecord) bool {
	if v.Token == 0 || len(v.Operands) == 0 {
		return false
	}
	switch v.Form {
	case operatorPrefix:
		if len(v.Operands) != 1 {
			return false
		}
		switch v.Family {
		case operatorLiteralNegate, operatorNumericSame:
			return v.Token == syntax.Minus
		case operatorBoolean:
			return v.Token == syntax.Bang
		case operatorIntegralSame:
			return v.Token == syntax.Tilde
		case operatorAddress:
			return v.Token == syntax.Ampersand
		case operatorDereference:
			return v.Token == syntax.Star
		}
	case operatorPostfix:
		return len(v.Operands) == 1 && (v.Family == operatorOptionalForce && v.Token == syntax.Bang || v.Family == operatorMutation && (v.Token == syntax.PlusPlus || v.Token == syntax.MinusMinus))
	case operatorBinary:
		if len(v.Operands) != 2 {
			return false
		}
		switch v.Family {
		case operatorNumericSame:
			return v.Token == syntax.Minus || v.Token == syntax.Star || v.Token == syntax.Slash
		case operatorAdd:
			return v.Token == syntax.Plus
		case operatorIntegralSame:
			return v.Token == syntax.Percent || v.Token == syntax.Ampersand || v.Token == syntax.Pipe || v.Token == syntax.Caret
		case operatorShift:
			return v.Token == syntax.ShiftLeft || v.Token == syntax.ShiftRight
		case operatorBoolean:
			return v.Token == syntax.LogicalAnd || v.Token == syntax.LogicalOr
		case operatorOrdering:
			return v.Token == syntax.Less || v.Token == syntax.LessEqual || v.Token == syntax.Greater || v.Token == syntax.GreaterEqual
		case operatorEquality:
			return v.Token == syntax.Equal || v.Token == syntax.NotEqual
		}
	}
	return false
}

type operatorPlan struct {
	children []symbol.SyntaxRef
	result   typedValue
	exact    infer.Term
}

type castRecord struct {
	Header                      recordHeader
	Source, Destination, Result valueID
}
type castPlan struct {
	source      symbol.SyntaxRef
	destination typedValue
	result      typedValue
}

func (w *walker) prepareCast(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
	semantic := semanticRefs(ref.Module, node, tree)
	if ctx.suppressValue || len(semantic) < 2 {
		for i := range items {
			items[i].ctx.suppressValue = true
		}
		return items
	}
	p := &castPlan{source: semantic[0]}
	p.result = w.reserveExpression(ref, w.origin(ref, node, "cast result", ctx.typeOwner, ctx.genericOwner))
	p.destination = w.resolveTypeUse(semantic[1], ctx.typeOwner, ctx.genericOwner, "cast target", typeUseCastTarget)
	for i := range items {
		items[i].ctx.expected = expectedType{}
		if items[i].ref == semantic[1] {
			items[i].ctx.suppressValue = true
			items[i].ctx.typePosition = true
		}
	}
	w.castPlans[ref] = p
	return items
}

func (w *walker) finishCast(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	p := w.castPlans[ref]
	if p == nil || ctx.suppressValue {
		return
	}
	origin := w.origin(ref, node, "cast result", ctx.typeOwner, ctx.genericOwner)
	if w.session.Fatal() {
		return
	}
	source := w.valuesBySyntax[p.source]
	if source.ID == 0 || p.destination.ID == 0 || !w.successfulExpressions[p.source] {
		w.failExpression(ref, origin)
		return
	}
	result := w.expressionResult(ref, p.destination.Term, origin)
	if p.destination.Known != 0 {
		result.Known = p.destination.Known
		w.knownValues[result.ID] = result.Known
		w.valuesBySyntax[ref] = result
		w.rigidValues[result.ID] = w.isRigidType(result.Known)
	}
	w.addConstraint(infer.Equal(result.Term, p.destination.Term, origin))
	if w.session.Fatal() {
		return
	}
	header := w.header(ref, ctx.genericOwner, false)
	record := castRecord{Header: header, Source: source.ID, Destination: p.destination.ID, Result: result.ID}
	specialized, ok := w.addRecord(retainedRecord{Header: header, Cast: &record})
	if !ok {
		return
	}
	if w.rigidValues[source.ID] {
		w.retainRequirement(header, requirementUnsupportedConversion, source.ID)
	}
	if p.destination.Known != 0 && w.isRigidType(p.destination.Known) {
		w.retainRequirement(header, requirementUnsupportedConversion, p.destination.ID)
	}
	if _, ok := w.addRecord(retainedRecord{Header: header, Expression: &expressionRecord{Header: header, Kind: expressionCast, Result: result.ID, Children: []valueID{source.ID}, Specialized: specialized}}); ok {
		w.successfulExpressions[ref] = true
	}
}

func operatorSharesResult(kind syntax.NodeKind, token syntax.TokenKind) bool {
	if kind == syntax.PrefixTerm {
		return token == syntax.Minus || token == syntax.Tilde
	}
	if kind != syntax.BinaryExpr {
		return false
	}
	switch token {
	case syntax.Plus, syntax.Minus, syntax.Star, syntax.Slash, syntax.Percent,
		syntax.Ampersand, syntax.Pipe, syntax.Caret, syntax.ShiftLeft, syntax.ShiftRight:
		return true
	default:
		return false
	}
}

func (w *walker) prepareOperator(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
	if ctx.suppressValue {
		for i := range items {
			items[i].ctx.suppressValue = true
		}
		return items
	}
	p := &operatorPlan{}
	p.result = w.reserveExpression(ref, w.origin(ref, node, "operator result", ctx.typeOwner, ctx.genericOwner))
	for i := range items {
		child, _ := tree.Node(items[i].ref.Node)
		if child.Kind() == syntax.Missing || child.Kind() == syntax.Error {
			continue
		}
		p.children = append(p.children, items[i].ref)
		items[i].ctx.expected = expectedType{}
	}
	// Same-type operator evidence projects a known destination during the one
	// authored traversal. The shift count and comparison operands are excluded.
	if _, known := w.knownDestination(ctx.expected); operatorSharesResult(node.Kind(), node.Token()) && known {
		for i := range items {
			if node.Kind() == syntax.PrefixTerm && node.Token() == syntax.Minus {
				child, _ := tree.Node(items[i].ref.Node)
				if child.Kind() == syntax.Literal && (child.Token() == syntax.IntegerLiteral || child.Token() == syntax.FloatLiteral) {
					continue
				}
			}
			if node.Kind() == syntax.BinaryExpr && (node.Token() == syntax.ShiftLeft || node.Token() == syntax.ShiftRight) && i == 1 {
				continue
			}
			items[i].ctx.expected = w.expectationFor(items[i].ref, ctx.expected.Destination, ctx.expected.Role)
		}
	}
	w.operatorPlans[ref] = p
	return items
}

func (w *walker) finishOperator(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	p := w.operatorPlans[ref]
	if p == nil || ctx.suppressValue {
		return
	}
	origin := w.origin(ref, node, "operator result", ctx.typeOwner, ctx.genericOwner)
	operands := make([]typedValue, 0, len(p.children))
	for _, child := range p.children {
		v := w.valuesBySyntax[child]
		if v.ID == 0 || !w.successfulExpressions[child] {
			w.failExpression(ref, origin)
			return
		}
		operands = append(operands, v)
	}
	if len(operands) == 0 || node.Kind() == syntax.BinaryExpr && len(operands) != 2 {
		w.failExpression(ref, origin)
		return
	}
	resultTerm := infer.Term{}
	form, family := operatorPrefix, operatorFamily(0)
	if node.Kind() == syntax.PostfixExpr {
		form = operatorPostfix
	}
	if node.Kind() == syntax.BinaryExpr {
		form = operatorBinary
	}
	builtins := w.generation.inputs.Types.Builtins()
	addEqual := func(a, b infer.Term, role string) {
		w.addConstraint(infer.Equal(a, b, w.origin(ref, node, role, ctx.typeOwner, ctx.genericOwner)))
	}
	capability := func(c infer.Constraint) { w.addConstraint(c) }
	if node.Kind() == syntax.PrefixTerm {
		op := operands[0]
		switch node.Token() {
		case syntax.Minus:
			exact := infer.Term{}
			if childPlan := w.expressionPlans[p.children[0]]; childPlan != nil {
				exact = childPlan.exactLiteral
			}
			if childPlan := w.operatorPlans[p.children[0]]; exact == (infer.Term{}) && childPlan != nil {
				exact = childPlan.exact
			}
			if exact != (infer.Term{}) {
				family = operatorLiteralNegate
				p.exact = w.session.NegateLiteral(exact, origin)
				resultTerm = p.exact
			} else {
				family = operatorNumericSame
				resultTerm = w.session.Variable(origin)
				capability(infer.Numeric(op.Term, origin))
				addEqual(resultTerm, op.Term, "unary result")
			}
		case syntax.Bang:
			family = operatorBoolean
			resultTerm = w.session.Known(builtins.Bool)
			addEqual(op.Term, resultTerm, "boolean operand")
		case syntax.Tilde:
			family = operatorIntegralSame
			resultTerm = w.session.Variable(origin)
			capability(infer.Integral(op.Term, origin))
			addEqual(resultTerm, op.Term, "integral result")
		case syntax.Ampersand:
			family = operatorAddress
			resultTerm = w.session.Variable(origin)
			w.addConstraint(infer.ConstrainShape(resultTerm, infer.PointerShape(infer.Leaf(op.Term)), origin))
		case syntax.Star:
			family = operatorDereference
			resultTerm = w.session.Variable(origin)
			w.addConstraint(infer.ConstrainShape(op.Term, infer.PointerShape(infer.Leaf(resultTerm)), origin))
		}
	} else if node.Kind() == syntax.PostfixExpr {
		op := operands[0]
		switch node.Token() {
		case syntax.Bang:
			family = operatorOptionalForce
			resultTerm = w.session.Variable(origin)
			w.addConstraint(infer.ConstrainShape(op.Term, infer.OptionalShape(infer.Leaf(resultTerm)), origin))
		case syntax.PlusPlus, syntax.MinusMinus:
			family = operatorMutation
			resultTerm = w.session.Variable(origin)
			capability(infer.Numeric(op.Term, origin))
			addEqual(resultTerm, op.Term, "mutation result")
		}
	} else {
		left, right := operands[0], operands[1]
		switch node.Token() {
		case syntax.Minus, syntax.Star, syntax.Slash:
			family = operatorNumericSame
			resultTerm = w.session.Variable(origin)
			capability(infer.Numeric(left.Term, origin))
			capability(infer.Numeric(right.Term, origin))
			addEqual(left.Term, right.Term, "same operands")
			addEqual(resultTerm, left.Term, "operator result")
		case syntax.Plus:
			family = operatorAdd
			resultTerm = w.session.Variable(origin)
			addEqual(left.Term, right.Term, "same operands")
			addEqual(resultTerm, left.Term, "operator result")
			if w.plusNeedsNumeric(p.children, operands, ctx.genericOwner) {
				capability(infer.Numeric(left.Term, origin))
			}
		case syntax.Percent, syntax.Ampersand, syntax.Pipe, syntax.Caret:
			family = operatorIntegralSame
			resultTerm = w.session.Variable(origin)
			capability(infer.Integral(left.Term, origin))
			capability(infer.Integral(right.Term, origin))
			addEqual(left.Term, right.Term, "same operands")
			addEqual(resultTerm, left.Term, "operator result")
		case syntax.ShiftLeft, syntax.ShiftRight:
			family = operatorShift
			resultTerm = w.session.Variable(origin)
			capability(infer.Integral(left.Term, origin))
			capability(infer.Integral(right.Term, origin))
			addEqual(resultTerm, left.Term, "shift result")
		case syntax.LogicalAnd, syntax.LogicalOr:
			family = operatorBoolean
			resultTerm = w.session.Known(builtins.Bool)
			addEqual(left.Term, resultTerm, "left boolean operand")
			addEqual(right.Term, resultTerm, "right boolean operand")
		case syntax.Less, syntax.LessEqual, syntax.Greater, syntax.GreaterEqual:
			family = operatorOrdering
			resultTerm = w.session.Known(builtins.Bool)
			addEqual(left.Term, right.Term, "ordered operands")
			capability(infer.Ordered(left.Term, origin))
		case syntax.Equal, syntax.NotEqual:
			family = operatorEquality
			resultTerm = w.session.Known(builtins.Bool)
			addEqual(left.Term, right.Term, "equality operands")
		}
	}
	if family == 0 {
		w.failExpression(ref, origin)
		return
	}
	if w.session.Fatal() {
		return
	}
	result := w.expressionResult(ref, resultTerm, origin)
	if w.session.Fatal() {
		return
	}
	if family == operatorBoolean || family == operatorOrdering || family == operatorEquality {
		result.Known = builtins.Bool
		w.knownValues[result.ID] = result.Known
		w.valuesBySyntax[ref] = result
	}
	if result.ID == 0 || !w.publishedSyntax[ref] {
		return
	}
	switch family {
	case operatorNumericSame, operatorAdd, operatorIntegralSame, operatorMutation:
		for _, operand := range operands {
			w.rigidValues[result.ID] = w.rigidValues[result.ID] || w.rigidValues[operand.ID]
		}
	case operatorShift:
		w.rigidValues[result.ID] = w.rigidValues[operands[0].ID]
	}
	if family == operatorDereference {
		w.deriveDereferencePlace(ref, operands[0], result, ctx)
	}
	if family == operatorAddress || family == operatorMutation {
		w.retainPlaceUse(p.children[0], ref, ctx)
	}
	header := w.header(ref, ctx.genericOwner, false)
	ids := make([]valueID, len(operands))
	for i := range operands {
		ids[i] = operands[i].ID
	}
	record := operatorRecord{Header: header, Form: form, Family: family, Token: node.Token(), Operands: ids, Result: result.ID, GenericOwner: ctx.genericOwner}
	specialized, ok := w.addRecord(retainedRecord{Header: header, Operator: &record})
	if !ok {
		return
	}
	if family == operatorMutation {
		assignment := assignmentRecord{Header: header, Kind: assignmentCompound, Place: operands[0].ID, Source: result.ID, Operator: node.Token(), Statement: ref}
		w.addRecord(retainedRecord{Header: header, Assignment: &assignment})
		w.retainCompatibility(ref, ctx.genericOwner, result.ID, operands[0].ID, compatibilityAssignment, 0, 0, node.Span(), false)
	}
	for i, operand := range operands {
		if i >= len(p.children) || !w.rigidValues[operand.ID] {
			continue
		}
		switch family {
		case operatorNumericSame, operatorAdd, operatorMutation:
			w.retainOperatorRequirement(header, requirementNumeric, operand.ID, node.Token())
		case operatorIntegralSame, operatorShift:
			w.retainOperatorRequirement(header, requirementIntegral, operand.ID, node.Token())
		case operatorOrdering:
			w.retainOperatorRequirement(header, requirementOrdered, operand.ID, node.Token())
		case operatorEquality:
			w.retainOperatorRequirement(header, requirementEquatable, operand.ID, node.Token())
		}
	}
	w.applyExpected(result, p.exact, ctx.expected, origin)
	if destination := w.optionalDestinations[ref]; destination != 0 {
		w.retainCompatibility(ref, ctx.genericOwner, result.ID, destination, compatibilityOptionalInjection, 0, 0, node.Span(), false)
	}
	if _, ok := w.addRecord(retainedRecord{Header: header, Expression: &expressionRecord{Header: header, Kind: map[operatorForm]expressionKind{operatorPrefix: expressionPrefix, operatorPostfix: expressionPostfix, operatorBinary: expressionBinary}[form], Result: result.ID, Children: ids, Specialized: specialized}}); ok {
		w.successfulExpressions[ref] = true
	}
}

func (w *walker) retainOperatorRequirement(header recordHeader, kind requirementKind, subject valueID, token syntax.TokenKind) {
	if w.session == nil || w.session.Fatal() {
		return
	}
	if header.Owner == 0 || subject == 0 {
		return
	}
	if w.activeBranch != nil {
		w.activeBranch.requirements++
	} else if !w.generation.addGenericRequirement() {
		return
	}
	record := requirementRecord{Header: header, Kind: kind, Subject: subject, Operator: token}
	w.addRecord(retainedRecord{Header: header, Requirement: &record})
}

func (w *walker) plusNeedsNumeric(refs []symbol.SyntaxRef, values []typedValue, owner symbol.SymbolID) bool {
	for i, ref := range refs {
		if p := w.expressionPlans[ref]; p != nil && p.literal.Kind >= literalInteger && p.literal.Kind <= literalFloat {
			return true
		}
		if i < len(values) {
			if id, ok := w.knownValues[values[i].ID]; ok {
				key, found := w.generation.inputs.Types.Key(id)
				if found && key.Kind() == types.Builtin {
					b, _ := key.Builtin()
					if b == types.Str {
						continue
					}
					if b >= types.Int && b <= types.F64 {
						return true
					}
				}
			}
		}
	}
	if owner != 0 {
		for i, ref := range refs {
			if i < len(values) && w.rigidValues[values[i].ID] || w.rigidOperand(ref) {
				return true
			}
		}
	}
	return false
}

func (w *walker) isRigidType(id types.TypeID) bool {
	key, ok := w.generation.inputs.Types.Key(id)
	return ok && key.Kind() == types.TypeParameter
}

func (w *walker) rigidOperand(ref symbol.SyntaxRef) bool {
	node, ok := w.node(ref.Module, ref.Node)
	if !ok {
		return false
	}
	query := ref
	if node.Kind() == syntax.Path {
		children := node.Children()
		if len(children) != 0 {
			query.Node = children[len(children)-1]
		}
	}
	r, ok := w.generation.inputs.Resolution.Reference(query)
	if !ok || r.State != symbol.ResolutionResolved {
		return false
	}
	s, ok := w.generation.inputs.Resolution.Symbols.Symbol(r.Symbol)
	if !ok || s.Kind != symbol.SymbolParameter || s.Containing == 0 {
		return false
	}
	sig, ok := w.program.Signature(s.Containing)
	if !ok {
		return false
	}
	for i, id := range sig.Parameters {
		if id == s.ID && i < len(sig.Inputs) {
			tmpl, found := w.program.Template(sig.Inputs[i])
			return found && tmpl.Kind == infer.TemplateParameter
		}
	}
	return false
}
