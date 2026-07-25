package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type assignmentKind uint8

const (
	assignmentSimple assignmentKind = iota + 1
	assignmentCompound
)

type assignmentRecord struct {
	Header        recordHeader
	Kind          assignmentKind
	Place, Source valueID
	Operator      syntax.TokenKind
	Statement     symbol.SyntaxRef
}

func validAssignmentOperator(kind assignmentKind, token syntax.TokenKind) bool {
	if kind == assignmentSimple {
		return token == 0
	}
	switch token {
	case syntax.PlusAssign, syntax.MinusAssign, syntax.StarAssign, syntax.SlashAssign, syntax.PercentAssign, syntax.PlusPlus, syntax.MinusMinus:
		return true
	}
	return false
}

type assignmentPlan struct {
	left, right symbol.SyntaxRef
	leftValue   typedValue
}

func (w *walker) prepareAssignment(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
	semantic := semanticRefs(ref.Module, node, tree)
	if len(semantic) < 2 {
		for i := range items {
			items[i].ctx.suppressValue = true
		}
		return items
	}
	p := &assignmentPlan{left: semantic[0], right: semantic[1]}
	p.leftValue = w.reserveExpression(p.left, w.originForRef(p.left, "assignment place", ctx.typeOwner, ctx.genericOwner))
	if resolved, ok := w.generation.inputs.Resolution.Reference(p.left); ok && resolved.State == symbol.ResolutionResolved {
		w.escapeDestinations[p.right] = resolved.Symbol
		if symbolValue := w.valuesBySymbol[resolved.Symbol]; symbolValue.ID != 0 {
			if known := w.knownValues[symbolValue.ID]; known != 0 {
				p.leftValue.Known = known
			}
		}
	}
	if p.leftValue.Known != 0 {
		w.knownValues[p.leftValue.ID] = p.leftValue.Known
		w.valuesBySyntax[p.left] = p.leftValue
	}
	for i := range items {
		if items[i].ref == p.right {
			items[i].ctx.expected = w.expectationFor(p.right, p.leftValue.ID, compatibilityAssignment)
		}
	}
	w.assignmentPlans[ref] = p
	return items
}

func (w *walker) finishAssignment(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	if w.session.Fatal() {
		return
	}
	p := w.assignmentPlans[ref]
	if p == nil {
		return
	}
	left, right := w.valuesBySyntax[p.left], w.valuesBySyntax[p.right]
	if left.ID == 0 || right.ID == 0 || !w.successfulExpressions[p.left] || !w.successfulExpressions[p.right] {
		return
	}
	w.retainPlaceUse(p.left, ref, ctx)
	header := w.header(ref, ctx.genericOwner, false)
	kind := assignmentSimple
	source := right
	if node.Token() != syntax.Assign {
		kind = assignmentCompound
		origin := w.origin(ref, node, "compound assignment result", ctx.typeOwner, ctx.genericOwner)
		temporary, ok := w.newSlotValue(w.session.Variable(origin), origin)
		if !ok {
			return
		}
		family, token := compoundOperator(node.Token())
		if family == operatorNumericSame {
			w.addConstraint(infer.Numeric(left.Term, origin))
			w.addConstraint(infer.Numeric(right.Term, origin))
		} else if family == operatorIntegralSame {
			w.addConstraint(infer.Integral(left.Term, origin))
			w.addConstraint(infer.Integral(right.Term, origin))
		}
		if family == operatorAdd && w.plusNeedsNumeric([]symbol.SyntaxRef{p.left, p.right}, []typedValue{left, right}, ctx.genericOwner) {
			w.addConstraint(infer.Numeric(left.Term, origin))
		}
		w.addConstraint(infer.Equal(left.Term, right.Term, origin))
		w.addConstraint(infer.Equal(temporary.Term, left.Term, origin))
		if w.session.Fatal() {
			return
		}
		op := operatorRecord{Header: header, Form: operatorBinary, Family: family, Token: token, Operands: []valueID{left.ID, right.ID}, Result: temporary.ID, GenericOwner: ctx.genericOwner}
		if _, ok := w.addRecord(retainedRecord{Header: header, Operator: &op}); !ok {
			return
		}
		for _, operand := range []typedValue{left, right} {
			if !w.rigidValues[operand.ID] {
				continue
			}
			kind := requirementNumeric
			if family == operatorIntegralSame {
				kind = requirementIntegral
			}
			w.retainOperatorRequirement(header, kind, operand.ID, token)
		}
		source = temporary
	}
	record := assignmentRecord{Header: header, Kind: kind, Place: left.ID, Source: right.ID, Operator: node.Token(), Statement: ref}
	if kind == assignmentSimple {
		record.Operator = 0
	}
	if _, ok := w.addRecord(retainedRecord{Header: header, Assignment: &record}); !ok {
		return
	}
	w.retainCompatibility(ref, ctx.genericOwner, source.ID, left.ID, compatibilityAssignment, 0, 0, node.Span(), false)
}

func compoundOperator(token syntax.TokenKind) (operatorFamily, syntax.TokenKind) {
	switch token {
	case syntax.PlusAssign:
		return operatorAdd, syntax.Plus
	case syntax.MinusAssign:
		return operatorNumericSame, syntax.Minus
	case syntax.StarAssign:
		return operatorNumericSame, syntax.Star
	case syntax.SlashAssign:
		return operatorNumericSame, syntax.Slash
	default:
		return operatorIntegralSame, syntax.Percent
	}
}
