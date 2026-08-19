package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// prepareStatement is the single entry for every authored statement kind. It
// visits each statement exactly once, allocates the lexical region owned by the
// region-owning kinds, and pins the control operands those regions retain.
func (w *walker) prepareStatement(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	if node.Kind() == syntax.AssignmentStmt {
		return w.prepareAssignment(ref, node, ctx, tree)
	}
	items := childItems(ref, node, ctx)
	if ctx.suppressValue {
		for index := range items {
			items[index].ctx.suppressValue = true
		}
		return items
	}
	switch node.Kind() {
	case syntax.BlockStmt:
		return w.prepareBlock(ref, ctx, items)
	case syntax.IfStmt:
		return w.prepareIf(ref, node, ctx, tree, items)
	case syntax.WhileStmt:
		return w.prepareWhile(ref, node, ctx, tree, items)
	case syntax.RangeLoopStmt:
		return w.prepareRangeLoop(ref, node, ctx, tree, items)
	case syntax.ForStmt:
		return w.prepareFor(ref, node, ctx, tree, items)
	case syntax.SwitchStmt:
		return w.prepareSwitch(ref, node, ctx, tree, items)
	case syntax.SwitchCase:
		return w.prepareSwitchCase(ref, node, ctx, tree, items)
	case syntax.ReturnStmt:
		return w.prepareReturn(ref, node, ctx, tree, items)
	case syntax.DeferStmt:
		return w.prepareDefer(ref, node, ctx, tree, items)
	}
	return items
}

// finishStatement retains the leaf control records once their authored operands
// have completed their single traversal. Region-owning kinds already retained
// theirs on entry and only close their region here.
func (w *walker) finishStatement(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) {
	if node.Kind() == syntax.AssignmentStmt {
		w.finishAssignment(ref, node, ctx)
	}
	if ctx.suppressValue {
		return
	}
	switch node.Kind() {
	case syntax.ReturnStmt:
		w.finishReturn(ref, node, ctx, tree)
	case syntax.BreakStmt, syntax.ContinueStmt:
		w.finishJump(ref, node, ctx)
	case syntax.PrintStmt:
		w.finishPrint(ref, node, ctx, tree)
	case syntax.ExpressionStmt:
		w.finishExpressionStatement(ref, node, ctx, tree)
	case syntax.AssignmentStmt:
		w.retainControl(ref, ctx, controlEmission{
			kind: controlExpression, form: statementAssignment, region: ctx.control.region,
		})
	}
}

// statementFormFor classifies an expression statement by the immediate retained
// expression record kind. Grouping is never unwrapped: 06a classifies and 06b
// decides legality.
func (w *walker) statementFormFor(ref symbol.SyntaxRef, tree *syntax.Tree) statementForm {
	node, ok := tree.Node(ref.Node)
	if !ok {
		return statementDiscard
	}
	switch node.Kind() {
	case syntax.CallExpr:
		return statementCall
	case syntax.PostfixExpr:
		if node.Token() == syntax.PlusPlus || node.Token() == syntax.MinusMinus {
			return statementPostfixUpdate
		}
	}
	return statementDiscard
}

func (w *walker) finishExpressionStatement(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) {
	semantic := semanticRefs(ref.Module, node, tree)
	emission := controlEmission{kind: controlExpression, form: statementDiscard, region: ctx.control.region}
	if len(semantic) != 0 {
		emission.form = w.statementFormFor(semantic[0], tree)
		if value, ok := w.valuesBySyntax[semantic[0]]; ok && value.ID != 0 && w.successfulExpressions[semantic[0]] {
			emission.values = append(emission.values, controlValue{Role: valueDiscarded, Value: value.ID})
		}
	}
	w.retainControl(ref, ctx, emission)
}

func (w *walker) finishPrint(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) {
	emission := controlEmission{kind: controlPrint, form: statementPrint, region: ctx.control.region}
	// The AST node retains the exact opening keyword (`print` vs `println`)
	// through its stored token; only the trailing-newline bit differs between
	// the two.
	emission.printNewline = node.Token() == syntax.KwPrintln
	for ordinal, operand := range semanticRefs(ref.Module, node, tree) {
		value, ok := w.valuesBySyntax[operand]
		if !ok || value.ID == 0 || !w.successfulExpressions[operand] {
			continue
		}
		emission.values = append(emission.values, controlValue{Role: valuePrintOperand, Value: value.ID, Ordinal: uint32(ordinal)})
	}
	w.retainControl(ref, ctx, emission)
}

// callableResult reads the declared result of the callable owning a return. A
// named callable uses its prepared signature; an anonymous one uses the memoized
// concrete resolution of its authored result occurrence.
func (w *walker) callableResult(ctx walkContext, origin infer.Origin) (infer.Term, types.TypeID, bool) {
	if ctx.callable.Symbol != 0 {
		signature, ok := w.program.Signature(ctx.callable.Symbol)
		if !ok || signature.State != infer.DeclarationReady {
			return infer.Term{}, 0, false
		}
		known := types.TypeID(0)
		if template, found := w.program.Template(signature.Result); found && template.Kind == infer.TemplateKnown {
			known = template.Known
		}
		return w.termForTemplate(signature.Result, signature.TypeParams, origin), known, true
	}
	if ctx.callable.Syntax == (symbol.SyntaxRef{}) {
		return infer.Term{}, 0, false
	}
	node, ok := w.node(ctx.callable.Syntax.Module, ctx.callable.Syntax.Node)
	if !ok {
		return infer.Term{}, 0, false
	}
	item, found := w.generation.inputs.Graph.Module(ctx.callable.Syntax.Module)
	if !found || item.Tree == nil {
		return infer.Term{}, 0, false
	}
	_, _, resultNode, _ := functionParts(item.Tree, node)
	if resultNode == 0 {
		return infer.Term{}, 0, false
	}
	result := w.session.ResolveType(symbol.SyntaxRef{Module: ctx.callable.Syntax.Module, Node: resultNode}, ctx.typeOwner)
	if result.State != infer.TypeFinal {
		return infer.Term{}, 0, false
	}
	return w.session.Known(result.Type), result.Type, true
}

func (w *walker) prepareReturn(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem) []walkItem {
	semantic := semanticRefs(ref.Module, node, tree)
	if len(semantic) == 0 {
		return items
	}
	origin := w.originForRef(semantic[0], "return value", ctx.callable.Symbol, ctx.genericOwner)
	term, known, ok := w.callableResult(ctx, origin)
	if !ok {
		return items
	}
	destination, published := w.newSlotValue(term, origin)
	if !published {
		return items
	}
	if known != 0 {
		destination.Known = known
		w.knownValues[destination.ID] = known
	}
	w.expectations[semantic[0]] = w.expectationFor(semantic[0], destination.ID, compatibilityReturn)
	return items
}

func (w *walker) finishReturn(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) {
	semantic := semanticRefs(ref.Module, node, tree)
	emission := controlEmission{kind: controlReturn, form: statementOther, region: ctx.control.region}
	if len(semantic) == 0 {
		origin := w.originForRef(ref, "bare return", ctx.callable.Symbol, ctx.genericOwner)
		if term, _, ok := w.callableResult(ctx, origin); ok {
			w.addConstraint(infer.Equal(term, w.session.Known(w.generation.inputs.Types.Builtins().Void), origin))
		}
		w.retainControl(ref, ctx, emission)
		return
	}
	value, found := w.valuesBySyntax[semantic[0]]
	if found && value.ID != 0 && w.successfulExpressions[semantic[0]] {
		emission.values = append(emission.values, controlValue{Role: valueReturn, Value: value.ID})
		if expected, ok := w.expectations[semantic[0]]; ok && expected.Destination != 0 && expected.Role == compatibilityReturn {
			w.retainCompatibility(ref, ctx.genericOwner, value.ID, expected.Destination, compatibilityReturn, 0,
				ctx.callable.Symbol, spanForRef(w.generation.inputs, semantic[0]), false)
		}
	}
	w.retainControl(ref, ctx, emission)
}

// finishExpressionBodyReturn retains the implicit return record for a named
// function's expression body (`fn f() T => expr;`). The body expression node is
// not a statement, so it never produces a ReturnStmt control record of its own;
// this synthesizes exactly the controlReturn shape finishReturn's success path
// retains for a real `return expr;`, including the return compatibility wiring,
// so 06b's IR builder produces the same populated body it builds for the
// block-bodied equivalent.
func (w *walker) finishExpressionBodyReturn(ref symbol.SyntaxRef, ctx walkContext) {
	emission := controlEmission{kind: controlReturn, form: statementOther, region: ctx.control.region, syntheticSyntax: true}
	value, found := w.valuesBySyntax[ref]
	if found && value.ID != 0 && w.successfulExpressions[ref] {
		emission.values = append(emission.values, controlValue{Role: valueReturn, Value: value.ID})
		if expected, ok := w.expectations[ref]; ok && expected.Destination != 0 && expected.Role == compatibilityReturn {
			w.retainCompatibility(ref, ctx.genericOwner, value.ID, expected.Destination, compatibilityReturn, 0,
				ctx.callable.Symbol, spanForRef(w.generation.inputs, ref), false)
		}
	}
	w.retainControl(ref, ctx, emission)
}
