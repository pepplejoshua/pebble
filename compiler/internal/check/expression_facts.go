package check

import (
	"bytes"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type literalKind uint8

const (
	literalInteger literalKind = iota + 1
	literalFloat
	literalBool
	literalChar
	literalString
	literalNil
	literalNone
)

type interpolationPartKind uint8

const (
	interpolationText interpolationPartKind = iota + 1
	interpolationValue
)

type expressionKind uint8

const (
	expressionName expressionKind = iota + 1
	expressionPath
	expressionLiteral
	expressionInterpolated
	expressionContext
	expressionSome
	expressionSliceFrom
	expressionSizeof
	expressionGrouped
	expressionTuple
	expressionArray
	expressionArrayRepeat
	expressionRecordValue
	expressionFunction
	expressionPartialMember
	expressionPrefix
	expressionPostfix
	expressionBinary
	expressionCast
	expressionCall
	expressionBracket
	expressionSlice
	expressionMember
)

type literalPayload struct {
	Kind         literalKind
	NumericBytes []byte
	Bool         bool
	Rune         rune
	Text         string
}

type interpolationPart struct {
	Kind  interpolationPartKind
	Span  source.Span
	Text  string
	Value valueID
}

type expressionRecord struct {
	Header       recordHeader
	Kind         expressionKind
	Result       valueID
	Children     []valueID
	Symbol       symbol.SymbolID
	Literal      literalPayload
	Parts        []interpolationPart
	Specialized  recordID
	TypeArgument valueID
}

type expressionPlan struct {
	result       typedValue
	children     []symbol.SyntaxRef
	exactLiteral infer.Term
	literal      literalPayload
	parts        []interpolationPart
	symbol       symbol.SymbolID
	specialized  recordID
	arrayElement typedValue
	arrayLength  uint64
	arrayKnown   bool
	typeValue    typedValue
	slicePointer symbol.SyntaxRef
	sliceCount   symbol.SyntaxRef
	record       *recordPlan
}

func cloneLiteralPayload(value literalPayload) literalPayload {
	value.NumericBytes = bytes.Clone(value.NumericBytes)
	return value
}

func cloneInterpolationParts(values []interpolationPart) []interpolationPart {
	out := append([]interpolationPart(nil), values...)
	return out
}

func cloneExpressionRecord(value expressionRecord) expressionRecord {
	value.Children = append([]valueID(nil), value.Children...)
	value.Literal = cloneLiteralPayload(value.Literal)
	value.Parts = cloneInterpolationParts(value.Parts)
	return value
}

func (w *walker) prepareExpression(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
	if node.Kind() == syntax.RecordField {
		for i := range items {
			if i == 0 {
				items[i].ctx.suppressValue = true
			}
		}
		return items
	}
	if ctx.suppressValue {
		for i := range items {
			items[i].ctx.suppressValue = true
		}
		return items
	}
	plan := &expressionPlan{}
	w.expressionPlans[ref] = plan
	children := node.Children()
	suppressAll := func() {
		for i := range items {
			items[i].ctx.suppressValue = true
		}
	}
	switch node.Kind() {
	case syntax.Name, syntax.Literal, syntax.PartialMemberExpr:
		if node.Kind() == syntax.PartialMemberExpr {
			suppressAll()
		}
	case syntax.Path:
		suppressAll()
	case syntax.InterpolatedString:
		for i := range items {
			child, _ := tree.Node(items[i].ref.Node)
			if child.Kind() == syntax.Literal && child.Token() == syntax.InterpolationText {
				items[i].ctx.suppressValue = true
			} else if child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				plan.children = append(plan.children, items[i].ref)
			}
		}
	case syntax.SomeExpr, syntax.GroupedTerm:
		for i := range items {
			child, _ := tree.Node(items[i].ref.Node)
			if len(plan.children) == 0 && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				plan.children = append(plan.children, items[i].ref)
			} else if child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				items[i].ctx.suppressValue = true
			}
		}
		if node.Kind() == syntax.SomeExpr && len(plan.children) != 0 {
			if id, ok := w.knownDestination(ctx.expected); ok {
				if key, found := w.generation.inputs.Types.Key(id); found && key.Kind() == types.Optional {
					if payload, childOK := key.Child(); childOK {
						childRef := plan.children[0]
						childOrigin := w.originForRef(childRef, "some payload", ctx.typeOwner, ctx.genericOwner)
						destination, published := w.newSlotValue(w.session.Known(payload), childOrigin)
						if published {
							destination.Known = payload
							w.knownValues[destination.ID] = payload
							w.expectations[childRef] = w.expectationFor(childRef, destination.ID, compatibilityOptionalInjection)
						}
					}
				}
			}
		}
	case syntax.SizeofExpr:
		suppressAll()
		for _, id := range children {
			child, _ := tree.Node(id)
			if child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				plan.typeValue = w.resolveTypeUse(symbol.SyntaxRef{Module: ref.Module, Node: id}, ctx.typeOwner, ctx.genericOwner, "sizeof type", typeUseSizeof)
				break
			}
		}
	case syntax.SliceFromExpr:
		for _, item := range items {
			child, _ := tree.Node(item.ref.Node)
			if child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				plan.children = append(plan.children, item.ref)
			}
		}
		if len(plan.children) >= 2 {
			plan.slicePointer, plan.sliceCount = plan.children[0], plan.children[1]
		}
	case syntax.TupleTerm:
		w.prepareTuple(ref, node, ctx, tree, items, plan)
	case syntax.ArrayExpr:
		w.prepareArray(ref, node, ctx, tree, items, plan)
	case syntax.ArrayRepeatExpr:
		w.prepareArrayRepeat(ref, node, ctx, tree, items, plan)
	case syntax.RecordExpr:
		w.prepareRecord(ref, node, ctx, tree, items, plan)
	case syntax.PrefixTerm, syntax.PostfixExpr, syntax.BinaryExpr:
		for i := range items {
			child, _ := tree.Node(items[i].ref.Node)
			if child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				plan.children = append(plan.children, items[i].ref)
			}
		}
	}
	return items
}

func (w *walker) finishExpression(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) {
	if ctx.suppressValue {
		return
	}
	plan := w.expressionPlans[ref]
	if plan == nil {
		if node.Kind() == syntax.ContextExpr || node.Kind() == syntax.FunctionTerm {
			w.retainExistingExpression(ref, node, ctx)
		}
		return
	}
	if node.Kind() == syntax.RecordExpr {
		w.finishRecord(ref, node, ctx, tree, plan)
		return
	}
	origin := w.origin(ref, node, "expression result", ctx.typeOwner, ctx.genericOwner)
	value := plan.result
	kind := expressionKind(0)
	children := make([]valueID, 0, len(plan.children))
	for _, child := range plan.children {
		if childValue, ok := w.valuesBySyntax[child]; ok && childValue.ID != 0 {
			children = append(children, childValue.ID)
		}
		if !w.successfulExpressions[child] {
			w.failExpression(ref, origin)
			return
		}
	}
	switch node.Kind() {
	case syntax.Name, syntax.Path:
		kind = expressionName
		query := ref
		if node.Kind() == syntax.Path {
			kind = expressionPath
			for i := len(node.Children()) - 1; i >= 0; i-- {
				child, _ := tree.Node(node.Children()[i])
				if child.Kind() == syntax.Name {
					query.Node = node.Children()[i]
					break
				}
			}
		}
		resolution, ok := w.generation.inputs.Resolution.Reference(query)
		if !ok || resolution.State != symbol.ResolutionResolved || resolution.Symbol == 0 || !w.successfulValueSymbol(resolution.Symbol) {
			w.failExpression(ref, origin)
			return
		}
		plan.symbol = resolution.Symbol
		term := w.symbolTerm(resolution.Symbol, origin)
		value = w.expressionResult(ref, term, origin)
		w.addConstraint(infer.Equal(value.Term, term, origin))
		if known := w.valuesBySymbol[resolution.Symbol].Known; known != 0 {
			value.Known = known
			w.knownValues[value.ID] = known
			w.valuesBySyntax[ref] = value
		}
		if symbolValue := w.valuesBySymbol[resolution.Symbol]; w.rigidValues[symbolValue.ID] {
			w.rigidValues[value.ID] = true
		}
		if plan.symbol != 0 {
			if resolved, found := w.generation.inputs.Resolution.Symbols.Symbol(plan.symbol); found {
				switch resolved.Kind {
				case symbol.SymbolBinding, symbol.SymbolParameter, symbol.SymbolLoopBinding, symbol.SymbolExternBinding:
					if w.generation.trackPlace() {
						w.placeCandidates[ref] = value.ID
						w.storagePlace(ref, value.ID, resolution.Symbol)
					}
				}
			}
		}
	case syntax.Literal:
		kind = expressionLiteral
		value = w.finishLiteral(ref, node, ctx, plan, origin)
	case syntax.InterpolatedString:
		kind = expressionInterpolated
		value = w.expressionResult(ref, w.session.Known(w.generation.inputs.Types.Builtins().Str), origin)
		for _, id := range node.Children() {
			child, _ := tree.Node(id)
			if child.Kind() == syntax.Literal && child.Token() == syntax.InterpolationText {
				decoded, ok := child.DecodedLiteral()
				if ok {
					plan.parts = append(plan.parts, interpolationPart{Kind: interpolationText, Span: child.Span(), Text: string([]byte(decoded.Text))})
				}
			} else if childValue, ok := w.valuesBySyntax[symbol.SyntaxRef{Module: ref.Module, Node: id}]; ok && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				plan.parts = append(plan.parts, interpolationPart{Kind: interpolationValue, Span: child.Span(), Value: childValue.ID})
			}
		}
	case syntax.SomeExpr:
		kind = expressionSome
		value = w.expressionResult(ref, w.session.Variable(origin), origin)
		child := w.firstChildValue(plan)
		w.addConstraint(infer.ConstrainShape(value.Term, infer.OptionalShape(infer.Leaf(child.Term)), origin))
	case syntax.SizeofExpr:
		kind = expressionSizeof
		value = w.expressionResult(ref, w.session.Known(w.generation.inputs.Types.Builtins().Uint), origin)
	case syntax.SliceFromExpr:
		kind = expressionSliceFrom
		if plan.slicePointer == (symbol.SyntaxRef{}) || plan.sliceCount == (symbol.SyntaxRef{}) {
			w.failExpression(ref, origin)
			return
		}
		pointer := w.valuesBySyntax[plan.slicePointer]
		count := w.valuesBySyntax[plan.sliceCount]
		if pointer.ID == 0 || count.ID == 0 || !w.successfulExpressions[plan.slicePointer] || !w.successfulExpressions[plan.sliceCount] {
			w.failExpression(ref, origin)
			return
		}
		item, ok := w.generation.inputs.Graph.Module(ref.Module)
		if !ok || item.Key.Package != module.StandardPackage {
			w.generation.report("slice is restricted to the standard library package", node.Span())
			w.failExpression(ref, origin)
			return
		}
		pointee := w.session.Variable(origin)
		w.addConstraint(infer.ConstrainShape(pointer.Term, infer.PointerShape(infer.Leaf(pointee)), origin))
		w.addConstraint(infer.Integral(count.Term, w.originForRef(plan.sliceCount, "slice count", ctx.typeOwner, ctx.genericOwner)))
		result := w.session.Variable(origin)
		w.addConstraint(infer.ConstrainShape(result, infer.SliceShape(infer.Leaf(pointee)), origin))
		value = w.expressionResult(ref, result, origin)
	case syntax.GroupedTerm:
		kind = expressionGrouped
		child := w.firstChildValue(plan)
		value = w.expressionResult(ref, child.Term, origin)
		if child.Known != 0 {
			value.Known = child.Known
			w.knownValues[value.ID] = child.Known
			w.valuesBySyntax[ref] = value
		}
		if w.rigidValues[child.ID] {
			w.rigidValues[value.ID] = true
		}
		w.addConstraint(infer.Equal(value.Term, child.Term, origin))
		if len(plan.children) != 0 {
			if _, place := w.places[plan.children[0]]; place && w.generation.trackPlace() {
				w.placeCandidates[ref] = value.ID
				w.copyPlace(ref, plan.children[0], value.ID)
			}
		}
	case syntax.TupleTerm:
		kind = expressionTuple
		value = w.finishTuple(ref, node, ctx, plan, origin)
	case syntax.ArrayExpr:
		kind = expressionArray
		value = w.finishArray(ref, node, ctx, plan, origin)
	case syntax.ArrayRepeatExpr:
		kind = expressionArrayRepeat
		value = w.finishArrayRepeat(ref, node, ctx, plan, origin)
	case syntax.PartialMemberExpr:
		kind = expressionPartialMember
		value = w.finishPartialMember(ref, node, ctx, tree, plan, origin)
	case syntax.PrefixTerm:
		kind = expressionPrefix
		value = w.expressionResult(ref, w.session.Variable(origin), origin)
	case syntax.PostfixExpr:
		kind = expressionPostfix
		value = w.expressionResult(ref, w.session.Variable(origin), origin)
	case syntax.BinaryExpr:
		kind = expressionBinary
		value = w.expressionResult(ref, w.session.Variable(origin), origin)
	default:
		return
	}
	if value.ID == 0 || !w.publishedSyntax[ref] {
		return
	}
	if node.Kind() == syntax.PartialMemberExpr && plan.specialized == 0 {
		return
	}
	w.successfulExpressions[ref] = true
	plan.result = value
	w.applyExpected(value, plan.exactLiteral, ctx.expected, w.origin(ref, node, expectedRoleText(ctx.expected.Role, 0), ctx.typeOwner, ctx.genericOwner))
	if destination := w.optionalDestinations[ref]; destination != 0 {
		w.retainCompatibility(ref, ctx.genericOwner, value.ID, destination, compatibilityOptionalInjection, 0, 0, spanForRef(w.generation.inputs, ref), false)
	}
	if node.Kind() == syntax.SomeExpr && len(plan.children) != 0 {
		child := w.valuesBySyntax[plan.children[0]]
		if expected, ok := w.expectations[plan.children[0]]; ok && expected.Destination != 0 {
			w.retainCompatibility(ref, ctx.genericOwner, child.ID, expected.Destination, compatibilityOptionalInjection, 0, 0, spanForRef(w.generation.inputs, plan.children[0]), false)
		}
	}
	header := w.header(ref, ctx.genericOwner, !w.publishedSyntax[ref])
	record := expressionRecord{Header: header, Kind: kind, Result: value.ID, Children: children, Symbol: plan.symbol, Literal: plan.literal, Parts: plan.parts, Specialized: plan.specialized, TypeArgument: plan.typeValue.ID}
	w.addRecord(retainedRecord{Header: header, Expression: &record})
}

func (w *walker) expressionResult(ref symbol.SyntaxRef, term infer.Term, origin infer.Origin) typedValue {
	if existing, ok := w.valuesBySyntax[ref]; ok && existing.ID != 0 {
		w.addConstraint(infer.Equal(existing.Term, term, origin))
		if !w.publishedSyntax[ref] {
			existing, _ = w.publishExistingSyntax(ref, existing, origin)
		}
		return existing
	}
	value := w.newValue(term, origin)
	value, _ = w.publishExistingSyntax(ref, value, origin)
	return value
}

func (w *walker) failExpression(ref symbol.SyntaxRef, origin infer.Origin) typedValue {
	errorTerm := w.session.Error(origin)
	if existing, ok := w.valuesBySyntax[ref]; ok && existing.ID != 0 {
		w.addConstraint(infer.Equal(existing.Term, errorTerm, origin))
		existing, _ = w.rootExistingSlot(existing, origin)
		w.successfulExpressions[ref] = false
		return existing
	}
	value, _ := w.newSlotValue(errorTerm, origin)
	w.valuesBySyntax[ref] = value
	w.successfulExpressions[ref] = false
	return value
}

func (w *walker) successfulValueSymbol(id symbol.SymbolID) bool {
	value, ok := w.generation.inputs.Resolution.Symbols.Symbol(id)
	if !ok || value.Error {
		return false
	}
	switch value.Kind {
	case symbol.SymbolBinding, symbol.SymbolParameter, symbol.SymbolLoopBinding,
		symbol.SymbolField, symbol.SymbolVariant, symbol.SymbolExternBinding:
		return true
	case symbol.SymbolFunction, symbol.SymbolMethod, symbol.SymbolExternFunction, symbol.SymbolBuiltinFunction:
		return !value.Generic
	default:
		return false
	}
}

func (w *walker) finishLiteral(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, plan *expressionPlan, origin infer.Origin) typedValue {
	b := w.generation.inputs.Types.Builtins()
	term := w.session.Error(origin)
	switch node.Token() {
	case syntax.IntegerLiteral:
		plan.literal.Kind = literalInteger
		plan.literal.NumericBytes = w.copySource(node.Span())
		plan.exactLiteral = w.session.IntegerLiteral(plan.literal.NumericBytes, origin)
		term = plan.exactLiteral
	case syntax.FloatLiteral:
		plan.literal.Kind = literalFloat
		plan.literal.NumericBytes = w.copySource(node.Span())
		plan.exactLiteral = w.session.FloatLiteral(plan.literal.NumericBytes, origin)
		term = plan.exactLiteral
	case syntax.KwTrue, syntax.KwFalse:
		plan.literal.Kind, plan.literal.Bool = literalBool, node.Token() == syntax.KwTrue
		term = w.session.Known(b.Bool)
	case syntax.CharacterLiteral:
		plan.literal.Kind = literalChar
		if decoded, ok := node.DecodedLiteral(); ok {
			plan.literal.Rune = decoded.Rune
		}
		term = w.session.Known(b.Char)
	case syntax.StringLiteral:
		plan.literal.Kind = literalString
		if decoded, ok := node.DecodedLiteral(); ok {
			plan.literal.Text = string([]byte(decoded.Text))
		}
		term = w.session.Known(b.Str)
	case syntax.KwNil:
		plan.literal.Kind = literalNil
		pointee := w.shapeLeaf(ctx.expected, types.Pointer, origin)
		term = w.session.Variable(origin)
		w.addConstraint(infer.ConstrainShape(term, infer.PointerShape(infer.Leaf(pointee.Term)), origin))
	case syntax.KwNone:
		plan.literal.Kind = literalNone
		payload := w.shapeLeaf(ctx.expected, types.Optional, origin)
		term = w.session.Variable(origin)
		w.addConstraint(infer.ConstrainShape(term, infer.OptionalShape(infer.Leaf(payload.Term)), origin))
	}
	value := w.expressionResult(ref, term, origin)
	switch plan.literal.Kind {
	case literalBool:
		value.Known = b.Bool
	case literalChar:
		value.Known = b.Char
	case literalString:
		value.Known = b.Str
	}
	if value.Known != 0 {
		w.knownValues[value.ID] = value.Known
		w.valuesBySyntax[ref] = value
	}
	return value
}

func (w *walker) shapeLeaf(expected expectedType, kind types.Kind, origin infer.Origin) typedValue {
	var knownChild types.TypeID
	hasKnownChild := false
	if id, ok := w.knownDestination(expected); ok {
		if key, found := w.generation.inputs.Types.Key(id); found && key.Kind() == kind {
			if child, childOK := key.Child(); childOK {
				knownChild, hasKnownChild = child, true
			}
		}
	}
	// Create exactly one term, and only the one actually needed: session.Known
	// and session.Variable both have real session-mutating side effects
	// (Variable registers a solver cell that must later be resolved through
	// unification or it is reported as a spurious unresolved-variable error).
	// Calling session.Variable unconditionally here and then discarding it in
	// favor of session.Known whenever a known destination shape existed — as
	// this function used to do — left an orphaned, never-bound cell behind
	// that the solver's finalizeUnresolved pass reported as a spurious T0510
	// ("inference variable has no unique semantic type") for both `nil`
	// against a known pointer destination and `none` against a known
	// optional destination (both callers of this helper). See the identical
	// fix and postmortem for prepareArray in aggregate_facts.go (commit
	// 4a479e8) for the full story, including a first attempt at that fix
	// that made the same mistake with the two term constructors swapped.
	var term infer.Term
	if hasKnownChild {
		term = w.session.Known(knownChild)
	} else {
		term = w.session.Variable(origin)
	}
	value, _ := w.newSlotValue(term, origin)
	return value
}

func (w *walker) firstChildValue(plan *expressionPlan) typedValue {
	if len(plan.children) != 0 {
		return w.valuesBySyntax[plan.children[0]]
	}
	return typedValue{Term: w.session.Error(infer.Origin{})}
}

func (w *walker) copySource(span source.Span) []byte {
	file, ok := w.generation.inputs.Sources.File(span.Source)
	if !ok {
		return nil
	}
	return bytes.Clone(file.Slice(span))
}

func (w *walker) retainExistingExpression(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	value, ok := w.valuesBySyntax[ref]
	if !ok || value.ID == 0 || !w.publishedSyntax[ref] || !w.successfulExpressions[ref] {
		return
	}
	if node.Kind() == syntax.FunctionTerm && node.Data()&syntax.FunctionExpressionBody != 0 {
		item, _ := w.generation.inputs.Graph.Module(ref.Module)
		_, _, _, body := functionParts(item.Tree, node)
		if body == 0 || !w.successfulExpressions[symbol.SyntaxRef{Module: ref.Module, Node: body}] {
			w.successfulExpressions[ref] = false
			return
		}
	}
	kind := expressionContext
	if node.Kind() == syntax.FunctionTerm {
		kind = expressionFunction
	}
	header := w.header(ref, ctx.genericOwner, false)
	record := expressionRecord{Header: header, Kind: kind, Result: value.ID}
	w.addRecord(retainedRecord{Header: header, Expression: &record})
}
