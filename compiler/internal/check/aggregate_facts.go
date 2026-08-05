package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type aggregateKind uint8

const (
	aggregateStruct aggregateKind = iota + 1
	aggregateEnumVariant
	aggregateTaggedVariant
)

type fieldValue struct {
	Field, NameSyntax  symbol.SyntaxRef
	Name               string
	NameSpan           source.Span
	Member             symbol.SymbolID
	Value, Destination valueID
	Ordinal            uint32
}

type aggregateRecord struct {
	Header            recordHeader
	Kind              aggregateKind
	Result, Receiver  valueID
	Declaration       symbol.SymbolID
	Fields            []fieldValue
	DeclarationFields []symbol.SymbolID
}

type recordFieldPlan struct {
	field       symbol.SyntaxRef
	name        symbol.SyntaxRef
	value       symbol.SyntaxRef
	nameText    string
	nameSpan    source.Span
	member      symbol.SymbolID
	destination typedValue
	ordinal     uint32
}

type recordPlan struct {
	receiver          typedValue
	declaration       symbol.SymbolID
	declarationFields []symbol.SymbolID
	fields            []recordFieldPlan
}

func cloneAggregateRecord(value aggregateRecord) aggregateRecord {
	value.Fields = append([]fieldValue(nil), value.Fields...)
	value.DeclarationFields = append([]symbol.SymbolID(nil), value.DeclarationFields...)
	return value
}

func (w *walker) prepareTuple(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem, plan *expressionPlan) {
	var expectedElements []types.TypeID
	if id, ok := w.knownDestination(ctx.expected); ok {
		if key, found := w.generation.inputs.Types.Key(id); found {
			expectedElements, _ = key.Elements()
		}
	}
	ordinal := uint32(0)
	for i := range items {
		child, _ := tree.Node(items[i].ref.Node)
		if child.Kind() == syntax.Missing || child.Kind() == syntax.Error {
			continue
		}
		plan.children = append(plan.children, items[i].ref)
		if int(ordinal) < len(expectedElements) {
			origin := w.originForRef(items[i].ref, expectedRoleText(compatibilityTupleComponent, ordinal), ctx.typeOwner, ctx.genericOwner)
			destination, _ := w.newSlotValue(w.session.Known(expectedElements[ordinal]), origin)
			w.knownValues[destination.ID] = expectedElements[ordinal]
			w.expectations[items[i].ref] = w.expectationFor(items[i].ref, destination.ID, compatibilityTupleComponent)
		}
		ordinal++
	}
}

func (w *walker) finishTuple(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, plan *expressionPlan, origin infer.Origin) typedValue {
	result := w.expressionResult(ref, w.session.Variable(origin), origin)
	shapes := make([]infer.Shape, 0, len(plan.children))
	for ordinal, childRef := range plan.children {
		child := w.valuesBySyntax[childRef]
		if child.ID == 0 {
			continue
		}
		shapes = append(shapes, infer.Leaf(child.Term))
		if expected, ok := w.expectations[childRef]; ok && expected.Destination != 0 {
			w.retainCompatibility(ref, ctx.genericOwner, child.ID, expected.Destination, compatibilityTupleComponent, uint32(ordinal), 0, spanForRef(w.generation.inputs, childRef), false)
		}
	}
	if len(shapes) != 0 {
		w.addConstraint(infer.ConstrainShape(result.Term, infer.TupleShape(shapes), origin))
	}
	return result
}

func (w *walker) prepareArray(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem, plan *expressionPlan) {
	origin := w.origin(ref, node, "array element destination", ctx.typeOwner, ctx.genericOwner)
	var knownElement types.TypeID
	hasKnownElement := false
	if id, ok := w.knownDestination(ctx.expected); ok {
		if key, found := w.generation.inputs.Types.Key(id); found {
			if _, element, array := key.Array(); array {
				knownElement, hasKnownElement = element, true
			}
		}
	}
	// Create exactly one term for the element slot, and only that one:
	// session.Known and session.Variable both have real session-mutating
	// side effects (Variable registers a solver cell that must later be
	// resolved or it is reported as a spurious unresolved-variable error;
	// Known runs its own mutability/resource-limit bookkeeping), so calling
	// either of them "just in case" and then discarding the result in favor
	// of the other — as this function used to do unconditionally for
	// Variable, and as an earlier attempt at this fix did unconditionally
	// for Known — leaves unwanted session state behind. prepareTuple never
	// had this bug because it never creates a term it doesn't use.
	var elementTerm infer.Term
	if hasKnownElement {
		elementTerm = w.session.Known(knownElement)
	} else {
		elementTerm = w.session.Variable(origin)
	}
	plan.arrayElement, _ = w.newSlotValue(elementTerm, origin)
	if hasKnownElement {
		plan.arrayElement.Known = knownElement
		w.knownValues[plan.arrayElement.ID] = knownElement
	}
	for i := range items {
		child, _ := tree.Node(items[i].ref.Node)
		if child.Kind() == syntax.Missing || child.Kind() == syntax.Error {
			continue
		}
		plan.children = append(plan.children, items[i].ref)
		w.expectations[items[i].ref] = w.expectationFor(items[i].ref, plan.arrayElement.ID, compatibilityTupleComponent)
	}
}

func (w *walker) finishArray(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, plan *expressionPlan, origin infer.Origin) typedValue {
	plan.arrayKnown, plan.arrayLength = true, uint64(len(plan.children))
	result := w.expressionResult(ref, w.session.Variable(origin), origin)
	for ordinal, childRef := range plan.children {
		child := w.valuesBySyntax[childRef]
		if child.ID == 0 {
			continue
		}
		w.addConstraint(infer.Equal(child.Term, plan.arrayElement.Term, w.originForRef(childRef, "array element", ctx.typeOwner, ctx.genericOwner)))
		w.retainCompatibility(ref, ctx.genericOwner, child.ID, plan.arrayElement.ID, compatibilityTupleComponent, uint32(ordinal), 0, spanForRef(w.generation.inputs, childRef), false)
	}
	w.addConstraint(infer.ConstrainShape(result.Term, infer.ArrayShape(uint64(len(plan.children)), infer.Leaf(plan.arrayElement.Term)), origin))
	return result
}

func (w *walker) prepareArrayRepeat(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem, plan *expressionPlan) {
	semantic := uint32(0)
	for i := range items {
		child, _ := tree.Node(items[i].ref.Node)
		if child.Kind() == syntax.Missing || child.Kind() == syntax.Error {
			continue
		}
		if semantic == 0 {
			plan.children = append(plan.children, items[i].ref)
			if id, ok := w.knownDestination(ctx.expected); ok {
				if key, found := w.generation.inputs.Types.Key(id); found {
					if _, element, array := key.Array(); array {
						childOrigin := w.originForRef(items[i].ref, "array repeat value", ctx.typeOwner, ctx.genericOwner)
						destination, published := w.newSlotValue(w.session.Known(element), childOrigin)
						if published {
							destination.Known = element
							w.knownValues[destination.ID] = element
							w.expectations[items[i].ref] = w.expectationFor(items[i].ref, destination.ID, compatibilityTupleComponent)
						}
					}
				}
			}
		} else {
			items[i].ctx.suppressValue = true
			length := w.evaluator.ArrayLength(items[i].ref)
			plan.arrayKnown = length.State == infer.ArrayLengthKnown
			plan.arrayLength = length.Value
		}
		semantic++
	}
}

func (w *walker) finishArrayRepeat(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, plan *expressionPlan, origin infer.Origin) typedValue {
	result := w.expressionResult(ref, w.session.Variable(origin), origin)
	child := w.firstChildValue(plan)
	if plan.arrayKnown {
		w.addConstraint(infer.ConstrainShape(result.Term, infer.ArrayShape(plan.arrayLength, infer.Leaf(child.Term)), origin))
	}
	if len(plan.children) != 0 {
		if expected, ok := w.expectations[plan.children[0]]; ok && expected.Destination != 0 {
			w.retainCompatibility(ref, ctx.genericOwner, child.ID, expected.Destination, compatibilityTupleComponent, 0, 0, spanForRef(w.generation.inputs, plan.children[0]), false)
		}
	}
	return result
}

func (w *walker) prepareRecord(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem, plan *expressionPlan) {
	rp := &recordPlan{}
	plan.record = rp
	children := node.Children()
	fieldOrdinal := uint32(0)
	for i, id := range children {
		child, _ := tree.Node(id)
		if child.Kind() != syntax.RecordField {
			if child.Kind() != syntax.Missing && child.Kind() != syntax.Error && rp.receiver.ID == 0 {
				items[i].ctx.suppressValue = true
				baseRef := symbol.SyntaxRef{Module: ref.Module, Node: id}
				rp.receiver = w.resolveUnrecordedType(baseRef, ctx.typeOwner, ctx.genericOwner, "record declaration")
				if rp.receiver.Known != 0 {
					rp.declaration, rp.declarationFields = w.nominalDeclaration(rp.receiver.Known)
				}
			}
			continue
		}
		fieldRef := symbol.SyntaxRef{Module: ref.Module, Node: id}
		field := recordFieldPlan{field: fieldRef, ordinal: fieldOrdinal}
		fieldChildren := child.Children()
		if len(fieldChildren) != 0 {
			field.name = symbol.SyntaxRef{Module: ref.Module, Node: fieldChildren[0]}
			nameNode, _ := tree.Node(fieldChildren[0])
			field.nameSpan = nameNode.Span()
			if nameNode.Kind() == syntax.Name {
				field.nameText = string(w.copySource(nameNode.Span()))
			}
		}
		if len(fieldChildren) > 1 {
			field.value = symbol.SyntaxRef{Module: ref.Module, Node: fieldChildren[len(fieldChildren)-1]}
		}
		origin := w.originForRef(fieldRef, expectedRoleText(compatibilityRecordField, fieldOrdinal), ctx.typeOwner, ctx.genericOwner)
		field.destination, _ = w.newSlotValue(w.session.Variable(origin), origin)
		if field.nameText != "" {
			w.addConstraint(infer.HasField(w.recordReceiverTerm(ctx, rp, origin), field.nameText, field.destination.Term, origin))
		}
		if field.value != (symbol.SyntaxRef{}) {
			w.expectations[field.value] = w.expectationFor(field.value, field.destination.ID, compatibilityRecordField)
		}
		if resolved, ok := w.generation.inputs.Resolution.Reference(field.name); ok && resolved.State == symbol.ResolutionResolved {
			field.member = resolved.Symbol
		}
		rp.fields = append(rp.fields, field)
		fieldOrdinal++
	}
}

func (w *walker) recordReceiverTerm(ctx walkContext, plan *recordPlan, origin infer.Origin) infer.Term {
	if plan.receiver.ID != 0 {
		return plan.receiver.Term
	}
	if id, ok := w.knownDestination(ctx.expected); ok {
		term := w.session.Known(id)
		plan.receiver, _ = w.newSlotValue(term, origin)
		plan.receiver.Known = id
		w.knownValues[plan.receiver.ID] = id
		plan.declaration, plan.declarationFields = w.nominalDeclaration(id)
		return term
	}
	plan.receiver, _ = w.newSlotValue(w.session.Variable(origin), origin)
	return plan.receiver.Term
}

// recordConstructionKind selects the aggregate record kind for a .{ } literal.
// A single-field literal against a tagged-union (union enum) destination is a
// variant construction (.{ Int = 42 } is sugar for Data.Int(42)) and builds an
// aggregateTaggedVariant; every other shape is an ordinary aggregateStruct.
// Multi-field literals against a tagged union stay aggregateStruct so the
// existing rejection path reports them.
func (w *walker) recordConstructionKind(rp *recordPlan) aggregateKind {
	if len(rp.fields) != 1 {
		return aggregateStruct
	}
	if rp.declaration != 0 {
		if declaration, ok := w.program.TypeDeclaration(rp.declaration); ok && declaration.Nominal == infer.NominalTaggedUnion {
			return aggregateTaggedVariant
		}
	}
	return aggregateStruct
}

func (w *walker) finishRecord(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, plan *expressionPlan) {
	origin := w.origin(ref, node, "record result", ctx.typeOwner, ctx.genericOwner)
	rp := plan.record
	if rp == nil {
		return
	}
	receiver := w.recordReceiverTerm(ctx, rp, origin)
	kind := w.recordConstructionKind(rp)
	fields := make([]fieldValue, 0, len(rp.fields))
	runtime := make([]valueID, 0, len(rp.fields))
	childrenSuccessful := true
	for _, field := range rp.fields {
		value := w.valuesBySyntax[field.value]
		if value.ID == 0 {
			childrenSuccessful = false
			continue
		}
		childrenSuccessful = childrenSuccessful && w.successfulExpressions[field.value]
		fields = append(fields, fieldValue{Field: field.field, NameSyntax: field.name, Name: string([]byte(field.nameText)), NameSpan: field.nameSpan, Member: field.member, Value: value.ID, Destination: field.destination.ID, Ordinal: field.ordinal})
		runtime = append(runtime, value.ID)
		w.retainCompatibility(field.field, ctx.genericOwner, value.ID, field.destination.ID, compatibilityRecordField, field.ordinal, field.member, field.nameSpan, false)
	}
	if !childrenSuccessful {
		result := w.failExpression(ref, origin)
		header := w.header(ref, ctx.genericOwner, true)
		aggregate := aggregateRecord{Header: header, Kind: kind, Result: result.ID, Receiver: rp.receiver.ID, Declaration: rp.declaration, Fields: fields, DeclarationFields: rp.declarationFields}
		plan.specialized, _ = w.addRecord(retainedRecord{Header: header, Aggregate: &aggregate})
		return
	}
	result := w.expressionResult(ref, receiver, origin)
	w.addConstraint(infer.Equal(result.Term, receiver, origin))
	header := w.header(ref, ctx.genericOwner, !w.publishedSyntax[ref])
	aggregate := aggregateRecord{Header: header, Kind: kind, Result: result.ID, Receiver: rp.receiver.ID, Declaration: rp.declaration, Fields: fields, DeclarationFields: rp.declarationFields}
	specialized, _ := w.addRecord(retainedRecord{Header: header, Aggregate: &aggregate})
	plan.specialized = specialized
	if specialized == 0 || !w.publishedSyntax[ref] {
		return
	}
	w.successfulExpressions[ref] = true
	record := expressionRecord{Header: header, Kind: expressionRecordValue, Result: result.ID, Children: runtime, Specialized: specialized}
	w.addRecord(retainedRecord{Header: header, Expression: &record})
}

func (w *walker) finishPartialMember(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, plan *expressionPlan, origin infer.Origin) typedValue {
	result := w.expressionResult(ref, w.session.Variable(origin), origin)
	children := node.Children()
	if len(children) == 0 {
		return result
	}
	// When the target nominal type is already known during the authored
	// traversal (.Empty where a let/var annotation or return type fixes the
	// receiver), ground the receiver immediately. Otherwise the receiver is a
	// solver variable that settles through unification (e.g. `entry.state ==
	// .Empty`, where the sibling operand's structural field type binds the
	// receiver at solve time); the member symbol and nominal declaration are
	// re-derived by name from the solved receiver type before typed-IR
	// construction and validated there.
	receiverOrigin := w.origin(ref, node, "partial member receiver", ctx.typeOwner, ctx.genericOwner)
	var declaration symbol.SymbolID
	var declarationFields []symbol.SymbolID
	var receiver typedValue
	if id, known := w.knownDestination(ctx.expected); known {
		declaration, declarationFields = w.nominalDeclaration(id)
		receiver, _ = w.newSlotValue(w.session.Known(id), receiverOrigin)
		receiver.Known = id
		w.knownValues[receiver.ID] = id
	} else {
		receiver, _ = w.newSlotValue(w.session.Variable(receiverOrigin), receiverOrigin)
	}
	nameNode, _ := tree.Node(children[0])
	name := string(w.copySource(nameNode.Span()))
	nameRef := symbol.SyntaxRef{Module: ref.Module, Node: children[0]}
	member := symbol.SymbolID(0)
	if resolved, ok := w.generation.inputs.Resolution.Reference(nameRef); ok && resolved.State == symbol.ResolutionResolved {
		member = resolved.Symbol
	}
	memberOrigin := w.originForRef(nameRef, "partial member", ctx.typeOwner, ctx.genericOwner)
	memberDestination, _ := w.newSlotValue(w.session.Variable(memberOrigin), memberOrigin)
	if name != "" {
		w.addConstraint(infer.HasField(receiver.Term, name, memberDestination.Term, memberOrigin))
	}
	if receiver.Known != 0 {
		w.addConstraint(infer.Equal(result.Term, w.session.Known(receiver.Known), origin))
	} else {
		w.addConstraint(infer.Equal(result.Term, receiver.Term, origin))
		if destination := ctx.expected.Destination; destination != 0 && w.generation.hasValue(destination) {
			w.addConstraint(infer.Equal(result.Term, w.generation.values[destination-1].Term, origin))
		}
	}
	header := w.header(ref, ctx.genericOwner, declaration == 0 || name == "")
	field := fieldValue{Field: ref, NameSyntax: nameRef, Name: string([]byte(name)), NameSpan: nameNode.Span(), Member: member, Value: result.ID, Destination: memberDestination.ID}
	aggregate := aggregateRecord{Header: header, Kind: aggregateEnumVariant, Result: result.ID, Receiver: receiver.ID, Declaration: declaration, Fields: []fieldValue{field}, DeclarationFields: declarationFields}
	plan.specialized, _ = w.addRecord(retainedRecord{Header: header, Aggregate: &aggregate})
	plan.symbol = member
	return result
}

func (w *walker) nominalDeclaration(id types.TypeID) (symbol.SymbolID, []symbol.SymbolID) {
	key, ok := w.generation.inputs.Types.Key(id)
	if !ok {
		return 0, nil
	}
	declaration, _, ok := key.Nominal()
	if !ok {
		return 0, nil
	}
	members := w.generation.inputs.Resolution.Members(declaration)
	fields := make([]symbol.SymbolID, 0, len(members))
	for _, member := range members {
		value, found := w.generation.inputs.Resolution.Symbols.Symbol(member)
		if found && (value.Kind == symbol.SymbolField || value.Kind == symbol.SymbolVariant) {
			fields = append(fields, member)
		}
	}
	return declaration, fields
}
