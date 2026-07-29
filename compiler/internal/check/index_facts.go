package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type slicePlan struct {
	base, start, end symbol.SyntaxRef
	result           typedValue
}

func (w *walker) prepareSlice(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
	semantic := semanticRefs(ref.Module, node, tree)
	if ctx.suppressValue || len(semantic) == 0 {
		for i := range items {
			items[i].ctx.suppressValue = true
		}
		return items
	}
	p := &slicePlan{base: semantic[0]}
	index := 1
	if node.Data()&syntax.SliceStartPresent != 0 && index < len(semantic) {
		p.start = semantic[index]
		index++
	}
	if node.Data()&syntax.SliceEndPresent != 0 && index < len(semantic) {
		p.end = semantic[index]
	}
	p.result = w.reserveExpression(ref, w.origin(ref, node, "slice result", ctx.typeOwner, ctx.genericOwner))
	for i := range items {
		items[i].ctx.expected = expectedType{}
	}
	w.slicePlans[ref] = p
	return items
}

func (w *walker) finishSlice(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	p := w.slicePlans[ref]
	if p == nil || ctx.suppressValue {
		return
	}
	origin := w.origin(ref, node, "slice", ctx.typeOwner, ctx.genericOwner)
	base := w.valuesBySyntax[p.base]
	if base.ID == 0 || !w.successfulExpressions[p.base] {
		w.failExpression(ref, origin)
		return
	}
	start, end := typedValue{}, typedValue{}
	if p.start != (symbol.SyntaxRef{}) {
		start = w.valuesBySyntax[p.start]
		if start.ID == 0 || !w.successfulExpressions[p.start] {
			w.failExpression(ref, origin)
			return
		}
		w.addConstraint(infer.Integral(start.Term, w.originForRef(p.start, "slice start", ctx.typeOwner, ctx.genericOwner)))
	}
	if p.end != (symbol.SyntaxRef{}) {
		end = w.valuesBySyntax[p.end]
		if end.ID == 0 || !w.successfulExpressions[p.end] {
			w.failExpression(ref, origin)
			return
		}
		w.addConstraint(infer.Integral(end.Term, w.originForRef(p.end, "slice end", ctx.typeOwner, ctx.genericOwner)))
	}
	w.addConstraint(infer.Sliceable(base.Term, p.result.Term, origin))
	if w.session.Fatal() {
		return
	}
	result, ok := w.publishExistingSyntax(ref, p.result, origin)
	if w.session.Fatal() {
		return
	}
	if !ok {
		return
	}
	header := w.header(ref, ctx.genericOwner, false)
	if p.start != (symbol.SyntaxRef{}) {
		w.evaluateIndexBound(p.start)
	}
	if p.end != (symbol.SyntaxRef{}) {
		w.evaluateIndexBound(p.end)
	}
	record := indexRecord{Header: header, Mode: indexSlice, Base: base.ID, Start: start.ID, End: end.ID, Result: result.ID, StartPresent: start.ID != 0, EndPresent: end.ID != 0, StartSyntax: p.start, EndSyntax: p.end}
	record.EscapeDestination = w.escapeDestinations[ref]
	w.applyKnownArrayLength(&record, p.base)
	specialized, ok := w.addRecord(retainedRecord{Header: header, Index: &record})
	if !ok {
		return
	}
	w.retainRequirement(header, requirementUnsupportedSlice, base.ID)
	w.applyExpected(result, infer.Term{}, ctx.expected, origin)
	children := []valueID{base.ID}
	if start.ID != 0 {
		children = append(children, start.ID)
	}
	if end.ID != 0 {
		children = append(children, end.ID)
	}
	if _, ok := w.addRecord(retainedRecord{Header: header, Expression: &expressionRecord{Header: header, Kind: expressionSlice, Result: result.ID, Children: children, Specialized: specialized}}); ok {
		w.successfulExpressions[ref] = true
	}
}

func (w *walker) applyKnownArrayLength(record *indexRecord, base symbol.SyntaxRef) {
	if value := w.valuesBySyntax[base]; value.ID != 0 {
		if known := w.knownValues[value.ID]; known != 0 {
			if key, exists := w.generation.inputs.Types.Key(known); exists {
				if length, _, array := key.Array(); array {
					record.KnownArrayLength, record.HasKnownArrayLength = length, true
					return
				}
			}
		}
	}
	if p := w.expressionPlans[base]; p != nil && p.arrayKnown {
		record.KnownArrayLength, record.HasKnownArrayLength = p.arrayLength, true
		return
	}
	node, ok := w.node(base.Module, base.Node)
	if !ok {
		return
	}
	query := base
	if node.Kind() == syntax.Path {
		children := node.Children()
		if len(children) != 0 {
			query.Node = children[len(children)-1]
		}
	}
	resolved, ok := w.generation.inputs.Resolution.Reference(query)
	if !ok || resolved.State != symbol.ResolutionResolved {
		return
	}
	sym, ok := w.generation.inputs.Resolution.Symbols.Symbol(resolved.Symbol)
	if !ok || sym.Kind != symbol.SymbolParameter {
		return
	}
	sig, ok := w.program.Signature(sym.Containing)
	if !ok {
		return
	}
	for i, id := range sig.Parameters {
		if id == sym.ID && i < len(sig.Inputs) {
			if template, found := w.program.Template(sig.Inputs[i]); found && template.Kind == infer.TemplateArray {
				record.KnownArrayLength, record.HasKnownArrayLength = template.Length, true
			} else if found && template.Kind == infer.TemplateKnown {
				if key, exists := w.generation.inputs.Types.Key(template.Known); exists {
					if length, _, array := key.Array(); array {
						record.KnownArrayLength, record.HasKnownArrayLength = length, true
					}
				}
			}
			return
		}
	}
}

func (w *walker) deriveIndexPlace(ref, baseRef symbol.SyntaxRef, base, index, result typedValue) {
	if known, ok := w.knownValues[base.ID]; ok {
		if key, found := w.generation.inputs.Types.Key(known); found && key.Kind() == types.Builtin {
			builtin, _ := key.Builtin()
			if builtin == types.Str {
				return
			}
		}
	}
	w.deriveProjectionPlace(ref, baseRef, result.ID, placeProjection{Kind: placeIndex, Base: base.ID, Index: index.ID})
}
