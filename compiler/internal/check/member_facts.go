package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"strconv"
)

type memberKind uint8

const (
	memberStatic memberKind = iota + 1
	memberField
	memberTuple
	memberMethod
	memberVariant
)

type memberRecord struct {
	Header       recordHeader
	Kind         memberKind
	Base, Result valueID
	Member       symbol.SymbolID
	Name         string
	NameSpan     source.Span
	TupleOrdinal uint32
}
type memberPlan struct {
	base, name symbol.SyntaxRef
	nameText   string
	nameSpan   source.Span
	kind       memberKind
	member     symbol.SymbolID
	ordinal    uint32
	callSite   symbol.SyntaxRef
}

func (w *walker) prepareMember(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
	for i := range items {
		items[i].ctx.expected = expectedType{}
	}
	children := node.Children()
	if len(children) < 2 || ctx.suppressValue {
		for i := range items {
			items[i].ctx.suppressValue = true
		}
		return items
	}
	if ctx.deferredMember {
		for i := range items {
			items[i].ctx.deferredMember = false
			if items[i].ref.Node == children[1] {
				items[i].ctx.suppressValue = true
			}
		}
		return items
	}
	p := &memberPlan{base: symbol.SyntaxRef{Module: ref.Module, Node: children[0]}, name: symbol.SyntaxRef{Module: ref.Module, Node: children[1]}, kind: memberField, callSite: ctx.callSite}
	name, _ := tree.Node(children[1])
	p.nameSpan = name.Span()
	p.nameText = string(w.copySource(name.Span()))
	if name.Kind() == syntax.Literal {
		if n, err := strconv.ParseUint(p.nameText, 10, 32); err == nil {
			p.kind, p.ordinal = memberTuple, uint32(n)
		}
	}
	if resolved, ok := w.generation.inputs.Resolution.Reference(p.name); ok && resolved.State == symbol.ResolutionResolved {
		p.member = resolved.Symbol
		if selected, found := w.generation.inputs.Resolution.Symbols.Symbol(resolved.Symbol); found && selected.Kind == symbol.SymbolVariant {
			p.kind = memberVariant
		} else {
			p.kind = memberStatic
		}
	}
	if ctx.immediateCall && p.kind == memberField {
		p.kind = memberMethod
	}
	for i := range items {
		if items[i].ref == p.name || p.kind == memberStatic || p.kind == memberVariant {
			items[i].ctx.suppressValue = true
		}
	}
	w.memberPlans[ref] = p
	return items
}

func (w *walker) finishMember(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	if ctx.suppressValue || ctx.deferredMember {
		return
	}
	p := w.memberPlans[ref]
	if p == nil {
		return
	}
	origin := w.origin(ref, node, "member result", ctx.typeOwner, ctx.genericOwner)
	base := typedValue{}
	if p.kind != memberStatic && p.kind != memberVariant {
		base = w.valuesBySyntax[p.base]
		if base.ID == 0 || !w.successfulExpressions[p.base] {
			w.failExpression(ref, origin)
			return
		}
	}
	// A static or variant member is exactly its resolved symbol term. Allocating
	// a session variable first and then discarding it on those paths would leave
	// an unconstrained inference variable behind and report a spurious T0510.
	term := infer.Term{}
	if p.kind == memberStatic || p.kind == memberVariant {
		term = w.symbolTerm(p.member, origin)
	} else {
		term = w.session.Variable(origin)
	}
	switch p.kind {
	case memberField:
		w.addConstraint(infer.HasField(base.Term, p.nameText, term, origin))
	case memberTuple:
		w.addConstraint(infer.HasComponent(base.Term, p.ordinal, term, origin))
	case memberMethod:
		site := p.callSite
		if site == (symbol.SyntaxRef{}) {
			site = ref
		}
		w.addConstraint(infer.SelectMethod(base.Term, p.nameText, term, nil, site, origin))
	}
	result := w.expressionResult(ref, term, origin)
	if result.ID == 0 || !w.publishedSyntax[ref] {
		return
	}
	header := w.header(ref, ctx.genericOwner, false)
	record := memberRecord{Header: header, Kind: p.kind, Base: base.ID, Result: result.ID, Member: p.member, Name: p.nameText, NameSpan: p.nameSpan, TupleOrdinal: p.ordinal}
	specialized, ok := w.addRecord(retainedRecord{Header: header, Member: &record})
	if !ok {
		return
	}
	if p.kind == memberField {
		w.retainRequirement(header, requirementUnsupportedField, base.ID)
	}
	if p.kind == memberMethod {
		w.retainRequirement(header, requirementUnsupportedMethod, base.ID)
	}
	if p.kind == memberField {
		w.deriveProjectionPlace(ref, p.base, result.ID, placeProjection{Kind: placeField, Base: base.ID, Member: p.member})
	}
	if p.kind == memberTuple {
		w.deriveProjectionPlace(ref, p.base, result.ID, placeProjection{Kind: placeTuple, Base: base.ID, TupleOrdinal: p.ordinal})
	}
	w.successfulExpressions[ref] = true
	w.applyExpected(result, infer.Term{}, ctx.expected, origin)
	expression := expressionRecord{Header: header, Kind: expressionMember, Result: result.ID, Symbol: p.member, Specialized: specialized}
	if base.ID != 0 {
		expression.Children = []valueID{base.ID}
	}
	w.addRecord(retainedRecord{Header: header, Expression: &expression})
}
