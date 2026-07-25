package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type placeKind uint8

const (
	placeStorage placeKind = iota + 1
	placeDereference
	placeField
	placeTuple
	placeIndex
)

type placeProjection struct {
	Kind         placeKind
	Base         valueID
	Member       symbol.SymbolID
	TupleOrdinal uint32
	Index        valueID
}

func validPlaceProjection(p placeProjection) bool {
	switch p.Kind {
	case placeStorage:
		return p.Base == 0 && p.Member == 0 && p.TupleOrdinal == 0 && p.Index == 0
	case placeDereference:
		return p.Base != 0 && p.Member == 0 && p.TupleOrdinal == 0 && p.Index == 0
	case placeField:
		return p.Base != 0 && p.TupleOrdinal == 0 && p.Index == 0
	case placeTuple:
		return p.Base != 0 && p.Member == 0 && p.Index == 0
	case placeIndex:
		return p.Base != 0 && p.Member == 0 && p.TupleOrdinal == 0 && p.Index != 0
	}
	return false
}

type placeRecord struct {
	Header      recordHeader
	Root        symbol.SymbolID
	RootKind    symbol.SymbolKind
	RootMutable bool
	Projections []placeProjection
}

type placeCandidate struct {
	value       valueID
	root        symbol.SymbolID
	rootKind    symbol.SymbolKind
	mutable     bool
	projections []placeProjection
	alternative alternativeTag
}

func (w *walker) storagePlace(ref symbol.SyntaxRef, value valueID, id symbol.SymbolID) {
	s, ok := w.generation.inputs.Resolution.Symbols.Symbol(id)
	if !ok {
		return
	}
	mutable := false
	if node, found := w.node(s.Declaration.Module, s.Declaration.Node); found {
		mutable = mutable || (node.Kind() == syntax.BindingDecl || node.Kind() == syntax.ExternBinding) && node.Token() == syntax.KwVar
	}
	w.places[ref] = placeCandidate{value: value, root: id, rootKind: s.Kind, mutable: mutable, projections: []placeProjection{{Kind: placeStorage}}}
}

func (w *walker) copyPlace(to, from symbol.SyntaxRef, value valueID) {
	p, ok := w.places[from]
	if !ok {
		return
	}
	p.value = value
	p.projections = append([]placeProjection(nil), p.projections...)
	w.places[to] = p
}

func (w *walker) deriveDereferencePlace(ref symbol.SyntaxRef, base, result typedValue, ctx walkContext) {
	if !w.generation.trackPlace() {
		return
	}
	p := placeCandidate{value: result.ID}
	if len(w.operatorPlans[ref].children) != 0 {
		if inherited, ok := w.places[w.operatorPlans[ref].children[0]]; ok {
			p = inherited
			p.value = result.ID
			p.projections = append([]placeProjection(nil), inherited.projections...)
		}
	}
	p.projections = append(p.projections, placeProjection{Kind: placeDereference, Base: base.ID})
	w.places[ref] = p
}

func (w *walker) deriveProjectionPlace(ref, baseRef symbol.SyntaxRef, result valueID, projection placeProjection) {
	p, ok := w.places[baseRef]
	if !ok {
		return
	}
	if !w.generation.trackPlace() {
		return
	}
	p.value = result
	p.projections = append(append([]placeProjection(nil), p.projections...), projection)
	w.places[ref] = p
}

func (w *walker) retainPlaceUse(candidateRef, syntaxRef symbol.SyntaxRef, ctx walkContext) (valueID, bool) {
	p, ok := w.places[candidateRef]
	if !ok || p.value == 0 {
		return 0, false
	}
	header := w.header(syntaxRef, ctx.genericOwner, false)
	header.Alternative = p.alternative
	record := placeRecord{Header: header, Root: p.root, RootKind: p.rootKind, RootMutable: p.mutable, Projections: append([]placeProjection(nil), p.projections...)}
	_, ok = w.addRecord(retainedRecord{Header: header, Place: &record})
	return p.value, ok
}
