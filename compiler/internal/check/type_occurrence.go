package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type typeUseKind uint8

const (
	typeUseAnnotation typeUseKind = iota + 1
	typeUseCastTarget
	typeUseSizeof
	typeUseExplicitArgument
)

type typeUseRecord struct {
	Header recordHeader
	Kind   typeUseKind
	Type   valueID
}

type typedValue struct {
	ID   valueID
	Term infer.Term
}

func (w *walker) resolveType(ref symbol.SyntaxRef, typeOwner, genericOwner symbol.SymbolID, role string) typedValue {
	if w.resolvedTypes[ref] {
		w.generation.report("type occurrence resolved more than once", spanForRef(w.generation.inputs, ref))
		return w.errorValue(ref, typeOwner, genericOwner, role)
	}
	w.resolvedTypes[ref] = true
	origin := w.originForRef(ref, role, typeOwner, genericOwner)
	result := w.session.ResolveType(ref, typeOwner)
	term := w.session.Error(origin)
	suppressed := result.State != infer.TypeFinal
	if !suppressed {
		term = w.session.Known(result.Type)
	}
	value, published := w.newSlotValue(term, origin)
	suppressed = suppressed || !published
	w.retainTypeUse(ref, genericOwner, value.ID, suppressed)
	return value
}

func (w *walker) preparedType(ref symbol.SyntaxRef, term infer.Term, typeOwner, genericOwner symbol.SymbolID, role string, suppressed bool) typedValue {
	w.preparedTypes[ref] = true
	origin := w.originForRef(ref, role, typeOwner, genericOwner)
	value, published := w.newSlotValue(term, origin)
	suppressed = suppressed || !published
	w.retainTypeUse(ref, genericOwner, value.ID, suppressed)
	return value
}

func (w *walker) retainTypeUse(ref symbol.SyntaxRef, genericOwner symbol.SymbolID, value valueID, suppressed bool) {
	header := w.header(ref, genericOwner, suppressed)
	record := typeUseRecord{Header: header, Kind: typeUseAnnotation, Type: value}
	w.generation.addRecord(retainedRecord{Header: header, TypeUse: &record})
}

func (w *walker) errorValue(ref symbol.SyntaxRef, typeOwner, genericOwner symbol.SymbolID, role string) typedValue {
	origin := w.originForRef(ref, role, typeOwner, genericOwner)
	return w.newValue(w.session.Error(origin), origin)
}

func typeNodeForParameter(tree *syntax.Tree, parameter syntax.NodeID) syntax.NodeID {
	node, ok := tree.Node(parameter)
	if !ok {
		return 0
	}
	children := node.Children()
	for index := len(children) - 1; index >= 0; index-- {
		child, childOK := tree.Node(children[index])
		if childOK && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
			return children[index]
		}
	}
	return 0
}
