package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildPlace folds a retained place record's complete projection chain. Place
// nodes are internal structure; the authored source maps to the value Load or
// to the place's eventual statement owner in later statement construction.
func (s *irBuildState) buildPlace(ref symbol.SyntaxRef) (tir.NodeID, bool) {
	record, ok := s.places[ref]
	if !ok || record == nil || len(record.Projections) == 0 {
		return 0, false
	}
	rootType, ok := s.handoff.Solution.SymbolType(record.Root)
	if !ok || rootType.State != infer.TypeFinal || rootType.Type == 0 {
		return 0, false
	}
	finalType := rootType.Type
	if assignment := s.assignmentPlace(record.Header.Syntax); assignment != 0 {
		if typ, found := s.resolveType(assignment); found {
			finalType = typ
		}
	}
	var current tir.NodeID
	for i, projection := range record.Projections {
		typ := rootType.Type
		if i > 0 {
			if i+1 < len(record.Projections) {
				typ, ok = s.resolveType(record.Projections[i+1].Base)
			} else {
				typ = finalType
				ok = typ != 0
			}
			if !ok {
				return 0, false
			}
		}
		n := tir.Node{Type: typ, Writable: record.RootMutable, Span: record.Header.Span}
		switch projection.Kind {
		case placeStorage:
			n.Kind, n.Symbol = tir.StoragePlace, record.Root
		case placeDereference:
			child, childOK := s.buildValue(projection.Base)
			if !childOK {
				return 0, false
			}
			n.Kind, n.Children = tir.DereferencePlace, []tir.NodeID{child}
		case placeField:
			memberID := projection.Member
			structuralName := ""
			if memberID == 0 {
				var result valueID
				if i+1 < len(record.Projections) {
					result = record.Projections[i+1].Base
				} else {
					result = s.assignmentPlace(record.Header.Syntax)
				}
				member := s.membersByResult[result]
				if member == nil || member.Base != projection.Base {
					return 0, false
				}
				memberID = member.Member
				structuralName = member.Name
				if memberID == 0 {
					memberID = s.memberSymbol(projection.Base, member.Name)
				}
			}
			if memberID == 0 && structuralName == "" {
				return 0, false
			}
			n.Kind, n.Member, n.Children = tir.FieldPlace, memberID, []tir.NodeID{current}
			if memberID == 0 {
				if structuralName == "len" {
					memberID = tir.StructuralFieldLen
				} else if structuralName == "data" {
					memberID = tir.StructuralFieldData
				}
				n.SyntheticRole = "structural-field"
				n.Origin = record.Header.Span
				n.Member = memberID
			}
		case placeTuple:
			n.Kind, n.Ordinal, n.Children = tir.TuplePlace, projection.TupleOrdinal, []tir.NodeID{current}
		case placeIndex:
			child, childOK := s.buildValue(projection.Index)
			if !childOK {
				return 0, false
			}
			n.Kind, n.Children = tir.CheckedIndexPlace, []tir.NodeID{current, child}
		default:
			return 0, false
		}
		var added bool
		current, added = s.addNode(n, symbol.SyntaxRef{})
		if !added {
			return 0, false
		}
	}
	return current, true
}

func (s *irBuildState) assignmentPlace(ref symbol.SyntaxRef) valueID {
	for _, retained := range s.handoff.Records.Records() {
		if retained.Assignment != nil && retained.Assignment.Statement == ref {
			return retained.Assignment.Place
		}
	}
	return 0
}

// buildPlaceForValue derives the same chain used by 06a for an expression
// result. It is needed because ordinary reads do not retain a place record.
func (s *irBuildState) buildPlaceForValue(id valueID) (tir.NodeID, bool) {
	if existing, ok := s.placeValues[id]; ok {
		return existing, true
	}
	record, ok := s.expressionsByResult[id]
	if !ok {
		return 0, false
	}
	typ, ok := s.resolveType(id)
	if !ok {
		return 0, false
	}
	var root symbol.SymbolID
	var rootType types.TypeID
	var current tir.NodeID
	if record.Kind == expressionName || record.Kind == expressionPath {
		sym, found := s.symbol(record.Symbol)
		if !found {
			return 0, false
		}
		switch sym.Kind {
		case symbol.SymbolBinding, symbol.SymbolParameter, symbol.SymbolLoopBinding, symbol.SymbolExternBinding:
			root = sym.ID
			rootType = typ
		default:
			return 0, false
		}
		n, made := s.addNode(tir.Node{Kind: tir.StoragePlace, Type: rootType, Span: record.Header.Span, Symbol: root, Writable: s.symbolMutable(root)}, symbol.SyntaxRef{})
		if !made {
			return 0, false
		}
		current = n
	} else if index := s.indexForValue(id, record.Header.Syntax); index != nil && index.Mode == indexValue {
		if s.isString(index.Base) {
			return 0, false
		}
		base, found := s.buildPlaceForValue(index.Base)
		if !found {
			return 0, false
		}
		child, found := s.buildValue(index.Start)
		if !found {
			return 0, false
		}
		n, made := s.addNode(tir.Node{Kind: tir.CheckedIndexPlace, Type: typ, Span: record.Header.Span, Writable: s.placeWritableValue(index.Base), Children: []tir.NodeID{base, child}}, symbol.SyntaxRef{})
		if !made {
			return 0, false
		}
		current = n
	} else if len(record.Children) == 1 {
		base, found := s.buildPlaceForValue(record.Children[0])
		if !found {
			return 0, false
		}
		current = base
		writable := s.placeWritableValue(record.Children[0])
		if member := s.membersByResult[id]; member != nil {
			n := tir.Node{Type: typ, Span: record.Header.Span, Writable: writable}
			if member.Kind == memberField {
				n.Kind, n.Member = tir.FieldPlace, member.Member
				if n.Member == 0 {
					if member.Name == "len" {
						n.Member = tir.StructuralFieldLen
					} else if member.Name == "data" {
						n.Member = tir.StructuralFieldData
					}
					n.SyntheticRole = "structural-field"
					n.Origin = record.Header.Span
				}
				if n.Member == 0 {
					n.Member = s.memberSymbol(record.Children[0], member.Name)
				}
			} else if member.Kind == memberTuple {
				n.Kind, n.Ordinal = tir.TuplePlace, member.TupleOrdinal
			} else {
				return 0, false
			}
			n.Children = []tir.NodeID{current}
			made, ok := s.addNode(n, symbol.SyntaxRef{})
			if !ok {
				return 0, false
			}
			current = made
		} else if op := s.operatorForValue(id, record.Header.Syntax); op != nil && op.Family == operatorDereference {
			child, found := s.buildValue(op.Operands[0])
			if !found {
				return 0, false
			}
			n, made := s.addNode(tir.Node{Kind: tir.DereferencePlace, Type: typ, Span: record.Header.Span, Writable: writable, Children: []tir.NodeID{child}}, symbol.SyntaxRef{})
			if !made {
				return 0, false
			}
			current = n
		} else {
			return 0, false
		}
	} else {
		return 0, false
	}
	s.placeValues[id] = current
	return current, true
}

func (s *irBuildState) operatorForValue(id valueID, ref symbol.SyntaxRef) *operatorRecord {
	if op := s.operatorsByResult[id]; op != nil {
		return op
	}
	return s.operatorsBySyntax[ref]
}

func (s *irBuildState) indexForValue(id valueID, ref symbol.SyntaxRef) *indexRecord {
	if index := s.indexesByResult[id]; index != nil {
		return index
	}
	return s.indexesBySyntax[ref]
}

func (s *irBuildState) symbolMutable(id symbol.SymbolID) bool {
	for _, retained := range s.handoff.Records.Records() {
		if retained.Binding != nil && retained.Binding.Symbol == id {
			return retained.Binding.Kind == bindingLocalVar || retained.Binding.Kind == bindingGlobalVar || retained.Binding.Kind == bindingExternVar
		}
		if retained.Place != nil && retained.Place.Root == id {
			return retained.Place.RootMutable
		}
	}
	return false
}

func (s *irBuildState) memberSymbol(base valueID, name string) symbol.SymbolID {
	typ, ok := s.resolveType(base)
	if !ok {
		return 0
	}
	key, ok := s.handoff.Semantics.Types().Key(typ)
	if !ok {
		return 0
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return 0
	}
	d, ok := s.handoff.Semantics.TypeDeclaration(decl)
	if !ok {
		return 0
	}
	for _, member := range d.Members {
		sym, found := s.symbol(member.Symbol)
		if found && sym.Name == name {
			return member.Symbol
		}
	}
	return 0
}

func (s *irBuildState) placeWritableValue(id valueID) bool {
	record, ok := s.expressionsByResult[id]
	if !ok {
		return false
	}
	if record.Kind == expressionName || record.Kind == expressionPath {
		return s.symbolMutable(record.Symbol)
	}
	if index := s.indexForValue(id, record.Header.Syntax); index != nil {
		return s.placeWritableValue(index.Base)
	}
	if len(record.Children) == 1 {
		return s.placeWritableValue(record.Children[0])
	}
	return false
}

func (s *irBuildState) isString(id valueID) bool {
	typ, ok := s.resolveType(id)
	if !ok {
		return false
	}
	key, ok := s.handoff.Semantics.Types().Key(typ)
	if !ok || key.Kind() != types.Builtin {
		return false
	}
	builtin, _ := key.Builtin()
	return builtin == types.Str
}
