package infer

import "github.com/pepplejoshua/pebble/compiler/internal/types"

type structuralState uint8

const (
	structuralUnresolved structuralState = iota
	structuralError
	structuralRigid
	structuralKnown
	structuralShape
)

func (s *Session) structure(term Term) (structuralState, types.TypeID, Shape) {
	if term.kind == termError {
		return structuralError, 0, Shape{}
	}
	if term.kind == termKnown {
		if _, rigid := s.typeParameter(term.known); rigid {
			return structuralRigid, term.known, Shape{}
		}
		return structuralKnown, term.known, Shape{}
	}
	root := s.find(term.id)
	if root == 0 || s.cells[root-1].error {
		return structuralError, 0, Shape{}
	}
	cell := &s.cells[root-1]
	if cell.known != 0 {
		if _, rigid := s.typeParameter(cell.known); rigid {
			return structuralRigid, cell.known, Shape{}
		}
		return structuralKnown, cell.known, Shape{}
	}
	if cell.shape != nil {
		return structuralShape, 0, *cell.shape
	}
	return structuralUnresolved, 0, Shape{}
}

func (s *Session) termIsError(term Term) bool {
	state, _, _ := s.structure(term)
	return state == structuralError
}

func (s *Session) recoverTerms(terms ...Term) bool {
	if s.Fatal() {
		return false
	}
	changed := false
	for _, term := range terms {
		if !term.belongs(s.token) || term.kind == termKnown || term.kind == termError {
			continue
		}
		root := s.find(term.id)
		if root != 0 && !s.cells[root-1].error {
			s.cells[root-1].error = true
			changed = true
		}
	}
	return changed
}

func callableRecovery(arguments []CallableArgument, result Term) []Term {
	terms := make([]Term, 0, len(arguments)+1)
	for _, argument := range arguments {
		terms = append(terms, argument.Destination)
	}
	return append(terms, result)
}

func (s *Session) callable(callee Term, arguments []CallableArgument, result Term, origin Origin) (bool, bool, bool) {
	state, known, shape := s.structure(callee)
	switch state {
	case structuralUnresolved:
		return false, true, true
	case structuralError:
		return s.recoverTerms(callableRecovery(arguments, result)...), true, false
	case structuralRigid:
		return s.recoverTerms(callableRecovery(arguments, result)...), true, false
	case structuralKnown:
		key, ok := s.program.typeKey(known)
		if !ok {
			return s.failStructural("callable subject is not a store-owned type", origin, callableRecovery(arguments, result))
		}
		_, parameters, functionResult, variadic, ok := key.Function()
		if !ok {
			return s.failStructural("type is not callable", origin, callableRecovery(arguments, result))
		}
		return s.applyCallableKnown(parameters, functionResult, variadic, arguments, result, origin)
	case structuralShape:
		if shape.kind == shapeLeaf {
			return false, true, true
		}
		if shape.kind != shapeFunction {
			return s.failStructural("type shape is not callable", origin, callableRecovery(arguments, result))
		}
		return s.applyCallableShape(shape, arguments, result, origin)
	default:
		return false, true, true
	}
}

func (s *Session) applyCallableKnown(parameters []types.TypeID, functionResult types.TypeID, variadic bool, arguments []CallableArgument, result Term, origin Origin) (bool, bool, bool) {
	if !s.stepDecompose(origin) {
		return false, false, false
	}
	if (!variadic && len(arguments) != len(parameters)) || (variadic && len(arguments) < len(parameters)) {
		return s.failStructural("call arity is incompatible with callable type", origin, callableRecovery(arguments, result))
	}
	changed := false
	for index, parameter := range parameters {
		c, ok := s.unify(arguments[index].Destination, s.Known(parameter), childOrigin(origin, index))
		changed = changed || c
		if !ok {
			s.recoverTerms(callableRecovery(arguments, result)...)
			return changed, false, false
		}
	}
	for index := len(parameters); index < len(arguments); index++ {
		if s.termIsError(arguments[index].Source) {
			changed = s.recoverTerms(arguments[index].Destination) || changed
			continue
		}
		c, ok := s.unify(arguments[index].Destination, arguments[index].Source, childOrigin(origin, index))
		changed = changed || c
		if !ok {
			s.recoverTerms(callableRecovery(arguments, result)...)
			return changed, false, false
		}
	}
	c, ok := s.unify(result, s.Known(functionResult), childOrigin(origin, len(arguments)))
	return changed || c, ok, false
}

func (s *Session) applyCallableShape(shape Shape, arguments []CallableArgument, result Term, origin Origin) (bool, bool, bool) {
	if !s.stepDecompose(origin) {
		return false, false, false
	}
	parameters := shape.children[:len(shape.children)-1]
	if (!shape.variadic && len(arguments) != len(parameters)) || (shape.variadic && len(arguments) < len(parameters)) {
		return s.failStructural("call arity is incompatible with callable shape", origin, callableRecovery(arguments, result))
	}
	changed := false
	for index, parameter := range parameters {
		c, ok := s.unifyShapeWithTerm(parameter, arguments[index].Destination, childOrigin(origin, index))
		changed = changed || c
		if !ok {
			s.recoverTerms(callableRecovery(arguments, result)...)
			return changed, false, false
		}
	}
	for index := len(parameters); index < len(arguments); index++ {
		if s.termIsError(arguments[index].Source) {
			changed = s.recoverTerms(arguments[index].Destination) || changed
			continue
		}
		c, ok := s.unify(arguments[index].Destination, arguments[index].Source, childOrigin(origin, index))
		changed = changed || c
		if !ok {
			s.recoverTerms(callableRecovery(arguments, result)...)
			return changed, false, false
		}
	}
	c, ok := s.unifyShapeWithTerm(shape.children[len(shape.children)-1], result, childOrigin(origin, len(arguments)))
	return changed || c, ok, false
}

func (s *Session) indexable(receiver, result Term, origin Origin) (bool, bool, bool) {
	state, known, shape := s.structure(receiver)
	switch state {
	case structuralUnresolved:
		return false, true, true
	case structuralError:
		return s.recoverTerms(result), true, false
	case structuralRigid:
		return s.recoverTerms(result), true, false
	case structuralKnown:
		if !s.stepDecompose(origin) {
			return false, false, false
		}
		key, ok := s.program.typeKey(known)
		if !ok {
			return s.failStructural("index receiver is not a store-owned type", origin, []Term{result})
		}
		if _, element, ok := key.Array(); ok {
			changed, success := s.unify(result, s.Known(element), origin)
			return changed, success, false
		}
		if key.Kind() == types.Slice {
			element, _ := key.Child()
			changed, success := s.unify(result, s.Known(element), origin)
			return changed, success, false
		}
		builtins := s.program.builtins()
		if known == builtins.Str {
			changed, success := s.unify(result, s.Known(builtins.Char), origin)
			return changed, success, false
		}
		return s.failStructural("type is not structurally indexable", origin, []Term{result})
	case structuralShape:
		if shape.kind == shapeLeaf {
			return false, true, true
		}
		if shape.kind != shapeArray && shape.kind != shapeSlice {
			return s.failStructural("type shape is not structurally indexable", origin, []Term{result})
		}
		if !s.stepDecompose(origin) {
			return false, false, false
		}
		changed, success := s.unifyShapeWithTerm(shape.children[0], result, origin)
		return changed, success, false
	default:
		return false, true, true
	}
}

func (s *Session) sliceable(receiver, result Term, origin Origin) (bool, bool, bool) {
	state, known, shape := s.structure(receiver)
	switch state {
	case structuralUnresolved:
		return false, true, true
	case structuralError:
		return s.recoverTerms(result), true, false
	case structuralRigid:
		return s.recoverTerms(result), true, false
	case structuralKnown:
		if !s.stepDecompose(origin) {
			return false, false, false
		}
		key, ok := s.program.typeKey(known)
		if !ok {
			return s.failStructural("slice receiver is not a store-owned type", origin, []Term{result})
		}
		var element types.TypeID
		if _, candidate, array := key.Array(); array {
			element = candidate
		} else if key.Kind() == types.Slice {
			element, _ = key.Child()
		} else if known == s.program.builtins().Str {
			changed, success := s.unify(result, s.Known(known), origin)
			return changed, success, false
		} else {
			return s.failStructural("type is not structurally sliceable", origin, []Term{result})
		}
		changed, success := s.constrainShape(result, SliceShape(Leaf(s.Known(element))), origin)
		return changed, success, false
	case structuralShape:
		if shape.kind == shapeLeaf {
			return false, true, true
		}
		if shape.kind != shapeArray && shape.kind != shapeSlice {
			return s.failStructural("type shape is not structurally sliceable", origin, []Term{result})
		}
		if !s.stepDecompose(origin) {
			return false, false, false
		}
		changed, success := s.constrainShape(result, SliceShape(shape.children[0]), origin)
		return changed, success, false
	default:
		return false, true, true
	}
}

func (s *Session) hasComponent(receiver Term, ordinal uint32, result Term, origin Origin) (bool, bool, bool) {
	state, known, shape := s.structure(receiver)
	switch state {
	case structuralUnresolved:
		return false, true, true
	case structuralError:
		return s.recoverTerms(result), true, false
	case structuralRigid:
		return s.recoverTerms(result), true, false
	case structuralKnown:
		if !s.stepDecompose(origin) {
			return false, false, false
		}
		key, ok := s.program.typeKey(known)
		if !ok {
			return s.failStructural("component receiver is not a store-owned type", origin, []Term{result})
		}
		elements, ok := key.Elements()
		if !ok {
			return s.failStructural("type is not structurally a tuple", origin, []Term{result})
		}
		if uint64(ordinal) >= uint64(len(elements)) {
			return s.failStructural("tuple ordinal is out of range", origin, []Term{result})
		}
		changed, success := s.unify(result, s.Known(elements[ordinal]), origin)
		return changed, success, false
	case structuralShape:
		if shape.kind == shapeLeaf {
			return false, true, true
		}
		if shape.kind != shapeTuple {
			return s.failStructural("type shape is not structurally a tuple", origin, []Term{result})
		}
		if !s.stepDecompose(origin) {
			return false, false, false
		}
		if uint64(ordinal) >= uint64(len(shape.children)) {
			return s.failStructural("tuple ordinal is out of range", origin, []Term{result})
		}
		changed, success := s.unifyShapeWithTerm(shape.children[ordinal], result, origin)
		return changed, success, false
	default:
		return false, true, true
	}
}

func (s *Session) failStructural(message string, origin Origin, recovery []Term) (bool, bool, bool) {
	changed := s.recoverTerms(recovery...)
	s.conflict(CodeCapability, message, origin)
	return changed, false, false
}
