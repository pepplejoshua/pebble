package infer

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func (s *Session) find(id InferID) InferID {
	if id == 0 || uint64(id) > uint64(len(s.cells)) {
		return 0
	}
	for s.cells[id-1].parent != id {
		id = s.cells[id-1].parent
	}
	return id
}

func (s *Session) unify(a, b Term, origin Origin) (bool, bool) {
	if !a.belongs(s.token) || !b.belongs(s.token) {
		return false, s.conflict(CodeResourceLimit, "constraint contains a foreign term", origin)
	}
	if a.kind == termError || b.kind == termError {
		s.taintResult(a)
		s.taintResult(b)
		return false, true
	}
	if !s.stepUnify(origin) {
		return false, false
	}
	if a.kind == termKnown && b.kind == termKnown {
		if a.known == b.known {
			return false, true
		}
		return false, s.conflict(CodeUnification, s.describeTypeConflict(a.known, b.known), origin)
	}
	if a.kind == termKnown {
		return s.bindKnown(b, a.known, origin)
	}
	if b.kind == termKnown {
		return s.bindKnown(a, b.known, origin)
	}
	ra, rb := s.find(a.id), s.find(b.id)
	if ra == 0 || rb == 0 {
		return false, s.conflict(CodeResourceLimit, "invalid inference variable", origin)
	}
	if ra == rb {
		return false, true
	}
	ca, cb := s.cells[ra-1], s.cells[rb-1]
	if ca.error || cb.error {
		return false, true
	}
	if len(ca.literals) > 0 && len(cb.literals) > 0 && ca.literals[0].kind != cb.literals[0].kind {
		return false, s.markRootsConflict(ra, rb, CodeUnification, "integer and floating literal classes cannot unify", origin)
	}
	if ca.known != 0 && cb.known != 0 && ca.known != cb.known {
		return false, s.markRootsConflict(ra, rb, CodeUnification, s.describeTypeConflict(ca.known, cb.known), origin)
	}
	if ca.known != 0 && cb.shape != nil {
		if _, ok := s.matchKnownShape(ca.known, *cb.shape, origin); !ok {
			return false, s.markRootsError(ra, rb)
		}
	}
	if cb.known != 0 && ca.shape != nil {
		if _, ok := s.matchKnownShape(cb.known, *ca.shape, origin); !ok {
			return false, s.markRootsError(ra, rb)
		}
	}
	if ca.shape != nil && cb.shape != nil {
		if _, ok := s.unifyShapes(*ca.shape, *cb.shape, origin); !ok {
			return false, s.markRootsError(ra, rb)
		}
	}
	parent, child := ra, rb
	if ca.rank < cb.rank {
		parent, child = rb, ra
	} else if ca.rank == cb.rank && ra > rb {
		parent, child = rb, ra
	}
	pc, cc := &s.cells[parent-1], &s.cells[child-1]
	cc.parent = parent
	if pc.rank == cc.rank {
		pc.rank++
	}
	if cc.minimum < pc.minimum {
		pc.minimum = cc.minimum
	}
	if pc.known == 0 {
		pc.known = cc.known
	}
	if pc.shape == nil && cc.shape != nil {
		shape := cloneShape(*cc.shape)
		pc.shape = &shape
	}
	pc.literals = append(pc.literals, cloneLiteralValues(cc.literals)...)
	pc.capabilities |= cc.capabilities
	pc.capabilityEvidence = appendCapabilityEvidence(pc.capabilityEvidence, cc.capabilityEvidence...)
	if originBefore(cc.origin, pc.origin) {
		pc.origin = cc.origin
	}
	if pc.known != 0 && !s.validateKnownRoot(parent, origin) {
		if !s.Fatal() {
			pc.error = true
		}
		return true, false
	}
	return true, true
}

func (s *Session) validateKnownRoot(root InferID, origin Origin) bool {
	cell := &s.cells[root-1]
	if parameter, rigid := s.typeParameter(cell.known); rigid {
		for _, evidence := range cell.capabilityEvidence {
			if evidence.origin.GenericOwner == 0 {
				return s.conflict(CodeDamagedInput, "rigid capability constraint has no generic owner", evidence.origin)
			}
			s.recordRequirement(evidence.origin.GenericOwner, evidence.capability, parameter, cell.known, evidence.origin)
		}
		for _, literal := range cell.literals {
			owner := origin.GenericOwner
			if owner == 0 {
				owner = literal.origin.GenericOwner
			}
			if owner == 0 {
				return s.conflict(CodeDamagedInput, "literal fitting against a rigid type has no generic owner", origin)
			}
			s.recordLiteralRequirement(owner, cell.known, literal, origin)
		}
		return true
	}
	return s.fitLiterals(cell.literals, cell.known, origin) && s.checkCapabilities(cell.capabilities, cell.known, origin)
}

func (s *Session) bindKnown(term Term, id types.TypeID, origin Origin) (bool, bool) {
	if term.kind == termError {
		return false, true
	}
	if term.kind == termKnown {
		if term.known == id {
			return false, true
		}
		return false, s.conflict(CodeUnification, s.describeTypeConflict(term.known, id), origin)
	}
	root := s.find(term.id)
	if root == 0 {
		return false, s.conflict(CodeResourceLimit, "invalid inference variable", origin)
	}
	cell := &s.cells[root-1]
	if cell.error {
		return false, true
	}
	if cell.known != 0 {
		if cell.known == id {
			return false, true
		}
		return false, s.markRootConflict(root, CodeUnification, s.describeTypeConflict(cell.known, id), origin)
	}
	if cell.shape != nil {
		if _, ok := s.matchKnownShape(id, *cell.shape, origin); !ok {
			return false, s.markRootError(root)
		}
	}
	if parameter, rigid := s.typeParameter(id); rigid {
		for _, evidence := range cell.capabilityEvidence {
			if evidence.origin.GenericOwner == 0 {
				return false, s.markRootConflict(root, CodeDamagedInput, "rigid capability constraint has no generic owner", evidence.origin)
			}
			s.recordRequirement(evidence.origin.GenericOwner, evidence.capability, parameter, id, evidence.origin)
		}
		for _, literal := range cell.literals {
			owner := origin.GenericOwner
			if owner == 0 {
				owner = literal.origin.GenericOwner
			}
			if owner == 0 {
				return false, s.markRootConflict(root, CodeDamagedInput, "literal fitting against a rigid type has no generic owner", origin)
			}
			s.recordLiteralRequirement(owner, id, literal, origin)
		}
		cell.known = id
		return true, true
	}
	if !s.fitLiterals(cell.literals, id, origin) {
		return false, s.markRootError(root)
	}
	if !s.checkCapabilities(cell.capabilities, id, origin) {
		return false, s.markRootError(root)
	}
	cell.known = id
	return true, true
}

func (s *Session) constrainShape(subject Term, shape Shape, origin Origin) (bool, bool) {
	if subject.kind == termError {
		return false, true
	}
	if subject.kind == termKnown {
		return s.matchKnownShape(subject.known, shape, origin)
	}
	root := s.find(subject.id)
	if root == 0 {
		return false, s.conflict(CodeResourceLimit, "invalid shape subject", origin)
	}
	cell := &s.cells[root-1]
	if cell.error {
		return false, true
	}
	if s.occurs(root, shape) {
		if s.Fatal() {
			return false, false
		}
		return false, s.markRootConflict(root, CodeOccursCheck, "recursive inference shape failed occurs check", origin)
	}
	if cell.known != 0 {
		return s.matchKnownShape(cell.known, shape, origin)
	}
	if cell.shape != nil {
		changed, ok := s.unifyShapes(*cell.shape, shape, origin)
		if !ok {
			return changed, s.markRootError(root)
		}
		return changed, true
	}
	copy := cloneShape(shape)
	cell.shape = &copy
	return true, true
}

func (s *Session) unifyShapes(a, b Shape, origin Origin) (bool, bool) {
	if !s.stepDecompose(origin) {
		return false, false
	}
	if a.kind == shapeLeaf && b.kind == shapeLeaf {
		return s.unify(a.term, b.term, origin)
	}
	if a.kind == shapeLeaf {
		return s.unifyShapeWithTerm(b, a.term, origin)
	}
	if b.kind == shapeLeaf {
		return s.unifyShapeWithTerm(a, b.term, origin)
	}
	if a.kind != b.kind || a.length != b.length || a.convention != b.convention || a.variadic != b.variadic || a.declaration != b.declaration || len(a.children) != len(b.children) {
		return false, s.conflict(CodeUnification, "incompatible algebraic type shapes", origin)
	}
	changed := false
	for i := range a.children {
		c, ok := s.unifyShapes(a.children[i], b.children[i], childOrigin(origin, i))
		changed = changed || c
		if !ok {
			return changed, false
		}
	}
	return changed, true
}

func (s *Session) matchKnownShape(id types.TypeID, shape Shape, origin Origin) (bool, bool) {
	if shape.kind == shapeLeaf {
		return s.unify(Term{owner: s.token, kind: termKnown, known: id}, shape.term, origin)
	}
	if !s.stepDecompose(origin) {
		return false, false
	}
	key, ok := s.program.typeKey(id)
	if !ok {
		return false, s.conflict(CodeResourceLimit, "shape match uses a foreign TypeID", origin)
	}
	var children []types.TypeID
	switch shape.kind {
	case shapePointer:
		if key.Kind() != types.Pointer {
			return false, s.conflict(CodeUnification, "expected pointer type", origin)
		}
		child, _ := key.Child()
		children = []types.TypeID{child}
	case shapeArray:
		length, child, ok := key.Array()
		if !ok || length != shape.length {
			return false, s.conflict(CodeUnification, "array shape mismatch", origin)
		}
		children = []types.TypeID{child}
	case shapeSlice:
		if key.Kind() != types.Slice {
			return false, s.conflict(CodeUnification, "expected slice type", origin)
		}
		child, _ := key.Child()
		children = []types.TypeID{child}
	case shapeTuple:
		var ok bool
		children, ok = key.Elements()
		if !ok || len(children) != len(shape.children) {
			return false, s.conflict(CodeUnification, "tuple shape mismatch", origin)
		}
	case shapeOptional:
		if key.Kind() != types.Optional {
			return false, s.conflict(CodeUnification, "expected optional type", origin)
		}
		child, _ := key.Child()
		children = []types.TypeID{child}
	case shapeFunction:
		convention, params, result, variadic, ok := key.Function()
		if !ok || convention != shape.convention || variadic != shape.variadic || len(params)+1 != len(shape.children) {
			return false, s.conflict(CodeUnification, "function shape mismatch", origin)
		}
		children = append(params, result)
	case shapeNominal:
		declaration, args, ok := key.Nominal()
		if !ok || declaration != shape.declaration || len(args) != len(shape.children) {
			return false, s.conflict(CodeUnification, "nominal type mismatch", origin)
		}
		children = args
	default:
		return false, s.conflict(CodeResourceLimit, "invalid shape kind", origin)
	}
	changed := false
	for i, child := range children {
		c, ok := s.matchKnownShape(child, shape.children[i], childOrigin(origin, i))
		changed = changed || c
		if !ok {
			return changed, false
		}
	}
	return changed, true
}

func (s *Session) occurs(root InferID, shape Shape) bool {
	stack := []Shape{shape}
	seen := make(map[InferID]bool)
	for len(stack) > 0 {
		last := len(stack) - 1
		value := stack[last]
		stack = stack[:last]
		if !s.stepDecompose(Origin{}) {
			return true
		}
		if value.kind == shapeLeaf && value.term.kind != termKnown && value.term.kind != termError {
			candidate := s.find(value.term.id)
			if candidate == root {
				return true
			}
			if candidate != 0 && !seen[candidate] {
				seen[candidate] = true
				if attached := s.cells[candidate-1].shape; attached != nil {
					stack = append(stack, *attached)
				}
			}
		}
		for i := len(value.children) - 1; i >= 0; i-- {
			stack = append(stack, value.children[i])
		}
	}
	return false
}

func (s *Session) materializeShape(shape Shape) (types.TypeID, bool) {
	if shape.kind == shapeLeaf {
		return s.resolvedType(shape.term)
	}
	children := make([]types.TypeID, len(shape.children))
	for i, child := range shape.children {
		id, ok := s.materializeShape(child)
		if !ok {
			return 0, false
		}
		children[i] = id
	}
	var key types.TypeKey
	switch shape.kind {
	case shapePointer:
		key = types.PointerKey(children[0])
	case shapeArray:
		key = types.ArrayKey(shape.length, children[0])
	case shapeSlice:
		key = types.SliceKey(children[0])
	case shapeTuple:
		key = types.TupleKey(children)
	case shapeOptional:
		key = types.OptionalKey(children[0])
	case shapeFunction:
		key = types.FunctionKey(shape.convention, children[:len(children)-1], children[len(children)-1], shape.variadic)
	case shapeNominal:
		key = types.NominalKey(shape.declaration, children)
	default:
		return 0, false
	}
	if s.speculative {
		return 0, false
	}
	id, err := s.program.internType(key)
	if err != nil {
		s.conflict(CodeResourceLimit, fmt.Sprintf("cannot intern inferred type: %v", err), Origin{})
		return 0, false
	}
	return id, true
}

func (s *Session) materializeReadyShapes() (bool, bool) {
	if s.Fatal() {
		return false, false
	}
	changed := false
	for index := range s.cells {
		if s.Fatal() {
			return changed, false
		}
		id := InferID(index + 1)
		if s.find(id) != id {
			continue
		}
		cell := &s.cells[index]
		if cell.error || cell.known != 0 || cell.shape == nil {
			continue
		}
		known, ok := s.materializeShape(*cell.shape)
		if s.Fatal() {
			return changed, false
		}
		if !ok {
			continue
		}
		if !s.fitLiterals(cell.literals, known, cell.origin) || !s.checkCapabilities(cell.capabilities, known, cell.origin) {
			if s.Fatal() {
				return changed, false
			}
			cell.error = true
			continue
		}
		cell.known = known
		changed = true
	}
	return changed, true
}

func (s *Session) resolvedType(term Term) (types.TypeID, bool) {
	if !term.belongs(s.token) || term.kind == termError {
		return 0, false
	}
	if term.kind == termKnown {
		return term.known, true
	}
	root := s.find(term.id)
	if root == 0 {
		return 0, false
	}
	cell := s.cells[root-1]
	if cell.error {
		return 0, false
	}
	return cell.known, cell.known != 0
}

func (s *Session) stepUnify(origin Origin) bool {
	s.unificationSteps++
	if s.unificationSteps > s.config.MaxUnificationSteps {
		s.limit("unification step", s.config.MaxUnificationSteps, origin)
		s.failed = true
		return false
	}
	return true
}
func (s *Session) stepDecompose(origin Origin) bool {
	s.decompositionSteps++
	if s.decompositionSteps > s.config.MaxDecompositionSteps {
		s.limit("decomposition step", s.config.MaxDecompositionSteps, origin)
		s.failed = true
		return false
	}
	return true
}

func (s *Session) markRootConflict(root InferID, code diagnostic.Code, message string, origin Origin) bool {
	s.conflict(code, message, origin, s.cells[root-1].origin)
	if !s.Fatal() {
		s.cells[root-1].error = true
	}
	return false
}
func (s *Session) markRootsConflict(a, b InferID, code diagnostic.Code, message string, origin Origin) bool {
	s.conflict(code, message, origin, s.cells[a-1].origin, s.cells[b-1].origin)
	if !s.Fatal() {
		s.cells[a-1].error = true
		s.cells[b-1].error = true
	}
	return false
}
func (s *Session) conflict(code diagnostic.Code, message string, origin Origin, related ...Origin) bool {
	if code == CodeResourceLimit {
		s.markFatal()
	}
	if s.speculative {
		if s.speculativeConflict == nil || (code == CodeResourceLimit && s.speculativeConflict.code != CodeResourceLimit) {
			s.speculativeConflict = &inferenceConflict{code: code, message: message, origin: origin, related: append([]Origin(nil), related...)}
		}
	} else {
		s.reporter.error(code, message, origin, related...)
	}
	s.failed = true
	return false
}
func (s *Session) markRootError(root InferID) bool {
	if !s.Fatal() {
		s.cells[root-1].error = true
	}
	return false
}
func (s *Session) markRootsError(a, b InferID) bool {
	if !s.Fatal() {
		s.cells[a-1].error = true
		s.cells[b-1].error = true
	}
	return false
}

func cloneLiteralValues(values []literalValue) []literalValue {
	out := make([]literalValue, len(values))
	for i := range values {
		out[i] = values[i].clone()
	}
	return out
}

func appendCapabilityEvidence(values []capabilityEvidence, additions ...capabilityEvidence) []capabilityEvidence {
	for _, addition := range additions {
		duplicate := false
		for _, existing := range values {
			if existing.capability == addition.capability && existing.origin.GenericOwner == addition.origin.GenericOwner && existing.origin.Syntax == addition.origin.Syntax {
				duplicate = true
				break
			}
		}
		if !duplicate {
			values = append(values, addition)
		}
	}
	return values
}
func originBefore(a, b Origin) bool {
	if originEmpty(b) {
		return !originEmpty(a)
	}
	if originEmpty(a) {
		return false
	}
	if a.Syntax.Module != b.Syntax.Module {
		return a.Syntax.Module < b.Syntax.Module
	}
	if a.Syntax.Node != b.Syntax.Node {
		return a.Syntax.Node < b.Syntax.Node
	}
	if a.Span.Source != b.Span.Source {
		return a.Span.Source < b.Span.Source
	}
	if a.Span.Start != b.Span.Start {
		return a.Span.Start < b.Span.Start
	}
	if a.Span.End != b.Span.End {
		return a.Span.End < b.Span.End
	}
	if a.Role != b.Role {
		return a.Role < b.Role
	}
	if a.Symbol != b.Symbol {
		return a.Symbol < b.Symbol
	}
	return a.GenericOwner < b.GenericOwner
}

func originEmpty(value Origin) bool {
	if value.Syntax.Module != 0 || value.Syntax.Node != 0 || value.Span.Source != 0 || value.Span.Start != 0 || value.Span.End != 0 || value.Role != "" || value.Symbol != 0 || value.GenericOwner != 0 {
		return false
	}
	return true
}
func childOrigin(origin Origin, index int) Origin {
	origin.Role = fmt.Sprintf("%s child %d", origin.Role, index+1)
	return origin
}

func (s *Session) describeTypeConflict(a, b types.TypeID) string {
	ka, _ := s.program.typeKey(a)
	kb, _ := s.program.typeKey(b)
	lookup := func(id types.TypeID) (types.TypeKey, bool) { return s.program.typeKey(id) }
	resolve := types.ResolveFromResult(s.program.inputs.Resolution)
	return fmt.Sprintf("cannot unify %s with %s", types.DescribeKeyResolved(ka, lookup, resolve), types.DescribeKeyResolved(kb, lookup, resolve))
}
