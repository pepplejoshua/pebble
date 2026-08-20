package infer

import (
	"fmt"
	"strconv"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func (s *Session) structuralField(receiver Term, name string, field Term, origin Origin) (bool, bool, bool) {
	state, known, shape := s.structure(receiver)
	switch state {
	case structuralUnresolved:
		return false, true, true
	case structuralError, structuralRigid:
		return s.recoverTerms(field), true, false
	case structuralKnown:
		if !s.stepDecompose(origin) {
			return false, false, false
		}
		key, ok := s.program.typeKey(known)
		if !ok {
			return s.failStructural("field receiver is not a store-owned type", origin, []Term{field})
		}
		if key.Kind() == types.Pointer {
			known, _ = key.Child()
			key, ok = s.program.typeKey(known)
			if !ok {
				return s.failStructural("field receiver pointee is not a store-owned type", origin, []Term{field})
			}
		}
		if _, _, nominal := key.Nominal(); nominal {
			return s.hasField(receiver, name, field, origin)
		}
		if length, _, array := key.Array(); array {
			if name != "len" {
				return s.failStructural("array type has no field named "+name, origin, []Term{field})
			}
			return s.arrayLengthField(length, field, origin)
		}
		if key.Kind() == types.Slice {
			element, _ := key.Child()
			switch name {
			case "len":
				changed, success := s.unify(field, s.Known(s.program.builtins().Uint), origin)
				return changed, success, false
			case "data":
				changed, success := s.constrainShape(field, PointerShape(Leaf(s.Known(element))), origin)
				return changed, success, false
			default:
				return s.failStructural("slice type has no field named "+name, origin, []Term{field})
			}
		}
		if key.Kind() == types.Optional {
			if name != "has_value" {
				return s.failStructural("optional type has no field named "+name, origin, []Term{field})
			}
			changed, success := s.unify(field, s.Known(s.program.builtins().Bool), origin)
			return changed, success, false
		}
		if known == s.program.builtins().Str && name == "len" {
			changed, success := s.unify(field, s.Known(s.program.builtins().Uint), origin)
			return changed, success, false
		}
		return s.failStructural("type has no structural field named "+name, origin, []Term{field})
	case structuralShape:
		if shape.kind == shapeLeaf {
			return false, true, true
		}
		if shape.kind == shapePointer && len(shape.children) == 1 {
			shape = shape.children[0]
			// A pointer shape produced by address-of keeps its pointee as a
			// leaf term. That term may acquire its concrete/nominal shape later
			// (notably when it is a slice index, possibly reached through
			// further field projections, e.g. &self.field[i]), and the
			// element type's own shape can itself resolve through more than
			// one deferred leaf hop. Follow it until it settles, bailing out
			// as still-pending (rather than failing) if a hop makes no
			// progress or the chain runs unexpectedly long.
			for hops := 0; shape.kind == shapeLeaf && hops < 8; hops++ {
				childState, childKnown, childShape := s.structure(shape.term)
				switch childState {
				case structuralKnown:
					return s.structuralField(s.Known(childKnown), name, field, origin)
				case structuralShape:
					if childShape.kind == shapeLeaf && childShape.term == shape.term {
						return false, true, true
					}
					shape = childShape
				default:
					return false, true, true
				}
			}
		}
		if shape.kind == shapeNominal {
			return s.hasField(receiver, name, field, origin)
		}
		if !s.stepDecompose(origin) {
			return false, false, false
		}
		switch shape.kind {
		case shapeArray:
			if name != "len" {
				return s.failStructural("array type has no field named "+name, origin, []Term{field})
			}
			return s.arrayLengthField(shape.length, field, origin)
		case shapeSlice:
			switch name {
			case "len":
				changed, success := s.unify(field, s.Known(s.program.builtins().Uint), origin)
				return changed, success, false
			case "data":
				changed, success := s.constrainShape(field, PointerShape(shape.children[0]), origin)
				return changed, success, false
			default:
				return s.failStructural("slice type has no field named "+name, origin, []Term{field})
			}
		case shapeOptional:
			if name != "has_value" {
				return s.failStructural("optional type has no field named "+name, origin, []Term{field})
			}
			changed, success := s.unify(field, s.Known(s.program.builtins().Bool), origin)
			return changed, success, false
		default:
			return s.failStructural("type has no structural field named "+name, origin, []Term{field})
		}
	default:
		return false, true, true
	}
}

func (s *Session) arrayLengthField(length uint64, field Term, origin Origin) (bool, bool, bool) {
	changed, success := s.unify(field, s.Known(s.program.builtins().Uint), origin)
	if !success {
		return changed, false, false
	}
	literal := s.IntegerLiteral([]byte(strconv.FormatUint(length, 10)), origin)
	literalChanged, literalSuccess := s.unify(field, literal, origin)
	return changed || literalChanged, literalSuccess, false
}

func (s *Session) instantiate(value Constraint) (bool, bool) {
	template, ok := s.program.Template(value.template)
	if !ok {
		return false, s.conflict(CodeInvalidType, "generic instantiation uses an invalid template", value.origin)
	}
	mapping := make(map[symbol.SymbolID]Term, len(value.substitutions))
	for _, substitution := range value.substitutions {
		if _, exists := mapping[substitution.Parameter]; exists {
			return false, s.conflict(CodeInvalidType, "generic instantiation repeats a substitution", value.origin)
		}
		mapping[substitution.Parameter] = substitution.Argument
	}
	shape, ok := s.templateShape(template, mapping, value.origin, 0)
	if !ok {
		return false, false
	}
	return s.constrainShape(value.a, shape, value.origin)
}

func (s *Session) templateShape(value TypeTemplate, mapping map[symbol.SymbolID]Term, origin Origin, depth uint32) (Shape, bool) {
	if depth >= s.config.MaxTypeSyntaxDepth {
		s.limit("template expansion depth", uint64(s.config.MaxTypeSyntaxDepth), origin)
		return Shape{}, false
	}
	switch value.Kind {
	case TemplateKnown:
		return Leaf(s.Known(value.Known)), true
	case TemplateParameter:
		if term, ok := mapping[value.Parameter]; ok {
			return Leaf(term), true
		}
		id, ok := s.program.typeParams[value.Parameter]
		if !ok {
			s.conflict(CodeInvalidType, "generic instantiation is missing a substitution", origin)
			return Shape{}, false
		}
		return Leaf(s.Known(id)), true
	}
	children := make([]Shape, len(value.Children))
	for index, childID := range value.Children {
		child, ok := s.program.Template(childID)
		if !ok {
			s.conflict(CodeInvalidType, "generic template contains an invalid child", childOrigin(origin, index))
			return Shape{}, false
		}
		children[index], ok = s.templateShape(child, mapping, childOrigin(origin, index), depth+1)
		if !ok {
			return Shape{}, false
		}
	}
	switch value.Kind {
	case TemplatePointer:
		return PointerShape(children[0]), true
	case TemplateArray:
		return ArrayShape(value.Length, children[0]), true
	case TemplateSlice:
		return SliceShape(children[0]), true
	case TemplateTuple:
		return TupleShape(children), true
	case TemplateOptional:
		return OptionalShape(children[0]), true
	case TemplateFunction:
		return FunctionShape(value.Convention, children[:len(children)-1], children[len(children)-1], value.Variadic), true
	case TemplateNominal:
		return NominalShape(value.Declaration, children), true
	default:
		s.conflict(CodeInvalidType, "generic template has an invalid algebraic kind", origin)
		return Shape{}, false
	}
}

func (s *Session) hasField(receiver Term, name string, field Term, origin Origin) (bool, bool, bool) {
	if receiver.kind == termError || field.kind == termError {
		return false, true, false
	}
	var declaration symbol.SymbolID
	var arguments []Term
	if id, known := s.resolvedType(receiver); known {
		key, _ := s.program.typeKey(id)
		if key.Kind() == types.Pointer {
			id, _ = key.Child()
			key, _ = s.program.typeKey(id)
		}
		decl, ids, nominal := key.Nominal()
		if !nominal {
			return false, s.fieldConflict(receiver, field, CodeCapability, "field receiver is not a nominal type", origin), false
		}
		declaration = decl
		for _, arg := range ids {
			arguments = append(arguments, s.Known(arg))
		}
	} else if receiver.kind != termKnown {
		root := s.find(receiver.id)
		if root == 0 || s.cells[root-1].error {
			return false, true, false
		}
		shape := s.cells[root-1].shape
		if shape == nil {
			return false, true, true
		}
		if shape.kind == shapePointer && len(shape.children) == 1 {
			shape = &shape.children[0]
		}
		if shape.kind != shapeNominal {
			return false, true, true
		}
		declaration = shape.declaration
		for _, child := range shape.children {
			if child.kind != shapeLeaf {
				return false, true, true
			}
			arguments = append(arguments, child.term)
		}
	}
	decl, ok := s.program.TypeDeclaration(declaration)
	if !ok || decl.State != DeclarationReady {
		return false, s.fieldConflict(receiver, field, CodeCapability, "field receiver declaration is unavailable", origin), false
	}
	var member TemplateID
	for _, candidate := range decl.Members {
		sym, exists := s.program.inputs.Resolution.Symbols.Symbol(candidate.Symbol)
		if exists && sym.Name == name {
			member = candidate.Type
			break
		}
	}
	if member == 0 {
		message := "nominal type has no field named " + name
		if suggestion, ok := diagnostic.Suggest(name, s.memberCandidates(declaration)); ok {
			message = fmt.Sprintf("nominal type has no field named %q (did you mean %q?)", name, suggestion)
		}
		return false, s.fieldConflict(receiver, field, CodeCapability, message, origin), false
	}
	mapping := make(map[symbol.SymbolID]Term, len(decl.Parameters))
	if len(arguments) != len(decl.Parameters) {
		return false, s.conflict(CodeInvalidType, "nominal field substitution arity mismatch", origin), false
	}
	for index, parameter := range decl.Parameters {
		mapping[parameter] = arguments[index]
	}
	template, _ := s.program.Template(member)
	shape, ok := s.templateShape(template, mapping, origin, 0)
	if !ok {
		return false, false, false
	}
	changed, success := s.unifyShapeWithTerm(shape, field, origin)
	return changed, success, false
}

// memberCandidates returns the declared field, variant, and method names of a
// nominal type, for use in did-you-mean suggestions on member access.
func (s *Session) memberCandidates(declaration symbol.SymbolID) []string {
	var names []string
	for _, candidateID := range s.program.inputs.Resolution.Members(declaration) {
		if candidate, exists := s.program.inputs.Resolution.Symbols.Symbol(candidateID); exists && candidate.Name != "" {
			names = append(names, candidate.Name)
		}
	}
	return names
}

func (s *Session) selectMethod(value Constraint) (bool, bool, bool) {
	if value.a.kind == termError || value.b.kind == termError {
		return false, true, false
	}
	declaration, receiverArguments, delayed, ok := s.receiverNominal(value.a, value.origin)
	if delayed || !ok {
		if !ok && !delayed {
			s.taintResult(value.b)
		}
		return false, ok, delayed
	}
	decl, ok := s.program.TypeDeclaration(declaration)
	if !ok || decl.State != DeclarationReady || decl.Form != DeclarationNominal {
		return false, s.conflict(CodeCapability, "method receiver declaration is unavailable", value.origin), false
	}

	var selected symbol.Symbol
	for _, candidateID := range s.program.inputs.Resolution.Members(declaration) {
		candidate, exists := s.program.inputs.Resolution.Symbols.Symbol(candidateID)
		if !exists || candidate.Error || candidate.Name != value.name {
			continue
		}
		if candidate.Kind != symbol.SymbolMethod {
			return false, s.conflict(CodeCapability, "selected member is not an instance method", value.origin), false
		}
		if selected.ID != 0 {
			return false, s.conflict(CodeDamagedInput, "receiver declaration has duplicate method identities", value.origin), false
		}
		selected = candidate
	}
	if selected.ID == 0 {
		return false, s.conflict(CodeCapability, "nominal type has no method named "+value.name, value.origin), false
	}
	signature, ok := s.program.Signature(selected.ID)
	if !ok || signature.State != DeclarationReady || len(signature.TypeParams) < len(decl.Parameters) {
		return false, s.conflict(CodeDamagedInput, "selected method signature is unavailable", value.origin), false
	}
	for i, parameter := range decl.Parameters {
		if signature.TypeParams[i] != parameter {
			return false, s.conflict(CodeDamagedInput, "method signature has a damaged containing-type environment", value.origin), false
		}
	}
	if len(receiverArguments) != len(decl.Parameters) {
		return false, s.conflict(CodeDamagedInput, "method receiver substitution arity mismatch", value.origin), false
	}

	state, exists := s.methodStates[value.site]
	if !exists {
		localParameters := signature.TypeParams[len(decl.Parameters):]
		if len(value.explicit) > len(localParameters) {
			return false, s.conflict(CodeInvalidType, "method has too many explicit type arguments", value.origin), false
		}
		arguments := append([]Term(nil), value.explicit...)
		missing := len(localParameters) - len(arguments)
		if uint64(len(s.cells))+uint64(missing) > uint64(s.config.MaxInferVariables) {
			s.limit("inference variable", uint64(s.config.MaxInferVariables), value.origin)
			return false, false, false
		}
		for len(arguments) < len(localParameters) {
			term := s.newCell(childOrigin(value.origin, len(arguments)), termVariable, nil)
			if term.kind == termError {
				return false, false, false
			}
			arguments = append(arguments, term)
		}
		state = methodState{method: selected.ID, arguments: arguments, ownedFrom: len(value.explicit)}
		s.methodStates[value.site] = state
	} else if state.method != selected.ID {
		return false, s.conflict(CodeDamagedInput, "method selection changed after receiver resolution", value.origin), false
	}

	mapping := make(map[symbol.SymbolID]Term, len(signature.TypeParams))
	for i, parameter := range decl.Parameters {
		mapping[parameter] = receiverArguments[i]
	}
	localParameters := signature.TypeParams[len(decl.Parameters):]
	for i, parameter := range localParameters {
		mapping[parameter] = state.arguments[i]
	}
	parameters := make([]Shape, len(signature.Inputs))
	for i, templateID := range signature.Inputs {
		template, exists := s.program.Template(templateID)
		if !exists {
			return false, s.failMethodState(value.site, "method parameter template is unavailable", value.origin), false
		}
		shape, success := s.templateShape(template, mapping, childOrigin(value.origin, i), 0)
		if !success {
			s.failMethodArguments(value.site)
			return false, false, false
		}
		parameters[i] = shape
	}
	resultTemplate, exists := s.program.Template(signature.Result)
	if !exists {
		return false, s.failMethodState(value.site, "method result template is unavailable", value.origin), false
	}
	result, success := s.templateShape(resultTemplate, mapping, childOrigin(value.origin, len(parameters)), 0)
	if !success {
		s.failMethodArguments(value.site)
		return false, false, false
	}
	if len(parameters) != 0 {
		pointerReceiver := parameters[0].kind == shapePointer
		if !pointerReceiver && parameters[0].kind == shapeLeaf {
			if id, known := s.resolvedType(parameters[0].term); known {
				if key, found := s.program.typeKey(id); found {
					pointerReceiver = key.Kind() == types.Pointer
				}
			}
		}
		s.methodPointerReceivers[value.site] = pointerReceiver
	}
	changed, success := s.constrainShape(value.b, FunctionShape(signature.Convention, parameters, result, signature.Variadic), value.origin)
	if !success {
		s.failMethodArguments(value.site)
		return changed, false, false
	}
	state.ready = true
	s.methodStates[value.site] = state
	return changed, true, false
}

func (s *Session) callMember(value Constraint) (bool, bool, bool) {
	if value.a.kind == termError || value.b.kind == termError || value.c.kind == termError {
		return false, true, false
	}
	declaration, _, delayed, ok := s.receiverNominal(value.a, value.origin)
	if delayed || !ok {
		if !ok && !delayed {
			s.taintResult(value.b)
			s.taintResult(value.c)
			for _, a := range value.arguments {
				s.taintResult(a.Destination)
			}
		}
		return false, ok, delayed
	}
	method := false
	for _, candidateID := range s.program.inputs.Resolution.Members(declaration) {
		candidate, exists := s.program.inputs.Resolution.Symbols.Symbol(candidateID)
		if exists && !candidate.Error && candidate.Name == value.name && candidate.Kind == symbol.SymbolMethod {
			method = true
			break
		}
	}
	if method {
		changed, success, delayed := s.selectMethod(Constraint{a: value.a, b: value.b, name: value.name, site: value.site, explicit: value.explicit, origin: value.origin})
		if delayed || !success {
			return changed, success, delayed
		}
		// A generic method — one declaring type parameters of its own beyond
		// the containing type's inherited ones — needs every argument tied to
		// its instantiated parameter, mirroring the direct-call rule in the
		// walker (call_facts.go finishCall): a non-literal argument (a bare
		// function value, a call result, a field read) grounds the method's own
		// inferred type argument. Without this, a method-local type parameter
		// that appears only inside a function-typed parameter (e.g. `fn
		// convert[K, R](self Outer[K], conv fn(K) R)`) stays a free inference
		// cell even when the argument's concrete signature names it (fn(int)
		// int grounds R), and the solve reports it as unresolvable. The direct
		// call adds Equal(source, destination) at the walker because its callee
		// symbol is known there; an instance method call's callee symbol is
		// resolved only here in the solver, so the same grounding is added
		// here, once the resolved method is known to declare its own type
		// parameters.
		if state, stateOK := s.methodStates[value.site]; stateOK && state.ready {
			if signature, sigOK := s.program.Signature(state.method); sigOK && signature.State == DeclarationReady {
				if decl, declOK := s.program.TypeDeclaration(declaration); declOK && decl.State == DeclarationReady {
					if len(signature.TypeParams) > len(decl.Parameters) {
						for _, argument := range value.arguments {
							if _, unifyOK := s.unify(argument.Source, argument.Destination, childOrigin(value.origin, 0)); !unifyOK {
								s.failMethodArguments(value.site)
								return changed, false, false
							}
						}
					}
					// An aggregate-literal argument — an array literal, a tuple
					// literal, or a `some` construction — infers its OWN
					// structural type at walk time, because a method call's
					// argument destinations are fresh slots (an instance
					// method's symbol is resolvable only here in the solver, so
					// prepareCall cannot anchor them as KNOWN the way
					// prepareDirect anchors a direct call's parameters). Left
					// alone, the literal's self-typed structure (`[3]int` from
					// int elements, `?int`, `(int, int)`) survives into the
					// compatibility record and fails classify() as
					// array/optional/tuple vs the concrete parameter type
					// (`[3]i32`, `?i32`, `(i32, i32)`), the same C0601 a plain
					// `take([1, 2, 3])` call would get without an anchor. The
					// parameter types ARE concretely known here — selectMethod
					// just substituted them — so grounding an argument whose
					// source cell still carries an unresolved aggregate SHAPE
					// to its destination binds the literal's element/payload
					// types to the parameter type, exactly the grounding a
					// direct call's walk-time anchor produces. The shape gate
					// keeps already-typed values untouched: a non-literal
					// argument of a mismatched scalar width (u8 into u32, i32
					// into i64) has no shape and stays on the ordinary
					// compatibility/coercion path — that widening gap is
					// general (it reproduces for a plain call) and separately
					// tracked, not a method-specific shape gap.
					for _, argument := range value.arguments {
						sourceRoot := s.find(argument.Source.id)
						if sourceRoot == 0 || s.cells[sourceRoot-1].shape == nil {
							continue
						}
						if _, unifyOK := s.unify(argument.Source, argument.Destination, childOrigin(value.origin, 0)); !unifyOK {
							s.failMethodArguments(value.site)
							return changed, false, false
						}
					}
				}
			}
		}
		receiverShape := Leaf(value.a)
		receiverPointer := false
		pointeeShape := Shape{}
		pointeeKnown := false
		if receiverType, known := s.resolvedType(value.a); known {
			if key, found := s.program.typeKey(receiverType); found {
				receiverPointer = key.Kind() == types.Pointer
				if receiverPointer {
					if pointee, ok := key.Child(); ok {
						pointeeShape = Leaf(s.Known(pointee))
						pointeeKnown = true
					}
				}
			}
		} else if value.a.kind != termKnown {
			root := s.find(value.a.id)
			if root != 0 && !s.cells[root-1].error && s.cells[root-1].shape != nil {
				receiverPointer = s.cells[root-1].shape.kind == shapePointer
				if receiverPointer && len(s.cells[root-1].shape.children) == 1 {
					pointeeShape = s.cells[root-1].shape.children[0]
					pointeeKnown = true
				}
			}
		}
		if s.methodPointerReceivers[value.site] && !receiverPointer {
			receiverShape = PointerShape(receiverShape)
		} else if !s.methodPointerReceivers[value.site] && receiverPointer && pointeeKnown {
			// Mirror of the auto-reference above: the receiver argument is a
			// pointer but the resolved method takes a VALUE receiver. The
			// method's own receiver parameter shape is the pointee, so the
			// receiver argument's shape must be the POINTEE's shape rather
			// than the pointer's — the checker-level half of auto-
			// dereferencing the receiver, matching how a value receiver
			// passed through a pointer already resolves method NAME selection
			// (receiverNominal unwraps the pointer) but the receiver ARGUMENT
			// constraint previously still demanded a pointer-shaped parameter
			// and failed the shape unification. The pointer-receiver-method-
			// on-a-pointer case is untouched: receiverPointer is true there
			// but so is methodPointerReceivers, so the branch above applies
			// and the pointer leaf is kept.
			receiverShape = pointeeShape
		}
		shapes := []Shape{receiverShape}
		for _, argument := range value.arguments {
			shapes = append(shapes, Leaf(argument.Destination))
		}
		shapeChanged, shapeSuccess := s.constrainShape(value.b, FunctionShape(types.Pebble, shapes, Leaf(value.c), false), value.origin)
		return changed || shapeChanged, shapeSuccess, false
	}
	changed, success, delayed := s.hasField(value.a, value.name, value.b, value.origin)
	if delayed || !success {
		return changed, success, delayed
	}
	shapes := make([]Shape, 0, len(value.arguments))
	for _, argument := range value.arguments {
		shapes = append(shapes, Leaf(argument.Destination))
	}
	shapeChanged, shapeSuccess := s.constrainShape(value.b, FunctionShape(types.Pebble, shapes, Leaf(value.c), false), value.origin)
	return changed || shapeChanged, shapeSuccess, false
}

func (s *Session) receiverNominal(receiver Term, origin Origin) (symbol.SymbolID, []Term, bool, bool) {
	if receiver.kind == termError {
		return 0, nil, false, true
	}
	if id, known := s.resolvedType(receiver); known {
		key, ok := s.program.typeKey(id)
		if !ok {
			return 0, nil, false, s.conflict(CodeDamagedInput, "method receiver type is foreign", origin)
		}
		if key.Kind() == types.Pointer {
			id, _ = key.Child()
			key, ok = s.program.typeKey(id)
			if !ok {
				return 0, nil, false, s.conflict(CodeDamagedInput, "method receiver pointee type is foreign", origin)
			}
		}
		declaration, ids, nominal := key.Nominal()
		if !nominal {
			return 0, nil, false, s.receiverConflict(receiver, CodeCapability, s.methodReceiverNotNominal(s.describeKeyResolved(key, origin)), origin)
		}
		arguments := make([]Term, len(ids))
		for i, argument := range ids {
			arguments[i] = s.Known(argument)
		}
		return declaration, arguments, false, true
	}
	if receiver.kind == termKnown {
		name := "<type>"
		if key, ok := s.program.typeKey(receiver.known); ok {
			name = s.describeKeyResolved(key, origin)
		}
		return 0, nil, false, s.receiverConflict(receiver, CodeCapability, s.methodReceiverNotNominal(name), origin)
	}
	root := s.find(receiver.id)
	if root == 0 || s.cells[root-1].error {
		return 0, nil, false, true
	}
	shape := s.cells[root-1].shape
	if shape == nil {
		return 0, nil, true, true
	}
	if shape.kind == shapePointer && len(shape.children) == 1 {
		shape = &shape.children[0]
		// A pointer shape produced by address-of keeps its pointee as a leaf
		// term. That term may already resolve to a known type (a local whose
		// initializer is a record construct) or acquire its nominal shape
		// later, and the pointee's own shape can itself resolve through more
		// than one deferred leaf hop. Follow it until it settles, evaluating a
		// settled leaf through the same nominal lookup the known-type path
		// uses and bailing out as still-pending (rather than failing) if a hop
		// makes no progress or the chain runs unexpectedly long.
		for hops := 0; shape.kind == shapeLeaf && hops < 8; hops++ {
			if id, known := s.resolvedType(shape.term); known {
				return s.receiverNominal(s.Known(id), origin)
			}
			childState, _, childShape := s.structure(shape.term)
			switch childState {
			case structuralShape:
				if childShape.kind == shapeLeaf && childShape.term == shape.term {
					return 0, nil, true, true
				}
				shape = &childShape
			default:
				return 0, nil, true, true
			}
		}
	}
	if shape.kind != shapeNominal {
		return 0, nil, false, s.receiverConflict(receiver, CodeCapability, s.methodReceiverNotNominal(s.receiverShapeName(shape, origin)), origin)
	}
	arguments := make([]Term, len(shape.children))
	for i, child := range shape.children {
		if child.kind != shapeLeaf {
			return 0, nil, true, true
		}
		arguments[i] = child.term
	}
	return shape.declaration, arguments, false, true
}

// receiverConflict reports a method-receiver capability conflict and, when the
// receiver is a live inference variable, taints its union-find root the same
// way markRootConflict taints the participants of a unify failure. Without
// the taint the receiver cell stays unresolved and every constraint that
// structurally depends on it re-reports a fresh T0510 (the diagnostic cascade
// the error-taint mechanism suppresses). A receiver that is a known constant
// (termKnown) or an explicit error term has no cell to taint and cannot
// cascade, so it is reported bare.
func (s *Session) receiverConflict(receiver Term, code diagnostic.Code, message string, origin Origin) bool {
	if receiver.kind == termVariable && receiver.belongs(s.token) {
		if root := s.find(receiver.id); root != 0 && !s.Fatal() {
			s.cells[root-1].error = true
		}
	}
	return s.conflict(code, message, origin)
}

// fieldConflict reports a field-access capability conflict and taints both the
// receiver's root (mirroring receiverConflict) and the constraint's own result
// term -- the field cell passed in -- the same way receiverNominal's failure is
// handled in selectMethod/callMember. A field access' result cell is a fresh
// cell created by the SAME constraint that is failing: it is not downstream of
// the receiver through any other constraint, so tainting the receiver alone
// leaves it unresolved and it re-reports a fresh T0510 (the diagnostic cascade
// the error-taint mechanism suppresses). Known constants and explicit error
// terms have no cell to taint and cannot cascade.
func (s *Session) fieldConflict(receiver, field Term, code diagnostic.Code, message string, origin Origin) bool {
	if receiver.kind == termVariable && receiver.belongs(s.token) {
		if root := s.find(receiver.id); root != 0 && !s.Fatal() {
			s.cells[root-1].error = true
		}
	}
	s.taintResult(field)
	return s.conflict(code, message, origin)
}

// methodReceiverNotNominal builds the "receiver is not a nominal type" message
// with the receiver's actual type name so the diagnostic says what the type
// really was rather than only that it was not nominal.
func (s *Session) methodReceiverNotNominal(name string) string {
	return fmt.Sprintf("cannot call method: %s is not a struct, union, or enum type", name)
}

// typeKeyLookup builds a child-TypeID lookup closure from the session's
// prepared program, for use with types.DescribeKeyResolved.
func (s *Session) typeKeyLookup() func(types.TypeID) (types.TypeKey, bool) {
	return func(id types.TypeID) (types.TypeKey, bool) { return s.program.typeKey(id) }
}

// describeKeyResolved renders a type key for a diagnostic through the
// session's type store and symbol table, qualifying cross-module nominal names
// relative to the module the diagnostic's origin lives in (mirroring the LSP's
// hover/inlay rendering), so a receiver from an imported module reads as e.g.
// "set::Set[str]" rather than bare "Set[str]".
func (s *Session) describeKeyResolved(key types.TypeKey, origin Origin) string {
	return types.DescribeKeyResolved(key, s.typeKeyLookup(), types.ResolveFromResultQualified(s.program.inputs.Resolution, origin.Syntax.Module, types.QualifierMap(s.program.modules[origin.Syntax.Module].Imports)))
}

// receiverShapeName returns a short human-readable name for a receiver whose
// inference shape is not nominal, for use in the method-receiver diagnostic.
func (s *Session) receiverShapeName(shape *Shape, origin Origin) string {
	if shape == nil {
		return "value"
	}
	switch shape.kind {
	case shapePointer:
		return "pointer"
	case shapeArray:
		return "array"
	case shapeSlice:
		return "slice"
	case shapeTuple:
		return "tuple"
	case shapeOptional:
		return "optional"
	case shapeFunction:
		return "function"
	case shapeLeaf:
		if id, known := s.resolvedType(shape.term); known {
			if key, ok := s.program.typeKey(id); ok {
				return s.describeKeyResolved(key, origin)
			}
		}
		return "value"
	}
	return "value"
}

func (s *Session) failMethodState(site symbol.SyntaxRef, message string, origin Origin) bool {
	// Kept separate from argument cleanup so all malformed prepared-program
	// paths recover without leaving hidden inference roots unresolved.
	s.failMethodArguments(site)
	return s.conflict(CodeDamagedInput, message, origin)
}

func (s *Session) failMethodArguments(site symbol.SyntaxRef) {
	if s.Fatal() {
		return
	}
	state := s.methodStates[site]
	for _, term := range state.arguments[state.ownedFrom:] {
		if term.kind == termVariable || term.kind == termIntLiteral || term.kind == termFloatLiteral {
			if root := s.find(term.id); root != 0 {
				s.cells[root-1].error = true
			}
		}
	}
}

func (s *Session) unifyShapeWithTerm(shape Shape, term Term, origin Origin) (bool, bool) {
	if shape.kind == shapeLeaf {
		return s.unify(shape.term, term, origin)
	}
	return s.constrainShape(term, shape, origin)
}
