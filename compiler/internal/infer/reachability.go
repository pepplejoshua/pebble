package infer

// inactiveGuardedRoots returns the final representatives whose only published
// or constrained use is an unselected guarded slot. Such roots are deliberately
// invisible to defaulting and unresolved-variable recovery.
func (s *Session) inactiveGuardedRoots() map[InferID]bool {
	inactive := make(map[InferID]bool)
	markCandidate := func(term Term) {
		if root := s.termRoot(term); root != 0 {
			inactive[root] = true
		}
	}
	markLive := func(term Term) {
		if root := s.termRoot(term); root != 0 {
			delete(inactive, root)
		}
	}

	for _, slot := range s.slots {
		if slot.guarded {
			selected, ok := s.selections[slot.choice.constraint]
			if !ok || selected != slot.alternative {
				markCandidate(slot.term)
			}
		}
	}
	if len(inactive) == 0 {
		return inactive
	}
	for _, term := range s.symbolRoots {
		markLive(term)
	}
	for _, term := range s.syntaxRoots {
		markLive(term)
	}
	for _, value := range s.instantiations {
		for _, term := range value.arguments {
			markLive(term)
		}
	}
	for _, value := range s.methodStates {
		for _, term := range value.arguments {
			markLive(term)
		}
	}
	for _, slot := range s.slots {
		if !slot.guarded {
			markLive(slot.term)
			continue
		}
		if selected, ok := s.selections[slot.choice.constraint]; ok && selected == slot.alternative {
			markLive(slot.term)
		}
	}
	for _, entry := range s.constraints {
		if entry.value.kind != constraintOneOf {
			s.visitConstraintTerms(entry.value, markLive)
			continue
		}
		selected, ok := s.selections[entry.id]
		if !ok || int(selected) >= len(entry.value.alternatives) {
			continue
		}
		for _, value := range entry.value.alternatives[selected].Constraints {
			s.visitConstraintTerms(value, markLive)
		}
	}
	return inactive
}

func (s *Session) termRoot(term Term) InferID {
	if !term.belongs(s.token) || term.kind == termKnown || term.kind == termError {
		return 0
	}
	return s.find(term.id)
}

func (s *Session) visitConstraintTerms(value Constraint, visit func(Term)) {
	visit(value.a)
	visit(value.b)
	visit(value.c)
	for _, term := range value.explicit {
		visit(term)
	}
	for _, argument := range value.arguments {
		visit(argument.Source)
		visit(argument.Destination)
	}
	for _, substitution := range value.substitutions {
		visit(substitution.Argument)
	}
	visitShapeTerms(value.shape, visit)
	for _, alternative := range value.alternatives {
		for _, nested := range alternative.Constraints {
			s.visitConstraintTerms(nested, visit)
		}
	}
}

func visitShapeTerms(shape Shape, visit func(Term)) {
	if shape.kind == 0 {
		return
	}
	stack := []Shape{shape}
	for len(stack) != 0 {
		last := len(stack) - 1
		current := stack[last]
		stack = stack[:last]
		if current.kind == shapeLeaf {
			visit(current.term)
		}
		for i := len(current.children) - 1; i >= 0; i-- {
			stack = append(stack, current.children[i])
		}
	}
}
