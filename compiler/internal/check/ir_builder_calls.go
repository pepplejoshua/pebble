package check

import (
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildCall dispatches an expressionCall record onto the matching typed-IR call
// node kind, correlating the frozen callRecord and its contextFlowRecord by the
// shared call-site SyntaxRef.
func (s *irBuildState) buildCall(record *expressionRecord, node *tir.Node) bool {
	call, ok := s.callsBySyntax[record.Header.Syntax]
	if !ok || call == nil {
		return false
	}
	flow, _ := s.contextFlowsBySyntax[record.Header.Syntax]
	switch call.Target.Kind {
	case callDirect:
		return s.buildDirectCall(call, flow, node)
	case callIndirect:
		return s.buildIndirectCall(call, flow, node)
	case callMethod:
		return s.buildMethodCall(call, flow, node)
	case callVariant:
		return s.buildVariantConstruct(call, node)
	}
	return false
}

func (s *irBuildState) buildDirectCall(call *callRecord, flow *contextFlowRecord, node *tir.Node) bool {
	if !call.Target.ConventionKnown || call.Target.Convention == 0 || call.Target.Symbol == 0 {
		return false
	}
	functionType, ok := s.resolveType(call.Callee)
	if !ok {
		return false
	}
	node.Kind = tir.DirectCall
	node.Symbol = call.Target.Symbol
	node.FunctionType = functionType
	node.Convention = call.Target.Convention
	action, ok := callContextAction(flow, node.Convention)
	if !ok {
		return false
	}
	node.ContextAction = action
	if call.Target.Site != (symbol.SyntaxRef{}) {
		instantiation, found := s.handoff.Solution.Instantiation(call.Target.Site)
		if !found {
			return false
		}
		typeArgs := make([]types.TypeID, 0, len(instantiation.Arguments))
		for _, argument := range instantiation.Arguments {
			if argument.State != infer.TypeFinal || argument.Type == 0 {
				return false
			}
			typeArg := argument.Type
			if s.activeSubstitution != nil {
				substituted, err := s.store.Substitute(typeArg, s.activeSubstitution)
				if err != nil {
					return false
				}
				typeArg = substituted
			}
			typeArgs = append(typeArgs, typeArg)
		}
		node.TypeArgs = typeArgs
		// A direct generic call inside a generic body is solved against the
		// enclosing function's own type parameter (clamp[T] calling
		// min(x, hi)), so the solve-time arguments are symbolic. Resolve them
		// through the active specialization substitution and, when they become
		// concrete, build the callee's own specialization so the unit carries a
		// runnable declaration the backend's findCalledFunctionDeclaration can
		// match. Symbolic calls (a generic body built without a substitution)
		// keep their symbolic TypeArgs and build nothing; their concrete
		// specialization is built when the enclosing generic is specialized.
		concrete := true
		for _, typeArg := range typeArgs {
			if s.containsTypeParameter(typeArg, 0) {
				concrete = false
				break
			}
		}
		if concrete {
			signature, hasSignature := s.handoff.Semantics.Signature(call.Target.Symbol)
			if !hasSignature || len(signature.TypeParams) != len(typeArgs) {
				return false
			}
			concreteInstantiation := infer.Instantiation{
				Generic:   call.Target.Symbol,
				Arguments: make([]infer.TypeResult, len(typeArgs)),
			}
			for i, typeArg := range typeArgs {
				concreteInstantiation.Arguments[i] = infer.TypeResult{State: infer.TypeFinal, Type: typeArg}
			}
			if _, ok := s.buildSpecialization(concreteInstantiation); !ok {
				return false
			}
		}
	}
	return s.buildCallChildren(call, node)
}

func (s *irBuildState) buildIndirectCall(call *callRecord, flow *contextFlowRecord, node *tir.Node) bool {
	callee, ok := s.buildValue(call.Callee)
	if !ok {
		return false
	}
	functionType, ok := s.resolveType(call.Callee)
	if !ok {
		return false
	}
	convention, ok := s.functionConvention(functionType)
	if !ok {
		return false
	}
	node.Kind = tir.IndirectCall
	node.FunctionType = functionType
	node.Convention = convention
	action, ok := callContextAction(flow, convention)
	if !ok {
		return false
	}
	node.ContextAction = action
	node.Children = append(node.Children, callee)
	return s.buildCallChildren(call, node)
}

func (s *irBuildState) buildMethodCall(call *callRecord, flow *contextFlowRecord, node *tir.Node) bool {
	method, ok := s.handoff.Solution.Method(call.Target.Site)
	if !ok || method.Method == 0 {
		return s.buildIndirectCall(call, flow, node)
	}
	functionType, ok := s.resolveType(call.Callee)
	if !ok {
		return false
	}
	convention, ok := s.functionConvention(functionType)
	if !ok {
		return false
	}
	receiver, ok := s.buildValue(call.Receiver)
	if !ok {
		return false
	}
	functionKey, functionKeyOK := s.typeKey(functionType)
	if functionKeyOK {
		_, parameters, _, _, isFunction := functionKey.Function()
		if isFunction && len(parameters) != 0 {
			selfType := parameters[0]
			receiverType, receiverTypeOK := s.resolveType(call.Receiver)
			selfKey, selfKeyOK := s.typeKey(selfType)
			receiverKey, receiverKeyOK := s.typeKey(receiverType)
			if receiverTypeOK && selfKeyOK && receiverKeyOK && selfKey.Kind() == types.Pointer && receiverKey.Kind() != types.Pointer {
				pointee, pointeeOK := selfKey.Child()
				if pointeeOK && pointee == receiverType {
					place, placeOK := s.buildPlaceForValue(call.Receiver)
					if !placeOK {
						return false
					}
					address, addressOK := s.addNode(tir.Node{Kind: tir.AddressOf, Type: selfType, Span: call.Header.Span, Children: []tir.NodeID{place}}, symbol.SyntaxRef{})
					if !addressOK {
						return false
					}
					receiver = address
				}
			}
		}
	}
	node.Kind = tir.MethodCall
	node.Symbol = method.Method
	node.FunctionType = functionType
	node.Convention = convention
	// A method whose signature names type parameters — inherited from its
	// containing generic type (a NON-generic method like
	// `fn is_ok(self Result[T, E]) bool`, which the resolver does not mark
	// Generic because it declares no type parameters of its own) OR declared
	// on itself (a generic method like `fn map[U](self Result[T, E], f fn(T)
	// U) Result[U, E]`, marked Generic) — needs a concrete FunctionDeclaration
	// specialization in the unit with TypeArgs exactly matching this node,
	// the same contract a free generic function's DirectCall satisfies and what
	// the backend's findCalledFunctionDeclaration matches against. The full
	// argument list is receiver-bound first (the method's inherited parameters,
	// in the containing type's declared order), then the method's own solved
	// local arguments. Both halves are resolved through the ACTIVE
	// specialization substitution, so a method call inside a generic method
	// body (`self.rehash(...)` while rehash[K,V] is being specialized) gets its
	// concrete arguments from the specialized receiver, not the solve-time
	// symbolic ones. A method with no type parameters at all (on a non-generic
	// type) keeps empty TypeArgs and resolves to its symbolic declaration
	// downstream.
	signature, hasSignature := s.handoff.Semantics.Signature(method.Method)
	if hasSignature && len(signature.TypeParams) != 0 {
		typeArgs, ok := s.methodSpecializationArgs(call, method, signature)
		if !ok {
			return false
		}
		concrete := true
		for _, argument := range typeArgs {
			if s.containsTypeParameter(argument, 0) {
				concrete = false
				break
			}
		}
		if concrete {
			instantiation := infer.Instantiation{
				Generic:   method.Method,
				Arguments: make([]infer.TypeResult, len(typeArgs)),
			}
			for i, argument := range typeArgs {
				instantiation.Arguments[i] = infer.TypeResult{State: infer.TypeFinal, Type: argument}
			}
			if _, ok := s.buildSpecialization(instantiation); !ok {
				return false
			}
		}
		node.TypeArgs = typeArgs
	}
	action, ok := callContextAction(flow, convention)
	if !ok {
		return false
	}
	node.ContextAction = action
	node.Children = append(node.Children, receiver)
	return s.buildCallChildren(call, node)
}

// methodSpecializationArgs assembles the full ordered type-argument list for
// one generic method call: the receiver's concrete nominal arguments first (the
// method's inherited type parameters, in the containing type's declared order),
// then the method's own solved local arguments. The method signature lists its
// type parameters in exactly that order, so the assembled list is directly
// usable as an infer.Instantiation for buildSpecialization. Both halves are
// resolved through the active specialization substitution when one is present:
// a method call inside a generic body (e.g. `self.rehash(...)` while
// rehash[K,V] is itself being specialized) must read its receiver arguments
// from the specialized receiver type and substitute its own local arguments
// with the enclosing specialization's mapping, never the solve-time symbolic
// ones.
func (s *irBuildState) methodSpecializationArgs(call *callRecord, method infer.MethodSelection, signature infer.Signature) ([]types.TypeID, bool) {
	receiverArgs := make([]types.TypeID, 0, len(signature.TypeParams))
	if receiverType, ok := s.resolveType(call.Receiver); ok {
		key, found := s.typeKey(receiverType)
		if found && key.Kind() == types.Pointer {
			receiverType, _ = key.Child()
			key, found = s.typeKey(receiverType)
		}
		if found {
			if _, nominalArgs, nominal := key.Nominal(); nominal {
				receiverArgs = append(receiverArgs, nominalArgs...)
			}
		}
	}
	localArgs := make([]types.TypeID, len(method.Arguments))
	for i, argument := range method.Arguments {
		if argument.State != infer.TypeFinal || argument.Type == 0 {
			return nil, false
		}
		localArgs[i] = argument.Type
		if s.activeSubstitution != nil {
			substituted, err := s.store.Substitute(argument.Type, s.activeSubstitution)
			if err != nil {
				return nil, false
			}
			localArgs[i] = substituted
		}
	}
	full := append(receiverArgs, localArgs...)
	if len(full) != len(signature.TypeParams) {
		return nil, false
	}
	return full, true
}

// containsTypeParameter reports whether typ contains a TypeParameter anywhere in
// its structure. A method call whose specialization arguments still reference a
// type parameter is symbolic — it lives inside a generic body that has not been
// concretely instantiated — so no concrete FunctionDeclaration is built for it
// here; the concrete declaration is built instead when the enclosing generic is
// itself specialized, at which point the active substitution has made these
// arguments concrete. A type missing from the live store or an over-deep walk
// reports true (treated as non-concrete, so nothing is built and the call keeps
// its symbolic TypeArgs).
func (s *irBuildState) containsTypeParameter(typ types.TypeID, depth uint32) bool {
	if typ == 0 || depth >= 32 {
		return true
	}
	key, ok := s.store.Key(typ)
	if !ok {
		return true
	}
	switch key.Kind() {
	case types.TypeParameter:
		return true
	case types.Builtin:
		return false
	case types.Pointer, types.Slice, types.Optional:
		child, _ := key.Child()
		return s.containsTypeParameter(child, depth+1)
	case types.Array:
		_, element, _ := key.Array()
		return s.containsTypeParameter(element, depth+1)
	case types.Tuple, types.Nominal:
		elements, _ := key.Elements()
		for _, element := range elements {
			if s.containsTypeParameter(element, depth+1) {
				return true
			}
		}
		return false
	case types.Function:
		_, parameters, result, _, _ := key.Function()
		for _, parameter := range parameters {
			if s.containsTypeParameter(parameter, depth+1) {
				return true
			}
		}
		return s.containsTypeParameter(result, depth+1)
	}
	return true
}

func (s *irBuildState) buildVariantConstruct(call *callRecord, node *tir.Node) bool {
	if call.Target.Symbol == 0 {
		return false
	}
	node.Kind = tir.VariantConstruct
	node.Member = call.Target.Symbol
	return s.buildCallChildren(call, node)
}

// buildCallChildren builds each ordered fixed argument. The authored argument
// value is callArgument.Source; Destination is a slot-typed compatibility
// bookkeeping value that never becomes a child.
func (s *irBuildState) buildCallChildren(call *callRecord, node *tir.Node) bool {
	sorted := make([]callArgument, len(call.Arguments))
	copy(sorted, call.Arguments)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i].Ordinal < sorted[j].Ordinal })
	for _, argument := range sorted {
		var valueNode tir.NodeID
		var ok bool
		if compatibility := s.compatibilityBySource[argument.Source]; compatibility != nil {
			valueNode, ok = s.buildCompatibility(argument.Source, compatibility)
		} else {
			valueNode, ok = s.buildValue(argument.Source)
		}
		if !ok {
			return false
		}
		node.Children = append(node.Children, valueNode)
	}
	return true
}

// functionConvention extracts the calling convention carried by a function
// value's own type, used where the callRecord does not record one directly.
func (s *irBuildState) functionConvention(functionType types.TypeID) (types.CallingConvention, bool) {
	key, ok := s.typeKey(functionType)
	if !ok {
		return 0, false
	}
	convention, _, _, _, ok := key.Function()
	return convention, ok
}

// callContextAction maps a call's contextFlowRecord to its exact ContextAction.
// contextForward/contextNone carry directly; an indirect call (contextIndirect)
// has no recorded convention and resolves its action from the callee's own
// convention, so every Pebble call forwards context and every C call has none.
func callContextAction(flow *contextFlowRecord, convention types.CallingConvention) (tir.ContextAction, bool) {
	if flow != nil {
		switch flow.Kind {
		case contextForward:
			return tir.ContextForward, true
		case contextNone:
			return tir.ContextNone, true
		case contextIndirect:
		default:
			return 0, false
		}
	}
	switch convention {
	case types.Pebble:
		return tir.ContextForward, true
	case types.C:
		return tir.ContextNone, true
	}
	return 0, false
}
