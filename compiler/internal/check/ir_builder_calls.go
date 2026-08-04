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
		node.TypeArgs = make([]types.TypeID, 0, len(instantiation.Arguments))
		for _, argument := range instantiation.Arguments {
			if argument.State != infer.TypeFinal || argument.Type == 0 {
				return false
			}
			node.TypeArgs = append(node.TypeArgs, argument.Type)
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
	convention, ok := functionConvention(s.handoff, functionType)
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
	convention, ok := functionConvention(s.handoff, functionType)
	if !ok {
		return false
	}
	receiver, ok := s.buildValue(call.Receiver)
	if !ok {
		return false
	}
	node.Kind = tir.MethodCall
	node.Symbol = method.Method
	node.FunctionType = functionType
	node.Convention = convention
	node.TypeArgs = make([]types.TypeID, 0, len(method.Arguments))
	for _, argument := range method.Arguments {
		if argument.State != infer.TypeFinal || argument.Type == 0 {
			return false
		}
		node.TypeArgs = append(node.TypeArgs, argument.Type)
	}
	action, ok := callContextAction(flow, convention)
	if !ok {
		return false
	}
	node.ContextAction = action
	node.Children = append(node.Children, receiver)
	return s.buildCallChildren(call, node)
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
func functionConvention(handoff *solveHandoff, functionType types.TypeID) (types.CallingConvention, bool) {
	key, ok := handoff.Semantics.Types().Key(functionType)
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
