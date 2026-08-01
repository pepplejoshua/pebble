package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildSpecialization builds (or returns the already-built) typed IR for one
// concrete instantiation of a generic callable. Structurally recursive
// self-instantiation returns the in-progress entry's reserved declaration
// reference when it is available, rather than recursing forever. If recursion
// is encountered before the declaration node is published, this reports that
// honestly; deep recursive-generic behavior is exercised by a later slice.
// Anonymous function literals nested in the specialized body are not handled:
// their HoistedFunctionValue path requires the normal function map entry.
func (s *irBuildState) buildSpecialization(instantiation infer.Instantiation) (tir.NodeID, bool) {
	if s == nil || s.handoff == nil || s.records == nil || s.store == nil || s.builder == nil || s.cache == nil {
		return 0, false
	}
	var callable *callableRecord
	for _, retained := range s.handoff.Records.Records() {
		if retained.Callable != nil && retained.Callable.Symbol == instantiation.Generic {
			callable = retained.Callable
			break
		}
	}
	if callable == nil {
		return 0, false
	}
	signature, ok := s.handoff.Semantics.Signature(instantiation.Generic)
	if !ok || len(signature.TypeParams) != len(instantiation.Arguments) {
		return 0, false
	}
	typeArgs := make([]types.TypeID, len(instantiation.Arguments))
	substitution := make(map[symbol.SymbolID]types.TypeID, len(signature.TypeParams))
	for i, argument := range instantiation.Arguments {
		if argument.State != infer.TypeFinal || argument.Type == 0 {
			return 0, false
		}
		typeArgs[i] = argument.Type
		substitution[signature.TypeParams[i]] = argument.Type
	}
	key := newSpecializationKey(instantiation.Generic, typeArgs, signature.Convention)
	entry, alreadyInProgress := s.cache.reserve(key)
	if entry == nil {
		return 0, false
	}
	if alreadyInProgress {
		return entry.DeclNode, entry.DeclNode != 0
	}
	if entry.State == specializationDone {
		return entry.DeclNode, entry.DeclNode != 0
	}

	specialized, err := buildSpecializedSignature(s.store, s.handoff, s.records, callable, instantiation)
	if err != nil {
		return 0, false
	}
	declared, exists := s.symbol(instantiation.Generic)
	if !exists {
		return 0, false
	}
	fid, err := s.builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: instantiation.Generic, Span: declared.Span})
	if err != nil {
		return 0, false
	}
	entry.FunctionID = fid

	params := make([]tir.Parameter, len(callable.Parameters))
	for i := range callable.Parameters {
		ps, exists := s.symbolForParameter(instantiation.Generic, i)
		if !exists || i >= len(specialized.Parameters) {
			return 0, false
		}
		params[i] = tir.Parameter{Symbol: ps.ID, Type: specialized.Parameters[i]}
		if _, ok := s.addNode(tir.Node{Kind: tir.ParameterDeclaration, Span: ps.Span, Symbol: ps.ID}, symbol.SyntaxRef{}); !ok {
			return 0, false
		}
	}

	previousSubstitution := s.activeSubstitution
	s.activeSubstitution = substitution
	bodyNode, bodyOK := s.withFreshScope(func() (tir.NodeID, bool) {
		s.functions[instantiation.Generic] = fid
		region := s.functionRegions[instantiation.Generic]
		if region == 0 || uint64(region) > uint64(len(s.handoff.Records.Controls())) {
			return 0, false
		}
		bodyRegion := region
		for _, child := range s.handoff.Records.Controls()[region-1].Children {
			if owner := s.owner[child]; owner != nil && owner.Kind == controlBlock {
				bodyRegion = child
				break
			}
		}
		var addRegions func(controlID) bool
		addRegions = func(id controlID) bool {
			if id == 0 || uint64(id) > uint64(len(s.handoff.Records.Controls())) {
				return false
			}
			r, err := s.builder.AddRegion()
			if err != nil {
				return false
			}
			s.regions[id] = r
			for _, child := range s.handoff.Records.Controls()[id-1].Children {
				if !addRegions(child) {
					return false
				}
			}
			return true
		}
		if !addRegions(region) {
			return 0, false
		}
		node, buildable, unsupported := s.buildRegionBlock(bodyRegion, true)
		if unsupported || !buildable {
			return 0, false
		}
		return node, true
	})
	s.activeSubstitution = previousSubstitution
	if !bodyOK {
		return 0, false
	}
	if err := s.builder.CompleteFunctionDecl(fid, bodyNode); err != nil {
		return 0, false
	}
	declNode, ok := s.addNode(tir.Node{
		Kind: tir.FunctionDeclaration, Span: declared.Span, Symbol: instantiation.Generic,
		Function: fid, Parameters: params, ResultType: specialized.Result,
		Convention: signature.Convention, Variadic: callable.Variadic, Inline: callable.Inline, HasBody: true,
	}, symbol.SyntaxRef{})
	if !ok {
		return 0, false
	}
	entry.DeclNode = declNode
	s.cache.finish(key)
	return declNode, true
}
