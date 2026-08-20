package check

import (
	"sort"
	"strconv"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildValue is the single shared, recursive, memoized dispatcher for typed-IR
// value construction. It builds children before parents and memoizes every
// valueID so a value referenced by multiple parents is only built once.
func (s *irBuildState) buildValue(id valueID) (tir.NodeID, bool) {
	return s.buildValueBase(id)
}

func (s *irBuildState) buildValueBase(id valueID) (tir.NodeID, bool) {
	if id == 0 {
		return 0, false
	}
	if existing, ok := s.values[id]; ok {
		return existing, true
	}
	record, ok := s.expressionsByResult[id]
	if !ok {
		return 0, false
	}
	nid, ok := s.buildValueRecord(id, record, record.Header.Syntax)
	if !ok {
		return 0, false
	}
	s.values[id] = nid
	return nid, true
}

// buildValueRecord is buildValueBase's core: it builds one value's typed-IR
// node (and, recursively, its children) WITHOUT memoizing the result. buildValueBase
// memoizes around it; a caller that needs a fresh copy per site — a `let`
// global constant inlined at each reference — calls it directly, because a
// TIR node may be owned by only one function, and the same constant referenced
// from two helpers must not share a node.
func (s *irBuildState) buildValueRecord(id valueID, record *expressionRecord, ref symbol.SyntaxRef) (tir.NodeID, bool) {
	typ, ok := s.resolveType(id)
	if !ok {
		return 0, false
	}
	node := tir.Node{Type: typ, Span: record.Header.Span, Syntax: record.Header.Syntax}
	switch record.Kind {
	case expressionCast:
		cast := s.castsByRecord[record.Specialized]
		if cast == nil {
			return 0, false
		}
		sourceType, sourceOK := s.resolveType(cast.Source)
		destination, destinationOK := s.resolveType(cast.Destination)
		if !sourceOK || !destinationOK {
			return 0, false
		}
		child, childOK := s.buildValue(cast.Source)
		if !childOK {
			return 0, false
		}
		if sourceType == destination {
			node.Kind, node.ExplicitCast, node.Children = tir.SourceAlias, true, []tir.NodeID{child}
			break
		}
		// During symbolic build of a generic function body, either side may
		// still contain a TypeParameter. We cannot determine the coercion kind
		// until specialization time when activeSubstitution maps each TypeParam-
		// eter to its concrete instantiation. Defer to specialization by emitting
		// a passthrough SourceAlias here — the real coercion node is emitted
		// when buildSpecialization rebuilds the body with concrete types.
		if hasTypeParameter(s.handoff.Semantics, sourceType) || hasTypeParameter(s.handoff.Semantics, destination) {
			node.Kind, node.ExplicitCast, node.Children = tir.SourceAlias, true, []tir.NodeID{child}
			break
		}
		class := classify(s.handoff.Semantics, sourceType, destination)
		coercion := coercionFor(s.handoff.Semantics, class, sourceType, destination)
		if coercion == coercionNone && (class == compatibleExplicit || class == compatibleForbidden) {
			sourceKey, sourceFound := s.store.Key(sourceType)
			destinationKey, destinationFound := s.store.Key(destination)
			if sourceFound && destinationFound && sourceKey.Kind() == types.Pointer && destinationKey.Kind() == types.Pointer {
				coercion = coercionPointerCast
			}
		}
		coercionNode := map[coercionKind]tir.NodeKind{
			coercionIntegerCast: tir.IntegerCast, coercionIntegerToFloat: tir.IntegerToFloat,
			coercionFloatToInteger: tir.FloatToInteger, coercionFloatCast: tir.FloatCast,
			coercionEnumToInteger: tir.EnumToInteger, coercionCharToInteger: tir.CharToInteger,
			coercionIntegerToChar:         tir.IntegerToChar,
			coercionOptionalIntegerToEnum: tir.OptionalIntegerToEnum,
			coercionCheckedIntegerToEnum:  tir.CheckedIntegerToEnum, coercionPointerCast: tir.PointerCast,
			coercionPointerToInteger: tir.PointerToInteger,
		}[coercion]
		if coercionNode == 0 {
			return 0, false
		}
		node.Kind, node.Children = coercionNode, []tir.NodeID{child}
	case expressionLiteral:
		if !s.buildLiteral(record, &node) {
			return 0, false
		}
	case expressionName, expressionPath:
		if initializer, ok := s.globalLetInitializers[record.Symbol]; ok {
			initializerRecord, found := s.expressionsByResult[initializer]
			if !found {
				return 0, false
			}
			return s.buildValueRecord(initializer, initializerRecord, symbol.SyntaxRef{})
		}
		if !s.buildSymbolValue(record, &node) {
			return 0, false
		}
	case expressionFunction:
		callable := s.callableForSyntax(record.Header.Syntax)
		if callable == nil || callable.Symbol == 0 || len(callable.Captures) != 0 {
			return 0, false
		}
		function := s.functions[callable.Symbol]
		if function == 0 {
			return 0, false
		}
		node.Kind, node.Symbol, node.Function = tir.HoistedFunctionValue, callable.Symbol, function
	case expressionMember:
		if member := s.membersByResult[id]; member != nil && (member.Kind == memberField || member.Kind == memberMethod || member.Kind == memberTuple) {
			if member.Kind == memberField && member.Name == "len" && len(record.Children) == 1 {
				baseType, found := s.resolveType(record.Children[0])
				if found {
					if key, keyFound := s.typeKey(baseType); keyFound {
						if length, _, array := key.Array(); array {
							node.Kind = tir.IntegerLiteral
							node.Literal = tir.Literal{Kind: tir.LiteralInteger, IntegerNum: strconv.FormatUint(length, 10), IntegerDen: "1"}
							break
						}
					}
				}
			}
			if place, ok := s.buildPlaceForValue(id); ok {
				node.Kind, node.Children = tir.Load, []tir.NodeID{place}
			} else if len(record.Children) == 1 {
				base, ok := s.buildValue(record.Children[0])
				if !ok {
					return 0, false
				}
				memberID := member.Member
				if memberID == 0 {
					memberID = s.memberSymbol(record.Children[0], member.Name)
				}
				if memberID == 0 && member.Kind == memberField {
					switch member.Name {
					case "len":
						memberID = tir.StructuralFieldLen
					case "data":
						memberID = tir.StructuralFieldData
					case "has_value":
						memberID = tir.StructuralFieldHasValue
					}
				}
				if member.Kind == memberField || member.Kind == memberMethod {
					node.Kind, node.Member, node.Children = tir.FieldValue, memberID, []tir.NodeID{base}
				} else {
					node.Kind, node.Ordinal, node.Children = tir.TupleElementValue, member.TupleOrdinal, []tir.NodeID{base}
				}
			} else {
				return 0, false
			}
		} else if !s.buildVariantMember(record, &node) {
			return 0, false
		}
	case expressionContext:
		node.Kind = tir.ContextValue
		node.ContextAction = tir.ContextExpr
	case expressionSizeof:
		if !s.buildSizeof(record, &node) {
			return 0, false
		}
	case expressionSliceFrom:
		if len(record.Children) != 2 || !s.buildChildren(record, &node) {
			return 0, false
		}
		node.Kind = tir.SliceFromRaw
	case expressionTuple:
		node.Kind = tir.TupleValue
		if !s.buildChildren(record, &node) {
			return 0, false
		}
		if components := s.tuplesBySyntax[record.Header.Syntax]; len(components) != 0 {
			sort.Slice(components, func(i, j int) bool { return components[i].Ordinal < components[j].Ordinal })
			tupleChildren := append([]tir.NodeID(nil), node.Children...)
			typeArgs := make([]types.TypeID, 0, len(components))
			needsCoercion := false
			for _, component := range components {
				destination, ok := s.resolveType(component.Destination)
				if !ok || component.Ordinal >= uint32(len(tupleChildren)) {
					return 0, false
				}
				child := tupleChildren[component.Ordinal]
				sourceType, _ := s.resolveType(component.Source)
				coercion := coercionFor(s.handoff.Semantics, classify(s.handoff.Semantics, sourceType, destination), sourceType, destination)
				needsCoercion = needsCoercion || coercion != coercionNone
				if coercion != coercionNone {
					wrapped, ok := s.addCoercionNode(coercion, destination, child, record.Header.Span, symbol.SyntaxRef{})
					if !ok {
						return 0, false
					}
					child = wrapped
				}
				tupleChildren[component.Ordinal] = child
				typeArgs = append(typeArgs, destination)
			}
			if !needsCoercion {
				break
			}
			sourceTuple, ok := s.addNode(tir.Node{Kind: tir.TupleValue, Type: typ, Span: record.Header.Span, Children: append([]tir.NodeID(nil), node.Children...)}, symbol.SyntaxRef{})
			if !ok {
				return 0, false
			}
			node.Kind, node.TypeArgs, node.Children = tir.TupleCoerce, typeArgs, append([]tir.NodeID{sourceTuple}, tupleChildren...)
		}
	case expressionArray:
		node.Kind = tir.ArrayValue
		if !s.buildChildren(record, &node) {
			return 0, false
		}
	case expressionArrayRepeat:
		node.Kind = tir.ArrayRepeat
		if !s.buildArrayRepeat(record, &node) {
			return 0, false
		}
	case expressionRecordValue:
		if s.buildRecordConstruct(record, &node) {
			break
		}
		if !s.buildTaggedVariantConstruct(record, &node) {
			return 0, false
		}
	case expressionPartialMember:
		if !s.buildEnumVariantShorthand(record, &node) {
			return 0, false
		}
	case expressionCall:
		if !s.buildCall(record, &node) {
			return 0, false
		}
	case expressionGrouped:
		node.Kind = tir.SourceAlias
		node.ExplicitCast = false
		if !s.buildChildren(record, &node) {
			return 0, false
		}
	case expressionSome:
		if len(record.Children) != 1 {
			return 0, false
		}
		payload, ok := s.buildValue(record.Children[0])
		if !ok {
			return 0, false
		}
		if sourceType, sourceOK := s.resolveType(record.Children[0]); sourceOK {
			if key, keyOK := s.store.Key(node.Type); keyOK && key.Kind() == types.Optional {
				if targetPayload, childOK := key.Child(); childOK && sourceType != targetPayload {
					// A `some <payload>` whose payload's own type differs from
					// the SomeOptional's declared payload type — the payload
					// needs a width/type conversion (a u8 local into a ?u32
					// destination, a narrower call result or field read, an
					// f32 value into a ?f64 destination). The destination
					// payload type is pinned by the solve (see
					// expression_facts' SomeExpr finish), so the payload child
					// is wrapped in the ordinary coercion the pair calls for —
					// the exact classify/coercionFor/addCoercionNode machinery
					// the expressionTuple case above uses for a tuple element
					// whose type differs from its destination element, and the
					// same coercion a plain (non-optional) compatible pair
					// accepts.
					class := classify(s.handoff.Semantics, sourceType, targetPayload)
					coercion := coercionFor(s.handoff.Semantics, class, sourceType, targetPayload)
					if coercion != coercionNone {
						if wrapped, ok := s.addCoercionNode(coercion, targetPayload, payload, record.Header.Span, symbol.SyntaxRef{}); ok {
							payload = wrapped
						}
					}
				}
			}
		}
		node.Kind, node.Children = tir.SomeOptional, []tir.NodeID{payload}
	case expressionInterpolated:
		node.Kind = tir.InterpolatedString
		if !s.buildInterpolated(record, &node) {
			return 0, false
		}
	case expressionPrefix, expressionPostfix, expressionBinary:
		op := s.operatorsByResult[id]
		if op == nil {
			op = s.operatorsBySyntax[record.Header.Syntax]
		}
		if op != nil && op.Family == operatorAddress {
			if len(op.Operands) != 1 {
				return 0, false
			}
			if place, ok := s.buildPlaceForValue(op.Operands[0]); ok {
				node.Kind, node.Children = tir.AddressOf, []tir.NodeID{place}
			} else {
				return 0, false
			}
		} else if op != nil && op.Family == operatorDereference {
			if place, ok := s.buildPlaceForValue(id); ok {
				node.Kind, node.Children = tir.Load, []tir.NodeID{place}
			} else {
				return 0, false
			}
		} else if !s.buildOperatorValue(record, &node) {
			return 0, false
		}
	case expressionSlice:
		index := s.indexForValue(id, record.Header.Syntax)
		if index == nil {
			return 0, false
		}
		base, ok := s.buildValue(index.Base)
		if !ok {
			return 0, false
		}
		node.Kind, node.Children = tir.CheckedSlice, []tir.NodeID{base}
		node.SliceStartPresent, node.SliceEndPresent = index.StartPresent, index.EndPresent
		if index.StartPresent {
			start, ok := s.buildValue(index.Start)
			if !ok {
				return 0, false
			}
			node.Children = append(node.Children, start)
		}
		if index.EndPresent {
			end, ok := s.buildValue(index.End)
			if !ok {
				return 0, false
			}
			node.Children = append(node.Children, end)
		}
	case expressionBracket:
		if place, ok := s.buildPlaceForValue(id); ok {
			node.Kind, node.Children = tir.Load, []tir.NodeID{place}
		} else if index := s.indexForValue(id, record.Header.Syntax); index != nil {
			base, ok := s.buildValue(index.Base)
			if !ok {
				return 0, false
			}
			if index.Mode == indexValue {
				start, ok := s.buildValue(index.Start)
				if !ok {
					return 0, false
				}
				node.Kind, node.Children = tir.CheckedIndex, []tir.NodeID{base, start}
			} else {
				node.Kind, node.Children = tir.CheckedSlice, []tir.NodeID{base}
				node.SliceStartPresent, node.SliceEndPresent = index.StartPresent, index.EndPresent
				if index.StartPresent {
					start, ok := s.buildValue(index.Start)
					if !ok {
						return 0, false
					}
					node.Children = append(node.Children, start)
				}
				if index.EndPresent {
					end, ok := s.buildValue(index.End)
					if !ok {
						return 0, false
					}
					node.Children = append(node.Children, end)
				}
			}
		} else if !s.buildGenericFunctionValue(record, &node) {
			return 0, false
		}
	default:
		return 0, false
	}
	if node.Kind == 0 {
		return 0, false
	}
	if ref == (symbol.SyntaxRef{}) {
		node.Syntax = symbol.SyntaxRef{}
	}
	nid, ok := s.addNode(node, ref)
	if !ok {
		return 0, false
	}
	return nid, true
}

func (s *irBuildState) callableForSyntax(ref symbol.SyntaxRef) *callableRecord {
	for _, retained := range s.handoff.Records.Records() {
		if retained.Callable != nil && retained.Header.Syntax == ref {
			return retained.Callable
		}
	}
	return nil
}

// buildGenericFunctionValue handles a bare generic function reference
// (e.g. `identity[i32]` used as a value) whose bracket record has neither an
// index nor a place. It requires the solved instantiation at the bracket site,
// a resolved function symbol, and fully final concrete type arguments; builds
// the specialized declaration so the unit contains runnable typed IR, records
// the matching tir.Instantiation reference, and emits a GenericFunctionValue.
func (s *irBuildState) buildGenericFunctionValue(record *expressionRecord, node *tir.Node) bool {
	instantiation, found := s.handoff.Solution.Instantiation(record.Header.Syntax)
	if !found {
		return false
	}
	sym, ok := s.symbol(instantiation.Generic)
	if !ok || (sym.Kind != symbol.SymbolFunction && sym.Kind != symbol.SymbolExternFunction) {
		return false
	}
	typeArgs := make([]types.TypeID, len(instantiation.Arguments))
	for i, argument := range instantiation.Arguments {
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
		typeArgs[i] = typeArg
	}
	// A bare generic reference inside a generic body (wrap[T] taking max[T] as
	// a value) is solved against the enclosing function's own type parameter.
	// Resolve the type arguments through the active specialization substitution
	// and build the callee's concrete specialization only once they are fully
	// concrete; a symbolic reference keeps its symbolic TypeArgs and builds
	// nothing, since its concrete specialization is built when the enclosing
	// generic is specialized.
	concrete := true
	for _, typeArg := range typeArgs {
		if s.containsTypeParameter(typeArg, 0) {
			concrete = false
			break
		}
	}
	if concrete {
		concreteInstantiation := instantiation
		concreteInstantiation.Arguments = make([]infer.TypeResult, len(typeArgs))
		for i, typeArg := range typeArgs {
			concreteInstantiation.Arguments[i] = infer.TypeResult{State: infer.TypeFinal, Type: typeArg}
		}
		if _, ok := s.buildSpecialization(concreteInstantiation); !ok {
			return false
		}
	}
	ref, err := s.builder.AddInstantiation(tir.Instantiation{
		Site:        record.Header.Syntax,
		Declaration: instantiation.Generic,
		TypeArgs:    typeArgs,
	})
	if err != nil {
		return false
	}
	node.Kind, node.Symbol, node.GenericRef, node.TypeArgs = tir.GenericFunctionValue, instantiation.Generic, ref, typeArgs
	return true
}

func mustType(records *solvedRecords, id valueID) types.TypeID {
	typ, _ := typeOfValue(records, id)
	return typ
}

func (s *irBuildState) buildCompatibility(source valueID, compatibility *compatibilityRecord) (tir.NodeID, bool) {
	sourceType, ok := s.resolveType(source)
	if !ok {
		return 0, false
	}
	destination, ok := s.resolveType(compatibility.Destination)
	if !ok {
		return s.buildValueBase(source)
	}
	if implicitArrayToSlice(s.handoff, compatibility, sourceType, destination) {
		// An array literal directly initializing a slice-typed binding: build
		// the array value and take a full slice of it, mirroring exactly what
		// the two-step workaround (`var arr [N]T = [...]; var s []T = arr[:];`)
		// lowers to — an ArrayValue base wrapped in a full CheckedSlice (no
		// start or end bound), so the backend's existing array-local and slice
		// construction machinery composes unchanged.
		child, ok := s.buildValueBase(source)
		if !ok {
			return 0, false
		}
		return s.addNode(tir.Node{Kind: tir.CheckedSlice, Type: destination, Span: compatibility.Header.Span, Children: []tir.NodeID{child}}, symbol.SyntaxRef{})
	}
	class := classify(s.handoff.Semantics, sourceType, destination)
	if class != compatibleImplicit {
		return s.buildValueBase(source)
	}
	coercion := coercionFor(s.handoff.Semantics, class, sourceType, destination)
	if coercion == coercionNone {
		return s.buildValueBase(source)
	}
	child, ok := s.buildValueBase(source)
	if !ok {
		return 0, false
	}
	return s.addCoercionNode(coercion, destination, child, compatibility.Header.Span, symbol.SyntaxRef{})
}

func (s *irBuildState) addCoercionNode(kind coercionKind, destination types.TypeID, child tir.NodeID, span source.Span, ref symbol.SyntaxRef) (tir.NodeID, bool) {
	irKind := map[coercionKind]tir.NodeKind{
		coercionIntegerCast: tir.IntegerCast, coercionIntegerToFloat: tir.IntegerToFloat,
		coercionFloatToInteger: tir.FloatToInteger, coercionFloatCast: tir.FloatCast,
		coercionOptionalInject: tir.OptionalInject, coercionEnumToInteger: tir.EnumToInteger,
		coercionCharToInteger:         tir.CharToInteger,
		coercionIntegerToChar:         tir.IntegerToChar,
		coercionOptionalIntegerToEnum: tir.OptionalIntegerToEnum, coercionCheckedIntegerToEnum: tir.CheckedIntegerToEnum,
		coercionPointerCast:      tir.PointerCast,
		coercionPointerToInteger: tir.PointerToInteger,
	}[kind]
	if irKind == 0 {
		return 0, false
	}
	return s.addNode(tir.Node{Kind: irKind, Type: destination, Span: span, Children: []tir.NodeID{child}}, ref)
}
