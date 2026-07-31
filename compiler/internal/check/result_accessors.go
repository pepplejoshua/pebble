package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type valueCategory uint8

const (
	valueCategoryValue valueCategory = iota + 1
	valueCategoryPlace
	valueCategoryNonvalue
	valueCategoryCall
)

type PlaceProjectionResult struct {
	Kind         placeKind
	Base         valueID
	Member       symbol.SymbolID
	TupleOrdinal uint32
	Index        valueID
}

type ExitResult struct {
	Kind   controlExitKind
	Target controlID
}

type contextAction uint8

const (
	contextActionExpression contextAction = iota + 1
	contextActionForward
	contextActionNone
	contextActionIndirect
)

type ExpressionResult struct {
	Syntax   symbol.SyntaxRef
	Span     source.Span
	Type     infer.TypeResult
	Category valueCategory
	Node     tir.NodeID
}
type PlaceResult struct {
	Syntax      symbol.SyntaxRef
	Span        source.Span
	Type        infer.TypeResult
	Kind        placeKind
	Root        symbol.SymbolID
	Writable    bool
	Projections []PlaceProjectionResult
	Node        tir.NodeID
}
type ConversionResult struct {
	Syntax              symbol.SyntaxRef
	Span                source.Span
	Source, Destination types.TypeID
	Class               compatibilityClass
	Coercion            coercionKind
	Role                compatibilityRole
	Ordinal             uint32
	Node                tir.NodeID
}
type CallResult struct {
	Syntax     symbol.SyntaxRef
	Span       source.Span
	Kind       callKind
	Symbol     symbol.SymbolID
	Convention types.CallingConvention
	Variadic   bool
	Arguments  []ConversionResult
	Context    contextAction
	Node       tir.NodeID
}
type MemberResult struct {
	Syntax        symbol.SyntaxRef
	Span          source.Span
	Kind          memberKind
	Owner, Member symbol.SymbolID
	TupleOrdinal  uint32
	Node          tir.NodeID
}
type ControlResult struct {
	Syntax    symbol.SyntaxRef
	Span      source.Span
	Reachable bool
	Exits     []ExitResult
	Target    controlID
	Defers    []tir.NodeID
	Node      tir.NodeID
}

func newResult(h *solveHandoff, records *solvedRecords, requirements map[symbol.SymbolID][]Requirement, unit *tir.Unit, successful bool) *Result {
	r := &Result{successful: successful, solution: h.Solution, records: records, requirements: requirements, ir: unit}
	r.expressions = make(map[symbol.SyntaxRef]ExpressionResult)
	r.places = make(map[symbol.SyntaxRef]PlaceResult)
	r.conversions = make(map[symbol.SyntaxRef]ConversionResult)
	r.calls = make(map[symbol.SyntaxRef]CallResult)
	r.members = make(map[symbol.SyntaxRef]MemberResult)
	r.controls = make(map[symbol.SyntaxRef]ControlResult)
	all := h.Records.Records()
	byID := make(map[recordID]retainedRecord, len(all))
	for i, record := range all {
		byID[recordID(i+1)] = record
	}
	node := func(ref symbol.SyntaxRef) tir.NodeID {
		if unit == nil {
			return 0
		}
		n, _ := unit.SourceMap(ref)
		return n
	}
	typeOf := func(id valueID) infer.TypeResult {
		if records == nil {
			return infer.TypeResult{}
		}
		result, _ := records.Root(id)
		return result
	}
	for _, retained := range all {
		if !activeOperatorRecord(h, retained.Header) {
			continue
		}
		ref, span := retained.Header.Syntax, retained.Header.Span
		if expression := retained.Expression; expression != nil {
			category := valueCategoryValue
			if specialized := byID[expression.Specialized]; specialized.Place != nil {
				category = valueCategoryPlace
			} else if specialized.Call != nil {
				category = valueCategoryCall
			}
			r.expressions[ref] = ExpressionResult{ref, span, typeOf(expression.Result), category, node(ref)}
		}
		if place := retained.Place; place != nil {
			projections := make([]PlaceProjectionResult, len(place.Projections))
			for i, projection := range place.Projections {
				projections[i] = PlaceProjectionResult{projection.Kind, projection.Base, projection.Member, projection.TupleOrdinal, projection.Index}
			}
			kind := placeStorage
			if len(place.Projections) != 0 {
				kind = place.Projections[len(place.Projections)-1].Kind
			}
			r.places[ref] = PlaceResult{ref, span, typeOf(place.Value), kind, place.Root, place.RootMutable, projections, node(ref)}
		}
		if conversion := retained.Compatibility; conversion != nil {
			r.conversions[ref] = makeConversion(h, records, conversion.Header, conversion.Source, conversion.Destination, conversion.Role, conversion.Ordinal, node(ref))
		}
		if cast := retained.Cast; cast != nil {
			r.conversions[ref] = makeConversionWithClass(h, records, cast.Header, cast.Source, cast.Destination, compatibleExplicit, 0, node(ref))
		}
		if call := retained.Call; call != nil {
			arguments := make([]ConversionResult, len(call.Arguments))
			for i, argument := range call.Arguments {
				arguments[i] = makeConversion(h, records, call.Header, argument.Source, argument.Destination, compatibilityArgument, argument.Ordinal, 0)
			}
			context := contextActionNone
			if flow := contextAt(all, ref); flow != nil {
				context = contextAction(flow.Kind)
			}
			r.calls[ref] = CallResult{ref, span, call.Target.Kind, call.Target.Symbol, call.Target.Convention, call.Target.Variadic, arguments, context, node(ref)}
		}
		if member := retained.Member; member != nil {
			r.members[ref] = MemberResult{ref, span, member.Kind, memberOwner(h, records, member), member.Member, member.TupleOrdinal, node(ref)}
		}
		if control := retained.Control; control != nil {
			r.controls[ref] = makeControlResult(h, control, node(ref), deferNodes(h, unit, control))
		}
	}
	return r
}

func contextAt(all []retainedRecord, ref symbol.SyntaxRef) *contextFlowRecord {
	for i := range all {
		if all[i].Header.Syntax == ref && all[i].ContextFlow != nil {
			return all[i].ContextFlow
		}
	}
	return nil
}

// memberOwner resolves the declaring type symbol of a member access's receiver.
// For a base-bearing member (field, tuple component, method), Owner is the
// nominal declaration of the solved base value's type. A static or variant
// member has no base, so Owner is the resolved member symbol's containing
// declaration. Tuple receivers are not nominal, so they yield no Owner; that is
// inherent to the source data, not an omission.
func memberOwner(h *solveHandoff, records *solvedRecords, member *memberRecord) symbol.SymbolID {
	if h == nil || h.Semantics == nil || h.Semantics.Types() == nil || records == nil || member == nil {
		return 0
	}
	if member.Base != 0 {
		result, ok := records.Root(member.Base)
		if !ok || result.State != infer.TypeFinal {
			return 0
		}
		key, ok := h.Semantics.Types().Key(result.Type)
		if !ok {
			return 0
		}
		declaration, _, nominal := key.Nominal()
		if !nominal {
			return 0
		}
		return declaration
	}
	if member.Member != 0 {
		if selected, ok := h.Semantics.Resolution().Symbols.Symbol(member.Member); ok {
			return selected.Containing
		}
	}
	return 0
}

func makeConversion(h *solveHandoff, records *solvedRecords, header recordHeader, source, destination valueID, role compatibilityRole, ordinal uint32, node tir.NodeID) ConversionResult {
	class := compatibleForbidden
	if records != nil && h != nil {
		if s, sok := records.Root(source); sok {
			if d, dok := records.Root(destination); dok {
				class = classify(h.Semantics, s.Type, d.Type)
			}
		}
	}
	return makeConversionWithClass(h, records, header, source, destination, class, role, node).withOrdinal(ordinal)
}

func makeConversionWithClass(h *solveHandoff, records *solvedRecords, header recordHeader, source, destination valueID, class compatibilityClass, role compatibilityRole, node tir.NodeID) ConversionResult {
	var sourceType, destinationType types.TypeID
	if records != nil {
		if result, ok := records.Root(source); ok {
			sourceType = result.Type
		}
		if result, ok := records.Root(destination); ok {
			destinationType = result.Type
		}
	}
	coercion := coercionNone
	if h != nil {
		coercion = coercionFor(h.Semantics, class, sourceType, destinationType)
	}
	return ConversionResult{header.Syntax, header.Span, sourceType, destinationType, class, coercion, role, 0, node}
}

func (r ConversionResult) withOrdinal(ordinal uint32) ConversionResult { r.Ordinal = ordinal; return r }

func makeControlResult(h *solveHandoff, control *controlRecord, node tir.NodeID, defers []tir.NodeID) ControlResult {
	result := ControlResult{Syntax: control.Header.Syntax, Span: control.Header.Span, Reachable: true, Target: control.Target, Node: node}
	result.Defers = defers
	switch control.Kind {
	case controlReturn:
		result.Exits = []ExitResult{{Kind: exitReturn}}
	case controlBreak:
		result.Exits = []ExitResult{{Kind: exitBreak, Target: control.Target}}
	case controlContinue:
		result.Exits = []ExitResult{{Kind: exitContinue, Target: control.Target}}
	default:
		result.Exits = []ExitResult{{Kind: exitFallthrough}}
	}
	return result
}

func deferNodes(h *solveHandoff, unit *tir.Unit, control *controlRecord) []tir.NodeID {
	if h == nil || unit == nil || control == nil {
		return nil
	}
	regions := h.Records.Controls()
	byRegion := make(map[controlID][]deferRecord)
	for _, record := range h.Records.Records() {
		if record.Defer != nil && activeOperatorRecord(h, record.Header) {
			byRegion[record.Defer.Region] = append(byRegion[record.Defer.Region], *record.Defer)
		}
	}
	stop := control.Target
	if control.Kind != controlBreak && control.Kind != controlContinue {
		stop = 0
	}
	var result []tir.NodeID
	for region := control.Region; region != 0 && region != stop; {
		entries := byRegion[region]
		for i := len(entries) - 1; i >= 0; i-- {
			node, ok := unit.SourceMap(entries[i].Statement)
			if ok {
				result = append(result, node)
			}
		}
		if uint64(region) > uint64(len(regions)) {
			break
		}
		region = regions[region-1].Parent
	}
	return result
}
