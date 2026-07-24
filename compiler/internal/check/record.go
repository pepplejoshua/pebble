package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

type recordHeader struct {
	ID          recordID
	Syntax      symbol.SyntaxRef
	Span        source.Span
	Owner       symbol.SymbolID
	Alternative alternativeTag
	Suppressed  bool
}

// retainedRecord is the lifecycle arena entry used by this slice. Later fact
// slices attach their closed record payloads while retaining this header and
// component accounting contract.
type retainedRecord struct {
	Header              recordHeader
	Values              []valueID
	Controls            []controlID
	Binding             *bindingRecord
	Callable            *callableRecord
	TypeUse             *typeUseRecord
	ContextFlow         *contextFlowRecord
	UnsupportedCallable *unsupportedCallableRecord
	Expression          *expressionRecord
	Aggregate           *aggregateRecord
	Compatibility       *compatibilityRecord
}

func cloneRetainedRecord(value retainedRecord) retainedRecord {
	value.Values = append([]valueID(nil), value.Values...)
	value.Controls = append([]controlID(nil), value.Controls...)
	if value.Binding != nil {
		copy := *value.Binding
		value.Binding = &copy
	}
	if value.Callable != nil {
		copy := *value.Callable
		copy.Parameters = append([]valueID(nil), value.Callable.Parameters...)
		copy.Captures = append([]symbol.SymbolID(nil), value.Callable.Captures...)
		value.Callable = &copy
	}
	if value.TypeUse != nil {
		copy := *value.TypeUse
		value.TypeUse = &copy
	}
	if value.ContextFlow != nil {
		copy := *value.ContextFlow
		value.ContextFlow = &copy
	}
	if value.UnsupportedCallable != nil {
		copy := *value.UnsupportedCallable
		copy.TypeParameters = append([]symbol.SyntaxRef(nil), value.UnsupportedCallable.TypeParameters...)
		value.UnsupportedCallable = &copy
	}
	if value.Expression != nil {
		copy := cloneExpressionRecord(*value.Expression)
		value.Expression = &copy
	}
	if value.Aggregate != nil {
		copy := cloneAggregateRecord(*value.Aggregate)
		value.Aggregate = &copy
	}
	if value.Compatibility != nil {
		copy := *value.Compatibility
		value.Compatibility = &copy
	}
	return value
}

func (value *retainedRecord) assignHeader(header recordHeader) {
	value.Header = header
	if value.Binding != nil {
		value.Binding.Header = header
	}
	if value.Callable != nil {
		value.Callable.Header = header
	}
	if value.TypeUse != nil {
		value.TypeUse.Header = header
	}
	if value.ContextFlow != nil {
		value.ContextFlow.Header = header
	}
	if value.UnsupportedCallable != nil {
		value.UnsupportedCallable.Header = header
	}
	if value.Expression != nil {
		value.Expression.Header = header
	}
	if value.Aggregate != nil {
		value.Aggregate.Header = header
	}
	if value.Compatibility != nil {
		value.Compatibility.Header = header
	}
}

func (value retainedRecord) payloadResources() ([]valueID, uint64, bool) {
	payloads := 0
	values := append([]valueID(nil), value.Values...)
	components := uint64(len(value.Values)) + uint64(len(value.Controls))
	add := func(ids ...valueID) {
		for _, id := range ids {
			if id != 0 {
				values = append(values, id)
				components++
			}
		}
	}
	if value.Binding != nil {
		payloads++
		if value.Binding.Header != value.Header || value.Binding.Symbol == 0 || value.Binding.Kind < bindingLocalLet || value.Binding.Kind > bindingRangeIterator || value.Binding.AnnotationPresent != (value.Binding.Annotation != 0) || value.Binding.InitializerPresent != (value.Binding.Initializer != 0) {
			return nil, 0, false
		}
		add(value.Binding.Annotation, value.Binding.Initializer)
	}
	if value.Callable != nil {
		payloads++
		if value.Callable.Header != value.Header || value.Callable.Kind < callableNamed || value.Callable.Kind > callableLiteral || value.Callable.Result == 0 || (value.Callable.Kind == callableLiteral) != (value.Callable.Expression != 0) || (value.Callable.Kind == callableLiteral) != (value.Callable.Symbol == 0) {
			return nil, 0, false
		}
		add(value.Callable.Expression, value.Callable.Result)
		for _, id := range value.Callable.Parameters {
			if id == 0 {
				return nil, 0, false
			}
			add(id)
		}
		components += uint64(len(value.Callable.Captures))
	}
	if value.TypeUse != nil {
		payloads++
		if value.TypeUse.Header != value.Header || value.TypeUse.Kind < typeUseAnnotation || value.TypeUse.Kind > typeUseExplicitArgument || value.TypeUse.Type == 0 {
			return nil, 0, false
		}
		add(value.TypeUse.Type)
	}
	if value.ContextFlow != nil {
		payloads++
		zeroSuppressedExpression := value.ContextFlow.Kind == contextExpression && value.ContextFlow.Header.Suppressed && value.ContextFlow.Context == 0 && value.ContextFlow.Callee == 0
		if value.ContextFlow.Header != value.Header || value.ContextFlow.Kind < contextExpression || value.ContextFlow.Kind > contextIndirect || (value.ContextFlow.Context == 0 && !zeroSuppressedExpression) {
			return nil, 0, false
		}
		add(value.ContextFlow.Callee)
	}
	if value.UnsupportedCallable != nil {
		payloads++
		if value.UnsupportedCallable.Header != value.Header || len(value.UnsupportedCallable.TypeParameters) == 0 {
			return nil, 0, false
		}
		components += uint64(len(value.UnsupportedCallable.TypeParameters))
	}
	if value.Expression != nil {
		payloads++
		expression := value.Expression
		if expression.Header != value.Header || expression.Kind < expressionName || expression.Kind > expressionMember || expression.Result == 0 {
			return nil, 0, false
		}
		if expression.Kind == expressionLiteral {
			if expression.Literal.Kind < literalInteger || expression.Literal.Kind > literalNone {
				return nil, 0, false
			}
		} else if expression.Literal.Kind != 0 || len(expression.Literal.NumericBytes) != 0 || expression.Literal.Bool || expression.Literal.Rune != 0 || expression.Literal.Text != "" {
			return nil, 0, false
		}
		if (expression.Literal.Kind == literalInteger || expression.Literal.Kind == literalFloat) != (len(expression.Literal.NumericBytes) != 0) {
			return nil, 0, false
		}
		if expression.Kind != expressionInterpolated && len(expression.Parts) != 0 {
			return nil, 0, false
		}
		add(expression.Result)
		for _, id := range expression.Children {
			if id == 0 {
				return nil, 0, false
			}
			add(id)
		}
		components += uint64(len(expression.Literal.NumericBytes)) + uint64(len(expression.Parts))
		for _, part := range expression.Parts {
			if part.Kind < interpolationText || part.Kind > interpolationValue || (part.Kind == interpolationValue) != (part.Value != 0) {
				return nil, 0, false
			}
			add(part.Value)
		}
	}
	if value.Aggregate != nil {
		payloads++
		aggregate := value.Aggregate
		if aggregate.Header != value.Header || aggregate.Kind < aggregateStruct || aggregate.Kind > aggregateTaggedVariant || aggregate.Result == 0 {
			return nil, 0, false
		}
		add(aggregate.Result, aggregate.Receiver)
		for _, field := range aggregate.Fields {
			if field.Field == (symbol.SyntaxRef{}) || field.NameSyntax == (symbol.SyntaxRef{}) || field.Value == 0 || field.Destination == 0 {
				return nil, 0, false
			}
			add(field.Value, field.Destination)
		}
		components += uint64(len(aggregate.Fields)) + uint64(len(aggregate.DeclarationFields))
	}
	if value.Compatibility != nil {
		payloads++
		compatibility := value.Compatibility
		if compatibility.Header != value.Header || compatibility.Source == 0 || compatibility.Destination == 0 || compatibility.Role < compatibilityAssignment || compatibility.Role > compatibilityBranch {
			return nil, 0, false
		}
		add(compatibility.Source, compatibility.Destination)
	}
	if payloads > 1 {
		return nil, 0, false
	}
	return values, components, true
}

type recordArena struct {
	values     []retainedRecord
	components uint64
}

func (a *recordArena) append(value retainedRecord, validValue func(valueID) bool, validControl func(controlID) bool, maxRecords, maxComponents uint32) (recordID, bool) {
	if a == nil || value.Header.ID != 0 || !value.Header.Alternative.valid() || uint64(len(a.values)) >= uint64(maxRecords) {
		return 0, false
	}
	values, components, validPayload := value.payloadResources()
	if !validPayload {
		return 0, false
	}
	if components > uint64(maxComponents) || a.components > uint64(maxComponents)-components {
		return 0, false
	}
	for _, id := range values {
		if !validValue(id) {
			return 0, false
		}
	}
	for _, id := range value.Controls {
		if !validControl(id) {
			return 0, false
		}
	}

	id := recordID(len(a.values) + 1)
	header := value.Header
	header.ID = id
	value.assignHeader(header)
	value = cloneRetainedRecord(value)
	a.values = append(a.values, value)
	a.components += components
	return id, true
}

func (a *recordArena) record(id recordID) (retainedRecord, bool) {
	if a == nil || !id.valid() || uint64(id) > uint64(len(a.values)) {
		return retainedRecord{}, false
	}
	return cloneRetainedRecord(a.values[id-1]), true
}

type controlRegion struct {
	ID       controlID
	Parent   controlID
	Depth    uint32
	Children []controlID
}

func cloneControlRegion(value controlRegion) controlRegion {
	value.Children = append([]controlID(nil), value.Children...)
	return value
}

type controlArena struct {
	values []mutableControlRegion
}

type mutableControlRegion struct {
	ID     controlID
	Parent controlID
	Depth  uint32
}

func (a *controlArena) append(parent controlID, maxDepth, maxRegions uint32) (controlID, bool) {
	if a == nil || uint64(len(a.values)) >= uint64(maxRegions) || uint64(len(a.values)) >= uint64(^uint32(0)) {
		return 0, false
	}

	depth := uint32(1)
	if parent != 0 {
		if uint64(parent) > uint64(len(a.values)) {
			return 0, false
		}
		parentRegion := a.values[parent-1]
		if parentRegion.ID != parent || parentRegion.Depth == 0 || parentRegion.Depth == ^uint32(0) {
			return 0, false
		}
		depth = parentRegion.Depth + 1
	}
	if depth > maxDepth {
		return 0, false
	}

	id := controlID(len(a.values) + 1)
	a.values = append(a.values, mutableControlRegion{ID: id, Parent: parent, Depth: depth})
	return id, true
}

func (a *controlArena) region(id controlID) (mutableControlRegion, bool) {
	if a == nil || !id.valid() || uint64(id) > uint64(len(a.values)) {
		return mutableControlRegion{}, false
	}
	return a.values[id-1], true
}

// freeze materializes ordered child lists in linear work. The first pass
// validates regions and counts one edge for every non-root. Exact child slices
// are then allocated, and the second pass fills them in control-ID order.
func (a *controlArena) freeze(maxRegions uint32) ([]controlRegion, bool) {
	if a == nil || uint64(len(a.values)) > uint64(maxRegions) || uint64(len(a.values)) > uint64(^uint32(0)) {
		return nil, false
	}
	counts := make([]uint32, len(a.values))
	for index, value := range a.values {
		if value.ID != controlID(index+1) || value.Depth == 0 {
			return nil, false
		}
		if value.Parent == 0 {
			if value.Depth != 1 {
				return nil, false
			}
			continue
		}
		if uint64(value.Parent) > uint64(index) {
			return nil, false
		}
		parent := a.values[value.Parent-1]
		if parent.Depth == ^uint32(0) || value.Depth != parent.Depth+1 || counts[value.Parent-1] == ^uint32(0) {
			return nil, false
		}
		counts[value.Parent-1]++
	}

	result := make([]controlRegion, len(a.values))
	for index, value := range a.values {
		result[index] = controlRegion{ID: value.ID, Parent: value.Parent, Depth: value.Depth}
		if uint64(counts[index]) > uint64(int(^uint(0)>>1)) {
			return nil, false
		}
		result[index].Children = make([]controlID, int(counts[index]))
	}
	next := make([]uint32, len(a.values))
	for _, value := range a.values {
		if value.Parent == 0 {
			continue
		}
		parentIndex := value.Parent - 1
		result[parentIndex].Children[next[parentIndex]] = value.ID
		next[parentIndex]++
	}
	return result, true
}

type frozenRecords struct {
	values     []retainedRecord
	controls   []controlRegion
	components uint64
}

func (f frozenRecords) Records() []retainedRecord {
	result := make([]retainedRecord, len(f.values))
	for index := range f.values {
		result[index] = cloneRetainedRecord(f.values[index])
	}
	return result
}

func (f frozenRecords) Controls() []controlRegion {
	result := make([]controlRegion, len(f.controls))
	for index := range f.controls {
		result[index] = cloneControlRegion(f.controls[index])
	}
	return result
}

func (f frozenRecords) Components() uint64 { return f.components }
