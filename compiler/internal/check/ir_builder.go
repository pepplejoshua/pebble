package check

import (
	"math/big"
	"sort"
	"strconv"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildUnit constructs the declaration/nonvalue portion of typed IR. It is
// intentionally not called by run06b yet: later 06b.7b parts add values,
// places, calls, coercions, and statements at this orchestration point.
func buildUnit(handoff *solveHandoff, records *solvedRecords, requirements map[symbol.SymbolID][]Requirement, config Config) (*tir.Unit, bool) {
	if handoff == nil || handoff.GenerationHadErrors || handoff.Semantics == nil || handoff.Solution == nil || records == nil {
		return nil, false
	}
	b := tir.NewBuilder(handoff.Semantics.Types(), tir.Config{
		MaxIRNodes: config.MaxIRNodes, MaxIRComponents: config.MaxIRComponents,
		MaxDumpBytes: config.MaxDumpBytes,
	})
	state := &irBuildState{handoff: handoff, records: records, builder: b}
	if !state.buildModules() || !state.buildTypes() || !state.buildDeclarations() || !state.buildTypeUses() || !state.indexExpressions() || !state.buildBlocks() || !state.buildRequirements(requirements) {
		return nil, false
	}
	unit, err := b.Build()
	if err != nil {
		panic(err)
	}
	return unit, true
}

type irBuildState struct {
	handoff              *solveHandoff
	records              *solvedRecords
	builder              *tir.Builder
	functions            map[symbol.SymbolID]tir.FunctionID
	functionNodes        map[symbol.SymbolID]tir.NodeID
	regions              map[controlID]tir.RegionID
	values               map[valueID]tir.NodeID
	places               map[symbol.SyntaxRef]*placeRecord
	placeValues          map[valueID]tir.NodeID
	expressionsByResult  map[valueID]*expressionRecord
	aggregatesByRecord   map[recordID]*aggregateRecord
	operatorsBySyntax    map[symbol.SyntaxRef]*operatorRecord
	operatorsByResult    map[valueID]*operatorRecord
	membersByResult      map[valueID]*memberRecord
	indexesByResult      map[valueID]*indexRecord
	indexesBySyntax      map[symbol.SyntaxRef]*indexRecord
	callsBySyntax        map[symbol.SyntaxRef]*callRecord
	contextFlowsBySyntax map[symbol.SyntaxRef]*contextFlowRecord
}

func (s *irBuildState) addNode(node tir.Node, ref symbol.SyntaxRef) (tir.NodeID, bool) {
	if node.Syntax == (symbol.SyntaxRef{}) {
		node.Syntax = ref
	}
	id, err := s.builder.AddNode(node)
	if err != nil {
		return 0, false
	}
	if ref != (symbol.SyntaxRef{}) {
		if err := s.builder.MapSource(ref, id); err != nil {
			return 0, false
		}
	}
	return id, true
}

func (s *irBuildState) buildModules() bool {
	byID := make(map[module.ModuleID]frozenModule, len(s.handoff.Compilation.Modules))
	for _, m := range s.handoff.Compilation.Modules {
		byID[m.ID] = m
	}
	for _, id := range s.handoff.Compilation.DependencyOrder {
		m, ok := byID[id]
		if !ok {
			return false
		}
		imports := make([]tir.ImportDecl, len(m.Imports))
		for i, imp := range m.Imports {
			imports[i] = tir.ImportDecl{Span: imp.Span, Target: imp.Target}
			if _, ok := s.addNode(tir.Node{Kind: tir.Import, Span: imp.Span, TargetModule: imp.Target}, symbol.SyntaxRef{}); !ok {
				return false
			}
		}
		if _, ok := s.addNode(tir.Node{Kind: tir.Module, Span: m.Span, Symbol: symbol.SymbolID(m.ID)}, symbol.SyntaxRef{}); !ok {
			return false
		}
		if err := s.builder.AddModule(tir.ModuleDecl{ID: m.ID, Key: m.Key, Source: m.Source, Span: m.Span, Imports: imports, Declarations: m.Declarations}); err != nil {
			return false
		}
	}
	return true
}

func (s *irBuildState) symbol(id symbol.SymbolID) (symbol.Symbol, bool) {
	r := s.handoff.Semantics.Resolution()
	if r == nil || r.Symbols == nil {
		return symbol.Symbol{}, false
	}
	return r.Symbols.Symbol(id)
}

func (s *irBuildState) buildTypes() bool {
	for _, m := range s.handoff.Compilation.Modules {
		for _, id := range m.Declarations {
			sym, ok := s.symbol(id)
			if !ok || (sym.Kind != symbol.SymbolType && sym.Kind != symbol.SymbolExternType) {
				continue
			}
			d, ok := s.handoff.Semantics.TypeDeclaration(id)
			if !ok {
				return false
			}
			nodeID, ok := s.addNode(tir.Node{Kind: tir.TypeDeclaration, Span: sym.Span, Symbol: id}, sym.Declaration)
			if !ok {
				return false
			}
			members := make([]symbol.SymbolID, 0, len(d.Members))
			for _, member := range d.Members {
				members = append(members, member.Symbol)
				ms, exists := s.symbol(member.Symbol)
				if !exists {
					return false
				}
				kind := tir.FieldDeclaration
				if ms.Kind == symbol.SymbolVariant {
					kind = tir.VariantDeclaration
				}
				if _, ok := s.addNode(tir.Node{Kind: kind, Span: ms.Span, Symbol: member.Symbol}, ms.Declaration); !ok {
					return false
				}
			}
			if err := s.builder.AddTypeDecl(tir.TypeDecl{Symbol: id, Span: sym.Span, Members: members, Node: nodeID}); err != nil {
				return false
			}
			for _, parameter := range d.Parameters {
				ps, exists := s.symbol(parameter)
				if !exists {
					return false
				}
				if _, ok := s.addNode(tir.Node{Kind: tir.TypeParameterDeclaration, Span: ps.Span, Symbol: parameter}, ps.Declaration); !ok {
					return false
				}
			}
		}
	}
	return true
}

func typeOfValue(records *solvedRecords, id valueID) (types.TypeID, bool) {
	if id == 0 {
		return 0, false
	}
	r, ok := records.Root(id)
	return r.Type, ok && r.State == infer.TypeFinal && r.Type != 0
}

func (s *irBuildState) buildDeclarations() bool {
	s.functions = make(map[symbol.SymbolID]tir.FunctionID)
	s.functionNodes = make(map[symbol.SymbolID]tir.NodeID)
	for _, retained := range s.handoff.Records.Records() {
		if retained.Callable != nil && retained.Callable.Kind != callableLiteral {
			c := retained.Callable
			sym, ok := s.symbol(c.Symbol)
			if !ok {
				return false
			}
			params := make([]tir.Parameter, len(c.Parameters))
			for i, value := range c.Parameters {
				typ, ok := typeOfValue(s.records, value)
				if !ok {
					return false
				}
				ps, exists := s.symbolForParameter(c.Symbol, i)
				if !exists {
					return false
				}
				params[i] = tir.Parameter{Symbol: ps.ID, Type: typ}
				if _, ok := s.addNode(tir.Node{Kind: tir.ParameterDeclaration, Span: ps.Span, Symbol: ps.ID}, ps.Declaration); !ok {
					return false
				}
			}
			result, ok := typeOfValue(s.records, c.Result)
			if !ok {
				return false
			}
			kind := tir.FunctionDeclaration
			if c.Kind == callableExtern {
				kind = tir.ExternDeclaration
			}
			fid, err := s.builder.AddFunctionDecl(tir.FunctionDecl{Symbol: c.Symbol, Span: sym.Span})
			if err != nil {
				return false
			}
			node, ok := s.addNode(tir.Node{Kind: kind, Span: sym.Span, Syntax: c.Header.Syntax, Symbol: c.Symbol, Function: fid, Parameters: params, ResultType: result, Convention: c.Convention, Variadic: c.Variadic, Inline: c.Inline, HasBody: c.BodyPresent}, c.Header.Syntax)
			if !ok {
				return false
			}
			s.functions[c.Symbol], s.functionNodes[c.Symbol] = fid, node
		}
		if retained.Binding != nil {
			b := retained.Binding
			if b.Kind == bindingParameter || b.Kind == bindingRangeIterator {
				continue
			}
			sym, ok := s.symbol(b.Symbol)
			if !ok {
				return false
			}
			typ, ok := typeOfValue(s.records, b.Annotation)
			if !ok && b.InitializerPresent {
				typ, ok = typeOfValue(s.records, b.Initializer)
			}
			if !ok {
				return false
			}
			switch b.Kind {
			case bindingGlobalLet, bindingGlobalVar:
				node, ok := s.addNode(tir.Node{Kind: tir.GlobalDeclaration, Span: sym.Span, Symbol: b.Symbol}, b.Header.Syntax)
				if !ok {
					return false
				}
				if err := s.builder.AddGlobalDecl(tir.GlobalDecl{Symbol: b.Symbol, Span: sym.Span, Type: typ, Node: node}); err != nil {
					return false
				}
			case bindingLocalLet, bindingLocalVar:
				if _, ok := s.addNode(tir.Node{Kind: tir.LocalDeclaration, Span: sym.Span, Symbol: b.Symbol}, b.Header.Syntax); !ok {
					return false
				}
			case bindingExternLet, bindingExternVar:
				if _, ok := s.addNode(tir.Node{Kind: tir.ExternDeclaration, Span: sym.Span, Symbol: b.Symbol}, b.Header.Syntax); !ok {
					return false
				}
			}
		}
	}
	return true
}

func (s *irBuildState) symbolForParameter(owner symbol.SymbolID, ordinal int) (symbol.Symbol, bool) {
	sig, ok := s.handoff.Semantics.Signature(owner)
	if !ok || ordinal >= len(sig.Parameters) {
		return symbol.Symbol{}, false
	}
	return s.symbol(sig.Parameters[ordinal])
}

func (s *irBuildState) buildTypeUses() bool {
	for _, retained := range s.handoff.Records.Records() {
		if retained.TypeUse == nil {
			continue
		}
		typ, ok := typeOfValue(s.records, retained.TypeUse.Type)
		if !ok {
			return false
		}
		if _, ok := s.addNode(tir.Node{Kind: tir.TypeUse, Span: retained.Header.Span, Syntax: retained.Header.Syntax, TypeArg: typ}, retained.Header.Syntax); !ok {
			return false
		}
	}
	return true
}

// buildBlocks reserves the lexical RegionIDs needed by later statement
// construction. It intentionally builds no Block nodes yet; the next part
// will populate those nodes once their statement children exist.
func (s *irBuildState) buildBlocks() bool {
	controls := s.handoff.Records.Controls()
	s.regions = make(map[controlID]tir.RegionID, len(controls))
	for i := range controls {
		r, err := s.builder.AddRegion()
		if err != nil {
			return false
		}
		s.regions[controls[i].ID] = r
	}
	for _, c := range controls {
		if c.ID == 0 || s.regions[c.ID] == 0 {
			return false
		}
	}
	return true
}

func (s *irBuildState) buildRequirements(groups map[symbol.SymbolID][]Requirement) bool {
	for _, rs := range groups {
		for _, r := range rs {
			kind := tir.RequirementKind(r.Kind)
			if r.Kind == RequirementLiteralFits {
				kind = tir.RequirementLiteralFits
			}
			literalKind := tir.LiteralKind(0)
			switch r.LiteralKind {
			case infer.ExactInteger:
				literalKind = tir.LiteralInteger
			case infer.ExactFloat:
				literalKind = tir.LiteralFloat
			}
			if err := s.builder.AddRequirement(tir.Requirement{Owner: r.Owner, Parameter: r.Parameter, Kind: kind, Subject: r.Subject, Origin: r.Origin, Operator: r.Operator, LiteralKind: literalKind, Numerator: r.Numerator, Denominator: r.Denominator}); err != nil {
				return false
			}
		}
	}
	return true
}

func (s *irBuildState) indexExpressions() bool {
	s.expressionsByResult = make(map[valueID]*expressionRecord)
	s.aggregatesByRecord = make(map[recordID]*aggregateRecord)
	s.operatorsBySyntax = make(map[symbol.SyntaxRef]*operatorRecord)
	s.operatorsByResult = make(map[valueID]*operatorRecord)
	s.membersByResult = make(map[valueID]*memberRecord)
	s.indexesByResult = make(map[valueID]*indexRecord)
	s.indexesBySyntax = make(map[symbol.SyntaxRef]*indexRecord)
	s.callsBySyntax = make(map[symbol.SyntaxRef]*callRecord)
	s.contextFlowsBySyntax = make(map[symbol.SyntaxRef]*contextFlowRecord)
	s.places = make(map[symbol.SyntaxRef]*placeRecord)
	for _, retained := range s.handoff.Records.Records() {
		if !activeOperatorRecord(s.handoff, retained.Header) {
			continue
		}
		if retained.Expression != nil {
			s.expressionsByResult[retained.Expression.Result] = retained.Expression
		}
		if retained.Aggregate != nil {
			s.aggregatesByRecord[retained.Aggregate.Header.ID] = retained.Aggregate
		}
		if retained.Operator != nil {
			s.operatorsBySyntax[retained.Operator.Header.Syntax] = retained.Operator
			s.operatorsByResult[retained.Operator.Result] = retained.Operator
		}
		if retained.Member != nil {
			s.membersByResult[retained.Member.Result] = retained.Member
		}
		if retained.Index != nil {
			s.indexesByResult[retained.Index.Result] = retained.Index
			s.indexesBySyntax[retained.Index.Header.Syntax] = retained.Index
		}
		if retained.Call != nil {
			s.callsBySyntax[retained.Call.Header.Syntax] = retained.Call
		}
		if retained.ContextFlow != nil {
			s.contextFlowsBySyntax[retained.ContextFlow.Header.Syntax] = retained.ContextFlow
		}
		if retained.Place != nil {
			s.places[retained.Place.Header.Syntax] = retained.Place
		}
	}
	return true
}

// buildValue is the single shared, recursive, memoized dispatcher for typed-IR
// value construction. It builds children before parents and memoizes every
// valueID so a value referenced by multiple parents is only built once.
func (s *irBuildState) buildValue(id valueID) (tir.NodeID, bool) {
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
	typ, ok := typeOfValue(s.records, id)
	if !ok {
		return 0, false
	}
	node := tir.Node{Type: typ, Span: record.Header.Span, Syntax: record.Header.Syntax}
	switch record.Kind {
	case expressionLiteral:
		if !s.buildLiteral(record, &node) {
			return 0, false
		}
	case expressionName, expressionPath:
		if !s.buildSymbolValue(record, &node) {
			return 0, false
		}
	case expressionMember:
		if member := s.membersByResult[id]; member != nil && (member.Kind == memberField || member.Kind == memberTuple) {
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
				if member.Kind == memberField {
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
	case expressionTuple:
		node.Kind = tir.TupleValue
		if !s.buildChildren(record, &node) {
			return 0, false
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
		if !s.buildRecordConstruct(record, &node) {
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
	case expressionInterpolated:
		node.Kind = tir.InterpolatedString
		if !s.buildInterpolated(record, &node) {
			return 0, false
		}
	case expressionPrefix, expressionBinary:
		op := s.operatorsByResult[id]
		if op == nil {
			op = s.operatorsBySyntax[record.Header.Syntax]
		}
		if op != nil && op.Family == operatorDereference {
			if place, ok := s.buildPlaceForValue(id); ok {
				node.Kind, node.Children = tir.Load, []tir.NodeID{place}
			} else {
				return 0, false
			}
		} else if !s.buildOperatorValue(record, &node) {
			return 0, false
		}
	case expressionBracket:
		if place, ok := s.buildPlaceForValue(id); ok {
			node.Kind, node.Children = tir.Load, []tir.NodeID{place}
		} else {
			return 0, false
		}
	default:
		return 0, false
	}
	if node.Kind == 0 {
		return 0, false
	}
	nid, ok := s.addNode(node, record.Header.Syntax)
	if !ok {
		return 0, false
	}
	if s.values == nil {
		s.values = make(map[valueID]tir.NodeID)
	}
	s.values[id] = nid
	return nid, true
}

// buildPlace folds a retained place record's complete projection chain. Place
// nodes are internal structure; the authored source maps to the value Load or
// to the place's eventual statement owner in later statement construction.
func (s *irBuildState) buildPlace(ref symbol.SyntaxRef) (tir.NodeID, bool) {
	record, ok := s.places[ref]
	if !ok || record == nil || len(record.Projections) == 0 {
		return 0, false
	}
	rootType, ok := s.handoff.Solution.SymbolType(record.Root)
	if !ok || rootType.State != infer.TypeFinal || rootType.Type == 0 {
		return 0, false
	}
	finalType := rootType.Type
	if assignment := s.assignmentPlace(record.Header.Syntax); assignment != 0 {
		if typ, found := typeOfValue(s.records, assignment); found {
			finalType = typ
		}
	}
	var current tir.NodeID
	for i, projection := range record.Projections {
		typ := rootType.Type
		if i > 0 {
			if i+1 < len(record.Projections) {
				typ, ok = typeOfValue(s.records, record.Projections[i+1].Base)
			} else {
				typ = finalType
				ok = typ != 0
			}
			if !ok {
				return 0, false
			}
		}
		n := tir.Node{Type: typ, Writable: record.RootMutable, Span: record.Header.Span}
		switch projection.Kind {
		case placeStorage:
			n.Kind, n.Symbol = tir.StoragePlace, record.Root
		case placeDereference:
			child, childOK := s.buildValue(projection.Base)
			if !childOK {
				return 0, false
			}
			n.Kind, n.Children = tir.DereferencePlace, []tir.NodeID{child}
		case placeField:
			memberID := projection.Member
			if memberID == 0 {
				var result valueID
				if i+1 < len(record.Projections) {
					result = record.Projections[i+1].Base
				} else {
					result = s.assignmentPlace(record.Header.Syntax)
				}
				member := s.membersByResult[result]
				if member == nil || member.Base != projection.Base {
					return 0, false
				}
				memberID = member.Member
				if memberID == 0 {
					memberID = s.memberSymbol(projection.Base, member.Name)
				}
			}
			if memberID == 0 {
				return 0, false
			}
			n.Kind, n.Member, n.Children = tir.FieldPlace, memberID, []tir.NodeID{current}
		case placeTuple:
			n.Kind, n.Ordinal, n.Children = tir.TuplePlace, projection.TupleOrdinal, []tir.NodeID{current}
		case placeIndex:
			child, childOK := s.buildValue(projection.Index)
			if !childOK {
				return 0, false
			}
			n.Kind, n.Children = tir.CheckedIndexPlace, []tir.NodeID{current, child}
		default:
			return 0, false
		}
		var added bool
		current, added = s.addNode(n, symbol.SyntaxRef{})
		if !added {
			return 0, false
		}
	}
	return current, true
}

func (s *irBuildState) assignmentPlace(ref symbol.SyntaxRef) valueID {
	for _, retained := range s.handoff.Records.Records() {
		if retained.Assignment != nil && retained.Assignment.Statement == ref {
			return retained.Assignment.Place
		}
	}
	return 0
}

// buildPlaceForValue derives the same chain used by 06a for an expression
// result. It is needed because ordinary reads do not retain a place record.
func (s *irBuildState) buildPlaceForValue(id valueID) (tir.NodeID, bool) {
	if s.placeValues == nil {
		s.placeValues = make(map[valueID]tir.NodeID)
	}
	if existing, ok := s.placeValues[id]; ok {
		return existing, true
	}
	record, ok := s.expressionsByResult[id]
	if !ok {
		return 0, false
	}
	typ, ok := typeOfValue(s.records, id)
	if !ok {
		return 0, false
	}
	var root symbol.SymbolID
	var rootType types.TypeID
	var current tir.NodeID
	if record.Kind == expressionName || record.Kind == expressionPath {
		sym, found := s.symbol(record.Symbol)
		if !found {
			return 0, false
		}
		switch sym.Kind {
		case symbol.SymbolBinding, symbol.SymbolParameter, symbol.SymbolLoopBinding, symbol.SymbolExternBinding:
			root = sym.ID
			rootType = typ
		default:
			return 0, false
		}
		n, made := s.addNode(tir.Node{Kind: tir.StoragePlace, Type: rootType, Span: record.Header.Span, Symbol: root, Writable: s.symbolMutable(root)}, symbol.SyntaxRef{})
		if !made {
			return 0, false
		}
		current = n
	} else if index := s.indexForValue(id, record.Header.Syntax); index != nil && index.Mode == indexValue {
		if s.isString(index.Base) {
			return 0, false
		}
		base, found := s.buildPlaceForValue(index.Base)
		if !found {
			return 0, false
		}
		child, found := s.buildValue(index.Start)
		if !found {
			return 0, false
		}
		n, made := s.addNode(tir.Node{Kind: tir.CheckedIndexPlace, Type: typ, Span: record.Header.Span, Writable: s.placeWritableValue(index.Base), Children: []tir.NodeID{base, child}}, symbol.SyntaxRef{})
		if !made {
			return 0, false
		}
		current = n
	} else if len(record.Children) == 1 {
		base, found := s.buildPlaceForValue(record.Children[0])
		if !found {
			return 0, false
		}
		current = base
		writable := s.placeWritableValue(record.Children[0])
		if member := s.membersByResult[id]; member != nil {
			n := tir.Node{Type: typ, Span: record.Header.Span, Writable: writable}
			if member.Kind == memberField {
				n.Kind, n.Member = tir.FieldPlace, member.Member
				if n.Member == 0 {
					n.Member = s.memberSymbol(record.Children[0], member.Name)
				}
			} else if member.Kind == memberTuple {
				n.Kind, n.Ordinal = tir.TuplePlace, member.TupleOrdinal
			} else {
				return 0, false
			}
			n.Children = []tir.NodeID{current}
			made, ok := s.addNode(n, symbol.SyntaxRef{})
			if !ok {
				return 0, false
			}
			current = made
		} else if op := s.operatorForValue(id, record.Header.Syntax); op != nil && op.Family == operatorDereference {
			child, found := s.buildValue(op.Operands[0])
			if !found {
				return 0, false
			}
			n, made := s.addNode(tir.Node{Kind: tir.DereferencePlace, Type: typ, Span: record.Header.Span, Writable: writable, Children: []tir.NodeID{child}}, symbol.SyntaxRef{})
			if !made {
				return 0, false
			}
			current = n
		} else {
			return 0, false
		}
	} else {
		return 0, false
	}
	s.placeValues[id] = current
	return current, true
}

func (s *irBuildState) operatorForValue(id valueID, ref symbol.SyntaxRef) *operatorRecord {
	if op := s.operatorsByResult[id]; op != nil {
		return op
	}
	return s.operatorsBySyntax[ref]
}

func (s *irBuildState) indexForValue(id valueID, ref symbol.SyntaxRef) *indexRecord {
	if index := s.indexesByResult[id]; index != nil {
		return index
	}
	return s.indexesBySyntax[ref]
}

func (s *irBuildState) symbolMutable(id symbol.SymbolID) bool {
	for _, retained := range s.handoff.Records.Records() {
		if retained.Binding != nil && retained.Binding.Symbol == id {
			return retained.Binding.Kind == bindingLocalVar || retained.Binding.Kind == bindingGlobalVar || retained.Binding.Kind == bindingExternVar
		}
		if retained.Place != nil && retained.Place.Root == id {
			return retained.Place.RootMutable
		}
	}
	return false
}

func (s *irBuildState) memberSymbol(base valueID, name string) symbol.SymbolID {
	typ, ok := typeOfValue(s.records, base)
	if !ok {
		return 0
	}
	key, ok := s.handoff.Semantics.Types().Key(typ)
	if !ok {
		return 0
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return 0
	}
	d, ok := s.handoff.Semantics.TypeDeclaration(decl)
	if !ok {
		return 0
	}
	for _, member := range d.Members {
		sym, found := s.symbol(member.Symbol)
		if found && sym.Name == name {
			return member.Symbol
		}
	}
	return 0
}

func (s *irBuildState) placeWritableValue(id valueID) bool {
	record, ok := s.expressionsByResult[id]
	if !ok {
		return false
	}
	if record.Kind == expressionName || record.Kind == expressionPath {
		return s.symbolMutable(record.Symbol)
	}
	if index := s.indexForValue(id, record.Header.Syntax); index != nil {
		return s.placeWritableValue(index.Base)
	}
	if len(record.Children) == 1 {
		return s.placeWritableValue(record.Children[0])
	}
	return false
}

func (s *irBuildState) isString(id valueID) bool {
	typ, ok := typeOfValue(s.records, id)
	if !ok {
		return false
	}
	key, ok := s.handoff.Semantics.Types().Key(typ)
	if !ok || key.Kind() != types.Builtin {
		return false
	}
	builtin, _ := key.Builtin()
	return builtin == types.Str
}

func (s *irBuildState) buildLiteral(record *expressionRecord, node *tir.Node) bool {
	switch record.Literal.Kind {
	case literalBool:
		node.Kind = tir.BoolLiteral
		node.Literal = tir.Literal{Kind: tir.LiteralBool, Bool: record.Literal.Bool}
	case literalChar:
		node.Kind = tir.CharLiteral
		node.Literal = tir.Literal{Kind: tir.LiteralChar, Char: record.Literal.Rune}
	case literalString:
		node.Kind = tir.StringLiteral
		node.Literal = tir.Literal{Kind: tir.LiteralString, String: record.Literal.Text}
	case literalInteger:
		node.Kind = tir.IntegerLiteral
		num, den, ok := decodeIntegerLiteral(record.Literal.NumericBytes)
		if !ok {
			return false
		}
		node.Literal = tir.Literal{Kind: tir.LiteralInteger, IntegerNum: num, IntegerDen: den}
	case literalFloat:
		node.Kind = tir.FloatLiteral
		str, ok := decodeFloatLiteral(record.Literal.NumericBytes)
		if !ok {
			return false
		}
		node.Literal = tir.Literal{Kind: tir.LiteralFloat, Float: str}
	case literalNil:
		node.Kind = tir.NilPointer
	case literalNone:
		node.Kind = tir.NoneOptional
	default:
		return false
	}
	return true
}

func (s *irBuildState) buildSymbolValue(record *expressionRecord, node *tir.Node) bool {
	sym, ok := s.symbol(record.Symbol)
	if !ok {
		return false
	}
	switch sym.Kind {
	case symbol.SymbolBinding, symbol.SymbolParameter, symbol.SymbolLoopBinding, symbol.SymbolExternBinding, symbol.SymbolField:
		node.Kind = tir.SymbolValue
		node.Symbol = record.Symbol
	case symbol.SymbolVariant:
		node.Kind = tir.EnumVariantValue
		node.Member = record.Symbol
	default:
		return false
	}
	return true
}

// buildVariantMember handles a dotted member access that resolves to an enum
// variant with no runtime base (e.g. Color.red). The member machinery records
// these as expressionMember with a variant symbol; the selected variant becomes
// an EnumVariantValue.
func (s *irBuildState) buildVariantMember(record *expressionRecord, node *tir.Node) bool {
	if len(record.Children) != 0 {
		return false
	}
	sym, ok := s.symbol(record.Symbol)
	if !ok || sym.Kind != symbol.SymbolVariant {
		return false
	}
	node.Kind = tir.EnumVariantValue
	node.Member = record.Symbol
	return true
}

func (s *irBuildState) buildSizeof(record *expressionRecord, node *tir.Node) bool {
	argType, ok := typeOfValue(s.records, record.TypeArgument)
	if !ok {
		return false
	}
	node.Kind = tir.SizeofType
	node.TypeArg = argType
	return true
}

func (s *irBuildState) buildChildren(record *expressionRecord, node *tir.Node) bool {
	children := make([]tir.NodeID, 0, len(record.Children))
	for _, childID := range record.Children {
		childNode, ok := s.buildValue(childID)
		if !ok {
			return false
		}
		children = append(children, childNode)
	}
	node.Children = children
	return true
}

func (s *irBuildState) buildArrayRepeat(record *expressionRecord, node *tir.Node) bool {
	if len(record.Children) != 1 {
		return false
	}
	length, ok := s.arrayLength(record.Result)
	if !ok {
		return false
	}
	valueChild, ok := s.buildValue(record.Children[0])
	if !ok {
		return false
	}
	countNode, ok := s.addNode(tir.Node{
		Kind:          tir.IntegerLiteral,
		Type:          s.handoff.Semantics.Types().Builtins().Uint,
		Origin:        record.Header.Span,
		SyntheticRole: "array-repeat-count",
		Literal:       tir.Literal{Kind: tir.LiteralInteger, IntegerNum: strconv.FormatUint(length, 10), IntegerDen: "1"},
	}, symbol.SyntaxRef{})
	if !ok {
		return false
	}
	node.Kind = tir.ArrayRepeat
	node.Children = []tir.NodeID{valueChild, countNode}
	return true
}

func (s *irBuildState) arrayLength(id valueID) (uint64, bool) {
	typ, ok := typeOfValue(s.records, id)
	if !ok {
		return 0, false
	}
	key, ok := s.handoff.Semantics.Types().Key(typ)
	if !ok {
		return 0, false
	}
	length, _, ok := key.Array()
	return length, ok
}

func (s *irBuildState) buildRecordConstruct(record *expressionRecord, node *tir.Node) bool {
	aggregate, ok := s.aggregatesByRecord[record.Specialized]
	if !ok || aggregate == nil || aggregate.Kind != aggregateStruct || aggregate.Declaration == 0 {
		return false
	}
	sorted := make([]fieldValue, len(aggregate.Fields))
	copy(sorted, aggregate.Fields)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i].Ordinal < sorted[j].Ordinal })
	fields := make([]tir.FieldInit, 0, len(sorted))
	for _, fv := range sorted {
		valueNode, ok := s.buildValue(fv.Value)
		if !ok {
			return false
		}
		fields = append(fields, tir.FieldInit{Field: fv.Member, Value: valueNode})
	}
	node.Kind = tir.RecordConstruct
	node.Symbol = aggregate.Declaration
	node.Fields = fields
	return true
}

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
	functionType, ok := typeOfValue(s.records, call.Callee)
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
	functionType, ok := typeOfValue(s.records, call.Callee)
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
		return false
	}
	functionType, ok := typeOfValue(s.records, call.Callee)
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
		valueNode, ok := s.buildValue(argument.Source)
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

func (s *irBuildState) buildInterpolated(record *expressionRecord, node *tir.Node) bool {
	children := make([]tir.NodeID, 0, len(record.Parts))
	for _, part := range record.Parts {
		if part.Kind != interpolationValue {
			continue
		}
		valueNode, ok := s.buildValue(part.Value)
		if !ok {
			return false
		}
		children = append(children, valueNode)
	}
	node.Kind = tir.InterpolatedString
	node.Children = children
	return true
}

func (s *irBuildState) buildOperatorValue(record *expressionRecord, node *tir.Node) bool {
	op, ok := s.operatorsBySyntax[record.Header.Syntax]
	if !ok || op == nil {
		return false
	}
	switch op.Form {
	case operatorPrefix:
		node.Kind = tir.PrefixValue
	case operatorBinary:
		if op.Family == operatorShift {
			node.Kind = tir.CheckedShift
		} else if op.Family == operatorBoolean && (op.Token == syntax.LogicalAnd || op.Token == syntax.LogicalOr) {
			node.Kind = tir.ShortCircuitValue
		} else if op.Family == operatorIntegralSame && op.Token == syntax.Percent {
			node.Kind = tir.CheckedArithmetic
		} else if (op.Family == operatorNumericSame || op.Family == operatorAdd) && s.operatorHasIntegerOperand(op) {
			node.Kind = tir.CheckedArithmetic
		} else {
			node.Kind = tir.BinaryValue
		}
	default:
		return false
	}
	if !allowedOperatorFamily(op.Family, op.Form) {
		return false
	}
	children := make([]tir.NodeID, 0, len(op.Operands))
	for _, operand := range op.Operands {
		operandNode, ok := s.buildValue(operand)
		if !ok {
			return false
		}
		children = append(children, operandNode)
	}
	node.Operator = op.Token
	node.Children = children
	return true
}

func (s *irBuildState) operatorHasIntegerOperand(op *operatorRecord) bool {
	if len(op.Operands) == 0 {
		return false
	}
	typ, ok := typeOfValue(s.records, op.Operands[0])
	if !ok {
		return false
	}
	key, ok := s.handoff.Semantics.Types().Key(typ)
	if !ok {
		return false
	}
	builtin, ok := key.Builtin()
	return ok && isIntegerBuiltin(builtin)
}

func allowedOperatorFamily(family operatorFamily, form operatorForm) bool {
	switch family {
	case operatorLiteralNegate:
		return form == operatorPrefix
	case operatorBoolean:
		return form == operatorPrefix || form == operatorBinary
	case operatorNumericSame, operatorIntegralSame:
		return form == operatorPrefix || form == operatorBinary
	case operatorAdd, operatorShift, operatorOrdering, operatorEquality:
		return form == operatorBinary
	}
	return false
}

func decodeIntegerLiteral(bytes []byte) (string, string, bool) {
	stripped := make([]byte, 0, len(bytes))
	for _, b := range bytes {
		if b != '_' {
			stripped = append(stripped, b)
		}
	}
	base := 10
	digits := string(stripped)
	switch {
	case strings.HasPrefix(digits, "0x") || strings.HasPrefix(digits, "0X"):
		base, digits = 16, digits[2:]
	case strings.HasPrefix(digits, "0b") || strings.HasPrefix(digits, "0B"):
		base, digits = 2, digits[2:]
	case strings.HasPrefix(digits, "0o") || strings.HasPrefix(digits, "0O"):
		base, digits = 8, digits[2:]
	}
	if digits == "" {
		return "", "", false
	}
	value, ok := new(big.Int).SetString(digits, base)
	if !ok {
		return "", "", false
	}
	return value.String(), "1", true
}

func decodeFloatLiteral(bytes []byte) (string, bool) {
	stripped := make([]byte, 0, len(bytes))
	for _, b := range bytes {
		if b != '_' {
			stripped = append(stripped, b)
		}
	}
	if len(stripped) == 0 {
		return "", false
	}
	return string(stripped), true
}
