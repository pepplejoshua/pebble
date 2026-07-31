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
	handoff             *solveHandoff
	records             *solvedRecords
	builder             *tir.Builder
	functions           map[symbol.SymbolID]tir.FunctionID
	functionNodes       map[symbol.SymbolID]tir.NodeID
	regions             map[controlID]tir.RegionID
	values              map[valueID]tir.NodeID
	expressionsByResult map[valueID]*expressionRecord
	aggregatesByRecord  map[recordID]*aggregateRecord
	operatorsBySyntax   map[symbol.SyntaxRef]*operatorRecord
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
		if !s.buildVariantMember(record, &node) {
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
		if !s.buildOperatorValue(record, &node) {
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
		if op.Family == operatorBoolean && (op.Token == syntax.LogicalAnd || op.Token == syntax.LogicalOr) {
			node.Kind = tir.ShortCircuitValue
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
