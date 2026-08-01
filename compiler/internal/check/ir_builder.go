package check

import (
	"math/big"
	"sort"
	"strconv"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildUnit constructs the declaration/nonvalue portion of typed IR. It is
// intentionally not called by run06b yet: later 06b.7b parts add values,
// places, calls, coercions, and statements at this orchestration point.
func buildUnit(handoff *solveHandoff, records *solvedRecords, requirements map[symbol.SymbolID][]Requirement, diagnostics *diagnostic.DiagnosticSet, config Config, store *types.Store) (unit *tir.Unit, ok bool) {
	if handoff == nil || handoff.GenerationHadErrors || handoff.Semantics == nil || handoff.Solution == nil || records == nil {
		return nil, false
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	fail := func(message string) (*tir.Unit, bool) {
		reporter.add(diagnostic.Diagnostic{Severity: diagnostic.Error, Code: CodeGeneration, Message: message})
		reporter.flush()
		return nil, false
	}
	b := tir.NewBuilder(handoff.Semantics.Types(), tir.Config{
		MaxIRNodes: config.MaxIRNodes, MaxIRComponents: config.MaxIRComponents,
		MaxDumpBytes: config.MaxDumpBytes,
	})
	state := &irBuildState{handoff: handoff, records: records, builder: b, store: store, cache: newSpecializationCache(), irBuildScope: newIRBuildScope()}
	steps := []struct {
		name  string
		build func() bool
	}{
		{"buildModules", state.buildModules}, {"buildTypes", state.buildTypes},
		{"buildDeclarations", state.buildDeclarations}, {"buildTypeUses", state.buildTypeUses},
		{"indexExpressions", state.indexExpressions}, {"indexControls", state.indexControls},
		{"buildBlocks", state.buildBlocks}, {"finishFunctionDeclarations", state.finishFunctionDeclarations},
		{"buildRequirements", func() bool { return state.buildRequirements(requirements) }},
		{"buildSpecializations", state.buildSpecializations},
	}
	for _, step := range steps {
		if !step.build() {
			return fail("typed-IR construction failed during " + step.name)
		}
	}
	unit, err := b.Build()
	if err != nil {
		return fail("typed-IR construction failed during Build: " + err.Error())
	}
	return unit, true
}

// buildSpecializations triggers a real, built specialization for every
// generic instantiation the program actually uses, so the published
// unit contains runnable typed IR for each one instead of only the
// never-runnable symbolic declaration. Structurally recursive
// instantiations are handled by buildSpecialization's own cache
// (07.2/07.3f); this just ensures every top-level instantiation is
// reached at least once.
func (s *irBuildState) buildSpecializations() bool {
	if s.handoff.Solution == nil {
		return true
	}
	for _, instantiation := range s.handoff.Solution.Instantiations() {
		if _, ok := s.buildSpecialization(instantiation); !ok {
			return false
		}
	}
	return true
}

// irBuildScope holds every piece of output memoization that must be
// fresh and isolated per function-body build: two different builds
// (the normal symbolic build, or two different concrete
// specializations of the same generic) must never share these maps,
// since they are keyed by identities (valueID, controlID,
// symbol.SymbolID, symbol.SyntaxRef) that repeat across
// specializations of the same generic declaration.
type irBuildScope struct {
	functions     map[symbol.SymbolID]tir.FunctionID
	functionNodes map[symbol.SymbolID]tir.NodeID
	regions       map[controlID]tir.RegionID
	values        map[valueID]tir.NodeID
	placeValues   map[valueID]tir.NodeID
	blockNodes    map[controlID]tir.NodeID
	deferNodes    map[symbol.SyntaxRef]tir.NodeID
}

func newIRBuildScope() *irBuildScope {
	return &irBuildScope{
		functions:     make(map[symbol.SymbolID]tir.FunctionID),
		functionNodes: make(map[symbol.SymbolID]tir.NodeID),
		regions:       make(map[controlID]tir.RegionID),
		values:        make(map[valueID]tir.NodeID),
		placeValues:   make(map[valueID]tir.NodeID),
		blockNodes:    make(map[controlID]tir.NodeID),
		deferNodes:    make(map[symbol.SyntaxRef]tir.NodeID),
	}
}

type irBuildState struct {
	*irBuildScope
	handoff                      *solveHandoff
	records                      *solvedRecords
	builder                      *tir.Builder
	store                        *types.Store
	cache                        *specializationCache
	activeSubstitution           map[symbol.SymbolID]types.TypeID
	places                       map[symbol.SyntaxRef]*placeRecord
	expressionsByResult          map[valueID]*expressionRecord
	aggregatesByRecord           map[recordID]*aggregateRecord
	castsByRecord                map[recordID]*castRecord
	compatibilityBySource        map[valueID]*compatibilityRecord
	tuplesBySyntax               map[symbol.SyntaxRef][]*compatibilityRecord
	operatorsBySyntax            map[symbol.SyntaxRef]*operatorRecord
	operatorsByResult            map[valueID]*operatorRecord
	membersByResult              map[valueID]*memberRecord
	indexesByResult              map[valueID]*indexRecord
	indexesBySyntax              map[symbol.SyntaxRef]*indexRecord
	callsBySyntax                map[symbol.SyntaxRef]*callRecord
	contextFlowsBySyntax         map[symbol.SyntaxRef]*contextFlowRecord
	compatibilityReturnsBySource map[valueID]*compatibilityRecord
	byRegion                     map[controlID][]*controlRecord
	bySyntax                     map[symbol.SyntaxRef]*controlRecord
	owner                        map[controlID]*controlRecord
	defersByRegion               map[controlID][]*deferRecord
	deferByStatement             map[symbol.SyntaxRef]*deferRecord
	deferByHeader                map[symbol.SyntaxRef]*deferRecord
	variantBySyntax              map[symbol.SyntaxRef]symbol.SymbolID
	functionDecls                []irFunctionDecl
	functionRegions              map[symbol.SymbolID]controlID
}

// withFreshScope runs build with a brand-new, empty irBuildScope
// active, then restores whatever scope was active before -- so a
// specialized function body can be built with output memoization
// completely isolated from the normal build (or from any other
// specialization), without permanently disturbing the state.
func (s *irBuildState) withFreshScope(build func() (tir.NodeID, bool)) (tir.NodeID, bool) {
	previous := s.irBuildScope
	s.irBuildScope = newIRBuildScope()
	defer func() { s.irBuildScope = previous }()
	return build()
}

type irFunctionDecl struct {
	callable               callableRef
	header                 symbol.SyntaxRef
	span                   source.Span
	params                 []tir.Parameter
	result                 types.TypeID
	kind                   callableKind
	convention             types.CallingConvention
	variadic, inline, body bool
}

func (s *irBuildState) addNode(node tir.Node, ref symbol.SyntaxRef) (tir.NodeID, bool) {
	if s.activeSubstitution != nil {
		ref = symbol.SyntaxRef{}
		node.Syntax = symbol.SyntaxRef{}
	}
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

// resolveType resolves value's solved type and applies the active
// specialization substitution when one is present.
func (s *irBuildState) resolveType(id valueID) (types.TypeID, bool) {
	typ, ok := typeOfValue(s.records, id)
	if !ok || s.activeSubstitution == nil {
		return typ, ok
	}
	substituted, err := s.store.Substitute(typ, s.activeSubstitution)
	if err != nil {
		return 0, false
	}
	return substituted, true
}

func (s *irBuildState) buildDeclarations() bool {
	s.functionRegions = make(map[symbol.SymbolID]controlID)
	s.functionDecls = nil
	for _, retained := range s.handoff.Records.Records() {
		if retained.Callable != nil {
			c := retained.Callable
			sym, ok := s.symbol(c.Symbol)
			if !ok {
				return false
			}
			params := make([]tir.Parameter, len(c.Parameters))
			for i, value := range c.Parameters {
				typ, ok := s.resolveType(value)
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
			result, ok := s.resolveType(c.Result)
			if !ok {
				return false
			}
			fid, err := s.builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: c.Symbol, Span: sym.Span})
			if err != nil {
				return false
			}
			s.functions[c.Symbol] = fid
			s.functionDecls = append(s.functionDecls, irFunctionDecl{
				callable: callableRef{Symbol: c.Symbol, Syntax: c.Header.Syntax}, header: c.Header.Syntax,
				span: sym.Span, params: params, result: result, kind: c.Kind, convention: c.Convention,
				variadic: c.Variadic, inline: c.Inline, body: c.BodyPresent,
			})
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
			typ, ok := s.resolveType(b.Annotation)
			if !ok && b.InitializerPresent {
				typ, ok = s.resolveType(b.Initializer)
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
				if _, ok := s.addNode(tir.Node{Kind: tir.ExternDeclaration, Span: sym.Span, Symbol: b.Symbol, Convention: b.Convention}, b.Header.Syntax); !ok {
					return false
				}
			}
		}
	}
	for _, retained := range s.handoff.Records.Records() {
		if retained.Control == nil || retained.Control.Kind != controlFunction {
			continue
		}
		if retained.Control.Callable.Symbol != 0 {
			s.functionRegions[retained.Control.Callable.Symbol] = retained.Control.Region
		}
	}
	return true
}

func (s *irBuildState) symbolForParameter(owner symbol.SymbolID, ordinal int) (symbol.Symbol, bool) {
	sig, ok := s.handoff.Semantics.Signature(owner)
	if ok {
		if ordinal >= len(sig.Parameters) {
			return symbol.Symbol{}, false
		}
		return s.symbol(sig.Parameters[ordinal])
	}
	ownerSymbol, ok := s.symbol(owner)
	if !ok {
		return symbol.Symbol{}, false
	}
	resolution := s.handoff.Semantics.Resolution()
	for _, scope := range resolution.Scopes.All() {
		if scope.Kind != symbol.ScopeFunction || scope.Origin != ownerSymbol.Declaration {
			continue
		}
		parameters := make([]symbol.Symbol, 0, len(scope.Symbols))
		for _, id := range scope.Symbols {
			candidate, exists := resolution.Symbols.Symbol(id)
			if exists && candidate.Kind == symbol.SymbolParameter {
				parameters = append(parameters, candidate)
			}
		}
		if ordinal < len(parameters) {
			return parameters[ordinal], true
		}
	}
	return symbol.Symbol{}, false
}

func (s *irBuildState) buildTypeUses() bool {
	for _, retained := range s.handoff.Records.Records() {
		if retained.TypeUse == nil {
			continue
		}
		typ, ok := s.resolveType(retained.TypeUse.Type)
		if !ok {
			return false
		}
		if _, ok := s.addNode(tir.Node{Kind: tir.TypeUse, Span: retained.Header.Span, Syntax: retained.Header.Syntax, TypeArg: typ}, retained.Header.Syntax); !ok {
			return false
		}
	}
	return true
}

func (s *irBuildState) buildStatementValue(id valueID) (tir.NodeID, bool) {
	if compatibility := s.compatibilityBySource[id]; compatibility != nil && compatibility.Role == compatibilityAssignment {
		return s.buildCompatibility(id, compatibility)
	}
	return s.buildValue(id)
}

func (s *irBuildState) buildReturnValue(id valueID) (tir.NodeID, bool) {
	if compatibility := s.compatibilityReturnsBySource[id]; compatibility != nil {
		return s.buildCompatibility(id, compatibility)
	}
	return s.buildValue(id)
}

func (s *irBuildState) bindingForSyntax(ref symbol.SyntaxRef) *bindingRecord {
	for _, retained := range s.handoff.Records.Records() {
		if retained.Binding != nil && retained.Header.Syntax == ref {
			return retained.Binding
		}
	}
	return nil
}

func (s *irBuildState) assignmentForSyntax(ref symbol.SyntaxRef) *assignmentRecord {
	for _, retained := range s.handoff.Records.Records() {
		if retained.Assignment != nil && retained.Assignment.Statement == ref {
			return retained.Assignment
		}
	}
	return nil
}

func (s *irBuildState) finishFunctionDeclarations() bool {
	for _, decl := range s.functionDecls {
		region := s.functionRegions[decl.callable.Symbol]
		bodyRegion := region
		if region != 0 && uint64(region) <= uint64(len(s.handoff.Records.Controls())) {
			for _, child := range s.handoff.Records.Controls()[region-1].Children {
				if owner := s.owner[child]; owner != nil && owner.Kind == controlBlock {
					bodyRegion = child
					break
				}
			}
		}
		node, hasBody := s.blockNodes[bodyRegion]
		fid := s.functions[decl.callable.Symbol]
		if node != 0 {
			if err := s.builder.CompleteFunctionDecl(fid, node); err != nil {
				return false
			}
		}
		kind := tir.FunctionDeclaration
		if decl.kind == callableExtern {
			kind = tir.ExternDeclaration
		}
		declSyntax := decl.header
		if decl.kind == callableLiteral {
			declSyntax = symbol.SyntaxRef{}
		}
		declNode, ok := s.addNode(tir.Node{Kind: kind, Span: decl.span, Syntax: declSyntax, Symbol: decl.callable.Symbol, Function: fid, Parameters: decl.params, ResultType: decl.result, Convention: decl.convention, Variadic: decl.variadic, Inline: decl.inline, HasBody: decl.body}, declSyntax)
		if !ok {
			return false
		}
		s.functionNodes[decl.callable.Symbol] = declNode
		_ = hasBody
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
	s.castsByRecord = make(map[recordID]*castRecord)
	s.compatibilityBySource = make(map[valueID]*compatibilityRecord)
	s.compatibilityReturnsBySource = make(map[valueID]*compatibilityRecord)
	s.tuplesBySyntax = make(map[symbol.SyntaxRef][]*compatibilityRecord)
	s.operatorsBySyntax = make(map[symbol.SyntaxRef]*operatorRecord)
	s.operatorsByResult = make(map[valueID]*operatorRecord)
	s.membersByResult = make(map[valueID]*memberRecord)
	s.indexesByResult = make(map[valueID]*indexRecord)
	s.indexesBySyntax = make(map[symbol.SyntaxRef]*indexRecord)
	s.callsBySyntax = make(map[symbol.SyntaxRef]*callRecord)
	s.contextFlowsBySyntax = make(map[symbol.SyntaxRef]*contextFlowRecord)
	s.places = make(map[symbol.SyntaxRef]*placeRecord)
	s.variantBySyntax = make(map[symbol.SyntaxRef]symbol.SymbolID)
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
		if retained.Cast != nil {
			s.castsByRecord[retained.Cast.Header.ID] = retained.Cast
		}
		if retained.Compatibility != nil {
			c := retained.Compatibility
			if c.Role == compatibilityTupleComponent {
				s.tuplesBySyntax[c.Header.Syntax] = append(s.tuplesBySyntax[c.Header.Syntax], c)
			} else if c.Role == compatibilityReturn {
				s.compatibilityReturnsBySource[c.Source] = c
			} else if _, exists := s.compatibilityBySource[c.Source]; !exists {
				s.compatibilityBySource[c.Source] = c
			}
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
		if retained.Member != nil && retained.Member.Kind == memberVariant && retained.Member.Member != 0 {
			s.variantBySyntax[retained.Header.Syntax] = retained.Member.Member
		}
	}
	return true
}

// indexControls mirrors validateControlFlow's indexes so construction and
// validation walk the frozen control arena in the same authored order. It also
// indexes defer records the same way defer_validation.go does: by region in
// record order (which is exactly registration/Ordinal order per prepareDefer),
// by deferred statement, and by header.
func (s *irBuildState) indexControls() bool {
	s.byRegion = make(map[controlID][]*controlRecord)
	s.bySyntax = make(map[symbol.SyntaxRef]*controlRecord)
	s.owner = make(map[controlID]*controlRecord)
	s.defersByRegion = make(map[controlID][]*deferRecord)
	s.deferByStatement = make(map[symbol.SyntaxRef]*deferRecord)
	s.deferByHeader = make(map[symbol.SyntaxRef]*deferRecord)
	retainedRecords := s.handoff.Records.Records()
	for i := range retainedRecords {
		retained := &retainedRecords[i]
		if !activeOperatorRecord(s.handoff, retained.Header) {
			continue
		}
		if retained.Defer != nil {
			record := retained.Defer
			s.defersByRegion[record.Region] = append(s.defersByRegion[record.Region], record)
			s.deferByStatement[record.Statement] = record
			s.deferByHeader[record.Header.Syntax] = record
		}
		if retained.Control == nil {
			continue
		}
		ctrl := retained.Control
		s.byRegion[ctrl.Region] = append(s.byRegion[ctrl.Region], ctrl)
		s.bySyntax[ctrl.Header.Syntax] = ctrl
		if regionOwningControl(ctrl.Kind) {
			s.owner[ctrl.Region] = ctrl
		}
	}
	return true
}

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
		class := classify(s.handoff.Semantics, sourceType, destination)
		coercion := coercionFor(s.handoff.Semantics, class, sourceType, destination)
		coercionNode := map[coercionKind]tir.NodeKind{
			coercionIntegerCast: tir.IntegerCast, coercionIntegerToFloat: tir.IntegerToFloat,
			coercionFloatToInteger: tir.FloatToInteger, coercionFloatCast: tir.FloatCast,
			coercionEnumToInteger: tir.EnumToInteger, coercionOptionalIntegerToEnum: tir.OptionalIntegerToEnum,
			coercionCheckedIntegerToEnum: tir.CheckedIntegerToEnum,
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
	case expressionSome:
		if len(record.Children) != 1 {
			return 0, false
		}
		payload, ok := s.buildValue(record.Children[0])
		if !ok {
			return 0, false
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
		if op != nil && op.Family == operatorDereference {
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
	nid, ok := s.addNode(node, record.Header.Syntax)
	if !ok {
		return 0, false
	}
	s.values[id] = nid
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
		typeArgs[i] = argument.Type
	}
	if _, ok := s.buildSpecialization(instantiation); !ok {
		return false
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
		coercionOptionalIntegerToEnum: tir.OptionalIntegerToEnum, coercionCheckedIntegerToEnum: tir.CheckedIntegerToEnum,
	}[kind]
	if irKind == 0 {
		return 0, false
	}
	return s.addNode(tir.Node{Kind: irKind, Type: destination, Span: span, Children: []tir.NodeID{child}}, ref)
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
		if typ, found := s.resolveType(assignment); found {
			finalType = typ
		}
	}
	var current tir.NodeID
	for i, projection := range record.Projections {
		typ := rootType.Type
		if i > 0 {
			if i+1 < len(record.Projections) {
				typ, ok = s.resolveType(record.Projections[i+1].Base)
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
	if existing, ok := s.placeValues[id]; ok {
		return existing, true
	}
	record, ok := s.expressionsByResult[id]
	if !ok {
		return 0, false
	}
	typ, ok := s.resolveType(id)
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
	typ, ok := s.resolveType(base)
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
	typ, ok := s.resolveType(id)
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
	case symbol.SymbolFunction:
		if sym.Generic {
			return false
		}
		function := s.functions[record.Symbol]
		if function == 0 {
			return false
		}
		node.Kind, node.Symbol, node.Function = tir.HoistedFunctionValue, record.Symbol, function
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
	argType, ok := s.resolveType(record.TypeArgument)
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
	typ, ok := s.resolveType(id)
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

func (s *irBuildState) buildInterpolated(record *expressionRecord, node *tir.Node) bool {
	parts := make([]tir.InterpolationPart, 0, len(record.Parts))
	for _, part := range record.Parts {
		switch part.Kind {
		case interpolationText:
			parts = append(parts, tir.InterpolationPart{Kind: tir.InterpolationTextPart, Text: part.Text})
		case interpolationValue:
			valueNode, ok := s.buildValue(part.Value)
			if !ok {
				return false
			}
			parts = append(parts, tir.InterpolationPart{Kind: tir.InterpolationValuePart, Value: valueNode})
		}
	}
	node.Kind = tir.InterpolatedString
	node.Parts = parts
	return true
}

func (s *irBuildState) buildOperatorValue(record *expressionRecord, node *tir.Node) bool {
	op, ok := s.operatorsBySyntax[record.Header.Syntax]
	if !ok || op == nil {
		return false
	}
	switch op.Form {
	case operatorPrefix:
		if (op.Family == operatorLiteralNegate || op.Family == operatorNumericSame) && s.operatorHasIntegerOperand(op) {
			node.Kind = tir.CheckedNegate
		} else {
			node.Kind = tir.PrefixValue
		}
	case operatorPostfix:
		if op.Family != operatorOptionalForce || op.Token != syntax.Bang {
			return false
		}
		node.Kind = tir.CheckedOptionalUnwrap
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
	switch node.Kind {
	case tir.PrefixValue, tir.BinaryValue, tir.ShortCircuitValue, tir.CheckedArithmetic, tir.CheckedNegate, tir.CheckedShift:
		node.Operator = op.Token
	}
	node.Children = children
	return true
}

func (s *irBuildState) operatorHasIntegerOperand(op *operatorRecord) bool {
	if len(op.Operands) == 0 {
		return false
	}
	typ, ok := s.resolveType(op.Operands[0])
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
	case operatorOptionalForce:
		return form == operatorPostfix
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
