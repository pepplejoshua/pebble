package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildUnit constructs the declaration/nonvalue portion of typed IR. It is
// intentionally not called by run06b yet: later 06b.7b parts add values,
// places, calls, coercions, and statements at this orchestration point.
func buildUnit(handoff *solveHandoff, records *solvedRecords, requirements map[symbol.SymbolID][]Requirement, diagnostics *diagnostic.DiagnosticSet, config Config, store *types.Store) (unit *tir.Unit, ok bool) {
	if !canContinueWithPartial(handoff, config) || handoff.Semantics == nil || handoff.Solution == nil || records == nil {
		return nil, false
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	fail := func(message string) (*tir.Unit, bool) {
		reporter.add(diagnostic.Diagnostic{Severity: diagnostic.Error, Code: CodeGeneration, Message: message})
		reporter.flush()
		return nil, false
	}
	resolution := handoff.Semantics.Resolution()
	runtimeInfo := tir.RuntimeInfo{}
	if resolution != nil {
		runtimeInfo.Allocator, _ = resolution.Runtime(symbol.RuntimeAllocator)
		runtimeInfo.Context, _ = resolution.Runtime(symbol.RuntimeContext)
	}
	if runtimeInfo.Allocator != 0 {
		members := resolution.Members(runtimeInfo.Allocator)
		for _, member := range members {
			if value, ok := resolution.Symbols.Symbol(member); ok {
				switch value.Name {
				case "ptr":
					runtimeInfo.AllocatorPtr = member
				case "alloc":
					runtimeInfo.AllocatorAlloc = member
				case "realloc":
					runtimeInfo.AllocatorRealloc = member
				case "free":
					runtimeInfo.AllocatorFree = member
				}
			}
		}
	}
	if runtimeInfo.Context != 0 {
		for _, member := range resolution.Members(runtimeInfo.Context) {
			if value, ok := resolution.Symbols.Symbol(member); ok && value.Name == "default_allocator" {
				runtimeInfo.ContextDefaultAllocator = member
			}
		}
	}
	// Specialization substitution interns composite sizeof targets before the
	// immutable IR type snapshot is taken.
	for _, instantiation := range handoff.Solution.Instantiations() {
		signature, exists := handoff.Semantics.Signature(instantiation.Generic)
		if !exists || len(signature.TypeParams) != len(instantiation.Arguments) {
			continue
		}
		substitution := make(map[symbol.SymbolID]types.TypeID, len(signature.TypeParams))
		for index, parameter := range signature.TypeParams {
			if instantiation.Arguments[index].State != infer.TypeFinal {
				substitution = nil
				break
			}
			substitution[parameter] = instantiation.Arguments[index].Type
		}
		if substitution == nil {
			continue
		}
		for _, resolved := range records.roots {
			if resolved.State == infer.TypeFinal {
				if _, err := store.Substitute(resolved.Type, substitution); err != nil {
					return fail("typed-IR construction failed during specialization type substitution")
				}
			}
		}
	}
	typeSnapshot, err := store.Snapshot()
	if err != nil {
		return fail("typed-IR construction failed during specialization type snapshot")
	}
	b := tir.NewBuilder(typeSnapshot, tir.Config{
		MaxIRNodes: config.MaxIRNodes, MaxIRComponents: config.MaxIRComponents,
		MaxDumpBytes: config.MaxDumpBytes,
		Runtime:      runtimeInfo,
	})
	state := &irBuildState{handoff: handoff, records: records, builder: b, store: store, cache: newSpecializationCache(), irBuildScope: newIRBuildScope()}
	steps := []struct {
		name  string
		build func() bool
	}{
		{"buildModules", state.buildModules}, {"buildTypes", state.buildTypes},
		{"buildDeclarations", state.buildDeclarations}, {"buildTypeUses", state.buildTypeUses},
		{"indexExpressions", state.indexExpressions}, {"indexControls", state.indexControls},
		{"buildGlobalInitializers", state.buildGlobalInitializers},
		{"buildBlocks", state.buildBlocks}, {"finishFunctionDeclarations", state.finishFunctionDeclarations},
		{"buildRequirements", func() bool { return state.buildRequirements(requirements) }},
		{"buildSpecializations", state.buildSpecializations},
	}
	for _, step := range steps {
		if !step.build() {
			return fail("typed-IR construction failed during " + step.name)
		}
	}
	// Building interned new concrete types (generic substitutions resolved
	// through the active mapping during buildBlocks/buildSpecializations) after
	// the snapshot NewBuilder received; the unit's owning snapshot must cover
	// every referenced type or Build's verifier rejects the unit.
	freshSnapshot, err := store.Snapshot()
	if err != nil {
		return fail("typed-IR construction failed during specialization type snapshot")
	}
	if err := b.RefreshSnapshot(freshSnapshot); err != nil {
		return fail("typed-IR construction failed during specialization type snapshot")
	}
	unit, err = b.Build()
	if err != nil {
		return fail("typed-IR construction failed during Build: " + err.Error())
	}
	return unit, true
}

// buildSpecializations triggers a real, built specialization for every
// generic instantiation the program actually uses, so the published
// unit contains runnable typed IR for each one instead of only the
// never-runnable symbolic declaration. A symbolic instantiation — one whose
// type arguments still reference a generic body's own type parameter
// (clamp[T] calling min(x, hi) with its own T) — is skipped here: its
// concrete specialization is built when the enclosing generic is itself
// specialized, at which point buildDirectCall resolves the call's type
// arguments through the active substitution. Structurally recursive
// instantiations are handled by buildSpecialization's own cache
// (07.2/07.3f); this just ensures every top-level instantiation is
// reached at least once.
func (s *irBuildState) buildSpecializations() bool {
	if s.handoff.Solution == nil {
		return true
	}
	for _, instantiation := range s.handoff.Solution.Instantiations() {
		if !s.concreteInstantiation(instantiation) {
			continue
		}
		if _, ok := s.buildSpecialization(instantiation); !ok {
			return false
		}
	}
	return true
}

// concreteInstantiation reports whether an instantiation's type arguments are
// all fully concrete, with no type parameter anywhere in their structure.
func (s *irBuildState) concreteInstantiation(instantiation infer.Instantiation) bool {
	for _, argument := range instantiation.Arguments {
		if argument.State != infer.TypeFinal || argument.Type == 0 || s.containsTypeParameter(argument.Type, 0) {
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
	globalLetInitializers        map[symbol.SymbolID]valueID
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
			memberTypes := make([]types.TypeID, 0, len(d.Members))
			for _, member := range d.Members {
				members = append(members, member.Symbol)
				// Generic declarations keep a parameterized member template until
				// specialization. A member typed as the struct's own type parameter
				// records that parameter's TypeID here so the backend can substitute
				// it per instantiation; a member whose template is a composite that
				// wraps a parameter stays unresolved (0) until the backend recovers
				// it from the specialized usage node. Ordinary declarations are
				// always known here and no longer depend on usage evidence.
				memberType := types.TypeID(0)
				if template, found := s.handoff.Semantics.Template(member.Type); found {
					switch template.Kind {
					case infer.TemplateKnown:
						memberType = template.Known
					case infer.TemplateParameter:
						if parameterType, ok := s.handoff.Semantics.TypeParameter(template.Parameter); ok {
							memberType = parameterType
						}
					}
				}
				memberTypes = append(memberTypes, memberType)
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
			if err := s.builder.AddTypeDecl(tir.TypeDecl{Symbol: id, Span: sym.Span, Members: members, MemberTypes: memberTypes, Node: nodeID, Union: d.Nominal == infer.NominalUnion}); err != nil {
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

// typeKey resolves a type's key against the live type store rather than the
// frozen inference snapshot. A type resolved during a specialization build has
// already been substituted through the active mapping, which interns concrete
// types into the store AFTER the freeze-time snapshot was taken; those
// substituted types are absent from the snapshot, so any key lookup of a
// resolveType result must read the store. Test-constructed build states bind
// no store, so the frozen snapshot is the fallback there (they never apply a
// substitution).
func (s *irBuildState) typeKey(typ types.TypeID) (types.TypeKey, bool) {
	if s.store != nil {
		return s.store.Key(typ)
	}
	return s.handoff.Semantics.Types().Key(typ)
}

func (s *irBuildState) buildDeclarations() bool {
	s.functionRegions = make(map[symbol.SymbolID]controlID)
	s.functionDecls = nil
	s.globalLetInitializers = make(map[symbol.SymbolID]valueID)
	mappedParameterRefs := make(map[symbol.SyntaxRef]struct{})
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
				parameterRef := ps.Declaration
				if _, exists := mappedParameterRefs[parameterRef]; exists {
					parameterRef = symbol.SyntaxRef{}
				} else {
					mappedParameterRefs[parameterRef] = struct{}{}
				}
				if _, ok := s.addNode(tir.Node{Kind: tir.ParameterDeclaration, Span: ps.Span, Symbol: ps.ID}, parameterRef); !ok {
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
				// A `let` (immutable) global constant's VALUE is inlined at
				// each reference site by buildValueBase, so the backend never
				// needs a global-storage scheme: the initializer is a
				// compile-time constant (std/io.peb's MODE_* and Seek* are all
				// literals), and rebinding a `let` is a checker error, so
				// every reference observes the same value. Recording the
				// initializer's value here (only for immutable `let`, never
				// mutable `var`, whose value could change) is what lets a
				// reference rebuild the constant instead of emitting a
				// SymbolValue for a storage the backend has no machinery for.
				if b.Kind == bindingGlobalLet && b.InitializerPresent && b.Initializer != 0 {
					s.globalLetInitializers[b.Symbol] = b.Initializer
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

// buildGlobalInitializers builds the compile-time-constant initializer value
// node for every mutable (`var`) module-level global and attaches it to the
// global's GlobalDecl (builder.SetGlobalInitializer). It runs as its own step
// after indexExpressions — the initializer's expression record must be
// indexed before buildValue can lower it — and before buildBlocks. An
// immutable (`let`) global's initializer is deliberately skipped: its value is
// inlined at each reference site by buildValueRecord's
// globalLetInitializers path, so the backend never needs storage (or a stored
// initializer) for it. A `var` global instead resolves through real storage,
// and this step is what seeds that storage's initial value: the checker has
// already validated the initializer is a compile-time constant (C0616), so the
// built node is always a constant expression the backend can emit as a C
// static initializer. Records are iterated in their fixed order so node
// creation order (and therefore every emitted NodeID) stays deterministic.
func (s *irBuildState) buildGlobalInitializers() bool {
	for _, retained := range s.handoff.Records.Records() {
		b := retained.Binding
		if b == nil || b.Kind != bindingGlobalVar || !b.InitializerPresent || b.Initializer == 0 {
			continue
		}
		initializerNode, ok := s.buildValue(b.Initializer)
		if !ok {
			return false
		}
		if err := s.builder.SetGlobalInitializer(b.Symbol, initializerNode); err != nil {
			return false
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
		if !activeOperatorRecord(s.handoff, retained.Header) {
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
		if retained.Aggregate != nil && (retained.Aggregate.Kind == aggregateEnumVariant || retained.Aggregate.Kind == aggregateTaggedVariant) && len(retained.Aggregate.Fields) != 0 {
			if member := caseVariantMember(s.handoff, retained.Aggregate); member != 0 {
				s.variantBySyntax[retained.Header.Syntax] = member
			}
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
