package check

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeGeneration diagnostic.Code = "C0619"

type Inputs struct {
	Graph         *module.Graph
	Sources       *source.FileSet
	Resolution    *symbol.Result
	Types         *types.Store
	LiteralTarget infer.LiteralTarget
}

type generationState uint8

const (
	generationMutable generationState = iota + 1
	generationFrozen
)

type generatedValue struct {
	ID     valueID
	Term   infer.Term
	Root   valueRoot
	Origin infer.Origin
}

type generationCounters struct {
	syntaxVisits        uint32
	traversalDepth      uint32
	trackedPlaces       uint32
	genericRequirements uint32
	constantDepth       uint32
	constantOperations  uint64
}

type generation struct {
	inputs      Inputs
	config      Config
	diagnostics *diagnostic.DiagnosticSet
	reporter    *generationReporter
	state       generationState
	values      []generatedValue
	roots       rootArena
	records     recordArena
	controls    controlArena
	counters    generationCounters
}

type frozenGeneration struct {
	inputs   Inputs
	values   []generatedValue
	roots    frozenRoots
	records  frozenRecords
	counters generationCounters
}

func newGeneration(inputs Inputs, diagnostics *diagnostic.DiagnosticSet, config Config) *generation {
	if diagnostics == nil {
		diagnostics = diagnostic.NewDiagnosticSet()
	}
	config = normalizeConfig(config)
	g := &generation{
		inputs: inputs, config: config, diagnostics: diagnostics,
		reporter: newGenerationReporter(diagnostics, config.MaxDiagnostics),
		state:    generationMutable,
	}
	g.validateSnapshot()
	return g
}

func (g *generation) mutable(operation string) bool {
	if g == nil {
		return false
	}
	if g.state != generationMutable {
		g.report(fmt.Sprintf("cannot %s after generation is frozen", operation), source.Span{})
		g.reporter.flush()
		return false
	}
	return true
}

func (g *generation) addValue(value generatedValue) (valueID, bool) {
	if !g.mutable("append a value") {
		return 0, false
	}
	if value.ID != 0 || value.Root != (valueRoot{}) {
		g.report("new value contains a preassigned or foreign identity", value.Origin.Span)
		return 0, false
	}
	if uint64(len(g.values)) >= uint64(g.config.MaxSyntaxVisits) {
		if g != nil && g.state == generationMutable {
			g.reportLimit("value", uint64(g.config.MaxSyntaxVisits))
		}
		return 0, false
	}
	id := valueID(len(g.values) + 1)
	value.ID = id
	g.values = append(g.values, value)
	return id, true
}

func (g *generation) hasValue(id valueID) bool {
	return g != nil && id.valid() && uint64(id) <= uint64(len(g.values))
}

func (g *generation) addRoot(value valueID, root valueRoot) bool {
	if !g.mutable("append a root") {
		return false
	}
	if !g.roots.append(value, root, g.hasValue, g.validRoot, g.config.MaxSyntaxVisits) {
		g.report("invalid, duplicate, foreign, or over-limit value root", source.Span{})
		return false
	}
	g.values[value-1].Root = root
	return true
}

func (g *generation) validRoot(root valueRoot) bool {
	if g == nil || !root.Alternative.valid() {
		return false
	}
	zeroSyntax := root.Syntax == (symbol.SyntaxRef{})
	zeroSlot := root.Slot == (infer.SlotID{})
	switch root.Kind {
	case rootSyntax:
		return !root.Alternative.Guarded && g.validSyntax(root.Syntax) && root.Symbol == 0 && zeroSlot && root.Parameter == 0
	case rootSymbol:
		return !root.Alternative.Guarded && zeroSyntax && g.validSymbol(root.Symbol) && zeroSlot && root.Parameter == 0
	case rootInstantiation, rootMethod:
		return !root.Alternative.Guarded && g.validSyntax(root.Syntax) && root.Symbol == 0 && zeroSlot
	case rootSlot:
		return zeroSyntax && root.Symbol == 0 && !zeroSlot && root.Parameter == 0
	default:
		return false
	}
}

func (g *generation) addRecord(value retainedRecord) (recordID, bool) {
	if !g.mutable("append a record") {
		return 0, false
	}
	if value.Header.ID != 0 || !g.validSyntax(value.Header.Syntax) || !g.validRecordSpan(value.Header.Syntax, value.Header.Span) || (value.Header.Owner != 0 && !g.validSymbol(value.Header.Owner)) {
		g.report("semantic record contains an invalid or foreign header", value.Header.Span)
		return 0, false
	}
	if value.Binding != nil && !g.validSymbol(value.Binding.Symbol) {
		g.report("binding record contains an invalid or foreign symbol", value.Header.Span)
		return 0, false
	}
	if value.Callable != nil {
		if value.Callable.Symbol != 0 && !g.validSymbol(value.Callable.Symbol) {
			g.report("callable record contains an invalid or foreign symbol", value.Header.Span)
			return 0, false
		}
		for _, capture := range value.Callable.Captures {
			if !g.validSymbol(capture) {
				g.report("callable record contains an invalid or foreign capture", value.Header.Span)
				return 0, false
			}
		}
	}
	if value.UnsupportedCallable != nil {
		for _, parameter := range value.UnsupportedCallable.TypeParameters {
			if !g.validSyntax(parameter) {
				g.report("unsupported callable record contains invalid type-parameter syntax", value.Header.Span)
				return 0, false
			}
		}
	}
	if value.ContextFlow != nil {
		zeroSuppressedExpression := value.ContextFlow.Kind == contextExpression && value.ContextFlow.Header.Suppressed && value.ContextFlow.Context == 0 && value.ContextFlow.Callee == 0
		if value.ContextFlow.Context == 0 && !zeroSuppressedExpression {
			g.report("context-flow record contains an invalid zero runtime type", value.Header.Span)
			return 0, false
		}
		if value.ContextFlow.Context != 0 {
			if _, ok := g.inputs.Types.Key(value.ContextFlow.Context); !ok {
				g.report("context-flow record contains a foreign runtime type", value.Header.Span)
				return 0, false
			}
		}
		if value.ContextFlow.Caller.Symbol != 0 && !g.validSymbol(value.ContextFlow.Caller.Symbol) {
			g.report("context-flow record contains an invalid caller", value.Header.Span)
			return 0, false
		}
		if value.ContextFlow.Caller.Syntax != (symbol.SyntaxRef{}) && !g.validSyntax(value.ContextFlow.Caller.Syntax) {
			g.report("context-flow record contains invalid caller syntax", value.Header.Span)
			return 0, false
		}
	}
	if value.Expression != nil {
		if value.Expression.Symbol != 0 && !g.validSymbol(value.Expression.Symbol) {
			g.report("expression record contains an invalid symbol", value.Header.Span)
			return 0, false
		}
		if value.Expression.Specialized != 0 && uint64(value.Expression.Specialized) > uint64(len(g.records.values)) {
			g.report("expression record contains an invalid specialized join", value.Header.Span)
			return 0, false
		}
		if value.Expression.Specialized != 0 {
			joined := g.records.values[value.Expression.Specialized-1]
			if joined.Header.Syntax != value.Header.Syntax || joined.Header.Alternative != value.Header.Alternative || joined.Header.Owner != value.Header.Owner {
				g.report("expression record contains a mismatched specialized join", value.Header.Span)
				return 0, false
			}
		}
	}
	if value.Aggregate != nil {
		if value.Aggregate.Declaration != 0 && !g.validSymbol(value.Aggregate.Declaration) {
			g.report("aggregate record contains an invalid declaration", value.Header.Span)
			return 0, false
		}
		for _, field := range value.Aggregate.Fields {
			if !g.validSyntax(field.Field) || !g.validSyntax(field.NameSyntax) || !g.validRecordSpan(field.NameSyntax, field.NameSpan) || (field.Member != 0 && !g.validSymbol(field.Member)) {
				g.report("aggregate record contains invalid field evidence", value.Header.Span)
				return 0, false
			}
		}
		for _, field := range value.Aggregate.DeclarationFields {
			if !g.validSymbol(field) {
				g.report("aggregate record contains an invalid declaration field", value.Header.Span)
				return 0, false
			}
		}
	}
	if value.Compatibility != nil {
		if value.Compatibility.DestinationSymbol != 0 && !g.validSymbol(value.Compatibility.DestinationSymbol) {
			g.report("compatibility record contains an invalid destination symbol", value.Header.Span)
			return 0, false
		}
	}
	if value.Call != nil {
		if value.Call.Target.Symbol != 0 && !g.validSymbol(value.Call.Target.Symbol) {
			g.report("call record contains an invalid target symbol", value.Header.Span)
			return 0, false
		}
		if value.Call.Target.Site != (symbol.SyntaxRef{}) && !g.validSyntax(value.Call.Target.Site) {
			g.report("call record contains an invalid target site", value.Header.Span)
			return 0, false
		}
		if value.Call.Target.Kind == callDirect && value.Call.Target.Symbol != 0 {
			if target, ok := g.inputs.Resolution.Symbols.Symbol(value.Call.Target.Symbol); !ok || target.Generic != (value.Call.Target.Site != (symbol.SyntaxRef{})) {
				g.report("call record has an inexact generic application site", value.Header.Span)
				return 0, false
			}
		}
	}
	if value.Operator != nil && value.Operator.GenericOwner != 0 && !g.validSymbol(value.Operator.GenericOwner) {
		g.report("operator record contains an invalid generic owner", value.Header.Span)
		return 0, false
	}
	if value.Assignment != nil && !g.validSyntax(value.Assignment.Statement) {
		g.report("assignment record contains invalid statement syntax", value.Header.Span)
		return 0, false
	}
	if value.Assignment != nil {
		moduleValue, moduleOK := g.inputs.Graph.Module(value.Assignment.Statement.Module)
		node, nodeOK := moduleValue.Tree.Node(value.Assignment.Statement.Node)
		if !moduleOK || !nodeOK || node.Kind() != syntax.AssignmentStmt && node.Kind() != syntax.PostfixExpr {
			g.report("assignment record does not name an assignment occurrence", value.Header.Span)
			return 0, false
		}
	}
	if value.Place != nil {
		if value.Place.Root != 0 && !g.validSymbol(value.Place.Root) {
			g.report("place record contains an invalid root", value.Header.Span)
			return 0, false
		}
		if value.Place.Root != 0 {
			root, _ := g.inputs.Resolution.Symbols.Symbol(value.Place.Root)
			if root.Kind != value.Place.RootKind {
				g.report("place record root kind does not match its symbol", value.Header.Span)
				return 0, false
			}
			module, moduleOK := g.inputs.Graph.Module(root.Declaration.Module)
			if !moduleOK {
				g.report("place record root declaration is not owned by the module graph", value.Header.Span)
				return 0, false
			}
			declaration, declarationOK := module.Tree.Node(root.Declaration.Node)
			if !declarationOK {
				g.report("place record root declaration is invalid", value.Header.Span)
				return 0, false
			}
			mutable := (declaration.Kind() == syntax.BindingDecl || declaration.Kind() == syntax.ExternBinding) && declaration.Token() == syntax.KwVar
			if value.Place.RootMutable != mutable {
				g.report("place record root mutability does not match its declaration", value.Header.Span)
				return 0, false
			}
		}
		for _, projection := range value.Place.Projections {
			if projection.Member != 0 && !g.validSymbol(projection.Member) {
				g.report("place record contains an invalid member", value.Header.Span)
				return 0, false
			}
		}
	}
	if value.Index != nil && value.Index.EscapeDestination != 0 && !g.validSymbol(value.Index.EscapeDestination) {
		g.report("index record contains an invalid escape destination", value.Header.Span)
		return 0, false
	}
	if value.Member != nil {
		if value.Member.Member != 0 && !g.validSymbol(value.Member.Member) {
			g.report("member record contains an invalid member symbol", value.Header.Span)
			return 0, false
		}
		if !g.validSyntax(value.Member.Header.Syntax) || !g.validRecordSpan(value.Member.Header.Syntax, value.Member.NameSpan) {
			g.report("member record contains invalid name evidence", value.Header.Span)
			return 0, false
		}
	}
	id, ok := g.records.append(value, g.hasValue, g.hasControl, g.config.MaxSemanticRecords, g.config.MaxRecordComponents)
	if !ok {
		g.report("invalid, foreign, or over-limit semantic record", value.Header.Span)
	}
	return id, ok
}

func (g *generation) hasControl(id controlID) bool {
	return g != nil && id.valid() && uint64(id) <= uint64(len(g.controls.values))
}

func (g *generation) addControl(parent controlID) (controlID, bool) {
	if !g.mutable("append a control region") {
		return 0, false
	}
	id, ok := g.controls.append(parent, g.config.MaxControlDepth, g.config.MaxSemanticRecords)
	if !ok {
		g.report("invalid, foreign, or over-limit control region", source.Span{})
	}
	return id, ok
}

func (g *generation) chargeSyntaxVisit() bool {
	return g.charge32("syntax visit", &g.counters.syntaxVisits, g.config.MaxSyntaxVisits)
}

func (g *generation) enterTraversal() bool {
	return g.charge32("traversal depth", &g.counters.traversalDepth, g.config.MaxTraversalDepth)
}

func (g *generation) leaveTraversal() {
	if g != nil && g.counters.traversalDepth != 0 {
		g.counters.traversalDepth--
	}
}

func (g *generation) trackPlace() bool {
	return g.charge32("tracked place", &g.counters.trackedPlaces, g.config.MaxTrackedPlaces)
}

func (g *generation) addGenericRequirement() bool {
	return g.charge32("generic requirement", &g.counters.genericRequirements, g.config.MaxGenericRequirements)
}

func (g *generation) enterConstant() bool {
	return g.charge32("constant depth", &g.counters.constantDepth, g.config.MaxConstantDepth)
}

func (g *generation) leaveConstant() {
	if g != nil && g.counters.constantDepth != 0 {
		g.counters.constantDepth--
	}
}

func (g *generation) chargeConstantOperations(count uint64) bool {
	if !g.mutable("charge constant operations") {
		return false
	}
	if count > g.config.MaxConstantOperations || g.counters.constantOperations > g.config.MaxConstantOperations-count {
		g.reportLimit("constant operation", g.config.MaxConstantOperations)
		return false
	}
	g.counters.constantOperations += count
	return true
}

func (g *generation) constantBitsAllowed(bits uint32) bool {
	if !g.mutable("check constant bits") {
		return false
	}
	if bits > g.config.MaxConstantBits {
		g.reportLimit("constant bits", uint64(g.config.MaxConstantBits))
		return false
	}
	return true
}

func (g *generation) charge32(name string, counter *uint32, limit uint32) bool {
	if !g.mutable("charge " + name) {
		return false
	}
	if *counter >= limit {
		g.reportLimit(name, uint64(limit))
		return false
	}
	*counter++
	return true
}

func (g *generation) freeze() (frozenGeneration, bool) {
	if g == nil {
		return frozenGeneration{}, false
	}
	if g.state != generationMutable {
		g.report("generation is already frozen", source.Span{})
		g.reporter.flush()
		return frozenGeneration{}, false
	}
	g.state = generationFrozen
	controls, controlsOK := g.controls.freeze(g.config.MaxSemanticRecords)
	if !controlsOK {
		g.report("control hierarchy is inconsistent or exceeds its resource bound", source.Span{})
		g.reporter.flush()
		return frozenGeneration{}, false
	}
	values := append([]generatedValue(nil), g.values...)
	records := make([]retainedRecord, len(g.records.values))
	for index := range g.records.values {
		records[index] = cloneRetainedRecord(g.records.values[index])
	}
	frozen := frozenGeneration{
		inputs: g.inputs, values: values,
		roots:    frozenRoots{values: cloneRootedValues(g.roots.values)},
		records:  frozenRecords{values: records, controls: controls, components: g.records.components},
		counters: g.counters,
	}
	g.reporter.flush()
	return frozen, true
}

func (f frozenGeneration) Values() []generatedValue {
	return append([]generatedValue(nil), f.values...)
}

func (g *generation) reportLimit(resource string, limit uint64) {
	g.report(fmt.Sprintf("%s limit of %d exceeded", resource, limit), source.Span{})
}

func (g *generation) report(message string, span source.Span) {
	if g != nil && g.reporter != nil {
		g.reporter.add(diagnostic.Diagnostic{Severity: diagnostic.Error, Code: CodeGeneration, Message: message, Primary: diagnostic.Label{Span: span}})
	}
}

func (g *generation) validateSnapshot() {
	if g.inputs.Graph == nil || g.inputs.Sources == nil || g.inputs.Resolution == nil || g.inputs.Types == nil {
		g.report("checker generation requires graph, sources, resolution, and type store", source.Span{})
		return
	}
	if g.inputs.Resolution.Symbols == nil || g.inputs.Resolution.Scopes == nil {
		g.report("checker generation requires complete resolution stores", source.Span{})
		return
	}
	if g.inputs.LiteralTarget.WordBits != 32 && g.inputs.LiteralTarget.WordBits != 64 {
		g.report("literal target word width must be 32 or 64", source.Span{})
	}
	modules := g.inputs.Graph.Modules()
	if len(modules) == 0 || g.inputs.Graph.Root == 0 {
		g.report("checker generation requires a rooted module graph", source.Span{})
	}
	for index, item := range modules {
		if item.ID != module.ModuleID(index+1) || item.Tree == nil {
			g.report("module graph contains an inconsistent module", source.Span{})
			continue
		}
		file, fileOK := g.inputs.Sources.File(item.Source)
		root, rootOK := item.Tree.Node(item.Tree.Root())
		if !fileOK || !rootOK || root.Kind() != syntax.File || root.Span().Source != item.Source || root.Span().End > file.Len() {
			g.report("module source and syntax tree are inconsistent", source.Span{Source: item.Source})
		}
		for _, edge := range item.Imports {
			if _, ok := g.inputs.Graph.Module(edge.Target); !ok || edge.Span.Source != item.Source || edge.Span.End < edge.Span.Start || (fileOK && edge.Span.End > file.Len()) {
				g.report("module graph contains an inconsistent import edge", edge.Span)
			}
		}
	}
	for _, value := range g.inputs.Resolution.References() {
		if !g.validSyntax(value.Syntax) || value.State < symbol.ResolutionResolved || value.State > symbol.ResolutionDeferred || (value.Symbol != 0 && !g.validSymbol(value.Symbol)) {
			g.report("resolution contains an inconsistent reference", source.Span{})
		}
	}
	for _, value := range g.inputs.Resolution.CaptureList() {
		if !g.validSyntax(value.Function) || !g.validSymbol(value.Symbol) {
			g.report("resolution contains an inconsistent capture", source.Span{})
		}
	}
	for _, value := range g.inputs.Resolution.Symbols.All() {
		if value.ID == 0 || !g.validSymbol(value.ID) || (value.Module != 0 && !g.validModule(value.Module)) || (value.Declaration != (symbol.SyntaxRef{}) && !g.validSyntax(value.Declaration)) {
			g.report("resolution contains an inconsistent symbol", value.Span)
		}
	}
	for _, value := range g.inputs.Resolution.Scopes.All() {
		if value.ID == 0 || (value.Module != 0 && !g.validModule(value.Module)) || (value.Owner != 0 && !g.validSymbol(value.Owner)) || (value.Origin != (symbol.SyntaxRef{}) && !g.validSyntax(value.Origin)) {
			g.report("resolution contains an inconsistent scope", source.Span{})
		}
	}
	if g.inputs.Types.Len() < 16 {
		g.report("semantic type store is missing required builtins", source.Span{})
	}
}

func (g *generation) validModule(id module.ModuleID) bool {
	if g == nil || g.inputs.Graph == nil {
		return false
	}
	_, ok := g.inputs.Graph.Module(id)
	return ok
}

func (g *generation) validSyntax(ref symbol.SyntaxRef) bool {
	if g == nil || ref.Module == 0 || ref.Node == 0 || g.inputs.Graph == nil {
		return false
	}
	item, ok := g.inputs.Graph.Module(ref.Module)
	if !ok || item.Tree == nil {
		return false
	}
	_, ok = item.Tree.Node(ref.Node)
	return ok
}

func (g *generation) validSymbol(id symbol.SymbolID) bool {
	if g == nil || id == 0 || g.inputs.Resolution == nil || g.inputs.Resolution.Symbols == nil {
		return false
	}
	_, ok := g.inputs.Resolution.Symbols.Symbol(id)
	return ok
}

func (g *generation) validRecordSpan(ref symbol.SyntaxRef, span source.Span) bool {
	if g == nil || g.inputs.Graph == nil || span.Start > span.End {
		return false
	}
	item, ok := g.inputs.Graph.Module(ref.Module)
	if !ok || item.Source != span.Source || g.inputs.Sources == nil {
		return false
	}
	file, ok := g.inputs.Sources.File(item.Source)
	return ok && span.End <= file.Len()
}

type generationReporter struct {
	budget *generationDiagnosticBudget
}

func newGenerationReporter(set *diagnostic.DiagnosticSet, max uint32) *generationReporter {
	return &generationReporter{budget: newGenerationDiagnosticBudget(set, max)}
}

func (r *generationReporter) add(value diagnostic.Diagnostic) {
	if r != nil {
		r.budget.add(value)
	}
}

func (r *generationReporter) flush() {}

type generationDiagnosticBudget struct {
	set           *diagnostic.DiagnosticSet
	max           uint32
	count         uint32
	lastIndex     int
	lastPrimary   diagnostic.Label
	hasDiagnostic bool
	overflow      bool
}

func newGenerationDiagnosticBudget(set *diagnostic.DiagnosticSet, max uint32) *generationDiagnosticBudget {
	if set == nil {
		set = diagnostic.NewDiagnosticSet()
	}
	return &generationDiagnosticBudget{set: set, max: max, lastIndex: -1}
}

func (b *generationDiagnosticBudget) add(value diagnostic.Diagnostic) bool {
	if b == nil || b.overflow {
		return false
	}
	if b.count >= b.max {
		if b.hasDiagnostic {
			b.set.Replace(b.lastIndex, diagnostic.Diagnostic{
				Severity: diagnostic.Error, Code: CodeGeneration,
				Message: fmt.Sprintf("generation diagnostic limit of %d reached", b.max),
				Primary: b.lastPrimary,
			})
		}
		b.overflow = true
		return false
	}
	b.lastIndex = b.set.Len()
	b.lastPrimary = value.Primary
	b.set.Add(value)
	b.hasDiagnostic = true
	b.count++
	return true
}
