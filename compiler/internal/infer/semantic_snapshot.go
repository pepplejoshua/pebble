package infer

import (
	"fmt"
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// Keep program identities non-zero-sized so distinct allocations cannot share
// an address. The token carries identity only and has no route back to Program.
type programToken struct{ _ byte }

// SemanticSnapshot is the immutable, tree-free semantic view published after
// one solve. All variable-length records are independently owned.
type SemanticSnapshot struct {
	types           *types.Snapshot
	resolution      *symbol.Result
	programIdentity *programToken
	solveIdentity   *sessionToken
	storeLength     uint32
	templates       []TypeTemplate
	declarations    map[symbol.SymbolID]TypeDeclaration
	declarationIDs  []symbol.SymbolID
	signatures      map[symbol.SymbolID]Signature
	signatureIDs    []symbol.SymbolID
	typeParams      map[symbol.SymbolID]types.TypeID
	typeParamIDs    []symbol.SymbolID
	owners          map[symbol.SymbolID][]symbol.SymbolID
	ownerIDs        []symbol.SymbolID
	runtimeTypes    RuntimeTypes
	runtimeReady    bool
}

// Snapshot validates and copies the semantic state belonging to one exact
// finalized solve. Failure is atomic and reported as bounded T0512.
func Snapshot(program *Program, solution *Solution, diagnostics *diagnostic.DiagnosticSet) (*SemanticSnapshot, bool) {
	config := Config{}
	if program != nil {
		config = normalizeConfig(program.config)
	} else {
		config = normalizeConfig(config)
	}
	report := newReporter(diagnostics, config.MaxDiagnostics)
	fail := func(message string) (*SemanticSnapshot, bool) {
		report.error(CodeResourceLimit, message, Origin{Role: "semantic snapshot"})
		report.flush()
		return nil, false
	}

	if program == nil || !program.valid || program.inputs.Types == nil || program.inputs.Resolution == nil || program.inputs.Resolution.Symbols == nil || program.inputs.Resolution.Scopes == nil {
		return fail("semantic snapshot requires a valid prepared program")
	}
	identity := program.identity
	if identity == nil || solution == nil || !solution.finalized || solution.programIdentity != identity || solution.solveIdentity == nil {
		return fail("semantic snapshot requires the exact finalized program solution")
	}
	if program.storeMu == nil {
		return fail("semantic snapshot requires serialized store ownership")
	}

	program.storeMu.Lock()
	if program.inputs.Types.Len() != solution.storeLength {
		program.storeMu.Unlock()
		return fail("semantic snapshot store length does not match solve finalization")
	}
	typeSnapshot, err := program.inputs.Types.Snapshot()
	program.storeMu.Unlock()
	if err != nil {
		return fail(fmt.Sprintf("semantic type snapshot failed: %v", err))
	}
	if typeSnapshot.Len() != solution.storeLength {
		return fail("semantic type snapshot length does not match solve finalization")
	}

	b := semanticSnapshotBuilder{
		program: program, solution: solution, typeSnapshot: typeSnapshot,
		maxSteps: config.MaxDecompositionSteps, maxWidth: config.MaxShapeComponents,
	}
	snapshot, err := b.build()
	if err != nil {
		return fail(err.Error())
	}
	return snapshot, true
}

func (s *SemanticSnapshot) Types() *types.Snapshot {
	if s == nil {
		return nil
	}
	return s.types
}

func (s *SemanticSnapshot) Resolution() *symbol.Result {
	if s == nil {
		return nil
	}
	return s.resolution
}

func (s *SemanticSnapshot) TypeDeclaration(id symbol.SymbolID) (TypeDeclaration, bool) {
	if s == nil {
		return TypeDeclaration{}, false
	}
	v, ok := s.declarations[id]
	return cloneDeclaration(v), ok
}

func (s *SemanticSnapshot) TypeDeclarations() []TypeDeclaration {
	if s == nil {
		return nil
	}
	out := make([]TypeDeclaration, 0, len(s.declarationIDs))
	for _, id := range s.declarationIDs {
		out = append(out, cloneDeclaration(s.declarations[id]))
	}
	return out
}

func (s *SemanticSnapshot) Signature(id symbol.SymbolID) (Signature, bool) {
	if s == nil {
		return Signature{}, false
	}
	v, ok := s.signatures[id]
	return cloneSignature(v), ok
}

func (s *SemanticSnapshot) Signatures() []Signature {
	if s == nil {
		return nil
	}
	out := make([]Signature, 0, len(s.signatureIDs))
	for _, id := range s.signatureIDs {
		out = append(out, cloneSignature(s.signatures[id]))
	}
	return out
}

func (s *SemanticSnapshot) Template(id TemplateID) (TypeTemplate, bool) {
	if s == nil || !id.IsValid() || uint64(id) > uint64(len(s.templates)) {
		return TypeTemplate{}, false
	}
	v := s.templates[id-1]
	v.Children = append([]TemplateID(nil), v.Children...)
	return v, true
}

func (s *SemanticSnapshot) Templates() []TypeTemplate {
	if s == nil {
		return nil
	}
	out := make([]TypeTemplate, len(s.templates))
	for i, v := range s.templates {
		out[i] = v
		out[i].Children = append([]TemplateID(nil), v.Children...)
	}
	return out
}

func (s *SemanticSnapshot) RuntimeTypes() (RuntimeTypes, bool) {
	if s == nil || !s.runtimeReady {
		return RuntimeTypes{}, false
	}
	return s.runtimeTypes, true
}

func (s *SemanticSnapshot) TypeParameter(id symbol.SymbolID) (types.TypeID, bool) {
	if s == nil {
		return 0, false
	}
	v, ok := s.typeParams[id]
	return v, ok
}

func (s *SemanticSnapshot) OwnerParameters(id symbol.SymbolID) []symbol.SymbolID {
	if s == nil {
		return nil
	}
	return append([]symbol.SymbolID(nil), s.owners[id]...)
}

func (s *SemanticSnapshot) Matches(solution *Solution) bool {
	return s != nil && solution != nil && solution.finalized &&
		s.programIdentity != nil && s.programIdentity == solution.programIdentity &&
		s.solveIdentity != nil && s.solveIdentity == solution.solveIdentity &&
		s.types != nil && s.storeLength == solution.storeLength && s.types.Len() == solution.storeLength
}

type semanticSnapshotBuilder struct {
	program      *Program
	solution     *Solution
	typeSnapshot *types.Snapshot
	steps        uint64
	maxSteps     uint64
	maxWidth     uint32
	sawTypeError bool
}

func (b *semanticSnapshotBuilder) validateResolution() error {
	resolution := b.program.inputs.Resolution
	references := resolution.References()
	if err := b.validateWidth(len(references)); err != nil {
		return err
	}
	var previous symbol.SyntaxRef
	for index, value := range references {
		if index != 0 && !refLess(previous, value.Syntax) {
			return fmt.Errorf("semantic snapshot resolver references are not deterministic")
		}
		previous = value.Syntax
		exact, ok := resolution.Reference(value.Syntax)
		if !ok || exact != value || !b.validSyntax(value.Syntax) || !b.validResolutionValue(value) {
			return fmt.Errorf("semantic snapshot resolver reference is invalid")
		}
	}

	captures := resolution.CaptureList()
	if err := b.validateWidth(len(captures)); err != nil {
		return err
	}
	seenFunctions := make(map[symbol.SyntaxRef]struct{})
	for index := 0; index < len(captures); {
		function := captures[index].Function
		if !b.validSyntax(function) {
			return fmt.Errorf("semantic snapshot resolver capture function is invalid")
		}
		if _, duplicate := seenFunctions[function]; duplicate {
			return fmt.Errorf("semantic snapshot resolver capture groups are duplicated")
		}
		seenFunctions[function] = struct{}{}
		end := index
		var expected []symbol.SymbolID
		seenSymbols := make(map[symbol.SymbolID]struct{})
		for end < len(captures) && captures[end].Function == function {
			id := captures[end].Symbol
			if !b.validSymbol(id) {
				return fmt.Errorf("semantic snapshot resolver capture symbol is invalid")
			}
			if _, duplicate := seenSymbols[id]; duplicate {
				return fmt.Errorf("semantic snapshot resolver capture is duplicated")
			}
			seenSymbols[id] = struct{}{}
			expected = append(expected, id)
			end++
		}
		if !equalSymbolIDs(expected, resolution.Captures(function)) {
			return fmt.Errorf("semantic snapshot resolver capture order is inconsistent")
		}
		index = end
	}

	for _, owner := range resolution.Symbols.All() {
		members := resolution.Members(owner.ID)
		if err := b.validateWidth(len(members)); err != nil {
			return err
		}
		seen := make(map[symbol.SymbolID]struct{}, len(members))
		for _, id := range members {
			member, ok := resolution.Symbols.Symbol(id)
			if !ok || member.ID != id || member.Containing != owner.ID || (member.Kind != symbol.SymbolField && member.Kind != symbol.SymbolVariant && member.Kind != symbol.SymbolMethod) {
				return fmt.Errorf("semantic snapshot resolver member identity is invalid")
			}
			if _, duplicate := seen[id]; duplicate {
				return fmt.Errorf("semantic snapshot resolver member identity is duplicated")
			}
			seen[id] = struct{}{}
		}
	}

	if err := b.charge(uint64(len(b.program.modules))); err != nil {
		return err
	}
	moduleIDs := make([]module.ModuleID, 0, len(b.program.modules))
	for id := range b.program.modules {
		moduleIDs = append(moduleIDs, id)
	}
	sort.Slice(moduleIDs, func(i, j int) bool { return moduleIDs[i] < moduleIDs[j] })
	for _, id := range moduleIDs {
		item := b.program.modules[id]
		if item.Tree == nil {
			return fmt.Errorf("semantic snapshot resolver module lacks syntax")
		}
		root := item.Tree.Root()
		if err := b.charge(uint64(root)); err != nil {
			return err
		}
		for rawNode := uint64(1); rawNode <= uint64(root); rawNode++ {
			node := syntax.NodeID(rawNode)
			ref := symbol.SyntaxRef{Module: id, Node: node}
			if !b.validResolverRoles(ref) {
				return fmt.Errorf("semantic snapshot resolver role is invalid")
			}
		}
	}
	return nil
}

func (b *semanticSnapshotBuilder) validResolutionValue(value symbol.Resolution) bool {
	switch value.State {
	case symbol.ResolutionResolved:
		return b.validSymbol(value.Symbol)
	case symbol.ResolutionError, symbol.ResolutionDeferred:
		return value.Symbol == 0 || b.validSymbol(value.Symbol)
	default:
		return false
	}
}

func (b *semanticSnapshotBuilder) validResolverRoles(ref symbol.SyntaxRef) bool {
	if ref == (symbol.SyntaxRef{}) {
		return true
	}
	resolution := b.program.inputs.Resolution
	if value, ok := resolution.Reference(ref); ok && (value.Syntax != ref || !b.validResolutionValue(value)) {
		return false
	}
	if target, ok := resolution.Qualifier(ref); ok {
		if target == 0 {
			return false
		}
		if _, exists := b.program.modules[module.ModuleID(target)]; !exists {
			return false
		}
	}
	if mode, ok := resolution.Bracket(ref); ok && (mode < symbol.BracketDeferred || mode > symbol.BracketValueNames) {
		return false
	}
	seen := make(map[symbol.SymbolID]struct{})
	for _, id := range resolution.Captures(ref) {
		if !b.validSymbol(id) {
			return false
		}
		if _, duplicate := seen[id]; duplicate {
			return false
		}
		seen[id] = struct{}{}
	}
	return true
}

func (b *semanticSnapshotBuilder) build() (*SemanticSnapshot, error) {
	if err := b.validateResolution(); err != nil {
		return nil, err
	}
	if err := b.validateSolution(); err != nil {
		return nil, err
	}
	if err := b.validateWidth(len(b.program.templates)); err != nil {
		return nil, err
	}
	for _, count := range []int{len(b.program.declarations), len(b.program.signatures), len(b.program.typeParams), len(b.program.owners)} {
		if err := b.validateWidth(count); err != nil {
			return nil, err
		}
	}

	result := &SemanticSnapshot{
		types: b.typeSnapshot, resolution: b.program.inputs.Resolution,
		programIdentity: b.solution.programIdentity, solveIdentity: b.solution.solveIdentity,
		storeLength:  b.solution.storeLength,
		declarations: make(map[symbol.SymbolID]TypeDeclaration, len(b.program.declarations)),
		signatures:   make(map[symbol.SymbolID]Signature, len(b.program.signatures)),
		typeParams:   make(map[symbol.SymbolID]types.TypeID, len(b.program.typeParams)),
		owners:       make(map[symbol.SymbolID][]symbol.SymbolID, len(b.program.owners)),
		runtimeTypes: b.program.runtimeTypes, runtimeReady: b.program.runtimeReady,
	}

	result.templates = make([]TypeTemplate, len(b.program.templates))
	for index, value := range b.program.templates {
		if err := b.validateTemplate(index, value); err != nil {
			return nil, err
		}
		result.templates[index] = value
		result.templates[index].Children = append([]TemplateID(nil), value.Children...)
	}

	result.declarationIDs = sortedMapSymbolIDs(b.program.declarations)
	for _, id := range result.declarationIDs {
		value := b.program.declarations[id]
		if err := b.validateDeclaration(id, value); err != nil {
			return nil, err
		}
		result.declarations[id] = cloneDeclaration(value)
	}

	result.signatureIDs = sortedMapSymbolIDs(b.program.signatures)
	for _, id := range result.signatureIDs {
		value := b.program.signatures[id]
		if err := b.validateSignature(id, value); err != nil {
			return nil, err
		}
		result.signatures[id] = cloneSignature(value)
	}

	result.typeParamIDs = sortedMapSymbolIDs(b.program.typeParams)
	for _, id := range result.typeParamIDs {
		typeID := b.program.typeParams[id]
		if !b.validSymbol(id) || !b.typeSnapshot.Contains(typeID) {
			return nil, fmt.Errorf("semantic snapshot has an invalid type-parameter binding")
		}
		key, _ := b.typeSnapshot.Key(typeID)
		parameter, ok := key.TypeParameter()
		if !ok || parameter != id {
			return nil, fmt.Errorf("semantic snapshot type-parameter identity is inconsistent")
		}
		result.typeParams[id] = typeID
	}

	result.ownerIDs = sortedMapSymbolIDs(b.program.owners)
	for _, id := range result.ownerIDs {
		parameters := b.program.owners[id]
		if !b.validSymbol(id) {
			return nil, fmt.Errorf("semantic snapshot has an invalid owner")
		}
		if err := b.validateWidth(len(parameters)); err != nil {
			return nil, err
		}
		seen := make(map[symbol.SymbolID]struct{}, len(parameters))
		for _, parameter := range parameters {
			if !b.validSymbol(parameter) {
				return nil, fmt.Errorf("semantic snapshot has an invalid owner parameter")
			}
			value, _ := b.program.inputs.Resolution.Symbols.Symbol(parameter)
			if value.Kind != symbol.SymbolTypeParameter {
				return nil, fmt.Errorf("semantic snapshot owner parameter has the wrong symbol kind")
			}
			if _, ok := b.program.typeParams[parameter]; !ok {
				return nil, fmt.Errorf("semantic snapshot owner parameter has no rigid type binding")
			}
			if _, duplicate := seen[parameter]; duplicate {
				return nil, fmt.Errorf("semantic snapshot owner parameter is duplicated")
			}
			seen[parameter] = struct{}{}
		}
		result.owners[id] = append([]symbol.SymbolID(nil), parameters...)
	}

	if err := b.validateOwnerJoins(); err != nil {
		return nil, err
	}
	if err := b.validateRuntime(); err != nil {
		return nil, err
	}
	return result, nil
}

func (b *semanticSnapshotBuilder) validateSolution() error {
	r := b.solution
	if r == nil || !r.finalized || r.programIdentity != b.program.identity || r.solveIdentity == nil || r.storeLength != b.typeSnapshot.Len() {
		return fmt.Errorf("semantic snapshot solution ownership is inconsistent")
	}
	if len(r.symbols) != len(r.manifest.symbols) || len(r.syntax) != len(r.manifest.syntax) || len(r.requirements) != len(r.manifest.requirements) || len(r.instantiations) != len(r.manifest.instantiations) || len(r.methods) != len(r.manifest.methods) || len(r.selections) != len(r.manifest.selections) || len(r.slots) != len(r.manifest.slots) || len(r.orderedSlots) != len(r.manifest.slots) {
		return fmt.Errorf("semantic snapshot solution tables are incomplete")
	}
	if !strictlyIncreasingSymbols(r.manifest.symbols) || !strictlyIncreasingRefs(r.manifest.syntax) || !strictlyIncreasingRequirementOwners(r.manifest.requirements) || !strictlyIncreasingRefs(r.manifest.instantiations) || !strictlyIncreasingRefs(r.manifest.methods) || !strictlyIncreasingSelections(r.manifest.selections) {
		return fmt.Errorf("semantic snapshot solution table order is invalid")
	}

	for _, id := range r.manifest.symbols {
		if err := b.charge(1); err != nil {
			return err
		}
		value, ok := r.symbols[id]
		if !ok || !b.validSymbol(id) || !b.validTypeResult(value) {
			return fmt.Errorf("semantic snapshot symbol table is invalid")
		}
	}
	for _, ref := range r.manifest.syntax {
		if err := b.charge(1); err != nil {
			return err
		}
		value, ok := r.syntax[ref]
		if !ok || !b.validSyntax(ref) || !b.validResolverRoles(ref) || !b.validTypeResult(value) {
			return fmt.Errorf("semantic snapshot syntax table is invalid")
		}
	}
	for _, manifest := range r.manifest.requirements {
		values, ok := r.requirements[manifest.owner]
		if !ok || uint64(manifest.count) != uint64(len(values)) || !b.validSymbol(manifest.owner) {
			return fmt.Errorf("semantic snapshot requirement table is incomplete")
		}
		if err := b.validateWidth(len(values)); err != nil {
			return err
		}
		seen := make(map[semanticRequirementKey]struct{}, len(values))
		for index, value := range values {
			if index != 0 && requirementLess(value, values[index-1]) {
				return fmt.Errorf("semantic snapshot requirement order is invalid")
			}
			if err := b.validateRequirement(manifest.owner, value); err != nil {
				return err
			}
			key := semanticKey(value)
			if _, duplicate := seen[key]; duplicate {
				return fmt.Errorf("semantic snapshot requirement is duplicated")
			}
			seen[key] = struct{}{}
		}
	}
	for _, ref := range r.manifest.instantiations {
		if err := b.charge(1); err != nil {
			return err
		}
		value, ok := r.instantiations[ref]
		if !ok || value.Site != ref || !b.validSyntax(ref) || !b.validSymbol(value.Generic) {
			return fmt.Errorf("semantic snapshot instantiation table is invalid")
		}
		generic, _ := b.program.inputs.Resolution.Symbols.Symbol(value.Generic)
		if !generic.Generic || (generic.Kind != symbol.SymbolType && generic.Kind != symbol.SymbolFunction && generic.Kind != symbol.SymbolMethod) {
			return fmt.Errorf("semantic snapshot instantiation target is not generic")
		}
		if err := b.validateWidth(len(value.Arguments)); err != nil {
			return err
		}
		expected, ok := b.genericParameters(value.Generic)
		if !ok || len(value.Arguments) != len(expected) || !b.validResolverRoles(ref) {
			return fmt.Errorf("semantic snapshot instantiation arguments do not match the generic declaration")
		}
		for _, argument := range value.Arguments {
			if !b.validTypeResult(argument) {
				return fmt.Errorf("semantic snapshot instantiation argument is invalid")
			}
		}
	}
	for _, ref := range r.manifest.methods {
		if err := b.charge(1); err != nil {
			return err
		}
		value, ok := r.methods[ref]
		if !ok || value.Site != ref || !b.validSyntax(ref) || !b.validSymbol(value.Method) {
			return fmt.Errorf("semantic snapshot method table is invalid")
		}
		method, _ := b.program.inputs.Resolution.Symbols.Symbol(value.Method)
		if method.Kind != symbol.SymbolMethod {
			return fmt.Errorf("semantic snapshot method selection has the wrong symbol kind")
		}
		if err := b.validateWidth(len(value.Arguments)); err != nil {
			return err
		}
		local, ok := b.methodLocalParameters(value.Method)
		if !ok || len(value.Arguments) != len(local) || !b.validResolverRoles(ref) {
			return fmt.Errorf("semantic snapshot method arguments do not match the selected method")
		}
		for _, argument := range value.Arguments {
			if !b.validTypeResult(argument) {
				return fmt.Errorf("semantic snapshot method argument is invalid")
			}
		}
	}
	for _, manifest := range r.manifest.selections {
		selected, ok := r.selections[manifest.id]
		if !ok || !manifest.id.IsValid() || selected != manifest.alternative || manifest.alternatives == 0 || selected >= manifest.alternatives {
			return fmt.Errorf("semantic snapshot selection table is invalid")
		}
		if err := b.charge(1); err != nil {
			return err
		}
	}
	for index, id := range r.manifest.slots {
		value, ok := r.slots[id]
		if !ok || id.owner != r.solveIdentity || id.ordinal == 0 || !b.validTypeResult(value) {
			return fmt.Errorf("semantic snapshot slot table is invalid")
		}
		ordered := r.orderedSlots[index]
		if ordered.Slot != id || ordered.Result != value {
			return fmt.Errorf("semantic snapshot slot order is invalid")
		}
		if index != 0 && r.manifest.slots[index-1].ordinal >= id.ordinal {
			return fmt.Errorf("semantic snapshot slot order is not deterministic")
		}
		if err := b.charge(1); err != nil {
			return err
		}
	}
	if r.successful && b.sawTypeError {
		return fmt.Errorf("semantic snapshot successful solution contains an error result")
	}
	return nil
}

func (b *semanticSnapshotBuilder) validateRequirement(owner symbol.SymbolID, value Requirement) error {
	if err := b.charge(1); err != nil {
		return err
	}
	if value.Owner != owner || !b.validSymbol(value.Owner) || !b.validSymbol(value.Parameter) || !b.typeSnapshot.Contains(value.Subject) {
		return fmt.Errorf("semantic snapshot requirement identity is invalid")
	}
	if !containsSymbol(b.program.owners[owner], value.Parameter) || value.Origin.GenericOwner != owner {
		return fmt.Errorf("semantic snapshot requirement parameter does not belong to its owner")
	}
	key, _ := b.typeSnapshot.Key(value.Subject)
	parameter, ok := key.TypeParameter()
	if !ok || parameter != value.Parameter {
		return fmt.Errorf("semantic snapshot requirement subject is not its rigid parameter")
	}
	if value.Kind < RequirementNumeric || value.Kind > RequirementLiteralFits {
		return fmt.Errorf("semantic snapshot requirement kind is invalid")
	}
	if !b.validOrigin(value.Origin) || !b.validResolverRoles(value.Origin.Syntax) {
		return fmt.Errorf("semantic snapshot requirement origin is invalid")
	}
	if value.Kind == RequirementLiteralFits {
		if value.LiteralKind < ExactInteger || value.LiteralKind > ExactFloat || !canonicalSignedDecimal(value.Numerator) {
			return fmt.Errorf("semantic snapshot literal requirement is invalid")
		}
		if value.LiteralKind == ExactInteger && value.Denominator != "" {
			return fmt.Errorf("semantic snapshot integer requirement has a denominator")
		}
		if value.LiteralKind == ExactFloat && !canonicalPositiveDecimal(value.Denominator) {
			return fmt.Errorf("semantic snapshot float requirement has an invalid denominator")
		}
	} else if value.LiteralKind != 0 || value.Numerator != "" || value.Denominator != "" {
		return fmt.Errorf("semantic snapshot nonliteral requirement has a literal payload")
	}
	if value.Origin.Syntax == (symbol.SyntaxRef{}) {
		return fmt.Errorf("semantic snapshot requirement lacks a syntax origin")
	}
	return nil
}

type semanticRequirementKey struct {
	owner       symbol.SymbolID
	kind        RequirementKind
	subject     types.TypeID
	literalKind ExactLiteralKind
	numerator   string
	denominator string
}

func semanticKey(value Requirement) semanticRequirementKey {
	key := semanticRequirementKey{owner: value.Owner, kind: value.Kind, subject: value.Subject}
	if value.Kind == RequirementLiteralFits {
		key.literalKind = value.LiteralKind
		key.numerator = value.Numerator
		key.denominator = value.Denominator
	}
	return key
}

func canonicalSignedDecimal(value string) bool {
	if value == "" {
		return false
	}
	start := 0
	if value[0] == '-' {
		start = 1
		if len(value) == 1 || value[1] == '0' {
			return false
		}
	}
	if value[start] == '0' && len(value)-start != 1 {
		return false
	}
	for index := start; index < len(value); index++ {
		if value[index] < '0' || value[index] > '9' {
			return false
		}
	}
	return true
}

func canonicalPositiveDecimal(value string) bool {
	if value == "" || value[0] < '1' || value[0] > '9' {
		return false
	}
	for index := 1; index < len(value); index++ {
		if value[index] < '0' || value[index] > '9' {
			return false
		}
	}
	return true
}

func (b *semanticSnapshotBuilder) validateTemplate(index int, value TypeTemplate) error {
	if value.ID != TemplateID(index+1) {
		return fmt.Errorf("semantic snapshot template IDs are not contiguous")
	}
	if err := b.validateWidth(len(value.Children)); err != nil {
		return err
	}
	for _, child := range value.Children {
		if !child.IsValid() || child >= value.ID || uint64(child) > uint64(len(b.program.templates)) {
			return fmt.Errorf("semantic snapshot template child is invalid")
		}
	}
	zeroScalar := func() bool {
		return value.Known == 0 && value.Parameter == 0 && value.Declaration == 0 && value.Length == 0 && value.Convention == 0 && !value.Variadic
	}
	switch value.Kind {
	case TemplateKnown:
		if !b.typeSnapshot.Contains(value.Known) || value.Parameter != 0 || value.Declaration != 0 || value.Length != 0 || value.Convention != 0 || value.Variadic || len(value.Children) != 0 {
			return fmt.Errorf("semantic snapshot known template is damaged")
		}
	case TemplateParameter:
		parameter, ok := b.program.inputs.Resolution.Symbols.Symbol(value.Parameter)
		if value.Known != 0 || !b.validSymbol(value.Parameter) || !ok || parameter.Kind != symbol.SymbolTypeParameter || value.Declaration != 0 || value.Length != 0 || value.Convention != 0 || value.Variadic || len(value.Children) != 0 {
			return fmt.Errorf("semantic snapshot parameter template is damaged")
		}
	case TemplatePointer, TemplateSlice, TemplateOptional:
		if !zeroScalar() || len(value.Children) != 1 {
			return fmt.Errorf("semantic snapshot unary template is damaged")
		}
	case TemplateArray:
		if value.Known != 0 || value.Parameter != 0 || value.Declaration != 0 || value.Convention != 0 || value.Variadic || len(value.Children) != 1 {
			return fmt.Errorf("semantic snapshot array template is damaged")
		}
	case TemplateTuple:
		if !zeroScalar() || len(value.Children) == 0 {
			return fmt.Errorf("semantic snapshot tuple template is damaged")
		}
	case TemplateFunction:
		if value.Known != 0 || value.Parameter != 0 || value.Declaration != 0 || value.Length != 0 || (value.Convention != types.Pebble && value.Convention != types.C) || len(value.Children) == 0 {
			return fmt.Errorf("semantic snapshot function template is damaged")
		}
	case TemplateNominal:
		declaration, ok := b.program.inputs.Resolution.Symbols.Symbol(value.Declaration)
		if value.Known != 0 || value.Parameter != 0 || !b.validSymbol(value.Declaration) || !ok || (declaration.Kind != symbol.SymbolType && declaration.Kind != symbol.SymbolExternType && declaration.Kind != symbol.SymbolRuntimeType) || value.Length != 0 || value.Convention != 0 || value.Variadic {
			return fmt.Errorf("semantic snapshot nominal template is damaged")
		}
	default:
		return fmt.Errorf("semantic snapshot template tag is invalid")
	}
	return nil
}

func (b *semanticSnapshotBuilder) validateDeclaration(id symbol.SymbolID, value TypeDeclaration) error {
	if value.Symbol != id || !b.validSymbol(id) || (value.State != DeclarationReady && value.State != DeclarationError) {
		return fmt.Errorf("semantic snapshot declaration identity or state is invalid")
	}
	declarationSymbol, _ := b.program.inputs.Resolution.Symbols.Symbol(id)
	if declarationSymbol.Kind != symbol.SymbolType && declarationSymbol.Kind != symbol.SymbolExternType && declarationSymbol.Kind != symbol.SymbolRuntimeType {
		return fmt.Errorf("semantic snapshot declaration has the wrong symbol kind")
	}
	if value.Form != 0 && value.Form != DeclarationNominal && value.Form != DeclarationAlias {
		return fmt.Errorf("semantic snapshot declaration form is invalid")
	}
	if value.Nominal != 0 && (value.Nominal < NominalStruct || value.Nominal > NominalExtern) {
		return fmt.Errorf("semantic snapshot nominal declaration kind is invalid")
	}
	if err := b.validateWidth(len(value.Parameters)); err != nil {
		return err
	}
	if err := b.validateWidth(len(value.Members)); err != nil {
		return err
	}
	for _, parameter := range value.Parameters {
		if !b.validSymbol(parameter) {
			return fmt.Errorf("semantic snapshot declaration parameter is invalid")
		}
		parameterSymbol, _ := b.program.inputs.Resolution.Symbols.Symbol(parameter)
		if parameterSymbol.Kind != symbol.SymbolTypeParameter || parameterSymbol.Containing != id {
			return fmt.Errorf("semantic snapshot declaration parameter ownership is invalid")
		}
	}
	if value.State == DeclarationReady {
		if value.Form != DeclarationNominal && value.Form != DeclarationAlias {
			return fmt.Errorf("semantic snapshot declaration form is invalid")
		}
		if value.Form == DeclarationNominal && (value.Nominal < NominalStruct || value.Nominal > NominalExtern) {
			return fmt.Errorf("semantic snapshot nominal declaration kind is invalid")
		}
		if value.Form == DeclarationAlias && value.Nominal != 0 {
			return fmt.Errorf("semantic snapshot alias has a nominal kind")
		}
		if !value.Template.IsValid() || uint64(value.Template) > uint64(len(b.program.templates)) {
			return fmt.Errorf("semantic snapshot ready declaration lacks a template")
		}
		if len(value.Parameters) == 0 {
			if !b.typeSnapshot.Contains(value.Concrete) {
				return fmt.Errorf("semantic snapshot concrete declaration has an invalid type")
			}
			template := b.program.templates[value.Template-1]
			if template.Kind != TemplateKnown || template.Known != value.Concrete {
				return fmt.Errorf("semantic snapshot concrete declaration template is inconsistent")
			}
		} else if value.Concrete != 0 {
			return fmt.Errorf("semantic snapshot generic declaration has a concrete type")
		}
	} else {
		if value.Concrete != 0 && !b.typeSnapshot.Contains(value.Concrete) {
			return fmt.Errorf("semantic snapshot error declaration has an invalid concrete type")
		}
		if value.Template != 0 && uint64(value.Template) > uint64(len(b.program.templates)) {
			return fmt.Errorf("semantic snapshot error declaration has an invalid template")
		}
	}
	if value.Form == DeclarationAlias && len(value.Members) != 0 {
		return fmt.Errorf("semantic snapshot alias has members")
	}
	expectedMembers := b.declarationMembers(id)
	if len(value.Members) != len(expectedMembers) {
		return fmt.Errorf("semantic snapshot declaration members do not match resolver order")
	}
	for index, member := range value.Members {
		if !b.validSymbol(member.Symbol) || !member.Type.IsValid() || uint64(member.Type) > uint64(len(b.program.templates)) {
			return fmt.Errorf("semantic snapshot member descriptor is invalid")
		}
		if member.Symbol != expectedMembers[index] {
			return fmt.Errorf("semantic snapshot declaration members do not match resolver order")
		}
		sym, _ := b.program.inputs.Resolution.Symbols.Symbol(member.Symbol)
		if sym.Containing != id || (sym.Kind != symbol.SymbolField && sym.Kind != symbol.SymbolVariant) {
			return fmt.Errorf("semantic snapshot member owner is inconsistent")
		}
	}
	return nil
}

func (b *semanticSnapshotBuilder) validateSignature(id symbol.SymbolID, value Signature) error {
	if value.Symbol != id || !b.validSymbol(id) || (value.State != DeclarationReady && value.State != DeclarationError) {
		return fmt.Errorf("semantic snapshot signature identity or state is invalid")
	}
	if err := b.validateWidth(len(value.Parameters)); err != nil {
		return err
	}
	if err := b.validateWidth(len(value.TypeParams)); err != nil {
		return err
	}
	if err := b.validateWidth(len(value.Inputs)); err != nil {
		return err
	}
	ownerSymbol, _ := b.program.inputs.Resolution.Symbols.Symbol(id)
	if ownerSymbol.Kind != symbol.SymbolFunction && ownerSymbol.Kind != symbol.SymbolMethod && ownerSymbol.Kind != symbol.SymbolExternFunction {
		return fmt.Errorf("semantic snapshot signature has the wrong symbol kind")
	}
	if value.Convention != 0 && value.Convention != types.Pebble && value.Convention != types.C {
		return fmt.Errorf("semantic snapshot signature convention is invalid")
	}
	if !equalSymbolIDs(value.Parameters, b.directSymbols(id, symbol.SymbolParameter)) {
		return fmt.Errorf("semantic snapshot signature parameters do not match resolver order")
	}
	expectedTypeParams, ok := b.genericParameters(id)
	if !ok || !equalSymbolIDs(value.TypeParams, expectedTypeParams) {
		return fmt.Errorf("semantic snapshot signature type parameters do not match resolver ownership")
	}
	for _, parameter := range value.Parameters {
		if !b.validSymbol(parameter) {
			return fmt.Errorf("semantic snapshot signature parameter is invalid")
		}
		parameterSymbol, _ := b.program.inputs.Resolution.Symbols.Symbol(parameter)
		if parameterSymbol.Kind != symbol.SymbolParameter || parameterSymbol.Containing != id {
			return fmt.Errorf("semantic snapshot signature parameter ownership is invalid")
		}
	}
	for _, parameter := range value.TypeParams {
		if !b.validSymbol(parameter) {
			return fmt.Errorf("semantic snapshot signature type parameter is invalid")
		}
		parameterSymbol, _ := b.program.inputs.Resolution.Symbols.Symbol(parameter)
		inheritedOwner := symbol.SymbolID(0)
		if ownerSymbol.Kind == symbol.SymbolMethod {
			inheritedOwner = ownerSymbol.Containing
		}
		if parameterSymbol.Kind != symbol.SymbolTypeParameter || (parameterSymbol.Containing != id && parameterSymbol.Containing != inheritedOwner) {
			return fmt.Errorf("semantic snapshot signature type-parameter ownership is invalid")
		}
	}
	for _, input := range value.Inputs {
		if input != 0 && uint64(input) > uint64(len(b.program.templates)) {
			return fmt.Errorf("semantic snapshot signature input is invalid")
		}
	}
	if value.Result != 0 && uint64(value.Result) > uint64(len(b.program.templates)) {
		return fmt.Errorf("semantic snapshot signature result is invalid")
	}
	if value.State == DeclarationReady {
		if len(value.Parameters) != len(value.Inputs) || value.Result == 0 || (value.Convention != types.Pebble && value.Convention != types.C) {
			return fmt.Errorf("semantic snapshot ready signature is incomplete")
		}
		for _, input := range value.Inputs {
			if input == 0 {
				return fmt.Errorf("semantic snapshot ready signature has an invalid input")
			}
		}
	}
	return nil
}

func (b *semanticSnapshotBuilder) directSymbols(owner symbol.SymbolID, kind symbol.SymbolKind) []symbol.SymbolID {
	var result []symbol.SymbolID
	for _, value := range b.program.inputs.Resolution.Symbols.All() {
		if !value.Error && value.Kind == kind && value.Containing == owner {
			result = append(result, value.ID)
		}
	}
	return result
}

func (b *semanticSnapshotBuilder) declarationMembers(owner symbol.SymbolID) []symbol.SymbolID {
	var result []symbol.SymbolID
	for _, id := range b.program.inputs.Resolution.Members(owner) {
		value, ok := b.program.inputs.Resolution.Symbols.Symbol(id)
		if ok && !value.Error && (value.Kind == symbol.SymbolField || value.Kind == symbol.SymbolVariant) {
			result = append(result, id)
		}
	}
	return result
}

func (b *semanticSnapshotBuilder) genericParameters(id symbol.SymbolID) ([]symbol.SymbolID, bool) {
	value, ok := b.program.inputs.Resolution.Symbols.Symbol(id)
	if !ok {
		return nil, false
	}
	switch value.Kind {
	case symbol.SymbolType, symbol.SymbolExternType, symbol.SymbolRuntimeType:
		declaration, ok := b.program.declarations[id]
		if !ok {
			return nil, false
		}
		return declaration.Parameters, true
	case symbol.SymbolFunction, symbol.SymbolMethod, symbol.SymbolExternFunction:
		signature, ok := b.program.signatures[id]
		if !ok {
			return nil, false
		}
		return signature.TypeParams, true
	default:
		return nil, false
	}
}

func (b *semanticSnapshotBuilder) methodLocalParameters(id symbol.SymbolID) ([]symbol.SymbolID, bool) {
	method, ok := b.program.inputs.Resolution.Symbols.Symbol(id)
	if !ok || method.Kind != symbol.SymbolMethod {
		return nil, false
	}
	signature, ok := b.program.signatures[id]
	if !ok {
		return nil, false
	}
	inherited := b.program.owners[method.Containing]
	if len(signature.TypeParams) < len(inherited) || !equalSymbolIDs(signature.TypeParams[:len(inherited)], inherited) {
		return nil, false
	}
	return signature.TypeParams[len(inherited):], true
}

func (b *semanticSnapshotBuilder) validateOwnerJoins() error {
	for _, id := range sortedMapSymbolIDs(b.program.declarations) {
		declaration := b.program.declarations[id]
		if !equalSymbolIDs(declaration.Parameters, b.program.owners[id]) {
			return fmt.Errorf("semantic snapshot declaration owner order is inconsistent")
		}
	}
	for _, id := range sortedMapSymbolIDs(b.program.signatures) {
		signature := b.program.signatures[id]
		if !equalSymbolIDs(signature.TypeParams, b.program.owners[id]) {
			return fmt.Errorf("semantic snapshot signature owner order is inconsistent")
		}
	}

	expectedBindings := make([]symbol.SymbolID, 0)
	expectedOwners := make(map[symbol.SymbolID][]symbol.SymbolID)
	for _, value := range b.program.inputs.Resolution.Symbols.All() {
		if value.Error || value.Kind != symbol.SymbolTypeParameter {
			continue
		}
		expectedBindings = append(expectedBindings, value.ID)
		expectedOwners[value.Containing] = append(expectedOwners[value.Containing], value.ID)
	}
	for _, id := range sortedMapSymbolIDs(b.program.signatures) {
		owner, _ := b.program.inputs.Resolution.Symbols.Symbol(id)
		parameters := append([]symbol.SymbolID(nil), expectedOwners[id]...)
		if owner.Kind == symbol.SymbolMethod {
			parameters = append(append([]symbol.SymbolID(nil), expectedOwners[owner.Containing]...), parameters...)
		}
		expectedOwners[id] = parameters
	}
	if !equalSymbolIDs(sortedMapSymbolIDs(b.program.typeParams), expectedBindings) {
		return fmt.Errorf("semantic snapshot type-parameter bindings are missing, extra, or orphaned")
	}
	if !equalSymbolIDs(sortedMapSymbolIDs(b.program.owners), sortedMapSymbolIDs(expectedOwners)) {
		return fmt.Errorf("semantic snapshot owner table is missing or has an extra owner")
	}
	for _, id := range sortedMapSymbolIDs(expectedOwners) {
		if !equalSymbolIDs(b.program.owners[id], expectedOwners[id]) {
			return fmt.Errorf("semantic snapshot owner parameter bindings are missing, extra, orphaned, duplicated, or unordered")
		}
	}
	return nil
}

func (b *semanticSnapshotBuilder) validateRuntime() error {
	if err := b.charge(2); err != nil {
		return err
	}
	if !b.program.runtimeReady || !b.typeSnapshot.Contains(b.program.runtimeTypes.Allocator) || !b.typeSnapshot.Contains(b.program.runtimeTypes.Context) {
		return fmt.Errorf("semantic snapshot runtime identities are unavailable")
	}
	for _, value := range []struct {
		kind symbol.RuntimeType
		id   types.TypeID
	}{{symbol.RuntimeAllocator, b.program.runtimeTypes.Allocator}, {symbol.RuntimeContext, b.program.runtimeTypes.Context}} {
		kind, id := value.kind, value.id
		symbolID, ok := b.program.inputs.Resolution.Runtime(kind)
		if !ok || !b.validSymbol(symbolID) {
			return fmt.Errorf("semantic snapshot runtime symbol is invalid")
		}
		key, _ := b.typeSnapshot.Key(id)
		declaration, arguments, ok := key.Nominal()
		if !ok || declaration != symbolID || len(arguments) != 0 {
			return fmt.Errorf("semantic snapshot runtime type is not the exact nominal identity")
		}
		decl, ok := b.program.declarations[symbolID]
		if !ok || decl.State != DeclarationReady || decl.Concrete != id {
			return fmt.Errorf("semantic snapshot runtime declaration is incomplete")
		}
	}
	return nil
}

func (b *semanticSnapshotBuilder) validTypeResult(value TypeResult) bool {
	switch value.State {
	case TypeFinal:
		return b.typeSnapshot.Contains(value.Type)
	case TypeError:
		b.sawTypeError = true
		return value.Type == 0
	default:
		return false
	}
}

func (b *semanticSnapshotBuilder) validOrigin(value Origin) bool {
	return (value.Syntax == (symbol.SyntaxRef{}) || b.validSyntax(value.Syntax)) &&
		(value.Symbol == 0 || b.validSymbol(value.Symbol)) &&
		(value.GenericOwner == 0 || b.validSymbol(value.GenericOwner))
}

func (b *semanticSnapshotBuilder) validSymbol(id symbol.SymbolID) bool {
	if id == 0 {
		return false
	}
	value, ok := b.program.inputs.Resolution.Symbols.Symbol(id)
	return ok && value.ID == id
}

func (b *semanticSnapshotBuilder) validSyntax(ref symbol.SyntaxRef) bool {
	if ref.Module == 0 || ref.Node == 0 {
		return false
	}
	item, ok := b.program.modules[module.ModuleID(ref.Module)]
	if !ok || item.Tree == nil {
		return false
	}
	_, ok = item.Tree.Node(ref.Node)
	return ok
}

func (b *semanticSnapshotBuilder) validateWidth(count int) error {
	if count < 0 || uint64(count) > uint64(b.maxWidth) {
		return fmt.Errorf("semantic snapshot component width exceeds shape limit")
	}
	return b.charge(uint64(count))
}

func (b *semanticSnapshotBuilder) charge(amount uint64) error {
	if amount > b.maxSteps-b.steps {
		return fmt.Errorf("semantic snapshot decomposition limit exceeded")
	}
	b.steps += amount
	return nil
}

func sortedMapSymbolIDs[V any](values map[symbol.SymbolID]V) []symbol.SymbolID {
	ids := make([]symbol.SymbolID, 0, len(values))
	for id := range values {
		ids = append(ids, id)
	}
	sort.Slice(ids, func(i, j int) bool { return ids[i] < ids[j] })
	return ids
}

func equalSymbolIDs(a, b []symbol.SymbolID) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func strictlyIncreasingSymbols(values []symbol.SymbolID) bool {
	for index, value := range values {
		if value == 0 || index != 0 && values[index-1] >= value {
			return false
		}
	}
	return true
}

func strictlyIncreasingRefs(values []symbol.SyntaxRef) bool {
	for index, value := range values {
		if value == (symbol.SyntaxRef{}) || index != 0 && !refLess(values[index-1], value) {
			return false
		}
	}
	return true
}

func strictlyIncreasingRequirementOwners(values []requirementTableManifest) bool {
	for index, value := range values {
		if value.owner == 0 || index != 0 && values[index-1].owner >= value.owner {
			return false
		}
	}
	return true
}

func strictlyIncreasingSelections(values []selectionTableManifest) bool {
	for index, value := range values {
		if !value.id.IsValid() || index != 0 && values[index-1].id >= value.id {
			return false
		}
	}
	return true
}
