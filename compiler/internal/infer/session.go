package infer

import (
	"fmt"
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type capability uint8

const (
	capNumeric capability = 1 << iota
	capIntegral
	capOrdered
)

type ufCell struct {
	parent             InferID
	rank               uint8
	minimum            InferID
	known              types.TypeID
	shape              *Shape
	literals           []literalValue
	capabilities       capability
	capabilityEvidence []capabilityEvidence
	error              bool
	origin             Origin
}

type capabilityEvidence struct {
	capability capability
	origin     Origin
}

type storedConstraint struct {
	id       ConstraintID
	value    Constraint
	origin   OriginID
	done     bool
	requeues uint32
}

type publishedInstantiation struct {
	site        symbol.SyntaxRef
	generic     symbol.SymbolID
	arguments   []Term
	choice      ChoiceRef
	alternative uint32
	guarded     bool
}

type publishedSlot struct {
	id          SlotID
	term        Term
	choice      ChoiceRef
	alternative uint32
	guarded     bool
}

type slotPublicationKey struct {
	term        Term
	choice      ChoiceRef
	alternative uint32
	guarded     bool
}

type methodState struct {
	method    symbol.SymbolID
	arguments []Term
	ownedFrom int
	ready     bool
}

type resolveKey struct {
	ref   symbol.SyntaxRef
	owner symbol.SymbolID
}

type occurrenceResult struct {
	shape    Shape
	conflict *inferenceConflict
}

type inferenceConflict struct {
	code    diagnostic.Code
	message string
	origin  Origin
	related []Origin
}

type Session struct {
	program                *Program
	config                 Config
	token                  *sessionToken
	reporter               *reporter
	solved                 bool
	solution               *Solution
	invalid                bool
	cells                  []ufCell
	constraints            []storedConstraint
	origins                []Origin
	shapeComponents        uint32
	unificationSteps       uint64
	decompositionSteps     uint64
	totalRequeues          uint64
	choiceStates           uint64
	choiceCount            uint32
	constraintCount        uint64
	symbolRoots            map[symbol.SymbolID]Term
	syntaxRoots            map[symbol.SyntaxRef]Term
	instantiations         map[symbol.SyntaxRef]publishedInstantiation
	instantiationArguments uint64
	methodStates           map[symbol.SyntaxRef]methodState
	methodSites            map[symbol.SyntaxRef]bool
	resolveMemo            map[resolveKey]TypeResult
	typeOccurrenceMemo     map[resolveKey]occurrenceResult
	valueOccurrenceMemo    map[symbol.SyntaxRef]*inferenceConflict
	requirements           []Requirement
	selections             map[ConstraintID]uint32
	slots                  []publishedSlot
	slotPublications       map[slotPublicationKey]bool
	speculative            bool
	speculativeConflict    *inferenceConflict
	failed                 bool
	fatal                  bool
}

func NewSession(program *Program, diagnostics *diagnostic.DiagnosticSet, config Config) *Session {
	config = normalizeConfig(config)
	s := &Session{
		program: program, config: config, token: &sessionToken{},
		reporter:    newReporter(diagnostics, config.MaxDiagnostics),
		symbolRoots: make(map[symbol.SymbolID]Term), syntaxRoots: make(map[symbol.SyntaxRef]Term),
		instantiations: make(map[symbol.SyntaxRef]publishedInstantiation),
		methodStates:   make(map[symbol.SyntaxRef]methodState),
		methodSites:    make(map[symbol.SyntaxRef]bool),
		resolveMemo:    make(map[resolveKey]TypeResult), selections: make(map[ConstraintID]uint32),
		typeOccurrenceMemo:  make(map[resolveKey]occurrenceResult),
		valueOccurrenceMemo: make(map[symbol.SyntaxRef]*inferenceConflict),
		slotPublications:    make(map[slotPublicationKey]bool),
	}
	s.reporter.onFatal = s.markFatal
	s.reporter.isFatal = s.Fatal
	if program == nil || !program.valid || program.inputs.Types == nil {
		s.invalid = true
		s.reporter.error(CodeResourceLimit, "inference session requires a valid prepared program", Origin{})
	} else if program.ensureIdentity() == nil {
		s.invalid = true
		s.reporter.error(CodeResourceLimit, "inference session requires a program identity", Origin{})
	}
	return s
}

// Fatal reports whether the session can no longer accept facts or perform
// inference. It exposes only the builder lifecycle barrier, not diagnostics or
// mutable solver state.
func (s *Session) Fatal() bool {
	return s == nil || s.invalid || s.fatal
}

func (s *Session) markFatal() {
	if s != nil {
		s.fatal = true
	}
}

func (s *Session) errorTerm() Term {
	if s == nil {
		return Term{}
	}
	return Term{owner: s.token, kind: termError}
}

// Error returns this session's absorbing recovery term. It lets a client keep
// generating independent facts after damaged upstream syntax or resolution
// without manufacturing a semantic TypeID.
func (s *Session) Error(origin Origin) Term {
	if !s.mutable() {
		return s.errorTerm()
	}
	_ = origin
	return s.errorTerm()
}

func (s *Session) mutable() bool {
	if s == nil {
		return false
	}
	if s.Fatal() {
		return false
	}
	if s.solved {
		s.reporter.error(CodeResourceLimit, "inference session is already solved", Origin{})
		s.reporter.flush()
		return false
	}
	return true
}

func (s *Session) newCell(origin Origin, kind termKind, literal *literalValue) Term {
	if !s.mutable() {
		return s.errorTerm()
	}
	if uint32(len(s.cells)) >= s.config.MaxInferVariables {
		s.limit("inference variable", uint64(s.config.MaxInferVariables), origin)
		return s.errorTerm()
	}
	id := InferID(len(s.cells) + 1)
	cell := ufCell{parent: id, minimum: id, origin: origin}
	if literal != nil {
		cell.literals = []literalValue{literal.clone()}
	}
	s.cells = append(s.cells, cell)
	return Term{owner: s.token, kind: kind, id: id}
}

func (s *Session) Variable(origin Origin) Term { return s.newCell(origin, termVariable, nil) }

func (s *Session) Known(id types.TypeID) Term {
	if !s.mutable() {
		return s.errorTerm()
	}
	if _, ok := s.program.typeKey(id); !ok {
		s.reporter.error(CodeResourceLimit, "known term uses a TypeID outside the prepared store", Origin{})
		s.failed = true
		return s.errorTerm()
	}
	return Term{owner: s.token, kind: termKnown, known: id}
}

func (s *Session) IntegerLiteral(text []byte, origin Origin) Term {
	if !s.mutable() {
		return s.errorTerm()
	}
	value, err := parseIntegerLiteral(text, s.config)
	if err != nil {
		s.reporter.error(CodeLiteral, err.Error(), origin)
		s.failed = true
		return s.errorTerm()
	}
	literal := literalValue{kind: literalInteger, integer: value, origin: origin}
	return s.newCell(origin, termIntLiteral, &literal)
}

func (s *Session) FloatLiteral(text []byte, origin Origin) Term {
	if !s.mutable() {
		return s.errorTerm()
	}
	value, err := parseFloatLiteral(text, s.config)
	if err != nil {
		s.reporter.error(CodeLiteral, err.Error(), origin)
		s.failed = true
		return s.errorTerm()
	}
	literal := literalValue{kind: literalFloat, rational: value, origin: origin}
	return s.newCell(origin, termFloatLiteral, &literal)
}

func (s *Session) NegateLiteral(term Term, origin Origin) Term {
	if !s.mutable() {
		return s.errorTerm()
	}
	if !term.belongs(s.token) {
		return s.invalidTerm("negated literal belongs to another session", origin)
	}
	if term.kind != termIntLiteral && term.kind != termFloatLiteral {
		s.reporter.error(CodeInvalidType, "only an exact literal can be negated by inference", origin)
		return s.errorTerm()
	}
	root := s.find(term.id)
	if root == 0 || len(s.cells[root-1].literals) != 1 {
		return s.errorTerm()
	}
	v := s.cells[root-1].literals[0].clone()
	v.origin = origin
	if v.kind == literalInteger {
		v.integer.Neg(v.integer)
	} else {
		v.rational.Neg(v.rational)
	}
	kind := termIntLiteral
	if v.kind == literalFloat {
		kind = termFloatLiteral
	}
	return s.newCell(origin, kind, &v)
}

func (s *Session) Add(value Constraint) ConstraintID {
	return s.addConstraint(value)
}

// AddChoice atomically adds one top-level OneOf and returns the unforgeable
// capability used for guarded publication.
func (s *Session) AddChoice(value Constraint) (ConstraintID, ChoiceRef) {
	if value.kind != constraintOneOf {
		if s.mutable() {
			s.reporter.error(CodeResourceLimit, "AddChoice requires one top-level OneOf constraint", value.origin)
			s.failed = true
		}
		return 0, ChoiceRef{}
	}
	id := s.addConstraint(value)
	if id == 0 {
		return 0, ChoiceRef{}
	}
	return id, ChoiceRef{owner: s.token, constraint: id, alternatives: uint32(len(value.alternatives))}
}

func (s *Session) addConstraint(value Constraint) ConstraintID {
	if !s.mutable() {
		return 0
	}
	units, choices, components := constraintResources(value)
	if units == 0 || s.constraintCount+units > uint64(s.config.MaxConstraints) {
		s.limit("constraint", uint64(s.config.MaxConstraints), value.origin)
		return 0
	}
	if uint64(s.choiceCount)+uint64(choices) > uint64(s.config.MaxChoices) {
		s.limit("choice", uint64(s.config.MaxChoices), value.origin)
		return 0
	}
	if uint64(s.shapeComponents)+uint64(components) > uint64(s.config.MaxShapeComponents) {
		s.limit("shape component", uint64(s.config.MaxShapeComponents), value.origin)
		return 0
	}
	if err := s.validateConstraint(value, 0); err != nil {
		s.reporter.error(CodeResourceLimit, err.Error(), value.origin)
		s.failed = true
		return 0
	}
	methodSites, ok := constraintMethodSites(value)
	if !ok {
		s.reporter.error(CodeResourceLimit, "method-selection site is repeated", value.origin)
		s.failed = true
		return 0
	}
	for _, site := range methodSites {
		if s.methodSites[site] {
			s.reporter.error(CodeResourceLimit, "method-selection site is repeated", value.origin)
			s.failed = true
			return 0
		}
	}
	id := ConstraintID(len(s.constraints) + 1)
	originID := s.addOrigin(value.origin)
	s.shapeComponents += components
	s.choiceCount += choices
	s.constraintCount += units
	s.constraints = append(s.constraints, storedConstraint{id: id, value: cloneConstraint(value), origin: originID})
	for _, site := range methodSites {
		s.methodSites[site] = true
	}
	return id
}

func (s *Session) PublishSlot(term Term) SlotID {
	return s.publishSlot(ChoiceRef{}, 0, term, false)
}

func (s *Session) PublishGuardedSlot(choice ChoiceRef, alternative uint32, term Term) SlotID {
	return s.publishSlot(choice, alternative, term, true)
}

func (s *Session) publishSlot(choice ChoiceRef, alternative uint32, term Term, guarded bool) SlotID {
	if !s.mutable() {
		return SlotID{}
	}
	if !term.belongs(s.token) {
		s.invalidTerm("slot publication contains a foreign or invalid term", Origin{})
		return SlotID{}
	}
	if guarded {
		if choice.owner != s.token || !choice.constraint.IsValid() || choice.alternatives == 0 || alternative >= choice.alternatives || int(choice.constraint) > len(s.constraints) || s.constraints[choice.constraint-1].value.kind != constraintOneOf {
			s.invalidTerm("guarded slot uses an invalid or foreign choice alternative", Origin{})
			return SlotID{}
		}
	} else if choice.owner != nil || choice.constraint.IsValid() || choice.alternatives != 0 || alternative != 0 {
		s.invalidTerm("ordinary slot has invalid guard state", Origin{})
		return SlotID{}
	}
	key := slotPublicationKey{term: term, choice: choice, alternative: alternative, guarded: guarded}
	if s.slotPublications[key] {
		s.invalidTerm("solved slot published more than once", Origin{})
		return SlotID{}
	}
	if uint32(len(s.slots)) >= s.config.MaxSolvedSlots {
		s.limit("solved slot", uint64(s.config.MaxSolvedSlots), Origin{})
		s.failed = true
		return SlotID{}
	}
	id := SlotID{owner: s.token, ordinal: uint32(len(s.slots) + 1)}
	s.slots = append(s.slots, publishedSlot{id: id, term: term, choice: choice, alternative: alternative, guarded: guarded})
	s.slotPublications[key] = true
	return id
}

func (s *Session) PublishSymbol(id symbol.SymbolID, term Term) {
	if !s.mutable() {
		return
	}
	if id == 0 || !term.belongs(s.token) {
		s.invalidTerm("invalid symbol publication", Origin{Symbol: id})
		return
	}
	if _, exists := s.symbolRoots[id]; exists {
		s.invalidTerm("symbol type published more than once", Origin{Symbol: id})
		return
	}
	s.symbolRoots[id] = term
}

func (s *Session) PublishSyntax(ref symbol.SyntaxRef, term Term) {
	if !s.mutable() {
		return
	}
	if ref.Module == 0 || ref.Node == 0 || !term.belongs(s.token) {
		s.invalidTerm("invalid syntax publication", Origin{Syntax: ref})
		return
	}
	if _, exists := s.syntaxRoots[ref]; exists {
		s.invalidTerm("syntax type published more than once", Origin{Syntax: ref})
		return
	}
	s.syntaxRoots[ref] = term
}

func (s *Session) PublishInstantiation(site symbol.SyntaxRef, generic symbol.SymbolID, arguments []Term) {
	s.publishInstantiation(ChoiceRef{}, 0, site, generic, arguments, false)
}

// PublishGuardedInstantiation publishes one generic application only when the
// exact OneOf alternative identified by choice is selected.
func (s *Session) PublishGuardedInstantiation(choice ChoiceRef, alternative uint32, site symbol.SyntaxRef, generic symbol.SymbolID, arguments []Term) {
	s.publishInstantiation(choice, alternative, site, generic, arguments, true)
}

func (s *Session) publishInstantiation(choice ChoiceRef, alternative uint32, site symbol.SyntaxRef, generic symbol.SymbolID, arguments []Term, guarded bool) {
	if !s.mutable() {
		return
	}
	if site.Module == 0 || site.Node == 0 || generic == 0 {
		s.invalidTerm("invalid generic instantiation publication", Origin{Syntax: site, Symbol: generic})
		return
	}
	if guarded {
		if choice.owner != s.token || !choice.constraint.IsValid() || choice.alternatives == 0 || alternative >= choice.alternatives || uint64(choice.constraint) > uint64(len(s.constraints)) {
			s.invalidTerm("guarded generic instantiation uses an invalid or foreign choice alternative", Origin{Syntax: site, Symbol: generic})
			return
		}
		stored := s.constraints[choice.constraint-1].value
		if stored.kind != constraintOneOf || choice.alternatives != uint32(len(stored.alternatives)) {
			s.invalidTerm("guarded generic instantiation uses an invalid or foreign choice alternative", Origin{Syntax: site, Symbol: generic})
			return
		}
	} else if choice != (ChoiceRef{}) || alternative != 0 {
		s.invalidTerm("ordinary generic instantiation has invalid guard state", Origin{Syntax: site, Symbol: generic})
		return
	}
	if _, exists := s.instantiations[site]; exists {
		s.invalidTerm("generic instantiation published more than once", Origin{Syntax: site, Symbol: generic})
		return
	}
	for _, term := range arguments {
		if !term.belongs(s.token) {
			s.invalidTerm("generic argument belongs to another session", Origin{Syntax: site, Symbol: generic})
			return
		}
	}
	componentLimit := uint64(s.config.MaxShapeComponents)
	if uint64(len(s.instantiations)) >= componentLimit {
		s.limit("generic instantiation publication", componentLimit, Origin{Syntax: site, Symbol: generic})
		s.failed = true
		return
	}
	argumentCount := uint64(len(arguments))
	if argumentCount > componentLimit || s.instantiationArguments > componentLimit-argumentCount {
		s.limit("retained generic instantiation argument", componentLimit, Origin{Syntax: site, Symbol: generic})
		s.failed = true
		return
	}
	copyArgs := append([]Term(nil), arguments...)
	s.instantiationArguments += argumentCount
	s.instantiations[site] = publishedInstantiation{
		site: site, generic: generic, arguments: copyArgs,
		choice: choice, alternative: alternative, guarded: guarded,
	}
}

func (s *Session) addOrigin(origin Origin) OriginID {
	s.origins = append(s.origins, origin)
	return OriginID(len(s.origins))
}

func (s *Session) validateConstraint(value Constraint, depth uint32) error {
	if depth > s.config.MaxTypeSyntaxDepth {
		return fmt.Errorf("choice nesting exceeds limit %d", s.config.MaxTypeSyntaxDepth)
	}
	validTerm := func(term Term) bool { return term.belongs(s.token) }
	switch value.kind {
	case constraintEqual, constraintLiteralFits:
		if !validTerm(value.a) || !validTerm(value.b) {
			return fmt.Errorf("constraint contains a foreign or invalid term")
		}
	case constraintNumeric, constraintIntegral, constraintOrdered:
		if !validTerm(value.a) {
			return fmt.Errorf("constraint contains a foreign or invalid term")
		}
	case constraintHasField:
		if !validTerm(value.a) || !validTerm(value.b) || value.name == "" {
			return fmt.Errorf("invalid field constraint")
		}
	case constraintSelectMethod:
		if !validTerm(value.a) || !validTerm(value.b) || value.name == "" || value.site.Module == 0 || value.site.Node == 0 {
			return fmt.Errorf("invalid method-selection constraint")
		}
		for _, term := range value.explicit {
			if !validTerm(term) {
				return fmt.Errorf("method-selection constraint contains a foreign explicit argument")
			}
		}
	case constraintCallable:
		if !validTerm(value.a) || !validTerm(value.b) {
			return fmt.Errorf("invalid callable constraint")
		}
		for _, argument := range value.arguments {
			if !validTerm(argument.Source) || !validTerm(argument.Destination) {
				return fmt.Errorf("callable constraint contains a foreign or invalid argument")
			}
		}
	case constraintIndexable, constraintSliceable:
		if !validTerm(value.a) || !validTerm(value.b) {
			return fmt.Errorf("invalid structural constraint")
		}
	case constraintShape:
		if !validTerm(value.a) || !validShape(value.shape, s.token) {
			return fmt.Errorf("invalid algebraic shape")
		}
		if depth := shapeDepth(value.shape); depth > s.config.MaxTypeSyntaxDepth {
			return fmt.Errorf("algebraic shape depth %d exceeds limit %d", depth, s.config.MaxTypeSyntaxDepth)
		}
	case constraintInstantiate:
		if !validTerm(value.a) {
			return fmt.Errorf("invalid instantiation subject")
		}
		if _, ok := s.program.Template(value.template); !ok {
			return fmt.Errorf("instantiation uses a foreign template")
		}
		seen := make(map[symbol.SymbolID]bool)
		for _, sub := range value.substitutions {
			if sub.Parameter == 0 || seen[sub.Parameter] || !validTerm(sub.Argument) {
				return fmt.Errorf("invalid instantiation substitution")
			}
			seen[sub.Parameter] = true
		}
	case constraintTypeOccurrence:
		if !validTerm(value.a) || value.ref.Module == 0 || value.ref.Node == 0 {
			return fmt.Errorf("invalid type-occurrence constraint")
		}
		if _, _, ok := s.program.node(value.ref); !ok {
			return fmt.Errorf("type-occurrence constraint uses foreign or damaged syntax")
		}
		if value.owner != 0 {
			if _, ok := s.program.inputs.Resolution.Symbols.Symbol(value.owner); !ok {
				return fmt.Errorf("type-occurrence constraint uses a foreign owner")
			}
		}
	case constraintValueOccurrence:
		if value.ref.Module == 0 || value.ref.Node == 0 {
			return fmt.Errorf("invalid value-occurrence constraint")
		}
		if _, _, ok := s.program.node(value.ref); !ok {
			return fmt.Errorf("value-occurrence constraint uses foreign or damaged syntax")
		}
	case constraintOneOf:
		if uint32(len(value.alternatives)) == 0 || uint32(len(value.alternatives)) > s.config.MaxChoiceAlternatives {
			return fmt.Errorf("choice alternative count exceeds limit")
		}
		for _, alt := range value.alternatives {
			if len(alt.Constraints) == 0 {
				return fmt.Errorf("choice alternative is empty")
			}
			for _, nested := range alt.Constraints {
				if err := s.validateConstraint(nested, depth+1); err != nil {
					return err
				}
			}
		}
	default:
		return fmt.Errorf("unknown constraint kind")
	}
	return nil
}

func constraintMethodSites(value Constraint) ([]symbol.SyntaxRef, bool) {
	var sites []symbol.SyntaxRef
	seen := make(map[symbol.SyntaxRef]bool)
	stack := []Constraint{value}
	for len(stack) != 0 {
		last := len(stack) - 1
		current := stack[last]
		stack = stack[:last]
		if current.kind == constraintSelectMethod {
			if seen[current.site] {
				return nil, false
			}
			seen[current.site] = true
			sites = append(sites, current.site)
		}
		for i := len(current.alternatives) - 1; i >= 0; i-- {
			for j := len(current.alternatives[i].Constraints) - 1; j >= 0; j-- {
				stack = append(stack, current.alternatives[i].Constraints[j])
			}
		}
	}
	return sites, true
}

func constraintResources(value Constraint) (units uint64, choices, components uint32) {
	stack := []Constraint{value}
	for len(stack) != 0 {
		last := len(stack) - 1
		current := stack[last]
		stack = stack[:last]
		units++
		if current.kind == constraintShape {
			count, ok := shapeComponents(current.shape)
			if !ok || uint64(components)+uint64(count) > uint64(^uint32(0)) {
				return 0, 0, 0
			}
			components += count
		}
		if current.kind == constraintOneOf {
			choices++
			for i := len(current.alternatives) - 1; i >= 0; i-- {
				for j := len(current.alternatives[i].Constraints) - 1; j >= 0; j-- {
					stack = append(stack, current.alternatives[i].Constraints[j])
				}
			}
		}
	}
	return units, choices, components
}

func (s *Session) invalidTerm(message string, origin Origin) Term {
	s.reporter.error(CodeResourceLimit, message, origin)
	s.failed = true
	return s.errorTerm()
}
func (s *Session) limit(kind string, limit uint64, origin Origin) {
	s.conflict(CodeResourceLimit, fmt.Sprintf("%s limit of %d exceeded", kind, limit), origin)
}

func (s *Session) ResolveType(ref symbol.SyntaxRef, owner symbol.SymbolID) TypeResult {
	if !s.mutable() {
		return TypeResult{State: TypeError}
	}
	key := resolveKey{ref: ref, owner: owner}
	if result, ok := s.resolveMemo[key]; ok {
		return result
	}
	result := s.program.resolveConcreteOccurrence(ref, owner, s.reporter)
	if result.State == TypeError {
		s.failed = true
	}
	if s.Fatal() {
		return result
	}
	s.resolveMemo[key] = result
	return result
}

func sortedSymbolIDs(values map[symbol.SymbolID]Term) []symbol.SymbolID {
	ids := make([]int, 0, len(values))
	for id := range values {
		ids = append(ids, int(id))
	}
	sort.Ints(ids)
	out := make([]symbol.SymbolID, len(ids))
	for i, id := range ids {
		out[i] = symbol.SymbolID(id)
	}
	return out
}
