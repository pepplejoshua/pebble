package infer

import (
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

type applyResult struct {
	changed bool
	success bool
	delayed bool
}

func (s *Session) Solve() *Solution {
	if s == nil {
		return &Solution{}
	}
	if s.solved {
		if !s.Fatal() {
			s.reporter.error(CodeResourceLimit, "inference session is already solved", Origin{})
			s.reporter.flush()
		}
		return repeatedSolveRecovery(s.token)
	}
	if !s.Fatal() {
		s.solveOrdinary()
		if !s.Fatal() {
			s.solveChoices()
		}
		if !s.Fatal() {
			s.solveOrdinary()
		}
		if !s.Fatal() {
			inactive := s.inactiveGuardedRoots()
			s.defaultLiterals(inactive)
			if !s.Fatal() {
				s.solveOrdinary()
			}
			if !s.Fatal() {
				s.finalizeUnresolved(inactive)
			}
		}
	}
	result := s.freezeSolution()
	s.solved = true
	s.solution = result
	s.reporter.flush()
	return result
}

func (s *Session) solveOrdinary() {
	if s.Fatal() {
		return
	}
	for {
		changed := false
		for index := range s.constraints {
			if s.Fatal() {
				return
			}
			entry := &s.constraints[index]
			if entry.done || entry.value.kind == constraintOneOf {
				continue
			}
			result := s.apply(entry.value)
			if s.Fatal() {
				return
			}
			changed = changed || result.changed
			if !result.delayed {
				entry.done = true
				changed = true
			} else {
				if !s.chargeRequeue(entry, entry.value.origin) {
					if !s.Fatal() {
						entry.done = true
					}
				}
			}
		}
		if s.Fatal() {
			return
		}
		materialized, _ := s.materializeReadyShapes()
		if s.Fatal() {
			return
		}
		changed = changed || materialized
		if !changed {
			return
		}
	}
}

func (s *Session) apply(value Constraint) applyResult {
	switch value.kind {
	case constraintEqual:
		changed, ok := s.unify(value.a, value.b, value.origin)
		return applyResult{changed: changed, success: ok}
	case constraintNumeric:
		changed, ok := s.applyCapability(value.a, capNumeric, value.origin)
		return applyResult{changed: changed, success: ok}
	case constraintIntegral:
		changed, ok := s.applyCapability(value.a, capIntegral, value.origin)
		return applyResult{changed: changed, success: ok}
	case constraintOrdered:
		changed, ok := s.applyCapability(value.a, capOrdered, value.origin)
		return applyResult{changed: changed, success: ok}
	case constraintLiteralFits:
		changed, ok := s.literalFits(value.a, value.b, value.origin)
		return applyResult{changed: changed, success: ok}
	case constraintShape:
		changed, ok := s.constrainShape(value.a, value.shape, value.origin)
		return applyResult{changed: changed, success: ok}
	case constraintInstantiate:
		changed, ok := s.instantiate(value)
		return applyResult{changed: changed, success: ok}
	case constraintTypeOccurrence:
		changed, ok := s.applyTypeOccurrence(value)
		return applyResult{changed: changed, success: ok}
	case constraintValueOccurrence:
		return applyResult{success: s.applyValueOccurrence(value)}
	case constraintHasField:
		changed, ok, delayed := s.hasField(value.a, value.name, value.b, value.origin)
		return applyResult{changed: changed, success: ok, delayed: delayed}
	case constraintHasComponent:
		changed, ok, delayed := s.hasComponent(value.a, value.ordinal, value.b, value.origin)
		return applyResult{changed: changed, success: ok, delayed: delayed}
	case constraintSelectMethod:
		changed, ok, delayed := s.selectMethod(value)
		return applyResult{changed: changed, success: ok, delayed: delayed}
	case constraintCallMember:
		changed, ok, delayed := s.callMember(value)
		return applyResult{changed: changed, success: ok, delayed: delayed}
	case constraintCallable:
		changed, ok, delayed := s.callable(value.a, value.arguments, value.b, value.origin)
		return applyResult{changed: changed, success: ok, delayed: delayed}
	case constraintIndexable:
		changed, ok, delayed := s.indexable(value.a, value.b, value.origin)
		return applyResult{changed: changed, success: ok, delayed: delayed}
	case constraintSliceable:
		changed, ok, delayed := s.sliceable(value.a, value.b, value.origin)
		return applyResult{changed: changed, success: ok, delayed: delayed}
	default:
		return applyResult{success: false, delayed: true}
	}
}

func (s *Session) chargeRequeue(entry *storedConstraint, origin Origin) bool {
	entry.requeues++
	s.totalRequeues++
	if entry.requeues > s.config.MaxConstraintRequeues || s.totalRequeues > s.config.MaxTotalRequeues {
		s.limit("constraint requeue", uint64(s.config.MaxConstraintRequeues), origin)
		s.failed = true
		return false
	}
	return true
}

type sessionSnapshot struct {
	cells               []ufCell
	requirements        []Requirement
	failed              bool
	methodStates        map[symbol.SyntaxRef]methodState
	constraints         []constraintProgress
	typeOccurrenceMemo  map[resolveKey]occurrenceResult
	valueOccurrenceMemo map[symbol.SyntaxRef]*inferenceConflict
}

type constraintProgress struct {
	done     bool
	requeues uint32
}

func (s *Session) snapshot() sessionSnapshot {
	cells := make([]ufCell, len(s.cells))
	for i, cell := range s.cells {
		cells[i] = cell
		cells[i].literals = cloneLiteralValues(cell.literals)
		cells[i].capabilityEvidence = append([]capabilityEvidence(nil), cell.capabilityEvidence...)
		if cell.shape != nil {
			shape := cloneShape(*cell.shape)
			cells[i].shape = &shape
		}
	}
	constraints := make([]constraintProgress, len(s.constraints))
	for i, entry := range s.constraints {
		constraints[i] = constraintProgress{done: entry.done, requeues: entry.requeues}
	}
	return sessionSnapshot{cells: cells, requirements: append([]Requirement(nil), s.requirements...), failed: s.failed, methodStates: cloneMethodStates(s.methodStates), constraints: constraints, typeOccurrenceMemo: cloneTypeOccurrenceMemo(s.typeOccurrenceMemo), valueOccurrenceMemo: cloneValueOccurrenceMemo(s.valueOccurrenceMemo)}
}

func (s *Session) restore(snapshot sessionSnapshot) {
	s.cells = cloneCells(snapshot.cells)
	s.requirements = snapshot.requirements
	s.failed = snapshot.failed
	s.methodStates = cloneMethodStates(snapshot.methodStates)
	s.typeOccurrenceMemo = cloneTypeOccurrenceMemo(snapshot.typeOccurrenceMemo)
	s.valueOccurrenceMemo = cloneValueOccurrenceMemo(snapshot.valueOccurrenceMemo)
	for i, progress := range snapshot.constraints {
		s.constraints[i].done = progress.done
		s.constraints[i].requeues = progress.requeues
	}
}

func cloneMethodStates(values map[symbol.SyntaxRef]methodState) map[symbol.SyntaxRef]methodState {
	out := make(map[symbol.SyntaxRef]methodState, len(values))
	for site, state := range values {
		state.arguments = append([]Term(nil), state.arguments...)
		out[site] = state
	}
	return out
}

func cloneCells(values []ufCell) []ufCell {
	cells := make([]ufCell, len(values))
	for i, cell := range values {
		cells[i] = cell
		cells[i].literals = cloneLiteralValues(cell.literals)
		cells[i].capabilityEvidence = append([]capabilityEvidence(nil), cell.capabilityEvidence...)
		if cell.shape != nil {
			shape := cloneShape(*cell.shape)
			cells[i].shape = &shape
		}
	}
	return cells
}

func (s *Session) solveChoices() {
	if s.Fatal() {
		return
	}
	var choices []int
	for i, entry := range s.constraints {
		if !entry.done && entry.value.kind == constraintOneOf {
			choices = append(choices, i)
		}
	}
	if len(choices) == 0 {
		return
	}
	base := s.snapshot()
	inheritedFailure := s.failed
	s.failed = false
	searchBase := s.snapshot()
	type globalChoiceSolution struct {
		state      sessionSnapshot
		selections map[ConstraintID]uint32
	}
	var solutions []globalChoiceSolution
	var firstConflict *inferenceConflict
	var explore func(int, map[ConstraintID]uint32)
	explore = func(position int, selected map[ConstraintID]uint32) {
		if len(solutions) >= 2 || s.Fatal() {
			return
		}
		if position == len(choices) {
			s.solveOrdinary()
			if s.Fatal() {
				return
			}
			if !s.failed {
				copySelections := make(map[ConstraintID]uint32, len(selected))
				for id, alternative := range selected {
					copySelections[id] = alternative
				}
				solutions = append(solutions, globalChoiceSolution{state: s.snapshot(), selections: copySelections})
			}
			return
		}
		level := s.snapshot()
		entry := s.constraints[choices[position]]
		for alternativeIndex, alternative := range entry.value.alternatives {
			if len(solutions) >= 2 || s.Fatal() {
				break
			}
			s.choiceStates++
			if s.choiceStates > s.config.MaxChoiceStates {
				s.speculativeConflict = nil
				s.limit("choice state", s.config.MaxChoiceStates, entry.value.origin)
				if firstConflict == nil {
					firstConflict = s.speculativeConflict
				}
				break
			}
			s.restore(level)
			s.failed = false
			s.speculativeConflict = nil
			if s.solveConstraintSet(alternative.Constraints) && !s.failed {
				s.solveOrdinary()
			}
			if s.Fatal() {
				return
			}
			if !s.failed {
				selected[entry.id] = uint32(alternativeIndex)
				explore(position+1, selected)
				if s.Fatal() {
					return
				}
				delete(selected, entry.id)
			} else if firstConflict == nil && s.speculativeConflict != nil {
				firstConflict = s.speculativeConflict
			}
		}
		if s.Fatal() {
			return
		}
		s.restore(level)
	}
	s.restore(searchBase)
	s.speculative = true
	explore(0, make(map[ConstraintID]uint32))
	if s.Fatal() {
		fatalConflict := cloneConflict(s.speculativeConflict)
		s.restore(base)
		s.speculative = false
		s.speculativeConflict = nil
		// Rollback restores the pre-choice failure bit. Fatal finalization is
		// independently unsuccessful regardless of the branch's mutations.
		s.failed = true
		if fatalConflict != nil && fatalConflict.code == CodeResourceLimit {
			s.reporter.error(fatalConflict.code, fatalConflict.message, fatalConflict.origin, fatalConflict.related...)
		} else {
			s.reporter.error(CodeResourceLimit, "fatal failure during inference choice evaluation", s.constraints[choices[0]].value.origin)
		}
		return
	}
	s.speculative = false
	s.speculativeConflict = nil
	if len(solutions) == 1 {
		s.restore(solutions[0].state)
		s.failed = inheritedFailure
		for id, alternative := range solutions[0].selections {
			s.selections[id] = alternative
		}
		for _, index := range choices {
			s.constraints[index].done = true
		}
		s.solveOrdinary()
		return
	}
	s.restore(base)
	for _, index := range choices {
		s.constraints[index].done = true
	}
	if len(solutions) == 0 && firstConflict != nil {
		s.conflict(firstConflict.code, firstConflict.message, firstConflict.origin, firstConflict.related...)
		return
	}
	message := "multiple global inference choice assignments remain viable"
	if len(solutions) == 0 {
		message = "no global inference choice assignment is viable"
	}
	s.conflict(CodeAmbiguous, message, s.constraints[choices[0]].value.origin)
}

func (s *Session) solveConstraintSet(values []Constraint) bool {
	for {
		changed := false
		for _, value := range values {
			if s.Fatal() {
				return false
			}
			if value.kind == constraintOneOf {
				// Nested choices are viable only if exactly one nested branch is viable.
				if !s.solveInlineChoice(value) {
					return false
				}
				continue
			}
			result := s.apply(value)
			if s.Fatal() {
				return false
			}
			if !result.success {
				return false
			}
			changed = changed || result.changed
		}
		if !changed {
			return !s.failed
		}
	}
}

func (s *Session) solveInlineChoice(value Constraint) bool {
	base := s.snapshot()
	parentConflict := s.speculativeConflict
	viable := make([]int, 0, len(value.alternatives))
	for index, alternative := range value.alternatives {
		if s.Fatal() {
			break
		}
		s.choiceStates++
		if s.choiceStates > s.config.MaxChoiceStates {
			s.restore(base)
			s.speculativeConflict = parentConflict
			s.conflict(CodeResourceLimit, "choice state limit exceeded", value.origin)
			return false
		}
		s.restore(base)
		s.failed = false
		s.speculativeConflict = nil
		if s.solveConstraintSet(alternative.Constraints) && !s.failed {
			viable = append(viable, index)
		}
		if s.Fatal() {
			return false
		}
	}
	if s.Fatal() {
		return false
	}
	fatalConflict := cloneConflict(s.speculativeConflict)
	s.restore(base)
	s.speculativeConflict = parentConflict
	if s.Fatal() {
		if fatalConflict != nil {
			s.speculativeConflict = fatalConflict
		}
		s.failed = true
		return false
	}
	if len(viable) != 1 {
		message := "nested inference choice has no viable alternative"
		if len(viable) > 1 {
			message = "nested inference choice has multiple viable alternatives"
		}
		s.conflict(CodeAmbiguous, message, value.origin)
		return false
	}
	return s.solveConstraintSet(value.alternatives[viable[0]].Constraints)
}

func (s *Session) defaultLiterals(inactive map[InferID]bool) {
	if s.Fatal() {
		return
	}
	builtins := s.program.builtins()
	for index := range s.cells {
		if s.Fatal() {
			return
		}
		id := InferID(index + 1)
		if s.find(id) != id {
			continue
		}
		if inactive[id] {
			continue
		}
		cell := &s.cells[index]
		if cell.error || cell.known != 0 || cell.shape != nil || len(cell.literals) == 0 {
			continue
		}
		if s.literalDefaultBlocked(id) {
			continue
		}
		target := builtins.Int
		if cell.literals[0].kind == literalFloat {
			target = builtins.F64
		}
		if s.fitLiterals(cell.literals, target, cell.origin) && s.checkCapabilities(cell.capabilities, target, cell.origin) {
			if s.Fatal() {
				return
			}
			cell.known = target
		} else {
			if s.Fatal() {
				return
			}
			cell.error = true
		}
	}
}

func (s *Session) literalDefaultBlocked(root InferID) bool {
	for _, entry := range s.constraints {
		if entry.done {
			continue
		}
		switch entry.value.kind {
		case constraintHasField, constraintHasComponent, constraintSelectMethod, constraintCallMember, constraintCallable, constraintIndexable, constraintSliceable:
			if s.termHasRoot(entry.value.a, root) || s.termHasRoot(entry.value.b, root) {
				return true
			}
			for _, term := range entry.value.explicit {
				if s.termHasRoot(term, root) {
					return true
				}
			}
			for _, argument := range entry.value.arguments {
				if s.termHasRoot(argument.Source, root) || s.termHasRoot(argument.Destination, root) {
					return true
				}
			}
		}
	}
	return false
}

func (s *Session) termHasRoot(term Term, root InferID) bool {
	if term.kind == termKnown || term.kind == termError || !term.belongs(s.token) {
		return false
	}
	return s.find(term.id) == root
}

func (s *Session) finalizeUnresolved(inactive map[InferID]bool) {
	if s.Fatal() {
		return
	}
	for index := range s.cells {
		id := InferID(index + 1)
		if s.find(id) != id {
			continue
		}
		if inactive[id] {
			continue
		}
		cell := &s.cells[index]
		if cell.error || cell.known != 0 {
			continue
		}
		s.reporter.error(CodeUnresolved, "inference variable has no unique semantic type", cell.origin)
		if s.Fatal() {
			return
		}
		cell.error = true
		s.failed = true
	}
}

func (s *Session) freezeSolution() *Solution {
	var programIdentity *programToken
	if s.program != nil {
		programIdentity = s.program.ensureIdentity()
	}
	result := &Solution{
		programIdentity: programIdentity, solveIdentity: s.token, finalized: true,
		successful: !s.invalid && !s.failed,
		symbols:    make(map[symbol.SymbolID]TypeResult), syntax: make(map[symbol.SyntaxRef]TypeResult),
		requirements: make(map[symbol.SymbolID][]Requirement), instantiations: make(map[symbol.SyntaxRef]Instantiation),
		methods: make(map[symbol.SyntaxRef]MethodSelection), selections: make(map[ConstraintID]uint32),
		slots: make(map[SlotID]TypeResult),
	}
	for _, id := range sortedSymbolIDs(s.symbolRoots) {
		result.manifest.symbols = append(result.manifest.symbols, id)
		result.symbols[id] = s.termResult(s.symbolRoots[id])
		if result.symbols[id].State == TypeError {
			result.successful = false
		}
	}
	refs := make([]symbol.SyntaxRef, 0, len(s.syntaxRoots))
	for ref := range s.syntaxRoots {
		refs = append(refs, ref)
	}
	sort.Slice(refs, func(i, j int) bool { return refLess(refs[i], refs[j]) })
	for _, ref := range refs {
		result.manifest.syntax = append(result.manifest.syntax, ref)
		result.syntax[ref] = s.termResult(s.syntaxRoots[ref])
		if result.syntax[ref].State == TypeError {
			result.successful = false
		}
	}
	for _, req := range s.requirements {
		result.requirements[req.Owner] = append(result.requirements[req.Owner], req)
	}
	for owner := range result.requirements {
		sort.SliceStable(result.requirements[owner], func(i, j int) bool {
			return requirementLess(result.requirements[owner][i], result.requirements[owner][j])
		})
	}
	requirementOwners := make([]int, 0, len(result.requirements))
	for owner := range result.requirements {
		requirementOwners = append(requirementOwners, int(owner))
	}
	sort.Ints(requirementOwners)
	for _, owner := range requirementOwners {
		id := symbol.SymbolID(owner)
		result.manifest.requirements = append(result.manifest.requirements, requirementTableManifest{owner: id, count: uint32(len(result.requirements[id]))})
	}
	instRefs := make([]symbol.SyntaxRef, 0, len(s.instantiations))
	for ref := range s.instantiations {
		instRefs = append(instRefs, ref)
	}
	sort.Slice(instRefs, func(i, j int) bool { return refLess(instRefs[i], instRefs[j]) })
	for _, ref := range instRefs {
		published := s.instantiations[ref]
		if published.guarded {
			if s.Fatal() {
				continue
			}
			selected, ok := s.selections[published.choice.constraint]
			if !ok || selected != published.alternative {
				continue
			}
		}
		result.manifest.instantiations = append(result.manifest.instantiations, ref)
		arguments := make([]TypeResult, len(published.arguments))
		for i, term := range published.arguments {
			arguments[i] = s.termResult(term)
			if arguments[i].State == TypeError {
				result.successful = false
			}
		}
		result.instantiations[ref] = Instantiation{Site: ref, Generic: published.generic, Arguments: arguments}
	}
	methodRefs := make([]symbol.SyntaxRef, 0, len(s.methodStates))
	for ref, state := range s.methodStates {
		if state.ready {
			methodRefs = append(methodRefs, ref)
		}
	}
	sort.Slice(methodRefs, func(i, j int) bool { return refLess(methodRefs[i], methodRefs[j]) })
	for _, ref := range methodRefs {
		result.manifest.methods = append(result.manifest.methods, ref)
		state := s.methodStates[ref]
		arguments := make([]TypeResult, len(state.arguments))
		for i, term := range state.arguments {
			arguments[i] = s.termResult(term)
			if arguments[i].State == TypeError {
				result.successful = false
			}
		}
		result.methods[ref] = MethodSelection{Site: ref, Method: state.method, Arguments: arguments}
	}
	if !s.Fatal() {
		selectionIDs := make([]int, 0, len(s.selections))
		for id := range s.selections {
			selectionIDs = append(selectionIDs, int(id))
		}
		sort.Ints(selectionIDs)
		for _, rawID := range selectionIDs {
			id := ConstraintID(rawID)
			selected := s.selections[id]
			result.selections[id] = selected
			alternatives := uint32(0)
			if id.IsValid() && uint64(id) <= uint64(len(s.constraints)) {
				alternatives = uint32(len(s.constraints[id-1].value.alternatives))
			}
			result.manifest.selections = append(result.manifest.selections, selectionTableManifest{id: id, alternative: selected, alternatives: alternatives})
		}
	}
	for _, published := range s.slots {
		if published.guarded {
			if s.Fatal() {
				continue
			}
			selected, ok := s.selections[published.choice.constraint]
			if !ok || selected != published.alternative {
				continue
			}
		}
		value := s.termResult(published.term)
		result.slots[published.id] = value
		result.orderedSlots = append(result.orderedSlots, SlotType{Slot: published.id, Result: value})
		result.manifest.slots = append(result.manifest.slots, published.id)
		if value.State == TypeError {
			result.successful = false
		}
	}
	if s.program != nil {
		result.storeLength = s.program.storeLength()
	}
	return result
}

func requirementLess(a, b Requirement) bool {
	if a.Origin.Syntax != b.Origin.Syntax {
		return refLess(a.Origin.Syntax, b.Origin.Syntax)
	}
	if a.Origin.Span.Source != b.Origin.Span.Source {
		return a.Origin.Span.Source < b.Origin.Span.Source
	}
	if a.Origin.Span.Start != b.Origin.Span.Start {
		return a.Origin.Span.Start < b.Origin.Span.Start
	}
	if a.Origin.Span.End != b.Origin.Span.End {
		return a.Origin.Span.End < b.Origin.Span.End
	}
	if a.Kind != b.Kind {
		return a.Kind < b.Kind
	}
	if a.Parameter != b.Parameter {
		return a.Parameter < b.Parameter
	}
	if a.LiteralKind != b.LiteralKind {
		return a.LiteralKind < b.LiteralKind
	}
	if a.Numerator != b.Numerator {
		return a.Numerator < b.Numerator
	}
	if a.Denominator != b.Denominator {
		return a.Denominator < b.Denominator
	}
	return a.Origin.Role < b.Origin.Role
}

func (s *Session) termResult(term Term) TypeResult {
	if id, ok := s.resolvedType(term); ok {
		return TypeResult{State: TypeFinal, Type: id}
	}
	return TypeResult{State: TypeError}
}

func refLess(a, b symbol.SyntaxRef) bool {
	if a.Module != b.Module {
		return a.Module < b.Module
	}
	return a.Node < b.Node
}
