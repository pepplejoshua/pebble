package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const (
	CodeInvalidTarget diagnostic.Code = "C0611"
	CodeMissingReturn diagnostic.Code = "C0607"
	CodeStatementForm diagnostic.Code = "C0612"
	CodeUnreachable   diagnostic.Code = "C0618"
	// CodeUnboundRangeIterator rejects a range loop authored without the
	// explicit `: name` iterator (`loop start..end { ... }`). V2 requires
	// the bound form by policy (unlike V1, which synthesizes an implicit
	// name), so the checker enforces it here at the loop's own span instead
	// of leaving the omission for the backend's rangeNode.Symbol == 0 guard.
	CodeUnboundRangeIterator diagnostic.Code = "C0622"
)

// switchIsExhaustive determines whether an else-less controlSwitch is
// exhaustive for the purpose of exit-set computation. It answers only the
// narrow question "does this switch contribute fallthrough due to missing
// cases?" — it does not perform full legality validation (duplicate
// detection, category rejection, etc.), which belongs to a later pass.
//
// A switch is exhaustive (contributes no extra fallthrough) when:
//   - bool subject with both true and false case arms present, or
//   - u8 subject with every value in 0..255 covered, or
//   - i8 subject with every value in -128..127 covered, or
//   - enum/tagged-union subject with every variant covered by some case arm.
//
// For any other subject category or an unresolved subject type, it returns
// false (conservatively treats the switch as potentially fall-through).
// Wider integer widths (u16/i16 and up) are intentionally not enumerated:
// their domains are too large to prove exhaustive case-by-case, so they
// conservatively always require a fallback arm.
func switchIsExhaustive(handoff *solveHandoff, records *solvedRecords, ctrl *controlRecord, bySyntax map[symbol.SyntaxRef]*controlRecord) bool {
	if ctrl == nil || ctrl.Kind != controlSwitch || ctrl.ElsePresent {
		return false
	}

	// Resolve the subject value's type.
	if len(ctrl.Values) == 0 {
		return false
	}
	subjectValue := ctrl.Values[0].Value
	if subjectValue == 0 {
		return false
	}
	typeResult, ok := records.Root(subjectValue)
	if !ok || typeResult.State != infer.TypeFinal {
		return false
	}
	typeSnapshot := handoff.Semantics.Types()
	if typeSnapshot == nil {
		return false
	}
	typeKey, ok := typeSnapshot.Key(typeResult.Type)
	if !ok {
		return false
	}

	// Collect covered case constants by iterating the switch's composition.
	coveredBools := make(map[bool]bool)
	coveredIntegers := make(map[int64]bool)
	coveredEnumVariants := make(map[symbol.SymbolID]bool)
	variantBySyntax := make(map[symbol.SyntaxRef]symbol.SymbolID)
	for _, record := range handoff.Records.Records() {
		if !activeOperatorRecord(handoff, record.Header) {
			continue
		}
		if record.Member != nil && record.Member.Kind == memberVariant && record.Member.Member != 0 {
			variantBySyntax[record.Header.Syntax] = record.Member.Member
			continue
		}
		// A base-less `.name` switch case label (e.g. `case .red:`) produces an
		// aggregateEnumVariant/aggregateTaggedVariant aggregate record, not a
		// memberVariant member record, because the resolver defers partial-member
		// names — mirrors validateSwitches' identical indexing in
		// switch_validation.go and caseVariantMember's doc comment there.
		if record.Aggregate != nil && (record.Aggregate.Kind == aggregateEnumVariant || record.Aggregate.Kind == aggregateTaggedVariant) && len(record.Aggregate.Fields) != 0 {
			if member := caseVariantMember(handoff, record.Aggregate); member != 0 {
				variantBySyntax[record.Header.Syntax] = member
			}
		}
	}

	for _, child := range ctrl.Composition {
		if child.Role != roleCase {
			continue
		}
		arm, ok := bySyntax[child.Arm]
		if !ok || arm == nil {
			continue
		}
		for _, val := range arm.Values {
			if val.Role != valueCase {
				continue
			}
			if variant, found := variantBySyntax[val.Syntax]; found {
				coveredEnumVariants[variant] = true
				continue
			}
			if constResult, found := records.Constant(val.Syntax); found && constResult.State == constantKnown {
				switch constResult.Value.Kind {
				case constantBoolean:
					coveredBools[constResult.Value.Boolean] = true
				case constantInteger:
					// Only finite-width integer subjects with enumerable domains
					// (u8/i8) can be proven exhaustive, so track covered values
					// here and let the width-specific check below consume them.
					// Values outside int64 range are never in a u8/i8 domain and
					// can be skipped safely.
					if constResult.Value.Integer != nil && constResult.Value.Integer.IsInt64() {
						coveredIntegers[constResult.Value.Integer.Int64()] = true
					}
				}
			}
		}
	}

	// Bool exhaustiveness: both true and false must be covered.
	if builtin, ok := typeKey.Builtin(); ok && builtin == types.Bool {
		return coveredBools[true] && coveredBools[false]
	}

	if builtin, ok := typeKey.Builtin(); ok {
		return integerSwitchIsExhaustive(builtin, coveredIntegers)
	}

	// Enum/tagged-union exhaustiveness: every variant must be covered.
	if declSymbol, _, ok := typeKey.Nominal(); ok {
		typeDecl, ok := handoff.Semantics.TypeDeclaration(declSymbol)
		if !ok {
			return false
		}
		if typeDecl.Nominal != infer.NominalEnum && typeDecl.Nominal != infer.NominalTaggedUnion {
			return false
		}
		// Use the declaration-complete ordered member list (not the filtered
		// TypeDeclaration.Members), filtered to SymbolVariant kind.
		resolution := handoff.Semantics.Resolution()
		if resolution == nil {
			return false
		}
		allMembers := resolution.Members(declSymbol)
		variantCount := 0
		for _, memberID := range allMembers {
			sym, ok := resolution.Symbols.Symbol(memberID)
			if !ok || sym.Kind != symbol.SymbolVariant {
				continue
			}
			variantCount++
			if !coveredEnumVariants[memberID] {
				return false
			}
		}
		return variantCount > 0
	}

	return false
}

// integerSwitchIsExhaustive reports whether the covered values contain the
// complete domain of an integer type that the checker can enumerate. Wider
// integer domains are intentionally not enumerated and always return false.
func integerSwitchIsExhaustive(builtin types.BuiltinKind, covered map[int64]bool) bool {
	var min, max int64
	switch builtin {
	case types.U8:
		min, max = 0, 255
	case types.I8:
		min, max = -128, 127
	default:
		return false
	}
	for value := min; value <= max; value++ {
		if !covered[value] {
			return false
		}
	}
	return true
}

// auditControlArena rechecks the 06a freeze invariants on the frozen control
// region arena before flow analysis. For a GenerationHadErrors handoff it
// produces no diagnostics (the handoff is already known-damaged). For a clean
// handoff it independently re-verifies every tree-free invariant derivable
// from the frozen arena and records alone. Any disagreement emits C0619.
func auditControlArena(handoff *solveHandoff, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff.GenerationHadErrors {
		return true
	}
	reporter := newValidationReporter(diagnostics, config.MaxDiagnostics)
	fail := func(message string) bool {
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeGeneration,
			Message:  message,
		})
		reporter.flush()
		return false
	}

	controls := handoff.Records.Controls()
	records := handoff.Records.Records()

	// Phase 1: Arena structural invariants.
	if !auditControlArenaStructure(controls, fail) {
		return false
	}

	// Phase 2: Record region/target validity.
	if !auditControlRecordRegions(controls, records, fail) {
		return false
	}

	// Phase 3: One controlFunction per root; consistent callableRef.
	if !auditControlFunctionRoots(controls, records, fail) {
		return false
	}

	// Phase 4: Composition well-formedness per parent kind.
	if !auditControlCompositions(records, fail) {
		return false
	}

	// Phase 5: One matching frozen control record per arm; lexical placement.
	if !auditControlArmResolution(controls, records, fail) {
		return false
	}

	return true
}

// auditControlArenaStructure rechecks the 06a freeze invariants on the
// controlRegion arena: contiguous IDs, root depth one, parent-depth
// increments, each non-root in exactly one child list, edge count.
func auditControlArenaStructure(controls []controlRegion, fail func(string) bool) bool {
	if len(controls) == 0 {
		return true
	}

	// Contiguous IDs starting at 1.
	for i, region := range controls {
		if region.ID != controlID(i+1) {
			return fail("control region IDs are not contiguous starting at 1")
		}
	}

	// Root depth one; non-root parent-depth increments; parent has smaller ID.
	// Build expected child lists bottom-up from Parent fields.
	expectedChildren := make([][]controlID, len(controls))
	for i, region := range controls {
		if region.Parent == 0 {
			if region.Depth != 1 {
				return fail("root control region does not have depth 1")
			}
			continue
		}
		if !region.Parent.valid() || uint64(region.Parent) > uint64(i) {
			return fail("control region parent has invalid ID")
		}
		parent := controls[region.Parent-1]
		if region.Depth != parent.Depth+1 {
			return fail("control region depth does not equal parent depth plus one")
		}
		expectedChildren[region.Parent-1] = append(expectedChildren[region.Parent-1], region.ID)
	}

	// Cross-check each region's Children list: no duplicates, every entry
	// points back to this region as parent, and the set of expected children
	// (derived from Parent fields) exactly matches the Children slice.
	for i, region := range controls {
		if uint32(len(region.Children)) != uint32(len(expectedChildren[i])) {
			return fail("control region children count does not match derived child count")
		}
		seen := make(map[controlID]bool, len(region.Children))
		for _, child := range region.Children {
			if !child.valid() || uint64(child) > uint64(len(controls)) {
				return fail("control region Children contains invalid ID")
			}
			if seen[child] {
				return fail("control region Children contains duplicate entry")
			}
			seen[child] = true
			if controls[child-1].Parent != region.ID {
				return fail("control region Children entry does not point back to this parent")
			}
		}
		for _, expected := range expectedChildren[i] {
			if !seen[expected] {
				return fail("control region Children is missing an expected child")
			}
		}
	}

	// Edge count (non-root count) equals total regions minus root count.
	edges := 0
	roots := 0
	for _, region := range controls {
		edges += len(region.Children)
		if region.Parent == 0 {
			roots++
		}
	}
	if edges != len(controls)-roots {
		return fail("control arena edge count does not equal regions minus roots")
	}

	return true
}

// auditControlRecordRegions checks that every controlRecord.Region and
// controlRecord.Target (for break/continue) names a valid, in-range
// controlID within the frozen arena, and every deferRecord.Region does
// likewise.
func auditControlRecordRegions(controls []controlRegion, records []retainedRecord, fail func(string) bool) bool {
	for _, record := range records {
		if record.Control != nil {
			ctrl := record.Control
			if !ctrl.Region.valid() || uint64(ctrl.Region) > uint64(len(controls)) {
				return fail("control record references out-of-range region")
			}
			if ctrl.Target != 0 && (!ctrl.Target.valid() || uint64(ctrl.Target) > uint64(len(controls))) {
				return fail("control record references out-of-range target")
			}
		}
		if record.Defer != nil {
			if !record.Defer.Region.valid() || uint64(record.Defer.Region) > uint64(len(controls)) {
				return fail("defer record references out-of-range region")
			}
		}
	}
	return true
}

// auditControlFunctionRoots checks that exactly one controlFunction record
// exists per root region, and every non-function record in a function's
// region tree carries the same callableRef.
func auditControlFunctionRoots(controls []controlRegion, records []retainedRecord, fail func(string) bool) bool {
	roots := make(map[controlID]bool)
	for _, region := range controls {
		if region.Parent == 0 {
			roots[region.ID] = true
		}
	}

	functionRegions := make(map[controlID]bool)
	functionCallables := make(map[controlID]callableRef)
	for _, record := range records {
		if record.Control == nil || record.Control.Kind != controlFunction {
			continue
		}
		region := record.Control.Region
		if !roots[region] {
			return fail("controlFunction record does not belong to a function root")
		}
		if functionRegions[region] {
			return fail("multiple controlFunction records for one function root")
		}
		functionRegions[region] = true
		functionCallables[region] = record.Control.Callable
	}

	for _, region := range controls {
		if region.Parent != 0 {
			continue
		}
		if !functionRegions[region.ID] {
			return fail("function root has no controlFunction record")
		}
	}

	for _, record := range records {
		if record.Control == nil || record.Control.Kind == controlFunction {
			continue
		}
		root := findFunctionRoot(controls, record.Control.Region)
		if root == 0 {
			return fail("control record region does not descend from a function root")
		}
		expected := functionCallables[root]
		actual := record.Control.Callable
		if expected != actual {
			return fail("control record has inconsistent callable reference")
		}
	}

	return true
}

// auditControlCompositions independently re-verifies every region-owning
// controlRecord's Composition satisfies the same cardinality and order rules
// that validCompositionSequence enforces at generation time, without reusing
// that function. Uses the type-level allowedStructuralRoles table (a pure
// closed-set lookup in the same package) for role membership, but re-derives
// sequence invariants from scratch.
func auditControlCompositions(records []retainedRecord, fail func(string) bool) bool {
	for _, record := range records {
		if record.Control == nil {
			continue
		}
		ctrl := record.Control
		allowed := allowedStructuralRoles(ctrl.Kind)
		if allowed == nil {
			if len(ctrl.Composition) != 0 {
				return fail("non-region-owning control record carries a nonempty composition")
			}
			continue
		}
		if err := auditCompositionSequence(ctrl.Kind, ctrl.Composition, ctrl.ElsePresent); err != "" {
			return fail(err)
		}
	}
	return true
}

// auditCompositionSequence re-derives the per-kind composition sequence
// rules. Each kind's entries must appear in exactly the order and count
// specified. Returns an error message on violation, or "" if valid.
func auditCompositionSequence(kind controlKind, composition []structuralChild, elsePresent bool) string {
	switch kind {
	case controlIf:
		if elsePresent {
			if len(composition) != 2 || composition[0].Role != roleThen || composition[1].Role != roleElse {
				return "controlIf composition is not roleThen then roleElse"
			}
		} else {
			if len(composition) != 1 || composition[0].Role != roleThen {
				return "controlIf composition is not exactly one roleThen"
			}
		}
	case controlWhile, controlRangeLoop, controlSwitchCase:
		if len(composition) != 1 || composition[0].Role != roleBody {
			return "controlWhile/controlRangeLoop/controlSwitchCase composition is not exactly one roleBody"
		}
	case controlFor:
		index := 0
		if index < len(composition) && composition[index].Role == roleInitializer {
			index++
		}
		if index < len(composition) && composition[index].Role == roleUpdate {
			index++
		}
		if index != len(composition)-1 || composition[index].Role != roleBody {
			return "controlFor composition is not optional roleInitializer then optional roleUpdate then roleBody"
		}
	case controlSwitch:
		next := uint32(0)
		elseCount := 0
		for index, entry := range composition {
			switch entry.Role {
			case roleCase:
				if elseCount != 0 || entry.Ordinal != next {
					return "controlSwitch composition has non-contiguous or out-of-order roleCase ordinals"
				}
				next++
			case roleElse:
				if elseCount != 0 || index != len(composition)-1 {
					return "controlSwitch composition has roleElse not at the end or multiple roleElse"
				}
				elseCount++
			default:
				return "controlSwitch composition contains an invalid role"
			}
		}
		if elsePresent && elseCount != 1 {
			return "controlSwitch claims ElsePresent but has no roleElse"
		}
		if !elsePresent && elseCount != 0 {
			return "controlSwitch has roleElse but does not claim ElsePresent"
		}
	default:
		if len(composition) != 0 {
			return "unexpected nonempty composition for kind"
		}
	}
	return ""
}

// auditControlArmResolution checks that for each structuralChild.Arm in a
// region-owning record's Composition, exactly one other active controlRecord
// exists whose Header.Syntax matches the arm, and that lexical placement is
// correct: region-owning arms have their Region as a child of the parent
// record's region; leaf arms have Region equal to the parent record's region.
func auditControlArmResolution(controls []controlRegion, records []retainedRecord, fail func(string) bool) bool {
	// Build syntax-to-record index.
	bySyntax := make(map[symbol.SyntaxRef][]*retainedRecord, len(records))
	for i := range records {
		if records[i].Control != nil {
			bySyntax[records[i].Header.Syntax] = append(bySyntax[records[i].Header.Syntax], &records[i])
		}
	}

	for _, record := range records {
		if record.Control == nil {
			continue
		}
		if allowedStructuralRoles(record.Control.Kind) == nil {
			continue
		}
		ctrl := record.Control
		for _, entry := range ctrl.Composition {
			matches := bySyntax[entry.Arm]
			if len(matches) != 1 {
				return fail("structural composition arm does not resolve to exactly one control record")
			}
			armRecord := matches[0]
			if regionOwningControl(armRecord.Control.Kind) {
				if !armRecord.Control.Region.valid() || uint64(armRecord.Control.Region) > uint64(len(controls)) {
					return fail("structural composition region-owning arm has invalid region")
				}
				if controls[armRecord.Control.Region-1].Parent != ctrl.Region {
					return fail("structural composition region-owning arm has the wrong parent region")
				}
			} else if armRecord.Control.Region != ctrl.Region {
				return fail("structural composition leaf arm names a region other than its parent's")
			}
		}
	}
	return true
}
