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
)

// switchIsExhaustive determines whether an else-less controlSwitch is
// exhaustive for the purpose of exit-set computation. It answers only the
// narrow question "does this switch contribute fallthrough due to missing
// cases?" — it does not perform full legality validation (duplicate
// detection, category rejection, etc.), which belongs to a later pass.
//
// A switch is exhaustive (contributes no extra fallthrough) when:
//   - bool subject with both true and false case arms present, or
//   - enum/tagged-union subject with every variant covered by some case arm.
//
// For any other subject category or an unresolved subject type, it returns
// false (conservatively treats the switch as potentially fall-through).
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
	coveredEnumVariants := make(map[symbol.SymbolID]bool)
	variantBySyntax := make(map[symbol.SyntaxRef]symbol.SymbolID)
	for _, record := range handoff.Records.Records() {
		if !activeOperatorRecord(handoff, record.Header) || record.Member == nil || record.Member.Kind != memberVariant || record.Member.Member == 0 {
			continue
		}
		variantBySyntax[record.Header.Syntax] = record.Member.Member
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
					// Integer subjects are not exhaustively enumerable here.
				}
			}
		}
	}

	// Bool exhaustiveness: both true and false must be covered.
	if builtin, ok := typeKey.Builtin(); ok && builtin == types.Bool {
		return coveredBools[true] && coveredBools[false]
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

type controlExitKind uint8

const (
	exitFallthrough controlExitKind = iota + 1
	exitReturn
	exitBreak
	exitContinue
	exitDiverge
)

type controlExit struct {
	kind   controlExitKind
	target controlID
}

// validateControlFlow performs the tree-free flow and statement legality pass.
// It deliberately does not validate switch categories or defer edges; those
// consumers use the same exit representation in later validation passes.
func validateControlFlow(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.GenerationHadErrors || records == nil || handoff.Semantics == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	report := func(ctrl *controlRecord, code diagnostic.Code, severity diagnostic.Severity, message string) {
		if severity == diagnostic.Error {
			failed = true
		}
		var primary diagnostic.Label
		if ctrl != nil {
			primary = diagnostic.Label{Span: ctrl.Header.Span}
		}
		reporter.add(diagnostic.Diagnostic{Severity: severity, Code: code, Message: message, Primary: primary})
	}

	controls := handoff.Records.Controls()
	retained := handoff.Records.Records()
	byRegion := make(map[controlID][]*controlRecord)
	owner := make(map[controlID]*controlRecord)
	bySyntax := make(map[symbol.SyntaxRef]*controlRecord)
	for i := range retained {
		if retained[i].Control == nil || !activeOperatorRecord(handoff, retained[i].Header) {
			continue
		}
		ctrl := retained[i].Control
		byRegion[ctrl.Region] = append(byRegion[ctrl.Region], ctrl)
		bySyntax[ctrl.Header.Syntax] = ctrl
		if regionOwningControl(ctrl.Kind) {
			owner[ctrl.Region] = ctrl
		}
	}

	contains := func(exits []controlExit, want controlExit) bool {
		for _, exit := range exits {
			if exit == want {
				return true
			}
		}
		return false
	}
	addExit := func(exits []controlExit, exit controlExit) []controlExit {
		if !contains(exits, exit) {
			return append(exits, exit)
		}
		return exits
	}
	ancestor := func(from, target controlID) bool {
		if from == 0 || target == 0 || uint64(from) > uint64(len(controls)) || uint64(target) > uint64(len(controls)) {
			return false
		}
		for current := from; current != 0; current = controls[current-1].Parent {
			if current == target {
				return true
			}
		}
		return false
	}

	var evalRegion func(controlID, bool) []controlExit
	var evalRecord func(*controlRecord, bool) []controlExit
	var evalSequence func([]*controlRecord, bool) []controlExit
	warningSuppressed := 0

	resultType := func(ctrl *controlRecord) (types.TypeID, bool) {
		if ctrl == nil || ctrl.Callable.Symbol == 0 {
			return 0, false
		}
		signature, ok := handoff.Semantics.Signature(ctrl.Callable.Symbol)
		if !ok {
			return 0, false
		}
		template, ok := handoff.Semantics.Template(signature.Result)
		return template.Known, ok && template.Kind == infer.TemplateKnown
	}
	isVoidResult := func(ctrl *controlRecord) bool {
		result, ok := resultType(ctrl)
		return ok && result == handoff.Semantics.Types().Builtins().Void
	}

	valuePrintable := func(value valueID) bool {
		resolved, ok := records.Root(value)
		if !ok || resolved.State != infer.TypeFinal {
			return false
		}
		key, ok := handoff.Semantics.Types().Key(resolved.Type)
		if !ok {
			return false
		}
		builtin, ok := key.Builtin()
		return ok && (builtin == types.Bool || builtin == types.Char || builtin == types.Str || isIntegerBuiltin(builtin) || isFloatBuiltin(builtin))
	}

	evalRecord = func(ctrl *controlRecord, reachable bool) []controlExit {
		if ctrl == nil {
			return nil
		}
		switch ctrl.Kind {
		case controlBlock:
			return evalRegion(ctrl.Region, reachable)
		case controlReturn:
			if len(ctrl.Values) != 0 && isVoidResult(ctrl) {
				report(ctrl, CodeMissingReturn, diagnostic.Error, "value return is forbidden for void function")
			}
			return []controlExit{{kind: exitReturn}}
		case controlBreak, controlContinue:
			if ctrl.Target == 0 || !ancestor(ctrl.Region, ctrl.Target) {
				report(ctrl, CodeInvalidTarget, diagnostic.Error, "jump has a missing or inconsistent target")
				return []controlExit{{kind: map[controlKind]controlExitKind{controlBreak: exitBreak, controlContinue: exitContinue}[ctrl.Kind], target: ctrl.Target}}
			}
			targetOwner := owner[ctrl.Target]
			if targetOwner == nil || (ctrl.Kind == controlContinue && targetOwner.Kind != controlWhile && targetOwner.Kind != controlFor && targetOwner.Kind != controlRangeLoop) || (ctrl.Kind == controlBreak && targetOwner.Kind != controlWhile && targetOwner.Kind != controlFor && targetOwner.Kind != controlRangeLoop && targetOwner.Kind != controlSwitch) {
				report(ctrl, CodeInvalidTarget, diagnostic.Error, "jump target is not an enclosing control target")
			}
			kind := exitBreak
			if ctrl.Kind == controlContinue {
				kind = exitContinue
			}
			return []controlExit{{kind: kind, target: ctrl.Target}}
		case controlIf:
			var result []controlExit
			for _, child := range ctrl.Composition {
				if arm := bySyntax[child.Arm]; arm != nil {
					for _, exit := range evalRecord(arm, reachable) {
						result = addExit(result, exit)
					}
				}
			}
			if !ctrl.ElsePresent {
				result = addExit(result, controlExit{kind: exitFallthrough})
			}
			return result
		case controlWhile, controlFor, controlRangeLoop:
			var body []controlExit
			for _, child := range ctrl.Composition {
				if child.Role == roleBody {
					if arm := bySyntax[child.Arm]; arm != nil {
						body = evalRecord(arm, reachable)
					}
				}
			}
			result := []controlExit{}
			breakFound := false
			for _, exit := range body {
				switch {
				case exit.kind == exitContinue && exit.target == ctrl.Region:
				case exit.kind == exitBreak && exit.target == ctrl.Region:
					breakFound = true
				default:
					result = addExit(result, exit)
				}
			}
			infinite := ctrl.Kind == controlFor && !ctrl.ConditionPresent
			if !infinite && ctrl.ConditionPresent && len(ctrl.Values) > 0 {
				if _, ok := records.Root(ctrl.Values[0].Value); ok {
					if syntaxRoot, found := handoff.Roots.Root(ctrl.Values[0].Value); found {
						if constant, found := records.Constant(syntaxRoot.Syntax); found && constant.State == constantKnown && constant.Value.Kind == constantBoolean && constant.Value.Boolean {
							infinite = true
						}
					}
				}
			}
			if !infinite || breakFound {
				result = addExit(result, controlExit{kind: exitFallthrough})
			}
			return result
		case controlSwitch, controlSwitchCase:
			var result []controlExit
			for _, child := range ctrl.Composition {
				if arm := bySyntax[child.Arm]; arm != nil {
					for _, exit := range evalRecord(arm, reachable) {
						if exit.kind == exitBreak && exit.target == ctrl.Region {
							result = addExit(result, controlExit{kind: exitFallthrough})
						} else {
							result = addExit(result, exit)
						}
					}
				}
			}
			if ctrl.Kind == controlSwitch && !ctrl.ElsePresent && !switchIsExhaustive(handoff, records, ctrl, bySyntax) {
				result = addExit(result, controlExit{kind: exitFallthrough})
			}
			return result
		case controlExpression:
			if ctrl.StatementForm == statementDiscard && len(ctrl.Values) != 0 {
				value, ok := records.Root(ctrl.Values[0].Value)
				void := false
				if ok && value.State == infer.TypeFinal {
					void = value.Type == handoff.Semantics.Types().Builtins().Void
				}
				if !void {
					report(ctrl, CodeStatementForm, diagnostic.Error, "expression statement discards a non-void value")
				}
			}
		case controlPrint:
			for _, value := range ctrl.Values {
				if !valuePrintable(value.Value) {
					report(ctrl, CodeStatementForm, diagnostic.Error, "print operand is not printable")
				}
			}
		}
		return []controlExit{{kind: exitFallthrough}}
	}

	evalSequence = func(sequence []*controlRecord, reachable bool) []controlExit {
		result := []controlExit{}
		canReach := reachable
		unreachableReported := false
		for _, ctrl := range sequence {
			if ctrl.Kind == controlFunction {
				continue
			}
			if !canReach {
				if warningSuppressed == 0 && !unreachableReported {
					report(ctrl, CodeUnreachable, diagnostic.Warning, "statement is unreachable")
					unreachableReported = true
				}
				warningSuppressed++
			}
			exits := evalRecord(ctrl, canReach)
			if !canReach {
				warningSuppressed--
				continue
			}
			for _, exit := range exits {
				if exit.kind != exitFallthrough {
					result = addExit(result, exit)
				}
			}
			canReach = contains(exits, controlExit{kind: exitFallthrough})
		}
		if canReach {
			result = addExit(result, controlExit{kind: exitFallthrough})
		}
		return result
	}

	evalRegion = func(region controlID, reachable bool) []controlExit {
		sequence := byRegion[region]
		if first := owner[region]; first != nil && first.Kind == controlFunction && uint64(region) <= uint64(len(controls)) {
			for _, child := range controls[region-1].Children {
				if childOwner := owner[child]; childOwner != nil {
					sequence = append(sequence, childOwner)
				}
			}
			return evalSequence(sequence, reachable)
		}
		if len(sequence) == 0 {
			return []controlExit{{kind: exitFallthrough}}
		}
		if first := owner[region]; first != nil && (first.Kind == controlBlock || first.Kind == controlFunction) {
			for i, ctrl := range sequence {
				if ctrl == first {
					sequence = sequence[i+1:]
					break
				}
			}
			if uint64(region) <= uint64(len(controls)) {
				for _, child := range controls[region-1].Children {
					if childOwner := owner[child]; childOwner != nil {
						sequence = append(sequence, childOwner)
					}
				}
			}
		}
		return evalSequence(sequence, reachable)
	}

	for _, root := range controls {
		if root.Parent != 0 {
			continue
		}
		function := owner[root.ID]
		if function == nil || function.Kind != controlFunction {
			continue
		}
		exits := evalRegion(root.ID, true)
		if !isVoidResult(function) && contains(exits, controlExit{kind: exitFallthrough}) {
			report(function, CodeMissingReturn, diagnostic.Error, "non-void function can fall through without returning")
		}
	}
	reporter.flush()
	return !failed
}
