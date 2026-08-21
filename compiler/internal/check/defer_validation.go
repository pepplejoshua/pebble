package check

import (
	"fmt"
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

const CodeInvalidDefer diagnostic.Code = "C0613"

// deferExit is the flow exit representation needed by defer validation. The
// ordinary flow validator intentionally omits source regions because it only
// needs exit identity; defer expansion also needs the region at the edge's
// source.
type deferExit struct {
	kind   controlExitKind
	target controlID
	source controlID
}

func validateDefers(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if !canContinueWithPartial(handoff, config) || handoff.Solution == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	retained := handoff.Records.Records()
	controls := handoff.Records.Controls()
	deferByRegion := make(map[controlID][]deferRecord)
	byRegion := make(map[controlID][]*controlRecord)
	owner := make(map[controlID]*controlRecord)
	bySyntax := make(map[symbol.SyntaxRef]*controlRecord)
	variantBySyntax := collectVariantBySyntax(handoff)
	for i := range retained {
		record := &retained[i]
		if !activeOperatorRecord(handoff, record.Header) {
			continue
		}
		if record.Defer != nil {
			deferByRegion[record.Defer.Region] = append(deferByRegion[record.Defer.Region], *record.Defer)
		}
		if record.Control == nil {
			continue
		}
		byRegion[record.Control.Region] = append(byRegion[record.Control.Region], record.Control)
		bySyntax[record.Control.Header.Syntax] = record.Control
		if regionOwningControl(record.Control.Kind) {
			owner[record.Control.Region] = record.Control
		}
	}

	// descendant reports whether child lies within the region subtree rooted at
	// root, root itself included. A jump reachable inside a deferred compound
	// statement is contained exactly when its target is within the deferred
	// statement's own region subtree; any other target (an enclosing loop or
	// switch, a sibling region, the function root) exits past the deferred
	// statement's boundary. The IR builder cannot terminate on such an exit: its
	// defer chain walks the crossed regions up to the deferred statement's
	// registered region and rebuilds the deferred statement itself (see
	// deferChainFor in ir_builder_control.go), so the checker rejects it here as
	// C0613 instead of letting construction recurse.
	descendant := func(root, child controlID) bool {
		for current := child; current != 0; {
			if uint64(current) > uint64(len(controls)) {
				return false
			}
			if current == root {
				return true
			}
			current = controls[current-1].Parent
		}
		return false
	}
	// deferredExitForbidden reports whether the statement directly deferred by
	// record is itself an exit (return/break/continue/nested defer): the
	// statement-level C0613 shape.
	deferredExitForbidden := func(statement *controlRecord) bool {
		if statement == nil {
			return false
		}
		switch statement.Kind {
		case controlReturn, controlBreak, controlContinue, controlDefer:
			return true
		}
		return false
	}
	// regionHasEscapingExit reports whether the region subtree rooted at region
	// contains an exit that crosses the deferred statement's own region boundary:
	// a return, a break/continue whose target is not within the boundary subtree,
	// or a nested defer. skip names the deferred statement's own record, which
	// owns the walk's root region and must not be treated as an exit.
	var regionHasEscapingExit func(controlID, controlID, *controlRecord) bool
	regionHasEscapingExit = func(region, boundary controlID, skip *controlRecord) bool {
		if uint64(region) > uint64(len(controls)) {
			return false
		}
		for _, ctrl := range byRegion[region] {
			if ctrl == skip || !activeOperatorRecord(handoff, ctrl.Header) {
				continue
			}
			switch ctrl.Kind {
			case controlReturn:
				return true
			case controlBreak, controlContinue:
				if ctrl.Target != 0 && !descendant(boundary, ctrl.Target) {
					return true
				}
			case controlDefer:
				// A defer registered inside a deferred statement runs only when
				// the outer defer fires and has no region of its own to exit, so
				// it is a deferred nested defer (C0613).
				return true
			}
		}
		if uint64(region) <= uint64(len(controls)) {
			for _, child := range controls[region-1].Children {
				if regionHasEscapingExit(child, boundary, nil) {
					return true
				}
			}
		}
		return false
	}
	for _, record := range retained {
		if record.Defer == nil || !activeOperatorRecord(handoff, record.Header) {
			continue
		}
		statement := bySyntax[record.Defer.Statement]
		forbidden := deferredExitForbidden(statement)
		if !forbidden && statement != nil && regionOwningControl(statement.Kind) {
			forbidden = regionHasEscapingExit(statement.Region, statement.Region, statement)
		}
		if forbidden {
			reporter.add(diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeInvalidDefer,
				Message:  "deferred return, break, continue, or defer is invalid",
				Primary:  diagnostic.Label{Span: record.Defer.Header.Span},
			})
			failed = true
		}
	}

	addExit := func(exits []deferExit, want deferExit) []deferExit {
		for _, exit := range exits {
			if exit == want {
				return exits
			}
		}
		return append(exits, want)
	}
	var evalRegion func(controlID) []deferExit
	var evalRecord func(*controlRecord) []deferExit
	var evalSequence func([]*controlRecord) []deferExit
	withSource := func(exits []deferExit, source controlID) []deferExit {
		for index := range exits {
			if exits[index].kind == exitFallthrough {
				exits[index].source = source
			}
		}
		return exits
	}
	evalRecord = func(ctrl *controlRecord) []deferExit {
		if ctrl == nil {
			return nil
		}
		switch ctrl.Kind {
		case controlBlock:
			return evalRegion(ctrl.Region)
		case controlReturn:
			return []deferExit{{kind: exitReturn, source: ctrl.Region}}
		case controlBreak, controlContinue:
			kind := exitBreak
			if ctrl.Kind == controlContinue {
				kind = exitContinue
			}
			return []deferExit{{kind: kind, target: ctrl.Target, source: ctrl.Region}}
		case controlIf:
			var result []deferExit
			for _, child := range ctrl.Composition {
				if arm := bySyntax[child.Arm]; arm != nil {
					for _, exit := range evalRecord(arm) {
						result = addExit(result, exit)
					}
				}
			}
			if !ctrl.ElsePresent {
				result = addExit(result, deferExit{kind: exitFallthrough})
			}
			return result
		case controlWhile, controlFor, controlRangeLoop:
			var body []deferExit
			for _, child := range ctrl.Composition {
				if child.Role == roleBody {
					if arm := bySyntax[child.Arm]; arm != nil {
						body = evalRecord(arm)
					}
				}
			}
			result := []deferExit{}
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
			if ctrl.Kind != controlFor || ctrl.ConditionPresent || breakFound {
				result = addExit(result, deferExit{kind: exitFallthrough})
			}
			return result
		case controlSwitch, controlSwitchCase:
			var result []deferExit
			for _, child := range ctrl.Composition {
				if arm := bySyntax[child.Arm]; arm != nil {
					for _, exit := range evalRecord(arm) {
						if exit.kind == exitBreak && exit.target == ctrl.Region {
							result = addExit(result, deferExit{kind: exitFallthrough})
						} else {
							result = addExit(result, exit)
						}
					}
				}
			}
			if ctrl.Kind == controlSwitch && !ctrl.ElsePresent && !switchIsExhaustive(handoff, records, ctrl, bySyntax, variantBySyntax) {
				result = addExit(result, deferExit{kind: exitFallthrough})
			}
			return result
		default:
			return []deferExit{{kind: exitFallthrough}}
		}
	}
	evalSequence = func(sequence []*controlRecord) []deferExit {
		result := []deferExit{}
		canReach := true
		for _, ctrl := range sequence {
			if ctrl.Kind == controlFunction {
				continue
			}
			exits := evalRecord(ctrl)
			if !canReach {
				continue
			}
			for _, exit := range exits {
				if exit.kind != exitFallthrough {
					result = addExit(result, exit)
				}
			}
			canReach = false
			for _, exit := range exits {
				if exit.kind == exitFallthrough {
					canReach = true
					break
				}
			}
		}
		if canReach {
			result = addExit(result, deferExit{kind: exitFallthrough})
		}
		return result
	}
	evalRegion = func(region controlID) []deferExit {
		sequence := append([]*controlRecord(nil), byRegion[region]...)
		if first := owner[region]; first != nil && (first.Kind == controlBlock || first.Kind == controlFunction) {
			if first.Kind == controlBlock {
				for i, ctrl := range sequence {
					if ctrl == first {
						sequence = sequence[i+1:]
						break
					}
				}
			}
			if uint64(region) <= uint64(len(controls)) {
				for _, child := range controls[region-1].Children {
					if childOwner := owner[child]; childOwner != nil {
						seen := false
						for _, existing := range sequence {
							seen = seen || existing == childOwner
						}
						if !seen {
							sequence = append(sequence, childOwner)
						}
					}
				}
			}
			sort.SliceStable(sequence, func(i, j int) bool {
				return sequence[i].Header.Span.Start < sequence[j].Header.Span.Start
			})
		}
		return withSource(evalSequence(sequence), region)
	}
	// Fallthrough is the only exit whose source is set at a region boundary.
	var edges []deferExit
	for _, region := range controls {
		if region.Parent == 0 && owner[region.ID] != nil && owner[region.ID].Kind == controlFunction {
			edges = append(edges, evalRegion(region.ID)...)
		}
	}
	limit := normalizeConfig(config).MaxDeferEdges
	var total uint64
	for _, edge := range edges {
		if edge.kind != exitFallthrough && edge.kind != exitReturn && edge.kind != exitBreak && edge.kind != exitContinue {
			continue
		}
		for current := edge.source; current != 0; current = controls[current-1].Parent {
			if current == edge.target {
				break
			}
			defers := deferByRegion[current]
			for index := len(defers) - 1; index >= 0; index-- {
				total++
				if total > limit {
					reporter.add(diagnostic.Diagnostic{Severity: diagnostic.Error, Code: CodeGeneration, Message: fmt.Sprintf("defer edge limit of %d exceeded", limit)})
					failed = true
					goto budgetDone
				}
			}
		}
	}
budgetDone:
	reporter.flush()
	return !failed
}
