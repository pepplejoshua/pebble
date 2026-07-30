package check

import (
	"fmt"

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
	if handoff == nil || handoff.GenerationHadErrors || handoff.Solution == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	retained := handoff.Records.Records()
	for _, record := range retained {
		if record.Defer == nil || !activeOperatorRecord(handoff, record.Header) {
			continue
		}
		for _, candidate := range retained {
			if !activeOperatorRecord(handoff, candidate.Header) || candidate.Header.Syntax != record.Defer.Statement {
				continue
			}
			forbidden := candidate.Control != nil && (candidate.Control.Kind == controlReturn || candidate.Control.Kind == controlBreak || candidate.Control.Kind == controlContinue)
			forbidden = forbidden || candidate.Defer != nil
			if forbidden {
				reporter.add(diagnostic.Diagnostic{
					Severity: diagnostic.Error,
					Code:     CodeInvalidDefer,
					Message:  "deferred return, break, continue, or defer is invalid",
					Primary:  diagnostic.Label{Span: record.Defer.Header.Span},
				})
				failed = true
				break
			}
		}
	}

	controls := handoff.Records.Controls()
	deferByRegion := make(map[controlID][]deferRecord)
	byRegion := make(map[controlID][]*controlRecord)
	owner := make(map[controlID]*controlRecord)
	bySyntax := make(map[symbol.SyntaxRef]*controlRecord)
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
			if ctrl.Kind == controlSwitch && !ctrl.ElsePresent && !switchIsExhaustive(handoff, records, ctrl, bySyntax) {
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
		if first := owner[region]; first != nil && first.Kind == controlFunction && uint64(region) <= uint64(len(controls)) {
			for _, child := range controls[region-1].Children {
				if childOwner := owner[child]; childOwner != nil {
					sequence = append(sequence, childOwner)
				}
			}
			return withSource(evalSequence(sequence), region)
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
