package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type assignmentRecordKey struct {
	syntaxRef symbol.SyntaxRef
	owner     symbol.SymbolID
}

func validateAssignmentRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)

	places := make(map[assignmentRecordKey]*placeRecord)
	mutations := make(map[assignmentRecordKey]valueID)
	postfixStatements := make(map[valueID]bool)
	forUpdates := make(map[symbol.SyntaxRef]bool)
	for _, retained := range handoff.Records.Records() {
		if !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		key := assignmentRecordKey{syntaxRef: retained.Header.Syntax, owner: retained.Header.Owner}
		if retained.Place != nil {
			places[key] = retained.Place
		}
		if retained.Operator != nil && retained.Operator.Family == operatorMutation {
			mutations[key] = retained.Operator.Result
		}
		if retained.Control == nil {
			continue
		}
		if retained.Control.Kind == controlExpression && retained.Control.StatementForm == statementPostfixUpdate {
			for _, value := range retained.Control.Values {
				if value.Role == valueDiscarded {
					postfixStatements[value.Value] = true
				}
			}
		}
		if retained.Control.Kind == controlFor {
			for _, child := range retained.Control.Composition {
				if child.Role == roleUpdate {
					forUpdates[child.Arm] = true
				}
			}
		}
	}

	failed := false
	// No activeOperatorRecord filter here: the original scan this replaces
	// matched by syntax ref and operator family alone (see
	// place_validation.go's identical precompute for the same note).
	addressOperators := make(addressOperatorsBySyntax)
	for _, retained := range handoff.Records.Records() {
		if retained.Operator != nil && retained.Operator.Family == operatorAddress {
			addressOperators[retained.Header.Syntax] = true
		}
	}
	for _, retained := range handoff.Records.Records() {
		assignment := retained.Assignment
		if assignment == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		place := places[assignmentRecordKey{syntaxRef: retained.Header.Syntax, owner: retained.Header.Owner}]
		if place == nil {
			continue
		}
		writability := placeWritability(handoff, records, place, addressOperators)
		if writability == placeWritabilityUnresolved {
			continue
		}
		bad := writability == placeNotWritable || writability == placeStringIndex
		if assignment.Kind == assignmentCompound && (assignment.Operator == syntax.PlusPlus || assignment.Operator == syntax.MinusMinus) {
			result := mutations[assignmentRecordKey{syntaxRef: retained.Header.Syntax, owner: retained.Header.Owner}]
			bad = bad || (!postfixStatements[result] && !forUpdates[assignment.Header.Syntax])
		}
		if bad {
			failed = true
			reporter.add(diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodePlace,
				Message:  "assignment place or mutation is not used legally",
				Primary:  diagnostic.Label{Span: assignment.Header.Span},
			})
		}
	}
	return !failed
}
