package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

// validateCastRecords rejects retained explicit cast records whose
// source/destination pair is compatibleForbidden. Such a cast is not merely
// unimplemented: every pair classify marks compatibleForbidden is deliberately
// disallowed by the primitive/composite conversion matrix. Reporting it here —
// before IR construction — gives the user a clean C0601 instead of the generic
// C0619 buildBlocks catch-all that buildValueBase otherwise turns into.
func validateCastRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	for _, retained := range handoff.Records.Records() {
		cast := retained.Cast
		if cast == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		source, sourceOK := records.Root(cast.Source)
		destination, destinationOK := records.Root(cast.Destination)
		if !sourceOK || !destinationOK || source.State != infer.TypeFinal || destination.State != infer.TypeFinal {
			continue
		}
		if classify(handoff.Semantics, source.Type, destination.Type) == compatibleForbidden {
			failed = true
			reporter.add(diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeConversion,
				Message:  "cannot cast value: no valid conversion exists between these types",
				Primary:  diagnostic.Label{Span: cast.Header.Span},
			})
		}
	}
	return !failed
}
