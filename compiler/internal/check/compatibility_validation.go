package check

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

const CodeConversion diagnostic.Code = "C0601"

func validateCompatibilityRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	for _, retained := range handoff.Records.Records() {
		compatibility := retained.Compatibility
		if compatibility == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		source, sourceOK := records.Root(compatibility.Source)
		destination, destinationOK := records.Root(compatibility.Destination)
		if !sourceOK || !destinationOK || source.State != infer.TypeFinal || destination.State != infer.TypeFinal {
			continue
		}
		if classify(handoff.Semantics, source.Type, destination.Type) != compatibleForbidden {
			continue
		}
		failed = true
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeConversion,
			Message:  fmt.Sprintf("cannot convert value for %s", expectedRoleText(compatibility.Role, compatibility.Ordinal)),
			Primary:  diagnostic.Label{Span: compatibility.Header.Span},
		})
	}
	return !failed
}
