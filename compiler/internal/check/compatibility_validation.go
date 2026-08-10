package check

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeConversion diagnostic.Code = "C0601"

// isPointerToPointerCompatibility reports whether source and destination are
// both distinct pointer types — the one compatibleExplicit shape this
// validator actually rejects when reached implicitly (see the call site).
func isPointerToPointerCompatibility(handoff *solveHandoff, sourceID, destinationID types.TypeID) bool {
	if handoff == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil {
		return false
	}
	source, ok := handoff.Semantics.Types().Key(sourceID)
	if !ok {
		return false
	}
	destination, ok := handoff.Semantics.Types().Key(destinationID)
	if !ok {
		return false
	}
	return source.Kind() == types.Pointer && destination.Kind() == types.Pointer
}

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
		class := classify(handoff.Semantics, source.Type, destination.Type)
		if class == compatibleForbidden {
			// An array literal directly initializing a slice-typed binding
			// (`var s []int = [1, 2, 3];`) is valid — equivalent to constructing
			// the array then taking a full slice of it — even though classify
			// still reports array→slice as compatibleForbidden for every other
			// position (call arguments, returns, casts, plain reassignment),
			// which keep their existing C0601.
			if implicitArrayToSlice(handoff, compatibility, source.Type, destination.Type) {
				continue
			}
			failed = true
			reporter.add(diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeConversion,
				Message:  fmt.Sprintf("cannot convert value for %s", expectedRoleText(compatibility.Role, compatibility.Ordinal)),
				Primary:  diagnostic.Label{Span: compatibility.Header.Span},
			})
		} else if class == compatibleExplicit && isPointerToPointerCompatibility(handoff, source.Type, destination.Type) {
			// Every other compatibleExplicit pair (integer widening/
			// narrowing, tuple coercion, enum<->integer, ...) is
			// deliberately left exactly as permissive as before this
			// check existed — narrowing those is a separate, unrelated
			// language-design question this task does not touch. Only
			// pointer-to-pointer is newly rejected here, per
			// spec/compiler/proposals/11-raw-pointers-and-unsafe-ops.md
			// §3: every pointer-to-pointer pair is explicit-only.
			failed = true
			reporter.add(diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeConversion,
				Message:  fmt.Sprintf("cannot implicitly convert value for %s; use an explicit cast", expectedRoleText(compatibility.Role, compatibility.Ordinal)),
				Primary:  diagnostic.Label{Span: compatibility.Header.Span},
			})
		}
	}
	return !failed
}
