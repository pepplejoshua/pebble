package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
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
		if hasTypeParameter(handoff.Semantics, source.Type) || hasTypeParameter(handoff.Semantics, destination.Type) {
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

// hasTypeParameter reports whether typ involves a generic type parameter
// anywhere in its structure. During template checking of a generic function
// body, a type parameter's type is represented as a TypeParameter-kind key
// rather than a concrete builtin/composite type. We skip cast validation for
// such pairs because the actual legality depends on the concrete instantiation
// — deferring to instantiation time follows the same pattern used by
// deferredGenericRequirement for requirement checks on generic bodies.
func hasTypeParameter(snapshot *infer.SemanticSnapshot, typ types.TypeID) bool {
	if snapshot == nil || snapshot.Types() == nil {
		return false
	}
	key, ok := snapshot.Types().Key(typ)
	if !ok {
		return false
	}
	if key.Kind() == types.TypeParameter {
		return true
	}
	switch key.Kind() {
	case types.Pointer, types.Slice, types.Optional:
		child, _ := key.Child()
		return hasTypeParameter(snapshot, child)
	case types.Array:
		_, element, _ := key.Array()
		return hasTypeParameter(snapshot, element)
	case types.Tuple, types.Nominal:
		elements, _ := key.Elements()
		for _, e := range elements {
			if hasTypeParameter(snapshot, e) {
				return true
			}
		}
		return false
	case types.Function:
		_, params, result, _, _ := key.Function()
		for _, p := range params {
			if hasTypeParameter(snapshot, p) {
				return true
			}
		}
		return hasTypeParameter(snapshot, result)
	default:
		return false
	}
}
