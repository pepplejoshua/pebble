package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func validateContextFlowRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	report := func(code diagnostic.Code, header recordHeader, message string) {
		failed = true
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     code,
			Message:  message,
			Primary:  diagnostic.Label{Span: header.Span},
		})
	}

	runtime, runtimeReady := handoff.Semantics.RuntimeTypes()
	for _, retained := range handoff.Records.Records() {
		flow := retained.ContextFlow
		if flow == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		switch flow.Kind {
		case contextForward, contextNone, contextIndirect:
			continue
		case contextExpression:
			signature, found := handoff.Semantics.Signature(flow.Caller.Symbol)
			if !found {
				continue
			}
			if signature.Convention != types.Pebble {
				report(CodeCall, flow.Header, "context expression is invalid in a C-convention body")
				continue
			}
			if !runtimeReady || runtime.Context == 0 || runtime.Context != flow.Context {
				report(CodeAggregate, flow.Header, "context expression has no matching runtime context type")
			}
		}
	}
	return !failed
}
