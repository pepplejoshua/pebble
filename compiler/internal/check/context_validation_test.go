package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const contextValidationSource = `
fn current_context() void {
    context;
}
extern fn foreign() void;
`

func runContextValidation(t *testing.T) (*diagnostic.DiagnosticSet, *solveHandoff, *solvedRecords) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(contextValidationSource)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, handoff, records
}

func TestValidateContextFlowRecordsAcceptsContextExpression(t *testing.T) {
	diagnostics, handoff, records := runContextValidation(t)
	if !validateContextFlowRecords(handoff, records, diagnostics, Config{}) {
		t.Fatalf("valid context expression was rejected: %+v", diagnostics.Items())
	}
	if hasValidationDiagnostic(diagnostics, CodeCall) || hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("valid context expression produced a validation diagnostic: %+v", diagnostics.Items())
	}
}

func TestValidateContextFlowRecordsSkipsCallContextKinds(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(callValidationSource + "\nextern fn foreign() void;\nfn caller() void { foreign(); }\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	counts := map[contextFlowKind]int{}
	for _, retained := range handoff.Records.Records() {
		if retained.ContextFlow != nil {
			counts[retained.ContextFlow.Kind]++
		}
	}
	if counts[contextForward] == 0 || counts[contextNone] == 0 || counts[contextIndirect] == 0 {
		t.Fatalf("call fixture context kinds = %+v", counts)
	}
	if !validateContextFlowRecords(handoff, records, diagnostics, Config{}) {
		t.Fatalf("call context flow was rejected: %+v", diagnostics.Items())
	}
	if hasValidationDiagnostic(diagnostics, CodeCall) || hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("call context flow produced a validation diagnostic: %+v", diagnostics.Items())
	}
}

func TestValidateContextFlowRecordsRejectsCBody(t *testing.T) {
	diagnostics, handoff, records := runContextValidation(t)
	var cSymbol symbol.SymbolID
	for _, signature := range handoff.Semantics.Signatures() {
		if signature.Convention == types.C {
			cSymbol = signature.Symbol
			break
		}
	}
	if cSymbol == 0 {
		t.Fatal("fixture did not produce a C-convention signature")
	}
	for index := range handoff.Records.values {
		flow := handoff.Records.values[index].ContextFlow
		if flow != nil && flow.Kind == contextExpression {
			flow.Caller.Symbol = cSymbol
			break
		}
	}
	if validateContextFlowRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeCall) {
		t.Fatalf("C-body context expression was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateContextFlowRecordsSkipsInactiveRecord(t *testing.T) {
	diagnostics, handoff, records := runContextValidation(t)
	for index := range handoff.Records.values {
		flow := handoff.Records.values[index].ContextFlow
		if flow != nil && flow.Kind == contextExpression {
			handoff.Records.values[index].Header.Alternative = alternativeTag{Choice: 999999, Index: 1, Guarded: true}
			break
		}
	}
	if !validateContextFlowRecords(handoff, records, diagnostics, Config{}) {
		t.Fatalf("inactive context expression was validated: %+v", diagnostics.Items())
	}
	if hasValidationDiagnostic(diagnostics, CodeCall) || hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("inactive context expression produced a diagnostic: %+v", diagnostics.Items())
	}
}
