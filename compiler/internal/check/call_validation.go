package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeCall diagnostic.Code = "C0604"

func validateCallRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	report := func(header recordHeader) {
		failed = true
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeCall,
			Message:  "call site is invalid",
			Primary:  diagnostic.Label{Span: header.Span},
		})
	}
	resolution := handoff.Semantics.Resolution()
	if resolution == nil || resolution.Symbols == nil {
		return true
	}
	typeSnapshot := handoff.Semantics.Types()

	for _, retained := range handoff.Records.Records() {
		call := retained.Call
		if call == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		result, ok := records.Root(call.Result)
		if !ok || result.State != infer.TypeFinal {
			continue
		}

		valid := true
		switch call.Target.Kind {
		case callDirect:
			signature, found := handoff.Semantics.Signature(call.Target.Symbol)
			if !found || signature.State != infer.DeclarationReady || signature.Symbol != call.Target.Symbol {
				valid = false
			} else if call.Target.Convention != signature.Convention ||
				call.Target.Variadic != signature.Variadic ||
				(!signature.Variadic && call.Target.FixedCount != uint32(len(signature.Inputs))) ||
				(!signature.Variadic && len(call.Arguments) != len(signature.Inputs)) ||
				(signature.Convention == types.C && signature.Variadic) {
				valid = false
			}

		case callIndirect:
			callee, found := records.Root(call.Callee)
			if !found || callee.State != infer.TypeFinal {
				continue
			}
			key, found := typeSnapshot.Key(callee.Type)
			if !found {
				valid = false
				break
			}
			convention, parameters, functionResult, variadic, isFunction := key.Function()
			if !isFunction {
				valid = false
			} else if result.Type != functionResult ||
				(!variadic && len(call.Arguments) != len(parameters)) ||
				(convention == types.C && variadic) {
				valid = false
			}

		case callMethod:
			selection, found := handoff.Solution.Method(call.Target.Site)
			if !found || call.Receiver == 0 || len(call.Arguments) != len(selection.Arguments) {
				valid = false
			} else {
				selected, resolved := resolution.Symbols.Symbol(selection.Method)
				if !resolved || selected.Error || selected.Kind != symbol.SymbolMethod {
					valid = false
				}
			}

		case callVariant:
			selected, found := resolution.Symbols.Symbol(call.Target.Symbol)
			if !found || selected.Error || selected.Kind != symbol.SymbolVariant {
				valid = false
				break
			}
			declaration, found := handoff.Semantics.TypeDeclaration(selected.Containing)
			if !found {
				valid = false
				break
			}
			var payload infer.TemplateID
			memberFound := false
			for _, member := range declaration.Members {
				if member.Symbol == call.Target.Symbol {
					payload = member.Type
					memberFound = true
					break
				}
			}
			template, found := handoff.Semantics.Template(payload)
			if !memberFound || !found {
				valid = false
				break
			}
			expected := 1
			if template.Kind == infer.TemplateKnown && template.Known == typeSnapshot.Builtins().Void {
				expected = 0
			}
			if len(call.Arguments) != expected {
				valid = false
			}

		default:
			valid = false
		}
		if !valid {
			report(call.Header)
		}
	}
	return !failed
}
