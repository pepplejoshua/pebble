package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeEntryPoint diagnostic.Code = "C0620"

func validateEntry(handoff *solveHandoff, records *solvedRecords, requirements map[symbol.SymbolID][]Requirement, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || records == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || config.Entry.Mode != EntryRequired {
		return true
	}

	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	var span source.Span
	report := func(message string) {
		failed = true
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeEntryPoint,
			Message:  message,
			Primary:  diagnostic.Label{Span: span},
		})
	}

	entryID := config.Entry.Symbol
	resolution := handoff.Semantics.Resolution()
	if resolution == nil || resolution.Symbols == nil {
		report("configured entry point cannot be resolved")
		return !failed
	}
	if handoff.Solution == nil {
		report("configured entry point has no solved callable signature")
		return !failed
	}
	entry, ok := resolution.Symbols.Symbol(entryID)
	if !ok {
		report("configured entry point cannot be resolved")
		return !failed
	}
	span = entry.Span

	var callable *callableRecord
	for _, retained := range handoff.Records.Records() {
		if retained.Callable == nil || retained.Callable.Symbol != entryID || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		callable = retained.Callable
		span = callable.Header.Span
		break
	}
	if callable == nil {
		report("configured entry point has no active callable record")
		return !failed
	}

	signature, signatureOK := handoff.Semantics.Signature(entryID)
	var entryType infer.TypeResult
	typeOK := false
	entryType, typeOK = handoff.Solution.SymbolType(entryID)
	if !typeOK || entryType.State != infer.TypeFinal || !signatureOK {
		report("configured entry point has no solved callable signature")
		return !failed
	}

	resultTemplate, resultOK := handoff.Semantics.Template(signature.Result)
	builtins := handoff.Semantics.Types().Builtins()
	validResult := resultOK && resultTemplate.Kind == infer.TemplateKnown && (resultTemplate.Known == builtins.Void || resultTemplate.Known == builtins.Int)
	valid := entry.Kind == symbol.SymbolFunction && !entry.Generic && entry.Module == handoff.Compilation.Root &&
		signature.Convention == types.Pebble && !signature.Variadic && len(signature.Parameters) == 0 && validResult &&
		len(callable.Captures) == 0 && len(requirements[entryID]) == 0
	if !valid {
		report("configured entry point does not satisfy entry-point requirements")
	}
	return !failed
}
