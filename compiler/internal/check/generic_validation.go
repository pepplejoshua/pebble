package check

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

const CodeGenericInstantiation diagnostic.Code = "C0621"

func requirementKindName(kind RequirementKind) string {
	switch kind {
	case RequirementOrdered:
		return "Ordered"
	case RequirementEquatable:
		return "Equatable"
	case RequirementNumeric:
		return "Numeric"
	case RequirementIntegral:
		return "Integral"
	case RequirementLiteralFits:
		return "LiteralFits"
	}
	return "unknown"
}

func instantiationSpan(handoff *solveHandoff, instantiation infer.Instantiation) source.Span {
	if handoff == nil {
		return source.Span{}
	}
	for _, record := range handoff.Records.Records() {
		if record.Call != nil && record.Call.Target.Site == instantiation.Site && record.Call.Target.Symbol == instantiation.Generic {
			return record.Header.Span
		}
	}
	return source.Span{}
}

func concreteSatisfiesRequirement(requirement Requirement, argument infer.TypeResult, semantics *infer.SemanticSnapshot) bool {
	if argument.State != infer.TypeFinal || semantics == nil || semantics.Types() == nil {
		return false
	}
	key, ok := semantics.Types().Key(argument.Type)
	if !ok {
		return false
	}
	switch requirement.Kind {
	case RequirementNumeric:
		builtin, ok := key.Builtin()
		return ok && (isIntegerBuiltin(builtin) || isFloatBuiltin(builtin))
	case RequirementIntegral:
		builtin, ok := key.Builtin()
		return ok && isIntegerBuiltin(builtin)
	case RequirementOrdered:
		return typeSatisfiesOrdered(key, argument.Type, semantics)
	case RequirementEquatable:
		return typeSatisfiesEquatable(key, argument.Type, semantics)
	case RequirementLiteralFits:
		// Literal range checking is intentionally deferred to a later slice.
		return true
	}
	return true
}

func validateGenericInstantiations(handoff *solveHandoff, records *solvedRecords, requirements map[symbol.SymbolID][]Requirement, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil || requirements == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	for _, instantiation := range handoff.Solution.Instantiations() {
		ownerRequirements := requirements[instantiation.Generic]
		bad := false
		var failedKind RequirementKind
		for _, requirement := range ownerRequirements {
			if requirement.Kind == RequirementLiteralFits {
				continue
			}
			ordinal := parameterOrdinal(handoff, instantiation.Generic, requirement.Parameter)
			if ordinal < 0 || ordinal >= len(instantiation.Arguments) {
				continue
			}
			if !concreteSatisfiesRequirement(requirement, instantiation.Arguments[ordinal], handoff.Semantics) {
				bad, failedKind = true, requirement.Kind
				break
			}
		}
		if bad {
			failed = true
			reporter.add(diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeGenericInstantiation,
				Message:  fmt.Sprintf("generic %s requirement failed at this call site", requirementKindName(failedKind)),
				Primary:  diagnostic.Label{Span: instantiationSpan(handoff, instantiation)},
			})
		}
	}
	return !failed
}
