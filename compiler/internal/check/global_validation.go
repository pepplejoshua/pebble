package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const (
	CodeBindingInitializer diagnostic.Code = "C0602"
	CodeNonconstantGlobal  diagnostic.Code = "C0616"
)

func validateBindings(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	report := func(code diagnostic.Code, header recordHeader) {
		failed = true
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     code,
			Message:  "binding initializer is invalid",
			Primary:  diagnostic.Label{Span: header.Span},
		})
	}

	for _, retained := range handoff.Records.Records() {
		binding := retained.Binding
		if binding == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		switch binding.Kind {
		case bindingLocalLet, bindingLocalVar, bindingGlobalLet, bindingGlobalVar:
		default:
			continue
		}
		if !binding.InitializerPresent {
			report(CodeBindingInitializer, binding.Header)
			continue
		}
		if binding.Global {
			constant, ok := records.Constant(binding.InitializerSyntax)
			if !ok || constant.State != constantKnown {
				report(CodeNonconstantGlobal, binding.Header)
			}
		}
	}
	return !failed
}

func validateSizeof(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	for _, retained := range handoff.Records.Records() {
		typeUse := retained.TypeUse
		if typeUse == nil || typeUse.Kind != typeUseSizeof || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		resolved, ok := records.Root(typeUse.Type)
		if !ok || resolved.State != infer.TypeFinal {
			continue
		}
		key, ok := handoff.Semantics.Types().Key(resolved.Type)
		if !ok {
			continue
		}
		invalid := key.Kind() == types.Function
		if key.Kind() == types.Builtin {
			builtin, builtinOK := key.Builtin()
			invalid = builtinOK && builtin == types.Void
		}
		if key.Kind() == types.Nominal {
			declaration, _, declarationOK := key.Nominal()
			if declarationOK {
				descriptor, descriptorOK := handoff.Semantics.TypeDeclaration(declaration)
				invalid = descriptorOK && descriptor.Nominal == infer.NominalExtern
			}
		}
		if invalid {
			failed = true
			reporter.add(diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeAggregate,
				Message:  "sizeof type is invalid",
				Primary:  diagnostic.Label{Span: typeUse.Header.Span},
			})
		}
	}
	return !failed
}

func sizeofTypeInvalid(handoff *solveHandoff, store *types.Store, typ types.TypeID) bool {
	key, ok := store.Key(typ)
	if !ok {
		return false
	}
	invalid := key.Kind() == types.Function
	if key.Kind() == types.Builtin {
		builtin, builtinOK := key.Builtin()
		invalid = builtinOK && builtin == types.Void
	}
	if key.Kind() == types.Nominal {
		declaration, _, declarationOK := key.Nominal()
		if declarationOK {
			descriptor, descriptorOK := handoff.Semantics.TypeDeclaration(declaration)
			invalid = descriptorOK && descriptor.Nominal == infer.NominalExtern
		}
	}
	return invalid
}

// validateSpecializedSizeof checks symbolic sizeof records after replacing
// the enclosing generic's type parameters with concrete instantiation args.
func validateSpecializedSizeof(handoff *solveHandoff, records *solvedRecords, store *types.Store, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil || store == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	for _, instantiation := range handoff.Solution.Instantiations() {
		signature, ok := handoff.Semantics.Signature(instantiation.Generic)
		if !ok || len(signature.TypeParams) != len(instantiation.Arguments) {
			continue
		}
		substitution := make(map[symbol.SymbolID]types.TypeID, len(signature.TypeParams))
		for index, parameter := range signature.TypeParams {
			argument := instantiation.Arguments[index]
			if argument.State != infer.TypeFinal {
				substitution = nil
				break
			}
			substitution[parameter] = argument.Type
		}
		if substitution == nil {
			continue
		}
		for _, retained := range handoff.Records.Records() {
			typeUse := retained.TypeUse
			if typeUse == nil || typeUse.Kind != typeUseSizeof || retained.Header.Owner != instantiation.Generic || !activeOperatorRecord(handoff, retained.Header) {
				continue
			}
			resolved, ok := records.Root(typeUse.Type)
			if !ok || resolved.State != infer.TypeFinal {
				continue
			}
			specialized, err := store.Substitute(resolved.Type, substitution)
			if err != nil || !sizeofTypeInvalid(handoff, store, specialized) {
				continue
			}
			failed = true
			reporter.add(diagnostic.Diagnostic{Severity: diagnostic.Error, Code: CodeAggregate, Message: "sizeof type is invalid", Primary: diagnostic.Label{Span: typeUse.Header.Span}})
		}
	}
	return !failed
}
