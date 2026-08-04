package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeCall diagnostic.Code = "C0604"

const (
	CodeCaptureViolation diagnostic.Code = "C0617"
	CodeGenericAnonymous diagnostic.Code = "C0608"
)

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
			if !found {
				callee, calleeFound := records.Root(call.Callee)
				if !calleeFound || callee.State != infer.TypeFinal {
					continue
				}
				key, keyFound := typeSnapshot.Key(callee.Type)
				convention, parameters, functionResult, variadic, isFunction := key.Function()
				if !keyFound || !isFunction || result.Type != functionResult || (!variadic && len(call.Arguments) != len(parameters)) || (convention == types.C && variadic) {
					valid = false
				}
			} else if call.Receiver == 0 {
				valid = false
			} else {
				signature, sigFound := handoff.Semantics.Signature(selection.Method)
				if !sigFound || signature.State != infer.DeclarationReady || len(signature.Inputs) == 0 ||
					len(call.Arguments) != len(signature.Inputs)-1 {
					valid = false
				} else {
					selected, resolved := resolution.Symbols.Symbol(selection.Method)
					if !resolved || selected.Error || selected.Kind != symbol.SymbolMethod {
						valid = false
					}
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

func validateCallableRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
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
	resolution := handoff.Semantics.Resolution()
	if resolution == nil || resolution.Symbols == nil {
		return true
	}
	typeSnapshot := handoff.Semantics.Types()

	for _, retained := range handoff.Records.Records() {
		if !activeOperatorRecord(handoff, retained.Header) || retained.Callable == nil {
			continue
		}
		callable := retained.Callable
		invalidConvention := callable.Kind == callableExtern && callable.Convention != types.C

		if callable.BodyPresent && callable.Convention != types.Pebble {
			invalidConvention = true
		}
		if callable.Variadic && callable.Convention != types.C {
			invalidConvention = true
		}
		if invalidConvention {
			report(CodeCall, callable.Header, "callable declaration is invalid")
		}

		if callable.Kind == callableMethod && len(callable.Parameters) != 0 {
			self, ok := records.Root(callable.Parameters[0])
			if ok && self.State == infer.TypeFinal {
				selfKey, keyOK := typeSnapshot.Key(self.Type)
				method, methodOK := resolution.Symbols.Symbol(callable.Symbol)
				declaration, declarationOK := handoff.Semantics.TypeDeclaration(method.Containing)
				if keyOK && methodOK && declarationOK && declaration.Concrete != 0 {
					selfType := self.Type
					if selfKey.Kind() == types.Pointer {
						selfType, keyOK = selfKey.Child()
					}
					if keyOK && selfType != declaration.Concrete {
						report(CodeCall, callable.Header, "method self parameter is invalid")
					}
				}
			}
		}

		if callable.Kind == callableLiteral && len(callable.Captures) != 0 {
			report(CodeCaptureViolation, callable.Header, "anonymous function captures an enclosing binding")
		}
	}

	for _, retained := range handoff.Records.Records() {
		if retained.UnsupportedCallable == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		report(CodeGenericAnonymous, retained.UnsupportedCallable.Header, "generic anonymous functions are unsupported")
	}
	return !failed
}
