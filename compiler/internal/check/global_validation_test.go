package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

func runGlobalValidation(t *testing.T, source string) (*diagnostic.DiagnosticSet, *solveHandoff, *solvedRecords) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
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

func TestValidateBindingsEmptyFormIgnoresTypeErrorSuppression(t *testing.T) {
	diagnostics, handoff, records := runGlobalValidation(t, "fn check() void { let x; }")
	var binding *bindingRecord
	for _, retained := range handoff.Records.Records() {
		if retained.Binding != nil && retained.Binding.Kind == bindingLocalLet {
			binding = retained.Binding
			break
		}
	}
	if binding == nil {
		t.Fatal("empty binding record was not retained")
	}
	result, ok := handoff.Solution.SymbolType(binding.Symbol)
	if !ok || result.State != infer.TypeError {
		t.Fatalf("empty binding symbol type = %+v, found=%v, want TypeError", result, ok)
	}
	if validateBindings(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeBindingInitializer) {
		t.Fatalf("empty binding did not report C0602: %+v", diagnostics.Items())
	}
}

func TestValidateBindingsPresenceAndGlobalConstants(t *testing.T) {
	tests := []struct {
		name        string
		source      string
		binding     diagnostic.Code
		nonconstant bool
	}{
		{name: "local initializer", source: "fn check() void { let x i32 = 1; }"},
		{name: "global initializer", source: "let x i32 = 1;"},
		{name: "mutable global initializer", source: "var x i32 = 1;"},
		{name: "nonconstant global initializer", source: "fn get() i32 => 1; let x i32 = get();", nonconstant: true},
		{name: "local annotation", source: "fn check() void { let x i32; }", binding: CodeBindingInitializer},
		{name: "global annotation", source: "let x i32;", binding: CodeBindingInitializer},
		{name: "empty local", source: "fn check() void { let x; }", binding: CodeBindingInitializer},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			diagnostics, handoff, records := runGlobalValidation(t, test.source)
			validateBindings(handoff, records, diagnostics, Config{})
			if hasValidationDiagnostic(diagnostics, test.binding) != (test.binding != "") {
				t.Fatalf("C0602 presence = %v, want %v: %+v", hasValidationDiagnostic(diagnostics, CodeBindingInitializer), test.binding != "", diagnostics.Items())
			}
			if hasValidationDiagnostic(diagnostics, CodeNonconstantGlobal) != test.nonconstant {
				t.Fatalf("C0616 presence = %v, want %v: %+v", hasValidationDiagnostic(diagnostics, CodeNonconstantGlobal), test.nonconstant, diagnostics.Items())
			}
		})
	}
}

func TestValidateSizeofConcreteType(t *testing.T) {
	diagnostics, handoff, records := runGlobalValidation(t, "fn check() void { let x u64 = sizeof i32; }")
	if !validateSizeof(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("concrete sizeof was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateSizeofVoid(t *testing.T) {
	diagnostics, handoff, records := runGlobalValidation(t, "fn check() void { let x u64 = sizeof void; }")
	if validateSizeof(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("void sizeof was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateSizeofFunctionAndGenericTypes(t *testing.T) {
	diagnostics, handoff, records := runGlobalValidation(t, "fn check() void { let x u64 = sizeof fn() void; }")
	if validateSizeof(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("invalid sizeof function type was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateSizeofGenericTypeParameterIsDeferred(t *testing.T) {
	diagnostics, handoff, records := runGlobalValidation(t, "fn check[T]() void { let x u64 = sizeof T; }")
	if !validateSizeof(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("generic sizeof type parameter was rejected before instantiation: %+v", diagnostics.Items())
	}
}

func TestValidateSizeofExternType(t *testing.T) {
	diagnostics, handoff, records := runGlobalValidation(t, "extern type Opaque; fn check() void { let x u64 = sizeof Opaque; }")
	if validateSizeof(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("extern sizeof type was not rejected: %+v", diagnostics.Items())
	}
}

func TestGlobalValidationSkipsInactiveRecords(t *testing.T) {
	diagnostics, handoff, records := runGlobalValidation(t, "let x i32; fn check() void { let y u64 = sizeof void; }")
	for index := range handoff.Records.values {
		retained := &handoff.Records.values[index]
		if retained.Binding == nil && retained.TypeUse == nil {
			continue
		}
		retained.Header.Alternative = alternativeTag{Guarded: true, Choice: 999999, Index: 1}
	}
	if !validateBindings(handoff, records, diagnostics, Config{}) || !validateSizeof(handoff, records, diagnostics, Config{}) ||
		hasValidationDiagnostic(diagnostics, CodeBindingInitializer) || hasValidationDiagnostic(diagnostics, CodeAggregate) {
		t.Fatalf("inactive records produced diagnostics: %+v", diagnostics.Items())
	}
}
