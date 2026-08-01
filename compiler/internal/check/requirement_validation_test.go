package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

func TestRun06bPublishesNumericRequirement(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn add[T](a T, b T) T { return a + b; }`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatal("missing handoff")
	}
	result := run06b(handoff, diagnostics, Config{}, inputs.Types)
	if !result.Successful() {
		t.Fatalf("generic numeric function rejected: %+v", diagnostics.Items())
	}
	owner := handoff.Compilation.Modules[0].Declarations[0]
	requirements := result.Requirements(owner)
	if len(requirements) != 1 || requirements[0].Kind != RequirementNumeric || requirements[0].Parameter == 0 || requirements[0].Operator == 0 {
		t.Fatalf("unexpected requirements: %+v", requirements)
	}
}

func TestRun06bDeduplicatesRepeatedGenericUses(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn addTwice[T](a T) T { let first T = a + a; let second T = a + a; return first + second; }`)})
	handoff := run06a(inputs, diagnostics, Config{})
	result := run06b(handoff, diagnostics, Config{}, inputs.Types)
	if !result.Successful() {
		t.Fatalf("repeated generic use rejected: %+v", diagnostics.Items())
	}
	owner := handoff.Compilation.Modules[0].Declarations[0]
	count := 0
	for _, requirement := range result.Requirements(owner) {
		if requirement.Kind == RequirementNumeric {
			count++
		}
	}
	if count != 1 {
		t.Fatalf("expected one normalized numeric requirement, got %+v", result.Requirements(owner))
	}
}

func TestRun06bPublishesEquatableRequirement(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn same[T](a T, b T) bool { return a == b; }`)})
	handoff := run06a(inputs, diagnostics, Config{})
	result := run06b(handoff, diagnostics, Config{}, inputs.Types)
	if !result.Successful() {
		t.Fatalf("generic equality function rejected: %+v", diagnostics.Items())
	}
	owner := handoff.Compilation.Modules[0].Declarations[0]
	requirements := result.Requirements(owner)
	if len(requirements) != 1 || requirements[0].Kind != RequirementEquatable || requirements[0].Parameter == 0 || requirements[0].Operator == 0 {
		t.Fatalf("unexpected requirements: %+v", requirements)
	}
}

func TestRun06bRejectsUnsupportedGenericRequirement(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn field[T](value T) i32 { return value.field; }`)})
	handoff := run06a(inputs, diagnostics, Config{})
	result := run06b(handoff, diagnostics, Config{}, inputs.Types)
	if result.Successful() || !hasValidationDiagnostic(diagnostics, CodeUnsupportedGeneric) {
		t.Fatalf("expected C0610, result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
}

func TestRun06bPublishesLiteralFitRequirement(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn literal[T](value T) T { return value + 1; }`)})
	handoff := run06a(inputs, diagnostics, Config{})
	result := run06b(handoff, diagnostics, Config{}, inputs.Types)
	if !result.Successful() {
		t.Fatalf("generic literal function rejected: %+v", diagnostics.Items())
	}
	owner := handoff.Compilation.Modules[0].Declarations[0]
	for _, requirement := range result.Requirements(owner) {
		if requirement.Kind == RequirementLiteralFits {
			if requirement.Parameter == 0 || requirement.LiteralKind == 0 || requirement.Numerator != "1" {
				t.Fatalf("unexpected literal requirement: %+v", requirement)
			}
			return
		}
	}
	t.Fatalf("no literal-fit requirement: %+v", result.Requirements(owner))
}

func TestValidateRequirementsRejectsMissingJoin(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn add[T](a T, b T) T { return a + b; }`)})
	handoff := run06a(inputs, diagnostics, Config{})
	records, ok := resolveRecords(handoff, diagnostics, Config{})
	if !ok {
		t.Fatal("failed to resolve test records")
	}
	values := handoff.Records.Records()
	for index := range values {
		if values[index].Requirement != nil {
			values[index].Requirement.Kind = requirementOrdered
			break
		}
	}
	broken := *handoff
	broken.Records = frozenRecords{values: values}
	set := diagnostic.NewDiagnosticSet()
	_, valid := validateRequirements(&broken, records, set, Config{})
	if valid || !hasValidationDiagnostic(set, CodeGeneration) {
		t.Fatalf("expected missing join C0619, valid=%v diagnostics=%+v", valid, set.Items())
	}
}

func TestValidateRequirementsSkipsInactiveRecord(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn add[T](a T, b T) T { return a + b; }`)})
	handoff := run06a(inputs, diagnostics, Config{})
	records, ok := resolveRecords(handoff, diagnostics, Config{})
	if !ok {
		t.Fatal("failed to resolve test records")
	}
	values := handoff.Records.Records()
	for index := range values {
		if values[index].Requirement != nil {
			values[index].Header.Alternative = alternativeTag{Guarded: true, Choice: infer.ConstraintID(^uint32(0)), Index: 0}
			values[index].Requirement.Header = values[index].Header
		}
	}
	broken := *handoff
	broken.Records = frozenRecords{values: values}
	set := diagnostic.NewDiagnosticSet()
	result, valid := validateRequirements(&broken, records, set, Config{})
	if !valid || set.Len() != 0 || len(result) != 0 {
		t.Fatalf("inactive requirement was not skipped: valid=%v result=%+v diagnostics=%+v", valid, result, set.Items())
	}
}
