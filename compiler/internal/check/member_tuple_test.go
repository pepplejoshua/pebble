package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

func TestTupleComponentAccessClean(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f() void {
    let pair (i32, str) = (1, "a");
    let first i32 = pair.0;
    let second str = pair.1;
}
`)})

	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatal("handoff is nil")
	}
	if handoff.GenerationHadErrors {
		t.Fatalf("generation had errors: %+v", diagnostics.Items())
	}
	if !handoff.Solution.Successful() {
		t.Fatalf("solution is not successful: %+v", diagnostics.Items())
	}
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics: %+v", diagnostics.Items())
	}
}

func TestTupleComponentAccessOutOfRange(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f() void {
    let pair (i32, str) = (1, "a");
    let third i32 = pair.2;
}
`)})

	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatal("handoff is nil")
	}
	if !diagnostics.HasErrors() {
		t.Fatal("expected diagnostic for out-of-range ordinal")
	}
	items := diagnostics.Items()
	foundCapability := false
	for _, item := range items {
		if item.Code == infer.CodeCapability {
			foundCapability = true
			break
		}
	}
	if !foundCapability {
		t.Fatalf("expected CodeCapability in diagnostics: %+v", items)
	}
}

func TestTupleComponentRigidRetainsUnsupportedRequirement(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn first[T](value T) T => value.0;
`)})

	facts := run06a3(inputs, diagnostics, Config{})
	var member *memberRecord
	var requirement *requirementRecord
	var requirementRetained retainedRecord
	for i := range facts.Generation.records.values {
		retained := &facts.Generation.records.values[i]
		if retained.Member != nil && retained.Member.Kind == memberTuple {
			if member != nil {
				t.Fatal("multiple tuple-member records")
			}
			member = retained.Member
		}
		if retained.Requirement != nil && retained.Requirement.Kind == requirementUnsupportedComponent {
			if requirement != nil {
				t.Fatal("multiple unsupported-component requirements")
			}
			requirement = retained.Requirement
			requirementRetained = *retained
		}
	}
	if member == nil || requirement == nil {
		t.Fatalf("member=%+v requirement=%+v diagnostics=%+v", member, requirement, diagnostics.Items())
	}
	requirementHeader, memberHeader := requirement.Header, member.Header
	requirementHeader.ID, memberHeader.ID = 0, 0
	if requirementHeader != memberHeader || requirement.Subject != member.Base || requirement.Operator != 0 || requirement.Header.Owner == 0 {
		t.Fatalf("member=%+v requirement=%+v", member, requirement)
	}
	if got := facts.Generation.counters.genericRequirements; got != 1 {
		t.Fatalf("generic requirements=%d, want 1", got)
	}
	if diagnostics.HasErrors() {
		t.Fatalf("generation diagnostics=%+v", diagnostics.Items())
	}
	invalid := cloneRetainedRecord(requirementRetained)
	header := invalid.Header
	header.ID = 0
	invalid.assignHeader(header)
	invalid.Requirement.Kind = requirementUnsupportedComponent + 1
	before, components := len(facts.Generation.records.values), facts.Generation.records.components
	if id, ok := facts.Generation.addRecord(invalid); ok || id != 0 || len(facts.Generation.records.values) != before || facts.Generation.records.components != components {
		t.Fatal("out-of-range requirement kind mutated the record arena")
	}
}
