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
	if handoff == nil || handoff.GenerationHadErrors || !handoff.Solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("tuple access failed: %+v", diagnostics.Items())
	}
}

func TestStructuralFieldAccessClean(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn slice_len(s []i32) uint => s.len;
fn generic_len[T](s []T) uint => s.len;
fn slice_data(s []i32) *i32 => s.data;
fn array_len(a [5]i32) uint => a.len;
fn string_len(s str) uint => s.len;
fn loop_len(items []i32) void { loop 0..items.len : i {} }
type Own = struct { len uint; data *i32; };
fn own(o Own) uint => o.len;
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors || !handoff.Solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("structural fields rejected: %+v", diagnostics.Items())
	}
}

func TestStructuralFieldUnknownRejected(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(s []i32) uint => s.missing;
`)})
	run06a(inputs, diagnostics, Config{})
	if !diagnostics.HasErrors() {
		t.Fatal("unknown structural field was accepted")
	}
}

func TestOptionalHasValueAndNominalCollisionAccessClean(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Own = struct { has_value i32; };
fn optional(o ?i32) bool => o.has_value;
fn nominal(o Own) i32 => o.has_value;
type Box = struct { value ?i32; fn check(self *Box) bool => self.value.has_value; };
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors || !handoff.Solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("optional or nominal structural field rejected: %+v", diagnostics.Items())
	}
}

func TestOptionalUnknownFieldRejected(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn f(o ?i32) i32 => o.foo;
`)})
	run06a(inputs, diagnostics, Config{})
	if !diagnostics.HasErrors() {
		t.Fatal("unknown optional field was accepted")
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
