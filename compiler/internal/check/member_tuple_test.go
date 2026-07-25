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
