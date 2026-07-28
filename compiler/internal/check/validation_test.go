package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

// TestAuditHandoffValid verifies a well-formed handoff passes the audit.
func TestAuditHandoffValid(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct { x i32; y i32; };

fn distance(p Point) i32 {
    return p.x + p.y;
}

fn main() void {
    let origin = Point.{ x = 0, y = 0 };
    let d = distance(origin);
    print d;
}
`)})

	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatal("handoff is nil")
	}
	if handoff.Semantics == nil {
		t.Fatal("handoff.Semantics is nil")
	}
	if handoff.Solution == nil {
		t.Fatal("handoff.Solution is nil")
	}

	result := auditHandoff(handoff, diagnostics, normalizeConfig(Config{}))
	if !result {
		t.Fatal("auditHandoff rejected a valid handoff")
	}
}

// TestAuditHandoffGenerationHadErrorsNilSemantics verifies the one exception:
// GenerationHadErrors == true with Semantics == nil returns false and emits
// no new diagnostic.
func TestAuditHandoffGenerationHadErrorsNilSemantics(t *testing.T) {
	diagnostics := diagnostic.NewDiagnosticSet()
	handoff := &solveHandoff{GenerationHadErrors: true}

	before := diagnostics.Len()
	result := auditHandoff(handoff, diagnostics, normalizeConfig(Config{}))
	after := diagnostics.Len()

	if result {
		t.Fatal("auditHandoff should have returned false")
	}
	if after != before {
		t.Fatalf("expected no new diagnostic, got %d", after-before)
	}
}

// TestAuditHandoffMismatch verifies that a mismatched Semantics and Solution
// (from independently constructed handoffs) fails the audit with C0619.
func TestAuditHandoffMismatch(t *testing.T) {
	inputsA, _ := factInputs(t, checkProvider{"main.peb": []byte(`
fn a() void { print 1; }
`)})
	inputsB, _ := factInputs(t, checkProvider{"main.peb": []byte(`
fn b() void { print 2; }
`)})

	diagsA := diagnostic.NewDiagnosticSet()
	diagsB := diagnostic.NewDiagnosticSet()
	handoffA := run06a(inputsA, diagsA, Config{})
	handoffB := run06a(inputsB, diagsB, Config{})

	diagnostics := diagnostic.NewDiagnosticSet()
	handoff := &solveHandoff{
		Semantics: handoffA.Semantics,
		Solution:  handoffB.Solution,
	}

	result := auditHandoff(handoff, diagnostics, normalizeConfig(Config{}))
	if result {
		t.Fatal("auditHandoff should have rejected mismatched handoff")
	}

	foundCodeGeneration := false
	for _, d := range diagnostics.Items() {
		if d.Code == CodeGeneration {
			foundCodeGeneration = true
			break
		}
	}
	if !foundCodeGeneration {
		t.Fatal("expected a C0619 diagnostic for mismatched semantics/solution")
	}
}

// TestAuditHandoffTypeFinalNotContained mixes semantics from one program
// with a solution from another and confirms auditHandoff rejects it with
// C0619. Note: this construction is rejected by the earlier
// Semantics.Matches(Solution) check (mismatched program/solve identity),
// not by the later TypeFinal-containment check specifically - Matches
// already guarantees Types().Len() == Solution.storeLength, so no handoff
// reachable through this package's normal construction can pass Matches
// while still having an out-of-range TypeFinal result. The containment
// check's own distinct failure mode (a TypeFinal result carrying a
// zero/invalid TypeID, which Matches's aggregate length check would not
// catch) isn't independently constructible here, since Solution's
// internals are private to the infer package. This test still verifies a
// real, meaningful rejection path; it just isn't proof of the containment
// check's own branch specifically.
func TestAuditHandoffTypeFinalNotContained(t *testing.T) {
	// A program with few types (just builtins plus a simple struct).
	inputsSimple, _ := factInputs(t, checkProvider{"main.peb": []byte(`fn main() void { print 1; }`)})

	// A program with more types, producing larger TypeIDs.
	inputsComplex, _ := factInputs(t, checkProvider{"main.peb": []byte(`
type A = struct { x i32; y i32; };
type B = struct { a A; b A; };
type C = struct { x i32; y i32; z i32; };

fn main() void {
    let v = C.{ x = 1, y = 2, z = 3 };
    print v.x;
}
`)})

	diagsSimple := diagnostic.NewDiagnosticSet()
	diagsComplex := diagnostic.NewDiagnosticSet()
	handoffSimple := run06a(inputsSimple, diagsSimple, Config{})
	handoffComplex := run06a(inputsComplex, diagsComplex, Config{})

	// Confirm the complex solution has TypeFinal results.
	hasTypeFinal := false
	for _, st := range handoffComplex.Solution.SymbolTypes() {
		if st.Result.State == infer.TypeFinal {
			hasTypeFinal = true
			break
		}
	}
	if !hasTypeFinal {
		t.Fatal("complex solution has no TypeFinal results")
	}

	// Confirm at least one TypeFinal in the complex solution is not contained
	// in the simple snapshot's type set (different store lengths).
	snap := handoffSimple.Semantics.Types()
	hasUncontained := false
	for _, st := range handoffComplex.Solution.SymbolTypes() {
		if st.Result.State == infer.TypeFinal && !snap.Contains(st.Result.Type) {
			hasUncontained = true
			break
		}
	}
	if !hasUncontained {
		// Also check SyntaxTypes and Slots.
		for _, st := range handoffComplex.Solution.SyntaxTypes() {
			if st.Result.State == infer.TypeFinal && !snap.Contains(st.Result.Type) {
				hasUncontained = true
				break
			}
		}
	}
	if !hasUncontained {
		for _, st := range handoffComplex.Solution.Slots() {
			if st.Result.State == infer.TypeFinal && !snap.Contains(st.Result.Type) {
				hasUncontained = true
				break
			}
		}
	}
	if !hasUncontained {
		t.Fatal("test setup: no uncontained TypeFinal found between complex solution and simple type snapshot")
	}

	// Mix semantics from the simple program with solution from the complex one.
	diagnostics := diagnostic.NewDiagnosticSet()
	handoff := &solveHandoff{
		Semantics: handoffSimple.Semantics,
		Solution:  handoffComplex.Solution,
	}

	result := auditHandoff(handoff, diagnostics, normalizeConfig(Config{}))
	if result {
		t.Fatal("auditHandoff should have rejected mixed handoff with uncontained TypeFinal")
	}

	foundCodeGeneration := false
	for _, d := range diagnostics.Items() {
		if d.Code == CodeGeneration {
			foundCodeGeneration = true
			break
		}
	}
	if !foundCodeGeneration {
		t.Fatal("expected a C0619 diagnostic for uncontained TypeFinal result")
	}
}

// TestAuditHandoffTypeErrorExempt verifies that TypeError-state results
// whose Type is not contained in the type snapshot do NOT by themselves
// cause the audit to fail — recovery states are exempt from the containment
// check. This is a regression guard against over-tightening the check.
func TestAuditHandoffTypeErrorExempt(t *testing.T) {
	// A program with type errors produces TypeError results.
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn conflict() void {
    let x u8 = 1;
    let y i32 = 2;
    let z i32 = x + y;
}
`)})

	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatal("handoff is nil")
	}
	if handoff.Semantics == nil {
		t.Fatal("handoff.Semantics is nil (should be non-nil even with errors)")
	}
	if handoff.Solution == nil {
		t.Fatal("handoff.Solution is nil")
	}

	// Confirm there is at least one TypeError result.
	hasTypeError := false
	for _, st := range handoff.Solution.SymbolTypes() {
		if st.Result.State == infer.TypeError {
			hasTypeError = true
			break
		}
	}
	if !hasTypeError {
		for _, st := range handoff.Solution.SyntaxTypes() {
			if st.Result.State == infer.TypeError {
				hasTypeError = true
				break
			}
		}
	}
	if !hasTypeError {
		for _, st := range handoff.Solution.Slots() {
			if st.Result.State == infer.TypeError {
				hasTypeError = true
				break
			}
		}
	}
	if !hasTypeError {
		t.Fatal("expected at least one TypeError result in the conflicting program")
	}

	// The audit should pass (TypeError results are exempt from containment).
	config := normalizeConfig(Config{})
	diagSet := diagnostic.NewDiagnosticSet()
	result := auditHandoff(handoff, diagSet, config)
	if !result {
		t.Fatal("auditHandoff should have passed despite TypeError results")
	}
}
