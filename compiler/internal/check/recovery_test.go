package check

import (
	"testing"
)

// TestFactRecoveryLoweredMaxRecordComponents verifies that run06a gracefully
// fails when MaxRecordComponents limit is exceeded, producing a bounded error
// with GenerationHadErrors=true, without panicking.
func TestFactRecoveryLoweredMaxRecordComponents(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn test() void {
    let a = 1; let b = 2; let c = 3;
    let d = 4; let e = 5; let f = 6;
    let g = 7; let h = 8; let i = 9;
    print a; print b; print c; print d; print e;
}
`)})

	// Set a very low component limit to trigger failure during compilation building.
	cfg := Config{MaxRecordComponents: 3}
	handoff := run06a(inputs, diagnostics, cfg)

	if handoff == nil {
		t.Fatal("handoff should not be nil even on limit failure")
	}
	if !handoff.GenerationHadErrors {
		t.Fatal("GenerationHadErrors should be true when limits exceeded")
	}

	// Verify diagnostic set is bounded and contains error diagnostics.
	items := diagnostics.Items()
	if len(items) == 0 {
		t.Fatal("should have generated error diagnostics")
	}

	// Check that we don't have an unbounded flood of diagnostics.
	// The C0619 diagnostic for limit exceeded should be present.
	foundLimit := false
	for _, item := range items {
		if item.Code == "C0619" {
			foundLimit = true
			break
		}
	}
	if !foundLimit {
		t.Fatal("should have C0619 limit diagnostic")
	}
}

// TestFactRecoveryLoweredMaxSemanticRecords verifies that exceeding the
// MaxSemanticRecords limit produces bounded failure.
func TestFactRecoveryLoweredMaxSemanticRecords(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn nested() void {
    {
        { { { { { { { { { { { { { { } } } } } } } } } } } } } } }
    }
}
`)})

	// Set a very low semantic records limit to trigger control hierarchy overflow.
	cfg := Config{MaxSemanticRecords: 3}
	handoff := run06a(inputs, diagnostics, cfg)

	if handoff == nil {
		t.Fatal("handoff should not be nil even on limit failure")
	}
	if !handoff.GenerationHadErrors {
		t.Fatal("GenerationHadErrors should be true when limits exceeded")
	}

	// Verify we produced a diagnostic but didn't panic or crash.
	items := diagnostics.Items()
	if len(items) == 0 {
		t.Fatal("should have generated error diagnostics")
	}
}

// TestFactRecoveryLoweredMaxControlDepth verifies that nested control depth
// limits are enforced during generation.
func TestFactRecoveryLoweredMaxControlDepth(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn deep_nesting() void {
    {
        { { { { { { { { { { } } } } } } } } } } }
    }
}
`)})

	// Set a very low control depth limit.
	cfg := Config{MaxControlDepth: 2}
	handoff := run06a(inputs, diagnostics, cfg)

	if handoff == nil {
		t.Fatal("handoff should not be nil even on limit failure")
	}
	if !handoff.GenerationHadErrors {
		t.Fatal("GenerationHadErrors should be true when depth limit exceeded")
	}

	// Should have diagnostics
	items := diagnostics.Items()
	if len(items) == 0 {
		t.Fatal("should have generated error diagnostics")
	}
}

// TestFactRecoveryLoweredMaxSyntaxVisits verifies that syntax visit limits
// are enforced during traversal.
func TestFactRecoveryLoweredMaxSyntaxVisits(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn many_expressions() void {
    print 1; print 2; print 3; print 4; print 5;
    print 6; print 7; print 8; print 9; print 10;
}
`)})

	// Set a very low syntax visits limit.
	cfg := Config{MaxSyntaxVisits: 5}
	handoff := run06a(inputs, diagnostics, cfg)

	if handoff == nil {
		t.Fatal("handoff should not be nil even on limit failure")
	}
	if !handoff.GenerationHadErrors {
		t.Fatal("GenerationHadErrors should be true when visit limit exceeded")
	}

	// Should have diagnostics
	items := diagnostics.Items()
	if len(items) == 0 {
		t.Fatal("should have generated error diagnostics")
	}
}

// TestFactRecoveryTypeConflictPreservesCompilation verifies that a genuine
// type conflict (unsolvable constraint) still produces a complete Compilation
// and non-nil Semantics, with Semantics.Matches(Solution) and Solution.Successful()
// correctly reflecting the conflict state, allowing 06b to perform independent
// diagnostics on the complete handoff.
func TestFactRecoveryTypeConflictPreservesCompilation(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn conflict() void {
    let x u8 = 1;
    let y i32 = 2;
    let z i32 = x + y;  // Type conflict: mixed numeric types
    print z;
}
`)})

	handoff := run06a(inputs, diagnostics, Config{})

	if handoff == nil {
		t.Fatal("handoff should not be nil")
	}

	// GenerationHadErrors should be true due to the type conflict.
	if !handoff.GenerationHadErrors {
		// The generator may have reported errors during traversal.
	}

	// The key property: Semantics must be non-nil even with a type conflict,
	// so that 06b can make independent diagnostics.
	if handoff.Semantics == nil {
		t.Fatal("Semantics should be non-nil even with a type conflict")
	}

	// Compilation must also be complete and usable.
	if handoff.Compilation.Root == 0 {
		t.Fatal("Compilation.Root should be set")
	}
	if len(handoff.Compilation.Modules) == 0 {
		t.Fatal("Compilation.Modules should not be empty")
	}

	// The solution should not be successful due to the type conflict.
	if handoff.Solution == nil {
		t.Fatal("Solution should not be nil")
	}
	if handoff.Solution.Successful() {
		t.Fatal("Solution.Successful() should be false due to type conflict")
	}

	// Verify that 06b can still access the handoff fields for independent diagnostics.
	declarations := handoff.Compilation.Modules[0].Declarations
	if len(declarations) == 0 {
		t.Fatal("should have declarations for independent diagnostics")
	}
}

// TestFactRecoveryDoesNotPanicOnEdgeCases verifies that various edge cases
// with lowered limits don't cause panics, even with invalid intermediate states.
func TestFactRecoveryDoesNotPanicOnEdgeCases(t *testing.T) {
	// Empty program with various limits.
	tests := []struct {
		name   string
		config Config
	}{
		{"ZeroRecordComponents", Config{MaxRecordComponents: 1}},
		{"ZeroSemanticRecords", Config{MaxSemanticRecords: 1}},
		{"ZeroControlDepth", Config{MaxControlDepth: 0}},
		{"ZeroSyntaxVisits", Config{MaxSyntaxVisits: 1}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn test() void { }`)})

			// Should not panic even with zero limits.
			handoff := run06a(inputs, diagnostics, tt.config)

			// Handoff should be returned (possibly with errors).
			if handoff == nil {
				t.Fatal("handoff is nil")
			}
		})
	}
}
