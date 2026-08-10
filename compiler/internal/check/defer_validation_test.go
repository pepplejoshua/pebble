package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

func validateDeferFixture(t *testing.T, source string, config Config) (*diagnostic.DiagnosticSet, bool) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, config)
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("06a reported errors: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(config))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, validateDefers(handoff, records, diagnostics, config)
}

func TestValidateDefersRejectsDeferredControlStatements(t *testing.T) {
	cases := []struct {
		name   string
		source string
		code   diagnostic.Code
	}{
		{name: "return", source: `fn f() void { defer return; }`, code: CodeInvalidDefer},
		{name: "jumps", source: `fn f(flag bool) void { while flag { defer break; defer continue; } }`, code: CodeInvalidDefer},
		{name: "nested", source: `fn f() void { defer defer print 1; }`, code: CodeInvalidDefer},
		{name: "return inside deferred block", source: `fn f() int { defer { return 1; } return 0; }`, code: CodeInvalidDefer},
		{name: "break inside deferred block escapes enclosing loop", source: `fn f() void { var i int = 0; while i < 3 { defer { break; } i = i + 1; } }`, code: CodeInvalidDefer},
		{name: "return inside deferred if", source: `fn f() int { defer if true { return 1; } return 0; }`, code: CodeInvalidDefer},
		{name: "nested defer inside deferred block", source: `fn f() void { defer { defer print 1; } }`, code: CodeInvalidDefer},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics, valid := validateDeferFixture(t, tc.source, Config{})
			if valid || !hasControlDiagnostic(diagnostics, tc.code) {
				t.Fatalf("invalid defer accepted: valid=%v diagnostics=%+v", valid, diagnostics.Items())
			}
		})
	}
}

func TestValidateDefersAllowsDeferredStatementsAndNestedRegions(t *testing.T) {
	diagnostics, valid := validateDeferFixture(t, `
fn f(flag bool, x i32) void {
    defer print 1;
    defer { print 2; }
    defer x = 3;
    if flag {
        defer print 4;
        while flag { defer print 5; break; }
    }
    return;
}
`, Config{})
	if !valid || hasControlDiagnostic(diagnostics, CodeInvalidDefer) || hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatalf("valid defers rejected: %+v", diagnostics.Items())
	}
}

// TestValidateDefersAllowsContainedExitInsideDeferredBlock verifies that a
// break/continue whose target loop is itself entirely inside the deferred
// statement is not rejected — only an exit whose target lies OUTSIDE the
// deferred statement's own region is C0613. The IR builder can terminate on
// a contained exit (its defer-chain walk never crosses the deferred
// statement's registered region), unlike an escaping exit, which is what
// crashed the compiler before this fix.
func TestValidateDefersAllowsContainedExitInsideDeferredBlock(t *testing.T) {
	diagnostics, valid := validateDeferFixture(t, `
fn f() void {
    defer {
        var i int = 0;
        while i < 3 {
            i = i + 1;
            break;
        }
    }
}
`, Config{})
	if !valid || hasControlDiagnostic(diagnostics, CodeInvalidDefer) || hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatalf("a break contained within a loop fully inside a deferred block was wrongly rejected: %+v", diagnostics.Items())
	}
}

func TestValidateDefersEnforcesMaxDeferEdges(t *testing.T) {
	diagnostics, valid := validateDeferFixture(t, `
fn f(flag bool) void {
    defer print 1;
    defer print 2;
    if flag {
        defer print 2;
        return;
    }
    return;
}
`, Config{MaxDeferEdges: 1})
	if valid || !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatalf("defer edge budget was not enforced: valid=%v diagnostics=%+v", valid, diagnostics.Items())
	}
}

// TestValidateDefersRegionSequenceSourceOrder verifies that defer edges from a
// return inside a later `if` are still accounted for when a leading defer
// statement precedes the control-flow sibling in source order. Before the
// ordering fix the trailing `return;` was evaluated before the `if` record,
// which dropped the if-body's return exit and undercounted defer edges, so the
// budget below was (wrongly) not exhausted.
func TestValidateDefersRegionSequenceSourceOrder(t *testing.T) {
	diagnostics, valid := validateDeferFixture(t, `
fn f(flag bool) void {
    defer print 1;
    if flag {
        return;
    }
    return;
}
`, Config{MaxDeferEdges: 1})
	if valid || !hasControlDiagnostic(diagnostics, CodeGeneration) {
		t.Fatalf("defer edge budget was not enforced with source-ordered sequence: valid=%v diagnostics=%+v", valid, diagnostics.Items())
	}
}
