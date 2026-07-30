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
