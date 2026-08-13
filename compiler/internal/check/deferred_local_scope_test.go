package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// runDeferredLocalFixture drives one source through the full checker pipeline
// and returns the diagnostics. A fixture whose deferred local declaration's
// binding leaks into the enclosing scope (the Phase 3 #29 defect) is exactly a
// fixture the resolver should reject with N0001: a reference to the deferred
// local outside the defer must not resolve.
func runDeferredLocalFixture(t *testing.T, source string) (*diagnostic.DiagnosticSet, *Result) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	return diagnostics, Check(inputs, diagnostics, Config{})
}

// TestCheckerDeferredLocalBindingScopedToDefer proves a bare deferred local
// declaration (`defer var x = 5;`) does NOT leak its binding into the
// enclosing function scope: a reference to x after the defer must be a clean
// N0001 undefined-name error. Before the fix the resolver declared the
// deferred binding in the enclosing scope and this program compiled.
func TestCheckerDeferredLocalBindingScopedToDefer(t *testing.T) {
	t.Parallel()
	diagnostics, result := runDeferredLocalFixture(t, `
fn f() void {
    defer var x i32 = 5;
    print x;
    return;
}
`)
	if result.Successful() || !hasControlDiagnostic(diagnostics, symbol.CodeUndefinedName) {
		t.Fatalf("a reference to a bare deferred local outside the defer was accepted: successful=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
}

// TestCheckerDeferredBlockLocalInvisibleOutsideDefer proves a block-wrapped
// deferred local declaration (`defer { var x = 5; }`) is scoped to the
// deferred block: a reference to x after the defer is a clean N0001
// undefined-name error.
func TestCheckerDeferredBlockLocalInvisibleOutsideDefer(t *testing.T) {
	t.Parallel()
	diagnostics, result := runDeferredLocalFixture(t, `
fn f() void {
    defer { var x i32 = 5; print x; }
    print x;
    return;
}
`)
	if result.Successful() || !hasControlDiagnostic(diagnostics, symbol.CodeUndefinedName) {
		t.Fatalf("a reference to a deferred block local outside the defer was accepted: successful=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
}

// TestCheckerDeferredLocalUsedInsideOwnDeferAccepts proves a deferred local
// declaration used only inside its own defer scope still checks clean: a bare
// deferred local with no external reference, and a block-wrapped deferred
// local referenced by a later statement inside the same block. Both must build
// typed IR (the checker accepting a deferred binding is the V1-parity behavior
// the backend gap row pairs with the scope leak).
func TestCheckerDeferredLocalUsedInsideOwnDeferAccepts(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name   string
		source string
	}{
		{
			name:   "bare",
			source: "fn f() void { defer var x i32 = 5; return; }\n",
		},
		{
			name:   "block-local-referenced-in-block",
			source: "fn f() void { defer { var x i32 = 5; print x; } return; }\n",
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			diagnostics, result := runDeferredLocalFixture(t, tc.source)
			if !result.Successful() || result.IR() == nil {
				t.Fatalf("valid deferred local declaration rejected: successful=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
			}
		})
	}
}
