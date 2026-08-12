package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

// TestCheckAcceptsBareStringLiteralExpressionBody is the end-to-end checker
// guard for the T0501 false positive: a function whose "=>" body is a bare
// string literal must prepare a ready signature (previously it was wrongly
// marked DeclarationError, which cascaded into call-site failures), so the
// program that calls it must check clean.
func TestCheckAcceptsBareStringLiteralExpressionBody(t *testing.T) {
	diagnostics, result := runConventionCheck(t, `
fn f() str => "hello";
fn main() int { let s str = f(); if s == "hello" { return 42; } return 1; }
`)
	if !result.Successful() || hasValidationDiagnostic(diagnostics, diagnostic.Code("T0501")) || diagnostics.HasErrors() {
		t.Fatalf("bare string-literal arrow body was rejected: %+v", diagnostics.Items())
	}
}

// TestCheckAcceptsBareStringLiteralMethodBody is the same guard for a method
// declared inside a struct.
func TestCheckAcceptsBareStringLiteralMethodBody(t *testing.T) {
	diagnostics, result := runConventionCheck(t, `
type Box = struct { fn tag(self Box) str => "b"; };
fn main() int { return 0; }
`)
	if !result.Successful() || diagnostics.HasErrors() {
		t.Fatalf("bare string-literal method body was rejected: %+v", diagnostics.Items())
	}
}

// TestCheckAcceptsGenuinePebbleAnnotation proves the fix does not weaken real
// convention-annotation detection: a leading "Pebble" convention string on a
// plain "fn" declaration is still accepted (Pebble-convention callables may
// have bodies, so this checks clean end to end).
func TestCheckAcceptsGenuinePebbleAnnotation(t *testing.T) {
	diagnostics, result := runConventionCheck(t, `
fn "Pebble" f() int => 42;
fn main() int { return f(); }
`)
	if !result.Successful() || diagnostics.HasErrors() {
		t.Fatalf("genuine Pebble convention annotation was rejected: %+v", diagnostics.Items())
	}
}

// TestCheckStillRejectsMalformedConventionAnnotation proves a nonsense string
// in the genuine leading-annotation position still reports T0501 rather than
// being silently accepted.
func TestCheckStillRejectsMalformedConventionAnnotation(t *testing.T) {
	diagnostics, result := runConventionCheck(t, `
fn "nonsense" f() int => 42;
fn main() int { return f(); }
`)
	if result.Successful() || !hasValidationDiagnostic(diagnostics, diagnostic.Code("T0501")) {
		t.Fatalf("malformed convention annotation was not rejected with T0501: %+v", diagnostics.Items())
	}
}

func runConventionCheck(t *testing.T, source string) (*diagnostic.DiagnosticSet, *Result) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	return diagnostics, Check(inputs, diagnostics, Config{})
}
