package infer

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// TestConventionIgnoresBareStringLiteralExpressionBody is the regression
// guard for the T0501 false positive: a named function (and a method) whose
// "=>" body is a BARE STRING LITERAL used to be misread as a malformed
// leading calling-convention annotation, because convention() scanned every
// direct StringLiteral child of the FunctionDecl node with no positional
// awareness, and the parser attaches an arrow body's tail expression as a
// trailing direct child. Every callable in the fixture must reach
// DeclarationReady with the default Pebble convention and no T0501.
func TestConventionIgnoresBareStringLiteralExpressionBody(t *testing.T) {
	program, diagnostics := prepareSource(t, []byte(`
fn f() str => "hello";
type Box = struct { fn tag(self Box) str => "b"; };
`))
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}
	checked := map[string]bool{}
	for _, candidate := range program.inputs.Resolution.Symbols.All() {
		if candidate.Error || (candidate.Kind != symbol.SymbolFunction && candidate.Kind != symbol.SymbolMethod) {
			continue
		}
		if candidate.Name == "f" || candidate.Name == "tag" {
			checked[candidate.Name] = true
		}
		signature, ok := program.Signature(candidate.ID)
		if !ok || signature.State != DeclarationReady {
			t.Fatalf("callable %q (%v) signature not ready: ok=%v signature=%+v", candidate.Name, candidate.Kind, ok, signature)
		}
		if signature.Convention != types.Pebble {
			t.Fatalf("callable %q convention = %v, want Pebble", candidate.Name, signature.Convention)
		}
	}
	if !checked["f"] || !checked["tag"] {
		t.Fatalf("did not inspect the fixture callables: %v", checked)
	}
}

// TestConventionIgnoresBareStringLiteralBodyInFunctionLiteral is the same
// guard for an anonymous function literal (a syntax.FunctionTerm node), which
// prepareSignatures processes just like a named declaration and which suffers
// the identical false positive.
func TestConventionIgnoresBareStringLiteralBodyInFunctionLiteral(t *testing.T) {
	program, diagnostics := prepareSource(t, []byte(`fn main() int { let cb = fn() str => "hi"; return 0; }`))
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}
	for _, candidate := range program.inputs.Resolution.Symbols.All() {
		if candidate.Error || candidate.Kind != symbol.SymbolFunction {
			continue
		}
		signature, ok := program.Signature(candidate.ID)
		if !ok || signature.State != DeclarationReady {
			t.Fatalf("callable signature not ready: ok=%v signature=%+v", ok, signature)
		}
	}
}

// TestConventionDetectsGenuineAnnotationOnPlainFunction proves the fix does
// not weaken real convention-annotation detection: a leading string on a
// plain "fn" declaration (parsed as a modifier before the function name, the
// only position convention() now scans) must still set the signature's
// convention.
func TestConventionDetectsGenuineAnnotationOnPlainFunction(t *testing.T) {
	program, diagnostics := prepareSource(t, []byte(`fn "C" f() int => 42; fn "Pebble" g() int => 7;`))
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}
	var f, g symbol.SymbolID
	for _, candidate := range program.inputs.Resolution.Symbols.All() {
		switch candidate.Name {
		case "f":
			f = candidate.ID
		case "g":
			g = candidate.ID
		}
	}
	if f == 0 || g == 0 {
		t.Fatalf("fixture callables not found: f=%d g=%d", f, g)
	}
	fSignature, ok := program.Signature(f)
	if !ok || fSignature.State != DeclarationReady || fSignature.Convention != types.C {
		t.Fatalf("f signature=%+v ok=%v", fSignature, ok)
	}
	gSignature, ok := program.Signature(g)
	if !ok || gSignature.State != DeclarationReady || gSignature.Convention != types.Pebble {
		t.Fatalf("g signature=%+v ok=%v", gSignature, ok)
	}
}

// TestConventionStillRejectsMalformedAnnotation proves a nonsense string in
// the genuine leading-annotation position still reports T0501.
func TestConventionStillRejectsMalformedAnnotation(t *testing.T) {
	_, diagnostics := prepareSource(t, []byte(`fn "nonsense" f() int => 42;`))
	if !hasDiagnostic(diagnostics, CodeInvalidType) {
		t.Fatalf("want T0501, got %+v", diagnostics.Items())
	}
}
