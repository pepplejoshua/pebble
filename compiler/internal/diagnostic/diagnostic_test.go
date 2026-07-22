package diagnostic

import (
	"bytes"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

func TestRenderText(t *testing.T) {
	sources := source.NewFileSet()
	id, err := sources.Add("sample.peb", []byte("let value = @;\n"))
	if err != nil {
		t.Fatal(err)
	}
	diagnostics := NewDiagnosticSet()
	diagnostics.Error("L0001", "unexpected character '@'", source.NewSpan(id, 12, 13))

	var output bytes.Buffer
	if err := RenderText(&output, sources, diagnostics.Items()); err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"sample.peb:1:13", "error[L0001]", "unexpected character '@'", "^"} {
		if !strings.Contains(output.String(), want) {
			t.Fatalf("output does not contain %q:\n%s", want, output.String())
		}
	}
}
