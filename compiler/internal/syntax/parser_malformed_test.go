package syntax

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

func TestParseMalformedInterpolationsTerminates(t *testing.T) {
	for _, contents := range []string{
		"fn{`f {if",
		"fn{`{) ",
		"fn{`text {while",
		"fn{`text {",
	} {
		t.Run(contents, func(t *testing.T) {
			files := source.NewFileSet()
			id, err := files.Add("main.peb", []byte(contents))
			if err != nil {
				t.Fatal(err)
			}
			file, ok := files.File(id)
			if !ok {
				t.Fatal("source file was not stored")
			}
			diagnostics := diagnostic.NewDiagnosticSet()
			Parse(file, diagnostics)
			if len(diagnostics.Items()) == 0 {
				t.Fatal("malformed source produced no diagnostics")
			}
		})
	}
}
