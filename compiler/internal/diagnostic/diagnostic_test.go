package diagnostic

import (
	"bytes"
	"reflect"
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

func TestDiagnosticSetReplaceNonFinalPreservesDiagnostic(t *testing.T) {
	set := NewDiagnosticSet()
	first := Diagnostic{Severity: Warning, Code: "W0001", Message: "first"}
	original := Diagnostic{Severity: Note, Code: "N0001", Message: "original"}
	last := Diagnostic{Severity: Error, Code: "E0001", Message: "last"}
	set.Add(first)
	set.Add(original)
	set.Add(last)

	replacement := Diagnostic{
		Severity: Error,
		Code:     "E0002",
		Message:  "replacement",
		Primary: Label{
			Span:    source.NewSpan(7, 11, 19),
			Message: "primary label",
		},
		Related: []Label{{
			Span:    source.NewSpan(8, 23, 29),
			Message: "related label",
		}},
		Notes: []string{"first note", "second note"},
		Help:  []string{"replacement help"},
	}
	if !set.Replace(1, replacement) {
		t.Fatal("Replace returned false for a valid index")
	}

	items := set.Items()
	if len(items) != 3 {
		t.Fatalf("Len() = %d, want 3", set.Len())
	}
	if !reflect.DeepEqual(items[0], first) || !reflect.DeepEqual(items[2], last) {
		t.Fatalf("Replace changed diagnostic order: %#v", items)
	}
	if !reflect.DeepEqual(items[1], replacement) {
		t.Fatalf("replacement = %#v, want %#v", items[1], replacement)
	}
	if got := set.ErrorCount(); got != 2 {
		t.Fatalf("ErrorCount() = %d, want 2", got)
	}
	if !set.HasErrors() {
		t.Fatal("HasErrors() = false, want true")
	}
}

func TestDiagnosticSetReplaceSeverityTransitions(t *testing.T) {
	severities := []Severity{Error, Warning, Note}
	for _, from := range severities {
		for _, to := range severities {
			name := from.String() + "_to_" + to.String()
			t.Run(name, func(t *testing.T) {
				set := NewDiagnosticSet()
				set.Add(Diagnostic{Severity: from})

				if !set.Replace(0, Diagnostic{Severity: to}) {
					t.Fatal("Replace returned false for a valid index")
				}

				wantErrors := 0
				if to == Error {
					wantErrors = 1
				}
				if got := set.ErrorCount(); got != wantErrors {
					t.Fatalf("ErrorCount() = %d, want %d", got, wantErrors)
				}
				if got := set.HasErrors(); got != (wantErrors != 0) {
					t.Fatalf("HasErrors() = %t, want %t", got, wantErrors != 0)
				}
			})
		}
	}
}

func TestDiagnosticSetReplaceInvalidIndexIsAtomic(t *testing.T) {
	set := NewDiagnosticSet()
	set.Add(Diagnostic{Severity: Warning, Code: "W0001", Message: "first"})
	set.Add(Diagnostic{Severity: Error, Code: "E0001", Message: "second"})
	wantItems := set.Items()
	wantErrors := set.ErrorCount()

	for _, index := range []int{-1, set.Len(), set.Len() + 1} {
		if set.Replace(index, Diagnostic{Severity: Error, Code: "E9999"}) {
			t.Errorf("Replace(%d, ...) = true, want false", index)
		}
		if got := set.Items(); !reflect.DeepEqual(got, wantItems) {
			t.Fatalf("Replace(%d, ...) mutated items: %#v", index, got)
		}
		if got := set.ErrorCount(); got != wantErrors {
			t.Fatalf("Replace(%d, ...) changed ErrorCount to %d, want %d", index, got, wantErrors)
		}
		if got := set.Len(); got != len(wantItems) {
			t.Fatalf("Replace(%d, ...) changed Len to %d, want %d", index, got, len(wantItems))
		}
	}
}

func TestDiagnosticSetReplaceNilReceiver(t *testing.T) {
	var set *DiagnosticSet
	if set.Replace(0, Diagnostic{Severity: Error}) {
		t.Fatal("Replace on nil receiver returned true, want false")
	}
}
