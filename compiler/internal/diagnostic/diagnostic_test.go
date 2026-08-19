package diagnostic

import (
	"bytes"
	"encoding/json"
	"reflect"
	"strconv"
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

func TestRenderTextRelated(t *testing.T) {
	sources := source.NewFileSet()
	id, err := sources.Add("sample.peb", []byte("let value = @;\n"))
	if err != nil {
		t.Fatal(err)
	}
	diagnostics := []Diagnostic{{
		Severity: Error,
		Code:     "L0001",
		Message:  "unexpected character '@'",
		Primary:  Label{Span: source.NewSpan(id, 12, 13), Message: "primary label"},
		Related: []Label{{
			Span:    source.NewSpan(id, 12, 13),
			Message: "value may not be initialized here",
		}},
		Notes: []string{"a note"},
		Help:  []string{"a help hint"},
	}}

	var output bytes.Buffer
	if err := RenderText(&output, sources, diagnostics); err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"sample.peb:1:13",
		"error[L0001]",
		"--> sample.peb:1:13: value may not be initialized here",
		"a note",
		"a help hint",
	} {
		if !strings.Contains(output.String(), want) {
			t.Fatalf("output does not contain %q:\n%s", want, output.String())
		}
	}
}

func TestRenderTextRelatedCrossFile(t *testing.T) {
	sources := source.NewFileSet()
	primaryID, err := sources.Add("main.peb", []byte("let value = @;\n"))
	if err != nil {
		t.Fatal(err)
	}
	relatedID, err := sources.Add("types.peb", []byte("type Alias = Int\nvar out: Int = 0\n"))
	if err != nil {
		t.Fatal(err)
	}
	diagnostics := []Diagnostic{{
		Severity: Error,
		Code:     "C0621",
		Message:  "generic requirement failed at this instantiation site",
		Primary:  Label{Span: source.NewSpan(primaryID, 12, 13), Message: "used here"},
		Related: []Label{{
			Span:    source.NewSpan(relatedID, 32, 33),
			Message: "generic requirement declared here",
		}},
	}}

	var output bytes.Buffer
	if err := RenderText(&output, sources, diagnostics); err != nil {
		t.Fatal(err)
	}
	text := output.String()
	if !strings.Contains(text, "main.peb:1:13") {
		t.Fatalf("output does not attribute primary to main.peb:1:13:\n%s", text)
	}
	if !strings.Contains(text, "--> types.peb:2:16: generic requirement declared here") {
		t.Fatalf("output does not attribute related label to types.peb:2:16:\n%s", text)
	}
	if strings.Index(text, "main.peb") > strings.Index(text, "types.peb") {
		t.Fatalf("related label must be rendered after the primary block:\n%s", text)
	}
}

func TestRenderJSON(t *testing.T) {
	sources := source.NewFileSet()
	primaryID, err := sources.Add("main.peb", []byte("let value = @;\n"))
	if err != nil {
		t.Fatal(err)
	}
	relatedID, err := sources.Add("types.peb", []byte("type Alias = Int\nvar out: Int = 0\n"))
	if err != nil {
		t.Fatal(err)
	}
	diagnostics := []Diagnostic{
		{
			Severity: Error,
			Code:     "C0621",
			Message:  "generic requirement failed at this instantiation site",
			Primary:  Label{Span: source.NewSpan(primaryID, 12, 13), Message: "used here"},
			Related: []Label{{
				Span:    source.NewSpan(relatedID, 32, 33),
				Message: "generic requirement declared here",
			}},
			Notes: []string{"note one", "note two"},
			Help:  []string{"add a bound to the generic parameter"},
		},
		{
			Severity: Warning,
			Code:     "W0007",
			Message:  "unused variable 'value'",
			Primary:  Label{Span: source.NewSpan(primaryID, 4, 7)},
		},
	}

	var output bytes.Buffer
	if err := RenderJSON(&output, sources, diagnostics); err != nil {
		t.Fatal(err)
	}
	raw := output.String()

	var decoded []renderedDiagnostic
	if err := json.Unmarshal([]byte(raw), &decoded); err != nil {
		t.Fatalf("output is not valid JSON: %v\n%s", err, raw)
	}
	if len(decoded) != 2 {
		t.Fatalf("decoded %d diagnostics, want 2:\n%s", len(decoded), raw)
	}

	first := decoded[0]
	if first.Severity != "error" {
		t.Errorf("first severity = %q, want %q", first.Severity, "error")
	}
	if first.Code != "C0621" {
		t.Errorf("first code = %q, want %q", first.Code, "C0621")
	}
	if first.Message != "generic requirement failed at this instantiation site" {
		t.Errorf("first message = %q", first.Message)
	}
	if first.Path != "main.peb" || first.Line != 1 || first.Column != 13 {
		t.Errorf("first primary = %s:%d:%d, want main.peb:1:13", first.Path, first.Line, first.Column)
	}
	if first.Label != "used here" {
		t.Errorf("first primary label message = %q, want %q", first.Label, "used here")
	}
	if len(first.Related) != 1 {
		t.Fatalf("first related count = %d, want 1:\n%s", len(first.Related), raw)
	}
	related := first.Related[0]
	if related.Path != "types.peb" || related.Line != 2 || related.Column != 16 {
		t.Errorf("related = %s:%d:%d, want types.peb:2:16", related.Path, related.Line, related.Column)
	}
	if related.Message != "generic requirement declared here" {
		t.Errorf("related message = %q", related.Message)
	}
	if !reflect.DeepEqual(first.Notes, []string{"note one", "note two"}) {
		t.Errorf("notes = %#v, want [note one note two]", first.Notes)
	}
	if !reflect.DeepEqual(first.Help, []string{"add a bound to the generic parameter"}) {
		t.Errorf("help = %#v", first.Help)
	}

	second := decoded[1]
	if second.Severity != "warning" {
		t.Errorf("second severity = %q, want %q", second.Severity, "warning")
	}
	if second.Code != "W0007" {
		t.Errorf("second code = %q, want %q", second.Code, "W0007")
	}
	if second.Path != "main.peb" || second.Line != 1 || second.Column != 5 {
		t.Errorf("second primary = %s:%d:%d, want main.peb:1:5", second.Path, second.Line, second.Column)
	}
	if len(second.Related) != 0 {
		t.Errorf("second related count = %d, want 0", len(second.Related))
	}
	if len(second.Notes) != 0 || len(second.Help) != 0 {
		t.Errorf("second notes/help = %#v/%#v, want empty", second.Notes, second.Help)
	}

	for _, key := range []string{"severity", "code", "message", "path", "line", "column", "label", "related", "notes", "help"} {
		if !strings.Contains(raw, "\""+key+"\"") {
			t.Errorf("JSON output is missing lowercase key %q:\n%s", key, raw)
		}
	}
}

func TestRenderJSONEmpty(t *testing.T) {
	var output bytes.Buffer
	if err := RenderJSON(&output, source.NewFileSet(), nil); err != nil {
		t.Fatal(err)
	}
	if got := strings.TrimSpace(output.String()); got != "[]" {
		t.Fatalf("empty input rendered as %q, want []", got)
	}
}

func TestRenderJSONUnresolvedPrimary(t *testing.T) {
	sources := source.NewFileSet()
	diagnostics := []Diagnostic{{
		Severity: Error,
		Code:     "E0001",
		Message:  "no source available",
		Primary:  Label{Span: source.NewSpan(99, 0, 1)},
	}}

	var output bytes.Buffer
	if err := RenderJSON(&output, sources, diagnostics); err != nil {
		t.Fatal(err)
	}
	var decoded []renderedDiagnostic
	if err := json.Unmarshal(output.Bytes(), &decoded); err != nil {
		t.Fatalf("output is not valid JSON: %v\n%s", err, output.String())
	}
	first := decoded[0]
	if first.Severity != "error" || first.Code != "E0001" || first.Message != "no source available" {
		t.Errorf("unexpected diagnostic: %#v", first)
	}
	if first.Path != "" || first.Line != 0 || first.Column != 0 {
		t.Errorf("unresolved primary should fall back to empty path and zero line/column, got %s:%d:%d", first.Path, first.Line, first.Column)
	}
}

func TestRenderTextAndJSONAgree(t *testing.T) {
	sources := source.NewFileSet()
	id, err := sources.Add("agree.peb", []byte("let value = @;\n"))
	if err != nil {
		t.Fatal(err)
	}
	d := Diagnostic{
		Severity: Error,
		Code:     "L0001",
		Message:  "unexpected character '@'",
		Primary:  Label{Span: source.NewSpan(id, 12, 13)},
	}

	var text bytes.Buffer
	if err := RenderText(&text, sources, []Diagnostic{d}); err != nil {
		t.Fatal(err)
	}
	textLine, textColumn := extractTextPosition(t, text.String(), "agree.peb:")

	var jsonOut bytes.Buffer
	if err := RenderJSON(&jsonOut, sources, []Diagnostic{d}); err != nil {
		t.Fatal(err)
	}
	var decoded []renderedDiagnostic
	if err := json.Unmarshal(jsonOut.Bytes(), &decoded); err != nil {
		t.Fatalf("output is not valid JSON: %v\n%s", err, jsonOut.String())
	}
	if len(decoded) != 1 {
		t.Fatalf("decoded %d diagnostics, want 1", len(decoded))
	}
	if decoded[0].Line != textLine || decoded[0].Column != textColumn {
		t.Errorf("RenderText reports %d:%d but RenderJSON reports %d:%d", textLine, textColumn, decoded[0].Line, decoded[0].Column)
	}
}

func extractTextPosition(t *testing.T, output, pathPrefix string) (line, column int) {
	t.Helper()
	idx := strings.Index(output, pathPrefix)
	if idx < 0 {
		t.Fatalf("output does not contain %q:\n%s", pathPrefix, output)
	}
	rest := output[idx+len(pathPrefix):]
	lineEnd := strings.IndexByte(rest, ':')
	columnEnd := strings.IndexByte(rest[lineEnd+1:], ':')
	if lineEnd < 0 || columnEnd < 0 {
		t.Fatalf("output has no line:column after %q:\n%s", pathPrefix, output)
	}
	var err error
	line, err = strconv.Atoi(rest[:lineEnd])
	if err != nil {
		t.Fatalf("bad line in %q: %v", output[idx:], err)
	}
	column, err = strconv.Atoi(rest[lineEnd+1 : lineEnd+1+columnEnd])
	if err != nil {
		t.Fatalf("bad column in %q: %v", output[idx:], err)
	}
	return line, column
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

func TestEditDistance(t *testing.T) {
	cases := []struct {
		a, b string
		want int
	}{
		{"", "", 0},
		{"a", "", 1},
		{"", "a", 1},
		{"a", "a", 0},
		{"kitten", "sitting", 3},
		{"compute_total", "compute_totl", 1},
		{"count", "cout", 1},
		{"count", "counts", 1},
		{"xyzzy", "count", 5},
	}
	for _, tc := range cases {
		if got := EditDistance(tc.a, tc.b); got != tc.want {
			t.Errorf("EditDistance(%q, %q) = %d, want %d", tc.a, tc.b, got, tc.want)
		}
	}
}

func TestSuggest(t *testing.T) {
	candidates := []string{"compute_total", "wrapping_add_u64", "count", "main"}

	t.Run("closest within threshold", func(t *testing.T) {
		got, ok := Suggest("compute_totl", candidates)
		if !ok || got != "compute_total" {
			t.Fatalf("Suggest = %q, %t; want %q, true", got, ok, "compute_total")
		}
	})
	t.Run("member typo", func(t *testing.T) {
		got, ok := Suggest("cout", []string{"count"})
		if !ok || got != "count" {
			t.Fatalf("Suggest = %q, %t; want %q, true", got, ok, "count")
		}
	})
	t.Run("unrelated name rejected", func(t *testing.T) {
		if got, ok := Suggest("xyzzy", candidates); ok {
			t.Fatalf("Suggest(%q, %v) = %q, true; want false", "xyzzy", candidates, got)
		}
	})
	t.Run("empty target rejected", func(t *testing.T) {
		if got, ok := Suggest("", candidates); ok {
			t.Fatalf("Suggest(\"\", ...) = %q, true; want false", got)
		}
	})
	t.Run("no candidates rejected", func(t *testing.T) {
		if got, ok := Suggest("compute_totl", nil); ok {
			t.Fatalf("Suggest(..., nil) = %q, true; want false", got)
		}
	})
	t.Run("tie resolves deterministically", func(t *testing.T) {
		// "bob" is distance 1 from both "job" (substitution) and "bo"
		// (deletion); the lexicographically smaller one must win regardless of
		// input order.
		for _, order := range [][]string{{"job", "bo"}, {"bo", "job"}} {
			got, ok := Suggest("bob", order)
			if !ok || got != "bo" {
				t.Fatalf("Suggest(%q, %v) = %q, %t; want %q, true", "bob", order, got, ok, "bo")
			}
		}
	})
}
