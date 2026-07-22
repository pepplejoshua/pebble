package syntax

import (
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

func TestLexerSourceCorpus(t *testing.T) {
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("could not locate lexer test source")
	}
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(filename), "..", "..", ".."))
	corpusRoot := filepath.Join(repoRoot, "tests", "lexer")

	for _, expectation := range []struct {
		directory string
		valid     bool
	}{
		{directory: "valid", valid: true},
		{directory: "invalid", valid: false},
	} {
		caseRoot := filepath.Join(corpusRoot, expectation.directory)
		paths, err := collectPebbleFiles(caseRoot)
		if err != nil {
			t.Fatal(err)
		}
		if len(paths) == 0 {
			t.Fatalf("no lexer cases found in %s", expectation.directory)
		}
		slices.Sort(paths)

		for _, path := range paths {
			path := path
			relativePath, err := filepath.Rel(caseRoot, path)
			if err != nil {
				t.Fatal(err)
			}
			t.Run(filepath.ToSlash(relativePath), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				sources := source.NewFileSet()
				id, err := sources.Add(filepath.ToSlash(path), contents)
				if err != nil {
					t.Fatal(err)
				}
				file, _ := sources.File(id)
				diagnostics := diagnostic.NewDiagnosticSet()
				lexer := NewLexer(file, diagnostics)

				limit := len(contents) + 32
				for count := 0; ; count++ {
					if count > limit {
						t.Fatalf("lexer did not reach EOF after %d tokens", limit)
					}
					if lexer.Next().Kind == EOF {
						break
					}
				}

				if expectation.valid && diagnostics.HasErrors() {
					t.Fatalf("valid source produced diagnostics:\n%s", renderDiagnostics(t, sources, diagnostics))
				}
				if !expectation.valid && !diagnostics.HasErrors() {
					t.Fatal("invalid source produced no diagnostics")
				}
				if !expectation.valid {
					expectedCode := diagnostic.Code(filepath.Base(filepath.Dir(path)))
					for _, item := range diagnostics.Items() {
						if item.Severity == diagnostic.Error && item.Code != expectedCode {
							t.Fatalf("error code = %s, want %s:\n%s", item.Code, expectedCode, renderDiagnostics(t, sources, diagnostics))
						}
					}
				}
			})
		}
	}
}

func collectPebbleFiles(root string) ([]string, error) {
	var paths []string
	err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !entry.IsDir() && filepath.Ext(path) == ".peb" {
			paths = append(paths, path)
		}
		return nil
	})
	slices.Sort(paths)
	return paths, err
}

func TestLexerTokenBoundaries(t *testing.T) {
	tokens, diagnostics := lexText(t, "fn int Vec[u8] 52u8 1..=2")
	want := []TokenKind{
		KwFn,
		Identifier,
		Identifier,
		LeftBracket,
		Identifier,
		RightBracket,
		IntegerLiteral,
		Identifier,
		IntegerLiteral,
		RangeInclusive,
		IntegerLiteral,
		EOF,
	}
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics: %+v", diagnostics.Items())
	}
	if !slices.Equal(tokens, want) {
		t.Fatalf("tokens:\n got: %v\nwant: %v", tokens, want)
	}
}

func TestInterpolationTokens(t *testing.T) {
	tokens, diagnostics := lexText(t, "`a {x + 1} b`")
	want := []TokenKind{
		InterpolationStart,
		InterpolationText,
		InterpolationExprStart,
		Identifier,
		Plus,
		IntegerLiteral,
		InterpolationExprEnd,
		InterpolationText,
		InterpolationEnd,
		EOF,
	}
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics: %+v", diagnostics.Items())
	}
	if !slices.Equal(tokens, want) {
		t.Fatalf("tokens:\n got: %v\nwant: %v", tokens, want)
	}
}

func lexText(t *testing.T, text string) ([]TokenKind, *diagnostic.DiagnosticSet) {
	t.Helper()
	sources := source.NewFileSet()
	id, err := sources.Add("test.peb", []byte(text))
	if err != nil {
		t.Fatal(err)
	}
	file, _ := sources.File(id)
	diagnostics := diagnostic.NewDiagnosticSet()
	lexer := NewLexer(file, diagnostics)
	var tokens []TokenKind
	for {
		token := lexer.Next()
		tokens = append(tokens, token.Kind)
		if token.Kind == EOF {
			return tokens, diagnostics
		}
		if len(tokens) > len(text)+16 {
			t.Fatal(fmt.Sprintf("lexer did not terminate: %v", tokens))
		}
	}
}

func renderDiagnostics(t *testing.T, sources *source.FileSet, diagnostics *diagnostic.DiagnosticSet) string {
	t.Helper()
	var output bytes.Buffer
	if err := diagnostic.RenderText(&output, sources, diagnostics.Items()); err != nil {
		t.Fatal(err)
	}
	return output.String()
}
