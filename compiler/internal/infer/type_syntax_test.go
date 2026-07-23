package infer

import (
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type inferenceMemoryProvider map[module.CanonicalPath][]byte

type fixedArrayLengths struct{}

func (fixedArrayLengths) ArrayLength(symbol.SyntaxRef) ArrayLengthResult {
	return ArrayLengthResult{State: ArrayLengthKnown, Value: 4}
}

func (p inferenceMemoryProvider) Canonicalize(raw string) (module.CanonicalPath, error) {
	canonical := module.CanonicalPath(path.Clean(filepath.ToSlash(raw)))
	if _, ok := p[canonical]; !ok {
		return "", &module.ProviderError{Kind: module.ProviderNotFound, Path: raw, Err: fs.ErrNotExist}
	}
	return canonical, nil
}
func (p inferenceMemoryProvider) ReadFile(name module.CanonicalPath) ([]byte, error) {
	value, ok := p[name]
	if !ok {
		return nil, fs.ErrNotExist
	}
	return append([]byte(nil), value...), nil
}

func TestSourceDrivenTypeSyntaxFixtures(t *testing.T) {
	root := filepath.Join(inferenceRepoRoot(t), "tests", "types")
	valid, err := os.ReadDir(filepath.Join(root, "valid"))
	if err != nil {
		t.Fatal(err)
	}
	for _, entry := range valid {
		if entry.IsDir() || filepath.Ext(entry.Name()) != ".peb" {
			continue
		}
		t.Run("valid/"+entry.Name(), func(t *testing.T) {
			program, diagnostics := prepareFixture(t, filepath.Join(root, "valid", entry.Name()))
			if diagnostics.HasErrors() {
				t.Fatalf("valid fixture diagnostics: %+v", diagnostics.Items())
			}
			for _, declaration := range program.TypeDeclarations() {
				if declaration.State != DeclarationReady || declaration.Template == 0 {
					t.Fatalf("declaration not ready: %+v", declaration)
				}
			}
		})
	}
	invalidRoot := filepath.Join(root, "invalid")
	codes, err := os.ReadDir(invalidRoot)
	if err != nil {
		t.Fatal(err)
	}
	for _, codeDir := range codes {
		entries, err := os.ReadDir(filepath.Join(invalidRoot, codeDir.Name()))
		if err != nil {
			t.Fatal(err)
		}
		for _, entry := range entries {
			if entry.IsDir() || filepath.Ext(entry.Name()) != ".peb" {
				continue
			}
			t.Run("invalid/"+codeDir.Name()+"/"+entry.Name(), func(t *testing.T) {
				_, diagnostics := prepareFixture(t, filepath.Join(invalidRoot, codeDir.Name(), entry.Name()))
				if !hasDiagnostic(diagnostics, diagnostic.Code(codeDir.Name())) {
					t.Fatalf("want %s, got %+v", codeDir.Name(), diagnostics.Items())
				}
			})
		}
	}
}

func TestTypePreparationRecoversAfterInvalidDeclaration(t *testing.T) {
	program, diagnostics := prepareSource(t, []byte(`
fn invalid(value struct { field int; }) void {}
type Later = struct { value int; };
`))
	if !hasDiagnostic(diagnostics, CodeAnonymousAggregate) {
		t.Fatalf("missing anonymous aggregate diagnostic: %+v", diagnostics.Items())
	}
	for _, declaration := range program.TypeDeclarations() {
		symbolValue, _ := program.inputs.Resolution.Symbols.Symbol(declaration.Symbol)
		if symbolValue.Name == "Later" {
			if declaration.State != DeclarationReady || declaration.Template == 0 {
				t.Fatalf("later declaration did not recover: %+v", declaration)
			}
			return
		}
	}
	t.Fatal("missing later declaration")
}

func prepareFixture(t *testing.T, filename string) (*Program, *diagnostic.DiagnosticSet) {
	t.Helper()
	contents, err := os.ReadFile(filename)
	if err != nil {
		t.Fatal(err)
	}
	return prepareSource(t, contents)
}

func prepareSource(t *testing.T, contents []byte) (*Program, *diagnostic.DiagnosticSet) {
	t.Helper()
	provider := inferenceMemoryProvider{"main.peb": contents}
	diagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "types"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	program := Prepare(ProgramInputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, ArrayLengths: fixedArrayLengths{}, LiteralTarget: LiteralTarget{WordBits: 64}}, diagnostics, Config{})
	return program, diagnostics
}

func inferenceRepoRoot(t *testing.T) string {
	t.Helper()
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("cannot locate repository")
	}
	root := filepath.Clean(filepath.Join(filepath.Dir(filename), "..", "..", ".."))
	if !strings.HasSuffix(filepath.ToSlash(root), "/pebble") {
		t.Fatalf("unexpected repository root %s", root)
	}
	return root
}
