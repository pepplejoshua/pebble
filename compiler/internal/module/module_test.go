package module

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

type memoryProvider struct {
	files      map[CanonicalPath][]byte
	unreadable map[CanonicalPath]bool
	invalid    map[CanonicalPath]bool
}

func (p *memoryProvider) Canonicalize(raw string) (CanonicalPath, error) {
	canonical := CanonicalPath(path.Clean(filepath.ToSlash(raw)))
	if strings.IndexByte(raw, 0) >= 0 || p.invalid[canonical] {
		return "", &ProviderError{Kind: ProviderInvalidPath, Path: raw, Err: fs.ErrInvalid}
	}
	if _, ok := p.files[canonical]; ok || p.unreadable[canonical] {
		return canonical, nil
	}
	return "", &ProviderError{Kind: ProviderNotFound, Path: raw, Err: fs.ErrNotExist}
}

func (p *memoryProvider) ReadFile(canonical CanonicalPath) ([]byte, error) {
	if p.unreadable[canonical] {
		return nil, &ProviderError{Kind: ProviderUnreadable, Path: string(canonical), Err: fs.ErrPermission}
	}
	contents, ok := p.files[canonical]
	if !ok {
		return nil, &ProviderError{Kind: ProviderNotFound, Path: string(canonical), Err: fs.ErrNotExist}
	}
	return append([]byte(nil), contents...), nil
}

func TestModuleFixtureCorpus(t *testing.T) {
	repoRoot := testRepoRoot(t)
	fixtureRoot := filepath.Join(repoRoot, "tests", "module")

	validCases, err := os.ReadDir(filepath.Join(fixtureRoot, "valid"))
	if err != nil {
		t.Fatal(err)
	}
	for _, entry := range validCases {
		if !entry.IsDir() {
			continue
		}
		t.Run("valid/"+entry.Name(), func(t *testing.T) {
			graph, diagnostics := buildFixture(t, filepath.Join(fixtureRoot, "valid", entry.Name()), entry.Name(), "")
			if diagnostics.HasErrors() {
				t.Fatalf("valid module fixture produced diagnostics: %+v", diagnostics.Items())
			}
			if graph.Root == 0 || graph.Len() < 2 {
				t.Fatalf("graph root/size = %d/%d", graph.Root, graph.Len())
			}
			assertGraphInvariants(t, graph)
		})
	}

	invalidRoot := filepath.Join(fixtureRoot, "invalid")
	codes, err := os.ReadDir(invalidRoot)
	if err != nil {
		t.Fatal(err)
	}
	for _, codeDirectory := range codes {
		if !codeDirectory.IsDir() {
			continue
		}
		cases, err := os.ReadDir(filepath.Join(invalidRoot, codeDirectory.Name()))
		if err != nil {
			t.Fatal(err)
		}
		for _, entry := range cases {
			if !entry.IsDir() {
				continue
			}
			name := codeDirectory.Name() + "/" + entry.Name()
			t.Run("invalid/"+name, func(t *testing.T) {
				_, diagnostics := buildFixture(t, filepath.Join(invalidRoot, name), entry.Name(), codeDirectory.Name())
				items := moduleErrors(diagnostics.Items())
				if len(items) == 0 {
					t.Fatal("invalid module fixture produced no module diagnostic")
				}
				for _, item := range items {
					if string(item.Code) != codeDirectory.Name() {
						t.Fatalf("module diagnostic = %s, want %s: %+v", item.Code, codeDirectory.Name(), items)
					}
				}
			})
		}
	}
}

func TestRecoveryFixtureInspectsIndependentImports(t *testing.T) {
	root := filepath.Join(testRepoRoot(t), "tests", "module", "recovery", "damaged_dependency")
	graph, diagnostics := buildFixture(t, root, "damaged_dependency", "")
	if graph.Len() != 3 {
		t.Fatalf("loaded modules = %d, want 3", graph.Len())
	}
	if !diagnostics.HasErrors() || len(moduleErrors(diagnostics.Items())) != 0 {
		t.Fatalf("want parser recovery diagnostics only, got %+v", diagnostics.Items())
	}
}

func TestRequiredFixtureShapes(t *testing.T) {
	required := []string{
		"valid/relative", "valid/parent_relative", "valid/standard", "valid/package_root",
		"valid/shared_dependency", "valid/diamond",
		"invalid/M0001/invalid_spelling", "invalid/M0002/missing_file", "invalid/M0002/unreadable",
		"invalid/M0003/duplicate_target", "invalid/M0004/qualifier_collision",
		"invalid/M0005/self_cycle", "invalid/M0005/multi_module_cycle",
		"invalid/M0006/ambiguous_roots", "invalid/M0007/depth_limit", "invalid/M0007/module_limit",
		"recovery/damaged_dependency",
	}
	root := filepath.Join(testRepoRoot(t), "tests", "module")
	for _, relative := range required {
		if info, err := os.Stat(filepath.Join(root, filepath.FromSlash(relative))); err != nil || !info.IsDir() {
			t.Errorf("required fixture %s is missing", relative)
		}
	}
}

func TestDiamondGraphSharesOneModuleAndIsDeterministic(t *testing.T) {
	fixture := filepath.Join(testRepoRoot(t), "tests", "module", "valid", "diamond")
	first, firstDiagnostics := buildFixture(t, fixture, "diamond", "")
	second, secondDiagnostics := buildFixture(t, fixture, "diamond", "")
	if first.Len() != 4 {
		t.Fatalf("diamond modules = %d, want 4", first.Len())
	}
	if got, want := graphSnapshot(first), graphSnapshot(second); got != want {
		t.Fatalf("graph is not deterministic:\nfirst:\n%s\nsecond:\n%s", got, want)
	}
	if !reflect.DeepEqual(firstDiagnostics.Items(), secondDiagnostics.Items()) {
		t.Fatal("diagnostics are not deterministic")
	}
	order := first.DependencyOrder()
	if len(order) != 4 || order[len(order)-1] != first.Root {
		t.Fatalf("dependency order = %v", order)
	}
}

func TestGraphAccessorsDoNotExposeImportStorage(t *testing.T) {
	fixture := filepath.Join(testRepoRoot(t), "tests", "module", "valid", "relative")
	graph, _ := buildFixture(t, fixture, "relative", "")
	root, _ := graph.Module(graph.Root)
	if len(root.Imports) == 0 {
		t.Fatal("fixture has no imports")
	}
	root.Imports[0].Qualifier = "changed"
	again, _ := graph.Module(graph.Root)
	if again.Imports[0].Qualifier == "changed" {
		t.Fatal("Module exposed mutable import storage")
	}
	modules := graph.Modules()
	modules[0].Imports[0].Spelling = "changed"
	again, _ = graph.Module(graph.Root)
	if again.Imports[0].Spelling == "changed" {
		t.Fatal("Modules exposed mutable import storage")
	}
}

func TestCycleDiagnosticLabelsEveryAuthoredEdge(t *testing.T) {
	fixture := filepath.Join(testRepoRoot(t), "tests", "module", "invalid", "M0005", "multi_module_cycle")
	_, diagnostics := buildFixture(t, fixture, "multi_module_cycle", "M0005")
	items := moduleErrors(diagnostics.Items())
	if len(items) != 1 || items[0].Code != CodeModuleCycle {
		t.Fatalf("cycle diagnostics = %+v", items)
	}
	if len(items[0].Related) != 2 { // Three-edge cycle: two related labels plus the primary.
		t.Fatalf("cycle related labels = %d, want 2", len(items[0].Related))
	}
	if len(items[0].Notes) != 1 || !strings.Contains(items[0].Notes[0], "import chain") {
		t.Fatalf("cycle notes = %v", items[0].Notes)
	}
}

func TestImportSpellingContract(t *testing.T) {
	valid := []string{"./math", "../shared/math", "std:mem", "std:mem/arena", "collections/map", "./dir/./math"}
	invalid := []string{"", "/absolute", `bad\\path`, "trailing/", "empty//part", "module.peb", "std:", "std:../mem", "package/../mem", "./..", "../shared/..", "C:/module"}
	for _, spelling := range valid {
		if _, ok := validateImportSpelling(spelling); !ok {
			t.Errorf("valid spelling rejected: %q", spelling)
		}
	}
	for _, spelling := range invalid {
		if _, ok := validateImportSpelling(spelling); ok {
			t.Errorf("invalid spelling accepted: %q", spelling)
		}
	}
}

func TestModuleDiagnosticLimitIsBounded(t *testing.T) {
	provider := &memoryProvider{files: map[CanonicalPath][]byte{
		"main.peb": []byte("import \"./one\"; import \"./two\";"),
	}}
	diagnostics := diagnostic.NewDiagnosticSet()
	build(BuildConfig{EntryPath: "main.peb", Package: "app"}, provider, source.NewFileSet(), diagnostics, 1)
	if got := len(moduleErrors(diagnostics.Items())); got != 1 {
		t.Fatalf("module diagnostics = %d, want bounded at 1", got)
	}
}

func TestImportDepthUsesDeterministicShortestDiscoveryPath(t *testing.T) {
	provider := &memoryProvider{files: map[CanonicalPath][]byte{
		"main.peb":   []byte("import \"./deep\"; import \"./shared\";"),
		"deep.peb":   []byte("import \"./shared\";"),
		"shared.peb": []byte("import \"./leaf\";"),
		"leaf.peb":   []byte("fn leaf() int => 0;"),
	}}
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := Build(BuildConfig{EntryPath: "main.peb", Package: "app", MaxImportDepth: 2}, provider, source.NewFileSet(), diagnostics)
	if diagnostics.HasErrors() || graph.Len() != 4 {
		t.Fatalf("shortest-path graph = %d modules, diagnostics %+v", graph.Len(), diagnostics.Items())
	}
}

func TestFileSystemProviderCanonicalizesAndReads(t *testing.T) {
	directory := t.TempDir()
	filename := filepath.Join(directory, "module.peb")
	if err := os.WriteFile(filename, []byte("fn main() int => 0;"), 0o600); err != nil {
		t.Fatal(err)
	}
	provider := FileSystemProvider{}
	canonical, err := provider.Canonicalize(filename)
	if err != nil {
		t.Fatal(err)
	}
	contents, err := provider.ReadFile(canonical)
	if err != nil || string(contents) != "fn main() int => 0;" {
		t.Fatalf("read = %q, %v", contents, err)
	}
	_, err = provider.Canonicalize(filepath.Join(directory, "missing.peb"))
	if !errors.Is(err, fs.ErrNotExist) || providerFailure(err) != ProviderNotFound {
		t.Fatalf("missing classification = %v", err)
	}
}

func buildFixture(t *testing.T, root, caseName, expectedCode string) (*Graph, *diagnostic.DiagnosticSet) {
	t.Helper()
	provider := &memoryProvider{files: make(map[CanonicalPath][]byte), unreadable: make(map[CanonicalPath]bool), invalid: make(map[CanonicalPath]bool)}
	err := filepath.WalkDir(root, func(filename string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() || filepath.Ext(filename) != ".peb" {
			return nil
		}
		relative, err := filepath.Rel(root, filename)
		if err != nil {
			return err
		}
		contents, err := os.ReadFile(filename)
		if err != nil {
			return err
		}
		provider.files[CanonicalPath(filepath.ToSlash(relative))] = contents
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if caseName == "unreadable" {
		delete(provider.files, "blocked.peb")
		provider.unreadable["blocked.peb"] = true
	}
	if caseName == "invalid_provider_path" {
		provider.invalid["bad.peb"] = true
	}
	config := BuildConfig{
		EntryPath: "main.peb", Package: "app", StandardRoot: "std",
		SearchRoots: []SearchRoot{{Package: "package-a", Path: "roots/a"}, {Package: "package-b", Path: "roots/b"}},
	}
	switch caseName {
	case "depth_limit":
		config.MaxImportDepth = 1
	case "module_limit":
		config.MaxModules = 2
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := Build(config, provider, source.NewFileSet(), diagnostics)
	if expectedCode != "" && len(moduleErrors(diagnostics.Items())) == 0 {
		t.Fatalf("fixture expected %s but got %+v", expectedCode, diagnostics.Items())
	}
	return graph, diagnostics
}

func assertGraphInvariants(t *testing.T, graph *Graph) {
	t.Helper()
	for index, module := range graph.Modules() {
		if module.ID != ModuleID(index+1) || module.Tree == nil {
			t.Fatalf("invalid module at index %d: %+v", index, module)
		}
		if found, ok := graph.Lookup(module.Key); !ok || found != module.ID {
			t.Fatalf("key lookup failed for module %d", module.ID)
		}
		for _, edge := range module.Imports {
			if edge.Target == 0 || edge.Span.Source != module.Source || edge.Qualifier == "" {
				t.Fatalf("invalid edge in module %d: %+v", module.ID, edge)
			}
		}
	}
}

func graphSnapshot(graph *Graph) string {
	var result strings.Builder
	for _, module := range graph.Modules() {
		fmt.Fprintf(&result, "%d %s %s\n", module.ID, module.Key.Package, module.Key.Path)
		for _, edge := range module.Imports {
			fmt.Fprintf(&result, "  %s %s -> %d [%d,%d)\n", edge.Spelling, edge.Qualifier, edge.Target, edge.Span.Start, edge.Span.End)
		}
	}
	fmt.Fprintf(&result, "order %v", graph.DependencyOrder())
	return result.String()
}

func moduleErrors(items []diagnostic.Diagnostic) []diagnostic.Diagnostic {
	var result []diagnostic.Diagnostic
	for _, item := range items {
		if item.Severity == diagnostic.Error && strings.HasPrefix(string(item.Code), "M") {
			result = append(result, item)
		}
	}
	return result
}

func testRepoRoot(t *testing.T) string {
	t.Helper()
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("could not locate module test source")
	}
	return filepath.Clean(filepath.Join(filepath.Dir(filename), "..", "..", ".."))
}
