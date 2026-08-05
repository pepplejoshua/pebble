package syntax

import (
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"testing"
)

func TestParserRepositoryCorpus(t *testing.T) {
	_, filename, _, _ := runtime.Caller(0)
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(filename), "..", "..", ".."))
	var paths []string
	for _, directory := range []string{filepath.Join("compiler", "std"), "examples"} {
		found, err := collectPebbleFiles(filepath.Join(repoRoot, directory))
		if err != nil {
			t.Fatal(err)
		}
		paths = append(paths, found...)
	}
	if len(paths) == 0 {
		t.Fatal("no standard-library or example files found")
	}
	slices.Sort(paths)
	for _, path := range paths {
		path := path
		relativePath, _ := filepath.Rel(repoRoot, path)
		t.Run(filepath.ToSlash(relativePath), func(t *testing.T) {
			contents, err := os.ReadFile(path)
			if err != nil {
				t.Fatal(err)
			}
			_, diagnostics, sources := parseFileText(t, filepath.ToSlash(relativePath), string(contents))
			if diagnostics.HasErrors() {
				t.Fatalf("repository source produced parser diagnostics:\n%s", renderDiagnostics(t, sources, diagnostics))
			}
		})
	}
}
