package syntax

import (
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

func TestParserFileCorpus(t *testing.T) {
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("could not locate parser file test source")
	}
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(filename), "..", "..", ".."))
	corpusRoot := filepath.Join(repoRoot, "tests", "parser", "file")

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
			t.Fatalf("no file parser cases found in %s", expectation.directory)
		}
		slices.Sort(paths)
		for _, path := range paths {
			path := path
			relativePath, err := filepath.Rel(corpusRoot, path)
			if err != nil {
				t.Fatal(err)
			}
			t.Run(filepath.ToSlash(relativePath), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				tree, diagnostics, sources := parseFileText(t, filepath.ToSlash(path), string(contents))
				root := mustNode(t, tree, tree.Root())
				if root.Kind() != File {
					t.Fatalf("root = %s, want File", root.Kind())
				}
				rootChildren := root.Children()
				if len(rootChildren) == 0 || mustNode(t, tree, rootChildren[len(rootChildren)-1]).Kind() != EndOfFile {
					t.Fatalf("File root does not end with EndOfFile:\n%s", tree.DumpString())
				}
				if root.Span().Start != 0 || root.Span().End != uint32(len(contents)) {
					t.Fatalf("file span = %+v, want [0,%d)", root.Span(), len(contents))
				}
				file, _ := sources.File(root.Span().Source)
				validateTreeSpans(t, tree, tree.Root(), file)
				if expectation.valid && diagnostics.HasErrors() {
					t.Fatalf("valid file produced diagnostics:\n%s", renderDiagnostics(t, sources, diagnostics))
				}
				if !expectation.valid && !diagnostics.HasErrors() {
					t.Fatal("invalid file produced no diagnostics")
				}
				if !expectation.valid {
					expectedCode := diagnostic.Code(filepath.Base(filepath.Dir(path)))
					for _, item := range diagnostics.Items() {
						if item.Severity == diagnostic.Error && item.Code != expectedCode {
							t.Fatalf("error code = %s, want %s:\n%s", item.Code, expectedCode, renderDiagnostics(t, sources, diagnostics))
						}
					}
				}
				second, secondDiagnostics, _ := parseFileText(t, filepath.ToSlash(path), string(contents))
				if tree.DumpString() != second.DumpString() || !sameDiagnostics(diagnostics.Items(), secondDiagnostics.Items()) {
					t.Fatal("complete-file parsing is not deterministic")
				}
			})
		}
	}
}

func TestParserRecoveryCorpus(t *testing.T) {
	_, filename, _, _ := runtime.Caller(0)
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(filename), "..", "..", ".."))
	caseRoot := filepath.Join(repoRoot, "tests", "parser", "recovery")
	paths, err := collectPebbleFiles(caseRoot)
	if err != nil {
		t.Fatal(err)
	}
	if len(paths) == 0 {
		t.Fatal("no parser recovery cases found")
	}
	for _, path := range paths {
		path := path
		t.Run(filepath.Base(path), func(t *testing.T) {
			contents, err := os.ReadFile(path)
			if err != nil {
				t.Fatal(err)
			}
			tree, diagnostics, _ := parseFileText(t, filepath.ToSlash(path), string(contents))
			if !diagnostics.HasErrors() {
				t.Fatal("recovery case produced no diagnostics")
			}
			if countKind(tree, tree.Root(), FunctionDecl) == 0 || countKind(tree, tree.Root(), ReturnStmt) == 0 {
				t.Fatalf("recovery lost the function or its later return statement:\n%s", tree.DumpString())
			}
			second, secondDiagnostics, _ := parseFileText(t, filepath.ToSlash(path), string(contents))
			if tree.DumpString() != second.DumpString() || !sameDiagnostics(diagnostics.Items(), secondDiagnostics.Items()) {
				t.Fatal("recovery result is not deterministic")
			}
		})
	}
}

func TestFileRecoveryPreservesLaterDeclarationsAndStatements(t *testing.T) {
	text := `
fn broken() int {
    let value = ;
    return value;
}
fn later() int {
    return 1;
}
`
	tree, diagnostics, _ := parseFileText(t, "recovery.peb", text)
	if !diagnostics.HasErrors() {
		t.Fatal("damaged file produced no diagnostics")
	}
	if got := countKind(tree, tree.Root(), FunctionDecl); got != 2 {
		t.Fatalf("function declarations = %d, want 2:\n%s", got, tree.DumpString())
	}
	if got := countKind(tree, tree.Root(), ReturnStmt); got != 2 {
		t.Fatalf("return statements = %d, want 2:\n%s", got, tree.DumpString())
	}
}

func TestAggregateAndSwitchRecoveryKeepLaterSyntax(t *testing.T) {
	text := `
type Broken = struct {
    value int
    fn get(self *Broken) int => self.value;
};
type Later = int;
fn choose(value int) int {
    switch value {
        damaged tokens;
        case 1: return 1;
        else: return 2;
    }
}
`
	tree, diagnostics, _ := parseFileText(t, "structured-recovery.peb", text)
	if !diagnostics.HasErrors() {
		t.Fatal("damaged file produced no diagnostics")
	}
	for kind, want := range map[NodeKind]int{
		TypeDecl: 2, FunctionDecl: 2, SwitchCase: 2, ReturnStmt: 2,
	} {
		if got := countKind(tree, tree.Root(), kind); got != want {
			t.Fatalf("%s count = %d, want %d:\n%s", kind, got, want, tree.DumpString())
		}
	}
}

func TestCompleteFileTreeContainsBodiesAndLiterals(t *testing.T) {
	text := `
type Point = struct { x int; fn get(self *Point) int => self.x; };
fn make() Point {
    let build = fn(value int) int => value;
    return Point.{ x = build(1) };
}
`
	tree, diagnostics, _ := parseFileText(t, "structure.peb", text)
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics: %+v\n%s", diagnostics.Items(), tree.DumpString())
	}
	for kind, want := range map[NodeKind]int{
		File: 1, EndOfFile: 1, TypeDecl: 1, StructType: 1, FunctionDecl: 2,
		FunctionTerm: 1, RecordExpr: 1, RecordField: 1,
	} {
		if got := countKind(tree, tree.Root(), kind); got != want {
			t.Fatalf("%s count = %d, want %d:\n%s", kind, got, want, tree.DumpString())
		}
	}
}

func TestParseAcceptsNilDiagnosticSet(t *testing.T) {
	sources := source.NewFileSet()
	id, err := sources.Add("empty.peb", nil)
	if err != nil {
		t.Fatal(err)
	}
	file, _ := sources.File(id)
	tree := Parse(file, nil)
	if mustNode(t, tree, tree.Root()).Kind() != File {
		t.Fatal("Parse did not return a File root")
	}
}

func parseFileText(t *testing.T, path, text string) (*Tree, *diagnostic.DiagnosticSet, *source.FileSet) {
	t.Helper()
	sources := source.NewFileSet()
	id, err := sources.Add(path, []byte(text))
	if err != nil {
		t.Fatal(err)
	}
	file, _ := sources.File(id)
	diagnostics := diagnostic.NewDiagnosticSet()
	return Parse(file, diagnostics), diagnostics, sources
}

func countKind(tree *Tree, root NodeID, kind NodeKind) int {
	node, ok := tree.Node(root)
	if !ok {
		return 0
	}
	count := 0
	if node.Kind() == kind {
		count++
	}
	for _, child := range node.Children() {
		count += countKind(tree, child, kind)
	}
	return count
}
