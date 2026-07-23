package syntax

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

func FuzzParseFile(f *testing.F) {
	for _, seed := range [][]byte{
		nil,
		[]byte("fn main() int { return 0; }"),
		[]byte("fn broken( int { let value = ; }"),
		[]byte("type Pair[T] = struct { left, right T; };"),
		[]byte("\xff\xfe"),
	} {
		f.Add(seed)
	}
	f.Fuzz(func(t *testing.T, contents []byte) {
		first, firstDiagnostics, firstFile := fuzzParse(contents)
		if first == nil {
			return // FileSet rejects invalid UTF-8 before parsing.
		}
		second, secondDiagnostics, _ := fuzzParse(contents)
		if first.DumpString() != second.DumpString() || !sameDiagnostics(firstDiagnostics.Items(), secondDiagnostics.Items()) {
			t.Fatal("parser result is not deterministic")
		}
		validateTreeSpans(t, first, first.Root(), firstFile)
	})
}

func fuzzParse(contents []byte) (*Tree, *diagnostic.DiagnosticSet, *source.File) {
	sources := source.NewFileSet()
	id, err := sources.Add("fuzz.peb", contents)
	if err != nil {
		return nil, nil, nil
	}
	file, _ := sources.File(id)
	diagnostics := diagnostic.NewDiagnosticSet()
	return Parse(file, diagnostics), diagnostics, file
}

func validateTreeSpans(t *testing.T, tree *Tree, id NodeID, file *source.File) {
	t.Helper()
	node, ok := tree.Node(id)
	if !ok {
		t.Fatalf("invalid node ID %d", id)
	}
	span := node.Span()
	if span.Source != file.ID() || span.Start > span.End || span.End > file.Len() {
		t.Fatalf("%s has invalid span %+v for file length %d", node.Kind(), span, file.Len())
	}
	for _, child := range node.Children() {
		validateTreeSpans(t, tree, child, file)
	}
}
