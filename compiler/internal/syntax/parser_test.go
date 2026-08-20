package syntax

import (
	"errors"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

func TestParserFragmentCorpus(t *testing.T) {
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("could not locate parser test source")
	}
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(filename), "..", "..", ".."))
	corpusRoot := filepath.Join(repoRoot, "tests", "parser")

	for _, fragment := range []struct {
		name  string
		parse func(*source.File, *diagnostic.DiagnosticSet) *Tree
	}{
		{name: "expression", parse: parseExpressionFragment},
		{name: "type", parse: parseTypeFragment},
	} {
		fragment := fragment
		for _, expectation := range []struct {
			directory string
			valid     bool
		}{
			{directory: "valid", valid: true},
			{directory: "invalid", valid: false},
		} {
			caseRoot := filepath.Join(corpusRoot, fragment.name, expectation.directory)
			paths, err := collectPebbleFiles(caseRoot)
			if err != nil {
				t.Fatal(err)
			}
			if len(paths) == 0 {
				t.Fatalf("no %s parser cases found in %s", fragment.name, expectation.directory)
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
					tree, diagnostics, sources := parseFragmentText(t, filepath.ToSlash(path), string(contents), fragment.parse)
					if tree == nil || tree.Root() == 0 {
						t.Fatal("parser returned no fragment root")
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

					second, secondDiagnostics, _ := parseFragmentText(t, filepath.ToSlash(path), string(contents), fragment.parse)
					if tree.DumpString() != second.DumpString() {
						t.Fatalf("tree dump is not deterministic:\nfirst:\n%s\nsecond:\n%s", tree.DumpString(), second.DumpString())
					}
					if !sameDiagnostics(diagnostics.Items(), secondDiagnostics.Items()) {
						t.Fatal("diagnostics are not deterministic")
					}
				})
			}
		}
	}
}

func TestParserSliceFromExpression(t *testing.T) {
	tree, diagnostics, sources := parseFragmentText(t, "slice.peb", "slice ptr, count", parseExpressionFragment)
	if diagnostics.Len() != 0 {
		t.Fatalf("unexpected diagnostics: %v", diagnostics.Items())
	}
	root, ok := tree.Node(tree.Root())
	if !ok || root.Kind() != SliceFromExpr || len(root.Children()) != 2 {
		t.Fatalf("root = %v with %d children, want SliceFromExpr with 2", root.Kind(), len(root.Children()))
	}
	for i, want := range []string{"ptr", "count"} {
		child, _ := tree.Node(root.Children()[i])
		if child.Kind() != Name {
			t.Fatalf("child %d = %s, want Name", i, child.Kind())
		}
		file, _ := sources.File(child.Span().Source)
		if string(file.Slice(child.Span())) != want {
			t.Fatalf("child %d spelling = %q, want %q", i, file.Slice(child.Span()), want)
		}
	}
}

func TestExpressionPrecedenceAndAssociativity(t *testing.T) {
	tree, diagnostics, _ := parseFragmentText(t, "precedence.peb", "a + b * c as T - d - e", parseExpressionFragment)
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics: %+v", diagnostics.Items())
	}
	root := mustNode(t, tree, tree.Root())
	if root.Kind() != BinaryExpr || root.Token() != Minus {
		t.Fatalf("root = %s/%s, want left-associated subtraction", root.Kind(), root.Token())
	}
	left := mustNode(t, tree, root.Children()[0])
	if left.Kind() != BinaryExpr || left.Token() != Minus {
		t.Fatalf("left child = %s/%s, want preceding subtraction", left.Kind(), left.Token())
	}
	addition := mustNode(t, tree, left.Children()[0])
	product := mustNode(t, tree, addition.Children()[1])
	cast := mustNode(t, tree, product.Children()[1])
	if addition.Token() != Plus || product.Token() != Star || cast.Kind() != CastExpr {
		t.Fatalf("precedence shape is wrong:\n%s", tree.DumpString())
	}
}

func TestEveryExpressionPrecedenceBoundary(t *testing.T) {
	boundaries := []struct {
		lower  string
		higher string
	}{
		{lower: "||", higher: "&&"},
		{lower: "&&", higher: "|"},
		{lower: "|", higher: "^"},
		{lower: "^", higher: "&"},
		{lower: "&", higher: "=="},
		{lower: "==", higher: "<"},
		{lower: "<", higher: "<<"},
		{lower: "<<", higher: "+"},
		{lower: "+", higher: "*"},
	}
	for _, boundary := range boundaries {
		text := "a " + boundary.lower + " b " + boundary.higher + " c"
		t.Run(boundary.lower+"_before_"+boundary.higher, func(t *testing.T) {
			tree, diagnostics, _ := parseFragmentText(t, "boundary.peb", text, parseExpressionFragment)
			if diagnostics.HasErrors() {
				t.Fatalf("unexpected diagnostics: %+v", diagnostics.Items())
			}
			root := mustNode(t, tree, tree.Root())
			right := mustNode(t, tree, root.Children()[1])
			if root.Token().String() != boundary.lower || right.Token().String() != boundary.higher {
				t.Fatalf("wrong precedence shape:\n%s", tree.DumpString())
			}
		})
	}

	tree, diagnostics, _ := parseFragmentText(t, "cast-boundary.peb", "a * b as T", parseExpressionFragment)
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics: %+v", diagnostics.Items())
	}
	root := mustNode(t, tree, tree.Root())
	if root.Token() != Star || mustNode(t, tree, root.Children()[1]).Kind() != CastExpr {
		t.Fatalf("cast did not bind more tightly than multiplication:\n%s", tree.DumpString())
	}
}

func TestEveryLeftAssociativeBinaryLevel(t *testing.T) {
	for _, operator := range []string{"||", "&&", "|", "^", "&", "<<", "+", "-", "*", "/", "%"} {
		t.Run(operator, func(t *testing.T) {
			tree, diagnostics, _ := parseFragmentText(t, "associativity.peb", "a "+operator+" b "+operator+" c", parseExpressionFragment)
			if diagnostics.HasErrors() {
				t.Fatalf("unexpected diagnostics: %+v", diagnostics.Items())
			}
			root := mustNode(t, tree, tree.Root())
			left := mustNode(t, tree, root.Children()[0])
			if root.Kind() != BinaryExpr || left.Kind() != BinaryExpr || root.Token() != left.Token() {
				t.Fatalf("operator is not left associative:\n%s", tree.DumpString())
			}
		})
	}
}

func TestNonAssociativeOperatorsRejectChains(t *testing.T) {
	for _, text := range []string{"a == b != c", "a < b >= c"} {
		tree, diagnostics, _ := parseFragmentText(t, "non-associative.peb", text, parseExpressionFragment)
		if len(diagnostics.Items()) != 1 || diagnostics.Items()[0].Code != codeInvalidSyntax {
			t.Fatalf("%q diagnostics = %+v, want one P0006", text, diagnostics.Items())
		}
		if mustNode(t, tree, tree.Root()).Kind() != Error {
			t.Fatalf("%q did not preserve damaged chain in Error node:\n%s", text, tree.DumpString())
		}
	}
}

func TestNeutralBracketApplicationAndSliceShape(t *testing.T) {
	for _, text := range []string{"identity[int](x)", "functions[i](x)"} {
		tree, diagnostics, _ := parseFragmentText(t, "bracket.peb", text, parseExpressionFragment)
		if diagnostics.HasErrors() {
			t.Fatalf("%s: unexpected diagnostics: %+v", text, diagnostics.Items())
		}
		call := mustNode(t, tree, tree.Root())
		bracket := mustNode(t, tree, call.Children()[0])
		if call.Kind() != CallExpr || bracket.Kind() != BracketApply {
			t.Fatalf("%s: want CallExpr(BracketApply), got:\n%s", text, tree.DumpString())
		}
	}

	for text, wantFlags := range map[string]uint32{
		"values[:]":         0,
		"values[start:]":    1,
		"values[:end]":      2,
		"values[start:end]": 3,
	} {
		tree, diagnostics, _ := parseFragmentText(t, "slice.peb", text, parseExpressionFragment)
		if diagnostics.HasErrors() {
			t.Fatalf("%s: unexpected diagnostics: %+v", text, diagnostics.Items())
		}
		root := mustNode(t, tree, tree.Root())
		if root.Kind() != SliceExpr || root.Data() != wantFlags {
			t.Fatalf("%s: slice root = %s data=%d, want SliceExpr data=%d", text, root.Kind(), root.Data(), wantFlags)
		}
	}
}

func TestParserRecoveryPreservesFollowingListElements(t *testing.T) {
	tree, diagnostics, _ := parseFragmentText(t, "recovery.peb", "call(first,,third)", parseExpressionFragment)
	if len(diagnostics.Items()) != 1 || diagnostics.Items()[0].Code != codeExpectedExpression {
		t.Fatalf("diagnostics = %+v, want one P0003", diagnostics.Items())
	}
	root := mustNode(t, tree, tree.Root())
	if root.Kind() != CallExpr || len(root.Children()) != 4 {
		t.Fatalf("recovered call did not preserve arguments:\n%s", tree.DumpString())
	}
	if mustNode(t, tree, root.Children()[2]).Kind() != Missing || mustNode(t, tree, root.Children()[3]).Kind() != Name {
		t.Fatalf("missing and surviving argument have wrong shape:\n%s", tree.DumpString())
	}
}

func TestParserRecoveryConsumesDamagedListElementLocally(t *testing.T) {
	tree, diagnostics, _ := parseFragmentText(t, "recovery.peb", "call(first unexpected, third)", parseExpressionFragment)
	if len(diagnostics.Items()) != 1 || diagnostics.Items()[0].Code != codeInvalidSyntax {
		t.Fatalf("diagnostics = %+v, want one P0006", diagnostics.Items())
	}
	root := mustNode(t, tree, tree.Root())
	if root.Kind() != CallExpr || len(root.Children()) != 4 {
		t.Fatalf("local recovery lost a following argument:\n%s", tree.DumpString())
	}
	if mustNode(t, tree, root.Children()[2]).Kind() != Error || mustNode(t, tree, root.Children()[3]).Kind() != Name {
		t.Fatalf("damaged and surviving arguments have wrong shape:\n%s", tree.DumpString())
	}
}

func TestParserRecoveryStopsBeforeFollowingStatement(t *testing.T) {
	text := `type Point = struct {
    x int;
    y int;
};

fn add(p Point, scale int) Point {
    return Point.{ x = p.x + scale, y = p.y + scale };
}

fn main() int {
    var origin Point = Point.{ x = 0, y = 0 };
    var broken int = add(origin,
    return origin.x as int;
}`
	tree, diagnostics, sources := parseFragmentText(t, "recovery.peb", text, Parse)
	file, _ := sources.File(tree.source)

	foundCallDiagnostic := false
	for _, item := range diagnostics.Items() {
		if item.Code == codeInvalidSyntax && strings.Contains(item.Message, "expected ',' or ')' after call argument") {
			foundCallDiagnostic = true
		}
	}
	if !foundCallDiagnostic {
		t.Fatalf("missing call-argument diagnostic: %+v\n%s", diagnostics.Items(), tree.DumpString())
	}

	returnCount, memberCount, originCount := 0, 0, 0
	for id := NodeID(1); ; id++ {
		node, ok := tree.Node(id)
		if !ok {
			break
		}
		if node.Kind() == ReturnStmt && strings.Contains(string(file.Slice(node.Span())), "return origin.x") {
			returnCount++
		}
		if node.Kind() == MemberExpr && strings.Contains(string(file.Slice(node.Span())), "origin.x") {
			memberCount++
		}
		if node.Kind() == Name && string(file.Slice(node.Span())) == "origin" {
			originCount++
		}
	}
	if returnCount != 1 || memberCount != 1 || originCount != 3 {
		t.Fatalf("following statement was not structurally preserved: returns=%d members=%d origins=%d\n%s", returnCount, memberCount, originCount, tree.DumpString())
	}
}

func TestLexerAndParserDiagnosticsStayInSourceOrder(t *testing.T) {
	_, diagnostics, _ := parseFragmentText(t, "ordering.peb", "value extra @", parseExpressionFragment)
	items := diagnostics.Items()
	if len(items) != 2 || items[0].Code != codeInvalidSyntax || items[1].Code != codeUnsupportedCharacter {
		t.Fatalf("diagnostic order = %+v, want P0006 before later L0001", items)
	}
}

func TestInvalidLexerTokenDoesNotGetDuplicateParserDiagnostic(t *testing.T) {
	_, diagnostics, _ := parseFragmentText(t, "invalid-token.peb", "@", parseExpressionFragment)
	items := diagnostics.Items()
	if len(items) != 1 || items[0].Code != codeUnsupportedCharacter {
		t.Fatalf("diagnostics = %+v, want only lexer L0001", items)
	}
}

func TestDamagedFragmentsTerminate(t *testing.T) {
	cases := []struct {
		name  string
		text  string
		parse func(*source.File, *diagnostic.DiagnosticSet) *Tree
	}{
		{name: "empty expression", text: "", parse: parseExpressionFragment},
		{name: "delimiter noise", text: "([[[,,,)))", parse: parseExpressionFragment},
		{name: "deep expression", text: strings.Repeat("(", 300) + "x" + strings.Repeat(")", 300), parse: parseExpressionFragment},
		{name: "broken type list", text: "Map[?,, fn(]", parse: parseTypeFragment},
		{name: "deep type", text: strings.Repeat("?", 300) + "T", parse: parseTypeFragment},
	}
	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			tree, diagnostics, _ := parseFragmentText(t, "damaged.peb", test.text, test.parse)
			if tree == nil || tree.Root() == 0 || !diagnostics.HasErrors() {
				t.Fatalf("damaged fragment did not return an error tree: tree=%v diagnostics=%+v", tree, diagnostics.Items())
			}
			if diagnostics.ErrorCount() > defaultParserDiagnosticLimit+1 {
				t.Fatalf("diagnostic limit exceeded: %d", diagnostics.ErrorCount())
			}
		})
	}
}

func TestParserNestingLimitIsBounded(t *testing.T) {
	text := strings.Repeat("(", 20) + "x" + strings.Repeat(")", 20)
	sources := source.NewFileSet()
	id, err := sources.Add("nested.peb", []byte(text))
	if err != nil {
		t.Fatal(err)
	}
	file, _ := sources.File(id)
	diagnostics := diagnostic.NewDiagnosticSet()
	p := newParser(file, diagnostics)
	p.nestingMax = 8
	p.tree.root = p.parseExpression()
	p.requireEOF("after expression")
	found := false
	for _, item := range diagnostics.Items() {
		if item.Code == codeNestingLimit {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("nesting limit produced no P0007: %+v", diagnostics.Items())
	}
}

func TestTreeAccessorsDoNotExposeMutableStorage(t *testing.T) {
	tree, _, _ := parseFragmentText(t, "tree.peb", "f(x)", parseExpressionFragment)
	root := mustNode(t, tree, tree.Root())
	children := root.Children()
	children[0] = 0
	rootAgain := mustNode(t, tree, tree.Root())
	if rootAgain.Children()[0] == 0 {
		t.Fatal("mutating returned children changed the tree")
	}
}

func TestTokenCursorEOFIsStable(t *testing.T) {
	sources := source.NewFileSet()
	id, err := sources.Add("empty.peb", nil)
	if err != nil {
		t.Fatal(err)
	}
	file, _ := sources.File(id)
	diagnostics := diagnostic.NewDiagnosticSet()
	cursor := tokenCursor{lexer: NewLexer(file, diagnostics)}
	first := cursor.peek(0)
	for range 8 {
		if got := cursor.advance(); got != first {
			t.Fatalf("EOF changed: got %+v, want %+v", got, first)
		}
	}
}

func TestTreeDumpPropagatesWriterFailure(t *testing.T) {
	tree, _, _ := parseFragmentText(t, "dump.peb", "x", parseExpressionFragment)
	if err := tree.Dump(failingWriter{}); !errors.Is(err, errWriterFailed) {
		t.Fatalf("Dump error = %v, want writer failure", err)
	}
}

var errWriterFailed = errors.New("writer failed")

type failingWriter struct{}

func (failingWriter) Write([]byte) (int, error) { return 0, errWriterFailed }

func parseFragmentText(t *testing.T, path, text string, parse func(*source.File, *diagnostic.DiagnosticSet) *Tree) (*Tree, *diagnostic.DiagnosticSet, *source.FileSet) {
	t.Helper()
	sources := source.NewFileSet()
	id, err := sources.Add(path, []byte(text))
	if err != nil {
		t.Fatal(err)
	}
	file, _ := sources.File(id)
	diagnostics := diagnostic.NewDiagnosticSet()
	return parse(file, diagnostics), diagnostics, sources
}

func mustNode(t *testing.T, tree *Tree, id NodeID) Node {
	t.Helper()
	node, ok := tree.Node(id)
	if !ok {
		t.Fatalf("node %d not found", id)
	}
	return node
}

func sameDiagnostics(left, right []diagnostic.Diagnostic) bool {
	if len(left) != len(right) {
		return false
	}
	for index := range left {
		if left[index].Code != right[index].Code || left[index].Severity != right[index].Severity ||
			left[index].Message != right[index].Message || left[index].Primary.Span != right[index].Primary.Span {
			return false
		}
	}
	return true
}
