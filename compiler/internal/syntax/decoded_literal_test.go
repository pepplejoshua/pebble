package syntax

import (
	"bytes"
	"fmt"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

func TestLexerDecodesEveryCommonEscape(t *testing.T) {
	tests := []struct {
		name      string
		spelling  string
		value     rune
		byteValue bool
	}{
		{name: "backslash", spelling: `\\`, value: '\\'},
		{name: "double quote", spelling: `\"`, value: '"'},
		{name: "single quote", spelling: `\'`, value: '\''},
		{name: "line feed", spelling: `\n`, value: '\n'},
		{name: "carriage return", spelling: `\r`, value: '\r'},
		{name: "horizontal tab", spelling: `\t`, value: '\t'},
		{name: "zero byte", spelling: `\0`, value: 0},
		{name: "byte zero", spelling: `\x00`, value: 0, byteValue: true},
		{name: "byte high boundary", spelling: `\xFF`, value: 0xff, byteValue: true},
		{name: "Unicode null", spelling: `\u{0}`, value: 0},
		{name: "before surrogate range", spelling: `\u{D7FF}`, value: 0xd7ff},
		{name: "after surrogate range", spelling: `\u{E000}`, value: 0xe000},
		{name: "maximum scalar", spelling: `\u{10FFFF}`, value: 0x10ffff},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			stringToken := singleLiteralToken(t, `"`+test.spelling+`"`, StringLiteral)
			if stringToken.decoded.kind != DecodedString {
				t.Fatalf("string decoded kind = %d, want %d", stringToken.decoded.kind, DecodedString)
			}
			wantText := string(test.value)
			if test.byteValue {
				wantText = string([]byte{byte(test.value)})
			}
			if stringToken.decoded.text != wantText {
				t.Fatalf("string decoded bytes = % x, want % x", []byte(stringToken.decoded.text), []byte(wantText))
			}

			characterToken := singleLiteralToken(t, `'`+test.spelling+`'`, CharacterLiteral)
			if characterToken.decoded.kind != DecodedCharacter || characterToken.decoded.rune != test.value {
				t.Fatalf("character decoded = {%d %U}, want {%d %U}", characterToken.decoded.kind, characterToken.decoded.rune, DecodedCharacter, test.value)
			}

			interpolationToken := interpolationTextToken(t, "`"+test.spelling+"`")
			if interpolationToken.decoded.kind != DecodedInterpolationText || interpolationToken.decoded.text != wantText {
				t.Fatalf("interpolation decoded = {%d % x}, want {%d % x}", interpolationToken.decoded.kind, []byte(interpolationToken.decoded.text), DecodedInterpolationText, []byte(wantText))
			}
		})
	}
}

func TestLexerDecodesInterpolationOnlyEscapes(t *testing.T) {
	for _, test := range []struct {
		spelling string
		want     string
	}{
		{spelling: `\{`, want: "{"},
		{spelling: "\\`", want: "`"},
	} {
		token := interpolationTextToken(t, "`"+test.spelling+"`")
		if token.decoded.kind != DecodedInterpolationText || token.decoded.text != test.want {
			t.Fatalf("%q decoded to {%d %q}, want {%d %q}", test.spelling, token.decoded.kind, token.decoded.text, DecodedInterpolationText, test.want)
		}
	}
}

func TestLexerEmptyStringHasPresentDecodedValue(t *testing.T) {
	token := singleLiteralToken(t, `""`, StringLiteral)
	if token.decoded.kind != DecodedString || token.decoded.text != "" {
		t.Fatalf("empty string decoded = %+v", token.decoded)
	}
}

func TestInvalidLexerTokensCarryNoDecodedValue(t *testing.T) {
	tests := []string{
		"\"line\nline\"",
		`"\q"`,
		`"\x0"`,
		`"\u"`,
		`"\u{}"`,
		`"\u{1234567}"`,
		`"\u{D800}"`,
		`"\u{DFFF}"`,
		`"\u{110000}"`,
		`''`,
		`'ab'`,
		"'a\nb'",
		`'\q'`,
		"`\\q`",
		"`unterminated",
		"`{value",
	}

	for _, text := range tests {
		t.Run(fmt.Sprintf("%q", text), func(t *testing.T) {
			tokens, diagnostics := lexTokens(t, text)
			if !diagnostics.HasErrors() {
				t.Fatal("invalid literal produced no diagnostics")
			}
			foundInvalid := false
			for _, token := range tokens {
				if token.Kind != Invalid {
					continue
				}
				foundInvalid = true
				if token.decoded != (tokenDecodedLiteral{}) {
					t.Fatalf("invalid token retained decoded value %+v", token.decoded)
				}
			}
			if !foundInvalid {
				t.Fatal("invalid literal produced no Invalid token")
			}

			tree, _, _ := parseFragmentText(t, "invalid-literal.peb", text, parseExpressionFragment)
			assertNoDecodedLiteral(t, tree, tree.Root())
		})
	}
}

func TestParserPreservesDecodedLiteralAndSourceSpan(t *testing.T) {
	tests := []struct {
		name string
		text string
		want DecodedLiteral
	}{
		{name: "empty string", text: `""`, want: DecodedLiteral{Kind: DecodedString}},
		{name: "ordinary string", text: `"A\xFF\u{2603}\n"`, want: DecodedLiteral{Kind: DecodedString, Text: "A" + string([]byte{0xff}) + "☃\n"}},
		{name: "direct Unicode string", text: `"𐀀"`, want: DecodedLiteral{Kind: DecodedString, Text: "𐀀"}},
		{name: "character", text: `'\xFF'`, want: DecodedLiteral{Kind: DecodedCharacter, Rune: 0xff}},
		{name: "maximum character", text: `'\u{10FFFF}'`, want: DecodedLiteral{Kind: DecodedCharacter, Rune: 0x10ffff}},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			tree, diagnostics, sources := parseFragmentText(t, "literal.peb", test.text, parseExpressionFragment)
			if diagnostics.HasErrors() {
				t.Fatalf("unexpected diagnostics: %+v", diagnostics.Items())
			}
			node := mustNode(t, tree, tree.Root())
			got, ok := node.DecodedLiteral()
			if !ok || got != test.want {
				t.Fatalf("DecodedLiteral() = {%+v, %t}, want {%+v, true}", got, ok, test.want)
			}
			file, _ := sources.File(node.Span().Source)
			if node.Span().Start != 0 || node.Span().End != uint32(len(test.text)) || string(file.Slice(node.Span())) != test.text {
				t.Fatalf("span = %+v spelling = %q, want unchanged %q", node.Span(), file.Slice(node.Span()), test.text)
			}
		})
	}
}

func TestParserPreservesInterpolationTextInOrder(t *testing.T) {
	text := "`a\\{\\xFF{x}\\`z`"
	tree, diagnostics, sources := parseFragmentText(t, "interpolation.peb", text, parseExpressionFragment)
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics: %+v", diagnostics.Items())
	}
	root := mustNode(t, tree, tree.Root())
	if root.Kind() != InterpolatedString || len(root.Children()) != 3 {
		t.Fatalf("unexpected interpolation tree:\n%s", tree.DumpString())
	}

	first := mustNode(t, tree, root.Children()[0])
	last := mustNode(t, tree, root.Children()[2])
	wantFirst := "a{" + string([]byte{0xff})
	for _, check := range []struct {
		node Node
		want string
	}{
		{node: first, want: wantFirst},
		{node: last, want: "`z"},
	} {
		decoded, ok := check.node.DecodedLiteral()
		if !ok || decoded.Kind != DecodedInterpolationText || decoded.Text != check.want {
			t.Fatalf("interpolation text = {%+v, %t}, want %q", decoded, ok, check.want)
		}
		file, _ := sources.File(check.node.Span().Source)
		if len(file.Slice(check.node.Span())) == 0 {
			t.Fatal("interpolation text lost its authored source span")
		}
	}
	if _, ok := mustNode(t, tree, root.Children()[1]).DecodedLiteral(); ok {
		t.Fatal("embedded expression unexpectedly exposed decoded text")
	}
}

func TestParserPreservesDecodedStringsInEveryLiteralPosition(t *testing.T) {
	text := `
import "core\x2fio";
let value str = "body";
fn inline "C" call() int => 0;
extern "lib\x63" { fn run() int; }
type Callback = fn "C"() int;
`
	tree, diagnostics, _ := parseFragmentText(t, "literal-positions.peb", text, Parse)
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics: %+v\n%s", diagnostics.Items(), tree.DumpString())
	}

	var got []string
	var visit func(NodeID)
	visit = func(id NodeID) {
		node := mustNode(t, tree, id)
		if decoded, ok := node.DecodedLiteral(); ok && decoded.Kind == DecodedString {
			got = append(got, decoded.Text)
		}
		for _, child := range node.Children() {
			visit(child)
		}
	}
	visit(tree.Root())
	want := []string{"core/io", "body", "C", "libc", "C"}
	if fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("decoded strings = %q, want %q", got, want)
	}
}

func TestDecodedLiteralAccessorRejectsMismatches(t *testing.T) {
	for _, text := range []string{"0", "1.0", "true", "false", "nil", "none"} {
		tree, diagnostics, _ := parseFragmentText(t, "non-decoded.peb", text, parseExpressionFragment)
		if diagnostics.HasErrors() {
			t.Fatalf("%s: unexpected diagnostics: %+v", text, diagnostics.Items())
		}
		if decoded, ok := mustNode(t, tree, tree.Root()).DecodedLiteral(); ok || decoded != (DecodedLiteral{}) {
			t.Fatalf("%s: DecodedLiteral() = {%+v, %t}, want zero, false", text, decoded, ok)
		}
	}

	mismatches := []Node{
		{value: node{kind: Error, token: StringLiteral, decoded: DecodedLiteral{Kind: DecodedString, Text: "bad"}}},
		{value: node{kind: Literal, token: IntegerLiteral, decoded: DecodedLiteral{Kind: DecodedString, Text: "bad"}}},
		{value: node{kind: Literal, token: StringLiteral, decoded: DecodedLiteral{Kind: DecodedCharacter, Rune: 'x'}}},
		{value: node{kind: Literal, token: CharacterLiteral, decoded: DecodedLiteral{Kind: DecodedInterpolationText, Text: "bad"}}},
	}
	for i, mismatch := range mismatches {
		if decoded, ok := mismatch.DecodedLiteral(); ok || decoded != (DecodedLiteral{}) {
			t.Fatalf("mismatch %d: DecodedLiteral() = {%+v, %t}, want zero, false", i, decoded, ok)
		}
	}
}

func singleLiteralToken(t *testing.T, text string, want TokenKind) Token {
	t.Helper()
	tokens, diagnostics := lexTokens(t, text)
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics for %q: %+v", text, diagnostics.Items())
	}
	if len(tokens) != 2 || tokens[0].Kind != want || tokens[1].Kind != EOF {
		t.Fatalf("tokens for %q = %+v, want %s EOF", text, tokens, want)
	}
	return tokens[0]
}

func interpolationTextToken(t *testing.T, text string) Token {
	t.Helper()
	tokens, diagnostics := lexTokens(t, text)
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics for %q: %+v", text, diagnostics.Items())
	}
	wantKinds := []TokenKind{InterpolationStart, InterpolationText, InterpolationEnd, EOF}
	if len(tokens) != len(wantKinds) {
		t.Fatalf("tokens for %q = %+v", text, tokens)
	}
	for i, want := range wantKinds {
		if tokens[i].Kind != want {
			t.Fatalf("token %d for %q = %s, want %s", i, text, tokens[i].Kind, want)
		}
	}
	return tokens[1]
}

func lexTokens(t *testing.T, text string) ([]Token, *diagnostic.DiagnosticSet) {
	t.Helper()
	sources := source.NewFileSet()
	id, err := sources.Add("literal.peb", []byte(text))
	if err != nil {
		t.Fatal(err)
	}
	file, _ := sources.File(id)
	diagnostics := diagnostic.NewDiagnosticSet()
	lexer := NewLexer(file, diagnostics)
	var tokens []Token
	for range len(text) + 16 {
		token := lexer.Next()
		tokens = append(tokens, token)
		if token.Kind == EOF {
			return tokens, diagnostics
		}
	}
	t.Fatalf("lexer did not terminate for %q", text)
	return nil, diagnostics
}

func assertNoDecodedLiteral(t *testing.T, tree *Tree, id NodeID) {
	t.Helper()
	node := mustNode(t, tree, id)
	if decoded, ok := node.DecodedLiteral(); ok || decoded != (DecodedLiteral{}) {
		t.Fatalf("%s unexpectedly retained decoded literal {%+v, %t}", node.Kind(), decoded, ok)
	}
	for _, child := range node.Children() {
		assertNoDecodedLiteral(t, tree, child)
	}
}

func TestDecodedByteStringsMayContainInvalidUTF8(t *testing.T) {
	token := singleLiteralToken(t, `"\xFF"`, StringLiteral)
	if !bytes.Equal([]byte(token.decoded.text), []byte{0xff}) || strings.ToValidUTF8(token.decoded.text, "replacement") == token.decoded.text {
		t.Fatalf("decoded byte string = % x, want raw ff", []byte(token.decoded.text))
	}
}
