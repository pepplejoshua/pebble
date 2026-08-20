package main

import (
	"os"
	"path/filepath"
	"testing"

	"go.lsp.dev/protocol"
)

func TestIdentifierCompletionsIncludeKeywords(t *testing.T) {
	dir := t.TempDir()
	src := "type Point = struct {\n    x int;\n    y int;\n};\n\nfn main() int {\n    var origin Point = Point.{ x = 0, y = 0 };\n    return 0;\n}\n"
	path := filepath.Join(dir, "main.peb")
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	// Position inside function body, not after a dot. Use offset at "return 0;" line start.
	// Compute byte offset of "return 0;" inside main.
	off := uint32(searchOffset(src, "return 0;"))
	items := completionsAtOffset(path, off)
	if len(items) == 0 {
		t.Fatalf("identifier completions returned no items")
	}
	byName := make(map[string]structuredCompletionItem, len(items))
	for _, it := range items {
		byName[it.Name] = it
	}
	for _, kw := range []string{"if", "return", "fn", "let", "while", "struct", "true", "false"} {
		it, ok := byName[kw]
		if !ok {
			t.Fatalf("identifier completion missing keyword %q; got %d items", kw, len(items))
		}
		if it.Kind != int(protocol.CompletionItemKindKeyword) {
			t.Fatalf("keyword %q kind = %d, want %d (Keyword)", kw, it.Kind, int(protocol.CompletionItemKindKeyword))
		}
		if it.Detail != "" {
			t.Fatalf("keyword %q detail = %q, want empty", kw, it.Detail)
		}
	}
	// Ensure regular identifiers are still present.
	for _, want := range []string{"origin", "Point"} {
		if _, ok := byName[want]; !ok {
			t.Fatalf("identifier completion missing expected identifier %q", want)
		}
	}
}

func TestMemberCompletionsExcludeKeywords(t *testing.T) {
	dir := t.TempDir()
	src := "type Point = struct {\n    x int;\n    y int;\n};\n\nfn main() int {\n    var origin Point = Point.{ x = 0, y = 0 };\n    var z int = origin.x;\n    return 0;\n}\n"
	path := filepath.Join(dir, "main.peb")
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	// Member position: right after "origin." in "origin.x"
	dotIdx := searchOffset(src, "origin.")
	off := uint32(dotIdx + len("origin.")) // cursor right after dot
	items := completionsAtOffset(path, off)
	if len(items) == 0 {
		t.Fatalf("member completions returned no items")
	}
	byName := make(map[string]bool, len(items))
	for _, it := range items {
		byName[it.Name] = true
	}
	if !byName["x"] || !byName["y"] {
		t.Fatalf("member completion missing fields x/y; got %v", items)
	}
	for _, kw := range []string{"if", "return", "fn", "let", "while"} {
		if byName[kw] {
			t.Fatalf("member completion should NOT contain keyword %q; got %v", kw, items)
		}
	}
}

func searchOffset(src, sub string) int {
	idx := 0
	// naive search
	for i := 0; i+len(sub) <= len(src); i++ {
		if src[i:i+len(sub)] == sub {
			return i
		}
		idx = i
		_ = idx
	}
	panic("substring not found: " + sub)
}
