package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestMemberCompletionsIncludeSlicePseudoFields(t *testing.T) {
	dir := t.TempDir()
	src := "fn main() int {\n" +
		"    var arr [10]int = [1,2,3,4,5,6,7,8,9,10];\n" +
		"    var s []int = arr[2:7];\n" +
		"    var n = s.\n" +
		"    return 0;\n" +
		"}\n"
	path := filepath.Join(dir, "main.peb")
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	off := uint32(searchOffset(src, "s.\n")) + 2
	items := completionsAtOffset(path, off)
	byName := make(map[string]structuredCompletionItem, len(items))
	for _, it := range items {
		byName[it.Name] = it
	}
	if _, ok := byName["len"]; !ok {
		t.Fatalf("expected 'len' completion for slice receiver, got %v", items)
	}
	if _, ok := byName["data"]; !ok {
		t.Fatalf("expected 'data' completion for slice receiver, got %v", items)
	}
}

func TestMemberCompletionsIncludeArrayLen(t *testing.T) {
	dir := t.TempDir()
	src := "fn main() int {\n" +
		"    var arr [10]int = [1,2,3,4,5,6,7,8,9,10];\n" +
		"    var n = arr.\n" +
		"    return 0;\n" +
		"}\n"
	path := filepath.Join(dir, "main.peb")
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	off := uint32(searchOffset(src, "arr.\n")) + 4
	items := completionsAtOffset(path, off)
	byName := make(map[string]structuredCompletionItem, len(items))
	for _, it := range items {
		byName[it.Name] = it
	}
	if _, ok := byName["len"]; !ok {
		t.Fatalf("expected 'len' completion for array receiver, got %v", items)
	}
	if _, ok := byName["data"]; ok {
		t.Fatalf("array receiver should not offer 'data', got %v", items)
	}
}

func TestHoverOnSlicePseudoFieldNameToken(t *testing.T) {
	dir := t.TempDir()
	src := "fn find_min(items []int) int {\n" +
		"    var min_val = items[0];\n" +
		"    loop 1..items.len : iter {\n" +
		"        min_val = items[iter];\n" +
		"    }\n" +
		"    return min_val;\n" +
		"}\n"
	path := filepath.Join(dir, "main.peb")
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	// Position inside the "len" token of "items.len".
	off := uint32(searchOffset(src, "items.len")) + uint32(len("items.")) + 1
	hover := hoverTypeAtOffset(path, off)
	if hover == "" {
		t.Fatal("expected non-empty hover on the 'len' pseudo-field token")
	}
}
