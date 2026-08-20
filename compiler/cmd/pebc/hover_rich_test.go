package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func hoverForType(t *testing.T, src, typeName string) string {
	t.Helper()
	dir := t.TempDir()
	path := filepath.Join(dir, "main.peb")
	if err := os.WriteFile(path, []byte(src), 0o644); err != nil {
		t.Fatalf("write temp peb: %v", err)
	}
	needle := "type " + typeName
	idx := strings.Index(src, needle)
	if idx < 0 {
		t.Fatalf("src does not contain %q", needle)
	}
	// offset inside the type name itself
	nameIdx := idx + len("type ")
	off := uint32(nameIdx + len(typeName)/2)
	got := hoverTypeAtOffset(path, off)
	if got == "" {
		t.Fatalf("hover for %q returned empty (src %q)", typeName, src)
	}
	return got
}

func TestHoverRichStruct(t *testing.T) {
	src := "type Point = struct {\n    x int;\n    y int;\n    z i32;\n};\nfn main() int { return 0; }\n"
	got := hoverForType(t, src, "Point")
	if !strings.Contains(got, "type Point") {
		t.Fatalf("struct hover missing type name: %q", got)
	}
	if !strings.Contains(got, "struct") {
		t.Fatalf("struct hover missing keyword struct: %q", got)
	}
	for _, want := range []string{"x int", "y int", "z i32"} {
		if !strings.Contains(got, want) {
			t.Fatalf("struct hover missing field %q: %q", want, got)
		}
	}
}

func TestHoverRichEnum(t *testing.T) {
	src := "type Color = enum {\n    Red,\n    Green,\n    Blue,\n};\nfn main() int { return 0; }\n"
	got := hoverForType(t, src, "Color")
	if !strings.Contains(got, "type Color") {
		t.Fatalf("enum hover missing type name: %q", got)
	}
	if !strings.Contains(got, "enum") {
		t.Fatalf("enum hover missing keyword enum: %q", got)
	}
	for _, want := range []string{"Red;", "Green;", "Blue;"} {
		if !strings.Contains(got, want) {
			t.Fatalf("enum hover missing variant %q: %q", want, got)
		}
	}
	// Plain enum variants must not carry payloads
	if strings.Contains(got, "(Color)") || strings.Contains(got, "Red(Color)") {
		t.Fatalf("plain enum variant should not have payload: %q", got)
	}
}

func TestHoverRichTaggedUnion(t *testing.T) {
	// Fixture based on std/result.peb's Result[T,E] = union enum { Ok T; Err E; }
	// Use concrete payload types to keep the hover deterministic.
	src := "type MyResult = union enum {\n    Ok i32;\n    Err str;\n};\nfn main() int { return 0; }\n"
	got := hoverForType(t, src, "MyResult")
	if !strings.Contains(got, "type MyResult") {
		t.Fatalf("tagged union hover missing type name: %q", got)
	}
	if !strings.Contains(got, "enum") {
		t.Fatalf("tagged union hover missing keyword enum: %q", got)
	}
	if !strings.Contains(got, "Ok(i32)") {
		t.Fatalf("tagged union hover missing payload variant Ok(i32): %q", got)
	}
	if !strings.Contains(got, "Err(str)") {
		t.Fatalf("tagged union hover missing payload variant Err(str): %q", got)
	}
}

func TestHoverRichUnion(t *testing.T) {
	// Untagged union (NominalUnion): the resolver registers its members as
	// SymbolVariant (same as enum/tagged-union), not SymbolField -- see
	// internal/check/member_validation.go's untaggedUnionDeclaration doc
	// comment -- but they behave like fields (unsafe reinterpret-the-bytes
	// access) and must render in the "name type;" form, not as payload
	// variants.
	src := "type MyUnion = union {\n    a i32;\n    b str;\n};\nfn main() int { return 0; }\n"
	got := hoverForType(t, src, "MyUnion")
	if !strings.Contains(got, "type MyUnion") {
		t.Fatalf("union hover missing type name: %q", got)
	}
	if !strings.Contains(got, "union") {
		t.Fatalf("union hover missing keyword union: %q", got)
	}
	for _, want := range []string{"a i32", "b str"} {
		if !strings.Contains(got, want) {
			t.Fatalf("union hover missing member %q: %q", want, got)
		}
	}
}

func TestHoverRichAlias(t *testing.T) {
	src := "type Foo = i32;\nfn main() int { return 0; }\n"
	got := hoverForType(t, src, "Foo")
	if got != "type Foo = i32" && got != "type Foo" {
		t.Fatalf("alias hover unexpected: %q", got)
	}
	if !strings.Contains(got, "type Foo") {
		t.Fatalf("alias hover missing type name: %q", got)
	}
}
