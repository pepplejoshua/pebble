package stdlib

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
)

// TestNewWithDiskRootReadsFromDisk proves that a Provider constructed with a
// real diskRoot reads std: imports from that directory on disk, not from the
// embedded go:embed copy. It writes a temp std/ directory whose content is
// deliberately DIFFERENT from the real embed (which contains set.peb) and
// confirms the provider returns the disk version.
func TestNewWithDiskRootReadsFromDisk(t *testing.T) {
	root := t.TempDir()
	diskContent := "fn disk_version() int { return 999; }\n"
	if err := os.WriteFile(filepath.Join(root, "set.peb"), []byte(diskContent), 0o644); err != nil {
		t.Fatal(err)
	}

	p := New(module.FileSystemProvider{}, root)

	got, err := p.ReadFile(module.CanonicalPath(StandardRoot + "/set.peb"))
	if err != nil {
		t.Fatalf("ReadFile from diskRoot: %v", err)
	}
	if string(got) != diskContent {
		t.Fatalf("ReadFile returned %q, want disk content %q (embed must not be served when a real root exists)", got, diskContent)
	}
}

// TestNewWithEmptyDiskRootFallsBackToEmbed proves that a Provider constructed
// with an empty diskRoot serves std: imports from the embedded go:embed copy,
// exactly as a standalone binary with nothing next to it must.
func TestNewWithEmptyDiskRootFallsBackToEmbed(t *testing.T) {
	p := New(module.FileSystemProvider{}, "")

	got, err := p.ReadFile(module.CanonicalPath(StandardRoot + "/set.peb"))
	if err != nil {
		t.Fatalf("ReadFile from embed: %v", err)
	}
	if len(got) == 0 {
		t.Fatal("embed served empty content")
	}
	// The embedded copy must not contain our synthetic disk marker, proving we
	// got the real embed rather than any on-disk content.
	if strings.Contains(string(got), "disk_version") {
		t.Fatal("ReadFile returned disk content, expected embedded stdlib")
	}
}

// TestNewWithNonexistentDiskRootFallsBackToEmbed proves that a Provider
// constructed with a diskRoot that does not exist (or is not a directory) also
// falls back to the embed rather than failing or serving empty content.
func TestNewWithNonexistentDiskRootFallsBackToEmbed(t *testing.T) {
	missing := filepath.Join(t.TempDir(), "no-such-std")
	p := New(module.FileSystemProvider{}, missing)

	got, err := p.ReadFile(module.CanonicalPath(StandardRoot + "/set.peb"))
	if err != nil {
		t.Fatalf("ReadFile with nonexistent diskRoot: %v", err)
	}
	if len(got) == 0 {
		t.Fatal("embed served empty content")
	}
}

// TestNewWithFileDiskRootFallsBackToEmbed proves that a diskRoot pointing at a
// regular file (not a directory) falls back to the embed.
func TestNewWithFileDiskRootFallsBackToEmbed(t *testing.T) {
	fileRoot := filepath.Join(t.TempDir(), "std")
	if err := os.WriteFile(fileRoot, []byte("not a dir"), 0o644); err != nil {
		t.Fatal(err)
	}
	p := New(module.FileSystemProvider{}, fileRoot)

	got, err := p.ReadFile(module.CanonicalPath(StandardRoot + "/set.peb"))
	if err != nil {
		t.Fatalf("ReadFile with file-as-diskRoot: %v", err)
	}
	if len(got) == 0 {
		t.Fatal("embed served empty content")
	}
}
