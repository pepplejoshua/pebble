package main

import (
	"path/filepath"
	"testing"
)

// TestDetectChangeNoopVsChanged exercises the content-hash change-detection
// logic directly: an identical rewrite of a tracked file must record a "noop"
// (not a change), a real content edit must record a "changed" and update the
// stored hash, and an untracked file must be ignored entirely.
func TestDetectChangeNoopVsChanged(t *testing.T) {
	dir := t.TempDir()
	tracked := filepath.Join(dir, "tracked.peb")
	writeFile(t, tracked, "fn main() int { return 1; }\n")

	d := &daemon{
		fileHashes: map[string]string{},
		recent:     make([]watchReport, 0, watchReportCap),
	}
	d.trackFiles([]string{tracked})

	// Initial build recorded the file; no events yet.
	if len(d.recent) != 0 {
		t.Fatalf("expected no events after tracking, got %d", len(d.recent))
	}

	// Identical rewrite -> noop, not a change.
	writeFile(t, tracked, "fn main() int { return 1; }\n")
	d.detectChange(tracked)
	if len(d.recent) != 1 || d.recent[0].Kind != "noop" {
		t.Fatalf("identical rewrite: got %+v, want one noop event", d.recent)
	}

	// Real content change -> changed, and the stored hash updates.
	writeFile(t, tracked, "fn main() int { return 2; }\n")
	d.detectChange(tracked)
	if len(d.recent) != 2 || d.recent[1].Kind != "changed" {
		t.Fatalf("real change: got %+v, want a changed event", d.recent)
	}

	// Untracked file events must be ignored entirely.
	untracked := filepath.Join(dir, "untracked.peb")
	writeFile(t, untracked, "fn main() int { return 3; }\n")
	before := len(d.recent)
	d.detectChange(untracked)
	if len(d.recent) != before {
		t.Fatalf("untracked file produced an event: %+v", d.recent[before:])
	}

	// A second identical rewrite after the change is again a noop.
	writeFile(t, tracked, "fn main() int { return 2; }\n")
	d.detectChange(tracked)
	if len(d.recent) != 3 || d.recent[2].Kind != "noop" {
		t.Fatalf("second identical rewrite: got %+v, want a noop event", d.recent)
	}
}

// TestTrackFilesScopesToModuleGraph verifies trackFiles replaces the tracked
// set with exactly the files of the most recent build's module graph.
func TestTrackFilesScopesToModuleGraph(t *testing.T) {
	dir := t.TempDir()
	a := filepath.Join(dir, "a.peb")
	b := filepath.Join(dir, "b.peb")
	writeFile(t, a, "fn a() int { return 1; }\n")
	writeFile(t, b, "fn b() int { return 2; }\n")

	d := &daemon{
		fileHashes: map[string]string{},
		recent:     make([]watchReport, 0, watchReportCap),
	}
	d.trackFiles([]string{a, b})
	if len(d.fileHashes) != 2 {
		t.Fatalf("tracked %d files, want 2", len(d.fileHashes))
	}

	// A later build that no longer includes b drops it.
	d.trackFiles([]string{a})
	if len(d.fileHashes) != 1 {
		t.Fatalf("tracked %d files after rebuild, want 1", len(d.fileHashes))
	}
	if _, ok := d.fileHashes[a]; !ok {
		t.Fatal("a.peb dropped from tracking unexpectedly")
	}
	if _, ok := d.fileHashes[b]; ok {
		t.Fatal("b.peb still tracked after leaving the module graph")
	}
}
