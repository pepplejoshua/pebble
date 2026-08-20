package main

import (
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"
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

// fakeScheduler captures the debouncer's timer callbacks instead of running
// them, so a test can fire them deterministically with no realtime waits. The
// returned timers are real but never fire on their own within the test.
type fakeScheduler struct {
	mu        sync.Mutex
	callbacks []func()
}

func (f *fakeScheduler) after(_ time.Duration, cb func()) *time.Timer {
	// The timer is created but its callback is captured for manual firing; a
	// real timer armed for an hour will not fire during the test.
	f.mu.Lock()
	f.callbacks = append(f.callbacks, cb)
	f.mu.Unlock()
	return time.NewTimer(time.Hour)
}

// fireAll runs every captured timer callback exactly once, in order.
func (f *fakeScheduler) fireAll() {
	f.mu.Lock()
	cbs := append([]func(){}, f.callbacks...)
	f.callbacks = nil
	f.mu.Unlock()
	for _, cb := range cbs {
		cb()
	}
}

// newTestDebouncer builds a debouncer whose timer callbacks are captured by a
// fakeScheduler and whose fires are collected into delivered.
func newTestDebouncer() (*debouncer, *fakeScheduler, *[]string) {
	fake := &fakeScheduler{}
	var delivered []string
	b := newDebouncer(fileChangeDebounce, func(key string) {
		delivered = append(delivered, key)
	})
	b.after = fake.after
	return b, fake, &delivered
}

// TestDebouncerCoalescesBursts verifies the core guarantee behind the
// double-rebuild fix: a burst of rapid events for one path schedules exactly
// one timer and delivers exactly one event, only after the burst quiets down,
// and a later event schedules a fresh delivery rather than being lost.
func TestDebouncerCoalescesBursts(t *testing.T) {
	b, fake, delivered := newTestDebouncer()

	b.note("main.peb")
	b.note("main.peb")
	b.note("main.peb")
	if n := len(fake.callbacks); n != 1 {
		t.Fatalf("burst of 3 notes scheduled %d timers, want 1", n)
	}
	fake.fireAll()
	if got := strings.Join(*delivered, ","); got != "main.peb" {
		t.Fatalf("delivered %q after burst, want a single main.peb", got)
	}

	// A genuinely later event (after the burst already fired) must schedule a
	// fresh timer and deliver again.
	b.note("main.peb")
	if n := len(fake.callbacks); n != 1 {
		t.Fatalf("post-fire note scheduled %d timers, want 1", n)
	}
	fake.fireAll()
	if got := strings.Join(*delivered, ","); got != "main.peb,main.peb" {
		t.Fatalf("delivered %q, want main.peb exactly twice", got)
	}
}

// TestDebouncerKeysAreIndependent verifies that events for different paths are
// debounced separately: a burst on one path never suppresses or merges an
// event for another.
func TestDebouncerKeysAreIndependent(t *testing.T) {
	b, fake, delivered := newTestDebouncer()

	b.note("a.peb")
	b.note("b.peb")
	b.note("a.peb")
	if n := len(fake.callbacks); n != 2 {
		t.Fatalf("notes for two keys scheduled %d timers, want 2", n)
	}
	fake.fireAll()
	sort.Strings(*delivered)
	if got := strings.Join(*delivered, ","); got != "a.peb,b.peb" {
		t.Fatalf("delivered %q, want a.peb and b.peb once each", got)
	}
}
