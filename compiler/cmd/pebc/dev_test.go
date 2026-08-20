package main

import (
	"testing"
)

// TestDevChangeTrackerDetectsNewChangedEvents exercises the change-detection
// logic directly (no live daemon): a `changed` event new since the last poll
// must be reported, re-reporting the same response must be idempotent, a
// `noop` event alone must never be a change, and a subsequent change after
// intervening noops must still be detected.
func TestDevChangeTrackerDetectsNewChangedEvents(t *testing.T) {
	tracker := &devChangeTracker{}

	if tracker.hasNewChange(nil) {
		t.Fatal("nil events reported a change")
	}
	if !tracker.hasNewChange([]watchReport{{Kind: "changed"}}) {
		t.Fatal("first changed event not detected")
	}
	if tracker.hasNewChange([]watchReport{{Kind: "changed"}}) {
		t.Fatal("same changed event reported twice")
	}
	if tracker.hasNewChange([]watchReport{{Kind: "noop"}}) {
		t.Fatal("noop event reported as a change")
	}
	if !tracker.hasNewChange([]watchReport{{Kind: "noop"}, {Kind: "changed"}}) {
		t.Fatal("second changed event not detected")
	}
	if tracker.hasNewChange([]watchReport{{Kind: "noop"}, {Kind: "changed"}}) {
		t.Fatal("second changed event reported twice")
	}
}

// TestDevChangeTrackerHandlesBoundedLogRollover verifies that when the
// daemon's bounded log rolls and drops old entries (so the count goes down),
// the tracker does not fabricate a change, yet still detects a genuinely new
// change afterward.
func TestDevChangeTrackerHandlesBoundedLogRollover(t *testing.T) {
	tracker := &devChangeTracker{}

	// Two changes recorded.
	tracker.hasNewChange([]watchReport{{Kind: "changed"}, {Kind: "changed"}})

	// The bounded log rolls and drops old entries; the count resets downward.
	if tracker.hasNewChange([]watchReport{{Kind: "changed"}}) {
		t.Fatal("log rollover misreported as a source change")
	}

	// A genuinely new change after the rollover is still detected.
	if !tracker.hasNewChange([]watchReport{{Kind: "changed"}, {Kind: "changed"}}) {
		t.Fatal("change after rollover not detected")
	}
}

// TestBuildFailureKeepsChildRunning encodes the dev-reload invariant that a
// failed build must never kill the last-known-good running child, while a
// successful build supersedes it.
func TestBuildFailureKeepsChildRunning(t *testing.T) {
	fail := buildOutcome{ok: false, diagnostics: "syntax error"}
	if !fail.keepRunning() {
		t.Fatal("failed build should keep the running child")
	}

	ok := buildOutcome{ok: true, binaryPath: "/x/bin"}
	if ok.keepRunning() {
		t.Fatal("successful build should not keep the old child")
	}
}
