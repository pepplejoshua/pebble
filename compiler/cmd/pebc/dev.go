package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"time"
)

// pebc dev: a fast rebuild-restart front end (slice 21.3)
//
// `pebc dev <entry.peb>` supervises a program compiled and served by the
// persistent daemon (21.1a/21.1b). It ensures a daemon is running, does an
// initial build, launches the executable as a child, and then watches for
// source changes. On a new change it rebuilds via the daemon and, on success,
// kills and relaunches the child; on a failed build it leaves the last-known-
// good child running and prints the diagnostics.
//
// Change detection is POLLING, not push-based: pebc dev queries the daemon's
// `watch-status` RPC on a short interval (300ms by default) and looks for any
// new `changed` event since the last poll. A real push mechanism (the daemon
// proactively telling a connected client "something changed" rather than the
// client asking) would be nicer but is explicitly out of scope for this slice;
// polling is an acceptable, honestly-scoped v1. NOTE FOR A FUTURE PUSH-BASED
// VERSION: replace devChangeTracker + the poll ticker with a daemon-initiated
// notification channel carrying change events, while keeping the rest of the
// rebuild/restart machinery identical.

// defaultDevPollInterval is how often pebc dev asks the daemon for a
// watch-status update.
const defaultDevPollInterval = 300 * time.Millisecond

// runDev implements `pebc dev <entry.peb>`.
func runDev(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("pebc dev", flag.ContinueOnError)
	fs.SetOutput(stderr)
	output := fs.String("o", "", "output executable path (default: entry file basename)")
	poll := fs.Duration("poll", defaultDevPollInterval, "watch-status poll interval")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "pebc dev: usage: pebc dev [-o out] <entry.peb>")
		return 2
	}
	entry, err := filepath.Abs(fs.Arg(0))
	if err != nil {
		fmt.Fprintf(stderr, "pebc dev: cannot resolve entry: %v\n", err)
		return 1
	}

	if err := ensureDaemon(stderr); err != nil {
		fmt.Fprintf(stderr, "pebc dev: cannot ensure daemon: %v\n", err)
		return 1
	}

	// Initial build: a runnable binary is required before supervision starts.
	outcome := devRPCBuild(entry, *output)
	if !outcome.ok {
		reportBuildFailure(stderr, outcome)
		fmt.Fprintln(stderr, "pebc dev: initial build failed; nothing to run")
		return 1
	}

	sup := &devSupervisor{stdout: stdout, stderr: stderr}
	sup.launch(outcome.binaryPath)

	tracker := &devChangeTracker{}
	pollTicker := time.NewTicker(*poll)
	defer pollTicker.Stop()

	sig := make(chan os.Signal, 1)
	signal.Notify(sig, os.Interrupt)
	defer signal.Stop(sig)

	for {
		select {
		case <-sig:
			// Clean shutdown on Ctrl-C (SIGINT). Kill the supervised child so
			// no orphan program is left behind, then exit. The daemon is
			// intentionally LEFT running: it has its own idle-timeout self-
			// shutdown (21.1a), so it will exit on its own when idle, and
			// staying warm serves the next `pebc dev` or `daemon build` faster.
			// This is the explicit design decision the 21.3 plan's open
			// question left to this slice to settle.
			sup.kill()
			fmt.Fprintln(stderr, "\npebc dev: stopped (daemon left running)")
			return 0
		case <-pollTicker.C:
			events, err := devRPCWatchStatus()
			if err != nil {
				// The daemon may be mid-restart (stale-binary re-exec);
				// retry on the next poll.
				continue
			}
			if !tracker.hasNewChange(events) {
				continue
			}
			// A tracked source file changed. Rebuild via the daemon. A
			// successful build kills and relaunches the child; a failed build
			// leaves the last-known-good child running and reports the
			// diagnostics, waiting for the next fix.
			outcome := devRPCBuild(entry, *output)
			if !outcome.ok {
				reportBuildFailure(stderr, outcome)
				continue
			}
			sup.restart(outcome.binaryPath)
		}
	}
}

// reportBuildFailure prints a failed build's diagnostics and error to stderr.
func reportBuildFailure(stderr io.Writer, b buildOutcome) {
	if b.diagnostics != "" {
		fmt.Fprint(stderr, b.diagnostics)
	}
	if b.err != "" {
		fmt.Fprintln(stderr, b.err)
	}
}

// ensureDaemon makes sure a daemon is running for the current project root,
// starting one in the background if none is live. It reuses the existing
// daemon machinery (`pingDaemon` for the probe, and the `pebc daemon start`
// subprocess path for launching) rather than duplicating the socket-ownership
// or probe-then-attach logic — `pebc daemon start` owns all of that.
func ensureDaemon(stderr io.Writer) error {
	root, err := daemonRoot()
	if err != nil {
		return err
	}
	sockPath := daemonSocketPath(root)
	if live, _ := pingDaemon(sockPath); live {
		return nil
	}
	exe, err := os.Executable()
	if err != nil {
		return err
	}
	cmd := exec.Command(exe, "daemon", "start")
	cmd.Dir = root
	// The daemon is a detached background process that serves this and future
	// pebc dev / daemon build invocations. Its own lifecycle messages go to
	// stderr so they never corrupt pebc dev's stdout, which is reserved for
	// the supervised program's real output.
	cmd.Stdout = stderr
	cmd.Stderr = stderr
	cmd.Stdin = nil
	if err := cmd.Start(); err != nil {
		return err
	}
	// Detach: the daemon continues running after pebc dev exits.
	_ = cmd.Process.Release()
	// Poll until the daemon's socket is live; it may take a moment to bind and
	// enter its event loop.
	deadline := time.Now().Add(5 * time.Second)
	for time.Now().Before(deadline) {
		if live, _ := pingDaemon(sockPath); live {
			return nil
		}
		time.Sleep(50 * time.Millisecond)
	}
	return errors.New("daemon did not come up within 5s")
}

// daemonRPC performs one daemon RPC round-trip for the current project root.
func daemonRPC(method string, req daemonRequest) (daemonResponse, error) {
	root, err := daemonRoot()
	if err != nil {
		return daemonResponse{}, err
	}
	conn, err := net.DialTimeout("unix", daemonSocketPath(root), 2*time.Second)
	if err != nil {
		return daemonResponse{}, fmt.Errorf("no daemon running for %s: %w", root, err)
	}
	defer conn.Close()
	_ = conn.SetDeadline(time.Now().Add(daemonClientTimeout))
	req.Method = method
	if err := writeDaemonMessage(conn, req); err != nil {
		return daemonResponse{}, err
	}
	var resp daemonResponse
	if err := readDaemonMessage(conn, &resp); err != nil {
		return daemonResponse{}, err
	}
	return resp, nil
}

// devRPCBuild runs a build through the daemon and returns its outcome.
func devRPCBuild(entry, output string) buildOutcome {
	resp, err := daemonRPC("build", daemonRequest{Entry: entry, Output: output})
	if err != nil {
		return buildOutcome{err: err.Error()}
	}
	return buildOutcome{
		ok:          resp.OK,
		binaryPath:  resp.Output,
		diagnostics: resp.Diagnostics,
		err:         resp.Error,
	}
}

// devRPCWatchStatus returns the daemon's recent watch-status event log.
func devRPCWatchStatus() ([]watchReport, error) {
	resp, err := daemonRPC("watch-status", daemonRequest{})
	if err != nil {
		return nil, err
	}
	return resp.WatchEvents, nil
}

// buildOutcome captures the result of one daemon build and what it implies for
// the supervised child.
type buildOutcome struct {
	ok          bool
	binaryPath  string
	diagnostics string
	err         string
}

// keepRunning reports whether the currently-running child should be left
// running after this build outcome. A failed build must never kill the
// last-known-good child; only a successful build supersedes it.
func (b buildOutcome) keepRunning() bool { return !b.ok }

// devChangeTracker detects a NEW `changed` watch event since the last poll.
// It works by counting how many `changed` events the daemon's bounded log has
// reported so far and comparing it against the previous count. Because the
// log is bounded (watchReportCap), a rollover between polls can only
// under-count, never fabricate a change, which is the safe direction.
type devChangeTracker struct {
	seenChanged int
}

// hasNewChange reports whether the response contains a `changed` event not
// present on a previous call, and advances the tracker past it.
func (t *devChangeTracker) hasNewChange(events []watchReport) bool {
	total := 0
	for _, ev := range events {
		if ev.Kind == "changed" {
			total++
		}
	}
	if total > t.seenChanged {
		t.seenChanged = total
		return true
	}
	t.seenChanged = total
	return false
}

// devSupervisor launches and restarts the supervised child program, forwarding
// its stdout/stderr to pebc dev's own so the user sees the program's real
// output live.
type devSupervisor struct {
	stdout io.Writer
	stderr io.Writer
	cmd    *exec.Cmd
}

// launch starts the given binary as the supervised child.
func (s *devSupervisor) launch(binaryPath string) {
	cmd := exec.Command(binaryPath)
	cmd.Stdout = s.stdout
	cmd.Stderr = s.stderr
	cmd.Stdin = os.Stdin
	if err := cmd.Start(); err != nil {
		fmt.Fprintf(s.stderr, "pebc dev: cannot start %q: %v\n", binaryPath, err)
		return
	}
	s.cmd = cmd
	fmt.Fprintf(s.stderr, "pebc dev: started %s (pid %d)\n", binaryPath, cmd.Process.Pid)
}

// restart kills the currently-running child (if any) and launches the new
// binary. Used after a successful rebuild.
func (s *devSupervisor) restart(binaryPath string) {
	s.kill()
	s.launch(binaryPath)
}

// kill terminates the supervised child if one is running.
func (s *devSupervisor) kill() {
	if s.cmd != nil && s.cmd.Process != nil {
		_ = s.cmd.Process.Kill()
		s.cmd = nil
	}
}
