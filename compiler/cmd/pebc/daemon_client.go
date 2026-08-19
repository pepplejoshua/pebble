package main

import (
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"sort"
	"time"
)

// daemonClientTimeout bounds a single daemon RPC round-trip.
const daemonClientTimeout = 2 * time.Minute

// newDaemonFlagSet returns a flag set for the daemon subcommands, writing
// parse errors to stderr.
func newDaemonFlagSet(stderr io.Writer) *flag.FlagSet {
	fs := flag.NewFlagSet("pebc daemon", flag.ContinueOnError)
	fs.SetOutput(stderr)
	return fs
}

// daemonRoot resolves the project root a client operates against. It matches
// the daemon's own resolution: the absolute working directory.
func daemonRoot() (string, error) {
	root, err := os.Getwd()
	if err != nil {
		return "", err
	}
	return filepath.Abs(root)
}

// pingDaemon connects to the socket at sockPath and reports whether a live
// daemon responds. It returns false for any connect/read failure (absent
// socket, stale socket from a dead daemon, etc.).
func pingDaemon(sockPath string) (bool, error) {
	conn, err := net.DialTimeout("unix", sockPath, 2*time.Second)
	if err != nil {
		return false, nil
	}
	defer conn.Close()
	_ = conn.SetDeadline(time.Now().Add(5 * time.Second))
	if err := writeDaemonMessage(conn, daemonRequest{Method: "ping"}); err != nil {
		return false, err
	}
	var resp daemonResponse
	if err := readDaemonMessage(conn, &resp); err != nil {
		return false, err
	}
	return resp.OK, nil
}

// daemonPing implements `pebc daemon ping`: report whether a daemon is live.
func daemonPing(args []string, stdout, stderr io.Writer) int {
	fs := newDaemonFlagSet(stderr)
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "pebc daemon ping: unexpected arguments")
		return 2
	}
	root, err := daemonRoot()
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon ping: %v\n", err)
		return 1
	}
	live, err := pingDaemon(daemonSocketPath(root))
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon ping: %v\n", err)
		return 1
	}
	if live {
		fmt.Fprintln(stdout, "daemon is running")
		return 0
	}
	fmt.Fprintln(stdout, "daemon is not running")
	return 0
}

// daemonWatchStatus implements `pebc daemon watch-status`: report the
// daemon's tracked files and the recent content-change detection log.
func daemonWatchStatus(args []string, stdout, stderr io.Writer) int {
	fs := newDaemonFlagSet(stderr)
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "pebc daemon watch-status: unexpected arguments")
		return 2
	}
	root, err := daemonRoot()
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon watch-status: %v\n", err)
		return 1
	}
	sockPath := daemonSocketPath(root)
	conn, err := net.DialTimeout("unix", sockPath, 2*time.Second)
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon watch-status: no daemon running for %s: %v\n", root, err)
		return 1
	}
	defer conn.Close()
	_ = conn.SetDeadline(time.Now().Add(5 * time.Second))
	if err := writeDaemonMessage(conn, daemonRequest{Method: "watch-status"}); err != nil {
		fmt.Fprintf(stderr, "pebc daemon watch-status: %v\n", err)
		return 1
	}
	var resp daemonResponse
	if err := readDaemonMessage(conn, &resp); err != nil {
		fmt.Fprintf(stderr, "pebc daemon watch-status: %v\n", err)
		return 1
	}
	if !resp.OK {
		fmt.Fprintln(stderr, "pebc daemon watch-status: request failed")
		return 1
	}
	if len(resp.WatchFiles) == 0 {
		fmt.Fprintln(stdout, "no tracked files (run a build first)")
	} else {
		fmt.Fprintf(stdout, "tracked files (%d):\n", len(resp.WatchFiles))
		for _, p := range sortedKeys(resp.WatchFiles) {
			fmt.Fprintf(stdout, "  %s  %s\n", p, shortHash(resp.WatchFiles[p]))
		}
	}
	fmt.Fprintln(stdout, "recent events:")
	if len(resp.WatchEvents) == 0 {
		fmt.Fprintln(stdout, "  (none)")
	}
	for _, ev := range resp.WatchEvents {
		fmt.Fprintf(stdout, "  [%s] %-8s %s\n", ev.Time, ev.Kind, ev.Path)
	}
	return 0
}

// sortedKeys returns the map keys in sorted order for stable output.
func sortedKeys(m map[string]string) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

// shortHash returns the first 12 hex chars of a content hash.
func shortHash(h string) string {
	if len(h) > 12 {
		return h[:12]
	}
	return h
}

// daemonBuild implements `pebc daemon build <entry.peb> [-o out]`: send a
// build request to the running daemon and print the outcome.
func daemonBuild(args []string, stdout, stderr io.Writer) int {
	fs := newDaemonFlagSet(stderr)
	output := fs.String("o", "", "output executable path")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "pebc daemon build: usage: pebc daemon build [-o out] <entry.peb>")
		return 2
	}
	entry, err := filepath.Abs(fs.Arg(0))
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon build: cannot resolve entry: %v\n", err)
		return 1
	}
	root, err := daemonRoot()
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon build: %v\n", err)
		return 1
	}
	sockPath := daemonSocketPath(root)
	conn, err := net.DialTimeout("unix", sockPath, 2*time.Second)
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon build: no daemon running for %s (start one with `pebc daemon start`): %v\n", root, err)
		return 1
	}
	defer conn.Close()
	_ = conn.SetDeadline(time.Now().Add(daemonClientTimeout))
	if err := writeDaemonMessage(conn, daemonRequest{Method: "build", Entry: entry, Output: *output}); err != nil {
		fmt.Fprintf(stderr, "pebc daemon build: %v\n", err)
		return 1
	}
	var resp daemonResponse
	if err := readDaemonMessage(conn, &resp); err != nil {
		fmt.Fprintf(stderr, "pebc daemon build: %v\n", err)
		return 1
	}
	if resp.Error != "" {
		fmt.Fprintln(stderr, resp.Error)
	}
	if resp.Diagnostics != "" {
		fmt.Fprint(stderr, resp.Diagnostics)
	}
	if !resp.OK {
		return 1
	}
	fmt.Fprintln(stdout, resp.Output)
	return 0
}

// daemonStop implements `pebc daemon stop`: ask the running daemon to exit.
func daemonStop(args []string, stdout, stderr io.Writer) int {
	fs := newDaemonFlagSet(stderr)
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "pebc daemon stop: unexpected arguments")
		return 2
	}
	root, err := daemonRoot()
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon stop: %v\n", err)
		return 1
	}
	sockPath := daemonSocketPath(root)
	conn, err := net.DialTimeout("unix", sockPath, 2*time.Second)
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon stop: no daemon running for %s: %v\n", root, err)
		return 1
	}
	defer conn.Close()
	_ = conn.SetDeadline(time.Now().Add(5 * time.Second))
	if err := writeDaemonMessage(conn, daemonRequest{Method: "stop"}); err != nil {
		fmt.Fprintf(stderr, "pebc daemon stop: %v\n", err)
		return 1
	}
	var resp daemonResponse
	if err := readDaemonMessage(conn, &resp); err != nil {
		fmt.Fprintf(stderr, "pebc daemon stop: %v\n", err)
		return 1
	}
	fmt.Fprintln(stdout, "daemon stopped")
	return 0
}
