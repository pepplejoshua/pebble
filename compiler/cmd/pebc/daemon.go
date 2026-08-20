package main

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/fsnotify/fsnotify"
)

// Daemon architecture
//
// `pebc daemon start` runs a long-lived process that owns compilation state
// for one project root. This slice implements only the process lifecycle:
// single-instance-per-project-root, idle-timeout self-shutdown, stale-binary
// self-restart, and a minimal build-request RPC. Every build request still
// runs the full pipeline (see compileOnce); the win at this slice is purely
// from reusing the loaded process, warmed runtime/cc caches, and the linked
// binary image instead of exec'ing a fresh process per build.
//
// Process model:
//   - The daemon binds a Unix domain socket at <root>/.pebble/daemon.sock
//     (see daemonSocketPath). Project root is the daemon's working directory
//     resolved to an absolute path; a client must run from the same root.
//   - Single instance is enforced by (a) probing the socket for a live
//     daemon and attaching/no-op'ing if one responds, and (b) relying on the
//     OS to reject a second bind of the same socket path (EADDRINUSE).
//   - Idle-timeout: if no request arrives within -idle-timeout, the daemon
//     removes its socket and exits 0.
//   - Stale-binary self-restart: on each request (and periodically) the
//     daemon recomputes a content hash of the running executable. If it
//     differs from the hash recorded at startup, the daemon re-execs itself
//     (a child `pebc daemon start` inheriting the bound listener file
//     descriptor), then the old process exits. Re-exec is used rather than
//     exit-and-wait-for-relaunch because `pebc daemon start` is a foreground
//     long-lived process with no supervising parent to relaunch it; handing
//     the listener fd to the child makes the restart atomic with no
//     socket-path race.

// defaultIdleTimeout is the idle time after which a daemon shuts itself down.
const defaultIdleTimeout = 30 * time.Minute

// staleCheckInterval is how often the daemon re-hashes its own executable.
const staleCheckInterval = 5 * time.Second

// fileChangeDebounce is how long the daemon waits for a watched file's events
// to quiet down before hashing it. A single logical save commonly produces
// several raw fsnotify events (and the file can be read mid-write), so events
// are coalesced per path and only the settled final state is ever hashed. This
// guarantees at most one real "changed" transition per logical edit.
const fileChangeDebounce = 75 * time.Millisecond

// daemonSocketDir is the project-local directory that holds daemon state.
const daemonSocketDir = ".pebble"

// daemonSocketFile is the daemon's Unix socket file within the socket dir.
const daemonSocketFile = "daemon.sock"

// daemonInheritFdEnv names the env var carrying an inherited listener fd.
const daemonInheritFdEnv = "PEBC_DAEMON_INHERIT_FD"

// daemonClientEnv marks a process that was relaunched by the daemon itself,
// so it must not re-announce "started" or remove the socket on shutdown.
const daemonReexecEnv = "PEBC_DAEMON_REEXEC"

// daemonSocketPath returns the daemon socket path for a project root.
func daemonSocketPath(root string) string {
	return filepath.Join(root, daemonSocketDir, daemonSocketFile)
}

// daemonOptions carries the runtime configuration for one daemon process.
type daemonOptions struct {
	root        string
	idleTimeout time.Duration
}

// runDaemon dispatches `pebc daemon <subcommand>`. It is invoked from run()
// before the one-shot flags are parsed.
func runDaemon(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "pebc daemon: missing subcommand (start|build|ping|stop|watch-status)")
		return 2
	}
	sub, rest := args[0], args[1:]
	switch sub {
	case "start":
		return daemonStart(rest, stdout, stderr)
	case "build":
		return daemonBuild(rest, stdout, stderr)
	case "ping":
		return daemonPing(rest, stdout, stderr)
	case "stop":
		return daemonStop(rest, stdout, stderr)
	case "watch-status":
		return daemonWatchStatus(rest, stdout, stderr)
	default:
		fmt.Fprintf(stderr, "pebc daemon: unknown subcommand %q\n", sub)
		return 2
	}
}

// daemonStart launches (or attaches to) the daemon process for the current
// project root.
func daemonStart(args []string, stdout, stderr io.Writer) int {
	idle := defaultIdleTimeout
	fs := newDaemonFlagSet(stderr)
	fs.DurationVar(&idle, "idle-timeout", defaultIdleTimeout, "shut down after this long without a build request")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "pebc daemon start: unexpected arguments")
		return 2
	}
	root, err := os.Getwd()
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon start: cannot determine working directory: %v\n", err)
		return 1
	}
	root, err = filepath.Abs(root)
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon start: cannot resolve working directory: %v\n", err)
		return 1
	}
	return serveDaemon(daemonOptions{root: root, idleTimeout: idle}, stdout, stderr)
}

// serveDaemon runs the daemon event loop. It first checks whether a daemon is
// already live for the project root and, if so, attaches/no-ops instead of
// spawning a duplicate.
func serveDaemon(opts daemonOptions, stdout, stderr io.Writer) int {
	sockPath := daemonSocketPath(opts.root)
	inherited := os.Getenv(daemonReexecEnv) != ""

	var listener net.Listener
	if inherited {
		// This process was re-exec'd by its predecessor and inherited the
		// bound listener fd. Adopt it directly; the socket file already
		// exists and must not be re-bound (a fresh bind would create a
		// throwaway listener whose close would unlink the path) nor removed
		// on shutdown (this is the continuation of the same instance).
		fdFile := os.NewFile(3, "daemon-listen")
		if fdFile == nil {
			fmt.Fprintln(stderr, "pebc daemon start: no inherited listener fd")
			return 1
		}
		lf, err := net.FileListener(fdFile)
		if err != nil {
			fmt.Fprintf(stderr, "pebc daemon start: cannot adopt inherited listener: %v\n", err)
			return 1
		}
		listener = lf
	} else {
		if live, _ := pingDaemon(sockPath); live {
			fmt.Fprintf(stdout, "pebc daemon: already running for %s; attaching to existing instance\n", opts.root)
			return 0
		}

		// A live daemon responds on the socket; otherwise the socket path is
		// either absent or a stale leftover from a crashed daemon (e.g.
		// kill -9, an OOM, or the machine sleeping mid-run never lets the
		// listener's own cleanup remove the file). net.Listen refuses to
		// bind a unix socket path that already exists on disk even when
		// nothing is listening on it, so a stale file left the daemon unable
		// to ever start again -- confirmed by reproducing directly: killing
		// a daemon and immediately restarting it failed with "address
		// already in use" even though pingDaemon correctly reported no live
		// daemon. Since the probe above already confirmed nothing is live,
		// clearing a stale file here is safe; the narrow window against a
		// concurrent starter is still covered by the post-Listen-failure
		// reprobe below.
		if err := os.MkdirAll(filepath.Dir(sockPath), 0o755); err != nil {
			fmt.Fprintf(stderr, "pebc daemon start: cannot create %q: %v\n", filepath.Dir(sockPath), err)
			return 1
		}
		_ = os.Remove(sockPath)

		l, err := net.Listen("unix", sockPath)
		if err != nil {
			// Another daemon won the bind race between our probe and now.
			if live, _ := pingDaemon(sockPath); live {
				fmt.Fprintf(stdout, "pebc daemon: already running for %s; attaching to existing instance\n", opts.root)
				return 0
			}
			fmt.Fprintf(stderr, "pebc daemon start: cannot bind socket %q: %v\n", sockPath, err)
			return 1
		}
		listener = l
	}

	hash, err := executableHash()
	if err != nil {
		fmt.Fprintf(stderr, "pebc daemon start: cannot hash own executable: %v\n", err)
		return 1
	}

	if !inherited {
		fmt.Fprintf(stdout, "pebc daemon: listening on %s (pid %d, idle timeout %s)\n", sockPath, os.Getpid(), opts.idleTimeout)
	} else {
		fmt.Fprintf(stdout, "pebc daemon: re-exec adopted listener (pid %d)\n", os.Getpid())
	}

	d := &daemon{
		opts:         opts,
		sockPath:     sockPath,
		listener:     listener,
		startupHash:  hash,
		lastActivity: time.Now(),
	}
	d.startWatching()
	d.run()
	if !d.handedOff {
		_ = os.Remove(sockPath)
	}
	return 0
}

// daemon is the long-lived lifecycle state machine.
type daemon struct {
	opts         daemonOptions
	sockPath     string
	listener     net.Listener
	startupHash  string
	mu           sync.Mutex
	lastActivity time.Time
	handedOff    bool

	// watchEvents carries the path of a .peb file whose content may have
	// changed, produced by the fsnotify goroutine and consumed by run().
	// Nil when the watcher failed to start; the run() select case then
	// simply never fires.
	watchEvents chan string
	watcher     *fsnotify.Watcher
	// fileHashes maps a tracked source path to its last-known SHA-256. The
	// tracked set is derived from the last build's resolved module graph
	// (see trackFiles), not a glob of every *.peb under the root, so the
	// daemon only watches files that actually participate in a build.
	fileHashes map[string]string
	// recent is a bounded, most-recent-first log of detected watch events,
	// exposed via the watch-status RPC for observability.
	recent []watchReport

	// debounce coalesces raw fsnotify events per path so detectChange only
	// ever observes a file's settled final state (see fileChangeDebounce).
	debounce *debouncer
}

// watchReport describes one content-change detection for observability.
type watchReport struct {
	Path string `json:"path"`
	// Kind is "changed" (content hash differed) or "noop" (identical rewrite
	// or touch with no real edit).
	Kind string `json:"kind"`
	Time string `json:"time"`
}

// watchReportCap bounds the number of recent watch reports retained.
const watchReportCap = 64

// run serves requests until an idle timeout or a stale-binary re-exec.
func (d *daemon) run() {
	acceptErr := make(chan error, 1)
	go func() {
		for {
			conn, err := d.listener.Accept()
			if err != nil {
				acceptErr <- err
				return
			}
			go d.handle(conn)
		}
	}()

	idleTicker := time.NewTicker(time.Second)
	defer idleTicker.Stop()
	staleTicker := time.NewTicker(staleCheckInterval)
	defer staleTicker.Stop()

	for {
		select {
		case err := <-acceptErr:
			if !isClosedListenerErr(err) {
				fmt.Fprintf(os.Stderr, "pebc daemon: accept failed: %v\n", err)
			}
			return
		case <-idleTicker.C:
			d.mu.Lock()
			idle := time.Since(d.lastActivity)
			d.mu.Unlock()
			if idle >= d.opts.idleTimeout {
				fmt.Fprintf(os.Stderr, "pebc daemon: idle for %s; shutting down\n", idle)
				_ = d.listener.Close()
				return
			}
		case <-staleTicker.C:
			if d.reexecIfStale() {
				return
			}
		case path := <-d.watchEvents:
			d.touch()
			d.detectChange(path)
		}
	}
}

// handle serves one client connection.
func (d *daemon) handle(conn net.Conn) {
	defer conn.Close()
	d.touch()
	var req daemonRequest
	if err := readDaemonMessage(conn, &req); err != nil {
		return
	}
	switch req.Method {
	case "ping":
		_ = writeDaemonMessage(conn, daemonResponse{OK: true})
	case "watch-status":
		files, recent := d.watchStatusSnapshot()
		_ = writeDaemonMessage(conn, daemonResponse{OK: true, WatchFiles: files, WatchEvents: recent})
	case "build":
		d.touch()
		if d.reexecIfStale() {
			// A stale binary restarts the daemon; tell the client to retry.
			_ = writeDaemonMessage(conn, daemonResponse{OK: false, Error: "daemon is restarting; please retry"})
			return
		}
		d.serveBuild(conn, req)
	case "hover":
		d.touch()
		if d.reexecIfStale() {
			_ = writeDaemonMessage(conn, daemonResponse{OK: false, Error: "daemon is restarting; please retry"})
			return
		}
		d.serveHover(conn, req)
	case "stop":
		_ = writeDaemonMessage(conn, daemonResponse{OK: true})
		_ = d.listener.Close()
	default:
		_ = writeDaemonMessage(conn, daemonResponse{OK: false, Error: "unknown method: " + req.Method})
	}
}

// touch records that the daemon has seen activity.
func (d *daemon) touch() {
	d.mu.Lock()
	d.lastActivity = time.Now()
	d.mu.Unlock()
}

// startWatching sets up the fsnotify watcher for the project root and begins
// consuming its events. It is non-fatal: if the watcher cannot be started the
// daemon keeps running without change detection.
func (d *daemon) startWatching() {
	w, err := fsnotify.NewWatcher()
	if err != nil {
		fmt.Fprintf(os.Stderr, "pebc daemon: cannot start file watcher: %v (change detection disabled)\n", err)
		return
	}
	d.watcher = w
	d.fileHashes = map[string]string{}
	d.watchEvents = make(chan string, 256)
	d.recent = make([]watchReport, 0, watchReportCap)
	d.debounce = newDebouncer(fileChangeDebounce, func(path string) {
		select {
		case d.watchEvents <- path:
		default:
			// Drop events if the daemon event loop is saturated rather than
			// block the fsnotify goroutine.
		}
	})
	if err := d.watchTree(d.opts.root); err != nil {
		fmt.Fprintf(os.Stderr, "pebc daemon: cannot watch project root: %v (change detection disabled)\n", err)
		w.Close()
		d.watcher = nil
		d.watchEvents = nil
		return
	}
	go d.watchLoop()
}

// watchTree recursively adds dir and every subdirectory to the watcher.
// fsnotify does not recurse automatically, so the tree must be walked and
// each directory watched explicitly. New subdirectories created after the
// watch starts are handled in watchLoop by re-walking them on Create.
func (d *daemon) watchTree(root string) error {
	return filepath.WalkDir(root, func(path string, de os.DirEntry, err error) error {
		if err != nil {
			return nil
		}
		if de.IsDir() {
			if err := d.watcher.Add(path); err != nil {
				// Best effort: a directory that cannot be watched (e.g. a
				// permission issue) must not abort the whole walk.
				return nil
			}
		}
		return nil
	})
}

// watchLoop forwards fsnotify events to the daemon's event loop. It also
// watches newly created directories so .peb files added after startup are
// still observed.
func (d *daemon) watchLoop() {
	for {
		select {
		case ev, ok := <-d.watcher.Events:
			if !ok {
				return
			}
			if ev.Op&fsnotify.Create != 0 {
				if fi, err := os.Stat(ev.Name); err == nil && fi.IsDir() {
					_ = d.watchTree(ev.Name)
				}
			}
			if filepath.Ext(ev.Name) != ".peb" {
				continue
			}
			// Coalesce rapid events for the same path into a single delivery
			// after a quiet period, so detectChange never hashes a file while
			// it is mid-write (the source of spurious duplicate "changed"
			// transitions for one logical edit).
			d.debounce.note(ev.Name)
		case err, ok := <-d.watcher.Errors:
			if !ok {
				return
			}
			fmt.Fprintf(os.Stderr, "pebc daemon: watch error: %v\n", err)
		}
	}
}

// debouncer coalesces rapid per-key events: note(key) starts (or extends) a
// quiet-period timer for that key, and only when the timer actually fires is
// the key delivered to fire. A burst of notes for the same key therefore
// produces exactly one delivery, made only after the burst has quieted down.
type debouncer struct {
	mu       sync.Mutex
	timers   map[string]*time.Timer
	interval time.Duration
	// after is the timer scheduler; time.AfterFunc in production, a fake in
	// tests that never runs on its own.
	after func(time.Duration, func()) *time.Timer
	fire  func(key string)
}

// newDebouncer returns a debouncer that waits interval of quiet before
// delivering a noted key to fire.
func newDebouncer(interval time.Duration, fire func(string)) *debouncer {
	return &debouncer{
		timers:   map[string]*time.Timer{},
		interval: interval,
		after:    time.AfterFunc,
		fire:     fire,
	}
}

// note records an event for key. If a timer for key is already pending it is
// reset (the quiet window restarts) rather than delivering immediately; if
// none exists a new timer is started.
func (b *debouncer) note(key string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if t, ok := b.timers[key]; ok {
		t.Reset(b.interval)
		return
	}
	var t *time.Timer
	t = b.after(b.interval, func() {
		b.mu.Lock()
		if b.timers[key] == t {
			delete(b.timers, key)
			b.mu.Unlock()
			b.fire(key)
			return
		}
		b.mu.Unlock()
	})
	b.timers[key] = t
}

// trackFiles re-derives the tracked-file set from the module graph of the
// most recent build, recording each file's current content hash. Files that
// leave the graph are dropped; new files are added with their current hash.
func (d *daemon) trackFiles(paths []string) {
	if d.fileHashes == nil {
		return
	}
	d.mu.Lock()
	defer d.mu.Unlock()
	d.trackFilesLocked(paths)
}

// trackFilesLocked is trackFiles assuming d.mu is already held.
func (d *daemon) trackFilesLocked(paths []string) {
	next := make(map[string]string, len(paths))
	for _, p := range paths {
		abs, err := filepath.Abs(p)
		if err != nil {
			continue
		}
		abs = filepath.Clean(abs)
		hash, err := fileSHA256(abs)
		if err != nil {
			continue
		}
		next[abs] = hash
	}
	d.fileHashes = next
}

// detectChange recomputes the content hash of a watched file and records
// whether it actually changed. Files not in the tracked set are ignored so a
// touch of an unrelated file never disturbs tracked state. This slice only
// detects and reports; it never skips any real parse/check work.
func (d *daemon) detectChange(path string) {
	abs, err := filepath.Abs(path)
	if err != nil {
		return
	}
	abs = filepath.Clean(abs)
	d.mu.Lock()
	defer d.mu.Unlock()
	last, tracked := d.fileHashes[abs]
	if !tracked {
		return
	}
	hash, err := fileSHA256(abs)
	if err != nil {
		return
	}
	if hash == last {
		d.recordLocked(watchReport{Path: abs, Kind: "noop", Time: time.Now().Format(time.RFC3339)})
		return
	}
	d.fileHashes[abs] = hash
	d.recordLocked(watchReport{Path: abs, Kind: "changed", Time: time.Now().Format(time.RFC3339)})
}

// recordLocked appends a watch report, keeping the log bounded. Callers must
// hold d.mu.
func (d *daemon) recordLocked(r watchReport) {
	d.recent = append(d.recent, r)
	if len(d.recent) > watchReportCap {
		d.recent = append([]watchReport(nil), d.recent[len(d.recent)-watchReportCap:]...)
	}
}

// watchStatusSnapshot returns the current tracked files (path -> hash) and the
// recent watch reports, newest first.
func (d *daemon) watchStatusSnapshot() (map[string]string, []watchReport) {
	d.mu.Lock()
	defer d.mu.Unlock()
	files := make(map[string]string, len(d.fileHashes))
	for k, v := range d.fileHashes {
		files[k] = v
	}
	recent := append([]watchReport(nil), d.recent...)
	for i, j := 0, len(recent)-1; i < j; i, j = i+1, j-1 {
		recent[i], recent[j] = recent[j], recent[i]
	}
	return files, recent
}

// fileSHA256 returns the hex SHA-256 of a file's contents.
func fileSHA256(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}

// serveBuild runs the full pipeline for one entry file and writes the result.
func (d *daemon) serveBuild(conn net.Conn, req daemonRequest) {
	// Serialize compilations; compileOnce is not required to be concurrent.
	d.mu.Lock()
	defer d.mu.Unlock()

	// Output path is validated here so a bad -o fails the request cleanly.
	output := req.Output
	if output == "" {
		output = defaultBinaryPath(filepath.Base(req.Entry))
	}
	output, err := filepath.Abs(output)
	if err != nil {
		_ = writeDaemonMessage(conn, daemonResponse{OK: false, Error: "cannot resolve output path: " + err.Error()})
		return
	}

	var diag strings.Builder
	res := compileOnce(compileRequest{
		mode:       modeBuild,
		entryPath:  req.Entry,
		outputPath: output,
		trackFiles: true,
		stderr:     &diag,
	})
	if res.files != nil {
		d.trackFilesLocked(res.files)
	}
	if res.code != 0 {
		// compileOnce already renders diagnostics into res.diagnostics when a
		// full diagnostic set exists; diag (the raw stderr capture) then holds
		// that exact same text, so echoing both back would print it twice.
		// diag only carries independent content (e.g. "cannot resolve entry")
		// on the fatal paths where res.diagnostics is empty.
		errMsg := diag.String()
		if res.diagnostics != "" {
			errMsg = ""
		}
		_ = writeDaemonMessage(conn, daemonResponse{OK: false, Diagnostics: res.diagnostics, Error: errMsg, StructuredDiagnostics: res.structuredDiagnostics})
		return
	}
	_ = writeDaemonMessage(conn, daemonResponse{OK: true, Output: res.binaryPath, StructuredDiagnostics: res.structuredDiagnostics})
}

// serveHover answers a read-only type query at a source offset: it runs the
// same fresh full check a build uses (there is no warm checked state to query
// yet) and returns the rendered type at the requested byte offset, or "" when
// nothing useful is there.
func (d *daemon) serveHover(conn net.Conn, req daemonRequest) {
	// Serialize compilations; buildProgram is not required to be concurrent.
	d.mu.Lock()
	defer d.mu.Unlock()

	typ := hoverTypeAtOffset(req.Entry, req.Offset)
	_ = writeDaemonMessage(conn, daemonResponse{OK: true, Hover: typ})
}

// reexecIfStale restarts the daemon when the running executable has changed.
// It returns true when the daemon should stop serving (a restart was
// triggered or the executable is unreadable).
func (d *daemon) reexecIfStale() bool {
	hash, err := executableHash()
	if err != nil {
		fmt.Fprintf(os.Stderr, "pebc daemon: cannot hash own executable: %v\n", err)
		return true
	}
	if hash == d.startupHash {
		return false
	}
	fmt.Fprintf(os.Stderr, "pebc daemon: executable changed; restarting\n")
	if err := d.reexec(); err != nil {
		fmt.Fprintf(os.Stderr, "pebc daemon: restart failed: %v\n", err)
		// Cannot safely serve a stale binary; exit.
		return true
	}
	return true
}

// reexec spawns a fresh copy of this daemon inheriting the listener fd, then
// closes our listener so the child owns the socket exclusively.
func (d *daemon) reexec() error {
	exe, err := os.Executable()
	if err != nil {
		return err
	}
	ul, ok := d.listener.(*net.UnixListener)
	if !ok {
		return fmt.Errorf("listener is not a unix socket")
	}
	f, err := ul.File()
	if err != nil {
		return err
	}
	cmd := exec.Command(exe, "daemon", "start",
		"-idle-timeout", d.opts.idleTimeout.String())
	cmd.Env = append(os.Environ(),
		daemonInheritFdEnv+"=3",
		daemonReexecEnv+"=1",
	)
	cmd.ExtraFiles = []*os.File{f}
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Stdin = nil
	if err := cmd.Start(); err != nil {
		f.Close()
		return err
	}
	// This process successfully spawned a replacement that inherited the
	// listener fd and now owns the socket path. Record the hand-off so the
	// parent does not unlink the socket the child is serving on.
	d.handedOff = true
	// Detach from the child; it continues after we exit.
	_ = cmd.Process.Release()
	// The child inherited the raw listener fd and is serving on the same
	// socket path. Do not let this Close unlink the socket file out from
	// under the child; it owns the path now and will clean it up on its own
	// final shutdown.
	ul.SetUnlinkOnClose(false)
	_ = ul.Close()
	return nil
}

// executableHash returns a content hash of the running executable.
func executableHash() (string, error) {
	exe, err := os.Executable()
	if err != nil {
		return "", err
	}
	f, err := os.Open(exe)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}

func isClosedListenerErr(err error) bool {
	return err != nil && (strings.Contains(err.Error(), "use of closed network connection") ||
		strings.Contains(err.Error(), "closed"))
}
