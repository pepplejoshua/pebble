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
		fmt.Fprintln(stderr, "pebc daemon: missing subcommand (start|build|ping|stop)")
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

		// A live daemon responds on the socket; otherwise the socket is
		// either absent or a stale leftover from a crashed daemon. Bind
		// fresh.
		if err := os.MkdirAll(filepath.Dir(sockPath), 0o755); err != nil {
			fmt.Fprintf(stderr, "pebc daemon start: cannot create %q: %v\n", filepath.Dir(sockPath), err)
			return 1
		}

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
}

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
	case "build":
		d.touch()
		if d.reexecIfStale() {
			// A stale binary restarts the daemon; tell the client to retry.
			_ = writeDaemonMessage(conn, daemonResponse{OK: false, Error: "daemon is restarting; please retry"})
			return
		}
		d.serveBuild(conn, req)
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
		stderr:     &diag,
	})
	if res.code != 0 {
		_ = writeDaemonMessage(conn, daemonResponse{OK: false, Diagnostics: res.diagnostics, Error: diag.String()})
		return
	}
	_ = writeDaemonMessage(conn, daemonResponse{OK: true, Output: res.binaryPath})
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
