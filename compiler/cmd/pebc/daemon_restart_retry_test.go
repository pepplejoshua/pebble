package main

import (
	"net"
	"os"
	"path/filepath"
	"testing"
	"time"
)

// TestDaemonRPCRetriesOnRestartingError simulates a stale-binary self-restart
// (reexecIfStale in daemon.go) without needing a real rebuild: a first fake
// listener answers exactly one request with daemonRestartingError and then
// closes (mirroring the old daemon's listener closing during reexec), and a
// second fake listener on the SAME socket path comes up shortly after
// (mirroring the re-exec'd child binding the same socket) and answers for
// real. daemonRPCForRoot must transparently retry and return the real
// daemonResponse from the second listener, not the restarting error.
func TestDaemonRPCRetriesOnRestartingError(t *testing.T) {
	// A short-lived unix socket path (not t.TempDir(), whose test-name-based
	// nesting can exceed the ~104-byte sun_path limit on macOS).
	dir, err := os.MkdirTemp("", "pebcd")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	// Redirect the shared daemon-socket state directory into this test's own
	// scratch dir -- daemonSocketPath now resolves to a shared OS cache
	// directory outside any project, so without this override the test would
	// write a real socket file into the developer's actual cache directory.
	stateDir := filepath.Join(dir, "state")
	origStateDir := daemonStateDir
	daemonStateDir = func() (string, error) {
		if err := os.MkdirAll(stateDir, 0o755); err != nil {
			return "", err
		}
		return stateDir, nil
	}
	t.Cleanup(func() { daemonStateDir = origStateDir })
	root := filepath.Join(dir, "r")
	if err := os.MkdirAll(root, 0o755); err != nil {
		t.Fatal(err)
	}
	sockPath := daemonSocketPath(root)

	ln1, err := net.Listen("unix", sockPath)
	if err != nil {
		t.Fatal(err)
	}
	go func() {
		conn, err := ln1.Accept()
		if err != nil {
			return
		}
		var req daemonRequest
		_ = readDaemonMessage(conn, &req)
		_ = writeDaemonMessage(conn, daemonResponse{OK: false, Error: daemonRestartingError})
		conn.Close()
		ln1.Close()
		_ = os.Remove(sockPath)

		// Simulate the brief gap while the re-exec'd child spawns and binds.
		time.Sleep(150 * time.Millisecond)

		ln2, err := net.Listen("unix", sockPath)
		if err != nil {
			return
		}
		defer ln2.Close()
		// waitForDaemon's pingDaemon probe consumes one connection before the
		// real retried request arrives on a second one -- accept in a loop and
		// answer each on its own terms (a "ping" method gets OK:true, anything
		// else gets the real answer), rather than assuming exactly one client.
		for i := 0; i < 2; i++ {
			conn2, err := ln2.Accept()
			if err != nil {
				return
			}
			var req2 daemonRequest
			_ = readDaemonMessage(conn2, &req2)
			if req2.Method == "ping" {
				_ = writeDaemonMessage(conn2, daemonResponse{OK: true})
			} else {
				_ = writeDaemonMessage(conn2, daemonResponse{OK: true, Hover: "real answer"})
			}
			conn2.Close()
		}
	}()

	resp, err := daemonRPCForRoot(root, "hover", daemonRequest{Entry: "main.peb", Offset: 0})
	if err != nil {
		t.Fatalf("daemonRPCForRoot returned error: %v", err)
	}
	if !resp.OK {
		t.Fatalf("expected OK response after retry, got %+v", resp)
	}
	if resp.Hover != "real answer" {
		t.Fatalf("expected retried response, got %+v", resp)
	}
}
