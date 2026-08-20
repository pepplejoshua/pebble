package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// buildPEBC compiles the pebc binary to a fresh temp path and returns it.
// This mirrors the intent of spawning a real OS subprocess for `pebc`
// (rather than calling run() in-process, which would hijack the test's own
// stdin/stdout).
func buildPEBC(t *testing.T) string {
	t.Helper()
	bin := filepath.Join(t.TempDir(), "pebc")
	modRoot := findModuleRoot(t)
	cmd := exec.Command("go", "build", "-o", bin, "./cmd/pebc")
	cmd.Dir = modRoot
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("building pebc: %v\n%s", err, out)
	}
	return bin
}

// findModuleRoot walks up from the working directory to the directory
// holding the module's go.mod.
func findModuleRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("cannot locate module root (go.mod)")
	return ""
}

// writeLSPFrame writes an LSP Content-Length framed message to w.
func writeLSPFrame(t *testing.T, w io.Writer, msg []byte) {
	t.Helper()
	frame := fmt.Sprintf("Content-Length: %d\r\n\r\n%s", len(msg), msg)
	if _, err := w.Write([]byte(frame)); err != nil {
		t.Fatalf("writing LSP frame: %v", err)
	}
}

// readLSPFrame reads a single LSP Content-Length framed message from r.
func readLSPFrame(t *testing.T, r *bufio.Reader) []byte {
	t.Helper()
	// Read headers until we see a blank line.
	var contentLength int = -1
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			t.Fatalf("reading LSP header: %v", err)
		}
		line = strings.TrimRight(line, "\r\n")
		if line == "" {
			break
		}
		if strings.HasPrefix(strings.ToLower(line), "content-length:") {
			var n int
			if _, err := fmt.Sscanf(line, "Content-Length: %d", &n); err != nil {
				t.Fatalf("parsing Content-Length %q: %v", line, err)
			}
			contentLength = n
		}
	}
	if contentLength < 0 {
		t.Fatal("LSP response had no Content-Length header")
	}
	body := make([]byte, contentLength)
	if _, err := io.ReadFull(r, body); err != nil {
		t.Fatalf("reading LSP body: %v", err)
	}
	return body
}

func TestLSPServerHandshakeSubprocess(t *testing.T) {
	bin := buildPEBC(t)

	cmd := exec.Command(bin, "lsp")
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("starting pebc lsp: %v", err)
	}
	defer func() {
		_ = cmd.Process.Kill()
	}()

	reader := bufio.NewReader(stdout)

	// 1. initialize
	initReq := []byte(`{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":123,"rootUri":null,"capabilities":{}}}`)
	writeLSPFrame(t, stdin, initReq)
	initBody := readLSPFrame(t, reader)

	var initResp struct {
		JSONRPC string          `json:"jsonrpc"`
		ID      int             `json:"id"`
		Result  json.RawMessage `json:"result"`
		Error   json.RawMessage `json:"error"`
	}
	if err := json.Unmarshal(initBody, &initResp); err != nil {
		t.Fatalf("parsing initialize response %s: %v", initBody, err)
	}
	if initResp.Error != nil {
		t.Fatalf("initialize returned error: %s", initResp.Error)
	}
	if initResp.Result == nil {
		t.Fatalf("initialize response missing result: %s", initBody)
	}
	var result struct {
		ServerInfo struct {
			Name string `json:"name"`
		} `json:"serverInfo"`
	}
	if err := json.Unmarshal(initResp.Result, &result); err != nil {
		t.Fatalf("parsing initialize result: %v", err)
	}
	if result.ServerInfo.Name != "pebc" {
		t.Fatalf("serverInfo.name = %q, want pebc", result.ServerInfo.Name)
	}

	// 2. shutdown
	shutReq := []byte(`{"jsonrpc":"2.0","id":2,"method":"shutdown"}`)
	writeLSPFrame(t, stdin, shutReq)
	shutBody := readLSPFrame(t, reader)
	var shutResp struct {
		Result json.RawMessage `json:"result"`
		Error  json.RawMessage `json:"error"`
	}
	if err := json.Unmarshal(shutBody, &shutResp); err != nil {
		t.Fatalf("parsing shutdown response %s: %v", shutBody, err)
	}
	if shutResp.Error != nil {
		t.Fatalf("shutdown returned error: %s", shutResp.Error)
	}
	if shutResp.Result == nil {
		t.Fatalf("shutdown response missing result: %s", shutBody)
	}

	// 3. exit notification (no id) -- must terminate the process.
	exitReq := []byte(`{"jsonrpc":"2.0","method":"exit"}`)
	writeLSPFrame(t, stdin, exitReq)

	// Confirm the subprocess actually terminates.
	done := make(chan error, 1)
	go func() { done <- cmd.Wait() }()
	select {
	case err := <-done:
		if err != nil {
			var exitErr *exec.ExitError
			if !asExitError(err, &exitErr) {
				t.Fatalf("pebc lsp exited with unexpected error: %v", err)
			}
		}
	case <-time.After(10 * time.Second):
		t.Fatal("pebc lsp did not terminate after exit notification")
	}
}

func asExitError(err error, target **exec.ExitError) bool {
	if e, ok := err.(*exec.ExitError); ok {
		*target = e
		return true
	}
	return false
}

// TestLSPGarbageInputDoesNotCrash confirms that sending garbage bytes to a
// fresh `pebc lsp` process's stdin does not crash it or hang forever: the
// LSP header framing should simply fail to parse a Content-Length, and the
// connection should remain alive (or close cleanly) rather than panic.
func TestLSPGarbageInputDoesNotCrash(t *testing.T) {
	bin := buildPEBC(t)

	cmd := exec.Command(bin, "lsp")
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("starting pebc lsp: %v", err)
	}
	defer func() {
		_ = cmd.Process.Kill()
	}()

	// Drain stdout so the subprocess's write buffer can never block it.
	go func() { _, _ = io.Copy(io.Discard, stdout) }()

	// Write pure garbage (not valid LSP framing).
	if _, err := stdin.Write([]byte("this is not valid lsp framing at all\x00\xff\xfe")); err != nil {
		t.Fatalf("writing garbage: %v", err)
	}

	// The process should NOT crash: if it crashed we'd expect an early exit.
	// Give it a moment, then confirm it is still running (alive) or has
	// exited cleanly without a panic. We verify it does not panic by checking
	// stderr is empty and the process is still alive shortly after.
	done := make(chan error, 1)
	go func() { done <- cmd.Wait() }()

	select {
	case <-done:
		// If it exited, it must be clean (no panic output on stderr).
		if stderr.Len() > 0 {
			t.Fatalf("pebc lsp crashed on garbage input; stderr: %s", stderr.String())
		}
	case <-time.After(500 * time.Millisecond):
		// Still alive and not crashed: good. Kill it for cleanup.
		_ = cmd.Process.Kill()
		<-done
		if stderr.Len() > 0 {
			t.Fatalf("pebc lsp produced stderr while handling garbage: %s", stderr.String())
		}
	}
}
