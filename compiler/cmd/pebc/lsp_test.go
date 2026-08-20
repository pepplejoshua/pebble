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

// readLSPFrameWithTimeout reads one frame in a goroutine and fails the test
// if none arrives within timeout, rather than blocking forever.
func readLSPFrameWithTimeout(t *testing.T, r *bufio.Reader, timeout time.Duration) []byte {
	t.Helper()
	ch := make(chan []byte, 1)
	go func() { ch <- readLSPFrame(t, r) }()
	select {
	case body := <-ch:
		return body
	case <-time.After(timeout):
		t.Fatalf("no LSP frame arrived within %s", timeout)
		return nil
	}
}

// TestLSPDiagnosticsOnSave exercises the full 21.4b flow against a real
// scratch project: initialize with a real rootUri/workspaceFolders, open and
// save a file with a real, precisely-known type error, and confirm the
// server's unsolicited textDocument/publishDiagnostics notification reports
// it at the exact right position. Then fixes the file, saves again, and
// confirms an empty diagnostics array clears it.
func TestLSPDiagnosticsOnSave(t *testing.T) {
	bin := buildPEBC(t)

	// A short, shallow root -- NOT t.TempDir() directly. Go's testing package
	// nests t.TempDir() under a per-test, per-call directory
	// (".../TestName<random>/002/"), which for a Unix domain socket path
	// (root + "/.pebble/daemon.sock") can exceed macOS's 104-byte
	// sockaddr_un.sun_path limit -- confirmed directly during this slice's
	// verification (a real 107-byte path silently failed to bind, and
	// ensureDaemonForRoot's daemon never came up, timing out after 5s and
	// falling back to empty diagnostics -- not a bug in the diagnostics
	// logic itself, every other direct reproduction proved that correct).
	// This is a real, separate robustness gap in the daemon's socket-path
	// construction (21.1a) worth a dedicated follow-up for projects with
	// long/deep paths; out of scope for this slice, so the test sidesteps
	// it with a short root instead of exercising the bug.
	root, err := os.MkdirTemp("", "pebclsp")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })
	mainPath := filepath.Join(root, "main.peb")
	brokenSrc := "fn main() int {\n  var x i32 = \"not a number\";\n  return 0;\n}\n"
	fixedSrc := "fn main() int {\n  var x i32 = 42;\n  return 0;\n}\n"
	if err := os.WriteFile(mainPath, []byte(brokenSrc), 0o644); err != nil {
		t.Fatal(err)
	}
	docURI := "file://" + mainPath
	rootURI := "file://" + root

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
		_ = cmd.Wait()
		// Best-effort: stop any daemon this test started for the scratch root.
		stopCmd := exec.Command(bin, "daemon", "stop")
		stopCmd.Dir = root
		_ = stopCmd.Run()
		t.Logf("lsp stderr: %s", stderr.String())
	}()

	reader := bufio.NewReader(stdout)

	initReq := fmt.Sprintf(`{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootUri":%q,"workspaceFolders":[{"uri":%q,"name":"scratch"}],"capabilities":{}}}`, rootURI, rootURI)
	writeLSPFrame(t, stdin, []byte(initReq))
	initBody := readLSPFrameWithTimeout(t, reader, 10*time.Second)
	t.Logf("initialize response: %s", initBody)

	openReq := fmt.Sprintf(`{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":%q,"languageId":"pebble","version":1,"text":%q}}}`, docURI, brokenSrc)
	writeLSPFrame(t, stdin, []byte(openReq))
	// A real editor never fires didOpen immediately followed by didSave with
	// zero gap (didOpen happens when a file is first opened, typically
	// seconds to minutes before any save) -- but sending them back-to-back
	// with no gap at all can race go.lsp.dev/jsonrpc2's stream reader into
	// silently dropping the second notification's dispatch (confirmed via
	// direct reproduction during this slice's verification; see the KNOWN
	// ISSUE note at the top of lsp.go). This small delay keeps the test
	// realistic and reliable rather than working around a library race this
	// slice isn't scoped to fix.
	time.Sleep(200 * time.Millisecond)
	saveReq := fmt.Sprintf(`{"jsonrpc":"2.0","method":"textDocument/didSave","params":{"textDocument":{"uri":%q}}}`, docURI)
	writeLSPFrame(t, stdin, []byte(saveReq))

	type publishParams struct {
		URI         string `json:"uri"`
		Diagnostics []struct {
			Range struct {
				Start struct {
					Line      int `json:"line"`
					Character int `json:"character"`
				} `json:"start"`
				End struct {
					Line      int `json:"line"`
					Character int `json:"character"`
				} `json:"end"`
			} `json:"range"`
			Severity int    `json:"severity"`
			Code     string `json:"code"`
			Message  string `json:"message"`
		} `json:"diagnostics"`
	}

	readPublish := func() publishParams {
		t.Helper()
		for i := 0; i < 10; i++ {
			body := readLSPFrameWithTimeout(t, reader, 20*time.Second)
			var msg struct {
				Method string          `json:"method"`
				Params json.RawMessage `json:"params"`
			}
			if err := json.Unmarshal(body, &msg); err != nil {
				t.Fatalf("parsing frame %s: %v", body, err)
			}
			if msg.Method != "textDocument/publishDiagnostics" {
				t.Logf("skipping non-publishDiagnostics frame: %s", body)
				continue
			}
			var p publishParams
			if err := json.Unmarshal(msg.Params, &p); err != nil {
				t.Fatalf("parsing publishDiagnostics params %s: %v", msg.Params, err)
			}
			return p
		}
		t.Fatal("no publishDiagnostics notification arrived")
		return publishParams{}
	}

	brokenDiag := readPublish()
	t.Logf("diagnostics for broken file: %+v", brokenDiag)
	if brokenDiag.URI != docURI {
		t.Fatalf("URI = %q, want %q", brokenDiag.URI, docURI)
	}
	if len(brokenDiag.Diagnostics) != 1 {
		t.Fatalf("diagnostics = %d, want exactly 1: %+v", len(brokenDiag.Diagnostics), brokenDiag.Diagnostics)
	}
	d := brokenDiag.Diagnostics[0]
	// The error is on line 2, column 15 (1-based) -- 0-based: line 1, char 14.
	if d.Range.Start.Line != 1 || d.Range.Start.Character != 14 {
		t.Fatalf("start position = (%d,%d), want (1,14)", d.Range.Start.Line, d.Range.Start.Character)
	}
	if !strings.Contains(d.Message, "cannot unify") {
		t.Fatalf("message = %q, want it to mention the real type error", d.Message)
	}

	// Fix the file and save again -- confirm diagnostics clear to empty.
	if err := os.WriteFile(mainPath, []byte(fixedSrc), 0o644); err != nil {
		t.Fatal(err)
	}
	writeLSPFrame(t, stdin, []byte(saveReq))
	fixedDiag := readPublish()
	t.Logf("diagnostics after fix: %+v", fixedDiag)
	if len(fixedDiag.Diagnostics) != 0 {
		t.Fatalf("diagnostics after fix = %+v, want empty (cleared)", fixedDiag.Diagnostics)
	}
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
