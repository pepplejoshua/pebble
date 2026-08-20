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

// TestLSPHover exercises the 21.4c hover flow against a real scratch project:
// initialize with a real rootUri, open a file with a variable of a known type,
// send a real textDocument/hover REQUEST at the position of the literal, and
// read the RESPONSE (a request/response, not an async notification, so we read
// it directly) confirming it reports the exact type "i32". It also hovers over
// whitespace (no type info) and confirms a clean nil result, not an error or
// crash.
func TestLSPHover(t *testing.T) {
	bin := buildPEBC(t)

	// Short, shallow root -- NOT t.TempDir() directly (see the documented
	// 104-byte Unix socket path limit note in TestLSPDiagnosticsOnSave).
	root, err := os.MkdirTemp("", "pebclsp")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })
	mainPath := filepath.Join(root, "main.peb")
	// A complete, type-checking program: hover needs a successful full check
	// (the daemon has no warm state, so it re-checks the whole file). main must
	// return int (the entry-point requirement), and the literal 42 is i32.
	src := "fn main() int {\n  var x i32 = 42;\n  return 0;\n}\n"
	if err := os.WriteFile(mainPath, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	docURI := "file://" + mainPath
	rootURI := "file://" + root

	// Compute the 0-based LSP line/character of the literal "42" by its byte
	// offset in the source.
	litIdx := strings.Index(src, "42")
	if litIdx < 0 {
		t.Fatal("test fixture missing literal 42")
	}
	before := src[:litIdx]
	litLine := strings.Count(before, "\n")
	lastNL := strings.LastIndex(before, "\n")
	litChar := litIdx - lastNL - 1

	// Whitespace: the space between "var" and "x" on line 1 (0-based). Hovering
	// there must yield a clean nil result, not an error or crash.
	wsIdx := strings.Index(src, "var ")
	if wsIdx < 0 {
		t.Fatal("test fixture missing 'var '")
	}
	wsOffset := wsIdx + 3 // the space immediately after "var"
	wsBefore := src[:wsOffset]
	wsLine := strings.Count(wsBefore, "\n")
	wsLastNL := strings.LastIndex(wsBefore, "\n")
	wsChar := wsOffset - wsLastNL - 1

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
		stopCmd := exec.Command(bin, "daemon", "stop")
		stopCmd.Dir = root
		_ = stopCmd.Run()
		t.Logf("lsp stderr: %s", stderr.String())
	}()

	reader := bufio.NewReader(stdout)

	initReq := fmt.Sprintf(`{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootUri":%q,"workspaceFolders":[{"uri":%q,"name":"scratch"}],"capabilities":{}}}`, rootURI, rootURI)
	writeLSPFrame(t, stdin, []byte(initReq))
	readLSPFrameWithTimeout(t, reader, 10*time.Second)

	// open the document so the server tracks it as open.
	openReq := fmt.Sprintf(`{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":%q,"languageId":"pebble","version":1,"text":%q}}}`, docURI, src)
	writeLSPFrame(t, stdin, []byte(openReq))
	time.Sleep(200 * time.Millisecond)

	// A real hover REQUEST at the literal "42".
	hoverReq := fmt.Sprintf(`{"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{"textDocument":{"uri":%q},"position":{"line":%d,"character":%d}}}`, docURI, litLine, litChar)
	writeLSPFrame(t, stdin, []byte(hoverReq))
	hoverBody := readLSPFrameWithTimeout(t, reader, 20*time.Second)

	var hoverResp struct {
		ID     int `json:"id"`
		Result struct {
			Contents struct {
				Kind  string `json:"kind"`
				Value string `json:"value"`
			} `json:"contents"`
		} `json:"result"`
		Error json.RawMessage `json:"error"`
	}
	if err := json.Unmarshal(hoverBody, &hoverResp); err != nil {
		t.Fatalf("parsing hover response %s: %v", hoverBody, err)
	}
	if hoverResp.Error != nil {
		t.Fatalf("hover returned error: %s (body %s)", hoverResp.Error, hoverBody)
	}
	if hoverResp.ID != 2 {
		t.Fatalf("hover response id = %d, want 2 (body %s)", hoverResp.ID, hoverBody)
	}
	if hoverResp.Result.Contents.Value != "i32" {
		t.Fatalf("hover reported type %q, want \"i32\" (body %s)", hoverResp.Result.Contents.Value, hoverBody)
	}
	t.Logf("hover at literal 42 reported: %q", hoverResp.Result.Contents.Value)

	// Hover over whitespace: must be a clean nil result, not an error/crash.
	wsReq := fmt.Sprintf(`{"jsonrpc":"2.0","id":3,"method":"textDocument/hover","params":{"textDocument":{"uri":%q},"position":{"line":%d,"character":%d}}}`, docURI, wsLine, wsChar)
	writeLSPFrame(t, stdin, []byte(wsReq))
	wsBody := readLSPFrameWithTimeout(t, reader, 20*time.Second)
	var wsResp struct {
		ID     int             `json:"id"`
		Result json.RawMessage `json:"result"`
		Error  json.RawMessage `json:"error"`
	}
	if err := json.Unmarshal(wsBody, &wsResp); err != nil {
		t.Fatalf("parsing whitespace hover response %s: %v", wsBody, err)
	}
	if wsResp.Error != nil {
		t.Fatalf("whitespace hover returned error: %s (body %s)", wsResp.Error, wsBody)
	}
	if wsResp.ID != 3 {
		t.Fatalf("whitespace hover response id = %d, want 3 (body %s)", wsResp.ID, wsBody)
	}
	if string(wsResp.Result) != "null" {
		t.Fatalf("whitespace hover result = %s, want null (body %s)", wsResp.Result, wsBody)
	}
	t.Logf("hover over whitespace correctly returned null result")

	// Shut the server down cleanly.
	shutReq := []byte(`{"jsonrpc":"2.0","id":4,"method":"shutdown"}`)
	writeLSPFrame(t, stdin, shutReq)
	readLSPFrameWithTimeout(t, reader, 10*time.Second)
	writeLSPFrame(t, stdin, []byte(`{"jsonrpc":"2.0","method":"exit"}`))
}

// computeLSPPos returns the 0-based (line, character) of the byte offset of
// sub within src, emulating how the LSP server converts a position back to a
// byte offset (line-start byte + character) for ASCII content with no tabs.
func computeLSPPos(t *testing.T, src, sub string) (int, int) {
	t.Helper()
	off := strings.Index(src, sub)
	if off < 0 {
		t.Fatalf("fixture missing %q", sub)
	}
	before := src[:off]
	line := strings.Count(before, "\n")
	lastNL := strings.LastIndex(before, "\n")
	ch := off - lastNL - 1
	return line, ch
}

// hoverAndExpect sends a real textDocument/hover REQUEST at the given
// 0-based line/character and asserts the response reports exactly wantType.
func hoverAndExpect(t *testing.T, stdin io.WriteCloser, reader *bufio.Reader, docURI string, id, line, ch int, wantType string) {
	t.Helper()
	req := fmt.Sprintf(`{"jsonrpc":"2.0","id":%d,"method":"textDocument/hover","params":{"textDocument":{"uri":%q},"position":{"line":%d,"character":%d}}}`, id, docURI, line, ch)
	writeLSPFrame(t, stdin, []byte(req))
	body := readLSPFrameWithTimeout(t, reader, 20*time.Second)
	var resp struct {
		ID     int `json:"id"`
		Result struct {
			Contents struct {
				Kind  string `json:"kind"`
				Value string `json:"value"`
			} `json:"contents"`
		} `json:"result"`
		Error json.RawMessage `json:"error"`
	}
	if err := json.Unmarshal(body, &resp); err != nil {
		t.Fatalf("parsing hover response %s: %v", body, err)
	}
	if resp.Error != nil {
		t.Fatalf("hover returned error: %s (body %s)", resp.Error, body)
	}
	if resp.ID != id {
		t.Fatalf("hover response id = %d, want %d (body %s)", resp.ID, id, body)
	}
	if resp.Result.Contents.Value != wantType {
		t.Fatalf("hover reported type %q, want %q (body %s)", resp.Result.Contents.Value, wantType, body)
	}
	t.Logf("hover at (%d,%d) reported: %q", line, ch, resp.Result.Contents.Value)
}

// TestLSPHoverVariableReference exercises the far more common real-world hover
// case: hovering over a variable NAME REFERENCE (a genuine read of the
// variable, not its declaration or a literal) and confirming the resolved
// checked type comes back. Uses a type-correct program of the same shape as
// the 21.4c report's reproduction: a variable declared once and referenced
// twice inside a binary expression, plus a reference to a second variable.
// All three references resolve through a real LSP client round-trip.
func TestLSPHoverVariableReference(t *testing.T) {
	bin := buildPEBC(t)

	root, err := os.MkdirTemp("", "pebclsp")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })
	mainPath := filepath.Join(root, "main.peb")
	// Type-correct variant: entry returns int, so the i32-typed `doubled`
	// needs an explicit `as int` cast on return. `count` and `doubled` are
	// both declared as i32, so their references must report "i32".
	src := "fn main() int {\n  var count i32 = 99;\n  var doubled i32 = count + count;\n  return doubled as int;\n}\n"
	if err := os.WriteFile(mainPath, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	docURI := "file://" + mainPath
	rootURI := "file://" + root

	// Positions of the three references we exercise.
	firstCountLine, firstCountCh := computeLSPPos(t, src, "count + count")
	firstCountCh += len("count") // middle of the first "count" operand
	secondCountLine, secondCountCh := computeLSPPos(t, src, "+ count;")
	secondCountCh += len("+ ") // start of the second "count" operand
	doubledRefLine, doubledRefCh := computeLSPPos(t, src, "return doubled")
	doubledRefCh += len("return ") // middle of "doubled" in the return

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
		stopCmd := exec.Command(bin, "daemon", "stop")
		stopCmd.Dir = root
		_ = stopCmd.Run()
		t.Logf("lsp stderr: %s", stderr.String())
	}()

	reader := bufio.NewReader(stdout)

	initReq := fmt.Sprintf(`{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootUri":%q,"workspaceFolders":[{"uri":%q,"name":"scratch"}],"capabilities":{}}}`, rootURI, rootURI)
	writeLSPFrame(t, stdin, []byte(initReq))
	readLSPFrameWithTimeout(t, reader, 10*time.Second)

	openReq := fmt.Sprintf(`{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":%q,"languageId":"pebble","version":1,"text":%q}}}`, docURI, src)
	writeLSPFrame(t, stdin, []byte(openReq))
	time.Sleep(200 * time.Millisecond)

	// First reference to `count` (left operand of the binary expression).
	hoverAndExpect(t, stdin, reader, docURI, 2, firstCountLine, firstCountCh, "var count: i32")
	// Second (distinct syntax node) reference to the same variable.
	hoverAndExpect(t, stdin, reader, docURI, 3, secondCountLine, secondCountCh, "var count: i32")
	// Reference to a different variable, `doubled`, in the return.
	hoverAndExpect(t, stdin, reader, docURI, 4, doubledRefLine, doubledRefCh, "var doubled: i32")

	shutReq := []byte(`{"jsonrpc":"2.0","id":5,"method":"shutdown"}`)
	writeLSPFrame(t, stdin, shutReq)
	readLSPFrameWithTimeout(t, reader, 10*time.Second)
	writeLSPFrame(t, stdin, []byte(`{"jsonrpc":"2.0","method":"exit"}`))
}

// TestLSPHoverDeclarationAndRicherContent exercises the richer hover content
// and the declaration-site coverage gaps reported by the project owner:
// hovering a variable's OWN declaration name (not a later reference), a
// function parameter, a function's own declared name, a struct field, a
// nominal (struct) type's declaration name, and a pointer-typed value. Each
// asserts the richer kind-and-type rendering ("var x: T", "param p: T",
// "fn f(...) R", "field f: T", "type T"), confirming real type names rather
// than the coarse placeholders.
func TestLSPHoverDeclarationAndRicherContent(t *testing.T) {
	bin := buildPEBC(t)

	root, err := os.MkdirTemp("", "pebclsp")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })
	mainPath := filepath.Join(root, "main.peb")
	src := "type Point = struct {\n" +
		"    x int;\n" +
		"    y int;\n" +
		"};\n" +
		"\n" +
		"fn add(p Point, scale int) Point {\n" +
		"    return Point.{ x = p.x + scale, y = p.y + scale };\n" +
		"}\n" +
		"\n" +
		"fn main() int {\n" +
		"    var origin Point = Point.{ x = 0, y = 0 };\n" +
		"    var ptr *Point = &origin;\n" +
		"    let moved = add(origin, 5);\n" +
		"    return moved.x as int;\n" +
		"}\n"
	if err := os.WriteFile(mainPath, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	docURI := "file://" + mainPath
	rootURI := "file://" + root

	// The struct field `x`'s declared name in the type declaration.
	fieldLine, fieldCh := computeLSPPos(t, src, "    x int;")
	fieldCh += len("    x")
	// The type declaration name `Point`.
	typeLine, typeCh := computeLSPPos(t, src, "type Point =")
	typeCh += len("type ")
	// The parameter name `p` in `fn add(p Point, ...)`.
	paramLine, paramCh := computeLSPPos(t, src, "fn add(p Point")
	paramCh += len("fn add(")
	// The function's own declared name `add`.
	fnLine, fnCh := computeLSPPos(t, src, "fn add(")
	fnCh += len("fn ")
	// The declaration name `origin` in `var origin Point`.
	varLine, varCh := computeLSPPos(t, src, "var origin Point")
	varCh += len("var ")
	// The pointer-typed declaration name `ptr` in `var ptr *Point`.
	ptrLine, ptrCh := computeLSPPos(t, src, "var ptr *Point")
	ptrCh += len("var ")
	// The nominal type name `Point` in a type position (`var origin Point`).
	nomTypeLine, nomTypeCh := computeLSPPos(t, src, "var origin Point")
	nomTypeCh += len("var origin ")
	// A later reference to `moved` (value use, not declaration).
	movedLine, movedCh := computeLSPPos(t, src, "return moved.x")
	movedCh += len("return ")
	// The nominal type name `Point` in the return type of `fn add`.
	retTypeLine, retTypeCh := computeLSPPos(t, src, ") Point {")
	retTypeCh += len(") ")

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
		stopCmd := exec.Command(bin, "daemon", "stop")
		stopCmd.Dir = root
		_ = stopCmd.Run()
		t.Logf("lsp stderr: %s", stderr.String())
	}()

	reader := bufio.NewReader(stdout)

	initReq := fmt.Sprintf(`{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootUri":%q,"workspaceFolders":[{"uri":%q,"name":"scratch"}],"capabilities":{}}}`, rootURI, rootURI)
	writeLSPFrame(t, stdin, []byte(initReq))
	readLSPFrameWithTimeout(t, reader, 10*time.Second)

	openReq := fmt.Sprintf(`{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":%q,"languageId":"pebble","version":1,"text":%q}}}`, docURI, src)
	writeLSPFrame(t, stdin, []byte(openReq))
	time.Sleep(200 * time.Millisecond)

	// id counter for hover requests.
	id := 2
	hover := func(line, ch int, want string) {
		t.Helper()
		hoverAndExpect(t, stdin, reader, docURI, id, line, ch, want)
		id++
	}

	hover(fieldLine, fieldCh, "field x: int")
	hover(typeLine, typeCh, "type Point")
	hover(paramLine, paramCh, "param p: Point")
	hover(fnLine, fnCh, "fn add(Point, int) Point")
	hover(varLine, varCh, "var origin: Point")
	hover(ptrLine, ptrCh, "var ptr: *Point")
	hover(nomTypeLine, nomTypeCh, "type Point")
	hover(movedLine, movedCh, "let moved: Point")
	hover(retTypeLine, retTypeCh, "type Point")

	shutReq := []byte(`{"jsonrpc":"2.0","id":` + fmt.Sprint(id) + `,"method":"shutdown"}`)
	writeLSPFrame(t, stdin, shutReq)
	readLSPFrameWithTimeout(t, reader, 10*time.Second)
	writeLSPFrame(t, stdin, []byte(`{"jsonrpc":"2.0","method":"exit"}`))
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
		// If it exited, it must be clean (no panic output on stderr). A
		// routine "starting" log line is expected now that the server logs
		// its lifecycle to stderr for editor log panels; only a panic
		// signals a real crash.
		if strings.Contains(stderr.String(), "panic") {
			t.Fatalf("pebc lsp crashed on garbage input; stderr: %s", stderr.String())
		}
	case <-time.After(500 * time.Millisecond):
		// Still alive and not crashed: good. Kill it for cleanup.
		_ = cmd.Process.Kill()
		<-done
		if strings.Contains(stderr.String(), "panic") {
			t.Fatalf("pebc lsp panicked while handling garbage: %s", stderr.String())
		}
	}
}
