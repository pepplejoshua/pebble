package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/url"
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

// TestLSPInlayHints exercises the textDocument/inlayHint feature against a real
// scratch project: initialize, open a type-correct file, send a real
// textDocument/inlayHint REQUEST over the whole file range, and read the
// RESPONSE. It asserts three things in one realistic program:
//
//   - A binding WITHOUT an explicit type (`let count = 99;`) gets a type hint
//     " i32" anchored right after the binding name.
//   - A binding WITH an explicit type (`var origin Point = ...`) gets NO type
//     hint (redundant-hint suppression, matching gopls/rust-analyzer).
//   - A multi-parameter call (`add(origin, 5)`) gets a parameter-name hint
//     before each argument: "p: " before `origin`, "scale: " before `5`, in
//     declared-parameter order and at the right positions.
func TestLSPInlayHints(t *testing.T) {
	bin := buildPEBC(t)

	// Short, shallow root -- NOT t.TempDir() directly (see the documented
	// 104-byte Unix socket path limit note in TestLSPDiagnosticsOnSave).
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
		"    let count = 99;\n" +
		"    let moved = add(origin, 5);\n" +
		"    return moved.x as int;\n" +
		"}\n"
	if err := os.WriteFile(mainPath, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	docURI := "file://" + mainPath
	rootURI := "file://" + root

	// Expected anchor positions (0-based line/character).
	countLine, countCh := computeLSPPos(t, src, "let count =")
	countCh += len("let ") // start of "count" name
	countEndCh := countCh + len("count")

	movedLine, movedCh := computeLSPPos(t, src, "let moved =")
	movedCh += len("let ") // start of "moved" name
	movedEndCh := movedCh + len("moved")

	// origin is explicitly annotated, so its name-end must carry NO type hint.
	originLine, originCh := computeLSPPos(t, src, "var origin Point")
	originCh += len("var ") // start of "origin" name
	originEndCh := originCh + len("origin")

	// First argument `origin` of the call `add(origin, 5)`.
	arg0Line, arg0Ch := computeLSPPos(t, src, "origin, 5)")
	// Second argument `5` of the call `add(origin, 5)`.
	arg1Line, arg1Ch := computeLSPPos(t, src, "5);")
	// arg1Ch already points at the "5".

	// Whole-file request range: start (0,0) to a far-beyond end that the
	// server clamps to the file end.
	endLine := strings.Count(src, "\n")

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

	inlayReq := fmt.Sprintf(`{"jsonrpc":"2.0","id":2,"method":"textDocument/inlayHint","params":{"textDocument":{"uri":%q},"range":{"start":{"line":0,"character":0},"end":{"line":%d,"character":100000}}}}`, docURI, endLine)
	writeLSPFrame(t, stdin, []byte(inlayReq))
	body := readLSPFrameWithTimeout(t, reader, 20*time.Second)

	var resp struct {
		ID     int             `json:"id"`
		Error  json.RawMessage `json:"error"`
		Result []struct {
			Position struct {
				Line      int `json:"line"`
				Character int `json:"character"`
			} `json:"position"`
			Label       string `json:"label"`
			Kind        int    `json:"kind"`
			PaddingLeft bool   `json:"paddingLeft"`
		} `json:"result"`
	}
	if err := json.Unmarshal(body, &resp); err != nil {
		t.Fatalf("parsing inlayHint response %s: %v", body, err)
	}
	if resp.Error != nil {
		t.Fatalf("inlayHint returned error: %s (body %s)", resp.Error, body)
	}
	if resp.ID != 2 {
		t.Fatalf("inlayHint response id = %d, want 2 (body %s)", resp.ID, body)
	}
	t.Logf("inlayHint response: %+v", resp.Result)

	// Find helpers over the returned hints.
	findType := func(line, ch int) (string, bool) {
		for _, h := range resp.Result {
			if h.Kind == 1 && h.Position.Line == line && h.Position.Character == ch {
				return h.Label, true
			}
		}
		return "", false
	}
	findParam := func(line, ch int) (string, bool, bool) {
		for _, h := range resp.Result {
			if h.Kind == 2 && h.Position.Line == line && h.Position.Character == ch {
				return h.Label, h.PaddingLeft, true
			}
		}
		return "", false, false
	}

	// 1. Unannotated binding `count` gets a " int" type hint at its name end.
	// (An unannotated integer literal infers the default `int`, which is why
	// this is "int" rather than "i32" — the point of the test is the hint
	// appears at all, not which specific integer width is inferred.)
	label, ok := findType(countLine, countEndCh)
	if !ok {
		t.Fatalf("expected type hint at (count name end) (%d,%d); got hints %+v", countLine, countEndCh, resp.Result)
	}
	if label != " int" {
		t.Fatalf("type hint label = %q, want \" int\"", label)
	}

	// 2. Unannotated binding `moved` gets a " Point" type hint at its name end.
	label, ok = findType(movedLine, movedEndCh)
	if !ok {
		t.Fatalf("expected type hint at (moved name end) (%d,%d); got hints %+v", movedLine, movedEndCh, resp.Result)
	}
	if label != " Point" {
		t.Fatalf("type hint label = %q, want \" Point\"", label)
	}

	// 3. Annotated binding `origin` gets NO type hint at its name end.
	if _, ok := findType(originLine, originEndCh); ok {
		t.Fatalf("annotated binding `origin` should NOT produce a type hint, but one was found at (%d,%d)", originLine, originEndCh)
	}

	// 4. Parameter hints: "p: " before `origin`, "scale: " before `5`. Neither
	// gets PaddingLeft -- the label's own trailing ": " is enough spacing on
	// the right, and an unconditional PaddingLeft on the left previously
	// double-spaced every non-first argument (already preceded by ", " in the
	// source) and looked wrong even on the first argument once seen live.
	plabel, ppad, ok := findParam(arg0Line, arg0Ch)
	if !ok {
		t.Fatalf("expected parameter hint before first arg `origin` at (%d,%d); got hints %+v", arg0Line, arg0Ch, resp.Result)
	}
	if plabel != "p: " {
		t.Fatalf("first parameter hint label = %q, want \"p: \"", plabel)
	}
	if ppad {
		t.Fatalf("first parameter hint should have PaddingLeft=false (padding is never applied to parameter hints)")
	}
	plabel, ppad, ok = findParam(arg1Line, arg1Ch)
	if !ok {
		t.Fatalf("expected parameter hint before second arg `5` at (%d,%d); got hints %+v", arg1Line, arg1Ch, resp.Result)
	}
	if plabel != "scale: " {
		t.Fatalf("second parameter hint label = %q, want \"scale: \"", plabel)
	}
	if ppad {
		t.Fatalf("second parameter hint should have PaddingLeft=false (padding is never applied to parameter hints)")
	}

	// Shut the server down cleanly.
	shutReq := []byte(`{"jsonrpc":"2.0","id":3,"method":"shutdown"}`)
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

// definitionAt sends a real textDocument/definition REQUEST at the given
// 0-based line/character and returns the parsed response's result Location
// (nil when the response reports null) plus the raw body, failing the test on
// a transport/error response.
func definitionAt(t *testing.T, stdin io.WriteCloser, reader *bufio.Reader, docURI string, id, line, ch int) (*struct {
	URI   string `json:"uri"`
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
}, string) {
	t.Helper()
	req := fmt.Sprintf(`{"jsonrpc":"2.0","id":%d,"method":"textDocument/definition","params":{"textDocument":{"uri":%q},"position":{"line":%d,"character":%d}}}`, id, docURI, line, ch)
	writeLSPFrame(t, stdin, []byte(req))
	body := readLSPFrameWithTimeout(t, reader, 20*time.Second)
	var resp struct {
		ID     int             `json:"id"`
		Result json.RawMessage `json:"result"`
		Error  json.RawMessage `json:"error"`
	}
	if err := json.Unmarshal(body, &resp); err != nil {
		t.Fatalf("parsing definition response %s: %v", body, err)
	}
	if resp.Error != nil {
		t.Fatalf("definition returned error: %s (body %s)", resp.Error, body)
	}
	if resp.ID != id {
		t.Fatalf("definition response id = %d, want %d (body %s)", resp.ID, id, body)
	}
	if string(resp.Result) == "null" {
		return nil, string(body)
	}
	var loc struct {
		URI   string `json:"uri"`
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
	}
	if err := json.Unmarshal(resp.Result, &loc); err != nil {
		t.Fatalf("parsing definition result %s: %v", resp.Result, err)
	}
	return &loc, string(body)
}

// TestLSPDefinition exercises the textDocument/definition feature against a
// real scratch project with a real LSP client round-trip. It asserts:
//
//   - A request at a variable NAME REFERENCE resolves to that variable's
//     declaration location in the SAME file.
//   - A request at a function CALL SITE resolves to the function's
//     declaration location.
//   - A request at a position with nothing resolvable (a literal) returns a
//     null result, not an error.
//   - A request at a symbol declared in a DIFFERENT file (a local import)
//     resolves to the declaration in that other file, with the returned URI
//     pointing at the imported file, not the request's.
//   - A request ON a declaration's own name still resolves to itself (no-op
//     navigation, not an error).
func TestLSPDefinition(t *testing.T) {
	bin := buildPEBC(t)

	// Short, shallow root -- NOT t.TempDir() directly (see the documented
	// 104-byte Unix socket path limit note in TestLSPDiagnosticsOnSave).
	root, err := os.MkdirTemp("", "pebclsp")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })

	helperPath := filepath.Join(root, "helper.peb")
	helperSrc := "fn helper_fn() i32 {\n    return 42;\n}\n"
	if err := os.WriteFile(helperPath, []byte(helperSrc), 0o644); err != nil {
		t.Fatal(err)
	}

	mainPath := filepath.Join(root, "main.peb")
	src := "import \"./helper\";\n" +
		"\n" +
		"fn add(a i32, b i32) i32 {\n" +
		"    return a + b;\n" +
		"}\n" +
		"\n" +
		"fn main() int {\n" +
		"    var x i32 = 7;\n" +
		"    let y = add(x, 5);\n" +
		"    let z = helper::helper_fn();\n" +
		"    return y as int + z as int;\n" +
		"}\n"
	if err := os.WriteFile(mainPath, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	// The daemon canonicalizes paths through EvalSymlinks (on macOS, /var
	// resolves to /private/var), so the expected URIs must match the
	// canonicalized forms.
	canonMain, err := filepath.EvalSymlinks(mainPath)
	if err != nil {
		t.Fatal(err)
	}
	canonHelper, err := filepath.EvalSymlinks(helperPath)
	if err != nil {
		t.Fatal(err)
	}
	mainPath = canonMain
	helperPath = canonHelper
	docURI := "file://" + mainPath
	helperDocURI := "file://" + helperPath
	rootURI := "file://" + root

	// Position of the reference to `x` inside `add(x, 5)`.
	xRefLine, xRefCh := computeLSPPos(t, src, "add(x, 5)")
	xRefCh += len("add(") // the "x" reference
	// Position of the function call site `add(`.
	callLine, callCh := computeLSPPos(t, src, "let y = add(")
	callCh += len("let y = ") // the "add" callee name
	// Position of a literal `7` (nothing resolvable).
	litLine, litCh := computeLSPPos(t, src, "var x i32 = 7;")
	litCh += len("var x i32 = ") // the "7"
	// Position of the cross-file call `helper::helper_fn()`.
	cfLine, cfCh := computeLSPPos(t, src, "helper::helper_fn")
	cfCh += len("helper::") // the "helper_fn" member name
	// Position of `x`'s own declaration name in `var x i32`.
	xDeclLine, xDeclCh := computeLSPPos(t, src, "var x i32")
	xDeclCh += len("var ") // the "x" declaration name

	// Expected declaration positions (computed from the respective files).
	// `x`'s declaration: `var x i32 = 7;` on its line, at its name.
	expXLine, expXCh := computeLSPPos(t, src, "var x i32")
	expXCh += len("var ")
	// `add`'s declaration: `fn add(` on its line, at its name.
	expAddLine, expAddCh := computeLSPPos(t, src, "fn add(")
	expAddCh += len("fn ")
	// `helper_fn`'s declaration lives in helper.peb: `fn helper_fn(`.
	expHLine, expHCh := computeLSPPos(t, helperSrc, "fn helper_fn(")
	expHCh += len("fn ")

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

	id := 2
	expectLoc := func(line, ch int, wantURI string, wantLine, wantCh int, label string) {
		t.Helper()
		loc, body := definitionAt(t, stdin, reader, docURI, id, line, ch)
		id++
		if loc == nil {
			t.Fatalf("%s: got null result (body %s)", label, body)
		}
		if loc.URI != wantURI {
			t.Fatalf("%s: URI = %q, want %q (body %s)", label, loc.URI, wantURI, body)
		}
		if loc.Range.Start.Line != wantLine || loc.Range.Start.Character != wantCh {
			t.Fatalf("%s: start = (%d,%d), want (%d,%d) (body %s)", label, loc.Range.Start.Line, loc.Range.Start.Character, wantLine, wantCh, body)
		}
		t.Logf("%s -> (%d,%d) at %s", label, wantLine, wantCh, wantURI)
	}
	expectNone := func(line, ch int, label string) {
		t.Helper()
		loc, body := definitionAt(t, stdin, reader, docURI, id, line, ch)
		id++
		if loc != nil {
			t.Fatalf("%s: expected null result, got %+v (body %s)", label, loc, body)
		}
		t.Logf("%s -> null", label)
	}

	// 1. Variable reference `x` -> its declaration in the same file.
	expectLoc(xRefLine, xRefCh, docURI, expXLine, expXCh, "var reference x")
	// 2. Function call site `add(` -> its declaration.
	expectLoc(callLine, callCh, docURI, expAddLine, expAddCh, "call site add")
	// 3. Literal `7` -> no result.
	expectNone(litLine, litCh, "literal 7")
	// 4. Cross-file call `helper_fn()` -> declaration in helper.peb.
	expectLoc(cfLine, cfCh, helperDocURI, expHLine, expHCh, "cross-file helper_fn")
	// 5. Declaration's own name `x` -> itself (no-op navigation).
	expectLoc(xDeclLine, xDeclCh, docURI, expXLine, expXCh, "declaration name x")

	shutReq := []byte(`{"jsonrpc":"2.0","id":` + fmt.Sprint(id) + `,"method":"shutdown"}`)
	writeLSPFrame(t, stdin, shutReq)
	readLSPFrameWithTimeout(t, reader, 10*time.Second)
	writeLSPFrame(t, stdin, []byte(`{"jsonrpc":"2.0","method":"exit"}`))
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
	hoverAndExpect(t, stdin, reader, docURI, 2, firstCountLine, firstCountCh, "var count i32")
	// Second (distinct syntax node) reference to the same variable.
	hoverAndExpect(t, stdin, reader, docURI, 3, secondCountLine, secondCountCh, "var count i32")
	// Reference to a different variable, `doubled`, in the return.
	hoverAndExpect(t, stdin, reader, docURI, 4, doubledRefLine, doubledRefCh, "var doubled i32")

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
// asserts the richer kind-and-type rendering ("var x T", "param p T",
// "fn f(...) R", "field f T", "type T"), confirming real type names rather
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

	hover(fieldLine, fieldCh, "field x int")
	hover(typeLine, typeCh, "type Point")
	hover(paramLine, paramCh, "param p Point")
	hover(fnLine, fnCh, "fn add(Point, int) Point")
	hover(varLine, varCh, "var origin Point")
	hover(ptrLine, ptrCh, "var ptr *Point")
	hover(nomTypeLine, nomTypeCh, "type Point")
	hover(movedLine, movedCh, "let moved Point")
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

// TestLSPQualifiedCrossModuleTypes exercises cross-module type qualification
// through a real LSP round-trip rooted at a temp directory. A value whose type
// comes from an imported module (Set[str] from std:set) must render qualified
// ("set::Set[str]") in both hover and inlay hints, while a type declared in the
// current module (Point) must stay bare. Qualification does not depend on the
// on-disk std tree, so a temp-rooted daemon exercises it fully.
func TestLSPQualifiedCrossModuleTypes(t *testing.T) {
	bin := buildPEBC(t)

	root, err := os.MkdirTemp("", "pebclsp")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })
	mainPath := filepath.Join(root, "main.peb")
	src := "import \"std:hash\";\n" +
		"import \"std:set\";\n" +
		"\n" +
		"type Point = struct { x int; y int; };\n" +
		"\n" +
		"fn main() int {\n" +
		"    var s = set::new[str](hash::hash_str, fn (a, b str) bool => a == b);\n" +
		"    var origin Point = Point.{ x = 0, y = 0 };\n" +
		"    return origin.x as int;\n" +
		"}\n"
	if err := os.WriteFile(mainPath, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	docURI := "file://" + mainPath
	rootURI := "file://" + root

	// Hover the `s` declaration name: must render qualified.
	sLine, sCh := computeLSPPos(t, src, "var s = set::new")
	sCh += len("var ") // the "s" name
	// Inlay type hint anchor is right after the `s` name.
	sEndCh := sCh + len("s")
	// Hover the `origin` declaration name: must stay bare.
	oLine, oCh := computeLSPPos(t, src, "var origin Point")
	oCh += len("var ")

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

	// Hover `s` -> qualified cross-module type.
	hoverAndExpect(t, stdin, reader, docURI, 2, sLine, sCh, "var s set::Set[str]")
	// Hover `origin` -> same-module type stays bare.
	hoverAndExpect(t, stdin, reader, docURI, 3, oLine, oCh, "var origin Point")

	// Inlay hint over the whole file: the unannotated `s` binding must get a
	// qualified type hint " set::Set[str]" at its name end.
	endLine := strings.Count(src, "\n")
	inlayReq := fmt.Sprintf(`{"jsonrpc":"2.0","id":4,"method":"textDocument/inlayHint","params":{"textDocument":{"uri":%q},"range":{"start":{"line":0,"character":0},"end":{"line":%d,"character":100000}}}}`, docURI, endLine)
	writeLSPFrame(t, stdin, []byte(inlayReq))
	body := readLSPFrameWithTimeout(t, reader, 20*time.Second)
	var resp struct {
		ID     int             `json:"id"`
		Error  json.RawMessage `json:"error"`
		Result []struct {
			Position struct {
				Line      int `json:"line"`
				Character int `json:"character"`
			} `json:"position"`
			Label string `json:"label"`
			Kind  int    `json:"kind"`
		} `json:"result"`
	}
	if err := json.Unmarshal(body, &resp); err != nil {
		t.Fatalf("parsing inlayHint response %s: %v", body, err)
	}
	if resp.Error != nil {
		t.Fatalf("inlayHint returned error: %s (body %s)", resp.Error, body)
	}
	if resp.ID != 4 {
		t.Fatalf("inlayHint response id = %d, want 4 (body %s)", resp.ID, body)
	}
	found := false
	for _, h := range resp.Result {
		if h.Kind == 1 && h.Position.Line == sLine && h.Position.Character == sEndCh {
			found = true
			if h.Label != " set::Set[str]" {
				t.Fatalf("s type hint label = %q, want %q", h.Label, " set::Set[str]")
			}
		}
	}
	if !found {
		t.Fatalf("expected a type hint at (s name end) (%d,%d); got hints %+v", sLine, sEndCh, resp.Result)
	}
	t.Logf("qualified inlay hint for s: %+v", resp.Result)

	shutReq := []byte(`{"jsonrpc":"2.0","id":5,"method":"shutdown"}`)
	writeLSPFrame(t, stdin, shutReq)
	readLSPFrameWithTimeout(t, reader, 10*time.Second)
	writeLSPFrame(t, stdin, []byte(`{"jsonrpc":"2.0","method":"exit"}`))
}

// TestLSPStdlibDefinitionRealFile exercises go-to-definition INTO the embedded
// standard library through a real LSP round-trip. The daemon runs rooted at a
// scratch directory placed under the repo root (the checkout), so walking up
// from the daemon's working directory locates the real on-disk std/ tree
// exactly as a self-hosted editor session would. A definition request on a
// stdlib symbol must therefore resolve to a REAL file on disk (os.Stat
// confirms the target exists), not a synthetic "std:embedded/..." URI.
func TestLSPStdlibDefinitionRealFile(t *testing.T) {
	bin := buildPEBC(t)

	// Repo root is the parent of the module (go.mod) directory; it holds both
	// runtime/ (locateRuntimeRoot's anchor) and compiler/std.
	compilerRoot := findModuleRoot(t)
	repoRoot := filepath.Dir(compilerRoot)
	if info, err := os.Stat(filepath.Join(repoRoot, "runtime", "include")); err != nil || !info.IsDir() {
		t.Skipf("runtime/ not found under %q; cannot resolve on-disk std", repoRoot)
	}

	// A short scratch dir under the repo root so the daemon's cwd walks up to
	// find runtime/ + compiler/std, and its socket path is isolated from any
	// real editor daemon.
	root, err := os.MkdirTemp(repoRoot, "pebclspstd")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })

	mainPath := filepath.Join(root, "main.peb")
	src := "import \"std:hash\";\n" +
		"import \"std:set\";\n" +
		"\n" +
		"fn main() int {\n" +
		"    var s = set::new[str](hash::hash_str, fn (a, b str) bool => a == b);\n" +
		"    return 0;\n" +
		"}\n"
	if err := os.WriteFile(mainPath, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	docURI := "file://" + mainPath
	rootURI := "file://" + root

	// Position of the `new` callee in `set::new[str]` -- a symbol declared in
	// std:set. This must resolve to the real on-disk std/set.peb file.
	newLine, newCh := computeLSPPos(t, src, "set::new[str]")
	newCh += len("set::")

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

	// Definition on the `new` callee (declared in std:set). The returned URI
	// must be a REAL file on disk -- the std/set.peb this checkout was built
	// from -- not a synthetic "std:embedded/set.peb" path.
	loc, body := definitionAt(t, stdin, reader, docURI, 2, newLine, newCh)
	if loc == nil {
		t.Fatalf("definition for set::new resolved to null (body %s)", body)
	}
	fsPath, err := urlPathToFsPath(loc.URI)
	if err != nil {
		t.Fatalf("definition URI %q is not a valid file path: %v (body %s)", loc.URI, err, body)
	}
	if strings.Contains(fsPath, "std:embedded") {
		t.Fatalf("definition URI %q still uses the synthetic embedded path; want a real file (body %s)", loc.URI, body)
	}
	info, err := os.Stat(fsPath)
	if err != nil || info.IsDir() {
		t.Fatalf("definition target %q is not a real file on disk: %v (body %s)", loc.URI, err, body)
	}
	if filepath.Base(fsPath) != "set.peb" {
		t.Fatalf("definition target %q does not point at std/set.peb (body %s)", loc.URI, body)
	}
	t.Logf("definition for set::new -> real file %s", fsPath)

	shutReq := []byte(`{"jsonrpc":"2.0","id":4,"method":"shutdown"}`)
	writeLSPFrame(t, stdin, shutReq)
	readLSPFrameWithTimeout(t, reader, 10*time.Second)
	writeLSPFrame(t, stdin, []byte(`{"jsonrpc":"2.0","method":"exit"}`))
}

// urlPathToFsPath converts a file:// URI to a filesystem path, failing on a
// URI that does not start with file://.
func urlPathToFsPath(uri string) (string, error) {
	const prefix = "file://"
	if !strings.HasPrefix(uri, prefix) {
		return "", fmt.Errorf("not a file:// URI")
	}
	// LSP file:// URIs are absolute filesystem paths (file:///...). Strip the
	// scheme and percent-decode the remainder (e.g. "%3A" -> ":").
	raw := strings.TrimPrefix(uri, prefix)
	p, err := url.PathUnescape(raw)
	if err != nil {
		return "", err
	}
	return p, nil
}
