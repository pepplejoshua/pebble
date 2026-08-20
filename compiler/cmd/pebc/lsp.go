package main

import (
	"context"
	"flag"
	"io"
	"os"
	"sync"

	"go.lsp.dev/jsonrpc2"
	"go.lsp.dev/protocol"
	"go.lsp.dev/uri"
)

// pebc lsp: an LSP transport skeleton (slice 21.4a).
//
// Implements only the initialize/shutdown/exit handshake over stdio, per
// the LSP spec's lifecycle: `shutdown` tells the server to prepare to exit
// but must NOT terminate the process; only the `exit` notification actually
// ends it. No real language features (diagnostics, hover, etc.) are wired
// up yet -- those are separate future slices (21.4b/21.4c).
//
// Transport is stdio (os.Stdin/os.Stdout), the standard LSP convention: the
// editor spawns this process and talks to its stdio directly -- a
// completely separate transport from the daemon's own Unix-socket
// build-request RPC (daemon_rpc.go).
//
// KNOWN ISSUE (21.4b): sending two notifications back-to-back with zero gap
// (e.g. didOpen immediately followed by didSave, no delay between the two
// stdin writes) can silently drop the second one's dispatch -- confirmed via
// direct reproduction: the handler for the second notification is simply
// never invoked. This traces to go.lsp.dev/jsonrpc2's raw stream reading, not
// this file's dispatch logic (confirmed both handlers work reliably given a
// small real-world gap between messages, e.g. 200ms+, and confirmed via a
// standalone entry-point debug print that DidOpen fires reliably every time
// while DidSave's dispatch is what gets lost under zero-gap back-to-back
// sends -- likely two frames landing in a single stdioReadWriteCloser.Read()
// call in a way the library's frame parser doesn't fully pipeline). No real
// editor fires didOpen and didSave with zero gap (didOpen happens when a file
// is first opened, typically seconds to minutes before any save), so this is
// a documented limitation rather than a slice-blocking bug. A future slice
// touching this transport should investigate go.lsp.dev/jsonrpc2's frame
// reader directly if this ever needs to be made airtight for adversarial
// input.

type lspServer struct {
	protocol.UnimplementedServer
	done chan struct{}
	// root is the resolved workspace root filesystem path, captured from the
	// editor's initialize params. It is the root we start/attach a daemon for.
	root string
	// conn is the jsonrpc2 connection, stored so handlers can push
	// notifications (publishDiagnostics) back to the client.
	conn jsonrpc2.Conn
	// mu guards the open document set.
	mu   sync.Mutex
	open map[uri.URI]bool
}

func (s *lspServer) Initialize(ctx context.Context, params *protocol.InitializeParams) (*protocol.InitializeResult, error) {
	// Prefer workspaceFolders[0]; fall back to rootUri. Some minimal/test
	// clients send neither, in which case we fall back to the process working
	// directory so we still have a root to attach a daemon to.
	root := ""
	if folders, ok := params.WorkspaceFolders.Get(); ok && len(folders) > 0 {
		root = folders[0].URI.FsPath()
	}
	if root == "" && params.RootURI != nil {
		root = params.RootURI.FsPath()
	}
	if root == "" {
		// No root advertised by the client; use the process cwd. This keeps
		// the daemon resolution deterministic even for clients that omit both
		// workspaceFolders and rootUri.
		if cwd, err := os.Getwd(); err == nil {
			root = cwd
		}
	}
	s.root = root
	return &protocol.InitializeResult{
		Capabilities: protocol.ServerCapabilities{},
		ServerInfo: protocol.ServerInfo{
			Name: "pebc",
		},
	}, nil
}

// DidOpen records that a document is open. A real editor sends didOpen before
// didSave; we track it so the workspace state mirrors a real session.
func (s *lspServer) DidOpen(ctx context.Context, params *protocol.DidOpenTextDocumentParams) error {
	s.mu.Lock()
	if s.open == nil {
		s.open = make(map[uri.URI]bool)
	}
	s.open[params.TextDocument.URI] = true
	s.mu.Unlock()
	return nil
}

// DidSave triggers a build through the daemon for the saved file and publishes
// the resulting diagnostics (or clears them on a clean build).
func (s *lspServer) DidSave(ctx context.Context, params *protocol.DidSaveTextDocumentParams) error {
	docURI := params.TextDocument.URI
	fsPath := docURI.FsPath()

	// Ensure a daemon is running for this server's resolved root.
	if err := ensureDaemonForRoot(s.root, io.Discard); err != nil {
		// Cannot build; still publish an empty set so stale markers clear.
		s.publishDiagnostics(ctx, docURI, nil)
		return nil
	}

	resp, err := daemonRPCForRoot(s.root, "build", daemonRequest{Entry: fsPath, Output: ""})
	if err != nil {
		s.publishDiagnostics(ctx, docURI, nil)
		return nil
	}

	protos := make([]protocol.Diagnostic, 0, len(resp.StructuredDiagnostics))
	for _, sd := range resp.StructuredDiagnostics {
		protos = append(protos, toProtocolDiagnostic(sd))
	}
	s.publishDiagnostics(ctx, docURI, protos)
	return nil
}

// publishDiagnostics sends a publishDiagnostics notification for the document.
// It is always sent (even with an empty slice) so the editor clears stale
// markers from a previous failed build once the code is clean.
func (s *lspServer) publishDiagnostics(ctx context.Context, docURI uri.URI, diags []protocol.Diagnostic) {
	if diags == nil {
		diags = []protocol.Diagnostic{}
	}
	params := &protocol.PublishDiagnosticsParams{
		URI:         docURI,
		Diagnostics: diags,
	}
	_ = s.conn.Notify(ctx, protocol.MethodTextDocumentPublishDiagnostics, params)
}

// toProtocolDiagnostic converts one structured daemon diagnostic into an LSP
// protocol.Diagnostic, converting the 1-based source positions to the 0-based
// positions LSP expects.
func toProtocolDiagnostic(sd structuredDiagnostic) protocol.Diagnostic {
	var severity protocol.DiagnosticSeverity
	switch sd.Severity {
	case "error":
		severity = protocol.DiagnosticSeverityError
	case "warning":
		severity = protocol.DiagnosticSeverityWarning
	default:
		severity = protocol.DiagnosticSeverityHint
	}
	return protocol.Diagnostic{
		Range: protocol.Range{
			Start: protocol.Position{Line: uint32(sd.StartLine - 1), Character: uint32(sd.StartCol - 1)},
			End:   protocol.Position{Line: uint32(sd.EndLine - 1), Character: uint32(sd.EndCol - 1)},
		},
		Severity: severity,
		Code:     protocol.String(sd.Code),
		Message:  protocol.String(sd.Message),
	}
}

func (s *lspServer) Shutdown(ctx context.Context) error {
	// Per the LSP spec: acknowledge shutdown but keep the process alive.
	// Only the `exit` notification actually terminates it.
	return nil
}

func (s *lspServer) Exit(ctx context.Context) error {
	close(s.done)
	return nil
}

// stdioReadWriteCloser adapts os.Stdin/os.Stdout into the single
// io.ReadWriteCloser jsonrpc2.NewStream expects.
//
// Close() must actually close os.Stdin: the read goroutine blocks in
// os.Stdin.Read() waiting for the next frame, and jsonrpc2.Conn.Close()
// only unblocks that read by closing the underlying stream. A no-op Close
// would leave Conn.Close() (and thus `exit`) hanging forever.
type stdioReadWriteCloser struct {
	in  *os.File
	out io.Writer
}

func (s stdioReadWriteCloser) Read(p []byte) (int, error)  { return s.in.Read(p) }
func (s stdioReadWriteCloser) Write(p []byte) (int, error) { return s.out.Write(p) }
func (s stdioReadWriteCloser) Close() error                { return s.in.Close() }

// runLSP implements `pebc lsp`.
func runLSP(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("pebc lsp", flag.ContinueOnError)
	fs.SetOutput(stderr)
	if err := fs.Parse(args); err != nil {
		return 2
	}

	server := &lspServer{done: make(chan struct{})}
	stream := jsonrpc2.NewStream(stdioReadWriteCloser{in: os.Stdin, out: os.Stdout})
	ctx, conn, _ := protocol.NewServer(context.Background(), server, stream)
	server.conn = conn
	conn.Go(ctx, protocol.ServerHandler(server, nil))

	select {
	case <-server.done:
	case <-conn.Done():
	}
	_ = conn.Close()
	return 0
}
