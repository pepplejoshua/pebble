package main

import (
	"context"
	"flag"
	"io"
	"os"

	"go.lsp.dev/jsonrpc2"
	"go.lsp.dev/protocol"
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

type lspServer struct {
	protocol.UnimplementedServer
	done chan struct{}
}

func (s *lspServer) Initialize(ctx context.Context, params *protocol.InitializeParams) (*protocol.InitializeResult, error) {
	return &protocol.InitializeResult{
		Capabilities: protocol.ServerCapabilities{},
		ServerInfo: protocol.ServerInfo{
			Name: "pebc",
		},
	}, nil
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
	conn.Go(ctx, protocol.ServerHandler(server, nil))

	select {
	case <-server.done:
	case <-conn.Done():
	}
	_ = conn.Close()
	return 0
}
