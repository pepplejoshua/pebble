package main

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io"
)

// daemon protocol
//
// The daemon and its companion CLI talk over a Unix domain socket using a
// simple length-prefixed JSON framing: each message is a 4-byte big-endian
// length N followed by exactly N bytes of JSON. Requests and responses both
// use this framing. The protocol is intentionally minimal for this slice;
// nothing fancier is needed yet.
//
// A request carries a Method and method-specific fields. A response carries
// ok and an optional diagnostic/error payload.

const daemonFrameSize = 4

// daemonRequest is the JSON body of a request sent to the daemon.
type daemonRequest struct {
	Method string `json:"method"`
	// Entry is the absolute path of the .peb entry file for a "build".
	Entry string `json:"entry,omitempty"`
	// Output is the requested executable path for a "build". Empty selects
	// the default binary path derived from Entry.
	Output string `json:"output,omitempty"`
	// Offset is a byte offset into Entry used by the "hover" RPC to ask for
	// the checked type at a source position.
	Offset uint32 `json:"offset,omitempty"`
	// StartOffset and EndOffset bound an inlay-hint query (the "inlayHints"
	// RPC): only hints whose anchor position falls within [StartOffset,
	// EndOffset) into Entry are returned. A whole-file query passes 0 and the
	// file length.
	StartOffset uint32 `json:"startOffset,omitempty"`
	EndOffset   uint32 `json:"endOffset,omitempty"`
}

// daemonResponse is the JSON body of a response sent back to a client.
type daemonResponse struct {
	OK bool `json:"ok"`
	// Output is the path of the built executable on a successful build.
	Output string `json:"output,omitempty"`
	// Diagnostics holds rendered diagnostics on a failed build.
	Diagnostics string `json:"diagnostics,omitempty"`
	// Error holds a transport or unexpected error message.
	Error string `json:"error,omitempty"`
	// WatchFiles maps each tracked source path to its last-known SHA-256,
	// populated by the watch-status method.
	WatchFiles map[string]string `json:"watch_files,omitempty"`
	// WatchEvents is the recent change-detection log (newest first),
	// populated by the watch-status method.
	WatchEvents []watchReport `json:"watch_events,omitempty"`
	// StructuredDiagnostics carries machine-readable diagnostics (with
	// file/line/column ranges) on a failed build, in addition to the
	// rendered Diagnostics string. Line/column values are 1-based, matching
	// source.Position; LSP clients convert to 0-based.
	StructuredDiagnostics []structuredDiagnostic `json:"structured_diagnostics,omitempty"`
	// Hover carries the rendered checked type at a requested source offset,
	// populated by the "hover" RPC. Empty means no type information is
	// available at that position (e.g. whitespace or a keyword), not an
	// error.
	Hover string `json:"hover,omitempty"`
	// InlayHints carries machine-readable inlay hints (with file/line/column
	// anchors and a kind) for a source range, populated by the "inlayHints"
	// RPC. Line/column values are 1-based, matching source.Position; the LSP
	// server converts them to 0-based.
	InlayHints []structuredInlayHint `json:"inlay_hints,omitempty"`
}

// structuredDiagnostic is the machine-readable form of one compiler diagnostic,
// with a resolved file path and 1-based line/column endpoints. The daemon's
// build RPC fills this from the compiler's diagnostic set; the LSP server reads
// it to publish editor diagnostics.
type structuredDiagnostic struct {
	File      string `json:"file"`
	StartLine int    `json:"startLine"`
	StartCol  int    `json:"startCol"`
	EndLine   int    `json:"endLine"`
	EndCol    int    `json:"endCol"`
	Severity  string `json:"severity"`
	Code      string `json:"code"`
	Message   string `json:"message"`
}

// structuredInlayHint is the machine-readable form of one inlay hint, following
// the structuredDiagnostic pattern. Line/Column are 1-based (matching
// source.Position); the LSP server converts them to 0-based LSP positions. The
// anchor Position sits right after a binding name (type hints) or immediately
// before a call argument (parameter hints). Kind is "type" or "parameter".
type structuredInlayHint struct {
	File  string `json:"file"`
	Line  int    `json:"line"`
	Col   int    `json:"col"`
	Label string `json:"label"`
	Kind  string `json:"kind"`
}

// inlayHint kinds, shared by the daemon's structured form and the LSP layer's
// protocol.InlayHintKind mapping.
const (
	inlayHintType      = "type"
	inlayHintParameter = "parameter"
)

// writeDaemonMessage writes a length-prefixed JSON message to w.
func writeDaemonMessage(w io.Writer, payload any) error {
	data, err := json.Marshal(payload)
	if err != nil {
		return err
	}
	var header [daemonFrameSize]byte
	binary.BigEndian.PutUint32(header[:], uint32(len(data)))
	if _, err := w.Write(header[:]); err != nil {
		return err
	}
	_, err = w.Write(data)
	return err
}

// readDaemonMessage reads one length-prefixed JSON message from r into
// payload. It returns io.EOF when the stream is closed between messages.
func readDaemonMessage(r io.Reader, payload any) error {
	var header [daemonFrameSize]byte
	if _, err := io.ReadFull(r, header[:]); err != nil {
		return err
	}
	length := binary.BigEndian.Uint32(header[:])
	if length > 1<<24 {
		return fmt.Errorf("daemon: oversized message of %d bytes", length)
	}
	buf := make([]byte, length)
	if _, err := io.ReadFull(r, buf); err != nil {
		return err
	}
	return json.Unmarshal(buf, payload)
}
