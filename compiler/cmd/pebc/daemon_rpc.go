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
