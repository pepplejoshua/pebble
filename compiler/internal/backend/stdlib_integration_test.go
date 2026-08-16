// Package backend provides integration tests for the Pebble standard library.
//
// CONVENTION — how stdlib integration tests work
//
// Each test in this file compiles one real .peb program under tests/stdlib/
// through the full pipeline:
//
//  1. Module resolution via stdlib.New(module.FileSystemProvider{}) — the SAME
//     provider that pebc (compiler/cmd/pebc/main.go) uses, serving all std:
//     imports from the embedded embed.FS backed by compiler/std/*.peb.
//  2. Type checking via check.Check with entry validation enabled.
//  3. C emission via backend.Emit.
//  4. cc compilation against the Pebble runtime sources (runtime/src/*.c)
//     in PEBBLE_RT_MODE_SAFE with -Wall -Wextra -Werror.
//  5. Execution of the resulting binary, capturing exit code and combined
//     stdout+stderr.
//
// Test programs communicate results via printed labels and exit codes: every
// assertion inside a .peb program prints "PASS: <check_name>" or "FAIL:
// <check_name>" to stdout, and returns the total failure count as its exit
// code. Exit code 0 means all checks passed.
//
// Every test uses the bounded harness (loopExecutionTimeout = 5 s) so a
// miscompiled non-terminating loop fails loudly instead of hanging the Go
// test process. Tests skip under `go test -short` via requireCIntegration(t).

package backend

import (
	"bytes"
	"context"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/stdlib"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// repoRoot returns the repository root directory by walking up from this
// test file's location. From compiler/internal/backend/, three parent
// directories lead to the repo root.
func repoRoot() string {
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		panic("stdlib_integration_test: cannot determine repo root")
	}
	return filepath.Clean(filepath.Join(filepath.Dir(file), "..", "..", ".."))
}

// testProgramPath returns the absolute path to a Pebble test program under
// tests/stdlib/.
func testProgramPath(relPath string) string {
	return filepath.Join(repoRoot(), "tests", "stdlib", relPath)
}

// compilePebbleTestFile runs a real .peb program from tests/stdlib/ through
// the full pipeline (module resolution via the stdlib provider, type checking,
// C emission, cc compilation, execution) using the bounded harness. Returns
// the program's exit code and combined stdout+stderr.
func compilePebbleTestFile(t *testing.T, relPath string) (int, string) {
	t.Helper()
	requireCIntegration(t)

	absPath := testProgramPath(relPath)

	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()

	provider := stdlib.New(module.FileSystemProvider{})
	graph := module.Build(module.BuildConfig{
		EntryPath:    absPath,
		Package:      "main",
		StandardRoot: stdlib.StandardRoot,
	}, provider, sources, diagnostics)

	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})

	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}

	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entryID = candidate.ID
			break
		}
	}
	if entryID == 0 {
		t.Fatalf("no main function found in %s", relPath)
	}

	result := check.Check(check.Inputs{
		Graph:         graph,
		Sources:       sources,
		Resolution:    resolution,
		Types:         store,
		LiteralTarget: infer.LiteralTarget{WordBits: 64},
	}, diagnostics, check.Config{})

	if !result.Successful() || diagnostics.Len() > 0 {
		t.Fatalf("check failed for %s\n%v", relPath, diagnostics.Items())
	}

	unit := result.IR()
	if unit == nil {
		t.Fatalf("checker returned no IR for %s", relPath)
	}

	var buf bytes.Buffer
	if err := Emit(unit, unit.Snapshot(), entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed for %s: %v", relPath, err)
	}

	binary := compileEmittedC(t, buf.Bytes())
	return runBoundedBinary(t, binary)
}

// runBoundedBinary executes the given compiled binary under loopExecutionTimeout
// and returns (exitCode, combinedOutput).
func runBoundedBinary(t *testing.T, binary string) (int, string) {
	t.Helper()

	ctx, cancel := context.WithTimeout(context.Background(), loopExecutionTimeout)
	defer cancel()

	run := exec.CommandContext(ctx, binary)
	output, err := run.CombinedOutput()

	if run.ProcessState == nil {
		t.Fatalf("compiled program did not start: %v\n%s", err, output)
	}

	if err != nil && err.Error() == "context deadline exceeded" {
		t.Fatalf("compiled program timed out after %s (non-terminating loop?)", loopExecutionTimeout)
	}

	code := run.ProcessState.ExitCode()
	return code, string(output)
}

// TestStdlibVec exercises Vec correctness: eq, reverse, push, pop, insert,
// remove, swap_remove, clear, resize, truncate, as_slice, and delete. The
// entire suite runs under the bounded harness so a pre-fix Vec.reverse empty-
// vector bug (unsigned underflow → out-of-bounds spin) fails within 5 s
// instead of hanging the test process.
func TestStdlibVec(t *testing.T) {
	t.Parallel()

	code, output := compilePebbleTestFile(t, "vec_test.peb")
	t.Logf("vec_test.peb output:\n%s", output)
	if code != 0 {
		t.Fatalf("vec_test.peb exited %d, want 0 (%d failures reported in output above)", code, code)
	}
}

// TestStdlibMem exercises mem::delete_slice correctness: a deleted slice must
// report both data == nil AND len == 0 (the pre-fix code cleared .data but
// left the stale .len behind). Also confirms deleting zero-length and
// nil-backed empty slices does not crash, and that normal mem allocation,
// copy, and cleanup are unaffected.
func TestStdlibMem(t *testing.T) {
	t.Parallel()

	code, output := compilePebbleTestFile(t, "mem_test.peb")
	t.Logf("mem_test.peb output:\n%s", output)
	if code != 0 {
		t.Fatalf("mem_test.peb exited %d, want 0 (%d failures reported in output above)", code, code)
	}
}

// TestStdlibStrByteAt exercises str_byte_at — the new checked raw-byte read on
// str. It verifies ASCII byte reads, multi-byte UTF-8 sequences (reading both
// raw bytes individually), three-byte and four-byte UTF-8 sequences, embedded
// NUL handling, empty-string bounds checking, and that the existing str[i]
// scalar-index path is completely unaffected.
func TestStdlibStrByteAt(t *testing.T) {
	t.Parallel()

	code, output := compilePebbleTestFile(t, "str_byte_at_test.peb")
	t.Logf("str_byte_at_test.peb output:\n%s", output)
	if code != 0 {
		t.Fatalf("str_byte_at_test.peb exited %d, want 0 (%d failures reported in output above)", code, code)
	}
}

// TestStdlibStrIndexRegression proves that str[i] (Unicode-scalar indexing)
// still returns decoded scalars after the str_byte_at addition — not raw bytes.
func TestStdlibStrIndexRegression(t *testing.T) {
	t.Parallel()

	code, output := compilePebbleTestFile(t, "str_index_regression_test.peb")
	t.Logf("str_index_regression_test.peb output:\n%s", output)
	if code != 0 {
		t.Fatalf("str_index_regression_test.peb exited %d, want 0 (%d failures reported in output above)", code, code)
	}
}

// TestStdlibStringByteCorrectness exercises String/hash_str byte-vs-scalar
// correctness: the byte-oriented String methods (push_str, starts_with,
// ends_with, find, insert) and hash::hash_str must operate on raw UTF-8 bytes
// via str_byte_at, not decoded-and-truncated Unicode scalars. It verifies
// multi-byte UTF-8 push_str/insert byte fidelity, multi-byte
// starts_with/ends_with/find, hash_str hashing the full byte length past an
// embedded NUL (hash_bytes agreement), hash_str determinism and multi-byte
// sensitivity, and the new push_byte/push_bytes raw-byte appends including
// embedded NULs with no C-string truncation.
func TestStdlibStringByteCorrectness(t *testing.T) {
	t.Parallel()

	code, output := compilePebbleTestFile(t, "string_byte_correctness_test.peb")
	t.Logf("string_byte_correctness_test.peb output:\n%s", output)
	if code != 0 {
		t.Fatalf("string_byte_correctness_test.peb exited %d, want 0 (%d failures reported in output above)", code, code)
	}
}

// TestStdlibHmapBoundedProbe exercises the bounded HashMap probing added in
// Slice 6: insert/get_by_ref/remove now probe at most self.cap slots instead of
// looping with an unbounded `while true`. It verifies ordinary correctness is
// unaffected across multiple rehashes (the main risk of the change is an
// off-by-one in the bound silently breaking lookups), that insert-after-remove
// reuses a tombstone slot correctly, and a stress case where colliding keys
// build probe chains of real length (~21 occupied slots of a 32-slot table) so
// the bounded loop's upper edge gets real coverage.
func TestStdlibHmapBoundedProbe(t *testing.T) {
	t.Parallel()

	code, output := compilePebbleTestFile(t, "hmap_bounded_probe_test.peb")
	t.Logf("hmap_bounded_probe_test.peb output:\n%s", output)
	if code != 0 {
		t.Fatalf("hmap_bounded_probe_test.peb exited %d, want 0 (%d failures reported in output above)", code, code)
	}
}

// TestStdlibSetBoundedProbe exercises the bounded Set probing added in Slice 6:
// insert/contains/remove now probe at most self.cap slots instead of looping
// with an unbounded `while true`. It mirrors the HashMap test: ordinary
// correctness across multiple rehashes, tombstone-slot reuse after a remove,
// and a colliding-keys stress case building probe chains of real length.
func TestStdlibSetBoundedProbe(t *testing.T) {
	t.Parallel()

	code, output := compilePebbleTestFile(t, "set_bounded_probe_test.peb")
	t.Logf("set_bounded_probe_test.peb output:\n%s", output)
	if code != 0 {
		t.Fatalf("set_bounded_probe_test.peb exited %d, want 0 (%d failures reported in output above)", code, code)
	}
}

// TestStdlibIoResult exercises the Result-returning checked I/O API added in
// Slice 5: open_checked (existing and non-existing paths), write_all +
// read_all_into round-trip with byte-exact comparison, read_line_into line-by-
// line reading through to clean EOF (Ok = false), and read_all_into on an empty
// file returning Ok(0).
func TestStdlibIoResult(t *testing.T) {
	t.Parallel()

	code, output := compilePebbleTestFile(t, "io_result_test.peb")
	t.Logf("io_result_test.peb output:\n%s", output)
	if code != 0 {
		t.Fatalf("io_result_test.peb exited %d, want 0 (%d failures reported in output above)", code, code)
	}
}

// TestStdlibIoModule exercises the REAL std:io module imported via
// "import \"std:io\"": public accessor functions (is_ok_uint, ok_value_uint,
// err_message_uint, is_ok_bool, ok_value_bool, err_message_bool) via the
// public test helpers (test_write_all, test_read_all, test_read_line,
// test_roundtrip), plus open_checked and open_error_message.  Covers
// write+read round-trip, multi-line read with clean EOF, empty file,
// and real Err results with non-empty error messages.
func TestStdlibIoModule(t *testing.T) {
	t.Parallel()

	code, output := compilePebbleTestFile(t, "io_module_test.peb")
	t.Logf("io_module_test.peb output:\n%s", output)
	if code != 0 {
		t.Fatalf("io_module_test.peb exited %d, want 0 (%d failures reported in output above)", code, code)
	}
}
