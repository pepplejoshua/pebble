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
