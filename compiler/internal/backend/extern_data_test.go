package backend

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// compileAndRunWithShim is compileAndRun for programs that reference an extern
// variable: an extern variable's real storage lives in another translation
// unit, so the test links the emitted C against a small test-only C shim (the
// shimSource argument, a file-scope definition of the variable) in addition to
// the runtime objects, mirroring the compileEmittedC invocation shape under
// -Wall -Wextra -Werror. The shim's external-linkage variable is used by the
// emitted C, so neither translation unit trips -Wunused-variable.
func compileAndRunWithShim(t *testing.T, emitted []byte, shimSource string, wantCode int) {
	t.Helper()
	requireCIntegration(t)
	cc, err := exec.LookPath("cc")
	if err != nil {
		t.Skipf("skipping end-to-end check: cc not on PATH (%v)", err)
	}
	dir := t.TempDir()
	program := filepath.Join(dir, "program.c")
	if err := os.WriteFile(program, emitted, 0o644); err != nil {
		t.Fatalf("write emitted C: %v", err)
	}
	shim := filepath.Join(dir, "shim.c")
	if err := os.WriteFile(shim, []byte(shimSource), 0o644); err != nil {
		t.Fatalf("write shim C: %v", err)
	}
	binary := filepath.Join(dir, "program")
	runtimeRoot := runtimeSourceRoot(t)
	objectsDir, err := cachedRuntimeObjects(cc, runtimeRoot)
	if err != nil {
		if runtimeCCMissing {
			t.Skipf("skipping end-to-end check: cc not on PATH (%v)", err)
		}
		t.Fatalf("compiling cached runtime objects: %v", err)
	}
	compileArgs := []string{
		"-std=c11",
		"-Wall", "-Wextra", "-Werror",
		"-DPEBBLE_RT_MODE_SAFE",
		"-I", filepath.Join(runtimeRoot, "include"),
		program, shim,
	}
	for _, sourceFile := range runtimeSourceFiles {
		compileArgs = append(compileArgs, filepath.Join(objectsDir, strings.TrimSuffix(sourceFile, ".c")+".o"))
	}
	compileArgs = append(compileArgs, "-o", binary)
	compile := exec.Command(cc, compileArgs...)
	if output, err := compile.CombinedOutput(); err != nil {
		t.Fatalf("cc compilation failed: %v\n%s", err, output)
	}
	runCompiledBinary(t, binary, wantCode, false, false)
}

// TestEmitExternDataReadInitialValueCompilesAndRuns proves an extern variable
// read resolves to the variable's REAL C name and observes its external value:
// the shim defines shim_seed = 5, the Pebble program declares
// `extern { var shim_seed int; }` and `return shim_seed;`, and the linked
// program must exit 5 — the exact read shape of the parity-gap reproduction,
// with a self-contained C shim standing in for libc's errno.
func TestEmitExternDataReadInitialValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, "extern {\n    var shim_seed int;\n}\n\nfn main() int {\n    return shim_seed;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRunWithShim(t, buf.Bytes(), "#include <stdint.h>\nint32_t shim_seed = 5;\n", 5)
}

// TestEmitExternDataReadWritesCDeclaration pins the emitted-C shape of the
// exact tracker reproduction (the errno program): a referenced extern variable
// must emit a forward `extern <ctype> <name>;` declaration using its REAL C
// name — never a synthesized pebble_global_/pebble_local_ storage name — and a
// read must resolve to that same real C name. errno itself cannot link on this
// platform (it is a macro, not a symbol), so the shape is pinned as text and
// the runnable interop is proven by the shim tests.
func TestEmitExternDataReadWritesCDeclaration(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, "extern {\n    var errno int;\n}\n\nfn main() int {\n    return errno;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"extern int64_t errno;",
		"return errno;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_global_") || strings.Contains(out, "pebble_local_") {
		t.Errorf("emitted C references a synthesized storage name for an extern variable, want only its real C name:\n%s", out)
	}
}

// TestEmitExternDataWriteAcrossFunctionsCompilesAndRuns proves a write to an
// extern variable in one function is observed by a read in another — real
// shared mutable state across the translation-unit boundary: bump() increments
// the shim-backed counter twice, main returns the counter, and the linked
// program must exit 2.
func TestEmitExternDataWriteAcrossFunctionsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `extern {
    var shim_counter int;
}

fn bump() void {
    shim_counter = shim_counter + 1;
}

fn main() int {
    bump();
    bump();
    return shim_counter;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRunWithShim(t, buf.Bytes(), "#include <stdint.h>\nint32_t shim_counter = 0;\n", 2)
}

// TestEmitExternDataCompoundAssignmentCompilesAndRuns covers the compound-assign
// path for an extern variable place (`shim_count += 5;`), which resolves through
// buildCompoundStore's extern-variable branch and writes the real C name.
func TestEmitExternDataCompoundAssignmentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `extern {
    var shim_count int;
}

fn main() int {
    shim_count += 5;
    return shim_count;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRunWithShim(t, buf.Bytes(), "#include <stdint.h>\nint32_t shim_count = 10;\n", 15)
}

// TestEmitExternDataUintReadCompilesAndRuns covers a uint-typed extern variable
// read (buildUintExpr's resolution path): the shim defines shim_u = 3 and the
// program's comparison against it must observe 3. The shim's C type must agree
// with the emitted declaration, which uses the fixed-width uint spelling
// (uint64_t), not uint.
func TestEmitExternDataUintReadCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, "extern {\n    var shim_u uint;\n}\n\nfn main() int {\n    if shim_u == 3 {\n        return 9;\n    }\n    return 0;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRunWithShim(t, buf.Bytes(), "#include <stdint.h>\nuint64_t shim_u = 3;\n", 9)
}

// TestEmitExternDataBoolReadCompilesAndRuns covers a bool-typed extern variable
// read (buildBoolExpr's resolution path).
func TestEmitExternDataBoolReadCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, "extern {\n    var shim_flag bool;\n}\n\nfn main() int {\n    if shim_flag {\n        return 7;\n    }\n    return 0;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRunWithShim(t, buf.Bytes(), "#include <stdbool.h>\nbool shim_flag = true;\n", 7)
}

// TestEmitExternDataCoexistsWithGlobalCompilesAndRuns proves the extern-variable
// forward declarations and mutable-global storage definitions share the file-
// scope region without interfering: a program that both reads an extern variable
// and reads/writes a var global compiles and runs, observing both.
func TestEmitExternDataCoexistsWithGlobalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `extern {
    var shim_base int;
}

var counter int = 5;

fn main() int {
    counter = counter + shim_base;
    return counter;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRunWithShim(t, buf.Bytes(), "#include <stdint.h>\nint32_t shim_base = 3;\n", 8)
}

// TestEmitExternDataUnusedDoesNotEmitDeclaration guards against emitting a
// forward declaration for an extern variable the reachable program never
// references: an unused extern variable needs no declaration (and may not even
// exist in the linked libraries), so the program must still compile and run
// with the identifier absent from the emitted C.
func TestEmitExternDataUnusedDoesNotEmitDeclaration(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, "extern {\n    var shim_unused int;\n}\n\nfn main() int {\n    return 0;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if strings.Contains(out, "shim_unused") {
		t.Errorf("emitted C declares an unused extern variable, want none:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

// TestEmitExternDataMissingSymbolTableRejected guards the clean-error path:
// a referenced extern variable needs its real C name from the symbol table, so
// Emit called without a symbol result must reject with an
// extern-variable-specific error rather than emit an undeclared identifier or
// guess a name.
func TestEmitExternDataMissingSymbolTableRejected(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, _ := buildFixtureWithSymbols(t, "extern {\n    var shim_seed int;\n}\n\nfn main() int {\n    return shim_seed;\n}")
	var buf bytes.Buffer
	err := Emit(unit, snapshot, entryID, sources, nil, &buf)
	if err == nil {
		t.Fatal("Emit accepted a referenced extern variable without a symbol table")
	}
	if !strings.Contains(err.Error(), "extern variable symbol") {
		t.Fatalf("unexpected rejection: %v", err)
	}
	if buf.Len() != 0 {
		t.Fatalf("Emit wrote output on failure: %q", buf.String())
	}
}
