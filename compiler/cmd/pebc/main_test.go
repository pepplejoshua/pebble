package main

import (
	"bytes"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestRunSingleFileEmitsRunnableC(t *testing.T) {
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { return 42; }\n")
	outPath := filepath.Join(dir, "out.c")
	if code := run([]string{"-o", outPath, sourcePath}, &bytes.Buffer{}, &bytes.Buffer{}); code != 0 {
		t.Fatalf("run returned %d", code)
	}
	emitted, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatal(err)
	}
	if err := compileEmittedC(t, dir, emitted, "single", 42); err != nil {
		t.Fatal(err)
	}
}

func TestRunReportsTypeError(t *testing.T) {
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { return missing(); }\n")
	var stderr bytes.Buffer
	if code := run([]string{sourcePath}, &bytes.Buffer{}, &stderr); code == 0 {
		t.Fatal("run unexpectedly succeeded")
	}
	if stderr.Len() == 0 {
		t.Fatal("run produced no diagnostic")
	}
}

func TestRunReportsMissingEntryPoint(t *testing.T) {
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn helper() void {}\n")
	var stderr bytes.Buffer
	if code := run([]string{sourcePath}, &bytes.Buffer{}, &stderr); code == 0 {
		t.Fatal("run unexpectedly succeeded")
	}
	if !strings.Contains(stderr.String(), "no main function found") {
		t.Fatalf("missing-entry diagnostic = %q", stderr.String())
	}
}

func TestRunMultiModuleImportEmitsRunnableC(t *testing.T) {
	dir := t.TempDir()
	mainPath := filepath.Join(dir, "main.peb")
	writeFile(t, mainPath, "import \"./helper\";\n\nfn main() int { if helper::answer() == 7 { return 7; } else { return 0; } }\n")
	writeFile(t, filepath.Join(dir, "helper.peb"), "fn answer() int { return 7; }\n")
	outPath := filepath.Join(dir, "out.c")
	var stderr bytes.Buffer
	if code := run([]string{"-o", outPath, mainPath}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("multi-module run returned %d: %s", code, stderr.String())
	}
	emitted, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"static int32_t pebble_fn_", "return 7;", "pebble_fn_"} {
		if !strings.Contains(string(emitted), want) {
			t.Fatalf("emitted C missing %q:\n%s", want, emitted)
		}
	}
	if err := compileEmittedC(t, dir, emitted, "multi", 7); err != nil {
		t.Fatal(err)
	}
}

func TestRunStdImportEmitsRunnableC(t *testing.T) {
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "import \"std:mem\";\n\nfn main() int { var values []int = mem::new_slice[int](3); values[0] = 42; return values[0]; }\n")
	outPath := filepath.Join(dir, "out.c")
	var stderr bytes.Buffer
	if code := run([]string{"-o", outPath, sourcePath}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("std-import run returned %d: %s", code, stderr.String())
	}
	emitted, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(emitted), "M0002") {
		t.Fatalf("emitted C contains module-resolution diagnostics:\n%s", emitted)
	}
	if err := compileEmittedC(t, dir, emitted, "std", 42); err != nil {
		t.Fatal(err)
	}
}

func TestRunPreludeFlagEmitsRunnableC(t *testing.T) {
	dir := t.TempDir()
	// Since the Allocator/Context cutover, an explicit -prelude REPLACES the
	// compiler's embedded default runtime prelude (every compilation sees
	// Allocator/Context from the prelude), so a custom prelude must provide the
	// runtime types itself. Read the real runtime prelude and extend it with a
	// user type, proving the -prelude mechanism still injects declarations
	// visible to every module without an import.
	runtimePrelude, err := os.ReadFile("../../prelude/runtime.peb")
	if err != nil {
		t.Fatal(err)
	}
	preludePath := filepath.Join(dir, "prelude.peb")
	writeFile(t, preludePath, string(runtimePrelude)+"\ntype Frobnicator = struct { quux i32; };\n")
	mainPath := filepath.Join(dir, "main.peb")
	writeFile(t, mainPath, "fn main() int { let f Frobnicator = Frobnicator.{ quux = 42 }; return f.quux; }\n")
	outPath := filepath.Join(dir, "out.c")
	var stderr bytes.Buffer
	if code := run([]string{"-prelude", preludePath, "-o", outPath, mainPath}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("prelude run returned %d: %s", code, stderr.String())
	}
	emitted, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatal(err)
	}
	if err := compileEmittedC(t, dir, emitted, "prelude", 42); err != nil {
		t.Fatal(err)
	}
}

func TestRunFlagCompilesAndRuns(t *testing.T) {
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skipf("skipping: cc not on PATH (%v)", err)
	}
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { print 42; return 7; }\n")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-run", sourcePath}, &stdout, &stderr); code != 7 {
		t.Fatalf("run returned %d, want 7; stderr=%q", code, stderr.String())
	}
	if got := stdout.String(); got != "42\n" {
		t.Fatalf("stdout = %q, want %q", got, "42\n")
	}
}

func TestRunFlagWithOutputPath(t *testing.T) {
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skipf("skipping: cc not on PATH (%v)", err)
	}
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { print 1; return 0; }\n")
	outPath := filepath.Join(dir, "out.c")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-run", "-o", outPath, sourcePath}, &stdout, &stderr); code != 0 {
		t.Fatalf("run returned %d; stderr=%q", code, stderr.String())
	}
	if got := stdout.String(); got != "1\n" {
		t.Fatalf("stdout = %q, want %q", got, "1\n")
	}
	emitted, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("output file %q was not written: %v", outPath, err)
	}
	if !strings.Contains(string(emitted), "pebble_rt.h") {
		t.Fatalf("emitted C at %q looks wrong:\n%s", outPath, emitted)
	}
}

func TestRunAutoDetectsRuntimeRoot(t *testing.T) {
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skipf("skipping: cc not on PATH (%v)", err)
	}
	repoRoot, err := findRepoRoot()
	if err != nil {
		t.Fatal(err)
	}
	oldWD, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	if err := os.Chdir(repoRoot); err != nil {
		t.Fatal(err)
	}
	defer os.Chdir(oldWD)
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { print 99; return 0; }\n")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-run", sourcePath}, &stdout, &stderr); code != 0 {
		t.Fatalf("run returned %d; stderr=%q", code, stderr.String())
	}
	if got := stdout.String(); got != "99\n" {
		t.Fatalf("stdout = %q, want %q", got, "99\n")
	}
}

func TestRunFlagReportsCompileError(t *testing.T) {
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skipf("skipping: cc not on PATH (%v)", err)
	}
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { return 0; }\n")
	var stderr bytes.Buffer
	if code := run([]string{"-run", "-runtime-root", filepath.Join(dir, "bogus"), sourcePath}, &bytes.Buffer{}, &stderr); code == 0 {
		t.Fatal("run unexpectedly succeeded")
	}
	if stderr.Len() == 0 {
		t.Fatal("run produced no diagnostic")
	}
}

func findRepoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 6; i++ {
		if _, err := os.Stat(filepath.Join(dir, "runtime", "include")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", errors.New("cannot locate repo root")
}

func writeFile(t *testing.T, path, contents string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(contents), 0o600); err != nil {
		t.Fatal(err)
	}
}

func compileEmittedC(t *testing.T, dir string, emitted []byte, name string, expectedCode int) error {
	t.Helper()
	runtimeRoot := filepath.Join("..", "..", "..", "runtime")
	outputPath := filepath.Join(dir, name)
	cPath := filepath.Join(dir, name+".c")
	if err := os.WriteFile(cPath, emitted, 0o600); err != nil {
		return err
	}
	args := []string{"-std=c11", "-Wall", "-Wextra", "-Werror", "-DPEBBLE_RT_MODE_SAFE", "-I" + filepath.Join(runtimeRoot, "include"), cPath}
	for _, source := range []string{"context.c", "panic.c", "platform_host.c", "arith.c", "bounds.c", "optional.c", "str.c"} {
		args = append(args, filepath.Join(runtimeRoot, "src", source))
	}
	args = append(args, "-o", outputPath)
	if output, err := exec.Command("cc", args...).CombinedOutput(); err != nil {
		return &commandError{command: "cc", output: output, err: err}
	}
	command := exec.Command(outputPath)
	if err := command.Run(); err != nil {
		if exit, ok := err.(*exec.ExitError); ok && exit.ExitCode() == expectedCode {
			return nil
		}
		return &commandError{command: outputPath, err: err}
	}
	return &commandError{command: outputPath, err: os.ErrInvalid}
}

type commandError struct {
	command string
	output  []byte
	err     error
}

func (e *commandError) Error() string {
	return e.command + ": " + e.err.Error() + "\n" + string(e.output)
}
