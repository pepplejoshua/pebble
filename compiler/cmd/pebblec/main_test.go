package main

import (
	"bytes"
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
