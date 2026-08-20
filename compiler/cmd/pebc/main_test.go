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
	if code := run([]string{"-emit-c", outPath, sourcePath}, &bytes.Buffer{}, &bytes.Buffer{}); code != 0 {
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
	if code := run([]string{"-emit-c", outPath, mainPath}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("multi-module run returned %d: %s", code, stderr.String())
	}
	emitted, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"static int64_t pebble_fn_", "return 7LL;", "pebble_fn_"} {
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
	if code := run([]string{"-emit-c", outPath, sourcePath}, &bytes.Buffer{}, &stderr); code != 0 {
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
	writeFile(t, mainPath, "fn main() int { let f Frobnicator = Frobnicator.{ quux = 42 }; return f.quux as int; }\n")
	outPath := filepath.Join(dir, "out.c")
	var stderr bytes.Buffer
	if code := run([]string{"-prelude", preludePath, "-emit-c", outPath, mainPath}, &bytes.Buffer{}, &stderr); code != 0 {
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
	requireCIntegration(t)
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skipf("skipping: cc not on PATH (%v)", err)
	}
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { println 42; return 7; }\n")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-run", sourcePath}, &stdout, &stderr); code != 7 {
		t.Fatalf("run returned %d, want 7; stderr=%q", code, stderr.String())
	}
	if got := stdout.String(); got != "42\n" {
		t.Fatalf("stdout = %q, want %q", got, "42\n")
	}
}

func TestRunFlagWithOutputPath(t *testing.T) {
	requireCIntegration(t)
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skipf("skipping: cc not on PATH (%v)", err)
	}
	repoRoot, err := findRepoRoot()
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { println 1; return 0; }\n")
	outPath := filepath.Join(dir, "app")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-runtime-root", filepath.Join(repoRoot, "runtime"), "-run", "-o", outPath, sourcePath}, &stdout, &stderr); code != 0 {
		t.Fatalf("run returned %d; stderr=%q", code, stderr.String())
	}
	if got := stdout.String(); got != "1\n" {
		t.Fatalf("stdout = %q, want %q", got, "1\n")
	}
	if _, err := os.Stat(outPath); err != nil {
		t.Fatalf("executable %q was not written: %v", outPath, err)
	}
}

func TestRunAutoDetectsRuntimeRoot(t *testing.T) {
	requireCIntegration(t)
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
	writeFile(t, sourcePath, "fn main() int { println 99; return 0; }\n")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-run", sourcePath}, &stdout, &stderr); code != 0 {
		t.Fatalf("run returned %d; stderr=%q", code, stderr.String())
	}
	if got := stdout.String(); got != "99\n" {
		t.Fatalf("stdout = %q, want %q", got, "99\n")
	}
}

func TestRunFlagReportsCompileError(t *testing.T) {
	requireCIntegration(t)
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

func TestDefaultBuildModeProducesExecutable(t *testing.T) {
	requireCIntegration(t)
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skipf("skipping: cc not on PATH (%v)", err)
	}
	repoRoot, err := findRepoRoot()
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	t.Chdir(dir)
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { println 7; return 0; }\n")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-runtime-root", filepath.Join(repoRoot, "runtime"), sourcePath}, &stdout, &stderr); code != 0 {
		t.Fatalf("default build returned %d; stderr=%q", code, stderr.String())
	}
	binaryPath := filepath.Join(dir, "main")
	if info, err := os.Stat(binaryPath); err != nil || info.IsDir() {
		t.Fatalf("executable %q was not created (err=%v)", binaryPath, err)
	}
	cmd := exec.Command(binaryPath)
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("executable failed: %v\n%s", err, output)
	}
	if got := string(output); got != "7\n" {
		t.Fatalf("executable stdout = %q, want %q", got, "7\n")
	}
}

func TestOutputFlagControlsExecutablePath(t *testing.T) {
	requireCIntegration(t)
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skipf("skipping: cc not on PATH (%v)", err)
	}
	repoRoot, err := findRepoRoot()
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	t.Chdir(dir)
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { println 8; return 0; }\n")
	binaryPath := filepath.Join(dir, "myapp")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-runtime-root", filepath.Join(repoRoot, "runtime"), "-o", binaryPath, sourcePath}, &stdout, &stderr); code != 0 {
		t.Fatalf("build returned %d; stderr=%q", code, stderr.String())
	}
	if info, err := os.Stat(binaryPath); err != nil || info.IsDir() {
		t.Fatalf("executable %q was not created (err=%v)", binaryPath, err)
	}
	cmd := exec.Command(binaryPath)
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("executable failed: %v\n%s", err, output)
	}
	if got := string(output); got != "8\n" {
		t.Fatalf("executable stdout = %q, want %q", got, "8\n")
	}
}

func TestCheckFlagSucceedsWithoutCC(t *testing.T) {
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { return 42; }\n")
	t.Setenv("PATH", "")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-check", sourcePath}, &stdout, &stderr); code != 0 {
		t.Fatalf("check returned %d; stderr=%q", code, stderr.String())
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 1 {
		t.Fatalf("check created unexpected output files in %q: %v", dir, entries)
	}
}

func TestCheckFlagReportsTypeErrorWithoutCC(t *testing.T) {
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { return missing(); }\n")
	t.Setenv("PATH", "")
	var stdout, stderr bytes.Buffer
	if code := run([]string{"-check", sourcePath}, &stdout, &stderr); code != 1 {
		t.Fatalf("check returned %d, want 1", code)
	}
	if stderr.Len() == 0 {
		t.Fatal("check produced no diagnostic")
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 1 {
		t.Fatalf("check created unexpected output files in %q: %v", dir, entries)
	}
}

func TestFlagModesAreMutuallyExclusive(t *testing.T) {
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { return 0; }\n")
	cases := [][]string{
		{"-emit-c", filepath.Join(dir, "out.c"), "-o", filepath.Join(dir, "app"), sourcePath},
		{"-emit-c", filepath.Join(dir, "out.c"), "-run", sourcePath},
		{"-check", "-o", filepath.Join(dir, "app"), sourcePath},
		{"-check", "-run", sourcePath},
		{"-check", "-emit-c", filepath.Join(dir, "out.c"), sourcePath},
	}
	for _, args := range cases {
		var stderr bytes.Buffer
		if code := run(args, &bytes.Buffer{}, &stderr); code != 2 {
			t.Errorf("run(%v) = %d, want 2; stderr=%q", args, code, stderr.String())
		}
	}
}

func TestLinkFlagsAcceptedAndRepeatable(t *testing.T) {
	dir := t.TempDir()
	sourcePath := filepath.Join(dir, "main.peb")
	writeFile(t, sourcePath, "fn main() int { return 0; }\n")
	outPath := filepath.Join(dir, "out.c")
	var stderr bytes.Buffer
	if code := run([]string{"-emit-c", outPath, "-l", "pthread", "-l", "m", "-L", "/a", "-L", "/b", "-I", "/x", "-I", "/y", sourcePath}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("emit-c with link flags returned %d: %s", code, stderr.String())
	}
	if _, err := os.Stat(outPath); err != nil {
		t.Fatalf("emitted C not written: %v", err)
	}
}

func TestBuildCCArgsIncludesLinkFlags(t *testing.T) {
	args := buildCCArgs("/rt", "-DPEBBLE_RT_MODE_SAFE", "prog.c", "out", []string{"a.c", "b.c"}, []string{"pthread", "m"}, []string{"/usr/local/lib"}, []string{"/usr/local/include"})
	joined := strings.Join(args, " ")
	for _, want := range []string{"-I/rt/include", "prog.c", "a.c", "b.c", "-I/usr/local/include", "-L/usr/local/lib", "-lpthread", "-lm", "-o", "out"} {
		if !strings.Contains(joined, want) {
			t.Errorf("cc args %q missing %q", joined, want)
		}
	}
}

func TestNoArgsPrintsHelp(t *testing.T) {
	var stderr bytes.Buffer
	if code := run(nil, &bytes.Buffer{}, &stderr); code == 0 {
		t.Fatal("no-args run unexpectedly succeeded")
	}
	for _, want := range []string{"Usage: pebc", "Flags:", "Examples:", "-emit-c"} {
		if !strings.Contains(stderr.String(), want) {
			t.Errorf("help output missing %q:\n%s", want, stderr.String())
		}
	}
}

func TestHelpFlagPrintsUsage(t *testing.T) {
	var stderr bytes.Buffer
	if code := run([]string{"-h"}, &bytes.Buffer{}, &stderr); code == 0 {
		t.Fatal("-h unexpectedly exited 0")
	}
	for _, want := range []string{"Usage: pebc", "Flags:", "Examples:"} {
		if !strings.Contains(stderr.String(), want) {
			t.Errorf("help output missing %q:\n%s", want, stderr.String())
		}
	}
}

func TestUnknownFlagPrintsUsage(t *testing.T) {
	var stderr bytes.Buffer
	if code := run([]string{"-bogus"}, &bytes.Buffer{}, &stderr); code == 0 {
		t.Fatal("unknown flag unexpectedly exited 0")
	}
	if !strings.Contains(stderr.String(), "Usage: pebc") {
		t.Errorf("help output missing usage:\n%s", stderr.String())
	}
}

// setExecutableDirForTest redirects executableDirFunc to a fixed directory for
// the duration of the test, simulating an installed pebc whose std/ and
// runtime/ siblings live in that directory. It returns a restore function.
func setExecutableDirForTest(t *testing.T, dir string) func() {
	t.Helper()
	orig := executableDirFunc
	executableDirFunc = func() string { return dir }
	return func() { executableDirFunc = orig }
}

// chdirForTest changes the working directory for the duration of the test,
// creating dir (and any parents) first if needed.
func chdirForTest(t *testing.T, dir string) {
	t.Helper()
	oldWD, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.Chdir(oldWD) })
}

// makeFakeRuntime creates a minimal runtime/ layout (include/ + src/) under
// dir and returns its path.
func makeFakeRuntime(t *testing.T, dir string) string {
	t.Helper()
	rt := filepath.Join(dir, "runtime")
	for _, sub := range []string{"include", "src"} {
		if err := os.MkdirAll(filepath.Join(rt, sub), 0o755); err != nil {
			t.Fatal(err)
		}
	}
	return rt
}

// makeFakeStd creates a minimal std/ layout (set.peb, vec.peb, mem/) under dir
// and returns its path.
func makeFakeStd(t *testing.T, dir string) string {
	t.Helper()
	std := filepath.Join(dir, "std")
	if err := os.MkdirAll(std, 0o755); err != nil {
		t.Fatal(err)
	}
	for _, name := range []string{"set.peb", "vec.peb"} {
		if err := os.WriteFile(filepath.Join(std, name), []byte("fn __fake() void {}\n"), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.MkdirAll(filepath.Join(std, "mem"), 0o755); err != nil {
		t.Fatal(err)
	}
	return std
}

// makeFakeCheckout creates a minimal checkout root (runtime/ anchor plus
// compiler/std) and returns it.
func makeFakeCheckout(t *testing.T) string {
	t.Helper()
	root := t.TempDir()
	makeFakeRuntime(t, root)
	if err := os.MkdirAll(filepath.Join(root, "compiler"), 0o755); err != nil {
		t.Fatal(err)
	}
	makeFakeStd(t, filepath.Join(root, "compiler"))
	return root
}

func TestLocateRuntimeRootPrefersBinaryRelative(t *testing.T) {
	dir := t.TempDir()
	binaryRT := makeFakeRuntime(t, dir)
	checkout := makeFakeCheckout(t)
	defer setExecutableDirForTest(t, dir)()
	chdirForTest(t, filepath.Join(checkout, "deep", "nested"))

	got, err := locateRuntimeRoot("")
	if err != nil {
		t.Fatalf("locateRuntimeRoot: %v", err)
	}
	if got != binaryRT {
		t.Fatalf("locateRuntimeRoot = %q, want binary-relative %q", got, binaryRT)
	}
}

func TestLocateStdRootPrefersBinaryRelative(t *testing.T) {
	dir := t.TempDir()
	binaryStd := makeFakeStd(t, dir)
	checkout := makeFakeCheckout(t)
	defer setExecutableDirForTest(t, dir)()
	chdirForTest(t, filepath.Join(checkout, "deep", "nested"))

	got, err := locateStdRoot()
	if err != nil {
		t.Fatalf("locateStdRoot: %v", err)
	}
	if got != binaryStd {
		t.Fatalf("locateStdRoot = %q, want binary-relative %q", got, binaryStd)
	}
}

func TestLocateRuntimeRootFallsBackToCwdWalk(t *testing.T) {
	checkout := makeFakeCheckout(t)
	defer setExecutableDirForTest(t, t.TempDir())()
	chdirForTest(t, filepath.Join(checkout, "a", "b", "c"))

	got, err := locateRuntimeRoot("")
	if err != nil {
		t.Fatalf("locateRuntimeRoot: %v", err)
	}
	want := filepath.Join(checkout, "runtime")
	// os.Getwd() resolves the /var -> /private/var symlink, so normalize the
	// expected path the same way.
	resolved, rerr := filepath.EvalSymlinks(want)
	if rerr == nil {
		want = resolved
	}
	if got != want {
		t.Fatalf("locateRuntimeRoot = %q, want cwd-walk-up %q", got, want)
	}
}

func TestLocateStdRootFallsBackToCwdWalk(t *testing.T) {
	checkout := makeFakeCheckout(t)
	defer setExecutableDirForTest(t, t.TempDir())()
	chdirForTest(t, filepath.Join(checkout, "a", "b", "c"))

	got, err := locateStdRoot()
	if err != nil {
		t.Fatalf("locateStdRoot: %v", err)
	}
	want := filepath.Join(checkout, "compiler", "std")
	resolved, rerr := filepath.EvalSymlinks(want)
	if rerr == nil {
		want = resolved
	}
	if got != want {
		t.Fatalf("locateStdRoot = %q, want cwd-walk-up %q", got, want)
	}
}

func TestLocateRuntimeRootOverrideStillWins(t *testing.T) {
	defer setExecutableDirForTest(t, t.TempDir())()
	chdirForTest(t, makeFakeCheckout(t))

	override := filepath.Join(t.TempDir(), "custom-runtime")
	if err := os.MkdirAll(override, 0o755); err != nil {
		t.Fatal(err)
	}
	got, err := locateRuntimeRoot(override)
	if err != nil {
		t.Fatalf("locateRuntimeRoot: %v", err)
	}
	if got != override {
		t.Fatalf("locateRuntimeRoot = %q, want override %q", got, override)
	}
}

func TestLocateRuntimeRootNotFoundErrors(t *testing.T) {
	defer setExecutableDirForTest(t, "")()
	chdirForTest(t, t.TempDir())

	if _, err := locateRuntimeRoot(""); err == nil {
		t.Fatal("locateRuntimeRoot unexpectedly succeeded with no runtime anywhere")
	}
}

func TestLocateStdRootNotFoundFallsThroughToEmbed(t *testing.T) {
	defer setExecutableDirForTest(t, "")()
	chdirForTest(t, t.TempDir())

	if _, err := locateStdRoot(); err == nil {
		t.Fatal("locateStdRoot unexpectedly succeeded with no std anywhere")
	}
	// realStdlibPath returns "" when no on-disk std tree can be located, so the
	// caller keeps the synthetic embedded path (the go:embed fallback).
	if got := realStdlibPath("std:embedded/set.peb"); got != "" {
		t.Fatalf("realStdlibPath = %q, want \"\" (fall back to embedded stdlib)", got)
	}
	if got := realStdlibPath("std:embedded/nope.peb"); got != "" {
		t.Fatalf("realStdlibPath = %q, want \"\" for unknown embedded module", got)
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
	requireCIntegration(t)
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

func requireCIntegration(t *testing.T) {
	t.Helper()
	if testing.Short() {
		t.Skip("skipping C compile-and-run integration test in short mode")
	}
}

type commandError struct {
	command string
	output  []byte
	err     error
}

func (e *commandError) Error() string {
	return e.command + ": " + e.err.Error() + "\n" + string(e.output)
}
