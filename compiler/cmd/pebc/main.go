package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/pepplejoshua/pebble/compiler/internal/backend"
	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/stdlib"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	flags := flag.NewFlagSet("pebc", flag.ContinueOnError)
	flags.SetOutput(stderr)
	outputPath := flags.String("o", "", "write emitted C to path instead of stdout")
	runFlag := flags.Bool("run", false, "compile the emitted C with cc and execute it, forwarding its exit code")
	releaseFlag := flags.Bool("release", false, "compile in release mode (no runtime safety checks) when -run is set")
	runtimeRootFlag := flags.String("runtime-root", "", "path to the runtime/ directory (auto-detected from the working directory when empty)")
	if err := flags.Parse(args); err != nil {
		return 2
	}
	if flags.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: pebc [-o path] [-run] [-release] [-runtime-root dir] <entry.peb>")
		return 2
	}

	provider := stdlib.New(module.FileSystemProvider{})
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	entryPath, err := provider.Canonicalize(flags.Arg(0))
	if err != nil {
		fmt.Fprintf(stderr, "pebc: cannot resolve entry %q: %v\n", flags.Arg(0), err)
		return 1
	}
	graph := module.Build(module.BuildConfig{EntryPath: string(entryPath), Package: "main", StandardRoot: stdlib.StandardRoot}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		fmt.Fprintf(stderr, "pebc: cannot initialize type store: %v\n", err)
		return 1
	}

	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entryID = candidate.ID
			break
		}
	}
	if entryID == 0 {
		if diagnostics.Len() > 0 {
			_ = diagnostic.RenderText(stderr, sources, diagnostics.Items())
		}
		fmt.Fprintln(stderr, "pebc: no main function found")
		return 1
	}

	inputs := check.Inputs{
		Graph: graph, Sources: sources, Resolution: resolution, Types: store,
		LiteralTarget: infer.LiteralTarget{WordBits: 64},
	}
	result := check.Check(inputs, diagnostics, check.Config{
		Entry: check.EntryPoint{Mode: check.EntryRequired, Symbol: entryID},
	})
	if !result.Successful() || diagnostics.Len() > 0 {
		if err := diagnostic.RenderText(stderr, sources, diagnostics.Items()); err != nil {
			fmt.Fprintf(stderr, "pebc: rendering diagnostics failed: %v\n", err)
		}
		return 1
	}
	unit := result.IR()
	if unit == nil {
		fmt.Fprintln(stderr, "pebc: internal error: checker returned no typed IR")
		return 1
	}

	var out io.Writer = stdout
	var file *os.File
	emitPath := *outputPath
	if emitPath == "" && *runFlag {
		dir, err := os.MkdirTemp("", "pebc-emit-")
		if err != nil {
			fmt.Fprintf(stderr, "pebc: cannot create temp dir: %v\n", err)
			return 1
		}
		defer os.RemoveAll(dir)
		emitPath = filepath.Join(dir, "program.c")
	}
	if emitPath != "" {
		file, err = os.Create(emitPath)
		if err != nil {
			fmt.Fprintf(stderr, "pebc: cannot create output %q: %v\n", emitPath, err)
			return 1
		}
		defer file.Close()
		out = file
	}
	if err := backend.Emit(unit, unit.Snapshot(), entryID, sources, out); err != nil {
		fmt.Fprintf(stderr, "pebc: emission failed: %v\n", err)
		return 1
	}
	if file != nil {
		if err := file.Close(); err != nil {
			fmt.Fprintf(stderr, "pebc: cannot close output %q: %v\n", emitPath, err)
			return 1
		}
	}
	if *runFlag {
		return compileAndRun(*runtimeRootFlag, *releaseFlag, emitPath, stdout, stderr)
	}
	return 0
}

// compileAndRun compiles the emitted C with cc against the runtime, executes
// the resulting binary, and returns the binary's own exit code.
func compileAndRun(runtimeRootFlag string, release bool, emittedPath string, stdout, stderr io.Writer) int {
	runtimeRoot, err := locateRuntimeRoot(runtimeRootFlag)
	if err != nil {
		fmt.Fprintf(stderr, "pebc: %v\n", err)
		return 1
	}
	cc, err := exec.LookPath("cc")
	if err != nil {
		fmt.Fprintf(stderr, "pebc: -run requires cc on PATH: %v\n", err)
		return 1
	}
	dir, err := os.MkdirTemp("", "pebc-run-")
	if err != nil {
		fmt.Fprintf(stderr, "pebc: cannot create temp dir: %v\n", err)
		return 1
	}
	defer os.RemoveAll(dir)
	binaryPath := filepath.Join(dir, "program")

	define := "-DPEBBLE_RT_MODE_SAFE"
	if release {
		define = "-DPEBBLE_RT_MODE_RELEASE"
	}
	srcFiles, err := filepath.Glob(filepath.Join(runtimeRoot, "src", "*.c"))
	if err != nil {
		fmt.Fprintf(stderr, "pebc: cannot glob runtime sources: %v\n", err)
		return 1
	}
	if len(srcFiles) == 0 {
		fmt.Fprintf(stderr, "pebc: no runtime sources found under %q\n", filepath.Join(runtimeRoot, "src"))
		return 1
	}

	args := []string{"-std=c11", "-Wall", "-Wextra", "-Werror", define, "-I" + filepath.Join(runtimeRoot, "include"), emittedPath}
	args = append(args, srcFiles...)
	args = append(args, "-o", binaryPath)
	if output, err := exec.Command(cc, args...).CombinedOutput(); err != nil {
		fmt.Fprintf(stderr, "pebc: cc compilation failed: %v\n%s", err, output)
		return 1
	}

	run := exec.Command(binaryPath)
	run.Stdout = stdout
	run.Stderr = stderr
	if err := run.Run(); err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			return exitErr.ExitCode()
		}
		fmt.Fprintf(stderr, "pebc: cannot execute compiled program: %v\n", err)
		return 1
	}
	return 0
}

// locateRuntimeRoot returns the runtime/ directory. An explicit override is
// used as-is; otherwise the working directory is walked upward (up to 6
// levels) looking for a directory containing both runtime/include and
// runtime/src.
func locateRuntimeRoot(override string) (string, error) {
	if override != "" {
		return override, nil
	}
	cwd, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("cannot determine working directory: %v", err)
	}
	dir := cwd
	for i := 0; i < 6; i++ {
		if isRuntimeDir(dir) {
			return filepath.Join(dir, "runtime"), nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", errors.New("cannot locate runtime/ — pass -runtime-root explicitly")
}

// isRuntimeDir reports whether dir contains a runtime/ subdirectory with both
// include/ and src/ present.
func isRuntimeDir(dir string) bool {
	for _, sub := range []string{"include", "src"} {
		info, err := os.Stat(filepath.Join(dir, "runtime", sub))
		if err != nil || !info.IsDir() {
			return false
		}
	}
	return true
}
