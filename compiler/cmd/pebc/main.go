package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

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

// run parses the pebc command line and returns the process exit code.
func run(args []string, stdout, stderr io.Writer) int {
	flags := flag.NewFlagSet("pebc", flag.ContinueOnError)
	flags.SetOutput(stderr)
	flags.Usage = func() { printUsage(stderr) }

	outputPath := flags.String("o", "", "output executable path")
	runFlag := flags.Bool("run", false, "execute the built program and forward its exit code")
	releaseFlag := flags.Bool("release", false, "build without runtime safety checks")
	checkFlag := flags.Bool("check", false, "check the program only; build nothing")
	emitCFlag := flags.String("emit-c", "", "write the generated C source to a path and stop")
	preludeFlag := flags.String("prelude", "", "path to a prelude module parsed before the entry module")
	runtimeRootFlag := flags.String("runtime-root", "", "path to the runtime/ directory")
	var linkLibs, linkPaths, includePaths stringList
	flags.Var(&linkLibs, "l", "link against a library (repeatable)")
	flags.Var(&linkPaths, "L", "add a library search path (repeatable)")
	flags.Var(&includePaths, "I", "add a C include search path (repeatable)")

	if err := flags.Parse(args); err != nil {
		return 2
	}
	if flags.NArg() != 1 {
		flags.Usage()
		return 2
	}
	if *checkFlag && (*outputPath != "" || *runFlag || *emitCFlag != "") {
		fmt.Fprintln(stderr, "pebc: -check cannot be combined with -o, -run, or -emit-c")
		flags.Usage()
		return 2
	}
	if *emitCFlag != "" && (*outputPath != "" || *runFlag) {
		fmt.Fprintln(stderr, "pebc: -emit-c cannot be combined with -o or -run")
		flags.Usage()
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
	graph := module.Build(module.BuildConfig{EntryPath: string(entryPath), Package: "main", PreludePath: *preludeFlag, StandardRoot: stdlib.StandardRoot}, provider, sources, diagnostics)
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
	if *checkFlag {
		return 0
	}
	unit := result.IR()
	if unit == nil {
		fmt.Fprintln(stderr, "pebc: internal error: checker returned no typed IR")
		return 1
	}

	emitPath := *emitCFlag
	if emitPath == "" {
		dir, err := os.MkdirTemp("", "pebc-emit-")
		if err != nil {
			fmt.Fprintf(stderr, "pebc: cannot create temp dir: %v\n", err)
			return 1
		}
		defer os.RemoveAll(dir)
		emitPath = filepath.Join(dir, "program.c")
	}
	file, err := os.Create(emitPath)
	if err != nil {
		fmt.Fprintf(stderr, "pebc: cannot create output %q: %v\n", emitPath, err)
		return 1
	}
	if err := backend.Emit(unit, unit.Snapshot(), entryID, sources, resolution, file); err != nil {
		file.Close()
		fmt.Fprintf(stderr, "pebc: emission failed: %v\n", err)
		return 1
	}
	if err := file.Close(); err != nil {
		fmt.Fprintf(stderr, "pebc: cannot close output %q: %v\n", emitPath, err)
		return 1
	}
	if *emitCFlag != "" {
		return 0
	}

	binaryPath := *outputPath
	if binaryPath == "" {
		if *runFlag {
			dir, err := os.MkdirTemp("", "pebc-run-")
			if err != nil {
				fmt.Fprintf(stderr, "pebc: cannot create temp dir: %v\n", err)
				return 1
			}
			defer os.RemoveAll(dir)
			binaryPath = filepath.Join(dir, "program")
		} else {
			binaryPath = defaultBinaryPath(string(entryPath))
		}
	}
	if code := buildExecutable(*runtimeRootFlag, *releaseFlag, emitPath, binaryPath, []string(linkLibs), []string(linkPaths), []string(includePaths), stderr); code != 0 {
		return code
	}
	if *runFlag {
		return runBinary(binaryPath, stdout, stderr)
	}
	return 0
}

// stringList collects repeated string flags into a slice, so flags like -l,
// -L, and -I can be passed more than once.
type stringList []string

func (s *stringList) String() string { return strings.Join(*s, ",") }

func (s *stringList) Set(value string) error {
	*s = append(*s, value)
	return nil
}

// defaultBinaryPath returns the default output executable path for an entry
// file: its basename with the .peb extension stripped, relative to the
// working directory (matching go build's default-output convention).
func defaultBinaryPath(entryPath string) string {
	return strings.TrimSuffix(filepath.Base(entryPath), filepath.Ext(entryPath))
}

// buildExecutable compiles the emitted C at emittedPath with cc against a
// cached, prebuilt runtime static library into an executable at binaryPath,
// and returns 0 on success.
func buildExecutable(runtimeRootFlag string, release bool, emittedPath, binaryPath string, linkLibs, linkPaths, includePaths []string, stderr io.Writer) int {
	runtimeRoot, err := locateRuntimeRoot(runtimeRootFlag)
	if err != nil {
		fmt.Fprintf(stderr, "pebc: %v\n", err)
		return 1
	}
	cc, err := exec.LookPath("cc")
	if err != nil {
		fmt.Fprintf(stderr, "pebc: cc not on PATH: %v\n", err)
		return 1
	}
	define := "-DPEBBLE_RT_MODE_SAFE"
	if release {
		define = "-DPEBBLE_RT_MODE_RELEASE"
	}
	archive, err := runtimeArchive(runtimeRoot, define, cc)
	if err != nil {
		fmt.Fprintf(stderr, "pebc: %v\n", err)
		return 1
	}
	args := buildLinkArgs(runtimeRoot, define, emittedPath, binaryPath, []string{archive}, linkLibs, linkPaths, includePaths)
	if output, err := exec.Command(cc, args...).CombinedOutput(); err != nil {
		fmt.Fprintf(stderr, "pebc: cc compilation failed: %v\n%s", err, output)
		return 1
	}
	return 0
}

// buildCCArgs assembles the cc command line that compiles the emitted C at
// emittedPath into the executable at binaryPath, linking the runtime sources
// under runtimeRoot/src and any user-supplied -l/-L/-I flags.
func buildCCArgs(runtimeRoot, define, emittedPath, binaryPath string, srcFiles []string, linkLibs, linkPaths, includePaths []string) []string {
	return buildLinkArgs(runtimeRoot, define, emittedPath, binaryPath, srcFiles, linkLibs, linkPaths, includePaths)
}

// buildLinkArgs assembles the cc command line that compiles the emitted C at
// emittedPath into the executable at binaryPath, linking the given runtime
// inputs (source files or a prebuilt static library) and any user-supplied
// -l/-L/-I flags.
func buildLinkArgs(runtimeRoot, define, emittedPath, binaryPath string, runtimeInputs []string, linkLibs, linkPaths, includePaths []string) []string {
	args := []string{"-std=c11", "-Wall", "-Wextra", "-Werror", define, "-I" + filepath.Join(runtimeRoot, "include"), emittedPath}
	args = append(args, runtimeInputs...)
	for _, p := range includePaths {
		args = append(args, "-I"+p)
	}
	for _, p := range linkPaths {
		args = append(args, "-L"+p)
	}
	for _, lib := range linkLibs {
		args = append(args, "-l"+lib)
	}
	return append(args, "-o", binaryPath)
}

// runBinary executes the binary at binaryPath, forwarding its exit code.
func runBinary(binaryPath string, stdout, stderr io.Writer) int {
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

// printUsage writes the full pebc help text to w.
func printUsage(w io.Writer) {
	fmt.Fprint(w, usageText)
}

const usageText = `Pebble Compiler

Usage: pebc [flags] <entry.peb>

By default, pebc checks, compiles, and links <entry.peb> into a
runnable executable and stops. Use -run to also execute it immediately.

Flags:
  -o <path>            output executable path (default: entry file's
                       basename, written to the working directory)
  -run                 also execute the built program, forwarding its
                       exit code
  -release             build without runtime safety checks (default:
                       checks enabled)
  -check               check the program only; report errors, build
                       nothing
  -emit-c <path>       write the generated C source to <path> instead
                       of building an executable
  -l <library>         link against <library> (repeatable)
  -L <path>            add <path> to the linker search path (repeatable)
  -I <path>            add <path> to the C include search path (repeatable)
  -prelude <path>      parse <path> before the entry module; its
                       top-level declarations are visible everywhere
                       without an import
  -runtime-root <dir>  path to the runtime/ directory (auto-detected
                       from the working directory when omitted)

Examples:
  pebc main.peb                    build ./main
  pebc main.peb -run               build and run it immediately
  pebc main.peb -o build/app -run  build to a specific path, then run it
  pebc main.peb -release -run      run without safety checks
  pebc main.peb -check             just check for errors
  pebc main.peb -emit-c out.c      inspect the generated C
  pebc server.peb -l pthread -run  link against libpthread
`

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
