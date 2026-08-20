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

	"github.com/pepplejoshua/pebble/compiler/internal/module"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

// run parses the pebc command line and returns the process exit code.
func run(args []string, stdout, stderr io.Writer) int {
	// The daemon subcommand owns its own argument parsing and lifecycle.
	// It must be dispatched before the one-shot flags are parsed so that
	// `pebc daemon ...` is never misread as a missing entry file.
	if len(args) > 0 && args[0] == "daemon" {
		return runDaemon(args[1:], stdout, stderr)
	}
	if len(args) > 0 && args[0] == "dev" {
		return runDev(args[1:], stdout, stderr)
	}
	if len(args) > 0 && args[0] == "lsp" {
		return runLSP(args[1:], stdout, stderr)
	}

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

	if _, err := providerCanonicalize(flags.Arg(0)); err != nil {
		fmt.Fprintf(stderr, "pebc: cannot resolve entry %q: %v\n", flags.Arg(0), err)
		return 1
	}

	mode := modeBuild
	if *checkFlag {
		mode = modeCheck
	} else if *emitCFlag != "" {
		mode = modeEmitC
	}
	binaryPath := *outputPath
	if binaryPath == "" && *runFlag {
		dir, err := os.MkdirTemp("", "pebc-run-")
		if err != nil {
			fmt.Fprintf(stderr, "pebc: cannot create temp dir: %v\n", err)
			return 1
		}
		defer os.RemoveAll(dir)
		binaryPath = filepath.Join(dir, "program")
	}
	res := compileOnce(compileRequest{
		mode:         mode,
		entryPath:    flags.Arg(0),
		outputPath:   binaryPath,
		emitCPath:    *emitCFlag,
		prelude:      *preludeFlag,
		runtimeRoot:  *runtimeRootFlag,
		release:      *releaseFlag,
		linkLibs:     []string(linkLibs),
		linkPaths:    []string(linkPaths),
		includePaths: []string(includePaths),
		stderr:       stderr,
	})
	if res.code != 0 {
		return res.code
	}
	if *runFlag {
		if res.binaryPath == "" {
			res.binaryPath = binaryPath
		}
		return runBinary(res.binaryPath, stdout, stderr)
	}
	return 0
}

// providerCanonicalize resolves an entry path to its canonical form for the
// one-shot CLI. It exists so the entry-path check in run() remains visible
// before compileOnce runs the pipeline.
func providerCanonicalize(path string) (module.CanonicalPath, error) {
	return (module.FileSystemProvider{}).Canonicalize(path)
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

Subcommands:
  pebc daemon start|build|ping|stop|watch-status
                       manage a persistent background compiler daemon
                       for the current project root
  pebc dev <entry.peb> watch for changes and automatically rebuild and
                       restart the built program on save
  pebc lsp             run a Language Server Protocol server over stdio
                       (for editor integration)

Examples:
  pebc main.peb                    build ./main
  pebc main.peb -run               build and run it immediately
  pebc main.peb -o build/app -run  build to a specific path, then run it
  pebc main.peb -release -run      run without safety checks
  pebc main.peb -check             just check for errors
  pebc main.peb -emit-c out.c      inspect the generated C
  pebc server.peb -l pthread -run  link against libpthread
  pebc dev main.peb                fast rebuild-restart on save
  pebc lsp                         run as an LSP server (for editors)
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

// locateStdRoot returns the on-disk std/ directory the embedded standard
// library was compiled from (compiler/std under the checkout root, the sibling
// of runtime/), walking up from the working directory exactly the way
// locateRuntimeRoot does. It returns an error when no such directory can be
// found, so callers can fall back to the synthetic embedded paths.
func locateStdRoot() (string, error) {
	cwd, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("cannot determine working directory: %v", err)
	}
	dir := cwd
	for i := 0; i < 6; i++ {
		if isRuntimeDir(dir) {
			for _, candidate := range []string{
				filepath.Join(dir, "compiler", "std"),
				filepath.Join(dir, "std"),
			} {
				if isStdDir(candidate) {
					return candidate, nil
				}
			}
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", errors.New("cannot locate std/ directory (no checkout found by walking up from the working directory)")
}

// isStdDir reports whether dir looks like the compiler's embedded stdlib
// source tree: the .peb modules and mem/ subdirectory that go:embed packs.
func isStdDir(dir string) bool {
	for _, entry := range []string{"set.peb", "vec.peb", "mem"} {
		info, err := os.Stat(filepath.Join(dir, entry))
		if err != nil {
			return false
		}
		if entry == "mem" && !info.IsDir() {
			return false
		}
	}
	return true
}

