package backend

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"testing"
	"time"
)

var (
	runtimeObjectsOnce sync.Once
	runtimeObjectsDir  string
	runtimeObjectsErr  error
	runtimeCCMissing   bool
)

var runtimeSourceFiles = []string{
	"context.c",
	"panic.c",
	"platform_host.c",
	"arith.c",
	"bounds.c",
	"optional.c",
	"str.c",
	"deref.c",
}

func TestMain(m *testing.M) {
	code := m.Run()
	if runtimeObjectsDir != "" {
		_ = os.RemoveAll(runtimeObjectsDir)
	}
	os.Exit(code)
}

// fixtureProvider serves module source from an in-memory map, mirroring the
// check package's own checkProvider test double so a .peb source string can
// run through the full pipeline exactly the way check tests build units.
type fixtureProvider map[module.CanonicalPath][]byte

func (p fixtureProvider) Canonicalize(path string) (module.CanonicalPath, error) {
	key := module.CanonicalPath(path)
	if _, ok := p[key]; !ok {
		return "", fmt.Errorf("missing %s", path)
	}
	return key, nil
}

func (p fixtureProvider) ReadFile(path module.CanonicalPath) ([]byte, error) {
	value, ok := p[path]
	if !ok {
		return nil, fmt.Errorf("missing %s", path)
	}
	return append([]byte(nil), value...), nil
}

// buildFixture runs one .peb source through the full check pipeline and
// returns the resulting typed-IR unit, its type snapshot, and the resolved
// entry symbol ID. With requireEntry set, the unit is built under
// check.EntryRequired, the same configuration entry_validation_test.go uses;
// without it, no entry validation runs, so fixtures with shapes the checker
// itself would reject as entries (parameters, non-void results) still build
// and let Emit's own validation be exercised directly.
func buildFixture(t *testing.T, sourceText, entryName string, requireEntry bool) (*tir.Unit, *types.Snapshot, symbol.SymbolID, *source.FileSet) {
	t.Helper()
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "facts"}, fixtureProvider{"main.peb": []byte(sourceText)}, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	inputs := check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}

	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == entryName {
			entryID = candidate.ID
		}
	}
	if entryID == 0 {
		t.Fatalf("missing symbol %q", entryName)
	}

	config := check.Config{}
	if requireEntry {
		config.Entry = check.EntryPoint{Mode: check.EntryRequired, Symbol: entryID}
	}
	result := check.Check(inputs, diagnostics, config)
	if !result.Successful() {
		t.Fatalf("check failed: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("check succeeded without an IR unit")
	}
	return unit, unit.Snapshot(), entryID, sources
}

// buildFixtureWithSymbols is buildFixture for programs that call C-convention
// extern functions: an extern call lowers to its real C name (malloc, not
// pebble_fn_<symbolID>), which requires the symbol table to map the extern
// declaration's SymbolID back to its authored identifier, so this helper also
// returns the symbol.Result the backend must be given.
func buildFixtureWithSymbols(t *testing.T, sourceText string) (*tir.Unit, *types.Snapshot, symbol.SymbolID, *source.FileSet, *symbol.Result) {
	t.Helper()
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "facts"}, fixtureProvider{"main.peb": []byte(sourceText)}, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entryID = candidate.ID
		}
	}
	if entryID == 0 {
		t.Fatalf("missing symbol %q", "main")
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{Entry: check.EntryPoint{Mode: check.EntryRequired, Symbol: entryID}})
	if !result.Successful() {
		t.Fatalf("check failed: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("check succeeded without an IR unit")
	}
	return unit, unit.Snapshot(), entryID, sources, resolution
}

func buildStdFixture(t *testing.T, sourceText, entryName string) (*tir.Unit, *types.Snapshot, symbol.SymbolID, *source.FileSet) {
	t.Helper()
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: module.StandardPackage}, fixtureProvider{"main.peb": []byte(sourceText)}, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == entryName {
			entryID = candidate.ID
		}
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed: %+v", diagnostics.Items())
	}
	return result.IR(), result.IR().Snapshot(), entryID, sources
}

func buildStdMemFixture(t *testing.T, sourceText, entryName string) (*tir.Unit, *types.Snapshot, symbol.SymbolID, *source.FileSet) {
	t.Helper()
	mem, err := os.ReadFile("../../std/mem.peb")
	if err != nil {
		t.Fatal(err)
	}
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{"main.peb": []byte(sourceText), "std/mem.peb": mem}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == entryName {
			entryID = candidate.ID
		}
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed: %+v", diagnostics.Items())
	}
	return result.IR(), result.IR().Snapshot(), entryID, sources
}

func emitRuntimeAndRun(t *testing.T, sourceText string, wantCode int) {
	t.Helper()
	requireCIntegration(t)
	unit, snapshot, entryID, sources := buildStdFixture(t, sourceText, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), wantCode, false)
}

// emitAndRun drives one .peb entry source through buildFixture, Emit, and the
// end-to-end cc compile + run. wantCode is the expected process exit code;
// with wantAbnormal set, the process must instead terminate abnormally (a
// non-zero exit or a signal, as abort() produces) rather than exiting with
// any specific code.
func emitAndRun(t *testing.T, sourceText string, requireEntry bool, wantCode int, wantAbnormal bool) {
	t.Helper()
	requireCIntegration(t)
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", requireEntry)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), wantCode, wantAbnormal)
}

// emitAndRunBounded is emitAndRun for programs that contain a while loop: the
// program's execution is bounded by the loop's own condition, so the compiled
// binary is run through the bounded harness (compileAndRunBounded) rather than
// the unbounded compileAndRun. A miscompiled non-terminating loop therefore
// fails the test loudly and quickly instead of hanging the whole test run.
func emitAndRunBounded(t *testing.T, sourceText string, requireEntry bool, wantCode int, wantAbnormal bool) {
	t.Helper()
	requireCIntegration(t)
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", requireEntry)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRunBounded(t, buf.Bytes(), wantCode, wantAbnormal)
}

// compileAndRun cc's already-emitted C against the runtime in
// PEBBLE_RT_MODE_SAFE (the same configuration every end-to-end test here
// uses), runs the binary, and asserts the exit behavior. With wantAbnormal,
// the process must terminate abnormally; otherwise its exit code must equal
// wantCode.
func compileAndRun(t *testing.T, emitted []byte, wantCode int, wantAbnormal bool) {
	t.Helper()
	binary := compileEmittedC(t, emitted)
	runCompiledBinary(t, binary, wantCode, wantAbnormal, false)
}

// loopExecutionTimeout bounds the execution of a compiled program whose
// termination is guaranteed only by its own loop conditions. A genuinely
// non-terminating while loop would otherwise hang the Go test process forever;
// with this timeout the run fails loudly and quickly instead.
const loopExecutionTimeout = 5 * time.Second

// compileEmittedC cc's already-emitted C against the runtime in
// PEBBLE_RT_MODE_SAFE (the same configuration every end-to-end test here uses)
// under -Wall -Wextra -Werror, and returns the path to the compiled binary.
// Every emitted local is followed by a (void) cast (see buildLeadingStatement)
// specifically so this backend's own output is immune to -Wunused-variable
// regardless of whether a test fixture happens to read the local afterward, so
// the strict flags apply uniformly — there is no lenient path to opt into.
func compileEmittedC(t *testing.T, emitted []byte) string {
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
		program,
	}
	for _, sourceFile := range runtimeSourceFiles {
		compileArgs = append(compileArgs, filepath.Join(objectsDir, strings.TrimSuffix(sourceFile, ".c")+".o"))
	}
	compileArgs = append(compileArgs, "-o", binary)
	compile := exec.Command(cc, compileArgs...)
	if output, err := compile.CombinedOutput(); err != nil {
		t.Fatalf("cc compilation failed: %v\n%s", err, output)
	}
	return binary
}

func compileEmittedCRelease(t *testing.T, emitted []byte) string {
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
	binary := filepath.Join(dir, "program")
	runtimeRoot := runtimeSourceRoot(t)
	compileArgs := []string{
		"-std=c11", "-Wall", "-Wextra", "-Werror",
		"-DPEBBLE_RT_MODE_RELEASE",
		"-I", filepath.Join(runtimeRoot, "include"), program,
	}
	for _, sourceFile := range runtimeSourceFiles {
		compileArgs = append(compileArgs, filepath.Join(runtimeRoot, "src", sourceFile))
	}
	compileArgs = append(compileArgs, "-o", binary)
	compile := exec.Command(cc, compileArgs...)
	if output, err := compile.CombinedOutput(); err != nil {
		t.Fatalf("cc RELEASE compilation failed: %v\n%s", err, output)
	}
	return binary
}

func cachedRuntimeObjects(cc, runtimeRoot string) (string, error) {
	runtimeObjectsOnce.Do(func() {
		if _, err := exec.LookPath("cc"); err != nil {
			runtimeCCMissing = true
			runtimeObjectsErr = err
			return
		}
		runtimeObjectsDir, runtimeObjectsErr = os.MkdirTemp("", "pebble-backend-runtime-")
		if runtimeObjectsErr != nil {
			return
		}
		for _, sourceFile := range runtimeSourceFiles {
			objectFile := filepath.Join(runtimeObjectsDir, strings.TrimSuffix(sourceFile, ".c")+".o")
			compile := exec.Command(cc,
				"-std=c11",
				"-Wall", "-Wextra", "-Werror",
				"-DPEBBLE_RT_MODE_SAFE",
				"-I", filepath.Join(runtimeRoot, "include"),
				"-c", filepath.Join(runtimeRoot, "src", sourceFile),
				"-o", objectFile,
			)
			if output, err := compile.CombinedOutput(); err != nil {
				runtimeObjectsErr = fmt.Errorf("%s: %w\n%s", sourceFile, err, output)
				return
			}
		}
	})
	return runtimeObjectsDir, runtimeObjectsErr
}

// compileAndRunBounded is compileAndRun for programs whose execution is not
// statically guaranteed to terminate: a while loop's only bound is its own
// condition, so a buggy or non-terminating loop could otherwise hang the Go
// test process forever. It wraps the compiled binary's execution in the
// loopExecutionTimeout context so a genuinely non-terminating program fails
// the test loudly and quickly instead of hanging the run, while a program
// that terminates promptly (normally, or abnormally via a panic such as the
// overflow abort) finishes well before the deadline. Behavior is otherwise
// identical to compileAndRun: wantCode is the expected exit code, and with
// wantAbnormal the process must terminate abnormally (a non-zero exit or a
// signal, as abort() produces) rather than exiting with any specific code.
func compileAndRunBounded(t *testing.T, emitted []byte, wantCode int, wantAbnormal bool) {
	t.Helper()
	binary := compileEmittedC(t, emitted)
	runCompiledBinary(t, binary, wantCode, wantAbnormal, true)
}

// runCompiledBinary runs one already-compiled binary and asserts its exit
// behavior, discarding the process's combined stdout+stderr. With bounded set,
// execution is wrapped in the loopExecutionTimeout context so a genuinely
// non-terminating program (a miscompiled while loop) fails the test loudly and
// quickly instead of hanging the run; a program that terminates promptly —
// normally, or abnormally via a panic such as the overflow abort — finishes
// well before the deadline. With wantAbnormal, the process must terminate
// abnormally (a non-zero exit or a signal, as abort() produces); otherwise its
// exit code must equal wantCode.
func runCompiledBinary(t *testing.T, binary string, wantCode int, wantAbnormal, bounded bool) {
	t.Helper()
	runCompiledBinaryCapture(t, binary, wantCode, wantAbnormal, bounded)
}

// runCompiledBinaryCapture is runCompiledBinary with one difference: it
// returns the process's combined stdout+stderr (as a string) alongside the
// same exit-behavior assertions. The captured output is what a print-statement
// test asserts on — an exit code alone cannot carry printed text — so the run
// logic lives here and the output-free runCompiledBinary delegates to it.
func runCompiledBinaryCapture(t *testing.T, binary string, wantCode int, wantAbnormal, bounded bool) string {
	t.Helper()
	var run *exec.Cmd
	if bounded {
		ctx, cancel := context.WithTimeout(context.Background(), loopExecutionTimeout)
		defer cancel()
		run = exec.CommandContext(ctx, binary)
	} else {
		run = exec.Command(binary)
	}
	output, err := run.CombinedOutput()
	if run.ProcessState == nil {
		t.Fatalf("compiled program did not start: %v\n%s", err, output)
	}
	if errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("compiled program timed out after %s (a non-terminating loop?), err=%v\n%s", loopExecutionTimeout, err, output)
	}
	code := run.ProcessState.ExitCode()
	if wantAbnormal {
		// CombinedOutput returns a non-nil error for any non-zero exit or
		// signal; a clean exit 0 would mean the overflow check never fired.
		// In bounded execution this branch runs only after the deadline check
		// above, so reaching it proves the abnormal termination is a genuine
		// panic, not a timeout.
		if err == nil {
			t.Fatalf("compiled program exited 0, want abnormal termination\n%s", output)
		}
		t.Logf("compiled program terminated abnormally (err=%v, exit code %d): %s", err, code, output)
		return string(output)
	}
	// A non-zero exit is expected behavior for some programs (the exit code
	// IS the program's output), so the run error is not itself a failure —
	// only a mismatch with the wanted code is. A signaled process would
	// report exit code -1 and fail the comparison.
	if code != wantCode {
		t.Fatalf("compiled program exited %d, want %d\n%s", code, wantCode, output)
	}
	t.Logf("compiled program exited %d, want %d", code, wantCode)
	return string(output)
}

// compileAndRunCapture is compileAndRun with one difference: it returns the
// compiled program's combined stdout+stderr (as a string) alongside the same
// exit-behavior assertions, so a test can assert on printed text, not just the
// exit code.
func compileAndRunCapture(t *testing.T, emitted []byte, wantCode int, wantAbnormal bool) string {
	t.Helper()
	binary := compileEmittedC(t, emitted)
	return runCompiledBinaryCapture(t, binary, wantCode, wantAbnormal, false)
}

// emitAndRunCapture is emitAndRun with one difference: it returns the compiled
// program's combined stdout+stderr (as a string) alongside the same exit
// assertions, so a test can assert on actual printed output rather than only
// the exit code. It drives one .peb entry source through buildFixture, Emit,
// and the end-to-end cc compile + run.
func emitAndRunCapture(t *testing.T, sourceText string, requireEntry bool, wantCode int, wantAbnormal bool) string {
	t.Helper()
	requireCIntegration(t)
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", requireEntry)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	return compileAndRunCapture(t, buf.Bytes(), wantCode, wantAbnormal)
}

// compileAndRunCaptureBounded is compileAndRunCapture for programs whose
// execution is not statically guaranteed to terminate (a while loop's only
// bound is its own condition): execution is wrapped in the loopExecutionTimeout
// context so a genuinely non-terminating program fails the test loudly and
// quickly instead of hanging the run. It returns the captured stdout+stderr.
func compileAndRunCaptureBounded(t *testing.T, emitted []byte, wantCode int, wantAbnormal bool) string {
	t.Helper()
	binary := compileEmittedC(t, emitted)
	return runCompiledBinaryCapture(t, binary, wantCode, wantAbnormal, true)
}

// emitAndRunCaptureBounded is emitAndRunCapture for loop-containing programs,
// mirroring emitAndRunBounded: the compiled binary runs through the bounded
// harness so a miscompiled non-terminating loop fails loudly instead of
// hanging the whole test run. It returns the captured stdout+stderr.
func emitAndRunCaptureBounded(t *testing.T, sourceText string, requireEntry bool, wantCode int, wantAbnormal bool) string {
	t.Helper()
	requireCIntegration(t)
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", requireEntry)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	return compileAndRunCaptureBounded(t, buf.Bytes(), wantCode, wantAbnormal)
}

// buildI32EmptyBodyUnit hand-builds a unit whose entry has an i32 result type
// and a completely empty body block. The checker refuses to produce this shape
// itself (a non-void function must not fall through without returning), so it
// is constructed directly through the IR builder to exercise Emit's own body
// validation. The type snapshot is borrowed from a checker-built fixture so
// every TypeID the hand-built nodes reference is owned by the snapshot.
func buildI32EmptyBodyUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	block, err := builder.AddNode(tir.Node{
		Kind:   tir.Block,
		Region: region,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     entryID,
		Function:   fid,
		ResultType: snapshot.Builtins().I32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(fid, block); err != nil {
		t.Fatal(err)
	}
	unit, err := builder.Build()
	if err != nil {
		t.Fatalf("builder rejected the hand-built unit: %v", err)
	}
	return unit, snapshot, entryID
}

// buildBoolLocalReturnUnit hand-builds a unit whose i32 entry body is an
// Initialize declaring a bool local (symbol 25, bound to a true BoolLiteral)
// and a Return whose value is a bool-typed SymbolValue referencing that same
// symbol. The checker rejects this exact shape from source itself (C0601:
// cannot convert a bool for an i32 return value, confirmed against a real
// fixture), so it is constructed directly through the IR builder to exercise
// Emit's own requirement that every value in an accepted expression tree is
// typed to the entry's integer width — buildExpr's width gate must reject the
// bool-typed reference in the integer return position.
func buildBoolLocalReturnUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})

	initValue, err := builder.AddNode(tir.Node{
		Kind:    tir.BoolLiteral,
		Type:    snapshot.Builtins().Bool,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralBool, Bool: true},
	})
	if err != nil {
		t.Fatal(err)
	}
	init, err := builder.AddNode(tir.Node{
		Kind:     tir.Initialize,
		Symbol:   25,
		Children: []tir.NodeID{initValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	// The return references symbol 25, the bool local declared above, as a
	// bool-typed value: Emit's integer-return path must reject it.
	value, err := builder.AddNode(tir.Node{
		Kind:   tir.SymbolValue,
		Type:   snapshot.Builtins().Bool,
		Symbol: 25,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{value},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{init, ret})
}

// buildNonI32ReturnUnit hand-builds a unit whose i32 entry returns a bool
// literal. The checker would reject this shape itself (a bool does not unify
// with an i32 result), so it is constructed directly through the IR builder to
// exercise Emit's own requirement that every value in an accepted expression
// tree is typed i32.
func buildNonI32ReturnUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	literal, err := builder.AddNode(tir.Node{
		Kind:    tir.BoolLiteral,
		Type:    snapshot.Builtins().Bool,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralBool, Bool: true},
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{literal},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	block, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{ret},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     entryID,
		Function:   fid,
		ResultType: snapshot.Builtins().I32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(fid, block); err != nil {
		t.Fatal(err)
	}
	unit, err := builder.Build()
	if err != nil {
		t.Fatalf("builder rejected the hand-built unit: %v", err)
	}
	return unit, snapshot, entryID
}

// buildUnsupportedArithmeticOperatorUnit hand-builds a unit whose i32 entry
// returns a CheckedArithmetic node carrying an operator the backend does not
// map to a helper (division/modulo are now mapped, so no source-level
// CheckedArithmetic carries an unmapped operator — this shape is constructed
// directly through the IR builder to exercise Emit's own rejection of that
// branch). The type snapshot is borrowed from a checker-built fixture so every
// TypeID the hand-built nodes reference is owned by the snapshot.
func buildUnsupportedArithmeticOperatorUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	left, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "1"},
	})
	if err != nil {
		t.Fatal(err)
	}
	right, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "2"},
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	// A bitwise operator is integral but is not one the backend lowers to a
	// checked runtime helper; a CheckedArithmetic node carrying it must be a
	// clean rejection.
	value, err := builder.AddNode(tir.Node{
		Kind:     tir.CheckedArithmetic,
		Type:     i32,
		Operator: syntax.Ampersand,
		Children: []tir.NodeID{left, right},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{value},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	block, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{ret},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     entryID,
		Function:   fid,
		ResultType: i32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(fid, block); err != nil {
		t.Fatal(err)
	}
	unit, err := builder.Build()
	if err != nil {
		t.Fatalf("builder rejected the hand-built unit: %v", err)
	}
	return unit, snapshot, entryID
}

// buildUndeclaredLocalReferenceUnit hand-builds a unit whose i32 entry
// declares one local (Initialize for symbol 25) but whose Return references a
// different, never-declared symbol (a SymbolValue for symbol 26). The checker
// would never produce this from valid source — the reference would fail name
// resolution first — so it is constructed directly through the IR builder to
// exercise Emit's own requirement that a SymbolValue reference only symbols
// declared earlier in the entry body. The type snapshot is borrowed from a
// checker-built fixture so every TypeID the hand-built nodes reference is
// owned by the snapshot.
func buildUndeclaredLocalReferenceUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	initValue, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "1"},
	})
	if err != nil {
		t.Fatal(err)
	}
	init, err := builder.AddNode(tir.Node{
		Kind:     tir.Initialize,
		Symbol:   25,
		Children: []tir.NodeID{initValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	// Symbol 26 is never declared by any Initialize in the body, so the
	// return's reference to it must be a clean Emit rejection.
	undeclared, err := builder.AddNode(tir.Node{
		Kind:   tir.SymbolValue,
		Type:   i32,
		Symbol: 26,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{undeclared},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	block, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{init, ret},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     entryID,
		Function:   fid,
		ResultType: i32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(fid, block); err != nil {
		t.Fatal(err)
	}
	unit, err := builder.Build()
	if err != nil {
		t.Fatalf("builder rejected the hand-built unit: %v", err)
	}
	return unit, snapshot, entryID
}

// buildStatementsInBodyUnit finishes a hand-built i32 entry on builder: it
// adds the body Block carrying the given already-added statement nodes and the
// FunctionDeclaration, then completes the function and builds the unit. The
// region and function declaration are shared by every hand-built fixture in
// this file, so only the statement sequence differs. The type snapshot is
// borrowed from a checker-built fixture so every TypeID the hand-built nodes
// reference is owned by the snapshot.
func buildStatementsInBodyUnit(t *testing.T, builder *tir.Builder, snapshot *types.Snapshot, entryID symbol.SymbolID, fid tir.FunctionID, blockChildren []tir.NodeID) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	block, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: blockChildren,
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     entryID,
		Function:   fid,
		ResultType: snapshot.Builtins().I32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(fid, block); err != nil {
		t.Fatal(err)
	}
	unit, err := builder.Build()
	if err != nil {
		t.Fatalf("builder rejected the hand-built unit: %v", err)
	}
	return unit, snapshot, entryID
}

// addI32Literal adds an IntegerLiteral node carrying the given non-negative
// decimal text, typed to the snapshot's i32 builtin.
func addI32Literal(t *testing.T, builder *tir.Builder, i32 types.TypeID, num string) tir.NodeID {
	t.Helper()
	id, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: num},
	})
	if err != nil {
		t.Fatal(err)
	}
	return id
}

// buildStoreToUndeclaredSymbolUnit hand-builds a unit whose i32 entry body is
// an Initialize (symbol 25 bound to 1), a Store whose StoragePlace names a
// different symbol (26) that no Initialize ever declares, and the final Return
// of 1. Real source can never produce this shape — a reassignment of an
// undeclared name fails name resolution first — so it is constructed directly
// through the IR builder, the same pattern buildI32EmptyBodyUnit uses, to
// exercise Emit's own requirement that a Store targets a local already
// declared earlier in the entry body.
func buildStoreToUndeclaredSymbolUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	init, err := builder.AddNode(tir.Node{
		Kind:     tir.Initialize,
		Symbol:   25,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	storeValue, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "2"},
	})
	if err != nil {
		t.Fatal(err)
	}
	// Symbol 26 is never declared by any Initialize in the body, so the
	// Store's place for it must be a clean Emit rejection.
	place, err := builder.AddNode(tir.Node{
		Kind:     tir.StoragePlace,
		Type:     i32,
		Symbol:   26,
		Writable: true,
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	store, err := builder.AddNode(tir.Node{
		Kind:     tir.Store,
		Children: []tir.NodeID{place, storeValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{init, store, ret})
}

// buildNonI32StoreValueUnit hand-builds a unit whose i32 entry body is an
// Initialize (symbol 25 bound to 1), a Store that reassigns symbol 25 to a
// bool literal, and the final Return of 1. The checker rejects this shape
// itself (a bool does not unify with the i32 local's type — T0505), so it is
// constructed directly through the IR builder to exercise Emit's own
// requirement that a reassignment's new value is a valid i32 expression.
func buildNonI32StoreValueUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	init, err := builder.AddNode(tir.Node{
		Kind:     tir.Initialize,
		Symbol:   25,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	storeValue, err := builder.AddNode(tir.Node{
		Kind:    tir.BoolLiteral,
		Type:    snapshot.Builtins().Bool,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralBool, Bool: true},
	})
	if err != nil {
		t.Fatal(err)
	}
	place, err := builder.AddNode(tir.Node{
		Kind:     tir.StoragePlace,
		Type:     i32,
		Symbol:   25,
		Writable: true,
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	store, err := builder.AddNode(tir.Node{
		Kind:     tir.Store,
		Children: []tir.NodeID{place, storeValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{init, store, ret})
}

// buildStoreToNonStoragePlaceUnit hand-builds a unit whose i32 entry body is
// an Initialize (symbol 25 bound to 1), a Store whose first child is a
// TuplePlace rather than a plain StoragePlace, and the final Return of 1.
// Real source can never produce this shape for an i32 local — reassigning a
// whole tuple/struct element in place is not supported (only
// StoragePlace/CheckedIndexPlace/DereferencePlace are accepted Store
// targets, since pointers landed) — so it is constructed directly through
// the IR builder to exercise Emit's own requirement that a Store's place is
// one of those three kinds.
func buildStoreToNonStoragePlaceUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	init, err := builder.AddNode(tir.Node{
		Kind:     tir.Initialize,
		Symbol:   25,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	storeValue, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "2"},
	})
	if err != nil {
		t.Fatal(err)
	}
	tupleBase, err := builder.AddNode(tir.Node{
		Kind:     tir.StoragePlace,
		Type:     i32,
		Symbol:   25,
		Writable: true,
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	place, err := builder.AddNode(tir.Node{
		Kind:     tir.TuplePlace,
		Type:     i32,
		Writable: true,
		Children: []tir.NodeID{tupleBase},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	store, err := builder.AddNode(tir.Node{
		Kind:     tir.Store,
		Children: []tir.NodeID{place, storeValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{init, store, ret})
}

// buildStrConcatReassignmentUnit hand-builds a unit whose i32 entry body is an
// Initialize (symbol 25 declared as a str local from the literal "hi"), a
// Store reassigning that str local from a str-typed BinaryValue (the lowering
// `s = "h" + "i"` produces for string concatenation), and the final Return of
// 0. Real source can no longer produce this shape — the checker rejects
// `str + str` (C0603) before the backend ever sees it — so it is constructed
// directly through the IR builder to keep exercising Emit's own rejection of a
// str local reassigned from anything other than a string literal. The type
// snapshot is borrowed from a checker-built fixture so every TypeID the
// hand-built nodes reference is owned by the snapshot.
func buildStrConcatReassignmentUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	str := snapshot.Builtins().Str
	i32 := snapshot.Builtins().I32

	init, err := builder.AddNode(tir.Node{
		Kind:     tir.Initialize,
		Symbol:   25,
		Children: []tir.NodeID{addStrLiteral(t, builder, str, "hi")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	left, err := builder.AddNode(tir.Node{
		Kind:    tir.StringLiteral,
		Type:    str,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralString, String: "h"},
	})
	if err != nil {
		t.Fatal(err)
	}
	right, err := builder.AddNode(tir.Node{
		Kind:    tir.StringLiteral,
		Type:    str,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralString, String: "i"},
	})
	if err != nil {
		t.Fatal(err)
	}
	concat, err := builder.AddNode(tir.Node{
		Kind:     tir.BinaryValue,
		Type:     str,
		Operator: syntax.Plus,
		Children: []tir.NodeID{left, right},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	place, err := builder.AddNode(tir.Node{
		Kind:     tir.StoragePlace,
		Type:     str,
		Symbol:   25,
		Writable: true,
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	store, err := builder.AddNode(tir.Node{
		Kind:     tir.Store,
		Children: []tir.NodeID{place, concat},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "0")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{init, store, ret})
}

// addStrLiteral adds a StringLiteral node carrying the given raw text, typed to
// the snapshot's str builtin.
func addStrLiteral(t *testing.T, builder *tir.Builder, str types.TypeID, text string) tir.NodeID {
	t.Helper()
	id, err := builder.AddNode(tir.Node{
		Kind:    tir.StringLiteral,
		Type:    str,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralString, String: text},
	})
	if err != nil {
		t.Fatal(err)
	}
	return id
}

// buildIfWithoutElseUnit hand-builds a unit whose i32 entry ends with a tir.If
// that has no else arm (HasElse unset, two children: a bool comparison and the
// then-arm block). The checker refuses to produce this shape from source (a
// non-void function must not fall through without returning — C0607), so it is
// constructed directly through the IR builder to exercise Emit's own
// requirement that the final if has an else. The type snapshot is borrowed
// from a checker-built fixture so every TypeID the hand-built nodes reference
// is owned by the snapshot.
func buildIfWithoutElseUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	left, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "1"},
	})
	if err != nil {
		t.Fatal(err)
	}
	right, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "2"},
	})
	if err != nil {
		t.Fatal(err)
	}
	condition, err := builder.AddNode(tir.Node{
		Kind:     tir.BinaryValue,
		Type:     snapshot.Builtins().Bool,
		Operator: syntax.Less,
		Children: []tir.NodeID{left, right},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	thenValue, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "10"},
	})
	if err != nil {
		t.Fatal(err)
	}
	thenReturn, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{thenValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	thenBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{thenReturn},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	// HasElse deliberately left false: the final if must have an else arm for
	// this backend to accept it.
	ifNode, err := builder.AddNode(tir.Node{
		Kind:     tir.If,
		Region:   region,
		HasElse:  false,
		Children: []tir.NodeID{condition, thenBlock},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	block, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{ifNode},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     entryID,
		Function:   fid,
		ResultType: i32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(fid, block); err != nil {
		t.Fatal(err)
	}
	unit, err := builder.Build()
	if err != nil {
		t.Fatalf("builder rejected the hand-built unit: %v", err)
	}
	return unit, snapshot, entryID
}

// buildWhileAsTailUnit hand-builds a unit whose i32 entry's body block has a
// single child: a While loop, i.e. the loop is the block's last (and only)
// child. The checker refuses to produce this shape from source — a non-void
// function must not fall through without returning (C0607), so a trailing
// while is rejected before typed IR exists — which is exactly why the loop
// can only ever be a leading statement in this backend's block grammar. The
// unit is constructed directly through the IR builder to exercise Emit's own
// tail requirement: a block's last child must be a Return or a two-armed
// if/else, so a While there must be a clean rejection. The While itself is
// otherwise well-formed (a BinaryValue comparison for the condition and a
// Block body) so the rejection is specifically about its position, not its
// internals. The type snapshot is borrowed from a checker-built fixture so
// every TypeID the hand-built nodes reference is owned by the snapshot.
func buildWhileAsTailUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	left, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "1"},
	})
	if err != nil {
		t.Fatal(err)
	}
	right, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "2"},
	})
	if err != nil {
		t.Fatal(err)
	}
	condition, err := builder.AddNode(tir.Node{
		Kind:     tir.BinaryValue,
		Type:     snapshot.Builtins().Bool,
		Operator: syntax.Less,
		Children: []tir.NodeID{left, right},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	bodyBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: nil,
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	whileNode, err := builder.AddNode(tir.Node{
		Kind:     tir.While,
		Region:   region,
		Children: []tir.NodeID{condition, bodyBlock},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	// The entry body block's only child is the While — the loop is the tail.
	block, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{whileNode},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     entryID,
		Function:   fid,
		ResultType: i32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(fid, block); err != nil {
		t.Fatal(err)
	}
	unit, err := builder.Build()
	if err != nil {
		t.Fatalf("builder rejected the hand-built unit: %v", err)
	}
	return unit, snapshot, entryID
}

// buildWhileTrueWithBreakAsTailUnit hand-builds a unit whose i32 entry body's
// only child is a While whose condition is the literal boolean true but whose
// loop body contains a Break targeting the loop's own Region (and a Return, so
// the rejection is specifically about the break). Real source can never produce
// this shape as a non-void tail: the checker rejects a while-true loop with a
// break as a non-void function tail (C0607 — the break lets control fall
// through past the loop), so it is constructed directly through the IR builder,
// the same pattern buildWhileAsTailUnit uses, to exercise Emit's own
// terminalWhileIsExhaustive criterion that an accepted terminal while must
// contain no Break whose Target is the loop's Region. The type snapshot is
// borrowed from a checker-built fixture so every TypeID the hand-built nodes
// reference is owned by the snapshot.
func buildWhileTrueWithBreakAsTailUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32
	boolT := snapshot.Builtins().Bool

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	condition, err := builder.AddNode(tir.Node{
		Kind:    tir.BoolLiteral,
		Type:    boolT,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralBool, Bool: true},
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	brk, err := builder.AddNode(tir.Node{
		Kind:   tir.Break,
		Target: region,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	bodyBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{brk, ret},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	whileNode, err := builder.AddNode(tir.Node{
		Kind:     tir.While,
		Region:   region,
		Children: []tir.NodeID{condition, bodyBlock},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{whileNode})
}

// buildSiblingArmLocalLeakUnit hand-builds a unit whose i32 entry is a
// two-armed if/else where the then-arm declares a local (symbol 25, bound to
// 100) but the else-arm's return references that same symbol 25. Real source
// can never produce this shape — a reference to a name that only exists in
// the other arm fails name resolution first — so it is constructed directly
// through the IR builder, the same pattern buildIfWithoutElseUnit uses, to
// exercise Emit's own per-scope copy discipline: the else-arm must not see the
// then-arm's local. If the locals map were threaded through without copying,
// the else-arm would accept symbol 25 and emit a reference to a
// pebble_local_25 declared only inside the then-arm's C block. The type
// snapshot is borrowed from a checker-built fixture so every TypeID the
// hand-built nodes reference is owned by the snapshot.
func buildSiblingArmLocalLeakUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32
	boolT := snapshot.Builtins().Bool

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	left, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "1"},
	})
	if err != nil {
		t.Fatal(err)
	}
	right, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "2"},
	})
	if err != nil {
		t.Fatal(err)
	}
	condition, err := builder.AddNode(tir.Node{
		Kind:     tir.BinaryValue,
		Type:     boolT,
		Operator: syntax.Less,
		Children: []tir.NodeID{left, right},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	// The then-arm declares symbol 25 and returns it.
	thenValue, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "100"},
	})
	if err != nil {
		t.Fatal(err)
	}
	thenInit, err := builder.AddNode(tir.Node{
		Kind:     tir.Initialize,
		Symbol:   25,
		Children: []tir.NodeID{thenValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	thenRef, err := builder.AddNode(tir.Node{
		Kind:   tir.SymbolValue,
		Type:   i32,
		Symbol: 25,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	thenReturn, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{thenRef},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	thenBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{thenInit, thenReturn},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	// The else-arm returns the same symbol 25 — the then-arm's local, which
	// must not be in scope here.
	elseRef, err := builder.AddNode(tir.Node{
		Kind:   tir.SymbolValue,
		Type:   i32,
		Symbol: 25,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	elseReturn, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{elseRef},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	elseBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{elseReturn},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	ifNode, err := builder.AddNode(tir.Node{
		Kind:     tir.If,
		Region:   region,
		HasElse:  true,
		Children: []tir.NodeID{condition, thenBlock, elseBlock},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	block, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{ifNode},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     entryID,
		Function:   fid,
		ResultType: i32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(fid, block); err != nil {
		t.Fatal(err)
	}
	unit, err := builder.Build()
	if err != nil {
		t.Fatalf("builder rejected the hand-built unit: %v", err)
	}
	return unit, snapshot, entryID
}

// buildLoopIfArmLocalLeakUnit hand-builds a unit whose i32 entry is a while
// loop whose body contains a two-armed if where the then-arm declares a local
// (symbol 25, bound to 1) but the else-arm's Store targets that same symbol
// 25. Real source can never produce this shape — a reference to a name that
// only exists in the other arm fails name resolution first — so it is
// constructed directly through the IR builder, the same pattern
// buildSiblingArmLocalLeakUnit uses, to exercise Emit's own per-scope copy
// discipline inside a loop-body if: each arm is built by buildLoopBody, which
// clones the incoming locals, so the else-arm must not see the then-arm's
// local. If the locals map were threaded through without copying, the else-arm
// would accept symbol 25 and emit a reference to a pebble_local_25 declared
// only inside the then-arm's C block. The type snapshot is borrowed from a
// checker-built fixture so every TypeID the hand-built nodes reference is owned
// by the snapshot.
func buildLoopIfArmLocalLeakUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32
	boolT := snapshot.Builtins().Bool

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}

	addCompare := func(leftNum, rightNum string) tir.NodeID {
		t.Helper()
		left, err := builder.AddNode(tir.Node{
			Kind:    tir.IntegerLiteral,
			Type:    i32,
			Span:    source.NewSpan(0, 0, 1),
			Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: leftNum},
		})
		if err != nil {
			t.Fatal(err)
		}
		right, err := builder.AddNode(tir.Node{
			Kind:    tir.IntegerLiteral,
			Type:    i32,
			Span:    source.NewSpan(0, 0, 1),
			Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: rightNum},
		})
		if err != nil {
			t.Fatal(err)
		}
		condition, err := builder.AddNode(tir.Node{
			Kind:     tir.BinaryValue,
			Type:     boolT,
			Operator: syntax.Less,
			Children: []tir.NodeID{left, right},
			Span:     source.NewSpan(0, 0, 1),
		})
		if err != nil {
			t.Fatal(err)
		}
		return condition
	}

	// The while and if conditions are both 1 < 2 (always true), built as
	// separate nodes since each can have only one parent.
	whileCond := addCompare("1", "2")
	ifCond := addCompare("1", "2")

	// The then-arm declares symbol 25 and does nothing else.
	thenValue, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "1"},
	})
	if err != nil {
		t.Fatal(err)
	}
	thenInit, err := builder.AddNode(tir.Node{
		Kind:     tir.Initialize,
		Symbol:   25,
		Children: []tir.NodeID{thenValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	thenBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{thenInit},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}

	// The else-arm Stores to symbol 25 — the then-arm's local, which must not
	// be in scope here.
	elseValue, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "2"},
	})
	if err != nil {
		t.Fatal(err)
	}
	place, err := builder.AddNode(tir.Node{
		Kind:     tir.StoragePlace,
		Type:     i32,
		Symbol:   25,
		Writable: true,
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	elseStore, err := builder.AddNode(tir.Node{
		Kind:     tir.Store,
		Children: []tir.NodeID{place, elseValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	elseBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{elseStore},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}

	ifNode, err := builder.AddNode(tir.Node{
		Kind:     tir.If,
		Region:   region,
		HasElse:  true,
		Children: []tir.NodeID{ifCond, thenBlock, elseBlock},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	loopBody, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{ifNode},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	whileNode, err := builder.AddNode(tir.Node{
		Kind:     tir.While,
		Region:   region,
		Children: []tir.NodeID{whileCond, loopBody},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{whileNode, ret})
}

// buildCallArgumentCountMismatchUnit hand-builds a unit whose i32 entry calls
// a two-parameter helper with only one argument. The checker itself rejects a
// wrong argument count from real source, so this shape is constructed directly
// through the IR builder to exercise Emit's own requirement that a DirectCall's
// child count matches the callee's declared parameter count. The helper
// (symbol 24) declares parameters 25 and 26, both i32, and its body is a bare
// `return 0;` (valid, so the rejection is specifically the call-site count,
// not the helper's own shape). The entry's DirectCall to it carries a single
// IntegerLiteral child; its FunctionType is borrowed from a checker-built
// add-shaped fixture, since the snapshot is read-only and cannot intern a
// fresh function type.
func buildCallArgumentCountMismatchUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	realUnit, snapshot, entryID, _ := buildFixture(t, "fn add(a i32, b i32) i32 { return 0; } fn main() i32 { return add(1, 2); }", "main", false)
	var fnType types.TypeID
	for _, n := range realUnit.Nodes() {
		if n.Kind == tir.DirectCall {
			fnType = n.FunctionType
			break
		}
	}
	if fnType == 0 {
		t.Fatal("checker-built fixture has no DirectCall to borrow FunctionType from")
	}
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	helperFid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: 24})
	if err != nil {
		t.Fatal(err)
	}
	zero, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "0"},
	})
	if err != nil {
		t.Fatal(err)
	}
	helperRet, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: helperFid,
		Children: []tir.NodeID{zero},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	helperBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{helperRet},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     24,
		Function:   helperFid,
		Parameters: []tir.Parameter{{Symbol: 25, Type: i32}, {Symbol: 26, Type: i32}},
		ResultType: i32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(helperFid, helperBlock); err != nil {
		t.Fatal(err)
	}

	// The entry: Return of a DirectCall to symbol 24 with only ONE child,
	// while the callee declares two parameters.
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	one, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "1"},
	})
	if err != nil {
		t.Fatal(err)
	}
	call, err := builder.AddNode(tir.Node{
		Kind:          tir.DirectCall,
		Type:          i32,
		FunctionType:  fnType,
		Symbol:        24,
		Convention:    types.Pebble,
		ContextAction: tir.ContextForward,
		Children:      []tir.NodeID{one},
		Span:          source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{call},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{ret})
}

// runtimeSourceRoot locates the runtime directory relative to this test file,
// independent of the process working directory.
func runtimeSourceRoot(t *testing.T) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("cannot locate this test file")
	}
	return filepath.Clean(filepath.Join(filepath.Dir(thisFile), "..", "..", "..", "runtime"))
}

func assertEmitRejects(t *testing.T, unit *tir.Unit, snapshot *types.Snapshot, entryID symbol.SymbolID) {
	t.Helper()
	var buf bytes.Buffer
	err := Emit(unit, snapshot, entryID, nil, nil, &buf)
	if err == nil {
		t.Fatal("Emit succeeded for an unsupported entry shape")
	}
	if buf.Len() != 0 {
		t.Fatalf("Emit wrote output on failure: %q", buf.String())
	}
}

// print statement emission — every operand type family the checker allows
// (C0612 restricts print operands to bool, char, str, any integer builtin, or
// any float builtin), one combined printf per print statement, ending in the
// literal \n only for a `println` statement (`print` emits no trailing
// newline), matching v1's print codegen shape.

// assertEmitRejectsContaining is assertEmitRejects for rejections whose error
// message must name a specific part of the unsupported shape (here: the
// non-empty DeferChain the backend refuses to drop).
func assertEmitRejectsContaining(t *testing.T, unit *tir.Unit, snapshot *types.Snapshot, entryID symbol.SymbolID, wantSubstring string) {
	t.Helper()
	var buf bytes.Buffer
	err := Emit(unit, snapshot, entryID, nil, nil, &buf)
	if err == nil {
		t.Fatalf("Emit succeeded for an unsupported entry shape, want rejection containing %q", wantSubstring)
	}
	if buf.Len() != 0 {
		t.Fatalf("Emit wrote output on failure: %q", buf.String())
	}
	if !strings.Contains(err.Error(), wantSubstring) {
		t.Fatalf("Emit rejection error %q does not contain %q", err.Error(), wantSubstring)
	}
}

// buildTopLevelBreakUnit hand-builds a unit whose i32 entry body is an
// Initialize (symbol 25 bound to 1), a Break as a leading statement, and the
// final Return of 1. Real source can never produce a Break in the entry body
// outside a loop body — the checker's C0611 requires a jump to name a valid
// enclosing loop target, which a top-level break has no way to satisfy — so it
// is constructed directly through the IR builder, the same pattern
// buildWhileAsTailUnit uses, to exercise Emit's own rejection of a jump outside
// the loop-body grammar. The Break carries a valid target region (the builder
// verifies Target is nonzero and in range) so the rejection is specifically
// about its position, not its internals. The type snapshot is borrowed from a
// checker-built fixture so every TypeID the hand-built nodes reference is owned
// by the snapshot.
func buildTopLevelBreakUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	init, err := builder.AddNode(tir.Node{
		Kind:     tir.Initialize,
		Symbol:   25,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	brk, err := builder.AddNode(tir.Node{
		Kind:   tir.Break,
		Target: region,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "1")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{init, brk, ret})
}

// buildUnboundRangeLoopUnit hand-builds a unit whose i32 entry body is an
// unbound RangeLoop (Symbol zero — the `loop start..end { ... }` form with no
// `: name`) followed by the final Return. Real source can no longer produce
// this shape: the checker rejects the unbound form with C0622 before IR
// construction ever runs. But hand-built TIR can still carry one, and
// buildRangeLoop's rangeNode.Symbol == 0 guard must keep rejecting it cleanly
// as defense-in-depth, the same way buildTopLevelBreakUnit exercises Emit's
// own jump rejection after the checker's C0611 already closes the source path.
// The type snapshot is borrowed from a checker-built fixture so every TypeID
// the hand-built nodes reference is owned by the snapshot.
func buildUnboundRangeLoopUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	bodyBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: nil,
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	rangeNode, err := builder.AddNode(tir.Node{
		Kind:     tir.RangeLoop,
		Region:   region,
		Symbol:   0,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "0"), addI32Literal(t, builder, i32, "3"), bodyBlock},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{addI32Literal(t, builder, i32, "0")},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{rangeNode, ret})
}

// 10.24 — tuple- and struct-typed function parameters

// 10.21 — optional values

// buildStrCallCraftedArgUnit hand-builds a unit whose i32 entry calls a
// str-taking helper (symbol 24, parameter 25 of type str, body `return 0;`)
// with a single crafted argument node built by argBuilder. The crafted
// argument routes through buildCallArgument's str-parameter case into
// buildStrOperand, exercising the new Load(FieldPlace) case's validation
// gates for hand-built-IR shapes real source cannot produce. The helper's
// FunctionType is borrowed from a checker-built str-taking fixture, since the
// snapshot is read-only and cannot intern a fresh function type.
func buildStrCallCraftedArgUnit(t *testing.T, argBuilder func(*tir.Builder, *types.Snapshot) (tir.NodeID, error)) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	realUnit, snapshot, entryID, _ := buildFixture(t, "fn f(s str) i32 { return 0; } fn main() i32 { return f(\"hi\"); }", "main", false)
	var fnType types.TypeID
	for _, n := range realUnit.Nodes() {
		if n.Kind == tir.DirectCall {
			fnType = n.FunctionType
			break
		}
	}
	if fnType == 0 {
		t.Fatal("checker-built fixture has no DirectCall to borrow FunctionType from")
	}
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32
	str := snapshot.Builtins().Str

	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	helperFid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: 24})
	if err != nil {
		t.Fatal(err)
	}
	zero, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "0"},
	})
	if err != nil {
		t.Fatal(err)
	}
	helperRet, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: helperFid,
		Children: []tir.NodeID{zero},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	helperBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{helperRet},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     24,
		Function:   helperFid,
		Parameters: []tir.Parameter{{Symbol: 25, Type: str}},
		ResultType: i32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(helperFid, helperBlock); err != nil {
		t.Fatal(err)
	}

	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	arg, err := argBuilder(builder, snapshot)
	if err != nil {
		t.Fatal(err)
	}
	call, err := builder.AddNode(tir.Node{
		Kind:          tir.DirectCall,
		Type:          i32,
		FunctionType:  fnType,
		Symbol:        24,
		Convention:    types.Pebble,
		ContextAction: tir.ContextForward,
		Children:      []tir.NodeID{arg},
		Span:          source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{call},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{ret})
}

// buildStrLoadOfIntFieldArg builds the crafted call argument for
// TestEmitRejectsStrLoadOfNonStrField: a Load of an i32-typed FieldPlace. The
// Load's Type is i32 (not str), so buildStrOperand's new Load case passes the
// FieldPlace-place gate and rejects the loaded type.
func buildStrLoadOfIntFieldArg(builder *tir.Builder, snapshot *types.Snapshot) (tir.NodeID, error) {
	storage, err := builder.AddNode(tir.Node{
		Kind:   tir.StoragePlace,
		Type:   snapshot.Builtins().I32,
		Symbol: 27,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		return 0, err
	}
	field, err := builder.AddNode(tir.Node{
		Kind:     tir.FieldPlace,
		Type:     snapshot.Builtins().I32,
		Member:   26,
		Children: []tir.NodeID{storage},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		return 0, err
	}
	return builder.AddNode(tir.Node{
		Kind:     tir.Load,
		Type:     snapshot.Builtins().I32,
		Children: []tir.NodeID{field},
		Span:     source.NewSpan(0, 0, 1),
	})
}

// buildStrLoadOfStoragePlaceArg builds the crafted call argument for
// TestEmitRejectsStrLoadOfNonFieldPlace: a str-typed Load whose place is a
// bare StoragePlace. buildStrOperand's new Load case rejects it at the place
// gate, proving unrelated str-shaped loads are not broadened.
func buildStrLoadOfStoragePlaceArg(builder *tir.Builder, snapshot *types.Snapshot) (tir.NodeID, error) {
	storage, err := builder.AddNode(tir.Node{
		Kind:   tir.StoragePlace,
		Type:   snapshot.Builtins().Str,
		Symbol: 27,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		return 0, err
	}
	return builder.AddNode(tir.Node{
		Kind:     tir.Load,
		Type:     snapshot.Builtins().Str,
		Children: []tir.NodeID{storage},
		Span:     source.NewSpan(0, 0, 1),
	})
}

// 10.25 — aggregate values as compound-literal expressions (inline
// construction as call arguments)

// 10.24 — struct-typed function parameters

// 10.23 — str values (literal locals + equality)

// 10.40 — ordering comparisons between str values

// 10.36 — str reassignment and str-typed parameters/results

// 10.42 — str indexing (s[i] returning char)

// 10.26 — tuple- and struct-typed function return types

// --- 10.31: switch statements ---

// TestEmitSwitchRejectsCaseValue is superseded by 10.34: a CaseValue-based
// switch case (an enum variant) is now supported for a plain enum subject —
// see TestEmitEnumLocalSwitchGreenCompilesAndRuns and its neighbors — so
// there is no longer a rejection to test here.

// buildFixtureMaybeFailing is like buildFixture but returns an error instead of
// calling t.Fatal, for tests that expect the checker to reject a fixture.
func buildFixtureMaybeFailing(t *testing.T, sourceText, entryName string, requireEntry bool) (*tir.Unit, *types.Snapshot, symbol.SymbolID, *source.FileSet, error) {
	t.Helper()
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "facts"}, fixtureProvider{"main.peb": []byte(sourceText)}, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		return nil, nil, 0, nil, err
	}
	inputs := check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}

	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == entryName {
			entryID = candidate.ID
		}
	}
	if entryID == 0 {
		return nil, nil, 0, nil, fmt.Errorf("missing symbol %q", entryName)
	}

	config := check.Config{}
	if requireEntry {
		config.Entry = check.EntryPoint{Mode: check.EntryRequired, Symbol: entryID}
	}
	result := check.Check(inputs, diagnostics, config)
	if !result.Successful() {
		return nil, nil, 0, nil, fmt.Errorf("check failed: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		return nil, nil, 0, nil, fmt.Errorf("check succeeded without an IR unit")
	}
	return unit, unit.Snapshot(), entryID, sources, nil
}

// --- 10.34: plain enum locals and switch matching ---

// embeddedPreludePath mirrors module's embedded prelude module key so test
// helpers can exclude the always-present runtime prelude (Allocator/Context)
// from "the fixture's own type declarations".
const embeddedPreludePath = "prelude/runtime.peb"

// entryTypeDeclarations returns the unit's TypeDecls owned by a module other
// than the embedded runtime prelude. The prelude module (path
// prelude/runtime.peb) is always the first graph module, so
// unit.TypeDeclarations()[0] is now the prelude's Allocator, not the fixture's
// authored type; helpers that need the fixture's own declarations must filter
// the prelude out.
func entryTypeDeclarations(unit *tir.Unit) []tir.TypeDecl {
	preludeDecls := make(map[symbol.SymbolID]bool)
	for _, m := range unit.Modules() {
		if m.Key.Path == module.CanonicalPath(embeddedPreludePath) {
			for _, declaration := range m.Declarations {
				preludeDecls[declaration] = true
			}
		}
	}
	var out []tir.TypeDecl
	for _, declaration := range unit.TypeDeclarations() {
		if !preludeDecls[declaration.Symbol] {
			out = append(out, declaration)
		}
	}
	return out
}

// enumFixture resolves the single enum type a checker-built fixture declares:
// its TypeID (from the unit's Nominal TypeUse node) and its variant symbols in
// declared order (from the TypeDecl container's Members). Tests hardcode the
// fixture's symbol IDs where the existing suite does, but the C-emission test
// and the tagged-union rejection test resolve IDs from the unit so they stay
// robust to renumbering.
func enumFixture(t *testing.T, sourceText string) (*tir.Unit, *types.Snapshot, symbol.SymbolID, types.TypeID, []symbol.SymbolID, *source.FileSet) {
	t.Helper()
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", false)
	var decl symbol.SymbolID
	var members []symbol.SymbolID
	for _, td := range entryTypeDeclarations(unit) {
		decl = td.Symbol
		members = td.Members
		break
	}
	var enumType types.TypeID
	for _, n := range unit.Nodes() {
		if n.Kind == tir.TypeUse && n.TypeArg != 0 {
			if key, ok := snapshot.Key(n.TypeArg); ok {
				if d, _, ok := key.Nominal(); ok && d == decl {
					enumType = n.TypeArg
					break
				}
			}
		}
	}
	if enumType == 0 {
		t.Fatalf("fixture declares no enum type")
	}
	return unit, snapshot, entryID, enumType, members, sources
}

// --- 10.46: integer-to-enum casts (CheckedIntegerToEnum) ---

// emitAndRunRelease drives one .peb entry source through buildFixture, Emit,
// and an end-to-end cc compile + run in PEBBLE_RT_MODE_RELEASE — the release
// twin of emitAndRun (which compiles in SAFE mode), so a checked primitive's
// mode-dependent behavior can be asserted at both configurations.
func emitAndRunRelease(t *testing.T, sourceText string, wantCode int, wantAbnormal bool) {
	t.Helper()
	requireCIntegration(t)
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, wantCode, wantAbnormal, false)
}

// --- OptionalIntegerToEnum: integer-to-optional-enum casts (`5 as ?Color`) ---

// unionFixture builds one .peb source through the full check pipeline and
// resolves the tagged-union type's TypeID and its variant symbols in declared
// order, reusing enumFixture's exact type-resolution mechanism (a tagged union
// is a Nominal type exactly like a plain enum, so a TypeUse carries its
// TypeID the same way).
func unionFixture(t *testing.T, sourceText string) (*tir.Unit, *types.Snapshot, symbol.SymbolID, types.TypeID, []symbol.SymbolID, *source.FileSet) {
	t.Helper()
	return enumFixture(t, sourceText)
}

func emitAndRunRejects(t *testing.T, sourceText, wantSubstring string) {
	t.Helper()
	unit, snapshot, entryID, _ := buildFixture(t, sourceText, "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, wantSubstring)
}

// emitAndRunWithSymbols is emitAndRun for programs whose lowering needs the
// symbol table: the wrapping u64 builtins resolve their symbol back to a
// BuiltinFunction identity through the symbol result threaded into Emit (like
// externCName does), so the
// fixture is built with buildFixtureWithSymbols and Emit is called with the
// resolution threaded through. Behavior is otherwise identical to emitAndRun:
// compile the emitted C against the runtime in SAFE mode and assert the exit
// code.
func emitAndRunWithSymbols(t *testing.T, sourceText string, wantCode int) {
	t.Helper()
	requireCIntegration(t)
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, sourceText)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), wantCode, false)
}

// 10.38 — slice-typed function parameters and return values

// --- Struct fields: slice-typed fields constructed inline ---

// buildSliceOfUnsupportedElementParameterUnit hand-builds a unit whose i32
// entry calls a helper (symbol 24) that declares one [][3]i32 parameter. The
// [][3]i32 type is borrowed from a real checker-built fixture (fn f(x [][3]i32)
// i32, which the checker accepts even though the backend rejects the
// slice-of-array element type); the unit is otherwise the same shape
// buildCallArgumentCountMismatchUnit builds, so Emit's reachability walk
// validates the helper's signature and rejects the unsupported element type
// before building any body. A fixed-array slice element (or a tagged-union
// element) is exactly what validateSliceElementType still rejects — str is a
// supported slice element since the str-element slice work (Phase 3 #32), so a
// []str parameter no longer exercises this gate.
func buildSliceOfUnsupportedElementParameterUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	realUnit, snapshot, entryID, _ := buildFixture(t, "fn f(x [][3]i32) i32 { return 0; } fn main() i32 { return 0; }", "main", false)
	var sliceType types.TypeID
	for _, n := range realUnit.Nodes() {
		if n.Kind == tir.FunctionDeclaration && len(n.Parameters) == 1 {
			sliceType = n.Parameters[0].Type
			break
		}
	}
	if sliceType == 0 {
		t.Fatal("checker-built fixture has no slice-of-array parameter to borrow its type from")
	}
	i32 := snapshot.Builtins().I32
	callUnit, _, _, _ := buildFixture(t, "fn add(a i32, b i32) i32 { return 0; } fn main() i32 { return add(1, 2); }", "main", false)
	var fnType types.TypeID
	for _, n := range callUnit.Nodes() {
		if n.Kind == tir.DirectCall {
			fnType = n.FunctionType
			break
		}
	}
	if fnType == 0 {
		t.Fatal("no checker-built DirectCall to borrow FunctionType from")
	}
	builder := tir.NewBuilder(snapshot, tir.Config{})
	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	helperFid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: 24})
	if err != nil {
		t.Fatal(err)
	}
	zero := addI32Literal(t, builder, i32, "0")
	helperRet, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: helperFid,
		Children: []tir.NodeID{zero},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	helperBlock, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{helperRet},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     24,
		Function:   helperFid,
		Parameters: []tir.Parameter{{Symbol: 25, Type: sliceType}},
		ResultType: i32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(helperFid, helperBlock); err != nil {
		t.Fatal(err)
	}

	// The entry: a Return of a DirectCall to symbol 24. The argument count does
	// not need to match (the signature gate fires before buildCallArguments),
	// but passing zero children keeps the walk's own shape well-formed.
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	call, err := builder.AddNode(tir.Node{
		Kind:          tir.DirectCall,
		Type:          i32,
		FunctionType:  fnType,
		Symbol:        24,
		Convention:    types.Pebble,
		ContextAction: tir.ContextForward,
		Span:          source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{call},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	return buildStatementsInBodyUnit(t, builder, snapshot, entryID, fid, []tir.NodeID{ret})
}

// 10.39 — indexed element writes for arrays and slices

// --- Function types: slice 1/3, function-typed locals, function values, and
// general indirect calls (struct fields, parameters, and results are later
// slices) ---

// --- Function types: slice 2/3, function-typed struct fields ---

// --- Function types: slice 3/3, function-typed parameters and results ---

// --- Function types: u64 parameter/result support ---

// --- Function types: pointer parameter/result support ---

// --- Entry argv: main(argv []str) receives the real C process arguments ---

// emitArgvAndRun drives one .peb entry source through buildFixture, Emit, and
// the end-to-end cc compile + run, executing the compiled binary with the given
// command-line arguments rather than bare (the other run helpers pass none) and
// asserting its exit code. It is the argv-specific sibling of emitAndRun: the
// hosted main's argc/argv are only observable when the test actually passes
// arguments or asserts the bare-run count.
func emitArgvAndRun(t *testing.T, sourceText string, args []string, wantCode int) {
	t.Helper()
	requireCIntegration(t)
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", true)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedC(t, buf.Bytes())
	run := exec.Command(binary, args...)
	output, err := run.CombinedOutput()
	if run.ProcessState == nil {
		t.Fatalf("compiled program did not start: %v\n%s", err, output)
	}
	code := run.ProcessState.ExitCode()
	if code != wantCode {
		t.Fatalf("compiled program exited %d, want %d (args %v)\n%s", code, wantCode, args, output)
	}
	t.Logf("compiled program exited %d, want %d (args %v)", code, wantCode, args)
}

// TestEntryArgvSlice is the focused compile-run test for the main(argv []str)
// entry form. The []str argv INCLUDES argv[0] (the program name) — the raw C
// convention V1's codegen (codegen.c's `slice_str __argv = { argv, argc }`) and
// the runtime's own pebble_rt_args_from_argv (runtime/src/platform_host.c,
// whose smoke test asserts slice.len == argc) both adopt — so argv.len equals
// argc: 1 for a bare run, 1+N once N command-line arguments are passed. The
// program returns argv.len cast to int: a slice/str .len is a uint in this
// backend's value grammar, and every existing test returning one uses the same
// `as int` — the task's bare `return argv.len;` hits that pre-existing
// value-grammar rule (it fails identically for a plain `let s str = "hi";
// return s.len;`), not anything argv-specific.
func TestEntryArgvSlice(t *testing.T) {
	t.Parallel()
	const program = "fn main(argv []str) int { return argv.len as int; }"
	cases := []struct {
		name string
		args []string
		want int
	}{
		{"no-args", nil, 1},
		{"one-arg", []string{"alpha"}, 2},
		{"three-args", []string{"alpha", "beta", "gamma"}, 4},
		{"spaces-and-eq", []string{"", "with space", "x=y"}, 4},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitArgvAndRun(t, program, tc.args, tc.want)
		})
	}
}

// TestEntryArgvVoid exercises the void-result argv form: pebble_user_main
// returns nothing, argv is still wired through, and the emitted
// (void)pebble_local_<sym>; keeps the -Wall -Wextra -Werror build clean even
// though the body never reads the parameter.
func TestEntryArgvVoid(t *testing.T) {
	t.Parallel()
	emitArgvAndRun(t, "fn main(argv []str) void { }", []string{"a", "b"}, 0)
}

// TestEntryArgvEmittedCShape pins the entry-bridge C shape for the argv form:
// pebble_user_main takes the runtime's fixed PebbleStrSlice (the []str slice
// shape, matching pebble_rt_args_from_argv's return type directly — no
// pebble_slice_<typeID>_t typedef is involved), and the hosted main adapts the
// real OS argc/argv instead of discarding them.
func TestEntryArgvEmittedCShape(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, "fn main(argv []str) int { return argv.len as int; }", "main", true)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int pebble_user_main(PebbleContext *ctx, PebbleStrSlice pebble_local_",
		"= pebble_rt_args_from_argv(&ctx, argc, argv);",
		"return pebble_user_main(&ctx, pebble_local_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "(void)argc;") || strings.Contains(out, "(void)argv;") {
		t.Errorf("emitted C still discards argc/argv for the argv entry:\n%s", out)
	}
}

// buildEntryWithParamsUnit hand-builds a unit whose i32 entry declares the
// given parameters, so validateEntrySignature's parameter gate can be exercised
// directly. The checker refuses these shapes from real source (its own entry
// validation requires zero parameters or exactly the []str argv form), so they
// are constructed through the IR builder exactly like the other
// hand-built-rejection fixtures in this package. The snapshot and entryID come
// from the caller's checker-built fixture, so every TypeID the params reference
// (i32, or a []i32 slice type borrowed from that same fixture) is owned by the
// unit's store.
func buildEntryWithParamsUnit(t *testing.T, snapshot *types.Snapshot, entryID symbol.SymbolID, params []tir.Parameter) *tir.Unit {
	t.Helper()
	builder := tir.NewBuilder(snapshot, tir.Config{})
	i32 := snapshot.Builtins().I32
	region, err := builder.AddRegion()
	if err != nil {
		t.Fatal(err)
	}
	fid, err := builder.ReserveFunctionDecl(tir.FunctionDecl{Symbol: entryID})
	if err != nil {
		t.Fatal(err)
	}
	zero := addI32Literal(t, builder, i32, "0")
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{zero},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	block, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{ret},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := builder.AddNode(tir.Node{
		Kind:       tir.FunctionDeclaration,
		Symbol:     entryID,
		Function:   fid,
		Parameters: params,
		ResultType: i32,
		Convention: types.Pebble,
		Span:       source.NewSpan(0, 0, 1),
	}); err != nil {
		t.Fatal(err)
	}
	if err := builder.CompleteFunctionDecl(fid, block); err != nil {
		t.Fatal(err)
	}
	unit, err := builder.Build()
	if err != nil {
		t.Fatalf("builder rejected the hand-built unit: %v", err)
	}
	return unit
}

func TestEntryArgvRejectsTwoParameters(t *testing.T) {
	t.Parallel()
	// The two-parameter main(argc int, argv []str) form stays intentionally
	// unsupported per the project's V1-parity decision — do not implement it.
	// The checker already rejects it from real source (validArgvParameter
	// admits exactly one parameter), so this hand-built unit pins the backend's
	// own validateEntrySignature gate for the count.
	_, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	i32 := snapshot.Builtins().I32
	unit := buildEntryWithParamsUnit(t, snapshot, entryID, []tir.Parameter{
		{Symbol: 25, Type: i32},
		{Symbol: 26, Type: i32},
	})
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "main(argc int, argv []str) is not supported yet")
}

// TestEntryArgvRejectsNonStrSliceParameter pins the other half of the
// validateEntrySignature parameter gate: a single parameter whose type is a
// slice but not of str (here []i32) is rejected, not silently wired as argv.
// The checker refuses it from real source too, so the []i32 type is borrowed
// from a checker-built fixture and the unit is hand-built on the same snapshot.
func TestEntryArgvRejectsNonStrSliceParameter(t *testing.T) {
	t.Parallel()
	realUnit, snapshot, entryID, _ := buildFixture(t, "fn f(x []i32) i32 { return x[0]; } fn main() i32 { return 0; }", "main", false)
	var i32Slice types.TypeID
	for _, n := range realUnit.Nodes() {
		if n.Kind == tir.FunctionDeclaration && len(n.Parameters) == 1 && n.Symbol != entryID {
			i32Slice = n.Parameters[0].Type
			break
		}
	}
	if i32Slice == 0 {
		t.Fatal("checker-built fixture has no []i32 parameter to borrow its type from")
	}
	unit := buildEntryWithParamsUnit(t, snapshot, entryID, []tir.Parameter{{Symbol: 25, Type: i32Slice}})
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want []str")
}
