package backend

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

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
func buildFixture(t *testing.T, sourceText, entryName string, requireEntry bool) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
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
	return unit, unit.Snapshot(), entryID
}

func TestEmitEmptyEntryWritesC(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() void {}", "main", true)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{"pebble_rt.h", "pebble_rt_default_context", "pebble_user_main"} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitEmptyEntryCompilesAndRuns(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() void {}", "main", true)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

func TestEmitIntegerReturnEntryWritesC(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{"pebble_rt.h", "pebble_rt_default_context", "return 0;", "static int pebble_user_main(PebbleContext *ctx)"} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "static void pebble_user_main") {
		t.Errorf("emitted C still declares pebble_user_main returning void, want int:\n%s", out)
	}
}

func TestEmitIntegerReturnEntryCompilesAndRunsExitCode42(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { return 42; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitCheckedAddReturnCompilesAndRuns(t *testing.T) {
	// `return 1 + 2;` was rejected by 10.3; the checked arithmetic expression
	// tree is now accepted and lowered to pebble_rt_checked_add_i32(1, 2),
	// which must produce exit code 3 end to end.
	emitAndRun(t, "fn main() i32 { return 1 + 2; }", false, 3, false)
}

func TestEmitCheckedArithmeticPrecedenceCompilesAndRuns(t *testing.T) {
	// 1 + 2 * 3 must compute as 1 + (2 * 3) = 7. Precedence is already
	// resolved in the typed IR the checker built (the * node is a child of
	// the + node); the emitter only walks the tree, it does not re-implement
	// precedence.
	emitAndRun(t, "fn main() i32 { return 1 + 2 * 3; }", false, 7, false)
}

func TestEmitCheckedNegateFeedsArithmeticCompilesAndRuns(t *testing.T) {
	// A CheckedNegate feeding into a CheckedArithmetic: -5 + 10 = 5. This
	// exercises pebble_rt_checked_neg_i32(5) inside the add's left operand.
	emitAndRun(t, "fn main() i32 { return -5 + 10; }", false, 5, false)
}

func TestEmitCheckedDivisionCompilesAndRuns(t *testing.T) {
	// 7 / 2 = 3 (plain C division truncates toward zero, which is also the
	// language's semantics): the CheckedArithmetic node with operator Slash is
	// now lowered to pebble_rt_checked_div_i32(7, 2), exit code 3.
	emitAndRun(t, "fn main() i32 { return 7 / 2; }", false, 3, false)
}

func TestEmitCheckedModuloCompilesAndRuns(t *testing.T) {
	// 7 % 2 = 1, lowered to pebble_rt_checked_mod_i32(7, 2), exit code 1.
	emitAndRun(t, "fn main() i32 { return 7 % 2; }", false, 1, false)
}

func TestEmitCheckedArithmeticOverflowAborts(t *testing.T) {
	// 2147483647 + 1 overflows i32. Compiled in PEBBLE_RT_MODE_SAFE (the
	// same mode the other end-to-end tests use), the emitted
	// pebble_rt_checked_add_i32 call must panic through pebble_rt_panic, so
	// the process must terminate abnormally — not exit 0 and not return any
	// specific arithmetic value.
	emitAndRun(t, "fn main() i32 { return 2147483647 + 1; }", false, 0, true)
}

func TestEmitCheckedDivideByZeroAborts(t *testing.T) {
	// 1 / 0 divides by zero. The emitted pebble_rt_checked_div_i32 call must
	// panic through pebble_rt_panic (divide-by-zero is a fault in every
	// configuration, not just SAFE), so the process must terminate abnormally
	// — not exit 0 and not return any specific numeric value.
	emitAndRun(t, "fn main() i32 { return 1 / 0; }", false, 0, true)
}

func TestEmitLocalDeclarationsCompilesAndRuns(t *testing.T) {
	// Two locals feeding the return: x = 1, y = 2, return x + y = 3. Each
	// local emits one `const int32_t pebble_local_<id> = ...;` declaration in
	// declaration order, and the return expression references them by name.
	emitAndRun(t, "fn main() i32 { let x i32 = 1; let y i32 = 2; return x + y; }", false, 3, false)
}

func TestEmitLocalReferencingEarlierLocalCompilesAndRuns(t *testing.T) {
	// A local's initializer references an earlier local (y = x + x = 20) and
	// the final return references a later one (y - x = 10), confirming the
	// locals-so-far set is threaded through both the initializer and return
	// expression builds.
	emitAndRun(t, "fn main() i32 { let x i32 = 10; let y i32 = x + x; return y - x; }", false, 10, false)
}

func TestEmitLocalOverflowStillAborts(t *testing.T) {
	// 2147483647 + 1 overflows i32. The overflow must survive through a local
	// reference, not just literal operands: x holds the max literal and the
	// return's x + 1 lowers to pebble_rt_checked_add_i32(pebble_local_<x>, 1),
	// which must panic through pebble_rt_panic in PEBBLE_RT_MODE_SAFE — the
	// process must terminate abnormally, not exit 0 and not return any
	// specific arithmetic value.
	emitAndRun(t, "fn main() i32 { let x i32 = 2147483647; return x + 1; }", false, 0, true)
}

// emitAndRun drives one .peb entry source through buildFixture, Emit, and the
// end-to-end cc compile + run. wantCode is the expected process exit code;
// with wantAbnormal set, the process must instead terminate abnormally (a
// non-zero exit or a signal, as abort() produces) rather than exiting with
// any specific code.
func emitAndRun(t *testing.T, sourceText string, requireEntry bool, wantCode int, wantAbnormal bool) {
	t.Helper()
	unit, snapshot, entryID := buildFixture(t, sourceText, "main", requireEntry)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), wantCode, wantAbnormal)
}

// compileAndRun cc's already-emitted C against the runtime in
// PEBBLE_RT_MODE_SAFE (the same configuration every end-to-end test here
// uses), runs the binary, and asserts the exit behavior. With wantAbnormal,
// the process must terminate abnormally; otherwise its exit code must equal
// wantCode.
func compileAndRun(t *testing.T, emitted []byte, wantCode int, wantAbnormal bool) {
	t.Helper()
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

	compile := exec.Command(cc,
		"-std=c11",
		"-DPEBBLE_RT_MODE_SAFE",
		"-I", filepath.Join(runtimeRoot, "include"),
		program,
		filepath.Join(runtimeRoot, "src", "context.c"),
		filepath.Join(runtimeRoot, "src", "panic.c"),
		filepath.Join(runtimeRoot, "src", "platform_host.c"),
		filepath.Join(runtimeRoot, "src", "arith.c"),
		"-o", binary,
	)
	if output, err := compile.CombinedOutput(); err != nil {
		t.Fatalf("cc compilation failed: %v\n%s", err, output)
	}

	run := exec.Command(binary)
	output, err := run.CombinedOutput()
	if run.ProcessState == nil {
		t.Fatalf("compiled program did not start: %v\n%s", err, output)
	}
	code := run.ProcessState.ExitCode()
	if wantAbnormal {
		// CombinedOutput returns a non-nil error for any non-zero exit or
		// signal; a clean exit 0 would mean the overflow check never fired.
		if err == nil {
			t.Fatalf("compiled program exited 0, want abnormal termination\n%s", output)
		}
		t.Logf("compiled program terminated abnormally (err=%v, exit code %d): %s", err, code, output)
		return
	}
	// A non-zero exit is expected behavior for some programs (the exit code
	// IS the program's output), so the run error is not itself a failure —
	// only a mismatch with the wanted code is. A signaled process would
	// report exit code -1 and fail the comparison.
	if code != wantCode {
		t.Fatalf("compiled program exited %d, want %d\n%s", code, wantCode, output)
	}
	t.Logf("compiled program exited %d, want %d", code, wantCode)
}

// buildI32EmptyBodyUnit hand-builds a unit whose entry has an i32 result type
// and a completely empty body block. The checker refuses to produce this shape
// itself (a non-void function must not fall through without returning), so it
// is constructed directly through the IR builder to exercise Emit's own body
// validation. The type snapshot is borrowed from a checker-built fixture so
// every TypeID the hand-built nodes reference is owned by the snapshot.
func buildI32EmptyBodyUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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

// buildNonI32ReturnUnit hand-builds a unit whose i32 entry returns a bool
// literal. The checker would reject this shape itself (a bool does not unify
// with an i32 result), so it is constructed directly through the IR builder to
// exercise Emit's own requirement that every value in an accepted expression
// tree is typed i32.
func buildNonI32ReturnUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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

// buildAssignmentInBodyUnit hand-builds a unit whose i32 entry body is an
// Initialize (symbol 25 bound to 1), a Store that reassigns symbol 25 to 2,
// and the final Return of 1. `x = 2;` is legal source syntax but the checker
// rejects it for an unrelated reason (C0606: the assignment's result is not
// used legally), so this shape is constructed directly through the IR builder,
// the same pattern buildI32EmptyBodyUnit uses, to exercise Emit's own
// rejection of any statement that is not a local declaration or the final
// return. The type snapshot is borrowed from a checker-built fixture so every
// TypeID the hand-built nodes reference is owned by the snapshot.
func buildAssignmentInBodyUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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
	storeValue, err := builder.AddNode(tir.Node{
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
	store, err := builder.AddNode(tir.Node{
		Kind:     tir.Store,
		Children: []tir.NodeID{place, storeValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	retValue, err := builder.AddNode(tir.Node{
		Kind:    tir.IntegerLiteral,
		Type:    i32,
		Span:    source.NewSpan(0, 0, 1),
		Literal: tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "1"},
	})
	if err != nil {
		t.Fatal(err)
	}
	ret, err := builder.AddNode(tir.Node{
		Kind:     tir.Return,
		Function: fid,
		Children: []tir.NodeID{retValue},
		Span:     source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	block, err := builder.AddNode(tir.Node{
		Kind:     tir.Block,
		Region:   region,
		Children: []tir.NodeID{init, store, ret},
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
	err := Emit(unit, snapshot, entryID, &buf)
	if err == nil {
		t.Fatal("Emit succeeded for an unsupported entry shape")
	}
	if buf.Len() != 0 {
		t.Fatalf("Emit wrote output on failure: %q", buf.String())
	}
}

func TestEmitRejectsNonEmptyBody(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() void { let x i32 = 1; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUnsupportedArithmeticOperator(t *testing.T) {
	// Division and modulo are now lowered to pebble_rt_checked_div_i32 /
	// pebble_rt_checked_mod_i32, so no source-level CheckedArithmetic is
	// rejected for its operator anymore. This hand-built node carries a
	// bitwise operator (&), which the backend deliberately does not map to a
	// checked helper, so it must be a clean Emit rejection — not a guessed
	// lowering and not a panic in the Go test itself.
	unit, snapshot, entryID := buildUnsupportedArithmeticOperatorUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUndeclaredLocalReference(t *testing.T) {
	// A local is declared in the body but the return references a name that
	// was never declared. The checker would never build this from valid
	// source (resolution fails first), so it is hand-built through the IR
	// builder to exercise Emit's own requirement that a SymbolValue reference
	// only symbols declared earlier in the entry body.
	unit, snapshot, entryID := buildUndeclaredLocalReferenceUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsAssignmentInBody(t *testing.T) {
	// Reassignment is not supported by this slice. `x = 2;` is legal syntax
	// but the checker rejects it for an unrelated reason (C0606: the
	// assignment's result is not used legally), so the shape — an Initialize,
	// a Store, then the Return — is hand-built through the IR builder, the
	// same pattern buildI32EmptyBodyUnit uses.
	unit, snapshot, entryID := buildAssignmentInBodyUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNonI32Local(t *testing.T) {
	// A local of a non-i32 type. The checker produces this shape from valid
	// source (the bool local is legal on its own); Emit must reject it
	// because the local's initializer value is a bool literal, not an i32
	// expression.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let flag bool = true; return 1; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsVariableReturn(t *testing.T) {
	// A variable reference lowers to a SymbolValue, which is not a supported
	// expression node for the i32 entry's return value.
	unit, snapshot, entryID := buildFixture(t, "let x i32 = 1; fn main() i32 { return x; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNonI32ReturnValue(t *testing.T) {
	// A bool literal is not an i32 expression. The checker would never build
	// this shape itself, so it is hand-built through the IR builder to
	// exercise Emit's own non-i32 rejection.
	unit, snapshot, entryID := buildNonI32ReturnUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsI32EmptyBody(t *testing.T) {
	unit, snapshot, entryID := buildI32EmptyBodyUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsStatementBeforeReturn(t *testing.T) {
	// 10.6 makes a local declaration before the return a supported shape, so
	// the fixture here is a statement kind that is still rejected: a Print
	// before the final Return. Only Initialize statements (followed by one
	// Return) are accepted in the i32 entry body.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { print(\"hi\"); return 1; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsParameters(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main(args []str) void {}", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUnsupportedResultType(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() u32 { return 0; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUnknownEntrySymbol(t *testing.T) {
	unit, snapshot, _ := buildFixture(t, "fn main() void {}", "main", true)
	assertEmitRejects(t, unit, snapshot, symbol.SymbolID(0x7FFFFFFF))
}

func TestEmitNilArguments(t *testing.T) {
	empty := &tir.Unit{}
	snapshot := &types.Snapshot{}
	if err := Emit(nil, snapshot, 0, &bytes.Buffer{}); err == nil {
		t.Fatal("Emit accepted nil unit")
	}
	if err := Emit(empty, nil, 0, &bytes.Buffer{}); err == nil {
		t.Fatal("Emit accepted nil snapshot")
	}
	if err := Emit(empty, snapshot, 0, nil); err == nil {
		t.Fatal("Emit accepted nil writer")
	}
}
