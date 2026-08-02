package backend

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"

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

func TestEmitIfElseEntryWritesC(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { if 1 < 2 { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_rt.h",
		"static int pebble_user_main(PebbleContext *ctx)",
		"if (1 < 2) {",
		"        return 10;",
		"    } else {",
		"        return 20;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitIfElseLocalConditionAndArm(t *testing.T) {
	// A local declared before the if is visible in both the condition (x >= 10
	// is false for x = 7, so the else arm runs) and the else arm's return value
	// (x itself), proving the same locals set threads through the condition and
	// both arms.
	emitAndRun(t, "fn main() i32 { let x i32 = 7; if x >= 10 { return 1; } else { return x; } }", false, 7, false)
}

func TestEmitIfElseComparisonOperators(t *testing.T) {
	// All six comparison operators, each taking the branch matching its value;
	// both true and false outcomes are covered across the set so the emitter is
	// not silently only ever emitting one branch. The required shapes `1 < 2`
	// (true, exit 10) and `5 == 6` (false, exit 20) are in this table.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"less true", "fn main() i32 { if 1 < 2 { return 10; } else { return 20; } }", 10},
		{"equal false", "fn main() i32 { if 5 == 6 { return 10; } else { return 20; } }", 20},
		{"lessEqual true", "fn main() i32 { if 2 <= 2 { return 30; } else { return 40; } }", 30},
		{"lessEqual false", "fn main() i32 { if 3 <= 2 { return 30; } else { return 40; } }", 40},
		{"greater true", "fn main() i32 { if 3 > 2 { return 50; } else { return 60; } }", 50},
		{"greater false", "fn main() i32 { if 1 > 2 { return 50; } else { return 60; } }", 60},
		{"notEqual true", "fn main() i32 { if 1 != 2 { return 70; } else { return 80; } }", 70},
		{"notEqual false", "fn main() i32 { if 1 != 1 { return 70; } else { return 80; } }", 80},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRejectsIfWithoutElse(t *testing.T) {
	// The checker itself refuses an if-only tail (C0607: non-void function can
	// fall through without returning), so this shape is hand-built through the
	// IR builder to exercise Emit's own requirement that the final if has an
	// else.
	unit, snapshot, entryID := buildIfWithoutElseUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsLogicalAndCondition(t *testing.T) {
	// && is legal source but lowers to a ShortCircuitValue node, a different
	// kind than the BinaryValue comparison buildComparison accepts, so the
	// fixture must be rejected, not best-effort lowered.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { if 1 < 2 && 3 < 4 { return 1; } else { return 2; } }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsLogicalOrCondition(t *testing.T) {
	// Same ShortCircuitValue rejection as &&, for the || operator.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { if 1 < 2 || 3 < 4 { return 1; } else { return 2; } }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitLocalInArmCompilesAndRuns(t *testing.T) {
	// A local declared inside an arm is now a supported block under the
	// recursive grammar: the then-arm's block is one Initialize followed by
	// its Return, and the local is visible to that same arm's return. This is
	// exactly the shape 10.7 rejected as "local declared in an arm", now
	// accepted end to end (exit code 5).
	emitAndRun(t, "fn main() i32 { if 1 < 2 { let y i32 = 5; return y; } else { return 0; } }", false, 5, false)
}

func TestEmitNestedIfDiamondCompilesAndRuns(t *testing.T) {
	// A nested if inside an arm (a "diamond"): the then-arm's block is itself
	// a two-armed if/else under the same recursive grammar, so buildBlock
	// recurses a second level. Three variants cover the inner-true (exit 1),
	// inner-false (exit 2), and outer-false (exit 3) paths. This is exactly
	// the shape 10.7 rejected as "nested if in an arm", now accepted.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"inner true", "fn main() i32 { if 1 < 2 { if 3 < 4 { return 1; } else { return 2; } } else { return 3; } }", 1},
		{"inner false", "fn main() i32 { if 1 < 2 { if 5 < 4 { return 1; } else { return 2; } } else { return 3; } }", 2},
		{"outer false", "fn main() i32 { if 2 < 1 { if 3 < 4 { return 1; } else { return 2; } } else { return 3; } }", 3},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitNestedIfEntryWritesC(t *testing.T) {
	// The emitted C for a nested if must indent each level correctly so the
	// output is well-formed, readable C: the outer if is at the top level
	// (4 spaces), the nested if inside the then-arm one level deeper (8
	// spaces), and its returns two levels deep (12 spaces). Asserting the
	// literal indentation is what stops the recursive build from quietly
	// collapsing all levels onto one.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { if 1 < 2 { if 3 < 4 { return 1; } else { return 2; } } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    if (1 < 2) {\n",
		"        if (3 < 4) {\n",
		"            return 1;",
		"        } else {",
		"            return 2;",
		"        }",
		"    } else {",
		"        return 3;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitScopeIsolationCompilesAndRuns(t *testing.T) {
	// Both arms declare a source-level local named `a` with a different value,
	// and each arm references only its own `a`. The checker assigns the two
	// declarations distinct symbol IDs (confirmed against a real fixture
	// dump — 25 and 26), so each arm's `a` emits a distinct pebble_local_<id>
	// declared in its own C block; the true branch's 100 must win. This
	// exercises that the two arms' declarations don't collide with each
	// other's C names or scopes.
	emitAndRun(t, "fn main() i32 { if 1 < 2 { let a i32 = 100; return a; } else { let a i32 = 200; return a; } }", false, 100, false)
}

func TestEmitLocalsBeforeIfAndInArmCompilesAndRuns(t *testing.T) {
	// A local declared before the if is visible inside an arm, and the arm's
	// own local builds on top of it: x = 1, then-arm declares y = x + 1 and
	// returns it (2), while the else-arm returns the outer x (1). This proves
	// the outer local's declaration survives into the arm's scope while the
	// arm's own local is scoped to the arm.
	emitAndRun(t, "fn main() i32 { let x i32 = 1; if x < 10 { let y i32 = x + 1; return y; } else { return x; } }", false, 2, false)
}

func TestEmitRejectsLocalLeakingBetweenArms(t *testing.T) {
	// A local declared inside one arm must not be visible in the sibling arm.
	// This hand-built unit makes the else-arm's return reference the
	// then-arm's local (symbol 25); real source can't produce this shape (the
	// reference would fail name resolution first), so it is constructed
	// directly through the IR builder. Emit must reject it cleanly — if the
	// locals map were shared across arms instead of copied per scope, the
	// else-arm would silently see symbol 25 and emit a reference to a
	// pebble_local_25 declared only inside the then-arm's C block.
	unit, snapshot, entryID := buildSiblingArmLocalLeakUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
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

// emitAndRunBounded is emitAndRun for programs that contain a while loop: the
// program's execution is bounded by the loop's own condition, so the compiled
// binary is run through the bounded harness (compileAndRunBounded) rather than
// the unbounded compileAndRun. A miscompiled non-terminating loop therefore
// fails the test loudly and quickly instead of hanging the whole test run.
func emitAndRunBounded(t *testing.T, sourceText string, requireEntry bool, wantCode int, wantAbnormal bool) {
	t.Helper()
	unit, snapshot, entryID := buildFixture(t, sourceText, "main", requireEntry)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
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

// loopExecutionTimeout bounds the execution of a compiled program whose
// termination is guaranteed only by its own loop conditions. A genuinely
// non-terminating while loop would otherwise hang the Go test process forever;
// with this timeout the run fails loudly and quickly instead.
const loopExecutionTimeout = 5 * time.Second

// compileEmittedC cc's already-emitted C against the runtime in
// PEBBLE_RT_MODE_SAFE (the same configuration every end-to-end test here uses)
// and returns the path to the compiled binary.
func compileEmittedC(t *testing.T, emitted []byte) string {
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
	return binary
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

	ctx, cancel := context.WithTimeout(context.Background(), loopExecutionTimeout)
	defer cancel()
	run := exec.CommandContext(ctx, binary)
	output, err := run.CombinedOutput()
	if run.ProcessState == nil {
		t.Fatalf("compiled program did not start: %v\n%s", err, output)
	}
	if ctx.Err() == context.DeadlineExceeded {
		t.Fatalf("compiled program timed out after %s (a non-terminating loop?), err=%v\n%s", loopExecutionTimeout, err, output)
	}
	code := run.ProcessState.ExitCode()
	if wantAbnormal {
		// CombinedOutput returns a non-nil error for any non-zero exit or
		// signal; a clean exit 0 would mean the overflow check never fired.
		// This branch runs only after the deadline check above, so reaching it
		// proves the abnormal termination is a genuine panic, not a timeout.
		if err == nil {
			t.Fatalf("compiled program exited 0, want abnormal termination\n%s", output)
		}
		t.Logf("compiled program terminated abnormally (err=%v, exit code %d): %s", err, code, output)
		return
	}
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
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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
// DereferencePlace rather than a plain StoragePlace, and the final Return of
// 1. Real source can never produce this shape for an i32 local — a writable
// i32 place is always a plain StoragePlace — so it is constructed directly
// through the IR builder to exercise Emit's own requirement that a Store's
// place is a plain StoragePlace naming a local.
func buildStoreToNonStoragePlaceUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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
	derefBase, err := builder.AddNode(tir.Node{
		Kind:   tir.SymbolValue,
		Type:   i32,
		Symbol: 25,
		Span:   source.NewSpan(0, 0, 1),
	})
	if err != nil {
		t.Fatal(err)
	}
	place, err := builder.AddNode(tir.Node{
		Kind:     tir.DereferencePlace,
		Type:     i32,
		Writable: true,
		Children: []tir.NodeID{derefBase},
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
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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

func TestEmitReassignLocalCompilesAndRuns(t *testing.T) {
	// Reassignment of a `var` local is now a supported statement: x is
	// declared once and reassigned, and the final return reads the
	// reassigned value. This is exactly the shape 10.6 rejected (an
	// Initialize, a Store, then the Return), now accepted end to end —
	// the process must exit with the reassigned value 2.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; x = 2; return x; }", false, 2, false)
}

func TestEmitReassignUsingOwnValueCompilesAndRuns(t *testing.T) {
	// Each reassignment reads the local's own prior value: x starts at 1,
	// x = x + 1 makes it 2, the second x = x + 1 makes it 3, and the return
	// reads the final value — two increments, exit code 3. This exercises
	// that a Store's value expression can reference the very symbol being
	// reassigned (the read happens before the write).
	emitAndRun(t, "fn main() i32 { var x i32 = 1; x = x + 1; x = x + 1; return x; }", false, 3, false)
}

func TestEmitReassignInIfArmCompilesAndRuns(t *testing.T) {
	// A var declared before the if is reassigned inside the then-arm and read
	// by that same arm's return: x starts at 1, x < 10 is true, x = x + 5
	// makes it 6, exit code 6. The else arm reads the un-reassigned x (1).
	// This proves a Store is valid inside a nested block against a local
	// declared in an enclosing block — the scope map threaded through
	// buildBlock already contains x.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; if x < 10 { x = x + 5; return x; } else { return x; } }", false, 6, false)
}

func TestEmitReassignOverflowStillAborts(t *testing.T) {
	// 2147483647 + 1 overflows i32. The overflow must survive through a
	// reassignment, not just a local's initializer or a return expression:
	// x = x + 1 lowers to pebble_local_<x> = pebble_rt_checked_add_i32(
	// pebble_local_<x>, 1), which must panic through pebble_rt_panic in
	// PEBBLE_RT_MODE_SAFE — the process must terminate abnormally, not exit 0
	// and not return any specific arithmetic value.
	emitAndRun(t, "fn main() i32 { var x i32 = 2147483647; x = x + 1; return x; }", false, 0, true)
}

func TestEmitRejectsStoreToUndeclaredSymbol(t *testing.T) {
	// A Store's place must name a local declared earlier in the entry body.
	// The checker would never build a reassignment of an undeclared name from
	// valid source (resolution fails first), so it is hand-built through the
	// IR builder to exercise Emit's own in-scope requirement on the place's
	// symbol.
	unit, snapshot, entryID := buildStoreToUndeclaredSymbolUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNonI32StoreValue(t *testing.T) {
	// A reassignment's new value must be a valid i32 expression. The checker
	// rejects a bool assigned to an i32 var itself (T0505: the types do not
	// unify), so this shape is hand-built through the IR builder to exercise
	// Emit's own i32 gate on the Store's value child via buildExpr.
	unit, snapshot, entryID := buildNonI32StoreValueUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsStoreToNonStoragePlace(t *testing.T) {
	// A Store's place must be a plain StoragePlace naming a local. Real source
	// never produces a non-StoragePlace writable place for an i32 local, so
	// this shape — a Store whose first child is a DereferencePlace — is
	// hand-built through the IR builder to exercise Emit's own place-kind
	// requirement.
	unit, snapshot, entryID := buildStoreToNonStoragePlaceUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitWhileAccumulationCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: a real accumulation loop. i counts 0..4 and
	// sum accumulates i each pass, so sum = 0+1+2+3+4 = 10, returned as the
	// process exit code. This is the first program in the rewrite where a
	// loop actually iterates and accumulates across iterations. Execution is
	// bounded (compileAndRunBounded) so a miscompiled non-terminating loop
	// fails the test loudly instead of hanging it.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { sum = sum + i; i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitWhileNeverRunsCompilesAndRuns(t *testing.T) {
	// A loop whose condition is false before the first iteration: i = 10 is
	// not < 5, so the body never runs and x keeps its initial value 1. This
	// proves the emitted while does not run its body even once when the
	// condition is false at entry. Bounded execution in case of a
	// miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 10; var x i32 = 1; while i < 5 { x = 2; } return x; }", false, 1, false)
}

func TestEmitWhileCounterCompilesAndRuns(t *testing.T) {
	// A simple counter with no accumulator, to isolate the loop mechanism
	// from the accumulation pattern: i goes 0 -> 1 -> 2 -> 3, then i < 3 is
	// false and the loop exits, returning 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; } return i; }", false, 3, false)
}

func TestEmitWhileOverflowInBodyAborts(t *testing.T) {
	// Overflow must still be checked inside a loop body, not just in straight-
	// line code: x starts at 2147483640 and is incremented each of the ten
	// iterations, overflowing on the eighth (2147483647 + 1). The emitted
	// pebble_rt_checked_add_i32 call inside the loop must panic through
	// pebble_rt_panic partway through the loop, so the process terminates
	// abnormally — not exit 0, not return a silently wrapped value, and not a
	// hang. Because the program runs in bounded execution, reaching this
	// assert proves the abnormal termination is the genuine overflow abort and
	// not the bounded harness confusing it with a timeout.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 2147483640; var i i32 = 0; while i < 10 { x = x + 1; i = i + 1; } return x; }", false, 0, true)
}

func TestEmitRejectsWhileAsTail(t *testing.T) {
	// A while can only be a leading statement in the block grammar, never the
	// block's tail: a while does not satisfy the "must end in return or if"
	// requirement a block's tail has. The checker itself rejects this shape
	// from real source (C0607: non-void function can fall through without
	// returning — a while's condition evaluation does not guarantee a return),
	// so this unit is hand-built through the IR builder to exercise Emit's own
	// rejection of a While as the last child of the entry body block.
	unit, snapshot, entryID := buildWhileAsTailUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitIfElseInsideLoopBodyCompilesAndRuns(t *testing.T) {
	// The fixture 10.10 used to prove an if inside a while body was rejected
	// is now a supported shape (10.11 widens the loop-body grammar to include
	// if/else, following the same 10.8 precedent of a now-supported shape
	// becoming the new positive case). It is also the "break out early" pattern
	// 10.11 exists for: i reaches 2, the if short-circuits the loop by jumping
	// i to 10, and the loop exits — return 10 as the exit code. Bounded
	// execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 5 { if i == 2 { i = 10; } else { i = i + 1; } } return i; }", false, 10, false)
}

func TestEmitRejectsPrintInsideWhileBody(t *testing.T) {
	// The loop-body grammar still accepts only Initialize, Store, If, and
	// While; a Print inside the body (legal source) must be a clean Emit
	// rejection naming what was found, not a guessed lowering. This keeps
	// rejection coverage for a genuinely-unsupported statement kind after the
	// if-in-loop-body shape became a positive case above.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; while i < 3 { print(\"hi\"); i = i + 1; } return i; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitNoElseIfInLoopBodyCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: an if with no else inside a loop body. i counts
	// 0..9 but sum accumulates only while i < 5, so sum = 0+1+2+3+4 = 10,
	// returned as the process exit code. The no-else If is exactly the
	// two-child shape confirmed against a real fixture dump (condition,
	// then-arm, no third child), and the emitter must produce an if block with
	// no else. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i < 5 { sum = sum + i; } i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitNoElseIfInLoopBodyWritesC(t *testing.T) {
	// The emitted C for a no-else if inside a loop body must mirror buildIf's
	// indentation style: the while at the top level (4 spaces), the if one
	// level deeper (8 spaces), its store two levels deep (12 spaces), and the
	// if closed with no `else`. Asserting the literal indentation is what stops
	// the recursive build from quietly collapsing all levels onto one.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i < 5 { sum = sum + i; } i = i + 1; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    while (pebble_local_25 < 10) {\n",
		"        if (pebble_local_25 < 5) {\n",
		"            pebble_local_26 = pebble_rt_checked_add_i32(pebble_local_26, pebble_local_25);",
		"        }",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "        } else {") {
		t.Errorf("emitted C contains an else for a no-else if:\n%s", out)
	}
}

func TestEmitIfElseInLoopBodyEvenOddCompilesAndRuns(t *testing.T) {
	// A loop-body if with an else, both arms accumulating into distinct
	// enclosing locals: i counts 0..5, the then-arm counts evens (0, 2, 4) and
	// the else-arm counts odds (1, 3, 5), so even = 3 returned as the exit
	// code. The condition uses a checked modulo (`i % 2 == 0`), confirming
	// overflow-checked arithmetic is valid inside a loop-body if condition.
	// Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var even i32 = 0; var odd i32 = 0; while i < 6 { if i % 2 == 0 { even = even + 1; } else { odd = odd + 1; } i = i + 1; } return even; }", false, 3, false)
}

func TestEmitNestedWhileInLoopBodyCompilesAndRuns(t *testing.T) {
	// The nested double-loop confirmation fixture: i and j each count 0..2, so
	// the inner body runs 3 x 3 = 9 times and total = 9, returned as the exit
	// code. The inner While is a plain statement inside the outer loop's body
	// Block (the shape confirmed against a real fixture dump), and buildWhile
	// recurses into buildLoopBody for its own body unchanged. Bounded
	// execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { var j i32 = 0; while j < 3 { total = total + 1; j = j + 1; } i = i + 1; } return total; }", false, 9, false)
}

func TestEmitOverflowInsideNestedLoopIfAborts(t *testing.T) {
	// Overflow must still be checked deep inside nested control flow, not just
	// in straight-line loop bodies: x starts at 2147483640 and is incremented
	// inside a loop-body if nested inside the inner of two loops. Each of the
	// 3x3 = 9 iterations increments x (the if's condition 1 < 2 is always
	// true), overflowing on the eighth (2147483647 + 1). The emitted
	// pebble_rt_checked_add_i32 call must panic through pebble_rt_panic
	// partway through the nested loop, so the process terminates abnormally —
	// not exit 0, not return a silently wrapped value, and not a hang. Because
	// the program runs in bounded execution, reaching this assert proves the
	// abnormal termination is the genuine overflow abort and not the bounded
	// harness confusing it with a timeout.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 2147483640; var i i32 = 0; while i < 3 { var j i32 = 0; while j < 3 { if 1 < 2 { x = x + 1; } j = j + 1; } i = i + 1; } return x; }", false, 0, true)
}

func TestEmitRejectsLocalLeakingBetweenLoopIfArms(t *testing.T) {
	// A local declared inside one arm of a loop-body if must not be visible in
	// the sibling arm. This hand-built unit makes the else-arm's Store target
	// the then-arm's local (symbol 25); real source can't produce this shape
	// (the reference would fail name resolution first), so it is constructed
	// directly through the IR builder. Emit must reject it cleanly — if the
	// locals map were shared across arms instead of copied per scope, the
	// else-arm would silently accept symbol 25 and emit a reference to a
	// pebble_local_25 declared only inside the then-arm's C block.
	unit, snapshot, entryID := buildLoopIfArmLocalLeakUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsLogicalAndWhileCondition(t *testing.T) {
	// && is legal source but lowers to a ShortCircuitValue node, a different
	// kind than the BinaryValue comparison buildComparison accepts, so a while
	// condition using it must be rejected, not best-effort lowered — the same
	// rejection 10.7 established for if conditions.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; while i < 5 && i > 0 { i = i + 1; } return i; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsLogicalOrWhileCondition(t *testing.T) {
	// Same ShortCircuitValue rejection as &&, for the || operator.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; while i < 5 || i > 0 { i = i + 1; } return i; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitBoolLocalDeclarationCompilesAndRuns(t *testing.T) {
	// 10.14 makes a bool local with a bool literal initializer a supported
	// shape: the local emits `bool pebble_local_<id> = true;` and the i32
	// entry body continues to the return. This is exactly the fixture 10.13
	// rejected as a non-i32 local (the initializer is a bool literal, not an
	// i32 expression), now the new positive case. The unused bool local does
	// not disturb the i32 return value, exit code 1.
	emitAndRun(t, "fn main() i32 { let flag bool = true; return 1; }", false, 1, false)
}

func TestEmitBoolLocalIfCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: a bare bool local as an if condition. flag is
	// declared true and used directly (no comparison), so the then-arm runs
	// and the process exits 1 — proving a condition can be a bare reference to
	// an in-scope bool local, skipping the BinaryValue comparison path
	// entirely.
	emitAndRun(t, "fn main() i32 { var flag bool = true; if flag { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitBoolLocalIfWritesC(t *testing.T) {
	// The emitted C for the bare-bool if: the bool local must be declared with
	// the C bool keyword (backed by #include <stdbool.h>) and referenced
	// directly in the if condition, with the arms' returns indented one level.
	// Symbol 25 is the flag local, confirmed against the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var flag bool = true; if flag { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"#include <stdbool.h>",
		"bool pebble_local_25 = true;",
		"    if (pebble_local_25) {\n",
		"        return 1;",
		"        return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t pebble_local_25") {
		t.Errorf("emitted C declared the bool local as an integer:\n%s", out)
	}
}

func TestEmitBoolWhileNegationLoopCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: a bool accumulator flag driving a while loop.
	// done starts false, so while !done runs; each pass sums i and increments
	// it, and when i == 5 the if sets done = true, exiting the loop with sum =
	// 0+1+2+3+4 = 10 as the exit code. This exercises a bare !-negated bool
	// local as a while condition (a tir.PrefixValue with the Bang operator)
	// and a Store reassigning a bool local inside a loop-body if. Bounded
	// execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var done bool = false; var i i32 = 0; var sum i32 = 0; while !done { sum = sum + i; i = i + 1; if i == 5 { done = true; } } return sum; }", false, 10, false)
}

func TestEmitBoolWhileNegationLoopWritesC(t *testing.T) {
	// The emitted C for the !done loop must declare the bool flag, negate it
	// with plain C ! in the while condition, and reassign it with a plain bool
	// literal inside the loop-body if. Symbols 25 (done), 26 (i), and 27 (sum)
	// come from the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var done bool = false; var i i32 = 0; var sum i32 = 0; while !done { sum = sum + i; i = i + 1; if i == 5 { done = true; } } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"bool pebble_local_25 = false;",
		"    while (!(pebble_local_25)) {\n",
		"        if (pebble_local_26 == 5) {\n",
		"            pebble_local_25 = true;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_rt_checked_neg_i32") {
		t.Errorf("emitted C used the integer checked-negate helper for a bool !:\n%s", out)
	}
}

func TestEmitBoolLocalReassignCompilesAndRuns(t *testing.T) {
	// A bool local reassigned: flag is declared false, then a Store reassigns
	// it to true before the bare-bool if, so the then-arm runs and the process
	// exits 1. This proves a Store into a bool local is emitted and validated
	// against the bool grammar, mirroring how integer reassignment works.
	emitAndRun(t, "fn main() i32 { var flag bool = false; flag = true; if flag { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitRejectsBoolLocalInIntegerPosition(t *testing.T) {
	// A bool local referenced where an integer is expected must be rejected by
	// this backend. Real source `var flag bool = true; return flag;` never
	// reaches Emit — the checker itself rejects it (C0601: cannot convert a
	// bool for an i32 return value, confirmed against a fixture) — so the shape
	// is hand-built through the IR builder: the i32 entry declares a bool local
	// (Initialize for symbol 25 with a bool literal) and its Return references
	// that same symbol as a bool-typed SymbolValue. buildExpr's width gate must
	// reject the bool-typed value in the integer return position.
	unit, snapshot, entryID := buildBoolLocalReturnUnit(t)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32")
}

func TestEmitRejectsLogicalAndBoolCondition(t *testing.T) {
	// && combining two bool values as an if condition is legal source but
	// lowers to a ShortCircuitValue node (the operatorBoolean family), not a
	// bare bool value buildBoolExpr accepts, so it must be rejected, not
	// best-effort lowered — the same rejection 10.7 established, adapted from
	// comparisons to bare bool operands.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { if true && false { return 1; } else { return 0; } }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsLogicalOrBoolCondition(t *testing.T) {
	// Same ShortCircuitValue rejection as &&, for || combining two bool values.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { if true || false { return 1; } else { return 0; } }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsLogicalAndBoolWhileCondition(t *testing.T) {
	// Same ShortCircuitValue rejection as the if-condition && test, for a
	// while condition combining two bool values.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var done bool = false; while done && !done { done = true; } return 0; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNegatedComparisonCondition(t *testing.T) {
	// !(i < 5) is legal source but its negation operand is a comparison, not a
	// bare bool value: the real fixture dump shows the PrefixValue(Bang) wraps
	// a SourceAlias around the BinaryValue, a shape buildBoolExpr does not
	// accept (negating a comparison is outside this slice's grammar). Emit must
	// reject it cleanly rather than guess.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; while !(i < 5) { i = i + 1; } return i; }", "main", false)
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
	// before the final Return. Only Initialize declarations (and, since 10.9,
	// Store reassignments of an in-scope local) followed by one Return are
	// accepted in the i32 entry body.
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

func TestEmitBreakInsideLoopIfCompilesAndRuns(t *testing.T) {
	// The break-inside-a-loop-body-if fixture: i counts 0..9 but the loop
	// breaks when i == 5, so sum accumulates 0+1+2+3+4 = 10, returned as the
	// process exit code. The Break is a leaf node (no children, confirmed
	// against a fixture dump) in the then-arm of a no-else loop-body if and
	// must emit exactly `break;` at the arm's indentation. Bounded execution in
	// case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i == 5 { break; } sum = sum + i; i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitContinueInsideLoopIfCompilesAndRuns(t *testing.T) {
	// The continue-inside-a-loop-body-if fixture: i counts 1..5, skipping the
	// accumulation when i == 3, so sum = 1+2+4+5 = 12, returned as the process
	// exit code. The Continue is a leaf node in the then-arm of a no-else
	// loop-body if and must emit exactly `continue;`. Bounded execution in case
	// of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { i = i + 1; if i == 3 { continue; } sum = sum + i; } return sum; }", false, 12, false)
}

func TestEmitNestedLoopBreakTargetsInnerLoopCompilesAndRuns(t *testing.T) {
	// The nested-loop break fixture: the inner loop breaks when j == 1, so each
	// of the 3 outer iterations runs the inner body once (for j == 0) and
	// total = 3, returned as the exit code. Confirmed against a real fixture
	// dump that this Break's Target names the inner loop's region (the inner
	// While), not the outer one — so plain C break (innermost-loop semantics)
	// is a correct translation and the backend never needs to consult or
	// compare Target's value. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { var j i32 = 0; while j < 3 { if j == 1 { break; } total = total + 1; j = j + 1; } i = i + 1; } return total; }", false, 3, false)
}

func TestEmitBreakDirectInLoopBodyCompilesAndRuns(t *testing.T) {
	// A bare break directly in the loop body (not inside an if), the simplest
	// loop-body jump: i advances to 1 then the break exits the loop, so the
	// return reads 1. Shares the same leaf-node dispatch as the inside-if
	// fixtures. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; break; } return i; }", false, 1, false)
}

func TestEmitContinueDirectInLoopBodyCompilesAndRuns(t *testing.T) {
	// A bare continue directly in the loop body (not inside an if): i advances
	// each iteration before the continue, which skips the accumulation that
	// follows, so total stays 0 and the loop still terminates. This proves the
	// continue actually jumps to the loop's next iteration rather than falling
	// through the rest of the body. Bounded execution in case of a miscompiled
	// loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { i = i + 1; continue; total = total + 1; } return total; }", false, 0, false)
}

func TestEmitBreakInsideLoopIfWritesC(t *testing.T) {
	// The emitted C for the break-inside-if fixture must carry literal
	// `break;` statements at the arm's indentation: the while at the top level
	// (4 spaces), the if one level deeper (8), the break inside the arm two
	// levels deep (12), and no else. Asserting the literal indentation is what
	// stops the recursive build from quietly collapsing levels or emitting the
	// jump at the wrong depth.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i == 5 { break; } sum = sum + i; i = i + 1; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    while (pebble_local_25 < 10) {\n",
		"        if (pebble_local_25 == 5) {\n",
		"            break;",
		"        }",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitContinueInsideLoopIfWritesC(t *testing.T) {
	// The emitted C for the continue-inside-if fixture must carry a literal
	// `continue;` at the arm's indentation (12 spaces), mirroring the break
	// fixture's indentation.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { i = i + 1; if i == 3 { continue; } sum = sum + i; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"        if (pebble_local_25 == 3) {\n",
		"            continue;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitRejectsBreakWithDeferChain(t *testing.T) {
	// A real-source break inside a loop body that also contains a `defer` in
	// that loop body produces a Break whose DeferChain is non-empty (confirmed
	// against a fixture dump: deferChain=[<defer register>]). The checker
	// accepts defer inside a loop body, so real source reaches this shape; this
	// backend does not lower defer at all, so it must reject the jump cleanly,
	// naming the defer chain, rather than silently dropping the deferred
	// cleanup. The break is written *before* the defer so the loop body block's
	// children list the Break first (confirmed: [Break, DeferRegister]) and the
	// Break's own DeferChain rejection fires before the DeferRegister statement
	// would otherwise be rejected.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; while i < 3 { break; defer print 5; } return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "DeferChain")
}

func TestEmitRejectsContinueWithDeferChain(t *testing.T) {
	// Same non-empty DeferChain rejection for Continue: a defer registered in
	// the loop body and crossed by a continue produces a non-empty chain that
	// must be rejected, not silently dropped. The continue precedes the defer
	// so the Continue node is built (and rejected) before the DeferRegister
	// statement would be.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; continue; defer print 5; } return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "DeferChain")
}

func TestEmitRejectsBreakAsTopLevelLeadingStatement(t *testing.T) {
	// A Break reaching the entry body outside any loop body is unreachable from
	// real source — the checker's C0611 requires a jump to have a valid
	// enclosing loop target, so a break at the function's top level never
	// survives checking — but a hand-built unit can still place one as a
	// leading statement in the entry body block. buildBlock's generic
	// unsupported-statement path (buildLeadingStatement's default) must reject
	// it cleanly exactly like any other non-leading statement kind, not
	// silently emit a break outside a loop.
	unit, snapshot, entryID := buildTopLevelBreakUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

// assertEmitRejectsContaining is assertEmitRejects for rejections whose error
// message must name a specific part of the unsupported shape (here: the
// non-empty DeferChain the backend refuses to drop).
func assertEmitRejectsContaining(t *testing.T, unit *tir.Unit, snapshot *types.Snapshot, entryID symbol.SymbolID, wantSubstring string) {
	t.Helper()
	var buf bytes.Buffer
	err := Emit(unit, snapshot, entryID, &buf)
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
	_, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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

func TestEmitI64ReturnEntryWritesC(t *testing.T) {
	// Mirror of TestEmitIntegerReturnEntryWritesC at the wider width: the
	// pebble_user_main adapter must be declared with the 64-bit return type so
	// a wide return value is not truncated, not the i32 entry's "int".
	unit, snapshot, entryID := buildFixture(t, "fn main() i64 { return 42; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{"pebble_rt.h", "pebble_rt_default_context", "return 42;", "static int64_t pebble_user_main(PebbleContext *ctx)"} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "static int pebble_user_main") {
		t.Errorf("emitted C declares pebble_user_main returning plain int, want int64_t for an i64 entry:\n%s", out)
	}
}

func TestEmitI64ReturnEntryCompilesAndRunsExitCode42(t *testing.T) {
	emitAndRun(t, "fn main() i64 { return 42; }", false, 42, false)
}

func TestEmitI64CheckedAddCompilesAndRuns(t *testing.T) {
	// The checked helpers must be the i64 family at the wider width, producing
	// the right result end to end.
	emitAndRun(t, "fn main() i64 { return 1 + 2; }", false, 3, false)
}

func TestEmitI64CheckedAddWritesC(t *testing.T) {
	// Assert the exact helper name: an i64 entry's CheckedArithmetic must lower
	// to pebble_rt_checked_add_i64, proving the resolved width really reaches
	// the runtime function-name selection rather than staying hardcoded _i32.
	unit, snapshot, entryID := buildFixture(t, "fn main() i64 { return 1 + 2; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_rt_checked_add_i64(1, 2)",
		"static int64_t pebble_user_main(PebbleContext *ctx)",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_rt_checked_add_i32") {
		t.Errorf("emitted C uses the i32 checked helper for an i64 entry:\n%s", out)
	}
}

func TestEmitI64OverflowAborts(t *testing.T) {
	// 9223372036854775807 + 1 overflows i64. Compiled in PEBBLE_RT_MODE_SAFE
	// (the same mode every end-to-end test here uses), the emitted
	// pebble_rt_checked_add_i64 call must panic through pebble_rt_panic, so the
	// process must terminate abnormally — proving the i64 overflow story is
	// real end to end, not merely that an i64 entry compiles.
	emitAndRun(t, "fn main() i64 { return 9223372036854775807 + 1; }", false, 0, true)
}

func TestEmitI64DivideByZeroAborts(t *testing.T) {
	// 1 / 0 at i64 width: the emitted pebble_rt_checked_div_i64 call must
	// panic through pebble_rt_panic (divide-by-zero is a fault in every
	// configuration), so the process terminates abnormally.
	emitAndRun(t, "fn main() i64 { return 1 / 0; }", false, 0, true)
}

func TestEmitI64WhileAccumulationCompilesAndRuns(t *testing.T) {
	// The full control-flow story at i64: locals, mutation, a while loop, and
	// checked arithmetic all at the wider width. i counts 0..4 and sum
	// accumulates i each pass, so sum = 0+1+2+3+4 = 10, returned as the
	// process exit code. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i64 { var i i64 = 0; var sum i64 = 0; while i < 5 { sum = sum + i; i = i + 1; } return sum; }", false, 10, false)
}

func TestEmitI64WhileWritesC(t *testing.T) {
	// The emitted C for the i64 accumulation loop must declare its locals at
	// int64_t and use the i64 checked helpers, proving the width threads
	// through declarations, loop conditions, and arithmetic together. The
	// symbol IDs 25 (i) and 26 (sum) are the same ones the i32 fixture dump
	// established, so the assertions are exact.
	unit, snapshot, entryID := buildFixture(t, "fn main() i64 { var i i64 = 0; var sum i64 = 0; while i < 5 { sum = sum + i; i = i + 1; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int64_t pebble_local_25 = 0;",
		"int64_t pebble_local_26 = 0;",
		"    while (pebble_local_25 < 5) {\n",
		"        pebble_local_26 = pebble_rt_checked_add_i64(pebble_local_26, pebble_local_25);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t") {
		t.Errorf("emitted C declares an i32 local in an i64 entry:\n%s", out)
	}
}

func TestEmitI64RejectsI32Local(t *testing.T) {
	// An i32 local inside an i64 entry is a legal, well-typed Pebble program
	// the checker builds (the local is simply never returned), but this backend
	// emits exactly one width per entry and has no cast/coercion lowering, so
	// it must be rejected with a clear width-mismatch error naming the wanted
	// width — never crashed on, and never silently emitted as an i64 local.
	unit, snapshot, entryID := buildFixture(t, "fn main() i64 { let x i32 = 1; return 2; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i64")
}

func TestEmitI32RejectsI64Local(t *testing.T) {
	// The reverse direction: an i64 local inside an i32 entry is likewise a
	// legal program the checker builds and a clean width-mismatch rejection
	// for this backend.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let x i64 = 1; return 2; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32")
}
