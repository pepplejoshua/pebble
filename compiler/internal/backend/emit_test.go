package backend

import (
	"bytes"
	"context"
	"errors"
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

func TestEmitLogicalIfElseCompilesAndRuns(t *testing.T) {
	// && and || as an if condition are now supported: both lower to a
	// tir.ShortCircuitValue node (a different kind than the BinaryValue
	// comparison 10.7 handled), whose operands are themselves built through
	// buildBoolExpr. This table covers both operators with a true and a false
	// outcome each, so both directions are exercised. The `and true` row is
	// exactly the fixture 10.7/10.11 rejected — now the new positive case.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"and true", "fn main() i32 { if 1 < 2 && 3 < 4 { return 1; } else { return 2; } }", 1},
		{"and false", "fn main() i32 { if 1 < 2 && 5 < 4 { return 1; } else { return 2; } }", 2},
		{"or true", "fn main() i32 { if 1 < 2 || 3 < 4 { return 1; } else { return 2; } }", 1},
		{"or false", "fn main() i32 { if 5 < 4 || 1 > 2 { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitLogicalBoolLiteralAndCompilesAndRuns(t *testing.T) {
	// The bool-literal combination shape: && of two plain bool literals, no
	// comparison involved. This is exactly the fixture 10.14 rejected as a
	// ShortCircuitValue (if true && false), now accepted — true && false is
	// false, so the else arm runs and the process exits 0.
	emitAndRun(t, "fn main() i32 { if true && false { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitLogicalBoolLiteralOrCompilesAndRuns(t *testing.T) {
	// Same bool-literal combination for ||: true || false is true, so the
	// then-arm runs and the process exits 1.
	emitAndRun(t, "fn main() i32 { if true || false { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitLogicalBoolLocalCombinationCompilesAndRuns(t *testing.T) {
	// The bool-local combination fixture: a && !b combines a bare bool local, a
	// negation of another bool local, and the && operator — three different
	// operand shapes in one ShortCircuitValue. a = true and !b = !false = true,
	// so the then-arm runs and the process exits 1.
	emitAndRun(t, "fn main() i32 { var a bool = true; var b bool = false; if a && !b { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitLogicalNestedPrecedenceCompilesAndRuns(t *testing.T) {
	// A three-way nested combination. Precedence is already resolved in the
	// typed IR tree (confirmed against a real fixture dump: the && node is a
	// child of the || node for `false && false || true`), and Pebble's grammar
	// gives || precedence 1 and && precedence 2, so && binds tighter — the same
	// as C. Each row would evaluate to the *opposite* result under the wrong
	// grouping, so the expected exit code proves the tree is walked, not
	// re-derived: `false && false || true` is (false && false) || true = true
	// (a wrong `false && (false || true)` grouping would be false), and
	// `true || false && false` is true || (false && false) = true (a wrong
	// `(true || false) && false` grouping would be false).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"and then or", "fn main() i32 { if false && false || true { return 1; } else { return 0; } }", 1},
		{"or then and", "fn main() i32 { if true || false && false { return 1; } else { return 0; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitLogicalAndWritesC(t *testing.T) {
	// The emitted C for a && condition combining a local-typed comparison and
	// an unanchored literal comparison: x < 10 && 1 < 2 must lower to the
	// parenthesized (pebble_local_25 < 10 && 1 < 2), the local reference
	// resolved to its pebble_local name. Symbol 25 is the x local, confirmed
	// against the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var x i32 = 7; if x < 10 && 1 < 2 { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_25 = 7;",
		"    if ((pebble_local_25 < 10 && 1 < 2)) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitLogicalAndParenthesizedComparisonWritesC(t *testing.T) {
	// A parenthesized comparison operand (flag && (1 < 2)) arrives wrapped in a
	// SourceAlias (confirmed against a real fixture dump), which buildBoolExpr
	// must unwrap before lowering the comparison. The emitted C must therefore
	// carry the comparison directly inside the parenthesized &&, with the bool
	// local referenced by name: (pebble_local_25 && 1 < 2). Symbol 25 is the
	// flag local.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var flag bool = true; if flag && (1 < 2) { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "if ((pebble_local_25 && 1 < 2)) {") {
		t.Errorf("emitted C missing the unwrapped parenthesized comparison &&:\n%s", out)
	}
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
		"-std=c11",
		"-Wall", "-Wextra", "-Werror",
		"-DPEBBLE_RT_MODE_SAFE",
		"-I", filepath.Join(runtimeRoot, "include"),
		program,
		filepath.Join(runtimeRoot, "src", "context.c"),
		filepath.Join(runtimeRoot, "src", "panic.c"),
		filepath.Join(runtimeRoot, "src", "platform_host.c"),
		filepath.Join(runtimeRoot, "src", "arith.c"),
		filepath.Join(runtimeRoot, "src", "bounds.c"),
		filepath.Join(runtimeRoot, "src", "optional.c"),
		filepath.Join(runtimeRoot, "src", "str.c"),
		"-o", binary,
	}
	compile := exec.Command(cc, compileArgs...)
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
	runCompiledBinary(t, binary, wantCode, wantAbnormal, true)
}

// runCompiledBinary runs one already-compiled binary and asserts its exit
// behavior. With bounded set, execution is wrapped in the loopExecutionTimeout
// context so a genuinely non-terminating program (a miscompiled while loop)
// fails the test loudly and quickly instead of hanging the run; a program that
// terminates promptly — normally, or abnormally via a panic such as the
// overflow abort — finishes well before the deadline. With wantAbnormal, the
// process must terminate abnormally (a non-zero exit or a signal, as abort()
// produces); otherwise its exit code must equal wantCode.
func runCompiledBinary(t *testing.T, binary string, wantCode int, wantAbnormal, bounded bool) {
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
	realUnit, snapshot, entryID := buildFixture(t, "fn add(a i32, b i32) i32 { return 0; } fn main() i32 { return add(1, 2); }", "main", false)
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

func TestEmitLogicalAndWhileCompilesAndRuns(t *testing.T) {
	// && as a while condition, the shape 10.11 rejected: i counts 1..4 while
	// both sides hold (i < 5 && i > 0), then at i = 5 the left side fails and
	// the loop exits with i = 5 as the exit code. Bounded execution in case of
	// a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 1; while i < 5 && i > 0 { i = i + 1; } return i; }", false, 5, false)
}

func TestEmitLogicalOrWhileCompilesAndRuns(t *testing.T) {
	// || as a while condition: i counts 0..4 through the left side (i < 5),
	// then at i = 5 both sides are false (i < 5 || i == 10) and the loop
	// exits with i = 5. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while i < 5 || i == 10 { i = i + 1; } return i; }", false, 5, false)
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

func TestEmitBoolEqualityComparisonCompilesAndRuns(t *testing.T) {
	// ==/!= between two bool values: (1 < 2) == (3 < 4) is the genuine gap
	// 10.15 left as a confirmed remaining rejection — the outer BinaryValue's
	// two operands are bool-typed SourceAlias-wrapped comparisons (confirmed
	// against the real fixture dump), which used to fail buildExpr's width
	// gate. Both bool operands are now built under the bool grammar, so the
	// equality composes. Each row's expected exit code hand-verifies the
	// comparison: (1 < 2) is true and (3 < 4) is true, so true == true is true
	// (exit 1) and true != true is false (the != twin, exit 2); and true ==
	// false / true != false take the other arms.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal true", "fn main() i32 { if (1 < 2) == (3 < 4) { return 1; } else { return 2; } }", 1},
		{"equal false", "fn main() i32 { if (1 < 2) == (2 < 1) { return 1; } else { return 2; } }", 2},
		{"notEqual false twin", "fn main() i32 { if (1 < 2) != (3 < 4) { return 1; } else { return 2; } }", 2},
		{"notEqual true", "fn main() i32 { if (1 < 2) != (2 < 1) { return 1; } else { return 2; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitBoolEqualityLocalCompilesAndRuns(t *testing.T) {
	// The bare bool-local version: a == b compares two declared bool locals
	// directly, the same BinaryValue(Equal) shape with SymbolValue operands
	// instead of wrapped comparisons (confirmed against the real fixture dump).
	// a = true, b = false, so a == b is false and the else arm runs (exit 2);
	// with b = true the equality holds and the then arm runs (exit 1).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal false", "fn main() i32 { var a bool = true; var b bool = false; if a == b { return 1; } else { return 2; } }", 2},
		{"equal true", "fn main() i32 { var a bool = true; var b bool = true; if a == b { return 1; } else { return 2; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitBoolEqualityWithShortCircuitCompilesAndRuns(t *testing.T) {
	// Bool equality composes with 10.15's && / ||: (a == b) && (1 < 2) is a
	// ShortCircuitValue whose left operand is the bool-equality BinaryValue,
	// built through buildBoolExpr's BinaryValue case into buildComparison's
	// bool branch. a = true, b = true makes the equality true and the && with
	// the true comparison true (exit 1); a = true, b = false makes the equality
	// false, so the && short-circuits to false and the else arm runs (exit 2).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"and true", "fn main() i32 { var a bool = true; var b bool = true; if (a == b) && (1 < 2) { return 1; } else { return 2; } }", 1},
		{"and false", "fn main() i32 { var a bool = true; var b bool = false; if (a == b) && (1 < 2) { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitBoolEqualityWritesC(t *testing.T) {
	// The emitted C for the flagship fixture: (1 < 2) == (3 < 4) must lower to
	// the parenthesized bool equality ((1 < 2) == (3 < 4)) in the if condition
	// — each bool operand parenthesized so a comparison operand cannot chain
	// associatively with the outer operator — with the arms' returns indented
	// one level. Symbol-level operands emit parenthesized too, as
	// (pebble_local_<id>), matching the same rule.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { if (1 < 2) == (3 < 4) { return 1; } else { return 2; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    if ((1 < 2) == (3 < 4)) {\n",
		"        return 1;",
		"        return 2;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	unit, snapshot, entryID = buildFixture(t, "fn main() i32 { var a bool = true; var b bool = false; if a == b { return 1; } else { return 2; } }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out = buf.String()
	if !strings.Contains(out, "    if ((pebble_local_25) == (pebble_local_26)) {\n") {
		t.Errorf("emitted C missing the parenthesized bool-local equality:\n%s", out)
	}
}

func TestEmitNegatedComparisonWhileCompilesAndRuns(t *testing.T) {
	// The negation-of-a-comparison fixture 10.14 rejected: its PrefixValue(Bang)
	// wraps a SourceAlias around the comparison, and buildBoolExpr now unwraps
	// SourceAlias and lowers the comparison through buildComparison, so
	// !(i >= 5) compiles to !((pebble_local_<i> >= 5)) and drives the loop:
	// i counts 0..4 while !(i >= 5) holds, then i = 5 makes the negation false
	// and the loop exits with i = 5. Bounded execution in case of a
	// miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; while !(i >= 5) { i = i + 1; } return i; }", false, 5, false)
}

func TestEmitNegatedComparisonWhileWritesC(t *testing.T) {
	// The emitted C for the negated-comparison loop must carry the negation
	// and the comparison in the while condition, with the SourceAlias unwrapped
	// into a plain C comparison: while (!(pebble_local_25 >= 5)). Symbol 25 is
	// the i local, confirmed against the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var i i32 = 0; while !(i >= 5) { i = i + 1; } return i; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    while (!(pebble_local_25 >= 5)) {\n",
		"        pebble_local_25 = pebble_rt_checked_add_i32(pebble_local_25, 1);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
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

func TestEmitHelperPlusHelperCompilesAndRuns(t *testing.T) {
	// The flagship 10.17 fixture: a second, callable function. main calls
	// helper() twice and adds the results, so the process exit code is
	// 21 + 21 = 42. The helper is emitted as its own static function before
	// pebble_user_main, and each call site lowers to pebble_fn_<callee>(ctx)
	// with the context prepended by the backend (the IR threads context via
	// ContextAction, not an explicit argument child).
	emitAndRun(t, "fn helper() i32 { return 21; } fn main() i32 { return helper() + helper(); }", false, 42, false)
}

func TestEmitHelperPlusHelperWritesC(t *testing.T) {
	// The emitted C for the flagship fixture: the helper must be its own
	// `static int32_t pebble_fn_24(PebbleContext *ctx)` block (named
	// deterministically from symbol ID 24, the helper), defined before
	// pebble_user_main (definition-before-use, since there's no forward-
	// declaration mechanism), and each call site must lower to
	// pebble_fn_24(ctx) inside the entry's checked add. Symbols 24 (helper)
	// and 25 (main) come from the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn helper() i32 { return 21; } fn main() i32 { return helper() + helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx) {",
		"    return 21;",
		"static int pebble_user_main(PebbleContext *ctx)",
		"return pebble_rt_checked_add_i32(pebble_fn_24(ctx), pebble_fn_24(ctx));",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if !(strings.Index(out, "static int32_t pebble_fn_24") < strings.Index(out, "static int pebble_user_main")) {
		t.Errorf("helper definition does not precede pebble_user_main:\n%s", out)
	}
}

func TestEmitHelperWithFullGrammarBodyCompilesAndRuns(t *testing.T) {
	// A helper whose body uses the full recursive block grammar buildBlock
	// implements — bool and integer locals, a while loop, a loop-body if, and a
	// two-armed if/else as the tail — proving buildBlock is genuinely reused
	// for a non-entry function, not just a bare return. done gates the loop, i
	// counts 0..4, sum accumulates i, so sum = 0+1+2+3+4 = 10 and the tail's
	// sum > 3 arm returns it. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn helper() i32 { var done bool = false; var sum i32 = 0; var i i32 = 0; while !done { sum = sum + i; i = i + 1; if i == 5 { done = true; } } if sum > 3 { return sum; } else { return sum + 1; } } fn main() i32 { return helper(); }", false, 10, false)
}

func TestEmitHelperWithFullGrammarBodyWritesC(t *testing.T) {
	// The emitted C for the full-grammar helper must carry the helper's own
	// locals, loop, and tail if/else at their own 4-space top level inside the
	// helper's braces, distinct from the entry's body — proving the helper's
	// body is built as its own block with its own fresh scope, not interleaved
	// into pebble_user_main. Symbols 24 (helper) and 25 (main) come from the
	// real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn helper() i32 { var done bool = false; var sum i32 = 0; var i i32 = 0; while !done { sum = sum + i; i = i + 1; if i == 5 { done = true; } } if sum > 3 { return sum; } else { return sum + 1; } } fn main() i32 { return helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx) {",
		"    bool pebble_local_26 = false;",
		"    while (!(pebble_local_26)) {\n",
		"    if (pebble_local_27 > 3) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if !(strings.Index(out, "static int32_t pebble_fn_24") < strings.Index(out, "return pebble_fn_24(ctx);")) {
		t.Errorf("helper definition does not precede its call site in the entry:\n%s", out)
	}
}

func TestEmitTwoLevelCallChainCompilesAndRuns(t *testing.T) {
	// Two levels of calls: main calls helper1, helper1 calls helper2. The
	// reachability walk must follow calls transitively (not one level deep),
	// and the emission order must place helper2 before helper1 before
	// pebble_user_main even though helper1 is declared before helper2 in the
	// source — proving the post-order walk, not declaration order, drives the
	// C ordering (a called function's definition must precede its use). Exit
	// code 20.
	emitAndRun(t, "fn helper1() i32 { return helper2(); } fn helper2() i32 { return 20; } fn main() i32 { return helper1(); }", false, 20, false)
}

func TestEmitTwoLevelCallChainWritesC(t *testing.T) {
	// The emitted C for the two-level chain must define helper2 (symbol 25,
	// called first in the post-order walk) before helper1 (symbol 24) before
	// pebble_user_main, despite the source declaring helper1 first — the
	// forward-definition requirement. Symbols come from the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn helper1() i32 { return helper2(); } fn helper2() i32 { return 20; } fn main() i32 { return helper1(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	index25 := strings.Index(out, "static int32_t pebble_fn_25")
	index24 := strings.Index(out, "static int32_t pebble_fn_24")
	indexMain := strings.Index(out, "static int pebble_user_main")
	for name, index := range map[string]int{"pebble_fn_25 (helper2)": index25, "pebble_fn_24 (helper1)": index24, "pebble_user_main": indexMain} {
		if index < 0 {
			t.Errorf("emitted C missing %q:\n%s", name, out)
		}
	}
	if !(index25 < index24 && index24 < indexMain) {
		t.Errorf("emission order is not callee-before-caller (helper2, helper1, main):\n%s", out)
	}
}

func TestEmitI64HelperCompilesAndRuns(t *testing.T) {
	// The width discipline extends to called functions: an i64 entry calls an
	// i64 helper, the helper's C return type is int64_t, and the checked add
	// uses the i64 helper family. Exit code 42.
	emitAndRun(t, "fn helper() i64 { return 21; } fn main() i64 { return helper() + helper(); }", false, 42, false)
}

func TestEmitI64HelperWritesC(t *testing.T) {
	// The emitted C for an i64 helper must declare it int64_t and call it with
	// the i64 checked helper, mirroring the entry-width threading.
	unit, snapshot, entryID := buildFixture(t, "fn helper() i64 { return 21; } fn main() i64 { return helper() + helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int64_t pebble_fn_24(PebbleContext *ctx) {",
		"pebble_rt_checked_add_i64(pebble_fn_24(ctx), pebble_fn_24(ctx));",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t pebble_fn") {
		t.Errorf("emitted C declared an i32 helper for an i64 entry:\n%s", out)
	}
}

func TestEmitRejectsI64MainCallsI32Helper(t *testing.T) {
	// A called function must resolve to the entry's own integer width — there
	// is no cast/coercion lowering, the same reasoning 10.13 established for
	// locals. An i64 entry calling an i32 helper is a legal, checker-accepted
	// program, so this is a genuine backend-scope rejection naming the width
	// mismatch.
	unit, snapshot, entryID := buildFixture(t, "fn helper() i32 { return 21; } fn main() i64 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i64")
}

func TestEmitRejectsI32MainCallsI64Helper(t *testing.T) {
	// The reverse direction: an i32 entry calling an i64 helper is likewise a
	// clean width-mismatch rejection.
	unit, snapshot, entryID := buildFixture(t, "fn helper() i64 { return 21; } fn main() i32 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32")
}

func TestEmitRejectsSelfRecursion(t *testing.T) {
	// Recursion is legal, checker-accepted Pebble (confirmed against a real
	// fixture), so this is a genuine backend-scope boundary: the reachability
	// walk follows helper's call back to helper and must reject the cycle
	// cleanly, naming the chain, rather than emit a C definition that calls
	// itself before it is defined (there's no forward-declaration mechanism).
	unit, snapshot, entryID := buildFixture(t, "fn helper() i32 { return helper(); } fn main() i32 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "recursion is not supported")
}

func TestEmitRejectsMutualRecursion(t *testing.T) {
	// Two functions that call each other: a calls b and b calls a, so a can
	// reach itself through b. The walk must reject the cycle naming the chain
	// (symbol 24 -> symbol 25 -> symbol 24), not emit either function.
	unit, snapshot, entryID := buildFixture(t, "fn a() i32 { return b(); } fn b() i32 { return a(); } fn main() i32 { return a(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "recursion is not supported")
}

func TestEmitRejectsEntryReachedByHelperCycle(t *testing.T) {
	// The cycle can close through the entry itself: main calls helper, helper
	// calls main back. main is on the walk's DFS path, so the walk must reject
	// the cycle rather than re-emit the entry as a helper.
	unit, snapshot, entryID := buildFixture(t, "fn helper() i32 { return main(); } fn main() i32 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "recursion is not supported")
}

func TestEmitRejectsVoidHelperCall(t *testing.T) {
	// A void-result helper is deliberately out of scope this slice: its only
	// use is a bare statement call (helper();), which needs an
	// expression-statement construct the block grammar does not have, and its
	// body (statement-only, ending in ImplicitReturn) does not fit the
	// return/if-tail grammar buildBlock implements. The reachability walk must
	// reject it cleanly, naming the void result, not emit it. (A void helper
	// the entry never calls is simply unreachable and not emitted at all.)
	unit, snapshot, entryID := buildFixture(t, "fn helper() void {} fn main() i32 { helper(); return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "returns void")
}

func TestEmitUnreachableFunctionNotEmitted(t *testing.T) {
	// A declared function the entry never calls, directly or transitively, must
	// not be emitted at all — the generated C has no trace of it (symbol 25,
	// the unused function), so the -Wall -Wextra -Werror build cannot warn
	// about an unused static function. Only the reachable helper (symbol 24)
	// is emitted, and the program runs to exit 21.
	unit, snapshot, entryID := buildFixture(t, "fn helper() i32 { return 21; } fn unused() i32 { return 99; } fn main() i32 { return helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_fn_24") {
		t.Errorf("emitted C missing the reachable helper:\n%s", out)
	}
	if strings.Contains(out, "pebble_fn_25") {
		t.Errorf("emitted C contains the unreachable function (symbol 25), which would trigger -Wunused-function:\n%s", out)
	}
	binary := compileEmittedC(t, buf.Bytes())
	runCompiledBinary(t, binary, 21, false, false)
}

func TestEmitCallInConditionCompilesAndRuns(t *testing.T) {
	// A helper call is an ordinary expression of the entry's width, so it can
	// appear inside a comparison condition, not just a return value: the
	// reachability walk follows it there and buildComparison's operand path
	// (via buildExpr) lowers it. helper returns 3, 3 < 5 is true, so the
	// then-arm runs and the process exits 1.
	emitAndRun(t, "fn helper() i32 { return 3; } fn main() i32 { if helper() < 5 { return 1; } else { return 2; } }", false, 1, false)
}

func TestEmitHelperCallInLocalInitializerCompilesAndRuns(t *testing.T) {
	// A helper call in a local's initializer: x is declared as the helper's
	// result, and the return reads it — the locals scope threads through the
	// call expression like any other expression of the entry's width.
	emitAndRun(t, "fn helper() i32 { return 7; } fn main() i32 { let x i32 = helper(); return x + 1; }", false, 8, false)
}

func TestEmitAddParametersCompilesAndRuns(t *testing.T) {
	// The flagship 10.18 fixture: a two-parameter function called from the
	// entry with two arguments. Each parameter seeds the callee's locals scope
	// before its body is built, so the body's a + b reads them exactly like
	// declared locals, and the call site emits pebble_fn_<id>(ctx, 20, 22).
	// 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { return add(20, 22); }", false, 42, false)
}

func TestEmitAddParametersWritesC(t *testing.T) {
	// The emitted C for the flagship fixture: the helper's signature declares
	// each parameter with the same pebble_local_<symbolID> naming a local uses
	// (symbols 25 and 26, the a and b parameters from the real fixture dump),
	// each parameter gets a (void) cast against -Wunused-parameter, and the
	// call site passes the argument expressions after ctx. Symbol 24 is the
	// helper, 25 is main, matching the other 10.17/10.18 fixtures.
	unit, snapshot, entryID := buildFixture(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { return add(20, 22); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx, int32_t pebble_local_25, int32_t pebble_local_26) {",
		"    (void)pebble_local_25;",
		"    (void)pebble_local_26;",
		"    return pebble_rt_checked_add_i32(pebble_local_25, pebble_local_26);",
		"return pebble_fn_24(ctx, 20, 22);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if !(strings.Index(out, "static int32_t pebble_fn_24") < strings.Index(out, "static int pebble_user_main")) {
		t.Errorf("helper definition does not precede pebble_user_main:\n%s", out)
	}
}

func TestEmitBoolParameterCompilesAndRuns(t *testing.T) {
	// The bool-parameter fixture: choose takes a bool flag and two integer
	// values and returns one of the integers, so the flag's grammar is the
	// bool one (buildBoolExpr) while the other two parameters are the entry's
	// width. choose(true, 10, 20) takes the then-arm and returns x = 10, the
	// process exit code.
	emitAndRun(t, "fn choose(flag bool, x i32, y i32) i32 { if flag { return x; } else { return y; } } fn main() i32 { return choose(true, 10, 20); }", false, 10, false)
}

func TestEmitBoolParameterWritesC(t *testing.T) {
	// The emitted C for the bool-parameter fixture: the flag parameter (symbol
	// 25) is declared `bool pebble_local_25` in the signature while x and y
	// (symbols 26 and 27) are int32_t, and the call site passes the bool
	// literal and the two integer literals after ctx. Symbols come from the
	// real fixture dump (choose=24, flag=25, x=26, y=27, main=28).
	unit, snapshot, entryID := buildFixture(t, "fn choose(flag bool, x i32, y i32) i32 { if flag { return x; } else { return y; } } fn main() i32 { return choose(true, 10, 20); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx, bool pebble_local_25, int32_t pebble_local_26, int32_t pebble_local_27) {",
		"    (void)pebble_local_25;",
		"    (void)pebble_local_26;",
		"    (void)pebble_local_27;",
		"    if (pebble_local_25) {\n",
		"        return pebble_local_26;",
		"        return pebble_local_27;",
		"return pebble_fn_24(ctx, true, 10, 20);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitParameterInLoopAndIfCompilesAndRuns(t *testing.T) {
	// A parameter seeding the callee's scope must resolve for the full block
	// grammar, not just a bare return: n is read in the while condition and in
	// a loop-body if condition, while the loop accumulates and reassigns
	// locals. sum_to(5) accumulates 0+1+2+3+4 = 10, the process exit code.
	// Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn sum_to(n i32) i32 { var i i32 = 0; var total i32 = 0; while i < n { if i < n { total = total + i; } i = i + 1; } return total; } fn main() i32 { return sum_to(5); }", false, 10, false)
}

func TestEmitParameterForwardedToHelperCallCompilesAndRuns(t *testing.T) {
	// A parameter used as an argument to another call inside its own callee:
	// add forwards its own a parameter to double, whose result is added to the
	// other parameter b. This proves a parameter resolves at a nested call
	// site's argument position (buildCallArguments sees the seeded scope).
	// double(5) = 10, + b(2) = 12, the process exit code.
	emitAndRun(t, "fn double(x i32) i32 { return x + x; } fn add(a i32, b i32) i32 { return double(a) + b; } fn main() i32 { return add(5, 2); }", false, 12, false)
}

func TestEmitNestedCallArgumentCompilesAndRuns(t *testing.T) {
	// A call whose argument is itself a call: add(helper(), 5) passes the
	// result of helper() as the first argument. The checker coerces the nested
	// call to the i32 parameter, and buildCallArguments builds it with
	// buildExpr, so the emitted C is pebble_fn_<add>(ctx, pebble_fn_<helper>
	// (ctx), 5). helper() = 5, so add returns 5 + 5 = 10, the exit code.
	emitAndRun(t, "fn helper() i32 { return 5; } fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { return add(helper(), 5); }", false, 10, false)
}

func TestEmitUnusedParameterCompilesClean(t *testing.T) {
	// A genuinely-unused parameter (declared, never read in the callee's body)
	// must still compile under the shared harness's strict -Wall -Wextra
	// -Werror build. -Wunused-parameter genuinely fires for a named parameter
	// the body never reads (confirmed), so the per-parameter
	// (void)pebble_local_<id>; cast emitted right after the opening brace is
	// what keeps this compiling. Exit code 5.
	emitAndRun(t, "fn helper(unused i32) i32 { return 5; } fn main() i32 { return helper(5); }", false, 5, false)
}

func TestEmitI64ParameterizedHelperCompilesAndRuns(t *testing.T) {
	// The width discipline extends to parameters: an i64 entry calls an i64
	// helper whose i64 parameters seed its scope, and the checked add uses the
	// i64 helper family. Exit code 42.
	emitAndRun(t, "fn add(a i64, b i64) i64 { return a + b; } fn main() i64 { return add(20, 22); }", false, 42, false)
}

func TestEmitRejectsUnsupportedParameterType(t *testing.T) {
	// A str parameter is reachable from real source (the checker accepts it),
	// so this is a genuine backend-scope rejection, not hand-built IR:
	// validateHelperSignature must reject the parameter because its type is
	// neither the entry's width nor bool, naming the parameter position.
	unit, snapshot, entryID := buildFixture(t, "fn f(s str) i32 { return 1; } fn main() i32 { return f(\"hi\"); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}

func TestEmitRejectsParameterWidthMismatch(t *testing.T) {
	// A parameter of the other integer width follows the same width-consistency
	// rule 10.13 established for locals: an i64 parameter in an i32 entry (and
	// its result, here also i64) must be a clean rejection naming the width,
	// never a coercion. The parameter check fires before the result check.
	unit, snapshot, entryID := buildFixture(t, "fn f(a i64) i64 { return 0; } fn main() i32 { return f(0); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}

func TestEmitRejectsCallArgumentCountMismatch(t *testing.T) {
	// A call site passing fewer (or more) arguments than the callee declares
	// parameters is unreachable from real source — the checker rejects a wrong
	// argument count itself — so it is hand-built through the IR builder to
	// exercise Emit's own requirement that a DirectCall's child count matches
	// the callee's declared parameter count.
	unit, snapshot, entryID := buildCallArgumentCountMismatchUnit(t)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want 2")
}

func TestEmitTupleTwoElementReadBackCompilesAndRuns(t *testing.T) {
	// The confirmation fixture for tuple construction and an element read: a
	// two-element (i32, i32) tuple is declared from a tuple literal and its
	// second element is read back into the return value. The tuple type emits
	// one struct typedef with fields _0/_1 and the local is initialized with
	// the struct literal { 20, 22 }, and the element read lowers to
	// pebble_local_<id>._1, so the process exit code is 22.
	emitAndRun(t, "fn main() i32 { let t (i32, i32) = (20, 22); return t.1; }", false, 22, false)
}

func TestEmitTupleElementsReadBackAndAddedCompilesAndRuns(t *testing.T) {
	// The "elements read back and added" fixture: a three-element tuple's
	// elements 1 and 2 are read back and added: t.1 + t.2 = 20 + 30 = 50.
	emitAndRun(t, "fn main() i32 { let t (i32, i32, i32) = (10, 20, 30); return t.1 + t.2; }", false, 50, false)
}

func TestEmitTupleElementZeroReadCompilesAndRuns(t *testing.T) {
	// Regression test: reading a tuple's element 0 (t.0) used to be impossible
	// from any source — the tir verifier rejected a TuplePlace/TupleElementValue
	// with Ordinal 0, because Node.Ordinal is a zero-based element index (0 is
	// the tuple's first element, a legitimate value) but the verifier treated
	// Ordinal == 0 as an absent-field sentinel. Fixed directly in
	// compiler/internal/tir/verify.go (not this package): the erroneous
	// "requires Ordinal" checks for TupleElementValue/TuplePlace were removed,
	// since 0 is not damage and there is no way to distinguish a genuinely
	// unset Ordinal from a legitimate element-0 index without a type-level
	// cross-check the structural verifier doesn't otherwise do. t.0 now reads
	// the tuple's first element: pebble_local_<id>._0 = 20.
	emitAndRun(t, "fn main() i32 { let t (i32, i32) = (20, 22); return t.0; }", false, 20, false)
}

func TestEmitTupleThreeElementWritesC(t *testing.T) {
	// The emitted C for the three-element tuple: one struct typedef with three
	// positional fields written before pebble_user_main (definition before
	// use), the local declared as the typedef type and initialized with the
	// brace literal { 10, 20, 30 }, and both element reads lowering to
	// pebble_local_<id>._1 / ._2 inside the checked add. Symbol 25 is the t
	// local and tuple type 23 its (i32, i32, i32) type, confirmed against the
	// real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let t (i32, i32, i32) = (10, 20, 30); return t.1 + t.2; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n    int32_t _2;\n} pebble_tuple_23_t;",
		"pebble_tuple_23_t pebble_local_25 = { 10, 20, 30 };",
		"    (void)pebble_local_25;",
		"return pebble_rt_checked_add_i32(pebble_local_25._1, pebble_local_25._2);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	mainIndex := strings.Index(out, "static int pebble_user_main")
	if typedefIndex < 0 || mainIndex < 0 || typedefIndex > mainIndex {
		t.Errorf("tuple typedef does not precede pebble_user_main (definition before use):\n%s", out)
	}
}

func TestEmitTupleBoolElementDrivesIfCompilesAndRuns(t *testing.T) {
	// A tuple with a bool element mixed beside an integer element: the bool
	// element read (t.1) drives an if condition. t.1 = true runs the then-arm
	// (exit 10); with the bool element false the else-arm runs (exit 20). The
	// read is a bool value, so it must route through the bool grammar
	// (buildBoolExpr's Load case), not the integer one.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"bool true", "fn main() i32 { let t (i32, bool) = (1, true); if t.1 { return 10; } else { return 20; } }", 10},
		{"bool false", "fn main() i32 { let t (i32, bool) = (1, false); if t.1 { return 10; } else { return 20; } }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitTupleBoolElementDrivesIfWritesC(t *testing.T) {
	// The emitted C for the bool-element-if fixture: the typedef's second field
	// must be the C bool, the local's initializer carries the integer and bool
	// literals in order, and the if condition is the raw field read
	// pebble_local_<id>._1 (a C bool needs no comparison). Symbol 25 is the t
	// local, confirmed against the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let t (i32, bool) = (1, true); if t.1 { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    bool _1;\n} pebble_tuple_23_t;",
		"pebble_tuple_23_t pebble_local_25 = { 1, true };",
		"    if (pebble_local_25._1) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if !strings.Contains(out, "return 10;") || !strings.Contains(out, "return 20;") {
		t.Errorf("emitted C missing the if/else arms:\n%s", out)
	}
}

func TestEmitTupleElementAsCallArgumentCompilesAndRuns(t *testing.T) {
	// A tuple element read composes with 10.18's call-argument building: each
	// argument of add is a read of the tuple local's element 1. buildCallArguments
	// builds each argument with buildExpr, which lowers the Load(TuplePlace) read
	// to pebble_local_<id>._1, so add(22, 22) = 44 is the process exit code.
	// The tuple typedef must still be emitted before the helper's definition.
	emitAndRun(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let t (i32, i32) = (20, 22); return add(t.1, t.1); }", false, 44, false)
}

func TestEmitTupleElementAsCallArgumentWritesC(t *testing.T) {
	// The emitted C for the call-argument fixture: the tuple typedef precedes
	// both the helper and the entry, the local initializes to { 20, 22 }, and
	// the call site passes the two element reads after ctx. Symbols 24 (add),
	// 25/26 (its parameters), and 28 (t) come from the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let t (i32, i32) = (20, 22); return add(t.1, t.1); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n} pebble_tuple_23_t;",
		"pebble_tuple_23_t pebble_local_28 = { 20, 22 };",
		"return pebble_fn_24(ctx, pebble_local_28._1, pebble_local_28._1);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static int32_t pebble_fn_24")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("tuple typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitTupleLocalInsideHelperCompilesAndRuns(t *testing.T) {
	// A tuple-typed local declared inside a reachable helper's body, not the
	// entry's: the typedef-collection pass must walk helper bodies too, so the
	// tuple typedef is emitted before the helper's definition (which is before
	// pebble_user_main). helper declares t, reads its element 1, and returns
	// it; the entry just calls helper, so exit code 22 proves the helper's
	// tuple local was built and the typedef emitted correctly.
	emitAndRun(t, "fn helper() i32 { let t (i32, i32) = (20, 22); return t.1; } fn main() i32 { return helper(); }", false, 22, false)
}

func TestEmitI64TupleCompilesAndRuns(t *testing.T) {
	// The width discipline extends to tuple element types: an i64 entry's
	// (i64, i64) tuple's typedef fields are int64_t, the local is int64_t
	// backed, and the element read feeds the i64 entry's return. Exit code 22.
	emitAndRun(t, "fn main() i64 { let t (i64, i64) = (20, 22); return t.1; }", false, 22, false)
}

func TestEmitI64TupleWritesC(t *testing.T) {
	// The emitted C for the i64 tuple must use int64_t for both typedef fields
	// and for the pebble_user_main return type, proving the entry's width
	// threads into the tuple layout, not just the scalar declarations.
	unit, snapshot, entryID := buildFixture(t, "fn main() i64 { let t (i64, i64) = (20, 22); return t.1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int64_t _0;\n    int64_t _1;\n} pebble_tuple_23_t;",
		"pebble_tuple_23_t pebble_local_25 = { 20, 22 };",
		"return pebble_local_25._1;",
		"static int64_t pebble_user_main(PebbleContext *ctx)",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t _0") {
		t.Errorf("emitted C declared an i32 tuple field for an i64 entry:\n%s", out)
	}
}

func TestEmitArrayElementReadCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; return a[1]; }", false, 20, false)
}

func TestEmitArrayBoolElementDrivesIf(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a [2]bool = [false, true]; if a[1] { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitArrayExpressionIndexCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; let i i32 = 1; return a[i + 1]; }", false, 30, false)
}

func TestEmitArrayOutOfBoundsAborts(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a [2]i32 = [10, 20]; let i i32 = 2; return a[i]; }", false, 0, true)
}

func TestEmitArrayElementAsCallArgument(t *testing.T) {
	emitAndRun(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let a [2]i32 = [20, 22]; return add(a[0], a[1]); }", false, 42, false)
}

func TestEmitI64ArrayCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i64 { let a [2]i64 = [20, 22]; return a[1]; }", false, 22, false)
}

func TestEmitArrayWritesC(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; return a[1]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_25[3] = { 10, 20, 30 };",
		"return pebble_local_25[pebble_rt_checked_index_i32(1, 3)];",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitRejectsArrayRepeatInitializer(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let a [3]i32 = [1; 3]; return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "ArrayRepeat")
}

func TestEmitRejectsTupleWithUnsupportedElementType(t *testing.T) {
	// A tuple whose element type is neither the entry's width nor bool — here
	// a str element — is reachable from real source (the checker builds the
	// declaration fine), so this is a genuine backend-scope rejection. The
	// tuple typedef pass inspects the element types first and rejects the str
	// field with a clear error naming the wanted types, so no C is written.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let t (i32, str) = (1, \"hi\"); return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}

func TestEmitRejectsNestedTupleElement(t *testing.T) {
	// A tuple whose element is itself a tuple (tuple-of-tuple) is reachable
	// from real source, so this is a genuine backend-scope rejection: the
	// outer tuple's element 0 type is the inner (i32, i32) tuple, which is
	// neither the entry's width nor bool, and must be rejected by the tuple
	// typedef pass, not mis-emitted as a struct field of a struct.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let inner (i32, i32) = (1, 2); let outer ((i32, i32), bool) = (inner, true); return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}

func TestEmitRejectsWholeTupleStore(t *testing.T) {
	// Reassigning a whole tuple-typed local (t = (3, 4)) is reachable from
	// real source (the checker builds the Store fine) but is out of scope this
	// slice: only element reads of a tuple local are supported, never
	// assignment into or reassignment of one. The Store's place names a
	// tuple-typed local, so buildLeadingStatement rejects it with a clear
	// error naming the reassignment, not a guessed lowering.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var t (i32, i32) = (1, 2); t = (3, 4); return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "reassigning a whole tuple is not supported")
}

func TestEmitRejectsParenWrappedAggregateArgument(t *testing.T) {
	// The one shape in the inline-construction space that is still genuinely
	// rejected (10.25): an aggregate constructed inline but wrapped in an extra
	// set of parens — f(((1, 2))) or f((Point.{ x = 1, y = 2 })) — arrives at
	// the call site as a SourceAlias wrapping the TupleValue/RecordConstruct
	// (confirmed against a real fixture dump). This backend does not unwrap a
	// SourceAlias-wrapped argument for ANY argument type — the scalar analog
	// f((1)) is likewise rejected as "a SourceAlias of type int, want i32" —
	// so the aggregate forms stay a clean rejection naming what was found,
	// never a guessed lowering. (10.24's two rejection tests, which rejected
	// ALL inline construction as a call argument, became stale when inline
	// construction was added — the fixtures f((1, 2)) and
	// f(Point.{ x = 1, y = 2 }) are now the positive cases
	// TestEmitInlineTupleArgumentCompilesAndRuns /
	// TestEmitInlineStructArgumentCompilesAndRuns — and this test is their
	// replacement.)
	unit, snapshot, entryID := buildFixture(t, "fn f(t (i32, i32)) i32 { return t.1; } fn main() i32 { return f(((1, 2))); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want a reference to a tuple-typed local in scope or a tuple literal")

	unit, snapshot, entryID = buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x; } fn main() i32 { return f((Point.{ x = 1, y = 2 })); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want a reference to a struct-typed local in scope or a struct literal")
}

func TestEmitRejectsTupleLiteralIndex(t *testing.T) {
	// Indexing a tuple literal directly — (1, 2).1 — is out of scope: the
	// checker lowers it to a TupleElementValue whose child is the TupleValue
	// being indexed (not a tuple-typed local) and whose element type comes out
	// as the unanchored `int` builtin (confirmed against a real fixture). The
	// tuple's int element is not the entry's width, so the tuple typedef pass
	// rejects it cleanly; this keeps the only supported element read exactly
	// the tuple-local Load(TuplePlace) shape.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let x i32 = (1, 2).1; return x; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

// 10.24 — tuple- and struct-typed function parameters

func TestEmitTupleParameterCompilesAndRuns(t *testing.T) {
	// The flagship tuple-parameter fixture: sumT takes a whole (i32, i32)
	// tuple as its parameter and reads both elements back inside the callee,
	// including element 0 (confirming 10.19's tir-ordinal fix still holds in
	// the parameter path). The entry declares a tuple local and passes it by
	// value; the callee's parameter seeds its own scope as a tuple local, so
	// the reads resolve through the same Load(TuplePlace) machinery a tuple
	// local uses. 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn sumT(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { let t (i32, i32) = (20, 22); return sumT(t); }", false, 42, false)
}

func TestEmitTupleParameterWritesC(t *testing.T) {
	// The emitted C for the flagship fixture: the tuple typedef must precede
	// the helper, the helper's signature declares the parameter as
	// pebble_tuple_<typeID>_t pebble_local_<paramSymbol> (symbol 25, the t
	// parameter), the parameter gets the same (void) cast, the body reads
	// pebble_local_25._0 / ._1, and the call site passes the entry's tuple
	// local pebble_local_27 directly (no construction at the call site).
	// Symbols 24 (sumT), 25 (t param), 26 (main), 27 (t local), and tuple type
	// 23 come from the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn sumT(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { let t (i32, i32) = (20, 22); return sumT(t); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n} pebble_tuple_23_t;",
		"static int32_t pebble_fn_24(PebbleContext *ctx, pebble_tuple_23_t pebble_local_25) {",
		"    (void)pebble_local_25;",
		"    return pebble_rt_checked_add_i32(pebble_local_25._0, pebble_local_25._1);",
		"pebble_tuple_23_t pebble_local_27 = { 20, 22 };",
		"return pebble_fn_24(ctx, pebble_local_27);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static int32_t pebble_fn_24")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("tuple typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitTupleParameterUsedInBoolElementAndSecondCall(t *testing.T) {
	// A tuple parameter whose element types are mixed width/bool: the callee
	// reads the bool element to drive an if and the i32 element as a value.
	// This proves the parameter's element reads route through buildBoolExpr
	// (the Load(TuplePlace) bool path) exactly as a tuple local's do.
	// choose((10, true)) takes the then-arm and returns the i32 element 10.
	emitAndRun(t, "fn choose(t (i32, bool)) i32 { if t.1 { return t.0; } else { return 99; } } fn main() i32 { let t (i32, bool) = (10, true); return choose(t); }", false, 10, false)
}

func TestEmitTupleParameterParamOnlyTypeGetsTypedef(t *testing.T) {
	// The typedef-discovery extension: the (i32, i32) tuple type appears ONLY
	// as sumT's parameter type — sumT is never called (so no reachable body
	// constructs a tuple of that type) and main constructs no tuple at all —
	// yet the typedef must still be discovered, because the emitted helper's C
	// signature names pebble_tuple_<typeID>_t. Before 10.24's Parameters scan
	// in collectTupleTypes this returned nothing; the test drives
	// collectTupleTypes directly with a hand-built reachable-helper slice, so
	// it fails if the discovery stops being tied to a construction site. (The
	// concrete type ID 23 is confirmed from the fixture dump.)
	unit, snapshot, entryID := buildFixture(t, "fn sumT(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { return 0; }", "main", false)
	entryDecl, err := findFunctionDeclaration(unit, entryID, "entry function")
	if err != nil {
		t.Fatalf("entry declaration: %v", err)
	}
	_, entryBlock, err := findFunctionBody(unit, entryDecl, "entry function")
	if err != nil {
		t.Fatalf("entry body: %v", err)
	}
	sumTDecl, err := findFunctionDeclaration(unit, 24, "called function")
	if err != nil {
		t.Fatalf("sumT declaration: %v", err)
	}
	_, sumTBody, err := findFunctionBody(unit, sumTDecl, "called function")
	if err != nil {
		t.Fatalf("sumT body: %v", err)
	}
	helpers := []helperInfo{{decl: sumTDecl, block: sumTBody}}
	ids, err := collectTupleTypes(unit, snapshot, entryBlock, helpers)
	if err != nil {
		t.Fatalf("collectTupleTypes failed: %v", err)
	}
	found := false
	for _, id := range ids {
		if id == 23 {
			found = true
		}
	}
	if !found {
		t.Fatalf("tuple type 23 used only as a parameter type was not discovered, got %v", ids)
	}
}

// 10.21 — optional values

func TestEmitOptionalNoneNeverUnwrappedCompilesClean(t *testing.T) {
	// Regression coverage: `none` was initially thought unreachable from real
	// source (a since-fixed checker bug, compiler/internal/check's
	// shapeLeaf, made `let x ?i32 = none;` fail to type-check). It is
	// reachable — this proves a none-initialized local, never unwrapped,
	// compiles clean under -Wall -Wextra -Werror.
	emitAndRun(t, "fn main() i32 { let x ?i32 = none; return 1; }", false, 1, false)
}

func TestEmitOptionalUnwrapNoneAborts(t *testing.T) {
	// Force-unwrapping a none-initialized local panics via
	// pebble_rt_checked_unwrap_i32, aborting the process.
	emitAndRun(t, "fn main() i32 { let x ?i32 = none; return x!; }", false, 0, true)
}

func TestEmitOptionalSomeUnwrapCompilesAndRuns(t *testing.T) {
	// The confirmation fixture for optional construction and force-unwrap: a
	// some <expr> local is declared and force-unwrapped as the return value.
	// The optional type emits one struct typedef with has_value/value fields,
	// the local is initialized with { .has_value = true, .value = 42 }, and
	// the force-unwrap lowers to pebble_rt_checked_unwrap_i32, so the process
	// exit code is 42.
	emitAndRun(t, "fn main() i32 { let x ?i32 = some 42; return x!; }", false, 42, false)
}

func TestEmitOptionalSomeUnwrapI64CompilesAndRuns(t *testing.T) {
	// The i64 width discipline extends to optional payload types: an i64
	// entry's ?i64 optional's typedef value field is int64_t, the local is
	// initialized with the i64 payload, and the force-unwrap lowers to
	// pebble_rt_checked_unwrap_i64. Exit code 22.
	emitAndRun(t, "fn main() i64 { let x ?i64 = some 22; return x!; }", false, 22, false)
}

func TestEmitOptionalBoolPayloadDrivesIfCompilesAndRuns(t *testing.T) {
	// A bool-payload optional, force-unwrapped to drive an if condition. The
	// unwrap lowers to pebble_rt_checked_unwrap_bool, whose result drives the
	// if condition directly. With the bool payload true the then-arm runs
	// (exit 10); with false the else-arm runs (exit 20).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"bool true", "fn main() i32 { let x ?bool = some true; if x! { return 10; } else { return 20; } }", 10},
		{"bool false", "fn main() i32 { let x ?bool = some false; if x! { return 10; } else { return 20; } }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitOptionalUnwrapAsCallArgumentCompilesAndRuns(t *testing.T) {
	// An unwrapped optional element used as a call argument: the force-unwrap
	// produces an ordinary i32 value that is passed to a helper function.
	// add(x!, y!) = 10 + 20 = 30 is the process exit code.
	emitAndRun(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let x ?i32 = some 10; let y ?i32 = some 20; return add(x!, y!); }", false, 30, false)
}

func TestEmitOptionalSomeUnwrapWritesC(t *testing.T) {
	// The emitted C for the some-unwrap fixture: the optional typedef with
	// has_value/value fields, the local initialized with the struct literal
	// initializer, and the unwrap call site using pebble_rt_checked_unwrap_i32.
	// Symbol IDs come from the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let x ?i32 = some 42; return x!; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    int32_t value;\n} pebble_optional_",
		".has_value = true, .value = 42",
		"pebble_rt_checked_unwrap_i32(",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	mainIndex := strings.Index(out, "static int pebble_user_main")
	if typedefIndex < 0 || mainIndex < 0 || typedefIndex > mainIndex {
		t.Errorf("optional typedef does not precede pebble_user_main (definition before use):\n%s", out)
	}
}

func TestEmitOptionalI64WritesC(t *testing.T) {
	// The emitted C for the i64 optional must use int64_t for the typedef
	// value field and int64_t for pebble_user_main's return type, proving the
	// entry's width threads into the optional layout.
	unit, snapshot, entryID := buildFixture(t, "fn main() i64 { let x ?i64 = some 22; return x!; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    int64_t value;\n} pebble_optional_",
		"static int64_t pebble_user_main(PebbleContext *ctx)",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t value;") {
		t.Errorf("emitted C declared an i32 optional value field for an i64 entry:\n%s", out)
	}
}

func TestEmitOptionalBoolWritesC(t *testing.T) {
	// The emitted C for the bool optional must use bool for the typedef value
	// field and the unwrap must use pebble_rt_checked_unwrap_bool.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let x ?bool = some true; if x! { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    bool value;\n} pebble_optional_",
		"pebble_rt_checked_unwrap_bool(",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitOptionalLocalInsideHelperCompilesAndRuns(t *testing.T) {
	// An optional-typed local declared inside a reachable helper's body, not
	// the entry's: the typedef-collection pass must walk helper bodies too, so
	// the optional typedef is emitted before the helper's definition (which is
	// before pebble_user_main). helper declares x, force-unwraps it, and
	// returns it; the entry just calls helper, so exit code 42 proves the
	// helper's optional local was built and the typedef emitted correctly.
	emitAndRun(t, "fn helper() i32 { let x ?i32 = some 42; return x!; } fn main() i32 { return helper(); }", false, 42, false)
}

func TestEmitRejectsOptionalLocalStore(t *testing.T) {
	// Reassigning an optional-typed local (x = some 5) is out of scope this
	// slice. The Store's place names an optional-typed local, so
	// buildLeadingStatement rejects it with a clear error naming the
	// reassignment, not a guessed lowering.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var x ?i32 = some 1; x = some 2; return x!; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "reassigning an optional is not supported")
}

func TestEmitRejectsOptionalWithUnsupportedPayloadType(t *testing.T) {
	// An optional whose payload type is neither the entry's width nor bool —
	// here a str payload — is reachable from real source (the checker builds
	// the declaration fine), so this is a genuine backend-scope rejection. The
	// optional typedef pass inspects the payload type first and rejects the
	// str field with a clear error naming the wanted types, so no C is written.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let x ?str = some \"hi\"; return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}

func TestEmitStructTwoFieldReadBackCompilesAndRuns(t *testing.T) {
	// The confirmation fixture for struct construction and a field read: a
	// two-field Point { x i32; y i32; } is declared from a struct literal and
	// its x field is read back into the return value. The struct type emits
	// one struct typedef pebble_struct_<typeID>_t with one field per declared
	// struct field (named from each field's symbol ID), the local is
	// initialized with a designated-initializer struct literal, and the field
	// read lowers to pebble_local_<id>.pebble_field_<x>, so the process exit
	// code is x's value (1).
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { let point Point = Point.{ x = 1, y = 2 }; return point.x; }", false, 1, false)
}

func TestEmitStructFieldsWrittenOutOfDeclaredOrderCompilesAndRuns(t *testing.T) {
	// The out-of-declaration-order construction fixture: Point.{ y = ..., x =
	// ... } writes the fields in the opposite order from the struct's declared
	// order, so the RecordConstruct's Fields list is [y, x], not [x, y]
	// (confirmed against a real fixture dump). The designated-initializer
	// struct literal places each value under the C field its member symbol
	// names, so x still reads 22 and y still reads 20 regardless of the
	// construction order — proving the designated-initializer approach solves
	// the ordering problem rather than accidentally working because the test
	// happens to write fields in order.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"x read back", "type Point = struct { x i32; y i32; };\nfn main() i32 { let point Point = Point.{ y = 20, x = 22 }; return point.x; }", 22},
		{"y read back", "type Point = struct { x i32; y i32; };\nfn main() i32 { let point Point = Point.{ y = 20, x = 22 }; return point.y; }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStructBoolFieldDrivesIfCompilesAndRuns(t *testing.T) {
	// A struct with a bool field beside an integer field: the bool field read
	// (p.b) drives an if condition. b = true runs the then-arm (exit 10);
	// with the bool field false the else-arm runs (exit 20). The read is a
	// bool value, so it must route through the bool grammar (buildBoolExpr's
	// Load case), not the integer one.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"bool true", "type Pair = struct { a i32; b bool; };\nfn main() i32 { let p Pair = Pair.{ a = 1, b = true }; if p.b { return 10; } else { return 20; } }", 10},
		{"bool false", "type Pair = struct { a i32; b bool; };\nfn main() i32 { let p Pair = Pair.{ a = 1, b = false }; if p.b { return 10; } else { return 20; } }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStructFieldAsCallArgumentCompilesAndRuns(t *testing.T) {
	// A struct field read composes with 10.18's call-argument building: each
	// argument of add is a read of the struct local's field. buildCallArguments
	// builds each argument with buildExpr, which lowers the Load(FieldPlace)
	// read to pebble_local_<id>.pebble_field_<member>, so add(20, 22) = 42 is
	// the process exit code. The struct typedef must still be emitted before
	// the helper's definition.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return add(p.x, p.y); }", false, 42, false)
}

func TestEmitStructThreeFieldTwoReadsAddedCompilesAndRuns(t *testing.T) {
	// The "two fields read and added" fixture: a three-field struct's a and c
	// fields are read back and added: t.a + t.c = 10 + 30 = 40. The typedef's
	// fields follow the declared order (a, b, c), and the two reads resolve
	// their own fields by member symbol.
	emitAndRun(t, "type T = struct { a i32; b i32; c i32; };\nfn main() i32 { let t T = T.{ a = 10, b = 20, c = 30 }; return t.a + t.c; }", false, 40, false)
}

func TestEmitI64StructCompilesAndRuns(t *testing.T) {
	// The width discipline extends to struct field types: an i64 entry's
	// (i64, i64) struct's typedef fields are int64_t, and the field read feeds
	// the i64 entry's return. Exit code 22.
	emitAndRun(t, "type T = struct { a i64; b i64; };\nfn main() i64 { let t T = T.{ a = 20, b = 22 }; return t.b; }", false, 22, false)
}

func TestEmitStructLocalInsideHelperCompilesAndRuns(t *testing.T) {
	// A struct-typed local declared inside a reachable helper's body, not the
	// entry's: the typedef-collection pass must walk helper bodies too, so the
	// struct typedef is emitted before the helper's definition (which is
	// before pebble_user_main). helper declares point, reads its y field, and
	// returns it; the entry just calls helper, so exit code 22 proves the
	// helper's struct local was built and the typedef emitted correctly.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn helper() i32 { let point Point = Point.{ x = 20, y = 22 }; return point.y; } fn main() i32 { return helper(); }", false, 22, false)
}

func TestEmitStructOutOfOrderWritesC(t *testing.T) {
	// The emitted C for the out-of-declaration-order construction fixture.
	// The typedef's fields must be in the struct's *declared* order (x = 25
	// then y = 26, from TypeDecl.Members), NOT the construction order the
	// RecordConstruct's Fields carry ([y, x] — confirmed against a real
	// fixture dump). The local initializer is a C99 designated-initializer
	// brace list placing each value under its own member's C field, and the
	// field read lowers to pebble_local_<id>.pebble_field_<member>. Symbols
	// 24 (Point), 25 (x), 26 (y), 28 (point), and struct type 23 come from the
	// real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { let point Point = Point.{ y = 2, x = 1 }; return point.x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"pebble_struct_23_t pebble_local_28 = { .pebble_field_26 = 2, .pebble_field_25 = 1 };",
		"    (void)pebble_local_28;",
		"return pebble_local_28.pebble_field_25;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	mainIndex := strings.Index(out, "static int pebble_user_main")
	if typedefIndex < 0 || mainIndex < 0 || typedefIndex > mainIndex {
		t.Errorf("struct typedef does not precede pebble_user_main (definition before use):\n%s", out)
	}
}

func TestEmitStructBoolFieldWritesC(t *testing.T) {
	// The emitted C for the bool-field-if fixture: the typedef's second field
	// must be the C bool, the local's initializer carries the integer and bool
	// field values under their designated fields, and the if condition is the
	// raw field read pebble_local_<id>.pebble_field_<b> (a C bool needs no
	// comparison). Symbols 25 (a), 26 (b), 28 (p), and struct type 23 come
	// from the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "type Pair = struct { a i32; b bool; };\nfn main() i32 { let p Pair = Pair.{ a = 1, b = true }; if p.b { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    bool pebble_field_26;\n} pebble_struct_23_t;",
		"pebble_struct_23_t pebble_local_28 = { .pebble_field_25 = 1, .pebble_field_26 = true };",
		"    if (pebble_local_28.pebble_field_26) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStructFieldAsCallArgumentWritesC(t *testing.T) {
	// The emitted C for the call-argument fixture: the struct typedef precedes
	// both the helper and the entry, the local initializes to a designated
	// struct literal, and the call site passes the two field reads after ctx.
	// Symbols 24 (Point), 25/26 (x/y), 27 (add), 28/29 (its parameters), and
	// 31 (p) come from the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return add(p.x, p.y); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"pebble_struct_23_t pebble_local_31 = { .pebble_field_25 = 20, .pebble_field_26 = 22 };",
		"return pebble_fn_27(ctx, pebble_local_31.pebble_field_25, pebble_local_31.pebble_field_26);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static int32_t pebble_fn_27")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("struct typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitI64StructWritesC(t *testing.T) {
	// The emitted C for the i64 struct must use int64_t for both typedef
	// fields and for the pebble_user_main return type, proving the entry's
	// width threads into the struct layout, not just the scalar declarations.
	unit, snapshot, entryID := buildFixture(t, "type T = struct { a i64; b i64; };\nfn main() i64 { let t T = T.{ a = 20, b = 22 }; return t.b; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int64_t pebble_field_25;\n    int64_t pebble_field_26;\n} pebble_struct_23_t;",
		"pebble_struct_23_t pebble_local_28 = { .pebble_field_25 = 20, .pebble_field_26 = 22 };",
		"return pebble_local_28.pebble_field_26;",
		"static int64_t pebble_user_main(PebbleContext *ctx)",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t pebble_field_") {
		t.Errorf("emitted C declared an i32 struct field for an i64 entry:\n%s", out)
	}
}

func TestEmitRejectsStructUnsupportedFieldType(t *testing.T) {
	// A struct whose field type is neither the entry's width nor bool — here a
	// str field — is reachable from real source (the checker builds the
	// declaration and construction fine), so this is a genuine backend-scope
	// rejection. The struct typedef pass inspects each field's resolved type
	// first and rejects the str field with a clear error naming the wanted
	// types, so no C is written.
	unit, snapshot, entryID := buildFixture(t, "type S = struct { s str; };\nfn main() i32 { let x S = S.{ s = \"hi\" }; return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}

func TestEmitRejectsStructWholeReassignment(t *testing.T) {
	// Reassigning a whole struct-typed local (p = Point.{ ... }) is out of
	// scope this slice. The Store's place names a struct-typed local, so
	// buildLeadingStatement rejects it with a clear error naming the
	// reassignment, not a guessed lowering.
	unit, snapshot, entryID := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { var p Point = Point.{ x = 1, y = 2 }; p = Point.{ x = 3, y = 4 }; return p.x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "reassigning a whole struct is not supported")
}

func TestEmitRejectsStructFieldAssignment(t *testing.T) {
	// Assigning into a struct field after construction (point.x = 5) is out of
	// scope this slice. The Store's place is a FieldPlace (confirmed against a
	// real fixture), which the existing Store handling rejects as not being a
	// plain StoragePlace — a clear error, not a guessed lowering.
	unit, snapshot, entryID := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { var point Point = Point.{ x = 1, y = 2 }; point.x = 5; return point.x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want a plain StoragePlace")
}

func TestEmitRejectsStructFieldReadOffLiteral(t *testing.T) {
	// Reading a field directly off a struct literal (Point.{ x = 1, y = 2 }.x)
	// is reachable from real source but lowers to a FieldValue whose base is
	// the RecordConstruct itself, not a StoragePlace naming a struct local — a
	// value-category shape out of scope this slice (only Load(FieldPlace) of a
	// struct local is supported). The integer expression builder rejects the
	// FieldValue cleanly rather than guessing.
	unit, snapshot, entryID := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { return Point.{ x = 1, y = 2 }.x; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNestedStructFieldAccess(t *testing.T) {
	// Nested field access (o.inner.x, where inner is itself a struct-typed
	// field) is reachable from real source but out of scope twice over: the
	// struct-of-struct field type is itself rejected by the typedef pass first
	// (a struct field must be the entry's width or bool), so the program is a
	// clean rejection naming the unsupported field type before the nested read
	// (a FieldPlace whose base is another FieldPlace) is even reached.
	unit, snapshot, entryID := buildFixture(t, "type Inner = struct { x i32; };\ntype Outer = struct { inner Inner; y i32; };\nfn main() i32 { let o Outer = Outer.{ inner = Inner.{ x = 7 }, y = 8 }; return o.inner.x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "is not supported, want i32 or bool")
}

// 10.25 — aggregate values as compound-literal expressions (inline
// construction as call arguments)

func TestEmitInlineTupleArgumentCompilesAndRuns(t *testing.T) {
	// The flagship 10.25 tuple fixture (10.24's TestEmitRejectsTupleParameter
	// fixture, now the positive case): a freshly-constructed tuple built inline
	// at the call site — f((20, 22)) — rather than passed as an already-
	// declared local. The DirectCall argument is a TupleValue carrying the
	// same Children/Type shape it has as a local's declaration initializer
	// (confirmed against a real fixture dump), built by buildTupleValueExpr as
	// the positional C99 compound literal (pebble_tuple_<typeID>_t){ 20, 22 }.
	// 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn f(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { return f((20, 22)); }", false, 42, false)
}

func TestEmitInlineStructArgumentCompilesAndRuns(t *testing.T) {
	// The flagship 10.25 struct fixture (10.24's
	// TestEmitRejectsStructTypedParameter fixture, now the positive case): a
	// freshly-constructed struct built inline at the call site —
	// f(Point.{ x = 20, y = 22 }) — rather than passed as an already-declared
	// local. The DirectCall argument is a RecordConstruct carrying the same
	// Fields/Type shape it has as a local's declaration initializer (confirmed
	// against a real fixture dump), built by buildStructValueExpr as the
	// designated-initializer C99 compound literal
	// (pebble_struct_<typeID>_t){ .pebble_field_<x> = 20,
	// .pebble_field_<y> = 22 }. 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return f(Point.{ x = 20, y = 22 }); }", false, 42, false)
}

func TestEmitInlineStructArgumentOutOfOrderCompilesAndRuns(t *testing.T) {
	// The designated-initializer reuse, verified in the inline-argument
	// position too: Point.{ y = 22, x = 20 } writes the fields in the opposite
	// order from the struct's declared order, and the compound literal still
	// places each value under the C field its member symbol names, so x reads
	// 20 and y reads 22 and the sum is 42. This is the argument-position analog
	// of TestEmitStructFieldsWrittenOutOfDeclaredOrderCompilesAndRuns — the
	// reordering problem is solved by the designated-initializer form in the
	// inline position exactly as it is in a local declaration, not just by a
	// fixture that happens to write fields in order.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return f(Point.{ y = 22, x = 20 }); }", false, 42, false)
}

func TestEmitInlineAggregateArgumentWritesC(t *testing.T) {
	// The emitted C for inline construction at a call site: each argument must
	// be the C99 compound-literal expression — not a local reference and not a
	// bare brace list — with the cast naming the aggregate's own typedef. The
	// tuple form is the positional (pebble_tuple_23_t){ 20, 22 }; the struct
	// form written out of declared order is
	// (pebble_struct_23_t){ .pebble_field_26 = 22, .pebble_field_25 = 20 }.
	// Symbols and type IDs come from the real fixture dumps (tuple: f=24,
	// tuple type 23; struct: Point=24, x=25, y=26, f=27, struct type 23).
	unit, snapshot, entryID := buildFixture(t, "fn f(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { return f((20, 22)); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "return pebble_fn_24(ctx, (pebble_tuple_23_t){ 20, 22 });") {
		t.Errorf("emitted C missing the tuple compound-literal argument:\n%s", out)
	}

	unit, snapshot, entryID = buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return f(Point.{ y = 22, x = 20 }); }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out = buf.String()
	if !strings.Contains(out, "return pebble_fn_27(ctx, (pebble_struct_23_t){ .pebble_field_26 = 22, .pebble_field_25 = 20 });") {
		t.Errorf("emitted C missing the struct compound-literal argument:\n%s", out)
	}
}

// 10.24 — struct-typed function parameters

func TestEmitStructParameterCompilesAndRuns(t *testing.T) {
	// The flagship struct-parameter fixture: f takes a whole Point struct as
	// its parameter and reads both fields back inside the callee. The entry
	// declares a struct local and passes it by value; the callee's parameter
	// seeds its own scope as a struct local, so the reads resolve through the
	// same Load(FieldPlace) machinery a struct local uses. 20 + 22 = 42 is the
	// process exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return f(p); }", false, 42, false)
}

func TestEmitStructParameterWritesC(t *testing.T) {
	// The emitted C for the flagship fixture: the struct typedef must precede
	// the helper, the helper's signature declares the parameter as
	// pebble_struct_<typeID>_t pebble_local_<paramSymbol> (symbol 28, the p
	// parameter), the parameter gets the same (void) cast, the body reads
	// pebble_local_28.pebble_field_25 / .pebble_field_26, and the call site
	// passes the entry's struct local pebble_local_30 directly (no construction
	// at the call site). Symbols 24 (Point), 25 (x), 26 (y), 27 (f), 28 (p
	// param), 29 (main), 30 (p local), and struct type 23 come from the real
	// fixture dump.
	unit, snapshot, entryID := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return f(p); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"static int32_t pebble_fn_27(PebbleContext *ctx, pebble_struct_23_t pebble_local_28) {",
		"    (void)pebble_local_28;",
		"    return pebble_rt_checked_add_i32(pebble_local_28.pebble_field_25, pebble_local_28.pebble_field_26);",
		"pebble_struct_23_t pebble_local_30 = { .pebble_field_25 = 20, .pebble_field_26 = 22 };",
		"return pebble_fn_27(ctx, pebble_local_30);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static int32_t pebble_fn_27")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("struct typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitStructParameterBoolFieldDrivesIfCompilesAndRuns(t *testing.T) {
	// A struct parameter whose fields are mixed width/bool: the callee reads
	// the bool field to drive an if and an integer field as a value. This
	// proves the parameter's field reads route through buildBoolExpr (the
	// Load(FieldPlace) bool path) exactly as a struct local's do. With b true
	// the then-arm runs and returns the x field 10.
	emitAndRun(t, "type Pair = struct { x i32; b bool; };\nfn f(p Pair) i32 { if p.b { return p.x; } else { return 99; } } fn main() i32 { let p Pair = Pair.{ x = 10, b = true }; return f(p); }", false, 10, false)
}

func TestEmitStructParameterParamOnlyTypeGetsTypedef(t *testing.T) {
	// The typedef-discovery extension, struct side: the Point type appears ONLY
	// as f's parameter type — f is never called (so no reachable body
	// constructs a Point of that type) and main constructs no struct at all —
	// yet the typedef must still be discovered, because the emitted helper's C
	// signature names pebble_struct_<typeID>_t. Before 10.24's Parameters scan
	// in collectStructTypes this returned nothing; the test drives
	// collectStructTypes directly with a hand-built reachable-helper slice, so
	// it fails if the discovery stops being tied to a construction site. (The
	// concrete type ID 23 is confirmed from the fixture dump; the callee reads
	// both fields, so resolveStructInfo has every field's type available.)
	unit, snapshot, entryID := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return 0; }", "main", false)
	entryDecl, err := findFunctionDeclaration(unit, entryID, "entry function")
	if err != nil {
		t.Fatalf("entry declaration: %v", err)
	}
	_, entryBlock, err := findFunctionBody(unit, entryDecl, "entry function")
	if err != nil {
		t.Fatalf("entry body: %v", err)
	}
	fDecl, err := findFunctionDeclaration(unit, 27, "called function")
	if err != nil {
		t.Fatalf("f declaration: %v", err)
	}
	_, fBody, err := findFunctionBody(unit, fDecl, "called function")
	if err != nil {
		t.Fatalf("f body: %v", err)
	}
	helpers := []helperInfo{{decl: fDecl, block: fBody}}
	infos, err := collectStructTypes(unit, snapshot, entryBlock, helpers)
	if err != nil {
		t.Fatalf("collectStructTypes failed: %v", err)
	}
	found := false
	for _, info := range infos {
		if info.typ == 23 {
			found = true
		}
	}
	if !found {
		t.Fatalf("struct type 23 used only as a parameter type was not discovered, got %+v", infos)
	}
}

// 10.23 — str values (literal locals + equality)

func TestEmitStrLocalUnusedCompilesClean(t *testing.T) {
	// A str-typed local declared and never referenced beyond its own
	// declaration must still compile clean under -Wall -Wextra -Werror: the
	// emitted PebbleStr declaration carries the escaped bytes and compile-time
	// length, and the (void) cast immediately after suppresses the
	// -Wunused-variable warning exactly as every other local's does.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; return 1; }", false, 1, false)
}

func TestEmitStrLocalEscapedUnusedCompilesClean(t *testing.T) {
	// Same unused-local shape but with a decoded content that forces C escapes
	// (newline, tab, quote, backslash, and a control byte), so the escaped
	// C literal itself is exercised under -Wall -Wextra -Werror (a malformed
	// escape would not compile, and a silently-wrong one would still compile
	// here — the byte-correctness is asserted by the round-trip test below).
	emitAndRun(t, "fn main() i32 { let s str = \"a\\n1\\t\\\"\\\\\\0\"; return 1; }", false, 1, false)
}

func TestEmitStrEqualLiteralsCompilesAndRuns(t *testing.T) {
	// Two identical string literals compared equal, driving an if: the
	// comparison is between two StringLiteral operands (no local involved),
	// each embedded as a PebbleStr compound literal, so the then-arm runs and
	// the process exits 10.
	emitAndRun(t, "fn main() i32 { if \"hi\" == \"hi\" { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrDifferentLiteralsCompilesAndRuns(t *testing.T) {
	// Two different string literals compared equal (false): the lengths are
	// equal but the bytes differ, so pebble_rt_str_eq returns false and the
	// else-arm runs, exiting 20.
	emitAndRun(t, "fn main() i32 { if \"hi\" == \"ho\" { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrLocalAndLiteralEqualCompilesAndRuns(t *testing.T) {
	// A str local compared against a string literal — the mixed-operand shape:
	// one SymbolValue (a str local) and one StringLiteral. The local was
	// declared from the same decoded bytes as the literal, so equality holds
	// and the then-arm runs, exiting 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; if s == \"hi\" { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrNotEqualCompilesAndRuns(t *testing.T) {
	// != between two str locals, both directions: different strings are not
	// equal (then-arm, exit 10) and identical strings are not-not-equal
	// (else-arm, exit 20), so the negation of pebble_rt_str_eq is exercised
	// for both a true and a false outcome.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"different not equal", "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s != t { return 10; } else { return 20; } }", 10},
		{"identical not equal false", "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; if s != t { return 10; } else { return 20; } }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStrEqualityAsBoolValueCompilesAndRuns(t *testing.T) {
	// A str comparison used as a plain bool value (not just as an if/while
	// condition): the equality result is stored in a bool local and that local
	// drives the if. The comparison lowers to pebble_rt_str_eq, whose bool
	// result is the bool local's initializer, so the then-arm runs, exiting 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; let b bool = s == t; if b { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrEqualityAsLogicalOperandCompilesAndRuns(t *testing.T) {
	// A str comparison combined with && — the equality as a logical operand of
	// a larger bool expression. Both comparisons hold, so the conjunction is
	// true and the then-arm runs, exiting 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s == \"hi\" && t == \"ho\" { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrEqualityInWhileCompilesAndRuns(t *testing.T) {
	// A str == comparison as a bare while loop condition: the loop runs while
	// the sentinel string is "go" (it never changes — str locals are not
	// reassignable this slice), accumulating a counter until an in-loop
	// integer comparison breaks it. This exercises pebble_rt_str_eq through
	// buildCondition on a while (whose condition grammar routes BinaryValue to
	// buildComparison), not just an if, and runs under the bounded harness so
	// a miscompiled non-terminating loop fails loudly instead of hanging.
	emitAndRunBounded(t, "fn main() i32 { let s str = \"go\"; var n i32 = 0; while s == \"go\" { n = n + 1; if n == 2 { break; } } return n; }", false, 2, false)
}

func TestEmitStrEscapeRoundTripCompilesAndRuns(t *testing.T) {
	// The escaping-correctness fixture: a decoded literal containing a control
	// byte immediately followed by a digit character, plus an escaped quote
	// and backslash, compared against a differently-spelled literal that
	// decodes to the same bytes. The Pebble source spells the first with \\n,
	// \\t, \\", and \\\\ (the escapes that decode to newline, tab, quote, and
	// backslash) and the second with \\xHH byte escapes for the same four
	// bytes. If the emitter escaped either decoded string naively — e.g. a C
	// \\xHH hex escape, where C's maximal-munch rule would absorb the
	// following hex digit ('1' after newline, 'b' after tab — 0x0a1/0x09b are
	// a single wrong byte, not two) — the two C strings would not round-trip
	// to the same bytes and the equality would fail, exiting 3. The fixed-
	// width octal escapes (\\012 then '1', \\011 then 'b', \\042 for the
	// quote, \\134 for the backslash) make each escape self-delimiting, so
	// both sides reconstruct exactly the same 9 bytes and the then-arm runs,
	// exiting 7. Two sub-cases: both operands are literals directly, and one
	// operand is a local holding the same decoded content.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"two literals", "fn main() i32 { if \"a\\n1\\tb\\\"c\\\\d\" == \"a\\x0a1\\x09b\\x22c\\x5cd\" { return 7; } else { return 3; } }"},
		{"local vs literal", "fn main() i32 { let s str = \"a\\n1\\tb\\\"c\\\\d\"; if s == \"a\\x0a1\\x09b\\x22c\\x5cd\" { return 7; } else { return 3; } }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, 7, false)
		})
	}
}

func TestEmitStrEscapeRoundTripWritesC(t *testing.T) {
	// The literal escaped C text the round-trip fixture produces: the tab and
	// newline bytes must be emitted as fixed-width octal escapes, never as C
	// \\x hex escapes (which would absorb the following '1'/'b'), the quote as
	// \\", and the backslash as \\\\, so the emitted C string-literal body is
	// exactly a\\0121\\011b\\"c\\\\d. The .len field must carry the decoded
	// byte length 9. Symbol 25 is the s local, confirmed against the real
	// fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let s str = \"a\\n1\\tb\\\"c\\\\d\"; if s == \"a\\x0a1\\x09b\\x22c\\x5cd\" { return 7; } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_25 = { .data = (const uint8_t *)\"a\\0121\\011b\\\"c\\\\d\", .len = 9 };",
		"    (void)pebble_local_25;",
		"if (pebble_rt_str_eq(pebble_local_25, (PebbleStr){ .data = (const uint8_t *)\"a\\0121\\011b\\\"c\\\\d\", .len = 9 })) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrWritesC(t *testing.T) {
	// The emitted C for a str local compared against a string literal: the
	// local is declared directly as the runtime's PebbleStr (no typedef) with
	// the escaped bytes and compile-time length, and the equality lowers to
	// pebble_rt_str_eq(<local>, <literal-as-compound-literal>) — the literal
	// operand needs no declared local, so it is built inline as a PebbleStr
	// compound literal. Symbol 25 is the s local, confirmed against the real
	// fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let s str = \"hi\"; if s == \"hi\" { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_25 = { .data = (const uint8_t *)\"hi\", .len = 2 };",
		"    (void)pebble_local_25;",
		"if (pebble_rt_str_eq(pebble_local_25, (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 })) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrNotEqualWritesC(t *testing.T) {
	// The != lowering must negate the runtime helper: s != t emits
	// !pebble_rt_str_eq(pebble_local_25, pebble_local_26), not a comparison of
	// the two strings some other way. Symbols 25/26 are the s and t locals,
	// confirmed against the real fixture dump.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s != t { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_25 = { .data = (const uint8_t *)\"hi\", .len = 2 };",
		"PebbleStr pebble_local_26 = { .data = (const uint8_t *)\"ho\", .len = 2 };",
		"if (!pebble_rt_str_eq(pebble_local_25, pebble_local_26)) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitRejectsStrOrderingComparison(t *testing.T) {
	// An ordering comparison between two str values (s < "ho") is reachable
	// from real source — the checker does not reject it, confirmed against a
	// real fixture dump (a BinaryValue with operator Less and two str
	// operands) — so it is a genuine backend-scope rejection, not a
	// hand-built-IR shape. buildComparison's str path rejects any operator
	// other than ==/!= with a clear error.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let s str = \"hi\"; if s < \"ho\" { return 1; } else { return 0; } }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "compares two str operands")
}

func TestEmitRejectsStrReassignment(t *testing.T) {
	// Reassigning a str-typed local (s = "ho") is reachable from real source
	// but out of scope this slice: a str local is only ever initialized from
	// a string literal and then compared. The Store's place names a str-typed
	// local, so buildLeadingStatement rejects it with a clear error naming the
	// reassignment.
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { var s str = \"hi\"; s = \"ho\"; return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "reassigning a str is not supported")
}

func TestEmitRejectsStrIndexing(t *testing.T) {
	// String indexing (s[0]) is reachable from real source — confirmed
	// against a real fixture dump: `let c char = s[0];` lowers the read to a
	// tir.CheckedIndex node whose result type is char, a separate mechanism
	// this backend does not build for str (and a char-typed value is not a
	// supported local type). The declaration is therefore a clean rejection
	// naming the found type, never a guessed lowering. (The exact shape
	// `s[0] as i32` is rejected by the checker itself before typed IR — typed
	// IR construction failed — so the reachable form here is the char-typed
	// read.)
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let c char = s[0]; return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}

func TestEmitRejectsStrParameter(t *testing.T) {
	// A function taking a str parameter is reachable from real source but out
	// of scope this slice: validateHelperSignature requires every parameter to
	// be the entry's width or bool, and str is neither.
	unit, snapshot, entryID := buildFixture(t, "fn f(s str) i32 { return 1; } fn main() i32 { return f(\"hi\"); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}
