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
	"strconv"
	"strings"
	"sync"
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
	mem, err := os.ReadFile("../../../std/mem.peb")
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

func TestCheckStdMemImport(t *testing.T) {
	unit, _, _, _ := buildStdMemFixture(t, `import "std:mem"; fn main() i32 { return 0; }`, "main")
	if unit == nil {
		t.Fatal("std:mem import produced no IR")
	}
}

func TestEmitStdMemNewSliceCompilesAndRuns(t *testing.T) {
	unit, snapshot, entryID, sources := buildStdMemFixture(t, `import "std:mem"; fn main() i32 { var values []i32 = mem::new_slice[i32](3); values[0] = 42; return values[0]; }`, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitGenericReachabilityUsesSpecializationIdentity(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, `fn add_one[T](x T, y T) T => x; fn main() i32 { var a i32 = add_one[i32](40, 1); let p *i32 = &a; let b *i32 = add_one[*i32](p, p); return a + *b; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 80, false)
}

func TestEmitGenericReachabilityEmitsThreeSpecializations(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, `fn choose[T](x T) i32 => 7; fn main() i32 { var a i32 = choose[i32](1); var b i32 = choose[bool](true); let p *i32 = &a; var c i32 = choose[*i32](p); return a + b + c; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 21, false)
}

func TestEmitSliceFromRawCompilesAndRuns(t *testing.T) {
	unit, snapshot, entryID, sources := buildStdFixture(t, "fn main() i32 { var value i32 = 42; var ptr *i32 = &value; let values []i32 = slice ptr, 1; return values[0]; }", "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitRuntimeAllocatorValueCompiles(t *testing.T) {
	emitRuntimeAndRun(t, "fn main() i32 { let a = context.default_allocator; return 0; }", 0)
}

func TestEmitRuntimeAllocatorStateFieldCompiles(t *testing.T) {
	emitRuntimeAndRun(t, "fn main() i32 { let a = context.default_allocator; let p = a.ptr; return 0; }", 0)
}

func TestEmitRuntimeAllocatorRoundTrip(t *testing.T) {
	emitRuntimeAndRun(t, "fn main() i32 { let a = context.default_allocator; var p *i32 = (a.alloc)(a.ptr, 4) as *i32; *p = 42; let value = *p; (a.free)(a.ptr, p as *void); return value; }", 42)
}

func TestEmitRuntimeAllocatorUnparenthesizedRoundTrip(t *testing.T) {
	emitRuntimeAndRun(t, "fn main() i32 { let a = context.default_allocator; var p *i32 = a.alloc(a.ptr, 4) as *i32; *p = 42; let value = *p; a.free(a.ptr, p as *void); return value; }", 42)
}

func emitRuntimeAndRun(t *testing.T, sourceText string, wantCode int) {
	t.Helper()
	unit, snapshot, entryID, sources := buildStdFixture(t, sourceText, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), wantCode, false)
}

func TestEmitEmptyEntryWritesC(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() void {}", "main", true)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() void {}", "main", true)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

func TestEmitIntegerReturnEntryWritesC(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 42; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitValueMethodCallReadsReceiverField(t *testing.T) {
	emitAndRun(t, `type Point = struct { x i32; fn get(self Point) i32 => self.x; }; fn main() i32 { let p Point = Point.{ x = 41 }; return p.get(); }`, false, 41, false)
}

func TestEmitIndirectlyReachedMethodCall(t *testing.T) {
	emitAndRun(t, `type Point = struct { x i32; fn get(self Point) i32 => self.x; }; fn read(p Point) i32 { return p.get(); } fn main() i32 { let p Point = Point.{ x = 42 }; return read(p); }`, false, 42, false)
}

func TestEmitMethodCallWithExplicitArgument(t *testing.T) {
	// Was blocked until the call_validation.go fix (a real checker bug
	// compared a method call's argument count against its generic
	// type-argument count, wrongly rejecting any non-generic method call
	// with an argument beyond the receiver) — proves the receiver field and
	// the explicit argument both flow through correctly.
	emitAndRun(t, `type Point = struct { x i32; fn add(self Point, delta i32) i32 => self.x + delta; }; fn main() i32 { let p Point = Point.{ x = 40 }; return p.add(2); }`, false, 42, false)
}

func TestEmitPointerReceiverMethodCallCompilesAndRuns(t *testing.T) {
	// Was rejected before raw pointers landed (a pointer receiver's self
	// parameter has no backend representation until then) — since the
	// pointer backend-lowering slice, a pointer receiver is just an ordinary
	// pointer-typed parameter, so this now compiles and runs correctly.
	emitAndRun(t, `type Point = struct { fn get(self *Point) i32 => 1; }; fn main() i32 { let p *Point = nil; return p.get(); }`, false, 1, false)
}

func TestEmitFieldNilAssignmentRoundTripCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `type P = struct { d *i32; }; fn main() i32 { var value i32 = 7; var p P = P.{ d = &value }; p.d = nil; if p.d == nil { return 1; } else { return 0; } }`, false, 1, false)
}

func TestEmitRejectsGenericMethodCall(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, `type Box = struct { fn echo[T](self Box, value T) T => value; }; fn main() i32 { let box Box = Box.{}; return box.echo(42); }`, "main", false)
	var buf bytes.Buffer
	err := Emit(unit, snapshot, entryID, sources, &buf)
	if err == nil {
		t.Fatal("Emit accepted a generic method call, want the existing generic-call rejection")
	}
	if !strings.Contains(err.Error(), "generic") || !strings.Contains(err.Error(), "type argument") {
		t.Fatalf("generic-method rejection is not descriptive: %v", err)
	}
}

func TestEmitIntEntryExpressionCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() int => 0;", true, 0, false)
}

func TestEmitIntEntryCheckedAddCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() int { let x int = 40; let y int = 2; return x + y; }", true, 42, false)
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

func TestEmitCheckedArithmeticOverflowEmitsRealSourceLoc(t *testing.T) {
	// The overflow behavior is unchanged (the process still aborts via
	// pebble_rt_panic), and — new for this slice — the emitted
	// pebble_rt_checked_add_i32 call now carries the checked expression's own
	// resolved Pebble source location as its final argument, not the
	// zero-valued placeholder: (PebbleSourceLoc){"main.peb", 1, 24} for the
	// `2147483647 + 1` expression on the fixture's single line. Both halves
	// are proved here: the emitted C text is inspected directly for the
	// non-placeholder compound literal, and the compiled binary is run to
	// confirm the overflow still panics.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 2147483647 + 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, `pebble_rt_checked_add_i32(2147483647, 1, (PebbleSourceLoc){"main.peb", 1, 24})`) {
		t.Errorf("emitted C lacks a real source location on the checked-add call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { if 1 < 2 { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var x i32 = 7; if x < 10 && 1 < 2 { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var flag bool = true; if flag && (1 < 2) { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { if 1 < 2 { if 3 < 4 { return 1; } else { return 2; } } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", requireEntry)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", requireEntry)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	err := Emit(unit, snapshot, entryID, nil, &buf)
	if err == nil {
		t.Fatal("Emit succeeded for an unsupported entry shape")
	}
	if buf.Len() != 0 {
		t.Fatalf("Emit wrote output on failure: %q", buf.String())
	}
}

func TestEmitRejectsNonEmptyBody(t *testing.T) {
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() void { let x i32 = 1; }", "main", false)
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
	// A Store's place must be StoragePlace, CheckedIndexPlace, or
	// DereferencePlace. Real source never produces a TuplePlace as a Store's
	// writable target (reassigning a whole tuple element in place is not
	// supported), so this shape is hand-built through the IR builder to
	// exercise Emit's own place-kind requirement.
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
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var i i32 = 0; while i < 3 { print(\"hi\"); i = i + 1; } return i; }", "main", false)
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i < 5 { sum = sum + i; } i = i + 1; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    while (pebble_local_25 < 10) {\n",
		"        if (pebble_local_25 < 5) {\n",
		"            pebble_local_26 = pebble_rt_checked_add_i32(pebble_local_26, pebble_local_25, (PebbleSourceLoc){\"main.peb\", 1, 81});",
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var flag bool = true; if flag { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var done bool = false; var i i32 = 0; var sum i32 = 0; while !done { sum = sum + i; i = i + 1; if i == 5 { done = true; } } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { if (1 < 2) == (3 < 4) { return 1; } else { return 2; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources = buildFixture(t, "fn main() i32 { var a bool = true; var b bool = false; if a == b { return 1; } else { return 2; } }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 0; while !(i >= 5) { i = i + 1; } return i; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    while (!(pebble_local_25 >= 5)) {\n",
		"        pebble_local_25 = pebble_rt_checked_add_i32(pebble_local_25, 1, (PebbleSourceLoc){\"main.peb\", 1, 54});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitRejectsVariableReturn(t *testing.T) {
	// A variable reference lowers to a SymbolValue, which is not a supported
	// expression node for the i32 entry's return value.
	unit, snapshot, entryID, _ := buildFixture(t, "let x i32 = 1; fn main() i32 { return x; }", "main", false)
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
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { print(\"hi\"); return 1; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsParameters(t *testing.T) {
	unit, snapshot, entryID, _ := buildFixture(t, "fn main(args []str) void {}", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUnsupportedResultType(t *testing.T) {
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() u32 { return 0; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUnknownEntrySymbol(t *testing.T) {
	unit, snapshot, _, _ := buildFixture(t, "fn main() void {}", "main", true)
	assertEmitRejects(t, unit, snapshot, symbol.SymbolID(0x7FFFFFFF))
}

func TestEmitNilArguments(t *testing.T) {
	empty := &tir.Unit{}
	snapshot := &types.Snapshot{}
	if err := Emit(nil, snapshot, 0, nil, &bytes.Buffer{}); err == nil {
		t.Fatal("Emit accepted nil unit")
	}
	if err := Emit(empty, nil, 0, nil, &bytes.Buffer{}); err == nil {
		t.Fatal("Emit accepted nil snapshot")
	}
	if err := Emit(empty, snapshot, 0, nil, nil); err == nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 10 { if i == 5 { break; } sum = sum + i; i = i + 1; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { i = i + 1; if i == 3 { continue; } sum = sum + i; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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

func TestEmitRejectsBreakWithUnsupportedDeferStatement(t *testing.T) {
	// A real-source break inside a loop body that also contains a `defer` with
	// an unsupported deferred statement kind (Print) produces a Break whose
	// DeferChain references a Print node. The backend now attempts to emit
	// deferred statements but correctly rejects Print as an unsupported
	// deferred statement kind (only Store reassignment is currently supported).
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var i i32 = 0; while i < 3 { break; defer print 5; } return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "not a supported deferred statement kind")
}

func TestEmitRejectsContinueWithUnsupportedDeferStatement(t *testing.T) {
	// Same unsupported deferred statement rejection for Continue: a defer with
	// a Print node is correctly rejected as an unsupported deferred statement
	// kind, rather than being silently dropped or rejected for DeferChain.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; continue; defer print 5; } return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "not a supported deferred statement kind")
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

func TestEmitRangeLoopAccumulationCompilesAndRuns(t *testing.T) {
	// The confirmation fixture: a range loop lowers to a C for loop over the
	// bound iterator, and the iterator is an ordinary C loop counter the body
	// can read. i counts 0..3 (exclusive), sum accumulates i each pass, so
	// sum = 0+1+2 = 3, returned as the process exit code. The iterator's C
	// type is the entry's width (int32_t) and the body references it through
	// the seeded local scope like any other local. Bounded execution in case
	// of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", false, 3, false)
}

func TestEmitRangeLoopExclusiveVsInclusiveCompilesAndRuns(t *testing.T) {
	// The inclusive form (`..=`) differs from the exclusive form (`..`) by one
	// iteration: 0..3 sums 0+1+2 = 3, while 0..=3 sums 0+1+2+3 = 6 — the
	// emitted C condition is `<` for the exclusive form and `<=` for the
	// inclusive form (RangeLoop.RangeInclusive). Each row's expected exit code
	// therefore proves the right operator was chosen. Bounded execution.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"exclusive", "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", 3},
		{"inclusive", "fn main() i32 { var sum i32 = 0; loop 0..=3 : i { sum = sum + i; } return sum; }", 6},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRangeLoopBreakAndContinueCompilesAndRuns(t *testing.T) {
	// break/continue inside a range loop body work through the same
	// buildLoopJump machinery a while body uses: a Break's/Continue's Target
	// names the RangeLoop's own Region (confirmed against a real fixture dump),
	// and plain C break/continue already target the nearest enclosing loop,
	// which the emitted for loop is. i counts 0..9, breaking when i == 5 and
	// skipping the accumulation when i == 3, so sum = 0+1+2+4 = 7, returned as
	// the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var sum i32 = 0; loop 0..10 : i { if i == 5 { break; } if i == 3 { continue; } sum = sum + i; } return sum; }", false, 7, false)
}

func TestEmitRangeLoopNestedInRangeLoopCompilesAndRuns(t *testing.T) {
	// A range loop nested inside another range loop: the inner RangeLoop is a
	// plain statement in the outer loop's body Block, dispatched by
	// buildLoopBody exactly like a nested While. i and j each count 0..2, so
	// the inner body runs 3 x 3 = 9 times and total = 9, returned as the exit
	// code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; loop 0..3 : i { loop 0..3 : j { total = total + 1; } } return total; }", false, 9, false)
}

func TestEmitRangeLoopInsideWhileCompilesAndRuns(t *testing.T) {
	// A range loop nested inside a while loop's body: the RangeLoop is a
	// statement in the while's body Block, dispatched by buildLoopBody. Each
	// of the 2 while iterations runs the range loop's 3 inner iterations, so
	// total = 6, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 2 { loop 0..3 : j { total = total + 1; } i = i + 1; } return total; }", false, 6, false)
}

func TestEmitRangeLoopNonLiteralBoundsCompilesAndRuns(t *testing.T) {
	// The start/end are ordinary integer expressions built by buildExpr at
	// the entry's width — a local reference for the end, and checked
	// arithmetic for the start. With the iterator anchored by the i32
	// accumulation, n = 3 makes 1..=n sum 1+2+3 = 6, and the arithmetic start
	// 1+2 = 3 makes 3..=5 sum 3+4+5 = 12. Bounded execution.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"local reference bound", "fn main() i32 { var n i32 = 3; var sum i32 = 0; loop 1..=n : i { sum = sum + i; } return sum; }", 6},
		{"arithmetic bound", "fn main() i32 { var sum i32 = 0; loop 1+2..=5 : i { sum = sum + i; } return sum; }", 12},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRunBounded(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitRangeLoopIteratorComparisonOnlyCompilesAndRuns(t *testing.T) {
	// The iterator referenced only in a comparison (`if i == 2`) — never in a
	// width-anchoring position — stays the checker's unanchored int builtin
	// (confirmed against a real fixture dump), so the comparison operand
	// lowers it directly as its pebble_local_<symbol> name via
	// buildComparisonOperand's int-typed-SymbolValue case, and both bounds are
	// int-typed literals lowered as their decimal text. i counts 0..2, and the
	// accumulation happens once (when i == 2), so sum = 1. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { if i == 2 { sum = sum + 1; } } return sum; }", false, 1, false)
}

func TestEmitRangeLoopUnusedIteratorCompilesAndRuns(t *testing.T) {
	// A bound range loop whose iterator is never read in the body: the loop
	// still iterates over its C counter (the condition and increment read it),
	// so the body runs 3 times and sum = 3. The bounds are int-typed literals
	// (nothing anchors them), lowered as their decimal text. Bounded
	// execution.
	emitAndRunBounded(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + 1; } return sum; }", false, 3, false)
}

func TestEmitRangeLoopNestedIteratorAsInnerBoundCompilesAndRuns(t *testing.T) {
	// A nested range loop whose bound reads the outer loop's iterator
	// (`loop 0..i : j`): the outer iterator is an int-typed SymbolValue in the
	// inner loop's end position, lowered as its pebble_local_<symbol> name by
	// buildRangeBound's int-typed-SymbolValue case. The inner loop runs 0, 1,
	// and 2 iterations for i = 0, 1, 2, so total = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; loop 0..3 : i { loop 0..i : j { total = total + 1; } } return total; }", false, 3, false)
}

func TestEmitRangeLoopArrayIndexCompilesAndRuns(t *testing.T) {
	// The iterator used as an array index (`a[i]`): the int-typed iterator
	// SymbolValue is lowered as its pebble_local_<symbol> name by the
	// array-index int-typed-SymbolValue case, so the sum of the three elements
	// (10+20+30) = 60 is returned. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; var sum i32 = 0; loop 0..3 : i { sum = sum + a[i]; } return sum; }", false, 60, false)
}

func TestEmitRangeLoopI64EntryCompilesAndRuns(t *testing.T) {
	// A range loop inside an i64 entry: the iterator's C type follows the
	// entry's width (int64_t), and the bounds/iterator are anchored to i64 by
	// the i64 accumulation. sum = 0+1+2 = 3, returned as the exit code.
	// Bounded execution.
	emitAndRunBounded(t, "fn main() i64 { var sum i64 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", false, 3, false)
}

func TestEmitRangeLoopHelperCallBoundCompilesAndRuns(t *testing.T) {
	// A helper call as a range-loop bound: the end is a DirectCall to a
	// helper returning the entry's width, built by buildExpr exactly as any
	// other call expression, and emitted in the for-loop condition. five()
	// returns 5, so 0..five() sums 0+1+2+3+4 = 10. Bounded execution.
	emitAndRunBounded(t, "fn five() i32 { return 5; } fn main() i32 { var sum i32 = 0; loop 0..five() : i { sum = sum + i; } return sum; }", false, 10, false)
}

func TestEmitRangeLoopWritesC(t *testing.T) {
	// The emitted C for the flagship fixture must be a C for loop whose
	// init/condition/increment all use the iterator's own
	// pebble_local_<symbol> name as an ordinary C loop counter at the entry's
	// width: `for (int32_t pebble_local_26 = 0; pebble_local_26 < 3;
	// pebble_local_26++)`. Symbols 25 (sum) and 26 (the iterator) come from
	// the real fixture dump. The inclusive form's condition must instead be
	// `<=` (RangeLoop.RangeInclusive), so the two operators are distinguishable
	// in the emitted text.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var sum i32 = 0; loop 0..3 : i { sum = sum + i; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    for (int32_t pebble_local_26 = 0; pebble_local_26 < 3; pebble_local_26++) {\n",
		"        pebble_local_25 = pebble_rt_checked_add_i32(pebble_local_25, pebble_local_26, (PebbleSourceLoc){\"main.peb\", 1, 56});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	unit, snapshot, entryID, sources = buildFixture(t, "fn main() i32 { var sum i32 = 0; loop 0..=3 : i { sum = sum + i; } return sum; }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out = buf.String()
	if !strings.Contains(out, "    for (int32_t pebble_local_26 = 0; pebble_local_26 <= 3; pebble_local_26++) {\n") {
		t.Errorf("emitted C missing the inclusive for-loop header:\n%s", out)
	}
}

func TestEmitRejectsUnboundRangeLoop(t *testing.T) {
	// The unbound form (`loop start..end { ... }`, no `: name`) builds a
	// RangeLoop whose Symbol field is zero (confirmed against a real fixture
	// dump — nothing attaches an iterator), and there is no way to observe
	// such a loop's iteration count from inside the body, so it is rejected
	// cleanly rather than lowered with a synthetic counter the source never
	// names.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var sum i32 = 0; loop 0..3 { sum = sum + 1; } return sum; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "unbound range loop")
}

// assertEmitRejectsContaining is assertEmitRejects for rejections whose error
// message must name a specific part of the unsupported shape (here: the
// non-empty DeferChain the backend refuses to drop).
func assertEmitRejectsContaining(t *testing.T, unit *tir.Unit, snapshot *types.Snapshot, entryID symbol.SymbolID, wantSubstring string) {
	t.Helper()
	var buf bytes.Buffer
	err := Emit(unit, snapshot, entryID, nil, &buf)
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

func TestEmitForLoopAccumulationCompilesAndRuns(t *testing.T) {
	// The flagship fixture: a classic for loop lowers to a C for loop with
	// the same three clauses. step counts 0..3 (the initializer declares it as
	// an ordinary local of the entry's width, seeded into the loop's scope),
	// and total accumulates step each pass, so total = 0+1+2 = 3, returned as
	// the process exit code. The body references step through the seeded scope
	// like any other local. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; step = step + 1 { total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopAllClausesOmittedCompilesAndRuns(t *testing.T) {
	// for ;; { ... } — all three clauses absent: an infinite loop from the
	// header's perspective, so termination comes only from the explicit break
	// in the body. The body advances its own counter (declared outside) and
	// breaks at 3, so the program terminates with i = 3 as the exit code.
	// Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; for ;; { if i >= 3 { break; } i = i + 1; } return i; }", false, 3, false)
}

func TestEmitForLoopNoConditionNeedsBreakCompilesAndRuns(t *testing.T) {
	// for var i i32 = 0;; i = i + 1 { ... } — no condition clause (the
	// initializer and update are both present), so termination comes only from
	// the explicit break in the body — the omitted-condition combination the
	// brief calls out explicitly. total accumulates 0+1+2 = 3 before the break
	// at i == 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0;; i = i + 1 { if i >= 3 { break; } total = total + i; } return total; }", false, 3, false)
}

func TestEmitForLoopNoUpdateCompilesAndRuns(t *testing.T) {
	// for var step i32 = 0; step < 3; { ... } — no update clause; the body
	// advances step itself. total accumulates 0+1+2 = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; { total = total + step; step = step + 1; } return total; }", false, 3, false)
}

func TestEmitForLoopInitializerOnlyCompilesAndRuns(t *testing.T) {
	// for var i i32 = 0;; { ... } — initializer only (no condition, no
	// update), so the body advances i and breaks when it reaches 3. total
	// accumulates 0+1+2 = 3. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0;; { if i >= 3 { break; } total = total + i; i = i + 1; } return total; }", false, 3, false)
}

func TestEmitForLoopUpdateOnlyCompilesAndRuns(t *testing.T) {
	// for ; ; i = i + 1 { ... } — update only (no initializer, no condition):
	// i is declared outside, the header's update advances it, and the body
	// breaks when it reaches 3, so i = 3 is returned. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; for ; ; i = i + 1 { if i >= 3 { break; } } return i; }", false, 3, false)
}

func TestEmitForLoopConditionOnlyCompilesAndRuns(t *testing.T) {
	// for ; i < 3; { ... } — condition only (no initializer, no update): i is
	// declared outside, the header's condition alone controls the loop, and
	// the body both accumulates and advances i. This shape was blocked by a
	// real tir/verify.go bug (the verifier wrongly required For's first
	// child to always be CategoryNonvalue, but with no initializer the first
	// child is the condition itself, CategoryValue) until that bug was fixed
	// separately (commit "tir: fix verifier rejecting well-formed classic
	// for-loops missing an initializer"). sum accumulates 0+1+2 = 3.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; for ; i < 3; { sum = sum + i; i = i + 1; } return sum; }", false, 3, false)
}

func TestEmitForLoopConditionAndUpdateCompilesAndRuns(t *testing.T) {
	// for ; i < 3; i = i + 1 { ... } — condition and update, no initializer:
	// i is declared outside, the header's condition controls the loop and
	// its update advances i, so the body only needs to accumulate. Also
	// blocked by the same tir bug as the condition-only case above (the
	// verifier's fixed-position check rejected any For whose first child
	// was the condition, which includes this shape too). sum accumulates
	// 0+1+2 = 3.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; for ; i < 3; i = i + 1 { sum = sum + i; } return sum; }", false, 3, false)
}

func TestEmitForLoopBreakAndContinueCompilesAndRuns(t *testing.T) {
	// break/continue inside a for-loop body work through the same
	// buildLoopJump machinery a while/range body uses: a Break's/Continue's
	// Target names the For's own Region, and plain C break/continue already
	// target the nearest enclosing loop, which the emitted for loop is. step
	// counts 0..9, breaking when step == 5 and skipping the accumulation when
	// step == 3, so total = 0+1+2+4 = 7, returned as the exit code. Bounded
	// execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 10; step = step + 1 { if step == 5 { break; } if step == 3 { continue; } total = total + step; } return total; }", false, 7, false)
}

func TestEmitForLoopNestedInWhileCompilesAndRuns(t *testing.T) {
	// A classic for loop nested inside a while loop's body: the For is a
	// statement in the while's body Block, dispatched by buildLoopBody. Each
	// of the 2 while iterations runs the for loop's 3 inner iterations, so
	// total = 6, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 2 { for var j i32 = 0; j < 3; j = j + 1 { total = total + 1; } i = i + 1; } return total; }", false, 6, false)
}

func TestEmitForLoopNestedInRangeLoopCompilesAndRuns(t *testing.T) {
	// A classic for loop nested inside a range loop's body: the For is a
	// statement in the range loop's body Block, dispatched by buildLoopBody.
	// Each of the 3 range iterations runs the for loop's 2 inner iterations,
	// so total = 6, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; loop 0..3 : i { for var j i32 = 0; j < 2; j = j + 1 { total = total + 1; } } return total; }", false, 6, false)
}

func TestEmitForLoopNestedInForLoopCompilesAndRuns(t *testing.T) {
	// A classic for loop nested inside another classic for loop: the inner For
	// is a statement in the outer for's body Block, dispatched by
	// buildLoopBody exactly like a nested While or RangeLoop. i and j each
	// count 0..3, so the inner body runs 3 x 3 = 9 times and total = 9,
	// returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0; i < 3; i = i + 1 { for var j i32 = 0; j < 3; j = j + 1 { total = total + 1; } } return total; }", false, 9, false)
}

func TestEmitForLoopI64EntryCompilesAndRuns(t *testing.T) {
	// A classic for loop inside an i64 entry: the initializer's C type follows
	// the entry's width (int64_t). total accumulates 0+1+2 = 3, returned as
	// the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i64 { var total i64 = 0; for var step i64 = 0; step < 3; step = step + 1 { total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopBoolInitializerAndConditionCompilesAndRuns(t *testing.T) {
	// The bool grammar works in every for-loop clause: the initializer
	// declares a bool local (built by buildScalarInitializeCore's bool case),
	// the condition is a bare bool value (buildCondition -> buildBoolExpr), and
	// the update reassigns the bool (buildStoreCore's bool case). The loop
	// runs once (first is true), accumulates 1, flips first to false, and the
	// next condition check stops it, so total = 1. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var first bool = true; first; first = false { total = total + 1; } return total; }", false, 1, false)
}

func TestEmitForLoopLogicalConditionCompilesAndRuns(t *testing.T) {
	// A && condition in a for header goes through buildCondition ->
	// buildBoolExpr -> buildComparison exactly as an if/while condition does.
	// i counts 0..3 under the && condition, so total = 0+1+2 = 3. Bounded
	// execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0; i < 3 && i >= 0; i = i + 1 { total = total + i; } return total; }", false, 3, false)
}

func TestEmitForLoopInHelperFunctionCompilesAndRuns(t *testing.T) {
	// buildHelperFunctions builds each reachable helper's body with the same
	// buildBlock the entry uses, so a classic for loop works inside a helper
	// unchanged. sumTo(4) = 0+1+2+3 = 6, returned as the exit code. Bounded
	// execution.
	emitAndRunBounded(t, "fn sumTo(n i32) i32 { var total i32 = 0; for var step i32 = 0; step < n; step = step + 1 { total = total + step; } return total; } fn main() i32 { return sumTo(4); }", false, 6, false)
}

func TestEmitForLoopHelperCallInConditionCompilesAndRuns(t *testing.T) {
	// A helper call inside a for clause: the DirectCall is discovered by the
	// reachability walk (which follows For.Children generically) and built by
	// buildComparison/buildExpr like any other call. ten() returns 10, so the
	// loop counts 0..9 and total = 0+1+...+9 = 45. Bounded execution.
	emitAndRunBounded(t, "fn ten() i32 { return 10; } fn main() i32 { var total i32 = 0; for var step i32 = 0; step < ten(); step = step + 1 { total = total + step; } return total; }", false, 45, false)
}

func TestEmitForLoopWritesC(t *testing.T) {
	// The emitted C for the flagship fixture must be a C for loop whose
	// init/condition/update clauses match the source exactly: the initializer
	// declares the loop local as `<entry ctype> pebble_local_<symbol>` in the
	// header (no statement newline/indent, the for-header's first `;`
	// terminating the init clause), the condition is the plain comparison, and
	// the update is a bare assignment expression with NO trailing `;` (the for
	// statement supplies it). The initializer local gets its
	// -Wunused-variable (void) cast as the body's first statement, since its C
	// declaration lives in the header where a cast cannot go. Symbols 25
	// (total) and 26 (step) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; step = step + 1 { total = total + step; } return total; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    for (int32_t pebble_local_26 = 0; pebble_local_26 < 3; pebble_local_26 = pebble_rt_checked_add_i32(pebble_local_26, 1, (PebbleSourceLoc){\"main.peb\", 1, 75})) {\n",
		"        (void)pebble_local_26;",
		"        pebble_local_25 = pebble_rt_checked_add_i32(pebble_local_25, pebble_local_26, (PebbleSourceLoc){\"main.peb\", 1, 94});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitForLoopRejectsStoreInitializer(t *testing.T) {
	// An assignment as the for-loop initializer (for step = 0; ...) is
	// reachable from real source but out of scope: the initializer must be a
	// single local declaration, matching the backend's rule that only an
	// Initialize declares a local.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var step i32 = 0; for step = 0; step < 3; step = step + 1 { } return step; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "for loop initializer is a Store")
}

func TestEmitForLoopRejectsCompoundStoreInitializer(t *testing.T) {
	// A compound-assignment as the for-loop initializer (for x += 1; ...) is
	// reachable from real source but out of scope: the initializer must be a
	// single local declaration.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var x i32 = 0; for x += 1; x < 3; { } return x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "for loop initializer is a CompoundStore")
}

func TestEmitForLoopRejectsCompoundStoreUpdate(t *testing.T) {
	// A compound-assignment as the for-loop update (step += 1) is reachable
	// from real source but out of scope: the update must be a single Store
	// (a reassignment), matching the backend's rule that a reassignment
	// lowers through buildStoreCore.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; step += 1 { total = total + step; } return total; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "for loop update is a CompoundStore")
}

func TestEmitForLoopRejectsExpressionStatementClause(t *testing.T) {
	// A discarded-expression clause (for x + 1; ... or for ; ; x + 1 ...) is
	// reachable from real source but out of scope: a no-condition for whose
	// only clause is an ExpressionStatement is ambiguous (expression
	// initializer or expression update — both out of scope, so the rejection
	// is unambiguous in outcome).
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var x i32 = 0; for x + 1; ; { break; } return x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "with no condition has a ExpressionStatement clause")
	unit, snapshot, entryID, _ = buildFixture(t, "fn main() i32 { var x i32 = 0; for ; ; x + 1 { break; } return x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "with no condition has a ExpressionStatement clause")
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

func TestEmitI64ReturnEntryWritesC(t *testing.T) {
	// Mirror of TestEmitIntegerReturnEntryWritesC at the wider width: the
	// pebble_user_main adapter must be declared with the 64-bit return type so
	// a wide return value is not truncated, not the i32 entry's "int".
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { return 42; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { return 1 + 2; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_rt_checked_add_i64(1, 2, (PebbleSourceLoc){\"main.peb\", 1, 24})",
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { var i i64 = 0; var sum i64 = 0; while i < 5 { sum = sum + i; i = i + 1; } return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int64_t pebble_local_25 = 0;",
		"int64_t pebble_local_26 = 0;",
		"    while (pebble_local_25 < 5) {\n",
		"        pebble_local_26 = pebble_rt_checked_add_i64(pebble_local_26, pebble_local_25, (PebbleSourceLoc){\"main.peb\", 1, 69});",
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
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i64 { let x i32 = 1; return 2; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i64")
}

func TestEmitI32RejectsI64Local(t *testing.T) {
	// The reverse direction: an i64 local inside an i32 entry is likewise a
	// legal program the checker builds and a clean width-mismatch rejection
	// for this backend.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { let x i64 = 1; return 2; }", "main", false)
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper() i32 { return 21; } fn main() i32 { return helper() + helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx) {",
		"    return 21;",
		"static int pebble_user_main(PebbleContext *ctx)",
		"return pebble_rt_checked_add_i32(pebble_fn_24(ctx), pebble_fn_24(ctx), (PebbleSourceLoc){\"main.peb\", 1, 55});",
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper() i32 { var done bool = false; var sum i32 = 0; var i i32 = 0; while !done { sum = sum + i; i = i + 1; if i == 5 { done = true; } } if sum > 3 { return sum; } else { return sum + 1; } } fn main() i32 { return helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper1() i32 { return helper2(); } fn helper2() i32 { return 20; } fn main() i32 { return helper1(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper() i64 { return 21; } fn main() i64 { return helper() + helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int64_t pebble_fn_24(PebbleContext *ctx) {",
		"pebble_rt_checked_add_i64(pebble_fn_24(ctx), pebble_fn_24(ctx), (PebbleSourceLoc){\"main.peb\", 1, 55});",
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
	unit, snapshot, entryID, _ := buildFixture(t, "fn helper() i32 { return 21; } fn main() i64 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i64")
}

func TestEmitRejectsI32MainCallsI64Helper(t *testing.T) {
	// The reverse direction: an i32 entry calling an i64 helper is likewise a
	// clean width-mismatch rejection.
	unit, snapshot, entryID, _ := buildFixture(t, "fn helper() i64 { return 21; } fn main() i32 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32")
}

func TestEmitRejectsSelfRecursion(t *testing.T) {
	// Recursion is legal, checker-accepted Pebble (confirmed against a real
	// fixture), so this is a genuine backend-scope boundary: the reachability
	// walk follows helper's call back to helper and must reject the cycle
	// cleanly, naming the chain, rather than emit a C definition that calls
	// itself before it is defined (there's no forward-declaration mechanism).
	unit, snapshot, entryID, _ := buildFixture(t, "fn helper() i32 { return helper(); } fn main() i32 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "recursion is not supported")
}

func TestEmitRejectsMutualRecursion(t *testing.T) {
	// Two functions that call each other: a calls b and b calls a, so a can
	// reach itself through b. The walk must reject the cycle naming the chain
	// (symbol 24 -> symbol 25 -> symbol 24), not emit either function.
	unit, snapshot, entryID, _ := buildFixture(t, "fn a() i32 { return b(); } fn b() i32 { return a(); } fn main() i32 { return a(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "recursion is not supported")
}

func TestEmitRejectsEntryReachedByHelperCycle(t *testing.T) {
	// The cycle can close through the entry itself: main calls helper, helper
	// calls main back. main is on the walk's DFS path, so the walk must reject
	// the cycle rather than re-emit the entry as a helper.
	unit, snapshot, entryID, _ := buildFixture(t, "fn helper() i32 { return main(); } fn main() i32 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "recursion is not supported")
}

func TestEmitVoidHelperStatementCompilesAndRuns(t *testing.T) {
	// The flagship 10.33 shape: a void-returning helper called purely for its
	// side effect as a bare discarded-expression statement (helper(); on its
	// own line, a tir.ExpressionStatement wrapping a tir.DirectCall to a void
	// callee). This backend has no mutable-reference parameters or globals, so
	// a void helper cannot observe any effect outside itself; the observable
	// contract is that the call compiles and runs without error and the exit
	// code still reflects the caller's own subsequent logic (here return 1).
	emitAndRun(t, "fn helper() void {} fn main() i32 { helper(); return 1; }", false, 1, false)
}

func TestEmitVoidHelperStatementWithParamCompilesAndRuns(t *testing.T) {
	// A void helper with a parameter and a non-trivial self-contained body: it
	// computes internally (sum 0..x into a local) and returns void, so the
	// call's arguments are built by the same buildCallArguments machinery a
	// value-context call uses, and the exit code (7) reflects only the
	// caller's own logic, proving the call statement did not disturb it.
	emitAndRunBounded(t, "fn helper(x i32) void { var acc i32 = 0; var i i32 = 0; while i < x { acc = acc + i; i = i + 1; } } fn main() i32 { helper(4); return 7; }", false, 7, false)
}

func TestEmitVoidCallInLoopBodyCompilesAndRuns(t *testing.T) {
	// A void call as a statement inside a loop body: the ExpressionStatement
	// is a plain child of the loop body's Block, flowing through buildLoopBody's
	// statement switch (via the shared buildLeadingStatement) alongside the
	// accumulation Store, so the call executes on every iteration without
	// disrupting the loop's own logic. x = 0+1+2 = 3, the loop's own result.
	emitAndRunBounded(t, "fn helper() void {} fn main() i32 { var x i32 = 0; var i i32 = 0; while i < 3 { helper(); x = x + i; i = i + 1; } return x; }", false, 3, false)
}

func TestEmitVoidHelperCallingVoidHelperCompilesAndRuns(t *testing.T) {
	// A void helper whose own body is a void call statement plus its
	// ImplicitReturn tail: helper() calls inner(5) as a statement, then falls
	// off the end of its body. The reachability walk follows the nested call
	// and emits both helpers; the caller exits 3 on its own logic.
	emitAndRun(t, "fn inner(x i32) void { } fn helper() void { inner(5); } fn main() i32 { helper(); return 3; }", false, 3, false)
}

func TestEmitVoidCallInI64EntryCompilesAndRuns(t *testing.T) {
	// A void call statement reached from an i64 entry: the void helper is
	// emitted with the C return type void regardless of the entry's width, and
	// the call compiles and runs to the caller's own exit code.
	emitAndRun(t, "fn helper() void {} fn main() i64 { helper(); return 1; }", false, 1, false)
}

func TestEmitVoidHelperWritesC(t *testing.T) {
	// Confirm the emitted C for the flagship shape: the void helper is declared
	// with the C return type void (pebble_fn_<symbolID>, symbol 24 for this
	// fixture), and the call statement appears as a bare
	// `pebble_fn_24(ctx);` — a statement, not a value expression — at the
	// call's own position in program order.
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper() void {} fn main() i32 { helper(); return 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "static void pebble_fn_24(PebbleContext *ctx)") {
		t.Errorf("emitted C does not declare the void helper with the C return type void:\n%s", out)
	}
	if !strings.Contains(out, "    pebble_fn_24(ctx);") {
		t.Errorf("emitted C is missing the bare void-call statement:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 1, false)
}

func TestEmitRejectsNonVoidDiscardedCallStatement(t *testing.T) {
	// A call to a non-void-returning function used purely as a statement
	// (`f();` where f returns i32, the result silently discarded) IS reachable
	// from real source — the checker's C0612 rejects only a discarded
	// expression statement whose value is a non-void NON-call expression, and
	// deliberately permits a discarded call — so the backend must reject it
	// cleanly, naming the callee and its result type, rather than guessing
	// how a discarded non-void result would be dropped (a real future gap,
	// out of this slice's void-only scope).
	unit, snapshot, entryID, _ := buildFixture(t, "fn f() i32 { return 5; } fn main() i32 { f(); return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "result type is i32, want a call to a void-returning function")
}

func TestEmitUnreachableFunctionNotEmitted(t *testing.T) {
	// A declared function the entry never calls, directly or transitively, must
	// not be emitted at all — the generated C has no trace of it (symbol 25,
	// the unused function), so the -Wall -Wextra -Werror build cannot warn
	// about an unused static function. Only the reachable helper (symbol 24)
	// is emitted, and the program runs to exit 21.
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper() i32 { return 21; } fn unused() i32 { return 99; } fn main() i32 { return helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { return add(20, 22); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx, int32_t pebble_local_25, int32_t pebble_local_26) {",
		"    (void)pebble_local_25;",
		"    (void)pebble_local_26;",
		"    return pebble_rt_checked_add_i32(pebble_local_25, pebble_local_26, (PebbleSourceLoc){\"main.peb\", 1, 35});",
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn choose(flag bool, x i32, y i32) i32 { if flag { return x; } else { return y; } } fn main() i32 { return choose(true, 10, 20); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	// An array parameter is reachable from real source (the checker accepts
	// it), so this is a genuine backend-scope rejection, not hand-built IR:
	// validateHelperSignature must reject the parameter because its type is
	// neither the entry's width, bool, char, str, nor a tuple/struct type,
	// naming the parameter position.
	unit, snapshot, entryID, _ := buildFixture(t, "fn f(a [3]i32) i32 { return 1; } fn main() i32 { return f([10, 20, 30]); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32, bool, char, or str")
}

func TestEmitRejectsParameterWidthMismatch(t *testing.T) {
	// A parameter of the other integer width follows the same width-consistency
	// rule 10.13 established for locals: an i64 parameter in an i32 entry (and
	// its result, here also i64) must be a clean rejection naming the width,
	// never a coercion. The parameter check fires before the result check.
	unit, snapshot, entryID, _ := buildFixture(t, "fn f(a i64) i64 { return 0; } fn main() i32 { return f(0); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32, bool, char, or str")
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let t (i32, i32, i32) = (10, 20, 30); return t.1 + t.2; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n    int32_t _2;\n} pebble_tuple_23_t;",
		"pebble_tuple_23_t pebble_local_25 = { 10, 20, 30 };",
		"    (void)pebble_local_25;",
		"return pebble_rt_checked_add_i32(pebble_local_25._1, pebble_local_25._2, (PebbleSourceLoc){\"main.peb\", 1, 62});",
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let t (i32, bool) = (1, true); if t.1 { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let t (i32, i32) = (20, 22); return add(t.1, t.1); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { let t (i64, i64) = (20, 22); return t.1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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

func TestEmitIntEntryArrayElementReadCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() int { let a [3]int = [10, 20, 30]; return a[1]; }", true, 20, false)
}

func TestEmitIntEntryArrayOutOfBoundsAborts(t *testing.T) {
	emitAndRun(t, "fn main() int { let a [2]int = [10, 20]; let i int = 2; return a[i]; }", true, 0, true)
}

func TestEmitCheckedArrayIndexEmitsRealSourceLoc(t *testing.T) {
	// Since 10.44, checked array indexing carries a real, resolved Pebble
	// source location (the CheckedIndexPlace node's own Span) as its final
	// argument, not the zero-valued placeholder 10.43 deliberately left here
	// — this is one of the three checked-call categories 10.44 finished. The
	// fixture must still compile and run correctly end to end.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; return a[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, `pebble_rt_checked_index_i32(0, 3, (PebbleSourceLoc){"main.peb", 1,`) {
		t.Errorf("emitted C lacks a real source location on the checked-index call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 10, false)
}

func TestEmitArrayElementAsCallArgument(t *testing.T) {
	emitAndRun(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let a [2]i32 = [20, 22]; return add(a[0], a[1]); }", false, 42, false)
}

func TestEmitI64ArrayCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i64 { let a [2]i64 = [20, 22]; return a[1]; }", false, 22, false)
}

func TestEmitArrayWritesC(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; return a[1]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_25[3] = { 10, 20, 30 };",
		"return pebble_local_25[pebble_rt_checked_index_i32(1, 3, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitArrayRepeatSumCompilesAndRuns(t *testing.T) {
	// The flagship ArrayRepeat fixture (10.27): [5; 3] initializes all three
	// slots from a single evaluation of 5, so the sum of every element read is
	// 5 + 5 + 5 = 15. This is the end-to-end confirmation that the
	// three-statement emission (bare declaration, one-time repeat temp, fill
	// loop) produces a correct array that reads back exactly like 10.20's
	// ArrayValue arrays.
	emitAndRun(t, "fn main() i32 { let a [3]i32 = [5; 3]; return a[0] + a[1] + a[2]; }", false, 15, false)
}

func TestEmitArrayRepeatExprValueCompilesAndRuns(t *testing.T) {
	// A repeat value that is itself a non-trivial expression: x * 5 references
	// an earlier local through checked arithmetic, so the single evaluation is
	// pebble_rt_checked_mul_i32(pebble_local_<x>, 5) = 10, and all three slots
	// get that one value: 10 + 10 + 10 = 30. Proves the repeat value is built
	// through buildExpr (a local reference composing with checked arithmetic),
	// not just a bare literal.
	emitAndRun(t, "fn main() i32 { let x i32 = 2; let a [3]i32 = [x * 5; 3]; return a[0] + a[1] + a[2]; }", false, 30, false)
}

func TestEmitArrayRepeatBoolElementDrivesIfCompilesAndRuns(t *testing.T) {
	// A bool-element array repeat: [true; 2] fills both bool slots from one
	// evaluation of true, and the element read drives an if condition through
	// the existing Load(CheckedIndexPlace) bool path. a[1] is true, so the
	// then-arm runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let a [2]bool = [true; 2]; if a[1] { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitArrayRepeatI64CompilesAndRuns(t *testing.T) {
	// The width discipline extends to array repeat element types: an i64
	// entry's [2]i64 repeat fills both int64_t slots, the reads lower through
	// pebble_rt_checked_index_i64, and 7 + 7 = 14 is the process exit code.
	emitAndRun(t, "fn main() i64 { let a [2]i64 = [7; 2]; return a[0] + a[1]; }", false, 14, false)
}

func TestEmitArrayRepeatWritesC(t *testing.T) {
	// The emitted C for the flagship fixture: an ArrayRepeat-initialized local
	// is three C statements instead of one declaration line — the array's own
	// bare declaration, a synthetic temp (pebble_repeat_<symbolID>) holding
	// the one-time-evaluated repeat value, and a for loop over a size_t
	// counter (pebble_i_<symbolID>) filling every slot from the temp — still
	// followed by the (void) cast every array local gets. Both synthetic names
	// derive from the local's own symbol (25, confirmed against the real
	// fixture dump). The element reads are unchanged from 10.20.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let a [3]i32 = [5; 3]; return a[0] + a[1] + a[2]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    int32_t pebble_local_25[3];\n    int32_t pebble_repeat_25 = 5;\n    for (size_t pebble_i_25 = 0; pebble_i_25 < 3; pebble_i_25++) {\n        pebble_local_25[pebble_i_25] = pebble_repeat_25;\n    }\n    (void)pebble_local_25;",
		"pebble_rt_checked_index_i32(0, 3, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitArrayRepeatSingleEvaluationWritesC(t *testing.T) {
	// The single-evaluation proof: a repeat value that is a call to a helper
	// function. A naive brace-list duplication ({ v, v, v }) would have
	// emitted pebble_fn_<five>(ctx) three times — once per slot — evaluating
	// the call three times. This backend's one-time temp emission must contain
	// the call expression exactly once (in the pebble_repeat_<sym> = ... line),
	// so the call is evaluated exactly once at runtime. The assertion is
	// structural (strings.Count over the emitted C), the strongest proof
	// available without mutable global state to observe call count; the
	// end-to-end run confirms the resulting values are still correct
	// (5 + 5 + 5 = 15).
	unit, snapshot, entryID, sources := buildFixture(t, "fn five() i32 { return 5; } fn main() i32 { let a [3]i32 = [five(); 3]; return a[0] + a[1] + a[2]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// five's own body is `return 5;` (no call), so the only
	// pebble_fn_24(ctx) call site in the whole emitted file is the repeat
	// temp initializer — exactly one.
	if got := strings.Count(out, "pebble_fn_24(ctx)"); got != 1 {
		t.Errorf("pebble_fn_24(ctx) appears %d time(s) in the emitted C, want exactly 1 (the repeat value must be evaluated once, not once per slot):\n%s", got, out)
	}
	if !strings.Contains(out, "int32_t pebble_repeat_26 = pebble_fn_24(ctx);") {
		t.Errorf("emitted C missing the one-time repeat temp initialized from the call:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 15, false)
}

func TestEmitRejectsTupleWithUnsupportedElementType(t *testing.T) {
	// A tuple whose element type is neither the entry's width nor bool — here
	// a str element — is reachable from real source (the checker builds the
	// declaration fine), so this is a genuine backend-scope rejection. The
	// tuple typedef pass inspects the element types first and rejects the str
	// field with a clear error naming the wanted types, so no C is written.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { let t (i32, str) = (1, \"hi\"); return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}

func TestEmitNestedTupleElementCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let inner (i32, i32) = (20, 22); let outer ((i32, i32), bool) = (inner, true); return (outer.0).1; }", false, 22, false)
}

func TestEmitRejectsTupleNestedMoreThanOneLevel(t *testing.T) {
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { let a (i32, i32) = (1, 2); let b ((i32, i32), i32) = (a, 3); let c (((i32, i32), i32), i32) = (b, 4); return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "unsupported")
}

func TestEmitRejectsWholeTupleStore(t *testing.T) {
	// Reassigning a whole tuple-typed local (t = (3, 4)) is reachable from
	// real source (the checker builds the Store fine) but is out of scope this
	// slice: only element reads of a tuple local are supported, never
	// assignment into or reassignment of one. The Store's place names a
	// tuple-typed local, so buildLeadingStatement rejects it with a clear
	// error naming the reassignment, not a guessed lowering.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var t (i32, i32) = (1, 2); t = (3, 4); return 1; }", "main", false)
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
	unit, snapshot, entryID, _ := buildFixture(t, "fn f(t (i32, i32)) i32 { return t.1; } fn main() i32 { return f(((1, 2))); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want a reference to a tuple-typed local in scope or a tuple literal")

	unit, snapshot, entryID, _ = buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x; } fn main() i32 { return f((Point.{ x = 1, y = 2 })); }", "main", false)
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
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { let x i32 = (1, 2).1; return x; }", "main", false)
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn sumT(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { let t (i32, i32) = (20, 22); return sumT(t); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n} pebble_tuple_23_t;",
		"static int32_t pebble_fn_24(PebbleContext *ctx, pebble_tuple_23_t pebble_local_25) {",
		"    (void)pebble_local_25;",
		"    return pebble_rt_checked_add_i32(pebble_local_25._0, pebble_local_25._1, (PebbleSourceLoc){\"main.peb\", 1, 36});",
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
	unit, snapshot, entryID, _ := buildFixture(t, "fn sumT(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { return 0; }", "main", false)
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

func TestEmitOptionalUnwrapNoneEmitsRealSourceLoc(t *testing.T) {
	// The absent-optional unwrap still panics (the process aborts via
	// pebble_rt_checked_unwrap_i32), and — new for this slice — the emitted
	// unwrap call now carries the CheckedOptionalUnwrap node's own resolved
	// Pebble source location as its final argument, not the zero-valued
	// placeholder. The emitted C is inspected for the non-placeholder
	// compound literal, and the compiled binary is run to confirm the unwrap
	// of `none` still aborts.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let x ?i32 = none; return x!; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, `pebble_rt_checked_unwrap_i32(pebble_local_25.has_value, pebble_local_25.value, (PebbleSourceLoc){"main.peb", 1, `) {
		t.Errorf("emitted C lacks a real source location on the checked-unwrap call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let x ?i32 = some 42; return x!; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { let x ?i64 = some 22; return x!; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let x ?bool = some true; if x! { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var x ?i32 = some 1; x = some 2; return x!; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "reassigning an optional is not supported")
}

func TestEmitRejectsOptionalWithUnsupportedPayloadType(t *testing.T) {
	// An optional whose payload type is neither the entry's width nor bool —
	// here a str payload — is reachable from real source (the checker builds
	// the declaration fine), so this is a genuine backend-scope rejection. The
	// optional typedef pass inspects the payload type first and rejects the
	// str field with a clear error naming the wanted types, so no C is written.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { let x ?str = some \"hi\"; return 1; }", "main", false)
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

func TestEmitStructContainingTupleCompilesAndRuns(t *testing.T) {
	src := "type HasTuple = struct { t (i32, i32); x i32; }; fn main() i32 { let t (i32, i32) = (20, 22); let h HasTuple = HasTuple.{ t = t, x = 1 }; return h.t.1; }"
	emitAndRun(t, src, false, 22, false)
}

func TestEmitTupleContainingStructCompilesAndRuns(t *testing.T) {
	src := "type Point = struct { x i32; y i32; }; fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; let t (Point, i32) = (p, 1); return t.0.y; }"
	emitAndRun(t, src, false, 22, false)
}

func TestEmitArrayOfStructsCompilesAndRuns(t *testing.T) {
	src := "type Point = struct { x i32; y i32; }; fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; let a [2]Point = [p, p]; return a[1].x + a[1].y; }"
	emitAndRun(t, src, false, 42, false)
}

func TestEmitStructContainingOptionalCompilesAndRuns(t *testing.T) {
	src := "type HasOpt = struct { o ?i32; x i32; }; fn main() i32 { let o ?i32 = some 42; let h HasOpt = HasOpt.{ o = o, x = 1 }; return h.o!; }"
	emitAndRun(t, src, false, 42, false)
}

func TestEmitNestedTypedefOrderWritesAndCompiles(t *testing.T) {
	src := "type Point = struct { x i32; y i32; }; fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; let t (Point, i32) = (p, 1); return t.0.x + t.0.y; }"
	unit, snapshot, entryID, sources := buildFixture(t, src, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatal(err)
	}
	out := buf.String()
	inner := strings.Index(out, "pebble_struct_")
	outer := strings.Index(out, "pebble_tuple_")
	if inner < 0 || outer < 0 || inner > outer {
		t.Fatalf("nested typedef dependency order is wrong:\n%s", out)
	}
	if !strings.Contains(out, ".pebble_field_") || !strings.Contains(out, "._0") {
		t.Fatalf("nested access chain missing:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
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
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { let point Point = Point.{ y = 2, x = 1 }; return point.x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "type Pair = struct { a i32; b bool; };\nfn main() i32 { let p Pair = Pair.{ a = 1, b = true }; if p.b { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return add(p.x, p.y); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "type T = struct { a i64; b i64; };\nfn main() i64 { let t T = T.{ a = 20, b = 22 }; return t.b; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, _ := buildFixture(t, "type S = struct { s str; };\nfn main() i32 { let x S = S.{ s = \"hi\" }; return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32 or bool")
}

func TestEmitRejectsStructWholeReassignment(t *testing.T) {
	// Reassigning a whole struct-typed local (p = Point.{ ... }) is out of
	// scope this slice. The Store's place names a struct-typed local, so
	// buildLeadingStatement rejects it with a clear error naming the
	// reassignment, not a guessed lowering.
	unit, snapshot, entryID, _ := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { var p Point = Point.{ x = 1, y = 2 }; p = Point.{ x = 3, y = 4 }; return p.x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "reassigning a whole struct is not supported")
}

func TestEmitRejectsStructFieldAssignment(t *testing.T) {
	// FieldPlace stores lower through the same lvalue machinery as pointer and
	// indexed writes, preserving the mutation for the following read.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { var point Point = Point.{ x = 1, y = 2 }; point.x = 5; return point.x; }", false, 5, false)
}

func TestEmitRejectsStructFieldReadOffLiteral(t *testing.T) {
	// Reading a field directly off a struct literal (Point.{ x = 1, y = 2 }.x)
	// is reachable from real source but lowers to a FieldValue whose base is
	// the RecordConstruct itself, not a StoragePlace naming a struct local — a
	// value-category shape out of scope this slice (only Load(FieldPlace) of a
	// struct local is supported). The integer expression builder rejects the
	// FieldValue cleanly rather than guessing.
	unit, snapshot, entryID, _ := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { return Point.{ x = 1, y = 2 }.x; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNestedStructFieldAccess(t *testing.T) {
	// Nested field access (o.inner.x, where inner is itself a struct-typed
	// field) is reachable from real source but out of scope twice over: the
	// struct-of-struct field type is itself rejected by the typedef pass first
	// (a struct field must be the entry's width or bool), so the program is a
	// clean rejection naming the unsupported field type before the nested read
	// (a FieldPlace whose base is another FieldPlace) is even reached.
	emitAndRun(t, "type Inner = struct { x i32; };\ntype Outer = struct { inner Inner; y i32; };\nfn main() i32 { let i Inner = Inner.{ x = 7 }; let o Outer = Outer.{ inner = i, y = 8 }; return o.inner.x; }", false, 7, false)
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn f(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { return f((20, 22)); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "return pebble_fn_24(ctx, (pebble_tuple_23_t){ 20, 22 });") {
		t.Errorf("emitted C missing the tuple compound-literal argument:\n%s", out)
	}

	unit, snapshot, entryID, sources = buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return f(Point.{ y = 22, x = 20 }); }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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

func TestEmitPointerStructFieldReadWriteCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type P = struct { cap i32; }; fn mutate(p *P) void { p.cap = 9; } fn main() i32 { var p P = P.{ cap = 1 }; let pointer *P = &p; mutate(pointer); return p.cap; }", false, 9, false)
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
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return f(p); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"static int32_t pebble_fn_27(PebbleContext *ctx, pebble_struct_23_t pebble_local_28) {",
		"    (void)pebble_local_28;",
		"    return pebble_rt_checked_add_i32(pebble_local_28.pebble_field_25, pebble_local_28.pebble_field_26, (PebbleSourceLoc){\"main.peb\", 2, 28});",
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
	unit, snapshot, entryID, _ := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return 0; }", "main", false)
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"a\\n1\\tb\\\"c\\\\d\"; if s == \"a\\x0a1\\x09b\\x22c\\x5cd\" { return 7; } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; if s == \"hi\" { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s != t { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
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

// 10.40 — ordering comparisons between str values

func TestEmitStrOrderingLessCompilesAndRuns(t *testing.T) {
	// s < t where "hi" < "ho" is true (lexicographic byte comparison: 'i' <
	// 'o'), so the then-arm runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s < t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingLessFalseCompilesAndRuns(t *testing.T) {
	// s < t where "ho" < "hi" is false ('o' > 'i'), so the else-arm runs
	// and the process exits 20.
	emitAndRun(t, "fn main() i32 { let s str = \"ho\"; let t str = \"hi\"; if s < t { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrOrderingLessEqualCompilesAndRuns(t *testing.T) {
	// s <= t where "hi" <= "hi" is true (equal counts), so the then-arm
	// runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; if s <= t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingLessEqualFalseCompilesAndRuns(t *testing.T) {
	// s <= t where "hi" <= "ha" is false ('i' > 'a'), so the else-arm runs
	// and the process exits 20.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ha\"; if s <= t { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrOrderingGreaterCompilesAndRuns(t *testing.T) {
	// s > t where "ho" > "hi" is true ('o' > 'i'), so the then-arm runs
	// and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"ho\"; let t str = \"hi\"; if s > t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingGreaterFalseCompilesAndRuns(t *testing.T) {
	// s > t where "hi" > "ho" is false ('i' < 'o'), so the else-arm runs
	// and the process exits 20.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s > t { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrOrderingGreaterEqualCompilesAndRuns(t *testing.T) {
	// s >= t where "hi" >= "hi" is true (equal counts), so the then-arm
	// runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; if s >= t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingGreaterEqualFalseCompilesAndRuns(t *testing.T) {
	// s >= t where "ha" >= "hi" is false ('a' < 'i'), so the else-arm runs
	// and the process exits 20.
	emitAndRun(t, "fn main() i32 { let s str = \"ha\"; let t str = \"hi\"; if s >= t { return 10; } else { return 20; } }", false, 20, false)
}

func TestEmitStrOrderingLiteralOperandCompilesAndRuns(t *testing.T) {
	// An ordering comparison where one operand is a string literal directly
	// (not a local), confirming buildStrOperand's existing literal path works
	// unchanged in this new position. "hi" < "ho" is true, so the
	// then-arm runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; if s < \"ho\" { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingPrefixTieBreakCompilesAndRuns(t *testing.T) {
	// Two strings that share a prefix but differ in length: "hi" vs "hi!".
	// The shorter string must sort first (matching strcmp's convention for a
	// prefix — the shorter one is "less"), so "hi" < "hi!" is true and the
	// then-arm runs, exiting 10. This proves the shorter-string-sorts-first
	// tie-break behaves correctly at runtime.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi!\"; if s < t { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitStrOrderingWritesC(t *testing.T) {
	// The emitted C for an ordering comparison between two str locals: the
	// comparison must use pebble_rt_str_cmp with the source operator
	// translated to its C spelling, compared against 0. The ==/!= path is
	// still pebble_rt_str_eq-based (verified below). Symbols 25/26 are the
	// s and t locals, confirmed against the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let t str = \"ho\"; if s < t { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_25 = { .data = (const uint8_t *)\"hi\", .len = 2 };",
		"PebbleStr pebble_local_26 = { .data = (const uint8_t *)\"ho\", .len = 2 };",
		"if (pebble_rt_str_cmp(pebble_local_25, pebble_local_26) < 0) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrEqualityStillUsesStrEqWritesC(t *testing.T) {
	// Regression check: the ==/!= path must still use pebble_rt_str_eq,
	// not pebble_rt_str_cmp. This confirms this slice didn't disturb the
	// existing equality lowering.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let t str = \"hi\"; if s == t { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "if (pebble_rt_str_eq(pebble_local_25, pebble_local_26)) {") {
		t.Errorf("expected pebble_rt_str_eq for ==, got:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_str_cmp") {
		t.Errorf("== path must not use pebble_rt_str_cmp:\n%s", out)
	}
}

// 10.36 — str reassignment and str-typed parameters/results

func TestEmitStrReassignmentCompilesAndRuns(t *testing.T) {
	// Reassigning a str-typed local from a new string literal, the effect
	// observed indirectly (this backend has no way to return or print a str's
	// contents, so the reassignment's effect must be proven through a later
	// comparison): s starts as "hi", is reassigned to "ho", and the subsequent
	// comparisons prove the stored value actually changed — s == "ho" (the new
	// literal) is true and s != "hi" (the old literal) is true — so the
	// then-arm runs and the process exits 7. If the reassignment were a no-op
	// the stored value would still be "hi" and the else-arm would exit 3.
	emitAndRun(t, "fn main() i32 { var s str = \"hi\"; s = \"ho\"; if s != \"hi\" && s == \"ho\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReassignmentEscapedLiteralCompilesAndRuns(t *testing.T) {
	// The escaping correctness must survive a reassignment, not just a
	// declaration: reassigning from a literal whose decoded content forces C
	// escapes (newline, tab, quote, backslash, control byte), then comparing
	// against a differently-spelled literal that decodes to the same bytes.
	// If the reassignment's escaped C text were wrong the equality would fail
	// and the else-arm would exit 3 instead of 7.
	emitAndRun(t, "fn main() i32 { var s str = \"x\"; s = \"a\\n1\\tb\\\"c\\\\d\"; if s == \"a\\x0a1\\x09b\\x22c\\x5cd\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReassignmentWritesC(t *testing.T) {
	// The emitted C for a str local reassigned from a string literal: the
	// reassignment is a whole-struct PebbleStr assignment whose inner
	// construction text is byte-identical to the declaration's from the same
	// literal — `pebble_local_<sym> = (PebbleStr){ .data = (const uint8_t *)
	// "ho", .len = 2 };` — the (PebbleStr) compound-literal cast being what
	// makes the shared brace text a valid C assignment expression. Symbol 25 is
	// the s local, confirmed against the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var s str = \"hi\"; s = \"ho\"; if s != \"hi\" && s == \"ho\" { return 7; } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_25 = { .data = (const uint8_t *)\"hi\", .len = 2 };",
		"pebble_local_25 = (PebbleStr){ .data = (const uint8_t *)\"ho\", .len = 2 };",
		"    (void)pebble_local_25;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitRejectsStrReassignmentFromLocal(t *testing.T) {
	// Reassigning a str local from another str local (s = t) is reachable from
	// real source (confirmed against a real fixture dump: the Store's value
	// child is a SymbolValue) but out of scope — this slice is deliberately
	// literal-to-literal only — so it is a clean rejection naming what was
	// found.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var s str = \"hi\"; var t str = \"ho\"; s = t; return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "from a SymbolValue")
}

func TestEmitRejectsStrReassignmentFromCall(t *testing.T) {
	// Reassigning a str local from a str-returning call (s = g()) is reachable
	// from real source (confirmed against a real fixture dump: the Store's
	// value child is a DirectCall) but out of scope — literal-to-literal only —
	// so it is a clean rejection naming what was found.
	unit, snapshot, entryID, _ := buildFixture(t, "fn g() str { return \"ho\"; } fn main() i32 { var s str = \"hi\"; s = g(); return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "from a DirectCall")
}

func TestEmitRejectsStrReassignmentFromConcat(t *testing.T) {
	// Reassigning a str local from concatenation (s = "h" + "i") is reachable
	// from real source (confirmed against a real fixture dump: the Store's
	// value child is a BinaryValue of type str — concatenation lowers to a
	// str-typed BinaryValue, and interpolation is the separate
	// InterpolatedString node) but out of scope — concatenation/interpolation
	// needs runtime primitives this backend has none of — so it is a clean
	// rejection naming what was found.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var s str = \"hi\"; s = \"h\" + \"i\"; return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "from a BinaryValue")
}

func TestEmitStrParameterLiteralCompilesAndRuns(t *testing.T) {
	// A str-typed parameter passed a string literal at the call site, the
	// helper comparing it against a literal and returning a distinguishing
	// integer result: f("hi") compares s == "hi" (true) and exits 1, f("ho")
	// compares false and exits 0. The parameter is seeded into the callee's
	// scope as a str local and the comparison routes through the existing str
	// comparison machinery.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal", "fn f(s str) i32 { if s == \"hi\" { return 1; } else { return 0; } } fn main() i32 { return f(\"hi\"); }", 1},
		{"not equal", "fn f(s str) i32 { if s == \"hi\" { return 1; } else { return 0; } } fn main() i32 { return f(\"ho\"); }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStrParameterLocalCompilesAndRuns(t *testing.T) {
	// A str-typed parameter passed a str-typed local (not a literal directly)
	// at the call site: the local's decoded content flows into the callee
	// through the parameter, proving the value is passed rather than
	// re-created at the call site. With the local holding "hi" the comparison
	// is true (exit 1); with "ho" it is false (exit 0).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"local equals literal", "fn f(s str) i32 { if s == \"hi\" { return 1; } else { return 0; } } fn main() i32 { let x str = \"hi\"; return f(x); }", 1},
		{"local differs from literal", "fn f(s str) i32 { if s == \"hi\" { return 1; } else { return 0; } } fn main() i32 { let x str = \"ho\"; return f(x); }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStrParameterWritesC(t *testing.T) {
	// The parameter C type for a str-taking helper: the C signature declares
	// the parameter as the runtime ABI's fixed PebbleStr (the same C type a
	// str local is declared with, no typedef involved) with the
	// pebble_local_<symbol> naming every parameter uses, plus the (void) cast
	// every parameter gets. Symbols 24 (f), 25 (the s parameter), and 26 (main)
	// come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn f(s str) i32 { if s == \"hi\" { return 1; } else { return 0; } } fn main() i32 { return f(\"hi\"); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx, PebbleStr pebble_local_25) {",
		"    (void)pebble_local_25;",
		"return pebble_fn_24(ctx, (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 });",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrReturningHelperLocalDeclarationCompilesAndRuns(t *testing.T) {
	// A str-returning helper whose result is used in a str-typed local's
	// declaration, then compared: g returns "hi", the entry declares s from the
	// call (the one supported call position for declaring a str local — the
	// direct initializer of a matching str local), and the comparison against
	// the literal proves the returned value landed in the local. 7 on the
	// then-arm, 3 on the else.
	emitAndRun(t, "fn g() str { return \"hi\"; } fn main() i32 { let s str = g(); if s == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// A str-returning helper forwarding an already-declared str local: g
	// declares its own str local and `return s;` forwards it (a plain
	// SymbolValue return, the str analog of the tuple/struct forward), so the
	// return statement emits `return pebble_local_<s>;`. The returned value
	// lands in the entry's local and compares equal. 7 on the then-arm.
	emitAndRun(t, "fn g() str { var s str = \"hi\"; return s; } fn main() i32 { let s str = g(); if s == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReturningHelperDirectComparisonCompilesAndRuns(t *testing.T) {
	// A str-returning helper whose result is directly compared without an
	// intermediate local: g() == "hi" — confirmed checker-reachable (the
	// comparison's left operand is a DirectCall of type str, dumped from a real
	// fixture). The call result flows straight into pebble_rt_str_eq as a
	// PebbleStr value. 7 on the then-arm.
	emitAndRun(t, "fn g() str { return \"hi\"; } fn main() i32 { if g() == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReturningHelperChainedReturnCompilesAndRuns(t *testing.T) {
	// A str-returning helper whose result is another str-returning helper's
	// return value (`return g();` — confirmed checker-reachable, a DirectCall
	// as a str-returning function's Return child): h forwards g's result, the
	// entry declares s from h's call, and the comparison proves the value
	// survived the two-hop chain. 7 on the then-arm.
	emitAndRun(t, "fn g() str { return \"hi\"; } fn h() str { return g(); } fn main() i32 { let s str = h(); if s == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrParameterAndResultCompilesAndRuns(t *testing.T) {
	// A helper taking a str parameter and returning it — `fn echo(s str) str
	// { return s; }` — combining the str-parameter and str-result support: the
	// parameter seeds the callee's scope as a str local and the tail return
	// forwards it, so the same value round-trips through the function
	// boundary. 7 on the then-arm.
	emitAndRun(t, "fn echo(s str) str { return s; } fn main() i32 { let s str = echo(\"hi\"); if s == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReturningHelperAsCallArgumentCompilesAndRuns(t *testing.T) {
	// A str-returning helper's result passed as a str-typed call argument
	// (f(g())) — the argument builder shares buildStrOperand with the direct-
	// comparison and return paths, so a str value is uniformly whatever a str
	// expression builds. g's result lands in f's parameter and compares equal,
	// exiting 1.
	emitAndRun(t, "fn g() str { return \"hi\"; } fn f(s str) i32 { if s == \"hi\" { return 1; } else { return 0; } } fn main() i32 { return f(g()); }", false, 1, false)
}

func TestEmitStrReturningHelperWritesC(t *testing.T) {
	// The parameter and return C types for a str-taking, str-returning helper
	// (the greet flagship): the helper's C signature declares PebbleStr for
	// both the parameter and the return type — the runtime ABI's fixed type, no
	// typedef — its return statement forwards the parameter, and the call site
	// passes the literal as a PebbleStr compound literal and declares the
	// entry's local from the call. Symbols 24 (greet), 25 (the name
	// parameter), and 27 (the entry's s local) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn greet(name str) str { return name; } fn main() i32 { let s str = greet(\"hi\"); if s == \"hi\" { return 7; } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static PebbleStr pebble_fn_24(PebbleContext *ctx, PebbleStr pebble_local_25) {",
		"    (void)pebble_local_25;",
		"    return pebble_local_25;",
		"PebbleStr pebble_local_27 = pebble_fn_24(ctx, (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 });",
		"    (void)pebble_local_27;",
		"if (pebble_rt_str_eq(pebble_local_27, (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 })) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrIndexCompilesAndRuns(t *testing.T) {
	// String indexing (s[0]) is reachable from real source and lowers to a
	// bare tir.CheckedIndex whose result type is char (confirmed against a
	// real fixture dump: Children = [SymbolValue s, IntegerLiteral 0]); this
	// was 10.41's rejection fixture and is now the positive case —
	// buildCharOperand builds the read as the runtime's UTF-8 decoder
	// pebble_rt_str_char_at_i32. s = "hi", so s[0] decodes to 'h'; comparing
	// against the char literal proves the decoded value is correct
	// end-to-end, not just that it compiles.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let c char = s[0]; if c == 'h' { return 1; } else { return 0; } }", false, 1, false)
}

// 10.42 — str indexing (s[i] returning char)

func TestEmitStrIndexWritesC(t *testing.T) {
	// The exact emitted call for a str-typed local base: s[0] emits
	// pebble_rt_str_char_at_i32(pebble_local_<s>, 0) — the base is the
	// PebbleStr local's own C name (built by buildStrOperand's SymbolValue
	// case) and the literal index is emitted as its decimal text. Symbols 25
	// (s) and 26 (c) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let c char = s[0]; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_26 = pebble_rt_str_char_at_i32(pebble_local_25, 0, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrIndexOutOfBoundsEmitsRealSourceLoc(t *testing.T) {
	// Out-of-range str indexing (s = "hi" has 2 codepoints; s[5] is out of
	// bounds) still aborts via pebble_rt_str_char_at_i32, and since 10.44 the
	// call carries a real, resolved Pebble source location instead of the
	// zero-valued placeholder.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let s str = \"hi\"; let c char = s[5]; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_str_char_at_i32(") {
		t.Errorf("emitted C missing the str char-at call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
}

func TestEmitStrIndexLiteralBaseCompilesAndRuns(t *testing.T) {
	// A bare string literal base ("hi"[0]) is checker-reachable (confirmed
	// against a real fixture dump — the CheckedIndex base is a StringLiteral
	// node) and buildStrOperand already builds it unchanged as a PebbleStr
	// compound literal, so the decoder call takes the inline literal as its
	// base argument. "hi"[0] = 'h', exit 1.
	emitAndRun(t, "fn main() i32 { let c char = \"hi\"[0]; if c == 'h' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexLiteralBaseWritesC(t *testing.T) {
	// The emitted call for the literal-base shape: the base argument is the
	// inline PebbleStr compound literal, not a local reference.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let c char = \"hi\"[0]; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		`pebble_rt_str_char_at_i32((PebbleStr){ .data = (const uint8_t *)"hi", .len = 2 }, 0, (PebbleSourceLoc){"main.peb"`,
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrIndexMultiByteCompilesAndRuns(t *testing.T) {
	// s[i] is a Unicode-scalar-value index, not a byte offset: "aéb" is a (1
	// byte) + é (U+00E9, 2 bytes) + b (1 byte), so codepoint 1 is é and
	// codepoint 2 is b — byte offset 2 would land in the middle of é's
	// UTF-8 sequence. Both reads round-trip through equality against the char
	// literals, proving the decoder walks codepoints, not bytes.
	emitAndRun(t, "fn main() i32 { let s str = \"a\u00e9b\"; let c char = s[1]; if c == '\u00e9' { let d char = s[2]; if d == 'b' { return 1; } else { return 0; } } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexEmojiCompilesAndRuns(t *testing.T) {
	// The strongest multi-byte proof: "a😀b" is a (1 byte) + 😀 (U+1F600, 4
	// bytes) + b (1 byte), so codepoint 1 is the full 21-bit scalar value
	// 128512 — a 4-byte sequence — compared against the emoji char literal,
	// proving the index lands on the second codepoint and not partway through
	// the first one's bytes.
	emitAndRun(t, "fn main() i32 { let s str = \"a\U0001F600b\"; let c char = s[1]; if c == '\U0001F600' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexRuntimeLocalCompilesAndRuns(t *testing.T) {
	// The index is a runtime-computed width-typed local, not a literal: i is
	// declared i32 and computed by checked arithmetic (1 + 1 = 2), so the
	// CheckedIndex's index child is a CheckedArithmetic node built by
	// buildExpr. s = "abc", s[i] = s[2] = 'c', exit 1.
	emitAndRun(t, "fn main() i32 { let s str = \"abc\"; let i i32 = 1 + 1; let c char = s[i]; if c == 'c' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexLocalReferenceCompilesAndRuns(t *testing.T) {
	// The index is a plain width-typed local reference (a SymbolValue built by
	// buildExpr), reaching s[i] at a runtime-computed position: i = 1, so
	// s[1] = 'i'. Proves the width-typed SymbolValue index path, distinct from
	// the literal and arithmetic shapes.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let i i32 = 1; let c char = s[i]; if c == 'i' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexRangeLoopIteratorCompilesAndRuns(t *testing.T) {
	// A range loop's iterator used directly as a str index arrives as an
	// int-typed SymbolValue (the unanchored-int case, the same shortcut
	// buildArrayPlaceRead handles), confirmed against a real fixture dump.
	// Iterating 0..2 over "hi" and counting each match of 'h' (only index 0)
	// proves the iterator's C name is the correct index lvalue.
	emitAndRunBounded(t, "fn main() i32 { let s str = \"hi\"; var n i32 = 0; loop 0..2 : i { if s[i] == 'h' { n = n + 1; } } return n; }", false, 1, false)
}

func TestEmitStrIndexOutOfRangePanics(t *testing.T) {
	// s = "hi" has 2 codepoints; s[2] is past the last codepoint, so the
	// runtime's UTF-8 decoder panics (abort) instead of reading past the end.
	// The process must terminate abnormally, not exit cleanly.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let c char = s[2]; return 0; }", false, 0, true)
}

func TestEmitStrIndexNegativePanics(t *testing.T) {
	// A negative index — i = 0 - 1 = -1 computed by checked arithmetic (which
	// itself does not overflow) — panics the decoder. The process must
	// terminate abnormally.
	emitAndRun(t, "fn main() i32 { let s str = \"hi\"; let i i32 = 0; let j i32 = i - 1; let c char = s[j]; return 0; }", false, 0, true)
}

func TestEmitStrIndexI64EntryCompilesAndRuns(t *testing.T) {
	// The width-generic path: an i64 entry's str index emits
	// pebble_rt_str_char_at_i64 — only the index parameter's width varies by
	// the entry's; the result type is still the fixed int32_t char either
	// way. The index here is an i64-typed local reference, so the whole
	// i64 index path is exercised. s = "hi", i = 1, s[1] = 'i', exit 1.
	emitAndRun(t, "fn main() i64 { let s str = \"hi\"; let i i64 = 1; let c char = s[i]; if c == 'i' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitStrIndexI64EntryWritesC(t *testing.T) {
	// The emitted call for an i64 entry: the helper is the _i64 variant (the
	// index parameter is int64_t), and the base local is still the PebbleStr
	// local's own C name. Symbols 25 (s) and 26 (c) come from the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { let s str = \"hi\"; let c char = s[0]; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_26 = pebble_rt_str_char_at_i64(pebble_local_25, 0, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitRejectsNonStrCheckedIndex(t *testing.T) {
	// Indexing an array literal directly (['h', 'i'][0]) is reachable from
	// real source and lowers to a bare CheckedIndex too — an array literal
	// has no addressable place, so it cannot form a Load(CheckedIndexPlace) —
	// but this slice only lowers a str base. It is therefore a clean
	// rejection naming what was found (the ArrayValue base and its [2]char
	// type), never a guessed lowering.
	emitAndRunRejects(t, "fn main() i32 { let c char = ['h', 'i'][0]; return 0; }", "indexes a ArrayValue of type [2]char, want str")
}

// 10.26 — tuple- and struct-typed function return types

func TestEmitTupleReturningHelperCompilesAndRuns(t *testing.T) {
	// The flagship tuple-return fixture: makeT returns a fresh (i32, i32)
	// tuple constructed inline in its return statement, the entry declares a
	// matching tuple local from the call (the one supported call position for
	// a tuple-returning helper — the direct initializer of a matching local),
	// and reads both elements back. The helper's C signature must declare its
	// return type as the tuple's own typedef, its return statement must emit
	// the compound-literal expression, and the call site must initialize the
	// local from the call. 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { let t (i32, i32) = makeT(); return t.0 + t.1; }", false, 42, false)
}

func TestEmitStructReturningHelperCompilesAndRuns(t *testing.T) {
	// The flagship struct-return fixture, mirroring the tuple one: makeP
	// returns a fresh Point constructed inline in its return statement, the
	// entry declares a matching struct local from the call and reads both
	// fields back. The designated-initializer compound-literal return value is
	// exercised end-to-end. 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { return Point.{ x = 20, y = 22 }; } fn main() i32 { let p Point = makeP(); return p.x + p.y; }", false, 42, false)
}

func TestEmitTupleReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// A tuple-returning helper whose return statement forwards an
	// already-declared aggregate-typed local (a plain SymbolValue, not a fresh
	// construction): x is declared in the helper's body from a tuple literal,
	// and `return x;` forwards it, emitting `return pebble_local_<x>;`. The
	// entry assigns the call to a matching local and reads both elements back.
	// 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn makeT() (i32, i32) { let x (i32, i32) = (20, 22); return x; } fn main() i32 { let t (i32, i32) = makeT(); return t.0 + t.1; }", false, 42, false)
}

func TestEmitStructReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// The struct side of forwarding an already-declared local: p is declared
	// in the helper's body from a struct literal and `return p;` forwards it,
	// emitting `return pebble_local_<p>;`. The entry assigns the call to a
	// matching local and reads both fields back. 20 + 22 = 42 is the process
	// exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { let p Point = Point.{ x = 20, y = 22 }; return p; } fn main() i32 { let q Point = makeP(); return q.x + q.y; }", false, 42, false)
}

func TestEmitTupleReturningHelperWithBoolElementCompilesAndRuns(t *testing.T) {
	// A mixed width/bool tuple result: the bool element is built by
	// buildBoolExpr inside the tuple's brace list (proving the element grammar
	// dispatch in the return path, not just the local-declaration path), and
	// once read back in the entry it drives an if. With t.1 true the then-arm
	// returns the i32 element 20.
	emitAndRun(t, "fn makeT() (i32, bool) { return (20, true); } fn main() i32 { let t (i32, bool) = makeT(); if t.1 { return t.0; } else { return 99; } }", false, 20, false)
}

func TestEmitTupleReturningHelperIfElseTailCompilesAndRuns(t *testing.T) {
	// A tuple-returning helper whose body tail is a two-armed if/else (not a
	// bare return): each arm's return is a fresh tuple construction, proving
	// buildIf threads the enclosing function's resultInfo into both arms so
	// each arm's Return routes through buildAggregateReturnValue. With the flag
	// true the then-arm's (20, 22) wins, and 20 + 22 = 42 is the exit code.
	emitAndRun(t, "fn pick(b bool) (i32, i32) { if b { return (20, 22); } else { return (0, 0); } } fn main() i32 { let t (i32, i32) = pick(true); return t.0 + t.1; }", false, 42, false)
}

func TestEmitTupleReturningHelperWritesC(t *testing.T) {
	// The emitted C for the tuple flagship: the tuple typedef must precede the
	// helper, the helper's signature declares its return type as
	// pebble_tuple_23_t (the aggregate's own typedef, not the entry's scalar
	// int32_t), its return statement emits the C99 compound-literal expression
	// (pebble_tuple_23_t){ 20, 22 }, and the call site initializes the local
	// directly from pebble_fn_24(ctx). Symbols 24 (makeT), 25 (main), 26 (t
	// local), and tuple type 23 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { let t (i32, i32) = makeT(); return t.0 + t.1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n} pebble_tuple_23_t;",
		"static pebble_tuple_23_t pebble_fn_24(PebbleContext *ctx) {",
		"    return (pebble_tuple_23_t){ 20, 22 };",
		"pebble_tuple_23_t pebble_local_26 = pebble_fn_24(ctx);",
		"    (void)pebble_local_26;",
		"return pebble_rt_checked_add_i32(pebble_local_26._0, pebble_local_26._1, (PebbleSourceLoc){\"main.peb\", 1, 95});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static pebble_tuple_23_t pebble_fn_24")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("tuple typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitStructReturningHelperWritesC(t *testing.T) {
	// The emitted C for the struct flagship: the struct typedef precedes the
	// helper, the helper's signature declares its return type as
	// pebble_struct_23_t, its return statement emits the designated-
	// initializer compound-literal expression, and the call site initializes
	// the local from pebble_fn_27(ctx). Symbols 24 (Point), 25 (x), 26 (y), 27
	// (makeP), 28 (main), 29 (p local), and struct type 23 come from the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { return Point.{ x = 20, y = 22 }; } fn main() i32 { let p Point = makeP(); return p.x + p.y; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"static pebble_struct_23_t pebble_fn_27(PebbleContext *ctx) {",
		"    return (pebble_struct_23_t){ .pebble_field_25 = 20, .pebble_field_26 = 22 };",
		"pebble_struct_23_t pebble_local_29 = pebble_fn_27(ctx);",
		"return pebble_rt_checked_add_i32(pebble_local_29.pebble_field_25, pebble_local_29.pebble_field_26, (PebbleSourceLoc){\"main.peb\", 2, 101});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static pebble_struct_23_t pebble_fn_27")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("struct typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitTupleReturningHelperForwardsLocalWritesC(t *testing.T) {
	// The emitted C for the local-forwarding return: the helper's return
	// statement emits `return pebble_local_<x>;` (the already-declared local's
	// own C name, no re-construction). Symbols 24 (makeT), 25 (main), 26 (x
	// local), 27 (t local), and tuple type 23 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn makeT() (i32, i32) { let x (i32, i32) = (20, 22); return x; } fn main() i32 { let t (i32, i32) = makeT(); return t.0 + t.1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static pebble_tuple_23_t pebble_fn_24(PebbleContext *ctx) {",
		"    pebble_tuple_23_t pebble_local_26 = { 20, 22 };",
		"    return pebble_local_26;",
		"pebble_tuple_23_t pebble_local_27 = pebble_fn_24(ctx);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitTupleReturnResultTypeOnlyHelperCompilesAndRuns(t *testing.T) {
	// The typedef-discovery fixture (10.26): the (i32, i32) tuple type appears
	// only as makeT's result type — makeT constructs a tuple of that type in
	// its return (never anywhere in main, which only assigns the call to a
	// matching local and never reads an element or constructs a tuple), so the
	// emitted helper's C signature still names pebble_tuple_<typeID>_t and the
	// typedef must be emitted before it. The program compiles clean under the
	// strict flags and exits 0.
	emitAndRun(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { let t (i32, i32) = makeT(); return 0; }", false, 0, false)
}

func TestEmitStructReturnResultTypeOnlyHelperCompilesAndRuns(t *testing.T) {
	// The struct side of the typedef-discovery fixture: the Point type appears
	// only as makeP's result type — makeP constructs a Point in its return,
	// and main only assigns the call to a matching local and never reads a
	// field or constructs a Point — yet the typedef must still be emitted
	// before the helper whose C signature names pebble_struct_<typeID>_t. The
	// program compiles clean under the strict flags and exits 0.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { return Point.{ x = 20, y = 22 }; } fn main() i32 { let p Point = makeP(); return 0; }", false, 0, false)
}

func TestEmitTupleResultTypeScanGetsTypedef(t *testing.T) {
	// The ResultType scan in collectTupleTypes, proven load-bearing: the
	// (i32, i32) tuple type is used ONLY as makeT's result type, and the
	// helpers slice pairs makeT's declaration with main's body block — a real,
	// valid Block that contains no tuple construction (main is `return 0;`), so
	// the body walk finds nothing and makeT's Parameters are empty. The only
	// path by which collectTupleTypes can discover type 23 is the helper's own
	// ResultType; without 10.26's ResultType scan this returns nothing and the
	// test fails. (The concrete type ID 23 is confirmed from the fixture dump.)
	unit, snapshot, entryID, _ := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { return 0; }", "main", false)
	entryDecl, err := findFunctionDeclaration(unit, entryID, "entry function")
	if err != nil {
		t.Fatalf("entry declaration: %v", err)
	}
	_, entryBlock, err := findFunctionBody(unit, entryDecl, "entry function")
	if err != nil {
		t.Fatalf("entry body: %v", err)
	}
	makeTDecl, err := findFunctionDeclaration(unit, 24, "called function")
	if err != nil {
		t.Fatalf("makeT declaration: %v", err)
	}
	// Pair makeT's declaration with main's tuple-free body block, isolating the
	// ResultType scan as the sole discovery path for tuple type 23.
	helpers := []helperInfo{{decl: makeTDecl, block: entryBlock}}
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
		t.Fatalf("tuple type 23 used only as a helper's result type was not discovered, got %v", ids)
	}
}

func TestEmitRejectsTupleReturningHelperAsArgument(t *testing.T) {
	// Calling a tuple-returning helper outside the one supported position — as
	// an argument to another function (f(makeT())) — is reachable from real
	// source: the outer DirectCall's argument is the inner DirectCall. The
	// aggregate-argument builder rejects it cleanly, naming what was found,
	// never a guessed lowering.
	unit, snapshot, entryID, _ := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn f(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { return f(makeT()); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "argument 0 is a DirectCall")
}

func TestEmitRejectsStructReturningHelperAsArgument(t *testing.T) {
	// The struct side of the argument-position rejection: f(makeP()) passes a
	// struct-returning call as an argument, which the aggregate-argument
	// builder rejects naming what was found.
	unit, snapshot, entryID, _ := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { return Point.{ x = 20, y = 22 }; } fn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return f(makeP()); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "argument 0 is a DirectCall")
}

func TestEmitRejectsTupleReturningHelperAsOperand(t *testing.T) {
	// Calling a tuple-returning helper as an operand of an element read —
	// return makeT().0; — is reachable from real source: the read lowers to a
	// TupleElementValue whose child is the DirectCall, not a SymbolValue naming
	// a tuple-typed local. The integer expression builder rejects the
	// non-local base cleanly.
	unit, snapshot, entryID, _ := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { return makeT().0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "of a DirectCall")
}

func TestEmitRejectsTupleReturningHelperInAnotherHelpersReturn(t *testing.T) {
	// Calling a tuple-returning helper as another tuple-returning helper's
	// return value — `return makeT();` from makeT2 — is reachable from real
	// source but deliberately out of scope (a call is only supported as a
	// direct-initializer use, never this return-forwarding position). The
	// tuple-returning helper's own tail Return routes through
	// buildAggregateReturnValue, which rejects the DirectCall value cleanly,
	// naming what was found.
	unit, snapshot, entryID, _ := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn makeT2() (i32, i32) { return makeT(); } fn main() i32 { let t (i32, i32) = makeT2(); return t.0 + t.1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "returns a DirectCall")
}

func TestEmitRejectsStructReturningHelperInAnotherHelpersReturn(t *testing.T) {
	// The struct side of the return-forwarding rejection: `return makeP();`
	// from another struct-returning helper is a DirectCall return value,
	// rejected by buildAggregateReturnValue naming what was found.
	unit, snapshot, entryID, _ := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { return Point.{ x = 20, y = 22 }; } fn makeP2() Point { return makeP(); } fn main() i32 { let p Point = makeP2(); return p.x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "returns a DirectCall")
}

func TestEmitRejectsEntryReturningTuple(t *testing.T) {
	// The entry itself cannot declare a tuple/struct result type: its C return
	// type stays the integer entryReturnType regardless of what the language
	// lets a helper write. validateEntrySignature rejects the tuple result
	// exactly as it always has, unchanged by this slice.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() (i32, i32) { return (1, 2); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "entry function result type is (i32, i32), want void, int, i32, or i64")
}

func TestEmitTupleReturningHelperInIfArmLocalInitializerCompilesAndRuns(t *testing.T) {
	// A tuple-returning helper called from an if/else arm's own local
	// declaration: the arm is a block built by the same recursive buildBlock,
	// so the DirectCall initializer is handled by the identical
	// buildTupleLocalDeclaration path the top-level case uses — no special
	// plumbing for the nested position. With the flag true the then-arm
	// declares t from makeT() and returns t.0 = 20.
	emitAndRun(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { let b bool = true; if b { let t (i32, i32) = makeT(); return t.0; } else { return 0; } }", false, 20, false)
}

func TestEmitTupleReturningHelperInLoopBodyLocalInitializerCompilesAndRuns(t *testing.T) {
	// A tuple-returning helper called from a while loop body's local
	// declaration: the loop body is built by buildLoopBody, whose leading
	// statements go through the same buildLeadingStatement /
	// buildTupleLocalDeclaration path, so the DirectCall initializer works in
	// the nested position without special-casing. The loop declares t from
	// makeT() once, accumulates both elements, and the entry returns the sum
	// 42. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { var n i32 = 0; var s i32 = 0; while n < 1 { let t (i32, i32) = makeT(); s = s + t.0 + t.1; n = n + 1; } return s; }", false, 42, false)
}

// --- 10.31: switch statements ---

func TestEmitSwitchMultiValueCaseCompilesAndRuns(t *testing.T) {
	// The flagship fixture: a switch with a multi-value case (1, 2 share the
	// same body returning 10), a single-value case (3 returning 30), and an
	// else (default returning 0). Subject value 1 hits the multi-value case
	// and returns 10.
	emitAndRun(t, "fn main() i32 { switch 1 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 10, false)
}

func TestEmitSwitchMultiValueCaseSecondValueCompilesAndRuns(t *testing.T) {
	// Same switch as above but subject value 2 — still hits the multi-value
	// case and returns 10, confirming both SwitchCase nodes sharing the same
	// body produce the same result.
	emitAndRun(t, "fn main() i32 { switch 2 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 10, false)
}

func TestEmitSwitchSingleValueCaseCompilesAndRuns(t *testing.T) {
	// Subject value 3 hits the single-value case and returns 30.
	emitAndRun(t, "fn main() i32 { switch 3 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 30, false)
}

func TestEmitSwitchElseCompilesAndRuns(t *testing.T) {
	// Subject value 99 hits the else/default arm and returns 0.
	emitAndRun(t, "fn main() i32 { switch 99 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchBlockCaseBodyCompilesAndRuns(t *testing.T) {
	// A block-wrapped (braced, multi-statement) case body: the case declares
	// a local and returns an expression using it. This exercises the
	// Block-bodied path in buildSwitchCaseBody.
	emitAndRun(t, "fn main() i32 { switch 1 { case 1: { let x i32 = 42; return x; } else: return 0; } }", false, 42, false)
}

func TestEmitSwitchBareReturnCaseBodyCompilesAndRuns(t *testing.T) {
	// A bare single-statement case body (no braces): `case 1: return 10;`.
	// This exercises the bare-statement path in buildSwitchCaseBody.
	emitAndRun(t, "fn main() i32 { switch 1 { case 1: return 10; else: return 0; } }", false, 10, false)
}

func TestEmitSwitchBoolSubjectCompilesAndRuns(t *testing.T) {
	// A bool subject with bool case values: `switch true { case true: return
	// 1; else: return 0; }`. Bool case values are emitted as `case 1:` (true)
	// and `case 0:` (false) in C, since C switch requires integral constants.
	emitAndRun(t, "fn main() i32 { switch true { case true: return 1; else: return 0; } }", false, 1, false)
}

func TestEmitSwitchBoolSubjectFalseCompilesAndRuns(t *testing.T) {
	// Bool subject `false` hits the else/default arm.
	emitAndRun(t, "fn main() i32 { switch false { case true: return 1; else: return 0; } }", false, 0, false)
}

func TestEmitSwitchInHelperCompilesAndRuns(t *testing.T) {
	// A switch nested inside a helper function: the helper receives a
	// parameter and switches on it, returning different values. The entry
	// calls the helper with different arguments, confirming the switch works
	// in a helper context. The entry returns the sum of two calls: helper(1)
	// = 10 and helper(99) = 0, so exit code is 10.
	emitAndRun(t, "fn helper(x i32) i32 { switch x { case 1, 2: return 10; case 3: return 30; else: return 0; } } fn main() i32 { return helper(1) + helper(99); }", false, 10, false)
}

func TestEmitSwitchWithHelperCallInSubjectCompilesAndRuns(t *testing.T) {
	// A helper call as the switch subject expression: the subject is the
	// result of calling a helper, confirming buildExpr's DirectCall path
	// works in the subject position.
	emitAndRun(t, "fn getVal() i32 { return 2; } fn main() i32 { switch getVal() { case 1: return 10; case 2: return 20; else: return 0; } }", false, 20, false)
}

func TestEmitSwitchWritesC(t *testing.T) {
	// Confirm the emitted C for a switch fixture contains the expected
	// stacked case labels and body structure.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { switch 1 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"switch (1)",
		"case 1:",
		"case 2:",
		"case 3:",
		"default:",
		"return 10;",
		"return 30;",
		"return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitSwitchCompilesCleanUnderStrictFlags(t *testing.T) {
	// The emitted C for a switch must compile under -Wall -Wextra -Werror
	// with no warnings. This exercises the full cc compilation path.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { switch 1 { case 1, 2: return 10; case 3: return 30; else: return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 10, false)
}

// TestEmitSwitchRejectsCaseValue is superseded by 10.34: a CaseValue-based
// switch case (an enum variant) is now supported for a plain enum subject —
// see TestEmitEnumLocalSwitchGreenCompilesAndRuns and its neighbors — so
// there is no longer a rejection to test here.

func TestEmitSwitchRejectsNonExhaustiveNoElse(t *testing.T) {
	// A switch with no else and non-exhaustive cases, used as a tail
	// statement: some paths do not end in a return. The checker may or may
	// not reject this; if it reaches the backend, the switch as a whole is
	// a tail that must guarantee a return on every path, and a non-exhaustive
	// switch without else does not — but a C switch is a valid tail only if
	// every reachable path returns. The checker is expected to reject this
	// shape (no else means some paths fall through without returning), so
	// the fixture should fail at check time.
	_, _, _, _, err := buildFixtureMaybeFailing(t, "fn main() i32 { switch 1 { case 1: return 10; } }", "main", false)
	if err == nil {
		t.Log("checker accepted non-exhaustive switch without else — this may be a checker gap worth investigating")
	}
}

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

func TestEmitSwitchMultipleCasesWithLocalsCompilesAndRuns(t *testing.T) {
	// A switch where each case body declares its own local — confirming
	// scope isolation between arms. Case 1 declares x=10 and returns x; case
	// 2 declares x=20 and returns x; else returns 0. Subject 2 returns 20.
	emitAndRun(t, "fn main() i32 { switch 2 { case 1: { let x i32 = 10; return x; } case 2: { let x i32 = 20; return x; } else: return 0; } }", false, 20, false)
}

func TestEmitSwitchI64EntryCompilesAndRuns(t *testing.T) {
	// A switch with an i64 entry: the subject and case values are i64.
	emitAndRun(t, "fn main() i64 { switch 2 { case 1: return 100; case 2: return 200; else: return 0; } }", false, 200, false)
}

func TestEmitSwitchNestedInHelperWithParamsCompilesAndRuns(t *testing.T) {
	// A switch inside a helper that takes a parameter, with the subject
	// being the parameter itself. Exercises the full path: parameter seeding
	// into scope, switch subject resolution, case body building. Two calls:
	// helper(1) = 10, helper(5) = 0, sum = 10.
	emitAndRun(t, "fn classify(x i32) i32 { switch x { case 1: return 10; case 2: return 20; case 3: return 30; else: return 0; } } fn main() i32 { return classify(1) + classify(5); }", false, 10, false)
}

func TestEmitDeferBeforeReturnCompilesAndRuns(t *testing.T) {
	// A single defer running before a return, observably changing the returned
	// value. var x i32 = 0; defer x = x + 1; return x; should return 1, not 0,
	// proving the deferred Store executes before the return value is read.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; defer x = x + 1; return x; }", false, 1, false)
}

func TestEmitTwoDefersLIFOOrderCompilesAndRuns(t *testing.T) {
	// Two defers in the same scope, proving LIFO (last-registered-first)
	// order. The second-registered defer (x = x + 10) must run before the
	// first (x = x * 2). Starting from x=1: first defer registers x*2, then
	// x+10. LIFO means x+10 runs first (1+10=11), then x*2 (11*2=22).
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 1; defer x = x * 2; defer x = x + 10; return x; }", false, 22, false)
}

func TestEmitDeferInsideIfArmFiresCompilesAndRuns(t *testing.T) {
	// A defer inside an if-arm whose exit (return) is inside that same arm.
	// The defer fires because the return's DeferChain includes it. Both arms
	// return, so the if is the block's tail. Condition true: defer x=x+1
	// runs, return 0+1=1. Condition false: no defer, return 2.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; if x == 0 { defer x = x + 1; return x; } else { return 2; } }", false, 1, false)
}

func TestEmitDeferOutsideIfDoesNotFireCompilesAndRuns(t *testing.T) {
	// A defer registered inside a while-loop body, where the return is AFTER
	// the loop (outside the loop's region). The return's DeferChain does not
	// include the loop's defer because the return is outside the region the
	// defer was registered in. This proves static/lexical scoping: the defer
	// only fires at exits inside the same region it was registered in.
	// Loop: i counts 0..2, when i==0 defer x=x+100 is registered and x=1.
	// After loop, return x=1 (defer did NOT fire).
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; var i i32 = 0; while i < 2 { if i == 0 { defer x = x + 100; x = 1; } i = i + 1; } return x; }", false, 1, false)
}

func TestEmitDeferBeforeBreakCompilesAndRuns(t *testing.T) {
	// A defer before a break inside a loop. The break's DeferChain includes
	// the deferred Store, so it fires before the break. Loop: i counts 0..5,
	// defer x=x+1 fires on break when i==3, break exits. x starts at 10,
	// after defer x=11. Exit code 11.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 10; var i i32 = 0; while i < 5 { if i == 3 { defer x = x + 1; break; } i = i + 1; } return x; }", false, 11, false)
}

func TestEmitDeferBeforeContinueCompilesAndRuns(t *testing.T) {
	// A defer before a continue inside a loop. The continue's DeferChain
	// includes the deferred Store. Loop: i counts 0..5, when i==2, defer
	// x=x+1 fires on continue (skipping i=i+1), then i is incremented by
	// the loop body's i=i+1 for non-continue passes. Actually, continue
	// skips the rest of the body, so i=i+1 is skipped when i==2. Let me
	// restructure: use a for loop where the update is separate.
	// Simpler: while loop, i starts at 0, defer x=x+1 then continue when
	// i==1, so i stays at 1 forever — no, that would loop forever.
	// Better approach: defer fires on continue, then manually increment.
	// i: 0->1->2 (continue fires defer x=x+1) ->3->4->5 exits. x=20+1=21.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 20; var i i32 = 0; while i < 5 { if i == 2 { defer x = x + 1; i = i + 1; continue; } i = i + 1; } return x; }", false, 21, false)
}

func TestEmitDeferNestedScopesCompilesAndRuns(t *testing.T) {
	// Defers in multiple nested scopes crossing an exit: a defer at the
	// function's top level, a while loop with its own defer, and a break
	// inside the loop. The break's DeferChain should include the loop's
	// defer but NOT the function-level defer. After break, the function's
	// defer fires on the return.
	// x=0, outer defer x=x+100 registered.
	// Loop: i=0, inner defer x=x+1 registered. i==0 -> break.
	// Break's DeferChain: inner defer only. x = 0+1 = 1.
	// Return's DeferChain: outer defer. x = 1+100 = 101.
	emitAndRunBounded(t, "fn main() i32 { var x i32 = 0; defer x = x + 100; var i i32 = 0; while i < 5 { defer x = x + 1; if i == 0 { break; } i = i + 1; } return x; }", false, 101, false)
}

func TestEmitDeferInHelperCompilesAndRuns(t *testing.T) {
	// A defer inside a helper function. The helper has its own defer that
	// modifies a local before returning it. The entry calls the helper and
	// returns the result. helper(): x=0, defer x=x+5, return x -> returns 5.
	// main() returns helper() = 5.
	emitAndRunBounded(t, "fn helper() i32 { var x i32 = 0; defer x = x + 5; return x; } fn main() i32 { return helper(); }", false, 5, false)
}

func TestEmitDeferredStoreCOutput(t *testing.T) {
	// Confirm the emitted C for a fixture: the deferred statement's text
	// appears immediately before the return, and nothing is emitted at the
	// defer statement's own position in program order.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var x i32 = 0; defer x = x + 1; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// The deferred assignment must appear before the return.
	if !strings.Contains(out, "pebble_local_") {
		t.Errorf("emitted C has no local references:\n%s", out)
	}
	// The deferred statement should be an assignment, not a declaration.
	if strings.Contains(out, "defer") {
		t.Errorf("emitted C contains the word 'defer':\n%s", out)
	}
	// The return must be present.
	if !strings.Contains(out, "return") {
		t.Errorf("emitted C has no return statement:\n%s", out)
	}
	// Compile and run to confirm correctness.
	compileAndRunBounded(t, buf.Bytes(), 1, false)
}

func TestEmitDeferredVoidCallFiresCompilesAndRuns(t *testing.T) {
	// A deferred void call, mirroring 10.32's defer test structure: the defer
	// is registered in the same scope as the return, so the return's DeferChain
	// includes it and the call fires immediately before the exit. A void call
	// has no observable value, so the exit code is the caller's own; a
	// same-scope deferred Store is paired with it so the defer mechanism's
	// firing is independently observable (LIFO: x = x + 1 runs first -> x = 1,
	// then helper() runs; return reads x = 1).
	emitAndRunBounded(t, "fn helper() void {} fn main() i32 { var x i32 = 0; defer helper(); defer x = x + 1; return x; }", false, 1, false)
}

func TestEmitDeferredVoidCallBeforeBreakCompilesAndRuns(t *testing.T) {
	// A deferred void call inside a loop firing before a break, mirroring
	// 10.32's TestEmitDeferBeforeBreakCompilesAndRuns: the break's DeferChain
	// includes the loop-registered defers, so both the deferred Store and the
	// deferred void call run before the break. x starts at 10; on i == 3 the
	// break fires the deferred x = x + 1 (x = 11) then helper(); the program
	// exits 11. If the deferred call were miscompiled the build would fail.
	emitAndRunBounded(t, "fn helper() void {} fn main() i32 { var x i32 = 10; var i i32 = 0; while i < 5 { if i == 3 { defer helper(); defer x = x + 1; break; } i = i + 1; } return x; }", false, 11, false)
}

func TestEmitDeferredVoidCallOutsideLoopDoesNotFireCompilesAndRuns(t *testing.T) {
	// A deferred void call registered inside a loop whose exit is AFTER the
	// loop: the return's DeferChain does not include the loop-registered defer
	// (static/lexical scoping, the same property 10.32's
	// TestEmitDeferOutsideIfDoesNotFireCompilesAndRuns proves for a deferred
	// Store), so the call never fires — and, because a never-firing deferred
	// call has no call site, the reachability walk must NOT emit its callee at
	// all (emitting it would trip -Wunused-function under the mandated -Wall
	// -Wextra -Werror build). The program still compiles and runs to its own
	// exit code with no trace of the helper in the emitted C.
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper() void {} fn main() i32 { var x i32 = 0; var i i32 = 0; while i < 5 { if i == 0 { defer helper(); } i = i + 1; } return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if strings.Contains(out, "pebble_fn_") {
		t.Errorf("emitted C contains a helper for a deferred call that never fires, which would trip -Wunused-function:\n%s", out)
	}
	compileAndRunBounded(t, buf.Bytes(), 0, false)
}

func TestEmitDeferredVoidCallCOutput(t *testing.T) {
	// Confirm the emitted C for a deferred void call that fires: the call
	// statement's text appears immediately before the return, and nothing is
	// emitted at the defer statement's own position in program order (the
	// DeferRegister is a pure registration marker).
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper() void {} fn main() i32 { defer helper(); return 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_fn_24(ctx);") {
		t.Errorf("emitted C is missing the deferred void-call statement:\n%s", out)
	}
	if strings.Contains(out, "defer") {
		t.Errorf("emitted C contains the word 'defer':\n%s", out)
	}
	if !strings.Contains(out, "return") {
		t.Errorf("emitted C has no return statement:\n%s", out)
	}
	compileAndRunBounded(t, buf.Bytes(), 1, false)
}

// --- 10.34: plain enum locals and switch matching ---

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
	for _, td := range unit.TypeDeclarations() {
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

func TestEmitEnumLocalSwitchGreenCompilesAndRuns(t *testing.T) {
	// The flagship fixture from the brief: a plain enum local declared from a
	// variant literal and switched on, each case returning a distinct value.
	// c is Color.green (variant 26, ordinal 1), so the green case fires and the
	// exit code is 1. The emitted C typedef assigns pebble_variant_26 the value
	// 1 by declaration order, and the C switch compares the local's stored
	// constant against the case labels, so the right case fires end to end.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 1, false)
}

func TestEmitEnumLocalSwitchBlueCompilesAndRuns(t *testing.T) {
	// Same switch, c = Color.blue (variant 27, ordinal 2): the blue case fires
	// and the exit code is 2, proving a second variant value dispatches to a
	// different C case label rather than the switch only ever firing the first
	// case.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.blue;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 2, false)
}

func TestEmitEnumLocalSwitchRedCompilesAndRuns(t *testing.T) {
	// Same switch, c = Color.red (variant 25, ordinal 0): the red case fires
	// and the exit code is 0, proving the first declared variant (ordinal 0)
	// matches case label pebble_variant_25 — the 0 constant C assigns first.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 0, false)
}

func TestEmitEnumSwitchMultiValueCaseCompilesAndRuns(t *testing.T) {
	// A multi-value case on an enum subject: `case Color.red, Color.green:`
	// produces two SwitchCase nodes sharing one body node ID (confirmed against
	// a real fixture), which must stack as two C case labels sharing one body,
	// exactly as 10.31's integer multi-value cases do. c = Color.green hits the
	// multi-value case and returns 10.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red, Color.green: return 10; case Color.blue: return 20; }\n}", false, 10, false)
}

func TestEmitEnumSwitchMultiValueCaseOtherVariantCompilesAndRuns(t *testing.T) {
	// Same multi-value switch with c = Color.red: the other member of the
	// multi-value case fires, proving both stacked case labels share the body.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nswitch c { case Color.red, Color.green: return 10; case Color.blue: return 20; }\n}", false, 10, false)
}

func TestEmitEnumSwitchElseCompilesAndRuns(t *testing.T) {
	// An else arm on an enum switch: c = Color.blue is covered by no case
	// (only red and green have cases), so the else/default arm fires and
	// returns 20.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.blue;\nswitch c { case Color.red, Color.green: return 10; else: return 20; }\n}", false, 20, false)
}

func TestEmitEnumSwitchElseCaseHitCompilesAndRuns(t *testing.T) {
	// Same switch, c = Color.green: the case fires (10), not the else arm,
	// proving the else/default arm is not selected when a case matches.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red, Color.green: return 10; else: return 20; }\n}", false, 10, false)
}

func TestEmitEnumBlockCaseBodyCompilesAndRuns(t *testing.T) {
	// A block-wrapped (braced, multi-statement) case body on an enum switch,
	// exercising the Block-bodied path in buildSwitchCaseBody for an enum
	// subject: the case declares a local and returns an expression using it.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nswitch c { case Color.red: { let x i32 = 42; return x; } case Color.green: return 1; case Color.blue: return 2; }\n}", false, 42, false)
}

func TestEmitEnumSwitchBareReturnCaseBodyCompilesAndRuns(t *testing.T) {
	// A bare single-statement case body (no braces) on an enum switch,
	// exercising the bare-Return path in buildSwitchCaseBody.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.blue;\nswitch c { case Color.red: return 1; case Color.green: return 2; case Color.blue: return 3; }\n}", false, 3, false)
}

func TestEmitEnumStoreCompilesAndRuns(t *testing.T) {
	// Reassigning an enum-typed local (c = Color.red; after declaration) lowers
	// through buildStoreCore's enum branch to
	// `pebble_local_<sym> = pebble_variant_<red>;`, so the subsequent switch
	// fires the red case. This proves the value actually changed, not just that
	// the store compiled.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nc = Color.red;\nswitch c { case Color.red: return 7; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 7, false)
}

func TestEmitEnumVariantCallFormCompilesAndRuns(t *testing.T) {
	// A plain enum variant written with explicit empty parens — Color.red() —
	// is a zero-payload VariantConstruct (confirmed against a real fixture),
	// the same discriminant value as Color.red, so it is accepted as an enum
	// local's initializer and the red case fires.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red();\nswitch c { case Color.red: return 9; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 9, false)
}

func TestEmitEnumEqualityCompilesAndRuns(t *testing.T) {
	// Equality between enum values, c == Color.red — confirmed checker-
	// reachable (it produces a BinaryValue with two enum-typed operands), so it
	// lowers through buildComparison's enum branch to the plain C == on the
	// two enum constants. With c = green the comparison is false and the else
	// arm returns 5.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nif c == Color.red { return 1; } else { return 5; }\n}", false, 5, false)
}

func TestEmitEnumEqualityTrueCompilesAndRuns(t *testing.T) {
	// The == comparison with c = red is true, proving the equality actually
	// evaluates rather than always falling to the else arm.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nif c == Color.red { return 1; } else { return 5; }\n}", false, 1, false)
}

func TestEmitEnumOrderingCompilesAndRuns(t *testing.T) {
	// An ordering comparison on enum values, c < Color.blue — also confirmed
	// checker-reachable (the checker accepts it, unlike bool ordering). Both
	// operands lower to their enum constants (green = ordinal 1, blue = ordinal
	// 2), and the plain C < on the discriminants is the direct, correct
	// lowering: 1 < 2 is true, so the then-arm returns 1.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nif c < Color.blue { return 1; } else { return 5; }\n}", false, 1, false)
}

func TestEmitEnumWhileConditionCompilesAndRuns(t *testing.T) {
	// An enum equality comparison as a while condition, reassigning the enum
	// local inside the loop so the loop terminates. The first iteration sees
	// c == Color.red true (c = red), so the loop runs once, reassigns c to
	// blue, and exits on the second condition check. Bounded execution because
	// the loop's own condition is the only bound.
	emitAndRunBounded(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nvar n i32 = 0;\nwhile c == Color.red { n = n + 1; c = Color.blue; }\nreturn n;\n}", false, 1, false)
}

func TestEmitEnumLocalUnusedCompilesClean(t *testing.T) {
	// A plain enum local declared and never referenced after its declaration
	// still compiles clean under -Wall -Wextra -Werror: the emitted declaration
	// is followed by the same (void) cast every other local gets, so the strict
	// build never warns about an unused variable.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nreturn 3;\n}", false, 3, false)
}

func TestEmitEnumWritesC(t *testing.T) {
	// Confirm the emitted C directly for one fixture: the enum typedef's exact
	// shape (one named constant per variant, in declared order, named from each
	// variant's symbol ID; the typedef named from the enum's type ID), the
	// enum-typed local's declaration initializing from the variant's constant,
	// and the switch's case labels naming the same constants.
	unit, snapshot, entryID, enumType, variants, sources := enumFixture(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}")
	if len(variants) != 3 {
		t.Fatalf("fixture has %d variants, want 3 (red, green, blue)", len(variants))
	}
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	typedef := "typedef enum {\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[0])) + ",\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[1])) + ",\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[2])) + ",\n" +
		"} " + enumTypeName(enumType) + ";"
	if !strings.Contains(out, typedef) {
		t.Errorf("emitted C is missing the enum typedef %q:\n%s", typedef, out)
	}
	for _, want := range []string{
		enumTypeName(enumType) + " pebble_local_",
		"= pebble_variant_" + strconv.Itoa(int(variants[1])) + ";",
		"switch (pebble_local_",
		"case pebble_variant_" + strconv.Itoa(int(variants[0])) + ":",
		"case pebble_variant_" + strconv.Itoa(int(variants[1])) + ":",
		"case pebble_variant_" + strconv.Itoa(int(variants[2])) + ":",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

// unionFixture builds one .peb source through the full check pipeline and
// resolves the tagged-union type's TypeID and its variant symbols in declared
// order, reusing enumFixture's exact type-resolution mechanism (a tagged union
// is a Nominal type exactly like a plain enum, so a TypeUse carries its
// TypeID the same way).
func unionFixture(t *testing.T, sourceText string) (*tir.Unit, *types.Snapshot, symbol.SymbolID, types.TypeID, []symbol.SymbolID, *source.FileSet) {
	t.Helper()
	return enumFixture(t, sourceText)
}

func TestEmitUnionLocalSwitchValueCompilesAndRuns(t *testing.T) {
	// The flagship fixture from the brief: a tagged-union local constructed
	// with a payload (Choice.value(5)) and switched on by discriminant, each
	// case returning a distinct value. The union is emitted as a tagged struct
	// whose tag is the discriminant enum constant; the switch compares
	// pebble_local_<sym>.tag against the case labels, so the value case fires
	// and the exit code is 1.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 1, false)
}

func TestEmitUnionLocalSwitchEmptyCompilesAndRuns(t *testing.T) {
	// Same switch, c constructed as the payload-less variant (Choice.empty, an
	// EnumVariantValue): the empty case fires and the exit code is 0, proving
	// the payload-less variant of a tagged union (whose other variant DOES
	// carry a payload elsewhere in the reachable program) still dispatches to
	// its own discriminant case — the payload union is simply left unspecified.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.empty;\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 0, false)
}

func TestEmitUnionSwitchMultiValueCaseCompilesAndRuns(t *testing.T) {
	// A multi-value case on a tagged-union subject: `case Choice.empty,
	// Choice.value:` produces two SwitchCase nodes sharing one body node ID
	// (confirmed against a real fixture), which stack as two C case labels
	// sharing one body exactly as plain-enum and integer multi-value cases do.
	// c = Choice.value(5) hits the multi-value case and returns 10.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case Choice.empty, Choice.value: return 10; }\n}", false, 10, false)
}

func TestEmitUnionSwitchElseCompilesAndRuns(t *testing.T) {
	// An else arm on a tagged-union switch: c = Choice.empty is covered by no
	// case (only value has one), so the else/default arm fires and returns 20.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.empty;\nswitch c { case Choice.value: return 10; else: return 20; }\n}", false, 20, false)
}

func TestEmitUnionStoreCompilesAndRuns(t *testing.T) {
	// Reassigning a tagged-union local from a payload-less construction to a
	// payload-carrying one (c = Choice.value(5);) lowers through buildStoreCore
	// to the union's compound literal, so the subsequent switch fires the value
	// case. This proves the stored value actually changed, not just that the
	// store compiled.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.empty;\nc = Choice.value(5);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 1, false)
}

func TestEmitUnionBoolPayloadCompilesAndRuns(t *testing.T) {
	// A bool payload variant (Flag.on(true)): the union member is declared bool
	// and the payload expression builds under the bool grammar. The on case
	// fires and the exit code is 1.
	emitAndRun(t, "type Flag = union enum { off void; on bool; }; fn main() i32 {\nvar f Flag = Flag.on(true);\nswitch f { case Flag.off: return 0; case Flag.on: return 1; }\n}", false, 1, false)
}

func TestEmitUnionTwoPayloadVariantsCompilesAndRuns(t *testing.T) {
	// A tagged union with two non-void variants whose payload types differ (one
	// i32, one bool): both union members are declared (int32_t and bool), the
	// construction of each names its own member, and the discriminant alone
	// selects the correct case. Constructing the wide variant and switching
	// fires the wide case (exit 1), independent of the big variant's payload
	// member.
	emitAndRun(t, "type Shape = union enum { empty void; wide i32; big bool; }; fn main() i32 {\nvar a Shape = Shape.wide(42);\nvar b Shape = Shape.big(false);\nswitch a { case Shape.empty: return 0; case Shape.wide: return 1; case Shape.big: return 2; }\n}", false, 1, false)
}

func TestEmitUnionTwoPayloadVariantsOtherCaseCompilesAndRuns(t *testing.T) {
	// Same two-payload-variant union, switching on the bool-constructed local:
	// the big case fires (exit 2), proving the discriminant dispatch reaches
	// the other payload-carrying variant too.
	emitAndRun(t, "type Shape = union enum { empty void; wide i32; big bool; }; fn main() i32 {\nvar a Shape = Shape.wide(42);\nvar b Shape = Shape.big(true);\nswitch b { case Shape.empty: return 0; case Shape.wide: return 1; case Shape.big: return 2; }\n}", false, 2, false)
}

func TestEmitUnionVariantLiteralSwitchSubjectCompilesAndRuns(t *testing.T) {
	// A variant construction used directly as the switch subject (switch
	// Choice.value(5)) — confirmed checker-reachable — is built as the union's
	// compound literal and its .tag field read, so the value case fires.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nswitch Choice.value(5) { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 1, false)
}

func TestEmitUnionPayloadRoundTripsThroughConstruction(t *testing.T) {
	// No syntax in the language reads a tagged-union payload back out (a switch
	// case value is a bare expression — there is no pattern-binding syntax, so
	// a case can only ever match by discriminant), so the payload's round-trip
	// is observed through the only channel that exists: construction itself. An
	// anchored payload expression that overflows must be evaluated exactly once
	// and stored at construction time — if the backend dropped or mis-lowered
	// the payload, the overflow would never fire. Constructing
	// Choice.value(x + 1) with x = INT32_MAX must abort at construction, which
	// proves the payload value is genuinely evaluated and stored even though
	// the language cannot read it back.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nlet x i32 = 2147483647;\nvar c Choice = Choice.value(x + 1);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 0, true)
}

func TestEmitUnionAllVoidVariantsCompilesAndRuns(t *testing.T) {
	// A tagged union every variant of which is payload-less (union enum { a
	// void; b void; } — legal per the grammar, since `union enum` merely marks
	// the tagged form) reaches this backend with no payload-carrying
	// construction anywhere, so nothing in 10.34's plain-enum code path changes
	// for it: the type is emitted as a plain enum typedef exactly like an
	// `enum` declaration, and the switch dispatches on the bare enum value.
	// This test proves the all-void union case needs no new implementation.
	emitAndRun(t, "type Empty = union enum { a void; b void; }; fn main() i32 {\nvar e Empty = Empty.b;\nswitch e { case Empty.a: return 0; case Empty.b: return 1; }\n}", false, 1, false)
}

func TestEmitUnionSwitchInHelperCompilesAndRuns(t *testing.T) {
	// A tagged-union local and switch inside a reachable helper, the entry
	// calling the helper: collectUnionTypes walks every reachable helper's
	// body, so the union's typedef pair is discovered and emitted even when no
	// reachable *entry* statement constructs a payload variant.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn pick() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}\nfn main() i32 { return pick(); }", false, 1, false)
}

func TestEmitUnionLocalUnusedCompilesClean(t *testing.T) {
	// A tagged-union local declared and never referenced after its declaration
	// still compiles clean under -Wall -Wextra -Werror: the emitted declaration
	// is followed by the same (void) cast every other local gets, so the strict
	// build never warns about an unused variable.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nreturn 3;\n}", false, 3, false)
}

func TestEmitUnionWritesC(t *testing.T) {
	// Confirm the emitted C directly for one fixture: the tag-enum typedef
	// (one constant per variant in declared order), the tagged-struct typedef
	// with its union member(s) named pebble_field_<member> from each
	// constructed variant's symbol, the local declaration initializing from
	// the construction's compound literal (tag + designated payload member),
	// and the switch subject reading the .tag field.
	unit, snapshot, entryID, unionType, variants, sources := unionFixture(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}")
	if len(variants) != 2 {
		t.Fatalf("fixture has %d variants, want 2 (empty, value)", len(variants))
	}
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	tagEnum := "typedef enum {\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[0])) + ",\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[1])) + ",\n" +
		"} " + enumTypeName(unionType) + ";"
	if !strings.Contains(out, tagEnum) {
		t.Errorf("emitted C is missing the tag enum typedef %q:\n%s", tagEnum, out)
	}
	unionStruct := "typedef struct {\n" +
		"    " + enumTypeName(unionType) + " tag;\n" +
		"    union {\n" +
		"        int32_t pebble_field_" + strconv.Itoa(int(variants[1])) + ";\n" +
		"    } payload;\n" +
		"} " + unionTypeName(unionType) + ";"
	if !strings.Contains(out, unionStruct) {
		t.Errorf("emitted C is missing the tagged-struct typedef %q:\n%s", unionStruct, out)
	}
	for _, want := range []string{
		unionTypeName(unionType) + " pebble_local_",
		"(pebble_union_" + strconv.Itoa(int(unionType)) + "_t){ .tag = pebble_variant_" + strconv.Itoa(int(variants[1])) + ", .payload = { .pebble_field_" + strconv.Itoa(int(variants[1])) + " = 5 } }",
		"switch (pebble_local_",
		".tag) {",
		"case pebble_variant_" + strconv.Itoa(int(variants[0])) + ":",
		"case pebble_variant_" + strconv.Itoa(int(variants[1])) + ":",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitRejectsNonScalarUnionPayload(t *testing.T) {
	// A tagged-union payload that is not exactly the entry's resolved width or
	// bool — a tuple, struct, array, optional, str, or nested enum — is
	// reachable from real source (the checker accepts such a variant
	// declaration and construction) but is a clean rejection naming what is
	// unsupported, never guessed at. The rejection happens in the union-type
	// collection walk, where each constructed variant's payload type is first
	// resolved from its construction site.
	emitAndRunRejects(t, "type C = union enum { empty void; value (i32, i32); }; fn main() i32 {\nvar c C = C.value((1, 2));\nreturn 0;\n}", "carries a payload of type (int, int); only a payload of i32 or bool is supported")
}

func emitAndRunRejects(t *testing.T, sourceText, wantSubstring string) {
	t.Helper()
	unit, snapshot, entryID, _ := buildFixture(t, sourceText, "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, wantSubstring)
}

func TestEmitEnumSwitchInHelperCompilesAndRuns(t *testing.T) {
	// A plain enum local and switch inside a reachable helper, the entry
	// calling the helper: collectEnumTypes walks every reachable helper's body,
	// so the enum typedef is discovered and emitted even when no reachable
	// *entry* statement references the enum type. The helper's switch on its
	// own enum local fires the green case and returns 1.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn pick() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}\nfn main() i32 { return pick(); }", false, 1, false)
}

func TestEmitEnumLocalInLoopBodyCompilesAndRuns(t *testing.T) {
	// An enum-typed local declared inside a while loop body, reassigned and
	// compared there, and accumulated: the enum dispatch routes through
	// buildLeadingStatement from buildLoopBody exactly as a scalar local does.
	// i = 0 and i = 1 leave c as green, each adding 1 (n = 2); i = 2 reassigns
	// c to red, whose equality comparison is true and adds 10 (n = 12). Bounded
	// execution because the loop's own condition is the only bound.
	emitAndRunBounded(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 0;\nvar i i32 = 0;\nwhile i < 3 {\nvar c Color = Color.green;\nif i == 2 { c = Color.red; }\nif c == Color.red { n = n + 10; } else { n = n + 1; }\ni = i + 1;\n}\nreturn n;\n}", false, 12, false)
}

func TestEmitSliceBothBoundsCompilesAndRuns(t *testing.T) {
	// Slice from array with both bounds explicit: a[1:3] from [1,2,3,4,5]
	// gives elements [2,3]; s[0] should be 2.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s[0]; }", false, 2, false)
}

func TestEmitSliceStartOnlyCompilesAndRuns(t *testing.T) {
	// Slice with only start bound: a[2:] from [10,20,30,40,50] gives
	// elements [30,40,50]; s[0] should be 30.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [10, 20, 30, 40, 50]; var s []i32 = a[2:]; return s[0]; }", false, 30, false)
}

func TestEmitSliceEndOnlyCompilesAndRuns(t *testing.T) {
	// Slice with only end bound: a[:3] from [10,20,30,40,50] gives
	// elements [10,20,30]; s[0] should be 10.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [10, 20, 30, 40, 50]; var s []i32 = a[:3]; return s[0]; }", false, 10, false)
}

func TestEmitSliceNoBoundsCompilesAndRuns(t *testing.T) {
	// Slice with no bounds: a[:] from [10,20,30,40,50] gives all 5 elements;
	// s[2] should be 30.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [10, 20, 30, 40, 50]; var s []i32 = a[:]; return s[2]; }", false, 30, false)
}

func TestEmitSliceBoolElementCompilesAndRuns(t *testing.T) {
	// Bool-element slice: a[1:3] from [true, false, true, false] gives
	// [false, true]; s[0] is false, so if s[0] { return 1 } else { return 0 }
	// returns 0; s[1] is true, so if s[1] { return 1 } else { return 0 }
	// returns 1. Use the slice in an expression that drives the return.
	emitAndRun(t, "fn main() i32 { var a [4]bool = [true, false, true, false]; var s []bool = a[1:3]; if s[1] { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitSliceI64CompilesAndRuns(t *testing.T) {
	// i64-entry slice: a[1:3] from [100,200,300,400,500] gives [200,300];
	// s[0] should be 200.
	emitAndRun(t, "fn main() i64 { var a [5]i64 = [100, 200, 300, 400, 500]; var s []i64 = a[1:3]; return s[0]; }", false, 200, false)
}

func TestEmitSliceOutOfBoundsRangeAborts(t *testing.T) {
	// Out-of-range slice end bound: use a helper to supply a runtime end
	// value that exceeds the array length, bypassing the checker's
	// compile-time validation. pebble_rt_checked_slice_start_i32 must panic.
	emitAndRun(t, "fn getEnd() i32 { return 10; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var e i32 = getEnd(); var s []i32 = a[0:e]; return s[0]; }", false, 0, true)
}

func TestEmitSliceIndexOutOfBoundsAborts(t *testing.T) {
	// Out-of-range index into a valid slice: a[1:3] gives 2 elements [2,3];
	// s[5] is out of bounds, triggering pebble_rt_checked_index_i32.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s[5]; }", false, 0, true)
}

func TestEmitSliceRangeOutOfBoundsEmitsRealSourceLoc(t *testing.T) {
	// Since 10.44, the checked slice-range construction call also carries a
	// real, resolved Pebble source location (the CheckedSlice node's own
	// Span) instead of the zero-valued placeholder. This proves both that the
	// emitted C no longer uses the placeholder anywhere and that the runtime
	// still aborts on an invalid range (end bound past the array length).
	unit, snapshot, entryID, sources := buildFixture(t, "fn getEnd() i32 { return 10; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var e i32 = getEnd(); var s []i32 = a[0:e]; return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_slice_start_i32(") {
		t.Errorf("emitted C missing the checked slice-start call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
}

func TestEmitSliceIndexOutOfBoundsEmitsRealSourceLoc(t *testing.T) {
	// Since 10.44, an out-of-bounds read through a slice (s[5] on a 2-element
	// slice) also carries a real, resolved Pebble source location on its
	// checked-index call instead of the zero-valued placeholder.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s[5]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_index_i32(") {
		t.Errorf("emitted C missing the checked-index call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
}

func TestEmitSliceEmittedCDirectly(t *testing.T) {
	// Confirm the emitted C directly: slice typedef shape, construction
	// compound-literal text (including inline checked-start call), and
	// indexing expression (including inline checked-index call and .len cast).
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// Slice typedef must be present.
	if !strings.Contains(out, "typedef struct {") {
		t.Errorf("emitted C missing slice typedef:\n%s", out)
	}
	if !strings.Contains(out, "int32_t *data;") {
		t.Errorf("emitted C missing data field in slice typedef:\n%s", out)
	}
	if !strings.Contains(out, "size_t len;") {
		t.Errorf("emitted C missing len field in slice typedef:\n%s", out)
	}
	// Construction must contain pebble_rt_checked_slice_start_i32.
	if !strings.Contains(out, "pebble_rt_checked_slice_start_i32") {
		t.Errorf("emitted C missing checked-slice-start call:\n%s", out)
	}
	// Indexing must contain pebble_rt_checked_index_i32 and .data and .len.
	if !strings.Contains(out, "pebble_rt_checked_index_i32") {
		t.Errorf("emitted C missing checked-index call for slice indexing:\n%s", out)
	}
	if !strings.Contains(out, ".data[") {
		t.Errorf("emitted C missing .data subscript for slice indexing:\n%s", out)
	}
	if !strings.Contains(out, ".len") {
		t.Errorf("emitted C missing .len in slice indexing:\n%s", out)
	}
	// Run end-to-end to confirm correctness.
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitSliceUnsupportedElementTypeRejects(t *testing.T) {
	// A slice of tuple elements is unsupported — the checker builds it but
	// the backend must reject it cleanly.
	emitAndRunRejects(t, "fn main() i32 { var a [3](i32, i32) = [(1, 2), (3, 4), (5, 6)]; var s [](i32, i32) = a[0:2]; return s[0].0; }", "slice element type")
}

// 10.38 — slice-typed function parameters and return values

func TestEmitSliceParameterCompilesAndRuns(t *testing.T) {
	// The flagship slice-parameter fixture: first takes a []i32 parameter and
	// indexes it inside the helper; the entry slices an array into a slice
	// local and passes that local. s = a[1:3] = [2,3], so s[0] = 2 is the exit
	// code. The parameter seeds the callee's scope as a slice local and the
	// index resolves through the same Load(CheckedIndexPlace) machinery a
	// slice local uses.
	emitAndRun(t, "fn first(s []i32) i32 { return s[0]; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return first(s); }", false, 2, false)
}

func TestEmitSliceParameterBoolElementCompilesAndRuns(t *testing.T) {
	// A bool-element slice parameter: the element-typed index read routes
	// through the slice's bool element and drives the return. s = a[1:3] =
	// [false, true], so s[0] is false and the else arm exits 0.
	emitAndRun(t, "fn first(s []bool) i32 { if s[0] { return 1; } else { return 0; } } fn main() i32 { var a [4]bool = [true, false, true, false]; var s []bool = a[1:3]; return first(s); }", false, 0, false)
}

func TestEmitSliceParameterI64CompilesAndRuns(t *testing.T) {
	// The width-generic path holds for slice parameters too, mirroring 10.37's
	// own i64 test: an i64 entry calls an i64 slice-taking helper whose slice
	// parameter seeds the callee's scope; s = a[1:3] = [200,300], s[0] = 200 is
	// the exit code. The parameter's C type is pebble_slice_<id>_t with an
	// int64_t* data field.
	emitAndRun(t, "fn first(s []i64) i64 { return s[0]; } fn main() i64 { var a [5]i64 = [100, 200, 300, 400, 500]; var s []i64 = a[1:3]; return first(s); }", false, 200, false)
}

func TestEmitSliceReturningHelperInlineConstructionCompilesAndRuns(t *testing.T) {
	// The flagship slice-return fixture: view's tail return constructs the
	// slice inline (`return a[1:3];` — the Return child is a bare CheckedSlice,
	// confirmed against a real fixture), so the return needs the same
	// two-statement temp-then-construction shape a slice local's declaration
	// uses. The caller declares a slice local from the call (the supported
	// position) and indexes the result: view() = [2,3], s[0] = 2 is the exit
	// code. This confirms the two-statement return construction actually works
	// at runtime, not just that it compiles.
	emitAndRun(t, "fn view() []i32 { var a [5]i32 = [1, 2, 3, 4, 5]; return a[1:3]; } fn main() i32 { var s []i32 = view(); return s[0]; }", false, 2, false)
}

func TestEmitSliceReturningHelperForwardsParameterCompilesAndRuns(t *testing.T) {
	// A slice-returning helper forwarding its slice-typed parameter unchanged
	// (`return s;` — a plain SymbolValue return, the single-statement path):
	// echo passes its parameter back, the entry declares a slice local from the
	// call and indexes it. echo(s) = [2,3], t[0] = 2 is the exit code.
	emitAndRun(t, "fn echo(s []i32) []i32 { return s; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; var t []i32 = echo(s); return t[0]; }", false, 2, false)
}

func TestEmitSliceReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// The local side of forwarding an already-declared slice value: g declares
	// its own array and slice local and `return s;` forwards the local (a
	// plain SymbolValue), emitting `return pebble_local_<s>;`. The entry
	// assigns the call to a matching slice local and indexes it: g() = [2,3],
	// t[0] = 2 is the exit code.
	emitAndRun(t, "fn g() []i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s; } fn main() i32 { var t []i32 = g(); return t[0]; }", false, 2, false)
}

func TestEmitSliceReturningHelperI64CompilesAndRuns(t *testing.T) {
	// The i64 side of the return-side construction: the return temp must be
	// declared at int64_t (a width bug in exactly this spot was found and fixed
	// during 10.37's review), and the caller indexes the returned slice.
	// view() = [200,300], s[0] = 200 is the exit code.
	emitAndRun(t, "fn view() []i64 { var a [5]i64 = [100, 200, 300, 400, 500]; return a[1:3]; } fn main() i64 { var s []i64 = view(); return s[0]; }", false, 200, false)
}

func TestEmitSliceReturningHelperIfElseTailsCompilesAndRuns(t *testing.T) {
	// Two slice-construction returns in the two arms of an if/else tail: each
	// arm's Return child is a bare CheckedSlice, each built with its own temp
	// (named from the return value node's NodeID, so the two sibling-block
	// temps never collide even though they are the same slice type). With the
	// flag true the then-arm wins: a[0:2] = [1,2], s[0] = 1 is the exit code.
	emitAndRun(t, "fn pick(b bool) []i32 { if b { var a [3]i32 = [1, 2, 3]; return a[0:2]; } else { var a [3]i32 = [4, 5, 6]; return a[1:2]; } } fn main() i32 { var s []i32 = pick(true); return s[0]; }", false, 1, false)
}

func TestEmitSliceReturningHelperSwitchCasesCompilesAndRuns(t *testing.T) {
	// A slice-returning helper whose body tail is a switch whose case bodies
	// are bare single-statement returns of fresh slice constructions: each case
	// body routes through buildSwitchCaseBody's bare-Return slice path and
	// emits its own temp-then-return pair, each temp named from its own return
	// value node's NodeID. With the subject 1 the case-1 body wins: a[1:2] =
	// [2], s[0] = 2 is the exit code.
	emitAndRun(t, "fn pick(i i32) []i32 { var a [3]i32 = [1, 2, 3]; switch i { case 0: return a[0:1]; case 1: return a[1:2]; else: return a[2:3]; } } fn main() i32 { var s []i32 = pick(1); return s[0]; }", false, 2, false)
}

func TestEmitSliceReturningHelperWritesC(t *testing.T) {
	// The emitted C for the inline-construction-return fixture: the helper's C
	// signature declares its return type as the slice typedef, its body emits
	// the two-statement shape (a temp declaration holding the checked-start
	// result, then the return of the compound-literal construction using that
	// temp for both .data and .len), and the call site declares the entry's
	// slice local directly from the call. Symbols 24 (view), 26 (its a array
	// local), 27 (the entry's s local), return value node 18, and slice type 23
	// come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn view() []i32 { var a [5]i32 = [1, 2, 3, 4, 5]; return a[1:3]; } fn main() i32 { var s []i32 = view(); return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t *data;\n    size_t len;\n} pebble_slice_23_t;",
		"static pebble_slice_23_t pebble_fn_24(PebbleContext *ctx) {",
		"int32_t pebble_slice_ret_18 = pebble_rt_checked_slice_start_i32(1, 3, 5, (PebbleSourceLoc){\"main.peb\"",
		"return (pebble_slice_23_t){ .data = pebble_local_26 + pebble_slice_ret_18, .len = (size_t)(3 - pebble_slice_ret_18) };",
		"pebble_slice_23_t pebble_local_27 = pebble_fn_24(ctx);",
		"return pebble_local_27.data[pebble_rt_checked_index_i32(0, (int32_t)pebble_local_27.len, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitSliceReturningHelperI64WritesC(t *testing.T) {
	// The i64 counterpart of the return construction, confirming specifically
	// that the return-side temp is declared at int64_t (the exact spot where a
	// width bug was found and fixed during 10.37's review) and then running the
	// emitted C end-to-end.
	unit, snapshot, entryID, sources := buildFixture(t, "fn view() []i64 { var a [5]i64 = [100, 200, 300, 400, 500]; return a[1:3]; } fn main() i64 { var s []i64 = view(); return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int64_t *data;",
		"int64_t pebble_slice_ret_18 = pebble_rt_checked_slice_start_i64(1, 3, 5, (PebbleSourceLoc){\"main.peb\"",
		"return (pebble_slice_23_t){ .data = pebble_local_26 + pebble_slice_ret_18, .len = (size_t)(3 - pebble_slice_ret_18) };",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 200, false)
}

func TestEmitSliceParameterWritesC(t *testing.T) {
	// The parameter C type for a slice-taking helper: the C signature declares
	// the parameter as the slice type's own struct typedef (the same
	// pebble_slice_<typeID>_t 10.37's local declaration builds, no new typedef
	// shape) with the pebble_local_<symbol> naming every parameter uses, plus
	// the (void) cast every parameter gets, and the call site passes the
	// slice-typed local's own C name. Symbols 24 (first), 25 (its s parameter),
	// 28 (the entry's s local), and slice type 23 come from the real fixture
	// dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn first(s []i32) i32 { return s[0]; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return first(s); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx, pebble_slice_23_t pebble_local_25) {",
		"    (void)pebble_local_25;",
		"return pebble_local_25.data[pebble_rt_checked_index_i32(0, (int32_t)pebble_local_25.len, (PebbleSourceLoc){\"main.peb\"",
		"return pebble_fn_24(ctx, pebble_local_28);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitRejectsSliceConstructionAsCallArgument(t *testing.T) {
	// An inline slice construction used directly as a call argument (f(a[1:3]))
	// is confirmed checker-reachable (the DirectCall's child is a bare
	// CheckedSlice) but deliberately out of scope: a C function argument is a
	// pure expression position with nowhere to place the temp-declaration
	// statement the construction needs, so it is a clean rejection naming what
	// was found — not a GNU statement-expression workaround.
	unit, snapshot, entryID, _ := buildFixture(t, "fn f(x []i32) i32 { return x[0]; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; return f(a[1:3]); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "inline slice construction")
}

func TestEmitRejectsSliceParameterUnsupportedElementType(t *testing.T) {
	// A slice-typed parameter whose element type is not the entry's width or
	// bool is a clean rejection from validateHelperSignature. A []str parameter
	// is checker-reachable but not constructible from real source (a str array
	// is itself rejected by the array element gate before any slice of it could
	// reach a call site), so this is hand-built through the IR builder to
	// exercise the gate directly: helper symbol 24 takes one []str parameter
	// (its type borrowed from a real checker-built fixture) and main calls it,
	// so the reachability walk hits the gate before any body is built.
	unit, snapshot, entryID := buildSliceOfStrParameterUnit(t)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "unsupported element type")
}

// buildSliceOfStrParameterUnit hand-builds a unit whose i32 entry calls a
// helper (symbol 24) that declares one []str parameter. The []str type is
// borrowed from a real checker-built fixture (fn f(x []str) i32, which the
// checker accepts even though the backend rejects the slice-of-str element
// type); the unit is otherwise the same shape buildCallArgumentCountMismatchUnit
// builds, so Emit's reachability walk validates the helper's signature and
// rejects the unsupported element type before building any body.
func buildSliceOfStrParameterUnit(t *testing.T) (*tir.Unit, *types.Snapshot, symbol.SymbolID) {
	t.Helper()
	realUnit, snapshot, entryID, _ := buildFixture(t, "fn f(x []str) i32 { return 0; } fn main() i32 { return 0; }", "main", false)
	var strSlice types.TypeID
	for _, n := range realUnit.Nodes() {
		if n.Kind == tir.FunctionDeclaration && len(n.Parameters) == 1 {
			strSlice = n.Parameters[0].Type
			break
		}
	}
	if strSlice == 0 {
		t.Fatal("checker-built fixture has no []str parameter to borrow its type from")
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
		Parameters: []tir.Parameter{{Symbol: 25, Type: strSlice}},
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

func TestEmitArrayElementWriteCompilesAndRuns(t *testing.T) {
	// The flagship array element write: a[2] = 99 replaces the middle slot of
	// [1,2,3,4,5], and the sum of all five slots read back (1 + 2 + 99 + 4 + 5
	// = 111) confirms the write landed at exactly the indexed slot and
	// clobbered nothing else — not just that the program compiled.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; a[2] = 99; return a[0] + a[1] + a[2] + a[3] + a[4]; }", false, 111, false)
}

func TestEmitSliceElementWriteCompilesAndRuns(t *testing.T) {
	// The flagship slice element write: s = a[1:3] = [2, 3], s[0] = 9 replaces
	// the first slot of the slice's view, and s[0] + s[1] = 9 + 3 = 12 read
	// back confirms the write through the slice actually changed the underlying
	// element the slice views.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; s[0] = 9; return s[0] + s[1]; }", false, 12, false)
}

func TestEmitSliceParameterElementWriteObservedByCaller(t *testing.T) {
	// A slice-typed parameter's element written inside the helper, observed by
	// the caller after the call returns: set writes s[0] = 9 where the caller
	// passed s = a[1:3], so the write lands in a[1] (the slice's first slot is
	// the caller's array's second slot — a slice is a non-owning view). The
	// caller then reads its OWN array local a[1] and sees 9. This is the real
	// proof the write machinery is correct — the mutation reached the same
	// backing array the caller owns, not just some helper-local copy.
	emitAndRun(t, "fn set(s []i32) void { s[0] = 9; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; set(s); return a[1]; }", false, 9, false)
}

func TestEmitArrayBoolElementWriteCompilesAndRuns(t *testing.T) {
	// A bool-element array write: a[0] = true replaces a false slot, and the
	// element read back drives an if condition — exit 1 proves the bool write
	// landed and read back correctly (not just that it compiled).
	emitAndRun(t, "fn main() i32 { var a [2]bool = [false, false]; a[0] = true; if a[0] { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitSliceBoolElementWriteCompilesAndRuns(t *testing.T) {
	// A bool-element slice write: s = a[1:3] = [false, true], s[0] = true
	// replaces the view's first (false) slot, and reading s[0] back drives an
	// if condition — exit 1 proves the bool write through the slice landed.
	emitAndRun(t, "fn main() i32 { var a [4]bool = [true, false, true, false]; var s []bool = a[1:3]; s[0] = true; if s[0] { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitI64ArrayElementWriteCompilesAndRuns(t *testing.T) {
	// The width-generic path holds for writes too: an i64 entry writes an i64
	// array element (the lvalue lowers through pebble_rt_checked_index_i64 and
	// the RHS through buildExpr at i64), and a[1] = 21 read back is the exit
	// code.
	emitAndRun(t, "fn main() i64 { var a [2]i64 = [20, 22]; a[1] = 21; return a[1]; }", false, 21, false)
}

func TestEmitArrayElementWriteOutOfBoundsAborts(t *testing.T) {
	// An out-of-bounds array element WRITE: a[i] = 9 with a runtime i = 5 on a
	// [2]i32 array must panic through the exact same pebble_rt_checked_index_i32
	// call the read side uses — the lvalue text is identical either way, so
	// the write's bounds check fires at runtime (the runtime index bypasses the
	// checker's compile-time validation), not just a compile-time rejection.
	emitAndRun(t, "fn main() i32 { var a [2]i32 = [10, 20]; var i i32 = 5; a[i] = 9; return a[0]; }", false, 0, true)
}

func TestEmitSliceElementWriteOutOfBoundsAborts(t *testing.T) {
	// An out-of-bounds slice element WRITE: s = a[1:3] has len 2, and s[i] = 9
	// with a runtime i = 5 must panic through
	// pebble_rt_checked_index_i32(i, (int32_t)s.len) at runtime.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; var i i32 = 5; s[i] = 9; return s[0]; }", false, 0, true)
}

func TestEmitArrayElementWriteWritesC(t *testing.T) {
	// The emitted C for an array element write: the Store lowers to a plain
	// assignment expression whose lvalue is the exact bounds-checked subscript
	// buildPlaceLValue's CheckedIndexPlace case produces for an array base —
	// pebble_local_25[pebble_rt_checked_index_i32(0, 5)] = 9; — with no new
	// bounds-check call site. Symbols 25 (the array local a) come from the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; a[0] = 9; return a[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_25[5] = { 1, 2, 3, 4, 5 };",
		"pebble_local_25[pebble_rt_checked_index_i32(0, 5, (PebbleSourceLoc){\"main.peb\"",
		"return pebble_local_25[pebble_rt_checked_index_i32(0, 5, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 9, false)
}

func TestEmitSliceElementWriteWritesC(t *testing.T) {
	// The emitted C for a slice element write: the Store lowers to a plain
	// assignment expression whose lvalue is the exact bounds-checked .data
	// subscript buildPlaceLValue's CheckedIndexPlace case produces for a slice
	// base — pebble_local_26.data[pebble_rt_checked_index_i32(0,
	// (int32_t)pebble_local_26.len)] = 9; — the .len bound checked against the
	// slice's own length. Symbols 25 (array a), 26 (slice s) come from the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; s[0] = 9; return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_slice_start_26 = pebble_rt_checked_slice_start_i32(1, 3, 5, (PebbleSourceLoc){\"main.peb\"",
		"pebble_local_26.data[pebble_rt_checked_index_i32(0, (int32_t)pebble_local_26.len, (PebbleSourceLoc){\"main.peb\"",
		"return pebble_local_26.data[pebble_rt_checked_index_i32(0, (int32_t)pebble_local_26.len, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 9, false)
}

func TestEmitCharLocalEqualityTrueCompilesAndRuns(t *testing.T) {
	// A char-typed local declared from a literal, compared for equality,
	// driving a return value — the true outcome: c and d both hold 'a', so
	// c == d and the process exits 1. Exercises the full char path: a
	// CharLiteral local declaration, two char local references as == operands,
	// and the equality feeding a tail return.
	emitAndRun(t, "fn main() i32 { let c char = 'a'; let d char = 'a'; if c == d { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharLocalEqualityFalseCompilesAndRuns(t *testing.T) {
	// The false outcome of the equality fixture: d holds 'b' instead of 'a',
	// so c == d is false and the process exits 0 — proving the comparison
	// actually distinguishes the two scalar values rather than always being
	// true.
	emitAndRun(t, "fn main() i32 { let c char = 'a'; let d char = 'b'; if c == d { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitCharNotEqualCompilesAndRuns(t *testing.T) {
	// The != comparison between two char values: c='a' and d='b' differ, so
	// c != d is true and the process exits 1.
	emitAndRun(t, "fn main() i32 { let c char = 'a'; let d char = 'b'; if c != d { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharNonAsciiEqualityCompilesAndRuns(t *testing.T) {
	// A char-typed local declared from a non-ASCII literal — the accented
	// letter 'é' (U+00E9, 233) — compared for equality against the same
	// literal, proving the full Unicode scalar value round-trips through the
	// int32_t emission and back, not just an ASCII slice of it.
	emitAndRun(t, "fn main() i32 { let c char = 'é'; if c == 'é' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharEmojiEqualityCompilesAndRuns(t *testing.T) {
	// A char-typed local declared from an emoji — '😀' (U+1F600, 128512), a
	// value that needs more than a byte to represent — compared for equality.
	// This proves the full 21-bit Unicode scalar value round-trips, not just a
	// truncated low byte (which would collide with a different code point).
	emitAndRun(t, "fn main() i32 { let c char = '😀'; if c == '😀' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharReassignmentCompilesAndRuns(t *testing.T) {
	// A char-typed reassignment: c is declared var 'a', reassigned to 'b',
	// then compared against 'b' — the process exits 1 only if the reassignment
	// actually changed the stored int32_t value.
	emitAndRun(t, "fn main() i32 { var c char = 'a'; c = 'b'; if c == 'b' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharReassignmentFromLocalCompilesAndRuns(t *testing.T) {
	// A char-typed reassignment from another char-typed local: a holds 'a', b
	// holds 'b', a = b copies b's scalar value into a, and comparing a against
	// 'b' afterwards proves the copy landed — the char-typed local reference
	// is a valid reassignment right-hand side.
	emitAndRun(t, "fn main() i32 { var a char = 'a'; let b char = 'b'; a = b; if a == 'b' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharOrderingCompilesAndRuns(t *testing.T) {
	// An ordering comparison between two char values: c holds 'a' (97) and d
	// holds 'b' (98), so c < d is true and the process exits 1. Comparing
	// Unicode scalar values numerically is well-defined, and the checker
	// accepts ordering comparisons between chars (confirmed against a real
	// fixture), so the plain C operator is the correct lowering.
	emitAndRun(t, "fn main() i32 { let c char = 'a'; let d char = 'b'; if c < d { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharOrderingFalseCompilesAndRuns(t *testing.T) {
	// The false outcome of the char ordering fixture: 'b' < 'a' is false, so
	// the process exits 0 — proving the ordering distinguishes the two scalar
	// values in the correct direction.
	emitAndRun(t, "fn main() i32 { let c char = 'b'; let d char = 'a'; if c < d { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitCharLocalFromLocalCompilesAndRuns(t *testing.T) {
	// A char-typed local declared from a char-typed local reference: b is
	// declared from a (confirmed checker-reachable against a real fixture), so
	// b holds 'a' and the comparison is true. This exercises the SymbolValue
	// initializer shape a char local's declaration accepts.
	emitAndRun(t, "fn main() i32 { let a char = 'a'; let b char = a; if b == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharParameterAndResultCompilesAndRuns(t *testing.T) {
	// A char-typed parameter and result, called and compared: f takes a char,
	// forwards it as its char result, and main declares c from the call then
	// compares it against 'a' — proving the char value survives the
	// helper-call round trip at both the C int32_t parameter and return type.
	emitAndRun(t, "fn f(c char) char { return c; } fn main() i32 { let c char = f('a'); if c == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharParameterAndResultDistinctCompilesAndRuns(t *testing.T) {
	// The false outcome of the parameter/result fixture: f('b') returns 'b',
	// compared against 'a', so the process exits 0 — the value that survives
	// the call round trip is the argument, not a fixed constant.
	emitAndRun(t, "fn f(c char) char { return c; } fn main() i32 { let c char = f('b'); if c == 'a' { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitCharReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// A char-returning helper whose result is forwarded through its own local:
	// f declares x from its char parameter and returns x, proving a char local
	// inside a helper and a char return value both build correctly, and main
	// compares the surviving value.
	emitAndRun(t, "fn f(c char) char { var x char = c; return x; } fn main() i32 { let c char = f('a'); if c == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharCallArgumentLocalCompilesAndRuns(t *testing.T) {
	// A char-typed local passed as a call argument: main declares c from a
	// literal and passes it to f, which compares it against 'a' — the
	// char-typed local reference is a valid call-site argument for a char
	// parameter.
	emitAndRun(t, "fn f(x char) i32 { if x == 'a' { return 1; } else { return 0; } } fn main() i32 { let c char = 'a'; return f(c); }", false, 1, false)
}

func TestEmitCharCallArgumentLiteralCompilesAndRuns(t *testing.T) {
	// A char literal passed directly as a call argument (f('a')), no
	// intermediate local — the CharLiteral shape a char parameter accepts at
	// the call site.
	emitAndRun(t, "fn f(x char) i32 { if x == 'a' { return 1; } else { return 0; } } fn main() i32 { return f('a'); }", false, 1, false)
}

func TestEmitCharLocalFromCallCompilesAndRuns(t *testing.T) {
	// A char-typed local declared from a call to a char-returning helper
	// (confirmed checker-reachable against a real fixture): c is declared from
	// f('a') and compared, proving the DirectCall initializer shape works for
	// a char local.
	emitAndRun(t, "fn f(x char) char { return x; } fn main() i32 { let c char = f('a'); if c == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharI64EntryCompilesAndRuns(t *testing.T) {
	// A char-typed local inside an i64 entry: the entry's integer width picks
	// i64 arithmetic, but a char is still the fixed int32_t (the two are
	// unrelated concepts), so this confirms the char grammar is independent of
	// the entry's width.
	emitAndRun(t, "fn main() i64 { let c char = 'a'; if c == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharInLoopBodyCompilesAndRuns(t *testing.T) {
	// A char-typed local declared and compared inside a while-loop body: the
	// loop runs three passes, each declaring c='a' and summing 1 when c == 'a',
	// exiting with 3 — proving char locals work through the loop-body
	// leading-statement and condition paths.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 3 { let c char = 'a'; if c == 'a' { sum = sum + 1; } i = i + 1; } return sum; }", false, 3, false)
}

func TestEmitCharWritesC(t *testing.T) {
	// The emitted C for the char fixture: the char local is declared with the
	// fixed int32_t type and its literal emitted as an int32_t constant
	// ((int32_t)97 for 'a', (int32_t)98 for 'b'), the helper's parameter and
	// return type are both int32_t, the reassignment stores the call result,
	// and the comparison emits the plain C == operator between the two
	// int32_t operands. Symbols 24 (f), 25 (f's c parameter), 26 (main), 27
	// (main's c local) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn f(c char) char { return c; } fn main() i32 { var c char = 'a'; c = f('b'); if c == 'b' { return 1; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx, int32_t pebble_local_25) {",
		"int32_t pebble_local_27 = (int32_t)97;",
		"pebble_local_27 = pebble_fn_24(ctx, (int32_t)98);",
		"if (pebble_local_27 == (int32_t)98) {",
		"return pebble_local_25;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 1, false)
}

func TestEmitPointerAddressOfAndDerefCompilesAndRuns(t *testing.T) {
	// var y i32 = 5; let p *i32 = &y; return *p;
	// Takes the address of a local and dereferences it, proving the pointer
	// round-trip works end-to-end.
	emitAndRun(t, "fn main() i32 { var y i32 = 5; let p *i32 = &y; return *p; }", false, 5, false)
}

func TestEmitWriteThroughPointerCompilesAndRuns(t *testing.T) {
	// var y i32 = 5; let p *i32 = &y; *p = 9; return y;
	// Writes through a pointer and reads back through the original variable,
	// proving the pointer genuinely aliases y, not a copy.
	emitAndRun(t, "fn main() i32 { var y i32 = 5; let p *i32 = &y; *p = 9; return y; }", false, 9, false)
}

func TestEmitPointerReceiverSliceIndexAddressOfCompilesAndRuns(t *testing.T) {
	// The returned pointer must alias the backing slice in the original struct:
	// get(1) returns &self.data[1], and the caller mutates that element through
	// the returned pointer before reading it back through the struct field.
	source := "type V = struct { data []i32; fn get(self *V, index i32) *i32 { return &self.data[index]; } }; fn main() i32 { var values [3]i32 = [1, 2, 3]; let data []i32 = values[:]; var v V = V.{ data = data }; let pointer *V = &v; let p *i32 = pointer.get(1); *p = 9; return v.data[1]; }"
	emitAndRun(t, source, false, 9, false)
}

func TestEmitPointerReturnFromHelperCompilesAndRuns(t *testing.T) {
	// A helper accepts a pointer and returns it unchanged; the entry passes
	// the address of its own local (which stays live for the whole call) and
	// reads through the returned pointer. Proves both pointer-typed
	// parameters and pointer-typed helper results lower correctly.
	// fn identity(p *i32) *i32 { return p; }
	// fn main() i32 { var x i32 = 42; let p *i32 = identity(&x); return *p; }
	emitAndRun(t, "fn identity(p *i32) *i32 { return p; } fn main() i32 { var x i32 = 42; let p *i32 = identity(&x); return *p; }", false, 42, false)
}

func TestEmitPointerToPointerCompilesAndRuns(t *testing.T) {
	// var y i32 = 7; let p *i32 = &y; let q *i32 = p; return *q;
	// Pointer-to-pointer copy, then dereference through the copy.
	emitAndRun(t, "fn main() i32 { var y i32 = 7; let p *i32 = &y; let q *i32 = p; return *q; }", false, 7, false)
}

func TestEmitExplicitPointerCastRoundTripCompilesAndRuns(t *testing.T) {
	// var y i32 = 42; let p *i32 = &y; let q *void = p as *void; let r *i32 = q as *i32; return *r;
	// An explicit pointer-to-pointer cast (*i32 -> *void -> *i32) round-trips
	// correctly. Also exercises *void's own C representation (void *), which
	// pointerTypeName previously produced as a malformed empty type name
	// since it routed through cType (meant only for the fixed-width integer
	// kinds) rather than handling void/bool/char explicitly.
	emitAndRun(t, "fn main() i32 { var y i32 = 42; let p *i32 = &y; let q *void = p as *void; let r *i32 = q as *i32; return *r; }", false, 42, false)
}

func TestEmitNilPointerLocalCompilesAndRuns(t *testing.T) {
	// let p *i32 = nil; return 0;
	// Declaring a nil pointer local is valid; we just don't dereference it.
	emitAndRun(t, "fn main() i32 { let p *i32 = nil; return 0; }", false, 0, false)
}

func TestEmitPointerReassignCompilesAndRuns(t *testing.T) {
	// var y i32 = 5; var p *i32 = &y; var z i32 = 10; p = &z; return *p;
	// Reassigning a pointer local to point at a different variable.
	emitAndRun(t, "fn main() i32 { var y i32 = 5; var p *i32 = &y; var z i32 = 10; p = &z; return *p; }", false, 10, false)
}

func TestEmitPointerEmittedCContainsCheckedDeref(t *testing.T) {
	// Verify the emitted C contains pebble_rt_checked_deref_ptr calls for
	// dereference operations, not raw C dereferences.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var y i32 = 5; let p *i32 = &y; return *p; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_deref_ptr") {
		t.Errorf("emitted C missing pebble_rt_checked_deref_ptr call:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 5, false)
}

func TestEmitStructPointerRoundTripCompilesAndRuns(t *testing.T) {
	t.Skip("blocked: (*p).x on a struct pointer degrades to a tir.FieldValue node (field-of-value, not field-of-place) because the checker's place-tracking doesn't extend a DereferencePlace through a field-access base in this position — confirmed the same gap blocks even materializing the whole dereferenced struct into a local (`let v Point = *p;` also fails). Needs new struct-rvalue backend support, scoped as its own follow-up in spec/compiler/proposals/11-raw-pointers-and-unsafe-ops.md.")
	// Takes the address of a struct local, dereferences the pointer, and reads
	// a field through the dereferenced result.
	// type Point = struct { x i32; y i32; }
	// fn main() i32 { var point Point = Point.{ x = 3, y = 4 }; let p *Point = &point; return (*p).x; }
	emitAndRun(t, "type Point = struct { x i32; y i32; }; fn main() i32 { var point Point = Point.{ x = 3, y = 4 }; let p *Point = &point; return (*p).x; }", false, 3, false)
}

func TestEmitNullDerefReadPanics(t *testing.T) {
	// A pointer whose value is nil at runtime, dereferenced on the read side.
	// The null value is produced indirectly so the checker cannot reject it at
	// compile time: a helper stores nil in a local and returns it.
	// fn getNullPtr() *i32 { let p *i32 = nil; return p; }
	// fn main() i32 { let p *i32 = getNullPtr(); return *p; }
	// The dereference must panic with PEBBLE_PANIC_NULL_DEREFERENCE.
	emitAndRun(t, "fn getNullPtr() *i32 { let p *i32 = nil; return p; } fn main() i32 { let p *i32 = getNullPtr(); return *p; }", false, 0, true)
}

func TestEmitNullDerefWritePanics(t *testing.T) {
	// Same shape but assigning through the null pointer.
	// fn getNullPtr() *i32 { let p *i32 = nil; return p; }
	// fn main() i32 { let p *i32 = getNullPtr(); *p = 42; return 0; }
	emitAndRun(t, "fn getNullPtr() *i32 { let p *i32 = nil; return p; } fn main() i32 { let p *i32 = getNullPtr(); *p = 42; return 0; }", false, 0, true)
}
