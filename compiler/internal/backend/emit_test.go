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

func TestCheckStdMemImport(t *testing.T) {
	unit, _, _, _ := buildStdMemFixture(t, `import "std:mem"; fn main() i32 { return 0; }`, "main")
	if unit == nil {
		t.Fatal("std:mem import produced no IR")
	}
}

func TestCheckStdVecHasNoGenericPointerReceiverShapeErrors(t *testing.T) {
	vec, err := os.ReadFile("../../std/vec.peb")
	if err != nil {
		t.Fatal(err)
	}
	mem, err := os.ReadFile("../../std/mem.peb")
	if err != nil {
		t.Fatal(err)
	}
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb":    []byte(`import "std:vec"; fn main() void {}`),
		"std/vec.peb": vec,
		"std/mem.peb": mem,
	}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	for _, item := range diagnostics.Items() {
		text := fmt.Sprint(item)
		if strings.Contains(text, "T0505") {
			t.Fatalf("std:vec still has generic pointer receiver diagnostics: %v", diagnostics.Items())
		}
	}
}

func TestEmitStdMemNewSliceCompilesAndRuns(t *testing.T) {
	unit, snapshot, entryID, sources := buildStdMemFixture(t, `import "std:mem"; fn main() i32 { var values []i32 = mem::new_slice[i32](3); values[0] = 42; return values[0]; }`, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitExternCallNoArgumentsCompilesAndRuns(t *testing.T) {
	// A minimal extern call with no arguments (`extern fn rand() int; ...
	// rand();`) — the simplest shape that reproduces the original bug (a
	// direct call to an extern fn declaration failed at emit with "called
	// function symbol N concrete specialization not found"). The call must
	// lower to the function's real C name (rand, not a pebble_fn_<symbolID>
	// helper name), pass no hidden context (unlike a Pebble-convention call),
	// and produce no pebble_fn_<symbolID> prototype or definition. rand is a
	// real libc function declared by <stdlib.h>, so the whole pipeline
	// compiles AND RUNS; the discarded result keeps the test deterministic.
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `extern fn rand() int; fn main() int { var x int = rand(); return 0; }`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	emitted := buf.String()
	if !strings.Contains(emitted, "rand()") {
		t.Errorf("emitted C does not call the real C name rand():\n%s", emitted)
	}
	if strings.Contains(emitted, "pebble_fn_") {
		t.Errorf("emitted C contains a pebble_fn_ helper for an extern, want none:\n%s", emitted)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

func TestEmitExternCallWithArgumentsAndReturnCompilesAndRuns(t *testing.T) {
	// An extern call with arguments and a return value, mirroring
	// malloc/free's real shape (malloc takes a uint size and returns *void;
	// free takes *void and returns void): the malloc result is cast to *int
	// and stored, dereferenced and printed (the example asserts 42), then
	// freed. Exercises the C-convention argument lowering (no context
	// threaded), the real-C-name call emission for both a value-returning and
	// a void extern, and the resulting program compiles AND RUNS.
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `extern fn malloc(size uint) *void; extern fn free(ptr *void) void; fn main() int { var num *int = malloc(sizeof int) as *int; *num = 42; print *num; free(num as *void); return 42; }`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	emitted := buf.String()
	if !strings.Contains(emitted, "malloc(sizeof(int32_t))") {
		t.Errorf("emitted C does not call the real C name malloc with sizeof(int32_t):\n%s", emitted)
	}
	if strings.Contains(emitted, "pebble_fn_") {
		t.Errorf("emitted C contains a pebble_fn_ helper for an extern, want none:\n%s", emitted)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitGenericReachabilityUsesSpecializationIdentity(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, `fn add_one[T](x T, y T) T => x; fn main() i32 { var a i32 = add_one[i32](40, 1); let p *i32 = &a; let b *i32 = add_one[*i32](p, p); return a + *b; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 80, false)
}

func TestEmitGenericReachabilityEmitsThreeSpecializations(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, `fn choose[T](x T) i32 => 7; fn main() i32 { var a i32 = choose[i32](1); var b i32 = choose[bool](true); let p *i32 = &a; var c i32 = choose[*i32](p); return a + b + c; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 21, false)
}

// TestEmitGenericHelperSpecializedAtConcreteWidthCompilesAndRuns is the exact
// motivating repro for the compatible-integer-width parameter gate: a generic
// helper identity[T] called with an i32 local from an fn main() int entry
// (whose own resolved width is the abstract int builtin, NOT i32 — the two are
// distinct builtins sharing the int32_t C representation). The specialization
// identity[i32] has an i32-typed parameter, which the pre-fix
// validateHelperSignature rejected with "called function symbol ... has type
// i32, want int". The program must compile under -Wall -Wextra -Werror and run,
// returning the value identity passed through.
func TestEmitGenericHelperSpecializedAtConcreteWidthCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `fn identity[T](x T) T { return x; } fn main() int { var a i32 = 5; var r = identity(a); return r; }`, false, 5, false)
}

// TestEmitGenericClampShapeSpecializedAtConcreteWidthCompilesAndRuns mirrors
// the real std/math.peb motivating case (clamp[T] = max(lo, min(x, hi))) with
// the generic helper DEFINED INLINE so the test needs no std import: a
// two-level generic chain (min/max) whose specializations all substitute the
// concrete i32 width, called from an fn main() int entry with i32 locals and
// the result stored into an i32 local before returning (the shape that reaches
// emission — a direct `return clamp(...)` from an int entry hits a checker-level
// int-vs-i32 unification conflict instead). The clamp of (5, 10, 20) is 10.
func TestEmitGenericClampShapeSpecializedAtConcreteWidthCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `fn min[T](a T, b T) T { if a < b { return a; } return b; } fn max[T](a T, b T) T { if a > b { return a; } return b; } fn clamp[T](x T, lo T, hi T) T { return max(lo, min(x, hi)); } fn main() int { var x i32 = 5; var lo i32 = 10; var hi i32 = 20; var r i32 = clamp(x, lo, hi); return r; }`, false, 10, false)
}

// TestEmitGenericHelperConcreteWidthWritesInt32TParams asserts the emitted-C
// shape for the widened case, not just that Emit succeeds: the generic
// specialization identity[i32] must be declared AND defined as
// `int32_t pebble_fn_24_3(PebbleContext *ctx, int32_t pebble_local_26)` — the
// parameter declared at the entry's C representation (int32_t, since int and
// i32 share it) and the body returning it — and the int-declared entry must
// keep its plain-int pebble_user_main. Symbols 24 (identity[i32]) and 26 (its
// parameter) come from the fixture's typed-IR construction, deterministic for
// this exact source.
func TestEmitGenericHelperConcreteWidthWritesInt32TParams(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, `fn identity[T](x T) T { return x; } fn main() int { var a i32 = 5; var r = identity(a); return r; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24_3(PebbleContext *ctx, int32_t pebble_local_26);",
		"static int32_t pebble_fn_24_3(PebbleContext *ctx, int32_t pebble_local_26) {",
		"return pebble_local_26;",
		"static int pebble_user_main(PebbleContext *ctx) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

// TestEmitRejectsGenericHelperSpecializedAtMismatchedConcreteWidth is the
// regression guard for the widening: only a concrete width that SHARES the
// entry's C representation is admitted. An i64 specialization called from an
// int-declared entry (i64 has no int32_t representation, so
// isCompatibleIntegerWidth is false and isWidth is too) must still be a clean
// rejection naming the found type, never silently emitted at a guessed width.
func TestEmitRejectsGenericHelperSpecializedAtMismatchedConcreteWidth(t *testing.T) {
	emitAndRunRejects(t, `fn identity[T](x T) T { return x; } fn main() int { var a i64 = 5; var r i64 = identity(a); return 0; }`, "called function symbol 24 parameter 0 (symbol 26) has type i64, want int")
}

func TestEmitSliceFromRawCompilesAndRuns(t *testing.T) {
	unit, snapshot, entryID, sources := buildStdFixture(t, "fn main() i32 { var value i32 = 42; var ptr *i32 = &value; let values []i32 = slice ptr, 1; return values[0]; }", "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitStructWithAllocatorFieldCompilesAndRuns(t *testing.T) {
	// The standalone synthetic repro of the runtime-builtin struct-field-typedef
	// gap: a struct with an Allocator-typed field (mirroring std/hmap.peb's
	// HashMap[K,V].backing Allocator), constructed from context.default_allocator
	// exactly as hmap's new/with_capacity do. Allocator is a compiler builtin
	// with a hand-written PebbleAllocator C type (never a per-TypeID
	// pebble_struct_<id>_t typedef), so orderAggregateTypes must skip it
	// entirely from the typedef-emission postorder while the struct's OWN real
	// fields still get typedefs; the Allocator field itself is declared with
	// PebbleAllocator and initialized from the runtime context. Reading the
	// Allocator field back into a local (`let a = h.backing`) exercises the
	// runtime-typed field-read path too. The program must compile and run.
	emitAndRun(t, "type Holder = struct { value int; backing Allocator; }; fn main() int { var h Holder = Holder.{ value = 41, backing = context.default_allocator }; let a = h.backing; return h.value + 1; }", false, 42, false)
}

func TestEmitStructWithAllocatorFieldWritesC(t *testing.T) {
	// Confirm the emitted C directly: the struct's own typedef carries its real
	// field AND the Allocator field declared as the hand-written PebbleAllocator
	// (NOT a per-TypeID pebble_struct_<id>_t, whose typedef orderAggregateTypes
	// now skips), the construction initializes it from the runtime context's
	// default allocator, and exactly ONE struct typedef is emitted — no typedef
	// for the Allocator type itself. This is the "skipped entirely, not a
	// zero-valued placeholder" behavior the orderAggregateTypes fix guarantees.
	unit, snapshot, entryID, sources := buildFixture(t, "type Holder = struct { value int; backing Allocator; }; fn main() int { var h Holder = Holder.{ value = 41, backing = context.default_allocator }; return h.value + 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleAllocator pebble_field_",
		"(*ctx).allocator",
		"int32_t pebble_field_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Count(out, "typedef struct {") != 1 {
		t.Errorf("emitted C must contain exactly one struct typedef (for Holder, not the skipped Allocator runtime type):\n%s", out)
	}
	openIdx := strings.Index(out, "typedef struct {")
	closeIdx := strings.Index(out[openIdx:], "} pebble_struct_")
	block := out[openIdx : openIdx+closeIdx]
	for _, line := range strings.Split(block, "\n") {
		if strings.Contains(line, "pebble_field_") && strings.Contains(line, "pebble_struct_") {
			t.Errorf("emitted C declares a struct field with a per-TypeID pebble_struct_ type; the Allocator field must use PebbleAllocator:\n%s", out)
		}
	}
}

func TestEmitStructWithUintFieldCompilesAndRuns(t *testing.T) {
	// An ordinary (non-generic) struct field typed as uint -- the same narrow
	// entry-width/bool-only gate already widened this session for optional
	// payloads (d737242), slice elements (f85b4a0), and function-type
	// parameters/results (b5c139c), now widened for structFieldCType's own
	// scalar field-type gate: structFieldCType (the typedef), buildStructBraceList's
	// field-construction case, and buildStructFieldRead all gained a uint case.
	// Confirmed via the real std/hmap.peb motivating shape: HashMap's own
	// len/cap uint fields (see TestEmitStdHmapInsertGetFullConsumer).
	emitAndRun(t, "type Counter = struct { n uint; }; fn main() i32 { var c Counter = Counter.{ n = 5 }; return c.n as i32; }", false, 5, false)
}

func TestEmitStructWithUintFieldWritesUint64T(t *testing.T) {
	// Emitted-C shape check: the uint field's typedef must declare uint64_t,
	// not the entry width's own C type or a rejection.
	unit, snapshot, entryID, sources := buildFixture(t, "type Counter = struct { n uint; }; fn main() i32 { var c Counter = Counter.{ n = 5 }; return c.n as i32; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "uint64_t pebble_field_") {
		t.Errorf("expected a uint64_t struct field in emitted C, got:\n%s", out)
	}
}

func TestEmitUintHelperCallAsLocalInitializerCompilesAndRuns(t *testing.T) {
	// A call to a uint-returning helper used as a uint local's declaration
	// initializer (`var n = get_count();`) — the buildUintExpr DirectCall
	// gap: the checker routes a uint-typed initializer through buildUintExpr
	// (mirroring how buildExpr handles every other type), which had no
	// DirectCall case and rejected the shape with "unsupported uint
	// expression node DirectCall". The helper's C return type is uint64_t —
	// the exact C type a uint value uses — so the call emits directly with
	// no cast, and the program's exit code asserts the actual uint value
	// came back (get_count's 5 must reach the comparison through the uint
	// local), not just that Emit succeeded.
	emitAndRun(t, "fn get_count() uint { return 5; }\nfn main() i32 {\nvar n = get_count();\nif n != 5 { return 1; }\nreturn 0;\n}", false, 0, false)
}

func TestEmitUintHelperCallWithArgsAsLocalInitializerCompilesAndRuns(t *testing.T) {
	// The read_line-shaped variant of the buildUintExpr DirectCall gap: a
	// uint-returning helper taking a pointer and a char address-of cast to
	// *void, called as a uint local's initializer — the std/io.peb shape
	// `var bytes = read(file, &ch as *void, 1);` (read's declared result
	// type is uint, so bytes is inferred uint). The exit code asserts the
	// returned uint value (1, the max_bytes argument) reached the comparison
	// through the uint local, and that the pointer/char address-of
	// arguments — ordinary scalar/pointer shapes whose call produces no
	// pre-statement — passed through the call correctly.
	emitAndRun(t, "fn read(file *void, buffer *void, max_bytes uint) uint { return max_bytes; }\nfn main() i32 {\nvar file *void = nil;\nvar ch char = 'a';\nvar bytes = read(file, &ch as *void, 1);\nif bytes != 1 { return 1; }\nreturn 0;\n}", false, 0, false)
}

func TestEmitStructEnumFieldCompilesAndRuns(t *testing.T) {
	// The standalone synthetic repro of the enum-typed-struct-field gap: a
	// struct with a plain-enum-typed field mirroring std/hmap.peb's Entry
	// exactly (type EntryState = enum { Empty, Tombstone, Occupied }; struct
	// Entry { key, value, state EntryState }). All four required operations
	// are exercised through a real program whose exit code depends on each
	// one working: CONSTRUCTION places the variant's C enum constant into the
	// field's own pebble_enum_<typeID>_t (state starts .Empty); a FIELD READ
	// (e.state == .Empty) compares the projected field directly against a
	// variant literal and gates the pointer write; a FIELD ASSIGNMENT
	// mutating the already-constructed instance (e.state = .Tombstone) fires
	// only if the pointer write took; and a write THROUGH a POINTER to the
	// struct (mutate(&e)'s e->state = .Occupied, the slot.state = .Occupied
	// shape from hmap's insert) mutates through the -> projection. The final
	// != comparison returns 0 only if state really is Tombstone; any one of
	// the four breaking changes the exit code.
	emitAndRun(t, "type EntryState = enum { Empty, Tombstone, Occupied };\ntype Entry = struct { key i32; value i32; state EntryState; };\nfn mutate(e *Entry) void { e.state = .Occupied; }\nfn main() i32 {\nvar e Entry = Entry.{ key = 1, value = 2, state = .Empty };\nif e.state == .Empty { mutate(&e); }\nif e.state == .Occupied { e.state = .Tombstone; }\nif e.state != .Tombstone { return 1; }\nreturn 0;\n}", false, 0, false)
}

func TestEmitStructEnumFieldWritesC(t *testing.T) {
	// Emitted-C shape check: the struct field whose type is a plain enum is
	// declared with the enum's OWN typedef name — pebble_enum_<typeID>_t, the
	// identical C type an enum-typed local/parameter/result uses (see
	// enumTypeName) — no new typedef machinery. The construction initializes
	// the field from the variant's C enum constant, the enum typedef itself is
	// still emitted, and the read/compare/assign all use the plain
	// pebble_field_<member> projection.
	unit, snapshot, entryID, enumType, _, sources := enumFixture(t, "type EntryState = enum { Empty, Tombstone, Occupied };\ntype Entry = struct { key i32; value i32; state EntryState; };\nfn main() i32 {\nvar e Entry = Entry.{ key = 1, value = 2, state = .Empty };\nif e.state == .Empty { return 1; }\nreturn 0;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef enum {",
		enumTypeName(enumType) + " pebble_field_",
		"= pebble_variant_",
		".pebble_field_",
		" == pebble_variant_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func emitRuntimeAndRun(t *testing.T, sourceText string, wantCode int) {
	t.Helper()
	unit, snapshot, entryID, sources := buildStdFixture(t, sourceText, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), wantCode, false)
}

func TestEmitEmptyEntryWritesC(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() void {}", "main", true)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

func TestEmitIntegerReturnEntryWritesC(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitAutoReferencesValueForPointerReceiver(t *testing.T) {
	emitAndRun(t, `type S = struct { n i32; fn set(self *S, value i32) void { self.n = value; } }; fn main() i32 { var s = S.{ n = 0 }; s.set(9); return s.n; }`, false, 9, false)
}

func TestEmitGenericPointerReceiverCallsSiblingMethod(t *testing.T) {
	emitAndRun(t, `type Vec[T] = struct { value i32; fn reserve(self *Vec[i32], amount i32) void { self.value = amount; } fn push(self *Vec[i32], value i32) void { self.reserve(value); } }; fn main() i32 { var v = Vec[i32].{ value = 0 }; v.push(7); return v.value; }`, false, 7, false)
}

func TestEmitOptionalHasValueCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `fn main() i32 { let present ?i32 = some 7; if present.has_value { return 1; } else { return 0; } }`, false, 1, false)
	emitAndRun(t, `fn main() i32 { let absent ?i32 = none; if !absent.has_value { return 1; } else { return 0; } }`, false, 1, false)
}

func TestEmitFieldNilAssignmentRoundTripCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `type P = struct { d *i32; }; fn main() i32 { var value i32 = 7; var p P = P.{ d = &value }; p.d = nil; if p.d == nil { return 1; } else { return 0; } }`, false, 1, false)
}

func TestEmitGenericMethodCallCompilesAndRuns(t *testing.T) {
	// The exact motivating repro: a generic struct method redeclaring the
	// struct's own type parameter on itself (`fn get[K](self Box[K]) K`),
	// called via ordinary method-call syntax with the type argument inferred
	// from the receiver (never written at the call site). Before the fix, the
	// checker never built the method's concrete specialization, so the
	// backend's findCalledFunctionDeclaration had no FunctionDeclaration with
	// matching TypeArgs and rejected the call as an unloverable generic.
	emitAndRun(t, `type Box[K] = struct { value K; fn get[K](self Box[K]) K { return self.value; } }; fn main() int { var b Box[int] = Box[int].{ value = 5 }; return b.get(); }`, false, 5, false)
}

func TestEmitGenericMethodTwoSpecializationsCompileAndRun(t *testing.T) {
	// TWO specializations of the same generic struct calling the SAME method
	// in one program must each get their OWN method specialization: Box[int]'s
	// get returns 5 and Box[bool]'s get returns true. If the two method
	// specializations collided (shared a symbol/FunctionID), one read would
	// dispatch to the wrong width and the exit code would be wrong.
	emitAndRun(t, `type Box[K] = struct { value K; fn get[K](self Box[K]) K { return self.value; } }; fn main() int { var b Box[int] = Box[int].{ value = 5 }; var c Box[bool] = Box[bool].{ value = true }; if c.get() { return b.get(); } return 0; }`, false, 5, false)
}

func TestEmitGenericMethodExtraTypeParameterParametersCompileAndRun(t *testing.T) {
	// A method taking parameters beyond self that also depend on the type
	// parameters (mirroring std/hmap.peb's insert(self, key K, value V)) must
	// resolve those parameter types end to end: put(4, 5) writes both through
	// the pointer receiver, and the returned key plus the stored value encode
	// 4 * 10 + 5 = 45 in the exit code.
	emitAndRun(t, `type Pair[K, V] = struct { key K; value V; fn put[K, V](self *Pair[K, V], k K, v V) K { self.key = k; self.value = v; return self.key; } }; fn main() int { var p Pair[int, int] = Pair[int, int].{ key = 1, value = 2 }; let got int = p.put(4, 5); return got * 10 + p.value; }`, false, 45, false)
}

func TestEmitGenericMethodPointerReceiverCompilesAndRuns(t *testing.T) {
	// A pointer-receiver generic method on a generic struct: the receiver
	// value is auto-referenced for the `self *Box[K]` parameter, and the
	// method's own K resolves to the receiver's int.
	emitAndRun(t, `type Box[K] = struct { value K; fn get[K](self *Box[K]) K { return self.value; } }; fn main() int { var b Box[int] = Box[int].{ value = 7 }; return b.get(); }`, false, 7, false)
}

func TestEmitGenericStructDataFieldsCompileAndRun(t *testing.T) {
	// Generic struct data fields slice 1: a generic struct whose fields are the
	// struct's own type parameters (`key K`, `value V`) must emit per
	// -specialization concrete field types, so Pair[int, int] constructs and
	// reads both fields back. Before the slice, MemberTypes[i] for `key K` /
	// `value V` was unresolved (the generic declaration's template is a type
	// parameter, never a TemplateKnown concrete type), so resolveStructInfo
	// had no concrete type and the typedef/field read failed. 5 * 10 + 10 = 60
	// encodes both field values in the exit code, proving key == 5 and
	// value == 10 independently.
	emitAndRun(t, `type Pair[K, V] = struct { key K; value V; }; fn main() int { let p Pair[int, int] = Pair[int, int].{ key = 5, value = 10 }; return p.key * 10 + p.value; }`, false, 60, false)
}

func TestEmitGenericStructTwoSpecializationsCompileAndRun(t *testing.T) {
	// Two specializations of the SAME generic struct in one program must emit
	// two DISTINCT, independently-correct C typedefs (pebble_struct_<typeID>_t
	// per specialization TypeID): Pair[int, int]'s value field is int while
	// Pair[int, bool]'s is bool. The two share every field symbol (the member
	// types are the same declaration), so the backend must resolve each
	// instantiation's field type from ITS OWN construction evidence, not a
	// per-field-symbol global map. The read of q.value must dispatch to the
	// bool grammar and the three width field reads to the integer grammar;
	// 5 + 10 + 6 = 21 confirms all four values end to end.
	emitAndRun(t, `type Pair[K, V] = struct { key K; value V; }; fn main() int { let p Pair[int, int] = Pair[int, int].{ key = 5, value = 10 }; let q Pair[int, bool] = Pair[int, bool].{ key = 6, value = true }; if q.value { return p.key + p.value + q.key; } else { return 0; } }`, false, 21, false)
}

func TestEmitGenericStructDataFieldsWritesConcreteCTypedefs(t *testing.T) {
	// The emitted-C shape check: each specialization's typedef field C types
	// must match its concrete instantiation — int32_t for the int-typed fields
	// of Pair[int, int] AND of Pair[int, bool]'s key, bool for Pair[int,
	// bool]'s value — with no generic placeholder and no rejection. The two
	// typedefs are distinct pebble_struct_<typeID>_t definitions (25 for
	// Pair[int, int], 26 for Pair[int, bool], 27/28 the key/value field
	// symbols) from a real fixture dump. The entry's resolved width here is
	// types.Int, which cType maps to int32_t.
	unit, snapshot, entryID, sources := buildFixture(t, `type Pair[K, V] = struct { key K; value V; }; fn main() int { let p Pair[int, int] = Pair[int, int].{ key = 5, value = 10 }; let q Pair[int, bool] = Pair[int, bool].{ key = 6, value = true }; if q.value { return p.key + p.value; } else { return 0; } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_27;\n    int32_t pebble_field_28;\n} pebble_struct_25_t;",
		"typedef struct {\n    int32_t pebble_field_27;\n    bool pebble_field_28;\n} pebble_struct_26_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// The two specializations are distinct typedef names: the second does not
	// reuse the first's (a single shared layout would emit only one typedef).
	if strings.Count(out, "} pebble_struct_25_t;") != 1 || strings.Count(out, "} pebble_struct_26_t;") != 1 {
		t.Errorf("expected exactly one typedef each for the two specializations:\n%s", out)
	}
	typedefIndex := strings.Index(out, "typedef struct")
	mainIndex := strings.Index(out, "static int pebble_user_main")
	if typedefIndex < 0 || mainIndex < 0 || typedefIndex > mainIndex {
		t.Errorf("struct typedefs do not precede pebble_user_main (definition before use):\n%s", out)
	}
}

func TestEmitGenericStructOptionalFieldSingleCompileAndRun(t *testing.T) {
	// Generic struct compound fields, optional-wrapped: a single
	// specialization of `Box[K] = struct { value ?K; }` with K = int must
	// compile AND run with the correct unwrapped value. The field's member
	// template wraps the struct's own parameter, so the checker leaves its
	// MemberTypes entry unresolved and the backend must recover the concrete
	// Optional(int) type from the construction evidence. 5 is the unwrapped
	// value.
	emitAndRun(t, `type Box[K] = struct { value ?K; }; fn main() int { var b Box[int] = Box[int].{ value = some 5 }; return b.value!; }`, false, 5, false)
}

func TestEmitGenericStructOptionalTwoSpecializationsCompileAndRun(t *testing.T) {
	// The exact repro that was broken before this slice: TWO specializations
	// of the same generic struct in one program, the field type a compound
	// wrapping the struct's own parameter. Before the fix, Box[bool]'s struct
	// typedef reused Box[int]'s Optional(int) field type (pebble_optional_29_t)
	// and the Box[bool] construction referenced pebble_optional_30_t which was
	// never defined — a real cc failure (undeclared identifier). The
	// specialization is now resolved from its own construction evidence, so
	// each struct typedef carries its own payload optional and every optional
	// typedef is emitted. The three-way test proves all three values
	// independently: c.value! is true, d.value! is false (both bool
	// specializations, distinguishing by VALUE not just by type), b.value! is
	// 5 (int specialization), so the exit code 5 requires every field read to
	// dispatch against its own specialization's optional payload type. (d's
	// payload is `some false`, not `none` — force-unwrapping a `none` value
	// with `!` is checked and panics by design, so `none` can't stand in for
	// "false" here without aborting the program.)
	emitAndRun(t, `type Box[K] = struct { value ?K; }; fn main() int { var b Box[int] = Box[int].{ value = some 5 }; var c Box[bool] = Box[bool].{ value = some true }; var d Box[bool] = Box[bool].{ value = some false }; if c.value! { if d.value! { return 1; } else { return b.value!; } } else { return 0; } }`, false, 5, false)
}

func TestEmitGenericStructPointerFieldSingleCompileAndRun(t *testing.T) {
	// Generic struct compound fields, pointer-wrapped: a single specialization
	// of `Ref[K] = struct { ptr *K; }` with K = int must compile AND run with
	// the correct dereferenced value. The member's template wraps the struct's
	// own parameter, so its MemberTypes entry is unresolved and the backend
	// recovers the concrete *int pointee from the construction evidence; the
	// nil construction, the non-nil assignment, and the checked dereference all
	// agree on int32_t. 7 is the value read through the pointer field.
	emitAndRun(t, `type Ref[K] = struct { ptr *K; }; fn main() int { var r Ref[int] = Ref[int].{ ptr = nil }; var x int = 7; var p *int = &x; r.ptr = p; if r.ptr == nil { return 0; } else { return *r.ptr; } }`, false, 7, false)
}

func TestEmitGenericStructPointerTwoSpecializationsCompileAndRun(t *testing.T) {
	// The pointer analogue of the two-specialization repro: before this slice,
	// Ref[bool]'s struct typedef field reused Ref[int]'s pointee (both declared
	// `bool *` or both `int32_t *` depending on node order) — silently wrong
	// and a -Werror incompatible-pointer-types failure under the runtime
	// Makefile's flags. Now each specialization's pointee is recovered from its
	// own construction evidence. The program proves both independently: *s.ptr
	// reads Ref[bool]'s bool true (driving the outer if), *r.ptr reads Ref[int]'s
	// int 7 (the returned value), so exit code 7 requires both field reads to
	// dispatch against their own specialization's pointee type.
	emitAndRun(t, `type Ref[K] = struct { ptr *K; }; fn main() int { var r Ref[int] = Ref[int].{ ptr = nil }; var s Ref[bool] = Ref[bool].{ ptr = nil }; var x int = 7; var y bool = true; var p *int = &x; var q *bool = &y; r.ptr = p; s.ptr = q; if s.ptr == nil { return 0; } else { if *s.ptr { return *r.ptr; } else { return 1; } } }`, false, 7, false)
}

func TestEmitGenericStructPointerTwoSpecializationsWriteConcreteCTypedefs(t *testing.T) {
	// The emitted-C shape check for the pointer two-specialization case: each
	// specialization's typedef must declare the CORRECT pointee C type — int32_t
	// for Ref[int], bool for Ref[bool] — with no shared/wrong pointee and no
	// rejection. The two typedefs are distinct pebble_struct_<typeID>_t
	// definitions (24 for Ref[int], 25 for Ref[bool], 26 the ptr field symbol)
	// from a real fixture dump. Before the fix both typedefs declared the same
	// pointee (one specialization's won and the other was silently wrong).
	unit, snapshot, entryID, sources := buildFixture(t, `type Ref[K] = struct { ptr *K; }; fn main() int { var r Ref[int] = Ref[int].{ ptr = nil }; var s Ref[bool] = Ref[bool].{ ptr = nil }; var x int = 7; var y bool = true; var p *int = &x; var q *bool = &y; r.ptr = p; s.ptr = q; if s.ptr == nil { return 0; } else { if *s.ptr { return *r.ptr; } else { return 1; } } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t * pebble_field_26;\n} pebble_struct_24_t;",
		"typedef struct {\n    bool * pebble_field_26;\n} pebble_struct_25_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Each specialization is a distinct typedef name and the second does not
	// reuse the first's pointee (a shared layout would emit one typedef).
	if strings.Count(out, "} pebble_struct_24_t;") != 1 || strings.Count(out, "} pebble_struct_25_t;") != 1 {
		t.Errorf("expected exactly one typedef each for the two specializations:\n%s", out)
	}
}

func TestEmitGenericStructOptionalTwoSpecializationsWriteConcreteCTypedefs(t *testing.T) {
	// The emitted-C shape check for the optional two-specialization case: each
	// specialization's typedef must name its OWN payload optional type —
	// pebble_optional_29_t (Optional(int)) for Box[int], pebble_optional_30_t
	// (Optional(bool)) for Box[bool] — and BOTH optional typedefs must be
	// emitted (before this slice the bool-payload optional was referenced but
	// never defined, a real cc error). Struct type IDs 24/25, optional types
	// 29/30, field symbol 26 from a real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, `type Box[K] = struct { value ?K; }; fn main() int { var b Box[int] = Box[int].{ value = some 5 }; var c Box[bool] = Box[bool].{ value = some true }; var d Box[bool] = Box[bool].{ value = none }; if c.value! { if d.value! { return 1; } else { return b.value!; } } else { return 0; } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    pebble_optional_29_t pebble_field_26;\n} pebble_struct_24_t;",
		"typedef struct {\n    pebble_optional_30_t pebble_field_26;\n} pebble_struct_25_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Both optional typedefs must exist: the bool-payload optional is what was
	// referenced-but-undefined before this slice.
	if strings.Count(out, "} pebble_optional_29_t;") != 1 || strings.Count(out, "} pebble_optional_30_t;") != 1 {
		t.Errorf("expected exactly one typedef each for the two optional payloads:\n%s", out)
	}
}

func TestEmitGenericStructNestedFieldCompileAndRun(t *testing.T) {
	// Nested-generic struct fields: a generic struct field typed as ANOTHER
	// generic struct instantiated with the outer's own parameter
	// (`Outer[K] = struct { inner Inner[K]; }`). The inner construction
	// (`Inner[int].{ val = 5 }`) lives at the outer RecordConstruct's
	// Fields[0].Value — reachable only via the field-value recursion this
	// slice adds to collectStructTypesWalk, never via the Children-following
	// recursion (field values are in node.Fields, not node.Children, the same
	// gap collectFunctionTypesWalk/collectOptionalTypesWalk already closed) —
	// so before the fix the inner struct type was never collected for a C
	// typedef and emission failed outright with "struct type 0 is not in the
	// type snapshot" (the outer field read's unresolved TypeID). 5 is the
	// value read through both struct layers.
	emitAndRun(t, `type Inner[T] = struct { val T; }; type Outer[K] = struct { inner Inner[K]; }; fn main() int { var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } }; return o.inner.val; }`, false, 5, false)
}

func TestEmitGenericStructNestedFieldTwoSpecializationsCompileAndRun(t *testing.T) {
	// Two specializations of the outer struct in one program, each carrying a
	// DIFFERENT nested specialization (`Inner[int]` vs `Inner[bool]`): every
	// field symbol is shared across specializations (they come from the same
	// declaration), but each instantiation's nested struct type is now
	// collected from its own construction subtree, so Outer[int]'s inner and
	// Outer[bool]'s inner resolve to distinct C struct typedefs with the
	// correct per-K payload. b.inner.val must dispatch against Inner[bool]'s
	// bool grammar and o.inner.val against Inner[int]'s int grammar; exit
	// code 5 requires both field reads to hit their own specialization.
	emitAndRun(t, `type Inner[T] = struct { val T; }; type Outer[K] = struct { inner Inner[K]; }; fn main() int { var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } }; var b Outer[bool] = Outer[bool].{ inner = Inner[bool].{ val = true } }; if b.inner.val { return o.inner.val; } else { return 0; } }`, false, 5, false)
}

func TestEmitGenericStructNestedFieldWritesInnerTypedefFirst(t *testing.T) {
	// The emitted-C shape and ORDER check for the nested-generic case: the
	// inner struct's typedef must be emitted BEFORE the outer struct's, since
	// C requires a type to be fully defined before it is used as a by-value
	// member (a forward declaration is not enough for an embedded field).
	// orderAggregateTypes's DFS postorder emits dependencies first, so once
	// the fix collects Inner[int] at all, `pebble_struct_26_t` (Inner[int])
	// precedes `pebble_struct_25_t` (Outer[int], whose field names
	// pebble_struct_26_t). The order is asserted directly rather than trusting
	// the compile (which would also fail loudly under -Werror were it wrong).
	// Struct type IDs 26/25 and field symbols 26 (val) / 29 (inner) from a
	// real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, `type Inner[T] = struct { val T; }; type Outer[K] = struct { inner Inner[K]; }; fn main() int { var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } }; return o.inner.val; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_26;\n} pebble_struct_26_t;",
		"typedef struct {\n    pebble_struct_26_t pebble_field_29;\n} pebble_struct_25_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Recover the inner typedef name from the outer struct's struct-typed
	// field reference, then assert the inner typedef's definition precedes
	// that reference (dependency-first emission).
	innerName := ""
	for _, line := range strings.Split(out, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "pebble_struct_") && strings.Contains(trimmed, " pebble_field_") {
			innerName = strings.TrimSpace(strings.Fields(trimmed)[0])
			break
		}
	}
	if innerName == "" {
		t.Fatalf("emitted C has no struct-typed field reference:\n%s", out)
	}
	innerTypedefEnd := strings.Index(out, "} "+innerName+";")
	outerFieldRef := strings.Index(out, innerName+" pebble_field_")
	if innerTypedefEnd < 0 {
		t.Errorf("emitted C missing the inner struct typedef definition (%s):\n%s", innerName, out)
	} else if outerFieldRef < 0 {
		t.Errorf("emitted C missing the outer struct's struct-typed field reference:\n%s", out)
	} else if innerTypedefEnd > outerFieldRef {
		t.Errorf("inner struct typedef (%s) is not emitted before the outer struct that embeds it (inner typedef end %d > outer field reference %d):\n%s", innerName, innerTypedefEnd, outerFieldRef, out)
	}
}

func TestEmitGenericStructNestedFieldTwoSpecializationsWriteConcreteCTypedefs(t *testing.T) {
	// The emitted-C shape check for the nested-generic two-specialization case:
	// each outer specialization's inner field must name ITS OWN nested
	// specialization's typedef — pebble_struct_26_t (Inner[int]) inside
	// pebble_struct_25_t (Outer[int]), pebble_struct_28_t (Inner[bool])
	// inside pebble_struct_27_t (Outer[bool]) — with no shared/wrong inner
	// typedef (a shared layout would emit one). Struct type IDs 26/25/28/27,
	// field symbols 26 (val) / 29 (inner) from a real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, `type Inner[T] = struct { val T; }; type Outer[K] = struct { inner Inner[K]; }; fn main() int { var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } }; var b Outer[bool] = Outer[bool].{ inner = Inner[bool].{ val = true } }; if b.inner.val { return o.inner.val; } else { return 0; } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_26;\n} pebble_struct_26_t;",
		"typedef struct {\n    pebble_struct_26_t pebble_field_29;\n} pebble_struct_25_t;",
		"typedef struct {\n    bool pebble_field_26;\n} pebble_struct_28_t;",
		"typedef struct {\n    pebble_struct_28_t pebble_field_29;\n} pebble_struct_27_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Four distinct typedef names — the two specializations do not share
	// layouts (each outer embeds its own inner).
	for _, name := range []string{"pebble_struct_26_t", "pebble_struct_25_t", "pebble_struct_28_t", "pebble_struct_27_t"} {
		if strings.Count(out, "} "+name+";") != 1 {
			t.Errorf("expected exactly one typedef named %s:\n%s", name, out)
		}
	}
}

func TestEmitGenericStructDataFieldsCrossModuleContextCompileAndRun(t *testing.T) {
	// Generic struct data fields slice 1 (254a00c) substituted a directly
	// parameter-typed field from the struct's generic declaration key in the
	// snapshot — but only when that declaration key was the FIRST
	// all-TypeParameter-arg Nominal key for the declaration in allocation
	// order. A generic struct referenced from another generic context (a
	// method, or a generic function that names the struct with ITS OWN type
	// parameters) interns a SEPARATE all-TypeParameter key per context, so a
	// plain first-match scan could return a function/method context's key —
	// whose parameter symbols never match the declaration's own field
	// MemberTypes (each context's parameters are distinct symbols) — leaving
	// `val K` unsubstituted and buildStructTypedef rejecting the
	// type-parameter field. This is the exact std/hmap.peb Entry[int,int]
	// root cause: HashMap's entries []Entry[K,V] field reaches Entry's key K /
	// value V, and hmap's eight methods each intern an Entry[K,V] key with
	// their own inherited K/V symbols before Entry's canonical key lands.
	// This fixture is the minimal repro: Inner[K] (a direct parameter field
	// plus a method, which adds another all-TypeParameter key), Outer[K]
	// carrying an Inner[K] field, and a generic function newo[K] that
	// constructs Outer[K] with newo's OWN K — the function-context key interns
	// before Inner's canonical key. Before structTypeParameters selected the
	// canonical key (whose parameters are the ones the declaration's own field
	// MemberTypes reference), Emit failed with "field type type-parameter(...)
	// is not supported"; now the emitted Inner[int] typedef carries the
	// concrete int32_t field and the program compiles and runs to 5.
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb": []byte(`import "std:lib"; fn main() int { var o = lib::newo[int](5); return o.inner.val; }`),
		"std/lib.peb": []byte(`type Inner[K] = struct {
    val K;
    fn bump[K](self *Inner[K]) void { self.val = self.val; }
};
type Outer[K] = struct {
    inner Inner[K];
    fn touch[K](self *Outer[K]) void { self.inner.val = self.inner.val; }
};
fn newo[K](v K) Outer[K] {
    return Outer[K].{ inner = Inner[K].{ val = v } };
}`),
	}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("check succeeded without an IR unit")
	}
	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entryID = candidate.ID
		}
	}
	if entryID == 0 {
		t.Fatal("missing symbol \"main\"")
	}
	var buf bytes.Buffer
	if err := Emit(unit, unit.Snapshot(), entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed on the cross-module generic struct data-field fixture; structTypeParameters must substitute the declaration's own parameters, not a generic-function context's: %v", err)
	}
	out := buf.String()
	if strings.Contains(out, "type-parameter") {
		t.Errorf("emitted C still carries an unsubstituted type-parameter field:\n%s", out)
	}
	// The Inner[int] typedef's val field must be the concrete int32_t (the
	// shape that failed before the fix).
	if !strings.Contains(out, "    int32_t pebble_field_") {
		t.Errorf("emitted C missing Inner[int]'s concrete int32_t val field:\n%s", out)
	}
	// The end-to-end proof: reading o.inner.val through both struct layers
	// must compile under -Wall -Wextra -Werror and exit 5.
	compileAndRun(t, buf.Bytes(), 5, false)
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

func TestEmitBitwiseOperatorsCompilesAndRuns(t *testing.T) {
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"and", "fn main() i32 { let a i32 = 12; let b i32 = 10; return a & b; }", 8},
		{"or", "fn main() i32 { let a i32 = 12; let b i32 = 10; return a | b; }", 14},
		{"xor", "fn main() i32 { let a i32 = 12; let b i32 = 10; return a ^ b; }", 6},
		{"not", "fn main() i32 { let a i32 = 10; return (~a) & 15; }", 5},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitBitwiseCombinedExpressionCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a i32 = 12; let b i32 = 10; let c i32 = 3; return (a & b) | c; }", false, 11, false)
}

func TestEmitBitwiseI64CompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i64 { let a i64 = 12; let b i64 = 10; return (a ^ b) | 8; }", false, 14, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	err := Emit(unit, snapshot, entryID, nil, nil, &buf)
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

func TestEmitCompoundAddCompilesAndRuns(t *testing.T) {
	// The minimal repro: a compound assignment as an ordinary leading
	// statement. i += 1 lowers through the checked-arithmetic runtime helper
	// (pebble_rt_checked_add_i32), never a raw C `+=`, so i goes 5 -> 6 and the
	// process exits with the combined value 6.
	emitAndRun(t, "fn main() i32 { var i i32 = 5; i += 1; return i; }", false, 6, false)
}

func TestEmitCompoundAllOperatorsCompileAndRun(t *testing.T) {
	// Every compound operator in the language's set — +=, -=, *=, /=, %= — must
	// combine exactly like the corresponding x = x <op> y, since each lowers
	// through the same checked-arithmetic runtime helper buildExpr's
	// CheckedArithmetic case uses. Each case's expected value is independently
	// computed, not copied from the emission.
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"add", "fn main() i32 { var i i32 = 5; i += 4; return i; }", 9},
		{"sub", "fn main() i32 { var i i32 = 9; i -= 4; return i; }", 5},
		{"mul", "fn main() i32 { var i i32 = 3; i *= 7; return i; }", 21},
		{"div", "fn main() i32 { var i i32 = 21; i /= 3; return i; }", 7},
		{"mod", "fn main() i32 { var i i32 = 22; i %= 7; return i; }", 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

func TestEmitCompoundLoweringGoesThroughCheckedHelper(t *testing.T) {
	// The single most important correctness property: a compound assignment
	// must NOT emit a raw C `+=` (which would compile and "work" for in-range
	// values while silently dropping the overflow check). The emitted C for
	// `i += 1` must be the same pebble_rt_checked_add_i32 call a plain
	// `i = i + 1` emits, with the lvalue read as the helper's left operand and
	// written back as the assignment target.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 5; i += 1; return i; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_local_25 = pebble_rt_checked_add_i32(pebble_local_25, 1, (PebbleSourceLoc){\"main.peb\", 1, 32})") {
		t.Fatalf("emitted C does not combine through the checked helper:\n%s", out)
	}
	if strings.Contains(out, " += ") {
		t.Fatalf("emitted C uses a raw C += instead of the checked helper:\n%s", out)
	}
}

func TestEmitPostfixIncrementCompileAndRun(t *testing.T) {
	// A postfix i++ lowers through the SAME CompoundStore path as += (the
	// checker builds a postfix update as a CompoundStore with + and a
	// literal-one value child), so it must work everywhere a += does — here as
	// an ordinary leading statement. Two increments make i go 4 -> 6.
	emitAndRun(t, "fn main() i32 { var i i32 = 4; i++; i++; return i; }", false, 6, false)
}

func TestEmitPostfixDecrementCompileAndRun(t *testing.T) {
	// A postfix i-- is the -= twin: a CompoundStore with - and a literal-one
	// value child, emitted as pebble_rt_checked_sub_i32(pebble_local_<i>, 1),
	// making i go 6 -> 5.
	emitAndRun(t, "fn main() i32 { var i i32 = 6; i--; return i; }", false, 5, false)
}

func TestEmitCompoundOverflowStillAborts(t *testing.T) {
	// 2147483647 += 1 overflows i32. The overflow must survive through a
	// compound assignment, exactly as it does through a plain reassignment:
	// i += 1 lowers to pebble_local_<i> = pebble_rt_checked_add_i32(
	// pebble_local_<i>, 1), which must panic through pebble_rt_panic in
	// PEBBLE_RT_MODE_SAFE — the process must terminate abnormally, not exit 0
	// and not return a silently wrapped value. This is the proof that the
	// emission goes through the checked runtime helper rather than a naive
	// unchecked C `i += 1`.
	emitAndRun(t, "fn main() i32 { var i i32 = 2147483647; i += 1; return i; }", false, 0, true)
}

func TestEmitCheckedShiftsCompileAndRun(t *testing.T) {
	emitAndRun(t, "fn main() i32 { return (3 << 4) >> 2; }", false, 12, false)
	emitAndRun(t, "fn main() i64 { return (3 << 4) >> 2; }", false, 12, false)
}

func TestEmitCheckedShiftAcceptsNarrowerAmount(t *testing.T) {
	emitAndRun(t, "fn main() i32 { var amount u8 = 2; return 1 << amount; }", false, 4, false)
}

func TestEmitCheckedShiftOutOfRangeAbortsInSafeMode(t *testing.T) {
	emitAndRun(t, "fn main() i32 { return 1 << 32; }", false, 0, true)
}

func TestEmitCheckedShiftMasksCountInReleaseMode(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { return 1 << 35; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 8, false, false)
}

func TestEmitCheckedShiftNegativeCountMasksInReleaseMode(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var one i32 = 1; var amount i32 = -1; return (one << amount) >> 31; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 255, false, false)
}

func TestEmitPostfixIncrementOverflowStillAborts(t *testing.T) {
	// A postfix i++ overflows identically to i += 1 (it IS i += 1 at the IR
	// level): 2147483647++ must panic through pebble_rt_checked_add_i32 in
	// PEBBLE_RT_MODE_SAFE, not silently wrap.
	emitAndRun(t, "fn main() i32 { var i i32 = 2147483647; i++; return i; }", false, 0, true)
}

func TestEmitCompoundDivideByZeroStillAborts(t *testing.T) {
	// i /= 0 must fault through pebble_rt_checked_div_i32 exactly like a plain
	// i = i / 0 — divide-by-zero is a checked-semantics property a compound
	// assignment must preserve, not a raw C `/=` which would be UB.
	emitAndRun(t, "fn main() i32 { var i i32 = 7; i /= 0; return i; }", false, 0, true)
}

func TestEmitCompoundReleaseWrapsOverflow(t *testing.T) {
	// In PEBBLE_RT_MODE_RELEASE the checked add wraps instead of panicking
	// (same helper, different mode): 2147483647 += 1 wraps i to INT32_MIN, so
	// the follow-on `if i < 0` is true and the process exits 77. A naive raw
	// C `i += 1` on INT32_MAX is undefined behavior, so this test proves the
	// wrap is happening through the runtime's own checked helper.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 2147483647; i += 1; if i < 0 { return 77; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 77, false, false)
}

func TestEmitCompoundIndexedPlaceCompilesAndRuns(t *testing.T) {
	// A compound assignment to an array element — arr[i] += 5 — is a
	// CompoundStore whose place is a CheckedIndexPlace, emitted through the
	// same buildPlaceLValue machinery a plain indexed Store uses: the lvalue is
	// arr[pebble_rt_checked_index_i32(...)], read into the checked helper and
	// written back. i = 1 picks the 20 element, +5 makes it 25, returned.
	emitAndRun(t, "fn main() i32 { var arr [3]i32 = [10, 20, 30]; var i i32 = 1; arr[i] += 5; return arr[i]; }", false, 25, false)
}

func TestEmitCompoundIndexedPlaceEvaluatesIndexOnce(t *testing.T) {
	// The index call mutates count, so returning count proves whether the
	// compound store evaluates its place once (1) or twice (2).
	emitAndRun(t, "fn bump_and_get_index(p *i32) i32 { *p = *p + 1; return 0; } fn main() i32 { var count i32 = 0; var arr [1]i32 = [0]; arr[bump_and_get_index(&count)] += 1; return count; }", false, 1, false)
}

func TestEmitCompoundFieldPlaceCompilesAndRuns(t *testing.T) {
	// A compound assignment to a struct field — c.count -= 2 — is a
	// CompoundStore whose place is a FieldPlace (a struct field of exactly the
	// entry's width), emitted through the same buildPlaceLValue machinery a
	// plain field Store uses: pebble_local_<c>.pebble_field_<count>, read into
	// the checked helper and written back. 10 - 2 = 8, returned.
	emitAndRun(t, `type Counter = struct { count i32; }; fn main() i32 { var c Counter = Counter.{ count = 10 }; c.count -= 2; return c.count; }`, false, 8, false)
}

func TestEmitCompoundInLoopBodyCompilesAndRuns(t *testing.T) {
	// A compound assignment and a postfix increment inside a loop body (a
	// fall-through statement sequence) route through buildLeadingStatement
	// exactly like a Store does: sum accumulates i via sum += i and i advances
	// via i++, so sum = 0+1+2+3+4 = 10, returned as the exit code. Bounded
	// execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { sum += i; i++; } return sum; }", false, 10, false)
}

func TestEmitCompoundI64LocalCombinesViaI64Helper(t *testing.T) {
	// A compound assignment combines at the local's own declared width, not the
	// entry's: an i64 local inside an i64 entry combines through the _i64
	// checked helper (the checkedSuffix selection mirrors buildStoreCore's
	// targetInfo.kind dispatch). The runtime exit code cannot carry a full i64
	// (the low byte is all the OS sees), so the i64 story is proven by the
	// emitted helper name here and by the i64-boundary overflow abort in
	// TestEmitCompoundI64OverflowStillAborts.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { var x i64 = 9223372036854775800; x += 5; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_add_i64(pebble_local_25, 5") {
		t.Fatalf("emitted C does not combine at i64 through the checked helper:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_checked_add_i32") {
		t.Fatalf("emitted C combines at i32 instead of the local's own i64 width:\n%s", out)
	}
}

func TestEmitCompoundI64OverflowStillAborts(t *testing.T) {
	// 9223372036854775807 += 1 overflows i64 and must panic through
	// pebble_rt_checked_add_i64 in PEBBLE_RT_MODE_SAFE — the i64 checked-helper
	// suffix really is selected for a compound assignment, not the i32 twin.
	emitAndRun(t, "fn main() i64 { var x i64 = 9223372036854775807; x += 1; return x; }", false, 0, true)
}

func TestEmitCompoundI64LocalInsideI32Function(t *testing.T) {
	// An i64 local inside an i32 entry combines at i64 (buildExpr at the
	// local's own width, the _i64 checked helper) even though the entry's
	// resolved width is i32 — the same width independence a plain i64
	// reassignment already has. 21 += 21 makes 42, returned as i32 after the
	// cast.
	emitAndRun(t, "fn main() i32 { var y i64 = 21; y += 21; return y as i32; }", false, 42, false)
}

func TestEmitCompoundFloatLocalCompilesAndRuns(t *testing.T) {
	// A float compound assignment (checker-reachable: the -=, *=, /= families
	// are NumericSame and += is Add, both admitting floats) combines with the
	// same plain C operator buildFloatExpr's BinaryValue case uses — floats
	// have no checked arithmetic anywhere in this backend, so x += 1.0 emits
	// x = (x + 1.0). 2.5 += 1.0 = 3.5, truncated to exit code 3 by the C
	// float-to-int conversion of the process exit.
	emitAndRun(t, "fn main() f32 { var x f32 = 2.5; x += 1.0; return x; }", false, 3, false)
}

func TestEmitPostfixIncrementFloatCompilesAndRuns(t *testing.T) {
	// A postfix ++ on a float local lowers through the same CompoundStore path
	// (the checker's buildPostfixOne synthesizes a 1.0 float literal for a
	// float place), combining with the plain C operator: x goes 1.5 -> 2.5,
	// truncated to exit code 2.
	emitAndRun(t, "fn main() f64 { var x f64 = 1.5; x++; return x; }", false, 2, false)
}

func TestEmitCompoundDereferencePlaceCompilesAndRuns(t *testing.T) {
	// A compound assignment through a pointer — *p += 3 — is a CompoundStore
	// whose place is a DereferencePlace, emitted through the same
	// buildPlaceLValue machinery a plain write-through-pointer Store uses (the
	// null-checked dereference is the lvalue, read into the checked helper and
	// written back). v goes 7 -> 10, returned.
	emitAndRun(t, "fn main() i32 { var v i32 = 7; var p *i32 = &v; *p += 3; return v; }", false, 10, false)
}

func TestEmitCompoundSliceElementCompilesAndRuns(t *testing.T) {
	// A compound assignment to a slice element — s[0] += 4 — is a
	// CompoundStore whose place is a CheckedIndexPlace over a slice base,
	// emitted as s.data[pebble_rt_checked_index_i32(...)] both read and
	// written. The write lands in the backing array, so values[0] goes 1 -> 5.
	emitAndRun(t, "fn main() i32 { var values [3]i32 = [1, 2, 3]; let s []i32 = values[:]; s[0] += 4; return values[0]; }", false, 5, false)
}

func TestEmitDeferredCompoundStoreCompilesAndRuns(t *testing.T) {
	// A deferred compound assignment — defer x += 1; — routes through the same
	// buildCompoundStore a non-deferred compound assignment uses (the deferred
	// position accepts a CompoundStore exactly as it accepts a Store), so the
	// deferred statement runs just before the return and x goes 5 -> 6.
	emitAndRun(t, "fn main() i32 { var x i32 = 5; defer x += 1; return x; }", false, 6, false)
}

func TestEmitForLoopNoConditionCompoundUpdateCompilesAndRuns(t *testing.T) {
	// A lone no-condition for whose only clause is a CompoundStore is the
	// update-only shape, exactly like a lone no-condition Store: for ; ; i += 2
	// advances i by checked-add of 2 until the in-body break at i >= 3 fires at
	// i = 4, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; for ; ; i += 2 { if i >= 3 { break; } } return i; }", false, 4, false)
}

func TestEmitCompoundInsideIfArmAndSwitchCaseCompilesAndRuns(t *testing.T) {
	// A compound assignment in an if-arm and in a switch case body — the
	// fall-through statement-sequence positions b5be90d unified — routes through
	// the shared buildLeadingStatement exactly like a Store does: x starts 1,
	// the if-arm does x += 5 (x = 6), the switch case does x *= 3 (x = 18),
	// returned. Bounded execution is unnecessary (no loop) but harmless.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; if x < 10 { x += 5; } switch x { case 6: x *= 3; else: x = 0; } return x; }", false, 18, false)
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

func TestEmitPrintInsideWhileBodyCompilesAndRuns(t *testing.T) {
	// A print statement inside a loop body (legal source) is now a supported
	// statement kind: the loop body's statement switch routes it through the
	// shared buildPrint, so the body prints "hi" once per iteration (three
	// iterations) and returns the final i = 3 as the exit code. Bounded
	// execution in case of a miscompiled loop.
	out := emitAndRunCaptureBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { print(\"hi\"); i = i + 1; } return i; }", false, 3, false)
	if want := "hi\nhi\nhi\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitReturnsGlobalLetConstantCompilesAndRuns(t *testing.T) {
	// A reference to a top-level `let` global constant is no longer a bare
	// SymbolValue naming storage the backend has no mechanism to lower (see
	// globalLetInitializers in ir_builder.go) — it's inlined to a fresh copy
	// of its initializer at each reference site, so `return x;` for a global
	// `let x i32 = 1;` now lowers to the same IntegerLiteral shape `return
	// 1;` would, which was always a supported return value. This supersedes
	// the old TestEmitRejectsVariableReturn, which asserted the PRE-fix
	// rejection for this exact shape.
	emitAndRun(t, "let x i32 = 1; fn main() i32 { return x; }", false, 1, false)
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

func TestEmitPrintBeforeReturnCompilesAndRuns(t *testing.T) {
	// A print statement before the final Return is a supported leading
	// statement in the entry body block: the print emits its single printf
	// line ("hi" plus the statement's trailing newline), then the block's
	// tail returns 1. Asserting the captured stdout confirms the printed text
	// is exactly one line — not just that the program compiled and exited.
	out := emitAndRunCapture(t, "fn main() i32 { print(\"hi\"); return 1; }", false, 1, false)
	if want := "hi\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
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
	if err := Emit(nil, snapshot, 0, nil, nil, &bytes.Buffer{}); err == nil {
		t.Fatal("Emit accepted nil unit")
	}
	if err := Emit(empty, nil, 0, nil, nil, &bytes.Buffer{}); err == nil {
		t.Fatal("Emit accepted nil snapshot")
	}
	if err := Emit(empty, snapshot, 0, nil, nil, nil); err == nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitDeferredPrintBeforeBreakCompilesAndRuns(t *testing.T) {
	// A real-source break inside a loop body that also contains a `defer
	// print 5;` produces a Break whose DeferChain references a Print node. The
	// backend emits the deferred print through the shared buildPrint before
	// the break, so the loop's first (and only) iteration prints "5" and then
	// breaks, exiting with i = 0. Bounded execution in case of a miscompiled
	// loop.
	out := emitAndRunCaptureBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { break; defer print 5; } return 0; }", false, 0, false)
	if want := "5\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitDeferredPrintBeforeContinueCompilesAndRuns(t *testing.T) {
	// Same deferred-print path for Continue: each continue fires the deferred
	// `print 5` at the loop's next pass. The body increments i and continues
	// every pass (i becomes 1, 2, then 3), so the deferred print fires three
	// times before the loop condition fails at i = 3 and the program returns
	// 0. Bounded execution in case of a miscompiled loop.
	out := emitAndRunCaptureBounded(t, "fn main() i32 { var i i32 = 0; while i < 3 { i = i + 1; continue; defer print 5; } return 0; }", false, 0, false)
	if want := "5\n5\n5\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

// print statement emission — every operand type family the checker allows
// (C0612 restricts print operands to bool, char, str, any integer builtin, or
// any float builtin), one combined printf per print statement ending in \n,
// matching v1's print codegen shape.

func TestEmitPrintIntegerLiteralCompilesAndRuns(t *testing.T) {
	// The simplest print: a bare integer literal operand. A bare literal in a
	// print operand resolves to the unanchored int builtin (int32_t), so the
	// format specifier comes from PRId32; the captured output is exactly the
	// value plus the statement's single trailing newline.
	out := emitAndRunCapture(t, "fn main() i32 { print 42; return 0; }", false, 0, false)
	if want := "42\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitPrintEachIntegerWidthCompilesAndRuns(t *testing.T) {
	// Every integer builtin width, each through a local of that declared
	// width: the print dispatches on the operand's own resolved kind, so each
	// width gets its own PRId*/PRIu* specifier matching its fixed-width C
	// type — and each must compile clean under -Wall -Wextra -Werror, which is
	// exactly where a hand-picked (wrong) specifier would surface. All print
	// the same value 7.
	for _, tc := range []struct {
		width string
	}{
		{"i8"}, {"i16"}, {"i32"}, {"i64"}, {"u8"}, {"u16"}, {"u32"}, {"u64"},
	} {
		t.Run(tc.width, func(t *testing.T) {
			src := "fn main() i32 { let x " + tc.width + " = 7; print x; return 0; }"
			out := emitAndRunCapture(t, src, false, 0, false)
			if want := "7\n"; out != want {
				t.Fatalf("compiled program output = %q, want %q", out, want)
			}
		})
	}
}

func TestEmitUnsuffixedU64MaxLiteralCompilesAndRuns(t *testing.T) {
	// The exact reported bug: a decimal literal that does not fit in any
	// signed C integer type (UINT64_MAX's decimal form) assigned to a u64
	// local. Before the fix the emitted C was `uint64_t pebble_local_N =
	// 18446744073709551615;` — the unsuffixed literal made cc fail under the
	// mandated -Wall -Wextra -Werror via -Wimplicitly-unsigned-literal. The
	// emitted C must carry a "u" suffix and the program must compile, run,
	// and print the full value, not a truncated or misinterpreted one.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var y u64 = 18446744073709551615; print y; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "18446744073709551615u") {
		t.Fatalf("emitted C does not spell the u64 literal with an unsigned suffix:\n%s", out)
	}
	if got := compileAndRunCapture(t, buf.Bytes(), 0, false); got != "18446744073709551615\n" {
		t.Fatalf("compiled program output = %q, want %q", got, "18446744073709551615\n")
	}
}

func TestEmitUnsuffixedU32MaxLiteralCompilesAndRuns(t *testing.T) {
	// The same large-literal-at-unsigned-width shape at u32: UINT32_MAX's
	// decimal form in a u32 local. Unsuffixed it exceeds a signed 32-bit
	// literal's range and cc would warn under -Wall -Wextra -Werror; with the
	// "u" suffix it is a plain unsigned int constant. The emitted C must use
	// the suffix and the program must print the full value.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var y u32 = 4294967295; print y; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "4294967295u") {
		t.Fatalf("emitted C does not spell the u32 literal with an unsigned suffix:\n%s", out)
	}
	if strings.Contains(out, " = 4294967295;") {
		t.Fatalf("emitted C contains an unsuffixed UINT32_MAX literal:\n%s", out)
	}
	if got := compileAndRunCapture(t, buf.Bytes(), 0, false); got != "4294967295\n" {
		t.Fatalf("compiled program output = %q, want %q", got, "4294967295\n")
	}
}

func TestEmitSmallUnsignedLiteralRegressionCompilesAndRuns(t *testing.T) {
	// The already-working small-literal case must keep working: a value well
	// inside signed range at u64 width still compiles clean (now with a
	// harmless "u" suffix) and prints the correct value.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var y u64 = 123456789; print y; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	if got := compileAndRunCapture(t, buf.Bytes(), 0, false); got != "123456789\n" {
		t.Fatalf("compiled program output = %q, want %q", got, "123456789\n")
	}
}

func TestEmitLargeSignedLiteralNoUnsignedSuffix(t *testing.T) {
	// A large SIGNED literal (INT64_MAX) must not gain an unsigned suffix:
	// the emitted C must keep the plain decimal text so the value is the
	// signed maximum, not an unsigned constant of the same digits. The
	// program must compile under -Wall -Wextra -Werror (a bare INT64_MAX
	// decimal is exactly representable as signed long long, so it is
	// warning-free unsuffixed) and print the correct value.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let y i64 = 9223372036854775807; print y; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if strings.Contains(out, "9223372036854775807u") {
		t.Fatalf("emitted C gives the signed i64 literal an unsigned suffix:\n%s", out)
	}
	if !strings.Contains(out, "9223372036854775807") {
		t.Fatalf("emitted C is missing the signed i64 literal:\n%s", out)
	}
	if got := compileAndRunCapture(t, buf.Bytes(), 0, false); got != "9223372036854775807\n" {
		t.Fatalf("compiled program output = %q, want %q", got, "9223372036854775807\n")
	}
}

func TestEmitPrintBoolCompilesAndRuns(t *testing.T) {
	// A bool operand prints as the word true or false (v1's approach: the
	// bool expression wrapped in a C ternary selecting the const char *
	// literal), not as 1/0. Covers both literal operands and a bool-typed
	// local operand.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"true literal", "fn main() i32 { print true; return 0; }", "true\n"},
		{"false literal", "fn main() i32 { print false; return 0; }", "false\n"},
		{"true local", "fn main() i32 { let b bool = true; print b; return 0; }", "true\n"},
		{"false local", "fn main() i32 { let b bool = false; print b; return 0; }", "false\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintCharCompilesAndRuns(t *testing.T) {
	// A char operand prints as the single character its int32_t value encodes
	// (specifier %c), covering both a char literal and a char-typed local.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"literal", "fn main() i32 { print 'x'; return 0; }", "x\n"},
		{"local", "fn main() i32 { let c char = 'x'; print c; return 0; }", "x\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintStrCompilesAndRuns(t *testing.T) {
	// A str operand prints its bytes (specifier %s on the PebbleStr's .data
	// field), covering a string literal, a str-typed local, and the
	// parenthesized literal form `print ("hi")`, whose operand arrives wrapped
	// in a tir.SourceAlias that buildPrint unwraps before dispatching.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"literal", "fn main() i32 { print \"hello\"; return 0; }", "hello\n"},
		{"local", "fn main() i32 { let s str = \"hello\"; print s; return 0; }", "hello\n"},
		{"parenthesized literal", "fn main() i32 { print (\"hi\"); return 0; }", "hi\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintFloatCompilesAndRuns(t *testing.T) {
	// A float operand prints with %f (f32/f64 promote to double in a variadic
	// call either way, so %f covers both, matching v1). A bare literal resolves
	// to f64; an f32-typed local prints through the float grammar at its own
	// kind. %f's default precision is 6, so 3.5 prints as 3.500000.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"f64 literal", "fn main() i32 { print 3.5; return 0; }", "3.500000\n"},
		{"f64 local", "fn main() i32 { let x f64 = 3.5; print x; return 0; }", "3.500000\n"},
		{"f32 local", "fn main() i32 { let x f32 = 3.5; print x; return 0; }", "3.500000\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintMultipleOperandsOneLineCompilesAndRuns(t *testing.T) {
	// `print a, b, c;` — multiple comma-separated operands print on ONE line
	// with no separator between them (matching v1): all operands share one
	// printf call, one format string, one argument list, one trailing newline.
	// Mixed operand types exercise that the format specifiers and arguments are
	// assembled in operand order.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"three ints", "fn main() i32 { print 1, 2, 3; return 0; }", "123\n"},
		{"three strings", "fn main() i32 { print \"a\", \"b\", \"c\"; return 0; }", "abc\n"},
		{"mixed", "fn main() i32 { print 1, \" \", 2, \" \", 3.25; return 0; }", "1 2 3.250000\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintWritesSingleCombinedPrintf(t *testing.T) {
	// The emitted C for a mixed-type print must be exactly ONE printf call per
	// print statement whose format string concatenates one specifier per
	// operand in order (integer via the out-of-quotes "%"PRId32 macro
	// spelling, bool/char/str/float as %s/%c/%s/%f literals) and ends in the
	// literal \n, with the same number of comma-separated arguments in operand
	// order. Asserting the literal C text is what proves the one-call
	// combined shape, not a per-operand call. The operand texts are confirmed
	// against the fixture dump: the char literal 'x' emits (int32_t)120, the
	// string literal "hi" emits its PebbleStr compound literal's .data field
	// cast to const char *, and the bool emits the "true"/"false" ternary.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { print 1, true, 'x', \"hi\", 3.5; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if count := strings.Count(out, "printf("); count != 1 {
		t.Errorf("emitted C has %d printf( calls, want exactly one:\n%s", count, out)
	}
	for _, want := range []string{
		"printf(\"%\"PRId32\"%s\"\"%c\"\"%s\"\"%f\"\"\\n\", 1, (true ? \"true\" : \"false\"), (int32_t)120, (const char *)(PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 }.data, 3.5);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitPrintInVoidHelperCompilesAndRuns(t *testing.T) {
	// A print inside a reachable void helper, emitted as that helper's own C
	// statement: the reachability walk finds no call inside the Print's
	// operands here (all literals), and the helper body builds its leading
	// Print through the same buildLeadingStatement the entry uses. The entry
	// calls the helper, which prints "9" and returns void.
	out := emitAndRunCapture(t, "fn helper() void { print 9; }\nfn main() i32 { helper(); return 0; }", false, 0, false)
	if want := "9\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitPrintHelperCallOperandCompilesAndRuns(t *testing.T) {
	// A print operand that is a call to a helper: the operand's DirectCall is
	// reachable through the Print's child walk (so the helper is emitted), and
	// buildExpr/buildStrOperand build the call at the operand's own resolved
	// type. Covers an integer-returning helper and a str-returning helper.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"int helper", "fn helper() i32 { return 6; }\nfn main() i32 { print helper(); return 0; }", "6\n"},
		{"str helper", "fn helper() str { return \"hey\"; }\nfn main() i32 { print helper(); return 0; }", "hey\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitDeferredPrintAtVoidHelperExitCompilesAndRuns(t *testing.T) {
	// A function-level `defer print 7;` in a void helper fires at the helper's
	// ImplicitReturn exit (the tail emits its DeferChain before falling off
	// the end of the C function), so calling the helper prints "7" and the
	// entry returns 0.
	out := emitAndRunCapture(t, "fn helper() void { defer print 7; }\nfn main() i32 { helper(); return 0; }", false, 0, false)
	if want := "7\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitPrintInsideLoopIfCompilesAndRuns(t *testing.T) {
	// A print inside a loop-body if's arm routes through buildLoopBody -> the
	// Print case -> buildPrint, so it composes with the if-in-loop-body shape.
	// The arm fires once (when i == 1), printing "mark", and the loop returns
	// 2. Bounded execution in case of a miscompiled loop.
	out := emitAndRunCaptureBounded(t, "fn main() i32 { var i i32 = 0; while i < 2 { i = i + 1; if i == 1 { print \"mark\"; } } return i; }", false, 2, false)
	if want := "mark\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitForLoopCompoundStoreUpdateCompilesAndRuns(t *testing.T) {
	// A compound-assignment as the for-loop update (step += 1) is now a
	// supported update shape: the for-header update clause accepts a
	// CompoundStore exactly as it accepts a Store, emitting the same
	// pebble_rt_checked_add_i32 call buildCompoundStore produces, so step
	// counts 0..2, total = 0+1+2 = 3, returned as the exit code.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; step += 1 { total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopIndexedCompoundUpdateEvaluatesPlacePerUpdate(t *testing.T) {
	// A non-plain compound update cannot put its declaration in the C for
	// header. The emitter declares the pointer before the loop and assigns its
	// address in the update expression, so the changing index is evaluated once
	// on the iteration where the update runs.
	emitAndRunBounded(t, "fn main() i32 { var arr [2]i32 = [0, 0]; var i i32 = 0; for ; i < 1; arr[i] += 1 { i = i + 1; } return arr[1]; }", false, 1, false)
}

func TestEmitForLoopPostfixIncrementUpdateCompilesAndRuns(t *testing.T) {
	// A postfix i++ as the for-loop update lowers through the same CompoundStore
	// path as step += 1 (the checker builds a for-update postfix as a
	// CompoundStore with + and a literal-one value child), so i counts 0..2 and
	// total = 0+1+2 = 3, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0; i < 3; i++ { total = total + i; } return total; }", false, 3, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitI64NowDeclaresI32Local(t *testing.T) {
	// Stage 2's fix inverted this test's original assertion: an i32 local
	// inside an i64 entry was previously a clean width-mismatch rejection and
	// is now a legal, supported declaration (the local's own declared width,
	// not the entry's, governs its C type). The fixture declares the i32
	// local and returns the i64 constant, never leaking the i32 value without
	// a cast.
	emitAndRun(t, "fn main() i64 { let x i32 = 1; return 2; }", false, 2, false)
}

func TestEmitI32NowDeclaresI64Local(t *testing.T) {
	// Stage 2's fix inverted this test's original assertion: an i64 local
	// inside an i32 entry was previously a clean width-mismatch rejection and
	// is now a legal, supported declaration (the local's own declared width,
	// not the entry's, governs its C type and its initializer's ambient build
	// width). The fixture declares the i64 local and returns the i32 constant,
	// never leaking the i64 value without a cast.
	emitAndRun(t, "fn main() i32 { let x i64 = 1; return 2; }", false, 2, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitCastsI64HelperResultToI32Main(t *testing.T) {
	emitAndRun(t, "fn helper() i64 { return 21; } fn main() i32 { return helper() as i32; }", false, 21, false)
}

func TestEmitBuildsI64HelperBodyAtItsOwnWidth(t *testing.T) {
	emitAndRun(t, "fn helper() i64 { let value i64 = 20; return value + 1; } fn main() i32 { return helper() as i32; }", false, 21, false)
}

func TestEmitCastsI32HelperResultToI64Main(t *testing.T) {
	emitAndRun(t, "fn helper() i32 { return 7; } fn main() i64 { return (helper() as i64) + 1; }", false, 8, false)
}

func TestEmitCastsU32HelperResultToI32Main(t *testing.T) {
	emitAndRun(t, "fn helper() u32 { return 7; } fn main() i32 { return helper() as i32; }", false, 7, false)
}

func TestEmitDeclaresI64LocalInsideI32Function(t *testing.T) {
	// Stage 2's minimal repro: a local declared at a different integer width
	// than its own function. var y i64 = 100; inside an i32-returning
	// function is a plain i64 local used only internally (never returned or
	// leaked without a cast): buildScalarInitializeCore emits it as a C
	// int64_t (the local's own declared width, not the entry's i32), and the
	// function returns x unchanged as the exit code.
	emitAndRun(t, "fn main() i32 { var x i32 = 5; var y i64 = 100; return x; }", false, 5, false)
}

func TestEmitI64LocalsArithmeticInsideI32Function(t *testing.T) {
	// A local of the other width actually used in arithmetic with other
	// locals of that same other width, not just declared and ignored: two
	// i64 locals inside an i32 function are added together at i64 (buildExpr
	// builds both operands at the locals' own i64 width, lowering through the
	// i64 checked-arithmetic helper) and the explicitly-cast result is
	// returned as the exit code.
	emitAndRun(t, "fn main() i32 { var a i64 = 21; var b i64 = 21; return (a + b) as i32; }", false, 42, false)
}

func TestEmitReassignsI64LocalInsideI32Function(t *testing.T) {
	// A reassignment (a Store), not just the initial Initialize: an i64
	// local declared inside an i32 function is reassigned later in the same
	// body. buildStoreCore must build the new value at the local's own
	// recorded i64 width (not the entry's i32) and emit
	// `pebble_local_<sym> = <value>`; the reassigned value is then cast back
	// to i32 for the exit code.
	emitAndRun(t, "fn main() i32 { var y i64 = 100; y = 7; return y as i32; }", false, 7, false)
}

func TestEmitDeclaresU32LocalInsideI32Function(t *testing.T) {
	// A uint-family local (u32, not the i32/i64 pair) to confirm the fix is
	// generic across integer widths rather than hardcoded: a u32 local is
	// declared and then reassigned inside an i32 function, its value cast
	// back to i32 for the exit code. (Arithmetic on u32 is deliberately
	// avoided: the backend's checked-arithmetic helpers only cover i32/i64.)
	emitAndRun(t, "fn main() i32 { var a u32 = 40; a = 2; return a as i32; }", false, 2, false)
}

func TestEmitRejectsBareI64LocalReferenceInI32Context(t *testing.T) {
	// Regression: a bare (uncast) reference to a mismatched-width local used
	// where the width matters must still be a clean rejection, never a silent
	// coercion. The checker accepts `return y;` (an i64 local from an
	// i32-returning function), but the backend's buildExpr width gate rejects
	// the SymbolValue's own resolved i64 type against the ambient i32,
	// naming the i32 it wanted.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var y i64 = 100; return y; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32")
}

func TestEmitI64ForInitClauseInsideI32FunctionCompilesAndRuns(t *testing.T) {
	// The for-header initializer reuses buildScalarInitializeCore, so the
	// mismatched-width local fix covers it automatically: a classic for loop
	// whose init clause declares an i64 local inside an i32 function. The i64
	// local is only ever referenced through an explicit `as i32` cast (the
	// bare reference would fail the entry-width gate, as the test above
	// proves), and the loop accumulates 21 three times into an i32 counter.
	// Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; var i i32 = 0; for var limit i64 = 21; i < 3; i = i + 1 { total = total + (limit as i32); } return total; }", false, 63, false)
}

func TestEmitF64LocalDeclaresAndReturns(t *testing.T) {
	// Float Stage A's minimal repro (the required test 1): an f64 local is
	// declared, read back by a bare reference (the Return's SymbolValue), and
	// returned from an f64-returning main. buildScalarInitializeCore must emit
	// the local at its own float C type double, buildFloatExpr must both
	// accept the 3.14 FloatLiteral initializer and the SymbolValue reading the
	// local back, and buildBlock's tail-return float dispatch must emit
	// `return pebble_local_<sym>;`. The compile-and-run check asserts the
	// harness's observable contract: the hosted int main narrows the returned
	// double to the process exit code by C's float-to-integer truncation, so
	// 3.14 exits 3. That single code cannot distinguish 3.14 from (say) 3.0,
	// so the emitted-C assertions below pin the real value: the local is a C
	// double initialized from the untouched literal text 3.14, and the return
	// value is the local by name rather than a re-emitted (possibly
	// re-rounded) literal.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x f64 = 3.14; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"double pebble_local_",
		"= 3.14;",
		"return pebble_local_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 3, false)
}

func TestEmitF32LocalDeclaresAndReturns(t *testing.T) {
	// Confirms the float case is not hardcoded to one width (required test
	// 2): an f32 local (not f64) is declared and returned from an f32-
	// returning main. buildScalarInitializeCore must pick floatCType(F32) =
	// float for the C declaration, and buildFloatExpr must build the 3.9
	// initializer and the read-back SymbolValue at the f32 kind. f32 round-
	// trips as a C float (3.9f), and the hosted main truncates it to exit
	// code 3.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f32 { var x f32 = 3.9; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"float pebble_local_",
		"= 3.9;",
		"return pebble_local_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 3, false)
}

func TestEmitF64HelperParamAndReturnCompilesAndRuns(t *testing.T) {
	// The required f64-helper-parameter/return test: a reachable helper takes
	// an f64 parameter, multiplies it by an f64 literal (real float
	// arithmetic over the parameter), and returns an f64 result, called from
	// the entry with an f64 literal argument, the result stored into an f64
	// local via a DirectCall initializer and printed. This exercises every
	// widened path at once: validateHelperSignature admitting the f64
	// parameter and result, helperSignature declaring the parameter as a C
	// double (seeded localInfo{kind: types.F64}) and the result as a C
	// double (resultInfo{kind: types.F64}), buildCallArgument building the
	// 2.5 argument via buildFloatExpr at the f64 kind, buildScalarInitializeCore
	// building the call initializer via buildFloatExpr's DirectCall case, and
	// buildReturnStatement building `return value * 2.0;` via buildFloatExpr's
	// BinaryValue case. 2.5 * 3.0 = 7.5, which prints as 7.500000.
	out := emitAndRunCapture(t, `
fn scale(value f64, factor f64) f64 { return value * factor; }
fn main() i32 { let result f64 = scale(2.5, 3.0); print result; return 0; }`, false, 0, false)
	if want := "7.500000\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitF32HelperParamAndReturnCompilesAndRuns(t *testing.T) {
	// The f32 twin of the f64 test: an f32 helper parameter and result are
	// declared as C float (not double) and built at the f32 kind. 1.5 * 4.0
	// = 6.0f, which prints as 6.000000 (f32 promotes to double in the
	// variadic %f print call).
	out := emitAndRunCapture(t, `
fn scale(value f32, factor f32) f32 { return value * factor; }
fn main() i32 { let result f32 = scale(1.5, 4.0); print result; return 0; }`, false, 0, false)
	if want := "6.000000\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitMixedIntFloatHelperParamsCompilesAndRuns(t *testing.T) {
	// A helper mixing an integer parameter and an f64 parameter — the exact
	// signature shape leibniz_pi_approx.peb's approximate_pi uses — so the
	// integer argument still flows through buildExpr at the entry width while
	// the float argument flows through buildFloatExpr at the f64 kind.
	// `(mult as f64) * base` mixes an IntegerToFloat cast into the float
	// arithmetic inside the helper body. 3 as f64 * 1.5 = 4.5.
	out := emitAndRunCapture(t, `
fn scaled(mult int, base f64) f64 { return (mult as f64) * base; }
fn main() int { let r f64 = scaled(3, 1.5); print r; return 0; }`, false, 0, false)
	if want := "4.500000\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitFloatHelperReturnForwardCompilesAndRuns(t *testing.T) {
	// A float-returning helper whose body returns another float-returning
	// helper's call directly (a return forward): buildReturnStatement's float
	// dispatch builds the DirectCall via buildFloatExpr's DirectCall case,
	// which must build the f64 argument (a reference to the outer helper's own
	// f64 parameter) at the f64 kind. outer(1.0) = inner(1.0) + 1.0 = 2.0.
	out := emitAndRunCapture(t, `
fn inner(x f64) f64 { return x + 1.0; }
fn outer(x f64) f64 { return inner(x); }
fn main() i32 { let r f64 = outer(1.0); print r; return 0; }`, false, 0, false)
	if want := "2.000000\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitFloatNegationInHelperCompilesAndRuns(t *testing.T) {
	// A float helper body that negates its f64 parameter (`return -x;`) and
	// an entry that negates a float local: the checker lowers a float negate
	// to a PrefixValue (only an integer negate becomes CheckedNegate), so
	// buildFloatExpr's PrefixValue case emits the plain C `-`. neg(2.5) =
	// -2.5, prints as -2.500000.
	out := emitAndRunCapture(t, `
fn neg(x f64) f64 { return -x; }
fn main() i32 { let r f64 = neg(2.5); let s f64 = -r; print s; return 0; }`, false, 0, false)
	if want := "2.500000\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitLeibnizPiApproximationCompilesAndRuns(t *testing.T) {
	// The leibniz_pi_approx.peb shape end-to-end, with the file's staleness
	// fixed (float -> f64, `n as f64` for the mixed-int/float term): an f64
	// helper taking an int and an f64 parameter accumulates the Leibniz
	// series in a while loop (breaking when the term drops below the
	// precision), returns pi as an f64, and the entry stores the result in an
	// f64 local, prints it alongside the actual pi and the error. This is the
	// exact program from the task; the checked-in example file itself must
	// stay untouched (it still uses the undefined `float` type), so this test
	// carries the fixed form. 100000 terms of the slowly-converging Leibniz
	// series give 3.1415826535897198 (about 1e-5 off), so the printed values
	// are pinned below.
	out := emitAndRunCaptureBounded(t, `
fn approximate_pi(max_terms int, precision f64) f64 {
    var sum f64 = 0.0;
    var n = 0;
    var sign = 1.0;
    while n < max_terms {
        var term f64 = sign / (2.0 * (n as f64) + 1.0);
        sum = sum + term;
        sign = -sign;
        n = n + 1;
        if term < 0.0 {
            term = -term;
        }
        if term < precision {
            break;
        }
    }
    return sum * 4.0;
}

fn main() int {
    let max_terms = 100000;
    let precision = 0.000001;
    let pi_approx = approximate_pi(max_terms, precision);
    let actual_pi = 3.1415926535;
    var error = actual_pi - pi_approx;
    if error < 0.0 {
        error = -error;
    }

    print "Approximated pi:"; print pi_approx;
    print "Actual pi:"; print actual_pi;
    print "Error:"; print error;

    return 0;
}`, false, 0, false)
	want := "Approximated pi:\n3.141583\nActual pi:\n3.141593\nError:\n0.000010\n"
	if out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitReassignsF64LocalAndReturns(t *testing.T) {
	// Required test 4: a Store reassigns an already-declared f64 local
	// (x = 2.5;), so buildStoreCore's float case must build the new value via
	// buildFloatExpr at the local's own recorded f64 kind and emit
	// `pebble_local_<sym> = 2.5;`. The exit code (2) is the truncation of the
	// reassigned 2.5, and the emitted-C assertion pins the literal 2.5 in the
	// Store (the declaration carried 1.25, so the 2.5 substring can only come
	// from the reassignment).
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x f64 = 1.25; x = 2.5; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"double pebble_local_",
		"= 1.25;",
		"pebble_local_",
		"= 2.5;",
		"return pebble_local_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitParenthesizedFloatExpressions(t *testing.T) {
	// SourceAlias unwrapping, the parenthesized-expression distinction: a
	// grouped float literal `(3.5)` as a local's initializer and a grouped
	// float reference `(x)` as the main's return value both arrive as
	// SourceAlias nodes, which buildFloatExpr transparently recurses through
	// (exactly as buildExpr and buildBoolExpr already do). (3.5 narrows to
	// exit code 3.)
	emitAndRun(t, "fn main() f64 { var x f64 = (3.5); return (x); }", false, 3, false)
}

func TestEmitF64LiteralTruncatesToExitCode(t *testing.T) {
	// The float value round-trips through the real emitted C and the harness's
	// process-exit observation, not just "it compiles": a fractional f64
	// whose C float-to-int truncation would land on a different code than a
	// rounding (or an integer-truncated literal) lowering would. 3.99 must
	// truncate to 3 (not round to 4), and the compile-and-run of the actual
	// emitted C asserts 3.
	emitAndRun(t, "fn main() f64 { var x f64 = 3.99; return x; }", false, 3, false)
}

func TestEmitFloatArithmeticInFloatReturnPosition(t *testing.T) {
	// Regression: Stage A rejected this BinaryValue because float arithmetic
	// was not yet in buildFloatExpr. Stage B now lowers it as plain C float
	// arithmetic, with no checked runtime helper needed.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { return 1.0 + 2.0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	if !strings.Contains(buf.String(), "1.0 + 2.0") {
		t.Fatalf("emitted C did not contain plain float addition:\n%s", buf.String())
	}
	compileAndRun(t, buf.Bytes(), 3, false)
}

func TestEmitFloatArithmeticOperatorsCompileAndRun(t *testing.T) {
	for _, test := range []struct {
		name, source, emitted string
		expected              int
	}{
		{"addition", "fn main() f64 { var a f64 = 1.5; var b f64 = 2.5; return a + b; }", " + ", 4},
		{"subtraction", "fn main() f64 { var a f64 = 5.5; var b f64 = 2.5; return a - b; }", " - ", 3},
		{"multiplication", "fn main() f64 { var a f64 = 1.5; var b f64 = 2.0; return a * b; }", " * ", 3},
		{"division", "fn main() f64 { var a f64 = 8.0; var b f64 = 2.0; return a / b; }", " / ", 4},
	} {
		t.Run(test.name, func(t *testing.T) {
			unit, snapshot, entryID, sources := buildFixture(t, test.source, "main", false)
			var buf bytes.Buffer
			if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
				t.Fatalf("Emit failed: %v", err)
			}
			if !strings.Contains(buf.String(), test.emitted) {
				t.Fatalf("emitted C missing plain float operator %q:\n%s", test.emitted, buf.String())
			}
			compileAndRun(t, buf.Bytes(), test.expected, false)
		})
	}
}

func TestEmitFloatComparisonBetweenLocalsCompilesAndRuns(t *testing.T) {
	// Float comparisons use the same BinaryValue condition path as integer
	// comparisons, but operands are built by buildFloatExpr at f64 width.
	emitAndRun(t, "fn main() f64 { var a f64 = 1.5; var b f64 = 2.5; if a < b { return 7.0; } else { return 3.0; } }", false, 7, false)
}

func TestEmitRejectsMixedWidthFloatArithmeticAndComparison(t *testing.T) {
	for _, source := range []string{
		"fn main() f64 { var a f32 = 1.0; var b f64 = 2.0; return a + b; }",
		"fn main() f64 { var a f32 = 1.0; var b f64 = 2.0; if a < b { return 1.0; } else { return 0.0; } }",
	} {
		if _, _, _, _, err := buildFixtureMaybeFailing(t, source, "main", false); err == nil {
			t.Fatalf("checker accepted mixed-width float expression: %s", source)
		}
	}
}

func TestEmitIntegerToFloatCastCompilesAndRuns(t *testing.T) {
	// An integer local cast to a float and used in float arithmetic: 3 as f64
	// plus 0.5 must yield the real float 3.5, which the hosted main truncates
	// to exit code 3 — had the cast produced an integer (or an implicit
	// truncating coupling), the result would be a different value. The
	// emitted-C assertion pins the lowering: the integer child is built via
	// buildExpr at its own i32 width and wrapped in a plain C (double) cast,
	// exactly the FloatCast-free shape a well-defined int->float conversion
	// needs (no checked runtime primitive).
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x i32 = 3; return (x as f64) + 0.5; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(double)(pebble_local_") {
		t.Fatalf("emitted C missing IntegerToFloat (double) cast:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 3, false)
}

func TestEmitIntegerToFloatOfI64LocalCompilesAndRuns(t *testing.T) {
	// The IntegerToFloat child is resolved at its OWN integer width, not the
	// entry's: an i64 local (different from the f64 main's own width grammar)
	// cast to a float must still build the child via buildExpr at i64 width.
	// 16777217 (2^24+1) is not representable exactly in f32, so the narrowing
	// cast to f32 rounds it to 16777216.0f; adding 1.0 to the widened result
	// and truncating exits 16777217 % 256 == 1.
	emitAndRun(t, "fn main() f64 { var x i64 = 16777217; return ((x as f32) as f64) + 1.0; }", false, 1, false)
}

func TestEmitFloatCastNarrowingCompilesAndRuns(t *testing.T) {
	// An f64 local narrowed to f32. 16777217.5 (2^24 + 1.5) is not
	// representable in f32: round-to-nearest-even gives 16777218.0f, which
	// truncates to exit code 2 (verified empirically against cc). Had the cast
	// been missing (the f64 kept), truncation of 16777217.5 would exit 1 — so
	// the exit code discriminates the narrowing from a no-op. The emitted-C
	// assertion pins the (float) cast on the f64 local.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f32 { var x f64 = 16777217.5; return (x as f32); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(float)(pebble_local_") {
		t.Fatalf("emitted C missing FloatCast narrowing (float) cast:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitFloatCastWideningCompilesAndRuns(t *testing.T) {
	// An f32 local widened to f64. f32->f64 widening is exact, so no exit-code
	// truncation can distinguish the widened value from the unwidened one;
	// the emitted-C assertion pins the (double) cast on the f32 local, and the
	// arithmetic result (2.5 -> exit 2) verifies the widened value flows into
	// f64 arithmetic.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x f32 = 1.5; return (x as f64) + 1.0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(double)(pebble_local_") {
		t.Fatalf("emitted C missing FloatCast widening (double) cast:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitChainedIntegerToFloatAndFloatCastCompilesAndRuns(t *testing.T) {
	// Both new casts in one expression: an integer local cast to f32 (an
	// IntegerToFloat), used in f32 arithmetic, then the result widened to f64
	// by a FloatCast for the final f64 return. 33 as f32 / 1.5 as f32 = 22.0f,
	// widened to 22.0, plus 1.0 -> 23.0, exit 23. The emitted C must contain
	// both the (float) IntegerToFloat cast and the (double) FloatCast widening
	// cast.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() f64 { var x i32 = 33; var f f32 = 1.5; var g f32 = (x as f32) / f; return (g as f64) + 1.0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(float)(pebble_local_") {
		t.Fatalf("emitted C missing chained IntegerToFloat (float) cast:\n%s", out)
	}
	if !strings.Contains(out, "(double)(pebble_local_") {
		t.Fatalf("emitted C missing chained FloatCast (double) cast:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 23, false)
}

func TestEmitFloatToIntegerCompilesAndRuns(t *testing.T) {
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"f32 to i32", "fn main() i32 { let x f32 = 42.75; return x as i32; }", 42},
		{"f64 to i32", "fn main() i32 { let x f64 = 42.75; return x as i32; }", 42},
		{"f32 to i64", "fn main() i64 { let x f32 = 42.75; return x as i64; }", 42},
		{"f64 to i64", "fn main() i64 { let x f64 = 42.75; return x as i64; }", 42},
	} {
		t.Run(tc.name, func(t *testing.T) {
			unit, snapshot, entryID, sources := buildFixture(t, tc.src, "main", false)
			var buf bytes.Buffer
			if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
				t.Fatalf("Emit failed: %v", err)
			}
			out := buf.String()
			if !strings.Contains(out, "pebble_rt_checked_f") || !strings.Contains(out, "_to_i") {
				t.Fatalf("emitted C missing checked float-to-integer helper:\n%s", out)
			}
			compileAndRun(t, buf.Bytes(), tc.want, false)
		})
	}
}

func TestEmitFloatToIntegerBoundaryPanics(t *testing.T) {
	// f32 cannot represent INT32_MAX; 2147483647.0f rounds to 2^31 and must
	// be rejected rather than reaching C's undefined float-to-int conversion.
	emitAndRun(t, "fn main() i32 { let x f32 = 2147483647.0; return x as i32; }", false, 0, true)
	emitAndRun(t, "fn main() i64 { let x f64 = 9223372036854775808.0; return x as i64; }", false, 0, true)
}

func TestEmitFloatToIntegerReleaseReturnsSentinel(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let x f64 = 2147483648.0; return (x as i32) + 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 1, false, false)
}

func TestEmitSelfRecursionCompilesAndRuns(t *testing.T) {
	// Direct recursion: helper calls itself with a decrementing argument and a
	// base case, so the recursion terminates. fact(5) = 120 is the process
	// exit code. Before forward declarations this was a clean rejection
	// (TestEmitRejectsSelfRecursion); now every reachable helper gets a C
	// prototype before any definition, so a self-recursive call is legal C
	// and the backend emits and runs it end-to-end.
	emitAndRun(t, "fn fact(n i32) i32 { if n == 0 { return 1; } else { return n * fact(n - 1); } } fn main() i32 { return fact(5); }", false, 120, false)
}

func TestEmitMutualRecursionCompilesAndRuns(t *testing.T) {
	// Mutual/indirect recursion: a calls b and b calls a — a genuine
	// two-function call cycle. isEven(10) is true, so the exit code is 1.
	// Before forward declarations this was a clean rejection
	// (TestEmitRejectsMutualRecursion); now the prototype pass makes the
	// cycle legal C regardless of definition order, and the base cases
	// terminate the recursion.
	emitAndRun(t, "fn isEven(n i32) i32 { if n == 0 { return 1; } else { return isOdd(n - 1); } } fn isOdd(n i32) i32 { if n == 0 { return 0; } else { return isEven(n - 1); } } fn main() i32 { return isEven(10); }", false, 1, false)
}

func TestEmitThreeHopRecursionCompilesAndRuns(t *testing.T) {
	// The exact shape std/hmap.peb's insert/maybe_grow/rehash cycle has: three
	// helper functions calling each other in a cycle (insert -> maybe_grow ->
	// rehash -> insert) with a base case that terminates the recursion. Each
	// call is a direct recursive edge; the prototype pass must declare all
	// three before any definition so the cycle compiles no matter the
	// definition order. insert(1) counts up by one each hop until n > 10,
	// so the exit code is 11. Bounded execution in case the cycle never
	// terminates.
	emitAndRunBounded(t, "fn insert(n i32) i32 { if n == 0 { return 0; } else { return maybe_grow(n); } } fn maybe_grow(n i32) i32 { if n > 10 { return n; } else { return rehash(n); } } fn rehash(n i32) i32 { return insert(n + 1); } fn main() i32 { return insert(1); }", false, 11, false)
}

func TestEmitRecursionWritesPrototypesBeforeDefinitions(t *testing.T) {
	// The emitted-C shape for the three-hop cycle: every reachable helper must
	// be forward-declared (a static prototype ending in `;`) BEFORE any
	// helper definition (a static function ending in `{`), and each definition
	// must come after its own prototype — that is the mechanism that makes the
	// recursive calls legal C regardless of definition order. In particular,
	// rehash calls insert, whose definition follows rehash's, so insert's
	// prototype must appear before rehash's definition for the emitted C to
	// compile warning-free under -Wall -Wextra -Werror.
	unit, snapshot, entryID, sources := buildFixture(t, "fn insert(n i32) i32 { if n == 0 { return 0; } else { return maybe_grow(n); } } fn maybe_grow(n i32) i32 { if n > 10 { return n; } else { return rehash(n); } } fn rehash(n i32) i32 { return insert(n + 1); } fn main() i32 { return insert(1); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// Every helper has parameters, so the prototype and the definition both
	// begin "static int32_t pebble_fn_<id>(PebbleContext *ctx" and differ only
	// in their terminator (the prototype ends its parameter list with a `;`,
	// the definition with a `{`). Walk the shared prefix from each occurrence
	// to the next `;` or `{` to classify it.
	var firstDefinitionAt, lastPrototypeAt int
	for _, symbolID := range []string{"24", "26", "28"} {
		prefix := "static int32_t pebble_fn_" + symbolID + "(PebbleContext *ctx"
		var prototypeAt, definitionAt int
		for from := 0; ; {
			index := strings.Index(out[from:], prefix)
			if index < 0 {
				break
			}
			absolute := from + index
			rest := out[absolute+len(prefix):]
			if semi := strings.Index(rest, ";"); semi >= 0 && (strings.Index(rest, "{") < 0 || semi < strings.Index(rest, "{")) {
				prototypeAt = absolute
			} else if brace := strings.Index(rest, "{"); brace >= 0 {
				definitionAt = absolute
			}
			from = absolute + len(prefix)
		}
		if prototypeAt == 0 {
			t.Errorf("emitted C missing prototype for pebble_fn_%s:\n%s", symbolID, out)
		}
		if definitionAt == 0 {
			t.Errorf("emitted C missing definition for pebble_fn_%s:\n%s", symbolID, out)
		}
		if prototypeAt >= definitionAt {
			t.Errorf("prototype for pebble_fn_%s does not precede its definition:\n%s", symbolID, out)
		}
		if firstDefinitionAt == 0 || definitionAt < firstDefinitionAt {
			firstDefinitionAt = definitionAt
		}
		if prototypeAt > lastPrototypeAt {
			lastPrototypeAt = prototypeAt
		}
	}
	// Every prototype must come before every definition: the last prototype's
	// `;` must precede the first definition's `{`.
	if firstDefinitionAt == 0 || lastPrototypeAt == 0 {
		t.Fatalf("emitted C has no helper prototypes/definitions:\n%s", out)
	}
	if lastPrototypeAt >= firstDefinitionAt {
		t.Errorf("a prototype does not precede all definitions:\n%s", out)
	}
}

func TestEmitStdHmapInsertGetFullConsumer(t *testing.T) {
	// The real motivating case for the entire std/hmap.peb arc — roughly two
	// dozen fixes across this session's slices: a full std/hmap.peb consumer
	// (new + insert + get on HashMap[int, int]) that exercises every one of
	// the gaps that were found and fixed on the way to a real hashmap:
	// insert -> maybe_grow -> rehash -> insert genuine mutual recursion
	// (forward declarations make the cycle a non-issue), the optional-uint
	// payload (tombstone_index ?uint), the pointer-payload optional
	// (get_by_ref's ?*V), the runtime-builtin (Allocator) struct-field
	// typedef (HashMap's backing Allocator), the uint-typed struct fields
	// (len/cap), the type-parameter-field substitution (Entry[K,V]'s key K /
	// value V), the enum-typed struct field (Entry's state EntryState), the
	// slice-of-struct element (HashMap's `entries []Entry[K, V]`), and —
	// the last blocker, fixed this session — the uint expression grammar:
	// rehash/with_capacity compute `new_cap * (sizeof Entry[K, V])` (a
	// uint-typed CheckedArithmetic over a SizeofType operand), which the
	// general buildExpr path rejected until uint values were routed through
	// buildUintExpr everywhere (local declarations, reassignments, returns,
	// range-loop bounds, slice/array/str indices, optional payloads, struct
	// field reads/writes) and buildUintExpr gained the SizeofType, IntegerCast,
	// Load, and CheckedOptionalUnwrap cases it needed.
	//
	// The consumer inserts seven keys — enough to force maybe_grow to rehash
	// (load factor 7/8 > 0.7) with a doubled capacity, exercising the
	// `new_cap * (sizeof Entry[K, V])` shape with a non-trivial new_cap — and
	// then reads back two keys and returns their sum (10 + 70 = 80), asserting
	// the full pipeline compiles AND RUNS end-to-end, not just that Emit
	// returns no error. Mirrors TestCheckStdHmapU64HashFnTypes' fixture
	// pattern (os.ReadFile of the real module sources, fixtureProvider,
	// StandardRoot: "std").
	hmap, err := os.ReadFile("../../std/hmap.peb")
	if err != nil {
		t.Fatal(err)
	}
	mem, err := os.ReadFile("../../std/mem.peb")
	if err != nil {
		t.Fatal(err)
	}
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb":     []byte(`import "std:hmap"; fn userHash(x int) u64 => x as u64; fn userEq(a int, b int) bool => a == b; fn main() int { var m = hmap::new[int, int](userHash, userEq); m.insert(1, 10); m.insert(2, 20); m.insert(3, 30); m.insert(4, 40); m.insert(5, 50); m.insert(6, 60); m.insert(7, 70); let v1 = m.get(1)!; let v7 = m.get(7)!; return v1 + v7; }`),
		"std/hmap.peb": hmap,
		"std/mem.peb":  mem,
	}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed on std:hmap consumer: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("check succeeded without an IR unit")
	}
	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entryID = candidate.ID
		}
	}
	if entryID == 0 {
		t.Fatal("missing symbol \"main\"")
	}
	var buf bytes.Buffer
	err = Emit(unit, unit.Snapshot(), entryID, sources, nil, &buf)
	if err != nil {
		t.Fatalf("Emit failed on the full std:hmap consumer: %v", err)
	}
	// The capstone of the session's std/hmap.peb arc: the full consumer must
	// not only emit — it must compile clean under -Wall -Wextra -Werror and
	// RUN, returning the inserted value sum (m.get(1)! + m.get(7)! = 80).
	compileAndRun(t, buf.Bytes(), 80, false)
}

func TestEmitRejectsEntryReachedByHelperCycle(t *testing.T) {
	// The one cycle shape still rejected: a helper calling the entry function
	// (main) back closes a cycle through the entry, which is emitted under the
	// fixed C name pebble_user_main — not as a pebble_fn_<symbolID> helper the
	// forward-declaration pass covers — so the backend cannot lower a call to
	// it and rejects the cycle cleanly rather than emit a call to an
	// undeclared C identifier.
	unit, snapshot, entryID, _ := buildFixture(t, "fn helper() i32 { return main(); } fn main() i32 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "recursive call through the entry function is not supported")
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

func TestEmitArrayHelperParameterAndResultCompilesAndRuns(t *testing.T) {
	output := emitAndRunCaptureBounded(t, `fn sort_once(values [5]int) [5]int {
    if values[0] > values[1] {
        let first = values[0];
        values[0] = values[1];
        values[1] = first;
    }
    return values;
}
fn main() int {
    var values [5]int = [2, 1, 3, 4, 5];
    let sorted = sort_once(values);
    print sorted[0];
    print sorted[1];
    return 0;
}`, false, 0, false)
	if output != "1\n2\n" {
		t.Fatalf("array helper output = %q, want %q", output, "1\n2\n")
	}
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitNonVoidDiscardedCallStatementCompilesAndRuns(t *testing.T) {
	// A call to a non-void-returning function used purely as a statement
	// (`f();` where f returns i32, the result silently discarded) is reachable
	// from real source — the checker's C0612 rejects only a discarded
	// expression statement whose value is a non-void NON-call expression, and
	// deliberately permits a discarded call. The backend emits it exactly like
	// a void call (a bare `<call>;` statement), and C discards the returned
	// value with no warning even under -Wall -Wextra -Werror. The helper's
	// side effect (its print of the argument) still runs, its 999 result is
	// safely ignored, and the exit code reflects only the caller's own
	// subsequent logic (here return 1).
	output := emitAndRunCapture(t, "fn f(x i32) i32 { print x; return 999; } fn main() i32 { f(42); return 1; }", false, 1, false)
	if output != "42\n" {
		t.Fatalf("captured output = %q, want %q", output, "42\n")
	}
}

func TestEmitUnreachableFunctionNotEmitted(t *testing.T) {
	// A declared function the entry never calls, directly or transitively, must
	// not be emitted at all — the generated C has no trace of it (symbol 25,
	// the unused function), so the -Wall -Wextra -Werror build cannot warn
	// about an unused static function. Only the reachable helper (symbol 24)
	// is emitted, and the program runs to exit 21.
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper() i32 { return 21; } fn unused() i32 { return 99; } fn main() i32 { return helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitArrayHelperLiteralArgumentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn f(a [3]i32) i32 { return 1; } fn main() i32 { return f([10, 20, 30]); }", false, 1, false)
}

func TestEmitRejectsParameterWidthMismatch(t *testing.T) {
	// A parameter of the other integer width follows the same width-consistency
	// rule 10.13 established for locals: an i64 parameter in an i32 entry (and
	// its result, here also i64) must be a clean rejection naming the width,
	// never a coercion. The parameter check fires before the result check.
	unit, snapshot, entryID, _ := buildFixture(t, "fn f(a i64) i64 { return 0; } fn main() i32 { return f(0); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "has type i64, want i32, bool, char, str, f32, f64")
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitOptionalLocalStoreCompilesAndRuns(t *testing.T) {
	// Reassigning an optional-typed local (x = some 5) is now supported —
	// the std/hmap.peb insert shape (`tombstone_index = some index;`) needed
	// it, and buildStoreCore's optional-local case routes the new value
	// through buildOptionalValue, the same machinery an optional local's own
	// declaration initializer uses, emitting a plain whole-struct C
	// assignment. This was previously a clean rejection
	// ("reassigning an optional is not supported yet"); it is now a real
	// compile-and-run case: x is reassigned from some 1 to some 2, and the
	// force-unwrap returns 2.
	emitAndRun(t, "fn main() i32 { var x ?i32 = some 1; x = some 2; return x!; }", false, 2, false)
}

func TestEmitRejectsOptionalWithUnsupportedPayloadType(t *testing.T) {
	// An optional whose payload type is neither the entry's width nor bool —
	// here a str payload — is reachable from real source (the checker builds
	// the declaration fine), so this is a genuine backend-scope rejection. The
	// optional typedef pass inspects the payload type first and rejects the
	// str field with a clear error naming the wanted types, so no C is written.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { let x ?str = some \"hi\"; return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want a fixed-width integer, bool, tuple, struct, or enum")
}

func TestEmitOptionalUintSomeUnwrapCompilesAndRuns(t *testing.T) {
	// The std/hmap.peb-motivating payload width: a uint-payload optional
	// constructed with some and force-unwrapped. The typedef's .value field is
	// uint64_t (uint resolves to its OWN C width via the generic
	// resolvedBuiltin/cType pattern), the payload is built through uint's
	// dedicated buildUintExpr grammar, and the force-unwrap routes to the new
	// pebble_rt_checked_unwrap_u64 runtime helper (uint's uint64_t needs a
	// uint64_t-width unwrap, not the entry-width i32 helper). 5 as int exits
	// 5.
	emitAndRun(t, "fn main() i32 { var o ?uint = some 5; return o! as i32; }", false, 5, false)
}

func TestEmitOptionalUintNoneHasValueCompilesAndRuns(t *testing.T) {
	// The none side of a uint-payload optional: zeroOptionalPayloadLiteral
	// must pick a warning-clean zero literal for the uint64_t .value field (a
	// bare 0, scalar), the local initializes with has_value = false, and the
	// has_value read drives an if to the else arm. The process exit code 0
	// proves the none path — not the some path — was taken.
	emitAndRun(t, "fn main() i32 { var o ?uint = none; if o.has_value { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitOptionalUintSomeHasValueCompilesAndRuns(t *testing.T) {
	// The same has_value check with a some-initialized uint optional, so the
	// true arm is taken: proves the has_value tag is set by the some
	// construction and read back correctly for a uint payload (exit 1).
	emitAndRun(t, "fn main() i32 { var o ?uint = some 9; if o.has_value { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitOptionalU64SomeUnwrapCompilesAndRuns(t *testing.T) {
	// u64 shares the uint64_t C representation (and thus the same generic
	// resolvedBuiltin/cType typedef and the same pebble_rt_checked_unwrap_u64
	// unwrap helper) as uint, but is a DISTINCT builtin that flows through the
	// general buildExpr path at its own width rather than buildUintExpr. Exit
	// 22.
	emitAndRun(t, "fn main() i32 { var o ?u64 = some 22; return o! as i32; }", false, 22, false)
}

func TestEmitOptionalU64NoneHasValueCompilesAndRuns(t *testing.T) {
	// The none side of a u64-payload optional: the .value zero literal (0) and
	// the has_value=false tag both hold for a u64 payload.
	emitAndRun(t, "fn main() i32 { var o ?u64 = none; if o.has_value { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitOptionalUintTypedefWritesUint64T(t *testing.T) {
	// The emitted-C shape check for the uint payload: the optional typedef
	// declares the .value field as uint64_t (the C type uint resolves to),
	// never int32_t or a rejection, and the some construction assigns the
	// "u"-suffixed literal into it.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var o ?uint = some 5; return o! as i32; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "typedef struct {\n    bool has_value;\n    uint64_t value;\n} pebble_optional_") {
		t.Errorf("emitted C does not declare the uint payload's .value field as uint64_t:\n%s", out)
	}
	if strings.Contains(out, "int32_t value;") {
		t.Errorf("emitted C declared an i32 .value field for a uint payload:\n%s", out)
	}
	if !strings.Contains(out, ".has_value = true, .value = 5u") {
		t.Errorf("emitted C is missing the uint-payload some construction (.value = 5u):\n%s", out)
	}
	if !strings.Contains(out, "pebble_rt_checked_unwrap_u64(") {
		t.Errorf("emitted C is missing the u64-width force-unwrap helper call:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 5, false)
}

func TestEmitOptionalUintHmapInsertShapeCompilesAndRuns(t *testing.T) {
	// The real motivating fixture, mirroring std/hmap.peb's actual insert
	// shape without touching the std module: a none-initialized ?uint local
	// (tombstone_index), a has_value check, and a force-unwrap into a uint
	// binding inside the true arm. Since tombstone_index is none, the else
	// arm runs and the exit code is 3; the false-arm only is exercised
	// because that is what hmap's first insert does (tombstone_index starts
	// none).
	emitAndRun(t, "fn main() i32 { var tombstone_index ?uint = none; var result i32 = 0; if tombstone_index.has_value { let t = tombstone_index!; result = t as i32; } else { result = 3; } return result; }", false, 3, false)
}

func TestEmitOptionalUintHmapInsertShapeSomeCompilesAndRuns(t *testing.T) {
	// The same insert-shaped fixture with tombstone_index some-initialized, so
	// the has_value true arm runs: the force-unwrap binds t as a uint local
	// and its value (7) is cast to int and returned — the exact statement
	// sequence hmap.insert's tombstone path performs (`let t =
	// tombstone_index!; ... slot`). Exit 7.
	emitAndRun(t, "fn main() i32 { var tombstone_index ?uint = some 7; var result i32 = 0; if tombstone_index.has_value { let t = tombstone_index!; result = t as i32; } else { result = 3; } return result; }", false, 7, false)
}

func TestEmitRejectsOptionalUnwrapOfU8Payload(t *testing.T) {
	// The narrower fixed-width integers pass the typedef and some-value gates
	// (the generic resolvedBuiltin/cType mechanism), but their force-unwrap is
	// a clean rejection: there is no pebble_rt_checked_unwrap_i8/u8-style
	// runtime helper family beyond i32/i64/u64/bool yet. This pins that
	// residual precisely rather than emitting a call to a nonexistent helper.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var o ?u8 = some 5; return o! as i32; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "has no runtime unwrap helper")
}

func TestEmitOptionalPointerSomeHasValueCompilesAndRuns(t *testing.T) {
	// The exact minimal repro for pointer-payload optionals: a helper returns
	// `?*int` built from `some &y` / `none`, consumed in the entry via a
	// direct-call initializer and a `.has_value` read. The optional's .value
	// field is the pointee's pointer C type (int32_t *), the AddressOf child
	// flows through buildExpr's pointer path, and the has_value tag round-trips
	// through the call-returned optional struct. Only the tag is observed (the
	// pointed-to local is dead after the helper returns, so dereferencing
	// would be garbage — a language-semantics property, not a backend bug), so
	// exit 1 proves the some arm ran.
	emitAndRun(t, "fn find(x int) ?*int { var y int = x; if x > 0 { return some &y; } return none; } fn main() int { var v int = 5; let r = find(v); if r.has_value { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitOptionalPointerForceUnwrapCompilesAndRuns(t *testing.T) {
	// The force-unwrap path for a pointer payload: `find(&v)!` unwraps an
	// optional-pointer call result (hoisted into a pebble_temp_<id> optional
	// local so the call runs exactly once) through the new
	// pebble_rt_checked_unwrap_ptr runtime helper, and the resulting pointer is
	// dereferenced. The pointer names a live local in the entry's frame, so
	// `*p` reads 5, not garbage (unlike the dangling &y shape).
	emitAndRun(t, "fn find(p *int) ?*int { return some p; } fn main() int { var v int = 5; let p = find(&v)!; return *p; }", false, 5, false)
}

func TestEmitOptionalPointerNoneHasValueCompilesAndRuns(t *testing.T) {
	// The none side of a pointer-payload optional: get_by_ref-style `return
	// none;` produces a { .has_value = false, .value = 0 } optional (a null
	// pointer constant 0 is a warning-clean initializer for the int32_t *
	// .value field), and the has_value read drives an if to the else arm.
	// Exit 0 proves the none path — not the some path — was taken.
	emitAndRun(t, "fn find(x int) ?*int { var y int = x; if x > 0 { return some &y; } return none; } fn main() int { var v int = -1; let r = find(v); if r.has_value { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitOptionalPointerNoneLocalDeclCompilesAndRuns(t *testing.T) {
	// zeroOptionalPayloadLiteral's pointer-payload shape, in the one position
	// that exercises it as a local declaration's initializer: `var o ?*int =
	// none;`. The .value field's zero literal must be warning-clean against
	// the int32_t * field type (a bare 0, the null pointer constant — no
	// -Wmissing-braces shape needed for a scalar pointer). Exit 0 proves the
	// none tag.
	emitAndRun(t, "fn main() int { var o ?*int = none; if o.has_value { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitOptionalPointerSomeLocalDeclAndDerefCompilesAndRuns(t *testing.T) {
	// A some-initialized pointer-payload optional local (`var o ?*int = some
	// &y;`), unwrapped and dereferenced: `*(o!)` exercises the pointer-typed
	// CheckedOptionalUnwrap (via pebble_rt_checked_unwrap_ptr) feeding the
	// null-checked dereference, including the SourceAlias (grouped-expression
	// parens) the deref path introduces. y is alive in the entry frame, so the
	// deref reads 7. Exit 7.
	emitAndRun(t, "fn main() int { var y int = 7; var o ?*int = some &y; if !o.has_value { return 99; } return *(o!); }", false, 7, false)
}

func TestEmitOptionalPointerNoneForceUnwrapPanics(t *testing.T) {
	// A force-unwrap of an absent pointer-payload optional must panic with
	// PEBBLE_PANIC_UNWRAP_FAILED, exactly like every other payload width: the
	// pebble_rt_checked_unwrap_ptr helper has no null special-case — a null
	// payload VALUE is a valid unwrap result; only has_value=false faults.
	// find(-1) returns none, so the unwrap panics and the process terminates
	// abnormally.
	emitAndRun(t, "fn find(x int) ?*int { var y int = x; if x > 0 { return some &y; } return none; } fn main() int { var v int = -1; let p = find(v)!; return 1; }", false, 0, true)
}

func TestEmitOptionalPointerTypedefWritesPointeePointerCType(t *testing.T) {
	// The emitted-C shape check for the pointer payload: the optional typedef
	// declares the .value field as the pointee's pointer C type (int32_t * for
	// ?*int, via pointerTypeName), never a rejection or a scalar, and the some
	// construction assigns the AddressOf expression into it; the force-unwrap
	// routes to the new pebble_rt_checked_unwrap_ptr helper.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var y int = 7; var o ?*int = some &y; if !o.has_value { return 99; } return *(o!); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "typedef struct {\n    bool has_value;\n    int32_t * value;\n} pebble_optional_") {
		t.Errorf("emitted C does not declare the pointer payload's .value field as int32_t *:\n%s", out)
	}
	if strings.Contains(out, "int32_t value;") {
		t.Errorf("emitted C declared a scalar .value field for a pointer payload:\n%s", out)
	}
	if !strings.Contains(out, ".has_value = true, .value = (int32_t *)(&pebble_local_") {
		t.Errorf("emitted C is missing the pointer-payload some construction (.value = AddressOf):\n%s", out)
	}
	if !strings.Contains(out, "pebble_rt_checked_unwrap_ptr(") {
		t.Errorf("emitted C is missing the pointer-width force-unwrap helper call:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 7, false)
}

func TestEmitOptionalPointerHmapGetByRefGetShapeCompilesAndRuns(t *testing.T) {
	// The real motivating fixture, mirroring std/hmap.peb's actual
	// get_by_ref/get shape WITHOUT touching the std module: get_by_ref returns
	// `?*int` built from `some &entry.value` (the address of a struct field
	// through a struct-pointer parameter) / `none`, and get consumes it via
	// `let ptr = get_by_ref(...)`, a `!ptr.has_value` guard, and `return some
	// *(ptr!);` — the exact statement sequence hmap.get performs. The exit
	// code 5 requires the whole chain to round-trip: get_by_ref's optional
	// pointer → get's has_value check → force-unwrap → dereference → some int.
	emitAndRun(t, "type Entry = struct { value int; }; fn get_by_ref(entry *Entry, key int) ?*int { if key < 0 { return none; } return some &entry.value; } fn get(entry *Entry, key int) ?int { let ptr = get_by_ref(entry, key); if !ptr.has_value { return none; } return some *(ptr!); } fn main() int { var e Entry = Entry.{ value = 5 }; let r = get(&e, 1); if r.has_value { return r!; } else { return 0; } }", false, 5, false)
}

func TestEmitOptionalPointerHmapGetByRefGetShapeNoneCompilesAndRuns(t *testing.T) {
	// The none side of the same hmap-shaped fixture: get_by_ref returns none
	// for a negative key, get sees !ptr.has_value and returns `none` (an
	// int-payload optional), and main's has_value check falls to the else arm.
	// Exit 0 proves both none paths (the ?*int and the ?int) round-trip.
	emitAndRun(t, "type Entry = struct { value int; }; fn get_by_ref(entry *Entry, key int) ?*int { if key < 0 { return none; } return some &entry.value; } fn get(entry *Entry, key int) ?int { let ptr = get_by_ref(entry, key); if !ptr.has_value { return none; } return some *(ptr!); } fn main() int { var e Entry = Entry.{ value = 5 }; let r = get(&e, -1); if r.has_value { return 99; } else { return 0; } }", false, 0, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	// A struct whose field type is neither a fixed-width integer nor a
	// supported compound type — here a str field — is reachable from real
	// source (the checker builds the declaration and construction fine), so
	// this is a genuine backend-scope rejection. The struct typedef pass
	// inspects each field's resolved type first and rejects the str field with
	// a clear error naming the wanted types, so no C is written.
	unit, snapshot, entryID, _ := buildFixture(t, "type S = struct { s str; };\nfn main() i32 { let x S = S.{ s = \"hi\" }; return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "field type str is not supported, want a fixed-width integer, bool, tuple, struct, enum, pointer, slice, function type, or runtime type")
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "return pebble_fn_24(ctx, (pebble_tuple_23_t){ 20, 22 });") {
		t.Errorf("emitted C missing the tuple compound-literal argument:\n%s", out)
	}

	unit, snapshot, entryID, sources = buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return f(Point.{ y = 22, x = 20 }); }", "main", false)
	buf.Reset()
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	infos, err := collectStructTypes(unit, snapshot, entryBlock, helpers, nil)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitStructReturningHelperForwardsCallCompilesAndRuns(t *testing.T) {
	// A struct-returning helper whose tail return is a DirectCall to another
	// struct-returning helper (`return helper();` — the return-forwarding
	// shape io.peb's `return string::new();` uses), with no intermediate
	// local: makeP's return value is built from the call itself rather than
	// from a SymbolValue or RecordConstruct. The entry assigns makeP's call
	// to a matching local and reads both fields back, so a real field of the
	// returned struct must come through end-to-end. 20 + 22 = 42 is the
	// process exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn helper() Point { return Point.{ x = 20, y = 22 }; } fn makeP() Point { return helper(); } fn main() i32 { let p Point = makeP(); return p.x + p.y; }", false, 42, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitRejectsEntryReturningTuple(t *testing.T) {
	// The entry itself cannot declare a tuple/struct result type: its C return
	// type stays the scalar entryReturnType (integer, or a float since Float
	// Stage A) regardless of what the language
	// lets a helper write. validateEntrySignature rejects the tuple result
	// exactly as it always has, with the accepted-result list since Float
	// Stage A extended by f32 and f64.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() (i32, i32) { return (1, 2); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "entry function result type is (i32, i32), want void, int, i32, i64, f32, or f64")
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitTopLevelGuardIfCompilesAndRuns(t *testing.T) {
	// The minimal non-tail-if repro: a guard-clause if with no else, as a
	// leading statement in a top-level function body, followed by more code.
	// x = 5 takes the guard (x + 1 = 6); x = 0 falls through the guard to
	// the code after it (x + 10 = 10). Both calls run, so both the guard arm
	// and the fall-through code after the if are exercised.
	emitAndRun(t, "fn helper(x i32) i32 { if x > 0 { return x + 1; } return x + 10; } fn main() i32 { return helper(5) + helper(0); }", false, 16, false)
}

func TestEmitTopLevelGuardIfWritesC(t *testing.T) {
	// The emitted C for the minimal repro must be a plain no-else if whose
	// arm ends in return, followed by the enclosing return — the shape
	// buildLeadingIf produces, byte-identical in style to buildLoopIf's.
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper(x i32) i32 { if x > 0 { return 1; } return 0; } fn main() i32 { return helper(1); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    if (pebble_local_25 > 0) {",
		"        return 1;",
		"    }",
		"    return 0;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "} else {") {
		t.Errorf("emitted C contains an else for a no-else if:\n%s", out)
	}
}

func TestEmitTopLevelIfElseLeadingCompilesAndRuns(t *testing.T) {
	// A top-level if/else (both arms present) as a leading statement,
	// followed by more code that runs after either arm: pick(1) takes the
	// then-arm (result = 10, then 11) and pick(0) takes the else-arm
	// (result = 20, then 20), so both arms plus the fall-through code after
	// the if are exercised.
	emitAndRun(t, "fn pick(x i32) i32 { var result i32 = 0; if x > 0 { result = 10; } else { result = 20; } result = result + x; return result; } fn main() i32 { return pick(1) + pick(0); }", false, 31, false)
}

func TestEmitTopLevelSwitchLeadingCompilesAndRuns(t *testing.T) {
	// A switch as an ordinary top-level leading statement, followed by more
	// code: classify(1) hits the single-value case (11), classify(2) hits the
	// multi-value case (21), and classify(9) hits the else arm (31). Every
	// case body falls through to the statements after the switch, and none
	// ends in a return — the shape buildSwitch was previously unable to emit
	// in a non-tail position.
	emitAndRun(t, "fn classify(x i32) i32 { var result i32 = 0; switch x { case 1: result = result + 10; case 2, 3: result = result + 20; else: result = result + 30; } result = result + 1; return result; } fn main() i32 { return classify(1) + classify(2) + classify(9); }", false, 63, false)
}

func TestEmitTopLevelSwitchLeadingWithReturningCaseCompilesAndRuns(t *testing.T) {
	// A top-level switch whose case bodies may return OR fall through:
	// f(1) returns 99 from inside a case body, f(2) falls through case 2 to
	// the code after the switch (1 + 10 = 11), and f(9) falls through the
	// else arm (2 + 10 = 12). This confirms a case body's "may fall through"
	// grammar includes the return case, matching what buildSwitchCaseBody's
	// bare-Return path did for the tail position.
	emitAndRun(t, "fn f(x i32) i32 { var total i32 = 0; switch x { case 1: return 99; case 2: total = total + 1; else: total = total + 2; } return total + 10; } fn main() i32 { return f(1) + f(2) + f(9); }", false, 122, false)
}

func TestEmitSwitchInsideLoopBodyCompilesAndRuns(t *testing.T) {
	// The switch-in-loop repro: a switch as a statement inside a while loop
	// body, followed by more loop-body code. i counts 0, 1, 2; each switch
	// adds 1/2/3 by case, then the code after the switch adds 1, so total =
	// (1+1) + (2+1) + (3+1) = 9, returned as the exit code. This is the
	// position where If already worked but Switch did not. Bounded execution
	// in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { switch i { case 0: total = total + 1; case 1: total = total + 2; else: total = total + 3; } total = total + 1; i = i + 1; } return total; }", false, 9, false)
}

func TestEmitIfInsideSwitchCaseBodyCompilesAndRuns(t *testing.T) {
	// Nested control flow inside a fall-through switch case body: the case 2
	// body is itself an if/else whose arms reassign the enclosing local. x=2
	// hits the case 2 body's then-arm (total = 5), then the code after the
	// switch adds 1, so total = 6.
	emitAndRun(t, "fn main() i32 { var x i32 = 2; var total i32 = 0; switch x { case 1: total = total + 1; case 2: if x == 2 { total = total + 5; } else { total = total + 6; } else: total = total + 9; } total = total + 1; return total; }", false, 6, false)
}

func TestEmitSwitchInsideIfArmCompilesAndRuns(t *testing.T) {
	// Nested control flow inside a top-level if arm: the then-arm is itself
	// a switch whose case bodies fall through, and the code after the if runs
	// after either arm. x=1 takes the then-arm's case 1 (total = 10), then
	// the fall-through code adds 1, so total = 11.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; var total i32 = 0; if x > 0 { switch x { case 1: total = total + 10; else: total = total + 20; } } else { total = total + 30; } total = total + 1; return total; }", false, 11, false)
}

func TestEmitIfAndSwitchInsideTopLevelIfArmCompilesAndRuns(t *testing.T) {
	// Both a nested if and a nested switch inside a single top-level if arm,
	// plus more code after the outer if: x=1 takes the then-arm, whose own if
	// adds 1 and whose switch adds 10, then the outer fall-through code adds
	// 1, so total = 12.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; var y i32 = 2; var total i32 = 0; if x > 0 { if y > 0 { total = total + 1; } else { total = total + 2; } switch y { case 2: total = total + 10; else: total = total + 20; } } total = total + 1; return total; }", false, 12, false)
}

func TestEmitBreakAndContinueInsideLoopIfArmCompilesAndRuns(t *testing.T) {
	// Regression: break and continue inside an if arm inside a while loop
	// body still work after the loop-body dispatch was reorganized into the
	// shared fall-through builder. i counts 1..9; even i continues past the
	// total accumulation, and i=5 breaks the loop. Only odd i < 5 accumulate
	// (1 and 3), so total = 4.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 10 { i = i + 1; if i % 2 == 0 { continue; } if i == 5 { break; } total = total + i; } return total; }", false, 4, false)
}

func TestEmitBreakInsideSwitchCaseBodyTargetsSwitchCompilesAndRuns(t *testing.T) {
	// A break inside a switch case body targets the switch — Pebble's break
	// resolves to the nearest enclosing loop-or-switch, so the emitted C
	// break (which C also resolves to the nearest switch/loop) is the direct,
	// correct translation. The switch sits inside a loop, so if the break
	// wrongly leaked to the loop the accumulation after the switch would stop
	// early. i=0 falls through the else (+1, then after-switch +1 = 2), i=1
	// hits case 1's break (switch only, then after-switch +1 = 3), i=2 hits
	// case 2 (+10, then after-switch +1 = 14), so total = 14.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var total i32 = 0; while i < 3 { switch i { case 1: break; case 2: total = total + 10; else: total = total + 1; } total = total + 1; i = i + 1; } return total; }", false, 14, false)
}

func TestEmitEnumSwitchLeadingCompilesAndRuns(t *testing.T) {
	// An enum-typed subject in a fall-through (leading-position) switch: the
	// subject-building and CaseValue label logic shared with the tail-position
	// buildSwitch is reused unchanged by buildLoopSwitch. c = green hits the
	// CaseValue-based case (total = 10), then the fall-through code adds 1,
	// so total = 11.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar total i32 = 0;\nvar c Color = Color.green;\nswitch c { case Color.red: total = total + 1; case Color.green: total = total + 10; case Color.blue: total = total + 100; }\ntotal = total + 1;\nreturn total;\n}", false, 11, false)
}

func TestEmitReturnInsideLoopIfArmCompilesAndRuns(t *testing.T) {
	// A return inside an if arm inside a loop body exits the function early —
	// reachable now that the enclosing function's result grammar threads
	// through the fall-through builder into loop bodies. f(3) returns 3 from
	// inside the loop; f(20) never matches and returns 99 after the loop, so
	// the exit code is 3 + 99 = 102.
	emitAndRunBounded(t, "fn f(x i32) i32 { var i i32 = 0; while i < 10 { if i == x { return i; } i = i + 1; } return 99; } fn main() i32 { return f(3) + f(20); }", false, 102, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitEnumSwitchBaselessCaseLabelsCompileAndRun(t *testing.T) {
	// The base-less .name shorthand for switch case labels (the exact repro:
	// `case .red:` against a Color-typed subject). Each variant's subject must
	// fire its own case branch and exit with the matching code, proving the
	// shorthand resolves to the same member as the qualified `case Color.red:`
	// form rather than merely checking clean. No trailing return after the
	// switch: the shorthand must be recognized as exhaustive by
	// switchIsExhaustive (control_validation.go) exactly like a qualified
	// label already is, not merely resolve without also covering
	// exhaustiveness — a masked version of this test with a spurious trailing
	// return previously hid a real gap there (base-less case values produce
	// an aggregateEnumVariant record, not a memberVariant member record, so
	// switchIsExhaustive's coverage set never saw them and every base-less
	// switch spuriously required an unreachable trailing return).
	for _, fixture := range []struct {
		name  string
		value string
		want  int
	}{
		{"red", "Color.red", 1},
		{"green", "Color.green", 2},
		{"blue", "Color.blue", 3},
	} {
		t.Run(fixture.name, func(t *testing.T) {
			emitAndRun(t, "type Color = enum { red, green, blue }; fn main() int {\nvar c Color = "+fixture.value+";\nswitch c { case .red: return 1; case .green: return 2; case .blue: return 3; }\n}", false, fixture.want, false)
		})
	}
}

func TestEmitTaggedUnionSwitchBaselessCaseLabelsCompileAndRun(t *testing.T) {
	// Same base-less .name shorthand as
	// TestEmitEnumSwitchBaselessCaseLabelsCompileAndRun, against a tagged
	// union (union enum) subject instead of a plain enum — confirms the
	// exhaustiveness fix (switchIsExhaustive now also indexes
	// aggregateTaggedVariant records) covers both nominal-declaration kinds,
	// not just plain enums. No trailing return: the switch must be
	// recognized as exhaustive.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case .empty: return 0; case .value: return 1; }\n}", false, 1, false)
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

func TestEmitEnumShorthandComparisonAndAssignmentCompilesAndRuns(t *testing.T) {
	// Enum-variant shorthand literals (.Empty/.Occupied) in a var initializer,
	// an equality comparison, and an assignment. The loop body is the one
	// statement position where the backend already supports mid-body conditions
	// (the entry/helper body grammar allows only leading declarations, stores,
	// and calls followed by a tail return/if/switch), so the one-iteration
	// loop hosts the branching. The local starts .Empty: the first comparison
	// correctly does NOT fire, the local is reassigned .Occupied, and the
	// second comparison fires, accumulating n = 42. Every .Empty is a
	// deferred-receiver aggregate resolved from the solved enum type, and the
	// emitted C compares and assigns the enum's discriminant constants, so the
	// runtime branch behavior proves the shorthand lowered correctly.
	emitAndRunBounded(t, "type State = enum { Empty, Occupied };\nfn main() i32 {\nvar n i32 = 0;\nvar done i32 = 0;\nwhile done == 0 {\nvar s State = .Empty;\nif s == .Occupied { n = n + 1; }\ns = .Occupied;\nif s == .Occupied { n = n + 42; }\ndone = 1;\n}\nreturn n;\n}", false, 42, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitEnumToIntegerCompilesAndRuns(t *testing.T) {
	// The exact minimal repro from the brief: `Color.green as i32` in an i32
	// entry. The checker lowers the cast to a tir.EnumToInteger whose single
	// child is the EnumVariantValue Color.green, and the backend lowers it to a
	// plain C cast (int32_t)(pebble_variant_<green>) — green's ordinal in
	// declared order is 1, so the exit code is 1.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 { return Color.green as i32; }", false, 1, false)
}

func TestEmitEnumToIntegerZeroOrdinalCompilesAndRuns(t *testing.T) {
	// The first-declared variant (red) has C ordinal 0, so casting it to an
	// integer yields 0 — proving the cast reads the actual declared-order
	// discriminant rather than always producing a nonzero value.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 { return Color.red as i32; }", false, 0, false)
}

func TestEmitEnumToIntegerI64CompilesAndRuns(t *testing.T) {
	// A different destination integer width than the entry's result type: the
	// cast's destination (i64) is the entry's own width here, so the
	// EnumToInteger node's Type matches the surrounding width gate exactly as an
	// IntegerCast's does. green's ordinal 1 is returned as an i64.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i64 { return Color.green as i64; }", false, 1, false)
}

func TestEmitEnumToIntegerUnsignedNestedCompilesAndRuns(t *testing.T) {
	// An unsigned destination (u32) inside an i32 entry: a cast whose
	// destination is not the entry's width is only valid where the surrounding
	// context is that width, so it appears as a u32 local's initializer, then
	// the local is read back out as i32. The destination width is resolved from
	// the EnumToInteger node's own Type (u32 -> uint32_t), not from the entry.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 { let v u32 = Color.green as u32; return v as i32; }", false, 1, false)
}

func TestEmitEnumToIntegerFromLocalCompilesAndRuns(t *testing.T) {
	// An enum value read from a local (not just a variant literal) cast to an
	// integer: the EnumToInteger's child is a SymbolValue naming the enum-typed
	// local, built by buildEnumValue as pebble_local_<sym> and cast directly.
	// c = blue, whose ordinal is 2.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 { var c Color = Color.blue; return c as i32; }", false, 2, false)
}

func TestEmitEnumToIntegerFromLocalI64CompilesAndRuns(t *testing.T) {
	// The from-a-local form at a different width (i64): the local's value is
	// read and cast to the destination width, again via the same
	// buildEnumValue + plain C cast lowering.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i64 { var c Color = Color.blue; return c as i64; }", false, 2, false)
}

func TestEmitEnumToIntegerWritesC(t *testing.T) {
	// Confirm the emitted C directly: the EnumToInteger lowers to a plain C
	// cast of the enum constant to the destination C type, (int32_t)(...), with
	// no runtime helper and no intermediate enum-typedef step.
	unit, snapshot, entryID, enumType, variants, sources := enumFixture(t, "type Color = enum { red, green, blue }; fn main() i32 { return Color.green as i32; }")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	cast := "(int32_t)(pebble_variant_" + strconv.Itoa(int(variants[1])) + ")"
	if !strings.Contains(out, cast) {
		t.Errorf("emitted C missing the enum-to-integer cast %q:\n%s", cast, out)
	}
	for _, want := range []string{
		"typedef enum {",
		"} " + enumTypeName(enumType) + ";",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitCharToIntegerCompilesAndRuns(t *testing.T) {
	// The exact minimal repro from the brief: `let c char = 'A'; let n u32 =
	// c as u32; return n as i32;` — a char read out as its codepoint. The
	// checker lowers the cast to a tir.CharToInteger whose single child is the
	// char-typed local c, and the backend lowers it to a plain C cast
	// (uint32_t)(pebble_local_<c>) — 'A' is codepoint 65, so the exit code is
	// 65. The u32 destination is not the entry's i32 width, so the cast appears
	// as the u32 local's initializer, then the local is read back out as i32.
	emitAndRun(t, "fn main() i32 { let c char = 'A'; let n u32 = c as u32; return n as i32; }", false, 65, false)
}

func TestEmitCharToIntegerFromLiteralCompilesAndRuns(t *testing.T) {
	// The CharToInteger child can be a bare char literal (not just a local
	// reference): 'A' as u32 is built by buildCharOperand's CharLiteral path
	// and cast to uint32_t, then read back out as i32. Codepoint 65.
	emitAndRun(t, "fn main() i32 { let n u32 = 'A' as u32; return n as i32; }", false, 65, false)
}

func TestEmitCharToIntegerHashCharCompilesAndRuns(t *testing.T) {
	// The real motivating case from std/hash.peb:85-87 — `fn hash_char(val
	// char) u64 { return hash_u64(val as u64); }` — as a standalone fixture
	// (std/hash.peb itself is untouched). The `val as u64` is a CharToInteger
	// whose destination (u64) is the argument width of hash_u64, so the width
	// gate passes exactly as the hash module needs. hash_char('A') reads out
	// codepoint 65 and returns it as a u64, which main narrows to i32.
	emitAndRun(t, "fn hash_u64(val u64) u64 { return val; }\nfn hash_char(val char) u64 { return hash_u64(val as u64); }\nfn main() i32 { return hash_char('A') as i32; }", false, 65, false)
}

func TestEmitCharToIntegerU64CompilesAndRuns(t *testing.T) {
	// The same char-to-integer cast at the other motivating width (u64),
	// matching hash_char's destination directly in a u64 entry.
	emitAndRun(t, "fn main() i64 { let c char = 'A'; let n u64 = c as u64; return n as i64; }", false, 65, false)
}

func TestEmitCharToIntegerWritesC(t *testing.T) {
	// Confirm the emitted C directly: the CharToInteger lowers to a plain C
	// cast of the char value's expression to the destination C type,
	// (uint32_t)(...), with no runtime helper.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let c char = 'A'; let n u32 = c as u32; return n as i32; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(uint32_t)(pebble_local_") {
		t.Errorf("emitted C missing the char-to-integer cast (uint32_t)(pebble_local_...):\n%s", out)
	}
}

func TestEmitPointerToIntegerU64CompilesAndRuns(t *testing.T) {
	// The exact minimal repro of the pointer-to-integer shape: `let n u64 = p
	// as u64;` reads out a pointer's address as a u64. The address itself is
	// non-deterministic (it depends on the stack), so the deterministic
	// assertion is that the cast compiles and runs AND the pointer still
	// dereferences to 42 afterwards — the cast node is emitted as the u64
	// local's initializer, and if that emission were broken the whole program
	// would fail to compile or misbehave.
	emitAndRun(t, "fn main() i32 { var x i32 = 42; let p *i32 = &x; let n u64 = p as u64; return *p; }", false, 42, false)
}

func TestEmitPointerToIntegerUintCompilesAndRuns(t *testing.T) {
	// The uint twin: `let n uint = p as uint;`. uint is the platform-native
	// pointer-width builtin the backend routes through buildUintExpr, so this
	// exercises the buildUintExpr PointerToInteger lowering (a plain C cast to
	// uint64_t) as opposed to buildExpr's.
	emitAndRun(t, "fn main() i32 { var x i32 = 42; let p *i32 = &x; let n uint = p as uint; return *p; }", false, 42, false)
}

func TestEmitPointerToIntegerHashPtrCompilesAndRuns(t *testing.T) {
	// The real motivating case from std/hash.peb:80-82 — `fn hash_ptr[T](ptr
	// *T) u64 { return hash_u64(ptr as u64); }` — as a standalone fixture
	// (std/hash.peb itself is untouched). The `ptr as u64` is a
	// PointerToInteger whose destination (u64) is the argument width of
	// hash_u64, exactly the shape the std module needs. The cast's address
	// result is discarded; the deterministic assertion is the deref through
	// the original pointer still returns 42.
	emitAndRun(t, "fn hash_u64(val u64) u64 { return val; }\nfn hash_ptr[T](ptr *T) u64 { return hash_u64(ptr as u64); }\nfn main() i32 { var x i32 = 42; let p *i32 = &x; let h u64 = hash_ptr(p); return *p; }", false, 42, false)
}

func TestEmitPointerToIntegerWritesC(t *testing.T) {
	// Confirm the emitted C directly: the PointerToInteger lowers to a plain C
	// cast of the pointer expression to the destination C type,
	// (uint64_t)(...), with no runtime helper.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var x i32 = 42; let p *i32 = &x; let n u64 = p as u64; return *p; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "(uint64_t)(pebble_local_") {
		t.Errorf("emitted C missing the pointer-to-integer cast (uint64_t)(pebble_local_...):\n%s", out)
	}
}

// --- 10.46: integer-to-enum casts (CheckedIntegerToEnum) ---

// emitAndRunRelease drives one .peb entry source through buildFixture, Emit,
// and an end-to-end cc compile + run in PEBBLE_RT_MODE_RELEASE — the release
// twin of emitAndRun (which compiles in SAFE mode), so a checked primitive's
// mode-dependent behavior can be asserted at both configurations.
func emitAndRunRelease(t *testing.T, sourceText string, wantCode int, wantAbnormal bool) {
	t.Helper()
	unit, snapshot, entryID, sources := buildFixture(t, sourceText, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, wantCode, wantAbnormal, false)
}

func TestEmitCheckedIntegerToEnumOutOfRangeSafePanics(t *testing.T) {
	// The minimal repro: `5 as Color` where Color has three variants (red,
	// green, blue, ordinals 0-2) — 5 names no real variant. Compiled in
	// PEBBLE_RT_MODE_SAFE, the cast must panic through the runtime's checked
	// primitive before the enum value is ever used, so the process terminates
	// abnormally rather than exiting 0. The cast's destination enum type is
	// declared only as a local's type (never constructed or switched on), so
	// this also proves the enum typedef is still emitted for a cast-reached
	// enum.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 5 as Color;\nreturn 0;\n}", false, 0, true)
}

func TestEmitCheckedIntegerToEnumOutOfRangeReleaseDoesNotPanic(t *testing.T) {
	// The same `5 as Color` in PEBBLE_RT_MODE_RELEASE: the bounds check is
	// skipped entirely (release trusts the input), so the cast produces some
	// enum value and the program runs to its return 0. Which enum value it is
	// is explicitly NOT asserted — release is unchecked, so any value is
	// acceptable; the assertion is only that the program did not crash.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 5 as Color;\nreturn 0;\n}", 0, false)
}

func TestEmitCheckedIntegerToEnumValidRoundTripsSafe(t *testing.T) {
	// An in-range cast, `1 as Color` (ordinal 1, green), verified end to end
	// by casting it back with EnumToInteger: the round-trip value must be 1 in
	// SAFE mode.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 1 as Color;\nreturn c as i32;\n}", false, 1, false)
}

func TestEmitCheckedIntegerToEnumValidRoundTripsRelease(t *testing.T) {
	// The same in-range `1 as Color` round trip in RELEASE mode: the unchecked
	// cast passes 1 through, cast back to i32 it is still 1.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 1 as Color;\nreturn c as i32;\n}", 1, false)
}

func TestEmitCheckedIntegerToEnumNegativeSafePanics(t *testing.T) {
	// `-1 as Color`: a genuinely negative signed source. The backend widens it
	// to int64_t (sign-extending), and the primitive's unsigned comparison
	// (uint64_t)(-1) >= (uint64_t)3 must reject it — the exact case the
	// unsigned-comparison design exists to get right. SAFE mode panics.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = -1 as Color;\nreturn 0;\n}", false, 0, true)
}

func TestEmitCheckedIntegerToEnumNegativeReleaseDoesNotPanic(t *testing.T) {
	// The same `-1 as Color` in RELEASE mode: no bounds check, so the program
	// runs to its return 0 instead of panicking.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = -1 as Color;\nreturn 0;\n}", 0, false)
}

func TestEmitCheckedIntegerToEnumFromLocalSafePanics(t *testing.T) {
	// A source value from a local (not a literal): `n as Color` where n is an
	// i32 local holding 5 — out of range for a 3-variant enum. The local read
	// feeds the same checked cast; SAFE mode panics.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet n i32 = 5;\nlet c Color = n as Color;\nreturn 0;\n}", false, 0, true)
}

func TestEmitCheckedIntegerToEnumFromLocalValidRoundTripsSafe(t *testing.T) {
	// An in-range local source: n = 2, `n as Color` is ordinal 2 (blue),
	// round-tripped back through EnumToInteger to 2.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet n i32 = 2;\nlet c Color = n as Color;\nreturn c as i32;\n}", false, 2, false)
}

func TestEmitCheckedIntegerToEnumStorePositionCompilesAndRuns(t *testing.T) {
	// A store-position cast: `c = 2 as Color;` reassigns an already-declared
	// enum local (buildStoreCore's enum branch routes the value through
	// buildEnumValue, which now accepts the cast), so the store lands ordinal 2
	// (blue) and the round trip returns 2.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nc = 2 as Color;\nreturn c as i32;\n}", false, 2, false)
}

func TestEmitCheckedIntegerToEnumComparisonPositionCompilesAndRuns(t *testing.T) {
	// A comparison-position cast: `(2 as Color) == Color.blue` compares the
	// cast result against a variant literal (buildComparison's enum branch),
	// which is true since the cast produces ordinal 2.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nif (2 as Color) == Color.blue { return 1; } else { return 0; }\n}", false, 1, false)
}

func TestEmitCheckedIntegerToEnumWritesC(t *testing.T) {
	// Confirm the emitted C directly: the cast lowers to
	// `(<enum typedef>)pebble_rt_checked_int_to_enum((int64_t)(<child>),
	// <variant_count>, <loc>)` — the source widened to the primitive's int64_t
	// input, the variant count from the destination enum's 3 declared members,
	// and the result narrowed back to the enum typedef.
	unit, snapshot, entryID, enumType, _, sources := enumFixture(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 1 as Color;\nreturn c as i32;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	want := "(" + enumTypeName(enumType) + ")pebble_rt_checked_int_to_enum((int64_t)(1), 3, (PebbleSourceLoc)"
	if !strings.Contains(out, want) {
		t.Errorf("emitted C missing the integer-to-enum cast call %q:\n%s", want, out)
	}
	for _, want := range []string{
		"typedef enum {",
		"} " + enumTypeName(enumType) + ";",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

// --- OptionalIntegerToEnum: integer-to-optional-enum casts (`5 as ?Color`) ---

func TestEmitOptionalIntegerToEnumOutOfRangeHasValueFalseSafe(t *testing.T) {
	// The exact repro: `var c ?Color = 5 as ?Color;` where Color has three
	// variants (red, green, blue, ordinals 0-2) — 5 names no real variant.
	// The cast must produce an optional whose has_value is false, verified by
	// reading c.has_value directly. SAFE mode.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 5 as ?Color;\nif c.has_value { return 1; } else { return 0; }\n}", false, 0, false)
}

func TestEmitOptionalIntegerToEnumOutOfRangeHasValueFalseRelease(t *testing.T) {
	// The same `var c ?Color = 5 as ?Color;` in PEBBLE_RT_MODE_RELEASE.
	// Unlike the checked cast (which skips its check in RELEASE), the
	// optional's validity query must be correct in BOTH modes — a wrong
	// has_value would be silently incorrect, not merely unchecked — so
	// has_value is false here too, exactly as in SAFE.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 5 as ?Color;\nif c.has_value { return 1; } else { return 0; }\n}", 0, false)
}

func TestEmitOptionalIntegerToEnumOutOfRangeUnwrapPanicsSafe(t *testing.T) {
	// The strongest confirmation that has_value is false: force-unwrapping
	// the absent optional (`c!`) panics through
	// pebble_rt_checked_unwrap_i32 in SAFE mode, so the process terminates
	// abnormally rather than returning anything.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 5 as ?Color;\nreturn c! as i32;\n}", false, 0, true)
}

func TestEmitOptionalIntegerToEnumOutOfRangeUnwrapPanicsRelease(t *testing.T) {
	// The same force-unwrap in RELEASE: unwrapping an absent optional panics
	// in every configuration (the runtime's unwrap is not mode-gated), so
	// the process terminates abnormally here too — and the panicking unwrap
	// is itself the proof that has_value is false in RELEASE as well.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 5 as ?Color;\nreturn c! as i32;\n}", 0, true)
}

func TestEmitOptionalIntegerToEnumValidRoundTripsSafe(t *testing.T) {
	// An in-range cast, `1 as ?Color` (ordinal 1, green): has_value must be
	// true, and the unwrapped value must be the green variant — verified by a
	// round trip through EnumToInteger, exactly the CheckedIntegerToEnum
	// pattern (`return c as i32` == 1), with the unwrap reading the optional
	// local's stored enum value. SAFE mode.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 1 as ?Color;\nif !c.has_value { return 99; }\nreturn c! as i32;\n}", false, 1, false)
}

func TestEmitOptionalIntegerToEnumValidRoundTripsRelease(t *testing.T) {
	// The same in-range `1 as ?Color` round trip in RELEASE mode: the
	// validity query and the value both behave identically to SAFE, so the
	// unwrapped value still round-trips to 1 (green).
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 1 as ?Color;\nif !c.has_value { return 99; }\nreturn c! as i32;\n}", 1, false)
}

func TestEmitOptionalIntegerToEnumFromLocalCompilesAndRuns(t *testing.T) {
	// A source value from a local (not a literal): `n as ?Color` where n is
	// an i32 local. In range (n = 2, blue), has_value is true and the unwrap
	// round-trips to 2; out of range (n = 7), has_value is false.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 2;\nvar c ?Color = n as ?Color;\nif !c.has_value { return 99; }\nreturn c! as i32;\n}", false, 2, false)
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 7;\nvar c ?Color = n as ?Color;\nif c.has_value { return 99; } else { return 0; }\n}", false, 0, false)
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 2;\nvar c ?Color = n as ?Color;\nif !c.has_value { return 99; }\nreturn c! as i32;\n}", 2, false)
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 7;\nvar c ?Color = n as ?Color;\nif c.has_value { return 99; } else { return 0; }\n}", 0, false)
}

func TestEmitOptionalIntegerToEnumEvaluatesSourceExactlyOnce(t *testing.T) {
	// The actual point of the pre-statement design: the cast must evaluate
	// its source integer exactly ONCE. bump(&count) increments count through
	// the pointer and returns 0; if the source expression were embedded twice
	// (once for the has_value query, once for the value), count would be 2.
	// It must be 1. SAFE mode.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn bump(p *i32) i32 { *p = *p + 1; return 0; } fn main() i32 {\nvar count i32 = 0;\nvar c ?Color = bump(&count) as ?Color;\nif !c.has_value { return 99; }\nreturn count;\n}", false, 1, false)
}

func TestEmitOptionalIntegerToEnumEvaluatesSourceExactlyOnceRelease(t *testing.T) {
	// The same single-evaluation guarantee in RELEASE mode — identical
	// behavior, since the temp-hoisting design is mode-independent.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn bump(p *i32) i32 { *p = *p + 1; return 0; } fn main() i32 {\nvar count i32 = 0;\nvar c ?Color = bump(&count) as ?Color;\nif !c.has_value { return 99; }\nreturn count;\n}", 1, false)
}

func TestEmitOptionalIntegerToEnumForLoopInitializerCompilesAndRuns(t *testing.T) {
	// The for-loop-initializer position: `for var c ?Color = ...; ...` — the
	// cast is hoisted into an int64_t temp emitted as a statement before the
	// for, and the header's init clause is the optional-typed local reading
	// that temp. In range (1), the condition c.has_value is true so the body
	// breaks and the program returns 1; out of range (5), has_value is false
	// so the loop body never runs and the program returns 0. Bounded
	// execution, in both modes.
	emitAndRunBounded(t, "type Color = enum { red, green, blue }; fn main() i32 {\nfor var c ?Color = 1 as ?Color; c.has_value; { break; }\nreturn 1;\n}", false, 1, false)
	emitAndRunBounded(t, "type Color = enum { red, green, blue }; fn main() i32 {\nfor var c ?Color = 5 as ?Color; c.has_value; { break; }\nreturn 0;\n}", false, 0, false)
	unit, snapshot, entryID, sources := buildFixture(t, "type Color = enum { red, green, blue }; fn main() i32 {\nfor var c ?Color = 1 as ?Color; c.has_value; { break; }\nreturn 1;\n}", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileEmittedCRelease(t, buf.Bytes())
}

func TestEmitRejectsOptionalIntegerToEnumReturnPosition(t *testing.T) {
	// A return-position cast (`return 5 as ?Color;`) is rejected by the
	// CHECKER before the backend ever runs — a clean compile-time rejection,
	// not a backend crash and not a double-evaluated emission.
	if _, _, _, _, err := buildFixtureMaybeFailing(t, "type Color = enum { red, green, blue }; fn main() i32 {\nreturn 5 as ?Color;\n}", "main", false); err == nil {
		t.Errorf("checker accepted an integer-to-optional-enum cast in a return position")
	}
}

func TestEmitRejectsOptionalIntegerToEnumCallArgumentPosition(t *testing.T) {
	// A cast used as a call argument (`helper(5 as ?Color)` where helper
	// takes a ?Color parameter) reaches the backend and is cleanly rejected
	// naming the parameter's optional type — the pre-existing optional-typed
	// parameter limitation, never a crash or a guessed lowering.
	unit, snapshot, entryID, _ := buildFixture(t, "type Color = enum { red, green, blue }; fn helper(c ?Color) i32 { return 0; } fn main() i32 {\nreturn helper(5 as ?Color);\n}", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "parameter 0 (symbol")
}

func TestEmitOptionalIntegerToEnumWritesC(t *testing.T) {
	// Confirm the emitted C directly: the source integer is evaluated exactly
	// once into an int64_t temp (a pre-statement), and the optional local is
	// then constructed with both fields read from that single temp — the
	// has_value bool from the runtime validity query and the enum value
	// narrowed from the temp. The temp name derives from the Initialize
	// node's own id, mirroring pebble_compound_ptr_<id>. The enum typedef is
	// emitted before the optional typedef that names it as the value field.
	unit, snapshot, entryID, enumType, _, sources := enumFixture(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 1 as ?Color;\nreturn c! as i32;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if got := strings.Count(out, "int64_t pebble_temp_"); got != 1 {
		t.Errorf("source integer temp appears %d time(s), want exactly one (single evaluation):\n%s", got, out)
	}
	for _, want := range []string{
		"int64_t pebble_temp_",
		".has_value = pebble_rt_int_to_enum_is_valid(pebble_temp_",
		".value = (" + enumTypeName(enumType) + ")pebble_temp_",
		"(" + enumTypeName(enumType) + ")pebble_rt_checked_unwrap_i32(pebble_local_",
		"typedef struct {\n    bool has_value;\n    " + enumTypeName(enumType) + " value;\n} pebble_optional_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	enumIndex := strings.Index(out, "typedef enum {")
	optionalIndex := strings.Index(out, "} pebble_optional_")
	if enumIndex < 0 || optionalIndex < 0 || enumIndex > optionalIndex {
		t.Errorf("enum typedef does not precede the optional typedef that names it (definition before use):\n%s", out)
	}
}

func TestEmitOptionalImplicitInjectionCompilesAndRuns(t *testing.T) {
	// Implicit optional injection (`var o ?int = 5;`, no `some` keyword) is a
	// tir.OptionalInject node, distinct from an explicit tir.SomeOptional —
	// found unimplemented during a real-code audit (checker accepts it,
	// backend previously rejected it by name: "declares an optional-typed
	// local ... initialized from a OptionalInject, want some <expr> or
	// none"). Fixed by handling OptionalInject alongside SomeOptional in
	// buildOptionalLocalDeclaration's switch, since both lower to the
	// identical C. This is scoped to local declarations only, matching
	// SomeOptional's own existing scope — not a claim that implicit
	// injection works everywhere (e.g. an optional function RESULT type is
	// a separate, broader, not-yet-supported restriction, confirmed
	// unrelated to this fix and tracked separately).
	emitAndRun(t, "fn main() int {\nvar o ?int = 5;\nif o.has_value { return 1; } else { return 0; }\n}", false, 1, false)
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

func TestEmitUnionRecordConstructQualifiedCompilesAndRuns(t *testing.T) {
	// The .{ Int = 42 } construction surface for a tagged union, qualified form:
	// finishRecord routes it to an aggregateTaggedVariant record and the IR
	// builder produces the same VariantConstruct node as the call-syntax form
	// Data.Int(42), so the backend lowers it identically and the switch fires
	// the Int case (exit 1), proving the two syntaxes are equivalent.
	emitAndRun(t, "type Data = union enum { Int i32; Str str; }; fn main() i32 {\nvar d Data = Data.{ Int = 42 };\nswitch d { case Data.Int: return 1; case Data.Str: return 0; }\n}", false, 1, false)
}

func TestEmitUnionRecordConstructInferredCompilesAndRuns(t *testing.T) {
	// Same construction via the inferred receiver form (.{ Int = 42 } against an
	// annotated destination): the variant symbol is re-derived by name at IR
	// build time, and the emitted switch still fires the Int case (exit 1).
	emitAndRun(t, "type Data = union enum { Int i32; Str str; }; fn main() i32 {\nvar d Data = .{ Int = 42 };\nswitch d { case Data.Int: return 1; case Data.Str: return 0; }\n}", false, 1, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	// A tagged-union payload that is not exactly the entry's resolved width,
	// bool, or str — a tuple, struct, array, optional, or nested enum — is
	// reachable from real source (the checker accepts such a variant
	// declaration and construction) but is a clean rejection naming what is
	// unsupported, never guessed at. The rejection happens in the union-type
	// collection walk, where each constructed variant's payload type is first
	// resolved from its construction site.
	emitAndRunRejects(t, "type C = union enum { empty void; value (i32, i32); }; fn main() i32 {\nvar c C = C.value((1, 2));\nreturn 0;\n}", "carries a payload of type (int, int); only a payload of i32, bool, or str is supported")
}

func TestEmitNarrowedUnionVariantPayloadReadCompilesAndRuns(t *testing.T) {
	// Slice C's end-to-end program: a narrowed union-variant payload read
	// (`r.Ok` inside `case .Ok:`) must emit the payload projection the union's
	// construction side fills — pebble_local_<sym>.payload.pebble_field_<m> —
	// inside a helper whose union-typed parameter is passed by value. The
	// union also carries a str-typed payload (Err = "bad"), so the union
	// payload gate now admits str (PebbleStr member). unwrap_or(a, 0) reads
	// the Ok payload 42; unwrap_or(b, 100) falls to its def; 42 + 100 = 142.
	emitAndRun(t, `type Result[T, E] = union enum {
    Ok T;
    Err E;
};
fn unwrap_or(r Result[int, str], def int) int {
    switch r {
        case .Ok: return r.Ok;
        case .Err: return def;
    }
}
fn main() int {
    let a = Result[int, str].{ Ok = 42 };
    let b = Result[int, str].{ Err = "bad" };
    return unwrap_or(a, 0) + unwrap_or(b, 100);
}`, false, 142, false)
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

func TestEmitU8SliceFromArrayCompilesAndRuns(t *testing.T) {
	// []u8, an entry-width-independent fixed-width integer element (main
	// returns int, not u8): construct a [3]u8 array, slice it, index-read
	// element 1. Confirms both the array-typed-local element gate and the
	// slice-construction/index-read paths accept a non-entry-width scalar.
	emitAndRun(t, "fn main() int { var arr [3]u8 = [1 as u8, 2 as u8, 3 as u8]; var s []u8 = arr[:]; return s[1] as int; }", false, 2, false)
}

func TestEmitCharSliceFromArrayCompilesAndRuns(t *testing.T) {
	// []char, mirroring the u8 case above for char specifically (its own
	// fixed int32_t C representation, not a resolvedBuiltin integer).
	emitAndRun(t, "fn main() int { var arr [3]char = ['a', 'b', 'c']; var s []char = arr[:]; if s[1] == 'b' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitI64SliceNonEntryWidthCompilesAndRuns(t *testing.T) {
	// Unlike TestEmitSliceI64CompilesAndRuns (where i64 IS the entry width,
	// since main returns i64 there), this entry returns int, so i64 here is
	// genuinely a non-ambient width — the case that previously only worked
	// by coincidence when i64 happened to match the entry's own width.
	emitAndRun(t, "fn main() int { var arr [3]i64 = [100, 200, 300]; var s []i64 = arr[:]; return s[1] as int; }", false, 200, false)
}

func TestEmitU64EqualityComparisonCompilesAndRuns(t *testing.T) {
	// The exact minimal repro for the non-entry-width comparison bug: a u64
	// local (a non-entry width in an int-entry main) compared with == against
	// a literal, the result stored in a bool local and used in an if.
	// Previously failed with "entry function body expression contains a
	// SymbolValue of type u64, want int" because buildComparisonOperand built
	// both operands at the AMBIENT entry width rather than each operand's own
	// resolved width. The == is false (5 != 0), so the else branch runs and
	// exit code 1 proves the comparison emitted and evaluated correctly.
	emitAndRun(t, "fn main() int { let h u64 = 5; let eq bool = h == 0; if eq { return 0; } else { return 1; } }", false, 1, false)
}

func TestEmitU64OrderingComparisonsCompilesAndRuns(t *testing.T) {
	// All four ordering operators on a non-entry-width integer (u64 in an
	// int-entry main), each taking the branch matching its value so both true
	// and false outcomes are exercised — proving the fix is not specific to
	// equality. Each table row is also self-anchoring: the literal operand is
	// unified to u64 by the checker, so both operands of every comparison
	// resolve to the same width.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"less", "fn main() int { let h u64 = 5; if h < 10 { return 1; } else { return 2; } }", 1},
		{"less false", "fn main() int { let h u64 = 5; if h < 5 { return 1; } else { return 2; } }", 2},
		{"lessEqual", "fn main() int { let h u64 = 5; if h <= 5 { return 1; } else { return 2; } }", 1},
		{"lessEqual false", "fn main() int { let h u64 = 5; if h <= 4 { return 1; } else { return 2; } }", 2},
		{"greater", "fn main() int { let h u64 = 5; if h > 4 { return 1; } else { return 2; } }", 1},
		{"greater false", "fn main() int { let h u64 = 5; if h > 5 { return 1; } else { return 2; } }", 2},
		{"greaterEqual", "fn main() int { let h u64 = 5; if h >= 5 { return 1; } else { return 2; } }", 1},
		{"greaterEqual false", "fn main() int { let h u64 = 5; if h >= 6 { return 1; } else { return 2; } }", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitU64CheckedArithmeticCompilesAndRuns(t *testing.T) {
	// u64 add/sub/mul as a non-ambient width (u64 operands in u64-returning
	// helpers called from an int-entry main, mirroring the i64
	// non-entry-width slice test's avoidance of the entry width coinciding
	// with the type under test). Each helper lowers its CheckedArithmetic to
	// pebble_rt_checked_add/sub/mul_u64, which must produce the arithmetically
	// correct result for ordinary non-wrapping operands, returned via `as
	// int` and asserted as the process exit code.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"add", "fn addU(x u64, y u64) u64 { return x + y; } fn main() int { let r u64 = addU(40, 2); return r as int; }", 42},
		{"sub", "fn subU(x u64, y u64) u64 { return x - y; } fn main() int { let r u64 = subU(50, 8); return r as int; }", 42},
		{"mul", "fn mulU(x u64, y u64) u64 { return x * y; } fn main() int { let r u64 = mulU(6, 7); return r as int; }", 42},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitU64CheckedArithmeticWritesU64Helper(t *testing.T) {
	// Assert the exact helper names in the emitted C: a u64-returning
	// helper's CheckedArithmetic must lower to pebble_rt_checked_add_u64 (not
	// the empty-suffix pebble_rt_checked_add_ the pre-fix code emitted, which
	// only failed later at cc compile time), proving the resolved u64 width
	// really reaches the runtime function-name selection.
	unit, snapshot, entryID, sources := buildFixture(t, "fn addU(x u64, y u64) u64 { return x + y; } fn main() int { let r u64 = addU(40, 2); return r as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_add_u64(") {
		t.Errorf("emitted C missing the u64 checked-add helper:\n%s", out)
	}
	if strings.Contains(out, "pebble_rt_checked_add_(") || strings.Contains(out, "pebble_rt_checked_add_i64(") {
		t.Errorf("emitted C uses a wrong-width checked-add helper for u64 operands:\n%s", out)
	}
}

func TestEmitU64CheckedArithmeticOverflowAborts(t *testing.T) {
	// u64 overflow detection is real, not just "it compiles": each of add
	// (UINT64_MAX + 1), sub (0 - 1), and mul (UINT64_MAX * 2) wraps the
	// unsigned width, and the emitted pebble_rt_checked_*_u64 helper must
	// detect it with __builtin_*_overflow and panic through pebble_rt_panic
	// in PEBBLE_RT_MODE_SAFE — the process must terminate abnormally, not
	// exit 0 and not return any specific wrapped value.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"add", "fn addU(x u64, y u64) u64 { return x + y; } fn main() int { let r u64 = addU(18446744073709551615, 1); return r as int; }"},
		{"sub", "fn subU(x u64, y u64) u64 { return x - y; } fn main() int { let r u64 = subU(0, 1); return r as int; }"},
		{"mul", "fn mulU(x u64, y u64) u64 { return x * y; } fn main() int { let r u64 = mulU(18446744073709551615, 2); return r as int; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, 0, true)
		})
	}
}

func TestEmitU64HashBytesFnv1aCompilesAndRuns(t *testing.T) {
	// The motivating case: a helper mirroring std/hash.peb's hash_bytes
	// FNV-1a body verbatim (`hash = hash ^ (x as u64); hash = hash *
	// fnv_prime;` inside a loop over a real []u8 slice), called from an
	// int-entry main. This combines the u64 checked-arithmetic fix with the
	// already-landed comparison (30fca68) and slice-element-type (f85b4a0)
	// fixes: the body's u64 multiply and slice index must now emit the real
	// pebble_rt_checked_mul_u64 / pebble_rt_checked_index_u64 helpers rather
	// than the empty-suffix names that previously failed at cc compile time.
	// The fixed loop bound stands in for `loop 0..data.len : i`, whose
	// uint-typed range bound is a separate pre-existing blocker unrelated to
	// this slice (std/hash.peb itself still can't be imported for other
	// separately-tracked cast reasons; this fixture is standalone).
	//
	// SAFE mode: the FNV-1a multiply genuinely wraps mod 2^64 (the offset
	// basis times fnv_prime exceeds 2^64 on the very first iteration), so
	// the checked-mul helper must panic — the run terminates abnormally,
	// proving the u64 overflow path is reached by a real hash workload.
	// RELEASE mode: the same multiply wraps (the unsigned type's own defined
	// semantics), so the whole FNV-1a body runs to completion and the
	// resulting hash is nonzero, returned as exit code 0.
	src := "fn hash_bytes(data []u8) u64 { var hash u64 = 14695981039346656037; let fnv_prime u64 = 1099511628211; loop 0..3 : i { hash = hash ^ (data[i] as u64); hash = hash * fnv_prime; } return hash; } fn main() int { var data [3]u8 = [1, 2, 3]; var s []u8 = data[:]; var h u64 = hash_bytes(s); if h == 0 { return 1; } return 0; }"
	unit, snapshot, entryID, sources := buildFixture(t, src, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_rt_checked_mul_u64(",
		"pebble_rt_checked_index_u64(",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 0, true)
	releaseBinary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, releaseBinary, 0, false, false)
}

func TestEmitU64SliceConstructionInHelperCompilesAndRuns(t *testing.T) {
	// The slice-start gap this slice closes: constructing an ordinary INT
	// slice (arr[:]) inside a u64-returning helper — the u64-ness comes from
	// the AMBIENT function width, not the slice's own element type — must
	// lower to pebble_rt_checked_slice_start_u64 (not the empty-suffix name
	// that previously failed at cc compile time, nor a nonexistent helper).
	// The slice must build with correct bounds and its element read correctly,
	// called from an int-entry main.
	emitAndRun(t, "fn f() u64 { var arr [3]int = [1, 2, 3]; var s []int = arr[:]; return s[1] as u64; } fn main() int { return f() as int; }", false, 2, false)
}

func TestEmitU64SliceConstructionOutOfBoundsAborts(t *testing.T) {
	// The out-of-bounds twin of the slice-start fix: a runtime end bound past
	// the array length inside a u64-returning helper must route through
	// pebble_rt_checked_slice_start_u64 and panic with
	// PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS in every configuration, so the process
	// terminates abnormally. A helper supplies the out-of-range end to bypass
	// the checker's compile-time range validation.
	emitAndRun(t, "fn getEnd() u64 { return 10; } fn f() u64 { var arr [3]int = [1, 2, 3]; var e u64 = getEnd(); var s []int = arr[0:e]; return s[0] as u64; } fn main() int { return f() as int; }", false, 0, true)
}

func TestEmitU64SliceConstructionWritesU64Helper(t *testing.T) {
	// Assert the exact helper name in the emitted C: a slice construction
	// inside a u64-returning helper must call pebble_rt_checked_slice_start_u64.
	unit, snapshot, entryID, sources := buildFixture(t, "fn f() u64 { var arr [3]int = [1, 2, 3]; var s []int = arr[:]; return s[1] as u64; } fn main() int { return f() as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_slice_start_u64(") {
		t.Errorf("emitted C missing the u64 checked-slice-start helper:\n%s", out)
	}
}

func TestEmitU64StrIndexInHelperCompilesAndRuns(t *testing.T) {
	// The str-index gap this slice closes: indexing a str (s[i]) inside a
	// u64-returning helper must lower to pebble_rt_str_char_at_u64 (not the
	// empty-suffix name that previously failed at cc compile time), decode
	// the right codepoint, and drive a comparison to a real result.
	emitAndRun(t, "fn f() u64 { var s str = \"hi\"; var c char = s[1]; if c == 'i' { return 1; } return 0; } fn main() int { return f() as int; }", false, 1, false)
}

func TestEmitU64CompoundAndPostfixCompilesAndRuns(t *testing.T) {
	// The compound-assignment gate (checkedSuffix(placeWidth) == "") now
	// admits a u64 place for the +, -, * family (this slice's add/sub/mul_u64
	// helpers): += and postfix ++ on a u64 local inside a u64-returning helper
	// must route through pebble_rt_checked_add_u64 and produce the correct
	// value, called from an int-entry main.
	emitAndRun(t, "fn f() u64 { var x u64 = 40; x += 2; x++; return x; } fn main() int { return f() as int; }", false, 43, false)
}

func TestEmitU64CompoundOverflowStillAborts(t *testing.T) {
	// u64 compound += follows the same checked-overflow contract as a plain
	// `x = x + y`: adding 1 to UINT64_MAX must panic through
	// pebble_rt_checked_add_u64 in SAFE mode, so the process terminates
	// abnormally.
	emitAndRun(t, "fn f() u64 { var x u64 = 18446744073709551615; x += 1; return x; } fn main() int { return f() as int; }", false, 0, true)
}

func TestEmitRejectsU64CheckedDivision(t *testing.T) {
	// A u64 / or % has no checked runtime helper (div/mod is out of this
	// slice's scope — only +, -, * got u64 twins), so the backend must reject
	// it CLEANLY at Emit time, not emit a call to a nonexistent
	// pebble_rt_checked_div_u64/mod_u64 that would only fail at cc compile.
	// Both the plain expression form and the compound-assignment form are
	// asserted.
	emitAndRunRejects(t, "fn f() u64 { var a u64 = 10; var b u64 = 2; return a / b; } fn main() int { return f() as int; }", "only +, -, and * have a checked runtime helper")
	emitAndRunRejects(t, "fn f() u64 { var a u64 = 10; var b u64 = 2; return a % b; } fn main() int { return f() as int; }", "only +, -, and * have a checked runtime helper")
	emitAndRunRejects(t, "fn f() u64 { var x u64 = 10; x /= 2; return x; } fn main() int { return f() as int; }", "no checked division/modulo runtime helper")
	emitAndRunRejects(t, "fn f() u64 { var x u64 = 10; x %= 2; return x; } fn main() int { return f() as int; }", "no checked division/modulo runtime helper")
}

func TestEmitU8ComparisonCompilesAndRuns(t *testing.T) {
	// A different non-entry-width integer (u8 in an int-entry main) confirms
	// the fix generalizes rather than being u64-specific: both == and the
	// ordering operators build the u8 operands at their own resolved width.
	emitAndRun(t, "fn main() int { let h u8 = 3; if h == 3 { return 0; } else { return 1; } }", false, 0, false)
	emitAndRun(t, "fn main() int { let h u8 = 3; if h < 4 { return 0; } else { return 1; } }", false, 0, false)
}

func TestEmitRejectsMismatchedNonEntryWidthComparison(t *testing.T) {
	// A comparison between two mismatched non-entry-width integers (u64 vs
	// u8) is a CLEAN rejection — an error, never a crash and never
	// silently-wrong C. The checker requires both comparison operands to carry
	// the identical concrete type (validateBooleanOperators rejects
	// typeIDs[0] != typeIDs[1]) and rejects the mismatch itself at
	// type-check time as a T0505 "cannot unify" — so from real source the
	// rejection happens before typed IR ever reaches Emit. The backend's own
	// same-width guard in buildComparison (mirroring the enum branch) is
	// defense-in-depth for hand-built IR that bypasses the checker.
	_, _, _, _, err := buildFixtureMaybeFailing(t, "fn main() int { let a u64 = 5; let b u8 = 3; let eq bool = a == b; if eq { return 0; } else { return 1; } }", "main", false)
	if err == nil {
		t.Fatal("expected the checker to reject a u64 == u8 comparison, but the fixture built and checked successfully")
	}
	if !strings.Contains(err.Error(), "check failed") {
		t.Fatalf("expected a clean check-phase rejection, got: %v", err)
	}
}

func TestEmitU8SliceWritesUint8CType(t *testing.T) {
	// Emitted-C shape check: a []u8 array/slice pair must declare uint8_t,
	// not the ambient entry width's C type (int32_t) or any other width —
	// arrayElementCType's scalar fallback previously returned cType(width)
	// (the AMBIENT width), which would have been silently wrong here rather
	// than a clean rejection; this confirms the element's OWN resolved width
	// is what's actually emitted.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var arr [3]u8 = [1 as u8, 2 as u8, 3 as u8]; var s []u8 = arr[:]; return s[1] as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "uint8_t") {
		t.Errorf("expected uint8_t in emitted C, got:\n%s", out)
	}
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitSliceOfStructElementsCompilesAndRuns(t *testing.T) {
	// The standalone synthetic repro of the slice-of-struct-elements gap: a
	// []P slice constructed from a [3]P array via arr[:], then every index
	// shape std/hmap.peb's real insert/get/rehash use — a field write through a
	// slice index (s[1].state = .Occ, matching new_entries[i].state = .Empty),
	// an address-of slice index for in-place field mutation (let p *P = &s[2];
	// p.x = 50, matching `let entry = &self.entries[index]; entry.state =
	// .Occupied`), and a by-value element read into a struct local (let e =
	// s[1]; e.state == .Occ, matching `let e = old_entries[j];`) — compiles and
	// runs correctly. The exit code depends on all of them working: 30 (s[1].x
	// after the s[1].x = 30 write) + 50 (s[2].x after the pointer mutation) =
	// 80.
	emitAndRun(t, "type S = enum { Empty, Occ };\ntype P = struct { x i32; y i32; state S; };\nfn main() i32 {\nvar arr [3]P = [P.{ x = 1, y = 2, state = .Empty }, P.{ x = 3, y = 4, state = .Empty }, P.{ x = 5, y = 6, state = .Empty }];\nvar s []P = arr[:];\ns[1].state = .Occ;\ns[1].x = 30;\nlet p *P = &s[2];\np.x = 50;\np.state = .Occ;\nlet e = s[1];\nif e.state == .Occ { return e.x + s[2].x; }\nreturn 0;\n}", false, 80, false)
}

func TestEmitSliceOfStructElementsEmittedCShape(t *testing.T) {
	// The emitted-C shape check for a struct-element slice: the slice typedef's
	// .data field must be a pointer to the struct's OWN typedef name
	// (pebble_struct_<typeID>_t *data), not a rejection; the element struct's
	// typedef must be forward-declared BEFORE the slice typedef (C requires the
	// pointer target's name declared, even incompletely, before the slice block
	// references it — the slice block is emitted before the aggregate block
	// that fully defines the struct); and the struct's full definition must
	// carry the matching struct tag so the forward declaration and the
	// definition complete the same C type.
	unit, snapshot, entryID, sources := buildFixture(t, "type P = struct { x i32; y i32; };\nfn main() i32 { var a [3]P = [P.{ x = 1, y = 2 }, P.{ x = 3, y = 4 }, P.{ x = 5, y = 6 }]; var s []P = a[:]; return s[1].x + s[1].y; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// The slice typedef's data field must be a pointer to the struct's own
	// typedef name.
	fwdDecl := "typedef struct pebble_struct_"
	fwdIdx := strings.Index(out, fwdDecl)
	if fwdIdx < 0 {
		t.Fatalf("emitted C missing the struct forward typedef declaration:\n%s", out)
	}
	// Extract the struct typedef name from the forward declaration
	// `typedef struct pebble_struct_<id> pebble_struct_<id>_t;`.
	rest := out[fwdIdx+len(fwdDecl):]
	tagEnd := strings.Index(rest, " ")
	if tagEnd < 0 {
		t.Fatalf("emitted C malformed forward typedef declaration:\n%s", out)
	}
	structTag := "pebble_struct_" + rest[:tagEnd]
	structName := structTag + "_t"
	if !strings.Contains(out, structName+" *data;") {
		t.Fatalf("emitted C slice typedef's .data field is not a pointer to the struct's own typedef name %s:\n%s", structName, out)
	}
	// The struct's typedef name must be forward-declared before the slice
	// typedef that points at it.
	sliceIdx := strings.Index(out, "pebble_slice_")
	fwd := "typedef struct " + structTag + " " + structName + ";"
	if sliceIdx < 0 || !strings.Contains(out[:sliceIdx], fwd) {
		t.Fatalf("emitted C does not forward-declare the struct typedef before the slice typedef:\n%s", out)
	}
	// The struct's full definition must carry the matching tag.
	tag := "typedef struct " + structTag
	if !strings.Contains(out, tag) {
		t.Fatalf("emitted C struct definition missing the matching struct tag %s:\n%s", tag, out)
	}
	compileAndRun(t, buf.Bytes(), 7, false)
}

func TestEmitSliceOfStructElementsFromRawStdCompilesAndRuns(t *testing.T) {
	// The SliceFromRaw shape (`slice ptr, n`) with a struct element — how
	// std/hmap.peb's rehash/with_capacity actually construct
	// `let entries []Entry[K, V] = slice ptr, cap;` over an allocator-returned
	// pointer. SliceFromRaw is checker-restricted to the standard library
	// package (a C0619 "slice is restricted to the standard library package"
	// rejection for user modules), so this fixture builds in the std package
	// like the existing TestEmitSliceFromRawCompilesAndRuns. The slice's data
	// field points at a contiguous [3]P array (via &arr[0]) and indexes over
	// it, so the exit code proves the whole SliceFromRaw construction plus a
	// struct-element index read work.
	unit, snapshot, entryID, sources := buildStdFixture(t, "type P = struct { x i32; y i32; };\nfn main() i32 { var arr [3]P = [P.{ x = 1, y = 2 }, P.{ x = 3, y = 4 }, P.{ x = 5, y = 6 }]; let s []P = slice &arr[0], 3; return s[2].x + s[2].y; }", "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 11, false)
}

func TestEmitSliceEnumElementRejects(t *testing.T) {
	// A slice of enum elements is deliberately unsupported, mirroring the
	// pre-existing enum-typed ARRAY element restriction: an enum element is a
	// Nominal type exactly like a struct element, but sliceElementCType /
	// isSupportedSliceElementType exclude it explicitly (a separate,
	// already-tracked restriction). The struct/tuple/optional element widening
	// this test suite gained for slice-of-struct-elements deliberately does
	// NOT extend to enums, so a slice of an enum element must still be a clean
	// rejection naming the enum.
	emitAndRunRejects(t, "type E = enum { A, B }; fn main() i32 { var a [2]E = [E.A, E.B]; var s []E = a[:]; return 0; }", "enum-typed slice elements are not supported yet")
}

func TestEmitSliceOfTupleElementsCompilesAndRuns(t *testing.T) {
	// A tuple-element slice, the sibling of the struct-element widening: the
	// slice element gate (sliceElementCType / isSupportedSliceElementType)
	// accepts tuples exactly as arrays already do, so `var s [](i32, i32) =
	// a[0:2]` over a [3](i32, i32) array constructs a slice whose typedef's
	// data field points at the tuple's own typedef, and an indexed element
	// read projects through the tuple's positional field (s[1].0). The old
	// rejection of tuple-element slices is deliberately lifted (this exact
	// fixture used to be TestEmitSliceUnsupportedElementTypeRejects); the
	// exit code 3 = s[1].0 proves the whole construction-and-read path.
	emitAndRun(t, "fn main() i32 { var a [3](i32, i32) = [(1, 2), (3, 4), (5, 6)]; var s [](i32, i32) = a[0:2]; return s[1].0; }", false, 3, false)
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

func TestEmitUintBoundedRangeLoopReadsSliceLenCompilesAndRuns(t *testing.T) {
	// The exact shape examples/slice_minmax.peb uses (and the regression test
	// for the emit gap that file surfaced): a uint-bounded range loop whose end
	// bound is a slice's `.len` structural field — `loop 1..items.len : iter {`
	// — with the uint-typed iterator used to index the same slice inside the
	// body (`items[iter]`). The `.len` bound lowers to Load(FieldPlace) with
	// the StructuralFieldLen sentinel member; before the fix, buildPlaceLValue
	// fell through to declaredFieldType and failed with "field 4294967295 is
	// not declared". The loop's start `1` is anchored to the uint end bound, so
	// the iterator is a uint64_t C counter and the body index routes through
	// the uint index path. sub = arr[1:4] = [7, 99, 12], whose min is 7 — the
	// exit code — proving the loop actually iterates the slice at runtime, not
	// just that it emits.
	emitAndRun(t, "fn find_min(items []int) int { var min_val = items[0]; loop 1..items.len : iter { if items[iter] < min_val { min_val = items[iter]; } } return min_val; } fn main() int { var arr [5]int = [42, 7, 99, 12, 3]; var sub []int = arr[1:4]; return find_min(sub); }", false, 7, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitReSliceSliceFieldDefaultEndCompilesAndRuns(t *testing.T) {
	// Re-slicing an EXISTING slice-typed struct field with no start bound and
	// a runtime end — the exact String::as_slice() shape
	// (`return self.data[:self.len];` in std/string.peb). The CheckedSlice's
	// base is a Load(FieldPlace) reading the slice-typed field (NOT a bare
	// SymbolValue naming an array local), which buildSliceConstruction must
	// accept: the new slice's .data offsets the base slice's OWN .data
	// pointer, and the bounds-check helper's upper bound is the base slice's
	// RUNTIME .len field. arr = [10,20,30,40], self.data[:3] = [10,20,30], so
	// s[2] = 30 is the exit code.
	emitAndRun(t, `type Bag = struct {
    data []int;
    len uint;
};
fn view(self Bag) []int {
    return self.data[:self.len];
}
fn main() int {
    var arr [4]int = [10, 20, 30, 40];
    var b Bag = Bag.{ data = arr[:], len = 3 };
    var s []int = view(b);
    return s[2];
}`, false, 30, false)
}

func TestEmitReSliceSliceFieldExplicitStartCompilesAndRuns(t *testing.T) {
	// The explicit-start-bound twin of the re-slice fix: `self.data[1:self.len]`
	// must offset the base slice's own .data pointer by the runtime start, not
	// decay a raw array — proving the offset math, not just the zero-start
	// case. arr = [10,20,30,40], self.data[1:4] = [20,30,40], so s[1] = 30 is
	// the exit code.
	emitAndRun(t, `type Bag = struct {
    data []int;
    len uint;
};
fn view(self Bag) []int {
    return self.data[1:self.len];
}
fn main() int {
    var arr [4]int = [10, 20, 30, 40];
    var b Bag = Bag.{ data = arr[:], len = 4 };
    var s []int = view(b);
    return s[1];
}`, false, 30, false)
}

func TestEmitReSliceSliceFieldWritesC(t *testing.T) {
	// The emitted C for the re-slice fixture: the helper's CheckedSlice over a
	// Load(FieldPlace) slice base emits the same two-statement shape as the
	// array base (a temp declaration holding the checked-start result, then
	// the compound-literal construction using that temp for both .data and
	// .len), but with the slice-specific pieces: the runtime helper's upper
	// bound argument is the base slice's RUNTIME .len field
	// (pebble_local_28.pebble_field_25.len), the construction's .data is the
	// base slice's OWN .data pointer offset by the temp
	// (pebble_local_28.pebble_field_25.data + pebble_slice_ret_24), and the
	// .len is the runtime end bound minus the temp. Symbols 24 (slice type),
	// 25 (data field), 26 (len field), 28 (self), return value node 24, and
	// field value node 34 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, `type Bag = struct {
    data []int;
    len uint;
};
fn view(self Bag) []int {
    return self.data[:self.len];
}
fn main() int {
    var arr [4]int = [10, 20, 30, 40];
    var b Bag = Bag.{ data = arr[:], len = 3 };
    var s []int = view(b);
    return s[2];
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_slice_ret_24 = pebble_rt_checked_slice_start_i32(0, pebble_local_28.pebble_field_26, pebble_local_28.pebble_field_25.len, (PebbleSourceLoc){\"main.peb\"",
		"return (pebble_slice_24_t){ .data = pebble_local_28.pebble_field_25.data + pebble_slice_ret_24, .len = (size_t)(pebble_local_28.pebble_field_26 - pebble_slice_ret_24) };",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 30, false)
}

// --- Struct fields: slice-typed fields constructed inline ---

func TestEmitSliceStructFieldInlineConstructionCompilesAndRuns(t *testing.T) {
	// The exact repro this gap was filed for: a plain, non-generic struct with
	// a slice-typed field whose construction value is an inline slice
	// expression (`Bag.{ items = arr[:] }`). The RecordConstruct's field value
	// is a bare CheckedSlice node (confirmed against a real fixture dump), so
	// the construction needs the same two-statement temp-then-construction
	// shape a slice local's declaration and a slice return use: the checked
	// slice-start call is hoisted into a temp statement threaded ahead of the
	// struct declaration line, and the slice compound literal uses that temp
	// for both its .data offset and its .len. arr[:] = [1,2,3], so b.items[1] =
	// 2 is the exit code.
	emitAndRun(t, `type Bag = struct { items []int; };
fn main() int {
    var arr [3]int = [1, 2, 3];
    var b Bag = Bag.{ items = arr[:] };
    return b.items[1];
}`, false, 2, false)
}

func TestEmitSliceStructFieldInlineConstructionEmitsTempStatement(t *testing.T) {
	// The emitted C for the inline-construction fixture: the struct field
	// construction is the two-statement shape — a pebble_field_slice_<nodeID>
	// temp declaration holding the checked slice-start result, then the struct
	// declaration whose slice field's compound literal uses that temp for both
	// .data and .len. The temp name derives from the field value node's NodeID
	// (symbols 24 (Bag), 25 (items), 27 (arr), 28 (b), field value node 17, and
	// slice type 24 come from the real fixture dump), distinct from the
	// pebble_slice_start_<symbol> and pebble_slice_ret_<nodeID> temps so the
	// three can never collide.
	unit, snapshot, entryID, sources := buildFixture(t, `type Bag = struct { items []int; };
fn main() int {
    var arr [3]int = [1, 2, 3];
    var b Bag = Bag.{ items = arr[:] };
    return b.items[1];
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t *data;\n    size_t len;\n} pebble_slice_24_t;",
		"pebble_slice_24_t pebble_field_25;",
		"int32_t pebble_field_slice_17 = pebble_rt_checked_slice_start_i32(0, 3, 3, (PebbleSourceLoc){\"main.peb\"",
		"pebble_struct_23_t pebble_local_28 = { .pebble_field_25 = (pebble_slice_24_t){ .data = pebble_local_27 + pebble_field_slice_17, .len = (size_t)(3 - pebble_field_slice_17) } };",
		"return pebble_local_28.pebble_field_25.data[pebble_rt_checked_index_i32(1, (int32_t)pebble_local_28.pebble_field_25.len, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitSliceStructFieldSliceFromRawCompilesAndRuns(t *testing.T) {
	// The same gap via SliceFromRaw: a raw-pointer-derived slice (`slice ptr,
	// n` — the raw-slice builtin, restricted to std-package source) used
	// directly as a slice-typed field's construction value. The RecordConstruct
	// field value is a bare SliceFromRaw node (confirmed against a real fixture
	// dump), whose construction is a single expression with no temp
	// (buildRawSliceConstruction emits the compound literal directly). The
	// 1-element slice over the 42 value means b.items[0] = 42 is the exit code.
	unit, snapshot, entryID, sources := buildStdFixture(t, `type Bag = struct { items []i32; };
fn main() i32 {
    var value i32 = 42;
    var ptr *i32 = &value;
    var b Bag = Bag.{ items = slice ptr, 1 };
    return b.items[0];
}`, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// The SliceFromRaw construction is a single expression: no temp statement,
	// and the field value is the compound literal directly.
	if strings.Contains(out, "pebble_field_slice_") {
		t.Errorf("emitted C unexpectedly contains a field temp statement:\n%s", out)
	}
	if !strings.Contains(out, ".pebble_field_25 = (pebble_slice_24_t){ .data = pebble_local_28, .len = (size_t)(1) }") {
		t.Errorf("emitted C missing the inline SliceFromRaw field construction:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitSliceFieldReassignmentFromRawCompilesAndRuns(t *testing.T) {
	// A bare SliceFromRaw as a slice-field REASSIGNMENT value — the
	// std/string.peb grow shape (`self.data = slice ptr, new_cap;`), where the
	// Store's value is a SliceFromRaw node rather than a reference to a
	// slice-typed local. Before the fix, buildStoreCore's isSlice branch
	// rejected anything but a SymbolValue with "reassigns a slice-typed place
	// from a SliceFromRaw, want a reference to a slice-typed local in scope".
	// The 1-element slice over the 42 value means b.items[0] = 42 is the exit
	// code, proving the whole SliceFromRaw reassignment works at runtime, not
	// just that it emits.
	unit, snapshot, entryID, sources := buildStdFixture(t, `type Bag = struct { items []i32; };
fn main() i32 {
    var value i32 = 42;
    var ptr *i32 = &value;
    var b Bag = Bag.{ items = slice ptr, 1 };
    b.items = slice ptr, 1;
    return b.items[0];
}`, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitGlobalLetConstantAsFixedWidthArgumentCompilesAndRuns(t *testing.T) {
	// A top-level `let` global constant with an untyped literal initializer
	// stays at the abstract `int` builtin (not i32) until a use site pins it
	// to a concrete width — the std/io.peb `let SeekEnd = 2;` shape, passed
	// directly as an argument to a fixed-width `i32` parameter (mirroring
	// `fseek`'s `whence i32`). Before the fix, buildExpr's width gate
	// rejected the abstract-int-typed SymbolValue outright ("of type int,
	// want i32"); the checker side also needed a way to lower a reference to
	// a `let` global at all (globals aren't in locals scope), which the
	// buildDeclarations/buildValueRecord change provides by inlining a fresh
	// copy of the constant's initializer at each reference site. echo(Seed)
	// returning 7 proves the constant's value round-trips correctly through
	// a real i32-typed call argument, not just that it emits.
	unit, snapshot, entryID, sources := buildStdFixture(t, `let Seed = 7;
fn echo(x i32) i32 { return x; }
fn main() i32 {
    return echo(Seed);
}`, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 7, false)
}

func TestEmitSliceStructFieldLocalReferenceCompilesAndRuns(t *testing.T) {
	// The already-working shape must keep working exactly as before: construct
	// the slice into a local FIRST, then use that local as the field's
	// construction value. The field value is a SymbolValue naming a slice-typed
	// local, emitted as the local's own pebble_local_<symbol> C name with no
	// temp statement. arr[:] = [1,2,3], so b.items[1] = 2 is the exit code.
	emitAndRun(t, `type Bag = struct { items []int; };
fn main() int {
    var arr [3]int = [1, 2, 3];
    var s []int = arr[:];
    var b Bag = Bag.{ items = s };
    return b.items[1];
}`, false, 2, false)
}

func TestEmitGenericSliceStructFieldInlineConstructionCompilesAndRuns(t *testing.T) {
	// The generic case this gap was originally investigated for: a generic
	// struct with a slice field instantiated as Bag[int] and constructed inline
	// (`Bag[int].{ items = arr[:] }`). The root cause is unrelated to
	// genericity — the field value is the same bare CheckedSlice — so this
	// general fix must also resolve the generic case. arr[:] = [1,2,3], so
	// b.items[1] = 2 is the exit code.
	emitAndRun(t, `type Bag[K] = struct { items []K; };
fn main() int {
    var arr [3]int = [1, 2, 3];
    var b Bag[int] = Bag[int].{ items = arr[:] };
    return b.items[1];
}`, false, 2, false)
}

func TestEmitSliceStructFieldInlineConstructionAsCallArgumentRejects(t *testing.T) {
	// An inline slice construction in a pure expression position — a struct
	// value with such a field used as a call argument — is a clean rejection,
	// the same discipline buildSliceArgument applies to a bare CheckedSlice
	// call argument: a C function argument is a pure expression position with
	// nowhere to place the temp-declaration statement the construction needs.
	// The slice-typed-local-reference shape remains the supported spelling.
	emitAndRunRejects(t, `type Bag = struct { items []int; };
fn read(b Bag) int { return b.items[1]; }
fn main() int {
    var arr [3]int = [1, 2, 3];
    return read(Bag.{ items = arr[:] });
}`, "nowhere to place the temp-declaration statement")
}

func TestEmitOptionalResultCompilesAndRuns(t *testing.T) {
	// The exact repro this slice was filed for: an optional-returning helper
	// called as the direct initializer of a matching optional-typed local.
	// `return 5;` is an implicit injection whose return child is the bare
	// payload value (confirmed against a real fixture dump), so the backend
	// supplies the injection itself; has_value must be true and the unwrapped
	// value must be 5.
	emitAndRun(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
	emitAndRun(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultNoneCompilesAndRuns(t *testing.T) {
	// `return none;` from an optional-result helper: the caller-side has_value
	// must be false, so the false path of the if is taken and 0 is returned.
	// Both the bare tail return and the `some`-explicit form are exercised.
	emitAndRun(t, "fn f() ?int { return none; } fn main() int { var o ?int = f(); if o.has_value { return 99; } return 0; }", false, 0, false)
	emitAndRun(t, "fn f() ?int { return some 5; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultImplicitInjectionCompilesAndRuns(t *testing.T) {
	// The implicit-injection-in-return path specifically: `return 5;` with no
	// `some` keyword. The checker emits the bare payload IntegerLiteral as the
	// return child (no OptionalInject wrapper, unlike a local declaration's
	// OptionalInject and unlike an aggregate payload's OptionalInject), so the
	// backend's buildOptionalReturnValue must supply the has_value-true
	// injection itself. The unwrapped value round-trips to 5.
	emitAndRun(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
	// The explicit `some` form is the same C, confirming both spellings lower
	// identically through the return path.
	emitAndRun(t, "fn f() ?int { return some 5; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultForwardsLocalCompilesAndRuns(t *testing.T) {
	// A SymbolValue return: the helper declares an optional-typed local (itself
	// implicitly injected from the bare integer 5, exercising the existing
	// OptionalInject local-declaration path) and `return o;` forwards it,
	// emitting `return pebble_local_<o>;`. The entry assigns the call to a
	// matching optional local and unwraps 5.
	emitAndRun(t, "fn f() ?int { let o ?int = 5; return o; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultCallsHelperCompilesAndRuns(t *testing.T) {
	// One optional-returning helper calling another (`return g();`): the
	// return child is a DirectCall carrying the optional result type, built by
	// the same buildDirectCall machinery any call uses and forwarded as the
	// return value — the call already returns the optional's own C type.
	// g's 5 flows through f to the entry's unwrap.
	emitAndRun(t, "fn g() ?int { return 5; } fn f() ?int { return g(); } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultBoolPayloadCompilesAndRuns(t *testing.T) {
	// A bool-payload optional result: the payload is built by buildBoolExpr
	// (not hardcoded to an integer payload), and the unwrapped bool drives an
	// if at the call site (`if o!` — the same shape the existing bool optional
	// tests use for a local). The some-true and none forms both round-trip.
	emitAndRun(t, "fn f() ?bool { return some true; } fn main() int { var o ?bool = f(); if o! { return 1; } return 0; }", false, 1, false)
	emitAndRun(t, "fn f() ?bool { return none; } fn main() int { var o ?bool = f(); if !o.has_value { return 1; } return 0; }", false, 1, false)
}

func TestEmitOptionalResultStructPayloadCompilesAndRuns(t *testing.T) {
	// A struct-payload optional result: `return some P.{ x = 1, y = 2 };` is a
	// SomeOptional wrapping a RecordConstruct, built by the shared
	// buildOptionalValueExpr → buildStructValueExpr dispatch (the same payload
	// grammar a nested-in-aggregate optional uses). has_value must be true at
	// the call site and the emitted optional's value field must hold the
	// constructed struct. The payload field read-back (o!.x) is separately out
	// of scope — the caller-side FieldValue-on-unwrap read is a pre-existing,
	// unrelated limitation — so the value round-trip is asserted on the emitted
	// C (see TestEmitOptionalResultStructPayloadWritesC), which would fail to
	// compile under -Wall -Wextra -Werror if the payload type dispatch were
	// wrong. (A `return none;` variant for an aggregate payload is not covered
	// here: it trips a pre-existing, unrelated isEnumType heuristic that
	// misclassifies a struct payload as an enum when the struct is never
	// constructed anywhere in the program — reproducible with a bare
	// `var o ?P = none;` in main, no helper functions involved.)
	emitAndRun(t, "type P = struct { x int; y int; };\nfn f() ?P { return some P.{ x = 1, y = 2 }; } fn main() int { var o ?P = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
}

func TestEmitOptionalResultTuplePayloadCompilesAndRuns(t *testing.T) {
	// A tuple-payload optional result, in both spellings: the explicit
	// `return some (1, 2);` (SomeOptional wrapping a TupleValue) and the
	// implicit `return (1, 2);` whose aggregate payload the checker wraps in an
	// OptionalInject node (confirmed against a real fixture dump) — the shape
	// that motivated extending buildOptionalValueExpr to share the SomeOptional
	// case. has_value must be true at the call site; the tuple element
	// read-back (o!.0) is separately out of scope, so the value is asserted on
	// the emitted C, which would fail to compile if the payload dispatch were
	// wrong.
	emitAndRun(t, "fn f() ?(int, int) { return some (1, 2); } fn main() int { var o ?(int, int) = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
	emitAndRun(t, "fn f() ?(int, int) { return (1, 2); } fn main() int { var o ?(int, int) = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
}

func TestEmitOptionalResultWritesC(t *testing.T) {
	// The emitted C for the flagship repro: the optional typedef (bool
	// has_value plus the payload's int32_t value) precedes the helper, the
	// helper's signature declares its return type as pebble_optional_23_t (the
	// optional's own typedef, not the entry's scalar int32_t), its return
	// statement emits the injected compound literal
	// (pebble_optional_23_t){ .has_value = true, .value = 5 } for the bare
	// payload `return 5;`, and the call site initializes the local directly
	// from pebble_fn_24(ctx). Symbols 24 (f), 25 (main), 26 (o local), and
	// optional type 23 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return 1; } return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    int32_t value;\n} pebble_optional_23_t;",
		"static pebble_optional_23_t pebble_fn_24(PebbleContext *ctx) {",
		"    return (pebble_optional_23_t){ .has_value = true, .value = 5 };",
		"pebble_optional_23_t pebble_local_26 = pebble_fn_24(ctx);",
		"    (void)pebble_local_26;",
		"if (pebble_local_26.has_value) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static pebble_optional_23_t pebble_fn_24")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("optional typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitOptionalResultStructPayloadWritesC(t *testing.T) {
	// The emitted C for the struct-payload fixture: the struct typedef
	// precedes the optional typedef that names it as its value field
	// (definition before use — the same ordering the aggregate-typedef DFS
	// guarantees for tuple/struct payloads), and the helper's return emits the
	// nested compound literal with the struct construction as .value. Symbols
	// 24 (P), 25 (x), 26 (y), 27 (f), 28 (main), 29 (o local), struct type 23,
	// and optional type 24 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "type P = struct { x int; y int; };\nfn f() ?P { return some P.{ x = 1, y = 2 }; } fn main() int { var o ?P = f(); if o.has_value { return 1; } return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    pebble_struct_23_t value;\n} pebble_optional_24_t;",
		"static pebble_optional_24_t pebble_fn_27(PebbleContext *ctx) {",
		"    return (pebble_optional_24_t){ .has_value = true, .value = (pebble_struct_23_t){ .pebble_field_25 = 1, .pebble_field_26 = 2 } };",
		"pebble_optional_24_t pebble_local_29 = pebble_fn_27(ctx);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	structIndex := strings.Index(out, "typedef struct {\n    int32_t pebble_field_25")
	optionalIndex := strings.Index(out, "} pebble_optional_24_t;")
	if structIndex < 0 || optionalIndex < 0 || structIndex > optionalIndex {
		t.Errorf("struct typedef does not precede the optional typedef that names it (definition before use):\n%s", out)
	}
}

func TestEmitOptionalResultTuplePayloadWritesC(t *testing.T) {
	// The emitted C for the tuple-payload implicit-injection fixture: the
	// tuple typedef precedes the optional typedef, and the helper's return
	// emits the nested compound literal with the tuple construction as .value —
	// the OptionalInject-in-return shape, sharing buildOptionalValueExpr's
	// SomeOptional case. Symbols 24 (f), 25 (main), 26 (o local), tuple type
	// 23, and optional type 24 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn f() ?(int, int) { return (1, 2); } fn main() int { var o ?(int, int) = f(); if o.has_value { return 1; } return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n} pebble_tuple_23_t;",
		"typedef struct {\n    bool has_value;\n    pebble_tuple_23_t value;\n} pebble_optional_24_t;",
		"static pebble_optional_24_t pebble_fn_24(PebbleContext *ctx) {",
		"    return (pebble_optional_24_t){ .has_value = true, .value = (pebble_tuple_23_t){ 1, 2 } };",
		"pebble_optional_24_t pebble_local_26 = pebble_fn_24(ctx);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	tupleIndex := strings.Index(out, "} pebble_tuple_23_t;")
	optionalIndex := strings.Index(out, "} pebble_optional_24_t;")
	if tupleIndex < 0 || optionalIndex < 0 || tupleIndex > optionalIndex {
		t.Errorf("tuple typedef does not precede the optional typedef that names it (definition before use):\n%s", out)
	}
}

func TestEmitOptionalParameterCompilesAndRuns(t *testing.T) {
	// Optional-typed PARAMETERS: a helper taking a ?int parameter, called
	// with a scalar implicit-injection argument (`g(5)`, which arrives as an
	// OptionalInject node at a call site — unlike a return position's bare
	// payload). The parameter is seeded into the callee's scope exactly like
	// an optional local (localInfo{optional: ...}), so a body read
	// (o.has_value) resolves through the existing optional-local machinery.
	emitAndRun(t, "fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { return g(5); }", false, 1, false)
}

func TestEmitOptionalParameterNoneArgumentCompilesAndRuns(t *testing.T) {
	// A fresh `none` passed directly as the argument.
	emitAndRun(t, "fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { return g(none); }", false, 0, false)
}

func TestEmitOptionalParameterSomeArgumentCompilesAndRuns(t *testing.T) {
	// A fresh `some x` passed directly as the argument.
	emitAndRun(t, "fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { return g(some 5); }", false, 1, false)
}

func TestEmitOptionalParameterForwardsLocalCompilesAndRuns(t *testing.T) {
	// An already-declared optional-typed local passed as the argument — the
	// SymbolValue-forward shape.
	emitAndRun(t, "fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { var n ?int = none; return g(n); }", false, 0, false)
}

func TestEmitOptionalParameterForwardsCallResultCompilesAndRuns(t *testing.T) {
	// The result of another optional-returning call passed directly as the
	// argument — the DirectCall-forward shape.
	emitAndRun(t, "fn f() ?int { return 5; } fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { return g(f()); }", false, 1, false)
}

func TestEmitOptionalParameterStructPayloadCompilesAndRuns(t *testing.T) {
	// A tuple/struct-payload optional parameter, not just an integer payload
	// — confirms the payload-type dispatch isn't hardcoded to scalars. Bool
	// implicit injection is checker-rejected (T0505, matching the identical
	// limitation on the return-position work), so struct/tuple payloads use
	// explicit `some`.
	emitAndRun(t, "type P = struct { x int; y int; };\nfn g(o ?P) int { if o.has_value { return 1; } return 0; } fn main() int { return g(some P.{ x = 1, y = 2 }); }", false, 1, false)
}

func TestEmitOptionalParameterTuplePayloadCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn g(o ?(int, int)) int { if o.has_value { return 1; } return 0; } fn main() int { return g(some (1, 2)); }", false, 1, false)
}

func TestEmitOptionalResultTestsStillPass(t *testing.T) {
	// Regression guard: the optional-RESULT machinery (landed earlier this
	// session) is unaffected by adding parameter support — the shared
	// buildOptionalValue (generalized from buildOptionalReturnValue to serve
	// both the return and call-argument positions) still returns the exact
	// same C for a return-position optional value it did before
	// generalization.
	emitAndRun(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
}

func TestEmitNoneOptionalOfConstructedStructCompilesAndRuns(t *testing.T) {
	// Found during a real-code audit: isEnumType (the function distinguishing
	// a Nominal type's struct-vs-enum classification) used to guess from
	// FieldPlace/RecordConstruct usage evidence and defaulted to "enum" when
	// it found none — wrong for `var o ?P = none;`, since a `none` literal
	// never actually constructs its payload type, so the struct P was
	// misclassified as an enum and the optional's .value field referenced an
	// enum typedef that was never emitted. Fixed by reading each member's
	// own declaration-node kind directly (tir.FieldDeclaration vs
	// tir.VariantDeclaration, unconditional evidence, not a usage-evidence
	// guess). This also required two supporting fixes: collectStructTypes
	// now also collects a struct type reached only via an optional's payload
	// (mirroring the existing Parameters/ResultType scans), and a
	// NoneOptional's irrelevant .value field now uses the correct C
	// zero-initializer shape for its payload type ({0} for a struct/tuple,
	// which a bare 0 doesn't satisfy under -Wmissing-field-initializers /
	// -Wmissing-braces with -Werror; 0 for a scalar/enum payload, unchanged).
	// P here has real field evidence elsewhere in the program (constructed
	// by make()) — the deeper, separate case of a struct with ZERO field
	// evidence anywhere (declared, never constructed, ONLY ever named as a
	// none optional's payload) is still blocked by a different, narrower gap
	// (field-type resolution has no fallback when no FieldPlace/
	// RecordConstruct evidence exists anywhere in the whole program) —
	// tracked separately, not fixed here.
	emitAndRun(t, "type P = struct { x int; y int; };\nfn make() P { return P.{ x = 1, y = 2 }; }\nfn main() int {\nvar p P = make();\nvar o ?P = none;\nif o.has_value { return 1; }\nreturn p.x;\n}", false, 1, false)
}

func TestEmitNoneOptionalOfUnusedStructCompilesAndRuns(t *testing.T) {
	// P is only named as the payload of an absent optional. Its fields have no
	// FieldPlace or RecordConstruct usage evidence, so this exercises the
	// declaration-level member type carried by TypeDecl.
	emitAndRun(t, "type P = struct { x int; y int; }; fn main() int { var o ?P = none; if o.has_value { return 1; } return 0; }", false, 0, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	// in a PURE EXPRESSION position — here a return value, `return f(a[1:3]);`
	// — is confirmed checker-reachable (the DirectCall's child is a bare
	// CheckedSlice) but has nowhere to place the temp-declaration statement the
	// construction needs: a C function argument is a pure expression position,
	// so this remains a clean rejection naming what was found — not a GNU
	// statement-expression workaround. The leading-statement positions (a bare
	// call statement or a local's declaration initializer) now DO support it
	// (see TestEmitSliceConstructionAsCallArgumentCompilesAndRuns); this test
	// pins the remaining expression-position boundary.
	unit, snapshot, entryID, _ := buildFixture(t, "fn f(x []i32) i32 { return x[0]; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; return f(a[1:3]); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "inline slice construction")
}

func TestEmitSliceConstructionAsCallArgumentCompilesAndRuns(t *testing.T) {
	// The leading-statement-argument slice: an inline slice construction passed
	// directly as a call argument works when the CALL ITSELF is in a
	// leading-statement position, because that position has a natural place for
	// the temp-declaration statement the construction needs (the same pre-
	// threading buildScalarInitializeCore demonstrates). This is the exact shape
	// examples/prime_sieve.peb needs (sieve(primes[:], limit)) — previously
	// worked around there by binding the slice to a local first. The fixture
	// exercises BOTH supported positions: a bare void call statement
	// (mark(b[2:4])) and a scalar local's declaration initializer
	// (let r = first(a[1:3])). a[1] is 20, so the exit code 20 (returned only
	// when the mark() call actually set b[2] through the bool slice) proves
	// both slices were constructed and passed correctly at runtime, not just
	// emitted.
	emitAndRun(t, `fn first(x []int) int { return x[0]; }
fn mark(x []bool) void { x[0] = true; }
fn main() int {
    var a [5]int = [10, 20, 30, 40, 50];
    var b [5]bool = [false; 5];
    mark(b[2:4]);
    let r = first(a[1:3]);
    if b[2] {
        return r;
    } else {
        return 0;
    }
}`, false, 20, false)
}

func TestEmitSliceConstructionAsCallArgumentEmitsTempStatement(t *testing.T) {
	// The emitted C for the inline-construction call statement: the slice
	// argument is the two-statement temp-then-construction shape a slice local's
	// declaration and the return side already use — a pebble_slice_arg_<nodeID>
	// temp declaration holding the checked slice-start result, emitted as a
	// statement line directly before the call, then the call whose argument is
	// the slice compound literal using that temp for both .data and .len.
	// b[2:4] over a 5-element bool array: checked start (2, 4, 5), .data offset
	// 2, .len 2.
	unit, snapshot, entryID, sources := buildFixture(t, `fn mark(x []bool) void { x[0] = true; }
fn main() int {
    var b [5]bool = [false; 5];
    mark(b[2:4]);
    return 0;
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_slice_arg_",
		"pebble_rt_checked_slice_start_i32(2, 4, 5, (PebbleSourceLoc){\"main.peb\"",
		"pebble_local_",
		".len = (size_t)(4 - pebble_slice_arg_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if !strings.Contains(out, ".data = pebble_local_") {
		t.Errorf("emitted C missing slice compound literal .data offset:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitIntegerCastRoundTripCompilesAndRuns(t *testing.T) {
	// The intermediate i64 cast has a different width from the i32 entry, but
	// the outer cast returns to the entry width.
	// The test runner observes the process exit code, so reduce the result to
	// its low byte after the cast (300 exits as 44 on Unix either way).
	emitAndRunBounded(t, "fn main() i32 { var n i32 = 0; var done i32 = 0; while done == 0 { var x i32 = 300; n = (x as i64) as i32; done = 1; } return n % 256; }", false, 44, false)
}

func TestEmitIntegerCastTruncatesCompilesAndRuns(t *testing.T) {
	// 4294967297 narrowed to i32 wraps to 1, matching the fixed-width C cast.
	emitAndRunBounded(t, "fn main() i32 { var n i32 = 0; var done i32 = 0; while done == 0 { n = (4294967297 as i64) as i32; done = 1; } return n; }", false, 1, false)
}

func TestEmitIntegerCastUnsignedRoundTripCompilesAndRuns(t *testing.T) {
	// Exercise a differently-signed intermediate type rather than only i32/i64.
	emitAndRunBounded(t, "fn main() i32 { var n i32 = 0; var done i32 = 0; while done == 0 { var x i32 = 300; n = (x as u32) as i32; done = 1; } return n % 256; }", false, 44, false)
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
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
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

func TestEmitVariadicCallSumsCollectedSliceCompilesAndRuns(t *testing.T) {
	// The exact minimal repro: fn sum(...values []int) int { return
	// values[0] + values[1] + values[2]; } fn main() int { return sum(10,
	// 20, 30); } Confirms the collected variadic slice has both the right
	// length and the right values, by actually summing them (not just
	// reading .len), proving buildVariadicSliceArgument's compound-literal
	// array is populated correctly, not merely present.
	emitAndRun(t, "fn sum(...values []int) int { return values[0] + values[1] + values[2]; } fn main() int { return sum(10, 20, 30); }", false, 60, false)
}

func TestEmitVariadicCallLenOnlyCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn sum(...values []int) int { return values.len as int; } fn main() int { return sum(1, 2, 3); }", false, 3, false)
}

func TestEmitVariadicCallFixedPlusVariadicCompilesAndRuns(t *testing.T) {
	// Confirms the fixed/variadic split is correct in real emitted C: the
	// fixed `prefix` parameter and the collected `values` slice must both
	// carry their own, distinct values. 5*10 + values.len(3) = 53 — kept
	// well under 256 deliberately, since a Unix process exit code truncates
	// to its low 8 bits (5*1000+3 = 5003, and 5003 mod 256 = 139, which is
	// indistinguishable from SIGSEGV's exit-code convention purely by
	// coincidence — this exact confusion cost real investigation time
	// during this feature's development, so every variadic test here keeps
	// its expected exit code under 256 on purpose).
	emitAndRun(t, "fn tagged(prefix int, ...values []int) int { return prefix * 10 + (values.len as int); } fn main() int { return tagged(5, 1, 2, 3); }", false, 53, false)
}

func TestEmitVariadicCallZeroArgumentsCompilesAndRuns(t *testing.T) {
	// No variadic arguments at all: the collected slice must be the
	// established empty-slice shape (.data = NULL, .len = 0), not an
	// invalid empty array compound literal (a GNU extension, not portable
	// C99/C11).
	emitAndRun(t, "fn count(...values []int) int { return values.len as int; } fn main() int { return count(); }", false, 0, false)
}

func TestEmitVariadicCallBoolElementCompilesAndRuns(t *testing.T) {
	// Confirms the variadic element dispatch isn't hardcoded to int: a
	// []bool trailing parameter, collected via buildBoolExpr per element.
	emitAndRun(t, "fn allTrue(...values []bool) int { if values[0] && values[1] { return 1; } return 0; } fn main() int { return allTrue(true, true); }", false, 1, false)
}

func TestEmitVariadicCallBoolElementFalseCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn allTrue(...values []bool) int { if values[0] && values[1] { return 1; } return 0; } fn main() int { return allTrue(true, false); }", false, 0, false)
}

// --- Function types: slice 1/3, function-typed locals, function values, and
// general indirect calls (struct fields, parameters, and results are later
// slices) ---

func TestEmitFunctionTypedLocalCompilesAndRuns(t *testing.T) {
	// The exact minimal repro: a function-typed local initialized from a bare
	// top-level function reference (a HoistedFunctionValue), called through
	// an indirect call. add's own C name (pebble_fn_<symbol>) decays to a
	// function pointer matching the local's pebble_fnptr_<typeID>_t typedef,
	// no cast needed.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn main() int { var f fn(int, int) int = add; return f(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedLocalReassignmentCompilesAndRuns(t *testing.T) {
	// Reassigning a function-typed local to a different function of the same
	// signature (f = sub;) — the buildStoreCore functionType branch.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn sub(a int, b int) int { return a - b; } fn main() int { var f fn(int, int) int = add; f = sub; return f(5, 2); }", false, 3, false)
}

func TestEmitFunctionTypedLocalBoolSignatureCompilesAndRuns(t *testing.T) {
	// A bool-parameter/bool-result function type — confirms the
	// parameter/result C-type dispatch isn't hardcoded to int. This also
	// exercises the bool-returning-helper support added as a genuine
	// prerequisite (validateHelperSignature previously rejected any
	// bool-result helper outright, which would have made `id` itself
	// unemittable).
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { var f fn(bool) bool = id; if f(true) { return 1; } else { return 2; } }", false, 1, false)
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { var f fn(bool) bool = id; if f(false) { return 1; } else { return 2; } }", false, 2, false)
}

func TestEmitBoolReturningDirectCallInIfConditionCompilesAndRuns(t *testing.T) {
	// The exact repro: a plain bool-returning helper called directly in a bool
	// position. validateHelperSignature admits a bool result, so the if
	// condition lowers to a bool-typed DirectCall, which buildBoolExpr now
	// builds through buildDirectCall. The branch taken proves the call's C
	// bool result drives the condition correctly.
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { if id(true) { return 1; } else { return 2; } }", true, 1, false)
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { if id(false) { return 1; } else { return 2; } }", true, 2, false)
}

func TestEmitBoolReturningDirectCallInWhileConditionCompilesAndRuns(t *testing.T) {
	// The same direct call used as a while-loop condition: `while id(n < 2)`
	// calls id with a comparison argument, so the condition is a DirectCall
	// whose argument is itself a bool value built by the same builder. The
	// loop must run exactly twice, proving the call result keeps the loop
	// going and then stops it.
	emitAndRunBounded(t, "fn id(b bool) bool { return b; } fn main() int { var n int = 0; while id(n < 2) { n = n + 1; } return n; }", true, 2, false)
}

func TestEmitBoolReturningDirectCallWithShortCircuitCompilesAndRuns(t *testing.T) {
	// A bool-returning call composed with && / || / ! in the same condition:
	// the new DirectCall case must compose with the existing operand tree.
	// The && cases prove short-circuit composition (the false-left case skips
	// the right operand exactly as Pebble would), and the || / ! cases prove
	// the call also works on the right of an operator and under negation.
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { var flag bool = true; if id(true) && flag { return 1; } else { return 2; } }", true, 1, false)
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { var flag bool = true; if id(false) && flag { return 1; } else { return 2; } }", true, 2, false)
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { var flag bool = false; if id(true) || flag { return 1; } else { return 2; } }", true, 1, false)
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { if !id(false) { return 1; } else { return 2; } }", true, 1, false)
}

func TestEmitBoolReturningMethodCallInConditionCompilesAndRuns(t *testing.T) {
	// A bool-returning method call in a bool position lowers to a bool-typed
	// MethodCall (the receiver becomes the self parameter through
	// buildCallArguments), which buildBoolExpr builds through buildDirectCall
	// just like the DirectCall case. Both the true and false branch results
	// confirm the method result drives the condition.
	emitAndRun(t, "type Box = struct { value bool; fn isTrue(self Box) bool => self.value; }; fn main() int { let box Box = Box.{ value = true }; if box.isTrue() { return 1; } else { return 2; } }", true, 1, false)
	emitAndRun(t, "type Box = struct { value bool; fn isTrue(self Box) bool => self.value; }; fn main() int { let box Box = Box.{ value = false }; if box.isTrue() { return 1; } else { return 2; } }", true, 2, false)
}

func TestEmitFunctionTypedLocalWritesC(t *testing.T) {
	// Confirm the emitted C directly: the typedef shape
	// (typedef <ret> (*pebble_fnptr_<id>_t)(PebbleContext *ctx, ...);), the
	// function value assigned bare (no cast) at the declaration site, and the
	// indirect call threading ctx as the first argument.
	unit, snapshot, entryID, sources := buildFixture(t, "fn add(a int, b int) int { return a + b; } fn main() int { var f fn(int, int) int = add; return f(1, 2); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"(*pebble_fnptr_",
		")(PebbleContext *ctx, int32_t, int32_t);",
		"pebble_fnptr_",
		"= pebble_fn_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "(pebble_fnptr_") && strings.Contains(out, ")pebble_fn_") {
		t.Errorf("function value declaration should not be cast, found a cast form:\n%s", out)
	}
}

func TestEmitFunctionTypedLocalDoesNotRegressAllocator(t *testing.T) {
	// The general indirect-call path (buildFunctionIndirectCall) must not
	// interfere with the pre-existing allocator-specific indirect call
	// (context.default_allocator.alloc(...) and friends), which shares the
	// same tir.IndirectCall node kind but is detected and handled by a wholly
	// separate branch (allocatorCallee). Regression-covered already by this
	// file's existing TestEmitRuntimeAllocator* tests (run as part of the
	// full suite); this test adds one more real allocation round trip
	// alongside a function-typed local in the same program, confirming the
	// two mechanisms coexist correctly.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn main() int {\nvar f fn(int, int) int = add;\nlet allocator = context.default_allocator;\nlet p *int = allocator.alloc(allocator.ptr, sizeof int) as *int;\n*p = f(3, 4);\nreturn *p;\n}", false, 7, false)
}

// --- Function types: slice 2/3, function-typed struct fields ---

func TestEmitFunctionTypedStructFieldCallCompilesAndRuns(t *testing.T) {
	// The exact minimal repro: constructing a struct with a function-typed
	// field (Table.{ op = add }) and calling directly through the field
	// (t.op(1, 2)), no intermediate local. The field read reaches
	// buildFunctionValue as a bare FieldValue (the indirect call's direct
	// -callee shape), distinct from the Load(FieldPlace)-wrapped shape a
	// non-callee position uses (see the next test).
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int { var t Table = Table.{ op = add }; return t.op(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedStructFieldViaLocalCompilesAndRuns(t *testing.T) {
	// Reading a function-typed field into a local first, then calling
	// through the local — confirms the field-read value forwards correctly
	// into buildFunctionLocalDeclaration (slice 1) unchanged, and exercises
	// the Load(FieldPlace)-wrapped field-read shape a local-declaration
	// initializer position produces (distinct from the direct-callee
	// FieldValue shape the previous test uses).
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int { var t Table = Table.{ op = add }; var f fn(int, int) int = t.op; return f(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedStructFieldNeverReadCompilesAndRuns(t *testing.T) {
	// A function-typed field that is constructed but never read back by
	// name anywhere else — confirms the typedef-collection and
	// reachability-walk fixes (RecordConstruct.Fields isn't part of
	// node.Children, so both walks need an explicit case to find a
	// HoistedFunctionValue used only as a field's construction value)
	// correctly discover the referenced function and its typedef even
	// though nothing calls through the field at all.
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int { var t Table = Table.{ op = add }; return 5; }", false, 5, false)
}

func TestEmitFunctionTypedStructFieldWritesC(t *testing.T) {
	// Confirm the emitted C directly: the function typedef appears BEFORE
	// the struct typedef that names it as a field's C type (slice 2
	// reverses slice 1's "function typedefs are self-contained, append
	// last" assumption, since a struct field can now reference a function
	// typedef), and the field value is assigned bare (no cast).
	unit, snapshot, entryID, sources := buildFixture(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int { var t Table = Table.{ op = add }; return t.op(1, 2); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	fnptrIndex := strings.Index(out, "typedef int32_t (*pebble_fnptr_")
	structIndex := strings.Index(out, "typedef struct {\n    pebble_fnptr_")
	if fnptrIndex < 0 || structIndex < 0 || fnptrIndex > structIndex {
		t.Errorf("function typedef does not precede the struct typedef that names it as a field (definition before use):\n%s", out)
	}
	if !strings.Contains(out, ".pebble_field_") || !strings.Contains(out, "= pebble_fn_") {
		t.Errorf("emitted C missing expected field-initializer shape:\n%s", out)
	}
}

func TestEmitFunctionTypedStructFieldDoesNotRegressAllocator(t *testing.T) {
	// The exact collision this slice was built to avoid: a function-typed
	// struct field read (t.op) produces the same FieldValue TIR node kind a
	// real allocator field access does. This program exercises both in the
	// same run, confirming indirectCalleePlace's runtime-field-identity
	// check (hardened in 50b3970, reused unchanged here) still correctly
	// tells them apart.
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int {\nvar t Table = Table.{ op = add };\nlet allocator = context.default_allocator;\nlet p *int = allocator.alloc(allocator.ptr, sizeof int) as *int;\n*p = t.op(3, 4);\nreturn *p;\n}", false, 7, false)
}

// --- Function types: slice 3/3, function-typed parameters and results ---

func TestEmitFunctionTypedParameterCompilesAndRuns(t *testing.T) {
	// The exact minimal parameter repro: apply(f fn(int,int)int, x int, y
	// int) calls through the function-typed parameter.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int { return apply(add, 1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedResultCompilesAndRuns(t *testing.T) {
	// The exact minimal result repro: chooseOp() fn(int,int)int returns a
	// bare function reference, forwarded into a local and called through it.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn chooseOp() fn(int, int) int { return add; } fn main() int { var f fn(int, int) int = chooseOp(); return f(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedParameterMixedSignatureCompilesAndRuns(t *testing.T) {
	// A function-typed parameter combined with other parameter types and
	// positions in the same signature — confirms the dispatch works
	// regardless of where in the parameter list the function-typed one sits.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn apply(x int, f fn(int, int) int, y int) int { return f(x, y); } fn main() int { return apply(10, add, 20); }", false, 30, false)
}

func TestEmitFunctionTypedParameterStructFieldArgumentCompilesAndRuns(t *testing.T) {
	// Passing a function-typed STRUCT FIELD as a call argument — combines
	// slice 2 (struct fields) and slice 3 (parameters).
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int { var t Table = Table.{ op = add }; return apply(t.op, 1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedResultChainedCallCompilesAndRuns(t *testing.T) {
	// A function-returning helper's result forwarded directly as another
	// call's argument, and a function-returning helper calling another
	// function-returning helper — both real DirectCall-as-function-value
	// shapes, not just the caller-side local-declaration read-back.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn chooseOp() fn(int, int) int { return add; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int { return apply(chooseOp(), 1, 2); }", false, 3, false)
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn chooseOp() fn(int, int) int { return add; } fn wrap() fn(int, int) int { return chooseOp(); } fn main() int { var f fn(int, int) int = wrap(); return f(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedGenericValueCompilesAndRuns(t *testing.T) {
	// A generic function referenced as a first-class value
	// (GenericFunctionValue, confirmed checker-reachable via identity[int]),
	// both as a local's initializer and as a call argument.
	emitAndRun(t, "fn identity[T](x T) T { return x; } fn main() int { var f fn(int) int = identity[int]; return f(3); }", false, 3, false)
	emitAndRun(t, "fn identity[T](x T) T { return x; } fn apply(f fn(int) int) int { return f(5); } fn main() int { return apply(identity[int]); }", false, 5, false)
}

func TestEmitFunctionTypedParameterResultDoesNotRegressAllocator(t *testing.T) {
	// The same allocator-collision class slices 1 and 2 each guarded against,
	// exercised for parameters and results specifically.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int {\nlet allocator = context.default_allocator;\nlet p *int = allocator.alloc(allocator.ptr, sizeof int) as *int;\n*p = apply(add, 3, 4);\nreturn *p;\n}", false, 7, false)
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn chooseOp() fn(int, int) int { return add; } fn main() int {\nlet allocator = context.default_allocator;\nlet p *int = allocator.alloc(allocator.ptr, sizeof int) as *int;\nvar f fn(int, int) int = chooseOp();\n*p = f(3, 4);\nreturn *p;\n}", false, 7, false)
}

func TestEmitFunctionTypedParameterResultWritesC(t *testing.T) {
	// Confirm the emitted C directly: the function-typed parameter's C type
	// is the fnptr typedef, the argument is passed bare, and the indirect
	// call through the parameter threads ctx.
	unit, snapshot, entryID, sources := buildFixture(t, "fn add(a int, b int) int { return a + b; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int { return apply(add, 1, 2); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{"pebble_fnptr_", "pebble_local_", "(ctx, pebble_fn_"} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

// --- Function types: u64 parameter/result support ---

func TestEmitU64FunctionTypeResultCompilesAndRuns(t *testing.T) {
	// u64 as a function type's RESULT (the exact shape std/hmap.peb's
	// `hash_fn fn (K) u64` field uses): a function-typed local whose signature
	// returns u64, an indirect call through it, and the u64 result consumed as
	// a plain u64 rvalue. A prior attempt found validateFunctionTypeSignature's
	// result gate rejected a u64 result because isUint only admits the distinct
	// `uint` builtin, never U64 — so no type whose signature returned u64 could
	// even be declared. This confirms the u64 result flows end to end.
	emitAndRun(t, "fn hashOf(x int) u64 { return x as u64; } fn main() int { var f fn(int) u64 = hashOf; var r u64 = f(5); return r as int; }", false, 5, false)
}

func TestEmitU64FunctionTypeParamCompilesAndRuns(t *testing.T) {
	// The mirror: u64 as a function type's PARAMETER, used as a function-typed
	// local and called with a real u64 value. The param side had the identical
	// isUint-excludes-U64 bug as the result side, so this must be fixed for
	// symmetry, not just the result.
	emitAndRun(t, "fn udToInt(x u64) int { return x as int; } fn main() int { var v u64 = 7; var f fn(u64) int = udToInt; return f(v); }", false, 7, false)
	// And as a function-typed HELPER PARAMETER, called with a u64 argument.
	emitAndRun(t, "fn udToInt(x u64) int { return x as int; } fn apply(f fn(u64) int, x u64) int { return f(x); } fn main() int { var v u64 = 9; return apply(udToInt, v); }", false, 9, false)
}

func TestEmitU64FunctionTypeBothCompilesAndRuns(t *testing.T) {
	// A function type whose signature mentions u64 in BOTH the parameter and
	// the result, called round-trip through a u64 local.
	emitAndRun(t, "fn id(x u64) u64 { return x; } fn main() int { var f fn(u64) u64 = id; var r u64 = f(5); return r as int; }", false, 5, false)
}

func TestEmitU64FunctionTypeStructFieldCompilesAndRuns(t *testing.T) {
	// The motivating real-code shape, concretized without a generic struct:
	// a function-typed STRUCT FIELD whose signature returns u64 (hmap's
	// `hash_fn fn (K) u64`) and a separate struct field whose signature takes
	// u64, both called through the field. This is the position std/hmap.peb's
	// hash_fn field occupies.
	emitAndRun(t, "type Table = struct { hash fn(int) u64; }; fn hashOf(x int) u64 { return x as u64; } fn main() int { var t Table = Table.{ hash = hashOf }; var h u64 = t.hash(5); return h as int; }", false, 5, false)
	emitAndRun(t, "type Conv = struct { toi fn(u64) int; }; fn udToInt(x u64) int { return x as int; } fn main() int { var v u64 = 6; var c Conv = Conv.{ toi = udToInt }; return c.toi(v); }", false, 6, false)
}

func TestEmitU64FunctionTypeHelperParameterAndResultCompilesAndRuns(t *testing.T) {
	// A u64 function-type PARAMETER and a u64 function-type RESULT in ordinary
	// (non-function-typed) helper positions: a helper taking a fn(int) u64
	// parameter calls through it and returns the u64 result, and a helper
	// returning fn(u64) int is forwardable. This exercises the u64 rows of
	// validateHelperSignature / buildHelperFunctions / buildReturnStatement.
	emitAndRun(t, "fn hashOf(x int) u64 { return x as u64; } fn callHash(f fn(int) u64, x int) u64 { return f(x); } fn main() int { var r u64 = callHash(hashOf, 8); return r as int; }", false, 8, false)
	// A helper whose RESULT is a function type whose PARAMETER is u64.
	emitAndRun(t, "fn udToInt(x u64) int { return x as int; } fn choose() fn(u64) int { return udToInt; } fn main() int { var f fn(u64) int = choose(); var v u64 = 3; return f(v); }", false, 3, false)
}

func TestEmitU64FunctionTypeWritesC(t *testing.T) {
	// Confirm the emitted C directly: the function type whose RESULT is u64 is
	// typedef'd with a uint64_t return type (`uint64_t (*pebble_fnptr_<id>_t)`),
	// the function type whose PARAMETER is u64 declares that parameter as
	// uint64_t, and a u64-typed helper parameter/result is declared as uint64_t
	// too — a uint64_t, not an unsupported rejection.
	src := "fn hashOf(x int) u64 { return x as u64; } fn udToInt(x u64) int { return x as int; } fn main() int { var f fn(int) u64 = hashOf; var g fn(u64) int = udToInt; var r u64 = f(5); return r as int; }"
	unit, snapshot, entryID, sources := buildFixture(t, src, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef uint64_t (*pebble_fnptr_", // fn(int) u64 result
		", uint64_t);",                     // fn(u64) int parameter slot
		"uint64_t pebble_local_",           // the u64-typed helper parameter
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitU64FunctionTypeResultConsumedAsRvalueCompilesAndRuns(t *testing.T) {
	// A u64 function-type INDIRECT CALL result consumed as a plain rvalue in a
	// NON-local position (not just a local-declaration initializer): forwarded
	// directly as another expression's integer operand. This is the seat the
	// original tracker framed as the "results" gap — the indirect call's result
	// must be built at the u64 width even though the enclosing statement is
	// entry-width.
	emitAndRun(t, "fn hashOf(x int) u64 { return x as u64; } fn main() int { var f fn(int) u64 = hashOf; return (f(5) as int) + 1; }", false, 6, false)
}

func TestCheckStdHmapU64HashFnTypes(t *testing.T) {
	// The real motivating module: std/hmap.peb declares `hash_fn fn (K) u64`
	// and `fn new[K, V](hash_fn fn (K) u64, eq_fn fn (K, K) bool) HashMap[K, V]`
	// — u64 in function-type parameter and RESULT positions (line 19 and 191 in
	// the module source). The checker must type-accept the module with no
	// diagnostics: this confirms the u64 function-type result/parameter shapes
	// the hardware accepts and that the feature's motivating real-code type has
	// always been well-typed. Reading the real module files (not a fixture
	// string) mirrors TestCheckStdVecHasNoGenericPointerReceiverShapeErrors.
	//
	// (Emit of a full hmap consumer — hmap::new[int, int], insert, get — is
	// blocked not by u64 function types but by pre-existing, out-of-scope
	// generic-struct gaps: HashMap's `key K` / `value V` type-parameter fields,
	// its uint/slice/runtime-Allocator fields, and the generic method calls
	// insert/get, all rejected by the backend's generic-struct/method
	// supporters separately. The u64 `hash_fn fn (K) u64` and
	// `fn new[K, V](..., hash_fn fn (K) u64, ...)` types themselves check clean
	// and their shapes emit-and-run in every function-type position the
	// TestEmitU64FunctionType* tests above exercise concretely.)
	hmap, err := os.ReadFile("../../std/hmap.peb")
	if err != nil {
		t.Fatal(err)
	}
	mem, err := os.ReadFile("../../std/mem.peb")
	if err != nil {
		t.Fatal(err)
	}
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb":     []byte(`import "std:hmap"; fn userHash(x int) u64 => x as u64; fn userEq(a int, b int) bool => a == b; fn main() int { var m = hmap::new[int, int](userHash, userEq); return 0; }`),
		"std/hmap.peb": hmap,
		"std/mem.peb":  mem,
	}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed on std:hmap with u64 hash_fn: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("check succeeded without an IR unit")
	}
}

func TestCheckStdHmapRehash(t *testing.T) {
	// The C0619 regression this test pins: calling std/hmap.peb's generic
	// `rehash` method — an indexed slice-element FIELD WRITE
	// (`self.entries[i].state = .Empty`) inside a method of a generic struct
	// declared in a separate std module — failed with an opaque
	// "typed-IR construction failed" internal error. The root cause was that
	// member/function-type KEY lookups read the frozen inference snapshot while
	// the type being resolved had already been concretely substituted through
	// the active specialization mapping into the live type store; the concrete
	// Entry[int, int] / HashMap[int, int] types interned mid-build are absent
	// from the freeze-time snapshot, so the key lookup failed, member symbols
	// were left zeroed, and a FieldPlace with no Member reached the verifier.
	//
	// This is exactly the shape bisection isolated: the identical construct
	// checks clean when the struct is declared in main.peb and only fails when
	// the generic struct lives in an imported std module, because only then
	// does the specialization build substitute concrete types into the live
	// store after the snapshot was taken.
	hmap, err := os.ReadFile("../../std/hmap.peb")
	if err != nil {
		t.Fatal(err)
	}
	mem, err := os.ReadFile("../../std/mem.peb")
	if err != nil {
		t.Fatal(err)
	}
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb":     []byte(`import "std:hmap"; fn userHash(x int) u64 => x as u64; fn userEq(a int, b int) bool => a == b; fn main() int { var m = hmap::new[int, int](userHash, userEq); m.rehash(8); return 0; }`),
		"std/hmap.peb": hmap,
		"std/mem.peb":  mem,
	}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed on std:hmap rehash: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("check succeeded without an IR unit")
	}
}

func TestCheckStdModuleGenericMethodIndexedFieldWrite(t *testing.T) {
	// The minimal cross-module reproduction bisection isolated: a generic
	// struct in an imported std module whose method performs an indexed
	// slice-element field write. Before the store/snapshot fix this checked
	// clean when the same shape was declared in main.peb but failed C0619 the
	// moment the struct was moved into a std module, because only the
	// cross-module specialization build interns substituted concrete types
	// after the inference snapshot. `hmap::new` is kept in the module so the
	// call from main matches the real fixture layout.
	mem, err := os.ReadFile("../../std/mem.peb")
	if err != nil {
		t.Fatal(err)
	}
	hmap := `type EntryState = enum { Empty, Tombstone, Occupied };
type Entry[K, V] = struct { key K; value V; state EntryState; };
type HashMap[K, V] = struct { entries []Entry[K, V];
    fn rehash[K, V](self *HashMap[K, V], new_cap uint) void {
        loop 0..new_cap : i {
            self.entries[i].state = .Empty;
        }
    }
};
fn new[K, V](hash_fn fn (K) u64, eq_fn fn (K, K) bool) HashMap[K, V] {
    var arr [0]Entry[K, V] = [];
    var s []Entry[K, V] = arr[:];
    return HashMap[K, V].{ entries = s };
}`
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb":     []byte(`import "std:hmap"; fn userHash(x int) u64 => x as u64; fn userEq(a int, b int) bool => a == b; fn main() int { var m = hmap::new[int, int](userHash, userEq); m.rehash(8); return 0; }`),
		"std/hmap.peb": []byte(hmap),
		"std/mem.peb":  mem,
	}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed on minimal std-module generic indexed field write: %+v", diagnostics.Items())
	}
}

// TestEmitClosureLiteralArrowShorthand verifies that an anonymous closure literal
// using => expr arrow shorthand compiles and runs correctly when passed as a call
// argument. This is the end-to-end regression for the C0607 false-positive fix
// where the checker previously rejected fn (a, b str) bool => a == b as a
// "non-void function can fall through without returning".
func TestEmitClosureLiteralArrowShorthand(t *testing.T) {
	emitAndRun(t, `
fn call_it(f fn (str, str) bool) bool {
    return f("a", "a");
}
fn main() int {
    var r = call_it(fn (a, b str) bool => a == b);
    if r { return 1; }
    return 0;
}
`, true, 1, false)
}

// TestEmitClosureLiteralArrowShorthandFalse compares two strings and returns
// false, verifying the closure's expression body evaluates correctly.
func TestEmitClosureLiteralArrowShorthandFalse(t *testing.T) {
	emitAndRun(t, `
fn call_it(f fn (str, str) bool) bool {
    return f("a", "b");
}
fn main() int {
    var r = call_it(fn (a, b str) bool => a == b);
    if r { return 0; }
    return 1;
}
`, true, 1, false)
}
