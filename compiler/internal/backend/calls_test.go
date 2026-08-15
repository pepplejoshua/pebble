package backend

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestCheckStdVecHasNoGenericPointerReceiverShapeErrors(t *testing.T) {
	t.Parallel()
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

func TestEmitExternCallNoArgumentsCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	if !strings.Contains(emitted, "malloc(sizeof(int64_t))") {
		t.Errorf("emitted C does not call the real C name malloc with sizeof(int64_t):\n%s", emitted)
	}
	if strings.Contains(emitted, "pebble_fn_") {
		t.Errorf("emitted C contains a pebble_fn_ helper for an extern, want none:\n%s", emitted)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitOpaqueExternTypeFileRoundTripCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An opaque extern type (`type FILE;`, no body — "this exists in C, I'm
	// not describing its layout") previously emitted as a synthesized
	// pebble_struct_<id>_t instead of its real C name, so every real libc
	// call taking or returning *FILE (fopen/fclose/fputs/fgetc) failed to
	// compile with "incompatible pointer types". This is the full write
	// -then-read-back round trip against a REAL file on disk: fopen(w),
	// fputs, fclose, fopen(r), fgetc, fclose, remove — each call must use
	// the genuine FILE * (not a bogus struct) and each str argument
	// (fopen's path/mode, fputs's string) must lower to const char * (not a
	// PebbleStr struct) to agree with the real libc signatures. Compiled
	// under -Wall -Wextra -Werror; the exit code encodes exactly which step
	// failed (0 = every step succeeded, including reading back the 'h' the
	// write step wrote), so a wrong exit code pinpoints the failure.
	path := filepath.Join(t.TempDir(), "pebble_opaque_extern_test.txt")
	source := fmt.Sprintf(`extern {
    type FILE;
    fn fopen(path str, mode str) *FILE;
    fn fclose(file *FILE) i32;
    fn fputs(s str, file *FILE) i32;
    fn fgetc(file *FILE) i32;
    fn remove(path str) i32;
}
fn main() int {
    var f = fopen(%q, "w");
    if f == nil { return 1; }
    var w = fputs("hello", f);
    if w < 0 { return 2; }
    var closed = fclose(f);
    if closed != 0 { return 3; }
    var g = fopen(%q, "r");
    if g == nil { return 4; }
    var c = fgetc(g);
    if c != 104 { return 5; }
    var closed2 = fclose(g);
    if closed2 != 0 { return 6; }
    var removed = remove(%q);
    if removed != 0 { return 7; }
    return 0;
}`, path, path, path)
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, source)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

// TestEmitGenericHelperConcreteWidthWritesInt32TParams asserts the emitted-C
// shape for the widened case, not just that Emit succeeds: the generic
// specialization identity[i32] must be declared AND defined as
// `int32_t pebble_fn_24_3(PebbleContext *ctx, int32_t pebble_local_26)` — the
// parameter and result declared at i32's OWN concrete width (int32_t, the C
// type the specialization's i32 type argument carries, independent of the
// int-declared entry — int and i32 share no C representation anymore) and the
// body returning it — and the int-declared entry must keep its plain-int
// pebble_user_main. The i32 result is widened back to the int return with an
// explicit cast. Symbols 24 (identity[i32]) and 26 (its
// parameter) come from the fixture's typed-IR construction, deterministic for
// this exact source.
func TestEmitGenericHelperConcreteWidthWritesInt32TParams(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, `fn identity[T](x T) T { return x; } fn main() int { var a i32 = 5; var r = identity(a); return r as int; }`, "main", false)
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

func TestEmitValueMethodCallReadsReceiverField(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Point = struct { x i32; fn get(self Point) i32 => self.x; }; fn main() i32 { let p Point = Point.{ x = 41 }; return p.get(); }`, false, 41, false)
}

func TestEmitStructCallResultAsMethodReceiverCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A struct-typed call result used directly as a METHOD RECEIVER —
	// `mk().get()` where mk() returns a struct and get is a value-receiver
	// method. The method call's receiver is argument 0 of the underlying
	// DirectCall-shaped call, so it routes through buildAggregateArgument
	// exactly as a plain struct-typed call argument does; its new
	// DirectCall/MethodCall case builds the inner call with
	// buildDirectCallNested and passes the call expression
	// (`pebble_fn_<mk>(ctx)`) as the receiver argument. 20 + 22 = 42 is the
	// process exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; fn get(self Point) i32 => self.x + self.y; };\nfn mk() Point { return Point.{ x = 20, y = 22 }; } fn main() i32 { return mk().get(); }", false, 42, false)
}

func TestEmitStructFieldReadAsMethodReceiverCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A struct-typed field read used directly as a METHOD RECEIVER —
	// `h.p.get()` where h.p is a struct-typed field. The receiver is argument
	// 0 of the method call, so it routes through buildAggregateArgument as a
	// Load(FieldPlace); its extended Load case lowers it via buildPlaceLValue
	// to the plain C member-access expression
	// `pebble_local_<sym>.pebble_field_<member>`, used directly as the
	// receiver argument. 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "type Inner = struct { x i32; y i32; fn get(self Inner) i32 => self.x + self.y; }; type Holder = struct { p Inner; };\nfn main() i32 { let h Holder = Holder.{ p = Inner.{ x = 20, y = 22 } }; return h.p.get(); }", false, 42, false)
}

func TestEmitQualifiedStaticMethodCallCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A method declared inside a nominal type body with no self parameter is a
	// static method, callable on the bare type name. Its lowering must be a
	// plain direct call to the method's own C function — no receiver argument
	// prepended — so the struct it constructs flows back out of origin() intact.
	emitAndRun(t, `type Point = struct { x i32; y i32; fn origin() Point { return Point.{ x = 40, y = 2 }; } }; fn main() i32 { let p Point = Point.origin(); return p.x + p.y; }`, false, 42, false)
}

func TestEmitQualifiedStaticMethodWithArgumentsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The static call's authored arguments must arrive at the method exactly as
	// written: origin(2) below multiplies its argument through to the exit code.
	emitAndRun(t, `type Point = struct { x i32; fn origin(scale i32) Point { return Point.{ x = scale }; } }; fn main() i32 { let p Point = Point.origin(42); return p.x; }`, false, 42, false)
}

func TestEmitIndirectlyReachedMethodCall(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Point = struct { x i32; fn get(self Point) i32 => self.x; }; fn read(p Point) i32 { return p.get(); } fn main() i32 { let p Point = Point.{ x = 42 }; return read(p); }`, false, 42, false)
}

func TestEmitMethodCallWithExplicitArgument(t *testing.T) {
	t.Parallel()
	// Was blocked until the call_validation.go fix (a real checker bug
	// compared a method call's argument count against its generic
	// type-argument count, wrongly rejecting any non-generic method call
	// with an argument beyond the receiver) — proves the receiver field and
	// the explicit argument both flow through correctly.
	emitAndRun(t, `type Point = struct { x i32; fn add(self Point, delta i32) i32 => self.x + delta; }; fn main() i32 { let p Point = Point.{ x = 40 }; return p.add(2); }`, false, 42, false)
}

func TestEmitPointerReceiverMethodCallCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Was rejected before raw pointers landed (a pointer receiver's self
	// parameter has no backend representation until then) — since the
	// pointer backend-lowering slice, a pointer receiver is just an ordinary
	// pointer-typed parameter, so this now compiles and runs correctly.
	emitAndRun(t, `type Point = struct { fn get(self *Point) i32 => 1; }; fn main() i32 { let p *Point = nil; return p.get(); }`, false, 1, false)
}

func TestEmitAutoReferencesValueForPointerReceiver(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type S = struct { n i32; fn set(self *S, value i32) void { self.n = value; } }; fn main() i32 { var s = S.{ n = 0 }; s.set(9); return s.n; }`, false, 9, false)
}

func TestEmitGenericPointerReceiverCallsSiblingMethod(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Vec[T] = struct { value i32; fn reserve(self *Vec[i32], amount i32) void { self.value = amount; } fn push(self *Vec[i32], value i32) void { self.reserve(value); } }; fn main() i32 { var v = Vec[i32].{ value = 0 }; v.push(7); return v.value; }`, false, 7, false)
}

func TestEmitGenericMethodCallCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// TWO specializations of the same generic struct calling the SAME method
	// in one program must each get their OWN method specialization: Box[int]'s
	// get returns 5 and Box[bool]'s get returns true. If the two method
	// specializations collided (shared a symbol/FunctionID), one read would
	// dispatch to the wrong width and the exit code would be wrong.
	emitAndRun(t, `type Box[K] = struct { value K; fn get[K](self Box[K]) K { return self.value; } }; fn main() int { var b Box[int] = Box[int].{ value = 5 }; var c Box[bool] = Box[bool].{ value = true }; if c.get() { return b.get(); } return 0; }`, false, 5, false)
}

func TestEmitGenericMethodExtraTypeParameterParametersCompileAndRun(t *testing.T) {
	t.Parallel()
	// A method taking parameters beyond self that also depend on the type
	// parameters (mirroring std/hmap.peb's insert(self, key K, value V)) must
	// resolve those parameter types end to end: put(4, 5) writes both through
	// the pointer receiver, and the returned key plus the stored value encode
	// 4 * 10 + 5 = 45 in the exit code.
	emitAndRun(t, `type Pair[K, V] = struct { key K; value V; fn put[K, V](self *Pair[K, V], k K, v V) K { self.key = k; self.value = v; return self.key; } }; fn main() int { var p Pair[int, int] = Pair[int, int].{ key = 1, value = 2 }; let got int = p.put(4, 5); return got * 10 + p.value; }`, false, 45, false)
}

func TestEmitGenericMethodPointerReceiverCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A pointer-receiver generic method on a generic struct: the receiver
	// value is auto-referenced for the `self *Box[K]` parameter, and the
	// method's own K resolves to the receiver's int.
	emitAndRun(t, `type Box[K] = struct { value K; fn get[K](self *Box[K]) K { return self.value; } }; fn main() int { var b Box[int] = Box[int].{ value = 7 }; return b.get(); }`, false, 7, false)
}

func TestEmitGenericStructMethodTypeParameterResultCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The active-defect repro (proposal 13): a NON-generic method on a
	// generic struct whose declared RETURN type is the struct's own type
	// parameter directly (`fn get(self Box[T]) T`). The checker emits one
	// symbolic FunctionDeclaration (result type = type-parameter TypeID)
	// shared by every instantiation, so the backend must substitute the
	// receiver's concrete type argument into the method's C signature AND
	// build its body against that concrete instantiation — the exact
	// mechanism that had substituted only FIELD types, never a method's own
	// parameter/return types. Before the fix Emit failed with "called
	// function symbol ... has result type type-parameter(...)"; now the
	// emitted helper returns int32_t and reads the Box[int] value field
	// through the substituted self type.
	emitAndRun(t, `type Box[T] = struct { value T; fn get(self Box[T]) T { return self.value; } }; fn main() int { let b = Box[int].{ value = 42 }; return b.get(); }`, false, 42, false)
}

func TestEmitGenericStructMethodTypeParameterParameterCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The active-defect repro's other half (proposal 13): a non-generic
	// method on a generic struct whose declared PARAMETER type is the
	// struct's own type parameter directly (`fn set(self *Box[T], v T)`).
	// The v parameter's T must be substituted to int in the C signature (so
	// the call site's literal passes at the right width) and the body's
	// `self.value = v` write must store it into the Box[int] field.
	emitAndRun(t, `type Box[T] = struct { value T; fn set(self *Box[T], v T) void { self.value = v; } }; fn main() int { var b = Box[int].{ value = 1 }; b.set(42); return b.value; }`, false, 42, false)
}

func TestEmitGenericStructMethodTypeParameterTwoSpecializationsCompileAndRun(t *testing.T) {
	t.Parallel()
	// TWO instantiations of the SAME non-generic method in one program:
	// Box[int].get must return int and Box[bool].get must return bool, and
	// each needs its OWN C helper (the two share one symbolic
	// FunctionDeclaration, so the backend's per-instantiation substitution
	// must emit a separate pebble_fn_<symbol>_<function>_<hash> for each).
	// Before the fix Emit rejected the shared declaration's symbolic
	// signature outright; a per-helper substitution keyed only by FunctionID
	// would have silently let the first instantiation's signature win for
	// both call sites.
	emitAndRun(t, `type Box[T] = struct { value T; fn get(self Box[T]) T { return self.value; } }; fn main() int { let b = Box[int].{ value = 5 }; let c = Box[bool].{ value = true }; if c.get() { return b.get(); } return 0; }`, false, 5, false)
}

func TestEmitGenericStructMethodTypeParameterResultEmitsConcreteSignature(t *testing.T) {
	t.Parallel()
	// The emitted C must carry the CONCRETE substituted signature: the
	// helper returns int64_t (never a type parameter), its self parameter is
	// the Box[int] struct typedef, and its body returns the concrete field
	// read — the shape that failed with an unsubstituted
	// "type-parameter(symbol ...)" result type before the fix.
	unit, snapshot, entryID, sources := buildFixture(t, `type Box[T] = struct { value T; fn get(self Box[T]) T { return self.value; } }; fn main() int { let b = Box[int].{ value = 42 }; return b.get(); }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "static int64_t pebble_fn_") {
		t.Fatalf("emitted C has no int64_t-returning helper:\n%s", out)
	}
	if strings.Contains(out, "type-parameter") {
		t.Errorf("emitted C still carries an unsubstituted type parameter:\n%s", out)
	}
	if !strings.Contains(out, "pebble_local_28.pebble_field_26") {
		t.Errorf("emitted C missing the concrete Box[int] field read:\n%s", out)
	}
}

func TestEmitCompoundLoweringGoesThroughCheckedHelper(t *testing.T) {
	t.Parallel()
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
	if !strings.Contains(out, "pebble_local_27 = pebble_rt_checked_add_i32(pebble_local_27, 1, (PebbleSourceLoc){\"main.peb\", 1, 32})") {
		t.Fatalf("emitted C does not combine through the checked helper:\n%s", out)
	}
	if strings.Contains(out, " += ") {
		t.Fatalf("emitted C uses a raw C += instead of the checked helper:\n%s", out)
	}
}

func TestEmitTerminalWhileTrueHelperCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The std/hmap and std/set helpers that motivated this slice are non-void
	// reachable helpers whose bodies end in an exhaustive `while true`, not the
	// entry itself — so the terminal-loop tail must be accepted by the same
	// buildBlock a helper body uses (the exact same builder, just called from
	// the helper path). f's body ends in the while and returns 7 on its first
	// pass; main forwards f() as its exit code.
	emitAndRunBounded(t, "fn f() i32 { while true { return 7; } } fn main() i32 { return f(); }", false, 7, false)
}

func TestEmitPrintInterpolatedBoolHelperCallCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A bool-returning helper call used as an interpolated print value —
	// `print \`big? {isBig(7)}\`;` — must still be discovered as reachable
	// and emitted as a pebble_fn_<symbolID> helper: the value parts of an
	// InterpolatedString are stored in Parts[].Value (not Children), so
	// collectDirectCalls recurses into them or the helper prototype/definition
	// is never emitted and the emitted C references an undeclared identifier
	// (-Wimplicit-function-declaration under the mandated -Werror build). The
	// compile+cc+run here proves the helper is collected through the value
	// part; a helper-call expression and a helper-call && combination prove
	// the whole value part subtree is walked, not just a bare call.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"bool helper call", "fn isBig(n i32) bool { return n > 5; }\nfn main() i32 { print `big? {isBig(7)}`; return 0; }", "big? true\n"},
		{"bool helper expression", "fn isBig(n i32) bool { return n > 5; }\nfn main() i32 { print `big? {isBig(7) && isBig(3)}`; return 0; }", "big? false\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitPrintInVoidHelperCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestEmitDeferredPrintAtVoidHelperExitCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A function-level `defer print 7;` in a void helper fires at the helper's
	// ImplicitReturn exit (the tail emits its DeferChain before falling off
	// the end of the C function), so calling the helper prints "7" and the
	// entry returns 0.
	out := emitAndRunCapture(t, "fn helper() void { defer print 7; }\nfn main() i32 { helper(); return 0; }", false, 0, false)
	if want := "7\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitRangeLoopHelperCallBoundCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A helper call as a range-loop bound: the end is a DirectCall to a
	// helper returning the entry's width, built by buildExpr exactly as any
	// other call expression, and emitted in the for-loop condition. five()
	// returns 5, so 0..five() sums 0+1+2+3+4 = 10. Bounded execution.
	emitAndRunBounded(t, "fn five() i32 { return 5; } fn main() i32 { var sum i32 = 0; loop 0..five() : i { sum = sum + i; } return sum; }", false, 10, false)
}

func TestEmitForLoopInHelperFunctionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// buildHelperFunctions builds each reachable helper's body with the same
	// buildBlock the entry uses, so a classic for loop works inside a helper
	// unchanged. sumTo(4) = 0+1+2+3 = 6, returned as the exit code. Bounded
	// execution.
	emitAndRunBounded(t, "fn sumTo(n i32) i32 { var total i32 = 0; for var step i32 = 0; step < n; step = step + 1 { total = total + step; } return total; } fn main() i32 { return sumTo(4); }", false, 6, false)
}

func TestEmitForLoopHelperCallInConditionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A helper call inside a for clause: the DirectCall is discovered by the
	// reachability walk (which follows For.Children generically) and built by
	// buildComparison/buildExpr like any other call. ten() returns 10, so the
	// loop counts 0..9 and total = 0+1+...+9 = 45. Bounded execution.
	emitAndRunBounded(t, "fn ten() i32 { return 10; } fn main() i32 { var total i32 = 0; for var step i32 = 0; step < ten(); step = step + 1 { total = total + step; } return total; }", false, 45, false)
}

func TestEmitHelperPlusHelperCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The flagship 10.17 fixture: a second, callable function. main calls
	// helper() twice and adds the results, so the process exit code is
	// 21 + 21 = 42. The helper is emitted as its own static function before
	// pebble_user_main, and each call site lowers to pebble_fn_<callee>(ctx)
	// with the context prepended by the backend (the IR threads context via
	// ContextAction, not an explicit argument child).
	emitAndRun(t, "fn helper() i32 { return 21; } fn main() i32 { return helper() + helper(); }", false, 42, false)
}

func TestEmitHelperPlusHelperWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A helper whose body uses the full recursive block grammar buildBlock
	// implements — bool and integer locals, a while loop, a loop-body if, and a
	// two-armed if/else as the tail — proving buildBlock is genuinely reused
	// for a non-entry function, not just a bare return. done gates the loop, i
	// counts 0..4, sum accumulates i, so sum = 0+1+2+3+4 = 10 and the tail's
	// sum > 3 arm returns it. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn helper() i32 { var done bool = false; var sum i32 = 0; var i i32 = 0; while !done { sum = sum + i; i = i + 1; if i == 5 { done = true; } } if sum > 3 { return sum; } else { return sum + 1; } } fn main() i32 { return helper(); }", false, 10, false)
}

func TestEmitHelperWithFullGrammarBodyWritesC(t *testing.T) {
	t.Parallel()
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
		"    bool pebble_local_28 = false;",
		"    while (!(pebble_local_28)) {\n",
		"    if (pebble_local_29 > 3) {\n",
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	// The width discipline extends to called functions: an i64 entry calls an
	// i64 helper, the helper's C return type is int64_t, and the checked add
	// uses the i64 helper family. Exit code 42.
	emitAndRun(t, "fn helper() i64 { return 21; } fn main() i64 { return helper() + helper(); }", false, 42, false)
}

func TestEmitI64HelperWritesC(t *testing.T) {
	t.Parallel()
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

func TestEmitCastsI64HelperResultToI32Main(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "fn helper() i64 { return 21; } fn main() i32 { return helper() as i32; }", false, 21, false)
}

func TestEmitBuildsI64HelperBodyAtItsOwnWidth(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "fn helper() i64 { let value i64 = 20; return value + 1; } fn main() i32 { return helper() as i32; }", false, 21, false)
}

func TestEmitCastsI32HelperResultToI64Main(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "fn helper() i32 { return 7; } fn main() i64 { return (helper() as i64) + 1; }", false, 8, false)
}

func TestEmitCastsU32HelperResultToI32Main(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "fn helper() u32 { return 7; } fn main() i32 { return helper() as i32; }", false, 7, false)
}

func TestEmitF64HelperParamAndReturnCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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

func TestEmitSelfRecursionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Direct recursion: helper calls itself with a decrementing argument and a
	// base case, so the recursion terminates. fact(5) = 120 is the process
	// exit code. Before forward declarations this was a clean rejection
	// (TestEmitRejectsSelfRecursion); now every reachable helper gets a C
	// prototype before any definition, so a self-recursive call is legal C
	// and the backend emits and runs it end-to-end.
	emitAndRun(t, "fn fact(n i32) i32 { if n == 0 { return 1; } else { return n * fact(n - 1); } } fn main() i32 { return fact(5); }", false, 120, false)
}

func TestEmitMutualRecursionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Mutual/indirect recursion: a calls b and b calls a — a genuine
	// two-function call cycle. isEven(10) is true, so the exit code is 1.
	// Before forward declarations this was a clean rejection
	// (TestEmitRejectsMutualRecursion); now the prototype pass makes the
	// cycle legal C regardless of definition order, and the base cases
	// terminate the recursion.
	emitAndRun(t, "fn isEven(n i32) i32 { if n == 0 { return 1; } else { return isOdd(n - 1); } } fn isOdd(n i32) i32 { if n == 0 { return 0; } else { return isEven(n - 1); } } fn main() i32 { return isEven(10); }", false, 1, false)
}

func TestEmitThreeHopRecursionCompilesAndRuns(t *testing.T) {
	t.Parallel()
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

func TestEmitVoidHelperStatementCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A void helper with a parameter and a non-trivial self-contained body: it
	// computes internally (sum 0..x into a local) and returns void, so the
	// call's arguments are built by the same buildCallArguments machinery a
	// value-context call uses, and the exit code (7) reflects only the
	// caller's own logic, proving the call statement did not disturb it.
	emitAndRunBounded(t, "fn helper(x i32) void { var acc i32 = 0; var i i32 = 0; while i < x { acc = acc + i; i = i + 1; } } fn main() i32 { helper(4); return 7; }", false, 7, false)
}

func TestEmitVoidCallInLoopBodyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A void call as a statement inside a loop body: the ExpressionStatement
	// is a plain child of the loop body's Block, flowing through buildLoopBody's
	// statement switch (via the shared buildLeadingStatement) alongside the
	// accumulation Store, so the call executes on every iteration without
	// disrupting the loop's own logic. x = 0+1+2 = 3, the loop's own result.
	emitAndRunBounded(t, "fn helper() void {} fn main() i32 { var x i32 = 0; var i i32 = 0; while i < 3 { helper(); x = x + i; i = i + 1; } return x; }", false, 3, false)
}

func TestEmitVoidHelperCallingVoidHelperCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A void helper whose own body is a void call statement plus its
	// ImplicitReturn tail: helper() calls inner(5) as a statement, then falls
	// off the end of its body. The reachability walk follows the nested call
	// and emits both helpers; the caller exits 3 on its own logic.
	emitAndRun(t, "fn inner(x i32) void { } fn helper() void { inner(5); } fn main() i32 { helper(); return 3; }", false, 3, false)
}

func TestEmitVoidCallInI64EntryCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A void call statement reached from an i64 entry: the void helper is
	// emitted with the C return type void regardless of the entry's width, and
	// the call compiles and runs to the caller's own exit code.
	emitAndRun(t, "fn helper() void {} fn main() i64 { helper(); return 1; }", false, 1, false)
}

func TestEmitVoidHelperWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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

func TestEmitCallInConditionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A helper call is an ordinary expression of the entry's width, so it can
	// appear inside a comparison condition, not just a return value: the
	// reachability walk follows it there and buildComparison's operand path
	// (via buildExpr) lowers it. helper returns 3, 3 < 5 is true, so the
	// then-arm runs and the process exits 1.
	emitAndRun(t, "fn helper() i32 { return 3; } fn main() i32 { if helper() < 5 { return 1; } else { return 2; } }", false, 1, false)
}

func TestEmitAddParametersCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The flagship 10.18 fixture: a two-parameter function called from the
	// entry with two arguments. Each parameter seeds the callee's locals scope
	// before its body is built, so the body's a + b reads them exactly like
	// declared locals, and the call site emits pebble_fn_<id>(ctx, 20, 22).
	// 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { return add(20, 22); }", false, 42, false)
}

func TestEmitAddParametersWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// The bool-parameter fixture: choose takes a bool flag and two integer
	// values and returns one of the integers, so the flag's grammar is the
	// bool one (buildBoolExpr) while the other two parameters are the entry's
	// width. choose(true, 10, 20) takes the then-arm and returns x = 10, the
	// process exit code.
	emitAndRun(t, "fn choose(flag bool, x i32, y i32) i32 { if flag { return x; } else { return y; } } fn main() i32 { return choose(true, 10, 20); }", false, 10, false)
}

func TestEmitBoolParameterWritesC(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A parameter seeding the callee's scope must resolve for the full block
	// grammar, not just a bare return: n is read in the while condition and in
	// a loop-body if condition, while the loop accumulates and reassigns
	// locals. sum_to(5) accumulates 0+1+2+3+4 = 10, the process exit code.
	// Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn sum_to(n i32) i32 { var i i32 = 0; var total i32 = 0; while i < n { if i < n { total = total + i; } i = i + 1; } return total; } fn main() i32 { return sum_to(5); }", false, 10, false)
}

func TestEmitParameterForwardedToHelperCallCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A parameter used as an argument to another call inside its own callee:
	// add forwards its own a parameter to double, whose result is added to the
	// other parameter b. This proves a parameter resolves at a nested call
	// site's argument position (buildCallArguments sees the seeded scope).
	// double(5) = 10, + b(2) = 12, the process exit code.
	emitAndRun(t, "fn double(x i32) i32 { return x + x; } fn add(a i32, b i32) i32 { return double(a) + b; } fn main() i32 { return add(5, 2); }", false, 12, false)
}

func TestEmitNestedCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A call whose argument is itself a call: add(helper(), 5) passes the
	// result of helper() as the first argument. The checker coerces the nested
	// call to the i32 parameter, and buildCallArguments builds it with
	// buildExpr, so the emitted C is pebble_fn_<add>(ctx, pebble_fn_<helper>
	// (ctx), 5). helper() = 5, so add returns 5 + 5 = 10, the exit code.
	emitAndRun(t, "fn helper() i32 { return 5; } fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { return add(helper(), 5); }", false, 10, false)
}

func TestEmitUnusedParameterCompilesClean(t *testing.T) {
	t.Parallel()
	// A genuinely-unused parameter (declared, never read in the callee's body)
	// must still compile under the shared harness's strict -Wall -Wextra
	// -Werror build. -Wunused-parameter genuinely fires for a named parameter
	// the body never reads (confirmed), so the per-parameter
	// (void)pebble_local_<id>; cast emitted right after the opening brace is
	// what keeps this compiling. Exit code 5.
	emitAndRun(t, "fn helper(unused i32) i32 { return 5; } fn main() i32 { return helper(5); }", false, 5, false)
}

func TestEmitI64ParameterizedHelperCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The width discipline extends to parameters: an i64 entry calls an i64
	// helper whose i64 parameters seed its scope, and the checked add uses the
	// i64 helper family. Exit code 42.
	emitAndRun(t, "fn add(a i64, b i64) i64 { return a + b; } fn main() i64 { return add(20, 22); }", false, 42, false)
}

func TestEmitU8ParameterCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact reproduction from proposal 13's active defect: a helper whose
	// only parameter is u8 — a fixed-width integer that is neither the entry's
	// width (int) nor a C-compatible sibling — previously rejected at emission
	// ("has type u8, want int, ..."). The parameter is now declared uint8_t in
	// the C signature, seeded into the callee's scope at the u8 width, and the
	// body's `x as int` cast reads it and widens it to the entry's int. main
	// passes its own u8 local x = 5 through and returns 5, the process exit
	// code, proving the parameter's actual value was received end-to-end.
	emitAndRun(t, "fn take(x u8) int { return x as int; } fn main() int { let x u8 = 5; return take(x); }", false, 5, false)
}

func TestEmitU8ParameterCompilesAndRunsWithValue(t *testing.T) {
	t.Parallel()
	// A second u8-parameter fixture with a non-trivial value: take(200) must
	// return 200 (the parameter's actual received value, not zero or garbage).
	// 200 is well within u8's range and distinct from any default, so an exit
	// code of 200 proves the uint8_t parameter carried the caller's value.
	emitAndRun(t, "fn take(x u8) int { return x as int; } fn main() int { return take(200); }", false, 200, false)
}

func TestEmitI16ParameterCompilesAndRunsWithValue(t *testing.T) {
	t.Parallel()
	// An i16-typed parameter, a signed non-entry-width integer: take(300)
	// compares its parameter against 300 (a value that exceeds u8's range, so
	// the check can only pass if the int16_t parameter genuinely received the
	// full 300) and returns 1 when equal. Exit code 1 proves the comparison —
	// built at the parameter's own i16 width — saw the caller's value.
	emitAndRun(t, "fn take(x i16) int { if x == 300 { return 1; } return 0; } fn main() int { return take(300); }", false, 1, false)
}

func TestEmitU32ParameterCompilesAndRunsWithValue(t *testing.T) {
	t.Parallel()
	// A u32-typed parameter, a wider unsigned non-entry-width integer:
	// take(70000) — a value that exceeds u16's range, so the check can only
	// pass if the uint32_t parameter genuinely received the full 70000 —
	// compares its parameter against 70000 and returns 1 when equal. The
	// argument is passed as a u32-typed local, exercising the call-argument
	// side for a non-entry-width integer too. Exit code 1 proves the
	// parameter's value was received correctly.
	emitAndRun(t, "fn take(x u32) int { if x == 70000 { return 1; } return 0; } fn main() int { let v u32 = 70000; return take(v); }", false, 1, false)
}

func TestEmitI32ParameterI64EntryCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A concrete fixed-width integer parameter in an i64 entry: an i32-typed
	// parameter is neither the entry's width (i64) nor a C-compatible sibling
	// (int32_t vs int64_t), so it exercises the same widened gate for an
	// entry whose own width is not the abstract `int`. toI64(5) receives the
	// i32 value 5 at its own int32_t C type, casts it to the i64 result, and
	// main returns 5 — proving the entry-width and non-entry-width integer
	// parameters coexist in one program.
	emitAndRun(t, "fn toI64(x i32) i64 { return x as i64; } fn main() i64 { let v i32 = 5; return toI64(v); }", false, 5, false)
}

func TestEmitU8ParameterWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for the exact reproduction: the helper's prototype AND
	// definition both declare the u8 parameter at its own C type
	// (uint8_t pebble_local_25), the body reads it at that width and casts it
	// to the entry's int, and the call site passes main's u8 local
	// (uint8_t pebble_local_29 = 5u). Symbols come from the real fixture dump
	// (take=24, its parameter x=25, main's local x=29).
	unit, snapshot, entryID, sources := buildFixture(t, "fn take(x u8) int { return x as int; } fn main() int { let x u8 = 5; return take(x); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int64_t pebble_fn_24(PebbleContext *ctx, uint8_t pebble_local_25);",
		"static int64_t pebble_fn_24(PebbleContext *ctx, uint8_t pebble_local_25) {",
		"    (void)pebble_local_25;",
		"    return (int64_t)(pebble_local_25);",
		"uint8_t pebble_local_29 = 5u;",
		"return pebble_fn_24(ctx, pebble_local_29);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 5, false)
}

func TestEmitInlineAggregateArgumentWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for inline construction at a call site: each argument must
	// be the C99 compound-literal expression — not a local reference and not a
	// bare brace list — with the cast naming the aggregate's own typedef. The
	// tuple form is the positional (pebble_tuple_23_t){ 20, 22 }; the struct
	// form written out of declared order is
	// (pebble_struct_19_t){ .pebble_field_26 = 22, .pebble_field_25 = 20 }.
	// Symbols and type IDs come from the real fixture dumps (tuple: f=24,
	// tuple type 23; struct: Point=24, x=25, y=26, f=27, struct type 19).
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
	if !strings.Contains(out, "return pebble_fn_27(ctx, (pebble_struct_19_t){ .pebble_field_26 = 22, .pebble_field_25 = 20 });") {
		t.Errorf("emitted C missing the struct compound-literal argument:\n%s", out)
	}
}

func TestEmitStrParameterLiteralCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStrParameterWritesC(t *testing.T) {
	t.Parallel()
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

func TestEmitStrReturningHelperDirectComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-returning helper whose result is directly compared without an
	// intermediate local: g() == "hi" — confirmed checker-reachable (the
	// comparison's left operand is a DirectCall of type str, dumped from a real
	// fixture). The call result flows straight into pebble_rt_str_eq as a
	// PebbleStr value. 7 on the then-arm.
	emitAndRun(t, "fn g() str { return \"hi\"; } fn main() i32 { if g() == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReturningHelperChainedReturnCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-returning helper whose result is another str-returning helper's
	// return value (`return g();` — confirmed checker-reachable, a DirectCall
	// as a str-returning function's Return child): h forwards g's result, the
	// entry declares s from h's call, and the comparison proves the value
	// survived the two-hop chain. 7 on the then-arm.
	emitAndRun(t, "fn g() str { return \"hi\"; } fn h() str { return g(); } fn main() i32 { let s str = h(); if s == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrParameterAndResultCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A helper taking a str parameter and returning it — `fn echo(s str) str
	// { return s; }` — combining the str-parameter and str-result support: the
	// parameter seeds the callee's scope as a str local and the tail return
	// forwards it, so the same value round-trips through the function
	// boundary. 7 on the then-arm.
	emitAndRun(t, "fn echo(s str) str { return s; } fn main() i32 { let s str = echo(\"hi\"); if s == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReturningHelperAsCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-returning helper's result passed as a str-typed call argument
	// (f(g())) — the argument builder shares buildStrOperand with the direct-
	// comparison and return paths, so a str value is uniformly whatever a str
	// expression builds. g's result lands in f's parameter and compares equal,
	// exiting 1.
	emitAndRun(t, "fn g() str { return \"hi\"; } fn f(s str) i32 { if s == \"hi\" { return 1; } else { return 0; } } fn main() i32 { return f(g()); }", false, 1, false)
}

func TestEmitStrReturningHelperWritesC(t *testing.T) {
	t.Parallel()
	// The parameter and return C types for a str-taking, str-returning helper
	// (the greet flagship): the helper's C signature declares PebbleStr for
	// both the parameter and the return type — the runtime ABI's fixed type, no
	// typedef — its return statement forwards the parameter, and the call site
	// passes the literal as a PebbleStr compound literal and declares the
	// entry's local from the call. Symbols 24 (greet), 25 (the name
	// parameter), and 29 (the entry's s local) come from the real fixture dump.
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
		"PebbleStr pebble_local_29 = pebble_fn_24(ctx, (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 });",
		"    (void)pebble_local_29;",
		"if (pebble_rt_str_eq(pebble_local_29, (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 })) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitSwitchInHelperCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A switch nested inside a helper function: the helper receives a
	// parameter and switches on it, returning different values. The entry
	// calls the helper with different arguments, confirming the switch works
	// in a helper context. The entry returns the sum of two calls: helper(1)
	// = 10 and helper(99) = 0, so exit code is 10.
	emitAndRun(t, "fn helper(x i32) i32 { switch x { case 1, 2: return 10; case 3: return 30; else: return 0; } } fn main() i32 { return helper(1) + helper(99); }", false, 10, false)
}

func TestEmitSwitchWithHelperCallInSubjectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A helper call as the switch subject expression: the subject is the
	// result of calling a helper, confirming buildExpr's DirectCall path
	// works in the subject position.
	emitAndRun(t, "fn getVal() i32 { return 2; } fn main() i32 { switch getVal() { case 1: return 10; case 2: return 20; else: return 0; } }", false, 20, false)
}

func TestEmitSwitchNestedInHelperWithParamsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A switch inside a helper that takes a parameter, with the subject
	// being the parameter itself. Exercises the full path: parameter seeding
	// into scope, switch subject resolution, case body building. Two calls:
	// helper(1) = 10, helper(5) = 0, sum = 10.
	emitAndRun(t, "fn classify(x i32) i32 { switch x { case 1: return 10; case 2: return 20; case 3: return 30; else: return 0; } } fn main() i32 { return classify(1) + classify(5); }", false, 10, false)
}

func TestEmitDeferInHelperCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A defer inside a helper function. The helper has its own defer that
	// modifies a local before returning it. The entry calls the helper and
	// returns the result. helper(): x=0, defer x=x+5, return x -> returns 5.
	// main() returns helper() = 5.
	emitAndRunBounded(t, "fn helper() i32 { var x i32 = 0; defer x = x + 5; return x; } fn main() i32 { return helper(); }", false, 5, false)
}

func TestEmitDeferredVoidCallFiresCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A deferred void call inside a loop firing before a break, mirroring
	// 10.32's TestEmitDeferBeforeBreakCompilesAndRuns: the break's DeferChain
	// includes the loop-registered defers, so both the deferred Store and the
	// deferred void call run before the break. x starts at 10; on i == 3 the
	// break fires the deferred x = x + 1 (x = 11) then helper(); the program
	// exits 11. If the deferred call were miscompiled the build would fail.
	emitAndRunBounded(t, "fn helper() void {} fn main() i32 { var x i32 = 10; var i i32 = 0; while i < 5 { if i == 3 { defer helper(); defer x = x + 1; break; } i = i + 1; } return x; }", false, 11, false)
}

func TestEmitDeferredVoidCallOutsideLoopDoesNotFireCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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

func TestEmitU64StrIndexInHelperCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The str-index gap this slice closes: indexing a str (s[i]) inside a
	// u64-returning helper must lower to pebble_rt_str_char_at_u64 (not the
	// empty-suffix name that previously failed at cc compile time), decode
	// the right codepoint, and drive a comparison to a real result.
	emitAndRun(t, "fn f() u64 { var s str = \"hi\"; var c char = s[1]; if c == 'i' { return 1; } return 0; } fn main() int { return f() as int; }", false, 1, false)
}

func TestEmitCharParameterAndResultCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char-typed parameter and result, called and compared: f takes a char,
	// forwards it as its char result, and main declares c from the call then
	// compares it against 'a' — proving the char value survives the
	// helper-call round trip at both the C int32_t parameter and return type.
	emitAndRun(t, "fn f(c char) char { return c; } fn main() i32 { let c char = f('a'); if c == 'a' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitCharParameterAndResultDistinctCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The false outcome of the parameter/result fixture: f('b') returns 'b',
	// compared against 'a', so the process exits 0 — the value that survives
	// the call round trip is the argument, not a fixed constant.
	emitAndRun(t, "fn f(c char) char { return c; } fn main() i32 { let c char = f('b'); if c == 'a' { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitCharCallArgumentLiteralCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A char literal passed directly as a call argument (f('a')), no
	// intermediate local — the CharLiteral shape a char parameter accepts at
	// the call site.
	emitAndRun(t, "fn f(x char) i32 { if x == 'a' { return 1; } else { return 0; } } fn main() i32 { return f('a'); }", false, 1, false)
}

func TestEmitPointerReturnFromHelperCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A helper accepts a pointer and returns it unchanged; the entry passes
	// the address of its own local (which stays live for the whole call) and
	// reads through the returned pointer. Proves both pointer-typed
	// parameters and pointer-typed helper results lower correctly.
	// fn identity(p *i32) *i32 { return p; }
	// fn main() i32 { var x i32 = 42; let p *i32 = identity(&x); return *p; }
	emitAndRun(t, "fn identity(p *i32) *i32 { return p; } fn main() i32 { var x i32 = 42; let p *i32 = identity(&x); return *p; }", false, 42, false)
}

func TestEmitVariadicCallLenOnlyCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "fn sum(...values []int) int { return values.len as int; } fn main() int { return sum(1, 2, 3); }", false, 3, false)
}

func TestEmitVariadicCallFixedPlusVariadicCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// No variadic arguments at all: the collected slice must be the
	// established empty-slice shape (.data = NULL, .len = 0), not an
	// invalid empty array compound literal (a GNU extension, not portable
	// C99/C11).
	emitAndRun(t, "fn count(...values []int) int { return values.len as int; } fn main() int { return count(); }", false, 0, false)
}

func TestEmitVariadicCallBoolElementCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Confirms the variadic element dispatch isn't hardcoded to int: a
	// []bool trailing parameter, collected via buildBoolExpr per element.
	emitAndRun(t, "fn allTrue(...values []bool) int { if values[0] && values[1] { return 1; } return 0; } fn main() int { return allTrue(true, true); }", false, 1, false)
}

func TestEmitVariadicCallBoolElementFalseCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "fn allTrue(...values []bool) int { if values[0] && values[1] { return 1; } return 0; } fn main() int { return allTrue(true, false); }", false, 0, false)
}

func TestEmitVariadicCallSoleSliceTailArgumentForwardsSliceCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact reproduction: an existing slice-typed local passed as the SOLE
	// tail argument to a variadic slice parameter. V1's rule (and codegen
	// shortcut) forwards the whole slice directly rather than collecting one
	// element, and this backend must emit the slice local itself as the
	// argument — `sum(s)` over [1, 2, 3] returns 6. Bounded because of the
	// while loop.
	emitAndRunBounded(t, "fn sum(...values []int) int { var total int = 0; var i uint = 0; while i < values.len { total = total + values[i]; i = i + 1; } return total; } fn main() int { var arr [3]int = [1, 2, 3]; var s []int = arr[0:3]; return sum(s); }", false, 6, false)
}

func TestEmitVariadicCallSoleLiteralTailArgumentStillCollectsOneElement(t *testing.T) {
	t.Parallel()
	// A single int literal as the sole tail argument is NOT a slice, so it
	// must keep working exactly as one collected element: sum(5) == 5.
	emitAndRunBounded(t, "fn sum(...values []int) int { var total int = 0; var i uint = 0; while i < values.len { total = total + values[i]; i = i + 1; } return total; } fn main() int { return sum(5); }", false, 5, false)
}

func TestEmitVariadicCallMultipleTailElementsStillCollectCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Two or more tail arguments are never slice-forwarded — the collected
	// array-backed compound literal is still built: sum(1, 2, 3) == 6.
	emitAndRunBounded(t, "fn sum(...values []int) int { var total int = 0; var i uint = 0; while i < values.len { total = total + values[i]; i = i + 1; } return total; } fn main() int { return sum(1, 2, 3); }", false, 6, false)
}

func TestEmitVariadicCallFixedParameterWithSoleSliceTailForwardsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A fixed parameter alongside a variadic slice tail whose sole argument is
	// an existing slice: the fixed param passes normally and the slice is
	// forwarded whole. 10 + (1+2+3) = 16.
	emitAndRunBounded(t, "fn tagged(prefix int, ...values []int) int { var total int = prefix; var i uint = 0; while i < values.len { total = total + values[i]; i = i + 1; } return total; } fn main() int { var arr [3]int = [1, 2, 3]; var s []int = arr[0:3]; return tagged(10, s); }", false, 16, false)
}

func TestEmitGenericVariadicCallSoleSliceTailForwardsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The GENERIC variadic route reaches the same backend forward case by a
	// different checker path: instantiateSignature already binds the whole
	// slice to the sole tail argument, so the emitted C must forward the slice
	// local directly too. head(s) over [1, 2, 3] returns 1.
	emitAndRun(t, "fn head[T](...values []T) T { return values[0]; } fn main() int { var arr [3]int = [1, 2, 3]; var s []int = arr[0:3]; return head(s); }", false, 1, false)
}

func TestEmitBoolReturningDirectCallInIfConditionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact repro: a plain bool-returning helper called directly in a bool
	// position. validateHelperSignature admits a bool result, so the if
	// condition lowers to a bool-typed DirectCall, which buildBoolExpr now
	// builds through buildDirectCall. The branch taken proves the call's C
	// bool result drives the condition correctly.
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { if id(true) { return 1; } else { return 2; } }", true, 1, false)
	emitAndRun(t, "fn id(b bool) bool { return b; } fn main() int { if id(false) { return 1; } else { return 2; } }", true, 2, false)
}

func TestEmitBoolReturningDirectCallInWhileConditionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The same direct call used as a while-loop condition: `while id(n < 2)`
	// calls id with a comparison argument, so the condition is a DirectCall
	// whose argument is itself a bool value built by the same builder. The
	// loop must run exactly twice, proving the call result keeps the loop
	// going and then stops it.
	emitAndRunBounded(t, "fn id(b bool) bool { return b; } fn main() int { var n int = 0; while id(n < 2) { n = n + 1; } return n; }", true, 2, false)
}

func TestEmitBoolReturningDirectCallWithShortCircuitCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// A bool-returning method call in a bool position lowers to a bool-typed
	// MethodCall (the receiver becomes the self parameter through
	// buildCallArguments), which buildBoolExpr builds through buildDirectCall
	// just like the DirectCall case. Both the true and false branch results
	// confirm the method result drives the condition.
	emitAndRun(t, "type Box = struct { value bool; fn isTrue(self Box) bool => self.value; }; fn main() int { let box Box = Box.{ value = true }; if box.isTrue() { return 1; } else { return 2; } }", true, 1, false)
	emitAndRun(t, "type Box = struct { value bool; fn isTrue(self Box) bool => self.value; }; fn main() int { let box Box = Box.{ value = false }; if box.isTrue() { return 1; } else { return 2; } }", true, 2, false)
}

func TestEmitFunctionTypedParameterCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact minimal parameter repro: apply(f fn(int,int)int, x int, y
	// int) calls through the function-typed parameter.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int { return apply(add, 1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedResultCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact minimal result repro: chooseOp() fn(int,int)int returns a
	// bare function reference, forwarded into a local and called through it.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn chooseOp() fn(int, int) int { return add; } fn main() int { var f fn(int, int) int = chooseOp(); return f(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedParameterMixedSignatureCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A function-typed parameter combined with other parameter types and
	// positions in the same signature — confirms the dispatch works
	// regardless of where in the parameter list the function-typed one sits.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn apply(x int, f fn(int, int) int, y int) int { return f(x, y); } fn main() int { return apply(10, add, 20); }", false, 30, false)
}

func TestEmitFunctionTypedResultChainedCallCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A function-returning helper's result forwarded directly as another
	// call's argument, and a function-returning helper calling another
	// function-returning helper — both real DirectCall-as-function-value
	// shapes, not just the caller-side local-declaration read-back.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn chooseOp() fn(int, int) int { return add; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int { return apply(chooseOp(), 1, 2); }", false, 3, false)
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn chooseOp() fn(int, int) int { return add; } fn wrap() fn(int, int) int { return chooseOp(); } fn main() int { var f fn(int, int) int = wrap(); return f(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedGenericValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A generic function referenced as a first-class value
	// (GenericFunctionValue, confirmed checker-reachable via identity[int]),
	// both as a local's initializer and as a call argument.
	emitAndRun(t, "fn identity[T](x T) T { return x; } fn main() int { var f fn(int) int = identity[int]; return f(3); }", false, 3, false)
	emitAndRun(t, "fn identity[T](x T) T { return x; } fn apply(f fn(int) int) int { return f(5); } fn main() int { return apply(identity[int]); }", false, 5, false)
}

func TestEmitFunctionTypedParameterResultWritesC(t *testing.T) {
	t.Parallel()
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

func TestEmitU64FunctionTypeResultCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
	// The mirror: u64 as a function type's PARAMETER, used as a function-typed
	// local and called with a real u64 value. The param side had the identical
	// isUint-excludes-U64 bug as the result side, so this must be fixed for
	// symmetry, not just the result.
	emitAndRun(t, "fn udToInt(x u64) int { return x as int; } fn main() int { var v u64 = 7; var f fn(u64) int = udToInt; return f(v); }", false, 7, false)
	// And as a function-typed HELPER PARAMETER, called with a u64 argument.
	emitAndRun(t, "fn udToInt(x u64) int { return x as int; } fn apply(f fn(u64) int, x u64) int { return f(x); } fn main() int { var v u64 = 9; return apply(udToInt, v); }", false, 9, false)
}

func TestEmitU64FunctionTypeBothCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A function type whose signature mentions u64 in BOTH the parameter and
	// the result, called round-trip through a u64 local.
	emitAndRun(t, "fn id(x u64) u64 { return x; } fn main() int { var f fn(u64) u64 = id; var r u64 = f(5); return r as int; }", false, 5, false)
}

func TestEmitU64FunctionTypeHelperParameterAndResultCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	// A u64 function-type INDIRECT CALL result consumed as a plain rvalue in a
	// NON-local position (not just a local-declaration initializer): forwarded
	// directly as another expression's integer operand. This is the seat the
	// original tracker framed as the "results" gap — the indirect call's result
	// must be built at the u64 width even though the enclosing statement is
	// entry-width.
	emitAndRun(t, "fn hashOf(x int) u64 { return x as u64; } fn main() int { var f fn(int) u64 = hashOf; return (f(5) as int) + 1; }", false, 6, false)
}

func TestEmitPointerFunctionTypeResultCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The mirror repro for the pointer RESULT gap: a function-typed local
	// whose signature returns a `*int` result (`fn(*int) *int`), called
	// through an indirect call and the pointer result consumed as a pointer
	// value — assigned into a pointer local and dereferenced. Exercises the
	// `int32_t *` fnptr typedef return type and buildExpr's pointer-typed
	// indirect-call result path (an IndirectCall bypasses the width gate even
	// when its result is a pointer).
	emitAndRun(t, "fn identity(p *int) *int { return p; } fn main() int { var x int = 42; var f fn(*int) *int = identity; var p *int = f(&x); return *p; }", false, 42, false)
}

func TestEmitPointerFunctionTypeNilResultCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A function type whose RESULT is a pointer but whose parameters are all
	// non-pointer (`fn() *int`): a helper returning nil, referenced as a first
	// -class value, and the pointer result consumed through an indirect call.
	// This isolates the pointer-result spelling (an `int32_t *` typedef return
	// type after `(PebbleContext *ctx)`) from the pointer-parameter spelling.
	emitAndRun(t, "fn zeroPtr() *int { return nil; } fn main() int { var f fn() *int = zeroPtr; var p *int = f(); return 0; }", false, 0, false)
}

func TestEmitPointerFunctionTypeHelperParameterAndResultCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A pointer-bearing function-type PARAMETER and RESULT in ordinary (non
	// -function-typed) helper positions: a helper taking a `fn(*int) int`
	// parameter calls through it with a pointer argument, and a helper
	// returning `fn(*int) *int` is forwardable — the same rows of
	// validateHelperSignature / helperSignature / buildCallArgument the u64
	// function-type tests exercise, for pointer shapes.
	emitAndRun(t, "fn readPtr(p *int) int { return *p; } fn apply(f fn(*int) int, p *int) int { return f(p); } fn main() int { var x int = 42; return apply(readPtr, &x); }", false, 42, false)
	// A helper whose RESULT is a function type whose parameter AND result are
	// pointers, forwarded into a function-typed local and called through it.
	emitAndRun(t, "fn identity(p *int) *int { return p; } fn choose() fn(*int) *int { return identity; } fn main() int { var x int = 42; var f fn(*int) *int = choose(); var p *int = f(&x); return *p; }", false, 42, false)
}

func TestEmitPointerFunctionTypeWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C directly: the fnptr typedef declares the trailing
	// PebbleContext *ctx parameter first and the pointer parameter's C type
	// `int64_t *` right after it — the exact pointer spelling helperSignature
	// gives an ordinary helper's pointer parameter, not a rejection. The
	// function value is also assigned bare (no cast) at the declaration site.
	unit, snapshot, entryID, sources := buildFixture(t, "fn readPtr(p *int) int { return *p; } fn main() int { var x int = 3; var f fn(*int) int = readPtr; return f(&x); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		")(PebbleContext *ctx, int64_t *);", // fn(*int) int: ctx then the pointer C type
		"(*pebble_fnptr_",
		"= pebble_fn_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitNarrowFixedWidthFunctionTypeParamCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A first-class function type whose PARAMETER is any fixed-width integer
	// other than the entry's own width — u8/u16/i8/i16/u32/i64 — must compile
	// and run. This was a genuine validator/builder inconsistency: the exact
	// reproduction is a u8 parameter, which validateFunctionTypeSignature
	// admitted (any fixed-width integer, resolved at its OWN width) but whose
	// typedef builder functionTypeParamCType rejected (it only matched the
	// AMBIENT entry width, so a u8 parameter fell through to the "is not
	// supported, want int, uint, bool, char, str, or a pointer type"
	// rejection). Each row uses a different argument value so the emitted
	// parameter width is pinned: the value must arrive at the callee intact
	// and come back +1. (The u64 row is already covered by the u64
	// function-type tests above; every other fixed-width integer is here.)
	tests := []struct {
		name     string
		src      string
		wantCode int
	}{
		{"u8", "fn add_one(x u8) int { return x as int + 1; } fn main() int { var f fn(u8) int = add_one; return f(5); }", 6},
		{"u16", "fn add_one(x u16) int { return x as int + 1; } fn main() int { var f fn(u16) int = add_one; return f(6); }", 7},
		{"i8", "fn add_one(x i8) int { return x as int + 1; } fn main() int { var f fn(i8) int = add_one; return f(7); }", 8},
		{"i16", "fn add_one(x i16) int { return x as int + 1; } fn main() int { var f fn(i16) int = add_one; return f(8); }", 9},
		{"u32", "fn add_one(x u32) int { return x as int + 1; } fn main() int { var f fn(u32) int = add_one; return f(9); }", 10},
		{"i64", "fn add_one(x i64) int { return x as int + 1; } fn main() int { var f fn(i64) int = add_one; return f(10); }", 11},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.wantCode, false)
		})
	}
}

func TestEmitNarrowU8FunctionTypeParamWritesC(t *testing.T) {
	t.Parallel()
	// Confirm the emitted C directly for the narrowest repro: the fnptr
	// typedef declares the u8 parameter as uint8_t (the parameter's OWN
	// resolved C type, from cType(u8)), the hoisted helper declares its
	// parameter as uint8_t too, and the indirect call passes the argument at
	// that same width — so the typedef, the callee, and the call site always
	// agree. Before the fix, the typedef builder rejected the u8 parameter
	// outright.
	unit, snapshot, entryID, sources := buildFixture(t, "fn add_one(x u8) int { return x as int + 1; } fn main() int { var f fn(u8) int = add_one; return f(5); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef int64_t (*pebble_fnptr_",     // fn(u8) int result slot
		", uint8_t);",                         // the u8 parameter slot in the typedef
		"(PebbleContext *ctx, uint8_t pebble", // the hoisted helper's own u8 parameter
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitOtherFunctionTypeParamShapesCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The non-integer and word-sized parameter shapes the widened typedef
	// builder must leave completely untouched: uint (uint64_t via its own
	// explicit case), char (int32_t), and str (PebbleStr). The ambient
	// entry-width, u64, bool, and pointer parameter shapes already have their
	// own tests above/below, so they are not duplicated here.
	emitAndRun(t, "fn add_one(x uint) int { return x as int + 1; } fn main() int { var u uint = 5; var f fn(uint) int = add_one; return f(u); }", false, 6, false)
	emitAndRun(t, "fn isA(c char) int { if c == 'a' { return 1; } else { return 0; } } fn main() int { var f fn(char) int = isA; return f('a'); }", false, 1, false)
	emitAndRun(t, "fn getLen(s str) int { return 1; } fn main() int { var f fn(str) int = getLen; return f(\"hi\"); }", false, 1, false)
}

func TestEmitNarrowU8FunctionTypeResultStillRejected(t *testing.T) {
	t.Parallel()
	// The RESULT side of the gap (beyond the f32/f64 widening this phase adds)
	// is deliberately OUT of scope and must keep rejecting exactly as before:
	// a fn() u8 type passes parameter validation but its RESULT u8 is not among
	// the result shapes this backend can consume an indirect call's result in
	// (entry width, u64, bool, char, f32, f64, void, pointer). The regression
	// guards against the float widening leaking into an unrestricted result
	// gate — the narrow fixed-width integer results stay rejected.
	emitAndRunRejects(t, "fn get_u8() u8 { return 5; } fn main() int { var f fn() u8 = get_u8; return f() as int; }", "has result type u8, want int, u64, bool, char, str, f32, f64, void, a pointer type, or a plain struct type")
}

func TestCheckStdModuleGenericMethodIndexedFieldWrite(t *testing.T) {
	t.Parallel()
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

func TestEmitEnumHelperReturnCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The exact reproduction from the gap analysis (proposal 13, active
	// defect): a plain-enum-returning helper called from the entry, its result
	// bound into an enum-typed local via a DirectCall initializer and then
	// read back through a narrowing switch. Before the fix, helperSignature's
	// return-type switch had no isEnumType case — an enum result fell through
	// to the isStruct case (an enum is Nominal, so isStruct reports true) and
	// the tail return was rejected with a struct-flavored error. pick() returns
	// Color.green, so the green case fires and the exit code is 1.
	emitAndRun(t, `type Color = enum { red, green, blue };

fn pick() Color {
    return Color.green;
}

fn main() int {
    let c = pick();
    switch c {
        case Color.red: return 0;
        case Color.green: return 1;
        case Color.blue: return 2;
    }
}`, false, 1, false)
}

func TestEmitEnumHelperReturnWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for the enum-returning helper: the helper's prototype and
	// definition are declared with the enum's own pebble_enum_<typeID>_t C
	// return type (never a struct typedef), its tail return emits the
	// variant's C constant, and the entry binds the call's result into an
	// enum-typed local of the same typedef. The pebble_struct_<typeID>_t
	// negative check proves the helper no longer falls through to the struct
	// result shape the bug produced.
	unit, snapshot, entryID, enumType, variants, sources := enumFixture(t, `type Color = enum { red, green, blue };

fn pick() Color {
    return Color.green;
}

fn main() int {
    let c = pick();
    switch c {
        case Color.red: return 0;
        case Color.green: return 1;
        case Color.blue: return 2;
    }
}`)
	if len(variants) != 3 {
		t.Fatalf("fixture has %d variants, want 3 (red, green, blue)", len(variants))
	}
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static " + enumTypeName(enumType) + " pebble_fn_",
		"return pebble_variant_" + strconv.Itoa(int(variants[1])) + ";",
		enumTypeName(enumType) + " pebble_local_",
		"= pebble_fn_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_struct_"+strconv.Itoa(int(enumType))+"_t pebble_fn") {
		t.Errorf("emitted C declared the enum-returning helper with a struct typedef, want the enum typedef:\n%s", out)
	}
}

func TestEmitUnionHelperReturnCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The "or tagged union" half of the gap: a tagged-union-returning helper
	// constructs a payload-carrying variant, returns it, and the entry binds
	// the result into a union-typed local and reads it back through a
	// narrowing switch. pick() constructs Choice.value(5), so the value case
	// fires and the exit code is 1. This exercises the isTaggedUnionType
	// result case (declared with the union's pebble_union_<typeID>_t return
	// type), buildUnionCallInitializer at the call site, and
	// buildReturnStatement's union branch building the variant construction.
	emitAndRun(t, `type Choice = union enum { empty void; value int; };

fn pick() Choice {
    return Choice.value(5);
}

fn main() int {
    let c = pick();
    switch c {
        case Choice.empty: return 0;
        case Choice.value: return 1;
    }
}`, false, 1, false)
}

func TestEmitUnionHelperReturnWritesC(t *testing.T) {
	t.Parallel()
	// The emitted C for the tagged-union-returning helper: the helper is
	// declared with the union's own pebble_union_<typeID>_t C return type
	// (never a struct typedef), the entry binds the call's result into a
	// union-typed local of the same typedef, and the payload-carrying
	// construction's compound literal is present in the helper body.
	unit, snapshot, entryID, unionType, variants, sources := unionFixture(t, `type Choice = union enum { empty void; value int; };

fn pick() Choice {
    return Choice.value(5);
}

fn main() int {
    let c = pick();
    switch c {
        case Choice.empty: return 0;
        case Choice.value: return 1;
    }
}`)
	if len(variants) != 2 {
		t.Fatalf("fixture has %d variants, want 2 (empty, value)", len(variants))
	}
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static " + unionTypeName(unionType) + " pebble_fn_",
		unionTypeName(unionType) + " pebble_local_",
		"= pebble_fn_",
		".tag = pebble_variant_" + strconv.Itoa(int(variants[1])),
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_struct_"+strconv.Itoa(int(unionType))+"_t pebble_fn") {
		t.Errorf("emitted C declared the union-returning helper with a struct typedef, want the union typedef:\n%s", out)
	}
}

func TestEmitArrayValueDirectReturnCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A fixed-array-returning helper whose tail return is a direct ArrayValue
	// literal (`return [1, 2, 3];`), not a local or a forwarded call.
	emitAndRun(t, `fn make() [3]int {
    return [1, 2, 3];
}
fn main() int {
    let a = make();
    return a[0] + a[1] + a[2];
}`, false, 6, false)
}

func TestEmitArrayRepeatDirectReturnCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A fixed-array-returning helper whose tail return is a direct ArrayRepeat
	// (`return [7; 3];`), not a local or a forwarded call.
	emitAndRun(t, `fn repeated() [3]int {
    return [7; 3];
}
fn main() int {
    let a = repeated();
    return a[0] + a[1] + a[2];
}`, false, 21, false)
}

func TestEmitArrayRepeatDirectReturnEvaluatesValueOnce(t *testing.T) {
	t.Parallel()
	// Regression: an ArrayRepeat direct return (`return [v; N];`) must build
	// its value expression into a single C temp and reference that temp N
	// times in the compound literal, not repeat the raw expression string N
	// times — a brace-list ArrayValue literal like [f(), f(), f()] legitimately
	// evaluates each written element separately, but [v; N] is one source
	// expression meant to be evaluated exactly once and copied. Assert the
	// emitted C declares exactly one pebble_repeat_ret_ temp and the compound
	// literal's brace list references that same temp name three times, rather
	// than containing the value expression three separate times.
	unit, snapshot, entryID, sources := buildFixture(t, `fn repeated() [3]int {
    return [7; 3];
}
fn main() int {
    let a = repeated();
    return a[0] + a[1] + a[2];
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	compileAndRun(t, buf.Bytes(), 21, false)
	tempCount := strings.Count(out, "pebble_repeat_ret_")
	if tempCount < 4 {
		t.Fatalf("expected one temp declaration plus three brace-list references (>= 4 occurrences of pebble_repeat_ret_), got %d:\n%s", tempCount, out)
	}
	if !strings.Contains(out, "= 7LL;") {
		t.Fatalf("expected exactly one assignment of the repeated value 7 into the temp:\n%s", out)
	}
	if strings.Count(out, "= 7LL;") != 1 {
		t.Fatalf("expected the value 7 to be assigned exactly once (single evaluation), got %d occurrences of \"= 7;\":\n%s", strings.Count(out, "= 7;"), out)
	}
}

// --- proposal 14 audit: function declaration, anonymous function, direct call ---

// TestEmitFunctionDeclarationParameterResultMatrixCompilesAndRuns is the
// focused proof for proposal 14's "Pebble function declaration with
// parameters, result, body, and hidden context — Implemented, proof needed for
// the full parameter/result matrix" row. One program declares and directly
// calls a helper for each cell of the representative matrix: 0 parameters
// (zero), 1 parameter (one), and several parameters of DIFFERENT types in a
// single signature (combine: int, bool, str, struct, pointer), plus one helper
// per supported result category (int, bool, str, void, a struct, a pointer).
// Every helper carries its own body and the hidden context (the C
// `PebbleContext *ctx` every Pebble-convention helper is threaded); the exit
// code is the sum of each helper's distinguishable contribution, so a wrong
// parameter/result row fails the process code. Contributions: zero()=0,
// one(1)=1, b()=+1, s()=="hi"=+2, combine(3,true,"hi",{1,2},&4)=3+10+2+1+2+4
// =22, mk()={40,2}=+42, ptr()==nil=+1.
func TestEmitFunctionDeclarationParameterResultMatrixCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `
type Pair = struct { x int; y int; };
fn zero() int { return 0; }
fn one(n int) int { return n; }
fn combine(n int, flag bool, s str, p Pair, ptr *int) int {
    var base int = n;
    if flag { base = base + 10; }
    base = base + (s.len as int);
    base = base + p.x + p.y;
    base = base + *ptr;
    return base;
}
fn b() bool { return true; }
fn s() str { return "hi"; }
fn nothing() void {}
fn mk() Pair { return Pair.{ x = 40, y = 2 }; }
fn ptr() *int { return nil; }
fn main() int {
    nothing();
    var total int = zero() + one(1);
    if b() { total = total + 1; }
    if s() == "hi" { total = total + 2; }
    var v int = 4;
    let p = Pair.{ x = 1, y = 2 };
    total = total + combine(3, true, "hi", p, &v);
    let q = mk();
    total = total + q.x + q.y;
    if ptr() == nil { total = total + 1; }
    return total;
}`, false, 0+1+1+2+22+42+1, false)
}

// TestEmitFunctionDeclarationHiddenContextAndSignatureWritesC pins the emitted
// C for the matrix's mixed-signature declaration: the helper's prototype AND
// definition declare the hidden PebbleContext *ctx first, then each parameter
// at its own C type (int32_t, bool, PebbleStr, the struct typedef, int32_t *),
// each with the per-parameter (void) cast, and the call site threads ctx before
// the authored arguments — the declaration's hidden context and full parameter
// matrix surviving into C. Symbols/type IDs come from the real fixture dump
// (combine=27, n=28, flag=29, s=30, p=31, ptr=32, Pair struct type 19, main's
// v=37, p=38).
func TestEmitFunctionDeclarationHiddenContextAndSignatureWritesC(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, `
type Pair = struct { x int; y int; };
fn combine(n int, flag bool, s str, p Pair, ptr *int) int {
    var base int = n;
    if flag { base = base + 10; }
    base = base + (s.len as int);
    base = base + p.x + p.y;
    base = base + *ptr;
    return base;
}
fn main() int {
    var v int = 4;
    let p = Pair.{ x = 1, y = 2 };
    return combine(3, true, "hi", p, &v);
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int64_t pebble_fn_27(PebbleContext *ctx, int64_t pebble_local_28, bool pebble_local_29, PebbleStr pebble_local_30, pebble_struct_19_t pebble_local_31, int64_t * pebble_local_32);",
		"static int64_t pebble_fn_27(PebbleContext *ctx, int64_t pebble_local_28, bool pebble_local_29, PebbleStr pebble_local_30, pebble_struct_19_t pebble_local_31, int64_t * pebble_local_32) {",
		"    (void)pebble_local_28;",
		"    (void)pebble_local_29;",
		"    (void)pebble_local_30;",
		"    (void)pebble_local_31;",
		"    (void)pebble_local_32;",
		"return pebble_fn_27(ctx, 3LL, true, (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 }, pebble_local_38, (int64_t *)(&pebble_local_37));",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 22, false)
}

// TestEmitDirectCallExternSignatureMatrixCompilesAndRuns is the focused proof
// for proposal 14's "Direct call — V2 supports helper and extern direct calls
// — Implemented, proof needed for the signature matrix" row's extern half.
// It calls REAL libc functions so the whole pipeline compiles AND RUNS across
// four extern signature shapes not covered together before (existing extern
// tests separately covered uint/*void/void, str/*FILE, and i32): int param +
// int result (llabs — int is the 64-bit target-native word, so the matching
// libc absolute-value function is llabs, which takes and returns long long),
// one f64 param + f64 result (fabs), two f64 params + f64 result (pow), and a
// str param + uint result (strlen). Each call must lower to
// its real C name with no hidden context and no pebble_fn_ helper, and every
// typed result is consumed at its own width. The exit code is 3 + 5 + 7 + 2 =
// 17; a mis-lowered parameter or result width fails the process code. (The
// helper half of the same matrix — a direct call to a helper with multiple
// parameter types — is the combine(...) call in
// TestEmitFunctionDeclarationParameterResultMatrixCompilesAndRuns.)
func TestEmitDirectCallExternSignatureMatrixCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `
extern fn llabs(x int) int;
extern fn fabs(x f64) f64;
extern fn pow(x f64, y f64) f64;
extern fn strlen(s str) uint;
fn main() int {
    var total int = 0;
    total = total + llabs(-3);
    let r f64 = pow(2.0, 3.0);
    if r == 8.0 { total = total + 5; }
    let fl f64 = fabs(-2.5);
    if fl == 2.5 { total = total + 7; }
    total = total + (strlen("hi") as int);
    return total;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{"llabs(", "fabs(", "pow(", "strlen("} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C does not call the real C name %s:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_fn_") {
		t.Errorf("emitted C contains a pebble_fn_ helper for an extern, want none:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 3+5+7+2, false)
}

// compileAndRunWithShimFirst is compileAndRun for programs that call extern
// FUNCTIONS whose C definitions no included libc header declares (bool, char,
// narrow-width, and genuinely-mixed extern signatures): the backend emits no
// prototype for an extern function — the emitted C relies on the fixed libc
// header set — so the shim's definitions must be visible BEFORE the emitted
// C's call sites. The shim source is therefore concatenated ahead of the
// emitted C into one translation unit (unlike compileAndRunWithShim in
// extern_data_test.go, which links a separate shim translation unit for extern
// variables, whose forward declarations the emitted C provides itself). The
// combined file is compiled under the same -Wall -Wextra -Werror flags against
// the same cached runtime objects as compileEmittedC, and the binary must exit
// with wantCode.
func compileAndRunWithShimFirst(t *testing.T, emitted []byte, shimSource string, wantCode int) {
	t.Helper()
	requireCIntegration(t)
	cc, err := exec.LookPath("cc")
	if err != nil {
		t.Skipf("skipping end-to-end check: cc not on PATH (%v)", err)
	}
	dir := t.TempDir()
	combined := filepath.Join(dir, "combined.c")
	combinedSource := append([]byte(shimSource+"\n"), emitted...)
	if err := os.WriteFile(combined, combinedSource, 0o644); err != nil {
		t.Fatalf("write combined C: %v", err)
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
		combined,
	}
	for _, sourceFile := range runtimeSourceFiles {
		compileArgs = append(compileArgs, filepath.Join(objectsDir, strings.TrimSuffix(sourceFile, ".c")+".o"))
	}
	compileArgs = append(compileArgs, "-o", binary)
	compile := exec.Command(cc, compileArgs...)
	if output, err := compile.CombinedOutput(); err != nil {
		t.Fatalf("cc compilation failed: %v\n%s", err, output)
	}
	runCompiledBinary(t, binary, wantCode, false, false)
}

// --- proposal 14 audit: extern C signatures across accepted parameter/result types ---

// TestEmitExternCharAndMixedRealLibcCompilesAndRuns proves two accepted extern
// parameter/result families that the existing real-libc matrix did not cover:
// a `char` PARAMETER (putchar's real C signature is int putchar(int), and the
// char C ABI is int32_t, so the emitted `int32_t putchar(int32_t)` call agrees
// with the stdio.h declaration exactly — the returned int is compared against
// `'a' as i32`) and a genuinely mixed multi-parameter extern (memcpy takes
// *void + *void + uint and returns *void — three parameters of two families
// plus a pointer result, in one call). The memcpy buffers are scalar ints, not
// array locals: `&arr as *void` lowers through a broken statement-expression
// path (separately noted). Exit 0 = putchar returned 'a' and the copied value
// arrived; each other code names the exact step that failed.
func TestEmitExternCharAndMixedRealLibcCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `extern fn putchar(c char) i32;
extern fn memcpy(dest *void, src *void, n uint) *void;
fn main() int {
    var c char = 'a';
    var r i32 = putchar(c);
    if r != 'a' as i32 { return 1; }
    var src i32 = 10;
    var dst i32 = 0;
    var p *void = memcpy(&dst as *void, &src as *void, 4);
    if p == nil { return 2; }
    if dst != 10 { return 3; }
    return 0;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{"putchar(", "memcpy("} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C does not call the real C name %s:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_fn_") {
		t.Errorf("emitted C contains a pebble_fn_ helper for an extern, want none:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

// TestEmitExternSignatureFamilyShimCompilesAndRuns completes the accepted
// extern parameter/result families the real-libc tests cannot reach, because
// no included libc header declares them: bool → bool, char → char, narrow
// fixed-width integers (u8 → u8, i16 → i16), u64 → u64, f32 → f32, a void
// result with mixed params, and — the "multiple mixed parameters" shape — one
// extern taking i32 + f64 + str + *i32 and returning i64. Each declared extern
// is defined by a test-only C shim (the backend emits no prototype for an
// extern function, so the shim is concatenated AHEAD of the emitted C in one
// translation unit via compileAndRunWithShimFirst), and every parameter and
// result lands at its own C width: u8 emits uint8_t, i16 int16_t, u64
// uint64_t, f32 float, str const char *, i64 int64_t. Exit 0 = every
// signature shape lowered, linked, and round-tripped correctly; each other
// code names the exact step that failed.
func TestEmitExternSignatureFamilyShimCompilesAndRuns(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `extern fn shim_not(b bool) bool;
extern fn shim_next(c char) char;
extern fn shim_byte(x u8) u8;
extern fn shim_wide(a i16, b i16) i16;
extern fn shim_u64(x u64) u64;
extern fn shim_f32(x f32) f32;
extern fn shim_mix(a i32, b f64, s str, p *i32) i64;
extern fn shim_void_mixed(a int, b bool) void;
fn main() int {
    var nb bool = shim_not(true);
    if nb { return 1; }
    var nc char = shim_next('a');
    if nc != 'b' { return 2; }
    var nub u8 = shim_byte(200);
    if nub != 201 { return 3; }
    var nw i16 = shim_wide(1000, 2000);
    if nw != 3000 { return 4; }
    var nu u64 = shim_u64(18446744073709551615);
    if nu != 0 { return 5; }
    var nf f32 = shim_f32(1.5);
    if nf != 3.0 { return 6; }
    var x i32 = 40;
    var nm i64 = shim_mix(2, 1.5, "hi", &x);
    if nm != 193 { return 7; }
    shim_void_mixed(1, true);
    return 0;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"shim_not(", "shim_next(", "shim_byte(", "shim_wide(", "shim_u64(", "shim_f32(", "shim_mix(", "shim_void_mixed(",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C does not call the real C name %s:\n%s", want, out)
		}
	}
	if strings.Contains(out, "pebble_fn_") {
		t.Errorf("emitted C contains a pebble_fn_ helper for an extern, want none:\n%s", out)
	}
	compileAndRunWithShimFirst(t, buf.Bytes(), `#include <stdint.h>
#include <stdbool.h>
bool shim_not(bool b) { return !b; }
int32_t shim_next(int32_t c) { return c + 1; }
uint8_t shim_byte(uint8_t b) { return (uint8_t)(b + 1u); }
int16_t shim_wide(int16_t a, int16_t b) { return (int16_t)(a + b); }
uint64_t shim_u64(uint64_t x) { return x + 1u; }
float shim_f32(float x) { return x * 2.0f; }
int64_t shim_mix(int32_t a, double b, const char *s, int32_t *p) {
    return (int64_t)a + (int64_t)(b * 100.0) + (int64_t)(s[1] - s[0]) + (int64_t)(*p);
}
void shim_void_mixed(int32_t a, bool b) { (void)a; (void)b; }`, 0)
}

// --- proposal 14 audit: fixed-parameter value shapes ---

// TestEmitFixedParameterValueShapeMatrixCompilesAndRuns is the focused proof
// for proposal 14's "Fixed Pebble parameters — Implemented, proof needed by
// value shape" row. One program passes every required argument VALUE SHAPE to
// a helper's fixed (non-variadic) parameters, each with a distinguishable
// value, and the exit code is the arithmetic sum so a wrong, misplaced, or
// dropped argument fails the process code. sum5 receives, in order: a literal
// (1), a local variable reference (local = 2), the result of ANOTHER call (mk()
// = 3, the nested-call-as-argument shape), a struct-field read (p.x = 4), and
// a second struct-field read (p.y = 40) — sum5 = 50. The inline-construction
// shape is passed directly as an aggregate-typed argument: an inline struct
// literal (sumPoint(Point.{ x = 6, y = 7 }) = 13) and an inline tuple literal
// (sumPair((8, 9)) = 17). Exit code 80. A single struct-typed parameter
// receiving the inline literal also proves an aggregate value survived the
// call boundary field-by-field.
func TestEmitFixedParameterValueShapeMatrixCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Point = struct { x int; y int; };
fn mk() int { return 3; }
fn sum5(a int, b int, c int, d int, e int) int { return a + b + c + d + e; }
fn sumPoint(p Point) int { return p.x + p.y; }
fn sumPair(t (int, int)) int { return t.0 + t.1; }
fn main() int {
    let local int = 2;
    let p Point = Point.{ x = 4, y = 40 };
    return sum5(1, local, mk(), p.x, p.y) + sumPoint(Point.{ x = 6, y = 7 }) + sumPair((8, 9));
}`, false, 50+13+17, false)
}

// TestEmitFixedParameterValueShapeMatrixWritesC pins the lowered C for the
// same matrix: the call site threads ctx, then each argument value shape has
// its own lowered form — the literal as a plain constant, the local reference
// as its pebble_local_<sym> name, the nested call as a pebble_fn_<sym>(ctx)
// call (context threaded to it too), each struct-field read as the
// pebble_local_<p>.pebble_field_<member> projection, and the inline struct
// literal as a (pebble_struct_<id>_t){ .pebble_field_... = ... } compound
// literal. A wrong shape for any argument (e.g. the literal re-emitted as a
// local) fails the assertion.
func TestEmitFixedParameterValueShapeMatrixWritesC(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, `type Point = struct { x int; y int; };
fn mk() int { return 3; }
fn sum5(a int, b int, c int, d int, e int) int { return a + b + c + d + e; }
fn sumPoint(p Point) int { return p.x + p.y; }
fn sumPair(t (int, int)) int { return t.0 + t.1; }
fn main() int {
    let local int = 2;
    let p Point = Point.{ x = 4, y = 40 };
    return sum5(1, local, mk(), p.x, p.y) + sumPoint(Point.{ x = 6, y = 7 }) + sumPair((8, 9));
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"ctx, 1LL, pebble_local_",    // literal + local reference, both after ctx
		"pebble_fn_27(ctx), pebble_", // nested call (mk) threaded with ctx
		".pebble_field_",             // struct-field reads
		"(pebble_struct_19_t){",      // inline struct-literal argument
		"(pebble_tuple_",             // inline tuple-literal argument
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 50+13+17, false)
}

// --- proposal 14 audit: hidden context forwarding, nested and indirect ---

// TestEmitContextForwardingNestedCallChainCompilesAndRuns is the focused proof
// for proposal 14's "Hidden Pebble context forwarding — Implemented, proof
// needed for indirect and nested call chains" row's nested half. helperA calls
// helperB calls helperC, each a Pebble-convention helper (so the hidden
// context must thread through all three hops), and helperC actually USES the
// context: it binds `context`, reads its default_allocator, and performs a
// real alloc/write/read/free roundtrip that returns 42. If any hop re-fetched
// or defaulted the context instead of forwarding its own PebbleContext *ctx,
// the deepest allocator would not be the live runtime allocator and the
// roundtrip could not succeed. The companion WritesC test pins the per-hop
// `(ctx)` threading in the emitted C. Exit code 42.
func TestEmitContextForwardingNestedCallChainCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `fn helperC() int {
    let ctx = context;
    var p *i64 = (ctx.default_allocator.alloc)(ctx.default_allocator.ptr, 8) as *i64;
    *p = 42;
    let value = *p;
    (ctx.default_allocator.free)(ctx.default_allocator.ptr, p as *void);
    return value;
}
fn helperB() int { return helperC(); }
fn helperA() int { return helperB(); }
fn main() int { return helperA(); }`, false, 42, false)
}

// TestEmitContextForwardingNestedCallChainWritesC pins the emitted C for the
// three-hop chain: every helper definition declares the hidden PebbleContext
// *ctx parameter, every call site threads ctx as its first argument — main
// calls helperA(ctx), helperA's body calls helperB(ctx), helperB's body calls
// helperC(ctx) — and helperC's own `context` use lowers to the dereferenced
// (*ctx), never a re-fetched or freshly-constructed value. Each call passes
// the CURRENT helper's own ctx parameter (pebble_fn_26 -> pebble_fn_25 ->
// pebble_fn_24, the callee-before-caller post-order of the reachability
// walk). Symbols come from the real fixture dump (helperC=24, helperB=25,
// helperA=26, main=27).
func TestEmitContextForwardingNestedCallChainWritesC(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, `fn helperC() int {
    let ctx = context;
    var p *i64 = (ctx.default_allocator.alloc)(ctx.default_allocator.ptr, 8) as *i64;
    *p = 42;
    let value = *p;
    (ctx.default_allocator.free)(ctx.default_allocator.ptr, p as *void);
    return value;
}
fn helperB() int { return helperC(); }
fn helperA() int { return helperB(); }
fn main() int { return helperA(); }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int64_t pebble_fn_24(PebbleContext *ctx) {",
		"static int64_t pebble_fn_25(PebbleContext *ctx) {",
		"static int64_t pebble_fn_26(PebbleContext *ctx) {",
		"return pebble_fn_26(ctx);", // main -> helperA
		"return pebble_fn_25(ctx);", // helperA -> helperB
		"return pebble_fn_24(ctx);", // helperB -> helperC
		"= (*ctx);",                 // helperC's `context` use is the threaded ctx
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

// TestEmitContextForwardingIndirectCallCompilesAndRuns is the indirect half of
// the same row. apply takes a function-typed PARAMETER and calls through it
// (f(x)); the callee helper itself calls another Pebble-convention helper leaf,
// which USES the context (the allocator roundtrip). Context must thread across
// every hop: main -> apply (direct), apply -> helper (INDIRECT through the
// function-typed parameter — the boundary the row calls out), helper -> leaf
// (direct). If the indirect boundary dropped or corrupted ctx, leaf's
// allocator roundtrip could not succeed. helper(41) forwards to leaf(42), so
// the exit code is 42 — the exact value that round-tripped through the
// allocator at the deepest hop.
func TestEmitContextForwardingIndirectCallCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `fn leaf(x int) int {
    let ctx = context;
    var p *i64 = (ctx.default_allocator.alloc)(ctx.default_allocator.ptr, 8) as *i64;
    *p = x;
    let value = *p;
    (ctx.default_allocator.free)(ctx.default_allocator.ptr, p as *void);
    return value;
}
fn helper(x int) int { return leaf(x + 1); }
fn apply(f fn(int) int, x int) int { return f(x); }
fn main() int { return apply(helper, 41); }`, false, 42, false)
}

// --- proposal 14 audit: import/qualified paths, instance-method shapes, nested generic calls ---

// emitAndRunProvider is emitAndRun for a multi-file module fixture: it builds
// the module graph from the provider map, checks it, emits the C for the
// "main" entry, compiles, links against the runtime, and runs it expecting
// wantCode. This is the full pipeline proof — the source must pass the checker
// (module builder + symbol resolution + inference) AND the backend must emit C
// that cc accepts under -Wall -Wextra -Werror and runs to the right exit code.
func emitAndRunProvider(t *testing.T, provider fixtureProvider, wantCode int) {
	t.Helper()
	requireCIntegration(t)
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
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
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), wantCode, false)
}

// TestEmitImportedQualifiedValueFunctionTypePathsCompileAndRuns is the focused
// proof for proposal 14's "Import and qualified module value/function/type
// paths — Implemented, proof needed by symbol category" row. One imported
// module (lib.peb) declares every symbol category — a mutable module-level
// value (`var counter`), an immutable module-level value (`let constant`), a
// non-generic function (`mk`), a generic function (`identity`), a plain struct
// type (`Point`), and a two-level generic struct type (`Outer[T]` containing a
// `Wrap[T]` field) — and the root module resolves EACH category through a
// qualified `lib::` path: the mutable value is read, written through
// (`lib::counter = lib::counter + 1`), and read back, proving the write landed
// in the module's real shared storage rather than a copy (base=40, after=41);
// the immutable value is read (`lib::constant`); a typed binding names the
// imported struct type explicitly (`let p lib::Point`); construction uses the
// qualified generic type names (`lib::Outer[int].{ wrap = lib::Wrap[int].{ ... } }`);
// the non-generic imported function is called (`lib::mk()`); and the generic
// imported function is called with a NESTED qualified type argument
// (`lib::identity[lib::Outer[int]](o)`). The imported Point's method `sum` is
// also invoked through the qualified-constructed value. Exit code 124 =
// 40 + 41 + 30 + 5 + 6 + 2.
func TestEmitImportedQualifiedValueFunctionTypePathsCompileAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRunProvider(t, fixtureProvider{
		"main.peb": []byte(`import "./lib";
fn main() int {
    let base int = lib::counter;
    lib::counter = lib::counter + 1;
    let after int = lib::counter;
    let p lib::Point = lib::Point.{ x = 10, y = 20 };
    let q = lib::mk();
    let o lib::Outer[int] = lib::Outer[int].{ wrap = lib::Wrap[int].{ inner = 6 } };
    let o2 lib::Outer[int] = lib::identity[lib::Outer[int]](o);
    return base + after + p.sum() + q + o2.read() + lib::constant;
}`),
		"lib.peb": []byte(`var counter int = 40;
let constant int = 2;
type Point = struct { x int; y int; fn sum(self Point) int => self.x + self.y; };
type Wrap[T] = struct { inner T; };
type Outer[T] = struct { wrap Wrap[T]; fn read(self Outer[T]) T => self.wrap.inner; };
fn mk() int { return 5; }
fn identity[T](x T) T { return x; }`),
	}, 124)
}

// TestEmitInstanceMethodOwnerAndArgumentShapeMatrixCompilesAndRuns is the
// focused proof for proposal 14's "Instance method — Implemented, proof needed
// for all owner/value shapes" row. One program exercises a representative
// spread (not a full cross-product) of OWNER shapes — a plain struct value
// receiver (p.add), a plain struct pointer receiver (c.inc), a generic struct
// instance with a value receiver (b.get), and a generic struct instance with a
// pointer receiver (bp.bump) — and ARGUMENT shapes: a local reference (d), a
// call result (mk()), and a literal (3), plus a method result (g = b.get())
// reused as another method's argument (bp.bump(g)). Every sub-expression's
// contribution is summed so a wrong, misplaced, or dropped argument or a
// mis-dispatched receiver fails the exit code: p.add(d) + p.add(mk()) +
// p.add(3) = 108, c.inc() twice then c.read() = 42, bp.bump(g) = 1; total 151.
func TestEmitInstanceMethodOwnerAndArgumentShapeMatrixCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Point = struct { x int; fn add(self Point, delta int) int => self.x + delta; };
type Counter = struct { n int; fn inc(self *Counter) void { self.n = self.n + 1; } fn read(self Counter) int => self.n; };
type Box[T] = struct { value T; fn get(self Box[T]) T => self.value; fn bump(self *Box[T], k T) T { self.value = k; return self.value; } };
fn mk() int { return 100; }
fn main() int {
    let p Point = Point.{ x = 1 };
    let d int = 2;
    let sum_add = p.add(d) + p.add(mk()) + p.add(3);
    var c Counter = Counter.{ n = 40 };
    c.inc();
    c.inc();
    let b Box[int] = Box[int].{ value = 1 };
    let g = b.get();
    var bp Box[int] = Box[int].{ value = 0 };
    let r = bp.bump(g);
    return sum_add + c.read() + r;
}`, false, 151, false)
}

// TestEmitNestedGenericTypeArgumentCallCompilesAndRuns is the focused proof for
// proposal 14's "Generic call — V2 specializes named generic functions — proof
// needed for nested type arguments" row. The same generic function identity[T]
// is specialized at THREE nested type arguments in one program: Outer[int],
// Outer[bool] (a generic struct instantiation as the type argument), and
// Top[int] (a THREE-level nest Outer-free: Top[int] -> Mid[int] -> Deep[int]).
// pair[A, B] additionally carries a nested generic type argument in its SECOND
// position. Each call returns its whole nested aggregate value, so each
// specialization must thread the nested type argument through to the concrete
// field types or the .peek()/.read() reads dispatch to the wrong specialization
// and the exit code breaks: rb.peek() gates on Outer[bool]'s true, and the
// returned value is 5 + 7 + 5 = 17.
func TestEmitNestedGenericTypeArgumentCallCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Inner[T] = struct { val T; };
type Outer[K] = struct { inner Inner[K]; fn peek(self Outer[K]) K => self.inner.val; };
type Deep[A] = struct { a A; };
type Mid[B] = struct { d Deep[B]; };
type Top[C] = struct { m Mid[C]; fn read(self Top[C]) C => self.m.d.a; };
fn identity[T](x T) T { return x; }
fn pair[A, B](x A, y B) B { return y; }
fn main() int {
    let oi Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } };
    let ob Outer[bool] = Outer[bool].{ inner = Inner[bool].{ val = true } };
    let ri Outer[int] = identity[Outer[int]](oi);
    let rb Outer[bool] = identity[Outer[bool]](ob);
    let t Top[int] = Top[int].{ m = Mid[int].{ d = Deep[int].{ a = 7 } } };
    let three Top[int] = identity[Top[int]](t);
    let paired Outer[int] = pair[int, Outer[int]](1, oi);
    if rb.peek() { return ri.peek() + three.read() + paired.peek(); } else { return 0; }
}`, false, 17, false)
}

// TestEmitNestedGenericTypeArgumentCallWritesC pins the emitted-C shape for the
// nested-specialization proof: every identity[<nested>] / pair[..., <nested>]
// specialization's helper must carry the fully concrete substituted signature
// — never a residual "type-parameter(...)" anywhere in the output — so the
// nested type argument really threaded through specialization rather than
// leaking a symbolic parameter into the C.
func TestEmitNestedGenericTypeArgumentCallWritesC(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, `type Inner[T] = struct { val T; };
type Outer[K] = struct { inner Inner[K]; fn peek(self Outer[K]) K => self.inner.val; };
fn identity[T](x T) T { return x; }
fn pair[A, B](x A, y B) B { return y; }
fn main() int {
    let oi Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } };
    let ri Outer[int] = identity[Outer[int]](oi);
    let paired Outer[int] = pair[int, Outer[int]](1, oi);
    return ri.peek() + paired.peek();
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if strings.Contains(out, "type-parameter") {
		t.Errorf("emitted C still carries an unsubstituted type parameter in a nested type argument:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 10, false)
}

// TestEmitContextForwardingIndirectCallWritesC pins the emitted C for the
// indirect boundary: apply's body calls through the function-typed parameter
// as callee(ctx, arg) — the same `(ctx` threading a direct call gets — helper's
// body calls leaf with ctx, and main passes the bare pebble_fn_26 function
// value as apply's first argument (no ctx prepended to the VALUE itself, since
// it is not a call; the ctx is threaded when the value is invoked). Symbols
// come from the real fixture dump (leaf=24, helper=26, apply=28, f=29, x=30).
func TestEmitContextForwardingIndirectCallWritesC(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, `fn leaf(x int) int {
    let ctx = context;
    var p *i64 = (ctx.default_allocator.alloc)(ctx.default_allocator.ptr, 8) as *i64;
    *p = x;
    let value = *p;
    (ctx.default_allocator.free)(ctx.default_allocator.ptr, p as *void);
    return value;
}
fn helper(x int) int { return leaf(x + 1); }
fn apply(f fn(int) int, x int) int { return f(x); }
fn main() int { return apply(helper, 41); }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"return pebble_fn_28(ctx, pebble_fn_26, 41LL);", // main -> apply, helper passed as a bare value
		"return pebble_local_29(ctx, pebble_local_30);", // apply's INDIRECT call threads ctx through the fn-typed param
		"return pebble_fn_24(ctx,",                      // helper -> leaf, ctx threaded
		"= (*ctx);",                                     // leaf's `context` use is the threaded ctx
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}
