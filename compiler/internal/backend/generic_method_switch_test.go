package backend

import (
	"bytes"
	"os"
	"regexp"
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

// F5-01 — a generic tagged union's own NON-generic method switching on `self`.
// A method like `fn is_ok(self Result[T, E]) bool` declares no type parameters
// of its own, so the resolver does not mark it Generic; before the checker fix,
// such a method never got a concrete FunctionDeclaration specialization the way
// a free generic function or a generic method (one redeclaring type parameters,
// like `map[U]`) does. Its body kept the symbolic containing type (`Choice[T]`,
// `Result[T, E]`), and the backend's switch-subject builder — which
// distinguishes a tagged union from a plain enum by membership in the collected
// `unions` map, keyed by CONCRETE type IDs — found the symbolic type missing,
// fell through to the plain-enum path, and emitted `switch (pebble_local_<id>)`
// on the whole union struct instead of `switch (pebble_local_<id>.tag)`. cc
// rejected that with "statement requires expression of integer type".
//
// These tests prove the fix in isolation (a small local generic union, no
// std:result dependency) and through the real std library: a non-generic
// method switching on `self` emits the `.tag` discriminant projection and
// compiles/runs, and a sibling method that reads the union payload in a
// narrowed arm (`self.A` inside `case .A:`) is equally concrete end to end.

// TestEmitGenericUnionMethodSwitchOnSelfCompileAndRun proves a non-generic
// method on a generic tagged union whose body switches on `self` compiles and
// runs: is_a dispatches on the `.tag` discriminant, so a Choice[int] holding A
// returns true.
func TestEmitGenericUnionMethodSwitchOnSelfCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Choice[T] = union enum { A T; B void; fn is_a(self Choice[T]) bool { switch self { case .A: return true; case .B: return false; } } };
fn main() int { var c Choice[int] = Choice[int].{ A = 5 }; if !c.is_a() { return 1; } return 0; }`, false, 0, false)
}

// TestEmitGenericUnionMethodSwitchOnSelfAndPayloadReadCompileAndRun proves the
// same specialized body is fully concrete beyond the switch subject: a second
// non-generic method reads the union's payload in a narrowed arm
// (`return self.A;` inside `case .A:`) and returns it, which exercises the
// union-type collection's FieldPlace path on the concrete instantiation. An
// exit code of 0 requires both the `.tag` switch dispatch and the payload read
// to be correct.
func TestEmitGenericUnionMethodSwitchOnSelfAndPayloadReadCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Choice[T] = union enum { A T; B void; fn is_a(self Choice[T]) bool { switch self { case .A: return true; case .B: return false; } } fn value_or(self Choice[T], def T) T { switch self { case .A: return self.A; case .B: return def; } } };
fn main() int { var c Choice[int] = Choice[int].{ A = 5 }; if !c.is_a() { return 1; } if c.value_or(0) != 5 { return 2; } return 0; }`, false, 0, false)
}

// TestEmitGenericUnionMethodSwitchOnSelfEmitsTagProjection inspects the emitted
// C text directly: every switch subject that references a union-typed local
// must be the union's `.tag` discriminant, never the whole union struct. This
// is the exact failure mode the checker fix closes (the pre-fix output was
// `switch (pebble_local_...)` on the raw struct, which cc rejects).
func TestEmitGenericUnionMethodSwitchOnSelfEmitsTagProjection(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, `type Choice[T] = union enum { A T; B void; fn is_a(self Choice[T]) bool { switch self { case .A: return true; case .B: return false; } } };
fn main() int { var c Choice[int] = Choice[int].{ A = 5 }; if c.is_a() { return 0; } return 1; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	subject := regexp.MustCompile(`switch \(pebble_local_[0-9]+[^)]*\)`)
	matches := subject.FindAllString(out, -1)
	if len(matches) == 0 {
		t.Fatalf("emitted C has no union-typed switch subject:\n%s", out)
	}
	for _, match := range matches {
		if !strings.HasSuffix(match, ".tag)") {
			t.Errorf("emitted C switch subject %q is not a .tag projection:\n%s", match, out)
		}
	}
}

// emitAndRunStdResult drives one entry source that imports "std:result" through
// the full pipeline against the real std/result.peb module and asserts the
// process exit code. It mirrors buildStdMemFixture's multi-module fixture
// wiring (StandardRoot "std" with the std module served from disk).
func emitAndRunStdResult(t *testing.T, sourceText string, wantCode int) {
	t.Helper()
	requireCIntegration(t)
	result, err := os.ReadFile("../../std/result.peb")
	if err != nil {
		t.Fatal(err)
	}
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{"main.peb": []byte(sourceText), "std/result.peb": result}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
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
	outcome := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !outcome.Successful() {
		t.Fatalf("check failed: %+v", diagnostics.Items())
	}
	unit := outcome.IR()
	if unit == nil {
		t.Fatal("check succeeded without an IR unit")
	}
	var buf bytes.Buffer
	if err := Emit(unit, unit.Snapshot(), entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), wantCode, false)
}

// TestEmitStdResultIsOkCompileAndRun is the real std:result end-to-end repro
// from the F5-01 audit: import std:result, construct a Result[int, str] via
// result_ok, and call is_ok() — a non-generic method switching on `self` that
// used to emit invalid C (`switch (pebble_local_<id>)` instead of
// `switch (pebble_local_<id>.tag)`). The entry returns 0 when is_ok is true.
func TestEmitStdResultIsOkCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRunStdResult(t, `import "std:result";
fn main() int { var r result::Result[int, str] = result::result_ok[int, str](5); if r.is_ok() { return 0; } return 1; }`, 0)
}

// TestEmitStdResultIsOkAndUnwrapOrCompileAndRun exercises two of the audit's
// three named non-generic methods end to end: is_ok() and unwrap_or(). The
// latter also reads the Ok payload in a narrowed switch arm (`return self.Ok;`),
// so it covers the concrete instantiation's payload read, not just the tag
// dispatch. map() is deliberately not forced here: its Result[U, E] return type
// instantiates Result twice in one program, which trips a SEPARATE, pre-existing
// backend limitation (the discriminant enumerators pebble_variant_<variant>
// collide across the two union typedefs), reported rather than papered over.
func TestEmitStdResultIsOkAndUnwrapOrCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRunStdResult(t, `import "std:result";
fn main() int { var r result::Result[int, str] = result::result_ok[int, str](5); if !r.is_ok() { return 1; } var v int = r.unwrap_or(0); if v != 5 { return 2; } return 0; }`, 0)
}
