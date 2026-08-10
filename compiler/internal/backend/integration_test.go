package backend

import (
	"bytes"
	"os"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestCheckStdMemImport(t *testing.T) {
	t.Parallel()
	unit, _, _, _ := buildStdMemFixture(t, `import "std:mem"; fn main() i32 { return 0; }`, "main")
	if unit == nil {
		t.Fatal("std:mem import produced no IR")
	}
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

func TestCheckStdHmapRehash(t *testing.T) {
	t.Parallel()
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
