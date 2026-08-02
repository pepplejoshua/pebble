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
	cc, err := exec.LookPath("cc")
	if err != nil {
		t.Skipf("skipping end-to-end check: cc not on PATH (%v)", err)
	}
	unit, snapshot, entryID := buildFixture(t, "fn main() void {}", "main", true)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}

	dir := t.TempDir()
	program := filepath.Join(dir, "program.c")
	if err := os.WriteFile(program, buf.Bytes(), 0o644); err != nil {
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
		"-o", binary,
	)
	if output, err := compile.CombinedOutput(); err != nil {
		t.Fatalf("cc compilation failed: %v\n%s", err, output)
	}

	run := exec.Command(binary)
	output, err := run.CombinedOutput()
	if err != nil {
		t.Fatalf("compiled program failed to run: %v\n%s", err, output)
	}
	if code := run.ProcessState.ExitCode(); code != 0 {
		t.Fatalf("compiled program exited %d, want 0\n%s", code, output)
	}
	t.Logf("compiled program exited 0")
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

func TestEmitRejectsParameters(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main(args []str) void {}", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNonVoidResult(t *testing.T) {
	unit, snapshot, entryID := buildFixture(t, "fn main() i32 { return 0; }", "main", false)
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
