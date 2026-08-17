package backend

import (
	"bytes"
	"os"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestCrossModuleGenericHigherOrderNamedFunctionValueCompileAndRun(t *testing.T) {
	t.Parallel()
	requireCIntegration(t)
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb": []byte(`import "./helper";
fn is_positive(x i32) bool { return x > 0; }
fn main() int { var items [1]i32 = [21]; var result = helper::apply(items, is_positive); if result[0] { return 0; } return 1; }
`),
		"helper.peb": []byte(`fn apply[T, Ret](items [1]T, operation fn(T) Ret) [1]Ret {
    return [operation(items[0])];
}
`),
	}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entryID = candidate.ID
			break
		}
	}
	outcome := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !outcome.Successful() {
		t.Fatalf("check failed: %+v", diagnostics.Items())
	}
	var buf bytes.Buffer
	if err := Emit(outcome.IR(), outcome.IR().Snapshot(), entryID, sources, resolution, &buf); err != nil {
		t.Fatal(err)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

func TestStdFuncExternalNamedFunctionValueCompileAndRun(t *testing.T) {
	t.Parallel()
	requireCIntegration(t)
	funcSource, err := os.ReadFile("../../std/func.peb")
	if err != nil {
		t.Fatal(err)
	}
	memSource, err := os.ReadFile("../../std/mem.peb")
	if err != nil {
		t.Fatal(err)
	}
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb": []byte(`import "./helper"; import "std:func"; import "std:mem";
fn main() int { var items = mem::new_slice[i32](1); items[0] = 21; var result = func::filter(items, helper::is_positive); if result.len == 1 { return 0; } return 1; }
`),
		"helper.peb": []byte(`fn is_positive(x i32) bool { return x > 0; }
`),
		"std/func.peb": funcSource,
		"std/mem.peb":  memSource,
	}
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
			break
		}
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed: %+v", diagnostics.Items())
	}
	var buf bytes.Buffer
	if err := Emit(result.IR(), result.IR().Snapshot(), entryID, sources, resolution, &buf); err != nil {
		t.Fatal(err)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}
