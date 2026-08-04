package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestSliceFromRawStdPackage(t *testing.T) {
	result, diagnostics := checkSliceFixture(t, module.StandardPackage, "fn main() i32 { var value i32 = 7; var ptr *i32 = &value; let values []i32 = slice ptr, 1; return values[0]; }")
	if !result.Successful() {
		t.Fatalf("check failed: %v", diagnostics.Items())
	}
	found := false
	for _, node := range result.IR().Nodes() {
		if node.Kind == tir.SliceFromRaw {
			found = true
			if len(node.Children) != 2 {
				t.Fatalf("SliceFromRaw children = %d, want 2", len(node.Children))
			}
		}
	}
	if !found {
		t.Fatal("SliceFromRaw node not found")
	}
}

func TestSliceFromRawRejectsNonStandardPackage(t *testing.T) {
	result, diagnostics := checkSliceFixture(t, "app", "fn main() i32 { var value i32 = 7; var ptr *i32 = &value; let values []i32 = slice ptr, 1; return values[0]; }")
	if result.Successful() || len(diagnostics.Items()) == 0 {
		t.Fatal("slice outside std package was accepted")
	}
}

func TestSliceFromRawRejectsNonPointer(t *testing.T) {
	result, diagnostics := checkSliceFixture(t, module.StandardPackage, "fn main() i32 { let value i32 = 7; let values []i32 = slice value, 1; return values[0]; }")
	if result.Successful() || len(diagnostics.Items()) == 0 {
		t.Fatal("slice with non-pointer operand was accepted")
	}
}

func checkSliceFixture(t *testing.T, packageID module.PackageID, text string) (*Result, *diagnostic.DiagnosticSet) {
	t.Helper()
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := checkProvider{"main.peb": []byte(text)}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: packageID}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	return Check(Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, Config{}), diagnostics
}
