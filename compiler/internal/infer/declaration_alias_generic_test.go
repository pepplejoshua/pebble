package infer

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// TestPrepareDeclarationsAliasToCrossModuleGenericInstantiation is a
// regression test for a real bug: a function whose declared result type is
// a local type alias to a generic instantiation imported from ANOTHER
// module previously never reached DeclarationReady, cascading into a
// misleading checker C0604 "callable declaration is invalid" at a much
// later stage (declaration_facts.go's handleNamedCallable skips setting the
// callable record's Convention when the signature isn't ready).
//
// Root cause: prepareDeclarations processed every type declaration in ONE
// pass (sorted by SymbolID), interleaving alias resolution with generic
// nominal constructor template registration. When the alias's SymbolID
// sorted BEFORE the generic it referenced (which happens naturally for a
// cross-module reference, since symbols from an imported module are
// resolved after the importing module's own top-level declarations), the
// alias tried to apply the generic's type constructor before that
// constructor's own template was ever registered, so applyTypeConstructor
// returned 0 and the alias - and therefore any function using it as a
// result type - never resolved.
//
// The fix splits the single pass into two: register every generic nominal
// constructor template first, then resolve aliases in a second pass, so an
// alias can always find its referenced generic's template regardless of
// module/SymbolID ordering.
func TestPrepareDeclarationsAliasToCrossModuleGenericInstantiation(t *testing.T) {
	provider := inferenceMemoryProvider{
		"main.peb": []byte(`import "./dep"; type StatsResult = dep::Result[int, str]; fn count_file(filename str) StatsResult { return dep::result_ok[int, str](5); } fn main() int { return 0; }`),
		"dep.peb": []byte(`type Result[T, E] = union enum { Ok T; Err E; };
fn result_ok[T, E](value T) Result[T, E] { return Result[T, E].{ Ok = value }; }`),
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "declalias"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	program := Prepare(ProgramInputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, ArrayLengths: fixedArrayLengths{}, LiteralTarget: LiteralTarget{WordBits: 64}}, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}

	var countFile symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Kind == symbol.SymbolFunction && candidate.Name == "count_file" {
			countFile = candidate.ID
		}
	}
	if countFile == 0 {
		t.Fatal("count_file symbol not found")
	}

	signature, ok := program.Signature(countFile)
	if !ok || signature.State != DeclarationReady {
		t.Fatalf("count_file signature not ready: ok=%v signature=%+v", ok, signature)
	}
	if signature.Convention != types.Pebble {
		t.Fatalf("count_file signature convention = %v, want Pebble", signature.Convention)
	}
	if signature.Result == 0 {
		t.Fatal("count_file signature has no result template")
	}
}

// TestPrepareDeclarationsAliasToSameModuleGenericInstantiation is the
// same-module regression guard: this shape already worked before the fix
// (the generic's SymbolID naturally sorts before an alias declared later
// in the same file that references it), and must keep working exactly the
// same afterward.
func TestPrepareDeclarationsAliasToSameModuleGenericInstantiation(t *testing.T) {
	provider := inferenceMemoryProvider{
		"main.peb": []byte(`type Result[T, E] = union enum { Ok T; Err E; };
type IntResult = Result[int, str];
fn count_file(filename str) IntResult { return Result[int, str].{ Ok = 5 }; }
fn main() int { return 0; }`),
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "declalias"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	program := Prepare(ProgramInputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, ArrayLengths: fixedArrayLengths{}, LiteralTarget: LiteralTarget{WordBits: 64}}, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}

	var countFile symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Kind == symbol.SymbolFunction && candidate.Name == "count_file" {
			countFile = candidate.ID
		}
	}
	if countFile == 0 {
		t.Fatal("count_file symbol not found")
	}

	signature, ok := program.Signature(countFile)
	if !ok || signature.State != DeclarationReady || signature.Convention != types.Pebble {
		t.Fatalf("count_file signature not ready: ok=%v signature=%+v", ok, signature)
	}
}
