package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// genericSignatureFixture compiles one source program, locates the named
// generic function's own retained callableRecord, and collects every concrete
// instantiation of it solved at a real call site. The same factInputs/run06a/
// resolveRecords spine every other check test uses; it never reaches run06b,
// which is not part of this slice's job.
func genericSignatureFixture(t *testing.T, source, name string) (*types.Store, *solveHandoff, *solvedRecords, *callableRecord, []infer.Instantiation) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	var generic symbol.SymbolID
	for _, candidate := range inputs.Resolution.Symbols.All() {
		if candidate.Name == name && candidate.Kind == symbol.SymbolFunction {
			generic = candidate.ID
			break
		}
	}
	if generic == 0 {
		t.Fatalf("missing generic function %q", name)
	}
	var callable *callableRecord
	for _, retained := range handoff.Records.Records() {
		if retained.Callable != nil && retained.Callable.Symbol == generic {
			callable = retained.Callable
			break
		}
	}
	if callable == nil {
		t.Fatalf("missing callable record for %q", name)
	}
	var instantiations []infer.Instantiation
	for _, inst := range handoff.Solution.Instantiations() {
		if inst.Generic == generic {
			instantiations = append(instantiations, inst)
		}
	}
	if len(instantiations) == 0 {
		t.Fatalf("no instantiation of %q solved at a call site", name)
	}
	return inputs.Types, handoff, records, callable, instantiations
}

func TestBuildSpecializedSignatureSingleTypeParameter(t *testing.T) {
	store, handoff, records, callable, instantiations := genericSignatureFixture(t, `
fn identity[T](value T) T => value;
let inferred i32 = identity(1);
`, "identity")
	if len(instantiations) != 1 {
		t.Fatalf("instantiations = %d, want 1", len(instantiations))
	}
	got, err := buildSpecializedSignature(store, handoff, records, callable, instantiations[0])
	if err != nil {
		t.Fatalf("buildSpecializedSignature: %v", err)
	}
	if len(got.Parameters) != 1 {
		t.Fatalf("substituted parameters = %d, want 1", len(got.Parameters))
	}
	if got.Parameters[0] != store.Builtins().I32 {
		t.Fatalf("substituted parameter = %d, want i32 %d", got.Parameters[0], store.Builtins().I32)
	}
	if got.Result != store.Builtins().I32 {
		t.Fatalf("substituted result = %d, want i32 %d", got.Result, store.Builtins().I32)
	}
}

func TestBuildSpecializedSignatureTwoTypeParameters(t *testing.T) {
	store, handoff, records, callable, instantiations := genericSignatureFixture(t, `
fn pair[T, U](a T, b U) T => a;
let paired i32 = pair(1, 'x');
`, "pair")
	if len(instantiations) != 1 {
		t.Fatalf("instantiations = %d, want 1", len(instantiations))
	}
	got, err := buildSpecializedSignature(store, handoff, records, callable, instantiations[0])
	if err != nil {
		t.Fatalf("buildSpecializedSignature: %v", err)
	}
	if len(got.Parameters) != 2 {
		t.Fatalf("substituted parameters = %d, want 2", len(got.Parameters))
	}
	if got.Parameters[0] != store.Builtins().I32 {
		t.Fatalf("first parameter = %d, want i32 %d", got.Parameters[0], store.Builtins().I32)
	}
	if got.Parameters[1] != store.Builtins().Char {
		t.Fatalf("second parameter = %d, want char %d", got.Parameters[1], store.Builtins().Char)
	}
	if got.Result != store.Builtins().I32 {
		t.Fatalf("substituted result = %d, want i32 %d", got.Result, store.Builtins().I32)
	}
}

func TestBuildSpecializedSignatureCompositeParameter(t *testing.T) {
	store, handoff, records, callable, instantiations := genericSignatureFixture(t, `
fn first[T](items []T) T => items[0];
let values []i32 = [1, 2, 3];
let result i32 = first(values);
`, "first")
	if len(instantiations) != 1 {
		t.Fatalf("instantiations = %d, want 1", len(instantiations))
	}
	got, err := buildSpecializedSignature(store, handoff, records, callable, instantiations[0])
	if err != nil {
		t.Fatalf("buildSpecializedSignature: %v", err)
	}
	if len(got.Parameters) != 1 {
		t.Fatalf("substituted parameters = %d, want 1", len(got.Parameters))
	}
	wantSlice, err := store.Intern(types.SliceKey(store.Builtins().I32))
	if err != nil {
		t.Fatal(err)
	}
	if got.Parameters[0] != wantSlice {
		key, _ := store.Key(got.Parameters[0])
		t.Fatalf("substituted parameter = %d (kind %v), want []i32 %d", got.Parameters[0], key, wantSlice)
	}
	if got.Result != store.Builtins().I32 {
		t.Fatalf("substituted result = %d, want i32 %d", got.Result, store.Builtins().I32)
	}
}

func TestBuildSpecializedSignatureRejectsMalformedInstantiation(t *testing.T) {
	store, handoff, records, callable, instantiations := genericSignatureFixture(t, `
fn identity[T](value T) T => value;
let inferred i32 = identity(1);
`, "identity")
	base := instantiations[0]

	mismatched := base
	mismatched.Arguments = append([]infer.TypeResult(nil), base.Arguments...)
	mismatched.Arguments = mismatched.Arguments[:0]
	if _, err := buildSpecializedSignature(store, handoff, records, callable, mismatched); err == nil {
		t.Fatal("mismatched-arity (too few) instantiation accepted")
	}

	overflow := base
	overflow.Arguments = append(append([]infer.TypeResult(nil), base.Arguments...), infer.TypeResult{State: infer.TypeFinal, Type: store.Builtins().I32})
	if _, err := buildSpecializedSignature(store, handoff, records, callable, overflow); err == nil {
		t.Fatal("mismatched-arity (too many) instantiation accepted")
	}

	unresolved := base
	unresolved.Arguments = append([]infer.TypeResult(nil), base.Arguments...)
	unresolved.Arguments[0] = infer.TypeResult{State: infer.TypeError}
	if _, err := buildSpecializedSignature(store, handoff, records, callable, unresolved); err == nil {
		t.Fatal("non-final instantiation argument accepted")
	}
}
