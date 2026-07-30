package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

func TestBuildUnitDeclarations(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let answer i32 = 1;\nfn main(value i32) i32 => value;\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, Config{})
	if !ok || unit == nil {
		t.Fatal("buildUnit rejected valid handoff")
	}
	if len(unit.Modules()) != 1 || len(unit.FunctionDeclarations()) != 1 || len(unit.GlobalDeclarations()) != 1 {
		t.Fatalf("unexpected containers: modules=%d funcs=%d globals=%d", len(unit.Modules()), len(unit.FunctionDeclarations()), len(unit.GlobalDeclarations()))
	}
	seenFunction, seenGlobal := false, false
	for _, n := range unit.Nodes() {
		switch n.Kind {
		case tir.FunctionDeclaration:
			seenFunction = true
		case tir.GlobalDeclaration:
			seenGlobal = true
		}
	}
	if !seenFunction || !seenGlobal {
		t.Fatal("declaration nodes missing")
	}
}

func TestBuildUnitLocalDeclaration(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void { var local i32 = 1; }\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, Config{})
	if !ok || unit == nil {
		t.Fatal("buildUnit rejected valid handoff")
	}

	var local *bindingRecord
	for _, retained := range handoff.Records.Records() {
		if retained.Binding != nil {
			sym, exists := inputs.Resolution.Symbols.Symbol(retained.Binding.Symbol)
			if exists && sym.Name == "local" {
				local = retained.Binding
				break
			}
		}
	}
	if local == nil {
		t.Fatal("local binding record missing")
	}
	typ, ok := typeOfValue(records, local.Annotation)
	if !ok || typ != inputs.Types.Builtins().I32 {
		t.Fatalf("local binding has type %v, want i32", typ)
	}
	for _, n := range unit.Nodes() {
		if n.Kind == tir.LocalDeclaration && n.Symbol == local.Symbol {
			return
		}
	}
	t.Fatal("local declaration node missing")
}

func TestBuildUnitRejectsGenerationErrors(t *testing.T) {
	unit, ok := buildUnit(&solveHandoff{GenerationHadErrors: true}, nil, nil, Config{})
	if ok || unit != nil {
		t.Fatal("expected failed generation handoff to be rejected")
	}
}
