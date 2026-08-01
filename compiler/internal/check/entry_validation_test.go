package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

func entrySymbol(t *testing.T, inputs Inputs, name string) symbol.SymbolID {
	t.Helper()
	for _, candidate := range inputs.Resolution.Symbols.All() {
		if candidate.Name == name {
			return candidate.ID
		}
	}
	t.Fatalf("missing symbol %q", name)
	return 0
}

func TestRun06bAcceptsConfiguredVoidAndIntEntries(t *testing.T) {
	for _, test := range []struct {
		name     string
		typeName string
		body     string
	}{
		{name: "void", typeName: "void", body: ""},
		{name: "int", typeName: "int", body: "return 0;"},
	} {
		t.Run(test.name, func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn entry() " + test.typeName + " {" + test.body + "}")})
			id := entrySymbol(t, inputs, "entry")
			handoff := run06a(inputs, diagnostics, Config{})
			result := run06b(handoff, diagnostics, Config{Entry: EntryPoint{Mode: EntryRequired, Symbol: id}}, inputs.Types)
			if !result.Successful() || hasValidationDiagnostic(diagnostics, CodeEntryPoint) {
				t.Fatalf("entry rejected: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
			}
		})
	}
}

func TestRun06bRejectsIneligibleConfiguredEntries(t *testing.T) {
	for _, test := range []struct {
		name   string
		source string
	}{
		{"method", `type Box = struct { fn entry(self Box) void {} };`},
		{"generic", `fn entry[T](value T) void {}`},
		{"variadic_wrong_result", `fn entry(value i32) i64 { return 1; }`},
		{"extern", `extern fn entry() void;`},
		{"parameters", `fn entry(value i32) void {}`},
		{"wrong_result", `fn entry() i32 { return 0; }`},
	} {
		t.Run(test.name, func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(test.source)})
			id := entrySymbol(t, inputs, "entry")
			handoff := run06a(inputs, diagnostics, Config{})
			result := run06b(handoff, diagnostics, Config{Entry: EntryPoint{Mode: EntryRequired, Symbol: id}}, inputs.Types)
			if result.Successful() || !hasValidationDiagnostic(diagnostics, CodeEntryPoint) {
				t.Fatalf("%s: ineligible entry was accepted: result=%v diagnostics=%+v", test.name, result.Successful(), diagnostics.Items())
			}
		})
	}
}

func TestRun06bRejectsNonRootModuleEntry(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{
		"main.peb": []byte(`import "./lib";`),
		"lib.peb":  []byte(`fn entry() void {}`),
	})
	id := entrySymbol(t, inputs, "entry")
	handoff := run06a(inputs, diagnostics, Config{})
	result := run06b(handoff, diagnostics, Config{Entry: EntryPoint{Mode: EntryRequired, Symbol: id}}, inputs.Types)
	if result.Successful() || !hasValidationDiagnostic(diagnostics, CodeEntryPoint) {
		t.Fatalf("non-root entry was accepted: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
}

func TestValidateEntryNoneSkipsInvalidConfiguredSymbol(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn entry() i32 { return 0; }")})
	handoff := run06a(inputs, diagnostics, Config{})
	records, ok := resolveRecords(handoff, diagnostics, Config{})
	if !ok {
		t.Fatal("failed to resolve records")
	}
	set := diagnostic.NewDiagnosticSet()
	if !validateEntry(handoff, records, nil, set, Config{Entry: EntryPoint{Mode: EntryNone, Symbol: 0}}) || set.Len() != 0 {
		t.Fatalf("EntryNone performed validation: diagnostics=%+v", set.Items())
	}
}

func TestValidateEntryRejectsUnresolvedSymbolAndRequirements(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn entry() void {}")})
	handoff := run06a(inputs, diagnostics, Config{})
	records, ok := resolveRecords(handoff, diagnostics, Config{})
	if !ok {
		t.Fatal("failed to resolve records")
	}
	set := diagnostic.NewDiagnosticSet()
	if validateEntry(handoff, records, nil, set, Config{Entry: EntryPoint{Mode: EntryRequired, Symbol: 0}}) || !hasValidationDiagnostic(set, CodeEntryPoint) {
		t.Fatalf("unresolved entry was accepted: diagnostics=%+v", set.Items())
	}

	id := entrySymbol(t, inputs, "entry")
	set = diagnostic.NewDiagnosticSet()
	requirements := map[symbol.SymbolID][]Requirement{id: {{Owner: id, Kind: RequirementNumeric}}}
	if validateEntry(handoff, records, requirements, set, Config{Entry: EntryPoint{Mode: EntryRequired, Symbol: id}}) || !hasValidationDiagnostic(set, CodeEntryPoint) {
		t.Fatalf("entry with requirements was accepted: diagnostics=%+v", set.Items())
	}
}
