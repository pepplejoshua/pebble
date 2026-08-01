package check

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

type recoveryFixture struct {
	name             string
	path             string
	code             diagnostic.Code
	checkDiagnostics func(*testing.T, []diagnostic.Diagnostic)
	check            func(*testing.T, *solveHandoff, *Result)
}

func TestValidationRecovery(t *testing.T) {
	fixtures := []recoveryFixture{
		{
			name:  "global_then_place",
			path:  "../../../tests/check/validation/recovery/integration_global_then_valid.peb",
			code:  CodeBindingInitializer,
			check: checkRecoveredPlace,
		},
		{
			name:  "conversion_then_call",
			path:  "../../../tests/check/validation/recovery/integration_conversion_then_valid.peb",
			code:  CodeConversion,
			check: checkRecoveredCall,
		},
		{
			name:             "broken_valid_broken_valid",
			path:             "../../../tests/check/validation/recovery/integration_chain.peb",
			code:             CodeMissingReturn,
			checkDiagnostics: checkTwoMissingReturns,
			check:            checkRecoveredMember,
		},
		{
			name:  "ir_validator_gate",
			path:  "../../../tests/check/ir/recovery/validator_error_with_valid_ir.peb",
			code:  CodeBindingInitializer,
			check: checkRecoveredPlace,
		},
		{
			name:  "ir_generation_gate",
			path:  "../../../tests/check/ir/recovery/generation_then_gate.peb",
			code:  CodeConversion,
			check: checkRecoveredCall,
		},
	}

	for _, fixture := range fixtures {
		fixture := fixture
		t.Run(fixture.name, func(t *testing.T) {
			source, err := os.ReadFile(filepath.Clean(fixture.path))
			if err != nil {
				t.Fatal(err)
			}
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": source})
			// Keep the handoff only to locate authored refs; the assertion under
			// test is always the public Check result below.
			inspectDiagnostics := diagnostics
			handoff := run06a(inputs, inspectDiagnostics, Config{})
			if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
				t.Fatalf("06a did not produce a complete handoff: %+v", inspectDiagnostics.Items())
			}

			checkDiagnostics := newDiagnosticSet()
			result := Check(inputs, checkDiagnostics, Config{})
			if result.Successful() {
				t.Fatal("recovery fixture unexpectedly succeeded")
			}
			if result.IR() != nil {
				t.Fatal("failed recovery result must not publish IR")
			}
			items := checkDiagnostics.Items()
			if fixture.checkDiagnostics != nil {
				fixture.checkDiagnostics(t, items)
			} else if len(items) != 1 || items[0].Code != fixture.code {
				t.Fatalf("diagnostics = %+v, want exactly one %s diagnostic", items, fixture.code)
			}
			fixture.check(t, handoff, result)
		})
	}
}

func checkTwoMissingReturns(t *testing.T, items []diagnostic.Diagnostic) {
	t.Helper()
	if len(items) != 2 || items[0].Code != CodeMissingReturn || items[1].Code != CodeMissingReturn {
		t.Fatalf("diagnostics = %+v, want two %s diagnostics", items, CodeMissingReturn)
	}
	if items[0].Primary.Span == items[1].Primary.Span {
		t.Fatalf("diagnostics = %+v, want distinct missing-return spans", items)
	}
}

func newDiagnosticSet() *diagnostic.DiagnosticSet { return diagnostic.NewDiagnosticSet() }

func checkRecoveredPlace(t *testing.T, handoff *solveHandoff, result *Result) {
	t.Helper()
	ref := requirePlaceRef(t, handoff, func(place *placeRecord) bool {
		return place.RootKind == symbol.SymbolBinding && place.RootMutable
	})
	value, ok := result.Place(ref)
	if !ok || value.Type.State != infer.TypeFinal || value.Type.Type == 0 || !value.Writable {
		t.Fatalf("recovered place = %+v, found=%v; want final writable typed place", value, ok)
	}
}

func checkRecoveredCall(t *testing.T, handoff *solveHandoff, result *Result) {
	t.Helper()
	sink := findSymbolID(t, handoff, "recovered_sink", symbol.SymbolFunction)
	ref := requireCallRef(t, handoff, func(call *callRecord) bool {
		return call.Target.Kind == callDirect && call.Target.Symbol == sink
	})
	value, ok := result.Call(ref)
	if !ok || value.Symbol != sink || len(value.Arguments) != 1 || value.Arguments[0].Source == 0 {
		t.Fatalf("recovered call = %+v, found=%v; want solved direct call", value, ok)
	}
}

func checkRecoveredMember(t *testing.T, handoff *solveHandoff, result *Result) {
	t.Helper()
	ref := requireMemberRef(t, handoff, func(member *memberRecord) bool {
		return member.Kind == memberField && member.Name == "value"
	})
	value, ok := result.Member(ref)
	if !ok || value.Owner == 0 || value.Node != 0 {
		t.Fatalf("recovered member = %+v, found=%v; want solved member with failed-result node zero", value, ok)
	}
}
