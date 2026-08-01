package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

func TestRun06bSuccess(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatal("run06a produced invalid handoff")
	}

	result := run06b(handoff, diagnostics, Config{}, inputs.Types)
	if !result.Successful() {
		t.Fatal("run06b should have succeeded")
	}

	if result.Solution() != handoff.Solution {
		t.Fatal("Solution() returned unexpected value")
	}

	symID := handoff.Compilation.Modules[0].Declarations[0]
	tr, ok := result.SymbolType(symID)
	if !ok {
		t.Fatal("SymbolType should find the main symbol")
	}
	if tr.State != infer.TypeFinal {
		t.Fatalf("expected TypeFinal, got %v", tr.State)
	}
}

func TestRun06bIRPublishedOnSuccess(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	result := run06b(handoff, diagnostics, Config{}, inputs.Types)
	if !result.Successful() {
		t.Fatal("expected success")
	}
	if result.IR() == nil {
		t.Fatal("IR() should be non-nil for a successful result")
	}
}

func TestRun06bAuditHandoffFailure(t *testing.T) {
	inputsA, _ := factInputs(t, checkProvider{"main.peb": []byte("fn a() void { print 1; }\n")})
	inputsB, _ := factInputs(t, checkProvider{"main.peb": []byte("fn b() void { print 2; }\n")})
	handoffA := run06a(inputsA, diagnostic.NewDiagnosticSet(), Config{})
	handoffB := run06a(inputsB, diagnostic.NewDiagnosticSet(), Config{})

	handoff := &solveHandoff{
		Semantics: handoffA.Semantics,
		Solution:  handoffB.Solution,
	}

	result := run06b(handoff, diagnostic.NewDiagnosticSet(), Config{}, inputsA.Types)
	if result.Successful() {
		t.Fatal("expected auditHandoff to fail for mismatched semantics/solution")
	}
	if result.IR() != nil {
		t.Fatal("IR() should return nil for a failed result")
	}
}

func TestRun06bResolveRecordsFailure(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	full := run06a(inputs, diagnostics, Config{})
	if full == nil || len(full.Compilation.Modules) == 0 || len(full.Compilation.Modules[0].Declarations) == 0 {
		t.Fatal("test setup: no valid handoff")
	}
	symID := full.Compilation.Modules[0].Declarations[0]

	handoff := &solveHandoff{
		Semantics: full.Semantics,
		Solution:  full.Solution,
		Roots: frozenRoots{values: []rootedValue{
			{Value: 1, Root: valueRoot{Kind: rootSymbol, Symbol: symID}},
			{Value: 1, Root: valueRoot{Kind: rootSymbol, Symbol: symID}},
		}},
	}

	result := run06b(handoff, diagnostic.NewDiagnosticSet(), Config{}, inputs.Types)
	if result.Successful() {
		t.Fatal("expected resolveRecords to fail for duplicate root")
	}
	if result.IR() != nil {
		t.Fatal("IR() should return nil for a failed result")
	}
}

func TestResultNilReceiver(t *testing.T) {
	var r *Result = nil

	if r.Successful() {
		t.Fatal("nil result should not be successful")
	}
	if r.Solution() != nil {
		t.Fatal("nil result should return nil solution")
	}
	if tr, ok := r.SymbolType(0); ok {
		t.Fatalf("nil result should return false for SymbolType, got %+v", tr)
	}
	if reqs := r.Requirements(0); reqs != nil {
		t.Fatalf("nil result should return nil Requirements, got %+v", reqs)
	}
	if inst, ok := r.Instantiation(symbol.SyntaxRef{}); ok {
		t.Fatalf("nil result should return false for Instantiation, got %+v", inst)
	}
	if ir := r.IR(); ir != nil {
		t.Fatalf("nil result should return nil IR, got %+v", ir)
	}
}
