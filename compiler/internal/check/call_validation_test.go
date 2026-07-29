package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

const callValidationSource = `
fn add(left i32, right i32) i32 => left;
type Box = struct { value i32; fn get(self Box) i32 => self.value; };
type Choice = union enum { empty void; value i32; };
let direct i32 = add(1, 2);
let function fn(i32, i32) i32 = add;
let indirect i32 = function(3, 4);
let box Box = Box.{ value = 1 };
let method i32 = box.get();
let payload Choice = Choice.value(5);
let empty Choice = Choice.empty();
`

func runCallValidation(t *testing.T) (*diagnostic.DiagnosticSet, *solveHandoff, *solvedRecords) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(callValidationSource)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, handoff, records
}

func TestValidateCallRecordsAcceptsAllCallKinds(t *testing.T) {
	diagnostics, handoff, records := runCallValidation(t)
	counts := map[callKind]int{}
	for _, retained := range handoff.Records.Records() {
		if retained.Call != nil {
			counts[retained.Call.Target.Kind]++
		}
	}
	if counts[callDirect] != 1 || counts[callIndirect] != 1 || counts[callMethod] != 1 || counts[callVariant] != 2 {
		t.Fatalf("call counts = %+v; diagnostics=%+v", counts, diagnostics.Items())
	}
	if !validateCallRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeCall) {
		t.Fatalf("valid calls were rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCallRecordsRejectsArityMismatchByKind(t *testing.T) {
	for _, kind := range []callKind{callDirect, callIndirect, callMethod, callVariant} {
		t.Run(kindName(kind), func(t *testing.T) {
			diagnostics, handoff, records := runCallValidation(t)
			for _, retained := range handoff.Records.values {
				if retained.Call == nil || retained.Call.Target.Kind != kind || kind == callVariant && len(retained.Call.Arguments) != 1 {
					continue
				}
				retained.Call.Arguments = append(retained.Call.Arguments, callArgument{})
				break
			}
			if validateCallRecords(handoff, records, diagnostics, Config{}) == true || !hasValidationDiagnostic(diagnostics, CodeCall) {
				t.Fatalf("%s arity mismatch was accepted: %+v", kindName(kind), diagnostics.Items())
			}
		})
	}
}

func TestValidateCallRecordsSkipsInactiveGuardedCalls(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn identity[T](value T) T => value;
type Box = struct {
    functions [1]fn(i32) i32;
    fn echo[T](self Box, value T) T => value;
};
let function fn(i32) i32 = identity[i32];
let box Box = Box.{ functions = [function] };
let method i32 = box.echo[i32](1);
let indexed i32 = box.functions[0](2);
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	inactive := false
	for _, retained := range handoff.Records.Records() {
		if retained.Call != nil && retained.Header.Alternative.Guarded {
			inactive = true
			break
		}
	}
	if !inactive {
		t.Fatal("fixture did not produce a guarded call")
	}
	if !validateCallRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeCall) {
		t.Fatalf("inactive call produced a diagnostic: %+v", diagnostics.Items())
	}
}

func kindName(kind callKind) string {
	switch kind {
	case callDirect:
		return "direct"
	case callIndirect:
		return "indirect"
	case callMethod:
		return "method"
	case callVariant:
		return "variant"
	default:
		return "unknown"
	}
}
