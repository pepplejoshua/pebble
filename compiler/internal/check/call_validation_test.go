package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
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

// TestValidateCallRecordsAcceptsMethodCallWithArgument guards against a real
// bug: validateCallRecords used to compare a method call's explicit argument
// count against MethodSelection.Arguments, which is the method's own generic
// type-argument list (see infer.MethodSelection and methodState.arguments in
// internal/infer/instantiate.go), not its runtime call arguments. For any
// non-generic method that list is always empty, so any method call passing a
// real argument beyond the receiver was wrongly rejected with CodeCall. The
// fix compares against the method's resolved Signature.Inputs (minus one for
// the receiver, which occupies Inputs[0] exactly like Parameters[0] — see
// call_validation.go's callMethod case) instead.
func TestValidateCallRecordsAcceptsMethodCallWithArgument(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct { x i32; fn add(self Point, delta i32) i32 => self.x + delta; };
let p Point = Point.{ x = 40 };
let sum i32 = p.add(2);
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	if !validateCallRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeCall) {
		t.Fatalf("method call with a real argument was wrongly rejected: %+v", diagnostics.Items())
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

func TestValidateCallableRecordsAcceptsDeclarationsAndLegalLiterals(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let global i32 = 1;
extern fn foreign(value i32) i32;
fn plain(value i32) i32 => value;
type Box = struct { value i32; fn get(self Box) i32 => self.value; };
let reads_global = fn() i32 => global;
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	if !validateCallableRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeCall) || hasValidationDiagnostic(diagnostics, CodeCaptureViolation) || hasValidationDiagnostic(diagnostics, CodeGenericAnonymous) {
		t.Fatalf("legal callable declarations were rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCallableRecordsRejectsCapturesAndGenericAnonymous(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let valid i32 = fn() i32 => 1;
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	for index := range handoff.Records.values {
		retained := &handoff.Records.values[index]
		if retained.Callable != nil && retained.Callable.Kind == callableLiteral {
			retained.Callable.Captures = []symbol.SymbolID{1}
			break
		}
	}
	var header recordHeader
	for _, retained := range handoff.Records.values {
		if retained.Callable != nil && retained.Callable.Kind == callableLiteral {
			header = retained.Header
			break
		}
	}
	handoff.Records.values = append(handoff.Records.values, retainedRecord{
		Header: header,
		UnsupportedCallable: &unsupportedCallableRecord{
			Header:         header,
			TypeParameters: []symbol.SyntaxRef{{Module: 1, Node: 1}},
		},
	})
	if validateCallableRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeCaptureViolation) || !hasValidationDiagnostic(diagnostics, CodeGenericAnonymous) {
		t.Fatalf("illegal anonymous declarations were accepted: %+v", diagnostics.Items())
	}
}

func TestValidateCallableRecordsRejectsInvalidConventionAndSelf(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
extern fn foreign(value i32) i32;
type Box = struct { value i32; fn get(self Box, other i32) i32 => other; };
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	mutatedConvention, mutatedSelf := false, false
	for index := range handoff.Records.values {
		retained := &handoff.Records.values[index]
		if retained.Callable == nil {
			continue
		}
		if retained.Callable.Kind == callableExtern {
			retained.Callable.Convention = types.Pebble
			mutatedConvention = true
		}
		if retained.Callable.Kind == callableMethod && len(retained.Callable.Parameters) > 1 {
			retained.Callable.Parameters[0] = retained.Callable.Parameters[1]
			mutatedSelf = true
		}
	}
	if !mutatedConvention || !mutatedSelf {
		t.Fatalf("fixture did not produce extern and multi-parameter method records")
	}
	if validateCallableRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeCall) {
		t.Fatalf("invalid callable declarations were accepted: %+v", diagnostics.Items())
	}
}

func TestValidateCallableRecordsSkipsInactiveRecords(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn identity[T](value T) T => value;
type Box = struct { functions [1]fn(i32) i32; fn echo[T](self Box, value T) T => value; };
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
	var inactiveHeader recordHeader
	for _, retained := range handoff.Records.values {
		if retained.Call != nil && retained.Header.Alternative.Guarded && !activeOperatorRecord(handoff, retained.Header) {
			inactiveHeader = retained.Header
			break
		}
	}
	if inactiveHeader.Alternative == (alternativeTag{}) {
		t.Fatal("fixture did not produce an inactive guarded record")
	}
	for index := range handoff.Records.values {
		retained := &handoff.Records.values[index]
		if retained.Callable != nil {
			retained.Callable.Header.Alternative = inactiveHeader.Alternative
			retained.Header.Alternative = inactiveHeader.Alternative
			break
		}
	}
	handoff.Records.values = append(handoff.Records.values, retainedRecord{
		Header: inactiveHeader,
		UnsupportedCallable: &unsupportedCallableRecord{
			Header:         inactiveHeader,
			TypeParameters: []symbol.SyntaxRef{{Module: 1, Node: 1}},
		},
	})
	if !validateCallableRecords(handoff, records, diagnostics, Config{}) || hasValidationDiagnostic(diagnostics, CodeCall) || hasValidationDiagnostic(diagnostics, CodeCaptureViolation) || hasValidationDiagnostic(diagnostics, CodeGenericAnonymous) {
		t.Fatalf("inactive callable records produced a diagnostic: %+v", diagnostics.Items())
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
