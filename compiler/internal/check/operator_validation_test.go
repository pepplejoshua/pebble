package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

func validateOperatorFixture(t *testing.T, source string) (*diagnostic.DiagnosticSet, bool) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, validateArithmeticOperators(handoff, records, diagnostics, Config{})
}

func hasOperatorDiagnostic(set *diagnostic.DiagnosticSet) bool {
	for _, item := range set.Items() {
		if item.Code == CodeOperator {
			return true
		}
	}
	return false
}

func TestValidateArithmeticOperatorsAcceptsConcreteFamilies(t *testing.T) {
	diagnostics, successful := validateOperatorFixture(t, `
fn arithmetic(a i32, b i64, x f32, y f64) void {
  let neg = -a;
  let float_neg = -x;
  let sub = a - a;
  let mul = a * a;
  let div = a / a;
  let text = "a" + "b";
  let bits = ~a;
  let remainder = a % a;
  let anded = a & a;
  let ored = a | a;
  let xored = a ^ a;
  let shifted = a << b;
  let shifted_back = a >> b;
  let mixed = x + x;
  let mixed_float = y / y;
}`)
	if !successful || hasOperatorDiagnostic(diagnostics) {
		t.Fatalf("valid arithmetic was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateArithmeticOperatorsRejectsUnsignedUnaryMinus(t *testing.T) {
	diagnostics, successful := validateOperatorFixture(t, `
fn negate(value uint) uint { return -value; }`)
	if successful || !hasOperatorDiagnostic(diagnostics) {
		t.Fatalf("unsigned unary minus was not rejected: successful=%v diagnostics=%+v", successful, diagnostics.Items())
	}
}

func TestValidateArithmeticOperatorsAcceptsRigidOperands(t *testing.T) {
	diagnostics, successful := validateOperatorFixture(t, `
fn generic[T, U](left T, right U) T {
  let sum T = left + left;
  let shifted T = left << right;
  return sum;
}`)
	if !successful || hasOperatorDiagnostic(diagnostics) {
		t.Fatalf("rigid arithmetic was rejected: %+v", diagnostics.Items())
	}
}

// Real Pebble programs can't express a mismatched-operand binary arithmetic
// expression, since 06a's own unification already rejects it (the root would
// resolve to a TypeError state, which this validator silently skips). This
// constructs the record arena directly to prove the sameConcrete/resultMatches
// checks actually reject a mismatch rather than being vacuously unreachable.
func TestValidateArithmeticOperatorsRejectsMismatchedConcreteOperands(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}

	builtins := inputs.Types.Builtins()
	handoff.Records = frozenRecords{values: []retainedRecord{
		{
			Header: recordHeader{ID: 1, Owner: 1},
			Operator: &operatorRecord{
				Header:   recordHeader{ID: 1, Owner: 1},
				Form:     operatorBinary,
				Family:   operatorNumericSame,
				Operands: []valueID{1, 2},
				Result:   3,
			},
		},
	}}
	records := &solvedRecords{roots: map[valueID]infer.TypeResult{
		1: {State: infer.TypeFinal, Type: builtins.I32},
		2: {State: infer.TypeFinal, Type: builtins.I64},
		3: {State: infer.TypeFinal, Type: builtins.I32},
	}}

	fresh := diagnostic.NewDiagnosticSet()
	ok := validateArithmeticOperators(handoff, records, fresh, Config{})
	if ok || !hasOperatorDiagnostic(fresh) {
		t.Fatalf("mismatched concrete operand types were not rejected: ok=%v diagnostics=%+v", ok, fresh.Items())
	}
}

// Confirms an inactive guarded operator record (selection doesn't match its
// alternative index) is silently skipped, mirroring resolveRecords' own
// guarded-inactive policy, rather than being reported or crashing.
func TestValidateArithmeticOperatorsSkipsInactiveGuardedRecord(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}

	builtins := inputs.Types.Builtins()
	handoff.Records = frozenRecords{values: []retainedRecord{
		{
			Header: recordHeader{ID: 1, Owner: 1, Alternative: alternativeTag{Choice: 999999, Index: 7, Guarded: true}},
			Operator: &operatorRecord{
				Header:   recordHeader{ID: 1, Owner: 1, Alternative: alternativeTag{Choice: 999999, Index: 7, Guarded: true}},
				Form:     operatorBinary,
				Family:   operatorNumericSame,
				Operands: []valueID{1, 2},
				Result:   3,
			},
		},
	}}
	records := &solvedRecords{roots: map[valueID]infer.TypeResult{
		1: {State: infer.TypeFinal, Type: builtins.I32},
		2: {State: infer.TypeFinal, Type: builtins.I64},
		3: {State: infer.TypeFinal, Type: builtins.I32},
	}}

	fresh := diagnostic.NewDiagnosticSet()
	ok := validateArithmeticOperators(handoff, records, fresh, Config{})
	if !ok || hasOperatorDiagnostic(fresh) {
		t.Fatalf("inactive guarded operator record should be silently skipped: ok=%v diagnostics=%+v", ok, fresh.Items())
	}
}
