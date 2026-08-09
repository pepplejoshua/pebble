package check

import (
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
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

func validateBooleanOperatorFixture(t *testing.T, source string) (*diagnostic.DiagnosticSet, bool) {
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
	return diagnostics, validateBooleanOperators(handoff, records, diagnostics, Config{})
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

func TestValidateArithmeticOperatorsRejectsStringConcatenation(t *testing.T) {
	diagnostics, successful := validateOperatorFixture(t, `
fn main() int {
    let a = "hello";
    let b = "world";
    let c = a + b;
    return c.len as int;
}`)
	if successful || !hasOperatorDiagnostic(diagnostics) {
		t.Fatalf("str + str was not rejected: successful=%v diagnostics=%+v", successful, diagnostics.Items())
	}
	for _, item := range diagnostics.Items() {
		if item.Code != CodeOperator {
			continue
		}
		if !strings.Contains(item.Message, "push_str") || !strings.Contains(item.Message, "String") {
			t.Fatalf("str + str message does not point at String/push_str: %+v", item.Message)
		}
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

func TestValidateArithmeticOperatorsAcceptsLiteralFitRigidOperand(t *testing.T) {
	diagnostics, successful := validateOperatorFixture(t, `
fn literal[T](value T) T { return value + 1; }`)
	if !successful || hasOperatorDiagnostic(diagnostics) {
		t.Fatalf("literal-fit rigid arithmetic was rejected: %+v", diagnostics.Items())
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

func TestValidateBooleanOperatorsAcceptsConcreteFamilies(t *testing.T) {
	diagnostics, successful := validateBooleanOperatorFixture(t, `
type Color = enum { red, blue };
fn boolean_and_ordering(a bool, b bool, c char, s str, n i32, color Color) void {
  let not_value = !a;
  let and_value = a && b;
  let or_value = a || b;
  let char_order = c < c;
  let string_order = s <= s;
  let numeric_order = n >= n;
  let enum_order = color > color;
  let bool_equal = a == b;
  let char_equal = c != c;
  let string_equal = s == s;
  let numeric_equal = n == n;
  let enum_equal = color != color;
  let pointer_equal = &n == &n;
}`)
	if !successful || hasOperatorDiagnostic(diagnostics) {
		t.Fatalf("valid boolean/comparison operators were rejected: %+v", diagnostics.Items())
	}
}

func TestValidateBooleanOperatorsRejectsDirectMismatches(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}
	builtins := inputs.Types.Builtins()
	tests := []struct {
		name   string
		family operatorFamily
		token  syntax.TokenKind
		roots  map[valueID]infer.TypeResult
	}{
		{"ordering type mismatch", operatorOrdering, syntax.Less, map[valueID]infer.TypeResult{1: {State: infer.TypeFinal, Type: builtins.I32}, 2: {State: infer.TypeFinal, Type: builtins.I64}, 3: {State: infer.TypeFinal, Type: builtins.Bool}}},
		{"equality type mismatch", operatorEquality, syntax.Equal, map[valueID]infer.TypeResult{1: {State: infer.TypeFinal, Type: builtins.I32}, 2: {State: infer.TypeFinal, Type: builtins.I64}, 3: {State: infer.TypeFinal, Type: builtins.Bool}}},
		{"boolean non-bool operand", operatorBoolean, syntax.LogicalAnd, map[valueID]infer.TypeResult{1: {State: infer.TypeFinal, Type: builtins.I32}, 2: {State: infer.TypeFinal, Type: builtins.Bool}, 3: {State: infer.TypeFinal, Type: builtins.Bool}}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			handoff.Records = frozenRecords{values: []retainedRecord{{
				Header:   recordHeader{ID: 1, Owner: 1},
				Operator: &operatorRecord{Header: recordHeader{ID: 1, Owner: 1}, Form: operatorBinary, Family: test.family, Token: test.token, Operands: []valueID{1, 2}, Result: 3},
			}}}
			fresh := diagnostic.NewDiagnosticSet()
			if validateBooleanOperators(handoff, &solvedRecords{roots: test.roots}, fresh, Config{}) || !hasOperatorDiagnostic(fresh) {
				t.Fatalf("invalid operator was not rejected: diagnostics=%+v", fresh.Items())
			}
		})
	}
}

func TestValidateBooleanOperatorsSkipsRigidAndInactiveRecords(t *testing.T) {
	diagnostics, successful := validateBooleanOperatorFixture(t, `
fn generic[T](left T, right T) bool {
  return left == right;
}`)
	if !successful || hasOperatorDiagnostic(diagnostics) {
		t.Fatalf("rigid equality operand was rejected: %+v", diagnostics.Items())
	}

	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}
	builtins := inputs.Types.Builtins()
	guard := alternativeTag{Choice: 999999, Index: 7, Guarded: true}
	handoff.Records = frozenRecords{values: []retainedRecord{{
		Header:   recordHeader{ID: 1, Owner: 1, Alternative: guard},
		Operator: &operatorRecord{Header: recordHeader{ID: 1, Owner: 1, Alternative: guard}, Form: operatorBinary, Family: operatorOrdering, Token: syntax.Less, Operands: []valueID{1, 2}, Result: 3},
	}}}
	fresh := diagnostic.NewDiagnosticSet()
	if !validateBooleanOperators(handoff, &solvedRecords{roots: map[valueID]infer.TypeResult{1: {State: infer.TypeFinal, Type: builtins.I32}, 2: {State: infer.TypeFinal, Type: builtins.I64}, 3: {State: infer.TypeFinal, Type: builtins.Bool}}}, fresh, Config{}) || hasOperatorDiagnostic(fresh) {
		t.Fatalf("inactive guarded record was not skipped: diagnostics=%+v", fresh.Items())
	}
}
