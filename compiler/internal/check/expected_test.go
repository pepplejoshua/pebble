package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func TestExpectedReservedInitializerGetsItsExactSyntaxRoot(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let answer i32 = 42;`)})
	facts := run06a3(inputs, diagnostics, Config{})
	var binding *bindingRecord
	var expression *expressionRecord
	var compatibility *compatibilityRecord
	for _, retained := range facts.Generation.records.values {
		if retained.Binding != nil && retained.Binding.InitializerPresent {
			copy := *retained.Binding
			binding = &copy
		}
		if retained.Expression != nil && retained.Expression.Kind == expressionLiteral {
			copy := cloneExpressionRecord(*retained.Expression)
			expression = &copy
		}
		if retained.Compatibility != nil && retained.Compatibility.Role == compatibilityAssignment {
			copy := *retained.Compatibility
			compatibility = &copy
		}
	}
	if binding == nil || expression == nil || compatibility == nil {
		t.Fatalf("missing binding/expression/compatibility records: %+v", facts.Generation.records.values)
	}
	if expression.Result != binding.Initializer || compatibility.Source != binding.Initializer || compatibility.Destination != binding.Annotation {
		t.Fatalf("initializer joins = binding %+v expression %+v compatibility %+v", binding, expression, compatibility)
	}
	root, ok := facts.Generation.roots.root(binding.Initializer)
	if !ok || root.Kind != rootSyntax || root.Syntax != (symbol.SyntaxRef{Module: expression.Header.Syntax.Module, Node: expression.Header.Syntax.Node}) {
		t.Fatalf("initializer root = %+v, ok=%v", root, ok)
	}
	if diagnostics.HasErrors() {
		t.Fatalf("valid evidence diagnostics: %+v", diagnostics.Items())
	}
}

func TestExpectedOrdinaryCompatibilityDoesNotBecomeIdentityEvidence(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let widened i64 = sizeof i32;`)})
	facts := run06a3(inputs, diagnostics, Config{})
	var binding *bindingRecord
	for _, retained := range facts.Generation.records.values {
		if retained.Binding != nil && retained.Binding.InitializerPresent {
			binding = retained.Binding
		}
	}
	if binding == nil {
		t.Fatal("missing binding")
	}
	if expected := facts.Walk.expectations[facts.Generation.values[binding.Initializer-1].Origin.Syntax]; expected.Kind != expectNone {
		t.Fatalf("ordinary sizeof conversion received identity evidence: %+v", expected)
	}
}

func TestExpectedOptionalLiteralProjectsOnlyToPayload(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let maybe ?i32 = 1;`)})
	facts := run06a3(inputs, diagnostics, Config{})
	assignment, injection := 0, 0
	for _, retained := range facts.Generation.records.values {
		if retained.Compatibility == nil {
			continue
		}
		switch retained.Compatibility.Role {
		case compatibilityAssignment:
			assignment++
		case compatibilityOptionalInjection:
			injection++
		}
	}
	if assignment != 1 || injection != 1 {
		t.Fatalf("assignment/injection records = %d/%d", assignment, injection)
	}
	if diagnostics.HasErrors() {
		t.Fatalf("optional evidence diagnostics: %+v", diagnostics.Items())
	}
}

func TestExpectedContextualOperatorCarriesPreSolveIdentityProjection(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let sum i32 = 1 + 2;`)})
	facts := run06a3(inputs, diagnostics, Config{})
	var operator symbol.SyntaxRef
	for _, ref := range facts.Walk.order {
		node, _ := facts.Walk.node(ref.Module, ref.Node)
		if node.Kind() == syntax.BinaryExpr {
			operator = ref
			break
		}
	}
	if expected := facts.Walk.expectations[operator]; expected.Kind != expectIdentity || expected.Destination == 0 {
		t.Fatalf("operator expectation = %+v", expected)
	}
}

func TestExpectedCompatibilitySurvivesExpectNoneForTupleAndSome(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let source i32 = 1;
type Box = struct { value i32; };
let pair (i32, uint) = (source, sizeof i32);
let maybe ?i32 = some source;
let box Box = .{ value = source };
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	assignmentCompatibility, tupleCompatibility, someCompatibility, fieldCompatibility := 0, 0, 0, 0
	noneWithDestination := 0
	for _, expected := range facts.Walk.expectations {
		if expected.Kind == expectNone && expected.Destination != 0 {
			noneWithDestination++
		}
	}
	for _, retained := range facts.Generation.records.values {
		if retained.Compatibility == nil {
			continue
		}
		switch retained.Compatibility.Role {
		case compatibilityAssignment:
			assignmentCompatibility++
		case compatibilityTupleComponent:
			tupleCompatibility++
		case compatibilityOptionalInjection:
			someCompatibility++
		case compatibilityRecordField:
			fieldCompatibility++
		}
	}
	if noneWithDestination < 4 || assignmentCompatibility != 4 || tupleCompatibility != 2 || someCompatibility != 1 || fieldCompatibility != 1 {
		t.Fatalf("none destinations=%d assignment=%d tuple=%d some=%d field=%d", noneWithDestination, assignmentCompatibility, tupleCompatibility, someCompatibility, fieldCompatibility)
	}
	if diagnostics.HasErrors() {
		t.Fatalf("compatibility evidence diagnostics: %+v", diagnostics.Items())
	}
}

func TestExpectedNongenericFunctionProjectsKnownShapeComponents(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let function fn(i32) i64 = fn(value i32) i64 => 1;
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	parameterEvidence, resultEvidence, functionExpressions, compatibility := 0, 0, 0, 0
	for _, value := range facts.Generation.values {
		switch value.Origin.Role {
		case "function expected parameter 1":
			parameterEvidence++
			if root, ok := facts.Generation.roots.root(value.ID); !ok || root.Kind != rootSlot {
				t.Fatalf("function parameter destination root = %+v, ok=%v", root, ok)
			}
		case "function expected result":
			resultEvidence++
			if root, ok := facts.Generation.roots.root(value.ID); !ok || root.Kind != rootSlot {
				t.Fatalf("function result destination root = %+v, ok=%v", root, ok)
			}
		}
	}
	for _, retained := range facts.Generation.records.values {
		if retained.Expression != nil && retained.Expression.Kind == expressionFunction {
			functionExpressions++
		}
		if retained.Compatibility != nil {
			compatibility++
		}
	}
	if parameterEvidence != 1 || resultEvidence != 1 || functionExpressions != 1 || compatibility != 1 {
		t.Fatalf("function evidence parameter=%d result=%d expressions=%d compatibility=%d", parameterEvidence, resultEvidence, functionExpressions, compatibility)
	}
	if diagnostics.HasErrors() {
		t.Fatalf("function shape diagnostics: %+v", diagnostics.Items())
	}
}
