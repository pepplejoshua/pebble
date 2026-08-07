package check

import (
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

// TestExpressionBodyReturnBuildsRealReturn drives an expression-bodied function
// through the full pipeline and inspects the resulting body node graph: the
// function's Block must have a real child (not the empty block the old lowering
// produced), and that child must be a Return whose value traces back to the
// authored `1 + 2` computation.
func TestExpressionBodyReturnBuildsRealReturn(t *testing.T) {
	unit, ok := buildUnitFixture(t, `fn f() i32 => 1 + 2;`)
	if !ok || unit == nil {
		t.Fatal("expression-bodied function did not build")
	}
	block := functionBody(t, unit)
	if len(block.Children) == 0 {
		t.Fatalf("expression-bodied function block has no children: %+v", block)
	}
	returnNode := unit.Nodes()[block.Children[0]-1]
	if returnNode.Kind != tir.Return || len(returnNode.Children) != 1 {
		t.Fatalf("expression-bodied statement = %+v, want a single-value Return", returnNode)
	}
	value := unit.Nodes()[returnNode.Children[0]-1]
	if value.Kind != tir.CheckedArithmetic || value.Operator != syntax.Plus || len(value.Children) != 2 {
		t.Fatalf("expression-bodied returned value = %+v, want CheckedArithmetic 1 + 2", value)
	}
	left := unit.Nodes()[value.Children[0]-1]
	right := unit.Nodes()[value.Children[1]-1]
	if left.Kind != tir.IntegerLiteral || left.Literal.IntegerNum != "1" {
		t.Fatalf("expression-bodied left operand = %+v, want integer 1", left)
	}
	if right.Kind != tir.IntegerLiteral || right.Literal.IntegerNum != "2" {
		t.Fatalf("expression-bodied right operand = %+v, want integer 2", right)
	}
}

// TestExpressionBodyReturnMatchesBlockBody confirms the expression-bodied form
// produces a body identical in shape to the block-bodied equivalent: same node
// kinds, same child counts, recursively. This is parity, not just "doesn't
// crash".
func TestExpressionBodyReturnMatchesBlockBody(t *testing.T) {
	expression, ok := buildUnitFixture(t, `fn f() i32 => 1 + 2;`)
	if !ok || expression == nil {
		t.Fatal("expression-bodied function did not build")
	}
	block, ok := buildUnitFixture(t, `fn f() i32 { return 1 + 2; }`)
	if !ok || block == nil {
		t.Fatal("block-bodied function did not build")
	}
	expressionShape := blockBodyShape(t, expression)
	blockShape := blockBodyShape(t, block)
	if expressionShape != blockShape {
		t.Fatalf("expression body shape %q != block body shape %q", expressionShape, blockShape)
	}
}

// TestExpressionBodyReturnMismatchReportsCompatibilityDiagnostic confirms the
// expectation wiring from callableChildren engages the normal return
// compatibility check: a mismatched expression body reports the same T0505
// unify diagnostic as the block-bodied equivalent, and the wire's resolved
// record is retained as a compatibilityReturn.
func TestExpressionBodyReturnMismatchReportsCompatibilityDiagnostic(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn f() i32 => true;`)})
	run06a(inputs, diagnostics, Config{})
	if !hasValidationDiagnostic(diagnostics, infer.CodeUnification) {
		t.Fatalf("expression-bodied return mismatch reported no T0505 compatibility diagnostic: %+v", diagnostics.Items())
	}
	blockInputs, blockDiagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn f() i32 { return true; }`)})
	run06a(blockInputs, blockDiagnostics, Config{})
	if !hasValidationDiagnostic(blockDiagnostics, infer.CodeUnification) {
		t.Fatalf("block-bodied return mismatch reported no T0505 compatibility diagnostic: %+v", blockDiagnostics.Items())
	}
}

// TestExpressionBodyGenericSpecializationBody inspects the specialization's own
// body, not just that it builds: the substituted body must be a real Return of
// the parameter value at the substituted i32 type.
func TestExpressionBodyGenericSpecializationBody(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn identity[T](x T) T => x;
let a i32 = identity[i32](1);
`)
	if !ok || unit == nil {
		t.Fatal("generic expression-bodied function did not build")
	}
	var specialization tir.Node
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration && len(node.TypeArgs) == 1 {
			specialization = node
			break
		}
	}
	if specialization.Function == 0 || len(specialization.TypeArgs) != 1 {
		t.Fatalf("identity specialization declaration = %+v", specialization)
	}
	var body tir.FunctionDecl
	for _, candidate := range unit.FunctionDeclarations() {
		if candidate.FunctionID == specialization.Function {
			body = candidate
			break
		}
	}
	if body.Node == 0 {
		t.Fatal("specialization body missing")
	}
	block := unit.Nodes()[body.Node-1]
	if block.Kind != tir.Block || len(block.Children) != 1 {
		t.Fatalf("specialization body = %+v, want one Return", block)
	}
	returnNode := unit.Nodes()[block.Children[0]-1]
	if returnNode.Kind != tir.Return || len(returnNode.Children) != 1 {
		t.Fatalf("specialization statement = %+v, want a single-value Return", returnNode)
	}
	value := unit.Nodes()[returnNode.Children[0]-1]
	if value.Kind != tir.SymbolValue || value.Type != specialization.TypeArgs[0] {
		t.Fatalf("specialization returned value = %+v, want SymbolValue of the substituted type %v", value, specialization.TypeArgs[0])
	}
}

// TestExpressionBodyPrintRejectedAtParse confirms the grammar forbids a print
// statement as an expression body: the parser rejects `=> print(...)` at parse
// time (P0003 expected expression, P0005 expected terminator). print is a
// statement keyword, not a legal expression after `=>`, so this is a parse
// rejection rather than a type error.
func TestExpressionBodyPrintRejectedAtParse(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn f() void => print("x");`)})
	run06a(inputs, diagnostics, Config{})
	parseFailure := false
	for _, item := range diagnostics.Items() {
		if item.Code == diagnostic.Code("P0003") || item.Code == diagnostic.Code("P0005") {
			parseFailure = true
			break
		}
	}
	if !parseFailure {
		t.Fatalf("print-as-expression-body was not rejected at parse: %+v", diagnostics.Items())
	}
}

// blockBodyShape returns a canonical shape string for the single function body
// block: node kinds and child counts, recursively. Node IDs are not compared,
// so two independently built units with identical body shapes produce identical
// strings.
func blockBodyShape(t *testing.T, unit *tir.Unit) string {
	t.Helper()
	block := functionBody(t, unit)
	var builder strings.Builder
	appendNodeShape(&builder, unit, block)
	return builder.String()
}

func appendNodeShape(builder *strings.Builder, unit *tir.Unit, node tir.Node) {
	builder.WriteString(node.Kind.String())
	builder.WriteString("(")
	for index, child := range node.Children {
		if index != 0 {
			builder.WriteString(",")
		}
		appendNodeShape(builder, unit, unit.Nodes()[child-1])
	}
	builder.WriteString(")")
}

// TestClosureLiteralExpressionBodyReturnNoFalseC0607 verifies that an anonymous
// closure literal using => expr arrow shorthand does not trigger a false C0607
// (non-void function can fall through without returning). The closure's body
// must be treated identically to a named function's expression body.
func TestClosureLiteralExpressionBodyReturnNoFalseC0607(t *testing.T) {
	diagnostics, valid := validateControlFixture(t, `
fn call_it(f fn (str, str) bool) bool {
    return f("a", "a");
}
fn main() int {
    var r = call_it(fn (a, b str) bool => a == b);
    if r { return 1; }
    return 0;
}
`)
	if !valid || hasControlDiagnostic(diagnostics, CodeMissingReturn) {
		t.Fatalf("closure literal with => expr should not report C0607: valid=%v diagnostics=%+v", valid, diagnostics.Items())
	}
}

// TestClosureLiteralExpressionBodyReturnMismatchReportsCompatibilityDiagnostic
// confirms that a closure literal with a mismatched expression body reports the
// same T0505 unify diagnostic as a named function with the same issue.
func TestClosureLiteralExpressionBodyReturnMismatchReportsCompatibilityDiagnostic(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn take_bool(f fn () bool) {}
fn main() void {
    take_bool(fn () bool => "a");
}
`)})
	run06a(inputs, diagnostics, Config{})
	if !hasValidationDiagnostic(diagnostics, infer.CodeUnification) {
		t.Fatalf("closure literal return type mismatch reported no T0505: %+v", diagnostics.Items())
	}
	blockInputs, blockDiagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn take_bool(f fn () bool) {}
fn main() void {
    take_bool(fn () bool { return "a"; });
}
`)})
	run06a(blockInputs, blockDiagnostics, Config{})
	if !hasValidationDiagnostic(blockDiagnostics, infer.CodeUnification) {
		t.Fatalf("block-bodied closure literal return mismatch reported no T0505: %+v", blockDiagnostics.Items())
	}
}
