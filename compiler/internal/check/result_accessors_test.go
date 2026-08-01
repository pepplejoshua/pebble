package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// runAccessorFixture drives a full 06a+06b run for one module. The caller
// inspects the raw frozen records to locate the syntax reference of the exact
// expression/place/member/call/control occurrence it wants to query, then
// asserts on the accessor's closed snapshot and on the published IR.
func runAccessorFixture(t *testing.T, source string) (Inputs, *solveHandoff, *Result) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	result := run06b(handoff, diagnostics, Config{}, inputs.Types)
	return inputs, handoff, result
}

func deferredPrint(t *testing.T, unit *tir.Unit, id tir.NodeID) string {
	t.Helper()
	node := unit.Nodes()[id-1]
	if node.Kind == tir.DeferRegister {
		if len(node.Children) != 1 {
			t.Fatalf("defer register %d = %+v, want one child", id, node)
		}
		node = unit.Nodes()[node.Children[0]-1]
	}
	if node.Kind != tir.Print || len(node.Children) != 1 {
		t.Fatalf("deferred node %d = %+v, want Print or DeferRegister with one child", id, node)
	}
	operand := unit.Nodes()[node.Children[0]-1]
	if operand.Kind != tir.IntegerLiteral {
		t.Fatalf("deferred print operand = %+v, want IntegerLiteral", operand)
	}
	return operand.Literal.IntegerNum
}

func requireExpressionRef(t *testing.T, handoff *solveHandoff, match func(*expressionRecord) bool) symbol.SyntaxRef {
	t.Helper()
	for _, retained := range handoff.Records.Records() {
		if retained.Expression != nil && match(retained.Expression) {
			return retained.Header.Syntax
		}
	}
	t.Fatal("matching expression record not found")
	return symbol.SyntaxRef{}
}

func requirePlaceRef(t *testing.T, handoff *solveHandoff, match func(*placeRecord) bool) symbol.SyntaxRef {
	t.Helper()
	for _, retained := range handoff.Records.Records() {
		if retained.Place != nil && match(retained.Place) {
			return retained.Header.Syntax
		}
	}
	t.Fatal("matching place record not found")
	return symbol.SyntaxRef{}
}

func requireMemberRef(t *testing.T, handoff *solveHandoff, match func(*memberRecord) bool) symbol.SyntaxRef {
	t.Helper()
	for _, retained := range handoff.Records.Records() {
		if retained.Member != nil && match(retained.Member) {
			return retained.Header.Syntax
		}
	}
	t.Fatal("matching member record not found")
	return symbol.SyntaxRef{}
}

func requireCallRef(t *testing.T, handoff *solveHandoff, match func(*callRecord) bool) symbol.SyntaxRef {
	t.Helper()
	for _, retained := range handoff.Records.Records() {
		if retained.Call != nil && match(retained.Call) {
			return retained.Header.Syntax
		}
	}
	t.Fatal("matching call record not found")
	return symbol.SyntaxRef{}
}

func requireControlRef(t *testing.T, handoff *solveHandoff, match func(*controlRecord) bool) symbol.SyntaxRef {
	t.Helper()
	for _, retained := range handoff.Records.Records() {
		if retained.Control != nil && match(retained.Control) {
			return retained.Header.Syntax
		}
	}
	t.Fatal("matching control record not found")
	return symbol.SyntaxRef{}
}

func requireConversionRef(t *testing.T, handoff *solveHandoff, match func(*compatibilityRecord) bool) symbol.SyntaxRef {
	t.Helper()
	for _, retained := range handoff.Records.Records() {
		if retained.Compatibility != nil && match(retained.Compatibility) {
			return retained.Header.Syntax
		}
	}
	t.Fatal("matching compatibility record not found")
	return symbol.SyntaxRef{}
}

func TestResultExpressionAccessorSuccess(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, `fn main() i32 { let answer i32 = 42; return answer; }`)
	if !result.Successful() {
		t.Fatal("expected a successful result")
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("IR() should be non-nil for a successful result")
	}
	ref := requireExpressionRef(t, handoff, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalInteger && string(e.Literal.NumericBytes) == "42"
	})
	expr, ok := result.Expression(ref)
	if !ok {
		t.Fatal("Expression() should find the literal 42")
	}
	if expr.Category != valueCategoryValue {
		t.Fatalf("Category = %v, want valueCategoryValue", expr.Category)
	}
	if expr.Type.State != infer.TypeFinal || expr.Type.Type != inputs.Types.Builtins().I32 {
		t.Fatalf("Type = %+v, want final i32", expr.Type)
	}
	nid, mapped := unit.SourceMap(ref)
	if !mapped || nid != expr.Node {
		t.Fatalf("Node = %d, want SourceMap(ref) %d (mapped=%v)", expr.Node, nid, mapped)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.IntegerLiteral || node.Literal.IntegerNum != "42" {
		t.Fatalf("IR node at ref = %+v, want IntegerLiteral 42", node)
	}
}

func TestResultExpressionAccessorFailedResult(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, `fn main() void { let x i32 = 1; x = 2; }`)
	if result.Successful() {
		t.Fatal("fixture should fail validation")
	}
	if result.IR() != nil {
		t.Fatal("a failed result publishes no IR")
	}
	ref := requireExpressionRef(t, handoff, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalInteger && string(e.Literal.NumericBytes) == "2"
	})
	expr, ok := result.Expression(ref)
	if !ok {
		t.Fatal("Expression() must remain queryable in a failed result")
	}
	if expr.Node != 0 {
		t.Fatalf("Node = %d, want 0 in a failed result", expr.Node)
	}
	if expr.Type.State != infer.TypeFinal || expr.Type.Type != inputs.Types.Builtins().I32 {
		t.Fatalf("Type = %+v, want final i32 for a ref solved before the failure", expr.Type)
	}
	if expr.Category != valueCategoryValue {
		t.Fatalf("Category = %v, want valueCategoryValue", expr.Category)
	}
}

func TestResultPlaceAccessorSolvedTypeAndNode(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, `
type Box = struct { value i32; };
fn main() void {
    var box Box = Box.{ value = 1 };
    box.value = 2;
}
`)
	if !result.Successful() {
		t.Fatal("expected a successful result")
	}
	unit := result.IR()
	boxSymbol := findSymbolID(t, handoff, "box", symbol.SymbolBinding)
	valueSymbol := findSymbolID(t, handoff, "value", symbol.SymbolField)
	if boxSymbol == 0 || valueSymbol == 0 {
		t.Fatalf("missing symbols: box=%d value=%d", boxSymbol, valueSymbol)
	}
	ref := requirePlaceRef(t, handoff, func(p *placeRecord) bool { return p.Root == boxSymbol })
	place, ok := result.Place(ref)
	if !ok {
		t.Fatal("Place() should find the assignment destination")
	}
	// This is the regression test for the placeValue bug: the place's own
	// solved type must come from its retained value, not from scanning for a
	// coincidental record, so it must be the real field type.
	if place.Type.State != infer.TypeFinal || place.Type.Type != inputs.Types.Builtins().I32 {
		t.Fatalf("Place Type = %+v, want final i32 (placeValue bug)", place.Type)
	}
	if place.Kind != placeField {
		t.Fatalf("Kind = %v, want placeField", place.Kind)
	}
	if place.Root != boxSymbol {
		t.Fatalf("Root = %d, want box %d", place.Root, boxSymbol)
	}
	if !place.Writable {
		t.Fatal("Writable should be true for a var place")
	}
	if len(place.Projections) != 2 || place.Projections[0].Kind != placeStorage || place.Projections[1].Kind != placeField || place.Projections[1].Member != 0 {
		t.Fatalf("Projections = %+v, want storage then structural field with no retained member symbol", place.Projections)
	}
	storeID, mapped := unit.SourceMap(ref)
	if !mapped || storeID != place.Node {
		t.Fatalf("Node = %d, want SourceMap(ref) %d (mapped=%v)", place.Node, storeID, mapped)
	}
	store := unit.Nodes()[storeID-1]
	if store.Kind != tir.Store || len(store.Children) != 2 {
		t.Fatalf("IR node at place ref = %+v, want two-child Store", store)
	}
	placeNode := unit.Nodes()[store.Children[0]-1]
	if placeNode.Kind != tir.FieldPlace || placeNode.Member != valueSymbol {
		t.Fatalf("Store place child = %+v, want FieldPlace of %d", placeNode, valueSymbol)
	}
}

func TestResultPlaceAccessorFailedResult(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, `fn main() void { let x i32 = 1; x = 2; }`)
	if result.Successful() {
		t.Fatal("fixture should fail validation")
	}
	xSymbol := findSymbolID(t, handoff, "x", symbol.SymbolBinding)
	ref := requirePlaceRef(t, handoff, func(p *placeRecord) bool { return p.Root == xSymbol })
	place, ok := result.Place(ref)
	if !ok {
		t.Fatal("Place() must remain queryable in a failed result")
	}
	if place.Node != 0 {
		t.Fatalf("Node = %d, want 0 in a failed result", place.Node)
	}
	if place.Type.State != infer.TypeFinal || place.Type.Type != inputs.Types.Builtins().I32 {
		t.Fatalf("Type = %+v, want final i32 in a failed result", place.Type)
	}
	if place.Root != xSymbol {
		t.Fatalf("Root = %d, want x %d", place.Root, xSymbol)
	}
	if place.Writable {
		t.Fatal("Writable should be false for a let place")
	}
	if place.Kind != placeStorage {
		t.Fatalf("Kind = %v, want placeStorage", place.Kind)
	}
}

func TestResultMemberAccessorOwnerIsReceiverDeclaration(t *testing.T) {
	_, handoff, result := runAccessorFixture(t, `
type Box = struct { value i32; };
fn main(box Box) i32 { return box.value; }
`)
	if !result.Successful() {
		t.Fatal("expected a successful result")
	}
	unit := result.IR()
	boxSymbol := findSymbolID(t, handoff, "Box", symbol.SymbolType)
	valueSymbol := findSymbolID(t, handoff, "value", symbol.SymbolField)
	if boxSymbol == 0 || valueSymbol == 0 {
		t.Fatalf("missing symbols: Box=%d value=%d", boxSymbol, valueSymbol)
	}
	ref := requireMemberRef(t, handoff, func(m *memberRecord) bool {
		return m.Kind == memberField && m.Name == "value"
	})
	member, ok := result.Member(ref)
	if !ok {
		t.Fatal("Member() should find box.value")
	}
	if member.Kind != memberField {
		t.Fatalf("Kind = %v, want memberField", member.Kind)
	}
	// This is the regression test for the Owner bug: Owner is the receiver
	// type's own declaration symbol, distinct from the selected field symbol.
	if member.Owner != boxSymbol {
		t.Fatalf("Owner = %d, want Box %d (member Owner bug)", member.Owner, boxSymbol)
	}
	if member.Member != 0 {
		t.Fatalf("Member = %d, want zero for a structurally resolved field", member.Member)
	}
	nodeID, mapped := unit.SourceMap(ref)
	if !mapped || nodeID != member.Node {
		t.Fatalf("Node = %d, want SourceMap(ref) %d (mapped=%v)", member.Node, nodeID, mapped)
	}
	load := unit.Nodes()[nodeID-1]
	if load.Kind != tir.Load || len(load.Children) != 1 {
		t.Fatalf("IR node at member ref = %+v, want one-child Load", load)
	}
	placeNode := unit.Nodes()[load.Children[0]-1]
	if placeNode.Kind != tir.FieldPlace || placeNode.Member != valueSymbol {
		t.Fatalf("Load child = %+v, want FieldPlace of %d", placeNode, valueSymbol)
	}
}

func TestResultMemberAccessorFailedResult(t *testing.T) {
	_, handoff, result := runAccessorFixture(t, failedAccessorSource)
	if result.Successful() {
		t.Fatal("fixture should fail validation")
	}
	boxSymbol := findSymbolID(t, handoff, "Box", symbol.SymbolType)
	ref := requireMemberRef(t, handoff, func(m *memberRecord) bool {
		return m.Kind == memberField && m.Name == "value"
	})
	member, ok := result.Member(ref)
	if !ok {
		t.Fatal("Member() must remain queryable in a failed result")
	}
	if member.Node != 0 {
		t.Fatalf("Node = %d, want 0 in a failed result", member.Node)
	}
	if member.Owner != boxSymbol || member.Member != 0 {
		t.Fatalf("Owner/Member = %d/%d, want Box/structural field 0 in a failed result", member.Owner, member.Member)
	}
}

func TestResultConversionAccessorSuccess(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, `fn main() void { let annotated ?i32 = some 1; }`)
	if !result.Successful() {
		t.Fatal("expected a successful result")
	}
	unit := result.IR()
	ref := requireConversionRef(t, handoff, func(c *compatibilityRecord) bool {
		return c.Role == compatibilityOptionalInjection
	})
	conversion, ok := result.Conversion(ref)
	if !ok {
		t.Fatal("Conversion() should find the optional injection")
	}
	if conversion.Source != inputs.Types.Builtins().I32 {
		t.Fatalf("Source = %d, want i32", conversion.Source)
	}
	if conversion.Destination != inputs.Types.Builtins().I32 {
		t.Fatalf("Destination = %d, want the optional payload i32", conversion.Destination)
	}
	if conversion.Class != compatibleIdentity {
		t.Fatalf("Class = %v, want compatibleIdentity for the payload conversion", conversion.Class)
	}
	if conversion.Coercion != coercionNone {
		t.Fatalf("Coercion = %v, want coercionNone for the payload conversion", conversion.Coercion)
	}
	if conversion.Role != compatibilityOptionalInjection || conversion.Ordinal != 0 {
		t.Fatalf("Role/Ordinal = %v/%d, want optional injection/0", conversion.Role, conversion.Ordinal)
	}
	nodeID, mapped := unit.SourceMap(ref)
	if !mapped || nodeID != conversion.Node {
		t.Fatalf("Node = %d, want SourceMap(ref) %d (mapped=%v)", conversion.Node, nodeID, mapped)
	}
	node := unit.Nodes()[nodeID-1]
	if node.Kind != tir.SomeOptional {
		t.Fatalf("IR node at conversion ref = %+v, want SomeOptional", node)
	}
}

func TestResultConversionAccessorFailedResult(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, failedAccessorSource)
	if result.Successful() {
		t.Fatal("fixture should fail validation")
	}
	ref := requireConversionRef(t, handoff, func(c *compatibilityRecord) bool {
		return c.Role == compatibilityOptionalInjection
	})
	conversion, ok := result.Conversion(ref)
	if !ok {
		t.Fatal("Conversion() must remain queryable in a failed result")
	}
	if conversion.Node != 0 {
		t.Fatalf("Node = %d, want 0 in a failed result", conversion.Node)
	}
	if conversion.Class != compatibleIdentity || conversion.Coercion != coercionNone || conversion.Source != inputs.Types.Builtins().I32 || conversion.Destination != inputs.Types.Builtins().I32 {
		t.Fatalf("conversion = %+v, want identity i32 payload conversion in a failed result", conversion)
	}
}

func TestResultCallAccessorPebbleForwardsContext(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, `
fn sink(value i32) void {}
fn main() void { sink(7); }
`)
	if !result.Successful() {
		t.Fatal("expected a successful result")
	}
	unit := result.IR()
	sinkSymbol := findSymbolID(t, handoff, "sink", symbol.SymbolFunction)
	ref := requireCallRef(t, handoff, func(c *callRecord) bool {
		return c.Target.Kind == callDirect && c.Target.Symbol == sinkSymbol
	})
	call, ok := result.Call(ref)
	if !ok {
		t.Fatal("Call() should find sink(7)")
	}
	if call.Kind != callDirect || call.Symbol != sinkSymbol {
		t.Fatalf("Kind/Symbol = %v/%d, want callDirect/sink %d", call.Kind, call.Symbol, sinkSymbol)
	}
	if call.Convention != types.Pebble || call.Variadic {
		t.Fatalf("Convention/Variadic = %v/%v, want Pebble/false", call.Convention, call.Variadic)
	}
	if call.Context != contextActionForward {
		t.Fatalf("Context = %v, want contextActionForward for a Pebble-convention call", call.Context)
	}
	if len(call.Arguments) != 1 {
		t.Fatalf("Arguments = %d, want 1", len(call.Arguments))
	}
	argument := call.Arguments[0]
	if argument.Source != inputs.Types.Builtins().I32 || argument.Destination != inputs.Types.Builtins().I32 {
		t.Fatalf("argument Source/Destination = %d/%d, want i32/i32", argument.Source, argument.Destination)
	}
	if argument.Class != compatibleIdentity || argument.Coercion != coercionNone {
		t.Fatalf("argument Class/Coercion = %v/%v, want identity/none", argument.Class, argument.Coercion)
	}
	if argument.Role != compatibilityArgument || argument.Ordinal != 0 {
		t.Fatalf("argument Role/Ordinal = %v/%d, want argument/0", argument.Role, argument.Ordinal)
	}
	nodeID, mapped := unit.SourceMap(ref)
	if !mapped || nodeID != call.Node {
		t.Fatalf("Node = %d, want SourceMap(ref) %d (mapped=%v)", call.Node, nodeID, mapped)
	}
	node := unit.Nodes()[nodeID-1]
	if node.Kind != tir.DirectCall || node.Symbol != sinkSymbol || node.Convention != types.Pebble || node.ContextAction != tir.ContextForward {
		t.Fatalf("IR call node = %+v, want Pebble DirectCall forwarding context", node)
	}
	if len(node.Children) != 1 {
		t.Fatalf("IR call children = %d, want 1", len(node.Children))
	}
	operand := unit.Nodes()[node.Children[0]-1]
	if operand.Kind != tir.IntegerLiteral || operand.Literal.IntegerNum != "7" {
		t.Fatalf("IR call operand = %+v, want literal 7", operand)
	}
}

func TestResultCallAccessorCConventionDoesNotForwardContext(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, `
extern fn foreign(value i32) void;
fn main() void { foreign(7); }
`)
	if !result.Successful() {
		t.Fatal("expected a successful result")
	}
	unit := result.IR()
	foreignSymbol := findSymbolID(t, handoff, "foreign", symbol.SymbolExternFunction)
	ref := requireCallRef(t, handoff, func(c *callRecord) bool {
		return c.Target.Kind == callDirect && c.Target.Symbol == foreignSymbol
	})
	call, ok := result.Call(ref)
	if !ok {
		t.Fatal("Call() should find foreign(7)")
	}
	if call.Convention != types.C {
		t.Fatalf("Convention = %v, want C", call.Convention)
	}
	if call.Context != contextActionNone {
		t.Fatalf("Context = %v, want contextActionNone for a C-convention call", call.Context)
	}
	nodeID, mapped := unit.SourceMap(ref)
	if !mapped || nodeID != call.Node {
		t.Fatalf("Node = %d, want SourceMap(ref) %d (mapped=%v)", call.Node, nodeID, mapped)
	}
	node := unit.Nodes()[nodeID-1]
	if node.Kind != tir.DirectCall || node.Convention != types.C || node.ContextAction != tir.ContextNone {
		t.Fatalf("IR call node = %+v, want C DirectCall with no context", node)
	}
	_ = inputs
}

func TestResultCallAccessorFailedResult(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, failedAccessorSource)
	if result.Successful() {
		t.Fatal("fixture should fail validation")
	}
	sinkSymbol := findSymbolID(t, handoff, "sink", symbol.SymbolFunction)
	ref := requireCallRef(t, handoff, func(c *callRecord) bool {
		return c.Target.Kind == callDirect && c.Target.Symbol == sinkSymbol
	})
	call, ok := result.Call(ref)
	if !ok {
		t.Fatal("Call() must remain queryable in a failed result")
	}
	if call.Node != 0 {
		t.Fatalf("Node = %d, want 0 in a failed result", call.Node)
	}
	if call.Symbol != sinkSymbol || call.Convention != types.Pebble || call.Context != contextActionForward {
		t.Fatalf("call = %+v, want sink/Pebble/forward solved facts in a failed result", call)
	}
	if len(call.Arguments) != 1 || call.Arguments[0].Source != inputs.Types.Builtins().I32 || call.Arguments[0].Destination != inputs.Types.Builtins().I32 {
		t.Fatalf("Arguments = %+v, want one i32->i32 argument", call.Arguments)
	}
}

func TestResultControlAccessorDefersAcrossRegionBoundary(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, `
fn f() void {
    defer print 1;
    {
        defer print 2;
        return;
    }
}
`)
	if !result.Successful() {
		t.Fatal("expected a successful result")
	}
	unit := result.IR()
	if registers := nodesOfKind(unit, tir.DeferRegister); len(registers) != 2 {
		t.Fatalf("DeferRegister nodes = %d, want 2", len(registers))
	}
	ref := requireControlRef(t, handoff, func(c *controlRecord) bool { return c.Kind == controlReturn })
	control, ok := result.Control(ref)
	if !ok {
		t.Fatal("Control() should find the return")
	}
	if len(control.Exits) != 1 || control.Exits[0].Kind != exitReturn {
		t.Fatalf("Exits = %+v, want exactly exitReturn", control.Exits)
	}
	if control.Exits[0].Target != 0 || control.Target != 0 {
		t.Fatalf("return Target = %d, want 0", control.Target)
	}
	// The return is inside the inner block, so its defer chain must cross the
	// block boundary and run the inner defer (print 2) before the outer one
	// (print 1).
	if len(control.Defers) != 2 {
		t.Fatalf("Defers = %d, want 2", len(control.Defers))
	}
	for i, id := range control.Defers {
		want := []string{"2", "1"}[i]
		if got := deferredPrint(t, unit, id); got != want {
			t.Fatalf("Defers[%d] prints %s, want %s", i, got, want)
		}
	}
	nodeID, mapped := unit.SourceMap(ref)
	if !mapped || nodeID != control.Node {
		t.Fatalf("Node = %d, want SourceMap(ref) %d (mapped=%v)", control.Node, nodeID, mapped)
	}
	returnNode := unit.Nodes()[nodeID-1]
	if returnNode.Kind != tir.Return {
		t.Fatalf("IR node at control ref = %+v, want Return", returnNode)
	}
	if len(returnNode.DeferChain) != len(control.Defers) {
		t.Fatalf("IR Return DeferChain = %v, accessor Defers = %v", returnNode.DeferChain, control.Defers)
	}
	for i := range control.Defers {
		if got, want := deferredPrint(t, unit, returnNode.DeferChain[i]), deferredPrint(t, unit, control.Defers[i]); got != want {
			t.Fatalf("IR Return DeferChain prints %q, accessor Defers print %q", got, want)
		}
	}
	_ = inputs
}

func TestResultControlAccessorBreakContinueExitsTargetsAndDefers(t *testing.T) {
	inputs, handoff, result := runAccessorFixture(t, `
fn f(flag bool) void {
    while flag {
        defer print 1;
        if flag { break; }
        continue;
    }
    defer print 2;
}
`)
	if !result.Successful() {
		t.Fatal("expected a successful result")
	}
	unit := result.IR()
	if registers := nodesOfKind(unit, tir.DeferRegister); len(registers) != 2 {
		t.Fatalf("DeferRegister nodes = %d, want 2", len(registers))
	}
	breakRef := requireControlRef(t, handoff, func(c *controlRecord) bool { return c.Kind == controlBreak })
	continueRef := requireControlRef(t, handoff, func(c *controlRecord) bool { return c.Kind == controlContinue })
	var whileRegion controlID
	for _, retained := range handoff.Records.Records() {
		if retained.Control != nil && retained.Control.Kind == controlWhile {
			whileRegion = retained.Control.Region
		}
	}
	if whileRegion == 0 {
		t.Fatal("while control record not found")
	}
	br, ok := result.Control(breakRef)
	if !ok {
		t.Fatal("Control() should find the break")
	}
	if len(br.Exits) != 1 || br.Exits[0].Kind != exitBreak || br.Exits[0].Target != whileRegion {
		t.Fatalf("break Exits = %+v, want exitBreak to region %d", br.Exits, whileRegion)
	}
	if br.Target != whileRegion {
		t.Fatalf("break Target = %d, want loop region %d", br.Target, whileRegion)
	}
	if len(br.Defers) != 1 || deferredPrint(t, unit, br.Defers[0]) != "1" {
		t.Fatalf("break Defers = %v, want exactly the crossed while-body defer printing 1", br.Defers)
	}
	cr, ok := result.Control(continueRef)
	if !ok {
		t.Fatal("Control() should find the continue")
	}
	if len(cr.Exits) != 1 || cr.Exits[0].Kind != exitContinue || cr.Exits[0].Target != whileRegion {
		t.Fatalf("continue Exits = %+v, want exitContinue to region %d", cr.Exits, whileRegion)
	}
	if len(cr.Defers) != 1 || deferredPrint(t, unit, cr.Defers[0]) != "1" {
		t.Fatalf("continue Defers = %v, want exactly the crossed while-body defer printing 1", cr.Defers)
	}
	breakNodeID, mapped := unit.SourceMap(breakRef)
	if !mapped || breakNodeID != br.Node {
		t.Fatalf("break Node = %d, want SourceMap(ref) %d (mapped=%v)", br.Node, breakNodeID, mapped)
	}
	breakNode := unit.Nodes()[breakNodeID-1]
	if breakNode.Kind != tir.Break || breakNode.Target != tir.RegionID(whileRegion) {
		t.Fatalf("IR break node = %+v, want Break targeting region %d", breakNode, whileRegion)
	}
	if len(breakNode.DeferChain) != 1 || deferredPrint(t, unit, breakNode.DeferChain[0]) != deferredPrint(t, unit, br.Defers[0]) {
		t.Fatalf("IR break DeferChain = %v, accessor Defers = %v", breakNode.DeferChain, br.Defers)
	}
	continueNodeID, mapped := unit.SourceMap(continueRef)
	if !mapped || continueNodeID != cr.Node {
		t.Fatalf("continue Node = %d, want SourceMap(ref) %d (mapped=%v)", cr.Node, continueNodeID, mapped)
	}
	continueNode := unit.Nodes()[continueNodeID-1]
	if continueNode.Kind != tir.Continue || continueNode.Target != tir.RegionID(whileRegion) {
		t.Fatalf("IR continue node = %+v, want Continue targeting region %d", continueNode, whileRegion)
	}
	if len(continueNode.DeferChain) != 1 || deferredPrint(t, unit, continueNode.DeferChain[0]) != deferredPrint(t, unit, cr.Defers[0]) {
		t.Fatalf("IR continue DeferChain = %v, accessor Defers = %v", continueNode.DeferChain, cr.Defers)
	}
	_ = inputs
}

func TestResultControlAccessorFailedResult(t *testing.T) {
	_, handoff, result := runAccessorFixture(t, failedAccessorSource)
	if result.Successful() {
		t.Fatal("fixture should fail validation")
	}
	ref := requireControlRef(t, handoff, func(c *controlRecord) bool { return c.Kind == controlReturn })
	control, ok := result.Control(ref)
	if !ok {
		t.Fatal("Control() must remain queryable in a failed result")
	}
	if control.Node != 0 {
		t.Fatalf("Node = %d, want 0 in a failed result", control.Node)
	}
	if len(control.Exits) != 1 || control.Exits[0].Kind != exitReturn {
		t.Fatalf("Exits = %+v, want exactly exitReturn in a failed result", control.Exits)
	}
	if control.Defers != nil {
		t.Fatalf("Defers = %v, want nil when no IR was published", control.Defers)
	}
}

// failedAccessorSource is a single program that fails 06b validation (writing
// to the let binding x) after every other record resolved successfully, so each
// accessor can prove a failed result stays queryable for final solved facts.
const failedAccessorSource = `
type Box = struct { value i32; };
fn take(payload ?i32) void {}
fn sink(value i32) void {}
fn main(box Box) i32 {
    let x i32 = 1;
    defer print 1;
    x = 2;
    let annotated ?i32 = some 3;
    sink(3);
    take(some 3);
    return box.value;
}
`
