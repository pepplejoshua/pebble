package check

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// irBuilderCoverageSeen accumulates node kinds across every unit this file's
// own tests build, so TestIRBuilderNodeKindCoverage can assert real,
// end-to-end coverage instead of a synthetic one. This only works because Go
// runs a single file's tests in declaration order and nothing in this file
// calls t.Parallel(): TestIRBuilderNodeKindCoverage must be declared after
// every test whose coverage it depends on, and the whole package suite
// (go test ./...) must run for the map to be populated — running it alone
// via -run sees an empty map and fails.
var irBuilderCoverageSeen = make(map[tir.NodeKind]string)

func recordIRBuilderUnit(unit *tir.Unit) {
	for _, node := range unit.Nodes() {
		irBuilderCoverageSeen[node.Kind] = node.Kind.String()
	}
}

func buildTestIRUnit(state *irBuildState) (*tir.Unit, error) {
	unit, err := state.builder.Build()
	if err == nil {
		recordIRBuilderUnit(unit)
	}
	return unit, err
}

func TestBuildUnitDeclarations(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let answer i32 = 1;\nfn main(value i32) i32 => value;\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, diagnostics, Config{}, inputs.Types)
	if !ok || unit == nil {
		t.Fatal("buildUnit rejected valid handoff")
	}
	if len(unit.Modules()) != 1 || len(unit.FunctionDeclarations()) != 1 || len(unit.GlobalDeclarations()) != 1 {
		t.Fatalf("unexpected containers: modules=%d funcs=%d globals=%d", len(unit.Modules()), len(unit.FunctionDeclarations()), len(unit.GlobalDeclarations()))
	}
	seenFunction, seenGlobal := false, false
	for _, n := range unit.Nodes() {
		switch n.Kind {
		case tir.FunctionDeclaration:
			seenFunction = true
		case tir.GlobalDeclaration:
			seenGlobal = true
		}
	}
	if !seenFunction || !seenGlobal {
		t.Fatal("declaration nodes missing")
	}
}

func TestBuildUnitExternBindingDeclarations(t *testing.T) {
	unit, ok := buildUnitFixture(t, `extern "C" { let external i32; var mutable i32; }`)
	if !ok || unit == nil {
		t.Fatal("buildUnit rejected extern bindings")
	}
	seen := 0
	for _, node := range unit.Nodes() {
		if node.Kind != tir.ExternDeclaration {
			continue
		}
		seen++
		if node.Convention != types.C {
			t.Fatalf("extern binding convention = %v, want C", node.Convention)
		}
	}
	if seen != 2 {
		t.Fatalf("extern binding declarations = %d, want 2", seen)
	}
}

func TestBuildUnitImport(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{
		"main.peb":   []byte("import \"./helper\";\nfn main() void { print helper::helper_fn(); }\n"),
		"helper.peb": []byte("fn helper_fn() i32 { return 42; }\n"),
	})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, diagnostics, Config{}, inputs.Types)
	if !ok || unit == nil {
		t.Fatal("buildUnit rejected valid import")
	}
	recordIRBuilderUnit(unit)
}

func TestBuildUnitLocalDeclaration(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void { var local i32 = 1; }\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, diagnostics, Config{}, inputs.Types)
	if !ok || unit == nil {
		t.Fatal("buildUnit rejected valid handoff")
	}

	var local *bindingRecord
	for _, retained := range handoff.Records.Records() {
		if retained.Binding != nil {
			sym, exists := inputs.Resolution.Symbols.Symbol(retained.Binding.Symbol)
			if exists && sym.Name == "local" {
				local = retained.Binding
				break
			}
		}
	}
	if local == nil {
		t.Fatal("local binding record missing")
	}
	typ, ok := typeOfValue(records, local.Annotation)
	if !ok || typ != inputs.Types.Builtins().I32 {
		t.Fatalf("local binding has type %v, want i32", typ)
	}
	for _, n := range unit.Nodes() {
		if n.Kind == tir.LocalDeclaration && n.Symbol == local.Symbol {
			return
		}
	}
	t.Fatal("local declaration node missing")
}

func TestBuildUnitRejectsGenerationErrors(t *testing.T) {
	unit, ok := buildUnit(&solveHandoff{GenerationHadErrors: true}, nil, nil, nil, Config{}, nil)
	if ok || unit != nil {
		t.Fatal("expected failed generation handoff to be rejected")
	}
}

func buildUnitFixture(t *testing.T, source string) (*tir.Unit, bool) {
	return buildUnitFixtureWithConfig(t, source, Config{})
}

func buildUnitFixtureWithConfig(t *testing.T, source string, config Config) (*tir.Unit, bool) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, config)
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid fixture: %v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(config))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(config))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, diagnostics, config, inputs.Types)
	if ok && unit != nil {
		recordIRBuilderUnit(unit)
	}
	return unit, ok
}

func nodesOfKind(unit *tir.Unit, kind tir.NodeKind) []tir.NodeID {
	var out []tir.NodeID
	for i, node := range unit.Nodes() {
		if node.Kind == kind {
			out = append(out, tir.NodeID(i+1))
		}
	}
	sort.SliceStable(out, func(i, j int) bool {
		return unit.Nodes()[out[i]-1].Span.Start < unit.Nodes()[out[j]-1].Span.Start
	})
	return out
}

func TestBuildUnitG2IfElseBothArmsReturn(t *testing.T) {
	unit, ok := buildUnitFixture(t, `fn choose(flag bool) i32 { if flag { return 1; } else { return 2; } }`)
	if !ok || unit == nil {
		t.Fatal("if/else fixture was not buildable")
	}
	ifNodes := nodesOfKind(unit, tir.If)
	if len(ifNodes) != 1 {
		t.Fatalf("If nodes = %d, want 1", len(ifNodes))
	}
	ifNode := unit.Nodes()[ifNodes[0]-1]
	if ifNode.Kind != tir.If || !ifNode.HasElse || ifNode.Region == 0 || len(ifNode.Children) != 3 {
		t.Fatalf("If node = %+v, want HasElse and condition/then/else children", ifNode)
	}
	condition := unit.Nodes()[ifNode.Children[0]-1]
	if condition.Kind != tir.SymbolValue {
		t.Fatalf("If condition = %+v, want SymbolValue", condition)
	}
	thenBlock := unit.Nodes()[ifNode.Children[1]-1]
	elseBlock := unit.Nodes()[ifNode.Children[2]-1]
	if thenBlock.Kind != tir.Block || len(thenBlock.Children) != 1 {
		t.Fatalf("then arm = %+v, want one-statement Block", thenBlock)
	}
	if elseBlock.Kind != tir.Block || len(elseBlock.Children) != 1 {
		t.Fatalf("else arm = %+v, want one-statement Block", elseBlock)
	}
	thenReturn := unit.Nodes()[thenBlock.Children[0]-1]
	elseReturn := unit.Nodes()[elseBlock.Children[0]-1]
	if thenReturn.Kind != tir.Return || thenReturn.Function == 0 || len(thenReturn.Children) != 1 {
		t.Fatalf("then return = %+v", thenReturn)
	}
	if elseReturn.Kind != tir.Return || elseReturn.Function == 0 || len(elseReturn.Children) != 1 {
		t.Fatalf("else return = %+v", elseReturn)
	}
	if thenReturn.Function != elseReturn.Function {
		t.Fatalf("returns target different functions: %d != %d", thenReturn.Function, elseReturn.Function)
	}
	if value := unit.Nodes()[thenReturn.Children[0]-1]; value.Kind != tir.IntegerLiteral || value.Literal.IntegerNum != "1" {
		t.Fatalf("then return value = %+v, want literal 1", value)
	}
	if value := unit.Nodes()[elseReturn.Children[0]-1]; value.Kind != tir.IntegerLiteral || value.Literal.IntegerNum != "2" {
		t.Fatalf("else return value = %+v, want literal 2", value)
	}
}

func TestBuildUnitG2WhileWithBreakInConditional(t *testing.T) {
	unit, ok := buildUnitFixture(t, `fn f(flag bool) void { while flag { if flag { break; } print 1; } }`)
	if !ok || unit == nil {
		t.Fatal("while fixture was not buildable")
	}
	whileNodes := nodesOfKind(unit, tir.While)
	if len(whileNodes) != 1 {
		t.Fatalf("While nodes = %d, want 1", len(whileNodes))
	}
	whileNode := unit.Nodes()[whileNodes[0]-1]
	if whileNode.Region == 0 || len(whileNode.Children) != 2 {
		t.Fatalf("While node = %+v, want region and condition/body children", whileNode)
	}
	condition := unit.Nodes()[whileNode.Children[0]-1]
	if condition.Kind != tir.SymbolValue {
		t.Fatalf("While condition = %+v, want SymbolValue", condition)
	}
	body := unit.Nodes()[whileNode.Children[1]-1]
	if body.Kind != tir.Block || len(body.Children) != 2 {
		t.Fatalf("While body = %+v, want If then Print", body)
	}
	ifNode := unit.Nodes()[body.Children[0]-1]
	if ifNode.Kind != tir.If || ifNode.HasElse || len(ifNode.Children) != 2 {
		t.Fatalf("body If = %+v, want no-else condition/then", ifNode)
	}
	if then := unit.Nodes()[ifNode.Children[1]-1]; then.Kind != tir.Block || len(then.Children) != 1 {
		t.Fatalf("body If then = %+v", then)
	} else {
		breakNode := unit.Nodes()[then.Children[0]-1]
		if breakNode.Kind != tir.Break || breakNode.Target == 0 {
			t.Fatalf("body If break = %+v, want Break with target", breakNode)
		}
		if breakNode.Target != whileNode.Region {
			t.Fatalf("break target %d, want enclosing while region %d", breakNode.Target, whileNode.Region)
		}
	}
	if printNode := unit.Nodes()[body.Children[1]-1]; printNode.Kind != tir.Print {
		t.Fatalf("while body second statement = %+v, want Print", printNode)
	}
}

func TestBuildUnitG2RangeLoopExclusiveAndInclusive(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f() void {
    loop 0..10 : x { print x; }
    loop 5..=7 : y { print y; }
}
`)
	if !ok || unit == nil {
		t.Fatal("range-loop fixture was not buildable")
	}
	rangeNodes := nodesOfKind(unit, tir.RangeLoop)
	if len(rangeNodes) != 2 {
		t.Fatalf("RangeLoop nodes = %d, want 2", len(rangeNodes))
	}
	exclusive := unit.Nodes()[rangeNodes[0]-1]
	inclusive := unit.Nodes()[rangeNodes[1]-1]
	if exclusive.Kind != tir.RangeLoop || exclusive.RangeInclusive || exclusive.Region == 0 || len(exclusive.Children) != 3 {
		t.Fatalf("exclusive RangeLoop = %+v", exclusive)
	}
	if inclusive.Kind != tir.RangeLoop || !inclusive.RangeInclusive || inclusive.Region == 0 || len(inclusive.Children) != 3 {
		t.Fatalf("inclusive RangeLoop = %+v", inclusive)
	}
	exclusiveStart := unit.Nodes()[exclusive.Children[0]-1]
	exclusiveEnd := unit.Nodes()[exclusive.Children[1]-1]
	if exclusiveStart.Kind != tir.IntegerLiteral || exclusiveStart.Literal.IntegerNum != "0" {
		t.Fatalf("exclusive start = %+v, want 0", exclusiveStart)
	}
	if exclusiveEnd.Kind != tir.IntegerLiteral || exclusiveEnd.Literal.IntegerNum != "10" {
		t.Fatalf("exclusive end = %+v, want 10", exclusiveEnd)
	}
	inclusiveStart := unit.Nodes()[inclusive.Children[0]-1]
	inclusiveEnd := unit.Nodes()[inclusive.Children[1]-1]
	if inclusiveStart.Kind != tir.IntegerLiteral || inclusiveStart.Literal.IntegerNum != "5" {
		t.Fatalf("inclusive start = %+v, want 5", inclusiveStart)
	}
	if inclusiveEnd.Kind != tir.IntegerLiteral || inclusiveEnd.Literal.IntegerNum != "7" {
		t.Fatalf("inclusive end = %+v, want 7", inclusiveEnd)
	}
	exclusiveBody := unit.Nodes()[exclusive.Children[2]-1]
	if exclusiveBody.Kind != tir.Block || len(exclusiveBody.Children) != 1 {
		t.Fatalf("exclusive body = %+v, want one-statement Block", exclusiveBody)
	}
	printNode := unit.Nodes()[exclusiveBody.Children[0]-1]
	if printNode.Kind != tir.Print || len(printNode.Children) != 1 {
		t.Fatalf("range-loop body statement = %+v, want Print", printNode)
	}
	iterator := unit.Nodes()[printNode.Children[0]-1]
	if iterator.Kind != tir.SymbolValue || iterator.Symbol == 0 {
		t.Fatalf("range iterator operand = %+v, want SymbolValue of the loop binding", iterator)
	}
	// Regression: buildRangeLoop used to build the RangeLoop node without
	// ever attaching the iterator's own symbol.SymbolID anywhere on it,
	// even though the prepare phase resolves and records it
	// (bindingRangeIterator, valueRangeIterator) — leaving no way for any
	// TIR consumer to know which symbol.SymbolID a SymbolValue referencing
	// the iterator inside the loop body actually names. The RangeLoop
	// node's own Symbol field must equal the iterator's symbol used in the
	// body.
	if exclusive.Symbol == 0 {
		t.Fatalf("RangeLoop Symbol = 0, want the range iterator's own symbol.SymbolID")
	}
	if exclusive.Symbol != iterator.Symbol {
		t.Fatalf("RangeLoop Symbol = %d, want %d (the same symbol the body's SymbolValue references)", exclusive.Symbol, iterator.Symbol)
	}
}

func TestBuildUnitG2ForWithClausesAndInfinite(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f(limit i32) i32 {
    var total i32 = 0;
    for var step i32 = 0; step < limit; step += 1 {
        total = total + step;
    }
    return total;
}
fn g() void {
    for ; ; { break; }
}
`)
	if !ok || unit == nil {
		t.Fatal("for-loop fixture was not buildable")
	}
	forNodes := nodesOfKind(unit, tir.For)
	if len(forNodes) != 2 {
		t.Fatalf("For nodes = %d, want 2", len(forNodes))
	}
	claused := unit.Nodes()[forNodes[0]-1]
	if claused.Kind != tir.For || claused.Region == 0 || len(claused.Children) != 4 {
		t.Fatalf("claused For = %+v, want initializer/condition/update/body", claused)
	}
	initializer := unit.Nodes()[claused.Children[0]-1]
	condition := unit.Nodes()[claused.Children[1]-1]
	update := unit.Nodes()[claused.Children[2]-1]
	body := unit.Nodes()[claused.Children[3]-1]
	if initializer.Kind != tir.Initialize || initializer.Symbol == 0 {
		t.Fatalf("for initializer = %+v, want Initialize", initializer)
	}
	if condition.Kind != tir.BinaryValue || condition.Operator != syntax.Less {
		t.Fatalf("for condition = %+v, want BinaryValue with <", condition)
	}
	if update.Kind != tir.CompoundStore || update.Operator != syntax.Plus || len(update.Children) != 2 {
		t.Fatalf("for update = %+v, want CompoundStore with +", update)
	}
	updatePlace := unit.Nodes()[update.Children[0]-1]
	updateValue := unit.Nodes()[update.Children[1]-1]
	if updatePlace.Kind != tir.StoragePlace || updateValue.Kind != tir.IntegerLiteral || updateValue.Literal.IntegerNum != "1" {
		t.Fatalf("for update children = %+v, %+v, want StoragePlace and literal 1", updatePlace, updateValue)
	}
	if body.Kind != tir.Block || len(body.Children) != 1 {
		t.Fatalf("for body = %+v, want one-statement Block", body)
	}
	infinite := unit.Nodes()[forNodes[1]-1]
	if infinite.Kind != tir.For || infinite.Region == 0 || len(infinite.Children) != 1 {
		t.Fatalf("infinite For = %+v, want single body child", infinite)
	}
	infiniteBody := unit.Nodes()[infinite.Children[0]-1]
	if infiniteBody.Kind != tir.Block || len(infiniteBody.Children) != 1 {
		t.Fatalf("infinite for body = %+v", infiniteBody)
	}
	breakNode := unit.Nodes()[infiniteBody.Children[0]-1]
	if breakNode.Kind != tir.Break || breakNode.Target == 0 {
		t.Fatalf("infinite for break = %+v, want Break with target", breakNode)
	}
	if breakNode.Target != infinite.Region {
		t.Fatalf("infinite for break target %d, want loop region %d", breakNode.Target, infinite.Region)
	}
}

func TestBuildUnitG2NestedContinueTargetsEnclosingLoop(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f(flag bool) void {
    while flag {
        while true { continue; }
        print 1;
    }
}
`)
	if !ok || unit == nil {
		t.Fatal("nested-loop fixture was not buildable")
	}
	whileNodes := nodesOfKind(unit, tir.While)
	if len(whileNodes) != 2 {
		t.Fatalf("While nodes = %d, want 2", len(whileNodes))
	}
	continueNodes := nodesOfKind(unit, tir.Continue)
	if len(continueNodes) != 1 {
		t.Fatalf("Continue nodes = %d, want 1", len(continueNodes))
	}
	continueNode := unit.Nodes()[continueNodes[0]-1]
	if continueNode.Target == 0 {
		t.Fatal("continue has no target")
	}
	outer := unit.Nodes()[whileNodes[0]-1]
	inner := unit.Nodes()[whileNodes[1]-1]
	if outer.Region == inner.Region {
		t.Fatal("outer and inner loops share a region")
	}
	if continueNode.Target != inner.Region {
		t.Fatalf("continue target %d, want inner loop region %d", continueNode.Target, inner.Region)
	}
	innerBody := unit.Nodes()[inner.Children[1]-1]
	if innerBody.Kind != tir.Block || len(innerBody.Children) != 1 {
		t.Fatalf("inner loop body = %+v", innerBody)
	}
	if direct := innerBody.Children[0]; direct != continueNodes[0] {
		t.Fatalf("inner loop body child = %d, want the Continue %d", direct, continueNodes[0])
	}
	outerBody := unit.Nodes()[outer.Children[1]-1]
	if outerBody.Kind != tir.Block || len(outerBody.Children) != 2 {
		t.Fatalf("outer loop body = %+v, want inner loop then Print", outerBody)
	}
	if first := outerBody.Children[0]; first != whileNodes[1] {
		t.Fatalf("outer body first child = %d, want inner While %d", first, whileNodes[1])
	}
	if second := unit.Nodes()[outerBody.Children[1]-1]; second.Kind != tir.Print {
		t.Fatalf("outer body second child = %+v, want Print", unit.Nodes()[outerBody.Children[1]-1])
	}
}

func TestBuildUnitG2CompoundStore(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f() void {
    var arr [3]i32 = [1; 3];
    var i i32 = 0;
    arr[i] += 1;
}
`)
	if !ok || unit == nil {
		t.Fatal("compound-store fixture was not buildable")
	}
	compoundNodes := nodesOfKind(unit, tir.CompoundStore)
	if len(compoundNodes) != 1 {
		t.Fatalf("CompoundStore nodes = %d, want 1", len(compoundNodes))
	}
	compound := unit.Nodes()[compoundNodes[0]-1]
	if compound.Operator != syntax.Plus || len(compound.Children) != 2 {
		t.Fatalf("CompoundStore = %+v, want + operator and place/value children", compound)
	}
	place := unit.Nodes()[compound.Children[0]-1]
	value := unit.Nodes()[compound.Children[1]-1]
	if place.Kind != tir.CheckedIndexPlace || len(place.Children) != 2 {
		t.Fatalf("compound place = %+v, want CheckedIndexPlace", place)
	}
	if value.Kind != tir.IntegerLiteral || value.Literal.IntegerNum != "1" {
		t.Fatalf("compound value = %+v, want literal 1", value)
	}
	index := unit.Nodes()[place.Children[1]-1]
	if index.Kind != tir.SymbolValue || index.Symbol == 0 {
		t.Fatalf("compound index operand = %+v, want SymbolValue", index)
	}
}

func TestBuildUnitG1StatementsAndFunctionBody(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn sink(value i32) void { print value; }
fn main() i32 {
    var value i32 = 1;
    value = value + 1;
    sink(value);
    print value;
    return value;
}
`)
	if !ok || unit == nil {
		t.Fatal("G1 fixture was not buildable")
	}
	var main tir.FunctionDecl
	for _, decl := range unit.FunctionDeclarations() {
		for _, node := range unit.Nodes() {
			if node.Kind == tir.FunctionDeclaration && node.Symbol == decl.Symbol && node.HasBody {
				main = decl
			}
		}
	}
	if main.Node == 0 {
		t.Fatal("function declaration has no body block")
	}
	block := unit.Nodes()[main.Node-1]
	if block.Kind != tir.Block || len(block.Children) != 5 {
		t.Fatalf("main block = %+v, want five ordered statements", block)
	}
	want := []tir.NodeKind{tir.Initialize, tir.Store, tir.ExpressionStatement, tir.Print, tir.Return}
	for i, child := range block.Children {
		if got := unit.Nodes()[child-1].Kind; got != want[i] {
			t.Fatalf("statement %d = %v, want %v", i, got, want[i])
		}
	}
}

func TestBuildUnitG1ImplicitVoidReturn(t *testing.T) {
	unit, ok := buildUnitFixture(t, `fn main() void { print 1; }`)
	if !ok || unit == nil {
		t.Fatal("void fixture was not buildable")
	}
	for _, decl := range unit.FunctionDeclarations() {
		if decl.Node == 0 {
			continue
		}
		block := unit.Nodes()[decl.Node-1]
		if len(block.Children) == 0 || unit.Nodes()[block.Children[len(block.Children)-1]-1].Kind != tir.ImplicitReturn {
			t.Fatalf("void block children = %v, missing implicit return", block.Children)
		}
		return
	}
	t.Fatal("function declaration not found")
}

func TestBuildUnitG3ImplicitReturnDeferChain(t *testing.T) {
	unit, ok := buildUnitFixture(t, `fn f() void { defer print 1; }`)
	if !ok || unit == nil {
		t.Fatal("implicit-return defer fixture was not buildable")
	}
	registers := nodesOfKind(unit, tir.DeferRegister)
	if len(registers) != 1 {
		t.Fatalf("DeferRegister nodes = %d, want 1", len(registers))
	}
	if got := printedInteger(t, unit, deferRegisterChild(t, unit, registers[0])); got != "1" {
		t.Fatalf("deferred statement prints %s, want 1", got)
	}
	block := functionBody(t, unit)
	if len(block.Children) != 2 {
		t.Fatalf("block children = %v, want DeferRegister then ImplicitReturn", block.Children)
	}
	implicit := unit.Nodes()[block.Children[1]-1]
	if implicit.Kind != tir.ImplicitReturn {
		t.Fatalf("second child = %+v, want ImplicitReturn", implicit)
	}
	if len(implicit.DeferChain) != 1 || implicit.DeferChain[0] != registers[0] {
		t.Fatalf("ImplicitReturn DeferChain = %v, want [%d]", implicit.DeferChain, registers[0])
	}
}

func TestBuildUnitG3ImplicitReturnNestedIfDefers(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f() void {
    defer print 1;
    if true { defer print 2; return; }
}
`)
	if !ok || unit == nil {
		t.Fatal("nested implicit-return defer fixture was not buildable")
	}
	returnNodes := nodesOfKind(unit, tir.Return)
	if len(returnNodes) != 1 {
		t.Fatalf("Return nodes = %d, want 1", len(returnNodes))
	}
	returnNode := unit.Nodes()[returnNodes[0]-1]
	if len(returnNode.DeferChain) != 2 {
		t.Fatalf("Return DeferChain = %v, want 2 entries", returnNode.DeferChain)
	}
	for i, id := range returnNode.DeferChain {
		want := []string{"2", "1"}[i]
		if got := printedInteger(t, unit, deferRegisterChild(t, unit, id)); got != want {
			t.Fatalf("return defer chain entry %d runs print %s, want %s", i, got, want)
		}
	}
	block := functionBody(t, unit)
	implicit := unit.Nodes()[block.Children[len(block.Children)-1]-1]
	if implicit.Kind != tir.ImplicitReturn {
		t.Fatalf("last child = %+v, want ImplicitReturn", implicit)
	}
	if len(implicit.DeferChain) != 1 {
		t.Fatalf("ImplicitReturn DeferChain = %v, want outer defer only", implicit.DeferChain)
	}
	if got := printedInteger(t, unit, deferRegisterChild(t, unit, implicit.DeferChain[0])); got != "1" {
		t.Fatalf("implicit-return defer chain runs print %s, want 1", got)
	}
}

func TestBuildUnitG2BuildsIfDeferredFromG1(t *testing.T) {
	unit, ok := buildUnitFixture(t, `fn main(flag bool) void { if flag { print 1; } }`)
	if !ok || unit == nil {
		t.Fatal("if-containing function must build now that G2 owns If")
	}
	ifNodes := nodesOfKind(unit, tir.If)
	if len(ifNodes) != 1 {
		t.Fatalf("If nodes = %d, want 1", len(ifNodes))
	}
	ifNode := unit.Nodes()[ifNodes[0]-1]
	if ifNode.Region == 0 || ifNode.HasElse || len(ifNode.Children) != 2 {
		t.Fatalf("If node = %+v, want region, no else, condition plus then arm", ifNode)
	}
	if child := unit.Nodes()[ifNode.Children[0]-1]; child.Kind != tir.SymbolValue {
		t.Fatalf("If condition = %+v, want SymbolValue", child)
	}
	thenBlock := unit.Nodes()[ifNode.Children[1]-1]
	if thenBlock.Kind != tir.Block || len(thenBlock.Children) != 1 {
		t.Fatalf("If then arm = %+v, want one-statement Block", thenBlock)
	}
	printNode := unit.Nodes()[thenBlock.Children[0]-1]
	if printNode.Kind != tir.Print || len(printNode.Children) != 1 {
		t.Fatalf("If then statement = %+v, want Print of one operand", printNode)
	}
}

func testIRBuildState(t *testing.T, handoff *solveHandoff, records *solvedRecords, requirements map[symbol.SymbolID][]Requirement) *irBuildState {
	t.Helper()
	b := tir.NewBuilder(handoff.Semantics.Types(), tir.Config{
		MaxIRNodes: DefaultMaxIRNodes, MaxIRComponents: DefaultMaxIRComponents,
		MaxDumpBytes: DefaultMaxDumpBytes,
	})
	state := &irBuildState{handoff: handoff, records: records, builder: b, irBuildScope: newIRBuildScope()}
	if !state.buildModules() || !state.buildTypes() || !state.buildDeclarations() || !state.buildTypeUses() || !state.indexExpressions() || !state.indexControls() || !state.buildBlocks() || !state.finishFunctionDeclarations() || !state.buildRequirements(requirements) {
		t.Fatal("failed to build test IR state")
	}
	return state
}

func TestIRBuildStateWithFreshScope(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() i32 { return 1; }\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	state := testIRBuildState(t, handoff, records, requirements)
	if len(state.values) == 0 || len(state.blockNodes) == 0 {
		t.Fatal("normal build did not populate scope memoization")
	}
	outer := state.irBuildScope
	var outerValue valueID
	for id := range outer.values {
		outerValue = id
		break
	}
	synthetic := valueID(^uint32(0))
	sentinel := tir.NodeID(^uint32(0))
	got, buildOK := state.withFreshScope(func() (tir.NodeID, bool) {
		if state.irBuildScope == outer {
			t.Fatal("fresh build reused the outer scope")
		}
		if len(state.values) != 0 || len(state.blockNodes) != 0 {
			t.Fatal("fresh scope was not empty")
		}
		if _, exists := state.values[outerValue]; exists {
			t.Fatal("fresh scope contains an outer value")
		}
		state.values[synthetic] = sentinel
		return sentinel, true
	})
	if !buildOK || got != sentinel {
		t.Fatalf("withFreshScope result = (%d, %t), want (%d, true)", got, buildOK, sentinel)
	}
	if state.irBuildScope != outer {
		t.Fatal("withFreshScope did not restore the outer scope")
	}
	if got := state.values[outerValue]; got == 0 {
		t.Fatal("outer value was lost after scope swap")
	}
	if _, exists := state.values[synthetic]; exists {
		t.Fatal("synthetic value leaked into outer scope")
	}
}

func requireValueID(t *testing.T, handoff *solveHandoff, records *solvedRecords, predicate func(*expressionRecord) bool) valueID {
	t.Helper()
	for _, retained := range handoff.Records.Records() {
		if retained.Expression == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		if predicate(retained.Expression) {
			if _, ok := records.Root(retained.Expression.Result); ok {
				return retained.Expression.Result
			}
		}
	}
	t.Fatal("matching expression record not found")
	return 0
}

func TestIRBuildStateResolveTypeSubstitution(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn identity[T](value T) T => value;`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	state := testIRBuildState(t, handoff, records, requirements)
	state.store = inputs.Types

	var parameter valueID
	for _, retained := range handoff.Records.Records() {
		if retained.Callable != nil && retained.Callable.Parameters != nil {
			parameter = retained.Callable.Parameters[0]
			break
		}
	}
	if parameter == 0 {
		t.Fatal("generic parameter value missing")
	}
	direct, ok := typeOfValue(records, parameter)
	if !ok {
		t.Fatal("generic parameter has no symbolic type")
	}
	resolved, ok := state.resolveType(parameter)
	if !ok || resolved != direct {
		t.Fatalf("nil substitution resolved type = %v, %v; want %v, true", resolved, ok, direct)
	}
	key, ok := inputs.Types.Key(direct)
	if !ok {
		t.Fatal("symbolic parameter type is not interned")
	}
	declaration, ok := key.TypeParameter()
	if !ok {
		t.Fatalf("symbolic parameter type key = %v, want type parameter", key)
	}
	state.activeSubstitution = map[symbol.SymbolID]types.TypeID{declaration: inputs.Types.Builtins().I32}
	resolved, ok = state.resolveType(parameter)
	if !ok || resolved != inputs.Types.Builtins().I32 {
		t.Fatalf("active substitution resolved type = %v, %v; want i32, true", resolved, ok)
	}
}

func TestBuildValueLeafLiterals(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let b bool = true;
let c char = 'a';
let s str = "hello";
let i i32 = 0x2a;
let j i32 = 1_000;
let f f64 = 3.14_15;
let p *i32 = nil;
let o ?i32 = none;
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	// nil/none currently leave T0510 diagnostics, but the expression records and
	// their solved types are still present and buildable by buildValue.
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	state := testIRBuildState(t, handoff, records, requirements)

	boolID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalBool && e.Literal.Bool
	})
	charID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalChar && e.Literal.Rune == 'a'
	})
	stringID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalString && e.Literal.Text == "hello"
	})
	hexID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalInteger && string(e.Literal.NumericBytes) == "0x2a"
	})
	underscoreID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalInteger && string(e.Literal.NumericBytes) == "1_000"
	})
	floatID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalFloat && string(e.Literal.NumericBytes) == "3.14_15"
	})
	nilID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalNil
	})
	noneID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalNone
	})

	cases := []struct {
		id   valueID
		want tir.NodeKind
		num  string
		den  string
		str  string
		fl   string
		b    bool
		ch   rune
	}{
		{boolID, tir.BoolLiteral, "", "", "", "", true, 0},
		{charID, tir.CharLiteral, "", "", "", "", false, 'a'},
		{stringID, tir.StringLiteral, "", "", "hello", "", false, 0},
		{hexID, tir.IntegerLiteral, "42", "1", "", "", false, 0},
		{underscoreID, tir.IntegerLiteral, "1000", "1", "", "", false, 0},
		{floatID, tir.FloatLiteral, "", "", "", "3.1415", false, 0},
		{nilID, tir.NilPointer, "", "", "", "", false, 0},
		{noneID, tir.NoneOptional, "", "", "", "", false, 0},
	}
	for _, tc := range cases {
		_, ok := state.buildValue(tc.id)
		if !ok {
			t.Fatalf("buildValue failed for %v", tc.want)
		}
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	for _, tc := range cases {
		nid, _ := state.buildValue(tc.id)
		node := unit.Nodes()[nid-1]
		if node.Kind != tc.want {
			t.Fatalf("kind mismatch: got %v, want %v", node.Kind, tc.want)
		}
		if node.Literal.IntegerNum != tc.num {
			t.Fatalf("IntegerNum mismatch for %v: got %q, want %q", tc.want, node.Literal.IntegerNum, tc.num)
		}
		if node.Literal.IntegerDen != tc.den {
			t.Fatalf("IntegerDen mismatch for %v: got %q, want %q", tc.want, node.Literal.IntegerDen, tc.den)
		}
		if node.Literal.String != tc.str {
			t.Fatalf("String mismatch for %v: got %q, want %q", tc.want, node.Literal.String, tc.str)
		}
		if node.Literal.Float != tc.fl {
			t.Fatalf("Float mismatch for %v: got %q, want %q", tc.want, node.Literal.Float, tc.fl)
		}
		if node.Literal.Bool != tc.b {
			t.Fatalf("Bool mismatch for %v: got %v, want %v", tc.want, node.Literal.Bool, tc.b)
		}
		if node.Literal.Char != tc.ch {
			t.Fatalf("Char mismatch for %v: got %v, want %v", tc.want, node.Literal.Char, tc.ch)
		}
	}
}

func TestBuildValueSymbolAndEnumVariant(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Color = enum { red, blue };
let color Color = Color.red;
fn double(value i32) i32 => value;
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	state := testIRBuildState(t, handoff, records, requirements)

	var redSymbol, valueSymbol symbol.SymbolID
	for _, sym := range inputs.Resolution.Symbols.All() {
		if sym.Name == "red" && sym.Kind == symbol.SymbolVariant {
			redSymbol = sym.ID
		}
		if sym.Name == "value" && sym.Kind == symbol.SymbolParameter {
			valueSymbol = sym.ID
		}
	}
	if redSymbol == 0 || valueSymbol == 0 {
		t.Fatalf("missing symbols: red=%d value=%d", redSymbol, valueSymbol)
	}

	redID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionMember && e.Symbol == redSymbol
	})
	valueID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionName && e.Symbol == valueSymbol
	})

	if _, ok := state.buildValue(redID); !ok {
		t.Fatal("buildValue failed for enum variant")
	}
	if _, ok := state.buildValue(valueID); !ok {
		t.Fatal("buildValue failed for symbol value")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}

	redNode, _ := state.buildValue(redID)
	node := unit.Nodes()[redNode-1]
	if node.Kind != tir.EnumVariantValue || node.Member != redSymbol {
		t.Fatalf("enum variant node = %+v", node)
	}

	valueNode, _ := state.buildValue(valueID)
	node = unit.Nodes()[valueNode-1]
	if node.Kind != tir.SymbolValue || node.Symbol != valueSymbol {
		t.Fatalf("symbol value node = %+v", node)
	}
}

func TestBuildValueContextAndSizeof(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn contextful() void {
    context;
    let width uint = sizeof i32;
}
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	state := testIRBuildState(t, handoff, records, requirements)

	contextID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionContext
	})
	sizeofID := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionSizeof
	})

	if _, ok := state.buildValue(contextID); !ok {
		t.Fatal("buildValue failed for context")
	}
	if _, ok := state.buildValue(sizeofID); !ok {
		t.Fatal("buildValue failed for sizeof")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}

	ctxNode, _ := state.buildValue(contextID)
	node := unit.Nodes()[ctxNode-1]
	if node.Kind != tir.ContextValue || node.ContextAction != tir.ContextExpr {
		t.Fatalf("context node = %+v", node)
	}

	sizeofNode, _ := state.buildValue(sizeofID)
	node = unit.Nodes()[sizeofNode-1]
	if node.Kind != tir.SizeofType || node.TypeArg != inputs.Types.Builtins().I32 {
		t.Fatalf("sizeof node = %+v, want TypeArg=%d", node, inputs.Types.Builtins().I32)
	}
}

func TestBuildValueMemoizes(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let answer i32 = 42;\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	state := testIRBuildState(t, handoff, records, requirements)

	id := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalInteger
	})
	first, ok := state.buildValue(id)
	if !ok {
		t.Fatal("first buildValue failed")
	}
	second, ok := state.buildValue(id)
	if !ok {
		t.Fatal("second buildValue failed")
	}
	if first != second {
		t.Fatalf("buildValue not memoized: %d != %d", first, second)
	}
}

func TestBuildValueInactiveGuardedExpression(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let answer i32 = 42;\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	id := requireValueID(t, handoff, records, func(e *expressionRecord) bool {
		return e.Kind == expressionLiteral && e.Literal.Kind == literalInteger
	})
	for i := range handoff.Records.values {
		if handoff.Records.values[i].Expression != nil && handoff.Records.values[i].Expression.Result == id {
			handoff.Records.values[i].Header.Alternative = alternativeTag{Guarded: true, Choice: 999999, Index: 1}
		}
	}
	state := testIRBuildState(t, handoff, records, requirements)
	if _, ok := state.buildValue(id); ok {
		t.Fatal("buildValue built an inactive guarded expression")
	}
}

func testBuildValue(t *testing.T, source string) (*irBuildState, *solvedRecords) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	state := testIRBuildState(t, handoff, records, requirements)
	return state, records
}

func TestBuildValueTuple(t *testing.T) {
	state, records := testBuildValue(t, "let tuple (i32, i32) = (1, 2);")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionTuple })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.TupleValue || len(node.Children) != 2 {
		t.Fatalf("tuple node = %+v", node)
	}
}

func TestBuildUnitAddressOfCoverage(t *testing.T) {
	unit, ok := buildUnitFixture(t, `fn main() i32 {
    var value i32 = 5;
    let pointer *i32 = &value;
    return *pointer;
}`)
	if !ok || unit == nil {
		t.Fatal("address-of fixture was not buildable")
	}
	if len(nodesOfKind(unit, tir.AddressOf)) != 1 {
		t.Fatal("address-of node missing")
	}
}

func TestBuildValueArray(t *testing.T) {
	state, records := testBuildValue(t, "let inferred = [1, 2, 3];")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionArray })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.ArrayValue || len(node.Children) != 3 {
		t.Fatalf("array node = %+v", node)
	}
}

// TestBuildValueArrayLiteralAgainstKnownDestination is a regression test for a
// bug in prepareArray (aggregate_facts.go): the function used to
// unconditionally call session.Variable — which registers a real solver cell
// that must later be resolved — and then, only when a known destination
// element type existed, discard that cell in favor of session.Known instead.
// The abandoned Variable cell was never bound to anything, so it was reported
// as a spurious T0510 "inference variable has no unique semantic type" for
// every array literal checked against an explicitly known destination type
// (an explicit `[N]T` local annotation, a function return type, etc) — the
// unannotated case (`let a = [1, 2, 3]`) never hit this path and always
// worked, which is why it went unnoticed. Every one of these must build
// successfully.
func TestBuildValueArrayLiteralAgainstKnownDestination(t *testing.T) {
	sources := []string{
		"let a [2]i32 = [10, 20];",
		"let a [3]bool = [true, false, true];",
		"var a [2]i32 = [10, 20];",
	}
	for _, source := range sources {
		t.Run(source, func(t *testing.T) {
			state, records := testBuildValue(t, source)
			id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionArray })
			if _, ok := state.buildValue(id); !ok {
				t.Fatal("buildValue failed")
			}
			if _, err := buildTestIRUnit(state); err != nil {
				t.Fatalf("Build failed: %v", err)
			}
		})
	}
}

func TestBuildValueArrayRepeat(t *testing.T) {
	state, records := testBuildValue(t, "let repeated [5]i32 = [1; 5];\n")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionArrayRepeat })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.ArrayRepeat || len(node.Children) != 2 {
		t.Fatalf("array repeat node = %+v", node)
	}
	count := unit.Nodes()[node.Children[1]-1]
	if count.Kind != tir.IntegerLiteral || count.Literal.IntegerNum != "5" {
		t.Fatalf("count node = %+v", count)
	}
}

func TestBuildValueRecordConstruct(t *testing.T) {
	state, records := testBuildValue(t, "type Point = struct { x i32; y i32; };\nlet point Point = Point.{ x = 1, y = 2 };")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionRecordValue })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.RecordConstruct || len(node.Fields) != 2 {
		t.Fatalf("record node = %+v", node)
	}
}

func TestBuildValueCheckedIntegerNegation(t *testing.T) {
	state, records := testBuildValue(t, "let neg i32 = -1;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionPrefix })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.CheckedNegate || node.Operator != syntax.Minus || len(node.Children) != 1 {
		t.Fatalf("checked negate node = %+v", node)
	}
}

func TestBuildValueFloatNegationRemainsUnchecked(t *testing.T) {
	state, records := testBuildValue(t, "let neg f64 = -1.0;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionPrefix })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.PrefixValue || node.Operator != syntax.Minus || len(node.Children) != 1 {
		t.Fatalf("float prefix node = %+v", node)
	}
}

func TestBuildValueBinaryNumeric(t *testing.T) {
	state, records := testBuildValue(t, "let sum i32 = 1 + 2;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionBinary })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.CheckedArithmetic || node.Operator != syntax.Plus || len(node.Children) != 2 {
		t.Fatalf("binary node = %+v", node)
	}
}

func TestBuildValueFloatBinaryNumeric(t *testing.T) {
	state, records := testBuildValue(t, "let sum f32 = 1.0 + 2.0;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionBinary })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.BinaryValue || node.Operator != syntax.Plus || len(node.Children) != 2 {
		t.Fatalf("binary node = %+v", node)
	}
}

func TestBuildValueCheckedIntegralOperators(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		token    syntax.TokenKind
		wantKind tir.NodeKind
	}{
		{name: "modulo", source: "let value i32 = 5 % 2;", token: syntax.Percent, wantKind: tir.CheckedArithmetic},
		{name: "bitwise and", source: "let value i32 = 5 & 2;", token: syntax.Ampersand, wantKind: tir.BinaryValue},
		{name: "shift left", source: "let value i32 = 5 << 2;", token: syntax.ShiftLeft, wantKind: tir.CheckedShift},
		{name: "shift right", source: "let value i32 = 5 >> 2;", token: syntax.ShiftRight, wantKind: tir.CheckedShift},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			state, records := testBuildValue(t, tt.source)
			id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionBinary })
			nid, ok := state.buildValue(id)
			if !ok {
				t.Fatal("buildValue failed")
			}
			unit, err := buildTestIRUnit(state)
			if err != nil {
				t.Fatalf("Build failed: %v", err)
			}
			node := unit.Nodes()[nid-1]
			if node.Kind != tt.wantKind || node.Operator != tt.token || len(node.Children) != 2 {
				t.Fatalf("operator node = %+v", node)
			}
		})
	}
}

func TestBuildValueShortCircuit(t *testing.T) {
	state, records := testBuildValue(t, "let both bool = true && false;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionBinary })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.ShortCircuitValue || node.Operator != syntax.LogicalAnd || len(node.Children) != 2 {
		t.Fatalf("short-circuit node = %+v", node)
	}
}

func TestBuildValueSourceAlias(t *testing.T) {
	state, records := testBuildValue(t, "let grouped i32 = (1);")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionGrouped })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.SourceAlias || node.ExplicitCast || len(node.Children) != 1 {
		t.Fatalf("grouped node = %+v", node)
	}
}

func TestBuildValueInterpolatedString(t *testing.T) {
	state, records := testBuildValue(t, "let name str = \"Ada\";\nlet count i32 = 2;\nlet msg str = `hello {name}, you have {count} items!`;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionInterpolated })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.InterpolatedString || len(node.Parts) != 5 || len(node.Children) != 0 {
		t.Fatalf("interpolated node = %+v", node)
	}
	wantText := []struct {
		index int
		text  string
	}{{0, "hello "}, {2, ", you have "}, {4, " items!"}}
	for _, want := range wantText {
		part := node.Parts[want.index]
		if part.Kind != tir.InterpolationTextPart || part.Text != want.text || part.Value != 0 {
			t.Fatalf("interpolated text part[%d] = %+v", want.index, part)
		}
	}
	for _, index := range []int{1, 3} {
		part := node.Parts[index]
		if part.Kind != tir.InterpolationValuePart || part.Value == 0 || int(part.Value) > len(unit.Nodes()) {
			t.Fatalf("interpolated value part[%d] = %+v", index, part)
		}
		value := unit.Nodes()[part.Value-1]
		if value.Kind != tir.SymbolValue || value.Symbol == 0 {
			t.Fatalf("interpolated value node[%d] = %+v", index, value)
		}
	}
}

func TestBuildValueNestedComposites(t *testing.T) {
	state, records := testBuildValue(t, "let nested = ((1 + 2), [3, 4]);\n")
	tupleID := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionTuple })
	tupleNID, ok := state.buildValue(tupleID)
	if !ok {
		t.Fatal("buildValue failed for nested tuple")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	tupleNode := unit.Nodes()[tupleNID-1]
	if tupleNode.Kind != tir.TupleValue || len(tupleNode.Children) != 2 {
		t.Fatalf("nested tuple node = %+v", tupleNode)
	}
	aliasNode := unit.Nodes()[tupleNode.Children[0]-1]
	if aliasNode.Kind != tir.SourceAlias || len(aliasNode.Children) != 1 {
		t.Fatalf("alias child = %+v", aliasNode)
	}
	binaryNode := unit.Nodes()[aliasNode.Children[0]-1]
	if binaryNode.Kind != tir.CheckedArithmetic {
		t.Fatalf("binary child = %+v", binaryNode)
	}
	arrayNode := unit.Nodes()[tupleNode.Children[1]-1]
	if arrayNode.Kind != tir.ArrayValue || len(arrayNode.Children) != 2 {
		t.Fatalf("array child = %+v", arrayNode)
	}
}

func TestBuildValueInactiveComposite(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let inferred = [1, 2];\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	id := requireValueID(t, handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionArray })
	for i := range handoff.Records.values {
		if handoff.Records.values[i].Expression != nil && handoff.Records.values[i].Expression.Result == id {
			handoff.Records.values[i].Header.Alternative = alternativeTag{Guarded: true, Choice: 999999, Index: 1}
		}
	}
	state := testIRBuildState(t, handoff, records, requirements)
	if _, ok := state.buildValue(id); ok {
		t.Fatal("buildValue built an inactive guarded composite")
	}
}

func TestBuildValuePlacesAndLoads(t *testing.T) {
	state, records := testBuildValue(t, `
type Point = struct { x i32; y i32; };
var mutable Point = Point.{ x = 1, y = 2 };
let immutable Point = Point.{ x = 3, y = 4 };
let field i32 = mutable.x;
let rvalueField i32 = (Point.{ x = 9, y = 10 }).x;
let tuple (i32, i32) = (5, 6);
let component i32 = tuple.1;
let directComponent i32 = (7, 8).1;
fn indexed(array [2]i32) i32 => array[0];
fn deref(p *i32) i32 => *p;
`)
	ids := make([]valueID, 0)
	for _, retained := range state.handoff.Records.Records() {
		if retained.Expression != nil && (retained.Expression.Kind == expressionMember || retained.Expression.Kind == expressionBracket || retained.Expression.Kind == expressionPrefix) {
			ids = append(ids, retained.Expression.Result)
		}
	}
	if len(ids) < 4 {
		t.Fatalf("projected expressions = %d", len(ids))
	}
	for _, id := range ids {
		if _, ok := state.buildValue(id); !ok {
			e := state.expressionsByResult[id]
			t.Fatalf("buildValue failed for %d kind=%v op=%+v member=%+v children=%v", id, e.Kind, state.operatorsByResult[id], state.membersByResult[id], e.Children)
		}
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatal(err)
	}
	seen := map[tir.NodeKind]bool{}
	seenWritable, seenReadOnly := false, false
	for _, id := range ids {
		node := unit.Nodes()[state.values[id]-1]
		seen[node.Kind] = true
		if node.Kind == tir.Load && len(node.Children) != 1 {
			t.Fatalf("load node = %+v", node)
		}
	}
	for _, node := range unit.Nodes() {
		if node.Kind == tir.StoragePlace {
			seenWritable = seenWritable || node.Writable
			seenReadOnly = seenReadOnly || !node.Writable
		}
	}
	if !seenWritable || !seenReadOnly {
		t.Fatal("storage place writability was not preserved")
	}
	for _, kind := range []tir.NodeKind{tir.Load, tir.FieldValue} {
		if !seen[kind] {
			t.Fatalf("missing %v", kind)
		}
	}
	_ = records
}

func TestBuildValueStringIndexIsNotPlace(t *testing.T) {
	state, records := testBuildValue(t, `fn index(text str, index i32) *char => text[index];`)
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionBracket })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("string indexing failed to build")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatal(err)
	}
	if node := unit.Nodes()[nid-1]; node.Kind != tir.CheckedIndex || len(node.Children) != 2 {
		t.Fatalf("string index node = %+v", node)
	}
}

func TestBuildValueCastNodes(t *testing.T) {
	state, records := testBuildValue(t, `
let ii i64 = 1 as i64;
let ifl f64 = 1 as f64;
let fi i32 = 1.0 as i32;
let ff f32 = 1.0 as f32;
	fn identity(value i32) i32 => value as i32;
`)
	_ = records
	var ids []valueID
	for _, retained := range state.handoff.Records.Records() {
		if retained.Expression != nil && retained.Expression.Kind == expressionCast {
			ids = append(ids, retained.Expression.Result)
		}
	}
	if len(ids) != 5 {
		t.Fatalf("cast expressions = %d, want 5", len(ids))
	}
	for _, id := range ids {
		if _, ok := state.buildValue(id); !ok {
			t.Fatalf("cast %d failed to build", id)
		}
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatal(err)
	}
	seen := map[tir.NodeKind]bool{}
	for _, id := range ids {
		seen[unit.Nodes()[state.values[id]-1].Kind] = true
	}
	for _, kind := range []tir.NodeKind{tir.IntegerCast, tir.IntegerToFloat, tir.FloatToInteger, tir.FloatCast, tir.SourceAlias} {
		if !seen[kind] {
			t.Fatalf("missing cast node kind %v", kind)
		}
	}
}

func TestBuildValuePointerCastNode(t *testing.T) {
	state, _ := testBuildValue(t, `
fn main() i32 {
    var y i32 = 42;
    let p *i32 = &y;
    let q *void = p as *void;
    return 0;
}`)
	var castIDs []valueID
	for _, retained := range state.handoff.Records.Records() {
		if retained.Expression != nil && retained.Expression.Kind == expressionCast {
			castIDs = append(castIDs, retained.Expression.Result)
		}
	}
	if len(castIDs) != 1 {
		t.Fatalf("cast expressions = %d, want 1", len(castIDs))
	}
	for _, id := range castIDs {
		if _, ok := state.buildValue(id); !ok {
			t.Fatalf("cast %d failed to build", id)
		}
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatal(err)
	}
	for _, id := range castIDs {
		node := unit.Nodes()[state.values[id]-1]
		if node.Kind != tir.PointerCast {
			t.Fatalf("pointer cast node kind = %v, want PointerCast", node.Kind)
		}
	}
}

func TestBuildValueImplicitPointerCastRejected(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn main() i32 {
    var y i32 = 42;
    let p *i32 = &y;
    let q *i64 = p;
    return 0;
}`)})
	result := Check(inputs, diagnostics, Config{})
	if result.Successful() {
		t.Fatal("implicit pointer-to-pointer assignment should be rejected")
	}
}

func TestBuildValueSameTypePointerIdentity(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn main() i32 {
    var y i32 = 42;
    let p *i32 = &y;
    let q *i32 = p;
    return 0;
}`)})
	result := Check(inputs, diagnostics, Config{})
	if !result.Successful() {
		t.Fatalf("same-type pointer assignment should be accepted: %+v", diagnostics.Items())
	}
}

func TestBuildRetainedPlaceChain(t *testing.T) {
	state, _ := testBuildValue(t, `type Box = struct { value i32; }; fn main(box Box) void { box.value = 1; }`)
	count := 0
	for ref := range state.places {
		if _, ok := state.buildPlace(ref); !ok {
			t.Fatalf("buildPlace failed for %v", ref)
		}
		count++
	}
	if count == 0 {
		t.Fatal("no retained place")
	}
}

func TestBuildRetainedPlaceUsesSpecificMemberResult(t *testing.T) {
	state, _ := testBuildValue(t, `type Point = struct { x i32; y i32; }; fn main(point Point) void { point.x = point.y; }`)
	var xSymbol, ySymbol symbol.SymbolID
	for _, sym := range state.handoff.Semantics.Resolution().Symbols.All() {
		if sym.Kind != symbol.SymbolField {
			continue
		}
		switch sym.Name {
		case "x":
			xSymbol = sym.ID
		case "y":
			ySymbol = sym.ID
		}
	}
	if xSymbol == 0 || ySymbol == 0 {
		t.Fatalf("missing field symbols: x=%d y=%d", xSymbol, ySymbol)
	}

	var xValue, yValue valueID
	for _, retained := range state.handoff.Records.Records() {
		if retained.Member == nil || retained.Member.Base == 0 {
			continue
		}
		if retained.Member.Name == "x" {
			xValue = retained.Member.Result
		}
		if retained.Member.Name == "y" {
			yValue = retained.Member.Result
		}
	}
	if xValue == 0 || yValue == 0 {
		t.Fatalf("missing member values: x=%d y=%d", xValue, yValue)
	}
	if _, ok := state.buildValue(yValue); !ok {
		t.Fatal("buildValue failed for right-hand member")
	}
	var placeRef symbol.SyntaxRef
	for ref := range state.places {
		placeRef = ref
		break
	}
	place, ok := state.buildPlace(placeRef)
	if !ok {
		t.Fatal("buildPlace failed for assignment destination")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatal(err)
	}
	placeNode := unit.Nodes()[place-1]
	if placeNode.Kind != tir.FieldPlace || placeNode.Member != xSymbol || placeNode.Type != state.handoff.Semantics.Types().Builtins().I32 {
		t.Fatalf("assignment place = %+v, want x field %d", placeNode, xSymbol)
	}
	rightNode := unit.Nodes()[state.values[yValue]-1]
	if rightNode.Kind != tir.Load || len(rightNode.Children) != 1 {
		t.Fatalf("right-hand member value = %+v", rightNode)
	}
	rightPlace := unit.Nodes()[rightNode.Children[0]-1]
	if rightPlace.Kind != tir.FieldPlace || rightPlace.Member != ySymbol {
		t.Fatalf("right-hand place = %+v, want y field %d", rightPlace, ySymbol)
	}
}

// findSymbolID returns the first resolved symbol with the given name and kind.
func findSymbolID(t *testing.T, handoff *solveHandoff, name string, kinds ...symbol.SymbolKind) symbol.SymbolID {
	t.Helper()
	for _, sym := range handoff.Semantics.Resolution().Symbols.All() {
		if sym.Name != name {
			continue
		}
		for _, kind := range kinds {
			if sym.Kind == kind {
				return sym.ID
			}
		}
	}
	t.Fatalf("symbol %s not found", name)
	return 0
}

// requireCallValueID locates the expression result of a call matching match.
func requireCallValueID(t *testing.T, state *irBuildState, records *solvedRecords, match func(*callRecord) bool) valueID {
	t.Helper()
	handoff := state.handoff
	for _, retained := range handoff.Records.Records() {
		if retained.Call == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		if !match(retained.Call) {
			continue
		}
		for _, candidate := range handoff.Records.Records() {
			if candidate.Expression != nil && candidate.Expression.Kind == expressionCall && candidate.Header.Syntax == retained.Header.Syntax {
				if _, ok := records.Root(candidate.Expression.Result); ok {
					return candidate.Expression.Result
				}
			}
		}
	}
	t.Fatal("matching call expression record not found")
	return 0
}

func TestBuildValueDirectCall(t *testing.T) {
	state, records := testBuildValue(t, `
fn add(left i32, right i32) i32 => left + right;
let result i32 = add(1, 2);
`)
	addID := findSymbolID(t, state.handoff, "add", symbol.SymbolFunction)
	id := requireCallValueID(t, state, records, func(c *callRecord) bool {
		return c.Target.Kind == callDirect && c.Target.Symbol == addID
	})
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for direct call")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.DirectCall {
		t.Fatalf("call node = %+v", node)
	}
	if node.Symbol != addID {
		t.Fatalf("call Symbol = %d, want %d", node.Symbol, addID)
	}
	if node.Convention != types.Pebble {
		t.Fatalf("call Convention = %v, want Pebble", node.Convention)
	}
	if node.ContextAction != tir.ContextForward {
		t.Fatalf("call ContextAction = %v, want Forward", node.ContextAction)
	}
	if node.FunctionType == 0 {
		t.Fatal("call FunctionType is zero")
	}
	if len(node.Children) != 2 {
		t.Fatalf("call children = %d, want 2", len(node.Children))
	}
	first := unit.Nodes()[node.Children[0]-1]
	second := unit.Nodes()[node.Children[1]-1]
	if first.Kind != tir.IntegerLiteral || first.Literal.IntegerNum != "1" {
		t.Fatalf("first argument = %+v", first)
	}
	if second.Kind != tir.IntegerLiteral || second.Literal.IntegerNum != "2" {
		t.Fatalf("second argument = %+v", second)
	}
}

func TestBuildValueIndirectCall(t *testing.T) {
	state, records := testBuildValue(t, `
fn add(left i32, right i32) i32 => left + right;
let function fn(i32, i32) i32 = add;
let result i32 = function(3, 4);
`)
	functionID := findSymbolID(t, state.handoff, "function", symbol.SymbolBinding)
	id := requireCallValueID(t, state, records, func(c *callRecord) bool {
		return c.Target.Kind == callIndirect
	})
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for indirect call")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.IndirectCall {
		t.Fatalf("call node = %+v", node)
	}
	if node.Symbol != 0 {
		t.Fatalf("indirect call Symbol = %d, want none", node.Symbol)
	}
	if node.Convention != types.Pebble {
		t.Fatalf("call Convention = %v, want Pebble", node.Convention)
	}
	if node.ContextAction != tir.ContextForward {
		t.Fatalf("call ContextAction = %v, want Forward", node.ContextAction)
	}
	if node.FunctionType == 0 {
		t.Fatal("call FunctionType is zero")
	}
	if len(node.Children) != 3 {
		t.Fatalf("call children = %d, want callee plus two arguments", len(node.Children))
	}
	callee := unit.Nodes()[node.Children[0]-1]
	if callee.Kind != tir.SymbolValue || callee.Symbol != functionID {
		t.Fatalf("callee child = %+v, want symbol value %d", callee, functionID)
	}
	first := unit.Nodes()[node.Children[1]-1]
	second := unit.Nodes()[node.Children[2]-1]
	if first.Kind != tir.IntegerLiteral || first.Literal.IntegerNum != "3" {
		t.Fatalf("first argument = %+v", first)
	}
	if second.Kind != tir.IntegerLiteral || second.Literal.IntegerNum != "4" {
		t.Fatalf("second argument = %+v", second)
	}
}

func TestBuildValueMethodCall(t *testing.T) {
	state, records := testBuildValue(t, `
type Box = struct { value i32; fn get(self Box) i32 => self.value; };
let box Box = Box.{ value = 1 };
let result i32 = box.get();
`)
	boxID := findSymbolID(t, state.handoff, "box", symbol.SymbolBinding)
	id := requireCallValueID(t, state, records, func(c *callRecord) bool {
		return c.Target.Kind == callMethod
	})
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for method call")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.MethodCall {
		t.Fatalf("call node = %+v", node)
	}
	if node.Symbol == 0 {
		t.Fatal("method Symbol is zero")
	}
	var call *callRecord
	for _, retained := range state.handoff.Records.Records() {
		if retained.Call != nil && retained.Call.Target.Kind == callMethod {
			call = retained.Call
			break
		}
	}
	method, ok := state.handoff.Solution.Method(call.Target.Site)
	if !ok || method.Method != node.Symbol {
		t.Fatalf("method Symbol = %d, want solved method %d", node.Symbol, method.Method)
	}
	if node.Convention != types.Pebble {
		t.Fatalf("call Convention = %v, want Pebble", node.Convention)
	}
	if node.ContextAction != tir.ContextForward {
		t.Fatalf("call ContextAction = %v, want Forward", node.ContextAction)
	}
	if node.FunctionType == 0 {
		t.Fatal("call FunctionType is zero")
	}
	if len(node.Children) != 1 {
		t.Fatalf("call children = %d, want receiver exactly once", len(node.Children))
	}
	receiver := unit.Nodes()[node.Children[0]-1]
	if receiver.Kind != tir.SymbolValue || receiver.Symbol != boxID {
		t.Fatalf("receiver child = %+v, want symbol value %d", receiver, boxID)
	}
}

// TestBuildValueGenericMethodCallTypeArgs verifies that a valid generic method
// call with inferred type arguments publishes the concrete solved TypeArgs on
// the emitted MethodCall node, matching the (Symbol, TypeArgs, Convention)
// specialization key the declaration side of the same instantiation carries.
func TestBuildValueGenericMethodCallTypeArgs(t *testing.T) {
	// Block-bodied generic method fixture so the call builds through the real
	// statement pipeline instead of a global initializer that never lowers.
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Box = struct {
    fn echo[T](self Box, value T) T { return value; }
};
fn main() void {
    let box Box = Box.{};
    let result i32 = box.echo(1);
    print result;
}
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, diagnostics, Config{}, inputs.Types)
	if !ok || unit == nil {
		t.Fatal("buildUnit rejected a generic method call")
	}
	recordIRBuilderUnit(unit)

	var methodCall *tir.Node
	var solvedTypeArgs []types.TypeID
	methodsSeen := 0
	for _, retained := range handoff.Records.Records() {
		if retained.Call == nil || retained.Call.Target.Kind != callMethod {
			continue
		}
		methodsSeen++
		method, found := handoff.Solution.Method(retained.Call.Target.Site)
		if !found || method.Method == 0 {
			t.Fatalf("method selection not solved for site %+v", retained.Call.Target.Site)
		}
		for _, argument := range method.Arguments {
			solvedTypeArgs = append(solvedTypeArgs, argument.Type)
		}
		for _, node := range unit.Nodes() {
			if node.Kind == tir.MethodCall && node.Symbol == method.Method {
				methodCall = &node
			}
		}
	}
	if methodsSeen != 1 {
		t.Fatalf("method calls = %d, want exactly one inferred generic method call", methodsSeen)
	}
	if methodCall == nil {
		t.Fatal("generic method call produced no MethodCall node")
	}
	if len(solvedTypeArgs) != 1 || solvedTypeArgs[0] != inputs.Types.Builtins().Int {
		t.Fatalf("solved type args = %+v, want [int]", solvedTypeArgs)
	}
	if len(methodCall.TypeArgs) != len(solvedTypeArgs) {
		t.Fatalf("MethodCall TypeArgs = %v, want solved %v", methodCall.TypeArgs, solvedTypeArgs)
	}
	for i := range methodCall.TypeArgs {
		if methodCall.TypeArgs[i] != solvedTypeArgs[i] {
			t.Fatalf("MethodCall TypeArgs = %v, want solved %v", methodCall.TypeArgs, solvedTypeArgs)
		}
	}
	// The published triple must equal the specialization key for the solved
	// instantiation, so a consumer can correlate the call site with its
	// specialized declaration exactly as it does for a DirectCall site.
	methodKey := newSpecializationKey(methodCall.Symbol, methodCall.TypeArgs, methodCall.Convention)
	solvedKey := newSpecializationKey(methodCall.Symbol, solvedTypeArgs, methodCall.Convention)
	if methodKey != solvedKey {
		t.Fatalf("MethodCall key = %+v, want solved key %+v", methodKey, solvedKey)
	}
}

func TestBuildValueVariantConstruct(t *testing.T) {
	state, records := testBuildValue(t, `
type Choice = union enum { empty void; value i32; };
let variant Choice = Choice.value(2);
`)
	valueVariantID := findSymbolID(t, state.handoff, "value", symbol.SymbolVariant)
	id := requireCallValueID(t, state, records, func(c *callRecord) bool {
		return c.Target.Kind == callVariant
	})
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for variant construction")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.VariantConstruct {
		t.Fatalf("construction node = %+v", node)
	}
	if node.Member != valueVariantID {
		t.Fatalf("construction Member = %d, want variant %d", node.Member, valueVariantID)
	}
	if len(node.Children) != 1 {
		t.Fatalf("construction children = %d, want 1", len(node.Children))
	}
	payload := unit.Nodes()[node.Children[0]-1]
	if payload.Kind != tir.IntegerLiteral || payload.Literal.IntegerNum != "2" {
		t.Fatalf("construction payload = %+v", payload)
	}
}

func TestBuildValueExternCContextNone(t *testing.T) {
	state, records := testBuildValue(t, `
extern fn foreign(value i32) i32;
let result i32 = foreign(7);
`)
	id := requireCallValueID(t, state, records, func(c *callRecord) bool {
		return c.Target.Kind == callDirect
	})
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for extern call")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.DirectCall {
		t.Fatalf("call node = %+v", node)
	}
	if node.Convention != types.C {
		t.Fatalf("call Convention = %v, want C", node.Convention)
	}
	if node.ContextAction != tir.ContextNone {
		t.Fatalf("call ContextAction = %v, want None", node.ContextAction)
	}
}

func TestBuildValueGenericDirectCallTypeArgs(t *testing.T) {
	state, records := testBuildValue(t, `
type Box[T] = struct { value T; };
fn identity[T](value T) T => value;
let inferred i32 = identity(5);
let explicit i32 = identity[i32](6);
`)
	_ = records
	identityID := findSymbolID(t, state.handoff, "identity", symbol.SymbolFunction)
	var ids []valueID
	for _, retained := range state.handoff.Records.Records() {
		if retained.Call == nil || retained.Call.Target.Kind != callDirect || retained.Call.Target.Symbol != identityID {
			continue
		}
		for _, candidate := range state.handoff.Records.Records() {
			if candidate.Expression != nil && candidate.Expression.Kind == expressionCall && candidate.Header.Syntax == retained.Header.Syntax {
				ids = append(ids, candidate.Expression.Result)
			}
		}
	}
	if len(ids) != 2 {
		t.Fatalf("generic calls = %d, want 2", len(ids))
	}
	built := make([]tir.NodeID, len(ids))
	for i, id := range ids {
		nid, ok := state.buildValue(id)
		if !ok {
			t.Fatal("buildValue failed for generic call")
		}
		built[i] = nid
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	for _, nid := range built {
		node := unit.Nodes()[nid-1]
		if node.Kind != tir.DirectCall {
			t.Fatalf("call node = %+v", node)
		}
		if node.Symbol != identityID {
			t.Fatalf("call Symbol = %d, want %d", node.Symbol, identityID)
		}
		if len(node.TypeArgs) != 1 || node.TypeArgs[0] != state.handoff.Semantics.Types().Builtins().I32 {
			t.Fatalf("call TypeArgs = %v, want [i32]", node.TypeArgs)
		}
	}
}

func TestBuildValueNestedCallsAndPlaceReceiver(t *testing.T) {
	state, records := testBuildValue(t, `
fn add(left i32, right i32) i32 => left + right;
type Inner = struct { x i32; fn value(self Inner) i32 => self.x; };
let nested i32 = add(add(1, 2), 3);
fn read(p *Inner) i32 => (*p).value();
`)
	addID := findSymbolID(t, state.handoff, "add", symbol.SymbolFunction)
	var outerID, innerID valueID
	for _, retained := range state.handoff.Records.Records() {
		if retained.Call == nil || retained.Call.Target.Kind != callDirect || retained.Call.Target.Symbol != addID {
			continue
		}
		call := retained.Call
		if len(call.Arguments) != 2 {
			continue
		}
		argumentRecord := state.expressionsByResult[call.Arguments[0].Source]
		if argumentRecord != nil && argumentRecord.Kind == expressionCall {
			innerID = call.Arguments[0].Source
		}
		for _, candidate := range state.handoff.Records.Records() {
			if candidate.Expression != nil && candidate.Expression.Kind == expressionCall && candidate.Header.Syntax == retained.Header.Syntax {
				outerID = candidate.Expression.Result
			}
		}
	}
	if outerID == 0 || innerID == 0 {
		t.Fatal("nested direct calls not found")
	}
	methodID := requireCallValueID(t, state, records, func(c *callRecord) bool {
		return c.Target.Kind == callMethod
	})
	for _, id := range []valueID{outerID, innerID, methodID} {
		if _, ok := state.buildValue(id); !ok {
			t.Fatal("buildValue failed for call")
		}
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	outerNode := unit.Nodes()[state.values[outerID]-1]
	if outerNode.Kind != tir.DirectCall || len(outerNode.Children) != 2 {
		t.Fatalf("outer call = %+v", outerNode)
	}
	innerNode := unit.Nodes()[outerNode.Children[0]-1]
	if innerNode.Kind != tir.DirectCall || len(innerNode.Children) != 2 {
		t.Fatalf("inner call = %+v", innerNode)
	}
	innerFirst := unit.Nodes()[innerNode.Children[0]-1]
	innerSecond := unit.Nodes()[innerNode.Children[1]-1]
	if innerFirst.Kind != tir.IntegerLiteral || innerFirst.Literal.IntegerNum != "1" {
		t.Fatalf("inner first argument = %+v", innerFirst)
	}
	if innerSecond.Kind != tir.IntegerLiteral || innerSecond.Literal.IntegerNum != "2" {
		t.Fatalf("inner second argument = %+v", innerSecond)
	}
	outerSecond := unit.Nodes()[outerNode.Children[1]-1]
	if outerSecond.Kind != tir.IntegerLiteral || outerSecond.Literal.IntegerNum != "3" {
		t.Fatalf("outer second argument = %+v", outerSecond)
	}
	methodNode := unit.Nodes()[state.values[methodID]-1]
	if methodNode.Kind != tir.MethodCall || len(methodNode.Children) != 1 {
		t.Fatalf("method call = %+v", methodNode)
	}
	receiver := unit.Nodes()[methodNode.Children[0]-1]
	if receiver.Kind != tir.SourceAlias || len(receiver.Children) != 1 {
		t.Fatalf("method receiver = %+v, want SourceAlias of the grouped dereference", receiver)
	}
	derefLoad := unit.Nodes()[receiver.Children[0]-1]
	if derefLoad.Kind != tir.Load || len(derefLoad.Children) != 1 {
		t.Fatalf("method receiver load = %+v", derefLoad)
	}
	receiverPlace := unit.Nodes()[derefLoad.Children[0]-1]
	if receiverPlace.Kind != tir.DereferencePlace {
		t.Fatalf("method receiver place = %+v", receiverPlace)
	}
}

func TestBuildValueInactiveGuardedCall(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn add(left i32, right i32) i32 => left; let result i32 = add(1, 2);\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	var callRef symbol.SyntaxRef
	var callResult valueID
	for i := range handoff.Records.values {
		if handoff.Records.values[i].Call != nil {
			callRef = handoff.Records.values[i].Header.Syntax
		}
	}
	if callRef == (symbol.SyntaxRef{}) {
		t.Fatal("no call record retained")
	}
	for i := range handoff.Records.values {
		if handoff.Records.values[i].Header.Syntax == callRef {
			handoff.Records.values[i].Header.Alternative = alternativeTag{Guarded: true, Choice: 999999, Index: 1}
		}
	}
	state := testIRBuildState(t, handoff, records, requirements)
	for _, retained := range handoff.Records.Records() {
		if retained.Expression != nil && retained.Expression.Kind == expressionCall && retained.Header.Syntax == callRef {
			callResult = retained.Expression.Result
		}
	}
	if callResult == 0 {
		t.Fatal("call expression record missing")
	}
	if _, ok := state.buildValue(callResult); ok {
		t.Fatal("buildValue built an inactive guarded call")
	}
}

func TestBuildValueOptionalInject(t *testing.T) {
	state, records := testBuildValue(t, `
fn inject(value ?i32) i32 => value;
let result i32 = inject(5);
`)
	_ = records
	injectID := findSymbolID(t, state.handoff, "inject", symbol.SymbolFunction)
	id := requireCallValueID(t, state, records, func(c *callRecord) bool {
		return c.Target.Kind == callDirect && c.Target.Symbol == injectID
	})
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for optional inject call")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.DirectCall || len(node.Children) != 1 {
		t.Fatalf("call node = %+v", node)
	}
	arg := unit.Nodes()[node.Children[0]-1]
	if arg.Kind != tir.OptionalInject || len(arg.Children) != 1 {
		t.Fatalf("optional inject node = %+v", arg)
	}
	child := unit.Nodes()[arg.Children[0]-1]
	if child.Kind != tir.IntegerLiteral || child.Literal.IntegerNum != "5" {
		t.Fatalf("optional inject child = %+v", child)
	}
}

func TestBuildValueTupleCoerce(t *testing.T) {
	state, records := testBuildValue(t, `
let a i32 = 1;
let b i32 = 2;
let tuple (i64, f64) = (a, b);
`)
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionTuple })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for tuple coerce")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.TupleCoerce || len(node.Children) != 3 || len(node.TypeArgs) != 2 {
		t.Fatalf("tuple coerce node = %+v", node)
	}
	tupleRecord := state.expressionsByResult[id]
	components := state.tuplesBySyntax[tupleRecord.Header.Syntax]
	if len(components) != 2 {
		t.Fatalf("tuple component compatibility records = %d, want 2", len(components))
	}
	sort.Slice(components, func(i, j int) bool { return components[i].Ordinal < components[j].Ordinal })
	var destElements [2]types.TypeID
	for i, component := range components {
		dest, ok := typeOfValue(records, component.Destination)
		if !ok {
			t.Fatalf("component %d destination has no type", i)
		}
		destElements[i] = dest
	}
	if node.TypeArgs[0] != destElements[0] || node.TypeArgs[1] != destElements[1] {
		t.Fatalf("tuple coerce TypeArgs = %v, want %v", node.TypeArgs, destElements)
	}
	sourceTuple := unit.Nodes()[node.Children[0]-1]
	if sourceTuple.Kind != tir.TupleValue || len(sourceTuple.Children) != 2 {
		t.Fatalf("source tuple = %+v", sourceTuple)
	}
	firstCoerced := unit.Nodes()[node.Children[1]-1]
	secondCoerced := unit.Nodes()[node.Children[2]-1]
	if firstCoerced.Kind != tir.IntegerCast {
		t.Fatalf("first coerced child = %+v", firstCoerced)
	}
	if secondCoerced.Kind != tir.IntegerToFloat {
		t.Fatalf("second coerced child = %+v", secondCoerced)
	}
}

func TestBuildValueCheckedOptionalUnwrap(t *testing.T) {
	state, records := testBuildValue(t, `
let x ?i32 = some 5;
let y i32 = x!;
`)
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionPostfix })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for optional unwrap")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.CheckedOptionalUnwrap || len(node.Children) != 1 {
		t.Fatalf("optional unwrap node = %+v", node)
	}
	child := unit.Nodes()[node.Children[0]-1]
	if child.Kind != tir.SymbolValue {
		t.Fatalf("optional unwrap child = %+v", child)
	}
}

func TestBuildValueSomeOptional(t *testing.T) {
	state, records := testBuildValue(t, "fn main() void { let x ?i32 = some 5; }\n")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionSome })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for some")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.SomeOptional || len(node.Children) != 1 {
		t.Fatalf("some node = %+v", node)
	}
	payload := unit.Nodes()[node.Children[0]-1]
	if payload.Kind != tir.IntegerLiteral || payload.Literal.IntegerNum != "5" {
		t.Fatalf("some payload = %+v", payload)
	}
}

// TestBuildValueNoneAndNilAgainstKnownDestination is a regression test for a
// bug in shapeLeaf (expression_facts.go), the helper shared by the `nil` and
// `none` literal cases: it used to unconditionally call session.Variable —
// which registers a real solver cell that must later be resolved — and then,
// only when a known destination shape existed, discard that cell in favor of
// session.Known instead. The abandoned Variable cell was never bound to
// anything, so it was reported as a spurious T0510 "inference variable has no
// unique semantic type" for `none` against a known optional destination and
// `nil` against a known pointer destination alike — this is the exact same
// bug class as prepareArray's (see aggregate_facts.go, commit 4a479e8), just
// surfacing through a different, shared call site. Every one of these must
// build successfully.
func TestBuildValueNoneAndNilAgainstKnownDestination(t *testing.T) {
	cases := []struct {
		source string
		kind   literalKind
	}{
		{"fn main() i32 { let x ?i32 = none; return 1; }", literalNone},
		{"fn main() ?i32 { return none; }", literalNone},
		{"fn main() i32 { let p *i32 = nil; return 1; }", literalNil},
	}
	for _, c := range cases {
		t.Run(c.source, func(t *testing.T) {
			state, records := testBuildValue(t, c.source)
			id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool {
				return e.Kind == expressionLiteral && e.Literal.Kind == c.kind
			})
			if _, ok := state.buildValue(id); !ok {
				t.Fatal("buildValue failed")
			}
			if _, err := buildTestIRUnit(state); err != nil {
				t.Fatalf("Build failed: %v", err)
			}
		})
	}
}

func TestBuildValueCheckedSlice(t *testing.T) {
	// A 2-child CheckedSlice is otherwise structurally ambiguous: a
	// start-only slice (arr[1:]) and an end-only slice (arr[:3]) both
	// produce Children=[base, bound] with nothing to say which bound the
	// lone trailing child is — confirmed as a real, live bug (both fixtures
	// dumped byte-identical node shapes except for the bound's own literal
	// value) while implementing 10.37's slice-typed-local backend work.
	// SliceStartPresent/SliceEndPresent close that gap: they carry the same
	// StartPresent/EndPresent signal the checker's own indexRecord already
	// computes right where this node is built, so this test asserts not
	// just the child count (which was always distinguishable) but which
	// specific bound is present, at each of the four bound-presence shapes.
	tests := []struct {
		name      string
		source    string
		wantKids  int
		wantStart bool
		wantEnd   bool
	}{
		{"both bounds", "fn slice(arr []i32) []i32 => arr[1:3];", 3, true, true},
		{"start omitted", "fn slice(arr []i32) []i32 => arr[:3];", 2, false, true},
		{"end omitted", "fn slice(arr []i32) []i32 => arr[1:];", 2, true, false},
		{"both omitted", "fn slice(arr []i32) []i32 => arr[:];", 1, false, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			state, records := testBuildValue(t, tt.source)
			id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionSlice })
			nid, ok := state.buildValue(id)
			if !ok {
				t.Fatal("buildValue failed for slice")
			}
			unit, err := buildTestIRUnit(state)
			if err != nil {
				t.Fatalf("Build failed: %v", err)
			}
			node := unit.Nodes()[nid-1]
			if node.Kind != tir.CheckedSlice || len(node.Children) != tt.wantKids {
				t.Fatalf("slice node = %+v, want %d children", node, tt.wantKids)
			}
			if node.SliceStartPresent != tt.wantStart || node.SliceEndPresent != tt.wantEnd {
				t.Fatalf("slice node SliceStartPresent=%v SliceEndPresent=%v, want start=%v end=%v", node.SliceStartPresent, node.SliceEndPresent, tt.wantStart, tt.wantEnd)
			}
			base := unit.Nodes()[node.Children[0]-1]
			if base.Kind != tir.SymbolValue {
				t.Fatalf("slice base = %+v", base)
			}
			_ = records
		})
	}
}

func TestBuildValueEnumToInteger(t *testing.T) {
	state, records := testBuildValue(t, `
type Color = enum { red, blue };
let color Color = Color.red;
let value i32 = color as i32;
`)
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionCast })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for enum-to-integer cast")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.EnumToInteger || len(node.Children) != 1 {
		t.Fatalf("enum-to-integer node = %+v", node)
	}
	child := unit.Nodes()[node.Children[0]-1]
	if child.Kind != tir.SymbolValue {
		t.Fatalf("enum-to-integer child = %+v", child)
	}
	_ = records
}

func TestBuildValueOptionalIntegerToEnum(t *testing.T) {
	state, records := testBuildValue(t, `
type Color = enum { red, blue };
let value ?Color = 1 as ?Color;
`)
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionCast })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for optional integer-to-enum cast")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.OptionalIntegerToEnum || len(node.Children) != 1 {
		t.Fatalf("optional integer-to-enum node = %+v", node)
	}
	child := unit.Nodes()[node.Children[0]-1]
	if child.Kind != tir.IntegerLiteral || child.Literal.IntegerNum != "1" {
		t.Fatalf("optional integer-to-enum child = %+v", child)
	}
	_ = records
}

func TestBuildValueCheckedIntegerToEnum(t *testing.T) {
	state, records := testBuildValue(t, `
type Color = enum { red, blue };
let value Color = 1 as Color;
`)
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionCast })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed for checked integer-to-enum cast")
	}
	unit, err := buildTestIRUnit(state)
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.CheckedIntegerToEnum || len(node.Children) != 1 {
		t.Fatalf("checked integer-to-enum node = %+v", node)
	}
	child := unit.Nodes()[node.Children[0]-1]
	if child.Kind != tir.IntegerLiteral || child.Literal.IntegerNum != "1" {
		t.Fatalf("checked integer-to-enum child = %+v", child)
	}
	_ = records
}

// deferRegisterChild returns the sole child of a DeferRegister node.
func deferRegisterChild(t *testing.T, unit *tir.Unit, id tir.NodeID) tir.Node {
	t.Helper()
	node := unit.Nodes()[id-1]
	if node.Kind != tir.DeferRegister || len(node.Children) != 1 {
		t.Fatalf("node %d = %+v, want DeferRegister with one child", id, node)
	}
	return unit.Nodes()[node.Children[0]-1]
}

// printedInteger returns the literal of a one-operand Print whose operand is an
// integer literal, or fails.
func printedInteger(t *testing.T, unit *tir.Unit, node tir.Node) string {
	t.Helper()
	if node.Kind != tir.Print || len(node.Children) != 1 {
		t.Fatalf("deferred statement = %+v, want Print of one operand", node)
	}
	operand := unit.Nodes()[node.Children[0]-1]
	if operand.Kind != tir.IntegerLiteral {
		t.Fatalf("print operand = %+v, want IntegerLiteral", operand)
	}
	return operand.Literal.IntegerNum
}

func TestBuildUnitG3DeferRegisterAndReturnChain(t *testing.T) {
	unit, ok := buildUnitFixture(t, `fn f() void { defer print 1; return; }`)
	if !ok || unit == nil {
		t.Fatal("defer fixture was not buildable")
	}
	registers := nodesOfKind(unit, tir.DeferRegister)
	if len(registers) != 1 {
		t.Fatalf("DeferRegister nodes = %d, want 1", len(registers))
	}
	register := unit.Nodes()[registers[0]-1]
	if register.Kind != tir.DeferRegister || register.Region == 0 {
		t.Fatalf("DeferRegister = %+v, want a region", register)
	}
	if statement := deferRegisterChild(t, unit, registers[0]); printedInteger(t, unit, statement) != "1" {
		t.Fatalf("deferred statement = %+v, want print 1", statement)
	}
	block := functionBody(t, unit)
	if len(block.Children) != 2 {
		t.Fatalf("block children = %v, want DeferRegister then Return", block.Children)
	}
	if block.Children[0] != registers[0] {
		t.Fatalf("first child = %d, want DeferRegister %d", block.Children[0], registers[0])
	}
	returnNode := unit.Nodes()[block.Children[1]-1]
	if returnNode.Kind != tir.Return {
		t.Fatalf("second child = %+v, want Return", returnNode)
	}
	if len(returnNode.DeferChain) != 1 || returnNode.DeferChain[0] != registers[0] {
		t.Fatalf("Return DeferChain = %v, want [%d]", returnNode.DeferChain, registers[0])
	}
}

func TestBuildUnitG3NestedDefersInnermostFirst(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f() void {
    defer print 1;
    defer print 2;
    {
        defer print 3;
        return;
    }
}
`)
	if !ok || unit == nil {
		t.Fatal("nested-defer fixture was not buildable")
	}
	returnNodes := nodesOfKind(unit, tir.Return)
	if len(returnNodes) != 1 {
		t.Fatalf("Return nodes = %d, want 1", len(returnNodes))
	}
	returnNode := unit.Nodes()[returnNodes[0]-1]
	if len(returnNode.DeferChain) != 3 {
		t.Fatalf("Return DeferChain = %v, want 3 entries", returnNode.DeferChain)
	}
	want := []string{"3", "2", "1"}
	for i, id := range returnNode.DeferChain {
		if got := printedInteger(t, unit, deferRegisterChild(t, unit, id)); got != want[i] {
			t.Fatalf("defer chain entry %d runs print %s, want %s", i, got, want[i])
		}
	}
}

func TestBuildUnitG3BreakAndContinueDeferChains(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f(flag bool) void {
    while flag {
        defer print 1;
        if flag { break; }
        continue;
    }
    defer print 2;
}
`)
	if !ok || unit == nil {
		t.Fatal("break/continue defer fixture was not buildable")
	}
	if registers := nodesOfKind(unit, tir.DeferRegister); len(registers) != 2 {
		t.Fatalf("DeferRegister nodes = %d, want 2", len(registers))
	}
	breakNodes := nodesOfKind(unit, tir.Break)
	continueNodes := nodesOfKind(unit, tir.Continue)
	if len(breakNodes) != 1 || len(continueNodes) != 1 {
		t.Fatalf("Break=%d Continue=%d, want one each", len(breakNodes), len(continueNodes))
	}
	exits := []struct {
		name string
		node tir.Node
	}{{"break", unit.Nodes()[breakNodes[0]-1]}, {"continue", unit.Nodes()[continueNodes[0]-1]}}
	for _, exit := range exits {
		if len(exit.node.DeferChain) != 1 {
			t.Fatalf("%s DeferChain = %v, want exactly the crossed while-body defer", exit.name, exit.node.DeferChain)
		}
		if got := printedInteger(t, unit, deferRegisterChild(t, unit, exit.node.DeferChain[0])); got != "1" {
			t.Fatalf("%s defer chain entry prints %s, want print 1", exit.name, got)
		}
	}
}

func TestBuildUnitG3DeferAfterJumpStillIncluded(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f(flag bool) void {
    while flag {
        break;
        defer print 1;
    }
}
`)
	if !ok || unit == nil {
		t.Fatal("post-break defer fixture was not buildable")
	}
	breakNodes := nodesOfKind(unit, tir.Break)
	if len(breakNodes) != 1 {
		t.Fatalf("Break nodes = %d, want 1", len(breakNodes))
	}
	breakNode := unit.Nodes()[breakNodes[0]-1]
	// Defers register on region entry in authored order, so a break anywhere in
	// the region runs every defer the region registered, even one authored after
	// the break. This mirrors defer_validation.go, which collects the whole
	// crossed region's defers regardless of the exit's position.
	if len(breakNode.DeferChain) != 1 {
		t.Fatalf("Break DeferChain = %v, want the whole while-body defer set", breakNode.DeferChain)
	}
	if got := printedInteger(t, unit, deferRegisterChild(t, unit, breakNode.DeferChain[0])); got != "1" {
		t.Fatalf("break defer chain entry prints %s, want print 1", got)
	}
}

func TestBuildUnitG3PostfixUpdateSingleEvaluation(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f() i32 { return 42; }
fn main() void {
    var arr [3]i32 = [0; 3];
    arr[f()]++;
    var i i32 = 0;
    i--;
}
`)
	if !ok || unit == nil {
		t.Fatal("postfix fixture was not buildable")
	}
	compoundNodes := nodesOfKind(unit, tir.CompoundStore)
	if len(compoundNodes) != 2 {
		t.Fatalf("CompoundStore nodes = %d, want 2", len(compoundNodes))
	}
	var indexed, plain tir.Node
	for _, id := range compoundNodes {
		node := unit.Nodes()[id-1]
		if len(node.Children) != 2 {
			t.Fatalf("CompoundStore = %+v, want place/value children", node)
		}
		place := unit.Nodes()[node.Children[0]-1]
		switch place.Kind {
		case tir.CheckedIndexPlace:
			indexed = node
		case tir.StoragePlace:
			plain = node
		default:
			t.Fatalf("CompoundStore place = %+v, want CheckedIndexPlace or StoragePlace", place)
		}
	}
	if indexed.Kind != tir.CompoundStore || indexed.Operator != syntax.Plus {
		t.Fatalf("indexed update = %+v, want CompoundStore with +", indexed)
	}
	if plain.Kind != tir.CompoundStore || plain.Operator != syntax.Minus {
		t.Fatalf("plain update = %+v, want CompoundStore with -", plain)
	}
	indexedPlace := unit.Nodes()[indexed.Children[0]-1]
	if indexedPlace.Kind != tir.CheckedIndexPlace || len(indexedPlace.Children) != 2 {
		t.Fatalf("indexed update place = %+v, want two-child CheckedIndexPlace", indexedPlace)
	}
	index := unit.Nodes()[indexedPlace.Children[1]-1]
	if index.Kind != tir.DirectCall {
		t.Fatalf("indexed update index operand = %+v, want DirectCall", index)
	}
	if one := unit.Nodes()[indexed.Children[1]-1]; one.Kind != tir.IntegerLiteral || one.Literal.IntegerNum != "1" {
		t.Fatalf("indexed update operand = %+v, want literal one", one)
	}
	if one := unit.Nodes()[plain.Children[1]-1]; one.Kind != tir.IntegerLiteral || one.Literal.IntegerNum != "1" {
		t.Fatalf("plain update operand = %+v, want literal one", one)
	}
	// f() must be evaluated exactly once: the only DirectCall anywhere is the
	// index operand of the single CompoundStore place, never duplicated.
	directCalls := nodesOfKind(unit, tir.DirectCall)
	if len(directCalls) != 1 || directCalls[0] != indexedPlace.Children[1] {
		t.Fatalf("DirectCall nodes = %v, want exactly the one index operand", directCalls)
	}
}

// variantSymbols returns the symbols of every VariantDeclaration in the unit.
func variantSymbols(t *testing.T, unit *tir.Unit) map[symbol.SymbolID]bool {
	t.Helper()
	variants := map[symbol.SymbolID]bool{}
	for _, node := range unit.Nodes() {
		if node.Kind == tir.VariantDeclaration {
			variants[node.Symbol] = true
		}
	}
	return variants
}

// functionBody returns the single function body block node.
func functionBody(t *testing.T, unit *tir.Unit) tir.Node {
	t.Helper()
	for _, decl := range unit.FunctionDeclarations() {
		if decl.Node == 0 {
			continue
		}
		block := unit.Nodes()[decl.Node-1]
		if block.Kind != tir.Block {
			t.Fatalf("function body = %+v, want Block", block)
		}
		return block
	}
	t.Fatal("function declaration not found")
	return tir.Node{}
}

func TestBuildUnitG2SwitchExhaustiveEnum(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
type Color = enum { red, blue, green };
fn choose(value Color) void {
    switch value {
    case Color.red: { return; }
    case Color.blue: { return; }
    case Color.green: { return; }
    }
}
`)
	if !ok || unit == nil {
		t.Fatal("exhaustive enum switch was not buildable")
	}
	switchNodes := nodesOfKind(unit, tir.Switch)
	if len(switchNodes) != 1 {
		t.Fatalf("Switch nodes = %d, want 1", len(switchNodes))
	}
	switchNode := unit.Nodes()[switchNodes[0]-1]
	if switchNode.Kind != tir.Switch || switchNode.HasElse || switchNode.Region == 0 {
		t.Fatalf("Switch node = %+v, want region and no else", switchNode)
	}
	if len(switchNode.Children) != 4 {
		t.Fatalf("Switch children = %d, want subject plus 3 cases", len(switchNode.Children))
	}
	subject := unit.Nodes()[switchNode.Children[0]-1]
	if subject.Kind != tir.SymbolValue || subject.Symbol == 0 {
		t.Fatalf("Switch subject = %+v, want SymbolValue", subject)
	}
	variants := variantSymbols(t, unit)
	if len(variants) != 3 {
		t.Fatalf("variant declarations = %d, want 3", len(variants))
	}
	covered := map[symbol.SymbolID]bool{}
	for i := 1; i < len(switchNode.Children); i++ {
		caseNode := unit.Nodes()[switchNode.Children[i]-1]
		if caseNode.Kind != tir.SwitchCase {
			t.Fatalf("Switch child %d = %+v, want SwitchCase", i, caseNode)
		}
		if caseNode.CaseValue == 0 || caseNode.HasElse || caseNode.Literal != (tir.Literal{}) {
			t.Fatalf("SwitchCase = %+v, want variant CaseValue", caseNode)
		}
		if !variants[caseNode.CaseValue] {
			t.Fatalf("SwitchCase CaseValue %d is not a variant", caseNode.CaseValue)
		}
		covered[caseNode.CaseValue] = true
		if caseNode.Region == 0 {
			t.Fatal("SwitchCase has no region")
		}
		if len(caseNode.Children) != 1 {
			t.Fatalf("SwitchCase children = %d, want one body block", len(caseNode.Children))
		}
		body := unit.Nodes()[caseNode.Children[0]-1]
		if body.Kind != tir.Block || len(body.Children) != 1 {
			t.Fatalf("case body = %+v, want one-statement Block", body)
		}
		if ret := unit.Nodes()[body.Children[0]-1]; ret.Kind != tir.Return {
			t.Fatalf("case body statement = %+v, want Return", ret)
		}
	}
	if len(covered) != 3 {
		t.Fatalf("covered variants = %d, want all 3", len(covered))
	}
	block := functionBody(t, unit)
	if len(block.Children) == 0 || block.Children[len(block.Children)-1] != switchNodes[0] {
		t.Fatalf("function body children = %v, want the Switch last with no ImplicitReturn", block.Children)
	}
}

func TestBuildUnitG2SwitchBoolExhaustive(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn choose(flag bool) void {
    switch flag {
    case true: { return; }
    case false: { return; }
    }
}
`)
	if !ok || unit == nil {
		t.Fatal("bool switch was not buildable")
	}
	switchNodes := nodesOfKind(unit, tir.Switch)
	if len(switchNodes) != 1 {
		t.Fatalf("Switch nodes = %d, want 1", len(switchNodes))
	}
	switchNode := unit.Nodes()[switchNodes[0]-1]
	if switchNode.Kind != tir.Switch || switchNode.HasElse {
		t.Fatalf("Switch node = %+v, want no else", switchNode)
	}
	if len(switchNode.Children) != 3 {
		t.Fatalf("Switch children = %d, want subject plus two cases", len(switchNode.Children))
	}
	seenTrue, seenFalse := false, false
	for i := 1; i < len(switchNode.Children); i++ {
		caseNode := unit.Nodes()[switchNode.Children[i]-1]
		if caseNode.Kind != tir.SwitchCase || caseNode.CaseValue != 0 || caseNode.HasElse {
			t.Fatalf("SwitchCase = %+v, want scalar Literal case", caseNode)
		}
		if caseNode.Literal.Kind != tir.LiteralBool {
			t.Fatalf("SwitchCase Literal = %+v, want bool literal", caseNode.Literal)
		}
		if caseNode.Literal.Bool {
			seenTrue = true
		} else {
			seenFalse = true
		}
	}
	if !seenTrue || !seenFalse {
		t.Fatalf("bool cases not exhaustive: true=%t false=%t", seenTrue, seenFalse)
	}
	block := functionBody(t, unit)
	if len(block.Children) == 0 || block.Children[len(block.Children)-1] != switchNodes[0] {
		t.Fatalf("function body children = %v, want the Switch last with no ImplicitReturn", block.Children)
	}
}

func TestBuildUnitG2SwitchScalarElse(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn grade(value i32) i32 {
    switch value {
    case 1: { return 10; }
    else: { return 20; }
    }
}
`)
	if !ok || unit == nil {
		t.Fatal("scalar switch with else was not buildable")
	}
	switchNodes := nodesOfKind(unit, tir.Switch)
	if len(switchNodes) != 1 {
		t.Fatalf("Switch nodes = %d, want 1", len(switchNodes))
	}
	switchNode := unit.Nodes()[switchNodes[0]-1]
	if switchNode.Kind != tir.Switch || !switchNode.HasElse {
		t.Fatalf("Switch node = %+v, want HasElse", switchNode)
	}
	if len(switchNode.Children) != 3 {
		t.Fatalf("Switch children = %d, want subject, case, else", len(switchNode.Children))
	}
	caseNode := unit.Nodes()[switchNode.Children[1]-1]
	if caseNode.Kind != tir.SwitchCase || caseNode.CaseValue != 0 || caseNode.HasElse {
		t.Fatalf("scalar SwitchCase = %+v", caseNode)
	}
	if caseNode.Literal.Kind != tir.LiteralInteger || caseNode.Literal.IntegerNum != "1" || caseNode.Literal.IntegerDen != "1" {
		t.Fatalf("scalar case Literal = %+v, want integer 1", caseNode.Literal)
	}
	if len(caseNode.Children) != 1 {
		t.Fatalf("scalar case children = %d, want one body block", len(caseNode.Children))
	}
	if body := unit.Nodes()[caseNode.Children[0]-1]; body.Kind != tir.Block || len(body.Children) != 1 {
		t.Fatalf("scalar case body = %+v", body)
	} else if ret := unit.Nodes()[body.Children[0]-1]; ret.Kind != tir.Return {
		t.Fatalf("scalar case body statement = %+v, want Return", ret)
	}
	elseNode := unit.Nodes()[switchNode.Children[2]-1]
	if elseNode.Kind != tir.SwitchCase || !elseNode.HasElse || elseNode.CaseValue != 0 || elseNode.Literal != (tir.Literal{}) {
		t.Fatalf("else SwitchCase = %+v, want HasElse", elseNode)
	}
	if len(elseNode.Children) != 1 {
		t.Fatalf("else children = %d, want one body block", len(elseNode.Children))
	}
	if body := unit.Nodes()[elseNode.Children[0]-1]; body.Kind != tir.Block || len(body.Children) != 1 {
		t.Fatalf("else body = %+v", body)
	} else if ret := unit.Nodes()[body.Children[0]-1]; ret.Kind != tir.Return {
		t.Fatalf("else body statement = %+v, want Return", ret)
	}
}

func TestBuildUnitG2SwitchBreakTargetsSwitch(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn f(flag bool) void {
    switch flag {
    case true: break;
    case false: print 1;
    }
    print 2;
}
`)
	if !ok || unit == nil {
		t.Fatal("switch with break was not buildable")
	}
	switchNodes := nodesOfKind(unit, tir.Switch)
	breakNodes := nodesOfKind(unit, tir.Break)
	if len(switchNodes) != 1 || len(breakNodes) != 1 {
		t.Fatalf("Switch=%d Break=%d, want one each", len(switchNodes), len(breakNodes))
	}
	switchNode := unit.Nodes()[switchNodes[0]-1]
	breakNode := unit.Nodes()[breakNodes[0]-1]
	if breakNode.Kind != tir.Break || breakNode.Target == 0 {
		t.Fatalf("Break node = %+v, want targeted break", breakNode)
	}
	if breakNode.Target != switchNode.Region {
		t.Fatalf("break target %d, want switch region %d", breakNode.Target, switchNode.Region)
	}
}

func TestBuildUnitG2SwitchMultiValueCase(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
type Color = enum { red, blue, green };
fn choose(value Color) void {
    switch value {
    case Color.red, Color.blue: { return; }
    case Color.green: { return; }
    }
}

`)
	if !ok || unit == nil {
		t.Fatal("multi-value case switch was not buildable")
	}
	switchNodes := nodesOfKind(unit, tir.Switch)
	if len(switchNodes) != 1 {
		t.Fatalf("Switch nodes = %d, want 1", len(switchNodes))
	}
	switchNode := unit.Nodes()[switchNodes[0]-1]
	if len(switchNode.Children) != 4 {
		t.Fatalf("Switch children = %d, want subject plus 3 case nodes", len(switchNode.Children))
	}
	variants := variantSymbols(t, unit)
	if len(variants) != 3 {
		t.Fatalf("variant declarations = %d, want 3", len(variants))
	}
	covered := map[symbol.SymbolID]bool{}
	bodyCounts := map[tir.NodeID]int{}
	for i := 1; i < len(switchNode.Children); i++ {
		node := unit.Nodes()[switchNode.Children[i]-1]
		if node.Kind != tir.SwitchCase {
			t.Fatalf("Switch child %d = %+v, want SwitchCase", i, node)
		}
		if node.CaseValue == 0 || node.HasElse || node.Literal != (tir.Literal{}) {
			t.Fatalf("SwitchCase = %+v, want variant CaseValue", node)
		}
		if !variants[node.CaseValue] {
			t.Fatalf("SwitchCase CaseValue %d is not a variant", node.CaseValue)
		}
		covered[node.CaseValue] = true
		if len(node.Children) != 1 {
			t.Fatalf("SwitchCase children = %d, want one body block", len(node.Children))
		}
		bodyCounts[node.Children[0]]++
	}
	if len(covered) != 3 {
		t.Fatalf("covered variants = %d, want all 3", len(covered))
	}
	shared := 0
	sole := 0
	for _, count := range bodyCounts {
		if count == 2 {
			shared++
		}
		if count == 1 {
			sole++
		}
	}
	if shared != 1 || sole != 1 {
		t.Fatalf("body sharing = %v, want one shared body and one sole body", bodyCounts)
	}
}

func irFixturePaths(t *testing.T) []string {
	t.Helper()
	return validationFixturePaths(t, "../../../tests/check/ir/valid/*.peb")
}

func buildIRFixturePath(t *testing.T, path string, config Config) (*tir.Unit, bool) {
	t.Helper()
	contents, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
	handoff := run06a(inputs, diagnostics, config)
	if handoff == nil || handoff.GenerationHadErrors || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a rejected %s: %+v", path, diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(config))
	if !ok {
		t.Fatalf("records rejected %s: %+v", path, diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(config))
	if !ok {
		t.Fatalf("requirements rejected %s: %+v", path, diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, diagnostics, config, inputs.Types)
	if ok && unit != nil {
		recordIRBuilderUnit(unit)
	}
	return unit, ok
}

func TestIRFixtures(t *testing.T) {
	paths := irFixturePaths(t)
	seen := make(map[tir.NodeKind]string)
	for _, path := range paths {
		path := path
		t.Run(filepath.Base(path), func(t *testing.T) {
			unit, ok := buildIRFixturePath(t, path, Config{})
			if !ok || unit == nil {
				t.Fatal("valid IR fixture was rejected")
			}
			for _, node := range unit.Nodes() {
				seen[node.Kind] = filepath.Base(path)
			}
		})
	}
	if len(seen) == 0 {
		t.Fatal("fixtures built no IR nodes")
	}
}

func TestBuildValueNamedFunctionValue(t *testing.T) {
	// Block-bodied fixture so the value builds through the real statement
	// pipeline instead of an expression body that can lower to an empty block.
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn add(left i32, right i32) i32 { return left + right; }
fn main() void {
    let f fn(i32, i32) i32 = add;
}
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, diagnostics, Config{}, inputs.Types)
	if !ok || unit == nil {
		t.Fatal("buildUnit rejected a named function value")
	}
	recordIRBuilderUnit(unit)
	var addSymbol symbol.SymbolID
	for _, candidate := range inputs.Resolution.Symbols.All() {
		if candidate.Name == "add" && candidate.Kind == symbol.SymbolFunction {
			addSymbol = candidate.ID
			break
		}
	}
	if addSymbol == 0 {
		t.Fatal("add symbol not found")
	}
	declarations := unit.FunctionDeclarations()
	matched := false
	for _, node := range unit.Nodes() {
		if node.Kind != tir.HoistedFunctionValue || node.Symbol != addSymbol {
			continue
		}
		if node.Function == 0 {
			t.Fatalf("named function value = %+v, missing Function identity", node)
		}
		for _, declaration := range declarations {
			if declaration.FunctionID == node.Function && declaration.Symbol == addSymbol && declaration.Node != 0 {
				matched = true
			}
		}
	}
	if !matched {
		t.Fatal("named function value did not produce a matching HoistedFunctionValue and declaration")
	}
}

func TestBuildValueGenericFunctionValue(t *testing.T) {
	// Block-bodied fixture so the value builds through the real statement
	// pipeline instead of an expression body that can lower to an empty block.
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn identity[T](value T) T { return value; }
fn main() void {
    let f fn(i32) i32 = identity[i32];
}
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid setup: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	unit, ok := buildUnit(handoff, records, requirements, diagnostics, Config{}, inputs.Types)
	if !ok || unit == nil {
		t.Fatalf("buildUnit rejected a bare generic function value: %+v", diagnostics.Items())
	}
	recordIRBuilderUnit(unit)
	var identitySymbol symbol.SymbolID
	for _, candidate := range inputs.Resolution.Symbols.All() {
		if candidate.Name == "identity" && candidate.Kind == symbol.SymbolFunction {
			identitySymbol = candidate.ID
			break
		}
	}
	if identitySymbol == 0 {
		t.Fatal("identity symbol not found")
	}
	wantI32 := inputs.Types.Builtins().I32
	var valueNode *tir.Node
	for _, node := range unit.Nodes() {
		if node.Kind == tir.GenericFunctionValue {
			valueNode = &node
			break
		}
	}
	if valueNode == nil {
		t.Fatal("GenericFunctionValue node missing")
	}
	if valueNode.Symbol != identitySymbol {
		t.Fatalf("GenericFunctionValue Symbol = %d, want %d", valueNode.Symbol, identitySymbol)
	}
	if len(valueNode.TypeArgs) != 1 || valueNode.TypeArgs[0] != wantI32 {
		t.Fatalf("GenericFunctionValue TypeArgs = %v, want [i32 %d]", valueNode.TypeArgs, wantI32)
	}
	instantiations := unit.Instantiations()
	if uint64(valueNode.GenericRef) >= uint64(len(instantiations)) {
		t.Fatalf("GenericFunctionValue GenericRef %d out of range", valueNode.GenericRef)
	}
	instantiation := instantiations[valueNode.GenericRef]
	if instantiation.Declaration != identitySymbol {
		t.Fatalf("instantiation Declaration = %d, want %d", instantiation.Declaration, identitySymbol)
	}
	if len(instantiation.TypeArgs) != 1 || instantiation.TypeArgs[0] != wantI32 {
		t.Fatalf("instantiation TypeArgs = %v, want [i32 %d]", instantiation.TypeArgs, wantI32)
	}
	built := false
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration && node.Symbol == identitySymbol && len(node.TypeArgs) == 1 && node.TypeArgs[0] == wantI32 {
			built = true
		}
	}
	if !built {
		t.Fatal("specialized FunctionDeclaration with matching TypeArgs missing")
	}
}

func TestIRBuilderNodeKindCoverage(t *testing.T) {
	// HoistedFunctionValue and GenericFunctionValue are covered by the focused
	// tests declared above, which must therefore run before this one.
	//
	// TempBind, TempRead, and Sequence are permanent architectural exclusions,
	// not an implementation gap. They are the frozen schema's general-purpose
	// mechanism for evaluating an authored expression more than once and
	// capturing each result, but no construction path in this project's accepted
	// 06b.7b work has ever needed to evaluate an authored expression more than
	// once: every double-evaluation risk this slice actually encountered turned
	// out to have a dedicated, single-evaluation closed-form node instead of
	// needing a temp. G2's CompoundStore evaluates its place exactly once as a
	// single child rather than expanding to Load+Arithmetic+Store; G3's postfix
	// ++/-- builds as CompoundStore for the same reason; part E's MethodCall
	// receiver is a single child evaluated once; and F2's TupleCoerce and the
	// checked index-place bases are likewise single children. buildValue
	// memoizes every valueID, so a child node referenced by multiple consumers
	// (e.g. a TupleCoerce's source tuple and its coerced elements) is one
	// runtime evaluation shared across the DAG, never silent double evaluation.
	// These kinds are therefore architecturally unneeded for this specific
	// language's semantics, not an oversight.
	knownUnimplementedNodeKinds := map[tir.NodeKind]bool{
		tir.TempBind: true,
		tir.TempRead: true,
		tir.Sequence: true,
	}
	for kind := tir.FirstNodeKind; kind <= tir.LastNodeKind; kind++ {
		if knownUnimplementedNodeKinds[kind] {
			continue
		}
		if _, ok := irBuilderCoverageSeen[kind]; !ok {
			t.Fatalf("node kind %v was not produced by a real IR unit", kind)
		}
	}
}

func TestIRBuilderHoistedAnonymousFunctionValues(t *testing.T) {
	unit, ok := buildUnitFixture(t, `
fn consume(value fn() i32) void { }
fn main() void {
    let stored = fn() i32 => 1;
    consume(fn() i32 => 2);
}
`)
	if !ok || unit == nil {
		t.Fatal("anonymous functions must build")
	}
	values := nodesOfKind(unit, tir.HoistedFunctionValue)
	if len(values) != 2 {
		t.Fatalf("HoistedFunctionValue count = %d, want 2", len(values))
	}
	declarations := unit.FunctionDeclarations()
	if len(declarations) < 3 {
		t.Fatalf("function declarations = %d, want named and anonymous bodies", len(declarations))
	}
	for _, id := range values {
		node := unit.Nodes()[id-1]
		if node.Symbol == 0 || node.Function == 0 {
			t.Fatalf("hoisted value = %+v", node)
		}
		found := false
		for _, declaration := range declarations {
			if declaration.FunctionID == node.Function {
				if declaration.Symbol != node.Symbol || declaration.Node == 0 || unit.Nodes()[declaration.Node-1].Kind != tir.Block {
					t.Fatalf("function declaration = %+v", declaration)
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("no declaration for hoisted function %d", node.Function)
		}
	}
}

func TestIRFixtureGolden(t *testing.T) {
	unit, ok := buildIRFixturePath(t, "../../../tests/check/ir/valid/operations_and_calls.peb", Config{})
	if !ok || unit == nil {
		t.Fatal("golden fixture was rejected")
	}
	var got bytes.Buffer
	if err := unit.Dump(&got); err != nil {
		t.Fatalf("Dump failed: %v", err)
	}
	want, err := os.ReadFile("../../../tests/check/ir/operations_and_calls.tir.golden")
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(got.Bytes(), want) {
		t.Fatalf("typed-IR dump mismatch: want %d bytes, got %d bytes", len(want), got.Len())
	}
}

func TestIRInterpolationFixtureGolden(t *testing.T) {
	unit, ok := buildIRFixturePath(t, "../../../tests/check/ir/valid/interpolation_parts.peb", Config{})
	if !ok || unit == nil {
		t.Fatal("interpolation golden fixture was rejected")
	}
	var got bytes.Buffer
	if err := unit.Dump(&got); err != nil {
		t.Fatalf("Dump failed: %v", err)
	}
	want, err := os.ReadFile("../../../tests/check/ir/interpolation_parts.tir.golden")
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(got.Bytes(), want) {
		t.Fatalf("typed-IR interpolation dump mismatch: want %d bytes, got %d bytes", len(want), got.Len())
	}
}

func TestIRBuilderCoverageLimits(t *testing.T) {
	source := `fn main(value i32) i32 { let copy i32 = value + 1; return copy; }`
	for _, tc := range []struct {
		name   string
		config Config
	}{
		{name: "nodes", config: Config{MaxIRNodes: 1}},
		{name: "components", config: Config{MaxIRComponents: 1}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			defer func() {
				if recovered := recover(); recovered != nil {
					t.Fatalf("low %s limit panicked: %v", tc.name, recovered)
				}
			}()
			unit, ok := buildUnitFixtureWithConfig(t, source, tc.config)
			if ok || unit != nil {
				t.Fatalf("low %s limit returned unit: ok=%v unit=%v", tc.name, ok, unit)
			}
		})
	}
	limited, ok := buildUnitFixtureWithConfig(t, source, Config{MaxDumpBytes: 1})
	if !ok || limited == nil {
		t.Fatal("MaxDumpBytes must not prevent building a valid unit")
	}
	if err := limited.Dump(io.Discard); err != tir.ErrDumpOverflow {
		t.Fatalf("low dump limit error = %v, want %v", err, tir.ErrDumpOverflow)
	}
}

func TestBuildUnitRejectsMalformedHandoff(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatal("valid setup was rejected")
	}
	if unit, ok := buildUnit(handoff, nil, nil, diagnostics, Config{}, inputs.Types); ok || unit != nil {
		t.Fatal("nil records must be rejected")
	}
}

func TestBuildUnitReportsBuildVerifierFailure(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() i32 { return 1; }")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatal("valid setup was rejected")
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	for i := range handoff.Records.values {
		if handoff.Records.values[i].Control != nil && handoff.Records.values[i].Control.Kind == controlReturn {
			handoff.Records.values[i].Control.Callable.Symbol = 0
			break
		}
	}
	diagnostics = diagnostic.NewDiagnosticSet()
	unit, ok := buildUnit(handoff, records, requirements, diagnostics, Config{}, inputs.Types)
	if ok || unit != nil {
		t.Fatal("malformed return should fail closed-IR verification")
	}
	items := diagnostics.Items()
	if len(items) != 1 || items[0].Code != CodeGeneration {
		t.Fatalf("verifier failure diagnostics = %+v, want exactly one C0619", items)
	}
	if !strings.Contains(items[0].Message, "Build:") {
		t.Fatalf("verifier failure diagnostic = %q, want Build error context", items[0].Message)
	}
}

func TestIRBuilderConcurrentUnitReads(t *testing.T) {
	unit, ok := buildIRFixturePath(t, "../../../tests/check/ir/valid/operations_and_calls.peb", Config{})
	if !ok || unit == nil {
		t.Fatal("concurrent-read fixture was rejected")
	}
	var wg sync.WaitGroup
	for i := 0; i < 8; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 20; j++ {
				_ = unit.Nodes()
				_ = unit.Modules()
				_ = unit.FunctionDeclarations()
				_ = unit.GlobalDeclarations()
				_ = unit.SourceRefs()
				var dump bytes.Buffer
				if err := unit.Dump(&dump); err != nil {
					t.Errorf("concurrent Dump failed: %v", err)
				}
			}
		}()
	}
	wg.Wait()
}

func FuzzBuildUnit(f *testing.F) {
	config := Config{
		MaxSyntaxVisits: 500, MaxTraversalDepth: 64, MaxSemanticRecords: 1000,
		MaxRecordComponents: 1000, MaxControlDepth: 64, MaxTrackedPlaces: 1000,
		MaxGenericRequirements: 1000, MaxConstantDepth: 64, MaxConstantOperations: 2000,
		MaxConstantBits: 2048, MaxDiagnostics: 300, MaxValidationSteps: 2000,
		MaxIRNodes: 2000, MaxIRComponents: 10000, MaxFlowStates: 1000, MaxDeferEdges: 1000,
		MaxDumpBytes: 1 << 20,
	}
	paths, err := filepath.Glob("../../../tests/check/ir/valid/*.peb")
	if err != nil {
		f.Fatal(err)
	}
	for _, path := range paths {
		contents, err := os.ReadFile(path)
		if err == nil {
			f.Add(contents)
		}
	}
	f.Add([]byte("fn broken( int { let value = ; }"))
	f.Fuzz(func(t *testing.T, contents []byte) {
		if len(contents) > 512 {
			return
		}
		// The pipeline this fuzzes runs earlier phases (parsing, symbol
		// resolution, generation) that are out of this file's scope and may
		// have their own unbounded-loop bugs on malformed input, distinct
		// from anything buildUnit itself does. Run each case on its own
		// goroutine with a hard deadline so a hang upstream fails this test
		// cleanly instead of hanging `go test` (and, worse, saving a
		// hanging input to testdata/fuzz/ that would replay and hang every
		// future test run).
		done := make(chan struct{})
		var panicked any
		go func() {
			defer close(done)
			defer func() {
				panicked = recover()
			}()
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
			handoff := run06a(inputs, diagnostics, config)
			if handoff == nil || handoff.GenerationHadErrors || handoff.Semantics == nil || handoff.Solution == nil {
				return
			}
			records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(config))
			if !ok {
				return
			}
			requirements, ok := validateRequirements(handoff, records, diagnostics, normalizeConfig(config))
			if !ok {
				return
			}
			_, _ = buildUnit(handoff, records, requirements, diagnostics, config, inputs.Types)
		}()
		select {
		case <-done:
			if panicked != nil {
				t.Fatalf("build pipeline panicked: %v", panicked)
			}
		case <-time.After(5 * time.Second):
			t.Fatalf("build pipeline hung (likely an upstream parsing/generation bug, not buildUnit itself) on input: %q", contents)
		}
	})
}
