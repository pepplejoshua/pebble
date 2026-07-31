package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

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
	unit, ok := buildUnit(handoff, records, requirements, Config{})
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
	unit, ok := buildUnit(handoff, records, requirements, Config{})
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
	unit, ok := buildUnit(&solveHandoff{GenerationHadErrors: true}, nil, nil, Config{})
	if ok || unit != nil {
		t.Fatal("expected failed generation handoff to be rejected")
	}
}

func testIRBuildState(t *testing.T, handoff *solveHandoff, records *solvedRecords, requirements map[symbol.SymbolID][]Requirement) *irBuildState {
	t.Helper()
	b := tir.NewBuilder(handoff.Semantics.Types(), tir.Config{
		MaxIRNodes: DefaultMaxIRNodes, MaxIRComponents: DefaultMaxIRComponents,
		MaxDumpBytes: DefaultMaxDumpBytes,
	})
	state := &irBuildState{handoff: handoff, records: records, builder: b}
	if !state.buildModules() || !state.buildTypes() || !state.buildDeclarations() || !state.buildTypeUses() || !state.indexExpressions() || !state.buildBlocks() || !state.buildRequirements(requirements) {
		t.Fatal("failed to build test IR state")
	}
	return state
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
	unit, err := state.builder.Build()
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
	unit, err := state.builder.Build()
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
	unit, err := state.builder.Build()
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
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.TupleValue || len(node.Children) != 2 {
		t.Fatalf("tuple node = %+v", node)
	}
}

func TestBuildValueArray(t *testing.T) {
	state, records := testBuildValue(t, "let inferred = [1, 2, 3];")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionArray })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.ArrayValue || len(node.Children) != 3 {
		t.Fatalf("array node = %+v", node)
	}
}

func TestBuildValueArrayRepeat(t *testing.T) {
	state, records := testBuildValue(t, "let repeated [5]i32 = [1; 5];\n")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionArrayRepeat })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := state.builder.Build()
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
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.RecordConstruct || len(node.Fields) != 2 {
		t.Fatalf("record node = %+v", node)
	}
}

func TestBuildValuePrefixNegation(t *testing.T) {
	state, records := testBuildValue(t, "let neg i32 = -1;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionPrefix })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.PrefixValue || node.Operator != syntax.Minus || len(node.Children) != 1 {
		t.Fatalf("prefix node = %+v", node)
	}
}

func TestBuildValueBinaryNumeric(t *testing.T) {
	state, records := testBuildValue(t, "let sum i32 = 1 + 2;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionBinary })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.BinaryValue || node.Operator != syntax.Plus || len(node.Children) != 2 {
		t.Fatalf("binary node = %+v", node)
	}
}

func TestBuildValueShortCircuit(t *testing.T) {
	state, records := testBuildValue(t, "let both bool = true && false;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionBinary })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := state.builder.Build()
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
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.SourceAlias || node.ExplicitCast || len(node.Children) != 1 {
		t.Fatalf("grouped node = %+v", node)
	}
}

func TestBuildValueInterpolatedString(t *testing.T) {
	state, records := testBuildValue(t, "let item str = \"x\";\nlet msg str = `value {item}`;")
	id := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionInterpolated })
	nid, ok := state.buildValue(id)
	if !ok {
		t.Fatal("buildValue failed")
	}
	unit, err := state.builder.Build()
	if err != nil {
		t.Fatalf("Build failed: %v", err)
	}
	node := unit.Nodes()[nid-1]
	if node.Kind != tir.InterpolatedString || len(node.Children) != 1 {
		t.Fatalf("interpolated node = %+v", node)
	}
}

func TestBuildValueNestedComposites(t *testing.T) {
	state, records := testBuildValue(t, "let nested = ((1 + 2), [3, 4]);\n")
	tupleID := requireValueID(t, state.handoff, records, func(e *expressionRecord) bool { return e.Kind == expressionTuple })
	tupleNID, ok := state.buildValue(tupleID)
	if !ok {
		t.Fatal("buildValue failed for nested tuple")
	}
	unit, err := state.builder.Build()
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
	if binaryNode.Kind != tir.BinaryValue {
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
	unit, err := state.builder.Build()
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
	if _, ok := state.buildValue(id); ok {
		t.Fatal("string indexing unexpectedly built as a place/load")
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
	unit, err := state.builder.Build()
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
