package check

import (
	"math/big"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func constantInputs(t *testing.T, files checkProvider) (Inputs, *diagnostic.DiagnosticSet) {
	t.Helper()
	diagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "test"}, files, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("fixture setup diagnostics: %+v", diagnostics.Items())
	}
	return Inputs{Graph: graph, Sources: sources, Resolution: resolution}, diagnostics
}

func constantInitializer(t *testing.T, evaluator *constantEvaluator, name string) symbol.SyntaxRef {
	t.Helper()
	for _, candidate := range evaluator.inputs.Resolution.Symbols.All() {
		if candidate.Name != name || candidate.Kind != symbol.SymbolBinding {
			continue
		}
		initializer, ok, _ := evaluator.bindingInitializer(candidate.ID)
		if ok {
			return initializer
		}
	}
	t.Fatalf("missing constant %q", name)
	return symbol.SyntaxRef{}
}

func constantArrayLengths(inputs Inputs) []symbol.SyntaxRef {
	var refs []symbol.SyntaxRef
	for _, item := range inputs.Graph.Modules() {
		var visit func(syntax.NodeID)
		visit = func(id syntax.NodeID) {
			node, ok := item.Tree.Node(id)
			if !ok {
				return
			}
			children := node.Children()
			if node.Kind() == syntax.ArrayType && len(children) != 0 {
				refs = append(refs, symbol.SyntaxRef{Module: item.ID, Node: children[0]})
			}
			for _, child := range children {
				visit(child)
			}
		}
		visit(item.Tree.Root())
	}
	return refs
}

func requireInteger(t *testing.T, result constantResult, want string) {
	t.Helper()
	if result.State != constantKnown || result.Value.Kind != constantInteger || result.Value.Integer.String() != want {
		t.Fatalf("integer result = %+v, want %s", result, want)
	}
}

func TestConstantAcceptedFormsAndExactArithmetic(t *testing.T) {
	inputs, diagnostics := constantInputs(t, checkProvider{"main.peb": []byte(`
type Color = enum { red, green, blue };
let later = 40;
let forward = later + 2;
let quotient = -7 / 3;
let remainder = -7 % 3;
let bits = (~0 & 15) << 2;
let more_bits = ((20 >> 2) | 2) ^ 1;
let short_and = false && (1 / 0 == 0);
let short_or = true || (1 / 0 == 0);
let character = '\u{41}' == 'A';
let string_order = "alpha" < "beta";
let enum_order = Color.red < Color.blue;
`)})
	evaluator := newConstantEvaluator(inputs, diagnostics, Config{})
	for name, want := range map[string]string{
		"forward": "42", "quotient": "-2", "remainder": "-1", "bits": "60", "more_bits": "6",
	} {
		requireInteger(t, evaluator.evaluate(constantInitializer(t, evaluator, name)), want)
	}
	for _, name := range []string{"short_and", "short_or", "character", "string_order", "enum_order"} {
		result := evaluator.evaluate(constantInitializer(t, evaluator, name))
		if result.State != constantKnown || result.Value.Kind != constantBoolean {
			t.Fatalf("%s = %+v", name, result)
		}
		want := name != "short_and"
		if result.Value.Boolean != want {
			t.Fatalf("%s = %t, want %t", name, result.Value.Boolean, want)
		}
	}
	before := evaluator.operations
	requireInteger(t, evaluator.evaluate(constantInitializer(t, evaluator, "forward")), "42")
	if evaluator.operations != before {
		t.Fatalf("memoized query charged operations: %d -> %d", before, evaluator.operations)
	}
	if diagnostics.Len() != 0 {
		t.Fatalf("diagnostics = %+v", diagnostics.Items())
	}
}

func TestConstantImportedForwardReferencesAndCycleMemoization(t *testing.T) {
	inputs, diagnostics := constantInputs(t, checkProvider{
		"main.peb": []byte("import \"./lib\";\nlet answer = lib::size + forward;\nlet forward = later;\nlet later = 2;\nlet a = b;\nlet b = a;\n"),
		"lib.peb":  []byte("let size = 40;\n"),
	})
	evaluator := newConstantEvaluator(inputs, diagnostics, Config{})
	requireInteger(t, evaluator.evaluate(constantInitializer(t, evaluator, "answer")), "42")
	cycle := constantInitializer(t, evaluator, "a")
	if result := evaluator.evaluate(cycle); result.State != constantError {
		t.Fatalf("cycle result = %+v", result)
	}
	before := diagnostics.Len()
	if result := evaluator.evaluate(cycle); result.State != constantError || diagnostics.Len() != before {
		t.Fatalf("repeated cycle = %+v, diagnostics %d -> %d", result, before, diagnostics.Len())
	}
	if before != 1 || diagnostics.Items()[0].Code != CodeInvalidConstant {
		t.Fatalf("cycle diagnostics = %+v", diagnostics.Items())
	}
}

func TestConstantDepthLimitIsIndependentOfMemoOrder(t *testing.T) {
	sourceText := []byte("let leaf = (1);\nlet outer = leaf + 1;\n")
	evaluate := func(t *testing.T, preMemoize bool) (constantResult, diagnostic.Diagnostic) {
		t.Helper()
		inputs, diagnostics := constantInputs(t, checkProvider{"main.peb": sourceText})
		evaluator := newConstantEvaluator(inputs, diagnostics, Config{MaxConstantDepth: 3})
		if preMemoize {
			if result := evaluator.evaluate(constantInitializer(t, evaluator, "leaf")); result.State != constantKnown {
				t.Fatalf("leaf = %+v", result)
			}
		}
		result := evaluator.evaluate(constantInitializer(t, evaluator, "outer"))
		items := diagnostics.Items()
		if len(items) != 1 {
			t.Fatalf("diagnostics = %+v", items)
		}
		if leaf := evaluator.evaluate(constantInitializer(t, evaluator, "leaf")); leaf.State != constantKnown {
			t.Fatalf("leaf after outer query = %+v", leaf)
		}
		if diagnostics.Len() != 1 {
			t.Fatalf("leaf query duplicated diagnostics: %+v", diagnostics.Items())
		}
		frozen, ok := evaluator.freeze().Constant(constantInitializer(t, evaluator, "outer"))
		if !ok || frozen.contextual {
			t.Fatalf("frozen depth result retained contextual marker: %+v", frozen)
		}
		return result, items[0]
	}
	coldResult, coldDiagnostic := evaluate(t, false)
	warmResult, warmDiagnostic := evaluate(t, true)
	if coldResult.State != constantError || warmResult.State != coldResult.State {
		t.Fatalf("cold = %+v, pre-memoized = %+v", coldResult, warmResult)
	}
	if coldDiagnostic.Code != CodeInvalidConstant || warmDiagnostic.Code != coldDiagnostic.Code || warmDiagnostic.Message != coldDiagnostic.Message || warmDiagnostic.Primary.Span != coldDiagnostic.Primary.Span {
		t.Fatalf("cold diagnostic = %+v, pre-memoized = %+v", coldDiagnostic, warmDiagnostic)
	}
}

func TestConstantCachedSemanticErrorSkipsDepthRevalidation(t *testing.T) {
	inputs, diagnostics := constantInputs(t, checkProvider{"main.peb": []byte("let invalid = 1 / 0;\nlet dependent = ((invalid));\n")})
	evaluator := newConstantEvaluator(inputs, diagnostics, Config{MaxConstantDepth: 3})
	invalid := constantInitializer(t, evaluator, "invalid")
	if result := evaluator.evaluate(invalid); result.State != constantError {
		t.Fatalf("invalid = %+v", result)
	}
	if diagnostics.Len() != 1 {
		t.Fatalf("initial diagnostics = %+v", diagnostics.Items())
	}
	dependent := constantInitializer(t, evaluator, "dependent")
	if result := evaluator.evaluate(dependent); result.State != constantError {
		t.Fatalf("dependent = %+v", result)
	}
	if diagnostics.Len() != 1 {
		t.Fatalf("dependent added cascading diagnostic: %+v", diagnostics.Items())
	}
	for _, value := range evaluator.freeze().All() {
		if value.Result.contextual {
			t.Fatalf("memoized result retained contextual marker: %+v", value)
		}
	}

	unavailableInputs, unavailableDiagnostics := constantInputs(t, checkProvider{"main.peb": []byte("let unavailable = 1;\nlet dependent = ((unavailable));\n")})
	unavailableEvaluator := newConstantEvaluator(unavailableInputs, unavailableDiagnostics, Config{MaxConstantDepth: 3})
	unavailable := constantInitializer(t, unavailableEvaluator, "unavailable")
	unavailableEvaluator.memo[unavailable] = constantResult{State: constantUnavailable}
	unavailableEvaluator.memoDepth[unavailable] = constantDepth{height: 100}
	if result := unavailableEvaluator.evaluate(constantInitializer(t, unavailableEvaluator, "dependent")); result.State != constantUnavailable {
		t.Fatalf("dependent unavailable = %+v", result)
	}
	if unavailableDiagnostics.Len() != 0 {
		t.Fatalf("cached unavailable added diagnostic: %+v", unavailableDiagnostics.Items())
	}
}

func TestConstantCycleOriginIsIndependentOfEntryMember(t *testing.T) {
	sourceText := []byte("let a = b;\nlet b = c;\nlet c = a;\n")
	evaluate := func(t *testing.T, first string) diagnostic.Diagnostic {
		t.Helper()
		inputs, diagnostics := constantInputs(t, checkProvider{"main.peb": sourceText})
		evaluator := newConstantEvaluator(inputs, diagnostics, Config{})
		ref := constantInitializer(t, evaluator, first)
		if result := evaluator.evaluate(ref); result.State != constantError {
			t.Fatalf("%s = %+v", first, result)
		}
		before := diagnostics.Len()
		if result := evaluator.evaluate(ref); result.State != constantError || diagnostics.Len() != before {
			t.Fatalf("repeated %s = %+v, diagnostics %d -> %d", first, result, before, diagnostics.Len())
		}
		items := diagnostics.Items()
		if len(items) != 1 || items[0].Code != CodeInvalidConstant {
			t.Fatalf("diagnostics = %+v", items)
		}
		return items[0]
	}
	a := evaluate(t, "a")
	b := evaluate(t, "b")
	c := evaluate(t, "c")
	if a.Primary.Span != b.Primary.Span || a.Primary.Span != c.Primary.Span {
		t.Fatalf("cycle origins differ: a=%+v b=%+v c=%+v", a.Primary.Span, b.Primary.Span, c.Primary.Span)
	}
}

func TestArrayLengthOwnershipAndBoundaries(t *testing.T) {
	inputs, diagnostics := constantInputs(t, checkProvider{"main.peb": []byte(`
type Zero = [0]u8;
type Maximum = [18446744073709551615]u8;
type Overflow = [18446744073709551616]u8;
type Negative = [-1]u8;
type Boolean = [true]u8;
`)})
	evaluator := newConstantEvaluator(inputs, diagnostics, Config{MaxConstantBits: 65})
	refs := constantArrayLengths(inputs)
	if len(refs) != 5 {
		t.Fatalf("array refs = %d", len(refs))
	}
	wants := []infer.ArrayLengthResult{
		{State: infer.ArrayLengthKnown, Value: 0},
		{State: infer.ArrayLengthKnown, Value: ^uint64(0)},
		{State: infer.ArrayLengthError},
		{State: infer.ArrayLengthError},
		{State: infer.ArrayLengthError},
	}
	for i, ref := range refs {
		if got := evaluator.ArrayLength(ref); got != wants[i] {
			t.Fatalf("length %d = %+v, want %+v", i, got, wants[i])
		}
	}
	before := diagnostics.Len()
	if got := evaluator.ArrayLength(refs[3]); got.State != infer.ArrayLengthError || diagnostics.Len() != before {
		t.Fatalf("memoized length = %+v, diagnostics %d -> %d", got, before, diagnostics.Len())
	}
	if got := evaluator.ArrayLength(symbol.SyntaxRef{}); got.State != infer.ArrayLengthUnavailable {
		t.Fatalf("zero ref = %+v", got)
	}
	if got := evaluator.ArrayLength(symbol.SyntaxRef{Module: 999, Node: 1}); got.State != infer.ArrayLengthUnavailable {
		t.Fatalf("foreign ref = %+v", got)
	}
}

func TestConstantLimitsAndIndependentRecovery(t *testing.T) {
	tests := []struct {
		name   string
		source string
		config Config
		query  string
	}{
		{name: "depth", source: "let bad = (((1)));\n", config: Config{MaxConstantDepth: 2}, query: "bad"},
		{name: "operations", source: "let bad = 1 + 2;\n", config: Config{MaxConstantOperations: 2}, query: "bad"},
		{name: "literal bits", source: "let bad = 8;\n", config: Config{MaxConstantBits: 3}, query: "bad"},
		{name: "addition bits", source: "let bad = 7 + 1;\n", config: Config{MaxConstantBits: 3}, query: "bad"},
		{name: "multiplication bits", source: "let bad = 4 * 2;\n", config: Config{MaxConstantBits: 3}, query: "bad"},
		{name: "shift bits", source: "let bad = 1 << 3;\n", config: Config{MaxConstantBits: 3}, query: "bad"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			inputs, diagnostics := constantInputs(t, checkProvider{"main.peb": []byte(test.source)})
			evaluator := newConstantEvaluator(inputs, diagnostics, test.config)
			if result := evaluator.evaluate(constantInitializer(t, evaluator, test.query)); result.State != constantError {
				t.Fatalf("result = %+v", result)
			}
			if diagnostics.Len() != 1 || diagnostics.Items()[0].Code != CodeInvalidConstant {
				t.Fatalf("diagnostics = %+v", diagnostics.Items())
			}
		})
	}

	inputs, diagnostics := constantInputs(t, checkProvider{"main.peb": []byte("type A = [true]u8;\ntype B = [false]u8;\ntype C = [3]u8;\ntype D = [false]u8;\n")})
	evaluator := newConstantEvaluator(inputs, diagnostics, Config{MaxDiagnostics: 1})
	refs := constantArrayLengths(inputs)
	if got := evaluator.ArrayLength(refs[0]); got.State != infer.ArrayLengthError {
		t.Fatalf("first = %+v", got)
	}
	firstSpan := diagnostics.Items()[0].Primary.Span
	unrelated := diagnostic.Diagnostic{Severity: diagnostic.Error, Code: "X0001", Message: "unrelated"}
	diagnostics.Add(unrelated)
	if got := evaluator.ArrayLength(refs[1]); got.State != infer.ArrayLengthUnavailable {
		t.Fatalf("suppressed = %+v", got)
	}
	if got := evaluator.ArrayLength(refs[2]); got.State != infer.ArrayLengthKnown || got.Value != 3 {
		t.Fatalf("recovery = %+v", got)
	}
	if got := evaluator.ArrayLength(refs[3]); got.State != infer.ArrayLengthUnavailable {
		t.Fatalf("post-overflow suppression = %+v", got)
	}
	items := diagnostics.Items()
	if len(items) != 2 || items[0].Code != CodeGeneration || items[0].Message != "generation diagnostic limit of 1 reached" || items[0].Primary.Span != firstSpan || items[1].Code != unrelated.Code || items[1].Message != unrelated.Message {
		t.Fatalf("diagnostics = %+v", diagnostics.Items())
	}
}

func TestConstantBitwisePreflightMatchesArbitraryPrecisionSemantics(t *testing.T) {
	evaluator := &constantEvaluator{config: normalizeConfig(Config{MaxConstantBits: 8})}
	for _, op := range []syntax.TokenKind{syntax.Ampersand, syntax.Pipe, syntax.Caret} {
		for left := int64(-127); left <= 127; left++ {
			for right := int64(-127); right <= 127; right++ {
				a, b := big.NewInt(left), big.NewInt(right)
				want := new(big.Int)
				switch op {
				case syntax.Ampersand:
					want.And(a, b)
				case syntax.Pipe:
					want.Or(a, b)
				case syntax.Caret:
					want.Xor(a, b)
				}
				got, ok := evaluator.bitwiseResult(op, a, b)
				wantOK := want.BitLen() <= 8
				if ok != wantOK || ok && got.Cmp(want) != 0 {
					t.Fatalf("%d %s %d = {%v, %t}, want {%v, %t}", left, op, right, got, ok, want, wantOK)
				}
			}
		}
	}
}

func TestConstantIntegerParsingBoundsSignificantSpelling(t *testing.T) {
	evaluator := &constantEvaluator{config: normalizeConfig(Config{MaxConstantBits: 8})}
	tests := []struct {
		name     string
		spelling string
		want     string
		ok       bool
	}{
		{name: "binary zero", spelling: "0b0_0_0", want: "0", ok: true},
		{name: "binary boundary", spelling: "0b1111_1111", want: "255", ok: true},
		{name: "binary overflow", spelling: "0b1_0000_0000"},
		{name: "octal boundary", spelling: "0o3_7_7", want: "255", ok: true},
		{name: "octal overflow", spelling: "0o400"},
		{name: "decimal boundary", spelling: "2_5_5", want: "255", ok: true},
		{name: "decimal overflow", spelling: "256"},
		{name: "hex boundary", spelling: "0xF_F", want: "255", ok: true},
		{name: "hex overflow", spelling: "0x100"},
		{name: "long leading zeroes", spelling: "0x" + strings.Repeat("0_", 100_000) + "f_f", want: "255", ok: true},
		{name: "long significant overflow", spelling: "0x" + strings.Repeat("f_", 100_000) + "f"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			value, ok := evaluator.parseInteger([]byte(test.spelling))
			if ok != test.ok {
				t.Fatalf("parse ok = %t, want %t", ok, test.ok)
			}
			if ok && value.String() != test.want {
				t.Fatalf("value = %s, want %s", value, test.want)
			}
		})
	}
}

func TestConstantRejectedFormsAndFrozenCopies(t *testing.T) {
	inputs, diagnostics := constantInputs(t, checkProvider{"main.peb": []byte(`
let float_value = 1.5;
let aggregate = [1, 2];
let optional = some 1;
let cast = 1 as i32;
let size = sizeof i32;
` + "let interpolation = `value: {1}`;\n" + `
let divide = 1 / 0;
let remainder = 1 % 0;
let negative_shift = 1 << -1;
let category = true + 1;
let unequal_kinds = true == 1;
let unordered = true < false;
fn identity(value i32) i32 { let local = value; return local; }
let call = identity(1);
let pointer = &1;
let dereference = *pointer;
let indexing = [1, 2][0];
let tuple = (1, 2);
let repeat = [1; 2];
let function = fn() i32 => 1;
var mutable = 1;
let mutable_reference = mutable;
`)})
	evaluator := newConstantEvaluator(inputs, diagnostics, Config{})
	for _, name := range []string{
		"float_value", "aggregate", "optional", "cast", "size", "interpolation",
		"divide", "remainder", "negative_shift", "category", "unequal_kinds",
		"unordered", "call", "pointer", "dereference", "indexing", "tuple", "repeat",
		"function", "mutable_reference",
	} {
		if result := evaluator.evaluate(constantInitializer(t, evaluator, name)); result.State != constantError {
			t.Fatalf("%s = %+v", name, result)
		}
	}
	for _, reference := range inputs.Resolution.References() {
		candidate, ok := inputs.Resolution.Symbols.Symbol(reference.Symbol)
		if !ok || candidate.Name != "value" && candidate.Name != "local" {
			continue
		}
		if result := evaluator.evaluate(reference.Syntax); result.State != constantError {
			t.Fatalf("%s reference = %+v", candidate.Name, result)
		}
	}

	knownInputs, knownDiagnostics := constantInputs(t, checkProvider{"main.peb": []byte("let value = 123456789012345678901234567890;\n")})
	known := newConstantEvaluator(knownInputs, knownDiagnostics, Config{})
	ref := constantInitializer(t, known, "value")
	want := new(big.Int)
	want.SetString("123456789012345678901234567890", 10)
	requireInteger(t, known.evaluate(ref), want.String())
	frozen := known.freeze()
	first, ok := frozen.Constant(ref)
	if !ok {
		t.Fatal("frozen constant missing")
	}
	first.Value.Integer.SetInt64(0)
	all := frozen.All()
	all[0].Result.Value.Integer.SetInt64(1)
	second, ok := frozen.Constant(ref)
	if !ok || second.Value.Integer.Cmp(want) != 0 {
		t.Fatalf("frozen copy = %+v", second)
	}
}

func TestConstantNilAndInconsistentInputsAreUnavailable(t *testing.T) {
	for _, evaluator := range []*constantEvaluator{
		nil,
		newConstantEvaluator(Inputs{}, nil, Config{}),
		newConstantEvaluator(Inputs{Graph: &module.Graph{}, Sources: source.NewFileSet(), Resolution: &symbol.Result{}}, nil, Config{}),
	} {
		if result := evaluator.ArrayLength(symbol.SyntaxRef{}); result.State != infer.ArrayLengthUnavailable {
			t.Fatalf("result = %+v", result)
		}
	}
}
