package check

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func callableRecords(records []retainedRecord) []*callableRecord {
	var out []*callableRecord
	for index := range records {
		if records[index].Callable != nil {
			out = append(out, records[index].Callable)
		}
	}
	return out
}

func symbolForValue(g *generation, id valueID) symbol.SymbolID {
	root, ok := g.roots.root(id)
	if !ok || root.Kind != rootSymbol {
		return 0
	}
	return root.Symbol
}

func requireSlotRoot(t *testing.T, g *generation, id valueID) infer.SlotID {
	t.Helper()
	root, ok := g.roots.root(id)
	if !ok || root.Kind != rootSlot || root.Slot == (infer.SlotID{}) {
		t.Fatalf("value %d root = %+v, present=%v; want ordinary slot", id, root, ok)
	}
	count := 0
	for _, rooted := range g.roots.values {
		if rooted.Value == id {
			count++
		}
	}
	if count != 1 {
		t.Fatalf("value %d has %d roots", id, count)
	}
	return root.Slot
}

func TestDeclarationFactsExactRecordsPreparedTypesAndPublications(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Box[T] = struct { value T; fn replace[U](self Box[T], left, right U) U => left; };
type Plain = struct { fn get(self Plain) i32 => 1; };
fn choose(first, second i32) i32 => first;
extern { fn foreign(value i32) i32; let external i32; }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid declaration diagnostics: %+v", diagnostics.Items())
	}
	records := facts.Generation.records.values
	var choose, method, plainMethod, foreign *callableRecord
	for _, record := range callableRecords(records) {
		sym, _ := inputs.Resolution.Symbols.Symbol(record.Symbol)
		switch sym.Name {
		case "choose":
			choose = record
		case "replace":
			method = record
		case "get":
			plainMethod = record
		case "foreign":
			foreign = record
		}
	}
	if choose == nil || method == nil || plainMethod == nil || foreign == nil {
		t.Fatalf("missing callable records: choose=%v method=%v plain=%v foreign=%v", choose, method, plainMethod, foreign)
	}
	if len(choose.Parameters) != 2 {
		t.Fatalf("grouped parameters = %v", choose.Parameters)
	}
	first, _ := inputs.Resolution.Symbols.Symbol(symbolForValue(facts.Generation, choose.Parameters[0]))
	second, _ := inputs.Resolution.Symbols.Symbol(symbolForValue(facts.Generation, choose.Parameters[1]))
	if first.Name != "first" || second.Name != "second" {
		t.Fatalf("grouped order = %q, %q", first.Name, second.Name)
	}
	methodSignature, _ := facts.Program.Signature(method.Symbol)
	if len(methodSignature.TypeParams) != 2 {
		t.Fatalf("method generic parameters = %v", methodSignature.TypeParams)
	}
	ownerParameter, _ := inputs.Resolution.Symbols.Symbol(methodSignature.TypeParams[0])
	methodParameter, _ := inputs.Resolution.Symbols.Symbol(methodSignature.TypeParams[1])
	if ownerParameter.Name != "T" || methodParameter.Name != "U" {
		t.Fatalf("method generic order = %q, %q", ownerParameter.Name, methodParameter.Name)
	}
	if foreign.Kind != callableExtern || foreign.Convention != types.C || foreign.BodyPresent {
		t.Fatalf("extern callable = %+v", foreign)
	}
	for _, id := range []symbol.SymbolID{choose.Symbol, plainMethod.Symbol, foreign.Symbol} {
		if !facts.Walk.publishedSymbols[id] {
			t.Fatalf("callable %d was not published", id)
		}
	}
	preparedUses := 0
	for _, record := range records {
		if record.TypeUse != nil {
			preparedUses++
			requireSlotRoot(t, facts.Generation, record.TypeUse.Type)
			if facts.Walk.resolvedTypes[record.TypeUse.Header.Syntax] && facts.Walk.preparedTypes[record.TypeUse.Header.Syntax] {
				t.Fatalf("prepared type re-resolved: %+v", record.TypeUse)
			}
		}
	}
	if preparedUses == 0 {
		t.Fatal("no exact typeUseRecord retained")
	}
	for _, record := range callableRecords(records) {
		requireSlotRoot(t, facts.Generation, record.Result)
	}
}

func TestDeclarationFactsAnonymousCallableHasSymbol(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let value = fn(argument i32) i32 => argument;`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("diagnostics: %+v", diagnostics.Items())
	}
	var literal *callableRecord
	for _, record := range callableRecords(facts.Generation.records.values) {
		if record.Kind == callableLiteral {
			literal = record
			break
		}
	}
	if literal == nil || literal.Symbol == 0 {
		t.Fatalf("anonymous callable record = %+v", literal)
	}
	sym, ok := inputs.Resolution.Symbols.Symbol(literal.Symbol)
	if !ok || sym.Kind != symbol.SymbolFunction || sym.Error || sym.Declaration != literal.Header.Syntax {
		t.Fatalf("anonymous callable symbol = %+v, ok=%v", sym, ok)
	}
}

func TestDeclarationFactsBodyOwnerGenericOwnerAndIndependentRecovery(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn body() void { let damaged; let valid ?*i32 = nil; }
fn generic[T](value T) void { let local T = value; }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	var bodyOwner, genericOwner symbol.SymbolID
	for _, value := range inputs.Resolution.Symbols.All() {
		if value.Kind == symbol.SymbolFunction && value.Name == "body" {
			bodyOwner = value.ID
		}
		if value.Kind == symbol.SymbolFunction && value.Name == "generic" {
			genericOwner = value.ID
		}
	}
	if len(facts.Walk.resolvedTypes) != 2 {
		t.Fatalf("body-owned ResolveType queries = %v", facts.Walk.resolvedTypes)
	}
	valid, local := false, false
	for _, stored := range facts.Generation.records.values {
		if stored.Binding == nil {
			continue
		}
		sym, _ := inputs.Resolution.Symbols.Symbol(stored.Binding.Symbol)
		switch sym.Name {
		case "valid":
			valid = !stored.Binding.Header.Suppressed
			origin := facts.Generation.values[stored.Binding.Annotation-1].Origin
			if origin.Symbol != bodyOwner || origin.GenericOwner != 0 {
				t.Fatalf("nongeneric annotation origin = %+v", origin)
			}
		case "local":
			local = true
			origin := facts.Generation.values[stored.Binding.Annotation-1].Origin
			if origin.Symbol != genericOwner || origin.GenericOwner != genericOwner {
				t.Fatalf("generic annotation origin = %+v", origin)
			}
		}
	}
	if !valid || !local {
		t.Fatalf("independent binding recovery valid/local = %v/%v diagnostics=%+v", valid, local, diagnostics.Items())
	}
}

func newPublicationWalker(t *testing.T, max uint32) (*walker, []symbol.SymbolID, *diagnostic.DiagnosticSet) {
	t.Helper()
	return newPublicationWalkerWithConfig(t, Config{MaxSyntaxVisits: max})
}

func newPublicationWalkerWithConfig(t *testing.T, requested Config) (*walker, []symbol.SymbolID, *diagnostic.DiagnosticSet) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let first i32 = 1; let second i32 = 2;")})
	config := normalizeConfig(requested)
	generation := newGeneration(inputs, diagnostics, config)
	evaluator := newConstantEvaluatorWithBudget(inputs, diagnostics, config, generation.reporter.budget)
	program := infer.Prepare(infer.ProgramInputs{Graph: inputs.Graph, Sources: inputs.Sources, Resolution: inputs.Resolution, Types: inputs.Types, ArrayLengths: evaluator, LiteralTarget: inputs.LiteralTarget}, diagnostics, config.Inference)
	walk := newWalker(generation, evaluator, program, infer.NewSession(program, diagnostics, config.Inference))
	var bindings []symbol.SymbolID
	for _, value := range inputs.Resolution.Symbols.All() {
		if value.Kind == symbol.SymbolBinding {
			bindings = append(bindings, value.ID)
		}
	}
	return walk, bindings, diagnostics
}

func TestDeclarationFactsPublicationDuplicateAndRootFailureAreAtomic(t *testing.T) {
	t.Run("duplicate", func(t *testing.T) {
		walk, bindings, _ := newPublicationWalker(t, 4)
		known := walk.session.Known(walk.generation.inputs.Types.Builtins().I32)
		if _, ok := walk.publishSymbol(bindings[0], known, infer.Origin{Symbol: bindings[0]}); !ok {
			t.Fatal("first publication failed")
		}
		values, roots := len(walk.generation.values), len(walk.generation.roots.values)
		if _, ok := walk.publishSymbol(bindings[0], known, infer.Origin{Symbol: bindings[0]}); ok {
			t.Fatal("duplicate publication succeeded")
		}
		if len(walk.generation.values) != values || len(walk.generation.roots.values) != roots {
			t.Fatal("duplicate publication left partial checker state")
		}
	})
	t.Run("root limit", func(t *testing.T) {
		walk, bindings, _ := newPublicationWalker(t, 1)
		known := walk.session.Known(walk.generation.inputs.Types.Builtins().I32)
		if _, ok := walk.publishSymbol(bindings[0], known, infer.Origin{Symbol: bindings[0]}); !ok {
			t.Fatal("first publication failed")
		}
		values, roots := len(walk.generation.values), len(walk.generation.roots.values)
		if _, ok := walk.publishSymbol(bindings[1], known, infer.Origin{Symbol: bindings[1]}); ok {
			t.Fatal("over-limit publication succeeded")
		}
		if len(walk.generation.values) != values || len(walk.generation.roots.values) != roots || walk.publishedSymbols[bindings[1]] {
			t.Fatal("root failure left orphan value/root/publication")
		}
		if _, exists := walk.termsBySymbol[bindings[1]]; exists {
			t.Fatal("root preflight failure retained a symbol term")
		}
	})
	t.Run("slot root limit", func(t *testing.T) {
		walk, bindings, _ := newPublicationWalker(t, 1)
		known := walk.session.Known(walk.generation.inputs.Types.Builtins().I32)
		if _, ok := walk.publishSymbol(bindings[0], known, infer.Origin{Symbol: bindings[0]}); !ok {
			t.Fatal("root capacity setup failed")
		}
		values, roots := len(walk.generation.values), len(walk.generation.roots.values)
		if value, ok := walk.newSlotValue(known, infer.Origin{}); ok || value.ID != 0 {
			t.Fatalf("over-limit slot publication succeeded: %+v", value)
		}
		if len(walk.generation.values) != values || len(walk.generation.roots.values) != roots {
			t.Fatal("slot root failure left an orphan generated value or root")
		}
		if slot := walk.session.PublishSlot(known); slot == (infer.SlotID{}) {
			t.Fatal("slot root preflight consumed Session publication state")
		}
	})
	t.Run("Session slot rejection", func(t *testing.T) {
		walk, _, diagnostics := newPublicationWalkerWithConfig(t, Config{
			MaxSyntaxVisits: 4,
			Inference:       infer.Config{MaxSolvedSlots: 1},
		})
		if slot := walk.session.PublishSlot(walk.session.Known(walk.generation.inputs.Types.Builtins().I32)); slot == (infer.SlotID{}) {
			t.Fatal("slot capacity setup failed")
		}
		values, roots := len(walk.generation.values), len(walk.generation.roots.values)
		counters := walk.generation.counters
		term := walk.session.Known(walk.generation.inputs.Types.Builtins().Bool)
		if value, ok := walk.newSlotValue(term, infer.Origin{}); ok || value.ID != 0 {
			t.Fatalf("rejected Session slot retained a generated identity: %+v", value)
		}
		if len(walk.generation.values) != values || len(walk.generation.roots.values) != roots || walk.generation.counters != counters {
			t.Fatal("rejected Session slot left an orphan generated value or root")
		}
		if len(walk.publishedSlots) != 0 {
			t.Fatal("rejected Session slot retained checker slot state")
		}
		walk.session.Solve()
		requireSingleDiagnosticCode(t, diagnostics, infer.CodeResourceLimit)
		if _, ok := walk.addRecord(retainedRecord{}); ok || len(walk.generation.records.values) != 0 {
			t.Fatal("fatal slot rejection allowed later semantic record publication")
		}
	})
	t.Run("Session duplicate slot rejection", func(t *testing.T) {
		walk, _, diagnostics := newPublicationWalker(t, 4)
		term := walk.session.Known(walk.generation.inputs.Types.Builtins().I32)
		if slot := walk.session.PublishSlot(term); slot == (infer.SlotID{}) {
			t.Fatal("duplicate slot setup failed")
		}
		values, roots := len(walk.generation.values), len(walk.generation.roots.values)
		counters := walk.generation.counters
		if value, ok := walk.newSlotValue(term, infer.Origin{}); ok || value.ID != 0 {
			t.Fatalf("duplicate Session slot retained a generated identity: %+v", value)
		}
		if len(walk.generation.values) != values || len(walk.generation.roots.values) != roots || len(walk.publishedSlots) != 0 || walk.generation.counters != counters {
			t.Fatal("duplicate Session slot left partial checker publication state")
		}
		walk.session.Solve()
		requireSingleDiagnosticCode(t, diagnostics, infer.CodeResourceLimit)
	})
	t.Run("Session recovery slot rejection", func(t *testing.T) {
		walk, _, diagnostics := newPublicationWalkerWithConfig(t, Config{
			MaxSyntaxVisits: 4,
			Inference:       infer.Config{MaxSolvedSlots: 1},
		})
		term := walk.session.Known(walk.generation.inputs.Types.Builtins().Bool)
		value := walk.newValue(term, infer.Origin{})
		if value.ID == 0 {
			t.Fatal("recovery value setup failed")
		}
		if slot := walk.session.PublishSlot(walk.session.Known(walk.generation.inputs.Types.Builtins().I32)); slot == (infer.SlotID{}) {
			t.Fatal("slot capacity setup failed")
		}
		values, roots := len(walk.generation.values), len(walk.generation.roots.values)
		counters := walk.generation.counters
		if rooted, ok := walk.rootExistingSlot(value, infer.Origin{}); ok || rooted.ID != value.ID {
			t.Fatalf("rejected recovery slot result=%+v ok=%v", rooted, ok)
		}
		if len(walk.generation.values) != values || len(walk.generation.roots.values) != roots || len(walk.publishedSlots) != 0 || walk.generation.counters != counters {
			t.Fatal("rejected recovery slot changed checker publication state")
		}
		walk.session.Solve()
		requireSingleDiagnosticCode(t, diagnostics, infer.CodeResourceLimit)
	})
	t.Run("Session duplicate symbol rejection", func(t *testing.T) {
		walk, bindings, diagnostics := newPublicationWalker(t, 4)
		term := walk.session.Known(walk.generation.inputs.Types.Builtins().I32)
		walk.session.PublishSymbol(bindings[0], term)
		values, roots := len(walk.generation.values), len(walk.generation.roots.values)
		counters := walk.generation.counters
		if value, ok := walk.publishSymbol(bindings[0], term, infer.Origin{Symbol: bindings[0]}); ok || value.ID != 0 {
			t.Fatalf("duplicate Session symbol retained a generated identity: %+v", value)
		}
		if len(walk.generation.values) != values || len(walk.generation.roots.values) != roots || walk.publishedSymbols[bindings[0]] || walk.generation.counters != counters {
			t.Fatal("duplicate Session symbol left checker value/root/publication state")
		}
		if _, exists := walk.valuesBySymbol[bindings[0]]; exists {
			t.Fatal("duplicate Session symbol retained checker value lookup")
		}
		if _, exists := walk.termsBySymbol[bindings[0]]; exists {
			t.Fatal("duplicate Session symbol retained checker term lookup")
		}
		walk.session.Solve()
		requireSingleDiagnosticCode(t, diagnostics, infer.CodeResourceLimit)
		if _, ok := walk.addRecord(retainedRecord{}); ok || len(walk.generation.records.values) != 0 {
			t.Fatal("fatal symbol rejection allowed later semantic record publication")
		}
	})
	t.Run("Session duplicate symbol restores existing term", func(t *testing.T) {
		walk, bindings, diagnostics := newPublicationWalker(t, 4)
		origin := infer.Origin{Symbol: bindings[0]}
		previous := walk.symbolTerm(bindings[0], origin)
		walk.session.PublishSymbol(bindings[0], previous)
		incoming := walk.session.Known(walk.generation.inputs.Types.Builtins().I32)
		values, roots := len(walk.generation.values), len(walk.generation.roots.values)
		counters := walk.generation.counters
		if value, ok := walk.publishSymbol(bindings[0], incoming, origin); ok || value.ID != 0 {
			t.Fatalf("duplicate Session symbol retained a generated identity: %+v", value)
		}
		if got, exists := walk.termsBySymbol[bindings[0]]; !exists || got != previous {
			t.Fatalf("existing symbol term=%+v present=%v, want %+v", got, exists, previous)
		}
		if len(walk.generation.values) != values || len(walk.generation.roots.values) != roots || walk.publishedSymbols[bindings[0]] || walk.generation.counters != counters {
			t.Fatal("duplicate Session symbol changed checker publication state")
		}
		if _, exists := walk.valuesBySymbol[bindings[0]]; exists {
			t.Fatal("duplicate Session symbol retained checker value lookup")
		}
		walk.session.Solve()
		requireSingleDiagnosticCode(t, diagnostics, infer.CodeResourceLimit)
	})
}

func TestContextFactsUseExactRuntimeIdentityOnly(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn current() void { context; }`)})
	facts := run06a3(inputs, diagnostics, Config{})
	runtime, ok := facts.Program.RuntimeTypes()
	var context *contextFlowRecord
	for _, record := range facts.Generation.records.values {
		if record.ContextFlow != nil {
			context = record.ContextFlow
		}
	}
	if !ok || context == nil || context.Context != runtime.Context || context.Caller.Symbol == 0 || context.Header.Suppressed {
		t.Fatalf("context record = %+v runtime=%+v ready=%v diagnostics=%+v", context, runtime, ok, diagnostics.Items())
	}
	for _, record := range callableRecords(facts.Generation.records.values) {
		for _, parameter := range record.Parameters {
			sym, _ := inputs.Resolution.Symbols.Symbol(symbolForValue(facts.Generation, parameter))
			if sym.Runtime == symbol.RuntimeContext || sym.Name == "Context" {
				t.Fatalf("Context leaked into parameters: %+v", sym)
			}
		}
		for _, capture := range record.Captures {
			sym, _ := inputs.Resolution.Symbols.Symbol(capture)
			if sym.Runtime == symbol.RuntimeContext {
				t.Fatalf("Context leaked into captures: %+v", sym)
			}
		}
	}
}

func TestContextFactsRetainSuppressedRecordWithoutRuntimeContext(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn current() void { context; }`)})
	config := normalizeConfig(Config{})
	generation := newGeneration(inputs, diagnostics, config)
	evaluator := newConstantEvaluatorWithBudget(inputs, diagnostics, config, generation.reporter.budget)
	program := infer.Prepare(infer.ProgramInputs{
		Graph: inputs.Graph, Sources: inputs.Sources, Resolution: inputs.Resolution,
		Types: inputs.Types, ArrayLengths: evaluator, LiteralTarget: inputs.LiteralTarget,
	}, diagnostics, config.Inference)
	walk := newWalker(generation, evaluator, program, infer.NewSession(program, diagnostics, config.Inference))
	walk.runtimeTypes = func() (infer.RuntimeTypes, bool) { return infer.RuntimeTypes{}, false }
	walk.run()
	generation.reporter.flush()

	var context *contextFlowRecord
	var contextRef symbol.SyntaxRef
	for _, record := range generation.records.values {
		if record.ContextFlow != nil {
			context = record.ContextFlow
			contextRef = record.Header.Syntax
		}
	}
	if context == nil || context.Kind != contextExpression || context.Context != 0 || !context.Header.Suppressed {
		t.Fatalf("suppressed context record = %+v", context)
	}
	valueFound := false
	for _, rooted := range generation.roots.values {
		if rooted.Root.Kind == rootSyntax && rooted.Root.Syntax == contextRef {
			valueFound = true
			if generation.values[rooted.Value-1].Origin.Role != "runtime context" {
				t.Fatal("context recovery syntax root points at an unrelated value")
			}
		}
	}
	if !valueFound || !walk.publishedSyntax[contextRef] {
		t.Fatal("missing error syntax publication for unavailable runtime Context")
	}
}

func TestContextFactsZeroContextOnlyForSuppressedExpressionRecovery(t *testing.T) {
	header := recordHeader{Suppressed: true}
	valid := retainedRecord{Header: header, ContextFlow: &contextFlowRecord{
		Header: header, Kind: contextExpression,
	}}
	var arena recordArena
	if _, ok := arena.append(valid, func(valueID) bool { return true }, func(controlID) bool { return true }, 3, 3); !ok {
		t.Fatal("suppressed context expression rejected zero Context recovery")
	}

	unsuppressed := valid
	unsuppressed.Header.Suppressed = false
	unsuppressed.ContextFlow = &contextFlowRecord{Header: unsuppressed.Header, Kind: contextExpression}
	if _, ok := arena.append(unsuppressed, func(valueID) bool { return true }, func(controlID) bool { return true }, 3, 3); ok {
		t.Fatal("unsuppressed context expression accepted zero Context")
	}
	forward := valid
	forward.ContextFlow = &contextFlowRecord{Header: header, Kind: contextForward}
	if _, ok := arena.append(forward, func(valueID) bool { return true }, func(controlID) bool { return true }, 3, 3); ok {
		t.Fatal("non-expression context flow accepted zero Context")
	}
}

func TestDeclarationFactsInitializerPlaceholderRetainsExactValue(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let answer i32 = 42;`)})
	facts := run06a3(inputs, diagnostics, Config{})
	var initializer valueID
	var initializerRef symbol.SyntaxRef
	for _, record := range facts.Generation.records.values {
		if record.Binding != nil && record.Binding.InitializerPresent {
			initializer = record.Binding.Initializer
			moduleValue, _ := inputs.Graph.Module(record.Header.Syntax.Module)
			node, _ := moduleValue.Tree.Node(record.Header.Syntax.Node)
			_, initializerRef, _, _ = bindingParts(record.Header.Syntax, node)
			break
		}
	}
	reserved, ok := facts.Walk.valuesBySyntax[initializerRef]
	if !ok || reserved.ID != initializer || reserved.Term != facts.Generation.values[initializer-1].Term {
		t.Fatalf("initializer reservation = %+v, record value=%d", reserved, initializer)
	}
	root, rooted := facts.Generation.roots.root(initializer)
	if !rooted || root.Kind != rootSyntax || root.Syntax != initializerRef {
		t.Fatalf("06a.4 did not attach the initializer syntax root to its reserved value: %+v", root)
	}
}

func TestDeclarationFactsGlobalInitializerRetainsSyntaxAndConstant(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let value i32 = 1;`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	var binding *bindingRecord
	for _, retained := range handoff.Records.Records() {
		if retained.Binding == nil || !retained.Binding.Global {
			continue
		}
		value, exists := inputs.Resolution.Symbols.Symbol(retained.Binding.Symbol)
		if exists && value.Name == "value" {
			binding = retained.Binding
			break
		}
	}
	if binding == nil || !binding.InitializerPresent || binding.InitializerSyntax == (symbol.SyntaxRef{}) {
		t.Fatalf("global binding = %+v", binding)
	}
	item, exists := inputs.Graph.Module(binding.InitializerSyntax.Module)
	if !exists {
		t.Fatalf("missing module for initializer ref %+v", binding.InitializerSyntax)
	}
	node, exists := item.Tree.Node(binding.InitializerSyntax.Node)
	file, fileExists := inputs.Sources.File(node.Span().Source)
	if !exists || !fileExists || string(file.Slice(node.Span())) != "1" {
		t.Fatalf("initializer ref %+v does not span %q", binding.InitializerSyntax, "1")
	}
	result, found := records.Constant(binding.InitializerSyntax)
	if !found || result.State != constantKnown || result.Value.Kind != constantInteger || result.Value.Integer.String() != "1" {
		t.Fatalf("constant for %+v = %+v, found=%v, want integer 1", binding.InitializerSyntax, result, found)
	}
}

func TestDeclarationFactsAnonymousAndUnsupportedRecords(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let global i32 = 1;
let reads_global = fn() i32 => global;
fn outer(local i32) void {
    let captures = fn() i32 => local;
    let generic = fn[T](value T) T => value;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	valid, captured, unsupported := 0, 0, 0
	for _, record := range facts.Generation.records.values {
		if record.UnsupportedCallable != nil {
			unsupported++
			if len(record.UnsupportedCallable.TypeParameters) != 1 {
				t.Fatalf("unsupported evidence = %+v", record.UnsupportedCallable)
			}
		}
		if record.Callable != nil && record.Callable.Kind == callableLiteral {
			if len(record.Callable.Captures) == 0 {
				valid++
			} else {
				captured++
				if !record.Callable.Header.Suppressed {
					t.Fatal("capture record not suppressed")
				}
			}
		}
	}
	if valid != 1 || captured != 1 || unsupported != 1 {
		t.Fatalf("literal records valid/captured/unsupported = %d/%d/%d", valid, captured, unsupported)
	}
	for _, item := range diagnostics.Items() {
		if item.Code == "C0608" || item.Code == "C0617" {
			t.Fatalf("06a.3 emitted 06b diagnostic: %+v", item)
		}
	}
}

func TestDeclarationFactsGenericAnonymousDoesNotResolveInvalidOwner(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let generic = fn[T](value T) T => value;`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if len(facts.Walk.resolvedTypes) != 0 {
		t.Fatalf("generic anonymous resolved types: %v", facts.Walk.resolvedTypes)
	}
	unsupported := 0
	for _, record := range facts.Generation.records.values {
		if record.UnsupportedCallable != nil {
			unsupported++
		}
	}
	if unsupported != 1 {
		t.Fatalf("unsupported records = %d", unsupported)
	}
}

// TestDeclarationFactsGenericAnonymousReachesFullPipeline is a regression test
// for a fixed bug in infer.semanticSnapshotBuilder.validateOwnerJoins: a type
// parameter with no owner (Containing == 0, exactly what an unsupported
// generic anonymous function's own type parameter gets from the resolver,
// since it has no containing symbol to register against) was included when
// building the owner-table's expected join, while prepareDeclarations already
// skips registering owners for Containing == 0. That inconsistency made
// run06a fail with T0512 ("semantic snapshot owner table is missing or has
// an extra owner") for this exact source, even though nothing was actually
// wrong -- and meant no 06b validator could ever observe this construct
// through the real run06a/resolveRecords pipeline, only via direct record
// fabrication (see TestValidateCallableRecordsRejectsCapturesAndGenericAnonymous
// in call_validation_test.go).
func TestDeclarationFactsGenericAnonymousReachesFullPipeline(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`let identity = fn[T](value T) T => value;`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed to produce a usable handoff: %+v", diagnostics.Items())
	}
	for _, item := range diagnostics.Items() {
		if item.Code == "T0512" {
			t.Fatalf("owner table inconsistency resurfaced: %+v", diagnostics.Items())
		}
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	found := false
	for _, retained := range handoff.Records.Records() {
		if retained.UnsupportedCallable != nil {
			found = true
		}
	}
	if !found {
		t.Fatal("expected an active unsupportedCallableRecord to reach the real pipeline")
	}
	if validateCallableRecords(handoff, records, diagnostics, Config{}) || !hasValidationDiagnostic(diagnostics, CodeGenericAnonymous) {
		t.Fatalf("generic anonymous function was not rejected end-to-end: %+v", diagnostics.Items())
	}
}

// TestDeclarationFactsFreezePreservesRecords verifies freeze() preserves the
// exact records built during the mutable phase: headers, type-use slot roots,
// and callable-result slot roots all survive. The frozenRecords Records()
// accessor no longer defensively copies (frozen state is immutable by contract,
// callers must not mutate the shared view), so no mutate-then-reread isolation
// is asserted here.
func TestDeclarationFactsFreezePreservesRecords(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn use(value i32) void { let local i32 = value; context; }`)})
	facts := run06a3(inputs, diagnostics, Config{})
	before, _ := facts.Generation.records.record(1)
	frozen, ok := facts.Generation.freeze()
	if !ok {
		t.Fatalf("freeze failed: %+v", diagnostics.Items())
	}
	records := frozen.records.Records()
	if len(records) == 0 || before.Header.ID != records[0].Header.ID {
		t.Fatalf("frozen exact records missing: before=%+v after=%+v", before, records)
	}
	for _, record := range records {
		if record.TypeUse != nil {
			root, exists := frozen.roots.Root(record.TypeUse.Type)
			if !exists || root.Kind != rootSlot || root.Slot == (infer.SlotID{}) {
				t.Fatalf("freeze lost type-use slot root: %+v", record.TypeUse)
			}
		}
		if record.Callable != nil {
			root, exists := frozen.roots.Root(record.Callable.Result)
			if !exists || root.Kind != rootSlot || root.Slot == (infer.SlotID{}) {
				t.Fatalf("freeze lost callable-result slot root: %+v", record.Callable)
			}
		}
	}
}

// controlBindingRecords returns every retained controlBinding control record.
func controlBindingRecords(records []retainedRecord) []retainedRecord {
	var out []retainedRecord
	for _, record := range records {
		if record.Control != nil && record.Control.Kind == controlBinding {
			out = append(out, record)
		}
	}
	return out
}

// TestDeclarationFactsLocalBindingRetainsControlBinding proves an ordinary
// local let/var is no longer invisible to sequential flow: each retains its
// own controlBinding record, in authored order, in the function's own block
// region, as a leaf with an empty Composition.
func TestDeclarationFactsLocalBindingRetainsControlBinding(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn h() void {
    let x = 1;
    let y = 2;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	bindings := controlBindingRecords(facts.Generation.records.values)
	if len(bindings) != 2 {
		t.Fatalf("controlBinding records = %d, want 2", len(bindings))
	}
	for _, record := range bindings {
		ctrl := record.Control
		if regionOwningControl(ctrl.Kind) {
			t.Fatalf("controlBinding %+v is region-owning", ctrl)
		}
		if len(ctrl.Composition) != 0 {
			t.Fatalf("controlBinding %+v carries a nonempty composition", ctrl)
		}
		if ctrl.Region == 0 {
			t.Fatalf("controlBinding %+v has no region", ctrl)
		}
		if bindings[0].Control.Region != ctrl.Region {
			t.Fatalf("controlBinding records disagree on region: %+v vs %+v", bindings[0].Control, ctrl)
		}
		item, ok := inputs.Graph.Module(record.Header.Syntax.Module)
		if !ok {
			t.Fatal("missing module")
		}
		node, ok := item.Tree.Node(record.Header.Syntax.Node)
		if !ok || node.Kind() != syntax.BindingDecl {
			t.Fatalf("controlBinding record names kind %v, want BindingDecl", node.Kind())
		}
	}
	if bindings[0].Header.ID >= bindings[1].Header.ID {
		t.Fatalf("controlBinding records not in authored order: %+v", bindings)
	}
}

// TestDeclarationFactsControlBindingRetainedWithoutSymbol proves the
// top-of-function gate is symbol-independent: retention happens even for the
// exact failure handleBinding's own early return covers, where
// declarationSymbols(ref) resolves no SymbolBinding/SymbolExternBinding
// symbol at all (binding.ID == 0).
func TestDeclarationFactsControlBindingRetainedWithoutSymbol(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn body() void {
    let x i32 = 1;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	var ref symbol.SyntaxRef
	var node syntax.Node
	for _, candidate := range facts.Walk.order {
		candidateNode, ok := facts.Walk.node(candidate.Module, candidate.Node)
		if ok && candidateNode.Kind() == syntax.BindingDecl {
			ref, node = candidate, candidateNode
			break
		}
	}
	if ref == (symbol.SyntaxRef{}) {
		t.Fatal("no BindingDecl found")
	}
	var region controlID
	before := len(controlBindingRecords(facts.Generation.records.values))
	for _, record := range facts.Generation.records.values {
		if record.Control != nil && record.Control.Kind == controlBinding && record.Header.Syntax == ref {
			region = record.Control.Region
		}
	}
	if region == 0 {
		t.Fatal("setup: no existing controlBinding region to reuse")
	}
	// Simulate declarationSymbols(ref) resolving no binding symbol at all,
	// exactly the case handleBinding's own `if binding.ID == 0 { return }`
	// covers, independent from the top-of-function gate this proves.
	saved := facts.Walk.symbolsAt[ref]
	delete(facts.Walk.symbolsAt, ref)
	ctx := walkContext{control: controlContext{region: region}, callable: callableRef{Syntax: ref}}
	facts.Walk.handleBinding(ref, node, ctx)
	facts.Walk.symbolsAt[ref] = saved

	after := len(controlBindingRecords(facts.Generation.records.values))
	if after != before+1 {
		t.Fatalf("controlBinding records = %d, want %d (symbol-independent retention failed)", after, before+1)
	}
}

// TestDeclarationFactsNoControlBindingForGlobalsExternsParametersOrIterators
// proves the structural exclusion holds: a global binding, an extern
// module-level binding, a function parameter, and a range-loop iterator each
// retain no controlBinding record.
func TestDeclarationFactsNoControlBindingForGlobalsExternsParametersOrIterators(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let global_value i32 = 1;
extern { let extern_value i32; }
fn f(param i32) void {
    loop 0..param : idx { print idx; }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("valid diagnostics: %+v", diagnostics.Items())
	}
	if bindings := controlBindingRecords(facts.Generation.records.values); len(bindings) != 0 {
		t.Fatalf("controlBinding records = %+v, want none", bindings)
	}
}

func TestDeclarationFactsOwnedFixtures(t *testing.T) {
	patterns := []string{
		"../../../tests/check/facts/valid/declaration_*.peb", "../../../tests/check/facts/valid/context_*.peb",
		"../../../tests/check/facts/valid/anonymous_function_*.peb", "../../../tests/check/facts/invalid/C0608/generic_anonymous_*.peb",
		"../../../tests/check/facts/invalid/C0617/anonymous_capture_*.peb",
		"../../../tests/check/facts/recovery/declaration_*.peb",
	}
	for _, pattern := range patterns {
		paths, err := filepath.Glob(pattern)
		if err != nil {
			t.Fatal(err)
		}
		if len(paths) == 0 {
			t.Fatalf("no fixtures match %s", pattern)
		}
		for _, path := range paths {
			t.Run(fmt.Sprintf("%s/%s", filepath.Base(filepath.Dir(path)), filepath.Base(path)), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
				facts := run06a3(inputs, diagnostics, Config{})
				if len(facts.Walk.order) == 0 || len(facts.Generation.records.values) == 0 {
					t.Fatal("fixture produced no traversal/records")
				}
				for _, item := range diagnostics.Items() {
					if item.Code == CodeGeneration || item.Code == "C0608" || item.Code == "C0617" {
						t.Fatalf("unexpected diagnostic: %+v", item)
					}
				}
			})
		}
	}
}

func TestDeclarationFactsVisitLimitFixtureRequiresC0619(t *testing.T) {
	contents, err := os.ReadFile("../../../tests/check/facts/invalid/C0619/declaration_visit_limit.peb")
	if err != nil {
		t.Fatal(err)
	}
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
	run06a3(inputs, diagnostics, Config{MaxSyntaxVisits: 2})
	for _, item := range diagnostics.Items() {
		if item.Code == CodeGeneration {
			return
		}
	}
	t.Fatalf("lowered declaration visit fixture diagnostics = %+v; want C0619", diagnostics.Items())
}

func TestShared06aDiagnosticBudgetReplacesOnlyItsLastDiagnostic(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`type Bad = [1 / 0]i32; let later i32 = 1;`)})
	diagnostics.Error("EARLY", "earlier phase", source.Span{})
	run06a3(inputs, diagnostics, Config{MaxDiagnostics: 1, MaxTraversalDepth: 1})
	items := diagnostics.Items()
	if len(items) != 2 || items[0].Code != "EARLY" || items[1].Code != CodeGeneration {
		t.Fatalf("earlier diagnostic changed: %+v", items)
	}
	count := 0
	for _, item := range items {
		if item.Code == CodeGeneration {
			count++
		}
	}
	if count != 1 {
		t.Fatalf("shared 06a overflow diagnostics = %+v", items)
	}
}

// TestDeclarationFactsTaggedUnionVariantPublishesUnionType is a regression
// test for a real bug: a tagged-union variant's published symbol term used to
// stay as the variant's own payload type (member.Type) rather than being
// overridden to the declaring union type, unlike a plain enum variant (whose
// term IS correctly overridden a few lines above, in the exact same loop).
// A tagged-union variant construction's own value IS the union type, exactly
// like a plain enum variant's value IS the enum type — Choice.value(5) is a
// Choice, not an i32 — so without this override, prepareVariant's call-result
// constraint (compiler/internal/check/call_facts.go, which unifies the call's
// result with the variant symbol's own published term) unified the result
// with the payload type instead, and the full checker pipeline could not
// build ANY program that assigns a tagged-union construction to a
// union-typed destination: `var c Choice = Choice.value(5);` failed C0601,
// and even a top-level `let` failed C0616. Found while implementing the C
// backend's 10.34 (plain enum) slice, whose tagged-union rejection test had
// to hand-build its IR directly because this bug made the shape entirely
// unreachable from real source.
func TestDeclarationFactsTaggedUnionVariantPublishesUnionType(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Choice = union enum { empty void; value i32; };
fn main() i32 {
	var c Choice = Choice.value(5);
	return 0;
}
`)})
	result := Check(inputs, diagnostics, Config{})
	if !result.Successful() {
		t.Fatalf("tagged-union variant construction was rejected: %+v", diagnostics.Items())
	}
}
