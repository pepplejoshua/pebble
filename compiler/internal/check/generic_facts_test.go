package check

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func deferredChoiceHarness(t *testing.T, config Config) (*walker, symbol.SyntaxRef, *bracketPlan, *diagnostic.DiagnosticSet) {
	t.Helper()
	w, bindings, diagnostics := newPublicationWalkerWithConfig(t, config)
	ref := rootRef(t, w.generation.inputs)
	origin := infer.Origin{Syntax: ref}
	leftTerm, rightTerm := w.session.Variable(origin), w.session.Variable(origin)
	leftID, leftOK := w.generation.addValue(generatedValue{Term: leftTerm, Origin: origin})
	rightID, rightOK := w.generation.addValue(generatedValue{Term: rightTerm, Origin: origin})
	if !leftOK || !rightOK || len(bindings) == 0 {
		t.Fatal("could not construct deferred-choice harness")
	}
	left := typedValue{ID: leftID, Term: leftTerm}
	right := typedValue{ID: rightID, Term: rightTerm}
	p := &bracketPlan{
		genericBranch: &branchFacts{index: 0, constraints: []infer.Constraint{infer.Equal(leftTerm, leftTerm, origin)}, roots: []branchRoot{{value: left}}, rooted: map[valueID]bool{leftID: true}},
		runtimeBranch: &branchFacts{index: 1, constraints: []infer.Constraint{infer.Equal(rightTerm, rightTerm, origin)}, roots: []branchRoot{{value: right}}, rooted: map[valueID]bool{rightID: true}},
	}
	return w, ref, p, diagnostics
}

func checkerPublicationState(w *walker) [4]uint64 {
	return [4]uint64{uint64(len(w.generation.roots.values)), uint64(len(w.generation.records.values)), w.generation.records.components, uint64(w.generation.counters.genericRequirements)}
}

func requireSingleDiagnosticCode(t *testing.T, diagnostics *diagnostic.DiagnosticSet, code diagnostic.Code) {
	t.Helper()
	items := diagnostics.Items()
	if len(items) != 1 || items[0].Code != code {
		t.Fatalf("diagnostics=%+v, want sole %s", items, code)
	}
}

func TestDeferredBracketCheckerPreflightIsAtomic(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*walker, symbol.SyntaxRef, *bracketPlan)
	}{
		{"requirements", func(w *walker, _ symbol.SyntaxRef, p *bracketPlan) {
			w.generation.config.MaxGenericRequirements = w.generation.counters.genericRequirements
			p.genericBranch.requirements = 1
		}},
		{"roots", func(_ *walker, _ symbol.SyntaxRef, p *bracketPlan) {
			p.runtimeBranch.roots[0] = p.genericBranch.roots[0]
		}},
		{"records", func(w *walker, ref symbol.SyntaxRef, p *bracketPlan) {
			w.generation.config.MaxSemanticRecords = uint32(len(w.generation.records.values))
			header := w.header(ref, 0, false)
			value := typeUseRecord{Header: header, Kind: typeUseExplicitArgument, Type: p.genericBranch.roots[0].value.ID}
			p.genericBranch.records = append(p.genericBranch.records, pendingBranchRecord{local: 1, value: retainedRecord{Header: header, TypeUse: &value}})
		}},
		{"join", func(w *walker, ref symbol.SyntaxRef, p *bracketPlan) {
			header := w.header(ref, 0, false)
			expression := expressionRecord{Header: header, Kind: expressionBracket, Result: p.genericBranch.roots[0].value.ID, Specialized: 2}
			p.genericBranch.records = append(p.genericBranch.records, pendingBranchRecord{local: 1, value: retainedRecord{Header: header, Expression: &expression}})
		}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			w, ref, p, diagnostics := deferredChoiceHarness(t, Config{})
			test.mutate(w, ref, p)
			before := checkerPublicationState(w)
			if w.finalizeDeferredChoice(ref, walkContext{}, p) {
				t.Fatal("invalid deferred branch published")
			}
			if after := checkerPublicationState(w); after != before {
				t.Fatalf("checker state changed: before=%v after=%v", before, after)
			}
			if w.session.Fatal() {
				t.Fatal("checker preflight failure mutated inference")
			}
			requireSingleDiagnosticCode(t, diagnostics, CodeGeneration)
			if test.name == "join" {
				origin := infer.Origin{Syntax: ref}
				if value, ok := w.newSlotValue(w.session.Variable(origin), origin); !ok || value.ID == 0 {
					t.Fatal("independent publication did not recover after checker preflight failure")
				}
			}
		})
	}
}

func TestDeferredBracketPreflightRejectsForeignIdentitiesAndBranchOrder(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*walker, symbol.SyntaxRef, *bracketPlan)
	}{
		{"foreign site", func(w *walker, ref symbol.SyntaxRef, p *bracketPlan) {
			foreign := ref
			foreign.Node = ^syntax.NodeID(0)
			p.genericBranch.instantiations = append(p.genericBranch.instantiations, branchInstantiation{site: foreign, generic: w.generation.inputs.Resolution.Symbols.All()[0].ID})
		}},
		{"foreign generic", func(_ *walker, ref symbol.SyntaxRef, p *bracketPlan) {
			p.genericBranch.instantiations = append(p.genericBranch.instantiations, branchInstantiation{site: ref, generic: symbol.SymbolID(^uint32(0))})
		}},
		{"duplicate branch", func(_ *walker, _ symbol.SyntaxRef, p *bracketPlan) {
			p.runtimeBranch.index = 0
		}},
		{"reversed branches", func(_ *walker, _ symbol.SyntaxRef, p *bracketPlan) {
			p.genericBranch.index, p.runtimeBranch.index = 1, 0
		}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			w, ref, p, diagnostics := deferredChoiceHarness(t, Config{Inference: infer.Config{MaxChoices: 1}})
			test.mutate(w, ref, p)
			before := checkerPublicationState(w)
			if w.finalizeDeferredChoice(ref, walkContext{}, p) {
				t.Fatal("malformed deferred branch published")
			}
			if after := checkerPublicationState(w); after != before || w.session.Fatal() {
				t.Fatalf("preflight mutated state: before=%v after=%v fatal=%v", before, after, w.session.Fatal())
			}
			// With a one-choice limit this succeeds only if preflight did not add
			// the deferred choice to inference.
			choice, _ := w.session.AddChoice(infer.OneOf([]infer.Alternative{
				{Label: "left", Constraints: p.genericBranch.constraints},
				{Label: "right", Constraints: p.runtimeBranch.constraints},
			}, infer.Origin{Syntax: ref}))
			if choice == 0 || w.session.Fatal() {
				t.Fatal("checker preflight mutated inference choice state")
			}
			requireSingleDiagnosticCode(t, diagnostics, CodeGeneration)
		})
	}
}

func TestDeferredBracketInferenceFailureCommitsNoCheckerState(t *testing.T) {
	tests := []struct {
		name   string
		config Config
		mutate func(*walker, symbol.SyntaxRef, *bracketPlan)
	}{
		{"choice", Config{Inference: infer.Config{MaxConstraints: 1, MaxDiagnostics: 1}}, nil},
		{"slot", Config{Inference: infer.Config{MaxSolvedSlots: 1, MaxDiagnostics: 1}}, nil},
		{"instantiation", Config{Inference: infer.Config{MaxDiagnostics: 1}}, func(w *walker, ref symbol.SyntaxRef, p *bracketPlan) {
			generic := w.generation.inputs.Resolution.Symbols.All()[0].ID
			p.genericBranch.instantiations = append(p.genericBranch.instantiations, branchInstantiation{site: ref, generic: generic})
			p.runtimeBranch.instantiations = append(p.runtimeBranch.instantiations, branchInstantiation{site: ref, generic: generic})
		}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			w, ref, p, diagnostics := deferredChoiceHarness(t, test.config)
			if test.mutate != nil {
				test.mutate(w, ref, p)
			}
			before := checkerPublicationState(w)
			if w.finalizeDeferredChoice(ref, walkContext{}, p) {
				t.Fatal("fatal inference publication succeeded")
			}
			if after := checkerPublicationState(w); after != before {
				t.Fatalf("checker state changed: before=%v after=%v", before, after)
			}
			if !w.session.Fatal() {
				t.Fatal("inference rejection was not fatal")
			}
			solution := w.session.Solve()
			if solution.Successful() {
				t.Fatal("fatal deferred choice solved successfully")
			}
			if slots := solution.Slots(); len(slots) != 0 {
				t.Fatalf("fatal choice exposed guarded slots: %+v", slots)
			}
			for _, branch := range []*branchFacts{p.genericBranch, p.runtimeBranch} {
				for _, instantiation := range branch.instantiations {
					if _, visible := solution.Instantiation(instantiation.site); visible {
						t.Fatalf("fatal choice exposed guarded instantiation at %+v", instantiation.site)
					}
				}
			}
			for _, rooted := range append(p.genericBranch.roots, p.runtimeBranch.roots...) {
				if root, ok := w.generation.roots.root(rooted.value.ID); ok && root.Alternative.Guarded {
					if _, visible := solution.Slot(root.Slot); visible {
						t.Fatal("fatal choice exposed guarded slot")
					}
				}
			}
			requireSingleDiagnosticCode(t, diagnostics, infer.CodeResourceLimit)
		})
	}
}

func TestDeferredBracketAtomicCommitAndIndependentRecovery(t *testing.T) {
	w, ref, p, diagnostics := deferredChoiceHarness(t, Config{})
	header := w.header(ref, 0, false)
	member := memberRecord{Header: header, Kind: memberField, Base: p.genericBranch.roots[0].value.ID, Result: p.genericBranch.roots[0].value.ID, Name: "field", NameSpan: header.Span}
	p.genericBranch.records = append(p.genericBranch.records, pendingBranchRecord{local: 1, value: retainedRecord{Header: header, Member: &member}})
	expression := expressionRecord{Header: header, Kind: expressionBracket, Result: p.genericBranch.roots[0].value.ID, Specialized: 1}
	p.genericBranch.records = append(p.genericBranch.records, pendingBranchRecord{local: 2, value: retainedRecord{Header: header, Expression: &expression}})
	p.genericBranch.requirements = 1
	before := checkerPublicationState(w)
	if !w.finalizeDeferredChoice(ref, walkContext{}, p) {
		t.Fatalf("valid deferred choice failed: %+v", diagnostics.Items())
	}
	after := checkerPublicationState(w)
	if after[0] != before[0]+2 || after[1] != before[1]+2 || after[3] != before[3]+1 {
		t.Fatalf("atomic commit before=%v after=%v", before, after)
	}
	last := w.generation.records.values[len(w.generation.records.values)-1]
	if last.Expression == nil || last.Expression.Specialized != recordID(len(w.generation.records.values)-1) || !last.Header.Alternative.Guarded {
		t.Fatalf("deterministic specialized remap failed: %+v", last)
	}
	if diagnostics.HasErrors() {
		t.Fatalf("unexpected diagnostics=%+v", diagnostics.Items())
	}
}

func TestDeferredBracketCompleteRuntimeTraversalAndGuardedInstantiation(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn identity[T](value T) T => value;
type Box = struct { text str; };
let box Box = Box.{ text = "xy" };
let index int = 0;
let binary = box.text[index + 1];
let nested char = box.text[identity[int](0)];
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("generation diagnostics=%+v", diagnostics.Items())
	}
	binaryFound := false
	for _, retained := range facts.Generation.records.values {
		if retained.Expression == nil || retained.Expression.Kind != expressionBinary {
			continue
		}
		if !retained.Header.Alternative.Guarded || retained.Header.Alternative.Index != 1 || len(retained.Expression.Children) != 2 {
			t.Fatalf("binary record=%+v", retained.Expression)
		}
		left := facts.Generation.values[retained.Expression.Children[0]-1].Origin.Span
		right := facts.Generation.values[retained.Expression.Children[1]-1].Origin.Span
		if left.Start >= right.Start {
			t.Fatalf("binary child order=%+v then %+v", left, right)
		}
		binaryFound = true
	}
	if !binaryFound {
		t.Fatal("deferred runtime branch omitted the authored binary expression")
	}
	nestedInputs, nestedDiagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn identity[T](value T) T => value;
type Box = struct { text str; };
let box Box = Box.{ text = "xy" };
let nested char = box.text[identity[int](0)];
`)})
	nestedFacts := run06a3(nestedInputs, nestedDiagnostics, Config{})
	solution := nestedFacts.Session.Solve()
	if !solution.Successful() || nestedDiagnostics.HasErrors() {
		t.Fatalf("solve diagnostics=%+v", nestedDiagnostics.Items())
	}
	guardedInstantiation := false
	for _, ref := range nestedFacts.Walk.order {
		if _, ok := solution.Instantiation(ref); ok {
			guardedInstantiation = true
		}
	}
	if !guardedInstantiation {
		t.Fatal("selected runtime branch omitted its guarded instantiation")
	}
}

func TestDeferredBracketFailedRuntimeBranchIsInvisible(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn identity[T](value T) T => value;
type Box = struct { number i32; };
let box Box = Box.{ number = 1 };
let failed char = box.number[identity[i32](0)];
let independent i32 = identity[i32](1);
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	instantiations := 0
	for _, ref := range facts.Walk.order {
		if _, ok := solution.Instantiation(ref); ok {
			instantiations++
		}
	}
	if instantiations != 1 {
		t.Fatalf("visible instantiations=%d diagnostics=%+v", instantiations, diagnostics.Items())
	}
	for _, root := range facts.Generation.roots.values {
		if !root.Root.Alternative.Guarded {
			continue
		}
		if _, ok := solution.Slot(root.Root.Slot); ok {
			t.Fatalf("failed choice exposed guarded slot %+v", root.Root)
		}
	}
	for _, item := range diagnostics.Items() {
		if item.Code == infer.CodeUnresolved {
			t.Fatalf("failed choice produced T0510 cascade: %+v", diagnostics.Items())
		}
	}
	if items := diagnostics.Items(); len(items) != 1 || items[0].Code != infer.CodeInvalidType {
		t.Fatalf("failed alternatives leaked branch diagnostics: %+v", items)
	}
	if solution.Successful() || !diagnostics.HasErrors() {
		t.Fatalf("failed choice unexpectedly succeeded: %+v", diagnostics.Items())
	}
}

func TestDeferredGenericBranchPublishesNestedTypeInstantiation(t *testing.T) {
	contents, err := os.ReadFile("../../../tests/check/facts/valid/generic_method_nested_type.peb")
	if err != nil {
		t.Fatal(err)
	}
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
	var wrapper symbol.SymbolID
	for _, candidate := range inputs.Resolution.Symbols.All() {
		if candidate.Name == "Wrapper" && candidate.Kind == symbol.SymbolType {
			wrapper = candidate.ID
			break
		}
	}
	if wrapper == 0 {
		t.Fatal("missing Wrapper identity")
	}
	guarded := 0
	for _, ref := range facts.Walk.order {
		instantiation, ok := solution.Instantiation(ref)
		if !ok || instantiation.Generic != wrapper {
			continue
		}
		if len(instantiation.Arguments) != 1 || instantiation.Arguments[0].Type != inputs.Types.Builtins().I32 {
			t.Fatalf("nested type instantiation=%+v", instantiation)
		}
		guarded++
	}
	if guarded == 0 {
		t.Fatal("selected generic alternative omitted nested type instantiation")
	}
}

func TestGenericFactsExplicitInferredAndStandalone(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn identity[T](value T) T => value;
let inferred i32 = identity(1);
let explicit i32 = identity[i32](2);
let standalone fn(i32) i32 = identity[i32];
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
	count := 0
	for _, ref := range facts.Walk.order {
		if value, ok := solution.Instantiation(ref); ok {
			count++
			if len(value.Arguments) != 1 || value.Arguments[0].Type != inputs.Types.Builtins().I32 {
				t.Fatalf("instantiation=%+v", value)
			}
		}
	}
	if count != 3 {
		t.Fatalf("instantiations=%d", count)
	}
}

func TestCallGenericBracketLoweredLimitsRemainBounded(t *testing.T) {
	contents := []byte(`
fn identity[T](value T) T => value;
fn invoke[T](callee T) void { callee(); callee(); }
type Box = struct { text str; };
let box Box = Box.{ text = "x" };
let index int = 0;
let first i32 = identity(1);
let second char = box.text[index];
let third i32 = identity(3);
let fourth char = box.text[index];
`)
	configs := []Config{
		{Inference: infer.Config{MaxInferVariables: 1, MaxDiagnostics: 1}, MaxDiagnostics: 1},
		{Inference: infer.Config{MaxConstraints: 1, MaxDiagnostics: 1}, MaxDiagnostics: 1},
		{Inference: infer.Config{MaxSolvedSlots: 1, MaxDiagnostics: 1}, MaxDiagnostics: 1},
		{Inference: infer.Config{MaxChoices: 1, MaxDiagnostics: 1}, MaxDiagnostics: 1},
		{MaxSemanticRecords: 1, MaxRecordComponents: 2, MaxDiagnostics: 1},
		{MaxGenericRequirements: 1, MaxDiagnostics: 1},
	}
	for i, config := range configs {
		t.Run(string(rune('a'+i)), func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
			facts := run06a3(inputs, diagnostics, config)
			facts.Session.Solve()
			if !diagnostics.HasErrors() {
				t.Fatal("lowered limit produced no diagnostic")
			}
			seenEOF := false
			for _, ref := range facts.Walk.order {
				node, _ := facts.Walk.node(ref.Module, ref.Node)
				seenEOF = seenEOF || node.Kind() == syntax.EndOfFile
			}
			if !seenEOF {
				t.Fatal("limit recovery stopped authored traversal")
			}
		})
	}
}

func TestBracketFactsValueAndDeferredIsolation(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Box = struct { text str; };
let box Box = Box.{ text = "x" };
let index int = 0;
let value char = box.text[index];
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
	choices, guarded := 0, 0
	for _, root := range facts.Generation.roots.values {
		if root.Root.Alternative.Guarded {
			guarded++
			selected, selectedOK := solution.Selection(root.Root.Alternative.Choice)
			_, slotOK := solution.Slot(root.Root.Slot)
			if !selectedOK || slotOK != (selected == root.Root.Alternative.Index) {
				t.Fatalf("guarded slot selection=%d/%v tag=%+v slot=%v", selected, selectedOK, root.Root.Alternative, slotOK)
			}
		}
	}
	for _, retained := range facts.Generation.records.values {
		if retained.Expression != nil && retained.Expression.Kind == expressionBracket {
			if retained.Header.Alternative.Guarded {
				if retained.Expression.Specialized == 0 {
					t.Fatal("deferred bracket expression lacks specialized join")
				}
				choices++
			}
		}
	}
	if choices == 0 || guarded == 0 {
		t.Fatalf("choices/guarded=%d/%d", choices, guarded)
	}
}

func TestCallGenericBracketFactFixtures(t *testing.T) {
	patterns := []string{"../../../tests/check/facts/valid/call_*.peb", "../../../tests/check/facts/valid/generic_*.peb", "../../../tests/check/facts/valid/bracket_*.peb"}
	for _, pattern := range patterns {
		paths, err := filepath.Glob(pattern)
		if err != nil {
			t.Fatal(err)
		}
		for _, path := range paths {
			path := path
			t.Run(filepath.Base(path), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
				facts := run06a3(inputs, diagnostics, Config{})
				solution := facts.Session.Solve()
				if !solution.Successful() || diagnostics.HasErrors() {
					t.Fatalf("diagnostics=%+v", diagnostics.Items())
				}
			})
		}
	}
}
