package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func TestPlaceFactsAssignmentIndexSliceAndSingleEvaluation(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Box = struct { value i32; };
fn main(array [2]i32) void {
 var value i32 = 1;
 var box Box = Box.{ value = 1 };
 value = 2;
 value += 3;
 array[0] = value;
 box.value++;
 let sub []i32 = array[:1];
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	assignments, places, compound, slices, indexes := 0, 0, 0, 0, 0
	for _, retained := range facts.Generation.records.values {
		if retained.Assignment != nil {
			assignments++
			if retained.Assignment.Kind == assignmentCompound {
				compound++
			}
		}
		if retained.Place != nil {
			places++
			if len(retained.Place.Projections) == 0 {
				t.Fatal("empty place projection")
			}
		}
		if retained.Index != nil {
			if retained.Index.Mode == indexSlice {
				slices++
			} else {
				indexes++
			}
		}
	}
	if assignments != 4 || compound != 2 || places < 4 || slices != 1 || indexes != 1 {
		t.Fatalf("assignment/compound/place/slice/index=%d/%d/%d/%d/%d diagnostics=%+v", assignments, compound, places, slices, indexes, diagnostics.Items())
	}
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestPlaceFactsNonPlaceAndLimitRecovery(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn main() void { var value i32 = 1; let not_place = 1 + 2; value = 3; value = 4; }`)})
	facts := run06a3(inputs, diagnostics, Config{MaxTrackedPlaces: 1})
	if facts.Generation.counters.trackedPlaces != 1 || !diagnostics.HasErrors() {
		t.Fatalf("places=%d diagnostics=%+v", facts.Generation.counters.trackedPlaces, diagnostics.Items())
	}
	for ref := range facts.Walk.places {
		if facts.Walk.successfulExpressions[ref] {
			node, _ := facts.Walk.node(ref.Module, ref.Node)
			if node.Kind().String() == "BinaryExpr" {
				t.Fatal("operator result retained as place")
			}
		}
	}
}

func TestAssignmentFactsCompoundUsesOrdinarySlot(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn main() void { var value i32 = 1; value *= 2; }`)})
	facts := run06a3(inputs, diagnostics, Config{})
	found := false
	for _, retained := range facts.Generation.records.values {
		if retained.Assignment == nil || retained.Assignment.Kind != assignmentCompound {
			continue
		}
		for _, compatibility := range facts.Generation.records.values {
			if compatibility.Compatibility != nil && compatibility.Compatibility.Source != retained.Assignment.Source {
				if root, ok := facts.Generation.roots.root(compatibility.Compatibility.Source); ok && root.Kind == rootSlot && !root.Alternative.Guarded {
					found = true
				}
			}
		}
	}
	if !found || diagnostics.HasErrors() {
		t.Fatalf("compound slot=%v diagnostics=%+v", found, diagnostics.Items())
	}
}

func TestPlaceFactsDeferredRuntimeIndexIsGuarded(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Box = struct { values [2]i32; };
fn update(box Box) void { box.values[0] = 1; }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	guardedPlace, guardedMember := false, false
	for _, retained := range facts.Generation.records.values {
		if retained.Place != nil && retained.Header.Alternative.Guarded {
			selection, ok := solution.Selection(retained.Header.Alternative.Choice)
			guardedPlace = ok && selection == retained.Header.Alternative.Index
		}
		if retained.Member != nil && retained.Member.Kind == memberField && retained.Header.Alternative.Guarded && retained.Member.Name == "values" && retained.Member.NameSpan.End > retained.Member.NameSpan.Start {
			selection, ok := solution.Selection(retained.Header.Alternative.Choice)
			guardedMember = ok && selection == retained.Header.Alternative.Index
		}
	}
	if !guardedPlace || !guardedMember || !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("guarded place/member=%v/%v diagnostics=%+v", guardedPlace, guardedMember, diagnostics.Items())
	}
}

func TestPlaceFactsRetainsCompleteDeepProjection(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Box = struct { values [2]i32; };
fn update(pointer *Box) void { (*pointer).values[0] = 1; }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	want := []placeKind{placeStorage, placeDereference, placeField, placeIndex}
	found := false
	for _, retained := range facts.Generation.records.values {
		if retained.Place == nil || len(retained.Place.Projections) != len(want) {
			continue
		}
		found = true
		for i, projection := range retained.Place.Projections {
			if projection.Kind != want[i] {
				t.Fatalf("projection %d kind=%d want=%d", i, projection.Kind, want[i])
			}
		}
	}
	if solution := facts.Session.Solve(); !found || !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("found=%v diagnostics=%+v", found, diagnostics.Items())
	}
}

func TestIndexFactsAllBounds(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn main(array [3]i32) void {
 let a []i32 = array[:];
 let b []i32 = array[1:];
 let c []i32 = array[:2];
 let d []i32 = array[1:2];
 let e i32 = array[0];
 let ch char = "x"[0];
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	patterns := map[[2]bool]int{}
	known, indexed := 0, 0
	for _, retained := range facts.Generation.records.values {
		if retained.Index != nil {
			if retained.Index.Mode == indexSlice {
				patterns[[2]bool{retained.Index.StartPresent, retained.Index.EndPresent}]++
				if retained.Index.HasKnownArrayLength {
					known++
				}
			} else {
				indexed++
			}
		}
	}
	if len(patterns) != 4 || known != 4 || indexed != 2 {
		t.Fatalf("patterns=%+v known=%d indexed=%d diagnostics=%+v", patterns, known, indexed, diagnostics.Items())
	}
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestIndexFactsRetainBoundSyntaxAndConstants(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn main(array [3]i32, start i32) void {
 let sliced []i32 = array[1:2];
 let indexed i32 = array[0];
 let runtime []i32 = array[start:];
}
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	var sliced, indexed, runtime *indexRecord
	for _, retained := range handoff.Records.Records() {
		if retained.Index == nil {
			continue
		}
		switch {
		case retained.Index.Mode == indexSlice && retained.Index.EndPresent:
			sliced = retained.Index
		case retained.Index.Mode == indexValue:
			indexed = retained.Index
		case retained.Index.Mode == indexSlice && retained.Index.StartPresent:
			runtime = retained.Index
		}
	}
	if sliced == nil || indexed == nil || runtime == nil {
		t.Fatalf("missing index records: sliced=%v indexed=%v runtime=%v", sliced != nil, indexed != nil, runtime != nil)
	}
	if sliced.StartSyntax == (symbol.SyntaxRef{}) || sliced.EndSyntax == (symbol.SyntaxRef{}) || indexed.StartSyntax == (symbol.SyntaxRef{}) {
		t.Fatalf("missing bound syntax refs: sliced=%+v indexed=%+v", sliced, indexed)
	}
	for _, bound := range []struct {
		ref  symbol.SyntaxRef
		want string
	}{
		{sliced.StartSyntax, "1"},
		{sliced.EndSyntax, "2"},
		{indexed.StartSyntax, "0"},
	} {
		item, exists := inputs.Graph.Module(bound.ref.Module)
		if !exists {
			t.Fatalf("missing module for bound ref %+v", bound.ref)
		}
		boundNode, exists := item.Tree.Node(bound.ref.Node)
		file, fileExists := inputs.Sources.File(boundNode.Span().Source)
		if !exists || !fileExists || string(file.Slice(boundNode.Span())) != bound.want {
			t.Fatalf("bound ref %+v does not span %q", bound.ref, bound.want)
		}
		result, found := records.Constant(bound.ref)
		if !found || result.State != constantKnown || result.Value.Integer.String() != bound.want {
			t.Fatalf("constant for %+v = %+v, found=%v, want %s", bound.ref, result, found, bound.want)
		}
	}
	if runtime.StartSyntax == (symbol.SyntaxRef{}) {
		t.Fatal("runtime bound syntax ref missing")
	}
	result, found := records.Constant(runtime.StartSyntax)
	if !found || result.State == constantKnown {
		t.Fatalf("runtime constant = %+v, found=%v", result, found)
	}
}

func TestIndexFactsRetainsExactEscapeDestination(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn trim(values [4]i32) void { let tail []i32 = values[1:]; }`)})
	facts := run06a3(inputs, diagnostics, Config{})
	found := false
	for _, retained := range facts.Generation.records.values {
		if retained.Index == nil || retained.Index.Mode != indexSlice {
			continue
		}
		resolved, ok := inputs.Resolution.Symbols.Symbol(retained.Index.EscapeDestination)
		if !ok || resolved.Name != "tail" {
			t.Fatalf("escape destination=%d symbol=%+v", retained.Index.EscapeDestination, resolved)
		}
		found = true
	}
	if solution := facts.Session.Solve(); !found || !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("found=%v diagnostics=%+v", found, diagnostics.Items())
	}
}

func TestIndexFactsDelayedBasesResolve(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let indexed i32 = values[0];
let sliced []i32 = values[1:];
let values [3]i32;
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	indexes, slices := 0, 0
	for _, retained := range facts.Generation.records.values {
		if retained.Index == nil {
			continue
		}
		if retained.Index.Mode == indexSlice {
			slices++
		} else {
			indexes++
		}
	}
	if solution := facts.Session.Solve(); indexes != 1 || slices != 1 || !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("indexes/slices=%d/%d diagnostics=%+v", indexes, slices, diagnostics.Items())
	}
}

func TestIndexFactsErroneousBaseSuppressionAndIndependentRecovery(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let bad_index = missing[0];
let bad_slice = missing[:];
let valid_index i32 = [1, 2][0];
let valid_slice []i32 = [1, 2][1:];
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	validRecords := 0
	for _, retained := range facts.Generation.records.values {
		if retained.Index != nil {
			validRecords++
		}
	}
	failed, successful := 0, 0
	for _, ref := range facts.Walk.order {
		node, _ := facts.Walk.node(ref.Module, ref.Node)
		if node.Kind() != syntax.BracketApply && node.Kind() != syntax.SliceExpr {
			continue
		}
		if facts.Walk.successfulExpressions[ref] {
			successful++
		} else {
			failed++
			if facts.Walk.publishedSyntax[ref] {
				t.Fatalf("failed index/slice published syntax: %+v", ref)
			}
		}
	}
	solution := facts.Session.Solve()
	if validRecords != 2 || failed != 2 || successful != 2 || !diagnostics.HasErrors() {
		t.Fatalf("records/failed/successful=%d/%d/%d solution=%v diagnostics=%+v", validRecords, failed, successful, solution.Successful(), diagnostics.Items())
	}
}

func TestPlaceFactsNamedStringIndexIsNotPlace(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`fn read(text str) char { return text[0]; }`)})
	facts := run06a3(inputs, diagnostics, Config{})
	for ref, candidate := range facts.Walk.places {
		node, _ := facts.Walk.node(ref.Module, ref.Node)
		if node.Kind() == syntax.BracketApply && candidate.value != 0 {
			t.Fatalf("named string index retained as place: %+v", candidate)
		}
	}
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}
