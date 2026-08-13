package check

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func TestExpressionFactsPrimaryPayloadsAndAuthoredChildren(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let values = (0x2a, `a{1}b`, some 'x');\n")})
	facts := run06a3(inputs, diagnostics, Config{})
	var tuple, interpolation *expressionRecord
	var integer *expressionRecord
	for _, retained := range facts.Generation.records.values {
		if retained.Expression == nil {
			continue
		}
		switch retained.Expression.Kind {
		case expressionTuple:
			tuple = retained.Expression
		case expressionInterpolated:
			interpolation = retained.Expression
		case expressionLiteral:
			if retained.Expression.Literal.Kind == literalInteger && bytes.Equal(retained.Expression.Literal.NumericBytes, []byte("0x2a")) {
				integer = retained.Expression
			}
		}
	}
	if tuple == nil || len(tuple.Children) != 3 || interpolation == nil || integer == nil {
		t.Fatalf("primary records tuple=%+v interpolation=%+v integer=%+v", tuple, interpolation, integer)
	}
	if len(interpolation.Parts) != 3 || interpolation.Parts[0].Text != "a" || interpolation.Parts[1].Value == 0 || interpolation.Parts[2].Text != "b" {
		t.Fatalf("interpolation parts = %+v", interpolation.Parts)
	}
	clone := cloneExpressionRecord(*integer)
	clone.Literal.NumericBytes[0] = '9'
	if bytes.Equal(clone.Literal.NumericBytes, integer.Literal.NumericBytes) {
		t.Fatal("numeric literal payload was not copied defensively")
	}
	if diagnostics.HasErrors() {
		t.Fatalf("primary expression diagnostics: %+v", diagnostics.Items())
	}
}

func TestAggregateFactsSlotsSpecializedJoinsAndMemoizedCount(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct { x i32; y i32; };
let point Point = Point.{ x = 1, y = 2 };
let repeated [3]i32 = [1; 3];
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	var aggregate *aggregateRecord
	var recordExpression *expressionRecord
	for _, retained := range facts.Generation.records.values {
		if retained.Aggregate != nil && retained.Aggregate.Kind == aggregateStruct {
			aggregate = retained.Aggregate
		}
		if retained.Expression != nil && retained.Expression.Kind == expressionRecordValue {
			recordExpression = retained.Expression
		}
	}
	if aggregate == nil || recordExpression == nil || recordExpression.Specialized != aggregate.Header.ID || len(aggregate.Fields) != 2 || len(recordExpression.Children) != 2 {
		t.Fatalf("aggregate join = aggregate %+v expression %+v", aggregate, recordExpression)
	}
	for _, field := range aggregate.Fields {
		root, ok := facts.Generation.roots.root(field.Destination)
		if !ok || root.Kind != rootSlot || root.Alternative.Guarded {
			t.Fatalf("field destination %d root = %+v, ok=%v", field.Destination, root, ok)
		}
	}
	if diagnostics.HasErrors() {
		t.Fatalf("aggregate diagnostics: %+v", diagnostics.Items())
	}
}

func TestExpressionFactsSizeofAndPartialMemberRetainPolicyEvidence(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Color = enum { red, blue };
let color Color = .red;
let width uint = sizeof Color;
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	partial, sizeofUse := false, false
	for _, retained := range facts.Generation.records.values {
		if retained.Expression != nil && retained.Expression.Kind == expressionPartialMember {
			partial = retained.Expression.Specialized != 0
		}
		if retained.TypeUse != nil && retained.TypeUse.Kind == typeUseSizeof {
			sizeofUse = true
		}
	}
	if !partial || !sizeofUse {
		t.Fatalf("partial/sizeof policy evidence = %v/%v", partial, sizeofUse)
	}
	if diagnostics.HasErrors() {
		t.Fatalf("partial/sizeof diagnostics: %+v", diagnostics.Items())
	}
}

func TestAggregateFactsUseOnlyResolutionMemberIdentity(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct { x i32; };
type Color = enum { red, blue };
let explicit Point = Point.{ x = 1 };
let inferred Point = .{ x = 2 };
let color Color = .red;
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	var explicit, inferred *aggregateRecord
	partialMember := false
	for _, retained := range facts.Generation.records.values {
		if retained.Aggregate == nil || len(retained.Aggregate.Fields) != 1 {
			continue
		}
		field := retained.Aggregate.Fields[0]
		switch {
		case retained.Aggregate.Kind == aggregateStruct && field.Name == "x":
			if explicit == nil {
				explicit = retained.Aggregate
			} else {
				inferred = retained.Aggregate
			}
		case retained.Aggregate.Kind == aggregateEnumVariant && field.Name == "red":
			partialMember = field.Member == 0 && field.Destination != 0 && retained.Aggregate.Receiver != 0
		}
	}
	if explicit == nil || inferred == nil || explicit.Header.Span.Start > inferred.Header.Span.Start {
		t.Fatalf("struct aggregate evidence = explicit %+v inferred %+v", explicit, inferred)
	}
	// The named form resolves its member symbol through the resolver; the
	// anonymous form (no base-type name node) now resolves it by name against
	// the destination declaration grounded from the contextual expected type,
	// so BOTH carry a resolved member identity at walk time.
	if explicit.Fields[0].Member == 0 {
		t.Fatalf("named form field member unresolved: %+v", explicit.Fields[0])
	}
	if inferred.Fields[0].Member == 0 || inferred.Declaration == 0 || inferred.Receiver == 0 {
		t.Fatalf("anonymous form field member/receiver/declaration unresolved: %+v", inferred)
	}
	if !partialMember {
		t.Fatal("partial member identity not deferred")
	}
	if diagnostics.HasErrors() {
		t.Fatalf("member evidence diagnostics: %+v", diagnostics.Items())
	}
}

func TestExpressionFactsExcludeFailedAndUnsupportedExpressions(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn generic[T](value T) T => value;
let unresolved = missing;
let unapplied = generic;
let valid = fn() i32 => 1;
fn outer(local i32) void {
    let captured = fn() i32 => local;
    let unsupported = fn[T](value T) T => value;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	functionExpressions, capturedRecords, unsupportedRecords := 0, 0, 0
	failedRefs := make(map[symbol.SyntaxRef]bool)
	for _, ref := range facts.Walk.order {
		node, _ := facts.Walk.node(ref.Module, ref.Node)
		if node.Kind() != syntax.Name {
			continue
		}
		file, _ := inputs.Sources.File(node.Span().Source)
		spelling := string(file.Slice(node.Span()))
		if spelling == "missing" || spelling == "generic" {
			if resolution, ok := inputs.Resolution.Reference(ref); ok && (resolution.State != symbol.ResolutionResolved || spelling == "generic") {
				failedRefs[ref] = true
			}
		}
	}
	for _, retained := range facts.Generation.records.values {
		if retained.Expression != nil {
			if retained.Expression.Kind == expressionFunction {
				functionExpressions++
			}
			if failedRefs[retained.Expression.Header.Syntax] {
				t.Fatalf("failed expression received expression record: %+v", retained.Expression)
			}
		}
		if retained.Callable != nil && retained.Callable.Kind == callableLiteral && len(retained.Callable.Captures) != 0 {
			capturedRecords++
		}
		if retained.UnsupportedCallable != nil {
			unsupportedRecords++
		}
	}
	for ref := range failedRefs {
		if facts.Walk.publishedSyntax[ref] {
			t.Fatalf("failed name/path received successful syntax root: %+v", ref)
		}
		value := facts.Walk.valuesBySyntax[ref]
		root, rooted := facts.Generation.roots.root(value.ID)
		if value.ID == 0 || !rooted || root.Kind != rootSlot || root.Alternative.Guarded {
			t.Fatalf("failed expression recovery value/root = %+v/%+v, rooted=%v", value, root, rooted)
		}
	}
	if len(failedRefs) != 2 || functionExpressions != 1 || capturedRecords != 1 || unsupportedRecords != 1 {
		t.Fatalf("failed=%d function expressions=%d captures=%d unsupported=%d", len(failedRefs), functionExpressions, capturedRecords, unsupportedRecords)
	}
}

func TestExpressionFactsFailedReservedParentsUseSlotRoots(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn generic[T](value T) T => value;
let failed_name i32 = missing;
let failed_generic i32 = generic;
let failed_parent (i32, i32) = (missing_again, 1);
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	checked := 0
	for _, retained := range facts.Generation.records.values {
		if retained.Binding == nil || !retained.Binding.InitializerPresent {
			continue
		}
		binding, _ := inputs.Resolution.Symbols.Symbol(retained.Binding.Symbol)
		if binding.Name != "failed_name" && binding.Name != "failed_generic" && binding.Name != "failed_parent" {
			continue
		}
		root, rooted := facts.Generation.roots.root(retained.Binding.Initializer)
		if !rooted || root.Kind != rootSlot || root.Alternative.Guarded {
			t.Fatalf("%s initializer root = %+v, rooted=%v", binding.Name, root, rooted)
		}
		initializerRef := facts.Generation.values[retained.Binding.Initializer-1].Origin.Syntax
		if facts.Walk.publishedSyntax[initializerRef] {
			t.Fatalf("%s recovery published syntax root", binding.Name)
		}
		checked++
	}
	for _, retained := range facts.Generation.records.values {
		if retained.Compatibility == nil {
			continue
		}
		for _, id := range []valueID{retained.Compatibility.Source, retained.Compatibility.Destination} {
			if _, rooted := facts.Generation.roots.root(id); !rooted {
				t.Fatalf("compatibility value %d is not root-resolvable", id)
			}
		}
	}
	if checked != 3 {
		t.Fatalf("checked failed bindings = %d, want 3", checked)
	}
}

func TestExpressionFactsSomeCompatibilityRequiresDestination(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let source i32 = 1;
let annotated ?i32 = some source;
let inferred = some source;
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	annotated, inferred := 0, 0
	bindingByInitializer := make(map[symbol.SyntaxRef]string)
	for _, retained := range facts.Generation.records.values {
		if retained.Binding == nil || !retained.Binding.InitializerPresent {
			continue
		}
		binding, _ := inputs.Resolution.Symbols.Symbol(retained.Binding.Symbol)
		bindingByInitializer[facts.Generation.values[retained.Binding.Initializer-1].Origin.Syntax] = binding.Name
	}
	for _, retained := range facts.Generation.records.values {
		if retained.Compatibility == nil || retained.Compatibility.Role != compatibilityOptionalInjection {
			continue
		}
		node, _ := facts.Walk.node(retained.Header.Syntax.Module, retained.Header.Syntax.Node)
		if node.Kind() != syntax.SomeExpr {
			continue
		}
		bindingName := bindingByInitializer[retained.Header.Syntax]
		if bindingName == "annotated" {
			annotated++
		} else {
			inferred++
		}
	}
	if annotated != 1 || inferred != 0 {
		t.Fatalf("annotated/inferred some compatibility = %d/%d", annotated, inferred)
	}
}

func TestAggregateFactsFailedChildDoesNotPublishRecordResult(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct { x i32; };
let point Point = Point.{ x = missing };
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	var binding *bindingRecord
	var aggregate *aggregateRecord
	expressionCount := 0
	for _, retained := range facts.Generation.records.values {
		if retained.Binding != nil && retained.Binding.InitializerPresent {
			binding = retained.Binding
		}
		if retained.Aggregate != nil {
			aggregate = retained.Aggregate
		}
		if retained.Expression != nil && retained.Expression.Kind == expressionRecordValue {
			expressionCount++
		}
	}
	if binding == nil || aggregate == nil || !aggregate.Header.Suppressed || aggregate.Result != binding.Initializer || expressionCount != 0 {
		t.Fatalf("failed record recovery binding=%+v aggregate=%+v expressions=%d", binding, aggregate, expressionCount)
	}
	root, rooted := facts.Generation.roots.root(binding.Initializer)
	if !rooted || root.Kind != rootSlot || root.Alternative.Guarded || facts.Walk.publishedSyntax[facts.Generation.values[binding.Initializer-1].Origin.Syntax] {
		t.Fatalf("failed record result root=%+v rooted=%v", root, rooted)
	}
	for _, field := range aggregate.Fields {
		for _, id := range []valueID{field.Value, field.Destination} {
			if _, ok := facts.Generation.roots.root(id); !ok {
				t.Fatalf("failed record field value %d is not rooted", id)
			}
		}
	}
}

func TestExpressionFactsPlaceLimitDoesNotPreventLaterPublication(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
let first i32 = 1;
let second i32 = first;
let third i32 = second;
`)})
	facts := run06a3(inputs, diagnostics, Config{MaxTrackedPlaces: 1})
	if len(facts.Walk.placeCandidates) != 1 {
		t.Fatalf("tracked places = %d, want bounded 1", len(facts.Walk.placeCandidates))
	}
	publishedNames := 0
	for ref := range facts.Walk.publishedSyntax {
		node, _ := facts.Walk.node(ref.Module, ref.Node)
		if node.Kind() == syntax.Name {
			publishedNames++
		}
	}
	if publishedNames != 2 {
		t.Fatalf("name publications after place limit = %d, want 2", publishedNames)
	}
}

func TestAggregateFactsRecordLimitFailureIsAtomicAndLaterExpressionRecovers(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("let first = (1, 2);\nlet later = 3;\n")})
	facts := run06a3(inputs, diagnostics, Config{MaxSemanticRecords: 2, MaxRecordComponents: 2})
	seenLater := false
	for _, retained := range facts.Generation.records.values {
		if retained.Expression != nil && retained.Expression.Literal.Kind == literalInteger && bytes.Equal(retained.Expression.Literal.NumericBytes, []byte("3")) {
			seenLater = true
		}
	}
	if facts.Generation.records.components > uint64(facts.Generation.config.MaxRecordComponents) {
		t.Fatalf("component limit mutated past bound: %d", facts.Generation.records.components)
	}
	if !seenLater && !facts.Walk.publishedSyntax[facts.Walk.order[len(facts.Walk.order)-2]] {
		t.Fatal("later independent expression did not recover")
	}
}

func TestExpressionFactsComponentFailureIsAtomic(t *testing.T) {
	header := recordHeader{}
	record := retainedRecord{Header: header, Expression: &expressionRecord{
		Header: header, Kind: expressionTuple, Result: 1, Children: []valueID{2, 3},
	}}
	var arena recordArena
	beforeValues, beforeComponents := len(arena.values), arena.components
	if _, ok := arena.append(record, func(id valueID) bool { return id >= 1 && id <= 3 }, func(controlID) bool { return true }, 1, 2); ok {
		t.Fatal("over-component expression record was accepted")
	}
	if len(arena.values) != beforeValues || arena.components != beforeComponents {
		t.Fatalf("failed append mutated arena: records %d -> %d components %d -> %d", beforeValues, len(arena.values), beforeComponents, arena.components)
	}
}

func TestExpressionFactsRepositoryFixtures(t *testing.T) {
	patterns := []string{
		"../../../tests/check/facts/valid/evidence_*.peb",
		"../../../tests/check/facts/valid/aggregate_*.peb",
		"../../../tests/check/facts/invalid/T0505/evidence_*.peb",
		"../../../tests/check/facts/invalid/T0510/aggregate_*.peb",
		"../../../tests/check/facts/recovery/expression_*.peb",
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
				if facts.Generation.state != generationMutable || len(facts.Walk.order) == 0 {
					t.Fatalf("fixture did not retain mutable generated facts")
				}
				for _, item := range diagnostics.Items() {
					if item.Code == CodeGeneration {
						t.Fatalf("fixture produced generation inconsistency: %+v", item)
					}
				}
			})
		}
	}
}
