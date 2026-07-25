package check

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func frozenRegions(t *testing.T, facts *preparedFacts) []controlRegion {
	t.Helper()
	regions, ok := facts.Generation.controls.freeze(facts.Generation.config.MaxSemanticRecords)
	if !ok {
		t.Fatal("control hierarchy did not freeze")
	}
	return regions
}

func controlRecords(facts *preparedFacts) []controlRecord {
	var out []controlRecord
	for _, retained := range facts.Generation.records.values {
		if retained.Control != nil {
			out = append(out, *retained.Control)
		}
	}
	return out
}

// checkRegionInvariants proves the 06a freeze invariants that 06b rechecks:
// contiguous allocation order, root depth one, earlier valid parents, exact
// depth increments, one appearance in exactly one ascending child list, and an
// edge count equal to regions minus roots.
func checkRegionInvariants(t *testing.T, regions []controlRegion) (roots, edges int) {
	t.Helper()
	seen := make(map[controlID]int)
	for index, region := range regions {
		if region.ID != controlID(index+1) {
			t.Fatalf("region %d has noncontiguous id %d", index+1, region.ID)
		}
		if region.Parent == 0 {
			if region.Depth != 1 {
				t.Fatalf("root region %d has depth %d", region.ID, region.Depth)
			}
			roots++
			continue
		}
		if uint64(region.Parent) >= uint64(region.ID) {
			t.Fatalf("region %d parent %d is not earlier", region.ID, region.Parent)
		}
		parent := regions[region.Parent-1]
		if region.Depth != parent.Depth+1 {
			t.Fatalf("region %d depth %d, parent depth %d", region.ID, region.Depth, parent.Depth)
		}
	}
	for _, region := range regions {
		previous := controlID(0)
		for _, child := range region.Children {
			if child <= previous {
				t.Fatalf("region %d children are not ascending: %v", region.ID, region.Children)
			}
			previous = child
			if regions[child-1].Parent != region.ID {
				t.Fatalf("region %d child %d has parent %d", region.ID, child, regions[child-1].Parent)
			}
			seen[child]++
			edges++
		}
	}
	for _, region := range regions {
		if region.Parent != 0 && seen[region.ID] != 1 {
			t.Fatalf("region %d appears in %d child lists", region.ID, seen[region.ID])
		}
	}
	if edges != len(regions)-roots {
		t.Fatalf("edges %d, regions %d, roots %d", edges, len(regions), roots)
	}
	return roots, edges
}

func TestControlFactsRegionTopologyAndCallableOwnership(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn outer(flag bool) void {
    {
        while flag {
            if flag { print 1; } else { print 2; }
        }
    }
    let inner = fn() void { { print 3; } };
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	regions := frozenRegions(t, facts)
	roots, edges := checkRegionInvariants(t, regions)
	if roots != 2 || edges != len(regions)-2 {
		t.Fatalf("roots=%d edges=%d regions=%d", roots, edges, len(regions))
	}

	records := controlRecords(facts)
	owner := make(map[controlID]callableRef)
	functions := make(map[callableRef]int)
	for _, record := range records {
		if record.Region == 0 || record.StatementForm == 0 {
			t.Fatalf("control record %+v has no region or statement form", record)
		}
		if record.Kind == controlFunction {
			if regions[record.Region-1].Parent != 0 || regions[record.Region-1].Depth != 1 {
				t.Fatalf("controlFunction names nonroot region %d", record.Region)
			}
			functions[record.Callable]++
		}
		if existing, ok := owner[record.Region]; ok && existing != record.Callable {
			t.Fatalf("region %d carries %+v and %+v", record.Region, existing, record.Callable)
		}
		owner[record.Region] = record.Callable
	}
	if len(functions) != 2 {
		t.Fatalf("function roots = %+v", functions)
	}
	for callable, count := range functions {
		if count != 1 {
			t.Fatalf("callable %+v owns %d function records", callable, count)
		}
	}
	// Every region below a function root carries that root's exact callableRef.
	for _, region := range regions {
		if region.Parent == 0 {
			continue
		}
		if owner[region.ID] != owner[rootOf(regions, region.ID)] {
			t.Fatalf("region %d callable %+v differs from its root", region.ID, owner[region.ID])
		}
	}
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func rootOf(regions []controlRegion, id controlID) controlID {
	for regions[id-1].Parent != 0 {
		id = regions[id-1].Parent
	}
	return id
}

func TestStatementFactsEveryStatementForm(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn side() void { }
fn forms(value i32) void {
    var total i32 = value;
    print total;
    total = 1;
    side();
    total++;
    (total);
    return;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	forms := make(map[statementForm]int)
	for _, record := range controlRecords(facts) {
		forms[record.StatementForm]++
	}
	for _, form := range []statementForm{statementPrint, statementDiscard, statementAssignment, statementCall, statementPostfixUpdate, statementOther} {
		if forms[form] == 0 {
			t.Fatalf("statement form %d absent: %+v", form, forms)
		}
	}
	if forms[0] != 0 {
		t.Fatal("a control record carries a zero statement form")
	}
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

// TestStatementFactsGroupedCallIsDiscarded proves classification uses the
// immediate retained expression record kind and never unwraps grouping.
func TestStatementFactsGroupedCallIsDiscarded(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn side() void { }
fn forms() void { (side()); }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	found := false
	for _, record := range controlRecords(facts) {
		if record.Kind == controlExpression {
			found = true
			if record.StatementForm != statementDiscard {
				t.Fatalf("grouped call form = %d", record.StatementForm)
			}
		}
	}
	if !found || diagnostics.HasErrors() {
		t.Fatalf("found=%v diagnostics=%+v", found, diagnostics.Items())
	}
}

func TestControlFactsConditionElseAndRangeModes(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn modes(flag bool, limit i32) void {
    if flag { print 1; }
    if flag { print 2; } else { print 3; }
    while flag { break; }
    for var step i32 = 0; step < limit; step += 1 { break; }
    for ; ; { break; }
    loop 0..limit { print 4; }
    loop 0..=limit { print 5; }
    switch limit { case 1: print 6; }
    switch limit { case 1: print 7; else: print 8; }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	type shape struct {
		kind                                          controlKind
		conditionPresent, elsePresent, rangeInclusive bool
	}
	got := make(map[shape]int)
	for _, record := range controlRecords(facts) {
		got[shape{record.Kind, record.ConditionPresent, record.ElsePresent, record.RangeInclusive}]++
		if record.ConditionPresent {
			condition := 0
			for _, entry := range record.Values {
				if entry.Role == valueCondition {
					condition++
				}
			}
			if condition != 1 {
				t.Fatalf("%+v retains %d condition values", record, condition)
			}
		}
	}
	want := map[shape]int{
		{controlIf, true, false, false}:         1,
		{controlIf, true, true, false}:          1,
		{controlWhile, true, false, false}:      1,
		{controlFor, true, false, false}:        1,
		{controlFor, false, false, false}:       1,
		{controlRangeLoop, false, false, false}: 1,
		{controlRangeLoop, false, false, true}:  1,
		{controlSwitch, false, false, false}:    1,
		{controlSwitch, false, true, false}:     1,
	}
	for key, count := range want {
		if got[key] != count {
			t.Fatalf("shape %+v = %d, want %d (all: %+v)", key, got[key], count, got)
		}
	}
	// The omitted for condition fabricates no syntax node and no publication.
	for _, ref := range facts.Walk.order {
		node, _ := facts.Walk.node(ref.Module, ref.Node)
		if node.Kind() == syntax.ForStmt && node.Data()&syntax.ForConditionPresent == 0 {
			if _, ok := facts.Walk.expectations[ref]; ok {
				t.Fatal("omitted for condition produced an expectation")
			}
		}
	}
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestControlFactsBreakAndContinueCandidateTargets(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn jumps(flag bool, value i32) void {
    while flag {
        switch value {
        case 0: break;
        else: continue;
        }
    }
    switch value {
    case 1:
        while flag { break; }
    else:
        break;
    }
    while flag { break; }
    break;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	regions := frozenRegions(t, facts)
	checkRegionInvariants(t, regions)
	var breaks, continues []controlRecord
	kinds := make(map[controlID]controlKind)
	for _, record := range controlRecords(facts) {
		switch record.Kind {
		case controlBreak:
			breaks = append(breaks, record)
		case controlContinue:
			continues = append(continues, record)
		default:
			if regionOwningControl(record.Kind) {
				kinds[record.Region] = record.Kind
			}
		}
	}
	if len(breaks) != 5 || len(continues) != 1 {
		t.Fatalf("breaks=%d continues=%d", len(breaks), len(continues))
	}
	// A break inside a switch nested in a loop names the switch; a break inside
	// a loop nested in a switch names the loop; continue ignores switches.
	if kinds[breaks[0].Target] != controlSwitch {
		t.Fatalf("break in nested switch targets %d (kind %d)", breaks[0].Target, kinds[breaks[0].Target])
	}
	if kinds[continues[0].Target] != controlWhile {
		t.Fatalf("continue targets %d (kind %d)", continues[0].Target, kinds[continues[0].Target])
	}
	if kinds[breaks[1].Target] != controlWhile {
		t.Fatalf("break in loop nested in switch targets kind %d", kinds[breaks[1].Target])
	}
	if kinds[breaks[2].Target] != controlSwitch {
		t.Fatalf("break in switch else arm targets kind %d", kinds[breaks[2].Target])
	}
	if kinds[breaks[3].Target] != controlWhile {
		t.Fatalf("break in a plain loop targets kind %d", kinds[breaks[3].Target])
	}
	if breaks[4].Target != 0 {
		t.Fatalf("function-level break retained target %d", breaks[4].Target)
	}
	// Every nonzero target lives in the same function-root tree as its record.
	for _, record := range append(append([]controlRecord(nil), breaks...), continues...) {
		if record.Target != 0 && rootOf(regions, record.Target) != rootOf(regions, record.Region) {
			t.Fatalf("target %d is outside the record's function root", record.Target)
		}
	}
	if !diagnostics.HasErrors() {
		// 06a emits no C0611; a missing target is 06b policy.
		for _, item := range diagnostics.Items() {
			if item.Code == CodeGeneration {
				t.Fatalf("06a reported a control-flow policy failure: %+v", item)
			}
		}
	}
}

func TestControlFactsRangeIteratorPublicationAndBinding(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn walk(limit i32) void {
    var total i32 = 0;
    loop 0..limit : index { total += index; }
    loop 0..=limit { total += 1; }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	iterators := 0
	for _, retained := range facts.Generation.records.values {
		if retained.Binding == nil || retained.Binding.Kind != bindingRangeIterator {
			continue
		}
		iterators++
		binding := retained.Binding
		if binding.AnnotationPresent || binding.InitializerPresent || binding.Global || binding.Mutable {
			t.Fatalf("range iterator binding = %+v", binding)
		}
		resolved, ok := inputs.Resolution.Symbols.Symbol(binding.Symbol)
		if !ok || resolved.Name != "index" {
			t.Fatalf("range iterator symbol = %+v", resolved)
		}
		if !facts.Walk.publishedSymbols[binding.Symbol] {
			t.Fatal("range iterator was not published")
		}
	}
	if iterators != 1 {
		t.Fatalf("range iterator bindings = %d", iterators)
	}
	roles := make(map[controlValueRole]int)
	inclusive := 0
	for _, record := range controlRecords(facts) {
		if record.Kind != controlRangeLoop {
			continue
		}
		if record.RangeInclusive {
			inclusive++
		}
		for _, entry := range record.Values {
			roles[entry.Role]++
		}
	}
	if roles[valueRangeStart] != 2 || roles[valueRangeEnd] != 2 || roles[valueRangeIterator] != 1 || inclusive != 1 {
		t.Fatalf("roles=%+v inclusive=%d", roles, inclusive)
	}
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestSwitchFactsScalarAndNominalClassification(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Color = enum { red, green, blue };
fn scalar(value i32, text str) void {
    switch value { case 1, 2: print 1; else: print 2; }
    switch text { case "a": print 3; else: print 4; }
}
fn nominal(color Color) void {
    switch color { case Color.red: print 5; else: print 6; }
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	item, _ := inputs.Graph.Module(inputs.Graph.Root)
	scalar, nominal := 0, 0
	for _, ref := range facts.Walk.order {
		node, _ := facts.Walk.node(ref.Module, ref.Node)
		if node.Kind() != syntax.SwitchCase {
			continue
		}
		for _, value := range switchCaseValues(ref, node, item.Tree) {
			if facts.Walk.nominalCase(value, item.Tree) {
				nominal++
				if _, evaluated := facts.Constants.memo[value]; evaluated {
					t.Fatal("nominal case was constant evaluated")
				}
				if _, expected := facts.Walk.expectations[value]; expected {
					t.Fatal("nominal case carries scalar subject evidence")
				}
				continue
			}
			scalar++
			if _, evaluated := facts.Constants.memo[value]; !evaluated {
				t.Fatalf("scalar case %+v was not constant evaluated", value)
			}
		}
	}
	if scalar != 3 || nominal != 1 {
		t.Fatalf("scalar=%d nominal=%d", scalar, nominal)
	}
	cases := 0
	for _, record := range controlRecords(facts) {
		if record.Kind != controlSwitchCase {
			continue
		}
		previous := int64(-1)
		for _, entry := range record.Values {
			if entry.Role != valueCase || int64(entry.Ordinal) <= previous {
				t.Fatalf("switch case values are not ascending: %+v", record.Values)
			}
			previous = int64(entry.Ordinal)
			cases++
		}
	}
	if cases != 4 {
		t.Fatalf("retained case values = %d", cases)
	}
	for _, retained := range facts.Generation.records.values {
		if retained.Control != nil && retained.Control.Kind == controlSwitch && len(retained.Control.Values) != 1 {
			t.Fatalf("switch record retains %d subject values", len(retained.Control.Values))
		}
	}
}

func TestSwitchFactsInvalidConstantRecoversIntoLaterStatements(t *testing.T) {
	contents, err := os.ReadFile("../../../tests/check/facts/recovery/control_independent.peb")
	if err != nil {
		t.Fatal(err)
	}
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
	facts := run06a3(inputs, diagnostics, Config{})
	constant := 0
	for _, item := range diagnostics.Items() {
		switch item.Code {
		case CodeInvalidConstant:
			constant++
		case CodeGeneration:
			t.Fatalf("rejected switch constant produced a generation inconsistency: %+v", item)
		}
	}
	if constant == 0 {
		t.Fatalf("no C0614: %+v", diagnostics.Items())
	}
	prints, returns := 0, 0
	for _, record := range controlRecords(facts) {
		if record.Kind == controlPrint {
			prints++
		}
		if record.Kind == controlReturn {
			returns++
		}
	}
	if prints != 3 || returns != 1 {
		t.Fatalf("prints=%d returns=%d after recovery", prints, returns)
	}
	checkRegionInvariants(t, frozenRegions(t, facts))
}

func TestControlFactsLoweredLimitsFailAtomically(t *testing.T) {
	source := []byte(`
fn bounded(flag bool, value i32) void {
    loop 0..value : index { print index; }
    while flag { if flag { break; } }
    switch value { case 1 + 1: print 1; else: print 2; }
    defer print 3;
    return;
}
`)
	for _, group := range []struct {
		name   string
		config Config
	}{
		{"control depth", Config{MaxControlDepth: 2}},
		{"syntax visits", Config{MaxSyntaxVisits: 12}},
		{"semantic records", Config{MaxSemanticRecords: 6}},
		{"record components", Config{MaxRecordComponents: 8}},
		{"constant operations", Config{MaxConstantOperations: 1}},
		{"diagnostics", Config{MaxControlDepth: 2, MaxDiagnostics: 1}},
	} {
		t.Run(group.name, func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": source})
			facts := run06a3(inputs, diagnostics, group.config)
			if facts == nil || facts.Generation == nil {
				t.Fatal("lowered limit discarded the generation lifecycle")
			}
			if !diagnostics.HasErrors() {
				t.Fatalf("lowered %s produced no diagnostic", group.name)
			}
			if uint32(len(diagnostics.Items())) > group.config.MaxDiagnostics && group.config.MaxDiagnostics != 0 {
				t.Fatalf("lowered %s exceeded its diagnostic budget: %+v", group.name, diagnostics.Items())
			}
			// Every retained record and region stays internally consistent.
			regions, ok := facts.Generation.controls.freeze(facts.Generation.config.MaxSemanticRecords)
			if !ok {
				t.Fatalf("lowered %s left an inconsistent control hierarchy", group.name)
			}
			checkRegionInvariants(t, regions)
			for _, retained := range facts.Generation.records.values {
				if _, _, valid := retained.payloadResources(); !valid {
					t.Fatalf("lowered %s retained an invalid record: %+v", group.name, retained.Header)
				}
				if retained.Control != nil && uint64(retained.Control.Region) > uint64(len(regions)) {
					t.Fatalf("lowered %s retained a foreign region", group.name)
				}
			}
			facts.Session.Solve()
		})
	}
}

func TestControlFactsRejectsMalformedRecords(t *testing.T) {
	header := rootHeader(t, validGenerationInputs(t))
	callable := callableRef{Syntax: header.Syntax}
	cases := []controlRecord{
		{Header: header, Kind: controlBlock, Region: 1, Callable: callable},
		{Header: header, Kind: controlBlock, Region: 0, Callable: callable, StatementForm: statementOther},
		{Header: header, Kind: controlBlock, Region: 1, StatementForm: statementOther},
		{Header: header, Kind: controlBlock, Region: 1, Target: 1, Callable: callable, StatementForm: statementOther},
		{Header: header, Kind: controlBlock, Region: 1, Callable: callable, StatementForm: statementOther, ConditionPresent: true},
		{Header: header, Kind: controlWhile, Region: 1, Callable: callable, StatementForm: statementOther, ElsePresent: true},
		{Header: header, Kind: controlWhile, Region: 1, Callable: callable, StatementForm: statementOther, RangeInclusive: true},
		{Header: header, Kind: controlBlock, Region: 1, Callable: callable, StatementForm: statementOther, Values: []controlValue{{Role: valueCondition}}},
		{Header: header, Kind: controlKind(200), Region: 1, Callable: callable, StatementForm: statementOther},
	}
	for index, record := range cases {
		t.Run(fmt.Sprintf("case%d", index), func(t *testing.T) {
			value := record
			arena := &recordArena{}
			if _, ok := arena.append(retainedRecord{Header: header, Controls: []controlID{1}, Control: &value}, func(valueID) bool { return true }, func(controlID) bool { return true }, 8, 8); ok {
				t.Fatalf("malformed control record accepted: %+v", record)
			}
			if len(arena.values) != 0 || arena.components != 0 {
				t.Fatal("rejected control record mutated the arena")
			}
		})
	}
	valid := controlRecord{Header: header, Kind: controlBlock, Region: 1, Callable: callable, StatementForm: statementOther}
	arena := &recordArena{}
	if _, ok := arena.append(retainedRecord{Header: header, Controls: []controlID{1}, Control: &valid}, func(valueID) bool { return true }, func(controlID) bool { return true }, 8, 8); !ok {
		t.Fatal("valid control record rejected")
	}
	if len(arena.values) != 1 || arena.components != 1 {
		t.Fatalf("records=%d components=%d", len(arena.values), arena.components)
	}
}

func TestControlFactsRepositoryFixtures(t *testing.T) {
	groups := []struct {
		pattern    string
		wantErrors bool
	}{
		{"../../../tests/check/facts/valid/control_*.peb", false},
		{"../../../tests/check/facts/valid/defer_*.peb", false},
		{"../../../tests/check/facts/invalid/C0614/switch_*.peb", true},
		{"../../../tests/check/facts/recovery/control_*.peb", true},
	}
	for _, group := range groups {
		paths, err := filepath.Glob(group.pattern)
		if err != nil || len(paths) == 0 {
			t.Fatalf("glob %s: %v", group.pattern, err)
		}
		for _, path := range paths {
			t.Run(filepath.Base(path), func(t *testing.T) {
				contents, err := os.ReadFile(path)
				if err != nil {
					t.Fatal(err)
				}
				inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
				facts := run06a3(inputs, diagnostics, Config{})
				checkRegionInvariants(t, frozenRegions(t, facts))
				for _, item := range diagnostics.Items() {
					if item.Code == CodeGeneration {
						t.Fatalf("fixture produced a generation inconsistency: %+v", item)
					}
				}
				facts.Session.Solve()
				if group.wantErrors == !diagnostics.HasErrors() {
					t.Fatalf("errors=%v diagnostics=%+v", diagnostics.HasErrors(), diagnostics.Items())
				}
			})
		}
	}
}
