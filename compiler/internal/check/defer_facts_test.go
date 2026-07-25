package check

import (
	"os"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func deferRecords(facts *preparedFacts) []deferRecord {
	var out []deferRecord
	for _, retained := range facts.Generation.records.values {
		if retained.Defer != nil {
			out = append(out, *retained.Defer)
		}
	}
	return out
}

func TestDeferFactsRegistrationOrderAndRegions(t *testing.T) {
	contents, err := os.ReadFile("../../../tests/check/facts/valid/defer_registration.peb")
	if err != nil {
		t.Fatal(err)
	}
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
	facts := run06a3(inputs, diagnostics, Config{})
	regions := frozenRegions(t, facts)
	checkRegionInvariants(t, regions)

	records := deferRecords(facts)
	if len(records) != 5 {
		t.Fatalf("defer records = %d", len(records))
	}
	next := make(map[controlID]uint32)
	byRegion := make(map[controlID]int)
	for _, record := range records {
		if record.Region == 0 || uint64(record.Region) > uint64(len(regions)) {
			t.Fatalf("defer names region %d", record.Region)
		}
		if record.Ordinal != next[record.Region] {
			t.Fatalf("defer in region %d has ordinal %d, want %d", record.Region, record.Ordinal, next[record.Region])
		}
		next[record.Region]++
		byRegion[record.Region]++
		if !facts.Generation.validSyntax(record.Statement) {
			t.Fatalf("defer statement %+v is not valid syntax", record.Statement)
		}
		node, ok := facts.Walk.node(record.Statement.Module, record.Statement.Node)
		if !ok || node.Kind() == syntax.Missing || node.Kind() == syntax.Error {
			t.Fatalf("defer statement %+v is not a checked statement", record.Statement)
		}
	}
	// Two defers register in the nested block, one in the if arm, two at the
	// function body region; per-region ordinals restart at zero.
	counts := make(map[int]int)
	for _, count := range byRegion {
		counts[count]++
	}
	if counts[1] != 1 || counts[2] != 2 || len(byRegion) != 3 {
		t.Fatalf("defers per region = %+v", byRegion)
	}

	// Every deferRecord is paired with exactly one controlDefer leaf record in
	// the same lexical region.
	leaves := 0
	for _, record := range controlRecords(facts) {
		if record.Kind != controlDefer {
			continue
		}
		leaves++
		if record.StatementForm != statementOther || record.Target != 0 || len(record.Values) != 0 {
			t.Fatalf("controlDefer record = %+v", record)
		}
	}
	if leaves != len(records) {
		t.Fatalf("controlDefer records = %d, defer records = %d", leaves, len(records))
	}
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestDeferFactsVisitsDeferredStatementExactlyOnce(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn scope(flag bool) void {
    defer { print 1; print 2; }
    if flag { return; }
    while flag { break; }
    return;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	records := deferRecords(facts)
	if len(records) != 1 {
		t.Fatalf("defer records = %d", len(records))
	}
	seen := make(map[symbol.SyntaxRef]int)
	for _, ref := range facts.Walk.order {
		seen[ref]++
	}
	if seen[records[0].Statement] != 1 {
		t.Fatalf("deferred statement visited %d times", seen[records[0].Statement])
	}
	// Three exits leave this function, and the deferred block is still walked
	// once. Defer edge expansion belongs to 06b.
	for ref, count := range seen {
		if count != 1 {
			t.Fatalf("node %+v visited %d times", ref, count)
		}
	}
	prints := 0
	for _, record := range controlRecords(facts) {
		if record.Kind == controlPrint {
			prints++
		}
	}
	if prints != 2 {
		t.Fatalf("deferred print records = %d", prints)
	}
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

// TestDeferFactsGeneratesNestedJumpsWithoutPolicy proves 06a retains a deferred
// return, break, continue, and nested defer normally. C0613 is 06b's.
func TestDeferFactsGeneratesNestedJumpsWithoutPolicy(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn nested(flag bool) void {
    while flag {
        defer break;
        defer continue;
        defer defer print 1;
    }
    defer return;
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	records := deferRecords(facts)
	if len(records) != 5 {
		t.Fatalf("defer records = %d", len(records))
	}
	kinds := make(map[controlKind]int)
	for _, record := range controlRecords(facts) {
		kinds[record.Kind]++
	}
	if kinds[controlBreak] != 1 || kinds[controlContinue] != 1 || kinds[controlReturn] != 1 || kinds[controlDefer] != 5 {
		t.Fatalf("kinds = %+v", kinds)
	}
	for _, item := range diagnostics.Items() {
		if item.Code == CodeGeneration {
			t.Fatalf("deferred jump produced a generation inconsistency: %+v", item)
		}
	}
	// The inner defer of `defer defer` registers after its enclosing one.
	ordinals := make(map[controlID][]uint32)
	for _, record := range records {
		ordinals[record.Region] = append(ordinals[record.Region], record.Ordinal)
	}
	for region, values := range ordinals {
		for index, value := range values {
			if value != uint32(index) {
				t.Fatalf("region %d ordinals = %v", region, values)
			}
		}
	}
	checkRegionInvariants(t, frozenRegions(t, facts))
}

func TestDeferFactsRejectsMalformedRecords(t *testing.T) {
	header := rootHeader(t, validGenerationInputs(t))
	cases := []deferRecord{
		{Header: header, Region: 0, Statement: header.Syntax},
		{Header: header, Region: 1},
	}
	for _, record := range cases {
		value := record
		arena := &recordArena{}
		if _, ok := arena.append(retainedRecord{Header: header, Controls: []controlID{1}, Defer: &value}, func(valueID) bool { return true }, func(controlID) bool { return true }, 4, 4); ok {
			t.Fatalf("malformed defer record accepted: %+v", record)
		}
		if len(arena.values) != 0 || arena.components != 0 {
			t.Fatal("rejected defer record mutated the arena")
		}
	}
	valid := deferRecord{Header: header, Region: 1, Ordinal: 3, Statement: header.Syntax}
	arena := &recordArena{}
	id, ok := arena.append(retainedRecord{Header: header, Controls: []controlID{1}, Defer: &valid}, func(valueID) bool { return true }, func(controlID) bool { return true }, 4, 4)
	if !ok || id != 1 {
		t.Fatal("valid defer record rejected")
	}
	stored, _ := arena.record(id)
	if stored.Defer == nil || stored.Defer.Ordinal != 3 || stored.Defer.Header.ID != id {
		t.Fatalf("stored defer record = %+v", stored.Defer)
	}
}
