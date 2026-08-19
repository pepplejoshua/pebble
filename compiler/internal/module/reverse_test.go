package module

import (
	"reflect"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

// reverseGraph builds a synthetic graph from a map of filename to source,
// using relative imports ("./name"), and returns the graph plus a lookup of
// module IDs by key path.
func reverseGraph(t *testing.T, files map[CanonicalPath][]byte) (*Graph, map[CanonicalPath]ModuleID) {
	t.Helper()
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := Build(BuildConfig{EntryPath: "main.peb", Package: "app"}, &memoryProvider{files: files}, source.NewFileSet(), diagnostics)
	if diagnostics.HasErrors() {
		t.Fatalf("build produced diagnostics: %+v", diagnostics.Items())
	}
	ids := make(map[CanonicalPath]ModuleID)
	for _, module := range graph.Modules() {
		ids[module.Key.Path] = module.ID
	}
	return graph, ids
}

func TestReverseDependentsChain(t *testing.T) {
	// A imports B imports C.
	graph, ids := reverseGraph(t, map[CanonicalPath][]byte{
		"main.peb": []byte(`import "./b";`),
		"b.peb":    []byte(`import "./c";`),
		"c.peb":    []byte(`fn c() int => 0;`),
	})
	a, b, c := ids["main.peb"], ids["b.peb"], ids["c.peb"]
	reverse := graph.ReverseDependents()
	if got := reverse[b]; !reflect.DeepEqual(got, []ModuleID{a}) {
		t.Fatalf("direct importers of B = %v, want [A]", got)
	}
	if got := reverse[c]; !reflect.DeepEqual(got, []ModuleID{b}) {
		t.Fatalf("direct importers of C = %v, want [B]", got)
	}
	// A imports B directly but not C.
	if _, ok := reverse[a]; ok {
		t.Fatalf("A should have no importers, reverse has A: %v", reverse)
	}
	if _, ok := reverse[graph.Prelude]; ok {
		t.Fatalf("prelude should have no importers, reverse has prelude: %v", reverse)
	}
}

func TestTransitiveDependentsChain(t *testing.T) {
	// A imports B imports C. Changing C must report [B A] in dependency order.
	graph, ids := reverseGraph(t, map[CanonicalPath][]byte{
		"main.peb": []byte(`import "./b";`),
		"b.peb":    []byte(`import "./c";`),
		"c.peb":    []byte(`fn c() int => 0;`),
	})
	a, b, c := ids["main.peb"], ids["b.peb"], ids["c.peb"]
	if got := graph.TransitiveDependents(c); !reflect.DeepEqual(got, []ModuleID{b, a}) {
		t.Fatalf("transitive dependents of C = %v, want [B A]", got)
	}
	// Direct-only check: ReverseDependents for C is just [B].
	if got := graph.ReverseDependents()[c]; !reflect.DeepEqual(got, []ModuleID{b}) {
		t.Fatalf("direct importers of C = %v, want [B]", got)
	}
}

func TestTransitiveDependentsDiamond(t *testing.T) {
	// A imports B and C; both B and C import D.
	graph, ids := reverseGraph(t, map[CanonicalPath][]byte{
		"main.peb": []byte(`import "./b"; import "./c";`),
		"b.peb":    []byte(`import "./d";`),
		"c.peb":    []byte(`import "./d";`),
		"d.peb":    []byte(`fn d() int => 0;`),
	})
	a, b, c, d := ids["main.peb"], ids["b.peb"], ids["c.peb"], ids["d.peb"]
	result := graph.TransitiveDependents(d)
	if len(result) != 3 {
		t.Fatalf("transitive dependents of D = %v, want 3 modules", result)
	}
	count := make(map[ModuleID]int)
	position := make(map[ModuleID]int)
	for i, id := range result {
		count[id]++
		position[id] = i
	}
	if count[a] != 1 || count[b] != 1 || count[c] != 1 {
		t.Fatalf("transitive dependents of D = %v, each of A/B/C must appear exactly once", result)
	}
	// A must come after both B and C.
	if position[a] < position[b] || position[a] < position[c] {
		t.Fatalf("transitive dependents of D = %v, A must come after both B and C", result)
	}
}

func TestTransitiveDependentsLeafWithNoImporters(t *testing.T) {
	// A standalone module with no importers.
	graph, ids := reverseGraph(t, map[CanonicalPath][]byte{
		"main.peb": []byte(`fn main() int => 0;`),
	})
	main := ids["main.peb"]
	if got := graph.TransitiveDependents(main); len(got) != 0 {
		t.Fatalf("transitive dependents of leaf = %v, want empty", got)
	}
	// The prelude is never imported.
	if got := graph.TransitiveDependents(graph.Prelude); len(got) != 0 {
		t.Fatalf("transitive dependents of prelude = %v, want empty", got)
	}
	// An orphan module within a larger graph, imported by nobody.
	graph2, ids2 := reverseGraph(t, map[CanonicalPath][]byte{
		"main.peb":   []byte(`import "./b";`),
		"b.peb":      []byte(`import "./c";`),
		"c.peb":      []byte(`fn c() int => 0;`),
		"orphan.peb": []byte(`fn orphan() int => 0;`),
	})
	if got := graph2.TransitiveDependents(ids2["orphan.peb"]); len(got) != 0 {
		t.Fatalf("transitive dependents of orphan = %v, want empty", got)
	}
}

func TestReverseDependentsReturnsOwnedCopy(t *testing.T) {
	graph, ids := reverseGraph(t, map[CanonicalPath][]byte{
		"main.peb": []byte(`import "./b";`),
		"b.peb":    []byte(`fn b() int => 0;`),
	})
	a, b := ids["main.peb"], ids["b.peb"]
	first := graph.ReverseDependents()
	first[b][0] = 0
	second := graph.ReverseDependents()
	if got := second[b]; !reflect.DeepEqual(got, []ModuleID{a}) {
		t.Fatalf("ReverseDependents exposed shared storage: second call = %v, want [A]", got)
	}
}

func TestTransitiveDependentsReturnsOwnedCopy(t *testing.T) {
	graph, ids := reverseGraph(t, map[CanonicalPath][]byte{
		"main.peb": []byte(`import "./b";`),
		"b.peb":    []byte(`fn b() int => 0;`),
	})
	a, b := ids["main.peb"], ids["b.peb"]
	first := graph.TransitiveDependents(b)
	first[0] = 0
	second := graph.TransitiveDependents(b)
	if !reflect.DeepEqual(second, []ModuleID{a}) {
		t.Fatalf("TransitiveDependents exposed shared storage: second call = %v, want [A]", second)
	}
}
