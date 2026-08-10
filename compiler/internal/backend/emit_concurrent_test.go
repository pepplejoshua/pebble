package backend

import (
	"bytes"
	"fmt"
	"sync"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// TestEmitConcurrentSafe proves Emit is safe to call concurrently from many
// goroutines: each Emit invocation now builds its own request-scoped emitState
// (symbols/globals/externData/allocatorAdapters) instead of writing the
// package-level globals Emit used to own, so concurrent calls with independent
// units cannot corrupt one another. The test builds one independent
// unit/snapshot per fixture (covering the four state fields: mutable globals,
// extern variables, extern functions resolved through the symbol table, and a
// runtime Allocator construction that populates the allocator-adapter bridges),
// emits a sequential reference for each, then runs a batch of goroutines that
// all call Emit concurrently on every fixture and compare each output against
// its reference byte-for-byte. Under -race this is the direct proof: the old
// package-level-variable design raced here (and a serialized, misassigned
// output also fails the byte comparison), so a clean pass means the refactor
// actually removed the shared mutable state.
func TestEmitConcurrentSafe(t *testing.T) {
	t.Parallel()
	programs := []struct {
		name        string
		source      string
		withSymbols bool
	}{
		{name: "plain", source: "fn main() int { return 0; }"},
		{name: "globals", source: `var counter int = 0;

fn bump() void {
    counter = counter + 1;
}

fn read() int {
    return counter;
}

fn main() int {
    bump();
    return read();
}`},
		{name: "externVariable", source: "extern {\n    var errno int;\n}\n\nfn main() int {\n    return errno;\n}", withSymbols: true},
		{name: "externFunction", source: "extern fn rand() int; fn main() int { var x int = rand(); return 0; }", withSymbols: true},
		{name: "allocator", source: `fn my_alloc(ctx *void, size uint) *void { return nil; }
fn my_realloc(ctx *void, ptr *void, size uint) *void { return nil; }
fn my_free(ctx *void, ptr *void) void {}
fn main() int {
    var a = Allocator.{
        ptr = nil,
        alloc = my_alloc,
        realloc = my_realloc,
        free = my_free,
    };
    return 0;
}`},
		{name: "slice", source: "fn main() int { var s []int = [1, 2, 3]; return s[1]; }"},
		{name: "arrayLoop", source: "fn main() i32 { let a [3]i32 = [10, 20, 30]; var sum i32 = 0; loop 0..3 : i { sum = sum + a[i]; } return sum; }"},
		{name: "stringPrint", source: `fn main() int { var s str = "hi"; print s; return 0; }`},
		{name: "enumUnion", source: `type Color = enum { red, green, blue };

fn main() int {
    var c = Color.green;
    return 1;
}`},
	}

	type fixture struct {
		name      string
		unit      *tir.Unit
		snapshot  *types.Snapshot
		entry     symbol.SymbolID
		sources   *source.FileSet
		symbols   *symbol.Result
		reference []byte
	}

	fixtures := make([]fixture, 0, len(programs))
	for _, p := range programs {
		var unit *tir.Unit
		var snapshot *types.Snapshot
		var entryID symbol.SymbolID
		var sources *source.FileSet
		var symbols *symbol.Result
		if p.withSymbols {
			unit, snapshot, entryID, sources, symbols = buildFixtureWithSymbols(t, p.source)
		} else {
			unit, snapshot, entryID, sources = buildFixture(t, p.source, "main", false)
		}
		var reference bytes.Buffer
		if err := Emit(unit, snapshot, entryID, sources, symbols, &reference); err != nil {
			t.Fatalf("fixture %s: reference Emit failed: %v", p.name, err)
		}
		fixtures = append(fixtures, fixture{name: p.name, unit: unit, snapshot: snapshot, entry: entryID, sources: sources, symbols: symbols, reference: reference.Bytes()})
	}

	const goroutines = 16
	var wg sync.WaitGroup
	errCh := make(chan string, goroutines)
	for g := 0; g < goroutines; g++ {
		wg.Add(1)
		go func(g int) {
			defer wg.Done()
			for round := 0; round < 3; round++ {
				for i := range fixtures {
					f := &fixtures[i]
					var buf bytes.Buffer
					if err := Emit(f.unit, f.snapshot, f.entry, f.sources, f.symbols, &buf); err != nil {
						errCh <- fmt.Sprintf("goroutine %d fixture %s: Emit failed: %v", g, f.name, err)
						return
					}
					if !bytes.Equal(buf.Bytes(), f.reference) {
						errCh <- fmt.Sprintf("goroutine %d fixture %s: concurrent Emit output differs from the sequential reference", g, f.name)
						return
					}
				}
			}
		}(g)
	}
	wg.Wait()
	close(errCh)
	for msg := range errCh {
		t.Error(msg)
	}
}
