package check

import (
	"bytes"
	"os"
	"sync"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// concurrentResultFixture is a single-module program that validates
// successfully and exercises every public Result accessor category: plain
// expressions and arithmetic, places (assignment and update targets), a
// conversion via optional injection and one via an explicit cast, direct calls,
// a member access, and control flow (if, while, break, return, defer).
const concurrentResultFixture = `
type Box = struct { value i32; };
type Color = enum { red, blue };

fn take(payload ?i32) void {}
fn sink(value i32) void {}

fn helper(flag bool, box Box) i32 {
    var total i32 = 0;
    if flag {
        total = box.value;
    }
    return total;
}

fn main(box Box, flag bool) i32 {
    var total i32 = 0;
    defer print 1;
    while flag {
        total += 1;
        break;
    }
    total = helper(flag, box);
    sink(total);
    take(some 1);
    let converted i64 = box.value as i64;
    print total;
    print converted;
    return total;
}
`

// TestConcurrentResultReads extends the TestIRBuilderConcurrentUnitReads idiom
// from a bare *tir.Unit to the *Result returned by the public Check(...)
// entry point: one Result is built once, and several goroutines concurrently
// read a mix of its accessor surface — Result.IR(), the six per-ref accessors
// (Expression/Place/Conversion/Call/Member/Control), the solution/type
// accessors, and the IR unit's own read methods including Dump. The refs are
// located once up front, before any goroutine starts, from the published
// unit's SourceRefs; this is the "concurrent reads" half of the spec's
// completion bar and only passes under -race.
func TestConcurrentResultReads(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(concurrentResultFixture)})
	result := Check(inputs, diagnostics, Config{})
	unit := result.IR()
	if unit == nil {
		t.Fatalf("concurrent-read fixture was rejected: %+v", diagnostics.Items())
	}
	refs := unit.SourceRefs()
	var expressionRef, placeRef, conversionRef, callRef, memberRef, controlRef symbol.SyntaxRef
	found := map[string]bool{}
	for _, ref := range refs {
		if _, ok := result.Expression(ref); ok {
			if expressionRef.Module == 0 {
				expressionRef = ref
			}
			found["expression"] = true
		}
		if _, ok := result.Place(ref); ok {
			if placeRef.Module == 0 {
				placeRef = ref
			}
			found["place"] = true
		}
		if _, ok := result.Conversion(ref); ok {
			if conversionRef.Module == 0 {
				conversionRef = ref
			}
			found["conversion"] = true
		}
		if _, ok := result.Call(ref); ok {
			if callRef.Module == 0 {
				callRef = ref
			}
			found["call"] = true
		}
		if _, ok := result.Member(ref); ok {
			if memberRef.Module == 0 {
				memberRef = ref
			}
			found["member"] = true
		}
		if _, ok := result.Control(ref); ok {
			if controlRef.Module == 0 {
				controlRef = ref
			}
			found["control"] = true
		}
	}
	for _, category := range []string{"expression", "place", "conversion", "call", "member", "control"} {
		if !found[category] {
			t.Fatalf("fixture produced no %s accessor ref", category)
		}
	}
	var fnSymbol symbol.SymbolID
	if decls := unit.FunctionDeclarations(); len(decls) != 0 {
		fnSymbol = decls[0].Symbol
	}
	var wg sync.WaitGroup
	for i := 0; i < 8; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 20; j++ {
				_ = result.Successful()
				_ = result.Solution()
				_ = result.IR()
				if _, ok := result.Expression(expressionRef); !ok {
					t.Errorf("Expression() lost its ref during concurrent reads")
				}
				_, _ = result.Place(placeRef)
				_, _ = result.Conversion(conversionRef)
				if _, ok := result.Call(callRef); !ok {
					t.Errorf("Call() lost its ref during concurrent reads")
				}
				_, _ = result.Member(memberRef)
				if _, ok := result.Control(controlRef); !ok {
					t.Errorf("Control() lost its ref during concurrent reads")
				}
				_, _ = result.Instantiation(expressionRef)
				if fnSymbol != 0 {
					_, _ = result.SymbolType(fnSymbol)
					_ = result.Requirements(fnSymbol)
				}
				if unit := result.IR(); unit != nil {
					_ = unit.Nodes()
					_ = unit.Modules()
					_ = unit.TypeDeclarations()
					_ = unit.FunctionDeclarations()
					_ = unit.GlobalDeclarations()
					_ = unit.SourceRefs()
					_ = unit.NodeCount()
					var dump bytes.Buffer
					if err := unit.Dump(&dump); err != nil {
						t.Errorf("concurrent IR Dump failed: %v", err)
					}
				}
			}
		}()
	}
	wg.Wait()
}

// TestIndependentResultCompilations is the "independent compilations" half of
// the spec's completion bar: several goroutines each run their own independent
// Check(...) call on their own independently built Inputs and diagnostics, made
// from real .peb fixtures round-robined across goroutines, and every run must
// match what the same fixture produces in a single-threaded canonical run. A
// false pass here would mean Check() shares global mutable state that corrupts
// unrelated concurrent compilations, which only -race can expose.
func TestIndependentResultCompilations(t *testing.T) {
	paths := []string{
		"../../../tests/check/ir/valid/operations_and_calls.peb",
		"../../../tests/check/ir/valid/control_and_defer.peb",
		"../../../tests/check/validation/recovery/integration_chain.peb",
	}
	type fixture struct {
		path        string
		contents    []byte
		wantSuccess bool
		wantDiags   int
		wantDump    string
	}
	fixtures := make([]fixture, len(paths))
	for i, path := range paths {
		contents, err := os.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}
		fixtures[i] = fixture{path: path, contents: contents}
	}
	for i := range fixtures {
		inputs, diagnostics := factInputs(t, checkProvider{"main.peb": fixtures[i].contents})
		result := Check(inputs, diagnostics, Config{})
		fixtures[i].wantSuccess = result.Successful()
		fixtures[i].wantDiags = len(diagnostics.Items())
		if unit := result.IR(); unit != nil {
			var dump bytes.Buffer
			if err := unit.Dump(&dump); err != nil {
				t.Fatalf("canonical dump for %s failed: %v", fixtures[i].path, err)
			}
			fixtures[i].wantDump = dump.String()
		}
	}
	var wg sync.WaitGroup
	for g := 0; g < 4; g++ {
		wg.Add(1)
		go func(g int) {
			defer wg.Done()
			for j := 0; j < 5; j++ {
				fx := fixtures[(g+j)%len(fixtures)]
				inputs, diagnostics := factInputs(t, checkProvider{"main.peb": fx.contents})
				result := Check(inputs, diagnostics, Config{})
				if result.Successful() != fx.wantSuccess {
					t.Errorf("%s: Successful() = %v, want %v", fx.path, result.Successful(), fx.wantSuccess)
					continue
				}
				if got := len(diagnostics.Items()); got != fx.wantDiags {
					t.Errorf("%s: diagnostics = %d, want %d", fx.path, got, fx.wantDiags)
				}
				unit := result.IR()
				if !fx.wantSuccess {
					if unit != nil {
						t.Errorf("%s: failed result published IR", fx.path)
					}
					continue
				}
				if unit == nil {
					t.Errorf("%s: successful result published no IR", fx.path)
					continue
				}
				var dump bytes.Buffer
				if err := unit.Dump(&dump); err != nil {
					t.Errorf("%s: IR dump failed: %v", fx.path, err)
					continue
				}
				if dump.String() != fx.wantDump {
					t.Errorf("%s: IR dump differs from the single-threaded canonical run", fx.path)
				}
			}
		}(g)
	}
	wg.Wait()
}
