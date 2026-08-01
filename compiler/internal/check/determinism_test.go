package check

import (
	"bytes"
	"fmt"
	"reflect"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// TestFactDeterminismCleanProgram verifies that repeated runs of run06a on the
// same input produce identical output, including module structure, records, and
// diagnostic ordering.
func TestFactDeterminismCleanProgram(t *testing.T) {
	const runs = 8

	var handoffs []*solveHandoff
	var diagnosticSets []*diagnostic.DiagnosticSet

	// Run the same program 8 times.
	for i := 0; i < runs; i++ {
		inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct { x i32; y i32; };

fn distance(p Point) i32 {
    return p.x + p.y;
}

fn compute(value i32) i32 {
    let p = Point.{ x = value, y = value };
    return distance(p);
}

fn main() void {
    print compute(42);
}
`)})

		handoff := run06a(inputs, diagnostics, Config{})
		handoffs = append(handoffs, handoff)
		diagnosticSets = append(diagnosticSets, diagnostics)
	}

	// All handoffs must be non-nil and successful.
	for i, h := range handoffs {
		if h == nil {
			t.Fatalf("run %d: handoff is nil", i)
		}
		if h.Semantics == nil {
			t.Fatalf("run %d: Semantics is nil", i)
		}
	}

	// Compare compilation structures across all runs.
	firstComp := handoffs[0].Compilation
	for i := 1; i < runs; i++ {
		comp := handoffs[i].Compilation
		if comp.Root != firstComp.Root {
			t.Fatalf("run %d: Root %d != first %d", i, comp.Root, firstComp.Root)
		}
		if len(comp.Modules) != len(firstComp.Modules) {
			t.Fatalf("run %d: module count %d != first %d", i, len(comp.Modules), len(firstComp.Modules))
		}
		if len(comp.DependencyOrder) != len(firstComp.DependencyOrder) {
			t.Fatalf("run %d: dependency order length %d != first %d", i, len(comp.DependencyOrder), len(firstComp.DependencyOrder))
		}

		// Compare module IDs and declaration counts.
		for j := range comp.Modules {
			m1 := comp.Modules[j]
			m2 := firstComp.Modules[j]
			if m1.ID != m2.ID {
				t.Fatalf("run %d module %d: ID %d != first %d", i, j, m1.ID, m2.ID)
			}
			if len(m1.Declarations) != len(m2.Declarations) {
				t.Fatalf("run %d module %d: declarations %d != first %d", i, j, len(m1.Declarations), len(m2.Declarations))
			}
		}
	}

	// Compare record counts and structure across all runs.
	firstRecords := handoffs[0].Records.Records()
	for i := 1; i < runs; i++ {
		records := handoffs[i].Records.Records()
		if len(records) != len(firstRecords) {
			t.Fatalf("run %d: record count %d != first %d", i, len(records), len(firstRecords))
		}

		// Compare record headers and basic structure.
		for j := range records {
			r1 := records[j]
			r2 := firstRecords[j]
			if r1.Header.ID != r2.Header.ID {
				t.Fatalf("run %d record %d: ID %d != first %d", i, j, r1.Header.ID, r2.Header.ID)
			}
			// Record Kind (determined by which field is non-nil).
			if (r1.Binding != nil) != (r2.Binding != nil) {
				t.Fatalf("run %d record %d: Binding present differs", i, j)
			}
			if (r1.Expression != nil) != (r2.Expression != nil) {
				t.Fatalf("run %d record %d: Expression present differs", i, j)
			}
		}
	}

	// Compare diagnostic items across all runs.
	firstDiags := diagnosticSets[0].Items()
	for i := 1; i < runs; i++ {
		diags := diagnosticSets[i].Items()
		if len(diags) != len(firstDiags) {
			t.Fatalf("run %d: diagnostic count %d != first %d", i, len(diags), len(firstDiags))
		}

		// Compare diagnostic codes and messages in order.
		for j := range diags {
			d1 := diags[j]
			d2 := firstDiags[j]
			if d1.Code != d2.Code {
				t.Fatalf("run %d diagnostic %d: code %s != first %s", i, j, d1.Code, d2.Code)
			}
			if d1.Message != d2.Message {
				t.Fatalf("run %d diagnostic %d: message %q != first %q", i, j, d1.Message, d2.Message)
			}
		}
	}
}

// TestFactDeterminismWithAuditFailure verifies that repeated runs with an audit
// failure (using lowered limits) produce identical diagnostics in the same order.
func TestFactDeterminismWithAuditFailure(t *testing.T) {
	const runs = 8

	var diagnosticSets []*diagnostic.DiagnosticSet

	// Run a program that will exceed limits 8 times.
	for i := 0; i < runs; i++ {
		inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn many_locals() void {
    let a = 1;
    let b = 2;
    let c = 3;
    let d = 4;
    let e = 5;
    let f = 6;
    let g = 7;
    let h = 8;
    let i = 9;
    let j = 10;
    print a; print b; print c; print d; print e;
    print f; print g; print h; print i; print j;
}
`)})

		// Use a very low record limit to trigger an audit failure.
		cfg := Config{MaxSemanticRecords: 5}
		handoff := run06a(inputs, diagnostics, cfg)

		if handoff == nil {
			t.Fatalf("run %d: handoff is nil", i)
		}
		if !handoff.GenerationHadErrors {
			t.Fatalf("run %d: expected GenerationHadErrors=true", i)
		}

		diagnosticSets = append(diagnosticSets, diagnostics)
	}

	// Compare diagnostic items across all runs - they should be identical.
	firstDiags := diagnosticSets[0].Items()
	for i := 1; i < runs; i++ {
		diags := diagnosticSets[i].Items()
		if len(diags) != len(firstDiags) {
			t.Fatalf("run %d: diagnostic count %d != first %d (messages: %v vs %v)",
				i, len(diags), len(firstDiags),
				fmt.Sprintf("%v", diags),
				fmt.Sprintf("%v", firstDiags))
		}

		// Compare diagnostic codes, messages, and primary spans in order.
		for j := range diags {
			d1 := diags[j]
			d2 := firstDiags[j]
			if d1.Code != d2.Code {
				t.Fatalf("run %d diagnostic %d: code %s != first %s", i, j, d1.Code, d2.Code)
			}
			if d1.Message != d2.Message {
				t.Fatalf("run %d diagnostic %d: message %q != first %q", i, j, d1.Message, d2.Message)
			}
			if d1.Primary.Span.Source != d2.Primary.Span.Source || d1.Primary.Span.Start != d2.Primary.Span.Start || d1.Primary.Span.End != d2.Primary.Span.End {
				t.Fatalf("run %d diagnostic %d: span differs", i, j)
			}
		}
	}
}

// typeArgsKey derives a collision-resistant key from one node's TypeArgs so
// distinct specialization type-argument sets can be counted structurally.
func typeArgsKey(typeArgs []types.TypeID) uint64 {
	var key uint64
	for _, ta := range typeArgs {
		key = key*31 + uint64(ta)
	}
	return key
}

// genericDeterminismStats extracts the specialization surface the determinism
// fixture is required to exercise: how many distinct TypeArgs sets appear on
// nodes, whether any set repeats across more than one node, how many
// specialization FunctionDeclaration nodes exist, how many GenericFunctionValue
// nodes exist, and how many instantiation-table entries the unit publishes.
func genericDeterminismStats(unit *tir.Unit) (distinctTypeArgs int, repeatedTypeArgs bool, specializationDecls int, genericFunctionValues int, instantiations int) {
	occurrences := make(map[uint64]int)
	for _, node := range unit.Nodes() {
		if len(node.TypeArgs) == 0 {
			continue
		}
		occurrences[typeArgsKey(node.TypeArgs)]++
		if node.Kind == tir.FunctionDeclaration {
			specializationDecls++
		}
		if node.Kind == tir.GenericFunctionValue {
			genericFunctionValues++
		}
	}
	distinctTypeArgs = len(occurrences)
	for _, count := range occurrences {
		if count > 1 {
			repeatedTypeArgs = true
			break
		}
	}
	return distinctTypeArgs, repeatedTypeArgs, specializationDecls, genericFunctionValues, len(unit.Instantiations())
}

// TestGenericIRDeterminism verifies that repeated full-pipeline checks of the
// same valid generic program produce byte-identical typed-IR output, covering
// specialization declaration order, instantiation order, node IDs, TypeArgs,
// and the source map. The fixture deliberately creates four distinct
// specializations, repeated i32 instantiations at two call sites, and two bare
// generic function values so the instantiation table holds more than one
// ordered entry. i32 and char specialize eagerly through their bare generic
// values; i64 and f64 are reached only through call sites, so they are built
// lazily by buildSpecializations in solver-instantiation order, pinning that
// ordering in the dump too. Block bodies are required throughout: expression
// bodies lower to an empty Block (the pre-existing gap recorded under 07.3f),
// which would leave the specialized bodies empty.
func TestGenericIRDeterminism(t *testing.T) {
	const runs = 10
	const source = `
fn identity[T](value T) T { return value; }

fn main() void {
    let a i32 = identity(1);
    let b i32 = identity(2);
    let c char = identity('x');
    let d i64 = identity(4);
    let e f64 = identity(5.0);
    let f fn(i32) i32 = identity[i32];
    let g fn(char) char = identity[char];
    print a;
    print b;
    print c;
    print d;
    print e;
}
`

	var firstSuccessful bool
	var firstIRNil bool
	var firstDiagnostics []diagnostic.Diagnostic
	var firstDump []byte

	// Structural facts from the first run: a later regression that stops
	// building specializations must fail the test instead of silently
	// comparing identical empty output.
	var firstDistinctTypeArgs, firstSpecializationDecls, firstGenericFunctionValues, firstInstantiations int
	var firstRepeatedTypeArgs bool

	for run := 0; run < runs; run++ {
		inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
		result := Check(inputs, diagnostics, Config{})
		items := diagnostics.Items()
		ir := result.IR()

		if run == 0 {
			firstSuccessful = result.Successful()
			firstIRNil = ir == nil
			firstDiagnostics = append([]diagnostic.Diagnostic(nil), items...)
			if ir != nil {
				firstDump = dumpValidationIR(t, ir)
				firstDistinctTypeArgs, firstRepeatedTypeArgs, firstSpecializationDecls, firstGenericFunctionValues, firstInstantiations = genericDeterminismStats(ir)
			}
			continue
		}

		if result.Successful() != firstSuccessful {
			t.Fatalf("run %d: Successful() = %v, want %v", run, result.Successful(), firstSuccessful)
		}
		if !reflect.DeepEqual(items, firstDiagnostics) {
			t.Fatalf("run %d: diagnostics differ\n got: %+v\nwant: %+v", run, items, firstDiagnostics)
		}
		if (ir == nil) != firstIRNil {
			t.Fatalf("run %d: IR nilness differs: got %v, want %v", run, ir == nil, firstIRNil)
		}
		if ir != nil && !bytes.Equal(dumpValidationIR(t, ir), firstDump) {
			t.Fatalf("run %d: generic typed-IR dump differs from run 0", run)
		}
	}

	if !firstSuccessful || firstIRNil {
		t.Fatalf("generic determinism fixture was rejected: %+v", firstDiagnostics)
	}
	if firstDistinctTypeArgs < 2 {
		t.Fatalf("distinct TypeArgs sets = %d, want at least two distinct specializations", firstDistinctTypeArgs)
	}
	if !firstRepeatedTypeArgs {
		t.Fatal("no TypeArgs set repeats across more than one node: repeated instantiations not exercised")
	}
	if firstSpecializationDecls < 2 {
		t.Fatalf("specialization FunctionDeclaration nodes = %d, want at least two", firstSpecializationDecls)
	}
	if firstGenericFunctionValues < 1 {
		t.Fatalf("GenericFunctionValue nodes = %d, want at least one", firstGenericFunctionValues)
	}
	if firstInstantiations < 2 {
		t.Fatalf("instantiation table entries = %d, want at least two", firstInstantiations)
	}
}
