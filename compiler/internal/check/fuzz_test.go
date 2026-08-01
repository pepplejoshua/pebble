package check

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// FuzzCheck drives the full public checker entry point Check(...) — the entire
// 06a semantic-fact pipeline plus every 06b validator and typed-IR
// construction — instead of stopping at buildUnit like FuzzBuildUnit does. Its
// seed corpus is every real .peb fixture under the three fixture roots that the
// package already exercises (typed-IR construction, plain validation, and
// validation recovery) plus hand-crafted malformed inputs, so valid, invalid,
// and partially-recovered programs are all fuzzed from real bases.
func FuzzCheck(f *testing.F) {
	config := Config{
		MaxSyntaxVisits: 500, MaxTraversalDepth: 64, MaxSemanticRecords: 1000,
		MaxRecordComponents: 1000, MaxControlDepth: 64, MaxTrackedPlaces: 1000,
		MaxGenericRequirements: 1000, MaxConstantDepth: 64, MaxConstantOperations: 2000,
		MaxConstantBits: 2048, MaxDiagnostics: 300, MaxValidationSteps: 2000,
		MaxIRNodes: 2000, MaxIRComponents: 10000, MaxFlowStates: 1000, MaxDeferEdges: 1000,
		MaxDumpBytes: 1 << 20,
	}
	for _, pattern := range []string{
		"../../../tests/check/ir/valid/*.peb",
		"../../../tests/check/validation/valid/*.peb",
		"../../../tests/check/validation/recovery/*.peb",
	} {
		paths, err := filepath.Glob(pattern)
		if err != nil {
			f.Fatal(err)
		}
		for _, path := range paths {
			if contents, err := os.ReadFile(path); err == nil {
				f.Add(contents)
			}
		}
	}
	f.Add([]byte(""))
	f.Add([]byte("fn broken( int { let value = ; }"))
	f.Add([]byte("fn main() void { let x i32 = ;"))
	f.Add([]byte(strings.Repeat("(", 200) + "1" + strings.Repeat(")", 200)))
	f.Add([]byte("fn main() void { if flag { return; }"))
	f.Add([]byte("let global i32;"))
	f.Add([]byte("fn main() void { while true { continue; }"))
	f.Fuzz(func(t *testing.T, contents []byte) {
		if len(contents) > 512 {
			return
		}
		// Same shape as FuzzBuildUnit: run each case on its own goroutine with
		// a hard deadline. The full Check pipeline runs earlier phases
		// (parsing, symbol resolution, generation) that are out of this
		// package's scope and may have their own unbounded-loop bugs on
		// malformed input, and a hang inside f.Fuzz would both hang go test and
		// get saved to testdata/fuzz/ to replay-hang every future run.
		done := make(chan struct{})
		var panicked any
		go func() {
			defer close(done)
			defer func() {
				panicked = recover()
			}()
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
			result := Check(inputs, diagnostics, config)
			unit := result.IR()
			if unit == nil {
				return
			}
			var dump bytes.Buffer
			if err := unit.Dump(&dump); err != nil {
				t.Errorf("Check succeeded but IR dump failed: %v", err)
				return
			}
			for _, ref := range unit.SourceRefs() {
				_, _ = result.Expression(ref)
				_, _ = result.Place(ref)
				_, _ = result.Conversion(ref)
				_, _ = result.Call(ref)
				_, _ = result.Member(ref)
				_, _ = result.Control(ref)
			}
		}()
		select {
		case <-done:
			if panicked != nil {
				t.Fatalf("Check panicked: %v", panicked)
			}
		case <-time.After(5 * time.Second):
			t.Fatalf("Check hung (likely an upstream parsing/generation bug, not 06b) on input: %q", contents)
		}
	})
}
