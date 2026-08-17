// Package bench profiles the full pebc compilation pipeline, stage by stage,
// so `go test -bench . -cpuprofile ...` can show where the compiler spends
// time and allocations.
package bench

import (
	"io"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/backend"
	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/stdlib"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// fixturePath is the benchmark entry program. go test runs each test binary
// with the working directory set to the package directory, so the relative
// testdata/ path always resolves.
const fixturePath = "testdata/stdlib_pipeline.peb"

// pipeline carries the one-time benchmark setup shared by every benchmark:
// the stdlib-wrapping source provider and the fixed build/check configuration.
//
// The per-iteration scratch state (source.FileSet and diagnostic.DiagnosticSet)
// is deliberately constructed fresh inside every loop body, not hoisted:
// module.Build appends files to the FileSet and diagnostics to the
// DiagnosticSet, symbol.Resolve and check.Check append diagnostics too, and
// the graph's source IDs index the FileSet it was built against. None of the
// pipeline stages are designed to be called twice on the same FileSet or
// DiagnosticSet, so a reused set would silently corrupt IDs or accumulate
// stale diagnostics across iterations.
type pipeline struct {
	provider    module.SourceProvider
	buildConfig module.BuildConfig
	checkConfig check.Config
}

// built carries one front-end run's values. Fields beyond the last completed
// stage are left nil. Each run uses fresh sources/diagnostics (see pipeline).
type built struct {
	sources     *source.FileSet
	diagnostics *diagnostic.DiagnosticSet
	graph       *module.Graph
	resolution  *symbol.Result
	store       *types.Store
	result      *check.Result
	entryID     symbol.SymbolID
}

func newPipeline(b *testing.B) *pipeline {
	provider := stdlib.New(module.FileSystemProvider{})
	entry, err := provider.Canonicalize(fixturePath)
	if err != nil {
		b.Fatalf("bench: cannot canonicalize fixture: %v", err)
	}
	return &pipeline{
		provider: provider,
		buildConfig: module.BuildConfig{
			EntryPath:    string(entry),
			Package:      "main",
			StandardRoot: stdlib.StandardRoot,
		},
		checkConfig: check.Config{Entry: check.EntryPoint{Mode: check.EntryRequired}},
	}
}

// build runs stage 2 alone: module graph construction, which reads and
// lexes/parses every transitively imported file, including the whole stdlib
// closure reachable from the entry program.
func (p *pipeline) build(b *testing.B) *built {
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := module.Build(p.buildConfig, p.provider, sources, diagnostics)
	if diagnostics.HasErrors() || graph.Len() == 0 {
		b.Fatal("bench: module build produced errors")
	}
	return &built{sources: sources, diagnostics: diagnostics, graph: graph}
}

// resolve runs stages 2-3. Stage 3 alone is meaningless without a graph as
// input, so the reported number is the combined build+resolve cost; the delta
// against BenchmarkModuleBuild isolates resolution.
func (p *pipeline) resolve(b *testing.B) *built {
	built := p.build(b)
	built.resolution = symbol.Resolve(built.graph, built.sources, built.diagnostics, symbol.Config{})
	if built.diagnostics.HasErrors() {
		b.Fatal("bench: symbol resolution produced errors")
	}
	return built
}

// check runs stages 2-5: module build, symbol resolution, type-store
// initialization, and the checker (fact generation, inference solve, and
// typed-IR construction).
func (p *pipeline) check(b *testing.B) *built {
	built := p.resolve(b)
	store, err := types.New(types.Config{})
	if err != nil {
		b.Fatalf("bench: cannot initialize type store: %v", err)
	}
	built.store = store
	built.entryID = mainEntryID(b, built.resolution)
	inputs := check.Inputs{
		Graph: built.graph, Sources: built.sources, Resolution: built.resolution, Types: store,
		LiteralTarget: infer.LiteralTarget{WordBits: 64},
	}
	config := p.checkConfig
	config.Entry.Symbol = built.entryID
	built.result = check.Check(inputs, built.diagnostics, config)
	if !built.result.Successful() || built.diagnostics.HasErrors() {
		b.Fatal("bench: type check produced errors")
	}
	return built
}

// mainEntryID finds the "main" symbol exactly the way pebc does: scanning the
// resolution result's symbols for a symbol named "main".
func mainEntryID(b *testing.B, resolution *symbol.Result) symbol.SymbolID {
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			return candidate.ID
		}
	}
	b.Fatal("bench: no main function found in fixture")
	return 0
}

// BenchmarkModuleBuild times stage 2 alone.
func BenchmarkModuleBuild(b *testing.B) {
	p := newPipeline(b)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p.build(b)
	}
}

// BenchmarkSymbolResolve times stages 2-3. Stage 3 alone is meaningless
// without a graph as input, so this reports the combined number.
func BenchmarkSymbolResolve(b *testing.B) {
	p := newPipeline(b)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p.resolve(b)
	}
}

// BenchmarkCheck times stages 2-5.
func BenchmarkCheck(b *testing.B) {
	p := newPipeline(b)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p.check(b)
	}
}

// BenchmarkEmit times the full pipeline including stage 7 (C code
// generation). Output is written to io.Discard; we do not care about the
// resulting file.
func BenchmarkEmit(b *testing.B) {
	p := newPipeline(b)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		built := p.check(b)
		unit := built.result.IR()
		if unit == nil {
			b.Fatal("bench: checker returned no typed IR")
		}
		if err := backend.Emit(unit, unit.Snapshot(), built.entryID, built.sources, built.resolution, io.Discard); err != nil {
			b.Fatalf("bench: emission failed: %v", err)
		}
	}
}

// BenchmarkFullPipeline is the whole compilation end to end, matching exactly
// what a real `pebc -check` / `pebc -o ...` build does (including the entry
// symbol scan), without the external cc link step.
func BenchmarkFullPipeline(b *testing.B) {
	p := newPipeline(b)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		built := p.check(b)
		unit := built.result.IR()
		if unit == nil {
			b.Fatal("bench: checker returned no typed IR")
		}
		if err := backend.Emit(unit, unit.Snapshot(), built.entryID, built.sources, built.resolution, io.Discard); err != nil {
			b.Fatalf("bench: emission failed: %v", err)
		}
	}
}
