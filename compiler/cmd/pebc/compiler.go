package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/pepplejoshua/pebble/compiler/internal/backend"
	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/stdlib"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// compileMode selects which pipeline stages run for one compilation.
type compileMode uint8

const (
	// modeBuild checks, emits C, and links an executable.
	modeBuild compileMode = iota
	// modeCheck checks the program only and emits nothing.
	modeCheck
	// modeEmitC checks and writes the generated C to a path, then stops.
	modeEmitC
)

// compileRequest captures every pipeline parameter for one compilation. It is
// deliberately buildable both from the one-shot CLI flags and from a daemon
// build request, so both paths share the exact same pipeline implementation.
type compileRequest struct {
	mode         compileMode
	entryPath    string
	outputPath   string
	emitCPath    string
	prelude      string
	runtimeRoot  string
	release      bool
	linkLibs     []string
	linkPaths    []string
	includePaths []string
	// trackFiles, when true, makes compileOnce populate result.files with the
	// source paths of the resolved module graph (for the daemon's watcher).
	trackFiles bool
	stderr     io.Writer
}

// compileResult is the outcome of one pipeline run.
type compileResult struct {
	code        int
	binaryPath  string
	diagnostics string
	// files is the set of source file paths that this compilation actually
	// loaded (the resolved module graph). It lets the daemon track exactly
	// the files that participated in a build rather than every *.peb under
	// the project root. Populated only when req.trackFiles is set.
	files []string
	// structuredDiagnostics carries machine-readable diagnostics (file +
	// 1-based line/column ranges) for the build, populated for both success
	// (empty slice) and failure (populated) paths. Never nil so the daemon
	// JSON-encodes [] rather than null.
	structuredDiagnostics []structuredDiagnostic
}

// compiledProgram is the shared output of the pipeline's front half: module
// discovery, name resolution, and type checking. Both the build (compileOnce)
// and the read-only hover query (hoverTypeAtOffset) consume it; the build
// continues into C emission and linking, while hover only needs the typed IR
// and source graph to resolve a position to a type.
type compiledProgram struct {
	graph       *module.Graph
	sources     *source.FileSet
	resolution  *symbol.Result
	store       *types.Store
	unit        *tir.Unit
	diagnostics *diagnostic.DiagnosticSet
	entryID     symbol.SymbolID
	// graphFiles is the set of source file paths that this compilation
	// actually loaded (the resolved module graph). Populated only when
	// req.trackFiles is set.
	graphFiles []string
}

// buildProgram runs module graph build, name resolution, and type checking for
// one entry file, sharing the exact front half of the compilation between the
// build path and the read-only hover query. It does NOT emit C or link. The
// returned bool reports a fatal/infrastructure failure (entry unresolvable,
// type store init failure, or no main function): in that case the program is
// nil and the caller should treat the compilation as failed. A program with
// ordinary type errors still returns (non-nil, false); its unit field is nil in
// that case (typed IR is only published on a fully successful check), which the
// hover query reads as "no type here".
func buildProgram(req compileRequest) (*compiledProgram, bool) {
	if req.stderr == nil {
		req.stderr = io.Discard
	}
	provider := stdlib.New(module.FileSystemProvider{})
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	entryPath, err := provider.Canonicalize(req.entryPath)
	if err != nil {
		fmt.Fprintf(req.stderr, "pebc: cannot resolve entry %q: %v\n", req.entryPath, err)
		return nil, true
	}
	graph := module.Build(module.BuildConfig{EntryPath: string(entryPath), Package: "main", PreludePath: req.prelude, StandardRoot: stdlib.StandardRoot}, provider, sources, diagnostics)
	var graphFiles []string
	if req.trackFiles {
		seen := map[source.ID]bool{}
		for i := 1; i <= graph.Len(); i++ {
			m, ok := graph.Module(module.ModuleID(i))
			if !ok {
				continue
			}
			if seen[m.Source] {
				continue
			}
			seen[m.Source] = true
			if f, ok := sources.File(m.Source); ok {
				graphFiles = append(graphFiles, f.Path())
			}
		}
	}
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		fmt.Fprintf(req.stderr, "pebc: cannot initialize type store: %v\n", err)
		return nil, true
	}

	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entryID = candidate.ID
			break
		}
	}
	if entryID == 0 {
		if diagnostics.Len() > 0 {
			_ = diagnostic.RenderText(req.stderr, sources, diagnostics.Items())
		}
		fmt.Fprintln(req.stderr, "pebc: no main function found")
		return nil, true
	}

	inputs := check.Inputs{
		Graph: graph, Sources: sources, Resolution: resolution, Types: store,
		LiteralTarget: infer.LiteralTarget{WordBits: 64},
	}
	result := check.Check(inputs, diagnostics, check.Config{
		Entry: check.EntryPoint{Mode: check.EntryRequired, Symbol: entryID},
	})
	return &compiledProgram{
		graph:       graph,
		sources:     sources,
		resolution:  resolution,
		store:       store,
		unit:        result.IR(),
		diagnostics: diagnostics,
		entryID:     entryID,
		graphFiles:  graphFiles,
	}, false
}

// compileOnce runs the full pebc pipeline (module discovery, name resolution,
// type checking, C emission, and linking) for one entry file, constructing
// fresh state for the compilation exactly as the historical one-shot CLI did.
// It returns the process exit code and, on success, the path of the built
// executable. Every caller builds fresh state; nothing here is incremental.
func compileOnce(req compileRequest) *compileResult {
	p, fatal := buildProgram(req)
	if fatal || p == nil {
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
	}
	if p.unit == nil {
		var rendered stringsBuilder
		_ = diagnostic.RenderText(&rendered, p.sources, p.diagnostics.Items())
		_, _ = io.WriteString(req.stderr, rendered.String())
		return &compileResult{code: 1, diagnostics: rendered.String(), structuredDiagnostics: buildStructuredDiagnostics(p.diagnostics.Items(), p.sources)}
	}
	if req.mode == modeCheck {
		return &compileResult{code: 0, files: p.graphFiles, structuredDiagnostics: []structuredDiagnostic{}}
	}
	unit := p.unit
	emitPath := req.emitCPath
	if req.mode != modeEmitC {
		dir, err := os.MkdirTemp("", "pebc-emit-")
		if err != nil {
			fmt.Fprintf(req.stderr, "pebc: cannot create temp dir: %v\n", err)
			return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
		}
		defer os.RemoveAll(dir)
		emitPath = filepath.Join(dir, "program.c")
	}
	file, err := os.Create(emitPath)
	if err != nil {
		fmt.Fprintf(req.stderr, "pebc: cannot create output %q: %v\n", emitPath, err)
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
	}
	if err := backend.Emit(unit, unit.Snapshot(), p.entryID, p.sources, p.resolution, file); err != nil {
		file.Close()
		fmt.Fprintf(req.stderr, "pebc: emission failed: %v\n", err)
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
	}
	if err := file.Close(); err != nil {
		fmt.Fprintf(req.stderr, "pebc: cannot close output %q: %v\n", emitPath, err)
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
	}
	if req.mode == modeEmitC {
		return &compileResult{code: 0, files: p.graphFiles, structuredDiagnostics: []structuredDiagnostic{}}
	}

	binaryPath := req.outputPath
	if binaryPath == "" {
		binaryPath = defaultBinaryPath(req.entryPath)
	}
	if code := buildExecutable(req.runtimeRoot, req.release, emitPath, binaryPath, req.linkLibs, req.linkPaths, req.includePaths, req.stderr); code != 0 {
		return &compileResult{code: code, files: p.graphFiles, structuredDiagnostics: []structuredDiagnostic{}}
	}
	return &compileResult{code: 0, binaryPath: binaryPath, files: p.graphFiles, structuredDiagnostics: []structuredDiagnostic{}}
}

// stringsBuilder is a minimal thread-free string accumulator used to capture
// diagnostics without pulling in a large dependency.
type stringsBuilder struct {
	buf []byte
}

func (b *stringsBuilder) Write(p []byte) (int, error) {
	b.buf = append(b.buf, p...)
	return len(p), nil
}

func (b *stringsBuilder) String() string { return string(b.buf) }

// buildStructuredDiagnostics converts the compiler's diagnostic set into the
// machine-readable form the daemon ships over its build RPC. Each diagnostic's
// primary span is resolved through the FileSet to a file path and a 1-based
// start/end line/column. The result is never nil, so the daemon encodes an
// empty JSON array on a clean build rather than null.
func buildStructuredDiagnostics(diags []diagnostic.Diagnostic, sources *source.FileSet) []structuredDiagnostic {
	out := make([]structuredDiagnostic, 0, len(diags))
	for _, d := range diags {
		span := d.Primary.Span
		file, ok := sources.File(span.Source)
		if !ok {
			continue
		}
		start := file.Position(span.Start)
		end := file.Position(span.End)
		out = append(out, structuredDiagnostic{
			File:      file.Path(),
			StartLine: start.Line,
			StartCol:  start.Column,
			EndLine:   end.Line,
			EndCol:    end.Column,
			Severity:  d.Severity.String(),
			Code:      string(d.Code),
			Message:   d.Message,
		})
	}
	return out
}

// hoverTypeAtOffset performs a fresh full check of the entry module (the same
// daemon build path a build uses -- there is no warm checked state to query
// yet) and returns the rendered checked type at the given byte offset within
// the entry file. It returns "" when no type information is available at that
// position (e.g. hovering whitespace or a keyword), not an error. The offset is
// a byte count into the entry file's source as the daemon reads it from disk.
func hoverTypeAtOffset(entryPath string, offset uint32) string {
	p, fatal := buildProgram(compileRequest{entryPath: entryPath, stderr: io.Discard})
	if fatal || p == nil || p.unit == nil {
		return ""
	}
	// The entry module is Graph.Root (EntryPath becomes Root during graph
	// build), regardless of any separate prelude module.
	entryMod, ok := p.graph.Module(p.graph.Root)
	if !ok || entryMod.Tree == nil {
		return ""
	}
	tree := entryMod.Tree

	// Linear scan for the smallest (most specific) node whose span contains
	// the offset. A larger enclosing node (e.g. the whole call expression, or
	// the File root) is never preferred over the name/literal inside it.
	var best syntax.NodeID
	var bestWidth uint32
	found := false
	for id := syntax.NodeID(1); uint64(id) <= uint64(tree.Root()); id++ {
		n, ok := tree.Node(id)
		if !ok {
			continue
		}
		kind := n.Kind()
		// The File root, recovery, and EOF nodes span huge or meaningless
		// ranges and are never useful hover targets.
		if kind == syntax.File || kind == syntax.Error || kind == syntax.Missing || kind == syntax.EndOfFile {
			continue
		}
		span := n.Span()
		if offset < span.Start || offset > span.End {
			continue
		}
		width := span.End - span.Start
		if !found || width < bestWidth {
			best = id
			bestWidth = width
			found = true
		}
	}
	if !found {
		return ""
	}

	// Map the surface node to its typed-IR node via the unit's source map.
	tirID, ok := p.unit.SourceMap(symbol.SyntaxRef{Module: entryMod.ID, Node: best})
	if !ok {
		return ""
	}
	node, ok := p.unit.Node(tirID)
	if !ok || node.Type == 0 {
		return ""
	}
	snap := p.unit.Snapshot()
	if snap == nil {
		return ""
	}
	key, ok := snap.Key(node.Type)
	if !ok {
		return ""
	}
	return types.DescribeKey(key)
}
