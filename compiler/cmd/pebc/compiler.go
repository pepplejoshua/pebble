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

// compileOnce runs the full pebc pipeline (module discovery, name resolution,
// type checking, C emission, and linking) for one entry file, constructing
// fresh state for the compilation exactly as the historical one-shot CLI did.
// It returns the process exit code and, on success, the path of the built
// executable. Every caller builds fresh state; nothing here is incremental.
func compileOnce(req compileRequest) *compileResult {
	if req.stderr == nil {
		req.stderr = io.Discard
	}
	provider := stdlib.New(module.FileSystemProvider{})
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	entryPath, err := provider.Canonicalize(req.entryPath)
	if err != nil {
		fmt.Fprintf(req.stderr, "pebc: cannot resolve entry %q: %v\n", req.entryPath, err)
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
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
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
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
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
	}

	inputs := check.Inputs{
		Graph: graph, Sources: sources, Resolution: resolution, Types: store,
		LiteralTarget: infer.LiteralTarget{WordBits: 64},
	}
	result := check.Check(inputs, diagnostics, check.Config{
		Entry: check.EntryPoint{Mode: check.EntryRequired, Symbol: entryID},
	})
	if !result.Successful() || diagnostics.Len() > 0 {
		var rendered stringsBuilder
		_ = diagnostic.RenderText(&rendered, sources, diagnostics.Items())
		_, _ = io.WriteString(req.stderr, rendered.String())
		return &compileResult{code: 1, diagnostics: rendered.String(), structuredDiagnostics: buildStructuredDiagnostics(diagnostics.Items(), sources)}
	}
	if req.mode == modeCheck {
		return &compileResult{code: 0, files: graphFiles, structuredDiagnostics: []structuredDiagnostic{}}
	}
	unit := result.IR()
	if unit == nil {
		fmt.Fprintln(req.stderr, "pebc: internal error: checker returned no typed IR")
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
	}

	emitPath := req.emitCPath
	if req.mode == modeEmitC {
		// emitCPath is authoritative for modeEmitC.
	} else {
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
	if err := backend.Emit(unit, unit.Snapshot(), entryID, sources, resolution, file); err != nil {
		file.Close()
		fmt.Fprintf(req.stderr, "pebc: emission failed: %v\n", err)
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
	}
	if err := file.Close(); err != nil {
		fmt.Fprintf(req.stderr, "pebc: cannot close output %q: %v\n", emitPath, err)
		return &compileResult{code: 1, structuredDiagnostics: []structuredDiagnostic{}}
	}
	if req.mode == modeEmitC {
		return &compileResult{code: 0, files: graphFiles, structuredDiagnostics: []structuredDiagnostic{}}
	}

	binaryPath := req.outputPath
	if binaryPath == "" {
		binaryPath = defaultBinaryPath(string(entryPath))
	}
	if code := buildExecutable(req.runtimeRoot, req.release, emitPath, binaryPath, req.linkLibs, req.linkPaths, req.includePaths, req.stderr); code != 0 {
		return &compileResult{code: code, files: graphFiles, structuredDiagnostics: []structuredDiagnostic{}}
	}
	return &compileResult{code: 0, binaryPath: binaryPath, files: graphFiles, structuredDiagnostics: []structuredDiagnostic{}}
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
