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
	stderr       io.Writer
}

// compileResult is the outcome of one pipeline run.
type compileResult struct {
	code        int
	binaryPath  string
	diagnostics string
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
		return &compileResult{code: 1}
	}
	graph := module.Build(module.BuildConfig{EntryPath: string(entryPath), Package: "main", PreludePath: req.prelude, StandardRoot: stdlib.StandardRoot}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		fmt.Fprintf(req.stderr, "pebc: cannot initialize type store: %v\n", err)
		return &compileResult{code: 1}
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
		return &compileResult{code: 1}
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
		return &compileResult{code: 1, diagnostics: rendered.String()}
	}
	if req.mode == modeCheck {
		return &compileResult{code: 0}
	}
	unit := result.IR()
	if unit == nil {
		fmt.Fprintln(req.stderr, "pebc: internal error: checker returned no typed IR")
		return &compileResult{code: 1}
	}

	emitPath := req.emitCPath
	if req.mode == modeEmitC {
		// emitCPath is authoritative for modeEmitC.
	} else {
		dir, err := os.MkdirTemp("", "pebc-emit-")
		if err != nil {
			fmt.Fprintf(req.stderr, "pebc: cannot create temp dir: %v\n", err)
			return &compileResult{code: 1}
		}
		defer os.RemoveAll(dir)
		emitPath = filepath.Join(dir, "program.c")
	}
	file, err := os.Create(emitPath)
	if err != nil {
		fmt.Fprintf(req.stderr, "pebc: cannot create output %q: %v\n", emitPath, err)
		return &compileResult{code: 1}
	}
	if err := backend.Emit(unit, unit.Snapshot(), entryID, sources, resolution, file); err != nil {
		file.Close()
		fmt.Fprintf(req.stderr, "pebc: emission failed: %v\n", err)
		return &compileResult{code: 1}
	}
	if err := file.Close(); err != nil {
		fmt.Fprintf(req.stderr, "pebc: cannot close output %q: %v\n", emitPath, err)
		return &compileResult{code: 1}
	}
	if req.mode == modeEmitC {
		return &compileResult{code: 0}
	}

	binaryPath := req.outputPath
	if binaryPath == "" {
		binaryPath = defaultBinaryPath(string(entryPath))
	}
	if code := buildExecutable(req.runtimeRoot, req.release, emitPath, binaryPath, req.linkLibs, req.linkPaths, req.includePaths, req.stderr); code != 0 {
		return &compileResult{code: code}
	}
	return &compileResult{code: 0, binaryPath: binaryPath}
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
