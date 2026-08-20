package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

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
	result      *check.Result
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
		result:      result,
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
//
// The rendered text is richer than a bare type name: when the hovered position
// sits on a symbol's name or reference (a binding, parameter, field, variant,
// function, or type declaration), the text reports what KIND of thing it is
// plus its type, in gopls style ("var x: i32", "param p: str", "fn f(...) R",
// "field f: i32", "type Color"). Otherwise it falls back to the plain type
// description of the expression or literal at the position.
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

	snap := p.unit.Snapshot()
	resolve := types.ResolveFromResult(p.resolution)

	// A symbol-typed hover: resolve the position to a declaration or reference
	// symbol when possible (a parameter name, a var binding name, a field, a
	// variant, a function, or a type declaration), and render kind + type. This
	// covers both later references (via the resolution reference table) and a
	// declaration's OWN name (via the symbol's span matching the node).
	if sym, ok := symbolForNode(p, entryMod.ID, best); ok {
		if text, ok := renderSymbolHover(p, sym, resolve); ok {
			return text
		}
	}

	// Fall back to the typed-IR source map: map the surface node to its typed
	// IR node and describe the expression/literal's type.
	tirID, ok := p.unit.SourceMap(symbol.SyntaxRef{Module: entryMod.ID, Node: best})
	if !ok {
		return ""
	}
	node, ok := p.unit.Node(tirID)
	if !ok || node.Type == 0 {
		return ""
	}
	if snap == nil {
		return ""
	}
	key, ok := snap.Key(node.Type)
	if !ok {
		return ""
	}
	return types.DescribeKeyResolved(key, types.LookupFromSnapshot(snap), resolve)
}

// symbolForNode resolves the syntax node to a declaration symbol. It first
// checks the resolution reference table (a later USE of a name), then falls
// back to scanning the symbol store for a declaration whose name span matches
// the node exactly (a declaration's own name identifier).
func symbolForNode(p *compiledProgram, modID module.ModuleID, nodeID syntax.NodeID) (symbol.Symbol, bool) {
	ref := symbol.SyntaxRef{Module: modID, Node: nodeID}
	if res, ok := p.resolution.Reference(ref); ok && res.Symbol != 0 && res.State == symbol.ResolutionResolved {
		if sym, ok := p.resolution.Symbols.Symbol(res.Symbol); ok {
			return sym, true
		}
	}
	node, ok := p.sourcesNode(modID, nodeID)
	if !ok {
		return symbol.Symbol{}, false
	}
	nspan := node.Span()
	for _, candidate := range p.resolution.Symbols.All() {
		if candidate.Module != modID {
			continue
		}
		if candidate.Span.Source == nspan.Source && candidate.Span.Start == nspan.Start && candidate.Span.End == nspan.End {
			return candidate, true
		}
	}
	return symbol.Symbol{}, false
}

// renderSymbolHover produces the kind-and-type hover text for a symbol. The
// second return value reports whether a meaningful hover was produced.
func renderSymbolHover(p *compiledProgram, sym symbol.Symbol, resolve func(symbol.SymbolID) string) (string, bool) {
	name := sym.Name
	snap := p.unit.Snapshot()

	// Type declarations (struct/union/enum/extern/type parameter) render as
	// "type Name" without needing a value type.
	if sym.Kind == symbol.SymbolType || sym.Kind == symbol.SymbolExternType || sym.Kind == symbol.SymbolRuntimeType || sym.Kind == symbol.SymbolBuiltinType {
		if name == "" {
			return "", false
		}
		return "type " + name, true
	}
	if sym.Kind == symbol.SymbolTypeParameter {
		if name == "" {
			return "", false
		}
		return "type parameter " + name, true
	}

	// Resolve the symbol's own type from the solved semantic state.
	typeResult, ok := p.result.SymbolType(sym.ID)
	if !ok || typeResult.Type == 0 {
		return "", false
	}
	if snap == nil {
		return "", false
	}
	key, keyOK := snap.Key(typeResult.Type)
	if !keyOK {
		return "", false
	}
	lookup := types.LookupFromSnapshot(snap)
	typ := types.DescribeKeyResolved(key, lookup, resolve)

	switch sym.Kind {
	case symbol.SymbolBinding:
		if name == "" {
			return "", false
		}
		return bindingKeyword(p, sym) + " " + name + ": " + typ, true
	case symbol.SymbolExternBinding, symbol.SymbolLoopBinding:
		if name == "" {
			return "", false
		}
		return "var " + name + ": " + typ, true
	case symbol.SymbolParameter:
		if name == "" {
			return "", false
		}
		return "param " + name + ": " + typ, true
	case symbol.SymbolField:
		if name == "" {
			return "", false
		}
		return "field " + name + ": " + typ, true
	case symbol.SymbolVariant:
		owner := ""
		if resolve != nil && sym.Containing != 0 {
			owner = resolve(sym.Containing)
		}
		if owner == "" {
			owner = "<type>"
		}
		if name == "" {
			return "", false
		}
		return owner + "." + name, true
	case symbol.SymbolFunction, symbol.SymbolMethod, symbol.SymbolExternFunction, symbol.SymbolBuiltinFunction:
		return renderFunctionHover(name, key, snap, resolve), true
	default:
		// Unknown symbol kinds degrade to the bare type description.
		return typ, true
	}
}

// renderFunctionHover renders a function symbol's full signature in the form
// "fn name(p1 T1, p2 T2) R" from its function type key.
func renderFunctionHover(name string, key types.TypeKey, snap *types.Snapshot, resolve func(symbol.SymbolID) string) string {
	_, parameters, result, _, _ := key.Function()
	lookup := types.LookupFromSnapshot(snap)
	params := make([]string, len(parameters))
	for i, parameter := range parameters {
		params[i] = describeTypeID(lookup, parameter, resolve)
	}
	prefix := "fn "
	if name != "" {
		prefix += name
	}
	return prefix + "(" + strings.Join(params, ", ") + ") " + describeTypeID(lookup, result, resolve)
}

// describeTypeID describes a store type ID through the snapshot, resolving
// nominal and type-parameter names via resolve.
func describeTypeID(lookup func(types.TypeID) (types.TypeKey, bool), id types.TypeID, resolve func(symbol.SymbolID) string) string {
	if lookup == nil {
		return "<type>"
	}
	key, ok := lookup(id)
	if !ok {
		return "<type>"
	}
	return types.DescribeKeyResolved(key, lookup, resolve)
}

// sourcesNode looks up a syntax node in the entry module's tree.
func (p *compiledProgram) sourcesNode(modID module.ModuleID, nodeID syntax.NodeID) (syntax.Node, bool) {
	m, ok := p.graph.Module(modID)
	if !ok || m.Tree == nil {
		return syntax.Node{}, false
	}
	return m.Tree.Node(nodeID)
}

// bindingKeyword reports the source keyword ("let" or "var") a SymbolBinding
// was actually declared with. `let` and `var` bindings share the single
// SymbolBinding kind -- the resolved symbol table has no mutability field --
// so the distinction only survives in the syntax tree: sym.Declaration points
// at the BindingDecl node, whose Token() is KwLet or KwVar. Defaults to "var"
// if the declaration node can't be found, matching prior behavior.
func bindingKeyword(p *compiledProgram, sym symbol.Symbol) string {
	node, ok := p.sourcesNode(sym.Declaration.Module, sym.Declaration.Node)
	if !ok {
		return "var"
	}
	if node.Token() == syntax.KwLet {
		return "let"
	}
	return "var"
}
