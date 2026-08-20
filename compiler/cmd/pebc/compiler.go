package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
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
	"go.lsp.dev/protocol"
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
	// Resolve a real on-disk std/ directory (binary-relative, then cwd-walk-up)
	// so actual compilation reads std: imports from disk when one is available,
	// not just the go:embed copy. An empty result on the error case means "no
	// real root found, use the embed".
	stdRoot, _ := locateStdRoot()
	provider := stdlib.New(module.FileSystemProvider{}, stdRoot)
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
	// A build/emit-c needs a real, linkable entry point; modeCheck (and every
	// read-only LSP query, which uses modeCheck too) only needs the program to
	// check, so a library file with no main is legitimate there -- the checker
	// already supports this via check.EntryNone, buildProgram just never used
	// to give it the chance. Confirmed live: hovering/completing inside a real
	// compiler/std/*.peb file (none of which declare main) previously failed
	// outright with "no main function found" before this existed.
	entryMode := check.EntryRequired
	if entryID == 0 {
		if req.mode != modeCheck {
			if diagnostics.Len() > 0 {
				_ = diagnostic.RenderText(req.stderr, sources, diagnostics.Items())
			}
			fmt.Fprintln(req.stderr, "pebc: no main function found")
			return nil, true
		}
		entryMode = check.EntryNone
	}

	inputs := check.Inputs{
		Graph: graph, Sources: sources, Resolution: resolution, Types: store,
		LiteralTarget: infer.LiteralTarget{WordBits: 64},
	}
	result := check.Check(inputs, diagnostics, check.Config{
		Entry:                         check.EntryPoint{Mode: entryMode, Symbol: entryID},
		AllowPartialOnRecoveredErrors: req.mode == modeCheck,
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
// plus its type, in gopls style ("var x i32", "param p str", "fn f(...) R",
// "field f i32", "type Color"). Otherwise it falls back to the plain type
// description of the expression or literal at the position.
func hoverTypeAtOffset(entryPath string, offset uint32) string {
	p, fatal := buildProgram(compileRequest{mode: modeCheck, entryPath: entryPath, stderr: io.Discard})
	if fatal || p == nil {
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

	resolve := types.ResolveFromResultQualified(p.resolution, entryMod.ID, types.QualifierMap(entryMod.Imports))

	// A symbol-typed hover: resolve the position to a declaration or reference
	// symbol when possible (a parameter name, a var binding name, a field, a
	// variant, a function, or a type declaration), and render kind + type. This
	// covers both later references (via the resolution reference table) and a
	// declaration's OWN name (via the symbol's span matching the node). This
	// path needs no typed-IR unit, so it works even when the file has an
	// unrelated error elsewhere.
	if sym, ok := symbolForNode(p, entryMod.ID, best); ok {
		if text, ok := renderSymbolHover(p, sym, resolve); ok {
			return text
		}
	}

	// Fall back to the typed-IR source map: map the surface node to its typed
	// IR node and describe the expression/literal's type. This genuinely needs
	// the full typed-IR unit (its source map only exists once the whole unit is
	// built), so it is the LAST resort after the symbol-based paths above have
	// already been tried.
	if p.unit == nil {
		return ""
	}
	snap := p.unit.Snapshot()
	tirID, ok := p.unit.SourceMap(symbol.SyntaxRef{Module: entryMod.ID, Node: best})
	if !ok {
		// The typed-IR source map is keyed on the whole MemberExpr node, not
		// on the narrow member-name child. When hovering exactly on the name
		// token, best resolves to that Name node and misses. Widen once to the
		// direct parent MemberExpr when best is its second child.
		var parentID syntax.NodeID
		foundParent := false
		for id := syntax.NodeID(1); uint64(id) <= uint64(tree.Root()); id++ {
			n, ok := tree.Node(id)
			if !ok {
				continue
			}
			if n.Kind() != syntax.MemberExpr {
				continue
			}
			children := n.Children()
			if len(children) >= 2 && children[1] == best {
				parentID = id
				foundParent = true
				break
			}
		}
		if !foundParent {
			return ""
		}
		tirID, ok = p.unit.SourceMap(symbol.SyntaxRef{Module: entryMod.ID, Node: parentID})
		if !ok {
			return ""
		}
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

// definitionAtOffset performs a fresh full check of the entry module (the same
// daemon build path a build uses -- there is no warm checked state to query
// yet) and resolves the syntax node at the given byte offset to its
// declaration's location. It returns a structuredDefinition describing the
// target file path and the 1-based line/column range of the declaration's
// NAME span -- the symbol's own name node, tight and name-only, not the whole
// declaration statement. A zero File reports "no definition": the position
// sits on something with no resolvable declaration (a literal, whitespace, a
// keyword, or a compiler-owned builtin with no source span). Requesting a
// definition ON a declaration's own name resolves to that same declaration
// (standard LSP behavior: jumping to a symbol you're already on is a no-op
// navigation, not an error).
func definitionAtOffset(entryPath string, offset uint32) structuredDefinition {
	p, fatal := buildProgram(compileRequest{mode: modeCheck, entryPath: entryPath, stderr: io.Discard})
	if fatal || p == nil {
		return structuredDefinition{}
	}
	// The entry module is Graph.Root (EntryPath becomes Root during graph
	// build), regardless of any separate prelude module.
	entryMod, ok := p.graph.Module(p.graph.Root)
	if !ok || entryMod.Tree == nil {
		return structuredDefinition{}
	}
	tree := entryMod.Tree

	// Linear scan for the smallest (most specific) node whose span contains
	// the offset -- the exact pattern and exclusions hoverTypeAtOffset uses.
	var best syntax.NodeID
	var bestWidth uint32
	found := false
	for id := syntax.NodeID(1); uint64(id) <= uint64(tree.Root()); id++ {
		n, ok := tree.Node(id)
		if !ok {
			continue
		}
		kind := n.Kind()
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
		return structuredDefinition{}
	}

	// Resolve the position to a declaration or reference symbol. Both a later
	// USE of a name (via the resolution reference table) and a declaration's
	// OWN name (via a span match against the symbol store) resolve here.
	sym, ok := symbolForNode(p, entryMod.ID, best)
	if !ok {
		return structuredDefinition{}
	}
	return symbolDefinition(p, sym)
}

// symbolDefinition reports a symbol's declaration location: the canonical
// filesystem path of the module the symbol was declared in, and the 1-based
// line/column range of the symbol's own name-node span. The file path comes
// from the module graph's canonical ModuleKey.Path (an absolute, symlink-
// resolved path -- usable to build a file:// URI), not the module's display
// basename, so a cross-file definition target resolves to its real location.
// A zero value (empty File) means the symbol has no source location to jump
// to (e.g. a compiler-owned builtin whose span is empty), which is
// "no definition".
func symbolDefinition(p *compiledProgram, sym symbol.Symbol) structuredDefinition {
	file, ok := p.sources.File(sym.Span.Source)
	if !ok {
		return structuredDefinition{}
	}
	// Prefer the module's canonical path over the display basename so the
	// returned location is an absolute, jump-able filesystem path even when
	// the target is a different (e.g. imported) file. A module under the
	// embedded standard library carries a synthetic key path like
	// "std:embedded/set.peb" that is not a real filesystem path; translate it
	// to the real on-disk stdlib file this checkout was built from (the embed
	// is byte-identical to compiler/std/*.peb, so line/column math carries
	// over), falling back to the synthetic path when no checkout is found.
	path := file.Path()
	if m, ok := p.graph.Module(sym.Module); ok && m.Key.Path != "" {
		path = realStdlibPath(string(m.Key.Path))
		if path == "" {
			path = realPreludePath(string(m.Key.Path))
		}
		if path == "" {
			path = filepath.FromSlash(string(m.Key.Path))
		}
	}
	start := file.Position(sym.Span.Start)
	end := file.Position(sym.Span.End)
	return structuredDefinition{
		File:      path,
		StartLine: start.Line,
		StartCol:  start.Column,
		EndLine:   end.Line,
		EndCol:    end.Column,
	}
}

// documentSymbolsForFile performs a fresh full check of the entry module (the
// same daemon build path a build uses -- there is no warm checked state to
// query yet) and walks the resolved symbol table for that module, returning a
// nested outline TREE (not a flat list). Top-level entries are module-scope
// declarations worth showing in an outline: functions, methods' owning types,
// type/external-type declarations, and module-scope bindings. A type symbol's
// members (struct/union fields, enum variants, and methods) are nested under
// the type via Children, so the client renders a real, useful outline rather
// than a flat dump. Parameters are intentionally NOT shown as children:
// matching gopls' convention, the outline lists top-level symbols and a type's
// own members, but never a function's parameters. Each returned symbol carries
// both the whole-declaration Range (from the symbol's Declaration node span)
// and the tight SelectionRange (the symbol's own name span), plus a Detail with
// the real resolved type/signature (reusing the hover type-description path).
func documentSymbolsForFile(entryPath string) []structuredDocumentSymbol {
	p, fatal := buildProgram(compileRequest{mode: modeCheck, entryPath: entryPath, stderr: io.Discard})
	if fatal || p == nil {
		return nil
	}
	// The entry module is Graph.Root, regardless of any separate prelude.
	entryMod, ok := p.graph.Module(p.graph.Root)
	if !ok {
		return nil
	}
	modID := entryMod.ID

	// Filter to the requested file's module (a module-scope symbol lives in
	// exactly one module, so this selects "this file's" declarations) AND to
	// outline-worthy kinds. Parameters and type parameters share the same
	// Containing mechanism fields/variants/methods use to nest under their
	// owning type, but neither belongs in a real outline -- gopls never lists
	// a function's parameters, only actual declarations -- so they're excluded
	// unconditionally. A binding/loop binding is only excluded when it's a
	// LOCAL (Containing != 0, i.e. nested inside a function body): a
	// module-scope var/let (Containing == 0) is a real top-level declaration,
	// same as Go's package-level vars showing in gopls' outline, and stays.
	// SymbolModule is an import qualifier binding (e.g. "hash" for
	// `import "std:hash"`), not a declaration -- confirmed live it otherwise
	// shows up as a bogus top-level "Variable" entry named after the import.
	// A symbol with no authored name (an anonymous function literal passed
	// as a value, e.g. `fn (a, b str) bool => a == b`) has nothing meaningful
	// to show in an outline either, confirmed live it otherwise appears as an
	// empty-named top-level entry.
	var syms []symbol.Symbol
	for _, candidate := range p.resolution.Symbols.All() {
		if candidate.Module != modID {
			continue
		}
		if candidate.Name == "" {
			continue
		}
		switch candidate.Kind {
		case symbol.SymbolParameter, symbol.SymbolTypeParameter, symbol.SymbolModule:
			continue
		case symbol.SymbolBinding, symbol.SymbolExternBinding, symbol.SymbolLoopBinding:
			if candidate.Containing != 0 {
				continue
			}
		}
		syms = append(syms, candidate)
	}

	// Build a mutable tree: map each symbol ID to its structured form (with a
	// Children slice we fill in a second pass) and record its owning symbol.
	nodes := make(map[symbol.SymbolID]*structuredDocumentSymbol, len(syms))
	owning := make(map[symbol.SymbolID]symbol.SymbolID, len(syms))
	var order []symbol.SymbolID
	for i := range syms {
		sym := syms[i]
		ds := toStructuredSymbol(p, modID, sym)
		nodes[sym.ID] = &ds
		owning[sym.ID] = sym.Containing
		order = append(order, sym.ID)
	}

	// Pass 1: link every child into its parent's Children slice first. nodes
	// holds pointers, so this mutates the shared structs in place regardless
	// of which order ids come in -- a child with a higher SymbolID than its
	// parent (the overwhelmingly common case, since a struct's own symbol is
	// always collected before its fields) must still land in the parent's
	// Children before pass 2 takes its value.
	isChild := make(map[symbol.SymbolID]bool, len(syms))
	for _, id := range order {
		owner := owning[id]
		if owner == 0 {
			continue
		}
		parent, ok := nodes[owner]
		if !ok {
			continue
		}
		parent.Children = append(parent.Children, *nodes[id])
		isChild[id] = true
	}
	// Pass 2: now that every parent's Children is fully populated, collect
	// the top-level (non-child) symbols by value.
	var top []structuredDocumentSymbol
	for _, id := range order {
		if isChild[id] {
			continue
		}
		top = append(top, *nodes[id])
	}
	return top
}

// toStructuredSymbol converts one resolved symbol into its machine-readable
// outline form. The enclosing Range comes from the symbol's Declaration node
// (the whole declaration statement), falling back to the name span when the
// declaration node cannot be resolved; the SelectionRange is the symbol's own
// name span (sym.Span), exactly like hover/definition already use. Detail holds
// the real resolved type/signature (reusing the hover description path), which
// gives a function its signature and a field/binding its type in the outline.
func toStructuredSymbol(p *compiledProgram, modID module.ModuleID, sym symbol.Symbol) structuredDocumentSymbol {
	file, ok := p.sources.File(sym.Span.Source)
	if !ok {
		return structuredDocumentSymbol{Name: sym.Name, Kind: symbolKindToLSP(p, sym)}
	}
	// Tight name span (SelectionRange).
	selStart := file.Position(sym.Span.Start)
	selEnd := file.Position(sym.Span.End)
	// Whole-declaration span (Range), from the Declaration node.
	declStart, declEnd := selStart, selEnd
	if n, ok := p.sourcesNode(sym.Declaration.Module, sym.Declaration.Node); ok {
		ds := n.Span()
		if ds.Start <= ds.End {
			declStart = file.Position(ds.Start)
			declEnd = file.Position(ds.End)
		}
	}

	detail := ""
	if !isTypeKind(sym.Kind) {
		if mod, ok := p.graph.Module(sym.Module); ok {
			resolve := types.ResolveFromResultQualified(p.resolution, sym.Module, types.QualifierMap(mod.Imports))
			if txt, ok := renderSymbolHover(p, sym, resolve); ok {
				detail = txt
			}
		}
	}

	return structuredDocumentSymbol{
		Name:         sym.Name,
		Detail:       detail,
		Kind:         symbolKindToLSP(p, sym),
		StartLine:    declStart.Line,
		StartCol:     declStart.Column,
		EndLine:      declEnd.Line,
		EndCol:       declEnd.Column,
		SelStartLine: selStart.Line,
		SelStartCol:  selStart.Column,
		SelEndLine:   selEnd.Line,
		SelEndCol:    selEnd.Column,
	}
}

// isTypeKind reports whether a symbol kind is a type declaration (struct/enum/
// union/extern/type-parameter/builtin/runtime). These render with just their
// name in the outline (the Name already shows the type name), so we omit the
// redundant "type X" Detail that the hover path would produce.
func isTypeKind(k symbol.SymbolKind) bool {
	switch k {
	case symbol.SymbolType, symbol.SymbolExternType, symbol.SymbolBuiltinType, symbol.SymbolRuntimeType, symbol.SymbolTypeParameter:
		return true
	default:
		return false
	}
}

// symbolKindToLSP maps a resolved SymbolKind to the closest real LSP
// SymbolKind. Type declarations are refined to Struct/Enum when their
// underlying aggregate shape (struct/union/enum) is cheaply recoverable from
// the Declaration node; otherwise they fall back to Struct. Parameters and
// bindings become Variable, fields become Field, variants become EnumMember,
// and methods become Method.
func symbolKindToLSP(p *compiledProgram, sym symbol.Symbol) int {
	switch sym.Kind {
	case symbol.SymbolFunction, symbol.SymbolExternFunction, symbol.SymbolBuiltinFunction:
		return int(protocol.SymbolKindFunction)
	case symbol.SymbolMethod:
		return int(protocol.SymbolKindMethod)
	case symbol.SymbolType, symbol.SymbolExternType, symbol.SymbolBuiltinType, symbol.SymbolRuntimeType:
		if k := typeSymbolLSPKind(p, sym); k != 0 {
			return k
		}
		return int(protocol.SymbolKindStruct)
	case symbol.SymbolField:
		return int(protocol.SymbolKindField)
	case symbol.SymbolVariant:
		return int(protocol.SymbolKindEnumMember)
	case symbol.SymbolParameter, symbol.SymbolTypeParameter:
		return int(protocol.SymbolKindVariable)
	case symbol.SymbolBinding, symbol.SymbolExternBinding, symbol.SymbolLoopBinding:
		return int(protocol.SymbolKindVariable)
	default:
		return int(protocol.SymbolKindVariable)
	}
}

// aggregateKeyword returns the aggregate keyword for a type symbol's
// declaration, using the same node-walking approach as typeSymbolLSPKind.
// It returns "struct", "union", "enum", or "" when no aggregate shape is
// found (e.g. a type alias to a non-aggregate type). A tagged union
// (UnionType containing a literal KwEnum child) is reported as "enum" so the
// hover and LSP-kind paths agree: plain enums and tagged unions both render
// as enum-like types.
func aggregateKeyword(p *compiledProgram, sym symbol.Symbol) string {
	n, ok := p.sourcesNode(sym.Declaration.Module, sym.Declaration.Node)
	if !ok {
		return ""
	}
	if n.Kind() != syntax.TypeDecl && n.Kind() != syntax.ExternType {
		return ""
	}
	for _, child := range n.Children() {
		cn, ok := p.sourcesNode(sym.Declaration.Module, child)
		if !ok {
			continue
		}
		switch cn.Kind() {
		case syntax.StructType:
			return "struct"
		case syntax.UnionType:
			// A tagged union is "union enum { ... }": its UnionType node
			// carries a literal KwEnum child as its first semantic child.
			for _, gc := range cn.Children() {
				if gcn, ok := p.sourcesNode(sym.Declaration.Module, gc); ok && gcn.Kind() == syntax.Literal && gcn.Token() == syntax.KwEnum {
					return "enum"
				}
			}
			return "union"
		case syntax.EnumType:
			return "enum"
		}
	}
	return ""
}

// typeSymbolLSPKind inspects a type symbol's Declaration node to pick a more
// precise LSP kind than the generic fallback. For a TypeDecl (or ExternType)
// node, it scans the child nodes for the underlying aggregate shape:
// StructType/UnionType -> Struct, EnumType -> Enum. A non-aggregate underlying
// type (e.g. a type alias to a primitive) returns 0 so the caller falls back to
// Struct. Returns 0 when the declaration node cannot be resolved.
func typeSymbolLSPKind(p *compiledProgram, sym symbol.Symbol) int {
	kw := aggregateKeyword(p, sym)
	switch kw {
	case "struct", "union":
		return int(protocol.SymbolKindStruct)
	case "enum":
		return int(protocol.SymbolKindEnum)
	default:
		return 0
	}
}

// realStdlibPath translates an embedded-stdlib module key path (e.g.
// "std:embedded/set.peb") into the real on-disk file under the checkout's
// std/ directory, or "" when the key path is not a stdlib module, the on-disk
// std tree cannot be located (pebc running outside a checkout), or the target
// file does not exist on disk.
func realStdlibPath(keyPath string) string {
	if !strings.HasPrefix(keyPath, stdlib.StandardRoot+"/") {
		return ""
	}
	stdRoot, err := locateStdRoot()
	if err != nil {
		return ""
	}
	relative := strings.TrimPrefix(keyPath, stdlib.StandardRoot+"/")
	candidate := filepath.Join(stdRoot, filepath.FromSlash(relative))
	if info, err := os.Stat(candidate); err != nil || info.IsDir() {
		return ""
	}
	return candidate
}

// realPreludePath translates the embedded-prelude module key path
// ("prelude/runtime.peb" — see internal/module/build.go's embeddedPreludePath)
// into the real on-disk file under the checkout's prelude/ directory, or ""
// when the key path is not the prelude module, the on-disk prelude tree cannot
// be located (pebc running outside a checkout), or the target file does not
// exist on disk.
func realPreludePath(keyPath string) string {
	if keyPath != "prelude/runtime.peb" {
		return ""
	}
	preludeRoot, err := locatePreludeRoot()
	if err != nil {
		return ""
	}
	candidate := filepath.Join(preludeRoot, "runtime.peb")
	if info, err := os.Stat(candidate); err != nil || info.IsDir() {
		return ""
	}
	return candidate
}

// inlayHintsInRange walks the entry module's syntax tree once and returns
// every inlay hint whose anchor byte offset falls within [startOffset,
// endOffset]. Two categories are produced:
//
//   - Type hints on a `var`/`let` binding that has NO explicit type annotation:
//     rendered as " Type" and anchored right after the binding name (so the
//     editor shows `let x: i32 = ...`). The hint is suppressed entirely when
//     the binding's BindingDecl already carries an explicit type, mirroring how
//     gopls/rust-analyzer avoid redundant hints.
//   - Parameter-name hints before each argument of a resolved function call:
//     rendered as "name: " and anchored immediately before the argument (so the
//     editor shows `add(p: origin, scale: 5)`). The callee's real parameter
//     NAMES come from the symbol table (a function TypeKey only carries
//     parameter types), ordered by SymbolID to recover declaration order, and
//     each positional argument is matched to its parameter by index.
//
// The hint's own position must sit inside the requested range, so a partial
// (e.g. visible-region) query only returns the hints that apply to what the
// client actually requested.
func inlayHintsInRange(entryPath string, startOffset, endOffset uint32) []structuredInlayHint {
	p, fatal := buildProgram(compileRequest{mode: modeCheck, entryPath: entryPath, stderr: io.Discard})
	if fatal || p == nil {
		return nil
	}
	// The entry module is Graph.Root, regardless of any separate prelude.
	entryMod, ok := p.graph.Module(p.graph.Root)
	if !ok || entryMod.Tree == nil {
		return nil
	}
	tree := entryMod.Tree
	file, ok := p.sources.File(entryMod.Source)
	if !ok {
		return nil
	}
	modID := entryMod.ID

	var hints []structuredInlayHint
	for id := syntax.NodeID(1); uint64(id) <= uint64(tree.Root()); id++ {
		n, ok := tree.Node(id)
		if !ok {
			continue
		}
		switch n.Kind() {
		case syntax.BindingDecl:
			if h, ok := bindingTypeHint(p, modID, tree, file, n, startOffset, endOffset); ok {
				hints = append(hints, h)
			}
		case syntax.CallExpr:
			hints = append(hints, callParamHints(p, modID, tree, file, n, startOffset, endOffset)...)
		}
	}
	return hints
}

// bindingTypeHint produces a type inlay hint for a `var`/`let` binding whose
// name node is `nameNodeID`, or reports no hint. It is only emitted when the
// binding carries no explicit type annotation and a checked type is available.
func bindingTypeHint(p *compiledProgram, modID module.ModuleID, tree *syntax.Tree, file *source.File, node syntax.Node, startOffset, endOffset uint32) (structuredInlayHint, bool) {
	// Already-annotated bindings (e.g. `let x: i32 = ...`) get no redundant
	// type hint. This flag is set exactly as symbol/visit.go's resolveBinding
	// checks it.
	if node.Data()&syntax.BindingTypePresent != 0 {
		return structuredInlayHint{}, false
	}
	children := node.Children()
	if len(children) == 0 {
		return structuredInlayHint{}, false
	}
	nameNode, ok := tree.Node(children[0])
	if !ok || nameNode.Kind() != syntax.Name {
		return structuredInlayHint{}, false
	}
	// Anchor right after the binding name, before the `=`.
	anchor := nameNode.Span().End
	if !inlayHintInRange(anchor, startOffset, endOffset) {
		return structuredInlayHint{}, false
	}
	// Resolve the binding through its NAME node (the symbol's span is the
	// name node, not the whole BindingDecl), exactly as the hover path does.
	sym, ok := symbolForNode(p, modID, children[0])
	if !ok {
		return structuredInlayHint{}, false
	}
	typeResult, ok := p.result.SymbolType(sym.ID)
	if !ok || typeResult.Type == 0 {
		return structuredInlayHint{}, false
	}
	lookup := types.LookupFromStore(p.store)
	key, ok := lookup(typeResult.Type)
	if !ok {
		return structuredInlayHint{}, false
	}
	mod, _ := p.graph.Module(modID)
	resolve := types.ResolveFromResultQualified(p.resolution, modID, types.QualifierMap(mod.Imports))
	typ := types.DescribeKeyResolved(key, lookup, resolve)
	return makeInlayHint(file, anchor, " "+typ, inlayHintType), true
}

// callParamHints produces parameter-name inlay hints for a function call. It
// resolves the callee to its function symbol, recovers the declared parameter
// names from the symbol table, and emits one "name: " hint before each
// positional argument. The whole call is skipped when the argument count does
// not match the parameter count, or when the callee cannot be resolved to a
// real function symbol. A given argument is also skipped when its own source
// text already visually equals the parameter name.
func callParamHints(p *compiledProgram, modID module.ModuleID, tree *syntax.Tree, file *source.File, node syntax.Node, startOffset, endOffset uint32) []structuredInlayHint {
	children := node.Children()
	if len(children) < 2 {
		return nil
	}
	callee := children[0]
	sym, ok := symbolForNode(p, modID, callee)
	if !ok {
		return nil
	}
	// Recover the callee's declared parameter names, ordered by SymbolID to
	// match declaration order (verified against a real multi-parameter fn).
	var params []symbol.Symbol
	for _, candidate := range p.resolution.Symbols.All() {
		if candidate.Kind == symbol.SymbolParameter && candidate.Containing == sym.ID {
			params = append(params, candidate)
		}
	}
	if len(params) == 0 {
		return nil
	}
	sort.Slice(params, func(i, j int) bool { return params[i].ID < params[j].ID })

	args := children[1:]
	// Skip the whole call when arity mismatches: matching by index would be
	// wrong (defaulted params, varargs, or a partially-written call).
	if len(args) != len(params) {
		return nil
	}

	var hints []structuredInlayHint
	for i, argID := range args {
		argNode, ok := tree.Node(argID)
		if !ok {
			continue
		}
		if argNode.Kind() == syntax.Missing || argNode.Kind() == syntax.Error {
			continue
		}
		anchor := argNode.Span().Start
		if !inlayHintInRange(anchor, startOffset, endOffset) {
			continue
		}
		// Suppress when the argument already reads like the parameter name.
		if argText := string(file.Slice(argNode.Span())); argText == params[i].Name {
			continue
		}
		hints = append(hints, makeInlayHint(file, anchor, params[i].Name+": ", inlayHintParameter))
	}
	return hints
}

// makeInlayHint builds a structured inlay hint at a byte offset, resolving the
// offset to a 1-based line/column through the source file.
func makeInlayHint(file *source.File, offset uint32, label, kind string) structuredInlayHint {
	pos := file.Position(offset)
	return structuredInlayHint{
		File:  file.Path(),
		Line:  pos.Line,
		Col:   pos.Column,
		Label: label,
		Kind:  kind,
	}
}

// inlayHintInRange reports whether anchor falls within [start, end].
func inlayHintInRange(anchor, start, end uint32) bool {
	return anchor >= start && anchor <= end
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
	lookup := types.LookupFromStore(p.store)

	// Type declarations (struct/union/enum/extern/type parameter) render as
	// a multi-line aggregate description when an aggregate shape is present,
	// or a bare "type Name" (or "type Name = Underlying" for an alias when
	// cheaply resolvable) otherwise.
	if sym.Kind == symbol.SymbolType || sym.Kind == symbol.SymbolExternType || sym.Kind == symbol.SymbolRuntimeType || sym.Kind == symbol.SymbolBuiltinType {
		if name == "" {
			return "", false
		}
		kw := aggregateKeyword(p, sym)
		if kw == "" {
			// No aggregate shape: plain alias like "type Foo = i32".
			// Attempt to render the underlying type when easily resolvable;
			// otherwise fall back to the historic bare "type Name".
			if tr, ok := p.result.SymbolType(sym.ID); ok && tr.Type != 0 {
				if key, ok := lookup(tr.Type); ok {
					typ := types.DescribeKeyResolved(key, lookup, resolve)
					if typ != "" && typ != name {
						return "type " + name + " = " + typ, true
					}
				}
			}
			return "type " + name, true
		}
		// Collect members belonging to this type, in declaration order
		// (SymbolID ascending matches resolver insertion order, which is
		// declaration order; see callParamHints and documentSymbolsForFile).
		var members []symbol.Symbol
		for _, cand := range p.resolution.Symbols.All() {
			if cand.Containing != sym.ID {
				continue
			}
			if kw == "struct" {
				if cand.Kind != symbol.SymbolField {
					continue
				}
			} else if kw == "union" {
				// An untagged union's members are registered as SymbolVariant
				// by the resolver, same as an enum/tagged-union's variants
				// (see internal/check/member_validation.go's
				// untaggedUnionDeclaration doc comment) -- they behave like
				// fields (unsafe reinterpret-the-bytes access), not payload
				// variants, so they're still rendered in the struct-like
				// "name type;" form below, just collected under this kind.
				if cand.Kind != symbol.SymbolField && cand.Kind != symbol.SymbolVariant {
					continue
				}
			} else { // enum (covers plain enum and tagged union)
				if cand.Kind != symbol.SymbolVariant {
					continue
				}
			}
			if cand.Name == "" || cand.Error {
				continue
			}
			members = append(members, cand)
		}
		sort.Slice(members, func(i, j int) bool { return members[i].ID < members[j].ID })
		const hoverMemberLimit = 64
		total := len(members)
		truncated := false
		if total > hoverMemberLimit {
			members = members[:hoverMemberLimit]
			truncated = true
		}
		var b strings.Builder
		b.WriteString("type " + name + " " + kw + " {\n")
		for _, m := range members {
			if kw == "struct" || kw == "union" {
				typeStr := "<type>"
				if tr, ok := p.result.SymbolType(m.ID); ok && tr.Type != 0 {
					if key, ok := lookup(tr.Type); ok {
						typeStr = types.DescribeKeyResolved(key, lookup, resolve)
					}
				}
				b.WriteString("    " + m.Name + " " + typeStr + ";\n")
			} else {
				payload := ""
				// First try the typed-IR: MemberTypes holds the resolved payload type
				// (void for plain enum variants, concrete payload for tagged unions).
				if p.unit != nil {
					for _, td := range p.unit.TypeDeclarations() {
						if td.Symbol != sym.ID {
							continue
						}
						for idx, mid := range td.Members {
							if mid != m.ID || idx >= len(td.MemberTypes) {
								continue
							}
							pt := td.MemberTypes[idx]
							if pt == 0 {
								break
							}
							snap := p.unit.Snapshot()
							var key types.TypeKey
							var ok bool
							if snap != nil {
								key, ok = snap.Key(pt)
							}
							if !ok {
								key, ok = lookup(pt)
							}
							if ok {
								if bk, isB := key.Builtin(); isB && bk == types.Void {
									// no payload for plain enum variants
								} else {
									lookupSnap := types.LookupFromSnapshot(snap)
									if lookupSnap == nil {
										lookupSnap = lookup
									}
									typ := types.DescribeKeyResolved(key, lookupSnap, resolve)
									if typ == "" || typ == "<type>" {
										typ = types.DescribeKeyResolved(key, lookup, resolve)
									}
									if typ != "" && typ != "void" && typ != "<type>" {
										payload = typ
									}
								}
							}
							break
						}
						break
					}
				}
				if payload == "" {
					if declNode, ok := p.sourcesNode(m.Declaration.Module, m.Declaration.Node); ok {
						if declNode.Kind() == syntax.VariantDecl {
							children := declNode.Children()
							var semis []syntax.NodeID
							for _, cid := range children {
								if cn, ok := p.sourcesNode(m.Declaration.Module, cid); ok && cn.Kind() != syntax.Missing && cn.Kind() != syntax.Error && cn.Kind() != syntax.EndOfFile {
									semis = append(semis, cid)
								}
							}
							var payloadID types.TypeID
							found := false
							for i := len(semis) - 1; i >= 0; i-- {
								cid := semis[i]
								cn, _ := p.sourcesNode(m.Declaration.Module, cid)
								if cn.Kind() == syntax.Name {
									continue
								}
								if sol := p.result.Solution(); sol != nil {
									if tr, ok := sol.SyntaxType(symbol.SyntaxRef{Module: m.Declaration.Module, Node: cid}); ok && tr.Type != 0 {
										if key, ok := lookup(tr.Type); ok {
											if bk, isB := key.Builtin(); isB && bk == types.Void {
												continue
											}
											payloadID = tr.Type
											found = true
											break
										}
									}
								}
							}
							if found {
								if key, ok := lookup(payloadID); ok {
									typ := types.DescribeKeyResolved(key, lookup, resolve)
									if typ != "" && typ != "void" && typ != "<type>" {
										payload = typ
									}
								}
							}
						}
					}
				}
				// Final fallback: SymbolType (for cases where TIR unavailable and
				// syntax inspection missed, e.g. partial-failure resilience). Guard
				// against the owning-type case where SymbolType is the enum itself.
				if payload == "" {
					if tr, ok := p.result.SymbolType(m.ID); ok && tr.Type != 0 {
						if key, ok := lookup(tr.Type); ok {
							if bk, isB := key.Builtin(); !isB || bk != types.Void {
								typ := types.DescribeKeyResolved(key, lookup, resolve)
								if typ != "" && typ != "void" && typ != "<type>" {
									ownerName := ""
									if resolve != nil && m.Containing != 0 {
										ownerName = resolve(m.Containing)
									}
									if typ != ownerName {
										payload = typ
									}
								}
							}
						}
					}
				}
				if payload != "" {
					b.WriteString("    " + m.Name + "(" + payload + ");\n")
				} else {
					b.WriteString("    " + m.Name + ";\n")
				}
			}
		}
		if truncated {
			b.WriteString(fmt.Sprintf("    ... and %d more\n", total-hoverMemberLimit))
		}
		b.WriteString("}")
		return b.String(), true
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
	key, keyOK := lookup(typeResult.Type)
	if !keyOK {
		return "", false
	}
	typ := types.DescribeKeyResolved(key, lookup, resolve)

	switch sym.Kind {
	case symbol.SymbolBinding:
		if name == "" {
			return "", false
		}
		return bindingKeyword(p, sym) + " " + name + " " + typ, true
	case symbol.SymbolExternBinding, symbol.SymbolLoopBinding:
		if name == "" {
			return "", false
		}
		return "var " + name + " " + typ, true
	case symbol.SymbolParameter:
		if name == "" {
			return "", false
		}
		return "param " + name + " " + typ, true
	case symbol.SymbolField:
		if name == "" {
			return "", false
		}
		return "field " + name + " " + typ, true
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
		return renderFunctionHover(name, key, lookup, resolve), true
	default:
		// Unknown symbol kinds degrade to the bare type description.
		return typ, true
	}
}

// renderFunctionHover renders a function symbol's full signature in the form
// "fn name(p1 T1, p2 T2) R" from its function type key.
func renderFunctionHover(name string, key types.TypeKey, lookup func(types.TypeID) (types.TypeKey, bool), resolve func(symbol.SymbolID) string) string {
	_, parameters, result, _, _ := key.Function()
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

// signatureHelpAtOffset performs a fresh full check of the entry module (the
// same daemon build path a build uses) and finds the enclosing CallExpr whose
// span covers the given byte offset, then resolves the callee to its function
// symbol and returns the parameter list with the active parameter index. The
// second return value reports whether a usable call expression was found; when
// false the caller should return nil (no signature help available).
//
// Unlike hover/definition which search for the SMALLEST containing node,
// signature help must find the CALL expression the cursor sits inside – the
// cursor may be on whitespace between two arguments, on a trailing comma with
// nothing typed after it yet, or even mid-callee while the call is syntactically
// incomplete. The CallExpr node itself is always real and walkable even when
// some children are Missing/Error placeholders (parseCallSuffix already
// handles this for inlay hints).
func signatureHelpAtOffset(entryPath string, offset uint32) structuredSignatureHelp {
	p, fatal := buildProgram(compileRequest{mode: modeCheck, entryPath: entryPath, stderr: io.Discard})
	if fatal || p == nil {
		return structuredSignatureHelp{}
	}
	entryMod, ok := p.graph.Module(p.graph.Root)
	if !ok || entryMod.Tree == nil {
		return structuredSignatureHelp{}
	}
	tree := entryMod.Tree
	modID := entryMod.ID

	// Find the smallest CallExpr whose span contains the offset. We scan all
	// nodes but only consider CallExpr kinds so that nested calls resolve to
	// the innermost one covering the cursor position.
	var best syntax.NodeID
	var bestWidth uint32
	found := false
	for id := syntax.NodeID(1); uint64(id) <= uint64(tree.Root()); id++ {
		n, ok := tree.Node(id)
		if !ok {
			continue
		}
		if n.Kind() != syntax.CallExpr {
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
		return structuredSignatureHelp{}
	}

	callNode, ok := tree.Node(best)
	if !ok {
		return structuredSignatureHelp{}
	}
	children := callNode.Children()
	if len(children) < 2 {
		return structuredSignatureHelp{}
	}
	callee := children[0]
	sym, ok := symbolForNode(p, entryMod.ID, callee)
	if !ok {
		return structuredSignatureHelp{}
	}

	// Recover the callee's declared parameter names and types, ordered by
	// SymbolID to match declaration order (same pattern callParamHints uses).
	var params []symbol.Symbol
	for _, candidate := range p.resolution.Symbols.All() {
		if candidate.Kind == symbol.SymbolParameter && candidate.Containing == sym.ID {
			params = append(params, candidate)
		}
	}
	if len(params) == 0 {
		// No named parameters – still try to render the signature from the
		// function type key so callers see at least the shape.
		return renderSignatureFromTypeKey(p, sym, modID, 0)
	}
	sort.Slice(params, func(i, j int) bool { return params[i].ID < params[j].ID })

	// Collect direct argument expressions (skip Missing/Error placeholders
	// exactly as callParamHints does).
	args := make([]syntax.NodeID, 0, len(children)-1)
	for _, argID := range children[1:] {
		argNode, ok := tree.Node(argID)
		if !ok {
			continue
		}
		if argNode.Kind() == syntax.Missing || argNode.Kind() == syntax.Error {
			continue
		}
		args = append(args, argID)
	}

	// Determine the active parameter index by counting how many top-level
	// comma-separated argument slots come BEFORE the offset within this call.
	// Children are direct argument expressions (not a flattened token stream),
	// so we simply count how many args end before the offset.
	activeParam := 0
	for _, argID := range args {
		argNode, ok := tree.Node(argID)
		if !ok {
			continue
		}
		if argNode.Span().End <= offset {
			activeParam++
		}
	}
	if activeParam >= len(params) {
		activeParam = len(params) - 1
	}

	return buildSignatureHelp(sym, params, activeParam, p, modID)
}

// renderSignatureFromTypeKey builds a structuredSignatureHelp from the
// callee's function type key when there are no named parameters in the symbol
// table (e.g. builtins or extern functions). It renders the raw parameter
// types as labels.
func renderSignatureFromTypeKey(p *compiledProgram, sym symbol.Symbol, modID module.ModuleID, activeParam int) structuredSignatureHelp {
	typeResult, ok := p.result.SymbolType(sym.ID)
	if !ok || typeResult.Type == 0 {
		return structuredSignatureHelp{}
	}
	lookup := types.LookupFromStore(p.store)
	key, ok := lookup(typeResult.Type)
	if !ok {
		return structuredSignatureHelp{}
	}
	mod, _ := p.graph.Module(modID)
	resolve := types.ResolveFromResultQualified(p.resolution, modID, types.QualifierMap(mod.Imports))
	label := renderFunctionHover(sym.Name, key, lookup, resolve)
	_, parameters, _, _, _ := key.Function()
	var paramLabels []string
	for _, param := range parameters {
		paramLabels = append(paramLabels, describeTypeID(lookup, param, resolve))
	}
	return structuredSignatureHelp{
		Signatures: []structuredSignature{
			{Label: label, Parameters: paramLabels},
		},
		ActiveSignature: 0,
		ActiveParameter: min(activeParam, len(paramLabels)-1),
	}
}

// buildSignatureHelp constructs a structuredSignatureHelp from a resolved
// function symbol and its named parameters. The label includes each parameter
// as "name Type" using the same DescribeKeyResolved machinery that hover uses.
func buildSignatureHelp(sym symbol.Symbol, params []symbol.Symbol, activeParam int, p *compiledProgram, modID module.ModuleID) structuredSignatureHelp {
	typeResult, ok := p.result.SymbolType(sym.ID)
	if !ok || typeResult.Type == 0 {
		return structuredSignatureHelp{}
	}
	lookup := types.LookupFromStore(p.store)
	key, ok := lookup(typeResult.Type)
	if !ok {
		return structuredSignatureHelp{}
	}
	mod, _ := p.graph.Module(modID)
	resolve := types.ResolveFromResultQualified(p.resolution, modID, types.QualifierMap(mod.Imports))
	label := renderFunctionHover(sym.Name, key, lookup, resolve)

	paramLabels := make([]string, len(params))
	for i, param := range params {
		paramTyp, err := p.result.SymbolType(param.ID)
		if err && paramTyp.Type != 0 {
			if pk, ok := lookup(paramTyp.Type); ok {
				paramLabels[i] = param.Name + " " + types.DescribeKeyResolved(pk, lookup, resolve)
				continue
			}
		}
		paramLabels[i] = param.Name
	}
	return structuredSignatureHelp{
		Signatures: []structuredSignature{
			{Label: label, Parameters: paramLabels},
		},
		ActiveSignature: 0,
		ActiveParameter: min(activeParam, len(paramLabels)-1),
	}
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

// completionsAtOffset performs a fresh full check of the entry module (the
// same daemon build path a build uses -- there is no warm checked state to
// query yet) and returns the completion candidates at the given byte offset.
// It dispatches to one of two bounded paths based on the character immediately
// before the offset:
//
//   - If it is '.', it runs MEMBER completion: resolve the receiver expression
//     immediately before the dot to its checked type, and (auto-derefing a
//     pointer) collect the declared fields and methods of the nominal type.
//   - Otherwise it runs SCOPE-AWARE IDENTIFIER completion: find the lexical
//     scope containing the offset, then walk up the Parent chain collecting
//     every in-scope symbol (locals/params innermost, then module declarations,
//     then prelude), deduplicated by name so a shadowed outer symbol does not
//     produce two entries.
//
// In both cases the full candidate set for the scope/receiver is returned;
// client-side filtering narrows it (standard LSP behavior). An empty result is
// "no completions here", not an error. Both paths work even when the file has
// an unrelated syntax error elsewhere, following the same symbol-first,
// store-backed pattern hover/definition already use.
func completionsAtOffset(entryPath string, offset uint32) []structuredCompletionItem {
	p, fatal := buildProgram(compileRequest{mode: modeCheck, entryPath: entryPath, stderr: io.Discard})
	if fatal || p == nil {
		return nil
	}
	// The entry module is Graph.Root, regardless of any separate prelude.
	entryMod, ok := p.graph.Module(p.graph.Root)
	if !ok || entryMod.Tree == nil {
		return nil
	}
	tree := entryMod.Tree
	modID := entryMod.ID

	// Dispatch on the byte immediately before the offset: '.' selects member
	// completion, anything else selects scope-aware identifier completion. The
	// byte check is more reliable than trusting the LSP trigger character, which
	// different clients may send or omit.
	if offset > 0 {
		file, ok := p.sources.File(entryMod.Source)
		if ok {
			data := file.Text()
			if uint64(offset-1) < uint64(len(data)) && data[offset-1] == '.' {
				return memberCompletions(p, modID, tree, offset)
			}
		}
	}
	return identifierCompletions(p, modID, tree, offset)
}

// identifierCompletions returns scope-aware identifier completion candidates at
// offset: it finds the innermost lexical scope whose originating syntax node
// contains the offset, then walks up the scope's Parent chain collecting every
// symbol from every scope along the way (locals/params innermost, module-scope
// declarations, prelude), deduplicating by name so a shadowed outer symbol does
// not produce a second identical-looking entry -- the innermost one wins. Each
// symbol becomes one completion item whose Detail carries its real
// type/signature (reusing the hover description path).
func identifierCompletions(p *compiledProgram, modID module.ModuleID, tree *syntax.Tree, offset uint32) []structuredCompletionItem {
	// Every scope's Origin is a SyntaxRef naming the syntax node that created
	// it (a BlockStmt, function, loop, or the module root). Find the scope whose
	// origin node's span contains the offset with the SMALLEST width -- that is
	// the innermost lexical scope enclosing the position. The module scope's
	// origin is the tree root spanning the whole file, so it is always a valid
	// outermost fallback.
	var best symbol.Scope
	var bestWidth uint32
	found := false
	for _, scope := range p.resolution.Scopes.All() {
		if scope.Module != modID || scope.Origin.Module != modID {
			continue
		}
		node, ok := tree.Node(scope.Origin.Node)
		if !ok {
			continue
		}
		span := node.Span()
		if offset < span.Start || offset > span.End {
			continue
		}
		width := span.End - span.Start
		if !found || width < bestWidth {
			best = scope
			bestWidth = width
			found = true
		}
	}
	if !found {
		return nil
	}

	mod, _ := p.graph.Module(modID)
	resolve := types.ResolveFromResultQualified(p.resolution, modID, types.QualifierMap(mod.Imports))

	seen := make(map[string]bool)
	var items []structuredCompletionItem
	for cur := best.ID; cur != 0; {
		scope, ok := p.resolution.Scopes.Scope(cur)
		if !ok {
			break
		}
		for _, sid := range scope.Symbols {
			sym, ok := p.resolution.Symbols.Symbol(sid)
			if !ok || sym.Name == "" || sym.Error {
				continue
			}
			if seen[sym.Name] {
				continue
			}
			seen[sym.Name] = true
			items = append(items, completionItemForSymbol(p, sym, resolve))
		}
		cur = scope.Parent
	}
	items = append(items, keywordCompletionItems()...)
	return items
}

// memberCompletions returns member completion candidates for the receiver
// expression immediately before a '.' at offset. It finds the smallest syntax
// node whose span ends exactly at the dot (the receiver expression), resolves
// its checked type, auto-derefs a pointer, and -- when the type is Nominal (a
// struct/union/enum) -- collects the declared fields and methods whose
// Containing equals the nominal type's declaration symbol.
func memberCompletions(p *compiledProgram, modID module.ModuleID, tree *syntax.Tree, offset uint32) []structuredCompletionItem {
	// The '.' is the byte immediately before the cursor; the receiver is the
	// node whose span ends exactly at the dot's position.
	dotPos := offset - 1
	var receiver syntax.NodeID
	var receiverWidth uint32
	found := false
	for id := syntax.NodeID(1); uint64(id) <= uint64(tree.Root()); id++ {
		n, ok := tree.Node(id)
		if !ok {
			continue
		}
		kind := n.Kind()
		if kind == syntax.File || kind == syntax.Error || kind == syntax.Missing || kind == syntax.EndOfFile {
			continue
		}
		span := n.Span()
		if span.End != dotPos {
			continue
		}
		width := span.End - span.Start
		if !found || width < receiverWidth {
			receiver = id
			receiverWidth = width
			found = true
		}
	}
	if !found {
		return nil
	}

	typeID, ok := receiverTypeID(p, modID, receiver)
	if !ok {
		return nil
	}
	lookup := types.LookupFromStore(p.store)
	key, ok := lookup(typeID)
	if !ok {
		return nil
	}
	effectiveID := typeID
	// Auto-deref a pointer receiver exactly as structuralField does.
	if key.Kind() == types.Pointer {
		child, _ := key.Child()
		key, ok = lookup(child)
		if !ok {
			return nil
		}
		effectiveID = child
	}
	decl, _, ok := key.Nominal()
	if !ok {
		// Structural pseudo-fields: array/slice/optional/str have no nominal
		// declaration but still expose .len/.data/.has_value.
		mod, _ := p.graph.Module(modID)
		resolve := types.ResolveFromResultQualified(p.resolution, modID, types.QualifierMap(mod.Imports))
		fieldKind := int(protocol.CompletionItemKindField)
		if _, _, isArray := key.Array(); isArray {
			uintID := p.store.Builtins().Uint
			detail := "len uint"
			if uk, ok := lookup(uintID); ok {
				detail = "len " + types.DescribeKeyResolved(uk, lookup, resolve)
			}
			return []structuredCompletionItem{{Name: "len", Kind: fieldKind, Detail: detail}}
		}
		if key.Kind() == types.Slice {
			uintID := p.store.Builtins().Uint
			lenDetail := "len uint"
			if uk, ok := lookup(uintID); ok {
				lenDetail = "len " + types.DescribeKeyResolved(uk, lookup, resolve)
			}
			element, _ := key.Child()
			elemDesc := "<type>"
			if ek, ok := lookup(element); ok {
				elemDesc = types.DescribeKeyResolved(ek, lookup, resolve)
			}
			dataDetail := "data *" + elemDesc
			return []structuredCompletionItem{
				{Name: "len", Kind: fieldKind, Detail: lenDetail},
				{Name: "data", Kind: fieldKind, Detail: dataDetail},
			}
		}
		if key.Kind() == types.Optional {
			boolID := p.store.Builtins().Bool
			detail := "has_value bool"
			if bk, ok := lookup(boolID); ok {
				detail = "has_value " + types.DescribeKeyResolved(bk, lookup, resolve)
			}
			return []structuredCompletionItem{{Name: "has_value", Kind: fieldKind, Detail: detail}}
		}
		if effectiveID == p.store.Builtins().Str {
			uintID := p.store.Builtins().Uint
			detail := "len uint"
			if uk, ok := lookup(uintID); ok {
				detail = "len " + types.DescribeKeyResolved(uk, lookup, resolve)
			}
			return []structuredCompletionItem{{Name: "len", Kind: fieldKind, Detail: detail}}
		}
		return nil
	}

	mod, _ := p.graph.Module(modID)
	resolve := types.ResolveFromResultQualified(p.resolution, modID, types.QualifierMap(mod.Imports))

	seen := make(map[string]bool)
	var items []structuredCompletionItem
	for _, candidate := range p.resolution.Symbols.All() {
		if candidate.Containing != decl {
			continue
		}
		switch candidate.Kind {
		case symbol.SymbolField, symbol.SymbolMethod:
		default:
			continue
		}
		if candidate.Name == "" || candidate.Error {
			continue
		}
		if seen[candidate.Name] {
			continue
		}
		seen[candidate.Name] = true
		items = append(items, completionItemForSymbol(p, candidate, resolve))
	}
	return items
}

// receiverTypeID resolves the checked type of a receiver expression node. It
// first tries the symbol path (a bare identifier resolves via the reference
// table to its binding/parameter symbol, whose type comes from the store) --
// which works even when a full typed-IR unit was never built (partial-failure
// resilience). It then falls back to the typed-IR source map for arbitrary
// expressions (call results, field projections, etc.), which genuinely needs
// the fully-built unit.
func receiverTypeID(p *compiledProgram, modID module.ModuleID, nodeID syntax.NodeID) (types.TypeID, bool) {
	if sym, ok := symbolForNode(p, modID, nodeID); ok {
		if tr, ok := p.result.SymbolType(sym.ID); ok && tr.Type != 0 {
			return tr.Type, true
		}
	}
	if p.unit == nil {
		return 0, false
	}
	snap := p.unit.Snapshot()
	tirID, ok := p.unit.SourceMap(symbol.SyntaxRef{Module: modID, Node: nodeID})
	if !ok || snap == nil {
		return 0, false
	}
	node, ok := p.unit.Node(tirID)
	if !ok || node.Type == 0 {
		return 0, false
	}
	if _, ok := snap.Key(node.Type); !ok {
		return 0, false
	}
	return node.Type, true
}

// completionItemForSymbol converts one resolved symbol into its structured
// completion form: Name is the symbol's name, Kind is the closest LSP
// CompletionItemKind, and Detail is the real resolved type/signature reusing
// the hover description path.
func completionItemForSymbol(p *compiledProgram, sym symbol.Symbol, resolve func(symbol.SymbolID) string) structuredCompletionItem {
	detail := ""
	if txt, ok := renderSymbolHover(p, sym, resolve); ok {
		detail = txt
	}
	return structuredCompletionItem{
		Name:   sym.Name,
		Kind:   symbolCompletionKind(p, sym),
		Detail: detail,
	}
}

// pebbleKeywords is the flat list of every Pebble keyword. It should be kept
// in sync with internal/syntax/lexer.go's keyword table.
var pebbleKeywords = []string{
	"as",
	"break",
	"case",
	"context",
	"continue",
	"defer",
	"else",
	"enum",
	"extern",
	"false",
	"fn",
	"for",
	"if",
	"import",
	"inline",
	"let",
	"loop",
	"nil",
	"none",
	"print",
	"println",
	"return",
	"slice",
	"sizeof",
	"some",
	"struct",
	"switch",
	"true",
	"type",
	"union",
	"var",
	"while",
}

// keywordCompletionItems returns one structuredCompletionItem per Pebble
// keyword. Name is the keyword text (also the inserted text) and Kind is the
// LSP CompletionItemKind for Keyword. Detail is left empty.
func keywordCompletionItems() []structuredCompletionItem {
	items := make([]structuredCompletionItem, 0, len(pebbleKeywords))
	for _, kw := range pebbleKeywords {
		items = append(items, structuredCompletionItem{
			Name: kw,
			Kind: int(protocol.CompletionItemKindKeyword),
		})
	}
	return items
}

// symbolCompletionKind maps a resolved SymbolKind to the closest real LSP
// CompletionItemKind. Functions become Function, methods Method, type
// declarations Struct/Enum (via the same underlying-aggregate inspection
// symbolKindToLSP uses for documentSymbol), fields Field, and bindings/
// parameters/variants Variable/EnumMember.
func symbolCompletionKind(p *compiledProgram, sym symbol.Symbol) int {
	switch sym.Kind {
	case symbol.SymbolFunction, symbol.SymbolExternFunction, symbol.SymbolBuiltinFunction:
		return int(protocol.CompletionItemKindFunction)
	case symbol.SymbolMethod:
		return int(protocol.CompletionItemKindMethod)
	case symbol.SymbolType, symbol.SymbolExternType, symbol.SymbolBuiltinType, symbol.SymbolRuntimeType:
		if k := typeSymbolLSPKind(p, sym); k != 0 {
			if k == int(protocol.SymbolKindEnum) {
				return int(protocol.CompletionItemKindEnum)
			}
			return int(protocol.CompletionItemKindStruct)
		}
		return int(protocol.CompletionItemKindStruct)
	case symbol.SymbolField:
		return int(protocol.CompletionItemKindField)
	case symbol.SymbolVariant:
		return int(protocol.CompletionItemKindEnumMember)
	case symbol.SymbolParameter, symbol.SymbolTypeParameter:
		return int(protocol.CompletionItemKindVariable)
	case symbol.SymbolBinding, symbol.SymbolExternBinding, symbol.SymbolLoopBinding:
		return int(protocol.CompletionItemKindVariable)
	default:
		return int(protocol.CompletionItemKindVariable)
	}
}
