package module

import (
	"errors"
	"fmt"
	"path"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type candidate struct {
	key      ModuleKey
	display  string
	contents []byte
}

type builder struct {
	config         BuildConfig
	provider       SourceProvider
	sources        *source.FileSet
	diagnostics    *diagnostic.DiagnosticSet
	graph          *Graph
	displays       []string
	depths         []uint32
	maxModules     uint32
	maxDepth       uint32
	maxDiagnostics uint32
	moduleErrors   uint32
}

// Build discovers and validates the graph rooted at config.EntryPath. Syntax
// and provider failures are reported through diagnostics; a partial immutable
// graph is returned when loading can continue safely.
func Build(config BuildConfig, provider SourceProvider, sources *source.FileSet, diagnostics *diagnostic.DiagnosticSet) *Graph {
	return build(config, provider, sources, diagnostics, defaultMaxDiagnostics)
}

func build(config BuildConfig, provider SourceProvider, sources *source.FileSet, diagnostics *diagnostic.DiagnosticSet, maxDiagnostics uint32) *Graph {
	if sources == nil {
		sources = source.NewFileSet()
	}
	if diagnostics == nil {
		diagnostics = diagnostic.NewDiagnosticSet()
	}
	b := &builder{
		config: config, provider: provider, sources: sources, diagnostics: diagnostics,
		graph:      &Graph{byKey: make(map[ModuleKey]ModuleID)},
		maxModules: config.MaxModules, maxDepth: config.MaxImportDepth,
		maxDiagnostics: maxDiagnostics,
	}
	if b.maxModules == 0 {
		b.maxModules = DefaultMaxModules
	}
	if b.maxDepth == 0 {
		b.maxDepth = DefaultMaxImportDepth
	}
	if provider == nil {
		b.report(CodeModuleUnavailable, "no source provider configured", source.Span{})
		return b.graph
	}

	entryName := entryDisplay(config.EntryPath)
	if config.PreludePath != "" {
		prelude, ok := b.loadCandidate(config.Package, config.PreludePath, entryDisplay(config.PreludePath), source.Span{})
		if !ok {
			return b.graph
		}
		b.graph.Prelude = b.addModule(prelude, 0, source.Span{}, RolePrelude)
		if b.graph.Prelude == 0 {
			return b.graph
		}
	}
	entry, ok := b.loadCandidate(config.Package, config.EntryPath, entryName, source.Span{})
	if !ok {
		return b.graph
	}
	b.graph.Root = b.addModule(entry, 0, source.Span{}, RoleNormal)
	for index := 0; index < len(b.graph.modules); index++ {
		b.processImports(ModuleID(index+1), b.depths[index])
	}
	b.validateCyclesAndOrder()
	return b.graph
}

func entryDisplay(entry string) string {
	clean := filepath.ToSlash(filepath.Clean(entry))
	if base := path.Base(clean); base != "." && base != "/" {
		return base
	}
	return clean
}

func (b *builder) addModule(item candidate, depth uint32, importSpan source.Span, role ModuleRole) ModuleID {
	if existing, ok := b.graph.byKey[item.key]; ok {
		return existing
	}
	if uint32(len(b.graph.modules)) >= b.maxModules {
		b.report(CodeResourceLimit, fmt.Sprintf("module limit of %d exceeded", b.maxModules), importSpan)
		return 0
	}

	id := ModuleID(len(b.graph.modules) + 1)
	sourceID, err := b.sources.Add(item.display, item.contents)
	if err != nil {
		b.report(CodeModuleUnavailable, fmt.Sprintf("cannot load module %q: %v", item.display, err), importSpan)
		return 0
	}
	file, _ := b.sources.File(sourceID)
	tree := syntax.Parse(file, b.diagnostics)
	b.graph.modules = append(b.graph.modules, Module{ID: id, Key: item.key, Source: sourceID, Tree: tree, Role: role})
	b.graph.byKey[item.key] = id
	b.displays = append(b.displays, item.display)
	b.depths = append(b.depths, depth)
	return id
}

func (b *builder) processImports(id ModuleID, depth uint32) {
	module := b.graph.modules[id-1]
	file, _ := b.sources.File(module.Source)
	tree := module.Tree
	imports := b.importNodes(file, tree)
	type seenImport struct {
		key  ModuleKey
		span source.Span
	}
	seenTargets := make(map[ModuleKey]seenImport)
	seenQualifiers := make(map[string]seenImport)
	for _, authored := range imports {
		resolved, ok := b.resolveImport(id, authored.spelling, authored.span)
		if !ok {
			continue
		}
		qualifier := importQualifier(authored.spelling)
		if previous, duplicate := seenTargets[resolved.key]; duplicate {
			b.reportRelated(CodeDuplicateImport, fmt.Sprintf("module %q is imported more than once", authored.spelling), authored.span,
				diagnostic.Label{Span: previous.span, Message: "first imported here"})
			continue
		}
		if previous, collision := seenQualifiers[qualifier]; collision && previous.key != resolved.key {
			b.reportRelated(CodeQualifierCollision, fmt.Sprintf("import qualifier %q refers to different modules", qualifier), authored.span,
				diagnostic.Label{Span: previous.span, Message: "first qualifier binding is here"})
			continue
		}
		seen := seenImport{key: resolved.key, span: authored.span}
		seenTargets[resolved.key] = seen
		seenQualifiers[qualifier] = seen
		target, exists := b.graph.byKey[resolved.key]
		if !exists {
			if depth == b.maxDepth {
				b.report(CodeResourceLimit, fmt.Sprintf("import depth limit of %d exceeded", b.maxDepth), authored.span)
				continue
			}
			target = b.addModule(resolved, depth+1, authored.span, RoleNormal)
		}
		if target == 0 {
			continue
		}
		edge := ImportEdge{Span: authored.span, Spelling: authored.spelling, Qualifier: qualifier, Target: target}
		b.graph.modules[id-1].Imports = append(b.graph.modules[id-1].Imports, edge)
	}
}

type authoredImport struct {
	span     source.Span
	spelling string
}

func (b *builder) importNodes(file *source.File, tree *syntax.Tree) []authoredImport {
	root, ok := tree.Node(tree.Root())
	if !ok {
		return nil
	}
	var result []authoredImport
	for _, childID := range root.Children() {
		child, ok := tree.Node(childID)
		if !ok || child.Kind() != syntax.ImportDecl {
			continue
		}
		children := child.Children()
		if len(children) == 0 {
			continue
		}
		literal, ok := tree.Node(children[0])
		if !ok || literal.Kind() != syntax.Literal || literal.Token() != syntax.StringLiteral {
			continue
		}
		quoted := string(file.Slice(literal.Span()))
		spelling, err := strconv.Unquote(quoted)
		if err != nil {
			continue // The lexer/parser already diagnosed malformed string syntax.
		}
		result = append(result, authoredImport{span: child.Span(), spelling: spelling})
	}
	return result
}

func (b *builder) resolveImport(importer ModuleID, spelling string, span source.Span) (candidate, bool) {
	route, ok := validateImportSpelling(spelling)
	if !ok {
		b.report(CodeInvalidImport, fmt.Sprintf("invalid import spelling %q", spelling), span)
		return candidate{}, false
	}
	current := b.graph.modules[importer-1]
	currentDisplay := b.displays[importer-1]

	switch route.kind {
	case importRelative:
		requested := path.Join(path.Dir(string(current.Key.Path)), route.path+".peb")
		display := path.Clean(path.Join(path.Dir(currentDisplay), route.path+".peb"))
		return b.loadCandidate(current.Key.Package, requested, display, span)
	case importStandard:
		if b.config.StandardRoot == "" {
			b.report(CodeModuleUnavailable, fmt.Sprintf("standard module %q has no configured root", spelling), span)
			return candidate{}, false
		}
		requested := filepath.Join(b.config.StandardRoot, filepath.FromSlash(route.path+".peb"))
		return b.loadCandidate(StandardPackage, requested, "std:"+route.path+".peb", span)
	case importBare:
		return b.resolveBare(route.path, spelling, span)
	default:
		panic("unreachable import route")
	}
}

func (b *builder) resolveBare(route, spelling string, span source.Span) (candidate, bool) {
	var matches []candidate
	var firstFailure error
	for _, root := range b.config.SearchRoots {
		requested := filepath.Join(root.Path, filepath.FromSlash(route+".peb"))
		canonical, err := b.provider.Canonicalize(requested)
		if err != nil {
			if providerFailure(err) != ProviderNotFound && firstFailure == nil {
				firstFailure = err
			}
			continue
		}
		contents, err := b.provider.ReadFile(canonical)
		if err != nil {
			if providerFailure(err) != ProviderNotFound && firstFailure == nil {
				firstFailure = err
			}
			continue
		}
		matches = append(matches, candidate{
			key:     ModuleKey{Package: root.Package, Path: canonical},
			display: string(root.Package) + ":" + route + ".peb", contents: contents,
		})
	}
	if len(matches) == 0 {
		if firstFailure != nil && providerFailure(firstFailure) == ProviderInvalidPath {
			b.report(CodeInvalidImport, fmt.Sprintf("invalid provider path for import %q", spelling), span)
		} else if firstFailure != nil {
			b.report(CodeModuleUnavailable, fmt.Sprintf("module %q is unreadable", spelling), span)
		} else {
			b.report(CodeModuleUnavailable, fmt.Sprintf("module %q was not found", spelling), span)
		}
		return candidate{}, false
	}
	owners := make(map[PackageID]struct{})
	for _, found := range matches {
		owners[found.key.Package] = struct{}{}
	}
	if len(owners) > 1 {
		b.report(CodeAmbiguousPackage, fmt.Sprintf("import %q is owned by multiple package roots", spelling), span)
		return candidate{}, false
	}
	return matches[0], true // Configured root order breaks same-package ties.
}

func (b *builder) loadCandidate(pkg PackageID, requested, display string, span source.Span) (candidate, bool) {
	canonical, err := b.provider.Canonicalize(requested)
	if err != nil {
		b.reportProviderFailure(display, err, span)
		return candidate{}, false
	}
	contents, err := b.provider.ReadFile(canonical)
	if err != nil {
		b.reportProviderFailure(display, err, span)
		return candidate{}, false
	}
	return candidate{key: ModuleKey{Package: pkg, Path: canonical}, display: display, contents: contents}, true
}

func (b *builder) reportProviderFailure(requested string, err error, span source.Span) {
	switch providerFailure(err) {
	case ProviderInvalidPath:
		b.report(CodeInvalidImport, fmt.Sprintf("invalid module path %q", requested), span)
	case ProviderNotFound:
		b.report(CodeModuleUnavailable, fmt.Sprintf("module %q was not found", requested), span)
	default:
		b.report(CodeModuleUnavailable, fmt.Sprintf("module %q is unreadable", requested), span)
	}
}

func providerFailure(err error) ProviderFailure {
	var providerError *ProviderError
	if errors.As(err, &providerError) {
		return providerError.Kind
	}
	return ProviderUnreadable
}

type importKind uint8

const (
	importRelative importKind = iota + 1
	importStandard
	importBare
)

type importRoute struct {
	kind importKind
	path string
}

func validateImportSpelling(spelling string) (importRoute, bool) {
	if spelling == "" || strings.Contains(spelling, "\\") || strings.HasSuffix(spelling, "/") || strings.HasSuffix(spelling, ".peb") || path.IsAbs(spelling) || filepath.IsAbs(spelling) {
		return importRoute{}, false
	}
	kind := importBare
	body := spelling
	if strings.HasPrefix(spelling, "std:") {
		kind = importStandard
		body = strings.TrimPrefix(spelling, "std:")
	} else if strings.HasPrefix(spelling, "./") || strings.HasPrefix(spelling, "../") {
		kind = importRelative
	} else if strings.Contains(spelling, ":") {
		return importRoute{}, false
	}
	if body == "" {
		return importRoute{}, false
	}
	parts := strings.Split(body, "/")
	for _, part := range parts {
		if part == "" {
			return importRoute{}, false
		}
		if kind != importRelative && (part == "." || part == "..") {
			return importRoute{}, false
		}
	}
	if last := parts[len(parts)-1]; last == "." || last == ".." {
		return importRoute{}, false
	}
	clean := path.Clean(body)
	if clean == "." || (kind != importRelative && strings.HasPrefix(clean, "../")) {
		return importRoute{}, false
	}
	return importRoute{kind: kind, path: clean}, true
}

func importQualifier(spelling string) string {
	body := strings.TrimPrefix(spelling, "std:")
	return path.Base(path.Clean(body))
}

func (b *builder) report(code diagnostic.Code, message string, span source.Span) {
	if b.moduleErrors >= b.maxDiagnostics {
		return
	}
	b.diagnostics.Error(code, message, span)
	b.moduleErrors++
}

func (b *builder) reportRelated(code diagnostic.Code, message string, span source.Span, related ...diagnostic.Label) {
	if b.moduleErrors >= b.maxDiagnostics {
		return
	}
	b.diagnostics.Add(diagnostic.Diagnostic{
		Severity: diagnostic.Error, Code: code, Message: message,
		Primary: diagnostic.Label{Span: span}, Related: append([]diagnostic.Label(nil), related...),
	})
	b.moduleErrors++
}
