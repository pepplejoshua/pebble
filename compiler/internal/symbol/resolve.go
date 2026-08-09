package symbol

import (
	"fmt"
	"strconv"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type resolver struct {
	graph               *module.Graph
	sources             *source.FileSet
	diagnostics         *diagnostic.DiagnosticSet
	config              Config
	result              *Result
	modules             map[ModuleID]module.Module
	moduleScopes        map[ModuleID]ScopeID
	bindings            map[ScopeID]map[string]SymbolID
	memberBindings      map[SymbolID]map[string]SymbolID
	functionScopes      map[SyntaxRef]ScopeID
	typeScopes          map[SyntaxRef]ScopeID
	functionSymbols     map[SyntaxRef]SymbolID
	typeSymbols         map[SyntaxRef]SymbolID
	moduleBindings      map[SyntaxRef]SymbolID
	symbolFunctions     map[SymbolID]SyntaxRef
	nameDiagnostics     []diagnostic.Diagnostic
	diagnosticOverflow  bool
	symbolLimitReported bool
	scopeLimitReported  bool
	depthLimitReported  bool
}

// Resolve collects declarations and resolves names without mutating graph trees.
func Resolve(graph *module.Graph, sources *source.FileSet, diagnostics *diagnostic.DiagnosticSet, config Config) *Result {
	config = normalizedConfig(config)
	if diagnostics == nil {
		diagnostics = diagnostic.NewDiagnosticSet()
	}
	r := &resolver{
		graph: graph, sources: sources, diagnostics: diagnostics, config: config,
		result: &Result{
			Scopes: &ScopeStore{}, Symbols: &SymbolStore{},
			references: make(map[SyntaxRef]Resolution), qualifiers: make(map[SyntaxRef]ModuleID),
			brackets: make(map[SyntaxRef]BracketMode), captures: make(map[SyntaxRef][]SymbolID),
			members: make(map[SymbolID][]SymbolID),
		},
		modules: make(map[ModuleID]module.Module), moduleScopes: make(map[ModuleID]ScopeID),
		bindings: make(map[ScopeID]map[string]SymbolID), memberBindings: make(map[SymbolID]map[string]SymbolID),
		functionScopes: make(map[SyntaxRef]ScopeID), typeScopes: make(map[SyntaxRef]ScopeID),
		functionSymbols: make(map[SyntaxRef]SymbolID), typeSymbols: make(map[SyntaxRef]SymbolID),
		moduleBindings:  make(map[SyntaxRef]SymbolID),
		symbolFunctions: make(map[SymbolID]SyntaxRef),
	}
	if graph == nil {
		r.report(CodeResourceLimit, "name resolution requires a module graph", source.Span{})
		r.flushDiagnostics()
		return r.result
	}
	if sources == nil {
		r.report(CodeResourceLimit, "name resolution requires the compilation source set", source.Span{})
		r.flushDiagnostics()
		return r.result
	}
	for _, item := range graph.Modules() {
		r.modules[item.ID] = item
	}
	r.installPrelude()
	// Module scopes are allocated in graph ID order so qualified lookup can target
	// any reachable module before reference resolution begins.
	for _, item := range graph.Modules() {
		origin := SyntaxRef{Module: item.ID}
		if item.Tree != nil {
			origin.Node = item.Tree.Root()
		}
		scope := r.newScope(ScopeModule, r.result.prelude, item.ID, 0, origin)
		r.moduleScopes[item.ID] = scope
	}
	for _, item := range graph.Modules() {
		r.collectModule(item)
	}
	r.registerBuiltinFunctions()
	for _, item := range graph.Modules() {
		r.resolveModule(item)
	}
	r.flushDiagnostics()
	return r.result
}

// registerBuiltinFunctions binds the compiler-owned builtin function symbols
// into the prelude scope. They are deliberately registered AFTER module
// collection — rather than in installPrelude with the other prelude symbols —
// so the two extra prelude identities never renumber the module symbols that
// resolve against them (the golden typed-IR dumps and the backend's
// hardcoded pebble_fn_<symbolID> fixtures depend on module symbol IDs being
// stable). Because module collection only registers declarations and never
// performs name lookup, the prelude bindings need only exist before
// resolveModule runs, which is exactly when this runs.
func (r *resolver) registerBuiltinFunctions() {
	scope := r.result.prelude
	if scope == 0 {
		return
	}
	for _, function := range []struct {
		name string
		kind BuiltinFunction
	}{
		{"wrapping_mul_u64", BuiltinWrappingMulU64},
		{"wrapping_add_u64", BuiltinWrappingAddU64},
	} {
		id := r.addSymbol(Symbol{Name: function.name, Kind: SymbolBuiltinFunction, Scope: scope, BuiltinFunction: function.kind}, true, 0)
		r.result.builtinFunctions[function.kind] = id
	}
}

func (r *resolver) installPrelude() {
	scope := r.newScope(ScopePrelude, 0, 0, 0, SyntaxRef{})
	r.result.prelude = scope
	if scope == 0 {
		return
	}
	for kind := BuiltinBool; kind <= BuiltinF64; kind++ {
		id := r.addSymbol(Symbol{Name: kind.String(), Kind: SymbolBuiltinType, Scope: scope, Builtin: kind}, true, 0)
		r.result.builtins[kind] = id
	}
	allocator := r.addSymbol(Symbol{Name: "Allocator", Kind: SymbolRuntimeType, Scope: scope, Runtime: RuntimeAllocator}, true, 0)
	context := r.addSymbol(Symbol{Kind: SymbolRuntimeType, Scope: scope, Runtime: RuntimeContext}, false, 0)
	r.result.runtimes[RuntimeAllocator] = allocator
	r.result.runtimes[RuntimeContext] = context
	for _, member := range []struct {
		owner SymbolID
		name  string
	}{
		{allocator, "ptr"},
		{allocator, "alloc"},
		{allocator, "realloc"},
		{allocator, "free"},
		{context, "default_allocator"},
	} {
		if member.owner == 0 {
			continue
		}
		id := r.addSymbol(Symbol{Name: member.name, Kind: SymbolField, Containing: member.owner}, false, 0)
		if id != 0 {
			r.result.members[member.owner] = append(r.result.members[member.owner], id)
		}
	}
}

func normalizedConfig(c Config) Config {
	if c.MaxSymbols == 0 {
		c.MaxSymbols = DefaultMaxSymbols
	}
	if c.MaxScopes == 0 {
		c.MaxScopes = DefaultMaxScopes
	}
	if c.MaxScopeDepth == 0 {
		c.MaxScopeDepth = DefaultMaxScopeDepth
	}
	if c.MaxDiagnostics == 0 {
		c.MaxDiagnostics = DefaultMaxDiagnostics
	}
	return c
}

func (r *resolver) collectModule(item module.Module) {
	scope := r.moduleScopes[item.ID]
	if scope == 0 || item.Tree == nil {
		return
	}
	file, ok := r.sources.File(item.Source)
	if !ok || file == nil {
		r.report(CodeResourceLimit, fmt.Sprintf("source %d for module %d is absent from the compilation source set", item.Source, item.ID), source.Span{Source: item.Source})
		return
	}
	root, ok := item.Tree.Node(item.Tree.Root())
	if !ok {
		r.report(CodeResourceLimit, fmt.Sprintf("module %d has an inconsistent syntax tree", item.ID), source.Span{Source: item.Source})
		return
	}
	edges := make(map[source.Span][]module.ImportEdge)
	for _, edge := range item.Imports {
		edges[edge.Span] = append(edges[edge.Span], edge)
	}
	for _, childID := range root.Children() {
		node, ok := item.Tree.Node(childID)
		if !ok {
			continue
		}
		ref := SyntaxRef{Module: item.ID, Node: childID}
		switch node.Kind() {
		case syntax.ImportDecl:
			candidates := edges[node.Span()]
			if len(candidates) != 0 {
				r.collectImport(item, file, scope, ref, node, candidates[0])
			}
		case syntax.BindingDecl:
			r.moduleBindings[ref] = r.collectNamed(item, file, scope, ref, node, SymbolBinding, 0, 0, false)
		case syntax.TypeDecl:
			r.collectType(item, file, scope, ref, node)
		case syntax.FunctionDecl:
			r.collectFunction(item, file, scope, ref, node, SymbolFunction, 0)
		case syntax.ExternDecl:
			r.collectExtern(item, file, scope, childID, node)
		}
	}
}

func (r *resolver) collectImport(item module.Module, file *source.File, scope ScopeID, ref SyntaxRef, node syntax.Node, edge module.ImportEdge) {
	spelling := ""
	children := node.Children()
	if len(children) > 0 {
		if literal, ok := item.Tree.Node(children[0]); ok {
			if text := file.Slice(literal.Span()); text != nil {
				spelling, _ = strconv.Unquote(string(text))
			}
		}
	}
	if spelling != edge.Spelling {
		return
	}
	r.addSymbol(Symbol{Name: edge.Qualifier, Kind: SymbolModule, Span: node.Span(), Module: item.ID, Scope: scope, Declaration: ref, ImportTarget: edge.Target}, true, 0)
}

func (r *resolver) collectType(item module.Module, file *source.File, parent ScopeID, ref SyntaxRef, node syntax.Node) {
	id := r.collectNamed(item, file, parent, ref, node, SymbolType, 0, 0, hasChildKind(item.Tree, node, syntax.TypeParameter))
	if id == 0 {
		return
	}
	r.typeSymbols[ref] = id
	typeScope := r.newScope(ScopeType, parent, item.ID, id, ref)
	if typeScope == 0 {
		return
	}
	r.typeScopes[ref] = typeScope
	children := node.Children()
	for _, childID := range children {
		child, ok := item.Tree.Node(childID)
		if !ok {
			continue
		}
		if child.Kind() == syntax.TypeParameter {
			r.collectNestedName(item, file, typeScope, childID, child, SymbolTypeParameter, id, false)
		}
	}
	for _, childID := range children {
		child, ok := item.Tree.Node(childID)
		if !ok {
			continue
		}
		if child.Kind() == syntax.StructType || child.Kind() == syntax.UnionType || child.Kind() == syntax.EnumType {
			r.collectAggregate(item, file, typeScope, id, childID, child)
		}
	}
}

func (r *resolver) collectAggregate(item module.Module, file *source.File, typeScope ScopeID, owner SymbolID, nodeID syntax.NodeID, node syntax.Node) {
	for _, childID := range node.Children() {
		child, ok := item.Tree.Node(childID)
		if !ok {
			continue
		}
		switch child.Kind() {
		case syntax.FieldDecl:
			r.collectMemberNames(item, file, typeScope, owner, childID, child, SymbolField)
		case syntax.VariantDecl:
			r.collectMemberNames(item, file, typeScope, owner, childID, child, SymbolVariant)
		case syntax.Name: // enum variants are direct Name children.
			r.collectMemberName(item, file, typeScope, owner, SyntaxRef{Module: item.ID, Node: childID}, child, SymbolVariant)
		case syntax.FunctionDecl:
			ref := SyntaxRef{Module: item.ID, Node: childID}
			r.collectFunction(item, file, typeScope, ref, child, SymbolMethod, owner)
		}
	}
}

func (r *resolver) collectMemberNames(item module.Module, file *source.File, scope ScopeID, owner SymbolID, declNode syntax.NodeID, node syntax.Node, kind SymbolKind) {
	children := semanticNodeIDs(item.Tree, node.Children())
	if len(children) != 0 {
		children = children[:len(children)-1] // The final semantic child is the member type.
	}
	for _, childID := range children {
		child, ok := item.Tree.Node(childID)
		if !ok || child.Kind() != syntax.Name {
			continue
		}
		r.collectMemberName(item, file, scope, owner, SyntaxRef{Module: item.ID, Node: declNode}, child, kind)
	}
}

func (r *resolver) collectMemberName(item module.Module, file *source.File, scope ScopeID, owner SymbolID, decl SyntaxRef, nameNode syntax.Node, kind SymbolKind) SymbolID {
	name := r.nodeText(file, nameNode)
	symbol := Symbol{Name: name, Kind: kind, Span: nameNode.Span(), Module: item.ID, Scope: scope, Declaration: decl, Containing: owner, Error: name == ""}
	return r.addMemberSymbol(symbol, owner)
}

func (r *resolver) collectFunction(item module.Module, file *source.File, parent ScopeID, ref SyntaxRef, node syntax.Node, kind SymbolKind, containing SymbolID) {
	generic := hasChildKind(item.Tree, node, syntax.TypeParameter)
	var id SymbolID
	if kind == SymbolMethod {
		nameNode, ok := firstDirectChild(item.Tree, node, syntax.Name)
		if !ok {
			id = r.addMemberSymbol(Symbol{Kind: SymbolError, Span: node.Span(), Module: item.ID, Scope: parent, Declaration: ref, Containing: containing, Generic: generic, Error: true}, containing)
		} else {
			name := r.nodeText(file, nameNode)
			id = r.addMemberSymbol(Symbol{Name: name, Kind: kind, Span: nameNode.Span(), Module: item.ID, Scope: parent, Declaration: ref, Containing: containing, Generic: generic, Error: name == ""}, containing)
		}
	} else {
		id = r.collectNamed(item, file, parent, ref, node, kind, containing, 0, generic)
	}
	if id == 0 {
		return
	}
	r.functionSymbols[ref] = id
	fnScope := r.newScope(ScopeFunction, parent, item.ID, id, ref)
	if fnScope == 0 {
		return
	}
	r.functionScopes[ref] = fnScope
	for _, childID := range node.Children() {
		child, ok := item.Tree.Node(childID)
		if !ok {
			continue
		}
		switch child.Kind() {
		case syntax.TypeParameter:
			r.collectNestedName(item, file, fnScope, childID, child, SymbolTypeParameter, id, false)
		case syntax.Parameter:
			r.collectParameter(item, file, fnScope, id, childID, child)
		}
	}
}

func (r *resolver) collectParameter(item module.Module, file *source.File, scope ScopeID, owner SymbolID, nodeID syntax.NodeID, node syntax.Node) {
	children := semanticNodeIDs(item.Tree, node.Children())
	if len(children) != 0 {
		children = children[:len(children)-1] // The final semantic child is the parameter type.
	}
	for _, childID := range children {
		child, ok := item.Tree.Node(childID)
		if !ok || child.Kind() != syntax.Name {
			continue
		}
		name := r.nodeText(file, child)
		r.addSymbol(Symbol{Name: name, Kind: SymbolParameter, Span: child.Span(), Module: item.ID, Scope: scope, Declaration: SyntaxRef{Module: item.ID, Node: nodeID}, Containing: owner, Error: name == ""}, true, owner)
	}
}

func (r *resolver) collectNestedName(item module.Module, file *source.File, scope ScopeID, nodeID syntax.NodeID, node syntax.Node, kind SymbolKind, containing SymbolID, generic bool) SymbolID {
	children := node.Children()
	if len(children) == 0 {
		return r.addSymbol(Symbol{Kind: SymbolError, Span: node.Span(), Module: item.ID, Scope: scope, Declaration: SyntaxRef{Module: item.ID, Node: nodeID}, Containing: containing, Error: true}, false, containing)
	}
	nameNode, ok := item.Tree.Node(children[0])
	if !ok {
		return 0
	}
	name := r.nodeText(file, nameNode)
	return r.addSymbol(Symbol{Name: name, Kind: kind, Span: nameNode.Span(), Module: item.ID, Scope: scope, Declaration: SyntaxRef{Module: item.ID, Node: nodeID}, Containing: containing, Generic: generic, Error: name == ""}, true, containing)
}

func (r *resolver) collectNamed(item module.Module, file *source.File, scope ScopeID, ref SyntaxRef, node syntax.Node, kind SymbolKind, containing SymbolID, importTarget ModuleID, generic bool) SymbolID {
	nameNode, ok := firstDirectChild(item.Tree, node, syntax.Name)
	if !ok {
		return r.addSymbol(Symbol{Kind: SymbolError, Span: node.Span(), Module: item.ID, Scope: scope, Declaration: ref, Containing: containing, ImportTarget: importTarget, Generic: generic, Error: true}, false, containing)
	}
	name := r.nodeText(file, nameNode)
	return r.addSymbol(Symbol{Name: name, Kind: kind, Span: nameNode.Span(), Module: item.ID, Scope: scope, Declaration: ref, Containing: containing, ImportTarget: importTarget, Generic: generic, Error: name == ""}, true, containing)
}

func (r *resolver) collectExtern(item module.Module, file *source.File, scope ScopeID, nodeID syntax.NodeID, node syntax.Node) {
	var visit func(syntax.NodeID)
	visit = func(id syntax.NodeID) {
		n, ok := item.Tree.Node(id)
		if !ok {
			return
		}
		ref := SyntaxRef{Module: item.ID, Node: id}
		switch n.Kind() {
		case syntax.ExternFunction:
			r.collectFunction(item, file, scope, ref, n, SymbolExternFunction, 0)
			return
		case syntax.ExternType:
			r.collectNamed(item, file, scope, ref, n, SymbolExternType, 0, 0, false)
			return
		case syntax.ExternBinding:
			r.collectNamed(item, file, scope, ref, n, SymbolExternBinding, 0, 0, false)
			return
		}
		for _, child := range n.Children() {
			visit(child)
		}
	}
	visit(nodeID)
}

func (r *resolver) addSymbol(symbol Symbol, bind bool, functionOwner SymbolID) SymbolID {
	if uint32(len(r.result.Symbols.values)) >= r.config.MaxSymbols {
		if !r.symbolLimitReported {
			r.symbolLimitReported = true
			r.report(CodeResourceLimit, fmt.Sprintf("symbol limit of %d exceeded", r.config.MaxSymbols), symbol.Span)
		}
		return 0
	}
	symbol.ID = SymbolID(len(r.result.Symbols.values) + 1)
	if symbol.Builtin == 0 && symbol.Runtime == 0 && symbol.Name != "" && !symbol.Error && reservedBuiltin(symbol.Name) {
		symbol.Error = true
		r.report(CodeReservedBuiltin, fmt.Sprintf("%q is a reserved compiler-owned type name", symbol.Name), symbol.Span)
	}
	if bind && symbol.Name != "" && !symbol.Error {
		if original, duplicate := r.bindings[symbol.Scope][symbol.Name]; duplicate {
			symbol.Error = true
			r.duplicate(symbol, original)
		} else {
			r.bindings[symbol.Scope][symbol.Name] = symbol.ID
		}
	}
	r.result.Symbols.values = append(r.result.Symbols.values, symbol)
	r.appendScopeSymbol(symbol.Scope, symbol.ID)
	if functionOwner != 0 {
		if owner, ok := r.result.Symbols.Symbol(functionOwner); ok {
			r.symbolFunctions[symbol.ID] = owner.Declaration
		}
	}
	return symbol.ID
}

func reservedBuiltin(name string) bool {
	if name == "Allocator" {
		return true
	}
	for kind := BuiltinBool; kind <= BuiltinF64; kind++ {
		if name == kind.String() {
			return true
		}
	}
	return false
}

func (r *resolver) bindExistingMember(id, owner SymbolID) SymbolID {
	if id == 0 {
		return 0
	}
	symbol := &r.result.Symbols.values[id-1]
	if symbol.Error || symbol.Name == "" {
		return id
	}
	if original, duplicate := r.memberBindings[owner][symbol.Name]; duplicate {
		symbol.Error = true
		r.duplicate(*symbol, original)
	} else {
		r.memberBindings[owner][symbol.Name] = id
		r.result.members[owner] = append(r.result.members[owner], id)
	}
	return id
}

func (r *resolver) addMemberSymbol(symbol Symbol, owner SymbolID) SymbolID {
	id := r.addSymbol(symbol, false, owner)
	if id != 0 {
		r.bindExistingMember(id, owner)
	}
	return id
}

func (r *resolver) duplicate(symbol Symbol, original SymbolID) {
	old, _ := r.result.Symbols.Symbol(original)
	r.reportDiagnostic(diagnostic.Diagnostic{Severity: diagnostic.Error, Code: CodeDuplicate, Message: fmt.Sprintf("duplicate declaration of %q", symbol.Name), Primary: diagnostic.Label{Span: symbol.Span, Message: "duplicate declaration"}, Related: []diagnostic.Label{{Span: old.Span, Message: "first declared here"}}})
}

func (r *resolver) newScope(kind ScopeKind, parent ScopeID, moduleID ModuleID, owner SymbolID, origin SyntaxRef) ScopeID {
	depth := uint32(0)
	if parent != 0 {
		if p, ok := r.result.Scopes.Scope(parent); ok {
			depth = p.Depth + 1
		}
	}
	span := source.Span{}
	if item, ok := r.modules[moduleID]; ok && item.Tree != nil {
		if n, ok := item.Tree.Node(origin.Node); ok {
			span = n.Span()
		}
	}
	if depth > r.config.MaxScopeDepth {
		if !r.depthLimitReported {
			r.depthLimitReported = true
			r.report(CodeResourceLimit, fmt.Sprintf("scope depth limit of %d exceeded", r.config.MaxScopeDepth), span)
		}
		return 0
	}
	if uint32(len(r.result.Scopes.values)) >= r.config.MaxScopes {
		if !r.scopeLimitReported {
			r.scopeLimitReported = true
			r.report(CodeResourceLimit, fmt.Sprintf("scope limit of %d exceeded", r.config.MaxScopes), span)
		}
		return 0
	}
	id := ScopeID(len(r.result.Scopes.values) + 1)
	r.result.Scopes.values = append(r.result.Scopes.values, Scope{ID: id, Kind: kind, Parent: parent, Module: moduleID, Owner: owner, Origin: origin, Depth: depth})
	r.bindings[id] = make(map[string]SymbolID)
	if kind == ScopeType {
		r.memberBindings[owner] = make(map[string]SymbolID)
	}
	return id
}

func (r *resolver) appendScopeSymbol(scope ScopeID, symbol SymbolID) {
	if scope == 0 || uint64(scope) > uint64(len(r.result.Scopes.values)) {
		return
	}
	r.result.Scopes.values[scope-1].Symbols = append(r.result.Scopes.values[scope-1].Symbols, symbol)
}

func (r *resolver) nodeText(file *source.File, node syntax.Node) string {
	if node.Kind() != syntax.Name {
		return ""
	}
	return string(file.Slice(node.Span()))
}

func firstDirectChild(tree *syntax.Tree, node syntax.Node, kind syntax.NodeKind) (syntax.Node, bool) {
	for _, id := range node.Children() {
		if child, ok := tree.Node(id); ok && child.Kind() == kind {
			return child, true
		}
	}
	return syntax.Node{}, false
}

func hasChildKind(tree *syntax.Tree, node syntax.Node, kind syntax.NodeKind) bool {
	for _, id := range node.Children() {
		if child, ok := tree.Node(id); ok && child.Kind() == kind {
			return true
		}
	}
	return false
}

func semanticNodeIDs(tree *syntax.Tree, ids []syntax.NodeID) []syntax.NodeID {
	result := make([]syntax.NodeID, 0, len(ids))
	for _, id := range ids {
		if node, ok := tree.Node(id); ok && node.Kind() != syntax.Missing && node.Kind() != syntax.Error {
			result = append(result, id)
		}
	}
	return result
}

func (r *resolver) report(code diagnostic.Code, message string, span source.Span) {
	r.reportDiagnostic(diagnostic.Diagnostic{Severity: diagnostic.Error, Code: code, Message: message, Primary: diagnostic.Label{Span: span}})
}
func (r *resolver) reportDiagnostic(item diagnostic.Diagnostic) {
	if uint32(len(r.nameDiagnostics)) < r.config.MaxDiagnostics {
		r.nameDiagnostics = append(r.nameDiagnostics, item)
		return
	}
	r.diagnosticOverflow = true
}
func (r *resolver) flushDiagnostics() {
	if r.diagnosticOverflow && len(r.nameDiagnostics) != 0 {
		last := len(r.nameDiagnostics) - 1
		r.nameDiagnostics[last] = diagnostic.Diagnostic{Severity: diagnostic.Error, Code: CodeResourceLimit, Message: fmt.Sprintf("name-resolution diagnostic limit of %d reached", r.config.MaxDiagnostics), Primary: r.nameDiagnostics[last].Primary}
	}
	for _, item := range r.nameDiagnostics {
		r.diagnostics.Add(item)
	}
}
