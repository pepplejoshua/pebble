package symbol

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type nameContext uint8

const (
	contextAny nameContext = iota
	contextType
	contextValue
)

type functionContext struct {
	ref       SyntaxRef
	symbol    SymbolID
	anonymous bool
}

type walkContext struct {
	module   module.Module
	file     *source.File
	scope    ScopeID
	function functionContext
}

func (r *resolver) resolveModule(item module.Module) {
	scope := r.moduleScopes[item.ID]
	file, fileOK := r.sources.File(item.Source)
	if scope == 0 || !fileOK || file == nil || item.Tree == nil {
		return
	}
	root, ok := item.Tree.Node(item.Tree.Root())
	if !ok {
		return
	}
	ctx := walkContext{module: item, file: file, scope: scope}
	for _, childID := range root.Children() {
		node, ok := item.Tree.Node(childID)
		if !ok {
			continue
		}
		switch node.Kind() {
		case syntax.BindingDecl:
			r.resolveBinding(ctx, childID, node, true)
		case syntax.TypeDecl:
			r.resolveTypeDeclaration(ctx, childID, node)
		case syntax.FunctionDecl:
			r.resolveFunctionDeclaration(ctx, childID, node)
		case syntax.ExternDecl:
			r.resolveExternDeclaration(ctx, childID, node)
		}
	}
}

func (r *resolver) resolveTypeDeclaration(ctx walkContext, nodeID syntax.NodeID, node syntax.Node) {
	ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	typeScope := r.typeScopes[ref]
	if typeScope == 0 {
		typeScope = ctx.scope
	}
	typeCtx := ctx
	typeCtx.scope = typeScope
	seenDeclarationName := false
	for _, childID := range node.Children() {
		child, ok := ctx.module.Tree.Node(childID)
		if !ok {
			continue
		}
		switch child.Kind() {
		case syntax.Name:
			if !seenDeclarationName {
				seenDeclarationName = true
				continue
			}
			r.resolveType(typeCtx, childID)
		case syntax.TypeParameter, syntax.Missing, syntax.Error:
			continue
		case syntax.StructType, syntax.UnionType, syntax.EnumType:
			r.resolveAggregate(typeCtx, childID, child, r.typeSymbols[ref])
		default:
			r.resolveType(typeCtx, childID)
		}
	}
}

func (r *resolver) resolveAggregate(ctx walkContext, nodeID syntax.NodeID, node syntax.Node, owner SymbolID) {
	for _, childID := range node.Children() {
		child, ok := ctx.module.Tree.Node(childID)
		if !ok {
			continue
		}
		switch child.Kind() {
		case syntax.FieldDecl, syntax.VariantDecl:
			parts := semanticNodeIDs(ctx.module.Tree, child.Children())
			if len(parts) != 0 {
				r.resolveType(ctx, parts[len(parts)-1])
			}
		case syntax.FunctionDecl:
			r.resolveFunctionDeclaration(ctx, childID, child)
		case syntax.Name, syntax.Literal, syntax.Missing, syntax.Error: // enum name or tagged marker
		default:
			r.resolveType(ctx, childID)
		}
	}
}

func (r *resolver) resolveFunctionDeclaration(ctx walkContext, nodeID syntax.NodeID, node syntax.Node) {
	ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	fnScope := r.functionScopes[ref]
	if fnScope == 0 {
		return
	}
	fnCtx := ctx
	fnCtx.scope = fnScope
	fnCtx.function = functionContext{ref: ref, symbol: r.functionSymbols[ref]}
	r.resolveFunctionParts(fnCtx, node, node.Kind() != syntax.ExternFunction)
}

func (r *resolver) resolveFunctionParts(ctx walkContext, node syntax.Node, hasBody bool) {
	children := node.Children()
	body := syntax.NodeID(0)
	if hasBody && node.Data()&syntax.FunctionBodyPresent != 0 {
		semantic := semanticNodeIDs(ctx.module.Tree, children)
		if len(semantic) != 0 {
			body = semantic[len(semantic)-1]
		}
	}
	seenDeclarationName := node.Kind() == syntax.FunctionTerm
	for _, childID := range children {
		child, ok := ctx.module.Tree.Node(childID)
		if !ok {
			continue
		}
		if childID == body && node.Data()&syntax.FunctionExpressionBody != 0 {
			r.resolveExpression(ctx, childID)
			continue
		}
		switch child.Kind() {
		case syntax.Name:
			if !seenDeclarationName {
				seenDeclarationName = true
				continue
			}
			r.resolveType(ctx, childID)
		case syntax.TypeParameter, syntax.Literal, syntax.Missing, syntax.Error:
		case syntax.Parameter:
			parts := semanticNodeIDs(ctx.module.Tree, child.Children())
			if len(parts) != 0 {
				r.resolveType(ctx, parts[len(parts)-1])
			}
		case syntax.BlockStmt:
			r.resolveBlock(ctx, childID, child, true)
		default:
			r.resolveType(ctx, childID)
		}
	}
}

func (r *resolver) resolveExternDeclaration(ctx walkContext, nodeID syntax.NodeID, node syntax.Node) {
	var visit func(syntax.NodeID)
	visit = func(id syntax.NodeID) {
		n, ok := ctx.module.Tree.Node(id)
		if !ok {
			return
		}
		switch n.Kind() {
		case syntax.ExternFunction:
			r.resolveFunctionDeclaration(ctx, id, n)
			return
		case syntax.ExternType:
			return
		case syntax.ExternBinding:
			for _, childID := range n.Children() {
				if child, ok := ctx.module.Tree.Node(childID); ok && child.Kind() != syntax.Name {
					r.resolveType(ctx, childID)
				}
			}
			return
		}
		for _, childID := range n.Children() {
			visit(childID)
		}
	}
	visit(nodeID)
}

func (r *resolver) resolveBinding(ctx walkContext, nodeID syntax.NodeID, node syntax.Node, hoisted bool) SymbolID {
	children := node.Children()
	index := 1 // child zero is the declaration name, including when it is missing.
	if node.Data()&syntax.BindingTypePresent != 0 && index < len(children) {
		r.resolveType(ctx, children[index])
		index++
	}
	if node.Data()&syntax.BindingInitializerPresent != 0 && index < len(children) {
		r.resolveExpression(ctx, children[index])
	}
	ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	if hoisted {
		return r.moduleBindings[ref]
	}
	nameNode, ok := firstDirectChild(ctx.module.Tree, node, syntax.Name)
	if !ok {
		return r.addSymbol(Symbol{Kind: SymbolError, Span: node.Span(), Module: ctx.module.ID, Scope: ctx.scope, Declaration: ref, Error: true}, false, ctx.function.symbol)
	}
	name := r.nodeText(ctx.file, nameNode)
	id := r.addSymbol(Symbol{Name: name, Kind: SymbolBinding, Span: nameNode.Span(), Module: ctx.module.ID, Scope: ctx.scope, Declaration: ref, Containing: ctx.function.symbol, Error: name == ""}, true, ctx.function.symbol)
	if id != 0 && ctx.function.ref.Node != 0 {
		r.symbolFunctions[id] = ctx.function.ref
	}
	return id
}

func (r *resolver) resolveBlock(ctx walkContext, nodeID syntax.NodeID, node syntax.Node, reuse bool) {
	blockCtx := ctx
	if !reuse {
		child := r.newScope(ScopeBlock, ctx.scope, ctx.module.ID, ctx.function.symbol, SyntaxRef{Module: ctx.module.ID, Node: nodeID})
		if child == 0 {
			return
		}
		blockCtx.scope = child
	}
	for _, childID := range node.Children() {
		r.resolveStatement(blockCtx, childID)
	}
}

func (r *resolver) resolveStatement(ctx walkContext, nodeID syntax.NodeID) {
	node, ok := ctx.module.Tree.Node(nodeID)
	if !ok {
		return
	}
	switch node.Kind() {
	case syntax.BlockStmt:
		r.resolveBlock(ctx, nodeID, node, false)
	case syntax.BindingDecl:
		r.resolveBinding(ctx, nodeID, node, false)
	case syntax.IfStmt:
		children := node.Children()
		if len(children) > 0 {
			r.resolveExpression(ctx, children[0])
		}
		for _, id := range children[1:] {
			r.resolveStatement(ctx, id)
		}
	case syntax.WhileStmt:
		children := node.Children()
		if len(children) > 0 {
			r.resolveExpression(ctx, children[0])
		}
		if len(children) > 1 {
			r.resolveStatement(ctx, children[1])
		}
	case syntax.RangeLoopStmt:
		r.resolveRangeLoop(ctx, nodeID, node)
	case syntax.ForStmt:
		r.resolveFor(ctx, nodeID, node)
	case syntax.SwitchStmt:
		r.resolveSwitch(ctx, node)
	case syntax.SwitchCase:
		children := node.Children()
		for i, id := range children {
			if i == len(children)-1 {
				r.resolveStatement(ctx, id)
			} else {
				r.resolveExpression(ctx, id)
			}
		}
	case syntax.DeferStmt:
		for _, id := range node.Children() {
			r.resolveStatement(ctx, id)
		}
	case syntax.ReturnStmt, syntax.PrintStmt, syntax.AssignmentStmt, syntax.ExpressionStmt:
		for _, id := range node.Children() {
			r.resolveExpression(ctx, id)
		}
	case syntax.Missing, syntax.Error, syntax.BreakStmt, syntax.ContinueStmt:
	default:
		r.resolveExpression(ctx, nodeID)
	}
}

func (r *resolver) resolveRangeLoop(ctx walkContext, nodeID syntax.NodeID, node syntax.Node) {
	children := node.Children()
	if len(children) < 3 {
		for _, id := range children {
			r.resolveExpression(ctx, id)
		}
		return
	}
	r.resolveExpression(ctx, children[0])
	r.resolveExpression(ctx, children[1])
	loopScope := r.newScope(ScopeRangeLoop, ctx.scope, ctx.module.ID, ctx.function.symbol, SyntaxRef{Module: ctx.module.ID, Node: nodeID})
	if loopScope == 0 {
		return
	}
	loopCtx := ctx
	loopCtx.scope = loopScope
	bodyIndex := len(children) - 1
	if bodyIndex > 1 {
		candidate, ok := ctx.module.Tree.Node(children[2])
		if ok && candidate.Kind() == syntax.Name {
			name := r.nodeText(ctx.file, candidate)
			ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
			id := r.addSymbol(Symbol{Name: name, Kind: SymbolLoopBinding, Span: candidate.Span(), Module: ctx.module.ID, Scope: loopScope, Declaration: ref, Containing: ctx.function.symbol, Error: name == ""}, true, ctx.function.symbol)
			if id != 0 && ctx.function.ref.Node != 0 {
				r.symbolFunctions[id] = ctx.function.ref
			}
		}
	}
	r.resolveStatement(loopCtx, children[bodyIndex])
}

func (r *resolver) resolveFor(ctx walkContext, nodeID syntax.NodeID, node syntax.Node) {
	forScope := r.newScope(ScopeFor, ctx.scope, ctx.module.ID, ctx.function.symbol, SyntaxRef{Module: ctx.module.ID, Node: nodeID})
	if forScope == 0 {
		return
	}
	forCtx := ctx
	forCtx.scope = forScope
	children := node.Children()
	semantic := make([]syntax.NodeID, 0, len(children))
	for _, id := range children {
		if n, ok := ctx.module.Tree.Node(id); ok && n.Kind() != syntax.Missing && n.Kind() != syntax.Error {
			semantic = append(semantic, id)
		}
	}
	index := 0
	if node.Data()&syntax.ForInitializerPresent != 0 && index < len(semantic) {
		n, _ := ctx.module.Tree.Node(semantic[index])
		if n.Kind() == syntax.BindingDecl {
			r.resolveBinding(forCtx, semantic[index], n, false)
		} else {
			r.resolveStatement(forCtx, semantic[index])
		}
		index++
	}
	if node.Data()&syntax.ForConditionPresent != 0 && index < len(semantic) {
		r.resolveExpression(forCtx, semantic[index])
		index++
	}
	if node.Data()&syntax.ForUpdatePresent != 0 && index < len(semantic)-1 {
		r.resolveStatement(forCtx, semantic[index])
		index++
	}
	if index < len(semantic) {
		r.resolveStatement(forCtx, semantic[len(semantic)-1])
	}
}

func (r *resolver) resolveSwitch(ctx walkContext, node syntax.Node) {
	children := node.Children()
	if len(children) == 0 {
		return
	}
	r.resolveExpression(ctx, children[0])
	for _, id := range children[1:] {
		r.resolveStatement(ctx, id)
	}
}

func (r *resolver) resolveType(ctx walkContext, nodeID syntax.NodeID) {
	node, ok := ctx.module.Tree.Node(nodeID)
	if !ok {
		return
	}
	switch node.Kind() {
	case syntax.Name:
		r.resolveName(ctx, nodeID, contextType, true)
	case syntax.Path:
		r.resolvePath(ctx, nodeID, node, contextType)
	case syntax.BracketApply:
		r.resolveBracket(ctx, nodeID, node, contextType)
	case syntax.ArrayType:
		children := semanticChildren(ctx.module.Tree, node)
		if len(children) > 0 {
			r.resolveExpression(ctx, children[0])
		}
		if len(children) > 1 {
			r.resolveType(ctx, children[len(children)-1])
		}
	case syntax.StructType, syntax.UnionType, syntax.EnumType:
		r.resolveAggregate(ctx, nodeID, node, 0)
	case syntax.FieldDecl, syntax.VariantDecl:
		for _, id := range node.Children() {
			if n, ok := ctx.module.Tree.Node(id); ok && n.Kind() != syntax.Name {
				r.resolveType(ctx, id)
			}
		}
	case syntax.FunctionTerm, syntax.OptionalType, syntax.SliceType, syntax.PrefixTerm, syntax.GroupedTerm, syntax.TupleTerm:
		for _, id := range node.Children() {
			r.resolveType(ctx, id)
		}
	case syntax.Missing, syntax.Error, syntax.Literal:
	default:
		for _, id := range node.Children() {
			r.resolveType(ctx, id)
		}
	}
}

func (r *resolver) resolveExpression(ctx walkContext, nodeID syntax.NodeID) Resolution {
	node, ok := ctx.module.Tree.Node(nodeID)
	if !ok {
		return Resolution{State: ResolutionError}
	}
	switch node.Kind() {
	case syntax.Name:
		return r.resolveName(ctx, nodeID, contextValue, true)
	case syntax.Path:
		return r.resolvePath(ctx, nodeID, node, contextValue)
	case syntax.BracketApply:
		return r.resolveBracket(ctx, nodeID, node, contextValue)
	case syntax.MemberExpr:
		return r.resolveMember(ctx, nodeID, node)
	case syntax.RecordExpr:
		return r.resolveRecord(ctx, nodeID, node)
	case syntax.RecordField:
		children := node.Children()
		if len(children) > 1 {
			return r.resolveExpression(ctx, children[len(children)-1])
		}
	case syntax.CastExpr:
		children := node.Children()
		if len(children) > 0 {
			r.resolveExpression(ctx, children[0])
		}
		if len(children) > 1 {
			r.resolveType(ctx, children[1])
		}
	case syntax.SizeofExpr:
		for _, id := range node.Children() {
			r.resolveType(ctx, id)
		}
	case syntax.FunctionTerm:
		r.resolveAnonymousFunction(ctx, nodeID, node)
	case syntax.Missing, syntax.Error, syntax.Literal, syntax.ContextExpr, syntax.PartialMemberExpr:
		if node.Kind() == syntax.PartialMemberExpr {
			children := node.Children()
			if len(children) != 0 {
				r.deferName(ctx, children[0])
			}
		}
	default:
		for _, id := range node.Children() {
			r.resolveExpression(ctx, id)
		}
	}
	return Resolution{Syntax: SyntaxRef{Module: ctx.module.ID, Node: nodeID}, State: ResolutionDeferred}
}

func (r *resolver) resolveAnonymousFunction(ctx walkContext, nodeID syntax.NodeID, node syntax.Node) {
	ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	fnScope := r.newScope(ScopeFunction, ctx.scope, ctx.module.ID, 0, ref)
	if fnScope == 0 {
		return
	}
	fnCtx := ctx
	fnCtx.scope = fnScope
	fnCtx.function = functionContext{ref: ref, anonymous: true}
	for _, childID := range node.Children() {
		child, ok := ctx.module.Tree.Node(childID)
		if !ok {
			continue
		}
		switch child.Kind() {
		case syntax.TypeParameter:
			id := r.collectNestedName(ctx.module, ctx.file, fnScope, childID, child, SymbolTypeParameter, 0, false)
			if id != 0 {
				r.symbolFunctions[id] = ref
			}
		case syntax.Parameter:
			parts := semanticNodeIDs(ctx.module.Tree, child.Children())
			if len(parts) != 0 {
				parts = parts[:len(parts)-1]
			}
			for _, partID := range parts {
				part, ok := ctx.module.Tree.Node(partID)
				if !ok || part.Kind() != syntax.Name {
					continue
				}
				name := r.nodeText(ctx.file, part)
				id := r.addSymbol(Symbol{Name: name, Kind: SymbolParameter, Span: part.Span(), Module: ctx.module.ID, Scope: fnScope, Declaration: SyntaxRef{Module: ctx.module.ID, Node: childID}, Error: name == ""}, true, 0)
				if id != 0 {
					r.symbolFunctions[id] = ref
				}
			}
		}
	}
	r.resolveFunctionParts(fnCtx, node, true)
}

func (r *resolver) resolveRecord(ctx walkContext, nodeID syntax.NodeID, node syntax.Node) Resolution {
	children := node.Children()
	owner := SymbolID(0)
	start := 0
	if len(children) > 0 {
		first, _ := ctx.module.Tree.Node(children[0])
		if first.Kind() != syntax.RecordField && first.Kind() != syntax.Missing && first.Kind() != syntax.Error {
			base := r.resolveNodeWithContext(ctx, children[0], contextType)
			if base.State == ResolutionResolved {
				owner = base.Symbol
			}
			start = 1
		}
	}
	for _, fieldID := range children[start:] {
		field, ok := ctx.module.Tree.Node(fieldID)
		if !ok || field.Kind() != syntax.RecordField {
			continue
		}
		parts := field.Children()
		if len(parts) == 0 {
			continue
		}
		if owner != 0 {
			if name, ok := ctx.module.Tree.Node(parts[0]); ok {
				r.resolveNamedMember(ctx, parts[0], name, owner)
			}
		}
		if len(parts) > 1 {
			r.resolveExpression(ctx, parts[len(parts)-1])
		}
	}
	return Resolution{Syntax: SyntaxRef{Module: ctx.module.ID, Node: nodeID}, Symbol: owner, State: ResolutionDeferred}
}

func (r *resolver) resolveMember(ctx walkContext, nodeID syntax.NodeID, node syntax.Node) Resolution {
	children := node.Children()
	if len(children) < 2 {
		return Resolution{State: ResolutionError}
	}
	base := r.resolveNodeWithContext(ctx, children[0], contextAny)
	member, ok := ctx.module.Tree.Node(children[1])
	if !ok || member.Kind() != syntax.Name {
		return Resolution{State: ResolutionDeferred}
	}
	if base.State == ResolutionResolved {
		symbol, _ := r.result.Symbols.Symbol(base.Symbol)
		if symbol.Kind == SymbolType || symbol.Kind == SymbolExternType {
			return r.resolveNamedMember(ctx, children[1], member, base.Symbol)
		}
	}
	r.result.references[SyntaxRef{Module: ctx.module.ID, Node: children[1]}] = Resolution{Syntax: SyntaxRef{Module: ctx.module.ID, Node: children[1]}, State: ResolutionDeferred}
	return Resolution{Syntax: SyntaxRef{Module: ctx.module.ID, Node: nodeID}, State: ResolutionDeferred}
}

func (r *resolver) resolveNamedMember(ctx walkContext, nodeID syntax.NodeID, node syntax.Node, owner SymbolID) Resolution {
	ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	name := r.nodeText(ctx.file, node)
	id, ok := r.memberBindings[owner][name]
	if !ok {
		r.report(CodeUndefinedName, fmt.Sprintf("type has no member %q", name), node.Span())
		result := Resolution{Syntax: ref, State: ResolutionError}
		r.result.references[ref] = result
		return result
	}
	result := Resolution{Syntax: ref, Symbol: id, State: ResolutionResolved}
	r.result.references[ref] = result
	return result
}

func (r *resolver) resolveBracket(ctx walkContext, nodeID syntax.NodeID, node syntax.Node, expected nameContext) Resolution {
	children := semanticChildren(ctx.module.Tree, node)
	ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	if len(children) == 0 {
		r.result.brackets[ref] = BracketDeferred
		return Resolution{Syntax: ref, State: ResolutionError}
	}
	base := r.resolveNodeWithContext(ctx, children[0], contextAny)
	mode := r.classifyBracketBase(ctx, children[0], base, expected)
	if base.State == ResolutionResolved {
		symbol, _ := r.result.Symbols.Symbol(base.Symbol)
		if !validCategory(symbol.Kind, expected) {
			r.invalidCategory(symbol, expected, nodeSpan(ctx.module.Tree, children[0]))
			base.State = ResolutionError
		}
	}
	r.result.brackets[ref] = mode
	for _, id := range children[1:] {
		if mode == BracketTypeNames {
			r.resolveType(ctx, id)
		} else if mode == BracketValueNames {
			r.resolveExpression(ctx, id)
		} else {
			r.resolveNeutral(ctx, id)
		}
	}
	return base
}

func (r *resolver) classifyBracketBase(ctx walkContext, baseID syntax.NodeID, base Resolution, expected nameContext) BracketMode {
	node, ok := ctx.module.Tree.Node(baseID)
	if !ok {
		return BracketDeferred
	}
	if node.Kind() == syntax.GroupedTerm {
		children := semanticChildren(ctx.module.Tree, node)
		if len(children) == 1 {
			if enclosed, ok := r.resolutionForBracketClassification(ctx, children[0]); ok {
				base = enclosed
			}
			return r.classifyBracketBase(ctx, children[0], base, expected)
		}
		return BracketValueNames
	}
	if node.Kind() == syntax.MemberExpr || node.Kind() == syntax.PartialMemberExpr {
		return BracketDeferred
	}
	if node.Kind() != syntax.Name && node.Kind() != syntax.Path {
		switch node.Kind() {
		case syntax.Literal, syntax.InterpolatedString, syntax.ContextExpr,
			syntax.SomeExpr, syntax.SizeofExpr, syntax.PrefixTerm,
			syntax.PostfixExpr, syntax.BinaryExpr, syntax.CastExpr,
			syntax.CallExpr, syntax.BracketApply, syntax.SliceExpr,
			syntax.TupleTerm, syntax.ArrayExpr, syntax.ArrayRepeatExpr,
			syntax.RecordExpr:
			return BracketValueNames
		case syntax.FunctionTerm:
			if node.Data()&syntax.FunctionBodyPresent != 0 {
				return BracketValueNames
			}
		}
		return BracketDeferred
	}
	if base.State != ResolutionResolved {
		return BracketDeferred
	}
	symbol, ok := r.result.Symbols.Symbol(base.Symbol)
	if !ok {
		return BracketDeferred
	}
	if symbol.Generic && (isTypeSymbol(symbol.Kind) || isCallableSymbol(symbol.Kind)) {
		return BracketTypeNames
	}
	if isRuntimeSymbol(symbol.Kind) || isCallableSymbol(symbol.Kind) {
		return BracketValueNames
	}
	if expected == contextType && isTypeSymbol(symbol.Kind) {
		return BracketTypeNames
	}
	return BracketDeferred
}

func (r *resolver) resolutionForBracketClassification(ctx walkContext, nodeID syntax.NodeID) (Resolution, bool) {
	node, ok := ctx.module.Tree.Node(nodeID)
	if !ok {
		return Resolution{}, false
	}
	if node.Kind() == syntax.GroupedTerm {
		children := semanticChildren(ctx.module.Tree, node)
		if len(children) == 1 {
			return r.resolutionForBracketClassification(ctx, children[0])
		}
		return Resolution{}, false
	}
	ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	if node.Kind() == syntax.Name {
		resolution, ok := r.result.references[ref]
		return resolution, ok
	}
	if node.Kind() == syntax.Path {
		children := semanticChildren(ctx.module.Tree, node)
		if len(children) != 0 {
			memberRef := SyntaxRef{Module: ctx.module.ID, Node: children[len(children)-1]}
			resolution, ok := r.result.references[memberRef]
			return resolution, ok
		}
	}
	return Resolution{}, false
}

func (r *resolver) resolveNodeWithContext(ctx walkContext, nodeID syntax.NodeID, expected nameContext) Resolution {
	n, ok := ctx.module.Tree.Node(nodeID)
	if !ok {
		return Resolution{State: ResolutionError}
	}
	switch n.Kind() {
	case syntax.Name:
		return r.resolveName(ctx, nodeID, expected, true)
	case syntax.Path:
		return r.resolvePath(ctx, nodeID, n, expected)
	case syntax.BracketApply:
		return r.resolveBracket(ctx, nodeID, n, expected)
	default:
		return r.resolveExpression(ctx, nodeID)
	}
}

func (r *resolver) resolvePath(ctx walkContext, nodeID syntax.NodeID, node syntax.Node, expected nameContext) Resolution {
	children := node.Children()
	pathRef := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	if len(children) < 2 {
		return Resolution{Syntax: pathRef, State: ResolutionError}
	}
	baseNode, ok := ctx.module.Tree.Node(children[0])
	if !ok || baseNode.Kind() != syntax.Name {
		r.resolveNeutral(ctx, children[0])
		if memberNode, ok := ctx.module.Tree.Node(children[1]); ok && memberNode.Kind() == syntax.Name {
			r.deferName(ctx, children[1])
		}
		return Resolution{Syntax: pathRef, State: ResolutionDeferred}
	}
	baseRef := SyntaxRef{Module: ctx.module.ID, Node: children[0]}
	name := r.nodeText(ctx.file, baseNode)
	qualifierID := r.lookup(ctx.scope, name)
	if qualifierID == 0 {
		r.report(CodeUndefinedName, fmt.Sprintf("undefined name %q", name), baseNode.Span())
		r.result.references[baseRef] = Resolution{Syntax: baseRef, State: ResolutionError}
		return Resolution{Syntax: pathRef, State: ResolutionError}
	}
	qualifier, _ := r.result.Symbols.Symbol(qualifierID)
	if qualifier.Kind != SymbolModule || qualifier.Error {
		r.report(CodeInvalidQualifier, fmt.Sprintf("%q does not identify an imported module", name), baseNode.Span())
		r.result.references[baseRef] = Resolution{Syntax: baseRef, Symbol: qualifierID, State: ResolutionError}
		return Resolution{Syntax: pathRef, State: ResolutionError}
	}
	r.result.qualifiers[baseRef] = qualifier.ImportTarget
	memberNode, ok := ctx.module.Tree.Node(children[1])
	if !ok || memberNode.Kind() != syntax.Name {
		return Resolution{Syntax: pathRef, State: ResolutionError}
	}
	memberName := r.nodeText(ctx.file, memberNode)
	targetScope := r.moduleScopes[qualifier.ImportTarget]
	memberID := r.bindings[targetScope][memberName]
	memberRef := SyntaxRef{Module: ctx.module.ID, Node: children[1]}
	if memberID == 0 {
		r.report(CodeMissingMember, fmt.Sprintf("imported module %q has no member %q", name, memberName), memberNode.Span())
		result := Resolution{Syntax: memberRef, State: ResolutionError}
		r.result.references[memberRef] = result
		return result
	}
	member, _ := r.result.Symbols.Symbol(memberID)
	state := ResolutionResolved
	if !validCategory(member.Kind, expected) {
		r.invalidCategory(member, expected, memberNode.Span())
		state = ResolutionError
	}
	result := Resolution{Syntax: memberRef, Symbol: memberID, State: state}
	r.result.references[memberRef] = result
	return result
}

func (r *resolver) resolveName(ctx walkContext, nodeID syntax.NodeID, expected nameContext, record bool) Resolution {
	node, ok := ctx.module.Tree.Node(nodeID)
	ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	if !ok {
		return Resolution{Syntax: ref, State: ResolutionError}
	}
	name := r.nodeText(ctx.file, node)
	if name == "" {
		result := Resolution{Syntax: ref, State: ResolutionError}
		if record {
			r.result.references[ref] = result
		}
		return result
	}
	id := r.lookup(ctx.scope, name)
	if id == 0 {
		r.report(CodeUndefinedName, fmt.Sprintf("undefined name %q", name), node.Span())
		result := Resolution{Syntax: ref, State: ResolutionError}
		if record {
			r.result.references[ref] = result
		}
		return result
	}
	symbol, _ := r.result.Symbols.Symbol(id)
	state := ResolutionResolved
	if symbol.Error || !validCategory(symbol.Kind, expected) {
		if !symbol.Error {
			r.invalidCategory(symbol, expected, node.Span())
		}
		state = ResolutionError
	}
	result := Resolution{Syntax: ref, Symbol: id, State: state}
	if record {
		r.result.references[ref] = result
	}
	if state == ResolutionResolved {
		r.capture(ctx, id)
	}
	return result
}

func (r *resolver) lookup(scope ScopeID, name string) SymbolID {
	for scope != 0 {
		if id := r.bindings[scope][name]; id != 0 {
			return id
		}
		value, ok := r.result.Scopes.Scope(scope)
		if !ok {
			break
		}
		scope = value.Parent
	}
	return 0
}

func validCategory(kind SymbolKind, expected nameContext) bool {
	if expected == contextAny {
		return true
	}
	if expected == contextType {
		return isTypeSymbol(kind)
	}
	return isRuntimeSymbol(kind) || isCallableSymbol(kind)
}
func isTypeSymbol(kind SymbolKind) bool {
	return kind == SymbolType || kind == SymbolExternType || kind == SymbolTypeParameter || kind == SymbolBuiltinType || kind == SymbolRuntimeType
}
func isCallableSymbol(kind SymbolKind) bool {
	return kind == SymbolFunction || kind == SymbolExternFunction || kind == SymbolMethod
}
func isRuntimeSymbol(kind SymbolKind) bool {
	return kind == SymbolBinding || kind == SymbolExternBinding || kind == SymbolParameter || kind == SymbolLoopBinding || kind == SymbolField || kind == SymbolVariant
}
func (r *resolver) invalidCategory(symbol Symbol, expected nameContext, span source.Span) {
	want := "value"
	if expected == contextType {
		want = "type"
	}
	r.report(CodeInvalidCategory, fmt.Sprintf("%q resolves to a %s, not a %s", symbol.Name, symbol.Kind, want), span)
}

func (r *resolver) capture(ctx walkContext, id SymbolID) {
	if !ctx.function.anonymous {
		return
	}
	owner := r.symbolFunctions[id]
	if owner.Node == 0 || owner == ctx.function.ref {
		return
	}
	symbol, _ := r.result.Symbols.Symbol(id)
	if symbol.Kind != SymbolParameter && symbol.Kind != SymbolBinding && symbol.Kind != SymbolLoopBinding {
		return
	}
	scope, _ := r.result.Scopes.Scope(symbol.Scope)
	if scope.Kind == ScopeModule {
		return
	}
	list := r.result.captures[ctx.function.ref]
	for _, existing := range list {
		if existing == id {
			return
		}
	}
	if len(list) == 0 {
		r.result.captureOrder = append(r.result.captureOrder, ctx.function.ref)
	}
	r.result.captures[ctx.function.ref] = append(list, id)
}

func (r *resolver) resolveNeutral(ctx walkContext, nodeID syntax.NodeID) Resolution {
	node, ok := ctx.module.Tree.Node(nodeID)
	if !ok {
		return Resolution{State: ResolutionError}
	}
	switch node.Kind() {
	case syntax.Name:
		return r.resolveName(ctx, nodeID, contextAny, true)
	case syntax.Path:
		return r.resolvePath(ctx, nodeID, node, contextAny)
	case syntax.BracketApply:
		return r.resolveBracket(ctx, nodeID, node, contextAny)
	case syntax.MemberExpr:
		return r.resolveMember(ctx, nodeID, node)
	case syntax.RecordExpr:
		return r.resolveRecord(ctx, nodeID, node)
	case syntax.RecordField:
		children := semanticChildren(ctx.module.Tree, node)
		if len(children) > 1 {
			return r.resolveNeutral(ctx, children[len(children)-1])
		}
	case syntax.CastExpr:
		children := semanticChildren(ctx.module.Tree, node)
		if len(children) > 0 {
			r.resolveNeutral(ctx, children[0])
		}
		if len(children) > 1 {
			r.resolveType(ctx, children[1])
		}
	case syntax.SizeofExpr:
		for _, id := range node.Children() {
			r.resolveType(ctx, id)
		}
	case syntax.FunctionTerm:
		if node.Data()&syntax.FunctionBodyPresent != 0 {
			r.resolveAnonymousFunction(ctx, nodeID, node)
		} else {
			r.resolveType(ctx, nodeID)
		}
	case syntax.StructType, syntax.UnionType, syntax.EnumType:
		r.resolveAggregate(ctx, nodeID, node, 0)
	case syntax.FieldDecl, syntax.VariantDecl:
		children := semanticChildren(ctx.module.Tree, node)
		if len(children) != 0 {
			r.resolveType(ctx, children[len(children)-1])
		}
	case syntax.PartialMemberExpr:
		children := node.Children()
		if len(children) != 0 {
			r.deferName(ctx, children[0])
		}
	case syntax.Missing, syntax.Error, syntax.Literal, syntax.ContextExpr:
	default:
		for _, id := range node.Children() {
			r.resolveNeutral(ctx, id)
		}
	}
	return Resolution{Syntax: SyntaxRef{Module: ctx.module.ID, Node: nodeID}, State: ResolutionDeferred}
}

func (r *resolver) deferName(ctx walkContext, nodeID syntax.NodeID) {
	ref := SyntaxRef{Module: ctx.module.ID, Node: nodeID}
	if _, exists := r.result.references[ref]; !exists {
		r.result.references[ref] = Resolution{Syntax: ref, State: ResolutionDeferred}
	}
}
func semanticChildren(tree *syntax.Tree, node syntax.Node) []syntax.NodeID {
	var out []syntax.NodeID
	for _, id := range node.Children() {
		if n, ok := tree.Node(id); ok && n.Kind() != syntax.Missing && n.Kind() != syntax.Error {
			out = append(out, id)
		}
	}
	return out
}
func nodeSpan(tree *syntax.Tree, id syntax.NodeID) source.Span {
	if n, ok := tree.Node(id); ok {
		return n.Span()
	}
	return source.Span{}
}
