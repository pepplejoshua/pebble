package infer

import (
	"fmt"
	"strconv"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func (p *Program) prepareDeclarations() {
	// Rigid parameters exist before any template is resolved.
	for _, sym := range p.inputs.Resolution.Symbols.All() {
		if sym.Kind != symbol.SymbolTypeParameter || sym.Error {
			continue
		}
		id, err := p.internType(types.TypeParameterKey(sym.ID))
		if err != nil {
			p.valid = false
			p.reporter.error(CodeResourceLimit, fmt.Sprintf("cannot intern rigid type parameter: %v", err), Origin{Span: sym.Span, Symbol: sym.ID})
			continue
		}
		p.typeParams[sym.ID] = id
		if sym.Containing != 0 {
			p.owners[sym.Containing] = append(p.owners[sym.Containing], sym.ID)
		}
	}

	for _, sym := range p.inputs.Resolution.Symbols.All() {
		if sym.Error {
			continue
		}
		switch sym.Kind {
		case symbol.SymbolType:
			node, _, ok := p.node(sym.Declaration)
			if !ok {
				p.setDeclarationError(sym.ID)
				continue
			}
			bodyID, body, ok := declarationBodyNode(p.tree(sym.Module), node)
			if !ok {
				p.setDeclarationError(sym.ID)
				continue
			}
			form := DeclarationAlias
			nominal := NominalKind(0)
			switch body.Kind() {
			case syntax.StructType:
				form, nominal = DeclarationNominal, NominalStruct
			case syntax.UnionType:
				form = DeclarationNominal
				nominal = NominalUnion
				for _, child := range body.Children() {
					if n, ok := p.tree(sym.Module).Node(child); ok && n.Kind() == syntax.Literal && n.Token() == syntax.KwEnum {
						nominal = NominalTaggedUnion
						break
					}
				}
			case syntax.EnumType:
				form, nominal = DeclarationNominal, NominalEnum
			}
			decl := TypeDeclaration{Symbol: sym.ID, State: DeclarationReady, Form: form, Nominal: nominal, Parameters: append([]symbol.SymbolID(nil), p.owners[sym.ID]...)}
			p.declarations[sym.ID] = decl
			_ = bodyID
		case symbol.SymbolExternType:
			decl := TypeDeclaration{Symbol: sym.ID, State: DeclarationReady, Form: DeclarationNominal, Nominal: NominalExtern}
			p.declarations[sym.ID] = decl
		}
	}

	// Predeclare only concrete nominal identities. Generic declarations remain
	// SymbolID-identified constructors until application.
	for _, decl := range p.TypeDeclarations() {
		if decl.Form != DeclarationNominal || decl.State != DeclarationReady || len(decl.Parameters) != 0 || decl.Template != 0 {
			continue
		}
		id, err := p.internType(types.NominalKey(decl.Symbol, nil))
		if err != nil {
			p.declarationFailure(decl.Symbol, CodeResourceLimit, fmt.Sprintf("cannot predeclare nominal type: %v", err))
			continue
		}
		value := p.declarations[decl.Symbol]
		value.Concrete = id
		value.Template = p.addTemplate(TypeTemplate{Kind: TemplateKnown, Known: id})
		p.declarations[decl.Symbol] = value
	}

	// Register generic nominal constructor templates before resolving any
	// alias. Aliases may reference a generic declared in another module whose
	// SymbolID sorts after the alias, so a single pass would leave that
	// generic's constructor template unset when the alias resolves against it.
	for _, decl := range p.TypeDeclarations() {
		if decl.State != DeclarationReady || decl.Form == DeclarationAlias || decl.Template != 0 {
			continue
		}
		children := make([]TemplateID, 0, len(decl.Parameters))
		for _, parameter := range decl.Parameters {
			children = append(children, p.addTemplate(TypeTemplate{Kind: TemplateParameter, Parameter: parameter}))
		}
		value := p.declarations[decl.Symbol]
		value.Template = p.addTemplate(TypeTemplate{Kind: TemplateNominal, Declaration: decl.Symbol, Children: children})
		p.declarations[decl.Symbol] = value
	}

	// Resolve aliases, whose bodies may apply any constructor registered above.
	for _, decl := range p.TypeDeclarations() {
		if decl.State != DeclarationReady || decl.Form != DeclarationAlias {
			continue
		}
		p.resolveAliasDeclaration(decl.Symbol, 0)
	}

	// Member descriptors depend on all constructor templates being registered.
	for _, decl := range p.TypeDeclarations() {
		if decl.State != DeclarationReady || decl.Form != DeclarationNominal {
			continue
		}
		value := p.declarations[decl.Symbol]
		for _, memberID := range p.inputs.Resolution.Members(decl.Symbol) {
			member, ok := p.inputs.Resolution.Symbols.Symbol(memberID)
			if !ok || member.Error {
				continue
			}
			switch member.Kind {
			case symbol.SymbolField, symbol.SymbolVariant:
				template := p.memberTypeTemplate(member, decl.Symbol)
				if template != 0 {
					value.Members = append(value.Members, MemberDescriptor{Symbol: member.ID, Type: template})
				}
			}
		}
		p.declarations[decl.Symbol] = value
	}
}

func (p *Program) prepareSignatures() {
	for _, sym := range p.inputs.Resolution.Symbols.All() {
		if sym.Error {
			continue
		}
		if sym.Kind != symbol.SymbolFunction && sym.Kind != symbol.SymbolMethod && sym.Kind != symbol.SymbolExternFunction {
			continue
		}
		node, tree, ok := p.node(sym.Declaration)
		if !ok {
			p.signatures[sym.ID] = Signature{Symbol: sym.ID, State: DeclarationError}
			continue
		}
		if node.Kind() == syntax.FunctionTerm {
			continue
		}
		params := p.parametersFor(sym.ID)
		typeParams := append([]symbol.SymbolID(nil), p.owners[sym.ID]...)
		if sym.Kind == symbol.SymbolMethod && sym.Containing != 0 {
			inherited := p.owners[sym.Containing]
			typeParams = append(append([]symbol.SymbolID(nil), inherited...), typeParams...)
		}
		p.owners[sym.ID] = append([]symbol.SymbolID(nil), typeParams...)
		signature := Signature{Symbol: sym.ID, State: DeclarationReady, Parameters: params, TypeParams: typeParams, Convention: types.Pebble}
		if sym.Kind == symbol.SymbolExternFunction {
			signature.Convention = types.C
		}
		if convention, present, valid := p.convention(tree, node); present {
			if !valid {
				p.reporter.error(CodeInvalidType, "unknown calling convention", Origin{Span: node.Span(), Symbol: sym.ID})
				signature.State = DeclarationError
			} else {
				signature.Convention = convention
			}
		}
		paramNodes, resultNode := signatureNodes(tree, node)
		if resultNode == 0 {
			signature.State = DeclarationError
		}
		for _, parameter := range params {
			paramSymbol, _ := p.inputs.Resolution.Symbols.Symbol(parameter)
			paramNode := paramTypeNode(tree, paramSymbol.Declaration.Node)
			if paramNode == 0 {
				signature.State = DeclarationError
				signature.Inputs = append(signature.Inputs, 0)
				continue
			}
			template := p.resolveTemplate(symbol.SyntaxRef{Module: sym.Module, Node: paramNode}, sym.ID, false, 0)
			signature.Inputs = append(signature.Inputs, template)
		}
		_ = paramNodes
		if resultNode != 0 {
			signature.Result = p.resolveTemplate(symbol.SyntaxRef{Module: sym.Module, Node: resultNode}, sym.ID, false, 0)
		}
		var parameterNodeIDs []syntax.NodeID
		for _, id := range semanticNodeIDs(tree, node.Children()) {
			if n, _ := tree.Node(id); n.Kind() == syntax.Parameter {
				parameterNodeIDs = append(parameterNodeIDs, id)
			}
		}
		for i, id := range parameterNodeIDs {
			n, _ := tree.Node(id)
			if n.Data()&syntax.ParameterVariadic == 0 {
				continue
			}
			signature.Variadic = true
			if i != len(parameterNodeIDs)-1 {
				p.reporter.error(CodeInvalidType, "a variadic parameter must be the last parameter", Origin{Span: n.Span(), Symbol: sym.ID})
				signature.State = DeclarationError
			}
		}
		if signature.Result == 0 {
			signature.State = DeclarationError
		}
		for _, id := range signature.Inputs {
			if id == 0 {
				signature.State = DeclarationError
			}
		}
		p.signatures[sym.ID] = signature
	}
}

func (p *Program) resolveAliasDeclaration(id symbol.SymbolID, depth uint32) TemplateID {
	decl, ok := p.declarations[id]
	if !ok || decl.Form != DeclarationAlias {
		return 0
	}
	if decl.State == DeclarationError {
		return 0
	}
	if decl.Template != 0 {
		return decl.Template
	}
	if depth >= p.config.MaxAliasDepth {
		p.declarationFailure(id, CodeResourceLimit, "alias depth limit exceeded")
		return 0
	}
	switch p.aliasState[id] {
	case 1:
		p.reportAliasCycle(id)
		return 0
	case 2:
		return p.declarations[id].Template
	}
	p.aliasState[id] = 1
	p.aliasStack = append(p.aliasStack, id)
	sym, _ := p.inputs.Resolution.Symbols.Symbol(id)
	node, tree, ok := p.node(sym.Declaration)
	if !ok {
		p.setDeclarationError(id)
		return 0
	}
	bodyID, _, ok := declarationBodyNode(tree, node)
	if !ok {
		p.setDeclarationError(id)
		return 0
	}
	template := p.resolveTemplate(symbol.SyntaxRef{Module: sym.Module, Node: bodyID}, id, false, depth+1)
	if p.reporter.sessionFatal() {
		return 0
	}
	p.aliasStack = p.aliasStack[:len(p.aliasStack)-1]
	p.aliasState[id] = 2
	value := p.declarations[id]
	if template == 0 {
		value.State = DeclarationError
	} else {
		value.Template = template
		if concrete, ok := p.materializeTemplate(template, nil, false); ok {
			value.Concrete = concrete
			value.Template = p.addTemplate(TypeTemplate{Kind: TemplateKnown, Known: concrete})
		}
	}
	p.declarations[id] = value
	return value.Template
}

func (p *Program) reportAliasCycle(closing symbol.SymbolID) {
	start := 0
	for i, id := range p.aliasStack {
		if id == closing {
			start = i
			break
		}
	}
	cycle := append([]symbol.SymbolID(nil), p.aliasStack[start:]...)
	if len(cycle) == 0 {
		cycle = []symbol.SymbolID{closing}
	}
	first, _ := p.inputs.Resolution.Symbols.Symbol(closing)
	related := make([]Origin, 0, len(cycle))
	for _, id := range cycle {
		sym, _ := p.inputs.Resolution.Symbols.Symbol(id)
		related = append(related, Origin{Span: sym.Span, Symbol: id, Role: "alias in cycle"})
		value := p.declarations[id]
		value.State = DeclarationError
		p.declarations[id] = value
		p.aliasState[id] = 2
	}
	p.reporter.error(CodeAliasCycle, "transparent type alias cycle", Origin{Span: first.Span, Symbol: closing, Role: "cycle closes here"}, related...)
}

func (p *Program) memberTypeTemplate(member symbol.Symbol, owner symbol.SymbolID) TemplateID {
	if member.Kind == symbol.SymbolVariant {
		declNode, tree, ok := p.node(member.Declaration)
		if ok && declNode.Kind() == syntax.Name {
			return p.knownTemplate(p.builtins().Void)
		}
		if ok {
			children := semanticNodeIDs(tree, declNode.Children())
			if len(children) > 0 {
				last, _ := tree.Node(children[len(children)-1])
				if last.Kind() == syntax.Name && len(children) == 1 {
					return p.knownTemplate(p.builtins().Void)
				}
			}
		}
	}
	_, tree, ok := p.node(member.Declaration)
	if !ok {
		return 0
	}
	typeNode := memberTypeNode(tree, member.Declaration.Node)
	if typeNode == 0 {
		return 0
	}
	return p.resolveTemplate(symbol.SyntaxRef{Module: member.Module, Node: typeNode}, owner, true, 0)
}

func (p *Program) parametersFor(owner symbol.SymbolID) []symbol.SymbolID {
	var out []symbol.SymbolID
	for _, sym := range p.inputs.Resolution.Symbols.All() {
		if sym.Kind == symbol.SymbolParameter && sym.Containing == owner && !sym.Error {
			out = append(out, sym.ID)
		}
	}
	return out
}

func (p *Program) convention(tree *syntax.Tree, node syntax.Node) (types.CallingConvention, bool, bool) {
	for _, id := range node.Children() {
		n, ok := tree.Node(id)
		if !ok || n.Kind() != syntax.Literal || n.Token() != syntax.StringLiteral {
			continue
		}
		file, _ := p.inputs.Sources.File(n.Span().Source)
		raw := string(file.Slice(n.Span()))
		value, err := strconv.Unquote(raw)
		if err != nil {
			return 0, true, false
		}
		switch value {
		case "C", "c":
			return types.C, true, true
		case "Pebble", "pebble":
			return types.Pebble, true, true
		default:
			return 0, true, false
		}
	}
	return 0, false, true
}

func (p *Program) node(ref symbol.SyntaxRef) (syntax.Node, *syntax.Tree, bool) {
	m, ok := p.modules[ref.Module]
	if !ok || m.Tree == nil {
		return syntax.Node{}, nil, false
	}
	n, ok := m.Tree.Node(ref.Node)
	return n, m.Tree, ok
}
func (p *Program) tree(id symbol.ModuleID) *syntax.Tree { m := p.modules[id]; return m.Tree }

func declarationBodyNode(tree *syntax.Tree, node syntax.Node) (syntax.NodeID, syntax.Node, bool) {
	children := semanticNodeIDs(tree, node.Children())
	if len(children) < 2 {
		return 0, syntax.Node{}, false
	}
	id := children[len(children)-1]
	n, ok := tree.Node(id)
	return id, n, ok
}
func memberTypeNode(tree *syntax.Tree, nodeID syntax.NodeID) syntax.NodeID {
	node, ok := tree.Node(nodeID)
	if !ok {
		return 0
	}
	children := semanticNodeIDs(tree, node.Children())
	if len(children) < 2 {
		return 0
	}
	return children[len(children)-1]
}
func paramTypeNode(tree *syntax.Tree, nodeID syntax.NodeID) syntax.NodeID {
	return memberTypeNode(tree, nodeID)
}
func signatureNodes(tree *syntax.Tree, node syntax.Node) ([]syntax.NodeID, syntax.NodeID) {
	children := semanticNodeIDs(tree, node.Children())
	var params []syntax.NodeID
	result := syntax.NodeID(0)
	seenName := false
	for _, id := range children {
		n, _ := tree.Node(id)
		switch n.Kind() {
		case syntax.Literal:
			continue
		case syntax.Name:
			if !seenName {
				seenName = true
				continue
			}
			if result == 0 {
				result = id
			}
		case syntax.TypeParameter:
			continue
		case syntax.Parameter:
			params = append(params, id)
		default:
			if result == 0 {
				result = id
			}
		}
	}
	return params, result
}
func semanticNodeIDs(tree *syntax.Tree, ids []syntax.NodeID) []syntax.NodeID {
	out := make([]syntax.NodeID, 0, len(ids))
	for _, id := range ids {
		if n, ok := tree.Node(id); ok && n.Kind() != syntax.Missing && n.Kind() != syntax.Error && n.Kind() != syntax.EndOfFile {
			out = append(out, id)
		}
	}
	return out
}

func (p *Program) knownTemplate(id types.TypeID) TemplateID {
	return p.addTemplate(TypeTemplate{Kind: TemplateKnown, Known: id})
}
func (p *Program) setDeclarationError(id symbol.SymbolID) {
	value := p.declarations[id]
	value.Symbol = id
	value.State = DeclarationError
	p.declarations[id] = value
}
func (p *Program) declarationFailure(id symbol.SymbolID, code diagnostic.Code, message string) {
	sym, _ := p.inputs.Resolution.Symbols.Symbol(id)
	p.reporter.error(code, message, Origin{Span: sym.Span, Symbol: id})
	if p.reporter.sessionFatal() {
		return
	}
	p.setDeclarationError(id)
}
