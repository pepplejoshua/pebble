package infer

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func (p *Program) resolveTemplate(ref symbol.SyntaxRef, owner symbol.SymbolID, nestedAggregate bool, depth uint32) TemplateID {
	if depth >= p.config.MaxTypeSyntaxDepth {
		p.reporter.error(CodeResourceLimit, "type-syntax depth limit exceeded", p.origin(ref, owner))
		return 0
	}
	node, tree, ok := p.node(ref)
	if !ok {
		p.reporter.error(CodeDamagedInput, "type syntax is missing from its immutable tree", p.origin(ref, owner))
		return 0
	}
	children := semanticNodeIDs(tree, node.Children())
	switch node.Kind() {
	case syntax.Name, syntax.Path:
		selected, ok := p.resolvedSymbol(ref, node, tree)
		if !ok {
			return 0
		}
		return p.templateForSymbol(selected, owner, depth+1)
	case syntax.BracketApply:
		if len(children) < 2 {
			p.reporter.error(CodeInvalidType, "generic application requires type arguments", p.origin(ref, owner))
			return 0
		}
		baseNode, _ := tree.Node(children[0])
		baseRef := symbol.SyntaxRef{Module: ref.Module, Node: children[0]}
		selected, ok := p.resolvedSymbol(baseRef, baseNode, tree)
		if !ok {
			return 0
		}
		args := make([]TemplateID, 0, len(children)-1)
		for _, id := range children[1:] {
			arg := p.resolveTemplate(symbol.SyntaxRef{Module: ref.Module, Node: id}, owner, true, depth+1)
			if arg == 0 {
				return 0
			}
			args = append(args, arg)
		}
		return p.applyTypeConstructor(selected, args, owner, ref, depth+1)
	case syntax.PrefixTerm:
		if node.Token() != syntax.Star || len(children) != 1 {
			return p.invalidType(ref, owner, "invalid pointer type")
		}
		child := p.resolveTemplate(symbol.SyntaxRef{Module: ref.Module, Node: children[0]}, owner, true, depth+1)
		if child == 0 {
			return 0
		}
		return p.compositeTemplate(TypeTemplate{Kind: TemplatePointer, Children: []TemplateID{child}})
	case syntax.OptionalType:
		if len(children) != 1 {
			return p.invalidType(ref, owner, "invalid optional type")
		}
		child := p.resolveTemplate(symbol.SyntaxRef{Module: ref.Module, Node: children[0]}, owner, true, depth+1)
		if child == 0 {
			return 0
		}
		return p.compositeTemplate(TypeTemplate{Kind: TemplateOptional, Children: []TemplateID{child}})
	case syntax.SliceType:
		if len(children) != 1 {
			return p.invalidType(ref, owner, "invalid slice type")
		}
		child := p.resolveTemplate(symbol.SyntaxRef{Module: ref.Module, Node: children[0]}, owner, true, depth+1)
		if child == 0 {
			return 0
		}
		return p.compositeTemplate(TypeTemplate{Kind: TemplateSlice, Children: []TemplateID{child}})
	case syntax.ArrayType:
		if len(children) < 2 {
			return p.invalidType(ref, owner, "invalid array type")
		}
		lengthRef := symbol.SyntaxRef{Module: ref.Module, Node: children[0]}
		if p.inputs.ArrayLengths == nil {
			p.reporter.error(CodeResourceLimit, "array length evaluator is unavailable", p.origin(lengthRef, owner))
			return 0
		}
		length := p.inputs.ArrayLengths.ArrayLength(lengthRef)
		if length.State == ArrayLengthError {
			return 0
		}
		if length.State != ArrayLengthKnown {
			p.reporter.error(CodeResourceLimit, "array length is unavailable", p.origin(lengthRef, owner))
			return 0
		}
		child := p.resolveTemplate(symbol.SyntaxRef{Module: ref.Module, Node: children[len(children)-1]}, owner, true, depth+1)
		if child == 0 {
			return 0
		}
		return p.compositeTemplate(TypeTemplate{Kind: TemplateArray, Length: length.Value, Children: []TemplateID{child}})
	case syntax.GroupedTerm:
		if len(children) != 1 {
			return p.invalidType(ref, owner, "invalid grouped type")
		}
		return p.resolveTemplate(symbol.SyntaxRef{Module: ref.Module, Node: children[0]}, owner, nestedAggregate, depth+1)
	case syntax.TupleTerm:
		if len(children) == 0 {
			p.reporter.error(CodeEmptyTuple, "empty tuple type is invalid; use void", p.origin(ref, owner))
			return 0
		}
		parts := make([]TemplateID, 0, len(children))
		for _, id := range children {
			part := p.resolveTemplate(symbol.SyntaxRef{Module: ref.Module, Node: id}, owner, true, depth+1)
			if part == 0 {
				return 0
			}
			parts = append(parts, part)
		}
		return p.compositeTemplate(TypeTemplate{Kind: TemplateTuple, Children: parts})
	case syntax.FunctionTerm:
		var convention = types.Pebble
		if c, present, valid := p.convention(tree, node); present {
			if !valid {
				p.reporter.error(CodeInvalidType, "unknown calling convention", p.origin(ref, owner))
				return 0
			}
			convention = c
		}
		var parts []TemplateID
		for _, id := range children {
			n, _ := tree.Node(id)
			if n.Kind() == syntax.Literal {
				continue
			}
			part := p.resolveTemplate(symbol.SyntaxRef{Module: ref.Module, Node: id}, owner, true, depth+1)
			if part == 0 {
				return 0
			}
			parts = append(parts, part)
		}
		if len(parts) == 0 {
			return p.invalidType(ref, owner, "function type requires a result")
		}
		return p.compositeTemplate(TypeTemplate{Kind: TemplateFunction, Convention: convention, Children: parts})
	case syntax.StructType, syntax.UnionType, syntax.EnumType:
		// Direct named aggregate bodies are predeclared by prepareDeclarations
		// and never enter occurrence resolution. Any aggregate that reaches this
		// path is therefore anonymous, including a parameter or nested field.
		_ = nestedAggregate
		p.reporter.error(CodeAnonymousAggregate, "anonymous aggregate types are not permitted", p.origin(ref, owner))
		return 0
	case syntax.Missing, syntax.Error:
		p.reporter.error(CodeDamagedInput, "damaged syntax prevented type resolution", p.origin(ref, owner))
		return 0
	default:
		return p.invalidType(ref, owner, "syntax does not denote a type")
	}
}

func (p *Program) resolvedSymbol(ref symbol.SyntaxRef, node syntax.Node, tree *syntax.Tree) (symbol.SymbolID, bool) {
	lookup := ref
	if node.Kind() == syntax.Path {
		children := semanticNodeIDs(tree, node.Children())
		if len(children) == 0 {
			return 0, false
		}
		lookup.Node = children[len(children)-1]
	}
	resolution, ok := p.inputs.Resolution.Reference(lookup)
	if !ok || resolution.State != symbol.ResolutionResolved || resolution.Symbol == 0 {
		p.reporter.error(CodeDamagedInput, "type reference was not resolved by 04b", p.origin(lookup, 0))
		return 0, false
	}
	return resolution.Symbol, true
}

func (p *Program) templateForSymbol(id, owner symbol.SymbolID, depth uint32) TemplateID {
	sym, ok := p.inputs.Resolution.Symbols.Symbol(id)
	if !ok || sym.Error {
		return 0
	}
	switch sym.Kind {
	case symbol.SymbolBuiltinType:
		typeID, ok := p.builtinType(sym.Builtin)
		if !ok {
			return 0
		}
		return p.knownTemplate(typeID)
	case symbol.SymbolRuntimeType:
		switch sym.Runtime {
		case symbol.RuntimeContext:
			p.reporter.error(CodeInvalidType, "compiler-owned context type is not source-spellable", Origin{Span: sym.Span, Symbol: id})
			return 0
		case symbol.RuntimeAllocator:
			if !p.runtimeReady {
				return 0
			}
			return p.knownTemplate(p.runtimeTypes.Allocator)
		default:
			p.reporter.error(CodeInvalidType, "unknown compiler-owned runtime type", Origin{Span: sym.Span, Symbol: id})
			return 0
		}
	case symbol.SymbolTypeParameter:
		if !containsSymbol(p.ownerParameters(owner), id) {
			p.reporter.error(CodeInvalidType, "type parameter is outside the selected owner environment", Origin{Span: sym.Span, Symbol: id})
			return 0
		}
		return p.addTemplate(TypeTemplate{Kind: TemplateParameter, Parameter: id})
	case symbol.SymbolExternType:
		decl := p.declarations[id]
		if decl.Template == 0 {
			return 0
		}
		return decl.Template
	case symbol.SymbolType:
		decl, ok := p.declarations[id]
		if !ok {
			return 0
		}
		if decl.Form == DeclarationAlias && decl.Template == 0 {
			return p.resolveAliasDeclaration(id, depth)
		}
		if len(decl.Parameters) != 0 {
			p.reporter.error(CodeInvalidType, "generic type requires explicit arguments", Origin{Span: sym.Span, Symbol: id})
			return 0
		}
		return decl.Template
	default:
		p.reporter.error(CodeInvalidType, "resolved symbol is not a type", Origin{Span: sym.Span, Symbol: id})
		return 0
	}
}

func (p *Program) applyTypeConstructor(id symbol.SymbolID, args []TemplateID, owner symbol.SymbolID, ref symbol.SyntaxRef, depth uint32) TemplateID {
	decl, ok := p.declarations[id]
	if !ok {
		p.reporter.error(CodeInvalidType, "generic base is not a type declaration", p.origin(ref, owner))
		return 0
	}
	if decl.Form == DeclarationAlias && decl.Template == 0 {
		p.resolveAliasDeclaration(id, depth)
		decl = p.declarations[id]
	}
	if len(args) != len(decl.Parameters) {
		p.reporter.error(CodeInvalidType, fmt.Sprintf("generic type expects %d arguments, got %d", len(decl.Parameters), len(args)), p.origin(ref, owner))
		return 0
	}
	if decl.State != DeclarationReady || decl.Template == 0 {
		return 0
	}
	mapping := make(map[symbol.SymbolID]TemplateID, len(args))
	for i, param := range decl.Parameters {
		mapping[param] = args[i]
	}
	return p.substituteTemplate(decl.Template, mapping, depth+1)
}

func (p *Program) substituteTemplate(id TemplateID, mapping map[symbol.SymbolID]TemplateID, depth uint32) TemplateID {
	if depth >= p.config.MaxTypeSyntaxDepth {
		return 0
	}
	value, ok := p.Template(id)
	if !ok {
		return 0
	}
	if value.Kind == TemplateParameter {
		if replacement, ok := mapping[value.Parameter]; ok {
			return replacement
		}
		return id
	}
	if len(value.Children) == 0 {
		return id
	}
	children := make([]TemplateID, len(value.Children))
	changed := false
	for i, child := range value.Children {
		children[i] = p.substituteTemplate(child, mapping, depth+1)
		if children[i] == 0 {
			return 0
		}
		changed = changed || children[i] != child
	}
	if !changed {
		return id
	}
	value.ID = 0
	value.Children = children
	return p.compositeTemplate(value)
}

func (p *Program) compositeTemplate(value TypeTemplate) TemplateID {
	temporary := p.addTemplate(value)
	if p.deferMaterialization {
		return temporary
	}
	if id, ok := p.materializeTemplate(temporary, nil, false); ok {
		p.templates = p.templates[:len(p.templates)-1]
		return p.knownTemplate(id)
	}
	return temporary
}

func (p *Program) materializeTemplate(id TemplateID, mapping map[symbol.SymbolID]types.TypeID, report bool) (types.TypeID, bool) {
	value, ok := p.Template(id)
	if !ok {
		return 0, false
	}
	switch value.Kind {
	case TemplateKnown:
		return value.Known, true
	case TemplateParameter:
		if mapping != nil {
			if found, ok := mapping[value.Parameter]; ok {
				return found, true
			}
		}
		return 0, false
	}
	children := make([]types.TypeID, len(value.Children))
	for i, child := range value.Children {
		resolved, ok := p.materializeTemplate(child, mapping, report)
		if !ok {
			return 0, false
		}
		children[i] = resolved
	}
	var key types.TypeKey
	switch value.Kind {
	case TemplatePointer:
		key = types.PointerKey(children[0])
	case TemplateArray:
		key = types.ArrayKey(value.Length, children[0])
	case TemplateSlice:
		key = types.SliceKey(children[0])
	case TemplateTuple:
		key = types.TupleKey(children)
	case TemplateOptional:
		key = types.OptionalKey(children[0])
	case TemplateFunction:
		if len(children) == 0 {
			return 0, false
		}
		key = types.FunctionKey(value.Convention, children[:len(children)-1], children[len(children)-1], value.Variadic)
	case TemplateNominal:
		key = types.NominalKey(value.Declaration, children)
	default:
		return 0, false
	}
	result, err := p.internType(key)
	if err != nil {
		if report {
			p.reporter.error(CodeResourceLimit, fmt.Sprintf("cannot intern resolved type: %v", err), Origin{})
		}
		return 0, false
	}
	return result, true
}

func (p *Program) builtinType(kind symbol.BuiltinType) (types.TypeID, bool) {
	b := p.builtins()
	switch kind {
	case symbol.BuiltinBool:
		return b.Bool, true
	case symbol.BuiltinChar:
		return b.Char, true
	case symbol.BuiltinStr:
		return b.Str, true
	case symbol.BuiltinVoid:
		return b.Void, true
	case symbol.BuiltinInt:
		return b.Int, true
	case symbol.BuiltinUint:
		return b.Uint, true
	case symbol.BuiltinI8:
		return b.I8, true
	case symbol.BuiltinI16:
		return b.I16, true
	case symbol.BuiltinI32:
		return b.I32, true
	case symbol.BuiltinI64:
		return b.I64, true
	case symbol.BuiltinU8:
		return b.U8, true
	case symbol.BuiltinU16:
		return b.U16, true
	case symbol.BuiltinU32:
		return b.U32, true
	case symbol.BuiltinU64:
		return b.U64, true
	case symbol.BuiltinF32:
		return b.F32, true
	case symbol.BuiltinF64:
		return b.F64, true
	default:
		return 0, false
	}
}

func (p *Program) ownerParameters(owner symbol.SymbolID) []symbol.SymbolID {
	if owner == 0 {
		return nil
	}
	return p.owners[owner]
}
func containsSymbol(values []symbol.SymbolID, id symbol.SymbolID) bool {
	for _, v := range values {
		if v == id {
			return true
		}
	}
	return false
}
func (p *Program) invalidType(ref symbol.SyntaxRef, owner symbol.SymbolID, message string) TemplateID {
	p.reporter.error(CodeInvalidType, message, p.origin(ref, owner))
	return 0
}
func (p *Program) origin(ref symbol.SyntaxRef, owner symbol.SymbolID) Origin {
	origin := Origin{Syntax: ref, Symbol: owner}
	if n, _, ok := p.node(ref); ok {
		origin.Span = n.Span()
	}
	return origin
}

func (p *Program) resolveConcreteOccurrence(ref symbol.SyntaxRef, owner symbol.SymbolID, r *reporter) TypeResult {
	if ref.Module == 0 || ref.Node == 0 {
		return TypeResult{State: TypeError}
	}
	if owner != 0 {
		if _, ok := p.inputs.Resolution.Symbols.Symbol(owner); !ok {
			r.error(CodeResourceLimit, "type resolution owner is foreign", p.origin(ref, owner))
			return TypeResult{State: TypeError}
		}
	}
	// Occurrence resolution may build short-lived template descriptors while
	// reducing composite syntax. Work on a private scratch copy so the prepared
	// Program remains immutable and can be shared by independent sessions.
	scratch := p.resolutionScratch(r)
	template := scratch.resolveTemplate(ref, owner, false, 0)
	if template == 0 {
		return TypeResult{State: TypeError}
	}
	mapping := make(map[symbol.SymbolID]types.TypeID)
	for _, parameter := range p.ownerParameters(owner) {
		if id, ok := p.typeParams[parameter]; ok {
			mapping[parameter] = id
		}
	}
	id, ok := scratch.materializeTemplate(template, mapping, false)
	if !ok {
		r.error(CodeInvalidType, "type occurrence did not resolve to a concrete or rigid semantic type", p.origin(ref, owner))
		return TypeResult{State: TypeError}
	}
	return TypeResult{State: TypeFinal, Type: id}
}

func (p *Program) resolutionScratch(r *reporter) *Program {
	scratch := *p
	scratch.reporter = r
	scratch.templates = make([]TypeTemplate, len(p.templates))
	for i, value := range p.templates {
		scratch.templates[i] = value
		scratch.templates[i].Children = append([]TemplateID(nil), value.Children...)
	}
	scratch.declarations = make(map[symbol.SymbolID]TypeDeclaration, len(p.declarations))
	for id, value := range p.declarations {
		scratch.declarations[id] = cloneDeclaration(value)
	}
	scratch.aliasState = make(map[symbol.SymbolID]uint8, len(p.aliasState))
	for id, state := range p.aliasState {
		scratch.aliasState[id] = state
	}
	scratch.aliasStack = append([]symbol.SymbolID(nil), p.aliasStack...)
	return &scratch
}
