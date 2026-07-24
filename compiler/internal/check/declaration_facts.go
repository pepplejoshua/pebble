package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type bindingKind uint8

const (
	bindingLocalLet bindingKind = iota + 1
	bindingLocalVar
	bindingGlobalLet
	bindingGlobalVar
	bindingExternLet
	bindingExternVar
	bindingParameter
	bindingRangeIterator
)

type callableKind uint8

const (
	callableNamed callableKind = iota + 1
	callableMethod
	callableExtern
	callableLiteral
)

type bindingRecord struct {
	Header                                recordHeader
	Symbol                                symbol.SymbolID
	Kind                                  bindingKind
	Annotation, Initializer               valueID
	AnnotationPresent, InitializerPresent bool
	Global, Mutable                       bool
}

type callableRecord struct {
	Header                 recordHeader
	Kind                   callableKind
	Symbol                 symbol.SymbolID
	Expression             valueID
	Convention             types.CallingConvention
	Parameters             []valueID
	Result                 valueID
	Variadic, BodyPresent  bool
	ExpressionBody, Inline bool
	Captures               []symbol.SymbolID
}

type unsupportedCallableRecord struct {
	Header         recordHeader
	TypeParameters []symbol.SyntaxRef
}

func (w *walker) declarationSymbols(ref symbol.SyntaxRef) []symbol.Symbol {
	return append([]symbol.Symbol(nil), w.symbolsAt[ref]...)
}

func (w *walker) callableSymbol(ref symbol.SyntaxRef) (symbol.Symbol, bool) {
	for _, value := range w.symbolsAt[ref] {
		switch value.Kind {
		case symbol.SymbolFunction, symbol.SymbolMethod, symbol.SymbolExternFunction:
			return value, true
		}
	}
	return symbol.Symbol{}, false
}

func (w *walker) origin(ref symbol.SyntaxRef, node syntax.Node, role string, owner, genericOwner symbol.SymbolID) infer.Origin {
	return infer.Origin{Syntax: ref, Span: node.Span(), Role: role, Symbol: owner, GenericOwner: genericOwner}
}

func (w *walker) originForRef(ref symbol.SyntaxRef, role string, owner, genericOwner symbol.SymbolID) infer.Origin {
	return infer.Origin{Syntax: ref, Span: spanForRef(w.generation.inputs, ref), Role: role, Symbol: owner, GenericOwner: genericOwner}
}

func (w *walker) header(ref symbol.SyntaxRef, genericOwner symbol.SymbolID, suppressed bool) recordHeader {
	return recordHeader{Syntax: ref, Span: spanForRef(w.generation.inputs, ref), Owner: genericOwner, Suppressed: suppressed}
}

func (w *walker) newValue(term infer.Term, origin infer.Origin) typedValue {
	id, ok := w.generation.addValue(generatedValue{Term: term, Origin: origin})
	if !ok {
		return typedValue{Term: w.session.Error(origin)}
	}
	return typedValue{ID: id, Term: term}
}

func (w *walker) rigidTerms(parameters []symbol.SymbolID, origin infer.Origin) []infer.Substitution {
	out := make([]infer.Substitution, 0, len(parameters))
	for _, parameter := range parameters {
		id, ok := w.program.TypeParameter(parameter)
		if !ok {
			w.generation.report("prepared rigid type parameter is unavailable", origin.Span)
			continue
		}
		out = append(out, infer.Substitution{Parameter: parameter, Argument: w.session.Known(id)})
	}
	return out
}

func (w *walker) termForTemplate(template infer.TemplateID, parameters []symbol.SymbolID, origin infer.Origin) infer.Term {
	value, ok := w.program.Template(template)
	if !ok {
		return w.session.Error(origin)
	}
	switch value.Kind {
	case infer.TemplateKnown:
		return w.session.Known(value.Known)
	case infer.TemplateParameter:
		id, ok := w.program.TypeParameter(value.Parameter)
		if !ok {
			w.generation.report("prepared rigid type parameter is unavailable", origin.Span)
			return w.session.Error(origin)
		}
		return w.session.Known(id)
	default:
		term := w.session.Variable(origin)
		w.session.Add(infer.Instantiate(template, w.rigidTerms(parameters, origin), term, origin))
		return term
	}
}

func (w *walker) retainBinding(value bindingRecord) {
	w.generation.addRecord(retainedRecord{Header: value.Header, Binding: &value})
}

func (w *walker) retainCallable(value callableRecord) {
	w.generation.addRecord(retainedRecord{Header: value.Header, Callable: &value})
}

func (w *walker) callableErrorResult(ref symbol.SyntaxRef, owner, genericOwner symbol.SymbolID, role string) typedValue {
	origin := w.originForRef(ref, role, owner, genericOwner)
	value, _ := w.newSlotValue(w.session.Error(origin), origin)
	return value
}

func (w *walker) handleTypeDeclaration(ref symbol.SyntaxRef, node syntax.Node) {
	item, _ := w.generation.inputs.Graph.Module(ref.Module)
	for _, value := range w.declarationSymbols(ref) {
		if value.Kind != symbol.SymbolType && value.Kind != symbol.SymbolExternType {
			continue
		}
		descriptor, ok := w.program.TypeDeclaration(value.ID)
		genericOwner := symbol.SymbolID(0)
		if ok && len(descriptor.Parameters) != 0 {
			genericOwner = value.ID
		}
		origin := w.origin(ref, node, "type declaration", value.ID, genericOwner)
		if !ok || descriptor.State != infer.DeclarationReady {
			w.publishSymbol(value.ID, w.session.Error(origin), origin)
			continue
		}
		if descriptor.Concrete != 0 {
			w.publishSymbol(value.ID, w.session.Known(descriptor.Concrete), origin)
		}
		if body := declarationTypeBody(item.Tree, node); node.Kind() == syntax.TypeDecl && body != 0 && descriptor.Template != 0 {
			bodyRef := symbol.SyntaxRef{Module: ref.Module, Node: body}
			term := w.termForTemplate(descriptor.Template, descriptor.Parameters, w.originForRef(bodyRef, "type declaration body", value.ID, genericOwner))
			w.preparedType(bodyRef, term, value.ID, genericOwner, "type declaration body", false)
		}
		for _, member := range descriptor.Members {
			memberSymbol, memberOK := w.generation.inputs.Resolution.Symbols.Symbol(member.Symbol)
			if !memberOK {
				continue
			}
			term := w.termForTemplate(member.Type, descriptor.Parameters, w.origin(ref, node, "member type", value.ID, genericOwner))
			if descriptor.Nominal == infer.NominalEnum {
				term = w.termForTemplate(descriptor.Template, descriptor.Parameters, w.origin(ref, node, "enum variant", value.ID, genericOwner))
			}
			memberNode, _ := item.Tree.Node(memberSymbol.Declaration.Node)
			if typeNode := declarationMemberType(item.Tree, memberSymbol.Declaration.Node); (memberNode.Kind() == syntax.FieldDecl || memberNode.Kind() == syntax.VariantDecl) && typeNode != 0 {
				typeRef := symbol.SyntaxRef{Module: ref.Module, Node: typeNode}
				w.preparedType(typeRef, term, value.ID, genericOwner, "member annotation", false)
			}
			w.publishSymbol(member.Symbol, term, w.origin(ref, node, "member", value.ID, genericOwner))
		}
	}
}

func (w *walker) handleBinding(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	var binding symbol.Symbol
	for _, value := range w.declarationSymbols(ref) {
		if value.Kind == symbol.SymbolBinding || value.Kind == symbol.SymbolExternBinding {
			binding = value
			break
		}
	}
	if binding.ID == 0 {
		return
	}
	annotationRef, initializerRef, annotationPresent, initializerPresent := bindingParts(ref, node)
	origin := w.origin(ref, node, "binding", binding.ID, ctx.genericOwner)
	symbolTerm := w.symbolTerm(binding.ID, origin)
	annotation := typedValue{}
	if annotationPresent {
		annotation = w.resolveType(annotationRef, ctx.typeOwner, ctx.genericOwner, "binding annotation")
		w.session.Add(infer.Equal(symbolTerm, annotation.Term, origin))
	}
	initializer := typedValue{}
	if initializerPresent {
		initializerOrigin := w.originForRef(initializerRef, "binding initializer", binding.ID, ctx.genericOwner)
		initializer = w.newValue(w.session.Variable(initializerOrigin), initializerOrigin)
		w.valuesBySyntax[initializerRef] = initializer
		if annotationPresent {
			w.expectations[initializerRef] = w.expectationFor(initializerRef, annotation.ID, compatibilityAssignment)
			w.retainCompatibility(ref, ctx.genericOwner, initializer.ID, annotation.ID, compatibilityAssignment, 0, binding.ID, binding.Span, false)
		} else {
			w.session.Add(infer.Equal(symbolTerm, initializer.Term, origin))
		}
	}
	if !annotationPresent && !initializerPresent {
		symbolTerm = w.session.Error(origin)
	}
	_, published := w.publishSymbol(binding.ID, symbolTerm, origin)
	header := w.header(ref, ctx.genericOwner, !published || (!annotationPresent && !initializerPresent))
	kind, global, mutable := w.bindingKind(binding, node)
	w.retainBinding(bindingRecord{
		Header: header, Symbol: binding.ID, Kind: kind,
		Annotation: annotation.ID, Initializer: initializer.ID,
		AnnotationPresent: annotationPresent, InitializerPresent: initializerPresent,
		Global: global, Mutable: mutable,
	})
}

func (w *walker) handleNamedCallable(ref symbol.SyntaxRef, node syntax.Node) {
	callable, ok := w.callableSymbol(ref)
	if !ok {
		return
	}
	signature, prepared := w.program.Signature(callable.ID)
	genericOwner := symbol.SymbolID(0)
	if prepared && len(signature.TypeParams) != 0 {
		genericOwner = callable.ID
	}
	header := w.header(ref, genericOwner, !prepared || signature.State != infer.DeclarationReady)
	kind := callableNamed
	if callable.Kind == symbol.SymbolMethod {
		kind = callableMethod
	} else if callable.Kind == symbol.SymbolExternFunction {
		kind = callableExtern
	}
	record := callableRecord{
		Header: header, Kind: kind, Symbol: callable.ID,
		BodyPresent:    node.Kind() != syntax.ExternFunction && node.Data()&syntax.FunctionBodyPresent != 0,
		ExpressionBody: node.Data()&syntax.FunctionExpressionBody != 0,
	}
	for _, childID := range node.Children() {
		child, childOK := w.node(ref.Module, childID)
		if childOK && child.Kind() == syntax.Literal && child.Token() == syntax.KwInline {
			record.Inline = true
		}
	}
	if !prepared || signature.State != infer.DeclarationReady {
		origin := w.origin(ref, node, "damaged callable", callable.ID, genericOwner)
		w.publishSymbol(callable.ID, w.session.Error(origin), origin)
		record.Result = w.callableErrorResult(ref, callable.ID, genericOwner, "damaged callable result").ID
		w.retainCallable(record)
		return
	}
	record.Convention, record.Variadic = signature.Convention, signature.Variadic
	item, _ := w.generation.inputs.Graph.Module(ref.Module)
	_, parameterNodes, resultNode, _ := functionParts(item.Tree, node)
	typeValues := make(map[syntax.NodeID]typedValue)
	for index, id := range signature.Parameters {
		parameterSymbol, _ := w.generation.inputs.Resolution.Symbols.Symbol(id)
		parameterNode := parameterSymbol.Declaration.Node
		typeValue, exists := typeValues[parameterNode]
		if !exists {
			typeNode := typeNodeForParameter(item.Tree, parameterNode)
			typeRef := symbol.SyntaxRef{Module: ref.Module, Node: typeNode}
			term := w.termForTemplate(signature.Inputs[index], signature.TypeParams, w.originForRef(typeRef, "parameter annotation", callable.ID, genericOwner))
			typeValue = w.preparedType(typeRef, term, callable.ID, genericOwner, "parameter annotation", false)
			typeValues[parameterNode] = typeValue
		}
		parameterOrigin := w.originForRef(parameterSymbol.Declaration, "parameter", id, genericOwner)
		parameterValue, published := w.publishSymbol(id, typeValue.Term, parameterOrigin)
		record.Parameters = append(record.Parameters, parameterValue.ID)
		parameterHeader := w.header(parameterSymbol.Declaration, genericOwner, !published)
		w.retainBinding(bindingRecord{
			Header: parameterHeader, Symbol: id, Kind: bindingParameter,
			Annotation: typeValue.ID, AnnotationPresent: true,
		})
	}
	_ = parameterNodes
	resultRef := symbol.SyntaxRef{Module: ref.Module, Node: resultNode}
	resultTerm := w.termForTemplate(signature.Result, signature.TypeParams, w.originForRef(resultRef, "result annotation", callable.ID, genericOwner))
	result := w.preparedType(resultRef, resultTerm, callable.ID, genericOwner, "result annotation", false)
	record.Result = result.ID
	if len(signature.TypeParams) == 0 {
		parameters := make([]infer.Shape, len(record.Parameters))
		for index, id := range record.Parameters {
			parameters[index] = infer.Leaf(w.generation.values[id-1].Term)
		}
		origin := w.origin(ref, node, "callable", callable.ID, 0)
		term := w.session.Variable(origin)
		w.session.Add(infer.ConstrainShape(term, infer.FunctionShape(signature.Convention, parameters, infer.Leaf(result.Term), signature.Variadic), w.origin(ref, node, "callable shape", callable.ID, 0)))
		_, published := w.publishSymbol(callable.ID, term, origin)
		record.Header.Suppressed = record.Header.Suppressed || !published
	}
	w.retainCallable(record)
}

func functionParts(tree *syntax.Tree, node syntax.Node) (typeParameters, parameters []syntax.NodeID, result syntax.NodeID, body syntax.NodeID) {
	children := node.Children()
	semantic := make([]syntax.NodeID, 0, len(children))
	for _, id := range children {
		child, ok := tree.Node(id)
		if ok && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
			semantic = append(semantic, id)
		}
	}
	if node.Data()&syntax.FunctionBodyPresent != 0 && len(semantic) != 0 {
		body = semantic[len(semantic)-1]
		semantic = semantic[:len(semantic)-1]
	}
	seenDeclarationName := node.Kind() == syntax.FunctionTerm
	for _, id := range semantic {
		child, ok := tree.Node(id)
		if !ok {
			continue
		}
		switch child.Kind() {
		case syntax.Literal:
			continue
		case syntax.Name:
			if !seenDeclarationName {
				seenDeclarationName = true
				continue
			}
			if result == 0 {
				result = id
			}
		case syntax.TypeParameter:
			typeParameters = append(typeParameters, id)
		case syntax.Parameter:
			parameters = append(parameters, id)
		default:
			if result == 0 {
				result = id
			}
		}
	}
	return
}

func (w *walker) handleFunctionLiteral(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) (unsupported bool) {
	moduleValue, _ := w.generation.inputs.Graph.Module(ref.Module)
	typeParameters, parameterNodes, resultNode, _ := functionParts(moduleValue.Tree, node)
	if len(typeParameters) != 0 {
		origin := w.origin(ref, node, "unsupported generic anonymous function", ctx.typeOwner, ctx.genericOwner)
		w.publishSyntax(ref, w.session.Error(origin), origin)
		parameterRefs := make([]symbol.SyntaxRef, len(typeParameters))
		for index, id := range typeParameters {
			parameterRefs[index] = symbol.SyntaxRef{Module: ref.Module, Node: id}
		}
		header := w.header(ref, ctx.genericOwner, true)
		record := unsupportedCallableRecord{Header: header, TypeParameters: parameterRefs}
		w.generation.addRecord(retainedRecord{Header: header, UnsupportedCallable: &record})
		return true
	}
	record := callableRecord{
		Header: w.header(ref, ctx.genericOwner, false), Kind: callableLiteral,
		Convention: types.Pebble, BodyPresent: true,
		ExpressionBody: node.Data()&syntax.FunctionExpressionBody != 0,
	}
	for _, parameterNode := range parameterNodes {
		parameter, _ := moduleValue.Tree.Node(parameterNode)
		record.Variadic = record.Variadic || parameter.Data()&syntax.ParameterVariadic != 0
		typeRef := symbol.SyntaxRef{Module: ref.Module, Node: typeNodeForParameter(moduleValue.Tree, parameterNode)}
		typeValue := w.resolveType(typeRef, ctx.typeOwner, ctx.genericOwner, "function literal parameter")
		for _, parameterSymbol := range w.symbolsAt[symbol.SyntaxRef{Module: ref.Module, Node: parameterNode}] {
			if parameterSymbol.Kind != symbol.SymbolParameter {
				continue
			}
			parameterOrigin := w.originForRef(parameterSymbol.Declaration, "function literal parameter", parameterSymbol.ID, ctx.genericOwner)
			parameterValue, published := w.publishSymbol(parameterSymbol.ID, typeValue.Term, parameterOrigin)
			record.Parameters = append(record.Parameters, parameterValue.ID)
			parameterHeader := w.header(parameterSymbol.Declaration, ctx.genericOwner, !published)
			w.retainBinding(bindingRecord{Header: parameterHeader, Symbol: parameterSymbol.ID, Kind: bindingParameter, Annotation: typeValue.ID, AnnotationPresent: true})
		}
	}
	resultRef := symbol.SyntaxRef{Module: ref.Module, Node: resultNode}
	result := w.resolveType(resultRef, ctx.typeOwner, ctx.genericOwner, "function literal result")
	record.Result = result.ID
	for _, childID := range node.Children() {
		child, ok := moduleValue.Tree.Node(childID)
		if !ok || child.Kind() != syntax.Literal {
			continue
		}
		if child.Token() == syntax.KwInline {
			record.Inline = true
		}
		if decoded, ok := child.DecodedLiteral(); ok && decoded.Kind == syntax.DecodedString && (decoded.Text == "C" || decoded.Text == "c") {
			record.Convention = types.C
		}
	}
	captures := w.generation.inputs.Resolution.Captures(ref)
	record.Captures = append([]symbol.SymbolID(nil), captures...)
	origin := w.origin(ref, node, "function literal", ctx.typeOwner, ctx.genericOwner)
	term := w.session.Error(origin)
	if len(captures) == 0 {
		w.projectFunctionExpectation(ref, ctx, record, result)
		parameters := make([]infer.Shape, len(record.Parameters))
		for index, id := range record.Parameters {
			parameters[index] = infer.Leaf(w.generation.values[id-1].Term)
		}
		term = w.session.Variable(origin)
		w.session.Add(infer.ConstrainShape(term, infer.FunctionShape(record.Convention, parameters, infer.Leaf(result.Term), record.Variadic), w.origin(ref, node, "function literal shape", ctx.typeOwner, ctx.genericOwner)))
	}
	expression, published := w.publishSyntax(ref, term, origin)
	record.Expression = expression.ID
	record.Header.Suppressed = len(captures) != 0 || !published
	w.successfulExpressions[ref] = len(captures) == 0 && published
	w.retainCallable(record)
	return false
}

func (w *walker) bindingKind(value symbol.Symbol, node syntax.Node) (bindingKind, bool, bool) {
	mutable := node.Token() == syntax.KwVar
	if value.Kind == symbol.SymbolExternBinding {
		if mutable {
			return bindingExternVar, true, true
		}
		return bindingExternLet, true, false
	}
	global := false
	if scope, ok := w.generation.inputs.Resolution.Scopes.Scope(value.Scope); ok {
		global = scope.Kind == symbol.ScopeModule
	}
	if global {
		if mutable {
			return bindingGlobalVar, true, true
		}
		return bindingGlobalLet, true, false
	}
	if mutable {
		return bindingLocalVar, false, true
	}
	return bindingLocalLet, false, false
}

func bindingParts(ref symbol.SyntaxRef, node syntax.Node) (annotation, initializer symbol.SyntaxRef, annotationPresent, initializerPresent bool) {
	children := node.Children()
	annotationPresent = node.Kind() == syntax.ExternBinding || node.Data()&syntax.BindingTypePresent != 0
	initializerPresent = node.Kind() != syntax.ExternBinding && node.Data()&syntax.BindingInitializerPresent != 0
	index := 1
	if annotationPresent && index < len(children) {
		annotation = symbol.SyntaxRef{Module: ref.Module, Node: children[index]}
		index++
	}
	if initializerPresent && index < len(children) {
		initializer = symbol.SyntaxRef{Module: ref.Module, Node: children[index]}
	}
	return
}

func declarationTypeBody(tree *syntax.Tree, node syntax.Node) syntax.NodeID {
	children := node.Children()
	for index := len(children) - 1; index >= 0; index-- {
		child, ok := tree.Node(children[index])
		if ok && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
			return children[index]
		}
	}
	return 0
}

func declarationMemberType(tree *syntax.Tree, nodeID syntax.NodeID) syntax.NodeID {
	return typeNodeForParameter(tree, nodeID)
}

func (w *walker) node(moduleID module.ModuleID, id syntax.NodeID) (syntax.Node, bool) {
	item, ok := w.generation.inputs.Graph.Module(moduleID)
	if !ok || item.Tree == nil {
		return syntax.Node{}, false
	}
	return item.Tree.Node(id)
}

func spanForRef(inputs Inputs, ref symbol.SyntaxRef) source.Span {
	if inputs.Graph == nil {
		return source.Span{}
	}
	item, ok := inputs.Graph.Module(ref.Module)
	if !ok || item.Tree == nil {
		return source.Span{}
	}
	node, _ := item.Tree.Node(ref.Node)
	return node.Span()
}
