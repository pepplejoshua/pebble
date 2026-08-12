package check

import (
	"fmt"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type callKind uint8

const (
	callDirect callKind = iota + 1
	callIndirect
	callMethod
	callVariant
)

type callTarget struct {
	Kind            callKind
	Symbol          symbol.SymbolID
	Site            symbol.SyntaxRef
	Convention      types.CallingConvention
	ConventionKnown bool
	FixedCount      uint32
	Variadic        bool
}
type callArgument struct {
	Source, Destination valueID
	Ordinal             uint32
	Variadic            bool
}
type callRecord struct {
	Header                   recordHeader
	Callee, Receiver, Result valueID
	Arguments                []callArgument
	Target                   callTarget
}

func cloneCallRecord(v callRecord) callRecord {
	v.Arguments = append([]callArgument(nil), v.Arguments...)
	return v
}

type callPlan struct {
	callee              symbol.SyntaxRef
	arguments           []symbol.SyntaxRef
	target              callTarget
	calleeValue         typedValue
	destinations        []typedValue
	result              typedValue
	method              symbol.SyntaxRef
	deferredBracketCall bool
	genericDestinations []typedValue
	runtimeDestinations []typedValue
	genericResult       typedValue
	runtimeResult       typedValue
}

func semanticRefs(module symbol.ModuleID, node syntax.Node, tree *syntax.Tree) []symbol.SyntaxRef {
	var out []symbol.SyntaxRef
	for _, id := range node.Children() {
		child, ok := tree.Node(id)
		if ok && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
			out = append(out, symbol.SyntaxRef{Module: module, Node: id})
		}
	}
	return out
}
func (w *walker) reserveExpression(ref symbol.SyntaxRef, origin infer.Origin) typedValue {
	if v := w.valuesBySyntax[ref]; v.ID != 0 {
		return v
	}
	v := w.newValue(w.session.Variable(origin), origin)
	w.valuesBySyntax[ref] = v
	return v
}

func (w *walker) prepareCall(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
	for i := range items {
		items[i].ctx.expected = expectedType{}
	}
	if ctx.suppressValue {
		for i := range items {
			items[i].ctx.suppressValue = true
		}
		return items
	}
	semantic := semanticRefs(ref.Module, node, tree)
	if len(semantic) == 0 {
		return items
	}
	p := &callPlan{callee: semantic[0], arguments: semantic[1:], target: callTarget{Kind: callIndirect}}
	if id, kind, site, ok := w.staticTarget(p.callee, tree); ok {
		p.target.Kind, p.target.Symbol = kind, id
		if site != (symbol.SyntaxRef{}) {
			p.target.Site = site
		}
	} else if callee, ok := tree.Node(p.callee.Node); ok && callee.Kind() == syntax.MemberExpr {
		p.target.Kind, p.target.Site, p.method = callMethod, ref, p.callee
	} else if callee, ok := tree.Node(p.callee.Node); ok && callee.Kind() == syntax.BracketApply {
		if mode, found := w.generation.inputs.Resolution.Bracket(p.callee); found && mode == symbol.BracketDeferred {
			parts := semanticRefs(ref.Module, callee, tree)
			if len(parts) == 2 {
				if base, present := tree.Node(parts[0].Node); present && base.Kind() == syntax.MemberExpr {
					p.target.Site, p.method, p.deferredBracketCall = ref, p.callee, true
				}
			}
		}
	}
	if p.deferredBracketCall {
		common := w.reserveExpression(ref, w.origin(ref, node, "deferred call result", ctx.typeOwner, ctx.genericOwner))
		p.genericResult = w.newValue(common.Term, w.origin(ref, node, "deferred generic call result", ctx.typeOwner, ctx.genericOwner))
		p.runtimeResult = w.newValue(common.Term, w.origin(ref, node, "deferred runtime call result", ctx.typeOwner, ctx.genericOwner))
		p.genericDestinations = w.branchDestinations(p.arguments, ctx, "generic")
		p.runtimeDestinations = w.branchDestinations(p.arguments, ctx, "runtime")
	} else {
		p.result = w.reserveExpression(ref, w.origin(ref, node, "call result", ctx.typeOwner, ctx.genericOwner))
	}
	switch p.target.Kind {
	case callDirect:
		w.prepareDirect(p, ref, ctx)
	case callVariant:
		w.prepareVariant(p, ref, ctx)
	default:
		if p.deferredBracketCall {
			// Both exact interpretations are assembled after the authored call
			// arguments have completed their one common traversal.
		} else {
			p.destinations = w.freshDestinations(p.arguments, ctx)
		}
	}
	for i := range items {
		if items[i].ref == p.callee {
			items[i].ctx.expected = expectedType{}
			if p.target.Kind == callDirect || p.target.Kind == callVariant {
				items[i].ctx.suppressValue = true
			} else if p.target.Kind == callMethod || p.deferredBracketCall {
				items[i].ctx.immediateCall, items[i].ctx.callSite = true, ref
			}
		}
	}
	w.callPlans[ref] = p
	return items
}

func (w *walker) branchDestinations(refs []symbol.SyntaxRef, ctx walkContext, branch string) []typedValue {
	out := make([]typedValue, len(refs))
	for i, ref := range refs {
		origin := w.originForRef(ref, fmt.Sprintf("%s argument %d destination", branch, i+1), ctx.typeOwner, ctx.genericOwner)
		out[i] = w.newValue(w.session.Variable(origin), origin)
	}
	return out
}

func (w *walker) staticTarget(ref symbol.SyntaxRef, tree *syntax.Tree) (symbol.SymbolID, callKind, symbol.SyntaxRef, bool) {
	node, ok := tree.Node(ref.Node)
	if !ok {
		return 0, 0, symbol.SyntaxRef{}, false
	}
	query, site := ref, symbol.SyntaxRef{}
	switch node.Kind() {
	case syntax.Path, syntax.MemberExpr:
		children := node.Children()
		if len(children) == 0 {
			return 0, 0, site, false
		}
		query.Node = children[len(children)-1]
	case syntax.BracketApply:
		mode, found := w.generation.inputs.Resolution.Bracket(ref)
		if !found || mode != symbol.BracketTypeNames {
			return 0, 0, site, false
		}
		children := semanticRefs(ref.Module, node, tree)
		if len(children) == 0 {
			return 0, 0, site, false
		}
		id, kind, _, found := w.staticTarget(children[0], tree)
		return id, kind, ref, found
	case syntax.Name:
	default:
		return 0, 0, site, false
	}
	r, found := w.generation.inputs.Resolution.Reference(query)
	if !found || r.State != symbol.ResolutionResolved {
		return 0, 0, site, false
	}
	s, found := w.generation.inputs.Resolution.Symbols.Symbol(r.Symbol)
	if !found || s.Error {
		return 0, 0, site, false
	}
	switch s.Kind {
	case symbol.SymbolFunction, symbol.SymbolExternFunction, symbol.SymbolBuiltinFunction:
		return s.ID, callDirect, site, true
	case symbol.SymbolVariant:
		return s.ID, callVariant, site, true
	case symbol.SymbolMethod:
		// A method declared inside a nominal type body is a STATIC method
		// (callable on the bare type name) exactly when its first parameter is
		// not named "self" — the receiver parameter of an instance method. Its
		// qualified call lowers to a plain direct call to the method's own
		// function symbol with the authored arguments exactly as written, no
		// implicit self argument. An instance method (first parameter named
		// self) cannot be invoked through the type name; leaving it to the
		// ordinary member-call path rejects it for want of a receiver.
		signature, ok := w.program.Signature(s.ID)
		if !ok || signature.State != infer.DeclarationReady || methodHasSelf(signature, w.generation.inputs.Resolution) {
			return 0, 0, site, false
		}
		return s.ID, callDirect, site, true
	}
	return 0, 0, site, false
}

// methodHasSelf reports whether a method signature's first parameter is the
// authored receiver parameter named "self". A method whose first parameter is
// named self is an instance method; any other method — zero parameters or a
// first parameter carrying a different name — is a static method (an
// associated function) that takes its call-site arguments directly.
func methodHasSelf(signature infer.Signature, resolution *symbol.Result) bool {
	if len(signature.Parameters) == 0 {
		return false
	}
	parameter, ok := resolution.Symbols.Symbol(signature.Parameters[0])
	return ok && parameter.Name == "self"
}
func (w *walker) freshDestinations(refs []symbol.SyntaxRef, ctx walkContext) []typedValue {
	out := make([]typedValue, len(refs))
	for i, ref := range refs {
		origin := w.originForRef(ref, fmt.Sprintf("argument %d destination", i+1), ctx.typeOwner, ctx.genericOwner)
		out[i], _ = w.newSlotValue(w.session.Variable(origin), origin)
		w.expectations[ref] = w.expectationFor(ref, out[i].ID, compatibilityArgument)
	}
	return out
}
func (w *walker) explicitArgs(site symbol.SyntaxRef) []symbol.SyntaxRef {
	node, ok := w.node(site.Module, site.Node)
	if !ok || node.Kind() != syntax.BracketApply {
		return nil
	}
	item, _ := w.generation.inputs.Graph.Module(site.Module)
	children := semanticRefs(site.Module, node, item.Tree)
	if len(children) < 2 {
		return nil
	}
	return children[1:]
}

func (w *walker) prepareDirect(p *callPlan, ref symbol.SyntaxRef, ctx walkContext) {
	signature, ok := w.program.Signature(p.target.Symbol)
	if !ok || signature.State != infer.DeclarationReady {
		return
	}
	fixedCount := uint32(len(signature.Inputs))
	if signature.Variadic && len(signature.Inputs) != 0 {
		fixedCount--
	}
	p.target.Convention, p.target.ConventionKnown, p.target.FixedCount, p.target.Variadic = signature.Convention, true, fixedCount, signature.Variadic
	origin := w.originForRef(ref, "direct call", p.target.Symbol, ctx.genericOwner)
	if len(signature.TypeParams) != 0 {
		if p.target.Site == (symbol.SyntaxRef{}) {
			p.target.Site = ref
		}
		a, ok := w.prepareGeneric(p.target.Site, p.target.Symbol, w.explicitArgs(p.target.Site), ctx)
		if !ok {
			return
		}
		var result typedValue
		p.calleeValue, p.destinations, result = w.instantiateSignature(signature, a, origin)
		w.addConstraint(infer.Equal(p.result.Term, result.Term, origin))
		if ctx.expected.Destination != 0 && w.generation.hasValue(ctx.expected.Destination) {
			w.addConstraint(infer.Equal(p.result.Term, w.generation.values[ctx.expected.Destination-1].Term, origin))
		}
	} else {
		p.calleeValue, _ = w.newSlotValue(w.symbolTerm(p.target.Symbol, origin), origin)
		variadic := typedValue{}
		for i, t := range signature.Inputs {
			term := w.termForTemplate(t, nil, origin)
			v, _ := w.newSlotValue(term, origin)
			if template, found := w.program.Template(t); found && template.Kind == infer.TemplateKnown {
				v.Known = template.Known
				w.knownValues[v.ID] = template.Known
				w.rigidValues[v.ID] = w.isRigidType(template.Known)
			}
			if signature.Variadic && i == len(signature.Inputs)-1 {
				variadic = v
				continue
			}
			p.destinations = append(p.destinations, v)
			_ = i
		}
		if signature.Variadic {
			element := w.variadicElement(variadic, origin)
			// A variadic call whose tail has exactly one argument forwards the
			// whole slice directly when that argument's OWN statically-known
			// type is the variadic parameter's whole slice type (V1 parity:
			// `sum(s)` passes the slice through instead of collecting one
			// element). The argument's declared type is peeked BOTTOM-UP —
			// independently of any per-element expectation — before the
			// destinations are created, exactly as V1's check_expression
			// synthesizes the argument's type first and only then decides
			// between the slice-forwarding and single-element interpretations.
			// The equality check is deliberately strict: a sole tail argument
			// whose known type is a DIFFERENT slice (an element type that
			// happens to be a slice, or a mismatched element) keeps the
			// ordinary single-element destination, so no currently-accepted
			// call shape changes meaning.
			forward := typedValue{}
			if len(p.arguments) == int(p.target.FixedCount)+1 {
				if known := w.knownReferenceType(p.arguments[len(p.arguments)-1]); known != 0 && known == variadic.Known {
					forward, _ = w.newSlotValue(variadic.Term, origin)
					forward.Known = variadic.Known
					w.knownValues[forward.ID] = variadic.Known
					w.rigidValues[forward.ID] = w.isRigidType(variadic.Known)
				}
			}
			for i := int(p.target.FixedCount); i < len(p.arguments); i++ {
				v := forward
				if v.ID == 0 {
					term := element.Term
					if element.Known == 0 {
						term = w.session.Variable(origin)
					}
					v, _ = w.newSlotValue(term, origin)
					if element.Known != 0 {
						v.Known = element.Known
						w.knownValues[v.ID] = element.Known
						w.rigidValues[v.ID] = w.isRigidType(element.Known)
					}
				}
				p.destinations = append(p.destinations, v)
			}
		}
		w.addConstraint(infer.Equal(p.result.Term, w.termForTemplate(signature.Result, nil, origin), origin))
	}
	if len(p.destinations) < len(p.arguments) {
		p.destinations = append(p.destinations, w.freshDestinations(p.arguments[len(p.destinations):], ctx)...)
	}
	for i, a := range p.arguments {
		if i < len(p.destinations) {
			w.expectations[a] = w.expectationFor(a, p.destinations[i].ID, compatibilityArgument)
		}
	}
}

// variadicElement resolves the element type of a slice-typed variadic
// parameter so call-site arguments in the variadic tail can be checked
// individually against the slice's element type rather than against the
// slice itself. The parameter template is fully known for a nongeneric
// callable, so it is resolved to a concrete slice TypeID and the element
// type is taken from its TypeKey.
func (w *walker) variadicElement(parameter typedValue, origin infer.Origin) typedValue {
	if parameter.Known == 0 {
		return typedValue{}
	}
	key, ok := w.generation.inputs.Types.Key(parameter.Known)
	if !ok {
		return typedValue{}
	}
	element, ok := key.Child()
	if !ok {
		return typedValue{}
	}
	return typedValue{Term: w.session.Known(element), Known: element}
}

// knownReferenceType peeks the statically-known declared type of a simple
// reference expression — a Name or Path resolving to a binding or parameter
// whose declared annotation is already concretely known — WITHOUT pushing an
// expected type down or running any inference. It is the V1
// check_expression-equivalent "synthesize this expression's own type
// bottom-up first" operation for the limited shape whose type is already known
// at walk time: handleBinding and handleNamedCallable populate
// valuesBySymbol.Known for an annotated binding or a template-known parameter
// before any call referencing them is walked. It returns 0 for any expression
// whose type is not statically known (a call, an operator, a field read, a
// parenthesized expression, an unannotated binding), meaning the caller must
// fall back to the ordinary expected-type-driven checking.
func (w *walker) knownReferenceType(ref symbol.SyntaxRef) types.TypeID {
	node, ok := w.node(ref.Module, ref.Node)
	if !ok {
		return 0
	}
	switch node.Kind() {
	case syntax.Name:
	case syntax.Path:
		query := ref
		for i := len(node.Children()) - 1; i >= 0; i-- {
			child, ok := w.node(ref.Module, node.Children()[i])
			if ok && child.Kind() == syntax.Name {
				query.Node = node.Children()[i]
				break
			}
		}
		ref = query
	default:
		return 0
	}
	resolution, ok := w.generation.inputs.Resolution.Reference(ref)
	if !ok || resolution.State != symbol.ResolutionResolved || resolution.Symbol == 0 {
		return 0
	}
	return w.valuesBySymbol[resolution.Symbol].Known
}
func (w *walker) prepareVariant(p *callPlan, ref symbol.SyntaxRef, ctx walkContext) {
	member, ok := w.generation.inputs.Resolution.Symbols.Symbol(p.target.Symbol)
	if !ok {
		return
	}
	origin := w.originForRef(ref, "variant", p.target.Symbol, ctx.genericOwner)
	p.calleeValue, _ = w.newSlotValue(w.symbolTerm(p.target.Symbol, origin), origin)
	if d, ok := w.program.TypeDeclaration(member.Containing); ok {
		for _, m := range d.Members {
			if m.Symbol == p.target.Symbol {
				// The payload destination is anchored exactly as a non-generic
				// direct call anchors each of its parameters (see
				// prepareDirect): a member whose type is a known template is
				// published as a KNOWN destination, so a literal aggregate
				// payload (`Choice.value([1, 2, 3])`) grounds its elements to
				// the declared payload type at walk time instead of inferring
				// its own structural type that later fails classify() as
				// array/optional vs array/optional — the same C0601 a plain
				// `take([1, 2, 3])` call would get without the anchor. A
				// generic member (its type wraps a type parameter) still goes
				// through instantiate, exactly as prepareDirect's generic
				// branch instantiates its parameters.
				payloadOrigin := w.originForRef(ref, "variant payload", p.target.Symbol, ctx.genericOwner)
				term := w.termForTemplate(m.Type, d.Parameters, payloadOrigin)
				destination, _ := w.newSlotValue(term, payloadOrigin)
				if template, found := w.program.Template(m.Type); found && template.Kind == infer.TemplateKnown {
					destination.Known = template.Known
					w.knownValues[destination.ID] = template.Known
					w.rigidValues[destination.ID] = w.isRigidType(template.Known)
				}
				p.destinations = []typedValue{destination}
			}
		}
	}
	p.target.Convention, p.target.ConventionKnown, p.target.FixedCount = types.Pebble, true, uint32(len(p.destinations))
	w.addConstraint(infer.Equal(p.result.Term, w.symbolTerm(p.target.Symbol, origin), origin))
	for i, a := range p.arguments {
		if i < len(p.destinations) {
			w.expectations[a] = w.expectationFor(a, p.destinations[i].ID, compatibilityArgument)
		}
	}
}

func (w *walker) finishCall(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	if ctx.suppressValue {
		return
	}
	p := w.callPlans[ref]
	if p == nil {
		return
	}
	if p.deferredBracketCall {
		w.finishDeferredBracketCall(ref, node, ctx, p)
		return
	}
	origin := w.origin(ref, node, "call", ctx.typeOwner, ctx.genericOwner)
	callee := p.calleeValue
	receiver := typedValue{}
	if p.target.Kind == callIndirect || p.target.Kind == callMethod {
		callee = w.valuesBySyntax[p.callee]
		if p.target.Kind == callMethod {
			if bracket := w.bracketPlans[p.callee]; bracket != nil && bracket.choice != 0 {
				callee = bracket.genericResult
			}
		}
		if callee.ID == 0 || !w.successfulExpressions[p.callee] {
			w.failExpression(ref, origin)
			return
		}
	}
	var arguments []callArgument
	var callable []infer.CallableArgument
	for i, ar := range p.arguments {
		src := w.valuesBySyntax[ar]
		if src.ID == 0 || !w.successfulExpressions[ar] {
			w.failExpression(ref, origin)
			return
		}
		dst := src
		variadic := p.target.Variadic && i >= int(p.target.FixedCount)
		if i < len(p.destinations) {
			dst = p.destinations[i]
		}
		arguments = append(arguments, callArgument{Source: src.ID, Destination: dst.ID, Ordinal: uint32(i), Variadic: variadic})
		callable = append(callable, infer.CallableArgument{Source: src.Term, Destination: dst.Term})
		w.retainCompatibility(ref, ctx.genericOwner, src.ID, dst.ID, compatibilityArgument, uint32(i), 0, spanForRef(w.generation.inputs, ar), false)
	}
	if p.target.Kind == callIndirect {
		w.addConstraint(infer.Callable(callee.Term, callable, p.result.Term, origin))
	} else if p.target.Kind == callDirect {
		// Directly-called top-level generic functions need every argument tied
		// to its instantiated parameter so a non-literal argument grounds the
		// inferred type argument (literal arguments already connect through the
		// literal-fit expectation). Without this the parameter stays a free
		// variable whenever the result context does not name the type either.
		if signature, ok := w.program.Signature(p.target.Symbol); ok && len(signature.TypeParams) != 0 {
			for _, argument := range callable {
				w.addConstraint(infer.Equal(argument.Source, argument.Destination, origin))
			}
		}
	} else if p.target.Kind == callMethod {
		m := w.memberPlans[p.method]
		if m != nil {
			receiver = w.valuesBySyntax[m.base]
		}
		if bracket := w.bracketPlans[p.method]; bracket != nil && bracket.deferredMember != nil {
			receiver = w.valuesBySyntax[bracket.deferredMember.base]
		}
		member := w.memberPlans[p.method]
		if member != nil && receiver.ID != 0 {
			w.addConstraint(infer.CallMember(receiver.Term, member.nameText, callee.Term, callable, p.result.Term, nil, p.target.Site, origin))
		}
	}
	result, ok := w.publishExistingSyntax(ref, p.result, origin)
	if !ok {
		return
	}
	w.successfulExpressions[ref] = true
	w.applyExpected(result, infer.Term{}, ctx.expected, origin)
	header := w.header(ref, ctx.genericOwner, false)
	record := callRecord{Header: header, Callee: callee.ID, Receiver: receiver.ID, Result: result.ID, Arguments: arguments, Target: p.target}
	specialized, ok := w.addRecord(retainedRecord{Header: header, Call: &record})
	if !ok {
		return
	}
	if p.target.Kind == callIndirect {
		w.retainRequirement(header, requirementUnsupportedCall, callee.ID)
	}
	runtime, ready := w.program.RuntimeTypes()
	if ready {
		kind := contextIndirect
		if p.target.Kind != callIndirect {
			kind = contextForward
			if p.target.ConventionKnown && p.target.Convention == types.C {
				kind = contextNone
			}
		}
		flow := contextFlowRecord{Header: header, Kind: kind, Caller: ctx.callable, Callee: callee.ID, Context: runtime.Context}
		w.addRecord(retainedRecord{Header: header, ContextFlow: &flow})
	}
	var children []valueID
	if p.target.Kind == callIndirect {
		children = append(children, callee.ID)
	} else if receiver.ID != 0 {
		children = append(children, receiver.ID)
	}
	for _, a := range arguments {
		children = append(children, a.Source)
	}
	expression := expressionRecord{Header: header, Kind: expressionCall, Result: result.ID, Children: children, Symbol: p.target.Symbol, Specialized: specialized}
	w.addRecord(retainedRecord{Header: header, Expression: &expression})
}

func (w *walker) finishDeferredBracketCall(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, p *callPlan) {
	bracket := w.bracketPlans[p.callee]
	if bracket == nil || bracket.genericBranch == nil || bracket.runtimeBranch == nil || bracket.deferredMember == nil {
		return
	}
	receiver := w.valuesBySyntax[bracket.deferredMember.base]
	if receiver.ID == 0 {
		return
	}
	sources := make([]typedValue, len(p.arguments))
	for i, argument := range p.arguments {
		sources[i] = w.valuesBySyntax[argument]
		if sources[i].ID == 0 || !w.successfulExpressions[argument] {
			return
		}
	}
	previous := w.activeBranch
	w.activeBranch = bracket.genericBranch
	w.retainDeferredCallAlternative(ref, node, ctx, p.arguments, sources, p.genericDestinations, p.genericResult, bracket.genericResult, receiver, callMethod, p.target)
	w.activeBranch = bracket.runtimeBranch
	w.retainDeferredCallAlternative(ref, node, ctx, p.arguments, sources, p.runtimeDestinations, p.runtimeResult, bracket.result, typedValue{}, callIndirect, callTarget{Kind: callIndirect})
	w.activeBranch = previous
	w.successfulExpressions[ref] = w.finalizeDeferredChoice(p.callee, ctx, bracket)
	if w.successfulExpressions[ref] {
		// There is deliberately no ordinary syntax publication for the call:
		// its two possible values are represented by the guarded result slots.
	}
}

func (w *walker) retainDeferredCallAlternative(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, argumentRefs []symbol.SyntaxRef, sources, destinations []typedValue, result, callee, receiver typedValue, kind callKind, target callTarget) {
	origin := w.origin(ref, node, "deferred call", ctx.typeOwner, ctx.genericOwner)
	w.queueBranchRoot(result)
	arguments := make([]callArgument, 0, len(sources))
	callable := make([]infer.CallableArgument, 0, len(sources))
	for i, source := range sources {
		destination := destinations[i]
		w.queueBranchRoot(destination)
		arguments = append(arguments, callArgument{Source: source.ID, Destination: destination.ID, Ordinal: uint32(i)})
		callable = append(callable, infer.CallableArgument{Source: source.Term, Destination: destination.Term})
		w.retainCompatibility(ref, ctx.genericOwner, source.ID, destination.ID, compatibilityArgument, uint32(i), 0, spanForRef(w.generation.inputs, argumentRefs[i]), false)
	}
	if kind == callMethod {
		shapes := []infer.Shape{infer.Leaf(receiver.Term)}
		for _, destination := range destinations {
			shapes = append(shapes, infer.Leaf(destination.Term))
		}
		w.addConstraint(infer.ConstrainShape(callee.Term, infer.FunctionShape(types.Pebble, shapes, infer.Leaf(result.Term), false), origin))
		target.Kind, target.Site = callMethod, ref
	} else {
		w.addConstraint(infer.Callable(callee.Term, callable, result.Term, origin))
	}
	if ctx.expected.Destination != 0 && w.generation.hasValue(ctx.expected.Destination) {
		w.addConstraint(infer.Equal(result.Term, w.generation.values[ctx.expected.Destination-1].Term, origin))
		w.retainCompatibility(ref, ctx.genericOwner, result.ID, ctx.expected.Destination, ctx.expected.Role, 0, 0, spanForRef(w.generation.inputs, ref), false)
	}
	header := w.header(ref, ctx.genericOwner, false)
	record := callRecord{Header: header, Callee: callee.ID, Receiver: receiver.ID, Result: result.ID, Arguments: arguments, Target: target}
	specialized, ok := w.addRecord(retainedRecord{Header: header, Call: &record})
	if !ok {
		return
	}
	if kind == callIndirect {
		w.retainRequirement(header, requirementUnsupportedCall, callee.ID)
	}
	if runtime, ready := w.program.RuntimeTypes(); ready {
		flowKind := contextIndirect
		if kind == callMethod {
			flowKind = contextForward
		}
		flow := contextFlowRecord{Header: header, Kind: flowKind, Caller: ctx.callable, Callee: callee.ID, Context: runtime.Context}
		w.addRecord(retainedRecord{Header: header, ContextFlow: &flow})
	}
	children := make([]valueID, 0, len(sources)+1)
	if kind == callIndirect {
		children = append(children, callee.ID)
	} else {
		children = append(children, receiver.ID)
	}
	for _, source := range sources {
		children = append(children, source.ID)
	}
	expression := expressionRecord{Header: header, Kind: expressionCall, Result: result.ID, Children: children, Specialized: specialized}
	w.addRecord(retainedRecord{Header: header, Expression: &expression})
}
