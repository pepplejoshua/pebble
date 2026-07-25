package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type walkContext struct {
	callable       callableRef
	typeOwner      symbol.SymbolID
	genericOwner   symbol.SymbolID
	nominalOwner   symbol.SymbolID
	unsupported    bool
	typePosition   bool
	typeRoot       bool
	preparedType   bool
	controlDepth   uint32
	expected       expectedType
	suppressValue  bool
	immediateCall  bool
	callSite       symbol.SyntaxRef
	deferredMember bool
	branch         *branchFacts
}

type branchRoot struct {
	value typedValue
}

type branchInstantiation struct {
	site      symbol.SyntaxRef
	generic   symbol.SymbolID
	arguments []infer.Term
}

type pendingBranchRecord struct {
	local recordID
	value retainedRecord
}

type branchFacts struct {
	index          uint32
	constraints    []infer.Constraint
	records        []pendingBranchRecord
	roots          []branchRoot
	instantiations []branchInstantiation
	rooted         map[valueID]bool
	requirements   uint32
	typeSibling    *branchFacts
	typeOwner      symbol.SymbolID
	genericOwner   symbol.SymbolID
}

type walkItem struct {
	ref  symbol.SyntaxRef
	ctx  walkContext
	exit bool
}

type walker struct {
	generation            *generation
	evaluator             *constantEvaluator
	program               *infer.Program
	session               *infer.Session
	visited               map[symbol.SyntaxRef]bool
	order                 []symbol.SyntaxRef
	symbolsAt             map[symbol.SyntaxRef][]symbol.Symbol
	resolvedTypes         map[symbol.SyntaxRef]bool
	preparedTypes         map[symbol.SyntaxRef]bool
	valuesBySyntax        map[symbol.SyntaxRef]typedValue
	valuesBySymbol        map[symbol.SymbolID]typedValue
	termsBySymbol         map[symbol.SymbolID]infer.Term
	knownValues           map[valueID]types.TypeID
	rigidValues           map[valueID]bool
	expectations          map[symbol.SyntaxRef]expectedType
	optionalDestinations  map[symbol.SyntaxRef]valueID
	expressionPlans       map[symbol.SyntaxRef]*expressionPlan
	memberPlans           map[symbol.SyntaxRef]*memberPlan
	callPlans             map[symbol.SyntaxRef]*callPlan
	bracketPlans          map[symbol.SyntaxRef]*bracketPlan
	operatorPlans         map[symbol.SyntaxRef]*operatorPlan
	castPlans             map[symbol.SyntaxRef]*castPlan
	assignmentPlans       map[symbol.SyntaxRef]*assignmentPlan
	slicePlans            map[symbol.SyntaxRef]*slicePlan
	escapeDestinations    map[symbol.SyntaxRef]symbol.SymbolID
	placeCandidates       map[symbol.SyntaxRef]valueID
	places                map[symbol.SyntaxRef]placeCandidate
	successfulExpressions map[symbol.SyntaxRef]bool
	publishedSymbols      map[symbol.SymbolID]bool
	publishedSyntax       map[symbol.SyntaxRef]bool
	publishedSlots        map[infer.Term]infer.SlotID
	activeBranch          *branchFacts
	runtimeTypes          func() (infer.RuntimeTypes, bool)
}

type preparedFacts struct {
	Generation *generation
	Constants  *constantEvaluator
	Program    *infer.Program
	Session    *infer.Session
	Walk       *walker
}

func run06a3(inputs Inputs, diagnostics *diagnostic.DiagnosticSet, config Config) *preparedFacts {
	generation := newGeneration(inputs, diagnostics, config)
	evaluator := newConstantEvaluatorWithBudget(inputs, diagnostics, config, generation.reporter.budget)
	program := infer.Prepare(infer.ProgramInputs{
		Graph: inputs.Graph, Sources: inputs.Sources, Resolution: inputs.Resolution,
		Types: inputs.Types, ArrayLengths: evaluator, LiteralTarget: inputs.LiteralTarget,
	}, diagnostics, config.Inference)
	session := infer.NewSession(program, diagnostics, config.Inference)
	walk := newWalker(generation, evaluator, program, session)
	walk.run()
	generation.reporter.flush()
	return &preparedFacts{Generation: generation, Constants: evaluator, Program: program, Session: session, Walk: walk}
}

func newWalker(generation *generation, evaluator *constantEvaluator, program *infer.Program, session *infer.Session) *walker {
	w := &walker{
		generation: generation, evaluator: evaluator, program: program, session: session,
		visited: make(map[symbol.SyntaxRef]bool), symbolsAt: make(map[symbol.SyntaxRef][]symbol.Symbol),
		resolvedTypes: make(map[symbol.SyntaxRef]bool), preparedTypes: make(map[symbol.SyntaxRef]bool),
		valuesBySyntax: make(map[symbol.SyntaxRef]typedValue), publishedSymbols: make(map[symbol.SymbolID]bool),
		valuesBySymbol: make(map[symbol.SymbolID]typedValue), termsBySymbol: make(map[symbol.SymbolID]infer.Term), knownValues: make(map[valueID]types.TypeID), rigidValues: make(map[valueID]bool),
		expectations: make(map[symbol.SyntaxRef]expectedType), optionalDestinations: make(map[symbol.SyntaxRef]valueID), expressionPlans: make(map[symbol.SyntaxRef]*expressionPlan),
		memberPlans: make(map[symbol.SyntaxRef]*memberPlan), callPlans: make(map[symbol.SyntaxRef]*callPlan), bracketPlans: make(map[symbol.SyntaxRef]*bracketPlan),
		operatorPlans: make(map[symbol.SyntaxRef]*operatorPlan), castPlans: make(map[symbol.SyntaxRef]*castPlan), assignmentPlans: make(map[symbol.SyntaxRef]*assignmentPlan), slicePlans: make(map[symbol.SyntaxRef]*slicePlan),
		escapeDestinations:    make(map[symbol.SyntaxRef]symbol.SymbolID),
		placeCandidates:       make(map[symbol.SyntaxRef]valueID),
		places:                make(map[symbol.SyntaxRef]placeCandidate),
		successfulExpressions: make(map[symbol.SyntaxRef]bool),
		publishedSyntax:       make(map[symbol.SyntaxRef]bool), publishedSlots: make(map[infer.Term]infer.SlotID),
	}
	if program != nil {
		w.runtimeTypes = program.RuntimeTypes
	}
	if generation != nil && generation.inputs.Resolution != nil && generation.inputs.Resolution.Symbols != nil {
		for _, value := range generation.inputs.Resolution.Symbols.All() {
			if value.Declaration != (symbol.SyntaxRef{}) {
				w.symbolsAt[value.Declaration] = append(w.symbolsAt[value.Declaration], value)
			}
		}
	}
	return w
}

func (w *walker) run() {
	if w == nil || w.generation == nil || w.generation.inputs.Graph == nil {
		return
	}
	for _, moduleID := range w.generation.inputs.Graph.DependencyOrder() {
		item, ok := w.generation.inputs.Graph.Module(moduleID)
		if !ok || item.Tree == nil {
			w.generation.report("dependency order contains an invalid module", source.Span{})
			continue
		}
		w.walkTree(item)
	}
}

func (w *walker) walkTree(item module.Module) {
	stack := []walkItem{{ref: symbol.SyntaxRef{Module: item.ID, Node: item.Tree.Root()}}}
	for len(stack) != 0 {
		last := len(stack) - 1
		current := stack[last]
		stack = stack[:last]
		if current.exit {
			w.activeBranch = current.ctx.branch
			if item.Tree != nil {
				if node, ok := item.Tree.Node(current.ref.Node); ok {
					switch node.Kind() {
					case syntax.CallExpr:
						w.finishCall(current.ref, node, current.ctx)
					case syntax.MemberExpr:
						w.finishMember(current.ref, node, current.ctx)
					case syntax.BracketApply:
						w.finishBracket(current.ref, node, current.ctx, item.Tree)
					case syntax.PrefixTerm, syntax.PostfixExpr, syntax.BinaryExpr:
						w.finishOperator(current.ref, node, current.ctx)
					case syntax.CastExpr:
						w.finishCast(current.ref, node, current.ctx)
					case syntax.AssignmentStmt:
						w.finishAssignment(current.ref, node, current.ctx)
					case syntax.SliceExpr:
						w.finishSlice(current.ref, node, current.ctx)
					default:
						w.finishExpression(current.ref, node, current.ctx, item.Tree)
					}
				}
			}
			w.activeBranch = nil
			w.generation.leaveTraversal()
			continue
		}
		if !w.generation.enterTraversal() {
			continue
		}
		if !w.generation.chargeSyntaxVisit() {
			w.generation.leaveTraversal()
			continue
		}
		if w.visited[current.ref] {
			w.generation.report("syntax node visited more than once", spanForRef(w.generation.inputs, current.ref))
			w.generation.leaveTraversal()
			continue
		}
		w.visited[current.ref] = true
		w.order = append(w.order, current.ref)
		if expected, ok := w.expectations[current.ref]; ok {
			current.ctx.expected = expected
		}
		node, ok := item.Tree.Node(current.ref.Node)
		if !ok {
			w.generation.report("traversal reached an invalid syntax node", spanForRef(w.generation.inputs, current.ref))
			w.generation.leaveTraversal()
			continue
		}
		w.activeBranch = current.ctx.branch
		children := w.dispatch(current.ref, node, current.ctx, item.Tree)
		w.activeBranch = nil
		stack = append(stack, walkItem{ref: current.ref, ctx: current.ctx, exit: true})
		for index := len(children) - 1; index >= 0; index-- {
			stack = append(stack, children[index])
		}
	}
}

func (w *walker) addConstraint(value infer.Constraint) infer.ConstraintID {
	if w.activeBranch != nil {
		w.activeBranch.constraints = append(w.activeBranch.constraints, value)
		return 0
	}
	return w.session.Add(value)
}

func (w *walker) addRecord(value retainedRecord) (recordID, bool) {
	if w.session == nil || w.session.Fatal() {
		return 0, false
	}
	if w.activeBranch == nil {
		return w.generation.addRecord(value)
	}
	if uint64(len(w.activeBranch.records)) >= uint64(^uint32(0)) {
		w.generation.report("too many deferred-branch records", value.Header.Span)
		return 0, false
	}
	id := recordID(len(w.activeBranch.records) + 1)
	value.Header.ID = 0
	w.activeBranch.records = append(w.activeBranch.records, pendingBranchRecord{local: id, value: cloneRetainedRecord(value)})
	return id, true
}

func (w *walker) publishInstantiation(site symbol.SyntaxRef, generic symbol.SymbolID, arguments []infer.Term) {
	if w.activeBranch == nil {
		w.session.PublishInstantiation(site, generic, arguments)
		return
	}
	w.activeBranch.instantiations = append(w.activeBranch.instantiations, branchInstantiation{
		site: site, generic: generic, arguments: append([]infer.Term(nil), arguments...),
	})
}

func (w *walker) queueBranchRoot(value typedValue) bool {
	branch := w.activeBranch
	if branch == nil || value.ID == 0 || !w.generation.hasValue(value.ID) {
		return false
	}
	if branch.rooted == nil {
		branch.rooted = make(map[valueID]bool)
	}
	if branch.rooted[value.ID] {
		return true
	}
	branch.rooted[value.ID] = true
	branch.roots = append(branch.roots, branchRoot{value: value})
	return true
}

func childItems(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) []walkItem {
	children := node.Children()
	out := make([]walkItem, len(children))
	for index, id := range children {
		childContext := ctx
		childContext.typeRoot = false
		out[index] = walkItem{ref: symbol.SyntaxRef{Module: ref.Module, Node: id}, ctx: childContext}
	}
	return out
}

func (w *walker) dispatch(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	if ctx.unsupported {
		return childItems(ref, node, ctx)
	}
	switch node.Kind() {
	case syntax.Missing:
		_ = w.session.Error(w.origin(ref, node, "missing syntax", ctx.typeOwner, ctx.genericOwner))
		return nil
	case syntax.Error:
		_ = w.session.Error(w.origin(ref, node, "error syntax", ctx.typeOwner, ctx.genericOwner))
		return nil
	case syntax.File, syntax.ImportDecl, syntax.ExternDecl, syntax.ExternBlock,
		syntax.BlockStmt, syntax.ReturnStmt, syntax.IfStmt, syntax.WhileStmt,
		syntax.RangeLoopStmt, syntax.ForStmt, syntax.SwitchStmt, syntax.SwitchCase,
		syntax.DeferStmt, syntax.PrintStmt, syntax.BreakStmt, syntax.ContinueStmt,
		syntax.ExpressionStmt,
		syntax.StructType, syntax.UnionType, syntax.EnumType,
		syntax.FieldDecl, syntax.VariantDecl, syntax.Parameter, syntax.TypeParameter:
		return w.structuralChildren(ref, node, ctx, tree)
	case syntax.CallExpr:
		return w.prepareCall(ref, node, ctx, tree)
	case syntax.MemberExpr:
		return w.prepareMember(ref, node, ctx, tree)
	case syntax.BracketApply:
		return w.prepareBracket(ref, node, ctx, tree)
	case syntax.PrefixTerm, syntax.PostfixExpr, syntax.BinaryExpr:
		return w.prepareOperator(ref, node, ctx, tree)
	case syntax.CastExpr:
		return w.prepareCast(ref, node, ctx, tree)
	case syntax.AssignmentStmt:
		return w.prepareAssignment(ref, node, ctx, tree)
	case syntax.SliceExpr:
		return w.prepareSlice(ref, node, ctx, tree)
	case syntax.Name, syntax.Path, syntax.Literal, syntax.InterpolatedString,
		syntax.SomeExpr, syntax.SizeofExpr, syntax.GroupedTerm, syntax.TupleTerm,
		syntax.ArrayExpr, syntax.ArrayRepeatExpr, syntax.RecordExpr, syntax.RecordField,
		syntax.PartialMemberExpr:
		return w.prepareExpression(ref, node, ctx, tree)
	case syntax.EndOfFile:
		return nil
	case syntax.BindingDecl, syntax.ExternBinding:
		w.handleBinding(ref, node, ctx)
		return w.structuralChildren(ref, node, ctx, tree)
	case syntax.TypeDecl, syntax.ExternType:
		w.handleTypeDeclaration(ref, node)
		return w.structuralChildren(ref, node, ctx, tree)
	case syntax.FunctionDecl, syntax.ExternFunction:
		w.handleNamedCallable(ref, node)
		return w.callableChildren(ref, node, ctx, tree)
	case syntax.ContextExpr:
		w.handleContext(ref, node, ctx)
		return nil
	case syntax.OptionalType, syntax.SliceType, syntax.ArrayType:
		return childItems(ref, node, ctx)
	case syntax.FunctionTerm:
		if node.Data()&syntax.FunctionBodyPresent == 0 {
			return childItems(ref, node, ctx)
		}
		unsupported := w.handleFunctionLiteral(ref, node, ctx)
		items := childItems(ref, node, ctx)
		item, _ := w.generation.inputs.Graph.Module(ref.Module)
		_, _, _, bodyNode := functionParts(item.Tree, node)
		for index := range items {
			items[index].ctx.callable = callableRef{Syntax: ref}
			items[index].ctx.unsupported = unsupported
			items[index].ctx.controlDepth = 0
			items[index].ctx.suppressValue = items[index].ref.Node != bodyNode
		}
		return items
	default:
		w.generation.report("unknown syntax node kind in closed dispatch", node.Span())
		return childItems(ref, node, ctx)
	}
}

func (w *walker) structuralChildren(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
	switch node.Kind() {
	case syntax.ImportDecl, syntax.ExternBinding, syntax.Parameter, syntax.TypeParameter, syntax.FieldDecl, syntax.VariantDecl:
		for index := range items {
			items[index].ctx.suppressValue = true
		}
	case syntax.BindingDecl:
		_, initializer, _, initializerPresent := bindingParts(ref, node)
		for index := range items {
			items[index].ctx.suppressValue = !initializerPresent || items[index].ref != initializer
		}
	case syntax.TypeDecl, syntax.ExternType:
		for index := range items {
			child, _ := tree.Node(items[index].ref.Node)
			switch child.Kind() {
			case syntax.StructType, syntax.UnionType, syntax.EnumType:
				items[index].ctx.suppressValue = false
			default:
				items[index].ctx.suppressValue = true
			}
		}
	case syntax.StructType, syntax.UnionType, syntax.EnumType:
		for index := range items {
			child, _ := tree.Node(items[index].ref.Node)
			if child.Kind() == syntax.Name || child.Kind() == syntax.Literal {
				items[index].ctx.suppressValue = true
			}
		}
	}
	if isControlContainer(node.Kind()) {
		if ctx.controlDepth >= w.generation.config.MaxControlDepth {
			w.generation.reportLimit("control depth", uint64(w.generation.config.MaxControlDepth))
		} else {
			for index := range items {
				items[index].ctx.controlDepth = ctx.controlDepth + 1
			}
		}
	}
	if node.Kind() == syntax.TypeDecl {
		for _, value := range w.declarationSymbols(ref) {
			if value.Kind == symbol.SymbolType || value.Kind == symbol.SymbolExternType {
				ctx.nominalOwner, ctx.typeOwner = value.ID, value.ID
				if descriptor, ok := w.program.TypeDeclaration(value.ID); ok && len(descriptor.Parameters) != 0 {
					ctx.genericOwner = value.ID
				} else {
					ctx.genericOwner = 0
				}
				break
			}
		}
		for index := range items {
			items[index].ctx.nominalOwner, items[index].ctx.typeOwner, items[index].ctx.genericOwner = ctx.nominalOwner, ctx.typeOwner, ctx.genericOwner
			child, _ := tree.Node(items[index].ref.Node)
			if child.Kind() != syntax.Name && child.Kind() != syntax.TypeParameter && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				items[index].ctx.typePosition, items[index].ctx.typeRoot, items[index].ctx.preparedType = true, true, true
			}
		}
	}
	if node.Kind() == syntax.Parameter && ctx.callable.Symbol != 0 {
		for index := len(items) - 1; index >= 0; index-- {
			child, _ := tree.Node(items[index].ref.Node)
			if child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				items[index].ctx.typePosition, items[index].ctx.typeRoot, items[index].ctx.preparedType = true, true, true
				break
			}
		}
	}
	if node.Kind() == syntax.FieldDecl || node.Kind() == syntax.VariantDecl {
		for index := len(items) - 1; index >= 0; index-- {
			child, _ := tree.Node(items[index].ref.Node)
			if child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
				items[index].ctx.typePosition, items[index].ctx.typeRoot, items[index].ctx.preparedType = true, true, true
				break
			}
		}
	}
	return items
}

func isControlContainer(kind syntax.NodeKind) bool {
	switch kind {
	case syntax.BlockStmt, syntax.IfStmt, syntax.WhileStmt, syntax.RangeLoopStmt,
		syntax.ForStmt, syntax.SwitchStmt, syntax.SwitchCase, syntax.DeferStmt:
		return true
	default:
		return false
	}
}

func (w *walker) callableChildren(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
	callable, ok := w.callableSymbol(ref)
	if !ok {
		return items
	}
	_, _, resultNode, bodyNode := functionParts(tree, node)
	for index := range items {
		items[index].ctx.suppressValue = items[index].ref.Node != bodyNode
		items[index].ctx.callable = callableRef{Symbol: callable.ID, Syntax: ref}
		items[index].ctx.typeOwner = callable.ID
		if signature, prepared := w.program.Signature(callable.ID); prepared && len(signature.TypeParams) != 0 {
			items[index].ctx.genericOwner = callable.ID
		} else {
			items[index].ctx.genericOwner = 0
		}
		if callable.Kind == symbol.SymbolMethod {
			items[index].ctx.nominalOwner = callable.Containing
		}
		if items[index].ref.Node == resultNode {
			items[index].ctx.typePosition, items[index].ctx.typeRoot, items[index].ctx.preparedType = true, true, true
		}
	}
	return items
}

func dispatchedNodeKind(kind syntax.NodeKind) bool {
	switch kind {
	case syntax.Missing, syntax.Error, syntax.File, syntax.EndOfFile, syntax.ImportDecl,
		syntax.BindingDecl, syntax.TypeDecl, syntax.FunctionDecl, syntax.ExternDecl,
		syntax.ExternBlock, syntax.ExternFunction, syntax.ExternType, syntax.ExternBinding,
		syntax.Parameter, syntax.TypeParameter, syntax.BlockStmt, syntax.ReturnStmt,
		syntax.IfStmt, syntax.WhileStmt, syntax.RangeLoopStmt, syntax.ForStmt,
		syntax.SwitchStmt, syntax.SwitchCase, syntax.DeferStmt, syntax.PrintStmt,
		syntax.BreakStmt, syntax.ContinueStmt, syntax.AssignmentStmt, syntax.ExpressionStmt,
		syntax.Name, syntax.Path, syntax.Literal, syntax.InterpolatedString,
		syntax.ContextExpr, syntax.SomeExpr, syntax.SizeofExpr, syntax.PrefixTerm,
		syntax.PostfixExpr, syntax.BinaryExpr, syntax.CastExpr, syntax.CallExpr,
		syntax.BracketApply, syntax.SliceExpr, syntax.MemberExpr, syntax.GroupedTerm,
		syntax.TupleTerm, syntax.ArrayExpr, syntax.ArrayRepeatExpr, syntax.RecordExpr,
		syntax.RecordField, syntax.PartialMemberExpr, syntax.OptionalType, syntax.SliceType,
		syntax.ArrayType, syntax.FunctionTerm, syntax.StructType, syntax.UnionType,
		syntax.EnumType, syntax.FieldDecl, syntax.VariantDecl:
		return true
	default:
		return false
	}
}

func (w *walker) publishSymbol(id symbol.SymbolID, term infer.Term, origin infer.Origin) (typedValue, bool) {
	if w.session == nil || w.session.Fatal() {
		return typedValue{Term: term}, false
	}
	if w.activeBranch != nil {
		if id == 0 || w.publishedSymbols[id] {
			return typedValue{}, false
		}
		previous, hadPrevious := w.termsBySymbol[id]
		if existing, ok := w.termsBySymbol[id]; ok && existing != term {
			w.addConstraint(infer.Equal(existing, term, origin))
			if w.session.Fatal() {
				return typedValue{Term: existing}, false
			}
			term = existing
		} else {
			w.termsBySymbol[id] = term
		}
		value := w.newValue(term, origin)
		if !w.queueBranchRoot(value) {
			if value.ID != 0 && int(value.ID) == len(w.generation.values) {
				w.generation.values = w.generation.values[:len(w.generation.values)-1]
			}
			if hadPrevious {
				w.termsBySymbol[id] = previous
			} else {
				delete(w.termsBySymbol, id)
			}
			return typedValue{}, false
		}
		w.publishedSymbols[id] = true
		w.valuesBySymbol[id] = value
		return value, true
	}
	root := valueRoot{Kind: rootSymbol, Symbol: id}
	if id == 0 || w.publishedSymbols[id] || !w.canPublish(root) {
		w.generation.report("invalid, duplicate, or over-limit symbol publication", origin.Span)
		return typedValue{}, false
	}
	previous, hadPrevious := w.termsBySymbol[id]
	restoreTerm := func() {
		if hadPrevious {
			w.termsBySymbol[id] = previous
		} else {
			delete(w.termsBySymbol, id)
		}
	}
	if existing, ok := w.termsBySymbol[id]; ok && existing != term {
		w.addConstraint(infer.Equal(existing, term, origin))
		if w.session.Fatal() {
			return typedValue{Term: existing}, false
		}
		term = existing
	} else {
		w.termsBySymbol[id] = term
	}
	value, ok := w.commitPublication(term, origin, root)
	if !ok {
		restoreTerm()
		return typedValue{}, false
	}
	w.session.PublishSymbol(id, term)
	if w.session.Fatal() {
		delete(w.generation.roots.byValue, value.ID)
		w.generation.roots.values = w.generation.roots.values[:len(w.generation.roots.values)-1]
		w.generation.values = w.generation.values[:len(w.generation.values)-1]
		restoreTerm()
		return typedValue{Term: term}, false
	}
	w.publishedSymbols[id] = true
	w.valuesBySymbol[id] = value
	return value, true
}

func (w *walker) symbolTerm(id symbol.SymbolID, origin infer.Origin) infer.Term {
	if term, ok := w.termsBySymbol[id]; ok {
		return term
	}
	term := w.session.Variable(origin)
	w.termsBySymbol[id] = term
	return term
}

func (w *walker) publishSyntax(ref symbol.SyntaxRef, term infer.Term, origin infer.Origin) (typedValue, bool) {
	if w.session == nil || w.session.Fatal() {
		return typedValue{Term: term}, false
	}
	if w.activeBranch != nil {
		if existing, reserved := w.valuesBySyntax[ref]; reserved && existing.ID != 0 && !w.publishedSyntax[ref] {
			w.addConstraint(infer.Equal(existing.Term, term, origin))
			if w.session.Fatal() {
				return existing, false
			}
			return w.publishExistingSyntax(ref, existing, origin)
		}
		if w.publishedSyntax[ref] {
			return typedValue{}, false
		}
		value := w.newValue(term, origin)
		return w.publishExistingSyntax(ref, value, origin)
	}
	if existing, reserved := w.valuesBySyntax[ref]; reserved && existing.ID != 0 && !w.publishedSyntax[ref] {
		w.addConstraint(infer.Equal(existing.Term, term, origin))
		if w.session.Fatal() {
			return existing, false
		}
		return w.publishExistingSyntax(ref, existing, origin)
	}
	root := valueRoot{Kind: rootSyntax, Syntax: ref}
	if w.publishedSyntax[ref] || !w.canPublish(root) {
		w.generation.report("invalid, duplicate, or over-limit syntax publication", origin.Span)
		return typedValue{}, false
	}
	value, ok := w.commitPublication(term, origin, root)
	if !ok {
		return typedValue{}, false
	}
	w.session.PublishSyntax(ref, term)
	if w.session.Fatal() {
		delete(w.generation.roots.byValue, value.ID)
		w.generation.roots.values = w.generation.roots.values[:len(w.generation.roots.values)-1]
		w.generation.values = w.generation.values[:len(w.generation.values)-1]
		return typedValue{Term: term}, false
	}
	w.publishedSyntax[ref] = true
	w.valuesBySyntax[ref] = value
	return value, true
}

func (w *walker) publishExistingSyntax(ref symbol.SyntaxRef, value typedValue, origin infer.Origin) (typedValue, bool) {
	if w.session == nil || w.session.Fatal() {
		return value, false
	}
	if w.activeBranch != nil {
		if value.ID == 0 || w.publishedSyntax[ref] || !w.queueBranchRoot(value) {
			return value, false
		}
		// This records that the authored occurrence was generated. The actual
		// solution publication is an alternative-guarded slot, never syntax.
		w.publishedSyntax[ref] = true
		w.valuesBySyntax[ref] = value
		return value, true
	}
	if value.ID == 0 || w.publishedSyntax[ref] || !w.canPublish(valueRoot{Kind: rootSyntax, Syntax: ref}) {
		w.generation.report("invalid, duplicate, or over-limit syntax publication", origin.Span)
		return value, false
	}
	if !w.generation.addRoot(value.ID, valueRoot{Kind: rootSyntax, Syntax: ref}) {
		return value, false
	}
	w.session.PublishSyntax(ref, value.Term)
	if w.session.Fatal() {
		delete(w.generation.roots.byValue, value.ID)
		w.generation.roots.values = w.generation.roots.values[:len(w.generation.roots.values)-1]
		w.generation.values[value.ID-1].Root = valueRoot{}
		return value, false
	}
	w.publishedSyntax[ref] = true
	w.valuesBySyntax[ref] = value
	return value, true
}

func (w *walker) rootExistingSlot(value typedValue, origin infer.Origin) (typedValue, bool) {
	if w.session == nil || w.session.Fatal() {
		return value, false
	}
	if w.activeBranch != nil {
		return value, w.queueBranchRoot(value)
	}
	g := w.generation
	if g == nil || g.state != generationMutable || value.ID == 0 || !g.hasValue(value.ID) || g.values[value.ID-1].Term != value.Term {
		if g != nil {
			g.report("invalid recovery slot value", origin.Span)
		}
		return value, false
	}
	if root, rooted := g.roots.root(value.ID); rooted {
		return value, root.Kind == rootSlot && !root.Alternative.Guarded
	}
	slot, exists := w.publishedSlots[value.Term]
	if !exists {
		slot = w.session.PublishSlot(value.Term)
		if slot == (infer.SlotID{}) {
			if w.session.Fatal() {
				return value, false
			}
			g.report("inference rejected recovery slot publication", origin.Span)
			return value, false
		}
	}
	if !g.addRoot(value.ID, valueRoot{Kind: rootSlot, Slot: slot}) {
		g.report("recovery slot publication could not be rooted", origin.Span)
		return value, false
	}
	if !exists {
		w.publishedSlots[value.Term] = slot
	}
	return value, true
}

func (w *walker) canPublish(root valueRoot) bool {
	g := w.generation
	if g == nil || g.state != generationMutable || uint64(len(g.values)) >= uint64(g.config.MaxSyntaxVisits) || uint64(len(g.roots.values)) >= uint64(g.config.MaxSyntaxVisits) {
		return false
	}
	switch root.Kind {
	case rootSymbol:
		return g.validSymbol(root.Symbol)
	case rootSyntax:
		return g.validSyntax(root.Syntax)
	default:
		return false
	}
}

func (w *walker) commitPublication(term infer.Term, origin infer.Origin, root valueRoot) (typedValue, bool) {
	g := w.generation
	id, ok := g.addValue(generatedValue{Term: term, Origin: origin})
	if !ok {
		return typedValue{}, false
	}
	if !g.addRoot(id, root) {
		g.values = g.values[:len(g.values)-1]
		return typedValue{}, false
	}
	return typedValue{ID: id, Term: term}, true
}

func (w *walker) newSlotValue(term infer.Term, origin infer.Origin) (typedValue, bool) {
	g := w.generation
	if w.session == nil || w.session.Fatal() {
		return typedValue{Term: term}, false
	}
	if g == nil || g.state != generationMutable || uint64(len(g.values)) >= uint64(g.config.MaxSyntaxVisits) || uint64(len(g.roots.values)) >= uint64(g.config.MaxSyntaxVisits) {
		g.report("invalid or over-limit slot publication", origin.Span)
		return typedValue{Term: term}, false
	}

	id, ok := g.addValue(generatedValue{Term: term, Origin: origin})
	if !ok {
		return typedValue{Term: term}, false
	}
	if w.activeBranch != nil {
		value := typedValue{ID: id, Term: term}
		if !w.queueBranchRoot(value) {
			g.values = g.values[:len(g.values)-1]
			return typedValue{Term: term}, false
		}
		return value, true
	}
	slot, exists := w.publishedSlots[term]
	if !exists {
		slot = w.session.PublishSlot(term)
		if slot == (infer.SlotID{}) {
			g.values = g.values[:len(g.values)-1]
			if w.session.Fatal() {
				return typedValue{Term: term}, false
			}
			g.report("inference rejected slot publication", origin.Span)
			return typedValue{Term: term}, false
		}
	}
	root := valueRoot{Kind: rootSlot, Slot: slot}
	if !g.addRoot(id, root) {
		// The capacity and identity checks above make this unreachable without an
		// internal invariant violation. Do not attempt an unowned replacement.
		g.report("slot publication could not be rooted", origin.Span)
		return typedValue{ID: id, Term: term}, false
	}
	if !exists {
		w.publishedSlots[term] = slot
	}
	return typedValue{ID: id, Term: term}, true
}
