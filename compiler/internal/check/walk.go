package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type walkContext struct {
	callable     callableRef
	typeOwner    symbol.SymbolID
	genericOwner symbol.SymbolID
	nominalOwner symbol.SymbolID
	unsupported  bool
	typePosition bool
	typeRoot     bool
	preparedType bool
	controlDepth uint32
}

type walkItem struct {
	ref  symbol.SyntaxRef
	ctx  walkContext
	exit bool
}

type walker struct {
	generation       *generation
	evaluator        *constantEvaluator
	program          *infer.Program
	session          *infer.Session
	visited          map[symbol.SyntaxRef]bool
	order            []symbol.SyntaxRef
	symbolsAt        map[symbol.SyntaxRef][]symbol.Symbol
	resolvedTypes    map[symbol.SyntaxRef]bool
	preparedTypes    map[symbol.SyntaxRef]bool
	valuesBySyntax   map[symbol.SyntaxRef]typedValue
	publishedSymbols map[symbol.SymbolID]bool
	publishedSyntax  map[symbol.SyntaxRef]bool
	publishedSlots   map[infer.Term]infer.SlotID
	runtimeTypes     func() (infer.RuntimeTypes, bool)
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
		publishedSyntax: make(map[symbol.SyntaxRef]bool), publishedSlots: make(map[infer.Term]infer.SlotID),
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
		node, ok := item.Tree.Node(current.ref.Node)
		if !ok {
			w.generation.report("traversal reached an invalid syntax node", spanForRef(w.generation.inputs, current.ref))
			w.generation.leaveTraversal()
			continue
		}
		children := w.dispatch(current.ref, node, current.ctx, item.Tree)
		stack = append(stack, walkItem{exit: true})
		for index := len(children) - 1; index >= 0; index-- {
			stack = append(stack, children[index])
		}
	}
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
		syntax.AssignmentStmt, syntax.ExpressionStmt, syntax.Name, syntax.Path,
		syntax.Literal, syntax.InterpolatedString, syntax.SomeExpr, syntax.SizeofExpr, syntax.PrefixTerm,
		syntax.PostfixExpr, syntax.BinaryExpr, syntax.CallExpr, syntax.BracketApply,
		syntax.SliceExpr, syntax.MemberExpr, syntax.CastExpr, syntax.GroupedTerm, syntax.TupleTerm,
		syntax.ArrayExpr, syntax.ArrayRepeatExpr, syntax.RecordExpr, syntax.RecordField,
		syntax.PartialMemberExpr, syntax.StructType, syntax.UnionType, syntax.EnumType,
		syntax.FieldDecl, syntax.VariantDecl, syntax.Parameter, syntax.TypeParameter:
		return w.structuralChildren(ref, node, ctx, tree)
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
		for index := range items {
			items[index].ctx.callable = callableRef{Syntax: ref}
			items[index].ctx.unsupported = unsupported
			items[index].ctx.controlDepth = 0
		}
		return items
	default:
		w.generation.report("unknown syntax node kind in closed dispatch", node.Span())
		return childItems(ref, node, ctx)
	}
}

func (w *walker) structuralChildren(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
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
	_, _, resultNode, _ := functionParts(tree, node)
	for index := range items {
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
	root := valueRoot{Kind: rootSymbol, Symbol: id}
	if id == 0 || w.publishedSymbols[id] || !w.canPublish(root) {
		w.generation.report("invalid, duplicate, or over-limit symbol publication", origin.Span)
		return typedValue{}, false
	}
	value, ok := w.commitPublication(term, origin, root)
	if !ok {
		return typedValue{}, false
	}
	w.session.PublishSymbol(id, term)
	w.publishedSymbols[id] = true
	return value, true
}

func (w *walker) publishSyntax(ref symbol.SyntaxRef, term infer.Term, origin infer.Origin) (typedValue, bool) {
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
	w.publishedSyntax[ref] = true
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
	if g == nil || g.state != generationMutable || uint64(len(g.values)) >= uint64(g.config.MaxSyntaxVisits) || uint64(len(g.roots.values)) >= uint64(g.config.MaxSyntaxVisits) {
		g.report("invalid or over-limit slot publication", origin.Span)
		return typedValue{Term: term}, false
	}

	id, ok := g.addValue(generatedValue{Term: term, Origin: origin})
	if !ok {
		return typedValue{Term: term}, false
	}
	slot, exists := w.publishedSlots[term]
	if !exists {
		slot = w.session.PublishSlot(term)
		if slot == (infer.SlotID{}) {
			g.values = g.values[:len(g.values)-1]
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
