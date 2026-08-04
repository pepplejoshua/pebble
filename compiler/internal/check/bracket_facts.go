package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

type indexMode uint8

const (
	indexValue indexMode = iota + 1
	indexSlice
)

type indexRecord struct {
	Header                   recordHeader
	Mode                     indexMode
	Base, Start, End, Result valueID
	StartPresent, EndPresent bool
	KnownArrayLength         uint64
	HasKnownArrayLength      bool
	EscapeDestination        symbol.SymbolID
	StartSyntax, EndSyntax   symbol.SyntaxRef
}

func (w *walker) evaluateIndexBound(ref symbol.SyntaxRef) {
	if w.evaluator == nil {
		return
	}
	budget := w.evaluator.budget
	w.evaluator.budget = newGenerationDiagnosticBudget(diagnostic.NewDiagnosticSet(), w.evaluator.config.MaxDiagnostics)
	w.evaluator.evaluate(ref)
	w.evaluator.budget = budget
}

type guardedBranchRoot struct {
	value valueID
	root  valueRoot
}

type bracketPlan struct {
	mode           symbol.BracketMode
	base           symbol.SyntaxRef
	arguments      []symbol.SyntaxRef
	result         typedValue
	genericResult  typedValue
	generic        symbol.SymbolID
	application    genericApplication
	choice         infer.ConstraintID
	capability     infer.ChoiceRef
	tag            alternativeTag
	deferredMember *memberPlan
	genericBranch  *branchFacts
	runtimeBranch  *branchFacts
	typeArgument   typedValue
	memberValue    typedValue
	deferChoice    bool
}

func (w *walker) prepareBracket(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) []walkItem {
	items := childItems(ref, node, ctx)
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
	mode, ok := w.generation.inputs.Resolution.Bracket(ref)
	if !ok {
		mode = symbol.BracketDeferred
	}
	p := &bracketPlan{mode: mode, base: semantic[0], arguments: semantic[1:]}
	for i := range items {
		items[i].ctx.expected = expectedType{}
	}
	p.generic, _ = w.genericIdentity(p.base, tree)
	switch mode {
	case symbol.BracketTypeNames:
		for i := range items {
			items[i].ctx.suppressValue = true
		}
		if p.generic != 0 {
			if w.activeBranch != nil && w.activeBranch.typeSibling != nil {
				if declaration, found := w.program.TypeDeclaration(p.generic); found && declaration.State == infer.DeclarationReady {
					w.mirrorTypeInstantiation(ref, p.generic, p.arguments, declaration.Parameters)
				} else {
					p.application, _ = w.prepareGeneric(ref, p.generic, p.arguments, ctx)
				}
			} else {
				p.application, _ = w.prepareGeneric(ref, p.generic, p.arguments, ctx)
			}
		}
	case symbol.BracketValueNames:
		p.result = w.reserveExpression(ref, w.origin(ref, node, "bracket result", ctx.typeOwner, ctx.genericOwner))
	case symbol.BracketDeferred:
		w.prepareDeferredBracket(ref, node, ctx, tree, items, p)
	}
	w.bracketPlans[ref] = p
	return items
}

func (w *walker) genericIdentity(ref symbol.SyntaxRef, tree *syntax.Tree) (symbol.SymbolID, bool) {
	node, ok := tree.Node(ref.Node)
	if !ok {
		return 0, false
	}
	query := ref
	if node.Kind() == syntax.Path || node.Kind() == syntax.MemberExpr {
		c := node.Children()
		if len(c) > 0 {
			query.Node = c[len(c)-1]
		}
	}
	r, ok := w.generation.inputs.Resolution.Reference(query)
	if !ok || r.State != symbol.ResolutionResolved {
		return 0, false
	}
	s, ok := w.generation.inputs.Resolution.Symbols.Symbol(r.Symbol)
	return r.Symbol, ok && s.Generic
}

func (w *walker) prepareDeferredBracket(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem, p *bracketPlan) {
	if len(p.arguments) != 1 {
		return
	}
	baseNode, ok := tree.Node(p.base.Node)
	if !ok || baseNode.Kind() != syntax.MemberExpr {
		return
	}
	parts := baseNode.Children()
	if len(parts) < 2 {
		return
	}
	receiverRef := symbol.SyntaxRef{Module: ref.Module, Node: parts[0]}
	nameRef := symbol.SyntaxRef{Module: ref.Module, Node: parts[1]}
	nameNode, _ := tree.Node(parts[1])
	member := &memberPlan{base: receiverRef, name: nameRef, nameText: string(w.copySource(nameNode.Span())), nameSpan: nameNode.Span(), kind: memberField}
	p.deferredMember = member
	receiver := w.reserveExpression(receiverRef, w.originForRef(receiverRef, "deferred member receiver", ctx.typeOwner, ctx.genericOwner))
	p.genericBranch = &branchFacts{index: 0}
	p.runtimeBranch = &branchFacts{index: 1, typeSibling: p.genericBranch, typeOwner: ctx.typeOwner, genericOwner: ctx.genericOwner}
	common := w.reserveExpression(ref, w.origin(ref, node, "deferred bracket result", ctx.typeOwner, ctx.genericOwner))
	p.result = common
	p.genericResult = w.newValue(common.Term, w.origin(ref, node, "deferred generic result", ctx.typeOwner, ctx.genericOwner))
	p.memberValue = w.newValue(w.session.Variable(w.originForRef(p.base, "deferred runtime member", ctx.typeOwner, ctx.genericOwner)), w.originForRef(p.base, "deferred runtime member", ctx.typeOwner, ctx.genericOwner))
	p.typeArgument = w.newValue(w.session.Variable(w.originForRef(p.arguments[0], "deferred type argument", ctx.typeOwner, ctx.genericOwner)), w.originForRef(p.arguments[0], "deferred type argument", ctx.typeOwner, ctx.genericOwner))
	w.queueRootFor(p.genericBranch, p.typeArgument)
	w.queueRootFor(p.genericBranch, p.genericResult)
	w.queueRootFor(p.runtimeBranch, p.memberValue)
	w.queueRootFor(p.runtimeBranch, p.result)
	p.runtimeBranch.constraints = append(p.runtimeBranch.constraints, infer.ValueOccurrence(p.arguments[0], w.originForRef(p.arguments[0], "runtime bracket argument", ctx.typeOwner, ctx.genericOwner)))
	methodSite := ref
	if ctx.immediateCall && ctx.callSite != (symbol.SyntaxRef{}) {
		methodSite, p.deferChoice = ctx.callSite, true
	}
	p.genericBranch.constraints = append(p.genericBranch.constraints,
		infer.TypeOccurrence(p.arguments[0], ctx.typeOwner, p.typeArgument.Term, w.originForRef(p.arguments[0], "generic bracket argument", ctx.typeOwner, ctx.genericOwner)),
		infer.SelectMethod(receiver.Term, member.nameText, p.genericResult.Term, []infer.Term{p.typeArgument.Term}, methodSite, w.origin(ref, node, "generic member application", ctx.typeOwner, ctx.genericOwner)),
	)
	typeHeader := w.header(p.arguments[0], ctx.genericOwner, false)
	typeUse := typeUseRecord{Header: typeHeader, Kind: typeUseExplicitArgument, Type: p.typeArgument.ID}
	p.genericBranch.records = append(p.genericBranch.records, pendingBranchRecord{local: recordID(len(p.genericBranch.records) + 1), value: retainedRecord{Header: typeHeader, TypeUse: &typeUse}})
	for i := range items {
		if items[i].ref == p.base {
			items[i].ctx.deferredMember = true
		} else if items[i].ref == p.arguments[0] {
			items[i].ctx.branch = p.runtimeBranch
		}
	}
}

func (w *walker) queueRootFor(branch *branchFacts, value typedValue) {
	previous := w.activeBranch
	w.activeBranch = branch
	w.queueBranchRoot(value)
	w.activeBranch = previous
}

func (w *walker) finishBracket(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree) {
	if ctx.suppressValue {
		return
	}
	p := w.bracketPlans[ref]
	if p == nil {
		return
	}
	origin := w.origin(ref, node, "bracket application", ctx.typeOwner, ctx.genericOwner)
	if p.mode == symbol.BracketTypeNames {
		s, ok := w.generation.inputs.Resolution.Symbols.Symbol(p.generic)
		if !ok || (s.Kind != symbol.SymbolFunction && s.Kind != symbol.SymbolExternFunction) {
			w.failExpression(ref, origin)
			return
		}
		signature, ok := w.program.Signature(p.generic)
		if !ok {
			return
		}
		callable, _, _ := w.instantiateSignature(signature, p.application, origin)
		result := w.expressionResult(ref, callable.Term, origin)
		w.retainBracket(ref, ctx, result, 0, alternativeTag{}, nil)
		return
	}
	if p.mode == symbol.BracketDeferred {
		w.finishDeferredBracket(ref, ctx, p)
		return
	}
	if len(p.arguments) != 1 {
		return
	}
	base, index := w.valuesBySyntax[p.base], w.valuesBySyntax[p.arguments[0]]
	if base.ID == 0 || index.ID == 0 || !w.successfulExpressions[p.base] || !w.successfulExpressions[p.arguments[0]] {
		w.failExpression(ref, origin)
		return
	}
	w.addConstraint(infer.Integral(index.Term, origin))
	w.addConstraint(infer.Indexable(base.Term, p.result.Term, origin))
	if w.session.Fatal() {
		return
	}
	result, ok := w.publishExistingSyntax(ref, p.result, origin)
	if w.session.Fatal() {
		return
	}
	if !ok {
		return
	}
	header := w.header(ref, ctx.genericOwner, false)
	w.evaluateIndexBound(p.arguments[0])
	record := indexRecord{Header: header, Mode: indexValue, Base: base.ID, Start: index.ID, Result: result.ID, StartPresent: true, StartSyntax: p.arguments[0]}
	w.applyKnownArrayLength(&record, p.base)
	specialized, _ := w.addRecord(retainedRecord{Header: header, Index: &record})
	w.retainRequirement(header, requirementUnsupportedIndex, base.ID)
	w.deriveIndexPlace(ref, p.base, base, index, result)
	w.retainBracket(ref, ctx, result, specialized, alternativeTag{}, []valueID{base.ID, index.ID})
	_ = tree
}
func (w *walker) finishDeferredBracket(ref symbol.SyntaxRef, ctx walkContext, p *bracketPlan) {
	if p.deferredMember == nil || p.genericBranch == nil || p.runtimeBranch == nil {
		return
	}
	receiver := w.valuesBySyntax[p.deferredMember.base]
	argument := w.valuesBySyntax[p.arguments[0]]
	if receiver.ID == 0 || argument.ID == 0 {
		return
	}
	p.runtimeBranch.constraints = append(p.runtimeBranch.constraints,
		infer.HasField(receiver.Term, p.deferredMember.nameText, p.memberValue.Term, w.originForRef(p.base, "runtime member", ctx.typeOwner, ctx.genericOwner)),
		infer.Integral(argument.Term, w.originForRef(p.arguments[0], "runtime index", ctx.typeOwner, ctx.genericOwner)),
		infer.Indexable(p.memberValue.Term, p.result.Term, w.originForRef(ref, "runtime index", ctx.typeOwner, ctx.genericOwner)),
	)
	if ctx.expected.Destination != 0 && w.generation.hasValue(ctx.expected.Destination) {
		destination := w.generation.values[ctx.expected.Destination-1].Term
		p.genericBranch.constraints = append(p.genericBranch.constraints, infer.Equal(p.genericResult.Term, destination, w.originForRef(ref, "generic bracket expected result", ctx.typeOwner, ctx.genericOwner)))
		p.runtimeBranch.constraints = append(p.runtimeBranch.constraints, infer.Equal(p.result.Term, destination, w.originForRef(ref, "runtime bracket expected result", ctx.typeOwner, ctx.genericOwner)))
	}
	header := w.header(ref, ctx.genericOwner, false)
	previous := w.activeBranch
	w.activeBranch = p.genericBranch
	genericMember := memberRecord{Header: header, Kind: memberMethod, Base: receiver.ID, Result: p.genericResult.ID, Name: p.deferredMember.nameText, NameSpan: p.deferredMember.nameSpan}
	genericSpecialized, _ := w.addRecord(retainedRecord{Header: header, Member: &genericMember})
	w.retainRequirement(header, requirementUnsupportedMethod, receiver.ID)
	w.retainBracket(ref, ctx, p.genericResult, genericSpecialized, alternativeTag{}, []valueID{receiver.ID})
	w.retainDeferredResultCompatibility(ref, ctx, p.genericResult)
	w.activeBranch = p.runtimeBranch
	memberHeader := w.header(p.base, ctx.genericOwner, false)
	runtimeMember := memberRecord{Header: memberHeader, Kind: memberField, Base: receiver.ID, Result: p.memberValue.ID, Name: p.deferredMember.nameText, NameSpan: p.deferredMember.nameSpan}
	runtimeMemberSpecialized, _ := w.addRecord(retainedRecord{Header: memberHeader, Member: &runtimeMember})
	w.retainRequirement(memberHeader, requirementUnsupportedField, receiver.ID)
	w.addRecord(retainedRecord{Header: memberHeader, Expression: &expressionRecord{Header: memberHeader, Kind: expressionMember, Result: p.memberValue.ID, Children: []valueID{receiver.ID}, Specialized: runtimeMemberSpecialized}})
	w.evaluateIndexBound(p.arguments[0])
	record := indexRecord{Header: header, Mode: indexValue, Base: p.memberValue.ID, Start: argument.ID, Result: p.result.ID, StartPresent: true, StartSyntax: p.arguments[0]}
	specialized, _ := w.addRecord(retainedRecord{Header: header, Index: &record})
	w.retainRequirement(header, requirementUnsupportedIndex, p.memberValue.ID)
	w.retainBracket(ref, ctx, p.result, specialized, alternativeTag{}, []valueID{p.memberValue.ID, argument.ID})
	w.retainDeferredResultCompatibility(ref, ctx, p.result)
	w.activeBranch = previous
	if !p.deferChoice {
		w.successfulExpressions[ref] = w.finalizeDeferredChoice(ref, ctx, p)
		if w.successfulExpressions[ref] {
			w.deriveProjectionPlace(p.base, p.deferredMember.base, p.memberValue.ID, placeProjection{Kind: placeField, Base: receiver.ID})
			w.deriveIndexPlace(ref, p.base, p.memberValue, argument, p.result)
			if candidate, ok := w.places[ref]; ok {
				candidate.alternative = p.tag
				w.places[ref] = candidate
			}
		}
	} else {
		// The enclosing call completes and publishes the shared choice.
		w.successfulExpressions[ref] = true
	}
}

func (w *walker) retainDeferredResultCompatibility(ref symbol.SyntaxRef, ctx walkContext, result typedValue) {
	if ctx.expected.Destination == 0 || !w.generation.hasValue(ctx.expected.Destination) {
		return
	}
	w.retainCompatibility(ref, ctx.genericOwner, result.ID, ctx.expected.Destination, ctx.expected.Role, 0, 0, spanForRef(w.generation.inputs, ref), false)
}

func (w *walker) finalizeDeferredChoice(ref symbol.SyntaxRef, ctx walkContext, p *bracketPlan) bool {
	if p.choice != 0 {
		return true
	}
	node, ok := w.node(ref.Module, ref.Node)
	if !ok || p.genericBranch == nil || p.runtimeBranch == nil {
		return false
	}
	branches := []*branchFacts{p.genericBranch, p.runtimeBranch}
	if w.session.Fatal() || !w.preflightBranches(branches) {
		return false
	}
	choice, capability := w.session.AddChoice(infer.OneOf([]infer.Alternative{
		{Label: "generic application", Constraints: p.genericBranch.constraints},
		{Label: "runtime index", Constraints: p.runtimeBranch.constraints},
	}, w.origin(ref, node, "neutral bracket", ctx.typeOwner, ctx.genericOwner)))
	if choice == 0 || w.session.Fatal() {
		return false
	}
	var roots []guardedBranchRoot
	for _, branch := range branches {
		tag := alternativeTag{Choice: choice, Index: branch.index, Guarded: true}
		for _, pending := range branch.roots {
			slot := w.session.PublishGuardedSlot(capability, branch.index, pending.value.Term)
			if slot == (infer.SlotID{}) || w.session.Fatal() {
				return false
			}
			roots = append(roots, guardedBranchRoot{value: pending.value.ID, root: valueRoot{Kind: rootSlot, Slot: slot, Alternative: tag}})
		}
		for _, instantiation := range branch.instantiations {
			w.session.PublishGuardedInstantiation(capability, branch.index, instantiation.site, instantiation.generic, instantiation.arguments)
			if w.session.Fatal() {
				return false
			}
		}
	}

	records, ok := w.materializeBranchRecords(branches, choice)
	if !ok || !w.commitBranches(branches, roots, records) {
		return false
	}
	p.choice, p.capability = choice, capability
	p.tag = alternativeTag{Choice: choice, Index: 1, Guarded: true}
	return true
}

func (w *walker) preflightBranches(branches []*branchFacts) bool {
	g := w.generation
	if g == nil || g.state != generationMutable || w.session.Fatal() {
		return false
	}
	if len(branches) != 2 || branches[0] == nil || branches[1] == nil || branches[0].index != 0 || branches[1].index != 1 {
		g.report("invalid or unordered deferred-branch identities", source.Span{})
		return false
	}
	var requirements, roots uint64
	seenRoots := make(map[valueID]bool)
	for _, branch := range branches {
		requirements += uint64(branch.requirements)
		roots += uint64(len(branch.roots))
		for _, pending := range branch.roots {
			span := source.Span{}
			if g.hasValue(pending.value.ID) {
				span = g.values[pending.value.ID-1].Origin.Span
			}
			if pending.value.ID == 0 || !g.hasValue(pending.value.ID) || g.values[pending.value.ID-1].Term != pending.value.Term || seenRoots[pending.value.ID] {
				g.report("invalid or duplicate deferred-branch root", span)
				return false
			}
			if _, exists := g.roots.root(pending.value.ID); exists {
				g.report("deferred-branch value is already rooted", span)
				return false
			}
			seenRoots[pending.value.ID] = true
		}
		for _, instantiation := range branch.instantiations {
			if !g.validSyntax(instantiation.site) || !g.validSymbol(instantiation.generic) {
				g.report("invalid deferred-branch instantiation", spanForRef(g.inputs, instantiation.site))
				return false
			}
			for _, term := range instantiation.arguments {
				if term == (infer.Term{}) {
					g.report("invalid deferred-branch instantiation argument", spanForRef(g.inputs, instantiation.site))
					return false
				}
			}
			// Term intentionally has no public ownership query. Branch facts are
			// package-private and every production insertion copies terms created
			// by this walker's Session (prepareGeneric/mirrorTypeInstantiation).
			// PublishGuardedInstantiation remains the authoritative ownership
			// check and Fatal() prevents checker commit if that invariant is ever
			// violated by damaged internal state.
		}
	}
	if uint64(g.counters.genericRequirements) > uint64(g.config.MaxGenericRequirements) || requirements > uint64(g.config.MaxGenericRequirements)-uint64(g.counters.genericRequirements) {
		g.reportLimit("generic requirement", uint64(g.config.MaxGenericRequirements))
		return false
	}
	if uint64(len(g.roots.values)) > uint64(g.config.MaxSyntaxVisits) || roots > uint64(g.config.MaxSyntaxVisits)-uint64(len(g.roots.values)) {
		g.report("invalid, duplicate, foreign, or over-limit value root", source.Span{})
		return false
	}

	// Run the complete record validator against an independent arena. Choice 1
	// is a validation-only nonzero tag; actual identities are installed later.
	test := *g
	test.values = append([]generatedValue(nil), g.values...)
	test.roots.values = cloneRootedValues(g.roots.values)
	test.roots.byValue = make(map[valueID]int, len(g.roots.byValue))
	for id, index := range g.roots.byValue {
		test.roots.byValue[id] = index
	}
	test.records.values = make([]retainedRecord, len(g.records.values))
	for index := range g.records.values {
		test.records.values[index] = cloneRetainedRecord(g.records.values[index])
	}
	records, ok := w.materializeBranchRecordsFrom(branches, infer.ConstraintID(1), uint64(len(test.records.values)))
	if !ok {
		g.report("invalid deferred-branch record join", source.Span{})
		return false
	}
	for _, value := range records {
		if _, ok := test.addRecord(value); !ok {
			return false
		}
	}
	return true
}

func (w *walker) materializeBranchRecords(branches []*branchFacts, choice infer.ConstraintID) ([]retainedRecord, bool) {
	return w.materializeBranchRecordsFrom(branches, choice, uint64(len(w.generation.records.values)))
}

func (w *walker) materializeBranchRecordsFrom(branches []*branchFacts, choice infer.ConstraintID, base uint64) ([]retainedRecord, bool) {
	var result []retainedRecord
	for _, branch := range branches {
		tag := alternativeTag{Choice: choice, Index: branch.index, Guarded: true}
		branchBase := base + uint64(len(result))
		for index, pending := range branch.records {
			if pending.local != recordID(index+1) {
				return nil, false
			}
			value := cloneRetainedRecord(pending.value)
			header := value.Header
			header.Alternative = tag
			value.assignHeader(header)
			if value.Expression != nil && value.Expression.Specialized != 0 {
				local := uint64(value.Expression.Specialized)
				if local == 0 || local > uint64(index) || branchBase > uint64(^uint32(0))-local {
					return nil, false
				}
				value.Expression.Specialized = recordID(branchBase + local)
			}
			result = append(result, value)
		}
	}
	return result, true
}

func (w *walker) commitBranches(branches []*branchFacts, roots []guardedBranchRoot, records []retainedRecord) bool {
	g := w.generation
	beforeRequirements := g.counters.genericRequirements
	beforeRoots := len(g.roots.values)
	beforeRecords := len(g.records.values)
	beforeComponents := g.records.components
	rollback := func() {
		for _, rooted := range g.roots.values[beforeRoots:] {
			delete(g.roots.byValue, rooted.Value)
			if g.hasValue(rooted.Value) {
				g.values[rooted.Value-1].Root = valueRoot{}
			}
		}
		g.roots.values = g.roots.values[:beforeRoots]
		g.records.values = g.records.values[:beforeRecords]
		g.records.components = beforeComponents
		g.counters.genericRequirements = beforeRequirements
	}
	for _, branch := range branches {
		for count := uint32(0); count < branch.requirements; count++ {
			if !g.addGenericRequirement() {
				rollback()
				return false
			}
		}
	}
	for _, pending := range roots {
		if !g.addRoot(pending.value, pending.root) {
			rollback()
			return false
		}
	}
	for _, value := range records {
		if _, ok := g.addRecord(value); !ok {
			rollback()
			return false
		}
	}
	return true
}
func (w *walker) retainBracket(ref symbol.SyntaxRef, ctx walkContext, result typedValue, specialized recordID, tag alternativeTag, children []valueID) {
	if result.ID == 0 {
		return
	}
	w.successfulExpressions[ref] = true
	header := w.header(ref, ctx.genericOwner, false)
	header.Alternative = tag
	record := expressionRecord{Header: header, Kind: expressionBracket, Result: result.ID, Children: append([]valueID(nil), children...), Specialized: specialized}
	w.addRecord(retainedRecord{Header: header, Expression: &record})
}
