package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

// controlContext is the lexical control state threaded through the traversal.
// The frozen region arena remains the sole parent/depth/children authority; this
// carries only the enclosing region, the nearest candidate jump targets, and the
// enclosing switch subject needed to relate its case values.
type controlContext struct {
	region         controlID
	breakTarget    controlID
	continueTarget controlID
	subject        valueID
}

type controlEmission struct {
	kind             controlKind
	form             statementForm
	region           controlID
	target           controlID
	values           []controlValue
	composition      []structuralChild
	conditionPresent bool
	elsePresent      bool
	rangeInclusive   bool
	syntheticSyntax  bool
	iteratorSymbol   symbol.SymbolID
}

// retainControl appends one control record. Region-owning statements retain
// theirs on entry so that record allocation order is authored statement order
// within every region; leaf statements retain theirs once their values exist.
func (w *walker) retainControl(ref symbol.SyntaxRef, ctx walkContext, emission controlEmission) {
	if w.session == nil || w.session.Fatal() || emission.region == 0 {
		return
	}
	callable := ctx.callable
	if callable.Symbol == 0 {
		if value, ok := w.callableSymbol(callable.Syntax); ok {
			callable.Symbol = value.ID
		}
	}
	if callable.Syntax == (symbol.SyntaxRef{}) || !w.generation.validSyntax(callable.Syntax) ||
		callable.Symbol != 0 && !w.generation.validSymbol(callable.Symbol) {
		w.generation.report("control record has no valid owning callable", spanForRef(w.generation.inputs, ref))
		return
	}
	header := w.header(ref, ctx.genericOwner, false)
	record := controlRecord{
		Header: header, Kind: emission.kind, Region: emission.region, Target: emission.target,
		Callable: callable, StatementForm: emission.form, Values: emission.values,
		Composition:      emission.composition,
		ConditionPresent: emission.conditionPresent, ElsePresent: emission.elsePresent,
		RangeInclusive: emission.rangeInclusive, SyntheticSyntax: emission.syntheticSyntax,
		IteratorSymbol: emission.iteratorSymbol,
	}
	controls := []controlID{emission.region}
	if emission.target != 0 {
		controls = append(controls, emission.target)
	}
	w.addRecord(retainedRecord{Header: header, Controls: controls, Control: &record})
}

// beginCallableRegion allocates the single function-root region for one callable
// body and retains its owning controlFunction record. Every record below it
// carries the same callableRef.
func (w *walker) beginCallableRegion(ref symbol.SyntaxRef, items []walkItem, callable callableRef, bodyPresent bool) {
	region := controlID(0)
	if bodyPresent && w.session != nil && !w.session.Fatal() {
		region, _ = w.generation.addControl(0)
	}
	for index := range items {
		items[index].ctx.control = controlContext{region: region}
	}
	if region == 0 {
		return
	}
	owner := walkContext{callable: callable}
	if len(items) != 0 {
		owner.genericOwner = items[0].ctx.genericOwner
	}
	w.retainControl(ref, owner, controlEmission{kind: controlFunction, form: statementOther, region: region})
}

// enterRegion allocates a child region for a region-owning statement. A refused
// allocation is already reported as C0619; the statement then recovers as a leaf
// of its enclosing region instead of fabricating a second root.
func (w *walker) enterRegion(ctx walkContext) controlID {
	if ctx.control.region == 0 || w.session == nil || w.session.Fatal() {
		return 0
	}
	region, ok := w.generation.addControl(ctx.control.region)
	if !ok {
		return 0
	}
	return region
}

func (w *walker) stampRegion(items []walkItem, control controlContext) {
	for index := range items {
		items[index].ctx.control = control
	}
}

// reserveControlValue pins the value handle of an authored control operand
// before its subtree is visited, so the owning region record can be retained in
// authored order. The occurrence publishes and constrains itself as usual.
func (w *walker) reserveControlValue(ref symbol.SyntaxRef, ctx walkContext, role controlValueRole, ordinal uint32, purpose string) (controlValue, typedValue, bool) {
	if w.session == nil || w.session.Fatal() {
		return controlValue{}, typedValue{}, false
	}
	value := w.reserveExpression(ref, w.originForRef(ref, purpose, ctx.typeOwner, ctx.genericOwner))
	if value.ID == 0 || !w.generation.hasValue(value.ID) {
		return controlValue{}, typedValue{}, false
	}
	return controlValue{Role: role, Value: value.ID, Ordinal: ordinal, Syntax: ref}, value, true
}

func (w *walker) reserveCondition(ref symbol.SyntaxRef, ctx walkContext, purpose string) (controlValue, bool) {
	entry, value, ok := w.reserveControlValue(ref, ctx, valueCondition, 0, purpose)
	if !ok {
		return controlValue{}, false
	}
	w.addConstraint(infer.Equal(value.Term, w.session.Known(w.generation.inputs.Types.Builtins().Bool), w.originForRef(ref, purpose, ctx.typeOwner, ctx.genericOwner)))
	return entry, true
}

func (w *walker) prepareBlock(ref symbol.SyntaxRef, ctx walkContext, items []walkItem) []walkItem {
	region := w.enterRegion(ctx)
	if region == 0 {
		return items
	}
	control := ctx.control
	control.region = region
	w.stampRegion(items, control)
	w.retainControl(ref, ctx, controlEmission{kind: controlBlock, form: statementOther, region: region})
	return items
}

func (w *walker) prepareIf(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem) []walkItem {
	region := w.enterRegion(ctx)
	if region == 0 {
		return items
	}
	control := ctx.control
	control.region = region
	w.stampRegion(items, control)
	emission := controlEmission{
		kind: controlIf, form: statementOther, region: region,
		elsePresent: len(node.Children()) == 3,
		composition: expectedComposition(ref, node, tree),
	}
	if semantic := semanticRefs(ref.Module, node, tree); len(semantic) != 0 {
		emission.conditionPresent = true
		if entry, ok := w.reserveCondition(semantic[0], ctx, "if condition"); ok {
			emission.values = append(emission.values, entry)
		}
	}
	w.retainControl(ref, ctx, emission)
	return items
}

func (w *walker) loopConditionConstantCandidate(ref symbol.SyntaxRef) bool {
	if w == nil || w.evaluator == nil {
		return false
	}
	node, ok := w.evaluator.node(ref)
	if !ok {
		return false
	}
	switch node.Kind() {
	case syntax.Literal:
		return true
	case syntax.GroupedTerm, syntax.PrefixTerm, syntax.BinaryExpr:
		children := node.Children()
		if len(children) == 0 {
			return false
		}
		for _, child := range children {
			if !w.loopConditionConstantCandidate(symbol.SyntaxRef{Module: ref.Module, Node: child}) {
				return false
			}
		}
		return true
	case syntax.Name, syntax.Path, syntax.MemberExpr:
		id, ok := w.evaluator.referenceSymbol(ref, node)
		if !ok {
			return false
		}
		if _, initialized, _ := w.evaluator.bindingInitializer(id); !initialized {
			return false
		}
		return true
	}
	return false
}

func (w *walker) evaluateLoopCondition(ref symbol.SyntaxRef) {
	if w.loopConditionConstantCandidate(ref) {
		w.evaluator.evaluate(ref)
	}
}

func (w *walker) prepareWhile(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem) []walkItem {
	region := w.enterRegion(ctx)
	if region == 0 {
		return items
	}
	w.stampRegion(items, controlContext{region: region, breakTarget: region, continueTarget: region})
	emission := controlEmission{kind: controlWhile, form: statementOther, region: region, composition: expectedComposition(ref, node, tree)}
	if semantic := semanticRefs(ref.Module, node, tree); len(semantic) != 0 {
		emission.conditionPresent = true
		if entry, ok := w.reserveCondition(semantic[0], ctx, "while condition"); ok {
			emission.values = append(emission.values, entry)
			w.evaluateLoopCondition(semantic[0])
		}
	}
	w.retainControl(ref, ctx, emission)
	return items
}

func rangeLoopParts(ref symbol.SyntaxRef, node syntax.Node, tree *syntax.Tree) (start, end, iterator symbol.SyntaxRef) {
	children := node.Children()
	if len(children) < 3 {
		return
	}
	if child, ok := tree.Node(children[0]); ok && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
		start = symbol.SyntaxRef{Module: ref.Module, Node: children[0]}
	}
	if child, ok := tree.Node(children[1]); ok && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
		end = symbol.SyntaxRef{Module: ref.Module, Node: children[1]}
	}
	if len(children) >= 4 {
		if name, ok := tree.Node(children[2]); ok && name.Kind() == syntax.Name {
			iterator = symbol.SyntaxRef{Module: ref.Module, Node: children[2]}
		}
	}
	return
}

// rangeIteratorSymbol reaches the loop binding through the declaration index.
// 04b declares it on the range statement itself, never on the name occurrence,
// so no resolution reference exists for that name.
func (w *walker) rangeIteratorSymbol(ref symbol.SyntaxRef) (symbol.Symbol, bool) {
	for _, value := range w.symbolsAt[ref] {
		if value.Kind == symbol.SymbolLoopBinding && !value.Error {
			return value, true
		}
	}
	return symbol.Symbol{}, false
}

func (w *walker) prepareRangeLoop(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem) []walkItem {
	region := w.enterRegion(ctx)
	if region == 0 {
		return items
	}
	w.stampRegion(items, controlContext{region: region, breakTarget: region, continueTarget: region})
	start, end, iterator := rangeLoopParts(ref, node, tree)
	emission := controlEmission{
		kind: controlRangeLoop, form: statementOther, region: region,
		rangeInclusive: node.Token() == syntax.RangeInclusive,
		composition:    expectedComposition(ref, node, tree),
	}
	origin := w.originForRef(ref, "range bounds", ctx.typeOwner, ctx.genericOwner)
	var startValue typedValue
	startOK := false
	if start != (symbol.SyntaxRef{}) {
		var entry controlValue
		if entry, startValue, startOK = w.reserveControlValue(start, ctx, valueRangeStart, 0, "range start"); startOK {
			emission.values = append(emission.values, entry)
			w.addConstraint(infer.Integral(startValue.Term, origin))
		}
	}
	if end != (symbol.SyntaxRef{}) {
		if entry, endValue, ok := w.reserveControlValue(end, ctx, valueRangeEnd, 0, "range end"); ok {
			emission.values = append(emission.values, entry)
			w.addConstraint(infer.Integral(endValue.Term, origin))
			if startOK {
				w.addConstraint(infer.Equal(startValue.Term, endValue.Term, origin))
			}
		}
	}
	if iterator != (symbol.SyntaxRef{}) {
		for index := range items {
			if items[index].ref == iterator {
				items[index].ctx.suppressValue = true
			}
		}
		if binding, ok := w.rangeIteratorSymbol(ref); ok {
			bindingOrigin := w.originForRef(ref, "range iterator", binding.ID, ctx.genericOwner)
			bound, published := w.publishSymbol(binding.ID, w.symbolTerm(binding.ID, bindingOrigin), bindingOrigin)
			if published && bound.ID != 0 {
				emission.values = append(emission.values, controlValue{Role: valueRangeIterator, Value: bound.ID, Syntax: iterator})
				emission.iteratorSymbol = binding.ID
				if startOK {
					w.addConstraint(infer.Equal(bound.Term, startValue.Term, bindingOrigin))
				}
				header := w.header(ref, ctx.genericOwner, false)
				w.retainBinding(bindingRecord{Header: header, Symbol: binding.ID, Kind: bindingRangeIterator})
			}
		}
	}
	w.retainControl(ref, ctx, emission)
	return items
}

// forCondition reads the authored presence flags rather than guessing from the
// child shape. An omitted clause fabricates no syntax node and no publication.
func forCondition(ref symbol.SyntaxRef, node syntax.Node, tree *syntax.Tree) (symbol.SyntaxRef, bool) {
	if node.Data()&syntax.ForConditionPresent == 0 {
		return symbol.SyntaxRef{}, false
	}
	semantic := semanticRefs(ref.Module, node, tree)
	index := 0
	if node.Data()&syntax.ForInitializerPresent != 0 {
		index++
	}
	if index >= len(semantic) {
		return symbol.SyntaxRef{}, false
	}
	return semantic[index], true
}

func (w *walker) prepareFor(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem) []walkItem {
	region := w.enterRegion(ctx)
	if region == 0 {
		return items
	}
	w.stampRegion(items, controlContext{region: region, breakTarget: region, continueTarget: region})
	emission := controlEmission{kind: controlFor, form: statementOther, region: region, composition: expectedComposition(ref, node, tree)}
	if condition, present := forCondition(ref, node, tree); present {
		emission.conditionPresent = true
		if entry, ok := w.reserveCondition(condition, ctx, "for condition"); ok {
			emission.values = append(emission.values, entry)
			w.evaluateLoopCondition(condition)
		}
	}
	w.retainControl(ref, ctx, emission)
	return items
}

func switchSubject(ref symbol.SyntaxRef, node syntax.Node, tree *syntax.Tree) (symbol.SyntaxRef, bool) {
	children := node.Children()
	if len(children) == 0 {
		return symbol.SyntaxRef{}, false
	}
	child, ok := tree.Node(children[0])
	if !ok || child.Kind() == syntax.Missing || child.Kind() == syntax.Error || child.Kind() == syntax.SwitchCase {
		return symbol.SyntaxRef{}, false
	}
	return symbol.SyntaxRef{Module: ref.Module, Node: children[0]}, true
}

// switchElsePresent is true exactly when the parser emitted an else case, which
// is a SwitchCase child opened by the else keyword with no case expressions.
func switchElsePresent(node syntax.Node, tree *syntax.Tree) bool {
	for _, id := range node.Children() {
		child, ok := tree.Node(id)
		if ok && child.Kind() == syntax.SwitchCase && child.Token() == syntax.KwElse {
			return true
		}
	}
	return false
}

func (w *walker) prepareSwitch(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem) []walkItem {
	region := w.enterRegion(ctx)
	if region == 0 {
		return items
	}
	control := ctx.control
	control.region, control.breakTarget, control.subject = region, region, 0
	emission := controlEmission{
		kind: controlSwitch, form: statementOther, region: region,
		elsePresent: switchElsePresent(node, tree),
		composition: expectedComposition(ref, node, tree),
	}
	if subject, ok := switchSubject(ref, node, tree); ok {
		if entry, value, found := w.reserveControlValue(subject, ctx, valueSubject, 0, "switch subject"); found {
			emission.values = append(emission.values, entry)
			control.subject = value.ID
		}
	}
	w.stampRegion(items, control)
	w.retainControl(ref, ctx, emission)
	return items
}

// switchCaseValues returns the authored case expressions. The parser always
// appends the case body last, and the else case authors none.
func switchCaseValues(ref symbol.SyntaxRef, node syntax.Node, tree *syntax.Tree) []symbol.SyntaxRef {
	children := node.Children()
	if len(children) == 0 {
		return nil
	}
	var out []symbol.SyntaxRef
	for _, id := range children[:len(children)-1] {
		child, ok := tree.Node(id)
		if ok && child.Kind() != syntax.Missing && child.Kind() != syntax.Error {
			out = append(out, symbol.SyntaxRef{Module: ref.Module, Node: id})
		}
	}
	return out
}

// nominalCase decides the scalar/nominal split from syntax and 04b alone. The
// subject type is unsolved here, so no post-solve evidence may be consulted.
func (w *walker) nominalCase(ref symbol.SyntaxRef, tree *syntax.Tree) bool {
	node, ok := tree.Node(ref.Node)
	if !ok {
		return false
	}
	if node.Kind() == syntax.PartialMemberExpr {
		return true
	}
	_, kind, _, found := w.staticTarget(ref, tree)
	return found && kind == callVariant
}

func (w *walker) prepareSwitchCase(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext, tree *syntax.Tree, items []walkItem) []walkItem {
	region := w.enterRegion(ctx)
	if region == 0 {
		return items
	}
	control := ctx.control
	control.region = region
	w.stampRegion(items, control)
	emission := controlEmission{kind: controlSwitchCase, form: statementOther, region: region, composition: expectedComposition(ref, node, tree)}
	for ordinal, value := range switchCaseValues(ref, node, tree) {
		entry, reserved, ok := w.reserveControlValue(value, ctx, valueCase, uint32(ordinal), "switch case")
		if !ok {
			continue
		}
		emission.values = append(emission.values, entry)
		if w.nominalCase(value, tree) {
			// A nominal case retains subject, member identity, name, and span
			// through its member record. Selection and narrowing are post-solve.
			// A base-less .name shorthand has no self-describing base; its
			// member resolution depends on the subject's type, so wire the
			// subject as the expected shape destination. Qualified labels and
			// callVariant static targets already carry their own base and must
			// not be touched here.
			if node, ok := tree.Node(value.Node); ok && node.Kind() == syntax.PartialMemberExpr &&
				control.subject != 0 && w.generation.hasValue(control.subject) {
				if expected := w.expectationFor(value, control.subject, compatibilityBranch); expected.Kind != expectNone {
					w.expectations[value] = expected
				}
			}
			continue
		}
		// The shared memoized evaluator owns C0614 for every rejected form.
		w.switchCaseConstant(value)
		if control.subject == 0 || !w.generation.hasValue(control.subject) {
			continue
		}
		expected := w.expectationFor(value, control.subject, compatibilityBranch)
		if expected.Kind != expectNone {
			w.expectations[value] = expected
			continue
		}
		w.addConstraint(infer.Equal(reserved.Term, w.generation.values[control.subject-1].Term,
			w.originForRef(value, "switch case", ctx.typeOwner, ctx.genericOwner)))
	}
	w.retainControl(ref, ctx, emission)
	return items
}

// switchCaseConstant is the sole 06a.7 entry into the bounded constant
// evaluator. It reuses the existing memo and its C0614 reporting; no second
// evaluator and no reduced constant language exist.
func (w *walker) switchCaseConstant(ref symbol.SyntaxRef) (constantValue, bool) {
	if w == nil || w.evaluator == nil {
		return constantValue{}, false
	}
	result := w.evaluator.evaluate(ref)
	return result.Value, result.State == constantKnown
}

func (w *walker) finishJump(ref symbol.SyntaxRef, node syntax.Node, ctx walkContext) {
	kind, target := controlBreak, ctx.control.breakTarget
	if node.Kind() == syntax.ContinueStmt {
		// Continue ignores switches. A missing or illegal target is 06b's C0611.
		kind, target = controlContinue, ctx.control.continueTarget
	}
	w.retainControl(ref, ctx, controlEmission{
		kind: kind, form: statementOther, region: ctx.control.region, target: target,
	})
}
