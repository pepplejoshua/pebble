package check

import (
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

func (s *irBuildState) buildBlocks() bool {
	if s.byRegion == nil && !s.indexControls() {
		return false
	}
	controls := s.handoff.Records.Controls()
	for i := range controls {
		r, err := s.builder.AddRegion()
		if err != nil {
			return false
		}
		s.regions[controls[i].ID] = r
	}
	for _, c := range controls {
		if c.ID == 0 || s.regions[c.ID] == 0 {
			return false
		}
	}
	for _, decl := range s.functionDecls {
		region := s.functionRegions[decl.callable.Symbol]
		if region == 0 {
			continue
		}
		bodyRegion := region
		if uint64(region) <= uint64(len(s.handoff.Records.Controls())) {
			for _, child := range s.handoff.Records.Controls()[region-1].Children {
				if owner := s.owner[child]; owner != nil && owner.Kind == controlBlock {
					bodyRegion = child
					break
				}
			}
		}
		if _, buildable, unsupported := s.buildRegionBlock(bodyRegion, true); unsupported {
			return false
		} else if !buildable {
			return false
		}
	}
	return true
}

func (s *irBuildState) buildRegionBlock(region controlID, root bool) (tir.NodeID, bool, bool) {
	if existing := s.blockNodes[region]; existing != 0 {
		return existing, true, false
	}
	sequence := s.regionSequence(region)
	children := make([]tir.NodeID, 0, len(sequence))
	canFallthrough := true
	for _, ctrl := range sequence {
		if s.deferByStatement[ctrl.Header.Syntax] != nil {
			// A deferred statement is never a sequential statement: it runs only
			// when its defer fires, built once as that DeferRegister's child.
			continue
		}
		node, ok, unsupported, diverges := s.buildControlRecord(ctrl)
		if !ok {
			return 0, false, unsupported
		}
		children = append(children, node)
		canFallthrough = !diverges
	}
	if root && canFallthrough && isVoidCallable(s, s.owner[region]) {
		var chain []tir.NodeID
		if region != 0 {
			built, ok := s.deferChainFor(region, 0)
			if !ok {
				return 0, false, false
			}
			chain = built
		}
		implicit, ok := s.addNode(tir.Node{Kind: tir.ImplicitReturn, Origin: s.owner[region].Header.Span, SyntheticRole: "implicit-return", Function: s.functions[s.owner[region].Callable.Symbol], DeferChain: chain}, symbol.SyntaxRef{})
		if !ok {
			return 0, false, false
		}
		children = append(children, implicit)
	}
	node, ok := s.addNode(tir.Node{Kind: tir.Block, Span: s.owner[region].Header.Span, Region: s.regions[region], Children: children}, symbol.SyntaxRef{})
	if !ok {
		return 0, false, false
	}
	s.blockNodes[region] = node
	return node, true, false
}

func (s *irBuildState) regionSequence(region controlID) []*controlRecord {
	sequence := append([]*controlRecord(nil), s.byRegion[region]...)
	owner := s.owner[region]
	if owner == nil || (owner.Kind != controlBlock && owner.Kind != controlFunction) {
		return sequence
	}
	for i, ctrl := range sequence {
		if ctrl == owner {
			sequence = sequence[i+1:]
			break
		}
	}
	if uint64(region) <= uint64(len(s.handoff.Records.Controls())) {
		for _, child := range s.handoff.Records.Controls()[region-1].Children {
			if childOwner := s.owner[child]; childOwner != nil {
				seen := false
				for _, existing := range sequence {
					seen = seen || existing == childOwner
				}
				if !seen {
					sequence = append(sequence, childOwner)
				}
			}
		}
	}
	sort.SliceStable(sequence, func(i, j int) bool {
		return sequence[i].Header.Span.Start < sequence[j].Header.Span.Start
	})
	return sequence
}

// buildControlRecord dispatches one control record to its typed-IR node. It is
// the shared construction point for sequential statements and for every
// structural composition arm, mirroring validateControlFlow's recursive
// region/composition walk. The third return value reports a kind this part
// still cannot build (switches, defers, and temporaries are later 06b.7b
// parts); the fourth reports whether the built statement always diverges —
// its exit set has no fallthrough — which keeps root implicit-return insertion
// accurate.
func (s *irBuildState) buildControlRecord(ctrl *controlRecord) (tir.NodeID, bool, bool, bool) {
	if ctrl == nil {
		return 0, false, false, false
	}
	var node tir.NodeID
	var ok bool
	switch ctrl.Kind {
	case controlBlock:
		var unsupported bool
		node, ok, unsupported = s.buildRegionBlock(ctrl.Region, false)
		return node, ok, unsupported, false
	case controlBinding:
		binding := s.bindingForSyntax(ctrl.Header.Syntax)
		if binding == nil || !binding.InitializerPresent {
			return 0, false, false, false
		}
		value, valueOK := s.buildStatementValue(binding.Initializer)
		if !valueOK {
			return 0, false, false, false
		}
		node, ok = s.addNode(tir.Node{Kind: tir.Initialize, Span: ctrl.Header.Span, Symbol: binding.Symbol, Children: []tir.NodeID{value}}, symbol.SyntaxRef{})
	case controlExpression:
		if ctrl.StatementForm == statementAssignment {
			assignment := s.assignmentForSyntax(ctrl.Header.Syntax)
			if assignment == nil {
				return 0, false, false, false
			}
			place, placeOK := s.buildPlace(ctrl.Header.Syntax)
			if !placeOK {
				return 0, false, false, false
			}
			value, valueOK := s.buildStatementValue(assignment.Source)
			if !valueOK {
				return 0, false, false, false
			}
			if assignment.Kind == assignmentSimple {
				node, ok = s.addNode(tir.Node{Kind: tir.Store, Span: ctrl.Header.Span, Children: []tir.NodeID{place, value}}, ctrl.Header.Syntax)
			} else {
				_, operator := compoundOperator(assignment.Operator)
				node, ok = s.addNode(tir.Node{Kind: tir.CompoundStore, Span: ctrl.Header.Span, Operator: operator, Children: []tir.NodeID{place, value}}, ctrl.Header.Syntax)
			}
		} else if ctrl.StatementForm == statementPostfixUpdate {
			if len(ctrl.Values) != 1 {
				return 0, false, false, false
			}
			node, ok = s.buildPostfixUpdate(ctrl)
			if !ok {
				return 0, false, false, false
			}
		} else if len(ctrl.Values) == 1 {
			value, valueOK := s.buildValue(ctrl.Values[0].Value)
			if !valueOK {
				return 0, false, false, false
			}
			node, ok = s.addNode(tir.Node{Kind: tir.ExpressionStatement, Span: ctrl.Header.Span, Children: []tir.NodeID{value}}, ctrl.Header.Syntax)
		} else {
			return 0, false, false, false
		}
	case controlPrint:
		values := make([]tir.NodeID, 0, len(ctrl.Values))
		for _, entry := range ctrl.Values {
			value, valueOK := s.buildValue(entry.Value)
			if !valueOK {
				return 0, false, false, false
			}
			values = append(values, value)
		}
		node, ok = s.addNode(tir.Node{Kind: tir.Print, Span: ctrl.Header.Span, Children: values}, ctrl.Header.Syntax)
	case controlReturn:
		values := make([]tir.NodeID, 0, 1)
		if len(ctrl.Values) > 1 {
			return 0, false, false, false
		}
		if len(ctrl.Values) == 1 {
			value, valueOK := s.buildReturnValue(ctrl.Values[0].Value)
			if !valueOK {
				return 0, false, false, false
			}
			values = append(values, value)
		}
		chain, chainOK := s.deferChainFor(ctrl.Region, 0)
		if !chainOK {
			return 0, false, false, false
		}
		ref := ctrl.Header.Syntax
		if ctrl.SyntheticSyntax {
			// The synthesized expression-body return has no syntax node of its
			// own distinct from the body expression whose value it returns; that
			// expression already claimed its own ref via MapSource, so building
			// this node against the same ref would hit a duplicate source-map
			// entry. An empty ref (as ImplicitReturn uses) avoids the collision;
			// the span still comes from ctrl.Header.Span.
			ref = symbol.SyntaxRef{}
		}
		node, ok = s.addNode(tir.Node{Kind: tir.Return, Span: ctrl.Header.Span, Function: s.functions[ctrl.Callable.Symbol], Children: values, DeferChain: chain}, ref)
		return node, ok, false, true
	case controlIf:
		return s.buildIf(ctrl)
	case controlWhile:
		return s.buildWhile(ctrl)
	case controlRangeLoop:
		return s.buildRangeLoop(ctrl)
	case controlFor:
		return s.buildFor(ctrl)
	case controlSwitch:
		return s.buildSwitch(ctrl)
	case controlBreak:
		if ctrl.Target == 0 {
			return 0, false, false, false
		}
		target, mapped := s.regions[ctrl.Target]
		if !mapped || target == 0 {
			return 0, false, false, false
		}
		chain, chainOK := s.deferChainFor(ctrl.Region, ctrl.Target)
		if !chainOK {
			return 0, false, false, false
		}
		node, ok = s.addNode(tir.Node{Kind: tir.Break, Span: ctrl.Header.Span, Target: target, DeferChain: chain}, ctrl.Header.Syntax)
		return node, ok, false, true
	case controlContinue:
		if ctrl.Target == 0 {
			return 0, false, false, false
		}
		target, mapped := s.regions[ctrl.Target]
		if !mapped || target == 0 {
			return 0, false, false, false
		}
		chain, chainOK := s.deferChainFor(ctrl.Region, ctrl.Target)
		if !chainOK {
			return 0, false, false, false
		}
		node, ok = s.addNode(tir.Node{Kind: tir.Continue, Span: ctrl.Header.Span, Target: target, DeferChain: chain}, ctrl.Header.Syntax)
		return node, ok, false, true
	case controlDefer:
		node, ok, unsupported := s.buildDeferRegister(s.deferByHeader[ctrl.Header.Syntax])
		return node, ok, unsupported, false
	default:
		// controlSwitchCase is reached only by syntax through buildSwitchCase.
		return 0, false, true, false
	}
	return node, ok, false, false
}

// buildDeferRegister builds one DeferRegister node from a frozen deferRecord.
// The deferred statement's own control record is skipped from its region's
// sequence and built here instead as the register's single child. The register
// is memoized, so the same built statement node is shared by the containing
// block's ordered children and by every exit whose DeferChain crosses this
// region, exactly as lowering expects: it may expand a chain but never recompute
// lexical behavior.
func (s *irBuildState) buildDeferRegister(record *deferRecord) (tir.NodeID, bool, bool) {
	if record == nil || record.Header.Syntax == (symbol.SyntaxRef{}) {
		return 0, false, false
	}
	if existing := s.deferNodes[record.Header.Syntax]; existing != 0 {
		return existing, true, false
	}
	region, ok := s.regions[record.Region]
	if !ok || region == 0 {
		return 0, false, false
	}
	stmt := s.bySyntax[record.Statement]
	if stmt == nil {
		return 0, false, false
	}
	switch stmt.Kind {
	case controlReturn, controlBreak, controlContinue, controlDefer:
		// Deferred return/break/continue/nested defer are C0613 hard errors and
		// never survive into a generation-error-free handoff; reject defensively.
		return 0, false, false
	}
	stmtNode, ok, unsupported, _ := s.buildControlRecord(stmt)
	if !ok {
		return 0, false, unsupported
	}
	node, ok := s.addNode(tir.Node{Kind: tir.DeferRegister, Span: record.Header.Span, Region: region, Children: []tir.NodeID{stmtNode}}, record.Header.Syntax)
	if !ok {
		return 0, false, false
	}
	s.deferNodes[record.Header.Syntax] = node
	return node, true, false
}

// deferChainFor computes the exact ordered defer chain an exit crossing from
// source toward target runs: walking the frozen control-region Parent chain from
// source up, collecting every crossed region's registered defers in reverse
// registration order (innermost region first), and stopping at target without
// charging the target region itself. A zero target means the walk runs to the
// function root. This mirrors defer_validation.go's edge walk exactly so
// construction and validation attach the same defers to the same exits.
func (s *irBuildState) deferChainFor(source, target controlID) ([]tir.NodeID, bool) {
	controls := s.handoff.Records.Controls()
	var chain []tir.NodeID
	for current := source; current != 0; {
		if uint64(current) > uint64(len(controls)) {
			return nil, false
		}
		if current == target {
			break
		}
		defers := s.defersByRegion[current]
		for index := len(defers) - 1; index >= 0; index-- {
			node, ok, _ := s.buildDeferRegister(defers[index])
			if !ok {
				return nil, false
			}
			chain = append(chain, node)
		}
		current = controls[current-1].Parent
	}
	return chain, true
}

// buildPostfixUpdate builds one postfix ++/-- statement as a CompoundStore: the
// authored place is evaluated exactly once as the store's single place child and
// the increment operand is the exact literal one, so no temporary is required.
// The frozen schema has no dedicated postfix primitive, and the mutation's
// result value (the place's old value) is never produced as a value node because
// ++/-- are legal only as an expression statement or for-update (assignment is
// never an expression). The mutation operatorRecord correlates through the
// discarded statement value, exactly as assignment_validation.go correlates it.
func (s *irBuildState) buildPostfixUpdate(ctrl *controlRecord) (tir.NodeID, bool) {
	op := s.operatorsByResult[ctrl.Values[0].Value]
	if op == nil || op.Family != operatorMutation {
		return 0, false
	}
	place, placeOK := s.buildPlace(op.Header.Syntax)
	if !placeOK {
		return 0, false
	}
	one, oneOK := s.buildPostfixOne(op)
	if !oneOK {
		return 0, false
	}
	operator := syntax.Plus
	if op.Token == syntax.MinusMinus {
		operator = syntax.Minus
	}
	node, ok := s.addNode(tir.Node{Kind: tir.CompoundStore, Span: ctrl.Header.Span, Operator: operator, Children: []tir.NodeID{place, one}}, ctrl.Header.Syntax)
	return node, ok
}

// buildPostfixOne synthesizes the exact-literal-one operand of a postfix
// ++/-- statement. Its type is the mutated place's own type so CompoundStore's
// single read-modify-write primitive applies a unit increment/decrement without
// re-evaluating the authored place.
func (s *irBuildState) buildPostfixOne(op *operatorRecord) (tir.NodeID, bool) {
	if op == nil || len(op.Operands) == 0 {
		return 0, false
	}
	typ, ok := s.resolveType(op.Operands[0])
	if !ok || typ == 0 {
		return 0, false
	}
	kind := tir.IntegerLiteral
	literal := tir.Literal{Kind: tir.LiteralInteger, IntegerNum: "1", IntegerDen: "1"}
	if key, found := s.handoff.Semantics.Types().Key(typ); found {
		if builtin, isBuiltin := key.Builtin(); isBuiltin && isFloatBuiltin(builtin) {
			kind = tir.FloatLiteral
			literal = tir.Literal{Kind: tir.LiteralFloat, Float: "1.0"}
		}
	}
	node, ok := s.addNode(tir.Node{Kind: kind, Type: typ, Origin: op.Header.Span, SyntheticRole: "postfix-update-one", Literal: literal}, symbol.SyntaxRef{})
	return node, ok
}

// buildControlArm resolves one structural composition arm and builds its
// control record. Every arm is the owning record's own control record, exactly
// as validateControlFlow correlates it through bySyntax.
func (s *irBuildState) buildControlArm(child *structuralChild) (tir.NodeID, bool, bool) {
	if child == nil {
		return 0, false, false
	}
	arm := s.bySyntax[child.Arm]
	if arm == nil {
		return 0, false, false
	}
	node, ok, unsupported, _ := s.buildControlRecord(arm)
	return node, ok, unsupported
}

func (s *irBuildState) buildIf(ctrl *controlRecord) (tir.NodeID, bool, bool, bool) {
	condition, ok := controlValueForRole(ctrl, valueCondition)
	if !ok {
		return 0, false, false, false
	}
	conditionNode, ok := s.buildValue(condition)
	if !ok {
		return 0, false, false, false
	}
	children := []tir.NodeID{conditionNode}
	arms := 0
	for i := range ctrl.Composition {
		entry := &ctrl.Composition[i]
		if entry.Role != roleThen && entry.Role != roleElse {
			return 0, false, false, false
		}
		armNode, armOK, armUnsupported := s.buildControlArm(entry)
		if !armOK {
			return 0, false, armUnsupported, false
		}
		children = append(children, armNode)
		arms++
	}
	expected := 1
	if ctrl.ElsePresent {
		expected = 2
	}
	if arms != expected {
		return 0, false, false, false
	}
	region, ok := s.controlRegion(ctrl)
	if !ok {
		return 0, false, false, false
	}
	node, ok := s.addNode(tir.Node{Kind: tir.If, Span: ctrl.Header.Span, Region: region, HasElse: ctrl.ElsePresent, Children: children}, ctrl.Header.Syntax)
	return node, ok, false, false
}

func (s *irBuildState) buildWhile(ctrl *controlRecord) (tir.NodeID, bool, bool, bool) {
	condition, ok := controlValueForRole(ctrl, valueCondition)
	if !ok {
		return 0, false, false, false
	}
	conditionNode, ok := s.buildValue(condition)
	if !ok {
		return 0, false, false, false
	}
	body := compositionForRole(ctrl, roleBody)
	bodyNode, ok, unsupported := s.buildControlArm(body)
	if !ok {
		return 0, false, unsupported, false
	}
	region, ok := s.controlRegion(ctrl)
	if !ok {
		return 0, false, false, false
	}
	node, ok := s.addNode(tir.Node{Kind: tir.While, Span: ctrl.Header.Span, Region: region, Children: []tir.NodeID{conditionNode, bodyNode}}, ctrl.Header.Syntax)
	return node, ok, false, false
}

// buildRangeLoop builds a `loop start..end : name { body }` (or the bare,
// unbound `loop start..end { body }` form, which has no iterator symbol) as a
// tir.RangeLoop. The bound iterator's own symbol.SymbolID is attached
// directly as the node's Symbol field — this is the only place in typed IR
// that declaration exists. The general binding pass in ir_builder.go
// deliberately skips a retained bindingRangeIterator record (the same way it
// skips bindingParameter, which FunctionDeclaration attaches directly), so
// without this the iterator's declaration would be unrecoverable from typed
// IR entirely: nothing else names which symbol.SymbolID a SymbolValue inside
// the loop body that refers to the iterator actually is.
func (s *irBuildState) buildRangeLoop(ctrl *controlRecord) (tir.NodeID, bool, bool, bool) {
	start, startOK := controlValueForRole(ctrl, valueRangeStart)
	end, endOK := controlValueForRole(ctrl, valueRangeEnd)
	if !startOK || !endOK {
		return 0, false, false, false
	}
	startNode, ok := s.buildValue(start)
	if !ok {
		return 0, false, false, false
	}
	endNode, ok := s.buildValue(end)
	if !ok {
		return 0, false, false, false
	}
	body := compositionForRole(ctrl, roleBody)
	bodyNode, ok, unsupported := s.buildControlArm(body)
	if !ok {
		return 0, false, unsupported, false
	}
	region, ok := s.controlRegion(ctrl)
	if !ok {
		return 0, false, false, false
	}
	node, ok := s.addNode(tir.Node{Kind: tir.RangeLoop, Span: ctrl.Header.Span, Region: region, RangeInclusive: ctrl.RangeInclusive, Symbol: ctrl.IteratorSymbol, Children: []tir.NodeID{startNode, endNode, bodyNode}}, ctrl.Header.Syntax)
	return node, ok, false, false
}

func (s *irBuildState) buildFor(ctrl *controlRecord) (tir.NodeID, bool, bool, bool) {
	children := make([]tir.NodeID, 0, 4)
	if initializer := compositionForRole(ctrl, roleInitializer); initializer != nil {
		initNode, ok, unsupported := s.buildControlArm(initializer)
		if !ok {
			return 0, false, unsupported, false
		}
		children = append(children, initNode)
	}
	if ctrl.ConditionPresent {
		condition, ok := controlValueForRole(ctrl, valueCondition)
		if !ok {
			return 0, false, false, false
		}
		conditionNode, ok := s.buildValue(condition)
		if !ok {
			return 0, false, false, false
		}
		children = append(children, conditionNode)
	}
	if update := compositionForRole(ctrl, roleUpdate); update != nil {
		updateNode, ok, unsupported := s.buildControlArm(update)
		if !ok {
			return 0, false, unsupported, false
		}
		children = append(children, updateNode)
	}
	body := compositionForRole(ctrl, roleBody)
	bodyNode, ok, unsupported := s.buildControlArm(body)
	if !ok {
		return 0, false, unsupported, false
	}
	children = append(children, bodyNode)
	region, ok := s.controlRegion(ctrl)
	if !ok {
		return 0, false, false, false
	}
	node, ok := s.addNode(tir.Node{Kind: tir.For, Span: ctrl.Header.Span, Region: region, Children: children}, ctrl.Header.Syntax)
	return node, ok, false, false
}

func (s *irBuildState) buildSwitch(ctrl *controlRecord) (tir.NodeID, bool, bool, bool) {
	subject, ok := controlValueForRole(ctrl, valueSubject)
	if !ok {
		return 0, false, false, false
	}
	subjectNode, ok := s.buildValue(subject)
	if !ok {
		return 0, false, false, false
	}
	children := []tir.NodeID{subjectNode}
	for i := range ctrl.Composition {
		entry := &ctrl.Composition[i]
		if entry.Role != roleCase && entry.Role != roleElse {
			return 0, false, false, false
		}
		caseNodes, caseOK, unsupported := s.buildSwitchCase(entry)
		if !caseOK {
			return 0, false, unsupported, false
		}
		children = append(children, caseNodes...)
	}
	region, ok := s.controlRegion(ctrl)
	if !ok {
		return 0, false, false, false
	}
	node, ok := s.addNode(tir.Node{Kind: tir.Switch, Span: ctrl.Header.Span, Region: region, HasElse: ctrl.ElsePresent, Children: children}, ctrl.Header.Syntax)
	// An else-less switch contributes fallthrough exactly when it is not
	// exhaustive; an else-bearing switch covers the missing cases. This mirrors
	// validateControlFlow's exit-set rule and keeps the root implicit-return
	// synthesis from forcing a return after an exhaustive switch. As with If and
	// loops, case bodies that themselves fall through are treated conservatively:
	// the switch's own missing-else contribution is the part that matters for
	// exit-set correctness.
	diverges := ctrl.ElsePresent || switchIsExhaustive(s.handoff, s.records, ctrl, s.bySyntax)
	return node, ok, false, diverges
}

// buildSwitchCase resolves one roleCase/roleElse composition child of a switch
// to its controlSwitchCase control record and builds its SwitchCase node(s). A
// roleCase child with a single authored case value produces one SwitchCase node;
// a multi-value case produces one SwitchCase node per case value, all sharing the
// arm's one body block. A roleElse child produces a single HasElse SwitchCase
// node. Scalar case constants populate the node's Literal field; nominal
// variants populate CaseValue, correlating through the same variantBySyntax and
// records.Constant indexes the switch validators use.
func (s *irBuildState) buildSwitchCase(child *structuralChild) ([]tir.NodeID, bool, bool) {
	arm := s.bySyntax[child.Arm]
	if arm == nil || arm.Kind != controlSwitchCase {
		return nil, false, false
	}
	body := compositionForRole(arm, roleBody)
	bodyNode, ok, unsupported := s.buildControlArm(body)
	if !ok {
		return nil, false, unsupported
	}
	region, ok := s.controlRegion(arm)
	if !ok {
		return nil, false, false
	}
	if child.Role == roleElse {
		node, ok := s.addNode(tir.Node{Kind: tir.SwitchCase, Span: arm.Header.Span, Region: region, HasElse: true, Children: []tir.NodeID{bodyNode}}, arm.Header.Syntax)
		if !ok {
			return nil, false, false
		}
		return []tir.NodeID{node}, true, false
	}
	if len(arm.Values) == 0 {
		return nil, false, false
	}
	nodes := make([]tir.NodeID, 0, len(arm.Values))
	for _, entry := range arm.Values {
		if entry.Role != valueCase {
			return nil, false, false
		}
		node := tir.Node{Kind: tir.SwitchCase, Span: arm.Header.Span, Region: region, Children: []tir.NodeID{bodyNode}}
		ref := arm.Header.Syntax
		if len(nodes) != 0 {
			ref = symbol.SyntaxRef{}
		}
		if variant := s.variantBySyntax[entry.Syntax]; variant != 0 {
			node.CaseValue = variant
		} else {
			constResult, found := s.records.Constant(entry.Syntax)
			if !found || constResult.State != constantKnown {
				return nil, false, false
			}
			literal, ok := constantToLiteral(constResult.Value)
			if !ok {
				return nil, false, false
			}
			node.Literal = literal
		}
		nid, ok := s.addNode(node, ref)
		if !ok {
			return nil, false, false
		}
		nodes = append(nodes, nid)
	}
	return nodes, true, false
}

// constantToLiteral maps a frozen switch-case constant onto the closed SwitchCase
// Literal payload. Integer constants carry their canonical big.Int string with
// denominator one, exactly as buildLiteral does for authored integer literals.
func constantToLiteral(value constantValue) (tir.Literal, bool) {
	switch value.Kind {
	case constantBoolean:
		return tir.Literal{Kind: tir.LiteralBool, Bool: value.Boolean}, true
	case constantCharacter:
		return tir.Literal{Kind: tir.LiteralChar, Char: value.Character}, true
	case constantString:
		return tir.Literal{Kind: tir.LiteralString, String: value.String}, true
	case constantInteger:
		if value.Integer == nil {
			return tir.Literal{}, false
		}
		return tir.Literal{Kind: tir.LiteralInteger, IntegerNum: value.Integer.String(), IntegerDen: "1"}, true
	}
	return tir.Literal{}, false
}

func (s *irBuildState) controlRegion(ctrl *controlRecord) (tir.RegionID, bool) {
	for id, owner := range s.owner {
		if owner == ctrl {
			region, ok := s.regions[id]
			return region, ok && region != 0
		}
	}
	return 0, false
}

func controlValueForRole(ctrl *controlRecord, role controlValueRole) (valueID, bool) {
	if ctrl == nil {
		return 0, false
	}
	for _, entry := range ctrl.Values {
		if entry.Role == role {
			return entry.Value, true
		}
	}
	return 0, false
}

func compositionForRole(ctrl *controlRecord, role structuralRole) *structuralChild {
	if ctrl == nil {
		return nil
	}
	for i := range ctrl.Composition {
		if ctrl.Composition[i].Role == role {
			return &ctrl.Composition[i]
		}
	}
	return nil
}

func isVoidCallable(s *irBuildState, ctrl *controlRecord) bool {
	if ctrl == nil || ctrl.Callable.Symbol == 0 {
		return false
	}
	sig, ok := s.handoff.Semantics.Signature(ctrl.Callable.Symbol)
	if !ok {
		return false
	}
	template, ok := s.handoff.Semantics.Template(sig.Result)
	return ok && template.Kind == infer.TemplateKnown && template.Known == s.handoff.Semantics.Types().Builtins().Void
}
