package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

// auditFrozen performs the additional freeze-audit checks beyond generation.freeze().
func auditFrozen(gen *generation, frozen frozenGeneration, compilation frozenCompilation) bool {
	reporter := &auditReporter{gen: gen}

	// Check 1: Every root names a valueID that exists in frozen.values.
	// Built once as a set so this is O(values + roots), not O(roots x values).
	existingValues := make(map[valueID]struct{}, len(frozen.values))
	for _, val := range frozen.values {
		existingValues[val.ID] = struct{}{}
	}
	for _, rooted := range frozen.roots.All() {
		if _, found := existingValues[rooted.Value]; !found {
			reporter.error("root references nonexistent value")
			return false
		}

		// Check 2: Every alternativeTag on a root has a valid Choice if Guarded.
		if !rooted.Root.Alternative.valid() {
			reporter.error("root has invalid alternative tag")
			return false
		}
	}

	controls := frozen.records.Controls()
	records := frozen.records.Records()

	// Check 3: Every control/defer record's Region and Target exist.
	for _, record := range records {
		if record.Control != nil {
			ctrl := record.Control
			if !controlIDExists(controls, ctrl.Region) {
				reporter.error("control record references nonexistent region")
				return false
			}
			if ctrl.Target != 0 && !controlIDExists(controls, ctrl.Target) {
				reporter.error("control record references nonexistent target")
				return false
			}
		}
		if record.Defer != nil {
			deferRec := record.Defer
			if !controlIDExists(controls, deferRec.Region) {
				reporter.error("defer record references nonexistent region")
				return false
			}
		}

		// Check 2b: Every record with Guarded == true has a valid alternativeTag.
		if record.Header.Alternative.Guarded && !record.Header.Alternative.valid() {
			reporter.error("guarded record has invalid alternative tag")
			return false
		}
	}

	// Check 4: Exactly one controlFunction record per function root; identical callableRef.
	if !auditControlHierarchy(controls, records, reporter) {
		return false
	}

	// Check 4b: every region-owning control record's structural composition is
	// exact and resolves correctly (invariant 9), and every controlBinding
	// record satisfies invariant 10.
	if !auditStructuralComposition(gen, controls, records, reporter) {
		return false
	}

	// Check 5: The frozenCompilation is internally consistent.
	if !auditCompilation(compilation, reporter) {
		return false
	}

	return !reporter.failed
}

// controlIDExists checks if a controlID exists in the controls array.
func controlIDExists(controls []controlRegion, id controlID) bool {
	if !id.valid() {
		return false
	}
	return uint64(id) <= uint64(len(controls))
}

// auditControlHierarchy validates the control function requirements.
func auditControlHierarchy(controls []controlRegion, records []retainedRecord, reporter *auditReporter) bool {
	// Find all function roots (regions with parent == 0).
	functionRoots := make(map[controlID]bool)
	for _, region := range controls {
		if region.Parent == 0 {
			functionRoots[region.ID] = true
		}
	}

	// For each function root, find exactly one controlFunction record.
	foundFunctions := make(map[controlID]bool)
	var functionCallables map[controlID]callableRef
	functionCallables = make(map[controlID]callableRef)

	for _, record := range records {
		if record.Control != nil && record.Control.Kind == controlFunction {
			region := record.Control.Region
			if !functionRoots[region] {
				reporter.error("controlFunction record does not belong to a function root")
				return false
			}
			if foundFunctions[region] {
				reporter.error("multiple controlFunction records for one function root")
				return false
			}
			foundFunctions[region] = true
			functionCallables[region] = record.Control.Callable
		}
	}

	// Check that every function root has exactly one controlFunction. Iterate
	// controls (already in ascending controlID order) rather than ranging over
	// the functionRoots map directly, so which missing root is reported first
	// is deterministic across runs, not dependent on Go's randomized map order.
	for _, region := range controls {
		if region.Parent != 0 {
			continue
		}
		if !foundFunctions[region.ID] {
			reporter.error("function root has no controlFunction record")
			return false
		}
	}

	// Check that all records in each function's subtree have the same callableRef.
	for _, record := range records {
		if record.Control == nil || record.Control.Kind == controlFunction {
			continue
		}
		region := record.Control.Region
		// Find which function root this region belongs to.
		root := findFunctionRoot(controls, region)
		if root == 0 {
			reporter.error("control record region does not descend from a function root")
			return false
		}
		expected := functionCallables[root]
		actual := record.Control.Callable
		if expected.Symbol != actual.Symbol || expected.Syntax != actual.Syntax {
			reporter.error("control record has inconsistent callable reference")
			return false
		}
	}

	return true
}

// findFunctionRoot finds the root region (parent == 0) that a region descends from.
func findFunctionRoot(controls []controlRegion, id controlID) controlID {
	if !id.valid() || uint64(id) > uint64(len(controls)) {
		return 0
	}
	current := controls[id-1]
	for current.Parent != 0 {
		if uint64(current.Parent) > uint64(len(controls)) {
			return 0
		}
		current = controls[current.Parent-1]
	}
	return current.ID
}

// resolveNode reaches the immutable surface node/tree pair a SyntaxRef names,
// through the still-available module graph. The tree is not yet discarded at
// this point in run06a; buildFrozenCompilation, in this same file, already
// reads gen.inputs.Graph/tree structure directly before the final handoff
// discards them.
// isTopLevelChild reports whether a node is a direct child of its module's
// root File node. This is pure immutable graph structure: it consults no
// symbol, scope, or spelling, so it holds identically for a declaration whose
// symbol failed to resolve.
func resolveNode(gen *generation, ref symbol.SyntaxRef) (syntax.Node, *syntax.Tree, bool) {
	if gen == nil || gen.inputs.Graph == nil {
		return syntax.Node{}, nil, false
	}
	item, ok := gen.inputs.Graph.Module(ref.Module)
	if !ok || item.Tree == nil {
		return syntax.Node{}, nil, false
	}
	node, ok := item.Tree.Node(ref.Node)
	if !ok {
		return syntax.Node{}, nil, false
	}
	return node, item.Tree, true
}

// requiredControlKind is the closed, non-reversible mapping from an arm's
// authored syntax kind to the single controlKind its record must carry.
// AssignmentStmt and ExpressionStmt deliberately both map to
// controlExpression, matching what statement_facts.go actually retains,
// distinguished only by StatementForm, not by Kind.
func requiredControlKind(kind syntax.NodeKind) (controlKind, bool) {
	switch kind {
	case syntax.BindingDecl:
		return controlBinding, true
	case syntax.BlockStmt:
		return controlBlock, true
	case syntax.ReturnStmt:
		return controlReturn, true
	case syntax.IfStmt:
		return controlIf, true
	case syntax.WhileStmt:
		return controlWhile, true
	case syntax.RangeLoopStmt:
		return controlRangeLoop, true
	case syntax.ForStmt:
		return controlFor, true
	case syntax.SwitchStmt:
		return controlSwitch, true
	case syntax.SwitchCase:
		return controlSwitchCase, true
	case syntax.BreakStmt:
		return controlBreak, true
	case syntax.ContinueStmt:
		return controlContinue, true
	case syntax.DeferStmt:
		return controlDefer, true
	case syntax.PrintStmt:
		return controlPrint, true
	case syntax.AssignmentStmt, syntax.ExpressionStmt:
		return controlExpression, true
	default:
		return 0, false
	}
}

// auditStructuralComposition implements freeze-audit invariant 9 (every
// region-owning control record's structural composition is exact and
// resolves correctly, checked in order and stopping at the first failure)
// and invariant 10 (every retained controlBinding record). It uses the same
// expectedComposition function population used, so population and the audit
// cannot silently drift from each other.
func auditStructuralComposition(gen *generation, controls []controlRegion, records []retainedRecord, reporter *auditReporter) bool {
	topLevel, ok := topLevelSyntaxIndex(gen)
	if !ok {
		reporter.error("structural composition cannot index top-level syntax")
		return false
	}
	bySyntax := make(map[symbol.SyntaxRef][]*retainedRecord, len(records))
	for index := range records {
		if records[index].Control != nil {
			bySyntax[records[index].Header.Syntax] = append(bySyntax[records[index].Header.Syntax], &records[index])
		}
	}

	for index := range records {
		record := &records[index]
		if record.Control == nil {
			continue
		}
		ctrl := record.Control

		if ctrl.Kind == controlBinding {
			if !auditControlBindingRecord(gen, controls, topLevel, record, reporter) {
				return false
			}
		}

		// Gate on the kind, not on whether the retained slice happens to be
		// non-empty: a composition-owning record whose Composition was dropped
		// entirely must still be reconstructed and rejected. A genuinely empty
		// construct (a switch with no cases and no else) stays valid because
		// expectedComposition returns empty for it too, and the lengths agree.
		if allowedStructuralRoles(ctrl.Kind) == nil {
			continue
		}

		// 9.1: exact reconstruction, first. A record that fails this should
		// never be trusted enough to have its individual arms resolved.
		node, tree, ok := resolveNode(gen, record.Header.Syntax)
		if !ok {
			reporter.error("structural composition record names unreachable syntax")
			return false
		}
		expected := expectedComposition(record.Header.Syntax, node, tree)
		if len(expected) != len(ctrl.Composition) {
			reporter.error("structural composition does not match its expected reconstruction")
			return false
		}
		for entryIndex, entry := range expected {
			if entry != ctrl.Composition[entryIndex] {
				reporter.error("structural composition does not match its expected reconstruction")
				return false
			}
		}

		for _, entry := range ctrl.Composition {
			armNode, _, armOK := resolveNode(gen, entry.Arm)
			if !armOK {
				reporter.error("structural composition arm names unreachable syntax")
				return false
			}
			if armNode.Kind() == syntax.Missing || armNode.Kind() == syntax.Error {
				// Ordinary parser recovery: retained and checked by 9.1 above,
				// exempted from 9.2, 9.3, and 9.4, since no record exists to
				// resolve. No additional C0619 for damage already reported.
				continue
			}

			// 9.2: cross-record resolution, second, only after 9.1 passes.
			// Only records with Control != nil are counted; a bindingRecord
			// coincidentally sharing the same SyntaxRef (e.g. a for-initializer
			// binding) is not counted and causes no false ambiguity.
			matches := bySyntax[entry.Arm]
			if len(matches) != 1 {
				reporter.error("structural composition arm does not resolve to exactly one control record")
				return false
			}
			armRecord := matches[0]

			// 9.3: kind correspondence, third, only after 9.2 locates a
			// unique record.
			requiredKind, mapped := requiredControlKind(armNode.Kind())
			if !mapped || armRecord.Control.Kind != requiredKind {
				reporter.error("structural composition arm control kind does not match its syntax kind")
				return false
			}

			// 9.4: lexical placement, after the kind matches.
			if regionOwningControl(armRecord.Control.Kind) {
				if !controlIDExists(controls, armRecord.Control.Region) || controls[armRecord.Control.Region-1].Parent != ctrl.Region {
					reporter.error("structural composition region-owning arm has the wrong parent region")
					return false
				}
			} else if armRecord.Control.Region != ctrl.Region {
				reporter.error("structural composition leaf arm names a region other than its parent's")
				return false
			}
		}
	}
	return true
}

// topLevelSyntaxIndex builds the module-level declaration set once for the
// complete audit. Looking through a File node separately for every local
// binding would make an otherwise linear freeze audit quadratic in top-level
// declarations times local bindings.
func topLevelSyntaxIndex(gen *generation) (map[symbol.SyntaxRef]bool, bool) {
	if gen == nil || gen.inputs.Graph == nil {
		return nil, false
	}
	result := make(map[symbol.SyntaxRef]bool)
	for _, item := range gen.inputs.Graph.Modules() {
		if item.Tree == nil {
			return nil, false
		}
		root, ok := item.Tree.Node(item.Tree.Root())
		if !ok || root.Kind() != syntax.File {
			return nil, false
		}
		for _, child := range root.Children() {
			result[symbol.SyntaxRef{Module: item.ID, Node: child}] = true
		}
	}
	return result, true
}

// auditControlBindingRecord checks invariant 10 for one retained
// controlBinding record: its Header.Syntax names an actual BindingDecl
// occurrence, it is a leaf, its Composition is empty, and its Region is
// nonzero and names an existing region.
func auditControlBindingRecord(gen *generation, controls []controlRegion, topLevel map[symbol.SyntaxRef]bool, record *retainedRecord, reporter *auditReporter) bool {
	node, _, ok := resolveNode(gen, record.Header.Syntax)
	if !ok || node.Kind() != syntax.BindingDecl {
		reporter.error("controlBinding record does not name a BindingDecl occurrence")
		return false
	}
	// A module-level binding must never carry a controlBinding record. Decide
	// this from immutable graph structure — direct childhood of the module's
	// root File node — rather than from spelling or a successful symbol
	// lookup, so a local binding whose symbol is absent stays valid while a
	// top-level one is rejected regardless of what its symbol did.
	if topLevel[record.Header.Syntax] {
		reporter.error("controlBinding record names a top-level binding declaration")
		return false
	}
	if regionOwningControl(record.Control.Kind) {
		reporter.error("controlBinding record is not a leaf")
		return false
	}
	if len(record.Control.Composition) != 0 {
		reporter.error("controlBinding record carries a nonempty composition")
		return false
	}
	if record.Control.Region == 0 || !controlIDExists(controls, record.Control.Region) {
		reporter.error("controlBinding record names a missing or invalid region")
		return false
	}
	return true
}

// auditCompilation validates the frozenCompilation consistency.
func auditCompilation(compilation frozenCompilation, reporter *auditReporter) bool {
	// Check: one root module.
	if compilation.Root == 0 {
		reporter.error("compilation has no root module")
		return false
	}

	// Check: Root module must exist in Modules.
	rootExists := false
	for _, m := range compilation.Modules {
		if m.ID == compilation.Root {
			rootExists = true
			break
		}
	}
	if !rootExists {
		reporter.error("root module does not exist in modules list")
		return false
	}

	// Check: complete DependencyOrder (same module count, no gaps).
	if len(compilation.DependencyOrder) != len(compilation.Modules) {
		reporter.error("dependency order count does not match module count")
		return false
	}

	// Check: all modules in DependencyOrder exist.
	moduleIDMap := make(map[module.ModuleID]bool)
	for _, m := range compilation.Modules {
		moduleIDMap[m.ID] = true
	}
	for _, id := range compilation.DependencyOrder {
		if !moduleIDMap[id] {
			reporter.error("dependency order contains invalid module ID")
			return false
		}
	}

	// Check: every import target exists in Modules.
	for _, m := range compilation.Modules {
		for _, imp := range m.Imports {
			if !moduleIDMap[imp.Target] {
				reporter.error("import target does not exist in modules")
				return false
			}
		}
	}

	return true
}

// auditReporter collects audit errors.
type auditReporter struct {
	gen    *generation
	failed bool
}

func (r *auditReporter) error(message string) {
	r.failed = true
	if r.gen != nil {
		r.gen.report("freeze audit: "+message, source.Span{})
	}
}
