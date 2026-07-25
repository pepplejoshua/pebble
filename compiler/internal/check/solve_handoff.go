package check

import (
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

// frozenSource is a tree-free snapshot of source file identity.
type frozenSource struct {
	ID     source.ID
	Path   string
	Length uint32
}

// frozenImport is a tree-free snapshot of one import edge.
type frozenImport struct {
	Span   source.Span
	Target module.ModuleID
}

// frozenModule is a tree-free snapshot of one module.
type frozenModule struct {
	ID           module.ModuleID
	Key          module.ModuleKey
	Source       source.ID
	Span         source.Span
	Imports      []frozenImport
	Declarations []symbol.SymbolID
}

// frozenCompilation is the tree-free compilation metadata handoff.
type frozenCompilation struct {
	Root            module.ModuleID
	Modules         []frozenModule
	DependencyOrder []module.ModuleID
	Sources         []frozenSource
}

// solveHandoff is the package-private result after freeze audit, one solve, and snapshot.
type solveHandoff struct {
	Compilation         frozenCompilation
	Semantics           *infer.SemanticSnapshot
	Solution            *infer.Solution
	Records             frozenRecords
	Roots               frozenRoots
	Constants           frozenConstants
	GenerationHadErrors bool
}

// run06a is the sole entry point for freeze audit, solve, and handoff assembly.
func run06a(inputs Inputs, diagnostics *diagnostic.DiagnosticSet, config Config) *solveHandoff {
	// Step 1: run06a3 to get the complete mutable fact arena.
	facts := run06a3(inputs, diagnostics, config)
	if facts == nil || facts.Generation == nil || facts.Session == nil || facts.Program == nil {
		return &solveHandoff{GenerationHadErrors: true}
	}
	if facts.Session.Fatal() {
		return &solveHandoff{GenerationHadErrors: true}
	}

	// Step 2: Build and freeze the frozenCompilation. Note this reads limits
	// from facts.Generation.config (normalized by newGeneration), not from
	// this function's own config parameter, which may still be zero-valued.
	compilation, ok := buildFrozenCompilation(facts.Generation, inputs)
	if !ok {
		return &solveHandoff{GenerationHadErrors: true}
	}

	// Step 3: Freeze the generation to obtain frozen records, roots, values.
	frozen, ok := facts.Generation.freeze()
	if !ok {
		return &solveHandoff{GenerationHadErrors: true}
	}

	// Step 4: Perform additional freeze-audit checks.
	if !auditFrozen(facts.Generation, frozen, compilation) {
		return &solveHandoff{GenerationHadErrors: true}
	}

	// Step 5: Call Solve() exactly once.
	solution := facts.Session.Solve()

	// Step 6: Create the semantic snapshot.
	semantics, ok := infer.Snapshot(facts.Program, solution, diagnostics)
	if !ok {
		return &solveHandoff{GenerationHadErrors: true}
	}

	// Step 7: Post-solve ownership audit.
	if !semantics.Matches(solution) || semantics.Resolution() != inputs.Resolution {
		return &solveHandoff{GenerationHadErrors: true}
	}

	// Step 8: Assemble and return the handoff.
	return &solveHandoff{
		Compilation:         compilation,
		Semantics:           semantics,
		Solution:            solution,
		Records:             frozen.records,
		Roots:               frozen.roots,
		Constants:           facts.Constants.freeze(),
		GenerationHadErrors: diagnostics.HasErrors(),
	}
}

// componentBudget is one running charge shared across every module, import,
// declaration, and source entry in a single frozenCompilation. It is
// independent of the main record arena's MaxRecordComponents charge, scoped
// only to compilation metadata.
type componentBudget struct {
	charged uint64
	limit   uint64
}

func (b *componentBudget) charge() bool {
	if b == nil || b.charged >= b.limit {
		return false
	}
	b.charged++
	return true
}

// buildFrozenCompilation constructs the tree-free compilation snapshot. It
// reads limits from gen.config, the already-normalized copy newGeneration
// stored, not from a separately supplied Config that may still be zero-valued.
func buildFrozenCompilation(gen *generation, inputs Inputs) (frozenCompilation, bool) {
	if gen == nil || gen.inputs.Graph == nil || gen.inputs.Sources == nil {
		return frozenCompilation{}, false
	}

	graph := gen.inputs.Graph
	root := graph.Root
	modules := graph.Modules()
	budget := &componentBudget{limit: uint64(gen.config.MaxRecordComponents)}

	// Validate module count and ordering.
	if len(modules) == 0 || root == 0 {
		gen.report("compilation requires a rooted module graph", source.Span{})
		return frozenCompilation{}, false
	}

	// Build frozenModule entries in ascending ModuleID order.
	frozenModules := make([]frozenModule, len(modules))
	seenModuleIDs := make(map[module.ModuleID]bool)

	for index, item := range modules {
		// Validate module ID ordering.
		if item.ID != module.ModuleID(index+1) {
			gen.report("module graph is not ordered by ascending ModuleID", source.Span{})
			return frozenCompilation{}, false
		}
		if item.ID == 0 {
			gen.report("module has invalid ID", source.Span{})
			return frozenCompilation{}, false
		}
		seenModuleIDs[item.ID] = true

		// Extract module span from File node.
		if item.Tree == nil {
			gen.report("module has no syntax tree", source.Span{})
			return frozenCompilation{}, false
		}
		rootNode, ok := item.Tree.Node(item.Tree.Root())
		if !ok || rootNode.Kind() != syntax.File {
			gen.report("module syntax tree root is not a File node", source.Span{Source: item.Source})
			return frozenCompilation{}, false
		}
		moduleSpan := rootNode.Span()

		// Validate source consistency.
		file, ok := gen.inputs.Sources.File(item.Source)
		if !ok {
			gen.report("module source file is not reachable", source.Span{Source: item.Source})
			return frozenCompilation{}, false
		}
		if moduleSpan.Source != item.Source || moduleSpan.End > file.Len() {
			gen.report("module span is inconsistent with source", source.Span{Source: item.Source})
			return frozenCompilation{}, false
		}

		// Charge module to the shared compilation-metadata budget before allocating.
		if !budget.charge() {
			gen.report("compilation metadata exceeds resource bound", source.Span{})
			return frozenCompilation{}, false
		}

		// Build imports in authored order.
		frozenImports := make([]frozenImport, len(item.Imports))
		for impIndex, edge := range item.Imports {
			if !budget.charge() {
				gen.report("compilation metadata exceeds resource bound", edge.Span)
				return frozenCompilation{}, false
			}
			// Validate import target exists.
			if _, ok := graph.Module(edge.Target); !ok {
				gen.report("import target does not exist in graph", edge.Span)
				return frozenCompilation{}, false
			}
			frozenImports[impIndex] = frozenImport{
				Span:   edge.Span,
				Target: edge.Target,
			}
		}

		// Build declarations in source order. A module with zero top-level
		// declarations is a valid, successful result (an empty, non-nil-vs-nil
		// slice is not distinguishable from a failure by itself), so this
		// checks the explicit ok result rather than declarations == nil.
		declarations, declOK := buildModuleDeclarations(gen, item, inputs, budget)
		if !declOK {
			return frozenCompilation{}, false
		}

		frozenModules[index] = frozenModule{
			ID:           item.ID,
			Key:          item.Key,
			Source:       item.Source,
			Span:         moduleSpan,
			Imports:      frozenImports,
			Declarations: declarations,
		}
	}

	// Get dependency order and validate it matches module count.
	depOrder := graph.DependencyOrder()
	if len(depOrder) != len(modules) {
		gen.report("dependency order does not match module count", source.Span{})
		return frozenCompilation{}, false
	}

	// Validate all modules in dependency order exist.
	for _, modID := range depOrder {
		if !seenModuleIDs[modID] {
			gen.report("dependency order contains invalid module ID", source.Span{})
			return frozenCompilation{}, false
		}
	}

	// Collect all source files used in the graph, ordered by source.ID.
	sourceMap := make(map[source.ID]bool)
	for _, item := range modules {
		sourceMap[item.Source] = true
	}

	sourceIDs := make([]source.ID, 0, len(sourceMap))
	for id := range sourceMap {
		sourceIDs = append(sourceIDs, id)
	}
	sort.Slice(sourceIDs, func(i, j int) bool { return sourceIDs[i] < sourceIDs[j] })

	frozenSources := make([]frozenSource, len(sourceIDs))
	for index, id := range sourceIDs {
		if !budget.charge() {
			gen.report("compilation metadata exceeds resource bound", source.Span{})
			return frozenCompilation{}, false
		}
		file, ok := gen.inputs.Sources.File(id)
		if !ok {
			gen.report("source file is unreachable", source.Span{Source: id})
			return frozenCompilation{}, false
		}
		frozenSources[index] = frozenSource{
			ID:     id,
			Path:   file.Path(),
			Length: file.Len(),
		}
	}

	return frozenCompilation{
		Root:            root,
		Modules:         frozenModules,
		DependencyOrder: append([]module.ModuleID(nil), depOrder...),
		Sources:         frozenSources,
	}, true
}

// buildModuleDeclarations extracts top-level declarations for a module in
// source order, charging each to the caller's shared compilation budget. The
// returned slice is nil both on failure and on a module with zero top-level
// declarations (a legitimate, successful result), so the second return value
// is the sole failure signal; callers must check it, not the slice itself.
func buildModuleDeclarations(gen *generation, item module.Module, inputs Inputs, budget *componentBudget) ([]symbol.SymbolID, bool) {
	if gen == nil || inputs.Resolution == nil || inputs.Resolution.Symbols == nil || item.Tree == nil {
		return nil, false
	}

	// Get the File node's direct children in tree order.
	root, ok := item.Tree.Node(item.Tree.Root())
	if !ok || root.Kind() != syntax.File {
		return nil, false
	}

	childNodeIDs := root.Children()

	// Collect all module-level symbols indexed by their declaration node.
	moduleSymbolsByNode := make(map[syntax.NodeID][]symbol.Symbol)
	for _, sym := range inputs.Resolution.Symbols.All() {
		if sym.Containing == 0 && sym.Module == item.ID {
			moduleSymbolsByNode[sym.Declaration.Node] = append(moduleSymbolsByNode[sym.Declaration.Node], sym)
		}
	}

	// Walk file children and collect their symbols in order.
	var result []symbol.SymbolID
	for _, childID := range childNodeIDs {
		// Get symbols declared directly at this child node.
		if syms, ok := moduleSymbolsByNode[childID]; ok {
			for _, sym := range syms {
				if !budget.charge() {
					gen.report("compilation metadata exceeds resource bound", sym.Span)
					return nil, false
				}
				result = append(result, sym.ID)
			}
		}

		// For extern blocks, also collect symbols declared inside them.
		// They are folded into the extern block's contained declarations.
		childNode, ok := item.Tree.Node(childID)
		if ok && (childNode.Kind() == syntax.ExternDecl || childNode.Kind() == syntax.ExternBlock) {
			// Collect all symbols inside this extern block.
			descendants := collectDescendantSymbols(item.Tree, childID, moduleSymbolsByNode)
			for _, symID := range descendants {
				if !budget.charge() {
					gen.report("compilation metadata exceeds resource bound", source.Span{})
					return nil, false
				}
				result = append(result, symID)
			}
		}
	}

	return result, true
}

// collectDescendantSymbols gathers symbols declared within a subtree.
func collectDescendantSymbols(tree *syntax.Tree, rootID syntax.NodeID, symbolsByNode map[syntax.NodeID][]symbol.Symbol) []symbol.SymbolID {
	var result []symbol.SymbolID
	var queue []syntax.NodeID
	visited := make(map[syntax.NodeID]bool)
	queue = append(queue, rootID)

	for len(queue) > 0 {
		nodeID := queue[0]
		queue = queue[1:]

		if visited[nodeID] {
			continue
		}
		visited[nodeID] = true

		// Add symbols at this node (but skip the root itself, already processed).
		if nodeID != rootID {
			if syms, ok := symbolsByNode[nodeID]; ok {
				for _, sym := range syms {
					result = append(result, sym.ID)
				}
			}
		}

		// Enqueue children.
		if node, ok := tree.Node(nodeID); ok {
			for _, childID := range node.Children() {
				if !visited[childID] {
					queue = append(queue, childID)
				}
			}
		}
	}

	return result
}

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
