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
	GenerationFailed    bool
}

// run06a is the sole entry point for freeze audit, solve, and handoff assembly.
func run06a(inputs Inputs, diagnostics *diagnostic.DiagnosticSet, config Config) *solveHandoff {
	// Step 1: run06a3 to get the complete mutable fact arena.
	facts := run06a3(inputs, diagnostics, config)
	if facts == nil || facts.Generation == nil || facts.Session == nil || facts.Program == nil {
		return &solveHandoff{GenerationHadErrors: true, GenerationFailed: true}
	}
	if facts.Session.Fatal() {
		return &solveHandoff{GenerationHadErrors: true, GenerationFailed: true}
	}

	// Step 2: Build and freeze the frozenCompilation. Note this reads limits
	// from facts.Generation.config (normalized by newGeneration), not from
	// this function's own config parameter, which may still be zero-valued.
	compilation, ok := buildFrozenCompilation(facts.Generation, inputs)
	if !ok {
		return &solveHandoff{GenerationHadErrors: true, GenerationFailed: true}
	}

	// Step 3: Freeze the generation to obtain frozen records, roots, values.
	frozen, ok := facts.Generation.freeze()
	if !ok {
		return &solveHandoff{GenerationHadErrors: true, GenerationFailed: true}
	}

	// Step 4: Perform additional freeze-audit checks.
	if !auditFrozen(facts.Generation, frozen, compilation) {
		return &solveHandoff{GenerationHadErrors: true, GenerationFailed: true}
	}

	// Step 5: Call Solve() exactly once.
	solution := facts.Session.Solve()

	// Step 6: Create the semantic snapshot.
	semantics, ok := infer.Snapshot(facts.Program, solution, diagnostics)
	if !ok {
		return &solveHandoff{GenerationHadErrors: true, GenerationFailed: true}
	}

	// Step 7: Post-solve ownership audit.
	if !semantics.Matches(solution) || semantics.Resolution() != inputs.Resolution {
		return &solveHandoff{GenerationHadErrors: true, GenerationFailed: true}
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
