package module

// ReverseDependents returns, for each module, the set of modules that
// directly import it. The returned map and its slices are owned by the
// caller; mutating them does not affect the graph. Only modules that have at
// least one importer appear as keys, and each key's slice is ordered by
// module discovery/ID order.
//
// The index is computed once when the graph is built and cached on the graph
// itself, because a Graph is immutable after construction (its fields are
// unexported and accessors return copies). It is therefore safe to call this
// any number of times without recomputation.
func (g *Graph) ReverseDependents() map[ModuleID][]ModuleID {
	if g == nil {
		return nil
	}
	result := make(map[ModuleID][]ModuleID, len(g.reverse))
	for target, importers := range g.reverse {
		result[target] = append([]ModuleID(nil), importers...)
	}
	return result
}

// TransitiveDependents returns the full set of modules that depend on id,
// directly or transitively (id's importers, their importers, and so on).
// The returned slice is ordered such that a module always precedes the
// modules that depend on it — dependency order, as defined by
// DependencyOrder — so for the chain A imports B imports C, changing C
// reports [B A]. The slice is owned by the caller.
//
// The closure is computed fresh on each call with a BFS over the cached
// direct reverse index; the direct index is small and the per-query result
// is usually much smaller than the whole graph, so caching the full closure
// for every module would be premature.
func (g *Graph) TransitiveDependents(id ModuleID) []ModuleID {
	if g == nil || id == 0 || uint64(id) > uint64(len(g.modules)) {
		return nil
	}
	visited := make(map[ModuleID]bool, len(g.modules))
	queue := make([]ModuleID, 0, len(g.modules))
	if importers := g.reverse[id]; len(importers) > 0 {
		for _, importer := range importers {
			if !visited[importer] {
				visited[importer] = true
				queue = append(queue, importer)
			}
		}
	}
	collect := make([]ModuleID, 0, len(queue))
	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		collect = append(collect, current)
		for _, importer := range g.reverse[current] {
			if !visited[importer] {
				visited[importer] = true
				queue = append(queue, importer)
			}
		}
	}
	// Reorder by dependency order (dependencies before importers) so the
	// result is deterministic and a module always precedes its importers.
	positions := make([]int, len(g.modules)+1)
	for index, ordered := range g.dependency {
		positions[ordered] = index
	}
	result := append([]ModuleID(nil), collect...)
	for i := 1; i < len(result); i++ {
		for j := i; j > 0 && positions[result[j-1]] > positions[result[j]]; j-- {
			result[j-1], result[j] = result[j], result[j-1]
		}
	}
	return result
}
