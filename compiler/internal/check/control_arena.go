package check

type controlRegion struct {
	ID       controlID
	Parent   controlID
	Depth    uint32
	Children []controlID
}

func cloneControlRegion(value controlRegion) controlRegion {
	value.Children = append([]controlID(nil), value.Children...)
	return value
}

type controlArena struct {
	values []mutableControlRegion
}

type mutableControlRegion struct {
	ID     controlID
	Parent controlID
	Depth  uint32
}

func (a *controlArena) append(parent controlID, maxDepth, maxRegions uint32) (controlID, bool) {
	if a == nil || uint64(len(a.values)) >= uint64(maxRegions) || uint64(len(a.values)) >= uint64(^uint32(0)) {
		return 0, false
	}

	depth := uint32(1)
	if parent != 0 {
		if uint64(parent) > uint64(len(a.values)) {
			return 0, false
		}
		parentRegion := a.values[parent-1]
		if parentRegion.ID != parent || parentRegion.Depth == 0 || parentRegion.Depth == ^uint32(0) {
			return 0, false
		}
		depth = parentRegion.Depth + 1
	}
	if depth > maxDepth {
		return 0, false
	}

	id := controlID(len(a.values) + 1)
	a.values = append(a.values, mutableControlRegion{ID: id, Parent: parent, Depth: depth})
	return id, true
}

func (a *controlArena) region(id controlID) (mutableControlRegion, bool) {
	if a == nil || !id.valid() || uint64(id) > uint64(len(a.values)) {
		return mutableControlRegion{}, false
	}
	return a.values[id-1], true
}

// freeze materializes ordered child lists in linear work. The first pass
// validates regions and counts one edge for every non-root. Exact child slices
// are then allocated, and the second pass fills them in control-ID order.
func (a *controlArena) freeze(maxRegions uint32) ([]controlRegion, bool) {
	if a == nil || uint64(len(a.values)) > uint64(maxRegions) || uint64(len(a.values)) > uint64(^uint32(0)) {
		return nil, false
	}
	counts := make([]uint32, len(a.values))
	for index, value := range a.values {
		if value.ID != controlID(index+1) || value.Depth == 0 {
			return nil, false
		}
		if value.Parent == 0 {
			if value.Depth != 1 {
				return nil, false
			}
			continue
		}
		if uint64(value.Parent) > uint64(index) {
			return nil, false
		}
		parent := a.values[value.Parent-1]
		if parent.Depth == ^uint32(0) || value.Depth != parent.Depth+1 || counts[value.Parent-1] == ^uint32(0) {
			return nil, false
		}
		counts[value.Parent-1]++
	}

	result := make([]controlRegion, len(a.values))
	for index, value := range a.values {
		result[index] = controlRegion{ID: value.ID, Parent: value.Parent, Depth: value.Depth}
		if uint64(counts[index]) > uint64(int(^uint(0)>>1)) {
			return nil, false
		}
		result[index].Children = make([]controlID, int(counts[index]))
	}
	next := make([]uint32, len(a.values))
	for _, value := range a.values {
		if value.Parent == 0 {
			continue
		}
		parentIndex := value.Parent - 1
		result[parentIndex].Children[next[parentIndex]] = value.ID
		next[parentIndex]++
	}
	return result, true
}
