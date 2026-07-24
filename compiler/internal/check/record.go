package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

type recordHeader struct {
	ID          recordID
	Syntax      symbol.SyntaxRef
	Span        source.Span
	Owner       symbol.SymbolID
	Alternative alternativeTag
	Suppressed  bool
}

// retainedRecord is the lifecycle arena entry used by this slice. Later fact
// slices attach their closed record payloads while retaining this header and
// component accounting contract.
type retainedRecord struct {
	Header   recordHeader
	Values   []valueID
	Controls []controlID
}

func cloneRetainedRecord(value retainedRecord) retainedRecord {
	value.Values = append([]valueID(nil), value.Values...)
	value.Controls = append([]controlID(nil), value.Controls...)
	return value
}

type recordArena struct {
	values     []retainedRecord
	components uint64
}

func (a *recordArena) append(value retainedRecord, validValue func(valueID) bool, validControl func(controlID) bool, maxRecords, maxComponents uint32) (recordID, bool) {
	if a == nil || value.Header.ID != 0 || !value.Header.Alternative.valid() || uint64(len(a.values)) >= uint64(maxRecords) {
		return 0, false
	}
	components := uint64(len(value.Values)) + uint64(len(value.Controls))
	if components > uint64(maxComponents) || a.components > uint64(maxComponents)-components {
		return 0, false
	}
	for _, id := range value.Values {
		if !validValue(id) {
			return 0, false
		}
	}
	for _, id := range value.Controls {
		if !validControl(id) {
			return 0, false
		}
	}

	id := recordID(len(a.values) + 1)
	value.Header.ID = id
	value = cloneRetainedRecord(value)
	a.values = append(a.values, value)
	a.components += components
	return id, true
}

func (a *recordArena) record(id recordID) (retainedRecord, bool) {
	if a == nil || !id.valid() || uint64(id) > uint64(len(a.values)) {
		return retainedRecord{}, false
	}
	return cloneRetainedRecord(a.values[id-1]), true
}

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

type frozenRecords struct {
	values     []retainedRecord
	controls   []controlRegion
	components uint64
}

func (f frozenRecords) Records() []retainedRecord {
	result := make([]retainedRecord, len(f.values))
	for index := range f.values {
		result[index] = cloneRetainedRecord(f.values[index])
	}
	return result
}

func (f frozenRecords) Controls() []controlRegion {
	result := make([]controlRegion, len(f.controls))
	for index := range f.controls {
		result[index] = cloneControlRegion(f.controls[index])
	}
	return result
}

func (f frozenRecords) Components() uint64 { return f.components }
