package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

type valueID uint32
type recordID uint32
type controlID uint32

func (id valueID) valid() bool   { return id != 0 }
func (id recordID) valid() bool  { return id != 0 }
func (id controlID) valid() bool { return id != 0 }

type alternativeTag struct {
	Choice  infer.ConstraintID
	Index   uint32
	Guarded bool
}

func (tag alternativeTag) valid() bool {
	if !tag.Guarded {
		return tag.Choice == 0 && tag.Index == 0
	}
	return tag.Choice.IsValid()
}

type valueRootKind uint8

const (
	rootSyntax valueRootKind = iota + 1
	rootSymbol
	rootInstantiation
	rootMethod
	rootSlot
)

type valueRoot struct {
	Kind        valueRootKind
	Syntax      symbol.SyntaxRef
	Symbol      symbol.SymbolID
	Slot        infer.SlotID
	Parameter   uint32
	Alternative alternativeTag
}

type rootedValue struct {
	Value valueID
	Root  valueRoot
}

type rootArena struct {
	values  []rootedValue
	byValue map[valueID]int
}

func (a *rootArena) append(value valueID, root valueRoot, validValue func(valueID) bool, validRoot func(valueRoot) bool, limit uint32) bool {
	if a == nil || !validValue(value) || !validRoot(root) || uint64(len(a.values)) >= uint64(limit) {
		return false
	}
	if _, exists := a.byValue[value]; exists {
		return false
	}
	if a.byValue == nil {
		a.byValue = make(map[valueID]int)
	}
	a.values = append(a.values, rootedValue{Value: value, Root: root})
	a.byValue[value] = len(a.values) - 1
	return true
}

func (a *rootArena) root(value valueID) (valueRoot, bool) {
	if a == nil || !value.valid() {
		return valueRoot{}, false
	}
	if index, ok := a.byValue[value]; ok && index >= 0 && index < len(a.values) {
		return a.values[index].Root, true
	}
	// Frozen roots do not need to retain the mutable arena's index.
	for index := range a.values {
		if a.values[index].Value == value {
			return a.values[index].Root, true
		}
	}
	return valueRoot{}, false
}

func cloneRootedValues(values []rootedValue) []rootedValue {
	return append([]rootedValue(nil), values...)
}

type frozenRoots struct {
	values []rootedValue
}

func (f frozenRoots) All() []rootedValue { return cloneRootedValues(f.values) }

func (f frozenRoots) Root(value valueID) (valueRoot, bool) {
	return (&rootArena{values: f.values}).root(value)
}
