package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// specializationKey identifies one monomorphized instantiation of one generic
// callable for the whole compilation. Two instantiations share a cache entry
// exactly when their (generic symbol, ordered concrete type arguments, ABI
// options) tuples match: a specialization built once is reused everywhere, and
// an in-progress marker under the same key terminates structurally recursive
// generics.
type specializationKey struct {
	Generic    symbol.SymbolID
	TypeArgs   string
	Convention types.CallingConvention
}

// newSpecializationKey encodes the ordered concrete type-argument list into
// the key's comparable TypeArgs component. The encoding is a fixed-width
// lowercase-hex spelling of each uint32 TypeID, eight digits per argument, so
// concatenation needs no separator and cannot collide: equal element counts
// and elements yield equal keys, and any difference in count or element value
// yields a distinct string.
func newSpecializationKey(generic symbol.SymbolID, typeArgs []types.TypeID, convention types.CallingConvention) specializationKey {
	return specializationKey{Generic: generic, TypeArgs: encodeTypeArgs(typeArgs), Convention: convention}
}

const specializationHexDigits = "0123456789abcdef"

func encodeTypeArgs(typeArgs []types.TypeID) string {
	buffer := make([]byte, 0, len(typeArgs)*8)
	for _, argument := range typeArgs {
		value := uint32(argument)
		for shift := 28; shift >= 0; shift -= 4 {
			buffer = append(buffer, specializationHexDigits[(value>>uint(shift))&0xf])
		}
	}
	return string(buffer)
}

// specializationState tracks one specialization's build progress.
type specializationState uint8

const (
	// specializationInProgress marks a key whose body is currently being
	// checked or built; encountering it again means recursion back into the
	// same instantiation and must stop.
	specializationInProgress specializationState = iota + 1
	// specializationDone marks a key whose specialization is fully built and
	// safe to reuse.
	specializationDone
)

type specializationEntry struct {
	State specializationState
	// Built IR (a *tir.NodeID, function declaration index, etc.) is added by
	// a later slice once actual monomorphized IR construction exists.
}

type specializationCache struct {
	entries map[specializationKey]*specializationEntry
}

// newSpecializationCache returns a fresh, freestanding cache. It is purely
// in-process for one compilation run and is never persisted across builds.
func newSpecializationCache() *specializationCache {
	return &specializationCache{entries: make(map[specializationKey]*specializationEntry)}
}

// reserve returns the cache entry for key, creating and inserting an
// in-progress entry on the first call. alreadyInProgress reports whether the
// returned entry was already marked in progress — the recursion-termination
// signal a caller must treat as "do not recurse further". A nil cache
// (matching this package's defensive style) returns (nil, false).
func (c *specializationCache) reserve(key specializationKey) (entry *specializationEntry, alreadyInProgress bool) {
	if c == nil {
		return nil, false
	}
	if existing, ok := c.entries[key]; ok {
		return existing, existing.State == specializationInProgress
	}
	entry = &specializationEntry{State: specializationInProgress}
	c.entries[key] = entry
	return entry, false
}

// finish marks an in-progress entry done. It is a nil-safe silent no-op when
// the cache is nil, the key is absent, or the entry is not in progress —
// matching validationReporter.add/generationReporter.add's receiver guard.
func (c *specializationCache) finish(key specializationKey) {
	if c == nil {
		return
	}
	entry, ok := c.entries[key]
	if !ok || entry.State != specializationInProgress {
		return
	}
	entry.State = specializationDone
}

// lookup is a plain read of key's entry, with no mutation.
func (c *specializationCache) lookup(key specializationKey) (*specializationEntry, bool) {
	if c == nil {
		return nil, false
	}
	entry, ok := c.entries[key]
	return entry, ok
}
