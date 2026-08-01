package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestSpecializationKeyEncodingIdentity(t *testing.T) {
	typeArgs := []types.TypeID{types.TypeID(1), types.TypeID(2), types.TypeID(3)}
	first := newSpecializationKey(symbol.SymbolID(7), typeArgs, types.Pebble)
	second := newSpecializationKey(symbol.SymbolID(7), []types.TypeID{types.TypeID(1), types.TypeID(2), types.TypeID(3)}, types.Pebble)
	if first.TypeArgs == "" {
		t.Fatal("encoding of a nonempty argument list is empty")
	}
	if first != second {
		t.Fatalf("structurally identical keys differ: %+v vs %+v", first, second)
	}
}

func TestSpecializationKeyEncodingNoCollisions(t *testing.T) {
	cases := []struct {
		name string
		args []types.TypeID
	}{
		{name: "empty", args: nil},
		{name: "single", args: []types.TypeID{types.TypeID(1)}},
		{name: "different-content", args: []types.TypeID{types.TypeID(2), types.TypeID(3)}},
		{name: "same-content-reordered", args: []types.TypeID{types.TypeID(3), types.TypeID(2)}},
		{name: "longer", args: []types.TypeID{types.TypeID(1), types.TypeID(2), types.TypeID(3), types.TypeID(4)}},
	}
	encodings := make(map[string]bool)
	for _, tc := range cases {
		encoded := encodeTypeArgs(tc.args)
		if encodings[encoded] {
			t.Fatalf("distinct argument lists collided: %+v shares encoding %q", tc.args, encoded)
		}
		encodings[encoded] = true
	}
}

func TestSpecializationKeyDistinctArgumentsNeverCollide(t *testing.T) {
	bigger := []types.TypeID{types.TypeID(1), types.TypeID(1)}
	smaller := []types.TypeID{types.TypeID(1)}
	if encodeTypeArgs(bigger) == encodeTypeArgs(smaller) {
		t.Fatal("slices of different lengths produced the same encoding")
	}
	if newSpecializationKey(symbol.SymbolID(1), bigger, types.Pebble).TypeArgs == encodeTypeArgs(smaller) {
		t.Fatal("a multi-element slice collided with a shorter slice")
	}
}

func TestSpecializationCacheReserveFreshKey(t *testing.T) {
	cache := newSpecializationCache()
	key := newSpecializationKey(symbol.SymbolID(1), []types.TypeID{types.TypeID(5)}, types.Pebble)
	entry, alreadyInProgress := cache.reserve(key)
	if alreadyInProgress {
		t.Fatal("fresh key reported as already in progress")
	}
	if entry == nil {
		t.Fatal("reserve returned a nil entry for a fresh key")
	}
	if entry.State != specializationInProgress {
		t.Fatalf("fresh entry state = %v, want specializationInProgress", entry.State)
	}
}

func TestSpecializationCacheReserveSameKeyIsInProgress(t *testing.T) {
	cache := newSpecializationCache()
	key := newSpecializationKey(symbol.SymbolID(2), []types.TypeID{types.TypeID(9)}, types.Pebble)
	first, _ := cache.reserve(key)
	second, alreadyInProgress := cache.reserve(key)
	if second != first {
		t.Fatalf("recursive reserve returned a different entry: %p vs %p", second, first)
	}
	if !alreadyInProgress {
		t.Fatal("reserving the same in-progress key did not report alreadyInProgress")
	}
}

func TestSpecializationCacheFinishThenReserveIsDone(t *testing.T) {
	cache := newSpecializationCache()
	key := newSpecializationKey(symbol.SymbolID(3), []types.TypeID{types.TypeID(7)}, types.Pebble)
	entry, _ := cache.reserve(key)
	cache.finish(key)
	if entry.State != specializationDone {
		t.Fatalf("finished entry state = %v, want specializationDone", entry.State)
	}
	again, alreadyInProgress := cache.reserve(key)
	if again != entry {
		t.Fatalf("reserve after finish returned a different entry: %p vs %p", again, entry)
	}
	if alreadyInProgress {
		t.Fatal("reserve of a finished key reported alreadyInProgress")
	}
	if again.State != specializationDone {
		t.Fatalf("reused finished entry state = %v, want specializationDone", again.State)
	}
}

func TestSpecializationCacheFinishNoOpOnAbsentKey(t *testing.T) {
	cache := newSpecializationCache()
	cache.finish(newSpecializationKey(symbol.SymbolID(99), nil, types.Pebble))
	if len(cache.entries) != 0 {
		t.Fatalf("finish on an absent key inserted an entry: %+v", cache.entries)
	}
}

func TestSpecializationCacheLookupNeverReserved(t *testing.T) {
	cache := newSpecializationCache()
	entry, ok := cache.lookup(newSpecializationKey(symbol.SymbolID(4), []types.TypeID{types.TypeID(8)}, types.Pebble))
	if ok {
		t.Fatal("lookup of a never-reserved key reported a hit")
	}
	if entry != nil {
		t.Fatalf("lookup of a never-reserved key returned entry %+v", entry)
	}
}

func TestSpecializationCacheDistinctKeysDoNotCollide(t *testing.T) {
	cache := newSpecializationCache()
	keyA := newSpecializationKey(symbol.SymbolID(1), []types.TypeID{types.TypeID(1)}, types.Pebble)
	keyB := newSpecializationKey(symbol.SymbolID(2), []types.TypeID{types.TypeID(1)}, types.Pebble)
	keyC := newSpecializationKey(symbol.SymbolID(1), []types.TypeID{types.TypeID(2)}, types.Pebble)
	keyD := newSpecializationKey(symbol.SymbolID(1), []types.TypeID{types.TypeID(1)}, types.C)
	entryA, _ := cache.reserve(keyA)
	for _, key := range []specializationKey{keyB, keyC, keyD} {
		entry, alreadyInProgress := cache.reserve(key)
		if alreadyInProgress {
			t.Fatalf("fresh distinct key %+v was reported in progress because of a collision", key)
		}
		if entry == nil || entry == entryA {
			t.Fatalf("distinct key %+v did not get its own independent entry", key)
		}
	}
	if entry, ok := cache.lookup(keyA); !ok || entry != entryA {
		t.Fatal("key A lost its own entry")
	}
	for _, key := range []specializationKey{keyB, keyC, keyD} {
		entry, ok := cache.lookup(key)
		if !ok || entry == nil {
			t.Fatalf("key %+v lost its own entry", key)
		}
	}
	if len(cache.entries) != 4 {
		t.Fatalf("distinct keys share entries: %d entries, want 4", len(cache.entries))
	}
}

func TestSpecializationCacheNilReceiverSafety(t *testing.T) {
	var cache *specializationCache
	key := newSpecializationKey(symbol.SymbolID(1), nil, types.Pebble)
	if entry, ok := cache.reserve(key); entry != nil || ok {
		t.Fatalf("nil-cache reserve = (%p, %v), want (nil, false)", entry, ok)
	}
	cache.finish(key)
	if entry, ok := cache.lookup(key); entry != nil || ok {
		t.Fatalf("nil-cache lookup = (%p, %v), want (nil, false)", entry, ok)
	}
}
