package types

import (
	"errors"
	"slices"
	"sync"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

func TestSnapshotPreservesIDsBuiltinsAndKeys(t *testing.T) {
	store := mustStore(t, Config{})
	builtins := store.Builtins()
	tuple := mustIntern(t, store, TupleKey([]TypeID{builtins.Int, builtins.Str}))
	function := mustIntern(t, store, FunctionKey(C, []TypeID{tuple, builtins.Uint}, builtins.Bool, true))
	nominal := mustIntern(t, store, NominalKey(symbol.SymbolID(41), []TypeID{function}))

	snapshot := mustSnapshot(t, store)
	if got := snapshot.Builtins(); got != builtins {
		t.Fatalf("Builtins() = %+v, want %+v", got, builtins)
	}
	if got := snapshot.Len(); got != store.Len() {
		t.Fatalf("Len() = %d, want %d", got, store.Len())
	}
	if got, want := collectIDs(snapshot.IDs()), collectIDs(store.IDs()); !slices.Equal(got, want) {
		t.Fatalf("IDs() = %v, want %v", got, want)
	}

	for _, id := range collectIDs(store.IDs()) {
		if !snapshot.Contains(id) {
			t.Fatalf("Contains(%d) = false", id)
		}
		storeKind, storeOK := store.Kind(id)
		snapshotKind, snapshotOK := snapshot.Kind(id)
		if !storeOK || !snapshotOK || snapshotKind != storeKind {
			t.Fatalf("Kind(%d) = %d, %v; store = %d, %v", id, snapshotKind, snapshotOK, storeKind, storeOK)
		}
		storeKey, _ := store.Key(id)
		snapshotKey, ok := snapshot.Key(id)
		if !ok || !equalKeys(snapshotKey, storeKey) {
			t.Fatalf("Key(%d) was not preserved", id)
		}
	}
	if !snapshot.Contains(nominal) || snapshot.Contains(0) || snapshot.Contains(TypeID(snapshot.Len()+1)) {
		t.Fatal("Contains must be exactly the nonzero captured-range check")
	}
}

func TestSnapshotOwnsCopiesAndAccessorsAreDefensive(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	function := mustIntern(t, store, FunctionKey(Pebble, []TypeID{b.Int, b.Uint}, b.Bool, false))
	snapshot := mustSnapshot(t, store)

	// Damage the store's private backing data after capture. The snapshot must
	// remain independent of both the entry slice and variable-length key data.
	store.entries[function-1].elements[0] = b.Char
	store.entries = append(store.entries, PointerKey(b.Str))

	key, ok := snapshot.Key(function)
	if !ok {
		t.Fatal("captured function key missing")
	}
	key.elements[0] = b.F64
	_, parameters, _, _, ok := key.Function()
	if !ok || !slices.Equal(parameters, []TypeID{b.F64, b.Uint}) {
		t.Fatalf("local key mutation was not local: %v, %v", parameters, ok)
	}
	parameters[0] = b.I8

	_, captured, _, _, ok := mustSnapshotKey(t, snapshot, function).Function()
	if !ok || !slices.Equal(captured, []TypeID{b.Int, b.Uint}) {
		t.Fatalf("snapshot key changed through store or accessor mutation: %v, %v", captured, ok)
	}
	if snapshot.Len() != uint32(function) || snapshot.Contains(TypeID(function+1)) {
		t.Fatalf("snapshot entry set changed with store: Len=%d", snapshot.Len())
	}
}

func TestSnapshotDoesNotFreezeStore(t *testing.T) {
	store := mustStore(t, Config{})
	snapshot := mustSnapshot(t, store)
	before := snapshot.Len()

	pointer := mustIntern(t, store, PointerKey(store.Builtins().Int))
	if pointer != TypeID(before+1) {
		t.Fatalf("continued Intern ID = %d, want %d", pointer, before+1)
	}
	if snapshot.Len() != before || snapshot.Contains(pointer) {
		t.Fatalf("captured snapshot changed after Intern: Len=%d Contains(%d)=%v", snapshot.Len(), pointer, snapshot.Contains(pointer))
	}
	second := mustSnapshot(t, store)
	if second.Len() != store.Len() || !second.Contains(pointer) {
		t.Fatal("later snapshot did not capture continued store use")
	}
}

func TestSnapshotRejectsDamagedStoreAtomically(t *testing.T) {
	tests := []struct {
		name   string
		damage func(*Store)
	}{
		{"zero builtin", func(s *Store) { s.builtins.Int = 0 }},
		{"wrong builtin identity", func(s *Store) { s.builtins.Int = s.builtins.Uint }},
		{"damaged builtin key", func(s *Store) { s.entries[s.builtins.Int-1] = BuiltinKey(Uint) }},
		{"unknown key kind", func(s *Store) { s.entries = append(s.entries, TypeKey{kind: Kind(255)}) }},
		{"zero child", func(s *Store) { s.entries = append(s.entries, PointerKey(0)) }},
		{"out of range child", func(s *Store) { s.entries = append(s.entries, PointerKey(TypeID(len(s.entries)+2))) }},
		{"forward child", func(s *Store) {
			s.entries = append(s.entries, PointerKey(TypeID(len(s.entries)+2)))
			s.entries = append(s.entries, TypeParameterKey(symbol.SymbolID(1)))
		}},
		{"invalid tuple", func(s *Store) { s.entries = append(s.entries, TupleKey(nil)) }},
		{"invalid function convention", func(s *Store) {
			s.entries = append(s.entries, FunctionKey(CallingConvention(99), nil, s.builtins.Void, false))
		}},
		{"zero declaration", func(s *Store) { s.entries = append(s.entries, NominalKey(0, nil)) }},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			store := mustStore(t, Config{})
			test.damage(store)
			beforeEntries := cloneKeys(store.entries)
			beforeBuiltins := store.builtins

			snapshot, err := store.Snapshot()
			if snapshot != nil || !errors.Is(err, ErrInvalidKey) {
				t.Fatalf("Snapshot() = %v, %v; want nil, ErrInvalidKey", snapshot, err)
			}
			assertStoreState(t, store, beforeEntries, beforeBuiltins)
		})
	}
}

func TestSnapshotRejectsDuplicateKeysAtomically(t *testing.T) {
	tests := []struct {
		name  string
		setup func(*testing.T, *Store) TypeID
	}{
		{"builtin", func(_ *testing.T, store *Store) TypeID {
			store.entries = append(store.entries, BuiltinKey(Int))
			return TypeID(len(store.entries))
		}},
		{"composite", func(t *testing.T, store *Store) TypeID {
			original := mustIntern(t, store, FunctionKey(
				Pebble,
				[]TypeID{store.builtins.Int, store.builtins.Str},
				store.builtins.Bool,
				false,
			))
			store.entries = append(store.entries, store.entries[original-1].clone())
			return TypeID(len(store.entries))
		}},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			store := mustStore(t, Config{})
			duplicateID := test.setup(t, store)
			beforeEntries := cloneKeys(store.entries)
			beforeBuiltins := store.builtins

			snapshot, err := store.Snapshot()
			if snapshot != nil {
				if snapshot.Contains(duplicateID) {
					t.Fatalf("duplicate TypeID %d was published", duplicateID)
				}
				t.Fatalf("Snapshot() published a partial snapshot without duplicate TypeID %d", duplicateID)
			}
			if !errors.Is(err, ErrInvalidKey) {
				t.Fatalf("Snapshot() error = %v; want ErrInvalidKey", err)
			}
			assertStoreState(t, store, beforeEntries, beforeBuiltins)
		})
	}
}

func TestSnapshotRechargesConfiguredLimitsAtomically(t *testing.T) {
	tests := []struct {
		name   string
		key    func(Builtins) TypeKey
		damage func(*Config)
	}{
		{"max types", func(b Builtins) TypeKey { return PointerKey(b.Int) }, func(c *Config) { c.MaxTypes = builtinCount }},
		{"key components", func(b Builtins) TypeKey {
			return FunctionKey(Pebble, []TypeID{b.Int}, b.Void, false)
		}, func(c *Config) { c.MaxKeyComponents = 1 }},
		{"tuple elements", func(b Builtins) TypeKey { return TupleKey([]TypeID{b.Int, b.Uint}) }, func(c *Config) { c.MaxTupleElements = 1 }},
		{"function parameters", func(b Builtins) TypeKey {
			return FunctionKey(Pebble, []TypeID{b.Int, b.Uint}, b.Void, false)
		}, func(c *Config) { c.MaxFunctionParams = 1 }},
		{"generic arguments", func(b Builtins) TypeKey {
			return NominalKey(symbol.SymbolID(1), []TypeID{b.Int, b.Uint})
		}, func(c *Config) { c.MaxGenericArgs = 1 }},
		{"array length", func(b Builtins) TypeKey { return ArrayKey(5, b.Int) }, func(c *Config) { c.MaxArrayLength = 4 }},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			store := mustStore(t, Config{})
			mustIntern(t, store, test.key(store.Builtins()))
			test.damage(&store.config)
			beforeEntries := cloneKeys(store.entries)
			beforeBuiltins := store.builtins

			snapshot, err := store.Snapshot()
			if snapshot != nil || !errors.Is(err, ErrLimitExceeded) {
				t.Fatalf("Snapshot() = %v, %v; want nil, ErrLimitExceeded", snapshot, err)
			}
			assertStoreState(t, store, beforeEntries, beforeBuiltins)
		})
	}
}

func TestSnapshotChecksLimitsBeforeDamagedChildren(t *testing.T) {
	store := mustStore(t, Config{})
	store.entries = append(store.entries, TupleKey([]TypeID{
		store.builtins.Int,
		TypeID(len(store.entries) + 2),
	}))
	store.config.MaxTupleElements = 1

	snapshot, err := store.Snapshot()
	if snapshot != nil || !errors.Is(err, ErrLimitExceeded) {
		t.Fatalf("Snapshot() = %v, %v; want nil, ErrLimitExceeded", snapshot, err)
	}
}

func TestSnapshotConcurrentReads(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	for _, key := range []TypeKey{
		PointerKey(b.Int),
		ArrayKey(8, b.Char),
		SliceKey(b.U8),
		TupleKey([]TypeID{b.Int, b.Str}),
		OptionalKey(b.Uint),
		FunctionKey(C, []TypeID{b.Int, b.Str}, b.Void, true),
		NominalKey(symbol.SymbolID(50), []TypeID{b.Int}),
		TypeParameterKey(symbol.SymbolID(51)),
	} {
		mustIntern(t, store, key)
	}
	snapshot := mustSnapshot(t, store)
	wantIDs := collectIDs(snapshot.IDs())

	const readers = 32
	const repetitions = 200
	var wait sync.WaitGroup
	wait.Add(readers)
	for range readers {
		go func() {
			defer wait.Done()
			for range repetitions {
				if snapshot.Builtins() != b || snapshot.Len() != uint32(len(wantIDs)) {
					t.Errorf("concurrent scalar accessor mismatch")
					return
				}
				if got := collectIDs(snapshot.IDs()); !slices.Equal(got, wantIDs) {
					t.Errorf("concurrent IDs() = %v, want %v", got, wantIDs)
					return
				}
				for _, id := range wantIDs {
					if !snapshot.Contains(id) {
						t.Errorf("concurrent Contains(%d) = false", id)
						return
					}
					kind, kindOK := snapshot.Kind(id)
					key, keyOK := snapshot.Key(id)
					if !kindOK || !keyOK || kind != key.Kind() {
						t.Errorf("concurrent access mismatch for %d", id)
						return
					}
				}
			}
		}()
	}
	wait.Wait()
}

func TestNilSnapshotAndStoreAccessors(t *testing.T) {
	var store *Store
	if snapshot, err := store.Snapshot(); snapshot != nil || !errors.Is(err, ErrInvalidKey) {
		t.Fatalf("nil Store.Snapshot() = %v, %v", snapshot, err)
	}
	var snapshot *Snapshot
	if snapshot.Builtins() != (Builtins{}) || snapshot.Len() != 0 || snapshot.Contains(1) {
		t.Fatal("nil snapshot scalar accessors must return zero values")
	}
	if kind, ok := snapshot.Kind(1); ok || kind != 0 {
		t.Fatalf("nil Kind(1) = %d, %v", kind, ok)
	}
	if key, ok := snapshot.Key(1); ok || key.Kind() != 0 {
		t.Fatalf("nil Key(1) = %+v, %v", key, ok)
	}
	if got := collectIDs(snapshot.IDs()); len(got) != 0 {
		t.Fatalf("nil IDs() = %v", got)
	}
}

func mustSnapshot(t *testing.T, store *Store) *Snapshot {
	t.Helper()
	snapshot, err := store.Snapshot()
	if err != nil {
		t.Fatalf("Snapshot: %v", err)
	}
	return snapshot
}

func mustSnapshotKey(t *testing.T, snapshot *Snapshot, id TypeID) TypeKey {
	t.Helper()
	key, ok := snapshot.Key(id)
	if !ok {
		t.Fatalf("Snapshot.Key(%d) missing", id)
	}
	return key
}

func cloneKeys(keys []TypeKey) []TypeKey {
	cloned := make([]TypeKey, len(keys))
	for index, key := range keys {
		cloned[index] = key.clone()
	}
	return cloned
}

func assertStoreState(t *testing.T, store *Store, wantEntries []TypeKey, wantBuiltins Builtins) {
	t.Helper()
	if store.builtins != wantBuiltins || len(store.entries) != len(wantEntries) {
		t.Fatalf("Snapshot failure changed store metadata")
	}
	for index := range wantEntries {
		if !equalKeys(store.entries[index], wantEntries[index]) {
			t.Fatalf("Snapshot failure changed store entry %d", index+1)
		}
	}
}
