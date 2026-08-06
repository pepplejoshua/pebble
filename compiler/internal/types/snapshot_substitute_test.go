package types

import (
	"errors"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

func snapshotOf(t *testing.T, store *Store) *Snapshot {
	t.Helper()
	snapshot, err := store.Snapshot()
	if err != nil {
		t.Fatalf("Snapshot: %v", err)
	}
	return snapshot
}

func TestSnapshotSubstituteBareTypeParameter(t *testing.T) {
	store := mustStore(t, Config{})
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	snapshot := snapshotOf(t, store)

	result, err := snapshot.Substitute(tp, map[symbol.SymbolID]TypeID{parameter: store.Builtins().I32})
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}
	if result != store.Builtins().I32 {
		t.Fatalf("Substitute = %d, want mapped builtin I32 %d", result, store.Builtins().I32)
	}
}

func TestSnapshotSubstituteTypeParameterNotInMapIsUnchanged(t *testing.T) {
	store := mustStore(t, Config{})
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	snapshot := snapshotOf(t, store)

	result, err := snapshot.Substitute(tp, map[symbol.SymbolID]TypeID{101: store.Builtins().I32})
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}
	if result != tp {
		t.Fatalf("Substitute = %d, want unchanged type parameter %d", result, tp)
	}
}

func TestSnapshotSubstituteConcreteIsIdentity(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	substitutions := map[symbol.SymbolID]TypeID{100: b.I32}
	pointer := mustIntern(t, store, PointerKey(b.Int))
	nominal := mustIntern(t, store, NominalKey(symbol.SymbolID(10), []TypeID{b.Int}))
	snapshot := snapshotOf(t, store)

	for _, id := range []TypeID{b.Bool, pointer, nominal} {
		result, err := snapshot.Substitute(id, substitutions)
		if err != nil {
			t.Fatalf("Substitute(%d): %v", id, err)
		}
		if result != id {
			t.Fatalf("Substitute(%d) = %d, want identical %d", id, result, id)
		}
	}
}

func TestSnapshotSubstituteCompositeUsesInternedRewrite(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	// Intern the concrete pointer *i32 first so the rewrite is snapshot-resident.
	concrete := mustIntern(t, store, PointerKey(b.I32))
	original := mustIntern(t, store, PointerKey(tp))
	snapshot := snapshotOf(t, store)

	result, err := snapshot.Substitute(original, map[symbol.SymbolID]TypeID{parameter: b.I32})
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}
	if result != concrete {
		t.Fatalf("Substitute = %d, want already-interned rewrite %d", result, concrete)
	}
}

func TestSnapshotSubstituteCompositeNotInternedRejected(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	// Deliberately do NOT intern *i32, so the rewrite has no snapshot entry.
	original := mustIntern(t, store, PointerKey(tp))
	snapshot := snapshotOf(t, store)

	result, err := snapshot.Substitute(original, map[symbol.SymbolID]TypeID{parameter: b.I32})
	if result != 0 || !errors.Is(err, ErrInvalidKey) {
		t.Fatalf("Substitute = %d, %v; want zero, ErrInvalidKey for a non-interned rewrite", result, err)
	}
}

func TestSnapshotSubstituteRejectsForeignTypeID(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	snapshot := snapshotOf(t, store)

	result, err := snapshot.Substitute(TypeID(snapshot.Len()+1), map[symbol.SymbolID]TypeID{100: b.I32})
	if result != 0 || !errors.Is(err, ErrInvalidKey) {
		t.Fatalf("Substitute(foreign id) = %d, %v; want zero, ErrInvalidKey", result, err)
	}
}

func TestSnapshotSubstituteNilSnapshotRejected(t *testing.T) {
	var snapshot *Snapshot
	result, err := snapshot.Substitute(1, map[symbol.SymbolID]TypeID{})
	if result != 0 || !errors.Is(err, ErrInvalidKey) {
		t.Fatalf("Substitute(nil snapshot) = %d, %v; want zero, ErrInvalidKey", result, err)
	}
}

func TestSnapshotSubstituteEmptySubstitutionsReturnsInput(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	pointer := mustIntern(t, store, PointerKey(tp))
	snapshot := snapshotOf(t, store)

	for _, id := range []TypeID{pointer, tp, b.I32} {
		result, err := snapshot.Substitute(id, map[symbol.SymbolID]TypeID{})
		if err != nil {
			t.Fatalf("Substitute(%d): %v", id, err)
		}
		if result != id {
			t.Fatalf("Substitute(%d) with empty map = %d, want identical %d", id, result, id)
		}
	}
}
