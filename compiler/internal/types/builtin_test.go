package types

import (
	"errors"
	"slices"
	"testing"
)

func TestBuiltinsArePreinternedCanonicalAndDistinct(t *testing.T) {
	store := mustStore(t, Config{})
	builtins := store.Builtins()
	want := []struct {
		kind BuiltinKind
		id   TypeID
	}{
		{Bool, builtins.Bool}, {Char, builtins.Char},
		{Str, builtins.Str}, {Void, builtins.Void},
		{Int, builtins.Int}, {Uint, builtins.Uint},
		{I8, builtins.I8}, {I16, builtins.I16},
		{I32, builtins.I32}, {I64, builtins.I64},
		{U8, builtins.U8}, {U16, builtins.U16},
		{U32, builtins.U32}, {U64, builtins.U64},
		{F32, builtins.F32}, {F64, builtins.F64},
	}

	if got := store.Len(); got != builtinCount {
		t.Fatalf("Len() = %d, want %d builtins", got, builtinCount)
	}
	seen := make(map[TypeID]bool, len(want))
	for _, item := range want {
		if !item.id.IsValid() {
			t.Fatalf("builtin %d has invalid ID", item.kind)
		}
		if seen[item.id] {
			t.Fatalf("builtin %d reuses ID %d", item.kind, item.id)
		}
		seen[item.id] = true

		kind, ok := store.Kind(item.id)
		if !ok || kind != Builtin {
			t.Fatalf("Kind(%d) = %d, %v; want Builtin", item.id, kind, ok)
		}
		key, ok := store.Key(item.id)
		if !ok {
			t.Fatalf("Key(%d) missing", item.id)
		}
		gotBuiltin, ok := key.Builtin()
		if !ok || gotBuiltin != item.kind {
			t.Fatalf("Builtin() = %d, %v; want %d", gotBuiltin, ok, item.kind)
		}
		repeated, err := store.Intern(BuiltinKey(item.kind))
		if err != nil || repeated != item.id {
			t.Fatalf("reintern builtin %d = %d, %v; want %d", item.kind, repeated, err, item.id)
		}
	}

	if builtins.Int == builtins.I64 || builtins.Uint == builtins.U64 {
		t.Fatal("target-word builtins must differ from exact-width builtins")
	}
	if got := collectIDs(store.IDs()); !slices.Equal(got, builtinIDs(builtins)) {
		t.Fatalf("IDs() = %v, want fixed builtin order %v", got, builtinIDs(builtins))
	}
}

func TestNoAdditionalBuiltinOrErrorType(t *testing.T) {
	store := mustStore(t, Config{})
	before := store.Len()
	_, err := store.Intern(BuiltinKey(F64 + 1))
	if !errors.Is(err, ErrInvalidKey) {
		t.Fatalf("Intern(unknown builtin) error = %v, want ErrInvalidKey", err)
	}
	if got := store.Len(); got != before {
		t.Fatalf("invalid builtin changed Len from %d to %d", before, got)
	}
}

func builtinIDs(b Builtins) []TypeID {
	return []TypeID{
		b.Bool, b.Char, b.Str, b.Void,
		b.Int, b.Uint,
		b.I8, b.I16, b.I32, b.I64,
		b.U8, b.U16, b.U32, b.U64,
		b.F32, b.F64,
	}
}
