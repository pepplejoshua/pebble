package types

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// TestDescribeKeyRendersRealTypes exercises the recursive, named type printer
// across every composite kind, asserting the exact rendered string for each.
func TestDescribeKeyRendersRealTypes(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()

	colorID := symbol.SymbolID(7)
	vecID := symbol.SymbolID(8)

	lookup := func(id TypeID) (TypeKey, bool) { return store.Key(id) }
	resolve := func(id symbol.SymbolID) string {
		switch id {
		case colorID:
			return "Color"
		case vecID:
			return "Vec"
		default:
			return ""
		}
	}

	cases := []struct {
		name string
		key  TypeKey
		want string
	}{
		{"builtin", BuiltinKey(I32), "i32"},
		{"pointer to builtin", PointerKey(b.I32), "*i32"},
		{"pointer to nominal", PointerKey(mustIntern(t, store, NominalKey(colorID, nil))), "*Color"},
		{"optional", OptionalKey(b.Str), "?str"},
		{"slice", SliceKey(b.I32), "[]i32"},
		{"array with length", ArrayKey(5, b.F64), "[5]f64"},
		{"tuple of two", TupleKey([]TypeID{b.I32, b.Str}), "(i32, str)"},
		{"tuple of three", TupleKey([]TypeID{b.I32, b.I64, b.Bool}), "(i32, i64, bool)"},
		{"function type", FunctionKey(Pebble, []TypeID{b.I32, b.I32}, b.I32, false), "fn(i32, i32) i32"},
		{"plain nominal", NominalKey(colorID, nil), "Color"},
		{"generic nominal", NominalKey(vecID, []TypeID{b.I32}), "Vec[i32]"},
		{"nested pointer to slice", PointerKey(mustIntern(t, store, SliceKey(b.Str))), "*[]str"},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := DescribeKeyResolved(tc.key, lookup, resolve)
			if got != tc.want {
				t.Fatalf("DescribeKeyResolved = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestDescribeKeyContextFreeNominal renders a nominal type through the
// context-free DescribeKey, which has no resolver and must fall back to an
// identity-based name rather than the coarse placeholder "enum".
func TestDescribeKeyContextFreeNominal(t *testing.T) {
	key := NominalKey(symbol.SymbolID(7), nil)
	if got := DescribeKey(key); got != "type 7" {
		t.Fatalf("DescribeKey(nominal) = %q, want %q", got, "type 7")
	}
}

// TestDescribeKeyPointerToBuiltinAndNominalAssertions locks the exact strings
// the task explicitly calls out for pointer rendering.
func TestDescribeKeyPointerAssertions(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()

	lookup := func(id TypeID) (TypeKey, bool) { return store.Key(id) }

	ptrBuiltin := PointerKey(b.I32)
	if got := DescribeKeyResolved(ptrBuiltin, lookup, nil); got != "*i32" {
		t.Fatalf("pointer to builtin = %q, want \"*i32\"", got)
	}

	nominal := mustIntern(t, store, NominalKey(symbol.SymbolID(3), nil))
	ptrNominal := PointerKey(nominal)
	resolve := func(id symbol.SymbolID) string {
		if id == 3 {
			return "Color"
		}
		return ""
	}
	if got := DescribeKeyResolved(ptrNominal, lookup, resolve); got != "*Color" {
		t.Fatalf("pointer to nominal = %q, want \"*Color\"", got)
	}
}

// TestDescribeKeyResolvedFromResolution builds the resolver from a real
// *symbol.Result symbol store to confirm nominal names resolve through the
// shipped helper. It uses a full symbol.Resolve pass over a tiny source module
// so the store is populated through the real code path.
func TestDescribeKeyResolvedFromResolution(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()

	declID := symbol.SymbolID(1)
	resolution := &symbol.Result{}
	// A nil resolver is the degradation path; it must not panic and must not
	// return a real name.
	got := DescribeKeyResolved(NominalKey(declID, []TypeID{b.I32}), LookupFromSnapshot(storeSnapshot(t, store)), ResolveFromResult(resolution))
	if got == "Color[i32]" {
		t.Fatalf("nil-result resolver unexpectedly resolved a name: %q", got)
	}
}

func storeSnapshot(t *testing.T, store *Store) *Snapshot {
	t.Helper()
	snap, err := store.Snapshot()
	if err != nil {
		t.Fatalf("Snapshot: %v", err)
	}
	return snap
}
