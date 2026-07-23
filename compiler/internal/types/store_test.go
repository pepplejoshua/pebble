package types

import (
	"errors"
	"slices"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

func TestStructuralInterningAndNestedDecomposition(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()

	pointer := mustIntern(t, store, PointerKey(b.Int))
	array := mustIntern(t, store, ArrayKey(12, pointer))
	slice := mustIntern(t, store, SliceKey(b.Uint))
	tuple := mustIntern(t, store, TupleKey([]TypeID{array, slice}))
	optional := mustIntern(t, store, OptionalKey(tuple))
	functionKey := FunctionKey(Pebble, []TypeID{pointer, optional}, b.Bool, false)
	function := mustIntern(t, store, functionKey)

	for _, item := range []struct {
		id  TypeID
		key TypeKey
	}{
		{pointer, PointerKey(b.Int)},
		{array, ArrayKey(12, pointer)},
		{slice, SliceKey(b.Uint)},
		{tuple, TupleKey([]TypeID{array, slice})},
		{optional, OptionalKey(tuple)},
		{function, functionKey},
	} {
		if repeated := mustIntern(t, store, item.key); repeated != item.id {
			t.Fatalf("repeated Intern(%d) = %d, want %d", item.key.Kind(), repeated, item.id)
		}
	}

	key, ok := store.Key(function)
	if !ok {
		t.Fatal("function key missing")
	}
	convention, parameters, result, variadic, ok := key.Function()
	if !ok || convention != Pebble || !slices.Equal(parameters, []TypeID{pointer, optional}) ||
		result != b.Bool || variadic {
		t.Fatalf("function decomposition = %d, %v, %d, %v, %v", convention, parameters, result, variadic, ok)
	}
}

func TestEveryIdentityComponentDistinguishesKeys(t *testing.T) {
	store, err := newWithHash(Config{}, func(TypeKey) uint64 { return 1 })
	if err != nil {
		t.Fatalf("newWithHash: %v", err)
	}
	b := store.Builtins()

	groups := [][]TypeKey{
		{PointerKey(b.Int), PointerKey(b.Uint)},
		{ArrayKey(1, b.Int), ArrayKey(2, b.Int), ArrayKey(1, b.Uint)},
		{SliceKey(b.Int), SliceKey(b.Uint)},
		{
			TupleKey([]TypeID{b.Int}),
			TupleKey([]TypeID{b.Int, b.Uint}),
			TupleKey([]TypeID{b.Uint, b.Int}),
		},
		{OptionalKey(b.Int), OptionalKey(b.Uint)},
		{
			FunctionKey(Pebble, []TypeID{b.Int}, b.Bool, false),
			FunctionKey(Pebble, []TypeID{b.Int, b.Uint}, b.Bool, false),
			FunctionKey(C, []TypeID{b.Int}, b.Bool, false),
			FunctionKey(Pebble, []TypeID{b.Int}, b.Bool, true),
			FunctionKey(Pebble, []TypeID{b.Uint}, b.Bool, false),
			FunctionKey(Pebble, []TypeID{b.Int}, b.Char, false),
		},
		{
			NominalKey(symbol.SymbolID(1), nil),
			NominalKey(symbol.SymbolID(1), []TypeID{b.Int, b.Uint}),
			NominalKey(symbol.SymbolID(2), []TypeID{b.Int, b.Uint}),
			NominalKey(symbol.SymbolID(1), []TypeID{b.Uint, b.Int}),
		},
		{TypeParameterKey(symbol.SymbolID(3)), TypeParameterKey(symbol.SymbolID(4))},
	}

	seen := make(map[TypeID]bool)
	for _, id := range builtinIDs(b) {
		if seen[id] {
			t.Fatalf("colliding builtins reused ID %d", id)
		}
		seen[id] = true
	}
	for _, group := range groups {
		for _, key := range group {
			id := mustIntern(t, store, key)
			if seen[id] {
				t.Fatalf("different key %+v reused ID %d", key, id)
			}
			seen[id] = true
		}
	}
}

func TestNominalRecursiveAndGenericIdentity(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	declaration := symbol.SymbolID(20)

	nominal := mustIntern(t, store, NominalKey(declaration, nil))
	pointer := mustIntern(t, store, PointerKey(nominal))
	if repeated := mustIntern(t, store, NominalKey(declaration, []TypeID{})); repeated != nominal {
		t.Fatalf("recursive nominal changed from %d to %d", nominal, repeated)
	}
	if child, ok := mustKey(t, store, pointer).Child(); !ok || child != nominal {
		t.Fatalf("recursive pointer child = %d, %v; want %d", child, ok, nominal)
	}

	application := mustIntern(t, store, NominalKey(declaration, []TypeID{b.Int, b.Str}))
	decl, arguments, ok := mustKey(t, store, application).Nominal()
	if !ok || decl != declaration || !slices.Equal(arguments, []TypeID{b.Int, b.Str}) {
		t.Fatalf("generic nominal = %d, %v, %v", decl, arguments, ok)
	}
	if application == nominal {
		t.Fatal("generic application must differ from unapplied nominal")
	}

	parameter := mustIntern(t, store, TypeParameterKey(symbol.SymbolID(21)))
	if decl, ok := mustKey(t, store, parameter).TypeParameter(); !ok || decl != 21 {
		t.Fatalf("type parameter = %d, %v", decl, ok)
	}
}

func TestEmptyFunctionParametersAndNominalArgumentsAreValid(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()

	function := mustIntern(t, store, FunctionKey(Pebble, nil, b.Void, false))
	_, parameters, _, _, ok := mustKey(t, store, function).Function()
	if !ok || len(parameters) != 0 {
		t.Fatalf("empty function parameters = %v, %v", parameters, ok)
	}
	nominal := mustIntern(t, store, NominalKey(symbol.SymbolID(1), nil))
	_, arguments, ok := mustKey(t, store, nominal).Nominal()
	if !ok || len(arguments) != 0 {
		t.Fatalf("empty nominal arguments = %v, %v", arguments, ok)
	}
}

func TestEmptyTupleIsRejectedAtomically(t *testing.T) {
	store := mustStore(t, Config{})
	before := store.Len()
	_, err := store.Intern(TupleKey(nil))
	if !errors.Is(err, ErrInvalidKey) {
		t.Fatalf("Intern(TupleKey(nil)) error = %v, want ErrInvalidKey", err)
	}
	if got := store.Len(); got != before {
		t.Fatalf("empty tuple changed Len from %d to %d", before, got)
	}
	if id := mustIntern(t, store, TupleKey([]TypeID{store.Builtins().Int})); !id.IsValid() {
		t.Fatal("one-element tuple must be valid")
	}
}

func TestForcedHashCollisionsUseCompleteEquality(t *testing.T) {
	store, err := newWithHash(Config{}, func(TypeKey) uint64 { return 1 })
	if err != nil {
		t.Fatalf("newWithHash: %v", err)
	}
	b := store.Builtins()
	if b.Bool == b.Char || b.Int == b.Uint {
		t.Fatal("colliding builtins must remain distinct")
	}

	intPointer := mustIntern(t, store, PointerKey(b.Int))
	uintPointer := mustIntern(t, store, PointerKey(b.Uint))
	if intPointer == uintPointer {
		t.Fatal("colliding unequal pointer keys reused an ID")
	}
	if repeated := mustIntern(t, store, PointerKey(b.Int)); repeated != intPointer {
		t.Fatalf("colliding equal pointer key = %d, want %d", repeated, intPointer)
	}
}

func TestStoreDefensivelyCopiesKeys(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameters := []TypeID{b.Int, b.Uint}
	key := FunctionKey(Pebble, parameters, b.Bool, false)
	parameters[0] = b.Char
	id := mustIntern(t, store, key)

	returned, ok := store.Key(id)
	if !ok {
		t.Fatal("stored key missing")
	}
	returned.elements[0] = b.Char
	_, got, _, _, _ := mustKey(t, store, id).Function()
	if !slices.Equal(got, []TypeID{b.Int, b.Uint}) {
		t.Fatalf("stored key mutated through Key result: %v", got)
	}
	got[0] = b.Char
	_, got, _, _, _ = mustKey(t, store, id).Function()
	if !slices.Equal(got, []TypeID{b.Int, b.Uint}) {
		t.Fatalf("stored key mutated through Function result: %v", got)
	}
}

func TestInvalidKeysAreRejectedAtomically(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	badPointer := PointerKey(b.Int)
	badPointer.length = 1

	cases := []TypeKey{
		{},
		{kind: Kind(255)},
		BuiltinKey(BuiltinKind(255)),
		PointerKey(0),
		PointerKey(TypeID(store.Len() + 100)),
		TupleKey(nil),
		TupleKey([]TypeID{0}),
		FunctionKey(CallingConvention(255), nil, b.Void, false),
		FunctionKey(Pebble, []TypeID{0}, b.Void, false),
		FunctionKey(Pebble, nil, 0, false),
		NominalKey(0, nil),
		NominalKey(symbol.SymbolID(1), []TypeID{0}),
		TypeParameterKey(0),
		badPointer,
	}

	for index, key := range cases {
		before := store.Len()
		id, err := store.Intern(key)
		if id != 0 || !errors.Is(err, ErrInvalidKey) {
			t.Fatalf("case %d: Intern = %d, %v; want zero, ErrInvalidKey", index, id, err)
		}
		if got := store.Len(); got != before {
			t.Fatalf("case %d: invalid key changed Len from %d to %d", index, before, got)
		}
	}

	if _, ok := store.Key(0); ok {
		t.Fatal("Key(0) must fail")
	}
	if _, ok := store.Kind(TypeID(store.Len() + 1)); ok {
		t.Fatal("Kind(out of range) must fail")
	}
}

func TestResourceLimitsAreAtomic(t *testing.T) {
	t.Run("new requires builtin capacity", func(t *testing.T) {
		store, err := New(Config{MaxTypes: builtinCount - 1})
		if store != nil || !errors.Is(err, ErrLimitExceeded) {
			t.Fatalf("New = %v, %v; want nil, ErrLimitExceeded", store, err)
		}
	})

	t.Run("types", func(t *testing.T) {
		store := mustStore(t, Config{MaxTypes: builtinCount})
		before := store.Len()
		if repeated, err := store.Intern(BuiltinKey(Int)); err != nil || repeated != store.Builtins().Int {
			t.Fatalf("existing builtin at limit = %d, %v", repeated, err)
		}
		assertLimit(t, store, before, PointerKey(store.Builtins().Int))
	})

	t.Run("key components", func(t *testing.T) {
		store := mustStore(t, Config{MaxKeyComponents: 1})
		b := store.Builtins()
		assertLimit(t, store, store.Len(), FunctionKey(Pebble, []TypeID{b.Int}, b.Void, false))
	})

	t.Run("tuple elements", func(t *testing.T) {
		store := mustStore(t, Config{MaxTupleElements: 1})
		b := store.Builtins()
		assertLimit(t, store, store.Len(), TupleKey([]TypeID{b.Int, b.Uint}))
	})

	t.Run("function parameters", func(t *testing.T) {
		store := mustStore(t, Config{MaxFunctionParams: 1})
		b := store.Builtins()
		assertLimit(t, store, store.Len(), FunctionKey(Pebble, []TypeID{b.Int, b.Uint}, b.Void, false))
	})

	t.Run("generic arguments", func(t *testing.T) {
		store := mustStore(t, Config{MaxGenericArgs: 1})
		b := store.Builtins()
		assertLimit(t, store, store.Len(), NominalKey(symbol.SymbolID(1), []TypeID{b.Int, b.Uint}))
	})

	t.Run("array length", func(t *testing.T) {
		store := mustStore(t, Config{MaxArrayLength: 4})
		assertLimit(t, store, store.Len(), ArrayKey(5, store.Builtins().Int))
	})
}

func TestDeterministicIDsForIdenticalOrderedCalls(t *testing.T) {
	for _, test := range []struct {
		name string
		new  func() (*Store, error)
	}{
		{"default hash", func() (*Store, error) { return New(Config{}) }},
		{"forced collisions", func() (*Store, error) {
			return newWithHash(Config{}, func(TypeKey) uint64 { return 1 })
		}},
	} {
		t.Run(test.name, func(t *testing.T) {
			first, err := test.new()
			if err != nil {
				t.Fatalf("first store: %v", err)
			}
			second, err := test.new()
			if err != nil {
				t.Fatalf("second store: %v", err)
			}

			firstIDs := internDeterministicSequence(t, first)
			secondIDs := internDeterministicSequence(t, second)
			if !slices.Equal(firstIDs, secondIDs) {
				t.Fatalf("call-stream IDs differ: %v versus %v", firstIDs, secondIDs)
			}
			if got, want := collectIDs(first.IDs()), collectIDs(second.IDs()); !slices.Equal(got, want) {
				t.Fatalf("IDs order differs: %v versus %v", got, want)
			}
		})
	}
}

func internDeterministicSequence(t *testing.T, store *Store) []TypeID {
	t.Helper()
	b := store.Builtins()
	keys := []TypeKey{
		PointerKey(b.Int),
		ArrayKey(8, b.Char),
		SliceKey(b.U8),
		TupleKey([]TypeID{b.Int, b.Str}),
		OptionalKey(b.Uint),
		FunctionKey(C, []TypeID{b.Int, b.Str}, b.Void, true),
		NominalKey(symbol.SymbolID(50), []TypeID{b.Int}),
		TypeParameterKey(symbol.SymbolID(51)),
	}
	ids := make([]TypeID, len(keys))
	for index, key := range keys {
		ids[index] = mustIntern(t, store, key)
	}
	return ids
}

func mustStore(t *testing.T, config Config) *Store {
	t.Helper()
	store, err := New(config)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return store
}

func mustIntern(t *testing.T, store *Store, key TypeKey) TypeID {
	t.Helper()
	id, err := store.Intern(key)
	if err != nil {
		t.Fatalf("Intern(%d): %v", key.Kind(), err)
	}
	return id
}

func mustKey(t *testing.T, store *Store, id TypeID) TypeKey {
	t.Helper()
	key, ok := store.Key(id)
	if !ok {
		t.Fatalf("Key(%d) missing", id)
	}
	return key
}

func assertLimit(t *testing.T, store *Store, before uint32, key TypeKey) {
	t.Helper()
	id, err := store.Intern(key)
	if id != 0 || !errors.Is(err, ErrLimitExceeded) {
		t.Fatalf("Intern(limit key) = %d, %v; want zero, ErrLimitExceeded", id, err)
	}
	if got := store.Len(); got != before {
		t.Fatalf("limit failure changed Len from %d to %d", before, got)
	}
}

func collectIDs(sequence func(func(TypeID) bool)) []TypeID {
	var ids []TypeID
	sequence(func(id TypeID) bool {
		ids = append(ids, id)
		return true
	})
	return ids
}
