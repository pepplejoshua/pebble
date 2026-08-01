package types

import (
	"errors"
	"slices"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

func TestSubstituteBareTypeParameter(t *testing.T) {
	store := mustStore(t, Config{})
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))

	result, err := store.Substitute(tp, map[symbol.SymbolID]TypeID{parameter: store.Builtins().I32})
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}
	if result != store.Builtins().I32 {
		t.Fatalf("Substitute = %d, want mapped builtin I32 %d", result, store.Builtins().I32)
	}
}

func TestSubstituteTypeParameterNotInMapIsUnchanged(t *testing.T) {
	store := mustStore(t, Config{})
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))

	result, err := store.Substitute(tp, map[symbol.SymbolID]TypeID{101: store.Builtins().I32})
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}
	if result != tp {
		t.Fatalf("Substitute = %d, want unchanged type parameter %d", result, tp)
	}
}

func TestSubstitutePureBuiltinIsIdentityNoReintern(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	before := store.Len()
	substitutions := map[symbol.SymbolID]TypeID{100: b.I32}

	for _, builtin := range builtinIDs(b) {
		result, err := store.Substitute(builtin, substitutions)
		if err != nil {
			t.Fatalf("Substitute(%d): %v", builtin, err)
		}
		if result != builtin {
			t.Fatalf("Substitute(%d) = %d, want identical %d", builtin, result, builtin)
		}
	}
	if got := store.Len(); got != before {
		t.Fatalf("Substitute over pure builtins changed Len from %d to %d", before, got)
	}
}

func TestSubstituteCompositeWithoutTypeParameterIsIdentityNoReintern(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	substitutions := map[symbol.SymbolID]TypeID{100: b.I32}

	pointer := mustIntern(t, store, PointerKey(b.Int))
	slice := mustIntern(t, store, SliceKey(b.Str))
	tuple := mustIntern(t, store, TupleKey([]TypeID{pointer, slice}))
	function := mustIntern(t, store, FunctionKey(Pebble, []TypeID{pointer}, b.Bool, false))
	nominal := mustIntern(t, store, NominalKey(symbol.SymbolID(10), []TypeID{b.Int}))
	before := store.Len()

	for _, id := range []TypeID{pointer, slice, tuple, function, nominal} {
		result, err := store.Substitute(id, substitutions)
		if err != nil {
			t.Fatalf("Substitute(%d): %v", id, err)
		}
		if result != id {
			t.Fatalf("Substitute(%d) = %d, want identical %d", id, result, id)
		}
	}
	if got := store.Len(); got != before {
		t.Fatalf("Substitute over concrete composites changed Len from %d to %d", before, got)
	}
}

func TestSubstituteSingleChildKinds(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	substitutions := map[symbol.SymbolID]TypeID{parameter: b.I32}

	cases := []struct {
		name   string
		key    TypeKey
		verify func(t *testing.T, result TypeID)
	}{
		{
			name: "pointer",
			key:  PointerKey(tp),
			verify: func(t *testing.T, result TypeID) {
				t.Helper()
				child, ok := mustKey(t, store, result).Child()
				if !ok || child != b.I32 {
					t.Fatalf("pointer child = %d, %v; want I32 %d", child, ok, b.I32)
				}
			},
		},
		{
			name: "slice",
			key:  SliceKey(tp),
			verify: func(t *testing.T, result TypeID) {
				t.Helper()
				child, ok := mustKey(t, store, result).Child()
				if !ok || child != b.I32 {
					t.Fatalf("slice child = %d, %v; want I32 %d", child, ok, b.I32)
				}
			},
		},
		{
			name: "optional",
			key:  OptionalKey(tp),
			verify: func(t *testing.T, result TypeID) {
				t.Helper()
				child, ok := mustKey(t, store, result).Child()
				if !ok || child != b.I32 {
					t.Fatalf("optional child = %d, %v; want I32 %d", child, ok, b.I32)
				}
			},
		},
		{
			name: "array",
			key:  ArrayKey(12, tp),
			verify: func(t *testing.T, result TypeID) {
				t.Helper()
				length, element, ok := mustKey(t, store, result).Array()
				if !ok || length != 12 || element != b.I32 {
					t.Fatalf("array = %d, %d, %v; want length 12, element I32 %d", length, element, ok, b.I32)
				}
			},
		},
	}

	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			original := mustIntern(t, store, test.key)
			result, err := store.Substitute(original, substitutions)
			if err != nil {
				t.Fatalf("Substitute: %v", err)
			}
			if result == original {
				t.Fatal("Substitute returned the original composite; want a rewritten type")
			}
			test.verify(t, result)
		})
	}
}

func TestSubstituteTupleMixedElements(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	substitutions := map[symbol.SymbolID]TypeID{parameter: b.I32}

	original := mustIntern(t, store, TupleKey([]TypeID{b.Bool, tp, b.Str}))
	result, err := store.Substitute(original, substitutions)
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}
	if result == original {
		t.Fatal("Substitute returned the original tuple; want a rewritten type")
	}

	elements, ok := mustKey(t, store, result).Elements()
	if !ok {
		t.Fatal("result is not a tuple")
	}
	want := []TypeID{b.Bool, b.I32, b.Str}
	if !slices.Equal(elements, want) {
		t.Fatalf("tuple elements = %v, want %v", elements, want)
	}
	if elements[0] != b.Bool || elements[2] != b.Str {
		t.Fatal("non-substitutable tuple elements must keep their identical TypeIDs")
	}
}

func TestSubstituteNominalSubstitutesArgumentsOnly(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	substitutions := map[symbol.SymbolID]TypeID{parameter: b.I32}
	declaration := symbol.SymbolID(200)

	original := mustIntern(t, store, NominalKey(declaration, []TypeID{tp, b.Str}))
	result, err := store.Substitute(original, substitutions)
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}
	if result == original {
		t.Fatal("Substitute returned the original nominal; want a rewritten type")
	}

	gotDeclaration, arguments, ok := mustKey(t, store, result).Nominal()
	if !ok {
		t.Fatal("result is not nominal")
	}
	if gotDeclaration != declaration {
		t.Fatalf("nominal declaration = %d, want unchanged %d", gotDeclaration, declaration)
	}
	if !slices.Equal(arguments, []TypeID{b.I32, b.Str}) {
		t.Fatalf("nominal arguments = %v, want [I32, Str]", arguments)
	}
}

func TestSubstituteFunctionParametersAndResult(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameterT := symbol.SymbolID(100)
	parameterU := symbol.SymbolID(101)
	tp := mustIntern(t, store, TypeParameterKey(parameterT))
	up := mustIntern(t, store, TypeParameterKey(parameterU))
	substitutions := map[symbol.SymbolID]TypeID{parameterT: b.I32, parameterU: b.Str}

	original := mustIntern(t, store, FunctionKey(C, []TypeID{tp, b.Bool}, up, true))
	result, err := store.Substitute(original, substitutions)
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}
	if result == original {
		t.Fatal("Substitute returned the original function; want a rewritten type")
	}

	convention, parameters, resultType, variadic, ok := mustKey(t, store, result).Function()
	if !ok {
		t.Fatal("result is not a function type")
	}
	if convention != C || !variadic {
		t.Fatalf("convention/variadic not preserved: convention %d, variadic %v", convention, variadic)
	}
	if !slices.Equal(parameters, []TypeID{b.I32, b.Bool}) {
		t.Fatalf("function parameters = %v, want [I32, Bool]", parameters)
	}
	if resultType != b.Str {
		t.Fatalf("function result = %d, want Str %d", resultType, b.Str)
	}
}

func TestSubstituteNestedCompositeTwoLevelsDeep(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	substitutions := map[symbol.SymbolID]TypeID{parameter: b.I32}
	declaration := symbol.SymbolID(200)

	vec := mustIntern(t, store, NominalKey(declaration, []TypeID{tp}))
	original := mustIntern(t, store, PointerKey(vec))
	result, err := store.Substitute(original, substitutions)
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}
	if result == original {
		t.Fatal("Substitute returned the original composite; want a rewritten type")
	}

	child, ok := mustKey(t, store, result).Child()
	if !ok {
		t.Fatal("result is not a pointer")
	}
	if child == vec {
		t.Fatal("pointer still references the unrewritten nominal")
	}
	gotDeclaration, arguments, ok := mustKey(t, store, child).Nominal()
	if !ok {
		t.Fatal("pointer child is not nominal")
	}
	if gotDeclaration != declaration {
		t.Fatalf("nominal declaration = %d, want unchanged %d", gotDeclaration, declaration)
	}
	if !slices.Equal(arguments, []TypeID{b.I32}) {
		t.Fatalf("nominal arguments = %v, want [I32]", arguments)
	}
}

func TestSubstituteIdempotenceOnConcreteResult(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	substitutions := map[symbol.SymbolID]TypeID{parameter: b.I32}

	original := mustIntern(t, store, PointerKey(tp))
	once, err := store.Substitute(original, substitutions)
	if err != nil {
		t.Fatalf("first Substitute: %v", err)
	}
	before := store.Len()
	twice, err := store.Substitute(once, substitutions)
	if err != nil {
		t.Fatalf("second Substitute: %v", err)
	}
	if twice != once {
		t.Fatalf("second Substitute = %d, want identical %d", twice, once)
	}
	if got := store.Len(); got != before {
		t.Fatalf("idempotent Substitute changed Len from %d to %d", before, got)
	}
}

func TestSubstituteMultipleDistinctParameters(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameterT := symbol.SymbolID(100)
	parameterU := symbol.SymbolID(101)
	tp := mustIntern(t, store, TypeParameterKey(parameterT))
	up := mustIntern(t, store, TypeParameterKey(parameterU))
	substitutions := map[symbol.SymbolID]TypeID{
		parameterT: b.I32,
		parameterU: b.Str,
	}

	original := mustIntern(t, store, TupleKey([]TypeID{tp, up, tp}))
	result, err := store.Substitute(original, substitutions)
	if err != nil {
		t.Fatalf("Substitute: %v", err)
	}

	elements, ok := mustKey(t, store, result).Elements()
	if !ok {
		t.Fatal("result is not a tuple")
	}
	if !slices.Equal(elements, []TypeID{b.I32, b.Str, b.I32}) {
		t.Fatalf("tuple elements = %v, want [I32, Str, I32]", elements)
	}
}

func TestSubstituteRejectsForeignTypeID(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	before := store.Len()

	result, err := store.Substitute(TypeID(before+1), map[symbol.SymbolID]TypeID{100: b.I32})
	if result != 0 || !errors.Is(err, ErrInvalidKey) {
		t.Fatalf("Substitute(foreign id) = %d, %v; want zero, ErrInvalidKey", result, err)
	}
}

func TestSubstituteNilStoreRejected(t *testing.T) {
	var store *Store
	result, err := store.Substitute(1, map[symbol.SymbolID]TypeID{})
	if result != 0 || !errors.Is(err, ErrInvalidKey) {
		t.Fatalf("Substitute(nil store) = %d, %v; want zero, ErrInvalidKey", result, err)
	}
}

func TestSubstituteEmptySubstitutionsReturnsInput(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	pointer := mustIntern(t, store, PointerKey(tp))
	before := store.Len()

	for _, id := range []TypeID{pointer, tp, b.I32} {
		result, err := store.Substitute(id, map[symbol.SymbolID]TypeID{})
		if err != nil {
			t.Fatalf("Substitute(%d): %v", id, err)
		}
		if result != id {
			t.Fatalf("Substitute(%d) with empty map = %d, want identical %d", id, result, id)
		}
	}
	if got := store.Len(); got != before {
		t.Fatalf("empty-map Substitute changed Len from %d to %d", before, got)
	}
}

func TestSubstituteDepthCap(t *testing.T) {
	store := mustStore(t, Config{})
	b := store.Builtins()
	parameter := symbol.SymbolID(100)
	tp := mustIntern(t, store, TypeParameterKey(parameter))
	substitutions := map[symbol.SymbolID]TypeID{parameter: b.I32}

	within := tp
	for range maxSubstituteDepth - 1 {
		within = mustIntern(t, store, PointerKey(within))
	}
	if result, err := store.Substitute(within, substitutions); err != nil || result == within {
		t.Fatalf("Substitute at boundary depth = %d, %v; want success with rewrite", result, err)
	}

	exceeding := tp
	for range maxSubstituteDepth {
		exceeding = mustIntern(t, store, PointerKey(exceeding))
	}
	if _, err := store.Substitute(exceeding, substitutions); !errors.Is(err, ErrLimitExceeded) {
		t.Fatalf("Substitute past depth cap error = %v, want ErrLimitExceeded", err)
	}
}
