package types

import (
	"slices"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

func TestTypeKeyConstructorsAndAccessors(t *testing.T) {
	if got := BuiltinKey(Bool).Kind(); got != Builtin {
		t.Fatalf("BuiltinKey kind = %d, want Builtin", got)
	}
	if got, ok := BuiltinKey(Bool).Builtin(); !ok || got != Bool {
		t.Fatalf("Builtin() = %d, %v; want Bool", got, ok)
	}

	for _, key := range []TypeKey{PointerKey(1), SliceKey(1), OptionalKey(1)} {
		if got, ok := key.Child(); !ok || got != 1 {
			t.Fatalf("Child() = %d, %v; want 1", got, ok)
		}
	}
	if length, element, ok := ArrayKey(7, 2).Array(); !ok || length != 7 || element != 2 {
		t.Fatalf("Array() = %d, %d, %v; want 7, 2, true", length, element, ok)
	}

	elements := []TypeID{1, 2}
	tuple := TupleKey(elements)
	elements[0] = 9
	gotElements, ok := tuple.Elements()
	if !ok || !slices.Equal(gotElements, []TypeID{1, 2}) {
		t.Fatalf("Elements() = %v, %v; constructor did not copy", gotElements, ok)
	}
	gotElements[0] = 8
	gotElements, _ = tuple.Elements()
	if !slices.Equal(gotElements, []TypeID{1, 2}) {
		t.Fatalf("Elements() = %v after result mutation; accessor did not copy", gotElements)
	}

	parameters := []TypeID{2, 3}
	function := FunctionKey(C, parameters, 4, true)
	parameters[0] = 9
	convention, gotParameters, result, variadic, ok := function.Function()
	if !ok || convention != C || !slices.Equal(gotParameters, []TypeID{2, 3}) ||
		result != 4 || !variadic {
		t.Fatalf("Function() = %d, %v, %d, %v, %v", convention, gotParameters, result, variadic, ok)
	}
	gotParameters[0] = 8
	_, gotParameters, _, _, _ = function.Function()
	if !slices.Equal(gotParameters, []TypeID{2, 3}) {
		t.Fatalf("Function parameters changed through accessor: %v", gotParameters)
	}

	arguments := []TypeID{5, 6}
	nominal := NominalKey(symbol.SymbolID(7), arguments)
	arguments[0] = 9
	declaration, gotArguments, ok := nominal.Nominal()
	if !ok || declaration != 7 || !slices.Equal(gotArguments, []TypeID{5, 6}) {
		t.Fatalf("Nominal() = %d, %v, %v", declaration, gotArguments, ok)
	}
	gotArguments[0] = 8
	_, gotArguments, _ = nominal.Nominal()
	if !slices.Equal(gotArguments, []TypeID{5, 6}) {
		t.Fatalf("nominal arguments changed through accessor: %v", gotArguments)
	}

	parameter := TypeParameterKey(symbol.SymbolID(11))
	if declaration, ok := parameter.TypeParameter(); !ok || declaration != 11 {
		t.Fatalf("TypeParameter() = %d, %v; want 11, true", declaration, ok)
	}
}

func TestMismatchedTypeKeyAccessorsReturnZero(t *testing.T) {
	key := BuiltinKey(Bool)
	if child, ok := key.Child(); ok || child != 0 {
		t.Fatalf("Child() = %d, %v", child, ok)
	}
	if length, element, ok := key.Array(); ok || length != 0 || element != 0 {
		t.Fatalf("Array() = %d, %d, %v", length, element, ok)
	}
	if elements, ok := key.Elements(); ok || elements != nil {
		t.Fatalf("Elements() = %v, %v", elements, ok)
	}
	if convention, parameters, result, variadic, ok := key.Function(); ok || convention != 0 || parameters != nil || result != 0 || variadic {
		t.Fatalf("Function() = %d, %v, %d, %v, %v", convention, parameters, result, variadic, ok)
	}
	if declaration, arguments, ok := key.Nominal(); ok || declaration != 0 || arguments != nil {
		t.Fatalf("Nominal() = %d, %v, %v", declaration, arguments, ok)
	}
	if declaration, ok := key.TypeParameter(); ok || declaration != 0 {
		t.Fatalf("TypeParameter() = %d, %v", declaration, ok)
	}
	if builtin, ok := PointerKey(1).Builtin(); ok || builtin != 0 {
		t.Fatalf("Builtin() = %d, %v", builtin, ok)
	}
}
