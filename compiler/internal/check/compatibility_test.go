package check

import (
	"fmt"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// allBuiltinKinds is the canonical ordering of all 16 builtin kinds, matching
// the iota values in types.BuiltinKind. The exhaustive test iterates source
// × destination over this slice.
var allBuiltinKinds = []types.BuiltinKind{
	types.Bool, types.Char, types.Str, types.Void,
	types.Int, types.Uint, types.I8, types.I16, types.I32, types.I64,
	types.U8, types.U16, types.U32, types.U64,
	types.F32, types.F64,
}

// builtinKindNames provides human-readable labels for test output.
var builtinKindNames = map[types.BuiltinKind]string{
	types.Bool: "bool", types.Char: "char", types.Str: "str", types.Void: "void",
	types.Int: "int", types.Uint: "uint",
	types.I8: "i8", types.I16: "i16", types.I32: "i32", types.I64: "i64",
	types.U8: "u8", types.U16: "u16", types.U32: "u32", types.U64: "u64",
	types.F32: "f32", types.F64: "f64",
}

// isInteger reports whether k belongs to the integer family.
func isInteger(k types.BuiltinKind) bool {
	switch k {
	case types.Int, types.Uint,
		types.I8, types.I16, types.I32, types.I64,
		types.U8, types.U16, types.U32, types.U64:
		return true
	}
	return false
}

// isFloat reports whether k is one of the two float builtins.
func isFloat(k types.BuiltinKind) bool {
	return k == types.F32 || k == types.F64
}

// expectedPrimitiveClass is the independent ground-truth table for every
// source × destination primitive pair. It is written by hand from the spec
// rules and is intentionally NOT derived from classifyPrimitive's logic.
func expectedPrimitiveClass(src, dst types.BuiltinKind) compatibilityClass {
	if src == dst {
		return compatibleIdentity
	}
	srcNum := isInteger(src) || isFloat(src)
	dstNum := isInteger(dst) || isFloat(dst)
	if srcNum && dstNum {
		return compatibleExplicit
	}
	if src == types.Char && isInteger(dst) {
		return compatibleExplicit
	}
	return compatibleForbidden
}

func TestClassifyPrimitiveExhaustive(t *testing.T) {
	for _, src := range allBuiltinKinds {
		for _, dst := range allBuiltinKinds {
			t.Run(fmt.Sprintf("%s_to_%s", builtinKindNames[src], builtinKindNames[dst]), func(t *testing.T) {
				srcKey := types.BuiltinKey(src)
				dstKey := types.BuiltinKey(dst)
				got, ok := classifyPrimitive(srcKey, dstKey)
				if !ok {
					t.Fatalf("classifyPrimitive(%s, %s) returned ok=false; want true", builtinKindNames[src], builtinKindNames[dst])
				}
				want := expectedPrimitiveClass(src, dst)
				if got != want {
					t.Errorf("classifyPrimitive(%s, %s) = %d, want %d", builtinKindNames[src], builtinKindNames[dst], got, want)
				}
			})
		}
	}
}

func TestClassifyPrimitiveNonBuiltinReturnsFalse(t *testing.T) {
	pointerKey := types.PointerKey(1)
	builtinKey := types.BuiltinKey(types.I32)

	if _, ok := classifyPrimitive(pointerKey, builtinKey); ok {
		t.Error("classifyPrimitive(pointerKey, builtinKey) returned ok=true; want false")
	}
	if _, ok := classifyPrimitive(builtinKey, pointerKey); ok {
		t.Error("classifyPrimitive(builtinKey, pointerKey) returned ok=true; want false")
	}
	if _, ok := classifyPrimitive(pointerKey, pointerKey); ok {
		t.Error("classifyPrimitive(pointerKey, pointerKey) returned ok=true; want false")
	}

	arrayKey := types.ArrayKey(3, 1)
	if _, ok := classifyPrimitive(arrayKey, builtinKey); ok {
		t.Error("classifyPrimitive(arrayKey, builtinKey) returned ok=true; want false")
	}
	if _, ok := classifyPrimitive(builtinKey, arrayKey); ok {
		t.Error("classifyPrimitive(builtinKey, arrayKey) returned ok=true; want false")
	}
}

type compositeFixture struct {
	snapshot          *infer.SemanticSnapshot
	a, b              types.TypeID
	integer           types.TypeID
	enum, tagged      types.TypeID
	optionalA         types.TypeID
	optionalOptionalA types.TypeID
	ids               map[string]types.TypeID
}

func newCompositeFixture(t *testing.T) compositeFixture {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Color = enum { red, blue };
type Choice = union enum { value i32; };
fn main() void {}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if facts == nil || facts.Program == nil || facts.Session == nil {
		t.Fatal("failed to prepare composite fixture")
	}
	var color, choice symbol.SymbolID
	for _, value := range inputs.Resolution.Symbols.All() {
		if value.Name == "Color" {
			color = value.ID
		}
		if value.Name == "Choice" {
			choice = value.ID
		}
	}
	if color == 0 || choice == 0 {
		t.Fatal("failed to find nominal fixture declarations")
	}
	store := inputs.Types
	intern := func(key types.TypeKey) types.TypeID {
		id, err := store.Intern(key)
		if err != nil {
			t.Fatal(err)
		}
		return id
	}
	a := store.Builtins().I32
	b := store.Builtins().Char
	enum := intern(types.NominalKey(color, nil))
	tagged := intern(types.NominalKey(choice, nil))
	optionalA := intern(types.OptionalKey(a))
	optionalOptionalA := intern(types.OptionalKey(optionalA))
	ids := map[string]types.TypeID{
		"ptrA": intern(types.PointerKey(a)), "ptrA2": intern(types.PointerKey(a)), "ptrB": intern(types.PointerKey(b)),
		"arrayA": intern(types.ArrayKey(3, a)), "arrayB": intern(types.ArrayKey(3, b)),
		"arrayA5": intern(types.ArrayKey(5, a)), "sliceA": intern(types.SliceKey(a)), "sliceB": intern(types.SliceKey(b)),
		"tupleImplicit":             intern(types.TupleKey([]types.TypeID{a, optionalA})),
		"tupleImplicitDestination":  intern(types.TupleKey([]types.TypeID{a, optionalOptionalA})),
		"tupleExplicit":             intern(types.TupleKey([]types.TypeID{a, store.Builtins().F32})),
		"tupleExplicitDestination":  intern(types.TupleKey([]types.TypeID{a, store.Builtins().F64})),
		"tupleForbidden":            intern(types.TupleKey([]types.TypeID{a, store.Builtins().Str})),
		"tupleForbiddenDestination": intern(types.TupleKey([]types.TypeID{a, a})),
		"tupleShort":                intern(types.TupleKey([]types.TypeID{a})),
		"fnPebble":                  intern(types.FunctionKey(types.Pebble, []types.TypeID{a}, a, false)),
		"fnC":                       intern(types.FunctionKey(types.C, []types.TypeID{a}, a, false)),
		"optionalB":                 intern(types.OptionalKey(b)), "optionalEnum": intern(types.OptionalKey(enum)),
	}
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("composite fixture solve failed: %+v", diagnostics.Items())
	}
	snapshot, ok := infer.Snapshot(facts.Program, solution, diagnostics)
	if !ok {
		t.Fatal("failed to build composite semantic snapshot")
	}
	return compositeFixture{snapshot: snapshot, a: a, b: b, integer: store.Builtins().I32, enum: enum, tagged: tagged, optionalA: optionalA, optionalOptionalA: optionalOptionalA, ids: ids}
}

func TestClassifyCompositeMatrix(t *testing.T) {
	f := newCompositeFixture(t)
	s := f.snapshot
	ids := f.ids
	if ids["ptrA"] != ids["ptrA2"] {
		t.Fatal("structurally identical pointers were not interned to one TypeID")
	}
	cases := []struct {
		name     string
		src, dst types.TypeID
		want     compatibilityClass
	}{
		{"identical pointer", ids["ptrA"], ids["ptrA"], compatibleIdentity},
		{"different pointer payload", ids["ptrA"], ids["ptrB"], compatibleExplicit},
		// Pointer to integer is accepted only for pointer-width-or-wider
		// destinations (u64, uint, i64); every narrower integer destination is
		// compatibleForbidden (see isPointerWidthInteger and the classifyComposite
		// rule it gates). "pointer to integer" below uses f.integer, an i32.
		{"pointer to integer", ids["ptrA"], f.integer, compatibleForbidden},
		{"pointer to i32", ids["ptrA"], s.Types().Builtins().I32, compatibleForbidden},
		{"pointer to i8", ids["ptrA"], s.Types().Builtins().I8, compatibleForbidden},
		{"pointer to i16", ids["ptrA"], s.Types().Builtins().I16, compatibleForbidden},
		{"pointer to int", ids["ptrA"], s.Types().Builtins().Int, compatibleForbidden},
		{"pointer to u8", ids["ptrA"], s.Types().Builtins().U8, compatibleForbidden},
		{"pointer to u16", ids["ptrA"], s.Types().Builtins().U16, compatibleForbidden},
		{"pointer to u32", ids["ptrA"], s.Types().Builtins().U32, compatibleForbidden},
		{"pointer to u64", ids["ptrA"], s.Types().Builtins().U64, compatibleExplicit},
		{"pointer to uint", ids["ptrA"], s.Types().Builtins().Uint, compatibleExplicit},
		{"pointer to i64", ids["ptrA"], s.Types().Builtins().I64, compatibleExplicit},
		{"integer to pointer", f.integer, ids["ptrA"], compatibleForbidden},
		{"u32 to pointer", s.Types().Builtins().U32, ids["ptrA"], compatibleForbidden},
		{"different array payload", ids["arrayA"], ids["arrayB"], compatibleForbidden},
		{"different array length", ids["arrayA"], ids["arrayA5"], compatibleForbidden},
		{"different slices", ids["sliceA"], ids["sliceB"], compatibleForbidden},
		{"optional injection", f.a, f.optionalA, compatibleImplicit},
		{"nested optional injection", f.optionalA, f.optionalOptionalA, compatibleImplicit},
		{"optional unwrap", f.optionalA, f.a, compatibleForbidden},
		{"different optional payload", f.optionalA, ids["optionalB"], compatibleForbidden},
		{"enum to integer", f.enum, f.integer, compatibleExplicit},
		{"integer to enum", f.integer, f.enum, compatibleExplicit},
		{"char to integer", f.b, f.integer, compatibleExplicit},
		{"integer to char", f.integer, f.b, compatibleForbidden},
		{"char to u32", f.b, s.Types().Builtins().U32, compatibleExplicit},
		{"char to u64", f.b, s.Types().Builtins().U64, compatibleExplicit},
		{"u32 to char", s.Types().Builtins().U32, f.b, compatibleForbidden},
		{"integer to optional enum", f.integer, ids["optionalEnum"], compatibleExplicit},
		{"integer to tagged union", f.integer, f.tagged, compatibleForbidden},
		{"implicit tuple", ids["tupleImplicit"], ids["tupleImplicitDestination"], compatibleImplicit},
		{"explicit tuple", ids["tupleExplicit"], ids["tupleExplicitDestination"], compatibleExplicit},
		{"forbidden tuple", ids["tupleForbidden"], ids["tupleForbiddenDestination"], compatibleForbidden},
		{"different tuple arity", ids["tupleShort"], ids["tupleImplicit"], compatibleForbidden},
		{"distinct functions", ids["fnPebble"], ids["fnC"], compatibleForbidden},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := classify(s, tc.src, tc.dst); got != tc.want {
				t.Fatalf("classify = %d, want %d", got, tc.want)
			}
		})
	}
	if _, ok := classifyComposite(s, f.integer, s.Types().Builtins().F64); ok {
		t.Fatal("both-Builtin composite classification returned ok=true")
	}
}

func TestCoercionFor(t *testing.T) {
	f := newCompositeFixture(t)
	s := f.snapshot
	ids := f.ids
	builtins := s.Types().Builtins()

	cases := []struct {
		name string
		src  types.TypeID
		dst  types.TypeID
		want coercionKind
	}{
		// 1. compatibleIdentity → coercionNone
		{"identity builtin", f.a, f.a, coercionNone},
		{"identity pointer", ids["ptrA"], ids["ptrA"], coercionNone},
		{"identity array", ids["arrayA"], ids["arrayA"], coercionNone},
		{"identity slice", ids["sliceA"], ids["sliceA"], coercionNone},
		// 2. compatibleForbidden → coercionNone
		{"forbidden builtin", s.Types().Builtins().Bool, f.a, coercionNone},
		{"forbidden pointer", ids["ptrA"], ids["ptrB"], coercionPointerCast},
		{"forbidden array payload", ids["arrayA"], ids["arrayB"], coercionNone},
		{"forbidden different slice", ids["sliceA"], ids["sliceB"], coercionNone},
		// 3. integer cast (different integer builtins)
		{"integer cast i32→u32", f.a, builtins.U32, coercionIntegerCast},
		{"integer cast u32→i32", builtins.U32, f.a, coercionIntegerCast},
		{"integer cast u64→i64", builtins.U64, builtins.I64, coercionIntegerCast},
		// 4. integer↔float and float↔float
		{"integer to float i32→f32", f.a, builtins.F32, coercionIntegerToFloat},
		{"integer to float i64→f64", builtins.I64, builtins.F64, coercionIntegerToFloat},
		{"float to integer f32→i32", builtins.F32, f.a, coercionFloatToInteger},
		{"float to integer f64→i64", builtins.F64, builtins.I64, coercionFloatToInteger},
		{"float cast f32→f64", builtins.F32, builtins.F64, coercionFloatCast},
		{"float cast f64→f32", builtins.F64, builtins.F32, coercionFloatCast},
		// 5. optional injection
		{"optional inject A→?A", f.a, f.optionalA, coercionOptionalInject},
		{"optional inject ?A→??A", f.optionalA, f.optionalOptionalA, coercionOptionalInject},
		// 6. tuple coerce
		{"implicit tuple coerce", ids["tupleImplicit"], ids["tupleImplicitDestination"], coercionTupleCoerce},
		{"explicit tuple coerce", ids["tupleExplicit"], ids["tupleExplicitDestination"], coercionTupleCoerce},
		// 7. enum conversions
		{"enum to integer", f.enum, f.integer, coercionEnumToInteger},
		{"integer to bare enum", f.integer, f.enum, coercionCheckedIntegerToEnum},
		{"integer to optional enum", f.integer, ids["optionalEnum"], coercionOptionalIntegerToEnum},
		// 8. char conversions
		{"char to integer i32", f.b, f.integer, coercionCharToInteger},
		{"char to integer u32", f.b, s.Types().Builtins().U32, coercionCharToInteger},
		{"char to integer u64", f.b, s.Types().Builtins().U64, coercionCharToInteger},
		{"integer to char", f.integer, f.b, coercionNone},
		{"u32 to char", s.Types().Builtins().U32, f.b, coercionNone},
		// 9. pointer conversions — only pointer-width-or-wider integer
		// destinations (u64, uint, i64) yield coercionPointerToInteger; narrower
		// destinations classify compatibleForbidden, which coercionFor maps to
		// coercionNone.
		{"pointer to integer i32", ids["ptrA"], f.integer, coercionNone},
		{"pointer to i8", ids["ptrA"], builtins.I8, coercionNone},
		{"pointer to i16", ids["ptrA"], builtins.I16, coercionNone},
		{"pointer to int", ids["ptrA"], builtins.Int, coercionNone},
		{"pointer to u8", ids["ptrA"], builtins.U8, coercionNone},
		{"pointer to u16", ids["ptrA"], builtins.U16, coercionNone},
		{"pointer to u32", ids["ptrA"], builtins.U32, coercionNone},
		{"pointer to u64", ids["ptrA"], s.Types().Builtins().U64, coercionPointerToInteger},
		{"pointer to uint", ids["ptrA"], builtins.Uint, coercionPointerToInteger},
		{"pointer to i64", ids["ptrA"], builtins.I64, coercionPointerToInteger},
		{"integer to pointer", f.integer, ids["ptrA"], coercionNone},
		{"u64 to pointer", builtins.U64, ids["ptrA"], coercionNone},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			class := classify(s, tc.src, tc.dst)
			got := coercionFor(s, class, tc.src, tc.dst)
			if got != tc.want {
				t.Errorf("coercionFor(classify=%d, ...) = %d, want %d", class, got, tc.want)
			}
		})
	}
}
