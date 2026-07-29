package check

import (
	"fmt"
	"testing"

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
