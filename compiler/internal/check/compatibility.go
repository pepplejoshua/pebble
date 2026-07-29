package check

import "github.com/pepplejoshua/pebble/compiler/internal/types"

// compatibilityClass describes the conversion legality between a source and
// destination type pair.
type compatibilityClass uint8

const (
	// compatibleIdentity means no coercion is emitted; source and destination
	// are the same type.
	compatibleIdentity compatibilityClass = iota + 1
	// compatibleImplicit means a typed-IR coercion is emitted at a retained
	// assignment, fixed argument, return, record field, tuple component,
	// optional payload, or accepted branch boundary.
	compatibleImplicit
	// compatibleExplicit means conversion is accepted only by a retained
	// castRecord.
	compatibleExplicit
	// compatibleForbidden means conversion emits C0601.
	compatibleForbidden
)

// isIntegerBuiltin reports whether k belongs to the integer family of
// builtins: Int, Uint, I8, I16, I32, I64, U8, U16, U32, U64.
func isIntegerBuiltin(k types.BuiltinKind) bool {
	switch k {
	case types.Int, types.Uint,
		types.I8, types.I16, types.I32, types.I64,
		types.U8, types.U16, types.U32, types.U64:
		return true
	}
	return false
}

// isFloatBuiltin reports whether k is one of the two float builtins: F32, F64.
func isFloatBuiltin(k types.BuiltinKind) bool {
	return k == types.F32 || k == types.F64
}

// classifyPrimitive returns the compatibility class for a primitive
// (Builtin-kind) source and destination. The second return value is false
// when either source or destination is not a Builtin kind, signalling that
// the caller should defer to composite classification (which does not exist
// yet).
func classifyPrimitive(source, destination types.TypeKey) (compatibilityClass, bool) {
	srcKind, srcOK := source.Builtin()
	dstKind, dstOK := destination.Builtin()
	if !srcOK || !dstOK {
		return 0, false
	}
	if srcKind == dstKind {
		return compatibleIdentity, true
	}
	srcInteger := isIntegerBuiltin(srcKind)
	srcFloat := isFloatBuiltin(srcKind)
	dstInteger := isIntegerBuiltin(dstKind)
	dstFloat := isFloatBuiltin(dstKind)
	if (srcInteger || srcFloat) && (dstInteger || dstFloat) {
		return compatibleExplicit, true
	}
	return compatibleForbidden, true
}
