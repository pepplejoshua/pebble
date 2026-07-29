package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

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
// the caller should defer to composite classification.
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

func isEnumType(snapshot *infer.SemanticSnapshot, id types.TypeID) bool {
	if snapshot == nil || snapshot.Types() == nil {
		return false
	}
	key, ok := snapshot.Types().Key(id)
	if !ok || key.Kind() != types.Nominal {
		return false
	}
	declaration, _, ok := key.Nominal()
	if !ok {
		return false
	}
	value, ok := snapshot.TypeDeclaration(declaration)
	return ok && value.Nominal == infer.NominalEnum
}

func classifyComposite(snapshot *infer.SemanticSnapshot, sourceID, destinationID types.TypeID) (compatibilityClass, bool) {
	if snapshot == nil || snapshot.Types() == nil {
		return 0, false
	}
	source, sourceOK := snapshot.Types().Key(sourceID)
	destination, destinationOK := snapshot.Types().Key(destinationID)
	if !sourceOK || !destinationOK {
		return 0, false
	}
	if source.Kind() == types.Builtin && destination.Kind() == types.Builtin {
		return 0, false
	}
	if sourceID == destinationID {
		return compatibleIdentity, true
	}

	sourceBuiltin, sourceIsBuiltin := source.Builtin()
	destinationBuiltin, destinationIsBuiltin := destination.Builtin()
	if isEnumType(snapshot, sourceID) && destinationIsBuiltin && isIntegerBuiltin(destinationBuiltin) {
		return compatibleExplicit, true
	}
	if sourceIsBuiltin && isIntegerBuiltin(sourceBuiltin) && isEnumType(snapshot, destinationID) {
		return compatibleExplicit, true
	}
	if sourceIsBuiltin && isIntegerBuiltin(sourceBuiltin) && destination.Kind() == types.Optional {
		if child, ok := destination.Child(); ok && isEnumType(snapshot, child) {
			return compatibleExplicit, true
		}
	}
	if destination.Kind() == types.Optional {
		if child, ok := destination.Child(); ok && child == sourceID {
			return compatibleImplicit, true
		}
	}

	if source.Kind() == types.Tuple && destination.Kind() == types.Tuple {
		sourceElements, _ := source.Elements()
		destinationElements, _ := destination.Elements()
		if len(sourceElements) != len(destinationElements) {
			return compatibleForbidden, true
		}
		allImplicit := true
		for index := range sourceElements {
			class := classify(snapshot, sourceElements[index], destinationElements[index])
			if class == compatibleForbidden {
				return compatibleForbidden, true
			}
			if class == compatibleExplicit {
				allImplicit = false
			}
		}
		if allImplicit {
			return compatibleImplicit, true
		}
		return compatibleExplicit, true
	}
	return compatibleForbidden, true
}

func classify(snapshot *infer.SemanticSnapshot, sourceID, destinationID types.TypeID) compatibilityClass {
	if snapshot == nil || snapshot.Types() == nil {
		return compatibleForbidden
	}
	sourceKey, sourceOK := snapshot.Types().Key(sourceID)
	destinationKey, destinationOK := snapshot.Types().Key(destinationID)
	if !sourceOK || !destinationOK {
		return compatibleForbidden
	}
	if class, ok := classifyPrimitive(sourceKey, destinationKey); ok {
		return class
	}
	if class, ok := classifyComposite(snapshot, sourceID, destinationID); ok {
		return class
	}
	return compatibleForbidden
}
