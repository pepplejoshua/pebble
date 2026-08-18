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

// isPointerWidthInteger reports whether the integer builtin is at least as wide
// as a pointer on this compiler's ABI. It mirrors the backend's cType width
// convention (internal/backend/types.go): Uint and U64 map to uint64_t, Int
// and I64 map to int64_t, each exactly pointer-width, while every other
// integer builtin (I32, I8, I16, U8, U16, U32) maps to a 32-bit-or-narrower C
// type. A pointer-to-integer cast is accepted only for these pointer-width
// destinations: a narrower destination would force a truncating C cast that the
// mandated -Wall -Wextra -Werror build rejects with -Wpointer-to-int-cast, so
// such a cast is a clean checker rejection instead of a backend-only failure.
func isPointerWidthInteger(k types.BuiltinKind) bool {
	switch k {
	case types.Uint, types.U64, types.Int, types.I64:
		return true
	}
	return false
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
	if srcKind == types.Char && dstInteger {
		return compatibleExplicit, true
	}
	// u8 → char is unconditionally valid: every u8 value (the whole 0-255
	// range) is a valid Unicode scalar value (Basic Latin + Latin-1
	// Supplement), so the cast needs no runtime validity check. Only u8 is
	// accepted as a source — a wider integer (i16/u16/i32/...) can exceed the
	// Unicode codespace or land in the surrogate gap and would need a checked
	// design of its own (out of scope), so this is deliberately restricted to
	// the exact U8 → Char pair rather than any srcInteger.
	if srcKind == types.U8 && dstKind == types.Char {
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
	if source.Kind() == types.Pointer && destination.Kind() == types.Pointer {
		return compatibleExplicit, true
	}

	sourceBuiltin, sourceIsBuiltin := source.Builtin()
	destinationBuiltin, destinationIsBuiltin := destination.Builtin()
	// A pointer value cast to an integer is accepted only when the integer
	// destination is at least as wide as a pointer (u64, uint, i64); a narrower
	// destination (int, i8/i16/i32, u8/u16/u32) falls through to
	// compatibleForbidden so validateCastRecords rejects it with the clean C0601
	// before IR construction — a truncating pointer-to-int C cast would fail the
	// mandated -Wall -Wextra -Werror build with -Wpointer-to-int-cast.
	if source.Kind() == types.Pointer && destinationIsBuiltin && isIntegerBuiltin(destinationBuiltin) && isPointerWidthInteger(destinationBuiltin) {
		return compatibleExplicit, true
	}
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

// coercionKind identifies which typed-IR operation applies to a compatible
// (implicit or explicit) conversion pair.
type coercionKind uint8

const (
	coercionNone coercionKind = iota + 1
	coercionIntegerCast
	coercionIntegerToFloat
	coercionFloatToInteger
	coercionFloatCast
	coercionOptionalInject
	coercionTupleCoerce
	coercionEnumToInteger
	coercionCharToInteger
	coercionIntegerToChar
	coercionOptionalIntegerToEnum
	coercionCheckedIntegerToEnum
	coercionPointerCast
	coercionPointerToInteger
)

// coercionFor returns the typed-IR operation kind that would coerce sourceID
// to destinationID given their known compatibilityClass. class must be
// compatibleImplicit or compatibleExplicit for meaningful results; identity
// and forbidden pairs always return coercionNone.
func coercionFor(snapshot *infer.SemanticSnapshot, class compatibilityClass, sourceID, destinationID types.TypeID) coercionKind {
	if class == compatibleIdentity || class == compatibleForbidden {
		return coercionNone
	}
	sourceKey, _ := snapshot.Types().Key(sourceID)
	destKey, _ := snapshot.Types().Key(destinationID)

	srcBuiltin, srcIsBuiltin := sourceKey.Builtin()
	dstBuiltin, dstIsBuiltin := destKey.Builtin()

	if srcIsBuiltin && dstIsBuiltin {
		if isIntegerBuiltin(srcBuiltin) && isFloatBuiltin(dstBuiltin) {
			return coercionIntegerToFloat
		}
		if isFloatBuiltin(srcBuiltin) && isIntegerBuiltin(dstBuiltin) {
			return coercionFloatToInteger
		}
		if isIntegerBuiltin(srcBuiltin) && isIntegerBuiltin(dstBuiltin) {
			return coercionIntegerCast
		}
		if isFloatBuiltin(srcBuiltin) && isFloatBuiltin(dstBuiltin) {
			return coercionFloatCast
		}
		if srcBuiltin == types.Char && isIntegerBuiltin(dstBuiltin) {
			return coercionCharToInteger
		}
		if srcBuiltin == types.U8 && dstBuiltin == types.Char {
			return coercionIntegerToChar
		}
	}

	if destKey.Kind() == types.Optional {
		if child, ok := destKey.Child(); ok && child == sourceID {
			return coercionOptionalInject
		}
	}

	if sourceKey.Kind() == types.Tuple && destKey.Kind() == types.Tuple {
		return coercionTupleCoerce
	}

	if isEnumType(snapshot, sourceID) && dstIsBuiltin && isIntegerBuiltin(dstBuiltin) {
		return coercionEnumToInteger
	}

	if srcIsBuiltin && isIntegerBuiltin(srcBuiltin) && isEnumType(snapshot, destinationID) {
		return coercionCheckedIntegerToEnum
	}

	if srcIsBuiltin && isIntegerBuiltin(srcBuiltin) && destKey.Kind() == types.Optional {
		if child, ok := destKey.Child(); ok && isEnumType(snapshot, child) {
			return coercionOptionalIntegerToEnum
		}
	}

	if sourceKey.Kind() == types.Pointer && destKey.Kind() == types.Pointer {
		return coercionPointerCast
	}

	if sourceKey.Kind() == types.Pointer && dstIsBuiltin && isIntegerBuiltin(dstBuiltin) && isPointerWidthInteger(dstBuiltin) {
		return coercionPointerToInteger
	}

	return coercionNone
}

// implicitArrayToSlice reports whether a compatibility pair is an ARRAY LITERAL
// directly initializing a SLICE-TYPED BINDING with a matching element type —
// the one position where an array value is implicitly converted to a slice.
// The source must be an authored array literal expression (expressionArray),
// not any other array-typed value (an array local reference, an array-typed call
// result, or an array repeat): every other array→slice pair keeps the existing
// compatibleForbidden classification and its C0601, and a non-literal source has
// no literal to construct a backing array from. The compatibility must also be a
// binding-initializer position (its destination is a binding's declared-type
// annotation), never a plain assignment or compound assignment to an existing
// slice local, so the slice-local REINITIALIZATION gap stays exactly as narrow
// as the binding gap is. This is the form `var s []int = [1, 2, 3];`, treated
// as equivalent to constructing the array and taking a full slice of it. It is
// consulted in the assignment/binding path BEFORE classify, which still reports
// array→slice as compatibleForbidden for every other position (call arguments,
// returns, casts), which must keep their existing rejection.
func implicitArrayToSlice(handoff *solveHandoff, compatibility *compatibilityRecord, sourceType, destination types.TypeID) bool {
	if handoff == nil || compatibility == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil {
		return false
	}
	if !isBindingInitializerCompatibility(handoff, compatibility) {
		return false
	}
	sourceKey, sourceOK := handoff.Semantics.Types().Key(sourceType)
	destinationKey, destinationOK := handoff.Semantics.Types().Key(destination)
	if !sourceOK || !destinationOK || sourceKey.Kind() != types.Array || destinationKey.Kind() != types.Slice {
		return false
	}
	_, sourceElement, _ := sourceKey.Array()
	destinationElement, _ := destinationKey.Child()
	if sourceElement != destinationElement {
		return false
	}
	for _, retained := range handoff.Records.Records() {
		if retained.Expression != nil && retained.Expression.Result == compatibility.Source && retained.Expression.Kind == expressionArray {
			return true
		}
	}
	return false
}

// isBindingInitializerCompatibility reports whether a compatibility record is a
// binding-initializer position: an assignment-role compatibility whose
// destination value is a binding's declared-type annotation (the record
// handleBinding retains for `var s []int = ...`). It is false for a plain
// reassignment (`s = ...`) or compound assignment (`s += ...`), whose
// assignment-role compatibility destinations are the target local's own place
// value instead.
func isBindingInitializerCompatibility(handoff *solveHandoff, compatibility *compatibilityRecord) bool {
	if handoff == nil || compatibility == nil || compatibility.Role != compatibilityAssignment || compatibility.Destination == 0 {
		return false
	}
	for _, retained := range handoff.Records.Records() {
		if retained.Binding != nil && retained.Binding.Annotation == compatibility.Destination {
			return true
		}
	}
	return false
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
