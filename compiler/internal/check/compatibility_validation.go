package check

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeConversion diagnostic.Code = "C0601"

// cTypeWidth returns a string representing the concrete C width/category of a
// builtin kind. Integer builtins that share a C representation (Int and I32
// both map to int32_t, Uint and U64 both map to uint64_t) return the same
// value, enabling structural width comparisons across nominal boundaries.
func cTypeWidth(k types.BuiltinKind) string {
	switch k {
	case types.Int, types.I32:
		return "i32"
	case types.Uint, types.U64:
		return "u64"
	case types.I64:
		return "i64"
	case types.I8:
		return "i8"
	case types.I16:
		return "i16"
	case types.U8:
		return "u8"
	case types.U16:
		return "u16"
	case types.U32:
		return "u32"
	case types.F32:
		return "f32"
	case types.F64:
		return "f64"
	}
	return ""
}

// builtinTypeName returns the short Pebble-source name for a builtin kind
// ("i8", "int", "u64", "f32", etc.), used in diagnostic messages.
func builtinTypeName(k types.BuiltinKind) string {
	switch k {
	case types.Bool:
		return "bool"
	case types.Char:
		return "char"
	case types.Str:
		return "str"
	case types.Void:
		return "void"
	case types.Int:
		return "int"
	case types.Uint:
		return "uint"
	case types.I8:
		return "i8"
	case types.I16:
		return "i16"
	case types.I32:
		return "i32"
	case types.I64:
		return "i64"
	case types.U8:
		return "u8"
	case types.U16:
		return "u16"
	case types.U32:
		return "u32"
	case types.U64:
		return "u64"
	case types.F32:
		return "f32"
	case types.F64:
		return "f64"
	}
	return "<type>"
}

// describeTypeForDiagnostic returns a human-readable name for a type ID,
// suitable for inclusion in diagnostic messages.
func describeTypeForDiagnostic(snapshot *infer.SemanticSnapshot, id types.TypeID) string {
	key, ok := snapshot.Types().Key(id)
	if !ok {
		return "<unknown>"
	}
	if builtin, ok := key.Builtin(); ok {
		name := builtinTypeName(builtin)
		if name != "" {
			return name
		}
	}
	switch key.Kind() {
	case types.Pointer:
		return "pointer"
	case types.Tuple:
		return "tuple"
	case types.Optional:
		return "optional"
	case types.Array:
		return "array"
	case types.Slice:
		return "slice"
	case types.Function:
		return "function"
	case types.Nominal:
		return "enum"
	}
	return "<type>"
}

// sameConcreteIntegerWidth reports whether sourceID and destinationID are both
// integer builtins whose concrete C representation is identical (for example
// i32 and int, which both compile to int32_t). Pairs that share a concrete
// width are allowed to pass through without an explicit cast; pairs with
// different concrete widths require one. Non-integer pairs always return false.
func sameConcreteIntegerWidth(snapshot *infer.SemanticSnapshot, sourceID, destinationID types.TypeID) bool {
	if snapshot == nil || snapshot.Types() == nil {
		return false
	}
	srcKey, srcOK := snapshot.Types().Key(sourceID)
	dstKey, dstOK := snapshot.Types().Key(destinationID)
	if !srcOK || !dstOK {
		return false
	}
	srcBuiltin, srcIsBuiltin := srcKey.Builtin()
	dstBuiltin, dstIsBuiltin := dstKey.Builtin()
	if !srcIsBuiltin || !dstIsBuiltin {
		return false
	}
	if !isIntegerBuiltin(srcBuiltin) || !isIntegerBuiltin(dstBuiltin) {
		return false
	}
	return cTypeWidth(srcBuiltin) == cTypeWidth(dstBuiltin)
}

// compositeCoercionSkipsValidation reports whether sourceID and destinationID
// are both composite types (tuples or struct-nominal types) with matching
// element/field counts — the shape the IR builder's buildValueRecord handles
// via per-element TupleCoerce / field-coercion machinery. Rejecting such
// pairs at the compatibility-validation layer would break deliberate implicit
// tuple and struct field coercion features from earlier phases.
//
// The skip applies ONLY when the source value is itself a composite-typed
// expression (a tuple literal or a struct literal), not a bare local whose
// whole-value type differs from the destination. For bare locals the IR
// builder passes through a SymbolValue unchanged and the backend must reject
// a width mismatch; for composite expressions the builder inserts coercions
// into each child so the top-level compatibility record is just structural
// bookkeeping.
func compositeCoercionSkipsValidation(handoff *solveHandoff, sourceVal valueID, snapshot *infer.SemanticSnapshot, sourceType, destinationID types.TypeID) bool {
	if handoff == nil || snapshot == nil || snapshot.Types() == nil {
		return false
	}
	srcKey, srcOK := snapshot.Types().Key(sourceType)
	dstKey, dstOK := snapshot.Types().Key(destinationID)
	if !srcOK || !dstOK {
		return false
	}
	srcKind := srcKey.Kind()
	dstKind := dstKey.Kind()
	// Only skip for struct-to-struct or tuple-to-tuple pairs.
	if srcKind != dstKind {
		return false
	}
	if srcKind != types.Tuple && srcKind != types.Nominal {
		return false
	}
	// Structs: same nominal declaration means identical layout — the IR
	// builder's struct-field coercion machinery handles any payload mismatches.
	if srcKind == types.Nominal {
		srcDecl, _, _ := srcKey.Nominal()
		dstDecl, _, _ := dstKey.Nominal()
		if srcDecl != dstDecl {
			return false
		}
		// Also verify the source value is a struct-literal expression, not a
		// bare local: only struct literals trigger field-coercion insertion.
		return isCompositeExpressionSource(handoff, sourceVal, types.Nominal)
	}
	// Tuples: matching element count means the IR builder will handle
	// element-wise coercion via TupleCoerce nodes — but only when the source
	// is a tuple expression (literal), not a bare tuple local.
	srcElems, srcOk := srcKey.Elements()
	dstElems, dstOk := dstKey.Elements()
	if !srcOk || !dstOk {
		return false
	}
	if len(srcElems) != len(dstElems) {
		return false
	}
	return isCompositeExpressionSource(handoff, sourceVal, types.Tuple)
}

// isCompositeExpressionSource reports whether a value ID corresponds to a
// retained expression record whose kind matches the expected composite kind.
// This distinguishes "coercible composite expression" (tuple literal, struct
// literal) from "bare local of composite type": only the former triggers
// per-element coercion node insertion in the IR builder.
func isCompositeExpressionSource(handoff *solveHandoff, val valueID, expectedKind types.Kind) bool {
	for _, retained := range handoff.Records.Records() {
		if retained.Expression != nil && retained.Expression.Result == val {
			if expectedKind == types.Tuple && retained.Expression.Kind == expressionTuple {
				return true
			}
			if expectedKind == types.Nominal && retained.Expression.Kind != expressionTuple {
				// Struct literals and other nominal constructors are non-tuple
				// expression kinds; any such expression qualifies.
				return true
			}
		}
	}
	return false
}

func validateCompatibilityRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	for _, retained := range handoff.Records.Records() {
		compatibility := retained.Compatibility
		if compatibility == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		source, sourceOK := records.Root(compatibility.Source)
		destination, destinationOK := records.Root(compatibility.Destination)
		if !sourceOK || !destinationOK || source.State != infer.TypeFinal || destination.State != infer.TypeFinal {
			continue
		}
		class := classify(handoff.Semantics, source.Type, destination.Type)
		if class == compatibleForbidden {
			// An array literal directly initializing a slice-typed binding
			// (`var s []int = [1, 2, 3];`) is valid — equivalent to constructing
			// the array then taking a full slice of it — even though classify
			// still reports array→slice as compatibleForbidden for every other
			// position (call arguments, returns, casts, plain reassignment),
			// which keep their existing C0601.
			if implicitArrayToSlice(handoff, compatibility, source.Type, destination.Type) {
				continue
			}
			failed = true
			reporter.add(diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeConversion,
				Message:  fmt.Sprintf("cannot convert value for %s", expectedRoleText(compatibility.Role, compatibility.Ordinal)),
				Primary:  diagnostic.Label{Span: compatibility.Header.Span},
			})
		} else if class == compatibleExplicit {
			// The explicit-cast requirement applies ONLY to bare scalar-value
			// positions where a source value flows into a pre-declared
			// destination type: plain assignments, call arguments, and return
			// statements. Optional-injection, tuple-component coercion, and
			// struct-field construction are intentionally permissive implicit
			// coercions established in earlier phases; compatibilityBranch is
			// a branch-consistency check (switch-case value vs subject), not a
			// value-vs-fixed-destination check. Those four roles skip the
			// explicit-cast gate and keep their prior implicit behaviour.
			//
			// Composite-type pairs (tuple→tuple, struct→struct) whose
			// element-wise classification yields compatibleExplicit are also
			// skipped — but ONLY for assignment-role bindings where the IR
			// builder handles element-wise coercion (TupleCoerce, field
			// coercion) at build time. A top-level tuple/struct return or
			// argument is a value-transfer position and must still require an
			// explicit cast when concrete widths differ.
			switch compatibility.Role {
			case compatibilityAssignment:
				if compositeCoercionSkipsValidation(handoff, compatibility.Source, handoff.Semantics, source.Type, destination.Type) {
					continue
				}
				if sameConcreteIntegerWidth(handoff.Semantics, source.Type, destination.Type) {
					continue
				}
				failed = true
				srcName := describeTypeForDiagnostic(handoff.Semantics, source.Type)
				dstName := describeTypeForDiagnostic(handoff.Semantics, destination.Type)
				reporter.add(diagnostic.Diagnostic{
					Severity: diagnostic.Error,
					Code:     CodeConversion,
					Message:  fmt.Sprintf("cannot implicitly convert value of type %s to %s for %s; use an explicit cast", srcName, dstName, expectedRoleText(compatibility.Role, compatibility.Ordinal)),
					Primary:  diagnostic.Label{Span: compatibility.Header.Span},
				})
			case compatibilityArgument, compatibilityReturn:
				if sameConcreteIntegerWidth(handoff.Semantics, source.Type, destination.Type) {
					continue
				}
				failed = true
				srcName := describeTypeForDiagnostic(handoff.Semantics, source.Type)
				dstName := describeTypeForDiagnostic(handoff.Semantics, destination.Type)
				reporter.add(diagnostic.Diagnostic{
					Severity: diagnostic.Error,
					Code:     CodeConversion,
					Message:  fmt.Sprintf("cannot implicitly convert value of type %s to %s for %s; use an explicit cast", srcName, dstName, expectedRoleText(compatibility.Role, compatibility.Ordinal)),
					Primary:  diagnostic.Label{Span: compatibility.Header.Span},
				})
			default:
				// compatibilityOptionalInjection, compatibilityTupleComponent,
				// compatibilityRecordField, compatibilityBranch — restore
				// implicit-permissive behaviour (silently pass through as before).
			}
		}
	}
	return !failed
}
