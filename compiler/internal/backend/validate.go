package backend

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// validateEntrySignature checks the entry's calling convention, parameter
// count, and result type against the supported shapes: zero parameters, or
// exactly one parameter of the []str argv form (main(argv []str)), plus a void
// result (empty body), an int/i32/i64 result, or, since Float Stage A, an
// f32/f64 result (body under the recursive block grammar). The two-parameter
// main(argc int, argv []str) form stays intentionally unsupported (a
// documented V1-parity decision). On success it returns the resolved result
// builtin (types.Void, types.Int, types.I32, types.I64, types.F32, or
// types.F64) — for an integer entry that returned builtin IS the width every
// builder downstream emits at, threaded through Emit rather than re-derived.
// Whether the body actually matches the result's shape is decided by the
// body-validation step the caller dispatches on. When the single-parameter
// argv form is accepted, Emit reads decl.Parameters itself to wire the slice
// into pebble_user_main's signature and the body's locals scope (see
// emitEntryC).
func validateEntrySignature(decl tir.Node, snapshot *types.Snapshot) (types.BuiltinKind, error) {
	if decl.Convention != types.Pebble {
		return 0, fmt.Errorf("entry function uses %s calling convention, want Pebble", callingConventionName(decl.Convention))
	}
	if len(decl.Parameters) > 1 {
		return 0, fmt.Errorf("entry function has %d parameter(s), want 0 or a single []str argv (main(argc int, argv []str) is not supported yet)", len(decl.Parameters))
	}
	if len(decl.Parameters) == 1 && !isStrSlice(snapshot, decl.Parameters[0].Type) {
		return 0, fmt.Errorf("entry function parameter is a %s, want []str (main(argv []str) is the only parameterized entry shape supported)", describeType(snapshot, decl.Parameters[0].Type))
	}
	key, ok := snapshot.Key(decl.ResultType)
	if !ok {
		return 0, fmt.Errorf("entry function result type %d is not in the type snapshot", decl.ResultType)
	}
	builtin, ok := key.Builtin()
	if !ok || (builtin != types.Void && builtin != types.Int && builtin != types.I32 && builtin != types.I64 && builtin != types.F32 && builtin != types.F64) {
		return 0, fmt.Errorf("entry function result type is %s, want void, int, i32, i64, f32, or f64", describeType(snapshot, decl.ResultType))
	}
	return builtin, nil
}

// isStrSlice reports whether a type is a slice whose element type is str. It is
// the backend mirror of the checker's validArgvParameter (entry_validation.go)
// and gates exactly the one parameterized entry shape this backend supports:
// main(argv []str). A slice of str is NOT a general slice element this backend
// can lower (sliceElementCType rejects str by design — validateHelperSignature
// documents the deliberate gate), so this test is only consulted for the
// entry's own parameter, whose C type is the runtime's fixed PebbleStrSlice
// (pebble_rt.h), never a pebble_slice_<typeID>_t typedef.
func isStrSlice(snapshot *types.Snapshot, id types.TypeID) bool {
	key, ok := snapshot.Key(id)
	if !ok || key.Kind() != types.Slice {
		return false
	}
	element, ok := key.Child()
	if !ok {
		return false
	}
	elementKey, ok := snapshot.Key(element)
	if !ok {
		return false
	}
	builtin, ok := elementKey.Builtin()
	return ok && builtin == types.Str
}

// validateSliceElementType rejects a slice type whose element type is anything
// other than a fixed-width integer builtin (the entry's resolved width, uint,
// u8, u16, u32, u64, i8, i16, i32, or i64 — each resolved to its OWN width by
// resolvedBuiltin/cType, independent of the ambient `width` of the context the
// slice is being validated from), char, bool, or a tuple/optional/struct type
// — the same element gate 10.37
// enforces for a slice-typed local (see buildSliceLocalDeclaration and
// sliceElementCType), applied here to a slice-typed function parameter or
// result type so a helper signature naming a slice of str or any
// other unsupported element is a clean rejection before any body is built.
func validateSliceElementType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) error {
	key, ok := snapshot.Key(id)
	if !ok {
		return fmt.Errorf("slice type %d is not in the type snapshot", id)
	}
	element, ok := key.Child()
	if !ok {
		return fmt.Errorf("slice type %s has no element type", describeType(snapshot, id))
	}
	if !isSupportedSliceElementType(unit, snapshot, element) {
		return fmt.Errorf("slice element type is %s, want a fixed-width integer, char, bool, tuple, optional, struct, or enum", describeType(snapshot, element))
	}
	return nil
}

// validateHelperSignature checks one called function against the constraints
// every reachable helper must satisfy: Pebble-convention, parameters whose
// types are exactly the entry's resolved width, bool, str, a tuple type, a
// struct type, an optional type, or a pointer type, and a result of exactly
// the entry's resolved width, bool, str, a tuple
// type, a struct type, an optional type, or void. The width
// rule is the same reasoning 10.13 established for locals — a called function
// of the other width (an i32 helper called from an i64 entry, or vice versa) is
// a clean width-mismatch rejection, never a coercion, since there is no
// cast/coercion lowering to fall back on. A parameter's own type has the same
// options a local has: the entry's width, bool, str (a str parameter is
// declared as the runtime's PebbleStr and read/compared/returned exactly like a
// str local — 10.36), a tuple type (one of the
// shapes 10.19 supports — element types the entry's width or bool), a
// struct type (one of the shapes 10.22 supports — field types the entry's
// width or bool), an optional type (whose payload is read back through the
// same has_value / force-unwrap machinery an optional local uses), or a
// pointer type; a tuple/struct/str/optional result type has the same options.
// The
// tuple/struct's own internal shape is validated wherever
// its typedef gets built (buildTupleTypedef / buildStructTypedef), not here,
// and an optional result's own payload shape is likewise validated wherever
// its typedef gets built (buildOptionalTypedef / optionalPayloadCType) — the
// same coverage an optional parameter's payload gets through
// collectOptionalTypes.
// Anything else (a pointer, an array, an enum, a helper of the
// other integer width) is a clean rejection naming the position. A void-result
// helper is accepted: 10.33 added the one position such a call is legal in —
// a bare discarded-expression statement (buildExpressionStatement), which the
// void call's only reachable shape from real source (helper(); as its own
// statement) produces. A void call in any value position is still rejected by
// the value builders themselves (buildExpr's width gate and
// buildAggregateCallInitializer's result-type match), never silently emitted.
// validateExternSignature checks a C-convention extern declaration the entry
// (or a helper) actually calls: it must be C-convention (the checker already
// enforces this, so a mismatch is hand-built IR), non-variadic (a C-convention
// variadic function type is rejected by the checker too), and every parameter
// and the result must be typed by a C spelling this backend can emit at a call
// site — a fixed-width integer builtin (including uint/u64, each resolved to
// its own C type), bool, char, str (const char *), a pointer to a supported
// pointee, f32/f64, or a void result. This mirrors validateHelperSignature's
// own gate but narrowed to the shapes an extern call site can actually
// produce: the parameter grammar is exactly buildCallArgument's and the result
// flows into whatever consumes the call. No pebble_fn_<symbolID> prototype,
// definition, or body lookup is ever attempted for an extern, and it is never
// added to the reachable-helper emission order (see reachabilityWalk.visit).
func validateExternSignature(st *emitState, unit *tir.Unit, decl tir.Node, snapshot *types.Snapshot) error {
	if decl.Kind != tir.ExternDeclaration {
		return fmt.Errorf("called function symbol %d is not an extern declaration", decl.Symbol)
	}
	if decl.Convention != types.C {
		return fmt.Errorf("called extern function symbol %d uses %s calling convention, want C", decl.Symbol, callingConventionName(decl.Convention))
	}
	if decl.HasBody {
		return fmt.Errorf("called extern function symbol %d has a body, which this backend does not emit for an extern declaration", decl.Symbol)
	}
	if decl.Variadic {
		return fmt.Errorf("called extern function symbol %d is variadic, which this backend does not support yet", decl.Symbol)
	}
	for i, param := range decl.Parameters {
		if _, err := externCType(st, snapshot, param.Type); err != nil {
			return fmt.Errorf("called extern function symbol %d parameter %d (symbol %d) %v", decl.Symbol, i, param.Symbol, err)
		}
	}
	if _, err := externCType(st, snapshot, decl.ResultType); err != nil {
		return fmt.Errorf("called extern function symbol %d result type %v", decl.Symbol, err)
	}
	return nil
}

func validateHelperSignature(unit *tir.Unit, decl tir.Node, snapshot *types.Snapshot, width types.BuiltinKind) error {
	if decl.Convention != types.Pebble {
		return fmt.Errorf("called function symbol %d uses %s calling convention, want Pebble", decl.Symbol, callingConventionName(decl.Convention))
	}
	for i, param := range decl.Parameters {
		// A parameter's type is resolved the same way a local's initializer's
		// is: the entry's resolved width (built by buildExpr), bool (built by
		// buildBoolExpr), a char value (built by buildCharOperand — since
		// 10.41 a char parameter is seeded like a char local and read/
		// compared/returned exactly as one), a str value (built by
		// buildStrOperand — since 10.36 a
		// str parameter is seeded like a str local and read/compared/returned
		// exactly as one), an f32/f64 value (built by buildFloatExpr at the
		// parameter's own float kind — since this slice a float parameter is
		// seeded like a float local and read/compared/returned/passed exactly
		// as one), a tuple/struct type (read back through the
		// Load(TuplePlace)/Load(FieldPlace) machinery), or, since 10.38, a
		// slice type (read back through the same Load(CheckedIndexPlace)
		// machinery a slice local uses), nothing else. This is
		// exactly the width-consistency rule 10.13 established for locals,
		// applied to parameters and extended to the aggregate and str local
		// grammars 10.19/10.22/10.23/10.37 already build. A slice-typed
		// parameter's element type must still be exactly the entry's resolved
		// width or bool — the same gate 10.37 enforces for a slice local — so
		// a parameter of a slice type whose element is unsupported (a slice of
		// tuples, str, and so on) is a clean rejection, not a guessed
		// lowering. A scalar parameter whose own resolved integer width shares
		// the entry's C representation (isCompatibleIntegerWidth — the shape a
		// generic specialization produces: clamp[i32] has i32-typed parameters
		// even when the entry is declared `int`, and int/i32 are distinct
		// builtins sharing int32_t) is admitted too; helperSignature declares
		// it at that shared C type and buildCallArgument builds its call-site
		// argument at the parameter's own width, so no cast is ever needed.
		// Since the fixed-width-integer widening, a parameter of ANY concrete
		// fixed-width integer builtin (isFixedWidthInteger — a u8, i16, u32,
		// ... parameter in an int/i32/i64 entry, or an i64 parameter in an
		// i32 entry) is admitted the same way: helperSignature declares it at
		// the parameter's OWN C type (uint8_t for u8, int16_t for i16, and so
		// on), seeds it into the callee's scope at its own width, and
		// buildCallArgument builds its call-site argument at that same width,
		// so the body reads and the call passes the parameter at its own width
		// with no cast ever needed.
		if !isWidth(snapshot, width, param.Type) && !isCompatibleIntegerWidth(snapshot, width, param.Type) && !isUint(snapshot, param.Type) && !isU64(snapshot, param.Type) && !isFixedWidthInteger(snapshot, param.Type) && !isBool(snapshot, param.Type) && !isChar(snapshot, param.Type) && !isStr(snapshot, param.Type) && !isFloat(snapshot, param.Type) && !isTuple(snapshot, param.Type) && !isStruct(snapshot, param.Type) && !isArray(snapshot, param.Type) && !isSlice(snapshot, param.Type) && !isPointer(snapshot, param.Type) && !isOptional(snapshot, param.Type) && !isFunctionType(snapshot, param.Type) && !isEnumType(unit, snapshot, param.Type) {
			return fmt.Errorf("called function symbol %d parameter %d (symbol %d) has type %s, want a fixed-width integer (%s, uint, or u64), bool, char, str, f32, f64, a tuple/struct type, a slice type, a pointer type, an optional type, a function type, or an enum/union type (a parameter may be any fixed-width integer, uint, u64, bool, char, str, f32, f64, a tuple/struct type, a slice type, a pointer type, an optional type, a function type, or an enum/union type)", decl.Symbol, i, param.Symbol, describeType(snapshot, param.Type), wantName(width))
		}
		if isSlice(snapshot, param.Type) {
			if err := validateSliceElementType(unit, snapshot, width, param.Type); err != nil {
				return fmt.Errorf("called function symbol %d parameter %d (symbol %d) is a slice type with an unsupported element type: %v", decl.Symbol, i, param.Symbol, err)
			}
		}
		if isArray(snapshot, param.Type) {
			key, ok := snapshot.Key(param.Type)
			if !ok {
				return fmt.Errorf("called function symbol %d parameter %d (symbol %d) has array type missing from the type snapshot", decl.Symbol, i, param.Symbol)
			}
			_, element, ok := key.Array()
			if !ok || !isSupportedSliceElementType(unit, snapshot, element) {
				return fmt.Errorf("called function symbol %d parameter %d (symbol %d) has an unsupported array element type", decl.Symbol, i, param.Symbol)
			}
		}
		// A function-typed parameter's own signature needs no per-signature gate
		// here beyond the shared validateFunctionTypeSignature check: exactly
		// like an optional parameter's payload, it is validated wherever its
		// typedef gets built (buildFunctionTypedef via validateFunctionTypeSignature),
		// and collectFunctionTypes guarantees that typedef is emitted for every
		// reachable helper's parameter types, so an unsupported signature shape
		// (a tuple/struct/slice/optional/pointer parameter, or an
		// aggregate/str result) is a clean rejection at typedef build
		// time, never a guessed layout. The signature check is repeated here so
		// a reachable helper with a bad function-typed parameter fails during
		// helper discovery with the same message a function-typed local's
		// invalid type would name.
		if isFunctionType(snapshot, param.Type) {
			if err := validateFunctionTypeSignature(snapshot, width, param.Type); err != nil {
				return fmt.Errorf("called function symbol %d parameter %d (symbol %d) is a function type with an unsupported signature: %v", decl.Symbol, i, param.Symbol, err)
			}
		}
		// An optional parameter's own payload shape needs no per-payload gate
		// here: exactly like an optional result's, it is validated wherever its
		// typedef gets built (buildOptionalTypedef / optionalPayloadCType), and
		// collectOptionalTypes guarantees that typedef is emitted for every
		// reachable helper's parameter types, so an unsupported payload is a
		// clean rejection at typedef build time, never a guessed layout.
	}
	resultWidth, integerResult := resolvedBuiltin(snapshot, decl.ResultType)
	if (!integerResult || cType(resultWidth) == "") && !isBool(snapshot, decl.ResultType) && !isChar(snapshot, decl.ResultType) && !isStr(snapshot, decl.ResultType) && !isFloat(snapshot, decl.ResultType) && !isTuple(snapshot, decl.ResultType) && !isStruct(snapshot, decl.ResultType) && !isArray(snapshot, decl.ResultType) && !isSlice(snapshot, decl.ResultType) && !isVoid(snapshot, decl.ResultType) && !isPointer(snapshot, decl.ResultType) && !isOptional(snapshot, decl.ResultType) && !isFunctionType(snapshot, decl.ResultType) {
		return fmt.Errorf("called function symbol %d has result type %s, want its own integer width, bool, char, str, f32, f64, a tuple/struct result type, a slice result type, a pointer result type, an optional result type, a function result type, or void", decl.Symbol, describeType(snapshot, decl.ResultType))
	}
	if isSlice(snapshot, decl.ResultType) {
		if err := validateSliceElementType(unit, snapshot, width, decl.ResultType); err != nil {
			return fmt.Errorf("called function symbol %d has a slice result type with an unsupported element type: %v", decl.Symbol, err)
		}
	}
	if isArray(snapshot, decl.ResultType) {
		key, ok := snapshot.Key(decl.ResultType)
		if !ok {
			return fmt.Errorf("called function symbol %d has array result type missing from the type snapshot", decl.Symbol)
		}
		_, element, ok := key.Array()
		if !ok || !isSupportedSliceElementType(unit, snapshot, element) {
			return fmt.Errorf("called function symbol %d has an unsupported array result element type", decl.Symbol)
		}
	}
	if isFunctionType(snapshot, decl.ResultType) {
		if err := validateFunctionTypeSignature(snapshot, width, decl.ResultType); err != nil {
			return fmt.Errorf("called function symbol %d has a function result type with an unsupported signature: %v", decl.Symbol, err)
		}
	}
	return nil
}

// validateFunctionTypeSignature checks one function type used as a first-class
// value — a function-typed local's declared type, a function-typed value's own
// type, or an indirect call's callee type — against the constraints every such
// signature must satisfy, mirroring validateHelperSignature's own gate but
// narrowed to this slice's supported shapes. A function type must be
// Pebble-convention (a C-convention function type as a first-class local's
// type is not checker-reachable — the checker itself rejects assigning a
// C-convention function value to a fn(...) type and a `fn "C"(...)` type
// annotation fails typed-IR construction — so it is a clean rejection here,
// never supported), non-variadic, and every parameter must be one of the
// entry's resolved width, uint, u64, another fixed-width integer, bool, char,
// str, a float (f32/f64), or a pointer type, and the result must be one of
// the entry's resolved width, u64, bool, char, a float (f32/f64), void, or a
// pointer type. This is deliberately the
// set of shapes this slice can both BUILD (the parameter grammar is exactly
// buildCallArgument's, so every fn-typed call argument is buildable; the
// result grammar is exactly the positions the backend can consume an indirect
// call's result in — the entry's return via buildExpr, a bool position via
// buildBoolExpr, a char position via buildCharOperand, a float position via
// buildFloatExpr, a pointer result
// consumed by buildExpr's pointer-typed IndirectCall path, and a discarded
// statement via buildExpressionStatement) and whose C types are fully
// self-contained (the entry's cType, uint64_t, bool, int32_t, PebbleStr, a
// float/double via floatCType, a
// pointer's own `<pointee> *` spelling via pointerTypeName, or
// void — never a tuple/struct/slice/optional C type that would drag an
// aggregate typedef into the fnptr typedef and require the aggregate collectors
// to chase function-type signatures). Any other parameter/result shape — a
// tuple/struct/slice/optional, or an aggregate/str result — is a
// clean rejection naming what is unsupported, the same gate buildFunctionTypedef
// re-checks before emitting a typedef.
func validateFunctionTypeSignature(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) error {
	key, ok := snapshot.Key(id)
	if !ok {
		return fmt.Errorf("function type %d is not in the type snapshot", id)
	}
	convention, parameters, result, variadic, ok := key.Function()
	if !ok {
		return fmt.Errorf("type %s is not a function type", describeType(snapshot, id))
	}
	if convention != types.Pebble {
		return fmt.Errorf("function type %s uses the %s calling convention, want Pebble (a C-convention function type as a first-class value is not supported yet)", describeType(snapshot, id), callingConventionName(convention))
	}
	if variadic {
		return fmt.Errorf("function type %s is variadic, which is not supported yet", describeType(snapshot, id))
	}
	// A function type's parameter is admitted when it is an integer builtin
	// this backend emits (resolved to its OWN width via resolvedBuiltin/cType,
	// independent of the ambient `width` of the context the function type is
	// being validated from — this is what lets a `fn(int) u64` type be
	// validated from a u64-width call context and a `fn(u64) int` type from an
	// entry-width context), or uint/u64 (both resolve to uint64_t), or
	// bool/char/str, or a float (f32/f64, resolved to its own float/double C
	// type via floatCType), or a pointer type (spelled via pointerTypeName, the
	// same way an ordinary helper's pointer parameter is). The signature's
	// parameter C types are decided by the same resolution in
	// functionTypeParamCType, and each call argument is built at its
	// parameter's own resolved width by buildCallArgument, so the kind of
	// each parameter determines how it is built rather than the ambient width.
	for i, parameter := range parameters {
		paramWidth, integerParam := resolvedBuiltin(snapshot, parameter)
		if !(integerParam && cType(paramWidth) != "") && !isBool(snapshot, parameter) && !isChar(snapshot, parameter) && !isStr(snapshot, parameter) && !isFloat(snapshot, parameter) && !isPointer(snapshot, parameter) {
			return fmt.Errorf("function type %s parameter %d has type %s, want %s, uint, u64, or another fixed-width integer, bool, char, str, f32, f64, or a pointer type (a function-typed value's signature may only mention parameter shapes this backend can build as a call argument)", describeType(snapshot, id), i, describeType(snapshot, parameter), wantName(width))
		}
	}
	if !isWidth(snapshot, width, result) && !isU64(snapshot, result) && !isBool(snapshot, result) && !isChar(snapshot, result) && !isFloat(snapshot, result) && !isVoid(snapshot, result) && !isPointer(snapshot, result) {
		return fmt.Errorf("function type %s has result type %s, want %s, u64, bool, char, f32, f64, void, or a pointer type (a function-typed value's signature may only mention result shapes this backend can lower as an indirect call's result)", describeType(snapshot, id), describeType(snapshot, result), wantName(width))
	}
	return nil
}

// validateEmptyBody accepts only a block with no statements, or the single
// synthesized ImplicitReturn that a void entry's empty body ends with. Any
// other statement content is rejected, not best-effort lowered.
func validateEmptyBody(unit *tir.Unit, block tir.Node) error {
	if len(block.Children) == 0 {
		return nil
	}
	if len(block.Children) == 1 {
		if child, ok := unit.Node(block.Children[0]); ok && child.Kind == tir.ImplicitReturn {
			return nil
		}
	}
	if child, ok := unit.Node(block.Children[0]); ok {
		return fmt.Errorf("entry function body is not empty: unsupported statement %s found; this backend only emits an empty-bodied void entry", child.Kind)
	}
	return fmt.Errorf("entry function body is not empty: %d statement(s) found; this backend only emits an empty-bodied void entry", len(block.Children))
}
