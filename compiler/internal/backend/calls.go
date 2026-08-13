package backend

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildHelperFunctions builds the C text for every reachable helper, in the
// post-order discovery gives (callees before callers), each as its own
// `static <width> pebble_fn_<symbolID>(PebbleContext *ctx, <params>...) { ... }`
// block with its body built by the exact same buildBlock the entry's body
// uses — no parallel body-builder. Before the body is built, the helper's own
// parameters seed its locals scope exactly as if each had been Initialize'd:
// every parameter maps to its resolved type — the entry's width, bool, char
// (localInfo{isChar}), str
// (localInfo{isStr}), a tuple
// type (localInfo{tuple}), a struct type (localInfo{structType}), a slice
// type (localInfo{sliceType}), a pointer type (localInfo{pointerType}), or an
// optional type (localInfo{optional}) — so a
// SymbolValue reference or a Store targeting a parameter inside the body
// resolves through the existing machinery unchanged, and a tuple/struct
// parameter's element/field reads resolve through the same
// Load(TuplePlace)/Load(FieldPlace) machinery a tuple/struct local uses. The
// C signature declares each parameter with the same pebble_local_<symbolID>
// naming every local uses, so a parameter and a local are textually identical
// inside the body (which is correct: they behave identically once inside the
// function), a tuple/struct parameter's C type being its aggregate's own
// typedef name (pebble_tuple_<typeID>_t / pebble_struct_<typeID>_t), a slice
// parameter's being its slice's own typedef name (pebble_slice_<typeID>_t),
// an optional parameter's being the optional's own typedef name
// (pebble_optional_<typeID>_t), and a str
// parameter's C type being the runtime's fixed PebbleStr. Each
// parameter also gets a `(void)pebble_local_<symbolID>;` immediately after
// the opening brace, the same -Wunused-parameter defense the `(void)ctx;`
// already provides for the context (confirmed: -Wunused-parameter genuinely
// fires under -Wall -Wextra -Werror for a declared-but-never-read parameter).
// Each helper gets its own fresh scope for anything its body declares (the
// seeded parameters plus whatever buildBlock adds), so a helper's locals are
// invisible to the entry and to sibling helpers, exactly as two blocks at the
// same nesting level are isolated.
func buildHelperFunctions(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, helpers []helperInfo, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, error) {
	texts := make([]string, 0, len(helpers))
	for _, helper := range helpers {
		params, scope, bodyWidth, returnType, result, err := helperSignature(st, unit, snapshot, helper, width)
		if err != nil {
			return "", err
		}
		casts := make([]string, 0, len(helper.decl.Parameters))
		for _, param := range helper.decl.Parameters {
			casts = append(casts, fmt.Sprintf("    (void)pebble_local_%d;", param.Symbol))
		}
		statements, err := buildBlock(st, unit, snapshot, fileSet, helper.block, scope, 0, bodyWidth, result, unions)
		if err != nil {
			return "", err
		}
		paramList := ""
		if len(params) > 0 {
			paramList = ", " + strings.Join(params, ", ")
		}
		castText := ""
		if len(casts) > 0 {
			castText = strings.Join(casts, "\n") + "\n"
		}
		texts = append(texts, fmt.Sprintf(helperFunction, returnType, helperCName(helper.decl, helper.substitutions), paramList, castText, statements))
	}
	return strings.Join(texts, "\n"), nil
}

// helperSignature computes one reachable helper's C signature — the single
// source of truth shared by the forward-declaration pass
// (buildHelperPrototypes) and the definition pass (buildHelperFunctions), so
// the ~150-line parameter/result-type switch logic lives in exactly one place
// and a prototype and its definition can never disagree on a parameter's C
// type or the C return type. It returns:
//   - params: the C declaration of each parameter, in order (each
//     "<cType> pebble_local_<id>"; the prototype and the definition both use
//     the identical parameter names, so the two can never mismatch).
//   - scope: the locals-scope seeding the parameters produce — one
//     localInfo per parameter symbol, exactly the entry a definition's body
//     (buildBlock) needs to resolve references to its own parameters; the
//     prototype pass discards it, the definition pass seeds its scope with it.
//   - bodyWidth: the width the helper's body is built at — the entry's width
//     unless the helper's own result type is another supported integer
//     builtin (an i64 helper inside an i32 entry builds at i64).
//   - returnType: the C return type (cType(bodyWidth) for a scalar result,
//     the aggregate typedef name for a tuple/struct/slice/optional/function-
//     typed result, the fixed int32_t / bool / PebbleStr / void / pointer C
//     type names for the other result shapes).
//   - result: the resultInfo describing the result shape, so a definition's
//     tail-position Return is built by the right builder (buildExpr,
//     buildAggregateReturnValue, buildCharOperand, buildOptionalValue, etc.).
//
// A helper whose ResultType is a tuple/struct is declared with its
// aggregate's own typedef name as the C return type instead of the entry's
// scalar cType(width), and its body is built with a resultInfo recording that
// aggregate so the tail-position Return is built by buildAggregateReturnValue
// rather than buildExpr. A scalar-result helper is unchanged: cType(width) and
// resultInfo{kind: width}, so its emitted text is byte-identical to before
// this slice. A void-result helper (10.33) is declared with the C return type
// "void" and resultInfo{kind: types.Void}. The tuple/struct shape is
// validated wherever its typedef is built (buildTupleTypedef /
// buildStructTypedef), exactly like a tuple/struct parameter's, and an
// optional result's payload shape is likewise validated wherever its typedef
// is built (buildOptionalTypedef / optionalPayloadCType).
func helperSignature(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, helper helperInfo, width types.BuiltinKind) (params []string, scope map[symbol.SymbolID]localInfo, bodyWidth types.BuiltinKind, returnType string, result resultInfo, err error) {
	scope = make(map[symbol.SymbolID]localInfo, len(helper.decl.Parameters))
	params = make([]string, 0, len(helper.decl.Parameters))
	for _, param := range helper.decl.Parameters {
		switch {
		case isWidth(snapshot, width, param.Type):
			params = append(params, cType(width)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: width}
		case isCompatibleIntegerWidth(snapshot, width, param.Type):
			// A parameter whose own resolved integer width shares the entry's
			// C representation (a generic specialization substituted its type
			// parameter with a concrete fixed-width integer — clamp[i32] from
			// an `int`-declared entry: a distinct builtin from the entry's own
			// width that is textually the same C type). The C parameter is
			// declared with cType(width), which is byte-identical to the
			// parameter's own cType (the compatibility check guarantees the
			// two match), and the parameter seeds the callee's locals scope at
			// its OWN width (localInfo{kind: paramWidth}, exactly as a local
			// declared at that width is seeded by buildScalarInitializeCore),
			// so a reference to the parameter inside the body resolves through
			// the existing buildExpr machinery at the parameter's own width
			// unchanged, and a call site's argument — built by
			// buildCallArgument at that same parameter width — passes an
			// identically-typed C value.
			paramWidth, _ := resolvedBuiltin(snapshot, param.Type)
			params = append(params, cType(paramWidth)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: paramWidth}
		case isUint(snapshot, param.Type):
			params = append(params, "uint64_t"+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: types.Uint}
		case isU64(snapshot, param.Type):
			// A u64-typed parameter seeds the callee's locals scope as a
			// u64 local (localInfo{kind: types.U64}), exactly as a u64
			// local's Initialize does, so a reference to the parameter
			// inside the body resolves through the existing buildExpr
			// machinery at the u64 width unchanged. The C parameter is
			// declared as the fixed uint64_t — the same C type a u64
			// local is declared with — so passing a u64 by value is
			// trivially valid C.
			params = append(params, "uint64_t"+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: types.U64}
		case isFixedWidthInteger(snapshot, param.Type):
			// A parameter of ANY other concrete fixed-width integer
			// builtin — a u8, i8, u16, i16, u32, or i32/i64 that is
			// neither the entry's own width nor a C-compatible sibling —
			// seeds the callee's locals scope as a local of that exact
			// width (localInfo{kind: paramWidth}, exactly as a local
			// declared at that width is seeded by
			// buildScalarInitializeCore), so a reference to the parameter
			// inside the body resolves through the existing buildExpr
			// machinery at the parameter's OWN width unchanged (buildExpr's
			// width gate admits a node of any fixed-width integer type at
			// that width — the same per-operand width resolution the
			// switch-subject fix and struct-field reads use). The C
			// parameter is declared at the parameter's own C type
			// (cType(paramWidth) — uint8_t for a u8 parameter, int16_t for
			// an i16 one, and so on), the same C type a local of that
			// width is declared with, and a call site's argument — built by
			// buildCallArgument at that same parameter width — passes an
			// identically-typed C value. The abstract `int` and word-sized
			// `uint` builtins are excluded by the predicate and keep their
			// dedicated handling above (isWidth/isCompatibleIntegerWidth
			// for an `int`-declared entry, isUint for uint); u64 is also
			// covered but the isU64 case above takes precedence.
			paramWidth, _ := resolvedBuiltin(snapshot, param.Type)
			params = append(params, cType(paramWidth)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: paramWidth}
		case isBool(snapshot, param.Type):
			params = append(params, fmt.Sprintf("bool pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: types.Bool}
		case isChar(snapshot, param.Type):
			// A char-typed parameter seeds the callee's locals scope as a
			// char local (localInfo.isChar), exactly as a char local's
			// Initialize does, so a reference to the parameter inside the
			// body resolves through the existing buildCharOperand
			// machinery unchanged (read in any of the six comparisons,
			// forwarded by a char-returning helper's return, or passed to
			// another char parameter). The C parameter is declared as the
			// fixed int32_t — the same C type a char local is declared
			// with, no typedef involved — so passing a char by value is
			// trivially valid C.
			params = append(params, "int32_t"+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{isChar: true}
		case isFloat(snapshot, param.Type):
			// An f32/f64-typed parameter seeds the callee's locals scope as
			// a float local (localInfo{kind: kind}, where kind is the
			// parameter's own f32/f64 kind — the exact localInfo shape
			// buildScalarInitializeCore records for a float local of that
			// kind), so a reference to the parameter inside the body
			// resolves through the existing buildFloatExpr machinery at that
			// same float kind unchanged (read in any of the six comparisons,
			// used in float arithmetic, forwarded by a float-returning
			// helper's return, or passed to another float parameter). The C
			// parameter is declared at the parameter's own float C type
			// (floatCType — float for f32, double for f64), the same C type
			// a float local is declared with, no typedef involved — so
			// passing a float by value is trivially valid C.
			floatKind := resolvedFloatKind(snapshot, param.Type)
			params = append(params, floatCType(floatKind)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{kind: floatKind}
		case isTuple(snapshot, param.Type):
			// A tuple-typed parameter seeds the callee's locals scope as a
			// tuple local (localInfo.tuple), exactly as a tuple local's
			// Initialize does, so element reads inside the body resolve
			// through the existing Load(TuplePlace) machinery unchanged.
			// The C parameter is declared with the tuple's own struct
			// typedef name, so passing the whole tuple by value is trivially
			// valid C (a call site passes a tuple-typed local's own name).
			params = append(params, tupleTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{tuple: param.Type}
		case isEnumType(unit, snapshot, param.Type):
			// An enum/union-typed parameter seeds the callee's locals scope
			// as an enum local (localInfo.enumType), exactly as an
			// enum/union local's Initialize does, so a switch subject
			// referencing the parameter and a narrowed union-variant payload
			// read (`r.Ok` inside `case .Ok:` — the Load(FieldPlace) shape
			// buildStructFieldRead resolves) inside the body resolve through
			// the existing enum/union machinery unchanged. The C parameter is
			// declared with the type's own typedef name — pebble_union_
			// <typeID>_t for a tagged union (a payload-carrying construction
			// exists somewhere in the reachable program, so the union typedef
			// pair is emitted; see isTaggedUnionType), pebble_enum_<typeID>_t
			// for a plain enum — so passing the whole value by value is
			// trivially valid C (a call site passes an enum/union-typed
			// value of the same C type).
			ctypeName := enumTypeName(param.Type)
			if isTaggedUnionType(unit, snapshot, param.Type) {
				ctypeName = unionTypeName(param.Type)
			}
			params = append(params, ctypeName+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{enumType: param.Type}
		case isStruct(snapshot, param.Type):
			// A struct-typed parameter seeds the callee's locals scope as a
			// struct local (localInfo.structType), exactly as a struct
			// local's Initialize does, so field reads inside the body
			// resolve through the existing Load(FieldPlace) machinery
			// unchanged, declared with the struct's own struct typedef name.
			params = append(params, runtimeTypeName(unit, snapshot, param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{structType: param.Type, runtimeType: param.Type}
		case isArray(snapshot, param.Type):
			params = append(params, arrayTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{array: param.Type, arrayWrapped: true}
		case isSlice(snapshot, param.Type):
			// A slice-typed parameter (10.38) seeds the callee's locals
			// scope as a slice local (localInfo.sliceType), exactly as a
			// slice local's Initialize does, so an index of the parameter
			// inside the body (`s[0]`) resolves through the existing
			// Load(CheckedIndexPlace) machinery a slice local uses
			// unchanged, declared with the slice type's own struct typedef
			// name (pebble_slice_<typeID>_t — the same typedef 10.37
			// builds for a slice local, no new typedef shape needed). The
			// element type is validated to be the entry's width or bool by
			// validateHelperSignature, so the typedef always builds.
			params = append(params, sliceTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{sliceType: param.Type}
		case isOptional(snapshot, param.Type):
			// An optional-typed parameter seeds the callee's locals scope
			// as an optional local (localInfo.optional), exactly as an
			// optional local's Initialize does, so a reference to the
			// parameter inside the body — a `.has_value` read, a `!`
			// force-unwrap, forwarding it in a return or as another
			// call's argument — resolves through the existing optional-
			// local machinery (buildOptionalHasValue, the
			// CheckedOptionalUnwrap path, buildOptionalReturnValue)
			// completely unchanged, no new read-path code. The C
			// parameter is declared with the optional type's own struct
			// typedef name (pebble_optional_<typeID>_t — the same
			// typedef 10.21 builds for an optional local, no new typedef
			// shape needed), so passing the whole optional by value is
			// trivially valid C (a call site passes an optional-typed
			// value of the same C type). The payload type is validated
			// wherever its typedef is built (buildOptionalTypedef via
			// optionalPayloadCType) — the same coverage an optional
			// local's payload gets.
			params = append(params, optionalTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{optional: param.Type}
		case isStr(snapshot, param.Type):
			// A str-typed parameter seeds the callee's locals scope as a
			// str local (localInfo.isStr), exactly as a str local's
			// Initialize does, so a reference to the parameter inside the
			// body resolves through the existing buildStrOperand machinery
			// unchanged (read in a ==/!= comparison, forwarded by a
			// str-returning helper's return, or passed to another str
			// parameter). The C parameter is declared as the runtime ABI's
			// fixed PebbleStr type — the same C type a str local is declared
			// with, no typedef involved — so passing a str by value is
			// trivially valid C.
			params = append(params, "PebbleStr"+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{isStr: true}
		case isPointer(snapshot, param.Type):
			// A pointer-typed parameter seeds the callee's locals scope
			// as a pointer local (localInfo.pointerType). The C parameter
			// is declared with the pointer type's own C type name
			// (pointee_c_type *), so passing a pointer by value is
			// trivially valid C. pointerTypeName takes the pointee, not
			// the pointer type itself, so the pointee must be extracted
			// first.
			paramPointeeTypeID, paramPointeeOK := pointerPointeeType(snapshot, param.Type)
			ctypeName := ""
			if paramPointeeOK {
				ctypeName = pointerTypeNameForUnit(st, unit, snapshot, paramPointeeTypeID)
			}
			if ctypeName == "" {
				return nil, nil, 0, "", resultInfo{}, fmt.Errorf("called function symbol %d parameter (symbol %d) has unsupported pointer type %s", helper.decl.Symbol, param.Symbol, describeType(snapshot, param.Type))
			}
			params = append(params, ctypeName+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{pointerType: param.Type}
		case isFunctionType(snapshot, param.Type):
			// A function-typed parameter (function-types slice 3) seeds
			// the callee's locals scope as a function-typed local
			// (localInfo.functionType — the SAME field a function-typed
			// local's Initialize uses), so a reference to the parameter
			// inside the body — as an indirect call's callee (`f(x, y)`),
			// forwarded as another function-typed parameter's argument, or
			// returned by a function-result helper — resolves through the
			// existing buildFunctionValue machinery unchanged (its
			// SymbolValue case reads localInfo.functionType exactly like
			// buildFunctionLocalDeclaration's scope entry does). The C
			// parameter is declared with the function type's own pointer
			// typedef (pebble_fnptr_<typeID>_t, slice 1's functionTypeName
			// — the same C type a function-typed local is declared with),
			// so passing a function value by value is trivially valid C
			// (a call site passes a function value of the same C type).
			// The function type's own signature is validated by
			// validateHelperSignature above, so the typedef always builds.
			params = append(params, functionTypeName(param.Type)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			scope[param.Symbol] = localInfo{functionType: param.Type}
		default:
			// validateHelperSignature rules any unsupported parameter out
			// before a reachable helper is ever built, so this branch is
			// defense for hand-built IR only.
			return nil, nil, 0, "", resultInfo{}, fmt.Errorf("called function symbol %d parameter (symbol %d) has type %s, want a fixed-width integer (%s, uint, or u64), bool, char, str, f32, f64, a tuple/struct type, a slice type, a pointer type, an optional type, or a function type", helper.decl.Symbol, param.Symbol, describeType(snapshot, param.Type), wantName(width))
		}
	}
	bodyWidth = width
	if resultWidth, integerResult := resolvedBuiltin(snapshot, helper.decl.ResultType); integerResult && cType(resultWidth) != "" {
		bodyWidth = resultWidth
	}
	returnType = cType(bodyWidth)
	result = resultInfo{kind: bodyWidth}
	switch {
	case isVoid(snapshot, helper.decl.ResultType):
		// A void-result helper (10.33) is declared with the C return type
		// "void" — a void call has no value to return, so its body's tail
		// is an ImplicitReturn that emits nothing (buildBlock's ImplicitReturn
		// case). resultInfo records types.Void so buildBlock knows the tail
		// is a legal fall-through rather than a missing return, and the
		// helper is only ever reached by a bare discarded-expression
		// statement call (buildExpressionStatement), never as a value.
		returnType = "void"
		result = resultInfo{kind: types.Void}
	case isBool(snapshot, helper.decl.ResultType):
		// A bool-result helper (added for the function-types slice, whose
		// required bool-parameter/bool-result function-type test needs a
		// bool-returning function to be emittable as a helper) is declared
		// with the C return type "bool" and resultInfo{kind: types.Bool} so
		// buildBlock's tail-position Return builds its value via
		// buildBoolExpr rather than buildExpr, which would reject a
		// bool-typed value.
		returnType = "bool"
		result = resultInfo{kind: types.Bool}
	case isChar(snapshot, helper.decl.ResultType):
		// A char-result helper (10.41) is declared with the fixed C
		// int32_t as its C return type — the same C type a char local is
		// declared with, independent of the entry's resolved width, no
		// typedef involved — and resultInfo records the char shape so
		// buildBlock's tail-position Return builds its value via
		// buildCharOperand (a char literal, a SymbolValue naming a
		// char-typed local, or a call to another char-returning helper)
		// rather than buildExpr, which would reject a char-typed value.
		returnType = "int32_t"
		result = resultInfo{isChar: true}
	case isFloat(snapshot, helper.decl.ResultType):
		// An f32/f64-result helper (this slice) is declared with the C
		// return type of the result's own float kind (floatCType — float
		// for f32, double for f64, the same C type a float local is
		// declared with, no typedef involved) and resultInfo records the
		// float kind (resultInfo{kind: kind}) — the exact resultInfo shape
		// the entry's own f32/f64-returning main threads — so buildBlock's
		// tail-position Return builds its value via buildFloatExpr (a float
		// literal, a SymbolValue naming a float-typed local of the same
		// kind, float arithmetic, or a call to another float-returning
		// helper) rather than buildExpr, which would reject a float-typed
		// value.
		floatKind := resolvedFloatKind(snapshot, helper.decl.ResultType)
		returnType = floatCType(floatKind)
		result = resultInfo{kind: floatKind}
	case isTuple(snapshot, helper.decl.ResultType):
		returnType = tupleTypeName(helper.decl.ResultType)
		result = resultInfo{tuple: helper.decl.ResultType}
	case isTaggedUnionType(unit, snapshot, helper.decl.ResultType):
		// A tagged-union-result helper (the "or tagged union" half of the
		// enum/union helper-return gap) is declared with the union's own
		// pebble_union_<typeID>_t as its C return type — the same typedef a
		// tagged-union local is declared with, no new typedef shape needed —
		// and resultInfo records the union-result shape
		// (resultInfo.unionType, mirroring how resultInfo.enumType was added
		// for plain-enum results) so buildBlock's tail-position Return builds
		// its value via buildUnionValueExpr (a SymbolValue naming a
		// union-typed local, a fresh variant construction, a union-typed
		// struct field read, or a union-payload optional force-unwrap)
		// rather than buildAggregateReturnValue, which would reject an
		// EnumVariantValue/VariantConstruct as a struct return. This must
		// precede the isEnumType case below (and the isStruct case): a tagged
		// union is enum-shaped exactly like a plain enum and Nominal like a
		// struct, but its real C type is the tag-plus-payload struct, never
		// the bare tag enum or a struct typedef.
		returnType = unionTypeName(helper.decl.ResultType)
		result = resultInfo{unionType: helper.decl.ResultType}
	case isEnumType(unit, snapshot, helper.decl.ResultType):
		// A plain-enum-result helper (the reported gap: an enum-typed return
		// previously fell through to the isStruct case below — an enum is
		// Nominal, so isStruct reports true for it — and got a struct-flavored
		// rejection on a perfectly valid enum return) is declared with the
		// enum's own pebble_enum_<typeID>_t as its C return type — the same
		// typedef an enum local is declared with, no new typedef shape needed —
		// and resultInfo records the enum-result shape (resultInfo.enumType,
		// a field added alongside this case) so buildBlock's tail-position
		// Return builds its value via buildEnumValue (a variant literal, a
		// SymbolValue naming an enum-typed local or parameter, an
		// integer-to-enum cast, an enum-typed struct field read, or an
		// enum-payload optional force-unwrap) rather than buildExpr, which
		// would reject an enum-typed value.
		returnType = enumTypeName(helper.decl.ResultType)
		result = resultInfo{enumType: helper.decl.ResultType}
	case isStruct(snapshot, helper.decl.ResultType):
		returnType = runtimeTypeName(unit, snapshot, helper.decl.ResultType)
		result = resultInfo{structType: helper.decl.ResultType}
	case isArray(snapshot, helper.decl.ResultType):
		returnType = arrayTypeName(helper.decl.ResultType)
		result = resultInfo{arrayType: helper.decl.ResultType}
	case isStr(snapshot, helper.decl.ResultType):
		// A str-result helper (10.36) is declared with the runtime ABI's
		// fixed PebbleStr as its C return type — the same C type a str
		// local is declared with, no typedef involved — and resultInfo
		// records the str shape so buildBlock's tail-position Return builds
		// its value via buildStrOperand (a SymbolValue naming a str local, a
		// string literal, or a call to another str-returning helper) rather
		// than buildExpr, which would reject a str-typed value.
		returnType = "PebbleStr"
		result = resultInfo{isStr: true}
	case isSlice(snapshot, helper.decl.ResultType):
		// A slice-result helper (10.38) is declared with the slice type's
		// own struct typedef name (pebble_slice_<typeID>_t) as its C return
		// type — the same typedef 10.37 builds for a slice local, no new
		// typedef shape needed — and resultInfo records the slice shape so
		// buildBlock's tail-position Return builds its value via
		// buildSliceReturnValue (a SymbolValue naming a slice-typed local,
		// or a fresh CheckedSlice construction) rather than buildExpr,
		// which would reject a slice-typed value. The element type is
		// validated to be the entry's width or bool by
		// validateHelperSignature, so the typedef always builds.
		returnType = sliceTypeName(helper.decl.ResultType)
		result = resultInfo{sliceType: helper.decl.ResultType}
	case isPointer(snapshot, helper.decl.ResultType):
		// A pointer-result helper is declared with the pointer type's own
		// C type name as its return type. pointerTypeName takes the
		// pointee, not the pointer type itself (it appends " *" to the
		// pointee's own C type), so the pointee must be extracted first.
		// The body's tail-position Return builds its value via buildExpr
		// (which now handles pointer-typed nodes: AddressOf, SymbolValue,
		// NilPointer, DirectCall). resultInfo records the pointer shape
		// so buildBlock's tail-position Return can build the value
		// correctly.
		pointeeTypeID, ok := pointerPointeeType(snapshot, helper.decl.ResultType)
		if !ok {
			return nil, nil, 0, "", resultInfo{}, fmt.Errorf("called function symbol %d has unsupported pointer result type %s", helper.decl.Symbol, describeType(snapshot, helper.decl.ResultType))
		}
		returnType = pointerTypeNameForUnit(st, unit, snapshot, pointeeTypeID)
		result = resultInfo{pointerType: helper.decl.ResultType}
	case isOptional(snapshot, helper.decl.ResultType):
		// An optional-result helper is declared with the optional type's
		// own struct typedef name (pebble_optional_<typeID>_t) as its C
		// return type — the same typedef 10.21 builds for an optional
		// local, no new typedef shape needed — and resultInfo records the
		// optional shape so buildBlock's tail-position Return builds its
		// value via buildOptionalValue (a SymbolValue naming an
		// optional-typed local, a fresh SomeOptional/NoneOptional/
		// OptionalInject construction, or a call to another
		// optional-returning helper) rather than buildExpr, which would
		// reject an optional-typed value. The payload type is validated
		// wherever its typedef is built (buildOptionalTypedef via
		// optionalPayloadCType), exactly like a tuple/struct result's
		// internal shape.
		returnType = optionalTypeName(helper.decl.ResultType)
		result = resultInfo{optionalType: helper.decl.ResultType}
	case isFunctionType(snapshot, helper.decl.ResultType):
		// A function-result helper (function-types slice 3) is declared
		// with the function type's own pointer typedef
		// (pebble_fnptr_<typeID>_t) as its C return type — the same
		// typedef slice 1 builds for a function-typed local, no new typedef
		// shape needed — and resultInfo records the function-result shape
		// (resultInfo.functionType, mirroring how resultInfo.optionalType
		// was added for optional results) so buildBlock's tail-position
		// Return builds its value via buildFunctionValue (a bare function
		// reference, a function-typed local or parameter forward, a
		// function-typed struct field forward, or a call to another
		// function-result helper) rather than buildExpr, which would
		// reject a function-typed value. The function type's own signature
		// is validated by validateHelperSignature above, so the typedef
		// always builds.
		returnType = functionTypeName(helper.decl.ResultType)
		result = resultInfo{functionType: helper.decl.ResultType}
	}
	return params, scope, bodyWidth, returnType, result, nil
}

// buildHelperPrototypes builds one C forward declaration (prototype) per
// reachable helper, in the same order as the definitions buildHelperFunctions
// emits, using the exact same helperSignature source of truth — a prototype
// and its definition always agree on every parameter's C type and the C
// return type, and reuse the identical pebble_local_<id> parameter names, so
// the mandated -Wall -Wextra -Werror build cannot warn about a prototype
// disagreeing with its definition. The prototypes are emitted before ANY
// definition, which is what makes recursive/mutually-recursive calls legal C:
// a function's own body (or another function's body) may call a function
// whose definition comes later in the file, because the earlier prototype
// already declares it. The parameter names are the same ones the definition
// uses, so no -Wunused-parameter or other pedantry can fire on the prototype
// text itself (parameter names in a declaration are inert, but keeping them
// identical is the conservative choice). Every helper is reachable (the
// reachability walk emits exactly the reachable set), so a prototype never
// precedes a static function with no call site, and no -Wunused-function
// warning appears.
func buildHelperPrototypes(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, helpers []helperInfo, width types.BuiltinKind) (string, error) {
	prototypes := make([]string, 0, len(helpers))
	for _, helper := range helpers {
		params, _, _, returnType, _, err := helperSignature(st, unit, snapshot, helper, width)
		if err != nil {
			return "", err
		}
		paramList := ""
		if len(params) > 0 {
			paramList = ", " + strings.Join(params, ", ")
		}
		prototypes = append(prototypes, fmt.Sprintf(helperPrototype, returnType, helperCName(helper.decl, helper.substitutions), paramList))
	}
	return strings.Join(prototypes, "\n"), nil
}

// buildAggregateCallInitializer builds a tuple/struct-typed local's declaration
// whose initializer is a DirectCall to a helper returning the same aggregate
// type (10.26): `let t (i32, i32) = helperReturningTuple();`. This is the one
// position in which calling a tuple/struct-returning helper is supported — the
// direct initializer of a matching aggregate-typed local declaration. The
// call's result type is the DirectCall node's own Type, which is the callee's
// resolved result type (confirmed against a real fixture), and it must be
// exactly the local's declared type — double-checked against the callee's
// declared ResultType (defense for hand-built IR), so the emitted C never
// initializes a local of one aggregate type from a call returning another. The
// call itself is built by buildDirectCall, the same call-building machinery
// buildExpr's DirectCall case uses, so context and argument handling are
// identical to a scalar call — only the result type differs. wantTuple selects
// the tuple grammar (the local is declared pebble_tuple_<typeID>_t and its
// scope entry records localInfo{tuple}) over the struct grammar
// (pebble_struct_<typeID>_t and localInfo{structType}). Like every local, the
// declaration is followed by a (void) cast against -Wunused-variable.
func buildAggregateCallInitializer(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, wantTuple bool) (string, error) {
	calleeDecl, err := findCallDeclaration(unit, snapshot, initValue)
	if err != nil {
		return "", err
	}
	if calleeDecl.ResultType != initValue.Type {
		what := "tuple"
		if !wantTuple {
			what = "struct"
		}
		return "", fmt.Errorf("%s declares a %s-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, what, describeType(snapshot, initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
	}
	callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	if wantTuple {
		scope[statement.Symbol] = localInfo{tuple: initValue.Type}
		return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, tupleTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
	}
	scope[statement.Symbol] = localInfo{structType: initValue.Type}
	return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, structTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
}

func buildArrayCallInitializer(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	calleeDecl, err := findCallDeclaration(unit, snapshot, initValue)
	if err != nil {
		return "", err
	}
	if calleeDecl.ResultType != initValue.Type {
		return "", fmt.Errorf("%s declares an array-typed local of type %s initialized from a call whose result type is %s", context, describeType(snapshot, initValue.Type), describeType(snapshot, calleeDecl.ResultType))
	}
	callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{array: initValue.Type, arrayWrapped: true}
	return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, arrayTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
}

// buildOptionalCallInitializer builds an optional-typed local's declaration
// whose initializer is a DirectCall to a helper returning the same optional
// type: `var o ?int = f();`. This mirrors the tuple/struct
// buildAggregateCallInitializer shape exactly: an optional result, like a
// tuple/struct result, is a C struct type read back whole at the call site, so
// the call expression (built by buildDirectCall, the same call-building
// machinery buildExpr's DirectCall case uses) is the local's whole initializer
// — a `pebble_optional_<typeID>_t pebble_local_<symbol> = f(ctx, ...);`
// assignment. The call's result type is the DirectCall node's own Type, which
// is the callee's resolved result type, and it must be exactly the local's
// declared type — double-checked against the callee's declared ResultType
// (defense for hand-built IR), so the emitted C never initializes a local of
// one optional type from a call returning another. The local's scope entry
// records its optional type (localInfo with optional set), so a later
// has_value read or force-unwrap resolves the optional type being unwrapped.
// Like every local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildOptionalCallInitializer(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	calleeDecl, err := findCallDeclaration(unit, snapshot, initValue)
	if err != nil {
		return "", err
	}
	if calleeDecl.ResultType != initValue.Type {
		return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, describeType(snapshot, initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
	}
	callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{optional: initValue.Type}
	return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, optionalTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
}

// buildEnumCallInitializer builds a plain enum-typed local's declaration whose
// initializer is a DirectCall to a helper returning the same enum type:
// `let c Color = pick();`. This is the call-site half of the enum helper-return
// gap: once helperSignature admits an enum result type and declares the helper
// with the enum's own pebble_enum_<typeID>_t C return type, a caller binding
// that result into a matching enum-typed local needs this direct-initializer
// shape (mirroring the tuple/struct buildAggregateCallInitializer and the
// optional buildOptionalCallInitializer). The call's result type is the
// DirectCall node's own Type, which is the callee's resolved result type, and
// it must be exactly the local's declared type — double-checked against the
// callee's declared ResultType (defense for hand-built IR), so the emitted C
// never initializes a local of one enum type from a call returning another.
// The call itself is built by buildDirectCallWithPre, the same call-building
// machinery buildExpr's DirectCall case uses, and the call expression is the
// local's whole initializer — a `pebble_enum_<typeID>_t pebble_local_<symbol>
// = f(ctx, ...);` assignment, trivially valid C since the helper's C return
// type is the local's own typedef (see helperSignature's isEnumType case). The
// local's scope entry records its enum type (localInfo.enumType), so a later
// switch subject, reference, or comparison resolves the enum type being used.
// Like every local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildEnumCallInitializer(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	calleeDecl, err := findCallDeclaration(unit, snapshot, initValue)
	if err != nil {
		return "", err
	}
	if calleeDecl.ResultType != initValue.Type {
		return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, describeType(snapshot, initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
	}
	callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{enumType: initValue.Type}
	return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, enumTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
}

// buildUnionCallInitializer builds a tagged-union-typed local's declaration
// whose initializer is a DirectCall to a helper returning the same union type:
// `let c Choice = pick();`. This is the call-site half of the tagged-union
// helper-return support (the "or tagged union" half of the enum/union
// helper-return gap): once helperSignature admits a tagged-union result type
// and declares the helper with the union's own pebble_union_<typeID>_t C
// return type, a caller binding that result into a matching union-typed local
// needs this direct-initializer shape, mirroring buildEnumCallInitializer. The
// call's result type is the DirectCall node's own Type, which is the callee's
// resolved result type, and it must be exactly the local's declared type —
// double-checked against the callee's declared ResultType (defense for
// hand-built IR). The call itself is built by buildDirectCallWithPre, the same
// call-building machinery buildExpr's DirectCall case uses, and the call
// expression is the local's whole initializer — a `pebble_union_<typeID>_t
// pebble_local_<symbol> = f(ctx, ...);` assignment, trivially valid C since
// the helper's C return type is the local's own typedef (see helperSignature's
// isTaggedUnionType case). The local's scope entry records its union type
// (localInfo.enumType, exactly as buildUnionLocalDeclaration records it — a
// tagged union is enum-shaped like a plain enum), so a later switch subject,
// reference, or comparison resolves the union type being used. Like every
// local, the declaration is followed by a (void) cast against -Wunused-variable.
func buildUnionCallInitializer(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	calleeDecl, err := findCallDeclaration(unit, snapshot, initValue)
	if err != nil {
		return "", err
	}
	if calleeDecl.ResultType != initValue.Type {
		return "", fmt.Errorf("%s declares a union-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, describeType(snapshot, initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
	}
	callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{enumType: initValue.Type}
	return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, unionTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
}

func buildIndirectCall(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	if len(node.Children) < 1 || node.ContextAction != tir.ContextForward {
		return "", fmt.Errorf("indirect call has invalid callee or context action")
	}
	placeNode, allocatorCallee, ok := indirectCalleePlace(unit, node)
	if !ok {
		return "", fmt.Errorf("indirect call has invalid callee")
	}
	var base string
	var owner types.TypeID
	var member symbol.SymbolID
	if allocatorCallee && placeNode.Kind == tir.FieldPlace {
		var err error
		base, owner, err = buildPlaceLValue(st, unit, snapshot, fileSet, placeNode.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		member = placeNode.Member
	} else if allocatorCallee && placeNode.Kind == tir.FieldValue && len(placeNode.Children) == 1 {
		receiver, ok := unit.Node(placeNode.Children[0])
		if !ok {
			return "", fmt.Errorf("invalid allocator receiver")
		}
		var err error
		base, err = buildRuntimeValueNode(st, unit, snapshot, fileSet, placeNode.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		owner, member = receiver.Type, placeNode.Member
	}
	if !allocatorCallee {
		// The general indirect call: the callee is an ordinary function-typed
		// value, not an allocator function field. This is the f(1, 2) shape —
		// a call through a function-typed local (or a bare function value),
		// distinct from the allocator-specific FieldPlace/FieldValue shape
		// above. Both the callee and every argument are built under the
		// callee's own function type (buildFunctionIndirectCall), and the
		// allocator path above is completely untouched.
		return buildFunctionIndirectCall(st, unit, snapshot, fileSet, node, placeNode, locals, width)
	}
	field, mapped := runtimeFieldName(unit, owner, member)
	if !mapped || (member != unit.Runtime().AllocatorAlloc && member != unit.Runtime().AllocatorRealloc && member != unit.Runtime().AllocatorFree) {
		return "", fmt.Errorf("indirect call callee is not an allocator function field")
	}
	args := make([]string, 0, len(node.Children)-1)
	for _, id := range node.Children[1:] {
		arg, err := buildRuntimeCallArg(st, unit, snapshot, fileSet, id, locals, width)
		if err != nil {
			return "", err
		}
		args = append(args, arg)
	}
	cast := "PebbleFreeFn"
	if member == unit.Runtime().AllocatorAlloc {
		cast = "PebbleAllocFn"
	}
	if member == unit.Runtime().AllocatorRealloc {
		cast = "PebbleReallocFn"
	}
	if len(args) > 0 {
		args[0] = "(PebbleContext *)" + args[0]
	}
	return fmt.Sprintf("((%s)(%s.%s))(%s)", cast, base, field, strings.Join(args, ", ")), nil
}

// buildFunctionIndirectCall builds the C expression text for the GENERAL
// indirect call through an ordinary function-typed value — the f(1, 2) shape —
// where the callee (Children[0], already unwrapped past SourceAlias/Load by
// buildIndirectCall) is a function-typed value rather than an allocator
// function field, and every argument (Children[1:]) is built under the
// callee's own function type's parameter list, which REPLACES the
// tir.Node.Parameters an ordinary fixed function declaration would have. The
// callee's function type is the IndirectCall's FunctionType field (its own
// Type is the call's RESULT type, distinct from the callee's type — confirmed
// against a real fixture). The emitted C is
//
//	<callee-expr>(ctx, <arg0>, <arg1>, ...)
//
// for a Pebble-convention function type: the callee value's own C type IS
// pebble_fnptr_<typeID>_t (a function-typed local is declared as exactly that
// typedef), so calling it directly with the threaded context and its declared
// parameters is trivially valid C, no cast needed. Each argument's grammar is
// decided by buildCallArgument from its parameter's resolved type — the exact
// same per-parameter-type dispatch an ordinary fixed function's call uses.
// calleeNode is the unwrapped callee value node (a SymbolValue naming a
// function-typed local, or a HoistedFunctionValue), built by buildFunctionValue.
func buildFunctionIndirectCall(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node, calleeNode tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	calleeExpr, err := buildFunctionValue(st, unit, snapshot, fileSet, calleeNode, locals, "entry function body indirect call", width)
	if err != nil {
		return "", err
	}
	fnType := node.FunctionType
	if !isFunctionType(snapshot, fnType) {
		return "", fmt.Errorf("indirect call has a callee of type %s, want a function type", describeType(snapshot, fnType))
	}
	if err := validateFunctionTypeSignature(snapshot, width, fnType); err != nil {
		return "", err
	}
	key, ok := snapshot.Key(fnType)
	if !ok {
		return "", fmt.Errorf("indirect call callee type %d is not in the type snapshot", fnType)
	}
	_, parameters, _, _, ok := key.Function()
	if !ok {
		return "", fmt.Errorf("indirect call callee type %s is not a function type", describeType(snapshot, fnType))
	}
	if len(node.Children)-1 != len(parameters) {
		return "", fmt.Errorf("indirect call passes %d argument(s), want %d (the callee's function type %s declares %d parameter(s))", len(node.Children)-1, len(parameters), describeType(snapshot, fnType), len(parameters))
	}
	args := make([]string, 0, len(parameters))
	// A C-convention indirect callee (an extern function referenced as a
	// value, or a C-convention function pointer) declares its str parameters
	// against the libc header, so a str argument must be lowered to
	// `const char *` exactly like a direct C-convention call's (see
	// buildCallArguments); the callee's own convention decides.
	convention, _, _, _, _ := key.Function()
	for i, id := range node.Children[1:] {
		// Nested mode: an indirect call is a pure expression position (it may
		// appear anywhere a function value's result is consumed), so an inline
		// slice-construction argument's temp declaration is folded into a GNU
		// statement-expression argument by buildSliceArgument (nested == true
		// means no pre is ever returned, matching this function's single-string
		// return).
		_, arg, err := buildCallArgument(st, unit, snapshot, fileSet, node.Symbol, i, id, tir.Parameter{Type: parameters[i]}, convention == types.C, locals, width, true)
		if err != nil {
			return "", err
		}
		args = append(args, arg)
	}
	if len(args) == 0 {
		return fmt.Sprintf("%s(ctx)", calleeExpr), nil
	}
	return fmt.Sprintf("%s(ctx, %s)", calleeExpr, strings.Join(args, ", ")), nil
}

func buildRuntimeCallArg(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("invalid indirect-call argument")
	}
	if node.Kind == tir.IntegerLiteral {
		litWidth, _ := resolvedBuiltin(snapshot, node.Type)
		return integerLiteralText(node.Literal.IntegerNum, litWidth), nil
	}
	if isUint(snapshot, node.Type) {
		return buildUintExpr(st, unit, snapshot, fileSet, id, locals, width)
	}
	return buildExpr(st, unit, snapshot, fileSet, id, locals, width, width)
}

// buildDirectCall builds the C expression text for one tir.DirectCall: a call
// to another Pebble-convention function emitted as
// pebble_fn_<calleeSymbolID>(ctx, <arg0>, <arg1>, ...). Context threading is
// not an explicit IR child — the DirectCall records it as ContextAction
// (ContextForward for a Pebble-convention call) — so, exactly as the old
// backend textually injected `context`, this backend prepends ctx as the first
// C argument itself, the same way pebble_user_main receives it. Each argument
// is built by buildCallArguments, which decides each child's grammar from the
// callee's declared parameter type (the reachability walk has already resolved
// and validated the callee, so the checks here are defense against hand-built
// IR, matching the file's style). The function is shared (10.26) by the two
// call-building sites whose result type differs: buildExpr's DirectCall case
// (a scalar-width call) and buildAggregateCallInitializer (a tuple/struct-
// returning call used as a matching local's declaration initializer) — the
// context and argument handling are identical; only the call's result type
// differs, and that is decided by the caller, never here.
//
// This is the EXPRESSION-position entry point every call site not itself in a
// leading-statement position uses (19 call sites). It routes through the
// nested path (buildDirectCallNested, nested == true), so an inline slice
// construction passed as a call argument (f(a[1:3])) has its temp-declaration
// statement and compound literal folded into a single GNU statement-expression
// argument, `({ <temp decl>; <compound literal> })`, making the whole call a
// single primary expression valid in any pure C expression position. (Before
// the GNU statement-expression change, a non-empty pre was a clean rejection
// here.)
func buildDirectCall(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	return buildDirectCallNested(st, unit, snapshot, fileSet, node, locals, width)
}

// buildDirectCallNested is the pure-expression-position twin of
// buildDirectCallWithPre: it builds the identical call expression, but the
// nested == true path makes each inline slice-construction argument (f(a[1:3]))
// fold its temp-declaration statement and compound literal into a single GNU
// statement-expression argument (see buildSliceArgument /
// sliceConstructionStatementExpr), so the call is one pure expression valid in
// any position, with no pre-statement for the caller to place. Every call site
// not itself in a leading-statement position goes through here (buildDirectCall
// delegates to it), and a non-empty pre is impossible in nested mode — the
// check below is defense against a future pre-producing argument shape that
// forgets to wrap.
func buildDirectCallNested(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	pre, expr, err := buildDirectCallArgs(st, unit, snapshot, fileSet, node, locals, width, true)
	if err != nil {
		return "", err
	}
	if pre != "" {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument requires a temp-declaration statement that nested mode failed to fold into a GNU statement-expression", node.Symbol)
	}
	return expr, nil
}

// buildDirectCallWithPre is the leading-statement-position twin of
// buildDirectCallNested: it builds the identical call expression but ALSO
// returns an indent-free pre-statement that the caller must emit BEFORE the
// call whenever an argument is an inline slice construction (f(a[1:3])) — the
// temp-declaration statement the slice construction needs (the same
// two-statement temp-then-construction shape 10.37's slice local declaration
// uses). Only the leading-statement call sites call this — a bare call
// statement (buildExpressionStatement) and each local's declaration-initializer
// builder — positions that already have a natural place for a preceding
// statement, mirroring how buildScalarInitializeCore's pre mechanism threads a
// force-unwrap temp. Every other call site keeps calling buildDirectCall, which
// routes through the nested (GNU statement-expression) path instead. The
// returned pre has no indent; the caller prepends its own.
func buildDirectCallWithPre(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, string, error) {
	return buildDirectCallArgs(st, unit, snapshot, fileSet, node, locals, width, false)
}

// buildDirectCallArgs is the shared core of buildDirectCallWithPre (nested ==
// false, the leading-statement lowering: an inline slice-construction
// argument's temp declaration is returned as a pre for the caller to place
// before the call) and buildDirectCallNested (nested == true, the
// pure-expression lowering: each such argument is folded into a GNU
// statement-expression instead, so no pre is ever returned).
func buildDirectCallArgs(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, nested bool) (string, string, error) {
	// A call to a compiler-owned builtin function (wrapping_mul_u64 /
	// wrapping_add_u64) is lowered directly to its runtime helper:
	// pebble_rt_wrapping_<op>_u64(<arg0>, <arg1>). The helper is a real C
	// function in the runtime library with plain uint64_t parameters and a
	// uint64_t result and no context parameter, so the call is a bare two-arg
	// C call — no ctx, no pebble_fn_<symbolID> helper (the builtin has no
	// declaration in the unit). Each argument is built by buildExpr at the
	// operand's own resolved u64 width, matching the builtin's two u64
	// parameters; both are pure expressions, so no pre-statement is ever
	// produced.
	if builtinName, builtin := builtinFunctionCName(st, node.Symbol); builtin {
		if len(node.Children) != 2 {
			return "", "", fmt.Errorf("entry function body expression contains a call to builtin function symbol %d with %d argument(s), want exactly two (the wrapping u64 arithmetic builtins take two u64 operands)", node.Symbol, len(node.Children))
		}
		left, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, types.U64, width)
		if err != nil {
			return "", "", fmt.Errorf("entry function body expression contains a call to builtin function symbol %d whose first argument is invalid: %v", node.Symbol, err)
		}
		right, err := buildExpr(st, unit, snapshot, fileSet, node.Children[1], locals, types.U64, width)
		if err != nil {
			return "", "", fmt.Errorf("entry function body expression contains a call to builtin function symbol %d whose second argument is invalid: %v", node.Symbol, err)
		}
		return "", fmt.Sprintf("%s(%s, %s)", builtinName, left, right), nil
	}
	// A C-convention call (direct call to an extern fn declaration) is
	// lowered differently from a Pebble-convention call: no context parameter
	// is threaded, the callee is called by its real C name (malloc, not
	// pebble_fn_<symbolID>), and the result is the function's own C return
	// type (which the consuming expression — a cast, assignment, or
	// discarded-statement — already matches).
	if node.Convention == types.C {
		if node.ContextAction != tir.ContextNone {
			return "", "", fmt.Errorf("entry function body expression contains a C-convention call that records ContextAction %s, want NoContext", node.ContextAction)
		}
		var calleeDecl tir.Node
		var err error
		if len(node.TypeArgs) != 0 {
			calleeDecl, err = findCalledFunctionDeclaration(unit, node.Symbol, node.TypeArgs)
		} else {
			calleeDecl, err = findFunctionDeclaration(unit, node.Symbol, "called function")
			if err != nil {
				calleeDecl, err = findCalledFunctionByResult(unit, node.Symbol, node.Type)
			}
		}
		if err != nil {
			if len(node.TypeArgs) != 0 {
				return "", "", fmt.Errorf("entry function body expression contains a generic call with no matching specialization")
			}
			return "", "", err
		}
		if calleeDecl.Kind != tir.ExternDeclaration {
			return "", "", fmt.Errorf("entry function body expression contains a C-convention call to symbol %d, which is not an extern declaration", calleeDecl.Symbol)
		}
		calleeName, err := externCName(st, calleeDecl)
		if err != nil {
			return "", "", err
		}
		callPre, callArgs, err := buildCallArguments(st, unit, snapshot, fileSet, node, calleeDecl, locals, width, nested)
		if err != nil {
			return "", "", err
		}
		if callArgs == "" {
			return callPre, fmt.Sprintf("%s()", calleeName), nil
		}
		return callPre, fmt.Sprintf("%s(%s)", calleeName, callArgs), nil
	}
	if node.Convention != types.Pebble {
		return "", "", fmt.Errorf("entry function body expression contains a call using the %s calling convention, want Pebble", callingConventionName(node.Convention))
	}
	if node.ContextAction != tir.ContextForward {
		return "", "", fmt.Errorf("entry function body expression contains a call that records ContextAction %s, want ForwardCurrentContext (this backend only lowers Pebble-convention calls that thread the context)", node.ContextAction)
	}
	// The callee's own declaration supplies the parameter list that decides
	// each argument's grammar below (the reachability walk in
	// discoverReachableHelpers has already resolved and validated this
	// callee, so the checks here are defense against hand-built IR,
	// matching the file's style).
	var calleeDecl tir.Node
	var err error
	if len(node.TypeArgs) != 0 {
		calleeDecl, err = findCalledFunctionDeclaration(unit, node.Symbol, node.TypeArgs)
	} else {
		calleeDecl, err = findFunctionDeclaration(unit, node.Symbol, "called function")
		if err != nil {
			calleeDecl, err = findCalledFunctionByResult(unit, node.Symbol, node.Type)
		}
	}
	if err != nil {
		if len(node.TypeArgs) != 0 {
			return "", "", fmt.Errorf("entry function body expression contains a generic call with no matching specialization")
		}
		return "", "", err
	}
	// A generic struct method whose own parameter/return types reference the
	// containing struct's type parameter directly resolves (from the receiver,
	// exactly as the reachability walk resolved it) to a per-instantiation
	// substitution; the call's argument grammar and the callee's C name must be
	// built from the SAME substituted signature the helper was discovered and
	// emitted under, or the call would dispatch on symbolic type-parameter
	// types and name a different C function than the definition.
	calleeSubstitutions := genericStructMethodSubstitutions(unit, snapshot, node, calleeDecl)
	if calleeSubstitutions != nil {
		calleeDecl = substituteDeclarationSignature(snapshot, calleeDecl, calleeSubstitutions)
	}
	callPre, callArgs, err := buildCallArguments(st, unit, snapshot, fileSet, node, calleeDecl, locals, width, nested)
	if err != nil {
		return "", "", err
	}
	calleeName := helperCName(calleeDecl, calleeSubstitutions)
	if callArgs == "" {
		return callPre, fmt.Sprintf("%s(ctx)", calleeName), nil
	}
	return callPre, fmt.Sprintf("%s(ctx, %s)", calleeName, callArgs), nil
}

// buildCallArguments builds the comma-separated C argument list for a
// DirectCall's children, one expression per child in order. Each child's
// grammar is decided by the callee's corresponding parameter's resolved type
// — the entry's width parameters take buildExpr, bool parameters take
// buildBoolExpr, str parameters (since 10.36) take buildStrOperand, and
// tuple/struct parameters take buildAggregateArgument (an
// already-declared aggregate-typed local emitted as its own C name, or a
// freshly-constructed aggregate built inline as a compound-literal expression,
// see buildAggregateArgument) — so the same value grammars this backend
// already builds lower
// the arguments; the checker has already coerced each argument to its
// parameter's type, so a mismatch here is hand-built IR. The argument count
// must equal the callee's declared parameter count. A variadic callee
// (callee.Variadic — the checker's own declaration node sets it; see
// ir_builder.go's FunctionDeclaration construction) is the one exception:
// its trailing slice parameter is fed by every call-site argument from
// fixedCount (the number of fixed parameters, len(Parameters)-1, mirroring
// the checker's own FixedCount computation) onward, collected into a single
// runtime slice value built as ONE C99 compound-literal expression by
// buildVariadicSliceArgument — the array-literal element storage's automatic
// storage duration lasts until the end of the enclosing block (C11 6.5.2.5p16),
// so a callee can validly read the collected slice for the whole call — so
// the arity requirement for a variadic callee is len(call.Children) >=
// fixedCount rather than equality. Returns the joined argument text, empty
// when the callee takes no parameters (the caller then emits
// pebble_fn_<id>(ctx) with no argument list). nested is threaded to
// buildCallArgument for its slice-construction argument shape (see
// buildCallArgument); the variadic collected-element path always builds under
// nested == false, since a variadic slice element is never itself a slice
// construction.
func buildCallArguments(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, call tir.Node, callee tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, nested bool) (string, string, error) {
	variadic := callee.Variadic
	fixedCount := len(callee.Parameters)
	if variadic {
		if len(callee.Parameters) == 0 {
			return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose callee is variadic but declares no parameters (a variadic callee must declare its trailing slice parameter)", call.Symbol)
		}
		fixedCount--
		if len(call.Children) < fixedCount {
			return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d passing %d argument(s), want at least %d (the variadic callee declares %d fixed parameter(s) plus a trailing slice parameter)", call.Symbol, len(call.Children), fixedCount, fixedCount)
		}
	} else if len(call.Children) != len(callee.Parameters) {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d passing %d argument(s), want %d (the callee declares %d parameter(s))", call.Symbol, len(call.Children), len(callee.Parameters), len(callee.Parameters))
	}
	var pres []string
	args := make([]string, 0, len(call.Children))
	// A C-convention callee (an extern declaration) declares its parameters
	// against the real libc header, so a str argument must be lowered to the
	// C representation a real C function consumes — `const char *`, from the
	// PebbleStr value's .data — not passed as the whole PebbleStr struct a
	// Pebble-convention helper's own C signature accepts.
	cConvention := callee.Convention == types.C
	for i := 0; i < fixedCount; i++ {
		pre, arg, err := buildCallArgument(st, unit, snapshot, fileSet, call.Symbol, i, call.Children[i], callee.Parameters[i], cConvention, locals, width, nested)
		if err != nil {
			return "", "", err
		}
		if pre != "" {
			pres = append(pres, pre)
		}
		args = append(args, arg)
	}
	if variadic {
		pre, sliceArg, err := buildVariadicSliceArgument(st, unit, snapshot, fileSet, call, callee.Parameters[fixedCount], call.Children[fixedCount:], locals, width, nested)
		if err != nil {
			return "", "", err
		}
		if pre != "" {
			pres = append(pres, pre)
		}
		args = append(args, sliceArg)
	}
	return strings.Join(pres, "\n"), strings.Join(args, ", "), nil
}

// buildCallArgument builds one call-site argument expression for one callee
// parameter, deciding the child's grammar from the parameter's resolved type
// exactly as an ordinary call's per-argument loop always has — the entry's
// width parameters take buildExpr, uint parameters take buildUintExpr, bool
// parameters take buildBoolExpr, f32/f64 parameters take buildFloatExpr at the
// parameter's own float kind, char parameters take buildCharOperand, str
// parameters take buildStrOperand, tuple/struct parameters take
// buildAggregateArgument, slice parameters take buildSliceArgument, pointer
// parameters take buildExpr (which handles every pointer-value shape), and
// function-typed parameters (function-types slice 3) take buildFunctionValue
// (the shared builder every function-typed value position uses).
// The checker has already coerced the argument to the parameter's type, so a
// mismatch here is hand-built IR. position is the call-site argument index,
// used only to name the offending argument in rejection messages.
// cConvention is true when the callee is a C-convention extern declaration: a
// str argument to such a callee must be lowered to the const char * a real C
// function consumes (the PebbleStr value's .data field) rather than passed as
// the whole PebbleStr struct, so the emitted call site agrees with the libc
// header's own parameter type (fopen's path/mode, and so on).
//
// The return is a (pre, expr) pair: expr is the argument's C expression, and
// pre is an optional indent-free pre-statement the caller must emit BEFORE the
// enclosing call expression. Only an inline slice-construction argument
// (f(a[1:3]), via buildSliceArgument) ever produces a non-empty pre — the
// two-statement temp-then-construction shape the construction needs — and every
// other argument shape is a pure expression (pre == ""). nested selects how
// that one pre-bearing shape is delivered (see buildSliceArgument): false for a
// leading-statement position that can place the pre before the call, true for a
// pure expression position where the construction is folded into a GNU
// statement-expression argument instead (pre == "").
func buildCallArgument(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, calleeSymbol symbol.SymbolID, position int, argID tir.NodeID, param tir.Parameter, cConvention bool, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, nested bool) (string, string, error) {
	// A parameter's own resolved integer width, when the parameter is an
	// integer builtin the backend emits (the entry's width, uint, u64, or
	// any other fixed-width integer). Deciding the argument grammar from the
	// parameter's OWN width — rather than the ambient width of the call
	// site — lets a call whose result is consumed in a different-width
	// context (a u64 result assigned into a u64 local, for instance) still
	// build each argument at the parameter's type, and likewise lets a u64
	// parameter be called from an entry-width context. uint is deliberately
	// excluded below: it is the platform-native pointer-width builtin the
	// backend builds through buildUintExpr (sizeof, slice bounds, checked
	// arithmetic), not through the general buildExpr path.
	paramWidth, integerParam := resolvedBuiltin(snapshot, param.Type)
	if integerParam && cType(paramWidth) != "" && !isUint(snapshot, param.Type) {
		expr, err := buildExpr(st, unit, snapshot, fileSet, argID, locals, paramWidth, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	}
	switch {
	case isUint(snapshot, param.Type):
		expr, err := buildUintExpr(st, unit, snapshot, fileSet, argID, locals, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isBool(snapshot, param.Type):
		expr, err := buildBoolExpr(st, unit, snapshot, fileSet, argID, locals, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isFloat(snapshot, param.Type):
		// An f32/f64 parameter: the argument is a float value built by
		// buildFloatExpr at the parameter's OWN float kind — a reference to
		// a float-typed local in scope of the same kind, a float literal
		// directly (f(3.5)), or a call to a float-returning helper (f(g()))
		// — emitted at the same C float type (floatCType) the parameter is
		// declared with, so passing a float by value is trivially valid C.
		expr, err := buildFloatExpr(st, unit, snapshot, fileSet, argID, locals, resolvedFloatKind(snapshot, param.Type), width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isChar(snapshot, param.Type):
		// A char parameter: the argument is a char value built by
		// buildCharOperand — a reference to a char-typed local in scope, a
		// char literal directly (f('a')), or a call to a char-returning
		// helper (f(g())) — emitted as an int32_t value, the same C type
		// the parameter is declared with, so passing a char by value is
		// trivially valid C.
		expr, err := buildCharOperand(st, unit, snapshot, fileSet, argID, locals, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isTuple(snapshot, param.Type):
		expr, err := buildAggregateArgument(st, unit, snapshot, fileSet, argID, locals, param.Type, true, calleeSymbol, position, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isEnumType(unit, snapshot, param.Type):
		// An enum/union-typed parameter: the argument is a value of exactly
		// the parameter's type, emitted at the parameter's own C typedef (the
		// plain pebble_enum_<typeID>_t for a plain-enum parameter, or the
		// union's pebble_union_<typeID>_t for a tagged-union parameter — the
		// C type helperSignature declares the parameter with). For a plain
		// enum the supported arguments are a reference to an
		// already-declared enum-typed local in scope of exactly the
		// parameter's type (a SymbolValue — e.g. unwrap_or(a, 0) passing the
		// union local a), a variant literal directly (check(Color.green) /
		// check(Color.green()), an EnumVariantValue / payload-less
		// VariantConstruct built by buildEnumValue, the same grammar an
		// enum-typed local's declaration uses), and — the Phase 3 #35
		// widening — any of the remaining enum-value shapes buildEnumValue
		// already builds for a plain-enum position: a call to an
		// enum-returning helper used directly (check(pick()), a DirectCall),
		// an enum-typed struct field read used directly (check(s.c), a Load
		// of a FieldPlace, or check(mk().c), a FieldValue read off a
		// call-result struct), and an integer-to-enum cast used directly
		// (check(1 as Color), a CheckedIntegerToEnum), each delegated to
		// buildEnumValue exactly like the variant-literal and deref-read
		// cases below; for a tagged union the
		// argument may additionally be
		// a union-typed struct field read (h.tag), a union-payload optional
		// force-unwrap (o!), or an inline variant construction, all built by
		// the same buildUnionValueExpr a tagged-union payload uses (see
		// buildCallArgument's isTaggedUnionType branch below). The type's own
		// typedef makes passing the whole value by value trivially valid C,
		// matching the C type the parameter is declared with. Anything else —
		// a nonmatching local, or an unsupported value-source shape — is a
		// clean rejection, never a guessed lowering.
		if isTaggedUnionType(unit, snapshot, param.Type) {
			expr, err := buildUnionValueExpr(st, unit, snapshot, fileSet, argID, locals, fmt.Sprintf("call to symbol %d parameter %d (symbol %d) of union type %s", calleeSymbol, position, param.Symbol, unionTypeName(param.Type)), param.Type, width)
			if err != nil {
				return "", "", err
			}
			return "", expr, nil
		}
		argNode, ok := unit.Node(argID)
		if !ok {
			return "", "", fmt.Errorf("call to symbol %d parameter %d (symbol %d) references invalid node %d", calleeSymbol, position, param.Symbol, argID)
		}
		if argNode.Kind == tir.EnumVariantValue || argNode.Kind == tir.VariantConstruct {
			// A variant literal used directly as a plain-enum call argument —
			// `check(Color.green)` (an EnumVariantValue) or the zero-payload
			// parenthesized form `check(Color.green())` (a VariantConstruct) —
			// the reported Phase 3 #13 gap. The argument is delegated to
			// buildEnumValue's variant-literal case, the same grammar an
			// enum-typed local's declaration initializer uses, which emits the
			// variant's own C enum constant pebble_variant_<member> — trivially
			// valid at the parameter's own pebble_enum_<typeID>_t typedef, the
			// same C type helperSignature declares the parameter with (binding
			// the literal into a local first, the prior workaround, emits the
			// exact same constant). A payload-carrying construction is cleanly
			// rejected by buildEnumValue as a tagged-union construction.
			expr, err := buildEnumValue(st, unit, snapshot, fileSet, argID, locals, width)
			if err != nil {
				return "", "", err
			}
			return "", expr, nil
		}
		if argNode.Kind == tir.DirectCall {
			// A call to an enum-returning helper used directly as the
			// argument — `check(pick());` — the Phase 3 #35 widening. The
			// argument is delegated to buildEnumValue's DirectCall case, which
			// double-checks the callee's declared result type is the enum and
			// emits the whole call expression (`pebble_fn_<callee>(ctx, ...)`),
			// whose return type is the callee's declared
			// pebble_enum_<typeID>_t — trivially valid at the parameter's own
			// typedef, the same C type helperSignature declares the parameter
			// with. (An IndirectCall whose result were an enum is unreachable
			// from real source: a function-typed value's signature only ever
			// admits the result shapes the indirect-call lowering can emit, and
			// an enum is not one of them.)
			expr, err := buildEnumValue(st, unit, snapshot, fileSet, argID, locals, width)
			if err != nil {
				return "", "", err
			}
			return "", expr, nil
		}
		if argNode.Kind == tir.Load && len(argNode.Children) == 1 {
			// A whole enum read used directly as the argument — `use_enum(*ptr);` (a pointer deref) or
			// `use_enum(s.c);` (an enum-typed struct field, the Phase 3 #35
			// widening) — the read-side twin of the
			// enum-typed `*p = v;` write. The argument is a Load whose place
			// is a DereferencePlace or a FieldPlace, delegated to
			// buildEnumValue's Load case, which emits the null-checked
			// whole-enum deref value
			// buildDereferencePlaceRead produces
			// (`*(pebble_enum_<typeID>_t)(pebble_rt_checked_deref_ptr(...))`)
			// or the field projection
			// (`pebble_local_<sym>.pebble_field_<m>`, carrying the field's own
			// pebble_enum_<typeID>_t) buildStructFieldRead produces
			// — the enum's own typedef makes the by-value argument trivially
			// valid C.
			if place, ok := unit.Node(argNode.Children[0]); ok && (place.Kind == tir.DereferencePlace || place.Kind == tir.FieldPlace) {
				expr, err := buildEnumValue(st, unit, snapshot, fileSet, argID, locals, width)
				if err != nil {
					return "", "", err
				}
				return "", expr, nil
			}
		}
		if argNode.Kind == tir.FieldValue {
			// An enum-typed struct field read off a NON-ADDRESSABLE struct
			// VALUE used directly as the argument — `check(mk().c)` where mk
			// returns the struct by value — the call-result twin of the
			// `check(s.c)` local-field shape above (the checker lowers a field
			// read off an rvalue struct to a FieldValue, not a Load of a
			// FieldPlace). The argument is delegated to buildEnumValue's
			// FieldValue case, which emits the field projection carrying the
			// field's own pebble_enum_<typeID>_t — trivially valid at the
			// parameter's own typedef.
			expr, err := buildEnumValue(st, unit, snapshot, fileSet, argID, locals, width)
			if err != nil {
				return "", "", err
			}
			return "", expr, nil
		}
		if argNode.Kind == tir.CheckedIntegerToEnum {
			// An integer-to-enum cast used directly as the argument —
			// `check(1 as Color);` — the Phase 3 #35 widening. The argument is
			// delegated to buildEnumValue's CheckedIntegerToEnum case, which
			// emits buildCheckedIntegerToEnumExpr's bounds-checked cast
			// (`(pebble_enum_<typeID>_t)pebble_rt_checked_int_to_enum(...)`)
			// at the enum's own C typedef — trivially valid at the parameter's
			// own typedef, the same C type helperSignature declares the
			// parameter with.
			expr, err := buildEnumValue(st, unit, snapshot, fileSet, argID, locals, width)
			if err != nil {
				return "", "", err
			}
			return "", expr, nil
		}
		if argNode.Kind != tir.SymbolValue {
			return "", "", fmt.Errorf("call to symbol %d parameter %d (symbol %d) of type %s is a %s, want a reference to an enum-typed local of exactly that type in scope (binding the value into a local first is required)", calleeSymbol, position, param.Symbol, describeType(snapshot, param.Type), argNode.Kind)
		}
		info, declared := locals[argNode.Symbol]
		if !declared || info.enumType != param.Type {
			return "", "", fmt.Errorf("call to symbol %d parameter %d (symbol %d) passes symbol %d, which is not a local of the parameter's enum type %s", calleeSymbol, position, param.Symbol, argNode.Symbol, describeType(snapshot, param.Type))
		}
		return "", fmt.Sprintf("pebble_local_%d", argNode.Symbol), nil
	case isStruct(snapshot, param.Type):
		expr, err := buildAggregateArgument(st, unit, snapshot, fileSet, argID, locals, param.Type, false, calleeSymbol, position, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isArray(snapshot, param.Type):
		pre, expr, err := buildArrayArgument(st, unit, snapshot, fileSet, width, param.Type, argID, locals, calleeSymbol, position, nested)
		if err != nil {
			return "", "", err
		}
		return pre, expr, nil
	case isStr(snapshot, param.Type):
		// A str parameter: the argument is a str value built by
		// buildStrOperand — a reference to a str-typed local in scope, a
		// string literal directly (f("hi")), or a call to a str-returning
		// helper (f(g())) — emitted as a PebbleStr value, the same C type
		// the parameter is declared with, so passing a str by value is
		// trivially valid C. When the callee is a C-convention extern, the
		// real libc parameter is `const char *`, not a PebbleStr struct, so
		// the argument is the PebbleStr value's .data field cast to
		// const char * instead — the natural C spelling of a Pebble str (the
		// same cast buildPrint uses for a %s operand) — making the emitted
		// call site agree with the libc header (fopen's path/mode, and so on).
		expr, err := buildStrOperand(st, unit, snapshot, fileSet, argID, locals, width)
		if err != nil {
			return "", "", err
		}
		if cConvention {
			expr = "(const char *)(" + expr + ").data"
		}
		return "", expr, nil
	case isSlice(snapshot, param.Type):
		// A slice parameter (10.38): the argument is a reference to an
		// already-declared slice-typed local in scope of the matching type,
		// emitted as the local's own pebble_local_<symbol> C name — the
		// slice type's own struct typedef makes passing the whole slice by
		// value trivially valid C, no construction needed at the call site
		// (confirmed checker-reachable: f(s) passes a plain SymbolValue) —
		// or an inline slice construction (f(a[1:3])) built by
		// buildSliceArgument, whose temp declaration is returned as the pre
		// in a leading-statement position or folded into a GNU
		// statement-expression argument in a nested position (nested).
		return buildSliceArgument(st, unit, snapshot, fileSet, argID, locals, param.Type, calleeSymbol, position, width, nested)
	case isOptional(snapshot, param.Type):
		// An optional parameter: the argument is an optional value built by
		// buildOptionalValue — a SymbolValue naming an optional-typed local
		// in scope, a fresh SomeOptional/NoneOptional/OptionalInject
		// construction (a call site's scalar implicit injection, `g(5)`,
		// arrives as an OptionalInject node, unlike a return position's bare
		// payload), or a DirectCall to another optional-returning helper —
		// emitted as the optional's own compound literal or a forwarded
		// C name/call, the same C type the parameter is declared with, so
		// passing an optional by value is trivially valid C.
		expr, err := buildOptionalValue(st, unit, snapshot, fileSet, argID, locals, param.Type, fmt.Sprintf("entry function body expression contains a call to symbol %d whose parameter %d (symbol %d)", calleeSymbol, position, param.Symbol), width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isPointer(snapshot, param.Type):
		// A pointer parameter: the argument is a pointer value built by
		// buildExpr, which handles every pointer-value shape (AddressOf,
		// a reference to a pointer-typed local, nil, or a call to a
		// pointer-returning helper).
		expr, err := buildExpr(st, unit, snapshot, fileSet, argID, locals, width, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case isFunctionType(snapshot, param.Type):
		// A function-typed parameter (function-types slice 3): the argument
		// is a function value built by buildFunctionValue — a bare top-level
		// function reference (apply(add, 1, 2), a HoistedFunctionValue), an
		// in-scope function-typed local or parameter (a SymbolValue), a
		// function-typed struct field read (a FieldValue or Load(FieldPlace),
		// combining slice 2 and slice 3), a generic function referenced as a
		// value (a GenericFunctionValue), or a call to a function-returning
		// helper (a DirectCall whose result type is the function type) —
		// emitted as a pebble_fnptr_<typeID>_t value, the same C type the
		// parameter is declared with, so passing a function by value is
		// trivially valid C.
		argNode, ok := unit.Node(argID)
		if !ok {
			return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose parameter %d (symbol %d) references invalid argument node %d", calleeSymbol, position, param.Symbol, argID)
		}
		expr, err := buildFunctionValue(st, unit, snapshot, fileSet, argNode, locals, fmt.Sprintf("entry function body expression contains a call to symbol %d whose parameter %d (symbol %d)", calleeSymbol, position, param.Symbol), width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	default:
		// validateHelperSignature rules any unsupported parameter out
		// before a reachable helper is ever built, so this branch is
		// defense for hand-built IR only.
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose parameter %d (symbol %d) has type %s, want %s, bool, char, str, f32, f64, a tuple/struct type, a slice type, or a pointer type", calleeSymbol, position, param.Symbol, describeType(snapshot, param.Type), wantName(width))
	}
}

// buildWholeArrayDerefRead builds the C expression for a by-value whole-array
// read through a pointer deref — `*p` used as an array-typed VALUE in a
// position whose C destination is the wrapped pebble_array_<typeID>_t typedef:
// an array-typed call argument, an array-typed struct field's construction
// value, or an array return value. The value is a Load whose single child is a
// DereferencePlace whose single child is the pointer expression. The
// dereference yields the wrapped pebble_array_<typeID>_t struct (see
// pointerTypeNameForUnit's array case), so the whole dereference is the value
// itself, `*(pebble_array_<id>_t *)(pebble_rt_checked_deref_ptr(<ptr>,
// <loc>))` — a single expression with the null check performed exactly once —
// the same lowering buildArrayReturnValue's Load case uses. arrayType is the
// array type the destination expects, which must equal the Load's own type.
// context names the enclosing position in error messages.
func buildWholeArrayDerefRead(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, arrayType types.TypeID, locals map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	if node.Type != arrayType {
		return "", fmt.Errorf("%s is a Load of type %s, not an array-typed value of type %s", context, describeType(snapshot, node.Type), describeType(snapshot, arrayType))
	}
	if len(node.Children) != 1 {
		return "", fmt.Errorf("%s is a Load with %d child(ren), want exactly one place", context, len(node.Children))
	}
	place, ok := unit.Node(node.Children[0])
	if !ok {
		return "", fmt.Errorf("%s is a Load referencing invalid place node %d", context, node.Children[0])
	}
	if place.Kind != tir.DereferencePlace {
		return "", fmt.Errorf("%s is a Load whose place is a %s, want a DereferencePlace (a by-value whole-array read through a pointer)", context, place.Kind)
	}
	if len(place.Children) != 1 {
		return "", fmt.Errorf("%s deref read has %d child(ren), want exactly one (the pointer expression)", context, len(place.Children))
	}
	ptrExpr, err := buildExpr(st, unit, snapshot, fileSet, place.Children[0], locals, width, width)
	if err != nil {
		return "", fmt.Errorf("%s deref pointer expression: %v", context, err)
	}
	ptrCType := pointerTypeNameForUnit(st, unit, snapshot, place.Type)
	if ptrCType == "" {
		return "", fmt.Errorf("%s deref has unsupported pointee type %s", context, describeType(snapshot, place.Type))
	}
	checkedPtr := fmt.Sprintf("pebble_rt_checked_deref_ptr(%s, %s)", ptrExpr, buildSourceLoc(fileSet, place.Span))
	return fmt.Sprintf("*(%s)(%s)", ptrCType, checkedPtr), nil
}

func buildArrayArgument(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, width types.BuiltinKind, arrayType types.TypeID, argID tir.NodeID, locals map[symbol.SymbolID]localInfo, calleeSymbol symbol.SymbolID, position int, nested bool) (string, string, error) {
	node, ok := unit.Node(argID)
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose array parameter %d references invalid argument node %d", calleeSymbol, position, argID)
	}
	key, ok := snapshot.Key(arrayType)
	if !ok {
		return "", "", fmt.Errorf("array parameter type %s is not in the type snapshot", describeType(snapshot, arrayType))
	}
	length, element, ok := key.Array()
	if !ok {
		return "", "", fmt.Errorf("array parameter type %s has no length and element type", describeType(snapshot, arrayType))
	}
	if node.Kind == tir.DirectCall {
		if node.Type != arrayType {
			return "", "", fmt.Errorf("array argument call has type %s, want %s", describeType(snapshot, node.Type), describeType(snapshot, arrayType))
		}
		expr, err := buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
		return "", expr, err
	}
	if node.Kind == tir.ArrayValue {
		if uint64(len(node.Children)) != length {
			return "", "", fmt.Errorf("array argument has %d element(s), want %d", len(node.Children), length)
		}
		values := make([]string, len(node.Children))
		for i, childID := range node.Children {
			child, ok := unit.Node(childID)
			if !ok {
				return "", "", fmt.Errorf("array argument references invalid element node %d", childID)
			}
			var value string
			var err error
			if isBool(snapshot, element) {
				value, err = buildBoolExpr(st, unit, snapshot, fileSet, childID, locals, width)
			} else if elementWidth, integer := resolvedBuiltin(snapshot, element); integer && cType(elementWidth) != "" {
				value, err = buildExpr(st, unit, snapshot, fileSet, childID, locals, elementWidth, width)
			} else if isArray(snapshot, element) {
				// A nested array literal as a call argument (`f([[1,2,3],
				// [4,5,6]])`): each inner array element is built as its own
				// pebble_array_<innerID>_t wrapper compound literal by the same
				// nested aggregate value builder an outer array literal uses.
				value, err = buildNestedAggregateValue(st, unit, snapshot, fileSet, childID, locals, element, "array argument", width)
			} else {
				return "", "", fmt.Errorf("array argument element type %s is unsupported", describeType(snapshot, child.Type))
			}
			if err != nil {
				return "", "", err
			}
			values[i] = value
		}
		return "", fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(arrayType), strings.Join(values, ", ")), nil
	}
	if node.Kind == tir.ArrayRepeat {
		// A direct [v; N] repeat as a call argument — `sum([7; 3])`. The count
		// child is validated exactly as buildArrayRepeatLocalDeclaration and
		// buildArrayReturnValue validate it (a compile-time uint integer literal
		// equal to the parameter's declared length), and the single value
		// expression is built once and assigned to a C temp whose name is then
		// repeated `length` times in the parameter's array compound literal —
		// the same evaluate-once, copy-N-times lowering a direct ArrayRepeat
		// return uses (buildArrayReturnValue), because a brace-list
		// `{ f(), f(), f() }` would re-evaluate the value once per slot. The
		// temp declaration is returned as a pre-statement for the caller to
		// place before the call (or, in nested mode, folded into a GNU
		// statement-expression), mirroring the inline slice-construction
		// argument's delivery. The temp name derives from the argument node's
		// own NodeID — an argument has no local symbol to name it from —
		// distinct from pebble_repeat_<symbol> (local declarations) and
		// pebble_repeat_ret_<nodeID> (returns), so the three can never collide
		// even when a symbol ID numerically equals a node ID.
		if len(node.Children) != 2 {
			return "", "", fmt.Errorf("array argument is an ArrayRepeat with %d child(ren), want exactly two (the repeated value and the count)", len(node.Children))
		}
		countNode, ok := unit.Node(node.Children[1])
		if !ok {
			return "", "", fmt.Errorf("array argument is an ArrayRepeat referencing invalid count node %d", node.Children[1])
		}
		if countNode.Kind != tir.IntegerLiteral {
			return "", "", fmt.Errorf("array argument is an ArrayRepeat whose count is a %s, want a compile-time integer literal equal to the array's declared length %d", countNode.Kind, length)
		}
		if countNode.Type != snapshot.Builtins().Uint {
			return "", "", fmt.Errorf("array argument is an ArrayRepeat whose count has type %s, want uint (the count is a synthesized integer literal)", describeType(snapshot, countNode.Type))
		}
		count, err := strconv.ParseUint(countNode.Literal.IntegerNum, 10, 64)
		if err != nil {
			return "", "", fmt.Errorf("array argument is an ArrayRepeat whose count %q is not a valid non-negative integer", countNode.Literal.IntegerNum)
		}
		if count != length {
			return "", "", fmt.Errorf("array argument is an ArrayRepeat whose count %d does not equal the array's declared length %d", count, length)
		}
		var valueExpr string
		if isBool(snapshot, element) {
			valueExpr, err = buildBoolExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		} else if elementWidth, integer := resolvedBuiltin(snapshot, element); integer && cType(elementWidth) != "" {
			valueExpr, err = buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, elementWidth, width)
		} else if isFloat(snapshot, element) {
			valueExpr, err = buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], locals, resolvedFloatKind(snapshot, element), width)
		} else {
			return "", "", fmt.Errorf("array argument element type %s is unsupported", describeType(snapshot, element))
		}
		if err != nil {
			return "", "", err
		}
		ctype, err := arrayElementCType(unit, snapshot, width, element)
		if err != nil {
			return "", "", fmt.Errorf("array argument: %v", err)
		}
		tempName := fmt.Sprintf("pebble_repeat_arg_%d", argID)
		pre := fmt.Sprintf("%s %s = %s;", ctype, tempName, valueExpr)
		values := make([]string, length)
		for i := range values {
			values[i] = tempName
		}
		expr := fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(arrayType), strings.Join(values, ", "))
		if nested {
			return "", sliceConstructionStatementExpr(pre, expr), nil
		}
		return pre, expr, nil
	}
	if node.Kind == tir.Load {
		// A whole array read through a pointer deref used directly as the
		// argument — `useIt(*p)`, the array twin of the whole-struct/tuple deref
		// read buildAggregateArgument already passes (and the missing shape this
		// backend previously rejected: "array parameter ... is a Load"). The
		// argument is a Load whose place is a DereferencePlace; the dereference
		// yields the wrapped pebble_array_<typeID>_t struct, which is exactly
		// the parameter's C type, so the whole dereference is passed directly —
		// the same single-expression lowering buildArrayReturnValue uses for
		// `return *p;`, with the null check performed exactly once (Phase 3 #24).
		context := fmt.Sprintf("entry function body expression contains a call to symbol %d whose array parameter %d", calleeSymbol, position)
		expr, err := buildWholeArrayDerefRead(st, unit, snapshot, fileSet, node, arrayType, locals, context, width)
		return "", expr, err
	}
	if node.Kind != tir.SymbolValue {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose array parameter %d is a %s, want an array local, an array literal, an ArrayRepeat, an array-returning call, or a whole-array dereference read", calleeSymbol, position, node.Kind)
	}
	info, declared := locals[node.Symbol]
	if !declared || info.array != arrayType {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose array parameter %d is not a local of type %s", calleeSymbol, position, describeType(snapshot, arrayType))
	}
	if info.arrayWrapped {
		return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	values := make([]string, length)
	for i := range values {
		values[i] = fmt.Sprintf("pebble_local_%d[%d]", node.Symbol, i)
	}
	return "", fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(arrayType), strings.Join(values, ", ")), nil
}

// buildVariadicSliceArgument builds the ONE C argument expression a variadic
// call's trailing slice parameter receives: a C99 compound literal of the
// parameter's slice type collecting every call-site argument in the variadic
// tail as an element of an inline array compound literal,
//
//	(<sliceTypeName>){ .data = (<elementCType>[]){<arg0>, <arg1>, ...}, .len = (size_t)(<count>) }
//
// built purely as an expression — no pre-statement or temp declaration is
// needed, because a C99 array compound literal has automatic storage duration
// lasting until the end of the enclosing block (C11 6.5.2.5p16), not just the
// enclosing full expression, so the callee can validly read the collected
// elements for the whole call. Each variadic call-site argument is built as an
// individual scalar expression at the trailing parameter's slice ELEMENT type
// (resolved via the slice's TypeKey.Child, the same structural step 10.37's
// slice construction and validateSliceElementType use), dispatched through the
// exact same buildCallArgument grammar a fixed parameter of that type would use
// — the only element types this backend supports for any slice at all are the
// entry's width and bool (see validateHelperSignature / sliceElementCType), so
// the practical dispatch is buildExpr for int and buildBoolExpr for bool. The
// zero-variadic-arguments case emits the codebase's established empty-slice
// shape instead of an empty array compound literal — `.data = NULL, .len =
// (size_t)0` — because a zero-size array literal is a GNU extension, not
// portable C99/C11, and won't compile under the project's strict
// -Wall -Wextra -Werror harness (the same shape a SliceFromRaw construction of
// a nil pointer with count 0, `slice ptr, 0`, already produces — see
// buildRawSliceConstruction).
func buildVariadicSliceArgument(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, call tir.Node, sliceParam tir.Parameter, variadicIDs []tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, nested bool) (string, string, error) {
	sliceType := sliceParam.Type
	if !isSlice(snapshot, sliceType) {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic parameter %d (symbol %d) has type %s, want a slice type", call.Symbol, len(call.Children)-1, sliceParam.Symbol, describeType(snapshot, sliceType))
	}
	firstVariadic := len(call.Children) - len(variadicIDs)
	// A variadic call whose tail has exactly one argument whose resolved type is
	// the parameter's whole slice type (not the element type) forwards that
	// slice directly instead of collecting it into a synthesized array-backed
	// compound literal — V1's own codegen shortcut (`src/codegen.c`:
	// `arg_count == fixed_params + 1 && variadic_type->kind == TYPE_SLICE` →
	// `write_expression(exprs[fixed_params])`). The checker decides this shape
	// (a sole tail argument whose statically-known type equals the slice
	// parameter's type), so the argument node's own Type is already the slice
	// type; delegating to buildSliceArgument handles every value shape that
	// builder already supports (a slice-typed local reference, an inline slice
	// construction, or a slice-typed field read).
	if len(variadicIDs) == 1 {
		if node, ok := unit.Node(variadicIDs[0]); ok && node.Type == sliceType {
			return buildSliceArgument(st, unit, snapshot, fileSet, variadicIDs[0], locals, sliceType, call.Symbol, firstVariadic, width, nested)
		}
	}
	key, ok := snapshot.Key(sliceType)
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic parameter type %d is not in the type snapshot", call.Symbol, sliceType)
	}
	elementType, ok := key.Child()
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic parameter type %s has no element type", call.Symbol, describeType(snapshot, sliceType))
	}
	elemCType, err := sliceElementCType(unit, snapshot, width, elementType)
	if err != nil {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic parameter has an unsupported slice element type: %v", call.Symbol, err)
	}
	elems := make([]string, 0, len(variadicIDs))
	for j, argID := range variadicIDs {
		// A C-convention variadic callee is rejected upstream
		// (validateExternSignature), so a variadic slice element is always
		// built for a Pebble-convention callee and a str element stays a
		// PebbleStr value — cConvention is false by construction here.
		pre, expr, err := buildCallArgument(st, unit, snapshot, fileSet, call.Symbol, firstVariadic+j, argID, tir.Parameter{Symbol: sliceParam.Symbol, Type: elementType}, false, locals, width, false)
		if err != nil {
			return "", "", err
		}
		if pre != "" {
			// Defense: an inline slice-construction variadic element cannot
			// be placed — a slice element type is itself rejected above by
			// sliceElementCType, so no supported variadic element can ever
			// produce a pre. Reject rather than silently drop it.
			return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose variadic argument %d requires a temp declaration, which a variadic call's collected-element compound literal cannot place", call.Symbol, firstVariadic+j)
		}
		elems = append(elems, expr)
	}
	if len(elems) == 0 {
		return "", fmt.Sprintf("(%s){ .data = NULL, .len = (size_t)0 }", sliceTypeName(sliceType)), nil
	}
	return "", fmt.Sprintf("(%s){ .data = (%s[]){ %s }, .len = (size_t)(%d) }", sliceTypeName(sliceType), elemCType, strings.Join(elems, ", "), len(elems)), nil
}

// buildAggregateArgument builds one call-site argument for a tuple- or
// struct-typed parameter. The supported argument shapes are (10.25):
//
//   - a plain SymbolValue naming an already-declared aggregate-typed local in
//     scope whose declared type is exactly the parameter's tuple/struct type
//     (wantTuple selects which), emitted as the local's own pebble_local_<symbol>
//     C name — the aggregate's own struct typedef makes passing the whole value
//     by value trivially valid C, so no construction is needed at the call site
//     (this is 10.24's existing supported shape, unchanged);
//   - a freshly-constructed aggregate built inline at the call site — a
//     TupleValue for a tuple parameter (f((1, 2))) or a RecordConstruct for a
//     struct parameter (f(Point.{ x = 1, y = 2 })), both confirmed reachable
//     from real source and both carrying the same Children/Fields/Type shape
//     they have as a local's declaration initializer — emitted as a C99
//     compound-literal expression by buildTupleValueExpr / buildStructValueExpr,
//     which share their brace-list construction with the local-declaration
//     builders;
//   - (tuple branch only) a tuple-returning call used directly as the argument
//     — f(mk()) — a DirectCall or MethodCall, emitted as the call expression
//     itself (its C result type IS the tuple's own typedef, so passing the
//     whole result by value is trivially valid C), built by the same
//     pure-expression-position call machinery buildDirectCallNested uses for a
//     nested call in any other value position;
//   - (tuple branch only) a whole tuple read through a pointer deref or a
//     tuple-typed field read used directly as the argument — f(*ptr) /
//     f(h.t) — a Load whose place is a DereferencePlace or FieldPlace, emitted
//     as the null-checked dereference value buildDereferencePlaceRead produces
//     or the field-projection lvalue buildPlaceLValue produces, a plain
//     by-value tuple copy;
//   - (struct branch only) a whole struct read through a pointer deref used
//     directly as the argument — f(*ptr) — a Load whose place is a
//     DereferencePlace, emitted as the null-checked dereference value
//     buildDereferencePlaceRead produces, a plain by-value struct copy through
//     the pointer;
//   - (struct branch only) a struct-returning call used directly as the
//     argument — f(mk()) — a DirectCall or MethodCall, emitted as the call
//     expression itself (its C result type IS the struct's own typedef, so
//     passing the whole result by value is trivially valid C), built by the
//     same pure-expression-position call machinery buildDirectCallNested
//     uses for a nested call in any other value position;
//   - (struct branch only) a whole struct-typed field read used directly as
//     the argument — f(h.p) — a Load whose place is a FieldPlace, emitted as
//     the field-projection lvalue buildPlaceLValue produces, a plain by-value
//     struct copy read out of the enclosing struct.
//
// An inline construct whose own Type is not exactly the
// parameter's type (defense for hand-built IR — the checker coerces every
// argument to its parameter's type and rejects a mismatch itself) is a clean
// rejection, so the emitted C never passes a value of the wrong aggregate
// type to a parameter. Any other argument shape is a clean rejection naming
// what was found: a SourceAlias-wrapped argument (extra parens, e.g.
// f(((1, 2)))), a nested aggregate whose element/field types are outside the
// two supported grammars, or any other node kind. width is the entry's
// resolved integer width, threaded through to the inline builders so each
// element/field is built at the width the parameter's own typedef uses.
func buildAggregateArgument(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, argID tir.NodeID, locals map[symbol.SymbolID]localInfo, wantType types.TypeID, wantTuple bool, calleeSymbol symbol.SymbolID, position int, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(argID)
	if !ok {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d references invalid node %d", calleeSymbol, position, argID)
	}
	if node.Kind != tir.SymbolValue {
		context := fmt.Sprintf("entry function body expression contains a call to symbol %d whose argument %d", calleeSymbol, position)
		if wantTuple {
			if node.Kind == tir.TupleValue {
				if node.Type != wantType {
					return "", fmt.Errorf("%s is a TupleValue of type %s, not a tuple-typed value of type %s", context, describeType(snapshot, node.Type), tupleTypeName(wantType))
				}
				return buildTupleValueExpr(st, unit, snapshot, fileSet, node, locals, wantType, context, width)
			}
			if node.Kind == tir.DirectCall || node.Kind == tir.MethodCall {
				// A tuple-returning call used directly as the argument — `f(makeT())`
				// — the tuple-side mirror of the struct-returning-call argument shape
				// the struct branch handles, and the argument-position sibling of the
				// tuple-typed local declaration initializer buildAggregateCallInitializer
				// accepts (10.26). A tuple-returning call's C result type IS the tuple's
				// own typedef, so passing the whole result by value directly as the
				// argument is trivially valid C — no runtime helper, no temp local
				// needed. The call's result type is the node's own Type, which is the
				// callee's resolved result type, and it must be exactly the parameter's
				// tuple type (defense for hand-built IR — the checker coerces every
				// argument to its parameter's type), so the emitted C never passes a
				// value of the wrong tuple type. The call is built by buildDirectCallNested,
				// the same pure-expression-position call machinery buildExpr's DirectCall
				// case uses, since an argument position has nowhere for a leading
				// pre-statement.
				if node.Type != wantType {
					return "", fmt.Errorf("%s is a %s of type %s, not a tuple-typed value of type %s", context, node.Kind, describeType(snapshot, node.Type), tupleTypeName(wantType))
				}
				return buildDirectCallNested(st, unit, snapshot, fileSet, node, locals, width)
			}
			if node.Kind == tir.Load {
				// A by-value read of a whole tuple used directly as the call
				// argument, in two shapes — the tuple-side mirror of the struct
				// branch's Load case:
				//
				//   - a whole tuple read through a pointer deref — `use_tuple(*ptr);`
				//     — a Load of a DereferencePlace whose single child is the
				//     pointer expression. The emitted C is the null-checked
				//     dereference value buildDereferencePlaceRead produces,
				//     `*(pebble_tuple_<typeID>_t)(pebble_rt_checked_deref_ptr(...))`;
				//   - a tuple-typed field read — `use_tuple(h.p);` — lowered to a
				//     Load of a FieldPlace, emitted as the field-projection lvalue
				//     buildPlaceLValue produces (e.g.
				//     `pebble_local_<sym>.pebble_field_<member>`), the same
				//     whole-tuple field read buildAggregateCallInitializer-style
				//     tuple locals read through.
				//
				// Either way the tuple's own typedef makes passing the whole
				// dereferenced/field-read tuple by value trivially valid C, the same
				// by-value copy the symbol-reference and compound-literal argument
				// shapes make. The Load's Type must be exactly the parameter's tuple
				// type (defense for hand-built IR — the checker coerces every argument
				// to its parameter's type), and the place's resolved element type is
				// double-checked against it for the FieldPlace shape.
				if node.Type != wantType {
					return "", fmt.Errorf("%s is a Load of type %s, not a tuple-typed value of type %s", context, describeType(snapshot, node.Type), tupleTypeName(wantType))
				}
				if len(node.Children) != 1 {
					return "", fmt.Errorf("%s is a Load with %d child(ren), want exactly one place", context, len(node.Children))
				}
				place, ok := unit.Node(node.Children[0])
				if !ok {
					return "", fmt.Errorf("%s is a Load referencing invalid place node %d", context, node.Children[0])
				}
				if place.Kind == tir.DereferencePlace {
					return buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, locals, width, node.Span, false)
				}
				if place.Kind == tir.FieldPlace {
					lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
					if err != nil {
						return "", fmt.Errorf("%s tuple-field read: %v", context, err)
					}
					if elementType != wantType {
						return "", fmt.Errorf("%s reads a place of element type %s, not a tuple-typed value of type %s", context, describeType(snapshot, elementType), tupleTypeName(wantType))
					}
					return lvalue, nil
				}
				return "", fmt.Errorf("%s is a Load whose place is a %s, want a DereferencePlace (a by-value whole-tuple read through a pointer) or a FieldPlace (a by-value read of a tuple-typed field)", context, place.Kind)
			}
			return "", fmt.Errorf("%s is a %s, want a reference to a tuple-typed local in scope or a tuple literal (a TupleValue); only passing an already-declared tuple-typed local or constructing a fresh tuple literal inline is supported", context, node.Kind)
		}
		expr, structType, err := buildStructValueNode(st, unit, snapshot, fileSet, argID, locals, width)
		if err != nil {
			return "", fmt.Errorf("%s: %v", context, err)
		}
		if structType != wantType {
			return "", fmt.Errorf("%s is a %s of type %s, not a struct-typed value of type %s", context, node.Kind, describeType(snapshot, structType), structTypeName(wantType))
		}
		return expr, nil
	}
	info, declared := locals[node.Symbol]
	if !declared {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d references symbol %d, which is not a local in scope", calleeSymbol, position, node.Symbol)
	}
	if wantTuple {
		if info.tuple != wantType {
			return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d passes symbol %d, which is a local of type %s, not a tuple-typed local of type %s", calleeSymbol, position, node.Symbol, describeType(snapshot, node.Type), tupleTypeName(wantType))
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	if info.structType != wantType {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d passes symbol %d, which is a local of type %s, not a struct-typed local of type %s", calleeSymbol, position, node.Symbol, describeType(snapshot, node.Type), structTypeName(wantType))
	}
	return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
}

// sliceConstructionStatementExpr folds an inline slice construction's
// two-statement lowering (the temp-declaration statement and the
// compound-literal construction expression, exactly as buildSliceConstruction
// produces them) into a single GNU statement-expression primary expression,
// `({ <temp decl>; <construction expr>; })`, so the construction can live in a
// pure expression position that has nowhere for a separate pre-statement. The
// value of a GNU statement-expression is its last expression statement's
// value, and — as documented by GCC and confirmed against cc — that final
// expression statement still needs its terminating semicolon (a compound
// literal there is otherwise a plain cast-plus-brace sequence). The temp
// declaration keeps its own semicolon. GCC/Clang (this project's cc
// toolchain) support the extension, and a statement-expression is a primary
// expression, so it composes correctly inside a larger expression like a
// function-call argument list.
func sliceConstructionStatementExpr(tempDecl, constructionExpr string) string {
	return "({ " + tempDecl + " " + constructionExpr + "; })"
}

// buildSliceArgument builds one call-site argument for a slice-typed parameter
// (10.38). Two argument shapes are supported:
//
//   - a plain SymbolValue naming an already-declared slice-typed local in scope
//     whose declared type is exactly the parameter's slice type, emitted as the
//     local's own pebble_local_<symbol> C name — the slice type's own struct
//     typedef makes passing the whole slice by value trivially valid C, so no
//     construction is needed at the call site (confirmed checker-reachable via
//     a real fixture: f(s) passes a plain SymbolValue);
//   - an inline slice construction used directly as a call argument — f(a[1:3]),
//     a bare CheckedSlice, confirmed checker-reachable via a real fixture —
//     emitted as the same two-statement temp-then-construction shape 10.37's
//     local declaration and the return side use (a temp holding the checked-
//     start result, then the compound literal using that temp). How that text
//     is delivered depends on the caller's position, selected by nested: a
//     leading-statement position (nested == false) gets the temp declaration
//     returned as a separate pre-statement to place before the consuming
//     expression (a bare call statement or a local's declaration initializer),
//     while a pure expression position (nested == true) has nowhere for a
//     separate statement, so the temp declaration and compound literal are
//     instead folded into a GNU statement-expression primary expression by
//     sliceConstructionStatementExpr and returned as the argument text itself
//     (with an empty pre) — the REVERSE of the backend's original decision
//     never to reach for a GNU statement-expression;
//   - a by-value read of a slice-typed struct field used directly as the
//     argument — f(h.values), a Load whose place is a FieldPlace naming the
//     slice field (the same Load(FieldPlace) shape a slice field read in any
//     other value position uses, and the same shape buildSliceLocalDeclaration
//     accepts for a slice local's declaration initializer) — emitted as the
//     field-projection lvalue buildPlaceLValue produces, e.g.
//     `pebble_local_<sym>.pebble_field_<member>`, passed directly as the
//     argument: the slice type's own struct typedef makes passing the whole
//     slice by value trivially valid C, and buildPlaceLValue's output is
//     itself a valid inline C lvalue expression usable directly as a call
//     argument, so no pre-statement or temp is needed. The Load's own type
//     and the place's resolved type are both double-checked against the
//     parameter's slice type (defense for hand-built IR).
//
// Any other argument shape — a local that is not slice-typed, a SourceAlias-
// wrapped argument, or any other node kind — is likewise a clean rejection
// naming what was found, matching buildAggregateArgument's own discipline. The
// returned pre is indent-free (buildSliceConstruction is called with an empty
// indent); the caller prepends its own indent. width is the entry's resolved
// integer width, threaded through so the temp is declared at the correct width.
func buildSliceArgument(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, argID tir.NodeID, locals map[symbol.SymbolID]localInfo, wantType types.TypeID, calleeSymbol symbol.SymbolID, position int, width types.BuiltinKind, nested bool) (string, string, error) {
	node, ok := unit.Node(argID)
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a call to symbol %d whose argument %d references invalid node %d", calleeSymbol, position, argID)
	}
	context := fmt.Sprintf("entry function body expression contains a call to symbol %d whose argument %d", calleeSymbol, position)
	if node.Kind == tir.CheckedSlice {
		if node.Type != wantType {
			return "", "", fmt.Errorf("%s is an inline slice construction (a CheckedSlice) of type %s, not a slice-typed value of type %s", context, describeType(snapshot, node.Type), sliceTypeName(wantType))
		}
		tempDecl, constructionExpr, err := buildSliceConstruction(st, unit, snapshot, fileSet, node, locals, "", context, width, fmt.Sprintf("pebble_slice_arg_%d", argID), fmt.Sprintf("pebble_arg_backing_%d", argID))
		if err != nil {
			return "", "", err
		}
		if nested {
			return "", sliceConstructionStatementExpr(tempDecl, constructionExpr), nil
		}
		return tempDecl, constructionExpr, nil
	}
	if node.Kind == tir.Load {
		if node.Type != wantType {
			return "", "", fmt.Errorf("%s is a Load of type %s, not a slice-typed value of type %s", context, describeType(snapshot, node.Type), sliceTypeName(wantType))
		}
		if len(node.Children) != 1 {
			return "", "", fmt.Errorf("%s is a Load with %d child(ren), want exactly one place", context, len(node.Children))
		}
		lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", "", fmt.Errorf("%s slice-field read: %v", context, err)
		}
		if elementType != wantType {
			return "", "", fmt.Errorf("%s reads a place of element type %s, not a slice-typed value of type %s", context, describeType(snapshot, elementType), sliceTypeName(wantType))
		}
		return "", lvalue, nil
	}
	if node.Kind == tir.DirectCall || node.Kind == tir.MethodCall {
		// A slice-returning call used directly as the argument — `f(mk())` —
		// the slice-side mirror of the struct/tuple-returning-call argument
		// shapes buildAggregateArgument's struct and tuple branches accept. A
		// slice-returning call's C result type IS the slice's own typedef, so
		// passing the whole result by value directly as the argument is
		// trivially valid C — no runtime helper, no temp local needed. The
		// call's result type is the node's own Type, which is the callee's
		// resolved result type, and it must be exactly the parameter's slice
		// type (defense for hand-built IR — the checker coerces every argument
		// to its parameter's type), so the emitted C never passes a value of
		// the wrong slice type. The call is built by buildDirectCallNested,
		// the same pure-expression-position call machinery buildExpr's
		// DirectCall case uses, since an argument position has nowhere for a
		// leading pre-statement.
		if node.Type != wantType {
			return "", "", fmt.Errorf("%s is a %s of type %s, not a slice-typed value of type %s", context, node.Kind, describeType(snapshot, node.Type), sliceTypeName(wantType))
		}
		expr, err := buildDirectCallNested(st, unit, snapshot, fileSet, node, locals, width)
		return "", expr, err
	}
	if node.Kind != tir.SymbolValue {
		return "", "", fmt.Errorf("%s is a %s, want a reference to a slice-typed local in scope; only passing an already-declared slice-typed local is supported", context, node.Kind)
	}
	info, declared := locals[node.Symbol]
	if !declared {
		return "", "", fmt.Errorf("%s references symbol %d, which is not a local in scope", context, node.Symbol)
	}
	if info.sliceType != wantType {
		return "", "", fmt.Errorf("%s passes symbol %d, which is a local of type %s, not a slice-typed local of type %s", context, node.Symbol, describeType(snapshot, node.Type), sliceTypeName(wantType))
	}
	return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
}

// buildAggregateReturnValue builds the C expression text for a tuple/struct-
// returning function's tail-position return value (10.26). The enclosing
// function's result type comes from result (mutually exclusive tuple /
// structType, set by buildHelperFunctions from the helper's own ResultType),
// and the return-value shapes supported are (all confirmed against real
// fixtures):
//
//   - a plain SymbolValue naming an already-declared aggregate-typed local in
//     scope whose declared type is exactly the function's result type, emitted
//     as the local's own pebble_local_<symbol> C name — forwarding an
//     already-computed aggregate value without re-constructing it;
//   - a freshly-constructed aggregate built inline in the return — a
//     TupleValue (return (20, 22)) or a RecordConstruct (return
//     Point.{ x = 20, y = 22 }), emitted as a C99 compound-literal expression
//     by buildTupleValueExpr / buildStructValueExpr (the same 10.25 expression
//     builders an inline call argument uses), so the return statement emits
//     e.g. `return (pebble_tuple_23_t){ 20, 22 };`;
//   - (struct branch only) a DirectCall to another struct-returning helper
//     (return helperReturningStruct();), the return-forwarding shape io.peb's
//     `return string::new();` uses. The callee's declared ResultType is
//     double-checked against the function's result type (defense for hand-built
//     IR), then the call is built by buildDirectCallWithPre — the same
//     call-building machinery a struct-typed local's call initializer uses. The
//     call's argument building may require a temp-declaration pre-statement (an
//     inline slice-construction argument); since a return is a pure expression
//     position with nowhere to place that statement, the pre is returned
//     separately for the caller to thread into its statement sequence before the
//     final `return <expr>;` line, the same (pre, expr) convention
//     buildSliceReturnValue and buildScalarInitializeCore use.
//
// The function returns a (pre, expr) pair: expr is the return value's C
// expression and pre is the optional pre-statement the caller must emit BEFORE
// the `return <expr>;` line (empty for every shape but a DirectCall with a
// construction-needing argument). pre carries its own leading indent (indent,
// threaded in from the caller, is prepended to it), matching how
// buildSliceReturnValue returns its pre-indented temp declaration. An inline
// construct whose own Type is not exactly the function's result type (defense
// for hand-built IR — the checker coerces every return value to the function's
// declared result type) is a clean rejection, so the emitted C never returns a
// value of the wrong aggregate type; the struct branch's DirectCall shape is
// likewise rejected when the callee's declared ResultType differs from the
// function's result type. Any other return-value shape is a clean rejection
// naming what was found — including a DirectCall in the tuple branch, where
// only the SymbolValue and TupleValue shapes above are supported. width is the
// entry's resolved integer width, threaded through to the inline builders so
// each element/field is built at the width the result type's own typedef uses.
func buildAggregateReturnValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, result resultInfo, indent string, width types.BuiltinKind) (string, string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", "", fmt.Errorf("entry function body return statement references invalid value node %d", id)
	}
	if node.Kind == tir.SymbolValue {
		info, declared := locals[node.Symbol]
		if !declared {
			return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is not a local in scope", node.Symbol)
		}
		if result.tuple != 0 {
			if info.tuple != result.tuple {
				return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is a local of type %s, not a tuple-typed local of type %s", node.Symbol, describeType(snapshot, node.Type), tupleTypeName(result.tuple))
			}
			return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		}
		if info.structType != result.structType {
			return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is a local of type %s, not a struct-typed local of type %s", node.Symbol, describeType(snapshot, node.Type), structTypeName(result.structType))
		}
		return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	if result.tuple != 0 {
		context := "entry function body return statement"
		if node.Kind == tir.TupleValue {
			if node.Type != result.tuple {
				return "", "", fmt.Errorf("%s returns a TupleValue of type %s, not a tuple-typed value of type %s", context, describeType(snapshot, node.Type), tupleTypeName(result.tuple))
			}
			expr, err := buildTupleValueExpr(st, unit, snapshot, fileSet, node, locals, result.tuple, context, width)
			return "", expr, err
		}
		if node.Kind == tir.Load {
			// A whole tuple read used directly as the return value, in the same
			// two shapes buildAggregateArgument's tuple-branch Load case accepts
			// (mirroring the struct side's return handling): a whole tuple read
			// through a pointer deref — `return *ptr;` — lowered to a Load of a
			// DereferencePlace, emitted as the null-checked whole-tuple deref
			// value buildDereferencePlaceRead produces
			// (`*(pebble_tuple_<typeID>_t)(pebble_rt_checked_deref_ptr(...))`),
			// or a tuple-typed field read — `return h.p;` — a Load of a
			// FieldPlace, emitted as the field projection lvalue. Both are the
			// tuple's own pebble_tuple_<typeID>_t, so returning the whole value
			// by value is trivially valid C.
			if node.Type != result.tuple {
				return "", "", fmt.Errorf("%s returns a Load of type %s, not a tuple-typed value of type %s", context, describeType(snapshot, node.Type), tupleTypeName(result.tuple))
			}
			if len(node.Children) != 1 {
				return "", "", fmt.Errorf("%s returns a Load with %d child(ren), want exactly one place", context, len(node.Children))
			}
			place, ok := unit.Node(node.Children[0])
			if !ok {
				return "", "", fmt.Errorf("%s returns a Load referencing invalid place node %d", context, node.Children[0])
			}
			if place.Kind == tir.DereferencePlace {
				value, err := buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, locals, width, node.Span, false)
				return "", value, err
			}
			if place.Kind == tir.FieldPlace || place.Kind == tir.CheckedIndexPlace {
				lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
				if err != nil {
					return "", "", fmt.Errorf("%s tuple read: %v", context, err)
				}
				if elementType != result.tuple {
					return "", "", fmt.Errorf("%s returns a read of element type %s, not a tuple-typed value of type %s", context, describeType(snapshot, elementType), tupleTypeName(result.tuple))
				}
				return "", lvalue, nil
			}
			return "", "", fmt.Errorf("%s returns a Load whose place is a %s, want a DereferencePlace (a by-value whole-tuple read through a pointer) or a FieldPlace (a by-value read of a tuple-typed field)", context, place.Kind)
		}
		return "", "", fmt.Errorf("%s returns a %s, want a reference to a tuple-typed local in scope or a tuple literal (a TupleValue); only returning an already-declared tuple-typed local or constructing a fresh tuple literal inline is supported", context, node.Kind)
	}
	context := "entry function body return statement"
	if node.Kind == tir.RecordConstruct {
		if node.Type != result.structType {
			return "", "", fmt.Errorf("%s returns a RecordConstruct of type %s, not a struct-typed value of type %s", context, describeType(snapshot, node.Type), structTypeName(result.structType))
		}
		expr, err := buildStructValueExpr(st, unit, snapshot, fileSet, node, locals, context, width)
		return "", expr, err
	}
	if node.Kind == tir.DirectCall {
		calleeDecl, err := findCallDeclaration(unit, snapshot, node)
		if err != nil {
			return "", "", err
		}
		if calleeDecl.ResultType != result.structType {
			return "", "", fmt.Errorf("%s returns a call to symbol %d whose declared result type %s does not match the function's result type %s", context, node.Symbol, describeType(snapshot, calleeDecl.ResultType), structTypeName(result.structType))
		}
		callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", "", err
		}
		if callPre != "" {
			callPre = indent + callPre
		}
		return callPre, callExpr, nil
	}
	if node.Kind == tir.ContextValue {
		// The bare `context` expression used directly as a Context-typed
		// function's return value — `return context;`. ContextValue is the
		// distinct TIR shape the `context` keyword lowers to (not a
		// SymbolValue/RecordConstruct/DirectCall). Context is always the
		// already-threaded hidden `ctx` parameter, never a value to construct
		// from scratch, so the returned value is the dereferenced parameter
		// `(*ctx)` — a PebbleContext value matching the function's result type's
		// C type.
		if node.Type != result.structType {
			return "", "", fmt.Errorf("%s returns a ContextValue of type %s, not a struct-typed value of type %s", context, describeType(snapshot, node.Type), structTypeName(result.structType))
		}
		return "", "(*ctx)", nil
	}
	if node.Kind == tir.FieldValue {
		// A struct-typed field read off a NON-ADDRESSABLE struct VALUE used
		// directly as the return value — `return mk().inner;` — the
		// value-source counterpart of the Load(FieldPlace)/Load(CheckedIndexPlace)
		// whole-struct return shapes below (a struct LOCAL's field lowers to
		// Load(FieldPlace); a field of a call result, force-unwrap, or element
		// read lowers to this FieldValue). buildStructValueNode builds the
		// whole-struct projection, whose type must be exactly the function's
		// struct result type.
		expr, structType, err := buildStructValueNode(st, unit, snapshot, fileSet, id, locals, width)
		if err != nil {
			return "", "", fmt.Errorf("%s: %v", context, err)
		}
		if structType != result.structType {
			return "", "", fmt.Errorf("%s returns a field read of type %s, not a struct-typed value of type %s", context, describeType(snapshot, structType), structTypeName(result.structType))
		}
		return "", expr, nil
	}
	if node.Kind == tir.Load {
		// A whole struct read used directly as the return value, in the same
		// shapes buildAggregateArgument's struct-branch Load case accepts: a
		// whole struct read through a pointer deref — `return *ptr;` — lowered
		// to a Load of a DereferencePlace, emitted as the null-checked
		// whole-struct deref value buildDereferencePlaceRead produces, or a
		// struct-typed field read — `return h.p;` — a Load of a FieldPlace,
		// emitted as the field projection lvalue. Both are the struct's own
		// pebble_struct_<typeID>_t, so returning the whole value by value is
		// trivially valid C (the read-side twin of the resolved `*self =
		// other;` reset write).
		if node.Type != result.structType {
			return "", "", fmt.Errorf("%s returns a Load of type %s, not a struct-typed value of type %s", context, describeType(snapshot, node.Type), structTypeName(result.structType))
		}
		if len(node.Children) != 1 {
			return "", "", fmt.Errorf("%s returns a Load with %d child(ren), want exactly one place", context, len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", "", fmt.Errorf("%s returns a Load referencing invalid place node %d", context, node.Children[0])
		}
		if place.Kind == tir.DereferencePlace {
			value, err := buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, locals, width, node.Span, false)
			return "", value, err
		}
		if place.Kind == tir.FieldPlace || place.Kind == tir.CheckedIndexPlace {
			lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
			if err != nil {
				return "", "", fmt.Errorf("%s struct read: %v", context, err)
			}
			if elementType != result.structType {
				return "", "", fmt.Errorf("%s returns a read of element type %s, not a struct-typed value of type %s", context, describeType(snapshot, elementType), structTypeName(result.structType))
			}
			return "", lvalue, nil
		}
		return "", "", fmt.Errorf("%s returns a Load whose place is a %s, want a DereferencePlace (a by-value whole-struct read through a pointer) or a FieldPlace (a by-value read of a struct-typed field)", context, place.Kind)
	}
	return "", "", fmt.Errorf("%s returns a %s, want a reference to a struct-typed local in scope, a struct literal (a RecordConstruct), or a call to a struct-returning helper (a DirectCall); only returning an already-declared struct-typed local, constructing a fresh struct literal inline, or forwarding a struct-returning helper call is supported", context, node.Kind)
}

func buildArrayReturnValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, arrayType types.TypeID, indent string, width types.BuiltinKind) (string, string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", "", fmt.Errorf("array return references invalid value node %d", id)
	}
	if node.Type != arrayType {
		return "", "", fmt.Errorf("array return has type %s, want %s", describeType(snapshot, node.Type), describeType(snapshot, arrayType))
	}
	if node.Kind == tir.DirectCall {
		returnValue, err := buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
		return "", returnValue, err
	}
	if node.Kind == tir.ArrayValue {
		// A direct array literal as the tail return value — `return [1, 2,
		// 3];`. The per-element C expression strings are built by the same
		// buildArrayBraceElements an array-typed local's brace-list declaration
		// uses, and emitted in the same `(%s){ .data = { %s } }` compound
		// literal shape the SymbolValue tail return below produces for an
		// array local, so the two return shapes are interchangeable at the C
		// level.
		key, ok := snapshot.Key(arrayType)
		if !ok {
			return "", "", fmt.Errorf("array return type %s is not in the type snapshot", describeType(snapshot, arrayType))
		}
		length, elementType, ok := key.Array()
		if !ok {
			return "", "", fmt.Errorf("array return type %s has no length and element type", describeType(snapshot, arrayType))
		}
		if len(node.Children) != int(length) {
			return "", "", fmt.Errorf("array return is an ArrayValue with %d element expression(s), want %d", len(node.Children), length)
		}
		exprs, err := buildArrayBraceElements(st, unit, snapshot, fileSet, node, locals, "array return", width, elementType)
		if err != nil {
			return "", "", err
		}
		return "", fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(arrayType), strings.Join(exprs, ", ")), nil
	}
	if node.Kind == tir.ArrayRepeat {
		// A direct [v; N] repeat as the tail return value — `return [7; 3];`.
		// The count child is validated exactly as buildArrayRepeatLocalDeclaration
		// validates it (a compile-time uint integer literal equal to the
		// array's declared length), and the single value expression is built
		// once and assigned to a C temp whose name is then repeated `length`
		// times in the brace list. Unlike a brace-list array literal
		// `[f(), f(), f()]` — which evaluates each written element and so runs
		// f() three times — [v; N] is a single source expression meant to be
		// evaluated exactly ONCE and copied N times. A return is a pure
		// expression position with nowhere to place the temp-declaration
		// statement, so, mirroring buildSliceReturnValue's CheckedSlice shape,
		// the temp declaration is returned as a separate pre-return statement
		// for the caller (buildReturnStatement) to thread into its statement
		// sequence before the final `return <expr>;` line. The temp name derives
		// from the return value node's own NodeID (a return has no local symbol
		// to name it from), the same identity buildSliceReturnValue's
		// pebble_slice_ret_<nodeID> temp uses, distinct from the
		// pebble_repeat_<symbolID> temps an ArrayRepeat local declaration uses.
		key, ok := snapshot.Key(arrayType)
		if !ok {
			return "", "", fmt.Errorf("array return type %s is not in the type snapshot", describeType(snapshot, arrayType))
		}
		length, elementType, ok := key.Array()
		if !ok {
			return "", "", fmt.Errorf("array return type %s has no length and element type", describeType(snapshot, arrayType))
		}
		if len(node.Children) != 2 {
			return "", "", fmt.Errorf("array return is an ArrayRepeat with %d child(ren), want exactly two (the repeated value and the count)", len(node.Children))
		}
		countNode, ok := unit.Node(node.Children[1])
		if !ok {
			return "", "", fmt.Errorf("array return is an ArrayRepeat referencing invalid count node %d", node.Children[1])
		}
		if countNode.Kind != tir.IntegerLiteral {
			return "", "", fmt.Errorf("array return is an ArrayRepeat whose count is a %s, want a compile-time integer literal equal to the array's declared length %d", countNode.Kind, length)
		}
		if countNode.Type != snapshot.Builtins().Uint {
			return "", "", fmt.Errorf("array return is an ArrayRepeat whose count has type %s, want uint (the count is a synthesized integer literal)", describeType(snapshot, countNode.Type))
		}
		count, err := strconv.ParseUint(countNode.Literal.IntegerNum, 10, 64)
		if err != nil {
			return "", "", fmt.Errorf("array return is an ArrayRepeat whose count %q is not a valid non-negative integer", countNode.Literal.IntegerNum)
		}
		if count != length {
			return "", "", fmt.Errorf("array return is an ArrayRepeat whose count %d does not equal the array's declared length %d", count, length)
		}
		var valueExpr string
		if isBool(snapshot, elementType) {
			valueExpr, err = buildBoolExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		} else if isChar(snapshot, elementType) {
			valueExpr, err = buildCharOperand(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		} else if isFloat(snapshot, elementType) {
			valueExpr, err = buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], locals, resolvedFloatKind(snapshot, elementType), width)
		} else if elementWidth, integerElement := resolvedBuiltin(snapshot, elementType); integerElement && cType(elementWidth) != "" {
			valueExpr, err = buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, elementWidth, width)
		} else {
			valueExpr, err = buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, width)
		}
		if err != nil {
			return "", "", err
		}
		ctype, err := arrayElementCType(unit, snapshot, width, elementType)
		if err != nil {
			return "", "", fmt.Errorf("array return: %v", err)
		}
		tempName := fmt.Sprintf("pebble_repeat_ret_%d", id)
		preReturn := fmt.Sprintf("%s%s %s = %s;", indent, ctype, tempName, valueExpr)
		values := make([]string, length)
		for i := range values {
			values[i] = tempName
		}
		return preReturn, fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(arrayType), strings.Join(values, ", ")), nil
	}
	if node.Kind == tir.Load {
		// A whole array read through a pointer deref used directly as the
		// return value — `return *p;` in an array-returning helper. The return
		// value is a Load whose place is a DereferencePlace whose single child
		// is the pointer expression. An array return is the wrapped
		// pebble_array_<typeID>_t typedef (see the SymbolValue tail-return
		// below), and the dereference yields exactly that wrapped struct (see
		// pointerTypeNameForUnit's array case), so the whole dereference is
		// returned directly — `*(pebble_array_<id>_t *)(checked)` — a single
		// return expression with no pre-return statement. This is the same
		// lowering the shared buildWholeArrayDerefRead produces for the
		// array-argument and array-struct-field construction positions.
		returnValue, err := buildWholeArrayDerefRead(st, unit, snapshot, fileSet, node, arrayType, locals, "array return", width)
		return "", returnValue, err
	}
	if node.Kind != tir.SymbolValue {
		return "", "", fmt.Errorf("array return is a %s, want an array literal (an ArrayValue), an ArrayRepeat, an array local, or an array-returning call", node.Kind)
	}
	info, declared := locals[node.Symbol]
	if !declared || info.array != arrayType {
		return "", "", fmt.Errorf("array return references symbol %d, which is not a local of type %s", node.Symbol, describeType(snapshot, arrayType))
	}
	if info.arrayWrapped {
		return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	key, ok := snapshot.Key(arrayType)
	if !ok {
		return "", "", fmt.Errorf("array return type %s is not in the type snapshot", describeType(snapshot, arrayType))
	}
	length, _, ok := key.Array()
	if !ok {
		return "", "", fmt.Errorf("array return type %s has no length and element type", describeType(snapshot, arrayType))
	}
	values := make([]string, length)
	for i := range values {
		values[i] = fmt.Sprintf("pebble_local_%d[%d]", node.Symbol, i)
	}
	return "", fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(arrayType), strings.Join(values, ", ")), nil
}

// buildSliceReturnValue builds the C text pieces for a slice-returning
// function's tail-position return (10.38). The enclosing function's result
// type comes from result.sliceType (set by buildHelperFunctions from the
// helper's own ResultType), and the supported return-value shapes are (all
// confirmed against real fixtures):
//
//   - a plain SymbolValue naming an already-declared slice-typed local — or a
//     slice-typed parameter, which seeds the callee's scope identically — in
//     scope whose declared type is exactly the function's result type, emitted
//     as the local's own pebble_local_<symbol> C name: forwarding an
//     already-computed slice value without re-constructing it, a
//     single-statement return (preReturn is empty);
//   - a fresh CheckedSlice construction (`return a[1:3];`, whose tail Return
//     child is the bare CheckedSlice node — confirmed against a real fixture).
//     This is not a single expression: the construction needs the same
//     two-statement temp-then-construction shape 10.37's local declaration
//     uses (a temp holding the checked-start result, then the compound literal
//     using that temp), but a return is a pure expression position with nowhere
//     to place the temp-declaration statement, so the temp declaration is
//     returned as a separate pre-return statement text for the caller
//     (buildBlock / buildSwitchCaseBody) to thread into its statement sequence
//     before the final `return <expr>;` line — the same mechanical shape
//     deferred statements already demonstrate, just for construction complexity
//     rather than deferred cleanup;
//   - a fresh SliceFromRaw construction (`return slice ptr, n;`, the raw-slice
//     builtin restricted to std-package source), whose construction is a single
//     expression (buildRawSliceConstruction needs no temp), so preReturn is
//     empty;
//   - a by-value read of a slice-typed struct field used directly as the
//     return — `return b.items;` — a Load whose place is a FieldPlace (the
//     same Load(FieldPlace) shape buildSliceArgument accepts for a slice call
//     argument), or a whole-slice read through a pointer deref (`return *ptr;`)
//     — a Load of a DereferencePlace, the null-checked deref value
//     buildDereferencePlaceRead produces — both emitted as the slice's own
//     typedef, so returning the whole value by value is trivially valid C;
//   - a DirectCall to another slice-returning helper (`return g();`), the
//     return-forwarding shape io.peb's `return string::new();` uses for a
//     struct result (10.26). The callee's declared ResultType is double-checked
//     against the function's result type (defense for hand-built IR), then the
//     call is built by buildDirectCallWithPre; if the call's own argument
//     building produced a temp-declaration pre-statement (an inline
//     slice-construction argument to the nested call), it is threaded as a
//     pre-return statement ahead of the final `return <expr>;` line, the same
//     (pre, expr) convention the CheckedSlice shape uses.
//
// Any other return-value shape is likewise a clean rejection naming what was
// found. indent indents the temp declaration to match the surrounding statement
// text. width is the entry's resolved integer width, threaded through so the
// temp is declared at the correct width (the i64-entry width bug found and
// fixed in 10.37's review).
func buildSliceReturnValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, result resultInfo, indent string, width types.BuiltinKind) (string, string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", "", fmt.Errorf("entry function body return statement references invalid value node %d", id)
	}
	if node.Kind == tir.SymbolValue {
		info, declared := locals[node.Symbol]
		if !declared {
			return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is not a local in scope", node.Symbol)
		}
		if info.sliceType != result.sliceType {
			return "", "", fmt.Errorf("entry function body return statement returns symbol %d, which is a local of type %s, not a slice-typed local of type %s", node.Symbol, describeType(snapshot, node.Type), sliceTypeName(result.sliceType))
		}
		return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	context := "entry function body return statement"
	if node.Kind == tir.CheckedSlice {
		if node.Type != result.sliceType {
			return "", "", fmt.Errorf("%s returns a CheckedSlice of type %s, not a slice-typed value of type %s", context, describeType(snapshot, node.Type), sliceTypeName(result.sliceType))
		}
		// The temp name derives from the return value node's own NodeID — the
		// only stable identity in hand here (a return has no local symbol to
		// name it from), distinct from the pebble_slice_start_<symbol> temps a
		// slice local's declaration uses so the two can never collide even when
		// a symbol ID numerically equals a node ID.
		tempDecl, constructionExpr, err := buildSliceConstruction(st, unit, snapshot, fileSet, node, locals, indent, context, width, fmt.Sprintf("pebble_slice_ret_%d", id), fmt.Sprintf("pebble_slice_backing_ret_%d", id))
		if err != nil {
			return "", "", err
		}
		return tempDecl, constructionExpr, nil
	}
	if node.Kind == tir.SliceFromRaw {
		if node.Type != result.sliceType {
			return "", "", fmt.Errorf("%s returns a SliceFromRaw of type %s, not %s", context, describeType(snapshot, node.Type), sliceTypeName(result.sliceType))
		}
		construction, err := buildRawSliceConstruction(st, unit, snapshot, fileSet, node, locals, width, context)
		return "", construction, err
	}
	if node.Kind == tir.DirectCall {
		// A slice-returning call forwarded as the return value — `return g();`
		// — the slice-side mirror of the struct-return branch's DirectCall case
		// in buildAggregateReturnValue. The callee's declared result type is
		// double-checked against the function's result type (defense for
		// hand-built IR), then the call is built by buildDirectCallWithPre; if
		// the nested call's own argument building produced a temp-declaration
		// pre-statement (an inline slice-construction argument to g), it is
		// threaded before the final return line with the caller's indent.
		calleeDecl, err := findCallDeclaration(unit, snapshot, node)
		if err != nil {
			return "", "", err
		}
		if calleeDecl.ResultType != result.sliceType {
			return "", "", fmt.Errorf("%s returns a call to symbol %d whose declared result type %s does not match the function's result type %s", context, node.Symbol, describeType(snapshot, calleeDecl.ResultType), sliceTypeName(result.sliceType))
		}
		callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", "", err
		}
		if callPre != "" {
			callPre = indent + callPre
		}
		return callPre, callExpr, nil
	}
	if node.Kind == tir.Load {
		// A whole slice read used directly as the return value, in the same
		// shapes buildSliceArgument's Load case accepts: a whole slice read
		// through a pointer deref — `return *ptr;` — lowered to a Load of a
		// DereferencePlace, emitted as the null-checked whole-slice deref value
		// buildDereferencePlaceRead produces, or a slice-typed field read —
		// `return b.items;` — a Load of a FieldPlace, emitted as the field
		// projection lvalue. Both are the slice's own pebble_slice_<typeID>_t,
		// so returning the whole value by value is trivially valid C.
		if node.Type != result.sliceType {
			return "", "", fmt.Errorf("%s returns a Load of type %s, not a slice-typed value of type %s", context, describeType(snapshot, node.Type), sliceTypeName(result.sliceType))
		}
		if len(node.Children) != 1 {
			return "", "", fmt.Errorf("%s returns a Load with %d child(ren), want exactly one place", context, len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", "", fmt.Errorf("%s returns a Load referencing invalid place node %d", context, node.Children[0])
		}
		if place.Kind == tir.DereferencePlace {
			value, err := buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, locals, width, node.Span, false)
			return "", value, err
		}
		if place.Kind == tir.FieldPlace {
			lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
			if err != nil {
				return "", "", fmt.Errorf("%s slice read: %v", context, err)
			}
			if elementType != result.sliceType {
				return "", "", fmt.Errorf("%s returns a read of element type %s, not a slice-typed value of type %s", context, describeType(snapshot, elementType), sliceTypeName(result.sliceType))
			}
			return "", lvalue, nil
		}
		return "", "", fmt.Errorf("%s returns a Load whose place is a %s, want a DereferencePlace (a by-value whole-slice read through a pointer) or a FieldPlace (a by-value read of a slice-typed field)", context, place.Kind)
	}
	return "", "", fmt.Errorf("%s returns a %s, want a reference to a slice-typed local in scope or a fresh slice construction (a CheckedSlice); only returning an already-declared slice-typed local or constructing a fresh slice from an array inline is supported", context, node.Kind)
}

// helperFunction is the C text of one reachable helper function: a static
// function named deterministically pebble_fn_<symbolID> from the callee's
// stable IR identity (mirroring the pebble_local_<symbolID> naming
// discipline — never a counter), taking the Pebble context the same way
// pebble_user_main does plus one parameter declaration per callee parameter,
// each named pebble_local_<paramSymbol>. %s is the C return type for the
// entry's resolved width (cType), %d the callee's symbol ID, the third %s the
// comma-separated parameter declaration list (", <cType> pebble_local_<id>",
// empty for a zero-parameter callee), the fourth %s one
// `    (void)pebble_local_<id>;` per parameter (suppressing the confirmed
// -Wunused-parameter warning for a parameter the body never reads, the same
// discipline the (void)ctx; below applies to the context), and the last %s the
// helper's body statements built by buildBlock at depth 0 (4-space indent,
// exactly like the entry's own body).
const helperFunction = `static %s %s(PebbleContext *ctx%s) {
    (void)ctx;
%s%s
}`

// helperPrototype is the C text of one reachable helper's forward
// declaration: the same `static <return> pebble_fn_<symbolID>(PebbleContext
// *ctx, ...)` shape as its definition (helperFunction) but terminated by a
// semicolon — a C prototype. %s is the C return type, %d the callee's symbol
// ID, and the third %s the comma-separated parameter declaration list (the
// same ", <cType> pebble_local_<id>" list the definition uses, empty for a
// zero-parameter callee). Every reachable helper gets a prototype BEFORE any
// definition, which is the standard C fix for recursive and
// mutually-recursive functions: a call anywhere in the file (including inside
// a function whose own definition comes earlier) always has a preceding
// prototype in scope, so no call ever references an undeclared identifier.
const helperPrototype = `static %s %s(PebbleContext *ctx%s);`
