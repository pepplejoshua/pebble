package backend

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildScalarInitializeCore builds the declaration text for a scalar local at
// its own declared builtin type — any integer width (not just the entry's
// resolved width), bool, char, or, since Float Stage A, a float — WITHOUT the
// leading indent and WITHOUT the
// trailing `;` (and without the trailing (void) cast) a full block-level
// declaration statement gets: `<cType> pebble_local_<symbol> = <expr>`. It is
// the scalar tail of the Initialize dispatch, shared by buildLeadingStatement
// (which prepends the indent and appends `;` plus the (void) cast to form the
// full statement) and buildForInitClause (which uses the core as the
// for-header init clause, where the for statement's own header syntax supplies
// the terminating `;`), so the integer-width/bool/char/float validation, the
// buildExpr/buildBoolExpr/buildCharOperand/buildFloatExpr dispatch, and the
// scope recording
// live in exactly one place. An integer local is emitted at its own declared
// width (cType(kind)) and its initializer is built by buildExpr at that same
// width — so e.g. an i64 local inside an i32 function is an int64_t whose
// initializer is built at i64, not i32; a bool local is emitted as a C bool
// (built by buildBoolExpr); a char local is emitted as the fixed C int32_t
// (built by buildCharOperand); a float local is emitted at its own declared
// float type (floatCType(kind) — float for f32, double for f64) and its
// initializer is built by buildFloatExpr at that same kind. Anything else — a
// tuple/array/optional/struct/
// str local — is a clean rejection naming the type, matching
// buildLeadingStatement's own rule. On success the local is recorded in scope
// (localInfo{kind: kind} for an integer or float, localInfo{kind: types.Bool} for a
// bool, or localInfo{isChar: true} for a char) so a later reference or
// reassignment resolves against the same type.
func buildScalarInitializeCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, string, error) {
	// Check for a CheckedOptionalUnwrap whose child is a DirectCall or
	// MethodCall — a force-unwrap of a call result (`let v = m.get(5)!;`).
	// The call result must be materialized into a temp C variable so the
	// unwrap helper can reference its .has_value and .value fields without
	// evaluating the call twice. The temp is declared as the optional's own
	// C struct type and lives in the enclosing block scope, threaded as a
	// pre-statement before the local's own declaration.
	if initValue.Kind == tir.CheckedOptionalUnwrap && len(initValue.Children) == 1 {
		if child, ok := unit.Node(initValue.Children[0]); ok && (child.Kind == tir.DirectCall || child.Kind == tir.MethodCall) {
			kind, ok := resolvedBuiltin(snapshot, initValue.Type)
			if !ok || cType(kind) == "" {
				return "", "", fmt.Errorf("%s declares a local of type %s, want an integer type, bool, char, or float", context, describeType(snapshot, initValue.Type))
			}
			callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, child, scope, width)
			if err != nil {
				return "", "", err
			}
			optionalType := child.Type
			tempName := fmt.Sprintf("pebble_optional_temp_%d", statement.Symbol)
			pre := fmt.Sprintf("%s %s = %s;", optionalTypeName(optionalType), tempName, callExpr)
			if callPre != "" {
				pre = callPre + "\n" + pre
			}
			unwrapSuffix := optionalUnwrapSuffix(snapshot, initValue.Type)
			if unwrapSuffix == "" {
				return "", "", fmt.Errorf("%s declares a local of type %s initialized from an optional unwrap with a payload type %s, which has no runtime unwrap helper", context, describeType(snapshot, initValue.Type), describeType(snapshot, initValue.Type))
			}
			core := fmt.Sprintf("%s pebble_local_%d = pebble_rt_checked_unwrap_%s(%s.has_value, %s.value, %s)", cType(kind), statement.Symbol, unwrapSuffix, tempName, tempName, buildSourceLoc(fileSet, initValue.Span))
			scope[statement.Symbol] = localInfo{kind: kind}
			return pre, core, nil
		}
	}
	kind, ok := resolvedBuiltin(snapshot, initValue.Type)
	if !ok {
		return "", "", fmt.Errorf("%s local declaration declares a local of type %s, want an integer type, bool, char, or float", context, describeType(snapshot, initValue.Type))
	}
	switch kind {
	case types.Bool:
		// A bool local: emitted as a C bool, its value built by buildBoolExpr
		// (the bool grammar is genuinely different from the integer one).
		initExpr, err := buildBoolExpr(unit, snapshot, fileSet, statement.Children[0], scope, width)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{kind: types.Bool}
		return "", fmt.Sprintf("bool pebble_local_%d = %s", statement.Symbol, initExpr), nil
	case types.Char:
		// A char local: emitted as the fixed C int32_t (the language's char
		// is a full Unicode scalar value, always int32_t regardless of the
		// entry's resolved width), its value built by buildCharOperand (a char
		// literal, a reference to an in-scope char-typed local, or a call to a
		// char-returning helper). The scope entry records isChar so a later
		// reference or reassignment is validated and emitted as a char.
		initExpr, err := buildCharOperand(unit, snapshot, fileSet, statement.Children[0], scope, width)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{isChar: true}
		return "", fmt.Sprintf("int32_t pebble_local_%d = %s", statement.Symbol, initExpr), nil
	case types.F32, types.F64:
		// A float local (f32 or f64, Stage A): emitted at the local's own
		// declared float C type (floatCType — float for f32, double for f64),
		// its value built by buildFloatExpr (a float literal or a reference to
		// an in-scope float-typed local of the same kind). The scope entry
		// records the local's own float kind (localInfo{kind: kind}, exactly
		// as an integer local records its own width) so a later reference or
		// reassignment is validated and emitted as that kind's float.
		initExpr, err := buildFloatExpr(unit, snapshot, fileSet, statement.Children[0], scope, kind)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{kind: kind}
		return "", fmt.Sprintf("%s pebble_local_%d = %s", floatCType(kind), statement.Symbol, initExpr), nil
	case types.Uint:
		// A uint local: emitted at uint's own C type (cType — uint64_t, the
		// platform-native pointer-width type), its value built by buildUintExpr
		// (the dedicated uint grammar: sizeof results, checked uint
		// arithmetic, a reference to an in-scope uint-typed local, an
		// integer literal, or — the std/hmap.peb rehash/with_capacity shape —
		// a uint-typed checked-arithmetic tree over a SizeofType operand,
		// which buildExpr's general grammar rejects). This mirrors how every
		// other uint value position routes (call arguments, comparison
		// operands, struct field construction, optional payloads); before
		// this fix a uint local's initializer fell through to buildExpr,
		// which has no SizeofType case. The scope entry records the uint kind
		// (localInfo{kind: types.Uint}, exactly as helperSignature seeds a
		// uint parameter) so a later reference or reassignment is validated
		// and emitted as a uint.
		initExpr, err := buildUintExpr(unit, snapshot, fileSet, statement.Children[0], scope, width)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{kind: types.Uint}
		return "", fmt.Sprintf("%s pebble_local_%d = %s", cType(types.Uint), statement.Symbol, initExpr), nil
	}
	if cType(kind) == "" {
		// Anything that is not bool/char and not an integer builtin the
		// backend emits (str, void) is a clean rejection naming the type,
		// matching buildLeadingStatement's own rule.
		return "", "", fmt.Errorf("%s local declaration declares a local of type %s, want an integer type, bool, char, or float", context, describeType(snapshot, initValue.Type))
	}
	// An integer local of any builtin width, not just the entry's own:
	// emitted at the local's own declared width (cType(kind)), so e.g. an
	// i64 local inside an i32 function is an int64_t, and its initializer is
	// built by buildExpr at that same width (buildExpr re-checks every node
	// in the initializer is the local's own width). The scope entry records
	// the local's own width so a later reference or reassignment is
	// validated and emitted as that width's integer.
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// An integer local declared directly from a call — `let x =
		// f(a[1:3]);` — routed through buildDirectCallWithPre so an inline
		// slice-construction call argument's temp declaration is threaded as
		// the pre (a local declaration is a leading-statement position,
		// mirroring how the CheckedOptionalUnwrap case above threads a
		// force-unwrap temp). The call's result type is initValue.Type,
		// which resolvedBuiltin above already confirmed resolves to this
		// local's own kind, so the declaration is valid without buildExpr's
		// width re-check (buildExpr's DirectCall case emits the identical
		// call text for a non-slice call).
		callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", "", err
		}
		scope[statement.Symbol] = localInfo{kind: kind}
		return callPre, fmt.Sprintf("%s pebble_local_%d = %s", cType(kind), statement.Symbol, callExpr), nil
	}
	initExpr, err := buildExpr(unit, snapshot, fileSet, statement.Children[0], scope, kind, width)
	if err != nil {
		return "", "", err
	}
	scope[statement.Symbol] = localInfo{kind: kind}
	return "", fmt.Sprintf("%s pebble_local_%d = %s", cType(kind), statement.Symbol, initExpr), nil
}

// buildStoreCore builds the value text for a reassignment of a local already
// in scope, WITHOUT the leading indent and WITHOUT the trailing `;` a full
// block-level Store statement gets: `pebble_local_<symbol> = <expr>`. It is
// the Store dispatch, shared by buildLeadingStatement (which prepends the
// indent and appends the `;` to form the full statement) and
// buildForUpdateClause (which uses the core as the for-header update clause,
// where the for statement's own syntax supplies the `;`), so the
// place-validation and the buildExpr/buildBoolExpr/buildFloatExpr dispatch
// live in exactly
// one place. The place must be a plain StoragePlace naming a local in scope,
// or, since 10.39, a CheckedIndexPlace naming an element of an array or
// slice local (`arr[i] = v;` / `s[i] = v;`), and the new value is validated
// and emitted against the resolved place type — the local's own declared type
// for a StoragePlace (the local's own integer width via buildExpr — an i64
// local reassigned inside an i32 function builds its new value at i64 — a
// float local via buildFloatExpr at its own recorded float kind, so an f64
// local reassigned inside an f32 function builds its new value at f64 — bool
// via buildBoolExpr, or, since 10.36, str — a new value that must be a string
// literal, emitted as a whole-struct PebbleStr reassignment; see the isStr
// branch below), or the resolved element type for a CheckedIndexPlace (the
// entry's width via buildExpr or bool via buildBoolExpr, exactly as a scalar
// value position dispatches), mirroring buildLeadingStatement's Store case
// exactly, including its rejections of a Store targeting a
// tuple/array/optional/struct local.
func buildStoreCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, error) {
	if len(statement.Children) != 2 {
		return "", fmt.Errorf("%s reassignment has %d child(ren), want exactly two: the place being reassigned and the new value", context, len(statement.Children))
	}
	place, ok := unit.Node(statement.Children[0])
	if !ok {
		return "", fmt.Errorf("%s reassignment references invalid place node %d", context, statement.Children[0])
	}
	if place.Kind != tir.StoragePlace && place.Kind != tir.CheckedIndexPlace && place.Kind != tir.DereferencePlace && place.Kind != tir.FieldPlace {
		return "", fmt.Errorf("%s reassignment targets a %s, want a plain StoragePlace naming a local in scope, a CheckedIndexPlace naming an element of an array or slice local, a FieldPlace, or a DereferencePlace for a write through a pointer", context, place.Kind)
	}
	if place.Kind == tir.CheckedIndexPlace || place.Kind == tir.DereferencePlace || place.Kind == tir.FieldPlace {
		// An indexed element write (`arr[i] = v;` / `s[i] = v;`) or a
		// write-through-pointer (`*p = v;`). The left-hand lvalue text is
		// built entirely by buildPlaceLValue, which handles both CheckedIndex
		// (bounds-checked array/slice element) and DereferencePlace (null-
		// checked pointer dereference). The new value is built against the
		// resolved target type: buildExpr for the entry's width, buildBoolExpr
		// for bool.
		lvalue, elementType, err := buildPlaceLValue(unit, snapshot, fileSet, statement.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		if isBool(snapshot, elementType) {
			storeValue, err := buildBoolExpr(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if elementWidth, integerElement := resolvedBuiltin(snapshot, elementType); integerElement && cType(elementWidth) != "" {
			// An integer element of any fixed-width builtin, not just the
			// entry's own: the new value is built at the ELEMENT's own resolved
			// width (a u8 slice element inside an i32 function builds its new
			// value at u8), mirroring how a scalar local's reassignment builds
			// at the local's own declared width.
			storeValue, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, elementWidth, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if isChar(snapshot, elementType) {
			storeValue, err := buildCharOperand(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if isPointer(snapshot, elementType) {
			storeValue, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, width, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if isStr(snapshot, elementType) {
			storeValue, err := buildStrOperand(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if isEnumType(unit, snapshot, elementType) {
			// An enum-typed field (or indexed element) write — `entry.state =
			// .Occupied;`, the std/hmap.peb insert shape: the C field is declared
			// at its own pebble_enum_<typeID>_t (see structFieldCType), and the
			// new value is a variant literal built by the same buildEnumValue an
			// enum-typed local's reassignment uses, so `lvalue = pebble_variant_<m>;`
			// is the direct, uncoerced C store.
			storeValue, err := buildEnumValue(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if isSlice(snapshot, elementType) {
			// A slice-typed field (or indexed element) write — `self.entries =
			// new_entries;`, the std/hmap.peb rehash shape: the target is a
			// struct field declared at its own pebble_slice_<typeID>_t (see
			// structFieldCType), and the new value is a reference to an
			// already-declared slice-typed local of the matching type, emitted
			// as the local's own pebble_local_<symbol> C name — the same
			// whole-struct-copy value shape buildSliceArgument accepts for a
			// slice call argument, so `lvalue = pebble_local_<symbol>;` is the
			// direct, uncoerced C store.
			valueNode, ok := unit.Node(statement.Children[1])
			if !ok {
				return "", fmt.Errorf("%s reassignment references invalid value node %d", context, statement.Children[1])
			}
			if valueNode.Kind == tir.SliceFromRaw {
				// A bare SliceFromRaw directly as the slice-field reassignment
				// value — `self.data = slice ptr, new_cap;`, the std/string.peb
				// grow shape: the Store's value is a SliceFromRaw node (not a
				// reference to a slice-typed local), whose construction is a
				// single compound-literal expression (buildRawSliceConstruction,
				// the same construction a slice-typed local declaration's
				// SliceFromRaw initializer uses), so
				// `lvalue = <construction>;` is the direct, uncoerced C store.
				// The construction is for valueNode.Type, which must be exactly
				// the place's resolved slice type (defense for hand-built IR).
				if valueNode.Type != elementType {
					return "", fmt.Errorf("%s reassigns a slice-typed place of type %s from a SliceFromRaw of type %s", context, sliceTypeName(elementType), describeType(snapshot, valueNode.Type))
				}
				construction, err := buildRawSliceConstruction(unit, snapshot, fileSet, valueNode, scope, width, context)
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("%s = %s", lvalue, construction), nil
			}
			if valueNode.Kind != tir.SymbolValue {
				return "", fmt.Errorf("%s reassigns a slice-typed place from a %s, want a reference to a slice-typed local in scope", context, valueNode.Kind)
			}
			valueInfo, declared := scope[valueNode.Symbol]
			if !declared || valueInfo.sliceType != elementType {
				return "", fmt.Errorf("%s reassigns a slice-typed place of type %s from symbol %d, which is not a slice-typed local in scope of that type", context, sliceTypeName(elementType), valueNode.Symbol)
			}
			return fmt.Sprintf("%s = %s", lvalue, fmt.Sprintf("pebble_local_%d", valueNode.Symbol)), nil
		}
		return "", fmt.Errorf("%s reassigns an element of type %s, want a fixed-width integer, char, bool, pointer, enum, str, or slice", context, describeType(snapshot, elementType))
	}
	targetInfo, declared := scope[place.Symbol]
	lvalue := fmt.Sprintf("pebble_local_%d", place.Symbol)
	if !declared {
		// A plain StoragePlace whose symbol is not a local in scope: the write
		// targets a mutable module-level global, whose storage was emitted as a
		// file-scope static. The new value is built against the global's own
		// resolved type exactly as a local's is, and the store writes the
		// global's C name instead of a pebble_local_ name.
		ginfo, isGlobal := emitGlobals[place.Symbol]
		if !isGlobal {
			return "", fmt.Errorf("%s reassigns symbol %d, which is not a local in scope", context, place.Symbol)
		}
		targetInfo = ginfo.info
		lvalue = fmt.Sprintf("pebble_global_%d", place.Symbol)
	}
	// The new value is validated and emitted against the local's own declared
	// type: the local's own integer width for an integer local (buildExpr at
	// that width — an i64 local reassigned inside an i32 function builds its
	// new value at i64, not i32), the float grammar for a float local
	// (buildFloatExpr at the local's own recorded float kind — an f64 local
	// reassigned inside an f32 function builds its new value at f64, not f32),
	// the bool grammar for a bool local
	// (buildBoolExpr). A value of the wrong type — a bool assigned to an
	// integer local, or an integer assigned to a bool local — is rejected by
	// the appropriate builder.
	switch targetInfo.kind {
	case types.Int, types.I8, types.I16, types.I32, types.I64, types.U8, types.U16, types.U32, types.U64:
		storeValue, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, targetInfo.kind, width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
	case types.Uint:
		// A Store whose place names a uint-typed local is a uint
		// reassignment: the new value is built by buildUintExpr at uint's
		// own grammar (sizeof results, checked uint arithmetic, a reference
		// to an in-scope uint-typed local), mirroring how a uint local's
		// declaration initializer routes (buildScalarInitializeCore) and
		// how every other uint value position routes — the general
		// buildExpr path has no SizeofType case and no uint-typed checked
		// arithmetic. std/hmap.peb's insert reassigns its uint local index
		// (`index = (index + 1) % self.cap;`), the exact shape that
		// motivates this case.
		storeValue, err := buildUintExpr(unit, snapshot, fileSet, statement.Children[1], scope, width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
	case types.F32, types.F64:
		// A Store whose place names a float-typed local (f32 or f64, Stage A)
		// is a float reassignment: the new value is built by buildFloatExpr at
		// the local's own recorded float kind (a float literal or a reference
		// to an in-scope float-typed local of that same kind), so `x = 2.5;`
		// emits `pebble_local_<sym> = 2.5;` at the local's own C type. A value
		// of any other shape or type is a clean rejection by buildFloatExpr.
		storeValue, err := buildFloatExpr(unit, snapshot, fileSet, statement.Children[1], scope, targetInfo.kind)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
	case types.Bool:
		storeValue, err := buildBoolExpr(unit, snapshot, fileSet, statement.Children[1], scope, width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
	default:
		if targetInfo.enumType != 0 {
			if _, isUnion := unions[targetInfo.enumType]; isUnion {
				// A Store whose place names a tagged-union-typed local is a
				// whole-value reassignment — c = Choice.value(5); — whose new
				// value is a variant construction built by
				// buildUnionConstruction (a C99 compound literal of the
				// union's struct typedef), emitted as
				// `pebble_local_<sym> = (pebble_union_<id>_t){ .tag = ... };`.
				storeValue, err := buildUnionConstruction(unit, snapshot, fileSet, mustNode(unit, statement.Children[1]), scope, context, unions[targetInfo.enumType], width)
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
			}
			// A Store whose place names an enum-typed local is a whole-value
			// reassignment of a plain enum local — c = Color.red; — whose new
			// value is a variant literal (an EnumVariantValue, or a
			// zero-payload VariantConstruct) built by the enum value builder,
			// emitted as `pebble_local_<sym> = pebble_variant_<member>;`.
			storeValue, err := buildEnumValue(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if targetInfo.isStr {
			// A Store whose place names a str-typed local is a whole-str
			// reassignment. The only supported new-value shape is a string
			// literal (a StringLiteral), the same single shape a str local's
			// declaration accepts — this slice is deliberately literal-to-
			// literal only, so `s = "hi";` works while reassigning from any
			// other value does not. The emitted C is a whole-struct
			// reassignment, `pebble_local_<sym> = (PebbleStr){ .data = ...,
			// .len = <N> };`, whose inner PebbleStr construction text is the
			// exact same byte-for-byte text buildStrLocalDeclaration embeds in
			// a str local's declaration from the same literal (via
			// buildStrLiteralValue — the (PebbleStr) compound-literal cast is
			// what makes the brace text a valid C assignment expression).
			// Reassigning a str local from anything else — a str-typed local
			// (s = t;), a call result (s = g();), string concatenation (s =
			// "h" + "i";), all confirmed reachable from real source against
			// real fixtures — is a clean rejection naming what was found,
			// never a guessed lowering.
			storeValue, ok := unit.Node(statement.Children[1])
			if !ok {
				return "", fmt.Errorf("%s reassignment references invalid value node %d", context, statement.Children[1])
			}
			if storeValue.Kind != tir.StringLiteral {
				return "", fmt.Errorf("%s reassigns symbol %d, a str-typed local, from a %s; reassigning a str local from anything other than a string literal is not supported yet", context, place.Symbol, storeValue.Kind)
			}
			valueText, err := buildStrLiteralValue(storeValue)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = (PebbleStr)%s", lvalue, valueText), nil
		}
		if targetInfo.isChar {
			// A Store whose place names a char-typed local is a char
			// reassignment. The new value is built by buildCharOperand under
			// the char grammar — the same three shapes a char local's
			// declaration accepts (a char literal, a reference to an in-scope
			// char-typed local, or a call to a char-returning helper), each
			// emitted as an int32_t value — so `c = 'b';`, `c = d;`, and
			// `c = g();` (all confirmed checker-reachable against real
			// fixtures) reassign the fixed-width int32_t local correctly. A
			// value of any other shape or type is a clean rejection naming
			// what was found.
			storeValue, err := buildCharOperand(unit, snapshot, fileSet, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if targetInfo.functionType != 0 {
			// A Store whose place names a function-typed local is a
			// whole-value reassignment — `f = g;` — whose new value is
			// another function-typed value built by buildFunctionValue (a
			// reference to an in-scope function-typed local or a bare
			// function value), emitted as
			// `pebble_local_<sym> = <value>;`. The checker has already
			// coerced the new value to the local's own declared function
			// type (the two signatures match), so the assigned C function
			// pointer is always the local's own pebble_fnptr_<typeID>_t.
			storeValue, err := buildFunctionValue(unit, snapshot, fileSet, mustNode(unit, statement.Children[1]), scope, context, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if targetInfo.tuple != 0 {
			// A Store whose place names a tuple-typed local is a
			// whole-tuple reassignment, which is out of scope this slice
			// (only element reads of a tuple local are supported, never
			// assignment into or reassignment of one).
			return "", fmt.Errorf("%s reassigns symbol %d, a tuple-typed local of type %s; reassigning a whole tuple is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.tuple))
		}
		if targetInfo.array != 0 {
			return "", fmt.Errorf("%s reassigns symbol %d, an array-typed local of type %s; reassigning a whole array is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.array))
		}
		if targetInfo.optional != 0 {
			// A Store whose place names an optional-typed local is a
			// whole-value reassignment — `tombstone_index = some index;`, the
			// std/hmap.peb insert shape — whose new value is an optional value
			// built by the same buildOptionalValue machinery an optional
			// local's declaration initializer uses (a SomeOptional / none, a
			// forward of an in-scope optional-typed local, or a call to an
			// optional-returning helper), emitted as
			// `pebble_local_<sym> = <optional value>;` where the value is the
			// optional's own C type (pebble_optional_<typeID>_t), so the whole
			// reassignment is a plain C struct assignment.
			storeValue, err := buildOptionalValue(unit, snapshot, fileSet, statement.Children[1], scope, targetInfo.optional, context, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		if targetInfo.structType != 0 {
			return "", fmt.Errorf("%s reassigns symbol %d, a struct-typed local of type %s; reassigning a whole struct is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.structType))
		}
		if targetInfo.pointerType != 0 {
			// A Store whose place names a pointer-typed local is a pointer
			// reassignment — `p = q;` or `p = nil;`. The new value is a
			// pointer expression built by buildExpr which now handles
			// pointer-typed nodes (AddressOf, SymbolValue, NilPointer).
			storeValue, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, width, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%s = %s", lvalue, storeValue), nil
		}
		return "", fmt.Errorf("%s reassigns symbol %d, which is a local of type %s, want %s or bool", context, place.Symbol, describeType(snapshot, place.Type), wantName(width))
	}
}

// buildCompoundStore builds the value text for a compound assignment — a
// tir.CompoundStore, covering the +=, -=, *=, /=, %= family AND a postfix
// ++/-- (which the checker builds as a CompoundStore with + or - and a
// literal-one value child; see buildPostfixUpdate) — WITHOUT the leading
// indent and WITHOUT the trailing `;` a full block-level CompoundStore
// statement gets: `<lvalue> = <combined value>`.
//
// It is the direct compound twin of buildStoreCore, sharing its place grammar
// exactly: a plain StoragePlace naming a local in scope, a CheckedIndexPlace
// naming an element of an array or slice local (`arr[i] += 1;`), a FieldPlace
// (`self.field -= 1;`), or a DereferencePlace (`*p *= 2;`), the lvalue text
// built by buildPlaceLValue the same way buildStoreCore builds a plain
// reassignment's left side.
//
// The combination goes through the SAME checked-arithmetic runtime helpers
// buildExpr's CheckedArithmetic case uses (pebble_rt_checked_add_i32/...,
// picked by the same checkedArithmeticHelper operator mapping) — a compound
// assignment's whole point is that `x += y` carries the identical overflow and
// divide-by-zero semantics as `x = x + y`, so the emitted C is
// `<lvalue> = pebble_rt_checked_<op>_<suffix>(<lvalue>, <value>, <loc>)`, NOT
// a raw C `+=` (which would silently skip the checked semantics). The lvalue
// text appears twice — once read into the helper, once as the write target —
// which is side-effect-free for a plain local and bounds-checks per evaluation
// for an indexed lvalue whose index expression is itself side-effect-free.
//
// The combined value is validated and emitted against the resolved place type,
// exactly as buildStoreCore dispatches a plain reassignment's new value: an
// integer place builds its value by buildExpr at the place's own resolved
// width (an i64 local inside an i32 function combines at i64, via the _i64
// helper) and combines through the checked helper at that same width; a float
// place (a float compound assignment is checker-reachable — the -=, *=, /=
// families are NumericSame and += is Add, both accepting floats) builds its
// value by buildFloatExpr at the place's own float kind and combines with the
// plain C operator buildFloatExpr's BinaryValue case uses, since floats have
// no checked arithmetic anywhere in this backend (IEEE floats have no defined
// overflow/divide-by-zero fault). Any other place type (bool, str, char, enum,
// union, tuple, array, optional, struct, slice, pointer, runtime) is a clean
// rejection naming what was found, never a guessed lowering.
//
// The operator must be one of the checked-arithmetic set +, -, *, /, % — the
// only operators compoundOperator in the checker can attach to a CompoundStore
// (+= -> +, -= -> -, *= -> *, /= -> /, %= -> %, and a postfix ++/-- -> + or
// -). A CompoundStore carrying any other operator is hand-built IR and a clean
// rejection.
func buildCompoundStore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, statement tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, string, error) {
	if len(statement.Children) != 2 {
		return "", "", fmt.Errorf("%s compound assignment has %d child(ren), want exactly two: the place being combined into and the value to combine into it", context, len(statement.Children))
	}
	place, ok := unit.Node(statement.Children[0])
	if !ok {
		return "", "", fmt.Errorf("%s compound assignment references invalid place node %d", context, statement.Children[0])
	}
	if place.Kind != tir.StoragePlace && place.Kind != tir.CheckedIndexPlace && place.Kind != tir.DereferencePlace && place.Kind != tir.FieldPlace {
		return "", "", fmt.Errorf("%s compound assignment targets a %s, want a plain StoragePlace naming a local in scope, a CheckedIndexPlace naming an element of an array or slice local, a FieldPlace, or a DereferencePlace for a write through a pointer", context, place.Kind)
	}
	// The operator must be one of the five checked-arithmetic operators — the
	// full set compoundOperator in the checker can attach to a CompoundStore
	// (see the doc comment). Anything else is hand-built IR and a clean
	// rejection, never a guessed lowering.
	switch statement.Operator {
	case syntax.Plus, syntax.Minus, syntax.Star, syntax.Slash, syntax.Percent:
	default:
		return "", "", fmt.Errorf("%s compound assignment uses operator %s, want +, -, *, /, or %%", context, statement.Operator)
	}
	if place.Kind == tir.StoragePlace {
		targetInfo, declared := scope[place.Symbol]
		lvalue := fmt.Sprintf("pebble_local_%d", place.Symbol)
		if !declared {
			// A plain StoragePlace whose symbol is not a local in scope: the
			// compound assignment targets a mutable module-level global, exactly
			// as buildStoreCore resolves a plain global reassignment — the
			// combined value is built against the global's own resolved type and
			// the write lands on the global's file-scope C name.
			ginfo, isGlobal := emitGlobals[place.Symbol]
			if !isGlobal {
				return "", "", fmt.Errorf("%s compound assignment combines into symbol %d, which is not a local in scope", context, place.Symbol)
			}
			targetInfo = ginfo.info
			lvalue = fmt.Sprintf("pebble_global_%d", place.Symbol)
		}
		// The lvalue is the local's own C name; the combined value is built
		// against the local's own declared type, mirroring buildStoreCore's
		// targetInfo.kind switch: an integer local combines at its own declared
		// width (an i64 local inside an i32 function combines at i64), a float
		// local at its own float kind, and everything else — bool, str, char,
		// enum, union, tuple, array, optional, struct, slice, pointer, runtime —
		// is a clean rejection naming the local's type.
		switch targetInfo.kind {
		case types.Int, types.I8, types.I16, types.I32, types.I64, types.U8, types.U16, types.U32, types.U64:
			core, err := buildCompoundIntegerCore(unit, snapshot, fileSet, statement, lvalue, targetInfo.kind, scope, context, width)
			return "", core, err
		case types.Uint:
			core, err := buildCompoundUintCore(unit, snapshot, fileSet, statement, lvalue, scope, context, width)
			return "", core, err
		case types.F32, types.F64:
			core, err := buildCompoundFloatCore(unit, snapshot, fileSet, statement, lvalue, targetInfo.kind, scope, context)
			return "", core, err
		default:
			return "", "", fmt.Errorf("%s compound assignment combines into symbol %d, a %s local; compound assignment is supported only for integer and float locals", context, place.Symbol, describeType(snapshot, place.Type))
		}
	}
	// A non-plain place (indexed/field/dereference): the lvalue text and the
	// resolved element type come from buildPlaceLValue, exactly as buildStoreCore
	// builds a plain indexed/field/deref reassignment's left side. The element
	// must be the entry's own resolved width — the one scalar grammar a
	// non-plain element can take — so the checked helper is chosen at that
	// width; anything else (a bool, a pointer, or a non-entry-width integer
	// element) is a clean rejection.
	lvalue, elementType, err := buildPlaceLValue(unit, snapshot, fileSet, statement.Children[0], scope, width)
	if err != nil {
		return "", "", err
	}
	if isUint(snapshot, elementType) {
		// A uint-typed field (or indexed element) compound assignment —
		// `self.len += 1;`, the std/hmap.peb insert shape: the value is built
		// by buildUintExpr and combined with the plain C operator, exactly as
		// a uint-typed local's compound assignment does (buildCompoundUintCore).
		core, err := buildCompoundUintCore(unit, snapshot, fileSet, statement, lvalue, scope, context, width)
		return "", core, err
	}
	if !isWidth(snapshot, width, elementType) {
		return "", "", fmt.Errorf("%s compound assignment combines into an element of type %s, want %s", context, describeType(snapshot, elementType), wantName(width))
	}
	tempName := fmt.Sprintf("pebble_compound_ptr_%d", id)
	core, err := buildCompoundIntegerCore(unit, snapshot, fileSet, statement, "(*"+tempName+")", width, scope, context, width)
	if err != nil {
		return "", "", err
	}
	pre := fmt.Sprintf("%s *%s = &(%s);", cType(width), tempName, lvalue)
	return pre, core, nil
}

// buildCompoundIntegerCore builds the combined-value text for a compound
// assignment whose place resolves to an integer type: the new value is built by
// buildExpr at the place's own resolved width (placeWidth — the local's own
// declared width for a StoragePlace, the entry's resolved width for a
// non-plain place) and combined through the checked-arithmetic runtime helper
// checkedArithmeticHelper picks for the operator at that width, so `i += 1`
// emits `pebble_local_<i> = pebble_rt_checked_add_i32(pebble_local_<i>, 1,
// <loc>)` with the identical overflow and divide-by-zero semantics as a plain
// `i = i + 1`. A place width with no checked helper (any integer builtin other
// than int/i32/i64/u64 — the backend has no checked runtime primitive at those
// widths) is a clean rejection rather than a malformed helper name. u64 is
// admitted for the +, -, * family this slice added (pebble_rt_checked_add/
// sub/mul_u64), but a u64 /= or %= is cleanly rejected: there is no
// checked_div_u64/mod_u64, so emitting one would be a call to a nonexistent
// helper.
func buildCompoundIntegerCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, lvalue string, placeWidth types.BuiltinKind, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	if checkedSuffix(placeWidth) == "" {
		return "", fmt.Errorf("%s compound assignment combines at %s, which has no checked-arithmetic runtime helper; compound assignment is supported only at int, i32, or i64", context, wantName(placeWidth))
	}
	if placeWidth == types.U64 && (statement.Operator == syntax.Slash || statement.Operator == syntax.Percent) {
		return "", fmt.Errorf("%s compound assignment %ss at u64, which has no checked division/modulo runtime helper; u64 compound assignment is supported only for +, -, or *", context, statement.Operator)
	}
	helper, _ := checkedArithmeticHelper(statement.Operator, placeWidth)
	value, err := buildExpr(unit, snapshot, fileSet, statement.Children[1], scope, placeWidth, width)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s = %s(%s, %s, %s)", lvalue, helper, lvalue, value, buildSourceLoc(fileSet, statement.Span)), nil
}

// buildCompoundFloatCore builds the combined-value text for a compound
// assignment whose place resolves to a float type: the new value is built by
// buildFloatExpr at the place's own float kind (placeWidth — f32 or f64) and
// combined with the plain C operator buildFloatExpr's BinaryValue case uses,
// `x = (x + y)` — the same unchecked lowering every float arithmetic in this
// backend uses, since IEEE floats have no defined overflow or divide-by-zero
// fault and no checked float runtime primitives exist. %= on a float is
// rejected (the checker's operatorIntegralSame family never admits a float to
// %=, so a real fixture cannot produce it).
func buildCompoundFloatCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, lvalue string, placeWidth types.BuiltinKind, scope map[symbol.SymbolID]localInfo, context string) (string, error) {
	if statement.Operator == syntax.Percent {
		return "", fmt.Errorf("%s compound assignment uses %%%% on a float local, want +, -, *, or / (%% is integral-only)", context)
	}
	op, _ := arithmeticOperator(statement.Operator)
	value, err := buildFloatExpr(unit, snapshot, fileSet, statement.Children[1], scope, placeWidth)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s = (%s %s %s)", lvalue, lvalue, op, value), nil
}

// buildCompoundUintCore builds the combined-value text for a compound
// assignment whose place resolves to uint: the new value is built by
// buildUintExpr (the dedicated uint grammar — sizeof results, checked uint
// arithmetic, references to in-scope uint locals, integer casts) and combined
// with the plain C operator arithmeticOperator picks for the operator, `x = (x
// + y)` — the same unchecked lowering buildUintExpr uses for every uint
// arithmetic in this backend, exactly as buildCompoundFloatCore is the plain
// unchecked lowering for floats. This covers both the uint-typed struct field
// the std/hmap.peb insert shape uses (`self.len += 1;` — a FieldPlace whose
// resolved element type is uint) and a uint-typed local (`sum += 1`). A uint
// place has no checked runtime helper (checkedSuffix maps no width to uint), so
// the plain C operator is the whole lowering, never a malformed helper name.
func buildCompoundUintCore(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, lvalue string, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	if statement.Operator == syntax.Percent {
		return "", fmt.Errorf("%s compound assignment uses %%%% on a uint place, want +, -, *, or / (%% is integral-only)", context)
	}
	op, _ := arithmeticOperator(statement.Operator)
	value, err := buildUintExpr(unit, snapshot, fileSet, statement.Children[1], scope, width)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s = (%s %s %s)", lvalue, lvalue, op, value), nil
}
