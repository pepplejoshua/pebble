package backend

import (
	"fmt"
	"math/big"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func buildRuntimeValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	if node.Kind == tir.RecordConstruct {
		// A fresh Allocator construction used as a nested runtime-typed field
		// value (e.g. `Holder.{ a = Allocator.{ ptr, alloc, realloc, free } }`):
		// the runtime ABI compound literal, the same shape buildStructValueExpr
		// uses for an Allocator construction in a return/argument/store value
		// position.
		inits, err := buildRuntimeAllocatorBraceList(st, unit, snapshot, fileSet, node, scope, "runtime value", width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s){ %s }", runtimeTypeName(unit, snapshot, node.Type), inits), nil
	}
	if node.Kind == tir.SymbolValue {
		if node.Symbol == unit.Runtime().Context {
			return "(*ctx)", nil
		}
		if info, ok := scope[node.Symbol]; ok && info.runtimeType != 0 {
			return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		}
	}
	if node.Kind == tir.FieldValue && len(node.Children) == 1 {
		baseNode, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("invalid runtime receiver")
		}
		base, err := buildRuntimeValueNode(st, unit, snapshot, fileSet, node.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		owner := baseNode.Type
		if field, ok := runtimeFieldName(unit, owner, node.Member); ok {
			return base + "." + field, nil
		}
	}
	if node.Kind == tir.Load && len(node.Children) == 1 {
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("invalid runtime field place")
		}
		if place.Kind == tir.FieldPlace {
			return buildStructFieldRead(st, unit, snapshot, fileSet, place, scope, width, false)
		}
	}
	return "", fmt.Errorf("runtime value %s is not supported", node.Kind)
}

func buildRuntimeValueNode(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("invalid runtime value node %d", id)
	}
	if node.Kind == tir.ContextValue {
		return "(*ctx)", nil
	}
	return buildRuntimeValue(st, unit, snapshot, fileSet, node, scope, width)
}

// tupleSameCShape reports whether two tuple type IDs have the same structural
// shape at the C-representation level: the same arity and, pairwise, elements
// whose C types match (an integer element by its fixed-width C type — so the
// abstract int and the anchored i32, both int32_t, are the same shape; a tuple
// or optional element by recursive shape; every other element kind — bool,
// char, str, f32/f64, and the nominal struct/enum types — by exact type ID).
// The type store interns each occurrence of a structural tuple type
// expression separately (confirmed against a real fixture: a field declared
// (i32, i32) gets its own TypeID while a construction literal (20, 22) at that
// field gets a second (int, int) TypeID — the checker never unifies the
// literal's unanchored int elements onto the field's declared i32 elements),
// so two tuples of the same C shape legitimately carry distinct TypeIDs and
// the cast must target the DECLARED position's TypeID even though the two IDs
// differ. The comparison exists so buildTupleValueExpr can reject a
// genuinely different shape for hand-built IR — a different arity, or an
// element of a different C type (an i8 element vs an i32 field would make the
// compound literal's brace list a narrowing C initializer, and a bool element
// vs an i32 field a wrong-type initializer) — instead of silently casting to
// a typedef the brace list does not match.
func tupleSameCShape(snapshot *types.Snapshot, a, b types.TypeID) bool {
	keyA, ok := snapshot.Key(a)
	if !ok {
		return false
	}
	keyB, ok := snapshot.Key(b)
	if !ok {
		return false
	}
	elementsA, ok := keyA.Elements()
	if !ok {
		return false
	}
	elementsB, ok := keyB.Elements()
	if !ok {
		return false
	}
	if len(elementsA) != len(elementsB) {
		return false
	}
	for i := range elementsA {
		if !tupleElementSameCShape(snapshot, elementsA[i], elementsB[i]) {
			return false
		}
	}
	return true
}

func tupleElementSameCShape(snapshot *types.Snapshot, a, b types.TypeID) bool {
	if a == b {
		return true
	}
	if widthA, integerA := resolvedBuiltin(snapshot, a); integerA && cType(widthA) != "" {
		widthB, integerB := resolvedBuiltin(snapshot, b)
		return integerB && cType(widthB) != "" && cType(widthA) == cType(widthB)
	}
	if isBool(snapshot, a) || isBool(snapshot, b) {
		return isBool(snapshot, a) && isBool(snapshot, b)
	}
	if isChar(snapshot, a) || isChar(snapshot, b) {
		return isChar(snapshot, a) && isChar(snapshot, b)
	}
	if isStr(snapshot, a) || isStr(snapshot, b) {
		return isStr(snapshot, a) && isStr(snapshot, b)
	}
	if isFloat(snapshot, a) || isFloat(snapshot, b) {
		return isFloat(snapshot, a) && isFloat(snapshot, b) &&
			resolvedFloatKind(snapshot, a) == resolvedFloatKind(snapshot, b)
	}
	if isTuple(snapshot, a) || isTuple(snapshot, b) {
		return isTuple(snapshot, a) && isTuple(snapshot, b) && tupleSameCShape(snapshot, a, b)
	}
	if isOptional(snapshot, a) || isOptional(snapshot, b) {
		if !isOptional(snapshot, a) || !isOptional(snapshot, b) {
			return false
		}
		keyA, _ := snapshot.Key(a)
		keyB, _ := snapshot.Key(b)
		payloadA, okA := keyA.Child()
		payloadB, okB := keyB.Child()
		return okA && okB && tupleElementSameCShape(snapshot, payloadA, payloadB)
	}
	return false
}

// buildTupleValueExpr builds a freshly-constructed tuple value as an ordinary
// C expression (10.25): a TupleValue node lowered to a positional C99 compound
// literal, `(pebble_tuple_<typeID>_t){ <e0>, <e1>, ... }`, whose element
// expressions are the TupleValue's children in order — the tuple typedef's
// field order is already the construction order, so a positional compound
// literal is a direct, correct lowering. The element list is built and
// validated by buildTupleBraceList (the same logic a tuple-typed local's
// declaration initializer uses), so an element of any type other than the
// entry's width or bool is rejected exactly the same way it would be in a
// declaration. The cast makes the compound literal a value usable anywhere a
// tuple-typed value is needed: a struct field's value
// (buildNestedAggregateValue), an optional's tuple payload (some <tuple>), a
// call argument for a tuple-typed parameter (buildAggregateArgument), a
// tuple-returning function's tail return (buildAggregateReturnValue), a
// whole-tuple reassignment (buildTupleStoreValue), and a tuple print operand
// (buildTuplePrintValueExpr). The C cast must always name the CALLER'S
// intended target type (wantType — the field's declared tuple type, the
// optional payload's tuple type, the parameter's tuple type, the result type,
// or the place's tuple type), never the literal node's own Type: the checker
// interns each tuple-literal occurrence its own structural TypeID, so the
// literal's own ID can diverge from the position's declared ID even when the
// shapes are identical (see tupleSameCShape); naming the literal's ID would
// emit a reference to a pebble_tuple_<ID>_t typedef that is never collected
// when that ID is not the declared position's type (issue #95). The two IDs
// must be the same C shape (defense for hand-built IR — the checker validates
// a tuple literal's arity/element types against its target position for real
// source); a genuinely different shape is a clean rejection, never a silent
// wrong cast. The node must be a TupleValue; the caller already guarantees
// this, so the kind check is defense for hand-built IR.
func buildTupleValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, wantType types.TypeID, context string, width types.BuiltinKind) (string, error) {
	if node.Kind != tir.TupleValue {
		return "", fmt.Errorf("%s contains a %s, want a TupleValue (a tuple literal)", context, node.Kind)
	}
	if !tupleSameCShape(snapshot, node.Type, wantType) {
		return "", fmt.Errorf("%s is a tuple literal of type %s, which does not match the target tuple type %s", context, tupleTypeName(node.Type), tupleTypeName(wantType))
	}
	braceList, err := buildTupleBraceList(st, unit, snapshot, fileSet, node, scope, context, width)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(%s)%s", tupleTypeName(wantType), braceList), nil
}

func buildNestedAggregateValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, typ types.TypeID, context string, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("%s references invalid aggregate value", context)
	}
	if node.Kind == tir.SymbolValue {
		info, ok := scope[node.Symbol]
		if !ok {
			return "", fmt.Errorf("%s references unknown aggregate symbol", context)
		}
		if info.tuple != typ && info.array != typ && info.optional != typ && info.structType != typ {
			return "", fmt.Errorf("%s aggregate symbol has the wrong type", context)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	switch {
	case isTuple(snapshot, typ):
		return buildTupleValueExpr(st, unit, snapshot, fileSet, node, scope, typ, context, width)
	case isStruct(snapshot, typ):
		return buildStructValueExpr(st, unit, snapshot, fileSet, node, scope, context, width)
	case isOptional(snapshot, typ):
		return buildOptionalValueExpr(st, unit, snapshot, fileSet, node, scope, context, width)
	}
	return "", fmt.Errorf("%s aggregate type is unsupported", context)
}

func buildUintExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok || !isUint(snapshot, node.Type) {
		return "", fmt.Errorf("uint expression has invalid node or type")
	}
	switch node.Kind {
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint source alias has %d children", len(node.Children))
		}
		return buildUintExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width)
	case tir.IntegerLiteral:
		litWidth, _ := resolvedBuiltin(snapshot, node.Type)
		return integerLiteralText(node.Literal.IntegerNum, litWidth), nil
	case tir.SizeofType:
		// The result of `sizeof T` for a uint expression. The emitted C is
		// `sizeof(<T's C type>)` where the C type is resolved from the
		// SizeofType's TypeArg: a fixed-width integer at its own C type
		// (int32_t for int/i32, int64_t for i64, uint64_t for uint/u64 — the
		// original three-width dispatch), or any aggregate's own typedef name
		// — the std/hmap.peb rehash/with_capacity shape (`new_cap * (sizeof
		// Entry[K, V])`) where the TypeArg is the Entry STRUCT type, which
		// must lower to sizeof(pebble_struct_<typeID>_t), never the default
		// sizeof(uint64_t) the builtin-only three-width dispatch would fall
		// through to (that would size the allocation 8 bytes per Entry
		// instead of 12, corrupting the rehash table).
		typeName, err := sizeofCTypeName(st, unit, snapshot, node.TypeArg)
		if err != nil {
			return "", err
		}
		return "sizeof(" + typeName + ")", nil
	case tir.SymbolValue:
		if name, ok := localOrGlobalName(st, node.Symbol, locals); ok {
			return name, nil
		}
		return "", fmt.Errorf("uint expression references unknown symbol %d", node.Symbol)
	case tir.IntegerCast:
		// An integer value cast to uint (`x as uint`), the std/hmap.peb
		// insert/get shape (`(hash as uint) % self.cap`). The destination is
		// uint by construction (buildUintExpr's gate above already required
		// node.Type to be uint), so the whole cast lowers to a plain C cast
		// to uint's own C type (uint64_t). The single child is built at its
		// OWN width: a uint-typed child recurses into this same builder (a
		// uint-to-uint cast is a no-op but must still be accepted), and any
		// other fixed-width integer child — u64 for hmap's hash (the
		// checker anchors the hash to u64, so the child is a u64-typed
		// SymbolValue or call) — is built by buildExpr at the child's own
		// resolved width, since u64 (and every other non-uint integer)
		// flows through the general path.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint expression contains an IntegerCast with %d children, want exactly one", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("uint expression IntegerCast references invalid child node %d", node.Children[0])
		}
		childWidth, integerChild := resolvedBuiltin(snapshot, child.Type)
		var childExpr string
		var err error
		if isUint(snapshot, child.Type) {
			childExpr, err = buildUintExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		} else if integerChild && cType(childWidth) != "" {
			childExpr, err = buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, childWidth, width)
		} else {
			return "", fmt.Errorf("uint expression IntegerCast child has type %s, want a fixed-width integer", describeType(snapshot, child.Type))
		}
		if err != nil {
			return "", err
		}
		return "(" + cType(types.Uint) + ")(" + childExpr + ")", nil
	case tir.PointerToInteger:
		// A pointer value cast to uint (`ptr as uint`), the uint twin of
		// buildExpr's PointerToInteger case: the whole cast lowers to a plain
		// C cast to uint's own C type (uint64_t) of the single child pointer
		// expression, built by buildExpr (whose pointer branch handles every
		// pointer-value shape). The destination is uint by construction
		// (buildUintExpr's gate above already required node.Type to be uint).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint expression contains a PointerToInteger with %d children, want exactly one", len(node.Children))
		}
		childExpr, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, width)
		if err != nil {
			return "", err
		}
		return "(" + cType(types.Uint) + ")(" + childExpr + ")", nil
	case tir.CharToInteger:
		// A char value cast to uint (`c as uint`), the uint twin of
		// buildExpr's CharToInteger case: the whole cast lowers to a plain,
		// unchecked C cast to uint's own C type (uint64_t) of the single char
		// child expression, built by buildCharOperand (a char literal, a
		// char-typed local reference, a char-returning call, or a char element
		// read). The well-definedness reasoning is exactly buildExpr's: a char
		// is a Unicode scalar value, and every valid codepoint (max 0x10FFFF)
		// fits identically in every integer width Pebble has, so reading out
		// the char's codepoint as a uint is always well-defined and needs no
		// runtime helper. uint needed its OWN case only because buildUintExpr
		// is a separate builder function from buildExpr (a uint-typed
		// CharToInteger node routes here, never to buildExpr's case), not
		// because the semantics differ. The destination is uint by
		// construction (buildUintExpr's gate above already required node.Type
		// to be uint), so there's no need to re-derive the destination width
		// the way buildExpr's case does — just cast to cType(types.Uint)
		// directly, exactly as the IntegerCast/PointerToInteger cases above
		// do for their own uint-cast wrapping.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint expression contains a CharToInteger with %d children, want exactly one", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("uint expression CharToInteger references invalid child node %d", node.Children[0])
		}
		if !isChar(snapshot, child.Type) {
			return "", fmt.Errorf("uint expression CharToInteger child has type %s, want a char value", describeType(snapshot, child.Type))
		}
		childExpr, err := buildCharOperand(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		return "(" + cType(types.Uint) + ")(" + childExpr + ")", nil
	case tir.Load:
		// A by-value read of a uint-typed place — a uint struct field read
		// (`var old_cap = self.cap;`, the std/hmap.peb rehash shape, lowered
		// to Load(FieldPlace)), a uint-typed tuple element, or a uint element
		// of a slice/array — used as a uint value. The Load's Type is already
		// gated to uint by buildUintExpr's entry check, so the whole read is
		// the place's C lvalue built by buildPlaceLValue (the same projection
		// a uint field write or address-of targets), whose resolved type must
		// be uint (defense for hand-built IR).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint expression contains a Load with %d children, want exactly one place", len(node.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", fmt.Errorf("uint expression place: %v", err)
		}
		if !isUint(snapshot, placeType) {
			return "", fmt.Errorf("uint expression reads a place of type %s, want uint", describeType(snapshot, placeType))
		}
		return lvalue, nil
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional whose payload is uint (`tombstone_index!`
		// where tombstone_index is `?uint`, the std/hmap.peb insert shape). The
		// unwrap is bounds-checked via the runtime helper selected from the
		// PAYLOAD's own type (optionalUnwrapSuffix maps a uint payload to the
		// u64 helper, since uint's .value field is uint64_t) — passing the
		// optional local's has_value and value fields. The single child is a
		// SymbolValue naming the optional-typed local.
		unwrapSuffix := optionalUnwrapSuffix(snapshot, node.Type)
		if unwrapSuffix == "" {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap of a %s payload, which has no runtime unwrap helper", describeType(snapshot, node.Type))
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		if info, declared := locals[child.Symbol]; !declared || info.optional == 0 {
			return "", fmt.Errorf("uint expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		return fmt.Sprintf("pebble_rt_checked_unwrap_%s(pebble_local_%d.has_value, pebble_local_%d.value, %s)", unwrapSuffix, child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
	case tir.CheckedArithmetic:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("uint arithmetic has %d operands", len(node.Children))
		}
		left, err := buildUintExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildUintExpr(st, unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		op, ok := arithmeticOperator(node.Operator)
		if !ok {
			return "", fmt.Errorf("unsupported uint arithmetic operator %s", node.Operator)
		}
		return fmt.Sprintf("(%s %s %s)", left, op, right), nil
	case tir.DirectCall:
		// A call to a uint-returning helper used as a uint value (`var n =
		// get_count();`, or std/io.peb's read_line shape `var bytes = read(
		// file, &ch as *void, 1);` — bytes is inferred uint from read's own
		// declared result type, so its initializer is a DirectCall node the
		// checker routes through this builder). The emitted C is
		// `pebble_fn_<callee>(ctx, ...)` — a C function call whose return
		// type IS the callee's declared uint64_t result (helperSignature
		// declares a uint-result helper with cType(types.Uint) == uint64_t,
		// the exact C type a uint value uses, the same pattern the
		// PointerToInteger/IntegerCast cases above use for their own uint-cast
		// wrapping), so the expression is directly a uint value, no cast
		// needed. The callee's declared result type is double-checked to be
		// uint (defense for hand-built IR — buildUintExpr's entry gate already
		// required node.Type to be uint, matching the defensive result-type
		// checks other cases here do, e.g. CheckedOptionalUnwrap's payload
		// check). The call is built by buildDirectCallNested, the pure-
		// expression-position call machinery: buildUintExpr returns (string,
		// error) with no pre-threading and is called from pure expression
		// positions throughout this file (local declarations, returns,
		// compound assignments, slice/array indices, struct field values,
		// and more), so an inline slice-construction argument folds its temp
		// declaration into a GNU statement-expression argument rather than
		// returning a pre the caller cannot place.
		calleeDecl, err := findCallDeclaration(unit, snapshot, node)
		if err != nil {
			return "", err
		}
		if !isUint(snapshot, calleeDecl.ResultType) {
			return "", fmt.Errorf("uint expression contains a call to symbol %d whose declared result type %s is not uint", node.Symbol, describeType(snapshot, calleeDecl.ResultType))
		}
		callExpr, err := buildDirectCallNested(st, unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", err
		}
		return callExpr, nil
	default:
		return "", fmt.Errorf("unsupported uint expression node %s", node.Kind)
	}
}

// buildOptionalAggregateUnwrapExpr builds the C expression text for a
// force-unwrap of an optional whose payload is a fixed array or a slice — the
// one payload family with no by-value scalar to return through the runtime's
// pebble_rt_checked_unwrap_<suffix> helpers. The lowering is a GNU
// statement-expression whose first statement is the presence-only runtime
// check (pebble_rt_checked_unwrap_present, which panics on an absent optional
// with PEBBLE_PANIC_UNWRAP_FAILED) and whose value is the optional's own
// .value field — the payload's pebble_array_<typeID>_t or
// pebble_slice_<typeID>_t struct read by value, the same C type the optional
// typedef declares the field with (see optionalPayloadCType). The base is a
// SymbolValue naming an optional-typed local or a Load of an optional-typed
// place; a freshly-computed base (a call result) would need a
// temp-declaration statement this pure-expression position has nowhere to
// place, so it is a clean rejection. The node's own Type (the unwrap result)
// must be exactly the optional's payload.
func buildOptionalAggregateUnwrapExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	if len(node.Children) != 1 {
		return "", fmt.Errorf("force-unwrap of an aggregate-payload optional has %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
	}
	child, ok := unit.Node(node.Children[0])
	if !ok {
		return "", fmt.Errorf("force-unwrap of an aggregate-payload optional references invalid child node %d", node.Children[0])
	}
	for child.Kind == tir.SourceAlias {
		if len(child.Children) != 1 {
			return "", fmt.Errorf("force-unwrap of an aggregate-payload optional has a base SourceAlias with %d child(ren), want exactly one", len(child.Children))
		}
		child, ok = unit.Node(child.Children[0])
		if !ok {
			return "", fmt.Errorf("force-unwrap of an aggregate-payload optional references invalid alias child node %d", child.Children[0])
		}
	}
	var baseExpr string
	var optionalType types.TypeID
	if child.Kind == tir.SymbolValue {
		info, declared := scope[child.Symbol]
		if !declared {
			return "", fmt.Errorf("force-unwrap of an aggregate-payload optional references symbol %d, which is not a local declared earlier in the body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("force-unwrap of an aggregate-payload optional references symbol %d, which is not an optional-typed local", child.Symbol)
		}
		optionalType = info.optional
		baseExpr = fmt.Sprintf("pebble_local_%d", child.Symbol)
	} else if child.Kind == tir.Load && len(child.Children) == 1 {
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		if !isOptional(snapshot, placeType) {
			return "", fmt.Errorf("force-unwrap of an aggregate-payload optional reads a place of type %s, want an optional-typed place", describeType(snapshot, placeType))
		}
		optionalType = placeType
		baseExpr = lvalue
	} else {
		return "", fmt.Errorf("force-unwrap of an aggregate-payload optional unwraps a %s, want a SymbolValue naming an optional-typed local or a Load of an optional-typed place", child.Kind)
	}
	payloadKey, ok := snapshot.Key(optionalType)
	if !ok || payloadKey.Kind() != types.Optional {
		return "", fmt.Errorf("force-unwrap of an aggregate-payload optional unwraps type %d, which is not an optional type in the type snapshot", optionalType)
	}
	payload, ok := payloadKey.Child()
	if !ok || payload != node.Type {
		return "", fmt.Errorf("force-unwrap of an aggregate-payload optional unwraps an optional whose payload %s is not the %s type %s the unwrap yields", describeType(snapshot, payload), describeType(snapshot, node.Type), describeType(snapshot, node.Type))
	}
	return fmt.Sprintf("({ pebble_rt_checked_unwrap_present(%s.has_value, %s); %s.value; })", baseExpr, buildSourceLoc(fileSet, node.Span), baseExpr), nil
}

func buildOptionalValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	key, ok := snapshot.Key(node.Type)
	if !ok {
		return "", fmt.Errorf("%s optional value type %d is not in the type snapshot", context, node.Type)
	}
	payload, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("%s optional value has no payload type", context)
	}
	if node.Kind == tir.NoneOptional {
		return fmt.Sprintf("(%s){ .has_value = false, .value = %s }", optionalTypeName(node.Type), zeroOptionalPayloadLiteral(unit, snapshot, payload)), nil
	}
	if (node.Kind != tir.SomeOptional && node.Kind != tir.OptionalInject) || len(node.Children) != 1 {
		return "", fmt.Errorf("%s contains a %s, want some, none, or an implicit-injection (some-without-the-keyword) optional value", context, node.Kind)
	}
	var value string
	var err error
	payloadWidth, integerPayload := resolvedBuiltin(snapshot, payload)
	switch {
	case integerPayload && cType(payloadWidth) != "" && !isUint(snapshot, payload):
		value, err = buildExpr(st, unit, snapshot, fileSet, node.Children[0], scope, payloadWidth, width)
	case isUint(snapshot, payload):
		value, err = buildUintExpr(st, unit, snapshot, fileSet, node.Children[0], scope, width)
	case isBool(snapshot, payload):
		value, err = buildBoolExpr(st, unit, snapshot, fileSet, node.Children[0], scope, width)
	case isFloat(snapshot, payload):
		// A float-typed payload's some/injected value is built by
		// buildFloatExpr at the payload's OWN float kind (resolvedFloatKind —
		// f32 or f64) and the entry width, exactly as a float call argument, a
		// float local's declaration initializer, and a float comparison
		// operand are built (task #22, slice 86a): a float literal, a
		// reference to an in-scope float-typed local, or a call to a
		// float-returning helper. The optional struct's .value field is the
		// plain C float/double (see optionalPayloadCType), so the built
		// expression matches the field type with no cast.
		value, err = buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], scope, resolvedFloatKind(snapshot, payload), width)
	case isTuple(snapshot, payload):
		// A tuple-payload some/injected value is built by buildTupleValueExpr
		// targeting the optional's DECLARED payload tuple type (payload, from
		// the optional type's own key above) — the .value field's C type — not
		// the literal's own structural tuple type ID, which can diverge (issue
		// #95).
		value, err = buildTupleValueExpr(st, unit, snapshot, fileSet, mustNode(unit, node.Children[0]), scope, payload, context, width)
	case isTaggedUnionType(unit, snapshot, payload):
		// A tagged-union payload's some/injected value is a union value (a
		// reference to an already-declared union-typed local, a variant
		// construction, a union-typed field read, or a union-payload
		// force-unwrap), built by buildUnionValueExpr into the optional's
		// .value field — the same C type the optional typedef declares the
		// field with (see optionalPayloadCType). This precedes the isStruct
		// case below exactly as the SomeOptional case's own union branch
		// precedes isEnumType: a tagged union is Nominal like a struct but its
		// value is a union value, never a RecordConstruct.
		value, err = buildUnionValueExpr(st, unit, snapshot, fileSet, node.Children[0], scope, context, payload, width)
	case isEnumType(unit, snapshot, payload):
		// A plain-enum payload's some/injected value is an enum value (a
		// variant literal like Color.blue, an enum-typed local reference, an
		// enum-returning call, or an enum-typed field read), built by
		// buildEnumValue into the optional's .value field — the same C type
		// the optional typedef declares the field with. This follows the
		// isTaggedUnionType case exactly as the SomeOptional branch's own
		// union case precedes isEnumType: a tagged union is enum-shaped too,
		// but its value is a union value, never a plain enum constant.
		value, err = buildEnumValue(st, unit, snapshot, fileSet, node.Children[0], scope, width)
	case isStruct(snapshot, payload):
		value, err = buildStructValueExpr(st, unit, snapshot, fileSet, mustNode(unit, node.Children[0]), scope, context, width)
	case isArray(snapshot, payload):
		// An array-typed payload's some/injected value is an array value (an
		// array literal, a reference to an array-typed local, a by-value read
		// of an array-typed place, or a call to an array-returning helper),
		// built by buildOptionalArrayPayload into the optional's .value field
		// — the same C type the optional typedef declares the field with (see
		// optionalPayloadCType).
		value, err = buildOptionalArrayPayload(st, unit, snapshot, fileSet, node.Children[0], scope, context, width)
	case isSlice(snapshot, payload):
		// A slice-typed payload's some/injected value is a slice value (a
		// reference to a slice-typed local, a by-value read of a slice-typed
		// place, a fresh slice construction, a call to a slice-returning
		// helper, or a raw slice construction), built by
		// buildOptionalSlicePayload into the optional's .value field — the
		// same C type the optional typedef declares the field with (see
		// optionalPayloadCType). nested == true folds an inline
		// checked-slice construction's checked-start temp into a GNU
		// statement-expression, since this builder is called from pure
		// expression positions with nowhere for a pre-statement.
		var payloadExpr string
		_, payloadExpr, err = buildOptionalSlicePayload(st, unit, snapshot, fileSet, node.Children[0], scope, context, width, true)
		value = payloadExpr
	case isPointer(snapshot, payload):
		// A pointer payload's value is built by the same buildExpr path a
		// pointer-typed value takes anywhere else (AddressOf, NilPointer, a
		// pointer-typed local reference, or a pointer-returning call), whose
		// isPointer bypass ignores the ambient width args.
		value, err = buildExpr(st, unit, snapshot, fileSet, node.Children[0], scope, width, width)
	default:
		return "", fmt.Errorf("%s optional payload %s is unsupported", context, describeType(snapshot, payload))
	}
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(%s){ .has_value = true, .value = %s }", optionalTypeName(node.Type), value), nil
}

// buildStructValueExpr builds a freshly-constructed struct value as an
// ordinary C expression (10.25): a RecordConstruct node lowered to a
// designated-initializer C99 compound literal,
// `(pebble_struct_<typeID>_t){ .pebble_field_<m0> = <e0>, ... }`. The field
// list is built and validated by buildStructBraceList (the same logic a
// struct-typed local's declaration initializer uses), so a construction
// site's field order still need not match the struct's declared order — the
// designated-initializer form handles the ordering in this position exactly
// as it does in a declaration — and a field of any type other than the entry's
// width or bool is rejected the same way it would be in a declaration. The
// cast makes the compound literal a value usable anywhere a struct-typed
// value is needed — in this slice, only as a call argument for a
// struct-typed parameter (buildAggregateArgument). The node must be a
// RecordConstruct; the caller already guarantees this, so the kind check is
// defense for hand-built IR.
func buildStructValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	if node.Kind != tir.RecordConstruct {
		return "", fmt.Errorf("%s contains a %s, want a RecordConstruct (a struct literal)", context, node.Kind)
	}
	if runtimeType(unit, snapshot, node.Type) != 0 {
		// A construction of the compiler's Allocator runtime type used in a
		// value position (a function return, a call argument, a store value).
		// It must be emitted as a (PebbleAllocator){ ... } compound literal over
		// the hand-written runtime ABI struct (runtimeFieldName's state/alloc/
		// realloc/free fields, callback values bridged by file-scope adapters),
		// NOT the generic pebble_struct_<typeID>_t compound literal: Allocator
		// has no per-TypeID struct typedef (typedef collection excludes runtime
		// types) and every other reference to an Allocator value — a local, a
		// parameter, a field, a helper's return type — names the hand-written
		// PebbleAllocator, so the compound literal must carry the same C type.
		inits, err := buildRuntimeAllocatorBraceList(st, unit, snapshot, fileSet, node, scope, context, width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s){ %s }", runtimeTypeName(unit, snapshot, node.Type), inits), nil
	}
	preStatements, braceList, err := buildStructBraceList(st, unit, snapshot, fileSet, node, scope, "", context, width)
	if err != nil {
		return "", err
	}
	if preStatements != "" {
		// A slice-typed field whose construction value is an inline CheckedSlice
		// needs its temp-declaration statement, and this is a pure expression
		// position (a call argument, a return value, or a nested aggregate
		// field) with nowhere to place it separately. Unlike a bare CheckedSlice
		// call argument — which the GNU statement-expression lowering in
		// buildSliceArgument / sliceConstructionStatementExpr folds inline — a
		// struct literal's slice-construction FIELD sits inside one brace-list
		// element of the enclosing struct compound literal, so the individual
		// field cannot be wrapped on its own; instead the WHOLE struct value is
		// folded into a single GNU statement-expression, `({ <temp decl>;
		// <struct literal>; })`, whose value is the struct literal — the same
		// primary-expression folding 836fbea applied to a bare CheckedSlice
		// call argument, generalized from the single-construction case to the
		// whole-struct case. GCC/Clang (this project's cc toolchain) support
		// the extension, and a statement-expression is a primary expression, so
		// it composes correctly inside a call argument list, a return
		// statement, and a nested aggregate's compound literal. The slice
		// temp-declaration statements are all emitted inside the statement
		// expression's braces; the struct literal itself is the trailing
		// expression statement (with its terminating semicolon, as documented
		// for sliceConstructionStatementExpr). The slice-typed-local-reference
		// shape (empty preStatements) is unaffected and keeps emitting the bare
		// compound literal.
		return fmt.Sprintf("({ %s (%s)%s; })", preStatements, structTypeName(node.Type), braceList), nil
	}
	return fmt.Sprintf("(%s)%s", structTypeName(node.Type), braceList), nil
}

// buildEnumValue builds the C expression text for a plain enum value node of
// seven shapes (all confirmed against real fixtures): an EnumVariantValue
// (Color.green, a variant literal with no payload), a zero-payload
// VariantConstruct (Color.red(), the parenthesized-call form of a plain
// enum's payload-less variant), a SymbolValue naming an enum-typed local
// declared earlier in the body (emitted as its pebble_local_<symbolID> C name),
// a DirectCall to an enum-returning helper (`switch pick() { ... }`, `pick()
// == Color.red`, or a return forward `return pick();` — the call already
// returns the enum's own C type, so the whole call expression is directly an
// enum value), a SourceAlias (transparent grouped-expression parens, e.g.
// `(2 as Color)`, unwrapped to its single child), a Load of an enum-typed
// struct field
// (`entry.state`, the enum-typed-struct-field shape — the projection carries
// the field's own pebble_enum_<typeID>_t C type), — since
// CheckedIntegerToEnum support landed — an integer cast to an enum (`5 as
// Color`, built by
// buildCheckedIntegerToEnumExpr through the checked runtime primitive), and —
// since enum-element slices — a Load of a CheckedIndexPlace, a by-value read
// of one enum element of an array or slice local (`colors[1]` in a comparison
// or switch subject, the bounds-checked element projection carrying the
// element's own pebble_enum_<typeID>_t C type). A
// variant literal emits its C enum constant
// pebble_variant_<member>, whose value is the variant's ordinal in the enum's
// declared order. A payload-carrying variant — an EnumVariantValue or
// VariantConstruct with one or more children — is a tagged-union construction,
// which real source routes to buildUnionConstruction instead; this rejection is
// defense for hand-built IR where such a construction reaches this plain-enum
// builder. Anything else is a clean
// rejection, never a guessed lowering. This is the one shared builder for an
// enum value wherever one is needed this slice: an enum-typed local's
// declaration initializer, a reassignment's new value, an enum switch's
// subject, and an enum comparison's operand.
func buildEnumValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	switch node.Kind {
	case tir.EnumVariantValue:
		if len(node.Children) == 1 {
			return "", fmt.Errorf("entry function body expression constructs enum variant symbol %d with a payload; a tagged-union (union enum) construction routes through buildUnionConstruction, never a plain enum value", node.Member)
		}
		return enumVariantName(node.Member), nil
	case tir.VariantConstruct:
		if len(node.Children) >= 1 {
			return "", fmt.Errorf("entry function body expression constructs enum variant symbol %d with %d payload(s); a tagged-union (union enum) construction routes through buildUnionConstruction, never a plain enum value", node.Member, len(node.Children))
		}
		return enumVariantName(node.Member), nil
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || info.enumType == 0 {
			if ginfo, isGlobal := st.globals[node.Symbol]; isGlobal {
				if ginfo.info.enumType == 0 {
					return "", fmt.Errorf("entry function body expression references global symbol %d, which is not an enum-typed global", node.Symbol)
				}
				return fmt.Sprintf("pebble_global_%d", node.Symbol), nil
			}
			if einfo, isExtern := st.externData[node.Symbol]; isExtern {
				if einfo.info.enumType == 0 {
					return "", fmt.Errorf("entry function body expression references extern variable symbol %d, which is not an enum-typed extern variable", node.Symbol)
				}
				return einfo.name, nil
			}
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not an enum-typed local declared earlier in the body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.DirectCall:
		// A call to an enum-returning helper used directly as an enum value:
		// `switch pick() { ... }` (the tracker's repro), `pick() ==
		// Color.red` (a comparison operand), a reassignment's new value, a
		// struct field's value, or a return forward (`return pick();` in an
		// enum-returning helper). The emitted C is
		// `pebble_fn_<callee>(ctx, ...)` — a C function call whose return
		// type IS the callee's declared enum-typed result (helperSignature
		// declares an enum-result helper with the enum's own
		// pebble_enum_<typeID>_t C return type, the exact C type an enum
		// value uses), so the whole call expression is directly an enum
		// value, no cast or intermediate local needed. The callee's declared
		// result type is double-checked to be an enum type (defense for
		// hand-built IR — buildEnumValue's callers are enum-typed positions,
		// and the reachability walk has already validated the callee for real
		// source). The call is built by buildDirectCallNested, the pure-
		// expression-position call machinery — buildEnumValue returns
		// (string, error) with no pre-threading and is called from pure
		// expression positions throughout this file (switch subjects,
		// comparison operands, struct field values, reassignments, and
		// returns), so an inline slice-construction argument folds its temp
		// declaration into a GNU statement-expression argument rather than
		// returning a pre the caller cannot place.
		calleeDecl, err := findCallDeclaration(unit, snapshot, node)
		if err != nil {
			return "", err
		}
		if !isEnumType(unit, snapshot, calleeDecl.ResultType) {
			return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose declared result type %s is not an enum type", node.Symbol, describeType(snapshot, calleeDecl.ResultType))
		}
		callExpr, err := buildDirectCallNested(st, unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", err
		}
		return callExpr, nil
	case tir.SourceAlias:
		// A SourceAlias is transparent — it records grouped-expression parens
		// (e.g. `(2 as Color) == Color.blue`) and nothing else — so it is
		// unwrapped and its single child built, exactly as buildBoolExpr and
		// buildFloatExpr already unwrap it.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a SourceAlias with %d child(ren), want exactly one", len(node.Children))
		}
		return buildEnumValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
	case tir.CheckedIntegerToEnum:
		return buildCheckedIntegerToEnumExpr(st, unit, snapshot, fileSet, node, locals, "entry function body expression", width)
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an enum-payload optional (`c!` where c is
		// ?Color), used as an enum value — the read-back path for an
		// OptionalIntegerToEnum-constructed local (`5 as ?Color`), which is
		// how the in-range round trip (`c! as i32 == 1`) is observable end to
		// end. The unwrap reads the optional local's has_value and value
		// fields through the checked-unwrap runtime helper at the C enum's
		// underlying width (a C enum is int-compatible, so
		// pebble_rt_checked_unwrap_i32's value parameter and int32_t result
		// both accept the enum's representation directly), and the int32_t
		// result is narrowed back to the enum typedef. The child must name an
		// optional-typed local whose payload is exactly the enum the unwrap
		// yields.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of an enum-payload optional whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		info, declared := locals[child.Symbol]
		if !declared {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not a local declared earlier in the entry body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		payloadKey, ok := snapshot.Key(info.optional)
		if !ok || payloadKey.Kind() != types.Optional {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d with unresolvable optional type %d", child.Symbol, info.optional)
		}
		payload, ok := payloadKey.Child()
		if !ok || !isEnumType(unit, snapshot, payload) || payload != node.Type {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d whose payload %s is not the enum type %s the unwrap yields", child.Symbol, describeType(snapshot, payload), enumTypeName(node.Type))
		}
		return fmt.Sprintf("(%s)pebble_rt_checked_unwrap_i32(pebble_local_%d.has_value, pebble_local_%d.value, %s)", enumTypeName(node.Type), child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
	case tir.Load:
		// A read of an enum-typed place used directly as an enum value, in two
		// shapes:
		//
		//   - a struct field read of an enum-typed field (`entry.state`, the
		//     std/hmap.peb insert comparison shape) — lowered by the checker to a
		//     Load of a FieldPlace whose single child names the struct local,
		//     exactly the shape buildExpr's Load case routes to
		//     buildStructFieldRead. The projection
		//     pebble_local_<sym>.pebble_field_<m> carries the field's own
		//     pebble_enum_<typeID>_t C type, which is directly comparable to
		//     another enum value's constant.
		//   - an enum element of an array or slice local (`colors[1]`, the
		//     enum-element-slice shape) — lowered to a Load of a
		//     CheckedIndexPlace whose single child is the StoragePlace naming
		//     the array/slice local, exactly the shape a scalar element read
		//     uses. The bounds-checked element projection built by
		//     buildPlaceLValue carries the element's own pebble_enum_<typeID>_t
		//     C type.
		//
		// The Load's own Type must be the enum type (the checker guarantees it
		// for real source; a mismatch is a clean rejection for hand-built IR).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if !isEnumType(unit, snapshot, node.Type) {
			return "", fmt.Errorf("entry function body expression contains a Load of type %s, want an enum type", describeType(snapshot, node.Type))
		}
		if place.Kind == tir.FieldPlace {
			return buildStructFieldRead(st, unit, snapshot, fileSet, place, locals, width, false)
		}
		if place.Kind == tir.DereferencePlace {
			// A whole enum read through a pointer deref used directly as an
			// enum value — `read(*ptr)`, `*ptr == Color.green`, `return *ptr;`
			// in an enum-returning helper. The argument is a Load whose place
			// is a DereferencePlace, emitted as the null-checked dereference
			// value buildDereferencePlaceRead produces
			// (`*(pebble_enum_<typeID>_t)(pebble_rt_checked_deref_ptr(...))`)
			// — the enum's own typedef makes the by-value deref read trivially
			// valid C, the same whole-enum read shape the struct and tuple
			// sides' deref reads use.
			return buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, locals, width, node.Span, false)
		}
		if place.Kind != tir.CheckedIndexPlace {
			return "", fmt.Errorf("entry function body expression contains a Load whose place is a %s, want a FieldPlace (an enum-typed struct field read) or a CheckedIndexPlace (an enum element of an array or slice)", place.Kind)
		}
		lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", fmt.Errorf("entry function body expression contains an enum-element read: %v", err)
		}
		if !isEnumType(unit, snapshot, elementType) {
			return "", fmt.Errorf("entry function body expression contains an enum-element read of type %s, want an enum type", describeType(snapshot, elementType))
		}
		return lvalue, nil
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want an enum variant literal (an EnumVariantValue) or a reference to an enum-typed local", node.Kind)
	}
}

// buildCheckedIntegerToEnumExpr builds the C expression text for one integer-to-
// enum cast (a tir.CheckedIntegerToEnum, e.g. `5 as Color`), the shared builder
// behind every enum-value position the cast can occupy: an enum-typed local's
// declaration initializer, a reassignment's new value, an enum switch's
// subject, and an enum comparison's operand. The destination enum type is the
// node's own Type (a plain enum — the checker only ever routes an integer to a
// NominalEnum destination; a tagged-union destination is impossible), and the
// emitted C is
//
//	(<destination enum C type>)pebble_rt_checked_int_to_enum((int64_t)(<child expr>), <variant_count>, <source loc>)
//
// The single child is the ordinary integer being cast, built by the integer
// expression builder (buildExpr) at its own declared width, not by
// buildEnumValue (which is for enum-typed operands). The variant count comes
// from the destination enum's TypeDecl.Members length: Pebble enums are
// ordinal, variant Members[i] gets the C enum value i, so an integer names a
// real variant exactly when 0 <= value < variant_count — the runtime primitive
// enforces exactly that bound (see pebble_rt.h), SAFE panicking out-of-range
// and RELEASE returning the value unchecked.
func buildCheckedIntegerToEnumExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, context string, entryWidth types.BuiltinKind) (string, error) {
	if node.Kind != tir.CheckedIntegerToEnum {
		return "", fmt.Errorf("%s contains a %s, want a CheckedIntegerToEnum", context, node.Kind)
	}
	if len(node.Children) != 1 {
		return "", fmt.Errorf("%s contains a CheckedIntegerToEnum with %d children, want exactly one", context, len(node.Children))
	}
	info, err := resolveEnumInfo(unit, snapshot, node.Type)
	if err != nil {
		return "", fmt.Errorf("%s integer-to-enum cast: %v", context, err)
	}
	if len(info.variants) == 0 {
		return "", fmt.Errorf("%s integer-to-enum cast targets enum %s, which has no declared variants", context, enumTypeName(node.Type))
	}
	child, ok := unit.Node(node.Children[0])
	if !ok {
		return "", fmt.Errorf("%s integer-to-enum cast references invalid child node %d", context, node.Children[0])
	}
	childType, ok := snapshot.Key(child.Type)
	if !ok {
		return "", fmt.Errorf("%s integer-to-enum cast child has invalid type %d", context, child.Type)
	}
	childWidth, ok := childType.Builtin()
	if !ok || cType(childWidth) == "" {
		return "", fmt.Errorf("%s integer-to-enum cast child has non-integer type %s", context, describeType(snapshot, child.Type))
	}
	childExpr, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, childWidth, entryWidth)
	if err != nil {
		return "", fmt.Errorf("%s integer-to-enum cast child: %v", context, err)
	}
	return fmt.Sprintf("(%s)pebble_rt_checked_int_to_enum((int64_t)(%s), %d, %s)", enumTypeName(node.Type), childExpr, len(info.variants), buildSourceLoc(fileSet, node.Span)), nil
}

// buildUnionValueExpr builds the C expression text for one tagged-union value
// in a pure value position (an optional's `some <expr>` payload or a union-
// typed call argument): a reference to an already-declared union-typed local in
// scope of exactly want (a SymbolValue, emitted as the local's own
// pebble_local_<symbol> C name), a DirectCall to a union-returning helper
// (`takes(pick())` passing a freshly-returned union to a union parameter, or a
// return forward `return pick();` in a union-returning helper — the call
// already returns the union's own C type, so the whole call expression is
// directly a union value of exactly want), a read of a union-typed struct field
// (a Load
// of a FieldPlace, `h.tag`), a force-unwrap of a union-payload optional (a
// CheckedOptionalUnwrap, `o!`), or a fresh variant construction
// (Choice.value(5) / Choice.empty / Choice.empty(), built by the same
// buildUnionConstruction a union local's declaration uses, with the union's
// info resolved on demand by resolveUnionInfoForValue). A SourceAlias is
// transparent and unwrapped to its single child, exactly as buildEnumValue
// unwraps one. Every shape must resolve to exactly the want union type (the
// checker guarantees it for real source; the check is defense for hand-built
// IR) and emits a value of that union's own pebble_union_<typeID>_t C type, so
// the value is trivially valid wherever the union typedef is expected. Any
// other node kind is a clean rejection, never a guessed lowering.
func buildUnionValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, context string, want types.TypeID, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("%s references invalid node %d", context, id)
	}
	switch node.Kind {
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || info.enumType == 0 {
			if ginfo, isGlobal := st.globals[node.Symbol]; isGlobal {
				if ginfo.info.enumType != want {
					return "", fmt.Errorf("%s references global symbol %d, which is not a tagged-union-typed global of type %s", context, node.Symbol, unionTypeName(want))
				}
				return fmt.Sprintf("pebble_global_%d", node.Symbol), nil
			}
			if einfo, isExtern := st.externData[node.Symbol]; isExtern {
				if einfo.info.enumType != want {
					return "", fmt.Errorf("%s references extern variable symbol %d, which is not a tagged-union-typed extern variable of type %s", context, node.Symbol, unionTypeName(want))
				}
				return einfo.name, nil
			}
			return "", fmt.Errorf("%s references symbol %d, which is not an enum/tagged-union-typed local declared earlier in the body", context, node.Symbol)
		}
		if info.enumType != want {
			return "", fmt.Errorf("%s references symbol %d, a local of type %s, not the union type %s", context, node.Symbol, describeType(snapshot, info.enumType), unionTypeName(want))
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.DirectCall:
		// A call to a union-returning helper used directly as a tagged-union
		// value: `takes(pick())` (a union-typed call argument, e.g. the
		// std/optional.peb unwrap_or shape), `some pick()` (an optional's
		// union payload), or a return forward (`return pick();` in a
		// union-returning helper). The emitted C is
		// `pebble_fn_<callee>(ctx, ...)` — a C function call whose return
		// type IS the callee's declared union-typed result (helperSignature
		// declares a union-result helper with the union's own
		// pebble_union_<typeID>_t C return type, the exact C type a union
		// value uses), so the whole call expression is directly a union value
		// of exactly want, no cast or intermediate local needed. The call's
		// own Type (the callee's resolved result type) is double-checked to
		// be exactly want (defense for hand-built IR — the reachability walk
		// has already validated the callee for real source). The call is
		// built by buildDirectCallNested, the pure-expression-position call
		// machinery — buildUnionValueExpr returns (string, error) with no
		// pre-threading and is called from pure expression positions
		// throughout this file and its callers (call arguments, optional
		// payloads, and returns), so an inline slice-construction argument
		// folds its temp declaration into a GNU statement-expression argument
		// rather than returning a pre the caller cannot place.
		if node.Type != want {
			return "", fmt.Errorf("%s contains a call to symbol %d whose declared result type %s is not the union type %s", context, node.Symbol, describeType(snapshot, node.Type), unionTypeName(want))
		}
		callExpr, err := buildDirectCallNested(st, unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", err
		}
		return callExpr, nil
	case tir.EnumVariantValue, tir.VariantConstruct:
		if node.Type != want {
			return "", fmt.Errorf("%s constructs union type %s, want %s", context, unionTypeName(node.Type), unionTypeName(want))
		}
		info, err := resolveUnionInfoForValue(unit, snapshot, node.Type)
		if err != nil {
			return "", err
		}
		return buildUnionConstruction(st, unit, snapshot, fileSet, node, locals, context, info, width)
	case tir.Load:
		// A read of a union-typed struct field (`h.tag`, the struct-field
		// read-back shape): a Load of a FieldPlace whose place's declared field
		// type is the tagged union, lowered by buildStructFieldRead to the
		// field's own C projection, which the struct's typedef declares with the
		// union's own pebble_union_<typeID>_t (see structFieldCType).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a Load with %d child(ren), want exactly one place", context, len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s contains a Load referencing invalid place node %d", context, node.Children[0])
		}
		if place.Kind != tir.FieldPlace {
			return "", fmt.Errorf("%s contains a Load whose place is a %s, want a FieldPlace (a tagged-union struct field read)", context, place.Kind)
		}
		if node.Type != want {
			return "", fmt.Errorf("%s contains a Load of type %s, want a field of the union type %s", context, describeType(snapshot, node.Type), unionTypeName(want))
		}
		return buildStructFieldRead(st, unit, snapshot, fileSet, place, locals, width, false)
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of a union-payload optional (`o!`, the optional
		// read-back shape): the unwrap's own Type is the union, and the optional
		// local's payload must be exactly that union (see
		// buildUnionUnwrapExpr).
		if node.Type != want {
			return "", fmt.Errorf("%s contains a CheckedOptionalUnwrap of union type %s, want %s", context, unionTypeName(node.Type), unionTypeName(want))
		}
		return buildUnionUnwrapExpr(st, unit, snapshot, fileSet, node, locals, width)
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a SourceAlias with %d child(ren), want exactly one", context, len(node.Children))
		}
		return buildUnionValueExpr(st, unit, snapshot, fileSet, node.Children[0], locals, context, want, width)
	default:
		return "", fmt.Errorf("%s contains a %s of tagged-union type, want a reference to a union-typed local, a union variant construction, a union-typed struct field read, or a union-payload optional force-unwrap", context, node.Kind)
	}
}

// buildUnionUnwrapExpr builds the C expression text for one force-unwrap of an
// optional whose payload is a tagged union (a tir.CheckedOptionalUnwrap whose
// own Type is the union, e.g. `o!` where o is ?Choice): the union value read
// back out of the optional's .value field, checked for presence. The runtime's
// pebble_rt_checked_unwrap_* helpers are one-per-scalar-width and have no
// struct-returning form, so the check is emitted inline as a C conditional
// whose absent branch panics:
//
//	<base>.has_value ? <base>.value : (pebble_rt_panic(&(PebblePanicInfo){ .kind = PEBBLE_PANIC_UNWRAP_FAILED, ... }), (pebble_union_<typeID>_t){0})
//
// the absent branch's comma expression reuses the same
// PEBBLE_PANIC_UNWRAP_FAILED panic the scalar unwrap helpers raise (see
// buildUnionUnwrapPanicElse), and the ternary's two branches share the union's
// own typedef type, so the whole expression is a valid C value of
// pebble_union_<typeID>_t. The base is either an optional-typed local
// (a SymbolValue, `pebble_local_<sym>`) or an optional-typed struct field read
// (a Load of a FieldPlace, the `b.value!` shape); the optional's payload must
// be exactly the unwrap's own union Type (guaranteed for real source by the
// checker; the check is defense for hand-built IR).
func buildUnionUnwrapExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	if len(node.Children) != 1 {
		return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
	}
	child, ok := unit.Node(node.Children[0])
	if !ok {
		return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
	}
	var base string
	var optionalType types.TypeID
	switch child.Kind {
	case tir.SymbolValue:
		info, declared := locals[child.Symbol]
		if !declared || info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not an optional-typed local", child.Symbol)
		}
		base = fmt.Sprintf("pebble_local_%d", child.Symbol)
		optionalType = info.optional
	case tir.Load:
		if len(child.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of a %s, want a place", child.Kind)
		}
		expr, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		if !isOptional(snapshot, typ) {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of a %s, want an optional-typed place", describeType(snapshot, typ))
		}
		base = expr
		optionalType = typ
	default:
		return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local or a Load of an optional-typed place", child.Kind)
	}
	payloadKey, ok := snapshot.Key(optionalType)
	if !ok || payloadKey.Kind() != types.Optional {
		return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of an unresolvable optional type %d", optionalType)
	}
	payload, ok := payloadKey.Child()
	if !ok || payload != node.Type {
		return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d whose payload %s is not the union type %s the unwrap yields", child.Symbol, describeType(snapshot, payload), unionTypeName(node.Type))
	}
	return fmt.Sprintf("%s.has_value ? %s.value : %s", base, base, buildUnionUnwrapPanicElse(fileSet, node.Span, node.Type)), nil
}

// buildStrLiteralValue builds the C text constructing a PebbleStr value from a
// StringLiteral node's decoded bytes: the `{ .data = (const uint8_t *)
// "<escaped>", .len = <N> }` brace text every str value is built from. It is
// the single source of the string-literal-to-PebbleStr construction text,
// shared byte-for-byte by the three places a str value is built from a
// literal: a str-typed local's declaration initializer (buildStrLocalDeclaration
// embeds it in `PebbleStr pebble_local_<id> = <text>;`), a comparison operand
// with no local behind it (buildStrOperand wraps it in a (PebbleStr) compound
// literal), and a str-typed local's reassignment (buildStoreCore wraps it the
// same way) — so a declaration and a later reassignment from the same literal
// emit byte-identical PebbleStr construction text. The escaping is
// escapeCString's fixed-width-octal scheme (a \NNN octal escape for every
// non-printable byte, so C's maximal-munch escape rules can never swallow a
// following digit) and the length is the literal's compile-time decoded byte
// length, so no runtime strlen is involved. A StringLiteral whose literal kind
// is not a decoded string is a clean rejection.
func buildStrLiteralValue(node tir.Node) (string, error) {
	if node.Literal.Kind != tir.LiteralString {
		return "", fmt.Errorf("contains a StringLiteral with literal kind %s, want a decoded string", node.Literal.Kind)
	}
	text := node.Literal.String
	return fmt.Sprintf("{ .data = (const uint8_t *)\"%s\", .len = %d }", escapeCString(text), len(text)), nil
}

// buildCharLiteralValue builds the C text for one CharLiteral node: its
// decoded rune emitted as an int32_t decimal literal, `(int32_t)97`. A char's
// C representation is always the fixed int32_t — a Unicode scalar value fits
// in 21 bits, so no emitted literal ever overflows a signed 32-bit constant,
// regardless of the entry's resolved integer width (the two are unrelated
// concepts: the entry's width picks integer arithmetic's size; a char's size
// is fixed by the Unicode scalar value range). The decimal text comes from the
// literal's Char field (a Go rune, an int32 alias) with no escaping and no
// width splitting, so a non-ASCII value like 'é' (233) or an emoji such as
// '😀' (128512) emits its full scalar value, not a truncated byte. A
// CharLiteral whose literal kind is not a decoded character is a clean
// rejection.
func buildCharLiteralValue(node tir.Node) (string, error) {
	if node.Literal.Kind != tir.LiteralChar {
		return "", fmt.Errorf("contains a CharLiteral with literal kind %s, want a decoded character", node.Literal.Kind)
	}
	return fmt.Sprintf("(int32_t)%d", node.Literal.Char), nil
}

// buildCondition builds the C text for one if/while condition. It dispatches
// on the condition node's shape: a direct integer comparison (tir.BinaryValue)
// keeps the existing buildComparison path unchanged, while a bare bool value —
// a bool literal, a reference to an in-scope bool local, a unary ! negation of
// one of those (tir.PrefixValue with the Bang operator), a comparison used as
// a bool operand, or a && / || combination of any of these (a
// tir.ShortCircuitValue) — is routed through buildBoolExpr. Anything else is
// rejected by whichever builder it reaches.
func buildCondition(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body condition references invalid node %d", id)
	}
	if node.Kind == tir.BinaryValue {
		return buildComparison(st, unit, snapshot, fileSet, id, locals, width)
	}
	return buildBoolExpr(st, unit, snapshot, fileSet, id, locals, width)
}

// buildComparison builds the C text for an if condition. It accepts exactly a
// tir.BinaryValue with two operands and one of the six comparison operators
// (<, <=, >, >=, ==, !=), and emits the plain C operator directly — comparing
// two integers, two char values, or two bools with ==/!=, cannot overflow, so
// no runtime helper
// is needed. The operand grammar is decided from the operands' own resolved
// types, not assumed to be integers: when both operands carry the snapshot's
// str builtin, they are an equality between two str values built by
// buildStrOperand and lowered to the runtime helper
// pebble_rt_str_eq(<left>, <right>) (==) or its negation (!=) — ordering
// comparisons between strs are rejected cleanly, since the checker does not
// reject them from source (confirmed against a real fixture). When both
// operands carry the snapshot's
// char builtin, they are two char values built by buildCharOperand (a char
// literal, a char local reference, or a call to a char-returning helper), and
// all six operators are legal — comparing Unicode scalar values numerically
// is well-defined, and the checker accepts ordering comparisons between chars
// (confirmed against a real fixture) — emitted as the plain C operator with
// no runtime helper. When both
// operands carry the snapshot's
// bool builtin, they are built by buildBoolExpr (a bool comparison result, a
// bool local, a bool literal, a ! negation, or a && / || combination — the
// wrapped-comparison shape (1 < 2) == (3 < 4) is exactly this, its two
// SourceAlias-wrapped comparison operands being bool values), and only the
// ==/!= operators are legal for bool operands — the checker itself rejects an
// ordering comparison between bools (C0603, confirmed against a real fixture),
// so that ordering guard is defense for hand-built IR, not a reachable source
// shape. Both bool operands are parenthesized in the emitted C so a bool
// operand that is itself a comparison cannot chain associatively with the
// outer operator (e.g. (a == b) == (c == d) must not collapse to a left-to-
// right a == b == c == d). Otherwise each operand is built by
// buildComparisonOperand (an int-typed integer literal, an unanchored
// loop-iterator symbol, or any fixed-width-integer expression buildExpr
// accepts at the operand's own resolved width, including non-entry-width
// integers like u64, i64, or u8). Any other node kind, or any other operator on a
// BinaryValue (bitwise), is a clean rejection. The && / || that lower to
// ShortCircuitValue nodes are not this function's concern — buildCondition
// routes them to buildBoolExpr.
func buildComparison(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid node %d", id)
	}
	if node.Kind != tir.BinaryValue {
		return "", fmt.Errorf("entry function body if condition is a %s, want a direct integer comparison or a ==/!= between two bool values (<, <=, >, >=, ==, or !=)", node.Kind)
	}
	if len(node.Children) != 2 {
		return "", fmt.Errorf("entry function body if condition has %d operand(s), want exactly two operands", len(node.Children))
	}
	op, ok := comparisonOperator(node.Operator)
	if !ok {
		return "", fmt.Errorf("entry function body if condition uses operator %s, want one of <, <=, >, >=, ==, or !=", node.Operator)
	}
	leftOperand, ok := unit.Node(node.Children[0])
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid operand node %d", node.Children[0])
	}
	rightOperand, ok := unit.Node(node.Children[1])
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid operand node %d", node.Children[1])
	}
	if isStr(snapshot, leftOperand.Type) && isStr(snapshot, rightOperand.Type) {
		// A comparison between two str values: ==, !=, <, <=, >, >=. Equality
		// and inequality are lowered via the runtime helper pebble_rt_str_eq
		// (byte-for-byte, length-prefixed — no strlen, no NUL-termination
		// dependence): == emits the call directly and != emits its negation.
		// Ordering comparisons are lowered via pebble_rt_str_cmp, which
		// returns negative/zero/positive like C's memcmp/strcmp, and the result
		// is compared against 0 using the source operator translated to its C
		// spelling. Each operand is built by buildStrOperand — a reference to
		// an in-scope str local, or a string literal embedded as a PebbleStr
		// compound literal — so a literal operand participates in a comparison
		// without needing a declared local.
		left, err := buildStrOperand(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildStrOperand(st, unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		if node.Operator == syntax.Equal {
			return "pebble_rt_str_eq(" + left + ", " + right + ")", nil
		}
		if node.Operator == syntax.NotEqual {
			return "!pebble_rt_str_eq(" + left + ", " + right + ")", nil
		}
		// Ordering operators: <, <=, >, >= — the runtime helper
		// pebble_rt_str_cmp returns negative/zero/positive and the source
		// operator is translated to its C spelling by comparisonOperator,
		// which has already validated the token kind above.
		return "pebble_rt_str_cmp(" + left + ", " + right + ") " + op + " 0", nil
	}
	if isChar(snapshot, leftOperand.Type) && isChar(snapshot, rightOperand.Type) {
		// A comparison between two char values — c == 'a', c != d, and all
		// four ordering operators (c < d and so on), all confirmed
		// checker-reachable against real fixtures: a char is a Unicode scalar
		// value, and comparing two scalar values numerically is well-defined
		// for every one of the six operators, so the plain C operator is a
		// direct, correct lowering — no runtime helper (this is not the str
		// case) and no overflow concern (comparisons never fault). Both
		// operands are built by buildCharOperand (a char literal, a char
		// local reference, or a call to a char-returning helper), each
		// emitted as an int32_t value, so a literal operand participates
		// without needing a declared local.
		left, err := buildCharOperand(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildCharOperand(st, unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return left + " " + op + " " + right, nil
	}
	if isBool(snapshot, leftOperand.Type) && isBool(snapshot, rightOperand.Type) {
		// Both operands are bool values, so this is an equality between bools
		// — (1 < 2) == (3 < 4), a == b, true == a, and so on. Only ==/!= make
		// sense for bool operands; an ordering comparison here is impossible
		// from real source (the checker rejects it as C0603 before typed IR
		// exists), but is rejected cleanly rather than guessed for hand-built
		// IR. The operands are built under the bool grammar by buildBoolExpr,
		// each parenthesized so a comparison operand cannot chain associatively
		// with the outer operator in the emitted C.
		if node.Operator != syntax.Equal && node.Operator != syntax.NotEqual {
			return "", fmt.Errorf("entry function body if condition compares two bool operands with operator %s, want == or !=", node.Operator)
		}
		left, err := buildBoolExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildBoolExpr(st, unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return "(" + left + ") " + op + " (" + right + ")", nil
	}
	if isFloat(snapshot, leftOperand.Type) && leftOperand.Type == rightOperand.Type {
		// Float arithmetic and comparisons have defined C semantics, including
		// overflow, infinities, NaNs, and division by zero. Emit the comparison
		// directly after building both operands at their shared float width.
		left, err := buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], locals, resolvedFloatKind(snapshot, leftOperand.Type), width)
		if err != nil {
			return "", err
		}
		right, err := buildFloatExpr(st, unit, snapshot, fileSet, node.Children[1], locals, resolvedFloatKind(snapshot, rightOperand.Type), width)
		if err != nil {
			return "", err
		}
		return left + " " + op + " " + right, nil
	}
	if isEnumType(unit, snapshot, leftOperand.Type) && isEnumType(unit, snapshot, rightOperand.Type) {
		// A comparison between two plain enum values — c == Color.red,
		// c != Color.red, and (confirmed against a real fixture) the ordering
		// comparisons c < Color.red and so on, all accepted by the checker and
		// therefore reachable. Both operands are built by buildEnumValue (an
		// enum-typed local reference or a variant literal) and the plain C
		// operator is emitted directly: a C enum's value IS the variant's
		// ordinal in declared order, so comparing two enum values compares
		// their discriminants — a direct, correct lowering that cannot fault.
		// The two enum types must match (the checker guarantees it for real
		// source; mismatched operands are a clean rejection for hand-built IR).
		if leftOperand.Type != rightOperand.Type {
			return "", fmt.Errorf("entry function body if condition compares two enum values of different types %s and %s", enumTypeName(leftOperand.Type), enumTypeName(rightOperand.Type))
		}
		left, err := buildEnumValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildEnumValue(st, unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return left + " " + op + " " + right, nil
	}
	// Both integer operands are built at their OWN resolved integer widths
	// (see buildComparisonOperand), so a comparison between two fixed-width
	// integers of DIFFERENT widths (a u64 vs a u8, or a u64 vs an i32) is
	// rejected cleanly here rather than silently emitting mismatched C types
	// side by side — the same explicit same-type requirement the enum branch
	// above enforces. Real source never reaches here mismatched: the checker
	// requires both comparison operands to carry the identical concrete type
	// (confirmed against a real fixture — a `u64 == u8` comparison is a clean
	// T0505 "cannot unify" check-phase rejection before typed IR exists), so
	// this guard is defense for hand-built IR only, exactly like the enum
	// branch's.
	leftWidth, leftInteger := resolvedBuiltin(snapshot, leftOperand.Type)
	rightWidth, rightInteger := resolvedBuiltin(snapshot, rightOperand.Type)
	if leftInteger && rightInteger && cType(leftWidth) != "" && cType(rightWidth) != "" && leftWidth != rightWidth {
		return "", fmt.Errorf("entry function body if condition compares two integer values of different widths %s and %s", wantName(leftWidth), wantName(rightWidth))
	}
	left, err := buildComparisonOperand(st, unit, snapshot, fileSet, node.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	right, err := buildComparisonOperand(st, unit, snapshot, fileSet, node.Children[1], locals, width)
	if err != nil {
		return "", err
	}
	return left + " " + op + " " + right, nil
}

// buildComparisonOperand builds one comparison operand. A bare comparison
// between two untyped integer literals defaults both operands to the
// snapshot's int builtin (confirmed against a real fixture — the same for an
// i64 entry as for an i32 one, since a bare comparison has no anchor), so an
// IntegerLiteral of type int is lowered directly as its decimal text. An
// int-typed SymbolValue operand is likewise lowered directly as its
// pebble_local_<symbol> name: in this backend's grammar such a symbol can
// only be a range loop's iterator referenced from inside its own body when
// the iterator is never used in a width-anchoring position (confirmed against
// a real fixture — `loop 0..3 : i { if i == 2 { ... } }` leaves the iterator
// as the unanchored int builtin, since the comparison anchors nothing), and
// the iterator is always declared in C at the entry's width, so its name is
// the correct C lvalue in the comparison. Every
// other integer operand is built at its OWN resolved integer width rather
// than the ambient entry width — a comparison between two non-entry-width
// integers (a u64 local compared against a literal, an i8 local, and so on)
// must build each operand at that operand's own width, since the operand's C
// local is declared at its own width and the checker anchors each operand to
// the other (both comparison operands always carry the same concrete integer
// type). This is the same per-operand width resolution buildCallArgument
// performs for a function-type parameter. uint is deliberately excluded, as
// buildCallArgument excludes it: uint is the platform-native pointer-width
// builtin the backend builds through buildUintExpr (sizeof, slice bounds,
// checked arithmetic), not the general buildExpr path. The ambient entry
// width is threaded through as buildExpr's entryWidth parameter so any
// width-requiring child (a checked runtime call) still knows the true entry
// width, not conflated with the operand's own width. Anything else must be an
// expression of the entry's width that buildExpr
// accepts — a reference to a local declared earlier in the entry body, or
// checked negation
// and checked +, -, *, /, % arithmetic — and is delegated to buildExpr, whose
// own width gate and kind switch do the rejecting.
func buildComparisonOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid operand node %d", id)
	}
	if node.Kind == tir.IntegerLiteral && node.Type == snapshot.Builtins().Int {
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("entry function body if condition contains an integer literal with malformed text %q", text)
		}
		return text, nil
	}
	if node.Kind == tir.SymbolValue && node.Type == snapshot.Builtins().Int {
		if name, ok := localOrGlobalName(st, node.Symbol, locals); ok {
			return name, nil
		}
		return "", fmt.Errorf("entry function body if condition references symbol %d, which is not a local in scope", node.Symbol)
	}
	operandWidth, integerOperand := resolvedBuiltin(snapshot, node.Type)
	if integerOperand && cType(operandWidth) != "" && !isUint(snapshot, node.Type) {
		return buildExpr(st, unit, snapshot, fileSet, id, locals, operandWidth, width)
	}
	if isUint(snapshot, node.Type) {
		return buildUintExpr(st, unit, snapshot, fileSet, id, locals, width)
	}
	return buildExpr(st, unit, snapshot, fileSet, id, locals, width, width)
}

// buildStrOperand builds one str value in a position that accepts a str
// expression, which is exactly three shapes (each confirmed against a real
// fixture): a SymbolValue naming an in-scope str-typed local (emitted as its
// pebble_local_<symbolID> C name — a PebbleStr lvalue), a StringLiteral (a
// str value with no local behind it, emitted as a PebbleStr compound literal
// carrying the escaped bytes and their compile-time length, the same
// construction a str-typed local's declaration embeds), or — since 10.36 — a
// DirectCall to a str-returning helper (emitted as
// pebble_fn_<calleeSymbolID>(ctx, <args>) by buildDirectCall, the same
// call-building machinery buildExpr's DirectCall case uses), so a str-returning
// helper's result can be compared directly (g() == "hi") or passed to a str
// parameter (f(g())) without an intermediate local. width is the entry's
// resolved integer width, threaded through to buildDirectCall so a call's
// arguments are built at the width the callee's other parameters expect.
// Anything else — a reference to a non-str local, any other node — is a clean
// rejection, never a guessed lowering. The function is shared by the three
// positions a str value is built: a ==/!= comparison operand (buildComparison),
// a call-site argument for a str parameter (buildCallArguments), and a
// str-returning helper's tail-position return value (buildBlock /
// buildSwitchCaseBody dispatch on resultInfo.isStr).
func buildStrOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	switch node.Kind {
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared {
			if ginfo, isGlobal := st.globals[node.Symbol]; isGlobal {
				if !ginfo.info.isStr {
					return "", fmt.Errorf("entry function body expression references global symbol %d, which is not a str-typed global", node.Symbol)
				}
				return fmt.Sprintf("pebble_global_%d", node.Symbol), nil
			}
			if einfo, isExtern := st.externData[node.Symbol]; isExtern {
				if !einfo.info.isStr {
					return "", fmt.Errorf("entry function body expression references extern variable symbol %d, which is not a str-typed extern variable", node.Symbol)
				}
				return einfo.name, nil
			}
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a str-typed local declared earlier in the body", node.Symbol)
		}
		if !info.isStr {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a str-typed local declared earlier in the body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.StringLiteral:
		valueText, err := buildStrLiteralValue(node)
		if err != nil {
			return "", err
		}
		return "(PebbleStr)" + valueText, nil
	case tir.DirectCall, tir.MethodCall:
		// A call to a str-returning helper used directly as a str value. The
		// DirectCall's own Type is the callee's resolved result type, which
		// the reachability walk has already validated as str for a reachable
		// helper (the check here is defense for hand-built IR); the call is
		// built by the same buildDirectCall machinery a scalar-width call
		// uses, so context and argument handling are identical.
		if !isStr(snapshot, node.Type) {
			return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose result type is %s, want str", node.Symbol, describeType(snapshot, node.Type))
		}
		return buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
	case tir.Load:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if !isStr(snapshot, node.Type) {
			return "", fmt.Errorf("entry function body expression contains a Load of type %s, want str", describeType(snapshot, node.Type))
		}
		if place.Kind == tir.CheckedIndexPlace {
			return buildArrayPlaceRead(st, unit, snapshot, fileSet, place, locals, width, false)
		}
		if place.Kind != tir.FieldPlace {
			return "", fmt.Errorf("entry function body expression contains a str Load whose place is a %s, want a FieldPlace (a str-typed struct field read)", place.Kind)
		}
		return buildStructFieldRead(st, unit, snapshot, fileSet, place, locals, width, false)
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a str-typed local reference, a string literal, or a call to a str-returning function", node.Kind)
	}
}

// buildCharOperand builds one char value in a position that accepts a char
// expression, which is exactly four shapes (each confirmed against a real
// fixture): a CharLiteral (a char value with no local behind it, emitted as an
// int32_t decimal literal), a SymbolValue naming an in-scope char-typed local
// (emitted as its pebble_local_<symbolID> C name — an int32_t lvalue), a
// DirectCall to a char-returning helper (emitted as
// pebble_fn_<calleeSymbolID>(ctx, <args>) by buildDirectCall, the same
// call-building machinery buildExpr's DirectCall case uses), so a
// char-returning helper's result can be compared directly (g() == 'a') or
// passed to a char parameter (f(g())) without an intermediate local, and —
// since 10.42 — a tir.CheckedIndex, str indexing s[i], whose Children are
// [base, index]: the base is a str value built by buildStrOperand and the
// read is emitted as the runtime's UTF-8 decoder
// pebble_rt_str_char_at_<suffix>(<base>, <index>). width is
// the entry's resolved integer width, threaded through to buildDirectCall so a
// call's arguments are built at the width the callee's other parameters
// expect. Anything else — a reference to a non-char local, any other node — is
// a clean rejection, never a guessed lowering. The function is shared by the
// six positions a char value is built: a comparison operand (buildComparison),
// a char-typed local's declaration initializer (buildScalarInitializeCore), a
// char-typed local's reassignment new value (buildStoreCore), a call-site
// argument for a char parameter (buildCallArguments), and a char-returning
// helper's tail-position return value (buildBlock / buildSwitchCaseBody
// dispatch on resultInfo.isChar).
func buildCharOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	switch node.Kind {
	case tir.CharLiteral:
		valueText, err := buildCharLiteralValue(node)
		if err != nil {
			return "", err
		}
		return valueText, nil
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared {
			if ginfo, isGlobal := st.globals[node.Symbol]; isGlobal {
				if !ginfo.info.isChar {
					return "", fmt.Errorf("entry function body expression references global symbol %d, which is not a char-typed global", node.Symbol)
				}
				return fmt.Sprintf("pebble_global_%d", node.Symbol), nil
			}
			if einfo, isExtern := st.externData[node.Symbol]; isExtern {
				if !einfo.info.isChar {
					return "", fmt.Errorf("entry function body expression references extern variable symbol %d, which is not a char-typed extern variable", node.Symbol)
				}
				return einfo.name, nil
			}
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a char-typed local declared earlier in the body", node.Symbol)
		}
		if !info.isChar {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a char-typed local declared earlier in the body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.DirectCall, tir.MethodCall:
		// A call to a char-returning helper used directly as a char value. The
		// DirectCall's own Type is the callee's resolved result type, which
		// the reachability walk has already validated as char for a reachable
		// helper (the check here is defense for hand-built IR); the call is
		// built by the same buildDirectCall machinery a scalar-width call
		// uses, so context and argument handling are identical.
		if !isChar(snapshot, node.Type) {
			return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose result type is %s, want char", node.Symbol, describeType(snapshot, node.Type))
		}
		return buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
	case tir.IndirectCall:
		// A call through a function-typed value whose result is char
		// (confirmed checker-reachable: `let c char = f('a');` lowers the
		// initializer to a char-typed IndirectCall). The call itself is built
		// by the same buildIndirectCall machinery a scalar-width indirect call
		// uses — the callee and every argument are built under the callee's
		// own function type — and the result is an int32_t, the same C type a
		// char value uses everywhere.
		if !isChar(snapshot, node.Type) {
			return "", fmt.Errorf("entry function body expression contains an indirect call whose result type is %s, want char", describeType(snapshot, node.Type))
		}
		return buildIndirectCall(st, unit, snapshot, fileSet, node, locals, width)
	case tir.Load:
		// A char-typed element read of a char-element slice (`let c char =
		// s[0];`) or a char tuple-element read (`let c char = t.0;`). A slice
		// read is lowered by the checker to Load(CheckedIndexPlace), exactly
		// as an integer or bool element read does, and emitted by the same
		// buildArrayPlaceRead machinery an integer slice element read uses —
		// .data[pebble_rt_checked_index_i32/_i64(...)] at the ENTRY width —
		// whose resulting C type int32_t is the char value's C type
		// everywhere. A tuple-element read is lowered to Load(TuplePlace), the
		// same shape buildExpr's int Load case accepts, and resolved the same
		// way via buildPlaceLValue — the tuple element's own type comes back
		// from the place resolution and must be char (the Load's Type is
		// already gated to char by the caller, but the element check here is
		// defense for hand-built IR, mirroring buildTuplePlaceRead's own
		// validation), and the emitted C is pebble_local_<symbol>._<ordinal>.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if place.Kind == tir.TuplePlace {
			lvalue, elemType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
			if err != nil {
				return "", err
			}
			if !isChar(snapshot, elemType) {
				return "", fmt.Errorf("entry function body expression contains a char Load whose tuple element has type %s, want char", describeType(snapshot, elemType))
			}
			return lvalue, nil
		}
		if place.Kind == tir.FieldPlace {
			// A char-typed narrowed union-variant payload read (`c.value`
			// inside `case .value:` where the variant's payload type is char):
			// the Load's place is a FieldPlace whose base is the union local
			// and whose member is the variant, resolved by buildStructFieldRead
			// to the union typedef's .payload.pebble_field_<member> projection —
			// the same projection every other narrowed payload read uses. The
			// projection's C type is the member's declared C type (int32_t,
			// see unionMemberCType — the char C type everywhere), so the read
			// is a valid char value with no further coercion.
			return buildStructFieldRead(st, unit, snapshot, fileSet, place, locals, width, false)
		}
		if place.Kind != tir.CheckedIndexPlace {
			return "", fmt.Errorf("entry function body expression contains a char Load whose place is a %s, want a CheckedIndexPlace (a char-element slice read) or a TuplePlace (a char tuple-element read)", place.Kind)
		}
		return buildArrayPlaceRead(st, unit, snapshot, fileSet, place, locals, width, false)
	case tir.CheckedIndex:
		// String indexing s[i]. The checker produces a bare tir.CheckedIndex —
		// not Load(CheckedIndexPlace), the node array/slice indexing uses —
		// exactly when the indexed value has no addressable place: a str's
		// byte-level content is not addressable the way array/slice element
		// storage is, so str indexing is a pure decode-to-value operation
		// (confirmed against a real fixture: the node's Children are [base,
		// index] and its Type is the snapshot's char builtin). The base is a
		// str value built by buildStrOperand — a reference to an in-scope str
		// local, a bare string literal, or a call to a str-returning helper,
		// all three confirmed reachable against real fixtures ("hi"[0] and
		// g()[0] both lower to this exact shape) — and the index is built by
		// the same dispatch buildArrayPlaceRead uses: an int-typed
		// IntegerLiteral (a literal index is the unanchored int builtin even
		// in an i64 entry, confirmed against a real fixture) or int-typed
		// SymbolValue (a range loop's iterator used directly as the index, the
		// same unanchored-int case) lowered directly, anything else (a
		// width-typed local reference, checked arithmetic) via buildExpr. The
		// read is emitted as the runtime's UTF-8 decoder
		// pebble_rt_str_char_at_<suffix>(<base>, <index>): s[i] is a
		// Unicode-scalar-value index, not a byte offset, so the runtime walks
		// and decodes the variable-width UTF-8 byte sequence from the start,
		// panicking on a negative or out-of-range index or on malformed UTF-8
		// (pebble_rt.h declares _i32 and _i64 variants; the index parameter's
		// width varies by the entry's, the int32_t result does not — a char
		// always fits in 32 bits, so the width-selected helper returns a char
		// either way).
		//
		// Since the slice-index slice, a CheckedIndex whose base is NOT a str
		// — a slice-element read of a value with no addressable place, the
		// same char result (`view()[1]` where view returns []char) — is also
		// accepted, built by the shared buildSliceIndexValue. This char
		// builder returns only an expression (no leading-statement slot), so a
		// pure-projection base (a slice-typed local or place, which needs no
		// temp) is emitted directly; a freshly-computed base (a call result,
		// which needs a temp-declaration statement) is a clean rejection here —
		// the positions that CAN host a temp (a print, a local declaration, a
		// return) intercept the shape before reaching this case and thread
		// buildSliceIndexValue's pre. A CheckedIndex whose base does not
		// resolve to a str or a slice type is a clean rejection naming what
		// was found, never a guessed lowering.
		strBase, err := checkedIndexBaseIsStr(unit, snapshot, node)
		if err != nil {
			return "", err
		}
		if !strBase {
			pre, read, err := buildSliceIndexValue(st, unit, snapshot, fileSet, id, node, locals, width, false)
			if err != nil {
				return "", err
			}
			if pre != "" {
				base, ok := unit.Node(node.Children[0])
				if !ok {
					return "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", node.Children[0])
				}
				return "", fmt.Errorf("entry function body expression indexes a %s of type %s in a pure-expression position with nowhere to place the temp-declaration statement the freshly-computed slice value needs; bind the slice into a local first", base.Kind, describeType(snapshot, base.Type))
			}
			return read, nil
		}
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a CheckedIndex with %d child(ren), want exactly two (the str value being indexed and the index)", len(node.Children))
		}
		base, err := buildStrOperand(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		indexNode, ok := unit.Node(node.Children[1])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid index node %d", node.Children[1])
		}
		var index string
		if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
			if !isNonNegativeDecimal(indexNode.Literal.IntegerNum) {
				return "", fmt.Errorf("str index contains an integer literal with malformed text %q", indexNode.Literal.IntegerNum)
			}
			index = indexNode.Literal.IntegerNum
		} else if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
			// An int-typed SymbolValue index is a range loop's iterator
			// referenced directly (the same unanchored-int case
			// buildComparisonOperand and buildArrayPlaceRead handle), and the
			// iterator is always declared in C at the entry's width, so its
			// name is the correct C lvalue for the index.
			if name, ok := localOrGlobalName(st, indexNode.Symbol, locals); ok {
				index = name
			} else {
				return "", fmt.Errorf("str index references symbol %d, which is not a local in scope", indexNode.Symbol)
			}
		} else if isUint(snapshot, indexNode.Type) {
			// A uint-typed str index (a uint-typed local or loop iterator):
			// built by the dedicated uint grammar, mirroring the slice/array
			// index dispatch.
			index, err = buildUintExpr(st, unit, snapshot, fileSet, node.Children[1], locals, width)
			if err != nil {
				return "", fmt.Errorf("str index: %v", err)
			}
		} else {
			index, err = buildExpr(st, unit, snapshot, fileSet, node.Children[1], locals, width, width)
			if err != nil {
				return "", fmt.Errorf("str index: %v", err)
			}
		}
		return "pebble_rt_str_char_at_" + checkedSuffix(width) + "(" + base + ", " + index + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a char literal, a reference to a char-typed local declared earlier in the body, a call to a char-returning function, or a str index", node.Kind)
	}
}

// buildExpr builds the C expression text for an integer value node of the
// entry's resolved width, recursing into its operands. width (types.Int,
// types.I32, or types.I64) is the width resolved once in Emit; every node in an accepted
// tree must carry exactly that width's builtin — a node carrying the other
// width (an i32 local referenced inside an i64 entry, or vice versa) is a
// clean width-mismatch rejection, never a coercion. locals is the set of
// symbols in scope at this point in the
// entry body (a map is deliberately used, not a slice, so membership is a
// constant-time check); it is read-only for a SymbolValue reference and is
// otherwise threaded through unchanged. In addition to the scalar and call
// forms below, it accepts plain bitwise &, |, ^, and ~ expressions, which are
// safe to emit directly as C operators:
//
//   - IntegerLiteral — its decimal text (defensively validated, exactly as
//     10.3 validated a bare literal return), given a "u" suffix when the
//     literal's width is unsigned so a large value is an unsigned C constant.
//   - CheckedNegate with exactly one operand of the entry's width —
//     pebble_rt_checked_neg_<suffix>.
//   - CheckedArithmetic with exactly two operands of the entry's width and
//     operator +, -, *, /, or % — pebble_rt_checked_add_<suffix> /
//     pebble_rt_checked_sub_<suffix> / pebble_rt_checked_mul_<suffix> /
//     pebble_rt_checked_div_<suffix> / pebble_rt_checked_mod_<suffix>.
//   - BinaryValue with exactly two operands of the entry's width and operator
//     &, |, or ^ — the parenthesized plain C operator expression.
//   - PrefixValue with exactly one operand of the entry's width and operator
//     ~ — the parenthesized plain C bitwise-not expression.
//   - SymbolValue whose Symbol is in locals — pebble_local_<symbol ID>, the C
//     name buildBlock gave that local's declaration.
//   - DirectCall — a call to another Pebble-convention function whose result
//     is the entry's width (validated by the reachability walk in
//     discoverReachableHelpers). Each call-site argument is built by the
//     grammar its callee parameter resolves to — the entry's width for an
//     integer parameter (this builder), bool for a bool parameter
//     (buildBoolExpr) — so the call emits pebble_fn_<calleeSymbolID>(ctx,
//     <arg0>, <arg1>, ...), with the ctx argument prepended by this backend
//     since the typed IR threads context via ContextAction rather than as an
//     explicit child.
//
// CheckedArithmetic with any other operator (the integral operators that build
// this node but are not yet lowered) is rejected, not guessed. BinaryValue or
// PrefixValue with any other operator, including shifts, is also rejected. A SymbolValue
// referencing anything not in locals (a global, a symbol from an
// outer/different scope — none of which are reachable from this narrow body
// shape, but checked defensively rather than assumed) is a clean rejection.
// Any other node kind at any position — a non-integer
// operand, CheckedShift, and so on — is a clean rejection naming what was
// found.
// Emitting the checked runtime helpers (rather than raw C operators) is what
// keeps the IR nodes' real overflow and divide-by-zero semantics from silently
// disappearing in the emitted program.
//
// width is the width the expression tree is being built AT — the entry's own
// width for a top-level expression, but a value's own resolved width (u8, i64,
// ...) when buildScalarInitializeCore, buildCallArgument, buildStoreCore, or
// an IntegerCast child re-anchors the grammar. entryWidth is the width of the
// function being emitted, threaded unchanged through every recursive call so a
// width-suffixed runtime call buried in the tree (a slice/array index's
// pebble_rt_checked_index_i32/_i64, whose helper exists only at i32/i64) can
// always pick the entry's helper rather than the element's possibly-empty
// checkedSuffix.
func buildExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, entryWidth types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	if node.Kind == tir.ContextValue || runtimeType(unit, snapshot, node.Type) != 0 {
		if node.Kind == tir.ContextValue {
			return "(*ctx)", nil
		}
		if node.Kind == tir.SymbolValue {
			if node.Symbol == unit.Runtime().Context {
				return "(*ctx)", nil
			}
			if _, declared := locals[node.Symbol]; !declared {
				return "", fmt.Errorf("runtime symbol %d is not a local", node.Symbol)
			}
			return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		}
	}
	if node.Kind == tir.IndirectCall {
		return buildIndirectCall(st, unit, snapshot, fileSet, node, locals, width)
	}
	if node.Kind == tir.SliceFromRaw {
		return buildRawSliceConstruction(st, unit, snapshot, fileSet, node, locals, width, "entry function body expression")
	}
	// A pointer-typed node's Type is never the entry's width, so it must
	// bypass the width gate below. This covers every shape a pointer value
	// can take: freshly constructed (AddressOf, NilPointer), a reference to
	// an existing pointer-typed local (SymbolValue), or the result of a
	// pointer-returning helper call (DirectCall) — not just the construction
	// forms, since a pointer local is very commonly read back by name rather
	// than always rebuilt at each use site.
	if isPointer(snapshot, node.Type) {
		switch node.Kind {
		case tir.AddressOf:
			if len(node.Children) != 1 {
				return "", fmt.Errorf("entry function body expression contains an AddressOf with %d children, want exactly one", len(node.Children))
			}
			placeLValue, _, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, entryWidth)
			if err != nil {
				return "", fmt.Errorf("entry function body address-of place: %v", err)
			}
			pointeeTypeID, ok := pointerPointeeType(snapshot, node.Type)
			if !ok {
				return "", fmt.Errorf("entry function body expression contains an AddressOf with unsupported pointer type %s", describeType(snapshot, node.Type))
			}
			return "(" + pointerTypeNameForUnit(st, unit, snapshot, pointeeTypeID) + ")(&" + placeLValue + ")", nil
		case tir.NilPointer:
			pointeeTypeID, ok := pointerPointeeType(snapshot, node.Type)
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a NilPointer with unsupported pointer type %s", describeType(snapshot, node.Type))
			}
			return "(" + pointerTypeNameForUnit(st, unit, snapshot, pointeeTypeID) + ")(NULL)", nil
		case tir.SymbolValue:
			if _, declared := locals[node.Symbol]; !declared {
				return "", fmt.Errorf("entry function body expression references symbol %d, which is not a local declared earlier in the entry body", node.Symbol)
			}
			return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		case tir.Load:
			if len(node.Children) == 1 {
				place, ok := unit.Node(node.Children[0])
				if ok && place.Kind == tir.FieldPlace {
					return buildStructFieldRead(st, unit, snapshot, fileSet, place, locals, width, false)
				}
			}
			return "", fmt.Errorf("entry function body expression contains an unsupported pointer Load")
		case tir.DirectCall:
			return buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
		case tir.CheckedOptionalUnwrap:
			// A force-unwrap of an optional whose payload is a pointer
			// (`let p *i32 = o!;` or an inline `*(o!)`): the unwrap is only
			// the has_value check via the runtime helper (no overflow/width
			// concerns for a pointer payload), selected from the PAYLOAD's own
			// type by optionalUnwrapSuffix ("ptr" here). The helper's void *
			// result converts implicitly to any object pointer type in C.
			unwrapSuffix := optionalUnwrapSuffix(snapshot, node.Type)
			if unwrapSuffix == "" {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of a %s payload, which has no runtime unwrap helper", describeType(snapshot, node.Type))
			}
			if len(node.Children) != 1 {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
			}
			child, ok := unit.Node(node.Children[0])
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
			}
			if child.Kind == tir.SourceAlias && len(child.Children) == 1 {
				// A grouped-expression alias around the unwrap's base —
				// `(*p)!` unwrapping a dereferenced pointer-payload optional —
				// is transparent: unwrap the alias and process the base
				// exactly as if the parens were absent.
				child, ok = unit.Node(child.Children[0])
				if !ok {
					return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid alias child node %d", child.Children[0])
				}
			}
			if child.Kind == tir.Load && len(child.Children) == 1 {
				if _, ok := unit.Node(child.Children[0]); !ok {
					return "", fmt.Errorf("invalid optional place")
				}
				expr, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], locals, width)
				if err != nil {
					return "", err
				}
				if !isOptional(snapshot, typ) {
					return "", fmt.Errorf("optional unwrap base is not optional")
				}
				return fmt.Sprintf("pebble_rt_checked_unwrap_%s(%s.has_value, %s.value, %s)", unwrapSuffix, expr, expr, buildSourceLoc(fileSet, node.Span)), nil
			}
			if child.Kind != tir.SymbolValue {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
			}
			if info, declared := locals[child.Symbol]; !declared || info.optional == 0 {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
			}
			return fmt.Sprintf("pebble_rt_checked_unwrap_%s(pebble_local_%d.has_value, pebble_local_%d.value, %s)", unwrapSuffix, child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
		case tir.SourceAlias:
			// A SourceAlias is transparent — it records grouped-expression
			// parens — so a pointer-typed one unwraps to its single child,
			// exactly as the general-value SourceAlias case below does.
			if len(node.Children) != 1 {
				return "", fmt.Errorf("entry function body expression contains a pointer-typed SourceAlias with %d child(ren), want exactly one", len(node.Children))
			}
			return buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		case tir.PointerCast:
			if len(node.Children) != 1 {
				return "", fmt.Errorf("entry function body expression contains a PointerCast with %d children, want exactly one", len(node.Children))
			}
			child, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
			if err != nil {
				return "", fmt.Errorf("entry function body pointer cast child: %v", err)
			}
			pointeeTypeID, ok := pointerPointeeType(snapshot, node.Type)
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a PointerCast with unsupported pointer type %s", describeType(snapshot, node.Type))
			}
			return "(" + pointerTypeNameForUnit(st, unit, snapshot, pointeeTypeID) + ")(" + child + ")", nil
		default:
			return "", fmt.Errorf("entry function body expression contains a %s of pointer type %s, which this backend does not lower", node.Kind, describeType(snapshot, node.Type))
		}
	}
	// An array-typed or slice-typed node's Type is never the entry's width, so
	// it must bypass the width gate below the same way a pointer-typed node
	// does. The one array/slice value this backend lowers in a pure value
	// position is a force-unwrap of an array/slice-payload optional (`o!`
	// where o is `?[N]T` or `?[]T`): the aggregate payload has no by-value
	// scalar to return through the runtime's pebble_rt_checked_unwrap_<suffix>
	// helpers, so the unwrap lowers to a GNU statement-expression whose first
	// statement is the presence-only check (pebble_rt_checked_unwrap_present,
	// which panics on an absent optional) and whose value is the optional's
	// own .value field — the payload's pebble_array_<typeID>_t or
	// pebble_slice_<typeID>_t struct read by value. The base is a SymbolValue
	// naming an optional-typed local or a Load of an optional-typed place; a
	// freshly-computed base (a call result) would need a temp-declaration
	// statement this pure-expression position has nowhere to place, so it is a
	// clean rejection.
	if node.Kind == tir.CheckedOptionalUnwrap && (isArray(snapshot, node.Type) || isSlice(snapshot, node.Type)) {
		unwrapExpr, err := buildOptionalAggregateUnwrapExpr(st, unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", err
		}
		return unwrapExpr, nil
	}
	// A function-typed node's Type is never the entry's width, so it must
	// bypass the width gate below the same way a pointer-typed node does. This
	// covers the shapes a function value can take: a reference to an
	// existing function-typed local or parameter (SymbolValue), a bare
	// top-level function reference (HoistedFunctionValue), a generic function
	// referenced as a value (GenericFunctionValue), a call to a
	// function-returning helper (DirectCall), a function-typed struct field
	// read (FieldValue/Load), and (since function-types slice 2) a bare
	// function reference used as a struct field's construction value — all
	// built by buildFunctionValue. (The
	// general indirect call through such a value is handled by buildIndirectCall
	// at the top of this function, whose callee child is a function-typed node
	// routed through this same bypass.)
	if isFunctionType(snapshot, node.Type) {
		return buildFunctionValue(st, unit, snapshot, fileSet, node, locals, "entry function body expression", width)
	}
	// A node carrying the abstract `int` builtin (types.Int) is accepted at any
	// integer width. The checker deliberately leaves a value at this unanchored
	// type when nothing pins it to a fixed width — an untyped integer literal,
	// a range-loop iterator never used in a width-anchoring position, or a
	// `let`-declared global constant with an untyped literal initializer — and
	// this backend already treats such nodes as width-compatible in the narrow
	// positions buildRangeBound/buildComparisonOperand/buildCharOperand
	// special-case. `int` shares i32's C type (int32_t), so an `int`-typed node
	// is emitted at whatever integer width the surrounding position requests
	// (a literal as its decimal text, a SymbolValue as its already-declared C
	// name) with no cast needed; the gate below still rejects a genuinely
	// mismatched fixed-width node (an i64 value inside an i32 context), which
	// is the mismatch it exists to catch. Symmetrically, isCompatibleIntegerWidth
	// accepts a node carrying ANY concrete fixed-width integer builtin whose C
	// representation matches the requested width (an i32-typed value inside an
	// `int` context — the shape a generic specialization produces), for the same
	// reason: the two share a C type, so the value is emitted as-is with no
	// cast.
	if node.Kind != tir.CheckedIntegerToEnum && node.Kind != tir.OptionalIntegerToEnum && !isWidth(snapshot, width, node.Type) && !(isAbstractInt(snapshot, node.Type) && cType(width) != "") && !isCompatibleIntegerWidth(snapshot, width, node.Type) && !isTypeParameterType(snapshot, node.Type) {
		wantName, _ := builtinName(width)
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want %s", node.Kind, describeType(snapshot, node.Type), wantName)
	}
	switch node.Kind {
	case tir.IntegerLiteral:
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("entry function body expression contains an integer literal with malformed text %q", text)
		}
		return integerLiteralText(text, width), nil
	case tir.SizeofType:
		// A bare `sizeof T` used as the operand of an integer cast
		// (`(sizeof int) as int`). `sizeof T` is itself a uint-typed
		// expression, so the whole value is delegated to buildUintExpr,
		// whose SizeofType case resolves the C type name from the node's
		// own TypeArg (sizeofCTypeName) and emits `sizeof(<type>)` — the
		// exact same C a plain, uncast `sizeof T` used directly as a
		// uint-typed value produces. That branch ignores the width
		// parameter entirely (it sizes the type by its own C storage
		// type, never the surrounding context's width), so delegating
		// with this call's own width is safe.
		return buildUintExpr(st, unit, snapshot, fileSet, id, locals, width)
	case tir.IntegerCast:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains an IntegerCast with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an IntegerCast with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || cType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains an IntegerCast with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an IntegerCast referencing invalid child node %d", node.Children[0])
		}
		childType, ok := snapshot.Key(child.Type)
		if !ok {
			return "", fmt.Errorf("entry function body IntegerCast child has invalid type %d", child.Type)
		}
		childWidth, ok := childType.Builtin()
		if !ok || cType(childWidth) == "" {
			return "", fmt.Errorf("entry function body IntegerCast child has non-integer type %s", describeType(snapshot, child.Type))
		}
		childExpr, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, childWidth, entryWidth)
		if err != nil {
			return "", fmt.Errorf("entry function body integer cast child: %v", err)
		}
		return "(" + cType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.EnumToInteger:
		// An enum value cast to an integer (`Color.green as i32`), lowered as a
		// plain, unchecked C cast of the enum value's expression to the
		// destination integer type. An enum value once constructed is always a
		// valid member of its enum's declared variant set — no well-typed Pebble
		// program can observe an "invalid" enum value, unlike the reverse
		// integer-to-enum direction, which needs a runtime validity check — so
		// reading out the enum's underlying integer representation is always
		// well-defined and needs no runtime helper. The destination width is
		// resolved from the node's own Type exactly as IntegerCast resolves its
		// own (and the width gate above has already required it to be the
		// surrounding context's width); the single child is the enum value being
		// cast, built by buildEnumValue (an enum-typed local reference, a
		// variant literal, or a zero-payload variant construction), and the
		// emitted C is `(<destination C type>)(<enum value expression>)`. A
		// C enum's value IS the variant's ordinal in declared order and casts to
		// an integer type directly and trivially, so no intermediate step
		// through the enum's own typedef is needed. The reverse direction,
		// CheckedIntegerToEnum, is implemented in this file's sibling case (and
		// via buildCheckedIntegerToEnumExpr); only the optional-destination form,
		// OptionalIntegerToEnum, remains out of scope.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains an EnumToInteger with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an EnumToInteger with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || cType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains an EnumToInteger with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		childExpr, err := buildEnumValue(st, unit, snapshot, fileSet, node.Children[0], locals, entryWidth)
		if err != nil {
			return "", err
		}
		return "(" + cType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.CharToInteger:
		// A char value cast to an integer (`c as u32`), lowered as a plain,
		// unchecked C cast of the char value's expression to the destination
		// integer type. A char is a Unicode scalar value, and every valid
		// codepoint (max 0x10FFFF) fits identically in every integer width
		// Pebble has, so no well-typed Pebble program can construct a char
		// value whose codepoint overflows the destination width — reading out
		// the char's codepoint as an integer is always well-defined and needs
		// no runtime helper, exactly as enum-to-integer (EnumToInteger) casts
		// need none. The destination width is resolved from the node's own Type
		// exactly as IntegerCast resolves its own (and the width gate above has
		// already required it to be the surrounding context's width); the single
		// child is the char value being cast, built by buildCharOperand (a char
		// literal, a char-typed local reference, a char-returning call, or a
		// char element read), and the emitted C is `(<destination C type>)
		// (<char value expression>)`. The reverse direction, integer-to-char,
		// is out of scope (an arbitrary integer is not necessarily a valid
		// Unicode scalar, so that direction needs a validity-checked design of
		// its own).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CharToInteger with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CharToInteger with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || cType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains a CharToInteger with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		childExpr, err := buildCharOperand(st, unit, snapshot, fileSet, node.Children[0], locals, entryWidth)
		if err != nil {
			return "", err
		}
		return "(" + cType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.PointerToInteger:
		// A pointer value cast to an integer (`ptr as u64`), lowered as a
		// plain, unchecked C cast of the pointer value's expression to the
		// destination integer type. This mirrors EnumToInteger/CharToInteger
		// exactly: the reverse direction, integer-to-pointer, stays forbidden
		// (compatibleForbidden) by the checker, so this node can only ever
		// represent a well-typed pointer whose bit pattern is being read out
		// as an integer address — always well-defined and needing no runtime
		// helper. The destination width is resolved from the node's own Type;
		// the single child is the pointer value being cast, built by buildExpr
		// (which routes any pointer-typed child through its pointer branch),
		// and the emitted C is `(<destination C type>)(<pointer expression>)`.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a PointerToInteger with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a PointerToInteger with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || cType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains a PointerToInteger with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		childExpr, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return "(" + cType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.CheckedIntegerToEnum:
		// An integer cast to an enum (`5 as Color`), lowered through the
		// single canonical-width runtime primitive
		// pebble_rt_checked_int_to_enum. The destination enum type is the
		// node's own Type (an enum-typed value, exactly as the surrounding
		// width gate bypasses it — the enum check precedes the width gate by
		// design); its variant count is the destination enum's TypeDecl.Members
		// length (variant Members[i] gets the C enum value i, so an integer
		// names a real variant exactly when 0 <= value < variant_count). The
		// single child is the ordinary integer being cast, built by the
		// integer expression builder at its own declared width — not
		// buildEnumValue, which is for enum-typed operands. The emitted C is
		// `(<destination enum C type>)pebble_rt_checked_int_to_enum((int64_t)
		// (<child expr>), <variant_count>, <source loc>)`: the source is
		// widened to the primitive's int64_t input (sign-extending a negative
		// signed source, zero-extending any unsigned source up to 63 bits, and
		// bit-reinterpreting a u64 >= 2^63 as negative — all recovered
		// correctly by the primitive's single unsigned bounds comparison), and
		// the primitive's result is narrowed back to the enum type. SAFE mode
		// panics out-of-range; RELEASE returns the value unchanged (unchecked,
		// trusting the input). The reverse direction, OptionalIntegerToEnum
		// (`5 as ?Color`), is handled in the sibling case below.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedIntegerToEnum with %d children, want exactly one", len(node.Children))
		}
		return buildCheckedIntegerToEnumExpr(st, unit, snapshot, fileSet, node, locals, "entry function body expression", entryWidth)
	case tir.OptionalIntegerToEnum:
		// An integer cast to an optional enum (`5 as ?Color`): the ONE
		// supported position is a local variable declaration's initializer
		// (see buildOptionalIntegerToEnumDeclaration), where the backend can
		// emit a pre-statement to evaluate the source integer exactly once
		// before building both the has_value bool and the enum value from it.
		// Every expression position reaches here only through a plain
		// `(string, error)` builder with no pre-statement threading, so there
		// is nowhere to hoist the source's single evaluation — embedding the
		// source twice would evaluate it twice, wrong for a side-effecting
		// source. Rather than emit a wrong double-evaluated C or reach for a
		// GNU statement-expression, the cast is cleanly rejected here naming
		// what was found.
		return "", fmt.Errorf("entry function body expression contains an integer-to-optional-enum cast to %s, which is only supported as a local variable declaration's initializer; a C expression position has nowhere to place the temp-declaration statement that evaluates the source integer exactly once, so the cast is not supported here", describeType(snapshot, node.Type))
	case tir.FloatToInteger:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a FloatToInteger with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a FloatToInteger with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		// The float-to-integer conversion helpers exist only for i32 and i64
		// destinations. A u64 destination is cleanly rejected here rather than
		// emitted as a call to a nonexistent pebble_rt_checked_f*_to_u64
		// (checkedSuffix admits u64 for the +, -, * arithmetic family this
		// slice adds, but the float-conversion family has no u64 twin yet).
		if !ok || checkedSuffix(destinationWidth) == "" || destinationWidth == types.U64 {
			return "", fmt.Errorf("entry function body expression contains a FloatToInteger with non-integer destination type %s", describeType(snapshot, node.Type))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a FloatToInteger referencing invalid child node %d", node.Children[0])
		}
		childWidth := resolvedFloatKind(snapshot, child.Type)
		if childWidth == 0 {
			return "", fmt.Errorf("entry function body FloatToInteger child has non-float type %s", describeType(snapshot, child.Type))
		}
		childExpr, err := buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], locals, childWidth, entryWidth)
		if err != nil {
			return "", fmt.Errorf("entry function body float-to-integer cast child: %v", err)
		}
		helper := "pebble_rt_checked_" + childFloatSuffix(childWidth) + "_to_" + checkedSuffix(destinationWidth)
		return helper + "(" + childExpr + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	case tir.CheckedNegate:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedNegate with %d operand(s), want exactly one", len(node.Children))
		}
		if node.Operator != syntax.Minus {
			return "", fmt.Errorf("entry function body expression contains a CheckedNegate with operator %s, want -", node.Operator)
		}
		child, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		if checkedSuffix(width) == "" {
			// No pebble_rt_checked_neg_* runtime helper exists for this
			// width (the runtime implements only the i32/i64/u64 family).
			// A literal operand that fits the width's own signed range is
			// folded to its negated decimal text — the same negative C
			// constant spelling a negative switch case label uses — so a
			// narrow-width negative literal initializer (e.g. `let x i16
			// = -5;`) emits valid C instead of a call to a nonexistent
			// helper. A non-constant operand is a clean rejection rather
			// than a malformed call.
			if folded, ok := checkedNegateLiteral(unit, node.Children[0], width); ok {
				return folded, nil
			}
			name, _ := builtinName(width)
			return "", fmt.Errorf("entry function body expression contains a CheckedNegate at %s, a width with no checked-neg runtime helper and a non-constant operand", name)
		}
		// A literal negation at a width WITH a checked-neg runtime helper is
		// usually emitted as pebble_rt_checked_neg_<suffix>(<magnitude>, ...),
		// where <magnitude> is the operand's own positive literal text. At the
		// width's exact signed minimum that positive magnitude is not a
		// spellable C constant of the width (INT32_MAX is 2147483647, so
		// 2147483648 is not a valid int32_t literal; 9223372036854775808
		// exceeds even long long), so cc fails the mandated -Wall -Wextra
		// -Werror build with -Wconstant-conversion (i32/int) or
		// -Wimplicitly-unsigned-literal (i64). That one literal is folded to
		// its negated value — the same checkedNegateLiteral folding the
		// no-helper width path above uses — emitting the width's minimum
		// constant directly instead of an unspellable magnitude. Every other
		// literal and every non-literal operand keeps the runtime-helper path
		// unchanged; unsigned widths are untouched (their minimum is 0, never
		// the result of a negative literal).
		if folded, ok := checkedNegateLiteral(unit, node.Children[0], width); ok && !isUnsignedWidth(width) {
			if min, _, rangeOK := integerKindRange(width); rangeOK && folded == integerLiteralText(min.String(), width) {
				return checkedNegateMinimumText(width), nil
			}
		}
		return "pebble_rt_checked_neg_" + checkedSuffix(width) + "(" + child + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	case tir.CheckedArithmetic:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a CheckedArithmetic with %d operand(s), want exactly two", len(node.Children))
		}
		helper, ok := checkedArithmeticHelper(node.Operator, width)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedArithmetic with operator %s at %s, want an operator with a checked runtime helper (int/i32/i64 support +, -, *, /, and %%; at u64, only +, -, and * have a checked runtime helper)", node.Operator, wantName(width))
		}
		left, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		right, err := buildExpr(st, unit, snapshot, fileSet, node.Children[1], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return helper + "(" + left + ", " + right + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	case tir.CheckedShift:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a CheckedShift with %d operand(s), want exactly two", len(node.Children))
		}
		helper, ok := checkedShiftHelper(node.Operator, width)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedShift with operator %s, want << or >>", node.Operator)
		}
		left, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		amountNode, ok := unit.Node(node.Children[1])
		if !ok {
			return "", fmt.Errorf("entry function body expression references invalid shift amount node %d", node.Children[1])
		}
		amountType, ok := snapshot.Key(amountNode.Type)
		if !ok {
			return "", fmt.Errorf("entry function body shift amount has invalid type %d", amountNode.Type)
		}
		amountWidth, ok := amountType.Builtin()
		if !ok || cType(amountWidth) == "" {
			return "", fmt.Errorf("entry function body shift amount has non-integer type %s", describeType(snapshot, amountNode.Type))
		}
		amount, err := buildExpr(st, unit, snapshot, fileSet, node.Children[1], locals, amountWidth, entryWidth)
		if err != nil {
			return "", err
		}
		if amountWidth != width {
			amount = "(" + cType(width) + ")(" + amount + ")"
		}
		return helper + "(" + left + ", " + amount + ", " + buildSourceLoc(fileSet, node.Span) + ")", nil
	case tir.BinaryValue:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a BinaryValue with %d operand(s), want exactly two", len(node.Children))
		}
		op, ok := bitwiseOperator(node.Operator)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a BinaryValue with operator %s, want &, |, or ^", node.Operator)
		}
		left, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		right, err := buildExpr(st, unit, snapshot, fileSet, node.Children[1], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return "(" + left + " " + op + " " + right + ")", nil
	case tir.PrefixValue:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with %d operand(s), want exactly one", len(node.Children))
		}
		if node.Operator != syntax.Tilde {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with operator %s, want ~", node.Operator)
		}
		child, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return "~(" + child + ")", nil
	case tir.SymbolValue:
		if name, ok := localOrGlobalName(st, node.Symbol, locals); ok {
			return name, nil
		}
		return "", fmt.Errorf("entry function body expression references symbol %d, which is not a local declared earlier in the entry body", node.Symbol)
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional-typed local (x!). The child is a
		// SymbolValue naming the optional local, and this node's Type is the
		// unwrapped result type (the payload type, already gated above to an
		// integer builtin this grammar accepts). The unwrap is bounds-checked
		// via the runtime helper — selected from the PAYLOAD's own type (see
		// optionalUnwrapSuffix), so a uint/u64 payload routes to
		// pebble_rt_checked_unwrap_u64 rather than the entry-width helper —
		// passing the optional local's has_value and value fields.
		unwrapSuffix := optionalUnwrapSuffix(snapshot, node.Type)
		if unwrapSuffix == "" {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of a %s payload, which has no runtime unwrap helper", describeType(snapshot, node.Type))
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind == tir.SourceAlias && len(child.Children) == 1 {
			// A grouped-expression alias around the unwrap's base — `(*p)!`
			// unwraps the dereferenced optional `(*p)` — is transparent: the
			// base is the alias's single child, so the alias is unwrapped and
			// the base processed exactly as if the parens were absent.
			child, ok = unit.Node(child.Children[0])
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid alias child node %d", child.Children[0])
			}
		}
		if child.Kind == tir.Load && len(child.Children) == 1 {
			expr, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], locals, width)
			if err != nil {
				return "", err
			}
			if !isOptional(snapshot, typ) {
				return "", fmt.Errorf("optional unwrap base is not optional")
			}
			return fmt.Sprintf("pebble_rt_checked_unwrap_%s(%s.has_value, %s.value, %s)", unwrapSuffix, expr, expr, buildSourceLoc(fileSet, node.Span)), nil
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		info, declared := locals[child.Symbol]
		if !declared {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not a local declared earlier in the entry body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		return fmt.Sprintf("pebble_rt_checked_unwrap_%s(pebble_local_%d.has_value, pebble_local_%d.value, %s)", unwrapSuffix, child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
	case tir.Load:
		// A tuple element or struct field read. Reading one element of a
		// tuple-typed local (`t.1`) is lowered by the checker to a Load of a
		// TuplePlace whose single child is the StoragePlace naming the tuple
		// local, and reading one field of a struct-typed local (`point.x`) to
		// a Load of a FieldPlace whose single child is the StoragePlace naming
		// the struct local (both confirmed against real fixtures); these are
		// the only shapes real source produces for reading an element/field of
		// a compound local (a plain local read is a SymbolValue, not a Load).
		// The Load's Type is the element/field's own type, already gated to
		// the entry's width above, so the element/field must resolve to the
		// entry's width here. The emitted C is
		// pebble_local_<symbol>._<ordinal> for a tuple and
		// pebble_local_<symbol>.pebble_field_<member> for a struct.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if place.Kind != tir.TuplePlace {
			if place.Kind == tir.CheckedIndexPlace {
				return buildArrayPlaceRead(st, unit, snapshot, fileSet, place, locals, entryWidth, false)
			}
			if place.Kind == tir.FieldPlace {
				return buildStructFieldRead(st, unit, snapshot, fileSet, place, locals, width, false)
			}
			if place.Kind == tir.DereferencePlace {
				return buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, locals, width, node.Span, false)
			}
			return "", fmt.Errorf("entry function body expression contains a Load whose place is a %s, want a TuplePlace, CheckedIndexPlace, FieldPlace, or DereferencePlace", place.Kind)
		}
		return buildTuplePlaceRead(st, unit, snapshot, fileSet, place, locals, width, false)
	case tir.TupleElementValue:
		// The checker produces a TupleElementValue only when a tuple literal is
		// indexed directly — (1, 2).1 — whose child is the TupleValue being
		// indexed and whose element type comes out as the unanchored `int`
		// builtin (confirmed against a real fixture); that shape is out of
		// scope, and its int-typed element fails the width gate above before
		// reaching this case. The only in-scope element read of a tuple local
		// is Load(TuplePlace). This case is therefore defense for hand-built
		// IR matching the local-read shape: a TupleElementValue whose single
		// child is a SymbolValue naming a tuple-typed local is emitted exactly
		// like the Load(TuplePlace) read, and any other base is a clean
		// rejection.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue with %d child(ren), want exactly one (the tuple value being indexed)", len(node.Children))
		}
		base, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue referencing invalid node %d", node.Children[0])
		}
		if base.Kind == tir.Load && len(base.Children) == 1 {
			place, ok := unit.Node(base.Children[0])
			if !ok || place.Kind != tir.TuplePlace {
				return "", fmt.Errorf("tuple element base is not a tuple place")
			}
			return buildTuplePlaceRead(st, unit, snapshot, fileSet, place, locals, width, false)
		}
		if base.Kind == tir.SourceAlias && len(base.Children) == 1 {
			inner, ok := unit.Node(base.Children[0])
			if ok && inner.Kind == tir.Load && len(inner.Children) == 1 {
				place, ok := unit.Node(inner.Children[0])
				if ok && place.Kind == tir.TuplePlace && len(place.Children) == 1 {
					baseExpr, _, err := buildPlaceLValue(st, unit, snapshot, fileSet, place.Children[0], locals, width)
					if err != nil {
						return "", err
					}
					return fmt.Sprintf("%s._%d._%d", baseExpr, place.Ordinal, node.Ordinal), nil
				}
			}
		}
		if base.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression reads element %d of a %s, want a SymbolValue naming a tuple-typed local (indexing a tuple literal is not supported)", node.Ordinal, base.Kind)
		}
		return buildTupleElement(unit, snapshot, base.Symbol, node.Ordinal, locals, width, false)
	case tir.CheckedIndex:
		// A bare CheckedIndex in a pure scalar-expression position — a
		// slice/array-element read of a value with no addressable place
		// (`view()[0]` in a comparison, arithmetic, or call argument, or
		// `o![0]` where o is an array/slice-payload optional). The element
		// read is built at the ENTRY width: the runtime bounds-check helper
		// family (pebble_rt_checked_index_<suffix>) and the `.len` width cast
		// are entry-width operations, exactly as buildArrayPlaceRead and the
		// CheckedIndexPlace path use entryWidth for the same read — never the
		// element's own width, which can be narrower than the entry (an i8
		// element of a `?[3]i8` optional read inside `o![i] as int` builds its
		// child at the element's own width). The base here can only be a pure
		// projection safe to reference twice — a SymbolValue naming a
		// slice-typed local, a Load of a slice-typed place, or an
		// array/slice-payload optional force-unwrap — since a freshly-computed
		// base (a call result) needs a temp-declaration statement this
		// pure-expression position has nowhere to place (the positions that CAN
		// host a temp — a print, a local declaration, a return — intercept the
		// shape before reaching this case and thread buildSliceIndexValue's
		// pre). A str index's result is char, never the entry's integer width,
		// so a str base cannot reach this case from real source; if one does
		// (hand-built IR) it is a clean rejection.
		strBase, err := checkedIndexBaseIsStr(unit, snapshot, node)
		if err != nil {
			return "", err
		}
		if strBase {
			return "", fmt.Errorf("entry function body expression contains a str index whose result type is %s, want %s", describeType(snapshot, node.Type), wantName(width))
		}
		pre, read, err := buildSliceIndexValue(st, unit, snapshot, fileSet, id, node, locals, entryWidth, false)
		if err != nil {
			return "", err
		}
		if pre != "" {
			base, ok := unit.Node(node.Children[0])
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", node.Children[0])
			}
			return "", fmt.Errorf("entry function body expression indexes a %s of type %s in a pure-expression position with nowhere to place the temp-declaration statement the freshly-computed slice value needs; bind the slice into a local first", base.Kind, describeType(snapshot, base.Type))
		}
		return read, nil
	case tir.SourceAlias:
		if len(node.Children) == 1 {
			child, ok := unit.Node(node.Children[0])
			if ok && child.Kind == tir.TupleElementValue {
				return buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
			}
			return buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		}
		return "", fmt.Errorf("entry function body expression contains a SourceAlias, which is not supported")
	case tir.DirectCall, tir.MethodCall:
		// A call to another Pebble-convention function whose result is the
		// entry's own width. The width gate above already
		// checked node.Type (the call's result type, which is the callee's
		// resolved result type) is the entry's width. The call itself is built
		// by buildDirectCall, the single call-building machinery shared with an
		// aggregate-typed call used as a matching local's declaration
		// initializer (buildAggregateCallInitializer) — context and argument
		// handling are identical there; only the result type differs from the
		// scalar case.
		return buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want an integer literal, a reference to a local declared earlier in the body, checked +, -, *, /, %% arithmetic, bitwise &, |, ^, ~, or a call to another function", node.Kind)
	}
}

// checkedNegateLiteral folds a CheckedNegate whose single operand is a
// non-negative IntegerLiteral into its negated decimal C text at the given
// width, reporting ok=false when the operand is not a foldable literal or the
// negated value does not fit the width's range. It is the literal-only
// narrowing buildExpr's CheckedNegate case uses for widths with no
// pebble_rt_checked_neg_* runtime helper (the runtime implements only the
// i32/i64/u64 family): a literal `-5` at i16 emits the C constant `-5` — the
// same negative spelling integerLiteralText gives a negative switch case label
// — and a non-constant operand is left for the caller to reject cleanly
// rather than be emitted as a call to a nonexistent helper. The same folding
// is also applied at a width WITH a helper for the one literal whose positive
// magnitude is not a spellable C constant of that width: a negation at the
// width's exact signed minimum (see buildExpr's CheckedNegate case).
func checkedNegateLiteral(unit *tir.Unit, operandID tir.NodeID, width types.BuiltinKind) (string, bool) {
	operand, ok := unit.Node(operandID)
	if !ok || operand.Kind != tir.IntegerLiteral {
		return "", false
	}
	text := operand.Literal.IntegerNum
	if !isNonNegativeDecimal(text) {
		return "", false
	}
	value, parseOK := new(big.Int).SetString(text, 10)
	if !parseOK {
		return "", false
	}
	value.Neg(value)
	min, max, rangeOK := integerKindRange(width)
	if !rangeOK || value.Cmp(min) < 0 || value.Cmp(max) > 0 {
		return "", false
	}
	return integerLiteralText(value.String(), width), true
}

// checkedNegateMinimumText returns the C constant spelling of a signed width's
// exact minimum that compiles cleanly under -Wall -Wextra -Werror — the value
// buildExpr's CheckedNegate case emits when a literal negation folds to the
// width's minimum. The i32/int minimum is spellable as its plain decimal text
// (`-2147483648`, whose positive magnitude 2147483648 fits in a C long, so the
// negated constant never overflows a signed C type); the i64 minimum is NOT
// spellable as a decimal literal — C parses `-9223372036854775808` as the
// negation of `9223372036854775808`, which exceeds every signed C integer type
// including long long, so the constant is interpreted as unsigned and cc
// rejects it with -Wimplicitly-unsigned-literal — and the spellable spelling
// is the stdint.h INT64_MIN macro (the emitted C always includes <inttypes.h>,
// which pulls in <stdint.h>; the runtime uses INT64_MIN the same way).
func checkedNegateMinimumText(width types.BuiltinKind) string {
	if width == types.I64 {
		return "INT64_MIN"
	}
	min, _, _ := integerKindRange(width)
	return integerLiteralText(min.String(), width)
}

// buildFunctionValue builds the C expression text for one function-typed VALUE:
// a reference to an in-scope function-typed local (a SymbolValue, emitted as
// pebble_local_<symbol>, whose C type is the local's own pebble_fnptr_<typeID>_t)
// or a bare top-level function reference (a HoistedFunctionValue, emitted as
// the referenced function's own C name pebble_fn_<symbolID> — a bare C function
// name naturally decays to a function pointer of the exact fnptr typedef type,
// so no cast is needed at a declaration site). A SourceAlias (grouped-expression
// parens) is transparently unwrapped. It is the single builder shared by the
// three positions a function value can appear in: a function-typed local's
// declaration initializer (buildFunctionLocalDeclaration), a function-typed
// local's reassignment (buildStoreCore), and the general indirect call's callee
// (buildFunctionIndirectCall, also reachable through buildExpr's function-type
// bypass for a fn-typed node used as a value). Any other shape is a clean
// rejection naming what was found.
func buildFunctionValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, locals map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	switch node.Kind {
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || info.functionType == 0 {
			return "", fmt.Errorf("%s references symbol %d, which is not a function-typed local declared earlier in the body", context, node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.HoistedFunctionValue:
		decl, err := findFunctionDeclaration(unit, node.Symbol, "function value")
		if err != nil {
			return "", err
		}
		return helperCName(decl, nil), nil
	case tir.GenericFunctionValue:
		// A generic function referenced as a first-class value
		// (`var f fn(int) int = identity[int];`, function-types slice 3):
		// the same resolution a generic function CALL uses
		// (findCalledFunctionDeclaration with the value's own TypeArgs —
		// node.Symbol is the generic function's declaration symbol and
		// node.TypeArgs the concrete specialization's type arguments), and the
		// specialized declaration's own C name emitted exactly like a
		// HoistedFunctionValue's — a bare C function name that decays to the
		// exact fnptr typedef type, no cast needed. The specialization is
		// discovered as reachable by the same collectDirectCalls case added
		// for this node kind, so its pebble_fn_<symbol>_<function> definition
		// is always emitted.
		decl, err := findCalledFunctionDeclaration(unit, node.Symbol, node.TypeArgs)
		if err != nil {
			return "", err
		}
		return helperCName(decl, nil), nil
	case tir.DirectCall:
		// A call to a function-returning helper used as a function-typed value
		// (`var f fn(int, int) int = chooseOp();`, `return chooseOp();`,
		// `apply(chooseOp(), 1, 2)`, `f = chooseOp();`, or a struct field's
		// construction value `Table.{ op = chooseOp() }` — all confirmed
		// checker-reachable, all the same DirectCall-with-a-function-result
		// node whose Type is the function type): built by buildDirectCall, the
		// same call builder every other direct call uses. The emitted C is
		// `pebble_fn_<callee>(ctx, ...)` — a C function call whose return
		// type IS the callee's declared fnptr typedef, so the expression is a
		// function pointer of the exact value's C type, no cast needed.
		return buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a SourceAlias with %d child(ren), want exactly one", context, len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s contains a SourceAlias referencing invalid node %d", context, node.Children[0])
		}
		return buildFunctionValue(st, unit, snapshot, fileSet, child, locals, context, width)
	case tir.FieldValue:
		// A function-typed struct field read (`t.op`, function-types slice 2;
		// `self.hash_fn`/`self.eq_fn`, the std/hmap.peb insert/get shapes):
		// the receiver is a struct-typed local in scope (a SymbolValue),
		// emitted as its own pebble_local_<symbol> C name, and the field is
		// read as pebble_field_<member> — the exact same designated-field-name
		// convention every other struct field this backend emits already uses
		// (see buildStructBraceList's ".pebble_field_%d" inits and
		// buildFieldPlaceRead's identical trailing access), just reached
		// through buildFunctionValue instead of the width/bool-only field
		// -read path (buildFieldPlaceRead), since a function-typed field's
		// value isn't a scalar. A pointer-to-struct receiver (`self.hash_fn`
		// where self is a `*HashMap` method parameter) is supported too: the
		// C projection uses `->` instead of `.`, exactly as buildPlaceLValue's
		// FieldPlace case and buildStructFieldRead resolve a pointer receiver.
		// Anything else is a clean rejection naming what was found.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a FieldValue with %d child(ren), want exactly one (the struct receiver)", context, len(node.Children))
		}
		receiver, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s contains a FieldValue referencing invalid receiver node %d", context, node.Children[0])
		}
		if receiver.Kind != tir.SymbolValue {
			return "", fmt.Errorf("%s reads a function-typed field from a %s receiver, want a reference to a struct-typed local declared earlier in the body", context, receiver.Kind)
		}
		info, declared := locals[receiver.Symbol]
		if !declared {
			return "", fmt.Errorf("%s reads a function-typed field from symbol %d, which is not a local declared earlier in the body", context, receiver.Symbol)
		}
		access := "."
		if info.structType == 0 && info.runtimeType == 0 {
			// A pointer-to-struct receiver: the pointer's pointee must be a
			// struct type (info.pointerType records the pointer type, and
			// pointerPointeeType extracts its pointee), and the field read
			// uses the `->` access the C pointer gives.
			if _, ok := pointerPointeeType(snapshot, info.pointerType); !ok {
				return "", fmt.Errorf("%s reads a function-typed field from symbol %d, which is not a struct-typed local declared earlier in the body", context, receiver.Symbol)
			}
			access = "->"
		}
		return fmt.Sprintf("pebble_local_%d%spebble_field_%d", receiver.Symbol, access, node.Member), nil
	case tir.Load:
		// A function-typed field read used as an rvalue in a position other
		// than an indirect call's direct callee (e.g. a local declaration's
		// initializer, `var f fn(int, int) int = t.op;`) is wrapped in a
		// Load node by the checker (confirmed via a real fixture: Load's
		// single child here is a FieldPlace, not the bare FieldValue the
		// direct-callee position produces) — transparently unwrapped to its
		// single child.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a Load with %d child(ren), want exactly one", context, len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s contains a Load referencing invalid node %d", context, node.Children[0])
		}
		return buildFunctionValue(st, unit, snapshot, fileSet, child, locals, context, width)
	case tir.FieldPlace:
		// The lvalue-place form of a function-typed struct field read (the
		// shape a Load's child takes, as opposed to FieldValue's rvalue
		// form) — the receiver is a StoragePlace naming a struct-typed local
		// in scope, and the field is read the same
		// pebble_local_<symbol>.pebble_field_<member> way FieldValue's case
		// does. A pointer-to-struct receiver (`self.hash_fn` where self is a
		// `*HashMap` method parameter) is supported too, using `->` access,
		// exactly as the FieldValue case resolves it.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s contains a FieldPlace with %d child(ren), want exactly one (the struct receiver)", context, len(node.Children))
		}
		receiverPlace, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s contains a FieldPlace referencing invalid receiver node %d", context, node.Children[0])
		}
		if receiverPlace.Kind != tir.StoragePlace {
			return "", fmt.Errorf("%s reads a function-typed field from a %s receiver, want a reference to a struct-typed local declared earlier in the body", context, receiverPlace.Kind)
		}
		info, declared := locals[receiverPlace.Symbol]
		if !declared {
			return "", fmt.Errorf("%s reads a function-typed field from symbol %d, which is not a local declared earlier in the body", context, receiverPlace.Symbol)
		}
		access := "."
		if info.structType == 0 && info.runtimeType == 0 {
			if _, ok := pointerPointeeType(snapshot, info.pointerType); !ok {
				return "", fmt.Errorf("%s reads a function-typed field from symbol %d, which is not a struct-typed local declared earlier in the body", context, receiverPlace.Symbol)
			}
			access = "->"
		}
		return fmt.Sprintf("pebble_local_%d%spebble_field_%d", receiverPlace.Symbol, access, node.Member), nil
	default:
		return "", fmt.Errorf("%s contains a %s, want a reference to a function-typed local or a bare function value", context, node.Kind)
	}
}

// buildOptionalValue builds the C expression text for an optional-typed value
// in the one position-neutral grammar both an optional-returning function's
// tail-position return and a call site's optional-typed argument share: the
// target optional type comes in as optionalType (a return passes
// result.optionalType; a call site passes its parameter's type), and exactly
// four value shapes are supported (all confirmed against real fixtures):
//
//   - a plain SymbolValue naming an already-declared optional-typed local — or
//     an optional-typed parameter, which seeds the callee's scope identically —
//     in scope whose declared type is exactly the target optional type, emitted
//     as the local's own pebble_local_<symbol> C name: forwarding an
//     already-computed optional value without re-constructing it;
//   - a fresh SomeOptional / NoneOptional / OptionalInject construction of the
//     matching optional type (`some x;`, `none`, or an implicit injection — at
//     a call site the checker wraps even a scalar implicit injection like `g(5)`
//     in an OptionalInject node, while at a return position a scalar payload's
//     injection arrives as the bare payload value itself, the fourth shape
//     below; a tuple/struct payload's implicit injection is an OptionalInject
//     in both positions), built by the shared buildOptionalValueExpr (the
//     same builder a nested-in-aggregate optional value uses), which handles
//     every supported payload grammar and emits the optional's own compound
//     literal;
//   - a DirectCall to another optional-returning helper (`return g();` /
//     `g(f());` — the call node carries the optional result type, so it is
//     built by the same buildDirectCall machinery any call uses and forwarded
//     as the value, confirmed reachable from real source);
//   - a bare payload value whose Type is the optional's payload type rather
//     than the optional type itself — the scalar implicit-injection shape the
//     checker produces for `return 5;` in an optional-returning helper (the
//     payload is injected without an OptionalInject wrapper, unlike the
//     aggregate shapes above) — injected here as a fresh
//     `(pebble_optional_<typeID>_t){ .has_value = true, .value = <payload> }`
//     compound literal, the payload built by the grammar its own type selects.
//
// Any other value shape is a clean rejection. context names the position in
// rejection messages ("entry function body return statement" for a return, or
// "entry function body expression contains a call to symbol <N> whose
// parameter <M>" for a call argument). width is the entry's
// resolved integer width, threaded through to the inline builders so each
// payload is built at the width the target type's own typedef uses.
func buildOptionalValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, optionalType types.TypeID, context string, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("%s references invalid value node %d", context, id)
	}
	if node.Kind == tir.SymbolValue {
		info, declared := locals[node.Symbol]
		if !declared {
			return "", fmt.Errorf("%s names symbol %d, which is not a local in scope", context, node.Symbol)
		}
		if info.optional != optionalType {
			return "", fmt.Errorf("%s names symbol %d, which is a local of type %s, not an optional-typed local of type %s", context, node.Symbol, describeType(snapshot, node.Type), optionalTypeName(optionalType))
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	if node.Kind == tir.Load {
		// A whole optional read through a pointer deref used directly as the
		// optional value — `return *p;` in an optional-returning helper, or
		// `reset(*p);` passing the dereferenced optional. The value is a Load
		// whose place is a DereferencePlace, emitted as the null-checked
		// whole-optional deref value buildDereferencePlaceRead produces
		// (`*(pebble_optional_<typeID>_t)(pebble_rt_checked_deref_ptr(...))`)
		// — the optional's own typedef makes the by-value read trivially valid
		// C, the same whole-optional deref read shape the struct and tuple
		// sides use. The Load's own Type must be exactly the target optional
		// type (defense for hand-built IR).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s is a Load with %d child(ren), want exactly one place", context, len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s is a Load referencing invalid place node %d", context, node.Children[0])
		}
		if place.Kind != tir.DereferencePlace {
			return "", fmt.Errorf("%s is a Load whose place is a %s, want a DereferencePlace (a by-value whole-optional read through a pointer)", context, place.Kind)
		}
		if node.Type != optionalType {
			return "", fmt.Errorf("%s is a Load of type %s, not an optional-typed value of type %s", context, describeType(snapshot, node.Type), optionalTypeName(optionalType))
		}
		value, err := buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, locals, width, node.Span, false)
		if err != nil {
			return "", err
		}
		return value, nil
	}
	if node.Kind == tir.SomeOptional || node.Kind == tir.NoneOptional || node.Kind == tir.OptionalInject {
		// A fresh optional value built inline: `some x;`, `none`, or an
		// implicit injection `(1, 2);` / a call site's `g(5)` (OptionalInject).
		// buildOptionalValueExpr shares the SomeOptional
		// and OptionalInject cases — both carry exactly one payload child
		// and lower to the identical C — and handles the NoneOptional
		// zero-child form.
		return buildOptionalValueExpr(st, unit, snapshot, fileSet, node, locals, context, width)
	}
	if node.Kind == tir.DirectCall {
		// A forward of another optional-returning helper's result:
		// `return g();` or `g(f());`. The call's own result type is validated
		// by validateHelperSignature (the callee's declared ResultType must be
		// an optional type) and the checker coerces the value to the target
		// optional type, so a plain buildDirectCall of
		// the call node is the whole lowering — the call already returns
		// the optional's own C type.
		return buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
	}
	// The remaining supported shape is the bare scalar payload the checker
	// produces for implicit injection in a return (`return 5;` in a ?int
	// helper): the value node carries the payload type, not the optional
	// type. Inject it here — the same C a SomeOptional/OptionalInject of
	// this payload would emit, with the payload built by the grammar its own
	// type selects.
	key, ok := snapshot.Key(optionalType)
	if !ok {
		return "", fmt.Errorf("%s names a value of type %s whose target optional type %d is not in the type snapshot", context, describeType(snapshot, node.Type), optionalType)
	}
	payload, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("%s names a value of type %s whose target optional type %s has no payload type", context, describeType(snapshot, node.Type), optionalTypeName(optionalType))
	}
	if node.Type != payload {
		return "", fmt.Errorf("%s names a %s, want an optional-typed value of type %s (a reference to an optional-typed local, some/none, a call to an optional-returning helper) or a payload value of type %s", context, describeType(snapshot, node.Type), optionalTypeName(optionalType), describeType(snapshot, payload))
	}
	var value string
	var err error
	payloadWidth, integerPayload := resolvedBuiltin(snapshot, payload)
	switch {
	case integerPayload && cType(payloadWidth) != "" && !isUint(snapshot, payload):
		value, err = buildExpr(st, unit, snapshot, fileSet, id, locals, payloadWidth, width)
	case isUint(snapshot, payload):
		value, err = buildUintExpr(st, unit, snapshot, fileSet, id, locals, width)
	case isBool(snapshot, payload):
		value, err = buildBoolExpr(st, unit, snapshot, fileSet, id, locals, width)
	case isPointer(snapshot, payload):
		// A bare pointer payload implicitly injected into a ?*T optional (a
		// return/argument whose payload value is the pointer itself, e.g. a
		// `return &y;` inside a ?*int helper with no explicit some keyword) —
		// built by the same buildExpr pointer path any pointer-typed value
		// takes, whose isPointer bypass ignores the ambient width args.
		value, err = buildExpr(st, unit, snapshot, fileSet, id, locals, width, width)
	default:
		return "", fmt.Errorf("%s implicitly injects a payload value of type %s, want a fixed-width integer or bool", context, describeType(snapshot, payload))
	}
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(%s){ .has_value = true, .value = %s }", optionalTypeName(optionalType), value), nil
}

// buildBoolExpr builds the C text for a bool value node, used both for a bool
// local's initializer/reassignment value and for a bare bool if/while
// condition (via buildCondition). The bool grammar is genuinely different from
// the integer one buildExpr handles: there is no checked arithmetic — bools
// are combined, compared, and negated with plain C, which cannot fault — so it
// is a separate builder rather than a mode on buildExpr. width is the entry's
// resolved integer width, threaded through to the comparison path so a
// comparison used as a bool value's operand builds its own integer operands at
// the entry's width. It accepts exactly seven node kinds, each carrying the
// snapshot's bool builtin:
//
//   - BoolLiteral — the C literal true/false (requires #include <stdbool.h>).
//
//   - SymbolValue whose Symbol is a bool local in scope (the locals map
//     records types.Bool for it) — pebble_local_<symbol ID>, the same C name
//     buildLeadingStatement gave that local's declaration.
//
//   - PrefixValue with operator ! (syntax.Bang, confirmed against a real
//     fixture — a bool `!` is a PrefixValue, not the CheckedNegate integer
//     negation uses) and exactly one operand that is itself a bool value in
//     this grammar — !(<operand>), plain C negation. The operand is built by
//     recursing into this same builder, so a negated comparison (e.g.
//     !(i < 5)) is now accepted: its operand is a SourceAlias wrapping a
//     BinaryValue, both handled below.
//
//   - BinaryValue with one of the six comparison operators — delegated to
//     buildComparison, the same path a top-level if/while condition uses, so
//     a comparison can serve as an operand of && / || as well as stand alone.
//     buildComparison decides the operand grammar from the operands' resolved
//     types: integer operands take the integer comparison path, and two bool
//     operands — e.g. (1 < 2) == (3 < 4), whose SourceAlias-wrapped
//     comparison operands are bool values — take the bool-equality path.
//     (A BinaryValue with any other operator is rejected by buildComparison's
//     operator check.)
//
//   - ShortCircuitValue with operator && (syntax.LogicalAnd) or ||
//     (syntax.LogicalOr) — <(left) && (right)> / <(left) || (right)>,
//     parenthesized so nested combinations produce unambiguous C regardless of
//     depth. Both operands are built by recursing into this same builder, so
//     && and || combine literals, bool locals, ! negations, comparisons,
//     bool-returning calls, and nested && / || freely. Plain C && and || are
//     the correct lowering: both languages short-circuit the same way, so a
//     call operand is evaluated exactly when Pebble would evaluate it (a
//     skipped right operand is skipped in both languages). The operand tree
//     already encodes the language's &&-vs-|| precedence (confirmed: Pebble's
//     grammar gives || precedence 1 and && precedence 2), so this builder
//     never re-derives precedence.
//
//   - SourceAlias — a transparent wrapper (the grouped-expression parens), so
//     it is unwrapped and its single child built by recursing into this same
//     builder. A parenthesized comparison operand of && / || is exactly this
//     shape (confirmed against a real fixture: flag && (1 < 2) has the
//     comparison wrapped in a SourceAlias, while the unparenthesized
//     1 < 2 && 3 < 4 wraps nothing).
//
//   - Load of a TuplePlace — a tuple-typed local's bool element read (`t.1`
//     in a bool position), the same Load(TuplePlace) shape buildExpr's Load
//     case handles but with the element's own type gated to bool here, so the
//     read emits pebble_local_<symbol>._<ordinal> via buildTupleElement. (A
//     plain bool local read is a SymbolValue, not a Load.)
//
//   - DirectCall / MethodCall — a call to a bool-returning helper function or
//     method used directly as a bool value (confirmed checker-reachable: a
//     plain helper with a bool result type is admitted by
//     validateHelperSignature, so `if id(true) { ... }` lowers the condition
//     to a bool-typed DirectCall, and a bool-returning method call lowers to a
//     bool-typed MethodCall). The call is built by the same buildDirectCall
//     machinery a scalar-width call uses, and its result is a C bool, which
//     this position accepts directly.
//
// A SymbolValue referencing anything else — an integer local, a global, a
// parameter — and any other node kind at any position is a clean rejection
// naming what was found.
func buildBoolExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	if !isBool(snapshot, node.Type) && !isTypeParameterType(snapshot, node.Type) {
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want bool", node.Kind, describeType(snapshot, node.Type))
	}
	switch node.Kind {
	case tir.BoolLiteral:
		if node.Literal.Bool {
			return "true", nil
		}
		return "false", nil
	case tir.SymbolValue:
		if locals[node.Symbol].kind != types.Bool {
			if ginfo, isGlobal := st.globals[node.Symbol]; isGlobal {
				if ginfo.info.kind != types.Bool {
					return "", fmt.Errorf("entry function body expression references global symbol %d, which is not a bool-typed global", node.Symbol)
				}
				return fmt.Sprintf("pebble_global_%d", node.Symbol), nil
			}
			if einfo, isExtern := st.externData[node.Symbol]; isExtern {
				if einfo.info.kind != types.Bool {
					return "", fmt.Errorf("entry function body expression references extern variable symbol %d, which is not a bool-typed extern variable", node.Symbol)
				}
				return einfo.name, nil
			}
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a bool local declared earlier in the entry body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.DirectCall, tir.MethodCall:
		return buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
	case tir.IndirectCall:
		// A call through a function-typed value whose result is bool
		// (confirmed checker-reachable: `if f(true) { ... }` lowers the
		// condition to a bool-typed IndirectCall). The call itself is built by
		// the same buildIndirectCall machinery a scalar-width indirect call
		// uses — the callee and every argument are built under the callee's
		// own function type — and the result is a C bool, which this position
		// accepts directly.
		return buildIndirectCall(st, unit, snapshot, fileSet, node, locals, width)
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional-typed local with a bool payload (x!).
		// The child is a SymbolValue naming the optional local, and this
		// node's Type is bool (already gated above). The unwrap is
		// bounds-checked via the runtime helper, passing the optional local's
		// has_value and value fields.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind == tir.SourceAlias && len(child.Children) == 1 {
			// A grouped-expression alias around the unwrap's base — `(*p)!` —
			// is transparent: unwrap the alias and process the base exactly as
			// if the parens were absent.
			child, ok = unit.Node(child.Children[0])
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid alias child node %d", child.Children[0])
			}
		}
		if child.Kind == tir.Load && len(child.Children) == 1 {
			if _, ok := unit.Node(child.Children[0]); !ok {
				return "", fmt.Errorf("invalid optional place")
			}
			expr, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], locals, width)
			if err != nil {
				return "", err
			}
			if !isOptional(snapshot, typ) {
				return "", fmt.Errorf("optional unwrap base is not optional")
			}
			return fmt.Sprintf("pebble_rt_checked_unwrap_%s(%s.has_value, %s.value, %s)", checkedSuffix(width), expr, expr, buildSourceLoc(fileSet, node.Span)), nil
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		info, declared := locals[child.Symbol]
		if !declared {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not a local declared earlier in the entry body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		return fmt.Sprintf("pebble_rt_checked_unwrap_bool(pebble_local_%d.has_value, pebble_local_%d.value, %s)", child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
	case tir.Load:
		// A tuple-typed local's bool element read or a struct-typed local's
		// bool field read (see buildExpr's Load case for the shape
		// confirmation; here the Load's Type is the element/field's bool type,
		// already gated above).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if place.Kind != tir.TuplePlace {
			if place.Kind == tir.CheckedIndexPlace {
				return buildArrayPlaceRead(st, unit, snapshot, fileSet, place, locals, width, true)
			}
			if place.Kind == tir.FieldPlace {
				return buildStructFieldRead(st, unit, snapshot, fileSet, place, locals, width, true)
			}
			if place.Kind == tir.DereferencePlace {
				return buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, locals, width, node.Span, true)
			}
			return "", fmt.Errorf("entry function body expression contains a Load whose place is a %s, want a TuplePlace, CheckedIndexPlace, FieldPlace, or DereferencePlace", place.Kind)
		}
		return buildTuplePlaceRead(st, unit, snapshot, fileSet, place, locals, width, true)
	case tir.CheckedIndex:
		// A bare CheckedIndex in a pure bool-expression position — a
		// slice-element read of a bool-typed value with no addressable place
		// (`boolview()[0]` as a condition, a && / || operand, or a call
		// argument), the bool twin of buildExpr's CheckedIndex case. Same
		// pre-threading rule: a pure-projection base (a slice-typed local or
		// place, which needs no temp) is emitted directly; a freshly-computed
		// base (a call result) needs a temp-declaration statement this
		// position has nowhere to place and is a clean rejection — the
		// positions that CAN host a temp (a print, a local declaration, a
		// return) intercept the shape before reaching this case.
		strBase, err := checkedIndexBaseIsStr(unit, snapshot, node)
		if err != nil {
			return "", err
		}
		if strBase {
			return "", fmt.Errorf("entry function body expression contains a str index whose result type is %s, want bool", describeType(snapshot, node.Type))
		}
		pre, read, err := buildSliceIndexValue(st, unit, snapshot, fileSet, id, node, locals, width, true)
		if err != nil {
			return "", err
		}
		if pre != "" {
			base, ok := unit.Node(node.Children[0])
			if !ok {
				return "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", node.Children[0])
			}
			return "", fmt.Errorf("entry function body expression indexes a %s of type %s in a pure-expression position with nowhere to place the temp-declaration statement the freshly-computed slice value needs; bind the slice into a local first", base.Kind, describeType(snapshot, base.Type))
		}
		return read, nil
	case tir.TupleElementValue:
		// Defense for hand-built IR, exactly like buildExpr's TupleElementValue
		// case: the checker never produces this shape for a bool element read of
		// a tuple local (that is a Load of a TuplePlace); a TupleElementValue
		// whose single child is a SymbolValue naming a tuple-typed local is
		// accepted here, anything else is a clean rejection.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue with %d child(ren), want exactly one (the tuple value being indexed)", len(node.Children))
		}
		base, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue referencing invalid node %d", node.Children[0])
		}
		if base.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression reads element %d of a %s, want a SymbolValue naming a tuple-typed local (indexing a tuple literal is not supported)", node.Ordinal, base.Kind)
		}
		return buildTupleElement(unit, snapshot, base.Symbol, node.Ordinal, locals, width, true)
	case tir.PrefixValue:
		if node.Operator != syntax.Bang {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with operator %s, want !", node.Operator)
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with %d operand(s), want exactly one", len(node.Children))
		}
		child, err := buildBoolExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		return "!(" + child + ")", nil
	case tir.BinaryValue:
		// A comparison used as a bool value (an operand of && / ||, or the
		// condition routed here by buildCondition) is the same BinaryValue
		// shape buildComparison already lowers for a top-level condition, so it
		// is delegated unchanged. Non-comparison operators and non-integer
		// operands are rejected by buildComparison itself.
		return buildComparison(st, unit, snapshot, fileSet, id, locals, width)
	case tir.ShortCircuitValue:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a ShortCircuitValue with %d operand(s), want exactly two", len(node.Children))
		}
		op, ok := shortCircuitOperator(node.Operator)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a ShortCircuitValue with operator %s, want && or ||", node.Operator)
		}
		left, err := buildBoolExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildBoolExpr(st, unit, snapshot, fileSet, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return "(" + left + " " + op + " " + right + ")", nil
	case tir.SourceAlias:
		// A SourceAlias is transparent — it records grouped-expression parens
		// and nothing else — so it is unwrapped and its single child built.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a SourceAlias with %d child(ren), want exactly one", len(node.Children))
		}
		return buildBoolExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width)
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a bool literal, a reference to a bool local declared earlier in the body, a comparison, a && / || combination, or a ! negation", node.Kind)
	}
}

// buildFloatExpr builds one float value in a position that accepts a float
// expression, the float grammar's counterpart of buildBoolExpr: the float
// builtins' scalar shapes, built on top of the same locals/scope machinery
// buildExpr uses. Stage A supports exactly three node kinds, and no others,
// corresponding to declaring a float local, reading it, and (for a float-
// returning main) returning it:
//
//   - tir.FloatLiteral — a float literal (e.g. 3.14), emitted as its C
//     float/double constant text verbatim (the checker's validated decimal
//     text is already a valid C floating constant — a decimal point and/or
//     exponent are always present — and needs no suffix for either float
//     width, since assigning a double constant to a float is not a warning
//     under this suite's -Wall -Wextra -Werror). The text is defensively
//     re-validated before being trusted, mirroring how buildExpr's
//     IntegerLiteral case validates its own literal text.
//   - tir.SymbolValue — a reference to an in-scope float-typed local of the
//     same float kind, emitted as pebble_local_<symbolID> (the reader of a
//     float local).
//   - tir.SourceAlias — Pebble's grouped-expression parens, transparently
//     unwrapped (exactly one child), the same distinction buildExpr and
//     buildBoolExpr make for parenthesized float expressions.
//
// Width must be one of the two float builtins (F32 for an f32 position, F64
// for an f64 position) and every node in an accepted expression tree must
// carry exactly that builtin — a node carrying the other float kind, or a
// non-float value, is a clean rejection naming the wanted kind, never a
// coercion. entryWidth is the enclosing function's resolved integer width
// (the same value buildExpr receives as its own entryWidth parameter):
// buildFloatExpr never uses it for the float value itself — the float kind
// is always width — but a float place read's Load case resolves its place
// through buildPlaceLValue, whose CheckedIndexPlace subscript selects its
// bounds-checked index helper at the ENTRY width, never the float kind.
// Since this slice widened float helper parameters and results, a
// DirectCall/MethodCall to a float-returning helper is a supported float
// value (a float local's call initializer, a float call argument, a float
// comparison operand, a print operand, or a float-returning helper's
// tail-position return forward) built by the same buildDirectCall machinery a
// scalar-width call uses. Float arithmetic, comparisons, and casts are
// likewise supported. Shared by the positions a float value can appear in: a
// float local's declaration initializer (buildScalarInitializeCore), a float
// local's reassignment (buildStoreCore), a float call argument
// (buildCallArgument), a float comparison operand (buildComparison), a print
// operand (buildPrint), and a float-returning entry's or helper's
// tail-position return value (buildBlock / buildSwitchCaseBody dispatch on
// resultInfo.kind).
func buildFloatExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, entryWidth types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	if !isFloat(snapshot, node.Type) {
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want %s", node.Kind, describeType(snapshot, node.Type), wantName(width))
	}
	builtin, _ := resolvedBuiltin(snapshot, node.Type)
	if builtin != width {
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want %s", node.Kind, describeType(snapshot, node.Type), wantName(width))
	}
	switch node.Kind {
	case tir.FloatLiteral:
		text := node.Literal.Float
		if !isValidFloatLiteralText(text) {
			return "", fmt.Errorf("entry function body expression contains a float literal with malformed text %q", text)
		}
		return text, nil
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || info.kind != width {
			if ginfo, isGlobal := st.globals[node.Symbol]; isGlobal {
				if ginfo.info.kind != width {
					return "", fmt.Errorf("entry function body expression references global symbol %d, which is not a %s global", node.Symbol, wantName(width))
				}
				return fmt.Sprintf("pebble_global_%d", node.Symbol), nil
			}
			if einfo, isExtern := st.externData[node.Symbol]; isExtern {
				if einfo.info.kind != width {
					return "", fmt.Errorf("entry function body expression references extern variable symbol %d, which is not a %s extern variable", node.Symbol, wantName(width))
				}
				return einfo.name, nil
			}
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a %s local declared earlier in the body", node.Symbol, wantName(width))
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional whose payload is a float (`o!` where
		// o is `?f64`, the `if o! == 1.5` shape this slice unblocks). The
		// unwrap is bounds-checked via the runtime helper selected from the
		// PAYLOAD's own type (optionalUnwrapSuffix maps a float payload to the
		// f32/f64 helper, since a float payload's .value field is the plain C
		// float/double — see optionalPayloadCType — which only
		// pebble_rt_checked_unwrap_f32/f64 reads back at its true width) —
		// passing the optional local's has_value and value fields, the same
		// scalar-unwrap shape buildExpr's integer and bool payload unwraps
		// emit. node.Type is the payload float builtin, already gated to width
		// above, so a none unwrap panics with PEBBLE_PANIC_UNWRAP_FAILED
		// exactly like every other payload type's unwrap. The single child is
		// a SymbolValue naming the optional-typed local (a Load of an
		// optional-typed place, the other shape buildExpr's unwrap accepts, is
		// also handled).
		unwrapSuffix := optionalUnwrapSuffix(snapshot, node.Type)
		if unwrapSuffix == "" {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of a %s payload, which has no runtime unwrap helper", describeType(snapshot, node.Type))
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind == tir.Load && len(child.Children) == 1 {
			expr, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], locals, entryWidth)
			if err != nil {
				return "", err
			}
			if !isOptional(snapshot, typ) {
				return "", fmt.Errorf("optional unwrap base is not optional")
			}
			return fmt.Sprintf("pebble_rt_checked_unwrap_%s(%s.has_value, %s.value, %s)", unwrapSuffix, expr, expr, buildSourceLoc(fileSet, node.Span)), nil
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		info, declared := locals[child.Symbol]
		if !declared {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not a local declared earlier in the entry body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		return fmt.Sprintf("pebble_rt_checked_unwrap_%s(pebble_local_%d.has_value, pebble_local_%d.value, %s)", unwrapSuffix, child.Symbol, child.Symbol, buildSourceLoc(fileSet, node.Span)), nil
	case tir.Load:
		// A by-value read of a float-typed place — a float tuple element read
		// (`t.0`, lowered to Load(TuplePlace)), a float element of an array
		// (`a[0]`, lowered to Load(CheckedIndexPlace)), or a float element of
		// a slice (also CheckedIndexPlace) — used as a float value. The Load's
		// Type is already gated to this same float kind by the entry check
		// above, so the whole read is the place's C lvalue built by
		// buildPlaceLValue (the same projection a float element write or
		// address-of targets), whose resolved element type must be that same
		// float kind (defense for hand-built IR). A float place read is a
		// plain C member/index/deref expression exactly like an integer place
		// read, so no runtime helper call is involved — the checked_index
		// bounds probe inside a CheckedIndexPlace's C subscript (the one
		// runtime call an array/slice read emits) is selected at the ENTRY
		// width (entryWidth), never the float kind, exactly as buildExpr's
		// integer Load case resolves its CheckedIndexPlace.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d children, want exactly one place", len(node.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, entryWidth)
		if err != nil {
			return "", fmt.Errorf("entry function body expression place: %v", err)
		}
		if resolvedFloatKind(snapshot, placeType) != width {
			return "", fmt.Errorf("entry function body expression contains a Load whose place has element type %s, want %s", describeType(snapshot, placeType), wantName(width))
		}
		return lvalue, nil
	case tir.SourceAlias:
		// A SourceAlias is transparent — it records grouped-expression parens
		// and nothing else — so it is unwrapped and its single child built.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a SourceAlias with %d child(ren), want exactly one", len(node.Children))
		}
		return buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
	case tir.PrefixValue:
		// A unary minus on a float (`-x`, `-3.5`) arrives as a PrefixValue
		// with operator -: the checker only lowers a negate to CheckedNegate
		// when an operand is an integer, so a float negate is a PrefixValue.
		// Float negation is a defined C operation (the IEEE 754 sign flip;
		// only the -0.0 boundary is sign-flipped, which is exactly the
		// intended semantics), so the plain C `-` operator is a direct,
		// correct lowering of the built child.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with %d operand(s), want exactly one", len(node.Children))
		}
		if node.Operator != syntax.Minus {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with operator %s, want -", node.Operator)
		}
		child, err := buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return "(-" + child + ")", nil
	case tir.DirectCall, tir.MethodCall:
		// A call to a float-returning helper used directly as a float value
		// (a float local's call initializer, a float call argument, a float
		// comparison operand, a print operand, or a float-returning helper's
		// tail-position return forward). The call node's own Type is the
		// callee's resolved result type, gated to the same float kind above;
		// the call is built by the same buildDirectCall machinery a
		// scalar-width call uses, so context and argument handling are
		// identical.
		return buildDirectCall(st, unit, snapshot, fileSet, node, locals, width)
	case tir.BinaryValue:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body float arithmetic has %d operands, want exactly two", len(node.Children))
		}
		op, ok := arithmeticOperator(node.Operator)
		if !ok || node.Operator == syntax.Percent {
			return "", fmt.Errorf("entry function body float arithmetic uses operator %s, want +, -, *, or /", node.Operator)
		}
		left, err := buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		right, err := buildFloatExpr(st, unit, snapshot, fileSet, node.Children[1], locals, width, entryWidth)
		if err != nil {
			return "", err
		}
		return "(" + left + " " + op + " " + right + ")", nil
	case tir.IntegerToFloat:
		// An integer value cast to a float (`x as f64` where x is an integer).
		// The result is a float (this node's Type is the destination float
		// builtin, already gated to width above), and the single child is the
		// integer being cast. The child is built via buildExpr — NOT
		// buildFloatExpr — at its own resolved integer width, mirroring how
		// buildExpr's IntegerCast case resolves the child's own width
		// independently of the ambient width, because a cast's whole point is
		// that its operand's width differs from it. The lowering is a plain,
		// unchecked C cast `(<destination float type>)(<child>)`: C's
		// integer-to-float conversion is well-defined for every input (no
		// undefined behavior, no range fault), so — exactly like IntegerCast —
		// no checked runtime primitive is needed.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains an IntegerToFloat with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an IntegerToFloat with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || floatCType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains an IntegerToFloat with non-float destination type %s", describeType(snapshot, node.Type))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains an IntegerToFloat referencing invalid child node %d", node.Children[0])
		}
		childType, ok := snapshot.Key(child.Type)
		if !ok {
			return "", fmt.Errorf("entry function body IntegerToFloat child has invalid type %d", child.Type)
		}
		childWidth, ok := childType.Builtin()
		if !ok || cType(childWidth) == "" {
			return "", fmt.Errorf("entry function body IntegerToFloat child has non-integer type %s", describeType(snapshot, child.Type))
		}
		childExpr, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], locals, childWidth, entryWidth)
		if err != nil {
			return "", fmt.Errorf("entry function body integer-to-float cast child: %v", err)
		}
		return "(" + floatCType(destinationWidth) + ")(" + childExpr + ")", nil
	case tir.FloatCast:
		// A float value cast to a different float width (`x as f32` where x is
		// an f64, or vice versa). The result is a float (this node's Type is
		// the destination float builtin, already gated to width above), and the
		// single child is the float being cast. The child is built via a
		// recursive buildFloatExpr call at the CHILD's own resolved float width
		// (not the destination width), the same "recurse at the child's own
		// width" principle as IntegerCast. The lowering is a plain, unchecked C
		// cast `(<destination float type>)(<child>)`: C's float-to-float
		// conversion is well-defined for every input, so no checked runtime
		// primitive is needed.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a FloatCast with %d children, want exactly one", len(node.Children))
		}
		destination, ok := snapshot.Key(node.Type)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a FloatCast with invalid destination type %d", node.Type)
		}
		destinationWidth, ok := destination.Builtin()
		if !ok || floatCType(destinationWidth) == "" {
			return "", fmt.Errorf("entry function body expression contains a FloatCast with non-float destination type %s", describeType(snapshot, node.Type))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a FloatCast referencing invalid child node %d", node.Children[0])
		}
		childWidth := resolvedFloatKind(snapshot, child.Type)
		if childWidth == 0 {
			return "", fmt.Errorf("entry function body expression contains a FloatCast with non-float child type %s", describeType(snapshot, child.Type))
		}
		childExpr, err := buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], locals, childWidth, entryWidth)
		if err != nil {
			return "", fmt.Errorf("entry function body float cast child: %v", err)
		}
		return "(" + floatCType(destinationWidth) + ")(" + childExpr + ")", nil
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a float literal, a reference to a %s local declared earlier in the body, float arithmetic, a negation, or a call to a float-returning function", node.Kind, wantName(width))
	}
}
