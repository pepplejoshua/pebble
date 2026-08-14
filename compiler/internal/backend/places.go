package backend

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// indirectCalleePlace unwraps an IndirectCall's callee child (Children[0])
// past any SourceAlias (grouped-expression parens) and a single Load, and
// reports whether the unwrapped node is specifically the built-in Allocator's
// alloc/realloc/free field — NOT merely "any FieldPlace/FieldValue," which is
// too broad: a function-typed STRUCT FIELD (e.g. `t.op` where `op fn(int,
// int) int;`, slice 2 of the function-types feature) produces the exact same
// FieldValue node shape a real allocator field access does (confirmed via a
// real fixture) — the two are distinguished only by checking the field's
// actual owner type and member symbol against the runtime's own
// AllocatorAlloc/AllocatorRealloc/AllocatorFree identities via
// runtimeFieldName, exactly like the pre-existing allocator-only code already
// did before this function existed. Both buildIndirectCall and
// collectFunctionTypesWalk call this so the two agree on which case any given
// IndirectCall is — confirmed via a real fixture that the checker sets
// IndirectCall.FunctionType on BOTH the allocator case and the general case
// (not just the general one, contrary to an earlier, unverified assumption),
// so FunctionType alone cannot distinguish them.
func indirectCalleePlace(unit *tir.Unit, node tir.Node) (placeNode tir.Node, isAllocator bool, ok bool) {
	if len(node.Children) < 1 {
		return tir.Node{}, false, false
	}
	calleeNode, found := unit.Node(node.Children[0])
	if !found {
		return tir.Node{}, false, false
	}
	placeNode = calleeNode
	for placeNode.Kind == tir.SourceAlias && len(placeNode.Children) == 1 {
		placeNode, _ = unit.Node(placeNode.Children[0])
	}
	if placeNode.Kind == tir.Load && len(placeNode.Children) == 1 {
		placeNode, _ = unit.Node(placeNode.Children[0])
	}
	var owner types.TypeID
	var member symbol.SymbolID
	switch {
	case placeNode.Kind == tir.FieldPlace && len(placeNode.Children) == 1:
		if base, baseOK := unit.Node(placeNode.Children[0]); baseOK {
			owner, member = base.Type, placeNode.Member
		}
	case placeNode.Kind == tir.FieldValue && len(placeNode.Children) == 1:
		if base, baseOK := unit.Node(placeNode.Children[0]); baseOK {
			owner, member = base.Type, placeNode.Member
		}
	}
	if owner != 0 && member != 0 {
		if _, mapped := runtimeFieldName(unit, owner, member); mapped {
			isAllocator = member == unit.Runtime().AllocatorAlloc || member == unit.Runtime().AllocatorRealloc || member == unit.Runtime().AllocatorFree
		}
	}
	return placeNode, isAllocator, true
}

// buildTuplePlaceRead builds the C text for reading one element of a tuple
// local through the Load(TuplePlace) shape the checker actually produces for
// `t.<ordinal>` (confirmed against a real fixture): the TuplePlace carries the
// element Ordinal and its single child is the StoragePlace naming the tuple
// local. wantBool selects which grammar the element must satisfy — bool (the
// buildBoolExpr path) or the entry's width (the buildExpr path) — matching how
// the Load's own Type was already gated by the caller's builder. The emitted C
// is pebble_local_<symbol>._<ordinal>.
func buildTuplePlaceRead(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	if len(place.Children) != 1 {
		return "", fmt.Errorf("tuple place wants one base")
	}
	expr, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, place.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	key, ok := snapshot.Key(typ)
	if !ok {
		return "", fmt.Errorf("tuple place type %d is not in the type snapshot", typ)
	}
	elements, ok := key.Elements()
	if !ok || place.Ordinal >= uint32(len(elements)) {
		return "", fmt.Errorf("tuple element %d is out of range", place.Ordinal)
	}
	elem := elements[place.Ordinal]
	if wantBool && !isBool(snapshot, elem) {
		return "", fmt.Errorf("tuple element %d is not bool", place.Ordinal)
	}
	if !wantBool && !isWidth(snapshot, width, elem) {
		return "", fmt.Errorf("tuple element %d is not %s", place.Ordinal, wantName(width))
	}
	return fmt.Sprintf("%s._%d", expr, place.Ordinal), nil
}

// buildArrayPlaceRead lowers Load(CheckedIndexPlace) for an array or slice
// local. The index is built as an integer expression and checked with the
// runtime helper selected by the ENTRY width (width — the only width with a
// checked-index primitive) before it is used as the C subscript. For a slice
// base, the subscript uses .data and .len instead of the base array directly.
// The element value grammar is decided by the element's own type: bool for the
// buildBoolExpr path (wantBool), char, or any fixed-width integer at its own
// resolved width — the caller (buildExpr/buildBoolExpr/buildCharOperand) has
// already gated the Load's type against its consuming grammar, so this element
// check is defense for hand-built IR rather than a re-gate on the entry width.
func buildArrayPlaceRead(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	if len(place.Children) != 2 {
		return "", fmt.Errorf("CheckedIndexPlace wants two children")
	}
	baseExpr, arrayType, err := buildPlaceLValue(st, unit, snapshot, fileSet, place.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	// Check if the base is a slice-typed local.
	baseNode, ok := unit.Node(place.Children[0])
	if ok && (baseNode.Kind == tir.StoragePlace || isSlice(snapshot, arrayType)) {
		if info, declared := locals[baseNode.Symbol]; isSlice(snapshot, arrayType) || (declared && info.sliceType != 0) {
			sliceType := arrayType
			if sliceType == 0 {
				sliceType = info.sliceType
			}
			// Slice-typed base: use .data[checked_index(idx, (width_type).len)].
			sliceKey, ok := snapshot.Key(sliceType)
			if !ok {
				return "", fmt.Errorf("slice type %d is not in the type snapshot", sliceType)
			}
			element, ok := sliceKey.Child()
			if !ok {
				return "", fmt.Errorf("slice type %s has no element type", describeType(snapshot, info.sliceType))
			}
			if wantBool {
				if !isBool(snapshot, element) {
					return "", fmt.Errorf("slice element type is %s, want bool", describeType(snapshot, element))
				}
			} else if !isSupportedSliceElementType(unit, snapshot, element) {
				return "", fmt.Errorf("slice element type is %s, want a fixed-width integer, char, bool, str, tuple, optional, struct, or enum", describeType(snapshot, element))
			}
			indexNode, ok := unit.Node(place.Children[1])
			if !ok {
				return "", fmt.Errorf("slice index references invalid node %d", place.Children[1])
			}
			var index string
			if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
				if !isNonNegativeDecimal(indexNode.Literal.IntegerNum) {
					return "", fmt.Errorf("slice index contains an integer literal with malformed text %q", indexNode.Literal.IntegerNum)
				}
				index = indexNode.Literal.IntegerNum
			} else if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
				if name, ok := localOrGlobalName(st, indexNode.Symbol, locals); ok {
					index = name
				} else {
					return "", fmt.Errorf("slice index references symbol %d, which is not a local in scope", indexNode.Symbol)
				}
			} else if isUint(snapshot, indexNode.Type) {
				// A uint-typed slice index (the loop iterator from a
				// `loop 0..new_cap : i { ... }` whose bounds the checker
				// anchored to uint, or a uint-typed local used as an index):
				// built by the dedicated uint grammar, not the general
				// buildExpr path which rejects non-entry-width types.
				index, err = buildUintExpr(st, unit, snapshot, fileSet, place.Children[1], locals, width)
				if err != nil {
					return "", fmt.Errorf("slice index: %v", err)
				}
			} else {
				index, err = buildExpr(st, unit, snapshot, fileSet, place.Children[1], locals, width, width)
				if err != nil {
					return "", fmt.Errorf("slice index: %v", err)
				}
			}
			return fmt.Sprintf("%s.data[pebble_rt_checked_index_%s(%s, (%s)%s.len, %s)]", baseExpr, checkedSuffix(width), index, cType(width), baseExpr, buildSourceLoc(fileSet, place.Span)), nil
		}
	}
	// Array-typed base: original path.
	key, ok := snapshot.Key(arrayType)
	if !ok {
		return "", fmt.Errorf("array type %d is not in the type snapshot", arrayType)
	}
	length, element, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("checked index base is not an array")
	}
	if _, err := arrayLengthLiteral(length, width); err != nil {
		return "", err
	}
	if wantBool {
		if !isBool(snapshot, element) {
			return "", fmt.Errorf("array element type is %s, want bool", describeType(snapshot, element))
		}
	} else if !isStr(snapshot, element) && !isSupportedSliceElementType(unit, snapshot, element) {
		return "", fmt.Errorf("array element type is %s, want a fixed-width integer, char, bool, str, tuple, optional, or struct", describeType(snapshot, element))
	}
	indexNode, ok := unit.Node(place.Children[1])
	if !ok {
		return "", fmt.Errorf("array index references invalid node %d", place.Children[1])
	}
	var index string
	if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
		if !isNonNegativeDecimal(indexNode.Literal.IntegerNum) {
			return "", fmt.Errorf("array index contains an integer literal with malformed text %q", indexNode.Literal.IntegerNum)
		}
		index = indexNode.Literal.IntegerNum
	} else if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
		// An int-typed SymbolValue index can only be a range loop's iterator
		// referenced from inside its own body when the iterator is never used
		// in a width-anchoring position (the same unanchored-int case
		// buildComparisonOperand handles), and the iterator is always declared
		// in C at the entry's width, so its name is the correct C lvalue for
		// the subscript.
		if name, ok := localOrGlobalName(st, indexNode.Symbol, locals); ok {
			index = name
		} else {
			return "", fmt.Errorf("array index references symbol %d, which is not a local in scope", indexNode.Symbol)
		}
	} else if isUint(snapshot, indexNode.Type) {
		// A uint-typed array index (a uint-typed local or loop iterator):
		// built by the dedicated uint grammar, not the general buildExpr
		// path which rejects non-entry-width types.
		index, err = buildUintExpr(st, unit, snapshot, fileSet, place.Children[1], locals, width)
		if err != nil {
			return "", fmt.Errorf("array index: %v", err)
		}
	} else {
		var err error
		index, err = buildExpr(st, unit, snapshot, fileSet, place.Children[1], locals, width, width)
		if err != nil {
			return "", fmt.Errorf("array index: %v", err)
		}
	}
	literal, _ := arrayLengthLiteral(length, width)
	return fmt.Sprintf("%s[pebble_rt_checked_index_%s(%s, %s, %s)]", baseExpr, checkedSuffix(width), index, literal, buildSourceLoc(fileSet, place.Span)), nil
}

// buildSliceIndexValue builds the C text for a bare tir.CheckedIndex whose
// base is a slice-typed VALUE with no addressable place — the checker lowers
// foo()[i] (indexing a call's slice result directly) to a bare CheckedIndex
// because a call result has no place to address, exactly the reason str
// indexing lowers to a bare CheckedIndex rather than Load(CheckedIndexPlace),
// and indexing a method call's slice result inline inside a print statement is
// an ordinary operation real code hits. Unlike str indexing — whose element
// read is a stateless UTF-8 decode function, pebble_rt_str_char_at_<suffix>,
// safely callable on the base expression directly even repeatedly — a
// slice/array element read needs the base's .data pointer AND its .len (for
// the bounds check), so a freshly-computed base (a call result) must be
// materialized ONCE into a temp local before the read: evaluating the base
// twice (once for .len, once for .data) would run the underlying call twice —
// wrong (side effects run twice) and wasteful. The returned (pre, expr) pair
// follows the two-statement shape buildSliceConstruction established for
// exactly this "evaluate an expression once, then use its pieces multiple
// times" problem: pre is a leading temp-declaration statement the caller must
// place in its enclosing statement sequence before the expression (empty when
// the base is a pure projection safe to reference twice — a SymbolValue
// naming a slice-typed local, or a Load of a slice-typed place, both
// side-effect-free lvalues, the same reasoning the slice-of-slice re-slicing
// fix used), expr is the indexed element read.
//
// Exactly four base shapes are accepted:
//
//   - a SymbolValue naming a slice-typed local in scope — emitted as the
//     local's own pebble_local_<symbol> C name, no temp (a pure projection);
//   - a Load of a slice-typed place (a slice-typed struct field read off an
//     addressable receiver) — emitted as the place's own lvalue
//     (buildPlaceLValue), no temp;
//   - a DirectCall/MethodCall whose result type is a slice — built ONCE via
//     buildDirectCallWithPre into a temp local of the slice's own C type, then
//     read off the temp;
//   - a FieldValue reading a slice-typed field off a call result
//     (`make_bag().data[i]`) — the call receiver is built once and the field
//     read <receiver>.pebble_field_<member> materialized into the same temp.
//
// Anything else that resolves to a slice/array type (an array literal, an
// array-typed call result) is a clean rejection naming what was found — such a
// base is not buildable by buildExpr, so there is no way to materialize it
// without inventing a new array-temp lowering. The read is emitted as the same
// real runtime bounds check every other indexing path in this backend
// performs: <base>.data[pebble_rt_checked_index_<suffix>(<index>, (<cType>)
// <base>.len, <loc>)].
//
// The result's type is the slice's element type (node.Type), which the caller
// has already validated is what its context needs; wantBool selects the bool
// grammar exactly as buildArrayPlaceRead's wantBool does (a bool element is
// required), and any other element must satisfy isSupportedSliceElementType —
// the same restriction buildArrayPlaceRead and buildSliceConstruction enforce,
// never a new one. The index is built by the exact four-shape dispatch
// buildArrayPlaceRead and the CheckedIndexPlace path use (see
// buildSliceIndexOperand). id is the CheckedIndex node's own NodeID, used to
// name the temp (pebble_slice_index_<id>) so it can never collide with any
// other temp this backend emits. width is the entry's resolved integer width,
// used for the checked-index helper's suffix, the .len width cast, and the
// index's own width.
func buildSliceIndexValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, node tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, string, error) {
	if len(node.Children) != 2 {
		return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex with %d child(ren), want exactly two (the slice value being indexed and the index)", len(node.Children))
	}
	baseNode, ok := unit.Node(node.Children[0])
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", node.Children[0])
	}
	indexNode, ok := unit.Node(node.Children[1])
	if !ok {
		return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid index node %d", node.Children[1])
	}
	// Unwrap grouped-expression parens on the base (a SourceAlias): the
	// grouped base carries the same Type the SourceAlias did, so the dispatch
	// below is exactly what the checker validated.
	for baseNode.Kind == tir.SourceAlias {
		if len(baseNode.Children) != 1 {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex whose base SourceAlias has %d child(ren), want exactly one", len(baseNode.Children))
		}
		baseNode, ok = unit.Node(baseNode.Children[0])
		if !ok {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid base node %d", baseNode.Children[0])
		}
	}
	// Resolve the base's slice type and its C expression, plus the leading
	// temp-declaration statement a freshly-computed base needs.
	var sliceType types.TypeID
	var baseExpr, pre string
	switch baseNode.Kind {
	case tir.SymbolValue:
		info, declared := locals[baseNode.Symbol]
		if !declared || info.sliceType == 0 {
			return "", "", fmt.Errorf("entry function body expression indexes symbol %d, which is not a slice-typed local declared earlier in the body", baseNode.Symbol)
		}
		sliceType = info.sliceType
		baseExpr = fmt.Sprintf("pebble_local_%d", baseNode.Symbol)
	case tir.Load:
		if len(baseNode.Children) != 1 {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex whose base Load has %d child(ren), want exactly one place", len(baseNode.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, baseNode.Children[0], locals, width)
		if err != nil {
			return "", "", fmt.Errorf("entry function body expression slice-index base read: %v", err)
		}
		if !isSlice(snapshot, placeType) {
			return "", "", fmt.Errorf("entry function body expression indexes a Load of a place of type %s, want a slice-typed place", describeType(snapshot, placeType))
		}
		sliceType = placeType
		baseExpr = lvalue
	case tir.DirectCall, tir.MethodCall:
		// A slice-typed call result indexed directly (`view()[0]`,
		// `b.view()[1]`): the call is built ONCE into a temp local holding the
		// slice VALUE, and the bounds check plus element read run off the
		// temp's own .data/.len. The temp name derives from the CheckedIndex
		// node's own NodeID — the only stable identity in hand here (a bare
		// index has no local symbol to name it from), distinct from the
		// pebble_slice_start_<symbol> and pebble_slice_ret_<nodeID> temps so
		// the three can never collide.
		if !isSlice(snapshot, baseNode.Type) {
			return "", "", fmt.Errorf("entry function body expression indexes a %s whose result type is %s, want a slice-typed call result", baseNode.Kind, describeType(snapshot, baseNode.Type))
		}
		sliceType = baseNode.Type
		callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, baseNode, locals, width)
		if err != nil {
			return "", "", err
		}
		tempName := fmt.Sprintf("pebble_slice_index_%d", id)
		pre = fmt.Sprintf("%s %s = %s;", sliceTypeName(sliceType), tempName, callExpr)
		if callPre != "" {
			pre = callPre + "\n" + pre
		}
		baseExpr = tempName
	case tir.FieldValue:
		// A slice-typed struct field read off a call result
		// (`make_bag().data[i]`): the FieldValue's single child is the struct
		// receiver, and the field is read as <receiver>.pebble_field_<member> —
		// the same designated-field naming every other struct field this
		// backend emits uses (see buildStructFieldRead). The only supported
		// receiver is a DirectCall/MethodCall (the sole checker-reachable way
		// to obtain a non-addressable struct value; a struct LOCAL's field
		// indexes as a place and never reaches a bare CheckedIndex); the field
		// read off the call result is materialized into a temp because the
		// call would otherwise run twice (once for .len, once for .data).
		if !isSlice(snapshot, baseNode.Type) {
			return "", "", fmt.Errorf("entry function body expression indexes a FieldValue of type %s, want a slice-typed struct field", describeType(snapshot, baseNode.Type))
		}
		if len(baseNode.Children) != 1 {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex whose base FieldValue has %d child(ren), want exactly one (the struct receiver)", len(baseNode.Children))
		}
		receiver, ok := unit.Node(baseNode.Children[0])
		if !ok {
			return "", "", fmt.Errorf("entry function body expression contains a CheckedIndex referencing invalid receiver node %d", baseNode.Children[0])
		}
		if receiver.Kind != tir.DirectCall && receiver.Kind != tir.MethodCall {
			return "", "", fmt.Errorf("entry function body expression indexes a slice-typed struct field of a %s receiver, want a call result (a non-addressable struct value only arises from a call; a struct local's field indexes as a place)", receiver.Kind)
		}
		sliceType = baseNode.Type
		callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, receiver, locals, width)
		if err != nil {
			return "", "", err
		}
		tempName := fmt.Sprintf("pebble_slice_index_%d", id)
		pre = fmt.Sprintf("%s %s = (%s).pebble_field_%d;", sliceTypeName(sliceType), tempName, callExpr, baseNode.Member)
		if callPre != "" {
			pre = callPre + "\n" + pre
		}
		baseExpr = tempName
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional whose payload is an array or slice
		// used directly as the index base (`o![0]` where o is `?[N]T` or
		// `?[]T`). The payload has no by-value scalar to return through the
		// runtime's checked-unwrap helper family, so the unwrap is the same
		// GNU statement-expression value buildOptionalAggregateUnwrapExpr
		// produces (the presence-only check followed by the .value read) and
		// the element read indexes that value: an array base's wrapped
		// pebble_array_<typeID>_t .value indexes through its raw `.data`
		// member against the compile-time length, and a slice base's .value —
		// a pebble_slice_<typeID>_t header — indexes through `.data` against
		// its runtime `.len`, exactly as a slice local base does. The base
		// must be a stable optional local or optional-typed place (a
		// call-result base would need a temp this bare-Index position has
		// nowhere to place).
		unwrapExpr, err := buildOptionalAggregateUnwrapExpr(st, unit, snapshot, fileSet, baseNode, locals, width)
		if err != nil {
			return "", "", fmt.Errorf("entry function body expression indexes an optional force-unwrap: %v", err)
		}
		if isArray(snapshot, baseNode.Type) {
			arrayKey, ok := snapshot.Key(baseNode.Type)
			if !ok {
				return "", "", fmt.Errorf("entry function body expression indexes an array value whose type %d is not in the type snapshot", baseNode.Type)
			}
			length, element, ok := arrayKey.Array()
			if !ok {
				return "", "", fmt.Errorf("entry function body expression indexes a non-array unwrap of type %s", describeType(snapshot, baseNode.Type))
			}
			if wantBool {
				if !isBool(snapshot, element) {
					return "", "", fmt.Errorf("entry function body expression indexes an unwrapped array whose element type is %s, want bool", describeType(snapshot, element))
				}
			} else if !isSupportedSliceElementType(unit, snapshot, element) {
				return "", "", fmt.Errorf("entry function body expression indexes an unwrapped array whose element type is %s, want a fixed-width integer, char, bool, str, tuple, optional, struct, or enum", describeType(snapshot, element))
			}
			literal, err := arrayLengthLiteral(length, width)
			if err != nil {
				return "", "", err
			}
			index, err := buildSliceIndexOperand(st, unit, snapshot, fileSet, node.Children[1], indexNode, locals, width)
			if err != nil {
				return "", "", err
			}
			return "", fmt.Sprintf("%s.data[pebble_rt_checked_index_%s(%s, %s, %s)]", unwrapExpr, checkedSuffix(width), index, literal, buildSourceLoc(fileSet, node.Span)), nil
		}
		sliceKey, ok := snapshot.Key(baseNode.Type)
		if !ok {
			return "", "", fmt.Errorf("entry function body expression indexes a slice value whose type %d is not in the type snapshot", baseNode.Type)
		}
		element, ok := sliceKey.Child()
		if !ok {
			return "", "", fmt.Errorf("entry function body expression indexes an unwrapped slice of type %s, which has no element type", describeType(snapshot, baseNode.Type))
		}
		if wantBool {
			if !isBool(snapshot, element) {
				return "", "", fmt.Errorf("entry function body expression indexes an unwrapped slice whose element type is %s, want bool", describeType(snapshot, element))
			}
		} else if !isSupportedSliceElementType(unit, snapshot, element) {
			return "", "", fmt.Errorf("entry function body expression indexes an unwrapped slice whose element type is %s, want a fixed-width integer, char, bool, str, tuple, optional, struct, or enum", describeType(snapshot, element))
		}
		index, err := buildSliceIndexOperand(st, unit, snapshot, fileSet, node.Children[1], indexNode, locals, width)
		if err != nil {
			return "", "", err
		}
		read := fmt.Sprintf("%s.data[pebble_rt_checked_index_%s(%s, (%s)%s.len, %s)]", unwrapExpr, checkedSuffix(width), index, cType(width), unwrapExpr, buildSourceLoc(fileSet, node.Span))
		return "", read, nil
	default:
		return "", "", fmt.Errorf("entry function body expression indexes a %s of type %s, want a slice-typed value (a slice-typed local, a slice-typed place, a call returning a slice, or a slice-typed field of a call result); indexing an array literal or array-typed call result directly is not lowered", baseNode.Kind, describeType(snapshot, baseNode.Type))
	}
	sliceKey, ok := snapshot.Key(sliceType)
	if !ok {
		return "", "", fmt.Errorf("entry function body expression indexes a slice value whose type %d is not in the type snapshot", sliceType)
	}
	element, ok := sliceKey.Child()
	if !ok {
		return "", "", fmt.Errorf("entry function body expression indexes a slice value of type %s, which has no element type", describeType(snapshot, sliceType))
	}
	if wantBool {
		if !isBool(snapshot, element) {
			return "", "", fmt.Errorf("entry function body expression indexes a slice whose element type is %s, want bool", describeType(snapshot, element))
		}
	} else if !isSupportedSliceElementType(unit, snapshot, element) {
		return "", "", fmt.Errorf("entry function body expression indexes a slice whose element type is %s, want a fixed-width integer, char, bool, str, tuple, optional, struct, or enum", describeType(snapshot, element))
	}
	index, err := buildSliceIndexOperand(st, unit, snapshot, fileSet, node.Children[1], indexNode, locals, width)
	if err != nil {
		return "", "", err
	}
	read := fmt.Sprintf("%s.data[pebble_rt_checked_index_%s(%s, (%s)%s.len, %s)]", baseExpr, checkedSuffix(width), index, cType(width), baseExpr, buildSourceLoc(fileSet, node.Span))
	return pre, read, nil
}

// buildSliceIndexOperand builds the C text for one slice-index expression, the
// index child of a bare CheckedIndex, using the exact four-shape dispatch
// buildArrayPlaceRead and the CheckedIndexPlace path use (the same dispatch
// this file's str-indexing case also mirrors): an int-typed IntegerLiteral is
// emitted as its decimal text, an int-typed SymbolValue (a range loop's
// iterator referenced directly — the unanchored-int case, always declared at
// the entry's width) as its pebble_local_<symbol> C name, a uint-typed index
// via the dedicated buildUintExpr grammar (the general buildExpr path rejects
// a uint-typed value), and anything else via buildExpr.
func buildSliceIndexOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, indexID tir.NodeID, indexNode tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
		if !isNonNegativeDecimal(indexNode.Literal.IntegerNum) {
			return "", fmt.Errorf("slice index contains an integer literal with malformed text %q", indexNode.Literal.IntegerNum)
		}
		return indexNode.Literal.IntegerNum, nil
	}
	if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
		if name, ok := localOrGlobalName(st, indexNode.Symbol, locals); ok {
			return name, nil
		}
		return "", fmt.Errorf("slice index references symbol %d, which is not a local in scope", indexNode.Symbol)
	}
	if isUint(snapshot, indexNode.Type) {
		index, err := buildUintExpr(st, unit, snapshot, fileSet, indexID, locals, width)
		if err != nil {
			return "", fmt.Errorf("slice index: %v", err)
		}
		return index, nil
	}
	index, err := buildExpr(st, unit, snapshot, fileSet, indexID, locals, width, width)
	if err != nil {
		return "", fmt.Errorf("slice index: %v", err)
	}
	return index, nil
}

// buildStructFieldRead builds the C text for reading one field of a struct
// local through the Load(FieldPlace) shape the checker actually produces for
// `point.x` (confirmed against a real fixture): the FieldPlace carries the
// field's own member symbol in Member and its single child is the StoragePlace
// naming the struct local. wantBool selects which grammar the field must
// satisfy — bool (the buildBoolExpr path) or any fixed-width integer (uint,
// u64, the entry's width, or any other — each carried by the field's own C
// type). The field's own type is resolved from the struct's declared fields by
// matching FieldPlace.Member (see declaredFieldType), not assumed from the
// place's own Type. The emitted C is
// pebble_local_<symbol>.pebble_field_<member>. Since Slice C, a FieldPlace
// whose receiver is a TAGGED UNION and whose Member is one of its constructed
// variants (the narrowed union-variant payload read `self.Ok` inside `case
// .Ok:`) is lowered instead to
// pebble_local_<symbol>.payload.pebble_field_<member> — the exact projection
// the union's construction side fills (see buildUnionConstruction).
func buildStructFieldRead(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	baseExpr, structType, err := buildPlaceLValue(st, unit, snapshot, fileSet, place.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	access := "."
	if key, found := snapshot.Key(structType); found && key.Kind() == types.Pointer {
		pointee, childOK := key.Child()
		if !childOK {
			return "", fmt.Errorf("field read pointer has no pointee")
		}
		structType = pointee
		access = "->"
	}
	if place.Member == tir.StructuralFieldLen || place.Member == tir.StructuralFieldData || place.Member == tir.StructuralFieldHasValue {
		name := "len"
		if place.Member == tir.StructuralFieldData {
			name = "data"
		} else if place.Member == tir.StructuralFieldHasValue {
			name = "has_value"
		}
		key, found := snapshot.Key(structType)
		if !found {
			return "", fmt.Errorf("structural field receiver type %d is not in the type snapshot", structType)
		}
		if key.Kind() == types.Slice && (name == "len" || name == "data") {
			return baseExpr + access + name, nil
		}
		if name == "len" {
			if builtin, ok := key.Builtin(); ok && builtin == types.Str {
				return baseExpr + access + "len", nil
			}
		}
		if name == "has_value" && key.Kind() == types.Optional {
			return baseExpr + access + name, nil
		}
		return "", fmt.Errorf("unsupported structural field %s", name)
	}
	// A narrowed union-variant payload read (`self.Ok` inside `case .Ok:`,
	// the checker's Slice A acceptance): the Member is one of the receiver's
	// tagged-union variants, not a real struct field. This must come BEFORE
	// declaredFieldType below, whose member lookup would MIS-report a variant
	// as a field — a TypeDeclaration's Members list carries variants and its
	// MemberTypes the variant's payload type, so the lookup would succeed with
	// the wrong kind of member and emit a pebble_field_<member> projection no
	// real struct field satisfies. The payload lives in the union typedef's
	// .payload union under pebble_field_<member> — the exact member the
	// construction side designates (see buildUnionConstruction) — so the read
	// is the same projection the tag-matched construction wrote:
	// base.payload.pebble_field_<member> (with the same . / -> access a
	// pointer receiver resolves). The read's value type is the variant's
	// payload type, already resolved onto the place node's own Type by the
	// checker — the C type of the emitted projection is the payload member's
	// declared C type (unionMemberCType), which agrees.
	if unionVariantPayloadMember(unit, snapshot, structType, place.Member) {
		return fmt.Sprintf("%s%spayload.pebble_field_%d", baseExpr, access, place.Member), nil
	}
	fieldType, ok := declaredFieldType(unit, snapshot, structType, place.Member)
	if runtimeType(unit, snapshot, structType) != 0 {
		fieldType = place.Type
		ok = true
	}
	if !ok {
		return "", fmt.Errorf("field %d is not declared", place.Member)
	}
	if runtimeType(unit, snapshot, structType) != 0 {
		field, found := runtimeFieldName(unit, structType, place.Member)
		if !found {
			return "", fmt.Errorf("runtime field %d is not declared", place.Member)
		}
		return fmt.Sprintf("%s%s%s", baseExpr, access, field), nil
	}
	// Reading a field whose OWN type is a compiler-builtin runtime type (e.g.
	// `let a = holder.backing;`, where backing is Allocator): the C field is
	// declared with its hand-written C type (PebbleAllocator, see
	// structFieldCType), so the read is exactly the ordinary C field projection
	// with no width/bool coercion — the runtime value's type matches the C
	// field type directly. Without this case the width/bool check below would
	// reject the field.
	if runtimeType(unit, snapshot, fieldType) != 0 {
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	if wantBool {
		if !isBool(snapshot, fieldType) {
			return "", fmt.Errorf("field %d has type %s, want bool", place.Member, describeType(snapshot, fieldType))
		}
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	// A fixed-width integer field of ANY width, not just the ambient entry's
	// own: the C field is declared at the field's own resolved width (see
	// structFieldCType — a uint or u64 field is uint64_t), so the projection
	// pebble_local_<sym>.pebble_field_<member> carries the field's own C type
	// and is valid in a surrounding context of that same width. This mirrors
	// the generic resolvedBuiltin/cType widening already applied to optional
	// payloads (d737242), slice elements, function-type parameters/results,
	// and the struct-field typedef itself.
	if fieldWidth, integerField := resolvedBuiltin(snapshot, fieldType); integerField && cType(fieldWidth) != "" {
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	if isPointer(snapshot, fieldType) {
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	if isEnumType(unit, snapshot, fieldType) {
		// A plain enum-typed field (`entry.state`): the C field is declared at
		// the field's own pebble_enum_<typeID>_t (see structFieldCType), whose
		// C representation is just the variant's ordinal (a plain C enum
		// value), so the read is exactly the ordinary C field projection with
		// no width/bool coercion — the same direct projection the pointer case
		// two lines above uses, and the shape buildComparison's enum branch
		// consumes as an enum comparison operand.
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	if isStr(snapshot, fieldType) {
		return fmt.Sprintf("%s%spebble_field_%d", baseExpr, access, place.Member), nil
	}
	return "", fmt.Errorf("field %d has type %s, want a fixed-width integer, bool, pointer, or enum, or str", place.Member, describeType(snapshot, fieldType))
}

// buildStructValueNode builds the C expression text for a struct-typed VALUE
// (an rvalue, never a place) directly from a TIR node — the shape family a
// non-addressable struct value takes: a struct-typed call result (`mk()`), a
// struct-typed field read off another value (`h.p`, `mk().inner`), a
// force-unwrap of a struct-payload optional (`sp!`), a whole-struct read of an
// array/slice element or tuple ordinal, a struct literal, a struct-typed local
// reference, a dereference read, or a bare CheckedIndex over a slice-typed
// call result (`mk()[i]`). It is the value-position counterpart of
// buildPlaceLValue, and the one builder both a struct-typed METHOD RECEIVER /
// call argument (buildAggregateArgument) and a struct-typed FIELD READ's
// receiver (buildStructFieldValueRead) route through. Every shape is a pure
// expression: a receiver that would need a temp-declaration statement (a
// slice-typed call result being indexed — the one case buildSliceIndexValue
// materializes) is folded into a GNU statement-expression primary expression,
// exactly as buildStructValueExpr and buildSliceArgument already fold inline
// constructions. The returned TypeID is the struct-typed value's own type (the
// call result type for a call, the resolved element type for a Load/CheckedIndex,
// the unwrap payload for a force-unwrap), for the caller's type validation.
func buildStructValueNode(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, types.TypeID, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", 0, fmt.Errorf("struct value references invalid node %d", id)
	}
	switch node.Kind {
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || info.structType == 0 {
			return "", 0, fmt.Errorf("struct value node references symbol %d, which is not a struct-typed local declared earlier in the body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), info.structType, nil
	case tir.RecordConstruct:
		if isUntaggedUnionType(unit, snapshot, node.Type) {
			// An untagged-union construction used in a value position (a call
			// argument for an untagged-union-typed parameter, a whole-union
			// read): emitted as the union's own `typedef union { ... }`
			// compound literal, never the struct compound literal a struct
			// construction would build.
			expr, err := buildUntaggedUnionValueExpr(st, unit, snapshot, fileSet, node, locals, "union value", width)
			if err != nil {
				return "", 0, err
			}
			return expr, node.Type, nil
		}
		expr, err := buildStructValueExpr(st, unit, snapshot, fileSet, node, locals, "struct value", width)
		if err != nil {
			return "", 0, err
		}
		return expr, node.Type, nil
	case tir.ContextValue:
		return "(*ctx)", node.Type, nil
	case tir.DirectCall, tir.MethodCall:
		callExpr, err := buildDirectCallNested(st, unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", 0, err
		}
		return callExpr, node.Type, nil
	case tir.Load:
		if len(node.Children) != 1 {
			return "", 0, fmt.Errorf("struct value Load has %d child(ren), want exactly one place", len(node.Children))
		}
		lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], locals, width)
		if err != nil {
			return "", 0, err
		}
		return lvalue, elementType, nil
	case tir.CheckedOptionalUnwrap:
		unwrapExpr, err := buildOptionalAggregateUnwrapExpr(st, unit, snapshot, fileSet, node, locals, width)
		if err != nil {
			return "", 0, err
		}
		return unwrapExpr, node.Type, nil
	case tir.FieldValue:
		fieldExpr, err := buildStructFieldValueRead(st, unit, snapshot, fileSet, id, locals, width, false, true)
		if err != nil {
			return "", 0, err
		}
		return fieldExpr, node.Type, nil
	case tir.CheckedIndex:
		pre, read, err := buildSliceIndexValue(st, unit, snapshot, fileSet, id, node, locals, width, false)
		if err != nil {
			return "", 0, err
		}
		if pre != "" {
			return sliceConstructionStatementExpr(pre, read), node.Type, nil
		}
		return read, node.Type, nil
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return "", 0, fmt.Errorf("struct value SourceAlias has %d child(ren), want exactly one", len(node.Children))
		}
		return buildStructValueNode(st, unit, snapshot, fileSet, node.Children[0], locals, width)
	default:
		return "", 0, fmt.Errorf("struct value node is a %s, want a struct-typed local reference, a struct literal (a RecordConstruct), a call to a struct-returning helper, a whole-struct read of a place or aggregate element, a force-unwrap of a struct-payload optional, or a field read off any of those", node.Kind)
	}
}

// buildStructFieldValueRead builds the C text for reading one field of a
// struct VALUE (a tir.FieldValue: the shape a non-addressable struct receiver
// — a call result, a field read, a force-unwrap, an aggregate element read —
// produces, as opposed to Load(FieldPlace), the shape a struct LOCAL's field
// read uses; the checker lowers `mk().x`, `sp!.x`, `mk().inner.x`,
// `a[i].x`-off-a-call, etc. to a FieldValue whose single child is the struct
// value). The receiver is built by buildStructValueNode and the field is
// projected with a `.` member access (a struct value is never a pointer), and
// the field's own type is resolved and validated: wantBool selects the bool
// grammar (mirroring buildStructFieldRead's wantBool), wantStruct accepts a
// struct/aggregate-typed field (the whole-struct read shapes — a struct field
// as a method receiver, whole-struct local initializer, or another field's
// receiver), and otherwise any fixed-width integer, uint, float, pointer, enum,
// or str field is accepted — the caller's own grammar has already gated the
// FieldValue's own Type (the field type) to what it accepts. Structural
// members (`.len`/`.data`/`.has_value` off a slice/str/optional call result)
// and a narrowed tagged-union variant payload member are handled first, exactly
// as buildStructFieldRead handles them for a place.
func buildStructFieldValueRead(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool, wantStruct bool) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("struct field value read references invalid node %d", id)
	}
	if node.Kind != tir.FieldValue || len(node.Children) != 1 {
		return "", fmt.Errorf("struct field value read wants a FieldValue with exactly one receiver child")
	}
	receiverExpr, structType, err := buildStructValueNode(st, unit, snapshot, fileSet, node.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	if node.Member == tir.StructuralFieldLen || node.Member == tir.StructuralFieldData || node.Member == tir.StructuralFieldHasValue {
		name := "len"
		if node.Member == tir.StructuralFieldData {
			name = "data"
		} else if node.Member == tir.StructuralFieldHasValue {
			name = "has_value"
		}
		key, found := snapshot.Key(structType)
		if !found {
			return "", fmt.Errorf("structural field receiver type %d is not in the type snapshot", structType)
		}
		if key.Kind() == types.Slice && (name == "len" || name == "data") {
			return fmt.Sprintf("(%s).%s", receiverExpr, name), nil
		}
		if name == "len" {
			if builtin, ok := key.Builtin(); ok && builtin == types.Str {
				return fmt.Sprintf("(%s).%s", receiverExpr, "len"), nil
			}
		}
		if name == "has_value" && key.Kind() == types.Optional {
			return fmt.Sprintf("(%s).%s", receiverExpr, name), nil
		}
		return "", fmt.Errorf("unsupported structural field %s", name)
	}
	if unionVariantPayloadMember(unit, snapshot, structType, node.Member) {
		return fmt.Sprintf("(%s).payload.pebble_field_%d", receiverExpr, node.Member), nil
	}
	fieldType, ok := declaredFieldType(unit, snapshot, structType, node.Member)
	if runtimeType(unit, snapshot, structType) != 0 {
		fieldType = node.Type
		ok = true
	}
	if !ok {
		return "", fmt.Errorf("field %d is not declared", node.Member)
	}
	if runtimeType(unit, snapshot, structType) != 0 {
		field, found := runtimeFieldName(unit, structType, node.Member)
		if !found {
			return "", fmt.Errorf("runtime field %d is not declared", node.Member)
		}
		return fmt.Sprintf("(%s).%s", receiverExpr, field), nil
	}
	if runtimeType(unit, snapshot, fieldType) != 0 {
		return fmt.Sprintf("(%s).pebble_field_%d", receiverExpr, node.Member), nil
	}
	if wantBool {
		if !isBool(snapshot, fieldType) {
			return "", fmt.Errorf("field %d has type %s, want bool", node.Member, describeType(snapshot, fieldType))
		}
		return fmt.Sprintf("(%s).pebble_field_%d", receiverExpr, node.Member), nil
	}
	if wantStruct {
		// A whole struct/aggregate-typed field read (a struct field as a
		// method receiver, whole-struct local initializer, return value, or
		// another field's receiver). A nominal type is a struct unless its
		// declared members are enum variants; isDefinitelyEnumType (no
		// no-evidence fallback) excludes both a plain enum AND a tagged union
		// from the struct branch — the tagged union is accepted via its own
		// predicate below.
		if isTuple(snapshot, fieldType) || isTaggedUnionType(unit, snapshot, fieldType) || (isStruct(snapshot, fieldType) && !isDefinitelyEnumType(unit, snapshot, fieldType)) {
			return fmt.Sprintf("(%s).pebble_field_%d", receiverExpr, node.Member), nil
		}
		return "", fmt.Errorf("field %d has type %s, want a struct-typed field", node.Member, describeType(snapshot, fieldType))
	}
	if fieldWidth, integerField := resolvedBuiltin(snapshot, fieldType); integerField && cType(fieldWidth) != "" {
		return fmt.Sprintf("(%s).pebble_field_%d", receiverExpr, node.Member), nil
	}
	if isFloat(snapshot, fieldType) {
		return fmt.Sprintf("(%s).pebble_field_%d", receiverExpr, node.Member), nil
	}
	if isPointer(snapshot, fieldType) {
		return fmt.Sprintf("(%s).pebble_field_%d", receiverExpr, node.Member), nil
	}
	if isEnumType(unit, snapshot, fieldType) {
		return fmt.Sprintf("(%s).pebble_field_%d", receiverExpr, node.Member), nil
	}
	if isStr(snapshot, fieldType) {
		return fmt.Sprintf("(%s).pebble_field_%d", receiverExpr, node.Member), nil
	}
	return "", fmt.Errorf("field %d has type %s, want a fixed-width integer, bool, pointer, or enum, or str", node.Member, describeType(snapshot, fieldType))
}

// buildDereferencePlaceRead builds the C text for reading through a
// DereferencePlace: `*pebble_rt_checked_deref_ptr(<ptr_expr>, <loc>)`. The
// pointer expression is built by buildExpr, the null check is performed by the
// runtime primitive, and the dereference produces the pointee value. wantBool
// controls whether the caller expects a bool-typed result (for an `if *b` where
// b is *bool) — the C dereference of a bool pointer yields a C bool directly.
func buildDereferencePlaceRead(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, loadSpan source.Span, wantBool bool) (string, error) {
	if len(place.Children) != 1 {
		return "", fmt.Errorf("dereference place wants one child")
	}
	ptrExpr, err := buildExpr(st, unit, snapshot, fileSet, place.Children[0], locals, width, width)
	if err != nil {
		return "", fmt.Errorf("dereference pointer expression: %v", err)
	}
	checkedPtr := fmt.Sprintf("pebble_rt_checked_deref_ptr(%s, %s)", ptrExpr, buildSourceLoc(fileSet, loadSpan))
	// place.Type is already the pointee type, not the pointer type — a
	// DereferencePlace's own Type is what dereferencing produces (confirmed
	// via place_facts.go's deriveDereferencePlace, whose result is the
	// dereferenced value), the same reason it passes buildExpr's width gate
	// unmodified for a width-typed pointee.
	pointeeTypeID := place.Type
	pointeeCType := pointerTypeNameForUnit(st, unit, snapshot, pointeeTypeID)
	if pointeeCType == "" {
		return "", fmt.Errorf("dereference place has unsupported pointee type %s", describeType(snapshot, pointeeTypeID))
	}
	castExpr := fmt.Sprintf("*(%s)(%s)", pointeeCType, checkedPtr)
	if isArray(snapshot, pointeeTypeID) {
		// An array pointee dereferences to the wrapped pebble_array_<id>_t
		// struct; the by-value whole-array read projects the struct's raw
		// `.data` array member (the same projection buildPlaceLValue applies),
		// so the value is `(*(pebble_array_<id>_t *)(checked)).data` — the
		// parens keep the `.data` projection on the WHOLE dereference, since C
		// postfix `.` binds tighter than the unary `*`/cast.
		castExpr = fmt.Sprintf("(%s).data", castExpr)
	}
	if wantBool {
		if !isBool(snapshot, pointeeTypeID) {
			return "", fmt.Errorf("dereference read wants bool but pointee is %s", describeType(snapshot, pointeeTypeID))
		}
	}
	return castExpr, nil
}

func buildPlaceLValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, types.TypeID, error) {
	n, ok := unit.Node(id)
	if !ok {
		return "", 0, fmt.Errorf("place %d is invalid", id)
	}
	switch n.Kind {
	case tir.StoragePlace:
		info, ok := locals[n.Symbol]
		if !ok {
			return "", 0, fmt.Errorf("symbol %d is not a local", n.Symbol)
		}
		var typ types.TypeID
		switch {
		case info.tuple != 0:
			typ = info.tuple
		case info.array != 0:
			typ = info.array
		case info.optional != 0:
			typ = info.optional
		case info.structType != 0:
			typ = info.structType
		case info.sliceType != 0:
			typ = info.sliceType
		case info.pointerType != 0:
			typ = info.pointerType
		case info.runtimeType != 0:
			typ = info.runtimeType
		default:
			// A scalar local (int, bool, char, str). buildPlaceLValue is
			// only called for address-of and aggregate field/element access;
			// for scalars the node's own Type is the correct types.TypeID.
			typ = n.Type
		}
		base := fmt.Sprintf("pebble_local_%d", n.Symbol)
		if info.array != 0 && info.arrayWrapped {
			base += ".data"
		}
		return base, typ, nil
	case tir.TuplePlace:
		if len(n.Children) != 1 {
			return "", 0, fmt.Errorf("tuple place wants one base")
		}
		base, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, n.Children[0], locals, width)
		if err != nil {
			return "", 0, err
		}
		key, ok := snapshot.Key(typ)
		if !ok {
			return "", 0, fmt.Errorf("tuple type missing")
		}
		elems, ok := key.Elements()
		if !ok || n.Ordinal >= uint32(len(elems)) {
			return "", 0, fmt.Errorf("tuple element out of range")
		}
		return fmt.Sprintf("%s._%d", base, n.Ordinal), elems[n.Ordinal], nil
	case tir.FieldPlace:
		if len(n.Children) != 1 {
			return "", 0, fmt.Errorf("field place wants one base")
		}
		base, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, n.Children[0], locals, width)
		if err != nil {
			return "", 0, err
		}
		access := "."
		if key, found := snapshot.Key(typ); found && key.Kind() == types.Pointer {
			pointee, childOK := key.Child()
			if !childOK {
				return "", 0, fmt.Errorf("field place pointer has no pointee")
			}
			typ = pointee
			access = "->"
		}
		if n.Member == tir.StructuralFieldLen || n.Member == tir.StructuralFieldData || n.Member == tir.StructuralFieldHasValue {
			// A structural field — `.len`/`.data` on a slice, `.len` on a str,
			// or `.has_value` on an optional — is a sentinel member symbol (see
			// tir.StructuralFieldLen et al.), never a real struct field index,
			// so it must be projected before declaredFieldType would reject it
			// as undeclared. This mirrors buildStructFieldRead's handling of
			// the same sentinels; the `.len`/`.data`/`.has_value` field of the
			// underlying runtime aggregate is projected with the same `.` or
			// `->` access already resolved for a pointer receiver.
			name := "len"
			if n.Member == tir.StructuralFieldData {
				name = "data"
			} else if n.Member == tir.StructuralFieldHasValue {
				name = "has_value"
			}
			key, found := snapshot.Key(typ)
			if !found {
				return "", 0, fmt.Errorf("structural field receiver type %d is not in the type snapshot", typ)
			}
			if key.Kind() == types.Slice && (name == "len" || name == "data") {
				// `.len`/`.data` on a slice: the projection's type is the
				// resolved uint / pointer-to-element the checker assigned the
				// field access itself (n.Type), so a Load of it routes through
				// buildUintExpr / the pointer grammar by its own type.
				return fmt.Sprintf("%s%s%s", base, access, name), n.Type, nil
			}
			if name == "len" {
				if builtin, ok := key.Builtin(); ok && builtin == types.Str {
					return fmt.Sprintf("%s%s%s", base, access, "len"), n.Type, nil
				}
			}
			if name == "has_value" && key.Kind() == types.Optional {
				return fmt.Sprintf("%s%s%s", base, access, name), snapshot.Builtins().Bool, nil
			}
			return "", 0, fmt.Errorf("unsupported structural field %s", name)
		}
		// A narrowed union-variant payload projection (`self.Ok` inside `case
		// .Ok:`, Slice A) used as an lvalue — the write-side twin of
		// buildStructFieldRead's read case (set_error's `self.Err = error` on
		// a *Result receiver lowers to a Store whose place is a FieldPlace of
		// the Err variant). The variant's payload lives in the union typedef's
		// .payload union under pebble_field_<member>, so the lvalue is the
		// exact projection the construction side fills. This must come before
		// declaredFieldType, whose member lookup would MIS-report a variant as
		// a real field (a TypeDeclaration's Members carries variants). The
		// lvalue's type is the variant's payload type, already resolved onto
		// the place node's own Type by the checker.
		if unionVariantPayloadMember(unit, snapshot, typ, n.Member) {
			return fmt.Sprintf("%s%spayload.pebble_field_%d", base, access, n.Member), n.Type, nil
		}
		ft, ok := declaredFieldType(unit, snapshot, typ, n.Member)
		if !ok {
			return "", 0, fmt.Errorf("field %d is not declared", n.Member)
		}
		if field, ok := runtimeFieldName(unit, typ, n.Member); ok {
			return fmt.Sprintf("%s%s%s", base, access, field), ft, nil
		}
		if isArray(snapshot, ft) {
			// A fixed-array-typed struct field's C declaration is the array's
			// OWN typedef (pebble_array_<typeID>_t, a struct wrapping a
			// `elem data[length]` member — see structFieldCType /
			// buildArrayTypedefs), so the field's addressable array lvalue is
			// the typedef struct's `.data` member, `X.pebble_field_<m>.data` —
			// exactly the `.data` projection the WRAPPED array local case two
			// cases above applies (info.arrayWrapped appends `.data`), and the
			// same raw-array lvalue the array element read and write paths
			// subscript. The returned type stays the array type: callers
			// dispatch on it, never on the projected member.
			return fmt.Sprintf("%s%spebble_field_%d.data", base, access, n.Member), ft, nil
		}
		return fmt.Sprintf("%s%spebble_field_%d", base, access, n.Member), ft, nil
	case tir.CheckedIndexPlace:
		if len(n.Children) != 2 {
			return "", 0, fmt.Errorf("index place wants two children")
		}
		base, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, n.Children[0], locals, width)
		if err != nil {
			return "", 0, err
		}
		indexNode, ok := unit.Node(n.Children[1])
		if !ok {
			return "", 0, fmt.Errorf("invalid array index")
		}
		idx := ""
		if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
			idx = indexNode.Literal.IntegerNum
		} else if indexNode.Kind == tir.SymbolValue && indexNode.Type == snapshot.Builtins().Int {
			if name, ok := localOrGlobalName(st, indexNode.Symbol, locals); ok {
				idx = name
			} else {
				return "", 0, fmt.Errorf("symbol %d is not a local in scope", indexNode.Symbol)
			}
		} else if isUint(snapshot, indexNode.Type) {
			// A uint-typed index (`self.entries[index]` where index is a uint
			// local, or the uint-typed loop iterator `new_entries[i]` from a
			// `loop 0..new_cap : i { ... }` whose bounds the checker anchored
			// to uint): the index flows through the dedicated buildUintExpr
			// grammar, exactly as every other uint value position routes —
			// the general buildExpr path's entry-width gate would reject a
			// uint-typed value with "of type uint, want <entry width>".
			idx, err = buildUintExpr(st, unit, snapshot, fileSet, n.Children[1], locals, width)
			if err != nil {
				return "", 0, err
			}
		} else {
			idx, err = buildExpr(st, unit, snapshot, fileSet, n.Children[1], locals, width, width)
			if err != nil {
				return "", 0, err
			}
		}
		if isSlice(snapshot, typ) {
			// A slice-typed base: use .data[checked_index(idx, (width_type).len)].
			sliceKey, ok := snapshot.Key(typ)
			if !ok {
				return "", 0, fmt.Errorf("slice type %d is not in the type snapshot", typ)
			}
			elem, ok := sliceKey.Child()
			if !ok {
				return "", 0, fmt.Errorf("slice type has no element type")
			}
			return fmt.Sprintf("%s.data[pebble_rt_checked_index_%s(%s, (%s)%s.len, %s)]", base, checkedSuffix(width), idx, cType(width), base, buildSourceLoc(fileSet, n.Span)), elem, nil
		}
		key, ok := snapshot.Key(typ)
		if !ok {
			return "", 0, fmt.Errorf("array type missing")
		}
		length, elem, ok := key.Array()
		if !ok {
			return "", 0, fmt.Errorf("index base is not an array")
		}
		lit, _ := arrayLengthLiteral(length, width)
		if isArray(snapshot, elem) {
			// A nested array element — the indexed element is itself a fixed
			// array. Its C type is the inner array's own pebble_array_<innerID>_t
			// WRAPPER struct (see arrayElementCType's array case), so the
			// addressable array lvalue a SUBSEQUENT index (or a whole-array
			// read/write) wants is the element struct's raw `.data` member —
			// `base[i].data` — exactly the `.data` projection an array-typed
			// struct field applies (see the FieldPlace case above). The
			// returned type stays the array type: callers dispatch on it.
			return fmt.Sprintf("%s[pebble_rt_checked_index_%s(%s, %s, %s)].data", base, checkedSuffix(width), idx, lit, buildSourceLoc(fileSet, n.Span)), elem, nil
		}
		return fmt.Sprintf("%s[pebble_rt_checked_index_%s(%s, %s, %s)]", base, checkedSuffix(width), idx, lit, buildSourceLoc(fileSet, n.Span)), elem, nil
	case tir.DereferencePlace:
		// A dereference place: `*p` used as a write target (`*p = x;`). The
		// child is the pointer expression. The emitted C builds the pointer,
		// runs it through pebble_rt_checked_deref_ptr for null checking, and
		// produces `(*<checked_ptr>)` as the lvalue — wrapped in parentheses
		// so a later postfix projection (a `.field` member read like
		// `(*p).has_value`, a `[i]` element read like `(*p)[i]`, or the
		// `.value`/`.has_value` fields a force-unwrap reads) binds to the
		// WHOLE dereference, not to the checked-pointer call result: C's
		// postfix `.` and `[]` bind tighter than the unary `*`/cast, so a
		// bare `*(<type> *)(pebble_rt_checked_deref_ptr(...)).field` would
		// project the field off the void* call result, not off the
		// dereferenced value.
		if len(n.Children) != 1 {
			return "", 0, fmt.Errorf("dereference place wants one child")
		}
		ptrExpr, err := buildExpr(st, unit, snapshot, fileSet, n.Children[0], locals, width, width)
		if err != nil {
			return "", 0, fmt.Errorf("dereference pointer expression: %v", err)
		}
		checkedPtr := fmt.Sprintf("pebble_rt_checked_deref_ptr(%s, %s)", ptrExpr, buildSourceLoc(fileSet, n.Span))
		// n.Type is already the pointee type, not the pointer type (see the
		// matching comment in buildDereferencePlaceRead).
		pointeeTypeID := n.Type
		pointeeCType := pointerTypeNameForUnit(st, unit, snapshot, pointeeTypeID)
		if pointeeCType == "" {
			return "", 0, fmt.Errorf("dereference place has unsupported pointee type %s", describeType(snapshot, pointeeTypeID))
		}
		castExpr := fmt.Sprintf("(*(%s)(%s))", pointeeCType, checkedPtr)
		if isArray(snapshot, pointeeTypeID) {
			// An array pointee dereferences to the wrapped pebble_array_<id>_t
			// struct; the lvalue an element-index, whole-array write, or
			// whole-array read wants is the struct's raw `.data` array member
			// (the same projection buildPlaceLValue applies to an array-typed
			// struct field), so it is appended here before any postfix index.
			castExpr += ".data"
		}
		return castExpr, pointeeTypeID, nil
	}
	return "", 0, fmt.Errorf("place base %s is unsupported", n.Kind)
}

// unionVariantPayloadStoreTarget returns the payload lvalue and tag lvalue for
// a direct write to a tagged-union variant payload. The ordinary place builder
// must remain tag-free because it is also used for reads; Store emission uses
// this companion result to update the discriminant in the same C expression.
func unionVariantPayloadStoreTarget(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, string, bool, error) {
	if place.Kind != tir.FieldPlace || len(place.Children) != 1 {
		return "", "", false, nil
	}
	base, typ, err := buildPlaceLValue(st, unit, snapshot, fileSet, place.Children[0], locals, width)
	if err != nil {
		return "", "", false, err
	}
	access := "."
	if key, found := snapshot.Key(typ); found && key.Kind() == types.Pointer {
		pointee, ok := key.Child()
		if !ok {
			return "", "", false, fmt.Errorf("union variant payload store pointer has no pointee")
		}
		typ = pointee
		access = "->"
	}
	if !unionVariantPayloadMember(unit, snapshot, typ, place.Member) {
		return "", "", false, nil
	}
	payload := fmt.Sprintf("%s%spayload.pebble_field_%d", base, access, place.Member)
	tag := fmt.Sprintf("%s%stag", base, access)
	return payload, tag, true, nil
}
