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

func buildRuntimeLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.RecordConstruct {
		// A fresh runtime-typed record construction: only the built-in
		// Allocator can be constructed from source (`Allocator.{ ptr, alloc,
		// realloc, free }`), and its RecordConstruct has no parsed
		// TypeDeclaration (buildStructBraceList would reject it), so it is
		// emitted here as a designated-initializer PebbleAllocator local.
		return buildRuntimeAllocatorRecordDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
	}
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// A call to an Allocator-returning helper used as the direct
		// initializer of a matching Allocator-typed local — `let a = build();`.
		// The call expression is the whole initializer, declared with the
		// hand-written runtime C type name, the same aggregate-call-initializer
		// shape buildAggregateCallInitializer uses for ordinary struct-typed
		// locals. The callee's declared result type must be exactly the local's
		// type (defense for hand-built IR).
		calleeDecl, err := findCallDeclaration(unit, snapshot, initValue)
		if err != nil {
			return "", err
		}
		if calleeDecl.ResultType != initValue.Type {
			return "", fmt.Errorf("%s declares a runtime-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, runtimeTypeName(unit, snapshot, initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
		}
		callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{structType: initValue.Type, runtimeType: initValue.Type}
		return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, runtimeTypeName(unit, snapshot, initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
	}
	if initValue.Kind == tir.ContextValue {
		// The bare `context` expression used directly as a Context-typed local's
		// initializer — `let c = context;`. ContextValue is the distinct TIR
		// shape the `context` keyword lowers to, and Context is always the
		// already-threaded hidden `ctx` parameter — never a value to construct
		// from scratch (unlike an Allocator RecordConstruct) — so the local is
		// declared from the dereferenced parameter `(*ctx)`, a PebbleContext
		// value, and seeded as a struct-typed local so the general by-value
		// movement machinery treats it like the ordinary struct it is.
		scope[statement.Symbol] = localInfo{structType: initValue.Type, runtimeType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = (*ctx);\n%s(void)pebble_local_%d;", indent, runtimeTypeName(unit, snapshot, initValue.Type), statement.Symbol, indent, statement.Symbol), nil
	}
	if initValue.Kind != tir.FieldValue && initValue.Kind != tir.Load && initValue.Kind != tir.SymbolValue {
		return "", fmt.Errorf("%s declares a runtime-typed local initialized from a %s", context, initValue.Kind)
	}
	expr, err := buildRuntimeValue(st, unit, snapshot, fileSet, initValue, scope, width)
	if err != nil {
		return "", err
	}
	// The local is declared with its hand-written C type (PebbleAllocator) via
	// runtimeTypeName, but it is ALSO seeded as a struct-typed local
	// (localInfo.structType) so the general by-value movement machinery — a
	// whole-struct argument, a function return, a store into a struct field —
	// treats it exactly like the ordinary struct it is, emitting
	// pebble_local_<sym> whose C type matches the callee parameter or field
	// type declared with the same runtime type name.
	scope[statement.Symbol] = localInfo{structType: initValue.Type, runtimeType: initValue.Type}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, runtimeTypeName(unit, snapshot, initValue.Type), statement.Symbol, expr, indent, statement.Symbol), nil
}

// buildTupleLocalDeclaration builds one tuple-typed local's declaration: a
// `pebble_tuple_<typeID>_t pebble_local_<symbol> = { <element>, ... };` whose
// element expressions are the TupleValue initializer's children in order, each
// built by the grammar its own element type selects — buildExpr for an element
// of the entry's width, buildBoolExpr for a bool element. Every element type
// must be exactly the entry's width or bool; anything else (a str element, a
// nested tuple element) is a clean rejection naming the element position, since
// this backend emits exactly those two C field types. The local's scope entry
// records its tuple type (a localInfo with tuple set), so a later element read
// resolves the tuple type being indexed. TupleValue (a tuple literal) and
// TupleCoerce (a tuple literal with per-element coercions) are emitted as bare
// brace lists; a
// DirectCall to a tuple-returning helper whose result type matches the local's
// declared type, emitted by the same call-building machinery buildExpr's
// DirectCall case uses (see buildAggregateCallInitializer); or a SymbolValue
// naming an in-scope tuple-typed local of exactly the local's type, emitted as
// `pebble_tuple_<typeID>_t pebble_local_<symbol> = pebble_local_<other>;` — a
// plain C struct declaration-with-initializer, valid because the tuple's own
// typedef makes the by-value copy trivially correct (the same acceptance
// logic buildTupleStoreValue uses for the reassignment sibling `p = q;`).
// Initializing a tuple local from any other value is a clean rejection. Like
// every scalar local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildTupleLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// A call to a tuple-returning helper used as the direct initializer of
		// a matching tuple-typed local — `let t (i32, i32) =
		// helperReturningTuple();` — the one position (10.26) in which calling
		// a tuple-returning helper is supported.
		return buildAggregateCallInitializer(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width, true)
	}
	if initValue.Kind == tir.SymbolValue {
		// A reference to an in-scope tuple-typed local of exactly the local's
		// type used as the direct initializer — `let second (int, int) =
		// first;`, a whole-tuple local copy, the sibling of the reassignment
		// shape buildTupleStoreValue accepts (`p = q;`). The referenced
		// local's declared type must be exactly the local's type: the scope
		// lookup records the tuple type each local was declared with
		// (localInfo.tuple), so a non-tuple local, an undeclared symbol, or a
		// tuple local of a different type is a clean rejection naming what was
		// found. The emitted C is a declaration-with-initializer
		// `pebble_tuple_<typeID>_t pebble_local_<sym> = pebble_local_<other>;`
		// — the tuple's own typedef makes the by-value copy trivially valid C,
		// the same convention tuple call arguments and returns already use.
		valueInfo, declared := scope[initValue.Symbol]
		if !declared || valueInfo.tuple != initValue.Type {
			return "", fmt.Errorf("%s declares a tuple-typed local of type %s from symbol %d, which is not a tuple-typed local in scope of that type", context, tupleTypeName(initValue.Type), initValue.Symbol)
		}
		scope[statement.Symbol] = localInfo{tuple: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = pebble_local_%d;\n%s(void)pebble_local_%d;", indent, tupleTypeName(initValue.Type), statement.Symbol, initValue.Symbol, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.Load {
		// A by-value whole-tuple read used as the local's initializer, in two
		// shapes mirroring the struct-typed local's Load case (buildStructLocal
		// Declaration):
		//
		//   - one tuple element of an array or slice local — `let e =
		//     entries[j];` — lowered by the checker to a Load of a
		//     CheckedIndexPlace whose single child is the StoragePlace naming
		//     the array/slice local;
		//   - a whole tuple read through a pointer deref — `let q = *ptr;` —
		//     lowered to a Load of a DereferencePlace whose single child is the
		//     pointer expression.
		//
		// The emitted C is a whole-tuple copy declaration,
		// `pebble_tuple_<typeID>_t pebble_local_<symbol> = <lvalue>;`, where
		// the lvalue is the place's own C expression built by buildPlaceLValue
		// (the bounds-checked element projection for a CheckedIndexPlace, or
		// the null-checked dereference
		// `*(pebble_tuple_<typeID>_t)(pebble_rt_checked_deref_ptr(...))` for a
		// DereferencePlace) — the tuple's own typedef makes the by-value copy
		// trivially valid C. The lvalue's resolved element type must be
		// exactly the local's declared type (defense for hand-built IR).
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares a tuple-typed local of type %s initialized from a Load with %d child(ren), want exactly one place", context, tupleTypeName(initValue.Type), len(initValue.Children))
		}
		place, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s declares a tuple-typed local of type %s initialized from a Load referencing invalid place node %d", context, tupleTypeName(initValue.Type), initValue.Children[0])
		}
		if place.Kind != tir.CheckedIndexPlace && place.Kind != tir.DereferencePlace {
			return "", fmt.Errorf("%s declares a tuple-typed local of type %s initialized from a Load whose place is a %s, want a CheckedIndexPlace (a by-value tuple-element read) or a DereferencePlace (a by-value whole-tuple read through a pointer)", context, tupleTypeName(initValue.Type), place.Kind)
		}
		lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
		if err != nil {
			return "", fmt.Errorf("%s tuple-element read: %v", context, err)
		}
		if elementType != initValue.Type {
			return "", fmt.Errorf("%s declares a tuple-typed local of type %s initialized from a read of element type %s", context, tupleTypeName(initValue.Type), describeType(snapshot, elementType))
		}
		scope[statement.Symbol] = localInfo{tuple: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, tupleTypeName(initValue.Type), statement.Symbol, lvalue, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.TupleCoerce {
		if len(initValue.Children) < 2 {
			return "", fmt.Errorf("%s declares a tuple-typed local from a TupleCoerce with %d child(ren), want at least two", context, len(initValue.Children))
		}
		destination, err := tupleCoerceDestinationType(snapshot, initValue.TypeArgs)
		if err != nil {
			return "", fmt.Errorf("%s declares a tuple-typed local from a TupleCoerce: %v", context, err)
		}
		key, ok := snapshot.Key(destination)
		if !ok {
			return "", fmt.Errorf("%s declares a tuple-typed local whose destination type %d is not in the type snapshot", context, destination)
		}
		elements, ok := key.Elements()
		if !ok {
			return "", fmt.Errorf("%s declares a tuple-typed local of type %s, which has no element list", context, tupleTypeName(destination))
		}
		if len(initValue.Children)-1 != len(elements) {
			return "", fmt.Errorf("%s declares a tuple-typed local of type %s from a TupleCoerce with %d element expression(s), want %d", context, tupleTypeName(destination), len(initValue.Children)-1, len(elements))
		}
		braceList, err := buildTupleBraceElements(st, unit, snapshot, fileSet, destination, elements, initValue.Children[1:], scope, context, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{tuple: destination}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, tupleTypeName(destination), statement.Symbol, braceList, indent, statement.Symbol), nil
	}
	if initValue.Kind != tir.TupleValue {
		return "", fmt.Errorf("%s declares a tuple-typed local of type %s initialized from a %s, want a TupleValue (a tuple literal), a call to a tuple-returning helper, or a reference to a tuple-typed local in scope of that type", context, tupleTypeName(initValue.Type), initValue.Kind)
	}
	braceList, err := buildTupleBraceList(st, unit, snapshot, fileSet, initValue, scope, context, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{tuple: initValue.Type}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, tupleTypeName(initValue.Type), statement.Symbol, braceList, indent, statement.Symbol), nil
}

// buildArrayLocalDeclaration builds a fixed-length C array from an ArrayValue
// literal or an ArrayRepeat ([v; N]) initializer. Array elements use the same
// integer/bool builders as scalar locals; nested arrays and all other element
// types remain out of scope. An ArrayValue initializer emits the array's
// declaration directly with a C brace-list initializer (10.20); an ArrayRepeat
// initializer is emitted by buildArrayRepeatLocalDeclaration as a three-
// statement sequence (bare declaration, one-time-evaluated repeat temp, fill
// loop) so the repeat value is evaluated exactly once, not once per slot
// (10.27).
func buildArrayLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind != tir.ArrayValue && initValue.Kind != tir.ArrayRepeat && initValue.Kind != tir.SymbolValue && initValue.Kind != tir.Load && initValue.Kind != tir.CheckedOptionalUnwrap {
		return "", fmt.Errorf("%s declares an array-typed local of type %s initialized from a %s, want an ArrayValue (an array literal), an ArrayRepeat (a [v; N] repeat initializer), a reference to an array-typed local in scope of that type, a by-value whole-array read through a pointer (a Load of a DereferencePlace), or a force-unwrap of an array-payload optional", context, describeType(snapshot, initValue.Type), initValue.Kind)
	}
	key, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", fmt.Errorf("%s declares an array-typed local whose type %d is not in the type snapshot", context, initValue.Type)
	}
	length, elementType, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("%s declares an array-typed local of type %s, which has no array length and element type", context, describeType(snapshot, initValue.Type))
	}
	if _, err := arrayLengthLiteral(length, width); err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	// Every scalar element type must be a fixed-width integer builtin (resolved
	// to its OWN width by resolvedBuiltin/cType — the entry's width, uint, u8,
	// u16, u32, u64, i8, i16, i32, or i64), char, or bool — the same
	// isSupportedSliceElementType gate the slice-side element builders use,
	// widened here from the entry-width-only check so a slice constructed from
	// an array of u8/char/etc. elements (a[1:3] over a real [N]u8 backing
	// array) can actually be built. Tuple/array/optional/struct elements remain
	// accepted exactly as before. An enum element is a Nominal type exactly
	// like a struct element (see isEnumType) and is rejected here explicitly,
	// since enum-typed array elements are out of scope.
	if isEnumType(unit, snapshot, elementType) {
		return "", fmt.Errorf("%s declares an array-typed local of type %s whose element type %s is an enum type; enum-typed array elements are not supported yet", context, describeType(snapshot, initValue.Type), enumTypeName(elementType))
	}
	if !isStr(snapshot, elementType) && !isSupportedArrayElementType(unit, snapshot, elementType) && !isFloat(snapshot, elementType) {
		return "", fmt.Errorf("%s declares an array-typed local of type %s whose element type is %s, want a fixed-width integer, char, bool, f32, f64, an aggregate element type, or a nested fixed array element", context, describeType(snapshot, initValue.Type), describeType(snapshot, elementType))
	}
	if initValue.Kind == tir.ArrayRepeat && isStr(snapshot, elementType) {
		return "", fmt.Errorf("%s declares a str array local from an ArrayRepeat, want an ArrayValue (an array literal)", context)
	}
	scope[statement.Symbol] = localInfo{array: initValue.Type}
	if initValue.Kind == tir.ArrayRepeat {
		return buildArrayRepeatLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width, length, elementType)
	}
	if initValue.Kind == tir.SymbolValue {
		// A reference to an in-scope array-typed local of exactly the local's
		// type used as the direct initializer — `let second [3]int = first;`,
		// a whole-array local copy, the sibling of the reassignment shape
		// buildArrayStoreValue accepts (`a = b;`). The referenced local's
		// declared type must be exactly the local's type — same length AND same
		// element type, mirroring buildArrayStoreValue's own type-match guard
		// (the scope lookup records the array type each local was declared
		// with, localInfo.array, so a non-array local, an undeclared symbol, or
		// an array local of a different type is a clean rejection naming what
		// was found). Unlike the reassignment statement, which sits after both
		// declarations and can memcpy into an already-declared lvalue, this is
		// a DECLARATION: C cannot initialize a raw array variable from another
		// array variable in its declarator (only from a brace list), so the
		// whole-array copy must run after the declaration exists. The emitted C
		// is therefore three statements — a bare declaration with no
		// initializer, then a byte-for-byte memcpy of the source local's own
		// storage into the new local's, sized by the new local's storage (the
		// same memcpy shape buildArrayStoreValue's plain-local path emits, the
		// whole-array by-value copy convention array call arguments already
		// use), then the (void) cast every local ends with. The bare declaration
		// is uninitialized-but-immediately-fully-written: memcpy writes every
		// byte before any read, so -Wuninitialized does not fire (confirmed by
		// real cc compiles in the test suite), and -Wunused-variable is silenced
		// by the trailing (void) cast exactly as every other local does.
		valueInfo, declared := scope[initValue.Symbol]
		if !declared || valueInfo.array != initValue.Type {
			return "", fmt.Errorf("%s declares an array-typed local of type %s from symbol %d, which is not an array-typed local in scope of that type", context, describeType(snapshot, initValue.Type), initValue.Symbol)
		}
		elementCType, err := arrayElementCType(unit, snapshot, width, elementType)
		if err != nil {
			return "", fmt.Errorf("%s: %v", context, err)
		}
		st.hasArrayStore = true
		return strings.Join([]string{
			fmt.Sprintf("%s%s pebble_local_%d[%d];", indent, elementCType, statement.Symbol, length),
			fmt.Sprintf("%smemcpy(pebble_local_%d, &pebble_local_%d, sizeof(pebble_local_%d));", indent, statement.Symbol, initValue.Symbol, statement.Symbol),
			fmt.Sprintf("%s(void)pebble_local_%d;", indent, statement.Symbol),
		}, "\n"), nil
	}
	if initValue.Kind == tir.Load {
		// A whole array read through a pointer deref used directly as the
		// local's initializer — `let q [3]i32 = *p;`, the read-side twin of
		// the `*p = v;` write and the pointer-receiver reset shape
		// `self.data = other;`. The initializer is a Load whose place is a
		// DereferencePlace whose single child is the pointer expression. Like
		// the SymbolValue copy-initializer case above, C cannot initialize a
		// raw array variable from another array value in its declarator, so
		// the copy is three statements — a bare declaration, then a
		// byte-for-byte memcpy of the null-checked dereference pointer's
		// storage into the new local's (the dereference yields the array
		// object itself, whose address is the checked pointer cast to the
		// pointer-to-array C type), then the (void) cast every local ends
		// with.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares an array-typed local of type %s initialized from a Load with %d child(ren), want exactly one place", context, describeType(snapshot, initValue.Type), len(initValue.Children))
		}
		place, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s declares an array-typed local of type %s initialized from a Load referencing invalid place node %d", context, describeType(snapshot, initValue.Type), initValue.Children[0])
		}
		if place.Kind != tir.DereferencePlace {
			return "", fmt.Errorf("%s declares an array-typed local of type %s initialized from a Load whose place is a %s, want a DereferencePlace (a by-value whole-array read through a pointer)", context, describeType(snapshot, initValue.Type), place.Kind)
		}
		if place.Type != initValue.Type {
			return "", fmt.Errorf("%s declares an array-typed local of type %s initialized from a read of a dereference place of type %s", context, describeType(snapshot, initValue.Type), describeType(snapshot, place.Type))
		}
		if len(place.Children) != 1 {
			return "", fmt.Errorf("%s array deref read has %d child(ren), want exactly one (the pointer expression)", context, len(place.Children))
		}
		ptrExpr, err := buildExpr(st, unit, snapshot, fileSet, place.Children[0], scope, width, width)
		if err != nil {
			return "", fmt.Errorf("%s array deref pointer expression: %v", context, err)
		}
		ptrCType := pointerTypeNameForUnit(st, unit, snapshot, place.Type)
		if ptrCType == "" {
			return "", fmt.Errorf("%s array deref has unsupported pointee type %s", context, describeType(snapshot, place.Type))
		}
		checkedPtr := fmt.Sprintf("pebble_rt_checked_deref_ptr(%s, %s)", ptrExpr, buildSourceLoc(fileSet, place.Span))
		// The dereference yields the wrapped pebble_array_<id>_t struct (see
		// pointerTypeNameForUnit's array case); the memcpy source is the
		// address of that struct's raw `.data` array member.
		srcPtr := fmt.Sprintf("&(*(%s)(%s)).data", ptrCType, checkedPtr)
		elementCType, err := arrayElementCType(unit, snapshot, width, elementType)
		if err != nil {
			return "", fmt.Errorf("%s: %v", context, err)
		}
		st.hasArrayStore = true
		return strings.Join([]string{
			fmt.Sprintf("%s%s pebble_local_%d[%d];", indent, elementCType, statement.Symbol, length),
			fmt.Sprintf("%smemcpy(pebble_local_%d, %s, sizeof(pebble_local_%d));", indent, statement.Symbol, srcPtr, statement.Symbol),
			fmt.Sprintf("%s(void)pebble_local_%d;", indent, statement.Symbol),
		}, "\n"), nil
	}
	if initValue.Kind == tir.CheckedOptionalUnwrap {
		// A force-unwrap of an optional whose payload is an array used as the
		// direct initializer of a matching array-typed local — `let t [3]i32 =
		// o!;`. The optional's .value field is the wrapped pebble_array_<id>_t
		// struct (see optionalPayloadCType) whose raw `.data` member is the
		// array, and C cannot initialize a raw array variable from another
		// array in its declarator, so the unwrap lowers to the presence-only
		// runtime check followed by the same bare-declaration + memcpy shape a
		// whole-array local copy uses (memcpy from the unwrapped .value's
		// `.data`). When the unwrapped optional is a call result, the call is
		// hoisted into a pebble_temp_<id> optional local first so its
		// .has_value and .value fields are read from a single evaluation, the
		// same evaluate-once pattern the slice/pointer-payload unwrap
		// initializers use.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s force-unwrap initializer has %d child(ren), want exactly one optional value", context, len(initValue.Children))
		}
		child, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s force-unwrap initializer references invalid child node %d", context, initValue.Children[0])
		}
		for child.Kind == tir.SourceAlias {
			if len(child.Children) != 1 {
				return "", fmt.Errorf("%s force-unwrap initializer SourceAlias has %d child(ren), want exactly one", context, len(child.Children))
			}
			child, ok = unit.Node(child.Children[0])
			if !ok {
				return "", fmt.Errorf("%s force-unwrap initializer references invalid alias child node %d", context, child.Children[0])
			}
		}
		var optionalCExpr string
		var pre string
		if child.Kind == tir.DirectCall || child.Kind == tir.MethodCall {
			callPre, callText, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, child, scope, width)
			if err != nil {
				return "", err
			}
			tempName := fmt.Sprintf("pebble_temp_%d", statement.Children[0])
			optionalCExpr = tempName
			pre = callPre
			if pre != "" {
				pre = pre + "\n"
			}
			pre = pre + fmt.Sprintf("%s%s = %s;", optionalTypeName(child.Type), tempName, callText)
		} else if child.Kind == tir.SymbolValue {
			info, declared := scope[child.Symbol]
			if !declared || info.optional == 0 {
				return "", fmt.Errorf("%s force-unwrap initializer references symbol %d, which is not an optional-typed local declared earlier in the body", context, child.Symbol)
			}
			optionalCExpr = fmt.Sprintf("pebble_local_%d", child.Symbol)
		} else if child.Kind == tir.Load && len(child.Children) == 1 {
			lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			if !isOptional(snapshot, placeType) {
				return "", fmt.Errorf("%s force-unwrap initializer reads a place of type %s, want an optional-typed place", context, describeType(snapshot, placeType))
			}
			optionalCExpr = lvalue
		} else {
			return "", fmt.Errorf("%s force-unwrap initializer unwraps a %s, want an optional-typed local, an optional-typed place read, or a call to an optional-returning helper", context, child.Kind)
		}
		payloadKey, ok := snapshot.Key(child.Type)
		if !ok || payloadKey.Kind() != types.Optional {
			return "", fmt.Errorf("%s force-unwrap initializer unwraps type %d, which is not an optional type in the type snapshot", context, child.Type)
		}
		payload, ok := payloadKey.Child()
		if !ok || payload != initValue.Type {
			return "", fmt.Errorf("%s force-unwrap initializer unwraps an optional whose payload %s is not the local's array type %s", context, describeType(snapshot, payload), describeType(snapshot, initValue.Type))
		}
		elementCType, err := arrayElementCType(unit, snapshot, width, elementType)
		if err != nil {
			return "", fmt.Errorf("%s: %v", context, err)
		}
		st.hasArrayStore = true
		check := fmt.Sprintf("%spebble_rt_checked_unwrap_present(%s.has_value, %s);", indent, optionalCExpr, buildSourceLoc(fileSet, initValue.Span))
		body := strings.Join([]string{
			fmt.Sprintf("%s%s pebble_local_%d[%d];", indent, elementCType, statement.Symbol, length),
			fmt.Sprintf("%smemcpy(pebble_local_%d, &%s.value.data, sizeof(pebble_local_%d));", indent, statement.Symbol, optionalCExpr, statement.Symbol),
			fmt.Sprintf("%s(void)pebble_local_%d;", indent, statement.Symbol),
		}, "\n")
		scope[statement.Symbol] = localInfo{array: initValue.Type}
		if pre != "" {
			return indent + pre + "\n" + check + "\n" + body, nil
		}
		return check + "\n" + body, nil
	}
	if len(initValue.Children) != int(length) {
		return "", fmt.Errorf("%s declares an array-typed local of type %s with %d element expression(s), want %d", context, describeType(snapshot, initValue.Type), len(initValue.Children), length)
	}
	exprs, err := buildArrayBraceElements(st, unit, snapshot, fileSet, initValue, scope, context, width, elementType)
	if err != nil {
		return "", err
	}
	elementCType, err := arrayElementCType(unit, snapshot, width, elementType)
	if err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	return fmt.Sprintf("%s%s pebble_local_%d[%d] = { %s };\n%s(void)pebble_local_%d;", indent, elementCType, statement.Symbol, length, strings.Join(exprs, ", "), indent, statement.Symbol), nil
}

// buildArrayBraceElements builds the per-element C expression strings of an
// ArrayValue literal, using the element type to select the element grammar.
// It is the shared element builder for an array-typed local's brace-list
// declaration (buildArrayLocalDeclaration) and for the hidden backing array an
// array-literal slice initializer constructs before slicing it (the
// ArrayValue base of buildSliceConstruction).
func buildArrayBraceElements(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, initValue tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind, elementType types.TypeID) ([]string, error) {
	exprs := make([]string, len(initValue.Children))
	for i, child := range initValue.Children {
		var expr string
		var err error
		if isBool(snapshot, elementType) {
			expr, err = buildBoolExpr(st, unit, snapshot, fileSet, child, scope, width)
		} else if isChar(snapshot, elementType) {
			expr, err = buildCharOperand(st, unit, snapshot, fileSet, child, scope, width)
		} else if isFloat(snapshot, elementType) {
			expr, err = buildFloatExpr(st, unit, snapshot, fileSet, child, scope, resolvedFloatKind(snapshot, elementType), width)
		} else if elementWidth, integerElement := resolvedBuiltin(snapshot, elementType); integerElement && cType(elementWidth) != "" {
			// An integer element of any fixed-width builtin, not just the
			// entry's own: each element is built at the element's OWN resolved
			// width (an element of a [3]u8 array inside an i32 function builds
			// its value at u8), mirroring how buildScalarInitializeCore builds
			// a scalar local at its own declared width.
			expr, err = buildExpr(st, unit, snapshot, fileSet, child, scope, elementWidth, width)
		} else if isStr(snapshot, elementType) {
			expr, err = buildStrOperand(st, unit, snapshot, fileSet, child, scope, width)
		} else if isDefinitelyEnumType(unit, snapshot, elementType) {
			// A plain enum element (an enum-shaped Nominal type — the slice
			// construction backing-array shape `[Color.red, Color.green,
			// Color.blue]`, since enum-element slices; a user-level array local
			// of enum elements is still rejected before reaching here by
			// buildArrayLocalDeclaration, so this branch serves the slice
			// backing array). Each element is a variant literal built by the
			// same buildEnumValue an enum-typed local's declaration uses,
			// emitting the variant's C enum constant.
			expr, err = buildEnumValue(st, unit, snapshot, fileSet, child, scope, width)
		} else if isTuple(snapshot, elementType) {
			expr, err = buildNestedAggregateValue(st, unit, snapshot, fileSet, child, scope, elementType, context, width)
		} else if isStruct(snapshot, elementType) {
			expr, err = buildNestedAggregateValue(st, unit, snapshot, fileSet, child, scope, elementType, context, width)
		} else if isOptional(snapshot, elementType) {
			expr, err = buildNestedAggregateValue(st, unit, snapshot, fileSet, child, scope, elementType, context, width)
		} else if isArray(snapshot, elementType) {
			// A nested fixed-array element (`var a [2][3]i32 =
			// [[1,2,3],[4,5,6]];`): the element's C type is the inner array's
			// own pebble_array_<innerID>_t wrapper typedef (see
			// arrayElementCType's array case), so each inner array literal is
			// built as that wrapper's compound literal by the same nested
			// aggregate value builder struct/tuple/optional elements use.
			expr, err = buildNestedAggregateValue(st, unit, snapshot, fileSet, child, scope, elementType, context, width)
		} else {
			expr, err = buildExpr(st, unit, snapshot, fileSet, child, scope, width, width)
		}
		if err != nil {
			return nil, err
		}
		exprs[i] = expr
	}
	return exprs, nil
}

// buildArrayRepeatLocalDeclaration builds an array-typed local whose
// initializer is an ArrayRepeat ([v; N]): a single value expression repeated
// N times. The local is emitted as three C statements instead of one
// declaration line, so the repeat value is evaluated exactly once rather than
// once per slot (a naive brace-list { v, v, v } would re-evaluate v N times —
// wrong if v has any observable side effect, e.g. a checked-arithmetic panic
// or a call):
//
//	<indent>int32_t pebble_local_<sym>[<len>];
//	<indent>int32_t pebble_repeat_<sym> = <v>;
//	<indent>for (size_t pebble_i_<sym> = 0; pebble_i_<sym> < <len>; pebble_i_<sym>++) {
//	<indent>    pebble_local_<sym>[pebble_i_<sym>] = pebble_repeat_<sym>;
//	<indent>}
//	<indent>(void)pebble_local_<sym>;
//
// Both synthetic names derive from the local's own declaration symbol
// (pebble_repeat_<symbolID>, pebble_i_<symbolID>), which is guaranteed
// collision-free by construction: ArrayRepeat only ever appears as that one
// local's own initializer, so no other statement in the same function can
// reuse the symbol ID. The loop counter is size_t (C's own array-indexing
// idiom, available because pebble_rt.h includes <stddef.h>); comparing it
// against the array-length literal compiles clean under -Wall -Wextra -Werror
// (confirmed with a real cc compile), so no signed/unsigned adjustment is
// needed. The repeat value v is built by the grammar its element type selects
// — buildExpr for an element of the entry's width, buildBoolExpr for a bool
// element — and appears in the emitted C exactly once, so it is evaluated
// exactly once at runtime. The count child of an ArrayRepeat node is a
// synthesized compile-time IntegerLiteral of the snapshot's uint builtin that
// always equals the array type's own TypeKey.Array() length (confirmed
// against a real fixture: the checker builds it from the array's declared
// length in check's ir_builder_literals.go), so the loop bound comes from
// length here, and a count child that is not such a matching literal is a
// clean rejection for hand-built IR, never a guessed loop bound. The local's
// scope entry records the array type (a localInfo with array set) exactly as
// the ArrayValue path does, so element reads afterward resolve through the
// existing Load(CheckedIndexPlace) machinery unchanged — nothing about how
// the array is read changes, only how it is initialized. Like every local,
// the sequence ends with the (void) cast against -Wunused-variable.
func buildArrayRepeatLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, length uint64, elementType types.TypeID) (string, error) {
	if len(initValue.Children) != 2 {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat with %d child(ren), want exactly two (the repeated value and the count)", context, len(initValue.Children))
	}
	countNode, ok := unit.Node(initValue.Children[1])
	if !ok {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat referencing invalid count node %d", context, initValue.Children[1])
	}
	if countNode.Kind != tir.IntegerLiteral {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat whose count is a %s, want a compile-time integer literal equal to the array's declared length %d", context, countNode.Kind, length)
	}
	if countNode.Type != snapshot.Builtins().Uint {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat whose count has type %s, want uint (the count is a synthesized integer literal)", context, describeType(snapshot, countNode.Type))
	}
	count, err := strconv.ParseUint(countNode.Literal.IntegerNum, 10, 64)
	if err != nil {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat whose count %q is not a valid non-negative integer", context, countNode.Literal.IntegerNum)
	}
	if count != length {
		return "", fmt.Errorf("%s declares an array-typed local from ArrayRepeat whose count %d does not equal the array's declared length %d", context, count, length)
	}
	var valueExpr string
	if isBool(snapshot, elementType) {
		valueExpr, err = buildBoolExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
	} else if isChar(snapshot, elementType) {
		valueExpr, err = buildCharOperand(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
	} else if isFloat(snapshot, elementType) {
		valueExpr, err = buildFloatExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, resolvedFloatKind(snapshot, elementType), width)
	} else if elementWidth, integerElement := resolvedBuiltin(snapshot, elementType); integerElement && cType(elementWidth) != "" {
		valueExpr, err = buildExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, elementWidth, width)
	} else if isStruct(snapshot, elementType) || isTuple(snapshot, elementType) || isOptional(snapshot, elementType) || isArray(snapshot, elementType) {
		// An aggregate repeat value — `[Point.{ x = 1, y = 2 }; 2]` or the
		// nested-array `[[7,8,9]; 2]`, the repeat twin of the aggregate
		// ELEMENTS buildArrayBraceElements builds. The single value is an
		// aggregate literal (or an aggregate-typed SymbolValue) built by the
		// same buildNestedAggregateValue an aggregate element of the array's
		// element type uses — for an array element that means a nested array
		// literal or an in-scope array-typed local, via its isArray case — and
		// is emitted into the repeat temp and copied into every slot by the
		// fill loop.
		valueExpr, err = buildNestedAggregateValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, elementType, context, width)
	} else {
		valueExpr, err = buildExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, width, width)
	}
	if err != nil {
		return "", err
	}
	ctype, err := arrayElementCType(unit, snapshot, width, elementType)
	if err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	statements := []string{
		fmt.Sprintf("%s%s pebble_local_%d[%d];", indent, ctype, statement.Symbol, length),
		fmt.Sprintf("%s%s pebble_repeat_%d = %s;", indent, ctype, statement.Symbol, valueExpr),
		fmt.Sprintf("%sfor (size_t pebble_i_%d = 0; pebble_i_%d < %d; pebble_i_%d++) {", indent, statement.Symbol, statement.Symbol, length, statement.Symbol),
		fmt.Sprintf("%s    pebble_local_%d[pebble_i_%d] = pebble_repeat_%d;", indent, statement.Symbol, statement.Symbol, statement.Symbol),
		fmt.Sprintf("%s}", indent),
		fmt.Sprintf("%s(void)pebble_local_%d;", indent, statement.Symbol),
	}
	return strings.Join(statements, "\n"), nil
}

// buildSliceLocalDeclaration builds a slice-typed local's declaration from a
// CheckedSlice initializer (a slice expression like `var s []i32 = a[1:3];`)
// or, since 10.38, a DirectCall to a slice-returning helper (`var s []i32 =
// helperReturningSlice();`), or, since slice-local copy-initialization, a
// SymbolValue naming an in-scope slice-typed local of exactly the local's
// type (`var second []i32 = first;`). The CheckedSlice/DirectCall emitted C
// constructs a small struct with a data pointer (offset from the base array
// by the checked start) and a len field (end - start). The start bound is
// validated by pebble_rt_checked_slice_start_i32/i64, which panics if the
// range is invalid. Bounds omitted in source are resolved to their defaults:
// 0 for an absent start, the base array's compile-time element count for an
// absent end. The local's scope entry records its slice type
// (localInfo.sliceType) so a later index read resolves through the
// slice-indexing machinery.
//
// The construction is emitted as two C statements rather than one compound-
// literal initializer because the data pointer depends on the result of the
// pebble_rt_checked_slice_start call, which cannot appear as a sub-expression
// of its own compound literal (the pointer would reference a temporary).
// Instead: first store the validated start offset in a temp, then construct
// the slice struct using the temp for both the pointer offset and the length
// computation.
func buildSliceLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// A call to a slice-returning helper used as the direct initializer of a
		// matching slice-typed local — `let s []i32 = helperReturningSlice();` —
		// the position (10.38) in which a slice-returning helper's result lands
		// in a slice local, mirroring buildStrLocalDeclaration's own DirectCall
		// case. The call's result type is the DirectCall node's own Type, which
		// is the callee's resolved result type (confirmed against a real
		// fixture), and it must be exactly the local's declared type — double-
		// checked against the callee's declared ResultType (defense for
		// hand-built IR), so the emitted C never initializes a slice local from
		// a call returning another type. The call itself is built by
		// buildDirectCall, the same call-building machinery buildExpr's
		// DirectCall case uses. Like every local, the declaration is followed
		// by a (void) cast against -Wunused-variable.
		calleeDecl, err := findCallDeclaration(unit, snapshot, initValue)
		if err != nil {
			return "", err
		}
		if calleeDecl.ResultType != initValue.Type {
			return "", fmt.Errorf("%s declares a slice-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, sliceTypeName(initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
		}
		callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
		return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, sliceTypeName(initValue.Type), statement.Symbol, callExpr, indent, statement.Symbol)), nil
	}
	if initValue.Kind == tir.SliceFromRaw {
		construction, err := buildRawSliceConstruction(st, unit, snapshot, fileSet, initValue, scope, width, context)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, sliceTypeName(initValue.Type), statement.Symbol, construction, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.Load {
		// A by-value read of a slice-typed struct field used as a slice
		// local's declaration initializer — `var old_entries = self.entries;`,
		// the std/hmap.peb rehash shape — lowered by the checker to a Load of
		// a FieldPlace naming the slice field (the same Load(FieldPlace)
		// shape a slice field read in any other value position uses). The
		// emitted C is a whole-struct copy declaration,
		// `pebble_slice_<typeID>_t pebble_local_<symbol> = <lvalue>;`, where
		// the lvalue is the field projection built by buildPlaceLValue, and
		// its resolved type must be exactly the local's declared type
		// (defense for hand-built IR). Like every local, the declaration is
		// followed by a (void) cast against -Wunused-variable.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares a slice-typed local of type %s initialized from a Load with %d child(ren), want exactly one place", context, sliceTypeName(initValue.Type), len(initValue.Children))
		}
		lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
		if err != nil {
			return "", fmt.Errorf("%s slice-field read: %v", context, err)
		}
		if elementType != initValue.Type {
			return "", fmt.Errorf("%s declares a slice-typed local of type %s initialized from a read of element type %s", context, sliceTypeName(initValue.Type), describeType(snapshot, elementType))
		}
		scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, sliceTypeName(initValue.Type), statement.Symbol, lvalue, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.SymbolValue {
		// A reference to an in-scope slice-typed local of exactly the local's
		// type used as the direct initializer — `let second []i32 = first;`, a
		// whole-slice local copy, the sibling of the reassignment shape the
		// slice-typed Store machinery accepts. The referenced local's declared
		// type must be exactly the local's type: the scope lookup records the
		// slice type each local was declared with (localInfo.sliceType), so a
		// non-slice local, an undeclared symbol, or a slice local of a
		// different type is a clean rejection naming what was found. The
		// emitted C is a declaration-with-initializer
		// `pebble_slice_<typeID>_t pebble_local_<new> = pebble_local_<other>;`
		// — a slice is itself a struct ({data, len}) whose typedef makes the
		// by-value copy trivially valid C, the same convention slice call
		// arguments and returns already use. Copying the struct header shares
		// the source local's backing array (view/reference semantics over the
		// backing array, not a deep copy of the elements) — the same semantics
		// a slice passed by value to a helper has, where a write through the
		// copy is observed by the original (see the slice-parameter write
		// tests).
		valueInfo, declared := scope[initValue.Symbol]
		if !declared || valueInfo.sliceType != initValue.Type {
			return "", fmt.Errorf("%s declares a slice-typed local of type %s from symbol %d, which is not a slice-typed local in scope of that type", context, sliceTypeName(initValue.Type), initValue.Symbol)
		}
		scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = pebble_local_%d;\n%s(void)pebble_local_%d;", indent, sliceTypeName(initValue.Type), statement.Symbol, initValue.Symbol, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.CheckedOptionalUnwrap {
		// A force-unwrap of an optional whose payload is a slice used as the
		// direct initializer of a matching slice-typed local — `let t []i32 =
		// o!;`. The slice's .value field is a by-value slice header (the
		// optional's own pebble_slice_<typeID>_t, see optionalPayloadCType),
		// so the unwrap lowers to the presence-only runtime check followed by
		// the .value read — no scalar helper family exists for an aggregate
		// payload. When the unwrapped optional is a call result (`let t =
		// find(x)!;`), the call is hoisted into a pebble_temp_<id> optional
		// local first so its .has_value and .value fields are read from a
		// single evaluation, the same evaluate-once pattern the pointer-payload
		// unwrap initializer uses. A SymbolValue/Load child (an unwrap of an
		// optional-typed local or optional-typed field) needs no hoisting.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s force-unwrap initializer has %d child(ren), want exactly one optional value", context, len(initValue.Children))
		}
		child, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s force-unwrap initializer references invalid child node %d", context, initValue.Children[0])
		}
		for child.Kind == tir.SourceAlias {
			if len(child.Children) != 1 {
				return "", fmt.Errorf("%s force-unwrap initializer SourceAlias has %d child(ren), want exactly one", context, len(child.Children))
			}
			child, ok = unit.Node(child.Children[0])
			if !ok {
				return "", fmt.Errorf("%s force-unwrap initializer references invalid alias child node %d", context, child.Children[0])
			}
		}
		var optionalCExpr string
		if child.Kind == tir.DirectCall || child.Kind == tir.MethodCall {
			callPre, callText, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, child, scope, width)
			if err != nil {
				return "", err
			}
			tempName := fmt.Sprintf("pebble_temp_%d", statement.Children[0])
			optionalCExpr = tempName
			if callPre != "" {
				pre := fmt.Sprintf("%s%s %s = %s;", indent, optionalTypeName(child.Type), tempName, callText)
				body := fmt.Sprintf("%spebble_rt_checked_unwrap_present(%s.has_value, %s);\n%s%s pebble_local_%d = %s.value;\n%s(void)pebble_local_%d;", indent, optionalCExpr, buildSourceLoc(fileSet, initValue.Span), indent, sliceTypeName(initValue.Type), statement.Symbol, optionalCExpr, indent, statement.Symbol)
				scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
				return callPre + "\n" + pre + "\n" + body, nil
			}
		} else if child.Kind == tir.SymbolValue {
			info, declared := scope[child.Symbol]
			if !declared || info.optional == 0 {
				return "", fmt.Errorf("%s force-unwrap initializer references symbol %d, which is not an optional-typed local declared earlier in the body", context, child.Symbol)
			}
			optionalCExpr = fmt.Sprintf("pebble_local_%d", child.Symbol)
		} else if child.Kind == tir.Load && len(child.Children) == 1 {
			lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			if !isOptional(snapshot, placeType) {
				return "", fmt.Errorf("%s force-unwrap initializer reads a place of type %s, want an optional-typed place", context, describeType(snapshot, placeType))
			}
			optionalCExpr = lvalue
		} else {
			return "", fmt.Errorf("%s force-unwrap initializer unwraps a %s, want an optional-typed local, an optional-typed place read, or a call to an optional-returning helper", context, child.Kind)
		}
		payloadKey, ok := snapshot.Key(child.Type)
		if !ok || payloadKey.Kind() != types.Optional {
			return "", fmt.Errorf("%s force-unwrap initializer unwraps type %d, which is not an optional type in the type snapshot", context, child.Type)
		}
		payload, ok := payloadKey.Child()
		if !ok || payload != initValue.Type {
			return "", fmt.Errorf("%s force-unwrap initializer unwraps an optional whose payload %s is not the local's slice type %s", context, describeType(snapshot, payload), sliceTypeName(initValue.Type))
		}
		scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
		return fmt.Sprintf("%spebble_rt_checked_unwrap_present(%s.has_value, %s);\n%s%s pebble_local_%d = %s.value;\n%s(void)pebble_local_%d;", indent, optionalCExpr, buildSourceLoc(fileSet, initValue.Span), indent, sliceTypeName(initValue.Type), statement.Symbol, optionalCExpr, indent, statement.Symbol), nil
	}
	tempDecl, constructionExpr, err := buildSliceConstruction(st, unit, snapshot, fileSet, initValue, scope, indent, context, width, fmt.Sprintf("pebble_slice_start_%d", statement.Symbol), fmt.Sprintf("pebble_slice_backing_%d", statement.Symbol))
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{sliceType: initValue.Type}
	return strings.Join([]string{
		tempDecl,
		fmt.Sprintf("%s%s pebble_local_%d = %s;", indent, sliceTypeName(initValue.Type), statement.Symbol, constructionExpr),
		fmt.Sprintf("%s(void)pebble_local_%d;", indent, statement.Symbol),
	}, "\n"), nil
}

// buildOptionalLocalDeclaration builds one optional-typed local's declaration:
// a `pebble_optional_<typeID>_t pebble_local_<symbol> = { .has_value = true,
// .value = <expr> };` for a SomeOptional initializer, or
// `{ .has_value = false, .value = 0 }` for a NoneOptional (`none` — the
// payload value is irrelevant when absent, so zero is fine), or — since the
// OptionalIntegerToEnum slice — a two-statement
// temp-declaration-plus-declaration fragment for an integer-to-optional-enum
// cast initializer (`var c ?Color = 5 as ?Color;`), built by
// buildOptionalIntegerToEnumDeclaration.
// The payload expression is built by the grammar its own type selects —
// buildExpr for an integer payload, buildBoolExpr for a bool payload — exactly
// like the tuple and array element builders. The local's scope entry records
// its optional type (a localInfo with optional set), so a later force-unwrap
// resolves the optional type being unwrapped. Every payload type must be exactly
// the entry's width or bool; anything else is a clean rejection naming the
// payload type, since this backend emits exactly those two C types as the value
// field. Like every scalar local, the declaration is followed by a (void) cast
// against -Wunused-variable.
func buildOptionalLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, id tir.NodeID) (string, error) {
	key, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", fmt.Errorf("%s declares an optional-typed local whose type %d is not in the type snapshot", context, initValue.Type)
	}
	payloadType, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("%s declares an optional-typed local of type %s, which has no payload type", context, optionalTypeName(initValue.Type))
	}
	if initValue.Kind == tir.Load {
		// A by-value whole-optional read used as the local's initializer —
		// `let q ?i32 = *p;` where p is a `*?i32`, the read-side twin of the
		// optional-typed `*p = v;` write. The initializer is a Load whose
		// place is a DereferencePlace, emitted as the null-checked whole-
		// optional deref value buildDereferencePlaceRead produces
		// (`*(pebble_optional_<typeID>_t)(pebble_rt_checked_deref_ptr(...))`)
		// — the optional's own typedef makes the by-value copy trivially valid
		// C, the same whole-optional read shape the struct and tuple sides'
		// deref reads use. The dereference place's resolved type must be
		// exactly the local's declared type (defense for hand-built IR).
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from a Load with %d child(ren), want exactly one place", context, optionalTypeName(initValue.Type), len(initValue.Children))
		}
		place, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from a Load referencing invalid place node %d", context, optionalTypeName(initValue.Type), initValue.Children[0])
		}
		if place.Kind != tir.DereferencePlace {
			return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from a Load whose place is a %s, want a DereferencePlace (a by-value whole-optional read through a pointer)", context, optionalTypeName(initValue.Type), place.Kind)
		}
		if place.Type != initValue.Type {
			return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from a read of a dereference place of type %s", context, optionalTypeName(initValue.Type), describeType(snapshot, place.Type))
		}
		value, err := buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, scope, width, initValue.Span, false)
		if err != nil {
			return "", fmt.Errorf("%s optional deref read: %v", context, err)
		}
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, optionalTypeName(initValue.Type), statement.Symbol, value, indent, statement.Symbol), nil
	}
	switch initValue.Kind {
	case tir.SomeOptional, tir.OptionalInject:
		// SomeOptional (an explicit `some <expr>`) and OptionalInject (an
		// implicit injection, e.g. `var o ?int = 5;` with no `some` keyword)
		// both carry exactly one child, the payload expression, and lower to
		// the identical C: `{ .has_value = true, .value = <payload> }` — the
		// two TIR node kinds only distinguish authored syntax, not runtime
		// behavior, so they share this whole case body.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares an optional-typed local from %s with %d child(ren), want exactly one payload expression", context, initValue.Kind, len(initValue.Children))
		}
		var valueExpr string
		payloadWidth, integerPayload := resolvedBuiltin(snapshot, payloadType)
		switch {
		case integerPayload && cType(payloadWidth) != "" && !isUint(snapshot, payloadType):
			// Any fixed-width integer payload other than uint (uint flows
			// through its own dedicated grammar below) is built at its OWN
			// resolved width, mirroring buildCallArgument/buildComparisonOperand.
			expr, err := buildExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, payloadWidth, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isUint(snapshot, payloadType):
			expr, err := buildUintExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isBool(snapshot, payloadType):
			expr, err := buildBoolExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isFloat(snapshot, payloadType):
			// A float-typed payload's construction value is built by
			// buildFloatExpr at the payload's OWN float kind (resolvedFloatKind —
			// f32 or f64) and the entry width, exactly as a float call argument,
			// a float local's declaration initializer, and a float comparison
			// operand are built (task #22, slice 86a): a float literal, a
			// reference to an in-scope float-typed local, or a call to a
			// float-returning helper. The optional struct's .value field is the
			// plain C float/double (see optionalPayloadCType), so the built
			// expression matches the field type with no cast.
			expr, err := buildFloatExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, resolvedFloatKind(snapshot, payloadType), width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isTuple(snapshot, payloadType):
			expr, err := buildNestedAggregateValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, payloadType, context, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isTaggedUnionType(unit, snapshot, payloadType):
			// A tagged-union-payload optional initialized from `some <union>`:
			// the payload is a union value (a reference to an already-declared
			// union-typed local, a variant construction, a union-typed field
			// read, or a union-payload force-unwrap), built by buildUnionValueExpr
			// into the optional struct's .value field — which the optional
			// typedef declares with the union's own pebble_union_<typeID>_t
			// (see optionalPayloadCType). This must precede the isEnumType case
			// below: a tagged union is enum-shaped exactly like a plain enum,
			// but it is a real payload the optional must carry, not the bare
			// tag enum a plain-enum payload's only-supported integer cast
			// lowers through.
			expr, err := buildUnionValueExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, context, payloadType, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isEnumType(unit, snapshot, payloadType):
			// An enum-payload optional initialized from `some <variant>`: the
			// payload is a plain enum value (a variant literal like Color.blue,
			// an enum-typed local reference, an enum-returning call, or an
			// enum-typed field read), built by buildEnumValue into the
			// optional struct's .value field — which the optional typedef
			// declares with the enum's own pebble_enum_<typeID>_t. The
			// integer-to-optional-enum cast (`5 as ?Color`) is a separate,
			// distinct initializer shape handled by its own
			// OptionalIntegerToEnum case below and is unaffected.
			expr, err := buildEnumValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isStruct(snapshot, payloadType):
			expr, err := buildNestedAggregateValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, payloadType, context, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isArray(snapshot, payloadType):
			// An array-typed payload (`?[N]T`) initialized from `some <array>`:
			// the payload is an array value (an array literal, a reference to an
			// array-typed local, a by-value read of an array-typed place, or a
			// call to an array-returning helper), built by
			// buildOptionalArrayPayload into the optional struct's .value field —
			// which the optional typedef declares with the array's own
			// pebble_array_<typeID>_t (see optionalPayloadCType).
			expr, err := buildOptionalArrayPayload(st, unit, snapshot, fileSet, initValue.Children[0], scope, context, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isSlice(snapshot, payloadType):
			// A slice-typed payload (`?[]T`) initialized from `some <slice>`: the
			// payload is a slice value (a reference to a slice-typed local, a
			// by-value read of a slice-typed place, a fresh slice construction, a
			// call to a slice-returning helper, or a raw slice construction), built
			// by buildOptionalSlicePayload into the optional struct's .value field
			// — which the optional typedef declares with the slice's own
			// pebble_slice_<typeID>_t. An inline checked-slice construction's
			// checked-start temp comes back as the pre (nested == false, a local
			// declaration is a leading-statement position), threaded ahead of the
			// declaration line below.
			pre, expr, err := buildOptionalSlicePayload(st, unit, snapshot, fileSet, initValue.Children[0], scope, context, width, false)
			if err != nil {
				return "", err
			}
			scope[statement.Symbol] = localInfo{optional: initValue.Type}
			if pre != "" {
				return fmt.Sprintf("%s%s\n%s%s pebble_local_%d = { .has_value = true, .value = %s };\n%s(void)pebble_local_%d;", indent, pre, indent, optionalTypeName(initValue.Type), statement.Symbol, expr, indent, statement.Symbol), nil
			}
			valueExpr = expr
		case isPointer(snapshot, payloadType):
			// A pointer payload's value is built by the same buildExpr path a
			// pointer-typed value takes anywhere else (AddressOf, NilPointer,
			// a pointer-typed local reference, or a pointer-returning call) —
			// buildExpr's isPointer bypass handles the shape regardless of the
			// ambient width args, so no width is threaded here.
			expr, err := buildExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, width, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		default:
			return "", fmt.Errorf("%s declares an optional-typed local of type %s whose payload is %s, want a fixed-width integer, bool, tuple, struct, or enum", context, optionalTypeName(initValue.Type), describeType(snapshot, payloadType))
		}
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = { .has_value = true, .value = %s };\n%s(void)pebble_local_%d;", indent, optionalTypeName(initValue.Type), statement.Symbol, valueExpr, indent, statement.Symbol), nil
	case tir.NoneOptional:
		// NoneOptional has zero children and the payload value is irrelevant
		// when absent — but the zero-value literal's own shape still has to
		// match the payload's own C type: a bare 0 is fine for a scalar
		// (int/bool/enum) .value field, but a struct/tuple .value field needs
		// the aggregate zero-initializer {0} instead — a bare 0 there
		// triggers -Wmissing-field-initializers/-Wmissing-braces under this
		// project's -Werror (see zeroOptionalPayloadLiteral).
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = { .has_value = false, .value = %s };\n%s(void)pebble_local_%d;", indent, optionalTypeName(initValue.Type), statement.Symbol, zeroOptionalPayloadLiteral(unit, snapshot, payloadType), indent, statement.Symbol), nil
	case tir.DirectCall, tir.MethodCall:
		// A call to an optional-returning helper used as the direct
		// initializer of a matching optional-typed local: `var o ?int =
		// f();` or a method call (`let ptr = self.get_by_ref(key);`, the
		// std/hmap.peb get shape — a MethodCall whose result type is the
		// optional type) — the one position (alongside some/none and the
		// integer-to-optional-enum cast) in which an optional-typed local
		// may be initialized, mirroring the tuple/struct aggregate-call
		// initializer shape (buildAggregateCallInitializer).
		return buildOptionalCallInitializer(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
	case tir.OptionalIntegerToEnum:
		// An integer cast to an optional enum used directly as a local
		// declaration's initializer (`var c ?Color = 5 as ?Color;`). The
		// cast must evaluate its source integer exactly once and derive both
		// the has_value bool and the enum value from that single evaluation,
		// so the source is hoisted into an int64_t temp (see
		// buildOptionalIntegerToEnumDeclaration) — the one position this
		// backend supports the cast in, because a declaration statement has a
		// natural place to prepend the temp's own statement line.
		pre, core, err := buildOptionalIntegerToEnumDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, context, id, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return fmt.Sprintf("%s%s\n%s%s;\n%s(void)pebble_local_%d;", indent, pre, indent, core, indent, statement.Symbol), nil
	default:
		return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from a %s, want some <expr> or none", context, optionalTypeName(initValue.Type), initValue.Kind)
	}
}

// buildStructLocalDeclaration builds one struct-typed local's declaration: a
// `pebble_struct_<typeID>_t pebble_local_<symbol> = { .pebble_field_<m0> =
// <e0>, .pebble_field_<m1> = <e1> };` whose field initializers are the
// RecordConstruct's Fields, each value built by the grammar its own type
// selects — buildExpr for a field of the entry's width, buildBoolExpr for a
// bool field. The initializer is a C99 designated-initializer brace list
// (`.pebble_field_<member> = <expr>`), not a positional brace list, so the
// construction-site field order a RecordConstruct's Fields carry (which need
// not match the struct's declared order — a site may write Point.{ y = 2, x =
// 1 }) needs no reordering: each designated initializer places its value
// under exactly the C field its member symbol names, regardless of either
// order. Designated initializers are standard C99 and compile clean under
// -Wall -Wextra -Werror (confirmed by a real cc compile through this test
// suite's own harness). Every field type must be exactly the entry's width or
// bool, or str; anything else (a char field, a nested struct field) is a clean
// rejection naming the field position, since this backend emits exactly those
// three C field types. Four initializer shapes are supported (10.26): a
// RecordConstruct (a struct literal), emitted as a designated-initializer
// brace list, a DirectCall to a struct-returning helper whose result type
// matches the local's declared type, emitted by the same call-building
// machinery buildExpr's DirectCall case uses (see buildAggregateCallInitializer),
// a Load of a CheckedIndexPlace or DereferencePlace, a by-value whole-struct
// read (one struct element of an array or slice local — `let e = old_entries[j];`,
// or a whole struct read through a pointer — `let q = *ptr;`), or a SymbolValue
// naming an in-scope struct-typed local of exactly the local's type, emitted as
// `pebble_struct_<typeID>_t pebble_local_<symbol> = pebble_local_<other>;` — a
// plain C struct declaration-with-initializer, valid because the struct's own
// typedef makes the by-value copy trivially correct (the same acceptance logic
// buildStructStoreValue uses for the reassignment sibling `p = q;`).
// Initializing a struct local from any other value — a whole-struct
// copy of another local, anything else — is a clean rejection. The
// local's scope entry records its struct type (a localInfo with structType
// set), so a later field read resolves the struct type being projected. Like
// every scalar local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildStructLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// A call to a struct-returning helper used as the direct initializer of
		// a matching struct-typed local — `let p Point =
		// helperReturningPoint();` — the one position (10.26) in which calling
		// a struct-returning helper is supported.
		return buildAggregateCallInitializer(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width, false)
	}
	if initValue.Kind == tir.Load {
		// A by-value read used as the whole-struct initializer, in two shapes:
		//
		//   - one struct element of an array or slice local — `let e =
		//     old_entries[j];`, the std/hmap.peb rehash shape — lowered by the
		//     checker to a Load of a CheckedIndexPlace whose single child is the
		//     StoragePlace naming the array/slice local (the exact shape a scalar
		//     element read uses, confirmed against a real fixture);
		//   - a whole struct read through a pointer deref — `let q = *ptr;`, the
		//     read-side twin of the `*self = other;` write — lowered to a Load of
		//     a DereferencePlace whose single child is the pointer expression.
		//
		// The emitted C is a whole-struct copy declaration,
		// `pebble_struct_<typeID>_t pebble_local_<symbol> = <lvalue>;`, where the
		// lvalue is the place's own C expression built by buildPlaceLValue — the
		// bounds-checked element projection for a CheckedIndexPlace (the same
		// lvalue an address-of or field-write through a slice index lowers to),
		// or the null-checked dereference `*(pebble_struct_<typeID>_t)
		// (pebble_rt_checked_deref_ptr(...))` for a DereferencePlace (the same
		// whole-struct deref value buildDereferencePlaceRead produces) — and its
		// resolved element type must be exactly the local's declared type
		// (defense for hand-built IR). Like every local, the declaration is
		// followed by a (void) cast against -Wunused-variable.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a Load with %d child(ren), want exactly one place", context, structTypeName(initValue.Type), len(initValue.Children))
		}
		place, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a Load referencing invalid place node %d", context, structTypeName(initValue.Type), initValue.Children[0])
		}
		if place.Kind != tir.CheckedIndexPlace && place.Kind != tir.DereferencePlace {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a Load whose place is a %s, want a CheckedIndexPlace (a by-value struct-element read) or a DereferencePlace (a by-value whole-struct read through a pointer)", context, structTypeName(initValue.Type), place.Kind)
		}
		lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
		if err != nil {
			return "", fmt.Errorf("%s struct-element read: %v", context, err)
		}
		if elementType != initValue.Type {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a read of element type %s", context, structTypeName(initValue.Type), describeType(snapshot, elementType))
		}
		scope[statement.Symbol] = localInfo{structType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, structTypeName(initValue.Type), statement.Symbol, lvalue, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.FieldValue {
		// A struct-typed field read off a NON-ADDRESSABLE struct VALUE used as
		// the whole initializer — `let i Inner = mk().inner;` — the
		// value-source counterpart of the Load(CheckedIndexPlace)/
		// Load(DereferencePlace) whole-struct initializer shapes above (a
		// struct LOCAL's field lowers to Load(FieldPlace), which
		// buildPlaceLValue covers; a field of a call result, force-unwrap, or
		// element read lowers to this FieldValue). The projection
		// buildStructFieldValueRead produces carries the field's own
		// pebble_struct_<typeID>_t C type, so it initializes the struct local
		// directly.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s from a FieldValue with %d child(ren), want exactly one (the struct value)", context, structTypeName(initValue.Type), len(initValue.Children))
		}
		expr, err := buildStructFieldValueRead(st, unit, snapshot, fileSet, statement.Children[0], scope, width, false, true)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{structType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, structTypeName(initValue.Type), statement.Symbol, expr, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.SymbolValue {
		// A reference to an in-scope struct-typed local of exactly the local's
		// type used as the direct initializer — `let second Point = first;`, a
		// whole-struct local copy, the sibling of the reassignment shape
		// buildStructStoreValue accepts (`p = q;`). The referenced local's
		// declared type must be exactly the local's type: the scope lookup
		// records the struct type each local was declared with
		// (localInfo.structType), so a non-struct local, an undeclared symbol,
		// or a struct local of a different type is a clean rejection naming
		// what was found. The emitted C is a declaration-with-initializer
		// `pebble_struct_<typeID>_t pebble_local_<sym> = pebble_local_<other>;`
		// — the struct's own typedef makes the by-value copy trivially valid C,
		// the same convention struct call arguments and returns already use.
		valueInfo, declared := scope[initValue.Symbol]
		if !declared || valueInfo.structType != initValue.Type {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s from symbol %d, which is not a struct-typed local in scope of that type", context, structTypeName(initValue.Type), initValue.Symbol)
		}
		scope[statement.Symbol] = localInfo{structType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = pebble_local_%d;\n%s(void)pebble_local_%d;", indent, structTypeName(initValue.Type), statement.Symbol, initValue.Symbol, indent, statement.Symbol), nil
	}
	if initValue.Kind != tir.RecordConstruct {
		return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a %s, want a RecordConstruct (a struct literal), a call to a struct-returning helper, or a reference to a struct-typed local in scope of that type", context, structTypeName(initValue.Type), initValue.Kind)
	}
	preStatements, braceList, err := buildStructBraceList(st, unit, snapshot, fileSet, initValue, scope, indent, context, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{structType: initValue.Type}
	declaration := fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, structTypeName(initValue.Type), statement.Symbol, braceList, indent, statement.Symbol)
	if preStatements != "" {
		// A slice-typed field constructed from an inline CheckedSlice needs its
		// temp-declaration statement threaded ahead of the declaration line (the
		// brace list itself has nowhere to put it) — the same statement-position
		// shape buildSliceReturnValue demonstrates for a slice return.
		return preStatements + "\n" + declaration, nil
	}
	return declaration, nil
}

// buildEnumLocalDeclaration builds one plain enum-typed local's declaration: a
// `pebble_enum_<typeID>_t pebble_local_<symbol> = <initializer>;` whose
// initializer is a variant literal — an EnumVariantValue (Color.green, the
// member-access form) or a zero-payload VariantConstruct (Color.red(), the
// parenthesized-call form, which a plain enum's payload-less variants also
// produce — confirmed against a real fixture) — an integer cast to the enum
// (`5 as Color`, built by buildCheckedIntegerToEnumExpr through the checked
// runtime primitive, e.g. `pebble_local_<sym> =
// (pebble_enum_<typeID>_t)pebble_rt_checked_int_to_enum(...);`), a by-value
// read of one enum element of an array or slice local (`let c = colors[1];`, a
// Load of a CheckedIndexPlace, the same element-read shape
// buildStructLocalDeclaration gained for struct-element slices), or — since
// enum-local copy-initialization — a SymbolValue naming an in-scope
// enum-typed local of exactly the local's type, emitted as
// `pebble_enum_<typeID>_t pebble_local_<symbol> = pebble_local_<other>;` — a
// plain C declaration-with-initializer, valid because the enum's own typedef
// makes the by-value copy trivially correct (the same acceptance logic
// buildEnumValue's SymbolValue case uses for the reassignment sibling `p = q;`).
// A variant literal lowers to the variant's C
// enum constant, whose value is the variant's ordinal in the enum's declared
// order (the C typedef emits one named constant per variant in TypeDecl order,
// so the constant and the typedef agree by construction). A payload-carrying
// initializer — an EnumVariantValue or VariantConstruct with one or more
// children — is a tagged-union (union enum) construction, which real source
// routes to buildUnionLocalDeclaration instead (the type is a tagged union
// whenever any reachable construction carries a payload); this payload
// rejection is defense for hand-built IR where such a construction reaches
// this plain-enum builder, never guessed at. The
// initializer's variant symbol must be one of the enum's declared variants, and
// the enum type must actually be a plain enum (not a struct that shares the
// Nominal key shape — isEnumType distinguishes them). The local's scope entry
// records its enum type (a localInfo with enumType set), so a later reference,
// reassignment, switch subject, or comparison resolves the enum type being
// used. Like every scalar local, the declaration is followed by a (void) cast
// against -Wunused-variable.
func buildEnumLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.Load {
		// A by-value read of one enum element of an array or slice local — `let
		// c = colors[1];`, the enum-element-slice shape — lowered by the checker
		// to a Load of a CheckedIndexPlace whose single child is the
		// StoragePlace naming the array/slice local (the exact shape a scalar
		// element read uses). The emitted C is
		// `pebble_enum_<typeID>_t pebble_local_<symbol> = <lvalue>;`, where the
		// lvalue is the bounds-checked element projection built by
		// buildPlaceLValue (the same lvalue an address-of or field-write through
		// a slice index lowers to), and its resolved element type must be
		// exactly the local's declared type (defense for hand-built IR).
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from a Load with %d child(ren), want exactly one place", context, enumTypeName(initValue.Type), len(initValue.Children))
		}
		place, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from a Load referencing invalid place node %d", context, enumTypeName(initValue.Type), initValue.Children[0])
		}
		if place.Kind == tir.DereferencePlace {
			// A whole enum read through a pointer deref used directly as the
			// local's initializer — `let q = *ptr;`, the read-side twin of the
			// enum-typed `*p = v;` reset write. The emitted C is
			// `pebble_enum_<typeID>_t pebble_local_<symbol> = <deref value>;`
			// where the deref value is the null-checked whole-enum deref
			// buildDereferencePlaceRead produces
			// (`*(pebble_enum_<typeID>_t)(pebble_rt_checked_deref_ptr(...))`).
			if place.Type != initValue.Type {
				return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from a read of a dereference place of type %s", context, enumTypeName(initValue.Type), describeType(snapshot, place.Type))
			}
			value, err := buildDereferencePlaceRead(st, unit, snapshot, fileSet, place, scope, width, initValue.Span, false)
			if err != nil {
				return "", fmt.Errorf("%s enum deref read: %v", context, err)
			}
			scope[statement.Symbol] = localInfo{enumType: initValue.Type}
			return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, enumTypeName(initValue.Type), statement.Symbol, value, indent, statement.Symbol), nil
		}
		if place.Kind != tir.CheckedIndexPlace {
			return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from a Load whose place is a %s, want a CheckedIndexPlace (a by-value enum-element read)", context, enumTypeName(initValue.Type), place.Kind)
		}
		lvalue, elementType, err := buildPlaceLValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
		if err != nil {
			return "", fmt.Errorf("%s enum-element read: %v", context, err)
		}
		if elementType != initValue.Type {
			return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from a read of element type %s", context, enumTypeName(initValue.Type), describeType(snapshot, elementType))
		}
		scope[statement.Symbol] = localInfo{enumType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, enumTypeName(initValue.Type), statement.Symbol, lvalue, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.SymbolValue {
		// A reference to an in-scope enum-typed local of exactly the local's
		// type used as the direct initializer — `let second Color = first;`, a
		// whole-enum local copy, the sibling of the reassignment shape
		// buildEnumValue's SymbolValue case accepts (`p = q;`). The referenced local's
		// declared type must be exactly the local's type: the scope lookup
		// records the enum type each local was declared with (localInfo.enumType),
		// so a non-enum local, an undeclared symbol, or an enum local of a
		// different type is a clean rejection naming what was found. The emitted
		// C is a declaration-with-initializer
		// `pebble_enum_<typeID>_t pebble_local_<sym> = pebble_local_<other>;`
		// — the enum's own typedef makes the by-value copy trivially valid C,
		// the same convention enum call arguments and returns already use.
		valueInfo, declared := scope[initValue.Symbol]
		if !declared || valueInfo.enumType != initValue.Type {
			return "", fmt.Errorf("%s declares an enum-typed local of type %s from symbol %d, which is not an enum-typed local in scope of that type", context, enumTypeName(initValue.Type), initValue.Symbol)
		}
		scope[statement.Symbol] = localInfo{enumType: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = pebble_local_%d;\n%s(void)pebble_local_%d;", indent, enumTypeName(initValue.Type), statement.Symbol, initValue.Symbol, indent, statement.Symbol), nil
	}
	switch initValue.Kind {
	case tir.EnumVariantValue:
		if len(initValue.Children) == 1 {
			return "", fmt.Errorf("%s declares an enum-typed local initialized from an enum variant with a payload; a tagged-union (union enum) construction routes through buildUnionLocalDeclaration, never a plain enum declaration", context)
		}
	case tir.VariantConstruct:
		if len(initValue.Children) >= 1 {
			return "", fmt.Errorf("%s declares an enum-typed local initialized from a variant construction with %d payload(s); a tagged-union (union enum) construction routes through buildUnionLocalDeclaration, never a plain enum declaration", context, len(initValue.Children))
		}
	case tir.CheckedIntegerToEnum:
		// An integer cast to an enum (`let c Color = 5 as Color;`) is built by
		// buildCheckedIntegerToEnumExpr below — the value is produced by the
		// checked runtime primitive, not a variant constant.
	default:
		return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from a %s, want a variant literal (e.g. Color.green), an integer cast to the enum type (e.g. 5 as Color), or a reference to an enum-typed local in scope of that type", context, enumTypeName(initValue.Type), initValue.Kind)
	}
	info, err := resolveEnumInfo(unit, snapshot, initValue.Type)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{enumType: initValue.Type}
	if initValue.Kind == tir.CheckedIntegerToEnum {
		castExpr, err := buildCheckedIntegerToEnumExpr(st, unit, snapshot, fileSet, initValue, scope, context, width)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, enumTypeName(initValue.Type), statement.Symbol, castExpr, indent, statement.Symbol), nil
	}
	if !containsVariant(info.variants, initValue.Member) {
		return "", fmt.Errorf("%s declares an enum-typed local of type %s initialized from variant symbol %d, which is not one of its declared variants", context, enumTypeName(initValue.Type), initValue.Member)
	}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, enumTypeName(initValue.Type), statement.Symbol, enumVariantName(initValue.Member), indent, statement.Symbol), nil
}

// buildOptionalIntegerToEnumDeclaration builds the two-part C fragment a local
// declaration whose initializer is an OptionalIntegerToEnum (`var c ?Color =
// 5 as ?Color;`) needs. Unlike its sibling CheckedIntegerToEnum, the cast must
// produce an optional STRUCT VALUE with two fields — `{ .has_value = <bool>,
// .value = <enum value> }` — both derived from the SAME source integer, so the
// source must be evaluated exactly ONCE and both fields read from that single
// evaluation. Naively embedding the source expression twice would evaluate it
// twice, wrong whenever the source has a side effect (a function call, for
// instance). This backend has no expression-level "evaluate once, reuse twice"
// mechanism (buildExpr returns a plain `(string, error)` with no pre-statement
// threading), so the cast is supported ONLY as a local variable declaration's
// initializer — the two call sites that already emit a single indent-prefixed
// statement are the natural places to prepend one more line — and is cleanly
// rejected everywhere else. The returned pre string is the one-time
// evaluation statement, and core the local declaration WITHOUT leading indent
// and WITHOUT trailing `;`:
//
//	pre:  int64_t pebble_temp_<id> = (int64_t)(<child expr>);
//	core: pebble_optional_<typeID>_t pebble_local_<symbol> = { .has_value =
//	      pebble_rt_int_to_enum_is_valid(pebble_temp_<id>, <variant_count>),
//	      .value = (<enum C type>)pebble_temp_<id> }
//
// so the two callers assemble them into the two-line leading-statement form
// (buildOptionalLocalDeclaration, which prepends the indent to each line and
// appends `;` plus the (void) cast) and the leading-statement-plus-header form
// (buildForInitClause, which emits pre as a statement before the for and uses
// core as the for-header's init clause, where the header's own `;` terminates
// it) — mirroring buildCompoundStore's (pre, core, err) shape exactly. The
// temp name is derived from the Initialize statement's own node id, mirroring
// the pebble_compound_ptr_<id> naming scheme's uniqueness-per-call-site. The
// destination enum type and variant count are resolved exactly as
// buildCheckedIntegerToEnumExpr resolves them, except the node's own Type is
// the optional `?Color` — the destination enum is its payload, unwrapped via
// TypeKey.Child() the same way buildOptionalLocalDeclaration unwraps an
// optional-typed initializer's payload.
func buildOptionalIntegerToEnumDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, context string, id tir.NodeID, width types.BuiltinKind) (string, string, error) {
	if initValue.Kind != tir.OptionalIntegerToEnum {
		return "", "", fmt.Errorf("%s contains a %s, want an OptionalIntegerToEnum", context, initValue.Kind)
	}
	if len(initValue.Children) != 1 {
		return "", "", fmt.Errorf("%s contains an OptionalIntegerToEnum with %d child(ren), want exactly one integer value", context, len(initValue.Children))
	}
	optionalKey, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast has invalid destination type %d", context, initValue.Type)
	}
	if optionalKey.Kind() != types.Optional {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast destination %s is not an optional type", context, describeType(snapshot, initValue.Type))
	}
	enumType, ok := optionalKey.Child()
	if !ok {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast destination %s has no payload type", context, describeType(snapshot, initValue.Type))
	}
	info, err := resolveEnumInfo(unit, snapshot, enumType)
	if err != nil {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast: %v", context, err)
	}
	if len(info.variants) == 0 {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast targets enum %s, which has no declared variants", context, enumTypeName(enumType))
	}
	child, ok := unit.Node(initValue.Children[0])
	if !ok {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast references invalid child node %d", context, initValue.Children[0])
	}
	childType, ok := snapshot.Key(child.Type)
	if !ok {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast child has invalid type %d", context, child.Type)
	}
	childWidth, ok := childType.Builtin()
	if !ok || cType(childWidth) == "" {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast child has non-integer type %s", context, describeType(snapshot, child.Type))
	}
	childExpr, err := buildExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, childWidth, width)
	if err != nil {
		return "", "", fmt.Errorf("%s integer-to-optional-enum cast child: %v", context, err)
	}
	tempName := fmt.Sprintf("pebble_temp_%d", id)
	pre := fmt.Sprintf("int64_t %s = (int64_t)(%s);", tempName, childExpr)
	core := fmt.Sprintf("%s pebble_local_%d = { .has_value = pebble_rt_int_to_enum_is_valid(%s, %d), .value = (%s)%s }", optionalTypeName(initValue.Type), statement.Symbol, tempName, len(info.variants), enumTypeName(enumType), tempName)
	return pre, core, nil
}

// buildUnionLocalDeclaration builds one tagged-union-typed local's declaration:
// a `pebble_union_<typeID>_t pebble_local_<symbol> = <construction>;` whose
// initializer is a variant construction — a payload-carrying VariantConstruct
// (Choice.value(5)), a payload-less EnumVariantValue (Choice.empty), or a
// zero-payload VariantConstruct (Choice.empty()) — built by
// buildUnionConstruction as a C99 compound literal. The union type is the
// initializer value's own Type (the Initialize node carries no Type itself,
// confirmed against a real fixture — same as every other local kind), and the
// type must be a tagged union in this program (the caller's unions map,
// collected by collectUnionTypes from reachable payload-carrying
// constructions); a type that is enum-shaped but not in the union map routes
// here's sibling buildEnumLocalDeclaration instead. The construction is
// validated by buildUnionConstruction, which requires the constructed variant's
// symbol to be one of the union's declared variants, so the emitted C's tag
// value and payload member always exist in the union's typedef. The
// local's scope entry records its union type (a localInfo with enumType set —
// a tagged union is enum-shaped exactly like a plain enum), so a later switch
// subject, reassignment, or reference resolves the union type being used.
// Like every local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildUnionLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, unions map[types.TypeID]unionInfo, width types.BuiltinKind) (string, error) {
	if _, ok := unions[initValue.Type]; !ok {
		return "", fmt.Errorf("%s declares an enum-typed local of type %s, which is not a tagged-union type in this program", context, describeType(snapshot, initValue.Type))
	}
	construction, err := buildUnionConstruction(st, unit, snapshot, fileSet, initValue, scope, context, unions[initValue.Type], width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{enumType: initValue.Type}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, unionTypeName(initValue.Type), statement.Symbol, construction, indent, statement.Symbol), nil
}

// buildStrLocalDeclaration builds one str-typed local's declaration: a
// `PebbleStr pebble_local_<symbol> = { .data = (const uint8_t *)"<escaped>",
// .len = <N> };` whose initializer is a StringLiteral (a string literal), a
// call to a str-returning helper (a DirectCall whose result type is str —
// `let s str = g();`, since 10.36), a SymbolValue naming an in-scope
// str-typed local (`let s str = first;`, since str local
// copy-initialization), or a Load of a str tuple element or str struct field
// (`let s str = t.0;` for a (str, int) tuple, or `let s str = b.v;` for a
// Box.{ v = "hi" } struct) — the whole-str copy shapes, each emitted as a
// plain C struct
// declaration-with-initializer, valid because PebbleStr is a genuine C struct
// ({data, len}) and so copies by value like tuple/struct/enum locals do (the
// same acceptance logic buildStrOperand's SymbolValue case uses for the str
// value read). PebbleStr is the
// runtime ABI's length-prefixed string type (runtime/include/pebble_rt.h), a
// fixed runtime type rather than a program-specific shape, so the local is
// declared directly as PebbleStr with no typedef. .data points at the
// literal's bytes re-escaped into a safe C string literal by escapeCString
// (the decoded content is not assumed simple — a control character, a quote,
// or a backslash anywhere in it is escaped correctly, with every non-
// printable byte emitted as a fixed-width octal escape so a following digit
// can never be swallowed by C's maximal-munch escape rules); .len is the
// decoded byte length, a compile-time constant known from the literal itself,
// so no runtime strlen is involved. The initializer must be a StringLiteral,
// a matching str-returning DirectCall, a reference to a str-typed local in
// scope, or a Load of a str tuple element / str struct field:
// initializing a str local from any other value — a copy of another str
// local, anything else — is a clean rejection, keeping this slice's
// supported initializer exactly the string literal, a call to a
// str-returning helper, or a reference to a str-typed local in scope. The
// local's scope entry
// records isStr, so a later str ==/!= comparison, reassignment, or
// str-returning function return resolves the operand as a
// str local. Like every scalar local, the declaration is followed by a (void)
// cast against -Wunused-variable.
func buildStrLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
		// A call to a str-returning helper used as the direct initializer of a
		// matching str-typed local — `let s str = helperReturningStr();` — the
		// one position (10.36) in which calling a str-returning helper is
		// supported for declaring a str local. The call's result type is the
		// DirectCall node's own Type, which is the callee's resolved result
		// type (confirmed against a real fixture), and it must be exactly the
		// local's declared type — double-checked against the callee's declared
		// ResultType (defense for hand-built IR), so the emitted C never
		// initializes a str local from a call returning another type. The call
		// itself is built by buildDirectCall, the same call-building machinery
		// buildExpr's DirectCall case uses, so context and argument handling
		// are identical to a scalar call — only the result type differs. Like
		// every local, the declaration is followed by a (void) cast against
		// -Wunused-variable.
		calleeDecl, err := findCallDeclaration(unit, snapshot, initValue)
		if err != nil {
			return "", err
		}
		if calleeDecl.ResultType != initValue.Type {
			return "", fmt.Errorf("%s declares a str-typed local of type %s initialized from a call to symbol %d whose declared result type %s does not match", context, describeType(snapshot, initValue.Type), initValue.Symbol, describeType(snapshot, calleeDecl.ResultType))
		}
		callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{isStr: true}
		return withLeadingPre(callPre, indent, fmt.Sprintf("%sPebbleStr pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, statement.Symbol, callExpr, indent, statement.Symbol)), nil
	}
	if initValue.Kind == tir.SymbolValue {
		// A reference to an in-scope str-typed local used as the direct
		// initializer — `let second str = first;`, a whole-str local copy, the
		// sibling of the str value read buildStrOperand's SymbolValue case
		// accepts (a SymbolValue naming a str-typed local emits its
		// pebble_local_<symbol> C name). The referenced local must be an
		// in-scope str-typed local: the scope lookup records isStr for every
		// str local (str carries no per-declaration type ID the way
		// tuple/struct/enum do — every str is the same PebbleStr C type), so a
		// non-str local or an undeclared symbol is a clean rejection naming
		// what was found. The emitted C is a declaration-with-initializer
		// `PebbleStr pebble_local_<new> = pebble_local_<other>;` — PebbleStr is
		// a genuine C struct ({data, len}), so the by-value copy is trivially
		// valid C, the same convention str call arguments and returns already
		// use.
		valueInfo, declared := scope[initValue.Symbol]
		if !declared || !valueInfo.isStr {
			return "", fmt.Errorf("%s declares a str-typed local from symbol %d, which is not a str-typed local in scope", context, initValue.Symbol)
		}
		scope[statement.Symbol] = localInfo{isStr: true}
		return fmt.Sprintf("%sPebbleStr pebble_local_%d = pebble_local_%d;\n%s(void)pebble_local_%d;", indent, statement.Symbol, initValue.Symbol, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.FieldValue {
		// A str-typed struct field read off a NON-ADDRESSABLE struct VALUE
		// used as the whole initializer — `let s str = mk().s;` — the
		// value-source counterpart of the Load(FieldPlace) str-field read
		// below (a struct LOCAL's field), which a call-result, force-unwrap,
		// or element-read receiver lowers to instead. The projection
		// buildStructFieldValueRead produces carries the field's own PebbleStr
		// C type, so it initializes the PebbleStr local directly.
		expr, err := buildStructFieldValueRead(st, unit, snapshot, fileSet, statement.Children[0], scope, width, false, false)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{isStr: true}
		return fmt.Sprintf("%sPebbleStr pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, statement.Symbol, expr, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.Load {
		// A str value read back out of a compound local through a Load — a
		// tuple-ordinal read (`let s str = t.0;` for a (str, int) tuple) or a
		// struct-field read-back (`let s str = b.v;` for a Box.{ v = "hi" }
		// struct) — lowered by the checker to Load(TuplePlace)/
		// Load(FieldPlace), the same place shapes buildExpr's int Load case
		// accepts, and both previously rejected together by this switch's
		// final rejection (it had no Load case at all). The place is resolved
		// via buildPlaceLValue, the same machinery buildTuplePlaceRead /
		// buildStructFieldRead use, and the resolved element type must be
		// str: str carries no per-declaration type ID the way
		// tuple/struct/enum do — every str is the same PebbleStr C type — so
		// the check is on the element type the place resolution returns from
		// the tuple/struct type, mirroring how the SymbolValue case above
		// validates its str local. The emitted C is a declaration-with-
		// initializer `PebbleStr pebble_local_<new> = <lvalue>;` —
		// pebble_local_<symbol>._<ordinal> for a tuple element,
		// pebble_local_<symbol>.pebble_field_<member> for a struct field —
		// PebbleStr is a genuine C struct ({data, len}), so the by-value copy
		// is trivially valid C, the same convention str call arguments and
		// returns already use.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares a str-typed local from a Load with %d child(ren), want exactly one place", context, len(initValue.Children))
		}
		placeNode, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s declares a str-typed local from a Load referencing invalid place node %d", context, initValue.Children[0])
		}
		if placeNode.Kind != tir.TuplePlace && placeNode.Kind != tir.FieldPlace {
			return "", fmt.Errorf("%s declares a str-typed local from a Load whose place is a %s, want a TuplePlace (a str tuple-element read) or a FieldPlace (a str struct-field read)", context, placeNode.Kind)
		}
		lvalue, elemType, err := buildPlaceLValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		if !isStr(snapshot, elemType) {
			return "", fmt.Errorf("%s declares a str-typed local from a Load whose resolved element type is %s, want str", context, describeType(snapshot, elemType))
		}
		scope[statement.Symbol] = localInfo{isStr: true}
		return fmt.Sprintf("%sPebbleStr pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, statement.Symbol, lvalue, indent, statement.Symbol), nil
	}
	if initValue.Kind == tir.InterpolatedString {
		// An interpolated string used as the initializer of a str-typed local
		// declaration. Materialize it directly in the initializer using the
		// runtime helper pebble_rt_str_from_parts — unlike expression positions,
		// this doesn't need a GNU statement expression because the declaration
		// itself provides storage:
		//
		//   PebbleStr pebble_local_<sym> = pebble_rt_str_from_parts(ctx, parts, count);
		//
		// Each text part uses escapeCString (same as buildPrint); each bool
		// value part is validated against types.Bool and built with buildBoolExpr
		// (same scope as buildPrint's restricted bool-only interpolation).
		callExpr, err := st.buildInterpolatedStringParts(unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{isStr: true}
		return fmt.Sprintf("%sPebbleStr pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, statement.Symbol, callExpr, indent, statement.Symbol), nil
	}
	if initValue.Kind != tir.StringLiteral {
		return "", fmt.Errorf("%s declares a str-typed local initialized from a %s, want a StringLiteral (a string literal), a call to a str-returning helper, or a reference to a str-typed local in scope", context, initValue.Kind)
	}
	valueText, err := buildStrLiteralValue(initValue)
	if err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	scope[statement.Symbol] = localInfo{isStr: true}
	return fmt.Sprintf("%sPebbleStr pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, statement.Symbol, valueText, indent, statement.Symbol), nil
}

// buildFunctionLocalDeclaration builds one function-typed local's declaration:
// a `pebble_fnptr_<typeID>_t pebble_local_<symbol> = <init_expr>;` whose
// initializer is a function value — a bare top-level function reference (a
// HoistedFunctionValue, e.g. `var f fn(int, int) int = add;`) or a reference
// to an in-scope function-typed local (a SymbolValue, a function-to-function
// copy) — both built by buildFunctionValue. The local's C type is the function
// type's own pointer typedef, pebble_fnptr_<typeID>_t (see buildFunctionTypedef),
// so a bare function name (which decays to a function pointer of exactly that
// typedef's shape) needs no cast at the declaration site. The function type's
// own signature — its calling convention, parameter list, and result — is
// validated against this slice's supported shapes by
// validateFunctionTypeSignature before the typedef or declaration is emitted
// (a function type whose parameters/result mention anything other than the
// entry's width, uint, u64, or another fixed-width integer, bool, char, str,
// or f32/f64 parameters and the entry's width, u64, bool, char, f32/f64, or
// void result is a clean rejection naming what is unsupported).
// The scope entry records functionType so a later reference, reassignment, or
// indirect call resolves the local's declared function type. Like every local,
// the declaration is followed by a (void) cast against -Wunused-variable.
func buildFunctionLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	fnType := initValue.Type
	if !isFunctionType(snapshot, fnType) {
		return "", fmt.Errorf("%s declares a local of type %s, want a function type", context, describeType(snapshot, fnType))
	}
	if err := validateFunctionTypeSignature(snapshot, width, fnType); err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	valueText, err := buildFunctionValue(st, unit, snapshot, fileSet, initValue, scope, context, width)
	if err != nil {
		return "", err
	}
	scope[statement.Symbol] = localInfo{functionType: fnType}
	return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, functionTypeName(fnType), statement.Symbol, valueText, indent, statement.Symbol), nil
}

// buildPointerLocalDeclaration builds one pointer-typed local's declaration: a
// `<pointee_c_type> * pebble_local_<symbol> = <init_expr>;` whose initializer
// is an AddressOf expression (`let p *i32 = &y;`), another pointer-typed local
// (pointer copy), or a nil literal. The local's C type is the pointee's own
// C type name followed by ` *` (int32_t * for *i32, pebble_struct_<id>_t *
// for *Point, etc.), resolved by pointerTypeName from the pointer type's
// pointee. The scope entry records pointerType so a later dereference
// (*p) or address-of (&y) resolves the pointer type correctly. Like every
// scalar local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildPointerLocalDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	pointerTypeID := initValue.Type
	pointeeTypeID, ok := pointerPointeeType(snapshot, pointerTypeID)
	if !ok {
		return "", fmt.Errorf("%s declares a pointer-typed local with invalid pointer type", context)
	}
	ctypeName := pointerTypeNameForUnit(st, unit, snapshot, pointeeTypeID)
	if ctypeName == "" {
		return "", fmt.Errorf("%s declares a pointer-typed local with unsupported pointee type %s", context, describeType(snapshot, pointeeTypeID))
	}
	switch initValue.Kind {
	case tir.Load:
		fieldText, err := buildRuntimeValue(st, unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, fieldText, indent, statement.Symbol), nil
	case tir.AddressOf:
		// An address-of expression: `let p *i32 = &y;`. The AddressOf node
		// has one child (the place being addressed). The emitted C is
		// `<ctype> pebble_local_<sym> = &<place_lvalue>;`. An array-typed
		// place is a raw C array in this backend (int32_t a[3]), whose
		// address-of `&a` is `int32_t (*)[3]`, NOT the wrapped
		// pebble_array_<id>_t * the pointer local is declared with — so the
		// address is cast at the declaration site, the same
		// `(pebble_array_<id>_t *)(&a)` cast buildExpr's AddressOf case
		// emits for the value positions.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s address-of initializer has %d children, want exactly one", context, len(initValue.Children))
		}
		placeLValue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, initValue.Children[0], scope, width)
		if err != nil {
			return "", fmt.Errorf("%s address-of place: %v", context, err)
		}
		addressExpr := "&" + placeLValue
		if isArray(snapshot, placeType) {
			addressExpr = fmt.Sprintf("(%s)(%s)", arrayTypeName(placeType)+" *", addressExpr)
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, addressExpr, indent, statement.Symbol), nil
	case tir.SymbolValue:
		// A reference to another pointer-typed local: `let q *i32 = p;`.
		// The emitted C is a plain assignment.
		if _, declared := scope[initValue.Symbol]; !declared {
			return "", fmt.Errorf("%s references symbol %d, which is not a local in scope", context, initValue.Symbol)
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = pebble_local_%d;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, initValue.Symbol, indent, statement.Symbol), nil
	case tir.NilPointer:
		// A nil literal: `let p *i32 = nil;`. The emitted C uses NULL.
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = NULL;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, indent, statement.Symbol), nil
	case tir.DirectCall, tir.MethodCall:
		// A call to a pointer-returning helper used as the direct
		// initializer of a matching pointer-typed local: `let p *i32 =
		// helperReturningPointer();`.
		callPre, callText, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, callText, indent, statement.Symbol)), nil
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional whose payload is a pointer used as
		// the direct initializer of a matching pointer-typed local:
		// `let p *i32 = o!;`. When the unwrapped optional is a call result
		// (`let p *i32 = find(x)!;`), the call must be evaluated exactly once
		// (a call result is a C struct read back by value; naively embedding
		// it twice would run the call twice), so its result is hoisted into a
		// pebble_temp_<id> optional local first — the same evaluate-once /
		// reuse-twice pattern buildOptionalIntegerToEnumDeclaration uses for
		// its hoisted source temp, and only possible here because a local
		// declaration is a statement position with room for a preceding line.
		// A SymbolValue/Load child (an unwrap of an optional-typed local)
		// needs no hoisting and unwraps inline via buildExpr's pointer-branch
		// CheckedOptionalUnwrap case.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s force-unwrap initializer has %d child(ren), want exactly one optional value", context, len(initValue.Children))
		}
		child, ok := unit.Node(initValue.Children[0])
		if !ok {
			return "", fmt.Errorf("%s force-unwrap initializer references invalid child node %d", context, initValue.Children[0])
		}
		if child.Kind == tir.DirectCall || child.Kind == tir.MethodCall {
			callPre, callText, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, child, scope, width)
			if err != nil {
				return "", err
			}
			tempName := fmt.Sprintf("pebble_temp_%d", statement.Children[0])
			unwrapText := fmt.Sprintf("pebble_rt_checked_unwrap_ptr(%s.has_value, %s.value, %s)", tempName, tempName, buildSourceLoc(fileSet, initValue.Span))
			scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
			return withLeadingPre(callPre, indent, fmt.Sprintf("%s%s %s = %s;\n%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, optionalTypeName(child.Type), tempName, callText, indent, ctypeName, statement.Symbol, unwrapText, indent, statement.Symbol)), nil
		}
		unwrapText, err := buildExpr(st, unit, snapshot, fileSet, statement.Children[0], scope, width, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, unwrapText, indent, statement.Symbol), nil
	case tir.IndirectCall:
		callText, err := buildIndirectCall(st, unit, snapshot, fileSet, initValue, scope, width)
		if err != nil {
			return "", err
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, callText, indent, statement.Symbol), nil
	case tir.PointerCast:
		// An explicit pointer-to-pointer cast: `let q *void = p as *void;`.
		// The PointerCast node has one child (the source pointer value) and
		// its Type is the destination pointer type. The emitted C is a
		// simple assignment since C pointer types are already named.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s pointer cast initializer has %d children, want exactly one", context, len(initValue.Children))
		}
		childText, err := buildExpr(st, unit, snapshot, fileSet, initValue.Children[0], scope, width, width)
		if err != nil {
			return "", fmt.Errorf("%s pointer cast child: %v", context, err)
		}
		scope[statement.Symbol] = localInfo{pointerType: pointerTypeID}
		return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, ctypeName, statement.Symbol, childText, indent, statement.Symbol), nil
	default:
		return "", fmt.Errorf("%s declares a pointer-typed local initialized from a %s, want an AddressOf expression, another pointer local, a pointer-returning call, a pointer-to-pointer cast, a pointer-payload optional force-unwrap, or nil", context, initValue.Kind)
	}
}
