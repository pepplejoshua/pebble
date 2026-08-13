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

// resolveSliceInfo turns one collected slice TypeID into a sliceInfo with its
// element type resolved. The element type comes from the slice type's own
// Child() key, which for a Slice kind returns the element type.
func resolveSliceInfo(snapshot *types.Snapshot, id types.TypeID) (sliceInfo, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return sliceInfo{}, fmt.Errorf("slice type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Slice {
		return sliceInfo{}, fmt.Errorf("type %s is a %v, want a slice type", describeType(snapshot, id), key.Kind())
	}
	child, ok := key.Child()
	if !ok {
		return sliceInfo{}, fmt.Errorf("slice type %s has no element type", describeType(snapshot, id))
	}
	return sliceInfo{typ: id, elementType: child}, nil
}

// resolveStructInfo turns one collected struct TypeID into a structInfo with
// its fields in declared order. The declaration symbol comes from the type's
// own Nominal key (TypeKey.Nominal); the declared field order comes from the
// corresponding TypeDecl's Members (unit.TypeDeclarations), which lists the
// field symbols in the struct's source declaration order — NOT the
// construction-site order a RecordConstruct's Fields carry, which is why the
// order is resolved here rather than from any construction node. Each field's
// type comes from TypeDecl.MemberTypes. For a generic instantiation the
// TypeKey.Nominal arguments are the instantiation's concrete type arguments,
// so a member whose recorded type is one of the struct's own type parameters
// (the checker records the parameter's TypeID for a directly parameter-typed
// field) is substituted against those arguments — resolving Pair[K, V]'s `key
// K` to Pair[int, int]'s int and Pair[int, bool]'s int independently, from the
// instantiation's own evidence rather than the per-symbol fallback below.
// Members whose template the checker left unresolved (unless the usage-derived
// fieldTypes map provides a type) are rejected rather than guessed.
func resolveStructInfo(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID, fieldTypes map[symbol.SymbolID]types.TypeID) (structInfo, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return structInfo{}, fmt.Errorf("struct type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Nominal {
		return structInfo{}, fmt.Errorf("type %s is a %v, want a struct type", structTypeName(id), key.Kind())
	}
	decl, arguments, ok := key.Nominal()
	if !ok {
		return structInfo{}, fmt.Errorf("type %s has no nominal declaration", structTypeName(id))
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return structInfo{}, fmt.Errorf("struct type %s has no TypeDeclaration for symbol %d in the unit", structTypeName(id), decl)
	}
	var substitutions map[symbol.SymbolID]types.TypeID
	if len(arguments) > 0 {
		substitutions = structSubstitutions(unit, snapshot, decl, arguments)
	}
	fields := make([]structFieldInfo, len(typeDecl.Members))
	for i, member := range typeDecl.Members {
		fieldType := types.TypeID(0)
		if i < len(typeDecl.MemberTypes) {
			fieldType = typeDecl.MemberTypes[i]
		}
		if fieldType != 0 && substitutions != nil {
			substituted, err := snapshot.Substitute(fieldType, substitutions)
			if err != nil {
				return structInfo{}, fmt.Errorf("struct type %s field symbol %d type substitution: %v", structTypeName(id), member, err)
			}
			fieldType = substituted
		}
		if fieldType == 0 {
			// A member whose template wraps the struct's own parameter (e.g.
			// `?K` or `*K`) has no parameterized TypeID to substitute, so the
			// concrete field type is recovered per instantiation from the
			// struct's OWN construction evidence first: any RecordConstruct
			// whose type is exactly this instantiation (node.Type, not merely
			// the shared declaration) names the field's resolved type on its
			// value node. Two specializations share every field symbol, so the
			// per-symbol fieldTypes fallback below would let one
			// instantiation's type win over the other's; the scoped recovery
			// is what keeps them distinct.
			fieldType, ok = instantiatedFieldType(unit, id, member)
			if !ok {
				fieldType, ok = fieldTypes[member]
			}
			if !ok {
				return structInfo{}, fmt.Errorf("struct type %s field symbol %d has no resolvable type in the unit", structTypeName(id), member)
			}
		}
		fields[i] = structFieldInfo{member: member, typ: fieldType}
	}
	return structInfo{typ: id, decl: decl, fields: fields}, nil
}

// resolveUnionInfo turns one collected union TypeID into a unionInfo with its
// variants in declared order and its constructed members resolved. The
// declaration symbol comes from the type's own Nominal key (TypeKey.Nominal);
// the declared variant order comes from the corresponding TypeDecl's Members
// (unit.TypeDeclarations), the same mechanism resolveEnumInfo uses for a plain
// enum (the TypeDeclaration *node* carries only the symbol, so the container is
// authoritative). The constructed members come from the payloads map the walk
// accumulated (member symbol -> resolved payload type), listed in declared
// variant order so the C union member order is deterministic regardless of
// construction-site order. The type must actually be enum-shaped, not a struct
// that shares the Nominal key shape — isEnumType distinguishes the two from the
// unit's own node graph, so a collected non-enum Nominal type is a clean
// rejection, not a guessed layout.
func resolveUnionInfo(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID, payloads map[symbol.SymbolID]types.TypeID) (unionInfo, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return unionInfo{}, fmt.Errorf("union type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Nominal {
		return unionInfo{}, fmt.Errorf("type %s is a %v, want a tagged-union type", unionTypeName(id), key.Kind())
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return unionInfo{}, fmt.Errorf("type %s has no nominal declaration", unionTypeName(id))
	}
	if !isEnumType(unit, snapshot, id) {
		return unionInfo{}, fmt.Errorf("type %s is not a tagged-union type (its declaration symbol %d's members resolve to struct fields, not enum variants)", unionTypeName(id), decl)
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return unionInfo{}, fmt.Errorf("union type %s has no TypeDeclaration for symbol %d in the unit", unionTypeName(id), decl)
	}
	if len(typeDecl.Members) == 0 {
		return unionInfo{}, fmt.Errorf("union type %s has no declared variants", unionTypeName(id))
	}
	members := make([]unionMemberInfo, 0, len(payloads))
	for _, variant := range typeDecl.Members {
		if payloadType, ok := payloads[variant]; ok {
			members = append(members, unionMemberInfo{member: variant, payloadType: payloadType})
		}
	}
	return unionInfo{typ: id, decl: decl, variants: append([]symbol.SymbolID(nil), typeDecl.Members...), members: members}, nil
}

// resolveEnumInfo turns one collected enum TypeID into an enumInfo with its
// variants in declared order. The declaration symbol comes from the type's own
// Nominal key (TypeKey.Nominal); the declared variant order comes from the
// corresponding TypeDecl's Members (unit.TypeDeclarations), which lists the
// variant symbols in the enum's source declaration order — the same mechanism
// resolveStructInfo uses for structs (the TypeDeclaration *node* carries only
// the symbol, so the container is authoritative). The type must actually be a
// plain enum, not a struct that shares the Nominal key shape — isEnumType
// distinguishes the two from the unit's own node graph, so a collected
// non-enum Nominal type is a clean rejection, not a guessed layout.
func resolveEnumInfo(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) (enumInfo, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return enumInfo{}, fmt.Errorf("enum type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Nominal {
		return enumInfo{}, fmt.Errorf("type %s is a %v, want an enum type", enumTypeName(id), key.Kind())
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return enumInfo{}, fmt.Errorf("type %s has no nominal declaration", enumTypeName(id))
	}
	if !isEnumType(unit, snapshot, id) {
		return enumInfo{}, fmt.Errorf("type %s is not a plain enum (its declaration symbol %d's members resolve to struct fields, not enum variants)", enumTypeName(id), decl)
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return enumInfo{}, fmt.Errorf("enum type %s has no TypeDeclaration for symbol %d in the unit", enumTypeName(id), decl)
	}
	if len(typeDecl.Members) == 0 {
		return enumInfo{}, fmt.Errorf("enum type %s has no declared variants", enumTypeName(id))
	}
	return enumInfo{typ: id, decl: decl, variants: append([]symbol.SymbolID(nil), typeDecl.Members...)}, nil
}

// containsVariant reports whether id is one of the variant symbols in variants.
func containsVariant(variants []symbol.SymbolID, id symbol.SymbolID) bool {
	for _, variant := range variants {
		if variant == id {
			return true
		}
	}
	return false
}

// buildRuntimeAllocatorCallbackAdapter validates one Allocator callback field's
// construction value (a member of AllocatorAlloc/AllocatorRealloc/AllocatorFree)
// and, when it is a supported source-level function reference, registers — into
// the current Emit's allocatorAdapters map (emitState.allocatorAdapters),
// deduplicated by bridge name — the file-scope C bridge that adapts the user's
// emitted helper into the field's runtime ABI type, returning the bridge's C
// name for the construction's designated initializer to reference. Anything
// else is a clean rejection naming what was found. The checker already requires
// a callback field's value to be exactly the field's function type
// (fn(ctx *void, size uint) *void and friends), so the only reachable shape is
// a HoistedFunctionValue; everything else is defense for hand-built IR.
func buildRuntimeAllocatorCallbackAdapter(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, member symbol.SymbolID, valueNode tir.Node, context string) (string, error) {
	if valueNode.Kind != tir.HoistedFunctionValue {
		return "", fmt.Errorf("%s constructs an Allocator callback field from a %s, want a reference to a top-level function (e.g. `alloc = my_alloc`)", context, valueNode.Kind)
	}
	if !isFunctionType(snapshot, valueNode.Type) {
		return "", fmt.Errorf("%s constructs an Allocator callback field from a %s, want a function value", context, describeType(snapshot, valueNode.Type))
	}
	decl, err := findFunctionDeclaration(unit, valueNode.Symbol, "Allocator callback function")
	if err != nil {
		return "", err
	}
	info := unit.Runtime()
	name := ""
	prototype := ""
	definition := ""
	switch member {
	case info.AllocatorAlloc:
		name = fmt.Sprintf("pebble_rt_alloc_adapter_%d", decl.Symbol)
		prototype = fmt.Sprintf("static void *%s(PebbleContext *ctx, size_t size);", name)
		definition = fmt.Sprintf("static void *%s(PebbleContext *ctx, size_t size) {\n    return %s(ctx, (void *)ctx, size);\n}", name, helperCName(decl, nil))
	case info.AllocatorRealloc:
		name = fmt.Sprintf("pebble_rt_realloc_adapter_%d", decl.Symbol)
		prototype = fmt.Sprintf("static void *%s(PebbleContext *ctx, void *ptr, size_t new_size);", name)
		definition = fmt.Sprintf("static void *%s(PebbleContext *ctx, void *ptr, size_t new_size) {\n    return %s(ctx, (void *)ctx, ptr, new_size);\n}", name, helperCName(decl, nil))
	case info.AllocatorFree:
		name = fmt.Sprintf("pebble_rt_free_adapter_%d", decl.Symbol)
		prototype = fmt.Sprintf("static void %s(PebbleContext *ctx, void *ptr);", name)
		definition = fmt.Sprintf("static void %s(PebbleContext *ctx, void *ptr) {\n    %s(ctx, (void *)ctx, ptr);\n}", name, helperCName(decl, nil))
	default:
		return "", fmt.Errorf("%s references Allocator callback field %d, which is not alloc, realloc, or free", context, member)
	}
	if _, exists := st.allocatorAdapters[name]; exists {
		return name, nil
	}
	st.allocatorAdapters[name] = runtimeAllocatorAdapter{name: name, prototype: prototype, definition: definition}
	return name, nil
}

// buildRuntimeAllocatorBraceList validates one runtime Allocator literal's
// construction fields and builds its C99 designated-initializer brace-list
// content, `{ .state = <ptr>, .alloc = <bridge>, .realloc = <bridge>, .free =
// <bridge> }` — the part after the opening brace. The field names come from
// runtimeFieldName (ptr→state, alloc→alloc, realloc→realloc, free→free), one
// designated initializer per construction field, so the construction-site
// field order a RecordConstruct's Fields carry needs no reordering. The four
// known Allocator fields are handled: ptr's value is built under the pointer
// grammar (nil, a pointer-typed local, an address-of cast, a pointer-returning
// call), and each callback field's value must be a reference to a top-level
// source function, bridged into the runtime callback ABI by
// buildRuntimeAllocatorCallbackAdapter. Anything else — a non-Allocator runtime
// type, a wrong field count, an unknown or duplicated field, a malformed field
// value — is a clean rejection naming what was found. The same brace-list
// content serves both a local declaration
// (buildRuntimeAllocatorRecordDeclaration) and a value position
// (buildStructValueExpr, where it is wrapped as a (PebbleAllocator){ ... }
// compound literal), so a constructed Allocator can be returned from a
// function or passed inline as an argument with the exact runtime ABI shape.
func buildRuntimeAllocatorBraceList(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, initValue tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	if runtimeType(unit, snapshot, initValue.Type) != symbol.RuntimeAllocator {
		return "", fmt.Errorf("%s constructs a runtime-typed value of type %s, which is supported only for the built-in Allocator", context, runtimeTypeName(unit, snapshot, initValue.Type))
	}
	info := unit.Runtime()
	if len(initValue.Fields) != 4 {
		return "", fmt.Errorf("%s constructs an Allocator with %d field initializer(s), want exactly 4 (ptr, alloc, realloc, free)", context, len(initValue.Fields))
	}
	inits := make([]string, 0, len(initValue.Fields))
	seen := make(map[symbol.SymbolID]bool, len(initValue.Fields))
	for _, field := range initValue.Fields {
		if seen[field.Field] {
			return "", fmt.Errorf("%s constructs an Allocator with a duplicate initializer for field %d", context, field.Field)
		}
		seen[field.Field] = true
		cname, mapped := runtimeFieldName(unit, initValue.Type, field.Field)
		if !mapped {
			return "", fmt.Errorf("%s constructs an Allocator with a field that is not one of ptr, alloc, realloc, or free", context)
		}
		valueNode, ok := unit.Node(field.Value)
		if !ok {
			return "", fmt.Errorf("%s constructs an Allocator referencing invalid field value node %d", context, field.Value)
		}
		var expr string
		var err error
		switch field.Field {
		case info.AllocatorPtr:
			// ptr maps to the C state field, typed void *. The value is built
			// under the same pointer grammar buildExpr's pointer branch uses
			// (nil, a reference to a pointer-typed local, an address-of cast, a
			// pointer-returning call), and its C expression already matches the
			// void * state field type with no cast.
			if !isPointer(snapshot, valueNode.Type) {
				return "", fmt.Errorf("%s constructs an Allocator whose ptr initializer is %s, want a pointer value", context, describeType(snapshot, valueNode.Type))
			}
			expr, err = buildExpr(st, unit, snapshot, fileSet, field.Value, scope, width, width)
		case info.AllocatorAlloc, info.AllocatorRealloc, info.AllocatorFree:
			// alloc/realloc/free map to the runtime ABI callback fields, typed
			// PebbleAllocFn/PebbleReallocFn/PebbleFreeFn. The construction value
			// is bridged into the runtime ABI by a file-scope adapter, whose C
			// name the designated initializer references directly (a plain
			// compatible assignment, no function-pointer cast in emitted C).
			expr, err = buildRuntimeAllocatorCallbackAdapter(st, unit, snapshot, field.Field, valueNode, context)
		default:
			return "", fmt.Errorf("%s constructs an Allocator with unknown field %d", context, field.Field)
		}
		if err != nil {
			return "", err
		}
		inits = append(inits, fmt.Sprintf(".%s = %s", cname, expr))
	}
	return strings.Join(inits, ", "), nil
}

// buildRuntimeAllocatorRecordDeclaration builds one runtime Allocator literal's
// C local declaration:
//
//	PebbleAllocator pebble_local_<sym> = { .state = <ptr>, .alloc = <bridge>,
//	                                        .realloc = <bridge>, .free = <bridge> };
//	(void)pebble_local_<sym>;
//
// a C99 designated-initializer brace list over the hand-written runtime
// PebbleAllocator struct, delegating the brace-list content to
// buildRuntimeAllocatorBraceList. Like every local, the declaration is
// followed by a (void) cast against -Wunused-variable.
func buildRuntimeAllocatorRecordDeclaration(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	inits, err := buildRuntimeAllocatorBraceList(st, unit, snapshot, fileSet, initValue, scope, context, width)
	if err != nil {
		return "", err
	}
	// See buildRuntimeLocalDeclaration: the Allocator local is also seeded as a
	// struct-typed local so the general by-value movement machinery (argument,
	// return, field store) accepts it like the ordinary struct it is.
	scope[statement.Symbol] = localInfo{structType: initValue.Type, runtimeType: initValue.Type}
	return fmt.Sprintf("%s%s pebble_local_%d = { %s };\n%s(void)pebble_local_%d;", indent, runtimeTypeName(unit, snapshot, initValue.Type), statement.Symbol, inits, indent, statement.Symbol), nil
}

// tupleCoerceDestinationType finds the tuple type whose element list is the
// destination list carried by a TupleCoerce's TypeArgs. TupleCoerce.Type is the
// source tuple type, so it cannot name the local's destination C struct.
func tupleCoerceDestinationType(snapshot *types.Snapshot, typeArgs []types.TypeID) (types.TypeID, error) {
	if len(typeArgs) == 0 {
		return 0, fmt.Errorf("tuple coercion has no destination element types")
	}
	for id := range snapshot.IDs() {
		key, ok := snapshot.Key(id)
		if !ok || key.Kind() != types.Tuple {
			continue
		}
		elements, ok := key.Elements()
		if !ok || len(elements) != len(typeArgs) {
			continue
		}
		matches := true
		for i := range elements {
			if elements[i] != typeArgs[i] {
				matches = false
				break
			}
		}
		if matches {
			return id, nil
		}
	}
	return 0, fmt.Errorf("no tuple type matches the destination element types")
}

// buildTupleBraceList validates one TupleValue node's element list and builds
// its brace-list content, `{ <e0>, <e1>, ... }`, with each element expression
// built by the grammar its own element type selects — buildExpr at the
// element's OWN resolved integer width for an integer element (any fixed-width
// integer, mirroring buildArrayBraceElements), buildBoolExpr for a bool
// element, buildCharOperand for a char element, buildStrOperand for a str
// element, buildFloatExpr at the element's own float kind for an f32/f64
// element, and buildNestedAggregateValue for a tuple/struct/optional element.
// Every element type must be one of those — anything else (an enum, a
// pointer, a slice, a function type) is a clean rejection naming the element
// position, matching the tuple typedef's own element gate. context names the
// enclosing construct in error messages. The function is shared by the two
// places a TupleValue's elements are built (10.25): a tuple-typed local's
// declaration initializer (buildTupleLocalDeclaration embeds the returned
// brace list in the declaration statement) and a freshly-constructed tuple
// built inline as a call argument (buildTupleValueExpr wraps the same brace
// list in a compound-literal cast), so element-type validation and the
// per-element-type build dispatch live in exactly one place.
func buildTupleBraceList(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	key, ok := snapshot.Key(node.Type)
	if !ok {
		return "", fmt.Errorf("%s contains a tuple value whose type %d is not in the type snapshot", context, node.Type)
	}
	elements, ok := key.Elements()
	if !ok {
		return "", fmt.Errorf("%s contains a tuple value of type %s, which has no element list", context, tupleTypeName(node.Type))
	}
	if len(node.Children) != len(elements) {
		return "", fmt.Errorf("%s contains a tuple value of type %s with %d element expression(s), want %d (one per declared element)", context, tupleTypeName(node.Type), len(node.Children), len(elements))
	}
	return buildTupleBraceElements(st, unit, snapshot, fileSet, node.Type, elements, node.Children, scope, context, width)
}

func buildTupleBraceElements(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, tupleType types.TypeID, elements []types.TypeID, children []tir.NodeID, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	exprs := make([]string, len(elements))
	for i, elementType := range elements {
		var elementExpr string
		var err error
		switch {
		case isBool(snapshot, elementType):
			elementExpr, err = buildBoolExpr(st, unit, snapshot, fileSet, children[i], scope, width)
		case isChar(snapshot, elementType):
			elementExpr, err = buildCharOperand(st, unit, snapshot, fileSet, children[i], scope, width)
		case isStr(snapshot, elementType):
			elementExpr, err = buildStrOperand(st, unit, snapshot, fileSet, children[i], scope, width)
		case isFloat(snapshot, elementType):
			elementExpr, err = buildFloatExpr(st, unit, snapshot, fileSet, children[i], scope, resolvedFloatKind(snapshot, elementType), width)
		case isTuple(snapshot, elementType), isStruct(snapshot, elementType), isOptional(snapshot, elementType):
			elementExpr, err = buildNestedAggregateValue(st, unit, snapshot, fileSet, children[i], scope, elementType, context, width)
		case isAbstractInt(snapshot, elementType):
			elementExpr, err = buildExpr(st, unit, snapshot, fileSet, children[i], scope, width, width)
		default:
			if elementWidth, integerElement := resolvedBuiltin(snapshot, elementType); integerElement && cType(elementWidth) != "" {
				elementExpr, err = buildExpr(st, unit, snapshot, fileSet, children[i], scope, elementWidth, width)
			} else {
				return "", fmt.Errorf("%s contains a tuple value of type %s whose element %d is %s, want a fixed-width integer, bool, char, str, f32, f64, or a tuple/struct type", context, tupleTypeName(tupleType), i, describeType(snapshot, elementType))
			}
		}
		if err != nil {
			return "", err
		}
		exprs[i] = elementExpr
	}
	return "{ " + strings.Join(exprs, ", ") + " }", nil
}

func buildRawSliceConstruction(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, context string) (string, error) {
	if len(node.Children) != 2 {
		return "", fmt.Errorf("%s SliceFromRaw has %d children, want two", context, len(node.Children))
	}
	ptr, err := buildExpr(st, unit, snapshot, fileSet, node.Children[0], scope, width, width)
	if err != nil {
		return "", err
	}
	countNode, ok := unit.Node(node.Children[1])
	if !ok {
		return "", fmt.Errorf("%s SliceFromRaw references invalid count node", context)
	}
	var count string
	if countNode.Kind == tir.SymbolValue {
		if _, declared := scope[countNode.Symbol]; !declared {
			return "", fmt.Errorf("%s slice count references symbol %d outside the current scope", context, countNode.Symbol)
		}
		count = fmt.Sprintf("pebble_local_%d", countNode.Symbol)
	} else if countNode.Kind == tir.IntegerLiteral {
		litWidth, _ := resolvedBuiltin(snapshot, countNode.Type)
		count = integerLiteralText(countNode.Literal.IntegerNum, litWidth)
	} else {
		count, err = buildUintExpr(st, unit, snapshot, fileSet, node.Children[1], scope, width)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("(%s){ .data = %s, .len = (size_t)(%s) }", sliceTypeName(node.Type), ptr, count), nil
}

// buildSliceConstruction validates one CheckedSlice node (a slice expression
// `a[start:end]`) and builds the two pieces of C text its construction needs:
// a temp-declaration statement holding the checked-start result, and the
// compound-literal construction expression that uses that temp for both its
// .data pointer offset and its .len subtraction. The two-statement shape is
// required because the temp can't be a sub-expression of the very compound
// literal it initializes (the pointer offset would reference a value not yet
// computed in a well-defined order within one expression) — the same
// construction shape 10.37 established for a slice-typed local's declaration,
// kept here so both callers share one source of truth rather than two copies
// that could drift. tempName is the deterministic C identifier of the temp
// variable, derived by the caller from a stable identity (a slice local's own
// declaration symbol for a local declaration; the return value node's NodeID
// for a slice-returning helper's tail return). backingName is the deterministic
// C identifier of the hidden backing array an ArrayValue base constructs (the
// array-literal slice-initializer shape), used only when the base is an
// ArrayValue. The declaration statement (with
// indent) and the construction expression (unindented, a C99 compound literal)
// are returned separately so each caller assembles them into its own statement
// shape: buildSliceLocalDeclaration embeds the expression in a local
// declaration statement, and buildSliceReturnValue hands the declaration back
// to buildBlock/buildSwitchCaseBody to thread in as an extra pre-return
// statement before the final return line. The construction reuses the exact
// validation 10.37 established: the base must be an array-typed local in
// scope, the slice's element type must equal the base array's element type,
// and that element type must be the entry's resolved width or bool.
func buildSliceConstruction(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, tempName, backingName string) (string, string, error) {
	if initValue.Kind != tir.CheckedSlice {
		return "", "", fmt.Errorf("%s slice construction is a %s, want a CheckedSlice", context, initValue.Kind)
	}
	if len(initValue.Children) < 1 {
		return "", "", fmt.Errorf("%s CheckedSlice has %d child(ren), want at least one (the base array)", context, len(initValue.Children))
	}
	baseNode, ok := unit.Node(initValue.Children[0])
	if !ok {
		return "", "", fmt.Errorf("%s CheckedSlice references invalid base node %d", context, initValue.Children[0])
	}
	// Four base shapes are accepted:
	//
	// 1. A SymbolValue naming an ARRAY-typed local in scope (the original,
	//    unchanged path): the array local decays to a pointer to its first
	//    element, and the array's length is a compile-time constant
	//    (arrayLengthLiteral) used both as the default end bound and as the
	//    upper bound the runtime helper validates the range against.
	//
	// 2. A SLICE-typed value base — re-slicing an EXISTING slice — in two
	//    lowered shapes: a Load of a slice-typed place (`self.data[:self.len]`,
	//    std/string.peb's String::as_slice, lowered to a Load(FieldPlace)
	//    reading the slice-typed struct field) or a SymbolValue naming an
	//    already-declared slice-typed local in scope (`s[1:3]` where s is a
	//    slice local). There is no compile-time length for a slice base: the
	//    new slice's .data pointer must offset the EXISTING slice's own runtime
	//    .data, and the upper bound is the EXISTING slice's runtime .len field.
	//
	// 3. An ArrayValue literal base — an array literal directly initializing a
	//    slice-typed binding (`var s []int = [1, 2, 3];`, lowered by the
	//    checker to a full CheckedSlice over the ArrayValue). The literal is
	//    constructed into a hidden backing array local named by backingName and
	//    the slice slices that array, mirroring exactly what the two-step
	//    workaround (`var arr [N]T = [...]; var s []T = arr[:];`) lowers to.
	isSliceBase := baseNode.Kind == tir.Load && isSlice(snapshot, baseNode.Type)
	isArrayLiteralBase := baseNode.Kind == tir.ArrayValue
	baseSymbolIsSlice := false
	var baseInfo localInfo
	if !isSliceBase && !isArrayLiteralBase {
		if baseNode.Kind != tir.SymbolValue {
			return "", "", fmt.Errorf("%s slice base is a %s, want a SymbolValue naming an array or slice local", context, baseNode.Kind)
		}
		var declared bool
		baseInfo, declared = scope[baseNode.Symbol]
		if !declared {
			return "", "", fmt.Errorf("%s slice base references symbol %d, which is not a local in scope", context, baseNode.Symbol)
		}
		if baseInfo.sliceType != 0 {
			// A SymbolValue naming a slice-typed local base — `s[1:3]` where s
			// is a slice local. The base slice's own .data/.len fields are the
			// new slice's data pointer and upper bound, exactly like the
			// Load(FieldPlace) slice base below; only the C lvalue spelling of
			// the base (the local's own name) differs.
			isSliceBase = true
			baseSymbolIsSlice = true
		} else if baseInfo.array == 0 {
			return "", "", fmt.Errorf("%s slice base is not an array-typed local", context)
		}
	}
	sliceType := initValue.Type
	sliceKey, ok := snapshot.Key(sliceType)
	if !ok {
		return "", "", fmt.Errorf("%s slice type %d is not in the type snapshot", context, sliceType)
	}
	sliceElementType, ok := sliceKey.Child()
	if !ok {
		return "", "", fmt.Errorf("%s slice type %s has no element type", context, describeType(snapshot, sliceType))
	}
	if !isSupportedSliceElementType(unit, snapshot, sliceElementType) {
		return "", "", fmt.Errorf("%s slice element type is %s, want a fixed-width integer, char, bool, str, tuple, optional, struct, or enum", context, describeType(snapshot, sliceElementType))
	}
	// Per-base resolve the element type the new slice's element type must
	// match, plus the base-specific length and data-pointer expressions. The
	// array base's validations are exactly the original ones; the slice base
	// is the parallel Load(FieldPlace) path.
	var lengthLiteral, defaultEnd, dataExpr, backingDecl string
	if !isSliceBase && isArrayLiteralBase {
		// An ArrayValue literal base: construct the literal's elements into a
		// hidden backing array local (named by backingName) and slice that
		// array, mirroring exactly the two-step workaround's lowering. The
		// element validation is the same the array-typed-local path applies
		// (buildArrayLocalDeclaration), so the literal only builds when the
		// slice would have been buildable from a real [N]T local.
		arrayKey, ok := snapshot.Key(baseNode.Type)
		if !ok {
			return "", "", fmt.Errorf("%s base array literal type %d is not in the type snapshot", context, baseNode.Type)
		}
		length, arrayElementType, ok := arrayKey.Array()
		if !ok {
			return "", "", fmt.Errorf("%s base array literal is not an array type", context)
		}
		if sliceElementType != arrayElementType {
			return "", "", fmt.Errorf("%s slice element type %s does not match base array literal element type %s", context, describeType(snapshot, sliceElementType), describeType(snapshot, arrayElementType))
		}
		if _, err := arrayLengthLiteral(length, width); err != nil {
			return "", "", fmt.Errorf("%s: %v", context, err)
		}
		if len(baseNode.Children) != int(length) {
			return "", "", fmt.Errorf("%s base array literal has %d element expression(s), want %d", context, len(baseNode.Children), length)
		}
		exprs, err := buildArrayBraceElements(st, unit, snapshot, fileSet, baseNode, scope, context, width, arrayElementType)
		if err != nil {
			return "", "", err
		}
		// The hidden backing array's element C type is the slice element's own
		// C type (the equality check above guarantees they are identical), so
		// it is resolved by sliceElementCType — which admits a plain enum
		// element as its own pebble_enum_<typeID>_t — rather than
		// arrayElementCType, which deliberately still rejects enum-typed array
		// elements (enum-typed array element support stays out of scope).
		elementCType, err := sliceElementCType(unit, snapshot, width, arrayElementType)
		if err != nil {
			return "", "", fmt.Errorf("%s: %v", context, err)
		}
		lengthLiteral, _ = arrayLengthLiteral(length, width)
		defaultEnd = fmt.Sprintf("%d", length)
		dataExpr = backingName
		backingDecl = fmt.Sprintf("%s%s %s[%d] = { %s };", indent, elementCType, backingName, length, strings.Join(exprs, ", "))
	} else if !isSliceBase {
		arrayKey, ok := snapshot.Key(baseInfo.array)
		if !ok {
			return "", "", fmt.Errorf("%s base array type %d is not in the type snapshot", context, baseInfo.array)
		}
		length, arrayElementType, ok := arrayKey.Array()
		if !ok {
			return "", "", fmt.Errorf("%s base is not an array type", context)
		}
		if sliceElementType != arrayElementType {
			return "", "", fmt.Errorf("%s slice element type %s does not match base array element type %s", context, describeType(snapshot, sliceElementType), describeType(snapshot, arrayElementType))
		}
		if _, err := arrayLengthLiteral(length, width); err != nil {
			return "", "", fmt.Errorf("%s: %v", context, err)
		}
		lengthLiteral, _ = arrayLengthLiteral(length, width)
		defaultEnd = fmt.Sprintf("%d", length)
		dataExpr = fmt.Sprintf("pebble_local_%d", baseNode.Symbol)
		if baseInfo.arrayWrapped {
			dataExpr += ".data"
		}
	} else {
		// A slice-typed base value: either a Load of a slice-typed place (the
		// base slice value's own C lvalue, built by buildPlaceLValue, the same
		// projection a slice-typed struct field read in any other value
		// position uses) or a SymbolValue naming an already-declared slice
		// local (whose C lvalue is its own pebble_local_<symbol> name). Either
		// way the base's .data and .len sub-fields are the base slice's own
		// storage pointer and runtime length.
		var baseLvalue string
		var baseType types.TypeID
		if baseSymbolIsSlice {
			baseLvalue = fmt.Sprintf("pebble_local_%d", baseNode.Symbol)
			baseType = baseNode.Type
		} else {
			if len(baseNode.Children) != 1 {
				return "", "", fmt.Errorf("%s slice base Load has %d child(ren), want exactly one place", context, len(baseNode.Children))
			}
			var err error
			baseLvalue, baseType, err = buildPlaceLValue(st, unit, snapshot, fileSet, baseNode.Children[0], scope, width)
			if err != nil {
				return "", "", fmt.Errorf("%s slice base read: %v", context, err)
			}
		}
		if !isSlice(snapshot, baseType) {
			return "", "", fmt.Errorf("%s slice base reads a value of type %s, want a slice-typed value", context, describeType(snapshot, baseType))
		}
		baseSliceKey, ok := snapshot.Key(baseType)
		if !ok {
			return "", "", fmt.Errorf("%s base slice type %d is not in the type snapshot", context, baseType)
		}
		baseSliceElementType, ok := baseSliceKey.Child()
		if !ok {
			return "", "", fmt.Errorf("%s base slice type %s has no element type", context, describeType(snapshot, baseType))
		}
		if sliceElementType != baseSliceElementType {
			return "", "", fmt.Errorf("%s slice element type %s does not match base slice element type %s", context, describeType(snapshot, sliceElementType), describeType(snapshot, baseSliceElementType))
		}
		lengthLiteral = baseLvalue + ".len"
		defaultEnd = baseLvalue + ".len"
		dataExpr = baseLvalue + ".data"
	}
	// Extract start and end bounds from children. Children layout is
	// [base, start?, end?] with presence determined by
	// SliceStartPresent/SliceEndPresent.
	childIdx := 1
	var startExpr, endExpr string
	if initValue.SliceStartPresent {
		if childIdx >= len(initValue.Children) {
			return "", "", fmt.Errorf("%s CheckedSlice claims start present but has no start child", context)
		}
		startExpr = buildSliceBoundExpr(st, unit, snapshot, fileSet, initValue.Children[childIdx], scope, width, context)
		if startExpr == "" {
			return "", "", fmt.Errorf("%s failed to build slice start bound", context)
		}
		childIdx++
	} else {
		startExpr = "0"
	}
	if initValue.SliceEndPresent {
		if childIdx >= len(initValue.Children) {
			return "", "", fmt.Errorf("%s CheckedSlice claims end present but has no end child", context)
		}
		endExpr = buildSliceBoundExpr(st, unit, snapshot, fileSet, initValue.Children[childIdx], scope, width, context)
		if endExpr == "" {
			return "", "", fmt.Errorf("%s failed to build slice end bound", context)
		}
		childIdx++
	} else {
		endExpr = defaultEnd
	}
	if _, err := sliceElementCType(unit, snapshot, width, sliceElementType); err != nil {
		return "", "", fmt.Errorf("%s: %v", context, err)
	}
	startArg := startExpr
	if !initValue.SliceStartPresent {
		startArg = "0"
	}
	endArg := endExpr
	if !initValue.SliceEndPresent {
		endArg = lengthLiteral
	}
	sliceCType := sliceTypeName(sliceType)
	// Emit as two statements: first the checked-start call stored in a temp,
	// then the struct construction using the temp. The temp is declared at the
	// entry's own resolved width (cType(width)), matching whichever of
	// pebble_rt_checked_slice_start_i32/_i64 checkedSuffix(width) selects —
	// declaring it as a fixed int32_t regardless of width would silently
	// narrow an i64 entry's checked-start result.
	tempDecl := fmt.Sprintf("%s%s %s = pebble_rt_checked_slice_start_%s(%s, %s, %s, %s);", indent, cType(width), tempName, checkedSuffix(width), startArg, endArg, lengthLiteral, buildSourceLoc(fileSet, initValue.Span))
	if backingDecl != "" {
		// An ArrayValue base carries its hidden backing-array declaration as a
		// leading statement ahead of the checked-start temp, so the emitted
		// sequence is: declare the array from the literal's elements, compute
		// the checked start, then construct the slice struct over it — the
		// exact statement order the two-step workaround compiles to.
		tempDecl = backingDecl + "\n" + tempDecl
	}
	constructionExpr := fmt.Sprintf("(%s){ .data = %s + %s, .len = (size_t)(%s - %s) }", sliceCType, dataExpr, tempName, endExpr, tempName)
	return tempDecl, constructionExpr, nil
}

// buildSliceBoundExpr builds the C expression for one slice bound (start or
// end). The bound may be an integer literal or a reference to a local.
func buildSliceBoundExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, nodeID tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, context string) string {
	boundNode, ok := unit.Node(nodeID)
	if !ok {
		return ""
	}
	if boundNode.Kind == tir.IntegerLiteral && boundNode.Type == snapshot.Builtins().Int {
		return boundNode.Literal.IntegerNum
	}
	if boundNode.Kind == tir.SymbolValue {
		if _, declared := scope[boundNode.Symbol]; declared {
			return fmt.Sprintf("pebble_local_%d", boundNode.Symbol)
		}
	}
	if isUint(snapshot, boundNode.Type) {
		// A uint-typed slice bound (a range-loop iterator or uint local whose
		// type the checker anchored to uint): built by the dedicated uint
		// grammar, mirroring the slice/array index dispatch — the general
		// buildExpr path rejects non-entry-width types.
		expr, err := buildUintExpr(st, unit, snapshot, fileSet, nodeID, scope, width)
		if err != nil {
			return ""
		}
		return expr
	}
	expr, err := buildExpr(st, unit, snapshot, fileSet, nodeID, scope, width, width)
	if err != nil {
		return ""
	}
	return expr
}

// zeroOptionalPayloadLiteral is the C literal a NoneOptional's irrelevant
// .value field is initialized with — a bare 0 for a scalar payload type
// (int/bool/enum, all of which accept a plain 0 as a valid, warning-clean
// initializer), or the aggregate zero-initializer {0} for a struct/tuple
// payload (a bare 0 there triggers -Wmissing-field-initializers /
// -Wmissing-braces under -Werror, since the .value field's own C type is a
// struct, not a scalar).
func zeroOptionalPayloadLiteral(unit *tir.Unit, snapshot *types.Snapshot, payloadType types.TypeID) string {
	if isTuple(snapshot, payloadType) {
		return "{0}"
	}
	// An array-typed or slice-typed payload's .value field is a C struct
	// typedef (pebble_array_<typeID>_t with a `.data` member, or
	// pebble_slice_<typeID>_t with `.data`/`.len` — see optionalPayloadCType),
	// never a scalar: it needs the aggregate {0} zero-initializer exactly like
	// a tuple/struct payload, so a bare 0 would trigger
	// -Wmissing-field-initializers / -Wmissing-braces under -Werror.
	if isArray(snapshot, payloadType) {
		return "{0}"
	}
	if isSlice(snapshot, payloadType) {
		return "{0}"
	}
	// A tagged-union payload's C type is the union's own tag-plus-payload
	// struct (see optionalPayloadCType), not a scalar: it needs the aggregate
	// {0} zero-initializer exactly like a tuple/struct payload, so it is
	// checked BEFORE the isStruct/isEnumType split below (a union is both
	// Nominal and enum-shaped, so the plain-enum scalar-0 path below would
	// otherwise return a bare 0 that -Wmissing-braces rejects for the struct
	// .value field).
	if isTaggedUnionType(unit, snapshot, payloadType) {
		return "{0}"
	}
	// isStruct also reports true for an enum payload (both are Nominal keys,
	// indistinguishable at this level) — an enum's C type is a plain scalar
	// C enum, not a struct, so it must NOT take the aggregate {0} literal (a
	// brace-enclosed initializer around a scalar is itself a clean
	// -Wmissing-braces target). isEnumType resolves the ambiguity the same
	// way every other struct/enum-payload site in this file already does.
	if isStruct(snapshot, payloadType) && !isEnumType(unit, snapshot, payloadType) {
		return "{0}"
	}
	return "0"
}

// buildOptionalArrayPayload builds the C expression text for an array-typed
// optional payload's construction value — the value assigned to the optional
// struct's .value field, whose C type is the array's own
// pebble_array_<typeID>_t typedef (see optionalPayloadCType). It mirrors the
// array-value shapes buildArrayPrintValueExpr accepts, with the array-type
// typedef wrapping a plain raw-C-array local: a reference to an array-typed
// local that was itself declared as the wrapped typedef (a call-initialized
// local, emitted as its own pebble_local_<symbol> name directly), a reference
// to a plain literal-initialized array local (a raw C array that must be
// wrapped element-by-element into `(pebble_array_<typeID>_t){ .data = {
// <elem>, ... } }`), a by-value read of an array-typed place, a freshly
// constructed array literal (an ArrayValue, wrapped the same way), or a call
// to an array-returning helper (whose C result type IS the array typedef, so
// the call expression is the whole value). Any other shape is a clean
// rejection, never a guessed lowering.
func buildOptionalArrayPayload(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("%s references invalid array payload node %d", context, id)
	}
	key, ok := snapshot.Key(node.Type)
	if !ok {
		return "", fmt.Errorf("%s array payload of type %d is not in the type snapshot", context, node.Type)
	}
	length, element, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("%s payload is a %s, want an array-typed value", context, describeType(snapshot, node.Type))
	}
	if _, err := arrayLengthLiteral(length, width); err != nil {
		return "", err
	}
	switch node.Kind {
	case tir.SymbolValue:
		info, declared := scope[node.Symbol]
		if !declared || info.array != node.Type {
			return "", fmt.Errorf("%s references symbol %d, which is not an array-typed local of type %s in scope", context, node.Symbol, describeType(snapshot, node.Type))
		}
		if info.arrayWrapped {
			return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		}
		values := make([]string, 0, int(length))
		for i := uint64(0); i < length; i++ {
			values = append(values, fmt.Sprintf("pebble_local_%d[%d]", node.Symbol, i))
		}
		return fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(node.Type), strings.Join(values, ", ")), nil
	case tir.Load:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s payload is a Load with %d child(ren), want exactly one place", context, len(node.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		if !isArray(snapshot, placeType) || placeType != node.Type {
			return "", fmt.Errorf("%s payload is a Load of a place of type %s, want an array-typed place of type %s", context, describeType(snapshot, placeType), describeType(snapshot, node.Type))
		}
		values := make([]string, 0, int(length))
		for i := uint64(0); i < length; i++ {
			values = append(values, fmt.Sprintf("%s[%d]", lvalue, i))
		}
		return fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(node.Type), strings.Join(values, ", ")), nil
	case tir.ArrayValue:
		if uint64(len(node.Children)) != length {
			return "", fmt.Errorf("%s payload is an array literal of type %s with %d element(s), want %d", context, describeType(snapshot, node.Type), len(node.Children), length)
		}
		elementExprs, err := buildArrayBraceElements(st, unit, snapshot, fileSet, node, scope, context, width, element)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(node.Type), strings.Join(elementExprs, ", ")), nil
	case tir.DirectCall:
		calleeDecl, err := findCallDeclaration(unit, snapshot, node)
		if err != nil {
			return "", err
		}
		if !isArray(snapshot, calleeDecl.ResultType) || calleeDecl.ResultType != node.Type {
			return "", fmt.Errorf("%s payload is a call to symbol %d whose declared result type %s is not the array type %s", context, node.Symbol, describeType(snapshot, calleeDecl.ResultType), describeType(snapshot, node.Type))
		}
		callExpr, err := buildDirectCallNested(st, unit, snapshot, fileSet, node, scope, width)
		if err != nil {
			return "", err
		}
		return callExpr, nil
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s payload SourceAlias has %d child(ren), want exactly one", context, len(node.Children))
		}
		return buildOptionalArrayPayload(st, unit, snapshot, fileSet, node.Children[0], scope, context, width)
	}
	return "", fmt.Errorf("%s array payload is a %s, want a reference to an array-typed local, a by-value read of an array-typed place, an array literal, or a call to an array-returning helper", context, node.Kind)
}

// buildOptionalSlicePayload builds the two pieces of C text an optional's
// slice-typed payload construction needs — a leading temp-declaration
// statement (empty for every pure shape) and the slice VALUE expression
// assigned to the optional struct's .value field, whose C type is the slice's
// own pebble_slice_<typeID>_t typedef (see optionalPayloadCType). It accepts
// the same slice-value shapes a slice-typed call argument accepts: a
// reference to a slice-typed local, a by-value read of a slice-typed place, a
// fresh checked slice (`some a[:]`, whose checked-start temp is returned as
// the pre so a statement position can thread it, or folded into a GNU
// statement-expression when nested), a call to a slice-returning helper, or a
// raw-pointer-backed slice (a SliceFromRaw, a pure construction). nested
// selects the delivery of a CheckedSlice's temp exactly as buildSliceArgument
// does: a leading-statement position (nested == false) gets the temp
// declaration returned as the pre, while a pure expression position
// (nested == true) folds it into a GNU statement-expression primary expression
// so the construction can live inline with an empty pre.
func buildOptionalSlicePayload(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind, nested bool) (string, string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", "", fmt.Errorf("%s references invalid slice payload node %d", context, id)
	}
	if !isSlice(snapshot, node.Type) {
		return "", "", fmt.Errorf("%s payload is a %s, want a slice-typed value", context, describeType(snapshot, node.Type))
	}
	switch node.Kind {
	case tir.SymbolValue:
		info, declared := scope[node.Symbol]
		if !declared || info.sliceType != node.Type {
			return "", "", fmt.Errorf("%s references symbol %d, which is not a slice-typed local of type %s in scope", context, node.Symbol, describeType(snapshot, node.Type))
		}
		return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.Load:
		if len(node.Children) != 1 {
			return "", "", fmt.Errorf("%s payload is a Load with %d child(ren), want exactly one place", context, len(node.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, node.Children[0], scope, width)
		if err != nil {
			return "", "", err
		}
		if !isSlice(snapshot, placeType) || placeType != node.Type {
			return "", "", fmt.Errorf("%s payload is a Load of a place of type %s, want a slice-typed place of type %s", context, describeType(snapshot, placeType), describeType(snapshot, node.Type))
		}
		return "", lvalue, nil
	case tir.CheckedSlice:
		tempDecl, constructionExpr, err := buildSliceConstruction(st, unit, snapshot, fileSet, node, scope, "", context, width, fmt.Sprintf("pebble_slice_payload_%d", id), fmt.Sprintf("pebble_payload_backing_%d", id))
		if err != nil {
			return "", "", err
		}
		if nested {
			return "", sliceConstructionStatementExpr(tempDecl, constructionExpr), nil
		}
		return tempDecl, constructionExpr, nil
	case tir.DirectCall, tir.MethodCall:
		calleeDecl, err := findCallDeclaration(unit, snapshot, node)
		if err != nil {
			return "", "", err
		}
		if !isSlice(snapshot, calleeDecl.ResultType) || calleeDecl.ResultType != node.Type {
			return "", "", fmt.Errorf("%s payload is a call to symbol %d whose declared result type %s is not the slice type %s", context, node.Symbol, describeType(snapshot, calleeDecl.ResultType), describeType(snapshot, node.Type))
		}
		expr, err := buildDirectCallNested(st, unit, snapshot, fileSet, node, scope, width)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case tir.SliceFromRaw:
		expr, err := buildRawSliceConstruction(st, unit, snapshot, fileSet, node, scope, width, context)
		if err != nil {
			return "", "", err
		}
		return "", expr, nil
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return "", "", fmt.Errorf("%s payload SourceAlias has %d child(ren), want exactly one", context, len(node.Children))
		}
		return buildOptionalSlicePayload(st, unit, snapshot, fileSet, node.Children[0], scope, context, width, nested)
	}
	return "", "", fmt.Errorf("%s slice payload is a %s, want a reference to a slice-typed local, a by-value read of a slice-typed place, a fresh slice construction, a call to a slice-returning helper, or a raw slice construction", context, node.Kind)
}

// buildStructBraceList validates one RecordConstruct node's field list and
// builds its brace-list content, `{ .pebble_field_<m0> = <e0>, ... }`, a C99
// designated-initializer brace list with one designated initializer per
// constructed field. Each field's value is built by the grammar its own type
// selects — buildExpr for a field of the entry's width, buildBoolExpr for a
// bool field, buildStrOperand for a str field. The designated form places each
// value under exactly the C field its member symbol names, so the
// construction-site field order a RecordConstruct's Fields carry (which need
// not match the struct's declared order — a site may write Point.{ y = 2, x =
// 1 }) needs no reordering.
// Every field type must be exactly the entry's width, bool, or str; anything
// else (a char field, a nested struct field) is a clean rejection naming the
// field position, since this backend emits exactly those three C field types.
// context
// names the enclosing construct in error messages. The function is shared by
// the two places a RecordConstruct's fields are built (10.25): a struct-typed
// local's declaration initializer (buildStructLocalDeclaration embeds the
// returned brace list in the declaration statement) and a freshly-constructed
// struct built inline as a call argument (buildStructValueExpr wraps the same
// brace list in a compound-literal cast), so field-type validation and the
// buildExpr/buildBoolExpr dispatch live in exactly one place.
//
// The return is two pieces of C text rather than one so a slice-typed field
// whose construction value is an inline slice construction (`Bag.{ items =
// arr[:] }`, a bare CheckedSlice, or `slice ptr, n`, a SliceFromRaw) can be
// supported. A CheckedSlice construction needs the same two-statement
// temp-then-construction shape a slice local's declaration and a slice return
// use: the checked-start result is stored in a temp statement first, then the
// compound-literal construction uses that temp for both its pointer offset and
// its length subtraction, so the potentially-aborting checked call (and any
// side-effecting bound expression) is evaluated exactly once. That temp
// declaration has no place inside a brace list, so it is returned separately
// as preStatements (already indented) for a caller in a statement position
// (buildStructLocalDeclaration) to thread ahead of the declaration line; a
// caller in a pure expression position (buildStructValueExpr) must reject a
// non-empty preStatements rather than drop it — the same reason an inline
// slice construction passed as a call argument used to be rejected, before the
// GNU statement-expression lowering in buildSliceArgument /
// sliceConstructionStatementExpr folded THAT shape inline (a brace-list field
// value has no single expression to wrap the same way). A SliceFromRaw field
// value is a single expression with no temp
// (buildRawSliceConstruction emits the compound literal directly), so it needs
// no pre-statement. A slice-typed field whose construction value is a
// SymbolValue naming an already-declared slice-typed local is the same
// single-expression forward as before, with empty preStatements. indent indents
// the temp declarations to match the enclosing declaration line.
func buildStructBraceList(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, string, error) {
	key, ok := snapshot.Key(node.Type)
	if !ok {
		return "", "", fmt.Errorf("%s contains a struct value whose type %d is not in the type snapshot", context, node.Type)
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return "", "", fmt.Errorf("%s contains a struct value of type %s, which has no nominal declaration", context, structTypeName(node.Type))
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return "", "", fmt.Errorf("%s contains a struct value of type %s whose declaration symbol %d has no TypeDeclaration in the unit", context, structTypeName(node.Type), decl)
	}
	members := typeDecl.Members
	if len(node.Fields) != len(members) {
		return "", "", fmt.Errorf("%s contains a struct value of type %s with %d field initializer(s), want %d (one per declared field)", context, structTypeName(node.Type), len(node.Fields), len(members))
	}
	inits := make([]string, len(node.Fields))
	var pres []string
	for i, field := range node.Fields {
		declared := false
		for _, member := range members {
			if member == field.Field {
				declared = true
				break
			}
		}
		if !declared {
			return "", "", fmt.Errorf("%s contains a struct value of type %s with an initializer for symbol %d, which is not one of its declared fields", context, structTypeName(node.Type), field.Field)
		}
		valueNode, ok := unit.Node(field.Value)
		if !ok {
			return "", "", fmt.Errorf("%s contains a struct value of type %s referencing invalid field value node %d", context, structTypeName(node.Type), field.Value)
		}
		fieldType, found := declaredFieldType(unit, snapshot, node.Type, field.Field)
		if !found {
			fieldType = valueNode.Type
		}
		var expr string
		fieldWidth, integerField := resolvedBuiltin(snapshot, fieldType)
		switch {
		case integerField && cType(fieldWidth) != "" && !isUint(snapshot, fieldType):
			// Any fixed-width integer field other than uint (uint flows
			// through its own dedicated grammar below) is built at its OWN
			// resolved width, mirroring buildCallArgument/buildComparisonOperand
			// and the optional-payload widening (d737242) — so an i64 field
			// inside an i32 function builds its value at i64, not i32.
			built, err := buildExpr(st, unit, snapshot, fileSet, field.Value, scope, fieldWidth, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isUint(snapshot, fieldType):
			built, err := buildUintExpr(st, unit, snapshot, fileSet, field.Value, scope, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isBool(snapshot, fieldType):
			built, err := buildBoolExpr(st, unit, snapshot, fileSet, field.Value, scope, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isFloat(snapshot, fieldType):
			// A float-typed field's construction value is built by
			// buildFloatExpr at the field's OWN float kind (resolvedFloatKind —
			// f32 or f64) and the entry width, exactly as a float call argument,
			// a float local's declaration initializer, and a float comparison
			// operand are built (task #22, slice 86a): a float literal, a
			// reference to an in-scope float-typed local, or a call to a
			// float-returning helper. The C field's type is the plain C
			// float/double (see structFieldCType), so the built expression
			// matches the field type with no cast.
			built, err := buildFloatExpr(st, unit, snapshot, fileSet, field.Value, scope, resolvedFloatKind(snapshot, fieldType), width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isStr(snapshot, fieldType):
			// A str-typed field's construction value (Entry's `key = k`,
			// std/hmap.peb's insert) is one of the same three shapes
			// buildStrOperand accepts anywhere a str value is built — a string
			// literal, a reference to an in-scope str-typed local, or a call to
			// a str-returning helper — built by the same buildStrOperand a str
			// local's declaration initializer, a str call argument, and a str
			// comparison operand use. The C field's type is PebbleStr (see
			// structFieldCType), so the built expression matches the field type
			// with no cast.
			built, err := buildStrOperand(st, unit, snapshot, fileSet, field.Value, scope, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isTuple(snapshot, fieldType):
			built, err := buildNestedAggregateValue(st, unit, snapshot, fileSet, field.Value, scope, fieldType, context, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isOptional(snapshot, fieldType):
			built, err := buildNestedAggregateValue(st, unit, snapshot, fileSet, field.Value, scope, fieldType, context, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case runtimeType(unit, snapshot, fieldType) != 0:
			// A field of a compiler-builtin runtime type (Allocator, Context) is
			// initialized from the same runtime-value grammar a runtime-typed
			// local's declaration uses (buildRuntimeValue: `context.default_allocator`,
			// a runtime-typed local, or a load of a runtime-typed field) — NOT the
			// nested-aggregate grammar the isStruct case below would send it to
			// (Allocator is Nominal like a struct, but its value is never a
			// RecordConstruct). The C field's type is PebbleAllocator /
			// PebbleContext (see structFieldCType), so the built expression matches
			// the field type with no cast.
			built, err := buildRuntimeValueNode(st, unit, snapshot, fileSet, field.Value, scope, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isTaggedUnionType(unit, snapshot, fieldType):
			// A tagged-union-typed field's construction value is a union value (a
			// variant construction like Choice.value(5) / Choice.empty, a reference
			// to an already-declared union-typed local, a union-typed field read, or
			// a union-payload force-unwrap), built by the same buildUnionValueExpr an
			// optional's tagged-union payload and a union-typed call argument use,
			// NOT the enum grammar the isEnumType case below would send it to (a
			// tagged union is enum-shaped too — see isEnumType — but its value is a
			// union value, never a plain enum constant). This precedes the isEnumType
			// case exactly as buildOptionalValueExpr's own isTaggedUnionType case
			// precedes its isEnumType case: buildUnionValueExpr rejects nothing for a
			// payload-carrying variant, unlike buildEnumValue.
			built, err := buildUnionValueExpr(st, unit, snapshot, fileSet, field.Value, scope, context, fieldType, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isEnumType(unit, snapshot, fieldType):
			// An enum-typed field's construction value (Entry's `state = .Empty`,
			// std/hmap.peb's insert) is a variant literal — an EnumVariantValue or
			// a zero-payload VariantConstruct — built by the same buildEnumValue an
			// enum-typed local's declaration uses, NOT the nested-aggregate grammar
			// the isStruct case below would send it to (a plain enum is Nominal
			// exactly like a struct — see isEnumType — but its value is never a
			// RecordConstruct, so buildNestedAggregateValue's SymbolValue-or-nested-
			// construction dispatch would mishandle it). The variant literal lowers
			// to the variant's C enum constant, whose type matches the field's own
			// pebble_enum_<typeID>_t C type with no cast needed.
			built, err := buildEnumValue(st, unit, snapshot, fileSet, field.Value, scope, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isStruct(snapshot, fieldType):
			built, err := buildNestedAggregateValue(st, unit, snapshot, fileSet, field.Value, scope, fieldType, context, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isSlice(snapshot, fieldType):
			// A slice-typed field's construction value is one of three shapes
			// (all confirmed against real fixtures): a SymbolValue naming an
			// already-declared slice-typed local in scope of exactly the
			// field's type (a single-expression forward, the shape that always
			// worked); a bare CheckedSlice (`arr[:]` used directly as the
			// field's value — the general struct-field-construction gap this
			// case closes, reachable in both non-generic and generic struct
			// constructions), which needs the same two-statement
			// temp-then-construction shape a slice local's declaration uses,
			// so its temp declaration is returned as a pre-statement; or a
			// bare SliceFromRaw (`slice ptr, n` — restricted to std-package
			// source, where the raw-pointer slice builtin is available),
			// whose construction is a single expression (buildRawSliceConstruction
			// needs no temp) and needs no pre-statement. Anything else is a
			// clean rejection naming what was found.
			fieldValue, ok := unit.Node(field.Value)
			if !ok {
				return "", "", fmt.Errorf("%s contains a struct value of type %s referencing invalid field value node %d", context, structTypeName(node.Type), field.Value)
			}
			switch fieldValue.Kind {
			case tir.SymbolValue:
				local, declared := scope[fieldValue.Symbol]
				if !declared || local.sliceType != fieldType {
					return "", "", fmt.Errorf("%s contains a slice field %d initialized from a nonmatching local", context, field.Field)
				}
				expr = fmt.Sprintf("pebble_local_%d", fieldValue.Symbol)
			case tir.CheckedSlice:
				if fieldValue.Type != fieldType {
					return "", "", fmt.Errorf("%s contains a slice field %d initialized from a CheckedSlice of type %s, not a slice-typed value of type %s", context, field.Field, describeType(snapshot, fieldValue.Type), sliceTypeName(fieldType))
				}
				// The temp name derives from the field value node's own NodeID
				// — the only stable identity in hand here (a struct field has
				// no local symbol to name it from), distinct from the
				// pebble_slice_start_<symbol> temps a slice local's declaration
				// uses and the pebble_slice_ret_<nodeID> temps a slice return
				// uses, so the three can never collide even when a symbol ID
				// numerically equals a node ID.
				tempDecl, constructionExpr, err := buildSliceConstruction(st, unit, snapshot, fileSet, fieldValue, scope, indent, context, width, fmt.Sprintf("pebble_field_slice_%d", field.Value), fmt.Sprintf("pebble_field_backing_%d", field.Value))
				if err != nil {
					return "", "", err
				}
				pres = append(pres, tempDecl)
				expr = constructionExpr
			case tir.SliceFromRaw:
				if fieldValue.Type != fieldType {
					return "", "", fmt.Errorf("%s contains a slice field %d initialized from a SliceFromRaw of type %s, not a slice-typed value of type %s", context, field.Field, describeType(snapshot, fieldValue.Type), sliceTypeName(fieldType))
				}
				construction, err := buildRawSliceConstruction(st, unit, snapshot, fileSet, fieldValue, scope, width, context)
				if err != nil {
					return "", "", err
				}
				expr = construction
			default:
				return "", "", fmt.Errorf("%s contains a slice field %d initialized from a %s, want a slice local or a fresh slice construction (a CheckedSlice or a slice-from-raw)", context, field.Field, fieldValue.Kind)
			}
		case isArray(snapshot, fieldType):
			// A fixed-array-typed field's construction value (`Holder.{ values
			// = [1, 2, 3] }`): built by buildStructArrayFieldValue, mirroring
			// the array call-argument shapes (buildArrayArgument) — an array
			// literal as the array typedef's C99 compound literal, a reference
			// to an in-scope array-typed local (directly when the local is a
			// pebble_array_<typeID>_t wrapped value, element-by-element for a
			// raw C array local), or a call to an array-returning helper. The
			// C field is declared at the array's OWN typedef (see
			// structFieldCType), so the built expression matches the field type
			// with no cast.
			fieldPre, built, err := buildStructArrayFieldValue(st, unit, snapshot, fileSet, field.Value, scope, indent, fieldType, context, width)
			if err != nil {
				return "", "", err
			}
			if fieldPre != "" {
				pres = append(pres, fieldPre)
			}
			expr = built
		case isPointer(snapshot, fieldType):
			built, err := buildExpr(st, unit, snapshot, fileSet, field.Value, scope, width, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		case isFunctionType(snapshot, fieldType):
			// A function-typed field's construction value (`Table.{ op = add }`,
			// function-types slice 2): built by the same buildFunctionValue a
			// function-typed local's declaration and the general indirect call's
			// callee already use (slice 1) — a bare top-level function reference
			// (HoistedFunctionValue) or a reference to an in-scope function-typed
			// local (SymbolValue), whose C value already matches the field's own
			// pebble_fnptr_<typeID>_t C type with no cast needed.
			built, err := buildFunctionValue(st, unit, snapshot, fileSet, valueNode, scope, context, width)
			if err != nil {
				return "", "", err
			}
			expr = built
		default:
			return "", "", fmt.Errorf("%s contains a struct value of type %s whose field %d is %s, want a fixed-width integer, bool, str, tuple, struct, enum, pointer, slice, or function type", context, structTypeName(node.Type), field.Field, describeType(snapshot, fieldType))
		}
		inits[i] = fmt.Sprintf(".pebble_field_%d = %s", field.Field, expr)
	}
	preStatements := ""
	if len(pres) > 0 {
		preStatements = strings.Join(pres, "\n")
	}
	return preStatements, "{ " + strings.Join(inits, ", ") + " }", nil
}

// buildStructArrayFieldValue builds the C expression a fixed-array-typed struct
// field's construction value lowers to, of the shapes real source produces for
// an array-typed value (mirroring buildArrayArgument, the array call-argument
// builder): an ArrayValue literal, emitted as the array typedef's C99 compound
// literal `(pebble_array_<typeID>_t){ .data = { <elements> } }`; a reference to
// an in-scope array-typed local of exactly the field's type — a
// pebble_array_<typeID>_t WRAPPED local (the call-initialized,
// array-parameter, or global shape) emitted directly as its pebble_local_<id>
// C name, or a raw `int32_t arr[N]`-declared local wrapped element-by-element
// into the compound literal, exactly as buildArrayArgument does; or a call to
// an array-returning helper (a DirectCall, whose result C type IS the array
// typedef). The field's C type is the array's own typedef (see
// structFieldCType), so the built expression matches with no cast. Anything
// else is a clean rejection naming what was found. fieldType is the field's own
// array type, whose length and element type come from the type snapshot.
func buildStructArrayFieldValue(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, valueID tir.NodeID, scope map[symbol.SymbolID]localInfo, indent string, fieldType types.TypeID, context string, width types.BuiltinKind) (string, string, error) {
	key, ok := snapshot.Key(fieldType)
	if !ok {
		return "", "", fmt.Errorf("%s array field type %s is not in the type snapshot", context, describeType(snapshot, fieldType))
	}
	length, element, ok := key.Array()
	if !ok {
		return "", "", fmt.Errorf("%s array field type %s has no length and element type", context, describeType(snapshot, fieldType))
	}
	if _, err := arrayLengthLiteral(length, width); err != nil {
		return "", "", fmt.Errorf("%s: %v", context, err)
	}
	node, ok := unit.Node(valueID)
	if !ok {
		return "", "", fmt.Errorf("%s array field references invalid value node %d", context, valueID)
	}
	switch node.Kind {
	case tir.ArrayValue:
		if node.Type != fieldType {
			return "", "", fmt.Errorf("%s array field initialized from an ArrayValue of type %s, not an array-typed value of type %s", context, describeType(snapshot, node.Type), arrayTypeName(fieldType))
		}
		if uint64(len(node.Children)) != length {
			return "", "", fmt.Errorf("%s array field has %d element expression(s), want %d", context, len(node.Children), length)
		}
		elementExprs, err := buildArrayBraceElements(st, unit, snapshot, fileSet, node, scope, context, width, element)
		if err != nil {
			return "", "", err
		}
		return "", fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(fieldType), strings.Join(elementExprs, ", ")), nil
	case tir.ArrayRepeat:
		// A direct [v; N] repeat as the field's construction value —
		// `Box.{ data = [7; 3] }`, the ArrayRepeat twin of the ArrayValue case
		// above (which Phase 3 #4's 249995d fixed). The count child is
		// validated exactly as buildArrayRepeatLocalDeclaration validates it (a
		// compile-time uint integer literal equal to the field's declared
		// length), and the single value expression is built once and assigned
		// to an indented C temp whose name is then repeated `length` times in
		// the field's array compound literal — the same evaluate-once,
		// copy-N-times lowering buildArrayReturnValue and buildArrayArgument use
		// for a direct ArrayRepeat. The temp declaration is returned as a
		// pre-statement for the caller (buildStructBraceList) to emit before
		// the enclosing struct compound literal (which, like a slice-field
		// pre, folds the whole struct literal into a GNU statement-expression
		// at the value level). The temp name derives from the field value
		// node's own NodeID — a struct field has no local symbol to name it
		// from — distinct from pebble_repeat_<symbol> (local declarations),
		// pebble_repeat_ret_<nodeID> (returns), and pebble_repeat_arg_<nodeID>
		// (call arguments), so the four can never collide even when a symbol
		// ID numerically equals a node ID.
		if node.Type != fieldType {
			return "", "", fmt.Errorf("%s array field initialized from an ArrayRepeat of type %s, not an array-typed value of type %s", context, describeType(snapshot, node.Type), arrayTypeName(fieldType))
		}
		if len(node.Children) != 2 {
			return "", "", fmt.Errorf("%s array field is an ArrayRepeat with %d child(ren), want exactly two (the repeated value and the count)", context, len(node.Children))
		}
		countNode, ok := unit.Node(node.Children[1])
		if !ok {
			return "", "", fmt.Errorf("%s array field is an ArrayRepeat referencing invalid count node %d", context, node.Children[1])
		}
		if countNode.Kind != tir.IntegerLiteral {
			return "", "", fmt.Errorf("%s array field is an ArrayRepeat whose count is a %s, want a compile-time integer literal equal to the array's declared length %d", context, countNode.Kind, length)
		}
		if countNode.Type != snapshot.Builtins().Uint {
			return "", "", fmt.Errorf("%s array field is an ArrayRepeat whose count has type %s, want uint (the count is a synthesized integer literal)", context, describeType(snapshot, countNode.Type))
		}
		count, err := strconv.ParseUint(countNode.Literal.IntegerNum, 10, 64)
		if err != nil {
			return "", "", fmt.Errorf("%s array field is an ArrayRepeat whose count %q is not a valid non-negative integer", context, countNode.Literal.IntegerNum)
		}
		if count != length {
			return "", "", fmt.Errorf("%s array field is an ArrayRepeat whose count %d does not equal the array's declared length %d", context, count, length)
		}
		var valueExpr string
		if isBool(snapshot, element) {
			valueExpr, err = buildBoolExpr(st, unit, snapshot, fileSet, node.Children[0], scope, width)
		} else if elementWidth, integer := resolvedBuiltin(snapshot, element); integer && cType(elementWidth) != "" {
			valueExpr, err = buildExpr(st, unit, snapshot, fileSet, node.Children[0], scope, elementWidth, width)
		} else if isFloat(snapshot, element) {
			valueExpr, err = buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], scope, resolvedFloatKind(snapshot, element), width)
		} else if isChar(snapshot, element) {
			valueExpr, err = buildCharOperand(st, unit, snapshot, fileSet, node.Children[0], scope, width)
		} else if isStruct(snapshot, element) || isTuple(snapshot, element) || isOptional(snapshot, element) {
			valueExpr, err = buildNestedAggregateValue(st, unit, snapshot, fileSet, node.Children[0], scope, element, context, width)
		} else {
			return "", "", fmt.Errorf("%s array field element type %s is unsupported", context, describeType(snapshot, element))
		}
		if err != nil {
			return "", "", err
		}
		ctype, err := arrayElementCType(unit, snapshot, width, element)
		if err != nil {
			return "", "", fmt.Errorf("%s: %v", context, err)
		}
		tempName := fmt.Sprintf("pebble_field_repeat_%d", valueID)
		pre := fmt.Sprintf("%s%s %s = %s;", indent, ctype, tempName, valueExpr)
		values := make([]string, length)
		for i := range values {
			values[i] = tempName
		}
		return pre, fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(fieldType), strings.Join(values, ", ")), nil
	case tir.SymbolValue:
		info, declared := scope[node.Symbol]
		if !declared || info.array != fieldType {
			return "", "", fmt.Errorf("%s array field initialized from symbol %d, which is not an array-typed local of type %s in scope", context, node.Symbol, arrayTypeName(fieldType))
		}
		if info.arrayWrapped {
			return "", fmt.Sprintf("pebble_local_%d", node.Symbol), nil
		}
		values := make([]string, 0, int(length))
		for i := uint64(0); i < length; i++ {
			values = append(values, fmt.Sprintf("pebble_local_%d[%d]", node.Symbol, i))
		}
		return "", fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(fieldType), strings.Join(values, ", ")), nil
	case tir.DirectCall:
		if node.Type != fieldType {
			return "", "", fmt.Errorf("%s array field initialized from a call of type %s, want an array-typed value of type %s", context, describeType(snapshot, node.Type), arrayTypeName(fieldType))
		}
		expr, err := buildDirectCall(st, unit, snapshot, fileSet, node, scope, width)
		return "", expr, err
	case tir.Load:
		// A whole array read through a pointer deref used directly as the
		// field's construction value — `Box.{ data = *p }`. The field's C type
		// is the array's own pebble_array_<typeID>_t typedef, and the
		// dereference yields exactly that wrapped struct, so the whole
		// dereference is the construction value directly — the same
		// single-expression lowering buildArrayReturnValue uses for `return
		// *p;`, with the null check performed exactly once (Phase 3 #24).
		expr, err := buildWholeArrayDerefRead(st, unit, snapshot, fileSet, node, fieldType, scope, context, width)
		return "", expr, err
	}
	return "", "", fmt.Errorf("%s array field initialized from a %s, want an array literal, an ArrayRepeat, an array-typed local in scope, a call to an array-returning helper, or a whole-array dereference read", context, node.Kind)
}

// buildUnionConstruction builds the C expression text for one tagged-union
// variant construction, of three shapes (all confirmed against real fixtures):
// a payload-carrying VariantConstruct (Choice.value(5), the variant's payload
// expression as its one child), a payload-less EnumVariantValue (Choice.empty,
// the member-access form), and a zero-payload VariantConstruct (Choice.empty(),
// the parenthesized-call form). All three lower to a C99 compound literal of
// the union's own struct typedef:
//
//	(pebble_union_<typeID>_t){ .tag = pebble_variant_<member> }
//	(pebble_union_<typeID>_t){ .tag = pebble_variant_<member>, .payload = { .pebble_field_<member> = <payload expr> } }
//
// The tag is the variant's C enum constant (the same pebble_variant_<member>
// name a plain enum uses — the discriminant ordinal scheme is identical), so a
// payload-less construction leaves the payload union unspecified, which is
// legal C: the tag alone determines which member, if any, is meaningful. A
// payload-carrying construction's payload expression is built by the grammar
// its own type selects — buildExpr for a payload of the entry's width,
// buildBoolExpr for a bool payload, buildStrOperand for a str payload — and
// the payload union member is named
// pebble_field_<member> exactly as the union's typedef declares it. The node's
// Type is the union type and its Member the variant symbol (both confirmed
// against real fixtures); the member must be one of the union's declared
// variants (info.variants), and a payload-carrying construction must name a
// variant whose payload member the union's typedef declares (info.members).
// Any other node kind is a clean rejection, never a guessed lowering.
func buildUnionConstruction(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, node tir.Node, scope map[symbol.SymbolID]localInfo, context string, info unionInfo, width types.BuiltinKind) (string, error) {
	if !containsVariant(info.variants, node.Member) {
		return "", fmt.Errorf("%s constructs variant symbol %d, which is not one of the union %s's declared variants", context, node.Member, unionTypeName(node.Type))
	}
	tag := enumVariantName(node.Member)
	switch node.Kind {
	case tir.EnumVariantValue:
		if len(node.Children) != 0 {
			return "", fmt.Errorf("%s constructs union variant symbol %d with %d payload(s), want zero (a payload-less member access)", context, node.Member, len(node.Children))
		}
		return fmt.Sprintf("(%s){ .tag = %s }", unionTypeName(node.Type), tag), nil
	case tir.VariantConstruct:
		if len(node.Children) == 0 {
			return fmt.Sprintf("(%s){ .tag = %s }", unionTypeName(node.Type), tag), nil
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("%s constructs union variant symbol %d with %d payload(s), want exactly one (a tagged-union variant carries exactly one payload)", context, node.Member, len(node.Children))
		}
		payloadNode, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("%s constructs union variant symbol %d referencing invalid payload node %d", context, node.Member, node.Children[0])
		}
		memberType, hasMember := unionMemberType(info.members, node.Member)
		if !hasMember {
			return "", fmt.Errorf("%s constructs union variant symbol %d, whose payload type is not resolved (no construction of it is collected as a union member)", context, node.Member)
		}
		if payloadNode.Type != memberType {
			return "", fmt.Errorf("%s constructs union variant symbol %d with a payload of type %s, want %s (the variant's resolved payload type)", context, node.Member, describeType(snapshot, payloadNode.Type), describeType(snapshot, memberType))
		}
		var payloadExpr string
		var err error
		payloadWidth, integerPayload := resolvedBuiltin(snapshot, payloadNode.Type)
		switch {
		case integerPayload && cType(payloadWidth) != "":
			// Any fixed-width integer payload (the entry's own resolved width
			// or any other) is built by buildExpr at the payload's OWN width —
			// mirroring how buildOptionalValueExpr / structFieldCType resolve
			// each integer to its own C type, so the emitted expression's type
			// matches the union member's declared C type (unionMemberCType).
			payloadExpr, err = buildExpr(st, unit, snapshot, fileSet, node.Children[0], scope, payloadWidth, width)
		case isBool(snapshot, payloadNode.Type):
			payloadExpr, err = buildBoolExpr(st, unit, snapshot, fileSet, node.Children[0], scope, width)
		case isChar(snapshot, payloadNode.Type):
			// A char-typed payload: built by the char grammar (a char literal,
			// a reference to a char-typed local, or a call to a char-returning
			// helper), emitted as int32_t — the same C type the union's payload
			// member is declared with (see unionMemberCType).
			payloadExpr, err = buildCharOperand(st, unit, snapshot, fileSet, node.Children[0], scope, width)
		case isFloat(snapshot, payloadNode.Type):
			// A float-typed payload (f32/f64): built by buildFloatExpr at the
			// payload's own float kind, emitted as the plain C float/double the
			// union member declares (see unionMemberCType).
			payloadExpr, err = buildFloatExpr(st, unit, snapshot, fileSet, node.Children[0], scope, resolvedFloatKind(snapshot, payloadNode.Type), width)
		case isStr(snapshot, payloadNode.Type):
			// A str-typed payload (`Result[int, str].{ Err = "bad" }`): built
			// by the str grammar (buildStrOperand — a string literal, a
			// reference to a str-typed local, or a call to a str-returning
			// helper), emitted as a PebbleStr value, the same C type the
			// union's payload member is declared with (see unionMemberCType),
			// so the designated initializer needs no cast.
			payloadExpr, err = buildStrOperand(st, unit, snapshot, fileSet, node.Children[0], scope, width)
		case isUnionEnumType(unit, snapshot, payloadNode.Type):
			// A nested tagged-union payload (`Choice.value(Inner.b(7))`, a
			// union inside a union): built by the union grammar
			// (buildUnionValueExpr — a reference to a union-typed local, a
			// variant construction, or a union-typed field read), emitted as
			// the inner union's own pebble_union_<typeID>_t value, the same C
			// type the union's payload member is declared with (see
			// unionMemberCType). This precedes the plain-enum case below
			// exactly as buildOptionalValueExpr's union branch precedes its
			// enum branch: a tagged union is enum-shaped too.
			payloadExpr, err = buildUnionValueExpr(st, unit, snapshot, fileSet, node.Children[0], scope, context, payloadNode.Type, width)
		case isDefinitelyEnumType(unit, snapshot, payloadNode.Type):
			// A plain-enum payload (`Choice.value(Color.green)`): built by the
			// enum grammar (buildEnumValue — a variant literal, a reference to
			// an enum-typed local, or an enum-returning call), emitted as the
			// enum's own pebble_enum_<typeID>_t value, the same C type the
			// union's payload member is declared with (see unionMemberCType).
			payloadExpr, err = buildEnumValue(st, unit, snapshot, fileSet, node.Children[0], scope, width)
		default:
			return "", fmt.Errorf("%s constructs union variant symbol %d with an unsupported payload type %s", context, node.Member, describeType(snapshot, payloadNode.Type))
		}
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s){ .tag = %s, .payload = { .pebble_field_%d = %s } }", unionTypeName(node.Type), tag, node.Member, payloadExpr), nil
	default:
		return "", fmt.Errorf("%s constructs a %s, want a union variant construction (a VariantConstruct) or a member access (an EnumVariantValue)", context, node.Kind)
	}
}

// resolveUnionInfoForValue resolves one tagged-union type's unionInfo from the
// unit's own construction nodes, on demand, for the union-value expression
// positions that sit outside the builders holding Emit's pre-collected union
// map (an optional's `some <union>` payload, a union-typed call argument): the
// payload-carrying VariantConstructs of that type anywhere in the unit supply
// each constructed member's payload type, exactly the evidence
// collectUnionTypes accumulates before calling resolveUnionInfo, so a fresh
// resolution agrees with the emitted union typedef by construction. The
// checker has already validated every construction, so the payload-type gate
// collectUnionTypesWalk enforces at collection time needs no repetition here.
func resolveUnionInfoForValue(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) (unionInfo, error) {
	payloads := make(map[symbol.SymbolID]types.TypeID)
	for _, node := range unit.Nodes() {
		if node.Kind != tir.VariantConstruct || node.Type != id || len(node.Children) == 0 {
			continue
		}
		if len(node.Children) != 1 {
			return unionInfo{}, fmt.Errorf("union variant symbol %d is constructed with %d payload(s); a tagged-union variant carries exactly one payload", node.Member, len(node.Children))
		}
		payloadNode, ok := unit.Node(node.Children[0])
		if !ok {
			return unionInfo{}, fmt.Errorf("union variant symbol %d references invalid payload node %d", node.Member, node.Children[0])
		}
		payloads[node.Member] = payloadNode.Type
	}
	return resolveUnionInfo(unit, snapshot, id, payloads)
}

// buildUnionUnwrapPanicElse is the absent-optional branch of a union-payload
// force-unwrap's inline conditional: a comma expression whose first operand
// panics with PEBBLE_PANIC_UNWRAP_FAILED via the runtime's noreturn
// pebble_rt_panic (the same panic the scalar pebble_rt_checked_unwrap_*
// helpers raise) and whose second operand is a zero-valued compound literal of
// the union's own typedef, so the comma expression is itself a well-typed
// pebble_union_<typeID>_t value the conditional's other branch can share. The
// panic's file/line/column come from the unwrap node's own Span, resolved
// exactly like buildSourceLoc resolves a checked call's location.
func buildUnionUnwrapPanicElse(fileSet *source.FileSet, span source.Span, unionType types.TypeID) string {
	file := "NULL"
	line, column := 0, 0
	if fileSet != nil {
		if f, ok := fileSet.File(span.Source); ok {
			pos := f.Position(span.Start)
			file = fmt.Sprintf("%q", escapeCString(f.Path()))
			line, column = pos.Line, pos.Column
		}
	}
	info := fmt.Sprintf("(PebblePanicInfo){ .kind = PEBBLE_PANIC_UNWRAP_FAILED, .message = NULL, .file = %s, .line = %d, .column = %d }", file, line, column)
	return fmt.Sprintf("(pebble_rt_panic(&%s), (%s){0})", info, unionTypeName(unionType))
}

// buildTupleElement builds the C text for reading one element of a tuple local
// by symbol and ordinal: pebble_local_<symbol>._<ordinal>. The symbol must be a
// local the scope records as tuple-typed (its localInfo.tuple), the ordinal
// must be in range for that tuple type's element list, and the element's own
// type must satisfy the grammar wantBool selects — bool for the buildBoolExpr
// path, the entry's width for the buildExpr path. The tuple type comes from the
// scope record, not from any node field, so a read always resolves against the
// type the local was actually declared with.
func buildTupleElement(unit *tir.Unit, snapshot *types.Snapshot, symbolID symbol.SymbolID, ordinal uint32, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	info, declared := locals[symbolID]
	if !declared || info.tuple == 0 {
		return "", fmt.Errorf("entry function body expression reads an element of symbol %d, which is not a tuple-typed local declared earlier in the entry body", symbolID)
	}
	key, ok := snapshot.Key(info.tuple)
	if !ok {
		return "", fmt.Errorf("entry function body expression reads an element of a tuple local whose type %d is not in the type snapshot", info.tuple)
	}
	elements, ok := key.Elements()
	if !ok {
		return "", fmt.Errorf("entry function body expression reads an element of tuple type %s, which has no element list", tupleTypeName(info.tuple))
	}
	if ordinal >= uint32(len(elements)) {
		return "", fmt.Errorf("entry function body expression reads tuple element %d of %s, which has only %d element(s)", ordinal, tupleTypeName(info.tuple), len(elements))
	}
	element := elements[ordinal]
	if wantBool {
		if !isBool(snapshot, element) {
			return "", fmt.Errorf("entry function body expression reads tuple element %d, whose type is %s, want bool", ordinal, describeType(snapshot, element))
		}
	} else if !isWidth(snapshot, width, element) {
		return "", fmt.Errorf("entry function body expression reads tuple element %d, whose type is %s, want %s", ordinal, describeType(snapshot, element), wantName(width))
	}
	return fmt.Sprintf("pebble_local_%d._%d", symbolID, ordinal), nil
}

func runtimeFieldName(unit *tir.Unit, owner types.TypeID, member symbol.SymbolID) (string, bool) {
	info := unit.Runtime()
	if runtimeType(unit, unit.Snapshot(), owner) == symbol.RuntimeAllocator {
		switch member {
		case info.AllocatorPtr:
			return "state", true
		case info.AllocatorAlloc:
			return "alloc", true
		case info.AllocatorRealloc:
			return "realloc", true
		case info.AllocatorFree:
			return "free", true
		}
	}
	if runtimeType(unit, unit.Snapshot(), owner) == symbol.RuntimeContext && member == info.ContextDefaultAllocator {
		return "allocator", true
	}
	return "", false
}
