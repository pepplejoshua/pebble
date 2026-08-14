package backend

import (
	"fmt"
	"sort"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// appendTypedefBlock appends a second typedef block onto a first, joining them
// with a blank line when both are non-empty. Either may be empty; the result is
// the non-empty one when only one is non-empty, and empty when both are.
func appendTypedefBlock(first, second string) string {
	if first == "" {
		return second
	}
	if second == "" {
		return first
	}
	return first + "\n" + second
}

// orderAggregateTypes performs a stable dependency-first traversal. The input
// order is deliberately the historical tuple, optional, struct collection
// order, so unrelated programs retain their prior output. unit is threaded
// through so an enum-typed dependency (a plain enum is Nominal like a struct —
// see isEnumType) is never mistaken for a struct: enum types are not
// aggregates this pass orders (they are emitted by buildEnumTypedefs) and have
// no field dependencies to recurse into, so they are skipped entirely rather
// than appended to the postorder as a zero-valued structInfo.
func orderAggregateTypes(unit *tir.Unit, snapshot *types.Snapshot, tuples, optionals []types.TypeID, structs []structInfo) (aggregateTypeOrder, error) {
	structByType := make(map[types.TypeID]structInfo, len(structs))
	for _, info := range structs {
		structByType[info.typ] = info
	}
	// depth returns the maximum dependency-nesting level below id (a leaf is 0,
	// an aggregate/array field adds 1) together with whether ANY dependency edge
	// under id routes through an array type to a struct/tuple/optional. The
	// array flag drives the selective nesting check below: a chain built only
	// from struct/tuple/optional nesting may nest arbitrarily deep, while a
	// chain that passes through an array to an aggregate keeps the depth>1
	// rejection — a struct-field array's typedef is emitted BEFORE the
	// aggregate block (see Emit's fieldArrayTypedefs), and its inline `elem
	// data[length]` member needs the COMPLETE element type, so an array whose
	// element is itself an aggregate would reference a typedef not yet defined.
	// An array-of-array chain is exempt: buildArrayTypedefs emits inner array
	// typedefs before outer ones via DFS postorder, so the outer array's inline
	// .data member can safely name the inner array's own typedef — the chain
	// is fully self-contained within the array-typedef machinery.
	var depth func(types.TypeID, map[types.TypeID]bool) (int, bool)
	depth = func(id types.TypeID, active map[types.TypeID]bool) (int, bool) {
		if active[id] {
			return 0, false
		}
		active[id] = true
		defer delete(active, id)
		key, ok := snapshot.Key(id)
		if !ok {
			return 0, false
		}
		var deps []types.TypeID
		switch key.Kind() {
		case types.Tuple:
			deps, _ = key.Elements()
		case types.Optional:
			if c, ok := key.Child(); ok {
				deps = []types.TypeID{c}
			}
		case types.Array:
			if _, c, ok := key.Array(); ok {
				deps = []types.TypeID{c}
			}
		case types.Nominal:
			if in, ok := structByType[id]; ok {
				for _, f := range in.fields {
					deps = append(deps, f.typ)
				}
			}
		}
		max := 0
		throughArray := false
		for _, d := range deps {
			// A compiler-builtin runtime type (Allocator, Context) is Nominal
			// like an ordinary struct but is NOT an aggregate this pass orders:
			// its C typedef (PebbleAllocator / PebbleContext) is hand-written in
			// pebble_rt.h, never emitted here, so it must not count as a nesting
			// level (a struct field whose type is Allocator is a leaf, not a
			// dependency — mirroring collectStructTypesWalk's runtimeType==0
			// guard). Without it a struct containing a runtime-typed field would
			// be miscounted as nested and rejected by the depth check below.
			if (isTuple(snapshot, d) || isOptional(snapshot, d) || isArray(snapshot, d) || (isStruct(snapshot, d) && runtimeType(unit, snapshot, d) == 0)) && !isEnumType(unit, snapshot, d) && !isUntaggedUnionType(unit, snapshot, d) {
				if isArray(snapshot, d) {
					// An array whose element is itself an array is fully
					// self-contained within the array-typedef machinery:
					// buildArrayTypedefs emits inner array typedefs before outer
					// ones via DFS postorder, so the outer array's inline .data
					// member can safely name the inner array's own typedef. Only
					// flag throughArray when the array's element is a struct/
					// tuple/optional — the genuine ordering hazard where the
					// array's inline .data member needs an aggregate typedef not
					// yet defined.  Plain struct elements are excluded from this
					// hazard check (F5-18): their own typedef names only scalar
					// or plain-enum C types, so the DFS-postorder ordering in
					// the same pass can correctly emit the element's typedef
					// before the aggregate that holds the array field references
					// it; an array-of-plain-struct therefore does not require
					// the array block to lead the aggregate block.
					key, ok := snapshot.Key(d)
					if ok {
						_, elem, ok := key.Array()
						if ok && (isTuple(snapshot, elem) || isOptional(snapshot, elem) || (isStruct(snapshot, elem) && runtimeType(unit, snapshot, elem) == 0 && !isPlainStructField(unit, snapshot, elem))) {
							throughArray = true
						}
					}
				}
				subDepth, subArray := depth(d, active)
				if subArray {
					throughArray = true
				}
				if v := subDepth + 1; v > max {
					max = v
				}
			}
		}
		return max, throughArray
	}
	for _, id := range append(append(append([]types.TypeID{}, tuples...), optionals...), func() []types.TypeID {
		r := make([]types.TypeID, len(structs))
		for i := range structs {
			r[i] = structs[i].typ
		}
		return r
	}()...) {
		if nesting, throughArray := depth(id, map[types.TypeID]bool{}); nesting > 1 && throughArray {
			return aggregateTypeOrder{}, fmt.Errorf("aggregate type %s has more than one level of nesting, which is unsupported", describeType(snapshot, id))
		}
	}
	result := aggregateTypeOrder{}
	// DFS postorder gives dependencies before users while preserving roots.
	seen := make(map[types.TypeID]bool)
	var post []types.TypeID
	var dfs func(types.TypeID) error
	dfs = func(id types.TypeID) error {
		if seen[id] {
			return nil
		}
		seen[id] = true
		key, _ := snapshot.Key(id)
		var deps []types.TypeID
		switch key.Kind() {
		case types.Tuple:
			deps, _ = key.Elements()
		case types.Optional:
			if c, ok := key.Child(); ok {
				deps = []types.TypeID{c}
			}
		case types.Array:
			// An array-of-plain-struct type's inline `elem data[length]` member
			// names its element's OWN pebble_struct_<typeID>_t typedef (see
			// arrayElementCType), which lives in the aggregate block — so the
			// element (or a nested array chain that bottoms out in a plain
			// struct) must be pushed BEFORE the array in the postorder, exactly
			// like a tuple/optional/struct dependency. Recursing through the
			// element here guarantees the plain struct's typedef precedes the
			// array's regardless of root iteration order (F5-18).
			if _, c, ok := key.Array(); ok && (isPlainStructField(unit, snapshot, c) || isPlainStructArrayElement(unit, snapshot, c)) {
				deps = []types.TypeID{c}
			}
		case types.Nominal:
			if in, ok := structByType[id]; ok {
				for _, f := range in.fields {
					deps = append(deps, f.typ)
				}
			}
		}
		for _, dep := range deps {
			// A compiler-builtin runtime type (Allocator, Context) is Nominal
			// like an ordinary struct but is NOT an aggregate this pass orders:
			// its C typedef (PebbleAllocator / PebbleContext) is hand-written in
			// pebble_rt.h, never emitted here, and structByType has no entry for
			// it (it has no TypeDeclaration, so collectStructTypes never
			// collects it), so recursing would push a zero-valued structInfo
			// into the postorder and buildStructTypedef would reject it. Skip it
			// entirely — mirroring collectStructTypesWalk's runtimeType==0 guard
			// (a runtime type needs no typedef of its own).
			if (isTuple(snapshot, dep) || isOptional(snapshot, dep) || isPlainStructArrayElement(unit, snapshot, dep) || (isStruct(snapshot, dep) && runtimeType(unit, snapshot, dep) == 0)) && !isEnumType(unit, snapshot, dep) && !isUntaggedUnionType(unit, snapshot, dep) {
				if err := dfs(dep); err != nil {
					return err
				}
			}
		}
		post = append(post, id)
		return nil
	}
	all := append(append(append([]types.TypeID{}, tuples...), optionals...), func() []types.TypeID {
		r := make([]types.TypeID, len(structs))
		for i := range structs {
			r[i] = structs[i].typ
		}
		return r
	}()...)
	for _, id := range all {
		if err := dfs(id); err != nil {
			return aggregateTypeOrder{}, err
		}
	}
	for _, id := range post {
		if isTuple(snapshot, id) {
			result.tuples = append(result.tuples, id)
		} else if isOptional(snapshot, id) {
			result.optionals = append(result.optionals, id)
		} else if isArray(snapshot, id) {
			result.arrays = append(result.arrays, id)
		} else if isStruct(snapshot, id) {
			result.structs = append(result.structs, structByType[id])
		}
	}
	result.all = post
	return result, nil
}

// buildTupleTypedefs builds the C text of one struct typedef per tuple type in
// ids, in order, each joined by a newline. The caller (Emit) supplies ids in
// first-encountered order from the tuple-type collection pass, so every tuple
// type the emitted program references has exactly one typedef here, written
// before any function definition in the final output.
func buildTupleTypedefs(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		text, err := buildTupleTypedef(unit, snapshot, width, id, nil)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

func buildArrayTypedefs(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	// A NESTED array typedef's `.data` member is an inline `elem data[length]`
	// naming its array-typed element's own pebble_array_<innerID>_t typedef
	// (see arrayElementCType's array case), so each array type's element chain
	// must be emitted BEFORE the array type itself — C requires the inner
	// typedef fully defined before the outer references it. ids arrives in
	// first-encountered order from the collection pass (which may list an
	// outer before its inner), so the pass emits each id in dependency-first
	// DFS postorder, deduplicating within this block.
	emitted := make(map[types.TypeID]bool, len(ids))
	var build func(types.TypeID) error
	build = func(id types.TypeID) error {
		if emitted[id] {
			return nil
		}
		key, ok := snapshot.Key(id)
		if !ok {
			return fmt.Errorf("array type %d is not in the type snapshot", id)
		}
		_, element, ok := key.Array()
		if !ok {
			return fmt.Errorf("type %s is not an array type", describeType(snapshot, id))
		}
		if isArray(snapshot, element) {
			if err := build(element); err != nil {
				return err
			}
		}
		emitted[id] = true
		text, err := buildArrayTypedefText(unit, snapshot, width, id)
		if err != nil {
			return err
		}
		texts = append(texts, text)
		return nil
	}
	for _, id := range ids {
		if err := build(id); err != nil {
			return "", err
		}
	}
	return strings.Join(texts, "\n"), nil
}

// buildArrayTypedefText builds one array type's struct typedef:
//
//	typedef struct {
//	    <elem> data[<length>];
//	} pebble_array_<typeID>_t;
//
// with the element's C type from arrayElementCType (which validates the
// element). The caller is responsible for the element's own typedef (or
// builtin) being defined before this text is emitted.
func buildArrayTypedefText(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return "", fmt.Errorf("array type %d is not in the type snapshot", id)
	}
	length, element, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("type %s is not an array type", describeType(snapshot, id))
	}
	ctype, err := arrayElementCType(unit, snapshot, width, element)
	if err != nil {
		return "", fmt.Errorf("array type %s: %v", describeType(snapshot, id), err)
	}
	return fmt.Sprintf("typedef struct {\n    %s data[%d];\n} %s;", ctype, length, arrayTypeName(id)), nil
}

func buildAggregateTypedefs(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID, infos []structInfo, tagged map[types.TypeID]bool) (string, error) {
	structs := make(map[types.TypeID]structInfo, len(infos))
	for _, info := range infos {
		structs[info.typ] = info
	}
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		var text string
		var err error
		switch {
		case isTuple(snapshot, id):
			text, err = buildTupleTypedef(unit, snapshot, width, id, tagged)
		case isOptional(snapshot, id):
			text, err = buildOptionalTypedef(st, unit, snapshot, width, id, tagged)
		case isArray(snapshot, id):
			// An array-of-plain-struct type reached through an aggregate's
			// field/payload/element dependency is interleaved into this block
			// at its DFS-postorder position (see orderAggregateTypes' Array
			// case): its element's plain-struct typedef has already been
			// emitted earlier in this same block, and the aggregate that
			// references the array follows it, so emitting the array's own
			// typedef here satisfies C's define-before-use rule without the
			// field-array block leading the aggregate block (F5-18).
			text, err = buildArrayTypedefText(unit, snapshot, width, id)
		case isStruct(snapshot, id):
			text, err = buildStructTypedef(st, unit, snapshot, width, structs[id], tagged)
		}
		if err != nil {
			return "", err
		}
		if text != "" {
			texts = append(texts, text)
		}
	}
	return strings.Join(texts, "\n"), nil
}

// buildTupleTypedef builds the C text of one tuple type's struct typedef, with
// positional fields `_0`, `_1`, ... in element order (mirroring the old
// backend's own tuple-field naming convention, without the old 9-field cap):
//
//	typedef struct {
//	    int32_t _0;
//	    bool _1;
//	} pebble_tuple_<typeID>_t;
//
// Each field's C type comes from tupleElementCType, which validates the
// element is the entry's width or bool. A TypeID that is not a tuple type in
// the snapshot is a clean rejection, not a guessed layout.
func buildTupleTypedef(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID, tagged map[types.TypeID]bool) (string, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return "", fmt.Errorf("tuple type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Tuple {
		return "", fmt.Errorf("type %s is a %v, want a tuple type", tupleTypeName(id), key.Kind())
	}
	elements, ok := key.Elements()
	if !ok {
		return "", fmt.Errorf("tuple type %s has no element list", tupleTypeName(id))
	}
	fields := make([]string, len(elements))
	for i, element := range elements {
		ctype, err := tupleElementCType(unit, snapshot, width, element)
		if err != nil {
			return "", fmt.Errorf("tuple type %s: %v", tupleTypeName(id), err)
		}
		fields[i] = "    " + ctype + fmt.Sprintf(" _%d;", i)
	}
	head := "typedef struct"
	if tagged[id] {
		head = "typedef struct " + strings.TrimSuffix(tupleTypeName(id), "_t")
	}
	return fmt.Sprintf("%s {\n%s\n} %s;", head, strings.Join(fields, "\n"), tupleTypeName(id)), nil
}

// buildOptionalTypedefs builds the C text of one struct typedef per optional
// type in ids, in order, each joined by a newline. The caller (Emit) supplies
// ids in first-encountered order from the optional-type collection pass, so
// every optional type the emitted program references has exactly one typedef
// here, written before any function definition in the final output.
func buildOptionalTypedefs(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		text, err := buildOptionalTypedef(st, unit, snapshot, width, id, nil)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildOptionalTypedef builds the C text of one optional type's struct typedef:
//
//	typedef struct {
//	    bool has_value;
//	    int32_t value;
//	} pebble_optional_<typeID>_t;
//
// The value field's C type is the payload's own type (int32_t/int64_t for the
// entry's width, bool for a bool payload, or the payload's enum typedef for an
// enum payload). A TypeID that is not an optional type in the snapshot is a
// clean rejection, not a guessed layout.
func buildOptionalTypedef(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID, tagged map[types.TypeID]bool) (string, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return "", fmt.Errorf("optional type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Optional {
		return "", fmt.Errorf("type %s is a %v, want an optional type", optionalTypeName(id), key.Kind())
	}
	payloadType, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("optional type %s has no payload type", optionalTypeName(id))
	}
	valueCType, err := optionalPayloadCType(st, unit, snapshot, width, payloadType)
	if err != nil {
		return "", fmt.Errorf("optional type %s: %v", optionalTypeName(id), err)
	}
	head := "typedef struct"
	if tagged[id] {
		head = "typedef struct " + strings.TrimSuffix(optionalTypeName(id), "_t")
	}
	return fmt.Sprintf("%s {\n    bool has_value;\n    %s value;\n} %s;", head, valueCType, optionalTypeName(id)), nil
}

// buildStructTypedefs builds the C text of one struct typedef per struct type
// in infos, in order, each joined by a newline. The caller (Emit) supplies
// infos in first-encountered order from the struct-type collection pass, so
// every struct type the emitted program references has exactly one typedef
// here, written before any function definition in the final output.
func buildStructTypedefs(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, infos []structInfo) (string, error) {
	texts := make([]string, 0, len(infos))
	for _, info := range infos {
		text, err := buildStructTypedef(st, unit, snapshot, width, info, nil)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildStructTypedef builds the C text of one struct type's struct typedef,
// with one field per declared struct field, in the struct's *declared* order
// (from the TypeDecl's Members list, resolved by collectStructTypes — never
// the construction-site order a RecordConstruct's Fields carry), each named
// deterministically from the field's own stable symbol.SymbolID:
//
//	typedef struct {
//	    int32_t pebble_field_25;
//	    bool pebble_field_26;
//	} pebble_struct_<typeID>_t;
//
// Naming each C field from the field's symbol ID (mirroring the
// pebble_local_<symbolID> / pebble_fn_<symbolID> discipline) makes a C-field
// name collision impossible even if a source field name were a C keyword or
// duplicated another identifier. Each field's C type comes from
// structFieldCType, which validates the field is a fixed-width integer, bool,
// a supported compound type, or a runtime type.
// A structInfo whose TypeID is not a Nominal type in the snapshot is a clean
// rejection, not a guessed layout (defense for hand-built IR; collectStructTypes
// has already resolved every collected TypeID through resolveStructInfo).
func buildStructTypedef(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, info structInfo, tagged map[types.TypeID]bool) (string, error) {
	key, ok := snapshot.Key(info.typ)
	if !ok {
		return "", fmt.Errorf("struct type %d is not in the type snapshot", info.typ)
	}
	if key.Kind() != types.Nominal {
		return "", fmt.Errorf("type %s is a %v, want a struct type", structTypeName(info.typ), key.Kind())
	}
	fields := make([]string, len(info.fields))
	for i, field := range info.fields {
		ctype, err := structFieldCType(st, unit, snapshot, width, field.typ)
		if err != nil {
			return "", fmt.Errorf("struct type %s: %v", structTypeName(info.typ), err)
		}
		fields[i] = "    " + ctype + fmt.Sprintf(" pebble_field_%d;", field.member)
	}
	head := "typedef struct"
	if tagged[info.typ] {
		head = "typedef struct " + strings.TrimSuffix(structTypeName(info.typ), "_t")
	}
	return fmt.Sprintf("%s {\n%s\n} %s;", head, strings.Join(fields, "\n"), structTypeName(info.typ)), nil
}

// collectUnionPayloadStructs returns, in deterministic order, the PLAIN struct
// types used as tagged-union variant payloads (see
// unionPayloadCTypeAdmissible / isPlainStructPayload): each such struct's own
// typedef must be emitted BEFORE the union typedef block that references it as
// a payload member, but the union block leads the aggregate block that holds
// the struct typedefs (see Emit's assembly) — so Emit hoists these
// self-contained struct typedefs ahead of the union block and removes them
// from the aggregate block they would otherwise be double-emitted in. The
// structInfo for each payload type comes from the caller's already-ordered
// aggregate collection (orderAggregateTypes); a plain-struct payload absent
// from it — a struct with no construction, field, parameter, or other
// reference outside the union construction itself — is a clean error, since
// no typedef text could be built for it.
func collectUnionPayloadStructs(unit *tir.Unit, snapshot *types.Snapshot, unionInfos []unionInfo, ordered aggregateTypeOrder) ([]structInfo, error) {
	byType := make(map[types.TypeID]structInfo, len(ordered.structs))
	for _, info := range ordered.structs {
		byType[info.typ] = info
	}
	seen := make(map[types.TypeID]bool)
	var out []structInfo
	for _, info := range unionInfos {
		for _, member := range info.members {
			if !isStruct(snapshot, member.payloadType) || !isPlainStructPayload(unit, snapshot, member.payloadType) || seen[member.payloadType] {
				continue
			}
			seen[member.payloadType] = true
			sinfo, ok := byType[member.payloadType]
			if !ok {
				return nil, fmt.Errorf("union type %s references struct payload type %s that was not collected", unionTypeName(info.typ), describeType(snapshot, member.payloadType))
			}
			out = append(out, sinfo)
		}
	}
	return out, nil
}

// collectFunctionParamAndResultStructs returns, in deterministic order, the
// PLAIN struct types used as first-class function type parameters or results
// (F5-19 for parameters, F5-20 for results): each such struct's own typedef
// must be emitted BEFORE the function typedef block that references it, but the
// function block leads the aggregate block that holds the struct typedefs (see
// Emit's assembly) — so Emit hoists these self-contained struct typedefs ahead
// of the function block and removes them from the aggregate block they would
// otherwise be double-emitted in. The structInfo for each parameter or result
// type comes from the caller's already-ordered aggregate collection
// (orderAggregateTypes); a plain-struct parameter or result absent from it — a
// struct with no construction, field, parameter, or other reference outside the
// function type itself — is a clean error, since no typedef text could be built
// for it. Only structs that themselves do NOT carry function-typed fields are
// hoisted (a struct whose field is a function type needs its typedef AFTER the
// function block, creating a circular dependency if hoisted); such structs stay
// in the aggregate block where they correctly follow the function block.
func collectFunctionParamAndResultStructs(unit *tir.Unit, snapshot *types.Snapshot, functionTypes []types.TypeID, ordered aggregateTypeOrder) ([]structInfo, error) {
	byType := make(map[types.TypeID]structInfo, len(ordered.structs))
	for _, info := range ordered.structs {
		byType[info.typ] = info
	}
	seen := make(map[types.TypeID]bool)
	var out []structInfo
	for _, fnTypeID := range functionTypes {
		key, ok := snapshot.Key(fnTypeID)
		if !ok || key.Kind() != types.Function {
			continue
		}
		_, params, result, _, ok := key.Function()
		if !ok {
			continue
		}
		for _, paramType := range params {
			if !isStruct(snapshot, paramType) || !isPlainStructField(unit, snapshot, paramType) || seen[paramType] {
				continue
			}
			// A struct whose own field is a function type needs its typedef
			// AFTER the function block (so it can reference function typedefs),
			// so it must NOT be hoisted — doing so would create a circular C
			// dependency (the struct's typedef would name an undefined fnptr
			// typedef, and the fnptr typedef would name an undefined struct
			// typedef). Such structs stay in the aggregate block below the
			// function block where the ordering is correct.
			if hasFunctionTypedFields(unit, snapshot, paramType) {
				continue
			}
			seen[paramType] = true
			sinfo, ok := byType[paramType]
			if !ok {
				return nil, fmt.Errorf("function type %s references struct parameter type %s that was not collected", describeType(snapshot, fnTypeID), describeType(snapshot, paramType))
			}
			out = append(out, sinfo)
		}
		// Plain struct results (F5-20): same hoisting discipline as parameters —
		// the result's typedef must precede the function typedef, and only plain
		// structs (all scalar-ish fields) qualify because their typedef is fully
		// self-contained; non-plain structs, tuples, optionals, slices, arrays,
		// pointers, and function-typed results are rejected here and remain in
		// the aggregate block. The circular-dependency guard applies identically:
		// a struct whose field is a function type cannot be hoisted.
		if !seen[result] && isStruct(snapshot, result) && isPlainStructField(unit, snapshot, result) && !hasFunctionTypedFields(unit, snapshot, result) {
			seen[result] = true
			sinfo, ok := byType[result]
			if !ok {
				return nil, fmt.Errorf("function type %s references struct result type %s that was not collected", describeType(snapshot, fnTypeID), describeType(snapshot, result))
			}
			out = append(out, sinfo)
		}
	}
	return out, nil
}

// hasFunctionTypedFields reports whether a struct type carries any field whose
// type is a function type. This is used to determine whether a struct's typedef
// must come AFTER the function typedef block (so it can reference function
// typedefs) — if so, the struct should NOT be hoisted ahead of the function
// block.
func hasFunctionTypedFields(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	key, ok := snapshot.Key(id)
	if !ok || key.Kind() != types.Nominal {
		return false
	}
	decl, arguments, ok := key.Nominal()
	if !ok {
		return false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return false
	}
	var substitutions map[symbol.SymbolID]types.TypeID
	if len(arguments) > 0 {
		substitutions = structSubstitutions(unit, snapshot, decl, arguments)
	}
	for i := range typeDecl.Members {
		fieldType := types.TypeID(0)
		if i < len(typeDecl.MemberTypes) {
			fieldType = typeDecl.MemberTypes[i]
		}
		if fieldType != 0 && substitutions != nil {
			substituted, err := snapshot.Substitute(fieldType, substitutions)
			if err != nil {
				return false
			}
			fieldType = substituted
		}
		if fieldType != 0 && isFunctionType(snapshot, fieldType) {
			return true
		}
	}
	return false
}

// buildUnionTypedefs builds the C text of one tagged-union typedef pair per
// union type in infos, in dependency-first DFS postorder, each joined by a
// newline. Each pair is the discriminant enum typedef followed by the tagged
// struct typedef (in that order, since the struct typedef's tag field
// references the enum typedef by name — C requires a type fully defined before
// use). A NESTED union typedef's payload member is declared with its
// union-typed payload's OWN pebble_union_<innerID>_t typedef (see
// unionMemberCType's union case), so each union-typed payload member chain must
// be emitted BEFORE the union type itself — C requires the inner typedef fully
// defined before the outer references it. infos arrives in first-encountered
// order from the collection pass, which may list an outer before its inner (an
// INLINE `Outer.value(Inner.b(7))` construction records Outer first, the inner
// union only reached through the outer's payload child), so the pass emits each
// info in dependency-first DFS postorder, deduplicating within this block. The
// caller (Emit) supplies infos from the union-type collection pass, so every
// union type the emitted program references has exactly one pair here, written
// before any function definition in the final output.
func buildUnionTypedefs(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, infos []unionInfo) (string, error) {
	byID := make(map[types.TypeID]unionInfo, len(infos))
	for _, info := range infos {
		byID[info.typ] = info
	}
	texts := make([]string, 0, len(infos))
	emitted := make(map[types.TypeID]bool, len(infos))
	var build func(unionInfo) error
	build = func(info unionInfo) error {
		if emitted[info.typ] {
			return nil
		}
		// Emit every union-typed payload dependency first (DFS postorder):
		// the union-typed payload member's typedef name is inline-referenced by
		// this union's own typedef text, so the inner typedef must be fully
		// emitted before the outer's. The collection walk reaches a referenced
		// payload union through the constructing VariantConstruct's payload
		// child (see collectUnionTypesWalk), so every referenced payload union
		// is in infos; the not-collected path below is defense for hand-built
		// IR only.
		for _, member := range info.members {
			if !isUnionEnumType(unit, snapshot, member.payloadType) {
				continue
			}
			inner, ok := byID[member.payloadType]
			if !ok {
				return fmt.Errorf("union type %s references union payload type %s that was not collected", unionTypeName(info.typ), describeType(snapshot, member.payloadType))
			}
			if err := build(inner); err != nil {
				return err
			}
		}
		emitted[info.typ] = true
		text, err := buildUnionTypedef(st, unit, snapshot, width, info)
		if err != nil {
			return err
		}
		texts = append(texts, text)
		return nil
	}
	for _, info := range infos {
		if err := build(info); err != nil {
			return "", err
		}
	}
	return strings.Join(texts, "\n"), nil
}

// buildUnionTypedef builds the C text of one tagged-union type's typedef pair:
// the discriminant enum typedef (reused verbatim from buildEnumTypedef over the
// union's variants in declared order — the declared order IS the discriminant,
// exactly like a plain enum, so the switch case labels and the stored tag
// values agree with the typedef by construction) followed by the tagged struct
// typedef:
//
//	typedef enum {
//	    pebble_variant_25,
//	    pebble_variant_26,
//	} pebble_enum_23_t;
//	typedef struct {
//	    pebble_enum_23_t tag;
//	    union {
//	        int32_t pebble_field_26;
//	    } payload;
//	} pebble_union_23_t;
//
// The tag field is typed as the discriminant enum typedef, the union's
// identity carrier: a tagged union's value IS its discriminant plus the
// payload union, and the discriminant ordinal scheme is identical to a plain
// enum's. Each payload union member is named pebble_field_<memberSymbolID>
// from the variant's own stable symbol.SymbolID, exactly the naming discipline
// struct fields use (see buildStructTypedef) — deliberately distinct from
// pebble_variant_<memberSymbolID>, which names the *enum constant* (the tag
// value), not a union member, so the two can never collide. One member is
// declared per non-void variant actually constructed somewhere in the reachable
// program (the unionInfo's members, resolved by resolveUnionInfo); a variant
// never constructed has no member, since no payload for it is ever read or
// written. A unionInfo whose TypeID is not an enum-shaped Nominal type in the
// snapshot is a clean rejection, not a guessed layout (defense for hand-built
// IR; collectUnionTypes has already resolved every collected TypeID through
// resolveUnionInfo, which requires a tagged-union type).
func buildUnionTypedef(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, info unionInfo) (string, error) {
	key, ok := snapshot.Key(info.typ)
	if !ok {
		return "", fmt.Errorf("union type %d is not in the type snapshot", info.typ)
	}
	if key.Kind() != types.Nominal {
		return "", fmt.Errorf("type %s is a %v, want a tagged-union type", unionTypeName(info.typ), key.Kind())
	}
	enumText, err := buildEnumTypedef(snapshot, enumInfo{typ: info.typ, decl: info.decl, variants: info.variants}, nil)
	if err != nil {
		return "", err
	}
	members := make([]string, len(info.members))
	for i, member := range info.members {
		ctype, err := unionMemberCType(st, unit, snapshot, width, member.payloadType)
		if err != nil {
			return "", fmt.Errorf("union type %s: %v", unionTypeName(info.typ), err)
		}
		members[i] = "        " + ctype + fmt.Sprintf(" pebble_field_%d;", member.member)
	}
	structText := fmt.Sprintf("typedef struct {\n    %s tag;\n    union {\n%s\n    } payload;\n} %s;", enumTypeName(info.typ), strings.Join(members, "\n"), unionTypeName(info.typ))
	return enumText + "\n" + structText, nil
}

// untaggedUnionFieldCType is the C type one untagged-union field of the given
// type is declared with in its union's typedef, restricted to the SCALAR field
// types this slice supports: any fixed-width integer builtin resolved to its
// OWN width (int32_t/int64_t for the entry's width, uint64_t for uint/u64, and
// the narrow-width int8_t/int16_t/uint8_t/uint16_t/uint32_t for i8/i16/u8/u16/
// u32 — each at its own width, independent of the ambient entry width), bool
// for a bool field, and int32_t for a char field. Any other field type — a
// struct, array, tuple, optional, str, plain enum, or tagged union — is a
// clean rejection naming what is unsupported, leaving non-scalar untagged-union
// fields for a future slice.
func untaggedUnionFieldCType(snapshot *types.Snapshot, id types.TypeID) (string, error) {
	if fieldWidth, integerField := resolvedBuiltin(snapshot, id); integerField && cType(fieldWidth) != "" {
		return cType(fieldWidth), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isChar(snapshot, id) {
		return "int32_t", nil
	}
	return "", fmt.Errorf("untagged-union field type %s is not supported: this slice supports only scalar (fixed-width integer, uint, bool, char) untagged-union fields; struct, array, tuple, optional, str, and enum fields are a future slice", describeType(snapshot, id))
}

// buildUntaggedUnionTypedefs builds the C text of one real C union typedef per
// untagged-union type in infos, in order, each joined by a newline:
//
//	typedef union {
//	    int32_t pebble_field_25;
//	    bool pebble_field_26;
//	} pebble_union_<typeID>_t;
//
// The caller (Emit) supplies infos from the untagged-union-type collection
// pass, so every untagged union type the emitted program references has
// exactly one typedef here, written before any function definition in the
// final output. An untagged union's typedef carries NO tag field at all: it is
// the deliberate unsafe reinterpret-the-bytes contract, matching V1's direct C
// union semantics — the caller's collection pass has already excluded every
// untagged union from the struct collection, so no pebble_struct_<typeID>_t
// typedef ever duplicates this one.
func buildUntaggedUnionTypedefs(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, infos []unionInfo) (string, error) {
	texts := make([]string, 0, len(infos))
	for _, info := range infos {
		text, err := buildUntaggedUnionTypedef(st, unit, snapshot, width, info)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildUntaggedUnionTypedef builds the C text of one untagged-union type's
// real C union typedef:
//
//	typedef union {
//	    int32_t pebble_field_25;
//	    bool pebble_field_26;
//	} pebble_union_<typeID>_t;
//
// with one member per declared field, in the union's *declared* order (from
// the TypeDecl's Members list, resolved by resolveUntaggedUnionInfo), each
// named pebble_field_<memberSymbolID> exactly like a struct field, so a field
// read (`d.a`) and a field write (`d.a = e;`) are plain `.pebble_field_<m>`
// projections on the same C union storage. Each member's C type comes from
// untaggedUnionFieldCType, which restricts this first slice to scalar field
// types. There is NO tag field and no discriminant enum — the tagged-union
// shape buildUnionTypedef emits is deliberately absent, since there is nothing
// to track or check under the unsafe contract. A unionInfo whose TypeID is not
// an untagged-union Nominal type in the snapshot is a clean rejection, not a
// guessed layout (defense for hand-built IR; collectUntaggedUnionTypes has
// already resolved every collected TypeID through resolveUntaggedUnionInfo).
func buildUntaggedUnionTypedef(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, info unionInfo) (string, error) {
	key, ok := snapshot.Key(info.typ)
	if !ok {
		return "", fmt.Errorf("union type %d is not in the type snapshot", info.typ)
	}
	if key.Kind() != types.Nominal {
		return "", fmt.Errorf("type %s is a %v, want an untagged-union type", unionTypeName(info.typ), key.Kind())
	}
	if !isUntaggedUnionType(unit, snapshot, info.typ) {
		return "", fmt.Errorf("type %s is not an untagged union type", unionTypeName(info.typ))
	}
	members := make([]string, len(info.members))
	for i, member := range info.members {
		ctype, err := untaggedUnionFieldCType(snapshot, member.payloadType)
		if err != nil {
			return "", fmt.Errorf("union type %s: %v", unionTypeName(info.typ), err)
		}
		members[i] = "    " + ctype + fmt.Sprintf(" pebble_field_%d;", member.member)
	}
	return fmt.Sprintf("typedef union {\n%s\n} %s;", strings.Join(members, "\n"), unionTypeName(info.typ)), nil
}

// buildEnumTypedefs builds the C text of one enum typedef per plain enum type
// in infos, in order, each joined by a newline. The caller (Emit) supplies
// infos in first-encountered order from the enum-type collection pass, so
// every enum type the emitted program references has exactly one typedef here,
// written before any function definition in the final output. tagged marks the
// enum types whose typedef must carry the matching enum tag (an enum used as a
// slice element: its typedef name is forward-declared before the slice block,
// so the full definition must complete that same tag — see
// buildSliceElementForwardDeclarations).
func buildEnumTypedefs(snapshot *types.Snapshot, infos []enumInfo, tagged map[types.TypeID]bool) (string, error) {
	texts := make([]string, 0, len(infos))
	for _, info := range infos {
		text, err := buildEnumTypedef(snapshot, info, tagged)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildEnumTypedef builds the C text of one plain enum type's enum typedef,
// with one named constant per declared variant, in the enum's *declared* order
// (from the TypeDecl's Members list, resolved by collectEnumTypes — the same
// ordering a struct typedef resolves its fields by), each named
// deterministically from the variant's own stable symbol.SymbolID:
//
//	typedef enum {
//	    pebble_variant_25,
//	    pebble_variant_26,
//	    pebble_variant_27,
//	} pebble_enum_23_t;
//
// When tagged marks the type, the typedef carries the matching enum tag
// (`typedef enum pebble_enum_23 { ... } pebble_enum_23_t;`) so the definition
// completes the same type a forward declaration emitted before the slice
// typedef block started (an enum used as a slice element), exactly as a tagged
// struct's definition completes its own forward declaration.
//
// The declared order IS the discriminant: C assigns the constants the ordinal
// values 0, 1, 2, ... in declaration order, so variant Members[i] is the value
// i — the natural, stable discriminant the switch case labels and the values
// stored in enum-typed locals agree with by construction. Naming each constant
// from the variant's symbol ID (mirroring the pebble_field_<memberSymbolID>
// discipline) makes a C constant-name collision impossible even if a source
// variant name were a C keyword or duplicated another identifier. An enumInfo
// whose TypeID is not a Nominal type in the snapshot is a clean rejection, not
// a guessed layout (defense for hand-built IR; collectEnumTypes has already
// resolved every collected TypeID through resolveEnumInfo, which requires a
// plain enum).
func buildEnumTypedef(snapshot *types.Snapshot, info enumInfo, tagged map[types.TypeID]bool) (string, error) {
	key, ok := snapshot.Key(info.typ)
	if !ok {
		return "", fmt.Errorf("enum type %d is not in the type snapshot", info.typ)
	}
	if key.Kind() != types.Nominal {
		return "", fmt.Errorf("type %s is a %v, want an enum type", enumTypeName(info.typ), key.Kind())
	}
	if len(info.variants) == 0 {
		return "", fmt.Errorf("enum type %s has no declared variants", enumTypeName(info.typ))
	}
	constants := make([]string, len(info.variants))
	for i, variant := range info.variants {
		constants[i] = "    " + enumVariantName(variant) + ","
	}
	head := "typedef enum"
	if tagged[info.typ] {
		head = "typedef enum " + strings.TrimSuffix(enumTypeName(info.typ), "_t")
	}
	return fmt.Sprintf("%s {\n%s\n} %s;", head, strings.Join(constants, "\n"), enumTypeName(info.typ)), nil
}

// sliceElementForwardDeclaredTypes reports, for every slice type in infos,
// which element types (a struct, tuple, or optional — or, since enum-element
// slices, a plain enum) its .data pointer names in the slice's typedef text.
// Those element typedef names must be DECLARED — even incompletely — before the
// slice typedef that points at them, because the slice typedef block is
// emitted before the aggregate/enum block that fully defines them; C resolves a
// pointer to a declared-but-incomplete type. The returned set marks exactly
// those element types whose FULL definition must carry the matching struct or
// enum tag (buildStructTypedef / buildTupleTypedef / buildOptionalTypedef emit
// `typedef struct pebble_<kind>_<id> {` and buildEnumTypedef emits `typedef
// enum pebble_enum_<id> {` for them), so the forward declaration and the
// definition complete the same C type. A slice whose element is a scalar
// (integer, char, bool) needs no forward declaration: its data field names a
// builtin C type that needs no typedef. A tagged union is excluded: it is
// rejected by sliceElementCType before any typedef could be emitted.
func sliceElementForwardDeclaredTypes(unit *tir.Unit, snapshot *types.Snapshot, infos []sliceInfo) map[types.TypeID]bool {
	out := make(map[types.TypeID]bool)
	for _, info := range infos {
		if isStruct(snapshot, info.elementType) || isTuple(snapshot, info.elementType) || isOptional(snapshot, info.elementType) {
			out[info.elementType] = true
		} else if isDefinitelyEnumType(unit, snapshot, info.elementType) && !isUnionEnumType(unit, snapshot, info.elementType) {
			out[info.elementType] = true
		}
	}
	return out
}

// buildSliceElementForwardDeclarations builds the C text of one incomplete
// typedef declaration per element type in tagged, ordered by TypeID for
// deterministic output (each declaration is self-contained, so order does not
// matter semantically):
//
//	typedef struct pebble_struct_<typeID> pebble_struct_<typeID>_t;
//	typedef struct pebble_tuple_<typeID> pebble_tuple_<typeID>_t;
//	typedef struct pebble_optional_<typeID> pebble_optional_<typeID>_t;
//	typedef enum pebble_enum_<typeID> pebble_enum_<typeID>_t;
//
// Each declares the struct or enum TAG pebble_<kind>_<typeID> and the typedef
// name pebble_<kind>_<typeID>_t together, so a later full definition of the
// same tag (emitted with the matching tag by buildStructTypedef /
// buildTupleTypedef / buildOptionalTypedef / buildEnumTypedef) completes the
// type, and a pointer field in a slice typedef emitted between the two
// (`pebble_struct_<typeID>_t *data;`) is valid C against the incomplete
// declaration.
func buildSliceElementForwardDeclarations(unit *tir.Unit, snapshot *types.Snapshot, tagged map[types.TypeID]bool) string {
	ids := make([]types.TypeID, 0, len(tagged))
	for id := range tagged {
		ids = append(ids, id)
	}
	sort.Slice(ids, func(i, j int) bool { return ids[i] < ids[j] })
	decls := make([]string, 0, len(ids))
	for _, id := range ids {
		name := structTypeName(id)
		keyword := "struct"
		if isTuple(snapshot, id) {
			name = tupleTypeName(id)
		} else if isOptional(snapshot, id) {
			name = optionalTypeName(id)
		} else if isDefinitelyEnumType(unit, snapshot, id) {
			name = enumTypeName(id)
			keyword = "enum"
		}
		tag := strings.TrimSuffix(name, "_t")
		decls = append(decls, fmt.Sprintf("typedef %s %s %s;", keyword, tag, name))
	}
	return strings.Join(decls, "\n")
}

// buildSliceTypedefs builds the C text for every distinct slice type, one
// typedef per slice type, joining them with newlines. Each slice type is a
// small C struct with a data pointer and a length field.
func buildSliceTypedefs(unit *tir.Unit, snapshot *types.Snapshot, infos []sliceInfo, width types.BuiltinKind) (string, error) {
	texts := make([]string, 0, len(infos))
	for _, info := range infos {
		text, err := buildSliceTypedef(unit, snapshot, info, width)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildSliceTypedef builds the C text of one slice type's struct typedef:
//
//	typedef struct {
//	    int32_t *data;
//	    size_t len;
//	} pebble_slice_<typeID>_t;
//
// Field names data/len match PebbleStrSlice's own naming in pebble_rt.h.
func buildSliceTypedef(unit *tir.Unit, snapshot *types.Snapshot, info sliceInfo, width types.BuiltinKind) (string, error) {
	if info.elementType == 0 {
		return "", fmt.Errorf("slice type %s has no element type", sliceTypeName(info.typ))
	}
	elemCType, err := sliceElementCType(unit, snapshot, width, info.elementType)
	if err != nil {
		return "", fmt.Errorf("slice type %s: %v", sliceTypeName(info.typ), err)
	}
	return fmt.Sprintf("typedef struct {\n    %s *data;\n    size_t len;\n} %s;", elemCType, sliceTypeName(info.typ)), nil
}

// buildFunctionTypedefs builds the C text of one function-pointer typedef per
// function type in ids, in order, each joined by a newline. The caller (Emit)
// supplies ids in first-encountered order from the function-type collection
// pass, so every function type the emitted program references as a first-class
// value has exactly one typedef here, written before any function definition
// in the final output.
func buildFunctionTypedefs(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		text, err := buildFunctionTypedef(st, unit, snapshot, width, id)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildFunctionTypedef builds the C text of one function type's pointer
// typedef, mirroring v1's TYPE_FUNCTION typedef shape adapted to v2's
// TypeID-based naming and ctx-threading convention:
//
//	typedef <result-c-type> (*pebble_fnptr_<typeID>_t)(PebbleContext *ctx, <param-c-types>...);
//
// The function type's own signature is resolved via types.TypeKey.Function()
// (convention, parameters, result, variadic), and every parameter/result C
// type is resolved by functionTypeParamCType / functionTypeResultCType — the
// same C types buildHelperFunctions declares an ordinary Pebble-convention
// helper's parameters and result with, so a hoisted function's C name and the
// fnptr typedef always agree exactly. Only Pebble-convention function types
// are reachable as first-class values in this slice (a C-convention one is a
// clean rejection — see validateFunctionTypeSignature), so the typedef always
// carries the trailing PebbleContext *ctx parameter. Every parameter/result C
// type is self-contained (the entry's cType, uint64_t, bool, int32_t,
// PebbleStr, a float/double via floatCType, a pointer's own `<pointee> *`
// spelling via pointerTypeName, or
// void, or a plain struct's own pebble_struct_<typeID>_t (F5-19: a struct
// parameter's typedef is emitted before this fnptr typedef by Emit's
// typedef-ordering hoisting), so the typedef never references an aggregate
// typedef that might be emitted after it — except for plain-struct parameters
// whose self-contained typedefs are explicitly hoisted ahead of the function
// block by the caller.
func buildFunctionTypedef(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if err := validateFunctionTypeSignature(unit, snapshot, width, id); err != nil {
		return "", err
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return "", fmt.Errorf("function type %d is not in the type snapshot", id)
	}
	_, parameters, result, _, ok := key.Function()
	if !ok {
		return "", fmt.Errorf("type %s is not a function type", describeType(snapshot, id))
	}
	resultCType, err := functionTypeResultCType(st, unit, snapshot, width, result)
	if err != nil {
		return "", err
	}
	paramCTypes := make([]string, len(parameters))
	for i, parameter := range parameters {
		paramCType, err := functionTypeParamCType(st, unit, snapshot, width, parameter)
		if err != nil {
			return "", err
		}
		paramCTypes[i] = paramCType
	}
	if len(paramCTypes) == 0 {
		return fmt.Sprintf("typedef %s (*pebble_fnptr_%d_t)(PebbleContext *ctx);", resultCType, id), nil
	}
	return fmt.Sprintf("typedef %s (*pebble_fnptr_%d_t)(PebbleContext *ctx, %s);", resultCType, id, strings.Join(paramCTypes, ", ")), nil
}

// joinTypedefs joins two typedef text blocks into a single block, with a blank
// line between them when both are non-empty. Either may be empty; the result is
// empty when both are empty. Emit chains it twice (tuple joined with optional,
// then the result joined with struct) so the three typedef families form one
// block in a fixed order.
func joinTypedefs(tupleTypedefs, optionalTypedefs string) string {
	if tupleTypedefs == "" {
		return optionalTypedefs
	}
	if optionalTypedefs == "" {
		return tupleTypedefs
	}
	return tupleTypedefs + "\n" + optionalTypedefs
}
