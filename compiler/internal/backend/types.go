package backend

import (
	"fmt"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// isSupportedSliceElementType reports whether a slice element type is one this
// backend can emit: a fixed-width integer builtin (resolved to its own width by
// resolvedBuiltin/cType — the entry's width, uint, u8, u16, u32, u64, i8, i16,
// i32, or i64), char (the fixed int32_t), bool, f32/f64 (each the plain C
// float/double, the element support slice 86a already gave arrays and this
// slice gives slices), or — matching the aggregate
// element types arrayElementCType/sliceElementCType already accept — a tuple,
// optional, struct, or plain enum element (a plain enum is a simple
// integer-backed C enum typedef, emitted as the slice's `pebble_enum_<id>_t
// *data;` pointer — see sliceElementCType). A tagged union is enum-shaped
// (isDefinitelyEnumType reports true for it) but is NOT a supported element:
// its C representation is the tag-plus-payload struct typedef pair, and
// union-typed slice elements are out of scope, so isUnionEnumType (the
// declaration-level test, independent of whether any payload-carrying
// construction exists) keeps it a clean rejection. isDefinitelyEnumType — the
// positive-declaration-evidence form, no no-evidence fallback — is what
// distinguishes an enum from a method-only struct here (a method-only struct
// has no FieldDeclaration nodes and would be misreported by isEnumType's
// fallback). This is the single shared element gate sliceElementCType,
// buildSliceConstruction, and the index read/write value grammars all consult,
// mirroring how the function-types work admitted integer parameters/results by
// resolvedBuiltin/cType generically instead of a width-specific predicate list.
func isSupportedSliceElementType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	if elementWidth, integerElement := resolvedBuiltin(snapshot, id); integerElement && cType(elementWidth) != "" {
		return true
	}
	if isChar(snapshot, id) || isBool(snapshot, id) || isFloat(snapshot, id) {
		return true
	}
	if isDefinitelyEnumType(unit, snapshot, id) {
		return !isUnionEnumType(unit, snapshot, id)
	}
	return isTuple(snapshot, id) || isOptional(snapshot, id) || isStruct(snapshot, id)
}

// instantiatedFieldType recovers one field's type for a SPECIFIC struct
// instantiation from the unit's own construction evidence: any RecordConstruct
// whose type is exactly structType (not merely whose declaration matches) and
// whose Fields include member, returning that field value node's Type. Two
// specializations of one generic struct share every field symbol, so the
// recovery must be scoped to the instantiation's own type or the first
// constructed instantiation would supply the field type for the rest.
func instantiatedFieldType(unit *tir.Unit, structType types.TypeID, member symbol.SymbolID) (types.TypeID, bool) {
	for _, node := range unit.Nodes() {
		if node.Kind != tir.RecordConstruct || node.Type != structType {
			continue
		}
		for _, field := range node.Fields {
			if field.Field == member {
				if value, ok := unit.Node(field.Value); ok && value.Type != 0 {
					return value.Type, true
				}
			}
		}
	}
	return 0, false
}

// structSubstitutions builds the per-instantiation substitution map for a
// generic struct: it maps each of the struct's OWN type parameters (in their
// declared order, recovered from the struct's generic Nominal key in the
// snapshot — the Nominal whose arguments are that declaration's TypeParameter
// TypeIDs) to the concrete type arguments of the instantiation being resolved.
// The returned map is keyed by parameter declaration symbol, matching
// types.Snapshot.Substitute's key. A non-generic struct or a struct whose
// generic declaration key is absent from the snapshot yields nil (callers then
// fall back to MemberTypes/fieldTypes, which is exactly correct for the
// non-generic case).
func structSubstitutions(unit *tir.Unit, snapshot *types.Snapshot, decl symbol.SymbolID, arguments []types.TypeID) map[symbol.SymbolID]types.TypeID {
	parameters := structTypeParameters(unit, snapshot, decl)
	if len(parameters) == 0 || len(parameters) != len(arguments) {
		return nil
	}
	substitutions := make(map[symbol.SymbolID]types.TypeID, len(parameters))
	for index, parameter := range parameters {
		parameterKey, ok := snapshot.Key(parameter)
		if !ok {
			return nil
		}
		parameterSymbol, ok := parameterKey.TypeParameter()
		if !ok {
			return nil
		}
		substitutions[parameterSymbol] = arguments[index]
	}
	return substitutions
}

// structTypeParameters recovers the ordered list of a struct declaration's own
// type-parameter TypeIDs from the snapshot. The checker interns the generic
// declaration as a Nominal key whose arguments are that declaration's own
// TypeParameter TypeIDs in parameter order (e.g. Pair[K, V]'s declaration key
// carries [<K>, <V>]), which is the one Nominal in the snapshot whose arguments
// are all TypeParameter kinds. Its arguments are therefore the authoritative
// ordered parameter list needed to zip the instantiation's concrete arguments
// back onto the struct's parameters.
//
// That one-Nominal uniqueness assumption does NOT hold in a program where the
// generic declaration is referenced from other generic contexts: every
// generic function or method that names the declaration with its OWN type
// parameters interns a SEPARATE all-TypeParameter Nominal key (e.g.
// Entry[K, V] referenced from rehash[K, V]'s body interns a key whose
// arguments are rehash's own K/V TypeParameters, distinct from Entry's own),
// so a plain first-match scan returns whichever all-TypeParameter key was
// interned earliest — frequently a method/function context's, whose parameter
// symbols never match the field MemberTypes the substitution must rewrite.
// The declaration's OWN parameters are exactly the TypeParameters its own
// field MemberTypes reference (a directly parameter-typed field records the
// parameter's TypeID; each generic context's parameters are distinct symbols),
// so the scan prefers the candidate whose argument symbols include every
// field-referenced parameter symbol, falling back to the first all-TypeParameter
// key only when the declaration has no directly parameter-typed field (in which
// case no field's type depends on the substitution anyway).
func structTypeParameters(unit *tir.Unit, snapshot *types.Snapshot, decl symbol.SymbolID) []types.TypeID {
	referenced := map[symbol.SymbolID]bool{}
	if typeDecl, ok := findTypeDeclaration(unit, decl); ok {
		for _, memberType := range typeDecl.MemberTypes {
			if kind, ok := snapshot.Kind(memberType); ok && kind == types.TypeParameter {
				if key, ok := snapshot.Key(memberType); ok {
					if symbolID, ok := key.TypeParameter(); ok {
						referenced[symbolID] = true
					}
				}
			}
		}
	}
	var fallback []types.TypeID
	for id := range snapshot.IDs() {
		key, ok := snapshot.Key(id)
		if !ok || key.Kind() != types.Nominal {
			continue
		}
		candidate, arguments, ok := key.Nominal()
		if !ok || candidate != decl || len(arguments) == 0 {
			continue
		}
		allParameters := true
		for _, argument := range arguments {
			if kind, ok := snapshot.Kind(argument); !ok || kind != types.TypeParameter {
				allParameters = false
				break
			}
		}
		if !allParameters {
			continue
		}
		if fallback == nil {
			fallback = arguments
		}
		matches := true
		for symbolID := range referenced {
			found := false
			for _, argument := range arguments {
				if key, ok := snapshot.Key(argument); ok {
					if argumentSymbol, ok := key.TypeParameter(); ok && argumentSymbol == symbolID {
						found = true
						break
					}
				}
			}
			if !found {
				matches = false
				break
			}
		}
		if matches {
			return arguments
		}
	}
	return fallback
}

// genericStructMethodSubstitutions computes the per-instantiation substitution
// map for one method call to a generic struct method whose own parameter/result
// types may reference the containing struct's type parameter directly (a
// non-generic method like `fn get(self Box[T]) T`). The checker emits such a
// method as ONE symbolic FunctionDeclaration shared by every instantiation, so
// the concrete instantiation is recoverable only from the call site: the
// method call's receiver node carries the instantiation's concrete type (for a
// pointer receiver the auto-referenced AddressOf carries the pointer type, so
// the pointee is unwrapped first). The receiver's Nominal arguments zip onto
// the struct's own type parameters exactly as the field substitution
// (structSubstitutions) zips them, so the SAME map the field machinery uses
// substitutes the method's signature — no parallel mechanism. A call that is
// not a receiver-bound method call, a generic call (TypeArgs non-empty, which
// the checker's specialization machinery already resolves to a concrete
// FunctionDeclaration), a non-generic receiver, or a generic-struct method
// whose signature is already concrete (nothing in the callee's parameter/result
// types references a substituted parameter) yields nil — the caller then treats
// the callee exactly as before.
func genericStructMethodSubstitutions(unit *tir.Unit, snapshot *types.Snapshot, call tir.Node, callee tir.Node) map[symbol.SymbolID]types.TypeID {
	if call.Kind != tir.MethodCall || len(call.TypeArgs) != 0 || len(call.Children) == 0 {
		return nil
	}
	receiver, ok := unit.Node(call.Children[0])
	if !ok {
		return nil
	}
	structType := receiver.Type
	if key, ok := snapshot.Key(structType); ok && key.Kind() == types.Pointer {
		pointee, ok := key.Child()
		if !ok {
			return nil
		}
		structType = pointee
	}
	key, ok := snapshot.Key(structType)
	if !ok || key.Kind() != types.Nominal {
		return nil
	}
	decl, arguments, ok := key.Nominal()
	if !ok || len(arguments) == 0 {
		return nil
	}
	substitutions := structSubstitutions(unit, snapshot, decl, arguments)
	if substitutions == nil || !declarationReferencesSubstitution(snapshot, callee, substitutions) {
		return nil
	}
	return substitutions
}

// declarationReferencesSubstitution reports whether any of a callable
// declaration's own parameter types or result type actually reference one of
// the substitution's parameter symbols. A generic-struct method whose
// signature is already concrete (`fn reserve(self *Vec[i32], amount i32)`) must
// NOT be treated as needing substitution — its C helper keeps its plain
// pebble_fn_<symbol> name — while `fn get(self Box[T]) T` must. A substitution
// the signature never uses would otherwise stamp every generic-struct method
// with a per-instantiation C name for no reason.
func declarationReferencesSubstitution(snapshot *types.Snapshot, decl tir.Node, substitutions map[symbol.SymbolID]types.TypeID) bool {
	for _, param := range decl.Parameters {
		if substituted, err := snapshot.Substitute(param.Type, substitutions); err == nil && substituted != param.Type {
			return true
		}
	}
	if substituted, err := snapshot.Substitute(decl.ResultType, substitutions); err == nil && substituted != decl.ResultType {
		return true
	}
	return false
}

// substituteDeclarationSignature returns a defensive copy of decl whose
// parameter and result types have had every occurrence of the substitution's
// parameter symbols replaced by the concrete types — the monomorphized
// signature a specialized generic struct method's C helper is built from. The
// copy keeps the original Symbol, Function (FunctionID), Convention, Variadic,
// and body linkage, so findFunctionBody and every FunctionID-keyed mechanism
// keep working on the substituted declaration. The body it points at is
// unchanged: the method's body nodes still carry the symbolic type-parameter
// types, which the body builders resolve through the scope helperSignature
// seeds from these substituted parameter types (a self parameter of Box[int]
// makes every `self.value` field read resolve to int via declaredFieldType).
func substituteDeclarationSignature(snapshot *types.Snapshot, decl tir.Node, substitutions map[symbol.SymbolID]types.TypeID) tir.Node {
	decl.Parameters = append([]tir.Parameter(nil), decl.Parameters...)
	for i := range decl.Parameters {
		if substituted, err := snapshot.Substitute(decl.Parameters[i].Type, substitutions); err == nil {
			decl.Parameters[i].Type = substituted
		}
	}
	if substituted, err := snapshot.Substitute(decl.ResultType, substitutions); err == nil {
		decl.ResultType = substituted
	}
	return decl
}

// isEnumType reports whether id resolves to a plain enum type in the snapshot,
// as opposed to a struct — the two are indistinguishable in the type snapshot
// itself (both are Nominal keys carrying only the declaration symbol), so the
// distinction is resolved from the unit's own node graph directly: every
// declared member of a type carries its own member-declaration node — a
// struct field's is tir.FieldDeclaration, an enum variant's is
// tir.VariantDeclaration (confirmed unconditional: the checker's type-builder
// emits one of these for every member of every TypeDeclaration, independent of
// whether the member is ever actually used anywhere in the reachable program —
// see buildTypes). This is a direct, positive signal, not a heuristic guess
// from usage evidence: a type whose first member has a FieldDeclaration node is
// a struct, and one whose first member has a VariantDeclaration node is an
// enum, regardless of whether the type is ever constructed or field-accessed
// anywhere in the program. (An older version of this function guessed from
// FieldPlace/RecordConstruct usage evidence and defaulted to "enum" when no
// evidence was found either way — wrong for a struct that is declared but
// never constructed or field-accessed anywhere, e.g. only ever named as a
// `none`-optional's payload type, which produced an invalid reference to an
// enum typedef that was never emitted. The declaration-node signal has no such
// blind spot: it needs no usage evidence at all.)
func isEnumType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil || unit == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok || key.Kind() != types.Nominal {
		return false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return false
	}
	members := make(map[symbol.SymbolID]bool, len(typeDecl.Members))
	for _, m := range typeDecl.Members {
		members[m] = true
	}
	for _, node := range unit.Nodes() {
		if !members[node.Symbol] {
			continue
		}
		switch node.Kind {
		case tir.FieldDeclaration:
			return false
		case tir.VariantDeclaration:
			return true
		}
	}
	// No member-declaration node was found at all (a type with zero declared
	// members, if that's even expressible) — fall back to true, matching this
	// function's prior default for the case genuinely no evidence exists.
	return true
}

// isDefinitelyEnumType reports whether id is a Nominal type whose declared
// members are enum variants by positive declaration-node evidence: at least
// one member carries a VariantDeclaration node and none carries a
// FieldDeclaration node. This is the precise form of isEnumType WITHOUT its
// no-evidence fallback (which reports true for a type whose members carry no
// declaration node at all — a method-only struct has no FieldDeclaration
// nodes — and would therefore wrongly exclude such a real struct from
// struct-type collection). It is used only where a false positive would
// wrongly drop a struct type from collection; isEnumType remains the shared
// "is this enum-shaped" test everywhere else.
func isDefinitelyEnumType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	if unit == nil || snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok || key.Kind() != types.Nominal {
		return false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return false
	}
	members := make(map[symbol.SymbolID]bool, len(typeDecl.Members))
	for _, m := range typeDecl.Members {
		members[m] = true
	}
	sawVariant := false
	for _, node := range unit.Nodes() {
		if !members[node.Symbol] {
			continue
		}
		switch node.Kind {
		case tir.FieldDeclaration:
			return false
		case tir.VariantDeclaration:
			sawVariant = true
		}
	}
	return sawVariant
}

// isTaggedUnionType reports whether id is a tagged-union type in this program:
// an enum-shaped Nominal type (see isEnumType) carrying at least one
// payload-carrying construction somewhere in the unit. This is exactly the
// signal collectUnionTypes uses (a payload-carrying VariantConstruct is the
// one node shape that references a tagged-union type), so it agrees with which
// types get a pebble_union_<typeID>_t typedef pair: an enum-shaped type with
// no payload-carrying construction (a plain enum, or a union every variant of
// which is payload-less) is emitted as a plain pebble_enum_<typeID>_t typedef
// instead (see collectEnumTypes / buildUnionTypedef).
func isTaggedUnionType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	if !isEnumType(unit, snapshot, id) {
		return false
	}
	for _, node := range unit.Nodes() {
		if node.Kind == tir.VariantConstruct && node.Type == id && len(node.Children) >= 1 {
			return true
		}
	}
	return false
}

// isUnionEnumType reports whether id is a tagged-union (union enum) type by
// DECLARATION alone: an enum-shaped Nominal type (see isEnumType) whose
// TypeDeclaration carries at least one non-void member type — i.e. it was
// declared as `union enum { ... }` with at least one payload-carrying variant,
// regardless of whether any construction of it exists anywhere in the program.
// A plain enum's variants are payload-less and all carry void member types
// (confirmed against real fixtures: `enum { red, green, blue }` resolves every
// member type to void), so a non-void member type is exactly the
// declaration-level signal that distinguishes a union enum from a plain enum.
// An all-void union enum (`union enum { a void; b void; }`) has no non-void
// member type and is deliberately reported false, matching the backend's
// existing convention that such a type is emitted as a plain
// pebble_enum_<typeID>_t typedef (see isTaggedUnionType's doc: a union every
// variant of which is payload-less is a plain enum). This is the
// construction-independent complement to isTaggedUnionType, which requires a
// payload-carrying VariantConstruct in the unit: a tagged union referenced
// ONLY by a bare `sizeof` (sizeofCTypeName) is never constructed, so only this
// declaration-level test recognizes it and routes it to the union typedef pair.
func isUnionEnumType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) bool {
	if !isEnumType(unit, snapshot, id) {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return false
	}
	for _, memberType := range typeDecl.MemberTypes {
		// A zero member type is an unresolved template (a member whose type
		// wraps the type's own parameter), not a payload-carrying declaration;
		// a non-zero non-void member type is a declared payload variant.
		if memberType != 0 && !isVoid(snapshot, memberType) {
			return true
		}
	}
	return false
}

// unionVariantPayloadMember reports whether (ownerType, member) is the payload
// member of a tagged-union variant in this program: ownerType is a tagged-union
// type, member is one of its declared variants, and a payload-carrying
// construction of that exact variant exists in the unit — the condition under
// which the union's typedef declares the C union member
// pebble_field_<member> under its .payload union (see resolveUnionInfo /
// buildUnionTypedef). This is the read-side test for a narrowed union-variant
// payload access (`self.Ok` inside `case .Ok:`, Slice A): a variant never
// constructed anywhere has no union member, so reading it would emit a
// projection no C union member satisfies — a clean rejection, never guessed C.
func unionVariantPayloadMember(unit *tir.Unit, snapshot *types.Snapshot, ownerType types.TypeID, member symbol.SymbolID) bool {
	if !isTaggedUnionType(unit, snapshot, ownerType) {
		return false
	}
	key, ok := snapshot.Key(ownerType)
	if !ok {
		return false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return false
	}
	declared := false
	for _, m := range typeDecl.Members {
		if m == member {
			declared = true
			break
		}
	}
	if !declared {
		return false
	}
	for _, node := range unit.Nodes() {
		if node.Kind == tir.VariantConstruct && node.Type == ownerType && node.Member == member && len(node.Children) >= 1 {
			return true
		}
	}
	for _, node := range unit.Nodes() {
		if node.Kind != tir.FieldPlace || node.Member != member || len(node.Children) != 1 {
			continue
		}
		base, found := unit.Node(node.Children[0])
		baseType := base.Type
		if pointee, pointer := pointerPointeeType(snapshot, baseType); pointer {
			baseType = pointee
		}
		if found && baseType == ownerType {
			return true
		}
	}
	return false
}

func arrayElementCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isStr(snapshot, id) {
		return "PebbleStr", nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isChar(snapshot, id) {
		return "int32_t", nil
	}
	if isFloat(snapshot, id) {
		return floatCType(resolvedFloatKind(snapshot, id)), nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		if isEnumType(unit, snapshot, id) {
			return "", fmt.Errorf("array element type %s is an enum type; enum-typed array elements are not supported yet", enumTypeName(id))
		}
		return structTypeName(id), nil
	}
	// A scalar integer element resolves to its OWN width, not the ambient
	// entry width: a [3]u8 array inside an i32 function must declare its C
	// storage as uint8_t[3] (and a slice over it as a uint8_t* data pointer),
	// mirroring sliceElementCType's own element-own-width resolution.
	if elementWidth, integerElement := resolvedBuiltin(snapshot, id); integerElement && cType(elementWidth) != "" {
		return cType(elementWidth), nil
	}
	return "", fmt.Errorf("array element type %s is not supported", describeType(snapshot, id))
}

// sliceElementCType resolves the C pointer target type for a slice's data
// field: the element's C type. Any fixed-width integer builtin (resolved to its
// own width by resolvedBuiltin/cType), char (the fixed int32_t), bool, float
// (f32/f64, each the plain C float/double), tuple
// (the tuple's own typedef), optional (the optional's own typedef), struct
// (the struct's own typedef), and plain enum (the enum's own typedef
// pebble_enum_<typeID>_t — the same C type an enum-typed local/field is
// declared with) are supported slice element types, matching
// isSupportedSliceElementType and arrayElementCType's own element handling. A
// tagged union is enum-shaped (isDefinitelyEnumType reports true for it) but
// its C representation is the tag-plus-payload struct typedef pair, not a bare
// C enum — pointing a slice's data field at the discriminant enum would store
// tags where values belong — so isUnionEnumType (the declaration-level test)
// keeps it a clean rejection naming what was found. Any other element type is
// a clean rejection naming what was found.
func sliceElementCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isChar(snapshot, id) {
		return "int32_t", nil
	}
	// A float element (f32/f64) resolves its .data pointer target to the plain
	// C float/double — no typedef needed, exactly like a bool/char element and
	// mirroring arrayElementCType's float case (floatCType used for helper
	// parameters/results, task #22).
	if isFloat(snapshot, id) {
		return floatCType(resolvedFloatKind(snapshot, id)), nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		if isUnionEnumType(unit, snapshot, id) {
			return "", fmt.Errorf("slice element type %s is a tagged-union (union enum) type; tagged-union slice elements are not supported yet", describeType(snapshot, id))
		}
		if isDefinitelyEnumType(unit, snapshot, id) {
			return enumTypeName(id), nil
		}
		return structTypeName(id), nil
	}
	if elementWidth, integerElement := resolvedBuiltin(snapshot, id); integerElement && cType(elementWidth) != "" {
		return cType(elementWidth), nil
	}
	return "", fmt.Errorf("slice element type %s is not supported", describeType(snapshot, id))
}

// unionMemberType returns the resolved payload type of one of a tagged-union
// type's constructed members, by member symbol. The members list carries the
// payload types resolved by collectUnionTypes from construction sites, so a
// construction of a variant that was never collected as a union member reports
// false.
func unionMemberType(members []unionMemberInfo, member symbol.SymbolID) (types.TypeID, bool) {
	for _, m := range members {
		if m.member == member {
			return m.payloadType, true
		}
	}
	return 0, false
}

// declaredFieldType resolves one field's own type from a struct type's
// declared fields, matching the field's member symbol against the struct's
// TypeDecl.Members list (the declared field order). For a generic
// instantiation the member's recorded type is one of the declaration's own
// type parameters, so the concrete field type is substituted per instantiation
// exactly as resolveStructInfo does: the member's MemberTypes entry is the
// parameter's TypeID and the struct type's Nominal arguments are the concrete
// type arguments, so Pair[K, V]'s `value V` is Bool for Pair[int, bool] while
// staying Int for Pair[int, int] in the same program. The two instantiations
// share the same field symbol, which is why the field type is resolved from
// the instantiation's own arguments rather than any per-symbol evidence —
// per-symbol node-graph evidence would let the first instantiation's Int win
// over the second's Bool. When the member's MemberTypes entry is unresolved
// (a member whose template wraps a parameter) the type is instead recovered
// from the unit's own node graph, scoped to THIS instantiation: any
// RecordConstruct whose type is exactly structType and whose Fields contain
// the member (the value node's Type is the field's resolved type), or any
// FieldPlace whose base resolves to structType and whose Member is the member
// (its Type is the field's resolved type). The RecordConstruct/FieldPlace
// scoping is what keeps two specializations of one generic struct from
// borrowing each other's field type. A member that is not in the struct's
// declared member list, or whose type cannot be resolved from the unit,
// reports false.
func declaredFieldType(unit *tir.Unit, snapshot *types.Snapshot, structType types.TypeID, member symbol.SymbolID) (types.TypeID, bool) {
	key, ok := snapshot.Key(structType)
	if !ok {
		return 0, false
	}
	decl, arguments, ok := key.Nominal()
	if !ok {
		return 0, false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return 0, false
	}
	for index, declared := range typeDecl.Members {
		if declared != member {
			continue
		}
		fieldType := types.TypeID(0)
		if index < len(typeDecl.MemberTypes) {
			fieldType = typeDecl.MemberTypes[index]
		}
		if fieldType != 0 {
			substitutions := structSubstitutions(unit, snapshot, decl, arguments)
			if substitutions != nil {
				if substituted, err := snapshot.Substitute(fieldType, substitutions); err == nil {
					return substituted, true
				}
			}
			return fieldType, true
		}
		break
	}
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FieldPlace && node.Member == member && node.Type != 0 {
			if structOf, ok := fieldPlaceStructType(unit, node); ok && structOf == structType {
				return node.Type, true
			}
		}
		if node.Kind == tir.RecordConstruct && node.Symbol == decl && node.Type == structType {
			for _, field := range node.Fields {
				if field.Field == member {
					if value, ok := unit.Node(field.Value); ok && value.Type != 0 {
						return value.Type, true
					}
				}
			}
		}
	}
	return 0, false
}

// fieldPlaceStructType resolves the struct type a FieldPlace reads from by
// walking the place's base chain to the underlying StoragePlace or
// DereferencePlace (whose Type is the struct type). Two generic
// specializations of one struct share every field symbol, so FieldPlace
// evidence must be scoped to the struct type the read actually projects —
// matching the RecordConstruct recovery's node.Type == structType scoping —
// or the first matching FieldPlace in the unit (from another specialization)
// would supply the wrong field type.
func fieldPlaceStructType(unit *tir.Unit, node tir.Node) (types.TypeID, bool) {
	if len(node.Children) != 1 {
		return 0, false
	}
	base, ok := unit.Node(node.Children[0])
	if !ok {
		return 0, false
	}
	switch base.Kind {
	case tir.StoragePlace:
		return base.Type, true
	case tir.DereferencePlace:
		// A DereferencePlace's Type is already the pointee type, which for a
		// struct-field projection is the struct type being read.
		return base.Type, true
	case tir.FieldPlace:
		return fieldPlaceStructType(unit, base)
	}
	return 0, false
}

func resolvedFloatKind(snapshot *types.Snapshot, id types.TypeID) types.BuiltinKind {
	key, ok := snapshot.Key(id)
	if !ok {
		return 0
	}
	kind, ok := key.Builtin()
	if !ok || (kind != types.F32 && kind != types.F64) {
		return 0
	}
	return kind
}

// isWidth reports whether id is the snapshot's builtin for the entry's
// resolved integer width (types.Int, types.I32, or types.I64). The checked helpers this
// backend emits operate on exactly one width per entry, so every node in an
// accepted expression tree must carry exactly this type — a node carrying the
// other width is a clean rejection, never a coercion, since there is no
// cast/coercion lowering yet.
func isWidth(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return false
	}
	builtin, ok := key.Builtin()
	return ok && builtin == width && cType(width) != ""
}

// isAbstractInt reports whether id is the snapshot's abstract `int` builtin
// (types.Int) — the unanchored keyword type the checker leaves a value at when
// nothing pins it to a fixed width. It is a DISTINCT builtin from types.I32
// (each is its own BuiltinKind), but it shares i32's C representation
// (int32_t), so an `int`-typed value is emitted at whatever integer width the
// surrounding position requests without a cast. The width gate in buildExpr
// admits such a node at any integer width (see there); isWidth itself stays
// exact-match so every other call site keeps its existing meaning.
func isAbstractInt(snapshot *types.Snapshot, id types.TypeID) bool {
	return snapshot != nil && id == snapshot.Builtins().Int
}

// isTypeParameterType reports whether id is a type-parameter TypeID — the
// shape a generic struct method's own parameter/return types and body-node
// types carry when the method references the containing struct's type
// parameter directly (`fn get(self Box[T]) T`). Such a node's type is a
// placeholder the backend resolves structurally: a Load(FieldPlace) resolves
// its field's concrete type through the substituted self parameter's scope
// entry (declaredFieldType), and a SymbolValue names a local or parameter that
// helperSignature seeded with the substituted concrete type, so the emitted C
// is always concrete even though the TIR node type is symbolic. This predicate
// lets the value grammars admit such nodes at their width gates; a
// type-parameter node whose structural dispatch cannot resolve a concrete type
// is still a clean rejection at the dispatch site.
func isTypeParameterType(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.TypeParameter
}

func isUint(snapshot *types.Snapshot, id types.TypeID) bool {
	return snapshot != nil && id == snapshot.Builtins().Uint
}

// isU64 reports whether id is the snapshot's u64 builtin. It is the u64 twin
// of isUint: u64 is a DISTINCT builtin from uint in this compiler
// (snapshot.Builtins().U64 vs snapshot.Builtins().Uint), so it needs its own
// predicate rather than being folded into isUint, just as isUint is separate
// from isWidth. Unlike uint (the platform-native pointer-width type that the
// backend builds through buildUintExpr for its sizeof/slice-bounds/checked
// arithmetic), u64 is an ordinary fixed-width integer that flows through the
// general buildExpr path at its own resolved width.
func isU64(snapshot *types.Snapshot, id types.TypeID) bool {
	return snapshot != nil && id == snapshot.Builtins().U64
}

// isFixedWidthInteger reports whether id is any concrete fixed-width integer
// builtin — i8, i16, i32, i64, u8, u16, u32, or u64 — the builtins with a
// fixed-width C representation (cType). It is deliberately the complement of
// the abstract builtins: the unanchored `int` (types.Int) and the word-sized
// `uint` (types.Uint), each of which keeps its own dedicated handling
// (isAbstractInt / isUint and the buildUintExpr path), are excluded even
// though both have a cType. This is the parameter-gate twin of the
// resolvedBuiltin/cType subject widening the switch-subject fix (2b3d684)
// applied in buildSwitchStatement: any fixed-width integer is admitted at its
// OWN resolved width, not just the entry's, and a caller that wants the
// entry-width/compatible-width/uint/u64 subtleties must check those narrower
// predicates separately (helperSignature does, in this order). The entry's
// own width, uint, and u64 are all also covered by this predicate, so callers
// must not rely on it to exclude them.
func isFixedWidthInteger(snapshot *types.Snapshot, id types.TypeID) bool {
	builtin, ok := resolvedBuiltin(snapshot, id)
	return ok && cType(builtin) != "" && !isUint(snapshot, id) && !isAbstractInt(snapshot, id)
}

// isBool reports whether id is the snapshot's bool builtin. It is the bool
// twin of isWidth: every node in an accepted bool expression tree must carry
// exactly the bool builtin, since this backend has no cast/coercion lowering
// between bool and anything else.
func isBool(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Bool
}

// isStr reports whether id is the snapshot's str builtin. A str value is a
// builtin like bool, but unlike bool (or the entry's integer width) it has no
// arithmetic grammar this backend builds — a str local is initialized from a
// string literal (or, since 10.36, a call to a str-returning helper), may be
// reassigned from a string literal, and a str value is an operand of a ==/!=
// comparison, a call-site argument, or a str-returning function's return value
// — so it is recognized by this distinct predicate rather than by a shared
// scalar-builder switch.
func isStr(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Str
}

// isChar reports whether id is the snapshot's char builtin. A char value is a
// builtin like bool, but like str it has no width grammar this backend builds —
// its C representation is the fixed int32_t (the language's char is a full
// Unicode scalar value, which always fits in 32 bits, regardless of the entry's
// resolved integer width), so a char local is initialized from a char literal,
// a char-typed local reference, or a call to a char-returning helper, may be
// reassigned the same ways, and a char value is an operand of any of the six
// comparisons, a call-site argument, or a char-returning function's return
// value — recognized by this distinct predicate rather than by a shared
// scalar-builder switch.
func isChar(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Char
}

// isFloat reports whether id is one of the snapshot's two float builtins
// (f32 or f64). It is the float cousin of isBool: every node in an accepted
// float expression tree must carry exactly one of the two float builtins, and
// — unlike bool, which has just one type — which float builtin a node carries
// must also match the specific float kind the surrounding position wants, so
// buildFloatExpr additionally checks the resolved kind against its width
// argument rather than accepting either float builtin interchangeably.
func isFloat(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().F32 || id == snapshot.Builtins().F64
}

// isVoid reports whether id is the snapshot's void builtin. A void result is
// the third accepted result kind for a reachable helper (alongside the entry's
// width and a tuple/struct type), recognized so validateHelperSignature can
// admit a void-returning callee and buildHelperFunctions can declare it with
// the C return type "void"; a void-returning call is then built only in the
// bare discarded-expression statement position (buildExpressionStatement),
// never as a value.
func isVoid(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Void
}

// isTuple reports whether id resolves to a tuple type in the snapshot. It is
// how the emitter recognizes a tuple-typed local's declaration without
// consulting the builtin table: a tuple is not a types.BuiltinKind, so
// resolvedBuiltin returns no kind for it and the caller must ask whether the
// type is a tuple instead.
func isTuple(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return false
	}
	return key.Kind() == types.Tuple
}

// isArray reports whether id resolves to a fixed-length array type.
func isArray(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Array
}

// isSlice reports whether id resolves to a slice type in the snapshot.
func isSlice(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Slice
}

// isOptional reports whether id resolves to an optional type in the snapshot.
func isOptional(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Optional
}

// isStruct reports whether id resolves to a struct type in the snapshot. It is
// how the emitter recognizes a struct-typed local's declaration without
// consulting the builtin table: a struct is a Nominal type, not a
// types.BuiltinKind, so resolvedBuiltin returns no kind for it and the caller
// must ask whether the type is a struct instead. A generic struct's
// monomorphized instance is also Nominal (its Nominal arguments are the
// concrete type arguments), so it is recognized the same way; this backend
// never inspects the argument list, so a generic instance is emitted exactly
// like a non-generic struct of the same shape.
func isStruct(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Nominal
}

// isPointer reports whether id resolves to a pointer type in the snapshot. A
// pointer-typed local is declared with the pointee's own C type followed by
// ` *`, and its initializer is most commonly an AddressOf expression. The
// pointer type is recognized by this distinct predicate rather than by a
// shared scalar-builder switch, since a pointer is not a types.BuiltinKind.
func isPointer(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Pointer
}

// isFunctionType reports whether id resolves to a function type in the
// snapshot. A function-typed local is declared with the function type's own
// pointer typedef (pebble_fnptr_<typeID>_t) and its value is a function
// pointer; the function type is recognized by this distinct predicate rather
// than by a shared scalar-builder switch, since a function type is not a
// types.BuiltinKind (mirroring how isTuple / isSlice / isOptional recognize
// their own kinds).
func isFunctionType(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Function
}

// pointerPointeeType returns the pointee type of a pointer type. It is the
// single way to extract the child of a pointer type, mirroring how
// key.Child() works for Slice/Optional but restricted to Pointer kinds for
// clarity at call sites.
func pointerPointeeType(snapshot *types.Snapshot, pointerType types.TypeID) (types.TypeID, bool) {
	key, ok := snapshot.Key(pointerType)
	if !ok {
		return 0, false
	}
	if key.Kind() != types.Pointer {
		return 0, false
	}
	return key.Child()
}

// opaqueExternTypeName reports whether id is a Nominal type declared by an
// `extern { type Name; }` with no body — an OPAQUE extern type, meaning "this
// exists in C, I'm not describing its layout" — and, if so, returns its real
// authored C name (FILE, DIR, ...). The mapping is resolved the same way
// externCName resolves a function's real C name: a Nominal type's key carries
// its declaring symbol.SymbolID (see types.NominalKey), and the symbol table
// threaded into Emit (st.symbols) classifies that declaration as
// SymbolExternType and holds the exact identifier written after `type` in the
// source. An ordinary Pebble struct/enum/union is a SymbolType symbol, never
// SymbolExternType, so the kind check is what distinguishes an opaque extern
// type from a real struct (both are Nominal in the type snapshot). A nil or
// missing symbol table yields ok=false: without the table the real name cannot
// be known, and the caller must not guess a pebble_struct_<id>_t name for a
// type whose layout it does not describe.
func opaqueExternTypeName(st *emitState, snapshot *types.Snapshot, id types.TypeID) (string, bool) {
	if snapshot == nil || st.symbols == nil || st.symbols.Symbols == nil {
		return "", false
	}
	key, ok := snapshot.Key(id)
	if !ok || key.Kind() != types.Nominal {
		return "", false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return "", false
	}
	s, ok := st.symbols.Symbols.Symbol(decl)
	if !ok || s.Kind != symbol.SymbolExternType {
		return "", false
	}
	return s.Name, true
}

// isOpaqueExternType reports whether id is an opaque extern type (a
// `type Name;` inside an extern block, no body). It is the bool-only form of
// opaqueExternTypeName, used by struct-type collection to exclude such a type
// from the synthesized struct-typedef machinery: an opaque extern type has no
// layout of its own to emit — its real C name (FILE, DIR) is supplied by the
// libc header the preamble already includes — so collecting it as a struct
// would both emit a bogus empty typedef and break resolveStructInfo's
// field-resolution assumptions.
func isOpaqueExternType(st *emitState, snapshot *types.Snapshot, id types.TypeID) bool {
	_, ok := opaqueExternTypeName(st, snapshot, id)
	return ok
}

// pointerTypeName returns the full C type name for a pointer to the given
// pointee type: `int32_t *` for *i32, `bool *` for *bool, `pebble_struct_<id>_t *`
// for *Point, `pebble_tuple_<id>_t *` for a tuple pointer, `FILE *` for a
// pointer to an opaque extern type (its real C name, declared by the <stdio.h>
// preamble hasCExterns includes), etc. The pointee
// type must be a valid type in the snapshot. Returns "" for any unsupported
// pointee kind (defense for hand-built IR).
func pointerTypeName(st *emitState, snapshot *types.Snapshot, pointee types.TypeID) string {
	return pointerTypeNameForUnit(st, nil, snapshot, pointee)
}

func pointerTypeNameForUnit(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, pointee types.TypeID) string {
	if snapshot == nil {
		return ""
	}
	if builtin, ok := snapshot.Key(pointee); ok {
		if bk, ok := builtin.Builtin(); ok {
			// cType only maps the fixed-width integer kinds (Int/I32/I64) —
			// it's meant for width-typed locals, not every possible pointee.
			// void/bool/char are real, common pointee kinds (*void is
			// pervasive in std/libc.peb and std/mem.peb) with their own C
			// spellings that don't go through cType's narrower convention.
			switch bk {
			case types.Void:
				return "void *"
			case types.Bool:
				return "bool *"
			case types.Char:
				// Matches the existing convention: a char value/local is
				// always declared as int32_t in emitted C (see the
				// char-typed-parameter case in buildHelperFunctions).
				return "int32_t *"
			case types.F32, types.F64:
				// A float pointee (*f32, *f64) is declared with its plain C
				// float/double pointer type, the pointer form of the same
				// floatCType a float value/local uses — so `let p *f64 = &x;`
				// on a float-typed local (or `slice &arr[0], N` over a
				// [N]f64 array, the float-element SliceFromRaw shape) declares
				// the pointer at the element's own float width.
				return floatCType(bk) + " *"
			}
			if ctype := cType(bk); ctype != "" {
				return ctype + " *"
			}
			return ""
		}
	}
	if isStr(snapshot, pointee) {
		return "PebbleStr *"
	}
	if isTuple(snapshot, pointee) {
		return tupleTypeName(pointee) + " *"
	}
	if isOptional(snapshot, pointee) {
		// An optional pointee (a pointer to an optional value — `&o` on an
		// optional-typed local/parameter/field, so `let p *?i32 = &o;`) is
		// declared with the optional's own pebble_optional_<typeID>_t struct
		// typedef followed by ` *`, the same C type an optional-typed
		// local/parameter/field is declared with — an optional is always the
		// typedef'd struct in this backend, never a bare C form, so the
		// address-of, dereference, nil, and equality round-trips are
		// pointer-identical with the struct-pointee convention above.
		return optionalTypeName(pointee) + " *"
	}
	if isArray(snapshot, pointee) {
		// An array pointee (a pointer to a whole fixed array — `&a` on an
		// array-typed local/field, so `let p *[3]i32 = &a;`) is declared with
		// the array's own pebble_array_<typeID>_t struct typedef followed by
		// ` *`, the SAME C type an array-typed parameter/result/field/global is
		// declared with (arrayTypeName) — a pointer to the wrapped array
		// struct, so the pointer type-name is valid in every C context that
		// names it (a local declaration, a helper parameter, a helper return
		// type, and a cast). A standalone array LOCAL is a raw C array
		// (`int32_t pebble_local_<s>[3]`), so the address-of expression that
		// produces this pointer value is cast at the emission site
		// (`(pebble_array_<typeID>_t *)&pebble_local_<s>`), and dereferencing
		// the pointer yields the wrapped struct whose `.data` member is the
		// array — the whole-array read/write/index positions project `.data`
		// the same way buildPlaceLValue projects an array field.
		return arrayTypeName(pointee) + " *"
	}
	// An opaque extern type (type FILE;) is Nominal like a struct, so the
	// extern-type case must come BEFORE the isStruct fall-through: the real C
	// type name (FILE, from the already-included header) replaces the
	// synthesized pebble_struct_<id>_t a struct-typed pointee would get, and
	// the C that results actually agrees with the libc declaration of every
	// function that takes or returns such a pointer.
	if name, ok := opaqueExternTypeName(st, snapshot, pointee); ok {
		return name + " *"
	}
	if isUnionEnumType(unit, snapshot, pointee) {
		return unionTypeName(pointee) + " *"
	}
	// A plain enum pointee (a pointer to an enum value — `&colors[0]` on an
	// enum-element slice, or the address of an enum-typed field) is declared
	// with its own pebble_enum_<typeID>_t enum typedef, the same C type an
	// enum-typed local/field uses; without this case the isStruct fall-through
	// below (an enum is Nominal like a struct) would name a nonexistent
	// pebble_struct_<typeID>_t. isDefinitelyEnumType — the positive-evidence
	// form — is used rather than isEnumType so a method-only struct pointee
	// (no FieldDeclaration nodes; isEnumType's no-evidence fallback would
	// misreport it as an enum) keeps its struct typedef name. The
	// isUnionEnumType check above must stay first: a tagged union is
	// enum-shaped, but its pointer names the union's own pebble_union_<typeID>_t
	// typedef.
	if isDefinitelyEnumType(unit, snapshot, pointee) {
		return enumTypeName(pointee) + " *"
	}
	if isStruct(snapshot, pointee) {
		return structTypeName(pointee) + " *"
	}
	if isSlice(snapshot, pointee) {
		return sliceTypeName(pointee) + " *"
	}
	return ""
}

// arrayLengthLiteral validates that the compile-time length can be passed to
// the width-specific checked-index helper without a narrowing conversion.
func arrayLengthLiteral(length uint64, width types.BuiltinKind) (string, error) {
	max := uint64(^uint32(0) >> 1)
	if width == types.I64 {
		max = uint64(^uint64(0) >> 1)
	}
	if length > max {
		return "", fmt.Errorf("array length %d does not fit the %s checked-index helper", length, wantName(width))
	}
	return fmt.Sprintf("%d", length), nil
}

// tupleTypeName is the deterministic C name of one distinct tuple type's
// struct typedef: pebble_tuple_<typeID>_t, derived from the tuple type's own
// stable types.TypeID (stable within one Emit call), mirroring the
// pebble_fn_<symbolID> / pebble_local_<symbolID> naming discipline of reusing
// a stable IR identity rather than a counter.
func tupleTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_tuple_%d_t", id)
}

func arrayTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_array_%d_t", id)
}

// optionalTypeName is the deterministic C name of one distinct optional type's
// struct typedef: pebble_optional_<typeID>_t, derived from the optional
// type's own stable types.TypeID, mirroring the tuple naming discipline.
func optionalTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_optional_%d_t", id)
}

// structTypeName is the deterministic C name of one distinct struct type's
// struct typedef: pebble_struct_<typeID>_t, derived from the struct type's own
// stable types.TypeID, mirroring the tuple naming discipline.
func structTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_struct_%d_t", id)
}

// functionTypeName is the deterministic C name of one distinct function type's
// pointer typedef: pebble_fnptr_<typeID>_t, derived from the function type's
// own stable types.TypeID, following the same TypeID-based naming discipline
// as pebble_slice_<typeID>_t / pebble_optional_<typeID>_t /
// pebble_struct_<typeID>_t (NOT v1's ad-hoc canonical-name-string scheme).
func functionTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_fnptr_%d_t", id)
}

func runtimeType(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) symbol.RuntimeType {
	if unit == nil || snapshot == nil {
		return 0
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return 0
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return 0
	}
	info := unit.Runtime()
	switch decl {
	case info.Allocator:
		if decl != 0 {
			return symbol.RuntimeAllocator
		}
	case info.Context:
		if decl != 0 {
			return symbol.RuntimeContext
		}
	}
	return 0
}

func runtimeTypeName(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) string {
	switch runtimeType(unit, snapshot, id) {
	case symbol.RuntimeAllocator:
		return "PebbleAllocator"
	case symbol.RuntimeContext:
		return "PebbleContext"
	default:
		return structTypeName(id)
	}
}

// sliceTypeName is the deterministic C name of one distinct slice type's
// struct typedef: pebble_slice_<typeID>_t, derived from the slice type's own
// stable types.TypeID, mirroring the tuple/struct/optional naming discipline.
func sliceTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_slice_%d_t", id)
}

// enumTypeName is the deterministic C name of one distinct plain enum type's
// enum typedef: pebble_enum_<typeID>_t, derived from the enum type's own
// stable types.TypeID, mirroring the pebble_struct_<typeID>_t / pebble_tuple_
// <typeID>_t naming discipline of reusing a stable IR identity rather than a
// counter.
func enumTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_enum_%d_t", id)
}

// sizeofCTypeName resolves one type to the C type name a `sizeof T` lowers to
// — the type's own C storage type: a fixed-width integer at its own cType
// (int32_t for int/i32, int64_t for i64, uint64_t for uint/u64, and so on),
// bool as bool, char as the fixed int32_t, str as PebbleStr, a tuple/optional/
// struct/slice/enum/array as its own typedef name (tupleTypeName/
// optionalTypeName/structTypeName/sliceTypeName/enumTypeName/arrayTypeName),
// and a runtime type as its hand-written C type (runtimeTypeName). This is the
// general form of the SizeofType case's original builtin-only three-width
// dispatch; it exists so `sizeof` of an aggregate type (std/hmap.peb's
// `sizeof Entry[K, V]`, where the TypeArg is the Entry struct type) sizes the
// storage by the aggregate's OWN typedef, never the fallback sizeof(uint64_t)
// the builtin-only dispatch would produce. Anything without a C type this
// backend emits is a clean rejection naming the type.
func sizeofCTypeName(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID) (string, error) {
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isChar(snapshot, id) {
		return "int32_t", nil
	}
	if isStr(snapshot, id) {
		return "PebbleStr", nil
	}
	if width, integer := resolvedBuiltin(snapshot, id); integer && cType(width) != "" {
		return cType(width), nil
	}
	if runtimeType(unit, snapshot, id) != 0 {
		return runtimeTypeName(unit, snapshot, id), nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	if isSlice(snapshot, id) {
		return sliceTypeName(id), nil
	}
	if isArray(snapshot, id) {
		return arrayTypeName(id), nil
	}
	// A tagged union is enum-shaped (isEnumType reports true for it), but its
	// real C representation is the tag-plus-payload struct its typedef pair
	// emits, not a bare C enum — so `sizeof` on a tagged union must size the
	// union's own typedef (pebble_union_<typeID>_t), the same C type a
	// tagged-union local, parameter, and result are declared with (see
	// unionTypeName), and the same distinction structFieldCType /
	// optionalPayloadCType draw. This must run before the isEnumType check,
	// which would otherwise misclassify the tagged union as a plain enum and
	// size the bare tag enum typedef (too small) or, in a bare-sizeof program,
	// name a typedef that was never emitted. isUnionEnumType is the
	// declaration-level test (union enum with a declared payload variant),
	// independent of whether any construction exists — a tagged union reached
	// only by `sizeof` is never constructed, so the construction-based
	// isTaggedUnionType could not recognize it.
	if isUnionEnumType(unit, snapshot, id) {
		return unionTypeName(id), nil
	}
	if isEnumType(unit, snapshot, id) {
		return enumTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		return structTypeName(id), nil
	}
	if isPointer(snapshot, id) {
		if pointee, ok := pointerPointeeType(snapshot, id); ok {
			if name := pointerTypeNameForUnit(st, unit, snapshot, pointee); name != "" {
				return name, nil
			}
		}
	}
	return "", fmt.Errorf("sizeof of type %s is not supported, want a fixed-width integer, bool, char, str, tuple, optional, slice, array, enum, struct, or pointer", describeType(snapshot, id))
}

// unionTypeName is the deterministic C name of one distinct tagged-union type's
// struct typedef: pebble_union_<typeID>_t, derived from the union type's own
// stable types.TypeID, mirroring the pebble_struct_<typeID>_t / pebble_enum_
// <typeID>_t naming discipline of reusing a stable IR identity rather than a
// counter. The discriminant enum typedef the struct's tag field uses is
// pebble_enum_<typeID>_t (see enumTypeName) — the two names share the type ID
// suffix and never collide, since one spells "enum" and the other "union".
func unionTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_union_%d_t", id)
}

// enumVariantName is the deterministic C name of one plain enum variant's
// enum constant: pebble_variant_<memberSymbolID>, derived from the variant's
// own stable symbol.SymbolID (mirroring the pebble_field_<memberSymbolID>
// naming discipline struct fields use, and the pebble_local_<symbolID> /
// pebble_fn_<symbolID> discipline everywhere else), so a C constant name can
// never collide with another identifier even if a source variant name were a C
// keyword.
func enumVariantName(member symbol.SymbolID) string {
	return fmt.Sprintf("pebble_variant_%d", member)
}

// tupleElementCType is the C field type a tuple element of the given type is
// declared with in its tuple's struct typedef: any fixed-width integer builtin
// (the entry's resolved width, uint, u64, or any other fixed-width integer,
// each resolved to its OWN width by the generic resolvedBuiltin/cType pattern
// — the same generalization structFieldCType and arrayElementCType make),
// bool as bool, char as the fixed int32_t, str as PebbleStr, f32 as float, f64
// as double, and a tuple/optional/struct element as its own typedef name (the
// same element resolution arrayElementCType makes for a nested aggregate
// element). Any other element type is a clean rejection naming what was found.
func tupleElementCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isStr(snapshot, id) {
		return "PebbleStr", nil
	}
	if isChar(snapshot, id) {
		return "int32_t", nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isFloat(snapshot, id) {
		return floatCType(resolvedFloatKind(snapshot, id)), nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		if isEnumType(unit, snapshot, id) {
			return "", fmt.Errorf("element type %s is an enum type; enum-typed tuple elements are not supported yet", enumTypeName(id))
		}
		return structTypeName(id), nil
	}
	if elementWidth, integerElement := resolvedBuiltin(snapshot, id); integerElement && cType(elementWidth) != "" {
		return cType(elementWidth), nil
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("element type %s is not supported, want %s, bool, char, str, f32, f64, or a tuple/struct type", name, wantName(width))
		}
	}
	return "", fmt.Errorf("element type %s is not supported, want %s, bool, char, str, f32, f64, or a tuple/struct type", describeType(snapshot, id), wantName(width))
}

// structFieldCType is the C field type a struct field of the given type is
// declared with in its struct's typedef: any fixed-width integer builtin (the
// entry's resolved width, uint, u64, or any other fixed-width integer, each
// resolved to its OWN width by the generic resolvedBuiltin/cType pattern — so
// a uint or u64 field is uint64_t), bool for a bool field, PebbleStr for a
// str field (the same C type a str local, parameter, result, and union member
// is declared with), the field's own tuple/optional/struct/pointer/slice/
// array/function-type typedef, a plain enum field's own enum typedef
// (pebble_enum_<typeID>_t, the same C type an enum-typed local/parameter/
// result is declared with), or a compiler-builtin runtime type's hand-written
// C type. A fixed-array field is declared with the array's OWN typedef
// (pebble_array_<typeID>_t, the same C type a sizeof array type names — see
// arrayTypeName), mirroring how a slice field is declared with its
// pebble_slice_<typeID>_t typedef; the array's element type is validated by
// buildArrayTypedefs separately from this resolver, exactly like a slice
// field's element type. Any other field type — a char field — is a clean
// rejection naming what was found, since this backend emits exactly those C
// types as struct fields.
func structFieldCType(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if fieldWidth, integerField := resolvedBuiltin(snapshot, id); integerField && cType(fieldWidth) != "" {
		return cType(fieldWidth), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	// A float field (f32/f64) is declared with its plain C float/double type —
	// no typedef needed, exactly like a bool/str field — mirroring how
	// floatCType is already used for helper parameters/results (task #22).
	if isFloat(snapshot, id) {
		return floatCType(resolvedFloatKind(snapshot, id)), nil
	}
	if isStr(snapshot, id) {
		return "PebbleStr", nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isOptional(snapshot, id) {
		return optionalTypeName(id), nil
	}
	// A field of a compiler-builtin runtime type (Allocator, Context) is
	// declared with its hand-written C type (PebbleAllocator / PebbleContext,
	// defined in pebble_rt.h, never emitted as a pebble_struct_<id>_t typedef —
	// orderAggregateTypes skips runtime types for exactly this reason), so it
	// must be resolved before the isStruct case, which would otherwise name a
	// nonexistent pebble_struct_<id>_t. Mirrors how a runtime-typed local,
	// parameter, and result are declared via runtimeTypeName.
	if runtimeType(unit, snapshot, id) != 0 {
		return runtimeTypeName(unit, snapshot, id), nil
	}
	if isStruct(snapshot, id) {
		// A tagged union and a plain enum are both enum-shaped (isEnumType
		// reports true for each), but only a plain enum's C representation is a
		// bare C enum typedef — a tagged union's real representation is the
		// tag-plus-payload struct its buildUnionTypedef pair emits, so a field
		// whose type is a tagged union is declared with the union's own typedef
		// name (pebble_union_<typeID>_t), the same C type a tagged-union local,
		// parameter, and result are declared with (see unionTypeName), and the
		// same distinction isTaggedUnionType draws everywhere else in this file.
		if isTaggedUnionType(unit, snapshot, id) {
			return unionTypeName(id), nil
		}
		if isEnumType(unit, snapshot, id) {
			return enumTypeName(id), nil
		}
		return structTypeName(id), nil
	}
	if isPointer(snapshot, id) {
		pointee, ok := pointerPointeeType(snapshot, id)
		if !ok {
			return "", fmt.Errorf("field type %s has no pointer pointee", describeType(snapshot, id))
		}
		if name := pointerTypeNameForUnit(st, unit, snapshot, pointee); name != "" {
			return name, nil
		}
	}
	if isSlice(snapshot, id) {
		return sliceTypeName(id), nil
	}
	if isArray(snapshot, id) {
		return arrayTypeName(id), nil
	}
	if isFunctionType(snapshot, id) {
		// A function-typed field (`op fn(int, int) int;`, function-types
		// slice 2): declared with the function type's own pointer typedef,
		// pebble_fnptr_<typeID>_t (see functionTypeName / buildFunctionTypedef,
		// slice 1) — the same C type a function-typed local or value uses, so
		// storing a function value in the field is trivially valid C. The
		// field's signature is validated by validateFunctionTypeSignature
		// wherever the field's construction/read value is built
		// (buildFunctionValue), mirroring how a slice field's element type is
		// validated separately from this resolver.
		if err := validateFunctionTypeSignature(snapshot, width, id); err != nil {
			return "", fmt.Errorf("field type %s: %v", describeType(snapshot, id), err)
		}
		return functionTypeName(id), nil
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("field type %s is not supported, want a fixed-width integer, bool, str, tuple, struct, enum, pointer, slice, function type, or runtime type", name)
		}
	}
	return "", fmt.Errorf("field type %s is not supported, want a fixed-width integer, bool, str, tuple, struct, enum, pointer, slice, function type, or runtime type", describeType(snapshot, id))
}

// optionalPayloadCType is the C field type an optional payload of the given
// type is declared with in its optional's struct typedef: any fixed-width
// integer builtin (the entry's resolved width, uint, u64, or any other
// fixed-width integer, each resolved to its OWN width by the generic
// resolvedBuiltin/cType pattern — so a uint or u64 payload's .value field is
// uint64_t), bool for a bool payload, a float payload's plain C float/double
// type (floatCType — float for f32, double for f64), the payload's own
// tuple/struct typedef name, and, since the OptionalIntegerToEnum slice, the
// payload's own enum typedef (pebble_enum_<typeID>_t) for an enum payload —
// the destination shape of an integer cast to an optional enum (`5 as
// ?Color`), whose optional struct must carry the enum value field. A pointer
// payload (the std/hmap.peb get_by_ref shape, `?*V`) is declared with the
// pointee's own pointer C type, `<pointee> *` via pointerTypeName. Any other
// payload type is a clean rejection naming what was found, since this backend
// emits exactly those C types as optional value fields.
func optionalPayloadCType(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if payloadWidth, integerPayload := resolvedBuiltin(snapshot, id); integerPayload && cType(payloadWidth) != "" {
		return cType(payloadWidth), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	// A float payload (f32/f64) is declared with its plain C float/double type —
	// no typedef needed, exactly like a bool/str field — mirroring how
	// floatCType is already used for helper parameters/results (task #22).
	if isFloat(snapshot, id) {
		return floatCType(resolvedFloatKind(snapshot, id)), nil
	}
	if isTuple(snapshot, id) {
		return tupleTypeName(id), nil
	}
	if isStruct(snapshot, id) {
		// A tagged union and a plain enum are both enum-shaped (isEnumType
		// reports true for each), but only a plain enum's C representation is a
		// bare C enum typedef — a tagged union's real representation is the
		// tag-plus-payload struct its buildUnionTypedef pair emits, so an
		// optional payload that is a tagged union is declared with the union's
		// own typedef name (pebble_union_<typeID>_t) as the optional struct's
		// .value field, the same C type a tagged-union local, parameter, and
		// result are declared with (see unionTypeName), and the same distinction
		// isTaggedUnionType draws everywhere else in this file.
		if isTaggedUnionType(unit, snapshot, id) {
			return unionTypeName(id), nil
		}
		if isEnumType(unit, snapshot, id) {
			return enumTypeName(id), nil
		}
		return structTypeName(id), nil
	}
	if isPointer(snapshot, id) {
		pointee, ok := pointerPointeeType(snapshot, id)
		if !ok {
			return "", fmt.Errorf("payload type %s has no pointer pointee", describeType(snapshot, id))
		}
		if name := pointerTypeNameForUnit(st, unit, snapshot, pointee); name != "" {
			return name, nil
		}
		return "", fmt.Errorf("payload type %s has a pointee %s whose C type is unsupported", describeType(snapshot, id), describeType(snapshot, pointee))
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("payload type %s is not supported, want a fixed-width integer, bool, tuple, struct, or enum", name)
		}
	}
	return "", fmt.Errorf("payload type %s is not supported, want a fixed-width integer, bool, tuple, struct, or enum", describeType(snapshot, id))
}

// unionMemberCType is the C type one tagged-union payload member of the given
// payload type is declared with in its union's struct typedef: int32_t /
// int64_t for a payload of the entry's resolved width, bool for a bool payload,
// and the runtime ABI's fixed PebbleStr for a str payload. Any other payload
// type is a clean rejection naming what was found, since this backend emits
// exactly those three C types as union members.
func unionMemberCType(unit *tir.Unit, snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isWidth(snapshot, width, id) {
		return cType(width), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if isStr(snapshot, id) {
		return "PebbleStr", nil
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("payload type %s is not supported, want %s, bool, or str", name, wantName(width))
		}
	}
	return "", fmt.Errorf("payload type %s is not supported, want %s, bool, or str", describeType(snapshot, id), wantName(width))
}

// functionTypeParamCType resolves one function type's parameter to the C type
// an ordinary Pebble-convention helper's parameter of that type is declared
// with (see buildHelperFunctions): uint64_t for uint/u64, bool for bool,
// int32_t for char, PebbleStr for str, the pointee's own `<pointee> *` via
// pointerTypeName for a pointer parameter (the same spelling helperSignature
// gives an ordinary helper's pointer parameter), and cType(ownWidth) for any
// other integer — including the entry's own width and the narrow fixed-width
// integers (u8, u16, i8, i16, u32, i64), each resolved at ITS OWN width via
// resolvedBuiltin/cType, independent of the ambient `width` of the context
// the function type is being resolved from. This mirrors
// validateFunctionTypeSignature's parameter admission (deliberately
// width-independent) and buildCallArgument's per-argument own-width
// resolution, so a parameter's C type always agrees with the C type the
// corresponding call argument is built at — the exact self-contained set
// validateFunctionTypeSignature admits.
// Anything else is a clean rejection, defense for hand-built IR (the
// validation has already ruled every reachable parameter shape out).
func functionTypeParamCType(st *emitState, snapshot *types.Snapshot, width types.BuiltinKind, param types.TypeID) (string, error) {
	switch {
	case isUint(snapshot, param):
		return "uint64_t", nil
	case isU64(snapshot, param):
		return "uint64_t", nil
	case isBool(snapshot, param):
		return "bool", nil
	case isChar(snapshot, param):
		return "int32_t", nil
	case isStr(snapshot, param):
		return "PebbleStr", nil
	case isPointer(snapshot, param):
		// A pointer parameter is spelled the same way helperSignature spells
		// an ordinary helper's pointer parameter: pointerTypeName takes the
		// pointee, not the pointer type itself (it appends " *" to the
		// pointee's own C type), so the pointee must be extracted first.
		pointeeTypeID, ok := pointerPointeeType(snapshot, param)
		if !ok {
			return "", fmt.Errorf("function type parameter type %s has no pointer pointee", describeType(snapshot, param))
		}
		if ctypeName := pointerTypeName(st, snapshot, pointeeTypeID); ctypeName != "" {
			return ctypeName, nil
		}
		return "", fmt.Errorf("function type parameter type %s has a pointee %s whose C type is unsupported", describeType(snapshot, param), describeType(snapshot, pointeeTypeID))
	}
	// Any other fixed-width integer parameter (the entry's own width included,
	// which is exactly what the former isWidth case resolved, and the narrow
	// widths u8/u16/i8/i16/u32/i64) is resolved at ITS OWN width rather than
	// the ambient one, mirroring how validateFunctionTypeSignature validates
	// parameters and how buildArrayBraceElements/buildCallArgument build each
	// value at its own resolved width. A non-integer here is a clean rejection.
	paramWidth, integerParam := resolvedBuiltin(snapshot, param)
	if integerParam && cType(paramWidth) != "" {
		return cType(paramWidth), nil
	}
	return "", fmt.Errorf("function type parameter type %s is not supported, want %s, uint, bool, char, str, or a pointer type", describeType(snapshot, param), wantName(width))
}

// functionTypeResultCType resolves one function type's result to the C return
// type an ordinary Pebble-convention helper with that result is declared with
// (see buildHelperFunctions): the entry's cType(width) for a width result,
// bool, int32_t for char, the pointee's own `<pointee> *` via pointerTypeName
// for a pointer result (the same spelling helperSignature gives an ordinary
// helper's pointer result), and void — the exact self-contained set
// validateFunctionTypeSignature admits. Anything else is a clean rejection,
// defense for hand-built IR (the validation has already ruled every reachable
// result shape out).
func functionTypeResultCType(st *emitState, snapshot *types.Snapshot, width types.BuiltinKind, result types.TypeID) (string, error) {
	switch {
	case isWidth(snapshot, width, result):
		return cType(width), nil
	case isU64(snapshot, result):
		return "uint64_t", nil
	case isBool(snapshot, result):
		return "bool", nil
	case isChar(snapshot, result):
		return "int32_t", nil
	case isVoid(snapshot, result):
		return "void", nil
	case isPointer(snapshot, result):
		// A pointer result is spelled the same way helperSignature spells an
		// ordinary helper's pointer result: pointerTypeName takes the
		// pointee, not the pointer type itself (it appends " *" to the
		// pointee's own C type), so the pointee must be extracted first.
		pointeeTypeID, ok := pointerPointeeType(snapshot, result)
		if !ok {
			return "", fmt.Errorf("function type result type %s has no pointer pointee", describeType(snapshot, result))
		}
		if ctypeName := pointerTypeName(st, snapshot, pointeeTypeID); ctypeName != "" {
			return ctypeName, nil
		}
		return "", fmt.Errorf("function type result type %s has a pointee %s whose C type is unsupported", describeType(snapshot, result), describeType(snapshot, pointeeTypeID))
	}
	return "", fmt.Errorf("function type result type %s is not supported, want %s, bool, char, void, or a pointer type", describeType(snapshot, result), wantName(width))
}

// resolvedBuiltin resolves a TypeID to the builtin kind it names, if it names
// one. It is how the emitter decides what a value node's type means — the
// entry's integer width for an integer local's initializer, or bool for a bool
// local's — without re-deriving anything.
func resolvedBuiltin(snapshot *types.Snapshot, id types.TypeID) (types.BuiltinKind, bool) {
	if snapshot == nil {
		return 0, false
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return 0, false
	}
	return key.Builtin()
}

// cType returns the fixed-width C integer type corresponding to a Pebble
// integer builtin. Int and uint use Pebble's platform-independent 32-bit and
// 64-bit representations respectively.
func cType(width types.BuiltinKind) string {
	switch width {
	case types.Int:
		return "int32_t"
	case types.Uint:
		return "uint64_t"
	case types.I8:
		return "int8_t"
	case types.I16:
		return "int16_t"
	case types.I32:
		return "int32_t"
	case types.I64:
		return "int64_t"
	case types.U8:
		return "uint8_t"
	case types.U16:
		return "uint16_t"
	case types.U32:
		return "uint32_t"
	case types.U64:
		return "uint64_t"
	}
	return ""
}

// floatCType returns the C floating-point type corresponding to a Pebble
// float builtin: float for f32, double for f64. It is deliberately a separate
// helper from cType rather than an extension of it: cType is integer-specific
// by name and doc-comment, and its ""-means-not-an-integer contract is relied
// on by several integer-only paths (validateHelperSignature, the
// buildScalarInitializeCore fall-through), so overloading it with float kinds
// would change what those paths mean. Anything that is not a float builtin
// returns "", matching cType's convention.
func floatCType(width types.BuiltinKind) string {
	switch width {
	case types.F32:
		return "float"
	case types.F64:
		return "double"
	}
	return ""
}

// describeType renders a TypeID into a short human-readable spelling for
// error messages. It only needs to be good enough to name what was found.
func describeType(snapshot *types.Snapshot, id types.TypeID) string {
	if snapshot == nil {
		return fmt.Sprintf("type %d", id)
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return fmt.Sprintf("type %d", id)
	}
	switch key.Kind() {
	case types.Builtin:
		if builtin, ok := key.Builtin(); ok {
			if name, ok := builtinName(builtin); ok {
				return name
			}
		}
	case types.Pointer:
		if child, ok := key.Child(); ok {
			return "*" + describeType(snapshot, child)
		}
	case types.Array:
		if length, child, ok := key.Array(); ok {
			return fmt.Sprintf("[%d]%s", length, describeType(snapshot, child))
		}
	case types.Slice:
		if child, ok := key.Child(); ok {
			return "[]" + describeType(snapshot, child)
		}
	case types.Tuple:
		if elements, ok := key.Elements(); ok {
			parts := make([]string, len(elements))
			for i, element := range elements {
				parts[i] = describeType(snapshot, element)
			}
			return "(" + strings.Join(parts, ", ") + ")"
		}
	case types.Optional:
		if child, ok := key.Child(); ok {
			return "?" + describeType(snapshot, child)
		}
	case types.Function:
		if _, parameters, result, _, ok := key.Function(); ok {
			parts := make([]string, len(parameters))
			for i, parameter := range parameters {
				parts[i] = describeType(snapshot, parameter)
			}
			return "fn(" + strings.Join(parts, ", ") + ") " + describeType(snapshot, result)
		}
	case types.Nominal:
		if declaration, _, ok := key.Nominal(); ok {
			return fmt.Sprintf("nominal(symbol %d)", declaration)
		}
	case types.TypeParameter:
		if declaration, ok := key.TypeParameter(); ok {
			return fmt.Sprintf("type-parameter(symbol %d)", declaration)
		}
	}
	return fmt.Sprintf("type %d", id)
}
