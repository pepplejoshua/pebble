package types

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// Substitute rewrites id by replacing every TypeParameter occurrence found in
// the substitutions map with its mapped concrete TypeID, recursing through
// composite type structure exactly like Store.Substitute. The two methods share
// their substitution contract: a TypeParameter not present in substitutions is
// left unchanged, a substituted value must already be concrete, and composite
// structure is only rewritten when at least one child actually changed.
//
// A Snapshot, unlike a Store, owns no interning machinery — it is an immutable,
// independently owned view of a Store's already-issued casing. Substitution can
// therefore never materialize a brand-new type: a rewritten composite is
// recovered by looking up its rebuilt TypeKey among the snapshot's existing
// entries, and a composite whose rewrite was never interned by the originating
// program is returned as an error rather than partially substituted. This is
// the exact shape the backend needs for generic struct field types, where the
// substitution targets (the concrete type arguments of one instantiation) are
// always already-interned types, and the direct TypeParameter members of slice
// 1 need no composite rewrite at all. Returns an error if id is not
// snapshot-owned or if a rewritten composite has not been interned in the
// snapshot.
func (s *Snapshot) Substitute(id TypeID, substitutions map[symbol.SymbolID]TypeID) (TypeID, error) {
	if s == nil {
		return 0, fmt.Errorf("%w: nil snapshot", ErrInvalidKey)
	}
	if !s.Contains(id) {
		return 0, fmt.Errorf("%w: TypeID %d is not snapshot-owned", ErrInvalidKey, id)
	}
	if len(substitutions) == 0 {
		return id, nil
	}
	if !s.hasTypeParameter(id, 0) {
		return id, nil
	}
	return s.substitute(id, substitutions, 0)
}

// hasTypeParameter reports whether a TypeParameter occurs anywhere in id's
// structure. It is a read-only pre-scan shared with Store.Substitute so a
// Snapshot.Substitute is a cheap no-op on already-concrete types. On depth
// overflow it reports true so the actual substitution path reports the
// resource-limit error rather than the pre-scan masking it.
func (s *Snapshot) hasTypeParameter(id TypeID, depth uint32) bool {
	if depth >= maxSubstituteDepth {
		return true
	}
	key, ok := s.Key(id)
	if !ok {
		return true
	}
	switch key.kind {
	case TypeParameter:
		return true
	case Builtin:
		return false
	case Pointer, Slice, Optional:
		child, _ := key.Child()
		return s.hasTypeParameter(child, depth+1)
	case Array:
		_, element, _ := key.Array()
		return s.hasTypeParameter(element, depth+1)
	case Tuple:
		elements, _ := key.Elements()
		for _, element := range elements {
			if s.hasTypeParameter(element, depth+1) {
				return true
			}
		}
		return false
	case Function:
		_, parameters, result, _, _ := key.Function()
		if s.hasTypeParameter(result, depth+1) {
			return true
		}
		for _, parameter := range parameters {
			if s.hasTypeParameter(parameter, depth+1) {
				return true
			}
		}
		return false
	case Nominal:
		_, arguments, _ := key.Nominal()
		for _, argument := range arguments {
			if s.hasTypeParameter(argument, depth+1) {
				return true
			}
		}
		return false
	}
	return true
}

// substitute is the depth-first core of Snapshot.Substitute, mirroring
// Store.substitute: every child is rewritten first and a composite is only
// rebuilt when at least one child actually changed; otherwise the original id
// is returned. The one structural difference is the terminal step — a rebuilt
// TypeKey is located among the snapshot's existing entries instead of being
// interned, and a rebuilt key no Store ever issued is an error.
func (s *Snapshot) substitute(id TypeID, substitutions map[symbol.SymbolID]TypeID, depth uint32) (TypeID, error) {
	if depth >= maxSubstituteDepth {
		return 0, limitError("SubstituteDepth", uint64(depth), uint64(maxSubstituteDepth))
	}
	key, ok := s.Key(id)
	if !ok {
		return 0, fmt.Errorf("%w: TypeID %d is not snapshot-owned", ErrInvalidKey, id)
	}
	switch key.kind {
	case Builtin:
		return id, nil
	case TypeParameter:
		declaration, _ := key.TypeParameter()
		if replacement, ok := substitutions[declaration]; ok {
			return replacement, nil
		}
		return id, nil
	case Pointer, Slice, Optional:
		child, _ := key.Child()
		replacement, err := s.substitute(child, substitutions, depth+1)
		if err != nil {
			return 0, err
		}
		if replacement == child {
			return id, nil
		}
		var rebuilt TypeKey
		if key.kind == Pointer {
			rebuilt = PointerKey(replacement)
		} else if key.kind == Slice {
			rebuilt = SliceKey(replacement)
		} else {
			rebuilt = OptionalKey(replacement)
		}
		return s.substitutedOrError(rebuilt)
	case Array:
		length, element, _ := key.Array()
		replacement, err := s.substitute(element, substitutions, depth+1)
		if err != nil {
			return 0, err
		}
		if replacement == element {
			return id, nil
		}
		return s.substitutedOrError(ArrayKey(length, replacement))
	case Tuple:
		elements, _ := key.Elements()
		replaced := make([]TypeID, len(elements))
		changed := false
		for index, element := range elements {
			replacement, err := s.substitute(element, substitutions, depth+1)
			if err != nil {
				return 0, err
			}
			replaced[index] = replacement
			changed = changed || replacement != element
		}
		if !changed {
			return id, nil
		}
		return s.substitutedOrError(TupleKey(replaced))
	case Function:
		convention, parameters, result, variadic, _ := key.Function()
		replacedParameters := make([]TypeID, len(parameters))
		changed := false
		for index, parameter := range parameters {
			replacement, err := s.substitute(parameter, substitutions, depth+1)
			if err != nil {
				return 0, err
			}
			replacedParameters[index] = replacement
			changed = changed || replacement != parameter
		}
		resultReplacement, err := s.substitute(result, substitutions, depth+1)
		if err != nil {
			return 0, err
		}
		changed = changed || resultReplacement != result
		if !changed {
			return id, nil
		}
		return s.substitutedOrError(FunctionKey(convention, replacedParameters, resultReplacement, variadic))
	case Nominal:
		declaration, arguments, _ := key.Nominal()
		replaced := make([]TypeID, len(arguments))
		changed := false
		for index, argument := range arguments {
			replacement, err := s.substitute(argument, substitutions, depth+1)
			if err != nil {
				return 0, err
			}
			replaced[index] = replacement
			changed = changed || replacement != argument
		}
		if !changed {
			return id, nil
		}
		return s.substitutedOrError(NominalKey(declaration, replaced))
	}
	return 0, invalidKeyError(key.kind, "unknown kind")
}

// substitutedOrError resolves a rebuilt composite TypeKey to the snapshot TypeID
// that already represents it, or rejects the substitution: a Snapshot can only
// produce types the originating Store actually interned.
func (s *Snapshot) substitutedOrError(rebuilt TypeKey) (TypeID, error) {
	if id, ok := s.lookup(rebuilt); ok {
		return id, nil
	}
	return 0, fmt.Errorf(
		"%w: substituted type %+v is not interned in the snapshot; a Snapshot can only rewrite to types the program already references",
		ErrInvalidKey,
		rebuilt,
	)
}

// lookup returns the snapshot's TypeID for an entry equal to key. It is a
// linear scan: a Snapshot has no hash buckets (that machinery belongs to the
// Store), and substitution is a cold backend path over a modest entry count.
func (s *Snapshot) lookup(key TypeKey) (TypeID, bool) {
	for index, entry := range s.entries {
		if equalKeys(entry, key) {
			return TypeID(index + 1), true
		}
	}
	return 0, false
}
