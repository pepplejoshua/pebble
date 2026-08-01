package types

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// maxSubstituteDepth bounds recursive descent through composite type structure
// during Substitute. Types are interned bottom-up (a composite's children are
// issued before the composite itself), so a structurally cyclic type is
// impossible and this cap is purely defensive, matching the codebase's
// default traversal depth of 256 used by symbol scopes, import resolution, and
// infer's type-syntax descent.
const maxSubstituteDepth uint32 = 256

// Substitute rewrites id by replacing every TypeParameter occurrence found in
// the substitutions map with its mapped concrete TypeID, recursing through
// composite type structure (pointers, arrays, slices, optionals, tuples,
// function types, and nominal type arguments). A TypeParameter not present in
// substitutions is left unchanged (it belongs to an outer scope this
// substitution doesn't touch). Builtin types and Nominal declarations
// themselves are never rewritten -- only Nominal type arguments recurse.
// Returns an error if id is not a store-owned type or if interning a rewritten
// composite fails (mirroring Store.Intern's own error contract).
//
// The substituted values in the map are resolved concrete types by contract:
// a value that still contains a TypeParameter relative to this map (a
// parameter this substitution also owns, or any other) is the caller's bug and
// is returned verbatim rather than silently completed or re-substituted.
func (s *Store) Substitute(id TypeID, substitutions map[symbol.SymbolID]TypeID) (TypeID, error) {
	if s == nil {
		return 0, fmt.Errorf("%w: nil store", ErrInvalidKey)
	}
	if !s.contains(id) {
		return 0, fmt.Errorf("%w: TypeID %d is not store-owned", ErrInvalidKey, id)
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
// structure. It is a read-only pre-scan used to make Substitute a cheap no-op
// on already-concrete types: no recursion work and, above all, no re-intern of
// an identical type. On depth overflow it reports true so the actual
// substitution path reports the resource-limit error rather than the pre-scan
// masking it.
func (s *Store) hasTypeParameter(id TypeID, depth uint32) bool {
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

// substitute is the depth-first core of Substitute. For each composite kind it
// rewrites every child first and only re-interns a new TypeKey when at least
// one child actually changed; otherwise it returns the original id. This is
// the same no-redundant-reconstruction principle that makes repeated
// Substitute calls on already-concrete types cheap no-ops, applied at every
// nesting level.
func (s *Store) substitute(id TypeID, substitutions map[symbol.SymbolID]TypeID, depth uint32) (TypeID, error) {
	if depth >= maxSubstituteDepth {
		return 0, limitError("SubstituteDepth", uint64(depth), uint64(maxSubstituteDepth))
	}
	key, ok := s.Key(id)
	if !ok {
		return 0, fmt.Errorf("%w: TypeID %d is not store-owned", ErrInvalidKey, id)
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
		if key.kind == Pointer {
			return s.Intern(PointerKey(replacement))
		}
		if key.kind == Slice {
			return s.Intern(SliceKey(replacement))
		}
		return s.Intern(OptionalKey(replacement))
	case Array:
		length, element, _ := key.Array()
		replacement, err := s.substitute(element, substitutions, depth+1)
		if err != nil {
			return 0, err
		}
		if replacement == element {
			return id, nil
		}
		return s.Intern(ArrayKey(length, replacement))
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
		return s.Intern(TupleKey(replaced))
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
		return s.Intern(FunctionKey(convention, replacedParameters, resultReplacement, variadic))
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
		return s.Intern(NominalKey(declaration, replaced))
	}
	return 0, invalidKeyError(key.kind, "unknown kind")
}
