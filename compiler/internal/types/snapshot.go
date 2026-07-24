package types

import (
	"fmt"
	"iter"
)

// Snapshot is an immutable, independently owned view of a Store.
type Snapshot struct {
	entries  []TypeKey
	builtins Builtins
}

// Snapshot copies and validates every issued type without changing the store.
// The caller must serialize this operation with all other store access.
func (s *Store) Snapshot() (*Snapshot, error) {
	if s == nil {
		return nil, fmt.Errorf("%w: nil store", ErrInvalidKey)
	}
	if uint64(len(s.entries)) > uint64(s.config.MaxTypes) {
		return nil, limitError("MaxTypes", uint64(len(s.entries)), uint64(s.config.MaxTypes))
	}
	if err := s.validateBuiltinsForSnapshot(); err != nil {
		return nil, err
	}

	// Validate the complete captured prefix before allocating or publishing its
	// copy. Store validation recharges every key against the configured limits.
	validationBuckets := make(map[uint64][]TypeID, len(s.entries))
	for index, key := range s.entries {
		if err := s.validate(key); err != nil {
			return nil, err
		}
		if !snapshotChildrenWithinPrefix(key, TypeID(index)) {
			return nil, invalidKeyError(key.kind, "child TypeID was not issued before parent")
		}

		hash := hashKey(key)
		for _, existing := range validationBuckets[hash] {
			if equalKeys(s.entries[existing-1], key) {
				return nil, invalidKeyError(key.kind, "duplicate complete key")
			}
		}
		validationBuckets[hash] = append(validationBuckets[hash], TypeID(index+1))
	}

	entries := make([]TypeKey, len(s.entries))
	for index, key := range s.entries {
		entries[index] = key.clone()
	}
	return &Snapshot{entries: entries, builtins: s.builtins}, nil
}

func snapshotChildrenWithinPrefix(key TypeKey, maxID TypeID) bool {
	contains := func(id TypeID) bool { return id.IsValid() && id <= maxID }
	containsAll := func(ids []TypeID) bool {
		for _, id := range ids {
			if !contains(id) {
				return false
			}
		}
		return true
	}

	switch key.kind {
	case Pointer, Array, Slice, Optional:
		return contains(key.child)
	case Tuple, Nominal:
		return containsAll(key.elements)
	case Function:
		return contains(key.result) && containsAll(key.elements)
	default:
		return true
	}
}

func (s *Store) validateBuiltinsForSnapshot() error {
	wantIDs := [...]TypeID{
		s.builtins.Bool, s.builtins.Char, s.builtins.Str, s.builtins.Void,
		s.builtins.Int, s.builtins.Uint,
		s.builtins.I8, s.builtins.I16, s.builtins.I32, s.builtins.I64,
		s.builtins.U8, s.builtins.U16, s.builtins.U32, s.builtins.U64,
		s.builtins.F32, s.builtins.F64,
	}
	wantKinds := [...]BuiltinKind{
		Bool, Char, Str, Void,
		Int, Uint,
		I8, I16, I32, I64,
		U8, U16, U32, U64,
		F32, F64,
	}
	for index, kind := range wantKinds {
		id := wantIDs[index]
		if id != TypeID(index+1) || !s.contains(id) || !equalKeys(s.entries[id-1], BuiltinKey(kind)) {
			return invalidKeyError(Builtin, "invalid builtin identity")
		}
	}
	return nil
}

// Builtins returns the captured primitive identities by value.
func (s *Snapshot) Builtins() Builtins {
	if s == nil {
		return Builtins{}
	}
	return s.builtins
}

// Kind returns the kind of a captured ID.
func (s *Snapshot) Kind(id TypeID) (Kind, bool) {
	if !s.Contains(id) {
		return 0, false
	}
	return s.entries[id-1].kind, true
}

// Key returns a defensive copy of a captured key.
func (s *Snapshot) Key(id TypeID) (TypeKey, bool) {
	if !s.Contains(id) {
		return TypeKey{}, false
	}
	return s.entries[id-1].clone(), true
}

// Len returns the number of captured types, including builtins.
func (s *Snapshot) Len() uint32 {
	if s == nil {
		return 0
	}
	return uint32(len(s.entries))
}

// Contains reports whether id is nonzero and within the captured entry set.
func (s *Snapshot) Contains(id TypeID) bool {
	return s != nil && id.IsValid() && uint64(id) <= uint64(len(s.entries))
}

// IDs yields captured IDs in ascending allocation order.
func (s *Snapshot) IDs() iter.Seq[TypeID] {
	return func(yield func(TypeID) bool) {
		if s == nil {
			return
		}
		for index := range s.entries {
			if !yield(TypeID(index + 1)) {
				return
			}
		}
	}
}
