package types

import (
	"errors"
	"fmt"
	"iter"
)

var (
	// ErrInvalidKey identifies malformed, noncanonical, or invalid-child keys.
	ErrInvalidKey = errors.New("invalid type key")
	// ErrLimitExceeded identifies a configured store resource limit.
	ErrLimitExceeded = errors.New("type store limit exceeded")
)

const (
	defaultMaxTypes          uint32 = 1 << 20
	defaultMaxKeyComponents  uint32 = 1 << 12
	defaultMaxTupleElements  uint32 = 1 << 10
	defaultMaxFunctionParams uint32 = 1 << 10
	defaultMaxGenericArgs    uint32 = 1 << 8
)

// Config bounds one semantic type store. Zero values select package defaults.
type Config struct {
	MaxTypes          uint32
	MaxKeyComponents  uint32
	MaxTupleElements  uint32
	MaxFunctionParams uint32
	MaxGenericArgs    uint32
	MaxArrayLength    uint64
}

type hashKeyFunc func(TypeKey) uint64

// Store owns canonical immutable semantic types for one compilation snapshot.
type Store struct {
	config   Config
	entries  []TypeKey
	buckets  map[uint64][]TypeID
	hash     hashKeyFunc
	builtins Builtins
}

// New creates one store and preinterns all primitive types in fixed order.
func New(config Config) (*Store, error) { return newWithHash(config, hashKey) }

// newWithHash is an internal construction seam used to force collisions in tests.
func newWithHash(config Config, hash hashKeyFunc) (*Store, error) {
	config = normalizedConfig(config)
	if config.MaxTypes < builtinCount {
		return nil, limitError("MaxTypes", uint64(builtinCount), uint64(config.MaxTypes))
	}
	if hash == nil {
		return nil, fmt.Errorf("%w: nil hash function", ErrInvalidKey)
	}

	store := &Store{
		config:  config,
		entries: make([]TypeKey, 0, builtinCount),
		buckets: make(map[uint64][]TypeID),
		hash:    hash,
	}
	if err := store.internBuiltins(); err != nil {
		return nil, err
	}
	return store, nil
}

func normalizedConfig(config Config) Config {
	if config.MaxTypes == 0 {
		config.MaxTypes = defaultMaxTypes
	}
	if config.MaxKeyComponents == 0 {
		config.MaxKeyComponents = defaultMaxKeyComponents
	}
	if config.MaxTupleElements == 0 {
		config.MaxTupleElements = defaultMaxTupleElements
	}
	if config.MaxFunctionParams == 0 {
		config.MaxFunctionParams = defaultMaxFunctionParams
	}
	if config.MaxGenericArgs == 0 {
		config.MaxGenericArgs = defaultMaxGenericArgs
	}
	if config.MaxArrayLength == 0 {
		config.MaxArrayLength = ^uint64(0)
	}
	return config
}

// Builtins returns the store's fixed primitive identities by value.
func (s *Store) Builtins() Builtins {
	if s == nil {
		return Builtins{}
	}
	return s.builtins
}

// Intern returns the canonical ID for key or atomically rejects the key.
func (s *Store) Intern(key TypeKey) (TypeID, error) {
	if s == nil {
		return 0, fmt.Errorf("%w: nil store", ErrInvalidKey)
	}
	if err := s.validate(key); err != nil {
		return 0, err
	}

	hash := s.hash(key)
	for _, candidate := range s.buckets[hash] {
		if equalKeys(s.entries[candidate-1], key) {
			return candidate, nil
		}
	}

	if uint64(len(s.entries)) >= uint64(s.config.MaxTypes) {
		return 0, limitError("MaxTypes", uint64(len(s.entries)+1), uint64(s.config.MaxTypes))
	}

	stored := key.clone()
	id := TypeID(len(s.entries) + 1)
	s.entries = append(s.entries, stored)
	s.buckets[hash] = append(s.buckets[hash], id)
	return id, nil
}

// Kind returns the kind of a store-owned ID.
func (s *Store) Kind(id TypeID) (Kind, bool) {
	if !s.contains(id) {
		return 0, false
	}
	return s.entries[id-1].kind, true
}

// Key returns a defensive copy of a store-owned key.
func (s *Store) Key(id TypeID) (TypeKey, bool) {
	if !s.contains(id) {
		return TypeKey{}, false
	}
	return s.entries[id-1].clone(), true
}

// Len returns the number of canonical types, including builtins.
func (s *Store) Len() uint32 {
	if s == nil {
		return 0
	}
	return uint32(len(s.entries))
}

// IDs yields all valid IDs in ascending allocation order.
func (s *Store) IDs() iter.Seq[TypeID] {
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

func (s *Store) contains(id TypeID) bool {
	return s != nil && id.IsValid() && uint64(id) <= uint64(len(s.entries))
}

func (s *Store) validate(key TypeKey) error {
	switch key.kind {
	case Builtin:
		if !validBuiltin(key.builtin) || !key.onlyBuiltin() {
			return invalidKeyError(key.kind, "invalid builtin representation")
		}
	case Pointer, Slice, Optional:
		if !key.onlyChild() || !s.contains(key.child) {
			return invalidKeyError(key.kind, "invalid child TypeID")
		}
		if err := s.checkComponents(key.kind, 1); err != nil {
			return err
		}
	case Array:
		if !key.onlyArray() || !s.contains(key.child) {
			return invalidKeyError(key.kind, "invalid array representation")
		}
		if key.length > s.config.MaxArrayLength {
			return limitError("MaxArrayLength", key.length, s.config.MaxArrayLength)
		}
		if err := s.checkComponents(key.kind, 1); err != nil {
			return err
		}
	case Tuple:
		if !key.onlyElements() || len(key.elements) == 0 {
			return invalidKeyError(key.kind, "tuple requires at least one element")
		}
		if uint64(len(key.elements)) > uint64(s.config.MaxTupleElements) {
			return limitError(
				"MaxTupleElements",
				uint64(len(key.elements)),
				uint64(s.config.MaxTupleElements),
			)
		}
		if err := s.checkComponents(key.kind, uint64(len(key.elements))); err != nil {
			return err
		}
		if !s.containsAll(key.elements) {
			return invalidKeyError(key.kind, "invalid tuple element TypeID")
		}
	case Function:
		if !key.onlyFunction() || !validConvention(key.convention) || !s.contains(key.result) {
			return invalidKeyError(key.kind, "invalid function representation")
		}
		if uint64(len(key.elements)) > uint64(s.config.MaxFunctionParams) {
			return limitError(
				"MaxFunctionParams",
				uint64(len(key.elements)),
				uint64(s.config.MaxFunctionParams),
			)
		}
		if err := s.checkComponents(key.kind, uint64(len(key.elements))+1); err != nil {
			return err
		}
		if !s.containsAll(key.elements) {
			return invalidKeyError(key.kind, "invalid function parameter TypeID")
		}
	case Nominal:
		if !key.onlyNominal() || key.declaration == 0 {
			return invalidKeyError(key.kind, "invalid nominal declaration")
		}
		if uint64(len(key.elements)) > uint64(s.config.MaxGenericArgs) {
			return limitError(
				"MaxGenericArgs",
				uint64(len(key.elements)),
				uint64(s.config.MaxGenericArgs),
			)
		}
		if err := s.checkComponents(key.kind, uint64(len(key.elements))); err != nil {
			return err
		}
		if !s.containsAll(key.elements) {
			return invalidKeyError(key.kind, "invalid nominal argument TypeID")
		}
	case TypeParameter:
		if !key.onlyTypeParameter() || key.declaration == 0 {
			return invalidKeyError(key.kind, "invalid type parameter declaration")
		}
	default:
		return invalidKeyError(key.kind, "unknown kind")
	}
	return nil
}

func (s *Store) containsAll(ids []TypeID) bool {
	for _, id := range ids {
		if !s.contains(id) {
			return false
		}
	}
	return true
}

func (s *Store) checkComponents(kind Kind, count uint64) error {
	if count > uint64(s.config.MaxKeyComponents) {
		return limitError("MaxKeyComponents", count, uint64(s.config.MaxKeyComponents))
	}
	return nil
}

func (k TypeKey) onlyBuiltin() bool {
	return k.child == 0 && k.length == 0 && len(k.elements) == 0 &&
		k.convention == 0 && k.result == 0 && !k.variadic && k.declaration == 0
}

func (k TypeKey) onlyChild() bool {
	return k.builtin == 0 && k.length == 0 && len(k.elements) == 0 &&
		k.convention == 0 && k.result == 0 && !k.variadic && k.declaration == 0
}

func (k TypeKey) onlyArray() bool {
	return k.builtin == 0 && len(k.elements) == 0 && k.convention == 0 &&
		k.result == 0 && !k.variadic && k.declaration == 0
}

func (k TypeKey) onlyElements() bool {
	return k.builtin == 0 && k.child == 0 && k.length == 0 &&
		k.convention == 0 && k.result == 0 && !k.variadic && k.declaration == 0
}

func (k TypeKey) onlyFunction() bool {
	return k.builtin == 0 && k.child == 0 && k.length == 0 && k.declaration == 0
}

func (k TypeKey) onlyNominal() bool {
	return k.builtin == 0 && k.child == 0 && k.length == 0 &&
		k.convention == 0 && k.result == 0 && !k.variadic
}

func (k TypeKey) onlyTypeParameter() bool {
	return k.builtin == 0 && k.child == 0 && k.length == 0 &&
		len(k.elements) == 0 && k.convention == 0 && k.result == 0 && !k.variadic
}

func invalidKeyError(kind Kind, reason string) error {
	return fmt.Errorf("%w: kind %d: %s", ErrInvalidKey, kind, reason)
}

func limitError(name string, value, limit uint64) error {
	return fmt.Errorf("%w: %s: value %d, limit %d", ErrLimitExceeded, name, value, limit)
}
