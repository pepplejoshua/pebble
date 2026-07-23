// Package types owns compilation-local immutable semantic type identities.
package types

// TypeID identifies one canonical semantic type in one Store. Zero is invalid.
type TypeID uint32

// IsValid reports whether id is nonzero. Store membership is checked by Store.
func (id TypeID) IsValid() bool { return id != 0 }
