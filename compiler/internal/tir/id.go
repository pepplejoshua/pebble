// Package tir owns the closed typed intermediate representation store.
package tir

// NodeID identifies one node in one Unit. Zero is invalid.
type NodeID uint32

// FunctionID identifies one function body region in one Unit. Zero is invalid.
type FunctionID uint32

// RegionID identifies one lexical control region in one Unit. Zero is invalid.
type RegionID uint32

// TempID identifies one temporary binding in one Unit. Zero is invalid.
type TempID uint32

// IsValid reports whether id is nonzero.
func (id NodeID) IsValid() bool     { return id != 0 }
func (id FunctionID) IsValid() bool { return id != 0 }
func (id RegionID) IsValid() bool   { return id != 0 }
func (id TempID) IsValid() bool     { return id != 0 }
