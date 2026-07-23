package infer

import (
	"math/big"

	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type termKind uint8

const (
	termKnown termKind = iota + 1
	termVariable
	termIntLiteral
	termFloatLiteral
	termError
)

type sessionToken struct{}

// Term is one closed inference term. Its payload is intentionally opaque.
type Term struct {
	owner *sessionToken
	kind  termKind
	id    InferID
	known types.TypeID
}

func (t Term) valid() bool                      { return t.owner != nil && t.kind >= termKnown && t.kind <= termError }
func (t Term) belongs(owner *sessionToken) bool { return t.valid() && t.owner == owner }

type literalKind uint8

const (
	literalInteger literalKind = iota + 1
	literalFloat
)

type literalValue struct {
	kind     literalKind
	integer  *big.Int
	rational *big.Rat
	origin   Origin
}

func (v literalValue) clone() literalValue {
	out := v
	if v.integer != nil {
		out.integer = new(big.Int).Set(v.integer)
	}
	if v.rational != nil {
		out.rational = new(big.Rat).Set(v.rational)
	}
	return out
}
