package infer

import (
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type TypeState uint8

const (
	TypeFinal TypeState = iota + 1
	TypeError
)

type TypeResult struct {
	State TypeState
	Type  types.TypeID
}
type SymbolType struct {
	Symbol symbol.SymbolID
	Result TypeResult
}
type SyntaxType struct {
	Syntax symbol.SyntaxRef
	Result TypeResult
}

type RequirementKind uint8

const (
	RequirementNumeric RequirementKind = iota + 1
	RequirementIntegral
	RequirementOrdered
	RequirementLiteralFits
)

type ExactLiteralKind uint8

const (
	ExactInteger ExactLiteralKind = iota + 1
	ExactFloat
)

type Requirement struct {
	Owner       symbol.SymbolID
	Parameter   symbol.SymbolID
	Kind        RequirementKind
	Subject     types.TypeID
	Origin      Origin
	LiteralKind ExactLiteralKind
	Numerator   string
	Denominator string
}
type Instantiation struct {
	Site      symbol.SyntaxRef
	Generic   symbol.SymbolID
	Arguments []TypeResult
}
type MethodSelection struct {
	Site      symbol.SyntaxRef
	Method    symbol.SymbolID
	Arguments []TypeResult
}

type Solution struct {
	successful     bool
	symbols        map[symbol.SymbolID]TypeResult
	syntax         map[symbol.SyntaxRef]TypeResult
	requirements   map[symbol.SymbolID][]Requirement
	instantiations map[symbol.SyntaxRef]Instantiation
	methods        map[symbol.SyntaxRef]MethodSelection
	selections     map[ConstraintID]uint32
}

func (r *Solution) Successful() bool { return r != nil && r.successful }
func (r *Solution) SymbolType(id symbol.SymbolID) (TypeResult, bool) {
	if r == nil {
		return TypeResult{}, false
	}
	v, ok := r.symbols[id]
	return v, ok
}
func (r *Solution) SymbolTypes() []SymbolType {
	if r == nil {
		return nil
	}
	ids := make([]int, 0, len(r.symbols))
	for id := range r.symbols {
		ids = append(ids, int(id))
	}
	sort.Ints(ids)
	out := make([]SymbolType, 0, len(ids))
	for _, id := range ids {
		sid := symbol.SymbolID(id)
		out = append(out, SymbolType{sid, r.symbols[sid]})
	}
	return out
}
func (r *Solution) SyntaxType(ref symbol.SyntaxRef) (TypeResult, bool) {
	if r == nil {
		return TypeResult{}, false
	}
	v, ok := r.syntax[ref]
	return v, ok
}
func (r *Solution) SyntaxTypes() []SyntaxType {
	if r == nil {
		return nil
	}
	refs := make([]symbol.SyntaxRef, 0, len(r.syntax))
	for ref := range r.syntax {
		refs = append(refs, ref)
	}
	sort.Slice(refs, func(i, j int) bool {
		if refs[i].Module != refs[j].Module {
			return refs[i].Module < refs[j].Module
		}
		return refs[i].Node < refs[j].Node
	})
	out := make([]SyntaxType, 0, len(refs))
	for _, ref := range refs {
		out = append(out, SyntaxType{ref, r.syntax[ref]})
	}
	return out
}
func (r *Solution) Requirements(owner symbol.SymbolID) []Requirement {
	if r == nil {
		return nil
	}
	return append([]Requirement(nil), r.requirements[owner]...)
}
func (r *Solution) Instantiation(site symbol.SyntaxRef) (Instantiation, bool) {
	if r == nil {
		return Instantiation{}, false
	}
	v, ok := r.instantiations[site]
	v.Arguments = append([]TypeResult(nil), v.Arguments...)
	return v, ok
}
func (r *Solution) Method(site symbol.SyntaxRef) (MethodSelection, bool) {
	if r == nil {
		return MethodSelection{}, false
	}
	v, ok := r.methods[site]
	v.Arguments = append([]TypeResult(nil), v.Arguments...)
	return v, ok
}
func (r *Solution) Selection(id ConstraintID) (uint32, bool) {
	if r == nil {
		return 0, false
	}
	v, ok := r.selections[id]
	return v, ok
}
