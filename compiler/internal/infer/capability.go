package infer

import (
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func (s *Session) builtinClass(id types.TypeID) typesBuiltin {
	key, ok := s.program.typeKey(id)
	if !ok {
		return builtinOther
	}
	kind, ok := key.Builtin()
	if !ok {
		return builtinOther
	}
	return builtinClassKind(kind)
}

// builtinClassKind classifies a builtin kind into the local typesBuiltin enum
// without any live Session, so the exact-literal fit math can be reused by
// validation consumers that only hold a semantic snapshot.
func builtinClassKind(kind types.BuiltinKind) typesBuiltin {
	switch kind {
	case types.Int:
		return builtinInt
	case types.Uint:
		return builtinUint
	case types.I8:
		return builtinI8
	case types.I16:
		return builtinI16
	case types.I32:
		return builtinI32
	case types.I64:
		return builtinI64
	case types.U8:
		return builtinU8
	case types.U16:
		return builtinU16
	case types.U32:
		return builtinU32
	case types.U64:
		return builtinU64
	case types.F32:
		return builtinF32
	case types.F64:
		return builtinF64
	default:
		return builtinOther
	}
}

func (s *Session) fitLiterals(values []literalValue, id types.TypeID, origin Origin) bool {
	class := s.builtinClass(id)
	for _, value := range values {
		var fits bool
		if value.kind == literalInteger {
			fits = integerFits(value.integer, class, s.program.inputs.LiteralTarget.WordBits)
		} else if class == builtinF32 {
			fits = floatFits(value.rational, 32)
		} else if class == builtinF64 {
			fits = floatFits(value.rational, 64)
		}
		if !fits {
			s.conflict(CodeLiteral, "exact numeric literal does not fit the required builtin type", origin, value.origin)
			return false
		}
	}
	return true
}

func (s *Session) checkCapabilities(caps capability, id types.TypeID, origin Origin) bool {
	if caps == 0 {
		return true
	}
	key, ok := s.program.typeKey(id)
	if !ok {
		return s.conflict(CodeCapability, "capability subject is not a store-owned type", origin)
	}
	if _, rigid := key.TypeParameter(); rigid {
		return true
	}
	class := s.builtinClass(id)
	isIntegral := class >= builtinInt && class <= builtinU64
	isNumeric := isIntegral || class == builtinF32 || class == builtinF64
	if caps&capNumeric != 0 && !isNumeric {
		return s.conflict(CodeCapability, "type does not satisfy the numeric capability", origin)
	}
	if caps&capIntegral != 0 && !isIntegral {
		return s.conflict(CodeCapability, "type does not satisfy the integral capability", origin)
	}
	return true // Ordered legality remains phase 6 policy.
}

func (s *Session) applyCapability(term Term, cap capability, origin Origin) (bool, bool) {
	if term.kind == termError {
		return false, true
	}
	if term.kind == termKnown {
		key, _ := s.program.typeKey(term.known)
		if parameter, rigid := key.TypeParameter(); rigid {
			if origin.GenericOwner == 0 {
				return false, s.conflict(CodeDamagedInput, "rigid capability constraint has no generic owner", origin)
			}
			s.recordRequirement(origin.GenericOwner, cap, parameter, term.known, origin)
			return false, true
		}
		return false, s.checkCapabilities(cap, term.known, origin)
	}
	root := s.find(term.id)
	if root == 0 || s.cells[root-1].error {
		return false, true
	}
	cell := &s.cells[root-1]
	cell.capabilityEvidence = appendCapabilityEvidence(cell.capabilityEvidence, capabilityEvidence{capability: cap, origin: origin})
	if cell.capabilities&cap == cap {
		if cell.known != 0 {
			if parameter, rigid := s.typeParameter(cell.known); rigid {
				if origin.GenericOwner == 0 {
					return false, s.conflict(CodeDamagedInput, "rigid capability constraint has no generic owner", origin)
				}
				s.recordRequirement(origin.GenericOwner, cap, parameter, cell.known, origin)
				return false, true
			}
			return false, s.checkCapabilities(cap, cell.known, origin)
		}
		return false, true
	}
	cell.capabilities |= cap
	if cell.known != 0 {
		if parameter, rigid := s.typeParameter(cell.known); rigid {
			if origin.GenericOwner == 0 {
				cell.error = true
				return true, s.conflict(CodeDamagedInput, "rigid capability constraint has no generic owner", origin)
			}
			s.recordRequirement(origin.GenericOwner, cap, parameter, cell.known, origin)
			return true, true
		}
		if !s.checkCapabilities(cell.capabilities, cell.known, origin) {
			if !s.Fatal() {
				cell.error = true
			}
			return true, false
		}
	}
	return true, true
}

func (s *Session) recordRequirement(owner symbol.SymbolID, cap capability, parameter symbol.SymbolID, subject types.TypeID, origin Origin) {
	if s.Fatal() {
		return
	}
	var kind RequirementKind
	switch cap {
	case capNumeric:
		kind = RequirementNumeric
	case capIntegral:
		kind = RequirementIntegral
	case capOrdered:
		kind = RequirementOrdered
	default:
		return
	}
	for i, existing := range s.requirements {
		if existing.Owner == owner && existing.Kind == kind && existing.Subject == subject {
			if originBefore(origin, existing.Origin) {
				s.requirements[i].Origin = origin
			}
			return
		}
	}
	s.requirements = append(s.requirements, Requirement{Owner: owner, Parameter: parameter, Kind: kind, Subject: subject, Origin: origin})
}

func (s *Session) recordLiteralRequirement(owner symbol.SymbolID, subject types.TypeID, literal literalValue, origin Origin) {
	if s.Fatal() {
		return
	}
	parameter, _ := s.typeParameter(subject)
	requirement := Requirement{Owner: owner, Parameter: parameter, Kind: RequirementLiteralFits, Subject: subject, Origin: origin}
	if literal.kind == literalInteger {
		requirement.LiteralKind = ExactInteger
		requirement.Numerator = literal.integer.String()
	} else {
		requirement.LiteralKind = ExactFloat
		requirement.Numerator = literal.rational.Num().String()
		requirement.Denominator = literal.rational.Denom().String()
	}
	for i, existing := range s.requirements {
		if existing.Owner == owner && existing.Kind == requirement.Kind && existing.Subject == subject && existing.LiteralKind == requirement.LiteralKind && existing.Numerator == requirement.Numerator && existing.Denominator == requirement.Denominator {
			if originBefore(origin, existing.Origin) {
				s.requirements[i].Origin = origin
			}
			return
		}
	}
	s.requirements = append(s.requirements, requirement)
}

func (s *Session) typeParameter(id types.TypeID) (symbol.SymbolID, bool) {
	key, ok := s.program.typeKey(id)
	if !ok {
		return 0, false
	}
	return key.TypeParameter()
}

func (s *Session) literalFits(literal, candidate Term, origin Origin) (bool, bool) {
	if literal.kind == termError || candidate.kind == termError {
		return false, true
	}
	if candidate.kind == termKnown {
		key, _ := s.program.typeKey(candidate.known)
		if _, rigid := key.TypeParameter(); rigid {
			return s.unify(literal, candidate, origin)
		}
		return s.bindKnown(literal, candidate.known, origin)
	}
	if id, ok := s.resolvedType(candidate); ok {
		return s.bindKnown(literal, id, origin)
	}
	// Preserve exact values by joining the type-choice classes. This remains
	// delayed until other evidence selects a concrete candidate.
	return s.unify(literal, candidate, origin)
}
