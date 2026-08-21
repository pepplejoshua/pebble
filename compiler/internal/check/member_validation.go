package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeMember diagnostic.Code = "C0605"

// untaggedUnionDeclaration reports whether declaration is an UNTAGGED union
// (`union { ... }`, NominalUnion) rather than a tagged union (`union enum { ... }`).
// The two are distinguished only at the checker's declaration level: the parser
// and resolver register the members of BOTH union forms as SymbolVariant, so a
// validators that must treat an untagged union's members as real fields (reads
// and writes of any declared member, unconditionally — the deliberate unsafe
// reinterpret-the-bytes contract) keys off this Nominal kind instead.
func untaggedUnionDeclaration(handoff *solveHandoff, declaration symbol.SymbolID) bool {
	if handoff == nil || handoff.Semantics == nil || declaration == 0 {
		return false
	}
	typeDecl, ok := handoff.Semantics.TypeDeclaration(declaration)
	return ok && typeDecl.Nominal == infer.NominalUnion
}

func validateMemberRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	resolution := handoff.Semantics.Resolution()
	typeSnapshot := handoff.Semantics.Types()
	failed := false
	variantBySyntax := collectVariantBySyntax(handoff)
	assignmentsByPlace := make(map[valueID]bool)
	for _, retained := range handoff.Records.Records() {
		if retained.Assignment != nil && activeOperatorRecord(handoff, retained.Header) {
			assignmentsByPlace[retained.Assignment.Place] = true
		}
	}

	report := func(header recordHeader) {
		failed = true
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeMember,
			Message:  "member operation is invalid",
			Primary:  diagnostic.Label{Span: header.Span},
		})
	}
	base := func(id valueID) (infer.TypeResult, types.TypeKey, bool) {
		result, ok := records.Root(id)
		if !ok || result.State != infer.TypeFinal {
			return infer.TypeResult{}, types.TypeKey{}, false
		}
		key, ok := typeSnapshot.Key(result.Type)
		return result, key, ok
	}

	for _, retained := range handoff.Records.Records() {
		member := retained.Member
		if member == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		if member.Kind == memberMethod {
			// Method selection is validated later by the call validator.
			continue
		}
		if result, ok := records.Root(member.Result); !ok || result.State != infer.TypeFinal {
			continue
		}

		switch member.Kind {
		case memberStatic, memberVariant:
			selected, ok := resolution.Symbols.Symbol(member.Member)
			if !ok || (member.Kind == memberVariant && selected.Kind != symbol.SymbolVariant) ||
				(member.Kind == memberStatic && selected.Kind != symbol.SymbolField && selected.Kind != symbol.SymbolMethod) {
				report(member.Header)
			}
		case memberField:
			_, key, ok := base(member.Base)
			if !ok {
				continue
			}
			if key.Kind() == types.Pointer {
				pointee, childOK := key.Child()
				if !childOK {
					report(member.Header)
					continue
				}
				key, childOK = typeSnapshot.Key(pointee)
				if !childOK {
					report(member.Header)
					continue
				}
			}
			declaration, _, ok := key.Nominal()
			if !ok {
				valid := member.Name == "len" && (key.Kind() == types.Array || (key.Kind() == types.Builtin && func() bool { builtin, ok := key.Builtin(); return ok && builtin == types.Str }()))
				if key.Kind() == types.Slice {
					valid = member.Name == "len" || member.Name == "data"
				}
				if key.Kind() == types.Optional {
					valid = member.Name == "has_value"
				}
				if !valid {
					report(member.Header)
				}
				continue
			}
			matched := false
			for _, id := range resolution.Members(declaration) {
				selected, found := resolution.Symbols.Symbol(id)
				if !found || selected.Name != member.Name {
					continue
				}
				if selected.Kind == symbol.SymbolField {
					matched = true
					break
				}
				// An untagged union's members are registered as SymbolVariant
				// (the parser emits VariantDecl for both union forms), but they
				// are real fields under the untagged-union contract: a read or
				// write of any declared member is unconditionally legal, with
				// no switch-case narrowing and no "was this field last written"
				// tracking — the deliberate unsafe semantics. Only an UNTAGGED
				// union member is accepted here; a tagged union's variant
				// member stays gated by narrowedUnionVariantAccess /
				// unionVariantPayloadWrite below.
				if selected.Kind == symbol.SymbolVariant && untaggedUnionDeclaration(handoff, declaration) {
					matched = true
					break
				}
			}
			if !matched && !narrowedUnionVariantAccess(handoff, resolution, declaration, member, variantBySyntax) && !unionVariantPayloadWrite(handoff, resolution, declaration, member, assignmentsByPlace) {
				report(member.Header)
			}
		case memberTuple:
			_, key, ok := base(member.Base)
			if !ok {
				continue
			}
			elements, ok := key.Elements()
			if !ok || member.TupleOrdinal >= uint32(len(elements)) {
				report(member.Header)
			}
		}
	}
	return !failed
}

// narrowedUnionVariantAccess accepts a member access that matched no real
// field of its base's declaration only when that declaration is a union and
// the access reads one of its variants by name (self.Ok) while lexically
// inside a switch-case arm narrowed to that exact variant.
func narrowedUnionVariantAccess(handoff *solveHandoff, resolution *symbol.Result, declaration symbol.SymbolID, member *memberRecord, variantBySyntax map[symbol.SyntaxRef]symbol.SymbolID) bool {
	if handoff == nil || resolution == nil || member == nil || declaration == 0 {
		return false
	}
	typeDecl, ok := handoff.Semantics.TypeDeclaration(declaration)
	if !ok || (typeDecl.Nominal != infer.NominalEnum && typeDecl.Nominal != infer.NominalTaggedUnion) {
		return false
	}
	variant := false
	for _, id := range resolution.Members(declaration) {
		selected, found := resolution.Symbols.Symbol(id)
		if found && selected.Kind == symbol.SymbolVariant && selected.Name == member.Name {
			variant = true
			break
		}
	}
	if !variant {
		return false
	}
	return switchCaseNarrowing(handoff, resolution, member, variantBySyntax)
}

// unionVariantPayloadWrite accepts a member access that matched no real field
// of its base's declaration when that declaration is a union, the member names
// one of its declared variants, and the access is the write target of an
// assignment (`self.Err = e;`). Writing a variant's own payload member
// establishes that variant as the active one — the backend sets the union's
// .tag to the variant's discriminant on the write — so, unlike a read, it
// needs no enclosing switch-case narrowing and is legal on a pointer receiver,
// a value receiver, or a plain local. A member whose name is not one of the
// union's declared variants stays rejected, exactly as an unknown field does.
func unionVariantPayloadWrite(handoff *solveHandoff, resolution *symbol.Result, declaration symbol.SymbolID, member *memberRecord, assignmentsByPlace map[valueID]bool) bool {
	if handoff == nil || resolution == nil || member == nil || declaration == 0 {
		return false
	}
	typeDecl, ok := handoff.Semantics.TypeDeclaration(declaration)
	if !ok || (typeDecl.Nominal != infer.NominalEnum && typeDecl.Nominal != infer.NominalTaggedUnion) {
		return false
	}
	variant := false
	for _, id := range resolution.Members(declaration) {
		selected, found := resolution.Symbols.Symbol(id)
		if found && selected.Kind == symbol.SymbolVariant && selected.Name == member.Name {
			variant = true
			break
		}
	}
	if !variant {
		return false
	}
	return assignmentsByPlace[member.Result]
}
