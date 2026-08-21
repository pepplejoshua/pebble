package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func validateSwitches(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if !canContinueWithPartial(handoff, config) || records == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil {
		return true
	}

	config = normalizeConfig(config)
	reporter := newValidationReporter(diagnostics, config.MaxDiagnostics)
	resolution := handoff.Semantics.Resolution()
	typeSnapshot := handoff.Semantics.Types()
	failed := false

	report := func(ctrl *controlRecord, message string) {
		failed = true
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeInvalidTarget,
			Message:  message,
			Primary:  diagnostic.Label{Span: ctrl.Header.Span},
		})
	}

	bySyntax := make(map[symbol.SyntaxRef]*controlRecord)
	variantBySyntax := collectVariantBySyntax(handoff)
	for _, retained := range handoff.Records.Records() {
		if !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		if retained.Control != nil {
			bySyntax[retained.Header.Syntax] = retained.Control
		}
	}

	for _, retained := range handoff.Records.Records() {
		ctrl := retained.Control
		if ctrl == nil || ctrl.Kind != controlSwitch || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}

		if len(ctrl.Values) == 0 || ctrl.Values[0].Role != valueSubject {
			continue
		}
		subjectValue := ctrl.Values[0].Value
		if subjectValue == 0 {
			continue
		}
		typeResult, ok := records.Root(subjectValue)
		if !ok || typeResult.State != infer.TypeFinal {
			continue
		}
		typeKey, ok := typeSnapshot.Key(typeResult.Type)
		if !ok {
			continue
		}

		// (a) Validate subject category.
		subjectBuiltin, isBuiltin := typeKey.Builtin()
		subjectDecl, _, isNominal := typeKey.Nominal()

		subjectCategory := switchSubjectCategory(subjectBuiltin, isBuiltin, subjectDecl, isNominal, handoff)
		if subjectCategory == 0 {
			report(ctrl, "switch subject type is not a valid switch category")
			continue
		}

		var caseArms []*controlRecord
		for _, child := range ctrl.Composition {
			if child.Role != roleCase {
				continue
			}
			if arm := bySyntax[child.Arm]; arm != nil {
				caseArms = append(caseArms, arm)
			}
		}

		if subjectCategory == categoryNominal {
			validateNominalSwitch(ctrl, caseArms, subjectDecl, resolution, variantBySyntax, report)
		} else {
			validateScalarSwitch(ctrl, caseArms, subjectCategory, records, variantBySyntax, report)
		}
	}

	reporter.flush()
	return !failed
}

// caseVariantMember recovers the variant symbol a base-less .name switch case
// label refers to. The resolver defers partial-member names (symbol/visit.go),
// so an aggregateEnumVariant/aggregateTaggedVariant record carries the authored
// name rather than a resolved member; the member is re-derived by name from the
// record's solved nominal declaration here, mirroring validateAggregateRecords
// and ir_builder_literals.go. When the record has no declaration (a generic
// receiver's case label is authored against a template that only solving
// materializes), the declaration is recovered from the solved receiver type.
func caseVariantMember(handoff *solveHandoff, aggregate *aggregateRecord) symbol.SymbolID {
	if handoff == nil || aggregate == nil || len(aggregate.Fields) == 0 {
		return 0
	}
	if member := aggregate.Fields[0].Member; member != 0 {
		return member
	}
	name := aggregate.Fields[0].Name
	if name == "" {
		return 0
	}
	declaration := aggregate.Declaration
	if declaration == 0 {
		if typeResult, ok := resolvedRootType(handoff, aggregate.Receiver); ok && typeResult.State == infer.TypeFinal {
			if key, found := handoff.Semantics.Types().Key(typeResult.Type); found {
				if decl, _, ok := key.Nominal(); ok {
					declaration = decl
				}
			}
		}
	}
	if declaration == 0 {
		return 0
	}
	for _, memberID := range handoff.Semantics.Resolution().Members(declaration) {
		selected, ok := handoff.Semantics.Resolution().Symbols.Symbol(memberID)
		if ok && selected.Kind == symbol.SymbolVariant && selected.Name == name {
			return memberID
		}
	}
	return 0
}

// collectVariantBySyntax indexes each switch case value's syntax reference to
// the variant symbol it names, for both memberVariant member records and
// base-less .name aggregate records, mirroring how validateSwitches relates a
// case arm to its subject declaration's variants.
func collectVariantBySyntax(handoff *solveHandoff) map[symbol.SyntaxRef]symbol.SymbolID {
	variantBySyntax := make(map[symbol.SyntaxRef]symbol.SymbolID)
	if handoff == nil {
		return variantBySyntax
	}
	for _, retained := range handoff.Records.Records() {
		if !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		if retained.Member != nil && retained.Member.Kind == memberVariant && retained.Member.Member != 0 {
			variantBySyntax[retained.Header.Syntax] = retained.Member.Member
		}
		if retained.Aggregate != nil && (retained.Aggregate.Kind == aggregateEnumVariant || retained.Aggregate.Kind == aggregateTaggedVariant) && len(retained.Aggregate.Fields) != 0 {
			if member := caseVariantMember(handoff, retained.Aggregate); member != 0 {
				variantBySyntax[retained.Header.Syntax] = member
			}
		}
	}
	return variantBySyntax
}

// switchCaseNarrowing reports whether a member access reading a union variant
// by name (self.Ok) sits lexically inside a switch-case arm narrowed to that
// exact variant. Among all case arms owned by the same callable as the access,
// the one with the smallest span still containing the access's own span is the
// narrowest enclosing arm; only that arm's variants may justify the access. A
// case arm owned by a different callable is never counted, so a nested closure
// body cannot inherit the enclosing case arm's narrowing.
func switchCaseNarrowing(handoff *solveHandoff, resolution *symbol.Result, member *memberRecord, variantBySyntax map[symbol.SyntaxRef]symbol.SymbolID) bool {
	if handoff == nil || resolution == nil || member == nil {
		return false
	}
	var bestSpan source.Span
	var bestValues []controlValue
	best := false
	for _, retained := range handoff.Records.Records() {
		ctrl := retained.Control
		if ctrl == nil || ctrl.Kind != controlSwitchCase || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		if retained.Header.Owner != member.Header.Owner {
			continue
		}
		if !spanContains(retained.Header.Span, member.Header.Span) {
			continue
		}
		if !best || spanLength(retained.Header.Span) < spanLength(bestSpan) {
			best, bestSpan, bestValues = true, retained.Header.Span, ctrl.Values
		}
	}
	if !best {
		return false
	}
	for _, value := range bestValues {
		if value.Role != valueCase {
			continue
		}
		variant, found := variantBySyntax[value.Syntax]
		if !found {
			continue
		}
		selected, ok := resolution.Symbols.Symbol(variant)
		if ok && selected.Name == member.Name {
			return true
		}
	}
	return false
}

func spanContains(outer, inner source.Span) bool {
	return outer.Source == inner.Source && outer.Start <= inner.Start && inner.End <= outer.End
}

func spanLength(span source.Span) uint32 {
	if span.End < span.Start {
		return 0
	}
	return span.End - span.Start
}

type switchCategory uint8

const (
	categoryBool    switchCategory = 1
	categoryChar    switchCategory = 2
	categoryStr     switchCategory = 3
	categoryInteger switchCategory = 4
	categoryNominal switchCategory = 5
)

func switchSubjectCategory(subjectBuiltin types.BuiltinKind, isBuiltin bool, subjectDecl symbol.SymbolID, isNominal bool, handoff *solveHandoff) switchCategory {
	switch {
	case isBuiltin && subjectBuiltin == types.Bool:
		return categoryBool
	case isBuiltin && subjectBuiltin == types.Char:
		return categoryChar
	case isBuiltin && subjectBuiltin == types.Str:
		return categoryStr
	case isBuiltin && isIntegerBuiltin(subjectBuiltin):
		return categoryInteger
	case isNominal:
		typeDecl, ok := handoff.Semantics.TypeDeclaration(subjectDecl)
		if ok && (typeDecl.Nominal == infer.NominalEnum || typeDecl.Nominal == infer.NominalTaggedUnion) {
			return categoryNominal
		}
	}
	return 0
}

func validateNominalSwitch(ctrl *controlRecord, caseArms []*controlRecord, subjectDecl symbol.SymbolID, resolution *symbol.Result, variantBySyntax map[symbol.SyntaxRef]symbol.SymbolID, report func(*controlRecord, string)) {
	allMembers := resolution.Members(subjectDecl)
	variantSet := make(map[symbol.SymbolID]bool)
	for _, memberID := range allMembers {
		sym, ok := resolution.Symbols.Symbol(memberID)
		if ok && sym.Kind == symbol.SymbolVariant {
			variantSet[memberID] = true
		}
	}

	seenVariants := make(map[symbol.SymbolID]bool)
	for _, arm := range caseArms {
		for _, val := range arm.Values {
			if val.Role != valueCase {
				continue
			}
			variantID, found := variantBySyntax[val.Syntax]
			if !found {
				report(ctrl, "switch case value is not a variant of the subject type")
				continue
			}
			if !variantSet[variantID] {
				report(ctrl, "switch case variant does not belong to the subject declaration")
				continue
			}
			if seenVariants[variantID] {
				report(ctrl, "duplicate switch case variant")
			}
			seenVariants[variantID] = true
		}
	}
}

func validateScalarSwitch(ctrl *controlRecord, caseArms []*controlRecord, subjectCategory switchCategory, records *solvedRecords, variantBySyntax map[symbol.SyntaxRef]symbol.SymbolID, report func(*controlRecord, string)) {
	var expectedKind constantKind
	switch subjectCategory {
	case categoryBool:
		expectedKind = constantBoolean
	case categoryChar:
		expectedKind = constantCharacter
	case categoryStr:
		expectedKind = constantString
	case categoryInteger:
		expectedKind = constantInteger
	}

	seenValues := make(map[string]bool)
	for _, arm := range caseArms {
		for _, val := range arm.Values {
			if val.Role != valueCase {
				continue
			}
			if _, found := variantBySyntax[val.Syntax]; found {
				report(ctrl, "switch case uses a variant value for a scalar subject")
				continue
			}

			constResult, found := records.Constant(val.Syntax)
			if !found || constResult.State != constantKnown {
				continue
			}

			if constResult.Value.Kind != expectedKind {
				report(ctrl, "switch case constant kind does not match subject type")
				continue
			}

			key := switchConstantKey(constResult.Value)
			if seenValues[key] {
				report(ctrl, "duplicate switch case value")
			}
			seenValues[key] = true
		}
	}
}

func switchConstantKey(value constantValue) string {
	switch value.Kind {
	case constantBoolean:
		if value.Boolean {
			return "true"
		}
		return "false"
	case constantInteger:
		if value.Integer != nil {
			return value.Integer.String()
		}
		return "0"
	case constantCharacter:
		return string(value.Character)
	case constantString:
		return value.String
	}
	return ""
}
