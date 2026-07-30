package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func validateSwitches(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.GenerationHadErrors || records == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil {
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
	variantBySyntax := make(map[symbol.SyntaxRef]symbol.SymbolID)
	for _, retained := range handoff.Records.Records() {
		if !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		if retained.Control != nil {
			bySyntax[retained.Header.Syntax] = retained.Control
		}
		if retained.Member != nil && retained.Member.Kind == memberVariant && retained.Member.Member != 0 {
			variantBySyntax[retained.Header.Syntax] = retained.Member.Member
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
