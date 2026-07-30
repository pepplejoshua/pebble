package check

import (
	"sort"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeUnsupportedGeneric diagnostic.Code = "C0610"

type RequirementKind uint8

const (
	RequirementNumeric RequirementKind = iota + 1
	RequirementIntegral
	RequirementOrdered
	RequirementEquatable
	RequirementLiteralFits
)

type Requirement struct {
	Owner       symbol.SymbolID
	Parameter   symbol.SymbolID
	Kind        RequirementKind
	Subject     types.TypeID
	Origin      symbol.SyntaxRef
	Operator    syntax.TokenKind
	LiteralKind infer.ExactLiteralKind
	Numerator   string
	Denominator string
}

type requirementGroup struct {
	requirement Requirement
	span        source.Span
}

func earlierSpan(a, b source.Span) bool {
	if a.Source != b.Source {
		return a.Source < b.Source
	}
	if a.Start != b.Start {
		return a.Start < b.Start
	}
	return a.End < b.End
}

func mapRequirementKind(kind requirementKind) (RequirementKind, infer.RequirementKind, bool) {
	switch kind {
	case requirementNumeric:
		return RequirementNumeric, infer.RequirementNumeric, true
	case requirementIntegral:
		return RequirementIntegral, infer.RequirementIntegral, true
	case requirementOrdered:
		return RequirementOrdered, infer.RequirementOrdered, true
	}
	return 0, 0, false
}

func parameterOrdinal(handoff *solveHandoff, owner, parameter symbol.SymbolID) int {
	if handoff == nil || handoff.Semantics == nil {
		return int(^uint(0) >> 1)
	}
	if signature, ok := handoff.Semantics.Signature(owner); ok {
		for index, candidate := range signature.TypeParams {
			if candidate == parameter {
				return index
			}
		}
	}
	if declaration, ok := handoff.Semantics.TypeDeclaration(owner); ok {
		for index, candidate := range declaration.Parameters {
			if candidate == parameter {
				return index
			}
		}
	}
	return int(^uint(0) >> 1)
}

func validateRequirements(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) (map[symbol.SymbolID][]Requirement, bool) {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return nil, true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	report := func(code diagnostic.Code, message string, span source.Span) {
		failed = true
		reporter.add(diagnostic.Diagnostic{Severity: diagnostic.Error, Code: code, Message: message, Primary: diagnostic.Label{Span: span}})
	}
	typeSnapshot := handoff.Semantics.Types()
	groups := make(map[[3]uint32]requirementGroup)
	owners := make(map[symbol.SymbolID]bool)
	for _, retained := range handoff.Records.Records() {
		record := retained.Requirement
		if record == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		owner := retained.Header.Owner
		if owner != 0 {
			owners[owner] = true
		}
		resolved, ok := records.Root(record.Subject)
		if !ok || resolved.State != infer.TypeFinal {
			continue
		}
		key, ok := typeSnapshot.Key(resolved.Type)
		if !ok {
			continue
		}
		if kind, inferKind, joinable := mapRequirementKind(record.Kind); joinable {
			matches := []infer.Requirement{}
			for _, candidate := range handoff.Solution.Requirements(owner) {
				if candidate.Owner == owner && candidate.Kind == inferKind && candidate.Subject == resolved.Type {
					matches = append(matches, candidate)
				}
			}
			if len(matches) != 1 || matches[0].Parameter == 0 {
				report(CodeGeneration, "generic requirement is missing or inconsistent in the solved requirement table", retained.Header.Span)
				continue
			}
			candidate := matches[0]
			value := Requirement{Owner: owner, Parameter: candidate.Parameter, Kind: kind, Subject: resolved.Type, Origin: retained.Header.Syntax, Operator: record.Operator}
			groupKey := [3]uint32{uint32(owner), uint32(kind), uint32(resolved.Type)}
			if existing, exists := groups[groupKey]; !exists || earlierSpan(retained.Header.Span, existing.span) {
				groups[groupKey] = requirementGroup{requirement: value, span: retained.Header.Span}
			}
			continue
		}
		if record.Kind == requirementEquatable {
			parameter, ok := key.TypeParameter()
			if !ok {
				continue
			}
			value := Requirement{Owner: owner, Parameter: parameter, Kind: RequirementEquatable, Subject: resolved.Type, Origin: retained.Header.Syntax, Operator: record.Operator}
			groupKey := [3]uint32{uint32(owner), uint32(RequirementEquatable), uint32(resolved.Type)}
			if existing, exists := groups[groupKey]; !exists || earlierSpan(retained.Header.Span, existing.span) {
				groups[groupKey] = requirementGroup{requirement: value, span: retained.Header.Span}
			}
			continue
		}
		switch record.Kind {
		case requirementUnsupportedField, requirementUnsupportedMethod, requirementUnsupportedIndex, requirementUnsupportedSlice, requirementUnsupportedCall, requirementUnsupportedConversion, requirementUnsupportedLayout, requirementUnsupportedPrint, requirementUnsupportedConstruction, requirementUnsupportedComponent:
			if key.Kind() == types.TypeParameter {
				report(CodeUnsupportedGeneric, "operation is unsupported on an unconstrained generic type parameter", retained.Header.Span)
			}
		}
	}

	// The solver has no all-requirements iterator. Prepared signatures and type
	// declarations are the complete owner set for generic declarations.
	for _, signature := range handoff.Semantics.Signatures() {
		owners[signature.Symbol] = true
	}
	for _, declaration := range handoff.Semantics.TypeDeclarations() {
		owners[declaration.Symbol] = true
	}
	for owner := range owners {
		for _, candidate := range handoff.Solution.Requirements(owner) {
			if candidate.Kind != infer.RequirementLiteralFits || candidate.Owner != owner || candidate.Parameter == 0 {
				continue
			}
			value := Requirement{Owner: owner, Parameter: candidate.Parameter, Kind: RequirementLiteralFits, Subject: candidate.Subject, Origin: candidate.Origin.Syntax, LiteralKind: candidate.LiteralKind, Numerator: candidate.Numerator, Denominator: candidate.Denominator}
			groupKey := [3]uint32{uint32(owner), uint32(RequirementLiteralFits), uint32(candidate.Parameter)}
			if existing, exists := groups[groupKey]; !exists || earlierSpan(candidate.Origin.Span, existing.span) {
				groups[groupKey] = requirementGroup{requirement: value, span: candidate.Origin.Span}
			}
		}
	}

	result := make(map[symbol.SymbolID][]Requirement)
	resultSpans := make(map[symbol.SymbolID]map[[2]uint32]source.Span)
	for _, group := range groups {
		owner := group.requirement.Owner
		if resultSpans[owner] == nil {
			resultSpans[owner] = make(map[[2]uint32]source.Span)
		}
		key := [2]uint32{uint32(group.requirement.Parameter), uint32(group.requirement.Kind)}
		if existing, exists := resultSpans[owner][key]; exists {
			if earlierSpan(group.span, existing) {
				for index := range result[owner] {
					if result[owner][index].Parameter == group.requirement.Parameter && result[owner][index].Kind == group.requirement.Kind {
						result[owner][index] = group.requirement
						break
					}
				}
				resultSpans[owner][key] = group.span
			}
			continue
		}
		resultSpans[owner][key] = group.span
		result[owner] = append(result[owner], group.requirement)
	}
	for owner := range result {
		requirements := result[owner]
		sort.SliceStable(requirements, func(i, j int) bool {
			left, right := requirements[i], requirements[j]
			li, ri := parameterOrdinal(handoff, owner, left.Parameter), parameterOrdinal(handoff, owner, right.Parameter)
			if li != ri {
				return li < ri
			}
			if left.Kind != right.Kind {
				return left.Kind < right.Kind
			}
			leftSpan := resultSpans[owner][[2]uint32{uint32(left.Parameter), uint32(left.Kind)}]
			rightSpan := resultSpans[owner][[2]uint32{uint32(right.Parameter), uint32(right.Kind)}]
			return earlierSpan(leftSpan, rightSpan)
		})
		result[owner] = requirements
	}
	return result, !failed
}
