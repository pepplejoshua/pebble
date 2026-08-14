package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

const CodeAggregate diagnostic.Code = "C0615"

func validateAggregateRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	resolution := handoff.Semantics.Resolution()
	if resolution == nil || resolution.Symbols == nil {
		return true
	}
	failed := false
	report := func(code diagnostic.Code, header recordHeader) {
		failed = true
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     code,
			Message:  "aggregate construction is invalid",
			Primary:  diagnostic.Label{Span: header.Span},
		})
	}
	fieldReport := func(code diagnostic.Code, field fieldValue, header recordHeader) {
		failed = true
		span := field.NameSpan
		if span.Start == 0 && span.End == 0 {
			span = header.Span
		}
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     code,
			Message:  "aggregate member is invalid",
			Primary:  diagnostic.Label{Span: span},
		})
	}

	for _, retained := range handoff.Records.Records() {
		aggregate := retained.Aggregate
		if aggregate == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		receiver, ok := records.Root(aggregate.Receiver)
		if !ok || receiver.State != infer.TypeFinal {
			continue
		}
		declaration, ok := handoff.Semantics.TypeDeclaration(aggregate.Declaration)
		if !ok {
			continue
		}

		symbolsByID := make(map[symbol.SymbolID]symbol.Symbol)
		orderedMembers := resolution.Members(aggregate.Declaration)
		for _, id := range orderedMembers {
			selected, found := resolution.Symbols.Symbol(id)
			if found {
				symbolsByID[id] = selected
			}
		}

		// byName resolves a construction-literal field name to its declared
		// member symbol. It is shared by the struct and union cases below: a
		// record literal's field may be resolved by the resolver when the
		// literal carries a base-type name (Data.{ a = 5 }) and stay unresolved
		// for the anonymous form (.{ a = 5 }), where the member is re-derived
		// by name against the solved destination declaration here. Both kinds
		// are indexed so the union case (whose members are registered as
		// SymbolVariant, like a tagged union's) can re-derive an anonymous
		// literal's field too; a struct's members are all SymbolField.
		byName := make(map[string]symbol.SymbolID)
		for _, id := range orderedMembers {
			selected, found := symbolsByID[id]
			if found && (selected.Kind == symbol.SymbolField || selected.Kind == symbol.SymbolVariant) {
				byName[selected.Name] = id
			}
		}

		switch aggregate.Kind {
		case aggregateStruct:
			if declaration.Nominal != infer.NominalStruct {
				report(CodeAggregate, aggregate.Header)
				continue
			}
			orderedFields := make([]symbol.SymbolID, 0, len(orderedMembers))
			for _, id := range orderedMembers {
				selected, found := symbolsByID[id]
				if found && selected.Kind == symbol.SymbolField {
					orderedFields = append(orderedFields, id)
				}
			}
			seen := make(map[symbol.SymbolID]bool, len(orderedFields))
			for _, field := range aggregate.Fields {
				id := field.Member
				selected, found := symbolsByID[id]
				if id == 0 {
					id, found = byName[field.Name]
					selected, found = symbolsByID[id]
				}
				if !found || selected.Kind != symbol.SymbolField {
					fieldReport(CodeMember, field, aggregate.Header)
					continue
				}
				if seen[id] {
					fieldReport(CodeMember, field, aggregate.Header)
					continue
				}
				seen[id] = true
			}
			for _, id := range orderedFields {
				if !seen[id] {
					fieldReport(CodeMember, fieldValue{}, aggregate.Header)
				}
			}

		case aggregateUnion:
			if declaration.Nominal != infer.NominalUnion {
				report(CodeAggregate, aggregate.Header)
				continue
			}
			// An untagged union's construction must specify exactly one field:
			// every field shares the same storage, so setting more than one (or
			// none) has no meaning. Zero and multi-field literals are rejected
			// with the construction diagnostic, matching how the struct case
			// reports a whole-construction problem on the header.
			if len(aggregate.Fields) != 1 {
				report(CodeAggregate, aggregate.Header)
				continue
			}
			field := aggregate.Fields[0]
			id := field.Member
			selected, found := symbolsByID[id]
			if id == 0 {
				id, found = byName[field.Name]
				selected, found = symbolsByID[id]
			}
			// The parser registers an untagged union's members as SymbolVariant
			// exactly like a tagged union's, but under the untagged-union
			// contract they are real fields: a construction must name one of
			// them, and any one of them is a valid construction target. Both
			// kinds are accepted here; the nominal check above already
			// restricted this case to NominalUnion.
			if !found || (selected.Kind != symbol.SymbolField && selected.Kind != symbol.SymbolVariant) {
				fieldReport(CodeMember, field, aggregate.Header)
			}

		case aggregateEnumVariant:
			if declaration.Nominal != infer.NominalEnum && declaration.Nominal != infer.NominalTaggedUnion {
				report(CodeMember, aggregate.Header)
				continue
			}
			if len(aggregate.Fields) == 0 {
				report(CodeMember, aggregate.Header)
				continue
			}
			name := aggregate.Fields[0].Name
			matched := false
			for _, id := range orderedMembers {
				selected, found := symbolsByID[id]
				if found && selected.Kind == symbol.SymbolVariant && selected.Name == name {
					matched = true
					break
				}
			}
			if !matched {
				fieldReport(CodeMember, aggregate.Fields[0], aggregate.Header)
			}

		case aggregateTaggedVariant:
			if declaration.Nominal != infer.NominalTaggedUnion {
				report(CodeMember, aggregate.Header)
				continue
			}
			if len(aggregate.Fields) != 1 {
				report(CodeMember, aggregate.Header)
				continue
			}
			name := aggregate.Fields[0].Name
			matched := false
			for _, id := range orderedMembers {
				selected, found := symbolsByID[id]
				if found && selected.Kind == symbol.SymbolVariant && selected.Name == name {
					matched = true
					break
				}
			}
			if !matched {
				fieldReport(CodeMember, aggregate.Fields[0], aggregate.Header)
			}
		}
	}
	return !failed
}
