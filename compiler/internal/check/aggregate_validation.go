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

		switch aggregate.Kind {
		case aggregateStruct:
			if declaration.Nominal != infer.NominalStruct {
				report(CodeAggregate, aggregate.Header)
				continue
			}
			byName := make(map[string]symbol.SymbolID)
			orderedFields := make([]symbol.SymbolID, 0, len(orderedMembers))
			for _, id := range orderedMembers {
				selected, found := symbolsByID[id]
				if found && selected.Kind == symbol.SymbolField {
					byName[selected.Name] = id
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
			// Tagged variants with payloads are call records in the current pipeline.
		}
	}
	return !failed
}
