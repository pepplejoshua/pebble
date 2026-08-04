package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeMember diagnostic.Code = "C0605"

func validateMemberRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	resolution := handoff.Semantics.Resolution()
	typeSnapshot := handoff.Semantics.Types()
	failed := false

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
			declaration, _, ok := key.Nominal()
			if !ok {
				valid := member.Name == "len" && (key.Kind() == types.Array || (key.Kind() == types.Builtin && func() bool { builtin, ok := key.Builtin(); return ok && builtin == types.Str }()))
				if key.Kind() == types.Slice {
					valid = member.Name == "len" || member.Name == "data"
				}
				if !valid {
					report(member.Header)
				}
				continue
			}
			matched := false
			for _, id := range resolution.Members(declaration) {
				selected, found := resolution.Symbols.Symbol(id)
				if found && selected.Name == member.Name && selected.Kind == symbol.SymbolField {
					matched = true
					break
				}
			}
			if !matched {
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
