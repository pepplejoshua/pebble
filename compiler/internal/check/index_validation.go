package check

import (
	"math/big"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

const CodeIndexBound diagnostic.Code = "C0609"

func validateIndexRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	for _, retained := range handoff.Records.Records() {
		index := retained.Index
		if index == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		result, ok := records.Root(index.Result)
		if !ok || result.State != infer.TypeFinal {
			continue
		}

		bad := false
		var start, end constantResult
		if index.StartPresent {
			start, ok = records.Constant(index.StartSyntax)
			if ok {
				if start.State == constantKnown && !validIndexConstant(start) {
					bad = true
				}
			}
		}
		if index.EndPresent && !bad {
			end, ok = records.Constant(index.EndSyntax)
			if ok {
				if end.State == constantKnown && !validIndexConstant(end) {
					bad = true
				}
			}
		}
		if !bad && index.HasKnownArrayLength {
			length := new(big.Int).SetUint64(index.KnownArrayLength)
			if index.StartPresent && start.State == constantKnown && !validIndexRange(start.Value.Integer, length, index.Mode) {
				bad = true
			}
			if index.EndPresent && end.State == constantKnown && !validIndexRange(end.Value.Integer, length, index.Mode) {
				bad = true
			}
		}
		if !bad && index.Mode == indexSlice && index.StartPresent && index.EndPresent && start.State == constantKnown && end.State == constantKnown && start.Value.Integer.Cmp(end.Value.Integer) > 0 {
			bad = true
		}
		if bad {
			failed = true
			reporter.add(diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeIndexBound,
				Message:  "index or slice bound is invalid",
				Primary:  diagnostic.Label{Span: retained.Header.Span},
			})
		}
	}
	return !failed
}

func validIndexConstant(result constantResult) bool {
	return result.Value.Kind == constantInteger && result.Value.Integer != nil && result.Value.Integer.Sign() >= 0
}

func validIndexRange(value, length *big.Int, mode indexMode) bool {
	if value == nil || value.Sign() < 0 {
		return false
	}
	if mode == indexValue {
		return value.Cmp(length) < 0
	}
	return value.Cmp(length) <= 0
}
