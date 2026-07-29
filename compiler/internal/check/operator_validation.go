package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodeOperator diagnostic.Code = "C0603"

func isSignedIntegerBuiltin(kind types.BuiltinKind) bool {
	switch kind {
	case types.Int, types.I8, types.I16, types.I32, types.I64:
		return true
	}
	return false
}

func activeOperatorRecord(handoff *solveHandoff, header recordHeader) bool {
	if !header.Alternative.Guarded {
		return true
	}
	selected, ok := handoff.Solution.Selection(header.Alternative.Choice)
	return ok && selected == header.Alternative.Index
}

func validateArithmeticOperators(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)

	// Requirements are retained separately from their operator, so index them
	// first to make rigidity independent of record ordering.
	rigid := make(map[uint32]map[valueID]bool)
	for _, record := range handoff.Records.Records() {
		if record.Requirement == nil || record.Requirement.Operator == 0 || !activeOperatorRecord(handoff, record.Header) {
			continue
		}
		if record.Requirement.Header.Owner != record.Header.Owner {
			continue
		}
		owner := uint32(record.Header.Owner)
		if rigid[owner] == nil {
			rigid[owner] = make(map[valueID]bool)
		}
		rigid[owner][record.Requirement.Subject] = true
	}

	failed := false
	for _, retained := range handoff.Records.Records() {
		op := retained.Operator
		if op == nil || !activeOperatorRecord(handoff, retained.Header) || !arithmeticOperatorFamily(op.Family) {
			continue
		}
		if op.Family == operatorLiteralNegate {
			continue
		}

		keys := make([]types.TypeKey, len(op.Operands)+1)
		typeIDs := make([]types.TypeID, len(keys))
		resolved := make([]bool, len(keys))
		ids := append(append([]valueID(nil), op.Operands...), op.Result)
		for i, id := range ids {
			result, ok := records.Root(id)
			if !ok || result.State != infer.TypeFinal {
				continue
			}
			key, ok := handoff.Semantics.Types().Key(result.Type)
			if !ok {
				continue
			}
			keys[i], resolved[i] = key, true
			typeIDs[i] = result.Type
		}

		bad := false
		isRigid := rigid[uint32(op.Header.Owner)]
		concrete := func(index int) bool { return resolved[index] && !isRigid[op.Operands[index]] }
		builtin := func(index int) (types.BuiltinKind, bool) {
			if !resolved[index] || keys[index].Kind() != types.Builtin {
				return 0, false
			}
			return keys[index].Builtin()
		}
		sameConcrete := func(left, right int) bool {
			if !concrete(left) || !concrete(right) {
				return true
			}
			leftKind, leftOK := builtin(left)
			rightKind, rightOK := builtin(right)
			return leftOK && rightOK && leftKind == rightKind
		}
		resultMatches := func(operand int) bool {
			if !resolved[len(op.Operands)] || !concrete(operand) || isRigid[op.Result] {
				return true
			}
			return keys[len(op.Operands)].Kind() == types.Builtin && keys[operand].Kind() == types.Builtin && typeIDs[len(op.Operands)] == typeIDs[operand]
		}

		switch op.Family {
		case operatorNumericSame:
			if op.Form == operatorPrefix {
				kind, ok := builtin(0)
				bad = concrete(0) && (!ok || !(isSignedIntegerBuiltin(kind) || isFloatBuiltin(kind)))
				bad = bad || !resultMatches(0)
			} else {
				left, leftOK := builtin(0)
				right, rightOK := builtin(1)
				bad = concrete(0) && (!leftOK || !(isIntegerBuiltin(left) || isFloatBuiltin(left)))
				bad = bad || concrete(1) && (!rightOK || !(isIntegerBuiltin(right) || isFloatBuiltin(right)))
				bad = bad || !sameConcrete(0, 1) || !resultMatches(0)
			}
		case operatorAdd:
			left, leftOK := builtin(0)
			right, rightOK := builtin(1)
			bad = concrete(0) && (!leftOK || !(isIntegerBuiltin(left) || isFloatBuiltin(left) || left == types.Str))
			bad = bad || concrete(1) && (!rightOK || !(isIntegerBuiltin(right) || isFloatBuiltin(right) || right == types.Str))
			bad = bad || !sameConcrete(0, 1) || !resultMatches(0)
		case operatorIntegralSame:
			left, leftOK := builtin(0)
			bad = concrete(0) && (!leftOK || !isIntegerBuiltin(left))
			if op.Form == operatorBinary {
				right, rightOK := builtin(1)
				bad = bad || concrete(1) && (!rightOK || !isIntegerBuiltin(right))
				bad = bad || !sameConcrete(0, 1)
			}
			bad = bad || !resultMatches(0)
		case operatorShift:
			left, leftOK := builtin(0)
			right, rightOK := builtin(1)
			bad = concrete(0) && (!leftOK || !isIntegerBuiltin(left))
			bad = bad || concrete(1) && (!rightOK || !isIntegerBuiltin(right))
			bad = bad || !resultMatches(0)
		}
		if bad {
			failed = true
			reporter.add(diagnostic.Diagnostic{Severity: diagnostic.Error, Code: CodeOperator, Message: "operator operands or result have invalid types", Primary: diagnostic.Label{Span: op.Header.Span}})
		}
	}
	return !failed
}

func arithmeticOperatorFamily(family operatorFamily) bool {
	switch family {
	case operatorLiteralNegate, operatorNumericSame, operatorAdd, operatorIntegralSame, operatorShift:
		return true
	}
	return false
}
