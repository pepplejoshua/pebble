package check

import (
	"math/big"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

func (s *irBuildState) buildInterpolated(record *expressionRecord, node *tir.Node) bool {
	parts := make([]tir.InterpolationPart, 0, len(record.Parts))
	for _, part := range record.Parts {
		switch part.Kind {
		case interpolationText:
			parts = append(parts, tir.InterpolationPart{Kind: tir.InterpolationTextPart, Text: part.Text})
		case interpolationValue:
			valueNode, ok := s.buildValue(part.Value)
			if !ok {
				return false
			}
			parts = append(parts, tir.InterpolationPart{Kind: tir.InterpolationValuePart, Value: valueNode})
		}
	}
	node.Kind = tir.InterpolatedString
	node.Parts = parts
	return true
}

func (s *irBuildState) buildOperatorValue(record *expressionRecord, node *tir.Node) bool {
	op, ok := s.operatorsBySyntax[record.Header.Syntax]
	if !ok || op == nil {
		return false
	}
	switch op.Form {
	case operatorPrefix:
		if (op.Family == operatorLiteralNegate || op.Family == operatorNumericSame) && s.operatorHasIntegerOperand(op) {
			node.Kind = tir.CheckedNegate
		} else {
			node.Kind = tir.PrefixValue
		}
	case operatorPostfix:
		if op.Family != operatorOptionalForce || op.Token != syntax.Bang {
			return false
		}
		node.Kind = tir.CheckedOptionalUnwrap
	case operatorBinary:
		if op.Family == operatorShift {
			node.Kind = tir.CheckedShift
		} else if op.Family == operatorBoolean && (op.Token == syntax.LogicalAnd || op.Token == syntax.LogicalOr) {
			node.Kind = tir.ShortCircuitValue
		} else if op.Family == operatorIntegralSame && op.Token == syntax.Percent {
			node.Kind = tir.CheckedArithmetic
		} else if (op.Family == operatorNumericSame || op.Family == operatorAdd) && s.operatorHasIntegerOperand(op) {
			node.Kind = tir.CheckedArithmetic
		} else {
			node.Kind = tir.BinaryValue
		}
	default:
		return false
	}
	if !allowedOperatorFamily(op.Family, op.Form) {
		return false
	}
	children := make([]tir.NodeID, 0, len(op.Operands))
	for _, operand := range op.Operands {
		operandNode, ok := s.buildValue(operand)
		if !ok {
			return false
		}
		children = append(children, operandNode)
	}
	switch node.Kind {
	case tir.PrefixValue, tir.BinaryValue, tir.ShortCircuitValue, tir.CheckedArithmetic, tir.CheckedNegate, tir.CheckedShift:
		node.Operator = op.Token
	}
	node.Children = children
	return true
}

func (s *irBuildState) operatorHasIntegerOperand(op *operatorRecord) bool {
	if len(op.Operands) == 0 {
		return false
	}
	typ, ok := s.resolveType(op.Operands[0])
	if !ok {
		return false
	}
	key, ok := s.typeKey(typ)
	if !ok {
		return false
	}
	builtin, ok := key.Builtin()
	return ok && isIntegerBuiltin(builtin)
}

func allowedOperatorFamily(family operatorFamily, form operatorForm) bool {
	switch family {
	case operatorLiteralNegate:
		return form == operatorPrefix
	case operatorBoolean:
		return form == operatorPrefix || form == operatorBinary
	case operatorNumericSame, operatorIntegralSame:
		return form == operatorPrefix || form == operatorBinary
	case operatorAdd, operatorShift, operatorOrdering, operatorEquality:
		return form == operatorBinary
	case operatorOptionalForce:
		return form == operatorPostfix
	}
	return false
}

func decodeIntegerLiteral(bytes []byte) (string, string, bool) {
	stripped := make([]byte, 0, len(bytes))
	for _, b := range bytes {
		if b != '_' {
			stripped = append(stripped, b)
		}
	}
	base := 10
	digits := string(stripped)
	switch {
	case strings.HasPrefix(digits, "0x") || strings.HasPrefix(digits, "0X"):
		base, digits = 16, digits[2:]
	case strings.HasPrefix(digits, "0b") || strings.HasPrefix(digits, "0B"):
		base, digits = 2, digits[2:]
	case strings.HasPrefix(digits, "0o") || strings.HasPrefix(digits, "0O"):
		base, digits = 8, digits[2:]
	}
	if digits == "" {
		return "", "", false
	}
	value, ok := new(big.Int).SetString(digits, base)
	if !ok {
		return "", "", false
	}
	return value.String(), "1", true
}

func decodeFloatLiteral(bytes []byte) (string, bool) {
	stripped := make([]byte, 0, len(bytes))
	for _, b := range bytes {
		if b != '_' {
			stripped = append(stripped, b)
		}
	}
	if len(stripped) == 0 {
		return "", false
	}
	return string(stripped), true
}
