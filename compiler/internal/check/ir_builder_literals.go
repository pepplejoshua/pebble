package check

import (
	"sort"
	"strconv"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

func (s *irBuildState) buildLiteral(record *expressionRecord, node *tir.Node) bool {
	switch record.Literal.Kind {
	case literalBool:
		node.Kind = tir.BoolLiteral
		node.Literal = tir.Literal{Kind: tir.LiteralBool, Bool: record.Literal.Bool}
	case literalChar:
		node.Kind = tir.CharLiteral
		node.Literal = tir.Literal{Kind: tir.LiteralChar, Char: record.Literal.Rune}
	case literalString:
		node.Kind = tir.StringLiteral
		node.Literal = tir.Literal{Kind: tir.LiteralString, String: record.Literal.Text}
	case literalInteger:
		node.Kind = tir.IntegerLiteral
		num, den, ok := decodeIntegerLiteral(record.Literal.NumericBytes)
		if !ok {
			return false
		}
		node.Literal = tir.Literal{Kind: tir.LiteralInteger, IntegerNum: num, IntegerDen: den}
	case literalFloat:
		node.Kind = tir.FloatLiteral
		str, ok := decodeFloatLiteral(record.Literal.NumericBytes)
		if !ok {
			return false
		}
		node.Literal = tir.Literal{Kind: tir.LiteralFloat, Float: str}
	case literalNil:
		node.Kind = tir.NilPointer
	case literalNone:
		node.Kind = tir.NoneOptional
	default:
		return false
	}
	return true
}

func (s *irBuildState) buildSymbolValue(record *expressionRecord, node *tir.Node) bool {
	sym, ok := s.symbol(record.Symbol)
	if !ok {
		return false
	}
	switch sym.Kind {
	case symbol.SymbolBinding, symbol.SymbolParameter, symbol.SymbolLoopBinding, symbol.SymbolExternBinding, symbol.SymbolField:
		node.Kind = tir.SymbolValue
		node.Symbol = record.Symbol
	case symbol.SymbolVariant:
		node.Kind = tir.EnumVariantValue
		node.Member = record.Symbol
	case symbol.SymbolFunction:
		if sym.Generic {
			return false
		}
		function := s.functions[record.Symbol]
		if function == 0 {
			return false
		}
		node.Kind, node.Symbol, node.Function = tir.HoistedFunctionValue, record.Symbol, function
	default:
		return false
	}
	return true
}

// buildVariantMember handles a dotted member access that resolves to an enum
// variant with no runtime base (e.g. Color.red). The member machinery records
// these as expressionMember with a variant symbol; the selected variant becomes
// an EnumVariantValue.
func (s *irBuildState) buildVariantMember(record *expressionRecord, node *tir.Node) bool {
	if len(record.Children) != 0 {
		return false
	}
	sym, ok := s.symbol(record.Symbol)
	if !ok || sym.Kind != symbol.SymbolVariant {
		return false
	}
	node.Kind = tir.EnumVariantValue
	node.Member = record.Symbol
	return true
}

func (s *irBuildState) buildSizeof(record *expressionRecord, node *tir.Node) bool {
	argType, ok := s.resolveType(record.TypeArgument)
	if !ok {
		return false
	}
	node.Kind = tir.SizeofType
	node.TypeArg = argType
	return true
}

func (s *irBuildState) buildChildren(record *expressionRecord, node *tir.Node) bool {
	children := make([]tir.NodeID, 0, len(record.Children))
	for _, childID := range record.Children {
		childNode, ok := s.buildValue(childID)
		if !ok {
			return false
		}
		children = append(children, childNode)
	}
	node.Children = children
	return true
}

func (s *irBuildState) buildArrayRepeat(record *expressionRecord, node *tir.Node) bool {
	if len(record.Children) != 1 {
		return false
	}
	length, ok := s.arrayLength(record.Result)
	if !ok {
		return false
	}
	valueChild, ok := s.buildValue(record.Children[0])
	if !ok {
		return false
	}
	countNode, ok := s.addNode(tir.Node{
		Kind:          tir.IntegerLiteral,
		Type:          s.handoff.Semantics.Types().Builtins().Uint,
		Origin:        record.Header.Span,
		SyntheticRole: "array-repeat-count",
		Literal:       tir.Literal{Kind: tir.LiteralInteger, IntegerNum: strconv.FormatUint(length, 10), IntegerDen: "1"},
	}, symbol.SyntaxRef{})
	if !ok {
		return false
	}
	node.Kind = tir.ArrayRepeat
	node.Children = []tir.NodeID{valueChild, countNode}
	return true
}

func (s *irBuildState) arrayLength(id valueID) (uint64, bool) {
	typ, ok := s.resolveType(id)
	if !ok {
		return 0, false
	}
	key, ok := s.typeKey(typ)
	if !ok {
		return 0, false
	}
	length, _, ok := key.Array()
	return length, ok
}

// buildEnumVariantShorthand handles the enum-variant shorthand literal (a
// leading dot followed by a variant name, e.g. .Empty). The semantic layer
// resolves the target enum type into a single aggregateEnumVariant record whose
// first field names the selected variant; the variant symbol itself is never
// name-resolved at 06a (the resolver defers partial-member names), so it is
// re-derived by name from the solved receiver type here. This produces the same
// EnumVariantValue node that the explicit and qualified forms (Color.red)
// already build, so the backend handles it for free.
func (s *irBuildState) buildEnumVariantShorthand(record *expressionRecord, node *tir.Node) bool {
	aggregate, ok := s.aggregatesByRecord[record.Specialized]
	if !ok || aggregate == nil || aggregate.Kind != aggregateEnumVariant || len(aggregate.Fields) == 0 {
		return false
	}
	member := aggregate.Fields[0].Member
	if member == 0 {
		member = s.memberSymbol(aggregate.Receiver, aggregate.Fields[0].Name)
	}
	if member == 0 {
		return false
	}
	node.Kind = tir.EnumVariantValue
	node.Member = member
	return true
}

func (s *irBuildState) buildRecordConstruct(record *expressionRecord, node *tir.Node) bool {
	aggregate, ok := s.aggregatesByRecord[record.Specialized]
	if !ok || aggregate == nil || aggregate.Kind != aggregateStruct || aggregate.Declaration == 0 {
		return false
	}
	sorted := make([]fieldValue, len(aggregate.Fields))
	copy(sorted, aggregate.Fields)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i].Ordinal < sorted[j].Ordinal })
	fields := make([]tir.FieldInit, 0, len(sorted))
	for _, fv := range sorted {
		valueNode, ok := s.buildValue(fv.Value)
		if !ok {
			return false
		}
		member := fv.Member
		if member == 0 {
			// Anonymous .{ ... } construction: the resolver cannot resolve the
			// field names (there is no base-type name node), so the member
			// symbol is re-derived by name from the record's solved receiver
			// type here — exactly as buildTaggedVariantConstruct does for
			// .{ Int = 42 }.
			member = s.memberSymbol(aggregate.Receiver, fv.Name)
		}
		fields = append(fields, tir.FieldInit{Field: member, Value: valueNode})
	}
	node.Kind = tir.RecordConstruct
	node.Symbol = aggregate.Declaration
	node.Fields = fields
	return true
}

// buildTaggedVariantConstruct handles a .{ Int = 42 } record literal against a
// tagged-union (union enum) destination, which finishRecord routes to an
// aggregateTaggedVariant record. It produces exactly the VariantConstruct node
// the call-syntax path (Data.Int(42)) already builds: Member is the variant
// symbol (name-resolved for the qualified form, re-derived by name for the
// inferred form) and the single payload value becomes the one child.
func (s *irBuildState) buildTaggedVariantConstruct(record *expressionRecord, node *tir.Node) bool {
	aggregate, ok := s.aggregatesByRecord[record.Specialized]
	if !ok || aggregate == nil || aggregate.Kind != aggregateTaggedVariant || len(aggregate.Fields) != 1 {
		return false
	}
	member := aggregate.Fields[0].Member
	if member == 0 {
		member = s.memberSymbol(aggregate.Receiver, aggregate.Fields[0].Name)
	}
	if member == 0 {
		return false
	}
	valueNode, ok := s.buildValue(aggregate.Fields[0].Value)
	if !ok {
		return false
	}
	node.Kind = tir.VariantConstruct
	node.Member = member
	node.Children = []tir.NodeID{valueNode}
	return true
}
