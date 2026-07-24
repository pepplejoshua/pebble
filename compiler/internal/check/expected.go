package check

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

type expectedKind uint8

const (
	expectNone expectedKind = iota
	expectIdentity
	expectLiteral
	expectShape
)

type compatibilityRole uint8

const (
	compatibilityAssignment compatibilityRole = iota + 1
	compatibilityArgument
	compatibilityReturn
	compatibilityRecordField
	compatibilityTupleComponent
	compatibilityOptionalInjection
	compatibilityBranch
)

type expectedType struct {
	Kind        expectedKind
	Destination valueID
	Role        compatibilityRole
}

type compatibilityRecord struct {
	Header              recordHeader
	Source, Destination valueID
	Role                compatibilityRole
	Ordinal             uint32
	DestinationSymbol   symbol.SymbolID
	DestinationSpan     source.Span
}

func (w *walker) expectationFor(ref symbol.SyntaxRef, destination valueID, role compatibilityRole) expectedType {
	if destination == 0 || !w.generation.hasValue(destination) {
		return expectedType{}
	}
	none := expectedType{Kind: expectNone, Destination: destination, Role: role}
	node, ok := w.node(ref.Module, ref.Node)
	if !ok {
		return none
	}
	switch node.Kind() {
	case syntax.Literal:
		if node.Token() == syntax.IntegerLiteral || node.Token() == syntax.FloatLiteral {
			if id, known := w.knownValues[destination]; known {
				if key, found := w.generation.inputs.Types.Key(id); found && key.Kind() == types.Optional {
					if payload, ok := key.Child(); ok {
						origin := w.originForRef(ref, "optional literal payload", 0, 0)
						payloadValue, published := w.newSlotValue(w.session.Known(payload), origin)
						if published {
							payloadValue.Known = payload
							w.knownValues[payloadValue.ID] = payload
							w.optionalDestinations[ref] = destination
							return expectedType{Kind: expectLiteral, Destination: payloadValue.ID, Role: compatibilityOptionalInjection}
						}
					}
				}
			}
			return expectedType{Kind: expectLiteral, Destination: destination, Role: role}
		}
		if node.Token() == syntax.KwNil || node.Token() == syntax.KwNone {
			return expectedType{Kind: expectShape, Destination: destination, Role: role}
		}
		return expectedType{Kind: expectIdentity, Destination: destination, Role: role}
	case syntax.SomeExpr, syntax.TupleTerm, syntax.ArrayExpr, syntax.ArrayRepeatExpr,
		syntax.RecordExpr, syntax.PartialMemberExpr, syntax.FunctionTerm:
		return expectedType{Kind: expectShape, Destination: destination, Role: role}
	case syntax.PrefixTerm, syntax.PostfixExpr, syntax.BinaryExpr:
		if id, known := w.knownValues[destination]; known {
			if key, found := w.generation.inputs.Types.Key(id); found && key.Kind() == types.Optional {
				if payload, ok := key.Child(); ok {
					origin := w.originForRef(ref, "optional operator payload", 0, 0)
					payloadValue, published := w.newSlotValue(w.session.Known(payload), origin)
					if published {
						payloadValue.Known = payload
						w.knownValues[payloadValue.ID] = payload
						w.optionalDestinations[ref] = destination
						return expectedType{Kind: expectIdentity, Destination: payloadValue.ID, Role: compatibilityOptionalInjection}
					}
				}
			}
		}
		return expectedType{Kind: expectIdentity, Destination: destination, Role: role}
	default:
		return none
	}
}

func (w *walker) applyExpected(actual typedValue, exactLiteral infer.Term, expected expectedType, origin infer.Origin) {
	if actual.ID == 0 || expected.Kind == expectNone || !w.generation.hasValue(expected.Destination) {
		return
	}
	destination := w.generation.values[expected.Destination-1]
	switch expected.Kind {
	case expectIdentity:
		w.session.Add(infer.Equal(actual.Term, destination.Term, origin))
	case expectLiteral:
		if exactLiteral == (infer.Term{}) {
			exactLiteral = actual.Term
		}
		w.session.Add(infer.LiteralFits(exactLiteral, destination.Term, origin))
	}
}

func (w *walker) knownDestination(expected expectedType) (types.TypeID, bool) {
	id, ok := w.knownValues[expected.Destination]
	return id, ok && id != 0
}

func (w *walker) projectFunctionExpectation(ref symbol.SyntaxRef, ctx walkContext, record callableRecord, result typedValue) {
	if ctx.expected.Kind != expectShape {
		return
	}
	id, ok := w.knownDestination(ctx.expected)
	if !ok {
		return
	}
	key, ok := w.generation.inputs.Types.Key(id)
	if !ok {
		return
	}
	convention, parameters, expectedResult, variadic, ok := key.Function()
	if !ok || convention != record.Convention || variadic != record.Variadic || len(parameters) != len(record.Parameters) {
		return
	}
	for index, parameter := range parameters {
		origin := w.originForRef(ref, fmt.Sprintf("function expected parameter %d", index+1), ctx.typeOwner, ctx.genericOwner)
		destination, published := w.newSlotValue(w.session.Known(parameter), origin)
		if !published {
			continue
		}
		destination.Known = parameter
		w.knownValues[destination.ID] = parameter
		actual := w.generation.values[record.Parameters[index]-1]
		w.session.Add(infer.Equal(actual.Term, destination.Term, origin))
	}
	resultOrigin := w.originForRef(ref, "function expected result", ctx.typeOwner, ctx.genericOwner)
	destination, published := w.newSlotValue(w.session.Known(expectedResult), resultOrigin)
	if published {
		destination.Known = expectedResult
		w.knownValues[destination.ID] = expectedResult
		w.session.Add(infer.Equal(result.Term, destination.Term, resultOrigin))
	}
}

func (w *walker) retainCompatibility(ref symbol.SyntaxRef, genericOwner symbol.SymbolID, sourceValue, destination valueID, role compatibilityRole, ordinal uint32, destinationSymbol symbol.SymbolID, destinationSpan source.Span, suppressed bool) recordID {
	header := w.header(ref, genericOwner, suppressed)
	record := compatibilityRecord{
		Header: header, Source: sourceValue, Destination: destination, Role: role,
		Ordinal: ordinal, DestinationSymbol: destinationSymbol, DestinationSpan: destinationSpan,
	}
	id, _ := w.generation.addRecord(retainedRecord{Header: header, Compatibility: &record})
	return id
}

func expectedRoleText(role compatibilityRole, ordinal uint32) string {
	switch role {
	case compatibilityAssignment:
		return "assignment"
	case compatibilityArgument:
		return fmt.Sprintf("argument %d", ordinal+1)
	case compatibilityReturn:
		return "return value"
	case compatibilityRecordField:
		return fmt.Sprintf("field %d", ordinal+1)
	case compatibilityTupleComponent:
		return fmt.Sprintf("tuple component %d", ordinal+1)
	case compatibilityOptionalInjection:
		return "optional injection"
	case compatibilityBranch:
		return "branch result"
	default:
		return "expected type"
	}
}
