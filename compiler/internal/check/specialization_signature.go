package check

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// specializedSignature is one instantiation's substituted parameter and result
// types, computed from a generic callable's symbolic signature and a concrete
// instantiation's solved type arguments. It is the monomorphized analog of the
// unspecialized shape buildDeclarations reads for a generic's own declaration:
// the same per-parameter/result value lookups, with every TypeParameter
// occurrence replaced by the concrete argument solved at one call site.
type specializedSignature struct {
	Parameters []types.TypeID
	Result     types.TypeID
}

// buildSpecializedSignature computes the substituted parameter/result types
// for one concrete instantiation of a generic callable. store is the single
// live type store that owns both the symbolic types read from records and the
// concrete argument types the instantiation solves to (handoff.Semantics.Types
// is only the read-only snapshot, which cannot intern rewritten composites, so
// the store must come from the caller). callable is the generic function's own
// retained callableRecord — the same shape buildDeclarations reads for the
// unspecialized case. instantiation gives the concrete type arguments solved
// for this specific call site, in the generic's own declared type-parameter
// order.
//
// A mismatched-arity instantiation or an argument that did not resolve to a
// final concrete type is an error, never a panic: anything malformed reaching
// this point should have been rejected by the generic validator, but this
// function does not assume it.
func buildSpecializedSignature(
	store *types.Store,
	handoff *solveHandoff,
	records *solvedRecords,
	callable *callableRecord,
	instantiation infer.Instantiation,
) (specializedSignature, error) {
	if store == nil {
		return specializedSignature{}, fmt.Errorf("specialization signature requires a type store")
	}
	if handoff == nil || handoff.Semantics == nil {
		return specializedSignature{}, fmt.Errorf("specialization signature requires a semantic snapshot")
	}
	if callable == nil {
		return specializedSignature{}, fmt.Errorf("specialization signature requires a callable record")
	}

	signature, ok := handoff.Semantics.Signature(instantiation.Generic)
	if !ok {
		return specializedSignature{}, fmt.Errorf("generic %d has no prepared signature", instantiation.Generic)
	}
	if len(signature.TypeParams) != len(instantiation.Arguments) {
		return specializedSignature{}, fmt.Errorf(
			"generic %d has %d type parameters but its instantiation solved %d arguments",
			instantiation.Generic, len(signature.TypeParams), len(instantiation.Arguments))
	}
	substitutions := make(map[symbol.SymbolID]types.TypeID, len(signature.TypeParams))
	for index, parameter := range signature.TypeParams {
		argument := instantiation.Arguments[index]
		if argument.State != infer.TypeFinal {
			return specializedSignature{}, fmt.Errorf(
				"generic %d instantiation argument %d is not final (state %v)",
				instantiation.Generic, index, argument.State)
		}
		substitutions[parameter] = argument.Type
	}

	parameters := make([]types.TypeID, len(callable.Parameters))
	for index, value := range callable.Parameters {
		symbolic, ok := typeOfValue(records, value)
		if !ok {
			return specializedSignature{}, fmt.Errorf(
				"generic %d parameter %d has no resolved symbolic type", instantiation.Generic, index)
		}
		substituted, err := store.Substitute(symbolic, substitutions)
		if err != nil {
			return specializedSignature{}, fmt.Errorf(
				"generic %d parameter %d substitution failed: %w", instantiation.Generic, index, err)
		}
		parameters[index] = substituted
	}

	symbolicResult, ok := typeOfValue(records, callable.Result)
	if !ok {
		return specializedSignature{}, fmt.Errorf("generic %d has no resolved symbolic result type", instantiation.Generic)
	}
	result, err := store.Substitute(symbolicResult, substitutions)
	if err != nil {
		return specializedSignature{}, fmt.Errorf("generic %d result substitution failed: %w", instantiation.Generic, err)
	}

	return specializedSignature{Parameters: parameters, Result: result}, nil
}
